// ###################################################################
// #### This file is part of the mathematics library project, and is
// #### offered under the licence agreement described on
// #### http://www.mrsoft.org/
// ####
// #### Copyright:(c) 2020, Michael R. . All rights reserved.
// ####
// #### Unless required by applicable law or agreed to in writing, software
// #### distributed under the License is distributed on an "AS IS" BASIS,
// #### WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// #### See the License for the specific language governing permissions and
// #### limitations under the License.
// ###################################################################


unit IOCompletionPortsThreadPool;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}


// #####################################################
// #### Thread pool based on IO completion ports
// #####################################################
interface

{$IFDEF MSWINDOWS}
uses MtxThreadPool, MatrixConst, SysUtils;



function CreateThreadPoolObj : IMtxThreadPool;
{$ENDIF}

implementation

{$IFDEF MSWINDOWS}
uses Classes, 
     {$IFDEF FPC} Windows 
     {$ELSE} 
     {$IF CompilerVersion >= 23.0} Winapi.Windows {$ELSE} Windows {$IFEND} 
     {$ENDIF}, SyncObjs, winCPUInfo, CPUFeatures, Contnrs;

type
  TMtxIOCompletionThrdPool = class;
  TTaskFinEvt = procedure( Sender : Pointer ) of Object;
  TIOAsyncCall = record
    fData : TObject;
    fProc : TMtxProc;
    fRec : Pointer;
    fRecProc : TMtxRecProc;
    fOnFinish : TTaskFinEvt;
    FFatalErrorAddr : Pointer; 
    FFatalException : Exception; 
  public
    procedure Execute;
    procedure Create(proc : TMtxProc; obj : TObject; onFinish : TTaskFinEvt);
    procedure CreateRec(proc : TMtxRecProc; rec : Pointer; onFinish : TTaskFinEvt);
  end;
  PIOAsyncCall = ^TIOAsyncCall;  

  { TIOAsyncCallThread is a pooled thread. It looks itself for work. }
  TIOAsyncCallThread = class(TThread)
  protected
    fPortHandle : THandle;
    
    procedure Execute; override;
  public
    procedure ForceTerminate;

    constructor Create(portHandle : THandle);
  end;

  TMtxIOCompletionThrdPool = class(TInterfacedObject, IMtxThreadPool)
  private
    fThreadList : TObjectList;
    fMaxThreads: integer;
    fNumThreads : integer;
    fNumCPU : integer;
    fPoolHdl : THandle;
  public
    procedure AddAsyncCall(call : PIOAsyncCall);

    procedure InitPool( maxNumThreads : integer );
    function CreateTaskGroup : IMtxAsyncCallGroup;

    property MaxThreads : integer read fMaxThreads;

    constructor Create;
    destructor Destroy; override;
  end;

type
  TIOPortThreadGroup = class(TInterfacedObject, IMtxAsyncCallGroup)
  private
    fRefCntBuf : Array[0..7] of byte;   // ensure 4 byte alignment on fRefCnt
    fRefCnt : PInteger;
    fCallIdx : integer;
    fPool : TMtxIOCompletionThrdPool;
    fTasks : Array[0..cMaxNumCores - 1] of TIOAsyncCall;
    procedure OnFinish( Sender : Pointer ) ;
  public
    procedure AddTaskRec(proc : TMtxRecProc; rec : Pointer);
    procedure AddTask(proc : TMtxProc; obj : TObject);
    procedure SyncAll;

    constructor Create(pool : TMtxIOCompletionThrdPool);
  end;

{ TSimpleWinThreadGroup }

procedure TIOPortThreadGroup.AddTask(proc : TMtxProc; obj : TObject);
begin
     InterlockedIncrement(fRefCnt^);
     fTasks[fCallIdx].Create(proc, obj, OnFinish);
     fPool.AddAsyncCall(@fTasks[fCallIdx]);
     inc(fCallIdx);
end;

constructor TIOPortThreadGroup.Create(pool : TMtxIOCompletionThrdPool);
begin
     fPool := pool;

     fCallIdx := 0;
     fRefCnt := PInteger( @fRefCntBuf[0] );

     if (TASMNativeUInt(fRefCnt) and $03) <> 0 then
        fRefCnt := PInteger( TASMNativeUInt(fRefCnt) + $04 - TASMNativeUInt(fRefCnt) and $3 );

     inherited Create;
end;

procedure TIOPortThreadGroup.OnFinish(Sender: Pointer);
begin
     PIOAsyncCall(Sender)^.fData.Free;
     
     InterlockedDecrement( fRefCnt^ );
end;

procedure TIOPortThreadGroup.SyncAll;
var failObj : Exception;
    failAddr : Pointer;
    i : Integer;
begin
     // ###########################################
     // #### Just spin wait until all references vanish
     while fRefCnt^ > 0 do
     begin
          TThread.SpinWait(100);
     end;

     failobj := nil;
     failAddr := nil;
     // ###########################################
     // #### Check if one task failed
     for i := 0 to fCallIdx - 1 do
     begin
          // only store one problem -> the others are freed
          if fTasks[i].FFatalException <> nil then
          begin
               if failObj <> nil
               then
                   // note: according to the help acqurieExceptionObject needs a
                   // call to ReleaseExceptionObject. But this function does nothing!
                   // TThread does also handle the excpetion like the way we do it here
                   // but frees the excpetion in the destructor (variable FFatalException) so 
                   // we do it likewise.
                   fTasks[i].FFatalException.Free
               else
               begin
                    failObj := fTasks[i].FFatalException;
                    failAddr := fTasks[i].FFatalErrorAddr;
               end;
          end;
     end;

     // ###########################################
     // #### Reraise if needed
     if Assigned( failObj ) then
        raise failObj at failAddr;
end;

procedure TIOPortThreadGroup.AddTaskRec(proc: TMtxRecProc; rec: Pointer);
begin
     InterlockedIncrement(fRefCnt^);
     ftasks[fCallIdx].CreateRec(proc, rec, OnFinish);
     fPool.AddAsyncCall(@ftasks[fCallIdx]);
     inc(fCallIdx);
end;

{ TIOAsyncCallThread }

const cShutdown = 0;

constructor TIOAsyncCallThread.Create(PortHandle : THandle);
begin
     fPortHandle := PortHandle;

     inherited Create(False);
end;

procedure TIOAsyncCallThread.Execute;
var bytesTransferred : DWORD;
    CompletionKey : ULONG_PTR;
    Overlapped: POverlapped;
    Task : PIOAsyncCall;
    finishedCalled : boolean;
begin
     {$IFNDEF FPC}
     NameThreadForDebugging('mrMath Worker Thread');
     {$ENDIF}
     while not Terminated do
     begin
          finishedCalled := True;
          task := nil;
          
          // perform two loops so we don't need to setup the exception record all the time
          try
             while not Terminated do
             begin
                  // ###########################################
                  // #### Just wait for the next task
                  if not GetQueuedCompletionStatus(fPortHandle, BytesTransferred, CompletionKey, Overlapped, INFINITE) then
                     RaiseLastOSError;

                  finishedCalled := False;
                  
                  // same as terminated flag
                  if CompletionKey = cShutdown then
                     exit;

                  Task := PIOAsyncCall(CompletionKey);
                  Task^.Execute;
          
                  // inform someone...
                  Task^.fOnFinish(Task);
                  finishedCalled := True;
             end;
          except
                On E : Exception do
                begin
                     if Assigned(Task) then
                     begin
                          Task^.FFatalException := Exception(AcquireExceptionObject);
                          Task^.FFatalErrorAddr := ExceptAddr;
                          if not finishedCalled then
                             Task^.fOnFinish(Task);   
                     end;
                end;
          end;
     end;
end;

procedure TIOAsyncCallThread.ForceTerminate;
begin  
     // send shutdown signal
     PostQueuedCompletionStatus( fPortHandle, 0, cShutdown, nil );
end;

{ TMtxIOCompletionThrdPool }

procedure TMtxIOCompletionThrdPool.AddAsyncCall(call: PIOAsyncCall);
begin
     if not PostQueuedCompletionStatus(fPoolHdl, 0, NativeUINT( call ), nil ) then 
        RaiseLastOSError;
end;

constructor TMtxIOCompletionThrdPool.Create;
begin
     fThreadList := TObjectList.Create(True);

     inherited Create;
end;

destructor TMtxIOCompletionThrdPool.Destroy;
var i : integer;
begin
     for i := 0 to fThreadList.Count - 1 do
         TIOAsyncCallThread(fThreadList[i]).ForceTerminate;

     // wait for threads to finish...
     fThreadList.Free;

     // release resources
     CloseHandle(fPoolHdl);

     inherited;
end;

procedure TIOAsyncCall.Create(proc : TMtxProc; obj : TObject; onFinish : TTaskFinEvt);
begin
     fOnFinish := onFinish;
     fProc := proc;
     fData := obj;
end;

procedure TIOAsyncCall.CreateRec(proc: TMtxRecProc; rec: Pointer; onFinish : TTaskFinEvt);
begin
     fOnFinish := onFinish;
     fRecProc := proc;
     fRec := rec;
end;

procedure TIOAsyncCall.Execute;
begin
     if Assigned(fRec)
     then
         fRecProc(fRec)
     else
         fProc(fData);
end;

procedure TMtxIOCompletionThrdPool.InitPool(maxNumThreads: integer);
var sysInfo : TSystemInfo;
    i: Integer;
    numAllocThreads : integer;
begin
     fNumThreads := 0;
     GetSystemInfo(sysInfo);
     fMaxThreads := sysInfo.dwNumberOfProcessors;

     if fMaxThreads > maxNumThreads then
        fMaxThreads := maxNumThreads;
     fNumCPU := sysInfo.dwNumberOfProcessors;

     numAllocThreads := fNumCPU;
     if fNumCPU > fMaxThreads then
        numAllocThreads := fMaxThreads;

     // ###########################################
     // #### Create the port
     fPoolHdl := CreateIoCompletionPort( INVALID_HANDLE_VALUE, 0, 0, numAllocThreads );

     if fPoolHdl = 0 then
        RaiseLastOSError;

     // ###########################################
     // #### Now allocate the given number of threads
     for i := 0 to numAllocThreads - 1 do
         fThreadList.Add( TIOAsyncCallThread.Create(fPoolHdl) );
end;


function TMtxIOCompletionThrdPool.CreateTaskGroup: IMtxAsyncCallGroup;
begin
     Result := TIOPortThreadGroup.Create(self);
end;

function CreateThreadPoolObj : IMtxThreadPool;
begin
     Result := TMtxIOCompletionThrdPool.Create;
end;

{$ENDIF}

end.
