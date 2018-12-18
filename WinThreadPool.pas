// ###################################################################
// #### This file is part of the mathematics library project, and is
// #### offered under the licence agreement described on
// #### http://www.mrsoft.org/
// ####
// #### Copyright:(c) 2011, Michael R. . All rights reserved.
// ####
// #### Unless required by applicable law or agreed to in writing, software
// #### distributed under the License is distributed on an "AS IS" BASIS,
// #### WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// #### See the License for the specific language governing permissions and
// #### limitations under the License.
// ###################################################################


unit WinThreadPool;

// #####################################################
// #### Thread pool for async matrix operations
// #####################################################

// this thread pool is basically a slim version of Andreas Hausladesn Async Call library!
// http://andy.jgknet.de/blog/bugfix-units/asynccalls-29-asynchronous-function-calls/
interface
{$IFDEF MSWINDOWS}
uses MtxThreadPool, SysUtils;

{.$DEFINE USE_THREAD_CPU_AFFINITY}

procedure InitWinMtxThreadPool;
procedure FinalizeWinMtxThreadPool;
function InitWinThreadGroup : IMtxAsyncCallGroup;

{$ENDIF}
implementation
{$IFDEF MSWINDOWS}
uses Classes, 
     {$IFDEF FPC} Windows 
     {$ELSE} 
     {$IF CompilerVersion >= 23.0} Winapi.Windows {$ELSE} Windows {$IFEND} 
     {$ENDIF}, SyncObjs, winCPUInfo, CPUFeatures;

type
  TWinMtxAsyncCall = class(TInterfacedObject, IMtxAsyncCall)
  private
    FEvent: THandle;
    FFinished: Boolean;
    FFatalException: Exception;
    FFatalErrorAddr: Pointer;
    fData : TObject;
    fProc : TMtxProc;
    fRec : Pointer;
    fRecProc : TMtxRecProc;

    procedure InternExecuteAsyncCall;
    procedure Quit;
  protected
    procedure ExecuteAsyncCall;
  public
    constructor Create(proc : TMtxProc; obj : TObject);
    constructor CreateRec(proc : TMtxRecProc; rec : Pointer);
    destructor Destroy; override;
    function _Release: Integer; stdcall;
    procedure ExecuteAsync;

    procedure Sync;
    function Finished: Boolean;
  end;

type
  { TWinMtxAsyncCallThread is a pooled thread. It looks itself for work. }
  TWinMtxAsyncCallThread = class(TThread)
  protected
    fWorking: Boolean;
    FCPUNum : integer;
    fSig : TSimpleEvent;
    fEvent : THandle;
    fTask : TWinMtxAsyncCall;
    procedure Execute; override;
  public
    procedure ForceTerminate;

    property Working: Boolean read fWorking;
    procedure StartTask(aTask : TWinMtxAsyncCall);

    constructor Create(CPUNum : integer);
    destructor Destroy; override;
  end;

type
  TMtxThreadPool = class(TObject)
  private
    fThreadList : TThreadList;
    fMaxThreads: integer;
    fNumThreads : integer;
    fNumCPU : integer;

    function AllocThread : TWinMtxAsyncCallThread;
  public
    procedure AddAsyncCall(call : TWinMtxAsyncCall);

    property MaxThreads : integer read fMaxThreads write fMaxThreads;

    constructor Create;
    destructor Destroy; override;
  end;

var threadPool : TMtxThreadPool = nil;

type
  TSimpleWinThreadGroup = class(TInterfacedObject, IMtxAsyncCallGroup)
  private
    fTaskList : IInterfaceList;
  public
    procedure AddTaskRec(proc : TMtxRecProc; rec : Pointer);
    procedure AddTask(proc : TMtxProc; obj : TObject);
    procedure SyncAll;

    constructor Create;
  end;

{ TSimpleWinThreadGroup }

procedure TSimpleWinThreadGroup.AddTask(proc : TMtxProc; obj : TObject);
var aTask : IMtxAsyncCall;
begin
     aTask := TWinMtxAsyncCall.Create(proc, obj);
     fTaskList.Add(aTask);
     aTask.ExecuteAsync;
end;

constructor TSimpleWinThreadGroup.Create;
begin
     fTaskList := TInterfaceList.Create;
     fTaskList.Capacity := numCPUCores;

     inherited Create;
end;

procedure TSimpleWinThreadGroup.SyncAll;
var i : integer;
    aTask : IMtxAsyncCall;
begin
     for i := 0 to fTaskList.Count - 1 do
     begin
          aTask := fTaskList[i] as IMtxAsyncCall;
          aTask.Sync;
     end;
end;

function InitWinThreadGroup : IMtxAsyncCallGroup;
begin
     Assert(Assigned(threadPool), 'Error thread pool not initialized. Call InitMtxThreadPool first');
     Result := TSimpleWinThreadGroup.Create;
end;

procedure InitWinMtxThreadPool;
begin
     Assert(Not Assigned(threadPool), 'Error thread pool already initialized. Call FinalizeMtxThreadPool first');
     threadPool := TMtxThreadPool.Create;
end;

procedure FinalizeWinMtxThreadPool;
begin
     Assert(Assigned(threadPool), 'Error thread pool not initialized. Call InitMtxThreadPool first');
     FreeAndNil(threadPool);
end;

procedure TSimpleWinThreadGroup.AddTaskRec(proc: TMtxRecProc; rec: Pointer);
var aTask : IMtxAsyncCall;
begin
     aTask := TWinMtxAsyncCall.CreateRec(proc, rec);
     fTaskList.Add(aTask);
     aTask.ExecuteAsync;
end;


{ TWinMtxAsyncCallThread }

constructor TWinMtxAsyncCallThread.Create(CPUNum: integer);
begin
     FCPUNum := CPUNum;
     //fCPUNum := -1;
     FreeOnTerminate := True;
     fSig := TSimpleEvent.Create;
     FEvent := CreateEvent(nil, True, False, nil);

     inherited Create(False);
end;

procedure TWinMtxAsyncCallThread.Execute;
var res : TWaitResult;
{$IFDEF USE_THREAD_CPU_AFFINITY}
    idx : integer;
{$IFDEF FPC}
    mask, procMask, sysmask : NativeUInt;
{$ELSE} {$IF CompilerVersion > 22}
    mask, procMask, sysmask : NativeUInt;
{$ELSE}
    mask, procMask, sysmask : DWord;
{$IFEND}
{$ENDIF}
{$ENDIF}
begin
     {$IFDEF USE_THREAD_CPU_AFFINITY}
     if FCPUNum >= 0 then
     begin
          GetProcessAffinityMask(GetCurrentProcess, procMask, sysmask);
          mask := 1;

          idx := FCPUNum;
          while (((mask and procMask) = 0) or (idx > 0)) and (mask <> 0) do
          begin
               if (mask and procMask) <> 0 then
                  dec(idx);

               mask := mask shl 1;
          end;

          if mask > 0 then
             SetThreadAffinityMask(Handle, mask);
     end;
     {$ENDIF}
     while not Terminated do
     begin
          res := fSig.WaitFor(1000);
          if Terminated or (res in [wrAbandoned, wrError]) then
             break;

          if res = wrSignaled then
          begin
               if Assigned(fTask) then
               begin
                    //try
                       fTask.InternExecuteAsyncCall;
                    //except
                    //end;
               end;

               fSig.ResetEvent;
               fWorking := False;
          end;
     end;
end;

procedure TWinMtxAsyncCallThread.ForceTerminate;
begin
     Terminate;
     fSig.SetEvent;
end;


destructor TWinMtxAsyncCallThread.Destroy;
begin
     fSig.Free;
     CloseHandle(fEvent);

     inherited;
end;

procedure TWinMtxAsyncCallThread.StartTask(aTask: TWinMtxAsyncCall);
begin
     assert(not fWorking, 'Though checked for not working - it''s working?!?!');

     fTask := aTask;
     fWorking := True;
     fTask.FEvent := fEvent;
     ResetEvent(fEvent);
     fSig.SetEvent;
end;

{ TMtxThreadPool }

procedure TMtxThreadPool.AddAsyncCall(call: TWinMtxAsyncCall);
var List: TList;
    FreeThreadFound: Boolean;
    I: Integer;
    cpuNum : DWord;
    obj : TWinMtxAsyncCallThread;
begin
     FreeThreadFound := False;

     List := fThreadList.LockList;
     try
        cpuNum := GetCurrentProcessorNumber;
        for I := 0 to List.Count - 1 do
        begin
             obj := TWinMtxAsyncCallThread(List[I]);
             
             // since the threads containing a cpunum >= 0 are masked we avoid using the same cpu
             if not obj.Working and (obj.FCPUNum <> Integer(cpuNum)) then
             begin
                  // Wake up the thread so it can execute the waiting async call.
                  TWinMtxAsyncCallThread(List[I]).StartTask(call);
                  FreeThreadFound := True;
                  Break;
             end;
        end;
        { All threads are busy, we need to allocate another thread if possible }
        if not FreeThreadFound and (List.Count < MaxThreads) then
           AllocThread;
     finally
            fThreadList.UnlockList;
     end;

     // try again -> a new thread has been created
     if not FreeThreadFound then
        call.InternExecuteAsyncCall;
end;

function TMtxThreadPool.AllocThread : TWinMtxAsyncCallThread;
var cpuIdx : integer;
begin
     cpuIdx := InterlockedIncrement(fNumThreads);

     if cpuIdx > fNumCPU then
        cpuIdx := -1;

     Result := TWinMtxAsyncCallThread.Create(cpuIdx - 1);
     fThreadList.Add(Result);
end;

constructor TMtxThreadPool.Create;
var sysInfo : TSystemInfo;
    i: Integer;
begin
     inherited Create;

     fNumThreads := 0;
     GetSystemInfo(sysInfo);
     fThreadList := TThreadList.Create;
     fMaxThreads := sysInfo.dwNumberOfProcessors;
     fNumCPU := sysInfo.dwNumberOfProcessors;

     for i := 0 to fNumCPU - 1 do
         AllocThread;
end;

destructor TMtxThreadPool.Destroy;
var list : TList;
    i : integer;
begin
     list := fThreadList.LockList;
     try
        for i := 0 to list.Count - 1 do
            TWinMtxAsyncCallThread(list[i]).ForceTerminate;
     finally
            fThreadList.UnlockList;
     end;
     fThreadList.Free;

     inherited;
end;

constructor TWinMtxAsyncCall.Create(proc : TMtxProc; obj : TObject);
begin
     inherited Create;

     FEvent := 0;

     fProc := proc;
     fData := obj;
end;

constructor TWinMtxAsyncCall.CreateRec(proc: TMtxRecProc; rec: Pointer);
begin
     inherited Create;

     FEvent := 0;

     fRecProc := proc;
     fRec := rec;
end;


destructor TWinMtxAsyncCall.Destroy;
begin
     fData.Free;

     inherited Destroy;
end;

function TWinMtxAsyncCall._Release: Integer; stdcall;
begin
     Result := InterlockedDecrement(FRefCount);
     if Result = 0 then
        Destroy;
end;

function TWinMtxAsyncCall.Finished: Boolean;
begin
     Result := (FEvent = 0) or FFinished or (WaitForSingleObject(FEvent, 0) = WAIT_OBJECT_0);
end;

procedure TWinMtxAsyncCall.InternExecuteAsyncCall;
begin
     assert(FFinished = False, 'Error finished may not be true');
     try
        ExecuteAsyncCall;
     except
           FFatalErrorAddr := ErrorAddr;
           FFatalException := Exception(AcquireExceptionObject);
     end;
     Quit;
end;

procedure TWinMtxAsyncCall.Quit;
begin
     FFinished := True;
     SetEvent(FEvent);
end;

procedure TWinMtxAsyncCall.Sync;
var E: Exception;
begin
     if not Finished then
     begin
          if WaitForSingleObject(FEvent, INFINITE) <> WAIT_OBJECT_0 then
             raise Exception.Create('IAsyncCall.Sync');
     end;

     FEvent := 0;
     if FFatalException <> nil then
     begin
          E := FFatalException;
          FFatalException := nil;

          raise E at FFatalErrorAddr;
     end;
end;

procedure TWinMtxAsyncCall.ExecuteAsync;
begin
     ThreadPool.AddAsyncCall(Self);
end;

procedure TWinMtxAsyncCall.ExecuteAsyncCall;
begin
     if Assigned(fRec)
     then
         fRecProc(fRec)
     else
         fProc(fData);
end;


{$ENDIF}

end.
