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

procedure InitWinMtxThreadPool;
procedure FinalizeWinMtxThreadPool;
function InitWinThreadGroup : IMtxAsyncCallGroup;

{$ENDIF}
implementation
{$IFDEF MSWINDOWS}
uses Classes, Windows, SyncObjs, winCPUInfo;

type
  TWinMtxAsyncCall = class(TInterfacedObject, IMtxAsyncCall)
  private
    FEvent: THandle;
    FReturnValue: Integer;
    FFinished: Boolean;
    FFatalException: Exception;
    FFatalErrorAddr: Pointer;
    fData : TObject;
    fProc : TMtxProc;
    FForceDifferentThread: Boolean;
    procedure InternExecuteAsyncCall;
    procedure Quit(AReturnValue: Integer);
  protected
    { Decendants must implement this method. It is called  when the async call
      should be executed. }
    function ExecuteAsyncCall: Integer; 
  public
    constructor Create(proc : TMtxProc; obj : TObject);
    destructor Destroy; override;
    function _Release: Integer; stdcall;
    procedure ExecuteAsync;

    function GetEvent: Cardinal;

    function Sync: Integer;
    function Finished: Boolean;
    function GetResult: Integer;
    procedure ForceDifferentThread;
  end;

type
  { TWinMtxAsyncCallThread is a pooled thread. It looks itself for work. }
  TWinMtxAsyncCallThread = class(TThread)
  protected
    fWorking: Boolean;
    FCPUNum : integer;
    fSig : TSimpleEvent;
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

//    function GetNexTWinMtxAsyncCall(Thread: TWinMtxAsyncCallThread): TWinMtxAsyncCall; // called from the threads
    function AllocThread : TWinMtxAsyncCallThread;
  public
    procedure AddAsyncCall(call : TWinMtxAsyncCall);
//    function RemoveAsyncCall(Call: TWinMtxAsyncCall): Boolean;

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

{ TWinMtxAsyncCallThread }

constructor TWinMtxAsyncCallThread.Create(CPUNum: integer);
begin
     FCPUNum := CPUNum;
     FreeOnTerminate := True;
     fSig := TSimpleEvent.Create(nil, True, False, '');

     inherited Create(False);
end;

procedure TWinMtxAsyncCallThread.Execute;
var idx : integer;
{$IFDEF FPC}
    mask, procMask, sysmask : NativeUInt;
{$ELSE} {$IF CompilerVersion > 22}
    mask, procMask, sysmask : NativeUInt;
{$ELSE}
    mask, procMask, sysmask : DWord;
{$IFEND}
{$ENDIF}

    res : TWaitResult;
begin
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
     while not Terminated do
     begin
          res := fSig.WaitFor(1000);
          if Terminated or (res in [wrAbandoned, wrError]) then
             break;

          if res = wrSignaled then
          begin
               if Assigned(fTask) then
               begin
                    try
                       fTask.InternExecuteAsyncCall;
                    except
                    end;
               end;

               fSig.ResetEvent;
               fWorking := False;
               //asyncCall := ThreadPool.GetNexTWinMtxAsyncCall(Self); // calls Suspend if nothing has to be done.
               //if asyncCall <> nil then
//               begin
//                    try
//                       asyncCall.InternExecuteAsyncCall;
//                    except
//                    end;
//               end;
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

     inherited;
end;

procedure TWinMtxAsyncCallThread.StartTask(aTask: TWinMtxAsyncCall);
begin
     if not fWorking then
     begin
          fWorking := True;
          fTask := aTask;
          fSig.SetEvent;
     end;
end;

{ TMtxThreadPool }

procedure TMtxThreadPool.AddAsyncCall(call: TWinMtxAsyncCall);
var List: TList;
    FreeThreadFound: Boolean;
    I: Integer;
begin
     FreeThreadFound := False;
     List := fThreadList.LockList;
     try
        for I := 0 to List.Count - 1 do
        begin
             if not TWinMtxAsyncCallThread(List[I]).Working then
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
        AddAsyncCall(call);
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

//function TMtxThreadPool.GetNexTWinMtxAsyncCall(
//  Thread: TWinMtxAsyncCallThread): TWinMtxAsyncCall;
//var List: TList;
//begin
//     List := FAsyncCalls.LockList;
//     try
//        if List.Count > 0 then
//        begin
//             { Get the "oldest" async call }
//             Result := TWinMtxAsyncCall(List[0]);
//             List.Delete(0);
//        end
//        else
//            Result := nil;
//     finally
//            FAsyncCalls.UnlockList;
//     end;
//     { Nothing to do, go sleeping... }
//     if Result = nil then
//        Thread.SuspendThread;
//end;

//function TMtxThreadPool.RemoveAsyncCall(Call: TWinMtxAsyncCall): Boolean;
//var List: TList;
//    Index: Integer;
//begin
//     List := FAsyncCalls.LockList;
//     try
//       Index := List.IndexOf(Call);
//       Result := Index >= 0;
//       if Result then
//          List.Delete(Index);
//     finally
//            FAsyncCalls.UnlockList;
//     end;
//end;

constructor TWinMtxAsyncCall.Create(proc : TMtxProc; obj : TObject);
begin
     inherited Create;

     FEvent := CreateEvent(nil, True, False, nil);
     fProc := proc;
     fData := obj;
end;

destructor TWinMtxAsyncCall.Destroy;
begin
     if FEvent <> 0 then
     begin
          try
             Sync;
          finally
                 CloseHandle(FEvent);
                 FEvent := 0;
          end;
     end;

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

procedure TWinMtxAsyncCall.ForceDifferentThread;
begin
     FForceDifferentThread := True;
end;

function TWinMtxAsyncCall.GetEvent: Cardinal;
begin
     Result := FEvent;
end;

procedure TWinMtxAsyncCall.InternExecuteAsyncCall;
var Value: Integer;
begin
     Value := 0;
     assert(FFinished = False, 'Error finished may not be true');
     try
        Value := ExecuteAsyncCall;
     except
           FFatalErrorAddr := ErrorAddr;
           FFatalException := Exception(AcquireExceptionObject);
     end;
     Quit(Value);
end;

procedure TWinMtxAsyncCall.Quit(AReturnValue: Integer);
begin
     FReturnValue := AReturnValue;
     FFinished := True;
     SetEvent(FEvent);
end;

function TWinMtxAsyncCall.GetResult: Integer;
var E: Exception;
begin
     if not Finished then
        raise Exception.Create('IAsyncCall.ReturnValue');

     Result := FReturnValue;

     if FFatalException <> nil then
     begin
          E := FFatalException;
          FFatalException := nil;
          raise E at FFatalErrorAddr;
     end;
end;

function TWinMtxAsyncCall.Sync: Integer;
var E: Exception;
begin
     if not Finished then
     begin
          if WaitForSingleObject(FEvent, INFINITE) <> WAIT_OBJECT_0 then
             raise Exception.Create('IAsyncCall.Sync');
     end;
     Result := FReturnValue;

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

function TWinMtxAsyncCall.ExecuteAsyncCall: Integer;
begin
     Result := fProc(fData);
end;


{$ENDIF}

end.
