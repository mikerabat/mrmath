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
uses MatrixConst, MtxThreadPool, SysUtils;

procedure InitWinMtxThreadPool;
procedure FinalizeWinMtxThreadPool;
function InitWinThreadGroup : IMtxAsyncCallGroup;

{$ENDIF}
implementation
{$IFDEF MSWINDOWS}
uses Classes, Windows, SyncObjs;

var sysInfo : TSystemInfo;

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
    procedure InternExecuteSyncCall;
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
    function SyncInThisThreadIfPossible: Boolean;

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
    FSuspended: Boolean;
    FCPUNum : integer;
    procedure Execute; override;
  public
    procedure ForceTerminate;
    procedure SuspendThread;
    procedure ResumeThread;

    property Suspended: Boolean read FSuspended;

    constructor Create(CPUNum : integer);
  end;

type
  TMtxThreadPool = class(TObject)
  private
    fThreadList : TThreadList;
    fAsyncCalls : TThreadList;
    fMaxThreads: integer;
    fNumThreds : integer;
    fNumCPU : integer;

    function GetNexTWinMtxAsyncCall(Thread: TWinMtxAsyncCallThread): TWinMtxAsyncCall; // called from the threads
    function AllocThread : TWinMtxAsyncCallThread;
  public
    procedure AddAsyncCall(call : TWinMtxAsyncCall);
    function RemoveAsyncCall(Call: TWinMtxAsyncCall): Boolean;

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

     inherited Create(True);
end;

procedure TWinMtxAsyncCallThread.Execute;
var asyncCall : TWinMtxAsyncCall;
{$IF CompilerVersion > 22}
    mask, procMask, sysmask : NativeUInt;
{$ELSE}
    mask, procMask, sysmask : DWord;
{$IFEND}
    idx : integer;
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
          asyncCall := ThreadPool.GetNexTWinMtxAsyncCall(Self); // calls Suspend if nothing has to be done.
          if asyncCall <> nil then
          begin
               try
                  asyncCall.InternExecuteAsyncCall;
               except
               end;
          end;
     end;
end;

procedure TWinMtxAsyncCallThread.ForceTerminate;
begin
     if Suspended then
     begin
          Terminate;
          ResumeThread;
     end
     else
         Terminate;
end;

procedure TWinMtxAsyncCallThread.ResumeThread;
begin
     FSuspended := False;
     Windows.ResumeThread(Handle);
end;

procedure TWinMtxAsyncCallThread.SuspendThread;
begin
     FSuspended := True;
     Windows.SuspendThread(Handle);
end;

{ TMtxThreadPool }

procedure TMtxThreadPool.AddAsyncCall(call: TWinMtxAsyncCall);
var List: TList;
    FreeThreadFound: Boolean;
    I: Integer;
begin
     List := FAsyncCalls.LockList;
     List.Add(Call);
     FAsyncCalls.UnlockList;

     FreeThreadFound := False;
     List := fThreadList.LockList;
     try
        for I := 0 to List.Count - 1 do
        begin
             if TWinMtxAsyncCallThread(List[I]).Suspended then
             begin
               { Wake up the thread so it can execute the waiting async call. }
               TWinMtxAsyncCallThread(List[I]).ResumeThread;
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
end;

function TMtxThreadPool.AllocThread : TWinMtxAsyncCallThread;
var cpuIdx : integer;
begin
     cpuIdx := InterlockedIncrement(fNumThreds);

     if cpuIdx > fNumCPU then
        cpuIdx := -1;

     Result := TWinMtxAsyncCallThread.Create(cpuIdx - 1);
     fThreadList.Add(Result);

     Result.ResumeThread;
end;

constructor TMtxThreadPool.Create;
var sysInfo : TSystemInfo;
    i: Integer;
begin
     inherited Create;

     fNumThreds := 0;
     GetSystemInfo(sysInfo);
     fThreadList := TThreadList.Create;
     fAsyncCalls := TThreadList.Create;
     fMaxThreads := sysInfo.dwNumberOfProcessors;
     fNumCPU := sysInfo.dwNumberOfProcessors;

     for i := 0 to fNumCPU - 1 do
         AllocThread.SuspendThread;
end;

destructor TMtxThreadPool.Destroy;
var list : TList;
    i : integer;
begin
     list := fThreadList.LockList;
     try
        for i := 0 to list.Count - 1 do
        begin
             TWinMtxAsyncCallThread(list[i]).Terminate;
             TWinMtxAsyncCallThread(list[i]).ResumeThread;
        end;
     finally
            fThreadList.UnlockList;
     end;
     fThreadList.Free;

     list := fAsyncCalls.LockList;
     try
        for i := 0 to list.Count - 1 do
            TObject(list[i]).Free;
     finally
            fAsyncCalls.UnlockList;
     end;
     fAsyncCalls.Free;

     inherited;
end;

function TMtxThreadPool.GetNexTWinMtxAsyncCall(
  Thread: TWinMtxAsyncCallThread): TWinMtxAsyncCall;
var List: TList;
begin
     List := FAsyncCalls.LockList;
     try
        if List.Count > 0 then
        begin
             { Get the "oldest" async call }
             Result := List[0];
             List.Delete(0);
        end
        else
            Result := nil;
     finally
            FAsyncCalls.UnlockList;
     end;
     { Nothing to do, go sleeping... }
     if Result = nil then
       Thread.SuspendThread;
end;

function TMtxThreadPool.RemoveAsyncCall(Call: TWinMtxAsyncCall): Boolean;
var List: TList;
    Index: Integer;
begin
     List := FAsyncCalls.LockList;
     try
       Index := List.IndexOf(Call);
       Result := Index >= 0;
       if Result then
          List.Delete(Index);
     finally
            FAsyncCalls.UnlockList;
     end;
end;

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

function TWinMtxAsyncCall._Release: Integer;
begin
     Result := InterlockedDecrement(FRefCount);
     if Result = 0 then
     begin
          try
             if FEvent <> 0 then
                Sync;
          finally
                 Destroy;
          end;
     end;
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
           FFatalException := AcquireExceptionObject;
     end;
     Quit(Value);
end;

procedure TWinMtxAsyncCall.InternExecuteSyncCall;
begin
     Quit( ExecuteAsyncCall() );
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
          if not SyncInThisThreadIfPossible then
          begin
               if WaitForSingleObject(FEvent, INFINITE) <> WAIT_OBJECT_0 then
                  raise Exception.Create('IAsyncCall.Sync');
          end;
     end;
     Result := FReturnValue;

     if FFatalException <> nil then
     begin
          E := FFatalException;
          FFatalException := nil;

          raise E at FFatalErrorAddr;
     end;
end;

function TWinMtxAsyncCall.SyncInThisThreadIfPossible: Boolean;
begin
     if not Finished then
     begin
          Result := False;
          if not FForceDifferentThread then
          begin
               { If no thread was assigned to this async call, remove it form the waiting
                 queue and execute it in the current thread. }
               if ThreadPool.RemoveAsyncCall(Self) then
               begin
                    InternExecuteSyncCall;
                    Result := True;
               end;
          end;
     end
     else
         Result := True;
end;

procedure TWinMtxAsyncCall.ExecuteAsync;
begin
     ThreadPool.AddAsyncCall(Self);
end;

function TWinMtxAsyncCall.ExecuteAsyncCall: Integer;
begin
     Result := fProc(fData);
end;

initialization
  GetSystemInfo(SysInfo);
  numCPUCores := SysInfo.dwNumberOfProcessors;
finalization
{$ENDIF}
end.
