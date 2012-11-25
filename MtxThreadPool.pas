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


unit MtxThreadPool;

// #####################################################
// #### Thread pool for async matrix operations
// #####################################################

// this thread pool is basically a slim version of Andreas Hausladesn Async Call library!
// http://andy.jgknet.de/blog/bugfix-units/asynccalls-29-asynchronous-function-calls/
interface

uses ASMConsts;

type
  TMtxProc = function(obj : TObject) : integer;
  TMtxDataProc = function(const data : Pointer) : integer;

procedure InitMtxThreadPool;
procedure FinalizeMtxThreadPool;

type
  IMtxAsyncCall = interface
   ['{B5263EB3-FFDE-4D66-B556-31D5E0D05BAC}']
    function Sync: Integer;
    function Finished: Boolean;
    function ReturnValue: Integer;
    procedure ForceDifferentThread;
  end;

function MtxAsyncCall(proc : TMtxProc; obj : TObject) : IMtxAsyncCall;
function MtxAsyncDataCall(proc : TMtxDataProc; const data : Pointer) : IMtxAsyncCall;

var numCPUCores : TASMNativeInt = 0;

implementation

uses SysUtils, Classes, Windows, SyncObjs, Math;

var sysInfo : TSystemInfo;

type
  TMtxAsyncCall = class(TInterfacedObject, IMtxAsyncCall)
  private
    FEvent: THandle;
    FReturnValue: Integer;
    FFinished: Boolean;
    FFatalException: Exception;
    FFatalErrorAddr: Pointer;
    FForceDifferentThread: Boolean;
    procedure InternExecuteAsyncCall;
    procedure InternExecuteSyncCall;
    procedure Quit(AReturnValue: Integer);
  protected
    { Decendants must implement this method. It is called  when the async call
      should be executed. }
    function ExecuteAsyncCall: Integer; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;
    function _Release: Integer; stdcall;
    function ExecuteAsync: TMtxAsyncCall;
    function SyncInThisThreadIfPossible: Boolean;

    function GetEvent: Cardinal;

    function Sync: Integer;
    function Finished: Boolean;
    function ReturnValue: Integer;
    procedure ForceDifferentThread;
  end;

  TProcMtxAsyncCall = class(TMtxAsyncCall)
  private
    fObj : TObject;
    fProc : TMtxProc;
  public
    function ExecuteAsyncCall : integer; override;

    constructor Create(proc : TMtxProc; obj : TObject);
  end;

  TDataMtxAsyncCall = class(TMtxAsyncCall)
  private
    fData : Pointer;
    fProc : TMtxDataProc;
  public
    function ExecuteAsyncCall : integer; override;

    constructor Create(proc : TMtxDataProc; const data : Pointer);
  end;

  { TMtxAsyncCallThread is a pooled thread. It looks itself for work. }
  TMtxAsyncCallThread = class(TThread)
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

    function GetNexTMtxAsyncCall(Thread: TMtxAsyncCallThread): TMtxAsyncCall; // called from the threads
    function AllocThread : TMtxAsyncCallThread;
  public
    procedure AddAsyncCall(call : TMtxAsyncCall);
    function RemoveAsyncCall(Call: TMtxAsyncCall): Boolean;

    property MaxThreads : integer read fMaxThreads write fMaxThreads;

    constructor Create;
    destructor Destroy; override;
  end;

var threadPool : TMtxThreadPool = nil;

procedure InitMtxThreadPool;
begin
     threadPool := TMtxThreadPool.Create;
end;

procedure FinalizeMtxThreadPool;
begin
     threadPool.Free;
end;

function MtxAsyncCall(proc : TMtxProc; obj : TObject) : IMtxAsyncCall;
begin
     { Execute the function synchron if no thread pool exists }
     assert(Assigned(threadPool), 'Error InitMtxThreadPool not called');
     assert(threadPool.MaxThreads > 0);

     Result := TProcMtxAsyncCall.Create(Proc, obj).ExecuteAsync;
end;

function MtxAsyncDataCall(proc : TMtxDataProc; const data : Pointer) : IMtxAsyncCall;
begin
     { Execute the function synchron if no thread pool exists }
     assert(Assigned(threadPool), 'Error InitMtxThreadPool not called');
     assert(threadPool.MaxThreads > 0);

     Result := TDataMtxAsyncCall.Create(Proc, data).ExecuteAsync;
end;


{ TMtxAsyncCallThread }

constructor TMtxAsyncCallThread.Create(CPUNum: integer);
begin
     FCPUNum := CPUNum;
     FreeOnTerminate := True;

     inherited Create(True);
end;

procedure TMtxAsyncCallThread.Execute;
var asyncCall : TMtxAsyncCall;
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
          asyncCall := ThreadPool.GetNexTMtxAsyncCall(Self); // calls Suspend if nothing has to be done.
          if asyncCall <> nil then
          begin
               try
                  asyncCall.InternExecuteAsyncCall;
               except
               end;
          end;
     end;
end;

procedure TMtxAsyncCallThread.ForceTerminate;
begin
     if Suspended then
     begin
          Terminate;
          ResumeThread;
     end
     else
         Terminate;
end;

procedure TMtxAsyncCallThread.ResumeThread;
begin
     FSuspended := False;
     Windows.ResumeThread(Handle);
end;

procedure TMtxAsyncCallThread.SuspendThread;
begin
     FSuspended := True;
     Windows.SuspendThread(Handle);
end;

{ TMtxThreadPool }

procedure TMtxThreadPool.AddAsyncCall(call: TMtxAsyncCall);
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
             if TMtxAsyncCallThread(List[I]).Suspended then
             begin
               { Wake up the thread so it can execute the waiting async call. }
               TMtxAsyncCallThread(List[I]).ResumeThread;
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

function TMtxThreadPool.AllocThread : TMtxAsyncCallThread;
var cpuIdx : integer;
begin
     cpuIdx := InterlockedIncrement(fNumThreds);

     if cpuIdx > fNumCPU then
        cpuIdx := -1;

     Result := TMtxAsyncCallThread.Create(cpuIdx - 1);
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
             TMtxAsyncCallThread(list[i]).Terminate;
             TMtxAsyncCallThread(list[i]).ResumeThread;
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

function TMtxThreadPool.GetNexTMtxAsyncCall(
  Thread: TMtxAsyncCallThread): TMtxAsyncCall;
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

function TMtxThreadPool.RemoveAsyncCall(Call: TMtxAsyncCall): Boolean;
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

{ TProcMtxAsyncCall }

constructor TProcMtxAsyncCall.Create(proc: TMtxProc; obj: TObject);
begin
     inherited Create;

     fProc := proc;
     fObj := obj;
end;

function TProcMtxAsyncCall.ExecuteAsyncCall : integer;
begin
     Result := fProc(fObj);
end;


constructor TMtxAsyncCall.Create;
begin
     inherited Create;
     FEvent := CreateEvent(nil, True, False, nil);
end;

destructor TMtxAsyncCall.Destroy;
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

     inherited Destroy;
end;

function TMtxAsyncCall._Release: Integer;
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

function TMtxAsyncCall.Finished: Boolean;
begin
     Result := (FEvent = 0) or FFinished or (WaitForSingleObject(FEvent, 0) = WAIT_OBJECT_0);
end;

procedure TMtxAsyncCall.ForceDifferentThread;
begin
     FForceDifferentThread := True;
end;

function TMtxAsyncCall.GetEvent: Cardinal;
begin
     Result := FEvent;
end;

procedure TMtxAsyncCall.InternExecuteAsyncCall;
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

procedure TMtxAsyncCall.InternExecuteSyncCall;
begin
     Quit( ExecuteAsyncCall() );
end;

procedure TMtxAsyncCall.Quit(AReturnValue: Integer);
begin
     FReturnValue := AReturnValue;
     FFinished := True;
     SetEvent(FEvent);
end;

function TMtxAsyncCall.ReturnValue: Integer;
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

function TMtxAsyncCall.Sync: Integer;
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

function TMtxAsyncCall.SyncInThisThreadIfPossible: Boolean;
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

function TMtxAsyncCall.ExecuteAsync: TMtxAsyncCall;
begin
     ThreadPool.AddAsyncCall(Self);
     Result := Self;
end;

{ TDataMtxAsyncCall }

constructor TDataMtxAsyncCall.Create(proc: TMtxDataProc; const data : Pointer);
begin
     fData := data;
     fProc := proc;

     inherited Create;
end;

function TDataMtxAsyncCall.ExecuteAsyncCall: integer;
begin
     Result := fProc(fData);
end;

initialization
  GetSystemInfo(SysInfo);
  numCPUCores := SysInfo.dwNumberOfProcessors;
finalization

end.
