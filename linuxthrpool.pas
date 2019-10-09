// ###################################################################
// #### This file is part of the mathematics library project, and is
// #### offered under the licence agreement described on
// #### http://www.mrsoft.org/
// ####
// #### Copyright:(c) 2017, Michael R. . All rights reserved.
// ####
// #### Unless required by applicable law or agreed to in writing, software
// #### distributed under the License is distributed on an "AS IS" BASIS,
// #### WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// #### See the License for the specific language governing permissions and
// #### limitations under the License.
// ###################################################################


unit linuxthrpool;

// #####################################################
// #### Thread pool for async matrix operations
// #####################################################

interface

{$IFDEF LINUX}
uses MtxThreadPool, SysUtils;

{$ENDIF}
implementation
{$IFDEF LINUX}
uses Classes, SyncObjs, ctypes;

const _SC_NPROCESSORS_ONLN = 83;

function sysconf(i : cint): clong; cdecl; external name 'sysconf';

type
  TMtxThreadPool = class;
  TLinuxMtxAsyncCall = class(TInterfacedObject, IMtxAsyncCall)
  private
    FEvent: TEvent;
    FFinished: Boolean;
    FFatalException: Exception;
    FFatalErrorAddr: Pointer;
    fData : TObject;
    fProc : TMtxProc;
    fRecProc : TMtxRecProc;
    fRec : Pointer;
    FForceDifferentThread: Boolean;
    fPool : TMtxThreadPool;
    procedure InternExecuteAsyncCall;
    procedure Quit;
  protected
    { Decendants must implement this method. It is called  when the async call
      should be executed. }
    procedure ExecuteAsyncCall;
  public
    constructor Create(pool : TMtxThreadPool; proc : TMtxProc; obj : TObject);
    constructor CreateRec(pool : TMtxThreadPool; proc : TMtxRecProc; rec : pointer);

    destructor Destroy; override;
    function _Release: Integer; stdcall;
    procedure ExecuteAsync;

    function GetEvent: TEvent;

    procedure Sync;
    function Finished : Boolean;
    procedure ForceDifferentThread;
  end;

  { TLinuxMtxAsyncCallThread is a pooled thread. It looks itself for work. }
  TLinuxMtxAsyncCallThread = class(TThread)
  protected
    fWorking: Boolean;
    FCPUNum : integer;
    fSig : TEvent;
    fTask : TLinuxMtxAsyncCall;
    procedure Execute; override;
  public
    procedure ForceTerminate;

    property Working: Boolean read fWorking;
    procedure StartTask(aTask : TLinuxMtxAsyncCall);

    constructor Create(CPUNum : integer);
    destructor Destroy; override;
  end;

  TMtxThreadPool = class(TInterfacedObject, IMtxThreadPool)
  private
    fThreadList : TThreadList;
    fMaxThreads: integer;
    fNumThreads : integer;
    fNumCPU : integer;

    function AllocThread : TLinuxMtxAsyncCallThread;
  public
    procedure AddAsyncCall(call : TLinuxMtxAsyncCall);

    procedure InitPool( maxNumThreads : integer );
    function CreateTaskGroup : IMtxAsyncCallGroup;

    property MaxThreads : integer read fMaxThreads write fMaxThreads;

    constructor Create;
    destructor Destroy; override;
  end;

type
  TSimpleLinuxThreadGroup = class(TInterfacedObject, IMtxAsyncCallGroup)
  private
    fTaskList : IInterfaceList;
    fPool : TMtxThreadPool;
  public
    procedure AddTask(proc : TMtxProc; obj : TObject);
    procedure AddTaskRec(proc : TMtxRecProc; rec : Pointer);
    procedure SyncAll;

    constructor Create(pool : TMtxThreadPool);
  end;

{ TSimpleLinuxThreadGroup }

procedure TSimpleLinuxThreadGroup.AddTask(proc : TMtxProc; obj : TObject);
var aTask : IMtxAsyncCall;
begin
     aTask := TLinuxMtxAsyncCall.Create(fPool, proc, obj);
     fTaskList.Add(aTask);
     aTask.ExecuteAsync;
end;

procedure TSimpleLinuxThreadGroup.AddTaskRec(proc : TMtxRecProc; rec : Pointer);
var aTask : IMtxAsyncCall;
begin
    aTask := TLinuxMtxAsyncCall.CreateRec(fPool, proc, rec);
    fTaskList.Add(aTask);
    aTask.ExecuteAsync;
end;

constructor TSimpleLinuxThreadGroup.Create(pool : TMtxThreadPool);
begin
     fTaskList := TInterfaceList.Create;
     fTaskList.Capacity := numCPUCores;
     fPool := pool;

     inherited Create;
end;

procedure TSimpleLinuxThreadGroup.SyncAll;
var i : integer;
    aTask : IMtxAsyncCall;
begin
     for i := 0 to fTaskList.Count - 1 do
     begin
          aTask := fTaskList[i] as IMtxAsyncCall;
          aTask.Sync;
     end;
end;

{ TLinuxMtxAsyncCallThread }

constructor TLinuxMtxAsyncCallThread.Create(CPUNum: integer);
begin
     FCPUNum := CPUNum;
     FreeOnTerminate := True;
     fSig := TEvent.Create(nil, True, False, '');

     inherited Create(False);
end;

procedure TLinuxMtxAsyncCallThread.Execute;
var res : TWaitResult;
begin
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
          end;
     end;
end;

procedure TLinuxMtxAsyncCallThread.ForceTerminate;
begin
     Terminate;
     fSig.SetEvent;
end;


destructor TLinuxMtxAsyncCallThread.Destroy;
begin
     fSig.Free;

     inherited;
end;

procedure TLinuxMtxAsyncCallThread.StartTask(aTask: TLinuxMtxAsyncCall);
begin
     if not fWorking then
     begin
          fTask := aTask;
          fWorking := True;
          fSig.SetEvent;
     end;
end;

{ TMtxThreadPool }

procedure TMtxThreadPool.AddAsyncCall(call: TLinuxMtxAsyncCall);
var List: TList;
    FreeThreadFound: Boolean;
    I: Integer;
begin
     FreeThreadFound := False;
     List := fThreadList.LockList;
     try
        for I := 0 to List.Count - 1 do
        begin
             if not TLinuxMtxAsyncCallThread(List[I]).Working then
             begin
                  // Wake up the thread so it can execute the waiting async call.
                  TLinuxMtxAsyncCallThread(List[I]).StartTask(call);
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

function TMtxThreadPool.AllocThread : TLinuxMtxAsyncCallThread;
var cpuIdx : integer;
begin
     cpuIdx := InterlockedIncrement(fNumThreads);

     if cpuIdx > fNumCPU then
        cpuIdx := -1;

     Result := TLinuxMtxAsyncCallThread.Create(cpuIdx - 1);
     fThreadList.Add(Result);
end;

procedure TMtxThreadPool.InitPool( maxNumThreads : integer );
var i: Integer;
    t : cint;
begin
     t := sysconf( _SC_NPROCESSORS_ONLN);

     fNumCPU := t;
     fNumThreads := t;
     fMaxThreads := t;

     for i := 0 to fNumCPU - 1 do
         AllocThread;
end;

function TMtxThreadPool.CreateTaskGroup : IMtxAsyncCallGroup;
begin
     Result := TSimpleLinuxThreadGroup.Create(self);
end;

constructor TMtxThreadPool.Create;
begin
     inherited Create;

     fThreadList := TThreadList.Create;
end;

destructor TMtxThreadPool.Destroy;
var list : TList;
    i : integer;
begin
     list := fThreadList.LockList;
     try
        for i := 0 to list.Count - 1 do
            TLinuxMtxAsyncCallThread(list[i]).ForceTerminate;
     finally
            fThreadList.UnlockList;
     end;
     fThreadList.Free;

     inherited;
end;


constructor TLinuxMtxAsyncCall.Create(pool : TMtxThreadPool; proc : TMtxProc; obj : TObject);
begin
     inherited Create;

     fPool := pool;
     FEvent := TEvent.Create(nil, True, False, '');
     fProc := proc;
     fData := obj;
end;

constructor TLinuxMtxAsyncCall.CreateRec(pool : TMtxThreadPool; proc : TMtxRecProc; rec : pointer);
begin
     inherited Create;

     fPool := pool;
     FEvent := TEvent.Create(nil, True, False, '');
     fRecProc := proc;
     fRec := rec;
end;

destructor TLinuxMtxAsyncCall.Destroy;
begin
     if Assigned(fEvent) then
     begin
          try
             Sync;
          finally
                 FreeAndNil(fEvent);
          end;
     end;

     fData.Free;

     inherited Destroy;
end;

function TLinuxMtxAsyncCall._Release: Integer; stdcall;
begin
     Result := InterlockedDecrement(FRefCount);
     if Result = 0 then
        Destroy;
end;

function TLinuxMtxAsyncCall.Finished: Boolean;
begin
     Result := not Assigned(FEvent) or FFinished or (FEvent.WaitFor(0) = wrSignaled);
end;

procedure TLinuxMtxAsyncCall.ForceDifferentThread;
begin
     FForceDifferentThread := True;
end;

function TLinuxMtxAsyncCall.GetEvent: TEvent;
begin
     Result := FEvent;
end;

procedure TLinuxMtxAsyncCall.InternExecuteAsyncCall;
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

procedure TLinuxMtxAsyncCall.Quit;
begin
     FFinished := True;
     fEvent.SetEvent;
end;

procedure TLinuxMtxAsyncCall.Sync;
var E: Exception;
begin
     if not Finished then
     begin
          if fEvent.WaitFor(INFINITE) <> wrSignaled  then
             raise Exception.Create('IAsyncCall.Sync');
     end;

     if FFatalException <> nil then
     begin
          E := FFatalException;
          FFatalException := nil;

          raise E at FFatalErrorAddr;
     end;
end;

procedure TLinuxMtxAsyncCall.ExecuteAsync;
begin
     fPool.AddAsyncCall(Self);
end;

procedure TLinuxMtxAsyncCall.ExecuteAsyncCall;
begin
     if not Assigned(fData)
     then
         fRecProc(fRec)
     else
         fProc(fData);
end;

function CreateThreadPoolObj : IMtxThreadPool;
begin
     Result := TMtxThreadPool.Create;
end;

initialization
   SetThreadPoolProvider( {$IFDEF FPC}@{$ENDIF}CreateThreadPoolObj );

   numCPUCores := sysconf( _SC_NPROCESSORS_ONLN);
   if numCPUCores > 64 then
      numCPUCores := 64;
   numRealCores := numCPUCores;

   numCoresForSimpleFuncs := numRealCores;
   if numCoresForSimpleFuncs > 3 then
      numCoresForSimpleFuncs := 3;

{$ENDIF}

end.
