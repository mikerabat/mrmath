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
// ###################################################################
// #### certain MACOS contributions and testing William Cantrall
// ###################################################################
unit MacOsThreadPool;

interface

// ###########################################
// #### Implementation of the mrmath threading interface
// #### for the MacOs system
// ###########################################

{$IFDEF DARWIN}
{$DEFINE MACOS}
{$ENDIF}

{$IFDEF MACOS}

uses
  MtxThreadPool, GCDDispatch, Math,
  {$IFDEF FPC}
  sysutils, classes 
  {$ELSE}
  Macapi.Foundation, system.sysutils, System.Classes
  {$ENDIF}
  ;

function CreateThreadPoolObj : IMtxThreadPool;

{$ENDIF}

implementation

{$IFDEF MACOS}

{$IFDEF FPC}
uses utf8process;
{$ENDIF}

var macThrPool : dispatch_queue_t = IntPtr(nil);

type
  TMacMtxAsyncCall = class(TInterfacedObject, IMtxAsyncCall)
  private
    fRecProc : TMtxRecProc;
    fRec : Pointer;
    fProc : TMtxProc;
    fData : TObject;
    fGroup: dispatch_object_t;  
  protected
    procedure ExecuteProc;
  public
    procedure ExecuteAsync;
    procedure Sync;

    constructor Create(proc : TMtxProc; obj : TObject; group: dispatch_object_t);
    constructor CreateRec(proc : TMtxRecProc; rec : Pointer; group: dispatch_object_t);
    destructor Destroy; override;
  end;

type
  TMacMtxAsyncGroup = class(TInterfacedObject, IMtxAsyncCallGroup)   
  private
    fGroup : dispatch_group_t;
    fTaskList : IInterfaceList;
  public
    procedure AddTask(proc : TMtxProc; obj : TObject); 
    procedure AddTaskRec(proc : TMtxRecProc; rec : Pointer);
    procedure SyncAll;

    constructor Create;
    destructor Destroy; override;
  end;

procedure TMacMtxAsyncGroup.AddTask(proc : TMtxProc; obj : TObject);
var aTask : IMtxAsyncCall;
begin
     aTask := TMacMtxAsyncCall.Create(proc, obj, fGroup);
     fTaskList.Add(aTask);
     aTask.ExecuteAsync;
end;

{AM}
procedure TMacMtxAsyncGroup.AddTaskRec(proc : TMtxRecProc; rec : Pointer);
var aTask : IMtxAsyncCall;
begin
     aTask := TMacMtxAsyncCall.CreateRec(proc, rec, fgroup);
     fTaskList.Add(aTask);
     aTask.ExecuteAsync;
end;
{AM}

constructor TMacMtxAsyncGroup.Create;
begin
     fTaskList := TInterfaceList.Create;
     fTaskList.Capacity := numCPUCores;

     fGroup := dispatch_group_create;
     
     inherited Create;
end;

destructor TMAcMtxAsyncGroup.Destroy;
begin
     dispatch_release(fGroup);
     
     inherited;
end;

procedure TMacMtxAsyncGroup.SyncAll;
begin
     dispatch_group_wait(fGroup, DISPATCH_TIME_FOREVER);
end;
  
procedure InitMacMtxThreadPool;
begin
     //wrc Assert(not Assigned(macThrPool), 'Error: initialize pool twice. Call FinalizeMtxThreadPool first.');
     if macThrPool = dispatch_group_t(nil) then
        macThrPool := dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT,0);
end;

//wrc Note: You do not need to retain or release any of the global dispatch queues,
//including the concurrent dispatch queues or the main dispatch queue.
//Any attempts to retain or release the queues are ignored.    (From Apple Developer)
procedure FinalizeMacMtxThreadPool;
begin
end;

procedure LocThreadProc( context : Pointer ) cdecl;          //wrc 9/26/2013
begin
     try
        TMacMtxAsyncCall(context).ExecuteProc;
     except
     end;
end;

{ TMacMtxAsyncCall }

constructor TMacMtxAsyncCall.Create(proc: TMtxProc; obj: TObject; group: dispatch_object_t);
begin
     inherited Create;

     fProc := proc;
     fData := obj;
     fGroup := group;
end;

constructor TMacMtxAsyncCall.CreateRec(proc: TMtxRecProc; rec: Pointer; group: dispatch_object_t);
begin
     inherited Create;

     fRecProc := proc;
     {AM}
     fRec := rec;
     {AM}
     fGroup := group;
end;


destructor TMacMtxAsyncCall.Destroy;
begin
     fData.Free;

     inherited;
end;

procedure TMacMtxAsyncCall.ExecuteAsync;
begin
     assert(Assigned(Pointer(macThrPool)), 'Error no thread pool avail: call InitMtxThreadPool first');
     dispatch_group_async_f(fGroup, macThrPool, self, {$IFDEF FPC}@{$ENDIF}LocThreadProc );
end;

procedure TMacMtxAsyncCall.ExecuteProc;
begin
     if not Assigned(fData)
     then
         fRecProc(fRec)
     else
         fProc(fData);
end;

procedure TMacMtxAsyncCall.Sync;
begin
     // do nothing here... -> on the group objects syncall the waiting happens
end;

// simple wrapper to provide the necessary interfaces
type
  TSimpleMacOSThreadPool = class(TInterfacedObject, IMtxThreadPool)
    procedure InitPool( maxNumThreads : integer );
    function CreateTaskGroup : IMtxAsyncCallGroup;
  end;

procedure TSimpleMacOSThreadPool.InitPool( maxNumThreads : integer );
begin
     //wrc Assert(not Assigned(macThrPool), 'Error: initialize pool twice. Call FinalizeMtxThreadPool first.');
     if macThrPool = dispatch_group_t(nil) then
        macThrPool := dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT,0);
end;

function TSimpleMacOSThreadPool.CreateTaskGroup : IMtxAsyncCallGroup;
begin
     assert(macThrPool <> dispatch_group_t(nil), 'Error thread pool not initialized. Call InitMtxThreadPool first');
     Result := TMacMtxAsyncGroup.Create;
end;

function CreateThreadPoolObj : IMtxThreadPool;
begin
     Result := TSimpleMacOSThreadPool.Create;
end;

{$IFDEF FPC}

initialization
  numCPUCores := Max(1, GetSystemThreadCount);
  if numCpuCores > cMaxNumCores then
     numCpuCores := cMaxNumCores;
  numRealCores := numCPUCores;

  numCoresForSimpleFuncs := numRealCores;
  if numCoresForSimpleFuncs > 3 then
     numCoresForSimpleFuncs := 3;

  numPCores := numCPUCores;
  numECores := 0;

  numUseCPUCores := numCPUCores;
   if numUseCPUCores > cMaxNumCores then
      numUseCPUCores := cMaxNumCores;

{$ELSE}

var cpuInfo : NSProcessInfo;

initialization
  cpuInfo := TNSProcessInfo.Create;

  numCPUCores := Max(1, cpuInfo.processorCount);
  if numCpuCores > cMaxNumCores then
     numCpuCores := cMaxNumCores;
  numRealCores := numCPUCores;

  numCoresForSimpleFuncs := numRealCores;
  if numCoresForSimpleFuncs > 3 then
     numCoresForSimpleFuncs := 3;

finalization
  cpuInfo.release;
{$ENDIF}

{$ENDIF}

end.
