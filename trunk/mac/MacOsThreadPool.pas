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

{$IFDEF MACOS}

uses MtxThreadPool, GCDDispatch,Macapi.Foundation,system.sysutils,System.Classes; 
procedure InitMacMtxThreadPool;
procedure FinalizeMacMtxThreadPool;

function InitMacMtxGroup : IMtxAsyncCallGroup; 
{$ENDIF}

implementation

{$IFDEF MACOS}

var macThrPool : dispatch_queue_t = IntPtr(nil);

type
  TMacMtxAsyncCall = class(TInterfacedObject, IMtxAsyncCall)
  private
    fResult : integer;
    fProc : TMtxProc;
    fData : TObject;
    fGroup: dispatch_object_t;  
  protected
    procedure ExecuteProc;
  public
    procedure ExecuteAsync;
    function Sync: Integer;
    function GetResult : integer;

    constructor Create(proc : TMtxProc; obj : TObject; group: dispatch_object_t); virtual;
    destructor Destroy; override;
  end;

type
  TMacMtxAsyncGroup = class(TInterfacedObject, IMtxAsyncCallGroup)   
  private
    fGroup : dispatch_group_t;
    fTaskList : IInterfaceList;
  public
    procedure AddTask(proc : TMtxProc; obj : TObject); 
    procedure SyncAll;

    constructor Create;
  end;
  
procedure TMacMtxAsyncGroup.AddTask(proc : TMtxProc; obj : TObject);
var aTask : IMtxAsyncCall;
begin
     aTask := TMacMtxAsyncCall.Create(proc, obj, fGroup);
     fTaskList.Add(aTask);
     aTask.ExecuteAsync;
end;
function InitMacMtxGroup : IMtxAsyncCallGroup;
begin
     Result := TMacMtxAsyncGroup.Create;
end;
constructor TMacMtxAsyncGroup.Create;
begin
     fTaskList := TInterfaceList.Create;
     fTaskList.Capacity := numCPUCores;

     fGroup := dispatch_group_create;
     
     inherited Create;
end;

procedure TMacMtxAsyncGroup.SyncAll;
begin
     dispatch_group_wait(fGroup, DISPATCH_TIME_FOREVER);
     dispatch_release(fGroup);
end;
  
procedure InitMacMtxThreadPool;
begin
     //wrc Assert(not Assigned(macThrPool), 'Error: initialize pool twice. Call FinalizeMtxThreadPool first.');
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

destructor TMacMtxAsyncCall.Destroy;
begin
     fData.Free;
     
     inherited;
end;

procedure TMacMtxAsyncCall.ExecuteAsync;
begin
     assert(Assigned(Pointer(macThrPool)), 'Error no thread pool avail: call InitMtxThreadPool first');
     dispatch_group_async_f(fGroup, macThrPool, self, LocThreadProc );
end;

procedure TMacMtxAsyncCall.ExecuteProc;
begin
     fResult := fProc(fData);
end;

function TMacMtxAsyncCall.GetResult: integer;
begin
     Result := fResult;
end;

function TMacMtxAsyncCall.Sync: Integer;
begin
     // do nothing here...
end;

var cpuInfo : NSProcessInfo;     

initialization
  cpuInfo := TNSProcessInfo.Create;

  numCPUCores := cpuInfo.processorCount;

finalization
  cpuInfo.release; 
{$ENDIF}

end.
