unit SimpleWinThreadPool;

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

interface

{$IFDEF MSWINDOWS}

uses MtxThreadPool, SyncObjs;

function CreateThreadPoolObj : IMtxThreadPool;

{$ENDIF}

implementation

{$IFDEF MSWINDOWS}

{$IFDEF FPC}
uses Windows, Classes, winCPUInfo;
{$ELSE}
uses {$IF CompilerVersion >= 23.0} Winapi.Windows {$ELSE} Windows {$IFEND}, Classes, winCPUInfo;
{$ENDIF}

{$IF Defined(FPC) or (CompilerVersion <= 20)}
type
  TThreadStartRoutine = function(lpThreadParameter: Pointer): Integer stdcall;

const WT_EXECUTEDEFAULT       = ULONG($00000000);

function QueueUserWorkItem(func: TThreadStartRoutine; Context: Pointer; Flags: ULONG): BOOL; stdcall; external 'kernel32.dll' name 'QueueUserWorkItem';

{$IFEND}

var threadPoolInit : boolean = False;

type
  TSimpleWinMtxAsyncCall = class(TInterfacedObject, IMtxAsyncCall)
  private
    fProc : TMtxProc;
    fData : TObject;
    fRecProc : TMtxRecProc;
    fRec : Pointer;
    fEvt : TSimpleEvent;
  protected
    procedure ExecuteProc;
  public
    procedure ExecuteAsync;
    procedure Sync;

    constructor Create(proc : TMtxProc; obj : TObject);
    constructor CreateRec(proc : TMtxRecProc; rec : pointer);
    destructor Destroy; override;

  end;

type
  TSimpleWinThreadGroup = class(TInterfacedObject, IMtxAsyncCallGroup)
  private
    fTaskList : IInterfaceList;
  public
    procedure AddTask(proc : TMtxProc; obj : TObject); 
    procedure AddTaskRec(proc : TMtxRecProc; rec : Pointer);
    procedure SyncAll;

    constructor Create;
  end;
  
{ TSimpleWinThreadGroup }

procedure TSimpleWinThreadGroup.AddTask(proc : TMtxProc; obj : TObject);
var aTask : IMtxAsyncCall;
begin
     aTask := TSimpleWinMtxAsyncCall.Create(proc, obj);
     fTaskList.Add(aTask);
     aTask.ExecuteAsync;
end;

procedure TSimpleWinThreadGroup.AddTaskRec(proc: TMtxRecProc; rec: Pointer);
var aTask : IMtxAsyncCall;
begin
     aTask := TSimpleWinMtxAsyncCall.CreateRec(proc, rec);
     fTaskList.Add(aTask);
     aTask.ExecuteAsync;
end;

constructor TSimpleWinThreadGroup.Create;
begin
     fTaskList := TInterfaceList.Create;

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
  
function EmptyThreadProc( lpParameter : Pointer ) : integer; stdcall;
begin
     // nothing to do... just for initialization
     sleep(0);
     Result := 0;
end;

function LocThreadProc( lpParameter : Pointer ) : integer; stdcall;
begin
     try
        TSimpleWinMtxAsyncCall(lpParameter).ExecuteProc;
     except 
     end;

     TSimpleWinMtxAsyncCall(lpParameter).fEvt.SetEvent;
     Result := 0;
end;


{ TSimpleWinMtxAsyncCall }

constructor TSimpleWinMtxAsyncCall.Create(proc: TMtxProc; obj: TObject);
begin
     inherited Create;

     fEvt := TSimpleEvent.Create;
     fProc := proc;
     fData := obj;
end;

constructor TSimpleWinMtxAsyncCall.CreateRec(proc: TMtxRecProc; rec: pointer);
begin
     inherited Create;

     fEvt := TSimpleEvent.Create;
     fRecProc := proc;
     fRec := rec;
end;


destructor TSimpleWinMtxAsyncCall.Destroy;
begin
     fEvt.Free;
     fData.Free;
     
     inherited;
end;

procedure TSimpleWinMtxAsyncCall.ExecuteAsync;
begin
     QueueUserWorkItem(@LocThreadProc, self, WT_EXECUTEDEFAULT);
end;

procedure TSimpleWinMtxAsyncCall.ExecuteProc;
begin
     if not Assigned(fData)
     then
         fRecProc(fRec)
     else
         fProc(fData);
end;

procedure TSimpleWinMtxAsyncCall.Sync;
begin
     fEvt.WaitFor(INFINITE);
end;

type
  TSimpleWinThreadPool = class(TInterfacedObject, IMtxThreadPool)
  public
    procedure InitPool( maxNumThreads : integer );
    function CreateTaskGroup : IMtxAsyncCallGroup;

    constructor Create;
  end;

function CreateThreadPoolObj : IMtxThreadPool;
begin
     Result := TSimpleWinThreadPool.Create;
end;

{ TSimpleWinThreadPool }

constructor TSimpleWinThreadPool.Create;
begin
     inherited Create;
end;

procedure TSimpleWinThreadPool.InitPool(maxNumThreads: integer);
var i: Integer;
begin
     if not threadPoolInit then
     begin
          // queue empty procedures -> initialize the pool
          for i := 0 to numCPUCores - 1 do
              QueueUserWorkItem(@EmptyThreadProc, nil, WT_EXECUTEDEFAULT);

          threadPoolInit := True;
     end;
end;

function TSimpleWinThreadPool.CreateTaskGroup: IMtxAsyncCallGroup;
begin
     Assert(threadPoolInit, 'Error thread pool not initialized. Call InitMtxThreadPool first');
     Result := TSimpleWinThreadGroup.Create;
end;

initialization

  SetThreadPoolProvider( {$IFDEF FPD}@{$ENDIF}SimpleWinThreadPoolProvdier );

{$ENDIF}

end.
