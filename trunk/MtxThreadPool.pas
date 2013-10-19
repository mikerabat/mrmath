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

unit MtxThreadPool;

// #####################################################
// #### Thread pool for async matrix operations stub.
// #### The real implementation is OS dependent!
// #####################################################

interface

uses MatrixConst;

type
  TMtxProc = function(obj : TObject) : integer;

type
  IMtxAsyncCall = interface
   ['{B5263EB3-FFDE-4D66-B556-31D5E0D05BAC}']
    function GetResult : integer;
    procedure ExecuteAsync;
    function Sync: Integer;
  end;
  IMtxAsyncCallGroup = interface
   ['{11438431-7A6A-4FB9-B67A-58CE23E324DB}']
   procedure AddTask(proc : TMtxProc; obj : TObject); 
   procedure SyncAll;
  end;


procedure InitMtxThreadPool;
procedure FinalizeMtxThreadPool;

function MtxInitTaskGroup : IMtxAsyncCallGroup;

var numCPUCores : TASMNativeInt = 0;

implementation

// ###########################################
// #### Windows Thread Pooling
// ###########################################

{$IFDEF MSWINDOWS }

{.$DEFINE USE_OS_THREADPOOL}

uses {$IFDEF USE_OS_THREADPOOL}SimpleWinThreadPool{$ELSE} WinThreadPool{$ENDIF};

procedure InitMtxThreadPool;
begin
     InitWinMtxThreadPool;
end;

procedure FinalizeMtxThreadPool;
begin
     FinalizeWinMtxThreadPool;
end;

function MtxInitTaskGroup : IMtxAsyncCallGroup;
begin
     Result := InitWinThreadGroup;
end;

{$ENDIF}

// ###########################################
// #### MACOS Thread pooling
// ###########################################

{$IFDEF MACOS }

uses MacOsThreadPool;

procedure InitMtxThreadPool;
begin
     InitMacMtxThreadPool;
end;

procedure FinalizeMtxThreadPool;
begin
     FinalizeMacMtxThreadPool;
end;

function MtxInitTaskGroup : IMtxAsyncCallGroup;
begin
     Result := InitMacMtxGroup;
end;

{$ENDIF}



end.
