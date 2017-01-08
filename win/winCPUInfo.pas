// ###################################################################
// #### This file is part of the mathematics library project, and is
// #### offered under the licence agreement described on
// #### http://www.mrsoft.org/
// ####
// #### Copyright:(c) 2015, Michael R. . All rights reserved.
// ####
// #### Unless required by applicable law or agreed to in writing, software
// #### distributed under the License is distributed on an "AS IS" BASIS,
// #### WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// #### See the License for the specific language governing permissions and
// #### limitations under the License.
// ###################################################################

unit winCPUInfo;

interface

{$IFDEF MSWINDOWS}

uses Windows;

// ###########################################
// #### get logical cpu information and determine a good initial block size
// also fills variables in the thread pool unit (MtxThreadPool.pas)

{$ENDIF}

implementation

uses MtxThreadPool, SysUtils;

{$IFDEF MSWINDOWS}

// ###########################################
// #### Get Processor information -> windows only
type
  TLogicalProcessorRelationship = (
    RelationProcessorCore = 0,
    RelationNumaNode = 1,
    RelationCache = 2,
    RelationProcessorPackage = 3,
    RelationGroup = 4,
    RelationAll = $FFFF
  );
  TProcessorCacheType = (
    CacheUnified,
    CacheInstruction,
    CacheData,
    CacheTrace
  );
  TCacheDescriptor = record
    Level: Byte;
    Associativity: Byte;
    LineSize: Word;
    Size: DWORD;
    pcType: TProcessorCacheType;
  end;
  PSystemLogicalProcessorInformation = ^TSystemLogicalProcessorInformation;
  TSystemLogicalProcessorInformation = record
    ProcessorMask: ULONG_PTR;
    Relationship: TLogicalProcessorRelationship;
    case Integer of
      0: (Flags: Byte);
      1: (NodeNumber: DWORD);
      2: (Cache: TCacheDescriptor);
      3: (Reserved: array [0..1] of ULONGLONG);
  end;

function GetLogicalProcessorInformation(
  Buffer: PSystemLogicalProcessorInformation;
  var ReturnLength: DWORD): BOOL; stdcall;
  external kernel32 name 'GetLogicalProcessorInformation';


var sysInfo : TSystemInfo;
    ReturnLength: DWORD;
    Buffer: array of TSystemLogicalProcessorInformation;
    i : integer;
    
initialization

  ReturnLength := 0;
  Buffer := nil;
  if not GetLogicalProcessorInformation(nil, ReturnLength) then
  begin
       if GetLastError = ERROR_INSUFFICIENT_BUFFER then
       begin
            SetLength(Buffer, ReturnLength div SizeOf(TSystemLogicalProcessorInformation) + 1);
            if not GetLogicalProcessorInformation(@Buffer[0], ReturnLength) then
               RaiseLastOSError;
       end
       else
           RaiseLastOSError;
  end;

  numRealCores := 0;

  for i := 0 to (ReturnLength div sizeof(TSystemLogicalProcessorInformation)) - 1 do
  begin
       if buffer[i].Relationship = RelationProcessorCore then
          inc(numRealCores);

       // todo: determine a good procedure to estimate the block size   
       //if (buffer[i].Relationship = RelationCache) and (buffer[i].Cache.Level = 2) then
//       begin
//            BlockedMatrixMultSize := buffer[i].Cache.Size div 1024;
//       end;
  end;
  
  GetSystemInfo(SysInfo);
  numCPUCores := SysInfo.dwNumberOfProcessors;

  numCoresForSimpleFuncs := numRealCores;
  if numCoresForSimpleFuncs > 3 then
     numCoresForSimpleFuncs := 3;

finalization
{$ENDIF}

end.
