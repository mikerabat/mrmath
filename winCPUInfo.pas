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

uses {$IFDEF FPC} Windows {$ELSE} {$IF CompilerVersion >= 23.0} Winapi.Windows {$ELSE} Windows {$IFEND} {$ENDIF};

// ###########################################
// #### get logical cpu information and determine a good initial block size
// also fills variables in the thread pool unit (MtxThreadPool.pas)

{$ENDIF}

implementation

{$IFDEF MSWINDOWS}

uses MtxThreadPool, SysUtils;

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

  CPU_Set_Information_Type = DWord;

  TCpuSet = packed record
    id : DWord;
    group : Word;
    LogicalProcessorIndex : Byte;
    CoreIndex : Byte;
    LastLevelCacheIndex : BYte;
    NumNodeIndex : Byte;
    EfficiencyClass : Byte;
    // union {
    //    BYTE AllFlags;
//        struct {
//          BYTE Parked : 1;
//          BYTE Allocated : 1;
//          BYTE AllocatedToTargetProcess : 1;
//          BYTE RealTime : 1;
//          BYTE ReservedFlags : 4;
//        } DUMMYSTRUCTNAME;
//      } DUMMYUNIONNAME2;
    Flags : Byte;

    //union {
//        DWORD Reserved;
//        BYTE  SchedulingClass;
//      };
    SchedulingClass : Byte;
    Reserved : Array[0..2] of byte;

    AllocationTag : UInt64;
  end;
  TSystemCPUSetInformation = packed record
    Size : DWord;
    atype : CPU_Set_Information_Type; // not stated: it's an enum and we assume it to be 4 bytes long
    cpuSet : TCpuSet;
  end;
  PSystemCPUSetInformation = ^TSystemCPUSetInformation;

type
  TLogicalProcessorInfProc = function (
  Buffer: PSystemLogicalProcessorInformation;
  var ReturnLength: DWORD): BOOL; stdcall;

  TGetSystemCpuSetInformationProc = function (
     informatin : PSystemCPUSetInformation;
     BufferLength : ULong;
     var ReturnedLength : ULong;
     Proc : THandle;
     Flags : ULong) : Bool; stdcall;

var sysInfo : TSystemInfo;
    ReturnLength: DWORD;
    Buffer: array of TSystemLogicalProcessorInformation;
    cpuBuf : array of TSystemCPUSetInformation;
    i : integer;
    logicalInfoProc : TLogicalProcessorInfProc;
    GetSystemCpuSetInformation : TGetSystemCpuSetInformationProc;
    dllHdl : THandle;
      
initialization

  ReturnLength := 0;
  Buffer := nil;
  dllHdl := LoadLibrary( 'kernel32.dll' ); // according to microsoft it's save to load kernel32.dll in the initialization section...

  numRealCores := 0;
  if dllHdl <> 0 then
  begin
       try
          try
             logicalInfoProc := TLogicalProcessorInfProc(GetProcAddress( dllHdl, 'GetLogicalProcessorInformation'));

             if Assigned(logicalInfoProc) then
             begin
                  if not logicalInfoProc(nil, ReturnLength) then
                  begin
                       if GetLastError = ERROR_INSUFFICIENT_BUFFER then
                       begin
                            SetLength(Buffer, ReturnLength div SizeOf(TSystemLogicalProcessorInformation) + 1);
                            if not logicalInfoProc(@Buffer[0], ReturnLength) then
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
             end;

             GetSystemCpuSetInformation := TGetSystemCpuSetInformationProc( GetProcAddress(dllHdl, 'GetSystemCpuSetInformation'));
             if Assigned(GetSystemCpuSetInformation) then
             begin
                  if not GetSystemCpuSetInformation( nil, 0, ReturnLength, GetCurrentProcess, 0) then
                  begin
                       if GetLastError = ERROR_INSUFFICIENT_BUFFER then
                       begin
                            if ReturnLength > 0 then
                            begin
                                 SetLength(cpuBuf, ReturnLength div sizeof(TSystemCPUSetInformation));
                                 if not GetSystemCpuSetInformation(@cpuBuf[0], ReturnLength, ReturnLength, GetCurrentProcess, 0) then
                                    RaiseLastOSError;


                                 // ###########################################
                                 // #### Go through all cpu sets.
                                 for i := 0 to Length(cpuBuf) - 1 do
                                 begin
                                      case cpuBuf[i].cpuSet.EfficiencyClass of
                                        0: inc(numPCores);
                                      else
                                          inc(numECores);
                                      end;
                                 end;
                            end;
                       end
                       else
                           RaiseLastOSError;
                  end;
             end;
          finally
                 FreeLibrary(dllHdl);
          end;
       except
             numRealCores := 0;
       end;
  end;
  
  GetSystemInfo(SysInfo);
  numCPUCores := SysInfo.dwNumberOfProcessors;
  numUseCPUCores := numCPUCores;
  if numUseCPUCores > cMaxNumCores then
     numUseCPUCores := cMaxNumCores;

  numCoresForSimpleFuncs := numRealCores;
  if numCoresForSimpleFuncs > 3 then
     numCoresForSimpleFuncs := 3;

  if numRealCores = 0 then
     numRealCores := numUseCPUCores;

  if numECores = 0 then
  begin
       numPCores := numUseCPUCores;
       numECores := 0;
  end;

finalization
{$ENDIF}

end.
