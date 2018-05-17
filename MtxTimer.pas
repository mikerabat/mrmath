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

unit MtxTimer;

interface

function MtxGetTime: Int64;

// used to convert to sec
var mtxFreq : Int64;

implementation

uses {$IFDEF FPC}
       {$IFDEF MSWINDOWS} Windows {$ENDIF}
       {$IFDEF LINUX} unixtype, linux {$ENDIF}

     {$ELSE}
       {$IF CompilerVersion >= 23.0} Winapi.Windows {$ELSE} Windows {$IFEND}
       {$IFDEF MACOS}System.Diagnostics{$ENDIF}
       {$IFDEF LINUX}unixtype, linux{$ENDIF}
     {$ENDIF};

{$IFDEF MACOS}
var sw:  TStopWatch; //wrc
{$ENDIF}

function MtxGetTime: Int64;
{$IFDEF LINUX}
var loc_Start : TTimeSpec;
{$ENDIF}
begin
   {$IFDEF MACOS}
   Result := sw.GetTimeStamp;
   {$ENDIF}

   {$IFDEF LINUX}
   clock_gettime(CLOCK_MONOTONIC, @loc_Start);
   Result := loc_Start.tv_sec*1000000000 + loc_Start.tv_nsec;
   {$ENDIF}

   {$IFDEF MSWINDOWS}
   Result := 0;
   QueryPerformanceCounter(Result);
   {$ENDIF}
end;

initialization
  {$IFDEF MSWINDOWS}
  QueryPerformanceFrequency(mtxFreq);
  {$ENDIF}

  {$IFDEF LINUX}
  mtxFreq := 1000000000;
  {$ENDIF}

  {$IFDEF MACOS}
  sw := TStopWatch.Create() ;
  sw.Start;
  mtxFreq := SW.Frequency;
finalization
  sw.Stop;
  {$ENDIF}

end.
