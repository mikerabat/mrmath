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

unit MacOsRandomGen;

interface

uses RandomEng;

// note: until now this is only a stub for the real implementation
// I don't have a Mac to test so this object here is only for completeness
// until someone will verify the /dev/random implementation. The random numbers generated
// here are simply the mersenne twister numbers from the base object

// the /dev/random implementation is experimental and not tested!
// simply uncomment the "DummyRND" switch to have the standard prng in place

{.$DEFINE DummyRND}

{$IFNDEF MSWINDOWS}

function CreateOsRndObj : TOsRndEngine;

{$ENDIF}

implementation

{$IFNDEF MSWINDOWS}
uses SysUtils, Classes;

type
  TMacOSRndEngine = class(TOsRndEngine)
  private
    const cNumPreCalc = 1000;
  private
    {$IFDEF DummyRND}
    fRnd : TRandomGenerator;
    {$ELSE}
    fBuf : Array[0..cNumPreCalc-1] of LongWord;
    fBufIdx : integer;
    
    fs : TFileStream;
    {$ENDIF}
  public
    procedure Init(seed : LongWord); override;
    function Random : LongWord; override;

    constructor Create;
    destructor Destroy; override;
  end;

{ TMacOSRndEngine }

constructor TMacOSRndEngine.Create;
begin
     {$IFNDEF DummyRND}
     fs := TFileStream.Create('/dev/random', fmOpenRead);
     fBufIdx := cNumPreCalc;
     {$ELSE}
     fRnd := TRandomGenerator.Create;
     fRnd.RandMethod := raMersenneTwister;
     fRnd.Init(0);
     {$ENDIF}
end;

destructor TMacOSRndEngine.Destroy;
begin
     {$IFNDEF DummyRND}
     fs.Free;
     {$ELSE}
     fRnd.Free;     
     {$ENDIF}
     
     inherited;
end;

procedure TMacOSRndEngine.Init(seed: LongWord);
begin
     {$IFNDEF DummyRND}
     fBufIdx := 0;

     if fs.Read(fBuf[0], sizeof(fBuf)) <> sizeof(fBuf) then
        RaiseLastOSError;
     {$ELSE}
     fRnd.Init(seed);
     {$ENDIF}
end;

function TMacOSRndEngine.Random: LongWord;
begin
     {$IFNDEF DummyRND}
     if fBufIdx = cNumPreCalc then
        Init(0);

     Result := fBuf[fBufIdx];
     inc(fBufIdx);
     {$ELSE}
     Result := fRnd.RandLW($FFFFFFFF);
     {$ENDIF}
end;

{$ENDIF}

end.
