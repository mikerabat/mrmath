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

unit winRandomGen;

interface

{$IFDEF MSWINDOWS}

uses RandomEng;

// interface to the CryptGenRandom windows API 

// ###########################################
// #### rnd object
// ###########################################

function CreateOsRndObj : TOsRndEngine;

{$ENDIF}

implementation

{$IFDEF MSWINDOWS}

uses sysutils, windows;

// ###########################################
// #### Function definitions
// ###########################################

type
  THCRYPTROV = pointer;

const PROV_RSA_FULL = $00000001;
      PROV_DSS = $00000003;
      PROV_RSA_AES = $00000018;
      PROV_DSS_DH = $0000000D;
      PROV_DH_SCHANNEL = $00000012;
      PROV_RSA_SCHANNEL = $0000000C;
      PROV_MS_EXCHANGE = $00000005;
      
      CRYPT_VERIFYCONTEXT = $F0000000;
      CRYPT_NEWKEYSET = 8;

// ###########################################
// #### Delayed loading
// ###########################################

type
  TCryptAcquireContext = function ( out phProv : THCRYPTROV; pszContainer : PChar; pszProvider : PChar; 
                              dwProvType : DWord; dwFlags : DWord) : boolean; stdcall;
  TCryptReleaseContext = function ( hProv : THCRYPTROV; dwFlags : DWord) : boolean; stdcall;
  TCryptGenRandom = function ( hProv : THCRYPTROV; dwLen : DWORD; pbBuffer : PByte) : boolean; stdcall;
var locADVAPIHdl : HMODULE = 0;

    locCryptAcquireContext : TCryptAcquireContext = nil;
    locCryptReleaseContext : TCryptReleaseContext = nil;
    locCryptGenRandom : TCryptGenRandom = nil;
    
procedure InitADVAPI;
begin
     if locADVAPIHdl = 0 then
     begin
          locADVAPIHdl := LoadLibrary('Advapi32.dll');
          if locADVAPIHdl = 0 then
             RaiseLastOSError;

          locCryptAcquireContext := TCryptAcquireContext( GetProcAddress(locADVAPIHdl,  'CryptAcquireContextW') );
          locCryptReleaseContext := TCryptReleaseContext( GetProcAddress(locADVAPIHdl,  'CryptReleaseContext') );
          locCryptGenRandom := TCryptGenRandom( GetProcAddress(locADVAPIHdl,  'CryptGenRandom') );
     end;
end;
    
function CryptAcquireContext( out phProv : THCRYPTROV; pszContainer : PChar; pszProvider : PChar; 
                              dwProvType : DWord; dwFlags : DWord) : boolean; stdcall;
begin
     Assert(locADVAPIHdl <> 0, 'Error - call InitRandLib first');

     Result := locCryptAcquireContext(phProv, pszContainer, pszProvider, dwProvType, dwFlags); 
end;

function CryptReleaseContext( hProv : THCRYPTROV; dwFlags : DWord) : boolean; stdcall; 
begin
     Assert(locADVAPIHdl <> 0, 'Error - call InitRandLib first');

     Result := locCryptReleaseContext(hProv, dwFlags);
end;

function CryptGenRandom( hProv : THCRYPTROV; dwLen : DWORD; pbBuffer : PByte) : boolean; stdcall; 
begin
     Assert(locADVAPIHdl <> 0, 'Error - call InitRandLib first');

     Result := locCryptGenRandom(hProv, dwLen, pbBuffer);
end;


type
  TWinRndEngine = class(TOsRndEngine)
  private
    const cNumPreCalc = 1000;
  private
    fBuf : Array[0..cNumPreCalc-1] of LongWord;  // precalculated buffer
    fBufIdx : integer;
    fphProv : THCRYPTROV; 
  
  public
    procedure Init(seed : LongWord); override;
    function Random : LongWord; override;

    constructor Create;
    destructor Destroy; override;
  end;

function CreateOsRndObj : TOsRndEngine;
begin
     Result := TWinRndEngine.Create;
end;

{ TWinRndEngine }

constructor TWinRndEngine.Create;
begin
     inherited Create;

     if locADVAPIHdl = 0 then
        InitADVAPI;
     
     // create a random object context with default parameters
     fBufIdx := cNumPreCalc;
     if not CryptAcquireContext(fPhProv, nil, nil, PROV_RSA_FULL, CRYPT_VERIFYCONTEXT) then
     begin
          if GetLastError = LongWord( NTE_BAD_KEYSET ) then
          begin
               if not CryptAcquireContext(fPhProv, nil, nil, PROV_RSA_FULL, CRYPT_NEWKEYSET or CRYPT_VERIFYCONTEXT) then
                  RaiseLastOSError;
          end
          else
              RaiseLastOSError;
     end;
end;

destructor TWinRndEngine.Destroy;
begin
     if fphProv <> nil then
        CryptReleaseContext(fphProv, 0); 
        
     inherited;
end;

procedure TWinRndEngine.Init(seed: LongWord);
begin
     // fetch new values
     fBufIdx := 0;
     if not CryptGenRandom(fphProv, cNumPreCalc*sizeof(LongWord), @fBuf[0]) then
        RaiseLastOSError;
end;

function TWinRndEngine.Random: LongWord;
begin
     if fBufIdx = cNumPreCalc then
        Init(0);

     // return from a set of precalculated values
     Result := fBuf[fBufIdx];
     inc(fBufIdx);
end;

{$ENDIF}

end.
