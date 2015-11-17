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

unit RandomEng;

interface

// ###########################################
// #### some simple random number generators
// ###########################################

type
  TOsRndEngine = class(TObject)
  public
    procedure Init(seed : LongWord); virtual; abstract;
    function Random : LongWord; virtual; abstract;
  end;
  
  TRandomAlgorithm = (raSystem, raMersenneTwister, raIntelRAND, raOS);
  TRandomInt = function(const ARange : integer) : LongInt of object;
  TRandomLW = function(const ARange : LongWord) : LongWord of object;
  TRandomDouble = function : double of object;
  TRandomGenerator = class(TObject)
  private
    fRandGFlag : boolean;
    fRandG2 : double;
    fRandMethod: TRandomAlgorithm;
    fRandDbl : TRandomDouble;
    fRandInt : TRandomInt;
    fRandLW : TRandomLW;

    fSysRandSeed : LongWord;  // rebuild delphi random generator but this time in a thread save way
                              // -> each object has it's own seed

    fOsRndEngine : TOsRndEngine;
    
    procedure SetRandMethod(const Value: TRandomAlgorithm);

    procedure SysRandomize;
    function SysRand : double;    // calls system.random    
    function SysRandInt(const aRange : LongInt) : LongInt; 
    function SysRandLw(const aRange : LongWord) : LongWord;
    

    // os specific random generators
    procedure OsRandomize;
    function OsRand : double;    // calls system.random    
    function OsRandInt(const aRange : LongInt) : LongInt; 
    function OsRandLw(const aRange : LongWord) : LongWord;
    
    // Intel random generator (RDRand instruction) - returns false if the instruction is not avail and 
    // system.random needed to be executed.
    function RDRandInt(const aRange : LongInt): LongInt;
    function RDRandLW(const aRange : LongWord) : LongWord;
    function RDRandDbl: double;

  private
    // Mersenne twister functions - Implementation of mt19937ar.c -> Mersenne  Twister with improved initilaization
    const cN = 624;
          cM = 397;
          cMatrix_A = $9908b0df;  // constant vector a
          cUpperMask = $80000000; // most significant w-r bits
          cLowerMask = $7fffffff; // least significant r bits
  private
    fMt : Array of LongWord;  // array of the state vector
    fmag01 : Array[0..1] of LongWord;
    fMTi : LongInt;

    procedure InitMersenne(Seed : LongInt);

    function RandMersenneDblWord : LongWord; {$IFNDEF FPC} {$IF CompilerVersion >= 17.0} inline; {$IFEND} {$ENDIF}
    function RandMersenneInt(const ARange: LongInt) : LongInt;
    function RandMersenneDbl : double;
    function RandMersenneLW(const ARange: LongWord): LongWord;
  public
    property RandMethod : TRandomAlgorithm read fRandMethod write SetRandMethod;

    procedure Init(Seed : LongInt);

    function Random : double;  // from [0 - 1)
    function RandInt : longInt; overload;  // integer to maxint
    function RandInt(const ARange: LongInt) : LongInt; overload;
    function RandLW(const aRange : LongWord) : LongWord;

    // ###########################################
    // #### Special distributed random numbers:
    function RandGauss : double;  // zero mean unit variance normal distributed random numbers
    function RandExp : double;    // exponential distributed random numbers
    function RandErlang(k : integer) : double; // mean=1
    function RandPoission(mean : integer) : integer;
  public
    // special initialization 
    procedure InitMersenneByArr(const initKey: array of LongWord);

    destructor Destroy; override;
  end;
  
implementation

uses SysUtils, CPUFeatures, Math, Classes, MtxTimer, winRandomGen, MacOSRandomGen;

const two2neg32: double = ((1.0/$10000) / $10000);  // 2^-32

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}


// Mersenne Twister translated from
// for copyright see: http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/MT2002/CODES/mt19937ar.c

{ TRandomGenerator }

// ###########################################
// #### Common Functions
// ###########################################

destructor TRandomGenerator.Destroy;
begin
     fOsRndEngine.Free;
     
     inherited;
end;

procedure TRandomGenerator.Init(Seed: Integer);
begin
     case fRandMethod of
      raSystem: if Seed = 0 
                then
                    SysRandomize
                else
                    fSysRandSeed := Seed;
      raMersenneTwister: InitMersenne(seed);
      raOS: if Seed = 0
            then
                OsRandomize
            else
                fOsRndEngine.Init(seed);
     end;
end;

function TRandomGenerator.RandInt: longInt;
begin
     Result := fRandInt(Maxint);
end;

function Rand: double;
begin
     Result := System.Random;
end;

// ###########################################
// #### Special distributed random numbers
// ###########################################

function TRandomGenerator.RandErlang(k: integer): double;
var unif, prod : double;
    i : integer;
begin
     if k < 1 then
     begin
          Result := NaN;
          exit;
     end;

     prod := 1;
     for i := 0 to k - 1 do
     begin
          repeat
                unif := Random;
          until unif <> 0;

          prod := prod*unif;
     end;

     Result := -ln(prod);
end;

function TRandomGenerator.RandExp: double;
var value : double;
begin
     repeat
           value := Random;
     until value <> 0;

     Result := -ln(value);
end;

function TRandomGenerator.RandGauss: double;
{ extended base Marsaglia-Bray algorithm -> calculates 2 normal values at once}
var v1, v2, rsq : double;
begin
     if fRandGFlag
     then
         Result := fRandG2
     else
     begin
          repeat
                v1 := 2*random - 1;
                v2 := 2*random - 1;

                rsq := sqr(v1) + sqr(v2);
          until (rsq < 1) and (rsq <> 0);
                
          Result := Sqrt(-2*Ln(rsq)/rsq);

          // get the second value
          fRandG2 := v1*Result;
          Result := v2*Result;
     end;

     fRandGFlag := not fRandGFlag;
end;

function TRandomGenerator.RandInt(const ARange: Integer): LongInt;
begin
     Result := fRandInt(ARange);
end;

function TRandomGenerator.RandLW(const aRange: LongWord): LongWord;
begin
     Result := fRandLW(aRange);
end;

function TRandomGenerator.Random: double;
begin
     Result := fRandDbl();
end;

function TRandomGenerator.RandPoission(mean: integer): integer;
var k :integer;
    b, l : double;
begin
     assert(mean > 0, 'mean < 1');
     k := 0;
     b := 1;
     l := exp(-mean);
     while b > l do
     begin
          inc(k);
          b := b*random;
     end;
     Result := k - 1;
end;

procedure TRandomGenerator.SetRandMethod(const Value: TRandomAlgorithm);
begin
     FreeAndNil(fOsRndEngine);
     fRandMethod := Value;

     case fRandMethod of
       raSystem: begin
                      fRandDbl := {$IFDEF FPC}@{$ENDIF}SysRand;
                      fRandInt := {$IFDEF FPC}@{$ENDIF}SysRandInt;
                      fRandLW := {$IFDEF FPC}@{$ENDIF}SysRandLw;
                 end;
       raMersenneTwister: begin
                               SetLength(fMt, cN);      
                               fMTi := cN + 1;

                               fRandDbl := {$IFDEF FPC}@{$ENDIF}RandMersenneDbl;
                               fRandInt := {$IFDEF FPC}@{$ENDIF}RandMersenneInt;
                               fRandLW := {$IFDEF FPC}@{$ENDIF}RandMersenneLW;
                          end;
       raIntelRAND: if IsHardwareRNDSupport then
                    begin
                         fRandInt := {$IFDEF FPC}@{$ENDIF}RDRandInt;
                         fRandDbl := {$IFDEF FPC}@{$ENDIF}RDRandDbl;
                         fRandLW := {$IFDEF FPC}@{$ENDIF}RDRandLW;
                    end
                    else
                        SetRandMethod(raSystem);

       raOS: begin
                  fOsRndEngine := CreateOsRndObj;

                  fRandInt := {$IFDEF FPC}@{$ENDIF}OsRandInt;
                  fRandDbl := {$IFDEF FPC}@{$ENDIF}OsRand;
                  fRandLW := {$IFDEF FPC}@{$ENDIF}OsRandLw;
             end;
     end;
end;


{$R-} {$Q-}  // range checking and overflowchecking needs to be off 

// ###########################################
// #### Standard random generator
// ###########################################

function TRandomGenerator.SysRand: double;
begin
     // do that the same as in the system unit -> note here the random function
     // takes the randseed variable instead of the upper 32bits
     SysRandLw(MaxInt);
     Result := fSysRandSeed*two2neg32;
end;

function TRandomGenerator.SysRandInt(const aRange : LongInt): LongInt;
var res : Int64;
begin
     //Result := System.Random(aRange);
     // linear congruential random number generator: x_n+1 = (a*x_n + c) mod m
     // m = 2^32; a = $8088405; c = 1
     
     res := fSysRandSeed*$8088405 + 1;
     fSysRandSeed := res and $FFFFFFFF;

     res := res*aRange;

     Result := LongInt(Int64Rec(res).Hi);
end;

function TRandomGenerator.SysRandLw(const aRange: LongWord): LongWord;
var res : int64;
begin
     //Result := System.Random(aRange);
     // linear congruential random number generator: x_n+1 = (a*x_n + c) mod m
     // m = 2^32; a = $8088405; c = 1
     
     res := fSysRandSeed*$8088405 + 1;
     fSysRandSeed := res and $00000000FFFFFFFF;

     res := res*aRange;

     Result := Int64Rec(res).Hi;
end;

procedure TRandomGenerator.SysRandomize;
begin
     fSysRandSeed := LongWord(MtxGetTime);
end;

// ###########################################
// #### OS specific random generators
// ###########################################

function TRandomGenerator.OsRand: double;
begin
     Result := fOsRndEngine.Random*two2neg32;
end;

function TRandomGenerator.OsRandInt(const aRange: Integer): LongInt;
begin
     Result := LongInt(fOsRndEngine.Random and $7FFFFFFF) mod aRange;
end;

function TRandomGenerator.OsRandLw(const aRange: LongWord): LongWord;
begin
     Result := fOsRndEngine.Random;
end;

procedure TRandomGenerator.OsRandomize;
begin
     fOsRndEngine.Init(LongWord(MtxGetTime));
end;

// ###########################################
// #### hardware random generator (intel RAND asm call)
// ###########################################

function TRAndomGenerator.RDRandDbl : double;
begin
     // do that the same as in the system unit
     Result := RDRandInt(MaxInt)*two2neg32;
end;

{$IFDEF FPC} {$ASMMODE intel} {$ENDIF}

// from http://stackoverflow.com/questions/28538370/using-intels-rdrand-opcode-in-delphi-6-7
function TryRdRand(out Value: LongWord): Boolean;
{$IFDEF FPC}
begin
{$ENDIF}
asm
   db   $0f
   db   $c7
   db   $f1
   jc   @success
   xor  eax,eax
   ret
@success:
{$IFNDEF x64}
   mov  [eax],ecx
{$ELSE}
   mov [rax], ecx
{$ENDIF}
   mov  eax,1
{$IFDEF FPC}
end;
{$ENDIF}
end;

function TRandomGenerator.RDRandInt(const aRange : LongInt) : LongInt;
var cnt : integer;
    y : LongWord;
begin
     cnt := 0;
     // according to the manual we shall create 10 numbers to ensure that
     // the value is a true random number...
     while cnt < 10 do
     begin
          if TryRdRand(y) then
             inc(cnt);
     end;

     Result := LongInt(y) mod aRange;
end;

function TRandomGenerator.RDRandLW(const aRange: LongWord): LongWord;
var cnt : integer;
    y : LongWord;
begin
     cnt := 0;
     // according to the manual we shall create 10 numbers to ensure that
     // the value is a true random number...
     while cnt < 10 do
     begin
          if TryRdRand(y) then
             inc(cnt);
     end;

     Result := y mod aRange;
end;

// ###########################################
// #### Mersenne Twister
// ###########################################

procedure TRandomGenerator.InitMersenne(Seed: Integer);
var mti : LongWord;
begin
     if seed = 0 then
        Seed := LongInt(MtxGetTime);
     
     fMt[0] := LongWord(Seed); // and $ffffffff; // for > 32 bit machines

     for mti := 1 to cN - 1 do
     begin
          fMT[mti] := (1812433253 * (fMt[mti - 1] xor (fMt[mti - 1] shr 30)) + mti);
          // fMT[mti] := fMT[mti] and and $ffffffff; // for > 32bit machines
     end;

     fMTi := cN;

     fmag01[0] := 0;
     fMag01[1] := cMatrix_A;
end;

procedure TRandomGenerator.InitMersenneByArr(const initKey : Array of LongWord);
var k : integer;
    i, j : LongWord;
    
begin
     if fRandMethod <> raMersenneTwister then
        raise Exception.Create('Function can only be called if random method is Mersenne Twister');
     
     assert(Length(initKey) > 0, 'Error init by array needs at least one element');
     InitMersenne( 19650218 );

     i := 1;
     j := 0;
     
     for k := Max(cN, Length(initKey)) - 1 downto 0 do
     begin
          fMt[i] := ( fMT[i] xor ( (fMt[i - 1] xor (fMT[i - 1] shr 30))* 1664525)) + 
                    initKey[j] + j; // non Linear
          // fMT[mti] := fMT[mti] and and $ffffffff; // for > 32bit machines
          inc(i); 
          inc(j);

          if i >= CN then
          begin
               fMT[0] := fMT[cN - 1];
               i := 1;
          end;

          if j > LongWord( High(initKey) ) then 
             j := 0;
     end;

     for k := cN - 2 downto 0 do
     begin
          fMt[i] := (fMT[i] xor ( ( fMT[i - 1] xor (fMt[i - 1] shr 30))* 1566083941)) - i;
          // fMT[mti] := fMT[mti] and and $ffffffff; // for > 32bit machines
          inc(i);

          if i >= cN then
          begin
               fMT[0] := fMT[cN - 1];
               i := 1;
          end;
     end;

     fMT[0] := $80000000;
end;

function TRandomGenerator.RandMersenneDblWord: LongWord;
var y : LongWord;
    kk : integer;
begin
     if fMTi > cN then
        InitMersenne(5489);

     if fMTi >= cN then  // generate N word sat one time
     begin
          for kk := 0 to cN - cM - 1 do
          begin
               y := (fMt[kk] and cUpperMask) or (fMT[kk + 1] and cLowerMask);
               fMT[kk] := fMT[kk + cM] xor (y shr 1) xor fmag01[y and $1];
          end;

          for kk := cN - cM to cN - 2 do
          begin
               y := (fMt[kk] and cUpperMask) or (fMT[kk + 1] and cLowerMask);
               fMT[kk] := fMT[kk + (cM - cN)] xor (y shr 1) xor fMag01[y and $1];
          end;

          y := (fMT[cN - 1] and cUpperMask) or (fMT[0] and cLowerMask);
          fMt[cN - 1] := fMT[cM - 1] xor (y shr 1) xor fMag01[y and $1];

          fMTi := 0;
     end;
     
     y := fMT[fMTi];

     inc(fMTi);

     // tempering
     y := y xor ( y shr 11 );
     y := y xor ( (y  shl 7) and $9d2c5680 );
     y := y xor ( (y shl 15) and $efc60000 );
     y := y xor ( y shr 18 );

     Result := y;
end;

function TRandomGenerator.RandMersenneInt(const ARange: Integer): LongInt;
var y : LongWord;
begin
     y := RandMersenneDblWord;
     // ensure positive integer (base range from 0 - maxint)
     Result := (LongInt(y and $7FFFFFFF)) mod aRange;
end;

function TRandomGenerator.RandMersenneLW(const ARange: LongWord): LongWord;
var y : LongWord;
begin
     y := RandMersenneDblWord;
     Result := y mod aRange;
end;

function TRandomGenerator.RandMersenneDbl: double;
// generates a random number on [0,1) with 53-bit resolution
// note: FPC seems to have troubles estimating if the output is single or double...
// -> thus the explicit assignment!
const cMultA : double = 67108864.0;
      cMultB : double = 1.0/9007199254740992.0;
var a, b : Cardinal;
    x, y : double;
begin
     a := RandMersenneDblWord;
     x := a shr 5;
     b := RandMersenneDblWord;
     y := b shr 6;
     
     Result := (x*cMultA+y)*cMultB;
end;

end.
