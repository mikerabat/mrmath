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

uses Types;

{$I 'mrMath_CPU.inc'}

// ###########################################
// #### some simple random number generators
// ###########################################

type
  TOsRndEngine = class(TObject)
  public
    procedure Init(seed : LongWord); virtual; abstract;
    function Random : LongWord; virtual; abstract;
  end;

  TRandomAlgorithm = (raSystem, raMersenneTwister, raIntelRAND, raOS, raChaCha);
  TRandomInt = function(const ARange : integer) : LongInt of object;
  TRandomLW = function(const ARange : LongWord) : LongWord of object;
  TRandomDouble = function : double of object;

type
  TChaChaCPUInstrType = (itFPU, itSSE, itAVX2);
  TChaChaK = Array[0..7] of LongWord;            // key
  TChaChaIV = Array[0..1] of LongWord;           // nonce 64 bit (todo: 96 bit implementation)
  TChaChaMode = (cmSpeed, cmBalance, cmSecure);  // number of rounds... (4, 12, 20)

type
  TChaChaMtx = Array[0..15] of LongWord;
  PChaChaMtx = ^TChaChaMtx;

  TChaChaAVXMtx = Array[0..31] of LongWord;
  PChaChaAVXMtx = ^TChaChaAVXMtx;


  { TRandomGenerator }

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
    type
      TChaChaDWRand = function : LongWord of object;
      TChaChaInitVec = procedure(k : TChaChaK; iv : TChaChaIV) of Object;

  private
    fChaChaMem : Pointer;
    fInpChaChaMTX : PChaChaMtx;
    fOutChaChaMtx : PChaChaMtx;

    {$IFNDEF MRMATH_NOASM}
    fOutChaChaMtxAVX : PChaChaAVXMtx;
    {$ENDIF}

    fChaChaIdx : integer;
    fChaChaMode : TChaChaMode;
    fNumChaChaRounds : integer;

    fChaChaDblWord : TChaChaDWRand;
    fChaChaInitVec : TChaChaInitVec;

    function PasRandChaChaDblWord : Longword;
    procedure ChaChaQuarterRound( var a, b, c, d : LongWord );
    procedure PasChaChaDoubleQuarterRound;   // one column and row round

    {$IFNDEF MRMATH_NOASM}
    function SSERandChaChaDblWord : Longword;
    procedure SSEChaChaDoubleQuarterRound;

    function AVXRandChaChaDblWord : Longword;
    procedure AVXInitChaChaByVec(k : TChaChaK; iv : TChaChaIV);
    {$ENDIF}

    function RandChaChaInt(const ARange: LongInt) : LongInt;
    function RandChaChaDbl : double;
    function RandChaChaLW(const ARange: LongWord): LongWord;

    procedure InitChaChaByVec(k : TChaChaK; iv : TChaChaIV);
    procedure SeedChaCha(seed : longInt);
    procedure SetChaChaMode(const Value: TChaChaMode);
  private
    const cRDRAND_RETRIES = 10;

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

    procedure Init(Seed : LongInt = 0);

    function Random : double;  // from [0 - 1)
    function RandInt : longInt; overload;  // integer to maxint
    function RandInt(const ARange: LongInt) : LongInt; overload;
    function RandLW(const aRange : LongWord) : LongWord;
    function RandInt64(const aRange : Int64) : Int64;
    function RandUint64(const aRange : UInt64) : UInt64;

    // Fisher Yates shuffeld index list
    function RandIndexArr( aFrom, aTo : integer ) : TIntegerDynArray; overload;
    procedure RandIndexArr( var idx : TIntegerDynArray); overload;

    // ###########################################
    // #### Special distributed random numbers:
    function RandGauss : double;  // zero mean unit variance normal distributed random numbers
    function RandExp : double;    // exponential distributed random numbers
    function RandErlang(k : integer) : double; // mean=1
    function RandPoission(mean : integer) : integer;
  public
    class var cpuSet : TChaChaCPUInstrType;

    procedure SetCpuInstrSet( aCpuSet : TChaChaCPUInstrType );

    property ChaChaMode : TChaChaMode read fChaChaMode write SetChaChaMode;

    // special initialization
    procedure InitMersenneByArr(const initKey: array of LongWord);
    procedure InitChaCha(const k : TChaChaK; const iv : TChaChaIV);
    procedure InitChaChaRandom; // randomly init the matrix by random numbers created by the mersenne twister
    procedure ChaChaAddEntropy( const inBuf : Array of Byte );

    constructor Create(aRandMethod : TRandomAlgorithm = raSystem);
    destructor Destroy; override;
  end;
  
implementation

uses SysUtils, CPUFeatures, Math, Classes, MatrixConst,
     {$IFNDEF MRMATH_NOASM}
     {$IFDEF x64}AVXChaChax64, {$ELSE}AVXChaCha,{$ENDIF}
     {$ENDIF}
     MtxTimer{$IFDEF MSWINDOWS}, winRandomGen {$ENDIF} {$IFNDEF MSWINDOWS}, MacOsRandomGen {$ENDIF};

const two2neg32: double = ((1.0/$10000) / $10000);  // 2^-32



// Mersenne Twister translated from
// for copyright see: http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/MT2002/CODES/mt19937ar.c

{ TRandomGenerator }

// ###########################################
// #### Common Functions
// ###########################################

destructor TRandomGenerator.Destroy;
begin
     fOsRndEngine.Free;
     if fChaChaMem <> nil then
        FreeMem(fChaChaMem);

     inherited;
end;

procedure TRandomGenerator.Init(Seed: LongInt);
begin
     case fRandMethod of
      raSystem: if Seed = 0 
                then
                    SysRandomize
                else
                    fSysRandSeed := Seed;
      raMersenneTwister: InitMersenne(seed);
      raChaCha: SeedChaCha(seed);
      raOS: if Seed = 0
            then
                OsRandomize
            else
                fOsRndEngine.Init(seed);
      raIntelRAND: // do nothing here; todo: maybe call rdseed
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

function TRandomGenerator.RandInt(const ARange: LongInt): LongInt;
begin
     Result := fRandInt(ARange);
end;

function TRandomGenerator.RandLW(const aRange: LongWord): LongWord;
begin
     Result := fRandLW(aRange);
end;

function TRandomGenerator.RandInt64(const aRange: Int64): Int64;
var val1, val2 : LongWord;
begin
     val1 := fRandLW($8FFFFFFF);
     val2 := fRandLW($FFFFFFFF);

     Result := ( ( Int64(val1) shl 32) + Int64(val2) ) mod aRange;
end;

function TRandomGenerator.RandUint64(const aRange: UInt64): UInt64;
var val1, val2 : LongWord;
begin
     val1 := fRandLW($FFFFFFFF);
     val2 := fRandLW($FFFFFFFF);

     Result := ( ( UInt64(val1) shl 32) + UInt64(val2) ) mod aRange;
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

procedure TRandomGenerator.SetChaChaMode(const Value: TChaChaMode);
begin
     fChaChaMode := Value;
     case fChaChaMode of
       cmSpeed: fNumChaChaRounds := 4;      // we use double rounds... so it's half of the proposed number of rounds
       cmBalance: fNumChaChaRounds := 6;
       cmSecure: fNumChaChaRounds := 10;
     end;
end;

procedure TRandomGenerator.SetCpuInstrSet(aCpuSet: TChaChaCPUInstrType);
begin
     {$IFNDEF MRMATH_NOASM}
     if (aCpuSet = itAVX2) then
     begin
          fChaChaDblWord := {$IFDEF FPC}@{$ENDIF}AVXRandChaChaDblWord;
          fChaChaInitVec := {$IFDEF FPC}@{$ENDIF}AVXInitChaChaByVec;
     end
     else if (aCpuSet = itSSE) then
     begin
          fChaChaDblWord := {$IFDEF FPC}@{$ENDIF}SSERandChaChaDblWord;
          fChaChaInitVec := {$IFDEF FPC}@{$ENDIF}InitChaChaByVec;
     end
     else
     {$ENDIF}
     begin
          fChaChaDblWord := {$IFDEF FPC}@{$ENDIF}PasRandChaChaDblWord;
          fChaChaInitVec := {$IFDEF FPC}@{$ENDIF}InitChaChaByVec;
     end;
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
       raChaCha: begin
                      fRandDbl := {$IFDEF FPC}@{$ENDIF}RandChaChaDbl;
                      fRandInt := {$IFDEF FPC}@{$ENDIF}RandChaChaInt;
                      fRandLW := {$IFDEF FPC}@{$ENDIF}RandChaChaLW;
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
     res := res and $FFFFFFFF;
     fSysRandSeed := LongWord(Res);
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
     res := res and $FFFFFFFF;
     fSysRandSeed := LongWord(Res);
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

function TRandomGenerator.OsRandInt(const aRange: LongInt): LongInt;
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

function TRandomGenerator.RDRandDbl: double;
begin
     // do that the same as in the system unit
     Result := RDRandInt(MaxInt)*two2neg32;
end;

{$IFNDEF MRMATH_NOASM}

// from http://stackoverflow.com/questions/28538370/using-intels-rdrand-opcode-in-delphi-6-7
{$IFNDEF x64}
function TryRdRand(out Value: LongWord): Boolean;
{$IFDEF FPC}
begin
{$ENDIF}
asm
   db   $0f
   db   $c7
   db   $f1           // store random value in ecx
   jb   @success
   xor  eax,eax
   ret
@success:
   mov  [eax],ecx
   mov eax, 1
{$IFDEF FPC}
end;
{$ENDIF}
end;
{$ENDIF}

{$IFDEF x64}
function TryRdRand(out Value: LongWord): Boolean;
{$IFDEF FPC}
begin
{$ENDIF}
asm
   db   $0f
   db   $c7
   db   $f0            // store value in eax
   jb   @success
   xor rax,rax
   ret
@success:
   {$IFDEF UNIX}
   mov [rdi], eax
   {$ELSE}
   mov [rcx], eax
   {$ENDIF}
   mov rax, 1
{$IFDEF FPC}
end;
{$ENDIF}
end;
{$ENDIF}

{$ENDIF}


function TRandomGenerator.RDRandInt(const aRange : LongInt) : LongInt;
{$IFNDEF MRMATH_NOASM}
var cnt : integer;
    y : LongWord;
begin
     cnt := 0;
     // according to the manual we shall try 10 times -> otherwise the cpu seems to
     // have some serious issues
     // see: https://software.intel.com/sites/default/files/managed/4d/91/DRNG_Software_Implementation_Guide_2.0.pdf
     while cnt < cRDRAND_RETRIES do
     begin
          if TryRdRand(y) then
             break;

          inc(cnt);
     end;

     if cnt = cRDRAND_RETRIES then
        raise Exception.Create('Error - RDRAND failed too often');

     Result := LongInt(y and $7FFFFFFF) mod aRange;
end;
{$ELSE}
begin
     raise Exception.Create('Not implemented without hardware support');
end;
{$ENDIF}

function TRandomGenerator.RDRandLW(const aRange: LongWord): LongWord;
{$IFNDEF MRMATH_NOASM}
var cnt : integer;
    y : LongWord;
begin
     cnt := 0;
     // according to the manual we shall try 10 times -
     // if it fails then the CPPU seems to have some serious issues...
     // see: https://software.intel.com/sites/default/files/managed/4d/91/DRNG_Software_Implementation_Guide_2.0.pdf
     while cnt < cRDRAND_RETRIES do
     begin
          if TryRdRand(y) then
             break;

          inc(cnt);
     end;

     if cnt = cRDRAND_RETRIES then
        raise Exception.Create('Error - RDRAND failed too often');

     Result := y mod aRange;
end;
{$ELSE}
begin
     raise Exception.Create('Not implemented without hardware support');
end;
{$ENDIF}

// ###########################################
// #### Mersenne Twister
// ###########################################

procedure TRandomGenerator.InitMersenne(Seed: LongInt);
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

procedure TRandomGenerator.InitMersenneByArr(const initKey: array of LongWord);
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

function TRandomGenerator.RandMersenneInt(const ARange: LongInt): LongInt;
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

constructor TRandomGenerator.Create(aRandMethod: TRandomAlgorithm);
begin
     SetCpuInstrSet(cpuSet);
     SetChaChaMode(cmSpeed);
     SetRandMethod(aRandMethod);

     inherited Create;
end;

function TRandomGenerator.RandIndexArr(aFrom, aTo: integer): TIntegerDynArray;
var i: Integer;
begin
     Result := nil;
     SetLength(Result, aTo - aFrom + 1);

     for i := 0 to Length(Result) - 1 do
         Result[i] := aFrom + i;

     RandIndexArr(Result);
end;

procedure TRandomGenerator.RandIndexArr(var idx: TIntegerDynArray);
var i: Integer;
    index, tmp : integer;
begin
     // Fisher yates shuffle:
     for i := Length(idx) - 1 downto 1 do
     begin
          index := RandInt(i + 1);

          tmp := idx[index];
          idx[index] := idx[i];
          idx[i] := tmp;
     end;

end;

// ###########################################
// #### ChaCha cipher random number generator
// some bases are from
// https://github.com/vnmakarov/mum-hash/blob/master/src/chacha-prng.h

{$IF defined(x64) or defined(MRMATH_NOASM)}
function rol(value: LongWord; Bits: Byte): LongWord;
begin
     Result := (value shl Bits) or (value shr (32 - bits));
end;
{$ELSE}
function rol(value: LongWord; Bits: Byte): LongWord;
{$IFDEF FPC}
begin
{$ENDIF}
asm
   xchg cl,dl
   rol eax, cl
end;
{$IFDEF FPC}
end;
{$ENDIF}
{$IFEND}

procedure TRandomGenerator.InitChaCha(const k: TChaChaK; const iv: TChaChaIV);
begin
     fChaChaInitVec(k, iv);
end;

procedure TRandomGenerator.SeedChaCha(seed: longInt);
var k : TChaChaK;
    iv : TChaChaIV;
begin
     if seed = 0 then
        seed := Integer(MtxGetTime);

     FillChar(k, sizeof(k), 0);
     FillChar(iv, sizeof(iv), 0);
     k[0] := LongWord(seed);
     fChaChaInitVec(k, iv);
end;

procedure TRandomGenerator.InitChaChaByVec(k: TChaChaK; iv: TChaChaIV);
var i : integer;
    numBytes : integer;
// from chacha-prng.h
const cChaChaConst : Array[0..15] of AnsiChar = 'expand 32-byte k';
begin
     if fChaChaMem = nil then
     begin
          // allocate for the AVX case -> 2 cha cha matrices at once
          numBytes := $20 + 2*sizeof(TChaChaMtx);
          fChaChaMem := GetMemory( numBytes );

          fInpChaChaMtx := AlignPtr32(fChaChaMem);
          fOutChaChaMTX := fInpChaChaMtx;
          inc(fOutChaChaMTX);
     end;

     Move(cChaChaConst[0], fInpChaChaMTX^[0], sizeof(cChaChaConst));
     for i := 0 to High(k) do
         fInpChaChaMTX^[i + 4] := k[i];
     fInpChaChaMTX^[12] := 0;
     fInpChaChaMTX^[13] := 0;
     fInpChaChaMTX^[14] := iv[0];
     fInpChaChaMTX^[15] := iv[1];

     fChaChaIdx := Length(fInpChaChaMTX^);
end;


procedure TRandomGenerator.InitChaChaRandom;
var k : TChaChaK;
    iv : TChaChaIV;
    i : integer;
begin
     // init with random mersenne twister random numbers (maybe something else is better?)
     InitMersenne(0);
     for i := 0 to High(k) do
         k[i] := RandMersenneDblWord;
     iv[0] := RandMersenneDblWord;
     iv[1] := RandMersenneDblWord;

     InitChaChaByVec(k, iv);
end;

// file from https://github.com/google/google-ctf/blob/master/third_party/tomcrypt/src/prngs/chacha20.c
// or https://git.ssrc.us:4443/elm/twizzler-public/-/tree/public/third-party/libtomcrypt/src/stream/chacha
// chacha random generator from:
// https://cr.yp.to/chacha/chacha-20080128.pdf
procedure TRandomGenerator.ChaChaAddEntropy(const inBuf: array of Byte);
var buf : Array[0..39] of byte; // 64byte - constants (16bytes) - counter (8bytes)
    aVal : LongWord;
    i : Integer;
begin
     if fChaChaMem = nil then
        raise Exception.Create('Error - ChaCha not initialized');
     // get the next 40 random numbers from the stream and fill buf
     for i := 0 to Length(buf) div 4 - 1 do
     begin
          aVal := fChaChaDblWord();
          Move( aVal, buf[i*sizeof(LongWord)], sizeof(longword));
     end;

     // now mix it up with the input
     for i := 0 to Length(inbuf) - 1 do
         buf[i mod Length(buf)] := buf[i mod Length(buf)] xor inBuf[i];

     // setup prng fields
     Move( buf[0], fInpChaChaMTX^[4], 32 );
     Move( buf[32], fInpChaChaMTX^[14], 8 );
     fInpChaChaMTX^[12] := 0;  // counter (UINT64) is 0: counter and $FFFFFFFF
     fInpChaChaMTX^[13] := 0;  // counter shr 32;
end;

procedure TRandomGenerator.ChaChaQuarterRound(var a, b, c, d: LongWord);
begin
     // ###########################################
     // ####
     // a += b; d ^= a; d <<<= 16;
     // c += d; b ^= c; b <<<= 12;
     // a += b; d ^= a; d <<<= 8;
     // c += d; b ^= c; b <<<= 7;
     a := a + b;
     d := d xor a;
     d := rol(d, 16);

     c := c + d;
     b := b xor c;
     b := rol(b, 12);

     a := a + b;
     d := d xor a;
     d := rol(d, 8);

     c := c + d;
     b := b xor c;
     b := rol(b, 7);
end;

procedure TRandomGenerator.PasChaChaDoubleQuarterRound;
begin
     ChaChaQuarterRound( fOutChaChaMtx^[0], fOutChaChaMtx^[4], fOutChaChaMtx^[8], fOutChaChaMtx^[12]);
     ChaChaQuarterRound( fOutChaChaMtx^[1], fOutChaChaMtx^[5], fOutChaChaMtx^[9], fOutChaChaMtx^[13]);
     ChaChaQuarterRound( fOutChaChaMtx^[2], fOutChaChaMtx^[6], fOutChaChaMtx^[10], fOutChaChaMtx^[14]);
     ChaChaQuarterRound( fOutChaChaMtx^[3], fOutChaChaMtx^[7], fOutChaChaMtx^[11], fOutChaChaMtx^[15]);
     ChaChaQuarterRound( fOutChaChaMtx^[0], fOutChaChaMtx^[5], fOutChaChaMtx^[10], fOutChaChaMtx^[15]);
     ChaChaQuarterRound( fOutChaChaMtx^[1], fOutChaChaMtx^[6], fOutChaChaMtx^[11], fOutChaChaMtx^[12]);
     ChaChaQuarterRound( fOutChaChaMtx^[2], fOutChaChaMtx^[7], fOutChaChaMtx^[8], fOutChaChaMtx^[13]);
     ChaChaQuarterRound( fOutChaChaMtx^[3], fOutChaChaMtx^[4], fOutChaChaMtx^[9], fOutChaChaMtx^[14]);
end;

(*
// https://eprint.iacr.org/2013/759.pdf
Algorithm 5: DOUBLEQUARTERROUND (optimized for 128-bit vectors)
Input:  v0, v1, v2, v3 (state matrix as four 4x32-bit vectors, each vector includes one row)
Output: v0, v1, v2, v3 (updated state matrix)
Flow
    v0 += v1; v3 ^= v0; v3 <<<= (16, 16, 16, 16);
    v2 += v3; v1 ^= v2; v1 <<<= (12, 12, 12, 12);
    v0 += v1; v3 ^= v0; v3 <<<= ( 8,  8,  8,  8);
    v2 += v3; v1 ^= v2; v1 <<<= ( 7,  7,  7,  7);
    v1 >>>= 32; v2 >>>= 64; v3 >>>= 96;
    v0 += v1; v3 ^= v0; v3 <<<= (16, 16, 16, 16);
    v2 += v3; v1 ^= v2; v1 <<<= (12, 12, 12, 12);
    v0 += v1; v3 ^= v0; v3 <<<= ( 8,  8,  8,  8);
    v2 += v3; v1 ^= v2; v1 <<<= ( 7,  7,  7,  7);
    v1 <<<= 32; v2 <<<= 64; v3 <<<= 96; Return
*)
const cShuf8 : Array[0..15] of byte = (3, 0, 1, 2,
                                       7, 4, 5, 6,
                                       11, 8, 9, 10,
                                       15, 12, 13, 14 );

{$IFNDEF MRMATH_NOASM}

procedure TRandomGenerator.SSEChaChaDoubleQuarterRound;
{$IFDEF x64}
var dXMM4, dXMM5 : TXMMArr;
{$ENDIF}
{$IFDEF FPC}
begin
{$ENDIF}
asm
   // rcx seems to have "self" as reference
   {$IFDEF UNIX}
   // Linux uses a diffrent ABI -> copy over the registers so they meet with winABI
   // The parameters are passed in the following order:
   // RDI, RSI, RDX, RCX, r8, r9 -> mov to RCX, RDX, R8, R9, width and height
   // in our case only rdi to rcx
   mov rcx, rdi;
   {$ENDIF}
   {$IFDEF x64}
   movupd dXMM4, xmm4;
   movupd dXMM5, xmm5;

   // 64bit version
   movdqu xmm5, [rip + cShuf8];

   // move the matrix to xmm0 to xmm3
   mov rdx, TRandomGenerator(rcx).fOutChaChaMtx;
   movdqa xmm0, [rdx];
   movdqa xmm1, [rdx + 16];
   movdqa xmm2, [rdx + 32];
   movdqa xmm3, [rdx + 48];
   {$ELSE}
   movdqu xmm5, cShuf8;

   // move the matrix to xmm0 to xmm3
   mov edx, TRandomGenerator(eax).fOutChaChaMtx;
   movdqa xmm0, [edx];
   movdqa xmm1, [edx + 16];
   movdqa xmm2, [edx + 32];
   movdqa xmm3, [edx + 48];
   {$ENDIF}

   // v0 += v1; v3 ^= v0; v3 <<<= 16
   paddd xmm0, xmm1;
   pxor xmm3, xmm0;
   pshufhw xmm3, xmm3, $B1;  // 10 11 00 01
   pshuflw xmm3, xmm3, $B1;

   // v2 += v3; v1 ^= v2; v1 <<<= (12, 12, 12, 12);
   paddd xmm2, xmm3;
   pxor xmm1, xmm2;
   // rotate is x << n | x >> 32 - n
   movapd xmm4, xmm1;
   pslld xmm4, 12;
   psrld xmm1, 20;  // 32 - 12
   por xmm1, xmm4;

   // v0 += v1; v3 ^= v0; v3 <<<= ( 8,  8,  8,  8);
   paddd xmm0, xmm1;
   pxor xmm3, xmm0;
   pshufb xmm3, xmm5;

   // v2 += v3; v1 ^= v2; v1 <<<= ( 7,  7,  7,  7);
   paddd xmm2, xmm3;
   pxor xmm1, xmm2;
   movapd xmm4, xmm1;
   pslld xmm4, 7;
   psrld xmm1, 25;  // 32 - 7
   por xmm1, xmm4;

   // v1 >>>= 32; v2 >>>= 64; v3 >>>= 96;

   // palignr is actually a sse3 opcode but ok...
   movapd xmm4, xmm1;
   palignr xmm1, xmm4, 4;
   movapd xmm4, xmm2;
   palignr xmm2, xmm4, 8;
   movapd xmm4, xmm3;
   palignr xmm3, xmm4, 12;


   // v0 += v1; v3 ^= v0; v3 <<<= 16
   paddd xmm0, xmm1;
   pxor xmm3, xmm0;
   pshufhw xmm3, xmm3, $B1;  // 10 11 00 01
   pshuflw xmm3, xmm3, $B1;

   // v2 += v3; v1 ^= v2; v1 <<<= (12, 12, 12, 12);
   paddd xmm2, xmm3;
   pxor xmm1, xmm2;
   // rotate is x << n | x >> 32 - n
   movapd xmm4, xmm1;
   pslld xmm4, 12;
   psrld xmm1, 20;  // 32 - 12
   por xmm1, xmm4;

   // v0 += v1; v3 ^= v0; v3 <<<= ( 8,  8,  8,  8);
   paddd xmm0, xmm1;
   pxor xmm3, xmm0;
   pshufb xmm3, xmm5;

   // v2 += v3; v1 ^= v2; v1 <<<= ( 7,  7,  7,  7);
   paddd xmm2, xmm3;
   pxor xmm1, xmm2;
   movapd xmm4, xmm1;
   pslld xmm4, 7;
   psrld xmm1, 25;  // 32 - 7
   por xmm1, xmm4;

   // v1 <<<= 32; v2 <<<= 64; v3 <<<= 96; Return
   movapd xmm4, xmm1;
   palignr xmm1, xmm4, 12;
   movapd xmm4, xmm2;
   palignr xmm2, xmm4, 8;
   movapd xmm4, xmm3;
   palignr xmm3, xmm4, 4;

   // move back
   {$IFDEF x64}
   movdqa [rdx], xmm0;
   movdqa [rdx + 16], xmm1;
   movdqa [rdx + 32], xmm2;
   movdqa [rdx + 48], xmm3;

   // cleanup registers
   movupd xmm4, dXMM4;
   movupd xmm5, dXMM5;
   {$ELSE}
   movdqa [edx], xmm0;
   movdqa [edx + 16], xmm1;
   movdqa [edx + 32], xmm2;
   movdqa [edx + 48], xmm3;
   {$ENDIF}
end;

{$IFDEF FPC}
end;
{$ENDIF}

procedure TRandomGenerator.AVXInitChaChaByVec(k: TChaChaK; iv: TChaChaIV);
var i : integer;
    numBytes : integer;
// from chacha-prng.h
const cChaChaConst : Array[0..15] of AnsiChar = 'expand 32-byte k';
begin
     if fChaChaMem = nil then
     begin
          // allocate for the AVX case -> 2 cha cha matrices at once
          numBytes := $40 + sizeof(TChaChaMtx) + Sizeof(TChaChaAVXMtx);
          fChaChaMem := GetMemory( numBytes );

          fInpChaChaMtx := AlignPtr64(fChaChaMem);
          fOutChaChaMtxAVX := PChaChaAVXMtx( fInpChaChaMtx );
          inc(PByte(fOutChaChaMtxAVX), sizeof(TChaChaMtx));
     end;

     Move(cChaChaConst[0], fInpChaChaMTX^[0], sizeof(cChaChaConst));
     for i := 0 to High(k) do
         fInpChaChaMTX^[i + 4] := k[i];
     fInpChaChaMTX^[12] := 0;
     fInpChaChaMTX^[13] := 0;
     fInpChaChaMTX^[14] := iv[0];
     fInpChaChaMTX^[15] := iv[1];

     fChaChaIdx := High(TChaChaAVXMtx) + 1;
end;


function TRandomGenerator.AVXRandChaChaDblWord: Longword;
var i : integer;
begin
     if fChaChaIdx > High(TChaChaAVXMtx) then
     begin
          // move the input matrix so we have nicely aligned memory for the quarter round
          for i := 0 to 3 do
          begin
               Move(fInpChaChaMtx^[4*i], fOutChaChaMtxAVX^[8*i], 4*sizeof(LongWord));
               Move(fInpChaChaMtx^[4*i], fOutChaChaMtxAVX^[8*i + 4], 4*sizeof(LongWord));
          end;

          // update index in the "second" matrix - this one is always odd so no further check
          inc(fOutChaChaMtxAVX^[3*8 + 4]);

          for i := 0 to fNumChaChaRounds - 1 do
              AVXChaChaDoubleQuarterRound(fOutChaChaMtxAVX);
          // undo the alignment so both matrices are adjacent again (not intermittent as used for the rounds)
          AVXRealingAndAddMtx(fOutChaChaMtxAVX, fInpChaChaMTX);

          // increment the one value that is off by one from the input matrix
          inc(fOutChaChaMtxAVX^[16 + 12]);

          // increment input matrix by 2
          fChaChaIdx := 0;
          inc(fInpChaChaMTX^[12], 2);
          if fInpChaChaMTX^[12] = 0 then
             inc(fInpChaChaMTX^[13]);
     end;

     Result := fOutChaChaMtxAVX^[fChaChaIdx];
     inc(fChaChaIdx);
end;

function TRandomGenerator.SSERandChaChaDblWord: Longword;
var i : integer;
begin
     if fChaChaIdx > High(fInpChaChaMTX^) then
     begin
          Move(fInpChaChaMTX^, fOutChaChaMtx^, sizeof(TChaChaMtx));

          for i := 0 to fNumChaChaRounds - 1 do
              SSEChaChaDoubleQuarterRound;
          for i := 0 to High(fInpChaChaMTX^) do
              inc(fOutChaChaMtx^[i], fInpChaChaMTX^[i]);

          fChaChaIdx := 0;

          inc(fInpChaChaMTX^[12]);
          if fInpChaChaMTX^[12] = 0 then
             inc(fInpChaChaMTX^[13]);
     end;

     Result := fOutChaChaMtx^[fChaChaIdx];
     inc(fChaChaIdx);
end;
{$ENDIF}

function TRandomGenerator.PasRandChaChaDblWord: Longword;
var i : integer;
begin
     if fChaChaIdx > High(fInpChaChaMTX^) then
     begin
          Move(fInpChaChaMTX^, fOutChaChaMtx^, sizeof(TChaChaMtx));

          for i := 0 to fNumChaChaRounds - 1 do
              PasChaChaDoubleQuarterRound;
          for i := 0 to High(fInpChaChaMTX^) do
              inc(fOutChaChaMtx^[i], fInpChaChaMTX^[i]);

          fChaChaIdx := 0;

          inc(fInpChaChaMTX^[12]);
          if fInpChaChaMTX^[12] = 0 then
             inc(fInpChaChaMTX^[13]);
     end;

     Result := fOutChaChaMtx^[fChaChaIdx];
     inc(fChaChaIdx);
end;

function TRandomGenerator.RandChaChaDbl: double;
begin
     Result := fChaChaDblWord()*two2neg32;
end;

function TRandomGenerator.RandChaChaInt(const ARange: LongInt): LongInt;
var y : LongWord;
begin
     y := fChaChaDblWord();
     // ensure positive integer (base range from 0 - maxint)
     Result := (LongInt(y and $7FFFFFFF)) mod aRange;
end;

function TRandomGenerator.RandChaChaLW(const ARange: LongWord): LongWord;
begin
     Result := fChaChaDblWord() mod aRange;
end;

initialization
  TRandomGenerator.cpuSet := itSSE;

end.
