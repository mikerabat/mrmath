// ###################################################################
// #### This file is part of the mathematics library project, and is
// #### offered under the licence agreement described on
// #### http://www.mrsoft.org/
// ####
// #### Copyright:(c) 2024, Michael R. . All rights reserved.
// ####
// #### Unless required by applicable law or agreed to in writing, software
// #### distributed under the License is distributed on an "AS IS" BASIS,
// #### WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// #### See the License for the specific language governing permissions and
// #### limitations under the License.
// ###################################################################

unit Wavelets;
{*

discrete and dyadic wavelet transformation:

the dyadic version (DYADIC_DWT) is the same as the swt transformation in matlab. This version is not downsampling the input vector and leads to
level * length(input) data. This transformation is invariant regarding the translation which is important if we will use morphological
information @ scale n

the FAST_DWT is the same as wavedec in matlab. Faster but not invariant regarding the translation (could be used for denoising ....)

to verify this implementation against matlab you must set dwtmode('per') in matlab !!!!

general:
to implement new wavelets just use the matlab function wfilters to get the filters. The bi-orthogonal wavelets are very intersting
because they are used in many 'edge' detection algorithms... (linear phase, ...)
bior3.1 is the quadratic spline wavelet....

if we have bior v,p we can say:

the bigger p the better the localization in the frequency domain
the lower p the better the localization in the time domain

good localization in the frequnecy domain requires a high p
good localization in the time domain requires a low p and low v

*}

interface

uses SysUtils, Types, MatrixConst;

type
  TWaveletType = (wtDaubechies2, wtBiortho13, wtBiortho22, wtBiortho31, wtBiortho39, 
                  wtMexicanHat, wtGaus1, wtGaus2, wtGaus3, wtGaus4); // these here do not have support for dwt! only for cwt
  TWaveletMode = (wmFast, wmDyadic, wmContionus);

  {
      consts for the SCAL denoising parameter
      'one' for no rescaling.
      'sln' for rescaling using a single estimation
         of level noise based on first level coefficients.
      'mln' for rescaling done using level dependent
         estimation of level noise.
    }
  TDenoisingScaleType = (stNoRescale, stSingleEstimation, stLevelDepEstimation);

  {
      consts for the tsr denoising parameter
      'rigrsure' use principle of Stein's Unbiased Risk.
      'heursure' is an heuristic variant of the first option.
      'sqtwolog' for universal threshold sqrt(2*log(.)).
      'minimaxi' for minimax thresholding.
  }
  TDenoisingThresholdEstimate = (teRegresure, teHeursure, tesqtwolog, teMiniMaxi);

  //consts for the SORH denoising parameter = hard or soft thresholding
  TDenoisingThresholdType = (ttHard, ttSoft);

// #################################################
// #### main Wavelet class - used for
// 1) creating the wavelet signal
// 2) denoising
type
  TWavelet = class
  protected
    // internal structure which uses 16Byte aligned memory pointers to hold the wavelet coefficients
    type
      TWaveFilt = record
        Lo_D : PConstDoubleArr;
        Hi_D : PConstDoubleArr;
        Lo_R : PConstDoubleArr;
        Hi_R : PConstDoubleArr;
        PMemLo_D : Pointer;
        PMemHi_D : Pointer;
        PMemLo_R : Pointer;
        PMemHi_R : Pointer;

        // precalculated reversed elements
        Lo_D_Rev : PConstDoubleArr;
        Hi_D_Rev : PConstDoubleArr;
        Lo_R_Rev : PConstDoubleArr;
        Hi_R_Rev : PConstDoubleArr;
        PMemLo_D_Rev : Pointer;
        PMemHi_D_Rev : Pointer;
        PMemLo_R_Rev : Pointer;
        PMemHi_R_Rev : Pointer;

        ncof : integer;
      end;

  type
    // the function assumes to get the reversed filter
    TDyadicConvolutionFunc = procedure(const inpv : TDoubleDynArray; const filtReverse : PConstDoubleArr; const level : integer; pRes : PConstDoubleArr) of Object;
    TConvolutionFunc = procedure(const inpv : TDoubleDynArray; const filt : PConstDoubleArr; order : integer; pRes : PConstDoubleArr) of Object;
  // #################################################
  // #### Member definitions
  private
    fMaxMaxlevel : integer;                       // theoretical max detail level
    fMaxlevel : integer;                          // max detail level for analysis
    fwavelettype : TWaveletType;
    fDWTMode: TWaveletMode;                       // dyadic or fast
    fDyadicConvolution : TDyadicConvolutionFunc;
    fConvolution : TConvolutionFunc;
    fInputLen : integer;
    fInputVec : TDoubleDynArray;                   // holds reference to original input
    fappc : TDynArrayofDoubleArray;                // holds approximation coefficients
    fdetc : TDynArrayofDoubleArray;                // holds detail coefficients
    fwfilt : TWavefilt;                            // mother wavelet

    fTmp : Pointer;
    fTmpSize : integer;
    fAlignedFiltInverse : PDouble;

    procedure InitAlignedFilter( filtLen : integer; filt : PConstDoubleArr );

    procedure CheckDWTSupport;                     // raises exception if the waveletype is not suitable for DWT
    
  // ###########################################
  // #### continuous wavelet support
  private
    fPsi1, fPsi2 : TDoubleDynArray;                // stores the calculated continous wavelts for later usage (there doesn't need to be convolution each time)

    function IsCalcWavelet : boolean;
    function WvletStep(iter : integer) : double;
    function IsOrthoWavelet : boolean;
    function MexicanHat(const lb, ub : double; numPts : integer) : TDoubleDynArray;
    function Gauss(const lb, ub : double; numPts : integer) : TDoubleDynArray;
    
    procedure WaveFunc(iter : integer; var psi1, psi2 : TDoubleDynArray; integrate : boolean);
    function IntegratedWavCoef(scale : integer; var order : integer; var pMem : Pointer) : PConstDoubleArr;
  protected
  
    // Daubechies Wavlet Filter Coefficients DB2
    const DB2_Lo_D : array [0..3] of double = (-0.12940952255092,   0.22414386804186,   0.83651630373747,   0.48296291314469);
    const DB2_Hi_D : array [0..3] of double = (-0.48296291314469,   0.83651630373747,  -0.22414386804186,  -0.12940952255092);
    const DB2_Lo_R : array [0..3] of double = ( 0.48296291314469,   0.83651630373747,   0.22414386804186,  -0.12940952255092);
    const DB2_Hi_R : array [0..3] of double = (-0.12940952255092,  -0.22414386804186,   0.83651630373747,  -0.48296291314469);

    // Biorthogonal 2.2 Wavelet
    const BIOR2_2_Lo_D : array [0..5] of double = (0, -0.125,   0.25,   0.75,   0.25, -0.125);
    const BIOR2_2_Hi_D : array [0..5] of double = (0, 0.25,   -0.5,    0.25,   0,     0     );
    const BIOR2_2_Lo_R : array [0..5] of double = (0, 0.25,    0.5,    0.25,   0,     0     );
    const BIOR2_2_Hi_R : array [0..5] of double = (0, 0.125,   0.25,  -0.75,   0.25,  0.125 );

    // Biorthogonal 3.1 Wavelet
    const BIOR3_1_Lo_D : array [0..3] of double = (-0.25,   0.75,   0.75,  -0.25);
    const BIOR3_1_Hi_D : array [0..3] of double = (-0.125,  0.375, -0.375,  0.125);
    const BIOR3_1_Lo_R : array [0..3] of double = ( 0.125,  0.375,  0.375,  0.125);
    const BIOR3_1_Hi_R : array [0..3] of double = (-0.25,  -0.75,   0.75,   0.25);

    // Biorthogonal 1.3 Wavelet
    const BIOR1_3_Lo_D : array [0..5] of double = (-0.06250000000000,   0.06250000000000,
                                                    0.50000000000000,   0.50000000000000,
                                                    0.06250000000000,  -0.06250000000000);
    const BIOR1_3_Hi_D : array [0..5] of double = (0,0,-0.5,0.5,0,0);
    const BIOR1_3_Lo_R : array [0..5] of double = (0,0,0.5,0.5,0,0);
    const BIOR1_3_Hi_R : array [0..5] of double = (-0.06250000000000,  -0.06250000000000,
                                                    0.50000000000000,  -0.50000000000000,
                                                    0.06250000000000,   0.06250000000000);


    // Biorthogonal 3.9 Wavelet
    const BIOR3_9_Lo_D : array [0..19] of double = ( -0.00067974437278,   0.00203923311835,   0.00506031921961,  -0.02061891264111,
                                                     -0.01411278793018,   0.09913478249423,   0.01230013626942,  -0.32019196836078,
                                                      0.00205002271157,   0.94212570067821,   0.94212570067821,   0.00205002271157,
                                                     -0.32019196836078,   0.01230013626942,   0.09913478249423,  -0.01411278793018,
                                                     -0.02061891264111,   0.00506031921961,   0.00203923311835,  -0.00067974437278);
    const BIOR3_9_Hi_D : array [0..19] of double = (  0, 0, 0, 0, 0, 0, 0, 0,
                                                     -0.17677669529664,   0.53033008588991,  -0.53033008588991,   0.17677669529664,
                                                      0, 0, 0, 0, 0, 0, 0, 0);
    const BIOR3_9_Lo_R : array [0..19] of double = (  0,0,0,0,0,0,0,0,
                                                      0.17677669529664,   0.53033008588991,   0.53033008588991,   0.17677669529664,
                                                      0, 0, 0, 0, 0, 0, 0, 0);
    const BIOR3_9_Hi_R : array [0..19] of double = ( -0.00067974437278,  -0.00203923311835,   0.00506031921961,   0.02061891264111,
                                                     -0.01411278793018,  -0.09913478249423,   0.01230013626942,   0.32019196836078,
                                                      0.00205002271157,  -0.94212570067821,   0.94212570067821,  -0.00205002271157,
                                                     -0.32019196836078,  -0.01230013626942,   0.09913478249423,   0.01411278793018,
                                                     -0.02061891264111,  -0.00506031921961,   0.00203923311835,   0.00067974437278);

    procedure InitWaveCoeffs(numCoef : integer);
    procedure ResetFilter;
    procedure ClearWaveCoeffs;

    // convolution - periodic + centered version
    // note: filt needs to be 16Byte aligned
    function conv(const inpv: TDoubleDynArray; const filt: PConstDoubleArr; order : integer) : TDoubleDynArray;
    //procedure CoreConvSSE(const inpv : TDoubleDynArray; const filt : PConstDoubleArr; order : integer; pRes : PConstDoubleArr);
    procedure CoreConvPas(const inpv : TDoubleDynArray; const filt : PConstDoubleArr; order : integer; pRes : PConstDoubleArr);

    // note: the filt coefficients must be 16byte aligned
    function convDyadic(const inpv: TDoubleDynArray; const filt, filtReverse: PConstDoubleArr; const level:integer) : TDoubleDynArray;
    
    procedure CoreConvDyadicPas(const inpv: TDoubleDynArray; const filtReverse: PConstDoubleArr; const level:integer; pRes : PConstDoubleArr);

    // decimate vector val by factor 2 : result[0] = val[1], result[1] = val[3], ....
    function dyadDown(const val : TDoubleDynArray) :  TDoubleDynArray;

    // insert zeros @ index = 1,3,5,7,.....
    function dyadUp(const val : TDoubleDynArray) : TDoubleDynArray;

    // single step dyadic wavelet transformation
    procedure dwtDyadic(const inpv : TDoubleDynArray; var detcv : TDoubleDynArray; var appcv : TDoubleDynArray; const level : integer);

    // single step discrete wavelet transformation
    procedure InternalDWT(const inpv : TDoubleDynArray; var detcv : TDoubleDynArray; var appcv : TDoubleDynArray);

    // single step inverse discrete wavelet transformation
    function InternalIDWT(const a : TDoubleDynArray; const d : TDoubleDynArray) : TDoubleDynArray;

    // set the maximum decomposition level
    procedure setMaxLevel (level : integer);


    // ###########################################
    // #### noise reduction 
    procedure wthresh(var x : TDoubleDynArray; mode : TDenoisingThresholdType; const thr : double);
    function noiseest(level : integer) : double;
    function thselect(const x : TDoubleDynArray; tsr : TDenoisingThresholdEstimate) : double;

    // ###########################################
    // #### Data returning
    function InternalGetDetailC(level : integer) : TDoubleDynArray;
    function InternalGetApproximationC(level : integer) : TDoubleDynArray;

    property InputLen : integer read fInputLen;
    property InputVec : TDoubleDynArray read fInputVec;
    property DetailVec : TDynArrayofDoubleArray read fdetc;
    property ApproxVec : TDynArrayofDoubleArray read fappc;

    public
    constructor Create(wavtype : TWaveletType; NoSSE : boolean = False; unroledLoops : boolean = True);
    destructor Destroy; override;

    // ###########################################
    // #### Decompositions
    procedure DWT(const input : TDoubleDynArray; maxLevel : integer);        // discrete fast wavelet transform
    procedure CWT(const input : TDoubleDynArray; levels : TIntegerDynArray); // continuous wavelet transform<
    procedure DyadicDWT(const input : TDoubleDynArray; maxLevel : integer);  // swt in matlabe. Basically the same as above but different convolution

    // get detail coefs of specified level
    function getDetailC(level : integer) : TDoubleDynArray;
    // get reconstructed signal of specified detail level
    function getDetail(level:integer) : TDoubleDynArray;

    // get approximation coefs of specified level
    function getApproximationC(level : integer) : TDoubleDynArray;
    // get reconstructed signal of specified approximation level
    function getApproximation(level:integer) : TDoubleDynArray;

    // approximation Coeff calculated by using the detail coefs (used e.g. for denoising)
    // level '0' means that the result is the reconstructed signal
    function AppCR(level : integer) : TDoubleDynArray;

    procedure Denoise(tsr : TDenoisingThresholdEstimate; cmode : TDenoisingThresholdType; scal : TDenoisingScaleType);

    property DWTMode : TWaveletMode read fDWTMode;
    property MaxLevel : integer read fMaxLevel;
    property WaveletType : TWaveletType read fwavelettype;
end;

implementation

uses MathUtilFunc, Math, CPUFeatures, MatrixASMStubSwitch, ASMVecConvolve;

// ###########################################
// #### Construction/Destruction
// ###########################################

{$REGION 'Construction/Destruction/INIT'}

constructor TWavelet.Create(wavtype : TWaveletType; NoSSE : boolean; unroledLoops : boolean);
begin
     // #################################################
     // #### Check processor type:
     // check cpu for availability of special instruction sets
     if False then //not NoSSE and IsSSE3Present then
     begin
          //fDyadicConvolution := CoreConvDyadicSSE;

          //fConvolution := CoreConvSSE;
     end
     else
     begin
          fDyadicConvolution := CoreconvDyadicPas;
          fConvolution := CoreConvPas;
     end;

     FillChar(fwfilt, sizeof(fwfilt), 0);
     
     fmaxlevel := 0;
     fmaxmaxlevel := 0;
     fwavelettype := wavtype;
     fDWTMode := wmFast;

     ResetFilter;
end;

destructor TWavelet.Destroy;
begin
     ClearWaveCoeffs;
     
     inherited;
end;

procedure TWavelet.InitAlignedFilter(filtLen: integer; filt : PConstDoubleArr);
var i : integer;
begin
     if filtLen*sizeof(double) > fTmpSize then
     begin
          if Assigned(fTmp) then
             FreeMem(fTmp);
          fAlignedFiltInverse := nil;

          // allocate double the memory - keep some spare
          fAlignedFiltInverse := MtxMallocAlign( 2*filtLen*sizeof(double), fTmp );
          fTmpSize := filtLen*sizeof(double)*2;
     end;

     // initialize memory with the inversed filter
     for i := 0 to filtLen - 1 do
         PConstDoubleArr(fAlignedFiltInverse)^[i] := filt^[filtLen - 1 - i];
end;

procedure TWavelet.InitWaveCoeffs(numCoef: integer);
begin
     ClearWaveCoeffs;

     fwfilt.ncof := numCoef;

     // assign memory 16 byte alligned for fast sse access
     fwfilt.Lo_D := MtxMallocAlign( numCoef*sizeof(double), fwfilt.PMemLo_D);
     fwfilt.Hi_D := MtxMallocAlign( numCoef*sizeof(double), fwfilt.PMemHi_D);
     fwfilt.Lo_R := MtxMallocAlign( numCoef*sizeof(double), fwfilt.PMemLo_R);
     fwfilt.Hi_R := MtxMallocAlign( numCoef*sizeof(double), fwfilt.PMemHi_R);

     fwfilt.Lo_D_Rev := MtxMallocAlign( numCoef*sizeof(double), fwfilt.PMemLo_D_Rev);
     fwfilt.Hi_D_Rev := MtxMallocAlign( numCoef*sizeof(double), fwfilt.PMemHi_D_Rev);
     fwfilt.Lo_R_Rev := MtxMallocAlign( numCoef*sizeof(double), fwfilt.PMemLo_R_Rev);
     fwfilt.Hi_R_Rev := MtxMallocAlign( numCoef*sizeof(double), fwfilt.PMemHi_R_Rev);
end;

procedure TWavelet.ClearWaveCoeffs;
begin
     if Assigned(fwfilt.PMemLo_D) then
        FreeMem(fwfilt.PMemLo_D);
     if Assigned(fwfilt.PMemHi_D) then
        FreeMem(fwfilt.PMemHi_D);
     if Assigned(fwfilt.PMemLo_R) then
        FreeMem(fwfilt.PMemLo_R);
     if Assigned(fwfilt.PMemHi_R) then
        FreeMem(fwfilt.PMemHi_R);

     if Assigned(fwfilt.PMemLo_D_Rev) then
        FreeMem(fwfilt.PMemLo_D_Rev);
     if Assigned(fwfilt.PMemHi_D_Rev) then
        FreeMem(fwfilt.PMemHi_D_Rev);
     if Assigned(fwfilt.PMemLo_R_Rev) then
        FreeMem(fwfilt.PMemLo_R_Rev);
     if Assigned(fwfilt.PMemHi_R_Rev) then
        FreeMem(fwfilt.PMemHi_R_Rev);

     FillChar(fwfilt, sizeof(fwfilt), 0);
end;


procedure TWavelet.ResetFilter;
var i : integer;
    sqrt2 : double;
procedure ReverseVec( src, dest : PConstDoubleArr; len : integer);
var counter: Integer;
begin
     for counter := 0 to Len - 1 do
         dest^[len - counter - 1] := src[counter];
end;
begin
     sqrt2 := sqrt(2);
     case fwavelettype of
       wtBiortho22 :
                    begin
                         InitWaveCoeffs(6);

                         for i := 0 to 5 do
                         begin
                              fwfilt.Lo_D[i] := BIOR2_2_Lo_D[i]*sqrt2;
                              fwfilt.Hi_D[i] := BIOR2_2_Hi_D[i]*sqrt2;
                              fwfilt.Lo_R[i] := BIOR2_2_Lo_R[i]*sqrt2;
                              fwfilt.Hi_R[i] := BIOR2_2_Hi_R[i]*sqrt2;

                              ReverseVec(fwfilt.Lo_D, fwfilt.Lo_D_Rev, 6);
                              ReverseVec(fwfilt.Hi_D, fwfilt.Hi_D_Rev, 6);
                              ReverseVec(fwfilt.Lo_R, fwfilt.Lo_R_Rev, 6);
                              ReverseVec(fwfilt.Hi_R, fwfilt.Hi_R_Rev, 6);
                         end
                    end;

       wtBiortho31 :
                    begin
                         InitWaveCoeffs(4);

                         for i := 0 to 3 do
                         begin
                              fwfilt.Lo_D[i] := BIOR3_1_Lo_D[i]*sqrt2;
                              fwfilt.Hi_D[i] := BIOR3_1_Hi_D[i]*sqrt2;
                              fwfilt.Lo_R[i] := BIOR3_1_Lo_R[i]*sqrt2;
                              fwfilt.Hi_R[i] := BIOR3_1_Hi_R[i]*sqrt2;

                              ReverseVec(fwfilt.Lo_D, fwfilt.Lo_D_Rev, 4);
                              ReverseVec(fwfilt.Hi_D, fwfilt.Hi_D_Rev, 4);
                              ReverseVec(fwfilt.Lo_R, fwfilt.Lo_R_Rev, 4);
                              ReverseVec(fwfilt.Hi_R, fwfilt.Hi_R_Rev, 4);
                         end
                    end;

       wtBiortho13 :
                    begin
                         InitWaveCoeffs(6);

                         for i := 0 to 5 do
                         begin
                              fwfilt.Lo_D[i] := BIOR1_3_Lo_D[i]*sqrt2;
                              fwfilt.Hi_D[i] := BIOR1_3_Hi_D[i]*sqrt2;
                              fwfilt.Lo_R[i] := BIOR1_3_Lo_R[i]*sqrt2;
                              fwfilt.Hi_R[i] := BIOR1_3_Hi_R[i]*sqrt2;

                              ReverseVec(fwfilt.Lo_D, fwfilt.Lo_D_Rev, 6);
                              ReverseVec(fwfilt.Hi_D, fwfilt.Hi_D_Rev, 6);
                              ReverseVec(fwfilt.Lo_R, fwfilt.Lo_R_Rev, 6);
                              ReverseVec(fwfilt.Hi_R, fwfilt.Hi_R_Rev, 6);
                         end
                    end;

       wtBiortho39 :
                    begin
                         InitWaveCoeffs(20);

                         for i := 0 to 19 do
                         begin
                              fwfilt.Lo_D[i] := BIOR3_9_Lo_D[i];
                              fwfilt.Hi_D[i] := BIOR3_9_Hi_D[i];
                              fwfilt.Lo_R[i] := BIOR3_9_Lo_R[i];
                              fwfilt.Hi_R[i] := BIOR3_9_Hi_R[i];

                              ReverseVec(fwfilt.Lo_D, fwfilt.Lo_D_Rev, 20);
                              ReverseVec(fwfilt.Hi_D, fwfilt.Hi_D_Rev, 20);
                              ReverseVec(fwfilt.Lo_R, fwfilt.Lo_R_Rev, 20);
                              ReverseVec(fwfilt.Hi_R, fwfilt.Hi_R_Rev, 20);
                         end;
                    end;

       wtDaubechies2 :
                      begin
                           InitWaveCoeffs(4);

                           for i := 0 to 3 do
                           begin
                                fwfilt.Lo_D[i] := DB2_Lo_D[i];
                                fwfilt.Hi_D[i] := DB2_Hi_D[i];
                                fwfilt.Lo_R[i] := DB2_Lo_R[i];
                                fwfilt.Hi_R[i] := DB2_Hi_R[i];

                                ReverseVec(fwfilt.Lo_D, fwfilt.Lo_D_Rev, 4);
                                ReverseVec(fwfilt.Hi_D, fwfilt.Hi_D_Rev, 4);
                                ReverseVec(fwfilt.Lo_R, fwfilt.Lo_R_Rev, 4);
                                ReverseVec(fwfilt.Hi_R, fwfilt.Hi_R_Rev, 4);
                           end;
                      end;
       // ###########################################
       // #### (analyitcaly) Evaluated wavelets
       wtMexicanHat,
       wtGaus1,
       wtGaus2,
       wtGaus3,
       wtGaus4:       begin
                           // nothing to do: it's not a standard wavelet and cannot be used in dwt
                           // there is only support for cwt
                      end;
     else
         // default = BIOR2.2
         InitWaveCoeffs(6);
         
         for i := 0 to 5 do
         begin
              fwfilt.Lo_D[i] := BIOR2_2_Lo_D[i]*sqrt2;
              fwfilt.Hi_D[i] := BIOR2_2_Hi_D[i]*sqrt2;
              fwfilt.Lo_R[i] := BIOR2_2_Lo_R[i]*sqrt2;
              fwfilt.Hi_R[i] := BIOR2_2_Hi_R[i]*sqrt2;

              ReverseVec(fwfilt.Lo_D, fwfilt.Lo_D_Rev, 6);
              ReverseVec(fwfilt.Hi_D, fwfilt.Hi_D_Rev, 6);
              ReverseVec(fwfilt.Lo_R, fwfilt.Lo_R_Rev, 6);
              ReverseVec(fwfilt.Hi_R, fwfilt.Hi_R_Rev, 6);
         end
     end;
end;

{$ENDREGION}

// ###########################################
// #### Misc functions / getter 
// ###########################################

{$REGION 'Misc functions'}

procedure TWavelet.CheckDWTSupport;
begin
     if fwavelettype in [wtMexicanHat, wtGaus1, wtGaus2, wtGaus3, wtGaus4] then
        raise Exception.Create('Error Mexican hat wavelet is only defined for dyadic transformation');
end;

procedure TWavelet.setMaxLevel (level : integer);
begin
     if (level > fmaxmaxlevel) and (fmaxlevel <> 0) then
        level := fmaxmaxlevel;
        
     fmaxlevel := level;
end;

function TWavelet.InternalGetDetailC(level : integer) : TDoubleDynArray;
begin
     Result := nil;

     level := Min( level, fMaxlevel - 1 );
     if (level < 0) or (level >= Length(fdetc)) or (Length(fdetc[level]) = 0) then
        exit;

     Result := fdetc[level];
end;

function TWavelet.getDetailC(level : integer) : TDoubleDynArray;
begin
     Result := InternalGetDetailC(level);
end;

function TWavelet.InternalGetApproximationC(level : integer) : TDoubleDynArray;
begin
     Result := nil;

     level := min(level, fMaxMaxlevel - 1);

     if (level < 0) or (level >= Length(fappc)) or (Length(fappc[level]) = 0) then
        exit;

     Result := fappc[level];
end;

function TWavelet.getApproximationC(level : integer) : TDoubleDynArray;
begin
     Result := InternalGetApproximationC(level);
end;

function TWavelet.getDetail(level:integer) : TDoubleDynArray;
var detC, ddetC : TDoubleDynArray;
    detconvn : TDoubleDynArray;
    k : integer;
begin
     if level >= fmaxlevel then
        level := fmaxlevel - 1;

     detC := InternalGetDetailC(level);
     ddetC := dyadUp(detC);
     Result := conv(ddetC, fwfilt.Hi_R, fwfilt.ncof);

     for k := 1 to level do
     begin
          detconvn := dyadUp(Result);
          Result := conv(detconvn, fwfilt.Lo_R, fwfilt.ncof);
     end;
end;

function TWavelet.getApproximation(level:integer) : TDoubleDynArray;
var appC, dappC : TDoubleDynArray;
    appconvn : TDoubleDynArray;
    k : integer;
begin
     if level >= fmaxlevel then
        level := fmaxlevel - 1;

     appC := InternalGetApproximationC(level);
     dappC := dyadUp(appC);
     Result := conv(dappC, fwfilt.Lo_R, fwfilt.ncof);

     for k := 1 to level do
     begin
          appconvn := dyadUp(Result);
          Result := conv(appconvn, fwfilt.Lo_R, fwfilt.ncof);
     end;
end;

function TWavelet.AppCR(level : integer) : TDoubleDynArray;
var i : integer;
begin
     Result := nil;
     if (fDWTMode <> wmFast) or (fmaxLevel > Length(fappc) + 1) or (Length(fappc[fmaxLevel - 1]) = 0) then
        exit;

     Result := fappc[fmaxLevel - 1];
     for i := fmaxlevel - 1 downto level do
         Result := InternalIDWT(Result, fdetc[i]);
end;

function TWavelet.dyadUp(const val : TDoubleDynArray) : TDoubleDynArray;
var i, l : longword;
    pResult : PDouble;
    pValue : PDouble;
begin
     l := Length(val) shl 1;

     SetLength(Result, l);
     pResult := PDouble(@Result[0]);
     pValue := PDouble(@val[0]);
     for i := 0 to Length(val) - 1 do
     begin
          pResult^ := pValue^;
          inc(pResult);
          pResult^ := 0;
          inc(PResult);

          inc(pValue);
     end;
end;

function TWavelet.dyadDown(const val : TDoubleDynArray) :  TDoubleDynArray;
var i, l : longword;
begin
     l := Length(val) shr 1;
     SetLength(Result, l);

     i := 0;
     while i < l do
     begin
          result[i] := val[(i shl 1) + 1];
          Inc(i);
     end;
end;

{$ENDREGION}

// ###########################################
// #### standard Convolution with periodic border handling
// ###########################################

{$REGION 'Convolution'}

// calculates the 'centered' convolution of inpv and filt (periodic border handling)
// Length inpv > Length filt
function TWavelet.conv(const inpv: TDoubleDynArray; const filt: PConstDoubleArr; order : integer) : TDoubleDynArray;
var ni, orderd2, i, k, ov: integer;
    res : TDoubleDynArray;
begin
     orderd2 := order shr 1;
     ni := Length(inpv);
     Setlength(Result, ni);
     SetLength(res, ni);

     //fConvolution(inpv, filt, order, @result[0]);

     InitAlignedFilter(order, filt);
     VecConvolveRevB( @res[order div 2], @inpv[order - 1], PDouble(fAlignedFiltInverse), ni - order + 1, order);

     // calulate left 'border'
     ov := order;
     for k := orderd2 - 1 downto 0 do
     begin
          Result[k] := 0;
          dec(ov);

          for i := 0 to ov - 1 do
              Result[k] := Result[k] + inpv[ov - 1 - i]*filt[i];

          for i := 0 to (order - ov) - 1 do
              Result[k] := Result[k] + inpv[ni - 1 - i]*filt[i + ov];
     end;

     // calculate right 'border'
     ov := 0;
     for k := ni - orderd2 + 1 to ni - 1 do
     begin
          Result[k] := 0;
          inc(ov);

          for i := 0 to ov - 1 do
              Result[k] := Result[k] + inpv[ov - i - 1]*filt[i];

          for i := 0 to (order - ov) - 1 do
              Result[k] := Result[k] + inpv[ni - 1 - i]*filt[i + ov];
     end;
end;

procedure TWavelet.CoreConvPas(const inpv: TDoubleDynArray;
  const filt: PConstDoubleArr; order: integer; pRes: PConstDoubleArr);
var index, indexw, ni, orderd2, i : integer;
begin
     assert(order and 1 <> 1, 'Error only even filter lengths are allowed - (pad with 0)');
     
     orderd2 := order shr 1;
     ni := Length(inpv);

     index := order - 1;
     indexw := orderd2;
     
     while index < ni do
     begin
          pRes^[indexw] := 0.0;
          for i := 0 to order - 1 do
              pRes^[indexw] := pRes^[indexw] + inpv[index - i]*filt[i];

          Inc(index);
          Inc(indexw);
     end;     
end;


{$ENDREGION}

// ###########################################
// #### Dyadic convolution (periodic border handling)
// ###########################################

{$REGION 'Dyadic convolution'}

// calculates the 'centered' convolution of inpv and filt (periodic border handling)
// Length inpv > Length filt
// this dyadic version is ignoring multiplications by '0'  in case of the dyadic version of the DWT --> faster
function TWavelet.convDyadic(const inpv : TDoubleDynArray; const filt, filtReverse : PConstDoubleArr; const level : integer) : TDoubleDynArray;
var index, ni, order, orderd2, i, k, ov, dy: integer;
    orderBase : integer;
begin
     orderBase := fwfilt.ncof;
     
     order := orderBase shl level;
     orderd2 := order shr 1;
     ni := length(inpv);
     setlength(result, ni);
     dy := 1 shl level;
     
     fDyadicConvolution(inpv, filtReverse, level, @Result[0]);     
     
     // second left border
     ov := order;
     for k := orderd2 - 2 downto 0 do
     begin
          result[k] := 0;
          dec(ov);

          index := ov - 1;
          for i := 0 to orderBase - 1 do
          begin
               result[k] := result[k] + inpv[index]*filt[i];
               dec(index, dy);

               if index < 0 then
                  index := index + ni;
          end;
     end;


     ov := 0;
     for k := ni - orderd2 to ni - 1 do
     begin
          result[k] := 0;
          inc(ov);

          index := ov - 1;
          for i := 0 to orderBase - 1 do
          begin
               result[k] := result[k] + inpv[index]*filt[i];
               dec(index, dy);
               
               if index < 0 then
                  index := index + ni;
          end;
     end;
end;

procedure TWavelet.CoreConvDyadicPas(const inpv: TDoubleDynArray;
  const filtReverse: PConstDoubleArr; const level: integer; pRes: PConstDoubleArr);
var index, indexw : integer;
    ni, order, orderd2, i : integer;
    orderBase : integer;
    dy : integer;
begin
     orderBase := fwfilt.ncof;

     order := orderBase shl level;
     orderd2 := order shr 1;

     indexw := orderd2 - 1;
     dy := 1 shl level;
     index := order - 1 - (orderBase - 1)*dy;
     ni := length(inpv) - (orderBase - 1)*dy;

     // todo: make the following loop in assembler -> commented code is a start...
     //ASMVecConvolveRevBEx( @pRes^[indexw], @inpv[ index + (orderbase - 1)*dy], @filtReverse[0], ni - index, orderBase, dy );

     while index < ni do
     begin
          pRes^[indexw] := 0.0;

          for i := 0 to orderBase - 1 do
              pRes^[indexw] := pRes^[indexw] + inpv[index + i*dy] * filtReverse[i];

          Inc(index);
          Inc(indexw);
     end;
end;

{$ENDREGION}


// DWT_DYADIC performs a single-level 1-D wavelet dyadic decomposition
procedure TWavelet.dwtDyadic(const inpv : TDoubleDynArray; var detcv : TDoubleDynArray;
 var appcv : TDoubleDynArray; const level : integer);
begin
     appcv := convDyadic(inpv, fwfilt.Lo_D, fwfilt.Lo_D_Rev, level);
     detcv := convDyadic(inpv, fwfilt.Hi_D, fwfilt.Hi_D_Rev, level);
end;


// DWT performs a single-level 1-D wavelet decomposition
procedure TWavelet.InternalDWT(const inpv : TDoubleDynArray; var detcv : TDoubleDynArray; var appcv : TDoubleDynArray);
begin
     appcv := conv(inpv, fwfilt.Lo_D, fwfilt.ncof);
     appcv := dyadDown(appcv);
     detcv := conv(inpv, fwfilt.Hi_D, fwfilt.ncof);
     detcv := dyadDown(detcv);
end;

// IDWT performs the inverse DWT
function TWavelet.InternalIDWT(const a : TDoubleDynArray; const d : TDoubleDynArray) : TDoubleDynArray;
var rd, ra : TDoubleDynArray;
    i : Integer;
begin
     Result := nil;
     if fDWTMode = wmDyadic then
        exit;

     ra := dyadUp(a);
     ra := conv(ra, fwfilt.Lo_R, fwfilt.ncof);

     rd := dyadUp(d);
     rd := conv(rd, fwfilt.Hi_R, fwfilt.ncof);

     Setlength(Result, Length(ra));
     for i := 0 to Length(Result) - 1 do
         Result[i] := rd[i] + ra[i];
end;


// ###########################################
// #### Noise reduction
// ###########################################

{$REGION 'Noise reduction'}

procedure TWavelet.wthresh(var x : TDoubleDynArray; mode : TDenoisingThresholdType; const thr : double);
var i : integer;
    tmp : double;
begin
     //soft thresholding is shrinkage.
     if mode = ttSoft then
     begin
          for i := 0 to Length(x) - 1 do
          begin
               tmp := abs(x[i]) - thr;
               tmp := (tmp + abs(tmp))/2;
               if x[i] > 0
               then
                   x[i] := tmp
               else
                   x[i] := -tmp;
          end;
     end
     // hard thresholding is cruder.
     else
         for i := 0 to Length(x) - 1 do
             if abs(x[i]) <= thr then
                x[i] := 0;
end;

function TWavelet.noiseest(level : integer) : double;
var  i : integer;
     d : TDoubleDynArray;
     w : integer;
begin
     Dec(level);
     SetLength(d, Length(fdetc[level]));

     for i := 0 to Length(d) - 1 do
         d[i] := abs(fdetc[level][i]);


     w := Length(d);

     // median:
     if w and 1 = 1
     then
         Result := KthLargest(@d[0], w, w div 2)
     else
         Result := (KthLargest(@d[0], w, w div 2) + KthLargest(@d[0], w, Max(0, w div 2 - 1)))/2;
     Result := Result/0.6745;
end;

function TWavelet.thselect(const x : TDoubleDynArray; tsr : TDenoisingThresholdEstimate) : double;
var i, len, t1, t2, best : integer;
    vw, vc : TDoubleDynArray;
    min, hthr, eta, crit, w  : double;
begin
     len := Length(x);

     case tsr of
       teRegresure: begin
                        SetLength(vw, len);
                        SetLength(vc, len);
                        for i := 0 to len - 1 do
                            vw[i] := sqr(x[i]);

                        QuickSort(vw, 0, -1);
                        vc[0] := vw[0];

                        for i := 1 to len - 1 do
                            vc[i] := vc[i - 1] + vw[i];

                        t1 := len - 2;
                        t2 := len - 1;

                        for i := 0 to len - 1 do
                        begin
                             vc[i] := (t1 + (vc[i] + vw[i]*t2)) / len;
                             Dec(t1, 2);
                             Dec(t2);
                        end;

                        min := MAXDOUBLE;
                        best := 0;
                        for i := 0 to len - 1 do
                        begin
                             if vc[i] < min then
                             begin
                                  min := vc[i];
                                  best := i;
                             end;
                        end;

                        Result := sqrt(vw[best]);
                   end;

       teHeursure: begin
                        hthr := sqrt(2*ln(len));
                        eta := 0;
                        for i := 0 to len - 1 do
                            eta:= eta + sqr(x[i]);

                        eta := (eta - len)/len;
                        crit := power((ln(len)/ln(2)), (1.5)/sqrt(len));

                        if eta < crit
                        then
                            Result := hthr
                        else
                        begin
                             w := thselect(x, teRegresure);

                             if w < hthr
                             then
                                 Result := w
                             else
                                 Result := hthr;
                        end;
                  end;

       tesqtwolog: Result := sqrt(2*ln(len));

       teMiniMaxi: begin
                        if len <= 32
                        then
                            Result := 0
                        else
                            Result := 0.3936 + 0.1829*(ln(len)/ln(2));
                   end;
     else
         Result := sqrt(2*ln(len));
     end;
end;

procedure TWavelet.Denoise(tsr : TDenoisingThresholdEstimate; cmode : TDenoisingThresholdType; scal : TDenoisingScaleType );
var s, w : TDoubleDynArray;
    thr, sc, max : double;
    i, k: Integer;
begin
     SetLength(s,fmaxlevel);

     case scal of
       stNoRescale: for k := 0 to fmaxlevel - 1 do
                        s[k] := 1.0;

       stSingleEstimation : begin
                                 sc := noiseest(1);
                                 for k := 0 to fmaxlevel - 1 do
                                     s[k] := sc;
                            end;

       stLevelDepEstimation : for k := 0 to fmaxlevel - 1 do
                                  s[k] := noiseest(k + 1);
     end;

     for k := 0 to fmaxlevel - 1 do
     begin
          if (tsr = tesqtwolog) OR (tsr = teMiniMaxi) then
          begin
               SetLength(w, Length(fdetc[k]));
               Move(fdetc[k][0], w[0], Length(w)*sizeof(double));
               thr := thselect(w, tsr)
          end
          else
          begin
               max := MINDOUBLE;
               for i := 0 to Length(fdetc[k]) - 1 do
                   if max < fdetc[k][i] then
                      max := fdetc[k][i];

               if s[k] < (1.490116119384766e-008*max)
               then
                   thr := 0
               else
               begin
                    SetLength(w, Length(fdetc[k]));
                    for i := 0 to Length(fdetc[k]) - 1 do
                        w[i] := fdetc[k][i]/s[k];

                    thr := thselect(w, tsr);
               end;
          end;

          thr := thr*s[k];
          wthresh(fdetc[k], cmode, thr);    // detail coefs thresholding
     end;
end;

{$ENDREGION}

// ###########################################
// #### Continuous wavelet functions
// ###########################################

{$REGION 'Continuouse wavelet support'}

const cConstMaxWaveletIter = 10;  // 10 iterations to get the finest mother wavelet resolution
          cConstMexHatLB = -8;  // mexican hat standard bounds (from Matlab)
          cConstMexHatUB = 8;
          
          cConstGausLB = -5;  // defaults from Matlab
          cConstGausUB = 5;


function TWavelet.IsCalcWavelet: boolean;
begin
     Result := WaveletType in [wtMexicanHat, wtGaus1, wtGaus2, wtGaus3, wtGaus4];
end;

function TWavelet.IsOrthoWavelet: boolean;
begin
     Result := WaveletType in [wtDaubechies2];
end;

function TWavelet.MexicanHat(const lb, ub: double;
  numPts: integer): TDoubleDynArray;
var xInc : double;
    i : integer;
    x : double;
    scale : double;
    x2 : double;
begin
     scale := 2/(sqrt(3)*power(pi, 0.25));
     
     xInc := (ub - lb)/(numPts - 1);
     x := lb;
     SetLength(Result, numPts);
     for i := 0 to numPts - 2 do
     begin
          x2 := sqr(x);
          Result[i] := scale*exp(-x2/2)*(1-x2);
          x := x + xInc;
     end;

     // don't let accumulated rounding problems fail the last point
     x2 := sqr(ub);
     Result[numPts - 1] := scale*exp(-x2/2)*(1-x2);
end;


// ###########################################
// #### Gauss wavelet implementation according to gauswavf.m
type
  TGausDeriveFunc = function (x, f0 : double) : double;

function Gaus1Derrive(x, f0 : double) : double;
begin
     Result := -2*x*f0; 
end;
  
function Gaus2Derrive(x, f0 : double) : double;
begin
     Result := - 2/sqrt(3)*(-1 + 2*sqr(x))*f0;
end;

function Gaus3Derrive(x, f0 : double) : double;
begin
     Result := - 4/sqrt(15)*x*(3 - 2*sqr(x))*f0;
end;

function Gaus4Derrive(x, f0 : double) : double;
begin
     Result := 4/sqrt(105)*(3 - 12*sqr(x) + 4*sqr(sqr(x)))*f0;
end;
  
function TWavelet.Gauss(const lb, ub: double;
  numPts: integer): TDoubleDynArray;
var xInc : double;
    i : integer;
    x : double;
    scale : double;
    x2 : double;
    fDerriveFunc : TGausDeriveFunc;
begin
     scale := power(2/pi, 0.25);

     case waveletType  of
       wtGaus1: fDerriveFunc := Gaus1Derrive;
       wtGaus2: fDerriveFunc := Gaus2Derrive;
       wtGaus3: fDerriveFunc := Gaus3Derrive;
       wtGaus4: fDerriveFunc := Gaus4Derrive;
     else
       raise Exception.Create('Error unknow wavelet function');
     end;
     
     xInc := (ub - lb)/(numPts - 1);
     x := lb;
     SetLength(Result, numPts);
     for i := 0 to numPts - 2 do
     begin
          x2 := sqr(x);
          Result[i] := scale*exp(-x2);

          // derivatives according to wavelettype
          Result[i] := fDerriveFunc(x, Result[i]);          

          x := x + xInc;
     end;

     // don't let accumulated rounding fail the last point
     x2 := sqr(ub);
     Result[numPts - 1] := scale*exp(-x2);
     Result[Length(result) - 1] := fDerriveFunc(ub, Result[Length(result) - 1]); 
end;

function TWavelet.IntegratedWavCoef(scale: integer; var order : integer; var pMem : Pointer): PConstDoubleArr;
var psi1, psi2 : TDoubleDynArray;
    xInc : double;
    idx1 : integer;
    idx2 : integer;
begin
     // create a "full" (max len) wavelet (the integrated version!)
     WaveFunc(cConstMaxWaveletIter, psi1, psi2, True);

     // note the function assumes a step of 1 in the input signal but we have
     // to adjust that to the wavelet support range 
     
     xInc := 1/(scale*WvletStep(cConstMaxWaveletIter));

     // reduce to the wanted scale
     // -> 1= every 1024 sample
     // -> 2= every 2/1024 sample ...
     idx1 := 0;
     idx2 := 0;

     while idx2 < Length(psi1) do
     begin
          psi1[idx1] := psi1[idx2];

          inc(idx1);
          idx2 := floor(xInc*idx1);
     end;
     
     order := idx1;
     if (order and 1) = 1 then
     begin
          psi1[order] := 0;
          inc(order);
     end;
        
     // result needs to be aligned and order needs to be even!
     Result := MtxAllocAlign( order*sizeof(double), pMem );
     Move(psi1[0], Result^, order*sizeof(double));
end;

// returns the approximation of the wavelet function:
// returns the scaling and wavelet functions on the 2^ITER
// points grid XVAL. The positive integer ITER is the number
// of iterations. 
procedure TWavelet.WaveFunc(iter: integer; var psi1, psi2 : TDoubleDynArray; integrate : boolean);
var numPts : integer;
    coef : double;
    step : double;
    i: Integer;
    p1 : TDoubleDynArray;
    p2 : TDoubleDynArray;
    numDismiss : integer;
    mul : double;
    pmem : Pointer;
    revHi_R : PConstDoubleArr;
    revLo_D : PConstDoubleArr;
    nb : integer;
    lastPsi1, lastPsi2 : double;
    
function GetNbPts : integer;
var lplus : integer;
    kk : integer;
begin
     lplus := fwfilt.ncof - 2;
     Result := 1;
     for kk := 0 to iter - 1 do
         Result := 2*Result + lplus;
end;
begin
     // do that calculation only one time:
     if Length(fPsi1) > 0 then
     begin
          psi1 := Copy(fPsi1, 0, Length(fPsi1));
          psi2 := Copy(fPsi2, 0, Length(fPsi2));
          exit;
     end;

     // ###########################################
     // #### calcualte "finest" available wavelet
     nb := 0;
     coef := power(sqrt(2), iter);
     step := WvletStep(iter);
     mul := 1;

     if WaveletType = wtMexicanHat then
     begin
          // this one can only be calculated
          coef := 1;
          numPts := 1 shl iter;
          psi1 := MexicanHat(cConstMexHatLB, cConstMexHatUB, numPts);
          psi2 := Copy(psi1, 0, Length(psi1));
     end
     else if WaveletType in [wtGaus1, wtGaus2, wtGaus3, wtGaus4] then
     begin
          // this one can only be calculated
          coef := 1;
          numPts := 1 shl iter;
          psi1 := Gauss(cConstGausLB, cConstGausUB, numPts);
          psi2 := Copy(psi1, 0, Length(psi1));
     end
     else if IsOrthoWavelet then
     begin
          numPts := Round((fwfilt.ncof - 1)/step + 1);
          nb := GetNbPts;
          
          SetLength(p1, 1 + fwfilt.ncof);
          p1[fwfilt.ncof div 2] := 1;

          for i := 0 to iter - 1 do
          begin
               p1 := dyadUp(p1);

               if i = 0 
               then
                   psi1 := conv(p1, fwfilt.Hi_R, fwfilt.ncof)
               else
                   psi1 := conv(p1, fwfilt.LO_R, fwfilt.ncof);

               p1 := Copy(psi1, 0, Length(psi1));
          end;

          psi2 := Copy(psi1, 0, Length(psi1));
     end
     else
     begin
          if WaveletType in [wtBiortho22, wtBiortho31, wtBiortho39] then
             mul := -1;

          pMem := GetMemory(2*sizeof(double)*fwfilt.ncof + 32);
          try
             revHi_R := pMem;
             revHi_R := PConstDoubleArr(Cardinal(revHi_R) + 16 - (Cardinal(revHi_R) and $F));

             revLo_D := revHi_R;
             inc(PDouble(revLo_D), fwfilt.ncof);
             revLo_D := PConstDoubleArr(Cardinal(revLo_D) + 16 - (Cardinal(revLo_D) and $F));

             for i := 0 to fwfilt.ncof - 1 do
             begin
                  revHi_R[i] := fwfilt.Hi_D[fwfilt.ncof - 1 - i];
                  revLo_D[i] := fwfilt.Lo_D[fwfilt.ncof - 1 - i];
             end;
          
             // biorthogonal wavelet
             numPts := Round((fwfilt.ncof - 1)/step);   // resulting array length
             nb := GetNbPts; // how many points to take from the array (others are 0!)

             SetLength(p1, 1 + fwfilt.ncof);
             SetLength(p2, 1 + fwfilt.ncof);
             p1[fwfilt.ncof div 2] := 1;
             p2[fwfilt.ncof div 2] := 1;

             for i := 0 to iter - 1 do
             begin
                  psi1 := dyadUp(p1);

                  if i = 0 
                  then
                      psi1 := conv(psi1, @revHi_R[0], fwfilt.ncof)
                  else
                      psi1 := conv(psi1, @revLo_D[0], fwfilt.ncof);

                  p1 := Copy(psi1, 0, Length(psi1));


                  psi2 := dyadUp(p2);
                  if i = 0 
                  then
                      psi2 := conv(psi2, fwfilt.Hi_R, fwfilt.ncof)
                  else
                      psi2 := conv(psi2, fwfilt.Lo_R, fwfilt.ncof);
               
                  p2 := Copy(psi2, 0, Length(psi2));
             end;
          finally
                 FreeMem(pMem);
          end;
     end;

     // scale (and integrate) mother functions
     lastPsi1 := 0;
     lastPsi2 := 0;

     if not IsCalcWavelet then 
     begin
          numDismiss := (Length(psi1) - nb) div 2 - 1;
          psi1 := Copy(psi1, numDismiss, numPts);
          psi2 := Copy(psi2, numDismiss, numPts);
     end;
     coef := coef*mul;
     for i := 0 to Length(psi2) - 1 do
     begin
          psi1[i] := coef*psi1[i];
          psi2[i] := coef*psi2[i];

          // this part implements intwav.m
          if integrate then
          begin
               lastPsi1 := psi1[i] + lastPsi1;
               psi1[i] := step*lastPsi1;
               lastPsi2 := psi2[i] + lastPsi2;
               psi2[i] := step*lastPsi2;
          end;
     end;

     // store for future use (in different scales)
     // note: the mexican hat wavelet needs to be recalculated 
     if not IsCalcWavelet then 
     begin
          fpsi1 := Copy(Psi1, 0, Length(Psi1));
          fpsi2 := Copy(Psi2, 0, Length(Psi2));
     end;
end;

function TWavelet.WvletStep(iter : integer): double;
begin
     if WaveletType = wtMexicanHat 
     then
         Result := (cConstMexHatUB - cConstMexHatLB)/((1 shl iter) - 1) 
     else if WaveletType in [wtGaus1, wtGaus2, wtGaus3, wtGaus4]
     then
         Result := (cConstGausUB - cConstGausLB)/((1 shl iter) - 1)     
     else
         Result := 1/(1 shl iter);
end;

{$ENDREGION}

// ###########################################
// #### Public interface
// ###########################################

{$REGION 'Public interface - main decomposition routines'}

procedure TWavelet.DWT(const input: TDoubleDynArray; maxLevel: integer);
var k :integer;
begin
     CheckDWTSupport;

     if length(input) = 0 then
        exit;

     fDWTMode := wmFast;
     fInputVec := Input;
     fInputLen := Length(input);
        
     fmaxmaxlevel := Round(Ln(fInputLen/(fwfilt.ncof - 1))/Ln(2));
     fMaxLevel := Min(maxLevel, fMaxMaxlevel);
     if fmaxlevel = 0 then
        fmaxlevel := fmaxmaxlevel;

     ResetFilter;

     if Length(fdetc) <> fMaxLevel then
     begin
          SetLength(fdetc, fmaxlevel);
          SetLength(fappc, fmaxlevel);
     end;

     // ###########################################
     // #### decomposition
     InternalDWT(input, fdetc[0], fappc[0]);

     for k := 1 to fmaxlevel - 1 do
         InternalDWT(fappc[k - 1], fdetc[k], fappc[k]);
end;

procedure TWavelet.DyadicDWT(const input: TDoubleDynArray; maxLevel: integer);
var k :integer;
begin
     CheckDWTSupport;

     if length(input) = 0 then
        exit;

     fDWTMode := wmDyadic;
     fInputVec := Input;
     fInputLen := Length(input);

     fmaxmaxlevel := Round(Ln(fInputLen/(fwfilt.ncof - 1))/Ln(2));
     fMaxLevel := Min(maxLevel, fMaxMaxlevel);
     if fmaxlevel = 0 then
        fmaxlevel := fmaxmaxlevel;

     ResetFilter;

     if Length(fdetc) <> fMaxLevel then
     begin
          SetLength(fdetc, fmaxlevel);
          SetLength(fappc, fmaxlevel);
     end;

     // ###########################################
     // #### decomposition
     dwtDyadic(input, fdetc[0], fappc[0], 0);

     for k := 1 to fmaxlevel - 1 do
         dwtDyadic(fappc[k - 1], fdetc[k], fappc[k], k);
end;

procedure TWavelet.CWT(const input: TDoubleDynArray; levels: TIntegerDynArray);
var counter : integer;
    pMem : Pointer;
    coef : PConstDoubleArr;
    order : integer;
    scale : double;
    i : integer;
    help : double;
    lCoef : integer;
begin
     if Length(input) = 0 then
        exit;

     fMaxLevel := Length(levels);
     fDWTMode := wmContionus;
     fInputLen := Length(input); // used in beat detector

     SetLength(fdetc, Length(Levels));

     for counter := 0 to Length(Levels) - 1 do
     begin
          // matlab uses the integrated version
          // note: the function returns the aligned pointer to pMem and ensures an even order!
          // level is 0 based -> adjust by one
          coef := IntegratedWavCoef(levels[counter] + 1, order, pMem);

          // flip the direction:
          lCoef := order - 1;
          for i := 0 to lCoef div 2 do
          begin
               help := coef[i];
               coef[i] := coef[lCoef - i];
               coef[lCoef - i] := help;
          end;

          scale := -sqrt((levels[counter] + 1));
          fdetc[counter] := conv(input, @coef[0], order);

          // scale and differentiate (since we have an integrated mother wavelet)
          for i := 1 to Length(DetailVec[counter]) - 1 do
              fdetc[counter][i - 1] := (fdetc[counter][i] - fdetc[counter][i - 1])*scale;

          FreeMem(pMem);
     end;
end;

{$ENDREGION}

end.
