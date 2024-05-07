// ###################################################################
// #### This file is part of the mathematics library project, and is
// #### offered under the licence agreement described on
// #### http://www.mrsoft.org/
// ####
// #### Copyright:(c) 2017, Michael R. . All rights reserved.
// ####
// #### Unless required by applicable law or agreed to in writing, software
// #### distributed under the License is distributed on an "AS IS" BASIS,
// #### WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// #### See the License for the specific language governing permissions and
// #### limitations under the License.
// ###################################################################

unit Corr;

interface

uses SysUtils, Math, MatrixConst, Matrix, BaseMathPersistence, Types;

{$I 'mrMath_CPU.inc'}

// standard simple linear correlation method:
type
  ECorrelateException = class(EBaseMatrixException);
type
  TCorrelation = class(TMatrixClass)
  protected
    function InternalCorrelateArr( w1, w2 : PDouble; len : integer) : Double;
    function InternalCorrelate(w1, w2: IMatrix; const VarianceRatioThreshold: double = 0; canRaiseException : boolean = True): double;
    function InternalWeightedCorrelate( w1, w2 : IMatrix; weights : IMatrix ) : double;
  public
    function Correlate(x, y : IMatrix; const VarianceRatioThreshold: double = 0; canRaiseException : boolean = True) : double; overload; // correlation coefficient between t, r (must have the same length)
    function Correlate(x, y : IMatrix; var prob, z : double) : double; overload; // pearson correlation with Fishers's z and probobality 

    function CorrelateWeighted(x, y, w : IMatrix) : double; overload; // weighted correlation
    function CorrelateWeighted(x, y, w : IMatrix; var prob, z : double) : double; overload; // weighted correlation
    
    // spearman correlation. X, Y need to be vectors
    class function CorrelateSpearman( x, y : IMatrix; var zd, probd, rs, probrs : double ) : double;
    class function Covariance(x, y : IMatrix; Unbiased : boolean = True) : IMatrix; overload; // covariance matrix
    class function Covariance(A : IMatrix; Unbiased : boolean = True) : IMatrix; overload;   // covariance matrix of matrix A
  end;

// see: https://en.wikipedia.org/wiki/Dynamic_time_warping
// based on Timothy Felty's matlab script (google timothy felty dynamic time warping)
// -> enhanced with maximum search window
// -> enhanced with different distance methods
// Added the recursive fast dtw method based on
//    FastDTW: Toward Accurate Dynamic Time Warping in Linear Time and Space
type
  TDynamicTimeWarpDistMethod = (dtwSquared, dtwAbsolute, dtwSymKullbackLeibler);
  TDynamicTimeWarepReduceFunc = procedure (X : PConstDoubleArr; inLen, inOffset : NativeInt; out newLen, newOffset : NativeInt) of object;

  TDynamicTimeWarp = class(TCorrelation)
  private
    type
      TCoordRec = record
        i, j : integer;
      end;
      TDistRec = record
        next : TCoordRec;
        curr : TCoordRec;
        dist : double;
      end;
  private
    fd : TDoubleDynArray;
    fAccDist : TDoubleDynArray;
    fW1, fW2 : IMatrix;
    fW1Arr, fW2Arr : TDoubleDynArray;
    fNumW : integer;
    fMaxSearchWin : integer;
    fMethod : TDynamicTimeWarpDistMethod;

    fMemX : IMatrix;       // holds the aligned memory
    fMemY : IMatrix;

    fX : PConstDoubleArr;  // fast access to fMemx and fMemY (sse aligned)
    fY : PConstDoubleArr;

    fWindow : Array of TCoordRec; // pairs of x and y indices. Used in dtw and fastdtw
    fDistIdx : Array of TDistRec;
    fPath : Array of TCoordRec;    // i, j that build up the path
    fNumPath : Integer;

    fMaxPathLen : integer;
    fMaxWinLen : integer;

    fReduceByHalf : TDynamicTimeWarepReduceFunc;

    // fastdtw implementation based on the python package on: https://pypi.python.org/pypi/fastdtw
    // and: Stan Salvador, and Philip Chan. “FastDTW: Toward accurate dynamic time warping in linear time and space.”  Intelligent Data Analysis 11.5 (2007): 561-580.
    procedure ReduceByHalfPas(X : PConstDoubleArr; inLen, inOffset : NativeInt; out newLen, newOffset : NativeInt);
    {$IFNDEF MRMATH_NOASM}
    procedure ReduceByHalfSSE(X : PConstDoubleArr; inLen, inOffset : NativeInt; out newLen, newOffset : NativeInt);
    {$ENDIF}
    function ExpandWindow(inXLen, inYLen : integer; radius : integer) : Integer;

    procedure InternalFastDTW(inXOffset, inXLen, inYOffset, inYLen : integer; radius : integer; var dist : double);
    function InternalDTW(inXOffset, inXLen, inYOffset, inYLen : integer; window : integer) : double;
    procedure DictNewCoords(var i, j, MaxDictIdx: integer); {$IFNDEF FPC} {$IF CompilerVersion >= 17.0} inline; {$IFEND} {$ENDIF}
    function DictValue(i, j, MaxDictIdx: integer): double; {$IFNDEF FPC} {$IF CompilerVersion >= 17.0} inline; {$IFEND} {$ENDIF}
    function GetPathByIndex(index: integer): TCoordRec;
    procedure InitXY(x, y : IMatrix); overload;
    procedure InitXY(x, y : TDoubleDynArray); overload;
  public
    // setup
    class var UseSSE : boolean;

    property W1 : IMatrix read fW1;  // stores the last result (warped vector)
    property W2 : IMatrix read fW2;

    property W1Arr : TDoubleDynArray read fW1Arr;
    property W2Arr : TDoubleDynArray read fW2Arr;
    
    
    property Path[index : integer] : TCoordRec read GetPathByIndex;
    property PathLen : integer read fNumPath;

    property MaxPathLen : integer read fMaxPathLen;
    property MaxWinLen : integer read fMaxWinLen;

    function DTW(t, r : IMatrix; var dist : double; MaxSearchWin : integer = 0) : IMatrix; overload;
    function DTWCorr(t, r : IMatrix; MaxSearchWin : integer = 0) : double; overload;  // calculate the correlation coefficient between both warped vectors

    function FastDTW(x, y : IMatrix; var dist : double; Radius : integer = 1)  : IMatrix; overload; // applies fastdtw
    function FastDTWCorr(t, r : IMatrix; Radius : integer = 1) : double; overload;  // calculate the correlation coefficient between both warped vectors
    function FastDTWCorr(t, r : IMatrix; var dist : double; Radius : integer = 1) : double; overload;


    // same Methods but with dynamic arrays
    function DTW(t, r : TDoubleDynArray; var dist : double; MaxSearchWin : integer = 0) : TDoubleDynArray; overload;
    function DTWCorr(t, r : TDoubleDynArray; MaxSearchWin : integer = 0) : double; overload;

    function FastDTW(x, y : TDoubleDynArray; var dist : double; Radius : integer = 1)  : TDoubleDynArray; overload;
    function FastDTWCorr(t, r : TDoubleDynArray; Radius : integer = 1) : double; overload;
    function FastDTWCorr(t, r : TDoubleDynArray; var dist : double; Radius : integer = 1) : double; overload;

    constructor Create(DistMethod : TDynamicTimeWarpDistMethod = dtwSquared); // -> 0 = infinity
    destructor Destroy; override;
  end;


// spearman rank-order correlation coefficient
// sum squared rank difference as D, number of standard deviations by which D deviates fro mits nullnypothesis as zd, two sided
// significance level of this deviation as probd, spearmans rank correlation as rs, and the two -sided significance level of its
// deviation from zero as probrs.
procedure spearmanCorr( data1, data2 : PConstDoubleArr; n : integer; var d, zd, probd, rs, probrs : double);

implementation

uses MatrixASMStubSwitch, MathUtilFunc, Statistics;

const cLocDivBy2 : Array[0..1] of double = (0.5, 0.5);
      cTiny : double = 1e-20;

// ###########################################
// #### Correlation (base implementation)
// ###########################################

function TCorrelation.Correlate(x, y: IMatrix; var prob, z: double): double;
var df : integer;
    t : double;
begin
     Result := Correlate(x, y);
     df := (x.Width*x.Height - 2);
     t := Result * sqrt( df/( (1 - Result + cTiny)*(1 + Result + cTiny) ) );
     
     z := 0.5*ln( (1 + (Result) + cTiny)/(1 - Result + cTiny)); // Fishers z transformation
     prob := betaI( 0.5*df, 0.5, df/(df + sqr(t)) );            // probability
end;


// note: afterwards w1 and w2 are mean normalized and w2 width and height is changed!
function TCorrelation.Correlate(x, y: IMatrix; const VarianceRatioThreshold: double = 0; canRaiseException : boolean = True): double;
var w1, w2 : IMatrix;
begin
     w1 := x;
     if x.Height <> 1 then
        w1 := x.Reshape(x.Width*x.Height, 1);
     w2 := y;
     if y.Height <> 1 then
        w2 := y.Reshape(y.Width*y.Height, 1);

     assert(w1.Width = w2.Width, 'Dimension error');
     
     Result := InternalCorrelate(w1, w2, VarianceRatioThreshold, canRaiseException);
end;

class function TCorrelation.Covariance(x, y: IMatrix; Unbiased : boolean = True): IMatrix;
var xc, tmp : IMatrix;
begin
     if x.Width*x.Height <> y.Width*y.Height then
        raise Exception.Create('Error length of x and y must be the same');

     xc := TDoubleMatrixClass(x.GetObjRef.ClassType).Create(2, x.Width*x.Height);

     // build matrix with 2 columns
     if x.Width = 1 
     then
         tmp := x
     else
         tmp := x.Reshape(1, x.Width*x.Height, True);
     xc.SetColumn(0, tmp);

     if y.Width = 1 
     then
         tmp := y
     else
         tmp := y.Reshape(1, x.Width*x.Height, True);
     xc.SetColumn(1, tmp);
     
     Result := Covariance(xc, Unbiased);
end;

// each row is an observation, each column a variable
class function TCorrelation.Covariance(A: IMatrix; Unbiased: boolean): IMatrix;
var aMean : IMatrix;
    ac : IMatrix;
    tmp : IMatrix;
    m : Integer;
begin
     aMean := A.Mean(False);
     ac := A.SubVec( aMean, True );

     m := ac.Height; 
     tmp := ac.Transpose;
     ac := tmp.Mult(ac);
    
     if Unbiased then
        dec(m);

     if m > 0 then
        ac.ScaleInPlace(1/m);

     Result := ac;
end;

function TCorrelation.InternalCorrelate(w1, w2: IMatrix; const VarianceRatioThreshold: double = 0; canRaiseException : boolean = true): double;
var meanVar1 : TMeanVarRec;
    meanVar2 : TMeanVarRec;
begin
     // note the routine avoids memory allocations thus it runs on the raw optimized functions:
     
     // calc: 1/(n-1)/(var_w1*var_w2) sum_i=0_n (w1_i - mean_w1)*(w2_i - mean_w2)
     MatrixMeanVar( @meanVar1, 2*sizeof(double), w1.StartElement, w1.LineWidth, w1.Width, 1, True, True);
     MatrixMeanVar( @meanVar2, 2*sizeof(double), w2.StartElement, w2.LineWidth, w2.Width, 1, True, True);

     // Optional: If signal has a MUCH smaller (eg: factor 3) variance
     //   than the reference signal -> don't scale it up
     if (VarianceRatioThreshold <> 0) and (meanVar2.aVar*VarianceRatioThreshold < meanVar1.aVar) then
        meanVar2.aVar := meanVar1.aVar;

     // For norming the variance may not be zero
     if SameValue(meanVar1.aVar, 0, cDefEpsilon) or SameValue(meanVar2.aVar, 0, cDefEpsilon) then
     begin
          if canRaiseException
          then
              raise ECorrelateException.Create('Correlation Error: variance of an input vector is zero')
          else
          begin
               Result := 0;
               exit;
          end;

     end;

     w1.AddInPlace( -meanVar1.aMean );     
     w2.AddInPlace( -meanVar2.aMean );

     // dot product:
     MatrixMult( @Result, sizeof(double), w1.StartElement, w2.StartElement, w1.Width, w1.Height, w2.Height, w2.Width, w1.LineWidth, sizeof(double) );
     Result := Result/sqrt(meanVar1.aVar*meanVar2.aVar)/(w1.Width - 1);
end;

function TCorrelation.InternalCorrelateArr(w1, w2: PDouble;
  len: integer): Double;
var meanVar1 : TMeanVarRec;
    meanVar2 : TMeanVarRec;
begin
     // calc: 1/(n-1)/(var_w1*var_w2) sum_i=0_n (w1_i - mean_w1)*(w2_i - mean_w2)
     MatrixMeanVar( @meanVar1, 2*sizeof(double), w1, len*sizeof(double), len, 1, True, True);
     MatrixMeanVar( @meanVar2, 2*sizeof(double), w2, len*sizeof(double), len, 1, True, True);

     MatrixAddAndScale( w1, len*sizeof(double), len, 1, -meanVar1.aMean, 1 );
     MatrixAddAndScale( w2, len*sizeof(double), len, 1, -meanVar2.aMean, 1 );
     
     // dot product:
     MatrixMult( @Result, sizeof(double), w1, w2, len, 1, 1, len, len*sizeof(double), sizeof(double));
     Result := Result/sqrt(meanVar1.aVar*meanVar2.aVar)/(len - 1);
end;

function TCorrelation.InternalWeightedCorrelate(w1, w2,
  weights: IMatrix): double;
var sumweights : double;
    wMean1, wMean2 : double;
    weightedW1, weightedW2 : IMatrix;
    t : IMatrix;
    ww : IMatrix;
    sxx, syy : double;
begin
     // calculated correlation according to
     // r = S_xy/sqrt(S_xx*S_yy)
     // where S_xy = sum_i=0_n weights[i]*( x[i] - M(x, w) )*( y[i] - M(y, w))
     // S_xx = sum_i=0_n weights[i]*( x[i] - M(x, w) )^2
     // S_yy = sum_i=0_n weights[i]*( y[i] - M(y, w) )^2
     // M(x, w) = sum_i=0_n w[i]*x[i] / sum w
     // M(y, w) = sum_i=0_n w[i]*y[i] / sum w

     // normalize weighted mean
     MatrixSum( @sumweights, sizeof(double), weights.StartElement, weights.LineWidth, weights.Width, 1, True );
     if SameValue(sumweights, 0, eps(1)) then
        raise ECorrelateException.Create('Weighting vector sums to zero');
     ww := weights;
     if not sameValue(sumweights, 1, eps( 1 ) ) then
        ww := weights.Scale( 1/sumweights );

     if sumweights > cDefEpsilon then
     begin
          MatrixMtxVecMult( @wMean1, sizeof(double), w1.StartElement, ww.StartElement, w1.LineWidth, sizeof(double), w1.Width, 1, 1, 0 );
          MatrixMtxVecMult( @wMean2, sizeof(double), w2.StartElement, ww.StartElement, w2.LineWidth, sizeof(double), w2.Width, 1, 1, 0 );
          
          weightedW1 := w1.ScaleAndAdd(-wMean1, 1);
          weightedW2 := w2.ScaleAndAdd(-wMean2, 1);

          t := weightedW1.ElementWiseMult(weightedw2);
          MatrixMtxVecMult( @Result, sizeof(double), t.StartElement, ww.StartElement, t.LineWidth, sizeof(double), t.Width, 1, 1, 0);

          weightedw1.ElementWiseMultInPlace(weightedw1);
          weightedw2.ElementWiseMultInPlace(weightedw2);
          MatrixMtxVecMult( @sxx, sizeof(double), weightedW1.StartElement, ww.StartElement, weightedW1.LineWidth, sizeof(double), weightedW1.Width, 1, 1, 0);
          MatrixMtxVecMult( @syy, sizeof(double), weightedW2.StartElement, ww.StartElement, weightedW2.LineWidth, sizeof(double), weightedW2.Width, 1, 1, 0);

          sxx := sqrt(sxx*syy);
          if sxx < eps(Result) then
             raise ECorrelateException.Create('Variances close too machine precission - out of bounds');
          
          Result := Result/sxx;
     end
     else
         raise ECorrelateException.Create('Sum of weights <= 0');
end;

// ###########################################
// #### Dynamic time warping
// ###########################################

{ TDynamicTimeWarp }

constructor TDynamicTimeWarp.Create(DistMethod : TDynamicTimeWarpDistMethod = dtwSquared);
begin
     fMethod := DistMethod;

     {$IFNDEF MRMATH_NOASM}
     if UseSSE
     then
         fReduceByHalf := {$IFDEF FPC}@{$ENDIF}ReduceByHalfSSE
     else
     {$ENDIF}
         fReduceByHalf := {$IFDEF FPC}@{$ENDIF}ReduceByHalfPas;

     inherited Create;
end;

// ###########################################
// #### Base DTW algorithm
// ###########################################

function TDynamicTimeWarp.DTW(t, r: IMatrix; var dist: double; MaxSearchWin : integer = 0): IMatrix;
begin
     DTW( t.SubMatrix, r.SubMatrix, dist, MaxSearchWin);

     fW1 := TDoubleMatrix.Create( Copy( fW1Arr, 0, fNumW ), fNumW, 1);
     fW2 := TDoubleMatrix.Create( Copy( fW2Arr, 0, fNumW ), fNumW, 1);
     
     Result := fw1;
end;

function TDynamicTimeWarp.DTWCorr(t, r: IMatrix; MaxSearchWin : integer = 0): double;
var dist : double;
begin
     // ###########################################
     // #### Create time warping vectors -> stored in fw1, fw2
     DTW(t, r, dist, MaxSearchWin);
     
     // ###########################################
     // #### Calculate correlation
     Result := InternalCorrelate(fw1, fw2);
end;

// ###########################################
// #### Fast DTW
// ###########################################

function TDynamicTimeWarp.FastDTW(x, y: IMatrix; var dist: double; Radius : integer = 1): IMatrix;
var counter: Integer;
begin
     dist := 0;

     radius := Max(1, radius);

     // ###########################################
     // #### Preparation
     fMaxPathLen := 0;
     fMaxWinLen := 0;

     InitXY(x, y);
     
     // prepare memory
     if Length(fWindow) < Max(x.Width, y.Width)*2 then
     begin
          SetLength(fWindow, (radius*4 + 4)*Max(x.Width, y.Width));
          SetLength(fPath, 3*Max(x.Width, y.Width));
          SetLength(fDistIdx, Length(fWindow));
     end;
     
     // ###########################################
     // #### Find optimal path
     fNumPath := 0;
     InternalFastDTW(0, x.Width, 0, y.Width, Max(1, Radius), dist);


     // ###########################################
     // #### Build result
     if not Assigned(fW1) then
     begin
          fW1 := MatrixClass.Create(fNumPath, 1);
          fW2 := MatrixClass.Create(fNumPath, 1);
     end;
     fW1.UseFullMatrix;
     fW2.UseFullMatrix;

     if fW1.Width < fNumPath then
     begin
          fW1.SetWidthHeight(fNumPath, 1);
          fW2.SetWidthHeight(fNumPath, 1);
     end;
     
     fW1.SetSubMatrix(0, 0, fNumPath, 1);
     fW2.SetSubMatrix(0, 0, fNumPath, 1);
     
     for counter := 0 to fNumPath - 1 do
     begin
          fW1.Vec[counter] := fX^[fPath[counter].i];
          fW2.Vec[counter] := fY^[fPath[counter].j];
     end;

     Result := FW1;
end;

function TDynamicTimeWarp.FastDTW(x, y: TDoubleDynArray; var dist: double;
  Radius: integer): TDoubleDynArray;
var counter: Integer;
begin
     dist := 0;

     radius := Max(1, radius);

     // ###########################################
     // #### Preparation
     fMaxPathLen := 0;
     fMaxWinLen := 0;
     

     InitXY(x, y);
     
     // prepare memory
     if Length(fWindow) < Max(Length(x), Length(y))*2 then
     begin
          SetLength(fWindow, (radius*4 + 4)*Max(Length(x), Length(y)));
          SetLength(fPath, 3*Max(Length(x), Length(y)));
          SetLength(fDistIdx, Length(fWindow));
     end;
     
     // ###########################################
     // #### Find optimal path
     fNumPath := 0;
     InternalFastDTW(0, Length(x), 0, Length(y), Max(1, Radius), dist);


     // ###########################################
     // #### Build result
     if not Assigned(fW1) then
     begin
          fW1 := TDoubleMatrix.Create(fNumPath, 1);
          fW2 := TDoubleMatrix.Create(fNumPath, 1);
     end;
     fW1.UseFullMatrix;
     fW2.UseFullMatrix;

     if fW1.Width < fNumPath then
     begin
          fW1.SetWidthHeight(fNumPath, 1);
          fW2.SetWidthHeight(fNumPath, 1);
     end;
     
     fW1.SetSubMatrix(0, 0, fNumPath, 1);
     fW2.SetSubMatrix(0, 0, fNumPath, 1);
     
     for counter := 0 to fNumPath - 1 do
     begin
          fW1.Vec[counter] := fX^[fPath[counter].i];
          fW2.Vec[counter] := fY^[fPath[counter].j];
     end;

     Result := FW1.SubMatrix;
end;

function TDynamicTimeWarp.FastDTWCorr(t, r: IMatrix; var dist: double;
  Radius: integer): double;
begin
     // ###########################################
     // #### Create time warping vectors -> stored in fw1, fw2
     FastDTW(t, r, dist, radius);

     // ###########################################
     // #### Calculate correlation
     Result := InternalCorrelate(fw1, fw2);
end;

function TDynamicTimeWarp.FastDTWCorr(t, r: TDoubleDynArray;
  Radius: integer): double;
var dist : double;
begin
     Result := FastDTWCorr(t, r, dist, radius);
end;

function TDynamicTimeWarp.FastDTWCorr(t, r: TDoubleDynArray; var dist: double;
  Radius: integer): double;
begin
     // ###########################################
     // #### Create time warping vectors -> stored in fw1, fw2
     FastDTW(t, r, dist, radius);

     // ###########################################
     // #### Calculate correlation
     Result := InternalCorrelate(fw1, fw2);
end;

function TDynamicTimeWarp.GetPathByIndex(index: integer): TCoordRec;
begin
     Result := fPath[index];
end;

function TDynamicTimeWarp.FastDTWCorr(t, r: IMatrix; Radius : integer = 1): double;
var dist : double;
begin
     // ###########################################
     // #### Create time warping vectors -> stored in fw1, fw2
     FastDTW(t, r, dist, radius);

     // ###########################################
     // #### Calculate correlation
     Result := InternalCorrelate(fw1, fw2);
end;

// ###########################################
// #### path functions
// ###########################################

function TDynamicTimeWarp.DictValue(i, j : integer; MaxDictIdx : integer) : double; 
var cnt : integer;
begin
     Result := MaxDouble;

     for cnt := MaxDictIDx - 1 downto 0 do
     begin
          if (i = fDistIdx[cnt].curr.i) and (j = fDistIdx[cnt].curr.j) then
          begin
               Result := fDistIdx[cnt].dist;
               break;
          end;
     end;
end;

destructor TDynamicTimeWarp.Destroy;
begin
     fW1 := nil;
     fW2 := nil;
     fAccDist := nil;
     fW1Arr := nil;
     fW2Arr := nil;
     
     inherited;
end;

procedure TDynamicTimeWarp.DictNewCoords(var i, j : integer; var MaxDictIdx : integer); 
begin
     dec(MaxDictIdx);
     while (MaxDictIdx >= 0) and ((fDistIdx[MaxDictIdx].curr.i <> i) or (fDistIdx[maxDictIdx].curr.j <> j)) do
           dec(MaxDictIdx);

     if MaxDictIdx >= 0 then
     begin
          i := fDistIdx[MaxDictIdx].next.i;
          j := fDistIdx[MaxDictIdx].next.j;
     end;
end;

// ###########################################
// #### private fast dtw functions
// ###########################################

function TDynamicTimeWarp.InternalDTW(inXOffset, inXLen, inYOffset, inYLen : integer; window : integer) : double;
var i, j : Integer;
    cnt : integer;
    dt : double;
    dIdx : integer;
    dist0, dist1, dist2 : double;
    tmp : TCoordRec;
begin
     // perform a full inXLen*inYLen coordinate space
     if window = 0 then
     begin
          if Length(fWindow) < inXLen*inYLen then
             SetLength(fWindow, inXLen*inYLen);
          
          for i := 1 to inXLen do
          begin
               for j := 1 to inYLen do
               begin
                    fWindow[window].i := i;
                    fWindow[window].j := j;
                    inc(window);
               end;
          end;
     end;

     fMaxWinLen := Max(fMaxWinLen, window);

     if Length( fDistIdx ) < (inXLen*inYLen) then
        SetLength( fDistIdx, inXLen*inYLen );

     // ###########################################
     // #### Prepare a path list through the given coordinate list
     // first we take a forward step through a given set of coordinates
     // and then search back for the shortest path
     fDistIdx[0].dist := 0;
     fDistIdx[0].next.i  := 0;
     fDistIdx[0].next.j := 0;
     fDistIdx[0].curr.i := 0;
     fDistIdx[0].curr.j := 0;
     
     dIdx := 1;
     
     for cnt := 0 to window - 1 do
     begin
          i := fWindow[cnt].i;
          j := fWindow[cnt].j;

          case fMethod of
            dtwSquared: dt := sqr( fX^[inXOffset + i - 1] - fY^[inYOffset + j - 1]);
            dtwAbsolute: dt := abs( fX^[inXOffset + i - 1] - fY^[inYOffset + j - 1] );
            dtwSymKullbackLeibler: dt := fX^[inXOffset + i - 1] - fY^[inYOffset + j - 1]*(ln(fX^[inXOffset + i - 1]) - ln(fY^[inYOffset + j - 1]));
          else
              dt := abs( fX^[inXOffset + i - 1] - fY^[inYOffset + j - 1] );
          end;

          dist0 := DictValue(i - 1, j, dIdx);
          dist1 := DictValue(i, j - 1, dIdx);
          dist2 := DictValue(i - 1, j - 1, dIdx);

          if (dist0 = cMaxDouble) and (dist1 = cMaxDouble) and (dist2 = cMaxDouble) then
             continue;
          
          fDistIdx[dIdx].curr.i := i;
          fDistIdx[dIdx].curr.j := j;
          
          // according to the distance measure store the path coordinates for the next step
          if dist2 <= Min(dist1, dist0) then
          begin
               fDistIdx[dIdx].dist := dist2 + dt;
               fDistIdx[dIdx].next.i := i - 1;
               fDistIdx[dIdx].next.j := j - 1;
          end 
          else if dist0 < Min(dist1, dist2) then
          begin
               fDistIdx[dIdx].dist := dist0 + dt;
               fDistIdx[dIdx].next.i := i - 1;
               fDistIdx[dIdx].next.j := j;
          end
          else 
          begin
               fDistIdx[dIdx].dist := dist1 + dt;
               fDistIdx[dIdx].next.i := i;
               fDistIdx[dIdx].next.j := j - 1;
          end;
          inc(dIdx);
     end;
      
     // ###########################################
     // #### Build path (backwards)
     Result := fDistIdx[dIdx - 1].dist;
     i := inXLen;  
     j := inYLen;
     fNumPath := 0;
     while ( (i > 0) and (j > 0) ) and (dIdx >= 0) do
     begin
          fPath[fNumPath].i := i - 1;
          fPath[fNumPath].j := j - 1;
          inc(fNumPath);
          
          DictNewCoords(i, j, dIdx);
     end;

     // reverse path
     i := 0;
     j := fNumPath - 1;
     while i < j do
     begin
          tmp := fPath[i];
          fPath[i] := fPath[j];
          fPath[j] := tmp; 

          inc(i);
          dec(j);
     end;

     fMaxPathLen := Max(fMaxPathLen, fNumPath);
end;

procedure TDynamicTimeWarp.InitXY(x, y: IMatrix);
begin
     if not Assigned(fMemX) or (fMemX.Width < 2* (x.Width + x.Width and 1)) then
     begin
          fMemx := MatrixClass.Create( 2* (x.Width + x.Width and 1), 1);
          fX := PConstDoubleArr(fMemX.StartElement);
     end;
     Move(x.StartElement^, fx^[0], sizeof(double)*x.Width);


     if not Assigned(fMemY) or (fMemY.Width < 2* (y.Width + y.Width and 1)) then
     begin
          fMemY := MatrixClass.Create( 2* (y.Width + y.Width and 1), 1);
          fY := PConstDoubleArr(fMemY.StartElement);
     end;
     Move(y.StartElement^, fy^[0], sizeof(double)*y.Width); 
end;

procedure TDynamicTimeWarp.InitXY(x, y: TDoubleDynArray);
var lenX, lenY : integer;
begin
     lenX := Length(x);
     lenY := Length(y);
     
     if not Assigned(fMemX) or (fMemX.Width < 2* (lenX + lenX and 1)) then
     begin
          fMemx := MatrixClass.Create( 2* (lenX + lenX and 1), 1);
          fX := PConstDoubleArr(fMemX.StartElement);
     end;
     Move(x[0], fx^[0], sizeof(double)*lenX);


     if not Assigned(fMemY) or (fMemY.Width < 2* (lenY + lenY and 1)) then
     begin
          fMemY := MatrixClass.Create( 2* (lenY + lenY and 1), 1);
          fY := PConstDoubleArr(fMemY.StartElement);
     end;
     Move(y[0], fy^[0], sizeof(double)*lenY); 
end;

procedure TDynamicTimeWarp.InternalFastDTW(inXOffset, inXLen, inYOffset, inYLen : integer; radius : integer; var dist : double);
var minTimeSize : Integer;
    newXOffset, newXLen : NativeInt;
    newYOffset, newYLen : NativeInt;
    windowCnt : integer;
begin
     minTimeSize := radius + 2;

     // check for break condition
     if (inXLen < minTimeSize) or (inYLen < minTimeSize) then
     begin
          dist := InternalDTW(inXOffset, inXLen, inYOffset, inYLen, 0);
          exit;
     end;

     // reduce by half recursively
     fReduceByHalf(fX, inXLen, inXOffset, newXLen, newXOffset);
     fReduceByHalf(fY, inYLen, inYOffset, newYLen, newYOffset);
     InternalFastDTW(newXOffset, newXLen, newYOffset, newYLen, radius, dist);

     // rebuild
     windowCnt := ExpandWindow(inXLen, inYLen, radius);
     dist := InternalDTW(inXOffset, inXLen, inYOffset, inYLen, windowCnt);
end;

procedure TDynamicTimeWarp.ReduceByHalfPas(X : PConstDoubleArr; inLen, inOffset : NativeInt; out newLen, newOffset : NativeInt);
var counter: NativeInt;
    idx : NativeInt;
begin
     newOffset := inOffset + inLen;
     newLen := inLen div 2;
     idx := inOffset;
     for counter := newOffset to newOffset + newLen - 1 do
     begin
          X^[counter] := 0.5*(x^[idx] + X^[idx + 1]);
          inc(idx, 2);
     end;
end;

{$IFNDEF MRMATH_NOASM}

{$IFDEF x64}
// 64bit version
procedure TDynamicTimeWarp.ReduceByHalfSSE(X : PConstDoubleArr; inLen,
  inOffset: NativeInt; out newLen, newOffset: NativeInt);
{$IFDEF FPC}
begin
{$ENDIF}
asm
{$IFDEF UNIX}
   // Linux uses a diffrent ABI -> copy over the registers so they meet with winABI
   // (note that the 5th and 6th parameter are are on the stack)
   // The parameters are passed in the following order:
   // RDI, RSI, RDX, RCX -> mov to RCX, RDX, R8, R9
   mov r8, rdx;
   mov r9, rcx;
   mov rcx, rdi;
   mov rdx, rsi;
{$ENDIF}

   // rcx self, rdx X, r8 inLen, r9 = inOffset

   // newOffset := inOffset + inLen;
   // newOffset := newOffset + newOffset and 1;
   mov r10, newOffset;
   mov r11, r8;
   add r11, r9;
   inc r11;       // make sure the last bit is 0
   and r11, $FFFFFFFFFFFFFFFE; // clear the lowest bit (ensure SSE alignment)
   mov [r10], r11;

   // newLen := inLen div 2;
   shr r8, 1;
   mov r10, newLen;
   mov [r10], r8;

   movupd xmm3, [rip + cLocDivBy2];

   // test if we have enough elements to handle 2 elements at once
   cmp r8, 2;
   jl @lastElem;

   @loop1:
      // loop handles two elements in one step
      movapd xmm0, [rdx + 8*r9];
      movapd xmm1, [rdx + 8*r9 + 16];

      // perform xmm0 = 0.5( [rcx + 8] + [rcx + 16]), 0.5*([rcx + 16] + [rcx + 32])
      haddpd xmm0, xmm1;
      mulpd xmm0, xmm3;

      // write back two element
      movapd [rdx + 8*r11], xmm0;

      add r11, 2;
      add r9, 4;
   sub r8, 2;
   jg @loop1;

   test r8, r8;
   je @exit;

   @lastElem:

   // last element
   movsd xmm0, [rcx + 8*r9];
   movsd xmm1, [rcx + 8*r9 + 8];
   addsd xmm0, xmm1;
   mulsd xmm0, xmm3;

   movsd [rdx + 8*r11], xmm0;

   @exit:
end;
{$IFDEF FPC}
end;
{$ENDIF}

{$ELSE}

// 32 bit version
procedure TDynamicTimeWarp.ReduceByHalfSSE(X : PConstDoubleArr; inLen,
  inOffset: NativeInt; out newLen, newOffset: NativeInt);
{$IFDEF FPC}
begin
{$ENDIF}
asm
   // edx: X, ecx : inlen
   push ebx;

   // newOffset := inOffset + inLen;
   mov ebx, newOffset;
   mov eax, inOffset;
   //add eax, inLen;
   add eax, ecx;
   inc eax;       // make sure the last bit is 0
   and eax, $FFFFFFFE; // clear the lowest bit (ensure SSE alignment)
   mov [ebx], eax;

   // newLen := inLen div 2;
   //mov ecx, inLen;
   shr ecx, 1;
   mov ebx, newLen;
   mov [ebx], ecx;

   movupd xmm3, cLocDivBy2;

   // eax := counter for x[idx]
   // edx := counter for x[counter];
   //mov edx, X;
   mov ebx, inOffset;

   // test if we have enough elements to handle 2 elements at once
   cmp ecx, 2;
   jl @lastElem;

   @loop1:
      // loop handles two elements in one step
      movapd xmm0, [edx + 8*ebx];
      movapd xmm1, [edx + 8*ebx + 16];

      // perform xmm0 = 0.5( [edx + 8] + [edx + 16]), 0.5*([edx + 16] + [edx + 32])
      haddpd xmm0, xmm1;
      mulpd xmm0, xmm3;

      // write back two element
      movapd [edx + 8*eax], xmm0;

      add eax, 2;
      add ebx, 4;
   sub ecx, 2;
   jg @loop1;

   test ecx, ecx;
   je @exit;

   @lastElem:

   // last element
   movsd xmm0, [edx + 8*ebx];
   movsd xmm1, [edx + 8*ebx + 8];
   addsd xmm0, xmm1;
   mulsd xmm0, xmm3;

   movsd [edx + 8*eax], xmm0;

   @exit:

   pop ebx;
end;
{$IFDEF FPC}
end;
{$ENDIF}

{$ENDIF}
{$ENDIF}

function TDynamicTimeWarp.ExpandWindow(inXLen, inYLen, radius: integer): Integer;
var cnt : integer;
    baseI, baseJ : integer;
    minJ, maxJ : integer;
    pathCnt : integer;
    prevRadiusPathIdx : integer;
    nextRadiusPathIdx : integer;
    i, j : integer;
    base : integer;
    numItems : integer;
begin
     Result := 0;
     
     prevRadiusPathIdx := 0;
     nextRadiusPathIdx := 0;
     base := fNumPath - 1;
     
     // handle last element
     for cnt := 1 to radius do
     begin
          fPath[fNumPath].i := fPath[base].i + cnt;
          fPath[fNumPath].j := fPath[base].j;
          inc(fNumPath);
     end;

     baseI := -1;
     
     // build up new window from the previous path
     for cnt := 0 to fNumPath - 1 do
     begin
          // check if we already caught that particular i
          if fPath[cnt].i = baseI then
             continue;

          baseI := fPath[cnt].i;
          baseJ := fPath[cnt].j;

          // find the min max in the given radius
          minJ := Max(0, baseJ - radius);
          maxJ := baseJ + radius;
          
          // find previous 
          while (fPath[prevRadiusPathIdx].i < baseI - radius) do
                inc(prevRadiusPathIdx);
          // find next
          while (nextRadiusPathIdx < fNumPath - 1) and (fPath[nextRadiusPathIdx].i <= baseI + radius) do
                inc(nextRadiusPathIdx);
          // find boundaries
          for pathCnt := prevRadiusPathIdx to nextRadiusPathIdx do
          begin
               minj := Max(0, Min( fPath[pathCnt].j - radius, minJ) );
               maxj := Max( fPath[pathCnt].j + radius, maxJ);
          end;

          // add to window list
          for i := 2*basei to 2*basei + 1 do
          begin
               numItems := Result + 2*(maxJ - minJ + 1);
               if Length(fWindow) < numItems then
                  SetLength(fWindow, Max(numItems, Min(2*Length(fWindow), Length(fWindow) + 1000)));
          
               for j := 2*minJ to 2*maxJ do
               begin
                    fWindow[Result].i := i + 1;        // per convention we add one
                    fWindow[Result].j := j + 1;
                    inc(Result);
               end;
          end;

          // remove the ones that are too much
          while (Result > 0) and ( (fWindow[Result - 1].i > inXLen) or (fWindow[Result - 1].j > inYLen) ) do
                dec(Result);
     end;
end;

function TDynamicTimeWarp.DTW(t, r: TDoubleDynArray; var dist: double;
  MaxSearchWin: integer): TDoubleDynArray;
var n, m : integer;
    counter: Integer;
    w, h : integer;
    startIdx, endIdx : integer;
begin
     fMaxSearchWin := MaxSearchWin;
     
     // ###########################################
     // #### Prepare memory
     if not Assigned(fd) or (Length(fd) <> Length(t)*Length(r)) then
     begin
          SetLength(fd, Length(t)*Length(r));
          SetLength(fAccDist, Length(t)*Length(r));
          SetLength(fWindow, 2*Max(Length(r), Length(t)));
          SetLength(fPath, Length(fWindow));
          SetLength(fW1Arr, Length(fWindow));
          SetLength(fW2Arr, Length(fWindow));
     end;
     fNumW := 0;

     if fMaxSearchWin = 0 then
        fMaxSearchWin := Max(Length(t), Length(r));
     w := Length(t);
     h := Length(r);
     

     // ###########################################
     // #### prepare distance matrix
     counter := 0;
     for m := 0 to h - 1 do // fd.Height - 1 do
     begin
          for n := 0 to w - 1 do // fd.Width - 1 do
          begin
               //if (fMaxSearchWin <= 0) or ( abs(n - m) <= fMaxSearchWin) then
               begin
                    case fMethod of
                      dtwSquared: fd[counter] := sqr( t[n] - r[m] );
                      dtwAbsolute: fd[counter] := abs( t[n] - r[m] );
                      dtwSymKullbackLeibler: fd[counter] := (t[n] - r[m])*(ln(t[n]) - ln(r[m]));
                    end;
               end;
               inc(counter);
          end;
     end;

     for n := 0 to Length(fAccDist) - 1 do
         fAccDist[n] := 0;

     //fAccDist.SetValue(0);
     fAccDist[0] := fd[0];
     //fAccDist[0, 0] := fd[0, 0];

     for n := 1 to w - 1 do // fd.Width - 1 do
         fAccDist[n] := fd[n] + fAccDist[n - 1];
         //fAccDist[n, 0] := fd[n, 0] + fAccDist[n - 1, 0];

     for m := 1 to h - 1 do // fd.Height - 1 do
         //fAccDist[0, m] := fd[0, m] + fAccDist[0, m - 1];
         fAccDist[m*w] := fd[m*w] + fAccDist[(m - 1)*w];

     for n := 1 to h - 1 do // fd.Height - 1 do
     begin
          startIdx := Max(1, n - fMaxSearchWin);
          endIdx := Min(n + fMaxSearchWin,  w - 1);
          if startIdx > 1 then
             fAccDist[startIdx + n*w - 1] := MaxDouble;
          if endIdx < w - 1 then
             fAccDist[endIdx + 1 + n*w] := MaxDouble;
          for m := Max(1, n - fMaxSearchWin) to Min(n + fMaxSearchWin,  w - 1) do 
          //for m := 1 to w - 1 do 
              fAccDist[m + n*w] := fD[m + n*w] + min( fAccDist[ m + (n - 1)*w ], min( fAccDist[(m - 1) + (n - 1)*w], fAccDist[ (m - 1) + n*w] ));
              //fAccDist[m, n] := fD[m, n] + min( fAccDist[ m, n - 1 ], min( fAccDist[m - 1, n - 1], fAccDist[ m - 1, n] ));
     end;

     //dist := fAccDist[fd.Width - 1, fd.Height - 1];
     dist := fAccDist[w - 1 + (h-1)*w];

     fNumW := 0;
     m := Length(t) - 1;
     n := Length(r) - 1;
     fWindow[fNumW].i := m;
     fWindow[fNumW].j := n;
     inc(fNumW);
     
     while (n + m) > 1 do
     begin
          if n - 1 <= 0 
          then
              dec(m)
          else if m - 1 <= 0 
          then
              dec(n)
          else
          begin
               //if fAccDist[m - 1, n - 1] < Min(fAccDist[m, n - 1], fAccDist[m - 1, n]) then
               if fAccDist[(m - 1) + (n - 1)*w] < Min(fAccDist[m + (n - 1)*w], fAccDist[(m - 1) + n*w]) then
               begin
                    dec(n);
                    dec(m);
               end
               else 
               begin
                    if fAccDist[m + (n - 1)*w] < fAccDist[(m - 1) + n*w] then
                    begin
                         dec(n);
                         if m - n > fMaxSearchWin then
                            dec(m);
                    end
                    else
                    begin
                         dec(m);

                         if n - m > fMaxSearchWin then
                            dec(n);
                    end;
               end;
          end;

          fWindow[fNumW].i := m;
          fWindow[fNumW].j := n;

          inc(fNumW);
     end;

     // ###########################################
     // #### Build final warped vector
     //fw1.SetSubMatrix(0, 0, fNumW, 1);
     //fw2.SetSubMatrix(0, 0, fNumW, 1);
     for counter := 0 to fNumW - 1 do
     begin
          fW1Arr[counter] := r[ fWindow[fNumW - 1 - counter].j ];
          fW2Arr[counter] := t[ fWindow[fNumW - 1 - counter].i ]; 
          
          fPath[counter] := fWindow[fNumW - 1 - counter];
          
          //fw1.Vec[counter] := r[ fWindow[fNumW - 1 - counter].j ];
          //fw2.Vec[counter] := t[ fWindow[fNumW - 1 - counter].i ]; 
     end;

     Result := fw1Arr; // fW1.SubMatrix;
end;

function TDynamicTimeWarp.DTWCorr(t, r: TDoubleDynArray;
  MaxSearchWin: integer): double;
var dist : double;
begin
     // ###########################################
     // #### Create time warping vectors -> stored in fw1, fw2
     DTW(t, r, dist, MaxSearchWin);
     
     // ###########################################
     // #### Calculate correlation
     Result := InternalCorrelateArr(@fw1Arr[0], @fw2Arr[0], fNumW);
end;

class function TCorrelation.CorrelateSpearman(x, y: IMatrix; var zd, probd, rs,
  probrs: double): double;
var x1, x2 : TDoubleDynArray;
begin
     x1 := x.SubMatrix;
     x2 := y.SubMatrix;

     assert(Length(x1) = Length(x2), 'Error unequal length in x, y');

     Result := 0;
     spearmanCorr( PConstDoubleArr( @x1[0] ), PConstDoubleArr( @x2[0] ),
                   Length(x1), Result, zd, probd, rs, probrs);
end;

function TCorrelation.CorrelateWeighted(x, y, w: IMatrix; var prob,
  z: double): double;
var df : integer;
    t : double;
begin
     Result := CorrelateWeighted(x, y, w);
     df := (x.Width*x.Height - 2);
     t := Result * sqrt( df/( (1 - Result + cTiny)*(1 + Result + cTiny) ) );
     
     z := 0.5*ln( (1 + (Result) + cTiny)/(1 - Result + cTiny)); // Fishers z transformation
     prob := betaI( 0.5*df, 0.5, df/(df + sqr(t)) );            // probability
end;


function TCorrelation.CorrelateWeighted(x, y, w: IMatrix): double;
var w1, w2 : IMatrix;
    weights : IMatrix;
begin
     weights := w;
     if weights.Height <> 1 then
        weights := w.Reshape(w.Width*w.Height, 1);
     w1 := x;
     if x.Height <> 1 then
        w1 := x.Reshape(x.Width*x.Height, 1);
     w2 := y;
     if y.Height <> 1 then
        w2 := y.Reshape(y.Width*y.Height, 1);

     assert(w1.Width = w2.Width, 'Dimension error');
     assert(weights.Width = w1.Width, 'Dimension error');

     Result := InternalWeightedCorrelate(w1, w2, weights);
end;


// ############################################################
// #### procedures for spearman rank
// ############################################################

// given a sorted array - replace the elements by their rank, including midranking of ties
// and return s as the sum of f^3 - f.
function crank( var w : TDoubleDynArray ) : double;
var j, ji,jt : integer;
    t, rank : double;
begin
     Result := 0;

     j := 0;
     while j < Length(w) - 1 do
     begin
          if w[j + 1] <> w[j] then
          begin
               w[j] := j;
               inc(j);
          end
          else
          begin
               jt := j + 1;
               while (jt < Length(w)) and (w[jt] = w[j]) do
                     inc(jt);
               rank := 0.5*(j + jt - 1);
               for ji := j to jt - 1 do
                   w[ji] := rank;

               t := jt - j;
               Result := REsult + t*t*t - t;
               j := jt;
          end;
     end;

     if j = Length(w) - 1 then
        w[Length(w) - 1] := Length(w);
end;

procedure spearmanCorr( data1, data2 : PConstDoubleArr; n : integer; var d, zd, probd, rs, probrs : double);
var j : integer;
    vard, t, sg, sf : double;
    fac, en3n, en, df : double;
    aved : double;
    wksp1, wksp2 : TDoubleDynArray;
begin
     SetLength(wksp1, n);
     SetLength(wksp2, n);

     Move( data1^[0], wksp1[0], n*sizeof(double));
     Move( data2^[0], wksp2[0], n*sizeof(double));

     QuickSort2(wksp1, wksp2);
     sf := crank( wksp1 );
     QuickSort2(wksp2, wksp1);
     sg := crank( wksp2 );
     d := 0;

     for j := 0 to n - 1 do
         d := d + sqr( wksp1[j] - wksp2[j] );

     en := n;
     en3n := en*en*en - en;
     aved := en3n/6 - (sf + sg)/12;
     fac := (1 - sf/en3n)*(1 - sg/en3n);
     vard := ((en - 1)*en*en*sqr(en + 1)/36)*fac;
     zd := (d - aved)/sqrt(vard);
     probd := errorFuncC(abs(zd)/1.4142136);

     rs := (1 - (6/en3n)*(d + (sf + sg)/12))/sqrt(fac);
     fac := (rs + 1)*(1 - rs);
     if fac > 0 then
     begin
          t := rs*sqrt((en - 2)/fac);
          df := en - 1;
          probrs := betai(0.5*df, 0.5, df/(df + t*t));
     end
     else
         probrs := 0;
end;

end.
