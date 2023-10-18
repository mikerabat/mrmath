// ###################################################################
// #### This file is part of the mathematics library project, and is
// #### offered under the licence agreement described on
// #### http://www.mrsoft.org/
// ####
// #### Copyright:(c) 2018, Michael R. . All rights reserved.
// ####
// #### Unless required by applicable law or agreed to in writing, software
// #### distributed under the License is distributed on an "AS IS" BASIS,
// #### WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// #### See the License for the specific language governing permissions and
// #### limitations under the License.
// ###################################################################

unit RBSplines;

// ###########################################
// #### Robust baseline spline calculation
// #### this class is based on the code from the public accessible:
// #### https://de.mathworks.com/matlabcentral/fileexchange/13812-splinefit
// ###########################################

// -> includes robust fitting 
// -> different order b splines
// -> nonuniform distributions of breaks

interface

uses sysUtils, Types, Matrix;

type
  ESplineException = class(Exception);
type
  TRobustBSpline = class(TMatrixClass)
  private
    const cRobustEstCnt : integer = 3;
  private
    fBeta : double;
    fOrder : integer;
    fPeriodic : boolean;

    fBreaks : IMatrix;
    fCoefs : IMatrix;

    function evalPoly(x : IMatrix; dim : integer; coefs : IMatrix; var idx : TIntegerDynArray) : IMatrix;
    procedure splineBase(breaks : TDoubleDynArray);
    function SolveSystem(x, y : IMatrix) : IMatrix;

    procedure ExpFunc( var x : double );
    function LQSolve(A, y : IMatrix) : IMatrix;
    function RobustSolve(A, y : IMatrix) : IMatrix;

    function IntCoefs : IMatrix; overload;
    procedure InternalInitCoefs(mtxX, mtxY: IMatrix; breaksLen : integer); overload;
  public
    property Coefs : IMatrix read fCoefs;
    
    procedure InitSpline(const x, y, breaks : TDoubleDynArray); overload;
    function EvalSpline(const x : TDoubleDynArray) : TDoubleDynArray; overload;

    procedure InitSpline(x, y, breaks : IMatrix); overload;
    function EvalSpline(x : IMatrix) : IMatrix; overload;

    // calculates the jth derrivative of the spline at points x
    function DiffSpline(const x : TDoubleDynArray; j : integer) : TDoubleDynArray; overload;
    function DiffSpline(x : IMatrix; j : integer) : IMatrix; overload;
    function IntSpline( const intFrom, intTo : double) : double; overload;      // definitive integral
    function IntSpline(const x : TDoubleDynArray) : TDoubleDynArray; overload;  // indefinite integral @ breaks[0]
    function IntSpline(a : double; const x : TDoubleDynArray) : TDoubleDynArray; overload;  // indefinite integral
    function IntSpline(x : IMatrix) : IMatrix; overload;  // indefinite integral @ breaks[0]
    function IntSpline(a : double; x : IMatrix) : IMatrix; overload;  // indefinite integral

    // ###########################################
    // #### Just for debugging
    function DebugSplineBase( breaks : TDoubleDynArray ) : IMatrix;
    function DebugEvalPoly( x, breaks : TDoubleDynArray) : IMatrix;
    function DebugSolveSystem( x, y, breaks : TDoubleDynArray) : IMatrix;
    
    // Spline Order, default is cubic spline with order = 4;
    // Beta = 0 -> non robust solving; 
    // Three iterations of weighted least squares
    // are performed. Weights are computed from previous residuals.
    // BETA close to 0 gives all data equal weighting.
    // Increase BETA to reduce the influence from outlying data. BETA close
    // to 1 may cause instability or rank deficiency.
    constructor Create( Order : integer = 4; beta : double = 0; periodicExpansion : boolean = False); 
  end;

implementation

uses Math, MatrixConst;

{ TSplineFilter }

constructor TRobustBSpline.Create(Order : integer = 4; beta : double = 0; periodicExpansion : boolean = False);
begin
     fOrder := order;
     fBeta := beta;   
     fPeriodic := periodicExpansion;     

     inherited Create;
end;

function TRobustBSpline.SolveSystem(x, y : IMatrix): IMatrix;
var base : IMatrix;
    i : Integer;
    A : IMatrix;
    hA : integer;
    idx : TIntegerDynArray;
    ii : integer;
    h : integer;
    tmp : IMatrix;
begin
     base := EvalPoly(x, forder, fCoefs, idx);

     // base as length(x) x forder (width x height)
     // extend to length(x) x (forder + length(breaks) )
     if fPeriodic then
     begin
          hA := fBreaks.Height - 1;
          A := MatrixClass.Create( base.Width, hA );
          

          for i := 0 to base.Width - 1 do
          begin
               h := base.Height;
               if Idx[i] + fOrder <= hA then
               begin
                    base.SetSubMatrix(i, 0, 1, h);
                    A.AssignSubMatrix(base, i, Idx[i]);
               end
               else
               begin
                    base.UseFullMatrix;
                    for ii := 0 to h - 1 do
                        A[i, (Idx[i] + ii) mod hA] := base[i, ii];
               end;
          end;
     end
     else
     begin
          A := MatrixClass.Create( base.Width, base.Height + fBreaks.Height - 2 );

          for i := 0 to base.Width - 1 do
          begin
               base.SetSubMatrix(i, 0, 1, base.Height);
               A.AssignSubMatrix(base, i, Idx[i]);
          end;
     end;

     A.UseFullMatrix;
     A.TransposeInPlace;

     if fBeta = 0 
     then
         Result := LQSolve(A, y)
     else
         Result := RobustSolve(A, y);

     if fPeriodic then
     begin
          tmp := Result.Clone;
          Result := MatrixClass.Create( tmp.Width + fOrder - 1, 1);
          for i := 0 to Result.VecLen - 1 do
              Result.Vec[i] := tmp.Vec[i mod tmp.VecLen];
     end;
end;

function TRobustBSpline.DebugEvalPoly(x, breaks: TDoubleDynArray): IMatrix;
var mtxX : IMatrix;
    idx : TIntegerDynArray;
begin
     mtxX := MatrixClass.Create(x, 1, Length(x));
     
     splineBase(breaks);
     Result := evalPoly(mtxX, forder, fCoefs, idx);
end;

function TRobustBSpline.DebugSolveSystem(x, y, breaks: TDoubleDynArray): IMatrix;
var mtxX, mtxY : IMatrix;
begin
     mtxX := MatrixClass.Create(x, 1, Length(x));
     mtxY := MatrixClass.Create(y, 1, Length(y));
     
     splineBase(breaks);
     Result := SolveSystem(mtxX, mtxY);
end;

function TRobustBSpline.DebugSplineBase(
  breaks: TDoubleDynArray): IMatrix;
begin
     splineBase(breaks);

     Result := fCoefs;
end;

function TRobustBSpline.DiffSpline(x: IMatrix; j: integer): IMatrix;
var res : TDoubleDynArray;
begin
     res := DiffSpline(x.SubMatrix, j);

     Result := MatrixClass.Create(res, 1, Length(res));
end;

function TRobustBSpline.DiffSpline(const x: TDoubleDynArray; j: integer): TDoubleDynArray;
var diffCoefs : IMatrix;
    mtxX : IMatrix;
    s, i: Integer;
    diffScale : double;
    dummy : TIntegerDynArray;
    ii: Integer;
begin
     if not Assigned(fCoefs) then
        raise Exception.Create('Call init spline first');
        
     if j = 0 
     then
         Result := EvalSpline(x)
     else if (j > 0) and (j < fOrder) then
     begin
          // derrivative of order j
          fCoefs.SetSubMatrix(0, 0, fCoefs.Width - j, fCoefs.Height);
          
          diffCoefs := fCoefs.Clone;
          fCoefs.UseFullMatrix;
          s := fCoefs.Width - 1;
          for i := 0 to diffCoefs.Width - 1 do
          begin
               diffScale := s - i;
               for ii := 1 to j - 1 do
                   diffScale := diffScale * (s - i - ii);
               diffCoefs.SetSubMatrix(i, 0, 1, diffCoefs.Height);
               diffCoefs.ScaleInPlace( diffScale );
          end;

          diffCoefs.UseFullMatrix;

          mtxX := MatrixClass.Create(x, 1, Length(x));
          Result := evalPoly(mtxX, 1, diffCoefs, dummy).SubMatrix;
     end
     else
     begin
          // derivative kills poly
          SetLength(Result, Length(x));
          if Length(Result) > 0 then
             FillChar(x[0], sizeof(double)*Length(x), 0);
     end;
end; 

function TRobustBSpline.evalPoly(x: IMatrix; dim : integer; coefs : IMatrix; var idx : TIntegerDynArray): IMatrix;
var xs : TDoubleDynArray;
    breakIdx : integer;
    y, yy : Integer;
    v : TDoubleDynArray;
    i : integer;
    ii : integer;
begin
     Result := nil;
     if (x.Height = 0) or not assigned(fBreaks) or (fBreaks.Height < 1) then
        exit;
        
     breakIdx := 0;
     SetLength(xs, x.Height);
     
     SetLength(idx, x.Height);
     SetLength(xs, x.Height);
     
     // xs to local coordinates
     for y := 0 to x.Height - 1 do
     begin
          while (breakIdx < fBreaks.VecLen - 2) and (fBreaks.Vec[breakIdx + 1] < x.Vec[y]) do
                inc(breakIdx);

          idx[y] := breakIdx;
          xs[y] := x.Vec[y] - fBreaks.Vec[breakIdx];
     end;

     SetLength(v, dim*x.Height);
     FillChar(v[0], Length(v)*sizeof(double), 0);
     
     for i := 0 to Coefs.Width - 1 do
     begin
          ii := 0;
          for y := 0 to Length(xs) - 1 do
          begin
               for yy := 0 to dim - 1 do
               begin
                    v[ii] := v[ii]*xs[y] + Coefs[i, yy + dim*idx[y]];
                    inc(ii);
               end;
          end;
     end;

     Result := MatrixClass.Create(v, 1, Length(v)); 
     Result.ReshapeInPlace( Length(xs), dim, True );
end;

function TRobustBSpline.EvalSpline(const x : TDoubleDynArray): TDoubleDynArray;
var mtxX : IMatrix;
    dummy : TIntegerDynArray;
begin
     if not Assigned(fCoefs) then
        raise Exception.Create('Call init spline first');

     mtxX := MatrixClass.Create(x, 1, Length(x));
     Result := evalPoly(mtxX, 1, fCoefs, dummy).SubMatrix;
end;

function TRobustBSpline.EvalSpline(x: IMatrix): IMatrix;
var dummy : TIntegerDynArray;
begin
     if not Assigned(fCoefs) then
        raise Exception.Create('Call init spline first');
        
     Result := evalPoly(x, 1, fCoefs, dummy);
end;

procedure TRobustBSpline.ExpFunc(var x: double);
begin
     x := -x;
     if x < -12 
     then
         x := 0
     else if x > 20 
     then
         x := exp(20)
     else
         x := exp(x);
end;

procedure TRobustBSpline.InitSpline(const x, y, breaks: TDoubleDynArray);
var mtxX, mtxY : IMatrix;
begin
     mtxX := MatrixClass.Create(x, 1, Length(x));
     mtxY := MatrixClass.Create(y, 1, Length(y));
     
     splineBase(breaks);
     
     InternalInitCoefs( mtxX, mtxY, Length(breaks) );
end;

procedure TRobustBSpline.InternalInitCoefs( mtxX, mtxY : IMatrix; breaksLen : integer );
var u : IMatrix;
    C : IMatrix;
    iX : integer;
    iY : integer;
    coefsX : integer;
    coefsY : integer;
    origHeight : integer;
begin
     u := SolveSystem(mtxX, mtxY);

     // update the base
     C := MatrixClass.Create( ( breaksLen - 1)*fOrder, breaksLen + fOrder - 2 );

     iY := 0;
     coefsX := 0;
     coefsY := 0;
     origHeight := fcoefs.Height;
     for iX := 0 to C.Width - 1 do
     begin
          fCoefs.SetSubMatrix(coefsX, coefsY, 1, fOrder);

          C.AssignSubMatrix(fCoefs, iX, iY);

          inc(iY);
          inc(coefsY, fOrder);
          if coefsY = origHeight then
          begin
               coefsY := 0;
               inc(coefsX);

               iY := 0;
          end;
     end;
     
     C.UseFullMatrix;
     fCoefs := u.Mult(C);
     fCoefs.ReshapeInPlace(fOrder, fcoefs.Width div fOrder, True);
end;

procedure TRobustBSpline.InitSpline(x, y, breaks: IMatrix);
var mtxX, mtxY : IMatrix;
begin
     mtxX := x.Clone;
     mtxY := y.Clone;
     
     splineBase(breaks.SubMatrix);

     InternalInitCoefs( mtxX, mtxY, breaks.Height );
end;

function TRobustBSpline.IntCoefs: IMatrix;
var hb : IMatrix;
    iCoefs : IMatrix;
    y : IMatrix;
    n, m : Integer;
    k : Integer;
begin
     hb := fBreaks.Diff(False);

     n := fCoefs.Width;
     m := fCoefs.Height;
     
     iCoefs := MatrixClass.Create(n + 1, m );
     iCoefs.AssignSubMatrix(fCoefs);

     iCoefs.SetSubMatrix(0, 0, 1, m);
     iCoefs.ScaleInPlace( 1/n );

     y := iCoefs.ElementWiseMult(hb);

     for k := 1 to n - 1 do
     begin
          iCoefs.SetSubMatrix(k, 0, 1, m);
          iCoefs.ScaleInPlace(1/(n - k));

          y.AddInplace(iCoefs);
          y.ElementWiseMultInPlace(hb);
     end;

     iCoefs.UseFullMatrix;
     y.CumulativeSumInPlace(False);
     iCoefs[n, 0] := 0;
     y.SetSubMatrix(0, 0, 1, m - 1);
     iCoefs.AssignSubMatrix(y, n, 1);


     iCoefs.UseFullMatrix;
     Result := iCoefs;
end;

function TRobustBSpline.IntSpline(const x : TDoubleDynArray): TDoubleDynArray;
var iCoefs : IMatrix;
    dummy : TIntegerDynArray;
    mtxX : IMatrix;
begin
     iCoefs := IntCoefs;

     mtxX := MatrixClass.Create(x, 1, Length(x));

     Result := evalPoly(mtxX, 1, iCoefs, dummy).SubMatrix;
end;

function TRobustBSpline.IntSpline(x: IMatrix): IMatrix;
var iCoefs : IMatrix;
    dummy : TIntegerDynArray;
    mtxX : IMatrix;
begin
     iCoefs := IntCoefs;

     if x.Width > 0 
     then
         mtxX := x.Transpose
     else
         mtxX := x;

     Result := evalPoly(mtxX, 1, iCoefs, dummy);
end;

function TRobustBSpline.IntSpline(a: double; const x: TDoubleDynArray): TDoubleDynArray;
var iCoefs : IMatrix;
    aMtx : IMatrix;
    i0 : IMatrix;
    dummy : TIntegerDynArray;
    mtxX : IMatrix;
begin
     iCoefs := IntCoefs;

     // indefinite integral from breaks[0] to a
     aMtx := MatrixClass.Create(1, 1, a);
     i0 := evalPoly(aMtx, 1, iCoefs, dummy);

     iCoefs.SetSubMatrix( iCoefs.Width - 1, 0, 1, iCoefs.Height);
     iCoefs.AddAndScaleInPlace(-i0.Vec[0], 1);
     iCoefs.UseFullMatrix;
     
     mtxX := MatrixClass.Create(x, 1, Length(x));

     Result := evalPoly(mtxX, 1, iCoefs, dummy).SubMatrix;
end;

function TRobustBSpline.IntSpline(a: double; x: IMatrix): IMatrix;
var iCoefs : IMatrix;
    aMtx : IMatrix;
    i0 : IMatrix;
    dummy : TIntegerDynArray;
    mtxX : IMatrix;
begin
     iCoefs := IntCoefs;

     // indefinite integral from breaks[0] to a
     aMtx := MatrixClass.Create(1, 1, a);
     i0 := evalPoly(aMtx, 1, iCoefs, dummy);

     iCoefs.SetSubMatrix( iCoefs.Width - 1, 0, 1, iCoefs.Height);
     iCoefs.AddAndScaleInPlace(-i0.Vec[0], 1);
     iCoefs.UseFullMatrix;
     
     if x.Width > 1 
     then
         mtxX := x.Transpose
     else
         mtxX := x;

     Result := evalPoly(mtxX, 1, iCoefs, dummy);
end;

function TRobustBSpline.IntSpline(const intFrom, intTo: double): double;
var iCoefs : IMatrix;
    a, b : IMatrix;
    dummy : TIntegerDynArray;
    mtxA, mtxB : IMatrix;
begin
     iCoefs := IntCoefs;

     a := MatrixClass.Create(1, 1, intFrom);
     b := MatrixClass.Create(1, 1, intTo);

     mtxA := evalPoly(a, 1, iCoefs, dummy);
     mtxB := evalPoly(b, 1, iCoefs, dummy);
     Result := mtxB.Vec[0] - mtxA.Vec[0];
end;

function TRobustBSpline.LQSolve(A, y: IMatrix): IMatrix;
var aInv : IMatrix;
begin
     if A.Width > A.Height then
     begin
          if A.PseudoInversion(aInv) <> srOk then
             raise ESplineException.Create('error no spline system convergence achieved');
            
          Result := aInv.Mult(y);          
     end
     else
         A.SolveLeastSquares(Result, y);

     Result.TransposeInPlace;
end;

function TRobustBSpline.RobustSolve(A, y: IMatrix): IMatrix;
var iter : integer;
    alpha : double;
    h : Integer;
    idx : integer;
    r : IMatrix;
    rrmean : double;
    sum : IMatrix;
    yspw, aspw : IMatrix;
begin
     // ###########################################
     // #### Robust solving of an overdetermined linear equation system
     // -> solves the system 3 times and estimates the residual
     // -> the residual is used to "weaken" the influence of equations
     //    that create a high residual.
     Result := LQSolve(A, y);
     h := y.Height;
     
     alpha := 0.5*fBeta/(1 - fBeta);
     for iter := 0 to cRobustEstCnt - 1 do
     begin
          // residual
          Result.TransposeInPlace;
          r := A.Mult(Result); 
          r.SubInPlace( y );
          
          r.ElementWiseMultInPlace(r);
          sum := r.Sum(False);
          rrmean := sum.Vec[0]/h;
          
          if rrMean = 0 then
             rrMean := 1;

          r.ScaleInPlace( alpha / rrmean );
          r.ElementwiseFuncInPlace( {$IFDEF FPC}@{$ENDIF}ExpFunc );

          // Solve weighted problem
          yspw := y.Clone;
          aspw := A.Clone;

          yspw.ElementWiseMultInPlace(r);
          for idx := 0 to A.Width - 1 do
          begin
               aspw.SetSubMatrix(idx, 0, 1, A.height);
               aspw.ElementWiseMultInPlace(r);
          end;
          aspw.UseFullMatrix;

          Result := LQSolve(aspw, yspw);
     end;
end;

procedure TRobustBSpline.splineBase(breaks: TDoubleDynArray);
var h : IMatrix;
    counter: Integer;
    pieces : integer;
    deg : integer;
    aCoefs : IMatrix;
    cnt: Integer;
    hh : IMatrix;
    idx : integer;
    k : integer;
    j : Integer;
    Q : IMatrix;
    c0 : IMatrix;
    x, y : integer;
    fmax : IMatrix;
    tmp : IMatrix;
    scale : IMatrix;
    sortIdx : TIntegerDynArray;
begin
     deg := fOrder - 1;
     pieces := Length(breaks);
     
     // differentiate breaks
     h := nil;
     if pieces > 1 then
     begin
          h := MatrixClass.Create(pieces - 1 + 2*deg, 1);
          for counter := 0 to pieces - 2 do
              h[deg + counter, 0] := breaks[counter + 1] - breaks[counter];      
     end;

     // extend breaks periodically
     if forder - 1 > 0 then
     begin
          for counter := 0 to deg - 1 do
          begin
               h[counter, 0] := h[ h.Width - 2*deg + counter, 0];
               h[h.Width - deg + counter, 0] := h[deg + counter, 0];
          end;
     end;

     pieces := h.Width;

     // initiate polynomial coefficients
     aCoefs := MatrixClass.Create( fOrder, fOrder*pieces );
     for counter := 0 to pieces - 1 do
         aCoefs[0, counter*fOrder] := 1;

     // expand h
     hh := MatrixClass.Create(1, fOrder*pieces);
     idx := 0;
     for counter := 0 to pieces - 1 do
     begin
          for cnt := 0 to fOrder - 1 do
          begin
               hh.Vec[idx] := h.Vec[ Math.Min(pieces - 1, counter + cnt) ];
               inc(idx);
          end;
     end;

     fmax := MatrixClass.Create( 1, aCoefs.height );
     // Recursive generation of B-splines          
     for k := 1 to fOrder - 1 do
     begin
          for j := 0 to k - 1 do
          begin
               aCoefs.SetSubMatrix(j, 0, 1, aCoefs.Height);
               aCoefs.ElementWiseMultInPlace(hh);
               aCoefs.ScaleInPlace( 1/(k - j) );
          end;

          aCoefs.UseFullMatrix;

          Q := aCoefs.Sum(True);
          Q.ReshapeInPlace( pieces, fOrder, True );
          Q.CumulativeSumInPlace(False);
          
          c0 := MatrixClass.Create( q.Width, q.Height );
          Q.SetSubMatrix(0, 0, q.Width, q.Height - 1);
          c0.AssignSubMatrix(Q, 0, 1);

          c0.ReshapeInPlace(1, c0.Width*c0.Height, True);
          aCoefs.AssignSubMatrix(c0, k, 0);                // assigns column

          // Normalize antiderivatives by max value
          Q.SetSubMatrix(0, fOrder - 1, Q.Width, 1);
          fmax := Q.RepeatMatrix(1, fOrder);
          fmax.ReshapeInPlace(1, aCoefs.Height, True);
          Q.UseFullMatrix;

          for j := 0 to k do
          begin
               aCoefs.SetSubMatrix(j, 0, 1, aCoefs.Height);
               aCoefs.ElementWiseDivInPlace(fmax);
          end;

          aCoefs.UseFullMatrix;

          // Diff of adjacent antiderivatives
          tmp := aCoefs.Clone;
          tmp.SetSubMatrix(0, fOrder - 1, k + 1, tmp.Height - fOrder + 1 );
          aCoefs.SetSubMatrix(0, 0, k + 1, aCoefs.Height - fOrder + 1 );
          aCoefs.SubInPlace(tmp);

          aCoefs.UseFullMatrix;
          
          idx := 0;
          while idx < aCoefs.Height do
          begin
               aCoefs[k, idx] := 0;
               inc(idx, fOrder);
          end;
     end;

     // Scale coefficients
     scale := MatrixClass.Create( hh.Width, hh.Height, 1);
     for k := 1 to forder - 1 do
     begin
          scale.ElementWiseDivInPlace(hh);
          aCoefs.SetSubMatrix(deg - k, 0, 1, aCoefs.Height);
          aCoefs.ElementWiseMultInPlace(scale);
     end;

     // reduce number of pieces
     dec(pieces, 2*deg);

     aCoefs.UseFullMatrix;
     
     // Sort coefficients by interval number
     SetLength(sortIdx, fOrder*pieces);

     idx := 0;
     for x := 1 to pieces do
     begin
          for y := 0 to fOrder - 1 do
          begin 
               sortIdx[idx] := fOrder*x + y*deg - 1;
               inc(idx);
          end;
     end;

     tmp := MatrixClass.Create(fOrder, Length(sortIdx));

     idx := 0;
     for y := 0 to tmp.Height - 1 do
     begin
          for x := 0 to tmp.Width - 1 do
              tmp[x, y] := aCoefs[x, sortIdx[idx]];

          inc(idx);
     end;

     aCoefs := tmp;


     // ###########################################
     // #### setup private members
     fCoefs := aCoefs;
     fBreaks := MatrixClass.Create(breaks, 1, Length(breaks));
end;

end.
