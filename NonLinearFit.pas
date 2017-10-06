unit NonLinearFit;

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

interface

// #################################################
// #### Nonlinear optimization using Levenberg Marquard
// #################################################

uses BaseMathPersistence, Matrix, Types;

type
  TNonLinOptIteratorObj = procedure (Sender : TObject; a, x : IMatrix; y : IMatrix) of Object;
  TNonLinOptIterator = procedure (Sender : TObject; a, x : IMatrix; y : IMatrix);
type
  TNonLinFitOptimizer = class(TInterfacedObject)
  private
    fOnIterate1: TNonLinOptIteratorObj;
    fOnIterate2: TNonLinOptIterator;
    fMaxIter: integer;
    fTolFun: double;
    fDerivStep: double;
    fTolX: double;
    fSqrtEPS : double;
    fHlp, fCoef, fVec : IMatrix;
    procedure WeightedNonLinIterator(weights : IMatrix; a, x, y : IMatrix);
  public
    property MaxIter : integer read fMaxIter write fMaxIter;
    property TolFun : double read fTolFun write fTolFun;
    property TolX : double read fTolX write fTolX;
    property DerivStep : double read fDerivStep write fDerivStep;

    property OnIterateObj : TNonLinOptIteratorObj read fOnIterate1 write fOnIterate1;
    property OnIterate : TNonLinOptIterator read fOnIterate2 write fOnIterate2;

    // Levenberg Marquard least square fit for functions in th form f(x, a)
    // -> iteratively tries to optimze params a such that
    // the merit function chi^2 = sum_1_N(((y_i - f(xi,a))/sigma_i)^2) is minimized


    // Returns the fitted params. The params must be in organized in columns!
    function Optimize(x, y, weights, a0 : IMatrix) : IMatrix; overload; //
    function Optimize(x, y, a0 : IMatrix) : IMatrix; overload; // fits with sigma = 1

    // tries to find the coefficients of a polynomial P(x) of degree N the fits the data Y best in a
    // least-squares sense. P is a row vector of length N + 1 in descending powser P(1)*x^n + P(2)*x^(n-1) + ... + P(N+1)
    //
    // the regression problem is formulated in format as y = V*p or
    // y = [x^3 x^2 x 1] [ p3
    //                     p2
    //                     p1
    //                     p0]
    // the data is assumed to be order column wise, the result will then be width=x.width, height=n + 1

    function PolynomFit(x, y : TDoubleDynArray; N : integer) : IMatrix; overload;
    function PolynomFit(x, y : IMatrix; N : integer) : IMatrix; overload;
    procedure PolynomFit(x, y : IMatrix; N : Integer; res : IMatrix); overload;  // store result in res
    procedure LineFit(x, y : IMatrix; res : IMatrix); overload;  // fits a straight line throu x and y
    function LineFit(x, y : IMatrix) : IMatrix; overload;
    
    function EvalPoly(x, coef : IMatrix) : IMatrix;
    
    constructor Create;
  end;

implementation

uses SysUtils, Math, MathUtilFunc, MatrixConst, OptimizedFuncs;

{ TNonLinOptimizer }

function TNonLinFitOptimizer.Optimize(x, y, weights, a0: IMatrix): IMatrix;
var lambda : double;
    iter : Integer;
    k : Integer;
    p : integer;
    zbeta : IMatrix;
    aOld : IMatrix;
    r : IMatrix;
    rplus : IMatrix;
    yFit : IMatrix;
    yPlus : IMatrix;
    sse, sseOld : double;
    delta : IMatrix;
    nb : double;
    a1 : IMatrix;
    J : IMatrix;
    diagJtJ : IMatrix;
    Jplus : IMatrix;
    i : Integer;
    yRef : IMatrix;
    stepSize : double;
begin
     assert(a0.width = 1, 'Error only param vectors are allowed');
     lambda := 0.01;
     p := a0.height;
     zbeta := TDoubleMatrix.Create(1, p);
     aOld := TDoubleMatrix.Create(1, a0.height);
     yFit := TDoubleMatrix.Create(y.Width, y.Height);
     yPlus := TDoubleMatrix.Create(yFit.Width, yFit.Height);
     delta := TDoubleMatrix.Create(zbeta.Width, zbeta.height);
     J := TDoubleMatrix.Create(a0.Height, x.Height);
     yRef := TDoubleMatrix.Create;
     yRef.Assign(y);

     Result := TDoubleMatrix.Create;
     Result.Assign(a0);

     // #################################################
     // #### initialize first step
     // scale y params by the given weighting
     yRef.ElementWiseMultInPlace(weights);

     WeightedNonLinIterator(weights, a0, x, yFit);

     r := yRef.Sub(yFit);
     sse := sqr(r.ElementwiseNorm2);

     // #################################################
     // #### Iterate until convergance
     for iter := 0 to fMaxIter - 1 do
     begin
          aOld.Assign(Result);
          sseOld := sse;

          // #################################################
          // #### Compute finite difference to the Jacobian
          for k := 0 to p - 1 do
          begin
               delta.Assign(zbeta);

               if Result[0, k] = 0 then
               begin
                    nb := sqrt(Result.ElementwiseNorm2);
                    delta[0, k] := fDerivStep*(nb + ifthen(nb = 0, 1, 0));
               end
               else
                   delta[0, k] := fDerivStep*Result[0, k];

               a1 := Result.Add(delta);
               WeightedNonLinIterator(weights, a1, x, yPlus);

               yplus.SubInPlace(yFit);
               yplus.ScaleInPlace(1/delta[0, k]);

               J.SetColumn(k, yplus);
               a1 := nil;
          end;

          // #################################################
          // #### Levenberg-Marquardt step: inv(J'*J + lambda*D)*J'*r
          diagJtJ := J.ElementWiseMult(J);
          diagJtJ.SumInPlace(False);
          Jplus := TDoubleMatrix.Create(J.Width, J.Height + diagJtJ.Width);
          Jplus.AssignSubMatrix(J);
          for i := 0 to diagJtJ.Width - 1 do
              JPlus[i, J.Height + i] := sqrt(lambda*diagJtJ[i, 0]);

          rplus := TDoubleMatrix.Create(r.Width, r.Height + p);
          rplus.AssignSubMatrix(r);

          if JPlus.PseudoInversionInPlace <> srOk then
             raise Exception.Create('Error could not invert Jacobian');
          Jplus.MultInPlace(rplus);
          Result.AddInplace(Jplus);

          stepSize := JPlus.ElementwiseNorm2;

          diagJtJ := nil;
          Jplus := nil;

          // evaluate the fitted values at the new coefficients and compute the residuals + sse.
          WeightedNonLinIterator(weights, Result, x, yFit);

          r := yRef.Sub(yFit);
          sse := sqr(r.ElementwiseNorm2);

          // if the LM step decreased teh SSE, decrease lambda to downwight the steepest descent direction
          if sse < sseOld then
          begin
               lambda := 0.1*lambda;
          end
          else
          begin
               lambda := 10*lambda;
               if lambda > 1e16 then
                  raise Exception.Create('Unable to find a step that decreases the error');

               diagJtJ := J.ElementWiseMult(J);
               diagJtJ.SumInPlace(False);
               Jplus := TDoubleMatrix.Create(J.Width, J.Height + diagJtJ.Width);
               Jplus.AssignSubMatrix(J);
               for i := 0 to diagJtJ.Width - 1 do
                   JPlus[i, J.Height + i] := diagJtJ[i, 0];

               if JPlus.PseudoInversionInPlace <> srOk then
                  raise Exception.Create('Error could not invert Jacobian');
               Jplus.MultInPlace(rplus);

               Result := aOld.Add(Jplus);

               r := yRef.Sub(yFit);
               sse := sqr(r.ElementwiseNorm2);
          end;

          rplus := nil;

          // #################################################
          // #### Check for convergence
          if abs(sse - sseOld) < fTolFun*sse then
             break;
          if stepSize < fTolX*(fSqrtEPS + Result.ElementwiseNorm2) then
             break;
     end;
end;

constructor TNonLinFitOptimizer.Create;
begin
     inherited Create;

     fMaxIter := 100;
     fTolFun := 1e-8;
     fTolX := 1e-8;
     fDerivStep := Power(eps(1), 1/3);
     fSqrtEPS := sqrt(eps(1));
end;

function TNonLinFitOptimizer.EvalPoly(x, coef: IMatrix): IMatrix;
var counter: Integer;
    doTranspose : boolean;
begin
     // init ( avoid memory allocations in case it's used in a loop
     if not Assigned(fHlp) then
        fHlp := TDoubleMatrix.Create;
     if not Assigned(fCoef) then
        fCoef := TDoubleMatrix.Create;
     if not Assigned(fVec) then
        fVec := TDoubleMatrix.Create;
     if fVec.Width <> x.VecLen then
        fVec.SetWidthHeight(x.VecLen, 1);
     if fCoef.Width <> coef.VecLen then
        fCoef.SetWidthHeight(coef.VecLen, 1);

     // prepare coefficents
     for counter := 0 to fCoef.Width - 1 do
         fCoef.Vec[counter] := coef.Vec[counter];
     
     if (fHlp.Width <> x.Width) or (fHlp.Height <> fcoef.Width) then
        fHlp.SetWidthHeight(x.VecLen, fcoef.Width);

     doTranspose := x.Width < x.Height;
     if doTranspose then
        x.TransposeInPlace;

     // prepare mult matrix
     fVec.SetValue( 1 );
     for counter := fHlp.Height - 1 downto 0 do
     begin
          fHlp.SetRow(counter, fVec);
          fVec.ElementWiseMultInPlace( x ); 
     end;

     // final multiplication
     Result := fCoef.Mult(fHlp);

     if doTranspose then
        x.TransposeInPlace;
end;

function TNonLinFitOptimizer.Optimize(x, y, a0: IMatrix): IMatrix;
var weights : IMatrix;
begin
     weights := TDoubleMatrix.Create(1, x.height);
     weights.ScaleAndAddInPlace(1, 0);

     Result := Optimize(x, y, weights, a0);
end;

function TNonLinFitOptimizer.PolynomFit(x, y: TDoubleDynArray;
  N: integer): IMatrix;
var xVals, yVals : IMatrix;
begin
     assert(Length(x) = Length(y), 'Error length of xvals is different to length of y');
     assert(Length(x) > n, 'error cannot calculate polynomfit on data less then N');
     assert(n > 0, 'Error polynomdegree must be at least 1');

     xVals := TDoubleMatrix.Create(x, 1, Length(x));
     yVals := TDoubleMatrix.Create(y, 1, Length(y));

     Result := PolynomFit(xVals, yVals, N);
end;

function TNonLinFitOptimizer.PolynomFit(x, y: IMatrix; N: integer): IMatrix;
begin
     assert(x.Height = y.Height, 'Error length of xvals is different to length of y');
     assert((x.Width >= 1) and (y.Width >= 1), 'Dimension error');
     assert(n > 0, 'Error polynomdegree must be at least 1');

     Result := TDoubleMatrix.Create(x.Width, N + 1);
     PolynomFit(x, y, N, Result);
end;

procedure TNonLinFitOptimizer.PolynomFit(x, y: IMatrix; N: Integer;
  res: IMatrix);
var V : IMatrix;
    j, i : integer;
    dim : integer;
    p : IMatrix;
begin
     assert(x.Height = y.Height, 'Error length of xvals is different to length of y');
     assert((x.Width >= 1) and (y.Width >= 1), 'Dimension error');
     assert(n > 0, 'Error polynomdegree must be at least 1');
     assert((res.width = x.Width) and (res.Height = N + 1), 'Resulting matrix size does not fit');
     
     for dim := 0 to x.Width - 1 do
     begin
          // construct Vandermonde matrix
          V := TDoubleMatrix.Create(n + 1, x.Height, 1);
          for j := n - 1 downto 0 do
          begin
               for i := 0 to V.Height - 1 do
                   V[j, i] := V[j + 1, i]*x[dim, i];
          end;

          // Create result p = V\y
          if V.PseudoInversionInPlace <> srOk then
             raise Exception.Create('Error cannot create pseudoinverse of the Vandermonde matrix');

          y.SetSubMatrix(dim, 0, 1, y.Height);
          p := V.Mult(y);

          // assign result
          res.SetColumn(dim, p);
     end;     
end;

procedure TNonLinFitOptimizer.LineFit(x, y, res: IMatrix);
var meanx, meanY : double;
    m1, m2 : double;
    counter: Integer;
    pX, pY : PDouble;
    pcX, pcY : PConstDoubleArr;
begin
     MatrixMean(@meanx, sizeof(double), x.StartElement, x.LineWidth, 1, x.Height, False);
     MatrixMean(@meany, sizeof(double), y.StartElement, y.LineWidth, 1, y.Height, False);

     m1 := 0;
     m2 := 0;

     if (x.LineWidth = sizeof(double)) and (y.LineWidth = sizeof(double)) then
     begin
          pcX := PConstDoubleArr( x.StartElement );
          pcY := PConstDoubleArr( y.StartElement );
          for counter := 0 to x.Height - 1 do
          begin
               m1 := m1 + (pcX^[counter] - meanX)*(pcY^[counter] - meanY);
               m2 := m2 + sqr(pcX^[counter] - meanX);
          end;
     end
     else
     begin           
          pX := x.StartElement;
          pY := y.StartElement;
          for counter := 0 to x.Height - 1 do
          begin
               m1 := m1 + (pX^ - meanX)*(pY^ - meanY);
               m2 := m2 + sqr(pX^ - meanX);

               inc(PByte(pX), x.LineWidth);
               inc(PByte(pY), y.LineWidth);
          end;
     end;

     res.Vec[0] := m1/m2;
     res.Vec[1] := meanY - m1/m2*meanX;
end;

function TNonLinFitOptimizer.LineFit(x, y: IMatrix): IMatrix;
begin
     Result := TDoubleMatrix.Create(1, 2);
     LineFit(x, y, Result); 
end;

procedure TNonLinFitOptimizer.WeightedNonLinIterator(weights, a, x,
  y: IMatrix);
begin
     if Assigned(fOnIterate1)
     then
         fOnIterate1(Self, a, x, y)
     else if Assigned(fOnIterate2)
     then
         fOnIterate2(Self, a, x, y);

     // weight the outcome...
     y.ElementWiseMultInPlace(weights);
end;

end.
