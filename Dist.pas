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

unit Dist;

interface

uses Matrix, BaseMathPersistence;

// ###################################################################
// #### different distance measures
// -> eukledian distance from mean
// -> abs distance from geometric median
// -> mahalonobis distance

// note that this class provides the possibility to calculate the distances from
// different initialization routins too (e.g use initEuclid and the L1 measure)
type
  TDistance = class(TMatrixClass)
  private
    fRx : Integer;
    fMean : IMatrix;
    fR : IMatrix;
    fNumIter : integer;
  protected
    class function ClassIdentifier : String; override;
    procedure DefineProps; override;
    function PropTypeOfName(const Name : string) : TPropType; override;

    procedure OnLoadIntProperty(const Name : String; Value : integer); override;
    function OnLoadObject(const Name : String; Obj : TBaseMathPersistence) : boolean; override;
  public
    // only valid for L1 distances: shows how many iterations were needed for the geometric median
    property NumIter : integer read fNumIter;
  
    // init the distance fields from "outside"; use aMean, aR, dimx for mahalonobis and the other one for L1, euclid
    procedure Init(aMean, aCov : IMatrix); overload;
    procedure Init(aMean : IMatrix); overload;

    // Returns the eucledian distance (L2) in square units. Return value is a vector of width Y
    // euclid(i) = sum( (Y(i, :) - MU).*((Y(i, :) - MU))
    procedure InitEuclid(X : IMatrix);
    function EuclidDist(Y : IMatrix) : IMatrix;
    class function Euclid(X, Y : IMatrix) : IMatrix;

    procedure InitMahal(X : IMatrix; doTryFastInf : boolean = True);
    function MahalDist(Y : IMatrix) : IMatrix;

    // regularized weiszfeld according to: "Robust L1 approaches to computing the geometric median and principal and independent compoents"
    // Keeling, Kunisch
    procedure InitL1DistReg(X : IMatrix; tau : double = 10; maxIter : integer = 200; relTol : double = 1e-6);

    // calculate geometric median using the weiszfeld algorithm
    procedure InitL1Dist(X : IMatrix; maxIter : integer = 200; relTol : double = 1e-6; beta : double = 1e-5);  
    function L1Dist(y : IMatrix) : IMatrix;
    class function L1(x, y : IMatrix) : IMatrix; overload;
    class function L1(x, y : IMatrix; maxIter : integer; relTol : double; beta : double) : IMatrix; overload;

    class function L1Reg(x, y : IMatrix) : IMatrix; overload;
    class function L1Reg(x, y : IMatrix; maxIter : integer; relTol : double; tau : double) : IMatrix; overload;

    // Returns the Mahalonobis distance in squared units.
    // Return value is a vector of width Y.
    // mahal =  (Y( i, :) - MU)* sigma^-1 *y(i, :) - MU)
    class function Mahalanobis(X, Y : IMatrix) : IMatrix; overload;

    // calculates the mahalonobis disance from given covariance matrix R and mean M
    class function Mahalanobis(Y, aCov, aMean : IMatrix) : IMatrix; overload;

    // ###########################################
    // #### Pairwise distance functions
    // #### as input is assumed: columns are coordinates, rows are elements
    // #### Result is a vector of length (Height)*(Height - 1) div 2
    // ###########################################
    class function EuclidPairDist( X : IMatrix ) : IMatrix; overload;
    class function NormEuclidPairDist( X : IMatrix ) : IMatrix; overload;  // normalized euclid: rows are scaled by inverse of variance
    class function AbsPairDist( X : IMatrix ) : IMatrix; overload;
    class function MinkowskyPairDist( X : IMatrix; exponent : Double = 2 ) : IMatrix; overload;
    class function ChebychevPairDist( X : IMatrix ) : IMatrix; overload;
    class function MahalanobisPairDist( X : IMatrix ) : IMatrix; overload;

    class function EuclidPairDist( X : TDoubleMatrix ) : TDoubleMatrix; overload;
    class function NormEuclidPairDist( X : TDoubleMatrix ) : TDoubleMatrix; overload;  // normalized euclid: rows are scaled by inverse of variance
    class function AbsPairDist( X : TDoubleMatrix ) : TDoubleMatrix; overload;
    class function MinkowskyPairDist( X : TDoubleMatrix; exponent : Double = 2 ) : TDoubleMatrix; overload;
    class function ChebychevPairDist( X : TDoubleMatrix ) : TDoubleMatrix; overload;
    class function MahalanobisPairDist( X : TDoubleMatrix ) : TDoubleMatrix; overload;
  end;

implementation

uses SysUtils, MatrixConst, MatrixASMStubSwitch, Math, Corr, MathUtilFunc, 
  LinAlgCholesky, Types;

{ TDistance }

procedure TDistance.InitMahal(X: IMatrix; doTryFastInf : boolean = True);
var C : IMatrix;
    R : IMatrix;
begin
     fRx := x.Height;
     fMean := X.Mean(False);
     C := X.SubVec(fMean, True);
     C.QR(R);
     R.TransposeInPlace;

     if not doTryFastInf or (R.Invert(fR) <> leOk) then
        if R.PseudoInversion(fR) = srNoConvergence then
           raise Exception.Create('Error could not invert matrix');
end;

function TDistance.MahalDist(Y: IMatrix): IMatrix;
var M : IMatrix;
    ri : IMatrix;
    i : integer;
    Q : IMatrix;
begin
     if not Assigned(fMean) or not Assigned(fR) then
        raise Exception.Create('Error call InitMahal first');

     M := Y.SubVec(fMean, True);

     if fRx < 0 then
     begin
          // from init with the cholesky decomposition of the covariance matrix
          Q := TDoubleMatrix.Create( Y.Width, 1 );
          Result := MatrixClass.Create( 1, Y.Height );
          for i := 0 to M.Height - 1 do
          begin
               M.SetSubMatrix(0, i, M.Width, 1);
               MatrixCholeskyBackSolve( fR.StartElement, fR.LineWidth, fR.Width, M.StartElement, sizeof(double), Q.StartElement, sizeof(double) );
               Result[0, i] := Q.ElementwiseNorm2(False);
          end;
     end
     else
     begin
          ri := fR.MultT2(M);
          ri.ElementWiseMultInPlace(ri);
          ri.SumInPlace(False);
          Result := ri.Transpose;
          Result.ScaleInPlace(fRX - 1);
     end;
end;


// ###############################################
// #### One time used short methods
class function TDistance.Mahalanobis(X, Y: IMatrix): IMatrix;
begin
     with TDistance.Create do
     try
        InitMahal(X);
        Result := MahalDist(Y);
     finally
            Free;
     end;
end;

class function TDistance.Mahalanobis(Y, aCov, aMean: IMatrix): IMatrix;
begin
     with TDistance.Create do
     try
        Init( aMean, aCov );
        Result := MahalDist(Y);
     finally
            Free;
     end;
end;

procedure TDistance.InitEuclid(X: IMatrix);
begin
     fMean := X.Mean(False);
end;

function TDistance.EuclidDist(Y: IMatrix): IMatrix;
var dist : IMatrix;
begin
     if not Assigned(fMean) then
        raise Exception.Create('Error call InitEuclid first');
     dist := Y.SubVec(fMean, True);
     dist.ElementWiseMultInPlace(dist);
     dist.SumInPlace(True);

     Result := dist;
end;

class function TDistance.Euclid(X, Y: IMatrix): IMatrix;
begin
     with TDistance.Create do
     try
        InitEuclid(X);
        Result := EuclidDist(Y);
     finally
            Free;
     end;
end;

procedure sqrtFunc(var Value : double);
begin
     Value := 1/sqrt(Value);
end;

procedure TDistance.InitL1Dist(X: IMatrix; maxIter : integer = 200; relTol : double = 1e-6; beta : double = 1e-5);
var eps : double;
    counter : integer;
    weights, data : IMatrix;
    i : integer;
    oldMean : IMatrix;
    meanSub : IMatrix;
begin
     // initialize with geometric median using the standard Weiszfeld algorithm
     // check out: https://de.mathworks.com/matlabcentral/fileexchange/64781-weiszfeld-input-structure-
     fNumIter := 0;
     
     fMean := X.Mean(False);

     oldMean := fMean.Clone;
     data := MatrixClass.Create;
     meanSub := MatrixClass.Create;
     for counter := 0 to maxIter - 1 do
     begin
          inc(fNumIter);
          
          // copy data
          data.Assign(X);
          data.SubVecInPlace(fMean, True);
          data.ElementWiseMultInPlace(data);
          weights := data.Sum(True);

          // check for regularization
          // extension to the original algorithm:
          // add beat for regularization and then reduce beta dividing it by 2 in each iteration
          // until divide by 2^10 is met
          if weights.Min < beta then
             weights.AddInPlace( beta );
          weights.ElementwiseFuncInPlace( {$IFDEF FPC}@{$ENDIF}sqrtFunc );

          data := X.Clone;
          for i := 0 to X.Width - 1 do
          begin
               data.SetSubMatrix(i, 0, 1, X.Height);
               data.ElementWiseMultInPlace(weights);
          end;
          data.UseFullMatrix;

          data.SumInPlace(False);
          weights.SumInPlace(False);
          fMean.Assign(data);
          fMean.ScaleInPlace(1/weights.Vec[0]);

          meanSub.Assign(fMean);
          meanSub.SubInPlace(oldMean);

          eps := meanSub.ElementwiseNorm2(True);
          if eps < relTol then
             break;

          oldMean.Assign(fMean);

          if counter < 10 then
             beta := beta/2;
     end;
end;

function TDistance.L1Dist(y: IMatrix): IMatrix;
var dist : IMatrix;
begin
     if not Assigned(fMean) then
        raise Exception.Create('Error call InitAbs first');
     dist := Y.SubVec(fMean, True);
     dist.AbsInPlace;
     dist.SumInPlace(True);

     Result := dist;
end;


class function TDistance.L1Reg(x, y: IMatrix): IMatrix;
begin
     Result := L1Reg(x, y, 200, 1e-6, 10);
end;

class function TDistance.L1Reg(x, y: IMatrix; maxIter: integer; relTol,
  tau: double): IMatrix;
begin
     with TDistance.Create do
     try
        InitL1DistReg(X, tau, maxIter, relTol);
        Result := L1Dist(Y);
     finally
            Free;
     end;
end;

class function TDistance.L1(x, y: IMatrix): IMatrix;
begin
     with TDistance.Create do
     try
        InitL1Dist(X);
        Result := L1Dist(Y);
     finally
            Free;
     end;
end;


class function TDistance.L1(x, y: IMatrix; maxIter: integer; relTol,
  beta: double): IMatrix;
begin
     with TDistance.Create do
     try
        InitL1Dist(X, maxIter, relTol, beta);
        Result := L1Dist(Y);
     finally
            Free;
     end;
end;

procedure TDistance.Init(aMean: IMatrix);
begin
     fMean := aMean.Clone;
     fR := nil;
     fRx := 0;
end;

procedure TDistance.Init(aMean, aCov: IMatrix);
begin
     if aCov.Cholesky(fR) <> crOk then
        raise Exception.Create('Cannot init mahalanobis');
     fMean := aMean.Clone;
     fRx := -1;
end;

// ######################################################################
// #### persistence functionality
// ######################################################################

const cDistIdentifier = 'Distance';
      cDistR = 'R';
      cDistRx = 'Rx';
      cDistMean = 'DistMean';

class function TDistance.ClassIdentifier: String;
begin
     Result := cDistIdentifier;
end;

procedure TDistance.DefineProps;
begin
     if Assigned(fR) then
     begin
          AddObject(cDistR, fR.GetObjRef);
          AddIntProperty(cDistRx, fRx);
     end;

     if Assigned(fMean) then
        AddObject(cDistMean, fMean.GetObjRef);
end;

function TDistance.PropTypeOfName(const Name: string): TPropType;
begin
     if SameText(Name, cDistR) or SameText(Name, cDistMean)
     then
         Result := ptObject
     else if SameText(Name, cDistRx)
     then
         Result := ptInteger
     else
         Result := inherited PropTypeOfName(Name);
end;

function TDistance.OnLoadObject(const Name: String;
  Obj: TBaseMathPersistence): boolean;
begin
     Result := True;
     if SameText(Name, cDistR)
     then
         fR := (obj as TDoubleMatrix) as IMatrix
     else if SameText(Name, cDistMean)
     then
         fMean := (obj as TDoubleMatrix) as IMatrix
     else
         Result := inherited OnLoadObject(Name, Obj);
end;

procedure TDistance.OnLoadIntProperty(const Name: String; Value: integer);
begin
     if SameText(Name, cDistRx)
     then
         fRx := Value
     else
         inherited;
end;

procedure TDistance.InitL1DistReg(X: IMatrix; tau: double; maxIter: integer;
  relTol: double);
var eps : double;
    counter : integer;
    data : IMatrix;
    dataP1 : IMatrix;
    i : integer;
    newMean : IMatrix;
    meanSub : IMatrix;
    denom : IMatrix;
    sumDenom : double;
    row : IMatrix;
    tmp : IMatrix;
procedure switchMtx(var x1, x2 : IMatrix);
var tmp : IMatrix;
begin
     tmp := x1;
     x1 := x2;
     x2 := tmp;
end;
begin
     // regularized geometric median
     fMean := X.Mean(False);
     fNumIter := 0;

     newMean := fMean.Clone;
     data := MatrixClass.Create;
     dataP1 := MatrixClass.Create(x.Width, x.Height);
     data.Assign(X);
     row := MatrixClass.Create(X.Width, 1);
     meanSub := MatrixClass.Create;
     denom := MatrixClass.Create;
     tmp := MatrixClass.Create( 1, x.Height );

     for counter := 0 to maxIter - 1 do
     begin
          inc(fNumIter);

          // 1 + ||ul - Y*e_j||
          denom.Assign(X);
          denom.SubVecInPlace(fMean, True);
          denom.ElementWiseMultInPlace(denom);
          denom.SumInPlace(True, True);
          denom.SQRTInPlace;
          denom.ScaleAndAddInPlace(1, tau);

          tmp.UseFullMatrix;
          tmp.SetValue( -Tau );
          tmp.ElementWiseDivInPlace(denom);
          tmp.SumInPlace(False, True);
          sumDenom := 1/(tmp.Vec[0]);

          // calculate new D = (D*e_j + tau*(u - X*e_j))/(1 + tau*||u - X*e_j]]_l2);
          // e_j is the Kroneker Delta function (is 0 except for j e_j=1)
          for i := 0 to X.Height - 1 do
          begin
               data.SetSubMatrix(0, i, data.Width, 1);
               X.SetSubMatrix(0, i, data.Width, 1);

               dataP1.SetSubMatrix(0, i, data.Width, 1);
               dataP1.SetRow(0, X );
               dataP1.SubInPlace(fMean);
               dataP1.ScaleInPlace(-tau);
               dataP1.AddInplace(data);
               dataP1.ScaleInPlace(1/denom.Vec[i]);
          end;
          data.UseFullMatrix;
          dataP1.UseFullMatrix;
          X.UseFullMatrix;

          // calculate new mean
          // u_new = sum_i=0_n ( (D - tau*Y)*e_j)/(1 + tau*||u - X*e_j]]_l2) ) /
          //         sum_i=0_n( -tau/(1 + tau*||u - X*e_j]]_l2) )
          newMean.SetValue(0);

          for i := 0 to Data.Height - 1 do
          begin
               data.SetSubMatrix(0, i, data.Width, 1);

               row.SetRow(0, X, i);
               row.ScaleInPlace(-tau);
               row.AddInplace(data);
               row.ScaleInPlace(1/denom.Vec[i]);

               newMean.AddInplace(row);
          end;

          newMean.ScaleInPlace(sumDenom);

          meanSub.Assign(fMean);
          meanSub.SubInPlace(newMean);

          // fast switch
          switchMtx(newMean, fMean);
          switchMtx(data, dataP1);

          // check for exit criteria
          eps := meanSub.ElementwiseNorm2(True);
          if eps < relTol then
             break;
     end;
end;


class function TDistance.EuclidPairDist(X: TDoubleMatrix): TDoubleMatrix;
var y1, y2 : Integer;
    h : integer;
    p1, p2 : PDouble;
    destLine : IMatrix;
    idx : integer;
begin
     Result := DefMatrixClass.Create(  X.Height*(X.Height - 1) div 2, 1);
     destLine := TDoubleMatrix.Create( X.Width, 1 );

     h := X.Height;
     idx := 0;
     for y1 := 0 to h - 2 do
     begin
          X.SetSubMatrix(0, y1, X.Width, 1);
          p1 := X.StartElement;
          for y2 := y1 + 1 to h - 1 do
          begin
               X.SetSubMatrix(0, y2, X.Width, 1);
               p2 := X.StartElement;

               MatrixSub( destLine.StartElement, destLine.LineWidth, p1, p2, X.Width, 1, X.LineWidth, X.LineWidth);
               Result.Vec[idx] := destLine.ElementwiseNorm2( True );

               inc(idx);
          end;
     end;

     X.UseFullMatrix;
end;


class function TDistance.NormEuclidPairDist(X: TDoubleMatrix): TDoubleMatrix;
var varX : IMatrix;
    y1, y2 : Integer;
    h : integer;
    p1, p2 : PDouble;
    destLine : IMatrix;
    idx : integer;
begin
     varX := X.Variance(False, True);
     for y1 := 0 to VarX.VecLen - 1 do
         varX.Vec[y1] := 1/varX.Vec[y1];

     Result := DefMatrixClass.Create(  X.Height*(X.Height - 1) div 2, 1);
     destLine := TDoubleMatrix.Create( X.Width, 1 );

     h := X.Height;
     idx := 0;
     for y1 := 0 to h - 2 do
     begin
          X.SetSubMatrix(0, y1, X.Width, 1);
          p1 := X.StartElement;
          for y2 := y1 + 1 to h - 1 do
          begin
               X.SetSubMatrix(0, y2, X.Width, 1);
               p2 := X.StartElement;

               MatrixSub( destLine.StartElement, destLine.LineWidth, p1, p2, X.Width, 1, X.LineWidth, X.LineWidth);
               destLine.ElementWiseMultInPlace(destLine);
               destLine.ElementWiseMultInPlace( varX );
               destLine.SumInPlace(True, True);
               Result.Vec[idx] := sqrt(destLine[0, 0]);

               destLine.UseFullMatrix;
               inc(idx);
          end;
     end;

     X.UseFullMatrix;
end;

class function TDistance.AbsPairDist(X: TDoubleMatrix): TDoubleMatrix;
var y1, y2 : Integer;
    h : integer;
    p1, p2 : PDouble;
    destLine : IMatrix;
    sumVal : double;
    idx : integer;
begin
     Result := DefMatrixClass.Create( X.Height*(X.Height - 1) div 2, 1);
     destLine := TDoubleMatrix.Create( X.Width, 1 );

     h := X.Height;
     idx := 0;
     for y1 := 0 to h - 2 do
     begin
          X.SetSubMatrix(0, y1, X.Width, 1);
          p1 := X.StartElement;
          for y2 := y1 + 1 to h - 1 do
          begin
               X.SetSubMatrix(0, y2, X.Width, 1);
               p2 := X.StartElement;

               MatrixSub( destLine.StartElement, destLine.LineWidth, p1, p2, X.Width, 1, X.LineWidth, X.LineWidth);
               destLine.AbsInPlace;
               MatrixSum( @sumVal, sizeof(double), destLine.StartElement, destLine.LineWidth, destLine.Width, 1, True );

               Result.Vec[idx] := sumVal;
               inc(idx);
          end;
     end;
     X.UseFullMatrix;
end;

class function TDistance.MinkowskyPairDist(X: TDoubleMatrix; exponent : Double = 2): TDoubleMatrix;
var y1, y2 : Integer;
    h : integer;
    p1, p2 : PDouble;
    destLine : IMatrix;
    sumVal : double;
    idx : integer;
    pDest : PConstDoubleArr;
    i : Integer;
begin
     Result := DefMatrixClass.Create( X.Height*(X.Height - 1) div 2, 1);
     destLine := TDoubleMatrix.Create( X.Width, 1 );

     h := X.Height;
     idx := 0;
     for y1 := 0 to h - 2 do
     begin
          X.SetSubMatrix(0, y1, X.Width, 1);
          p1 := X.StartElement;
          for y2 := y1 + 1 to h - 1 do
          begin
               X.SetSubMatrix(0, y2, X.Width, 1);
               p2 := X.StartElement;

               pDest := PConstDoubleArr( destLine.StartElement );
               MatrixSub( PDouble(pDest), destLine.LineWidth, p1, p2, X.Width, 1, X.LineWidth, X.LineWidth);

               for i := 0 to destLine.Width - 1 do
                   pDest^[i] := Power(abs(pDest^[i]), exponent);

               MatrixSum( @sumVal, sizeof(double), destLine.StartElement, destLine.LineWidth, destLine.Width, 1, True );

               Result.Vec[idx] := sumVal;
               inc(idx);
          end;
     end;
     X.UseFullMatrix;
end;

class function TDistance.MahalanobisPairDist(X: TDoubleMatrix): TDoubleMatrix;
// from https://stats.stackexchange.com/questions/65705/pairwise-mahalanobis-distances
var y1, y2 : Integer;
    h : integer;
    p1, p2 : PDouble;
    destLine : IMatrix;
    idx : integer;
    help : IMatrix;
    Q : IMatrix;
    i : integer;
    chol : IMatrix;
    pQ : PDouble;
    pX : PDouble;
begin
     // ###########################################
     // #### Init mahalonobis: 
     help := TCorrelation.Covariance(X);
     if help.Cholesky( chol ) <> crOk then
        raise Exception.Create('Cannot calculate Mahalanobis distance');
        
     Q := TDoubleMatrix.Create( chol.Width, X.Height );

     for i := 0 to x.Height - 1 do
     begin
          Q.SetSubMatrix(0, i, Q.Width, 1);
          X.SetSubMatrix(0, i, X.Width, 1);

          pQ := Q.StartElement;
          pX := X.StartElement;

          MatrixCholeskyBackSolve( chol.StartElement, chol.LineWidth, chol.Width, pX, sizeof(double), pQ, sizeof(double) );
     end;

     Q.UseFullMatrix;
     X.UseFullMatrix;


     Result := DefMatrixClass.Create( X.Height*(X.Height - 1) div 2, 1);
     destLine := TDoubleMatrix.Create( X.Width, 1 );

     // ###########################################
     // #### Pairwise eucledian distance from here:
     h := Q.Height;
     idx := 0;
     for y1 := 0 to h - 2 do
     begin
          Q.SetSubMatrix(0, y1, Q.Width, 1);
          p1 := Q.StartElement;
          for y2 := y1 + 1 to h - 1 do
          begin
               Q.SetSubMatrix(0, y2, Q.Width, 1);
               p2 := Q.StartElement;

               MatrixSub( destLine.StartElement, destLine.LineWidth, p1, p2, Q.Width, 1, Q.LineWidth, Q.LineWidth);
               Result.Vec[idx] := destLine.ElementwiseNorm2(True);

               inc(idx);
          end;
     end;

     X.UseFullMatrix;
end;

class function TDistance.ChebychevPairDist(X: TDoubleMatrix): TDoubleMatrix;
var y1, y2 : Integer;
    h : integer;
    p1, p2 : PDouble;
    destLine : IMatrix;
    idx : integer;
begin
     Result := DefMatrixClass.Create( X.Height*(X.Height - 1) div 2, 1);
     destLine := TDoubleMatrix.Create( X.Width, 1 );

     h := X.Height;
     idx := 0;
     for y1 := 0 to h - 2 do
     begin
          X.SetSubMatrix(0, y1, X.Width, 1);
          p1 := X.StartElement;
          for y2 := y1 + 1 to h - 1 do
          begin
               X.SetSubMatrix(0, y2, X.Width, 1);
               p2 := X.StartElement;

               MatrixSub( destLine.StartElement, destLine.LineWidth, p1, p2, X.Width, 1, X.LineWidth, X.LineWidth);
               destLine.AbsInPlace;
               Result.Vec[idx] := destLine.Max;
               inc(idx);
          end;
     end;
     X.UseFullMatrix;
end;


class function TDistance.EuclidPairDist(X: IMatrix): IMatrix;
begin
     Result := EuclidPairDist(x.GetObjRef);
end;

class function TDistance.NormEuclidPairDist(X: IMatrix): IMatrix;
begin
     Result := NormEuclidPairDist(X.GetObjRef);
end;

class function TDistance.AbsPairDist(X: IMatrix): IMatrix;
begin
     Result := AbsPairDist(X.GetObjRef);
end;

class function TDistance.MinkowskyPairDist(X: IMatrix;
  exponent: Double): IMatrix;
begin
     Result := MinkowskyPairDist(X.GetObjRef, exponent);
end;

class function TDistance.ChebychevPairDist(X: IMatrix): IMatrix;
begin
     Result := ChebychevPairDist(X.GetObjRef);
end;

class function TDistance.MahalanobisPairDist(X: IMatrix): IMatrix;
begin
     Result := MahalanobisPairDist(X.GetObjRef); 
end;

initialization
   RegisterMathIO(TDistance);

end.
