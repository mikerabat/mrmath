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

unit EM;

interface

uses Matrix;

//  EM algorithm for k multidimensional Gaussian mixture estimation
type
  TExpectationMax = class(TMatrixClass)
  private
    fNumIter : integer;
    fltol : double;
    fW, fM : IMatrix;
    fV : IMatrixDynArr;
    fk : Integer;
    fX : IMatrix;
    fS, fU : IMatrix;

    procedure InitEM;

    // ###########################################
    // #### EM steps
    function Likelihood: double;
    function Expectation : IMatrix;
    procedure Maximization( E : IMatrix );
  public
    // estimate weights matrix W, mean vectors M and covariance matrix V on
    // input data X. A data point is presented column wise.
    // k states the number of components.
    // returns false if search excceeds the maximum number of iterations
    function Estimate(X : IMatrix; k : integer; var W, M : IMatrix; var V : IMatrixDynArr) : boolean;

    // initialize search with these values
    procedure Init( W, M : IMatrix; V : IMatrixDynArr );

    // n: max number of iterations; ltol : percentage of the log likelihood difference between 2 iterations
    constructor Create( n : integer = 1000; ltol : double = 0.1);
  end;

implementation

uses
  Types, RandomEng, Math, Corr, MatrixConst;

{ TExpectationMax }

constructor TExpectationMax.Create(n: integer; ltol: double);
begin
     fNumIter := n;
     fltol := ltol;

     inherited Create;
end;

function TExpectationMax.Estimate(X : IMatrix; k : integer; var W, M : IMatrix; var V : IMatrixDynArr) : boolean;
var ln, lo : double;
    iter : Integer;
    E : IMatrix;
begin
     fX := X;
     fk := k;

     fU := fX.Mean(False);
     fU.TransposeInPlace;

     fS := TCorrelation.Covariance(fX);
     
     // ###########################################
     // #### initialize
     if not Assigned(fW) or not Assigned(fM) or not Assigned(fW) then
        InitEM;

     // ###########################################
     // #### Expectation maximization 
     ln := Likelihood;
     lo := 2*ln;

     iter := 0;
     while (abs(100*(ln-lo)/lo)>fltol) and (iter < fNumIter) do
     begin
          // E - Step:
          E := Expectation;

          // M - Step
          Maximization(E);
          lo := ln;
          ln := Likelihood;
          inc(iter);
     end;

     Result := iter < fNumIter;

     if Result then
     begin
          W := fW;
          M := fM;
          V := fV;
     end;
end;

function TExpectationMax.Expectation: IMatrix;
var a : double;
    s : IMatrix;
    iV : IMatrixDynArr;
    i, j : Integer;
    d : integer;
    dXM : IMatrix;
    tmp : IMatrix;
    p1 : double;
begin
     fX.UseFullMatrix;
     d := fX.Width;
     a := power(2*pi, 0.5*d);

     s := MatrixClass.Create( fK, 1 );
     SetLength(iV, Length(fV));

     for j := 0 to fK - 1 do
     begin
          if Abs( fV[j].Max - fV[j].Min ) < 2*cDefEpsilon then
          begin
               fV[j] := MatrixClass.CreateEye(d);
               fV[j].ScaleInPlace( cDefEpsilon );
          end;

          s.Vec[j] := sqrt( fV[j].Determinant );
          iV[j] := fV[j].Invert;
     end;

     Result := MatrixClass.Create( fK, fX.Height );
     for i := 0 to fX.Height - 1 do
     begin
          for j := 0 to fK - 1 do
          begin
               fX.SetSubMatrix(0, i, fX.Width, 1);
               dXM := fX.Transpose;
               fM.SetSubMatrix(j, 0, 1, fM.Height);
               dXM.SubInPlace(fM);

               tmp := iV[j].Mult( dXM );
               dXM.TransposeInPlace;
               dXM.MultInPlace(tmp);

               p1 := exp(-0.5*dXM.Vec[0])/(a*S.Vec[j]);
               Result[j, i] := fW.Vec[j]*p1;
          end;

          Result.SetSubMatrix(0, i, Result.Width, 1);
          tmp := Result.Sum(True);
          Result.ScaleInPlace(1/tmp.Vec[0]);
          Result.UseFullMatrix;
     end;
end;

procedure TExpectationMax.Init(W, M : IMatrix; V : IMatrixDynArr);
var i : integer;
begin
     fW := nil;
     fM := nil;
     fV := nil;

     if Assigned(W) then
        fW := W.Clone;
     if Assigned(M) then
        fM := M.Clone;
     if Assigned(V) then
     begin
          SetLength(fV, Length(V));
          for i := 0 to Length(V) - 1 do
              fV[i] := V[i].Clone;
     end;
end;

procedure TExpectationMax.InitEM;
var i : integer;
    idx : TIntegerDynArray;
    rnd : TRandomGenerator;
    newM : IMatrix;
    centerChanged : double;
    iter : Integer;
    minDist : double;
    minDistIdx : Integer;
    numMVals : TIntegerDynArray;
    mValsIdx : Array of TIntegerDynArray;
    ii: Integer;
    dist : IMatrix;
    dDist : double;
    v : IMatrix;
    lastCenterChanged : double;
    n : integer;
begin
     // random class centers -> create a shuffled random index list and use this as class centers
     rnd := TRandomGenerator.Create(raSystem);
     try
        idx := rnd.RandIndexArr(0, fx.Width - 1);
     finally
            rnd.Free;
     end;

     // take the first k values as class centers
     fM := MatrixClass.Create( fX.Width, fk );

     for i := 0 to fK - 1 do
         fM.SetRow(i, fX, idx[i]);

     n := fX.Height;
     centerChanged := MaxDouble;
     SetLength(numMVals, fK);
     newM := MatrixClass.Create( fX.Width, fk );

     SetLength(mValsIdx, fK);
     for i := 0 to fK - 1 do
         SetLength(mValsIdx[i], fX.Height );

     // maximum of 100 iterations:
     for iter := 0 to 100 - 1 do
     begin
          newM.SetValue(0);
          FillChar(numMVals[0], sizeof(integer)*Length(numMVals), 0);

          //
          for i := 0 to n - 1 do
          begin
               fX.SetSubMatrix(0, i, fX.Width, 1);

               minDist := MaxDouble;
               minDistIdx := -1;

               // find the closes "center"
               for ii := 0 to fk - 1 do
               begin
                    fM.SetSubMatrix(0, ii, fM.Width, 1);
                    dist := fM.Sub( fX );
                    dDist := dist.ElementwiseNorm2;
                    if dDist < minDist then
                    begin
                         minDistIdx := ii;
                         minDist := dDist;
                    end;
               end;

               // found the min dist -> add the index and add the example to the new center class
               assert(minDistIdx >= 0, 'No minimum distance found');
               newM.SetSubMatrix(0, minDistIdx, newM.Width, 1);
               newM.AddInplace(fX);
               mValsIdx[minDistIdx][ numMVals[minDistIdx] ] := i;
               inc( numMVals[minDistIdx] );
          end;

          // update centers
          for i := 0 to fK - 1 do
          begin
               newM.SetSubMatrix(0, i, newM.Width, 1);
               newM.ScaleInPlace(1/numMVals[i]);
          end;

          // check the change
          newM.UseFullMatrix;
          fM.UseFullMatrix;
          lastCenterChanged := (newM.Sub( fM ) as IMatrix).ElementwiseNorm2;

          if lastCenterChanged >= centerChanged*0.99 then
             break;

          fM.Assign(newM);
          centerChanged := lastCenterChanged;
     end;

     // ###########################################
     // #### We have an estimate for the kmeans centers -> estimate V and W
     fM.TransposeInPlace;
     fW := MatrixClass.Create( 1, fK );
     SetLength(fV, fK);

     fX.UseFullMatrix;

     for i := 0 to fK - 1 do
     begin
          fW.Vec[i] := 1/numMVals[i];

          // v[i] contains the covariance matrix of each center
          v := MatrixClass.Create(fX.Width, numMVals[i]);

          for ii := 0 to numMVals[i] - 1 do
              v.SetRow(ii, fX, mValsIdx[i][ii]);

          fV[i] := TCorrelation.Covariance( v );
     end;
end;

function TExpectationMax.Likelihood: double;
var n : integer;
    i : Integer;
    iV : IMatrix;
    A1, A2 : double;
    udiff : IMatrix;
    S : IMatrix;
begin
     n := fX.Height;

     Result := 0;

     for i := 0 to fK - 1 do
     begin
          // V is covariance aka square matrix. So invertion is no problem here:
          iV := fV[i].Invert;

          A1 := -0.5*n*ln(power(2*pi, fV[i].Width) * fV[i].Determinant);

          fM.SetSubMatrix( i, 0, 1, fM.Height);
          udiff := fU.Sub( fM );

          S := iV.Mult(fS);
          
          iV.MultInPlace(udiff);
          udiff.TransposeInPlace;
          udiff.MultInPlace(iV);

          A2 := -0.5*(n - 1)*( S.Trace + udiff.Vec[0] );
          
          Result := Result + fW.Vec[i]*( A1 + A2 );
     end;

     fM.UseFullMatrix;
end;

procedure TExpectationMax.Maximization(E: IMatrix);
var d, n : integer;
    i, j: Integer;
    tmp : IMatrix;
    dXM : IMatrix;
begin
     fX.UseFullMatrix;
     d := fX.Width;
     n := fX.Height;

     fW := MatrixClass.Create( fk, 1 );
     fM := MatrixClass.Create( fk, d);
     for i := 0 to fK - 1 do
         fV[i] := MatrixClass.Create( d, d );

     // compute weights
     for i := 0 to fk - 1 do
     begin
          fM.SetSubMatrix(i, 0, 1, fM.Height );
          for j := 0 to n - 1 do
          begin
               fW.Vec[i] := fW.Vec[i] + E[i, j];
               fX.SetSubMatrix(0, j, d, 1);
               tmp := fX.Transpose;
               tmp.ScaleInPlace( E[i, j] );
               
               fM.AddInplace( tmp );
          end;
          fM.ScaleInPlace(1/fW.Vec[i]);
     end;

     for i := 0 to fK - 1 do
     begin
          fM.SetSubMatrix(i, 0, 1, fM.Height);
          for j := 0 to n - 1 do
          begin
               fX.SetSubMatrix(0, j, d, 1);
               dXM := fX.Transpose;
               dXM.SubInPlace( fM );
               tmp := dXM.Transpose;
               dXM.MultInPlace(tmp);
               dXM.ScaleInPlace(E[i , j]); 

               fV[i].AddInplace(dXM);
          end;

          fV[i].ScaleInPlace(1/fW.Vec[i]);
     end;

     fW.ScaleInPlace( 1/n );
     fX.UseFullMatrix;
     fM.UseFullMatrix;
end;

end.
