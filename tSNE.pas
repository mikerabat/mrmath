// ###################################################################
// #### This file is part of the mathematics library project, and is
// #### offered under the licence agreement described on
// #### http://www.mrsoft.org/
// ####
// #### Copyright:(c) 2019, Michael R. . All rights reserved.
// ####
// #### Unless required by applicable law or agreed to in writing, software
// #### distributed under the License is distributed on an "AS IS" BASIS,
// #### WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// #### See the License for the specific language governing permissions and
// #### limitations under the License.
// ###################################################################

unit tSNE;

interface

uses Matrix, Types;

// this unit is based on the matlab implementation on
// https://lvdmaaten.github.io/tsne/

// t-Distributed Stochastic Neighbor Embedding:
//
type
  TtSNE = class(TMatrixClass)
  private
    fPerplexity : Integer;
    fInitDims : integer;
    fTol : Double;

    // internal states
    fBeta : double;
    fy_grads, fyincs : IMatrix;
    fQ : IMatrix;
    fMinGain : double;

    procedure UpdateGains(var Value : double; const data : PDouble; LineWidth : integer; x, y : integer);
    procedure HBetaExp( var value : double );
    procedure EntropyFuncQ( var Value : double; const data : PDouble; LineWidth : integer; x, y : integer );

    function NormalizeInput( X : TDoubleMatrix ) : IMatrix;
    function ApplyPCA( X : IMatrix ) : IMatrix;
    function PairwiseDist( X : IMatrix ) : IMatrix;
    function D2P( D : IMatrix ) : IMatrix;
    procedure HBeta( D : IMatrix; beta : double; var H : double; var P : IMatrix );
    function tsne_p( P : IMatrix; numDims : integer; labels : TIntegerDynArray ) : IMatrix;
  public
    function SymTSNE(X : TDoubleMatrix; classLbl : TIntegerDynArray; numDims : integer = 2) : TDoubleMatrix; overload;
    constructor Create( initDims : integer = 30; perplexity : integer = 30);

    class function SymTSNE(X : TDoubleMatrix; classLbl : TIntegerDynArray; numDims : integer;
                           initDims : integer; perplexity : integer) : TDoubleMatrix; overload;
  end;

implementation

uses SysUtils, PCA, MatrixConst, Math, Windows, RandomEng, Classes;

{ TtSNE }

constructor TtSNE.Create(initDims, perplexity: integer);
begin
     inherited Create;

     fTol := 1e-5;
     fInitDims := initDims;
     fPerplexity := perplexity;
end;

class function TtSNE.SymTSNE(X: TDoubleMatrix; classLbl: TIntegerDynArray;  numDims : integer;
  initDims, perplexity: integer): TDoubleMatrix;
begin
     with TtSNE.Create(initDims, perplexity) do
     try
        Result := SymTSNE( X, classLbl, numDims );
     finally
            Free;
     end;
end;

function TtSNE.NormalizeInput(X: TDoubleMatrix): IMatrix;
var maxVal : double;
    meanVec : IMatrix;
begin
     Result := X.Add( -X.Min );
     maxVal := Result.Max;
     if maxVal = 0 then
        raise Exception.Create('Error normalizing dataset - empty matrix');

     Result.ScaleInPlace(1/maxVal);
     meanVec := Result.Mean(False);
     Result.SubVecInPlace(meanVec, True);
end;

function TtSNE.SymTSNE(X: TDoubleMatrix;
  classLbl: TIntegerDynArray; numDims : integer = 2): TDoubleMatrix;
var xs : IMatrix;
    D,P : IMatrix;
    yData : IMatrix;
begin
     xs := NormalizeInput(X);
     xs := ApplyPCA(xs);

     D := PairwiseDist(xs);

     // Joint probabilities
     P := D2P(D);
     D := nil;

     // Run t-sne
     yData := tsne_p(P, numDims, classLbl);

     // handle the reference counting
     Result := TDoubleMatrix.Create;
     Result.TakeOver(yData.GetObjRef);

     fQ := nil;
     fy_grads := nil;
     fyincs := nil;
end;

function TtSNE.ApplyPCA(X: IMatrix): IMatrix;
var aPca : TMatrixPCA;
begin
     // apply pca and check if we can shrink it to the designated number of eigenvectors
     // note the pca routine assumes one example in one colum - the original tsne implementation
     // assumes one example in one row -> so transpose in beforehand
     aPca := TMatrixPCA.Create( [pcaTransposedEigVec] );
     try
        Result := X.Transpose;
        aPca.PCA(Result.GetObjRef, fInitDims, False);

        Result.SubVecInPlace(aPca.Mean, False);

        Result := aPca.EigVecsT.Mult( Result );
        Result.TransposeInPlace;
     finally
            aPca.Free;
     end;
end;

function TtSNE.PairwiseDist(X: IMatrix): IMatrix;
var sum_x : IMatrix;
begin
     sum_x := X.ElementWiseMult(X);
     sum_x.SumInPlace(True);

     Result := X.MultT2(X);
     Result.ScaleInPlace(-2);

     sum_x.TransposeInPlace;
     Result.AddVecInPlace(sum_x, True);

     sum_x.TransposeInPlace;
     Result.AddVecInPlace(sum_x, False);
end;

function TtSNE.D2P(D: IMatrix): IMatrix;
var i : TASMNativeInt;
    numPts : TASMNativeInt;
    betaMin, betaMax : double;
    beta : TDoubleDynArray;
    subVec : IMatrix;
    thisP : IMatrix;
    tries : integer;
    H, logU : double;
    hDiff : double;
    P : IMatrix;
procedure FillSubVec( subVec, D : IMatrix; i : integer);
var idx : integer;
    ii : integer;
begin
     idx := 0;
     for ii := 0 to D.Width - 1 do
     begin
          if ii <> i then
          begin
               subVec.Vec[idx] := D[ii, i];
               inc(idx);
          end;
     end;
end;
procedure FillP(P, subVec : IMatrix; i : integer);
var idx : integer;
    ii : integer;
begin
     idx := 0;
     for ii := 0 to D.Width - 1 do
     begin
          if ii <> i then
          begin
               P[ii, i] := subVec.Vec[idx];
               inc(idx);
          end;
     end;
end;

begin
     numPts := d.Height;
     SetLength(beta, numPts);
     for i := 0 to Length(beta) - 1 do
         beta[i] := 1;
     P := MatrixClass.Create(numPts, numPts, 0);
     logU := Ln(fPerplexity);

     // go through all datapoints
     subVec := MatrixClass.Create( D.Width - 1, 1 );

     for i := 0 to numPts - 1 do
     begin
          // Set minimum and maximum values for precision
          betaMin := -Infinity;
          betaMax := Infinity;

          // Compute the Gaussian kernel and entropy for the current precision

          // get vector
          FillSubVec(subVec, D, i );
          HBeta( subVec, beta[i], H, thisP );

          // Evaluate whether the perplexity is within tolerance
          Hdiff := H - logU;
          tries := 0;
          while (abs(Hdiff) > fTol) and (tries < 50) do
          begin
               // If not, increase or decrease precision
               if Hdiff > 0 then
               begin
                    betaMin := beta[i];
                    if IsInfinite(betaMax)
                    then
                        beta[i] := beta[i]*2
                    else
                        beta[i] := (beta[i] + betamax)/2;
               end
               else
               begin
                    betaMax := beta[i];
                    if isInfinite(betaMin)
                    then
                        beta[i] := beta[i]/2
                    else
                        beta[i] := (beta[i] + betamin)/2;
               end;

               // Recompute the values
               HBeta( subVec, beta[i], H, thisP );

               // Evaluate whether the perplexity is within tolerance
               Hdiff := H - logU;
               inc(tries);
          end;

          // Set the final row of P
          FillP(P, thisP, i);
     end;

     Result := P;
end;



// Function that computes the Gaussian kernel values given a vector of
// squared Euclidean distances, and the precision of the Gaussian kernel.
// The function also computes the perplexity of the distribution.

//function [H, P] = Hbeta(D, beta)
//    P = exp(-D * beta);
//    sumP = sum(P);
//    H = log(sumP) + beta * sum(D .* P) / sumP;
//    % why not: H = exp(-sum(P(P > 1e-5) .* log(P(P > 1e-5)))); ???
//    P = P / sumP;
//end

procedure TtSNE.HBeta(D: IMatrix; beta: double; var H : double; var P: IMatrix);
var sumP : double;
    muldp : IMatrix;
begin
     fBeta := beta;
     P := D.ElementwiseFunc( {$IFDEF FPC}@{$ENDIF}HBetaExp );
     sumP := (P.Sum(True) as IMatrix).Vec[0];
     muldp := D.ElementWiseMult(P);
     H := ln(sumP) + beta*(muldp.Sum(True) as IMatrix).Vec[0]/sumP;
     P.ScaleInPlace(1/sumP);
end;

procedure TtSNE.HBetaExp(var value: double);
begin
     value := exp( -value*fBeta );
end;

procedure EntropyFunc(var value : double);
begin
     if SameValue(value, 0, 1e-12)
     then
         value := 0
     else
         value := value*ln(value);
end;

procedure MaxFunc(var value : double);
begin
     value := Max(value, 2.2250738585072013830902327173324e-308); // don't really be sure why this is...
end;

function TtSNE.tsne_p(P: IMatrix; numDims : integer; labels: TIntegerDynArray): IMatrix;
var n : integer;                        // number of instances
    momentum : double;                  // initial momentum
    epsilon : double;
    scaleFact : double;
    i : integer;
    PT : IMatrix;
    constEntr : double;
    yData : IMatrix;
    gains : IMatrix;
    iter: Integer;
    sum_ydata : IMatrix;
    tmp : IMatrix;
    num : IMatrix;
    L, Q : IMatrix;
    cost : double;
const final_momentum : double = 0.8;    // value to which momentum is changed
      maxIter : integer = 1000;         // maximum number of iterations
      mom_switch_iter : integer = 250;  // iteration at which momentum is changed
      stop_lying_iter : integer = 100;   // iteration at which lying about P-values is stopped
      min_gain : double = 0.01;         // minimum gain for delta-bar-delta

begin
     MatrixToTxtFile('D:\P.txt', P.GetObjRef);
     WriteBinary('D:\P_bin.txt', P);

     n := P.Height;
     momentum := 0.5;
     epsilon := 500;
     fMinGain := min_gain;

     // ################################################
     // #### make sure P is what we expect it to be:
     // clear diagonal
     for i := 0 to P.Width - 1 do
         p[i, i] := 0;
     PT := P.Transpose;
     P.AddInPlace( PT );
     P.ScaleInPlace(0.5);

     PT := P.Sum(True);
     PT.SumInPlace(False, True);
     scaleFact := PT[0, 0];
     P.ScaleInPlace(1/scaleFact);
     P.ElementwiseFuncInPlace({$ifdef FPC}@{$ENDIF}MaxFunc);
     
     PT := P.AsVector(True);
     PT.ElementwiseFuncInPlace( {$IFDEF FPC}@{$ENDIF}EntropyFunc );
     PT.SumInPlace(True, True);
     
     constEntr := PT[0,0];
     P.ScaleInPlace(4);  // lie about the p-vals to find better local minima


     //WriteBinary('D:\P_pre_bin.txt', P);

     yData := MatrixClass.CreateRand( numDims, n, raMersenneTwister, 471 );
     yData.ScaleInPlace(0.0001);

     //MatrixToTxtFile('D:\ydatainit.txt', yData.GetObjRef, 16);
     //WriteBinary('D:\ydatainit_bin.txt', yData);

     fyincs := MatrixClass.Create(yData.Width, yData.Height );
     gains := MatrixClass.Create(yData.Width, yData.Height, 1);

     // #################################################
     // #### now iterate
     for iter := 1 to maxIter do
     begin
      //    MatrixToTxtFile('D:\ydata_' + IntToStr(iter) + '.txt', yData.GetObjRef, 16);

          //OutputDebugString( PChar(Format('%d: %.5f', [iter, ydata[0, 0] ]) ));
          //Add(Format('%.8f', [ydata[0, 0] ]) );
          // Compute joint probability that point i and j are neighbors
          sum_ydata := ydata.ElementWiseMult(yData);
          sum_ydata.SumInPlace(True, True);

     //     WriteBinary('D:\sum_ydata_bin.txt', sum_ydata);

    //      MatrixToTxtFile('D:\sumydata_' + IntToStr(iter) + '.txt', sum_ydata.GetObjRef, 16);

          // num = 1 ./ (1 + bsxfun(@plus, sum_ydata, bsxfun(@plus, sum_ydata', -2 * (ydata * ydata')))); % Student-t distribution
          tmp := yData.MultT2(yData);
          tmp.ScaleInPlace(-2);
          tmp.AddVecInPlace(sum_ydata, True);
          tmp.AddVecInPlace(sum_ydata, False);
          tmp.AddInPlace( 1 );

          num := MatrixClass.Create( tmp.Width, tmp.Height, 1);
          num.ElementWiseDivInPlace(tmp);

          // set diagonal to zero
          for i := 0 to num.Width - 1 do
              num[i, i] := 0;

       //   WriteBinary('D:\num_bin.txt', num);

       //   MatrixToTxtFile('D:\num_' + IntToStr(iter) + '.txt', num.GetObjRef, 16);

          tmp := num.AsVector(False);

          tmp.SumInPlace(True, False);

        //  WriteBinary('D:\tmp.txt', tmp);

          Q := num.Scale( 1/tmp[0, 0] );
          Q.ElementwiseFuncInPlace({$ifdef FPC}@{$ENDIF}MaxFunc ); // really??

          //WriteBinary('D:\q_bin.txt', Q);

          // Compute the gradients (faster implementation)
          L := P.Sub(Q);

          //WriteBinary('D:\L_one.txt', L);

          L.ElementWiseMultInPlace(num);

          //WriteBinary('D:\L_bin.txt', L);

          fy_grads := L.Sum(False);
          fy_grads.DiagInPlace(True);
          fy_grads.SubInPlace(L);

          //WriteBinary('D:\diag_L.txt', fy_grads);

          fy_grads.ScaleInPlace(4);
          fy_grads.MultInPlace(ydata);

          //WriteBinary('D:\ygrads.txt', fy_grads);

          // Update the solution
          gains.ElementwiseFuncInPlace( {$ifdef FPC}@{$ENDIF}UpdateGains );

          fyincs.ScaleInPlace(momentum);
          tmp := gains.ElementWiseMult(fy_grads);
          tmp.ScaleInPlace(epsilon);
          fyincs.SubInPlace(tmp);

          //WriteBinary('D:\yincs.txt', fyincs);

          yData.AddInplace(fyincs);

          //WriteBinary('D:\ydata_1.txt', yData);

          tmp := ydata.Mean( False );
          yData.SubVecInPlace( tmp, True );

          //WriteBinary('D:\ydata_2.txt', yData);

          //Update the momentum if necessary
          if iter = mom_switch_iter then
             momentum := final_momentum;

          if iter = stop_lying_iter then
             P.ScaleInPlace(1/4);

//
//          MatrixToTxtFile('D:\P_' + IntToStr(iter) + '.txt', P.GetObjRef, 16);
//          MatrixToTxtFile('D:\yincs_' + IntToStr(iter) + '.txt', fyincs.GetObjRef, 16);
//          MatrixToTxtFile('D:\ygrads_' + IntToStr(iter) + '.txt', fy_grads.GetObjRef, 16);
//          MatrixToTxtFile('D:\gains_' + IntToStr(iter) + '.txt', gains.GetObjRef, 16);
//          MatrixToTxtFile('D:\q_' + IntToStr(iter) + '.txt', q.GetObjRef, 16);


          // progress:
          if iter mod 10 = 9 then
          begin
               fQ := Q;
               PT := P.ElementwiseFunc( {$IFDEF FPC}@{$ENDIF}EntropyFuncQ );
               PT.SumInPlace(True, True);
               PT.SumInPlace(False, True);

               cost := constEntr - PT[0,0];
               OutputDebugString( PChar( Format( 'Iteration %d: error is %.4f', [iter, cost])) );
          end;
     end;

     Result := yData;
end;

procedure TtSNE.UpdateGains(var Value: double; const data: PDouble; LineWidth,
  x, y: integer);
begin
     if sign( fy_grads[x, y] ) = sign( fyincs[x, y] )
     then
         value := 0.8*value
     else
         value := value + 0.2;

     value := max(value, fminGain)
end;

procedure TtSNE.EntropyFuncQ(var Value: double; const data: PDouble; LineWidth,
  x, y: integer);
begin
     if not SameValue( fQ[x, y], 1e-20 )
     then
         value := Value*ln(fQ[x, y])
     else
         value := 0;
end;

end.
