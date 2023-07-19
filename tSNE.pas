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

uses Matrix, Types, RandomEng, PCA;

// ###################################################################
// this unit is based on the matlab implementation on
// https://lvdmaaten.github.io/tsne/
// ###################################################################

// t-Distributed Stochastic Neighbor Embedding:
type
  TTSNEDistFunc = (dfOrig, dfEuclid, dfNormEuclid, dfAbs, dfMahalanobis);
  TTSNEProgress = procedure(Sender : TObject; iter : integer; cost : double; yMap : IMatrix; var doCancel : boolean) of Object;
  TTSNEPCAInit = function : TMatrixPCA of Object;
  TtSNE = class(TMatrixClass)
  private
    const cK : double = 0.2;
          cPhi : double = 0.8;
  private
    fPerplexity : Integer;
    fInitDims : integer;
    fTol : Double;
    fDistFunc : TTSNEDistFunc;

    // internal states
    fBeta : double;
    fy_grads, fyincs : IMatrix;
    fQ : IMatrix;
    fMinGain : double;
    fNumIter : integer;

    fProgress : TTSNEProgress;
    fInitPCA : TTSNEPCAInit;

    fSeed : integer;
    fRandAlgorithm : TRandomAlgorithm;

    function DefInitPCA : TMatrixPCA;

    procedure UpdateGains(var Value : double; const data : PDouble; LineWidth : integer; x, y : integer);
    procedure HBetaExp( var value : double );
    procedure EntropyFuncQ( var Value : double; const data : PDouble; LineWidth : integer; x, y : integer );

    function NormalizeInput( X : TDoubleMatrix ) : IMatrix;
    function ApplyPCA( X : IMatrix ) : IMatrix;
    function PairwiseDist( X : TDoubleMatrix ) : IMatrix;
    function D2P( D : IMatrix ) : IMatrix;
    procedure HBeta( D : IMatrix; beta : double; var H : double; var P : IMatrix );
    function tsne_p( P : IMatrix; numDims : integer ) : IMatrix;
    function InternalTSNE(xs: TDoubleMatrix; numDims: integer): TDoubleMatrix;
    function PairDist(Xs: TDoubleMatrix): IMatrix;

    function InitRand( w, h : Integer ) : IMatrix;
  public
    property OnProgress : TTSNEProgress read fProgress write fProgress;
    property OnInitPCA : TTSNEPCAInit read fInitPCA write fInitPCA;

    // main function to map X to the number of desired dimensions
    function SymTSNE(X : TDoubleMatrix; numDims : integer = 2) : TDoubleMatrix; overload;

    // use this function if X has been preprocessed "outside" e.g. by applying an
    // incremental pca approach -> the algorithm starts with the pairwise distance calculation
    function SymTSNEPreprocessed( X : TDoubleMatrix; numDims : integer = 2) : TDoubleMatrix;

    constructor Create( initDims : integer = 30; perplexity : integer = 30; numIter : integer = 1000;
                        distFunc : TTSNEDistFunc = dfOrig; randSeed : integer = 0; randAlgorithm : TRandomAlgorithm = raMersenneTwister);

    class function SymTSNE(X : TDoubleMatrix; numDims : integer;
                           initDims : integer; perplexity : integer; numIter : integer = 1000;
                           distFunc : TTSNEDistFunc = dfOrig;
                           randSeed : integer = 0; randAlgorithm : TRandomAlgorithm = raMersenneTwister) : TDoubleMatrix; overload;
  end;

implementation

uses SysUtils, Math, Classes, MatrixASMStubSwitch, MatrixConst,
     Dist, BlockSizeSetup;

{ TtSNE }

constructor TtSNE.Create( initDims : integer = 30; perplexity : integer = 30; 
   numIter : integer = 1000; distFunc : TTSNEDistFunc = dfOrig; randSeed : integer = 0;
   randAlgorithm : TRandomAlgorithm = raMersenneTwister);
begin
     inherited Create;

     fDistFunc := distFunc;
     fNumIter := numIter;
     fTol := 1e-5;
     fInitDims := initDims;
     fPerplexity := perplexity;
     fSeed := RandSeed;
     fRandAlgorithm := randAlgorithm;
end;

class function TtSNE.SymTSNE(X : TDoubleMatrix; numDims : integer;
  initDims : integer; perplexity : integer; numIter : integer = 1000;
  distFunc : TTSNEDistFunc = dfOrig; randSeed : integer = 0;
  randAlgorithm : TRandomAlgorithm = raMersenneTwister) : TDoubleMatrix;
begin
     with TtSNE.Create(initDims, perplexity, numIter, distFunc, randSeed, randAlgorithm) do
     try
        Result := SymTSNE( X, numDims );
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

function TtSNE.SymTSNEPreprocessed(X: TDoubleMatrix;
  numDims: integer): TDoubleMatrix;
begin
     Result := InternalTSNE(X, numDims);
end;

function TtSNE.SymTSNE(X: TDoubleMatrix; numDims : integer = 2): TDoubleMatrix;
var xs : IMatrix;
begin
     xs := NormalizeInput(X);
     xs := ApplyPCA(xs);

     Result := InternalTSNE( xs.GetObjRef, numDims );
end;

function TtSNE.InitRand(w, h: Integer): IMatrix;
begin
     Result := MatrixClass.Create(w, h, True); // prepare for better block mult
     Result.InitRandom(fRandAlgorithm, fSeed);
end;

function TtSNE.InternalTSNE( xs : TDoubleMatrix; numDims : integer) : TDoubleMatrix;
var D, P : IMatrix;
    yData : IMatrix;
begin
     D := PairDist(xs);

     // Joint probabilities
     P := D2P(D);
     D := nil;

     // Run t-sne
     yData := tsne_p(P, numDims);

     // handle the reference counting
     Result := MatrixClass.Create;
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
     if Assigned(fInitPCA)
     then
         aPca := fInitPCA()
     else
         aPca := DefInitPCA();
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

function TtSNE.PairwiseDist(X: TDoubleMatrix): IMatrix;
var sum_x : IMatrix;
begin
     sum_x := X.ElementWiseMult(X);
     sum_x.SumInPlace(True);

     Result := X.MultT2(X);
     Result.ScaleInPlace(-2);
     Result.AddVecInPlace(sum_x, True);
     Result.AddVecInPlace(sum_x, False);
end;

function TtSNE.PairDist(Xs: TDoubleMatrix): IMatrix;
var pDist : IMatrix;
    w : integer;
    idx : integer;
    x, y : integer;
begin
     // ###########################################
     // #### Several distance measurements
     case fDistFunc of
      dfEuclid: pDist := TDistance.EuclidPairDist(Xs);
      dfNormEuclid: pDist := TDistance.NormEuclidPairDist(Xs);
      dfAbs: pDist := TDistance.AbsPairDist(Xs);
      dfMahalanobis: pDist := TDistance.MahalanobisPairDist(Xs);
     else
         // original implementation -> is already square form!
         Result := PairwiseDist(Xs);
         exit;
     end;

     // ########################################
     // #### Create squareform of the pairwise distance vector
     //w := Floor( (1 + sqrt (1 + 8 * pDist.VecLen)) / 2 );
     w := Xs.Height;

     Result := MatrixClass.Create( w, w );

     x := 1;
     y := 0;
     for idx := 0 to pDist.VecLen - 1 do
     begin
          Result[x, y] := pDist.Vec[idx];
          Result[y, x] := pDist.Vec[idx];

          inc(x);
          if x >= w then
          begin
               inc(y);
               x := y + 1;
          end;
     end;

     // Result := Res.*Res
     Result.ElementWiseMultInPlace(Result);
end;

function TtSNE.D2P(D: IMatrix): IMatrix;
var i : NativeInt;
    numPts : NativeInt;
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
     value := value*ln(value);
end;

procedure MaxFunc(var value : double);
var cmpVal : double;
begin
	    // branchless max with a very small error of 1e-308
     cmpVal := Integer(value >= cMinDouble);
     value :=  cmpVal*value + cMinDouble;
end;

function TtSNE.tsne_p(P: IMatrix; numDims : integer): IMatrix;
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
    doCancel : boolean;
    aSum : double;
    aMean : IMatrix;
    yMul : IMatrix;
    tmp2 : IMatrix;
    line1, line2 : PConstDoubleArr;
    y : integer;

const final_momentum : double = 0.8;    // value to which momentum is changed
      mom_switch_iter : integer = 250;  // iteration at which momentum is changed
      stop_lying_iter : integer = 100;   // iteration at which lying about P-values is stopped
      min_gain : double = 0.01;         // minimum gain for delta-bar-delta

begin
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
     MatrixMax(P.StartElement, P.LineWidth, P.Width, P.Height, cMinDouble);

     PT := P.AsVector(True);
     PT.ElementwiseFuncInPlace( {$IFDEF FPC}@{$ENDIF}EntropyFunc );
     PT.SumInPlace(True, True);
     constEntr := PT[0,0];
     P.ScaleInPlace(4);  // lie about the p-vals to find better local minima

     yData := InitRand(numdims, n);
     yData.ScaleInPlace(0.0001);

     fyincs := MatrixClass.Create(yData.Width, yData.Height );
     gains := MatrixClass.Create(yData.Width, yData.Height, 1);

     L := MatrixClass.Create(n, n);
     Q := MatrixClass.Create(n, n);
     fy_grads := MatrixClass.Create(numDims, n);

     sum_ydata := MatrixClass.Create(yData.Width, yData.Height);

     aMean := MatrixClass.Create(numDims, 1);
     yMul := MatrixClass.Create(n, n);
     num :=  MatrixClass.Create( yMul.Width, yMul.Height, 0);

     tmp := MatrixClass.Create(numDims, n);
     tmp2 := MatrixClass.Create(n, n);

     // #################################################
     // #### now iterate
     for iter := 1 to fNumIter do
     begin
          // Compute joint probability that point i and j are neighbors
          sum_ydata.SetWidthHeight(yData.Width, yData.Height);
          sum_ydata.Assign(yData);
          sum_ydata.ElementWiseMultInPlace(sum_ydata);
          //sum_ydata := ydata.ElementWiseMult(yData);
          sum_ydata.SumInPlace(True, True);

          // num = 1 ./ (1 + bsxfun(@plus, sum_ydata, bsxfun(@plus, sum_ydata', -2 * (ydata * ydata')))); % Student-t distribution
          // yMul := yData.MultT2(yData);
          MatrixMultT2Ex(yMul.StartElement, yMul.LineWidth, yData.StartElement, yData.StartElement, yData.Width, yData.Height,
                         yData.Width, yData.Height, yData.LineWidth, yData.LineWidth, BlockMatrixCacheSize, doNone, nil);
          yMul.ScaleInPlace(-2);
          yMul.AddVecInPlace(sum_ydata, True);
          yMul.AddVecInPlace(sum_ydata, False);
          yMul.AddInPlace( 1 );

          num.SetValue(1);
          num.ElementWiseDivInPlace(yMul);

          // set diagonal to zero
          for i := 0 to num.Width - 1 do
              num[i, i] := 0;

          aSum := num.Sum;
          Q.Assign( num );
          Q.ScaleInPlace( 1/aSum );
          //Q.ElementwiseFuncInPlace({$ifdef FPC}@{$ENDIF}MaxFunc );
          MatrixMax(Q.StartElement, Q.LineWidth, Q.Width, Q.Height, cMinDouble);

          // Compute the gradients (faster implementation)
          L.Assign(P);
          L.SubInPlace(Q);
          L.ElementWiseMultInPlace(num);

          // fy_grads := L.Sum(False);
          tmp2.SetValue(0);
          MatrixSum(tmp2.StartElement, tmp2.LineWidth, L.StartElement, L.LineWidth, L.Width, L.Height, False);

          // create diagonal from the given matrix
          line1 := PConstDoubleArr(tmp2.StartElement);
          line2 := line1;
          inc(PByte(line2), tmp2.LineWidth);

          for y := 1 to fy_grads.Height - 1 do
          begin
               line2^[y] := line1^[y];
               line1^[y] := 0;
               inc(PByte(line2), tmp2.LineWidth);
          end;

          //fy_grads.DiagInPlace(True);
          tmp2.SubInPlace(L);
          tmp2.ScaleInPlace(4);
          MatrixMult(fy_grads.StartElement, fy_grads.LineWidth, tmp2.StartElement, ydata.StartElement, tmp2.Width, tmp2.Height,
                     yData.Width, yData.Height, tmp2.LineWidth, ydata.LineWidth);
          //fy_grads := tmp2.Mult(ydata);

          // Update the solution
          gains.ElementwiseFuncInPlace( {$ifdef FPC}@{$ENDIF}UpdateGains );

          fyincs.ScaleInPlace(momentum);
          tmp.Assign(gains);
          tmp.ElementWiseMultInPlace(fy_grads);
          tmp.ScaleInPlace(epsilon);
          fyincs.SubInPlace(tmp);

          yData.AddInplace(fyincs);

          MatrixMean(aMean.StartElement, aMean.LineWidth, yData.StartElement,
                     yData.LineWidth, yData.Width, yData.Height, False);
          yData.SubVecInPlace( aMean, True );

          // Update the momentum if necessary
          if iter = mom_switch_iter then
             momentum := final_momentum;

          if iter = stop_lying_iter then
             P.ScaleInPlace(1/4);

          // progress:
          if Assigned(fProgress) and ( (iter = fNumIter) or (iter mod 10 = 0) ) then
          begin
               fQ := Q;
               PT := P.ElementwiseFunc( {$IFDEF FPC}@{$ENDIF}EntropyFuncQ );
               PT.SumInPlace(True, True);
               PT.SumInPlace(False, True);

               cost := constEntr - PT[0,0];

               doCancel := False;
               fProgress(Self, iter, cost, yData, doCancel);

               if doCancel then
                  break;
          end;
     end;

     Result := yData;
end;

procedure TtSNE.UpdateGains(var Value: double; const data: PDouble; LineWidth,
  x, y: integer);
var cmp1, cmp2 : double;
begin
     // branchless version
     cmp1 := Integer(sign( fy_grads[x, y] ) = sign( fyincs[x, y] ));
     cmp2 := Integer(cmp1 = 0);
     value := cmp1*cPhi*value + cmp2*(value + cK);
     cmp2 := Integer(value > fMinGain);
     cmp1 := Integer(cmp2 = 0);
     value := cmp2*value + cmp1*fMinGain;

     //if sign( fy_grads[x, y] ) = sign( fyincs[x, y] )
//     then
//         value := cPhi*value
//     else
//         value := value + cK;
//
//     value := Math.Max(value, fminGain)
end;

procedure TtSNE.EntropyFuncQ(var Value: double; const data: PDouble; LineWidth,
  x, y: integer);
begin
     value := Value*ln(fQ[x, y]);
end;

function TtSNE.DefInitPCA : TMatrixPCA;
begin
     Result := TMatrixPCA.Create( [pcaTransposedEigVec] );
end;

end.
