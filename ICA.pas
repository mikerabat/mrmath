unit ICA;

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

interface

// fast ica: Perform independent component analysis using Hyvarinen's fixed point
// algorithm

// Independent component analysis using the informax algorithm:

uses SysUtils, Classes, Matrix, MatrixConst, BaseMathPersistence, PCA;

type
  TFastICANonLinEstimator = (iePow3, ieTanh, ieGauss, ieSkew);
  TICAMethod = (imFastICA, imInfomax);
  
type
  TICAProps = record
    method : TICAMethod;
    BlockSize : integer;
    NumIter : integer;
    Annealing : boolean;
    KeepAdditionalData : boolean;

    // fast ica params
    NumIC : integer;
    epsilon : double;
    symApproach : boolean;
    nonLin : TFastICANonLinEstimator;
    //samplesize : double;   // 0 - 1: percentage of sampeles used in one iteration
    a1 : double;           // parameter for tuning tanh
    a2 : double;           // parameter for tuning gaus
    myy : double;          // step size algorithm
    maxFineTune : Integer; // max number of finetuning steps
    stabilization : boolean;
    fineTuning : boolean;
  end;

type
  TMatrixICA = class(TBaseMathPersistence)
  private
    fProps : TICAProps;

    fW : IMatrix;
    fWInv : IMatrix;
    fMeanVec : IMatrix;

    procedure Clear;
    function WhitenData(examples : TDoubleMatrix; out wz : IMatrix) : IMatrix;
    function InvertAndSQRT(mtx: IMatrix): IMatrix;

    procedure LocPow3(var value : double);
    procedure LocTanh(var value : double);
    procedure LocExp(var value : double);
    procedure LocSQR(var value : double);
    procedure LocInv(var value : double);

    function MtxPow3(X, B : IMatrix; const myy : double) : IMatrix;
    function MtxTanh(X, B : IMatrix; const myy : double) : IMatrix;
    function MtxGauss(X, B : IMatrix; const myy : double) : IMatrix;
    function MtxSkew(X, B : IMatrix; const myy : double) : IMatrix;

    procedure InfoMaxICA(Examples : TDoubleMatrix);
    procedure FastICA(Examples : TDoubleMatrix);
  protected
    procedure DefineProps; override;
    function PropTypeOfName(const Name : string) : TPropType; override;

    class function ClassIdentifier : String; override;
    function OnLoadObject(const Name : String; obj : TBaseMathPersistence) : boolean; override;
  public
    class function DefProps : TICAProps;
  
    property W : IMatrix read fW;
  
    procedure ICA(Examples : TDoubleMatrix; props : TICAProps);
    
    function ProjectToFeatureSpace(Example : TDoubleMatrix) : TDoubleMatrix; 
    function Reconstruct(Features : TDoubleMatrix) : TDoubleMatrix;
  
    constructor Create; 
    destructor Destroy; override;
  end;

implementation

uses MathUtilFunc, Math;

{ TMatrixICA }

constructor TMatrixICA.Create;
begin
     inherited Create;     
end;

procedure TMatrixICA.Clear;
begin
     fW := nil;
     fWInv := nil;
     fMeanVec := nil;
end;

class function TMatrixICA.DefProps: TICAProps;
begin
     Result.method := imFastICA;
     
     Result.BlockSize := 10;
     Result.NumIter := 200;
     Result.Annealing := False;

     Result.epsilon := 0.0001;
     Result.maxFineTune := 100;
     //Result.samplesize := 1; // all 
     Result.a1 := 1;
     Result.a2 := 1;
     Result.myy := 1;
     Result.NumIC := 0;
     Result.symApproach := True;
     Result.nonLin := ieTanh;
     Result.fineTuning := False;
     Result.stabilization := True;
end;

destructor TMatrixICA.Destroy;
begin
     inherited;
end;

procedure TMatrixICA.FastICA(Examples: TDoubleMatrix);
var pca : TMatrixPCA;
    whiteMtx : IMatrix;
    deWhiteMtx : IMatrix;
    counter: Integer;
    whiteImg : IMatrix;
    hlp, hlp2 : IMatrix;
    myyK : double;
    
    B : IMatrix;
    Bold, Bold2 : IMatrix;

    minAbsCos : double;
    minAbsCos2 : double;
    stroke : double;
    myy : double;

    isFineTune : boolean;
    numIter : integer;
    long : boolean;
    
function MinDiag(mtx : IMatrix) : double;
var x : integer;
    hlp : IMatrix;
begin
     hlp := B.MultT1(mtx);
     
     Result := abs(hlp[0, 0]);
     for x := 1 to Min(hlp.Width, hlp.Height) - 1 do
         Result := Min(Result, abs( hlp[x, x] ));
end;
    
begin
     // conversion of parts of the matlab fastica package

     // ###########################################
     // #### PCA preprocessing - data whitening
     pca := TMatrixPCA.Create([pcaEigVals]);
     try
        if not pca.PCA(Examples, 1 - 1e-10, True) then
           raise Exception.Create('Error in PCA preprocessing');
     
        fMeanVec := pca.Mean.Clone;
        
     
        // dewhitening
        whiteMtx := pca.EigVecs.Transpose;
        deWhiteMtx := pca.EigVecs.Clone;

        for counter := 0 to deWhiteMtx.Width - 1 do
        begin
             whiteMtx.SetSubMatrix(0, counter, whiteMtx.Width, 1);
             whiteMtx.ScaleInPlace(1/sqrt(pca.EigVals.Vec[counter]));

             deWhiteMtx.SetSubMatrix(counter, 0, 1, deWhiteMtx.Height);
             deWhiteMtx.ScaleInPlace(sqrt(pca.EigVals.Vec[counter]));
        end;

        whiteMtx.UseFullMatrix;
        deWhiteMtx.UseFullMatrix;

        whiteImg := TDoubleMatrix.Create(Examples.Width, pca.EigVecs.Width);
        for counter := 0 to Examples.Width - 1 do
        begin
             Examples.SetSubMatrix(counter, 0, 1, Examples.Height);

             hlp := Examples.Sub(pca.Mean);
             hlp := whiteMtx.Mult(hlp);

             whiteImg.SetColumn(counter, hlp);
        end;

        Examples.UseFullMatrix;
     finally
            pca.Free;
     end;

     // ###########################################
     // #### Fast ICA algorithm:
     myyK := 0.01;
     //faileureLimit := 5;
     stroke := 0;
     myy := fProps.myy;
     isFineTune := False;
     numIter := fProps.NumIter;
     long := False;

     // initial guess

     // orthogonalize random init
     hlp := TDoubleMatrix.CreateRand(Min(whiteImg.Width, fProps.NumIC), whiteImg.Height);
     hlp.AddInplace(-0.5);
     // it's not as stable as svd but faster and since we are dealing with random inits 
     // the rank is normally full
     hlp.QRFull(B, hlp2);  
     
     Bold := TDoubleMatrix.Create(B.Width, B.Height);
     Bold2 := TDoubleMatrix.Create(B.Width, B.Height);

     // iteration
     counter := 0;
     while counter < NumIter do
     begin
          // Symmetric orthogonalization.
          // B = B * real(inv(B' * B)^(1/2));
          hlp := B.MultT1(B);
          hlp := InvertAndSQRT(hlp);
          B.MultInPlace(hlp);

          // test for termination condition
          minAbsCos := MinDiag(Bold);
          minAbsCos2 := MinDiag(BOld2);

          if 1 - minAbsCos < fProps.epsilon then
          begin
               if fProps.fineTuning and not isFineTune then
               begin
                    Bold.SetValue(0);
                    Bold2.SetValue(0);

                    myy := fProps.myy*myyK;
                    counter := 0;
                    numIter := fProps.maxFineTune;
                    isFineTune := True;
                    long := False;
                    stroke := 0;
               end
               else
               begin
                    fW := B.MultT1(whiteMtx);
                    fWInv := deWhiteMtx.Mult(B);
                    break;
               end;
          end
          else if fProps.stabilization then
          begin
               if (stroke <> 0) and (1 - minAbsCos2 < fProps.epsilon) then
               begin
                    stroke := myy;
                    myy := myy*0.5;
               end
               else if (stroke <> 0) then // only one step with reduced learning rate?
               begin
                    myy := stroke;
                    stroke := 0;
               end
               else if not long and (counter > numIter div 2) then
               begin
                    // taking long -> reduce step size
                    myy := myy*0.5;
                    long := True;
               end;
          end; 
          
          Bold2 := Bold;
          Bold := B;

          case fProps.nonLin of
            iePow3: B := MtxPow3(whiteImg, B, myy);
            ieTanh: B := MtxTanh(whiteImg, B, myy);
            ieGauss: B := MtxGauss(whiteImg, B, myy);
            ieSkew: B := MtxSkew(whiteImg, B, myy);
          end;

          inc(counter);
     end;

     if (counter = NumIter) and not isFineTune then
        raise Exception.Create('Error no convergence after ' + IntTostr(fProps.NumIter) + ' steps');
     
end;

procedure elemExp(var value : double);
begin
     value := 1 - 2*1/(1 + Exp(-value));
end;

procedure TMatrixICA.ICA(Examples: TDoubleMatrix; props : TICAProps);
var origNumIC : integer;
begin
     fProps := props;
     
     origNumIC := fProps.NumIC;
     try
        if origNumIC = 0 then
           fProps.NumIC := Examples.Width - 1;
        
        if fProps.method = imFastICA 
        then
            FastICA(Examples)
        else
            InfoMaxICA(Examples);
     finally
            if origNumIC = 0 then
               fProps.NumIC := origNumIc;
     end;
end;

procedure TMatrixICA.InfoMaxICA(Examples: TDoubleMatrix);
var mx : IMatrix;
    numOfBlocks : integer;
    mIdentMatB : IMatrix;
    mW, mU, mUt : IMatrix;
    Wz : IMatrix;
    i : Integer;
    t : integer;
    annealingIdx :integer;
    pca : TMatrixPCA;
const cAnnealingLearnFact : Array[0..3] of double = (5, 3, 2, 1);
      cAnnealingIterFact : Array[0..3] of integer = (5, 3, 3, 1);
begin
     Clear;

     // ###########################################
     // #### PCA precprocessing
     pca := TMatrixPCA.Create([]);
     try
        pca.PCA(Examples, 1, True);

        // ###########################################
        // #### Whiten data
        mx := WhitenData(pca.EigVecsT, wz);

        numOfBlocks := Examples.Height div fProps.BlockSize;
        mIdentMatB := TDoubleMatrix.CreateEye(Examples.Width);
        mIdentMatB.ScaleInPlace(fProps.BlockSize);

        mw := TDoubleMatrix.CreateEye(Examples.Width);

        annealingIdx := 0;
        if not fProps.Annealing then
           annealingIdx := High(cAnnealingLearnFact);

        // ###########################################
        // #### ICA algorithm
        while annealingIdx <= High(cAnnealingLearnFact) do
        begin
             for i := 0 to fProps.NumIter div cAnnealingIterFact[annealingIdx] - 1 do
             begin
                  t := 0;

                  while t < fProps.BlockSize*numOfBlocks do
                  begin
                       // performs matlab code:
                       // mU = mW*mX(:,t:t+BlockSize-1); 
                       // mW = mW + LearnRate*(mIdentMatB + (1 - 2*(1./(1+exp(-mU))))*mU')*mW;
                       mx.UseFullMatrix;
                       mx.SetSubMatrix(t, 0, Min(mx.Width - t, fProps.BlockSize), mx.Height);
                       mU := mW.Mult(mX);
                       mUt := mU.Transpose;

                       // performs: 1 - 2*1./(1 + exp(-mU)
                       mU.ElementwiseFuncInPlace( {$IFDEF FPC}@{$ENDIF}elemExp );
                       mU.MultInPlace(mUt); 
                       mU.AddInplace(mIdentMatB);
                       mU.MultInPlace(mW);
                       mU.ScaleInPlace(cAnnealingLearnFact[annealingIdx]*fProps.myy);

                       mW.AddInplace(mU);                             

                       inc(t, fProps.BlockSize);
                  end;
                  mx.UseFullMatrix;
             end;

             inc(annealingIdx);
        end;

        // ###########################################
        // #### Build result
        fW := mW.Mult(Wz);
        fWInv := fW.Invert;
        fWInv := pca.EigVecs.Mult(fWInv);
     
        fW.MultInPlace(pca.EigVecsT);

        fMeanVec := pca.Mean.Clone;
     finally
            pca.Free;
     end;
end;

function TMatrixICA.InvertAndSQRT(mtx: IMatrix): IMatrix;
var u, v, s : IMatrix;
    tolerance : double;
    i, j : Integer;
begin
     // compute invCxx = inversion of the principal square root of matrix mtx
     // -> since A is a covariance matrix the principal square root
     // (which is numerically most stable calculated from the schur decomposition)
     // can be calculated with the SVD since both are the same in that case
     // -> use that same decomposition as well for the inverting process!
     if mtx.SVD(U, V, s, True) <> srOk then
        raise ELinEQSingularException.Create('Error could not invert covariance matrix C');

     // main algorithm see MatrixPseudoinverse
     tolerance := s.height*eps(s.Max);

     for i := 0 to s.Height - 1 do
     begin
          if sqr(s[0, i]) <= tolerance
          then
              s[0, i] := 0
          else
              s[0, i] := 1/sqrt(s[0, i]);
     end;

     // compute inversion by inv = V*W*U'
     U.TransposeInPlace;
     for i := 0 to U.Height - 1 do
     begin
          for j := 0 to U.Width - 1 do
              U[j, i] := U[j, i]*s[0, i];
     end;

     Result := V.Mult(U);
end;

procedure TMatrixICA.LocPow3(var value: double);
begin
     Value := Value*Value*Value;
end;

procedure TMatrixICA.LocSQR(var value: double);
begin
     value := value*value;
end;

procedure TMatrixICA.LocTanh(var value: double);
begin
     value := Tanh(fProps.a1*value);
end;

procedure TMatrixICA.LocInv(var value: double);
begin
     if value <> 0 then
        value := 1/value;
end;

function TMatrixICA.MtxTanh(X, B: IMatrix; const myy : double): IMatrix;
var hypTan : IMatrix;
    hlp, hlp1 : IMatrix;
    Y : IMatrix;
    hlp2 : IMatrix;
    Beta : IMatrix;
begin
     if myy = 1 then
     begin
          // B = X * hypTan / numSamples - ...
          // ones(size(B,1),1) * sum(1 - hypTan .^ 2) .* B / numSamples * ...
          // a1;
          hypTan := X.MultT1(B);
          hypTan.ElementwiseFuncInPlace({$IFDEF FPC}@{$ENDIF}LocTanh);

          Result := X.Mult(hypTan);
          Result.ScaleInPlace(1/x.Width);
     
          hlp := TDoubleMatrix.Create(1, B.Height, 1);

          hlp1 := TDoubleMatrix.Create(hypTan.Width, hypTan.Height, 1);
          hypTan.ElementWiseMultInPlace(hypTan);
          hlp1.SubInPlace(hypTan);
          hlp1.SumInPlace(False);
          hlp.MultInPlace(hlp1);
          hlp.ElementWiseMultInPlace(B);
          hlp.ScaleInPlace(fProps.a1/x.Width);
     
          Result.SubInPlace(hlp);
     end
     else
     begin
          // Y = X' * B;
          // hypTan = tanh(a1 * Y);
          // Beta = sum(Y .* hypTan);
          y := X.MultT1(B);
          hypTan := y.ElementwiseFunc({$IFDEF FPC}@{$ENDIF}LocTanh);
          Beta := Y.ElementWiseMult(hypTan);
          Beta.SumInPlace(False);
          
          // D = diag(1 ./ (Beta - a1 * sum(1 - hypTan .^ 2)));
          hlp1 := hypTan.ElementWiseMult(hypTan);
          hlp1.ScaleAndAddInPlace(-1, 1);
          hlp1.SumInPlace(False);
          hlp1.ScaleInPlace(-fProps.a1);
          hlp := beta.Sub(hlp1);
          
          hlp.ElementwiseFuncInPlace({$IFDEF FPC}@{$ENDIF}LocInv);
          hlp.DiagInPlace(True);

          // B = B + myy * B * (Y' * hypTan - diag(Beta)) * D;
          hlp1 := Y.MultT1(hypTan);
          hlp2 := Beta.Diag(True);
          hlp1.SubInPlace(hlp2);
          hlp1.MultInPlace(hlp);
          hlp2 := B.Mult(hlp1);
          hlp2.ScaleInPlace(myy);

          Result := B.Add(hlp2);
     end;
end;

procedure TMatrixICA.LocExp(var value: double);
begin
     Value := exp(-fProps.a2*Value*0.5);
end;

function TMatrixICA.MtxGauss(X, B: IMatrix; const myy : double): IMatrix;
var hlp : IMatrix;
    hlpSqr : IMatrix;
    ex : IMatrix;
    y : IMatrix;
    gauss : IMatrix;
    beta : IMatrix;
    d : IMatrix;
begin
     if myy = 1 then
     begin
          hlp := X.MultT1(B);
          hlpSqr := hlp.ElementWiseMult(hlp);
          ex := hlpSqr.ElementwiseFunc({$IFDEF FPC}@{$ENDIF}LocExp);

          hlp := ex.ElementWiseMult(hlp);
     
          hlpSqr.ScaleAndAddInPlace(1, -fProps.a2);
          hlpSqr.ElementWiseMultInPlace(ex);

          Result := X.Mult(hlp);
          Result.ScaleInPlace(1/x.Width);

          hlp := TDoubleMatrix.Create(1, B.Height, 1);
          hlpSqr.SumInPlace(False);
          hlp.MultInPlace(hlpSqr);
          hlp.ElementWiseMultInPlace(B);
          hlp.ScaleInPlace(1/x.Width);
     
          Result.SubInPlace(hlp);
     end
     else
     begin
          // Y = X' * B;
          y := X.MultT1(B);
          // ex = exp(-a2 * (Y .^ 2) / 2);
          hlpSqr := y.ElementWiseMult(y);
          ex := hlpSqr.ElementwiseFunc({$IFDEF FPC}@{$ENDIF}LocExp);

          // gauss = Y .* ex;
          gauss := Y.ElementWiseMult(ex);

          // Beta = sum(Y .* gauss);
          beta := Y.ElementWiseMult(gauss);
          beta.SumInPlace(False);

          // D = diag(1 ./ (Beta - sum((1 - a2 * (Y .^ 2)) .* ex)));
          D := Y.ElementWiseMult(Y);
          D.ScaleAndAddInPlace(1, -fProps.a2);
          D.ElementWiseMultInPlace(ex);
          D.SumInPlace(False);
          D := beta.Sub(D);
          D.ElementwiseFuncInPlace({$IFDEF FPC}@{$ENDIF}LocInv);
          d.DiagInPlace(True);

          // B = B + myy * B * (Y' * gauss - diag(Beta)) * D;
          beta.DiagInPlace(True);
          Result := Y.MultT1(gauss);
          Result.SubInPlace(beta);
          Result.MultInPlace(D);
          Result := B.Mult(Result);
          Result.ScaleInPlace(myy);
          Result.AddInplace(B);
     end;
end;

function TMatrixICA.MtxPow3(X, B: IMatrix; const myy : double): IMatrix;
var hlp : IMatrix;
    y : IMatrix;
    beta : IMatrix;
    D : IMatrix;
begin
     if myy = 1 then
     begin
          hlp := X.MultT1(B);
          hlp.ElementwiseFuncInPlace({$IFDEF FPC}@{$ENDIF}locPow3);

          hlp := X.Mult(hlp);
          hlp.ScaleInPlace(1/x.Width);

          B.ScaleInPlace(3);
          hlp.SubInPlace(B);
          Result := hlp;
     end
     else
     begin
          // Y = X' * B;
          // Gpow3 = Y .^ 3;
          // Beta = sum(Y .* Gpow3);
          y := x.MultT1(B);
          hlp := Y.ElementwiseFunc({$IFDEF FPC}@{$ENDIF}LocPow3);
          beta := Y.ElementWiseMult(hlp);
          beta.SumInPlace(False);

          // D = diag(1 ./ (Beta - 3 * numSamples));
          D := Beta.Add(-3*X.Width);
          D.ElementwiseFuncInPlace({$IFDEF FPC}@{$ENDIF}LocInv);
          D.DiagInPlace(True);

          // B = B + myy * B * (Y' * Gpow3 - diag(Beta)) * D;
          Y := Y.MultT1(hlp);
          beta.DiagInPlace(True);
          Y.SubInPlace(beta);
          Y.MultInPlace(D);
          hlp := B.Mult(Y);
          hlp.ScaleInPlace(myy);

          Result := B.Add(hlp);
     end;
end;

function TMatrixICA.MtxSkew(X, B: IMatrix; const myy : double): IMatrix;
var Gskew : IMatrix;
    y : IMatrix;
    beta : IMatrix;
    D : IMatrix;
begin
     if myy = 1 then
     begin
          // (X * ((X' * B) .^ 2)) / numSamples
          y := X.MultT1(B);
          y.ElementwiseFuncInPlace({$IFDEF FPC}@{$ENDIF}LocSqr);
          Result := X.Mult(y);
          Result.ScaleInPlace(1/x.Width);
     end
     else
     begin
          // Y = X' * B;
          y := X.MultT1(B);
          // Gskew = Y .^ 2;
          Gskew := Y.ElementWiseMult(Y);
          // Beta = sum(Y .* Gskew);
          beta := Y.ElementWiseMult(Gskew);
          beta.SumInPlace(False);
     
          // D = diag(1 ./ (Beta));
          D := Beta.ElementwiseFunc({$IFDEF FPC}@{$ENDIF}LocInv);
          D.DiagInPlace(True);

          // B = B + myy * B * (Y' * Gskew - diag(Beta)) * D;
          beta.DiagInPlace(True);
          y.TransposeInPlace;
          y.MultInPlace(Gskew);
          y.SubInPlace(beta);
          y.MultInPlace(D);

          Result := B.Mult(y);
          Result.ScaleInPlace(myy);
          Result.AddInplace(B);
     end;
end;

function TMatrixICA.ProjectToFeatureSpace(
  Example: TDoubleMatrix): TDoubleMatrix;
var meanNorm : IMatrix;
begin
     meanNorm := example.Sub(fMeanVec);
     Result := fW.Mult(meanNorm);
end;

function TMatrixICA.Reconstruct(Features: TDoubleMatrix): TDoubleMatrix;
begin
     // project to combined pca/ica space
     Result := fWInv.Mult(Features);
     Result.AddInplace(fMeanVec);
end;

function TMatrixICA.WhitenData(examples: TDoubleMatrix; out wz : IMatrix): IMatrix;
var mean : IMatrix;
    mWz : IMatrix;
    meanCentData : IMatrix;
    x : integer;
begin
     // mean centered data matrix
     mean := examples.Mean(True);
     meanCentData := examples.clone;
     for x := 0 to examples.Width - 1 do
     begin
          meanCentData.SetSubMatrix(x, 0, 1, meanCentData.Height);
          meanCentData.SubInPlace(mean);
     end;

     // covariance matrix
     meanCentData.UseFullMatrix;
     mWz := meanCentData.Transpose;
     mWz := meanCentData.Mult( mWz );
     mWz.ScaleInPlace(1/(examples.Height - 1));

     // principal square root + inversion
     mWz := InvertAndSQRT(mWz);
     mWz.ScaleInPlace(2);

     // result
     Result := mWz.Mult(meanCentData);
     Wz := mWz;
end;

// ###########################################
// #### Persistence
// ###########################################

class function TMatrixICA.ClassIdentifier: String;
begin
     Result := 'ICA';
end;

const cMean = 'MeanVec';
      cW = 'W';
      cWInv = 'WInv';

procedure TMatrixICA.DefineProps;
begin
     AddObject(cMean, fMeanVec.GetObjRef);
     AddObject(cW, fW.GetObjRef);
     AddObject(cWInv, fWInv.GetObjRef);
end;

function TMatrixICA.PropTypeOfName(const Name: string): TPropType;
begin
     if (CompareText(Name, cMean) = 0) or (CompareText(Name, cW) = 0) or (CompareText(Name, cWInv) = 0)
     then
         Result := ptObject
     else
         Result := inherited PropTypeOfName(Name);
end;


function TMatrixICA.OnLoadObject(const Name: String;
  obj: TBaseMathPersistence): boolean;
begin
     Result := True;

     if SameText(Name, cMean) 
     then
         fMeanVec := obj as TDoubleMatrix
     else if SameText(Name, cW) 
     then
         fW := obj as TDoubleMatrix
     else if SameText(Name, cWInv) 
     then
         fWInv := obj as TDoubleMatrix
     else
         Result := inherited OnLoadObject(Name, obj);
end;

initialization
   RegisterMathIO(TMatrixICA);

end.
