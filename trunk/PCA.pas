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

unit PCA;

// ###############################################
// #### Enhanced matrix functions
// ###############################################

interface

uses SysUtils, Classes, Matrix, Types, MatrixConst, BaseMathPersistence;

{$MINENUMSIZE 2}
type
  TPCASaveData = (pcaEigVals, pcaMeanNormalizedData, pcaTransposedEigVec);
  TPCASaveDataSet = set of TPCASaveData;
  EPCAException = class(EBaseMatrixException);
  TPCAProgress = procedure(Sender : TObject; progress : integer) of Object;

type
  TPCAData = record
    EigVals : TDoubleMatrix;
    EigVecs : TDoubleMatrix;
    EigVecsT : TDoubleMatrix;
    MeanElem : TDoubleMatrix;
  end;

// ##########################################
// #### base PCA algorithm based on Singular Value decomposition
type
  TMatrixPCA = class(TBaseMathPersistence)
  private
    fProgress : TPCAProgress;

    procedure OnLineEQProgress(Progress : integer);
    function GetEigVals: TDoubleMatrix;
    function GetEigVecs: TDoubleMatrix;
    function GetMeanElem: TDoubleMatrix;
  protected
    procedure DefineProps; override;
    class function ClassIdentifier : String; override;
    function OnLoadObject(const Name : String; obj : TBaseMathPersistence) : boolean; override;

    procedure DoProgress(Progress : Integer);
  protected
    fKeepFlags : TPCASaveDataSet;
    fEigVecs : TDoubleMatrix;
    fEigVecsT : TDoubleMatrix;
    fEigVals : TDoubleMatrix;
    fMeanElem : TDoubleMatrix;
    fMeanNormExamples : TDoubleMatrix;

    function WeightedMean(Examples : TDoubleMatrix; const Weights : Array of double) : TDoubleMatrix;
    function EigVecT : TDoubleMatrix;
    procedure SortEigValsEigVecs(EigVals : TDoubleMatrix; EigVecs : TDoubleMatrix; L, R : integer);
    function GetNumNodes: integer;
    procedure Clear; virtual;
  public
    property EigVals : TDoubleMatrix read GetEigVals;
    property EigVecs : TDoubleMatrix read GetEigVecs;
    property EigVecsT : TDoubleMatrix read EigVecT;
    property Mean : TDoubleMatrix read GetMeanElem;
    property NumModes : integer read GetNumNodes;
    // calculates the Principal components from the "Examples" matrix. It is assumed that one column
    // represents one example in the features space. All eigenvalues (and it's corresponding) eigenvectors
    // which values are smaller than CutEps are discarded. If IsRelativeValue is set to true the
    // CutEps value is treated as a percentage value between 0 and 1 and represents the threshold used
    // to cut of small eigenvalues.
    // Input: Examples:  matrix of training images, MxN (M values, N examples)
    function PCA(Examples : TDoubleMatrix; CutEps : double; IsRelativeValue : boolean) : boolean; virtual;

    // Calculates the PCA from a weighted dataset. One whole example is weighted with a constant factor
    // (caled temporal weight). The can be found in Danijel Skocaj's PHD:
    // "Robust Subspace Approaches to Visual Learning and Recognition", p 58ff.
    function TemporalWeightPCA(Examples : TDoubleMatrix; CutEps : double; IsRelativeValue : boolean; const Weights : Array of Double) : boolean; virtual;

    function ProjectToFeatureSpace(Example : TDoubleMatrix) : TDoubleMatrix; overload; virtual;
    procedure ProjectToFeatureSpace(Example : TDoubleMatrix; ResMatrix : TDoubleMatrix; Column : integer); overload;
    function Reconstruct(Features : TDoubleMatrix) : TDoubleMatrix;

    property OnProgress : TPCAProgress read fProgress write fProgress;

    constructor Create(KeepFlags : TPCASaveDataSet); overload;
    constructor Create(const pcaData : TPCAData); overload;
    destructor Destroy; override;
  end;
  TMatrixPCAClass = class of TMatrixPCA;


type
  TFastRobustPCAProps = record
    NumSubSubSpaces : LongInt;        // number of subsubspace which have to be built
    SubSpaceSizes : double;           // percentage of number of features used for one subspace
    SubSpaceExcludeList : Array of TIntegerDynArray; // features which shall be excluded when creating a subspace
    SubSpaceCutEPS : double;          // same as the cuteps in the normal pca method but for the subspaces
    Start : Double;                   // num Eigenvectors*Start random sampled elements used as initialized pixel set
    Stop : Double;                    // num Eigenvectors*Stop elements used as maximum minimal pixel set
    ReductionFactor : double;         // used to iterativly reduce the start*nEig set to stop*nEig
  end;

type
  TFastRobustPCAData = record
     Props : TFastRobustPCAProps;
     SubItemIndex : Array of TIntegerDynArray;
     SubEigVecs : TDoubleMatrixDynArr;
     SubEigVecsT : TDoubleMatrixDynArr;
     SubMeanElements : TDoubleMatrixDynArr;

     PCAData : TPCAData;
  end;

// #######################################################################
// #### Fast Robust PCA algorithm:
// Based on paper: Fast Robust PCA,
// Markus Storer, Peter M. Roth, Martin Urschler, and Horst Bischof
type
  TFastRobustPCA = class(TMatrixPCA)
  protected
    fProps : TFastRobustPCAProps;
    fSubItemIdx : Array of TIntegerDynArray;
    fSubEigVecs : TDoubleMatrixDynArr;
    fSubEigVecsT : TDoubleMatrixDynArr;
    fSubMeanElem : TDoubleMatrixDynArr;
    fIsLearning : boolean;
    function GenerateSampleList(len : integer; idx : integer) : TIntegerDynArray;
    function SubEigVecT(idx : integer) : TDoubleMatrix;
    function ErrorFromSubSpace(idx : integer; Example : TDoubleMatrix) : TDoubleMatrix;
    function RobustAlphaTrimmedLinEQSolver(Example : TDoubleMatrix; var Elements : TIntegerDynArray) : TDoubleMatrix;
  private
    type
      TListLoadFlag = (llNone, llSubSpaceExcludeList, llSubItemIdx, llSubEigVecs, llSubEigVecsT, llSubMeanelem);
  private
    fListFlag : TListLoadFlag;
    fIdx : integer;
  protected
    procedure DefineProps; override;
    class function ClassIdentifier : String; override;
    function OnLoadObject(const Name : String; obj : TBaseMathPersistence) : boolean; overload; override;
    procedure OnLoadDoubleProperty(const Name : String; const Value : double); override;
    procedure OnLoadBeginList(const Name : String; count : integer); override;
    procedure OnLoadListIntArr(const Value : TIntegerDynArray); override;
    function OnLoadObject(Obj : TBaseMathPersistence) : boolean; overload; override;
    procedure OnLoadEndList; override;
  protected
    procedure Clear; override;
  public
    procedure SetProperties(const props : TFastRobustPCAProps);

    function ProjectToFeatureSpace(Example : TDoubleMatrix) : TDoubleMatrix; override;

    function ProjectToFeatureSpaceNonRobust(Example : TDoubleMatrix) : TDoubleMatrix; overload;
    procedure ProjectToFeatureSpaceNonRobust(Example : TDoubleMatrix; ResMatrix : TDoubleMatrix; Column : integer); overload;

    // Creation of the standard PCA + subspaces as assigned in the properties.
    // The procedure first tests the error distribution in many different subsubspaces (gross outlier detection).
    // After the features having a small error are used to solve the original subspaces coefficients in a
    // robust manner. The robust procedure iteratively solves the overdetermined equation system and removes
    // the elements with the highest error in each step.
    function PCA(Examples : TDoubleMatrix; CutEps : double; IsRelativeValue : Boolean) : boolean; override;

    // Calculates the PCA from a weighted dataset. It uses the same algorithm as PCA but with the weighted
    // pca in behind
    function TemporalWeightPCA(Examples : TDoubleMatrix; CutEps : double; IsRelativeValue : boolean; const Weights : Array of Double) : boolean; override;

    constructor Create(KeepFlags : TPCASaveDataSet); overload;
    constructor Create(const PcaData : TFastRobustPCAData); overload;

    destructor Destroy; override;
  end;

implementation

uses Math, LinearAlgebraicEquations, Utilities, contnrs;

// ######################################################################
// #### local definitions
// ######################################################################

type
  TErrorIdx = record
    Error : double;
    idx : integer;
  end;
  PErrorIdx = ^TErrorIdx;

procedure TMatrixPCA.Clear;
begin
     FreeAndNil(fEigVecs);
     FreeAndNil(fEigVals);
     FreeAndNil(fMeanElem);
     FreeAndNil(fMeanNormExamples);
     FreeAndNil(fEigVecsT);
end;

constructor TMatrixPCA.Create(KeepFlags: TPCASaveDataSet);
begin
     inherited Create;

     fKeepFlags := KeepFlags;
end;

constructor TMatrixPCA.Create(const pcaData: TPCAData);
begin
     inherited Create;

     fEigVecs := pcaData.EigVecs;
     fEigVecsT := pcaData.EigVecsT;
     fEigVals := pcaData.EigVals;
     fMeanElem := pcaData.MeanElem;
end;

destructor TMatrixPCA.Destroy;
begin
     Clear;
     
     inherited;
end;

procedure TMatrixPCA.DoProgress(Progress: Integer);
begin
     if Assigned(fProgress) then
        fProgress(self, Progress);
end;

function TMatrixPCA.EigVecT: TDoubleMatrix;
begin
     if not Assigned(fEigVecsT) then
        fEigVecsT := fEigVecs.Transpose;

     Result := fEigVecsT;
end;

function TMatrixPCA.GetEigVals: TDoubleMatrix;
begin
     if not Assigned(fEigVals) then
        fEigVals := TDoubleMatrix.Create;

     Result := fEigVals;
end;

function TMatrixPCA.GetEigVecs: TDoubleMatrix;
begin
     if not Assigned(fEigVecs) then
        fEigVecs := TDoubleMatrix.Create;

     Result := fEigVecs;
end;

function TMatrixPCA.GetMeanElem: TDoubleMatrix;
begin
     if not Assigned(fMeanElem) then
        fMeanElem := TDoubleMatrix.Create;

     Result := fMeanElem;
end;

function TMatrixPCA.GetNumNodes: integer;
begin
     Result := 0;
     if Assigned(fEigVecs) then
        Result := fEigVecs.Width;
end;

procedure TMatrixPCA.SortEigValsEigVecs(EigVals : TDoubleMatrix; EigVecs : TDoubleMatrix; L, R : integer);
var I, J: Integer;
    P, T : Double;
    y : integer;
begin
     // mostly copied from TList implementations
     repeat
           I := L;
           J := R;
           P := EigVals[0, (L + R) shr 1];
           repeat
                 while EigVals[0, i] > P do
                       Inc(I);
                 while EigVals[0, j] < P do
                       Dec(J);

                 if I <= J then
                 begin
                      T := EigVals[0, i];
                      EigVals[0, i] := EigVals[0, j];
                      EigVals[0, j] := T;

                      // Exchange Eigenvecs
                      for y := 0 to EigVecs.Height - 1 do
                      begin
                           T := EigVecs[i, y];
                           EigVecs[i, y] := EigVecs[j, y];
                           EigVecs[j, y] := T;
                      end;

                      Inc(I);
                      Dec(J);
                 end;
           until I > J;

           if L < J then
              SortEigValsEigVecs(EigVals, EigVecs, L, J);
           L := I;
     until I >= R;
end;

// implementation of Daniejl S. weighted batch PCA function:
//    Minimization problem here is: e = sum_i=1^M sum_j=1^N w_j * (mean_ij - sum_l=1^k u_il*a_lj)^2
//    which means -> minimize the weighted reconstruction error.
//
//        Calc mean:     mean = (sum_j=1^N w_j)^-1 * sum_j=1^N w_j*x_j
//        Rescale input: xs_j = sqrt(w_j)*(x_j - mean)
//    if num examples (N) >= num dimensions (M)
//        Weighted covariance: C = 1/(sum_j=1^N w_j)*Xs*Xs'
//        Perform SVD on C -> Obtain eigenvectors and eigenvalues
//    else
//        Weighted inner product: C' = 1/(sum_j=1^N w_j)*Xs'*Xs
//        Perform SVD on C' -> eigenvectors U' and eigenvalues L'
//        Determine principal vectors U: u_i = Xs*u'_i/sqrt(sum_j=1^N w_j*sqrt(l'_i))
//        Eigenvalues are L = L'
//    end;
// By Mike:
// The function performs the SVD like in the normal pca routine but previously
// scales the mean normalized examples by sqrt(w_i) and the overall matrix by
// sqrt(sum w). 
function TMatrixPCA.TemporalWeightPCA(Examples: TDoubleMatrix; CutEps: double;
  IsRelativeValue: boolean; const Weights: array of Double): boolean;
var i : integer;
    sumEigVals : double;
    mv : TDoubleMatrix;
    weightSum : double;
begin
     DoProgress(0);

     weightSum := 0;
     for i := 0 to Length(weights) - 1 do
         weightSum := weightSum + weights[i];

     // calculate weighted mean
     fMeanElem := WeightedMean(Examples, Weights);

     fMeanNormExamples := TDoubleMatrix.Create;
     try
        // subtract mean from each column
        fMeanNormExamples.Assign(Examples);
        fMeanNormExamples.LineEQProgress := OnLineEQProgress;

        for i := 0 to fMeanNormExamples.Width - 1 do
        begin
             fMeanNormExamples.SetSubMatrix(i, 0, 1, fMeanNormExamples.Height);
             fMeanNormExamples.SubInPlace(fMeanElem);
             fMeanNormExamples.ScaleInPlace(sqrt(Weights[i]));
        end;
        fMeanNormExamples.UseFullMatrix;
        fMeanNormExamples.ScaleInPlace(1/sqrt(weightSum));
        
        // calcluated Eigenvectors and eigenvalues using the SVD algorithm
        // note: this time the decomposition is performed on the covariance matrix!
        mv := nil;
        try
           if Examples.Height > Examples.Width
           then
               Result := fMeanNormExamples.SVD(fEigVecs, mv, fEigVals) = srOk
           else
           begin
                fMeanNormExamples.TransposeInPlace;
                Result := fMeanNormExamples.SVD(mv, fEigVecs, fEigVals) = srOk;
           end;

           // calculate real eigenvalues
           for i := 0 to fEigVals.Height - 1 do
               fEigVals[0, i] := sqr(fEigVals[0, i]);
        finally
               mv.Free;
        end;

        if not Result then
        begin
             Clear;

             exit;
        end;

        if not (pcaMeanNormalizedData in fKeepFlags) then
           FreeAndNil(fMeanNormExamples);

        // dismiss all eigenvalues smaller the given threshold
        if IsRelativeValue then
        begin
             sumEigVals := 0;
             for i := 0 to fEigVals.Height - 1 do
                 sumEigVals := sumEigVals + fEigVals[0, i];

             if (CutEps >= 0) and (CutEps < 1) then
                CutEps := sumEigVals*CutEps;
        end;

        // sort the eigenvectors and eigenvalues
        SortEigValsEigVecs(fEigVals, fEigVecs, 0, fEigVals.Height - 1);

        // shrink eigenspace according to the given parameter
        if not IsRelativeValue or (CutEps < 1) then
        begin
             sumEigVals := 0;
             for i := 0 to fEigVals.Height - 1 do
             begin
                  sumEigVals := sumEigVals + fEigVals[0, i];

                  if (IsRelativeValue and (sumEigVals >= CutEps)) or
                     (not IsRelativeValue and (CutEps >= fEigVals[0, i]))
                  then
                  begin
                       if pcaEigVals in fKeepFlags then
                       begin
                            // shrink space to 0..i
                            if IsRelativeValue
                            then
                                fEigVals.SetSubMatrix(0, 0, 1, Min(i + 1, fEigVals.Height))
                            else
                                fEigVals.SetSubMatrix(0, 0, 1, i);

                            mv := TDoubleMatrix.Create;
                            try
                               mv.Assign(fEigVals, True);
                               FreeAndNil(fEigVals);
                            except
                                  FreeAndNil(mv);
                                  raise;
                            end;

                            fEigVals := mv;
                       end
                       else
                           FreeAndNil(fEigVals);

                       if IsRelativeValue
                       then
                           fEigVecs.SetSubMatrix(0, 0, Min(i + 1, fEigVecs.Width), fEigVecs.Height)
                       else
                           fEigVecs.SetSubMatrix(0, 0, i, fEigVecs.Height);
                       mv := TDoubleMatrix.Create;
                       try
                          mv.Assign(fEigVecs, True);
                          FreeAndNil(fEigVecs);
                          fEigVecs := mv;
                       except
                             FreeAndNil(mv);
                             raise;
                       end;

                       break;
                  end;
             end;
        end;

        // create transposed eigenvectors
        if pcaTransposedEigVec in fKeepFlags then
           EigVecT;

        DoProgress(100);
     except
           Clear;

           raise;
     end;
end;

function TMatrixPCA.WeightedMean(Examples: TDoubleMatrix; const Weights: array of double): TDoubleMatrix;
var weightSum : double;
    x, y : integer;
begin
     if Length(Weights) <> Examples.Width then
        raise ERangeError.Create('Dimension error: Number of weights does not fit the number of examples' + IntToStr(Length(weights)) + ',' + IntToStr(Examples.Width));

     Result := TDoubleMatrix.Create(1, Examples.Height);
     try
        // assume that one examples is covered by one column
        weightSum := 0;
        for x := 0 to Examples.Width - 1 do
            weightSum := weightSum + weights[x];

        for y := 0 to Examples.Height - 1 do
        begin
             Result[0, y] := 0;

             for x := 0 to Examples.Width - 1 do
                 Result[0, y] := Result[0, y] + Weights[x]*Examples[x, y];
        end;

        Result.ScaleInPlace(1/weightSum);
     except
           Result.Free;
           raise;
     end;
end;

// conversion of ICG's principal component analysis.
//     The Principal Components (PC) are the orthonormal eigenvectors of
//     cov(A). If A is a mean normalized matrix of random variables,
//     cov(A) can be computed as A*A' (without scaling by (n-1)).
//
//     If the a matrix A is a set of mean normalized random vectors the
//     covariance matrix C can be achieved from the product A*A'. Since for
//     each matrix A there exist U, s and V so that  A  = U*S*V' and
//         A' = V*S'*U',
//     where [U,S,V] = svd(A), the covariance matrix can be represented as
//         C = U*S*S'*U'.
//     So it is clear that C and A have a intentical set of orthonormal
//     eigenvectors and that the eigenvalues of C are the squared
//     eigenvalues of A.
function TMatrixPCA.PCA(Examples: TDoubleMatrix; CutEps: double;
  IsRelativeValue: boolean): boolean;
var i : integer;
    mv : TDoubleMatrix;
    sumEigVals : double;
    scale : double;
begin
     DoProgress(0);

     // free previous pca calculations
     Clear;

     // calculate mean
     fMeanElem := Examples.Mean(True);

     fMeanNormExamples := TDoubleMatrix.Create;
     try
        // subtract mean from each column
        fMeanNormExamples.Assign(Examples, True);
        fMeanNormExamples.LineEQProgress := OnLineEQProgress;

        for i := 0 to fMeanNormExamples.Width - 1 do
        begin
             fMeanNormExamples.SetSubMatrix(i, 0, 1, fMeanNormExamples.Height);
             fMeanNormExamples.SubInPlace(fMeanElem);
        end;
        fMeanNormExamples.UseFullMatrix;

        // calcluated Eigenvectors and eigenvalues using the SVD algorithm
        mv := nil;
        try
           if Examples.Height > Examples.Width
           then
               Result := fMeanNormExamples.SVD(fEigVecs, mv, fEigVals, True) = srOk
           else
           begin
                fMeanNormExamples.TransposeInPlace;
                Result := fMeanNormExamples.SVD(mv, fEigVecs, fEigVals, True) = srOk;
           end;

           // this scaling is apart from the original Matlab implementation but
           // I think it's necessary since the Covariance matrix is normaly: 1/N X*X' not
           // only X*X' -> thus a scaling of 1/sqrt(N) should by applied to the mean normalized examples

           // calculate real eigenvalues (the scale comes from the matlab function princomp function
           scale := 1/sqrt(fMeanNormExamples.Width);
           for i := 0 to fEigVals.Height - 1 do
               fEigVals[0, i] := sqr(fEigVals[0, i]*scale);
        finally
               mv.Free;
        end;

        if not Result then
        begin
             Clear;

             exit;
        end;

        if not (pcaMeanNormalizedData in fKeepFlags) then
           FreeAndNil(fMeanNormExamples);

        // dismiss all eigenvalues smaller the given threshold
        if IsRelativeValue then
        begin
             sumEigVals := 0;
             for i := 0 to fEigVals.Height - 1 do
                 sumEigVals := sumEigVals + fEigVals[0, i];

             if (CutEps >= 0) and (CutEps < 1)
             then
                 CutEps := sumEigVals*CutEps
             else
                 CutEps := sumEigVals;
        end;

        // sort the eigenvectors and eigenvalues
        SortEigValsEigVecs(fEigVals, fEigVecs, 0, fEigVals.Height - 1);

        // shrink eigenspace according to the given parameter
        // at least one eigenvector shall be kept
        sumEigVals := fEigVals[0, 0];
        for i := 1 to fEigVals.Height - 1 do
        begin
             sumEigVals := sumEigVals + fEigVals[0, i];

             if CutEps < sumEigVals then
             begin
                  if pcaEigVals in fKeepFlags then
                  begin
                       // shrink space to 0..i
                       fEigVals.SetSubMatrix(0, 0, 1, i);
                       mv := TDoubleMatrix.Create;
                       try
                          mv.Assign(fEigVals, True);
                          FreeAndNil(fEigVals);
                       except
                             FreeAndNil(mv);
                             raise;
                       end;
                       fEigVals := mv;
                  end
                  else
                      FreeAndNil(fEigVals);

                  fEigVecs.SetSubMatrix(0, 0, i, fEigVecs.Height);
                  mv := TDoubleMatrix.Create;
                  try
                     mv.Assign(fEigVecs, True);
                     FreeAndNil(fEigVecs);
                     fEigVecs := mv;
                  except
                        FreeAndNil(mv);
                        raise;
                  end;

                  break;
             end;
        end;

        DoProgress(100);

        // create transposed eigenvectors
        if pcaTransposedEigVec in fKeepFlags then
           EigVecT;
     except
           Clear;

           raise;
     end;
end;

procedure TMatrixPCA.ProjectToFeatureSpace(Example, ResMatrix: TDoubleMatrix; Column : integer);
var tmp : TDoubleMatrix;
begin
     if not Assigned(fEigVecs) then
        raise EPCAException.Create('PCA object not initialized');

     if (Example.Width <> 1) or (ResMatrix.Height <> fEigVecs.Width) then
        raise EPCAException.Create('Resulting dimension of projection matrix is wrong');

     tmp := ProjectToFeatureSpace(Example);
     try
        ResMatrix.SetColumn(column, tmp);
     finally
            tmp.Free;
     end;
end;

function TMatrixPCA.ProjectToFeatureSpace(
  Example: TDoubleMatrix): TDoubleMatrix;
var meanMtx : TDoubleMatrix;
begin
     Result := nil;

     if not Assigned(fEigVecs) then
        raise EPCAException.Create('PCA object not initialized');

     meanMtx := TDoubleMatrix.Create;
     try
        meanMtx.Assign(Example, True);

        // check dimensions -> transpose if necessary
        if (meanMtx.Width = fEigVecs.Height) and (meanMtx.Height = 1) then
           meanMtx.TransposeInPlace;

        if (meanMtx.Width <> fMeanElem.Width) or (meanMtx.Height <> fMeanElem.Height) then
           raise EPCAException.Create('Example dimension do not match');

        // ####################################################
        // #### features = EigVec'*(Example - meanElem)
        meanMtx.SubInPlace(fMeanElem);

        Result := EigVecT.Mult(meanMtx);
     finally
            meanMtx.Free;
     end;
end;


function TMatrixPCA.Reconstruct(Features: TDoubleMatrix): TDoubleMatrix;
begin
     if not Assigned(fEigVecs) then
        raise EPCAException.Create('PCA object not initialized');

     if (Features.Width <> 1) or (Features.Height <> fEigVecs.Width) then
        raise EPCAException.Create('Dimensions of PCA feature space differs from given example'); 
     
     // Example := (EigVec*Features) + MeanElem
     Result := fEigVecs.Mult(Features);
     Result.AddInplace(fMeanElem);
end;

{ TFastRobustPCA }

constructor TFastRobustPCA.Create(KeepFlags: TPCASaveDataSet);
begin
     inherited Create(KeepFlags);

     // set standard properties
     fProps.NumSubSubSpaces := 100;
     fProps.SubSpaceSizes := 0.01;   // use 1 percent per subspace
     fProps.SubSpaceExcludeList := nil; // just use random sampling
     fProps.Start := 50;                // 50*NumEigenVecs to start the robust method
     fProps.Stop := 20;                 // stop at 20*NumEigenVecs
     fProps.ReductionFactor := 0.75;    // Reduce the point set by this factor
     fProps.SubSpaceCutEPS := 1;      // per default use 90% of the energy in subspaces
end;

procedure TFastRobustPCA.Clear;
var i : integer;
begin
     inherited;

     if not fIsLearning then
     begin
          for i := 0 to Length(fSubEigVecs) - 1 do
          begin
               fSubEigVecs[i].Free;
               fSubMeanElem[i].Free;
               fSubEigVecsT[i].Free;
          end;

          fSubEigVecs := nil;
          fSubMeanElem := nil;
          fSubEigVecsT := nil;
     end;
end;

constructor TFastRobustPCA.Create(const PCaData: TFastRobustPCAData);
var i : Integer;
begin
     inherited Create(PCAData.PCAData);

     fProps := PCaData.Props;
     SetLength(fSubItemIdx, Length(PCaData.SubItemIndex));
     for i := 0 to Length(fSubItemIdx) - 1 do
         fSubItemIdx[i] := PCAData.SubItemIndex[i];
     fSubEigVecs := PCaData.SubEigVecs;
     fSubEigVecsT := PCAData.SubEigVecsT;
     fSubMeanElem := PCAData.SubMeanElements;
end;

class function TFastRobustPCA.ClassIdentifier: String;
begin
     Result := 'FRPCA';
end;

procedure TFastRobustPCA.DefineProps;
var i : integer;
begin
     // write the base properties:
     inherited;

     // #######################################################
     // #### Write out properties
     AddIntProperty('NumSubSubSpaces', fProps.NumSubSubSpaces);
     AddDoubleProperty('SubspaceSizes', fProps.SubSpaceSizes);
     if Length(fProps.SubSpaceExcludeList) > 0 then
     begin
          BeginList('SubSpaceExclList', Length(fProps.SubSpaceExcludeList));
          for i := 0 to Length(fProps.SubSpaceExcludeList) - 1 do
              AddListIntArr(fProps.SubSpaceExcludeList[i]);
          EndList;
     end;
     AddDoubleProperty('SubSpaceCutEPS', fProps.SubSpaceCutEPS);
     AddDoubleProperty('Start', fProps.Start);
     AddDoubleProperty('Stop', fProps.Stop);
     AddDoubleProperty('ReductFact', fProps.ReductionFactor);

     // #######################################################
     // #### Write out objects
     if Length(fSubItemIdx) > 0 then
     begin
          BeginList('SubItemIdx', Length(fSubItemIdx));
          for i := 0 to Length(fSubItemIdx) - 1 do
              AddListIntArr(fSubItemIdx[i]);
          EndList;
     end;
     if Length(fSubEigVecs) > 0 then
     begin
          BeginList('SubEigVecs', Length(fSubEigVecs));
          for i := 0 to Length(fSubEigVecs) - 1 do
              AddObject(fSubEigVecs[i]);
          EndList;
     end;

     if Length(fSubEigVecsT) > 0 then
     begin
          BeginList('SubEigVecsT', Length(fSubEigVecsT));
          for i := 0 to Length(fSubEigVecsT) - 1 do
              AddObject(fSubEigVecsT[i]);
          EndList;
     end;

     if Length(fSubMeanElem) > 0 then
     begin
          BeginList('SubMeanElem', Length(fSubMeanElem));
          for i := 0 to Length(fSubMeanElem) - 1 do
              AddObject(fSubMeanElem[i]);
          EndList;
     end;
end;

procedure TFastRobustPCA.OnLoadBeginList(const Name: String;
  count: integer);
begin
     fIdx := 0;
     if CompareText(Name, 'SubSpaceExclList') = 0 then
     begin
          SetLength(fProps.SubSpaceExcludeList, Count);
          fListFlag := llSubSpaceExcludeList;
     end
     else if CompareText(Name, 'SubItemIdx') = 0 then
     begin
          SetLength(fSubItemIdx, Count);
          fListFlag := llSubItemIdx;
     end
     else if CompareText(Name, 'SubEigVecs') = 0 then
     begin
          SetLength(fSubEigVecs, Count);
          fListFlag := llSubEigVecs;
     end
     else if CompareText(Name, 'SubEigVecsT') = 0 then
     begin
          SetLength(fSubEigVecsT, Count);
          fListFlag := llSubEigVecsT;
     end
     else if CompareText(Name, 'SubMeanElem') = 0 then
     begin
          SetLength(fSubMeanElem, Count);
          fListFlag := llSubMeanelem;
     end;
end;

procedure TFastRobustPCA.OnLoadEndList;
begin
     fListFlag := llNone;
end;

procedure TFastRobustPCA.OnLoadListIntArr(const Value: TIntegerDynArray);
begin
     case fListFlag of
       llSubSpaceExcludeList: fProps.SubSpaceExcludeList[fIdx] := Value;
       llSubItemIdx: fSubItemIdx[fIdx] := Value;
     end;

     inc(fIdx);
end;

function TFastRobustPCA.OnLoadObject(Obj: TBaseMathPersistence): boolean;
begin
     Result := True;

     case fListFlag of
       llSubEigVecs : fSubEigVecs[fIdx] := Obj as TDoubleMatrix;
       llSubEigVecsT : fSubEigVecsT[fIdx] := Obj as TDoubleMatrix;
       llSubMeanelem : fSubMeanElem[fIdx] := Obj as TDoubleMatrix;
     else
         Result := inherited OnLoadObject(Obj);
     end;
end;

function TFastRobustPCA.OnLoadObject(const Name: String;
  obj: TBaseMathPersistence): boolean;
begin
     Result := inherited OnLoadObject(Name, obj);
end;

procedure TFastRobustPCA.OnLoadDoubleProperty(const Name: String;
  const Value: double);
begin
     if CompareText(Name, 'SubspaceSizes') = 0
     then
         fProps.SubSpaceSizes := value
     else if CompareText(Name, 'SubSpaceCutEPS') = 0
     then
         fProps.SubSpaceCutEPS := Value
     else if CompareText(Name, 'Start') = 0
     then
         fProps.Start := Value
     else if CompareText(Name, 'Stop') = 0
     then
         fProps.Stop := Value
     else if CompareText(Name, 'ReductFact') = 0
     then
         fProps.ReductionFactor := Value
     else
         inherited;
end;

destructor TFastRobustPCA.Destroy;
begin
     Clear;
     
     inherited;
end;

function TFastRobustPCA.ErrorFromSubSpace(idx: integer;
  Example: TDoubleMatrix): TDoubleMatrix;
var x : TDoubleMatrix;
    a : TDoubleMatrix;
    xhat : TDoubleMatrix;
    i : integer;
begin
     xhat := nil;
     a := nil;
     x := TDoubleMatrix.Create(1, fSubMeanElem[idx].Height);
     try
        // project to feature space
        for i := 0 to x.Height - 1 do
            x[0, i] := Example[0, fSubItemIdx[idx][i]];

        x.SubInPlace(fSubMeanElem[idx]);
        a := SubEigVecT(idx).Mult(x);

        // project back
        xhat := fSubEigVecs[idx].Mult(a);

        // calculate absolute error
        Result := TDoubleMatrix.Create(1, xhat.Height);

        for i := 0 to Result.Height - 1 do
            Result[0, i] := abs(x[0, i] - xhat[0, i]);
     finally
            x.Free;
            a.Free;
            xhat.Free;
     end;
end;

function TFastRobustPCA.GenerateSampleList(len : integer;
  idx: integer): TIntegerDynArray;
var SetList : array of Boolean;
    i : integer;
    rndIdx : integer;
    rndIdx2 : integer;
    swap : integer;
    numExcluded : integer;
    resLen : integer;
begin
     SetLength(SetList, len);
     numExcluded := 0;
     if Length(fProps.SubSpaceExcludeList) > idx then
     begin
          for i := 0 to Length(fProps.SubSpaceExcludeList[idx]) - 1 do
          begin
               SetList[i] := True;
               inc(numExcluded);
          end;
     end;

     if fProps.SubSpaceSizes > 0.3 then
     begin
          // from this point it's better to do switching
          SetLength(Result, len - numExcluded);
          i := 0;
          rndIdx := 0;
          while i < Length(Result) do
          begin
               if not SetList[rndIdx] then
               begin
                    Result[i] := rndIdx;
                    inc(i);
               end;
               inc(rndIdx);
          end;

          // do random switches
          resLen := Length(Result);
          for i := 0 to 2*Length(Result) - 1 do
          begin
               rndIdx := random(resLen);
               rndIdx2 := random(resLen);

               swap := Result[rndIdx];
               Result[rndIdx] := Result[rndIdx2];
               Result[rndIdx2] := swap;
          end;

          SetLength(Result, Trunc(fProps.SubSpaceSizes*len));
     end
     else
     begin
          SetLength(Result, Trunc(fProps.SubSpaceSizes*len));

          // with that low percentage it is not so common to get collisions
          // thus do a direct randomized sampling
          i := 0;
          while i < Length(Result) do
          begin
               rndIdx := Random(Len);

               if not SetList[rndIdx] then
               begin
                    Result[i] := rndIdx;

                    inc(i);
               end;
          end;
     end;
end;

function TFastRobustPCA.PCA(Examples: TDoubleMatrix; CutEps: double;
  IsRelativeValue: Boolean): boolean;
var i, j : integer;
    SubExamples : TDoubleMatrix;
    keepTransposed : boolean;
    subEPS : double;
begin
     Clear;

     subEPS := fProps.SubSpaceCutEPS;
     if subEPS = 0 then
     begin
          subEPS := CutEPS;
          if CutEPS > 1 then
             subEPS := 1;
     end;
     
     keepTransposed := pcaTransposedEigVec in fKeepFlags;
     Exclude(fKeepFlags, pcaTransposedEigVec);
     
     // #########################################################
     // #### Create SubSubSpaces
     SetLength(fSubItemIdx, fProps.NumSubSubSpaces);
     SetLength(fSubEigVecs, fProps.NumSubSubSpaces);
     SetLength(fSubMeanElem, fProps.NumSubSubSpaces);
     SetLength(fSubEigVecsT, fProps.NumSubSubSpaces);
     fIsLearning := True;
     for i := 0 to fProps.NumSubSubSpaces - 1 do
     begin
          fSubItemIdx[i] := GenerateSampleList(Examples.Height, i);

          SubExamples := TDoubleMatrix.Create(Examples.Width, Length(fSubItemIdx[i]));
          try
             for j := 0 to Length(fSubItemIdx[i]) - 1 do
                 SubExamples.SetRow(j, Examples, fSubItemIdx[i][j]);

             inherited PCA(SubExamples, subEPS, True);
          finally
                 SubExamples.Free;
          end;

          // copy result
          fSubEigVecs[i] := fEigVecs;
          fSubMeanElem[i] := fMeanElem;

          if keepTransposed then
             fSubEigVecsT[i] := EigVecT;

          fEigVecs := nil;
          fMeanElem := nil;
          fEigVecsT := nil;
          FreeAndNil(fEigVals);
     end;

     // ##########################################################
     // #### Create Complete PCA set
     Result := inherited PCA(Examples, CutEps, IsRelativeValue);

     fIsLearning := False;
     
     if Assigned(fEigVecsT) then
        fEigVecsT.Free;
        
     fEigVecsT := nil;

     if keepTransposed then
        include(fKeepFlags, pcaTransposedEigVec);
end;

function ErrorSort(p1, p2 : Pointer) : integer;
begin
     Result := CompareValue(PErrorIdx(p1)^.Error, PErrorIdx(p2)^.Error);
end;

function TFastRobustPCA.ProjectToFeatureSpace(
  Example: TDoubleMatrix): TDoubleMatrix;
var i, j : Integer;
    errors : TDoubleMatrixDynArr;
    localMeanErrors : TDoubleDynArray;
    globalMeanError : double;
    err : TDoubleMatrix;
    errDist : TList;
    elem : PErrorIdx;
    actErr : double;
    Elements : TIntegerDynArray;
begin
     assert((Example.Width = 1) and (Example.height = fMeanElem.Height), 'Dimension error');
      
     // ##########################################################
     // #### Gross outlier detection by estimating the subsubspace errors
     SetLength(errors, fProps.NumSubSubSpaces);
     try
        // project the example to all subspaces.
        for i := 0 to fProps.NumSubSubSpaces - 1 do
            errors[i] := ErrorFromSubSpace(i, Example);

        // calculate all mean errors and the overall mean error
        globalMeanError := 0;
        SetLength(localMeanErrors, fProps.NumSubSubSpaces);

        for i := 0 to fProps.NumSubSubSpaces - 1 do
        begin
             err := errors[i].Mean(False);
             localMeanErrors[i] := err[0, 0];
             err.Free;

             globalMeanError := globalMeanError + 1/fProps.NumSubSubSpaces*localMeanErrors[i];
        end;

        // define set of inlining features
        // element error must be lower then min(localMeanError, globalMeanError);
        errdist := TList.Create;
        try
           errdist.Capacity := Round(fProps.NumSubSubSpaces*fProps.SubSpaceSizes*Example.Height/2);
           for i := 0 to fProps.NumSubSubSpaces - 1 do
           begin
                actErr := Min(globalMeanError, localMeanErrors[i]);
                for j := 0 to errors[i].Height - 1 do
                begin
                     if errors[i][0, j] < actErr then
                     begin
                          new(elem);
                          elem^.Error := errors[i][0, j];
                          elem^.idx := fSubItemIdx[i][j];
                          errdist.Add(elem);
                     end;
                end;
           end;

           // sort the list according to the error distribution
           errdist.Sort(ErrorSort);

           // create the point set
           SetLength(Elements, Min(Round(fProps.Start*fEigVecs.Width), errdist.Count));
           for i := 0 to Length(Elements) - 1 do
               Elements[i] := PErrorIdx(errdist[i])^.idx;
        finally
               for i := 0 to errDist.Count - 1 do
                   dispose(PErrorIdx(errDist[i]));

               errdist.Free;
        end;
     finally
            for i := 0 to Length(errors) - 1 do
                FreeAndNil(errors[i]);
     end;


     // ##############################################################
     // #### Project to the original feature space using only inlining features
     Result := RobustAlphaTrimmedLinEQSolver(Example, Elements);
end;

procedure TFastRobustPCA.ProjectToFeatureSpaceNonRobust(Example,
  ResMatrix: TDoubleMatrix; Column: integer);
var tmp : TDoubleMatrix;
begin
     if not Assigned(fEigVecs) then
        raise EPCAException.Create('PCA object not initialized');

     if (Example.Width <> 1) or (ResMatrix.Height <> fEigVecs.Width) then
        raise EPCAException.Create('Resulting dimension of projection matrix is wrong');

     tmp := ProjectToFeatureSpaceNonRobust(Example);
     try
        ResMatrix.SetColumn(column, tmp);
     finally
            tmp.Free;
     end;
end;


function TFastRobustPCA.ProjectToFeatureSpaceNonRobust(
  Example: TDoubleMatrix): TDoubleMatrix;
begin
     Result := inherited ProjectToFeatureSpace(Example);
end;

function TFastRobustPCA.RobustAlphaTrimmedLinEQSolver(Example : TDoubleMatrix;
  var Elements: TIntegerDynArray): TDoubleMatrix;
var numElements : integer;
    A : TDoubleMatrix;
    AInv : TDoubleMatrix;
    Y : TDoubleMatrix;
    X : TDoubleMatrix;
    Xa : TDoubleMatrix;
    i : integer;
    data : TDoubleDynArray;
    thresh : double;
    tIdx : integer;
    dataIdx : integer;
    meanNormExample : TDoubleMatrix;
    numNewElements : integer;
begin
     Result := nil;

     numElements := Length(Elements);

     A := nil;
     Y := nil;
     x := nil;
     Xa := nil;
     AInv := nil;
     meanNormExample := Example.Sub(fMeanElem);
     try
        repeat
              A := TDoubleMatrix.Create(fEigVecs.Width, numElements);
              X := TDoubleMatrix.Create(1, numElements);

              // copy rows respectively elements
              for i := 0 to numElements - 1 do
              begin
                   A.SetRow(i, fEigVecs, Elements[i]);
                   X[0, i] := meanNormExample[0, Elements[i]];
              end;

              // ##########################################################
              // #### solve this overdetermined linear set of equations

              // todo: eventually dismiss the pseudoinverse and apply a better
              // solving method
              if A.PseudoInversion(AInv) <> srOk then
                 raise EPCAException.Create('Error robustly solving PCA coefficients');
              AInv.MultInPlace(X);

              // ##########################################################
              // #### Check error distribution -> but only on the projected subset
              Xa := A.Mult(AInv);

              // check error
              for i := 0 to numElements - 1 do
                  X[0, i] := Abs(X[0, i] - Xa[0, i]);

              // reduce the number of points according to the error distribution
              data := X.SubMatrix;
              QuickSort(data[0], sizeof(double), Length(data), DoubleSortFunc);

              numNewElements := Max(fEigVecs.Width, Min(numElements - 1, Round(numElements*fProps.ReductionFactor)));
              if numNewElements <= 0 then
                 break;

              thresh := data[numNewElements];
              dataIdx := 0;

              for tidx := 0 to numElements - 1 do
              begin
                   if dataIdx = numNewElements - 1 then
                      break;
                      
                   if (X[0, tidx] <= thresh) then
                   begin
                        Elements[dataIdx] := Elements[tidx];
                        inc(dataIdx);
                   end;
              end;

              numElements := dataIdx;

              // ##########################################################
              // #### Create result
              if numElements <= Max(fEigVecs.Width, fProps.Stop*fEigVecs.Width) then
              begin
                   Result := AInv;
                   Ainv := nil;
              end;

              FreeAndNil(Xa);
              FreeAndNil(X);
              FreeAndNil(A);
              FreeAndNil(aInv);
        until numElements <= Max(fEigVecs.Width, fProps.Stop*fEigVecs.Width);

        FreeAndNil(meanNormExample);
     except
           A.Free;
           Y.Free;
           X.Free;
           Xa.Free;
           AInv.Free;
           meanNormExample.Free;

           raise;
     end;
end;

procedure TFastRobustPCA.SetProperties(const props: TFastRobustPCAProps);
begin
     fProps := Props;
     fProps.NumSubSubSpaces := Max(1, fProps.NumSubSubSpaces);
     fProps.SubSpaceSizes := Min(1, Max(0, fProps.SubSpaceSizes));
end;

function TFastRobustPCA.SubEigVecT(idx: integer): TDoubleMatrix;
begin
     if not Assigned(fSubEigVecsT[idx]) then
        fSubEigVecsT[idx] := fSubEigVecs[idx].Transpose;

     Result := fSubEigVecsT[idx];
end;

function TFastRobustPCA.TemporalWeightPCA(Examples: TDoubleMatrix;
  CutEps: double; IsRelativeValue: boolean;
  const Weights: array of Double): boolean;
var i, j : integer;
    SubExamples : TDoubleMatrix;
    keepTransposed : boolean;
    subEPS : double;
begin
     Clear;

     subEPS := fProps.SubSpaceCutEPS;
     if subEPS = 0 then
     begin
          subEPS := CutEPS;
          if CutEPS > 1 then
             subEPS := 1;
     end;
     
     keepTransposed := pcaTransposedEigVec in fKeepFlags;
     Exclude(fKeepFlags, pcaTransposedEigVec);
     
     // #########################################################
     // #### Create SubSubSpaces
     SetLength(fSubItemIdx, fProps.NumSubSubSpaces);
     SetLength(fSubEigVecs, fProps.NumSubSubSpaces);
     SetLength(fSubMeanElem, fProps.NumSubSubSpaces);
     SetLength(fSubEigVecsT, fProps.NumSubSubSpaces);
     fIsLearning := True;
     for i := 0 to fProps.NumSubSubSpaces - 1 do
     begin
          fSubItemIdx[i] := GenerateSampleList(Examples.Height, i);

          SubExamples := TDoubleMatrix.Create(Examples.Width, Length(fSubItemIdx[i]));
          try
             for j := 0 to Length(fSubItemIdx[i]) - 1 do
                 SubExamples.SetRow(j, Examples, fSubItemIdx[i][j]);

             inherited TemporalWeightPCA(SubExamples, subEPS, True, Weights);
          finally
                 SubExamples.Free;
          end;

          // copy result
          fSubEigVecs[i] := fEigVecs;
          fSubMeanElem[i] := fMeanElem;

          if keepTransposed then
             fSubEigVecsT[i] := EigVecT;

          fEigVecs := nil;
          fMeanElem := nil;
          fEigVecsT := nil;
          FreeAndNil(fEigVals);
     end;

     // ##########################################################
     // #### Create Complete PCA set
     Result := inherited TemporalWeightPCA(Examples, CutEps, IsRelativeValue, Weights);

     fIsLearning := False;
     
     if Assigned(fEigVecsT) then
        fEigVecsT.Free;
        
     fEigVecsT := nil;

     if keepTransposed then
        include(fKeepFlags, pcaTransposedEigVec);
end;

// ###########################################################
// #### Persistence functions

class function TMatrixPCA.ClassIdentifier: String;
begin
     Result := 'PCA';
end;

procedure TMatrixPCA.DefineProps;
begin
     if not Assigned(fMeanElem) then
        exit;

     AddObject('mean', fMeanElem);
     AddObject('pcavec', fEigVecs);

     if (pcaEigVals in fKeepFlags) and assigned(fEigVals) then
        AddObject('eigvals', fEigVals);
     if (pcaMeanNormalizedData in fKeepFlags) and assigned(fMeanNormExamples) then
        AddObject('meannormdata', fMeanNormExamples);
     if (pcaTransposedEigVec in fKeepFlags) and Assigned(fEigVecsT) then
        AddObject('pcavect', fEigVecsT);
end;

procedure TMatrixPCA.OnLineEQProgress(Progress: integer);
begin
     if Assigned(fProgress) then
        fProgress(self, progress*99 div 100);
end;

function TMatrixPCA.OnLoadObject(const Name: String;
  obj: TBaseMathPersistence) : boolean;
begin
     Result := True;
     if CompareText(name, 'mean') = 0
     then
         fMeanElem := obj as TDoubleMatrix
     else if CompareText(name, 'pcavec') = 0
     then
         fEigVecs := obj as TDoubleMatrix
     else if CompareText(name, 'eigvals') = 0 then
     begin
          fEigVals := obj as TDoubleMatrix;
          include(fKeepFlags, pcaEigVals);
     end
     else if CompareText(name, 'meannormdata') = 0 then
     begin
          fMeanNormExamples := obj as TDoubleMatrix;
          include(fKeepFlags, pcaMeanNormalizedData);
     end
     else if CompareText(name, 'pcavect') = 0 then
     begin
          fEigVecsT := obj as TDoubleMatrix;
          include(fKeepFlags, pcaTransposedEigVec);
     end
     else
         Result := False;
end;

initialization
  RegisterMathIO(TMatrixPCA);
  RegisterMathIO(TFastRobustPCA);

end.
