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

unit IncrementalPCA;

// ###############################################
// #### Incremental version of the PCA algorithm
// ###############################################

interface

uses SysUtils, PCA, Matrix, BaseMathPersistence, Types;

type
  TIncrPCAData = record
    Props : TPCAData;
    Weights : TDoubleDynArray;
    NumEigenVecs : integer;
    A : TDoubleMatrix;
  end;

// ###############################################
// #### incremental version of the base pca algorithm
// based on: Danijel Skocaj
// Robust Subspace Approaches to Visual Learning and Recognition
type
  TIncrementalPCA = class(TMatrixPCA)
  private
    fNumEigenvectorsToKeep: integer;
    fA : TDoubleMatrix;
    fWeights : TDoubleDynArray;
    fNumWeights : integer;

    procedure AddWeight(const weight : double);
    function GetA: TDoubleMatrix;
  protected
    procedure DefineProps; override;
    class function ClassIdentifier : String; override;
    function OnLoadObject(const Name : String; obj : TBaseMathPersistence) : boolean; override;
    procedure OnLoadIntProperty(const Name : String; Value : integer); override;
    procedure OnLoadDoubleArr(const Name : String; const Value : TDoubleDynArray); override;
  public
    property NumEigenvectorsToKeep : integer read fNumEigenvectorsToKeep write fNumEigenvectorsToKeep;
    property A : TDoubleMatrix read GetA;

    function UpdateEigenspace(mtx : TDoubleMatrix) : boolean; overload;
    // this function is an extension to the base incremental pca algorithm -> it uses the same blending algorithm
    // as the weighted incremental version (this is works since it is only a special case of the weighted version!)
    function UpdateEigenspace(mtx : TDoubleMatrix; const spacialWeights : Array of double) : boolean; overload;
    // note the spacial weights MUST have values between 0 and 1. 1 means no weighting and 0 means that pixel is not taken into account.
    // implementation of Daniejl S. weighted incremental PCA function (p 69ff):
    // this function supports spacial weights as well as temporal weights. If the function without spacial weights is called
    // all weights are set to 1 (no blending with the original image)
    function UpdateEigenspaceWeighted(mtx : TDoubleMatrix; const weight : double) : boolean; overload;
    function UpdateEigenspaceWeighted(mtx : TDoubleMatrix; const weight : double; const spacialWeights : Array of double) : boolean; overload;

    function PCA(Examples : TDoubleMatrix; CutEps : double; IsRelativeValue : boolean) : boolean; override;
    function TemporalWeightPCA(Examples : TDoubleMatrix; CutEps : double; IsRelativeValue : boolean; const Weights : Array of Double) : boolean; override;

    constructor Create(const Data : TIncrPCAData); overload;
    constructor Create(KeepFlags : TPCASaveDataSet); overload;
    destructor Destroy; override;
  end;

type
  TFastRobustPCADataEx = record
    RData : TFastRobustPCAData;
    A : TDoubleMatrix;
    Weights : TDoubleDynArray;
    NumEigenVecs : integer;
    SubA : TDoubleMatrixDynArr;
    SubWeights : Array of TDoubleDynArray;
  end;

// ###############################################
// #### incremental version of the fast robust pca algorithm
// a combination of the base fast robust pca method and the incremental pca algorithm
type
  TFastRobustIncrementalPCA = class(TFastRobustPCA)
  private
    fNumEigenvectorsToKeep : integer;
    fA : TDoubleMatrix;
    fSubA : TDoubleMatrixDynArr;
    fWeights : Array of TDoubleDynArray;
    fNumWeights : TIntegerDynArray;
    fSubEigVals : TDoubleMatrixDynArr;

    procedure AddWeight(idx : integer; const weight : double);
    procedure BuildSubspaces(Examples : TDoubleMatrix; origKeepFlags : TPCASaveDataSet);
  protected
    type
      TListLoadType = (llNone, llSubA, llWeights, llSubEigVals);
  private
    fIdx : integer;
    fListType : TListLoadType;
  protected
    procedure DefineProps; override;
    class function ClassIdentifier : String; override;
    procedure OnLoadBeginList(const Name : String; count : integer); override;
    procedure OnLoadEndList; override;
    function OnLoadObject(Obj : TBaseMathPersistence) : boolean; overload; override;
    function OnLoadObject(const Name : String; obj : TBaseMathPersistence) : boolean; overload; override;
    procedure OnLoadListDoubleArr(const Value : TDoubleDynArray); override;
    procedure OnLoadIntArr(const Name : String; const Value : TIntegerDynArray); override;
    procedure OnLoadIntProperty(const Name : String; Value : integer); overload; override;
  protected
    procedure Clear; override;
  public
    property NumEigenvectorsToKeep : integer read fNumEigenvectorsToKeep write fNumEigenvectorsToKeep;
    property A : TDoubleMatrix read fA;

    function UpdateEigenspace(mtx : TDoubleMatrix) : boolean; overload;
    // this function is an extension to the base incremental pca algorithm -> it uses the same blending algorithm
    // as the weighted incremental version (this is works since it is only a special case of the weighted version!)
    function UpdateEigenspace(mtx : TDoubleMatrix; const spacialWeights : Array of double) : boolean; overload;
    // note the spacial weights MUST have values between 0 and 1. 1 means no weighting and 0 means that pixel is not taken into account.
    // implementation of Daniejl S. weighted incremental PCA function (p 69ff):
    // this function supports spacial weights as well as temporal weights. If the function without spacial weights is called
    // all weights are set to 1 (no blending with the original image)
    function UpdateEigenspaceWeighted(mtx : TDoubleMatrix; const weight : double) : boolean; overload;
    function UpdateEigenspaceWeighted(mtx : TDoubleMatrix; const weight : double; const spacialWeights : Array of double) : boolean; overload;

    function PCA(Examples : TDoubleMatrix; CutEps : double; IsRelativeValue : boolean) : boolean; override;
    function TemporalWeightPCA(Examples : TDoubleMatrix; CutEps : double; IsRelativeValue : boolean; const Weights : Array of Double) : boolean; override;

    constructor Create(const Props : TFastRobustPCADataEx); overload;
    constructor Create(KeepFlags : TPCASaveDataSet); overload;
    destructor Destroy; override;
  end;


implementation

uses Math, Utilities, MatrixConst;

// ####################################################################
// #### Local functions - used to update the eigenspaces
// ####################################################################

// implements the update Eigenspace functions and holds references to all necessary data structures
type
  TSubspaceUpdater = class(TObject)
  private
    fNumEigenvectorsToKeep : integer;
    fEigVecs : TDoubleMatrix;
    fEigVals : TDoubleMatrix;
    fMeanElem : TDoubleMatrix;
    fA : TDoubleMatrix;
    fWeights : TDoubleDynArray;
    fNumWeights : integer;

    function InternalUpdateEigenspace(mtx : TDoubleMatrix; weighted : boolean) : boolean;
    function PrepareBlendExample(mtx : TDoubleMatrix; const spacialWeights : Array of double) : TDoubleMatrix;
  public
    function UpdateEigenspace(mtx : TDoubleMatrix; var EigVecs, EigVals, MeanElem, A : TDoubleMatrix) : boolean; overload;
    // this function is an extension to the base incremental pca algorithm -> it uses the same blending algorithm
    // as the weighted incremental version (this is works since it is only a special case of the weighted version!)
    function UpdateEigenspace(mtx : TDoubleMatrix; const spacialWeights : Array of double; var EigVecs, EigVals, MeanElem, A : TDoubleMatrix) : boolean; overload;
    // note the spacial weights MUST have values between 0 and 1. 1 means no weighting and 0 means that pixel is not taken into account.
    // implementation of Daniejl S. weighted incremental PCA function (p 69ff):
    // this function supports spacial weights as well as temporal weights. If the function without spacial weights is called
    // all weights are set to 1 (no blending with the original image)
    function UpdateEigenspaceWeighted(mtx : TDoubleMatrix; var weights : TDoubleDynArray; var numWeights : integer; var EigVecs, EigVals, MeanElem, A : TDoubleMatrix) : boolean; overload;
    function UpdateEigenspaceWeighted(mtx : TDoubleMatrix; var weights : TDoubleDynArray; var numWeights : integer; const spacialWeights : Array of double; var EigVecs, EigVals, MeanElem, A : TDoubleMatrix) : boolean; overload;

    constructor Create(NumVectorsToKeep : integer);
  end;

{ TSubspaceUpdater }

constructor TSubspaceUpdater.Create(NumVectorsToKeep: integer);
begin
     inherited Create;

     fNumEigenvectorsToKeep := NumVectorsToKeep;
end;

function TSubspaceUpdater.InternalUpdateEigenspace(mtx: TDoubleMatrix; weighted: boolean): boolean;
var normExample : TDoubleMatrix;
    a : TDoubleMatrix;
    r : TDoubleMatrix;
    a1 : TDoubleMatrix;
    u1 : TDoubleMatrix;
    normR : double;
    y : Integer;
    x : Integer;
    pca : TMatrixPCA;
    temp : TDoubleMatrix;
    eigVecsT : TDoubleMatrix;
begin
     Result := False;
     normExample := nil;
     a := nil;
     r := nil;
     a1 := nil;
     u1 := nil;
     temp := nil;
     eigVecsT := nil;

     try
        // project current example to the current feature space
        normExample := mtx.Sub(fMeanElem);
        eigVecsT := fEigVecs.Transpose;
        a := EigVecsT.Mult(normExample);
        FreeAndNil(eigVecsT);

        // reconstruct example
        FreeAndNil(normExample);
        normExample := fEigVecs.Mult(a);
        normExample.AddInplace(fMeanElem);

        // compute residual vector and normalize
        r := mtx.Sub(normExample);
        normR := 0;
        for y := 0 to r.Height - 1 do
            normR := normR + sqr(r[0, y]);
        normR := sqrt(normR);
        // check error -> todo: better Boundary check
        FreeAndNil(normExample);
        if normR < 100*MinDouble then
        begin
             FreeAndNil(r);
             FreeAndNil(a);

             exit;
        end;

        // normalize vector
        r.ScaleInPlace(1/normR);

        // append new basis vector to u
        u1 := TDoubleMatrix.Create(fEigVecs.Width + 1, fEigVecs.Height);
        for x := 0 to fEigVecs.Width - 1 do
            u1.SetColumn(x, fEigVecs, x);
        u1.SetColumn(u1.Width - 1, r);

        FreeAndNil(r);

        // append new value in feature space
        a1 := TDoubleMatrix.Create(fA.Width + 1, fA.Height + 1);
        a1.SetSubMatrix(0, 0, a1.Width, a1.Height - 1);
        for x := 0 to fA.Width - 1 do
            a1.SetColumn(x, fA, x);
        a1.SetColumn(a1.Width - 1, a);
        a1.UseFullMatrix;
        a1[a1.width - 1, a1.Height - 1] := normR;

        FreeAndNil(a);

        // perform full PCA on the new matrix a1
        pca := TMatrixPCA.Create([pcaEigVals]);
        try
           if not weighted
           then
               Result := pca.PCA(a1, 1, True)
           else
               Result := pca.TemporalWeightPCA(a1, 1, True, Copy(fWeights, 0, fNumWeights));

           if not Result then
              exit;

           // project the coefficient vectors to the new basis
           FreeAndNil(fA);

           for x := 0 to a1.Width - 1 do
           begin
                a1.SetSubMatrix(x, 0, 1, a1.Height);
                a1.SubInPlace(pca.Mean);
           end;
           a1.UseFullMatrix;
           fA := pca.EigVecsT.Mult(a1);
           FreeAndNil(a1);

           // update the mean
           temp := u1.Mult(pca.Mean);
           fMeanElem.AddInplace(temp);
           FreeAndNil(temp);

           // rotate the subspace
           u1.MultInPlace(pca.EigVecs);
           FreeAndNil(fEigVecs);
           fEigVecs := u1;
           u1 := nil;

           // discard the last eigenvector -> keep the basis constant
           if (fNumEigenvectorsToKeep > 0) and (fEigVecs.Width > fNumEigenvectorsToKeep) then
           begin
                temp := TDoubleMatrix.Create;
                fEigVecs.SetSubMatrix(0, 0, fEigVecs.Width - 1, fEigVecs.Height);
                temp.Assign(fEigVecs, True);
                FreeAndNil(fEigVecs);
                fEigVecs := temp;

                // remove the last row -> it contains the values of the least significant
                // elements
                fA.SetSubMatrix(0, 0, fA.Width, fA.Height - 1);
           end;

           if Assigned(fEigVals) then
              fEigVals.Assign(pca.EigVals);
        finally
               pca.Free;
        end;
     except
           a.Free;
           a1.Free;
           u1.Free;
           normExample.Free;
           r.Free;
           temp.Free;
           eigVecsT.Free;

           raise;
     end;
end;

function TSubspaceUpdater.PrepareBlendExample(mtx: TDoubleMatrix;
  const spacialWeights: array of double): TDoubleMatrix;
var uPinv : TDoubleMatrix;
    x, y : integer;
    rootWeights : TDoubleMatrix;
    residual : TDoubleMatrix;
    a : TDoubleMatrix;
begin
     uPinv := nil;
     rootWeights := nil;
     Result := nil;
     residual := nil;
     a := nil;
     try
        // #########################################################
        // #### Calculate the coefficients considering the spacial weights
        rootWeights := TDoubleMatrix.Create(1, Length(spacialWeights));
        for y := 0 to rootWeights.Height - 1 do
            rootWeights[0, y] := sqrt(spacialWeights[y]);

        uPinv := TDoubleMatrix.Create;
        uPinv.Assign(fEigVecs);

        // elementwise multiply the elements of U with the spacial weights
        for x := 0 to uPinv.Width - 1 do
        begin
             uPinv.SetSubMatrix(x, 0, 1, uPinv.Height);
             uPinv.ElementWiseMult(rootWeights);
        end;
        uPinv.UseFullMatrix;
        if uPinv.PseudoInversionInPlace <> srOk then
        begin
             FreeAndNil(uPinv);
             FreeAndNil(rootWeights);
             exit;
        end;

        // calculated weighted residual
        residual := mtx.Sub(fMeanElem);
        residual.ElementWiseMult(rootWeights);
        FreeAndNil(rootWeights);

        // now calculate the projected elements in feature space
        a := uPinv.Mult(residual);
        FreeAndNil(uPinv);

        // reconstruct example
        FreeAndNil(residual);
        residual := fEigVecs.Mult(a);
        residual.AddInplace(fMeanElem);

        FreeAndNil(a);

        // ############################################################
        // #### Blend the input and the reconstructed image considering the spatial weights
        Result := TDoubleMatrix.Create(1, mtx.Height);

        for y := 0 to Result.Height - 1 do
            Result[0, y] := mtx[0, y]*spacialWeights[y] + residual[0, y]*(1 - spacialWeights[y]);

        FreeAndNil(residual);
     except
           FreeAndNil(uPinv);
           FreeAndNil(rootWeights);
           FreeAndNil(residual);
           FreeAndNil(a);
           FreeAndNil(Result);
     end;
end;

function TSubspaceUpdater.UpdateEigenspace(mtx: TDoubleMatrix;
  const spacialWeights: array of double; var EigVecs, EigVals, MeanElem,
  A: TDoubleMatrix): boolean;
var blendMtx : TDoubleMatrix;
begin
     // #######################################################
     // #### Execute the generic eigenspace update
     fEigVecs := EigVecs;
     fEigVals := EigVals;
     fMeanElem := MeanElem;
     fA := A;

     blendMtx := PrepareBlendExample(mtx, spacialWeights);
     try
        Result := InternalUpdateEigenspace(blendMtx, False);
     finally
            blendMtx.Free;
     end;

     EigVecs := fEigVecs;
     EigVals := fEigVals;
     MeanElem := fMeanElem;
     A := fA;
end;

function TSubspaceUpdater.UpdateEigenspace(mtx: TDoubleMatrix; var EigVecs,
  EigVals, MeanElem, A: TDoubleMatrix): boolean;
begin
     // #######################################################
     // #### Execute the generic eigenspace update
     assert(Assigned(mtx), 'Error missing data');
     fEigVecs := EigVecs;
     fEigVals := EigVals;
     fMeanElem := MeanElem;
     fA := A;

     Result := InternalUpdateEigenspace(mtx, False);

     EigVecs := fEigVecs;
     EigVals := fEigVals;
     MeanElem := fMeanElem;
     A := fA;
end;

function TSubspaceUpdater.UpdateEigenspaceWeighted(mtx: TDoubleMatrix;
  var weights: TDoubleDynArray; var NumWeights : integer; var EigVecs, EigVals, MeanElem,
  A: TDoubleMatrix): boolean;
begin
     // #######################################################
     // #### Execute the generic eigenspace update
     fEigVecs := EigVecs;
     fEigVals := EigVals;
     fMeanElem := MeanElem;
     fWeights := Weights;
     fNumWeights := numWeights;
     fA := A;

     Result := InternalUpdateEigenspace(mtx, True);

     EigVecs := fEigVecs;
     EigVals := fEigVals;
     MeanElem := fMeanElem;
     A := fA;
     weights := fWeights;
     numWeights := fNumWeights;
end;

function TSubspaceUpdater.UpdateEigenspaceWeighted(mtx: TDoubleMatrix;
  var weights : TDoubleDynArray; var numWeights : integer; const spacialWeights: array of double; var EigVecs, EigVals, MeanElem,
  A: TDoubleMatrix): boolean;
var blendMtx : TDoubleMatrix;
begin
     // #######################################################
     // #### Execute the generic eigenspace update
     fEigVecs := EigVecs;
     fEigVals := EigVals;
     fMeanElem := MeanElem;
     fWeights := weights;
     fNumWeights := numWeights;
     fA := A;

     blendMtx := PrepareBlendExample(mtx, spacialWeights);
     try
        Result := InternalUpdateEigenspace(blendMtx, True);
     finally
            blendMtx.Free;
     end;

     EigVecs := fEigVecs;
     EigVals := fEigVals;
     MeanElem := fMeanElem;
     A := fA;
     numWeights := fNumWeights;
     weights := fWeights;
end;


// ####################################################################
// #### Object Implementations
// ####################################################################

{ TIncrementalPCA }

procedure TIncrementalPCA.AddWeight(const weight: double);
begin
     if Length(fWeights) <= fNumWeights + 1 then
        SetLength(fWeights, Max(10, Min(Length(fWeights) + 1000, 2*fNumWeights)));

     fWeights[fNumWeights] := weight;
     inc(fNumWeights);
end;

constructor TIncrementalPCA.Create(const Data: TIncrPCAData);
begin
     inherited Create(Data.Props);

     fA := Data.A;
     fWeights := Data.Weights;
     fNumWeights := Length(fWeights);
     fNumEigenvectorsToKeep := Data.NumEigenVecs;
end;

constructor TIncrementalPCA.Create(KeepFlags: TPCASaveDataSet);
begin
     inherited Create(KeepFlags);

     fNumEigenvectorsToKeep := -1;
     fNumWeights := 0;
end;

destructor TIncrementalPCA.Destroy;
begin
     fA.Free;

     inherited;
end;

function TIncrementalPCA.GetA: TDoubleMatrix;
begin
     if not Assigned(fA) then
        fA := TDoubleMatrix.Create;

     Result := fA;
end;

// ###############################################
// #### Persistence functions

class function TIncrementalPCA.ClassIdentifier: String;
begin
     Result := 'IPCA';
end;

procedure TIncrementalPCA.DefineProps;
begin
     inherited;

     if Assigned(fA) then
        AddObject('A', fA);
     AddIntProperty('NumEigenvectorsToKeep', fNumEigenvectorsToKeep);
     AddDoubleArr('IncrWeights', fWeights);
end;


procedure TIncrementalPCA.OnLoadDoubleArr(const Name: String;
  const Value: TDoubleDynArray);
begin
     if CompareText(Name, 'IncrWeights') = 0 then
     begin
          fNumWeights := Length(value);
          fWeights := Value;
     end
     else
         inherited;
end;

function TIncrementalPCA.OnLoadObject(const Name: String;
  obj: TBaseMathPersistence): boolean;
begin
     Result := True;
     if CompareText(Name, 'A') = 0
     then
         fA := obj as TDoubleMatrix
     else
         Result := inherited OnLoadObject(Name, Obj);
end;

procedure TIncrementalPCA.OnLoadIntProperty(const Name: String;
  Value: integer);
begin
     if CompareText(Name, 'NumEigenvectorsToKeep') = 0
     then
         fNumEigenvectorsToKeep := Value
     else
         inherited;
end;

function TIncrementalPCA.PCA(Examples: TDoubleMatrix; CutEps: double;
  IsRelativeValue: boolean): boolean;
var origKeepFlags : TPCASaveDataSet;
begin
     fNumWeights := 0;
     fWeights := nil;

     origKeepFlags := fKeepFlags;
     include(fKeepFlags, pcaMeanNormalizedData);

     Result := inherited PCA(Examples, CutEps, IsRelativeValue);

     if Result then
     begin
          fA := EigVecT.Mult(fMeanNormExamples);

          if not (pcaMeanNormalizedData in origKeepFlags) then
             FreeAndNil(fMeanNormExamples);

          if not (pcaTransposedEigVec in origKeepFlags) then
             FreeAndNil(fEigVecsT);

          fKeepFlags := origKeepFlags;
     end;
end;

function TIncrementalPCA.TemporalWeightPCA(Examples: TDoubleMatrix;
  CutEps: double; IsRelativeValue: boolean;
  const Weights: array of Double): boolean;
var origKeepFlags : TPCASaveDataSet;
begin
     SetLength(fWeights, Length(Weights));
     fNumWeights := Length(fWeights);
     SetLength(fWeights, Length(Weights));
     if Length(Weights) > 0 then
        Move(Weights[0], fWeights[0], sizeof(double)*Length(Weights));

     origKeepFlags := fKeepFlags;
     include(fKeepFlags, pcaMeanNormalizedData);

     Result := inherited TemporalWeightPCA(Examples, CutEps, IsRelativeValue, Weights);

     if Result then
     begin
          fA := EigVecT.Mult(fMeanNormExamples);

          if not (pcaMeanNormalizedData in origKeepFlags) then
             FreeAndNil(fMeanNormExamples);

          if not (pcaTransposedEigVec in origKeepFlags) then
             FreeAndNil(fEigVecsT);

          fKeepFlags := origKeepFlags;
     end;
end;

function TIncrementalPCA.UpdateEigenspace(mtx: TDoubleMatrix) : boolean;
var updater : TSubspaceUpdater;
begin
     Result := True;
     assert(mtx.Width = 1, 'Error only one example allowed');

     if not Assigned(fMeanElem) then
     begin
          fMeanElem := TDoubleMatrix.Create;
          fMeanElem.Assign(mtx, True);
          fEigVecs := TDoubleMatrix.Create(1, mtx.Height);
          fA := TDoubleMatrix.Create(1, 1);

          if pcaEigVals in fKeepFlags then
             fEigVals := TDoubleMatrix.Create(1, 1);

          exit;
     end;

     // #######################################################
     // #### Execute the generic eigenspace update
     updater := TSubspaceUpdater.Create(fNumEigenvectorsToKeep);
     try
        Result := updater.UpdateEigenspace(mtx, fEigVecs, fEigVals, fMeanElem, fA);
     finally
            updater.Free;
     end;
end;

function TIncrementalPCA.UpdateEigenspace(mtx: TDoubleMatrix;
  const spacialWeights: array of double): boolean;
var updater : TSubspaceUpdater;
begin
     Result := True;
     assert(mtx.Width = 1, 'Error only one example allowed');
     assert(mtx.Height = length(spacialWeights), 'Dimension error, spatial weights lenght differs from matrix height');

     if not Assigned(fMeanElem) then
     begin
          fMeanElem := TDoubleMatrix.Create;
          fMeanElem.Assign(mtx, True);
          fEigVecs := TDoubleMatrix.Create(1, mtx.Height);
          fA := TDoubleMatrix.Create(1, 1);

          if pcaEigVals in fKeepFlags then
             fEigVals := TDoubleMatrix.Create(1, 1);

          exit;
     end;

     // ########################################################
     // #### Use the subspace updater:
     updater := TSubspaceUpdater.Create(fNumEigenvectorsToKeep);
     try
        Result := updater.UpdateEigenspace(mtx, spacialWeights, fEigVecs, fEigVals, fMeanElem, fA);
     finally
            updater.Free;
     end;
end;

function TIncrementalPCA.UpdateEigenspaceWeighted(mtx: TDoubleMatrix;
  const weight: double; const spacialWeights: array of double): boolean;
var updater : TSubspaceUpdater;
begin
     Result := True;
     assert(mtx.Width = 1, 'Error only one example allowed');
     assert(mtx.Height = length(spacialWeights), 'Dimension error, spatial weights lenght differs from matrix height');

     AddWeight(weight);

     if not Assigned(fMeanElem) then
     begin
          fMeanElem := TDoubleMatrix.Create;
          fMeanElem.Assign(mtx, True);
          fEigVecs := TDoubleMatrix.Create(1, mtx.Height);
          fA := TDoubleMatrix.Create(1, 1);

          if pcaEigVals in fKeepFlags then
             fEigVals := TDoubleMatrix.Create(1, 1);

          exit;
     end;

     // ########################################################
     // #### Use the subspace updater:
     updater := TSubspaceUpdater.Create(fNumEigenvectorsToKeep);
     try
        Result := updater.UpdateEigenspaceWeighted(mtx, fWeights, fNumWeights, spacialWeights, fEigVecs, fEigVals, fMeanElem, fA);
     finally
            updater.Free;
     end;
end;

function TIncrementalPCA.UpdateEigenspaceWeighted(mtx: TDoubleMatrix;
  const weight: double): boolean;
var updater : TSubspaceUpdater;
begin
     Result := True;
     assert(mtx.Width = 1, 'Error only one example allowed');

     AddWeight(weight);

     if not Assigned(fMeanElem) then
     begin
          fMeanElem := TDoubleMatrix.Create;
          fMeanElem.Assign(mtx, True);
          fEigVecs := TDoubleMatrix.Create(1, mtx.Height);
          fA := TDoubleMatrix.Create(1, 1);

          if pcaEigVals in fKeepFlags then
             fEigVals := TDoubleMatrix.Create(1, 1);

          exit;
     end;

     // ########################################################
     // #### Use the subspace updater:
     updater := TSubspaceUpdater.Create(fNumEigenvectorsToKeep);
     try
        Result := updater.UpdateEigenspaceWeighted(mtx, fWeights, fNumWeights, fEigVecs, fEigVals, fMeanElem, fA);
     finally
            updater.Free;
     end;
end;

{ TFastRobustIncrementalPCA }

procedure TFastRobustIncrementalPCA.AddWeight(idx : integer; const weight: double);
begin
     if Length(fWeights[idx]) <= fNumWeights[idx] + 1 then
        SetLength(fWeights[idx], Max(10, Min(Length(fWeights[idx]) + 1000, 2*fNumWeights[idx])));

     fWeights[idx][fNumWeights[idx]] := weight;
     inc(fNumWeights[idx]);
end;

procedure TFastRobustIncrementalPCA.BuildSubspaces(Examples: TDoubleMatrix; origKeepFlags : TPCASaveDataSet);
var i, idx : integer;
    j : integer;
    x : TDoubleMatrix;
    subT : TDoubleMatrix;
begin
     // main subspace
     fA := EigVecT.Mult(fMeanNormExamples);

     if not (pcaMeanNormalizedData in origKeepFlags) then
        FreeAndNil(fMeanNormExamples);

     if not (pcaTransposedEigVec in origKeepFlags) then
        FreeAndNil(fEigVecsT);

     // subsubspaces
     SetLength(fSubA, fProps.NumSubSubSpaces);
     for idx := 0 to Length(fSubEigVecs) - 1 do
     begin
          x := TDoubleMatrix.Create(Examples.Width, fSubMeanElem[idx].Height);
          try
             // project to feature space
             for j := 0 to Examples.Width - 1 do
             begin
                  for i := 0 to x.Height - 1 do
                      x[j, i] := Examples[j, fSubItemIdx[idx][i]];

                  x.SetSubMatrix(j, 0, 1, x.Height);
                  x.SubInPlace(fSubMeanElem[idx]);
                  x.UseFullMatrix;
             end;

             subT := fSubEigVecs[idx].Transpose;
             try
                fSubA[idx] := subT.Mult(x);
             finally
                    subT.Free;
             end;
          finally
                 x.Free;
          end;
     end;
end;

procedure TFastRobustIncrementalPCA.Clear;
var i : integer;
begin
     inherited;

     if not fIsLearning then
     begin
          FreeAndNil(fA);
          for i := 0 to Length(fSubA) - 1 do
              fSubA[i].Free;

          for i := 0 to Length(fSubEigVals) - 1 do
               fSubEigVals[i].Free;

          fSubA := nil;
          fWeights := nil;
          fNumWeights := nil;
          fSubEigVals := nil;
          fSubEigVecsT := nil;
     end;
end;

constructor TFastRobustIncrementalPCA.Create(KeepFlags: TPCASaveDataSet);
begin
     inherited Create(KeepFlags);
end;

constructor TFastRobustIncrementalPCA.Create(const Props: TFastRobustPCADataEx);
var i : integer;
begin
     inherited Create(props.RData);

     fA := props.A;
     fNumEigenvectorsToKeep := Props.NumEigenVecs;
     fSubA := Props.SubA;
     SetLength(fWeights, 1 + Length(Props.SubWeights));
     fWeights[0] := Props.Weights;
     SetLength(fNumWeights, Length(fWeights));
     for i := 0 to Length(fNumWeights) - 1 do
         fNumWeights[i] := Length(fWeights[i]);
     fSubA := Props.SubA;
     for i := 0 to Length(Props.SubWeights) - 1 do
         fWeights[1 + i] := Props.SubWeights[i];
end;

destructor TFastRobustIncrementalPCA.Destroy;
begin
     // inherited calls Clear!
     inherited;
end;

procedure TFastRobustIncrementalPCA.DefineProps;
var i : Integer;
begin
     inherited;

     if Assigned(fA) then
        AddObject('A', fA);
     AddIntProperty('NumEigenvectorsToKeep', fNumEigenvectorsToKeep);
     if Length(fWeights) > 0 then
     begin
          BeginList('IPCAWeights', Length(fWeights));
          for i := 0 to Length(fWeights) - 1 do
              AddListDoubleArr(fWeights[i]);
          EndList;
     end;
     if Length(fNumWeights) > 0 then
        AddIntArr('IPCANumWeights', fNumWeights);
     if Length(fSubA) > 0 then
     begin
          BeginList('IPCASUBA', Length(fSubA));
          for i := 0 to Length(fSubA) - 1 do
              AddObject(fSubA[i]);
          EndList;
     end;
     if Length(fSubEigVals) > 0 then
     begin
          BeginList('IPCASUBEIGVALS', Length(fSubEigVals));
          for i := 0 to Length(fSubEigVals) - 1 do
              AddObject(fSubEigVals[i]);
          EndList;
     end;
end;

class function TFastRobustIncrementalPCA.ClassIdentifier: String;
begin
     Result := 'FRIPCA';
end;

procedure TFastRobustIncrementalPCA.OnLoadBeginList(const Name: String;
  count: integer);
begin
     fIdx := 0;
     if CompareText(Name, 'IPCAWeights') = 0 then
     begin
          fListType := llWeights;
          SetLength(fWeights, Count);
     end
     else if CompareText(Name, 'IPCASUBA') = 0 then
     begin
          fListType := llSubA;
          SetLength(fSubA, Count);
     end
     else if CompareText(Name, 'IPCASUBEIGVALS') = 0 then
     begin
          fListType := llSubEigVals;
          SetLength(fSubEigVals, Count);
     end
     else
         inherited;
end;

procedure TFastRobustIncrementalPCA.OnLoadEndList;
begin
     inherited;

     fListType := llNone;     
end;

procedure TFastRobustIncrementalPCA.OnLoadIntArr(const Name: String;
  const Value: TIntegerDynArray);
begin
     if CompareText(Name, 'IPCANumWeights') = 0
     then
         fNumWeights := Value
     else
         inherited;
end;

procedure TFastRobustIncrementalPCA.OnLoadListDoubleArr(
  const Value: TDoubleDynArray);
begin
     if fListType = llWeights then
     begin
          fWeights[fIdx] := Value;
          inc(fIdx);
     end
     else
         inherited;
end;

function TFastRobustIncrementalPCA.OnLoadObject(const Name: String;
  obj: TBaseMathPersistence): boolean;
begin
     Result := True;
     if CompareText(Name, 'A') = 0
     then
         fA := obj as TDoubleMatrix
     else
         Result := inherited OnLoadObject(Name, Obj);
end;

procedure TFastRobustIncrementalPCA.OnLoadIntProperty(const Name: String;
 Value: integer);
begin
     if CompareText(Name, 'NumEigenvectorsToKeep') = 0
     then
         fNumEigenvectorsToKeep := Value
     else
         inherited;
end;

function TFastRobustIncrementalPCA.OnLoadObject(
  Obj: TBaseMathPersistence): boolean;
begin
     Result := True;
     if fListType = llSubA then
     begin
          fSubA[fIdx] := obj as TDoubleMatrix;
          inc(fIdx);
     end
     else if fListType = llSubEigVals then
     begin
          fSubEigVals[fIdx] := obj as TDoubleMatrix;
          inc(fIdx);
     end
     else
         Result := inherited OnLoadObject(obj);
end;

function TFastRobustIncrementalPCA.PCA(Examples: TDoubleMatrix; CutEps: double;
  IsRelativeValue: boolean): boolean;
var origKeepFlags : TPCASaveDataSet;
begin
     Clear;

     fNumWeights := nil;
     fWeights := nil;

     origKeepFlags := fKeepFlags;
     include(fKeepFlags, pcaMeanNormalizedData);

     Result := inherited PCA(Examples, CutEps, IsRelativeValue);

     if Result then
     begin
          BuildSubspaces(Examples, origKeepFlags);
           
          fKeepFlags := origKeepFlags;
     end;
end;

function TFastRobustIncrementalPCA.TemporalWeightPCA(Examples: TDoubleMatrix;
  CutEps: double; IsRelativeValue: boolean;
  const Weights: array of Double): boolean;
var origKeepFlags : TPCASaveDataSet;
    i : Integer;
    sortWeights : TDoubleDynArray;
begin
     Clear;

     SetLength(fNumWeights, fProps.NumSubSubSpaces + 1);
     SetLength(fWeights, fProps.NumSubSubSpaces + 1);

     // build the list as it would be done incrementaly (sorted!)
     SetLength(sortWeights, Length(Weights));
     if Length(Weights) > 0 then
        Move(Weights[0], sortWeights[0], Length(Weights)*sizeof(double));
     QuickSort(sortWeights[0], sizeof(double), Length(Weights), DoubleSortFunc);
     
     for i := 0 to Length(fNumWeights) - 1 do
     begin
          fWeights[i] := Copy(sortWeights, 0, Min(fNumEigenvectorsToKeep, Length(Weights)));
          fNumWeights[i] := Length(fWeights[i]);
     end;

     // ###############################################################
     // #### Build Sub + SubSubspaces
     origKeepFlags := fKeepFlags;
     include(fKeepFlags, pcaMeanNormalizedData);

     Result := inherited TemporalWeightPCA(Examples, CutEps, IsRelativeValue, Weights);

     // build the A matrices of all subspaces
     if Result then
     begin
          BuildSubspaces(Examples, origKeepFlags);

          fKeepFlags := origKeepFlags;
     end;
end;

function TFastRobustIncrementalPCA.UpdateEigenspace(
  mtx: TDoubleMatrix): boolean;
var spacialWeights : TDoubleDynArray;
begin
     spacialWeights := nil;
     Result := UpdateEigenspace(mtx, spacialWeights);
end;

function TFastRobustIncrementalPCA.UpdateEigenspace(mtx: TDoubleMatrix;
  const spacialWeights: array of double): boolean;
begin
     Result := UpdateEigenspaceWeighted(mtx, -1);
end;

function TFastRobustIncrementalPCA.UpdateEigenspaceWeighted(mtx: TDoubleMatrix;
  const weight: double; const spacialWeights: array of double): boolean;
var i, j : integer;
    subExample : TDoubleMatrix;
    pcaUpdater : TSubspaceUpdater;
    subSpaceWeights : TDoubleDynArray;
    example : TDoubleMatrix;
    eigValMtx : TDoubleMatrix;
begin
     Result := True;
     assert(mtx.Width = 1, 'Error only one example allowed');

     subSpaceWeights := nil;
     // #######################################################
     // #### Update sub sub spaces
     fIsLearning := True;
     try
        if not Assigned(fMeanElem) then
        begin
             SetLength(fSubItemIdx, fProps.NumSubSubSpaces);
             SetLength(fSubEigVecs, fProps.NumSubSubSpaces);
             SetLength(fSubMeanElem, fProps.NumSubSubSpaces);
             SetLength(fSubEigVecsT, fProps.NumSubSubSpaces);
             SetLength(fSubEigVals, fProps.NumSubSubSpaces);
             SetLength(fSubA, fProps.NumSubSubSpaces);
        end;
        for i := 0 to fProps.NumSubSubSpaces - 1 do
        begin
             if fSubItemIdx[i] = nil then
                fSubItemIdx[i] := GenerateSampleList(mtx.Height, i);

             SubExample := TDoubleMatrix.Create(mtx.Width, Length(fSubItemIdx[i]));
             try
                if Length(spacialWeights) > 0 then
                begin
                     SetLength(subSpaceWeights, SubExample.Height);
                     for j := 0 to Length(subSpaceWeights) - 1 do
                         subSpaceWeights[j] := spacialWeights[fSubItemIdx[i][j]];
                end;

                for j := 0 to Length(fSubItemIdx[i]) - 1 do
                    SubExample.SetRow(j, mtx, fSubItemIdx[i][j]);

                if weight > 0 then
                   AddWeight(i + 1, weight);

                if not Assigned(fMeanElem) then
                begin
                     fSubMeanElem[i] := TDoubleMatrix.Create;
                     fSubMeanElem[i].Assign(SubExample, True);
                     fSubEigVecs[i] := TDoubleMatrix.Create(1, SubExample.Height);
                     fSubA[i] := TDoubleMatrix.Create(1, 1);

                     if pcaEigVals in fKeepFlags then
                        fSubEigVals[i] := TDoubleMatrix.Create(1, 1);
                     if pcaTransposedEigVec in fKeepFlags then
                        fSubEigVecsT[i] := TDoubleMatrix.Create(1, mtx.Height);
                end
                else
                begin
                     pcaUpdater := TSubspaceUpdater.Create(fNumEigenvectorsToKeep);
                     try
                        example := SubExample;
                        try
                           if Length(spacialWeights) <> 0 then
                              example := pcaUpdater.PrepareBlendExample(SubExample, subSpaceWeights);

                           eigValMtx := nil;
                           if Length(fSubEigVals) > 0 then
                              eigValMtx := fSubEigVals[i];

                           if weight > 0
                           then
                               pcaUpdater.UpdateEigenspaceWeighted(Example, fWeights[i + 1], fNumWeights[i + 1], fSubEigVecs[i], eigValMtx, fsubMeanElem[i], fSubA[i])
                           else
                               pcaUpdater.UpdateEigenspace(Example, fSubEigVecs[i], eigValMtx, fsubMeanElem[i], fSubA[i]);

                        finally
                               if example <> SubExample then
                                  FreeAndNil(example);
                        end;
                        fSubEigVecsT[i].Free;

                        if pcaTransposedEigVec in fKeepFlags then
                           fSubEigVecsT[i] := fSubEigVecs[i].Transpose;
                     finally
                            pcaUpdater.Free;
                     end;
                end;
             finally
                    SubExample.Free;
             end;
        end;

        // #######################################################
        // #### Update full sub space
        if weight > 0 then
           AddWeight(0, weight);

        if not Assigned(fMeanElem) then
        begin
             fMeanElem := TDoubleMatrix.Create;
             fMeanElem.Assign(mtx, True);
             fEigVecs := TDoubleMatrix.Create(1, mtx.Height);
             fA := TDoubleMatrix.Create(1, 1);

             fEigVals := nil;
             if pcaEigVals in fKeepFlags then
                fEigVals := TDoubleMatrix.Create(1, 1);

             exit;
        end;

        // #######################################################
        // #### Execute the generic eigenspace update
        pcaUpdater := TSubspaceUpdater.Create(fNumEigenvectorsToKeep);
        try
           example := mtx;
           try
              if Length(spacialWeights) <> 0 then
                 example := pcaUpdater.PrepareBlendExample(mtx, spacialWeights);

              if weight > 0
              then
                  Result := pcaUpdater.UpdateEigenspaceWeighted(example, fWeights[0], fNumWeights[0], fEigVecs, fEigVals, fMeanElem, fA)
              else
                  Result := pcaUpdater.UpdateEigenspace(example, fEigVecs, fEigVals, fMeanElem, fA);
           finally
                  if example <> mtx then
                     FreeAndNil(example);
           end;
        finally
               pcaUpdater.Free;
        end;
     finally
            fIsLearning := False;
     end;
end;

function TFastRobustIncrementalPCA.UpdateEigenspaceWeighted(mtx: TDoubleMatrix;
  const weight: double): boolean;
var spacialWeights : TDoubleDynArray;
begin
     spacialWeights := nil;
     Result := UpdateEigenspaceWeighted(mtx, weight, spacialWeights)
end;

initialization
  RegisterMathIO(TIncrementalPCA);
  RegisterMathIO(TFastRobustIncrementalPCA);

end.
