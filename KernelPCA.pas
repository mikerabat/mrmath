unit KernelPCA;

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

interface

// ###############################################
// #### kernel PCA related class
// #### a very simple implementation following the
// #### paper from Schölkopf et al: "Nonlinear Component Analysis as a Kernel
// #### Eigenvalue Problem". This class follows the tutorial from Ambarish Jash
// ###############################################

uses SysUtils, Classes, Matrix, PCA, Types, MatrixConst, BaseMathPersistence;

type
  TKernelMapping = (kmGauss, kmPoly);
  TKernelPCAProps = record
    mapping : TKernelMapping;
    doPseudoCenter : boolean;
    case TKernelMapping of
      kmGauss : ( sigma : double );
      kmPoly : ( Poly : Integer; C : double );
  end;
  TKernelPCA = class(TCommonPCAClass)
  private
    fSigma : double;
    fP : integer;
    fC : double;
    fEigVecs : IMatrix;  // scaled eigenvectors (scaled by 1/sqrt(eigval[i])
    fEigVals : IMatrix;
    fMapping : TKernelMapping;
    fSigmaExp : double;
    fPseudoCenter : boolean;
    fK : IMatrix;
    fOrigX : IMatrix;

    procedure GaussExpFunc( var value : double );
    procedure PolyFunc( var value : double );

    function GaussMapping(X : IMatrix) : IMatrix;
    function PolyMapping(X : IMatrix) : IMatrix;

    function PseudoCenter( K : IMatrix ) : IMatrix;

    procedure Clear;
    function ProjectXToFeatureSpace: IMatrix;  // projects the fK_cent matrix to the feature space
  protected
    procedure DefineProps; override;
    function PropTypeOfName(const Name: string): TPropType; override;

    class function ClassIdentifier : String; override;
    function OnLoadObject(const Name : String; obj : TBaseMathPersistence) : boolean; override;
    procedure OnLoadIntProperty(const Name : String; Value : integer); override;
    procedure OnLoadDoubleProperty(const Name : String; const Value : double); override;
  public
    procedure SetProperties(const props : TKernelPCAProps);

    function KernelPCA( X : TDoubleMatrix; cutEPS : double; isRelativeValue : boolean; ProjData : TDoubleMatrix) : boolean;
    function ProjectToFeatureSpace(Example : TDoubleMatrix) : TDoubleMatrix;

    constructor Create;

    class function KernelPCAPoly( X : TDoubleMatrix; cutEPS : double; isRelativeValue : boolean; p : integer; c : double ) : TDoubleMatrix;
    class function KernelPCAGauss( X : TDoubleMatrix; cutEPS : double; isRelativeValue : boolean; sigma : double = 1) : TDoubleMatrix;
  end;

implementation

uses MatrixASMStubSwitch, Math;

{ TKernelPCA }

constructor TKernelPCA.Create;
begin
     inherited Create;

     fPseudoCenter := True;
     fMapping := kmGauss;
     fSigma := 1;
end;

procedure TKernelPCA.Clear;
begin
     fEigVecs := nil;
     fEigVals := nil;
     fK := nil;
     fOrigX := nil;
end;

procedure TKernelPCA.SetProperties(const props: TKernelPCAProps);
begin
     fSigma := 1;
     fP := 1;
     fC := 0;

     fMapping := props.mapping;
     case fmapping of
       kmGauss: fSigma := props.sigma;
       kmPoly: begin
                    fC := props.C;
                    fP := props.Poly;
               end;
     end;
end;

// ###############################################################
// #### Main algorithm
// ###############################################################

function TKernelPCA.KernelPCA(X: TDoubleMatrix; cutEPS: double;
  isRelativeValue: boolean; ProjData : TDoubleMatrix): boolean;
var covMtx : IMatrix;
    mv : IMatrix;
    sumEigVals : double;
    i : integer;
    tmp : IMatrix;
begin
     Clear;

     // note the covariance method assumes row wise items -> we assume column wise examples
     fOrigX := X.Transpose;

     case fMapping of
       kmGauss: covMtx := GaussMapping(fOrigX);
       kmPoly: covMtx := PolyMapping(fOrigX);
     end;

     // pseudo center...
     fK := covMtx.Clone;
     if fPseudoCenter then
        covMtx := PseudoCenter(covMtx);

     // now we can perform the same decomposition - it's returning the same
     // values as eigen decomposition for covariance matrices but is faster in this libray
     Result := covMtx.SVD(fEigVecs, mv, fEigVals, True) = srOk;

     if Result then
     begin
          try
             if IsRelativeValue then
             begin
                  sumEigVals := 0;
                  for i := 0 to fEigVals.Height - 1 do
                      sumEigVals := sumEigVals + fEigVals[0, i];

                  Assert((CutEps >= 0) and (CutEps <= 1), 'error cut eps is not between 0 and 1');
                  CutEps := sumEigVals*CutEps;
             end
             else
             begin
                  sumEigVals := 0;
                  for i := 0 to Min(fEigVals.Height, Round(CutEps)) - 1 do
                      sumEigVals := sumEigVals + fEigVals[0, i];

                  cutEps := sumEigVals;
             end;

             // sort the eigenvectors and eigenvalues
             SortEigValsEigVecs(fEigVals.GetObjRef, fEigVecs.GetObjRef, 0, fEigVals.Height - 1);

             // shrink eigenspace according to the given parameter
             // at least one eigenvector shall be kept
             sumEigVals := fEigVals[0, 0];
             for i := 1 to fEigVals.Height - 1 do
             begin
                  sumEigVals := sumEigVals + fEigVals[0, i];

                  if CutEps < sumEigVals then
                  begin
                       // shrink space to 0..i
                       fEigVals.SetSubMatrix(0, 0, 1, i);
                       mv := MatrixClass.Create;
                       mv.Assign(fEigVals, True);
                       fEigVals := mv;

                       fEigVecs.SetSubMatrix(0, 0, i, fEigVecs.Height);
                       mv := MatrixClass.Create;
                       mv.Assign(fEigVecs, True);
                       fEigVecs := mv;

                       break;
                  end;
             end;

             // scaling of eigenvalues and vectors
             // -> to satisfy N*lamda*K*alpha = K*alpha
             for i := 0 to fEigVecs.Width - 1 do
             begin
                  fEigVecs.SetSubMatrix(i, 0, 1, fEigVecs.Height);
                  fEigVecs.ScaleInPlace(1/sqrt(fEigVals.Vec[i]*X.Height));
             end;
             fEigVecs.UseFullMatrix;

             if Assigned(ProjData) then
             begin
                  tmp := ProjectXToFeatureSpace;
                  ProjData.TakeOver(tmp);
                  tmp := nil;
             end;
          except
                Clear;
                raise;
          end;
     end
     else
         Clear;
end;

function TKernelPCA.ProjectToFeatureSpace(
  Example: TDoubleMatrix): TDoubleMatrix;
var k : IMatrix;
    x : IMatrix;
begin
     x := Example.Transpose;
     
     // following https://stats.stackexchange.com/questions/126014/how-to-project-a-new-vector-onto-the-pc-space-using-kernel-pca
     case fMapping of
       kmGauss: k := GaussMapping(x);
       kmPoly: k := PolyMapping(x);
     end;

     // K_test~ = K_test -1´M K - K_Test + 1'M * K * 1M
     // 1M is mxm * 1/M... L = height of eigenvector. M = width K
     // 1'M = LxM * 1/M
     if fPseudoCenter then
        k := PseudoCenter(k);

     // note: the eigenvectors are already scaled!
     Result := k.Mult( fEigVecs );
end;

// ###############################################
// #### Kernel mappings
// #### Base idea from https://github.com/steven2358/kmbox

function TKernelPCA.GaussMapping(X: IMatrix): IMatrix;
var covMtx : IMatrix;
    norm1, norm2 : IMatrix;
begin
     norm1 := X.ElementWiseMult(X);
     norm1.SumInPlace(True, True);
     norm2 := fOrigX.ElementWiseMult(fOrigX);
     norm2.SumInPlace(True, False);
     norm2.TransposeInPlace;

     covMtx := X.MultT2(fOrigX);
     covMtx.ScaleInPlace(-2);
     covMtx.AddVecInPlace(norm1, False);
     covMtx.AddVecInPlace(norm2, True);
     fSigmaExp := -1/(2*fSigma*fSigma);

     covMtx.ElementwiseFuncInPlace( GaussExpFunc );
     Result := covMtx;
end;

function TKernelPCA.PolyMapping(X: IMatrix): IMatrix;
begin
     assert( fP > 0, 'Poly mapping needs an exponent > 1');
     Result := X.MultT2(fOrigX);
     if fC <> 0 then
        Result.AddInplace(fC);
     Result.ElementwiseFuncInPlace( PolyFunc );
end;

procedure TKernelPCA.GaussExpFunc(var value: double);
begin
     value := exp( fSigmaExp*value );
end;

procedure TKernelPCA.PolyFunc(var value: double);
begin
     value := IntPower(value, fP);
end;

function TKernelPCA.PseudoCenter(K: IMatrix): IMatrix;
var ones1, ones2 : IMatrix;
    tmp : IMatrix;
    tmp2 : IMatrix;
begin
     // one_mat = ones(size(K));
     // this is for all
     // K_center = K - one_mat*K - K*one_mat + one_mat*K*one_mat;
     // we need that since the pca expects mean centered data


     // this is for a test item
     // K_test~ = K_test -1´M K - K_Test*1M + 1'M * K * 1M
     // 1M is mxm * 1/M... L = height of K. M = width K
     // 1'M = LxM * 1/M
     ones1 := MatrixClass.Create( fK.Width, fK.Height, 1/fK.Width);
     if fK = K
     then
         ones2 := ones1
     else
         ones2 := MatrixClass.Create( fK.Width, K.Height, 1/fk.Width);

     tmp := ones2.MultT2(fK);
     Result := K.Sub(tmp);

     tmp2 := K.MultT2(ones1);
     Result.SubInPlace(tmp2);
     tmp2 := nil;
     tmp := tmp.MultT2(ones1);
     Result.AddInplace(tmp);
end;

function TKernelPCA.ProjectXToFeatureSpace: IMatrix;
var k : IMatrix;
begin
     // Projecting the data in lower dimensions
     k := fK;
     if fPseudoCenter then
        k := PseudoCenter(fK);
     Result := k.Mult(fEigVecs);
end;

// ##################################################
// #### Class functions for direct call of Kernel PCA
// ##################################################

class function TKernelPCA.KernelPCAPoly(X: TDoubleMatrix; cutEPS: double;
  isRelativeValue: boolean; p : integer; c: double): TDoubleMatrix;
var props : TKernelPCAProps;
begin
     props.mapping := kmPoly;
     props.Poly := p;
     props.C := c;

     Result := nil;
     with TKernelPCA.Create do
     try
        SetProperties( props );
        Result := MatrixClass.Create; 
        if not KernelPCA(X, cutEPS, isRelativeValue, Result) then
           FreeAndNil(Result);
     finally
            Free;
     end;
end;

class function TKernelPCA.KernelPCAGauss(X: TDoubleMatrix; cutEPS: double;
  isRelativeValue: boolean; sigma: double): TDoubleMatrix;
var props : TKernelPCAProps;
begin
     props.mapping := kmGauss;
     props.sigma := sigma;

     Result := nil;
     with TKernelPCA.Create do
     try
        SetProperties( props );
        Result := MatrixClass.Create; 
        if not KernelPCA(X, cutEPS, isRelativeValue, Result) then
           FreeAndNil(Result);
     finally
            Free;
     end;
end;

// ###########################################
// #### Persistence functions
// ###########################################

class function TKernelPCA.ClassIdentifier: String;
begin
     Result := 'KernelPCA';
end;

procedure TKernelPCA.DefineProps;
begin
     if not Assigned(fEigVecs) then
        exit;

     AddObject('pcavec', fEigVecs.GetObjRef);

     if Assigned(fEigVals) then
        AddObject('eigvals', fEigVals.GetObjRef);
     if assigned(fOrigX) then
        AddObject('origData', fOrigX.GetObjRef);
     if Assigned(fK) then
        AddObject('K', fK.GetObjRef);

     AddIntProperty('Mapping', Integer(fMapping) );
     AddDoubleProperty('Sigma', fSigma);
     AddIntProperty('P', fP);
     AddDoubleProperty('C', fC);
     AddIntProperty('DoPseudoCenter', Integer(fPseudoCenter));
end;

function TKernelPCA.PropTypeOfName(const Name: string): TPropType;
begin
     if (CompareText(Name, 'pcavec') = 0) or (CompareText(Name, 'eigvals') = 0) or
        (CompareText(Name, 'origData') = 0) or (CompareText(Name, 'K') = 0)
     then
         Result := ptObject
     else
         Result := inherited PropTypeOfName(Name);
end;

procedure TKernelPCA.OnLoadDoubleProperty(const Name: String;
  const Value: double);
begin
     if CompareText(Name, 'Sigma') = 0 
     then
         fSigma := value
     else if CompareText(Name, 'C') = 0 
     then 
          fC := Value
     else
         inherited;
end;

procedure TKernelPCA.OnLoadIntProperty(const Name: String; Value: integer);
begin
     if CompareText(Name, 'P' ) = 0 
     then
         fP := value
     else if CompareText(Name, 'DoPseudoCenter') = 0
     then
         fPseudoCenter := Boolean(Value)
     else if CompareText(Name, 'Mapping') = 0
     then
         fMapping := TKernelMapping( value )
     else
         inherited;
end;

function TKernelPCA.OnLoadObject(const Name: String;
  obj: TBaseMathPersistence) : boolean;
begin
     Result := True;
     if CompareText(name, 'pcavec') = 0
     then
         fEigVecs := obj as TDoubleMatrix
     else if CompareText(name, 'eigvals') = 0 
     then
         fEigVals := obj as TDoubleMatrix
     else if CompareText(name, 'origData') = 0 
     then
         fOrigX := obj as TDoubleMatrix
     else if CompareText(name, 'K') = 0 
     then
         fK := obj as TDoubleMatrix
     else
         Result := False;
end;

initialization
  RegisterMathIO(TKernelPCA);

end.
