// ###################################################################
// #### This file is part of the mathematics library project, and is
// #### offered under the licence agreement described on
// #### http://www.mrsoft.org/
// ####
// #### Copyright:(c) 2014, Michael R. . All rights reserved.
// ####
// #### Unless required by applicable law or agreed to in writing, software
// #### distributed under the License is distributed on an "AS IS" BASIS,
// #### WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// #### See the License for the specific language governing permissions and
// #### limitations under the License.
// ###################################################################

unit CCA;

interface

uses SysUtils, Classes, Matrix, MatrixConst, BaseMathPersistence;

// ########################################################
// #### Implementation of the Canonical Correlation Analysis based on
// #### Singular Value Decomposition

// based on: Melzer T., Reiter M., Bischof H., "Appearance models based on
// kernel canonical correlation analysis", Pattern Recognition 36,
// 1961-1971, 2003
type

  { TMatrixCCA }

  TMatrixCCA = class(TMatrixClass)
  private
    fProgress: TMtxProgress;
    fWxT : IMatrix;
    fWyT : IMatrix;
    fR : IMatrix;
    fInvWy : IMatrix;   // used in the project method
    fMeanX : IMatrix;   // used in the project method
    fMeanY : IMatrix;   // used in the project method

    fLastProgress : integer;
    fBaseProgress : integer;
    function InvertAndSQRT(mtx : IMatrix) : IMatrix;
    procedure DoProgress(Progress : integer);
    procedure InvProgress( Progress : integer);
    procedure SVDProgress( Progress : integer);
  protected
    class function ClassIdentifier : String; override;
    procedure DefineProps; override;
    function PropTypeOfName(const Name : string) : TPropType; override;

    function OnLoadObject(const Name : String; Obj : TBaseMathPersistence) : boolean; override;
  public
    property WxT : IMatrix read fWxT;
    property WyT : IMatrix read fWyT;
    property R : IMatrix read fR;

    property OnProgress : TMtxProgress read fProgress write fProgress;

    function Project(X : TDoubleMatrix) : TDoubleMatrix;

    procedure CCA(X, Y: TDoubleMatrix; doRegularization : Boolean = True; Lamda : double = 1e-5);
  end;

implementation

uses Math, MathUtilFunc;

{ TMatrixCCA }

function TMatrixCCA.InvertAndSQRT(mtx: IMatrix): IMatrix;
var u, v, w : IMatrix;
    tolerance : double;
    i, j : Integer;
begin
     // compute invCxx = inversion of the principal square root of matrix mtx
     // -> since A is a covariance matrix the principal square root
     // (which is numerically most stable calculated from the schur decomposition)
     // can be calculated with the SVD since both are the same in that case
     // -> use that same decomposition as well for the inverting process!
     if mtx.SVD(U, V, W, True) <> srOk then
        raise ELinEQSingularException.Create('Error could not invert covariance matrix C');
     V.TransposeInPlace;
        
     // main algorithm see MatrixPseudoinverse
     tolerance := w.height*eps(w.Max);

     for i := 0 to W.Height - 1 do
     begin
          if sqr(W[0, i]) <= tolerance
          then
              W[0, i] := 0
          else
              W[0, i] := 1/sqrt(W[0, i]);
     end;

     // compute inversion by inv = V*W*U'
     U.TransposeInPlace;
     for i := 0 to U.Height - 1 do
     begin
          for j := 0 to U.Width - 1 do
              U[j, i] := U[j, i]*W[0, i];
     end;

     Result := V.Mult(U);
end;

procedure TMatrixCCA.DoProgress(Progress: integer);
begin
     if Assigned(fProgress) and (Progress > fLastProgress) then
        fProgress(Self, Progress);

     fLastProgress := Progress;
end;

procedure TMatrixCCA.InvProgress(Progress: integer);
begin
     DoProgress(fBaseProgress + Progress*37 div 100);
end;

procedure TMatrixCCA.SVDProgress(Progress: integer);
begin
     DoProgress(fBaseProgress + progress * 11 div 100);
end;

procedure TMatrixCCA.CCA(X, Y: TDoubleMatrix; doRegularization: Boolean;
  Lamda: double);
var meanNormX : IMatrix;
    meanNormY : IMatrix;
    Cxx, Cyy, Cxy : IMatrix;
    invCxx, invCyy : IMatrix;
    U, V, W : IMatrix;
    tmp : IMatrix;
    counter: Integer;
    N : integer;
    p, q : integer;
    numCC : integer;
begin
     fLastProgress := -1;

     N := X.Width;
     p := X.Height;
     q := Y.Height;

     assert(N = y.Width, 'Error matrices height must be the same');

     DoProgress(0);

     // ##################################################
     // #### mean normalize data
     fMeanX := X.Mean(True);
     fMeanY := Y.Mean(True);

     meanNormX := X.SubVec(fMeanX, False);
     meanNormY := Y.SubVec(fMeanY, False);

     DoProgress(2);

     // ##################################################
     // #### overall covariance matrix C = [Cxx Cxy; Cyx Cyy]
     // #### used method is more efficient than computing cov([X; Y]) directly
     tmp := meanNormX.Transpose;
     Cxx := meanNormX.Mult(tmp);
     Cxx.ScaleInPlace(1/(N-1));

     DoProgress(3);

     tmp := meanNormY.Transpose;
     Cyy := meanNormY.Mult(tmp);
     Cyy.ScaleInPlace(1/(N-1));

     DoProgress(5);

     Cxy := meanNormX.Mult(tmp);
     Cxy.ScaleInPlace(1/(N-1));

     DoProgress(8);
  //   Cyx := Cxy.Transpose;

     // ##################################################
     // ##### Regularization:
     // #####  Even if Cxx and Cyy have full rank the matrix B = [Cxx 0; 0 Cyy] will
     // #####  become singular. An approach to deal with these singularity is to add a
     // #####  multiple of the identity matrix to these matrices. As a result the
     // #####  matrices Cxx, Cyy and B are rendered positive definite. The original
     // #####  eigenvalues can be achieved by subtracting the values added before from
     // #####  the computed eigenvalues.
     if doRegularization then
     begin
          for counter := 0 to Min(N, p) - 1 do
              Cxx[counter, counter] := Cxx[counter, counter] + Lamda;
          for counter := 0 to Min(N, q) - 1 do
              Cyy[counter, counter] := Cyy[counter, counter] + Lamda;
     end;

     // ####################################################
     // ##### compute projection matrices
     // ####################################################

     // ####################################################
     // #### compute inverse matrices
     fBaseProgress := 8;
     cyy.LinEQProgress := {$IFDEF FPC}@{$ENDIF}InvProgress;
     invCyy := InvertAndSQRT(Cyy);
     DoProgress( 45 );
     fBaseProgress := 45;
     Cxx.LinEQProgress := {$IFDEF FPC}@{$ENDIF}InvProgress;
     invCxx := InvertAndSQRT(Cxx);
     DoProgress(82);

     // ####################################################
     // #### compute Wx, Wy.
     tmp := TDoubleMatrix.Create;
     tmp.Assign(invCxx, True);
     tmp.MultInPlace(cxy);
     tmp.MultInPlace(invCyy);

     DoProgress(83);
     fBaseProgress := 83;

     tmp.LinEQProgress := {$IFDEF FPC}@{$ENDIF}svdProgress;
     if tmp.SVD(U, V, W, True) <> srOk then
        raise ELinEQSingularException.Create('Error could not calculate SVD');

     V.TransposeInPlace;

     DoProgress(95);
     // ####################################################
     // #### Compute cannonical correlation vectors
     invCxx.MultInPlace(U);
     invCyy.MultInPlace(V);

     // number of CC's is min p, q!
     numCC := min(p, q);
     invCxx.SetSubMatrix(0, 0, numCC, invCxx.Height);
     fWxT := invCxx.Transpose;

     DoProgress(97);

     invCyy.SetSubMatrix(0, 0, numCC, invCyy.Height);
     fWyT := invCyy.Transpose;

     W.SetSubMatrix(0, 0, 1, numCC);
     fR := MatrixClass.Create;
     fR.Assign(W);

     // ###########################################
     // #### Prepare for projection method
     fInvWy := fWyT.Transpose;
     if fInvWy.InvertInPlace = leSingular then
        raise Exception.Create('Error cannot invert Wy for projection');

     fInvWy.MultInPlace(fWxT);

     DoProgress(100);
end;

function TMatrixCCA.Project(X: TDoubleMatrix): TDoubleMatrix;
var tmp : IMatrix;
begin
     if not Assigned(fInvWy) then
        raise Exception.Create('Error inverted matrix not ready');

     // Result := invWy*Wx'*(X-fMeanX) + fMeany
     tmp := X.Sub(fMeanX);
     Result := fInvWy.Mult(tmp);
     Result.AddInplace(fMeanY);
end;

// ######################################################################
// #### persistence functionality
// ######################################################################

const cCCAIdentifier = 'CCA';
      cCCAR = 'R';
      cCCAWx = 'Wx';
      cCCAWy = 'Wy';
      cCCAInvWy = 'invWy';
      cCCAMeanX = 'meanX';
      cCCAMeanY = 'meanY';

class function TMatrixCCA.ClassIdentifier: String;
begin
     Result := cCCAIdentifier;
end;

procedure TMatrixCCA.DefineProps;
begin
     if Assigned(fR) then
        AddObject(cCCAR, R.GetObjRef);
     if Assigned(fWxT) then
        AddObject(cCCAWx, fWxT.GetObjRef);
     if Assigned(fWyT) then
        AddObject(cCCAWy, fWyT.GetObjRef);
     if Assigned(fInvWy) then
        AddObject(cCCAInvWy, fInvWy.GetObjRef);
     if Assigned(fMeanX) then
        AddObject(cCCAMeanX, fMeanX.GetObjRef);
     if Assigned(fMeanY) then
        AddObject(cCCAMeanY, fMeanY.GetObjRef);
end;

function TMatrixCCA.PropTypeOfName(const Name: string): TPropType;
begin
     if SameText(Name, cCCAR) or SameText(Name, cCCAWx) or SameText(Name, cCCAWy) or
        SameText(Name, cCCAInvWy) or SameText(Name, cCCAMeanX) or SameText(Name, cCCAMeanY)
     then
         Result := ptObject
     else
         Result := inherited PropTypeOfName(Name);
end;

function TMatrixCCA.OnLoadObject(const Name: String;
  Obj: TBaseMathPersistence): boolean;
begin
     Result := True;
     if SameText(Name, cCCAR)
     then
         fR := obj as IMatrix
     else if SameText(Name, cCCAWx)
     then
         fWxT := obj as IMatrix
     else if SameText(Name, cCCAWy)
     then
         fWyT := obj as IMatrix
     else if SameText(Name, cCCAInvWy) 
     then
         fInvWy := obj as IMatrix
     else if SameText(Name, cCCAMeanY) 
     then
         fMeanY := obj as IMatrix
     else
         Result := inherited OnLoadObject(Name, Obj);
end;

initialization
   RegisterMathIO(TMatrixCCA);


end.
