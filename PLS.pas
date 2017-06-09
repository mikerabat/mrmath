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

// special thanks to Gustav Kaiser for implementing the algorithm

unit PLS;

interface

Uses SysUtils, MatrixConst, Matrix, BaseMathPersistence;

// ###########################################
// #### Partial least squares   
// ###########################################
Type
  TMatrixPLS = Class(TMatrixClass)
  private
    fSSQDif: TDoubleMatrix;
    fCW: TDoubleMatrix;
    fW: TDoubleMatrix;
    fBeta: TDoubleMatrix;
    fYRes: TDoubleMatrix;
    fTheta: TDoubleMatrix;
  protected
    procedure Clear;
    // part of matlabs "\" algo
    function mlDivide(A, y: TDoubleMatrix): IMatrix;

    // persistence
    procedure DefineProps; override;
    function PropTypeOfName(const Name : string) : TPropType; override;

    class function ClassIdentifier : String; override;
    function OnLoadObject(const Name : String; obj : TBaseMathPersistence) : boolean; override;
  public
    destructor Destroy; override;
    procedure Plsr(Xreg, Yreg: TDoubleMatrix; NInput, NComp: Integer);

    function Project(x : TDoubleMatrix) : TDoubleMatrix;
    
    // out
    property SSQDif: TDoubleMatrix read fSSQDif;
    property CW: TDoubleMatrix read fCW;
    property W: TDoubleMatrix read fW;
    property Beta: TDoubleMatrix read fBeta;
    property YRes: TDoubleMatrix read fYRes;         // residual when calculating plsr
    property Theta: TDoubleMatrix read fTheta;       
  End;



implementation

{ TMatrixPLS }

procedure TMatrixPLS.Clear;
begin
     // free stuff
     FreeAndNil(fSSQDif);
     FreeAndNil(fCW);
     FreeAndNil(fW);
     FreeAndNil(fBeta);
     FreeAndNil(fYRes);
     FreeAndNil(fTheta);
end;

destructor TMatrixPLS.Destroy;
begin
     Clear;

     inherited;
end;

function TMatrixPLS.mlDivide(A, y: TDoubleMatrix): IMatrix;
var tmp : TDoubleMatrix;
begin
     if A.SolveLeastSquares(tmp, y) <> qrOK then
        raise Exception.Create('Error: Failed to solve equation system.');
     Result := tmp;
end;

procedure TMatrixPLS.Plsr(Xreg, Yreg: TDoubleMatrix; NInput, NComp: Integer);
var LNrx, LNcx, LNry: Integer;
    Lssqx, Lssqy: IMatrix;
    Lx,Ly,LI, LR, LSSQ: IMatrix;
    i,j: Integer;
    n: integer;

  //test
    Lwt: IMatrix;
    LYPred: IMatrix;
begin
     Clear;
     //[nrx,ncx] = size(xreg);
     LNrx := Xreg.Height;
     LNcx := Xreg.Width;
     
     //[nry,ncy] = size(yreg);
     LNry := Yreg.Height;
     
     //if nrx ~= nry
     if LNrx <> LNry then
       raise Exception.Create('Number of rows for input and output must be same!');

     // alloc ssq
     LSSQ := MatrixClass.Create(NComp, 2);

     //ssqx = sum(sum(xreg.^2)');
     Lssqx := ((Xreg.ElementWiseMult(Xreg) As IMatrix).Sum(FALSE) As IMatrix).Sum(TRUE);
     //ssqy = sum(sum(yreg.^2)');
     Lssqy := ((Yreg.ElementWiseMult(Yreg) As IMatrix).Sum(FALSE) As IMatrix).Sum(TRUE);
     //x = xreg;
     Lx := xreg.Clone;
     //y = yreg;
     Ly := yreg.Clone;
     //I = eye(ncx);
     LI := MatrixClass.CreateEye(LNcx);
     //w = zeros(ncx,lv);
     fW := MatrixClass.Create(NComp, LNcx);
     LR := MatrixClass.Create(NComp, LNrx);

     //for i = 1:lv
     for i := 0 to NComp-1 do
     begin
          //w(:,i) = x'*y;
          fW.SetColumn(i, (Lx.Transpose As IMatrix).Mult(Ly) As IMatrix);
          //w(:,i) = w(:,i)/norm(w(:,i));
          fW.SetSubMatrix(i,0,1,fW.Height);
          fW.ScaleInPlace(1/fW.ElementwiseNorm2);
          //r(:,i) = x * w(:,i);
          LR.SetColumn(i, Lx.Mult(fW) As IMatrix);
          //x = x -  r(:,i) * w(:,i)';
          LR.SetSubMatrix(i, 0, 1, LR.Height);
          Lx.SubInPlace(LR.MultT2(fW) As IMatrix);
          //y = y - r*(r\y);
          LR.SetSubMatrix(0, 0, i+1, LR.Height);        
          Ly.SubInPlace(Lr.Mult( mlDivide(Lr.GetObjRef, Ly.GetObjRef) ) As IMatrix);

          //ssq(i,1) = (sum(sum(x.^2)'))*100/ssqx;
          Lwt := ((Lx.ElementWiseMult(Lx) As IMatrix).Sum(false) As IMatrix).Sum(True);
          LSSQ[i, 0] := (lwt[0,0]*100)/Lssqx[0,0];

          //ssq(i,2) = (sum(sum(y.^2)'))*100/ssqy;
          Lwt := ((Ly.ElementWiseMult(Ly) As IMatrix).Sum(false) As IMatrix).Sum(True);
          LSSQ[i, 1] := (lwt[0,0]*100)/Lssqy[0,0];

          fW.UseFullMatrix;
          LR.UseFullMatrix;
     end;

     //cw = r\yreg;
     fCW := mlDivide(Lr.GetObjRef, yreg).Clone;
     //thetam = w * cw; == fBeta
     fBeta := fW.Mult(fCW);

     //ssqdif = zeros(lv,2);
     fSSQDif := MatrixClass.Create(NComp, 2);
     //ssqdif(1,1) = 100 - ssq(1,1);
     fSSQDif[0,0] := 100 - LSSQ[0,0];
     //ssqdif(1,2) = 100 - ssq(1,2);
     fSSQDif[0,1] := 100 - LSSQ[0,1];
     //for i = 2:lv
     for i := 1 to NComp-1 do
     begin
          //for j = 1:2
          for j := 0 to 1 do
          begin
               //ssqdif(i,j) = -ssq(i,j) + ssq(i-1,j);
               fSSQDif[i,j] := -LSSQ[i,j] + LSSQ[i-1,j];
          end;
     end;

     //ypred = xreg * thetam;
     LYPred := Xreg.Mult(fBeta);
     //yres = yreg - ypred;
     fYRes := YReg.Sub(LYPred);
     //rms = norm(yres); <- dont needed

     //n = ncx / ninput;
     n := LNcx div NInput;
     fTheta := MatrixClass.Create(NInput, n);
     
     //for i = 1:ninput
     for i := 0 to NInput - 1 do
     begin
          //   theta(:,i) = thetam(n*(i-1)+1:n*i);
          fBeta.SetSubMatrix(0, n*i, 1, Trunc(n));
          fTheta.SetColumn(i, fBeta);
     end;
     fBeta.UseFullMatrix;
end;


function TMatrixPLS.Project(x: TDoubleMatrix): TDoubleMatrix;
begin
     Result := x.Mult(fTheta);
end;


function TMatrixPLS.PropTypeOfName(const Name: string): TPropType;
begin
     if SameText(Name, 'theta') or SameText(Name, 'ssqdif') or SameText(Name, 'cw') or
        SameText(Name, 'w') or SameText(Name, 'beta') or SameText(Name, 'yres') 
     then
         Result := ptObject
     else
         Result := inherited PropTypeOfName(Name);
end;

// ###########################################
// #### Persistence
// ###########################################

procedure TMatrixPLS.DefineProps;
begin
     if not Assigned(fTheta) then
        exit;

     AddObject('theta', fTheta);
     AddObject('ssqdif', fSSQDif);
     AddObject('cw', fCW);
     AddObject('w', fW);
     AddObject('beta', fBeta);
     AddObject('yres', fYRes);
end;

class function TMatrixPLS.ClassIdentifier: String;
begin
     Result := 'PLS';
end;

function TMatrixPLS.OnLoadObject(const Name: String;
  obj: TBaseMathPersistence): boolean;
begin
     Result := True;
     if CompareText(Name, 'theta') = 0 
     then
         fTheta := obj as TDoubleMatrix
     else if CompareText(Name, 'ssqdif') = 0 
     then
         fSSQDif := obj as TDoubleMatrix
     else if CompareText(Name, 'cw') = 0 
     then
         fCW := obj as TDoubleMatrix
     else if CompareText(Name, 'w') = 0 
     then
         fW := obj as TDoubleMatrix
     else if CompareText(Name, 'beta') = 0 
     then
         fBeta := obj as TDoubleMatrix
     else if CompareText(Name, 'yres') = 0 
     then
         fYRes := obj as TDoubleMatrix
     else
         Result := inherited OnLoadObject(name, obj);
end;

initialization
  RegisterMathIO(TMatrixPLS);

end.
