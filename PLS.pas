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
  TPLSAlgorithm = (paEig, paSVD);
  TMatrixPLS = Class(TMatrixClass)
  private
    fAlgorithm : TPLSAlgorithm;
    fBeta: IMatrix;
    fXMean, fYMean : IMatrix;
  protected
    procedure Clear;

    // persistence
    procedure DefineProps; override;
    function PropTypeOfName(const Name : string) : TPropType; override;

    class function ClassIdentifier : String; override;
    function OnLoadObject(const Name : String; obj : TBaseMathPersistence) : boolean; override;
  public
    destructor Destroy; override;

    function PLSRegress( Xreg, Yreg : TDoubleMatrix; NComp : integer ) : boolean; overload;
    function PLSRegress( Xreg, Yreg : TDoubleMatrix; NComp : integer; fitted : TDoubleMatrix ) : boolean; overload;

    function Project(x : TDoubleMatrix) : TDoubleMatrix;

    property Algorithm : TPLSAlgorithm read fAlgorithm write fAlgorithm;

    // out
    property Beta: IMatrix read fBeta;
    property YMean: IMatrix read fYMean;         // residual when calculating plsr
    property XMean: IMatrix read fXMean;
  End;



implementation

{ TMatrixPLS }

procedure TMatrixPLS.Clear;
begin
     fXMean := Nil;
     fYMean := nil;
     fBeta := nil;
end;

destructor TMatrixPLS.Destroy;
begin
     Clear;

     inherited;
end;

function TMatrixPLS.Project(x: TDoubleMatrix): TDoubleMatrix;
var tmp : IMatrix;
begin
     tmp := TDoubleMatrix.Create(X.Width + 1, X.Height);
     tmp := x.SubVec(fXMean, True);
     //
     Result := tmp.Mult(fBeta);
     Result.AddVecInPlace(fYMean, True);
end;


function TMatrixPLS.PropTypeOfName(const Name: string): TPropType;
begin
     if SameText(Name, 'beta') or SameText(Name, 'xmean') or SameText(Name, 'ymean')
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
     if not Assigned(fBeta) then
        exit;

     AddObject('beta', fBeta.GetObjRef);
     AddObject('ymean', fYMean.GetObjRef);
     AddObject('xmean', fXMean.GetObjRef);
end;

class function TMatrixPLS.ClassIdentifier: String;
begin
     Result := 'PLSREGRESS';
end;

function TMatrixPLS.OnLoadObject(const Name: String;
  obj: TBaseMathPersistence): boolean;
begin
     Result := True;
     if CompareText(Name, 'beta') = 0
     then
         fBeta := obj as TDoubleMatrix
     else if CompareText(Name, 'ymean') = 0
     then
         fYMean := obj as TDoubleMatrix
     else if CompareText(Name, 'xmean') = 0
     then
         fXMean := obj as TDoubleMatrix
     else
         Result := inherited OnLoadObject(name, obj);
end;

function TMatrixPLS.PLSRegress(Xreg, Yreg: TDoubleMatrix; NComp: integer): boolean;
begin
     Result := PLSRegress(Xreg, yReg, NComp, nil );
end;

function TMatrixPLS.PLSRegress(Xreg, Yreg: TDoubleMatrix; NComp: integer; fitted : TDoubleMatrix) : boolean;
var nobs : Integer;
    npred : integer;
    nresp : integer;
    ym, xm : IMatrix;
    S : IMatrix;
    R, P, V : IMatrix;
    T, U : IMatrix;
    Q : IMatrix;
    i, j : Integer;
    domIdx : integer;
    eigMax : double;
    eigVec, eigVal, tmp : IMatrix;
    rl, tl : IMatrix;
    tm : IMatrix;
    nt : double;
    pl, ql, ul, vl : IMatrix;
begin
     Result := False;

     nobs := XReg.Height;  // number of observations
     npred := XReg.Width;  // number of predictor values
     nresp := YReg.Width;  // number of responses

     // mean centering data matrix
     fXMean := Xreg.Mean(False);
     xm := Xreg.SubVec(fXMean, True);

     // mean centering responses
     fYMean := Yreg.Mean(False);
     ym := Yreg.SubVec(fYMean, True);

     // S = X'*Y;
     S := xm.MultT1(ym);

     // R = P = V = zeros (npred, NCOMP);
     // T = U = zeros (nobs, NCOMP);
     // Q = zeros (nresp, NCOMP);

     R := MatrixClass.Create( NCOMP, npred );
     P := MatrixClass.Create( NCOMP, npred );
     V := MatrixClass.Create( NCOMP, npred );
     T := MatrixClass.Create( NCOMP, nobs );
     U := MatrixClass.Create( NCOMP, nobs );
     Q := MatrixClass.Create( NCOMP, nresp );

     for i := 0 to NCOMP - 1 do
     begin
          // [eigvec eigval] = eig (S'*S);         # Y factor weights
          // domindex = find (diag (eigval) == max (diag (eigval))); # get dominant eigenvector
          // q  = eigvec(:,domindex);
          if fAlgorithm = paEig then
          begin
               tmp := S.MultT1(S);
               if tmp.SymEig(eigVal, eigVec ) <> qlOk then
                  exit;
          end
          else
          begin
               // actually eigenvalues are the sqrt of the SymEig values
               // -> since we seek just the largest we don't do that
               if S.SVD(tmp, eigVec, eigVal) <> srOk then
                  exit;
          end;

          // find maximum
          domIdx := 0;
          eigMax := eigVal.Vec[0];
          for j := 1 to eigVal.VecLen - 1 do
          begin
               if eigVal.Vec[j] > eigMax then
               begin
                    domIdx := j;
                    eigMax := eigVal.Vec[j];
               end;
          end;

          eigVec.SetSubMatrix(domIdx, 0, 1, eigVec.Height);


          // r  = S*q;            # X block factor weights
          // t  = X*r;            # X block factor scores
          // t  = t - mean (t);
          rl := S.Mult(eigVec);
          tl := xm.Mult(rl);
          tm := tl.Mean(False);
          tl.AddAndScaleInPlace(-tm.Vec[0], 1);

          // nt = sqrt (t'*t);     # compute norm
          // t  = t/nt;
          // r  = r/nt;            # normalize
          nt := tl.ElementwiseNorm2(True);
          tl.ScaleInPlace(1/nt);
          rl.ScaleInPlace(1/nt);

          // p  = X'*t;     # X block factor loadings
          // q  = Y'*t;     # Y block factor loadings
          // u  = Y*q;      # Y block factor scores
          // v  = p;
          pl := xm.MultT1(tl);
          ql := ym.MultT1(tl);
          ul := ym.Mult(ql);
          vl := pl;

          //## Ensure orthogonality
          //if a > 1
          //     v = v - V*(V'*p);
          //     u = u - T*(T'*u);
          //endif
          if i > 0 then
          begin
               tmp := V.MultT1(pl);
               tmp := V.Mult(tmp);
               vl.SubInPlace(tmp);

               tmp := T.MultT1(ul);
               tmp := T.Mult(tmp);
               ul.SubInPlace(tmp);
          end;

          // v = v/sqrt(v'*v); # normalize orthogonal loadings
          // S = S - v*(v'*S); # deflate S wrt loadings
          vl.Normalize(False);
          tmp := vl.MultT1(S);
          tmp := vl.Mult(tmp);
          S.SubInPlace(tmp);

          // ## Store data
          // R(:,a) = r;
          // T(:,a) = t;
          // P(:,a) = p;
          // Q(:,a) = q;
          // U(:,a) = u;
          // V(:,a) = v;
          R.SetColumn(i, rl);
          T.SetColumn(i, tl);
          P.SetColumn(i, pl);
          Q.SetColumn(i, ql);
          U.SetColumn(i, ul);
          V.SetColumn(i, vl);
     end;

     // ## Regression coefficients
     // B = R*Q';
     fBeta := R.MultT2(Q);

     // in case a resulting object is provided we do the multiplication here
     if Assigned(fitted) then
     begin
          fitted.Assign(T);
          fitted.MultInPlaceT2(Q);
          fitted.AddVecInPlace(fYMean, True);
     end;

     Result := True;
end;

initialization
  RegisterMathIO(TMatrixPLS);

end.
