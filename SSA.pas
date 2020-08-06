// ###################################################################
// #### This file is part of the mathematics library project, and is
// #### offered under the licence agreement described on
// #### http://www.mrsoft.org/
// ####
// #### Copyright:(c) 2020, Michael R. . All rights reserved.
// ####
// #### Unless required by applicable law or agreed to in writing, software
// #### distributed under the License is distributed on an "AS IS" BASIS,
// #### WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// #### See the License for the specific language governing permissions and
// #### limitations under the License.
// ###################################################################

unit SSA;

// ###########################################
// #### singular spectrum analysis
// #### based on: https://github.com/ElsevierSoftwareX/SOFTX-D-17-00081/tree/master/script_ov_SSA_example
// ###########################################

interface

uses Matrix, Types;

type
  ESingularSpectrumException = class(EBaseMatrixException);
type
  TSingularSpectrumAnalysis = class(TMatrixClass)
  private
    fV : IMatrix;
    fU : IMatrix;
    fEigVals : IMatrix;
    fM, fN, fK : Integer;

    function InternalCalcSSA(mtx : IMatrix; L : integer) : IMatrix;
    function EmbedMtx( values : IMatrix; L : integer ) : IMatrix;
  public
    function Reconstruct( numComponents : integer ) : IMatrix; overload;
    function Reconstruct( idx : TIntegerDynArray ) : IMatrix; overload;

    procedure CalcSSA( values : TDoubleDynArray; L : integer ); overload;
    procedure CalcSSA( values : IMatrix; L : integer ); overload;
  end;

implementation

uses MatrixConst, Math, MathUtilFunc, PCA;

{ TSingularSpectrumAnalysis }

procedure TSingularSpectrumAnalysis.CalcSSA(values: TDoubleDynArray;
  L: integer);
var mtx : IMatrix;
begin
     if Length(values) < L - 1 then
        raise ESingularSpectrumException.Create('Error: Vector length is too short for the given embedding length.');
     mtx := MatrixClass.Create( values, Length(values), 1);
end;

procedure TSingularSpectrumAnalysis.CalcSSA(values: IMatrix; L: integer);
var mtx : IMatrix;
begin
     mtx := values.AsVector(True);
     if mtx.Width < L - 1 then
        raise ESingularSpectrumException.Create('Error: Vector length is too short for the given embedding length.');

     InternalCalcSSA(mtx, L);
end;

function TSingularSpectrumAnalysis.EmbedMtx(values: IMatrix;
  L: integer): IMatrix;
var y : Integer;
    k : integer;
begin
     k := values.Width - L + 1;
     Result := MatrixClass.Create( L, k );

     for y := 0 to k - 1 do
     begin
          values.SetSubMatrix(y, 0, L, 1);
          Result.SetRow(y, values);
     end;
     values.UseFullMatrix;
     Result.TransposeInPlace;
end;

function TSingularSpectrumAnalysis.InternalCalcSSA(mtx: IMatrix;
  L: integer): IMatrix;
var X : IMatrix;
begin
     if L > mtx.VecLen div 2 then
        L := Result.VecLen - L;

     // ###################################
     // ######  create embededd matrix
     X := EmbedMtx(mtx, L);

     fM := L;
     fN := mtx.VecLen;
     fK := fN - L + 1;

     // ###################################
     // ###### singular value decomposition
     if X.SVD(fU, fV, fEigVals, True) <> srOk then
        raise ESingularSpectrumException.Create('Error - svd did not converge');

     // sort according to size of singular values
     SortSVD( fEigVals.GetObjRef, fU.GetObjRef, fV.GetObjRef, 0, fEigVals.Height - 1);
     fU.TransposeInPlace;
end;

function TSingularSpectrumAnalysis.Reconstruct(idx: TIntegerDynArray): IMatrix;
var res : TDoubleDynArray;
    Lp, Kp : integer;
    cmpCnt: Integer;
    scale2, scale3 : double;
    eigVal : double;

procedure AddOneComponent;
var k, m : integer;
    scale1 : double;
    pU : PConstDoubleArr;
    pV : PConstDoubleArr;
begin
     pU := PConstDoubleArr( fU.StartElement );
     pV := PConstDoubleArr( fV.StartElement );
     for k := 0 to Lp - 2 do
     begin
          scale1 := 1/(k + 1)*eigVal;
          for m := 0 to k do
              res[k] := res[k] + scale1*pU^[m]*pV^[k - m]; //scale1*fU.Vec[m]*fV.Vec[k - m]; //
     end;

     for k := LP-1 to Kp - 1 do
         for m := 0 to LP - 1 do
             res[k] := res[k] + scale2*pU^[m]*pV^[k - m]; //scale2*fU.Vec[m]*fV.Vec[k - m]; //

     for k := Kp to fN - 1 do
     begin
          scale3 := 1/(fN - k)*eigVal;
          for m := k - Kp + 1 to fN - Kp do
              res[k] := res[k] + scale3*pU^[m]*pV^[k - m]; //scale3*fU.Vec[m]*fV.Vec[k - m]; //
     end;
end;

begin
     SetLength(res, fN);

     Lp := Min( fM, fK );
     Kp := Max( fM, fK );

     // grouping and reconstruction in one step
     for cmpCnt := 0 to Length(idx) - 1 do
     begin
          fU.SetSubMatrix(0, idx[cmpCnt], fU.Width, 1);
          fV.SetSubMatrix(0, idx[cmpCnt], fV.Width, 1);
          eigVal := fEigVals.Vec[ idx[cmpCnt] ];

          scale2 := 1/Lp*eigVal;

          AddOneComponent;
     end;

     fV.UseFullMatrix;
     fU.UseFullMatrix;

     Result := MatrixClass.Create(res, 1, fN);
end;


function TSingularSpectrumAnalysis.Reconstruct(numComponents: integer): IMatrix;
var idx : TIntegerDynArray;
    i : integer;
begin
     SetLength(idx, numComponents);

     for i := 0 to numComponents - 1 do
         idx[i] := i;

     Result := Reconstruct(idx);
end;

end.
