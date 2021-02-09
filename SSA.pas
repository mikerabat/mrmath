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
// #### See: % For a complete discussion about the method, see Leles et al (2017) A New
// #### Algorithm in Singular Spectrum Analysis Framework: the Overlap-SSA (ov-SSA).
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

    procedure InternalCalcSSA(mtx : IMatrix; L : integer);
    function InternalCalcOverlappedSSA(mtx : IMatrix; L, Z, q : integer; numComponents : integer; idx : TIntegerDynArray ) : IMatrix;

    function EmbedMtx( values : IMatrix; L : integer ) : IMatrix;
  public
    function Reconstruct( numComponents : integer ) : IMatrix; overload;
    function Reconstruct( idx : TIntegerDynArray ) : IMatrix; overload;

    // calculate ssa in one run.
    procedure CalcSSA( values : TDoubleDynArray; L : integer ); overload;
    procedure CalcSSA( values : IMatrix; L : integer ); overload;

{ direct comment from the original implementation:
% Consider a time series segment of length Z.
% All samples within this segment are used to compute the SSA locally.
% However, since the SSA method suffers from boundary effects,
% the extreme points in the left and right edges are discarded.
% The quantity of discarded samples is given by L_B = (Z-q)/2.
% Only an inner subset of samples, q, is considered meaningful to represent the local time-series Z.
% The final reconstruction is given by the concatenation of the inner segmentes q, which do not overlap.
% The extreme edges of the original time-series need special attention.
% In the first run only the last L_B points are discarded.
% On the other hand, in the last run, the first L_B points are discarded.
% This approach is a modification of  the overlap-save method,
% a classic tool to calculate FFT (Fast Fourier Transform) convolution of
% infinite (and finite) duration time series.
% This adaptation was necessary because the standard SSA algorithm suffers
% from boundary effects on both sides. In the overlap-save method only the initial
% points must be discarded, because boundary effects occur only at the filtering initialization.
% For a complete discussion about the method, see Leles et al (2017) A New
% Algorithm in Singular Spectrum Analysis Framework: the Overlap-SSA (ov-SSA).
% SoftwareX. In Press
%
% Written by Michel Leles, 07/05/2015
}
    class function CalcSSAOverlap( values : TDoubleDynArray; L : integer; Z : integer; q : integer; numComponents : integer; idx : TIntegerDynArray ) : TDoubleDynArray; overload;
    class function CalcSSAOverlap( values : IMatrix; L : integer; Z : integer; q : integer; numComponents : integer; idx : TIntegerDynArray ) : IMatrix; overload;
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

     InternalCalcSSA(mtx, L);
end;

procedure TSingularSpectrumAnalysis.CalcSSA(values: IMatrix; L: integer);
var mtx : IMatrix;
begin
     mtx := values.AsVector(True);
     if mtx.Width < L - 1 then
        raise ESingularSpectrumException.Create('Error: Vector length is too short for the given embedding length.');

     InternalCalcSSA(mtx, L);
end;

class function TSingularSpectrumAnalysis.CalcSSAOverlap(values: TDoubleDynArray; L,
  Z, q: integer; numComponents : integer; idx : TIntegerDynArray) : TDoubleDynArray;
var mtx : IMatrix;
    res : IMatrix;
begin
     if Length(values) < L - 1 then
        raise ESingularSpectrumException.Create('Error: Vector length is too short for the given embedding length.');

     with TSingularSpectrumAnalysis.Create do
     try
        mtx := MatrixClass.Create( values, Length(values), 1);

        res := InternalCalcOverlappedSSA(mtx, L, Z, q, numComponents, idx);
        Result := res.SubMatrix;
     finally
            Free;
     end;
end;

class function TSingularSpectrumAnalysis.CalcSSAOverlap(values: IMatrix; L, Z,
  q: integer; numComponents : integer; idx : TIntegerDynArray) : IMatrix;
var mtx : IMatrix;
begin
     mtx := values.AsVector(True);
     if mtx.Width < L - 1 then
        raise ESingularSpectrumException.Create('Error: Vector length is too short for the given embedding length.');

     with TSingularSpectrumAnalysis.Create do
     try
        Result := InternalCalcOverlappedSSA(mtx, L, Z, q, numComponents, idx);
     finally
            Free;
     end;
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

function TSingularSpectrumAnalysis.InternalCalcOverlappedSSA(mtx: IMatrix; L,
  Z, q: integer; numComponents : integer; idx : TIntegerDynArray) : IMatrix;
var n, p, pp : integer;
    L_B : integer;
    y_aux : IMatrix;
    rho : integer;
function CalcSSA( fromIdx, toIdx : integer ) : IMatrix;
var series : IMatrix;
begin
     series := mtx.SubColMtx(fromIdx, toIdx);
     InternalCalcSSA(series, L);

     if Length(idx) > 0
     then
         Result := Reconstruct( idx )
     else
         Result := Reconstruct( numComponents );

     Result.TransposeInPlace;
end;
begin
     n := mtx.VecLen;
     Result := MatrixClass.Create( n, 1 );

     // number of iterations
     p := (N - Z) div q + 1;

     L_B := (Z - q) div 2; // number of points discarded at each iterations

     // first run
     y_aux := CalcSSA(0, Z - 1);
     y_aux.SetSubMatrix(0, 0, Z - L_B, 1);
     Result.AssignSubMatrix(y_aux, 0, 0);
     // loop
     for pp := 1 to P - 2 do
     begin
          rho := pp*q;
          y_aux := CalcSSA(rho, rho + Z - 1);

          y_aux.SetSubMatrix(L_B, 0, q, 1);
          Result.AssignSubMatrix(y_aux, rho + L_B, 0);
     end;

     // last run
     pp := P - 1;
     rho := (pp - 1)*q;
     y_aux := CalcSSA( rho, n - 1);
     y_aux.SetSubMatrix(L_B, 0, y_aux.Width - L_B, 1);
     Result.AssignSubMatrix(y_aux, rho + L_B, 0);
end;

procedure TSingularSpectrumAnalysis.InternalCalcSSA(mtx: IMatrix;
  L: integer);
var X : IMatrix;
begin
     if L > mtx.VecLen div 2 then
        L := mtx.VecLen - L;

     fU := nil;
     fV := nil;
     fEigVals := nil;

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
