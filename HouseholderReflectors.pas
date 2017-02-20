// ###################################################################
// #### This file is part of the mathematics library project, and is
// #### offered under the licence agreement described on
// #### http://www.mrsoft.org/
// ####
// #### Copyright:(c) 2017, Michael R. . All rights reserved.
// ####
// #### Unless required by applicable law or agreed to in writing, software
// #### distributed under the License is distributed on an "AS IS" BASIS,
// #### WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// #### See the License for the specific language governing permissions and
// #### limitations under the License.
// ###################################################################

// ###################################################################
// #### a set of routines to create and apply housholder reflections
// -> used in svd and QR decomposition

unit HouseholderReflectors;

interface

uses MatrixConst;

function GenElemHousholderRefl(A : PDouble; LineWidthA : TASMNativeInt; Height : TASMNativeInt; var Alpha : double; Tau : PDouble) : boolean;

procedure ApplyElemHousholderReflLeft(V : PDouble; LineWidthV : TASMNativeInt; C : PDouble; const LineWidthC : TASMNativeInt; width, height : TASMNativeInt;
  Tau : PDouble; Work : PDouble);

procedure ApplyElemHousholderReflRight(V : PDouble; LineWidthV : TASMNativeInt; C : PDouble; const LineWidthC : TASMNativeInt; width, height : TASMNativeInt;
  Tau : PDouble; Work : PDouble);


implementation

uses OptimizedFuncs, MathUtilFunc, SimpleMatrixOperations;

// "right" part of dlarf
procedure ApplyElemHousholderReflRight(V : PDouble; LineWidthV : TASMNativeInt; C : PDouble; const LineWidthC : TASMNativeInt; width, height : TASMNativeInt;
  Tau : PDouble; Work : PDouble);
begin
     // work = A(1:m, 2:n)T*A(1:m, 1)
     if tau^ <> 0 then
     begin
          // note: the original routine has here an option C -> but in all cases it's
          // A + 1

          // todo: there is some other scanning going on for non zero columns...
          // do the basic operation here...
          MatrixMtxVecMult(work, sizeof(double), C, V, LineWidthC, LineWidthV, width, height, 1, 0);
          MatrixRank1Update(C, LineWidthC, width, height, -Tau^, work, V, sizeof(double), LineWidthV);
     end;
end;


// apply householder transformation to A (one column)
// original DLARFG in Lapack
function GenElemHousholderRefl(A : PDouble; LineWidthA : TASMNativeInt; Height : TASMNativeInt; var Alpha : double; Tau : PDouble) : boolean;
var beta : double;
    xNorm : double;
    saveMin : double;
    rsafmn : double;
    cnt : integer;
begin
     Result := False;
     dec(height);

     if height <= 0 then
     begin
          Tau^ := 0;
          Result := True;
          exit;
     end;

     xNorm := MatrixElementwiseNorm2(A, LineWidthA, 1, height);
     if xNorm = 0 then
     begin
          // H = I
          Tau^ := 0;
          exit;
     end;

     beta := -sign( pythag(alpha, xnorm), alpha);

     cnt := 0;
     // todo: apply under/overflow code here as lapack does
     // check for possible under/overflow -> rescale
     saveMin := cMinDblDivEps;
//     // note this is not the original code
     if Abs(beta) < saveMin then
     begin
          rsafmn := 1/saveMin;

          repeat
                inc(cnt);
                MatrixScaleAndAdd(A, LineWidthA, 1, Height, 0, rsafmn);
                beta := beta*rsafmn;
                alpha := alpha*rsafmn;
          until abs(beta) >= saveMin;

          xnorm := MatrixElementwiseNorm2(A, LineWidthA, 1, height);
          beta := -sign( pythag(alpha, xnorm), alpha );
     end;

     Tau^ := (beta - alpha)/beta;
     MatrixScaleAndAdd(A, LineWidthA, 1, Height, 0, 1/(alpha - beta));

     while cnt > 0 do
     begin
          beta := beta*saveMin;
          dec(cnt);
     end;

     alpha := beta;

     Result := True;
end;


// original Dlarf in lapack
procedure ApplyElemHousholderReflLeft(V : PDouble; LineWidthV : TASMNativeInt; C : PDouble; const LineWidthC : TASMNativeInt; width, height : TASMNativeInt;
  Tau : PDouble; Work : PDouble);
begin
     // work = A(1:m, 2:n)T*A(1:m, 1)
     if tau^ <> 0 then
     begin
          // todo: there is some other scanning going on for non zero columns...
          // do the basic operation here...
          MatrixMtxVecMultT(work, sizeof(double), C, V, LineWidthC, LineWidthV, width, height, 1, 0);
          MatrixRank1Update(C, LineWidthC, width, height, -tau^, V, work, LineWidthV, sizeof(double));
     end;
end;


end.
