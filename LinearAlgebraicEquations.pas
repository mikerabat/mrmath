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


unit LinearAlgebraicEquations;

// ############################################
// #### Functions to solve linear algebraic equations
// ############################################

interface

uses SysUtils, Types, MatrixConst, OptimizedFuncs, Math, MathUtilFunc;

// solves the matrix A*X = B where A*x1=b1, A*x2=b2, ... A*xm=bm
// The function stores in A the inverse of A and B stores the result vectors
// A must be a square matrix (width*width) and B must be m*width.
function MatrixGaussJordanInPlace(A : PDouble; const LineWidthA : TASMNativeInt; B : PDouble; const LineWidthB : TASMNativeInt; width : TASMNativeInt;
                                  m : TASMNativeInt; const epsilon : double = 1e-20; progress : TLinEquProgress = nil) : TLinEquResult;

function MatrixGaussJordan(A : PDouble; const LineWidthA : TASMNativeInt; B : PDouble; const LineWidthB : TASMNativeInt;
                           invA : PDouble; const LineWidthInvA : TASMNativeInt; X : PDouble; const LineWidthX : TASMNativeInt;
                           width : TASMNativeInt; m : TASMNativeInt; const epsilon : double = 1e-20; progress : TLinEquProgress = nil) : TLinEquResult;

// interface functions (used in different parts - don't call them directly)
procedure LUSwap(A : PDouble; const LineWidthA : TASMNativeInt; width : TASMNativeInt; k1, k2 : TASMNativeInt; indx : PIntegerArray; var parity : TASMNativeInt);
procedure LUBacksup(A : PDouble; width, height : TASMNativeInt; B : PDouble; const LineWidth : TASMNativeInt);

// inplace LU decomposition of the matrix A. Diagonal elements of the lower triangular matrix are set to one
// thus the diagonal elements of the resulting matrix A are composed from the upper diagonal elements only.
// The index records the row permutation effected by the partial pivoting.
function MatrixLUDecompInPlace(A : PDouble; const LineWidthA : TASMNativeInt; width : TASMNativeInt; indx : PIntegerArray; progress : TLinEquProgress = nil) : TLinEquResult;
function MatrixLUDecomp(A : PDouble; const LineWidthA : TASMNativeInt; LUDecomp : PDouble; const LineWidthLU : TASMNativeInt; width : TASMNativeInt; indx : PIntegerArray; progress : TLinEquProgress = nil) : TLinEquResult; overload;
function MatrixLUDecomp(A : PDouble; const LineWidthA : TASMNativeInt; LUDecomp : PDouble; const LineWidthLU : TASMNativeInt; width : TASMNativeInt; progress : TLinEquProgress = nil) : TLinEquResult; overload;
procedure MatrixLUBackSubst(LUDecomp : PDouble; const LineWidthLU : TASMNativeInt; width : TASMNativeInt; const  indx : PIntegerArray; B : PDouble; const LineWidthB : TASMNativeInt; progress : TLinEquProgress = nil);

// inverse of a matrix by using the LU decomposition
function MatrixInverseInPlace(A : PDouble; const LineWidthA : TASMNativeInt; width : TASMNativeInt; progress : TLinEquProgress = nil) : TLinEquResult;

// Matrix determinant calculated from the LU decomposition. Returns zero in case of a singular matrix. Drawback is a double
// memory usage since the LU decomposition must be stored in a temporary matrix.
function MatrixDeterminant(A : PDouble; const LineWidthA : TASMNativeInt; width : TASMNativeInt; progress : TLinEquProgress = nil) : double;


// Matrix Line Equation Solver routines which are based on LU decomposition.
// note these functions use temporarily double the size of A memory.
// The result is stored in X. B and X must have the same size, also B may have
// more than one column.
function MatrixLinEQSolve(A : PDouble; const LineWidthA : TASMNativeInt; width : TASMNativeInt; B : PDouble; const LineWidthB : TASMNativeInt; X : PDouble; const LineWidthX : TASMNativeInt;
   Width2 : TASMNativeInt; const NumRefinments : TASMNativeInt = 0; progress : TLinEquProgress = nil) : TLinEquResult;


// Inplace Cholesky decomposition of the matrix A (A=L*L'). A must be a positive-definite symmetric matrix.
// The cholesky factor L is returned in the lower triangle of a, except for its diagonal elements which are returned in p
function MatrixCholeskyInPlace(A : PDouble; const LineWidthA : TASMNativeInt; width : TASMNativeInt; P : PDouble; LineWidthP : TASMNativeInt; progress : TLinEquProgress = nil) : TCholeskyResult;
function MatrixCholesky(dest : PDouble; const LineWidthDest : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; width : TASMNativeInt;
  P : PDouble; const LineWidthP : TASMNativeInt; progress : TLinEquProgress = nil) : TCholeskyResult;
// solves the set of linear equations Ax = b where A is a positive-definite symmetric matrix. A and P are input as the output of
// MatrixCholeskyInPlace. Only the lower triangle is accessed. The result is stored in X, thus the routine can be called multiple
// times. B and X can point to the same memory!
procedure MatrixCholeskySolveLinEq(A : PDouble; const LineWidthA : TASMNativeInt; width : TASMNativeInt; P : PDouble;
  const LineWidthP : TASMNativeInt; B : PDouble; const LineWidthB : TASMNativeInt; X : PDouble; const LineWidthX : TASMNativeInt; progress : TLinEquProgress = nil);


// Linpack version of cholesky decomposition
function MatrixCholeskyInPlace2(A : PDouble; const LineWidthA : TASMNativeInt; width : TASMNativeInt; progress : TLinEquProgress = nil) : TCholeskyResult;
function MatrixCholeskyInPlace3(A : PDouble; const LineWidthA : TASMNativeInt; width : TASMNativeInt; progress : TLinEquProgress = nil) : TCholeskyResult;
function MatrixCholeskyInPlace4(A : PDouble; const LineWidthA : TASMNativeInt; width : TASMNativeInt; pnlSize : TASMNativeInt; progress : TLinEquProgress = nil) : TCholeskyResult;


// Pseudoinversion - implementation taken from matlab
// X = pinv(A) produces a matrix X of the same dimensions
//  as A' so that A*X*A = A, X*A*X = X and A*X and X*A
//  are Hermitian. The computation is based on SVD(A) and any
//  singular values less than a tolerance are treated as zero.
//  The default tolerance is MAX(SIZE(A)) * NORM(A) * EPS(class(A))
// Note the Matrix in X is also used in the calculations, thus it's content is destroyed!
// dest must be at least as big as the transposed of X
function MatrixPseudoinverse(dest : PDouble; const LineWidthDest : TASMNativeInt; X : PDouble; const LineWidthX : TASMNativeInt;
  width, height : TASMNativeInt; progress : TLinEquProgress = nil) : TSVDResult;


// ######################################################
// #### internaly used objects and definitions
type
  TLinearEQProgress = class(TObject)
  public
    refProgress : TLinEquProgress;
    numRefinenmentSteps : TASMNativeInt;

    procedure LUDecompSolveProgress(Progress : integer);
    procedure RefinementProgress(Progress : integer);
  end;

function InternalBlkCholeskyInPlace(A : PDouble; const LineWidthA : TASMNativeInt; width : TASMNativeInt; pnlSize : TASMNativeInt;
   aMatrixMultT2Ex : TMatrixBlockedMultfunc; multMem : PDouble; progress : TLinEquProgress = nil) : TCholeskyResult;

implementation

uses ASMMatrixOperations, SimpleMatrixOperations, BlockSizeSetup, Classes, LinAlgSVD,
     HouseholderReflectors;

function MatrixPseudoinverse(dest : PDouble; const LineWidthDest : TASMNativeInt; X : PDouble; const LineWidthX : TASMNativeInt;
  width, height : TASMNativeInt; progress : TLinEquProgress) : TSVDResult;
var doTranspose : boolean;
    data : TDoubleDynArray;
    UTranspose : TDoubleDynArray;
    pData : PDouble;
    lineWidthData : TASMNativeInt;
    S, V : TDoubleDynArray;
    lineWidthV : TASMNativeInt;
    tolerance : double;
    r : TASMNativeInt;
    i : TASMNativeInt;
    k, l : TASMNativeInt;
    pUTranspose : PDouble;
    res : TDoubleDynArray;
    pRes : PDouble;
    destOffset : TASMNativeInt;
begin
     // pseudo inversion of a matrix: pinv(x) = x'*(x*x')^-1
     // based on the matlab implementation:
     // [u s v] = svd(x, 0)
     // s = diag(S);
     // tol = max(m,n) * eps(max(s));
     // r = sum(s > tol);
     // s = diag(ones(r,1)./s(1:r));
     // X = V(:,1:r)*s*U(:,1:r)';
     assert((width > 0) and (height > 0) and (lineWidthDest >= height*sizeof(double)), 'Dimension error');


     doTranspose := width > height;
     if doTranspose then
     begin
          data := MatrixTranspose(X, LineWidthX, width, height);
          pData := @data[0];
          lineWidthData := height*sizeof(double);

          Result := MatrixPseudoinverse(X, LineWidthX, pData, lineWidthData, height, width);

          MatrixTranspose(dest, LineWidthDest, x, LineWidthX, width, height);
          exit;
     end;

     SetLength(S, width);
     SetLength(V, sqr(width));
     lineWidthV := width*sizeof(double);
     Result := MatrixSVDInPlace(X, lineWidthX, width, height, @S[0], sizeof(double), @V[0], lineWidthV, progress);

     if Result = srNoConvergence then
        exit;

     tolerance := height*eps(MatrixMax(@S[0], width, 1, width*sizeof(double)));

     r := 0;
     for i := 0 to width - 1 do
     begin
          if s[i] > tolerance then
             inc(r)
     end;

     if r = 0 then
     begin
          // set all to zero
          destOffset := LineWidthDest - height*sizeof(double);
          for k := 0 to width - 1 do
          begin
               for l := 0 to height - 1 do
               begin
                    dest^ := 0;
                    inc(dest);
               end;

               inc(PByte(dest), destOffset);
          end;
     end
     else
     begin
          // invert
          for i := 0 to width - 1 do
          begin
               if s[i] > tolerance
               then
                   s[i] := 1/s[i]
               else
                   s[i] := 0;
          end;

          UTranspose := MatrixTranspose(X, LineWidthX, width, height);
          pUTranspose := @UTranspose[0];
          for k := 0 to width - 1 do
          begin
               for l := 0 to height - 1 do
               begin
                    pUTranspose^ := pUTranspose^*s[k];
                    inc(pUTranspose);
               end;
          end;

          res := MatrixMult(@V[0], @UTranspose[0], width, width, height, width, width*sizeof(double), height*sizeof(double));
          V := nil;
          UTranspose := nil;
          s := nil;

          // copy
          pRes := @res[0];
          for k := 0 to width - 1 do
          begin
               Move(pRes^, dest^, sizeof(double)*height);
               inc(PByte(dest), LineWidthDest);
               inc(PByte(pRes), sizeof(double)*height);
          end;
     end;
end;

function MatrixGaussJordan(A : PDouble; const LineWidthA : TASMNativeInt; B : PDouble; const LineWidthB : TASMNativeInt;
                           invA : PDouble; const LineWidthInvA : TASMNativeInt; X : PDouble; const LineWidthX : TASMNativeInt;
                           width : TASMNativeInt; m : TASMNativeInt; const epsilon : double; progress : TLinEquProgress) : TLinEquResult;
var i: TASMNativeInt;
    pInvA : PDouble;
    PX : PDouble;
begin
     Assert(width > 0, 'Dimension Error');
     Assert(lineWidthInvA >= width*sizeof(double), 'Dimension error');
     Assert(lineWidthX >= sizeof(double), 'Dimension error');

     // copy data -> now we can perform an inline gauss elimination procedure
     PInvA := invA;
     PX := X;
     for i := 0 to width - 1 do
     begin
          Move(A^, PInvA^, sizeof(double)*width);
          inc(PByte(PInvA), LineWidthInvA);
          inc(PByte(A), LineWidthA);
          Move(B^, PX^, sizeof(double)*m);
          inc(PByte(B), LineWidthB);
          inc(PByte(PX), LineWidthX);
     end;

     Result := MatrixGaussJordaninPlace(invA, lineWidthInvA, X, LineWidthX, width, m, epsilon, progress);
end;

function MatrixGaussJordanInPlace(A : PDouble; const LineWidthA : TASMNativeInt; B : PDouble; const LineWidthB : TASMNativeInt;
  width : TASMNativeInt; m : TASMNativeInt; const epsilon : double; progress : TLinEquProgress) : TLinEquResult;
var i, icol, irow, j, k, l, ll : TASMNativeInt;
    big, dum, pivinv : double;
    indxc, indxr, ipiv : Array of integer;
    pVal1 : PDouble;
    pVal2 : PDouble;
begin
     assert(LineWidthA >= width*sizeof(double), 'Dimension error');
     assert(LineWidthB >= m*sizeof(double), 'Dimension error');

     Result := leOk;

     SetLength(indxc, width);
     SetLength(indxr, width);
     SetLength(ipiv, width);

     icol := 0;
     irow := 0;

     if Assigned(progress) then
        progress(0);

     for j := 0 to width - 1 do
         ipiv[j] := 0;

     // main loop over the columns to be reduced
     for i := 0 to width - 1 do
     begin
          big := 0;
          for j := 0 to width - 1 do
          begin
               if ipiv[j] <> 1 then
               begin
                    for k := 0 to width - 1 do
                    begin
                         if ipiv[k] = 0 then
                         begin
                              pVal1 := PDouble(PAnsiChar(A) + j*LineWidthA);
                              inc(pVal1, k);

                              if abs(pVal1^) >= big then
                              begin
                                   big := abs(pVal1^);
                                   irow := j;
                                   icol := k;
                              end;
                         end
                         else if ipiv[k] > 1 then
                         begin
                              Result := leSingular;
                              exit;
                         end;
                    end;
               end;
          end;

          inc(ipiv[icol]);

          // we now have the pivot element, so we interchange rows, if needed, to put the pivot
          // element on the dagonal.

          if irow <> icol then
          begin
               pVal1 := PDouble(PAnsiChar(A) + irow*LineWidthA);
               pVal2 := PDouble(PAnsiChar(A) + icol*LineWidthA);
               for l := 0 to width - 1 do
               begin
                    DoubleSwap(pVal1^, pVal2^);
                    inc(pVal1);
                    inc(pVal2);
               end;

               pVal1 := PDouble(PAnsiChar(B) + irow*LineWidthB);
               pVal2 := PDouble(PAnsiChar(B) + icol*LineWidthB);
               for l := 0 to m - 1 do
               begin
                    DoubleSwap(pVal1^, pVal2^);
                    inc(pVal1);
                    inc(pVal2);
               end;
          end;

          // we are now ready to divide the pivot row by the pivot element, located in irow and icol
          indxr[i] := irow;
          indxc[i] := icol;

          pVal1 := PDouble(PAnsiChar(A) + icol*LineWidthA);
          inc(pVal1, icol);

          if abs(pVal1^) < epsilon then
          begin
               Result := leSingular;
               exit;
          end;

          pivinv := 1/pVal1^;

          pVal1^ := 1;
          pVal1 := PDouble(PAnsiChar(A) + icol*LineWidthA);
          for l := 0 to width - 1 do
          begin
               pVal1^ := pVal1^*pivinv;
               inc(pVal1);
          end;

          pVal1 := PDouble(PAnsiChar(B) + icol*LineWidthB);
          for l := 0 to m - 1 do
          begin
               pVal1^ := Pivinv*pVal1^;
               inc(pVal1);
          end;

          for ll := 0 to width - 1 do
          begin
               if ll <> icol then
               begin
                    pVal1 := PDouble(PAnsiChar(A) + ll*LineWidthA);
                    inc(pVal1, icol);
                    dum := pVal1^;
                    pVal1^ := 0;

                    pVal1 := PDouble(PAnsiChar(A) + ll*LineWidthA);
                    pVal2 := PDouble(PAnsiChar(A) + icol*LineWidthA);
                    for l := 0 to width - 1 do
                    begin
                         pVal1^ := pVal1^ - pVal2^*dum;
                         inc(pVal1);
                         inc(pVal2);
                    end;

                    pVal1 := PDouble(PAnsiChar(B) + ll*LineWidthB);
                    pVal2 := PDouble(PAnsiChar(B) + icol*LineWidthB);
                    for l := 0 to m - 1 do
                    begin
                         pVal1^ := pVal1^ - pVal2^*dum;
                         inc(pVal1);
                         inc(pVal2);
                    end;
               end;
          end;

          if Assigned(progress) then
             progress(100*i div width);
     end;

     for l := width - 1 downto 0 do
     begin
          if indxr[l] <> indxc[l] then
          begin
               for k := 0 to width - 1 do
               begin
                    pVal1 := PDouble(PAnsiChar(A) + k*LineWidthA);
                    pVal2 := pVal1;
                    inc(pval1, indxr[l]);
                    inc(pval2, indxc[l]);

                    DoubleSwap(pVal1^, pVal2^);
               end;
          end;
     end;

     if Assigned(progress) then
        progress(100);
end;

// LUSWAP performs a series of row interchanges on the matrix A.
// One row interchange is initiated for each of rows K1 through K2 of A.
procedure LUSwap(A : PDouble; const LineWidthA : TASMNativeInt; width : TASMNativeInt; k1, k2 : TASMNativeInt; indx : PIntegerArray; var parity : TASMNativeInt);
var i : TASMNativeInt;
    pA1, pA2 : PDouble;
begin
     for i := k1 to k2 do
     begin
          if indx^[i] <> i then
          begin
               // interchange rows
               pA1 := A;
               inc(PByte(pA1), i*LineWidthA);
               pA2 := A;
               inc(PByte(pA2), indx^[i]*LineWidthA);

               // swap a complete row at once
               MatrixRowSwap(pA1, pA2, width);

               parity := -parity;
          end;
     end;
end;

procedure LUBacksup(A : PDouble; width, height : TASMNativeInt; B : PDouble; const LineWidth : TASMNativeInt);
var j, i, k : TASMNativeInt;
    pA, pAi : PDouble;
    pB, pBj : PDouble;
    pBDest, pBDestj, pBDesti : PDouble;
begin
     pA := A;
     inc(PByte(pA), LineWidth);

     pB := B;
     pBDest := B;
     inc(PByte(pBDest), LineWidth);
     for k := 0 to height - 1 do
     begin
          pAi := pA;
          pBDesti := pBDest;

          for i := k + 1 to height - 1 do
          begin
               pBj := pB;
               pBDestj := pBDesti;
               for j := 0 to width - 1 do
               begin
                    pBDestj^ := pBDestj^ - pBj^*pAi^;

                    inc(pBj);
                    inc(pBDestj);
               end;

               inc(PByte(pAi), LineWidth);
               inc(PByte(pBDesti), LineWidth);
          end;
          inc(pA);
          inc(PByte(pA), LineWidth);
          inc(PByte(pB), LineWidth);
          inc(PByte(pBDest), LineWidth);
     end;
end;

const cBlkMultSize = 48;
type
  TRecMtxLUDecompData = record
    progress : TLinEquProgress;
    numCols,
    numCalc : TASMNativeInt;
    blkMultMem : Pdouble;
    LineWidth : TASMNativeInt;
  end;

function InternalRecursiveMatrixLUDecompInPlace(A : PDouble;  width, height : TASMNativeInt;
 indx : PIntegerArray; var parity : TASMNativeInt; var data : TRecMtxLUDecompData) : TLinEquResult;
var mn : TASMNativeInt;
    pA : PDouble;
    idx : TASMNativeInt;
    maxVal : double;
    nleft, nright : TASMNativeInt;
    i : TASMNativeInt;
    pB, a12, a21 : PDouble;
    absMaxVal : double;
begin
     mn := min(width, height);

     if mn > 1 then
     begin
          nleft := mn div 2;
          nright := width - nleft;

          Result := InternalRecursiveMatrixLUDecompInPlace(A, nLeft, height, indx, parity, data);

          if Result <> leOk then
             exit;

          pA := A;
          inc(pA, nLeft);
          LUSwap(pA, data.LineWidth, nright, 0, nleft - 1, indx, parity);

          // lu backsup A12 = L - one*A12
          if nRight > 1 then
             LUBacksup(A, nRight, nLeft, pA, data.LineWidth);

          // matrix mult sub
          // A22 = A22 - A21*A12
          pB := A;
          inc(pB, nleft);
          a12 := pB;
          inc(PByte(pB), nLeft*data.LineWidth);

          a21 := A;
          inc(PByte(a21), nleft*data.LineWidth);
          // in this case it's faster to have a small block size!
          if (nright > cBlkMultSize) or (height - nleft > cBlkMultSize)
          then
              BlockedMatrixMultiplication(pB, data.LineWidth, a21, a12, nleft, height - nleft, nright, nleft, data.LineWidth, data.LineWidth, cBlkMultSize, doSub, data.blkMultMem)
          else
          begin
               MatrixMult(data.blkMultMem, (nright + nright and $01)*sizeof(double), a21, a12, nleft, height - nleft, nright, nleft, data.LineWidth, data.LineWidth);
               MatrixSub(pB, data.LineWidth, pB, data.blkMultMem, nright, height - nleft, data.LineWidth, (nright + nright and $01)*sizeof(double));
          end;

          // apply recursive LU to A(nleft + 1, nleft + 1);
          Result := InternalRecursiveMatrixLUDecompInPlace(pB, nright, height - nleft, @(indx^[nleft]), parity, data);
          if Result <> leok then
             exit;

          for i := nLeft to width - 1 do
              indx^[i] := indx^[i] + nLeft;

          // dlswap
          LUSwap(A, data.LineWidth, nleft, nleft, mn - 1, indx, parity);
     end
     else
     begin
          // find maximum element of this column
          maxVal := 0;
          absMaxVal := 0;
          idx := -1;
          pA := A;
          for i := 0 to Height - 1 do
          begin
               if Abs(pA^) > absMaxVal then
               begin
                    idx := i;
                    maxVal := pA^;
                    absMaxVal := abs(maxVal);
               end;

               inc(PByte(pA), data.LineWidth);
          end;

          // now it's time to apply the gauss elimination
          indx^[0] := idx;

          // check for save invertion of maxVal
          if Abs(maxVal) > 10/MaxDouble then
          begin
               MatrixScaleAndAdd(A, data.LineWidth, 1, Height, 0, 1/maxVal);
               pA := A;
               inc(PByte(pA), data.LineWidth*idx);
               pA^ := A^;
               A^ := maxVal;

               Result := leOk;

               if Assigned(data.progress) then
               begin
                    inc(data.numCalc);
                    data.progress(data.numCalc*100 div data.numCols);
               end;
          end
          else
              Result := leSingular;
     end;
end;

function MatrixLUDecompInPlace(A : PDouble; const LineWidthA : TASMNativeInt; width : TASMNativeInt; indx : PIntegerArray; progress : TLinEquProgress) : TLinEquResult;
var parity : TASMNativeInt;
    mem : Array[0..(4+4*cBlkMultSize*cBlkMultSize)] of double;
    rc : TRecMtxLUDecompData;
begin
     FillChar(indx^, width*sizeof(integer), 0);
     parity := 1;
     rc.progress := progress;
     rc.numCols := width;
     rc.numCalc := 0;
     rc.blkMultMem := PDouble(TASMNativeUInt(@mem[0]) + 16 - (TASMNativeUInt(@mem[0]) and $0F));
     rc.LineWidth := LineWidthA;
     Result := InternalRecursiveMatrixLUDecompInPlace(A, width, width, indx, parity, rc);
end;

function MatrixLUDecomp(A : PDouble; const LineWidthA : TASMNativeInt; LUDecomp : PDouble; const LineWidthLU : TASMNativeInt; width : TASMNativeInt; indx : PIntegerArray; progress : TLinEquProgress) : TLinEquResult;
begin
     Assert(width > 0, 'Dimension Error');

     // copy data -> now we can perform an inline LU decomposition
     MatrixCopy(LUDecomp, LineWidthLU, A, LineWidthA, width, width);
     Result := MatrixLUDecompInPlace(LUDecomp, lineWidthLU, width, indx, progress);
end;

function MatrixLUDecomp(A : PDouble; const LineWidthA : TASMNativeInt; LUDecomp : PDouble; const LineWidthLU : TASMNativeInt; width : TASMNativeInt; progress : TLinEquProgress) : TLinEquResult;
var indx : array of integer;
begin
     Assert(width > 0, 'Dimension Error');
     setLength(indx, width);

     Result := MatrixLUDecomp(A, LineWidthA, LUDecomp, LineWidthLU, width, @indx[0], progress);
end;

procedure MatrixLUBackSubst(LUDecomp : PDouble; const LineWidthLU : TASMNativeInt; width : TASMNativeInt; const  indx : PIntegerArray;
  B : PDouble; const LineWidthB : TASMNativeInt; progress : TLinEquProgress);
var i : TASMNativeInt;
    ii : TASMNativeInt;
    ip : TASMNativeInt;
    j : TASMNativeInt;
    sum : double;
    pB : PDouble;
    pB2 : PDouble;
    pVal : PDouble;
    pVal2 : PDouble;
begin
     assert(width*sizeof(double) <= LineWidthLU, 'Dimension Error');

     if Assigned(progress) then
        progress(0);

     ii := -1;
     pB2 := B;
     for i := 0 to width - 1 do
     begin
          ip := indx^[i];
          pB := B;
          inc(PByte(pB), ip*LineWidthB);
          sum := pB^;
          pB^ := pB2^;

          if ii >= 0 then
          begin
               pVal := PDouble(PAnsiChar(LUDecomp) + i*LineWidthLU);
               inc(pVal, ii);
               pB := B;
               inc(PByte(pB), LineWidthB*ii);
               for j := ii to i - 1 do
               begin
                    sum := sum - pVal^*pB^;
                    inc(pVal);
                    inc(PByte(pB), LineWidthB);
               end;
          end
          else if sum <> 0
          then
              ii := i;

          pB2^ := sum;
          inc(PByte(pB2), LineWidthB);
     end;

     if Assigned(progress) then
        progress(50);

     pB := B;
     inc(PByte(pB), LineWidthB*(width - 1));
     pVal := PDouble(PAnsiChar(LUDecomp) + (width - 1)*LineWidthLU);
     inc(pVal, width - 1);
     for i := width - 1 downto 0 do
     begin
          sum := pB^;

          pB2 := pB;
          inc(PByte(pB2), LineWidthB);
          pVal2 := pVal;
          inc(pVal2);
          for j := i + 1 to width - 1 do
          begin
               sum := sum - pVal2^*pB2^;
               inc(PByte(pB2), LineWidthB);
               inc(pVal2);
          end;

          pB^ := sum/pVal^;

          dec(pVal);
          dec(PByte(pVal), LineWidthLU);
          dec(PByte(pB), LineWidthB);
     end;

     if Assigned(progress) then
        progress(100);
end;

function MatrixInverseInPlace(A : PDouble; const LineWidthA : TASMNativeInt; width : TASMNativeInt; progress : TLinEquProgress) : TLinEquResult;
var Y : PDouble;
    indx : array of integer;
    i, j : TASMNativeInt;
    pVal : PDouble;
    col : TDoubleDynArray;
    w : TASMNativeInt;
begin
     Assert(lineWidthA >= width*sizeof(double), 'Dimension Error');
     Assert(width > 0, 'Dimension error');

     w := width + width and $01;
     Y := GetMemory(w*w*sizeof(double));
     SetLength(indx, width);
     SetLength(col, width);

     MatrixCopy(Y, sizeof(double)*w, A, LineWidthA, width, width);
     Result := MatrixLUDecompInPlace(Y, w*sizeof(double), width, @indx[0], progress);

     if Result = leSingular then
     begin
          FreeMem(Y);
          exit;
     end;

     for j := 0 to width - 1 do
     begin
          pVal := A;
          inc(pVal, j);

          for i := 0 to width - 1 do
              col[i] := 0;
          col[j] := 1;
          MatrixLUBackSubst(Y, w*sizeof(double), width, @indx[0], @col[0], sizeof(double));

          for i := 0 to width - 1 do
          begin
               pVal^ := col[i];
               inc(PByte(pVal), LineWidthA);
          end;
     end;

     FreeMem(Y);
end;

function MatrixDeterminant(A : PDouble; const LineWidthA : TASMNativeInt; width : TASMNativeInt; progress : TLinEquProgress) : double;
var LUDecomp : PDouble;
    indx : Array of Integer;
    i : TASMNativeInt;
    pVal : PDouble;
    parity : TASMNativeInt;
    rc : TRecMtxLUDecompData;
    w : TASMNativeInt;
    mem : Array[0..(4+4*cBlkMultSize*cBlkMultSize)] of double;
begin
     assert(width > 0, 'Dimension error');
     assert(LineWidthA >= width*sizeof(double), 'Dimension error');

     w := width + width and $01;
     LUDecomp := GetMemory(w*w*sizeof(double));
     SetLength(indx, width);
     MatrixCopy(LUDecomp, w*sizeof(double), A, LineWidthA, width, width);

     rc.progress := progress;
     rc.numCols := width;
     rc.numCalc := 0;
     rc.blkMultMem := PDouble(TASMNativeUInt(@mem[0]) + 16 - TASMNativeUInt(@mem[0]) and $0F);
     rc.LineWidth := w*sizeof(double);

     parity := 1;
     if InternalRecursiveMatrixLUDecompInPlace(LUDecomp, width, width, @indx[0], parity, rc) = leSingular then
     begin
          Result := 0;
          FreeMem(LUDecomp);
          exit;
     end;
     pVal := LUDecomp;
     Result := parity;
     for i := 0 to width - 1 do
     begin
          Result := Result * pVal^;
          inc(pVal);
          inc(PByte(pVal), w*sizeof(double));
     end;

     FreeMem(LUDecomp);
end;

{ TLinearEQProgress }

procedure TLinearEQProgress.LUDecompSolveProgress(Progress: integer);
begin
     if numRefinenmentSteps > 0 
     then
         refProgress(progress*8 div 10)
     else
         refProgress(progress);
end;

procedure TLinearEQProgress.RefinementProgress(Progress: integer);
begin
     refProgress(80 + 2*progress div 10);
end;

function MatrixLinEQSolve(A : PDouble; const LineWidthA : TASMNativeInt; width : TASMNativeInt; B : PDouble; const LineWidthB : TASMNativeInt; X : PDouble;
 const LineWidthX : TASMNativeInt;  Width2 : TASMNativeInt; const NumRefinments : TASMNativeInt; progress : TLinEquProgress) : TLinEquResult;
var indx : Array of Integer;
    LUDecomp : TDoubleDynArray;
    sdp : double;
    row : TDoubleDynArray;
    pB : PDouble;
    i : TASMNativeInt;
    pA : PDouble;
    j, k : TASMNativeInt;
    pX : PDouble;
    pVal : PDouble;
    refinementCounter : TASMNativeInt;
    progObj : TLinearEQProgress;
    progRef : TLinEquProgress;
begin
     progRef := nil;
     progObj := nil;
     if Assigned(progress) then
     begin
          progObj := TLinearEQProgress.Create;
          progObj.refProgress := progress;
          progObj.numRefinenmentSteps := NumRefinments;
          progRef := {$IFDEF FPC}@{$ENDIF}progObj.LUDecompSolveProgress;
     end;


     // ###########################################
     // #### Standard LU Decomposition
     SetLength(LUDecomp, width*width);
     SetLength(indx, width);
     Result := MatrixLUDecomp(A, LineWidthA, @LUDecomp[0], width*sizeof(double), width, @indx[0], progRef);

     if Result = leSingular then
     begin
          progObj.Free;
          exit;
     end;

     for i := 0 to width2 - 1 do
     begin
          // copy one column
          pX := X;
          inc(pX, i);
          pVal := B;
          inc(pVal, i);
          for j := 0 to width - 1 do
          begin
               pX^ := pVal^;
               inc(PByte(pX), LineWidthX);
               inc(PByte(pVal), LineWidthB);
          end;
          pX := X;
          inc(pX, i);

          // calculate vector X
          MatrixLUBackSubst(@LUDecomp[0], width*sizeof(double), width, @indx[0], pX, LineWidthX);
     end;

     // ###########################################
     // #### Iterative refinements
     if NumRefinments > 0 then
     begin
          SetLength(row, width);
          // for each solution do a separate refinement:
          for k := 0 to width2 - 1 do
          begin
               if Assigned(progobj) then
                  progObj.RefinementProgress(Int64(k)*100 div Int64(width2));

               for refinementCounter := 0 to NumRefinments - 1 do
               begin
                    pb := B;

                    pA := A;
                    for i := 0 to width - 1 do
                    begin
                         pVal := pA;

                         sdp := -pB^;
                         inc(PByte(pB), LineWidthB);
                         pX := X;
                         for j := 0 to width - 1 do
                         begin
                              sdp := sdp + pX^*pVal^;
                              inc(pVal);
                              inc(pX);
                         end;

                         inc(PByte(pA), LineWidthA);
                         row[i] := sdp;
                    end;

                    MatrixLUBackSubst(@LUDecomp[0], sizeof(double)*width, width, @indx[0], @row[0], sizeof(double));
                    pX := X;
                    for i := 0 to width - 1 do
                    begin
                         pX^ := pX^ - row[i];
                         inc(PByte(pX), LineWidthX);
                    end;
               end;

               inc(B);
               inc(X);
          end;
     end;

     if Assigned(progObj) then
        progObj.Free;
     if Assigned(progress) then
        progress(100);
end;

function MatrixCholeskyInPlace(A : PDouble; const LineWidthA : TASMNativeInt; width : TASMNativeInt; P : PDouble;
  LineWidthP : TASMNativeInt; progress : TLinEquProgress) : TCholeskyResult;
var i, j, k : TASMNativeInt;
    sum : double;
    pA : PDouble;
    pA1 : PDouble;
    pAj : PDouble;
    pAj1 : PDouble;
begin
     assert(LineWidthA >= width*sizeof(double), 'Dimension error');

     if Assigned(progress) then
        progress(0);

     Result := crNoPositiveDefinite;
     pA := A;

     for i := 0 to width - 1 do
     begin
          if Assigned(progress) then
             progress(int64(i)*100 div width);

          pAj := pA;

          for j := i to width - 1 do
          begin
               sum := PDouble(PAnsiChar(pA) + j*sizeof(double))^;

               pAj1 := pAj;
               pA1 := pA;
               if i > 0 then
               begin
                    inc(pAj1, i - 1);
                    inc(pA1, i - 1);
                    for k := i - 1 downto 0 do
                    begin
                         sum := sum - pA1^*pAj1^;

                         dec(pA1);
                         dec(pAj1);
                    end;
               end;

               if i = j then
               begin
                    // check for Positive definite matrix
                    if sum <= 0 then
                       exit;

                    PDouble(PAnsiChar(P) + i*LineWidthP)^ := sqrt(sum);
               end
               else
               begin
                    pAj1 := pAj;
                    inc(pAj1, i);
                    pAj1^ := sum/PDouble(PAnsiChar(P) + i*LineWidthP)^;
               end;

               inc(PByte(pAj), LineWidthA);
          end;

          inc(PByte(pA), LineWidthA);
     end;

     if Assigned(progress) then
        progress(100);

     Result := crOk;
end;

function MatrixCholesky(dest : PDouble; const LineWidthDest : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt;
  width : TASMNativeInt; P : PDouble; const LineWidthP : TASMNativeInt; progress : TLinEquProgress) : TCholeskyResult;
var pDest : PDouble;
    pA : PDouble;
    i : TASMNativeInt;
begin
     // copy A to dest
     pA := A;
     pDest := Dest;
     for i := 0 to width - 1 do
     begin
          Move(pA^, pDest^, sizeof(double)*width);
          inc(PByte(pDest), LineWidthDest);
          inc(PByte(pA), LineWidthA);
     end;

     Result := MatrixCholeskyInPlace(dest, LineWidthDest, width, P, LineWidthP, progress);
end;

procedure MatrixCholeskySolveLinEq(A : PDouble; const LineWidthA : TASMNativeInt; width : TASMNativeInt; P : PDouble;
  const LineWidthP : TASMNativeInt; B : PDouble; const LineWidthB : TASMNativeInt; X : PDouble; const LineWidthX : TASMNativeInt;
  progress : TLinEquProgress);
var i, k : TASMNativeInt;
    sum : double;
    pX : PDouble;
    pXi : PDouble;
    pA : PDouble;
    pB : PDouble;
    pP : PDouble;
begin
     pB := B;
     pP := P;
     pXi := X;

     if Assigned(progress) then
        progress(0);

     // Solve L*y = b
     for i := 0 to width - 1 do
     begin
          sum := pB^;
          if i > 0 then
          begin
               pX := pXi;
               dec(PByte(pX), LineWidthX);
               pA := A;
               inc(PByte(pA), i*LineWidthA);
               inc(pA, i - 1);
               for k := i - 1 downto 0 do
               begin
                    sum := sum - pA^*pX^;
                    dec(pA);
                    dec(PByte(pX), LineWidthX);
               end;
          end;

          pXi^ := sum/pP^;

          inc(PByte(pB), LineWidthB);
          inc(PByte(pP), LineWidthP);
          inc(PByte(pXi), LineWidthX);
     end;

     if Assigned(progress) then
        progress(50);

     // Solve L' * x = y
     pP := P;
     inc(PByte(pP), (width - 1)*LineWidthP);
     pXi := X;
     inc(PByte(pXi), (width - 1)*LineWidthX);
     for i := width - 1 downto 0 do
     begin
          sum := pXi^;
          pX := pXi;

          if i < width - 1 then
          begin
               inc(PByte(pX), LineWidthX);

               pA := A;
               inc(pA, i);
               inc(PByte(pA), (i + 1)*LineWidthA);

               for k := i + 1 to width - 1 do
               begin
                    sum := sum - pA^*pX^;
                    inc(PByte(pX), LineWidthX);
                    inc(PByte(pA), LineWidthA);
               end;
          end;

          pXi^ := sum/pP^;
          dec(PByte(pP), LineWidthP);
          dec(PByte(pXi), LineWidthX);
     end;

     if Assigned(progress) then
        progress(100);
end;


// ##########################################################
// #### Cholesky routines
// ##########################################################

function InternalBlkCholeskyInPlace(A : PDouble; const LineWidthA : TASMNativeInt; width : TASMNativeInt; pnlSize : TASMNativeInt;
   aMatrixMultT2Ex : TMatrixBlockedMultfunc; multMem : PDouble; progress : TLinEquProgress = nil) : TCholeskyResult;
var j : integer;
    jb : integer;
    pAjj : PDouble;
    pA1 : PDouble;
    pAjb, pAj, pAjjb : PDouble;
begin
     Result := crOk;

     j := 0;
     pAjj := A;
     pA1 := A;
     while j < width do
     begin
          jb := min(pnlSize, width - j);

          GenericSymRankKUpd(pAjj, LineWidthA, pA1, LineWidthA, j, jb, -1, 1);

          Result := MatrixCholeskyInPlace2(pAjj, LineWidthA, jb, nil);

          if result <> crOk then
             exit;

          // compute the current block column
          if j + jb < width then
          begin
               pAjb := A;
               inc(PByte(pAjb), (j + jb)*LineWidthA);
               pAj := A;
               inc(PByte(pAj), j*LineWidthA);
               pAjjb := pAjb;
               inc(pAjjb, j);

               aMatrixMultT2Ex(pAjjb, LineWidthA, pAjb, pAj, j, width - j - jb, j, jb,
                              LineWidthA, LineWidthA, jb, doSub, multMem);

               GenericSolveLoTriMtxTranspNoUnit(pAjj, LineWidthA, pAjjb, LineWidthA, jb, width - j - jb);
          end;

          inc(pAjj, pnlSize);
          inc(PByte(pAjj), pnlSize*LineWidthA);
          inc(PByte(pA1), pnlSize*LineWidthA);
          inc(j, pnlSize);

          if assigned(progress) then
             progress(Int64(j)*100 div Int64(width));
     end;
end;

function MatrixCholeskyInPlace4(A : PDouble; const LineWidthA : TASMNativeInt; width : TASMNativeInt; pnlSize : TASMNativeInt; progress : TLinEquProgress = nil) : TCholeskyResult;
var multMem : PDouble;
begin
     if pnlSize = 0 then
        pnlSize := CholBlockSize;

     // single line version
     if width <= pnlSize then
     begin
          Result := MatrixCholeskyInPlace2(A, LineWidthA, width, progress);
          exit;
     end;

     // ###########################################
     // #### Blocked code
     // ###########################################
     multMem := GetMemory(BlockMultMemSize(pnlSize));
     Result := InternalBlkCholeskyInPlace(A, LineWidthA, width, pnlSize, {$IFDEF FPC}@{$ENDIF}MatrixMultT2Ex, multMem, progress);
     FreeMem(multMem);
end;


// recursive cholesky decomposition:
// This is the recursive version of the algorithm. It divides
// the matrix into four submatrices:
//
//        [  A11 | A12  ]  where A11 is n1 by n1 and A22 is n2 by n2
//    A = [ -----|----- ]  with n1 = n/2
//        [  A21 | A22  ]       n2 = n-n1
//
// The subroutine calls itself to factor A11. Update and scale A21
// or A12, update A22 then call itself to factor A22.



function InternaRecursivelCholeskyInPlace(A : PDouble; const LineWidthA : TASMNativeInt; width : TASMNativeInt) : TCholeskyResult;
var n1, n2 : TASMNativeInt;
    A21, A22 : PDouble;
begin
     Result := crNoPositiveDefinite;

     if width = 1 then
     begin
          if (A^ <= 0) or IsNan(A^) then
             exit;

          A^ := Sqrt(A^);
     end
     else
     begin
          n1 := width div 2;
          n2 := width - n1;

          if n1 > 0 then
          begin
               Result := InternaRecursivelCholeskyInPlace(A, LineWidthA, n1);

               if Result <> crOk then
                  exit;
          end;

          // lower triangualar cholesky:
          A21 := A;
          inc(PByte(A21), n1*LineWidthA);
          GenericSolveLoTriMtxTranspNoUnit(A, LineWidthA, A21, LineWidthA, n1, n2);

          // Update and factor A22
          A22 := A;
          inc(PByte(A22), n1*LineWidthA);
          inc(A22, n1);
          GenericSymRankKUpd(A22, LineWidthA, A21, LineWidthA, n1, n2, -1, 1);

          Result := InternaRecursivelCholeskyInPlace(A22, LineWidthA, n2);

          if Result <> crOk then
             exit;
     end;

     Result := crOk;
end;


function MatrixCholeskyInPlace3(A : PDouble; const LineWidthA : TASMNativeInt; width : TASMNativeInt; progress : TLinEquProgress = nil) : TCholeskyResult;
begin
     Result := InternaRecursivelCholeskyInPlace(A, LineWidthA, width)
end;

// single line but memory access optimal version of the cholesky decomposition
function MatrixCholeskyInPlace2(A : PDouble; const LineWidthA : TASMNativeInt; width : TASMNativeInt; progress : TLinEquProgress = nil) : TCholeskyResult;
var i : TASMNativeInt;
    j, k : TASMNativeInt;
    pAi : PDouble;
    sum : double;
    pAij : PDouble;
    pAik, pAjk : PDouble;
    pAjj : PDouble;
begin
     Result := crNoPositiveDefinite;

     // lower triangular cholesky decomposition
     for i := 0 to width - 1 do
     begin
          pAi := A;
          inc(PByte(pAi), i*LineWidthA);
          pAij := PAi;
          pAjj := A;
          for j := 0 to i do
          begin
               sum := pAij^;

               pAik := pAi;
               pAjk := A;
               inc(PByte(pAjk), j*LineWidthA);
               for k := 0 to j - 1 do
               begin
                    sum := sum - pAik^*pAjk^;
                    inc(pAik);
                    inc(pAjk);
               end;

               if i > j
               then
                   pAij^ := sum/pAjj^
               else if sum > 0 then
               begin
                    pAik := pAi;
                    inc(pAik, i);
                    pAik^ := sqrt(sum);
               end
               else
                   exit;   // not positive definite

               inc(pAij);
               inc(pAjj);
               inc(PByte(pAjj), LineWidthA);
          end;

          if Assigned(progress) then
             Progress(i*100 div (width - 1));
     end;

     Result := crOk;
end;

end.

