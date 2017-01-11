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


// Inplace svd decomposition of a Matrix A
// The output is the computation of A= U*W*V' whereas U is stored in A, and W is a vector 0..Width-1. The matrix V (not V') must be as large as Width*Width!
function MatrixSVDInPlace(A : PDouble; const LineWidthA : TASMNativeInt; width : TASMNativeInt; Height : TASMNativeInt; W : PDouble; const LineWidthW : TASMNativeInt;
                           V : PDouble; const LineWidthV : TASMNativeInt; progress : TLinEquProgress = nil) : TSVDResult;
function MatrixSVD(A : PDouble; const LineWidthA : TASMNativeInt; width : TASMNativeInt; Height : TASMNativeInt;
                   U : PDouble; const LineWidthU : TASMNativeInt; W : PDouble; const LineWidthW : TASMNativeInt;
                   V : PDouble; const LineWidthV : TASMNativeInt; progress : TLinEquProgress = nil) : TSVDResult;

function dgesvd( A : PDouble; const LineWidthA : TASMNativeInt; width : TASMNativeInt; Height : TASMNativeInt; 
                 W : PConstDoubleArr; V : PDouble; const LineWidthV : TASMNativeInt; SVDBlockSize : TASMNativeInt = 32; aWork : PByte = nil;
                 progress : TLinEquProgress = nil) : TSVDResult;
                 
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


// original functions from Numerical Recipies:
// In place QR decomposition. Constructs the QR decomposition of A (n*n). The upper triangle matrix R is returned
// in the upper triangle of a, except for the diagonal elements of R which are returned in
// d. The orthogonal matrix Q is represented as a product of n-1 Householder matrices Q1...Qn-1, where
// Qj = 1 - uj*(uj/cj). The ith component of uj is zero for i = 1..j-1 while the nonzero components are returned
// in a[i][j] for i = j...n . False is returned if no singularity was detected
function MatrixQRDecompInPlace(A : PDouble; const LineWidthA : TASMNativeInt; width : TASMNativeInt; C : PDouble; const LineWidthC : TASMNativeInt;
  D : PDouble; const LineWidthD : TASMNativeInt; progress : TLinEquProgress = nil) : TQRResult;
function MatrixQRDecomp(dest : PDouble; const LineWidthDest : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; width : TASMNativeInt;
  C : PDouble; const LineWidthC : TASMNativeInt; D : PDouble; const LineWidthD : TASMNativeInt; progress : TLinEquProgress = nil) : TQRResult;
// solves the System A*x = b. The input paramaters are the output parameters from the QR decomposition.
// b is the matrix right hand side and will be overwritten by the result x.
procedure MatrixQRSolveLinEq(A : PDouble; const LineWidthA : TASMNativeInt; width : TASMNativeInt; C : PDouble; const LineWidthC : TASMNativeInt;
  D : PDouble; const LineWidthD : TASMNativeInt; B : PDouble; const LineWidthB : TASMNativeInt; progress : TLinEquProgress = nil);


// implementation of Lapack's blocked QR decomposition
// the upper triangle matrix R is returned in the upper triangle of A. The elements below the
// diagonal with the array TAU, represent the orthogonal matrix Q as a product of elementary reflectors.
// further details: The matrix Q is represented as a product of elementary reflectors
//   Q = H(1) H(2) . . . H(k), where k = min(m,n).
//   Each H(i) has the form
//   H(i) = I - tau * v * v**T
//   where tau is a real scalar, and v is a real vector with
//   v(1:i-1) = 0 and v(i) = 1; v(i+1:m) is stored on exit in A(i+1:m,i),
//   and tau in TAU(i).
// note the matrix above starts with index 1 instead of 0.
// the output is the same as the matlab economy size output on a QR decomposition e.g. dcmp = qr(A, 0);
function MatrixQRDecompInPlace2(A : PDouble; const LineWidthA : TASMNativeInt; width, height : TASMNativeInt; tau : PDouble; progress : TLinEquProgress = nil) : TQRResult; overload;
function MatrixQRDecompInPlace2(A : PDouble; const LineWidthA : TASMNativeInt; width, height : TASMNativeInt; tau : PDouble; work : PDouble; pnlSize : TASMNativeInt; progress : TLinEquProgress = nil) : TQRResult; overload;

// according to the panel size and the global QRBlockmultsize we calculate here the needed memory (including some alignment buffer)
function QRDecompMemSize( pnlSize, width : TASMNativeInt) : TASMNativeInt;

// implementation of Lapacks dorgqr function: On start the matrix A and Tau contains the result of
// the MatrixQRDecompInPlace2 function (economy size QR Decomposition). On output A is replaced by the full Q
// matrix with orthonormal columns.
procedure MatrixQFromQRDecomp(A : PDouble; const LineWidthA : TASMNativeInt; width, height : TASMNativeInt; tau : PDouble; progress : TLinEquProgress = nil); overload;
procedure MatrixQFromQRDecomp(A : PDouble; const LineWidthA : TASMNativeInt; width, height : TASMNativeInt; tau : PDouble; BlockSize : TASMNativeInt; work : PDouble; progress : TLinEquProgress = nil); overload;

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

// ###########################################
// #### SVD
// ###########################################
  
procedure dgebd2(A : PDouble; const LineWidthA : TASMNativeInt; Width, Height : TASMNativeInt; 
                 D, E, TauQ, TauP : PConstDoubleArr; work : PDouble);

  

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


// ###########################################
// #### only for internal use
// ###########################################
type
  TRecMtxQRDecompData = record
    pWorkMem : PByte;
    work : PDouble;
    LineWidthWork : TASMNativeInt;
    BlkMultMem : PDouble;
    Progress : TLinEquProgress;
    qrWidth, qrHeight : TASMNativeInt;
    actIdx : TASMNativeInt;
    pnlSize : TASMNativeInt;

    MatrixMultT1 : TMatrixBlockedMultfunc;
    MatrixMultT2 : TMatrixBlockedMultfunc;
  end;

function InternalMatrixQRDecompInPlace2(A : PDouble; const LineWidthA : TASMNativeInt; width, height : TASMNativeInt; tau : PDouble; qrData : TRecMtxQRDecompData) : boolean;
procedure InternalBlkMatrixQFromQRDecomp(A : PDouble; const LineWidthA : TASMNativeInt; width, height : TASMNativeInt; tau : PDouble; qrData : TRecMtxQRDecompData);

function InternalBlkCholeskyInPlace(A : PDouble; const LineWidthA : TASMNativeInt; width : TASMNativeInt; pnlSize : TASMNativeInt; 
   aMatrixMultT2Ex : TMatrixBlockedMultfunc; multMem : PDouble; progress : TLinEquProgress = nil) : TCholeskyResult;
   
implementation

uses ASMMatrixOperations, SimpleMatrixOperations, BlockSizeSetup, Classes;

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

function MatrixSVDInPlace(A : PDouble; const LineWidthA : TASMNativeInt; width : TASMNativeInt; Height : TASMNativeInt; W : PDouble; const LineWidthW : TASMNativeInt;
  V : PDouble; const LineWidthV : TASMNativeInt; progress : TLinEquProgress) : TSVDResult;
const cMaxNumSVDIter = 75;
var flag : boolean;
    i, j, jj, k, l : TASMNativeInt;
    its : TASMNativeInt;
    nm : TASMNativeInt;
    anorm : double;
    c, f, g, h : double;
    s, scale, x, y, z : double;
    rv1 : Array of double;
    pA : PDouble;
    pAi : PDouble;
    pAj : PDouble;
    pV : PDouble;
    pVj : PDouble;
    invScale : double;
    invh : double;
    val : double;
    pWi : PDouble;
begin
     assert(LineWidthA >= width*sizeof(double), 'Dimension error');
     assert(LineWidthV >= width*sizeof(double), 'Dimension error');

     if Assigned(progress) then
        progress(0);

     SetLength(rv1, width);

     g := 0;
     scale := 0;
     anorm := 0;
     l := 1;
     nm := 0;

     Result := srNoConvergence;

     pWi := W;
     // Householder reduction to bidiagonal form
     pAi := A;
     for i := 0 to width - 1 do
     begin
          if Assigned(progress) then
             progress(0 + Round(10*(i/width)));

          l := i + 1;

          rv1[i] := scale*g;
          g := 0;
          s := 0;
          scale := 0;

          if i < height then
          begin
               pA := pAi;
               inc(pA, i);

               for k := i to height - 1 do
               begin
                    scale := scale + abs(pA^);

                    inc(PByte(pA), LineWidthA);
               end;

               if scale <> 0 then
               begin
                    pA := pAi;
                    inc(pA, i);
                    invScale := 1/scale;

                    for k := i to height - 1 do
                    begin
                         pA^ := pA^*invScale;
                         s := s + sqr(pA^);

                         inc(PByte(pA), LineWidthA);
                    end;

                    pA := pAi;
                    inc(pA, i);
                    f := pA^;

                    g := -sign(sqrt(s), f);
                    h := f*g - s;
                    pA^ := f - g;
                    invh := 1/h;

                    for j := l to width - 1 do
                    begin
                         s := 0;
                         pA := pAi;
                         pAj := pA;
                         inc(pA, i);
                         inc(pAj, j);

                         for k := i to height - 1 do
                         begin
                              s := s + pA^*pAj^;
                              inc(PByte(pA), LineWidthA);
                              inc(PByte(pAj), LineWidthA);
                         end;
                         f := s*invh;

                         pA := pAi;
                         pAj := pA;
                         inc(pA, i);
                         inc(pAj, j);
                         for k := i to height - 1 do
                         begin
                              pAj^ := pAj^ + f*pA^;
                              inc(PByte(pAj), LineWidthA);
                              inc(PByte(pA), LineWidthA);
                         end;
                    end;

                    pA := pAi;
                    inc(pA, i);
                    for k := i to height - 1 do
                    begin
                         pA^ := scale*pA^;
                         inc(PByte(pA), LineWidthA);
                    end;
               end; // end if scale
          end; // end if i < height

          pWi^ := scale*g;

          g := 0;
          scale := 0;
          s := 0;

          if (i < width) and (i <> width - 1) then
          begin
               pA := pAi;
               inc(pA, l);

               for k := l to width - 1 do
               begin
                    scale := scale + abs(pA^);

                    inc(pA);
               end;

               if scale <> 0 then
               begin
                    pA := pAi;
                    inc(pA, l);
                    invScale := 1/scale;

                    for k := l to width - 1 do
                    begin
                         pA^ := pA^*invScale;
                         s := s + sqr(pA^);

                         inc(pA);
                    end;

                    pA := pAi;
                    inc(pA, l);
                    f := pA^;

                    g := -sign(sqrt(s), f);
                    h := f*g - s;
                    pA^ := f - g;
                    invh := 1/h;

                    for k := l to width - 1 do
                    begin
                         rv1[k] := pA^*invh;
                         inc(pA);
                    end;

                    for j := l to height - 1 do
                    begin
                         s := 0;
                         pAj := A;
                         inc(pAj, l);
                         pA := pAi;
                         inc(pA, l);
                         inc(PByte(pAj), LineWidthA*j);

                         for k := l to width - 1 do
                         begin
                              s := s + pAj^*pA^;
                              inc(pAj);
                              inc(pA);
                         end;

                         pA := A;
                         inc(PByte(pA), LineWidthA*j);
                         inc(pA, l);
                         for k := l to width - 1 do
                         begin
                              pA^ := pA^ + s*rv1[k];
                              inc(pA);
                         end;
                    end;

                    pA := pAi;
                    inc(pA, l);

                    for k := l to width - 1 do
                    begin
                         pA^ := pA^*scale;
                         inc(pA);
                    end;
               end;
          end;

          anorm := max(anorm, abs(pWi^) + abs(rv1[i]));

          inc(PByte(pWi), LineWidthW);
          inc(PByte(pAi), LineWidthA);
     end;

     // accumulation of right-hand transformations
     pAi := A;
     inc(PByte(pAi), (width - 1)*LineWidthA);
     for i := width - 1 downto 0 do
     begin
          if Assigned(progress) then
             progress(Round(10 + 10*((width - 1 - i)/(width))));

          if i < width - 1 then
          begin
               if g <> 0 then
               begin
                    pV := V;
                    inc(pV, i);
                    inc(PByte(pV), LineWidthV*l);
                    pA := pAi;
                    inc(pA, l);
                    val := pA^;
                    val := 1/(val*g);

                    for j := l to width - 1 do
                    begin
                         pV^ := pA^*val;
                         inc(pA);
                         inc(PByte(pV), LineWidthV);
                    end;

                    for j := l to width - 1 do
                    begin
                         s := 0;

                         pA := pAi;
                         inc(pA, l);
                         pV := V;
                         inc(pV, j);
                         inc(PByte(pV), LineWidthV*l);

                         for k := l to width - 1 do
                         begin
                              s := s + pA^*pV^;
                              inc(pA);
                              inc(PByte(pV), LineWidthV);
                         end;

                         pV := V;
                         inc(pV, i);
                         inc(PByte(pV), LineWidthV*l);
                         pVj := V;
                         inc(pVj, j);
                         inc(PByte(pVj), LineWidthV*l);

                         for k := l to width - 1 do
                         begin
                              pVj^ := pVj^ + s*pV^;
                              inc(PByte(pVj), LineWidthV);
                              inc(PByte(pV), LineWidthV);
                         end;
                    end;
               end;  // end if g <> 0

               pV := V;
               inc(pV, l);
               inc(PByte(pV), LineWidthV*i);
               pVj := V;
               inc(pVj, i);
               inc(PByte(pVj), LineWidthV*l);

               for j := l to width - 1 do
               begin
                    pV^ := 0;
                    pVj^ := 0;
                    inc(pV);
                    inc(PByte(pVj), LineWidthV);
               end;
          end; // end if i < width -1
          pV := V;
          inc(pV, i);
          inc(PByte(pV), LineWidthV*i);

          pV^ := 1;
          g := rv1[i];
          l := i;

          dec(PByte(pAi), LineWidthA);
     end;

     pWi := W;
     inc(PByte(pWi), (min(width, height) - 1)*LineWidthW);
     pAi := A;
     inc(PByte(pAi), (min(width, height) - 1)*LineWidthA);
     // accumulation of left-hand transformations
     for i := min(width, height) - 1 downto 0 do
     begin
          if Assigned(progress) then
             progress(Round(20 + 10*((Min(width, height) - 1 - i)/(Min(width, height)))));

          l := i + 1;
          g := pWi^;

          pA := pAi;
          inc(pA, l);
          for j := l to width - 1 do
          begin
               pA^ := 0;
               inc(pA);
          end;

          if g <> 0 then
          begin
               g := 1/g;

               for j := l to width - 1 do
               begin
                    s := 0;
                    pA := A;
                    inc(pA, i);
                    inc(PByte(pA), l*LineWidthA);
                    pAj := A;
                    inc(pAj, j);
                    inc(PByte(pAj), l*LineWidthA);

                    for k := l to height - 1 do
                    begin
                         s := s + pA^*pAj^;
                         inc(PByte(pA), LineWidthA);
                         inc(PByte(pAj), LineWidthA);
                    end;

                    pA := pAi;
                    inc(pA, i);
                    f := (s/pA^)*g;
                    pAj := pAi;
                    inc(pAj, j);
                    for k := i to height - 1 do
                    begin
                         pAj^ := pAj^ + f*pA^;
                         inc(PByte(pAj), LineWidthA);
                         inc(PByte(pA), LineWidthA);
                    end;
               end;

               pA := pAi;
               inc(pA, i);
               for j := i to height - 1 do
               begin
                    pA^ := pA^*g;
                    inc(PByte(pA), LineWidthA);
               end;
          end
          else
          begin
               pA := pAi;
               inc(pA, i);
               for j := i to height - 1 do
               begin
                    pA^ := 0;
                    inc(PByte(pA), LineWidthA);
               end;
          end;
          pA := pAi;
          inc(pA, i);
          pA^ := pA^ + 1;

          dec(PByte(pWi), LineWidthW);
          dec(PByte(pAi), LineWidthA);
     end;

     // Diagonalization of the bidiagonal form: loop over singular values and over
     // allowed iterations
     for k := width - 1 downto 0 do
     begin
          if Assigned(progress) then
             progress(Round(30 + 70*((Min(width, height) - 1 - k)/(Min(width, height)))));

          for its := 0 to cMaxNumSVDIter - 1 do
          begin
               flag := true;
               // test for splitting
               for l := k downto 0 do
               begin
                    nm := l - 1;
                    if abs(rv1[l]) + anorm = anorm then
                    begin
                         flag := False;
                         break;
                    end;

                    if abs(PDouble(PAnsiChar(W) + nm*LineWidthW)^) + anorm = anorm then
                       break;
               end;

               // Cancellation of rv1[o] if l > 1
               if flag then
               begin
                    c := 0;
                    s := 1;
                    for i := l to k do
                    begin
                         f := s*rv1[i];
                         rv1[i] := c*rv1[i];
                         if abs(f) + anorm <> anorm then    // check if the value is lower than the precission in contrast to anorm
                         begin
                              g := PDouble(PAnsiChar(W) + i*LineWidthW)^;
                              h := pythag(f, g);
                              PDouble(PAnsiChar(W) + i*LineWidthW)^ := h;
                              h := 1/h;
                              c := g*h;
                              s := -f*h;
                              pA := A;
                              inc(pA, nm);
                              pAj := A;
                              inc(pAj, i);

                              for j := 0 to height - 1 do
                              begin
                                   y := pA^;
                                   z := pAj^;
                                   pA^ := y*c + z*s;
                                   pAj^ := z*c - y*s;

                                   inc(PByte(pA), LineWidthA);
                                   inc(PByte(pAj), LineWidthA);
                              end;
                         end;
                    end;
               end;

               z := PDouble(PAnsiChar(W) + k*LineWidthW)^;
               // convergence
               if l = k then
               begin
                    if z < 0 then
                    begin
                         PDouble(PAnsiChar(W) + k*LineWidthW)^ := -z;

                         pV := V;
                         inc(pV, k);

                         for j := 0 to width - 1 do
                         begin
                              pV^ := -pV^;
                              inc(PByte(pV), LineWidthV);
                         end;
                    end;

                    break;
               end;

               if its = cMaxNumSVDIter - 1 then
                  exit;

               x := PDouble(PAnsiChar(W) + l*LineWidthW)^;
               nm := k - 1;
               y := PDouble(PAnsiChar(W) + nm*LineWidthW)^;
               g := rv1[nm];
               h := rv1[k];
               f := ((y - z)*(y + z) + (g - h)*(g + h))/(2*h*y);
               g := pythag(f, 1);
               val := g;
               if f < 0 then
                  val := -val;
               f := ((x - z)*(x + z) + h*((y/(f + val)) - h))/x;
               c := 1;
               s := 1;

               // next QR transformation
               for j := l to nm do
               begin
                    i := j + 1;
                    g := rv1[i];
                    y := PDouble(PAnsiChar(W) + i*LineWidthW)^;
                    h := s*g;
                    g := c*g;
                    z := pythag(f, h);
                    rv1[j] := z;
                    c := f/z;
                    s := h/z;
                    f := x*c + g*s;
                    g := g*c - x*s;
                    h := y*s;
                    y := y*c;

                    pVj := V;
                    inc(pVj, j);
                    pV := V;
                    inc(pV, i);
                    for jj := 0 to width - 1 do
                    begin
                         x := pVj^;
                         z := pV^;
                         pVj^ := x*c + z*s;
                         pV^ := z*c - x*s;

                         inc(PByte(pV), LineWidthV);
                         inc(PByte(pVj), LineWidthV);
                    end;

                    z := pythag(f, h);
                    PDouble(PAnsiChar(W) + j*LineWidthW)^ := z;
                    // rotation can be arbitrary if z = 0
                    if z <> 0 then
                    begin
                         z := 1/z;
                         c := f*z;
                         s := h*z;
                    end;

                    f := c*g + s*y;
                    x := c*y - s*g;

                    pA := A;
                    inc(pA, i);
                    pAj := A;
                    inc(pAj, j);
                    for jj := 0 to height - 1 do
                    begin
                         y := pAj^;
                         z := pA^;
                         pAj^ := y*c + z*s;
                         pA^ := z*c - y*s;
                         inc(PByte(pAj), LineWidthA);
                         inc(PByte(pA), LineWidthA);
                    end;
               end;

               rv1[l] := 0;
               rv1[k] := f;
               PDouble(PAnsiChar(W) + k*LineWidthW)^ := x;
          end;
     end;

     Result := srOk;
end;

function MatrixSVD(A : PDouble; const LineWidthA : TASMNativeInt; width : TASMNativeInt; Height : TASMNativeInt;
                   U : PDouble; const LineWidthU : TASMNativeInt; W : PDouble; const LineWidthW : TASMNativeInt;
                   V : PDouble; const LineWidthV : TASMNativeInt; progress : TLinEquProgress) : TSVDResult;
begin
     assert(LineWidthA >= width*sizeof(double), 'Dimension error');
     assert(LineWidthV >= width*sizeof(double), 'Dimension error');

     MatrixCopy(U, LineWidthU, A, LineWidthA, width, Height);
     Result := MatrixSVDInPlace(U, LineWidthU, width, Height, W, LineWidthW, V, LineWidthV, progress);
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



function MatrixQRDecompInPlace(A : PDouble; const LineWidthA : TASMNativeInt; width : TASMNativeInt; C : PDouble; const LineWidthC : TASMNativeInt;
  D : PDouble; const LineWidthD : TASMNativeInt; progress : TLinEquProgress) : TQRResult;
var i, j, k : TASMNativeInt;
    scale, sigma, sum, tau : double;
    pA, pAj : PDouble;
    pC : PDouble;
    pD : PDouble;
    pAk : PDouble;
begin
     assert(LineWidthA >= width*sizeof(double), 'Dimension error');
     assert(LineWidthC >= sizeof(double), 'Dimension error');
     assert(LineWidthD >= sizeof(double), 'Dimension error');
     Result := qrOK;

     if Assigned(progress) then
        progress(0);

     pC := C;
     pD := D;
     pAk := A;
     for k := 0 to width - 2 do
     begin
          if Assigned(progress) then
             progress(Int64(k)*100 div width);

          scale := 0;
          pA := pAk;
          for i := k to width - 1 do
          begin
               scale := Max(scale, abs(pA^));
               inc(PByte(pA), LineWidthA);
          end;

          if scale = 0 then
          begin
               Result := qrSingular;
               pC^ := 0;
               pD^ := 0;
          end
          else
          begin
               pA := pAk;
               for i := k to width - 1 do
               begin
                    pA^ := pA^/scale;
                    inc(PByte(pA), LineWidthA);
               end;

               sum := 0;
               pA := pAk;
               for i := k to width - 1 do
               begin
                    sum := sum + sqr(pA^);
                    inc(PByte(pA), LineWidthA);
               end;

               sigma := sign(sqrt(sum), pAk^);
               pAk^ := pAk^ + sigma;
               pC^ := sigma*pAk^;
               pD^ := -scale*sigma;

               for j := k + 1 to width - 1 do
               begin
                    sum := 0;
                    pA := pAk;

                    pAj := pAk;
                    inc(pA, j - k);

                    for i := k to width - 1 do
                    begin
                         sum := sum + pA^*pAj^;
                         inc(PByte(pA), LineWidthA);
                         inc(PByte(pAj), LineWidthA);
                    end;

                    tau := sum/pC^;
                    pA := pAk;

                    pAj := pAk;
                    inc(pAj, j - k);
                    for i := k to width - 1 do
                    begin
                         pAj^ := pAj^ - tau*pA^;
                         inc(PByte(pA), LineWidthA);
                         inc(PByte(pAj), LineWidthA);
                    end;
               end;
          end;

          inc(PByte(pC), LineWidthC);
          inc(PByte(pD), LineWidthD);
          inc(pAk);
          inc(PByte(pAk), LineWidthA);
     end;

     pD := D;
     inc(PByte(pD), (width - 1)*LineWidthD);
     pA := A;
     inc(pA, width - 1);
     inc(PByte(pA), (width - 1)*LineWidthA);
     pD^ := pA^;
     if pD^ = 0 then
        Result := qrSingular;

     if Assigned(Progress) then
        progress(100);
end;

function MatrixQRDecomp(dest : PDouble; const LineWidthDest : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; width : TASMNativeInt;
  C : PDouble; const LineWidthC : TASMNativeInt; D : PDouble; const LineWidthD : TASMNativeInt; progress : TLinEquProgress) : TQRResult;
var pDest : PDouble;
    pA : PDouble;
    i : TASMNativeInt;
begin
     assert(LineWidthA >= width*sizeof(double), 'Dimension error');
     assert(LineWidthDest >= width*sizeof(double), 'Dimension error');
     assert(LineWidthC >= sizeof(double), 'Dimension error');
     assert(LineWidthD >= sizeof(double), 'Dimension error');

     // copy A to dest
     pA := A;
     pDest := Dest;
     for i := 0 to width - 1 do
     begin
          Move(pA^, pDest^, sizeof(double)*width);
          inc(PByte(pDest), LineWidthDest);
          inc(PByte(pA), LineWidthA);
     end;

     Result := MatrixQRDecompInPlace(dest, LineWidthDest, width, C, LineWidthC, D, LineWidthD, progress);
end;


procedure MatrixRSolve(A : PDouble; const LineWidthA : TASMNativeInt; width : TASMNativeInt; C : PDouble; const LineWidthC;
  D : PDouble; const LineWidthD : TASMNativeInt; B : PDouble; const LineWidthB : TASMNativeInt; progress : TLinEquProgress);
var i, j : TASMNativeInt;
    pA : PDouble;
    pAj : PDouble;
    pB : PDouble;
    pBj : PDOuble;
    pD : PDouble;
    sum : double;
begin
     pBj := B;
     inc(PByte(pBj), LineWidthB*(width - 1));
     pD := D;
     inc(PByte(pD), LineWidthD*(width - 1));
     pBj^ := pBj^/pD^;
     pAj := A;
     inc(PByte(pAj), (width - 2)*LineWidthA);
     inc(pAj, (width - 1));

     if Assigned(progress) then
        progress(0);

     for i := width - 2 downto 0 do
     begin
          sum := 0;

          pA := pAj;
          pB := pBj;
          for j := i + 1 to width - 1 do
          begin
               sum := sum + pA^*pB^;
               inc(pA);
               inc(PByte(pB), LineWidthB);
          end;

          dec(PByte(pBj), LineWidthB);
          dec(PByte(pD), LineWidthD);
          pBj^ := (pBj^ - sum)/pD^;

          dec(pAj);
          dec(PByte(pAj), LineWidthA);
     end;

     if Assigned(progress) then
        progress(100);
end;

procedure MatrixQRSolveLinEq(A : PDouble; const LineWidthA : TASMNativeInt; width : TASMNativeInt; C : PDouble; const LineWidthC : TASMNativeInt;
  D : PDouble; const LineWidthD : TASMNativeInt; B : PDouble; const LineWidthB : TASMNativeInt; progress : TLinEquProgress);
var i, j : TASMNativeInt;
    sum, tau : double;
    pA : PDouble;
    pAj : PDouble;
    pB : PDouble;
    pBj : PDOuble;
    pC : PDouble;
begin
     assert(LineWidthA >= width*sizeof(double), 'Dimension error');
     assert(LineWidthB >= sizeof(double), 'Dimension error');
     assert(LineWidthC >= sizeof(double), 'Dimension error');
     assert(LineWidthD >= sizeof(double), 'Dimension error');

     pAj := A;
     pBj := B;
     pC := C;

     for j := 0 to width - 2 do
     begin
          sum := 0;
          pA := pAj;
          pB := pBj;
          for i := j to width - 1 do
          begin
               sum := sum + pA^*pB^;
               inc(PByte(pA), LineWidthA);
               inc(PByte(pB), LineWidthB);
          end;

          tau := sum/pC^;

          pA := pAj;
          pB := pBj;
          for i := j to width - 1 do
          begin
               pB^ := pB^ - tau*pA^;
               inc(PByte(pB), LineWidthB);
               inc(PByte(pA), LineWidthA);
          end;

          inc(pAj);
          inc(PByte(pAj), LineWidthA);
          inc(PByte(pC), LineWidthC);
          inc(PByte(pBj), LineWidthB);
     end;

     MatrixRSolve(A, LineWidthA, width, C, LineWidthC, D, LineWidthD, B, LineWidthB, progress);
end;


// ##########################################################
// #### Blocked QR Decomposition methods
// ##########################################################

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

// original dger
// X, Y are vectors, incX, incY is the iteration in bytes from one to the next vector element
// A is a matrix
procedure Rank1Update(A : PDouble; const LineWidthA : TASMNativeInt; width, height : TASMNativeInt; 
  const alpha : double; X, Y : PDouble; incX, incY : TASMNativeInt); // {$IFNDEF FPC} {$IF CompilerVersion >= 17.0} inline; {$IFEND} {$ENDIF}
var i : TASMNativeInt;
    j : TASMNativeInt;
    tmp : double;
    pX, pY : PDouble;
    pA : PConstDoubleArr;
begin
     // performs A = A + alpha*X*Y' in row major form
     pX := X;
     for i := 0 to Height - 1 do
     begin
          tmp := alpha*pX^;
     
          pA := PConstDoubleArr(GenPtr(A, 0, i, LineWidthA));
          pY := Y;
          for j := 0 to Width - 1 do
          begin
               pA^[j] := pA^[j] + tmp*pY^;
               inc(PByte(pY), incY);
          end;

          inc(PByte(pX), incX);
     end;
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
          //GenericMtxVecMultT(work, sizeof(double), C, V, LineWidthC, LineWidthV, width, height, 1, 0);
          MatrixMtxVecMultT(work, sizeof(double), C, V, LineWidthC, LineWidthV, width, height, 1, 0);
          Rank1Update(C, LineWidthC, width, height, -tau^, V, work, LineWidthV, sizeof(double));
     end;
end;

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
          //GenericMtxVecMult(work, sizeof(double), C, V, LineWidthC, LineWidthV, width, height, 1, 0);
          MatrixMtxVecMult(work, sizeof(double), C, V, LineWidthC, LineWidthV, width, height, 1, 0);
          Rank1Update(C, LineWidthC, width, height, -Tau^, work, V, sizeof(double), LineWidthV);
     end;
end;

// implementation of lapack's DGEQR2
function MtxQRUnblocked(A : PDouble; LineWidthA : TASMNativeInt; width, height : TASMNativeInt; Tau : PDouble; const qrData : TRecMtxQRDecompData) : boolean;
var k : TASMNativeInt;
    i : TASMNativeInt;
    pA : PDouble;
    pTau : PDouble;
    aii : double;
    pAlpha : PDouble;
    lineRes : boolean;
    pC : PDouble;
begin
     Result := true;
     k := min(width, height);

     pTau := Tau;
     for i := 0 to k - 1 do
     begin
          // Generate elemetary reflector H(i) to annihilate A(1+i:height-1,i);
          pAlpha := GenPtr(A, i, i, LineWidthA);
          pA := GenPtr(A, i, min(i + 1, height - 1), LineWidthA);
          
          lineRes := GenElemHousholderRefl(pA, LineWidthA, height - i, pAlpha^, pTau);
          Result := Result and lineRes;

          // Apply H(i) to A(i:m,i+1:n) from the left
          if i < width - 1 then
          begin
               aii := pAlpha^;
               pAlpha^ := 1;
               pC := pAlpha;
               inc(pC);

               ApplyElemHousholderReflLeft(pAlpha, LineWidthA, pC, LineWidthA, width - i - 1, height - i, pTau, qrData.work);

               pAlpha^ := aii;
          end;

          if Assigned(qrData.Progress) then
             qrData.Progress((qrData.actIdx + i)*100 div qrData.qrWidth);

          inc(pTau);
     end;
end;

// original DLARFT in Lapack - forward columnwise
procedure CreateTMtx(n, k : TASMNativeInt; A : PDouble; LineWidthA : TASMNativeInt; Tau : PDouble; const qrData : TRecMtxQRDecompData);
var i, j : TASMNativeInt;
    pT, pA : PDouble;
    pA1 : PDouble;
    pDest : PDouble;
    pT1 : PConstDoublearr;
    pT2 : PDouble;
    x, y : TASMNativeInt;
    tmp : Double;
    pMem : PDouble;
    pResVec : PDouble;
    T : PDouble;
    pAij : PDouble;
    pcAIJ : PConstDoubleArr;
begin
     assert(k <= n, 'Error k needs to be smaller than n');

     // it is assumed that mem is a memory block of at least (n, k) where the first (k, k) elements are reserved for T
     T := qrData.work;

     for i := 0 to k - 1 do
     begin
          if Tau^ = 0 then
          begin
               // H = I
               pT := T;
               for j := 0 to i do
               begin
                    pT^ := 0;
                    inc(PByte(pT), qrData.LineWidthWork);
               end;
          end
          else
          begin
               // general case

               // T(1:i-1,i) := - tau(i) * A(i:j,1:i-1)**T * A(i:j,i)          **T = Transposed

               // T(1:i-1,i) := - tau(i) * V(i:n,1:i-1)' * V(i:n,i) */

               pT := GenPtr(T, i, 0, qrData.LineWidthWork );
               pcAIJ := PConstDoubleArr( GenPtr(A, 0, i, LineWidthA) );
               for j := 0 to i - 1 do
               begin
                    pT^ := -tau^*pcAij^[j];
                    inc(PByte(pT), qrData.LineWidthWork); 
               end;

               pA1 := GenPtr(A, 0, i + 1, LineWidthA);
               pAij := GenPtr(A, i, i + 1, LineWidthA);
               pT := GenPtr(T, i, 0, qrData.LineWidthWork );
               GenericMtxVecMultT(pT, qrData.LineWidthWork, pA1, pAij, LineWidthA, LineWidthA, i, n - i - 1, -tau^, 1 );
          end;

          // dtrmv: upper triangle matrix mult T(1:i-1,i) := T(1:i-1, 1:i-1)*T(1:i-1,i)
          if i > 0 then
          begin
               pDest := T;
               inc(pDest, i);
               for y := 0 to i - 1 do
               begin
                    pT1 := PConstdoubleArr(GenPtr(T, 0, y, qrData.LineWidthWork));
                    pT2 := pDest;
                    tmp := 0;
                    for x := y to i - 1 do
                    begin
                         tmp := tmp + pT1^[x]*pT2^;
                         inc(PByte(pT2), qrData.LineWidthWork);
                    end;

                    pDest^ := tmp;
                    inc(PByte(pDest), qrData.LineWidthWork);
               end;
          end;

          pT := GenPtr(T, i, i, qrData.LineWidthWork);
          pT^ := Tau^;  // fill in last Tau

          inc(Tau);
     end;
end;

// dlarft forward rowwise
procedure CreateTMtxR(n, k : TASMNativeInt; A : PDouble; LineWidthA : TASMNativeInt; Tau : PDouble; const qrData : TRecMtxQRDecompData);
var i, j : TASMNativeInt;
    pT, pA : PDouble;
    pA1 : PDouble;
    pDest : PDouble;
    pT1 : PConstdoubleArr;
    pt2 : PDouble;
    x, y : TASMNativeInt;
    tmp : Double;
    pMem : PDouble;
    pResVec : PDouble;
    T : PDouble;
    pAij : PDouble;
    pcAIJ : PConstDoubleArr;
    pA1i1 : PDouble;
    pAii1 : PDouble;
begin
     assert(k <= n, 'Error k needs to be smaller than n');

     // it is assumed that mem is a memory block of at least (n, k) where the first (k, k) elements are reserved for T
     T := qrData.work;    // kxk
     pMem := GenPtr(T, 0, k, qrData.LineWidthWork);

     for i := 0 to k - 1 do
     begin
          if Tau^ = 0 then
          begin
               // H = I
               pT := T;
               for j := 0 to i do
               begin
                    pT^ := 0;
                    inc(PByte(pT), qrData.LineWidthWork);
               end;
          end
          else
          begin
               // general case

               // T(1:i-1,i) := - tau(i) * V(1:i-1,i:j) * V(i,i:j)**T
               pT := GenPtr(T, i, 0, qrData.LineWidthWork );
               pAij := GenPtr(A, i, 0, LineWidthA);
               for j := 0 to i - 1 do
               begin
                    pT^ := -tau^*pAij^;
                    inc(PByte(pT), qrData.LineWidthWork);
                    inc(PByte(pAij), LineWidthA);
               end;

               pA1 := GenPtr(A, i + 1, 0, LineWidthA);
               pAij := GenPtr(A, i + 1, i, LineWidthA);
               pT := GenPtr(T, i, 0, qrData.LineWidthWork );
               MatrixMtxVecMult(pT, qrData.LineWidthWork, pA1, pAij, LineWidthA, sizeof(double), n - i - 1, i, -tau^, 1 );
          end;

          if i > 0 then
          begin
               pDest := T;
               inc(pDest, i);
               for y := 0 to i - 1 do
               begin
                    pT1 := PConstdoubleArr(GenPtr(T, 0, y, qrData.LineWidthWork));
                    pT2 := pDest;
                    tmp := 0;
                    for x := y to i - 1 do
                    begin
                         tmp := tmp + pT1^[x]*pT2^;
                         inc(PByte(pT2), qrData.LineWidthWork);
                    end;

                    pDest^ := tmp;
                    inc(PByte(pDest), qrData.LineWidthWork);
               end;
          end;

          pT := GenPtr(T, i, i, qrData.LineWidthWork);
          pT^ := Tau^;  // fill in last Tau

          inc(Tau);
     end;
end;

procedure MatrixSubT(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; LineWidthB : TASMNativeInt; width, height : TASMNativeInt);
var x, y : TASMNativeInt;
    pA : PConstDoubleArr;
    pB : PDouble;
begin
     // calculate A = A - B'
     for y := 0 to height - 1 do
     begin
          pA := PConstDoubleArr( A );
          pB := B;
          for x := 0 to width - 1 do
          begin
               pA^[x] := pA^[x] - pB^;
               inc(PByte(pB), LineWidthB);
          end;

          inc(PByte(A), LineWidthA);
          inc(B);
     end;
end;

// apply block reflector to a matrix
// original DLARFB in Lapack - Left, Transpose, Forward, Columnwise
procedure ApplyBlockReflector(A : PDouble; LineWidthA : TASMNativeInt; const qrData : TRecMtxQRDecompData;
  width, height : TASMNativeInt; widthT : TASMNativeInt; Transposed : boolean);
var pC1, pC2 : PDouble;
    pY1, pY2 : PDouble;
    T : PDouble;
    LineWidthT : TASMNativeInt;
    mem : PDouble;
    LineWidthMem : TASMNativeInt;
    heightW : TASMNativeInt;
begin
     mem := qrData.work;
     inc(PByte(mem), widthT*qrData.LineWidthWork);  // upper part (nb x nb) is dedicated to T, lower part to W in dlarfb
     LineWidthMem := qrData.LineWidthWork;

     T := qrData.work;
     LineWidthT := qrData.LineWidthWork;

     // (I - Y*T*Y')xA_trailing
     pY1 := A;
     pY2 := A;
     inc(PByte(pY2), widthT*LineWidthA);
     pC1 := A;
     inc(pC1, widthT);
     pC2 := pC1;
     inc(PByte(pC2), widthT*LineWidthA);

     // W = C1'*Y1
     MatrixMultTria2T1(mem, LineWidthMem, pC1, LineWidthA, pY1, LineWidthA, width - widthT, widthT, widthT, widthT);
     heightW := width - widthT;

     // W = W + C2'*Y2
     qrData.MatrixMultT1(mem, LineWidthMem, pC2, pY2, Width - WidthT, height - widthT, WidthT, height - widthT, LineWidthA, LineWidthA, QRMultBlockSize, doAdd, qrData.BlkMultMem);

     // W = W * T (using dtrm)  or W = W*T'
     if height > widthT then
     begin
          if transposed
          then
              GenericMtxMultTria2T1StoreT1(mem, LineWidthMem, T, LineWidthT, widthT, heightW, WidthT, WidthT)
          else
              GenericMtxMultTria2Store1(mem, LineWidthMem, T, LineWidthT, widthT, heightW, WidthT, WidthT);

          // C2 = C2 - Y2*W'
          qrData.MatrixMultT2(pC2, LineWidthA, pY2, mem, widthT, height - widthT, widthT, heightW, 
                              LineWidthA, LineWidthMem, QRMultBlockSize, doSub, qrData.BlkMultMem);

          // W = W*Y1' (lower left part of Y1! -> V1)
          GenericMtxMultLowTria2T2Store1(mem, LineWidthMem, A, LineWidthA, WidthT, heightW, widthT, widthT);
     end;

     // C1 = C1 - W'
     MatrixSubT(pC1, LineWidthA, mem, LineWidthMem, width - widthT, widthT);
end;

// dlarfb 'Right', 'Transpose', 'Forward', 'Rowwise'
procedure ApplyBlockReflectorRTFR(A : PDouble; LineWidthA : TASMNativeInt; const qrData : TRecMtxQRDecompData;
  width, height : TASMNativeInt; widthT : TASMNativeInt; Transposed : boolean);
var pC1, pC2 : PDouble;
    pV1, pV2 : PDouble;
    T : PDouble;
    LineWidthT : TASMNativeInt;
    mem : PDouble;
    LineWidthMem : TASMNativeInt;
    W : PDouble;
    heightC1, widthC1 : TASMNativeInt;
    widthC2, heightC2 : TASMNativeInt;

    widthV1, heightV1 : TASMNativeInt;
    widthV2, heightV2 : TASMNativeInt;

    widthW, heightW : TASMNativeInt;
begin
     // it is assumed that height contains the full height of the input matrix A
     // we subsect it in this routine
     widthC1 := widthT;
     heightC1 := height - widthT;
     widthC2 := width - widthT;
     heightC2 := height - widthT;

     widthV1 := widthT;
     heightV1 := widthT;
     widthV2 := width - widthT;
     heightV2 := widthT;

     widthW := widthT;
     heightW := height - widthT;

     // W := C * V**T  =  (C1*V1**T + C2*V2**T)  (stored in WORK)
     mem := qrData.work;
     inc(PByte(mem), widthT*qrData.LineWidthWork);  // upper part (nb x nb) is dedicated to T, lower part to W in dlarfb
     LineWidthMem := qrData.LineWidthWork; // widthT*sizeof(double);

     T := qrData.work;
     LineWidthT := qrData.LineWidthWork;
     
     pC1 := GenPtr(A, 0, widthT, LineWidthA);
     pC2 := pC1;
     inc(pC2, widthT);
     
     pV1 := A; // GenPtr(A, 0, 0, LineWidthA);
     pV2 := GenPtr(pV1, widthT, 0, LineWidthA);

     // W := W * V1**T  // dtrm right upper transpose unit combined with copy
     W := mem;
     GenericMtxMultTria2TUpper(W, LineWidthMem, pC1, LineWidthA, pV1, LineWidthA, widthC1, heightC1, widthV1, heightV1);

     if width > widthT then
     begin
          //  W := W + C2 * V2**T
          qrData.MatrixMultT2(W, LineWidthMem, pC2, pV2, 
                              widthC2, heightC2, widthV2, heightV2,
                              LineWidthA, LineWidthA, QRMultBlockSize, doAdd, qrData.BlkMultMem);
     end;

     // W := W * T  or  W * T**T
     if transposed
     then
         GenericMtxMultTria2T1StoreT1(W, LineWidthMem, T, LineWidthT, widthW, heightW, WidthT, WidthT)
     else
         GenericMtxMultTria2Store1(W, LineWidthMem, T, LineWidthT, widthW, heightW, WidthT, WidthT);
     
     // C2 := C2 - W * V2
     if width > widthT then
        MatrixMultEx(pC2, LineWidthA, W, pV2, widthW, heightW, widthV2, heightV2, LineWidthMem, 
                     LineWidthA, QRMultBlockSize, doSub, qrData.BlkMultMem); 

     // W := W * V1
     GenericMtxMultTria2Store1Unit(W, LineWidthMem, pV1, LineWidthA, widthW, heightW, widthV1, heightV1);

     // C1 := C1 - W
     MatrixSub(pC1, LineWidthA, pC1, W, widthC1, heightC1, LineWidthA, LineWidthMem);
end;

function InternalMatrixQRDecompInPlace2(A : PDouble; const LineWidthA : TASMNativeInt; width, height : TASMNativeInt; tau : PDouble; qrData : TRecMtxQRDecompData) : boolean;
var k : TASMNativeInt;
    idx : TASMNativeInt;
    ib : TASMNativeInt;
    pA : PDouble;
    pnlRes : boolean;
begin
     // ######################################################
     // #### original linpack DGEQRF routine
     k := Min(width, height);

     // check if the panel size fits the width/heigth -> if not adjust the panel size
     if (qrdata.pnlSize > 1) and (qrdata.pnlSize > k) then
        qrdata.pnlSize := max(2, k div 2);

     Result := True;
     qrData.LineWidthWork := sizeof(double)*qrdata.pnlSize;
     idx := 0;
     pA := A;

     if (qrdata.pnlSize >= 2) and (qrdata.pnlSize < k) then
     begin
          // calculate the qr decomposition for the current panel
          while idx < k - qrdata.pnlSize - 2 do
          begin
               ib := min(width - idx, qrdata.pnlSize);

               pnlRes := MtxQRUnblocked(pA, LineWidthA, ib, height - idx, Tau, qrData);
               Result := Result and pnlRes;

               // calculate T matrix
               if idx + ib <= height then
               begin
                    CreateTMtx(height - idx, ib, pA, LineWidthA, Tau, qrData);

                    // apply H to A from the left
                    ApplyBlockReflector(pA, LineWidthA, qrData, width - idx, height - idx, ib, False);
               end;

               inc(qrData.actIdx, ib);

               inc(pA, ib);
               inc(PByte(pA),ib*LineWidthA);
               inc(idx, ib);
               inc(Tau, ib);
          end;
     end;

     // calculate the last panel
     pnlRes := MtxQRUnblocked(pA, LineWidthA, width - idx, height - idx, Tau, qrData);
     Result := Result and pnlRes;
end;

function QRDecompMemSize( pnlSize, width : TASMNativeInt) : TASMNativeInt;
begin
     Result := pnlSize*sizeof(double)*width + 64 + BlockMultMemSize(QRMultBlockSize);
end;

function MatrixQRDecompInPlace2(A : PDouble; const LineWidthA : TASMNativeInt; width, height : TASMNativeInt; tau : PDouble; progress : TLinEquProgress = nil) : TQRResult; overload;
begin
     Result := MatrixQRDecompInPlace2(A, LineWidthA, width, height, tau, nil, QRBlockSize, Progress);
end;

function MatrixQRDecompInPlace2(A : PDouble; const LineWidthA : TASMNativeInt; width, height : TASMNativeInt; tau : PDouble; work : PDouble; pnlSize : TASMNativeInt; progress : TLinEquProgress = nil) : TQRResult; overload;
var res : boolean;
    qrData : TRecMtxQRDecompData;
begin
     qrData.pWorkMem := nil;
     qrData.work := work;
     qrData.BlkMultMem := nil;
     qrData.Progress := progress;
     qrData.qrWidth := width;
     qrData.qrHeight := height;
     qrData.actIdx := 0;
     qrData.pnlSize := pnlSize;
     qrData.MatrixMultT1 := {$IFDEF FPC}@{$ENDIF}MatrixMultT1Ex;
     qrData.MatrixMultT2 := {$IFDEF FPC}@{$ENDIF}MatrixMultT2Ex;

     if work = nil then
     begin
          qrData.pWorkMem := GetMemory( QRDecompMemSize(pnlSize, width) );
          qrData.work := PDouble(qrData.pWorkMem);
          if (NativeUInt(qrData.pWorkMem) and $0000000F) <> 0 then
             qrData.work := PDouble(NativeUInt(qrData.pWorkMem) + 16 - NativeUInt(qrData.pWorkMem) and $0F);
     end;

     // it's assumed that the work memory block may also be used
     // as blocked multiplication memory storage!
     qrData.BlkMultMem := qrData.work;
     inc(qrData.BlkMultMem, pnlSize*height);

     res := InternalMatrixQRDecompInPlace2(A, LineWidthA, width, height, tau, qrData);

     if work = nil then
        FreeMem(qrData.pWorkMem);

     if res
     then
         Result := qrOK
     else
         Result := qrSingular;
end;

// generates an m by n real matrix Q with orthonormal columns,
//  which is defined as the first n columns of a product of k elementary
//  reflectors of order m
// k: the number of elemetary reflectors whose product defines the matrix Q. width >= K >= 0.
// original dorg2r from netlib
procedure InternalMatrixQFromQRDecomp(A : PDouble; const LineWidthA : TASMNativeInt; width, height, k : TASMNativeInt; tau : PDouble; const qrData : TRecMtxQRDecompData);
var pA : PDouble;
    pAii : PDouble;
    i, j : TASMNativeInt;
    pAii1 : PDouble;
begin
     if width <= 0 then
        exit;

     // Initialise columns k+1:n to columns of the unit matrix
     for j := k to width - 1 do
     begin
          pA := A;
          inc(pA, j);

          for i := 0 to height - 1 do
          begin
               pA^ := 0;
               inc(PByte(pA), LineWidthA);
          end;

          pA := A;
          inc(pA, j);
          inc(PByte(pA), j*LineWidthA);
          pA^ := 1;
     end;

     // Apply H(i) to A(i:m,i:n) from the left
     inc(tau, k - 1);
     for i := k - 1 downto 0 do
     begin
          if i < width - 1 then
          begin
               pAii := GenPtr(A, i, i, LineWidthA);
               pAii1 := GenPtr(A, i + 1, i, LineWidthA);

               pAii^ := 1;
               ApplyElemHousholderReflLeft(pAii, LineWidthA, pAii1, LineWidthA, width - i - 1, height - i, tau, qrData.work);
          end;

          // unscaling
          if i < height - 1 then
          begin
               pAii := A;
               inc(pAii, i);
               inc(PByte(pAii), (i+1)*LineWidthA);

               MatrixScaleAndAdd(pAii, LineWidthA, 1, height - i - 1, 0, -tau^);
          end;

          pAii := A;
          inc(pAii, i);
          inc(PByte(pAii), i*LineWidthA);
          pAii^ := 1 - tau^;

          // Set A(1:i-1,i) to zero
          pA := A;
          inc(pA, i);
          for j := 0 to i - 1 do
          begin
               pA^ := 0;
               inc(PByte(pA), LineWidthA);
          end;

          dec(tau);

          // ###########################################
          // #### Progress
          if Assigned(qrData.Progress) then
             qrData.Progress((qrData.actIdx + i)*100 div qrData.qrWidth);
     end;
end;

procedure InternalBlkMatrixQFromQRDecomp(A : PDouble; const LineWidthA : TASMNativeInt; width, height : TASMNativeInt; tau : PDouble; qrData : TRecMtxQRDecompData);
var pA : PDouble;
    x, y : TASMNativeInt;
    numIter : TASMNativeInt;
    pTau : PDouble;
    counter: TASMNativeInt;
    idx : TASMNativeInt;
begin
     // check for quick return 
     if width <= 0 then
     begin
          qrData.work^ := 1;
          exit;
     end;

     // restrict block size...
     qrData.pnlSize := min(height, min(width, qrData.pnlSize));

     // zero out block not used by Q
     if height < width then
     begin
          for y := 0 to height - 1 do
          begin
               pA := A;
               inc(pA, height);
               inc(PByte(pA), y*LineWidthA);

               for x := height to width - 1 do
               begin
                    pA^ := 0;
                    inc(pA);
               end;
          end;

          // from here on we can do a normal q factorization
          width := height;
     end;

     if qrData.pnlSize = width then
     begin
          // unblocked code
          InternalMatrixQFromQRDecomp(A, LineWidthA, width, height, width, tau, qrData);
     end
     else
     begin
          numIter := (width div qrData.pnlSize);
          // apply unblocked code to the last block
          if numIter*qrData.pnlSize < width then
          begin
               idx := numIter*qrData.pnlSize;
               pA := GenPtr(A, idx, idx, LineWidthA);
               pTau := tau;
               inc(pTau, idx);
               InternalMatrixQFromQRDecomp(pA, LineWidthA, width - idx, height - idx, width - idx, pTau, qrData);

               // Set upper rows of current block to zero
               for y := 0 to idx - 1 do
               begin
                    pA := GenPtr(A, idx, y, LineWidthA);
                    for x := 0 to width - idx - 1 do
                    begin
                         pA^ := 0;
                         inc(pA);
                    end;
               end;
          end;

          // blocked code:
          for counter := numIter - 1 downto 0 do
          begin
               idx := counter*qrdata.pnlSize;
               pA := GenPtr(A, idx, idx, LineWidthA);
               pTau := tau;
               inc(pTau, idx);

               if idx + qrdata.pnlSize < width then
               begin
                    // form triangular factor of the block reflector
                    // H = H(i) H(i+1) . . . H(i+ib-1)
                    // calculate T matrix
                    CreateTMtx(height - idx, qrData.pnlSize, pA, LineWidthA, pTau, qrData);
                    // apply H to A from the left
                    ApplyBlockReflector(pA, LineWidthA, qrData, width - idx, height - idx, qrdata.pnlSize, True);
               end;

               InternalMatrixQFromQRDecomp(pA, LineWidthA, qrData.pnlSize, height - idx, qrData.pnlSize, pTau, qrData);

               // Set upper rows of current block to zero
               for y := 0 to idx - 1 do
               begin
                    pA := GenPtr(A, idx, y, LineWidthA);
                    for x := 0 to qrData.pnlSize - 1 do
                    begin
                         pA^ := 0;
                         inc(pA);
                    end;
               end;
          end;
     end;
end;

procedure MatrixQFromQRDecomp(A : PDouble; const LineWidthA : TASMNativeInt; width, height : TASMNativeInt; 
  tau : PDouble; progress : TLinEquProgress = nil); overload;
begin
     MatrixQFromQRDecomp(A, LineWidthA, width, height, tau, QRBlockSize, nil, progress);
end;

procedure MatrixQFromQRDecomp(A : PDouble; const LineWidthA : TASMNativeInt; width, height : TASMNativeInt; 
 tau : PDouble; BlockSize : TASMNativeInt; work : PDouble; progress : TLinEquProgress = nil);
var qrData : TRecMtxQRDecompData;
begin
     qrData.pWorkMem := nil;
     qrData.work := work;
     qrData.BlkMultMem := nil;
     qrData.Progress := progress;
     qrData.qrWidth := width;
     qrData.qrHeight := height;
     qrData.actIdx := 0;
     qrData.pnlSize := BlockSize;
     qrData.LineWidthWork := sizeof(double)*qrdata.pnlSize;
     qrData.MatrixMultT1 := {$IFDEF FPC}@{$ENDIF}MatrixMultT1Ex;
     qrData.MatrixMultT2 := {$IFDEF FPC}@{$ENDIF}MatrixMultT2Ex;

     if work = nil then
     begin
          qrData.pWorkMem := GetMemory( QRDecompMemSize(qrData.pnlSize, width) );
          qrData.work := PDouble(qrData.pWorkMem);
          if (NativeUInt(qrData.pWorkMem) and $0000000F) <> 0 then
             qrData.work := PDouble(NativeUInt(qrData.pWorkMem) + 16 - NativeUInt(qrData.pWorkMem) and $0F);
     end;

     // it's assumed that the work memory block may also be used
     // as blocked multiplication memory storage!
     qrData.BlkMultMem := qrData.work;
     inc(qrData.BlkMultMem, qrData.pnlSize*height);

     InternalBlkMatrixQFromQRDecomp(A, LineWidthA, width, height, tau, qrData);
     if not Assigned(work) then
        freeMem(qrData.pWorkMem);
end;

// ###########################################
// #### SVD implementation
// ###########################################

// generates an m by n real matrix Q with orthonormal columns,
//  which is defined as the first n columns of a product of k elementary
//  reflectors of order m
// k: the number of elemetary reflectors whose product defines the matrix Q. width >= K >= 0.
// original dorgl2 from netlib
procedure InternalMatrixQLeftFromQRDecomp(A : PDouble; const LineWidthA : TASMNativeInt; width, height, k : TASMNativeInt; tau : PDouble; const qrData : TRecMtxQRDecompData);
var pA : PDouble;
    pAii : PDouble;
    pC : PDouble;
    i, l, j : TASMNativeInt;
begin
     if height <= 0 then
        exit;

     if k < height then
     begin
          // Initialize rows k+1:m to rows of the unit matrix
          for j := 0 to width - 1 do
          begin
               pA := GenPtr(A, j, k, LineWidthA);

               for l := k to height - 1 do
               begin
                    pA^ := 0;
                    inc(PByte(pA), LineWidthA);
               end;

               if (j > k) and (j < height) then
               begin
                    pA := GenPtr(A, j, j, LineWidthA);
                    pA^ := 1;
               end;
          end;
     end;

     // Apply H(i) to A(i:m,i:n) from the right
     inc(tau, k - 1);
     for i := k - 1 downto 0 do
     begin
          if i < width - 1 then
          begin
               if i < height - 1 then
               begin
                    pAii := GenPtr(A, i, i, LineWidthA);
                    pC := pAii;
                    inc(PByte(pC), LineWidthA);

                    pAii^ := 1;
                    ApplyElemHousholderReflRight(pAii, sizeof(double), pC, LineWidthA, width - i, height - i - 1, tau, qrData.work);
               end;

               pAii := GenPtr(A, i + 1, i, LineWidthA);
               MatrixScaleAndAdd(pAii, LineWidthA, width - i - 1, 1, 0, -tau^);
          end;

          pAii := GenPtr(A, i, i, LineWidthA);
          pAii^ := 1 - tau^;

          // Set A(i,1:i-1) to zero
          pA := GenPtr(A, 0, i, LineWidthA);
          for l := 0 to i - 1 do
          begin
               pA^ := 0;
               inc(pA);
          end;

          dec(tau);

          // ###########################################
          // #### Progress
          if Assigned(qrData.Progress) then
             qrData.Progress((qrData.actIdx + i)*100 div qrData.qrWidth);
     end;
end;

// dorglq.f
procedure InternalBlkMatrixLeftQFromQRDecomp(A : PDouble; const LineWidthA : TASMNativeInt; width, height : TASMNativeInt; tau : PDouble; qrData : TRecMtxQRDecompData);
var pA : PDouble;
    x, y : TASMNativeInt;
    numIter : TASMNativeInt;
    pTau : PDouble;
    counter: TASMNativeInt;
    idx : TASMNativeInt;
begin
     // check for quick return ???
     if height <= 0 then
     begin
          qrData.work^ := 1;
          exit;
     end;

     // restrict block size...
     qrData.pnlSize := min(height, min(width, qrData.pnlSize));

     // zero out block not used by Q
     // note this part is never executed in standard svd
     //if height > width then
//     begin
//          for y := width to height - 1 do
//          begin
//               pCA := PConstDoubleArr( GenPtr(A, 0, y, LineWidthA) );
//
//               for x := 0 to width - 1 do
//                   pCA^[x] := 0;
//          end;
//
//          // from here on we can do a normal q factorization
//          height := width;
//     end;

     if qrData.pnlSize = height then
     begin
          // unblocked code
          InternalMatrixQLeftFromQRDecomp(A, LineWidthA, width, height, width, tau, qrData);
     end
     else
     begin
          numIter := (height div qrData.pnlSize);

          // apply blocked code to the last block
          if numIter*qrData.pnlSize < height then
          begin
               idx := numIter*qrData.pnlSize;
               
               // set Set A(idx+1:m,1:idx) to zero.
               for x := 0 to idx - 1 do
               begin
                    pA := GenPtr(A, x, idx, LineWidthA);
                    for y := 0 to height - idx - 1 do
                    begin
                         pA^ := 0;
                         inc(PByte(pA), LineWidthA);
                    end;
               end;
               
               // unblocked code
               pA := GenPtr(A, idx, idx, LineWidthA);
               pTau := tau;
               inc(pTau, idx);
               InternalMatrixQLeftFromQRDecomp(pA, LineWidthA, width - idx, height - idx, height - idx, pTau, qrData);
          end;

          // blocked code:
          for counter := numIter - 1 downto 0 do
          begin
               idx := counter*qrdata.pnlSize;
               pA := GenPtr(A, idx, idx, LineWidthA);
               pTau := tau;
               inc(pTau, idx);

               if idx + qrdata.pnlSize < height then
               begin
                    // form triangular factor of the block reflector
                    // H = H(i) H(i+1) . . . H(i+ib-1)
                    // calculate T matrix
                    CreateTMtxR(width - idx, qrData.pnlSize, pA, LineWidthA, pTau, qrData);
                    // apply H to A from the left
                    ApplyBlockReflectorRTFR(pA, LineWidthA, qrData, width - idx, height - idx, qrdata.pnlSize, True);
               end;

               InternalMatrixQLeftFromQRDecomp(pA, LineWidthA, width - idx, qrData.pnlSize, qrData.pnlSize, pTau, qrData);

               // Set columns of 1:i-1 of current block to zero
               for x := 0 to idx - 1 do
               begin
                    pA := GenPtr(A, x, idx, LineWidthA);
                    for y := 0 to qrData.pnlSize - 1 do
                    begin
                         pA^ := 0;
                         inc(PByte(pA), LineWidthA);
                    end;
               end;
          end;
     end;
end;


// work needs to be at least Max(width, height)
// the other arrays need to be at least min(width, height)
procedure dgebd2(A : PDouble; const LineWidthA : TASMNativeInt; Width, Height : TASMNativeInt; 
                 D, E, TauQ, TauP : PConstDoubleArr; work : PDouble);
var i : TASMNativeInt;
    pAii : PDouble;
    pAmin : PDouble;
    pC : PDouble;
begin
     if (width <= 0) or (height <= 0) then
        exit;
        
     if height >= width then
     begin
          // Reduce to upper bidiagonal form
          
          for i := 0 to width - 1 do
          begin
               pAii := GenPtr(A, i, i, LineWidthA);
               // Generate elementary reflector H(i) to annihilate A(i+1:m,i)
               pAmin := GenPtr(A, i, min( i + 1, height - 1), LineWidthA);
               GenElemHousholderRefl(pAmin, LineWidthA, height - i, pAii^, @tauq^[i]);

               d[i] := pAii^;
               pAii^ := 1;

               // Apply H(i) to A(i:m,i+1:n) from the left
               if i < width - 1 then
               begin
                    pC := pAii;
                    inc(pC);
                    ApplyElemHousholderReflLeft( pAii, LineWidthA, pC, LineWidthA, width - i - 1, height - i, @tauq[i], work);
               end;

               pAii^ := d[i];

               if i < width - 1 then
               begin
                    // Generate elementary reflector G(i) to annihilate A(i,i+2:n)
                    pAmin := GenPtr(A, min( i + 2, width - 1), i, LineWidthA);
                    inc(pAii);
                    GenElemHousholderRefl(pAmin, sizeof(double), width - i - 1, pAii^, @taup[i]);
                    e[i]:= pAii^;
                    pAii^ := 1;

                    // Apply G(i) to A(i+1:m,i+1:n) from the right
                    pC := pAii;
                    inc(PByte(pC), LineWidthA);
                    ApplyElemHousholderReflRight(pAii, sizeof(double), pC, LineWidthA,
                                                 width - i - 1, height - i - 1, @taup[i], work);
                    pAii^ := e[i];
               end
               else
                   taup[i] := 0;
          end;
     end
     else
     begin
          // Reduce to lower bidiagonal form
          for i := 0 to height - 1 do
          begin
               pAii := GenPtr(A, i, i, LineWidthA);
               
               pAmin := GenPtr(A, min( i + 1, width - 1), i, LineWidthA);
               GenElemHousholderRefl(pAmin, sizeof(double), width - i, pAii^, @taup^[i]);

               d[i] := pAii^;
               pAii^ := 1;

               // Apply G(i) to A(i+1:m,i:n) from the right
               if i < height - 1 then
               begin
                    pC := pAii;
                    inc(PByte(pC), LineWidthA);
                    ApplyElemHousholderReflRight(pAii, sizeof(double), pC, LineWidthA, width - i, height - i - 1, @taup[i], work);
               end;

               pAii^ := d[i];

               if i < height - 1 then
               begin
                    inc(PByte(pAii), LineWidthA);
                    
                    // Generate elementary reflector H(i) to annihilate
                    // A(i+2:m,i)
                    pAmin := GenPtr(A, i, min(i + 2, Height - 1), LineWidthA);
                    inc(PByte(pAii), LineWidthA);
                    
                    GenElemHousholderRefl(pAmin, LineWidthA, height - i - 1, pAii^, @tauq[i]);
                    e[i]:= pAii^;
                    pAii^ := 1;

                    // Apply H(i) to A(i+1:m,i+1:n) from the left
                    pC := pAii;
                    inc(pC);
                    
                    inc(pC);
                    ApplyElemHousholderReflLeft(pAii, LineWidthA, pC, LineWidthA, width - i - 1, height - i - 1, @tauq[i], work);
                    pAii^ := e[i];
               end
               else
                   tauq[i] := 0;
          end;
     end;
end;


// reduce parts of A to a bidiagonal form
procedure dlabrd(A : PDouble; const LineWidthA : TASMNativeInt; width, height : TASMNativeInt; nb : TASMNativeInt;
                 D, E, TauQ, TauP : PConstDoubleArr; X : PDouble; const LineWidthX : TASMNativeInt;
                 Y : PDouble; const LineWidthY : TASMNativeInt);
var i : TASMNativeInt;
    pAii00 : PDouble;                    // first index y, second x, first 1 -> +1 height, second 1 +1 width
    pAii01 : PDouble;
    pAii10 : PDouble;
    pAii11 : PDouble;
    pA0i01, pAi010 : PDouble;
    pAi000, pA0i00 : PDouble;

    pYi000, pY0i00 : PDouble;
    pYii10 : PDouble;
    pYi010 : PDouble;
    pXi000, pX0i00 : PDouble;
    pXi010 : PDouble;
    pXii10 : PDouble;
    pAminIM : PDouble;
begin
     if (width <= 0) or (Height <= 0) then
        exit;

     if height >= Width then
     begin
          for i := 0 to nb - 1 do
          begin
               // init variables
               pAii00 := GenPtr(A, i, i, LineWidthA);
               pAii01 := GenPtr(A, i + 1, i, LineWidthA);
               pA0i01 := GenPtr(A, i + 1, 0, LineWidthA);
               pAii11 := GenPtr(A, i + 1, i + 1, LineWidthA);
               pAi010 := GenPtr(A, 0, i + 1, LineWidthA);
               pAi000 := GenPtr(A, 0, i, LineWidthA);
               pA0i00 := GenPtr(A, i, 0, LineWidthA);
               
               pYi000 := GenPtr(Y, 0, i, LineWidthY);
               pY0i00 := GenPtr(Y, i, 0, LineWidthY);
               pYii10 := GenPtr(Y, i, i + 1, LineWidthY);
               pYi010 := GenPtr(Y, 0, i + 1, LineWidthY);
               pXi000 := GenPtr(X, 0, i, LineWidthX); 
               pXi010 := GenPtr(X, 0, i + 1, LineWidthX); 
               pXii10 := GenPtr(X, i, i + 1, LineWidthX);
               pX0i00 := GenPtr(X, i, 0, LineWidthX);
               
               // Update A(i:m,i)
               // CALL dgemv( 'No transpose', m-i+1, i-1, -one, a( i, 1 ),
               //             lda, y( i, 1 ), ldy, one, a( i, i ), 1 )
               MatrixMtxVecMult(pAii00, LineWidthA, pAi000, pYi000, LineWidthA, sizeof(double), i, height - i, -1, 1);
               //  CALL dgemv( 'No transpose', m-i+1, i-1, -one, x( i, 1 ),
               //              ldx, a( 1, i ), 1, one, a( i, i ), 1 )
               MatrixMtxVecMult(pAii00, LineWidthA, pXi000, pA0i00, LineWidthX, LineWidthA, i, height - i, -1, 1);

               // generate reflection Q(i) to annihilate A(i+1:m,i)
               pAminIM := GenPtr(A, i, Min(i + 1, Height - 1), LineWidthA);
               GenElemHousholderRefl(pAminIM, LineWidthA, height - i, pAii00^, @tauQ^[i]);

               d[i] := pAii00^;
               
               if i < width - 1 then
               begin
                    pAii00^ := 1;

                    // Compute Y(i+1:n, i)

                    // CALL dgemv( 'Transpose', m-i+1, n-i, one, a( i, i+1 ),
                    //             lda, a( i, i ), 1, zero, y( i+1, i ), 1 )
                    MatrixMtxVecMultT(pYii10, LineWidthY, pAii01, pAii00, LineWidthA, LineWidthA, width - i - 1, height - i, 1, 0);
                    // CALL dgemv( 'Transpose', m-i+1, i-1, one, a( i, 1 ), lda,
                    //              a( i, i ), 1, zero, y( 1, i ), 1 )
                    MatrixMtxVecMultT(pY0i00, LineWidthY, pAi000, pAii00, LineWidthA, LineWidthA, i, height - i, 1, 0);
                    // CALL dgemv( 'No transpose', n-i, i-1, -one, y( i+1, 1 ),
                    //             ldy, y( 1, i ), 1, one, y( i+1, i ), 1 )
                    MatrixMtxVecMult(pYii10, LineWidthY, pYi010, pY0i00, LineWidthY, LineWidthY, i, width - i - 1, -1, 1);
                    // CALL dgemv( 'Transpose', m-i+1, i-1, one, x( i, 1 ), ldx,
                    //              a( i, i ), 1, zero, y( 1, i ), 1 )
                    MatrixMtxVecMultT(pY0i00, LineWidthY, pXi000, pAii00, LineWidthX, LineWidthA, i, height - i, 1, 0);
                    // CALL dgemv( 'Transpose', i-1, n-i, -one, a( 1, i+1 ),
                    //              lda, y( 1, i ), 1, one, y( i+1, i ), 1 )
                    MatrixMtxVecMultT(pYii10, LineWidthY, pA0i01, pY0i00, LineWidthA, LineWidthY, width - i - 1, i, -1, 1);

                    // CALL dscal( n-i, tauq( i ), y( i+1, i ), 1 )
                    MatrixScaleAndAdd(pYii10, LineWidthY, 1, Width - i - 1, 0, tauq[i]);

                    // update A(i, i+1:n)

                    // CALL dgemv( 'No transpose', n-i, i, -one, y( i+1, 1 ),
                    //              ldy, a( i, 1 ), lda, one, a( i, i+1 ), lda )
                    MatrixMtxVecMult(pAii01, sizeof(double), pYi010, pAi000, LineWidthY, sizeof(double), i + 1, width - i - 1, -1, 1);
                    // CALL dgemv( 'Transpose', i-1, n-i, -one, a( 1, i+1 ),
                    //             lda, x( i, 1 ), ldx, one, a( i, i+1 ), lda )
                    MatrixMtxVecMultT(pAii01, sizeof(double), pA0i01, pXi000, LineWidthA, sizeof(double), width - i - 1, i, -1, 1);

                    // Generate reflection P(i) to annihilate A(i,i+2:n)
                    pAminIM := GenPtr(A, min(i + 2, width - 1), i, LineWidthA);
                    GenElemHousholderRefl(pAminIM, sizeof(double), width - i - 1, pAii01^, @taup^[i]);

                    e[i] := pAii01^;
                    pAii01^ := 1;
                    
                    // Compute X(i+1:m,i)

                    //CALL dgemv( 'No transpose', m-i, n-i, one, a( i+1, i+1 ),
                    //            lda, a( i, i+1 ), lda, zero, x( i+1, i ), 1 )
                    MatrixMtxVecMult(pXii10, LineWidthX, pAii11, pAii01, LineWidthA, sizeof(double), width - i - 1, height - i - 1, 1, 0);
                    // CALL dgemv( 'Transpose', n-i, i, one, y( i+1, 1 ), ldy,
                    //              a( i, i+1 ), lda, zero, x( 1, i ), 1 )
                    MatrixMtxVecMultT(pX0i00, LineWidthX, pYi010, pAii01, LineWidthY, sizeof(double), i + 1, width - i - 1, 1, 0);
                    // CALL dgemv( 'No transpose', m-i, i, -one, a( i+1, 1 ),
                    //             da, x( 1, i ), 1, one, x( i+1, i ), 1 )
                    MatrixMtxVecMult(pXii10, LineWidthX, pAi010, pX0i00, LineWidthA, LineWidthX, i + 1, height - i - 1, -1, 1);
                    // CALL dgemv( 'No transpose', i-1, n-i, one, a( 1, i+1 ),
                    //             lda, a( i, i+1 ), lda, zero, x( 1, i ), 1 )
                    MatrixMtxVecMult(pX0i00, LineWidthX, pA0i01, pAii01, LineWidthA, sizeof(double), width - i - 1, i, 1, 0);
                    // CALL dgemv( 'No transpose', m-i, i-1, -one, x( i+1, 1 ),
                    //             ldx, x( 1, i ), 1, one, x( i+1, i ), 1 )
                    MatrixMtxVecMult(pXii10, LineWidthX, pXi010, pX0i00, LineWidthX, LineWidthX, i, height - i - 1, -1, 1);
                    // CALL dscal( m-i, taup( i ), x( i+1, i ), 1 )
                    MatrixScaleAndAdd(pXii10, LineWidthX, 1, height - i - 1, 0, taup[i]);   
               end;
          end;
     end
     else
     begin
          // Reduce to lower bidiagonal form
          for i := 0 to nb - 1 do
          begin
               // init variables
               pAii00 := GenPtr(A, i, i, LineWidthA);
               pAii10 := GenPtr(A, i, i + 1, LineWidthA);
               pA0i01 := GenPtr(A, i + 1, 0, LineWidthA);
               pAii11 := GenPtr(A, i + 1, i + 1, LineWidthA);
               pAi010 := GenPtr(A, 0, i + 1, LineWidthA);
               pAi000 := GenPtr(A, 0, i, LineWidthA);
               pA0i00 := GenPtr(A, i, 0, LineWidthA);
               
               pYi000 := GenPtr(Y, 0, i, LineWidthY);
               pY0i00 := GenPtr(Y, i, 0, LineWidthY);
               pYii10 := GenPtr(Y, i, i + 1, LineWidthY);
               pYi010 := GenPtr(Y, 0, i + 1, LineWidthY);
               pXi000 := GenPtr(X, 0, i, LineWidthX); 
               pXi010 := GenPtr(X, 0, i + 1, LineWidthX); 
               pXii10 := GenPtr(X, i, i + 1, LineWidthX);
               pX0i00 := GenPtr(X, i, 0, LineWidthX);
          
               // Update A(i,i:n)

                // CALL dgemv( 'No transpose', n-i+1, i-1, -one, y( i, 1 ),
                //              ldy, a( i, 1 ), lda, one, a( i, i ), lda )
               GenericMtxVecMult(pAii00, sizeof(double), pYi000, pAi000, LineWidthY, sizeof(double), i, width - i, -1, 1); 
                // CALL dgemv( 'Transpose', i-1, n-i+1, -one, a( 1, i ), lda,
                //              x( i, 1 ), ldx, one, a( i, i ), lda )
               GenericMtxVecMultT(pAii00, sizeof(double), pA0i00, pXi000, LineWidthA, sizeof(double), width - i, i, -1, 1);

               // Generate reflection P(i) to annihilate A(i,i+1:n)
               pAminIM := GenPtr(A, i, Min(i + 1, Width - 1), LineWidthA);
               GenElemHousholderRefl(pAminIM, sizeof(double), width - i, pAii00^, @tauP^[i]);
               
               d^[i] := pAii00^;

               if i < height - 1 then
               begin
                    pAii00^ := 1;
                    
                    // Compute X(i+1:m,i)

                    // CALL dgemv( 'No transpose', m-i, n-i+1, one, a( i+1, i ),
                    //              lda, a( i, i ), lda, zero, x( i+1, i ), 1 )
                    GenericMtxVecMult(pXii10, LineWidthX, pAii10, pAii00, LineWidthA, sizeof(double), width - i, height - i - 1, 1, 0);
                    // CALL dgemv( 'Transpose', n-i+1, i-1, one, y( i, 1 ), ldy,
                    //              a( i, i ), lda, zero, x( 1, i ), 1 )
                    GenericMtxVecMultT(pX0i00, LineWidthX, pYi000, pAii00, LineWidthY, sizeof(double), i, width - i, 1, 0);
                    // CALL dgemv( 'No transpose', m-i, i-1, -one, a( i+1, 1 ),
                    //              lda, x( 1, i ), 1, one, x( i+1, i ), 1 )
                    GenericMtxVecMult(pXii10, LineWidthX, pAi010, pX0i00, LineWidthA, LineWidthX, i, height - i - 1, -1, 1);
                    // CALL dgemv( 'No transpose', i-1, n-i+1, one, a( 1, i ),
                    //             lda, a( i, i ), lda, zero, x( 1, i ), 1 )
                    GenericMtxVecMult(pX0i00, LineWidthX, pA0i00, pAii00, LineWidthA, sizeof(double), width - i, i, 1, 0);
                    // CALL dgemv( 'No transpose', m-i, i-1, -one, x( i+1, 1 ),
                    //             ldx, x( 1, i ), 1, one, x( i+1, i ), 1 )
                    GenericMtxVecMult(pXii10, LineWidthX, pXi010, pX0i00, LineWidthX, LineWidthX, i, height - i - 1, -1, 1);
                    // CALL dscal( m-i, taup( i ), x( i+1, i ), 1 )
                    MatrixScaleAndAdd(pXii10, LineWidthX, 1, height - i - 1, 0, taup^[i]);

                    // Update A(i+1:m,i)

                    // CALL dgemv( 'No transpose', m-i, i-1, -one, a( i+1, 1 ),
                    //             lda, y( i, 1 ), ldy, one, a( i+1, i ), 1 )
                    GenericMtxVecMult(pAii10, LineWidthA, pAi010, pYi000, LineWidthA, sizeof(double), i, height - i - 1, -1, 1);
                    // CALL dgemv( 'No transpose', m-i, i, -one, x( i+1, 1 ),
                    //             ldx, a( 1, i ), 1, one, a( i+1, i ), 1 )
                    GenericMtxVecMult(pAii10, LineWidthA, pXi010, pA0i00, LineWidthX, LineWidthA, i + 1, height - i - 1, -1, 1);

                    // Generate reflection Q(i) to annihilate A(i+2:m,i)
                    pAminIM := GenPtr(A, i, min(i + 2, height - 1), LineWidthA);
                    GenElemHousholderRefl(pAminIM, LineWidthA, height - i - 1, pAii10^, @tauq^[i]);
                    e^[i] := pAii10^;
                    pAii10^ := 1;
                    
                    // Compute Y(i+1:n,i)

                    //  CALL dgemv( 'Transpose', m-i, n-i, one, a( i+1, i+1 ),
                    //              lda, a( i+1, i ), 1, zero, y( i+1, i ), 1 )
                    GenericMtxVecMultT(pYii10, LineWidthY, pAii11, pAii10, LineWidthA, LineWidthA, width - i - 1, height - i - 1, 1, 0);
                    // CALL dgemv( 'Transpose', m-i, i-1, one, a( i+1, 1 ), lda,
                    //             a( i+1, i ), 1, zero, y( 1, i ), 1 )
                    GenericMtxVecMultT(pY0i00, LineWidthY, pAi010, pAii10, LineWidthA, LineWidthA, i, height - i - 1, 1, 0);
                    // CALL dgemv( 'No transpose', n-i, i-1, -one, y( i+1, 1 ),
                    //             ldy, y( 1, i ), 1, one, y( i+1, i ), 1 )
                    GenericMtxVecMult(pYii10, LineWidthY, pYi010, pY0i00, LineWidthY, LineWidthY, i, width - i - 1, -1, 1);
                    // CALL dgemv( 'Transpose', m-i, i, one, x( i+1, 1 ), ldx,
                    //             a( i+1, i ), 1, zero, y( 1, i ), 1 )
                    GenericMtxVecMultT(pY0i00, LineWidthY, pXi010, pAii10, LineWidthX, LineWidthA, i + 1, height - i - 1, 1, 0);
                    // CALL dgemv( 'Transpose', i, n-i, -one, a( 1, i+1 ), lda,
                    //             y( 1, i ), 1, one, y( i+1, i ), 1 )
                    GenericMtxVecMultT(pYii10, LineWidthY, pA0i01, pY0i00, LineWidthA, LineWidthY, width - i - 1, i + 1, -1, 1);
                    // CALL dscal( n-i, tauq( i ), y( i+1, i ), 1 )
                    MatrixScaleAndAdd(pYii10, LineWidthY, 1, width - i - 1, 0, tauq^[i]);
               end;
          end;
     end;
end;

procedure MatrixLeftQFromQRDecomp(A : PDouble; const LineWidthA : TASMNativeInt; width, height : TASMNativeInt; 
 tau : PDouble; BlockSize : TASMNativeInt; work : PDouble; progress : TLinEquProgress = nil);
var qrData : TRecMtxQRDecompData;
begin
     qrData.pWorkMem := nil;
     qrData.work := work;
     qrData.BlkMultMem := nil;
     qrData.Progress := progress;
     qrData.qrWidth := width;
     qrData.qrHeight := height;
     qrData.actIdx := 0;
     qrData.pnlSize := BlockSize;
     qrData.LineWidthWork := sizeof(double)*qrdata.pnlSize;
     qrData.MatrixMultT1 := {$IFDEF FPC}@{$ENDIF}MatrixMultT1Ex;
     qrData.MatrixMultT2 := {$IFDEF FPC}@{$ENDIF}MatrixMultT2Ex;

     if work = nil then
     begin
          qrData.pWorkMem := GetMemory(qrData.pnlSize*sizeof(double)*height + 64 + BlockMultMemSize(QRMultBlockSize) );
          qrData.work := PDouble(qrData.pWorkMem);
          if (NativeUInt(qrData.pWorkMem) and $0000000F) <> 0 then
             qrData.work := PDouble(NativeUInt(qrData.pWorkMem) + 16 - NativeUInt(qrData.pWorkMem) and $0F);
     end;

     // it's assumed that the work memory block may also be used
     // as blocked multiplication memory storage!
     qrData.BlkMultMem := qrData.work;
     inc(qrData.BlkMultMem, qrData.pnlSize*height);

     InternalBlkMatrixLeftQFromQRDecomp(A, LineWidthA, width, height, tau, qrData);
     if not Assigned(work) then
        freeMem(qrData.pWorkMem);
end;

// assumed nxn matrix
procedure dorglq(A : PDouble; const LineWidthA : TASMNativeInt; width, height : TASMNativeInt; tau : PDouble; BlockSize : integer; work : PDouble);
begin  
     MatrixLeftQFromQRDecomp(A, LineWidthA, width, height, tau, QRBlockSize, work);
end;

// DORGBR generates one of the real orthogonal matrices Q or P**T
// determined by DGEBRD when reducing a real matrix A to bidiagonal
// form: A = Q * B * P**T.  Q and P**T are defined as products of
// elementary reflectors H(i) or G(i) respectively.

// A is assumed to have been an M-by-K matrix, and Q
//  is of order M


procedure Dorgbr_Q(A : PDouble; const LineWidthA : TASMNativeInt; width, height, k : TASMNativeInt; tau : PDouble; work : PDouble); //; const LineWidthWork : TASMNativeInt);
begin
     // boils down essentially to Q from QR Decomp...
     // k = width = height for svd..
     MatrixQFromQRDecomp(A, LineWidthA, width, height, tau, QRBlockSize, work);
end;

procedure Dorgbr_P(A : PDouble; const LineWidthA : TASMNativeInt; width, height, k : TASMNativeInt; tau : PDouble; work : PDouble); //; const LineWidthWork : TASMNativeInt);
var pA : PDouble;
    i : Integer;
    j : Integer;
    pAij, PAi1j : PDouble;
begin
     // Shift the vectors which define the elementary reflectors one
     // row downward, and set the first row and column of P**T to
     // those of the unit matrix
     pA := A;
     pA^ := 1;
     inc(PByte(pA), LineWidthA);

     for i := 1 to width - 1 do
     begin
          pA^ := 0;
          inc(PByte(pA), LineWidthA);  
     end;

     for j := width - 1 downto 1 do   // rows
     begin     
          for i := j to width - 1 do // cols
          begin
               pAij := GenPtr(A, i, j, LineWidthA);
               pAi1j := GenPtr(A, i, j - 1, LineWidthA);
          
               pAij^ := pAi1j^;
          end;
     end;

     pAij := GenPtr(A, 1, 0, LineWidthA);
     for i := 1 to width - 1 do
     begin
          pAij^ := 0;
          inc(pAij);
     end;
         

     if width > 1 then
     begin
          pA := GenPtr(A, 1, 1, LineWidthA);
     
          // form P_Transposed
          MatrixLeftQFromQRDecomp(pA, LineWidthA, width - 1, height - 1, tau, QRBlockSize, work);
     end;
end;

// DGEBRD reduces a general real M-by-N matrix A to upper or lower
// bidiagonal form B by an orthogonal transformation: Q**T * A * P = B
// M : height
// N : width
// column major
// On entry, the M-by-N general matrix to be reduced.
// On exit,
// if m >= n, the diagonal and the first superdiagonal are
//   overwritten with the upper bidiagonal matrix B; the
//   elements below the diagonal, with the array TAUQ, represent
//   the orthogonal matrix Q as a product of elementary
//   reflectors, and the elements above the first superdiagonal,
//   with the array TAUP, represent the orthogonal matrix P as
//   a product of elementary reflectors;
// if m < n, the diagonal and the first subdiagonal are
//   overwritten with the lower bidiagonal matrix B; the
//   elements below the first subdiagonal, with the array TAUQ,
//   represent the orthogonal matrix Q as a product of
//   elementary reflectors, and the elements above the diagonal,
//   with the array TAUP, represent the orthogonal matrix P as
//   a product of elementary reflectors.
procedure dgebrd(A : PDouble; const LineWidthA : TASMNativeInt; width, height : TASMNativeInt;
                 D, E, TauQ, TauP : PDouble; work : PDouble; nb : integer = 32);
var lwkopt : TASMNativeInt;
    minmn : TASMNativeInt;
    ws : TASMNativeInt;
    nx : integer;
    i, j : TASMNativeInt;
    pWork : PDouble;
    ldwrkx, ldwrky : TASMNativeInt;
    pAinb, pAinbnb : PDouble;
    pAii : PDouble;
    pX, pY : PDouble;
    pXnb, pYnb : PDouble;
    pWorkMult : PDouble;
    pD, pE, pTauQ, pTauP : PDouble;
begin
     minmn := min(width, height);
     if minmn = 0 then
        exit;

     ldwrkx := height;
     ldwrky := width;

     // determine when to switch from blocked to unblocked code
     // note: we assume that work is large enough to hold (width + height)*nb elements
     nx := nb;  
     nx := Min(nx, minmn);
     
     i := 0;

     px := Work;
     py := Work;
     inc(py, ldwrkx*nb);   
     pWorkMult := pY;
     inc(pWorkMult, ldwrky*nb);  

     pD := D;
     pE := E;
     pTauQ := tauQ;
     pTauP := tauP;

     while i < minmn - nx do
     begin
          // Reduce rows and columns i:i+nb-1 to bidiagonal form and return
          // the matrices X and Y which are needed to update the unreduced
          // part of the matrix
          pAii := GenPtr(A, i, i, LineWidthA);
          dlabrd(pAii, LineWidthA, width - i, height - i, nb,  PConstDoubleArr(pD), PConstDoubleArr(pE), 
                 PConstDoubleArr(ptauQ), PConstDoubleArr(ptauP), px, nb*sizeof(double), py, nb*sizeof(double) );

          // update the trailing submatir A(i +nb:m, i+nb:n) using an update of the form
          // A = A - VY**T - X*U**T
          pAinb := GenPtr(A, i, i + nb, LineWidthA);
          pAinbnb := GenPtr(A, i + nb, i + nb, LineWidthA);

          //  CALL dgemm( 'No transpose', 'Transpose', m-i-nb+1, n-i-nb+1,
          //               nb, -one, a( i+nb, i ), lda,
          //               work( ldwrkx*nb+nb+1 ), ldwrky, one,
          //               a( i+nb, i+nb ), lda )
          pYnb := GenPtr(pY, 0, nb, nb*sizeof(double));
          
          MatrixMultT2Ex(pAinbnb, LineWidthA, pAinb, pYnb, nb, height - i - nb, nb, width - i - nb,
                          LineWidthA, nb*sizeof(double), nb, doSub, pWorkMult );
                          
          // CALL dgemm( 'No transpose', 'No transpose', m-i-nb+1, n-i-nb+1,
          //             nb, -one, work( nb+1 ), ldwrkx, a( i, i+nb ), lda,
          //             one, a( i+nb, i+nb ), lda )
          pXnb := GenPtr(pX, 0, nb, nb*sizeof(double)); 
          pAinb := GenPtr(A, i + nb, i, LineWidthA);
          MatrixMultEx(pAinbnb, LineWidthA, pXnb, pAinb, nb, height - i - nb, width - i - nb, nb,
                       nb*sizeof(double), LineWidthA, nb, doSub, pWorkMult );

          // Copy diagonal and off-diagonal elements of B back into A
          if height >= width then
          begin
               j := i;
               while j < i + nb do
               begin
                    pAii^ := PConstDoubleArr(d)^[j];
                    inc(pAii);
                    pAii^ := PConstDoubleArr(e)^[j];
                    inc(PByte(pAii), LineWidthA);
                    inc(j);
               end;
          end
          else
          begin
               j := i;
               while j < i + nb do
               begin
                    pAii^ := PConstDoubleArr(d)^[j];
                    inc(PByte(pAii), LineWidthA);
                    pAii^ := PConstDoubleArr(e)^[j];
                    inc(pAii);
                    inc(j);
               end;
          end;

          // counters
          inc(i, nb);
          inc(pD, nb);
          inc(pE, nb);
          inc(pTauQ, nb);
          inc(pTauP, nb);
     end;
     
     // ###########################################
     // #### Use unblocked code to reduce the remainder of the matrix
     pAii := GenPtr(A, i, i, LineWidthA);
     dgebd2(pAii, LineWidthA, width - i, height - i, PConstDoubleArr(pD), 
            PConstDoubleArr(pE), PConstDoubleArr(pTauQ), PConstDoubleArr(pTauP),
            work);
end;

// computes the singular value decomposition of a 2-by-2 triangular matrix
procedure dlasv2(const F, G, H : double; var ssmin, ssmax, snr, csr, snl, csl : double); 
var ft, fa, ht, ha : double;
    pmax : integer;
    swap : boolean;
    temp : double;
    ga, gt : double;
    clt : double;
    crt : double;
    slt : double;
    srt : double;
    gasmal : boolean;
    epsilon : double;
    d : double;
    l : double;
    m, t : double;
    mm, tt : double;
    a, r, s : double;
    tsign : double;
begin
     srt := 0;
     crt := 0;
     clt := 0;
     slt := 0;
     
     ft := f;
     fa := abs( ft );
     ht := h;
     ha := abs( h );

     // PMAX points to the maximum absolute element of matrix
     // PMAX = 1 if F largest in absolute values
     // PMAX = 2 if G largest in absolute values
     // PMAX = 3 if H largest in absolute values
     pmax := 1;
     swap := ha > fa;

     if swap then
     begin
          pmax := 3;
          
          temp := ft;
          ft := ht;
          ht := temp;
          temp := fa;
          fa := ha;
          ha := temp;
     end;

     gt := g;
     ga := abs( gt );
     if ga = 0 then
     begin
          // diagonal matrix
          ssmin := ha;
          ssmax := fa;
          clt := 1;
          crt := 1;
          slt := 0;
          srt := 0;
     end
     else
     begin
          gasmal := True;

          if ga > fa then
          begin
               pmax := 2;
               if fa/ga < cDoubleEpsilon then
               begin
                    // Case of very large GA
                    gasmal := False;
                    ssmax := ga;
                    if ha > 1 
                    then
                        ssmin := fa/(ga/ha)
                    else
                        ssmin := (fa/ga)*ha;

                    clt := 1;
                    slt := ht/gt;
                    srt := 1;
                    crt := ft/gt;
               end;
          end;

          if gasmal then
          begin
               // normal case
               d := fa - ha;
               if d = fa 
               then // copes with infinite F or H
                   l := 1
               else
                   l := d/fa;

               // Note that 0 .le. L .le. 1
               m := gt/ft;

               // Note that abs(M) .le. 1/macheps
               t := 2 - l;

               // Note that T .ge. 1
               mm := sqr(m);
               tt := sqr(t);

               s := sqrt( tt + mm );

               // Note that 1 .le. S .le. 1 + 1/macheps
               if l = 0 
               then
                   r := abs( m )
               else
                   r := sqrt(l*l + mm );

               a := 0.5*(s + r);

               ssmin := ha/a;
               ssmax := fa*a;

               if mm = 0 then
               begin
                    // note tha m is ver tiny
                    if l = 0 
                    then
                        t := sign(2, ft)*sign(1, gt)
                    else
                        t := gt/sign(d, ft) + m/t;
               end
               else
                   t := (m/(s + t) + m/(r + l) )*(1 + a);

               l := sqrt(sqr(t) + 4);
               crt := 2/l;
               srt := t/l;
               clt := (crt + srt*m)/a;
               slt := (ht / ft)*srt/a;
          end;
     end;

     if swap then
     begin
          csl := srt;
          snl := crt;
          csr := slt;
          snr := clt;
     end
     else
     begin
          csl := clt;
          snl := slt;
          csr := crt;
          snr := srt;
     end;

     // Correct signs of SSMAX and SSMIN
     tsign := 0;
     if pmax = 1 then
        tsign := sign(1, csr)*sign(1, csl)*sign(1, f);
     if pmax = 2 then
        tsign := sign(1, snr)*sign(1, csl)*sign(1, g);
     if pmax = 3 then
        tsign := sign(1, snr)*sign(1, snl)*sign(1, h);

     ssmax := sign(ssmax, tsign);
     ssmin := sign(ssmin, tsign*sign(1, f)*sign(1, h));
end;

// apply a plane rotation
procedure drot(N : TASMNativeInt; DX : PDouble; const LineWidthDX : TASMNativeInt; DY : PDouble; LineWidthDY : TASMNativeInt; const c, s : double); 
var i: Integer;
    pX : PConstDoubleArr;
    pY : PConstDoubleArr;
    dtemp : double;
begin 
     if n <= 0 then
        exit;

     // faster code if it's in i row...
     if (LineWidthDX = sizeof(double)) and (LineWidthDY = sizeof(double)) then
     begin
          pX := PConstDoubleArr(DX);
          pY := PConstDoubleArr(DY);
          for i := 0 to n - 1 do
          begin
               dtemp := c*pX[i] + s*pY[i];
               pY[i] := c*pY[i] - s*pX[i];
               px[i] := dtemp;
          end;
     end
     else
     begin
          for i := 0 to n - 1 do
          begin
               dtemp := c*dx^ + s*dy^;
               dy^ := c*dy^ - s*dx^;
               dx^ := dtemp;

               inc(PByte(dx), LineWidthDX);
               inc(PByte(dy), LineWidthDY);
          end;
     end;
end;

// compute singular values of 2 by 2 matrix
procedure dlas2(const F, G, H : double; var ssmin, ssmax : double); //inline;
var fa, ga, ha, fhmn, fhmx : double;
    ass, at, au : double;
    c : double;
begin
     fa := abs(f);
     ga := abs(g);
     ha := abs(h);
     fhmn := min(fa, ha);
     fhmx := max(fa, ha);

     if fhmn = 0 then
     begin
          ssmin := 0;
          if fhmx = 0 
          then
              ssmax := ga
          else
              ssmax := max( fhmx, ga )*sqrt( 1 + sqr(min( fhmx, ga ) / max( fhmx, ga ) ) );
     end
     else
     begin
          if ga < fhmx then
          begin
               ass := 1 + fhmn/fhmx;
               at := ( fhmx-fhmn ) / fhmx;
               au := sqr(ga/fhmx);
               c := 2/(sqrt( ass*ass + au) + sqrt( at*at + au ) );
               ssmin := fhmn*c;
               ssmax := fhmx/c;
          end
          else
          begin
               au := fhmx/ga;

               if au = 0 then
               begin
                    // Avoid possible harmful underflow if exponent range
                    // asymmetric (true SSMIN may not underflow even if
                    // AU underflows)
                    ssmin := (fhmn*fhmx)/ga;
                    ssmax := ga;
               end
               else
               begin
                    ass := 1 + fhmn/fhmx;
                    at := (fhmx - fhmn)/fhmx;
                    c := 1/( sqrt(1 + sqr(ass*au) ) + sqrt( 1 + sqr(at*au) ) );
                    ssmin := (fhmn*c)*au;
                    ssmin := ssmin + ssmin;
                    ssmax := ga/(c + c);
               end;
          end;
     end;
end;

// generates a plane rotation with real cosine and real sine
procedure dlartg(F, G : double; var CS, SN, R : double);
var f1, g1, scale : double;
    count : integer;
    i: Integer;
const cSaveMin : double = 6.7180e-138; // sqrt(100*MinDouble)/cDoubleEpsilon
      cSaveMax : double = 1.4885e+137; // 1/cSaveMin;
begin
     if g = 0 then
     begin
          cs := 1;
          sn := 0;
          r := f;
     end
     else if f = 0 then
     begin
          cs := 0;
          sn := 1;
          r := g;
     end
     else
     begin
          f1 := f;
          g1 := g;
          scale := max( abs(f1), abs(g1));
          if scale >= cSaveMax then
          begin
               count := 0;
               repeat
                     inc(count);
                     f1 := f1*cSaveMin;
                     g1 := g1*cSaveMin;

                     scale := max( abs(f1 ), abs( g1 ) );
               until scale >= cSaveMax;

               r := sqrt( sqr(f1) + sqr(g1) );
               cs := f1/r;
               sn := g1/r;

               for i := 0 to count - 1 do
                   r := r*cSaveMax;
          end
          else if scale <= cSaveMin then
          begin
               count := 0;
               repeat
                     inc(count);
                     f1 := f1*cSaveMax;
                     g1 := g1*cSaveMax;

                     scale := max( abs(f1 ), abs( g1 ) );
               until scale <= cSaveMin;

               r := sqrt( sqr(f1) + sqr(g1) );
               cs := f1/r;
               sn := g1/r;
               for i := 0 to count - 1 do
                   r := r*cSaveMin;
          end
          else
          begin
               // normal path
               r := sqrt( sqr(f1) + sqr(g1) );
               cs := f1/r;
               sn := g1/r;
          end;

          if (abs( f ) > abs( g )) and (cs < 0) then
          begin
               cs := -cs;
               sn := -sn;
               r := -r;
          end;
     end;
end;

procedure dlasr_LVF(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);
var cTemp, stemp, temp : double;
    x, y : TASMNativeInt;
    pcAy, pcAy1 : PConstDoubleArr;
begin
     for y := 0 to height - 2 do
     begin
          ctemp := c[y];
          stemp := s[y];

          if (ctemp <> 1) or (stemp <> 0) then
          begin
               pcAy := PConstDoubleArr(GenPtr(A, 0, y, LineWidthA));
               pcAy1 := PConstDoubleArr(GenPtr(A, 0, y + 1, LineWidthA));
               for x := 0 to width - 1 do
               begin
                    temp := pcAy1^[x];
                    pcAy1^[x] := cTemp*temp - stemp*pcAy^[x];
                    pcAy^[x] := stemp*temp + ctemp*pcAy^[x];
               end;
          end;
     end;
end;

procedure dlasr_LVB(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);
var cTemp, stemp, temp : double;
    x, y : TASMNativeInt;
    pcAy, pcAy1 : PConstDoubleArr;
begin
     for y := height - 2 downto 0 do
     begin
          ctemp := c[y];
          stemp := s[y];

          if (ctemp <> 1) or (stemp <> 0) then
          begin
               pcAy := PConstDoubleArr(GenPtr(A, 0, y, LineWidthA));
               pcAy1 := PConstDoubleArr(GenPtr(A, 0, y + 1, LineWidthA));
               for x := 0 to width - 1 do
               begin
                    temp := pcAy1^[x];
                    pcAy1^[x] := cTemp*temp - stemp*pcAy^[x];
                    pcAy^[x] := stemp*temp + ctemp*pcAy^[x];
               end;
          end;
     end;
end;

{
procedure dlasr_LTF(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);
var cTemp, stemp, temp : double;
    x, y : TASMNativeInt;
    pcAy, pcAy1 : PConstDoubleArr;
begin
     for y := 1 to height - 1 do
     begin
          ctemp := c[y - 1];
          stemp := s[y - 1];

          if (ctemp <> 1) or (stemp <> 0) then
          begin
               pcAy := PConstDoubleArr(GenPtr(A, 0, y, LineWidthA));
               pcAy1 := PConstDoubleArr(GenPtr(A, 0, 0, LineWidthA));
               for x := 0 to width - 1 do
               begin
                    temp := pcAy^[x];
                    pcAy^[x] := cTemp*temp - stemp*pcAy1^[x];
                    pcAy1^[x] := stemp*temp + ctemp*pcAy1^[x];
               end;
          end;
     end;
end;

procedure dlasr_LTB(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);
var cTemp, stemp, temp : double;
    x, y : TASMNativeInt;
    pcAy, pcAy1 : PConstDoubleArr;
begin
     for y := height - 1 downto 1 do
     begin
          ctemp := c[y - 1];
          stemp := s[y - 1];
          if (ctemp <> 1) or (stemp <> 0) then
          begin
               pcAy := PConstDoubleArr(GenPtr(A, 0, y, LineWidthA));
               pcAy1 := PConstDoubleArr(GenPtr(A, 0, 0, LineWidthA));
                              
               for x := 0 to width - 1 do
               begin
                    temp := pcAy^[x];
                    pcAy^[x] := ctemp*temp - stemp*pcAy1^[x];
                    pcAy1^[x] := stemp*temp + ctemp*pcAy1^[x];
               end;
          end;
     end;
end;

procedure dlasr_LBF(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);
var cTemp, stemp, temp : double;
    x, y : TASMNativeInt;
    pcAy, pcAy1 : PConstDoubleArr;
begin
     for y := 0 to height - 2 do
     begin
          ctemp := c[y];
          stemp := s[y];

          if (ctemp <> 1) or (stemp <> 0) then
          begin
               pcAy := PConstDoubleArr(GenPtr(A, 0, y, LineWidthA));
               pcAy1 := PConstDoubleArr(GenPtr(A, 0, height - 1, LineWidthA));
               for x := 0 to width - 1 do
               begin
                    temp := pcAy^[x];
                    pcAy^[x] := stemp*pcAy1^[x] + ctemp*temp;
                    pcAy1^[x] := ctemp*pcAy1^[x] - stemp*temp;
               end;
          end;
     end;
end;

procedure dlasr_LBB(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);
var cTemp, stemp, temp : double;
    x, y : TASMNativeInt;
    pcAy, pcAy1 : PConstDoubleArr;
begin
     for y := height - 2 downto 0 do
     begin
          ctemp := c[y];
          stemp := s[y];

          if (ctemp <> 1) or (stemp <> 0) then
          begin
               pcAy := PConstDoubleArr(GenPtr(A, 0, y, LineWidthA));
               pcAy1 := PConstDoubleArr(GenPtr(A, 0, height - 1, LineWidthA));
               for x := 0 to width - 1 do
               begin
                    temp := pcAy^[x];
                    pcAy^[x] := stemp*pcAy1^[x] + ctemp*temp;
                    pcAy1^[x] := ctemp*pcAy1^[x] - stemp*temp;
               end;
          end;
     end;
end;
}

procedure dlasr_RVF(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);
var temp : double;
    x, y : TASMNativeInt;
    pAx : PConstDoubleArr;
begin
     for y := 0 to Height - 1 do
     begin
          pAx := PConstDoubleArr( GenPtr( A, 0, y, LineWidthA) );
          
          for x := 0 to width - 2 do
          begin
               temp := pAx^[x + 1];
               pAx^[x + 1] := c^[x]*temp - s^[x]*pAx^[x];
               pAx^[x] := s^[x]*temp + c^[x]*pAx^[x];
          end;
     end;
end;

procedure dlasr_RVB(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);
var temp : double;
    x, y : TASMNativeInt;
    pAx : PConstDoubleArr;
begin
     for y := 0 to Height - 1 do
     begin
          pAx := PConstDoubleArr( GenPtr( A, 0, y, LineWidthA) );
          
          for x := width - 2 downto 0 do
          begin
               temp := pAx^[x + 1];
               pAx^[x + 1] := c^[x]*temp - s^[x]*pAx^[x];
               pAx^[x] := s^[x]*temp + c^[x]*pAx^[x];
          end;
     end;
end;

procedure dlasr_RTF(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);
var cTemp, stemp, temp : double;
    x, y : TASMNativeInt;
    pAy, pAy1 : PDouble;
begin
     for y := 1 to width - 1 do
     begin    
          ctemp := c[y - 1];
          stemp := s[y - 1];

          if (ctemp <> 1) or (stemp <> 0) then
          begin
               pAy := GenPtr(A, y, 0, LineWidthA);
               pAy1 := GenPtr(A, 0, 0, LineWidthA);
                               
               for x := 0 to height - 1 do
               begin
                    temp := pAy^;
                    pAy^ := ctemp*temp - stemp*pAy1^;
                    pAy1^ := stemp*temp + ctemp*pAy1^;

                    inc(PByte(pAy), LineWidthA);
                    inc(PByte(pAy1), LineWidthA);
               end;
          end;
     end;
end;

procedure dlasr_RTB(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);
var cTemp, stemp, temp : double;
    x, y : TASMNativeInt;
    pAy, pAy1 : PDouble;
begin
     for y := width - 1 downto 1 do
     begin
          ctemp := c[y - 1];
          stemp := s[y - 1];

          if (ctemp <> 1) or (stemp <> 0) then
          begin
               pAy := GenPtr(A, y, 0, LineWidthA);
               pAy1 := GenPtr(A, 0, 0, LineWidthA);
                               
               for x := 0 to height - 1 do
               begin
                    temp := pAy^;
                    pAy^ := ctemp*temp - stemp*pAy1^;
                    pAy1^ := stemp*temp + ctemp*pAy1^;

                    inc(PByte(pAy), LineWidthA);
                    inc(PByte(pAy1), LineWidthA);
               end;
          end;
     end;
end;

procedure dlasr_RBF(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);
var cTemp, stemp, temp : double;
    x, y : TASMNativeInt;
    pAy, pAy1 : PDouble;
begin
     for y := 0 to width - 2 do
     begin
          ctemp := c[y];
          stemp := s[y];

          if (ctemp <> 1) or (stemp <> 0) then
          begin
               pAy := GenPtr(A, y, 0, LineWidthA);
               pAy1 := GenPtr(A, width - 1, 0, LineWidthA);
                               
               for x := 0 to height - 1 do
               begin
                    temp := pAy^;
                    pAy^ := stemp*pAy1^ + ctemp*temp;
                    pAy1^ := ctemp*pAy1^ - stemp*temp;

                    inc(PByte(pAy), LineWidthA);
                    inc(PByte(pAy1), LineWidthA);
               end;
          end;
     end;
end;

procedure dlasr_RBB(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);
var cTemp, stemp, temp : double;
    x, y : TASMNativeInt;
    pAy, pAy1 : PDouble;
begin
     for y := width - 2 downto 0 do
     begin
          ctemp := c[y];
          stemp := s[y];

          if (ctemp <> 1) or (stemp <> 0) then
          begin
               pAy := GenPtr(A, y, 0, LineWidthA);
               pAy1 := GenPtr(A, width - 1, 0, LineWidthA);
                               
               for x := 0 to height - 1 do
               begin
                    temp := pAy^;
                    pAy^ := stemp*pAy1^ + ctemp*temp;
                    pAy1^ := ctemp*pAy1^ - stemp*temp;

                    inc(PByte(pAy), LineWidthA);
                    inc(PByte(pAy1), LineWidthA);
               end;
          end;
     end;
end;

procedure DswapCol(A : PDouble; const LineWidthA : TASMNativeInt; i1, i2, height : TASMNativeInt);
var temp : double;
    pAi1, pAi2 : PDouble;
    y: TASMNativeInt;
begin
     pAi1 := A;
     inc(pAi1, i1);
     pAi2 := A;
     inc(pAi2, i2);

     for y := 0 to height - 1 do
     begin
          temp := pAi1^;
          pAi1^ := pAi2^;
          pAi2^ := temp;

          inc(PByte(pAi1), LineWidthA);
          inc(PByte(pAi2), LineWidthA);
     end;
end;

procedure DswapRow(A : PDouble; const LineWidthA : TASMNativeInt; i1, i2, width : TASMNativeInt);
var temp : double;
    pAi1, pAi2 : PConstDoubleArr;
    x : TASMNativeInt;
begin
     pAi1 := PConstDoubleArr(A);
     inc(PByte(pAi1), i1*LineWidthA);
     pAi2 := PConstDoubleArr(A);
     inc(PByte(pAi2), i2*LineWidthA);

     for x := 0 to width - 1 do
     begin
          temp := pAi1^[x];
          pAi1^[x] := pAi2^[x];
          pAi2^[x] := temp;
     end;
end;

// upper diagonal
// DBDSQR computes the singular values and, optionally, the right and/or
// left singular vectors from the singular value decomposition (SVD) of
// a real N-by-N (upper or lower) bidiagonal matrix B using the implicit
// zero-shift QR algorithm.  The SVD of B has the form
function dbdsqr_upper(D : PConstDoubleArr; E : PConstDoubleArr;
                     VT : PDouble; const LineWidthVT : TASMNativeInt; U : PDouble; const LineWidthU : TASMNativeInt;
                     C : PDouble; const LineWidthC : TASMNativeInt; N, NCVT, NRU, NCC : TASMNativeInt; Work : PDouble) : TSVDResult;
const maxitr : integer = 6;
var nm1 : TASMNativeInt;
    nm12 : TASMNativeInt;
    nm13 : TASMNativeInt;
    smax : double;
    tol : Double;
    i : TASMNativeInt;
    thresh : double;
    sminoa : double;
    maxIt : TASMNativeInt;
    oldll : TASMNativeInt;
    iter : TASMNativeInt;
    oldm : TASMNativeInt;
    m : TASMNativeInt;
    mu : double;
    sminl : double;
    lll, ll: Integer;
    smin : double;
    abss, abse : double;
    idir : integer;
    doCont : boolean;
    shift, r : double;
    sll : double;
    f, g, h : double;
    cs, oldcs : double;
    sn, oldsn : double;
    pwork : PConstDoubleArr;
    sigmn, sigmx, sinr, cosr, sinl, cosl : double;
    isub : TASMNativeInt;
    j : TASMNativeInt;
    llFound : boolean;
begin
     Result := srOk;
     pwork := PConstDoubleArr(work);

     tol := 2e-14;//cDoubleEpsilon* max(10, min(100, Power(cDoubleEpsilon, -0.125 ) )) ;

     // Compute approximate maximum, minimum singular values
     smax := 0;
     for i := 0 to n - 1 do
         smax := max(smax, abs(d[i]));
     for i := 0 to n - 2 do
         smax := max(smax, abs(e[i]) );

     // relative accuracy desired
     sminl := 0;
     if tol >= 0 then
     begin
          sminoa := abs(d[0]);

          if sminoa <> 0 then
          begin
               mu := sminoa;
               for i := 1 to n - 1 do
               begin
                    mu := abs( d[i] )*( mu/(mu + abs(e[i - 1])));
                    sminoa := min(sminoa, mu);
                    if sminoa = 0 then
                       break;
               end;
          end;

          sminoa := sminoa/(sqrt(n));
          thresh := max( tol*sminoa, maxitr*n*n*MinDouble);
     end
     else
         thresh := max(abs(tol)*smax, maxitr*n*n*MinDouble);

     idir := 0;
     nm1 := n - 1;
     nm12 := nm1 + nm1;
     nm13 := nm12 + nm1; 

     // Prepare for main iteration loop for the singular values
     // MAXIT is the maximum number of passes through the inner
     // loop permitted before nonconvergence signalled.)
     maxIt := maxitr*n*n;
     oldll := -1;
     iter := 0;
     oldm := -1;

     // M points to last element of unconverged part of matrix
     m := n - 1;

     while m > 0 do
     begin
          if iter > maxit then
          begin
               Result := srNoConvergence;
               exit;
          end;

          // Find diagonal block of matrix to work on
          if (tol < 0) and (abs(d[m]) <= thresh) then
             d[m] := 0;

          smax := abs(d[m]);
          smin := smax;

          doCont := False;
          lll := 0;
          ll := -1;
          while lll < m do
          begin
               ll := m - lll - 1;
               abss := abs(d[ll]);
               abse := abs(e[ll]);

               if (tol < 0) and (abss <= thresh) then
                  d[ll] := 0;
               if abse <= thresh then
               begin
                    e[ll] := 0;
                    // matrix splits since e[ll] = 0

                    if ll = m - 1 then
                    begin
                         // Convergence of bottom singular value, return to top of loop
                         dec(m);
                         doCont := True;
                         break;
                    end;

                    break;
               end;

               smin := min(smin, abss);
               smax := max(smax, max(abss, abse));
               inc(lll);
          end;

          if doCont then
             continue;

          if lll = m then
             ll := -1;
             
          inc(ll);
          
          // E(LL) through E(M-1) are nonzero, E(LL-1) is zero
          if ll = m - 1 then
          begin
               // 2 by 2 block, handle separately
               dlasv2(d[m - 1], e[m - 1], d[m], sigmn, sigmx, sinr, cosr, sinl, cosl);
               d[m - 1] := sigmx;
               e[m - 1] := 0;
               d[m] := sigmn;

               // Compute singular vectors, if desired
               if ncvt > 0 then
                  drot(ncvt, GenPtr(vt, 0, m - 1, LineWidthVT), sizeof(double), GenPtr(vt, 0, m, LineWidthVT), sizeof(double), cosr, sinr);
               if NRU > 0 then
                  drot(nru, GenPtr(u, m - 1, 0, LineWidthU), LineWidthU, GenPtr(u, m, 0, LineWidthU), LineWidthU, cosl, sinl);
               if ncc > 0 then
                  drot(ncc, GenPtr(C, 0, m - 1, LineWidthC), sizeof(double), GenPtr(c, 0, m, LineWidthC), sizeof(double), cosl, sinl);

               dec(m, 2);

               // next m
               continue;
          end;
          
          // If working on new submatrix, choose shift direction
          // from larger end diagonal element towards smaller)
          if (ll > oldm) or (m < oldll) then
          begin
               if abs( d[ll] ) >= abs( d[m] ) 
               then
                   // chase bulge from top
                   idir := 1
               else // Chase bulge from bottom (big end) to top (small end)
                   idir := 2;
          end;

          // apply convergence test
          if idir = 1 then
          begin
               // Run convergence test in forward direction
               // First apply standard test to bottom of matrix
               if (abs(e[m - 1]) <= abs(tol)*abs(d[m]) ) or 
                  ( (tol < 0) and (abs( e[m - 1]) <= thresh ))
               then
               begin
                    e[m - 1] := 0;
                    continue;
               end;

               // apply convergence criterion forward
               if tol >= 0 then
               begin
                    mu := abs( d[ll] );
                    sminl := mu;

                    doCont := False;
                    for lll := ll to m - 1 do
                    begin
                         if abs( e[lll] ) <= tol*mu then
                         begin
                              e[lll] := 0;
                              doCont := True;
                              break;
                         end;

                         mu := abs( d[lll + 1]) * (mu / (mu + abs( e[lll] )));
                         sminl := min(sminl, mu);
                    end;

                    // check if we need to go all up 
                    if doCont then
                       continue;
               end;
          end
          else
          begin
               // Run convergence test in backward direction
               // First apply standard test to top of matrix

               if (abs(e[ll]) < abs(tol)*abs(d[ll]) ) or 
                  ( (tol < 0) and (abs(e[ll]) <= thresh) )
               then
               begin
                    e[ll] := 0;
                    // go all up
                    continue;
               end;

               if tol >= 0 then
               begin
                    // If relative accuracy desired,
                    // apply convergence criterion backward
                    mu := abs( d[m] );
                    sminl := mu;

                    doCont := False;
                    for lll := m - 1 downto ll do
                    begin
                         if abs(e[lll]) <= tol*mu then
                         begin
                              e[lll] := 0;
                              doCont := True;
                              break;
                         end;

                         mu := abs(d[lll])*(mu/(mu + abs(e[lll])));
                         sminl := min (sminl, mu);
                    end;

                    // all way up
                    if doCont then
                       continue; 
               end;
          end;

          oldll := ll;
          oldm := m; 

          // Compute shift.  First, test if shifting would ruin relative
          // accuracy, and if so set the shift to zero.
          if (tol >= 0) and  (n*tol*(sminl/smax) <= max(cDoubleEpsilon, 0.01*tol ) )
          then
              shift := 0
          else
          begin
               // Compute the shift from 2-by-2 block at end of matrix
               if idir = 1 then
               begin
                    sll := abs(d[ll]);
                    dlas2(d[m - 1], e[m - 1], d[m], shift, r);
               end
               else
               begin
                    sll := abs( d[m] );
                    dlas2(d[ll], e[ll], d[ll + 1], shift, r);
               end;

               if sll > 0 then
               begin
                    if sqr(shift/sll) < cDoubleEpsilon then
                       shift := 0;
               end;
          end;
          
          // increment iteration count
          iter := iter + m - ll;

          // If SHIFT = 0, do simplified QR iteration
          if shift = 0 then
          begin
               if idir = 1 then
               begin
                    // Chase bulge from top to bottom
                    // Save cosines and sines for later singular vector updates
                    cs := 1;
                    oldcs := 1;
                    oldsn := 0;

                    for i := ll to m - 1 do
                    begin
                         dlartg( d[i]*cs, e[i], cs, sn, r);
                         if i > ll then
                            e[i - 1] := oldsn*r;
                         dlartg(oldcs*r, d[i + 1]*sn, oldcs, oldsn, d[i]);
                         
                         pwork[i - ll] := cs;
                         pwork[i - ll + nm1] := sn;
                         pwork[i - ll + nm12] := oldcs;
                         pwork[i - ll + nm13] := oldsn;
                    end;

                    h := d[m]*cs;
                    d[m] := h*oldcs;
                    e[m - 1] := h*oldsn;

                    // update singular vectors
                    if ncvt > 0 then
                       dlasr_LVF(ncvt, m - ll + 1, GenPtr(VT, 0, ll, LineWidthVT), LineWidthVT,
                                 PConstDoubleArr(GenPtr(work, 0, 0, 0)), PConstDoubleArr(GenPtr(work, n - 1, 0, 0))); 
                    if nru > 0 then
                       dlasr_RVF(m - ll + 1, nru, GenPtr(u, ll, 0, LineWidthU), LineWidthU,
                                 PConstDoubleArr(GenPtr(work, nm12, 0, 0)), PConstDoubleArr(GenPtr(work, nm13, 0, 0)));
                    if ncc > 0 then
                       dlasr_LVF(ncc, m - ll + 1, GenPtr(c, 0, ll, LineWidthC), LineWidthC,
                                 PConstDoubleArr(GenPtr(work, nm12, 0, 0)), PConstDoubleArr(GenPtr(work, nm13, 0, 0))); 

                    // test convergence
                    if abs( e[m - 1] ) <= thresh then
                       e[m - 1] := 0;
               end
               else
               begin
                    // Chase bulge from bottom to top
                    // Save cosines and sines for later singular vector updates
                    // Chase bulge from top to bottom
                    // Save cosines and sines for later singular vector updates
                    cs := 1;
                    oldcs := 1;
                    oldsn := 0;

                    for i := m downto ll + 1 do
                    begin
                         dlartg( d[i]*cs, e[i - 1], cs, sn, r);
                         if i < m then
                            e[i] := oldsn*r;
                         dlartg(oldcs*r, d[i - 1]*sn, oldcs, oldsn, d[i]);
                         
                         pwork[i - ll - 1] := cs;
                         pwork[i - ll - 1 + nm1] := -sn;
                         pwork[i - ll - 1 + nm12] := oldcs;
                         pwork[i - ll - 1 + nm13] := -oldsn;
                    end;

                    h := d[ll]*cs;
                    d[ll] := h*oldcs;
                    e[ll] := h*oldsn;

                    // update singular vectors
                    if ncvt > 0 then
                       dlasr_LVB(ncvt, m - ll + 1, GenPtr(VT, 0, ll, LineWidthVT), LineWidthVT,
                                 PConstDoubleArr(GenPtr(work, nm12, 0, 0)), PConstDoubleArr(GenPtr(work, nm13, 0, 0))); 
                    if nru > 0 then
                       dlasr_RVB(m - ll + 1, nru, GenPtr(u, ll, 0, LineWidthU), LineWidthU,
                                 PConstDoubleArr(GenPtr(work, 0, 0, 0)), PConstDoubleArr(GenPtr(work, n - 1, 0, 0)));
                    if ncc > 0 then
                       dlasr_LVB(ncc, m - ll + 1, GenPtr(c, 0, ll, LineWidthC), LineWidthC,
                                 PConstDoubleArr(GenPtr(work, 0, 0, 0)), PConstDoubleArr(GenPtr(work, n - 1, 0, 0))); 

                    // test convergence
                    if abs( e[ll] ) <= thresh then
                       e[ll] := 0;
               end;
          end
          else
          begin
               // use non zero shift
               if idir = 1 then
               begin
                    // Chase bulge from top to bottom
                    // Save cosines and sines for later singular vector updates
                    f := ( abs(D[ll] ) - shift)*
                         ( sign(1, d[ll] ) + shift/d[ll] );
                    g := e[ll];

                    for i := ll to m - 1 do
                    begin
                         dlartg( f, g, cosr, sinr, r);
                         if i > ll then
                            e[i - 1] := r;

                         f := cosr*d[i] + sinr*e[i];
                         e[i] := cosr*e[i] - sinr*d[i];
                         g := sinr*d[i + 1];
                         d[i + 1] := cosr*d[i + 1];
                         dlartg(f, g, cosl, sinl, r);
                         
                         d[i] := r;
                         f := cosl*e[i] + sinl*d[i + 1];
                         d[i + 1] := cosl*d[i + 1] - sinl*e[i];
                         if i < m - 1 then
                         begin
                              g := sinl*e[i + 1];
                              e[i + 1] := cosl*e[i + 1];
                         end;
                         
                         pwork[i - ll] := cosr;
                         pwork[i - ll + nm1] := sinr;
                         pwork[i - ll + nm12] := cosl;
                         pwork[i - ll + nm13] := sinl;
                    end;

                    e[m - 1] := f;

                    // update singular vectors
                    if ncvt > 0 then
                       dlasr_LVF(ncvt, m - ll + 1, GenPtr(VT, 0, ll, LineWidthVT), LineWidthVT,
                                 PConstDoubleArr(GenPtr(work, 0, 0, 0)), PConstDoubleArr(GenPtr(work, n - 1, 0, 0))); 
                    if nru > 0 then
                       dlasr_RVF(m - ll + 1, nru, GenPtr(u, ll, 0, LineWidthU), LineWidthU,
                                 PConstDoubleArr(GenPtr(work, nm12, 0, 0)), PConstDoubleArr(GenPtr(work, nm13, 0, 0)));
                    if ncc > 0 then
                       dlasr_LVF(ncc, m - ll + 1, GenPtr(c, 0, ll, LineWidthC), LineWidthC,
                                 PConstDoubleArr(GenPtr(work, nm12, 0, 0)), PConstDoubleArr(GenPtr(work, nm13, 0, 0))); 

                    // test convergence
                    if abs( e[m - 1] ) <= thresh then
                       e[m - 1] := 0;
               end
               else
               begin
                    // Chase bulge from bottom to top
                    // Save cosines and sines for later singular vector updates
                    f := ( abs( d[m] ) - shift)*(sign(1, d[m]) + shift/d[m]);
                    g := e[m - 1];
                    
                    for i := m downto ll + 1 do
                    begin
                         dlartg(f, g, cosr, sinr, r);
                         if i < m then
                            e[i] := r;
                         f := cosr*d[i] + sinr*e[i - 1];
                         e[i - 1] := cosr*e[i - 1] - sinr*d[i];
                         g := sinr*d[i - 1];
                         d[i - 1] := cosr*d[i - -1];
                         dlartg(f, g, cosl, sinl, r);
                         d[i] := r;
                         f := cosl*e[i - 1] + sinl*d[i - 1];
                         d[i - 1] := cosl*d[i - 1] - sinl*e[i - 1];
                         if i > ll + 1 then
                         begin
                              g := sinl*e[i - 2];
                              e[i - 2] := cosl*e[i - 2];
                         end;
                         
                         pwork[i - ll - 1] := cs;
                         pwork[i - ll - 1 + nm1] := -sn;
                         pwork[i - ll - 1 + nm12] := oldcs;
                         pwork[i - ll - 1 + nm13] := -oldsn;
                    end;

                    e[ll] := f;

                    // test convergence
                    if abs( e[ll] ) <= thresh then
                       e[ll] := 0;

                    // update singular vectors
                    if ncvt > 0 then
                       dlasr_LVB(ncvt, m - ll + 1, GenPtr(VT, 0, ll, LineWidthVT), LineWidthVT,
                                 PConstDoubleArr(GenPtr(work, nm12, 0, 0)), PConstDoubleArr(GenPtr(work, nm13, 0, 0))); 
                    if nru > 0 then
                       dlasr_RVB(m - ll + 1, nru, GenPtr(u, ll, 0, LineWidthU), LineWidthU,
                                 PConstDoubleArr(GenPtr(work, 0, 0, 0)), PConstDoubleArr(GenPtr(work, n - 1, 0, 0)));
                    if ncc > 0 then
                       dlasr_LVB(ncc, m - ll + 1, GenPtr(c, 0, ll, LineWidthC), LineWidthC,
                                 PConstDoubleArr(GenPtr(work, 0, 0, 0)), PConstDoubleArr(GenPtr(work, n - 1, 0, 0))); 
               end;
          end;
     end;

     // All singular values converged, so make them positive
     for i := 0 to n - 1 do
     begin
          if d[i] < 0 then
          begin
               d[i] := -d[i];

               // change sign of singular vectors, if desired
               if ncvt > 0 then
                  MatrixScaleAndAdd(GenPtr(VT, 0, i, LineWidthVT), LineWidthVT, NCVT, 1, 0, -1); // ???
          end;
     end;

     // sort singular values into decreasing order
     for i := 0 to n - 2 do
     begin
          isub := 0;
          smin := d[0];

          for j := 1 to n - i - 1 do
          begin
               if d[j] <= smin then
               begin
                    isub := j;
                    smin := d[j];
               end;
          end;

          if isub <> n - i - 1 then
          begin
               // swap singular values and vectors
               d[isub] := d[n - i - 1];
               d[n - i - 1] := smin;
               
               if ncvt > 0 then
                  DswapRow(vt, LineWidthVT, isub, n - i - 1, ncvt);
               if nru > 0 then
                  DswapCol(u, LineWidthU, isub, n - i - 1, nru);
               if ncc > 0 then
                  DswapCol(C, LineWidthC, isub, n - i - 1, ncc);
          end;
     end;
end;

procedure WriteMatlData(const fileName: string;
  const data: PConstDoubleArr; width, height: integer);
var i : integer;
    s : string;
begin
     DecimalSeparator := '.';
     // write a file which can be read into matlab using the load command
     // the procedure is usefull to verify the results against this program.
     with TStringList.Create do
     try
        BeginUpdate;
        s := '';
        for i := 0 to width*height - 1 do
        begin
             s := s + Format('%.9f,', [data[i]]);

             if i mod width = width - 1 then
             begin
                  if width = 1 
                  then
                      System.Delete(s, Length(s), 1)
                  else
                      s[length(s)] := ';';
                  Add(s);
                  s := '';
             end;
        end;
        EndUpdate;

        SaveToFile(FileName {$IF not Defined(FPC) and (CompilerVersion >= 20)} , TEncoding.ASCII {$IFEND});
     finally
            Free;
     end;
end;


// wntuo = true
// wntvs = true
// wntuas = false
// wntvas = true
//
// path maxwrk

// jobu = 'o'
// jobvt = 's'
// The output is the computation of A= U*W*V' whereas U is stored in A, and W is a vector 0..Width-1. The matrix V (not V') must be as large as Width*Width!
function dgesvd( A : PDouble; const LineWidthA : TASMNativeInt; width : TASMNativeInt; Height : TASMNativeInt; 
                 W : PConstDoubleArr; V : PDouble; const LineWidthV : TASMNativeInt; SVDBlockSize : TASMNativeInt = 32;
                 aWork : PByte = nil;
                 progress : TLinEquProgress = nil) : TSVDResult;
var work : PDouble;
    pMem : PByte;
    eps1, smallnum , bignum : double;
    minmn : TASMNativeInt;
    absMax : double;
    doScale : boolean;
    mnthr : TASMNativeInt;
    pWorkITau : PDouble;
    pWorkTauQ, pWorkTaup, pWorkI, pWorkIE : PDouble;
    pWorkU : PDouble;
    pV, pA : PConstDoubleArr;
    pWorkIArr : PConstDoubleArr;
    isScaled : boolean;
    x, y : TASMNativeInt;
    pWorkIR : PDouble;
    LineWidthIR : TASMNativeInt;
    ldwrku, ldwrkr : TASMNativeInt;
    itau : TASMNativeInt;
    chunk : TASMNativeInt;
    w2, h2 : TASMNativeInt;
    memNeed : TASMNativeInt;
    qrMem : TASMNativeInt;
begin
     if width > Height then
        raise Exception.Create('Not implemented');

     // ###########################################
     // #### Machine constants
     minmn := Min(Width, Height);
     eps1 := eps(1);
     if MinDouble <= 1/MaxDouble
     then
         smallnum := 1/MaxDouble * (1 + eps1)
     else
         smallnum := MinDouble;

     smallnum := sqrt(smallnum)/eps1;
     bignum := 1/smallnum;
     
     // for factors of 2.. (better aligned memory)
     w2 := width + width and 1;
     h2 := height + height and 1;

     mnthr := Trunc( minmn*1.6 );

     // ###########################################
     // #### Allocate workspace for fast algorithm...
     if aWork = nil then
     begin
          memNeed := 16 + Max(2*w2*w2*sizeof(double) + w2,
                              sizeof(double)*(w2*w2 + 3*w2) + QRDecompMemSize(QRBlockSize, height)
                             )  + 
                     BlockMultMemSize( BlockMatrixCacheSize );

          // no qr decomp -> reduce workspace needed
          if (height >= width) and (Height <= mnthr) then
          begin
               // mem for TauP, TauQ, D, E, Bidiagonlaization multiplication memory
               memNeed := 16 + sizeof(double)*(  (3 + 5)*w2 + (w2 + height)*SVDBlockSize) + BlockMultMemSize(Max(SVDBlockSize, QRMultBlockSize));
          end;
     
          pMem := AllocMem(memNeed);
     end
     else
         pMem := aWork;

     work := PDouble(pMem);
     if (TASMNativeUInt(work) and $0000000F) <> 0 then
        work := PDouble(TASMNativeUInt(work) + 16 - TASMNativeUInt(work) and $0F);

     
     // ###########################################
     // #### Scale A if max element outside range [smallnum,bignum]
     absMax := abs(MatrixMax(A, width, height, LineWidthA));

     isScaled := (absMax < smallNum) or (absMax > bigNum);
     
     if absMax < smallnum
     then
         MatrixScaleAndAdd(A, LineWidthA, Width, Height, 0, smallnum/absMax)
     else if absMax > bigNum 
     then
         MatrixScaleAndAdd(A, LineWidthA, Width, Height, 0, bignum/absMax);
             
     // ###########################################
     // #### crossover
     if Height >= Width then
     begin
          if Height > mnthr then
          begin
               // path3 in dgesvd:
               pWorkIR := work;
               pWorkITau := work;
               inc(pWorkITau, w2*w2);
               pWorkI := pWorkITau;
               inc(pWorkI, w2);
               
               // Compute A=Q*R
               if MatrixQRDecompInPlace2(A, LineWidthA, width, height, pWorkITau, pWorkI, QRBlockSize) <> qrOK then
               begin   
                    Result := srNoConvergence;
                    if aWork = nil then
                       FreeMem(pMem);
                    exit;
               end;

               // copy R to VT - zeroing out below it
               MatrixCopy(V, LineWidthV, A, LineWidthA, width, width);

               for y := 1 to width - 1 do
               begin
                    pV := PConstDoubleArr( GenPtr(V, 0, y, LineWidthV) );
                    for x := 0 to  y - 1 do
                        pV^[x] := 0;
               end;

               // Generate Q in A
               MatrixQFromQRDecomp(A, LineWidthA, width, Height, pWorkITau, QRBlockSize, pWorkI);

               // Bidiagonalize R in VT, copying result to WORK(IR)
               pWorkIE := pWorkITau;
               pWorkTauQ := pWorkIE;
               inc(pWorkTauQ, w2);
               pWorkTauP := pWorkTauQ;
               inc(pWorkTauP, w2);
               pWorkI := pWorkTauP;
               inc(pWorkI, w2);
                
               dgebrd(V, LineWidthV, width, width, PDouble(W), pWorkIE, pWorkTauQ, pWorkTauP, pWorkI, SVDBlockSize );

               // copy lower triangle to work 
               for y := 0 to width - 1 do
               begin
                    pV := PConstDoubleArr( GenPtr(v, 0, y, LineWidthV) );
                    pWorkIArr := PConstDoubleArr( GenPtr(pWorkIR, 0, y, w2*sizeof(double)) );
                    for x := 0 to y do
                        pWorkIArr^[x] := pV^[x];
               end;

               // Generate left vectors bidiagonalizing R in WORK(IR) 
               Dorgbr_Q(pWorkIR, w2*sizeof(double), width, width, width, pWorkTauq, pWorkI);

               // Generate right vectors bidiangoizaing R in VT
               Dorgbr_P(V, LineWidthV, width, width, width, pWorkTaup, pWorkI);
               
               pWorkI := pWorkIE;
               inc(pWorkI, width);
               
               // Perform bidiagonal QR iteration, computing left
               // singular vectors of R in WORK(IR) and computing right
               // singular vectors of R in VT
               Result := dbdsqr_upper(W, PConstDoubleArr(pWorkIE), V, LineWidthV, pWorkIR, w2*sizeof(double), nil, 0, width, width, width, 0, pworki);

               if Result = srOk then
               begin
                    // Q in A by left singular vectors of R...

                    // pworkU = w2 x w2
                    pWorkU := pWorkIE;
                    pWorkI := pWorkU;   // w2*w2 
                    inc(pWorkI, w2*w2);
               
                    x := 0;
                    while x < height do
                    begin
                         chunk := min(height - x, width);
                                                    
                         MatrixMultEx(pWorkU, w2*sizeof(double), GenPtr(A, 0, x, LineWidthA), pWorkIR, width, chunk, width, width,
                                      LineWidthA, w2*sizeof(double), BlockMatrixCacheSize, doNone, pWorkI); 
                         MatrixCopy(GenPtr(A, 0, x, LineWidthA), LineWidthA, pWorkU, w2*sizeof(double), width, chunk);

                         inc(x, chunk);
                    end;
               end;
          end
          else
          begin
               // m at least N but not much larger
               pWorkIE := work;
               pWorkTauQ := work;
               inc(pWorkTauQ, w2);
               pWorkTaup := pWorkTauQ;
               inc(pWorkTaup, w2);

               pWorkI := pWorkTaup;
               inc(pWorkI, w2);
               
               // Bidiagonalize A
               dgebrd(A, LineWidthA, width, height, PDouble(W), pWorkIE, pWorkTauQ, pWorkTauP, pWorkI, SVDBlockSize);

               // right singular vectors on V, copy upper triangle part of A
               for y := 0 to width - 1 do
               begin
                    pV := PConstDoubleArr(GenPtr(V, 0, y, LineWidthV));
                    pA := PConstDoubleArr(GenPtr(A, 0, y, LineWidthA));
                    for x := y to width - 1 do
                        pV^[x] := pA^[x];
               end;

               dorgbr_P(v, LineWidthV, width, width, width, pWorkTaup, pWorkI);
               
               // Left singular vectors in A
               Dorgbr_Q(A, LineWidthA, width, height, width, pWorkTauq, pWorkI);

               pWorkI := pWorkIE;
               inc(pWorkI, w2);
               // bidiagonal QR iteration. if desired, computing
               // left singular vectors in A and computing right singular
               // vectors in VT 
               Result := dbdsqr_upper(W, PConstDoubleArr(pWorkIE), V, LineWidthV, A, LineWidthA, nil, 0, width, width, height, 0, pWorkI)
          end;
     end;

     // ###########################################
     // #### Cleanup
     if aWork = nil  then
        FreeMem(pMem);
end;

end.

