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


unit Eigensystems;

// ############################################
// #### Functions to extract eigenvalues and eigenvectors
// ############################################

interface

{$IFDEF FPC} {$MODESWITCH ADVANCEDRECORDS} {$ENDIF}

uses MatrixConst;

// ############################################
// #### functions for nonsymmetric matrices:
// executes the functions below in order to get the result.
function MatrixUnsymEigVecInPlace(A : PDouble; const LineWidthA : NativeInt; width : NativeInt; WR : PDouble;
 const LineWidthWR : NativeInt; WI : PDouble; const LineWidthWI : NativeInt; Eivec : PDouble; const LineWidthEivec : NativeInt) : TEigenvalueConvergence;
function MatrixUnsymEigVec(const A : PDouble; const LineWidthA : NativeInt; width : NativeInt; WR : PDouble;
 const LineWidthWR : NativeInt; WI : PDouble; const LineWidthWI : NativeInt; Eivec : PDouble; const LineWidthEivec : NativeInt) : TEigenvalueConvergence;

function MatrixUnsymEigInPlace(A : PDouble; const LineWidthA : NativeInt; width : NativeInt; WR : PDouble;
 const LineWidthWR : NativeInt; WI : PDouble; const LineWidthWI : NativeInt; balance : boolean) : TEigenvalueConvergence;
function MatrixUnsymEig(const A : PDouble; const LineWidthA : NativeInt; width : NativeInt; WR : PDouble;
 const LineWidthWR : NativeInt; WI : PDouble; const LineWidthWI : NativeInt; balance : boolean) : TEigenvalueConvergence;

// Given a Matrix A[0..width-1][0..Width-1], this routine replaces it by a balanced matrix
// with identical eigenvalues. A symmetric matrix is already balanced and is unaffected by this procedure.
procedure MatrixBalanceInPlace(A : PDouble; const LineWidthA : NativeInt; width : NativeInt; Scale : PDouble; const LineWidthScale : NativeInt);
procedure MatrixBalanceBackInPlace(A : PDouble; const LineWidthA : NativeInt; width : NativeInt; Scale : PDouble; const LineWidthScale : NativeInt);
// Reduction of Matrix A to hessenberg form by the elimination method. The real, nonsymmetric matrix
// A is replaced by an upper Hessenberg matrix with identical eigenvalues. Recommended, but not
// required, is that this routine be preceded by MatrixBalance. Non Hessenberg elements (which should be zero)
// are filled with random values and not replaced by zero elements.
procedure MatrixHessenbergPermInPlace(A : PDouble; const LineWidthA : NativeInt; width : NativeInt; perm : PInteger; const LineWidthPerm : NativeInt);
procedure MatrixHessenbergPerm(dest : PDouble; const LineWidthDest : NativeInt; A : PDouble; const LineWidthA : NativeInt; width : NativeInt; perm : PInteger; const LineWidthPerm : NativeInt);
procedure MatrixHessenbergInPlace(A : PDouble; const LineWidthA : NativeInt; width : NativeInt);
procedure MatrixHessenberg(dest : PDouble; const LineWidthDest : NativeInt; A : PDouble; const LineWidthA : NativeInt; width : NativeInt);

// copies the hessenberg matrix to dest using the permutation vector from the hessenberg transformation. Initializes the destination matrix for the eigenvector
// finding routine for unsymmetric matrices
procedure MatrixInitEivecHess(hess : PDouble; const LineWidthHess : NativeInt; width : NativeInt; dest : PDouble; const LineWidthDest : NativeInt; perm : PInteger; const LineWidthPerm : NativeInt);

// Finds all eigenvalues of an upper Hessenberg matrix A. On input a can be exactly as output from MatrixHessenberg, on output
// A is destroyed. The real and imaginary parts of the eigenvalues are returned in wr[0..width-1] and wi[0..width-1], respectively
function MatrixEigHessenbergInPlace(A : PDouble; const LineWidthA : NativeInt; width : NativeInt; WR : PDouble;
 const LineWidthWR : NativeInt; WI : PDouble; const LineWidthWI : NativeInt) : TEigenvalueConvergence;
// the following function does not destroy A on output.
function MatrixEigHessenberg(A : PDouble; const LineWidthA : NativeInt; width : NativeInt; WR : PDouble;
 const LineWidthWR : NativeInt; WI : PDouble; const LineWidthWI : NativeInt) : TEigenvalueConvergence;
// finds all eigenvalues and eigenvectors in an upper hessenberg matrix A. On input A can be exactly as output from MatrixHessenbergPerm, on
// output A is destroyed. For complex eigenvalues (n, n+1) only one eigenvector is stored whereas the real part is stored in vector n and
// the imaginary part is stored in n+1. Note the function does not seem to correctly work with matrices with rank lower than width!
function MatrixEigVecHessenbergInPlace(A : PDouble; const LineWidthA : NativeInt; width : NativeInt; WR : PDouble;
 const LineWidthWR : NativeInt; WI : PDouble; const LineWidthWI : NativeInt; Eivec : PDouble; const LineWidthEivec : NativeInt) : TEigenvalueConvergence;

 // normalize the eigenvectors either to a length of 1 (std) or such that the largest item has a value of 1
 procedure MatrixNormEivecInPlace(Eivec : PDouble; const LineWidthEivec : NativeInt; width : NativeInt;
   WI : PDouble; const LineWidthWI : NativeInt; normalizeLen : boolean);


// ############################################
// #### functions for symmetric matrices:

// Householder reduction of a real, symmetric matrix A[0..width-1][0..width-1]. On output
// A is replaced by the orthogonal matrix Q effecting the transformation. D[0..width-1] returns
// the diagonal elements of the tridiagonal matrix, and e[0..width - 1] of the off-diagonal elements with e[0] = 0.
procedure MatrixTridiagonalHouseInPlace(A : PDouble; const LineWidthA : NativeInt; const width : NativeInt;
  D : PDouble; const LineWidthD : NativeInt; E : PDouble; const LineWidthE : NativeInt);
procedure MatrixTridiagonalHouse(dest : PDouble; const LineWidthDest : NativeInt; A : PDouble; const LineWidthA : NativeInt; const width : NativeInt;
  D : PDouble; const LineWidthD : NativeInt; E : PDouble; const LineWidthE : NativeInt);

function MatrixTridiagonalQLImplicitInPlace(Z : PDouble; const LineWidthZ : NativeInt; width : NativeInt;
  D : PDouble; const LineWidthD : NativeInt; E : PDouble; const LineWidthE : NativeInt) : TEigenvalueConvergence;

function MatrixEigTridiagonalMatrixInPlace(A : PDouble; const LineWidthA : NativeInt; width : NativeInt; EigVals : PDouble; const LineWidthEigVals : NativeInt) : TEigenvalueConvergence;
function MatrixEigTridiagonalMatrix(Dest : PDouble; const LineWidthDest : NativeInt; A : PDouble; const LineWidthA : NativeInt; width : NativeInt; EigVals : PDouble; const LineWidthEigVals : NativeInt) : TEigenvalueConvergence;

// ###########################################
// #### Reduction of a real general matrix A to upper Hessenberg form H by
// an orthogonal similarity transformation Q' * A * Q = H
// original DGEHRD

// A: On entry, the N-by-N general matrix to be reduced.
// On exit, the upper triangle and the first subdiagonal of A
// are overwritten with the upper Hessenberg matrix H, and the
// elements below the first subdiagonal, with the array TAU,
// represent the orthogonal matrix Q as a product of elementary
// reflectors.
procedure MatrixHessenberg2InPlace(A : PDouble; const LineWidthA : NativeInt; width : NativeInt; tau : PDouble; hessBlockSize : integer);
procedure ThrMtxHessenberg2InPlace(A : PDouble; const LineWidthA : NativeInt; width : NativeInt; tau : PDouble; hessBlockSize : integer);

// generate a real orthogonal matrix Q from the element reflectors (matrixhessenberg2inplace) to
// satisfy Q' * A * Q = H (or Q * H * Q' = A)
// the input matrix A is overwritten by the orhogonal matrix Q
procedure MatrixQFromHessenbergDecomp(A : PDouble; const LineWidthA : NativeInt; width : NativeInt;
 tau : PDouble; BlockSize : NativeInt; work : PDouble; progress : TLinEquProgress = nil); overload;
procedure MatrixQFromHessenbergDecomp(A : PDouble; const LineWidthA : NativeInt; width : NativeInt;
 tau : PDouble; progress : TLinEquProgress = nil); overload;


// Symmetric eigenvalue problem.
// the function first tries to reduce the input matrix to a tridiagonal one and
// then tries to find the eigenvalues and optionaly eigenvectors
// on entry A is an nxn matrix and W is an nx1 vector.
// on exit W contains the eigenvalues and if wanted A the eigenvectors
function MatrixEigUpperSymmetricMatrixInPlace2(A : PDouble; LineWidthA : NativeInt; N : NativeInt; W : PConstDoubleArr; EigValOnly : boolean; blockSize : integer; progress : TLinEquProgress = nil ) : TEigenvalueConvergence;
function ThrMtxEigUpperSymmetricMatrixInPlace2(A : PDouble; LineWidthA : NativeInt; N : NativeInt; W : PConstDoubleArr; EigValOnly : boolean; blockSize : integer; progress : TLinEquProgress = nil ) : TEigenvalueConvergence;

function UperSymEigMemorySize( N, blockSize : NativeInt; EigValOnly : boolean; Threaded : Boolean; var blkMultSize : integer ) : NativeInt;

function InternalEigValFromSymMatrix( D, E : PConstDoubleArr; N : NativeInt) : TEigenvalueConvergence;
function InternalEigValEigVecFromSymTridiagMatrix( D, E : PConstDoubleArr; Z : PDouble;
 LineWidthZ : NativeInt; N : NativeInt; Work : PConstDoubleArr ) : TEigenvalueConvergence; overload;


// on entry A is a symmetric matrix (upper part will be used)
// on exit the diagonal and first superdiagonal of A are overwritten by the corresponding elements of
// the tridigonal matrix T, and the elements above the first superdiagonal, with the array TAU, represent the
// orthogonal matrix Q as a product of elementary reflectors.
// E and D conain the diagonal and off diagonal elements of the tridiagonal matrix T
// Tau (may be nil) contain the scalar factors of the elementary reflectors
// nb the blocksize used for block updates.
procedure MatrixUpperSymToTridiagInPlace( A : PDouble; LineWidthA : NativeInt; N : NativeInt;
  D, E, Tau : PConstDoubleArr; nb : NativeInt );

implementation

uses Math, MathUtilFunc, MatrixASMStubSwitch, HouseholderReflectors, types,
     BlockSizeSetup, ThreadedMatrixOperations, MtxThreadPool,
     LinAlgQR,
     SysUtils, MatrixRotations, Classes, CplxSimpleMatrixOperations;



// #########################################################################
// #### Eigenvalue calculation - NR (aka Eispack)
// #########################################################################

function MatrixEigTridiagonalMatrixInPlace(A : PDouble; const LineWidthA : NativeInt; width : NativeInt; EigVals : PDouble; const LineWidthEigVals : NativeInt) : TEigenvalueConvergence;
var E : Array of double;
begin
     SetLength(E, width);
     MatrixTridiagonalHouseInPlace(A, LineWidthA, width, EigVals, LineWidthEigVals, @E[0], sizeof(double));
     Result := MatrixTridiagonalQLImplicitInPlace(A, LineWidthA, width, EigVals, LineWidthEigVals, @E[0], sizeof(double));
end;

function MatrixEigTridiagonalMatrix(Dest : PDouble; const LineWidthDest : NativeInt; A : PDouble; const LineWidthA : NativeInt; width : NativeInt; EigVals : PDouble; const LineWidthEigVals : NativeInt) : TEigenvalueConvergence;
var E : Array of double;
begin
     SetLength(E, width);
     MatrixTridiagonalHouse(dest, LineWidthDest, A, LineWidthA, width, EigVals, LineWidthEigVals, @E[0], sizeof(double));
     Result := MatrixTridiagonalQLImplicitInPlace(dest, LineWidthDest, width, EigVals, LineWidthEigVals, @E[0], sizeof(double));
end;

procedure MatrixTridiagonalHouse(dest : PDouble; const LineWidthDest : NativeInt; A : PDouble; const LineWidthA : NativeInt; const width : NativeInt;
  D : PDouble; const LineWidthD : NativeInt; E : PDouble; const LineWidthE : NativeInt);
var pDest : PDouble;
    i : NativeInt;
begin
     Assert(width > 0, 'Dimension Error');
     Assert(LineWidthA >= width*sizeof(double), 'Dimension error');
     Assert(LineWidthD >= sizeof(double), 'Dimension error');
     Assert(LineWidthE >= sizeof(double), 'Dimension error');

     pDest := dest;
     // copy data -> now we can perform an inline LU decomposition
     for i := 0 to width - 1 do
     begin
          Move(A^, pDest^, sizeof(double)*width);
          inc(PByte(A), LineWidthA);
          inc(PByte(pDest), LineWidthDest);
     end;

     MatrixTridiagonalHouseInPlace(dest, LineWidthDest, width, D, LineWidthD, E, LineWidthE);
end;

procedure MatrixTridiagonalHouseInPlace(A : PDouble; const LineWidthA : NativeInt; const width : NativeInt;
  D : PDouble; const LineWidthD : NativeInt; E : PDouble; const LineWidthE : NativeInt);
var i, j : NativeInt;
    l, k : NativeInt;
    scale, hh, h, g, f : double;
    pA, pAi, pAj, pAk : PDouble;
    pEi, pEj, pEk : PDouble;
    pD : PDouble;
begin
     Assert(width > 0, 'Dimension Error');
     Assert(LineWidthA >= width*sizeof(double), 'Dimension error');
     Assert(LineWidthD >= sizeof(double), 'Dimension error');
     Assert(LineWidthE >= sizeof(double), 'Dimension error');

     pAi := A;
     inc(PByte(pAi), (width-1)*LineWidthA);
     pEi := E;
     inc(PByte(pEi), (width-1)*LineWidthE);
     for i := width - 1 downto 1 do
     begin
          l := i - 1;
          h := 0;
          scale := 0;

          if l > 0 then
          begin
               pA := pAi;

               for k := 0 to l do
               begin
                    scale := scale + abs(pA^);
                    inc(pA);
               end;

               if scale = 0 then
               begin
                    // skip transformation
                    dec(pA);
                    pEi^ := pA^;
               end
               else
               begin
                    pA := pAi;

                    for k := 0 to l do
                    begin
                         pA^ := pA^/scale;  // use scaled a's for transformation
                         h := h + sqr(pA^);
                         inc(pA);
                    end;

                    dec(pA);
                    f := pA^;
                    if f >= 0
                    then
                        g := -sqrt(h)
                    else
                        g := sqrt(h);

                    pEi^ := scale*g;
                    h := h - f*g;
                    pA^ := f - g;   // store u in the ith row of a

                    f := 0;

                    pAj := A;
                    pEj := E;
                    for j := 0 to l do
                    begin
                         pA := pAj;
                         inc(pA, i);
                         pAk := pAi;
                         inc(pAk, j);

                         // next statement can be omitted if eigenvectors not wanted
                         pA^ := pAk^/h;   // store u/H in ith column of a.
                         g := 0;

                         // Form an element of A*u in g
                         pAk := pAj;
                         pA := pAi;
                         for k := 0 to j do
                         begin
                              g := g + pA^*pAk^;
                              inc(pAk);
                              inc(pA);
                         end;
                         pAk := pAj;
                         pA := pAi;
                         inc(pA, j + 1);
                         inc(PByte(pAk), LineWidthA);
                         inc(pAk, j);
                         for k := j + 1 to l do
                         begin
                              g := g + pA^*pAk^;
                              inc(PByte(pAk), LineWidthA);
                              inc(pA);
                         end;

                         pEj^ := g/h;

                         pA := pAi;
                         inc(pA, j);
                         f := f + pEj^*pA^;

                         // next line
                         inc(PByte(pEj), LineWidthE);
                         inc(PByte(pAj), LineWidthA);
                    end;

                    hh := f/(h + h);

                    pAj := A;
                    pEj := E;
                    for j := 0 to l do
                    begin
                         pA := pAi;
                         inc(pA, j);

                         f := pA^;
                         pEj^ := pEj^ - hh*f;
                         g := pEj^;

                         pAk := pAj;
                         pEk := E;
                         pA := pAi;
                         for k := 0 to j do
                         begin
                              pAk^ := pAk^ - (f*pEk^ + g*pA^);
                              inc(pA);
                              inc(pAk);
                              inc(PByte(pEk), LineWidthE);
                         end;

                         inc(PByte(pAj), LineWidthA);
                         inc(PByte(pEj), LineWidthE);
                    end;
               end;
          end
          else
          begin
               pA := pAi;
               inc(pA, l);
               pEi^ := pA^;
          end;

          pD := D;
          inc(PByte(pD), i*LineWidthD);
          pD^ := h;

          dec(PByte(pAi), LineWidthA);
          dec(PByte(pEi), LineWidthE);
     end;

     // next statement can be omitted if eigenvectors not wanted
     D^ := 0;
     E^ := 0;

     // contents of this loop can be omitted if eigenvectors not wanted except for statement d[i] = a[i][i]

     // begin accumulation of transformation matrices.
     pAi := A;
     pD := D;
     for i := 0 to width - 1 do
     begin
          l := i - 1;
          if (pD^ <> 0) then
          begin
               for j := 0 to l do
               begin
                    g := 0;

                    pA := pAi;
                    pAk := A;
                    inc(pAk, j);
                    for k := 0 to l do
                    begin
                         g := g + pA^*pAk^;
                         inc(pA);
                         inc(PByte(pAk), LineWidthA);
                    end;

                    pA := A;
                    inc(pA, j);
                    pAk := A;
                    inc(pAk, i);
                    for k := 0 to l do
                    begin
                         pA^ := pA^ - g*pAk^;
                         inc(PByte(pA), LineWidthA);
                         inc(PByte(pAk), LineWidthA);
                    end;
               end;
          end;

          pA := pAi;
          inc(pA, i);
          pD^ := pA^;
          pA^ := 1;

          pAj := A;
          inc(pAj, i);
          pA := pAi;

          for j := 0 to l do
          begin
               pAj^ := 0;
               pA^ := 0;

               inc(PByte(pAj), LineWidthA);
               inc(pA);
          end;

          inc(PByte(pAi), LineWidthA);
          inc(PByte(pD), LineWidthD);
     end;
end;

function MatrixTridiagonalQLImplicitInPlace(Z : PDouble; const LineWidthZ : NativeInt; width : NativeInt;
  D : PDouble; const LineWidthD : NativeInt; E : PDouble; const LineWidthE : NativeInt) : TEigenvalueConvergence;
var m, l, iter, i, k : NativeInt;
    s, r, p, g, f, dd, c, b : double;
    pE, pEl, pEi : PDouble;
    pD, pDl : PDouble;
    pZ, pZi : PDouble;
const cMaxTridiagIter = 30;
begin
     pE := E;
     pEi := E;
     inc(PByte(pEi), LineWidthE);

     Result := qlNoConverge;

     for i := 1 to width - 1 do
     begin
          pE^ := pEi^;
          inc(PByte(pE), LineWidthE);
          inc(PByte(pEi), LineWidthE);
     end;

     pE := E;
     inc(PByte(pE), (width - 1)*LineWidthE);
     pE^ := 0;

     pDl := D;
     pEl := E;
     for l := 0 to width - 1 do
     begin
          iter := 0;

          repeat
                pD := pDl;
                pE := pEl;
                m := l;
                while m < width - 1 do
                begin
                     dd := abs(pD^);
                     inc(PByte(pD), LineWidthD);
                     dd := dd + abs(pD^);

                     if pE^ + dd = dd then
                        break;

                     inc(PByte(pE), LineWidthE);
                     inc(m);
                end;

                if m <> l then
                begin
                     inc(iter);
                     if iter = cMaxTridiagIter then
                        exit;

                     g := (PDouble(NativeUint(pDl) + NativeUint(LineWidthD))^ - pDl^)/(2*pEl^);
                     r := pythag(g, 1);
                     pD := D;
                     inc(PByte(pD), m*LineWidthD);
                     g := pD^ - pDl^ + pEl^/(g + sign(r, g));
                     s := 1;
                     c := 1;
                     p := 0;

                     pE := E;
                     inc(PByte(pE), (m - 1)*LineWidthE);
                     for i := m - 1 downto l do
                     begin
                          f := s*pE^;
                          b := c*pE^;
                          r := pythag(f, g);

                          inc(PByte(pE), LineWidthE);
                          pE^ := r;

                          if r = 0 then
                          begin
                               pD^ := pD^ - p;
                               pE := E;
                               inc(PByte(pE), m*LineWidthE);
                               pE^ := 0;

                               break;
                          end;

                          s := f/r;
                          c := g/r;
                          g := pD^ - p;
                          dec(PByte(pD), LineWidthD);
                          r := (pD^ - g)*s + 2*c*b;
                          p := s*r;
                          inc(PByte(pD), LineWidthD);
                          pD^ := g + p;
                          g := c*r - b;

                          // next loop can be omitted if eigenvectors not wanted
                          pZ := Z;
                          pZi := Z;
                          inc(pZ, i);
                          inc(pZi, i+1);
                          for k := 0 to width - 1 do
                          begin
                               f := pZi^;
                               pZi^ := s*pZ^ + c*f;
                               pZ^ := c*pZ^ - s*f;

                               inc(PByte(pZi), LineWidthZ);
                               inc(PByte(pZ), LineWidthZ);
                          end;

                          dec(PByte(pD), LineWidthD);
                          dec(PByte(pE), 2*LineWidthE);
                     end;

                     if (r = 0) and (i >= l) then
                        continue;

                     pDl^ := pDl^ - p;
                     pEl^ := g;
                     pE := E;
                     inc(PByte(pE), m*LineWidthE);
                     pE^ := 0;
                end;
          until m = l;

          inc(PByte(pDl), LineWidthD);
          inc(PByte(pEl), LineWidthE);
     end;

     Result := qlOk;
end;

procedure MatrixBalanceBackInPlace(A : PDouble; const LineWidthA : NativeInt; width : NativeInt; Scale : PDouble; const LineWidthScale : NativeInt);
var i : NativeInt;
begin
     for i := 0 to width - 1 do
     begin
          if Scale^ <> 1 then
          begin
               // Apply similarity transformation
               MatrixScaleAndAdd( GenPtr(A, 0, i, LineWidthA), LineWidthA, width, 1, 0, Scale^ );
               MatrixScaleAndAdd( GenPtr(A, i, 0, LineWidthA), LineWidthA, 1, width, 0, 1/Scale^ );
          end;

          inc(PByte(Scale), LineWidthScale);
     end;
end;

procedure MatrixBalanceInPlace(A : PDouble; const LineWidthA : NativeInt; width : NativeInt; Scale : PDouble; const LineWidthScale : NativeInt);
var j, i : NativeInt;
    last : boolean;
    s, r, g, f, c, sqrdx : double;
    pAi : PConstDoubleArr;
    pAj, pScale : PDouble;
const RADIX = 2.0;
begin
     assert(lineWidthA >= width*sizeof(double), 'Dimension error');
     assert(LineWidthScale >= sizeof(double), 'Dimension error');

     sqrdx := sqr(RADIX);
     last := False;

     MatrixInit(scale, LineWidthScale, 1, width, 1);
     
     while last = False do
     begin
          last := True;

          pScale := Scale;
          for i := 0 to width - 1 do
          begin
               r := 0;
               c := 0;

               pAi := GenPtrArr(A, 0, i, LineWidthA);
               pAj := GenPtr(A, i, 0, LineWidthA);
               for j := 0 to width - 1 do
               begin
                    if i <> j then
                    begin
                         c := c + abs(pAj^);
                         r := r + abs(pAi^[j]);
                    end;

                    inc(PByte(pAj), LineWidthA);
               end;

               if (c <> 0) and (r <> 0) then
               begin
                    g := r/RADIX;
                    f := 1;
                    s := c + r;

                    // find the integer power of the machine radix that comes closest to balancing the matrix.
                    while c < g do
                    begin
                         f := f*RADIX;
                         c := c*sqrdx;
                    end;
                    g := r*RADIX;
                    while c > g do
                    begin
                         f := f/RADIX;
                         c := c/sqrdx;
                    end;

                    if (c + r)/f < 0.95*s then
                    begin
                         pScale^ := pScale^*f;
                         last := False;
                         g := 1/f;

                         // Apply similarity transformation
                         MatrixScaleAndAdd( GenPtr(A, 0, i, LineWidthA), LineWidthA, width, 1, 0, g );
                         MatrixScaleAndAdd( GenPtr(A, i, 0, LineWidthA), LineWidthA, 1, width, 0, f );
                    end;
               end;

               inc(PByte(pScale), LineWidthScale);
          end;
     end;
end;

procedure MatrixHessenbergPerm(dest : PDouble; const LineWidthDest : NativeInt; A : PDouble; const LineWidthA : NativeInt; width : NativeInt; perm : PInteger; const LineWidthPerm : NativeInt);
begin
     assert(LineWidthA >= width*sizeof(double), 'Dimension Error');
     assert(LineWidthDest >= width*sizeof(double), 'Dimension Error');

     MatrixCopy(Dest, LineWidthDest, A, LineWidthA, width, width);
     MatrixHessenbergPermInPlace(dest, LineWidthDest, width, perm, LineWidthPerm);
end;

procedure MatrixHessenbergPermInPlace(A : PDouble; const LineWidthA : NativeInt; width : NativeInt; perm : PInteger; const LineWidthPerm : NativeInt);
var m, j, i : NativeInt;
    x, y : double;
    pAm, pAj, pAi : PDouble;
    pA : PDouble;
    pPerm : PInteger;
begin
     assert(LineWidthA >= width*sizeof(double), 'Dimension Error');
     pAm := A;
     pPerm := Perm;
     if Assigned(pPerm) then
     begin
          pPerm^ := 0;
          inc(PByte(pPerm), LineWidthPerm);
     end;
     m := 1;
     while m < width - 1 do
     begin
          inc(PByte(pAm), LineWidthA);
          x := 0;
          i := m;

          // find the pivot
          pAj := pAm;
          inc(pAj, m - 1);
          for j := m to width - 1 do
          begin
               if abs(pAj^) > abs(x) then
               begin
                    x := pAj^;
                    i := j;
               end;

               inc(PByte(pAj), LineWidthA);
          end;

          if Assigned(pPerm) then
          begin
               pPerm^ := i;
               inc(PByte(pPerm), LineWidthPerm);
          end;

          // interchange rows and columns
          if i <> m then
          begin
               pAi := A;
               inc(pAi, m - 1);
               inc(PByte(pAi), i*LineWidthA);
               pAj := pAm;
               inc(pAj, m - 1);
               for j := m - 1 to width - 1 do
               begin
                    DoubleSwap(pAi^, pAj^);
                    inc(pAj);
                    inc(pAi);
               end;

               pAi := A;
               inc(pAi, i);
               pAj := A;
               inc(pAj, m);
               for j := 0 to width - 1 do
               begin
                    DoubleSwap(pAi^, pAj^);
                    inc(PByte(pAi), LineWidthA);
                    inc(PByte(pAj), LineWidthA);
               end;
          end;

          // carry out the elimination
          if x <> 0 then
          begin
               pA := pAm;
               inc(PByte(pA), LineWidthA);
               inc(pA, m - 1);
               i := m + 1;
               while i < width do
               begin
                    y := pA^;
                    if y <> 0 then
                    begin
                         y := y/x;

                         pA^ := y;

                         pAi := A;
                         inc(pAi, m);
                         inc(PByte(pAi), i*LineWidthA);
                         pAj := pAm;
                         inc(pAj, m);
                         for j := m to width - 1 do
                         begin
                              pAi^ := pAi^ - y*pAj^;
                              inc(pAj);
                              inc(pAi);
                         end;

                         pAi := A;
                         inc(pAi, i);
                         pAj := A;
                         inc(pAj, m);
                         for j := 0 to width - 1 do
                         begin
                              pAj^ := pAj^ + y*pAi^;
                              inc(PByte(pAi), LineWidthA);
                              inc(PByte(pAj), LineWidthA);
                         end;
                    end;

                    inc(PByte(pA), LineWidthA);
                    inc(i);
               end;
          end;

          inc(m);
     end;
end;

function MatrixEigHessenberg(A : PDouble; const LineWidthA : NativeInt; width : NativeInt; WR : PDouble;
 const LineWidthWR : NativeInt; WI : PDouble; const LineWidthWI : NativeInt) : TEigenvalueConvergence;
var pDest : PDouble;
    dest : Array of double;
    i : NativeInt;
begin
     Assert(width > 0, 'Dimension Error');
     Assert(LineWidthA >= width*sizeof(double), 'Dimension error');

     setLength(dest, width*width);

     pDest := @dest[0];

     // copy data 
     for i := 0 to width - 1 do
     begin
          Move(A^, pDest^, sizeof(double)*width);
          inc(PByte(A), LineWidthA);
          inc(PByte(pDest), width*sizeof(double));
     end;

     Result := MatrixEigHessenbergInPlace(@dest[0], width*sizeof(double), width, WR, LineWidthWR, WI, LineWidthWI);
end;

function MatrixEigHessenbergInPlace(A : PDouble; const LineWidthA : NativeInt; width : NativeInt; WR : PDouble;
 const LineWidthWR : NativeInt; WI : PDouble; const LineWidthWI : NativeInt) : TEigenvalueConvergence;
var nn, m, l, k : NativeInt;
    j, its, i, mmin : NativeInt;
    x, y, z : double;
    u, v, w : double;
    r, s, t : double;
    q, p, anorm : double;
    pAi, pAj, pAl, pA, pAm, pAk : PDouble;
    pWr : PDouble;
    pWi : PDouble;
const cMaxEigHessenbergIter = 30;
begin
     Assert(width > 0, 'Dimension Error');
     Assert(LineWidthA >= width*sizeof(double), 'Dimension error');
     Assert(LineWidthWR >= sizeof(double), 'Dimension error');
     Assert(LineWidthWI >= sizeof(double), 'Dimension error');

     r := 0;
     p := 0;
     q := 0;
     Result := qlNoConverge;
     anorm := abs(A^);
     pAi := A;
     inc(PByte(pAi), LineWidthA);
     // compute matrix norm for possible use in locating single small subdiagonal element
     for i := 1 to width - 1 do
     begin
          pAj := pAi;
          inc(pAj, i - 1);
          for j := i - 1 to width - 1 do
          begin
               anorm := anorm + abs(pAj^);

               inc(pAj);
          end;

          inc(PByte(pAi), LineWidthA);
     end;

     nn := width - 1;
     t := 0;
     // gets changed only by an exceptional shift.
     while nn >= 0 do
     begin
          // begin search of the next eigenvalue
          its := 0;
          repeat
                // begin iteration: look for single small subdiagonal element.
                pAl := A;
                inc(pAl, nn);
                inc(PByte(pAl), nn*LineWidthA);
                l := nn;
                while l >= 1 do
                begin
                     s := abs(pAl^);
                     dec(pAl);
                     dec(PByte(pAl), LineWidthA);
                     s := s + abs(pAl^);

                     if s = 0 then
                        s := anorm;

                     inc(PByte(pAl), LineWidthA);
                     if abs(pAl^) + s = s then
                        break;

                     dec(PByte(pAl), LineWidthA);
                     dec(l);
                end;

                pA := A;
                inc(pA, nn);
                inc(PByte(pA), nn*LineWidthA);
                x := pA^;
                // one root found
                if l = nn then
                begin
                     pWr := WR;
                     inc(PByte(pWr), nn*LineWidthWR);
                     pWr^ := x + t;
                     pWi := WI;
                     inc(PByte(pWi), nn*LineWidthWI);
                     pWi^ := 0;
                     dec(nn);
                end
                else
                begin
                     dec(pA);
                     dec(PByte(pA), LineWidthA);
                     y := pA^;
                     inc(pA);
                     w := pA^;
                     dec(pA);
                     inc(PByte(pA), LineWidthA);
                     w := w*pA^;
                     // two roots found ...
                     if l = nn - 1 then
                     begin
                          p := 0.5*(y - x);
                          q := sqr(p) + w;
                          z := sqrt(abs(q));
                          x := x + t;

                          pWr := WR;
                          inc(PByte(pWr), (nn-1)*LineWidthWR);
                          pWi := WI;
                          inc(PByte(pWi), (nn-1)*LineWidthWI);
                          // ... a real pair
                          if q >= 0 then
                          begin
                               z := p + sign(z, p);

                               pWr^ := x + z;
                               inc(PByte(pWr), LineWidthWR);
                               pWr^ := x + z;
                               if z <> 0 then
                                  pWr^ := x - w/z;

                               pWi^ := 0;
                               inc(PByte(pWi), LineWidthWI);
                               pWi^ := 0;
                          end
                          else
                          begin
                               // ...a complex pair
                               pWr^ := x + p;
                               inc(PByte(pWr), LineWidthWR);
                               pWr^ := x + p;
                               pWi^ := -z;
                               inc(PByte(pWi), LineWidthWI);
                               pWi^ := z;
                          end;

                          dec(nn, 2);
                     end
                     else
                     begin
                          // no roots found. Continue iteration
                          if its = cMaxEigHessenbergIter then
                             exit;

                          // form exceptional shift
                          if (its = 10) or (its = 20) then
                          begin
                               t := t + x;
                               pAi := A;
                               for i := 0 to width - 1 do
                               begin
                                    pAi^ := pAi^ - x;
                                    inc(pAi);
                                    inc(PByte(pAi), LineWidthA);
                               end;

                               pA := A;
                               inc(pA, nn - 1);
                               inc(PByte(pA), nn*LineWidthA);
                               s := abs(pA^);
                               dec(pA);
                               dec(PByte(pA), LineWidthA);
                               s := s + abs(pA^);
                               x := 0.75*s;
                               y := x;
                               w := -0.4375*sqr(s);
                          end;
                          inc(its);

                          // form shift and the look for 2 consecutive small subdiagonal elements
                          pAm := A;
                          inc(pAm, nn - 2);
                          inc(PByte(pAm), (nn - 2)*LineWidthA);
                          m := nn - 2;
                          while m >= l do
                          begin
                               z := pAm^;
                               r := x - z;
                               s := y - z;

                               pAi := pAm;
                               inc(pAi);
                               pAl := pAm;
                               inc(PByte(pAl), LineWidthA);
                               p := (r*s - w)/pAl^ + pAi^;
                               inc(pAl);
                               q := pAl^ - z - r - s;
                               inc(PByte(pAl), LineWidthA);
                               r := pAl^;
                               // scale to prevent under- or overflow
                               s := abs(p) + abs(q) + abs(r);
                               p := p/s;
                               q := q/s;
                               r := r/s;

                               if m = l then
                                  break;

                               pAi := pAm;
                               dec(pAi);
                               u := abs(pAi^)*(abs(q) + abs(r));
                               dec(PByte(pAi), LineWidthA);
                               dec(PByte(pAl), LineWidthA);
                               v := abs(p)*(abs(pAi^) + abs(z) + abs(pAl^));

                               if u + v = v then
                                  break;

                               dec(pAm);
                               dec(PByte(pAm), LineWidthA);
                               dec(m);
                          end;

                          pAi := A;
                          inc(pAi, m);
                          inc(PByte(pAi), (m + 2)*LineWidthA);
                          for i := m + 2 to nn do
                          begin
                               pAi^ := 0;
                               if i <> m + 2 then
                               begin
                                    dec(pAi);
                                    pAi^ := 0;
                                    inc(pAi);
                               end;

                               inc(pAi);
                               inc(PByte(pAi), LineWidthA);
                          end;

                          // double QR step on rows l to nn and columns m to nn
                          pAk := A;
                          inc(pAk, m - 1);
                          inc(PByte(pAk), m*LineWidthA);
                          for k := m to nn - 1 do
                          begin
                               if k <> m then
                               begin
                                    p := pAk^;
                                    inc(PByte(pAk), LineWidthA);
                                    q := pAk^;
                                    r := 0;
                                    if k <> nn - 1 then
                                    begin
                                         inc(PByte(pAk), LineWidthA);
                                         r := pAk^;
                                         dec(PByte(pAk), LineWidthA);
                                    end;

                                    x := abs(p) + abs(q) + abs(r);
                                    if x <> 0 then
                                    begin
                                         p := p/x;
                                         q := q/x;
                                         r := r/x;
                                    end;
                                    dec(PByte(pAk), LineWidthA);
                               end;

                               s := sign(sqrt(sqr(p) + sqr(q) + sqr(r)), p);
                               if s <> 0 then
                               begin
                                    if k = m then
                                    begin
                                         if l <> m then
                                            pAk^ := -pAk^;
                                    end
                                    else
                                        pAk^ := -s*x;

                                    p := p + s;
                                    x := p/s;
                                    y := q/s;
                                    z := r/s;
                                    q := q/p;
                                    r := r/p;

                                    // Row modification
                                    pAj := pAk;
                                    inc(pAj);
                                    for j := k to nn do
                                    begin
                                         p := pAj^;
                                         inc(PByte(pAj), LineWidthA);
                                         p := p + q*pAj^;

                                         if k <> nn - 1 then
                                         begin
                                              inc(PByte(pAj), LineWidthA);
                                              p := p + r*pAj^;
                                              pAj^ := pAj^ - p*z;
                                              dec(PByte(pAj), LineWidthA);
                                         end;

                                         pAj^ := pAj^ - p*y;
                                         dec(PByte(pAj), LineWidthA);
                                         pAj^ := pAj^ - p*x;
                                         inc(pAj);
                                    end;

                                    // column modification
                                    if nn < k + 3
                                    then
                                        mmin := nn
                                    else
                                        mmin := k + 3;
                                    pAi := A;
                                    inc(pAi, k);
                                    inc(PByte(pAi), l*LineWidthA);
                                    for i := l to mmin do
                                    begin
                                         p := x*pAi^;
                                         inc(pAi);
                                         p := p + y*pAi^;
                                         if k <> nn - 1 then
                                         begin
                                              inc(pAi);
                                              p := p + z*pAi^;
                                              pAi^ := pAi^ - p*r;
                                              dec(pAi);
                                         end;
                                         pAi^ := pAi^ - p*q;
                                         dec(pAi);
                                         pAi^ := pAi^ - p;
                                         inc(PByte(pAi), LineWidthA);
                                    end;
                               end;

                               inc(pAk);
                               inc(PByte(pAk), LineWidthA);
                          end;
                     end;
                end;
          until l >= nn - 1;
     end;

     Result := qlOk;
end;

procedure MatrixInitEivecHess(hess : PDouble; const LineWidthHess : NativeInt; width : NativeInt; dest : PDouble; const LineWidthDest : NativeInt; perm : PInteger; const LineWidthPerm : NativeInt);
var i, j, k : NativeInt;
    pDest, pDesti, pDestj : PDouble;
    pPerm : PInteger;
    pHess : PDouble;
    mp : NativeInt;
begin
     pPerm := Perm;
     inc(PByte(pPerm), (width - 2)*LineWidthPerm);
     for mp := width - 2 downto 1 do
     begin
          pDest := dest;
          inc(PByte(pDest), (mp + 1)*LineWidthDest);
          inc(pDest, mp);

          pHess := hess;
          inc(PByte(pHess), (mp + 1)*LineWidthHess);
          inc(pHess, mp - 1);
          
          
          for k := mp + 1 to width - 1 do
          begin
               pDest^ := pHess^;
               inc(PByte(pDest), LineWidthDest);
               inc(PByte(pHess), LineWidthHess);
          end;

          i := pPerm^;

          if i <> mp then
          begin
               pDestj := Dest;
               inc(PByte(PDestj), mp*LineWidthDest);
               inc(pDestj, mp);

               pDesti := Dest;
               inc(PByte(PDesti), i*LineWidthDest);
               inc(pDesti, mp);
               for j := mp to width - 1 do
               begin
                    pDestj^ := pDesti^;
                    pDesti^ := 0;
                    inc(pDestj);
                    inc(pDesti);
               end;
               pDesti := Dest;
               inc(PByte(PDesti), i*LineWidthDest);
               inc(pDesti, mp);
               pDesti^ := 1;
          end;

          dec(PByte(pPerm), LineWidthPerm);
     end;
end;

procedure MatrixNormEivecInPlace(Eivec : PDouble; const LineWidthEivec : NativeInt; width : NativeInt; WI : PDouble;
  const LineWidthWI : NativeInt; normalizeLen : boolean);
var i, j : NativeInt;
    pWi : PDouble;
    pEivec : PDouble;
    pTmp : PDouble;
    pEivecCpl : PComplex;
    maxVal : double;
    t : TComplex;
begin
     assert(width > 0, 'Dimension error');
     assert(width*sizeof(Double) <= LineWidthEivec, 'Dimension error');
     assert(LineWidthWI >= sizeof(double), 'Dimension error');

     if normalizeLen then
     begin
          // result of MatrixEigVecHessenbergInPlace is not normalized
          // -> normalize to length of 1
          i := 0;
          while i < width do
          begin
               if PConstDoubleArr(WI)^[i] = 0 then
               begin
                    MatrixNormalize(Eivec, LineWidthEiVec, EiVec, LineWidthEiVec, 1, width, False );
                    inc(Eivec);
                    inc(i);
               end
               else
               begin
                    // complex eigenvector: two eigenvalues (conjugate complex) point to the same eigenvector
                    CplxGenericMtxNormalize(PComplex( Eivec ), LineWidthEivec,
                                            PComplex( Eivec ), LineWidthEivec, 1, width, False );
                    inc(Eivec, 2);
                    inc(i, 2);
               end;
          end;
     end
     else
     begin
          // normalize the largest element to 1
          pWi := WI;
          pEivec := Eivec;
          j := 0;
          while j < width do
          begin
               if pWi^ = 0 then
               begin
                    maxVal := pEivec^;
                    ptmp := pEivec;
                    inc(PByte(pTmp), LineWidthEivec);
                    for i := 1 to width - 1 do
                    begin
                         if abs(maxVal) < abs(pTmp^) then
                            maxVal := pTmp^;

                         inc(PByte(pTmp), LineWidthEivec);
                    end;

                    if maxVal <> 0 then
                       MatrixScaleAndAdd(pEivec, LineWidthEivec, 1, width, 0, 1/maxVal);
               end
               else
               begin
                    pEivecCpl := PComplex(pEivec);
                    t := pEivecCpl^;

                    inc(PByte(pEivecCpl), LineWidthEivec);

                    for i := 1 to width - 1 do
                    begin
                         t := CMax(pEivecCpl^ , t );

                         inc(PByte(pEivecCpl), LineWidthEivec);
                    end;

                    if  (t.real <> 0) or (t.imag <> 0) then
                    begin
                         pEivecCpl := PComplex( pEivec );
                         for i := 0 to width - 1 do
                         begin
                              pEivecCpl^ := CDiv( pEivecCpl^, t);
                              inc(PByte(pEivecCpl), LineWidthEivec);
                         end;
                    end;
                    inc(pEivec);
                    inc(PByte(pWi), LineWidthWI);
                    inc(j);
               end;

               inc(pEivec);
               inc(PByte(pWi), LineWidthWI);
               inc(j);
          end;
     end;
end;

procedure Cdiv(const Ar, Ai, Br, Bi : double; var Cr, Ci : double); {$IFNDEF FPC} {$IF CompilerVersion >= 17.0} inline; {$IFEND} {$ENDIF}
{ Complex division, (Cr,Ci) = (Ar,Ai)/(Br,Bi) }
var tmp : double;
    brt : double;
    bit : double;
begin
     if abs(br) > abs(bi) then
     begin
          tmp := bi / br;
          brt := tmp * bi + br;
          cr := (ar + tmp * ai) / brt;
          ci := (ai - tmp * ar) / brt
     end
     else
     begin
          tmp := br / bi;
          bit := tmp * br + bi;
          cr := (tmp * ar + ai) / bit;
          ci := (tmp * ai - ar) / bit
    end;
end;

// hqr2
function MatrixEigVecHessenbergInPlace(A : PDouble; const LineWidthA : NativeInt; width : NativeInt; WR : PDouble;
 const LineWidthWR : NativeInt; WI : PDouble; const LineWidthWI : NativeInt; Eivec : PDouble; const LineWidthEivec : NativeInt) : TEigenvalueConvergence;
var nn, m, l, k : NativeInt;
    j, its, i, mmin : NativeInt;
    x, y, z : double;
    u, v, w : double;
    r, s, t : double;
    q, p, anorm : double;
    ra, sa : double;
    pAi, pAj, pAl, pA, pAm, pAk : PDouble;
    pEivecj, pEiveck, pEiveci : PDouble;
    pWr, pWri : PDouble;
    pWi, pWii : PDouble;
    vr : double;
    vi : double;
    na : NativeInt;
const cMaxEigHessenbergIter = 30;
      cEPS = 2.22e-16;
begin
     Assert(width > 0, 'Dimension Error');
     Assert(LineWidthA >= width*sizeof(double), 'Dimension error');
     Assert(LineWidthEivec >= width*sizeof(double), 'Dimension error');
     Assert(LineWidthWR >= sizeof(double), 'Dimension error');
     Assert(LineWidthWI >= sizeof(double), 'Dimension error');

     r := 0;
     p := 0;
     q := 0;
     Result := qlNoConverge;
     anorm := 0;
     pAi := A;
     // compute matrix norm for possible use in locating single small subdiagonal element
     for i := 0 to width - 1 do
     begin
          pAj := pAi;
          inc(pAj, max(0, i - 1));
          for j := Max(0, i - 1) to width - 1 do
          begin
               anorm := anorm + abs(pAj^);

               inc(pAj);
          end;

          inc(PByte(pAi), LineWidthA);
     end;

     if anorm = 0 then
     begin
          Result := qlMatrixError;
          exit;
     end;
     
     nn := width - 1;
     t := 0;
     // gets changed only by an exceptional shift.
     while nn >= 0 do
     begin
          // begin search of the next eigenvalue
          its := 0;
          repeat
                // begin iteration: look for single small subdiagonal element.
                pAl := A;
                inc(pAl, nn);
                inc(PByte(pAl), nn*LineWidthA);
                l := nn;
                while l >= 1 do
                begin
                     s := abs(pAl^);
                     dec(pAl);
                     dec(PByte(pAl), LineWidthA);
                     s := s + abs(pAl^);

                     if s = 0 then
                        s := anorm;

                     inc(PByte(pAl), LineWidthA);

                     if abs(pAl^) <= cEPS*s then
                     begin
                          pAl^ := 0;
                          break;
                     end;

                     dec(PByte(pAl), LineWidthA);
                     dec(l);
                end;

                pA := A;
                inc(pA, nn);
                inc(PByte(pA), nn*LineWidthA);
                x := pA^;
                // one root found
                if l = nn then
                begin
                     pWr := WR;
                     inc(PByte(pWr), nn*LineWidthWR);
                     pWr^ := x + t;
                     pA^ := pWr^;
                     pWi := WI;
                     inc(PByte(pWi), nn*LineWidthWI);
                     pWi^ := 0;
                     dec(nn);
                end
                else
                begin
                     dec(pA);
                     dec(PByte(pA), LineWidthA);
                     y := pA^;
                     inc(pA);
                     w := pA^;
                     dec(pA);
                     inc(PByte(pA), LineWidthA);
                     w := w*pA^;
                     // two roots found ...
                     if l = nn - 1 then
                     begin
                          p := 0.5*(y - x);
                          q := sqr(p) + w;
                          z := sqrt(abs(q));
                          x := x + t;

                          pA := A;
                          inc(pA, nn);
                          inc(PByte(pA), nn*LineWidthA);
                          pA^ := x;
                          dec(pA);
                          dec(PByte(pA), LineWidthA);
                          pA^ := y + t;

                          pWr := WR;
                          inc(PByte(pWr), (nn - 1)*LineWidthWR);
                          pWi := WI;
                          inc(PByte(pWi), (nn - 1)*LineWidthWI);

                          // ... a real pair
                          if q >= 0 then
                          begin
                               z := p + sign(z, p);

                               pWr^ := x + z;
                               inc(PByte(pWr), LineWidthWR);
                               pWr^ := x + z;
                               if z <> 0 then
                                  pWr^ := x - w/z;

                               pWi^ := 0;
                               inc(PByte(pWi), LineWidthWI);
                               pWi^ := 0;

                               // update eigenvectors
                               inc(PByte(pA), LineWidthA);
                               x := pA^;
                               s := abs(x) + abs(z); 
                               p := x/s;
                               q := z/s;
                               r := sqrt(sqr(p) + sqr(q));

                               q := q/r;
                               p := p/r;
                               
                               pAk := pA;     // pA := @a[nn][nn-1]
                               pAj := pA;
                               dec(PByte(pAj), LineWidthA);

                               // row modification
                               for j := nn - 1 to width - 1 do
                               begin
                                    z := pAj^;
                                    pAj^ := q*z + p*pAk^;
                                    pAk^ := q*pAk^ - p*z;
                                    inc(pAk);
                                    inc(pAj);
                               end;

                               // column modification
                               pAj := A;
                               inc(pAj, nn);
                               pAk := pAj;
                               dec(pAk);
                               for i := 0 to nn do
                               begin
                                    z := pAk^;
                                    pAk^ := q*z + p*pAj^;
                                    pAj^ := q*pAj^ - p*z;
                                    inc(PByte(pAk), LineWidthA);
                                    inc(PByte(pAj), LineWidthA);
                               end;

                               // accumulate transformations
                               pEivecj := Eivec;
                               inc(pEivecj, nn - 1);
                               pEiveck := pEivecj;
                               inc(pEiveck);
                               for i := 0 to width - 1 do
                               begin
                                    z := pEivecj^;
                                    pEivecj^ := q*z + p*pEiveck^;
                                    pEiveck^ := q*pEiveck^ - p*z;
                                    inc(PByte(pEivecj), LineWidthEivec);
                                    inc(PByte(pEiveck), LineWidthEivec);
                               end;
                          end
                          else
                          begin
                               // ...a complex pair
                               pWr^ := x + p;
                               inc(PByte(pWr), LineWidthWR);
                               pWr^ := x + p;
                               pWi^ := z;
                               inc(PByte(pWi), LineWidthWI);
                               pWi^ := -z;
                          end;

                          dec(nn, 2);
                     end
                     else
                     begin
                          // no roots found. Continue iteration
                          if its = cMaxEigHessenbergIter then
                             exit;

                          // form exceptional shift
                          if (its = 10) or (its = 20) then
                          begin
                               t := t + x;
                               pAi := A;
                               for i := 0 to nn do
                               begin
                                    pAi^ := pAi^ - x;
                                    inc(pAi);
                                    inc(PByte(pAi), LineWidthA);
                               end;

                               pA := A;
                               inc(pA, nn - 1);
                               inc(PByte(pA), nn*LineWidthA);
                               s := abs(pA^);
                               dec(pA);
                               dec(PByte(pA), LineWidthA);
                               s := s + abs(pA^);
                               x := 0.75*s;
                               y := x;
                               w := -0.4375*sqr(s);
                          end;
                          inc(its);

                          // form shift and the look for 2 consecutive small subdiagonal elements
                          pAm := A;
                          inc(pAm, nn - 2);
                          inc(PByte(pAm), (nn - 2)*LineWidthA);
                          m := nn - 2;
                          while m >= l do
                          begin
                               z := pAm^;
                               r := x - z;
                               s := y - z;

                               pAi := pAm;
                               inc(pAi);
                               pAl := pAm;
                               inc(PByte(pAl), LineWidthA);
                               p := (r*s - w)/pAl^ + pAi^;
                               inc(pAl);
                               q := pAl^ - z - r - s;
                               inc(PByte(pAl), LineWidthA);
                               r := pAl^;
                               // scale to prevent under- or overflow
                               s := abs(p) + abs(q) + abs(r);
                               p := p/s;
                               q := q/s;
                               r := r/s;

                               if m = l then
                                  break;

                               pAi := pAm;
                               dec(pAi);
                               u := abs(pAi^)*(abs(q) + abs(r));
                               dec(PByte(pAi), LineWidthA);
                               dec(PByte(pAl), LineWidthA);
                               v := abs(p)*(abs(pAi^) + abs(z) + abs(pAl^));

                               if u <= cEPS*v then
                                  break;

                               dec(pAm);
                               dec(PByte(pAm), LineWidthA);
                               dec(m);
                          end;

                          pAi := A;
                          inc(pAi, m);
                          inc(PByte(pAi), (m + 2)*LineWidthA);
                          for i := m to nn - 2 do
                          begin
                               pAi^ := 0;
                               if i <> m then
                               begin
                                    dec(pAi);
                                    pAi^ := 0;
                                    inc(pAi);
                               end;

                               inc(pAi);
                               inc(PByte(pAi), LineWidthA);
                          end;

                          // double QR step on rows l to nn and columns m to nn
                          pAk := A;
                          inc(pAk, m - 1);
                          inc(PByte(pAk), m*LineWidthA);
                          for k := m to nn - 1 do
                          begin
                               if k <> m then
                               begin
                                    p := pAk^;
                                    inc(PByte(pAk), LineWidthA);
                                    q := pAk^;
                                    r := 0;
                                    if k + 1 <> nn then
                                    begin
                                         inc(PByte(pAk), LineWidthA);
                                         r := pAk^;
                                         dec(PByte(pAk), LineWidthA);
                                    end;

                                    x := abs(p) + abs(q) + abs(r);

                                    if x <> 0 then
                                    begin
                                         p := p/x;
                                         q := q/x;
                                         r := r/x;
                                    end;

                                    dec(PByte(pAk), LineWidthA);
                               end;

                               s := sign(sqrt(p * p + q * q + r * r), p);

                               if s <> 0 then
                               begin
                                    if k = m then
                                    begin
                                         if l <> m then
                                            pAk^ := -pAk^;
                                    end
                                    else
                                        pAk^ := -s*x;

                                    p := p + s;
                                    x := p/s;
                                    y := q/s;
                                    z := r/s;
                                    q := q/p;
                                    r := r/p;

                                    // Row modification
                                    pAj := A;
                                    inc(PByte(pAj), k*LineWidthA);
                                    inc(pAj, k);
                                    for j := k to width - 1 do
                                    begin
                                         p := pAj^;
                                         inc(PByte(pAj), LineWidthA);
                                         p := p + q*pAj^;

                                         if k + 1 <> nn then
                                         begin
                                              inc(PByte(pAj), LineWidthA);
                                              p := p + r*pAj^;
                                              pAj^ := pAj^ - p*z;
                                              dec(PByte(pAj), LineWidthA);
                                         end;

                                         pAj^ := pAj^ - p*y;
                                         dec(PByte(pAj), LineWidthA);
                                         pAj^ := pAj^ - p*x;
                                         inc(pAj);
                                    end;

                                    // column modification
                                    if nn < k + 3
                                    then
                                        mmin := nn
                                    else
                                        mmin := k + 3;
                                    pAi := A;
                                    inc(pAi, k);
                                    for i := 0 to mmin do
                                    begin
                                         p := x*pAi^;
                                         inc(pAi);
                                         p := p + y*pAi^;
                                         if k + 1 <> nn then
                                         begin
                                              inc(pAi);
                                              p := p + z*pAi^;
                                              pAi^ := pAi^ - p*r;
                                              dec(pAi);
                                         end;
                                         pAi^ := pAi^ - p*q;
                                         dec(pAi);
                                         pAi^ := pAi^ - p;
                                         inc(PByte(pAi), LineWidthA);
                                    end;

                                    // Accumulate transformations
                                    pEiveck := Eivec;
                                    inc(pEiveck, k);
                                    for i := 0 to width - 1 do
                                    begin
                                         p := x*pEiveck^;
                                         inc(pEiveck);
                                         p := p + y*pEiveck^;
                                         if k + 1 <> nn then
                                         begin
                                              inc(pEiveck);
                                              p := p + z*pEiveck^;
                                              pEiveck^ := pEiveck^ - p*r;
                                              dec(pEiveck);
                                         end;
                                         pEiveck^ := pEiveck^ - p*q;
                                         dec(pEiveck);
                                         pEiveck^ := pEiveck^ - p;
                                         inc(PByte(pEiveck), LineWidthEivec);
                                    end;
                               end;

                               inc(pAk);
                               inc(PByte(pAk), LineWidthA);
                          end;
                     end;
                end;
          until l >= nn - 1;
     end;

     // #########################################
     // #### All roots found. Backsubstitute to find vectors of upper triangular form.
     Result := qlOk;
     r := 0;
     s := 0;
     z := 0;
     pWr := Wr;
     pWi := Wi;
     inc(PByte(pWr), (width - 1)*LineWidthWR);
     inc(PByte(pWi), (width - 1)*LineWidthWI);
     for nn := width - 1 downto 0 do
     begin
          p := pWr^;
          q := pWi^;

          na := nn - 1;
          
          if q = 0 then
          begin
               m := nn;
               pAi := A;
               inc(PByte(pAi), nn*LineWidthA);
               inc(pAi, nn);
               pAi^ := 1;

               pWii := pWi;
               dec(PByte(pWii), LineWidthWI);
               pWri := pWr;
               dec(PByte(pWri), LineWidthWR);
               for i := nn - 1 downto 0 do
               begin
                    pAi := A;
                    inc(PByte(pAi), i*LineWidthA);
                    inc(pAi, i);
                    r := 0;
                    w := pAi^ - p;

                    pAj := A;
                    inc(PByte(pAj), i*LineWidthA);
                    inc(pAj, m);

                    pAk := A;
                    inc(PByte(pAk), m*LineWidthA);
                    inc(pAk, nn);
                    for j := m to nn do
                    begin
                         r := r + pAj^*pAk^;
                         inc(pAj);
                         inc(PByte(pAk), LineWidthA);
                    end;

                    if pWii^ < 0 then
                    begin
                         z := w;
                         s := r;
                    end
                    else
                    begin
                         m := i;

                         if pWii^ = 0 then
                         begin
                              t := w;
                              if t = 0 then
                                 t := cEPS*anorm;
                              pA := A;
                              inc(PByte(pA), i*LineWidthA);
                              inc(pA, nn);
                              pA^ := -r/t;
                         end
                         else
                         begin
                              {Solve the linear system:
                              | w   x |  | h[i][nn]   |   | -r |
                              |       |  |            | = |    |
                              | y   z |  | h[i+1][nn] |   | -s |  }
                              pA := A;
                              inc(PByte(pA), i*LineWidthA);
                              inc(pA, i + 1);
                              x := pA^;
                              dec(pA);
                              inc(PByte(pA), LineWidthA);
                              y := pA^;
                              q := sqr(pWri^ - p) + sqr(pWii^);
                              pA := A;
                              inc(PByte(pA), i*LineWidthA);
                              inc(pA, nn);
                              pA^ := (x*s - z*r)/q;
                              t := pA^;
                              inc(PByte(pA), LineWidthA);
                              if abs(x) > abs(z)
                              then
                                  pA^ := (-r - w*t)/x
                              else
                                  pA^ := (-s - y*t)/z;
                         end;

                         // overflow control
                         pA := A;
                         inc(PByte(pA), i*LineWidthA);
                         inc(pA, nn);
                         t := abs(pA^);

                         if cEPS*sqr(t) > 1 then
                         begin
                              for j := i to nn do
                              begin
                                   pA^ := pA^/t;
                                   inc(PByte(pA), LineWidthA);
                              end;
                         end;
                    end;

                    dec(PByte(pWii), LineWidthWI);
                    dec(PByte(pWri), LineWidthWR);
               end;
          end // if q = 0
          else if q < 0 then    // complex vector, only do one case. Last vector chosen imaginary so that eigenvector matrix is triangular
          begin
               m := na;

               pAk := A;
               inc(pAk, nn);
               inc(PByte(pAk), na*LineWidthA);
               pA := A;
               inc(pA, na);
               pAi := pA;
               inc(PByte(pA), (nn)*LineWidthA);
               inc(PByte(pAi), na*LineWidthA);

               pAj := A;
               inc(PByte(pAj), nn*LineWidthA);
               inc(pAj, nn);

               if abs(pA^) > abs(pAk^) then
               begin
                    pAi^ := -q/pA^;
                    pAk^ := -(pAj^ - p)/pA^;
               end
               else
                   Cdiv(0, -pAk^, pAi^ - p, q, pAi^, pAk^);

               pA^ := 0;
               pAj^ := 1;

               pAi := A;
               inc(PByte(pAi), (nn - 2)*LineWidthA);

               pWri := Wr;
               inc(PByte(pWri), (nn - 2)*LineWidthWR);
               pWii := Wi;
               inc(PByte(pWii), (nn - 2)*LineWidthWI);
               for i := nn - 2 downto 0 do
               begin
                    pA := pAi;
                    inc(pA, i);
                    w := pA^ - p;

                    ra := 0;
                    sa := 0;
                    
                    pA := A;
                    inc(PByte(pA), m*LineWidthA);
                    pAk := pA;
                    inc(pA, na);
                    inc(pAk, nn);
                    
                    pAm := pAi;
                    inc(pAm, m);

                    for j := m to nn do
                    begin
                         ra := ra + pAm^*pA^;
                         sa := sa + pAm^*pAk^;

                         inc(pAm);
                         inc(PByte(pA), LineWidthA);
                         inc(PByte(pAk), LineWidthA);
                    end;

                    if pWii^ < 0 then
                    begin
                         z := w;
                         r := ra;
                         s := sa
                    end
                    else
                    begin
                         m := i;

                         pA := pAi;
                         inc(pA, na);
                         pAk := pAi;
                         inc(pAk, nn);

                         if pWii^ = 0
                         then
                             Cdiv(-ra, -sa, w, q, pA^, pAk^)
                         else
                         begin
                              {solve complex linear system:
                              | w+i*q     x | | h[i][na] + i*h[i][en]  |   | -ra+i*sa |
                              |             | |                        | = |          |
                              |   y    z+i*q| | h[i+1][na]+i*h[i+1][en]|   | -r+i*s   |  }
                              pAk := pAi;
                              inc(pAk, i + 1);
                              pAm := pAi;
                              inc(PByte(pAm), LineWidthA);
                              inc(pAm, i);
                              x := pAk^;
                              y := pAm^;

                              vr := sqr(pWri^ - p) + sqr(pWii^) - sqr(q);
                              vi := 2*q*(pWri^ - p);
                              if (vr = 0) and (vi = 0) then
                                 vr := cEPS*anorm*(abs(w) + abs(q) + abs(x) + abs(y) + abs(z));

                              pAk := pAi;
                              inc(pAk, nn);
                              pAm := pAi;
                              inc(pAm, na);
                              CDiv(x*r - z*ra + q*sa, x*s - z*sa - q*ra, vr, vi, pAm^, pAk^);

                              pAl := pAm;
                              inc(PByte(pAl), LineWidthA);
                              pAj := pAk;
                              inc(PByte(pAj), LineWidthA);

                              if abs(x) > abs(z) + abs(q) then
                              begin
                                   pAl^ := (-ra - w*pAm^ + q*pAk^)/x;
                                   pAj^ := (-sa - w*pAk^ - q*pAm^)/x;
                              end
                              else
                                  Cdiv(-r - y*pAm^, -s - y*pAk^, z, q, pAl^, pAj^);
                         end;
                    end;

                    // overflow control
                    pAm := pAi;
                    inc(pAm, na);
                    pAl := pAi;
                    inc(pAl, nn);
                    t := max(Abs(pAm^), Abs(pAl^));

                    if cEps*sqr(t) > 1 then
                    begin
                         for j := i to nn do
                         begin
                              pAm^ := pAm^/t;
                              pAl^ := pAl^/t;
                              inc(PByte(pAm), LineWidthA);
                              inc(PByte(pAl), LineWidthA);
                         end;
                    end;

                    dec(PByte(pWii), LineWidthWI);
                    dec(PByte(pWri), LineWidthWR);
                    dec(PByte(pAi), LineWidthA);
               end;
          end;

          dec(PByte(pWr), LineWidthWR);
          dec(PByte(pWi), LineWidthWI);
     end;


     // multiply by transformation matrix to give vectors of original full matrix
     for j := width - 1 downto 0 do
     begin
          pEiveci := Eivec;
          for i := 0 to width - 1 do
          begin
               z := 0;

               pEivecj := pEiveci;

               pAj := A;
               inc(pAj, j);
               for k := 0 to j do
               begin
                    z := z + pEivecj^*pAj^;
                    inc(PByte(pAj), LineWidthA);
                    inc(pEivecj);
               end;
               
               pEivecj := pEiveci;
               inc(pEivecj, j);
               pEivecj^ := z;

               inc(PByte(pEiveci), LineWidthEivec);
          end;
     end;
end;

procedure MatrixHessenbergInPlace(A : PDouble; const LineWidthA : NativeInt; width : NativeInt);
begin
     MatrixHessenbergPermInPlace(A, LineWidthA, width, nil, 0);
end;

procedure MatrixHessenberg(dest : PDouble; const LineWidthDest : NativeInt; A : PDouble; const LineWidthA : NativeInt; width : NativeInt);
begin
     MatrixHessenbergPerm(dest, LineWidthDest, A, LineWidthA, width, nil, 0);
end;

// hqr2
function MatrixUnsymEigVecInPlace(A : PDouble; const LineWidthA : NativeInt; width : NativeInt; WR : PDouble;
  const LineWidthWR : NativeInt; WI : PDouble; const LineWidthWI : NativeInt; Eivec : PDouble; const LineWidthEivec : NativeInt) : TEigenvalueConvergence;
var perm : array of integer;
    scale : Array of double;
    pDest : PDouble;
    i: NativeInt;
begin
     setLength(perm, width);
     setLength(scale, width);

     // determine upper hessenberg matrix
     MatrixHessenbergPermInPlace(A, LineWidthA, width, @perm[0], sizeof(integer));

     // initialize to unit matrix -> it's assumed that the eigenvector matrix is already zeroed out!
     pDest := Eivec;
     for i := 0 to width - 1 do
     begin
          pDest^ := 1;
          inc(pDest);
          inc(PByte(pDest), LineWidthEivec);
     end;

     MatrixInitEivecHess(A, LineWidthA, width, Eivec, LineWidthEivec, @perm[0], sizeof(integer));

     // calculate eigenvalues and eigenvectors from the hessenberg matrix
     Result := MatrixEigVecHessenbergInPlace(A, LineWidthA, width, Wr, LineWidthWR, Wi, LineWidthWI, Eivec, LineWidthEivec);

     if Result <> qlOk then
        exit;
end;

function MatrixUnsymEigVec(const A : PDouble; const LineWidthA : NativeInt; width : NativeInt; WR : PDouble;
 const LineWidthWR : NativeInt; WI : PDouble; const LineWidthWI : NativeInt; Eivec : PDouble; const LineWidthEivec : NativeInt) : TEigenvalueConvergence;
var dest : Array of double;
    pA : Pdouble;
    i : NativeInt;
begin
     SetLength(dest, width*width);
     pA := A;

     for i := 0 to width - 1 do
     begin
          Move(pA^, dest[i*width], width*sizeof(double));
          inc(PByte(pA), LineWidthA);
     end;

     Result := MatrixUnsymEigVecInPlace(@dest[0], width*sizeof(double), width, WR, LineWidthWR, WI, LineWidthWI, Eivec, LineWidthEivec);
end;

function MatrixUnsymEigInPlace(A : PDouble; const LineWidthA : NativeInt; width : NativeInt; WR : PDouble;
 const LineWidthWR : NativeInt; WI : PDouble; const LineWidthWI : NativeInt; balance : boolean) : TEigenvalueConvergence;
var scale : Array of double;
begin
     setLength(scale, width);
     if Balance then
        MatrixBalanceInPlace(A, LineWidthA, width, @scale[0], sizeof(double));

     MatrixHessenbergInPlace(A, LineWidthA, width);

     // check eigenvalues on hessenberg matrix
     Result := MatrixEigHessenbergInPlace(A, LineWidthA, width, Wr, LineWidthWR, Wi, LineWidthWI);
end;

function MatrixUnsymEig(const A : PDouble; const LineWidthA : NativeInt; width : NativeInt; WR : PDouble;
 const LineWidthWR : NativeInt; WI : PDouble; const LineWidthWI : NativeInt; balance : boolean) : TEigenvalueConvergence;
var dest : Array of double;
    pA : Pdouble;
    i : NativeInt;
begin
     SetLength(dest, width*width);
     pA := A;

     for i := 0 to width - 1 do
     begin
          Move(pA^, dest[i*width], width*sizeof(double));
          inc(PByte(pA), LineWidthA);
     end;

     Result := MatrixUnsymEigInPlace(@dest[0], width*sizeof(double), width, WR, LineWidthWR, WI, LineWidthWI, balance);
end;


// ###########################################
// #### LAPCK Hessenberg
// ###########################################

type
  THessData = record
    mem : Pointer;
    work : PDouble;
    PnlSize : integer;

    reflData : TBlockReflrec; 
    
    // helper pointers
    T : PDouble;       // point to work // (w x h) = (pnlsize + 1 x pnlsize)
    Y : PDouble;       // point to T + tlinewidth*(pnlSize)  (w x h) = (pnlsize x height) 
    tLineWidth : NativeInt;      // pnlsize*sizeof(double)  
    yLineWidth : NativeInt;      // pnlsize*sizeof(double)
    blkMultMem : Pointer; // point to Y + N * NB * sizeof(Double)
    blkMultSize : integer;
  end;

procedure InitReflectorBlk( var eigData : THessData );
begin
     eigData.reflData.T := eigData.T;
     eigData.reflData.LineWidthT := eigData.tLineWidth;
     eigData.reflData.BlkMultMem := eigData.BlkMultMem;
     eigData.reflData.BlkMultSize := eigData.blkMultSize;
end;

// single threaded work mem need:
function EigMemSize(const hessData : THessData; width : NativeInt) : NativeInt;
var w4 : NativeInt;
begin
     // for factors of 4.. (better aligned memory)
     w4 := width;
     if w4 and $03 <> 0 then
        w4 := width + 4 - width and $03;

     // + 64 to get aligned ptr for both Y and multiplication (and rank1 updates)
     Result := 64 + Max(2*w4*sizeof(double), hessData.PnlSize*w4*sizeof(double) +  // Y
               hessData.PnlSize*(hessData.PnlSize + 1)*sizeof(double) +  // T
               BlockMultMemSize( hessData.blkMultSize ) );  // mult
end;

// unblocked hessenberg decomposition based on DGEHD2
procedure InternalHessenbergUnblocked( A : PDouble; const LineWidthA : NativeInt; iLo : NativeInt; width : NativeInt; tau : PDouble; const eigData : THessData );
var i : NativeInt;
    pA : PDouble;
    pTau : PDouble;
    aii : double;
    pAlpha : PDouble;
    pC : PDouble;
begin
     pTau := Tau;
     for i := iLo to width - 2 do
     begin
          // Generate elemetary reflector H(i) to  annihilate A(i+2:ihi,i)
          pAlpha := GenPtr(A, i, i + 1, LineWidthA);
          pA := GenPtr(A, i, min(i + 2, width - 1), LineWidthA);

          GenElemHousholderRefl(pA, LineWidthA, width - i - 1, pAlpha^, pTau);

          // Apply H(i) to A(1:ihi,i+1:ihi) from the right
          aii := pAlpha^;
          pAlpha^ := 1;

          pC := GenPtr(A, i + 1, 0, LineWidthA);
          ApplyElemHousholderReflRight(pAlpha, LineWidthA, pC, LineWidthA, width - i - 1, width, pTau, eigData.work);

          // Apply H(i) to A(i+1:ihi,i+1:n) from the left
          pC := GenPtr(A, i + 1, i + 1, LineWidthA);
          ApplyElemHousholderReflLeft (pAlpha, LineWidthA, pC, LineWidthA, width - i - 1, width - i - 1, pTau, eigData.work);
          pAlpha^ := aii;

          inc(pTau);
     end;
end;


//  DLAHR2 reduces the first NB columns of A real general n-BY-(n-k+1)
//  matrix A so that elements below the k-th subdiagonal are zero. The
//  reduction is performed by an orthogonal similarity transformation
//  Q' * A * Q. The routine returns the matrices V and T which determine
//  Q as a block reflector I - V*T*V', and also the matrix Y = A * V * T.
//procedure dlahr2( A : PDouble; const LineWidthA : NativeInt; T : PDouble; const LineWidthT : NativeInt; Y : PDouble; const LineWidthY : NativeInt; Tau : PDouble; N, K, NB : integer; const eigData : THessData);
procedure InternalHessenbergBlocked( A : PDouble; const LineWidthA : NativeInt; Tau : PDouble; N, K, NB : integer; const eigData : THessData);
var i: Integer;
    pA : PDouble;
    pV : PDouble;
    pY : PDouble;
    pT : PDouble;
    ei : double;
    pb1 : PDouble;
    pb2 : PDouble;
    pV1 : PDouble;
    pV2 : PDouble;
    pW : PDouble;
    pALpha : PDouble;
    pA1 : PDouble;
    nki : Integer;
    //s : string;
begin
     // quick return
     if n <= 1 then
        exit;

     //s := WriteMtx( A, LineWidthA, 7, 7, 3 );
     ei := 0;
     pAlpha := nil;

     for i := 0 to NB - 1 do
     begin
          // N-K-I+1 -> height
          nki := N - k - i - 1;

          if i > 0 then
          begin
               // Let V = (V1)   b = (b1)  (first I - 1 rows)
               //         (V2)       (b2)
               // where V1 is unit lower triangular

               // Update A(K+1:N,I)
               // Update I-th column of A - Y * V'
               pA := GenPtr(A, i, K + 1, LineWidthA);    // A(k + 1, i )
               pV := GenPtr(A, 0, K + i, LineWidthA );   // A(k + i - 1, 1);
               pY := GenPtr(eigData.Y, 0, K + 1, eigData.yLineWidth );   // y(k + 1, 1)
               MatrixMtxVecMult( pA, LineWidthA, pY, pV, eigData.yLineWidth, sizeof(double), i, N-k-1, -1, 1 );

               //s := WriteMtx(A, LineWidthA, 2*nb, 5);

               // apply I - V*T'*V' to this column (call it b) from the left using the last column of T as workspace

               // w = V1'*b1
               pV1 := GenPtr(A, 0, k + 1, LineWidthA);
               pV2 := GenPtr(A, 0, k + i + 1, LineWidthA );

               pb1 := GenPtr(A, i, k + 1, LineWidthA );
               pb2 := GenPtr(A, i, k + i + 1, LineWidthA );

               pW := GenPtr(eigData.T, NB - 1, 0, eigData.tLineWidth );

               MatrixCopy( pW, eigData.tLineWidth, pb1, LineWidthA, 1, i );
               // s := WriteMtx(pW, LineWidthT, nb, 5);
               // orig dtrmv lower transpose unit
               MtxMultLowTranspUnitVec( pV1, LineWidthA, pW, eigData.tLineWidth, i );

               //s := WriteMtx(pW, LineWidthT, nb, 5);

               // w := w + V2'*b2
               MatrixMtxVecMultT(pW, eigData.tLineWidth, pV2, pB2, LineWidthA, LineWidthA, i, nki, 1, 1);
               //s := WriteMtx(pW, LineWidthT, nb, 5);

               // w := T'*w
               MtxMultUpTranspNoUnitVec( eigData.T, eigData.tLineWidth, pW, eigData.tLineWidth, i );
               //s := WriteMtx(pW, LineWidthT, nb, 5);

               // b2 = b2 - V2*w
               //MatrixMultEx(pb2, LineWidthA, pV2, pW, i, N - k - i, 1, i, LineWidthA, LineWidthT, BlockMatrixCacheSize, doSub, eigData.blkMultMem );
               MatrixMtxVecMult(pB2, LineWidthA, pV2, pW, LineWidthA, eigData.tLineWidth, i, nki, -1, 1);
               //s := WriteMtx(A, LineWidthA, 2*nb, 5);

               // b1 = b1 - V1*w
               MtxMultLowNoTranspUnitVec( pV1, LineWidthA, pW, eigData.tLineWidth, i );
               MatrixSub( pb1, LineWidthA, pb1, pW, 1, i, LineWidthA, eigData.tLineWidth );

               pAlpha^ := ei;
          end;

          // ###########################################
          // #### Elementary reflector H(i) to annihilate A(k + i + 1:N, i)
          pAlpha := GenPtr( A, i, K + i + 1, LineWidthA );
          pA := GenPtr( A, i, Min( K + i + 2, N - 1), LineWidthA );
          GenElemHousholderRefl( pA, LineWidthA, nki, pAlpha^, tau );
          ei := pAlpha^;
          pAlpha^ := 1;

          // compute y(k + 1:N, i )
          pA1 := GenPtr( A, i + 1, k + 1, LineWidthA );
          pY := GenPtr( eigData.Y, i, k + 1, eigData.yLineWidth );
          pT := GenPtr( eigData.T, i, 0, eigData.tLineWidth );

          MatrixMtxVecMult( pY, eigData.yLineWidth, pA1, pAlpha, LineWidthA, LineWidthA, nki, N - k - 1, 1, 0 );

          if i > 0 then
          begin
               MatrixMtxVecMultT( pT, eigData.tLineWidth, GenPtr( A, 0, k + i + 1, LineWidthA), pAlpha, LineWidthA, LineWidthA, i, nki, 1, 0 );
               MatrixMtxVecMult( pY, eigData.yLineWidth, GenPtr( eigData.Y, 0, k + 1, eigData.yLineWidth ), pT, eigData.yLineWidth, eigData.tLineWidth, i, N - k - 1, -1, 1 );
          end;
          MatrixScaleAndAdd( pY, eigData.yLineWidth, 1, N - k - 1, 0, tau^ );

          // compute T(1:i, i)
          if i > 0 then
          begin
               MatrixScaleAndAdd( pT, eigData.tLineWidth, 1, i, 0, -tau^ );
               MtxMultUpNoTranspNoUnitVec( eigData.T, eigData.tLineWidth, pT, eigData.tLineWidth, i );
          end;
          pT := GenPtr(eigData.T, i, i, eigData.tLineWidth );
          pT^ := tau^;

          inc(tau);
     end;

     //s := WriteMtx(eigData.Y, eigData.yLineWidth, nb, 5);

     pA := GenPtr( A, NB - 1, k + NB, LineWidthA );
     pA^ := ei;

     // compute Y(1:K, 1:NB)
     MatrixCopy( eigData.Y, eigData.yLineWidth, GenPtr(A, 1, 0, LineWidthA ), LineWidthA, NB, k + 1);
     //s := WriteMtx(eigData.Y, eigData.yLineWidth, nb, 5);
     MtxMultRightLowerTriaUnit( eigData.Y, eigData.yLineWidth, GenPtr(A, 0, k + 1, LineWidthA ), LineWidthA, NB, k + 1, NB, NB );
     //s := WriteMtx(eigData.Y, eigData.yLineWidth, nb, 5);
//        IF( n.GT.k+nb )
//      $   CALL dgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', k,
//      $               nb, n-k-nb, one,
//      $               a( 1, 2+nb ), lda, a( k+1+nb, 1 ), lda, one, y,
//      $               ldy )
     if n >= k + nb then
        eigData.reflData.MatrixMultEx( eigData.Y, eigData.yLineWidth,
                                       GenPtr(A, 1 + NB, 0, LineWidthA),
                                       GenPtr(A, 0, K + 1 + NB, LineWidthA),
                                       n - k - nb - 1, k + 1, nb, n - k - nb - 1,
                                       LineWidthA, LineWidthA,
                                       Min( eigData.blkMultSize, HessMultBlockSize), doAdd, eigData.blkMultMem );

     MtxMultRightUpperTriaNoUnit(eigData.Y, eigData.yLineWidth, eigData.T, eigData.tLineWidth, NB, k + 1, NB, NB);
end;


// original DGEHRD
procedure InternalMatrixHessenberg(A : PDouble; const LineWidthA : NativeInt; width : NativeInt; tau : PDouble; const eigData : THessData );
var i : integer;
    pA : PDouble;
    pTau : PDouble;
    ib : integer;
    ei : double;
    pAib : PDouble;
    pA1, pA2 : PDouble;
    j : Integer;
    //s, s1, s2 : string;
begin
     MtxMemInit( tau, width*sizeof(double), 0 );

     // ###########################################
     // #### blocked code
     i := 0;
     pTau := tau;
     while i + eigData.PnlSize + 2 < width do
     begin
          ib := eigData.PnlSize;

          // reduce colums i:i+pnlsize -1  to hessenberg form returning the matrices V and T of the block reflector H  I _ V*T*V'
          // which performs the reduction and also the matrix Y = A*V*T
          pA := GenPtr(A, i, 0, LineWidthA);
          //dlahr2( pA, LineWidthA, eigData.T, eigData.tLineWidth, eigData.Y, eigData.yLineWidth, pTau, width, i, ib, eigData );
          InternalHessenbergBlocked( pA, LineWidthA, pTau, width, i, ib, eigData );

         // s := WriteMtx(eigData.T, eigData.tLineWidth, ib, ib);
//          s2 := WriteMtx(eigData.Y, eigData.yLineWidth, ib, width - ib);
//          s1 := WriteMtx(A, LineWidthA, 5, 5);


          //  Apply the block reflector H to A(1:ihi,i+ib:ihi) from the
          // right, computing  A := A - Y * V'. V(i+ib,ib-1) must be set to 1
          pAib := GenPtr( A, i + ib - 1, i + ib, LineWidthA );
          ei := pAib^;
          pAib^ := 1;
          pA1 := GenPtr( A, i, i + ib, LineWidthA );
          pA2 := GenPtr( A, i + ib, 0, LineWidthA );

          //  EI = A( I+IB, I+IB-1 )
          //  A( I+IB, I+IB-1 ) = ONE
          //  CALL DGEMM( 'No transpose', 'Transpose',
     // $                  IHI, IHI-I-IB+1,
     // $                  IB, -ONE, WORK, LDWORK, A( I+IB, I ), LDA, ONE,
     // $                  A( 1, I+IB ), LDA )
          eigData.reflData.MatrixMultT2(pA2, LineWidthA, eigData.Y, pA1, ib, width, ib, width - i - ib,
                                        eigData.yLineWidth, LineWidthA, Min( HessMultBlockSize, eigData.blkMultSize ), doSub, eigData.blkMultMem );
          pAib^ := ei;

          //s1 := WriteMtx(A, LineWidthA, 5, 5);
//          s2 := WriteMtx(pA2, LineWidthA, 5, 5);

          // apply the block reflector H to A from the right
          // dtrmm right lower transpose unit
          pA := GenPtr( A, i, i + 1, LineWidthA );
          //CALL DTRMM( 'Right', 'Lower', 'Transpose',
//     $                  'Unit', I, IB-1,
//     $                  ONE, A( I+1, I ), LDA, WORK, LDWORK )
          MtxMultRightLowerTriaUnitT2(eigData.Y, eigData.yLineWidth,
                                  pA, LineWidthA, ib - 1, i + 1,
                                  ib - 1, ib - 1 );

          // DO 30 j = 0, ib-2
                //CALL daxpy( i, -one, work( ldwork*j+1 ), 1,
//      $                     a( 1, i+j+1 ), 1 )
//    30       CONTINUE
          for j := 0 to ib - 2 do
              MatrixSubVec( GenPtr( A, i + j + 1, 0, LineWidthA ), LineWidthA, GenPtr(eigData.Y, j, 0, eigData.yLineWidth ), eigData.yLineWidth,
                             1, i + 1, False );

          //s1 := WriteMtx(A, LineWidthA, 5, 5);
          // apply the block reflector H to A from the left
           //CALL DLARFB( 'Left', 'Transpose', 'Forward',
//     $                   'Columnwise',
//     $                   IHI-I, N-I-IB+1, IB, A( I+1, I ), LDA, T, LDT,
//     $                   A( I+1, I+IB ), LDA, WORK, LDWORK )
          ApplyBlockReflectorLFC(pA, LineWidthA, eigData.reflData, width - i - ib, width - i - 1, ib, false);   // original dlarfb call is transposed but dlarfb reroutes to non transposed?!?

          //s1 := WriteMtx(A, LineWidthA, 5, 5);
//          s2 := WriteMtx(pA, LineWidthA, 5, 5);

          inc(pTau, eigData.PnlSize);
          inc(i, eigData.PnlSize);
     end;

     // ###########################################
     // #### last non blocked part
     InternalHessenbergUnblocked( A, LineWidthA, i, width, pTau, eigData);
end;

procedure MatrixQFromHessenbergDecomp(A : PDouble; const LineWidthA : NativeInt; width : NativeInt;
 tau : PDouble; progress : TLinEquProgress = nil); overload;
begin
     MatrixQFromHessenbergDecomp(A, LineWidthA, width, tau, QRBlockSize, nil, progress);
end;

procedure MatrixQFromHessenbergDecomp(A : PDouble; const LineWidthA : NativeInt; width : NativeInt;
 tau : PDouble; BlockSize : NativeInt; work : PDouble; progress : TLinEquProgress = nil);
var i, j : NativeInt;
    pAj : PConstDoubleArr;
begin
     // shift the vectors which define the elementary reflectors one column to the right
     //
     pAj := PConstDoubleArr(A);
     pAj^[0] := 1;
     for i := 1 to width - 1 do
         pAj^[i] := 0;

     // shift columns and zero out
     for j := 1 to width - 1 do
     begin
          pAj := GenPtrArr(A, 0, j, LineWidthA);
          for i := j downto 1 do
              pAj^[i] := pAj^[i - 1];
          for i := j + 1 to width - 1 do
              pAj^[i] := 0;

          pAj^[0] := 0;
     end;

     MatrixQFromQRDecomp(GenPtr(A, 1, 1, LineWidthA), LineWidthA, width - 1, width - 1, tau, BlockSize, work, progress);
end;

procedure MatrixHessenberg2InPlace(A : PDouble; const LineWidthA : NativeInt; width : NativeInt; tau : PDouble; hessBlockSize : integer);
var hessWork : THessData;
begin
     FillChar(hessWork, sizeof(hessWork), 0 );

     hesswork.PnlSize := hessBlockSize;
     hesswork.blkMultSize := Min( width, Max(QRMultBlockSize, HessMultBlockSize) );

     hessWork.work := MtxAllocAlign( EigMemSize(hesswork, width), hessWork.mem );
     hessWork.T := hessWork.work;
     hessWork.tLineWidth := sizeof(double)*hesswork.PnlSize;
     // Y needs to point directly "under" T to work with the block reflectors
     hessWork.Y := GenPtr(hessWork.work, 0, hesswork.PnlSize, hessWork.tLineWidth);
     hessWork.yLineWidth := hessWork.tLineWidth;

     // align ptr just around "under" Y
     hessWork.blkMultMem := AlignPtr32( GenPtr( hessWork.work, 0, hesswork.PnlSize + width, hessWork.tLineWidth ) );

     hessWork.reflData.MatrixMultT1 := {$IFDEF FPC}@{$ENDIF}MatrixMultT1Ex;
     hessWork.reflData.MatrixMultT2 := {$IFDEF FPC}@{$ENDIF}MatrixMultT2Ex;
     hessWork.reflData.MatrixMultEx := {$IFDEF FPC}@{$ENDIF}MatrixMultEx;

     InitReflectorBlk(hessWork);
     try
        InternalMatrixHessenberg( A, LineWidthA, width, tau, hessWork );
     finally
            FreeMem(hessWork.mem);
     end;
end;

procedure ThrMtxHessenberg2InPlace(A : PDouble; const LineWidthA : NativeInt; width : NativeInt; tau : PDouble; hessBlockSize : integer);
var hessWork : THessData;
begin
     // around here is a good crossover point where multithreaded code starts to be faster
     if width < 150 then
     begin
          MatrixHessenberg2InPlace(A, LineWidthA, width, tau, hessBlockSize);
          exit;
     end;
     FillChar(hessWork, sizeof(hessWork), 0 );

     hesswork.PnlSize := hessBlockSize;
     hesswork.blkMultSize := Min( width, Max(QRMultBlockSize, HessMultBlockSize) );
     hessWork.work := MtxAllocAlign( EigMemSize(hesswork, width) + (numUseCPUCores - 1)*BlockMultMemSize( hesswork.blkMultSize ), hessWork.mem );
     hessWork.T := hessWork.work;
     hessWork.tLineWidth := sizeof(double)*hesswork.PnlSize;
     // Y needs to point directly "under" T to work with the block reflectors
     hessWork.Y := GenPtr(hessWork.work, 0, hesswork.PnlSize, hessWork.tLineWidth);
     hessWork.yLineWidth := hessWork.tLineWidth;

     // align ptr just around "under" Y
     hessWork.blkMultMem := AlignPtr32( GenPtr( hessWork.work, 0, hesswork.PnlSize + width, hessWork.tLineWidth ) );

     hessWork.reflData.MatrixMultT1 := {$IFDEF FPC}@{$ENDIF}ThrMatrixMultT1Ex;
     hessWork.reflData.MatrixMultT2 := {$IFDEF FPC}@{$ENDIF}ThrMatrixMultT2Ex;
     hessWork.reflData.MatrixMultEx := {$IFDEF FPC}@{$ENDIF}ThrMatrixMultEx;

     InitReflectorBlk(hessWork);
     try
        InternalMatrixHessenberg( A, LineWidthA, width, tau, hessWork );
     finally
            FreeMem(hessWork.mem);
     end;
end;

// ###########################################
// #### Symmetric Eigenvalue calculation
// ###########################################

type
  TSymEigRec = record
    mem : Pointer;
    work : PDouble;
    LineWidthWork : NativeInt;
    D : PConstDoubleArr;
    E : PConstDoubleArr;
    Z : PDouble;
    LineWidthZ : NativeInt;
    Tau : PConstDoubleArr;
    iWork : PIntegerArray;
    reflData : TBlockReflrec;
    nb : integer;
    progress : TLinEquProgress;
    ApplyPlaneRotSeqRVB : TApplyPlaneRotSeqMatrix;
    ApplyPlaneRotSeqRVF : TApplyPlaneRotSeqMatrix;
  end;

// DLATRD
// reduces NB columns (eigWork.nb) of A - a real symmetric matrix - to a symmetric
// tridiagonal form. The upper part of A is used
procedure InternalSymmetricTridiagBlockReduction( A : PDouble; LineWidthA : NativeInt;
  N : NativeInt; const eigWork : TSymEigRec);
var i : NativeInt;
    iw : NativeInt;
    pA0i1 : PDouble;
    pAii1 : PDouble;
    pAi0 : PDouble;
    pWiw0 : PDouble;
    alpha : double;
    i1 : Integer;
begin
     // Reduce last NB columns of upper triangle
     iw := eigWork.nb - 1;
     for i := N - 1 downto N - eigWork.nb do
     begin
          i1 := i + 1;
          pA0i1 := GenPtr(A, i1, 0, LineWidthA);
          pAi0 := GenPtr(A, i, 0, LineWidthA);
          pWiw0 := GenPtr(eigWork.work, iw, 0, eigWork.LineWidthWork);

          if i < N - 1 then
          begin
               // update A(1:i, i)
               pAii1 := GenPtr(A, i1, i, LineWidthA);

               // CALL dgemv( 'No transpose', i, n-i, -one, a( 1, i+1 ),
//      $                     lda, w( i, iw+1 ), ldw, one, a( 1, i ), 1 )
               MatrixMtxVecMult(pAi0, LineWidthA,
                                pA0i1,
                                GenPtr(eigWork.work, iw + 1, i, eigWork.LineWidthWork),
                                LineWidthA, sizeof(double),
                                n - i1, i1, -1, 1 );
//                CALL dgemv( 'No transpose', i, n-i, -one, w( 1, iw+1 ),
//      $                     ldw, a( i, i+1 ), lda, one, a( 1, i ), 1 )
               MatrixMtxVecMult(pAi0, LineWidthA,
                                GenPtr(eigWork.work, iw + 1, 0, eigWork.LineWidthWork),
                                pAii1,
                                eigWork.LineWidthWork, sizeof(double),
                                n - i1, i1, -1, 1 );
          end;

          // generate elementary reflector H(i) to annihilate A(1:i-2,i)
          pAii1 := GenPtr( A, i, i - 1, LineWidthA );

          // CALL dlarfg( i-1, a( i-1, i ), a( 1, i ), 1, tau( i-1 ) )
          GenElemHousholderRefl( pAi0, LineWidthA,
                                 i,
                                 pAii1^,
                                 @eigWork.tau^[i - 1] );
          // e( i-1 ) = a( i-1, i )
          // a( i-1, i ) = one
          eigWork.E^[i - 1] := pAii1^;
          pAii1^ := 1;

          // compute W(1:i-1, i);
          // CALL dsymv( 'Upper', i-1, one, a, lda, a( 1, i ), 1,
//     $                     zero, w( 1, iw ), 1 )
          MatrixMtxVecMultUpperSym( pWiw0,
                                    eigWork.LineWidthWork,
                                    A, pAi0, LineWidthA, LineWidthA, i, 1, 0 );

          if i < N - 1 then
          begin
               // CALL dgemv( 'Transpose', i-1, n-i, one, w( 1, iw+1 ),
               //       $      ldw, a( 1, i ), 1, zero, w( i+1, iw ), 1 )
               MatrixMtxVecMultT(GenPtr( eigWork.work, iw, i1, eigWork.LineWidthWork),
                                 eigWork.LineWidthWork,
                                 GenPtr( eigWork.work, iw + 1, 0, eigWork.LineWidthWork),
                                 pAi0,
                                 eigWork.LineWidthWork, LineWidthA,
                                 n - i1, i,
                                 1, 0);
     //          CALL dgemv( 'No transpose', i-1, n-i, -one,
  // $                        a( 1, i+1 ), lda, w( i+1, iw ), 1, one,
  // $                        w( 1, iw ), 1 )
               MatrixMtxVecMult( pWiw0, eigWork.LineWidthWork,
                                 GenPtr( A, i1, 0, LineWidthA),
                                 GenPtr( eigWork.work, iw, i1, eigWork.LineWidthWork ),
                                 LineWidthA, eigWork.LineWidthWork,
                                 n - i1, i, -1, 1 );
              // CALL dgemv( 'Transpose', i-1, n-i, one, a( 1, i+1 ),
//      $                        lda, a( 1, i ), 1, zero, w( i+1, iw ), 1 )
               MatrixMtxVecMultT( GenPtr( eigWork.work, iw, i1, eigWork.LineWidthWork),
                                  eigWork.LineWidthWork,
                                  GenPtr( A, i1, 0, LineWidthA),
                                  pAi0,
                                  LineWidthA, LineWidthA,
                                  n - i1, i, 1, 0);
               // CALL dgemv( 'No transpose', i-1, n-i, -one,
//      $                        w( 1, iw+1 ), ldw, w( i+1, iw ), 1, one,
//      $                        w( 1, iw ), 1 )
               MatrixMtxVecMult( pWiw0, eigWork.LineWidthWork,
                                 GenPtr( eigWork.work, iw + 1, 0, eigWork.LineWidthWork ),
                                 GenPtr( eigWork.work, iw, i1, eigWork.LineWidthWork ),
                                 eigWork.LineWidthWork, eigWork.LineWidthWork,
                                 n - i1, i, -1, 1 );
          end;

          // CALL dscal( i-1, tau( i-1 ), w( 1, iw ), 1 )
          MatrixScaleAndAdd( pWiw0,
                             eigWork.LineWidthWork,
                             1, i,
                             0,
                             eigWork.tau^[ i - 1 ] );
          // -half*tau( i-1 )*ddot( i - 1, w(1, iw), 1, a(1, i), 1 )
          alpha := -0.5*eigWork.tau^[i - 1]*MatrixVecDotMult( pWiw0,
                                                              eigWork.LineWidthWork,
                                                              pAi0, LineWidthA,
                                                              i );
              
          // CALL daxpy( i-1, alpha, a( 1, i ), 1, w( 1, iw ), 1 )
          MatrixVecAdd( pAi0, LineWidthA,
                        pWiw0, eigWork.LineWidthWork,
                        i, alpha );

          dec(iw);
     end;
end;

// dsytd2 in lapack upper triangle A
procedure InternalSymmetricTridiagReduction( A : PDouble; LineWidthA : NativeInt; N : NativeInt;
  const eigWork : TSymEigRec);
var i: NativeInt;
    pAii1 : PDouble;
    pA0i1 : PDouble;
    taui : double;
    alpha : double;
begin
     // unblocked version for tridiagonal reduction
     for i := n - 2 downto 0 do
     begin
          // generate elementary reflector H(i) to annihilate A(1:i-1,i)
          pAii1 := GenPtr( A, i + 1, i, LineWidthA );
          pA0i1 := GenPtr(A, i + 1, 0, LineWidthA);

          // CALL dlarfg( i, a( i, i+1 ), a( 1, i+1 ), 1, taui )
          GenElemHousholderRefl( pA0i1, LineWidthA, i + 1, pAii1^, @taui );
          eigWork.E^[i] := pAii1^;

          if taui <> 0 then
          begin
               // apply H(i) from both sides to A(1:i, 1:i)
               pAii1^ := 1;
               // compute x = tau * A * v storing x in Tau(1:i)
               // CALL dsymv( uplo, i, taui, a, lda, a( 1, i+1 ), 1, zero,
               //      $                     tau, 1 )
               MatrixMtxVecMultUpperSym( PDouble(eigWork.tau), sizeof(double), A, 
                                         pA0i1,
                                         LineWidthA, LineWidthA, i + 1, taui, 0 );

               // *
               // Compute  w := x - 1/2 * tau * (x**T * v) * v
               //
               // alpha = -half*taui*ddot( i, tau, 1, a( 1, i+1 ), 1 )
               // CALL daxpy( i, alpha, a( 1, i+1 ), 1, tau, 1 )
               alpha := -0.5*taui*MatrixVecDotMult(PDouble(eigWork.tau), sizeof(double), 
                                                   pA0i1,
                                                   LineWidthA, i + 1 );
               MatrixVecAdd( pA0i1, LineWidthA,
                             PDouble(eigWork.tau), sizeof(double), 
                             i + 1, alpha );

               // apply the transformation as a rank-2 update
               // A := A - v * w**T - w * v**T
               //  CALL dsyr2( uplo, i, -one, a( 1, i+1 ), 1, tau, 1, a,
               //              lda )
               MatrixUpperSymRank2Update( A, LineWidthA, pA0i1, LineWidthA,
                                          PDouble(eigWork.tau), sizeof(double), i + 1, 1 );
               pAii1^ := eigWork.E^[i];
          end;
          // d( i+1 ) = a( i+1, i+1 )
          // tau( i ) = taui
          eigWork.d^[i + 1] := GenPtr(A, i + 1, i + 1, LineWidthA)^;
          eigWork.tau^[i] := taui;
     end;
     // d( 1 ) = a( 1, 1 )
     eigWork.d^[0] := A^;
end;

// reduces a symmetric matrix A (upper part used) to tridiagonal form
// dsytrd.m
// D contains the diagonal elements and needs to be of size N
// E Contains the off diagonal elements and is of size N - 1
//   -> E contains A(i, i + 1) as y, x coordinates aka the upper diagonal
// tau (dimension N - 1) contains the elementary reflectors
procedure InternalSymmetricToTridiagonal( A : PDouble; LineWidthA : NativeInt; N : NativeInt;
  const eigWork : TSymEigRec);
var i : integer;
    j : Integer;
    pA : PDouble;
begin
     // ###########################################
     // #### Blocked update

     // Reduce the upper triangle of A.
     // Columns 0:>nb are handled by the unblocked method.
     //kk = n - ( ( n-nx+nb-1 ) / nb )*nb
     i := N - eigWork.nb;
     while i > eigWork.nb do
     begin
          // Reduce columns i:i+nb-1 to tridiagonal form and form the
          // matrix W which is needed to update the unreduced part of
          // the matrix
          InternalSymmetricTridiagBlockReduction( A, LineWidthA, i + eigWork.nb, eigWork);

          // Update the unreduced submatrix A(1:i-1,1:i-1), using an
          // update of the form:  A := A - V*W**T - W*V**T
          //  CALL dsyr2k( uplo, 'No transpose', i-1, nb, -one, a( 1, i ),
          //               lda, work, ldwork, one, a, lda )
          MatrixUpperSymRank2Update( A, LineWidthA,
                                     GenPtr( A, i, 0, LineWidthA ), LineWidthA,
                                     eigWork.work, eigWork.LineWidthWork, i, eigWork.nb );

          // copy superdiagaonal elements back into A and diagonal elements into D
          for j := i to i + eigWork.nb - 1 do
          begin
               pA := GenPtr( A, j, j, LineWidthA );
               eigWork.D^[j] := pA^;
               dec(PByte(pA), LineWidthA);
               pA^ := eigWork.E^[j - 1];
          end;

          dec(i, eigWork.nb);

          if Assigned(eigWork.progress) then
             eigWork.progress( 50*(N - i) div N );
     end;

     // ###########################################
     // #### Unblocked part
     inc(i, eigWork.nb);
     InternalSymmetricTridiagReduction(A, LineWidthA, i, eigWork );
     if Assigned(eigWork.progress) then
        eigWork.progress( 50 );
     //dsytd2(A, LineWidthA, D, E, tau, i);
end;


procedure MatrixUpperSymToTridiagInPlace( A : PDouble; LineWidthA : NativeInt; N : NativeInt;
  D, E, Tau : PConstDoubleArr; nb : NativeInt );
var eigWork : TSymEigRec;
    byteSize : NativeInt;
begin
     assert( (D <> nil) and (E <> nil), 'Error D and E need to be set');
     FillChar(eigWork, sizeof(eigWork), 0);

     byteSize := 0;
     if 2*nb < N then
        byteSize := nb*N;

     eigWork.LineWidthWork := nb*sizeof(double);
     eigWork.nb := Max(2, nb);
     eigWork.D := D;
     eigWork.E := E;
     eigWork.Tau := Tau;

     if eigWork.Tau = nil then
        byteSize := byteSize + (N - 1);

     byteSize := byteSize*sizeof(double);

     if byteSize > 0 then
        eigWork.Work := MtxAllocAlign( byteSize,
                                       eigWork.mem); // tau + E + work
     if Tau = nil then
     begin
          eigWork.Tau := PConstDoubleArr(eigWork.work);
          if (2*nb < N) then
             eigWork.Tau := GenPtrArr( eigWork.work, 0, N, eigWork.LineWidthWork );
     end;

     InternalSymmetricToTridiagonal( A, LineWidthA, N, eigWork );

     if byteSize > 0 then
        FreeMem(eigWork.mem);
end;

// computes the eigenvalues of a 2-by-2 symmetric matrix.
// |A   B|
// |B   C|
// original DLAE2
procedure EigVal2x2SymMatrix(const A, B, C : double; var RT1, RT2 : double );
var sm, df : double;
    adf, tb, ab : double;
    acmx : double;
    acmn : double;
    rt : double;
begin
     sm := a + c;
     df := a - c;

     adf := abs( df );
     tb := b + b;
     ab := abs( tb );

     if abs( a ) > abs( c ) then
     begin
          acmx := a;
          acmn := c;
     end
     else
     begin
          acmx := c;
          acmn := a;
     end;

     if adf > ab
     then
         rt := adf*sqrt( 1 + sqr( ab/adf ) )
     else if adf < ab
     then
         rt := ab*sqrt( 1 + sqr( adf/ab) )
     else // includes case ab=adf=0
         rt := ab*sqrt( 2 );

     if sm < 0 then
     begin
          rt1 := 0.5*( sm - rt );
          rt2 := ( acmx/rt1 )*acmn - ( b/rt1 )*b;
     end
     else if sm > 0 then
     begin
          rt1 := 0.5*( sm + rt );
          rt2 := ( acmx/rt1 )*acmn - ( b/rt1 )*b;
     end
     else
     begin
          rt1 := 0.5*rt;
          rt2 := -0.5*rt;
     end;
end;

// computes the eigenvalues and eigenvectors of a symmetric 2x2 matrix
procedure EigValVec2x2SymMatrix(const A, B, C : double; var RT1, RT2, CS1, SN1 : double );
var sgn1, sgn2 : integer;
    AB, ACMN, ACMX, ACS, ADF, CS,
    CT, DF, RT, SM, TB, TN : double;
begin
     // ###########################################
     // ####Compute Eigenvalues
     sm := a + c;
     df := a - c;
     adf := abs(df);
     tb := b + b;
     ab := abs( TB );
     if abs(a) > abs(C) then
     begin
          acmx := a;
          acmn := c;
     end
     else
     begin
          acmx := c;
          acmn := a;
     end;
     if adf > ab
     then
         rt := adf*sqrt( 1 + sqr(ab/adf) )
     else if adf < ab
     then
         rt := ab*sqrt( 1 + sqr(adf/ab) )
     else // adf = AB
         rt := ab*sqrt( 2 );

     if sm < 0 then
     begin
          rt1 := 0.5*(sm - rt);
          sgn1 := -1;

          rt2 := (acmx/rt1)*acmn - (b/rt1)*b;
     end
     else if sm > 0 then
     begin
          rt1 := 0.5*(sm + rt);
          sgn1 := 1;

          rt2 := (acmx/rt1)*acmn - (b/rt1)*b;
     end
     else
     begin
          // rt1 = rt2
          rt1 := 0.5*rt;
          rt2 := -0.5*rt;
          sgn1 := 1;
     end;

     // ###########################################
     // #### Compute eigenvectors
     if df >= 0 then
     begin
          cs := df + rt;
          sgn2 := 1;
     end
     else
     begin
          cs := df - rt;
          sgn2 := -1;
     end;
     acs := abs(cs);
     if acs > ab then
     begin
          ct := -tb/cs;
          sn1 := 1/sqrt(1 + sqr(ct));
          cs1 := ct*sn1;
     end
     else
     begin
          if ab = 0 then
          begin
               cs1 := 1;
               sn1 := 0;
          end
          else
          begin
               tn := -cs/tb;
               cs1 := 1/sqrt(1 + sqr(tn));
               sn1 := tn*cs1;
          end;
     end;
     if sgn1 = sgn2 then
     begin
          tn := cs1;
          cs1 := -sn1;
          sn1 := tn;
     end;
end;

function MaxAbsTridiagMatrix( N : NativeInt; D, E : PConstDoubleArr ) : double;
begin
     Result := 0;
     if N > 0 then
        Result := MatrixAbsMax( PDouble(D), N, 1, N*Sizeof(double));
     if N > 1 then
        Result := Max( Result, MatrixAbsMax( PDouble(E), N - 1, 1, (N - 1)*Sizeof(double)) );
end;

// original dsterf
//http://www.netlib.org/lapack/explore-html/d2/d24/group__aux_o_t_h_e_rcomputational_gaf0616552c11358ae8298d0ac18ac023c.html#gaf0616552c11358ae8298d0ac18ac023c
function InternalEigValFromSymMatrix( D, E : PConstDoubleArr; N : NativeInt) : TEigenvalueConvergence;
var eps1 : double;
    eps2 : double;
    smallNum : double;
    bignum : double;
    ssmalnum : double;
    sbignum : double;
    nmaxit : integer;
    sigma : double;
    jtot : integer;
    l1 : integer;
    m : integer;
    l : integer;
    lsv : integer;
    iScale : integer;
    lend : integer;
    lendsv : integer;
    aNorm : double;
    i : integer;
    p : double;
    rte : double;
    rt1, rt2 : double;
    r, c, s, gamma : double;
    alpha, oldgam, oldc, bb : double;
const cMaxIter = 30;

label
  L10, L30, L50, L70, L90, L100, L120, L140, L150, L170;

begin
     Result := qlOk;

// ###########################################
// #### disclaimer: This is the ugliest from lapack I have ever encountered...
     if n <= 1 then
        exit;

     // ###########################################
     // #### Get machine constants
     eps1 := eps(1);
     eps2 := eps1*eps1;

     if MinDouble <= 1/MaxDouble
     then
         smallnum := 1/MaxDouble * (1 + eps1)
     else
         smallnum := MinDouble;

     smallnum := sqrt(smallnum/eps1);
     bignum := 1/smallnum;

     sbignum := sqrt( bigNum ) / 3.0;
     ssmalnum := sqrt( smallNum ) / eps2;

     // ###########################################
     // #### Compute the eigenvalues of the tridiagonal matrix.
     nmaxit := N*cMaxIter;
     jtot := 0;

     // Determine where the matrix splits and choose QL or QR iteration
     // for each block, according to whether top or bottom diagonal
     // element is smaller.
     l1 := 0;

L10:
     if l1 >= n then
        goto L170;

     Result := qlOk;

     if l1 > 0 then
        e^[l1 - 1] := 0;

     for m := l1 to n - 2 do
     begin
          if abs( e^[m] ) <= sqrt( abs( d^[ m ] ) )*sqrt( abs( d^[ m + 1 ] ) )*eps1 then
          begin
               e^[m] := 0;

               goto L30;
          end;
     end;

     m := n - 1;

L30:
     l := l1;
     lsv := l;
     lend := m;
     lendsv := lend;
     l1 := m + 1;
     if lend = l then
        goto L10;

     // ###########################################
     // #### Scale submatrix in rows and columns L to LEND
     aNorm := MaxAbsTridiagMatrix( lend - l + 1, PConstDoubleArr( @d^[l] ), PConstDoubleArr( @e^[l] ) );

     iScale := 0;
     if aNorm = 0 then
     begin
          Result := qlMatrixError;
          goto L10;
     end;
     if aNorm > sbignum then
     begin
          iScale := 1;
          MatrixScaleAndAdd(PDouble(@d^[l]), N*Sizeof(double), lend - l + 1, 1, 0, aNorm/sbigNum );
          MatrixScaleAndAdd(PDouble(@e^[l]), N*Sizeof(double), lend - l, 1, 0, aNorm/sbigNum );
     end
     else if aNorm < ssmalnum then
     begin
          iScale := 2;
          MatrixScaleAndAdd(PDouble(@d^[l]), N*Sizeof(double), lend - l  + 1, 1, 0, aNorm/ssmalnum );
          MatrixScaleAndAdd(PDouble(@e^[l]), N*Sizeof(double), lend - l, 1, 0, aNorm/ssmalnum );
     end;

     // computing 2nd power
     for i := l to lend - 1 do
         e^[i] := sqr(e^[i]);

     // ###########################################
     // #### choose between QL and QR iteration
     if abs( d^[lend] ) < abs( d^[l] ) then
     begin
          lend := lsv;
          l := lendsv;
     end;

     if lend >= l then
     begin
          // ###########################################
          // #### QL Iteration
          // Look for small subdiagonal element
L50:
          if l <> lend then
          begin
               m := l;
               while m < lend do
               begin
                    if abs( e^[m] ) <= eps2*abs( d^[m]*d^[m + 1] ) then
                       goto L70;
                    inc(m);
               end;
          end;

          m := lend;
L70:
          if m < lend then
             e^[m] := 0;

          p := d^[l];
          if m = l then
             goto L90;

          // if remaining matrix is 2 by 2 use dlae2 to compute its eigenvalues
          if m = l + 1 then
          begin
               rte := sqrt( e^[l] );
               EigVal2x2SymMatrix( d^[l], rte, d^[l + 1], rt1, rt2);
               d^[l] := rt1;
               d^[l + 1] := rt2;
               e^[l] := 0;
               inc(l, 2);
               if l <= lend then
                  goto L50;

               goto L150;
          end;

          // ###########################################
          // #### check iterations
          if jtot = nmaxit then
             goto L150;

          inc(jtot);

          // ###########################################
          // #### form shift
          rte := sqrt( e^[l] );
          sigma := (d^[l + 1] - p)/(2*rte);
          r := pythag( sigma, 1 );
          sigma := p - rte/(sigma + sign(r, sigma));

          c := 1;
          s := 0;
          gamma := d^[m] - sigma;
          p := sqr(gamma);

          // inner loop
          for i := m - 1 downto l do
          begin
               bb := e^[i];
               r := p + bb;
               if i <> m - 1 then
                  e^[i + 1] := s*r;

               oldc := c;
               c := p/r;
               s := bb/r;
               oldgam := gamma;
               alpha := d^[i];
               gamma := c*(alpha - sigma) - s*oldgam;
               d^[i + 1] := oldgam + (alpha - gamma);
               if c <> 0
               then
                   p := sqr(gamma)/c
               else
                   p := oldc*bb;
          end;

          e^[l] := s*p;
          d^[l] := sigma + gamma;
          goto L50;
//  Eigenvalue found

L90:
          d^[l] := p;
          inc(l);
          if l <= lend then
             goto L50;

          goto L150;
     end
     else
     begin
          // QR iteration

          // look for small superdiagonal element
L100:
          m := l;
          while m > lend do
          begin
               if Abs( e^[m - 1] ) <= eps2*abs( d^[m]*d^[m - 1] ) then
                  goto L120;

               dec(m);
          end;

          m := lend;
L120:
          if m > lend then
             e^[m - 1] := 0;

          p := d^[l];
          if m = l then
             goto L140;

          // if remaining matrix is 2 by 2 use dlae2 to compute its eigenvalues
          if m = l - 1 then
          begin
               rte := sqrt(e^[l - 1]);
               EigVal2x2SymMatrix( d^[l], rte, d^[l - 1], rt1, rt2 );
               d^[l] := rt1;
               d^[l - 1] := rt2;
               e^[l - 1] := 0;
               dec(l, 2);
               if l >= lend then
                  goto L100;

               goto L150;
          end;

          if jtot = nmaxit then
             goto L150;

          inc(jtot);

          // form shift
          rte := sqrt( e^[l - 1] );
          sigma := (d^[l - 1] - p)/(rte*2);
          r := pythag( sigma, 1 );
          sigma := p - rte/(sigma + sign(r, sigma));

          c := 1;
          s := 0;
          gamma := d^[m] - sigma;
          p := sqr(gamma);

          // inner loop
          for i := m to l - 1 do
          begin
               bb := e^[i];
               r := p + bb;
               if i <> m then
                  e^[i - 1] := s*r;

               oldc := c;
               c := p/r;
               s := bb/r;
               oldgam := gamma;
               alpha := d^[i + 1];
               gamma := c*(alpha - sigma) - s*oldgam;
               d^[i] := oldgam + (alpha - gamma);
               if c <> 0
               then
                   p := sqr(gamma)/c
               else
                   p := oldc*bb;
          end;

          e^[l - 1] := s*p;
          d^[l] := sigma + gamma;
          goto L100;

          // eigenvalue found
L140:
          d^[l] := p;

          dec(l);
          if l >= lend then
             goto L100;

          goto L150;
     end;

// ###########################################
// #### Undo scaling
L150:
     if iScale = 1 then
        MatrixScaleAndAdd(PDouble(@d^[lsv]), N*Sizeof(double), lendsv - lsv + 1, 1, 0, sbigNum/aNorm );
     if isCale = 2 then
        MatrixScaleAndAdd(PDouble(@d^[lsv]), N*Sizeof(double), lendsv - lsv + 1, 1, 0, ssmalnum/aNorm );


// ###########################################
// #### check for no convergence to an eigenvalue after a total of N*Maxit iterations

     // what a piece of code... goto 10 go immeditaly to L170
     if jtot < nmaxit then
        goto L10;

     Result := qlNoConverge;
     exit;

L170:

     // sort eigenvalues in increasing order
     QuickSort( d, n );
end;

// DSTEQR computes all eigenvalues and, optionally, eigenvectors of a
// symmetric tridiagonal matrix using the implicit QL or QR method with option 'I'
function InternalEigValEigVecFromSymTridiagMatrix( D, E : PConstDoubleArr; Z : PDouble;
 LineWidthZ : NativeInt; N : NativeInt; const eigWork : TSymEigRec ) : TEigenvalueConvergence; overload;
const cMaxIter = 30;
var eps1 : double;
    eps2 : double;
    smallNum : double;
    bignum : double;
    ssmalnum : double;
    sbignum : double;
    pZ : PDouble;
    k, i, ii : Integer;
    nmaxit : integer;
    jtot : integer;
    l1 : integer;
    m: Integer;
    tst : double;
    l, lsv, lend, lendsv : integer;
    aNorm : double;
    iScale : Integer;
    p : double;
    rt1, rt2 : double;
    c, s : double;
    pW1, pW2 : PConstDoubleArr;
    b, f, g, r : double;
    j : Integer;
    work : PConstDoubleArr;
label
   L10, L30, L40, L60, L80, L90, L110, L130, L140, L160;
begin
     Result := qlOk;
     if N = 0 then
        exit;
     if N = 1 then
     begin
          Z^ := 1;
          exit;
     end;

     work := PConstDoubleArr(eigWork.work);

     // ###########################################
     // #### Get machine constants
     eps1 := eps(1);
     eps2 := eps1*eps1;

     if MinDouble <= 1/MaxDouble
     then
         smallnum := 1/MaxDouble * (1 + eps1)
     else
         smallnum := MinDouble;

     smallnum := sqrt(smallnum/eps1);
     bignum := 1/smallnum;

     sbignum := sqrt( bigNum ) / 3.0;
     ssmalnum := sqrt( smallNum ) / eps2;

     // ###########################################
     // #### Initialize Z = diag( 1, N ) -> ones in the diagonal
     MatrixInit(Z, LineWidthZ, N, N, 0);
     pZ := Z;
     for i := 0 to N - 1 do
     begin
          pZ^ := 1;
          inc(pZ);
          inc(PByte(pZ), LineWidthZ);
     end;

     nmaxit := N*cMaxIter;
     jtot := 0;

     // ###########################################
     // #### Determine where the matrix splits and choose QL or QR
     l1 := 0;

L10:
     if Assigned(eigWork.Progress) then
        eigWork.progress( 50 + 30*(N - l1) div N);
     if l1 >= n then
        goto L160;

     Result := qlOk;

     if l1 > 0 then
        e^[l1 - 1] := 0;

     for m := l1 to n - 2 do
     begin
          tst := abs( e^[m] );
          if tst = 0 then
             goto L30;
          if tst <= sqrt( abs( d^[m] ) )*sqrt( abs( d^[m + 1] ) )*eps1 then
          begin
               e^[m] := 0;
               goto L30;
          end;
     end;

     m := n - 1;
L30:
     l := l1;
     lsv := l;
     lend := m;
     lendsv := lend;
     l1 := m + 1;

     if lend = l then
        goto L10;

     // ###########################################
     // #### Scale submatrix in rows and columns L to LEND
     aNorm := MaxAbsTridiagMatrix( lend - l + 1, PConstDoubleArr( @d^[l] ), PConstDoubleArr( @e^[l] ) );

     iScale := 0;
     if aNorm = 0 then
     begin
          Result := qlMatrixError;
          goto L10;
     end;
     if aNorm > sbignum then
     begin
          iScale := 1;
          MatrixScaleAndAdd(PDouble(@d^[l]), N*Sizeof(double), lend - l + 1, 1, 0, aNorm/sbigNum );
          MatrixScaleAndAdd(PDouble(@e^[l]), N*Sizeof(double), lend - l, 1, 0, aNorm/sbigNum );
     end
     else if aNorm < ssmalnum then
     begin
          iScale := 2;
          MatrixScaleAndAdd(PDouble(@d^[l]), N*Sizeof(double), lend - l  + 1, 1, 0, aNorm/ssmalnum );
          MatrixScaleAndAdd(PDouble(@e^[l]), N*Sizeof(double), lend - l, 1, 0, aNorm/ssmalnum );
     end;

     // ###########################################
     // #### Now chose between QL, QR
     if abs( d^[lend] ) < abs( d^[l] ) then
     begin
          lend := lsv;
          l := lendsv;
     end;

     if lend > l then
     begin
          // ###########################################
          // #### QL
L40:
          if l <> lend then
          begin
               for m := l to lend - 1 do
               begin
                    tst := sqr(abs( e^[m] ));
                    if tst <= eps2*abs( d^[m] )*abs( d^[m + 1] ) + smallNum then
                       goto L60;
               end;
          end;

          m := lend;
L60:
          if m < lend then
             e^[m] := 0;

          p := d^[l];
          if m = l then
             goto L80;

          // If remaining matrix is 2-by-2, use DLAE2 or SLAEV2
          // to compute its eigensystem.

          pW1 := @work^[l];
          pW2 := @work^[n - 1 + l];

          if m = l + 1 then
          begin
               EigValVec2x2SymMatrix( d^[l], e^[l], d^[l + 1], rt1, rt2, c, s);

               pW1^[0] := c;
               pW2^[0] := s;

               eigWork.ApplyPlaneRotSeqRVB( 2, n, GenPtr(Z, l, 0, LineWidthZ), LineWidthZ,
                                    pW1, pW2 );
                //CALL dlaev2( d( l ), e( l ), d( l+1 ), rt1, rt2, c, s )
//                work( l ) = c
//                work( n-1+l ) = s
//                CALL dlasr( 'R', 'V', 'B', n, 2, work( l ),
//      $                     work( n-1+l ), z( 1, l ), ldz )


               d^[l] := rt1;
               d^[l + 1] := rt2;
               e^[l] := 0;
               inc(l, 2);
               if l <= lend then
                  goto L40;

               goto L140;
          end;

          if jtot = nmaxit then
             goto L140;
          inc(jtot);

          // ###########################################
          // #### Form shift.
          g := ( d^[l + 1] - p)/(2*e^[l]);
          r := pythag( g, 1 );
          g := d^[m] - p + ( e^[l]/(g + sign(r, g)));

          s := 1;
          c := 1;
          p := 0;

          // ###########################################
          // #### inner loop
          for i := m - 1 downto l do
          begin
               f := s*e^[i];
               b := c*e^[i];
               GenPlaneRotation( g, f, c, s, r );

               if i <> m - 1 then
                  e^[i + 1] := r;

               g := d^[i + 1] - p;
               r := ( d^[i] - g)*s + 2*c*b;
               p := s*r;
               d^[i + 1] := g + p;
               g := c*r - b;

               // ###########################################
               // #### save rotations
               work^[i] := c;
               work^[n - 1 + i] := -s;
          end;

          // ###########################################
          // #### Apply rotations
           //mm = m - l + 1
          eigWork.ApplyPlaneRotSeqRVB( m - l + 1, n, GenPtr(Z, l, 0, LineWidthZ), LineWidthZ,
                                       pW1, pW2 );

          //   CALL dlasr( 'R', 'V', 'B', n, mm, work( l ), work( n-1+l ),
          //               z( 1, l ), ldz )

          d^[l] := d^[l] - p;
          e^[l] := g;
          goto L40;

          // ###########################################
          // #### eigenvalue found
L80:
          d^[l] := p;
          inc(l);
          if l <= lend then
             goto L40;

          goto L140;
     end
     else
     begin
          // ###########################################
          // #### QR Iteration
L90:
          m := l;
          while m > lend do
          begin
               tst := sqr(abs( e^[m - 1] ));
               if tst <= eps2*abs( d^[m] )*abs( d^[m - 1] ) + smallNum then
                  goto L110;

               dec(m);
          end;

          m := lend;
L110:
          if m > lend then
             e^[m - 1] := 0;

          p := d^[l];
          if m = l then
             goto L130;

          // If remaining matrix is 2-by-2, use DLAE2 or SLAEV2
          // to compute its eigensystem.

          pW1 := @work^[m];
          pW2 := @work^[n - 1 + m];

          if m = l - 1 then
          begin
               EigValVec2x2SymMatrix( d^[l - 1], e^[l - 1], d^[l], rt1, rt2, c, s);

               pW1^[0] := c;
               pW2^[0] := s;

               eigWork.ApplyPlaneRotSeqRVF( 2, n, GenPtr(Z, l - 1, 0, LineWidthZ), LineWidthZ,
                                            pW1, pW2 );
                //CALL dlaev2( d( l ), e( l ), d( l+1 ), rt1, rt2, c, s )
//                work( l ) = c
//                work( n-1+l ) = s
//                CALL dlasr( 'R', 'V', 'B', n, 2, work( l ),
//      $                     work( n-1+l ), z( 1, l ), ldz )


               d^[l - 1] := rt1;
               d^[l] := rt2;
               e^[l - 1] := 0;
               dec(l, 2);
               if l >= lend then
                  goto L90;

               goto L140;
          end;

          if jtot = nmaxit then
             goto L140;
          inc(jtot);

          // ###########################################
          // #### Form shift.
          g := (d^[l - 1] - p)/(2*e^[l - 1]);
          r := pythag(g, 1);
          g := d^[m] - p + (e^[l - 1]/(g + sign(r, g)));

          s := 1;
          c := 1;
          p := 0;

          // ###########################################
          // #### inner loop
          for i := m to l - 1 do
          begin
               f := s*e^[i];
               b := c*e^[i];

               GenPlaneRotation( g, f, c, s, r );

               if i <> m then
                  e^[i - 1] := r;

               g := d^[i] - p;
               r := ( d^[i + 1] - g)*s + 2*c*b;
               p := s*r;
               d^[i] := g + p;
               g := c*r - b;

               // ###########################################
               // #### save rotations
               work^[i] := c;
               work^[n - 1 + i] := s;
          end;

          // ###########################################
          // #### Apply rotations to form Eigenvectors
          eigWork.ApplyPlaneRotSeqRVF( l - m + 1, n, GenPtr(Z, m, 0, LineWidthZ), LineWidthZ,
                                       pW1, pW2 );

          d^[l] := d^[l] - p;
          e^[l - 1] := g;
          goto L90;

          // ###########################################
          // #### Eigenvalue found
L130:
          d^[l] := p;
          dec(l);
          if l >= lend then
             goto L90;
          goto L140;
     end;

     // ###########################################
     // #### Undo scaling
L140:
     if iScale = 1 then
     begin
          MatrixScaleAndAdd(PDouble(@d^[lsv]), N*Sizeof(double), lendsv - lsv + 1, 1, 0, sbigNum/aNorm );
          // e is not resacled in the other routine...
          MatrixScaleAndAdd(PDouble(@e^[lsv]), N*Sizeof(double), lendsv - lsv, 1, 0, sbigNum/aNorm );
     end;
     if isCale = 2 then
     begin
          MatrixScaleAndAdd(PDouble(@d^[lsv]), N*Sizeof(double), lendsv - lsv + 1, 1, 0, ssmalnum/aNorm );
          MatrixScaleAndAdd(PDouble(@e^[lsv]), N*Sizeof(double), lendsv - lsv, 1, 0, ssmalnum/aNorm );
     end;

     if jtot < nmaxit then
        goto L10;


     // N*30 iterations ended without convergence -> end here
     Result := qlNoConverge;
     exit;

     // ###########################################
     // #### order eigenvalues and eigenvectors
L160:
     i := 0;
     for ii := 1 to n - 1 do
     begin
          k := i;
          p := d^[i];

          for j := ii to n - 1 do
          begin
               if d^[j] < p then
               begin
                    k := j;
                    p := d^[j];
               end;
          end;

          if k <> i then
          begin
               d^[k] := d^[i];
               d^[i] := p;
               MatrixColSwap(GenPtr(Z, i, 0, LineWidthZ), GenPtr(Z, k, 0, LineWidthZ), LineWidthZ, N);
          end;

          inc(i);
     end;
end;

function InternalEigValEigVecFromSymTridiagMatrix( D, E : PConstDoubleArr; Z : PDouble;
 LineWidthZ : NativeInt; N : NativeInt; Work : PConstDoubleArr ) : TEigenvalueConvergence; overload;
var eigWork : TSymEigRec;
begin
     FillChar(eigWork, sizeof(eigWork), 0);
     eigWork.work := PDouble(Work);
     eigWork.reflData.MatrixMultT1 := {$IFDEF FPC}@{$ENDIF}MatrixMultT1Ex;
     eigWork.reflData.MatrixMultT2 := {$IFDEF FPC}@{$ENDIF}MatrixMultT2Ex;
     eigWork.reflData.MatrixMultEx := {$IFDEF FPC}@{$ENDIF}MatrixMultEx;
     eigWork.ApplyPlaneRotSeqRVB := {$IFDEF FPC}@{$ENDIF}ApplyPlaneRotSeqRVB;
     eigWork.ApplyPlaneRotSeqRVF := {$IFDEF FPC}@{$ENDIF}ApplyPlaneRotSeqRVF;

     Result := InternalEigValEigVecFromSymTridiagMatrix(D, E, Z, LineWidthZ, N, eigWork);
end;

(*
// dlaeda
procedure InternalCreateZMerge( N, tlvls, curlvl, curpbm : NativeInt;
  prmptr, perm, givptr : PIntegerArray; givcol : PIntegerArray; givNum : PConstDoubleArr; Q : PConstDoubleArr; QPtr : PIntegerArray;
  Z : PConstDoubleArr; ZTemp : PDouble);
var bsiz1, bsiz2, curr : NativeInt;
    i, k, mid : NativeInt;
    PSiz1, PSiz2, ptr, zptr1 : NativeInt;
begin
     mid := n div 2 + 1;
     ptr := 1;


     curr := ptr + curpbm*(1 shl curlvl) + 1 shl (curlvl - 1) - 1;

     // determine size of these matrices
     bsiz1 := Round( sqrt( qptr^[curr + 1] - qptr^[curr] ) );
     bsiz2 := Round( sqrt( qptr^[curr + 2] - qptr^[curr + 1] ) );
     for k := 1 to mid - bsiz1 - 1 do
         z^[k] := 0;

     // todo: determine if the references to Q are actually the real deal
     MatrixCopy( @Z^[mid + bsiz1], sizeof(double), @Q^[ qptr^[curr] + bsiz1 - 1], N*sizeof(double), bsiz1, 1 );
     MatrixCopy( @Z^[mid], sizeof(double), @Q^[ qptr^[curr] ], N*sizeof(double), bsiz2, 1 );

     for k := mid + bsiz2 to N do
         z^[k] := 0;

     // loop though remaining levels 1 -> curlvl applying the givens rotations and permutations
     // and the multiplying the center matrices
     // agains the current Z
     ptr := 1 shl tlvls + 1;
     for k := 1 to curlvl - 1 - 1 do
     begin
          curr := ptr + curpbm*(1 shl curlvl - k) + (1 shl (curlvl - k - 1)) - 1;
          psiz1 := prmptr^[curr + 1] - prmptr^[curr];
          psiz2 := prmptr^[curr + 2] - prmptr^[curr + 1];
          zptr1 := mid - psiz1;

          // apply given rotation at curr and curr + 1
          for i := givptr^[curr] to givptr^[curr + 1] - 1 do
              MatrixRotate(1, @Z^[zptr1 + givcol^[2*i - 1] - 1], sizeof(double),
                              @Z^[zptr1 + givcol^[2*i] - 1], sizeof(double),
                              givnum^[2*i - 1], givnum^[2*i]);

          //dooo sama
     end;


end;



// dlaed7.f
function InternalMergeEigSys(N, QSiz, tlvls, curlvl, curpbm : NativeInt;
  D : PConstDoubleArr; Q : PDouble; LineWidthQ : NativeInt;
  indxQ : PIntegerArray; rho : double; cutpnt : NativeInt;
  QStore : PDouble; QPtr, PrmPtr, Perm, GivPtr : PIntegerArray; GivNum : PDouble;
  work : PDouble; iWork : PInteger ) : TEigenvalueConvergence;
var coltyp, curr : NativeInt;
    i, N1, N2 : NativeInt;
    idlmda, indx, indxc : NativeInt;
    indxp, iq2, iss, iw, iz : NativeInt;
    k, ldq2, ptr : NativeInt;
begin
     ldq2 := qsiz;

     iz := 1;
     idlmda := iz + N;
     iw := idlmda + N;
     iq2 := iw + N;
     iss := iq2 + n*ldq2;

     indx := 1;
     indxc := indx + N;
     coltyp := indxc + N;
     indxp := coltyp + N;

     ptr := 1 + 1 shl tlvls;
     for i := 1 to curlvl - 1 do
         ptr := ptr + 1 shl (tlvls - i);

     curr := ptr + curpbm;


     Result := qlMatrixError;

end;

// dlaed0.f
function InternalSubEigValEigVecFromSymTridiagMatrix(
 D, E : PConstDoubleArr; Q : PDouble; LineWidthQ : NativeInt;
 QStore : PDouble; LineWidthQStore : NativeInt;
 N, QSiz : NativeInt; const eigWork : TSymEigRec ) : TEigenvalueConvergence;
var subpbs : NativeInt;
    tlvls : NativeInt;
    iWork : PIntegerArray;
    spm1 : NativeInt;
    i, j, k : NativeInt;
    submat, smm1 : NativeInt;
    temp : double;
    spm2 : NativeInt;
    indxq : NativeInt;
    curlvl : NativeInt;
    lgn : NativeInt;
    iprmpt,
    iperm,
    iqptr,
    igivpt,
    igivcl : NativeInt;
    igivnm,
    iq,
    iwrem : NativeInt;
    msd2, curprb : NativeInt;
    curr : NativeInt;
    matsiz : integer;
    pWork : PDouble;
begin
     // account for non zero start indexing
     iWork := eigWork.iWork;
     dec(PByte(iWork), sizeof(integer));
     iWork^[1] := N;

     subpbs := 1;
     tlvls := 0;

     // determine the size and placement of the submatrices and save in the
     // leading elements of iwork
     while iWork^[subpbs] > cSymEigSmallSize do
     begin
          for j := subpbs downto 1 do
          begin
               eigWork.iWork^[2*j] := (iWork^[j] + 1) div 2;
               eigWork.iWork^[2*j + 1] := iWork^[j] div 2;
          end;
          inc(tlvls);
          subpbs := 2*subpbs;
     end;

     for j := 2 to subpbs do
         iWork^[j] := iWork^[j] + iwork^[j - 1];

     // divide the matrix in subpbs submatrices of size at most cSymEigSmallSize
     // using rank-1 modifications

     spm1 := subpbs - 1;
     for i := 1 to spm1 do
     begin
          submat := iWork^[i];
          smm1 := submat - 1;  // minus 1 to account for the startindex 1
          d^[smm1] := d^[smm1] - abs( e^[smm1] );
          d^[submat] := d^[submat] - abs( e^[smm1] );
     end;

     // ###########################################
     // #### Initialize workspace pointers
     indxq := 4*n + 3;
     lgn := Ceil(log2( N ));
     if 1 shl lgn < n then
        inc(lgn);
     if 1 shl lgn < n then
        inc(lgn);

     iprmpt := indxq + n + 1;
     iperm := iprmpt + n*lgn;
     iqptr := iperm + n*lgn;
     igivpt := iqptr + n + 2;
     igivcl := igivpt + n*lgn;

     igivnm := 1;
     iq := igivnm + 2*n*lgn;
     iwrem := iq + n*n + 1;

     for i := 0 to subpbs do
     begin
          iwork^[iprmpt + i] := 1;
          iwork^[igivpt + i] := 1;
     end;

     iwork^[iqptr] := 1;

     // ###########################################
     // #### solve submatrices
     curr := 0;
     for i := 0 to spm1 do
     begin
          if i = 0 then
          begin
               submat := 1;
               matsiz := iwork^[1];
          end
          else
          begin
               submat := iwork^[i] + 1;
               matsiz := iwork^[i + 1] - iwork^[i];
          end;

          // minus one to take the zero indexing into account
          pWork := GenPtr(eigWork.work, iq - 1 + iWork^[iqptr + curr] - 1, 0, N*sizeof(double) );
          Result := InternalEigValEigVecFromSymTridiagMatrix( @d^[submat - 1], @e^[submat - 1],
                                                              pWork,
                                                              N*sizeof(double),
                                                              submat,
                                                              PConstDoubleArr(eigWork.work) );

          if Result <> qlOk then
             exit;

          MatrixMultEx( GenPtr(qstore, submat - 1, 0, LineWidthQStore), LineWidthQStore,
                        GenPtr(Q, submat - 1, 0, LineWidthQ),
                        pWork,
                        matsiz, QSiz, matsiz, matsiz,
                        LineWidthQ, N*sizeof(Double),
                        eigWork.reflData.blkMultSize, doNone, eigWork.reflData.blkMultMem );

          iwork^[iqptr + curr + 1] := iwork^[iqptr + curr] + sqr(matsiz);
          inc(curr);

          k := 1;
          for j := submat to iWork^[i + 1] do
          begin
               iwork^[indxq + j] := k;
               inc(k);
          end;
     end;

     // ###########################################
     // #### Merge eigensystems of adjacent submatrices
     curlvl := 1;
     while subpbs > 1 do
     begin
          if subpbs > 1 then
          begin
               spm2 := subpbs - 2;
               for i := 0 to spm2 do
               begin
                    if i = 0 then
                    begin
                         submat := 1;
                         matsiz := iwork^[2];
                         msd2 := iwork^[1];
                         curprb := 0;
                    end
                    else
                    begin
                         submat := iwork^[i] + 1;
                         matsiz := iwork^[i + 2] - iwork^[i];
                         msd2 := matsiz div 2;
                         inc(curprb);
                    end;
               end;

               // ###########################################
               // #### Merge lower order eigensystems into an eigensystem of size matsiz
               //Result := InternalMergeEigSys( matsiz, qsiz, tlvls, curlvl, curprb,
//                                              @d^[submat - 1],
//                                              GenPtr( QStore, submat - 1, 0, LineWidthQStore), LineWidthQStore,
//                                              @iWork^[indxq + submat],
//                                              e^[submat + msd2 - 2],
//                                              msd2,
//                                              @iWork^[iqptr], @iWork^[iprmpt], @iWork^[iperm],
//                                              @iWork^[igivnm], PIntegerArray(GenPtr( eigWork.work, iwrem, 0, 0 )),
//                                              iWork^[subpbs + 1] );

               if Result <> qlOk then
                  exit;

               iwork^[i div 2 + 1] := iwork^[ i + 2 ];
          end;
     end;

     // ###########################################
     // #### Sort


end; *)

// DSTEDC computes all eigenvalues and, optionally, eigenvectors of a
// symmetric tridiagonal matrix using the divide and conquer method.
// The eigenvectors of a full or band real symmetric matrix can also be
// found if InternalSymEigValUpperhas been used to reduce this
// matrix to tridiagonal form.
// DSTEDC in netlib with option 'I'
function InternalEigValEigVecFromSymTridiagMatrixDivedConquer( D, E : PConstDoubleArr; Z : PDouble; LineWidthZ : NativeInt; N : NativeInt;
  const eigWork : TSymEigRec ) : TEigenvalueConvergence;
//var start, fin : NativeInt;
//    pStoreZ : PDouble;
//    LineWidthStoreZ : NativeInt;
//    tiny : double;
//    m : NativeInt;
//    pZ : PDouble;
//    orgNorm : double;
begin
     Result := qlOk;

     // ###########################################
     // #### Quick return if possible
     if N = 0 then
        exit;
     if N = 1 then
     begin
          z^ := 1;
          exit;
     end;

     // ###########################################
     // #### do not use the divide an conquer algorithm for small sized matrices
     //if N <= cSymEigSmallSize then

     // code is not yet ready for the divide and conquer algorithm
     if True then
     begin
          Result := InternalEigValEigVecFromSymTridiagMatrix( D, E, Z, LineWidthZ, N, eigWork );
          exit;
     end;

     {
     LineWidthStoreZ := N*sizeof(double);
     pStoreZ := GenPtr( eigWork.work, 0, N, LineWidthZ);

     start := 0;
     while start < N do
     begin
          fin := start;

          // find the position of the subproblem such that e^[fin] <= tiny
          // or fin = N - 1 if no such subdiagonal exists.
          // the matrix identified by the elements between start and finish
          // constitutes an independent sub-problem
          while fin < N - 1 do
          begin
               tiny := cDoubleEpsilon*sqrt( abs( d^[fin] ) )*sqrt( abs( d^[fin + 1] ) );
               if abs( e^[fin] ) <= tiny then
                  break;

               inc(fin);
          end;

          // ###########################################
          // #### calculate the sub problem
          m := fin - start + 1;

          if m = 1 then
          begin
               start := fin + 1;
               continue;
          end;

          if m > cSymEigSmallSize then
          begin
               // scale
               orgNorm := MaxAbsTridiagMatrix( m, @d^[start], @e^[start] );
               MatrixScaleAndAdd(@d^[start], m*sizeof(double), m, 1, 0, 1/orgNorm);
               MatrixScaleAndAdd(@e^[start], m*sizeof(double), m - 1, 1, 0, 1/orgNorm);

               // calc sub problem
               Result := InternalSubEigValEigVecFromSymTridiagMatrix(
                                 @d^[start], @e^[start],
                                 GenPtr( Z, start, 0, LineWidthZ), LineWidthZ,
                                 pStoreZ, N*sizeof(double),
                                 N, m, eigWork );
               // scale back
               MatrixScaleAndAdd(@d^[start], m*sizeof(double), m, 1, 0, orgNorm);
          end
          else
          begin
               Result := InternalEigValEigVecFromSymTridiagMatrix( @d^[start], @e^[start],
                                                                   eigWork.work, m*sizeof(double), m, GenPtrArr( eigWork.work, m*m, 0, 0 ) );
               if Result <> qlOk then
                  exit;

               pZ := GenPtr(Z, start, 0, LineWidthZ);
               MatrixCopy(pStoreZ, LineWidthStoreZ, pZ, LineWidthZ, m, N);
               eigWork.reflData.MatrixMultEx(pZ, LineWidthZ, pStoreZ, eigWork.work, m, n, m, m, LineWidthStoreZ, m*sizeof(double),
                                             eigWork.reflData.blkMultSize, doNone, eigWork.reflData.blkMultMem );
          end;

          start := fin + 1;
     end;


     // ###########################################
     // #### Sorting...

      }
end;


// dlarft backward columnwise
procedure CreateTMtxBC(n, k : NativeInt; A : PDouble; LineWidthA : NativeInt;
  Tau : PConstDoubleArr; reflData : TBlockReflrec);
var i, j : NativeInt;
    pT : PDouble;
    pA1 : PDouble;
    pAij : PDouble;
    prevLastA : NativeInt;
    lastA : NativeInt;
    k1 : NativeInt;
    nki : NativeInt;
    pcAij : PConstDoubleArr;
begin
     assert(k <= n, 'Error k needs to be smaller than n');

     // A kxn matrix (k = width)
     // T: kxk triangular factor

     prevLastA := 0;
     k1 := k - 1;
     nki := n;  // n - k + i -> i starts from k

     for i := k1 downto 0 do
     begin
          dec(nki);
          if Tau^[i] = 0 then
          begin
               // H = I
               // DO j = i, k
               //    t( j, i ) = zero
               // END DO
               pT := GenPtr(reflData.T, i, i, reflData.LineWidthT);
               for j := i to k1 do
               begin
                    pT^ := 0;
                    inc(PByte(pT), reflData.LineWidthT);
               end;
          end
          else
          begin
               // general case
               lastA := 0;
               if i < k1 then
               begin
                    // DO lastv = 1, i-1
                    //     IF( v( lastv, i ).NE.zero ) EXIT
                    //  END DO

                    pAij := GenPtr(A, i, 0, LineWidthA);
                    while (pAij^ = 0) and (lastA < i - 1) do
                    begin
                         inc(lastA);
                         inc(PByte(pAij), LineWidthA);
                    end;

                    // T(i+1:k,i) = -tau(i) * V(j:n-k+i,i+1:k)**T * V(j:n-k+i,i)

                    (*  DIRECT = 'B' and STOREV = 'C':         DIRECT = 'B' and STOREV = 'R': */

/*               V = ( v1 v2 v3 )     k x n
/*                   ( v1 v2 v3 )
/*                   (  1 v2 v3 )
/*                   (     1 v3 )
/*                   (        1 ) *)


                    // loop to get the
                    // DO j = i+1, k
                    //     t( j, i ) = -tau( i ) * v( n-k+i , j )
                    //  END DO
                    //  j = max( lastv, prevlastv )
                    pT := GenPtr( reflData.T, i, i + 1, reflData.LineWidthT);
                    pcAij := GenPtrArr(A, 0, nki, LineWidthA);
                    for j := i + 1 to k1 do
                    begin
                         pT^ := -tau^[i]*pcAij^[j];
                         inc(PByte(pT), reflData.LineWidthT);
                    end;

                    j := Max(lastA, prevLastA);

                    pT := GenPtr(reflData.T, i, i + 1, reflData.LineWidthT);
                    pA1 := GenPtr(A, i + 1, j, LineWidthA);
                    pAij := GenPtr(A, i, j, LineWidthA);
                    // h = n - k + i - j, w = k - i

                    // CALL dgemv( 'Transpose', n-k+i-j, k-i, -tau( i ),
 //     $                           v( j, i+1 ), ldv, v( j, i ), 1, one,
 //     $                           t( i+1, i ), 1 )
                    MatrixMTxVecMultT( pT, reflData.LineWidthT, pA1, pAij, LineWidthA, LineWidthA,
                                       k1 - i, nki - j, -tau^[i], 1.0 );

                    // t(i + 1:k, i) := T(i + 1:k, i+1:k) * T(i + 1:k, i)
                    // CALL dtrmv( 'Lower', 'No transpose', 'Non-unit', k-i,
          // $                        t( i+1, i+1 ), ldt, t( i+1, i ), 1 )
                    MtxMultLowNoTranspNoUnitVec(GenPtr(reflData.T, i + 1, i + 1, reflData.LineWidthT),
                                                reflData.LineWidthT,
                                                pT, reflData.LineWidthT, k1 - i );

                    if i > 0
                    then
                        prevLastA := Min(prevLastA, lastA)
                    else
                        prevLastA := lastA;
               end;

               pT := GenPtr(reflData.T, i, i, reflData.LineWidthT);
               pT^ := Tau^[i];
          end;
     end;
end;

// is actually dormQL
procedure InternalEiVecFromQLeftUpperTranspose( A : PDouble; LineWidthA : NativeInt; tau : PConstDoubleArr;
                                       C : PDouble; LineWidthC : NativeInt;
                                       m, n : NativeInt; const eigWork : TSymEigRec);
var k : NativeInt;
    pAii : PDouble;
    pAi0 : PDouble;
    aii : double;
    i : Integer;
    mi : integer;
const cNBMin = 2;
begin
     // from input A is nxn ->
     // A is dimension wxh = kxm after increment
     k := m - 1;
     m := m - 1;

     if (m <= 0) or (n <= 1) then
        exit;


     // a(1, 2)
     inc(A);

     i := 0;

     // ###########################################
     // #### Blocked part
     if eigWork.nb > cNBMin then
     begin
          while i + eigWork.nb + 1 < k do
          begin
               // form triangular factor
               // nq = m
               // nw = n
               // mi = m - k  + i + ib - 1
               // k = ib
               // ni = n
               mi := m - k + i + eigWork.nb;
               pAi0 := GenPtr(A, i, 0, LineWidthA);
               CreateTMtxBC( mi, eigWork.nb, pAi0, LineWidthA,
                             @tau^[i], eigWork.reflData);

               // apply the T matrix..
               ApplyBlockReflectorLBC( pAi0, LineWidthA,
                                       C, LineWidthC,
                                       eigWork.work, eigWork.LineWidthWork,
                                       n, mi, eigWork.nb, True, eigWork.reflData );

               inc(i, eigWork.nb);

               if Assigned(eigWork.progress) then
                  eigWork.progress( 80 + 20*i div k);
          end;
     end;

     // ###########################################
     // #### Unblocked part
     while i < k do
     begin
          pAii := GenPtr( A, i, m - k + i, LineWidthA);
          aii := pAii^;
          pAii^ := 1;
          pAi0 := GenPtr(A, i, 0, LineWidthA);
          ApplyElemHousholderReflLeft( pAi0, LineWidthA,
                                       C, LineWidthC, n, m - k + i + 1,
                                       @tau^[i], eigWork.work );
          pAii^ := aii;

          if Assigned(eigWork.progress) then
             eigWork.progress( 80 + 20*i div k);
          inc(i);
     end;
end;

// calculates Eigenvalues and optionally Eigenvectors from a symmetric
// matrix A. The upper half of of A is used for the calculation and
// overwritten by either the Eigenvectors (if wanted) or through various intermediate
// steps. the vector D of eigwork contains the Eigenvalues and needs to be of size N.
// dsyevd in netlib
function InternalSymEigValUpper(A : PDouble; LineWidthA : NativeInt; N : NativeInt; EigValOnly : boolean; const eigWork : TSymEigRec ) : TEigenvalueConvergence;
var eps1 : double;
    smallNum : double;
    bigNum : double;
    mtxMax : double;
    sigma : double;
    doScale : boolean;
begin
     Result := qlMatrixError;
     if N <= 0 then
        exit;

     // ###########################################
     // #### quick return if possible...
     if N = 1 then
     begin
          eigWork.D^[0] := A^;
          if not EigValOnly then
             A^ := 1;

          Result := qlOk;
          exit;
     end;

     // ###########################################
     // #### Machine constants
     eps1 := eps(1);
     if MinDouble <= 1/MaxDouble
     then
         smallnum := 1/MaxDouble * (1 + eps1)
     else
         smallnum := MinDouble;

     smallnum := sqrt(smallnum/eps1);
     bignum := 1/smallnum;

     // ###########################################
     // #### Scale to machine precission
     mtxMax := MatrixAbsMaxUpper(A, N, LineWidthA);
     doScale := False;

     sigma := 0;
     if mtxMax >= bigNum then
     begin
          doScale := True;
          sigma := smallNum/mtxMax;
     end
     else if mtxMax <= smallNum then
     begin
          doScale := True;
          Sigma := bignum/mtxMax;
     end;
     if doScale then
        MatrixScaleAndAdd(A, LineWidthA, N, N, 0, sigma);


     // ###########################################
     // #### Hard work...
     InternalSymmetricToTridiagonal(A, LineWidthA, N, eigWork );

     // ###########################################
     // #### Eigenvectors and Eigenvalues
     if EigValOnly
     then
         Result := InternalEigValFromSymMatrix( eigWork.D, eigWork.E, N )
     else
     begin
          //CALL dstedc( 'I', n, w, work( inde ), work( indwrk ), n,
//                       work( indwk2 ), llwrk2, iwork, liwork, info )
//          CALL dormtr( 'L', uplo, 'N', n, n, a, lda, work( indtau ),
//                       work( indwrk ), n, work( indwk2 ), llwrk2, iinfo )
//          CALL dlacpy( 'A', n, n, work( indwrk ), n, a, lda )
          Result := InternalEigValEigVecFromSymTridiagMatrixDivedConquer( eigWork.D,
                                    eigWork.E, eigWork.Z, eigWork.LineWidthZ, N, eigWork);

          if Result = qlOk then
          begin
               InternalEiVecFromQLeftUpperTranspose( A, LineWidthA, eigWork.Tau, eigWork.Z, eigWork.LineWidthZ,
                                            N, N, eigWork );
               MatrixCopy( A, LineWidthA, eigWork.Z, eigWork.LineWidthZ, N, N );
          end;
     end;

     // ###########################################
     // #### Undo scale
     if doScale then
        MatrixScaleAndAdd(PDouble(eigWork.D), N*SizeOf(double), N, 1, 0, 1/sigma);


     if Assigned(eigWork.progress) then
        eigWork.progress( 100 );
end;

function MatrixEigUpperSymmetricMatrixInPlace2(A : PDouble; LineWidthA : NativeInt; N : NativeInt; W : PConstDoubleArr; EigValOnly : boolean; blockSize : integer; progress : TLinEquProgress = nil ) : TEigenvalueConvergence;
var eigWork : TSymEigRec;
    iworkSize : NativeInt;
    workSize : NativeInt;
begin
     // ###########################################
     // #### Initialize memory
     FillChar(eigWork, sizeof(eigWork), 0);

     eigWork.nb := Min(N, Max(2, blockSize));
     eigWork.D := PConstDoubleArr(W);
     eigWork.progress := progress;

     workSize := UperSymEigMemorySize( N, eigWork.nb, EigValOnly, False, eigWork.reflData.BlkMultSize);

     if EigValOnly then
     begin
          eigWork.E := MtxAllocAlign( workSize,// // Tau + E and nb*N for reduction of symmetric to tridiagonal
                                      eigWork.mem); // tau + E + work
          eigWork.LineWidthWork := eigWork.nb*sizeof(double);
          eigWork.tau := GenPtrArr(PDouble(eigWork.E), N - 1, 0, 0);
          eigWork.work := GenPtr( PDouble(eigWork.Tau), N - 1, 0, 0);
     end
     else
     begin
          //workSize := 1 + 3*N + 2*N*Max(1, ceil( Log2( N ) ) ) + 4*N*N;
          eigWork.reflData.blkMultSize := Min( N, QRMultBlockSize );

         // iworkSize := 6 + 6*N + 5*N*ceil( Log2(N) );
         // iworkSize := iWorkSize + (iWorkSize and 1);
          iWorkSize := 0; // not yet implemented
          eigWork.E := MtxAllocAlign( workSize,
                                      //(1 + 3*N + N*N )*sizeof(double), // Tau, E, Z
                                      eigWork.mem);
          eigWork.tau := GenPtrArr(PDouble(eigWork.E), N - 1, 0, 0);

          eigWork.LineWidthZ := N*sizeof(double);
          eigWork.Z := GenPtr( PDouble(eigWork.tau), N - 1, 0, 0);

          eigWork.iWork := PIntegerArray( GenPtr(eigWork.Z, 0, N, eigWork.LineWidthZ) );

          eigWork.work := PDouble( eigWork.iWork );
          inc( PInteger(eigWork.work), iWorkSize );  // nb*N bytes left. nb*N is also used in InternalEigValEigVecFromSymTridiagMatrix
          eigWork.LineWidthWork := eigWork.nb*sizeof(double);
          eigWork.reflData.T := GenPtr(eigWork.work, 0, N, eigWork.LineWidthWork);
          eigWork.reflData.LineWidthT := eigWork.nb*sizeof(double);

          eigWork.reflData.blkMultMem := GenPtr( eigwork.reflData.T, 0, eigWork.nb, eigWork.reflData.LineWidthT );
     end;

     eigWork.reflData.MatrixMultT1 := {$IFDEF FPC}@{$ENDIF}MatrixMultT1Ex;
     eigWork.reflData.MatrixMultT2 := {$IFDEF FPC}@{$ENDIF}MatrixMultT2Ex;
     eigWork.reflData.MatrixMultEx := {$IFDEF FPC}@{$ENDIF}MatrixMultEx;
     eigWork.ApplyPlaneRotSeqRVB := {$IFDEF FPC}@{$ENDIF}ApplyPlaneRotSeqRVB;
     eigWork.ApplyPlaneRotSeqRVF := {$IFDEF FPC}@{$ENDIF}ApplyPlaneRotSeqRVF;

     // ###########################################
     // #### Eigenvalue calculation
     Result := InternalSymEigValUpper(A, LineWidthA, N, EigValOnly, eigWork);
     FreeMem(eigWork.mem);
end;

// ###########################################
// #### Threaded symmetric eigenvalue functions
// ###########################################

// ##############################################################
// #### Local definitions used in the threading calls
// ##############################################################
type
  TMatrixRotateRec = record
  public
    Width : NativeInt;
    Height : NativeInt;
    A : PDouble;
    LineWidthA : NativeInt;
    C : PConstDoubleArr;
    S : PConstDoubleArr;

    procedure Create( aWidth, aHeight : NativeInt; aA : PDouble; const aLineWidthA : NativeInt; cC, cS : PConstDoubleArr);
  end;
  PMatrixRotateRec = ^TMatrixRotateRec;

procedure TMatrixRotateRec.Create( aWidth, aHeight : NativeInt; aA : PDouble; const aLineWidthA : NativeInt; cC, cS : PConstDoubleArr);
begin
     width := aWidth;
     Height := aHeight;
     A := aA;
     LineWidthA := aLineWidthA;
     C := cC;
     S := cS;
end;

procedure ThrApplyPlaneRotSeqRVBFunc( obj : Pointer);
begin
     with PMatrixRotateRec(obj)^ do
     begin
          if (Width > 0) and (Height > 0) then
             ApplyPlaneRotSeqRVB(width, height, A, LineWidthA, C, S);
     end;
end;

procedure ThrApplyPlaneRotSeqRVFFunc( obj : Pointer);
begin
     with PMatrixRotateRec(obj)^ do
     begin
          if (Width > 0) and (Height > 0) then
             ApplyPlaneRotSeqRVF(width, height, A, LineWidthA, C, S);
     end;
end;

procedure ThrApplyPlaneRotSeqRVB(width, height : NativeInt; A : PDouble; const LineWidthA : NativeInt; C, S : PConstDoubleArr);
var obj : TMatrixRotateRec;
    objs : Array[0..cMaxNumCores - 1] of TMatrixRotateRec;
    numUsed : integer;
    calls : IMtxAsyncCallGroup;
    counter: NativeInt;
    h : NativeInt;
begin
     //numUsed := numCPUCores - 1;
     numUsed := numRealCores;

     if numRealCores = numUseCPUCores then
        dec(numUsed);

     // check if threading is speeding up the process
     if (numUsed < 2) or (Width*Height < 8*numUsed) then
     begin
          obj.Create(Width, height, A, LineWidthA, C, S );

          ThrApplyPlaneRotSeqRVBFunc(@obj);
          exit;
     end;

     h := height div numUsed;
     for counter := 0 to numUsed - 1 do
     begin
          objs[counter].Create(width, h, A, LineWidthA, C, S);
          objs[counter].A := GenPtr(A, 0, counter*h, LineWidthA);
     end;

     objs[numUsed - 1].height := height - (numUsed - 1)*h;

     calls := MtxInitTaskGroup;
     for counter := 0 to numUsed - 2 do
         calls.AddTaskRec({$IFDEF FPC}@{$ENDIF}ThrApplyPlaneRotSeqRVBFunc, @objs[counter]);
     ThrApplyPlaneRotSeqRVBFunc(@objs[numUsed - 1]);
     calls.SyncAll;
end;

procedure ThrApplyPlaneRotSeqRVF(width, height : NativeInt; A : PDouble; const LineWidthA : NativeInt; C, S : PConstDoubleArr);
var obj : TMatrixRotateRec;
    objs : Array[0..cMaxNumCores - 1] of TMatrixRotateRec;
    numUsed : integer;
    calls : IMtxAsyncCallGroup;
    counter: NativeInt;
    h : NativeInt;
begin
     //numUsed := numCPUCores - 1;
     numUsed := numRealCores;

     if numRealCores = numUseCPUCores then
        dec(numUsed);

     // check if threading is speeding up the process
     if (numUsed < 2) or (Width*Height < 8*numUsed) then
     begin
          obj.Create(Width, height, A, LineWidthA, C, S );

          ThrApplyPlaneRotSeqRVFFunc(@obj);
          exit;
     end;

     h := height div numUsed;
     for counter := 0 to numUsed - 1 do
     begin
          objs[counter].Create(width, h, A, LineWidthA, C, S);
          objs[counter].A := GenPtr(A, 0, counter*h, LineWidthA);
     end;

     objs[numUsed - 1].height := height - (numUsed - 1)*h;

     calls := MtxInitTaskGroup;
     for counter := 0 to numUsed - 2 do
         calls.AddTaskRec({$IFDEF FPC}@{$ENDIF}ThrApplyPlaneRotSeqRVFFunc, @objs[counter]);
     ThrApplyPlaneRotSeqRVFFunc(@objs[numUsed - 1]);
     calls.SyncAll;
end;


// determine the memory needed for optimal working set sizes
function UperSymEigMemorySize( N, blockSize : NativeInt;
  EigValOnly : boolean; Threaded : Boolean; var blkMultSize : integer ) : NativeInt;
var multMem : NativeInt;
begin
     blkMultSize := 0;
     if EigValOnly then
     begin
          Result := blockSize*N*sizeof(double) +  // nb*N for reduction of symmetric to tridiagonal
                    2*(N - 1)*sizeof(double);      // Tau + E
     end
     else
     begin
          blkMultSize := Min(N, QRMultBlockSize);
                    //  T                work                 Tau/E     Z
          Result := sqr(blockSize) + Max(2, blockSize)*N + 2*(N - 1) + N*N + 2*N; // maximum work size
          MultMem := BlockMultMemSize( blkMultSize );
          if Threaded then
             multMem := numUseCPUCores*MultMem;

          Result := Result*sizeof(double) + multMem;
     end;
end;

function ThrMtxEigUpperSymmetricMatrixInPlace2(A : PDouble; LineWidthA : NativeInt; N : NativeInt; W : PConstDoubleArr; EigValOnly : boolean; blockSize : integer; progress : TLinEquProgress = nil ) : TEigenvalueConvergence;
var eigWork : TSymEigRec;
    iworkSize : NativeInt;
    workSize : NativeInt;
begin
     // ###########################################
     // #### For small matrices don't use the threaded version
     if N < 64 then
     begin
          Result := MatrixEigUpperSymmetricMatrixInPlace2(A, LineWidthA, N, W, EigValOnly, blockSize);
          exit;
     end;

     // ###########################################
     // #### Initialize memory
     FillChar(eigWork, sizeof(eigWork), 0);

     eigWork.nb := Min(N, Max(2, blockSize));
     eigWork.D := PConstDoubleArr(W);
     eigWork.progress := progress;

     workSize := UperSymEigMemorySize( N, eigWork.nb, EigValOnly, True, eigWork.reflData.BlkMultSize);

     if EigValOnly then
     begin
          eigWork.E := MtxAllocAlign( workSize, // // Tau + E, nb*N for reduction of symmetric to tridiagonal
                                      eigWork.mem); // tau + E + work
          eigWork.LineWidthWork := eigWork.nb*sizeof(double);
          eigWork.tau := GenPtrArr(PDouble(eigWork.E), N - 1, 0, 0);
          eigWork.work := GenPtr( PDouble(eigWork.Tau), N - 1, 0, 0);
     end
     else
     begin
          iWorkSize := 0; // not yet implemented
          eigWork.E := MtxAllocAlign( workSize,  // Tau, E, Z + T + nb*n
                                      eigWork.mem);
          eigWork.tau := GenPtrArr(PDouble(eigWork.E), N - 1, 0, 0);

          eigWork.LineWidthZ := N*sizeof(double);
          eigWork.Z := GenPtr( PDouble(eigWork.tau), N - 1, 0, 0);

          eigWork.iWork := PIntegerArray( GenPtr(eigWork.Z, 0, N, eigWork.LineWidthZ) );

          eigWork.work := PDouble( eigWork.iWork );
          inc( PInteger(eigWork.work), iWorkSize );  // nb*N bytes left. nb*N is also used in InternalEigValEigVecFromSymTridiagMatrix
          eigWork.LineWidthWork := eigWork.nb*sizeof(double);
          eigWork.reflData.T := GenPtr(eigWork.work, 0, N, eigWork.LineWidthWork);
          eigWork.reflData.LineWidthT := eigWork.nb*sizeof(double);

          eigWork.reflData.blkMultSize := Min( N, QRMultBlockSize );
          eigWork.reflData.BlkMultMem := GenPtr(eigWork.reflData.T, 0, eigWork.nb, eigWork.reflData.LineWidthT);
     end;

     eigWork.reflData.MatrixMultT1 := {$IFDEF FPC}@{$ENDIF}ThrMatrixMultT1Ex;
     eigWork.reflData.MatrixMultT2 := {$IFDEF FPC}@{$ENDIF}ThrMatrixMultT2Ex;
     eigWork.reflData.MatrixMultEx := {$IFDEF FPC}@{$ENDIF}ThrMatrixMultEx;
     eigWork.ApplyPlaneRotSeqRVB := {$IFDEF FPC}@{$ENDIF}ThrApplyPlaneRotSeqRVB;
     eigWork.ApplyPlaneRotSeqRVF := {$IFDEF FPC}@{$ENDIF}ThrApplyPlaneRotSeqRVF;

     // ###########################################
     // #### Eigenvalue calculation
     Result := InternalSymEigValUpper(A, LineWidthA, N, EigValOnly, eigWork);
     FreeMem(eigWork.mem);
end;



end.
