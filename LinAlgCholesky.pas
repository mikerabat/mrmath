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

unit LinAlgCholesky;

interface

uses MatrixConst;

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


// Lapack version of cholesky decomposition
function MatrixCholeskyInPlace2(A : PDouble; const LineWidthA : TASMNativeInt; width : TASMNativeInt; progress : TLinEquProgress = nil) : TCholeskyResult;
function MatrixCholeskyInPlace3(A : PDouble; const LineWidthA : TASMNativeInt; width : TASMNativeInt; progress : TLinEquProgress = nil) : TCholeskyResult;
function MatrixCholeskyInPlace4(A : PDouble; const LineWidthA : TASMNativeInt; width : TASMNativeInt; pnlSize : TASMNativeInt; progress : TLinEquProgress = nil) : TCholeskyResult;

// threaded version of Cholesky decomposition which makes use of the threaded matrix multiplication routine
function ThrMatrixCholeskyDecomp(A : PDouble; const LineWidthA : TASMNativeInt; width : TASMNativeInt; pnlSize : integer; work : PDouble; progress : TLinEquProgress) : TCholeskyResult;


implementation

uses OptimizedFuncs, SimpleMatrixOperations, BlockSizeSetup, Math, 
  MtxThreadPool, ThreadedMatrixOperations;

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
     multMem := AllocMem(BlockMultMemSize(pnlSize));
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

// ###########################################
// #### Cholesky decomposition
// ###########################################

function ThrMatrixCholeskyDecomp(A : PDouble; const LineWidthA : TASMNativeInt; width : TASMNativeInt; pnlSize : integer; work : PDouble; progress : TLinEquProgress) : TCholeskyResult;
var data : PDouble;
    multMem : PDouble;
begin
     if pnlSize = 0 then
        pnlSize := CholBlockSize;
        
     data := work;
     if work = nil then
        data := AllocMem(numCPUCores*( 4 + BlockMultMemSize(pnlSize) ) );

     multMem := data;
     if (NativeUInt(data) and $0000000F) <> 0 then
        multMem := PDouble(NativeUInt(data) + 16 - NativeUInt(data) and $0F); 

     Result := InternalBlkCholeskyInPlace(A, LineWidthA, width, pnlSize, {$IFDEF FPC}@{$ENDIF}ThrMatrixMultT2Ex, multMem, progress);

     if work = nil then
        FreeMem(data);
end;

// ###########################################
// #### NR functions
// ###########################################

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

end.
