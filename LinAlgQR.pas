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
// #### Implementation of the QR decomposition
// #### based on numerical recipies and a lapacks DGEQRF

unit LinAlgQR;

interface

uses MatrixConst, OptimizedFuncs;

type
  TQRDecompFunc = function (A : PDouble; const LineWidthA : TASMNativeInt; width, height : TASMNativeInt; tau : PDouble; work : PDouble; pnlSize : TASMNativeInt; progress : TLinEquProgress) : TQRResult;
  TQFromQRFunc = procedure (A : PDouble; const LineWidthA : TASMNativeInt; width, height : TASMNativeInt; tau : PDouble; BlockSize : TASMNativeInt; work : PDouble; progress : TLinEquProgress);


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

procedure MatrixLeftQFromQRDecomp(A : PDouble; const LineWidthA : TASMNativeInt; width, height : TASMNativeInt;
 tau : PDouble; BlockSize : TASMNativeInt; work : PDouble; progress : TLinEquProgress = nil);


// the following functions utilize the multithreaded matrix multiplication
// routines

// Threaded version of the matrix qr decomposition -> makes use of threaded matrix multiplications
function ThrMatrixQRDecomp(A : PDouble; const LineWidthA : TASMNativeInt; width, height : TASMNativeInt; tau : PDouble; work : PDouble; pnlSize : TASMNativeInt; progress : TLinEquProgress = nil) : TQRResult;

// Threaded version of the full Q creation -> makes use of threaded matrix multiplications
procedure ThrMatrixQFromQRDecomp(A : PDouble; const LineWidthA : TASMNativeInt; width, height : TASMNativeInt;
       tau : PDouble; BlockSize : TASMNativeInt; work : PDouble; progress : TLinEquProgress = nil);

procedure ThrMatrixLeftQFromQRDecomp(A : PDouble; const LineWidthA : TASMNativeInt; width, height : TASMNativeInt;
  tau : PDouble; BlockSize : TASMNativeInt; work : PDouble; progress : TLinEquProgress = nil);


// ###########################################
// #### Solve overdetermined equation system via QR decomposition (least squares)
// ###########################################

// solves A*x = y  A is width x height where height >= width; x and y are vectors
// -> implements the least squares approach as:
// -> A' * A * x = y        | QR decomposotion on A
//    R' * Q' * Q * R * x = R' * Q' * y
//    R' * R * x = R' *Q' * y   | Q'*Q = I
//    R * x = Q' * b             when R is non signular
//  -> solve R*x = Q' * b by back substitution

// if work is provided it needs to be at least (max( width*height, QRDecompMemSize) + 2*width)*sizeof(double) and for best performance
// alligned to 16bytes
function MatrixQRSolve(x : PDouble; const LineWidthX : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; 
                       y : PDouble; const LineWidthY : TASMNativeInt; width, height : TASMNativeInt; work : PDouble = nil) : TQRResult;

function ThrMatrixQRSolve(x : PDouble; const LineWidthX : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; 
                       y : PDouble; const LineWidthY : TASMNativeInt; width, height : TASMNativeInt) : TQRResult;


implementation

uses BlockSizeSetup, Classes, HouseholderReflectors, Math, ThreadedMatrixOperations, MathUtilFunc,
     MtxThreadPool;


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
    MatrixMultEx : TMatrixBlockedMultfunc;
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
    pT : PDouble;
    pA1 : PDouble;
    pDest : PDouble;
    pT1 : PConstDoublearr;
    pT2 : PDouble;
    x, y : TASMNativeInt;
    tmp : Double;
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

               MatrixMtxVecMultT(pT, qrData.LineWidthWork, pA1, pAij, LineWidthA, LineWidthA, i, n - i - 1, -tau^, 1 );
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
              MtxMultTria2T1StoreT1(mem, LineWidthMem, T, LineWidthT, widthT, heightW, WidthT, WidthT)
          else
              MtxMultTria2Store1(mem, LineWidthMem, T, LineWidthT, widthT, heightW, WidthT, WidthT);

          // C2 = C2 - Y2*W'
          qrData.MatrixMultT2(pC2, LineWidthA, pY2, mem, widthT, height - widthT, widthT, heightW,
                              LineWidthA, LineWidthMem, QRMultBlockSize, doSub, qrData.BlkMultMem);

          // W = W*Y1' (lower left part of Y1! -> V1)
          MtxMultLowTria2T2Store1(mem, LineWidthMem, A, LineWidthA, WidthT, heightW, widthT, widthT);
     end;

     // C1 = C1 - W'
     MatrixSubT(pC1, LineWidthA, mem, LineWidthMem, width - widthT, widthT);
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
          qrData.pWorkMem := MtxAlloc( QRDecompMemSize(pnlSize, width) );
          qrData.work := PDouble(qrData.pWorkMem);
          if (NativeUInt(qrData.pWorkMem) and $0000000F) <> 0 then
             qrData.work := PDouble(NativeUInt(qrData.pWorkMem) + 16 - NativeUInt(qrData.pWorkMem) and $0F);
     end;

     // it's assumed that the work memory block may also be used
     // as blocked multiplication memory storage!
     qrData.BlkMultMem := qrData.work;
     inc(qrData.BlkMultMem, pnlSize*width);

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
             qrData.Progress((qrData.actIdx + (k-i) - i)*100 div qrData.qrWidth);
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
     qrData.MatrixMultEx := {$IFDEF FPC}@{$ENDIF}MatrixMultEx;

     if work = nil then
     begin
          qrData.pWorkMem := MtxAlloc( QRDecompMemSize(qrData.pnlSize, width) );
          qrData.work := PDouble(qrData.pWorkMem);
          if (NativeUInt(qrData.pWorkMem) and $0000000F) <> 0 then
             qrData.work := PDouble(NativeUInt(qrData.pWorkMem) + 16 - NativeUInt(qrData.pWorkMem) and $0F);
     end;

     // it's assumed that the work memory block may also be used
     // as blocked multiplication memory storage!
     qrData.BlkMultMem := qrData.work;
     inc(qrData.BlkMultMem, qrData.pnlSize*width);

     InternalBlkMatrixQFromQRDecomp(A, LineWidthA, width, height, tau, qrData);
     if not Assigned(work) then
        freeMem(qrData.pWorkMem);
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
     MtxMultTria2TUpperUnit(W, LineWidthMem, pC1, LineWidthA, pV1, LineWidthA, widthC1, heightC1, widthV1, heightV1);

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
         MtxMultTria2T1StoreT1(W, LineWidthMem, T, LineWidthT, widthW, heightW, WidthT, WidthT)
     else
         MtxMultTria2Store1(W, LineWidthMem, T, LineWidthT, widthW, heightW, WidthT, WidthT);

     // C2 := C2 - W * V2
     if width > widthT then
        qrData.MatrixMultEx(pC2, LineWidthA, W, pV2, widthW, heightW, widthV2, heightV2, LineWidthMem,
                     LineWidthA, QRMultBlockSize, doSub, qrData.BlkMultMem);

     // W := W * V1
     MtxMultTria2Store1Unit(W, LineWidthMem, pV1, LineWidthA, widthW, heightW, widthV1, heightV1);

     // C1 := C1 - W
     MatrixSub(pC1, LineWidthA, pC1, W, widthC1, heightC1, LineWidthA, LineWidthMem);
end;


// dlarft forward rowwise
procedure CreateTMtxR(n, k : TASMNativeInt; A : PDouble; LineWidthA : TASMNativeInt; Tau : PDouble; const qrData : TRecMtxQRDecompData);
var i, j : TASMNativeInt;
    pT : PDouble;
    pA1 : PDouble;
    pDest : PDouble;
    pT1 : PConstdoubleArr;
    pt2 : PDouble;
    x, y : TASMNativeInt;
    tmp : Double;
    T : PDouble;
    pAij : PDouble;
begin
     assert(k <= n, 'Error k needs to be smaller than n');

     // it is assumed that mem is a memory block of at least (n, k) where the first (k, k) elements are reserved for T
     T := qrData.work;    // kxk

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
    pcA : PConstDoubleArr;
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
          pcA := PConstDoubleArr( GenPtr(A, 0, i, LineWidthA) );
          for l := 0 to i - 1 do
              pcA^[l] := 0;

          dec(tau);

          // ###########################################
          // #### Progress
          if Assigned(qrData.Progress) then
             qrData.Progress((qrData.actIdx + (k-1) - i)*100 div qrData.qrWidth);
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
     qrData.MatrixMultEx := {$IFDEF FPC}@{$ENDIF}MatrixMultEx;
     if work = nil then
     begin
          qrData.pWorkMem := MtxAlloc(qrData.pnlSize*sizeof(double)*height + 64 + BlockMultMemSize(QRMultBlockSize) );
          qrData.work := PDouble(qrData.pWorkMem);
          if (NativeUInt(qrData.pWorkMem) and $0000000F) <> 0 then
             qrData.work := PDouble(NativeUInt(qrData.pWorkMem) + 16 - NativeUInt(qrData.pWorkMem) and $0F);
     end;

     // it's assumed that the work memory block may also be used
     // as blocked multiplication memory storage!
     qrData.BlkMultMem := qrData.work;
     inc(qrData.BlkMultMem, qrData.pnlSize*width);

     InternalBlkMatrixLeftQFromQRDecomp(A, LineWidthA, width, height, tau, qrData);
     if not Assigned(work) then
        freeMem(qrData.pWorkMem);
end;


// ######################################################
// ##### Threaded version of matrix QR decomposition
// ######################################################

function ThrMatrixQRDecomp(A : PDouble; const LineWidthA : TASMNativeInt; width, height : TASMNativeInt;
   tau : PDouble; work : PDouble; pnlSize : TASMNativeInt; progress : TLinEquProgress = nil) : TQRResult;
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
     qrData.MatrixMultT1 := {$IFDEF FPC}@{$ENDIF}ThrMatrixMultT1Ex;
     qrData.MatrixMultT2 := {$IFDEF FPC}@{$ENDIF}ThrMatrixMultT2Ex;

     if work = nil then
     begin
          qrData.pWorkMem := MtxAlloc(pnlSize*sizeof(double)*height + 64 );
          qrData.work := PDouble(qrData.pWorkMem);
          if (NativeUInt(qrData.pWorkMem) and $0000000F) <> 0 then
             qrData.work := PDouble(NativeUInt(qrData.pWorkMem) + 16 - NativeUInt(qrData.pWorkMem) and $0F);
     end;

     qrData.BlkMultMem := GetMemory(numCPUCores*(4 + BlockMultMemSize(QRMultBlockSize)));

     res := InternalMatrixQRDecompInPlace2(A, LineWidthA, width, height, tau, qrData);

     if work = nil then
        FreeMem(qrData.pWorkMem);

     FreeMem(qrData.BlkMultMem);

     if res
     then
         Result := qrOK
     else
         Result := qrSingular;
end;

procedure ThrMatrixQFromQRDecomp(A : PDouble; const LineWidthA : TASMNativeInt; width, height : TASMNativeInt;
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
     qrData.MatrixMultT1 := {$IFDEF FPC}@{$ENDIF}ThrMatrixMultT1Ex;
     qrData.MatrixMultT2 := {$IFDEF FPC}@{$ENDIF}ThrMatrixMultT2Ex;
     qrData.MatrixMultEx := {$IFDEF FPC}@{$ENDIF}ThrMatrixMultEx;

     if work = nil then
     begin
          qrData.pWorkMem := MtxAlloc(BlockSize*sizeof(double)*height + 64 );
          qrData.work := PDouble(qrData.pWorkMem);
          if (NativeUInt(qrData.pWorkMem) and $0000000F) <> 0 then
             qrData.work := PDouble(NativeUInt(qrData.pWorkMem) + 16 - NativeUInt(qrData.pWorkMem) and $0F);
     end;

     qrData.BlkMultMem := GetMemory(numCPUCores*(4 + BlockMultMemSize(QRMultBlockSize)));

     InternalBlkMatrixQFromQRDecomp(A, LineWidthA, width, height, tau, qrData);
     if not Assigned(work) then
        freeMem(qrData.pWorkMem);

     FreeMem(qrData.BlkMultMem);
end;

procedure ThrMatrixLeftQFromQRDecomp(A : PDouble; const LineWidthA : TASMNativeInt; width, height : TASMNativeInt;
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
     qrData.MatrixMultT1 := {$IFDEF FPC}@{$ENDIF}ThrMatrixMultT1Ex;
     qrData.MatrixMultT2 := {$IFDEF FPC}@{$ENDIF}ThrMatrixMultT2Ex;
     qrData.MatrixMultEx := {$IFDEF FPC}@{$ENDIF}ThrMatrixMultEx;

     if work = nil then
     begin
          qrData.pWorkMem := MtxAlloc(BlockSize*sizeof(double)*height + 64 );
          qrData.work := PDouble(qrData.pWorkMem);
          if (NativeUInt(qrData.pWorkMem) and $0000000F) <> 0 then
             qrData.work := PDouble(NativeUInt(qrData.pWorkMem) + 16 - NativeUInt(qrData.pWorkMem) and $0F);
     end;

     qrData.BlkMultMem := GetMemory(numCPUCores*(4 + BlockMultMemSize(QRMultBlockSize)));

     InternalBlkMatrixLeftQFromQRDecomp(A, LineWidthA, width, height, tau, qrData);
     if not Assigned(work) then
        freeMem(qrData.pWorkMem);

     FreeMem(qrData.BlkMultMem);
end;


function InternalMatrixQRSolve(x : PDouble; const LineWidthX : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; 
    y : PDouble; const LineWidthY : TASMNativeInt; width, height : TASMNativeInt; 
    QRDecompInPlaceFunc : TQRDecompFunc; QFromQRDecompFunc : TQFromQRFunc;
    work : PDouble = nil) : TQRResult;
var mem : PByte;
    tau : PDouble;
    Q : PDouble;
    LineWidthQ : TASMNativeInt;
    w2 : TASMNativeInt;
    qy : PDouble;
    pX, ppX : PDouble;
    pA : PConstDoubleArr;

    value : double;

    w : TASMNativeInt;
    idxY : TASMNativeInt;
    idxX : TASMNativeInt;
    qrMem : PDouble;
begin
     assert( height >= width, 'Error function only defined for height >= width');
     
     qrMem := nil;
     if Assigned(work) then
     begin
          mem := PByte(work);
          w2 := width;
          tau := work;
          Q := tau;
          inc(Q, w2);
          qy := Q;
          inc(qy, w2*height);

          qrMem := Q;
     end
     else
     begin
          // reserve some meory
          w2 := width + width and $1;

          // reserve memory for tau and R
          mem := MtxAlloc( w2*height*sizeof(double) + 32 + 2*w2*sizeof(double) );
          tau := PDouble(mem);
          if TASMNativeUInt( mem ) and $F <> 0 then
             tau := PDouble(NativeUInt(mem) + 16 - NativeUInt(mem) and $0F);
          Q := tau;
          inc(Q, w2);

          qy := Q;
          inc(qy, w2*height);
     end;

     LineWidthQ := w2*sizeof(double);

     Result := QRDecompInPlaceFunc(A, LineWidthA, width, height, tau, qrMem, QRBlockSize, nil);

     if Result <> qrOK then
     begin
          FreeMem(mem);
          exit;
     end;

     MatrixCopy(Q, LineWidthQ, A, LineWidthA, width, height);
     QFromQRDecompFunc(Q, LineWidthQ, width, height, tau, QRBlockSize, qrMem, nil);
     

     // calcualte Q'*y -> store in qy
     MatrixMtxVecMultT(qy, sizeof(double), Q, y, LineWidthQ, LineWidthY, width, height, 1, 0);

     w := Min(width, height); 
     
     pX := x;
     inc(PByte(pX), (w - 1)*LineWidthX );
     ppX := pX;
     pA := PConstDoubleArr(GenPtr(A, 0, w - 1, LineWidthA));

     // back substitue R*x = qy
     for idxY := w - 1 downto 0 do
     begin
          value := 0;
          for idxX := idxY + 1 to w - 1 do
          begin
               value := value + pA^[idxX]*ppX^;
               inc(PByte(ppX), LineWidthX);
          end;
          
          value := PConstDoubleArr(qy)^[idxY] - value;
          pX^ := value/ pA^[idxY];

          ppX := pX;
          dec(PByte(pX), LineWidthX);
          dec(PByte(pA), LineWidthA);
     end;

     if work = nil then
        FreeMem( mem );
end;

// solves A*x = y  A is width x height where height >= width; x and y are vectors
function MatrixQRSolve(x : PDouble; const LineWidthX : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; 
                       y : PDouble; const LineWidthY : TASMNativeInt; width, height : TASMNativeInt; work : PDouble = nil) : TQRResult;
begin
     assert( height >= width, 'Error function only defined for height >= width');
     
     Result := InternalMatrixQRSolve(x, LineWidthX, A, LineWidthA, y, LineWidthY, width, height, 
                                     {$IFDEF FPC}@{$ENDIF}MatrixQRDecompInPlace2, 
                                     {$IFDEF FPC}@{$ENDIF}MatrixQFromQRDecomp, 
                                     work);
end;

function ThrMatrixQRSolve(x : PDouble; const LineWidthX : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; 
                       y : PDouble; const LineWidthY : TASMNativeInt; width, height : TASMNativeInt) : TQRResult;
begin
     assert( height >= width, 'Error function only defined for height >= width');

     // check if multithreading is a good choice (note this parameter is a crude one...)
     if (width > 8*QRBlockSize) 
     then
         Result := InternalMatrixQRSolve(x, LineWidthX, A, LineWidthA, y, LineWidthY, width, height,
                                        {$IFDEF FPC}@{$ENDIF}ThrMatrixQRDecomp, 
                                        {$IFDEF FPC}@{$ENDIF}ThrMatrixQFromQRDecomp, 
                                        nil)
     else
         Result := InternalMatrixQRSolve(x, LineWidthX, A, LineWidthA, y, LineWidthY, width, height,
                                        {$IFDEF FPC}@{$ENDIF}MatrixQRDecompInPlace2, 
                                        {$IFDEF FPC}@{$ENDIF}MatrixQFromQRDecomp, 
                                        nil)
end;


end.
