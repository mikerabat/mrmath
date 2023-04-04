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

uses MatrixConst, MatrixASMStubSwitch;

type
  TQRDecompFunc = procedure (A : PDouble; const LineWidthA : NativeInt; width, height : NativeInt; tau : PDouble; work : PDouble; pnlSize : NativeInt; progress : TLinEquProgress);
  TQFromQRFunc = procedure (A : PDouble; const LineWidthA : NativeInt; width, height : NativeInt; tau : PDouble; BlockSize : NativeInt; work : PDouble; progress : TLinEquProgress);


// original functions from Numerical Recipies:
// In place QR decomposition. Constructs the QR decomposition of A (n*n). The upper triangle matrix R is returned
// in the upper triangle of a, except for the diagonal elements of R which are returned in
// d. The orthogonal matrix Q is represented as a product of n-1 Householder matrices Q1...Qn-1, where
// Qj = 1 - uj*(uj/cj). The ith component of uj is zero for i = 1..j-1 while the nonzero components are returned
// in a[i][j] for i = j...n . False is returned if no singularity was detected
procedure MatrixQRDecompInPlace(A : PDouble; const LineWidthA : NativeInt; width : NativeInt; C : PDouble; const LineWidthC : NativeInt;
  D : PDouble; const LineWidthD : NativeInt; progress : TLinEquProgress = nil);
procedure MatrixQRDecomp(dest : PDouble; const LineWidthDest : NativeInt; A : PDouble; const LineWidthA : NativeInt; width : NativeInt;
  C : PDouble; const LineWidthC : NativeInt; D : PDouble; const LineWidthD : NativeInt; progress : TLinEquProgress = nil);
// solves the System A*x = b. The input paramaters are the output parameters from the QR decomposition.
// b is the matrix right hand side and will be overwritten by the result x.
procedure MatrixQRSolveLinEq(A : PDouble; const LineWidthA : NativeInt; width : NativeInt; C : PDouble; const LineWidthC : NativeInt;
  D : PDouble; const LineWidthD : NativeInt; B : PDouble; const LineWidthB : NativeInt; progress : TLinEquProgress = nil);


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
procedure MatrixQRDecompInPlace2(A : PDouble; const LineWidthA : NativeInt; width, height : NativeInt; tau : PDouble; work : PDouble; pnlSize : NativeInt; progress : TLinEquProgress = nil); overload;
procedure MatrixQRDecompInPlace2(A : PDouble; const LineWidthA : NativeInt; width, height : NativeInt; tau : PDouble; progress : TLinEquProgress = nil); overload;

// according to the panel size and the global QRBlockmultsize we calculate here the needed memory (including some alignment buffer)
function QRDecompMemSize( pnlSize, width, height : NativeInt) : NativeInt;

// implementation of Lapacks dorgqr function: On start the matrix A and Tau contains the result of
// the MatrixQRDecompInPlace2 function (economy size QR Decomposition). On output A is replaced by the full Q
// matrix with orthonormal columns.
procedure MatrixQFromQRDecomp(A : PDouble; const LineWidthA : NativeInt; width, height : NativeInt; tau : PDouble; BlockSize : NativeInt; work : PDouble; progress : TLinEquProgress = nil); overload;
procedure MatrixQFromQRDecomp(A : PDouble; const LineWidthA : NativeInt; width, height : NativeInt; tau : PDouble; progress : TLinEquProgress = nil); overload;

procedure MatrixLeftQFromQRDecomp(A : PDouble; const LineWidthA : NativeInt; width, height : NativeInt;
 tau : PDouble; BlockSize : NativeInt; work : PDouble; progress : TLinEquProgress = nil);


// the following functions utilize the multithreaded matrix multiplication
// routines

// Threaded version of the matrix qr decomposition -> makes use of threaded matrix multiplications
procedure ThrMatrixQRDecomp(A : PDouble; const LineWidthA : NativeInt; width, height : NativeInt; tau : PDouble; work : PDouble; pnlSize : NativeInt; progress : TLinEquProgress = nil);

// Threaded version of the full Q creation -> makes use of threaded matrix multiplications
procedure ThrMatrixQFromQRDecomp(A : PDouble; const LineWidthA : NativeInt; width, height : NativeInt;
       tau : PDouble; BlockSize : NativeInt; work : PDouble; progress : TLinEquProgress = nil);

procedure ThrMatrixLeftQFromQRDecomp(A : PDouble; const LineWidthA : NativeInt; width, height : NativeInt;
  tau : PDouble; BlockSize : NativeInt; work : PDouble; progress : TLinEquProgress = nil);


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
procedure MatrixQRSolve(x : PDouble; const LineWidthX : NativeInt; A : PDouble; const LineWidthA : NativeInt;
                       y : PDouble; const LineWidthY : NativeInt; width, height : NativeInt; work : PDouble = nil);

procedure ThrMatrixQRSolve(x : PDouble; const LineWidthX : NativeInt; A : PDouble; const LineWidthA : NativeInt;
                       y : PDouble; const LineWidthY : NativeInt; width, height : NativeInt);


implementation

uses BlockSizeSetup, Classes, HouseholderReflectors, Math, ThreadedMatrixOperations, MathUtilFunc,
     MtxThreadPool;


// ###########################################
// #### only for internal use
// ###########################################

type
  TRecMtxQRDecompData = record
    pWorkMem : Pointer;
    work : PDouble;
    LineWidthWork : NativeInt;
    BlkMultMem : PDouble;
    BlkMultSize : integer;
    Progress : TLinEquProgress;
    qrWidth, qrHeight : NativeInt;
    actIdx : NativeInt;
    pnlSize : NativeInt;

    reflData : TBlockReflrec;
    
    MatrixMultT1 : TMatrixBlockedMultfunc;
    MatrixMultT2 : TMatrixBlockedMultfunc;
    MatrixMultEx : TMatrixBlockedMultfunc;
  end;

procedure InitReflectorBlk( var qrData : TRecMtxQRDecompData );
begin
     qrData.reflData.T := qrData.work;
     qrData.reflData.LineWidthT := qrData.LineWidthWork;
     qrData.reflData.BlkMultMem := qrData.BlkMultMem;
     qrData.reflData.BlkMultSize := qrData.BlkMultSize;
     qrData.reflData.MatrixMultT1 := qrData.MatrixMultT1;
     qrData.reflData.MatrixMultT2 := qrData.MatrixMultT2;
     qrData.reflData.MatrixMultEx := qrData.MatrixMultEx;
end;


procedure MatrixQRDecompInPlace(A : PDouble; const LineWidthA : NativeInt; width : NativeInt; C : PDouble; const LineWidthC : NativeInt;
  D : PDouble; const LineWidthD : NativeInt; progress : TLinEquProgress);
var i, j, k : NativeInt;
    scale, sigma, sum, tau : double;
    pA, pAj : PDouble;
    pC : PDouble;
    pD : PDouble;
    pAk : PDouble;
begin
     assert(LineWidthA >= width*sizeof(double), 'Dimension error');
     assert(LineWidthC >= sizeof(double), 'Dimension error');
     assert(LineWidthD >= sizeof(double), 'Dimension error');

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

     if Assigned(Progress) then
        progress(100);
end;

procedure MatrixQRDecomp(dest : PDouble; const LineWidthDest : NativeInt; A : PDouble; const LineWidthA : NativeInt; width : NativeInt;
  C : PDouble; const LineWidthC : NativeInt; D : PDouble; const LineWidthD : NativeInt; progress : TLinEquProgress);
begin
     assert(LineWidthA >= width*sizeof(double), 'Dimension error');
     assert(LineWidthDest >= width*sizeof(double), 'Dimension error');
     assert(LineWidthC >= sizeof(double), 'Dimension error');
     assert(LineWidthD >= sizeof(double), 'Dimension error');

     // copy A to dest
     MatrixCopy(dest, LineWidthDest, A, LineWidthA, width, width);
     MatrixQRDecompInPlace(dest, LineWidthDest, width, C, LineWidthC, D, LineWidthD, progress);
end;


procedure MatrixRSolve(A : PDouble; const LineWidthA : NativeInt; width : NativeInt; C : PDouble; const LineWidthC;
  D : PDouble; const LineWidthD : NativeInt; B : PDouble; const LineWidthB : NativeInt; progress : TLinEquProgress);
var i, j : NativeInt;
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

procedure MatrixQRSolveLinEq(A : PDouble; const LineWidthA : NativeInt; width : NativeInt; C : PDouble; const LineWidthC : NativeInt;
  D : PDouble; const LineWidthD : NativeInt; B : PDouble; const LineWidthB : NativeInt; progress : TLinEquProgress);
var i, j : NativeInt;
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
procedure MtxQRUnblocked(A : PDouble; LineWidthA : NativeInt; width, height : NativeInt; Tau : PDouble; const qrData : TRecMtxQRDecompData);
var k : NativeInt;
    i : NativeInt;
    pA : PDouble;
    pTau : PDouble;
    aii : double;
    pAlpha : PDouble;
    pC : PDouble;
begin
     k := min(width, height);

     pTau := Tau;
     for i := 0 to k - 1 do
     begin
          // Generate elemetary reflector H(i) to annihilate A(1+i:height-1,i);
          pAlpha := GenPtr(A, i, i, LineWidthA);
          pA := GenPtr(A, i, min(i + 1, height - 1), LineWidthA);

          GenElemHousholderRefl(pA, LineWidthA, height - i, pAlpha^, pTau);

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
procedure CreateTMtx(n, k : NativeInt; A : PDouble; LineWidthA : NativeInt; Tau : PDouble; const reflData : TBlockReflrec);
var i, j : NativeInt;
    pT : PDouble;
    pA1 : PDouble;
    T : PDouble;
    pAij : PDouble;
    pcAIJ : PConstDoubleArr;
    LineWidthWork : NativeInt;
begin
     assert(k <= n, 'Error k needs to be smaller than n');

     // it is assumed that mem is a memory block of at least (n, k) where the first (k, k) elements are reserved for T
     T := reflData.T;
     LineWidthWork := reflData.LineWidthT;
     for i := 0 to k - 1 do
     begin
          if Tau^ = 0 then
          begin
               // H = I
               pT := T;
               for j := 0 to i do
               begin
                    pT^ := 0;
                    inc(PByte(pT), reflData.LineWidthT);
               end;
          end
          else
          begin
               // general case

               // T(1:i-1,i) := - tau(i) * A(i:j,1:i-1)**T * A(i:j,i)          **T = Transposed

               // T(1:i-1,i) := - tau(i) * V(i:n,1:i-1)' * V(i:n,i) */

               pT := GenPtr(T, i, 0, reflData.LineWidthT );
               pcAIJ := GenPtrArr(A, 0, i, LineWidthA);
               for j := 0 to i - 1 do
               begin
                    pT^ := -tau^*pcAij^[j];
                    inc(PByte(pT), reflData.LineWidthT);
               end;

               pA1 := GenPtr(A, 0, i + 1, LineWidthA);
               pAij := GenPtr(A, i, i + 1, LineWidthA);
               pT := GenPtr(T, i, 0, LineWidthWork );

               MatrixMtxVecMultT(pT, reflData.LineWidthT, pA1, pAij, LineWidthA, LineWidthA, i, n - i - 1, -tau^, 1 );
          end;

          // dtrmv: upper triangle matrix mult T(1:i-1,i) := T(1:i-1, 1:i-1)*T(1:i-1,i)
          if i > 0 then
             MtxMultUpNoTranspNoUnitVec( T, reflData.LineWidthT, GenPtr( T, i, 0, reflData.LineWidthT ),
                                         reflData.LineWidthT, i );
          pT := GenPtr(T, i, i, reflData.LineWidthT);
          pT^ := Tau^;  // fill in last Tau

          inc(Tau);
     end;
end;

procedure InternalMatrixQRDecompInPlace2(A : PDouble; const LineWidthA : NativeInt; width, height : NativeInt; tau : PDouble; qrData : TRecMtxQRDecompData);
var k : NativeInt;
    idx : NativeInt;
    ib : NativeInt;
    pA : PDouble;
begin
     // ######################################################
     // #### original linpack DGEQRF routine
     k := Min(width, height);

     // check if the panel size fits the width/heigth -> if not adjust the panel size
     if (qrdata.pnlSize > 1) and (qrdata.pnlSize > k) then
        qrdata.pnlSize := max(2, k div 2);

     idx := 0;
     pA := A;

     if (qrdata.pnlSize >= 2) and (qrdata.pnlSize < k) then
     begin
          // calculate the qr decomposition for the current panel
          while idx < k - qrdata.pnlSize - 2 do
          begin
               ib := min(width - idx, qrdata.pnlSize);

               MtxQRUnblocked(pA, LineWidthA, ib, height - idx, Tau, qrData);

               // calculate T matrix
               if idx + ib <= height then
               begin
                    CreateTMtx(height - idx, ib, pA, LineWidthA, Tau, qrData.reflData);

                    // apply H to A from the left
                    ApplyBlockReflectorLFC(pA, LineWidthA, qrData.reflData, width - idx - ib, height - idx, ib, False);
               end;

               inc(qrData.actIdx, ib);

               inc(pA, ib);
               inc(PByte(pA),ib*LineWidthA);
               inc(idx, ib);
               inc(Tau, ib);
          end;
     end;

     // calculate the last panel
     MtxQRUnblocked(pA, LineWidthA, width - idx, height - idx, Tau, qrData);
end;

function QRDecompMemSize( pnlSize, width, height : NativeInt) : NativeInt;
begin
     Result := 64 + Max((width + height + 4)*sizeof(double), pnlSize*sizeof(double)*width + BlockMultMemSize(Min(Max(height, width), QRMultBlockSize)) );
end;

procedure MatrixQRDecompInPlace2(A : PDouble; const LineWidthA : NativeInt; width, height : NativeInt; tau : PDouble; progress : TLinEquProgress = nil); overload;
begin
     MatrixQRDecompInPlace2(A, LineWidthA, width, height, tau, nil, QRBlockSize, Progress);
end;

procedure MatrixQRDecompInPlace2(A : PDouble; const LineWidthA : NativeInt; width, height : NativeInt; tau : PDouble; work : PDouble; pnlSize : NativeInt; progress : TLinEquProgress = nil); overload;
var qrData : TRecMtxQRDecompData;
begin
     qrData.pWorkMem := nil;
     qrData.work := work;
     qrData.BlkMultMem := nil;
     qrData.BlkMultSize := Min( QRMultBlockSize, Max(height, width ) );
     qrData.Progress := progress;
     qrData.qrWidth := width;
     qrData.qrHeight := height;
     qrData.actIdx := 0;
     qrData.pnlSize := pnlSize;
     qrData.LineWidthWork := pnlSize*sizeof(double);
     qrData.MatrixMultT1 := {$IFDEF FPC}@{$ENDIF}MatrixMultT1Ex;
     qrData.MatrixMultT2 := {$IFDEF FPC}@{$ENDIF}MatrixMultT2Ex;
     qrData.MatrixMultEx := {$IFDEF FPC}@{$ENDIF}MatrixMultEx;

     if work = nil then
        qrData.work := MtxAllocAlign( QRDecompMemSize(pnlSize, width, height) + (height + $3)*sizeof(double), qrData.pWorkMem );

     // it's assumed that the work memory block may also be used
     // as blocked multiplication memory storage!
     qrData.BlkMultMem := qrData.work;
     inc(qrData.BlkMultMem, pnlSize*width);

     // reflector data
     InitReflectorBlk(qrData);
     
     InternalMatrixQRDecompInPlace2(A, LineWidthA, width, height, tau, qrData);

     if work = nil then
        FreeMem(qrData.pWorkMem);
end;

// generates an m by n real matrix Q with orthonormal columns,
//  which is defined as the first n columns of a product of k elementary
//  reflectors of order m
// k: the number of elemetary reflectors whose product defines the matrix Q. width >= K >= 0.
// original dorg2r from netlib
procedure InternalMatrixQFromQRDecomp(A : PDouble; const LineWidthA : NativeInt; width, height, k : NativeInt; tau : PDouble; const qrData : TRecMtxQRDecompData);
var pA : PDouble;
    pAii : PDouble;
    i, j : NativeInt;
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

procedure InternalBlkMatrixQFromQRDecomp(A : PDouble; const LineWidthA : NativeInt; width, height : NativeInt; tau : PDouble; qrData : TRecMtxQRDecompData);
var pA : PDouble;
    x, y : NativeInt;
    numIter : NativeInt;
    pTau : PDouble;
    counter: NativeInt;
    idx : NativeInt;
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
                    CreateTMtx(height - idx, qrData.pnlSize, pA, LineWidthA, pTau, qrData.reflData);
                    // apply H to A from the left
                    ApplyBlockReflectorLFC(pA, LineWidthA, qrData.reflData, width - idx - qrdata.pnlSize, height - idx, qrdata.pnlSize, True);
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

procedure MatrixQFromQRDecomp(A : PDouble; const LineWidthA : NativeInt; width, height : NativeInt;
  tau : PDouble; progress : TLinEquProgress = nil); overload;
begin
     MatrixQFromQRDecomp(A, LineWidthA, width, height, tau, QRBlockSize, nil, progress);
end;

procedure MatrixQFromQRDecomp(A : PDouble; const LineWidthA : NativeInt; width, height : NativeInt;
 tau : PDouble; BlockSize : NativeInt; work : PDouble; progress : TLinEquProgress = nil);
var qrData : TRecMtxQRDecompData;
begin
     qrData.pWorkMem := nil;
     qrData.work := work;
     qrData.BlkMultMem := nil;
     qrData.BlkMultSize := Min( QRMultBlockSize, Max( width, height ) );
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
        qrData.work := MtxAllocAlign(QRDecompMemSize(qrData.pnlSize, width, height), qrData.pWorkMem);

     // it's assumed that the work memory block may also be used
     // as blocked multiplication memory storage!
     qrData.BlkMultMem := qrData.work;
     inc(qrData.BlkMultMem, qrData.pnlSize*width);

     InitReflectorBlk(qrData);

     InternalBlkMatrixQFromQRDecomp(A, LineWidthA, width, height, tau, qrData);
     if not Assigned(work) then
        FreeMem(qrData.pWorkMem);
end;

// dlarft forward rowwise
procedure CreateTMtxR(n, k : NativeInt; A : PDouble; LineWidthA : NativeInt; Tau : PDouble; const qrData : TRecMtxQRDecompData);
var i, j : NativeInt;
    pT : PDouble;
    pA1 : PDouble;
    pDest : PDouble;
    pT1 : PConstdoubleArr;
    pt2 : PDouble;
    x, y : NativeInt;
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
procedure InternalMatrixQLeftFromQRDecomp(A : PDouble; const LineWidthA : NativeInt; width, height, k : NativeInt; tau : PDouble; const qrData : TRecMtxQRDecompData);
var pA : PDouble;
    pAii : PDouble;
    pC : PDouble;
    i, l, j : NativeInt;
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
procedure InternalBlkMatrixLeftQFromQRDecomp(A : PDouble; const LineWidthA : NativeInt; width, height : NativeInt; tau : PDouble; qrData : TRecMtxQRDecompData);
var pA : PDouble;
    x, y : NativeInt;
    numIter : NativeInt;
    pTau : PDouble;
    counter: NativeInt;
    idx : NativeInt;
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
                    ApplyBlockReflectorRFR(pA, LineWidthA, qrData.reflData, width - idx, height - idx, qrdata.pnlSize, True);
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

procedure MatrixLeftQFromQRDecomp(A : PDouble; const LineWidthA : NativeInt; width, height : NativeInt;
 tau : PDouble; BlockSize : NativeInt; work : PDouble; progress : TLinEquProgress = nil);
var qrData : TRecMtxQRDecompData;
begin
     qrData.pWorkMem := nil;
     qrData.work := work;
     qrData.BlkMultMem := nil;
     qrData.BlkMultSize := Min( QRMultBlockSize, Max( width, height ) );
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
        qrData.work := MtxAllocAlign( qrData.pnlSize*sizeof(double)*height + 64 + BlockMultMemSize(qrData.BlkMultSize), qrData.pWorkMem);

     // it's assumed that the work memory block may also be used
     // as blocked multiplication memory storage!
     qrData.BlkMultMem := qrData.work;
     inc(qrData.BlkMultMem, qrData.pnlSize*width);

     // reference block reflector structure
     InitReflectorBlk(qrData);

     InternalBlkMatrixLeftQFromQRDecomp(A, LineWidthA, width, height, tau, qrData);
     if not Assigned(work) then
        FreeMem(qrData.pWorkMem);
end;


// ######################################################
// ##### Threaded version of matrix QR decomposition
// ######################################################

procedure ThrMatrixQRDecomp(A : PDouble; const LineWidthA : NativeInt; width, height : NativeInt;
   tau : PDouble; work : PDouble; pnlSize : NativeInt; progress : TLinEquProgress = nil);
var qrData : TRecMtxQRDecompData;
    ptrMem : Pointer;
begin
     qrData.pWorkMem := nil;
     qrData.work := work;
     qrData.BlkMultMem := nil;
     qrData.BlkMultSize := Min( QRMultBlockSize, Max( width, height ) );
     qrData.Progress := progress;
     qrData.qrWidth := width;
     qrData.qrHeight := height;
     qrData.actIdx := 0;
     qrData.pnlSize := pnlSize;
     qrData.LineWidthWork := qrData.pnlSize*sizeof(double);
     qrData.MatrixMultT1 := {$IFDEF FPC}@{$ENDIF}ThrMatrixMultT1Ex;
     qrData.MatrixMultT2 := {$IFDEF FPC}@{$ENDIF}ThrMatrixMultT2Ex;
     qrData.MatrixMultEx := {$IFDEF FPC}@{$ENDIF}ThrMatrixMultEx;

     if work = nil then
        qrData.work := MtxAllocAlign(pnlSize*sizeof(double)*height + 64, qrData.pWorkMem );

     qrData.BlkMultMem := MtxMallocAlign(numCPUCores*(4 + BlockMultMemSize(qrData.BlkMultSize)), ptrMem);

     // reference block reflector structure
     InitReflectorBlk(qrData);

     InternalMatrixQRDecompInPlace2(A, LineWidthA, width, height, tau, qrData);

     if qrData.pWorkMem <> nil then
        FreeMem(qrData.pWorkMem);

     FreeMem(ptrMem);
end;

procedure ThrMatrixQFromQRDecomp(A : PDouble; const LineWidthA : NativeInt; width, height : NativeInt;
  tau : PDouble; BlockSize : NativeInt; work : PDouble; progress : TLinEquProgress = nil);
var qrData : TRecMtxQRDecompData;
    ptrMem : Pointer;
begin
     qrData.pWorkMem := nil;
     qrData.work := work;
     qrData.BlkMultMem := nil;
     qrData.BlkMultSize := Min( QRMultBlockSize, Max( width, height ) );
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
        qrData.work := MtxAllocAlign(BlockSize*sizeof(double)*height + 64, qrData.pWorkMem );

     qrData.BlkMultMem := MtxMallocAlign(numCPUCores*(4 + BlockMultMemSize(qrData.BlkMultSize)), ptrMem);

     // reference block reflector structure
     InitReflectorBlk(qrData);
     
     InternalBlkMatrixQFromQRDecomp(A, LineWidthA, width, height, tau, qrData);
     if Assigned(qrData.pWorkMem) then
        FreeMem(qrData.pWorkMem);

     FreeMem(ptrMem);
end;

procedure ThrMatrixLeftQFromQRDecomp(A : PDouble; const LineWidthA : NativeInt; width, height : NativeInt;
  tau : PDouble; BlockSize : NativeInt; work : PDouble; progress : TLinEquProgress = nil);
var qrData : TRecMtxQRDecompData;
    ptrMem : Pointer;
begin
     qrData.pWorkMem := nil;
     qrData.work := work;
     qrData.BlkMultMem := nil;
     qrData.BlkMultSize := Min( QRMultBlockSize, Max( width, height ) );
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
        qrData.work := MtxAllocAlign(BlockSize*sizeof(double)*height + 64, qrData.pWorkMem );

     qrData.BlkMultMem := MtxMallocAlign(numCPUCores*(4 + BlockMultMemSize(qrData.BlkMultSize)), ptrMem);

     // reference block reflector structure
     InitReflectorBlk(qrData);

     InternalBlkMatrixLeftQFromQRDecomp(A, LineWidthA, width, height, tau, qrData);
     if Assigned(qrData.pWorkMem) then
        FreeMem(qrData.pWorkMem);

     FreeMem(ptrMem);
end;


procedure InternalMatrixQRSolve(x : PDouble; const LineWidthX : NativeInt; A : PDouble; const LineWidthA : NativeInt;
    y : PDouble; const LineWidthY : NativeInt; width, height : NativeInt;
    QRDecompInPlaceFunc : TQRDecompFunc; QFromQRDecompFunc : TQFromQRFunc;
    work : PDouble = nil);
var mem : Pointer;
    tau : PDouble;
    Q : PDouble;
    LineWidthQ : NativeInt;
    w2 : NativeInt;
    qy : PDouble;
    pX, ppX : PDouble;
    pA : PConstDoubleArr;

    value : double;

    w : NativeInt;
    idxY : NativeInt;
    idxX : NativeInt;
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

          qrMem := qy;
     end
     else
     begin
          // reserve some memory
          w2 := width + width and $1;

          // reserve memory for tau and R
          tau := MtxAllocAlign( w2*height*sizeof(double) + 32 + 2*w2*sizeof(double), mem );
          Q := tau;
          inc(Q, w2);

          qy := Q;
          inc(qy, w2*height);
     end;

     LineWidthQ := w2*sizeof(double);

     QRDecompInPlaceFunc(A, LineWidthA, width, height, tau, qrMem, QRBlockSize, nil);
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
procedure MatrixQRSolve(x : PDouble; const LineWidthX : NativeInt; A : PDouble; const LineWidthA : NativeInt;
                       y : PDouble; const LineWidthY : NativeInt; width, height : NativeInt; work : PDouble = nil);
begin
     assert( height >= width, 'Error function only defined for height >= width');
     
     InternalMatrixQRSolve(x, LineWidthX, A, LineWidthA, y, LineWidthY, width, height,
                          {$IFDEF FPC}@{$ENDIF}MatrixQRDecompInPlace2,
                          {$IFDEF FPC}@{$ENDIF}MatrixQFromQRDecomp,
                          work);
end;

procedure ThrMatrixQRSolve(x : PDouble; const LineWidthX : NativeInt; A : PDouble; const LineWidthA : NativeInt;
  y : PDouble; const LineWidthY : NativeInt; width, height : NativeInt);
begin
     assert( height >= width, 'Error function only defined for height >= width');

     // check if multithreading is a good choice (note this parameter is a crude one...)
     if (width > 8*QRBlockSize) 
     then
         InternalMatrixQRSolve(x, LineWidthX, A, LineWidthA, y, LineWidthY, width, height,
                               {$IFDEF FPC}@{$ENDIF}ThrMatrixQRDecomp,
                               {$IFDEF FPC}@{$ENDIF}ThrMatrixQFromQRDecomp,
                               nil)
     else
         InternalMatrixQRSolve(x, LineWidthX, A, LineWidthA, y, LineWidthY, width, height,
                               {$IFDEF FPC}@{$ENDIF}MatrixQRDecompInPlace2,
                               {$IFDEF FPC}@{$ENDIF}MatrixQFromQRDecomp,
                               nil)
end;


end.
