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

uses MatrixConst, MatrixASMStubSwitch;

function GenElemHousholderRefl(A : PDouble; LineWidthA : TASMNativeInt; Height : TASMNativeInt; var Alpha : double; Tau : PDouble) : boolean;

procedure ApplyElemHousholderReflLeft(V : PDouble; LineWidthV : TASMNativeInt; C : PDouble; const LineWidthC : TASMNativeInt; width, height : TASMNativeInt;
  Tau : PDouble; Work : PDouble);

// work needs to be at least w4 + h4 where w4 is the next number of width that is divisable by 4 without rest and same for h4
procedure ApplyElemHousholderReflRight(V : PDouble; LineWidthV : TASMNativeInt; C : PDouble; const LineWidthC : TASMNativeInt; width, height : TASMNativeInt;
  Tau : PDouble; Work : PDouble);


  // ###########################################
// #### Base implementation of Block reflectors for the Lapack decompositions
type
  TBlockReflrec = record
  //   work : PDouble;
  //   LineWidthWork : TASMNativeInt;
     T : PDouble;
     LineWidthT : TASMNativeInt;

     BlkMultSize : TASMNativeInt;
     BlkMultMem : PDouble;

     MatrixMultT1 : TMatrixBlockedMultfunc;
     MatrixMultT2 : TMatrixBlockedMultfunc;
     MatrixMultEx : TMatrixBlockedMultfunc;
  end;

// apply block reflector to a matrix
// original DLARFB in Lapack - Left, Transpose, Forward, Columnwise
procedure ApplyBlockReflectorLFC(A : PDouble; LineWidthA : TASMNativeInt; const reflData : TBlockReflrec;
  width, height : TASMNativeInt; k : TASMNativeInt; Transposed : boolean);


// block reflector right transposed forward rowwise
procedure ApplyBlockReflectorRFR(A : PDouble; LineWidthA : TASMNativeInt; const reflData : TBlockReflrec; //const qrData : TRecMtxQRDecompData;
  width, height : TASMNativeInt; widthT : TASMNativeInt; Transposed : boolean);

// block reflector left, No transpose, backward, columnwise
procedure ApplyBlockReflectorLBC(V : PDouble; LineWidthV : TASMNativeInt;
  C : PDouble; LineWidthC : TASMNativeInt; work : PDouble; LineWidthWork : TASMNativeInt;
  width, height : TASMNativeInt; k : TASMNativeInt; Transposed : boolean; const reflData : TBlockReflrec);


implementation

uses BlockSizeSetup, MathUtilFunc;

// "right" part of dlarf
procedure ApplyElemHousholderReflRight(V : PDouble; LineWidthV : TASMNativeInt; C : PDouble; const LineWidthC : TASMNativeInt; width, height : TASMNativeInt;
  Tau : PDouble; Work : PDouble);
var pVWork : PDouble;
    h4 : TASMNativeInt;
begin
     // work = A(1:m, 2:n)T*A(1:m, 1)
     if tau^ <> 0 then
     begin
          // note: the original routine has here an option C -> but in all cases it's
          // A + 1

          h4 := height;
          if height and $03 <> 0 then
             h4 := height + 4 - height and $03;
          pVWork := V;
          if LineWidthV <> sizeof(double) then
          begin
               pVWork := GenPtr(Work, h4, 0, sizeof(double));
               // make it a vector
               MatrixCopy(pVWork, sizeof(double), V, LineWidthV, 1, width);
          end;
          
          MatrixMtxVecMult(work, sizeof(double), C, pVWork, LineWidthC, sizeof(double), width, height, 1, 0);
          MatrixRank1Update(C, LineWidthC, width, height, -Tau^, work, pVWork, sizeof(double), sizeof(double));

          // todo: there is some other scanning going on for non zero columns...
          // do the basic operation here...
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

     xNorm := MatrixElementwiseNorm2(A, LineWidthA, 1, height, True);
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

          xnorm := MatrixElementwiseNorm2(A, LineWidthA, 1, height, True);
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

// ###########################################
// #### Blocked reflectors
// ###########################################

// apply block reflector to a matrix
// original DLARFB in Lapack - Left, Transpose, Forward, Columnwise
procedure ApplyBlockReflectorLFC(A : PDouble; LineWidthA : TASMNativeInt; const reflData : TBlockReflrec;
  width, height : TASMNativeInt; k : TASMNativeInt; Transposed : boolean);
var pC1, pC2 : PDouble;
    pV1, pV2 : PDouble;
    T : PDouble;
    LineWidthT : TASMNativeInt;
    mem : PDouble;
    LineWidthMem : TASMNativeInt;
    //s : string;
    //s1, s2 : string;
begin
     if (width <= 0) or (height <= 0) then
        exit;

     mem := reflData.T;
     inc(PByte(mem), k*reflData.LineWidthT);  // upper part (nb x nb) is dedicated to T, lower part to W in dlarfb
     LineWidthMem := reflData.LineWidthT;

     T := reflData.T;
     //s := WriteMtx(T, reflData.LineWidthT, 3, 3);
     lineWidthT := reflData.LineWidthT;

     // (I - Y*T*Y')xA_trailing
     pV1 := A;
     pV2 := GenPtr(A, 0, k, LineWidthA);
     pC1 := GenPtr(A, k, 0, LineWidthA);
     pC2 := GenPtr(A, k, k, LineWidthA);

     // W = C1'*V1
     MatrixMultTria2T1(mem, LineWidthMem, pC1, LineWidthA, pV1, LineWidthA, width, k, k, k);

     //s := WriteMtx( mem, LineWidthMem, k, 5 );

     // W = W + C2'*V2
     if height > k then
        reflData.MatrixMultT1(mem, LineWidthMem, pC2, pV2, Width, height - k, k, height - k, LineWidthA, LineWidthA, reflData.BlkMultSize, doAdd, reflData.BlkMultMem);

     // W = W * T (using dtrm)  or W = W*T'
     if transposed
     then
         MtxMultRightUpperTriaNoUnitT2(mem, LineWidthMem, T, LineWidthT, k, width, k, k)
     else
         MtxMultRightUpperTriaNoUnit(mem, LineWidthMem, T, LineWidthT, k, width, k, k);

     if height > k then
     begin
          // C2 = C2 - V2*W'
          reflData.MatrixMultT2(pC2, LineWidthA, pV2, mem, k, height - k, k, width,
                              LineWidthA, LineWidthMem, reflData.BlkMultSize, doSub, reflData.BlkMultMem);

          // W = W*V1' (lower left part of Y1! -> V1)
          MtxMultRightLowerTriaUnitT2(mem, LineWidthMem, A, LineWidthA, k, width, k, k);
     end;

     // C1 = C1 - W'
     MatrixSubT(pC1, LineWidthA, mem, LineWidthMem, width, k);
end;

// dlarfb 'Right', 'Transpose', 'Forward', 'Rowwise'
procedure ApplyBlockReflectorRFR(A : PDouble; LineWidthA : TASMNativeInt; const reflData : TBlockReflrec;
  width, height : TASMNativeInt; widthT : TASMNativeInt; Transposed : boolean);
var pC1, pC2 : PDouble;
    pV1, pV2 : PDouble;
    T : PDouble;
    LineWidthT : TASMNativeInt;
    LineWidthW : TASMNativeInt;
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
     // upper part (nb x nb) is dedicated to T, lower part to W in dlarfb
     W := GenPtr(reflData.T, 0, widthT, reflData.LineWidthT);
     LineWidthW := reflData.LineWidthT;

     T := reflData.T;
     LineWidthT := reflData.LineWidthT;

     pC1 := GenPtr(A, 0, widthT, LineWidthA);
     pC2 := pC1;
     inc(pC2, widthT);

     pV1 := A; // GenPtr(A, 0, 0, LineWidthA);
     pV2 := GenPtr(pV1, widthT, 0, LineWidthA);

     // W := W * V1**T  // dtrm right upper transpose unit combined with copy
     MtxMultTria2TUpperUnit(W, LineWidthW, pC1, LineWidthA, pV1, LineWidthA, widthC1, heightC1, widthV1, heightV1);

     if width > widthT then
     begin
          //  W := W + C2 * V2**T
          reflData.MatrixMultT2(W, LineWidthW, pC2, pV2,
                              widthC2, heightC2, widthV2, heightV2,
                              LineWidthA, LineWidthA, reflData.BlkMultSize, doAdd, reflData.BlkMultMem);
     end;

     // W := W * T  or  W * T**T
     if transposed
     then
         MtxMultRightUpperTriaNoUnitT2(W, LineWidthW, T, LineWidthT, widthW, heightW, WidthT, WidthT)
     else
         MtxMultRightUpperTriaNoUnit(W, LineWidthW, T, LineWidthT, widthW, heightW, WidthT, WidthT);

     // C2 := C2 - W * V2
     if width > widthT then
        reflData.MatrixMultEx(pC2, LineWidthA, W, pV2, widthW, heightW, widthV2, heightV2, LineWidthW,
                     LineWidthA, reflData.BlkMultSize, doSub, reflData.BlkMultMem);

     // W := W * V1
     MtxMultRightUpperTriaUnit(W, LineWidthW, pV1, LineWidthA, widthW, heightW, widthV1, heightV1);

     // C1 := C1 - W
     MatrixSub(pC1, LineWidthA, pC1, W, widthC1, heightC1, LineWidthA, LineWidthW);
end;


(*
 W := C**T * V  =  (C1**T * V1 + C2**T * V2)  (stored in WORK)
 *
 *              W := C2**T
 *
                DO 70 j = 1, k
                   CALL dcopy( n, c( m-k+j, 1 ), ldc, work( 1, j ), 1 )
    70          CONTINUE
 *
 *              W := W * V2
 *
                CALL dtrmm( 'Right', 'Upper', 'No transpose', 'Unit', n,
      $                     k, one, v( m-k+1, 1 ), ldv, work, ldwork )
                IF( m.GT.k ) THEN
 *
 *                 W := W + C1**T * V1
 *
                   CALL dgemm( 'Transpose', 'No transpose', n, k, m-k,
      $                        one, c, ldc, v, ldv, one, work, ldwork )
                END IF
 *
 *              W := W * T**T  or  W * T
 *
                CALL dtrmm( 'Right', 'Lower', transt, 'Non-unit', n, k,
      $                     one, t, ldt, work, ldwork )
 *
 *              C := C - V * W**T
 *
                IF( m.GT.k ) THEN
 *
 *                 C1 := C1 - V1 * W**T
 *
                   CALL dgemm( 'No transpose', 'Transpose', m-k, n, k,
      $                        -one, v, ldv, work, ldwork, one, c, ldc )
                END IF
 *
 *              W := W * V2**T
 *
                CALL dtrmm( 'Right', 'Upper', 'Transpose', 'Unit', n, k,
      $                     one, v( m-k+1, 1 ), ldv, work, ldwork )
 *
 *              C2 := C2 - W**T
 *
                DO 90 j = 1, k
                   DO 80 i = 1, n
                      c( m-k+j, i ) = c( m-k+j, i ) - work( i, j )
    80             CONTINUE
    90          CONTINUE
 *)

// used in symmetric eigenvalue problem:
// dlarfb 'left', 'No transpose', 'backward', 'columnwise'
procedure ApplyBlockReflectorLBC(V : PDouble; LineWidthV : TASMNativeInt;
  C : PDouble; LineWidthC : TASMNativeInt; work : PDouble; LineWidthWork : TASMNativeInt;
  width, height : TASMNativeInt; k : TASMNativeInt; Transposed : boolean; const reflData : TBlockReflrec);
var pC1, pC2 : PDouble;
    pV1, pV2 : PDouble;
    hk : TASMNativeInt;
begin
     if (width <= 0) or (height <= 0) then
        exit;

     // C = wxh = width x height
     // V = wxh = k x Height
     // T = wxh = k x k
     // Work = wxh = k x Width
     // V = (V1)                 C = (C1)
     //     (V2) last k rows         (C2) = last k rows
     //      V2 is an upper triangular

     pV1 := V;
     pV2 := GenPtr(V, 0, height - k, LineWidthV);
     pC1 := C;
     pC2 := GenPtr(C, 0, height - k, LineWidthC);
     hk := height - k;

     // W = C2**T
     MatrixTranspose( work, LineWidthWork, pC2, LineWidthC, width, k);

     //CALL dtrmm( 'Right', 'Upper', 'No transpose', 'Unit', n,
     // $                     k, one, v( m-k+1, 1 ), ldv, work, ldwork )

     // W = W*V2
     MtxMultRightUpperTriaUnit(work, LineWidthWork, pV2, LineWidthV, k, width, k, k);

     // W = W + C1**T*V1
     //              CALL dgemm( 'Transpose', 'No transpose', n, k, m-k,
     //                        one, c, ldc, v, ldv, one, work, ldwork )
     if height > k then
        reflData.MatrixMultT1(work, LineWidthWork, pC1, pV1, width, hk, k, hk, LineWidthC, LineWidthV,
                       reflData.blkMultSize, doAdd, reflData.blkMultMem);

     // CALL dtrmm( 'Right', 'Lower', transt, 'Non-unit', n, k,
     // $                     one, t, ldt, work, ldwork )
     // W = W*T**T or W*T
     if Transposed
     then
         MtxMultRightLowerTriaNoUnitT2( work, LineWidthWork, reflData.T, reflData.LineWidthT, k, width, k, k )
     else
         MtxMultRightLowerTriaNoUnit(work, LineWidthWork, reflData.T, reflData.LineWidthT, k, width, k, k );


     // C1 = C1 - V1*W**T

     //   CALL dgemm( 'No transpose', 'Transpose', m-k, n, k,
     // $                        -one, v, ldv, work, ldwork, one, c, ldc )
     if height > k then
        reflData.MatrixMultT2( pC1, LineWidthC, pV1, work, k, hk, k, width,
                               LineWidthV, LineWidthWork, reflData.blkMultSize,
                               doSub, reflData.blkMultMem );

     // W = W*V2**T
     // CALL dtrmm( 'Right', 'Upper', 'Transpose', 'Unit', n, k,
     // $                     one, v( m-k+1, 1 ), ldv, work, ldwork )
     MtxMultRightUpperTriaUnitT2(work, LineWidthWork, pV2, LineWidthV, k, width, k, k);

     // C2 := C2 - W**T
     MatrixSubT(pC2, LineWidthC, work, LineWidthWork, width, k);
end;


end.
