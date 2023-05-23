// ###################################################################
// #### This file is part of the mathematics library project, and is
// #### offered under the licence agreement described on
// #### http://www.mrsoft.org/
// ####
// #### Copyright:(c) 2018, Michael R. . All rights reserved.
// ####
// #### Unless required by applicable law or agreed to in writing, software
// #### distributed under the License is distributed on an "AS IS" BASIS,
// #### WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// #### See the License for the specific language governing permissions and
// #### limitations under the License.
// ###################################################################

unit FMAMatrixOperations;

// #################################################
// #### distributes the function to the assembler versions
// #################################################

interface

{$I 'mrMath_CPU.inc'}

uses MatrixConst;

// note: The ASM routines always carry out 2x2 matrix multiplications thus there must be an additional zero line/column in the
// input matrices if the width/height is uneven. The routine also performs better if the matrices are aligned to 16 byte boundaries!
procedure FMAMatrixMult(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt); overload;
procedure FMAMatrixMult(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt; mem : PDouble); overload;
procedure FMAMatrixMultTransposed(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt); overload;
procedure FMAMatrixMultDirect(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt); overload;

procedure FMAMtxVecMult(dest : PDouble; destLineWidth : NativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : NativeInt; width, height : NativeInt; alpha, beta : double);
procedure FMAMtxVecMultT(dest : PDouble; destLineWidth : NativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : NativeInt; width, height : NativeInt; alpha, beta : double);

procedure FMARank1Update(A : PDouble; const LineWidthA : NativeInt; width, height : NativeInt;
  const alpha : double; X, Y : PDouble; incX, incY : NativeInt);

// strassen algorithm for matrix multiplication
procedure FMAStrassenMatrixMultiplication(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt);

implementation

{$IFDEF FPC} {$S-} {$ENDIF}

uses Math, MatrixASMStubSwitch, AVXMatrixOperations, ASMMatrixOperations,
     {$IFDEF x64}
     FMAMatrixMultTransposedOperationsx64, FMAMatrixVectorMultOperationsx64, FMAMatrixMultOperationsx64,
     {$ELSE}
     FMAMatrixMultTransposedOperations, FMAMatrixVectorMultOperations, FMAMatrixMultOperations,
     {$ENDIF}
     SimpleMatrixOperations;

procedure FMAMatrixMultTransposed(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt); overload;
begin
     if (width1 = 0) or (width2 = 0) or (height1 = 0) or (height2 = 0) then
        exit;
     assert((width1 = width2), 'Dimension error');
     assert((destLineWidth - height2*sizeof(double) >= 0) and (LineWidth1 >= width1*sizeof(double)) and (LineWidth2 >= width2*sizeof(double)), 'Line widths do not match');

     if (width1 < 3) and (width2 < 3)
     then
         GenericMtxMultTransp(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2)
     else if (width1 < 2) or (width2 < 2) then
     begin
          // matrix/vector multiplication
          GenericMtxMultTransp(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2);
     end
     else
     begin
          // ########################################################################
          // ####  In this case mt2 is already transposed -> direct multiplication
          // check for alignment:
          if ((NativeUint(dest) and $0000001F) = 0) and ((NativeUint(mt1) and $0000001F) = 0) and ((NativeUint(mt2) and $0000001F) = 0) and
             ((destLineWidth and $0000001F) = 0) and ((LineWidth1 and $0000001F) = 0) and ((LineWidth2 and $0000001F) = 0) then
          begin
               if width1 and $0000000F = 0 then
               begin
                    if height2 and $01 = 1
                    then
                        FMAMatrixMultAlignedEvenW1OddH2TransposedMod16(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2)
                    else
                        FMAMatrixMultAlignedEvenW1EvenH2TransposedMod16(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2);
               end
               else
                   FMAMatrixMultAlignedTransposed(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2);
          end
          else
              FMAMatrixMultUnAlignedTransposed(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2);
     end;
end;

procedure FMAMatrixMult(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt; mem : PDouble); overload;
var mtx : PDouble;
    mtxLineWidth : NativeInt;
    help : NativeInt;
    aMem : Pointer;
begin
     if (width1 = 0) or (width2 = 0) or (height1 = 0) or (height2 = 0) then
     	  exit;
     assert((width1 = height2), 'Dimension error');
     assert((destLineWidth - Width2*sizeof(double) >= 0) and (LineWidth1 >= width1*sizeof(double)) and (LineWidth2 >= width2*sizeof(double)), 'Line widths do not match');

     if (width1 < 2) and (width2 < 2) then
     begin
          GenericMtxMult(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2);
     end
     else if (width1 < 2) or (width2 < 2) then
     begin
          // matrix/vector multiplication
          if width2 = 1
          then
              FMAMtxVecMult(dest, destLineWidth, mt1, mt2, LineWidth1, LineWidth2,  width1, height1, 1, 0)
          else
              GenericMtxMult(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2);
     end
     else
     begin
          // ########################################################################
          // ####  For all "bigger" matrices transpose first then multiply. It's always faster
          aMem := nil;
          mtxLineWidth := height2;
          if height2 and $03 <> 0 then
             mtxLineWidth := mtxLineWidth + 4 - (height2 and $03);
          mtxLineWidth := mtxLineWidth*sizeof(double);
          if Assigned(mem)
          then
              mtx := mem
          else
              mtx := MtxMallocAlign( width2*mtxLineWidth, aMem );
          assert(assigned(mtx), 'Error could not reserve transpose memory');
          AVXMatrixTranspose(mtx, mtxLineWidth, mt2, LineWidth2, width2, height2);
          help := width2;
          width2 := height2;
          height2 := help;

          FMAMatrixMultTransposed(dest, destLineWidth, mt1, mtx, width1, height1, width2, height2, LineWidth1, mtxLineWidth);

          if Assigned(aMem) then
             FreeMem(aMem);
     end;
end;

procedure FMAMatrixMult(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt); overload;
begin
     FMAMatrixMult(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2, nil);
end;

procedure InternalFMAStrassenMult(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt;
  width2 : NativeInt; height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt; mem : PDouble);
var a11, a12, a21, a22 : PDouble;
    b11, b12, b21, b22 : PDouble;
    s1, s2, s3, s4 : PDouble;
    t1, t2, t3, t4 : PDouble;
    P1, P2, P3, P4, P5, P6, P7 : PDouble;
    U1, U2, U3, U4, U5, U6, U7 : PDouble;
    c11, c12, c21, c22 : PDouble;
    k, m, n : NativeInt;
    lineK : NativeInt;
    lineN : NativeInt;
    x, y : PDouble;
    multLineW : NativeInt;
begin
     if (width1 <= cStrassenMinSize) or (height1 <= cStrassenMinSize) or (width2 <= cStrassenMinSize) then
     begin
          multLineW := Max(cStrassenMinSize, height2)*sizeof(double);
          AVXMatrixTranspose(mem, multLineW, mt2, LineWidth2, width2, height2);
          FMAMatrixMultTransposed(dest, destLineWidth, mt1, mem, width1, height1, height2, width2, LineWidth1, multLineW);
     end
     else
     begin
          k := width1 div 2;
          m := height1 div 2;
          n := width2 div 2;

          lineK := k*sizeof(double);
          lineN := n*sizeof(double);

          a11 := mt1;
          a12 := a11;
          inc(a12, k);
          a21 := mt1;
          inc(PByte(a21), m*LineWidth1);
          a22 := a21;
          inc(a22, k);

          b11 := mt2;
          b12 := b11;
          inc(b12, n);
          b21 := mt2;
          inc(PByte(b21), k*LineWidth2);
          b22 := b21;
          inc(b22, n);

          c11 := dest;
          c12 := c11;
          inc(c12, n);
          c21 := dest;
          inc(PByte(c21), m*destLineWidth);
          c22 := c21;
          inc(c22, n);

          x := mem;
          y := x;
          inc(Y, m*Max(k, n));

          S3 := X;
          T3 := Y;
          P7 := C21;
          S1 := X;
          T1 := Y;
          P5 := C22;
          S2 := X;
          T2 := Y;
          P6 := C12;
          S4 := X;
          P3 := C11;
          P1 := X;
          U2 := C12;
          U3 := C21;
          U4 := C12;
          U7 := C22;
          U5 := C12;
          T4 := Y;
          P4 := C11;
          U6 := C21;
          P2 := C11;
          U1 := C11;

          mem := Y;
          inc(mem, k*n);

          // memory efficient mult:

          // s3 = A11 - A21
          AVXMatrixSub(s3, lineK, a11, a21, k, m, LineWidth1, LineWidth1);
          // t3 = B22 - B12
          AVXMatrixSub(t3, lineN, B22, B12, n, k, LineWidth2, LineWidth2);
          // p7 = s3*t3
          InternalFMAStrassenMult(p7, destLineWidth, s3, t3, k, m, n, k, lineK, lineN, mem);
          // s1 = a21 + a22
          AVXMatrixAdd(s1, lineK, a21, a22, k, m, LineWidth1, LineWidth1);
          // t1 = b12 - b11
          AVXMatrixSub(t1, lineN, B12, B11, n, k, LineWidth2, LineWidth2);
          // p5 = s1*t1
          InternalFMAStrassenMult(p5, destLineWidth, s1, t1, k, m, n, k, lineK, lineN, mem);
          // s2 = S1 - A11
          AVXMatrixSub(s2, lineK, S1, A11, k, m, lineK, LineWidth1);
          // t2 = b22 - t1
          AVXMatrixSub(t2, lineN, B22, t1, n, k, LineWidth2, lineN);
          // p6 = s2*t2
          InternalFMAStrassenMult(p6, destLineWidth, s2, t2, k, m, n, k, lineK, lineN, mem);
          // s4 = A12 - S2
          AVXMatrixSub(s4, lineK, A12, S2, k, m, LineWidth1, lineK);
          // p3 = s4*b22
          InternalFMAStrassenMult(p3, destLineWidth, s4, b22, k, m, n, k, lineK, LineWidth2, mem);
          // p1 = A11*B11
          InternalFMAStrassenMult(p1, lineN, A11, B11, k, m, n, k, LineWidth1, LineWidth2, mem);
          // U2 = P1 + P6
          AVXMatrixAdd(U2, destLineWidth, P1, P6, n, m, LineN, destLineWidth);
          // U3 = U2 + P7
          AVXMatrixAdd(U3, destLineWidth, U2, P7, n, m, destLineWidth, destLineWidth);
          // U4 = U2 + P5
          AVXMatrixAdd(U4, destLineWidth, U2, P5, n, m, destLineWidth, destLineWidth);
          // U7 = U3 + P5
          AVXMatrixAdd(U7, destLineWidth, U3, P5, n, m, destLineWidth, destLineWidth);
          // U5 = U4 + P3
          AVXMatrixAdd(U5, destLineWidth, U4, P3, n, m, destLineWidth, destLineWidth);
          // t4 = T2 - B21
          AVXMatrixSub(t4, lineN, T2, B21, n, k, LineN, LineWidth2);
          // p4 = A22*t4
          InternalFMAStrassenMult(p4, destLineWidth, A22, t4, k, m, n, k, LineWidth1, lineN, mem);
          // U6 = U3 - P4
          AVXMatrixSub(U6, destLineWidth, U3, P4, n, m, destLineWidth, destLineWidth);
          // p2 = A12*B21
          InternalFMAStrassenMult(p2, destLineWidth, A12, B21, k, m, n, k, LineWidth1, LineWidth2, mem);
          // U1 = P1 + P2
          AVXMatrixAdd(U1, destLineWidth, P1, P2, n, m, lineN, destLineWidth);

          // tidy up work for uneven columns, rows....
          if ((width1 and $01) > 0) or ((height1 and $01) > 0) or ((width2 and $01) > 0) then
          begin
               // following the algorithm if all items are odd...:
               //
               //  A*B = [A1   ac    ][B1  bc  ] = [A1*B1   0]  +  Delta
               //        [ar'  alpha ][br' beta]   [  0     0]
               //
               // Delta = [ac   ]*[br' beta]   +   [  0     A1*bc]
               //         [alpha]                  [ar'*B1  ar'*bc]

               // we already have computed A1*B1...

               if ((width1 and $01) = 0) and ((width2 and $01) = 0) then
               begin
                    inc(PByte(dest), destLineWidth*(height1 - 1));
                    inc(PByte(A11), LineWidth1*(height1 - 1));
                    GenericMtxMult(dest, destLineWidth, A11, B11, width1, 1, width2, height2, LineWidth1, LineWidth2);
               end
               else if ((width1 and $01) = 0) and ((height1 and $01) = 0) then
               begin
                    inc(dest, (width2 - 1));
                    inc(B11, (width2 - 1));
                    GenericMtxMult(dest, destLineWidth, A11, B11, width1, height1, 1, height2, LineWidth1, LineWidth2);
               end
               else if ((height1 and $01) = 0) and ((width2 and $01) = 0) then
               begin
                    inc(A11, width1 - 1);
                    inc(PByte(B11), LineWidth2*(height2 - 1));
                    GenericMtxDeltaUpdate(dest, destLineWidth, A11, B11, width2, height1, LineWidth1);
               end
               else if ((width1 and $01) = 0) and ((height1 and $01) > 0) and ((width2 and $01) > 0) then
               begin
                    // last column [A]*bc
                    inc(dest, (width2 - 1));
                    inc(B11, (width2 - 1));
                    GenericMtxMult(dest, destLineWidth, A11, B11, width1, height1 - 1, 1, height2, LineWidth1, LineWidth2);
                    dec(B11, (width2 - 1));
                    dec(dest, (width2 - 1));

                    // [ar alpha]*[B bc]  (last line)
                    inc(PByte(dest), destLineWidth*(height1 - 1));
                    inc(PByte(A11), LineWidth1*(height1 - 1));
                    GenericMtxMult(dest, destLineWidth, A11, B11, width1, 1, width2, height2, LineWidth1, LineWidth2);
               end
               else
               begin
                    // all dimensions are odd!
                    // calc A1*bc
                    inc(dest, (width2 - 1));
                    inc(B11, (width2 - 1));
                    GenericMtxMult(dest, destLineWidth, A11, B11, width1 - 1, height1 - 1, 1, height2 - 1, LineWidth1, LineWidth2);
                    dec(B11, (width2 - 1));
                    dec(dest, (width2 - 1));

                    // calc ar'*B1 and ar'*bc
                    inc(PByte(dest), destLineWidth*(height1 - 1));
                    inc(PByte(A11), LineWidth1*(height1 - 1));
                    GenericMtxMult(dest, destLineWidth, A11, B11, width1 - 1, 1, width2, height2 - 1, LineWidth1, LineWidth2);

                    inc(dest, width2 - 1);
                    inc(B11, width2 - 1);
                    GenericMtxMult(dest, destLineWidth, A11, B11, width1 - 1, 1, 1, height2 - 1, LineWidth1, LineWidth2);

                    dec(dest, width2 - 1);
                    dec(PByte(dest), destLineWidth*(height1 - 1));
                    dec(PByte(A11), LineWidth1*(height1 - 1));
                    dec(B11, width2 - 1);

                    // last step is to add the vector product matrix to the existing sum...
                    inc(A11, width1 - 1);
                    inc(PByte(B11), LineWidth2*(height2 - 1));
                    GenericMtxDeltaUpdate(dest, destLineWidth, A11, B11, width2, height1, LineWidth1);
               end;
          end;
     end;
end;

procedure FMAStrassenMatrixMultiplication(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt);
var mem : PDouble;
    ptrMem : Pointer;
    memSize : NativeInt;
    m, k, n : NativeInt;
    lev : NativeInt;
begin
     // check the cutoff criterion:
     if (width1 <= cStrassenMinSize) or (height1 <= cStrassenMinSize) or (height2 <= cStrassenMinSize)
     then
         FMAMatrixMult(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2)
     else
     begin
          // calc the complete used additionaly memory
          memSize := 0;
          m := height1;
          k := width1;
          n := width2;
          lev := Min(m, Min(k, n));

          while lev > cStrassenMinSize do
          begin
               memSize := memSize + sizeof(double)*(m*max(k, n) + k*n);
               k := k shr 1;
               m := m shr 1;
               n := n shr 1;
               lev := lev shr 1;
          end;
          // additional memory used for the transposition
          memSize := memSize + Max(cStrassenMinSize*cStrassenMinSize, n*k)*sizeof(double);

          mem := MtxMallocAlign(memSize, ptrMem);
          try
             InternalFMAStrassenMult(Dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2, mem);
          finally
                 FreeMem(ptrMem);
          end;
     end;
end;

procedure FMAMatrixMultDirect(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt);
begin
     if (width1 = 0) or (width2 = 0) or (height1 = 0) or (height2 = 0) then
        exit;
     assert((width1 = height2), 'Dimension error');
     assert((destLineWidth - Width2*sizeof(double) >= 0) and (LineWidth1 >= width1*sizeof(double)) and (LineWidth2 >= width2*sizeof(double)), 'Line widths do not match');

     if (width1 < 3) and (width2 < 3)
     then
         GenericMtxMult(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2)
     else if (width1 < 2) or (width2 < 2) then
     begin
          // matrix/vector multiplication
          if width2 = 1
          then
              FMAMtxVecMult(dest, destLineWidth, mt1, mt2, LineWidth1, LineWidth2, width1, height1, 1, 0)
          else
              GenericMtxMult(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2);
     end
     else
     begin
          // check for alignment:
          if ((NativeUint(dest) and $0000001F) = 0) and ((NativeUint(mt1) and $0000001F) = 0) and ((NativeUint(mt2) and $0000001F) = 0) and
             ((destLineWidth and $0000001F) = 0) and ((LineWidth1 and $0000001F) = 0) and ((LineWidth2 and $0000001F) = 0)
          then
              FMAMatrixMultAligned(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2)
          else
              FMAMatrixMultUnaligned(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2)
     end;
end;

procedure FMAMtxVecMult(dest : PDouble; destLineWidth : NativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : NativeInt; width, height : NativeInt; alpha, beta : double);
begin
     if (width = 0) or (height = 0) then
        exit;

     if (LineWidthV = sizeof(double)) then
     begin
          if ((NativeUint(mt1) and $0000001F) = 0) and ((NativeUint(v) and $0000001F) = 0) and (LineWidthMT and $1F = 0)
          then
              FMAMatrixVectMultAlignedVAligned(dest, destLineWidth, mt1, v, LineWidthMT, LineWidthV, width, height, alpha, beta)
          else
              FMAMatrixVectMultUnAlignedVAligned(dest, destLineWidth, mt1, v, LineWidthMT, LineWidthV, width, height, alpha, beta);
     end
     else
         AVXMtxVecMult(dest, destLineWidth, mt1, v, LineWidthMT, LineWidthV, width, height, alpha, beta);
end;

// performs dest = beta*dest + mt1**T*v * alpha
procedure FMAMtxVecMultT(dest : PDouble; destLineWidth : NativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : NativeInt; width, height : NativeInt; alpha, beta : double);
begin
     if (width = 0) or (height = 0) then
        exit;

     // no speed gain agains the standard vect mul
     //if LineWidthV = sizeof(double)
//     then
//         ASMMatrixVectMultTDestVec(dest, destLineWidth, mt1, v, LineWidthMT, LineWidthV, width, height, alpha, beta)
//     else
         FMAMatrixVectMultT(dest, destLineWidth, mt1, v, LineWidthMT, LineWidthV, width, height, alpha, beta);
end;

procedure FMARank1Update(A : PDouble; const LineWidthA : NativeInt; width, height : NativeInt;
  const alpha : double; X, Y : PDouble; incX, incY : NativeInt);
begin
     if (width <= 0) or (height <= 0) then
        exit;

     // we only have a SSE optimized routine for the non sequential rank1 update
     if incY <> sizeof(double) 
     then
         ASMRank1Update(A, LineWidthA, width, height, x, y, incx, incy, alpha)
     else if ((NativeUint(A) and $0000001F) = 0) and ((NativeUint(Y) and $0000001F) = 0) and (LineWidthA and $1F = 0)
     then
         FMARank1UpdateSeqAligned(A, LineWidthA, width, height, x, y, incX, incY, alpha)
     else
         FMARank1UpdateSeq(A, LineWidthA, width, height, x, y, incX, incY, alpha);
end;

end.

