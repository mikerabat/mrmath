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


unit ASMMatrixMultOperations;

interface

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}
{$IFNDEF x64}

uses MatrixConst;

// full matrix operations -> there are no zero columns nor zero lines attached
procedure ASMMatrixMultAlignedEvenW1EvenW2(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1, height1, width2, height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
procedure ASMMatrixMultUnAlignedEvenW1EvenW2(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1, height1, width2, height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);

// the width of the second matrix is uneven -> the last column of mt2 must be handled differently
procedure ASMMatrixMultAlignedEvenW1OddW2(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1, height1, width2, height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
procedure ASMMatrixMultUnAlignedEvenW1OddW2(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1, height1, width2, height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);

// the width of mt1 is odd but the one of mt2 is even
procedure ASMMatrixMultAlignedOddW1EvenW2(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1, height1, width2, height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
procedure ASMMatrixMultUnAlignedOddW1EvenW2(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1, height1, width2, height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);

// both matrices have an odd width
procedure ASMMatrixMultAlignedOddW1OddW2(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1, height1, width2, height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
procedure ASMMatrixMultUnAlignedOddW1OddW2(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1, height1, width2, height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);

// some special types of multiplications used e.g. in QR Decomposition
// note the result is stored in mt2 again!
// dest = mt1'*mt2; where mt2 is a lower triangular matrix and the operation is transposition
// the function assumes a unit diagonal (does not touch the real middle elements)
// width and height values are assumed to be the "original" (non transposed) ones
procedure ASMMtxMultTria2T1(dest : PDouble; LineWidthDest : TASMNativeInt; mt1 : PDouble; LineWidth1 : TASMNativeInt; mt2 : PDouble; LineWidth2 : TASMNativeInt;
  width1, height1, width2, height2 : TASMNativeInt);
// mt1 = mt1*mt2'; where mt2 is an upper triangular matrix
procedure ASMMtxMultTria2T1StoreT1(mt1 : PDouble; LineWidth1 : TASMNativeInt; mt2 : PDouble; LineWidth2 : TASMNativeInt;
  width1, height1, width2, height2 : TASMNativeInt);
// calculates mt1 = mt1*mt2', mt2 = lower triangular matrix. diagonal elements are assumed to be 1!
procedure ASMMtxMultLowTria2T2Store1(mt1 : PDouble; LineWidth1 : TASMNativeInt; mt2 : PDouble; LineWidth2 : TASMNativeInt;
  width1, height1, width2, height2 : TASMNativeInt);

// W = C1*V1*T -> V1 is an upper triangular matrix with assumed unit diagonal entries. Operation on V1 transposition
procedure ASMMtxMultTria2TUpperUnit(dest : PDouble; LineWidthDest : TASMNativeInt; mt1 : PDouble; LineWidth1 : TASMNativeInt; mt2 : PDouble; LineWidth2 : TASMNativeInt;
  width1, height1, width2, height2 : TASMNativeInt);

// mt1 = mt1*mt2; where mt2 is an upper triangular matrix
procedure ASMMtxMultTria2Store1(mt1 : PDouble; LineWidth1 : TASMNativeInt; mt2 : PDouble; LineWidth2 : TASMNativeInt;
  width1, height1, width2, height2 : TASMNativeInt);

// mt1 = mt1*mt2; where mt2 is an upper triangular matrix - diagonal elements are unit
procedure ASMMtxMultTria2Store1Unit(mt1 : PDouble; LineWidth1 : TASMNativeInt; mt2 : PDouble; LineWidth2 : TASMNativeInt;
  width1, height1, width2, height2 : TASMNativeInt);




{$ENDIF}

implementation

{$IFNDEF x64}
{$IFDEF FPC} {$ASMMODE intel} {$ENDIF}
procedure ASMMatrixMultAlignedEvenW1EvenW2(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1, height1, width2, height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var destOffset : TASMNativeInt;
    iters1, iters2 : TASMNativeInt;
    bytesWidth2 : TASMNativeInt;
    Linewidth2_2 : TASMNativeInt;
    y : TASMNativeInt;
begin
     assert(((LineWidth1 and $0000000F) = 0) and ((destLineWidth and $0000000F) = 0) and ((LineWidth2 and $0000000F) = 0), 'Error line widths must be aligned');
     assert(((Cardinal(dest) and $0000000F) = 0) and ((Cardinal(mt1) and $0000000F) = 0) and ((Cardinal(mt2) and $0000000F) = 0), 'Error aligned operations cannot be performed');

     assert((width1 and $00000001 = 0) and (width2 and $00000001 = 0), 'Error widths are not even');

     asm
        push ebx;
        push esi;
        push edi;

        //destOffset := destLineWidth - Width2*sizeof(double);
        mov ecx, Width2;
        shl ecx, 3;
        mov edx, destLineWidth;
        sub edx, ecx;
        mov destOffset, edx;

        //iters1 := width2 div 2;
        mov edx, width2;
        shr edx, 1;
        mov iters1, edx;

        //iters2 := -(width1 div 2)*2*sizeof(double);
        mov edx, width1;
        and edx, $FFFFFFFE;
        shl edx, 3;
        imul edx, -1;
        mov iters2, edx;

        //bytesWidth2 := width2*sizeof(double);
        mov bytesWidth2, ecx;

        //Linewidth2_2 := 2*LineWidth2;
        mov edx, LineWidth2;
        shl edx, 1;
        mov LineWidth2_2, edx;

        // initalize matrices
        mov esi, mt2;  // use the esi register for faster access

        mov ecx, mt1;    // prepare ecx for the "reversed" for loop 
        sub ecx, iters2;

        // for y := 0 to height1 - 1:
        mov eax, Height1;
        mov y, eax;

        mov eax, LineWidth2;
        @@forylabel:
            // for x := 0 to width2 - 1:
            mov ebx, iters1;
            @@forxlabel:

                xorpd xmm0, xmm0;   // dest^ := 0
                xorpd xmm7, xmm7;   // (dest + 1)^ := 0;

                mov edi, esi;       // valcounter2

                // for idx := 0 to width1 div 2 do
                mov edx, iters2;

                @@InnerLoop:
                    movapd xmm1, [ecx + edx];

                    // load 2x2 block
                    movapd xmm3, [edi];
                    movapd xmm5, xmm3;

                    movapd xmm4, [edi + eax];
                    add edi, Linewidth2_2;
                    movapd xmm6, xmm4;

                    // swap such that we can immediately multiply
                    movhlps xmm4, xmm5;
                    movlhps xmm3, xmm6;

                    // it's a bit faster if we reuse a register - seems the cpu can
                    // then parallelize the instructions better
                    movapd xmm6, xmm1;

                    // multiply 2x2 and add
                    mulpd xmm3, xmm1;
                    mulpd xmm4, xmm6;
                    addpd xmm0, xmm3;
                    addpd xmm7, xmm4;

                    // end for idx := 0 to width1 div 2
                //dec edx;
                add edx, 16;
                jnz @@InnerLoop;

                // final horizontal addition and compact
                haddpd xmm0, xmm7;

                // store back result
                mov edi, dest;
                movntdq [edi], xmm0;

                // increment the pointers
                // inc(mt2), inc(dest);
                //add dword ptr [mt2], 8;
                add esi, 16;
                add dword ptr [dest], 16;

            // end for x := 0 to width2 - 1
            dec ebx;
            jnz @@forxlabel;

            // dec(mt2, Width2);
            // inc(PByte(mt1), LineWidth1);
            // inc(PByte(dest), destOffset);
            //mov ebx, bytesWidth2;
            //sub dword ptr [mt2], ebx;
            sub esi, bytesWidth2;
            add ecx, LineWidth1;
            mov ebx, destOffset;
            add dword ptr [dest], ebx;

        // end for y := 0 to height1 - 1
        //dec eax;
        dec y;
        jnz @@forylabel

        pop edi;
        pop esi;
        pop ebx;
     end;
end;

procedure ASMMatrixMultUnAlignedEvenW1EvenW2(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1, height1, width2, height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var destOffset : TASMNativeInt;
    iters1, iters2 : TASMNativeInt;
    bytesWidth2 : TASMNativeInt;
    Linewidth2_2 : TASMNativeInt;
    y : TASMNativeInt;
begin
     assert((width1 and $00000001 = 0) and (width2 and $00000001 = 0), 'Error widths are not even');

     destOffset := destLineWidth - Width2*sizeof(double);
     iters1 := width2 div 2;
     iters2 := -(width1 div 2)*2*sizeof(double);
     bytesWidth2 := width2*sizeof(double);
     Linewidth2_2 := 2*LineWidth2;
     asm
        push ebx;
        push esi;
        push edi;

        mov esi, mt2;  // use the esi register for faster access

        mov ecx, mt1;    // prepare ecx for the "reversed" for loop 
        sub ecx, iters2;

        // for y := 0 to height1 - 1:
        mov eax, Height1;
        mov y, eax;

        mov eax, LineWidth2;
        @@forylabel:
            // for x := 0 to width2 - 1:
            mov ebx, iters1;
            @@forxlabel:

                xorpd xmm0, xmm0;   // dest^ := 0
                xorpd xmm7, xmm7;   // (dest + 1)^ := 0;

                mov edi, esi;       // valcounter2

                // for idx := 0 to width1 div 2 do
                mov edx, iters2;

                @@InnerLoop:
                    movupd xmm1, [ecx + edx];

                    // load 2x2 block
                    movupd xmm3, [edi];
                    movapd xmm5, xmm3;

                    movupd xmm4, [edi + eax];
                    add edi, Linewidth2_2;
                    movapd xmm6, xmm4;

                    // swap such that we can immediately multiply
                    movhlps xmm4, xmm5;
                    movlhps xmm3, xmm6;

                    // it's a bit faster if we reuse a register - seems the cpu can
                    // then parallelize the instructions better
                    movapd xmm6, xmm1;

                    // multiply 2x2 and add
                    mulpd xmm3, xmm1;
                    mulpd xmm4, xmm6;
                    addpd xmm0, xmm3;
                    addpd xmm7, xmm4;

                    // end for idx := 0 to width1 div 2
                //dec edx;
                add edx, 16;
                jnz @@InnerLoop;

                // final horizontal addition and compact
                haddpd xmm0, xmm7;

                // store back result
                mov edi, dest;
                movupd [edi], xmm0;

                // increment the pointers
                // inc(mt2), inc(dest);
                //add dword ptr [mt2], 8;
                add esi, 16;
                add dword ptr [dest], 16;

            // end for x := 0 to width2 - 1
            dec ebx;
            jnz @@forxlabel;

            // dec(mt2, Width2);
            // inc(PByte(mt1), LineWidth1);
            // inc(PByte(dest), destOffset);
            //mov ebx, bytesWidth2;
            //sub dword ptr [mt2], ebx;
            sub esi, bytesWidth2;
            add ecx, LineWidth1;
            mov ebx, destOffset;
            add dword ptr [dest], ebx;

        // end for y := 0 to height1 - 1
        //dec eax;
        dec y;
        jnz @@forylabel

        pop edi;
        pop esi;
        pop ebx;
     end;
end;

procedure ASMMatrixMultUnAlignedOddW1EvenW2(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1, height1, width2, height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var destOffset : TASMNativeInt;
    iters1, iters2 : TASMNativeInt;
    Linewidth2_2 : TASMNativeInt;
    y : TASMNativeInt;
begin
     assert((width2 and $00000001) = 0, 'Error there is no odd width2');
     assert((width1 and $00000001) = 1, 'Error width1 is not even');

     // todo: is it better to not use pointer arithmetic?
     destOffset := destLineWidth - Width2*sizeof(double);
     iters1 := width2 div 2;
     iters2 := -(width1 - 1)*sizeof(double);
     Linewidth2_2 := 2*LineWidth2;
     asm
        push ebx;
        push esi;
        push edi;

        mov esi, mt2;  // use the esi register for faster access

        mov ecx, mt1;    // prepare ecx for the "reversed" for loop 
        sub ecx, iters2;

        xorpd xmm2, xmm2;
        
        // for y := 0 to height1 - 1:
        mov eax, Height1;
        mov y, eax;

        mov eax, LineWidth2;
        @@forylabel:
            // for x := 0 to width2 - 1:
            mov ebx, iters1;
            @@forxlabel:

                xorpd xmm0, xmm0;   // dest^ := 0
                xorpd xmm7, xmm7;   // (dest + 1)^ := 0;

                mov edi, esi;       // valcounter2

                // for idx := 0 to width1 div 2 do
                mov edx, iters2;

                @@InnerLoop:
                    movupd xmm1, [ecx + edx];

                    // load 2x2 block
                    movupd xmm3, [edi];
                    movapd xmm5, xmm3;

                    movupd xmm4, [edi + eax];
                    add edi, Linewidth2_2;
                    movapd xmm6, xmm4;

                    // swap such that we can immediately multiply
                    movhlps xmm4, xmm5;
                    movlhps xmm3, xmm6;

                    // it's a bit faster if we reuse a register - seems the cpu can
                    // then parallelize the instructions better
                    movapd xmm6, xmm1;

                    // multiply 2x2 and add
                    mulpd xmm3, xmm1;
                    mulpd xmm4, xmm6;
                    addpd xmm0, xmm3;
                    addpd xmm7, xmm4;

                    // end for idx := 0 to width1 div 2
                //dec edx;
                add edx, 16;
                jnz @@InnerLoop;

                // special treatment of the last column of mt1:
                movddup xmm1, [ecx];

                movupd xmm3, [edi];
                mulpd xmm1, xmm3;

                // final horizontal addition
                xorpd xmm3, xmm3;
                movq xmm2, xmm1;
                addsd xmm0, xmm2;
                haddpd xmm0, xmm3;

                movhlps xmm2, xmm1;
                addsd xmm7, xmm2;
                haddpd xmm7, xmm3;

                // compact result
                movlhps xmm0, xmm7;

                // store back result
                mov edi, dest;
                movupd [edi], xmm0;

                // increment the pointers
                // inc(mt2), inc(dest);
                //add dword ptr [mt2], 8;
                add esi, 16;
                add dword ptr [dest], 16;

            // end for x := 0 to width2 - 1
            dec ebx;
            jnz @@forxlabel;

            // dec(mt2, Width2);
            // inc(PByte(mt1), LineWidth1);
            // inc(PByte(dest), destOffset);
            //mov ebx, bytesWidth2;
            //sub dword ptr [mt2], ebx;
            mov esi, mt2; //sub esi, bytesWidth2;
            add ecx, LineWidth1;
            mov ebx, destOffset;
            add dword ptr [dest], ebx;

        // end for y := 0 to height1 - 1
        //dec eax;
        dec y;
        jnz @@forylabel

        pop edi;
        pop esi;
        pop ebx;
     end;
end;

procedure ASMMatrixMultAlignedOddW1EvenW2(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1, height1, width2, height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var destOffset : TASMNativeInt;
    iters1, iters2 : TASMNativeInt;
    Linewidth2_2 : TASMNativeInt;
    y : TASMNativeInt;
begin
     assert(((Cardinal(dest) and $0000000F) = 0) and ((Cardinal(mt1) and $0000000F) = 0) and ((Cardinal(mt2) and $0000000F) = 0), 'Error aligned operations cannot be performed');
     assert(((LineWidth1 and $0000000F) = 0) and ((destLineWidth and $0000000F) = 0) and ((LineWidth2 and $0000000F) = 0), 'Error line widths must be aligned');

     assert((width2 and $00000001) = 0, 'Error there is no odd width2');
     assert((width1 and $00000001) = 1, 'Error width1 is not even');

     destOffset := destLineWidth - Width2*sizeof(double);
     iters1 := width2 div 2;
     iters2 := -(width1 - 1)*sizeof(double);
     Linewidth2_2 := 2*LineWidth2;
     asm
        push ebx;
        push esi;
        push edi;

        mov esi, mt2;  // use the esi register for faster access

        mov ecx, mt1;    // prepare ecx for the "reversed" for loop 
        sub ecx, iters2;

        // for y := 0 to height1 - 1:
        mov eax, Height1;
        mov y, eax;

        mov eax, LineWidth2;
        @@forylabel:
            // for x := 0 to width2 - 1:
            mov ebx, iters1;
            @@forxlabel:

                xorpd xmm0, xmm0;   // dest^ := 0
                xorpd xmm7, xmm7;   // (dest + 1)^ := 0;

                mov edi, esi;       // valcounter2

                // for idx := 0 to width1 div 2 do
                mov edx, iters2;

                @@InnerLoop:
                    movapd xmm1, [ecx + edx];

                    // load 2x2 block
                    movapd xmm3, [edi];
                    movapd xmm5, xmm3;

                    movapd xmm4, [edi + eax];
                    add edi, Linewidth2_2;
                    movapd xmm6, xmm4;

                    // swap such that we can immediately multiply
                    movhlps xmm4, xmm5;
                    movlhps xmm3, xmm6;

                    // it's a bit faster if we reuse a register - seems the cpu can
                    // then parallelize the instructions better
                    movapd xmm6, xmm1;

                    // multiply 2x2 and add
                    mulpd xmm3, xmm1;
                    mulpd xmm4, xmm6;
                    addpd xmm0, xmm3;
                    addpd xmm7, xmm4;

                    // end for idx := 0 to width1 div 2
                //dec edx;
                add edx, 16;
                jnz @@InnerLoop;

                // special treatment of the last column of mt1:
                movddup xmm1, [ecx];

                movapd xmm3, [edi];
                mulpd xmm1, xmm3;

                // final horizontal addition
                xorpd xmm3, xmm3;
                movq xmm2, xmm1;
                addsd xmm0, xmm2;
                haddpd xmm0, xmm3;

                xorpd xmm2, xmm2;
                movhlps xmm2, xmm1;
                addsd xmm7, xmm2;
                haddpd xmm7, xmm3;

                // compact result
                movlhps xmm0, xmm7;

                // store back result
                mov edi, dest;
                movapd [edi], xmm0;

                // increment the pointers
                // inc(mt2), inc(dest);
                //add dword ptr [mt2], 8;
                add esi, 16;
                add dword ptr [dest], 16;

            // end for x := 0 to width2 - 1
            dec ebx;
            jnz @@forxlabel;

            // dec(mt2, Width2);
            // inc(PByte(mt1), LineWidth1);
            // inc(PByte(dest), destOffset);
            //mov ebx, bytesWidth2;
            //sub dword ptr [mt2], ebx;
            mov esi, mt2;
            add ecx, LineWidth1;
            mov ebx, destOffset;
            add dword ptr [dest], ebx;

        // end for y := 0 to height1 - 1
        //dec eax;
        dec y;
        jnz @@forylabel

        pop edi;
        pop esi;
        pop ebx;
     end;
end;

procedure ASMMatrixMultAlignedEvenW1OddW2(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1, height1, width2, height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var destOffset : TASMNativeInt;
    iters1, iters2 : TASMNativeInt;
    bytesWidth2 : TASMNativeInt;
    Linewidth2_2 : TASMNativeInt;
    y : TASMNativeInt;
begin
     assert(((Cardinal(dest) and $0000000F) = 0) and ((Cardinal(mt1) and $0000000F) = 0) and ((Cardinal(mt2) and $0000000F) = 0), 'Error aligned operations cannot be performed');
     assert(((LineWidth1 and $0000000F) = 0) and ((destLineWidth and $0000000F) = 0) and ((LineWidth2 and $0000000F) = 0), 'Error line widths must be aligned');

     assert((width2 and $00000001) = 1, 'Error there is no odd width2');
     assert((width1 and $00000001) = 0, 'Error width1 is not even');

     destOffset := destLineWidth - (Width2 - 1)*sizeof(double);
     iters1 := width2 div 2;
     iters2 := -width1*sizeof(double);
     bytesWidth2 := (width2 - 1)*sizeof(double);
     Linewidth2_2 := 2*LineWidth2;
     asm
        push ebx;
        push esi;
        push edi;

        mov esi, mt2;  // use the esi register for faster access

        mov ecx, mt1;    // prepare ecx for the "reversed" for loop
        sub ecx, iters2;

        // for y := 0 to height1 - 1:
        mov eax, Height1;
        mov y, eax;

        mov eax, LineWidth2;
        @@forylabel:
            // for x := 0 to width2 - 1:
            mov ebx, iters1;
            @@forxlabel:

                xorpd xmm0, xmm0;   // dest^ := 0
                xorpd xmm7, xmm7;   // (dest + 1)^ := 0;

                mov edi, esi;       // valcounter2

                // for idx := 0 to width1 div 2 do
                mov edx, iters2;

                @@InnerLoop:
                    movapd xmm1, [ecx + edx];

                    // load 2x2 block
                    movapd xmm3, [edi];
                    movapd xmm5, xmm3;

                    movapd xmm4, [edi + eax];
                    add edi, Linewidth2_2;
                    movapd xmm6, xmm4;

                    // swap such that we can immediately multiply
                    movhlps xmm4, xmm5;
                    movlhps xmm3, xmm6;

                    // it's a bit faster if we reuse a register - seems the cpu can
                    // then parallelize the instructions better
                    movapd xmm6, xmm1;

                    // multiply 2x2 and add
                    mulpd xmm3, xmm1;
                    mulpd xmm4, xmm6;
                    addpd xmm0, xmm3;
                    addpd xmm7, xmm4;

                    // end for idx := 0 to width1 div 2
                //dec edx;
                add edx, 16;
                jnz @@InnerLoop;

                // final horizontal addition and compact result
                haddpd xmm0, xmm7;

                // store back result
                mov edi, dest;
                movntdq [edi], xmm0;

                // increment the pointers
                // inc(mt2), inc(dest);
                //add dword ptr [mt2], 8;
                add esi, 16;
                add dword ptr [dest], 16;

            // end for x := 0 to width2 - 1
            dec ebx;
            jnz @@forxlabel;

            // handle the last column of mt2
            xorpd xmm0, xmm0;   // dest^ := 0
            mov edi, esi;       // valcounter2

            mov ebx, iters2;
            @@InnerLoop2:
                // load element from line
                movsd xmm1, [ecx + ebx];

                // load, multiply and add
                mulsd xmm1, [edi];
                add edi, eax;       // next row

                addsd xmm0, xmm1;
            add ebx, 8;
            jnz @@InnerLoop2;

            mov edi, dest;
            movsd [edi], xmm0;

            // dec(mt2, Width2);
            // inc(PByte(mt1), LineWidth1);
            // inc(PByte(dest), destOffset);
            //mov ebx, bytesWidth2;
            //sub dword ptr [mt2], ebx;
            sub esi, bytesWidth2;
            add ecx, LineWidth1;
            mov ebx, destOffset;
            add dword ptr [dest], ebx;

        // end for y := 0 to height1 - 1
        //dec eax;
        dec y;
        jnz @@forylabel

        pop edi;
        pop esi;
        pop ebx;
     end;
end;

procedure ASMMatrixMultUnAlignedEvenW1OddW2(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1, height1, width2, height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var destOffset : TASMNativeInt;
    iters1, iters2 : TASMNativeInt;
    bytesWidth2 : TASMNativeInt;
    Linewidth2_2 : TASMNativeInt;
    y : TASMNativeInt;
begin
     assert((width2 and $00000001) = 1, 'Error there is no odd width2');
     assert((width1 and $00000001) = 0, 'Error width1 is not even');

     destOffset := destLineWidth - (Width2 - 1)*sizeof(double);
     iters1 := width2 div 2;
     iters2 := -width1*sizeof(double);
     bytesWidth2 := (width2 - 1)*sizeof(double);
     Linewidth2_2 := 2*LineWidth2;
     asm
        push ebx;
        push esi;
        push edi;

        mov esi, mt2;  // use the esi register for faster access

        mov ecx, mt1;    // prepare ecx for the "reversed" for loop
        sub ecx, iters2;

        // for y := 0 to height1 - 1:
        mov eax, Height1;
        mov y, eax;

        mov eax, LineWidth2;
        @@forylabel:
            // for x := 0 to width2 - 1:
            mov ebx, iters1;
            @@forxlabel:

                xorpd xmm0, xmm0;   // dest^ := 0
                xorpd xmm7, xmm7;   // (dest + 1)^ := 0;

                mov edi, esi;       // valcounter2

                // for idx := 0 to width1 div 2 do
                mov edx, iters2;

                @@InnerLoop:
                    movupd xmm1, [ecx + edx];

                    // load 2x2 block
                    movupd xmm3, [edi];
                    movapd xmm5, xmm3;

                    movupd xmm4, [edi + eax];
                    add edi, Linewidth2_2;
                    movapd xmm6, xmm4;

                    // swap such that we can immediately multiply
                    movhlps xmm4, xmm5;
                    movlhps xmm3, xmm6;

                    // it's a bit faster if we reuse a register - seems the cpu can
                    // then parallelize the instructions better
                    movapd xmm6, xmm1;

                    // multiply 2x2 and add
                    mulpd xmm3, xmm1;
                    mulpd xmm4, xmm6;
                    addpd xmm0, xmm3;
                    addpd xmm7, xmm4;

                    // end for idx := 0 to width1 div 2
                //dec edx;
                add edx, 16;
                jnz @@InnerLoop;

                // final horizontal addition and compact
                haddpd xmm0, xmm7;

                // store back result
                mov edi, dest;
                movupd [edi], xmm0;

                // increment the pointers
                // inc(mt2), inc(dest);
                //add dword ptr [mt2], 8;
                add esi, 16;
                add dword ptr [dest], 16;

            // end for x := 0 to width2 - 1
            dec ebx;
            jnz @@forxlabel;

            // handle the last column of mt2
            xorpd xmm0, xmm0;   // dest^ := 0
            mov edi, esi;       // valcounter2

            mov ebx, iters2;
            @@InnerLoop2:
                // load element from line
                movsd xmm1, [ecx + ebx];

                // load, multiply and add
                mulsd xmm1, [edi];
                add edi, eax;       // next row

                addsd xmm0, xmm1;
            add ebx, 8;
            jnz @@InnerLoop2;

            mov edi, dest;
            movsd [edi], xmm0;

            // dec(mt2, Width2);
            // inc(PByte(mt1), LineWidth1);
            // inc(PByte(dest), destOffset);
            //mov ebx, bytesWidth2;
            //sub dword ptr [mt2], ebx;
            sub esi, bytesWidth2;
            add ecx, LineWidth1;
            mov ebx, destOffset;
            add dword ptr [dest], ebx;

        // end for y := 0 to height1 - 1
        //dec eax;
        dec y;
        jnz @@forylabel

        pop edi;
        pop esi;
        pop ebx;
     end;
end;

procedure ASMMatrixMultAlignedOddW1OddW2(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1, height1, width2, height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var destOffset : TASMNativeInt;
    iters1, iters2, iters3 : TASMNativeInt;
    Linewidth2_2 : TASMNativeInt;
    y : TASMNativeInt;
begin
     assert(((Cardinal(dest) and $0000000F) = 0) and ((Cardinal(mt1) and $0000000F) = 0) and ((Cardinal(mt2) and $0000000F) = 0), 'Error aligned operations cannot be performed');
     assert(((LineWidth1 and $0000000F) = 0) and ((destLineWidth and $0000000F) = 0) and ((LineWidth2 and $0000000F) = 0), 'Error line widths must be aligned');

     assert((width2 and $00000001) = 1, 'Error there is no odd width2');
     assert((width1 and $00000001) = 1, 'Error width1 is not even');

     destOffset := destLineWidth - (Width2 - 1)*sizeof(double);
     iters1 := width2 div 2;
     iters2 := -(width1 - 1)*sizeof(double);
     iters3 := -width1*sizeof(double);
     Linewidth2_2 := 2*LineWidth2;
     asm
        push ebx;
        push esi;
        push edi;

        mov esi, mt2;  // use the esi register for faster access

        mov ecx, mt1;    // prepare ecx for the "reversed" for loop 
        sub ecx, iters2;

        xorpd xmm2, xmm2;
        
        // for y := 0 to height1 - 1:
        mov eax, Height1;
        mov y, eax;

        mov eax, LineWidth2;
        @@forylabel:
            // for x := 0 to width2 - 1:
            mov ebx, iters1;
            @@forxlabel:

                xorpd xmm0, xmm0;   // dest^ := 0
                xorpd xmm7, xmm7;   // (dest + 1)^ := 0;

                mov edi, esi;       // valcounter2

                // for idx := 0 to width1 div 2 do
                mov edx, iters2;

                @@InnerLoop:
                    movapd xmm1, [ecx + edx];

                    // load 2x2 block
                    movapd xmm3, [edi];
                    movapd xmm5, xmm3;

                    movapd xmm4, [edi + eax];
                    add edi, Linewidth2_2;
                    movapd xmm6, xmm4;

                    // swap such that we can immediately multiply
                    movhlps xmm4, xmm5;
                    movlhps xmm3, xmm6;

                    // it's a bit faster if we reuse a register - seems the cpu can
                    // then parallelize the instructions better
                    movapd xmm6, xmm1;

                    // multiply 2x2 and add
                    mulpd xmm3, xmm1;
                    mulpd xmm4, xmm6;
                    addpd xmm0, xmm3;
                    addpd xmm7, xmm4;

                    // end for idx := 0 to width1 div 2
                //dec edx;
                add edx, 16;
                jnz @@InnerLoop;

                // special treatment of the last column of mt1:
                movddup xmm1, [ecx];

                movupd xmm3, [edi];
                mulpd xmm1, xmm3;

                // final horizontal addition
                xorpd xmm3, xmm3;
                movq xmm2, xmm1;
                addsd xmm0, xmm2;
                haddpd xmm0, xmm3;

                movhlps xmm2, xmm1;
                addsd xmm7, xmm2;
                haddpd xmm7, xmm3;

                // compact result
                movlhps xmm0, xmm7;

                // store back result
                mov edi, dest;
                movntdq [edi], xmm0;

                // increment the pointers
                // inc(mt2), inc(dest);
                //add dword ptr [mt2], 8;
                add esi, 16;
                add dword ptr [dest], 16;

            // end for x := 0 to width2 - 1
            dec ebx;
            jnz @@forxlabel;

            // handle the last column of mt2
            xorpd xmm0, xmm0;   // dest^ := 0
            mov edi, esi;       // valcounter2
            add ecx, 8;         // adjust for odd width1

            mov ebx, iters3;
            @@InnerLoop2:
                // load element from line
                movsd xmm1, [ecx + ebx];

                // load, multiply and add
                mulsd xmm1, [edi];
                add edi, eax;       // next row

                addsd xmm0, xmm1;
            add ebx, 8;
            jnz @@InnerLoop2;

            mov edi, dest;
            movsd [edi], xmm0;

            // dec(mt2, Width2);
            // inc(PByte(mt1), LineWidth1);
            // inc(PByte(dest), destOffset);
            //mov ebx, bytesWidth2;
            //sub dword ptr [mt2], ebx;
            mov esi, mt2;
            add ecx, LineWidth1;
            sub ecx, 8;                // undo odd adjustment
            mov ebx, destOffset;
            add dword ptr [dest], ebx;

        // end for y := 0 to height1 - 1
        //dec eax;
        dec y;
        jnz @@forylabel

        pop edi;
        pop esi;
        pop ebx;
     end;
end;

procedure ASMMatrixMultUnAlignedOddW1OddW2(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1, height1, width2, height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var destOffset : TASMNativeInt;
    iters1, iters2, iters3 : TASMNativeInt;
    Linewidth2_2 : TASMNativeInt;
    y : TASMNativeInt;
begin
     assert((width2 and $00000001) = 1, 'Error there is no odd width2');
     assert((width1 and $00000001) = 1, 'Error width1 is not even');

     destOffset := destLineWidth - (Width2 - 1)*sizeof(double);
     iters1 := width2 div 2;
     iters2 := -(width1 div 2)*2*sizeof(double);
     iters3 := -width1*sizeof(double);
     Linewidth2_2 := 2*LineWidth2;
     asm
        push ebx;
        push esi;
        push edi;

        mov esi, mt2;  // use the esi register for faster access

        mov ecx, mt1;    // prepare ecx for the "reversed" for loop 
        sub ecx, iters2;

        xorpd xmm2, xmm2;
        
        // for y := 0 to height1 - 1:
        mov eax, Height1;
        mov y, eax;

        mov eax, LineWidth2;
        @@forylabel:
            // for x := 0 to width2 - 1:
            mov ebx, iters1;
            @@forxlabel:

                xorpd xmm0, xmm0;   // dest^ := 0
                xorpd xmm7, xmm7;   // (dest + 1)^ := 0;

                mov edi, esi;       // valcounter2

                // for idx := 0 to width1 div 2 do
                mov edx, iters2;

                @@InnerLoop:
                    movupd xmm1, [ecx + edx];

                    // load 2x2 block
                    movupd xmm3, [edi];
                    movapd xmm5, xmm3;

                    movupd xmm4, [edi + eax];
                    add edi, Linewidth2_2;
                    movapd xmm6, xmm4;

                    // swap such that we can immediately multiply
                    movhlps xmm4, xmm5;
                    movlhps xmm3, xmm6;

                    // it's a bit faster if we reuse a register - seems the cpu can
                    // then parallelize the instructions better
                    movapd xmm6, xmm1;

                    // multiply 2x2 and add
                    mulpd xmm3, xmm1;
                    mulpd xmm4, xmm6;
                    addpd xmm0, xmm3;
                    addpd xmm7, xmm4;

                    // end for idx := 0 to width1 div 2
                //dec edx;
                add edx, 16;
                jnz @@InnerLoop;

                // special treatment of the last column of mt1:
                movddup xmm1, [ecx];

                movupd xmm3, [edi];
                mulpd xmm1, xmm3;

                // final horizontal addition
                xorpd xmm3, xmm3;
                movq xmm2, xmm1;
                addsd xmm0, xmm2;
                haddpd xmm0, xmm3;

                movhlps xmm2, xmm1;
                addsd xmm7, xmm2;
                haddpd xmm7, xmm3;

                // compact result
                movlhps xmm0, xmm7;

                // store back result
                mov edi, dest;
                movupd [edi], xmm0;

                // increment the pointers
                // inc(mt2), inc(dest);
                //add dword ptr [mt2], 8;
                add esi, 16;
                add dword ptr [dest], 16;

            // end for x := 0 to width2 - 1
            dec ebx;
            jnz @@forxlabel;

            // handle the last column of mt2
            xorpd xmm0, xmm0;   // dest^ := 0
            mov edi, esi;       // valcounter2
            add ecx, 8;         // adjust for odd width1

            mov ebx, iters3;
            @@InnerLoop2:
                // load element from line
                movsd xmm1, [ecx + ebx];

                // load, multiply and add
                mulsd xmm1, [edi];
                add edi, eax;       // next row

                addsd xmm0, xmm1;
            add ebx, 8;
            jnz @@InnerLoop2;

            mov edi, dest;
            movsd [edi], xmm0;

            // dec(mt2, Width2);
            // inc(PByte(mt1), LineWidth1);
            // inc(PByte(dest), destOffset);
            //mov ebx, bytesWidth2;
            //sub dword ptr [mt2], ebx;
            mov esi, mt2;
            add ecx, LineWidth1;
            sub ecx, 8;                // undo odd adjustment
            mov ebx, destOffset;
            add dword ptr [dest], ebx;

        // end for y := 0 to height1 - 1
        //dec eax;
        dec y;
        jnz @@forylabel

        pop edi;
        pop esi;
        pop ebx;
     end;
end;


// ###########################################
// #### Special multiplication routines (for now only used in QR Decomposition)
// ###########################################

// note the result is stored in mt2 again!
// dest = mt1'*mt2; where mt2 is a lower triangular matrix and the operation is transposition
// the function assumes a unit diagonal (does not touch the real middle elements)
// width and height values are assumed to be the "original" (non transposed) ones
procedure ASMMtxMultTria2T1(dest : PDouble; LineWidthDest : TASMNativeInt; mt1 : PDouble; LineWidth1 : TASMNativeInt;
  mt2 : PDouble; LineWidth2 : TASMNativeInt;
  width1, height1, width2, height2 : TASMNativeInt);
var pMt2 : PDouble;
    width2D2 : TASMNativeInt;
begin
     assert((width1 > 0) and (height1 > 0) and (height1 = height2), 'Dimension error');

     asm
        push ebx;
        push esi;
        push edi;

        // width2D2 := width2 div 2;
        mov eax, width2;
        shr eax, 1;
        mov width2D2, eax;

        // for x := 0 to width1 - 1 do
        mov eax, width1;

        @@forxloop:

          // pMT2 := mt2;
          // pDest := dest;

          mov ebx, mt2;
          mov pMT2, ebx;

          mov edx, dest;   // edx is pDest


          // for y := 0 to width2D2 - 1 do
          mov ecx, width2D2;
          test ecx, ecx;
          jz @@foryloopend;

          xor ecx, ecx;
          @@foryloop:

               // valCounter1 := PConstDoubleArr(mt1);
               // inc(PByte(valCounter1), 2*y*LineWidth1);
               mov esi, mt1;
               mov ebx, ecx;
               add ebx, ebx;
               imul ebx, LineWidth1;
               add esi, ebx;

               // valCounter2 := PConstDoubleArr(pMT2);
               // inc(PByte(valCounter2), (2*y + 1)*LineWidth2);
               mov edi, pMt2;
               mov ebx, ecx;
               add ebx, ebx;
               imul ebx, LineWidth2;
               add ebx, LineWidth2;
               add edi, ebx;

               // tmp[0] := valCounter1^[0];
               // inc(PByte(valCounter1), LineWidth1);
               movsd xmm0, [esi];
               add esi, LineWidth1;

               // if height2 - 2*y - 1 > 0 then
               mov ebx, ecx;
               add ebx, ebx;
               inc ebx;

               cmp ebx, height2;
               jnl @@PreInnerLoop;
                   // tmp[0] := tmp[0] + valCounter1^[0]*valCounter2^[0];
                   // tmp[1] := valCounter1^[0];
                   movsd xmm1, [esi];
                   movlhps xmm0, xmm1;

                   mulsd xmm1, [edi];
                   addsd xmm0, xmm1;

                   //inc(PByte(valCounter1), LineWidth1);
                   //inc(PByte(valCounter2), LineWidth2);

                   add esi, LineWidth1;
                   add edi, LineWidth2;

               @@PreInnerLoop:

               // rest is a double column!

               // prepare loop
               mov ebx, height2;
               sub ebx, ecx;
               sub ebx, ecx;
               sub ebx, 2;

               test ebx, ebx;
               jle @@InnerLoopEnd;

               @@InnerLoop:
                  // tmp[0] := tmp[0] + valCounter1^[0]*valCounter2^[0];
                  // tmp[1] := tmp[1] + valCounter1^[0]*valCounter2^[1];
                  movddup xmm1, [esi];
                  movupd xmm2, [edi];

                  mulpd xmm2, xmm1;
                  addpd xmm0, xmm2;

                  //inc(PByte(valCounter1), LineWidth1);
                  //inc(PByte(valCounter2), LineWidth2);

                  add esi, LineWidth1;
                  add edi, LineWidth2;

               dec ebx;
               jnz @@InnerLoop;

               @@InnerLoopEnd:


               // write back result

               // pDest^ := tmp[0];
               // PDouble(TASMNativeUInt(pDest) + sizeof(double))^ := tmp[1];

               movupd [edx], xmm0;

               // inc(pDest, 2);
               // inc(pMT2, 2);
               add edx, 16;
               add pMT2, 16;

          // end foryloop
          inc ecx;
          cmp ecx, width2D2;
          jne @@foryloop;

          @@foryloopend:


          //if (width2 and $01) = 1 then
          mov ecx, width2;
          and ecx, 1;

          jz @@ifend1;

            // special handling of last column (just copy the value)

            // valCounter1 := PConstDoubleArr(mt1);
            mov ecx, mt1;

            //inc(PByte(valCounter1), LineWidth1*(height1 - 1));
            mov ebx, height1;
            dec ebx;
            imul ebx, LineWidth1;

            // pDest^ := valCounter1^[0];
            movsd xmm0, [ecx + ebx];
            movsd [edx], xmm0;
          @@ifend1:


          //inc(mt1);
          //inc(PByte(dest), LineWidthDest);
          add mt1, 8;
          mov ebx, LineWidthDest;
          add dest, ebx;

       // end for loop
       dec eax;
       jnz @@forxloop;

       // epilog
       pop edi;
       pop esi;
       pop ebx;
     end;
end;

procedure ASMMtxMultTria2Store1Unit(mt1 : PDouble; LineWidth1 : TASMNativeInt; mt2 : PDouble; LineWidth2 : TASMNativeInt;
  width1, height1, width2, height2 : TASMNativeInt);
var iter : TASMNativeInt;
begin
     assert((width1 > 0) and (height1 > 0) and (width1 = height2), 'Dimension error');
     assert(width1 = width2, 'Width need to be the same');

     iter := -(width1-1)*sizeof(double);
     asm
        push ebx;
        push edi;
        push esi;
        // init

        // inc(mt2, width2 - 1);
        mov ebx, mt2;
        mov edx, width2;
        dec edx;
        imul edx, 8; //sizeof(double)
        add ebx, edx;

        mov edx, width2;
        dec edx;
        mov width2, edx;

        mov edx, LineWidth2;

        // for x := 0 to width2 - 2 do
        @@forxloop:
           mov edi, height1;

           mov eax, mt1;
           sub eax, iter;

           // for y := 0 to height1 - 1
           @@foryloop:
              // tmp := 0;
              xorpd xmm0, xmm0;

              // ebx, mt2
              mov ecx, ebx;

              // for idx := 0 to width1 - x - 2
              mov esi, iter;

              // check if we have enough iterations:
              cmp esi, 0;
              jge @@foridxloopend;

              @@foridxloop:
                 movsd xmm1, [ecx];
                 movsd xmm2, [eax + esi];

                 add ecx, edx;  // + linewidth2

                 mulsd xmm1, xmm2;
                 addsd xmm0, xmm1;

              add esi, 8;
              jnz @@foridxloop;

              @@foridxloopend:

              // last element is unit
              addsd xmm0, [eax];

              // PConstDoubleArr(pmt1)^[width2 - 1 - x] := tmp;
              mov ecx, eax;
              add ecx, iter;
              mov esi, width2;
              //dec esi;
              movsd [ecx + 8*esi], xmm0;

              // inc(PByte(pmT1), LineWidth1);
              add eax, LineWidth1;

           dec edi;
           jnz @@foryloop;

           // reduce for idx loop
           add iter, 8;
           // dec(mt2);
           sub ebx, 8;

        dec width2;
        jnz @@forxloop;

        @@endproc:

        // cleanup stack
        pop esi;
        pop edi;
        pop ebx;
     end;
end;

procedure ASMMtxMultTria2Store1(mt1 : PDouble; LineWidth1 : TASMNativeInt; mt2 : PDouble; LineWidth2 : TASMNativeInt;
  width1, height1, width2, height2 : TASMNativeInt);
var wm1 : TASMNativeInt;
    iter : TASMNativeInt;
begin
     assert((width1 > 0) and (height1 > 0) and (width1 = height2), 'Dimension error');

     wm1 := width2 - 1;
     iter := -width1*sizeof(double);
     asm
        push ebx;
        push edi;
        push esi;
        // init

        // inc(mt2, width2 - 1);
        mov ebx, mt2;
        mov edx, wm1;
        imul edx, 8; //sizeof(double)
        add ebx, edx;

        mov edx, LineWidth2;

        // for x := 0 to width2 - 1 do
        @@forxloop:
           mov edi, height1;

           mov eax, mt1;
           sub eax, iter;

           // for y := 0 to height1 - 1
           @@foryloop:
              // tmp := 0;
              xorpd xmm0, xmm0;

              // ebx, mt2
              mov ecx, ebx;

              // for idx := 0 to width1 - x - 1
              mov esi, iter;

              // check if we have enough iterations:
              test esi, esi;
              jz @@foridxloopend;

              @@foridxloop:
                 movsd xmm1, [ecx];
                 movsd xmm2, [eax + esi]

                 add ecx, edx;  // + linewidth2

                 mulsd xmm1, xmm2;
                 addsd xmm0, xmm1;

              add esi, 8;
              jnz @@foridxloop;

              @@foridxloopend:

              // PConstDoubleArr(pmt1)^[width2 - 1 - x] := tmp;
              mov ecx, eax;
              add ecx, iter;
              mov esi, width2;
              dec esi;
              movsd [ecx + 8*esi], xmm0;

              // inc(PByte(pmT1), LineWidth1);
              add eax, LineWidth1;

           dec edi;
           jnz @@foryloop;

           // reduce for idx loop
           add iter, 8;
           // dec(mt2);
           sub ebx, 8;

        dec width2;
        jnz @@forxloop;

        // cleanup stack
        pop esi;
        pop edi;
        pop ebx;
     end;
end;


// note the result is stored in mt1 again!
// mt1 = mt1*mt2; where mt2 is an upper triangular matrix
procedure ASMMtxMultTria2Store12(mt1 : PDouble; LineWidth1 : TASMNativeInt; mt2 : PDouble; LineWidth2 : TASMNativeInt;
  width1, height1, width2, height2 : TASMNativeInt);
var wm1 : TASMNativeInt;
    iter : TASMNativeInt;
begin
     assert((width1 > 0) and (height1 > 0) and (width1 = height2), 'Dimension error');

     wm1 := width2 - 1;
     iter := -width1*sizeof(double);
     asm
        push ebx;
        push edi;
        push esi;
        // init

        // inc(mt2, width2 - 1);
        mov ebx, mt2;
        mov edx, wm1;
        imul edx, 8; //sizeof(double)
        add ebx, edx;

        mov edx, LineWidth2;

        // for x := 0 to width2 - 1 do
        @@forxloop:
           mov edi, height1;

           mov eax, mt1;
           sub eax, iter;

           // for y := 0 to height1 - 1
           @@foryloop:
              // tmp := 0;
              xorpd xmm0, xmm0;

              // ebx, mt2
              mov ecx, ebx;

              // for idx := 0 to width1 - x - 1
              mov esi, iter;
              //test esi, esi;
              //jge @@foridxloopend;

              // check if we can do an unrolled loop
              add esi, 32; // -4*sizeof(double)
              jg @@foridxloopinit;

              // unrolled loop
              @@foridxunrolled:
                 movsd xmm1, [ecx];
                 movsd xmm3, [ecx + edx];
                 movupd xmm2, [eax + esi - 32];

                 add ecx, edx;
                 add ecx, edx;

                 movlhps xmm1, xmm3;
                 mulpd xmm1, xmm2;
                 addpd xmm0, xmm1;

                 movsd xmm1, [ecx];
                 movsd xmm3, [ecx + edx];
                 movupd xmm2, [eax + esi - 16];

                 add ecx, edx;
                 add ecx, edx;

                 movlhps xmm1, xmm3;
                 mulpd xmm1, xmm2;
                 addpd xmm0, xmm1;

              add esi, 32;
              jl @@foridxunrolled;

              movhlps xmm1, xmm0;
              addsd xmm0, xmm1;

              @@foridxloopinit:
              // undo too much substractiton
              sub esi, 32;

              // check if we have enough iterations:
              test esi, esi;
              jz @@foridxloopend;


              @@foridxloop:
                 movsd xmm1, [ecx];
                 movsd xmm2, [eax + esi]

                 add ecx, edx;  // + linewidth2

                 mulsd xmm1, xmm2;
                 addsd xmm0, xmm1;

              add esi, 8;
              jnz @@foridxloop;

              @@foridxloopend:

              // PConstDoubleArr(pmt1)^[width2 - 1 - x] := tmp;
              mov ecx, eax;
              add ecx, iter;
              mov esi, width2;
              dec esi;
              shl esi, 3; // *sizeof(doubl)
              movsd [ecx + esi], xmm0;

              // inc(PByte(pmT1), LineWidth1);
              add eax, LineWidth1;

           dec edi;
           jnz @@foryloop;

           // reduce for idx loop
           add iter, 8;
           // dec(mt2);
           sub ebx, 8;

        dec width2;
        jnz @@forxloop;

        // cleanup stack
        pop esi;
        pop edi;
        pop ebx;
     end;
end;

// calculates mt1 = mt1*mt2', mt2 = lower triangular matrix. diagonal elements are assumed to be 1!
procedure ASMMtxMultLowTria2T2Store1(mt1 : PDouble; LineWidth1 : TASMNativeInt; mt2 : PDouble; LineWidth2 : TASMNativeInt;
  width1, height1, width2, height2 : TASMNativeInt);
var iter : TASMNativeInt;
begin
     assert((width1 > 0) and (height1 > 0) and (width1 = height2), 'Dimension error');

     iter := -(width2 - 1)*sizeof(double);

     asm
        push ebx;
        push edi;
        push esi;

        // edx = iter
        mov edx, iter;

        // start from bottom
        // ebx: mt2
        // inc(PByte(mt2),(height2 - 1)*LineWidth2);
        mov ebx, mt2;
        mov eax, height2;
        dec eax;
        imul eax, LineWidth2;
        add ebx, eax;
        sub ebx, edx;

        // for x := 0 to width2 - 2
        mov ecx, width2;
        dec ecx;
        jz @@endproc;
        @@forxloop:
           mov eax, mt1;
           sub eax, edx;

           // for y := 0 to height1 - 1
           mov esi, height1;
           @@foryloop:
              xorpd xmm0, xmm0;

              // for idx := 0 to width2 - x - 2
              mov edi, edx;
              test edi, edi;
              jz @@foridxloopend;

              // unrolled loop 2x2
              add edi, 32;
              jg @@foridxloopStart;

              @@foridxlongloop:
                 movupd xmm1, [eax + edi - 32];
                 movupd xmm2, [ebx + edi - 32];
                 mulpd xmm1, xmm2;
                 addpd xmm0, xmm1;

                 movupd xmm1, [eax + edi - 16];
                 movupd xmm2, [ebx + edi - 16];
                 mulpd xmm1, xmm2;
                 addpd xmm0, xmm1;
              add edi, 32;
              jl @@foridxlongloop;

              haddpd xmm0, xmm0;

              @@foridxloopStart:
              sub edi, 32;

              jz @@foridxloopend;

              @@foridxloop:
                 movsd xmm1, [eax + edi];
                 movsd xmm2, [ebx + edi];
                 mulsd xmm1, xmm2;
                 addsd xmm0, xmm1;

              add edi, 8;
              jnz @@foridxloop;

              @@foridxloopend:

              // last element is unit:
              movsd xmm1, [eax];
              addsd xmm0, xmm1;

              // write back
              // PConstDoubleArr(pMt1)^[width2 - x - 1] := tmp + valCounter1^[width2 - x - 1];
              movsd [eax], xmm0;

              add eax, LineWidth1;

           dec esi;
           jnz @@foryloop;

           // dec(PByte(mt2), LineWidth2);
           sub ebx, LineWidth2;
           sub ebx, 8;

           // adjust iterator to the next x value for the idxloop
           add edx, 8;

        dec ecx;
        jnz @@forxloop;

        @@endproc:

        pop esi;
        pop edi;
        pop ebx;
     end;
end;


procedure ASMMtxMultTria2T1StoreT1(mt1 : PDouble; LineWidth1 : TASMNativeInt; mt2 : PDouble; LineWidth2 : TASMNativeInt;
  width1, height1, width2, height2 : TASMNativeInt);
var iter : TASMNativeInt;
    testExitLoopVal : TASMNativeInt;
begin
     assert((width1 > 0) and (height1 > 0) and (width1 = width2), 'Dimension error');

     iter := -width1*sizeof(double);
     testExitLoopVal := height2*sizeof(double) + iter;

     asm
        push ebx;
        push esi;
        push edi;

        // eax := mt1
        mov eax, mt1;
        sub eax, iter;

        // for y loop
        @@foryloop:
           mov ebx, mt2;
           sub ebx, iter;
           mov esi, iter;

           @@forxloop:
              xorpd xmm0, xmm0; // temp := 0

              mov edx, esi;  // loop counter x;

              // test if height2 > width1 and loop counter > width1
              mov edi, edx;
              test edi, edi;
              jge @@foriloopend;

              // in case x is odd -> handle the first element separately
              and edi, $E;
              jz @@foriloopinit;

              // single element handling
              movsd xmm1, [eax + edx];
              movsd xmm2, [ebx + edx];
              mulsd xmm1, xmm2;
              addsd xmm0, xmm1;
              add edx, 8;

              @@foriloopinit:

              // in case the last single x element was handled we do not need further looping
              test edx, edx;
              jz @@foriloopend;

              // for i := x to width1 - 1
              @@foriloop:
                 // two elements at a time:
                 movupd xmm1, [eax + edx];
                 movupd xmm2, [ebx + edx];
                 mulpd xmm1, xmm2;
                 addpd xmm0, xmm1;

              add edx, 16;
              jnz @@foriloop;

              @@foriloopend:

              // final result
              haddpd xmm0, xmm0;
              movsd [eax + esi], xmm0;

              add ebx, LineWidth2;
              add esi, 8;

           mov edx, testExitLoopVal;
           cmp esi, edx;
           jne @@forxloop;

           add eax, LineWidth1;
        dec height1;
        jnz @@foryloop;

        pop edi;
        pop esi;

        pop ebx;
     end;
end;

// W = C1*V1*T -> V1 is an upper triangular matrix with assumed unit diagonal entries. Operation on V1 transposition
procedure ASMMtxMultTria2TUpperUnit(dest : PDouble; LineWidthDest : TASMNativeInt; mt1 : PDouble; LineWidth1 : TASMNativeInt; mt2 : PDouble; LineWidth2 : TASMNativeInt;
  width1, height1, width2, height2 : TASMNativeInt);
var iter : TASMNativeInt;
    testExitLoopVal : TASMNativeInt;
begin
     assert((width1 > 0) and (height1 > 0) and (width1 = width2), 'Dimension error');

     iter := -width1*sizeof(double);
     testExitLoopVal := height2*sizeof(double) + iter;

     asm
        push ebx;
        push esi;
        push edi;

        // eax := mt1
        mov eax, mt1;
        sub eax, iter;

        // ecx = dest
        mov ecx, dest;
        sub ecx, iter;

        // for y loop
        @@foryloop:
           mov ebx, mt2;
           sub ebx, iter;
           mov esi, iter;

           @@forxloop:
              xorpd xmm0, xmm0; // temp := 0

              mov edx, esi;  // loop counter x;

              // test if height2 > width1 and loop counter > width1
              mov edi, edx;
              test edi, edi;
              jge @@foriloopend;

              // in case x is odd -> handle the first element separately
              and edi, $E;
              jz @@foriloopinit;

              // single element handling -> mt1 first element is assumed unit!
              movsd xmm0, [eax + edx];
              add edx, 8;

              jmp @@AfterLoopInit;

              @@foriloopinit:

              test edx, edx;
              jz @@foriloopend;

              // two elements init at a time:
              movsd xmm0, [eax + edx];
              movsd xmm1, [eax + edx + 8];
              movsd xmm2, [ebx + edx + 8];
              mulsd xmm1, xmm2;
              addsd xmm0, xmm1;

              add edx, 16;

              @@AfterLoopInit:

              // in case the last single x element was handled we do not need further looping
              test edx, edx;
              jz @@foriloopend;

              // for i := x to width1 - 1
              @@foriloop:
                 // two elements at a time:
                 movupd xmm1, [eax + edx];
                 movupd xmm2, [ebx + edx];
                 mulpd xmm1, xmm2;
                 addpd xmm0, xmm1;

              add edx, 16;
              jnz @@foriloop;

              @@foriloopend:

              // final result
              haddpd xmm0, xmm0;
              movsd [ecx + esi], xmm0;

              add ebx, LineWidth2;
              add esi, 8;

           mov edx, testExitLoopVal;
           cmp esi, edx;
           jne @@forxloop;

           add eax, LineWidth1;
           add ecx, LineWidthDest;
        dec height1;
        jnz @@foryloop;

        pop edi;
        pop esi;

        pop ebx;
     end;
end;

{$ENDIF}

end.
