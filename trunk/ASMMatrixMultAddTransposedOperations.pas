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


unit ASMMatrixMultAddTransposedOperations;

// ##############################################################
// #### Assembler optimized matrix multiplication assuming a transposed second
// #### matrix
// ##############################################################

interface

{$IFNDEF CPUX64}

procedure ASMMatrixMultAddAlignedEvenW1EvenH2Transposed(dest : PDouble; const destLineWidth : integer; mt1, mt2 : PDouble; width1 : integer; height1 : integer; width2 : integer; height2 : integer; const LineWidth1, LineWidth2 : integer);
procedure ASMMatrixMultAddUnAlignedEvenW1EvenH2Transposed(dest : PDouble; const destLineWidth : integer; mt1, mt2 : PDouble; width1 : integer; height1 : integer; width2 : integer; height2 : integer; const LineWidth1, LineWidth2 : integer);

procedure ASMMatrixMultAddAlignedEvenW1OddH2Transposed(dest : PDouble; const destLineWidth : integer; mt1, mt2 : PDouble; width1 : integer; height1 : integer; width2 : integer; height2 : integer; const LineWidth1, LineWidth2 : integer);
procedure ASMMatrixMultAddUnAlignedEvenW1OddH2Transposed(dest : PDouble; const destLineWidth : integer; mt1, mt2 : PDouble; width1 : integer; height1 : integer; width2 : integer; height2 : integer; const LineWidth1, LineWidth2 : integer);

procedure ASMMatrixMultAddAlignedOddW1EvenH2Transposed(dest : PDouble; const destLineWidth : integer; mt1, mt2 : PDouble; width1 : integer; height1 : integer; width2 : integer; height2 : integer; const LineWidth1, LineWidth2 : integer);
procedure ASMMatrixMultAddUnAlignedOddW1EvenH2Transposed(dest : PDouble; const destLineWidth : integer; mt1, mt2 : PDouble; width1 : integer; height1 : integer; width2 : integer; height2 : integer; const LineWidth1, LineWidth2 : integer);

procedure ASMMatrixMultAddAlignedOddW1OddH2Transposed(dest : PDouble; const destLineWidth : integer; mt1, mt2 : PDouble; width1 : integer; height1 : integer; width2 : integer; height2 : integer; const LineWidth1, LineWidth2 : integer);
procedure ASMMatrixMultAddUnAlignedOddW1OddH2Transposed(dest : PDouble; const destLineWidth : integer; mt1, mt2 : PDouble; width1 : integer; height1 : integer; width2 : integer; height2 : integer; const LineWidth1, LineWidth2 : integer);

{$ENDIF}

implementation

{$IFNDEF CPUX64}

procedure ASMMatrixMultAddUnAlignedEvenW1OddH2Transposed(dest : PDouble; const destLineWidth : integer; mt1, mt2 : PDouble; width1 : integer; height1 : integer; width2 : integer; height2 : integer; const LineWidth1, LineWidth2 : integer);
var iters1, iters2 : integer;
    y : integer;
    LineWidth2_2 : integer;
begin
     assert((width1 and $00000001 = 0) and (height2 and $00000001 = 1), 'Error width1 or height2 do not match');

     iters1 := height2 div 2;
     iters2 := -(width1 div 2)*2*sizeof(double);
     LineWidth2_2 := 2*LineWidth2;
     asm
        push ebx;
        push eax;
        push ecx;
        push edx;
        push esi;
        push edi;

        // prepare matrix pointers - remove constant offset here instead each time in the loop:
        mov ecx, mt1;
        mov edi, mt2;
        sub ecx, iters2;
        sub edi, iters2;
        mov mt1, ecx;
        mov mt2, edi;

        xorpd xmm2, xmm2;
        xorpd xmm6, xmm6;

        // for y := 0 to height1 - 1:
        mov eax, Height1;
        mov y, eax;

        @@forylabel:
            mov ecx, mt1;    // prepare ecx for the "reversed" for loop
            mov esi, dest;
            mov edi, mt2;

            // for x := 0 to width2 - 1:
            mov ebx, iters1;
            @@fory2label:
                mov eax, edi;
                add eax, Linewidth2;

                xorpd xmm7, xmm7;   // dest + 1 := 0;
                movupd xmm0, [esi];
                movhlps xmm0, xmm7;
                pslldq xmm0, 8;   // shift away to clear half of the register

                // for idx := 0 to width1 div 2 do
                mov edx, iters2;

                @@InnerLoop:
                    movupd xmm1, [ecx + edx];
                    movapd xmm5, xmm1;

                    // load 2x2 block
                    movupd xmm3, [edi + edx];
                    movupd xmm4, [eax + edx];

                    // multiply 2x2 and add
                    mulpd xmm3, xmm1;
                    mulpd xmm4, xmm5;

                    addpd xmm0, xmm3;
                    addpd xmm7, xmm4;

                    // end for idx := 0 to width1 div 2
                add edx, 16;
                jnz @@InnerLoop;

                haddpd xmm0, xmm2;
                haddpd xmm7, xmm6;

                // compact result
                movlhps xmm0, xmm7;

                // store back result
                movupd [esi], xmm0;

                // increment the pointers
                // inc(mt2), inc(dest);
                //add dword ptr [mt2], 8;
                add esi, 16;
                add edi, LineWidth2_2;
            // end for x := 0 to width2 - 1
            dec ebx;
            jnz @@fory2label;

            // take care of the last line separatly
            xorpd xmm0, xmm0;   // dest^ := 0
            movlpd xmm0, [esi];
            // for idx := 0 to width1 div 2 do
            mov edx, iters2;

            @@InnerLoop2:
                movupd xmm1, [ecx + edx];
                movupd xmm3, [edi + edx];

                // multiply 2x2 and add
                mulpd xmm3, xmm1;
                addpd xmm0, xmm3;

                // end for idx := 0 to width1 div 2
            add edx, 16;
            jnz @@InnerLoop2;

            haddpd xmm0, xmm2;

            // store back result
            movlpd [esi], xmm0;

            // dec(mt2, Width2);
            // inc(PByte(mt1), LineWidth1);
            // inc(PByte(dest), destOffset);
            //mov ebx, bytesWidth2;
            //sub dword ptr [mt2], ebx;
            mov ebx, LineWidth1;
            add dword ptr [mt1], ebx;
            mov ebx, DestLineWidth;
            add dword ptr [dest], ebx;

        // end for y := 0 to height1 - 1
        //dec eax;
        dec y;
        jnz @@forylabel

        pop edi;
        pop esi;
        pop edx;
        pop ecx;
        pop eax;
        pop ebx;
     end;
end;

procedure ASMMatrixMultAddAlignedEvenW1OddH2Transposed(dest : PDouble; const destLineWidth : integer; mt1, mt2 : PDouble; width1 : integer; height1 : integer; width2 : integer; height2 : integer; const LineWidth1, LineWidth2 : integer);
var iters1, iters2 : integer;
    y : integer;
    LineWidth2_2 : integer;
begin
     assert(((Cardinal(dest) and $0000000F) = 0) and ((Cardinal(mt1) and $0000000F) = 0) and ((Cardinal(mt2) and $0000000F) = 0), 'Error aligned operations cannot be performed');
     assert(((destLineWidth and $0000000F) = 0) and ((LineWidth1 and $0000000F) = 0) and ((LineWidth2 and $0000000F) = 0), 'Error aligned operations cannot be performed');

     assert((width1 and $00000001 = 0) and (height2 and $00000001 = 1), 'Error widths are not even');

     iters1 := height2 div 2;
     iters2 := -(width1 div 2)*2*sizeof(double);
     LineWidth2_2 := 2*LineWidth2;
     asm
        push ebx;
        push eax;
        push ecx;
        push edx;
        push esi;
        push edi;

        // prepare matrix pointers - remove constant offset here instead each time in the loop:
        mov ecx, mt1;
        mov edi, mt2;
        sub ecx, iters2;
        sub edi, iters2;
        mov mt1, ecx;
        mov mt2, edi;

        xorpd xmm2, xmm2;
        xorpd xmm6, xmm6;

        // for y := 0 to height1 - 1:
        mov eax, Height1;
        mov y, eax;

        @@forylabel:
            mov ecx, mt1;    // prepare ecx for the "reversed" for loop
            mov esi, dest;
            mov edi, mt2;

            // for x := 0 to width2 - 1:
            mov ebx, iters1;
            @@fory2label:
                mov eax, edi;
                add eax, Linewidth2;

                xorpd xmm7, xmm7;   // dest + 1 := 0;
                movapd xmm0, [esi];
                movhlps xmm0, xmm7;
                pslldq xmm0, 8;   // shift away to clear half of the register


                // for idx := 0 to width1 div 2 do
                mov edx, iters2;

                @@InnerLoop:
                    movapd xmm1, [ecx + edx];
                    movapd xmm5, xmm1;

                    // load 2x2 block
                    movapd xmm3, [edi + edx];
                    movapd xmm4, [eax + edx];

                    // multiply 2x2 and add
                    mulpd xmm3, xmm1;
                    mulpd xmm4, xmm5;

                    addpd xmm0, xmm3;
                    addpd xmm7, xmm4;

                    // end for idx := 0 to width1 div 2
                add edx, 16;
                jnz @@InnerLoop;

                haddpd xmm0, xmm2;
                haddpd xmm7, xmm6;

                // compact result
                movlhps xmm0, xmm7;

                // store back result
                movntdq [esi], xmm0;

                // increment the pointers
                // inc(mt2), inc(dest);
                //add dword ptr [mt2], 8;
                add esi, 16;
                add edi, LineWidth2_2;
            // end for x := 0 to width2 - 1
            dec ebx;
            jnz @@fory2label;

            // take care of the last line separatly
            xorpd xmm0, xmm0;   // dest^ := 0
            movlpd xmm0, [esi];
            
            // for idx := 0 to width1 div 2 do
            mov edx, iters2;

            @@InnerLoop2:
                movapd xmm1, [ecx + edx];
                movapd xmm3, [edi + edx];

                // multiply 2x2 and add
                mulpd xmm3, xmm1;
                addpd xmm0, xmm3;

                // end for idx := 0 to width1 div 2
            add edx, 16;
            jnz @@InnerLoop2;

            haddpd xmm0, xmm2;

            // store back result
            movlpd [esi], xmm0;

            // dec(mt2, Width2);
            // inc(PByte(mt1), LineWidth1);
            // inc(PByte(dest), destOffset);
            //mov ebx, bytesWidth2;
            //sub dword ptr [mt2], ebx;
            mov ebx, LineWidth1;
            add dword ptr [mt1], ebx;
            mov ebx, DestLineWidth;
            add dword ptr [dest], ebx;

        // end for y := 0 to height1 - 1
        //dec eax;
        dec y;
        jnz @@forylabel

        pop edi;
        pop esi;
        pop edx;
        pop ecx;
        pop eax;
        pop ebx;
     end;
end;

// note mt2 is transposed this time -> width1 and width2 must be the same!
procedure ASMMatrixMultAddAlignedEvenW1EvenH2Transposed(dest : PDouble; const destLineWidth : integer; mt1, mt2 : PDouble; width1 : integer; height1 : integer; width2 : integer; height2 : integer; const LineWidth1, LineWidth2 : integer);
var iters1, iters2 : integer;
    y : integer;
    LineWidth2_2 : integer;
begin
     assert(((Cardinal(dest) and $0000000F) = 0) and ((Cardinal(mt1) and $0000000F) = 0) and ((Cardinal(mt2) and $0000000F) = 0), 'Error aligned operations cannot be performed');
     assert(((destLineWidth and $0000000F) = 0) and ((LineWidth1 and $0000000F) = 0) and ((LineWidth2 and $0000000F) = 0), 'Error aligned operations cannot be performed');

     assert((width1 and $00000001 = 0) and (width2 and $00000001 = 0), 'Error widths are not even');

     iters1 := height2 div 2;
     iters2 := -(width1 div 2)*2*sizeof(double);
     LineWidth2_2 := 2*LineWidth2;
     asm
        push ebx;
        push eax;
        push ecx;
        push edx;
        push esi;
        push edi;

        // prepare matrix pointers - remove constant offset here instead each time in the loop:
        mov ecx, mt1;
        mov edi, mt2;
        sub ecx, iters2;
        sub edi, iters2;
        mov mt1, ecx;
        mov mt2, edi;

        xorpd xmm2, xmm2;
        xorpd xmm6, xmm6;

        // for y := 0 to height1 - 1:
        mov eax, Height1;
        mov y, eax;

        @@forylabel:
            mov ecx, mt1;    // prepare ecx for the "reversed" for loop
            mov esi, dest;
            mov edi, mt2;

            // for x := 0 to width2 - 1:
            mov ebx, iters1;
            @@fory2label:
                mov eax, edi;
                add eax, Linewidth2;

                xorpd xmm7, xmm7;   // dest + 1 := 0;
                movapd xmm0, [esi];
                movhlps xmm0, xmm7;
                pslldq xmm0, 8;   // shift away to clear half of the register
                
                // for idx := 0 to width1 div 2 do
                mov edx, iters2;

                @@InnerLoop:
                    movapd xmm1, [ecx + edx];
                    movapd xmm5, xmm1;

                    // load 2x2 block
                    movapd xmm3, [edi + edx];
                    movapd xmm4, [eax + edx];

                    // multiply 2x2 and add
                    mulpd xmm3, xmm1;
                    mulpd xmm4, xmm5;

                    addpd xmm0, xmm3;
                    addpd xmm7, xmm4;
                    
                    // end for idx := 0 to width1 div 2
                add edx, 16;
                jnz @@InnerLoop;

                haddpd xmm0, xmm2;
                haddpd xmm7, xmm6;

                // compact result
                movlhps xmm0, xmm7;

                // store back result
                movntdq [esi], xmm0;

                // increment the pointers
                // inc(mt2), inc(dest);
                //add dword ptr [mt2], 8;
                add esi, 16;
                add edi, LineWidth2_2;
            // end for x := 0 to width2 - 1
            dec ebx;
            jnz @@fory2label;

            // dec(mt2, Width2);
            // inc(PByte(mt1), LineWidth1);
            // inc(PByte(dest), destOffset);
            //mov ebx, bytesWidth2;
            //sub dword ptr [mt2], ebx;
            mov ebx, LineWidth1;
            add dword ptr [mt1], ebx;
            mov ebx, DestLineWidth;
            add dword ptr [dest], ebx;

        // end for y := 0 to height1 - 1
        //dec eax;
        dec y;
        jnz @@forylabel

        pop edi;
        pop esi;
        pop edx;
        pop ecx;
        pop eax;
        pop ebx;
     end;
end;

procedure ASMMatrixMultAddUnAlignedEvenW1EvenH2Transposed(dest : PDouble; const destLineWidth : integer; mt1, mt2 : PDouble; width1 : integer; height1 : integer; width2 : integer; height2 : integer; const LineWidth1, LineWidth2 : integer);
var iters1, iters2 : integer;
    y : integer;
    LineWidth2_2 : integer;
begin
     assert((width1 and $00000001 = 0) and (height2 and $00000001 = 0), 'Error widths are not even');

     iters1 := height2 div 2;
     iters2 := -(width1 div 2)*2*sizeof(double);
     LineWidth2_2 := 2*LineWidth2;
     asm
        push ebx;
        push eax;
        push ecx;
        push edx;
        push esi;
        push edi;

        mov ecx, mt1;
        mov edi, mt2;
        sub ecx, iters2;
        sub edi, iters2;
        mov mt1, ecx;
        mov mt2, edi;

        // for y := 0 to height1 - 1:
        mov eax, Height1;
        mov y, eax;

        @@forylabel:
            mov ecx, mt1;    // prepare ecx for the "reversed" for loop
            mov esi, dest;
            mov edi, mt2;

            // for x := 0 to width2 - 1:
            mov ebx, iters1;
            @@fory2label:
                mov eax, edi;
                add eax, Linewidth2;

                xorpd xmm7, xmm7;   // dest + 1 := 0;
                movupd xmm0, [esi];
                movhlps xmm0, xmm7;
                pslldq xmm0, 8;   // shift away to clear half of the register

                // for idx := 0 to width1 div 2 do
                mov edx, iters2;

                @@InnerLoop:
                    movupd xmm1, [ecx + edx];
                    movapd xmm5, xmm1;

                    // load 2x2 block
                    movupd xmm3, [edi + edx];
                    movupd xmm4, [eax + edx];

                    // multiply 2x2 and add
                    mulpd xmm3, xmm1;
                    mulpd xmm4, xmm5;

                    addpd xmm0, xmm3;
                    addpd xmm7, xmm4;
                    
                    // end for idx := 0 to width1 div 2
                //dec edx;
                add edx, 16;
                jnz @@InnerLoop;

                xorpd xmm2, xmm2;
                haddpd xmm0, xmm2;
                haddpd xmm7, xmm2;

                // compact result
                movlhps xmm0, xmm7;

                // store back result
                movupd [esi], xmm0;

                // increment the pointers
                // inc(mt2), inc(dest);
                //add dword ptr [mt2], 8;
                add esi, 16;
                add edi, LineWidth2_2;
            // end for x := 0 to width2 - 1
            dec ebx;
            jnz @@fory2label;

            // dec(mt2, Width2);
            // inc(PByte(mt1), LineWidth1);
            // inc(PByte(dest), destOffset);
            //mov ebx, bytesWidth2;
            //sub dword ptr [mt2], ebx;
            mov ebx, LineWidth1;
            add dword ptr [mt1], ebx;
            mov ebx, DestLineWidth;
            add dword ptr [dest], ebx;

        // end for y := 0 to height1 - 1
        //dec eax;
        dec y;
        jnz @@forylabel

        pop edi;
        pop esi;
        pop edx;
        pop ecx;
        pop eax;
        pop ebx;
     end;
end;

procedure ASMMatrixMultAddUnAlignedOddW1EvenH2Transposed(dest : PDouble; const destLineWidth : integer; mt1, mt2 : PDouble; width1 : integer; height1 : integer; width2 : integer; height2 : integer; const LineWidth1, LineWidth2 : integer);
var iters1, iters2 : integer;
    y : integer;
    LineWidth2_2 : integer;
begin
     assert((width1 and $00000001 = 1) and (height2 and $00000001 = 0), 'Error width1 is not odd or height2 is not even');

     iters1 := height2 div 2;
     iters2 := -(width1 div 2)*2*sizeof(double);
     LineWidth2_2 := 2*LineWidth2;
     asm
        push ebx;
        push eax;
        push ecx;
        push edx;
        push esi;
        push edi;

        // prepare matrix pointers - remove constant offset here instead each time in the loop:
        mov ecx, mt1;
        mov edi, mt2;
        sub ecx, iters2;
        sub edi, iters2;
        mov mt1, ecx;
        mov mt2, edi;

        xorpd xmm2, xmm2;
        xorpd xmm6, xmm6;

        // for y := 0 to height1 - 1:
        mov eax, Height1;
        mov y, eax;

        @@forylabel:
            mov ecx, mt1;    // prepare ecx for the "reversed" for loop
            mov esi, dest;
            mov edi, mt2;

            // for x := 0 to height2 - 1:
            mov ebx, iters1;
            @@fory2label:
                mov eax, edi;
                add eax, Linewidth2;

                xorpd xmm7, xmm7;   // dest + 1 := 0;
                movupd xmm0, [esi];
                movhlps xmm0, xmm7;
                pslldq xmm0, 8;   // shift away to clear half of the register

                // for idx := 0 to width1 div 2 do
                mov edx, iters2;

                @@InnerLoop:
                    movupd xmm1, [ecx + edx];
                    movapd xmm5, xmm1;

                    // load 2x2 block
                    movupd xmm3, [edi + edx];
                    movupd xmm4, [eax + edx];

                    // multiply 2x2 and add
                    mulpd xmm3, xmm1;
                    mulpd xmm4, xmm5;

                    addpd xmm0, xmm3;
                    addpd xmm7, xmm4;

                    // end for idx := 0 to width1 div 2
                add edx, 16;
                jnz @@InnerLoop;

                // multiply and add the last element
                movlpd xmm1, [ecx];
                movapd xmm5, xmm1;

                movlpd xmm3, [edi];
                movlpd xmm4, [eax];

                mulsd xmm3, xmm1;
                mulsd xmm4, xmm5;

                haddpd xmm0, xmm2;
                haddpd xmm7, xmm6;

                addsd xmm0, xmm3;
                addsd xmm7, xmm4;

                // compact result
                movlhps xmm0, xmm7;

                // store back result
                movupd [esi], xmm0;

                // increment the pointers
                // inc(mt2), inc(dest);
                //add dword ptr [mt2], 8;
                add esi, 16;
                add edi, LineWidth2_2;
            // end for x := 0 to width2 - 1
            dec ebx;
            jnz @@fory2label;

            // dec(mt2, Width2);
            // inc(PByte(mt1), LineWidth1);
            // inc(PByte(dest), destOffset);
            //mov ebx, bytesWidth2;
            //sub dword ptr [mt2], ebx;
            mov ebx, LineWidth1;
            add dword ptr [mt1], ebx;
            mov ebx, DestLineWidth;
            add dword ptr [dest], ebx;

        // end for y := 0 to height1 - 1
        //dec eax;
        dec y;
        jnz @@forylabel

        pop edi;
        pop esi;
        pop edx;
        pop ecx;
        pop eax;
        pop ebx;
     end;
end;

procedure ASMMatrixMultAddAlignedOddW1EvenH2Transposed(dest : PDouble; const destLineWidth : integer; mt1, mt2 : PDouble; width1 : integer; height1 : integer; width2 : integer; height2 : integer; const LineWidth1, LineWidth2 : integer);
var iters1, iters2 : integer;
    y : integer;
    LineWidth2_2 : integer;
begin
     assert(((Cardinal(dest) and $0000000F) = 0) and ((Cardinal(mt1) and $0000000F) = 0) and ((Cardinal(mt2) and $0000000F) = 0), 'Error aligned operations cannot be performed');
     assert(((destLineWidth and $0000000F) = 0) and ((LineWidth1 and $0000000F) = 0) and ((LineWidth2 and $0000000F) = 0), 'Error aligned operations cannot be performed');

     assert((width1 and $00000001 = 1) and (height2 and $00000001 = 0), 'Error width1 is not odd or height2 is not even');

     iters1 := height2 div 2;
     iters2 := -(width1 div 2)*2*sizeof(double);
     LineWidth2_2 := 2*LineWidth2;
     asm
        push ebx;
        push eax;
        push ecx;
        push edx;
        push esi;
        push edi;

        // prepare matrix pointers - remove constant offset here instead each time in the loop:
        mov ecx, mt1;
        mov edi, mt2;
        sub ecx, iters2;
        sub edi, iters2;
        mov mt1, ecx;
        mov mt2, edi;

        xorpd xmm2, xmm2;
        xorpd xmm6, xmm6;

        // for y := 0 to height1 - 1:
        mov eax, Height1;
        mov y, eax;

        @@forylabel:
            mov ecx, mt1;    // prepare ecx for the "reversed" for loop
            mov esi, dest;
            mov edi, mt2;

            // for x := 0 to height2 - 1:
            mov ebx, iters1;
            @@fory2label:
                mov eax, edi;
                add eax, Linewidth2;

                xorpd xmm7, xmm7;   // dest + 1 := 0;
                movapd xmm0, [esi];
                movhlps xmm0, xmm7;
                pslldq xmm0, 8;   // shift away to clear half of the register
                
                // for idx := 0 to width1 div 2 do
                mov edx, iters2;

                @@InnerLoop:
                    movapd xmm1, [ecx + edx];
                    movapd xmm5, xmm1;

                    // load 2x2 block
                    movapd xmm3, [edi + edx];
                    movapd xmm4, [eax + edx];

                    // multiply 2x2 and add
                    mulpd xmm3, xmm1;
                    mulpd xmm4, xmm5;

                    addpd xmm0, xmm3;
                    addpd xmm7, xmm4;

                    // end for idx := 0 to width1 div 2
                add edx, 16;
                jnz @@InnerLoop;

                // multiply and add the last element
                movlpd xmm1, [ecx];
                movapd xmm5, xmm1;

                movlpd xmm3, [edi];
                movlpd xmm4, [eax];

                mulsd xmm3, xmm1;
                mulsd xmm4, xmm5;

                haddpd xmm0, xmm2;
                haddpd xmm7, xmm6;

                addsd xmm0, xmm3;
                addsd xmm7, xmm4;

                // compact result
                movlhps xmm0, xmm7;

                // store back result
                movntdq [esi], xmm0;

                // increment the pointers
                // inc(mt2), inc(dest);
                //add dword ptr [mt2], 8;
                add esi, 16;
                add edi, LineWidth2_2;
            // end for x := 0 to width2 - 1
            dec ebx;
            jnz @@fory2label;

            // dec(mt2, Width2);
            // inc(PByte(mt1), LineWidth1);
            // inc(PByte(dest), destOffset);
            //mov ebx, bytesWidth2;
            //sub dword ptr [mt2], ebx;
            mov ebx, LineWidth1;
            add dword ptr [mt1], ebx;
            mov ebx, DestLineWidth;
            add dword ptr [dest], ebx;

        // end for y := 0 to height1 - 1
        //dec eax;
        dec y;
        jnz @@forylabel

        pop edi;
        pop esi;
        pop edx;
        pop ecx;
        pop eax;
        pop ebx;
     end;
end;

procedure ASMMatrixMultAddAlignedOddW1OddH2Transposed(dest : PDouble; const destLineWidth : integer; mt1, mt2 : PDouble; width1 : integer; height1 : integer; width2 : integer; height2 : integer; const LineWidth1, LineWidth2 : integer);
var iters1, iters2 : integer;
    y : integer;
    LineWidth2_2 : integer;
begin
     assert(((Cardinal(dest) and $0000000F) = 0) and ((Cardinal(mt1) and $0000000F) = 0) and ((Cardinal(mt2) and $0000000F) = 0), 'Error aligned operations cannot be performed');
     assert(((destLineWidth and $0000000F) = 0) and ((LineWidth1 and $0000000F) = 0) and ((LineWidth2 and $0000000F) = 0), 'Error aligned operations cannot be performed');

     assert((width1 and $00000001 = 1) and (height2 and $00000001 = 1), 'Error width1 or height2 are not odd');

     iters1 := height2 div 2;
     iters2 := -(width1 div 2)*2*sizeof(double);
     LineWidth2_2 := 2*LineWidth2;
     asm
        push ebx;
        push eax;
        push ecx;
        push edx;
        push esi;
        push edi;

        // prepare matrix pointers - remove constant offset here instead each time in the loop:
        mov ecx, mt1;
        mov edi, mt2;
        sub ecx, iters2;
        sub edi, iters2;
        mov mt1, ecx;
        mov mt2, edi;

        xorpd xmm2, xmm2;
        xorpd xmm6, xmm6;

        // for y := 0 to height1 - 1:
        mov eax, Height1;
        mov y, eax;

        @@forylabel:
            mov ecx, mt1;    // prepare ecx for the "reversed" for loop
            mov esi, dest;
            mov edi, mt2;

            // for x := 0 to height2 - 1:
            mov ebx, iters1;
            @@fory2label:
                mov eax, edi;
                add eax, Linewidth2;

                xorpd xmm7, xmm7;   // dest + 1 := 0;
                movapd xmm0, [esi];
                movhlps xmm0, xmm7;
                pslldq xmm0, 8;   // shift away to clear half of the register
                
                // for idx := 0 to width1 div 2 do
                mov edx, iters2;

                @@InnerLoop:
                    movapd xmm1, [ecx + edx];
                    movapd xmm5, xmm1;

                    // load 2x2 block
                    movapd xmm3, [edi + edx];
                    movapd xmm4, [eax + edx];

                    // multiply 2x2 and add
                    mulpd xmm3, xmm1;
                    mulpd xmm4, xmm5;

                    addpd xmm0, xmm3;
                    addpd xmm7, xmm4;

                    // end for idx := 0 to width1 div 2
                add edx, 16;
                jnz @@InnerLoop;

                // multiply and add the last element
                movlpd xmm1, [ecx];
                movapd xmm5, xmm1;

                movlpd xmm3, [edi];
                movlpd xmm4, [eax];

                mulsd xmm3, xmm1;
                mulsd xmm4, xmm5;

                haddpd xmm0, xmm2;
                haddpd xmm7, xmm6;

                addsd xmm0, xmm3;
                addsd xmm7, xmm4;

                // compact result
                movlhps xmm0, xmm7;

                // store back result
                movntdq [esi], xmm0;

                // increment the pointers
                // inc(mt2), inc(dest);
                //add dword ptr [mt2], 8;
                add esi, 16;
                add edi, LineWidth2_2;
            // end for x := 0 to width2 - 1
            dec ebx;
            jnz @@fory2label;

            // take care of the last line separatly
            xorpd xmm0, xmm0;   // dest^ := 0
            movlpd xmm0, [esi];

            // for idx := 0 to width1 div 2 do
            mov edx, iters2;

            @@InnerLoop2:
                movapd xmm1, [ecx + edx];
                movapd xmm3, [edi + edx];

                // multiply 2x2 and add
                mulpd xmm3, xmm1;
                addpd xmm0, xmm3;

                // end for idx := 0 to width1 div 2
            add edx, 16;
            jnz @@InnerLoop2;

            // multiply and add the last element
            movlpd xmm1, [ecx];
            movlpd xmm3, [edi];

            mulsd xmm3, xmm1;
            haddpd xmm0, xmm2;
            addsd xmm0, xmm3;

            // store back result
            movlpd [esi], xmm0;

            // dec(mt2, Width2);
            // inc(PByte(mt1), LineWidth1);
            // inc(PByte(dest), destOffset);
            //mov ebx, bytesWidth2;
            //sub dword ptr [mt2], ebx;
            mov ebx, LineWidth1;
            add dword ptr [mt1], ebx;
            mov ebx, DestLineWidth;
            add dword ptr [dest], ebx;

        // end for y := 0 to height1 - 1
        //dec eax;
        dec y;
        jnz @@forylabel

        pop edi;
        pop esi;
        pop edx;
        pop ecx;
        pop eax;
        pop ebx;
     end;
end;

procedure ASMMatrixMultAddUnAlignedOddW1OddH2Transposed(dest : PDouble; const destLineWidth : integer; mt1, mt2 : PDouble; width1 : integer; height1 : integer; width2 : integer; height2 : integer; const LineWidth1, LineWidth2 : integer);
var iters1, iters2 : integer;
    y : integer;
    LineWidth2_2 : integer;
begin
     assert((width1 and $00000001 = 1) and (height2 and $00000001 = 1), 'Error width1 or height2 are not odd');

     iters1 := height2 div 2;
     iters2 := -(width1 div 2)*2*sizeof(double);
     LineWidth2_2 := 2*LineWidth2;
     asm
        push ebx;
        push eax;
        push ecx;
        push edx;
        push esi;
        push edi;

        // prepare matrix pointers - remove constant offset here instead each time in the loop:
        mov ecx, mt1;
        mov edi, mt2;
        sub ecx, iters2;
        sub edi, iters2;
        mov mt1, ecx;
        mov mt2, edi;

        xorpd xmm2, xmm2;
        xorpd xmm6, xmm6;

        // for y := 0 to height1 - 1:
        mov eax, Height1;
        mov y, eax;

        @@forylabel:
            mov ecx, mt1;    // prepare ecx for the "reversed" for loop
            mov esi, dest;
            mov edi, mt2;

            // for x := 0 to width2 - 1:
            mov ebx, iters1;
            @@fory2label:
                mov eax, edi;
                add eax, Linewidth2;

                xorpd xmm7, xmm7;   // dest + 1 := 0;
                movupd xmm0, [esi];
                movhlps xmm0, xmm7;
                pslldq xmm0, 8;   // shift away to clear half of the register

                // for idx := 0 to width1 div 2 do
                mov edx, iters2;

                @@InnerLoop:
                    movupd xmm1, [ecx + edx];
                    movapd xmm5, xmm1;

                    // load 2x2 block
                    movupd xmm3, [edi + edx];
                    movupd xmm4, [eax + edx];

                    // multiply 2x2 and add
                    mulpd xmm3, xmm1;
                    mulpd xmm4, xmm5;

                    addpd xmm0, xmm3;
                    addpd xmm7, xmm4;

                    // end for idx := 0 to width1 div 2
                add edx, 16;
                jnz @@InnerLoop;

                // multiply and add the last element
                movlpd xmm1, [ecx];
                movapd xmm5, xmm1;

                movlpd xmm3, [edi];
                movlpd xmm4, [eax];

                mulsd xmm3, xmm1;
                mulsd xmm4, xmm5;

                haddpd xmm0, xmm2;
                haddpd xmm7, xmm6;

                addsd xmm0, xmm3;
                addsd xmm7, xmm4;

                // compact result
                movlhps xmm0, xmm7;

                // store back result
                movupd [esi], xmm0;

                // increment the pointers
                // inc(mt2), inc(dest);
                //add dword ptr [mt2], 8;
                add esi, 16;
                add edi, LineWidth2_2;
            // end for x := 0 to width2 - 1
            dec ebx;
            jnz @@fory2label;

            // take care of the last line separatly
            xorpd xmm0, xmm0;   // dest^ := 0
            movlpd xmm0, [esi];
            
            // for idx := 0 to width1 div 2 do
            mov edx, iters2;

            @@InnerLoop2:
                movupd xmm1, [ecx + edx];
                movupd xmm3, [edi + edx];

                // multiply 2x2 and add
                mulpd xmm3, xmm1;
                addpd xmm0, xmm3;

                // end for idx := 0 to width1 div 2
            add edx, 16;
            jnz @@InnerLoop2;

            // multiply and add the last element
            movlpd xmm1, [ecx];
            movlpd xmm3, [edi];

            mulsd xmm3, xmm1;
            haddpd xmm0, xmm2;
            addsd xmm0, xmm3;

            // store back result
            movlpd [esi], xmm0;

            // dec(mt2, Width2);
            // inc(PByte(mt1), LineWidth1);
            // inc(PByte(dest), destOffset);
            //mov ebx, bytesWidth2;
            //sub dword ptr [mt2], ebx;
            mov ebx, LineWidth1;
            add dword ptr [mt1], ebx;
            mov ebx, DestLineWidth;
            add dword ptr [dest], ebx;

        // end for y := 0 to height1 - 1
        //dec eax;
        dec y;
        jnz @@forylabel

        pop edi;
        pop esi;
        pop edx;
        pop ecx;
        pop eax;
        pop ebx;
     end;
end;

{$ENDIF}

end.
