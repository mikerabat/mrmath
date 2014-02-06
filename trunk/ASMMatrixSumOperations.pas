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


unit ASMMatrixSumOperations;

interface

{$IFNDEF CPUX64}

uses MatrixConst;

procedure ASMMatrixSumRowAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
procedure ASMMatrixSumRowUnAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
procedure ASMMatrixSumRowAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
procedure ASMMatrixSumRowUnAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);

procedure ASMMatrixSumColumnAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
procedure ASMMatrixSumColumnUnAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
procedure ASMMatrixSumColumnAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
procedure ASMMatrixSumColumnUnAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);

{$ENDIF}

implementation

{$IFNDEF CPUX64}

procedure ASMMatrixSumRowAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
var iters : TASMNativeInt;
begin
     Assert((Cardinal(Src) and $0000000F = 0) and ((srcLineWidth and $0000000F) = 0) and
            (Cardinal(dest) and $0000000F = 0) and ((destLineWidth and $0000000F) = 0), 'Error non aligned data');
     Assert((width and 1) = 0, 'Error width must be even');

     assert((width > 0) and (height > 0) and (srcLineWidth >= width*sizeof(double)) and (destLineWidth >= sizeof(double)), 'Dimension error');

     iters := -width*sizeof(double);

     asm
        push ebx;
        // helper registers for the mt1, mt2 and dest pointers
        mov ecx, src;
        sub ecx, iters;

        mov ebx, dest;

        // for y := 0 to height - 1:
        mov edx, Height;
        @@addforyloop:
            xorpd xmm0, xmm0;
            xorpd xmm1, xmm1;
            xorpd xmm2, xmm2;
            xorpd xmm3, xmm3;

            // for x := 0 to w - 1;
            // prepare for reverse loop
            mov eax, iters;
            @addforxloop:
                add eax, 128;
                jg @loopEnd;

                // prefetch data...
                // prefetch [ecx + eax];

                // addition:
                addpd xmm0, [ecx + eax - 128];
                addpd xmm1, [ecx + eax - 112];
                addpd xmm2, [ecx + eax - 96];
                addpd xmm3, [ecx + eax - 80];
                addpd xmm0, [ecx + eax - 64];
                addpd xmm1, [ecx + eax - 48];
                addpd xmm2, [ecx + eax - 32];
                addpd xmm3, [ecx + eax - 16];
            jmp @addforxloop

            @loopEnd:

            sub eax, 128;

            jz @buildRes;

            @addforxloop2:
                addpd xmm0, [ecx + eax];
            add eax, 16;
            jnz @addforxloop2;

            @buildRes:

            // build result
            addpd xmm0, xmm1;
            addpd xmm2, xmm3;
            addpd xmm0, xmm2;

            movhlps xmm1, xmm0;
            addsd xmm0, xmm1;

            // write result
            movlpd [ebx], xmm0;

            // next line:
            add ecx, srcLineWidth;
            add ebx, destLineWidth;

        // loop y end
        dec edx;
        jnz @@addforyloop;

        pop ebx;
     end;
end;

procedure ASMMatrixSumRowUnAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
var iters : TASMNativeInt;
begin
     Assert((width and 1) = 0, 'Error width must be even');

     assert((width > 0) and (height > 0) and (srcLineWidth >= width*sizeof(double)) and (destLineWidth >= sizeof(double)), 'Dimension error');

     iters := -width*sizeof(double);

     asm
        push ebx;
        // helper registers for the mt1, mt2 and dest pointers
        mov ecx, src;
        sub ecx, iters;

        mov ebx, dest;

        // for y := 0 to height - 1:
        mov edx, Height;
        @@addforyloop:
            xorpd xmm7, xmm7;

            // for x := 0 to w - 1;
            // prepare for reverse loop
            mov eax, iters;
            @addforxloop:
                add eax, 128;
                jg @loopEnd;

                // addition:
                movupd xmm0, [ecx + eax - 128];
                addpd xmm7, xmm0;

                movupd xmm1, [ecx + eax - 112];
                addpd xmm7, xmm1;

                movupd xmm2, [ecx + eax - 96];
                addpd xmm7, xmm2;

                movupd xmm3, [ecx + eax - 80];
                addpd xmm7, xmm3;

                movupd xmm0, [ecx + eax - 64];
                addpd xmm7, xmm0;

                movupd xmm1, [ecx + eax - 48];
                addpd xmm7, xmm1;

                movupd xmm2, [ecx + eax - 32];
                addpd xmm7, xmm2;

                movupd xmm3, [ecx + eax - 16];
                addpd xmm7, xmm3;
            jmp @addforxloop

            @loopEnd:

            sub eax, 128;

            jz @buildRes;

            @addforxloop2:
                movupd xmm0, [ecx + eax];
                addpd xmm7, xmm0;
            add eax, 16;
            jnz @addforxloop2;

            @buildRes:

            // build result
            movhlps xmm1, xmm7;
            addsd xmm7, xmm1;

            movlpd [ebx], xmm7;

            // next line:
            add ecx, srcLineWidth;
            add ebx, destLineWidth;

        // loop y end
        dec edx;
        jnz @@addforyloop;

        pop ebx;
     end;
end;

procedure ASMMatrixSumRowAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
var iters : TASMNativeInt;
begin
     Assert((Cardinal(Src) and $0000000F = 0) and ((srcLineWidth and $0000000F) = 0) and
            (Cardinal(dest) and $0000000F = 0) and ((destLineWidth and $0000000F) = 0), 'Error non aligned data');
     Assert((width and 1) = 1, 'Error width must be odd');

     assert((width > 0) and (height > 0) and (srcLineWidth >= width*sizeof(double)) and (destLineWidth >= sizeof(double)), 'Dimension error');

     iters := -(width - 1)*sizeof(double);

     asm
        push ebx;
        // helper registers for the mt1, mt2 and dest pointers
        mov ecx, src;
        sub ecx, iters;

        mov ebx, dest;

        // for y := 0 to height - 1:
        mov edx, Height;
        @@addforyloop:
            xorpd xmm0, xmm0;
            xorpd xmm1, xmm1;
            xorpd xmm2, xmm2;
            xorpd xmm3, xmm3;

            // for x := 0 to w - 1;
            // prepare for reverse loop
            mov eax, iters;
            @addforxloop:
                add eax, 128;
                jg @loopEnd;

                // prefetch data...
                // prefetch [ecx + eax];

                // addition:
                addpd xmm0, [ecx + eax - 128];
                addpd xmm1, [ecx + eax - 112];
                addpd xmm2, [ecx + eax - 96];
                addpd xmm3, [ecx + eax - 80];
                addpd xmm0, [ecx + eax - 64];
                addpd xmm1, [ecx + eax - 48];
                addpd xmm2, [ecx + eax - 32];
                addpd xmm3, [ecx + eax - 16];
            jmp @addforxloop

            @loopEnd:

            sub eax, 128;

            jz @buildRes;

            @addforxloop2:
                movapd xmm0, [ecx + eax];
                addpd xmm7, xmm0;
            add eax, 16;
            jnz @addforxloop2;

            @buildRes:

            // handle last element differently
            movlpd xmm2, [ecx + eax];
            addsd xmm7, xmm2;

            // build result
            addpd xmm0, xmm1;
            addpd xmm2, xmm3;
            addpd xmm0, xmm2;

            movhlps xmm1, xmm0;
            addsd xmm0, xmm1;

            // write result
            movlpd [ebx], xmm0;

            // next line:
            add ecx, srcLineWidth;
            add ebx, destLineWidth;

        // loop y end
        dec edx;
        jnz @@addforyloop;

        pop ebx;
     end;
end;

procedure ASMMatrixSumRowUnAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
var iters : TASMNativeInt;
const cOne : double = 1;
begin
     Assert((width and 1) = 1, 'Error width must be odd');

     assert((width > 0) and (height > 0) and (srcLineWidth >= width*sizeof(double)) and (destLineWidth >= sizeof(double)), 'Dimension error');

     iters := -(width - 1)*sizeof(double);

     asm
        push ebx;
        // helper registers for the mt1, mt2 and dest pointers
        mov ecx, src;
        sub ecx, iters;

        mov ebx, dest;

        // for y := 0 to height - 1:
        mov edx, Height;
        @@addforyloop:
            xorpd xmm7, xmm7;

            // for x := 0 to w - 1;
            // prepare for reverse loop
            mov eax, iters;
            @addforxloop:
                add eax, 128;
                jg @loopEnd;

                // addition:
                movupd xmm0, [ecx + eax - 128];
                addpd xmm7, xmm0;

                movupd xmm1, [ecx + eax - 112];
                addpd xmm7, xmm1;

                movupd xmm2, [ecx + eax - 96];
                addpd xmm7, xmm2;

                movupd xmm3, [ecx + eax - 80];
                addpd xmm7, xmm3;

                movupd xmm0, [ecx + eax - 64];
                addpd xmm7, xmm0;

                movupd xmm1, [ecx + eax - 48];
                addpd xmm7, xmm1;

                movupd xmm2, [ecx + eax - 32];
                addpd xmm7, xmm2;

                movupd xmm3, [ecx + eax - 16];
                addpd xmm7, xmm3;
            jmp @addforxloop

            @loopEnd:

            sub eax, 128;

            jz @buildRes;

            @addforxloop2:
                movupd xmm0, [ecx + eax];
                addpd xmm7, xmm0;
            add eax, 16;
            jnz @addforxloop2;

            @buildRes:

            // handle last element differently
            movlpd xmm2, [ecx + eax];
            addsd xmm7, xmm2;

            // build result
            movhlps xmm1, xmm7;
            addsd xmm7, xmm1;

            movlpd [ebx], xmm7;

            // next line:
            add ecx, srcLineWidth;
            add ebx, destLineWidth;

        // loop y end
        dec edx;
        jnz @@addforyloop;

        pop ebx;
     end;
end;

procedure ASMMatrixSumColumnAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
var iters : TASMNativeInt;
begin
     Assert((Cardinal(Src) and $0000000F = 0) and ((srcLineWidth and $0000000F) = 0) and
            (Cardinal(dest) and $0000000F = 0) and ((destLineWidth and $0000000F) = 0), 'Error non aligned data');
     Assert((width and 1) = 0, 'Error width must be even');

     assert((width > 1) and (height > 0) and (srcLineWidth >= width*sizeof(double)) and (destLineWidth >= width*sizeof(double)), 'Dimension error');

     iters := -height*srcLineWidth;

     asm
        push ebx;

        // helper registers for the mt1, mt2 and dest pointers
        mov ecx, src;
        sub ecx, iters;
        mov ebx, dest;

        // for x := 0 to width - 1:
        mov edx, Width;
        sar edx, 1;
        @@addforxloop:
            xorpd xmm0, xmm0;

            // for y := 0 to height - 1;
            // prepare for reverse loop
            mov eax, iters;
            @addforyloop:
                addpd xmm0, [ecx + eax];
            add eax, srcLineWidth;
            jnz @addforyloop;

            // build result
            movapd [ebx], xmm0;

            // next columns:
            add ecx, 16;
            add ebx, 16;

        // loop x end
        dec edx;
        jnz @@addforxloop;

        pop ebx;
     end;
end;

procedure ASMMatrixSumColumnUnAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
var iters : TASMNativeInt;
begin
     Assert((width and 1) = 0, 'Error width must be even');

     assert((width > 1) and (height > 0) and (srcLineWidth >= width*sizeof(double)) and (destLineWidth >= width*sizeof(double)), 'Dimension error');

     iters := -height*srcLineWidth;

     asm
        push ebx;

        // helper registers for the mt1, mt2 and dest pointers
        mov ecx, src;
        sub ecx, iters;
        mov ebx, dest;

        // for x := 0 to width - 1:
        mov edx, Width;
        sar edx, 1;
        @@addforxloop:
            xorpd xmm7, xmm7;

            // for y := 0 to height - 1;
            // prepare for reverse loop
            mov eax, iters;
            @addforyloop:
                movupd xmm0, [ecx + eax];
                addpd xmm7, xmm0;
            add eax, srcLineWidth;
            jnz @addforyloop;

            // build result
            movupd [ebx], xmm7;

            // next columns:
            add ecx, 16;
            add ebx, 16;

        // loop x end
        dec edx;
        jnz @@addforxloop;

        pop ebx;
     end;
end;

procedure ASMMatrixSumColumnAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
var iters : TASMNativeInt;
begin
     Assert((Cardinal(Src) and $0000000F = 0) and ((srcLineWidth and $0000000F) = 0) and
            (Cardinal(dest) and $0000000F = 0) and ((destLineWidth and $0000000F) = 0), 'Error non aligned data');
     Assert((width and 1) = 1, 'Error width must be even');

     assert((width > 1) and (height > 0) and (srcLineWidth >= width*sizeof(double)) and (destLineWidth >= width*sizeof(double)), 'Dimension error');

     iters := -height*srcLineWidth;

     asm
        push ebx;

        // helper registers for the mt1, mt2 and dest pointers
        mov ecx, src;
        sub ecx, iters;
        mov ebx, dest;

        // for x := 0 to width - 1:
        mov edx, Width;
        sar edx, 1;
        jz @lastColumn;
        @@addforxloop:
            xorpd xmm0, xmm0;

            // for y := 0 to height - 1;
            // prepare for reverse loop
            mov eax, iters;
            @addforyloop:
                addpd xmm0, [ecx + eax];
            add eax, srcLineWidth;
            jnz @addforyloop;

            // build result
            movapd [ebx], xmm0;

            // next columns:
            add ecx, 16;
            add ebx, 16;

        // loop x end
        dec edx;
        jnz @@addforxloop;

        @lastColumn:
        // handle last column
        xorpd xmm7, xmm7;

        // for y := 0 to height - 1;
        // prepare for reverse loop
        mov eax, iters;
        @addforyloop3:
            movlpd xmm0, [ecx + eax];
            addsd xmm7, xmm0;
        add eax, srcLineWidth;
        jnz @addforyloop3;

        // build result
        movlpd [ebx], xmm7;

        pop ebx;
     end;
end;

procedure ASMMatrixSumColumnUnAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
var iters : TASMNativeInt;
begin
     Assert((width and 1) = 1, 'Error width must be even');

     assert((width > 1) and (height > 0) and (srcLineWidth >= width*sizeof(double)) and (destLineWidth >= width*sizeof(double)), 'Dimension error');

     iters := -height*srcLineWidth;

     asm
        push ebx;

        // helper registers for the mt1, mt2 and dest pointers
        mov ecx, src;
        sub ecx, iters;
        mov ebx, dest;

        // for x := 0 to width - 1:
        mov edx, Width;
        sar edx, 1;
        jz @lastColumn;
        @@addforxloop:
            xorpd xmm7, xmm7;

            // for y := 0 to height - 1;
            // prepare for reverse loop
            mov eax, iters;
            @addforyloop:
                movupd xmm0, [ecx + eax];
                addpd xmm7, xmm0;
            add eax, srcLineWidth;
            jnz @addforyloop;

            // build result
            movupd [ebx], xmm7;

            // next columns:
            add ecx, 16;
            add ebx, 16;

        // loop x end
        dec edx;
        jnz @@addforxloop;

        @lastColumn:
        // handle last column
        xorpd xmm7, xmm7;

        // for y := 0 to height - 1;
        // prepare for reverse loop
        mov eax, iters;
        @addforyloop3:
            movlpd xmm0, [ecx + eax];
            addsd xmm7, xmm0;
        add eax, srcLineWidth;
        jnz @addforyloop3;

        // build result
        movlpd [ebx], xmm7;

        pop ebx;
     end;
end;

{$ENDIF}

end.
