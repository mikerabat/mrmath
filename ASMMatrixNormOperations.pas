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


unit ASMMatrixNormOperations;

interface

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}
{$IFNDEF x64}

uses MatrixConst;

function ASMMatrixElementwiseNorm2AlignedEvenW(dest : PDouble; const LineWidth : TASMNativeInt; Width, height : TASMNativeInt) : double;
function ASMMatrixElementwiseNorm2UnAlignedEvenW(dest : PDouble; const LineWidth : TASMNativeInt; Width, height : TASMNativeInt) : double;

function ASMMatrixElementwiseNorm2AlignedOddW(dest : PDouble; const LineWidth : TASMNativeInt; Width, height : TASMNativeInt) : double;
function ASMMatrixElementwiseNorm2UnAlignedOddW(dest : PDouble; const LineWidth : TASMNativeInt; Width, height : TASMNativeInt) : double;


procedure ASMMatrixNormalizeRowAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
procedure ASMMatrixNormalizeRowUnAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
procedure ASMMatrixNormalizeRowAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
procedure ASMMatrixNormalizeRowUnAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);

procedure ASMMatrixNormalizeColumnAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
procedure ASMMatrixNormalizeColumnUnAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
procedure ASMMatrixNormalizeColumnAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
procedure ASMMatrixNormalizeColumnUnAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);

{$ENDIF}

implementation

{$IFNDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$S-} {$ENDIF}

function ASMMatrixElementwiseNorm2AlignedEvenW(dest : PDouble; const LineWidth : TASMNativeInt; Width, height : TASMNativeInt) : double;
var iters : TASMNativeInt;
begin
     Assert((Cardinal(dest) and $0000000F = 0) and ((LineWidth and $0000000F) = 0), 'Error non aligned data');
     Assert((width and 1) = 0, 'Error width must be even');

     assert((width > 0) and (height > 0) and (LineWidth >= width*sizeof(double)), 'Dimension error');

     iters := -width*sizeof(double);

     asm
        // helper registers for the mt1, mt2 and dest pointers
        mov ecx, dest;
        sub ecx, iters;

        xorpd xmm7, xmm7;

        // for y := 0 to height - 1:
        mov edx, Height;
        @@addforyloop:
            // for x := 0 to w - 1;
            // prepare for reverse loop
            mov eax, iters;
            @addforxloop:
                add eax, 128;
                jg @loopEnd;

                // prefetch data...
                // prefetch [ecx + eax];

                // mul add:
                movapd xmm0, [ecx + eax - 128];
                mulpd xmm0, xmm0;
                addpd xmm7, xmm0;

                movapd xmm1, [ecx + eax - 112];
                mulpd xmm1, xmm1;
                addpd xmm7, xmm1;

                movapd xmm2, [ecx + eax - 96];
                mulpd xmm2, xmm2;
                addpd xmm7, xmm2;

                movapd xmm3, [ecx + eax - 80];
                mulpd xmm3, xmm3;
                addpd xmm7, xmm3;

                movapd xmm0, [ecx + eax - 64];
                mulpd xmm0, xmm0;
                addpd xmm7, xmm0;

                movapd xmm1, [ecx + eax - 48];
                mulpd xmm1, xmm1;
                addpd xmm7, xmm1;

                movapd xmm2, [ecx + eax - 32];
                mulpd xmm2, xmm2;
                addpd xmm7, xmm2;

                movapd xmm3, [ecx + eax - 16];
                mulpd xmm3, xmm3;
                addpd xmm7, xmm3;
            jmp @addforxloop

            @loopEnd:

            sub eax, 128;

            jz @nextLine;

            // prefetchw [ecx + eax + 128];

            @addforxloop2:
                movapd xmm0, [ecx + eax];
                mulpd xmm0, xmm0;
                addpd xmm7, xmm0;
            add eax, 16;
            jnz @addforxloop2;

            @nextLine:

            // next line:
            add ecx, LineWidth;

        // loop y end
        dec edx;
        jnz @@addforyloop;

        // build result
        movhlps xmm1, xmm7;
        addsd xmm7, xmm1;
        movsd Result, xmm7;
     end;
end;

function ASMMatrixElementwiseNorm2UnAlignedEvenW(dest : PDouble; const LineWidth : TASMNativeInt; Width, height : TASMNativeInt) : double;
var iters : TASMNativeInt;
begin
     Assert((width and 1) = 0, 'Error width must be even');

     assert((width > 0) and (height > 0) and (LineWidth >= width*sizeof(double)), 'Dimension error');

     iters := -width*sizeof(double);

     asm
        // helper registers for the mt1, mt2 and dest pointers
        mov ecx, dest;
        sub ecx, iters;

        xorpd xmm7, xmm7;

        // for y := 0 to height - 1:
        mov edx, Height;
        @@addforyloop:
            // for x := 0 to w - 1;
            // prepare for reverse loop
            mov eax, iters;
            @addforxloop:
                add eax, 128;
                jg @loopEnd;

                // mul add:
                movupd xmm0, [ecx + eax - 128];
                mulpd xmm0, xmm0;
                addpd xmm7, xmm0;

                movupd xmm1, [ecx + eax - 112];
                mulpd xmm1, xmm1;
                addpd xmm7, xmm1;

                movupd xmm2, [ecx + eax - 96];
                mulpd xmm2, xmm2;
                addpd xmm7, xmm2;

                movupd xmm3, [ecx + eax - 80];
                mulpd xmm3, xmm3;
                addpd xmm7, xmm3;

                movupd xmm0, [ecx + eax - 64];
                mulpd xmm0, xmm0;
                addpd xmm7, xmm0;

                movupd xmm1, [ecx + eax - 48];
                mulpd xmm1, xmm1;
                addpd xmm7, xmm1;

                movupd xmm2, [ecx + eax - 32];
                mulpd xmm2, xmm2;
                addpd xmm7, xmm2;

                movupd xmm3, [ecx + eax - 16];
                mulpd xmm3, xmm3;
                addpd xmm7, xmm3;
            jmp @addforxloop

            @loopEnd:

            sub eax, 128;

            jz @nextLine;

            @addforxloop2:
                movupd xmm0, [ecx + eax];
                mulpd xmm0, xmm0;
                addpd xmm7, xmm0;
            add eax, 16;
            jnz @addforxloop2;

            @nextLine:

            // next line:
            add ecx, LineWidth;

        // loop y end
        dec edx;
        jnz @@addforyloop;

        // build result
        movhlps xmm1, xmm7;
        addsd xmm7, xmm1;
        movsd Result, xmm7;
     end;
end;

function ASMMatrixElementwiseNorm2AlignedOddW(dest : PDouble; const LineWidth : TASMNativeInt; Width, height : TASMNativeInt) : double;
var iters : TASMNativeInt;
begin
     Assert((Cardinal(dest) and $0000000F = 0) and ((LineWidth and $0000000F) = 0), 'Error non aligned data');
     Assert((width and 1) = 1, 'Error width must be even');

     assert((width > 0) and (height > 0) and (LineWidth >= width*sizeof(double)), 'Dimension error');

     iters := -(width - 1)*sizeof(double);

     asm
        // helper registers for the mt1, mt2 and dest pointers
        mov ecx, dest;
        sub ecx, iters;

        xorpd xmm7, xmm7;

        // for y := 0 to height - 1:
        mov edx, Height;
        @@addforyloop:
            // for x := 0 to w - 1;
            // prepare for reverse loop
            mov eax, iters;
            @addforxloop:
                add eax, 128;
                jg @loopEnd;

                // prefetch data...
                // prefetch [ecx + eax];

                // mul add:
                movapd xmm0, [ecx + eax - 128];
                mulpd xmm0, xmm0;
                addpd xmm7, xmm0;

                movapd xmm1, [ecx + eax - 112];
                mulpd xmm1, xmm1;
                addpd xmm7, xmm1;

                movapd xmm2, [ecx + eax - 96];
                mulpd xmm2, xmm2;
                addpd xmm7, xmm2;

                movapd xmm3, [ecx + eax - 80];
                mulpd xmm3, xmm3;
                addpd xmm7, xmm3;

                movapd xmm0, [ecx + eax - 64];
                mulpd xmm0, xmm0;
                addpd xmm7, xmm0;

                movapd xmm1, [ecx + eax - 48];
                mulpd xmm1, xmm1;
                addpd xmm7, xmm1;

                movapd xmm2, [ecx + eax - 32];
                mulpd xmm2, xmm2;
                addpd xmm7, xmm2;

                movapd xmm3, [ecx + eax - 16];
                mulpd xmm3, xmm3;
                addpd xmm7, xmm3;
            jmp @addforxloop

            @loopEnd:

            sub eax, 128;

            jz @nextLine;

            @addforxloop2:
                movapd xmm0, [ecx + eax];
                mulpd xmm0, xmm0;
                addpd xmm7, xmm0;
            add eax, 16;
            jnz @addforxloop2;

            @nextLine:

            // handle last element differently
            movsd xmm0, [ecx + eax];
            mulsd xmm0, xmm0;
            addsd xmm7, xmm0;

            // next line:
            add ecx, LineWidth;

        // loop y end
        dec edx;
        jnz @@addforyloop;

        // build result
        movhlps xmm1, xmm7;
        addsd xmm7, xmm1;
        movsd Result, xmm7;
     end;
end;

function ASMMatrixElementwiseNorm2UnAlignedOddW(dest : PDouble; const LineWidth : TASMNativeInt; Width, height : TASMNativeInt) : double;
var iters : TASMNativeInt;
begin
     Assert((width and 1) = 1, 'Error width must be odd');

     assert((width > 0) and (height > 0) and (LineWidth >= width*sizeof(double)), 'Dimension error');

     iters := -(width - 1)*sizeof(double);

     asm
        // helper registers for the mt1, mt2 and dest pointers
        mov ecx, dest;
        sub ecx, iters;

        xorpd xmm7, xmm7;

        // for y := 0 to height - 1:
        mov edx, Height;
        @@addforyloop:
            // for x := 0 to w - 1;
            // prepare for reverse loop
            mov eax, iters;
            @addforxloop:
                add eax, 128;
                jg @loopEnd;

                // mul add:
                movupd xmm0, [ecx + eax - 128];
                mulpd xmm0, xmm0;
                addpd xmm7, xmm0;

                movupd xmm1, [ecx + eax - 112];
                mulpd xmm1, xmm1;
                addpd xmm7, xmm1;

                movupd xmm2, [ecx + eax - 96];
                mulpd xmm2, xmm2;
                addpd xmm7, xmm2;

                movupd xmm3, [ecx + eax - 80];
                mulpd xmm3, xmm3;
                addpd xmm7, xmm3;

                movupd xmm0, [ecx + eax - 64];
                mulpd xmm0, xmm0;
                addpd xmm7, xmm0;

                movupd xmm1, [ecx + eax - 48];
                mulpd xmm1, xmm1;
                addpd xmm7, xmm1;

                movupd xmm2, [ecx + eax - 32];
                mulpd xmm2, xmm2;
                addpd xmm7, xmm2;

                movupd xmm3, [ecx + eax - 16];
                mulpd xmm3, xmm3;
                addpd xmm7, xmm3;
            jmp @addforxloop

            @loopEnd:

            sub eax, 128;

            jz @nextLine;

            @addforxloop2:
                movupd xmm0, [ecx + eax];
                mulpd xmm0, xmm0;
                addpd xmm7, xmm0;
            add eax, 16;
            jnz @addforxloop2;

            @nextLine:

            // handle last element differently
            movsd xmm0, [ecx + eax];
            mulsd xmm0, xmm0;
            addsd xmm7, xmm0;

            // next line:
            add ecx, LineWidth;

        // loop y end
        dec edx;
        jnz @@addforyloop;

        // build result
        movhlps xmm1, xmm7;
        addsd xmm7, xmm1;
        movsd Result, xmm7;
     end;
end;

procedure ASMMatrixNormalizeRowAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
var iters : TASMNativeInt;
begin
     Assert((Cardinal(Src) and $0000000F = 0) and ((srcLineWidth and $0000000F) = 0) and
            (Cardinal(dest) and $0000000F = 0) and ((destLineWidth and $0000000F) = 0), 'Error non aligned data');
     Assert((width and 1) = 0, 'Error width must be even');

     assert((width > 0) and (height > 0) and (srcLineWidth >= width*sizeof(double)) and (destLineWidth >= width*sizeof(double)), 'Dimension error');

     iters := -width*sizeof(double);

     asm
        push ebx;
        // helper registers for the mt1, mt2 and dest pointers
        mov ecx, src;
        sub ecx, iters;

        mov ebx, dest;
        sub ebx, iters;

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

                // prefetch data...
                // prefetch [ecx + eax];

                // mul add:
                movapd xmm0, [ecx + eax - 128];
                mulpd xmm0, xmm0;
                addpd xmm7, xmm0;

                movapd xmm1, [ecx + eax - 112];
                mulpd xmm1, xmm1;
                addpd xmm7, xmm1;

                movapd xmm2, [ecx + eax - 96];
                mulpd xmm2, xmm2;
                addpd xmm7, xmm2;

                movapd xmm3, [ecx + eax - 80];
                mulpd xmm3, xmm3;
                addpd xmm7, xmm3;

                movapd xmm0, [ecx + eax - 64];
                mulpd xmm0, xmm0;
                addpd xmm7, xmm0;

                movapd xmm1, [ecx + eax - 48];
                mulpd xmm1, xmm1;
                addpd xmm7, xmm1;

                movapd xmm2, [ecx + eax - 32];
                mulpd xmm2, xmm2;
                addpd xmm7, xmm2;

                movapd xmm3, [ecx + eax - 16];
                mulpd xmm3, xmm3;
                addpd xmm7, xmm3;
            jmp @addforxloop

            @loopEnd:

            sub eax, 128;

            jz @buildRes;

            @addforxloop2:
                movapd xmm0, [ecx + eax];
                mulpd xmm0, xmm0;
                addpd xmm7, xmm0;
            add eax, 16;
            jnz @addforxloop2;

            @buildRes:

            // build result
            movhlps xmm1, xmm7;
            addsd xmm7, xmm1;
            sqrtsd xmm7, xmm7;
            movsd xmm0, cOne;
            divsd xmm0, xmm7;

            movddup xmm6, xmm0;

            // multiply the result and build the result
            // for x := 0 to w - 1;
            // prepare for reverse loop
            mov eax, iters;
            @addforxloop3:
                add eax, 128;
                jg @loopEnd2;

                // prefetch data...
                // prefetch [ecx + eax];
                // prefetchw [ebx + eax];

                // mul add:
                movapd xmm0, [ecx + eax - 128];
                mulpd xmm0, xmm6;
                movapd [ebx + eax - 128], xmm0;

                movapd xmm1, [ecx + eax - 112];
                mulpd xmm1, xmm6;
                movapd [ebx + eax - 112], xmm1;

                movapd xmm2, [ecx + eax - 96];
                mulpd xmm2, xmm6;
                movapd [ebx + eax - 96], xmm2;

                movapd xmm3, [ecx + eax - 80];
                mulpd xmm3, xmm6;
                movapd [ebx + eax - 80], xmm3;

                movapd xmm0, [ecx + eax - 64];
                mulpd xmm0, xmm6;
                movapd [ebx + eax - 64], xmm0;

                movapd xmm1, [ecx + eax - 48];
                mulpd xmm1, xmm6;
                movapd [ebx + eax - 48], xmm1;

                movapd xmm2, [ecx + eax - 32];
                mulpd xmm2, xmm6;
                movapd [ebx + eax - 32], xmm2;

                movapd xmm3, [ecx + eax - 16];
                mulpd xmm3, xmm6;
                movapd [ebx + eax - 16], xmm3;
            jmp @addforxloop3

            @loopEnd2:

            sub eax, 128;

            jz @nextLine;

            @addforxloop4:
                movapd xmm0, [ecx + eax];
                mulpd xmm0, xmm6;
                movapd [ebx + eax], xmm0;
            add eax, 16;
            jnz @addforxloop4;

            @nextLine:

            // next line:
            add ecx, srcLineWidth;
            add ebx, destLineWidth;

        // loop y end
        dec edx;
        jnz @@addforyloop;

        pop ebx;
     end;
end;

procedure ASMMatrixNormalizeRowUnAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
var iters : TASMNativeInt;
begin
     Assert((width and 1) = 0, 'Error width must be even');

     assert((width > 0) and (height > 0) and (srcLineWidth >= width*sizeof(double)) and (destLineWidth >= width*sizeof(double)), 'Dimension error');

     iters := -width*sizeof(double);

     asm
        push ebx;
        // helper registers for the mt1, mt2 and dest pointers
        mov ecx, src;
        sub ecx, iters;

        mov ebx, dest;
        sub ebx, iters;

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

                // mul add:
                movupd xmm0, [ecx + eax - 128];
                mulpd xmm0, xmm0;
                addpd xmm7, xmm0;

                movupd xmm1, [ecx + eax - 112];
                mulpd xmm1, xmm1;
                addpd xmm7, xmm1;

                movupd xmm2, [ecx + eax - 96];
                mulpd xmm2, xmm2;
                addpd xmm7, xmm2;

                movupd xmm3, [ecx + eax - 80];
                mulpd xmm3, xmm3;
                addpd xmm7, xmm3;

                movupd xmm0, [ecx + eax - 64];
                mulpd xmm0, xmm0;
                addpd xmm7, xmm0;

                movupd xmm1, [ecx + eax - 48];
                mulpd xmm1, xmm1;
                addpd xmm7, xmm1;

                movupd xmm2, [ecx + eax - 32];
                mulpd xmm2, xmm2;
                addpd xmm7, xmm2;

                movupd xmm3, [ecx + eax - 16];
                mulpd xmm3, xmm3;
                addpd xmm7, xmm3;
            jmp @addforxloop

            @loopEnd:

            sub eax, 128;

            jz @buildRes;

            @addforxloop2:
                movupd xmm0, [ecx + eax];
                mulpd xmm0, xmm0;
                addpd xmm7, xmm0;
            add eax, 16;
            jnz @addforxloop2;

            @buildRes:

            // build result
            movhlps xmm1, xmm7;
            addsd xmm7, xmm1;
            sqrtsd xmm7, xmm7;
            movsd xmm0, cOne;
            divsd xmm0, xmm7;

            movddup xmm6, xmm0;

            // multiply the result and build the result
            // for x := 0 to w - 1;
            // prepare for reverse loop
            mov eax, iters;
            @addforxloop3:
                add eax, 128;
                jg @loopEnd2;

                // mult:
                movupd xmm0, [ecx + eax - 128];
                mulpd xmm0, xmm6;
                movupd [ebx + eax - 128], xmm0;

                movupd xmm1, [ecx + eax - 112];
                mulpd xmm1, xmm6;
                movupd [ebx + eax - 112], xmm1;

                movupd xmm2, [ecx + eax - 96];
                mulpd xmm2, xmm6;
                movupd [ebx + eax - 96], xmm2;

                movupd xmm3, [ecx + eax - 80];
                mulpd xmm3, xmm6;
                movupd [ebx + eax - 80], xmm3;

                movupd xmm0, [ecx + eax - 64];
                mulpd xmm0, xmm6;
                movupd [ebx + eax - 64], xmm0;

                movupd xmm1, [ecx + eax - 48];
                mulpd xmm1, xmm6;
                movupd [ebx + eax - 48], xmm1;

                movupd xmm2, [ecx + eax - 32];
                mulpd xmm2, xmm6;
                movupd [ebx + eax - 32], xmm2;

                movupd xmm3, [ecx + eax - 16];
                mulpd xmm3, xmm6;
                movupd [ebx + eax - 16], xmm3;
            jmp @addforxloop3

            @loopEnd2:

            sub eax, 128;

            jz @nextLine;

            @addforxloop4:
                movupd xmm0, [ecx + eax];
                mulpd xmm0, xmm6;
                movupd [ebx + eax], xmm0;
            add eax, 16;
            jnz @addforxloop4;

            @nextLine:

            // next line:
            add ecx, srcLineWidth;
            add ebx, destLineWidth;

        // loop y end
        dec edx;
        jnz @@addforyloop;

        pop ebx;
     end;
end;

procedure ASMMatrixNormalizeRowAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
var iters : TASMNativeInt;
begin
     Assert((Cardinal(Src) and $0000000F = 0) and ((srcLineWidth and $0000000F) = 0) and
            (Cardinal(dest) and $0000000F = 0) and ((destLineWidth and $0000000F) = 0), 'Error non aligned data');
     Assert((width and 1) = 1, 'Error width must be odd');

     assert((width > 0) and (height > 0) and (srcLineWidth >= width*sizeof(double)) and (destLineWidth >= width*sizeof(double)), 'Dimension error');

     iters := -(width - 1)*sizeof(double);

     asm
        push ebx;
        // helper registers for the mt1, mt2 and dest pointers
        mov ecx, src;
        sub ecx, iters;

        mov ebx, dest;
        sub ebx, iters;

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

                // prefetch data...
                // prefetch [ecx + eax];

                // addition:
                movapd xmm0, [ecx + eax - 128];
                mulpd xmm0, xmm0;
                addpd xmm7, xmm0;

                movapd xmm1, [ecx + eax - 112];
                mulpd xmm1, xmm1;
                addpd xmm7, xmm1;

                movapd xmm2, [ecx + eax - 96];
                mulpd xmm2, xmm2;
                addpd xmm7, xmm2;

                movapd xmm3, [ecx + eax - 80];
                mulpd xmm3, xmm3;
                addpd xmm7, xmm3;

                movapd xmm0, [ecx + eax - 64];
                mulpd xmm0, xmm0;
                addpd xmm7, xmm0;

                movapd xmm1, [ecx + eax - 48];
                mulpd xmm1, xmm1;
                addpd xmm7, xmm1;

                movapd xmm2, [ecx + eax - 32];
                mulpd xmm2, xmm2;
                addpd xmm7, xmm2;

                movapd xmm3, [ecx + eax - 16];
                mulpd xmm3, xmm3;
                addpd xmm7, xmm3;
            jmp @addforxloop

            @loopEnd:

            sub eax, 128;

            jz @buildRes;

            @addforxloop2:
                movapd xmm0, [ecx + eax];
                mulpd xmm0, xmm0;
                addpd xmm7, xmm0;
            add eax, 16;
            jnz @addforxloop2;

            @buildRes:

            // handle last element differently
            movsd xmm2, [ecx + eax];
            mulsd xmm2, xmm2;
            addsd xmm7, xmm2;

            // build result
            movhlps xmm1, xmm7;
            addsd xmm7, xmm1;
            sqrtsd xmm7, xmm7;
            movsd xmm0, cOne;
            divsd xmm0, xmm7;

            movddup xmm6, xmm0;

            // multiply the result and build the result
            // for x := 0 to w - 1;
            // prepare for reverse loop
            mov eax, iters;
            @addforxloop3:
																add eax, 128;
                jg @loopEnd2;

                // prefetch data...
                // prefetch [ecx + eax];
                // prefetchw [ebx + eax];

                // addition:
                movapd xmm0, [ecx + eax - 128];
                mulpd xmm0, xmm6;
                movapd [ebx + eax - 128], xmm0;

                movapd xmm1, [ecx + eax - 112];
                mulpd xmm1, xmm6;
                movapd [ebx + eax - 112], xmm1;

                movapd xmm2, [ecx + eax - 96];
                mulpd xmm2, xmm6;
                movapd [ebx + eax - 96], xmm2;

                movapd xmm3, [ecx + eax - 80];
                mulpd xmm3, xmm6;
                movapd [ebx + eax - 80], xmm3;

                movapd xmm0, [ecx + eax - 64];
                mulpd xmm0, xmm6;
                movapd [ebx + eax - 64], xmm0;

                movapd xmm1, [ecx + eax - 48];
                mulpd xmm1, xmm6;
                movapd [ebx + eax - 48], xmm1;

                movapd xmm2, [ecx + eax - 32];
                mulpd xmm2, xmm6;
                movapd [ebx + eax - 32], xmm2;

                movapd xmm3, [ecx + eax - 16];
                mulpd xmm3, xmm6;
                movapd [ebx + eax - 16], xmm3;
            jmp @addforxloop3

            @loopEnd2:

            sub eax, 128;

            jz @nextLine;

            @addforxloop4:
                movapd xmm0, [ecx + eax];
                mulpd xmm0, xmm6;
                movapd [ebx + eax], xmm0;
            add eax, 16;
            jnz @addforxloop4;

            @nextLine:

            // handle last element
            movsd xmm0, [ecx + eax];
            mulsd xmm0, xmm6;
            movsd [ebx + eax], xmm0;

            // next line:
            add ecx, srcLineWidth;
            add ebx, destLineWidth;

        // loop y end
        dec edx;
        jnz @@addforyloop;

        pop ebx;
     end;
end;

procedure ASMMatrixNormalizeRowUnAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
var iters : TASMNativeInt;
begin
     Assert((width and 1) = 1, 'Error width must be odd');

     assert((width > 0) and (height > 0) and (srcLineWidth >= width*sizeof(double)) and (destLineWidth >= width*sizeof(double)), 'Dimension error');

     iters := -(width - 1)*sizeof(double);

     asm
        push ebx;
        // helper registers for the mt1, mt2 and dest pointers
        mov ecx, src;
        sub ecx, iters;

        mov ebx, dest;
        sub ebx, iters;

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
                mulpd xmm0, xmm0;
                addpd xmm7, xmm0;

                movupd xmm1, [ecx + eax - 112];
                mulpd xmm1, xmm1;
                addpd xmm7, xmm1;

                movupd xmm2, [ecx + eax - 96];
                mulpd xmm2, xmm2;
                addpd xmm7, xmm2;

                movupd xmm3, [ecx + eax - 80];
                mulpd xmm3, xmm3;
                addpd xmm7, xmm3;

                movupd xmm0, [ecx + eax - 64];
                mulpd xmm0, xmm0;
                addpd xmm7, xmm0;

                movupd xmm1, [ecx + eax - 48];
                mulpd xmm1, xmm1;
                addpd xmm7, xmm1;

                movupd xmm2, [ecx + eax - 32];
                mulpd xmm2, xmm2;
                addpd xmm7, xmm2;

                movupd xmm3, [ecx + eax - 16];
                mulpd xmm3, xmm3;
                addpd xmm7, xmm3;
            jmp @addforxloop

            @loopEnd:

            sub eax, 128;

            jz @buildRes;

            @addforxloop2:
                movupd xmm0, [ecx + eax];
                mulpd xmm0, xmm0;
                addpd xmm7, xmm0;
            add eax, 16;
            jnz @addforxloop2;

            @buildRes:

            // handle last element differently
            movsd xmm2, [ecx + eax];
            mulsd xmm2, xmm2;
            addsd xmm7, xmm2;

            // build result
            movhlps xmm1, xmm7;
            addsd xmm7, xmm1;
            sqrtsd xmm7, xmm7;
            movsd xmm0, cOne;
            divsd xmm0, xmm7;

            movddup xmm6, xmm0;

            // multiply the result and build the result
            // for x := 0 to w - 1;
            // prepare for reverse loop
            mov eax, iters;
            @addforxloop3:
                add eax, 128;
                jg @loopEnd2;

                // addition:
                movupd xmm0, [ecx + eax - 128];
                mulpd xmm0, xmm6;
                movupd [ebx + eax - 128], xmm0;

                movupd xmm1, [ecx + eax - 112];
                mulpd xmm1, xmm6;
                movupd [ebx + eax - 112], xmm1;

                movupd xmm2, [ecx + eax - 96];
                mulpd xmm2, xmm6;
                movupd [ebx + eax - 96], xmm2;

                movupd xmm3, [ecx + eax - 80];
                mulpd xmm3, xmm6;
                movupd [ebx + eax - 80], xmm3;

                movupd xmm0, [ecx + eax - 64];
                mulpd xmm0, xmm6;
                movupd [ebx + eax - 64], xmm0;

                movupd xmm1, [ecx + eax - 48];
                mulpd xmm1, xmm6;
                movupd [ebx + eax - 48], xmm1;

                movupd xmm2, [ecx + eax - 32];
                mulpd xmm2, xmm6;
                movupd [ebx + eax - 32], xmm2;

                movupd xmm3, [ecx + eax - 16];
                mulpd xmm3, xmm6;
                movupd [ebx + eax - 16], xmm3;
            jmp @addforxloop3

            @loopEnd2:

            sub eax, 128;

            jz @nextLine;

            @addforxloop4:
                movupd xmm0, [ecx + eax];
                mulpd xmm0, xmm6;
                movupd [ebx + eax], xmm0;
            add eax, 16;
            jnz @addforxloop4;

            @nextLine:

            // handle last element
            movsd xmm0, [ecx + eax];
            mulsd xmm0, xmm6;
            movsd [ebx + eax], xmm0;

            // next line:
            add ecx, srcLineWidth;
            add ebx, destLineWidth;

        // loop y end
        dec edx;
        jnz @@addforyloop;

        pop ebx;
     end;
end;

procedure ASMMatrixNormalizeColumnAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
var iters : TASMNativeInt;
    iters2 : TASMNativeInt;
begin
     Assert((Cardinal(Src) and $0000000F = 0) and ((srcLineWidth and $0000000F) = 0) and
            (Cardinal(dest) and $0000000F = 0) and ((destLineWidth and $0000000F) = 0), 'Error non aligned data');
     Assert((width and 1) = 0, 'Error width must be even');

     assert((width > 0) and (height > 0) and (srcLineWidth >= width*sizeof(double)) and (destLineWidth >= width*sizeof(double)), 'Dimension error');

     iters := height*srcLineWidth;
     iters2 := height*destLineWidth;

     asm
        push ebx;
        push esi;

        // helper registers for the mt1, mt2 and dest pointers
        mov ecx, src;
        sub ecx, srcLineWidth;
        mov ebx, dest;
        sub ebx, destLineWidth;

        // for x := 0 to width - 1:
        mov edx, Width;
        sar edx, 1;
        @@addforxloop:
            xorpd xmm7, xmm7;

            // for y := 0 to height - 1;
            // prepare for reverse loop
            mov eax, iters;
            @addforyloop:
                movapd xmm0, [ecx + eax];
                mulpd xmm0, xmm0;
                addpd xmm7, xmm0;
            sub eax, srcLineWidth;
            jnz @addforyloop;

            // build result
            sqrtpd xmm7, xmm7;
            movddup xmm6, cOne;
            divpd xmm6, xmm7;

            // multiply the result and build the result
            // for x := 0 to w - 1;
            // prepare for reverse loop
            mov eax, iters;
            mov esi, iters2;

            @addforyloop2:
                movapd xmm0, [ecx + eax];
                mulpd xmm0, xmm6;
                movapd [ebx + esi], xmm0;
                sub esi, destLineWidth;
            sub eax, srcLineWidth;
            jnz @addforyloop2;

            // next columns:
            add ecx, 16;
            add ebx, 16;

        // loop x end
        dec edx;
        jnz @@addforxloop;

        pop esi;
        pop ebx;
     end;
end;

procedure ASMMatrixNormalizeColumnUnAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
var iters : TASMNativeInt;
    iters2 : TASMNativeInt;
begin
     Assert((width and 1) = 0, 'Error width must be even');

     assert((width > 0) and (height > 0) and (srcLineWidth >= width*sizeof(double)) and (destLineWidth >= width*sizeof(double)), 'Dimension error');

     iters := height*srcLineWidth;
     iters2 := height*destLineWidth;

     asm
        push ebx;
        push esi;

        // helper registers for the mt1, mt2 and dest pointers
        mov ecx, src;
        sub ecx, srcLineWidth;
        mov ebx, dest;
        sub ebx, destLineWidth;

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
                mulpd xmm0, xmm0;
                addpd xmm7, xmm0;
            sub eax, srcLineWidth;
            jnz @addforyloop;

            // build result
            sqrtpd xmm7, xmm7;
            movddup xmm6, cOne;
            divpd xmm6, xmm7;

            // multiply the result and build the result
            // for x := 0 to w - 1;
            // prepare for reverse loop
            mov eax, iters;
            mov esi, iters2;

            @addforyloop2:
                movupd xmm0, [ecx + eax];
                mulpd xmm0, xmm6;
                movupd [ebx + esi], xmm0;
                sub esi, destLineWidth;
            sub eax, srcLineWidth;
            jnz @addforyloop2;

            // next columns:
            add ecx, 16;
            add ebx, 16;

        // loop x end
        dec edx;
        jnz @@addforxloop;

        pop esi;
        pop ebx;
     end;
end;

procedure ASMMatrixNormalizeColumnAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
var iters : TASMNativeInt;
    iters2 : TASMNativeInt;
begin
     Assert((Cardinal(Src) and $0000000F = 0) and ((srcLineWidth and $0000000F) = 0) and
            (Cardinal(dest) and $0000000F = 0) and ((destLineWidth and $0000000F) = 0), 'Error non aligned data');
     Assert((width and 1) = 1, 'Error width must be even');

     assert((width > 0) and (height > 0) and (srcLineWidth >= width*sizeof(double)) and (destLineWidth >= width*sizeof(double)), 'Dimension error');

     iters := height*srcLineWidth;
     iters2 := height*destLineWidth;

     asm
        push ebx;
        push esi;

        // helper registers for the mt1, mt2 and dest pointers
        mov ecx, src;
        sub ecx, srcLineWidth;
        mov ebx, dest;
        sub ebx, destLineWidth;

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
                movapd xmm0, [ecx + eax];
                mulpd xmm0, xmm0;
                addpd xmm7, xmm0;
            sub eax, srcLineWidth;
            jnz @addforyloop;

            // build result
            sqrtpd xmm7, xmm7;
            movddup xmm6, cOne;
            divpd xmm6, xmm7;

            // multiply the result and build the result
            // for x := 0 to w - 1;
            // prepare for reverse loop
            mov eax, iters;
            mov esi, iters2;

            @addforyloop2:
                movapd xmm0, [ecx + eax];
                mulpd xmm0, xmm6;
                movapd [ebx + esi], xmm0;
                sub esi, destLineWidth;
            sub eax, srcLineWidth;
            jnz @addforyloop2;

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
            movsd xmm0, [ecx + eax];
            mulsd xmm0, xmm0;
            addsd xmm7, xmm0;
        sub eax, srcLineWidth;
        jnz @addforyloop3;

        // build result
        sqrtsd xmm7, xmm7;
        movsd xmm6, cOne;
        divsd xmm6, xmm7;

        // multiply the result and build the result
        // for x := 0 to w - 1;
        // prepare for reverse loop
        mov eax, iters;
        mov esi, iters2;

        @addforyloop4:
            movsd xmm0, [ecx + eax];
            mulsd xmm0, xmm6;
            movsd [ebx + esi], xmm0;
            sub esi, destLineWidth;
        sub eax, srcLineWidth;
        jnz @addforyloop4;

        pop esi;
        pop ebx;
     end;
end;

procedure ASMMatrixNormalizeColumnUnAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
var iters : TASMNativeInt;
    iters2 : TASMNativeInt;
begin
     Assert((width and 1) = 1, 'Error width must be even');

     assert((width > 0) and (height > 0) and (srcLineWidth >= width*sizeof(double)) and (destLineWidth >= width*sizeof(double)), 'Dimension error');

     iters := height*srcLineWidth;
     iters2 := height*destLineWidth;

     asm
        push ebx;
        push esi;

        // helper registers for the mt1, mt2 and dest pointers
        mov ecx, src;
        sub ecx, srcLineWidth;
        mov ebx, dest;
        sub ebx, destLineWidth;

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
                mulpd xmm0, xmm0;
                addpd xmm7, xmm0;
            sub eax, srcLineWidth;
            jnz @addforyloop;

            // build result
            sqrtpd xmm7, xmm7;
            movddup xmm6, cOne;
            divpd xmm6, xmm7;

            // multiply the result and build the result
            // for x := 0 to w - 1;
            // prepare for reverse loop
            mov eax, iters;
            mov esi, iters2;

            @addforyloop2:
                movupd xmm0, [ecx + eax];
                mulpd xmm0, xmm6;
                movupd [ebx + esi], xmm0;
                sub esi, destLineWidth;
            sub eax, srcLineWidth;
            jnz @addforyloop2;

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
            movsd xmm0, [ecx + eax];
            mulsd xmm0, xmm0;
            addsd xmm7, xmm0;
        sub eax, srcLineWidth;
        jnz @addforyloop3;

        // build result
        sqrtsd xmm7, xmm7;
        movsd xmm6, cOne;
        divsd xmm6, xmm7;

        // multiply the result and build the result
        // for x := 0 to w - 1;
        // prepare for reverse loop
        mov eax, iters;
        mov esi, iters2;

        @addforyloop4:
            movsd xmm0, [ecx + eax];
            mulsd xmm0, xmm6;
            movsd [ebx + esi], xmm0;
            sub esi, destLineWidth;
        sub eax, srcLineWidth;
        jnz @addforyloop4;

        pop esi;
        pop ebx;
     end;
end;

{$ENDIF}

end.
