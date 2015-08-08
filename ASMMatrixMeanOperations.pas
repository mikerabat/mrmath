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


unit ASMMatrixMeanOperations;

interface

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}
{$IFNDEF x64}

uses MatrixConst;

procedure ASMMatrixMeanRowAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
procedure ASMMatrixMeanRowUnAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
procedure ASMMatrixMeanRowAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
procedure ASMMatrixMeanRowUnAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);

procedure ASMMatrixMeanColumnAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
procedure ASMMatrixMeanColumnUnAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
procedure ASMMatrixMeanColumnAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
procedure ASMMatrixMeanColumnUnAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);

procedure ASMMatrixVarRowAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean);
procedure ASMMatrixVarRowUnAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean);
procedure ASMMatrixVarRowAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean);
procedure ASMMatrixVarRowUnAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean);

procedure ASMMatrixVarColumnAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean);
procedure ASMMatrixVarColumnUnAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean);
procedure ASMMatrixVarColumnAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean);
procedure ASMMatrixVarColumnUnAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean);


{$ENDIF}

implementation

{$IFNDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$ENDIF}

procedure ASMMatrixMeanRowAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
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

        {$IFDEF FPC}
        mov eax, width;
        cvtsi2sd xmm5, eax;
        {$ELSE}
        cvtsi2sd xmm5, width;
        {$ENDIF}

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

            addpd xmm0, xmm1;
            addpd xmm2, xmm3;
            addpd xmm0, xmm2;

            // build result
            movhlps xmm1, xmm0;
            addsd xmm0, xmm1;

            divsd xmm0, xmm5;

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

procedure ASMMatrixMeanRowUnAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
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

        {$IFDEF FPC}
        mov eax, width;
        cvtsi2sd xmm5, eax;
        {$ELSE}
        cvtsi2sd xmm5, width;
        {$ENDIF}

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
            divsd xmm7, xmm5;

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

procedure ASMMatrixMeanRowAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
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

        {$IFDEF FPC}
        mov eax, width;
        cvtsi2sd xmm5, eax;
        {$ELSE}
        cvtsi2sd xmm5, width;
        {$ENDIF}

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

            addpd xmm0, xmm1;
            addpd xmm2, xmm3;
            addpd xmm0, xmm2;

            // handle last element differently
            movlpd xmm2, [ecx + eax];
            addsd xmm0, xmm2;

            // build result
            movhlps xmm1, xmm0;
            addsd xmm0, xmm1;

            divsd xmm0, xmm5;

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

procedure ASMMatrixMeanRowUnAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
var iters : TASMNativeInt;
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

        {$IFDEF FPC}
        mov eax, width;
        cvtsi2sd xmm5, eax;
        {$ELSE}
        cvtsi2sd xmm5, width;
        {$ENDIF}

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

            divsd xmm7, xmm5;

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

procedure ASMMatrixMeanColumnAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
var iters : TASMNativeInt;
begin
     Assert((Cardinal(Src) and $0000000F = 0) and ((srcLineWidth and $0000000F) = 0) and
            (Cardinal(dest) and $0000000F = 0) and ((destLineWidth and $0000000F) = 0), 'Error non aligned data');
     Assert((width and 1) = 0, 'Error width must be even');

     assert((width > 0) and (height > 0) and (srcLineWidth >= width*sizeof(double)) and (destLineWidth >= width*sizeof(double)), 'Dimension error');

     iters := -height*srcLineWidth;

     asm
        push ebx;

        // helper registers for the mt1, mt2 and dest pointers
        mov ecx, src;
        sub ecx, iters;
        mov ebx, dest;

        {$IFDEF FPC}
        mov eax, height;
        cvtsi2sd xmm5, eax;
        {$ELSE}
        cvtsi2sd xmm5, height;
        {$ENDIF}
        movddup xmm6, xmm5;

        // for x := 0 to width - 1:
        mov edx, Width;
        sar edx, 1;
        @@addforxloop:
            xorpd xmm7, xmm7;

            // for y := 0 to height - 1;
            // prepare for reverse loop
            mov eax, iters;
            @addforyloop:
                addpd xmm7, [ecx + eax];
            add eax, srcLineWidth;
            jnz @addforyloop;

            // build result
            divpd xmm7, xmm6;
            movapd [ebx], xmm7;

            // next columns:
            add ecx, 16;
            add ebx, 16;

        // loop x end
        dec edx;
        jnz @@addforxloop;

        pop ebx;
     end;
end;

procedure ASMMatrixMeanColumnUnAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
var iters : TASMNativeInt;
begin
     Assert((width and 1) = 0, 'Error width must be even');

     assert((width > 0) and (height > 0) and (srcLineWidth >= width*sizeof(double)) and (destLineWidth >= width*sizeof(double)), 'Dimension error');

     iters := -height*srcLineWidth;

     asm
        push ebx;

        // helper registers for the mt1, mt2 and dest pointers
        mov ecx, src;
        sub ecx, iters;
        mov ebx, dest;

        {$IFDEF FPC}
        mov eax, height;
        cvtsi2sd xmm5, eax;
        {$ELSE}
        cvtsi2sd xmm5, height;
        {$ENDIF}
        movddup xmm6, xmm5;

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
            divpd xmm7, xmm6;
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

procedure ASMMatrixMeanColumnAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
var iters : TASMNativeInt;
begin
     Assert((Cardinal(Src) and $0000000F = 0) and ((srcLineWidth and $0000000F) = 0) and
            (Cardinal(dest) and $0000000F = 0) and ((destLineWidth and $0000000F) = 0), 'Error non aligned data');
     Assert((width and 1) = 1, 'Error width must be even');

     assert((width > 0) and (height > 0) and (srcLineWidth >= width*sizeof(double)) and (destLineWidth >= width*sizeof(double)), 'Dimension error');

     iters := -height*srcLineWidth;

     asm
        push ebx;

        // helper registers for the mt1, mt2 and dest pointers
        mov ecx, src;
        sub ecx, iters;
        mov ebx, dest;

        {$IFDEF FPC}
        mov eax, height;
        cvtsi2sd xmm5, eax;
        {$ELSE}
        cvtsi2sd xmm5, height;
        {$ENDIF}
        movddup xmm6, xmm5;

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
                addpd xmm7, [ecx + eax];
            add eax, srcLineWidth;
            jnz @addforyloop;

            // build result
            divpd xmm7, xmm6;
            movapd [ebx], xmm7;

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
        divsd xmm7, xmm6;

        movlpd [ebx], xmm7;

        pop ebx;
     end;
end;

procedure ASMMatrixMeanColumnUnAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
var iters : TASMNativeInt;
begin
     Assert((width and 1) = 1, 'Error width must be even');

     assert((width > 0) and (height > 0) and (srcLineWidth >= width*sizeof(double)) and (destLineWidth >= width*sizeof(double)), 'Dimension error');

     iters := -height*srcLineWidth;

     asm
        push ebx;

        // helper registers for the mt1, mt2 and dest pointers
        mov ecx, src;
        sub ecx, iters;
        mov ebx, dest;

        {$IFDEF FPC}
        mov eax, height;
        cvtsi2sd xmm5, eax;
        {$ELSE}
        cvtsi2sd xmm5, height;
        {$ENDIF}
        movddup xmm6, xmm5;

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
            divpd xmm7, xmm6;
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
        divsd xmm7, xmm6;
        movlpd [ebx], xmm7;

        pop ebx;
     end;
end;


// ###########################################
// #### Variance calculation
// ###########################################

const cOnes : Array[0..1] of double = (1, 1);
      cOne : double = 1;
      

procedure ASMMatrixVarRowAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean);
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

        {$IFDEF FPC}
        mov eax, width;
        cvtsi2sd xmm5, eax;
        {$ELSE}
        cvtsi2sd xmm5, width;
        {$ENDIF}

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

            addpd xmm0, xmm1;
            addpd xmm2, xmm3;
            addpd xmm0, xmm2;

            // build result
            movhlps xmm1, xmm0;
            addsd xmm0, xmm1;

            divsd xmm0, xmm5;
            
            // we have calculated the mean -> 
            // repeat the loop to calculate the variance

            movddup xmm4, xmm0;
            xorpd xmm0, xmm0;
            
            // for x := 0 to w - 1;
            // prepare for reverse loop
            // variance = sum (x - mean)^2
            mov eax, iters;
            @addforxloop3:
                add eax, 128;
                jg @loopEnd2;
                // prefetch data...
                // prefetch [ecx + eax];

                // addition:
                movapd xmm1, [ecx + eax - 128];
                movapd xmm2, [ecx + eax - 112];
                subpd xmm1, xmm4;
                subpd xmm2, xmm4;
                mulpd xmm1, xmm1;
                mulpd xmm2, xmm2;
                addpd xmm0, xmm1;
                addpd xmm0, xmm2;
                
                movapd xmm1, [ecx + eax - 96];
                movapd xmm2, [ecx + eax - 80];
                subpd xmm1, xmm4;
                subpd xmm2, xmm4;
                mulpd xmm1, xmm1;
                mulpd xmm2, xmm2;
                addpd xmm0, xmm1;
                addpd xmm0, xmm2;
                
                movapd xmm1, [ecx + eax - 64];
                movapd xmm2, [ecx + eax - 48];
                subpd xmm1, xmm4;
                subpd xmm2, xmm4;
                mulpd xmm1, xmm1;
                mulpd xmm2, xmm2;
                addpd xmm0, xmm1;
                addpd xmm0, xmm2;
                
                movapd xmm1, [ecx + eax - 32];
                movapd xmm2, [ecx + eax - 16];
                subpd xmm1, xmm4;
                subpd xmm2, xmm4;
                mulpd xmm1, xmm1;
                mulpd xmm2, xmm2;
                addpd xmm0, xmm1;
                addpd xmm0, xmm2;
                
            jmp @addforxloop3

            @loopEnd2:

            sub eax, 128;

            jz @buildRes2;

            @addforxloop4:
                movapd xmm1, [ecx + eax];
                subpd xmm1, xmm4;
                mulpd xmm1, xmm1;
                addpd xmm0, xmm1;
            add eax, 16;
            jnz @addforxloop4;

            @buildRes2:

            // build result
            movhlps xmm1, xmm0;
            addsd xmm0, xmm1;
            
            // check if we need to use the unbiased version
            cmp unbiased, 0;
            jz @@dobiased;

            movsd xmm4, xmm5;
            subsd xmm4, cOne;
            maxsd xmm4, cOne;

            divsd xmm0, xmm4;

            jmp @@writeRes;

            @@dobiased:
            divsd xmm0, xmm5;
            
            // write result
            @@writeRes:
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

procedure ASMMatrixVarRowUnAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean);
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

        {$IFDEF FPC}
        mov eax, width;
        cvtsi2sd xmm5, eax;
        {$ELSE}
        cvtsi2sd xmm5, width;
        {$ENDIF}

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
            divsd xmm7, xmm5;

            // we have calculated the mean -> 
            // repeat the loop to calculate the variance

            movddup xmm4, xmm7;
            xorpd xmm0, xmm0;
            
            // for x := 0 to w - 1;
            // prepare for reverse loop
            // variance = sum (x - mean)^2
            mov eax, iters;
            @addforxloop3:
                add eax, 128;
                jg @loopEnd2;
                // prefetch data...
                // prefetch [ecx + eax];

                // addition:
                movupd xmm1, [ecx + eax - 128];
                movupd xmm2, [ecx + eax - 112];
                subpd xmm1, xmm4;
                subpd xmm2, xmm4;
                mulpd xmm1, xmm1;
                mulpd xmm2, xmm2;
                addpd xmm0, xmm1;
                addpd xmm0, xmm2;
                
                movupd xmm1, [ecx + eax - 96];
                movupd xmm2, [ecx + eax - 80];
                subpd xmm1, xmm4;
                subpd xmm2, xmm4;
                mulpd xmm1, xmm1;
                mulpd xmm2, xmm2;
                addpd xmm0, xmm1;
                addpd xmm0, xmm2;
                
                movupd xmm1, [ecx + eax - 64];
                movupd xmm2, [ecx + eax - 48];
                subpd xmm1, xmm4;
                subpd xmm2, xmm4;
                mulpd xmm1, xmm1;
                mulpd xmm2, xmm2;
                addpd xmm0, xmm1;
                addpd xmm0, xmm2;
                
                movupd xmm1, [ecx + eax - 32];
                movupd xmm2, [ecx + eax - 16];
                subpd xmm1, xmm4;
                subpd xmm2, xmm4;
                mulpd xmm1, xmm1;
                mulpd xmm2, xmm2;
                addpd xmm0, xmm1;
                addpd xmm0, xmm2;
                
            jmp @addforxloop3

            @loopEnd2:

            sub eax, 128;

            jz @buildRes2;

            @addforxloop4:
                movupd xmm1, [ecx + eax];
                subpd xmm1, xmm4;
                mulpd xmm1, xmm1;
                addpd xmm0, xmm1;
            add eax, 16;
            jnz @addforxloop4;

            @buildRes2:

            // build result
            movhlps xmm1, xmm0;
            addsd xmm0, xmm1;
            
            // check if we need to use the unbiased version
            cmp unbiased, 0;
            jz @@dobiased;

            movsd xmm4, xmm5;
            subsd xmm4, cOne;
            maxsd xmm4, cOne;

            divsd xmm0, xmm4;

            jmp @@writeRes;

            @@dobiased:
            divsd xmm0, xmm5;

            // write result
            @@writeRes:
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

procedure ASMMatrixVarRowAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean);
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

        {$IFDEF FPC}
        mov eax, width;
        cvtsi2sd xmm5, eax;
        {$ELSE}
        cvtsi2sd xmm5, width;
        {$ENDIF}

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

            addpd xmm0, xmm1;
            addpd xmm2, xmm3;
            addpd xmm0, xmm2;

            // handle last element differently
            movlpd xmm2, [ecx + eax];
            addsd xmm0, xmm2;

            // build result
            movhlps xmm1, xmm0;
            addsd xmm0, xmm1;

            divsd xmm0, xmm5;


            // we have calculated the mean -> 
            // repeat the loop to calculate the variance

            movddup xmm4, xmm0;
            xorpd xmm0, xmm0;
            
            // for x := 0 to w - 1;
            // prepare for reverse loop
            // variance = sum (x - mean)^2
            mov eax, iters;
            @addforxloop3:
                add eax, 128;
                jg @loopEnd2;
                // prefetch data...
                // prefetch [ecx + eax];

                // addition:
                movapd xmm1, [ecx + eax - 128];
                movapd xmm2, [ecx + eax - 112];
                subpd xmm1, xmm4;
                subpd xmm2, xmm4;
                mulpd xmm1, xmm1;
                mulpd xmm2, xmm2;
                addpd xmm0, xmm1;
                addpd xmm0, xmm2;
                
                movapd xmm1, [ecx + eax - 96];
                movapd xmm2, [ecx + eax - 80];
                subpd xmm1, xmm4;
                subpd xmm2, xmm4;
                mulpd xmm1, xmm1;
                mulpd xmm2, xmm2;
                addpd xmm0, xmm1;
                addpd xmm0, xmm2;
                
                movapd xmm1, [ecx + eax - 64];
                movapd xmm2, [ecx + eax - 48];
                subpd xmm1, xmm4;
                subpd xmm2, xmm4;
                mulpd xmm1, xmm1;
                mulpd xmm2, xmm2;
                addpd xmm0, xmm1;
                addpd xmm0, xmm2;
                
                movapd xmm1, [ecx + eax - 32];
                movapd xmm2, [ecx + eax - 16];
                subpd xmm1, xmm4;
                subpd xmm2, xmm4;
                mulpd xmm1, xmm1;
                mulpd xmm2, xmm2;
                addpd xmm0, xmm1;
                addpd xmm0, xmm2;
                
            jmp @addforxloop3

            @loopEnd2:

            sub eax, 128;

            jz @buildRes2;

            @addforxloop4:
                movapd xmm1, [ecx + eax];
                subpd xmm1, xmm4;
                mulpd xmm1, xmm1;
                addpd xmm0, xmm1;
            add eax, 16;
            jnz @addforxloop4;

            @buildRes2:

            // handle last element differently
            movlpd xmm1, [ecx + eax];
            subsd xmm1, xmm4;
            mulsd xmm1, xmm1;
            addsd xmm0, xmm1;

            // build result
            movhlps xmm1, xmm0;
            addsd xmm0, xmm1;
            
            // check if we need to use the unbiased version
            cmp unbiased, 0;
            jz @@dobiased;

            movsd xmm4, xmm5;
            subsd xmm4, cOne;
            maxsd xmm4, cOne;

            divsd xmm0, xmm4;

            jmp @@writeRes;

            @@dobiased:
            divsd xmm0, xmm5;
            
            // write result
            @@writeRes:
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

procedure ASMMatrixVarRowUnAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean);
var iters : TASMNativeInt;
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

        {$IFDEF FPC}
        mov eax, width;
        cvtsi2sd xmm5, eax;
        {$ELSE}
        cvtsi2sd xmm5, width;
        {$ENDIF}

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

            divsd xmm7, xmm5;


            // we have calculated the mean -> 
            // repeat the loop to calculate the variance

            movddup xmm4, xmm7;
            xorpd xmm0, xmm0;
            
            // for x := 0 to w - 1;
            // prepare for reverse loop
            // variance = sum (x - mean)^2
            mov eax, iters;
            @addforxloop3:
                add eax, 128;
                jg @loopEnd2;
                // prefetch data...
                // prefetch [ecx + eax];

                // addition:
                movupd xmm1, [ecx + eax - 128];
                movupd xmm2, [ecx + eax - 112];
                subpd xmm1, xmm4;
                subpd xmm2, xmm4;
                mulpd xmm1, xmm1;
                mulpd xmm2, xmm2;
                addpd xmm0, xmm1;
                addpd xmm0, xmm2;
                
                movupd xmm1, [ecx + eax - 96];
                movupd xmm2, [ecx + eax - 80];
                subpd xmm1, xmm4;
                subpd xmm2, xmm4;
                mulpd xmm1, xmm1;
                mulpd xmm2, xmm2;
                addpd xmm0, xmm1;
                addpd xmm0, xmm2;
                
                movupd xmm1, [ecx + eax - 64];
                movupd xmm2, [ecx + eax - 48];
                subpd xmm1, xmm4;
                subpd xmm2, xmm4;
                mulpd xmm1, xmm1;
                mulpd xmm2, xmm2;
                addpd xmm0, xmm1;
                addpd xmm0, xmm2;
                
                movupd xmm1, [ecx + eax - 32];
                movupd xmm2, [ecx + eax - 16];
                subpd xmm1, xmm4;
                subpd xmm2, xmm4;
                mulpd xmm1, xmm1;
                mulpd xmm2, xmm2;
                addpd xmm0, xmm1;
                addpd xmm0, xmm2;
                
            jmp @addforxloop3

            @loopEnd2:

            sub eax, 128;

            jz @buildRes2;

            @addforxloop4:
                movupd xmm1, [ecx + eax];
                subpd xmm1, xmm4;
                mulpd xmm1, xmm1;
                addpd xmm0, xmm1;
            add eax, 16;
            jnz @addforxloop4;

            @buildRes2:

            // handle last element differently
            movlpd xmm1, [ecx + eax];
            subsd xmm1, xmm4;
            mulsd xmm1, xmm1;
            addsd xmm0, xmm1;
            
            // build result
            movhlps xmm1, xmm0;
            addsd xmm0, xmm1;
            
            // check if we need to use the unbiased version
            cmp unbiased, 0;
            jz @@dobiased;

            movsd xmm4, xmm5;
            subsd xmm4, cOne;
            maxsd xmm4, cOne;

            divsd xmm0, xmm4;

            jmp @@writeRes;

            @@dobiased:
            divsd xmm0, xmm5;

            // write result
            @@writeRes:
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

procedure ASMMatrixVarColumnAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean);
var iters : TASMNativeInt;
begin
     Assert((Cardinal(Src) and $0000000F = 0) and ((srcLineWidth and $0000000F) = 0) and
            (Cardinal(dest) and $0000000F = 0) and ((destLineWidth and $0000000F) = 0), 'Error non aligned data');
     Assert((width and 1) = 0, 'Error width must be even');

     assert((width > 0) and (height > 0) and (srcLineWidth >= width*sizeof(double)) and (destLineWidth >= width*sizeof(double)), 'Dimension error');

     iters := -height*srcLineWidth;

     asm
        push ebx;

        // helper registers for the mt1, mt2 and dest pointers
        mov ecx, src;
        sub ecx, iters;
        mov ebx, dest;

        movupd xmm2, cOnes;

        {$IFDEF FPC}
        mov eax, height;
        cvtsi2sd xmm5, eax;
        {$ELSE}
        cvtsi2sd xmm5, height;
        {$ENDIF}
        movddup xmm6, xmm5;

        // for x := 0 to width - 1:
        mov edx, Width;
        sar edx, 1;
        @@addforxloop:
            xorpd xmm7, xmm7;

            // calculate mean
            
            // for y := 0 to height - 1;
            // prepare for reverse loop
            mov eax, iters;
            @addforyloop:
                addpd xmm7, [ecx + eax];
            add eax, srcLineWidth;
            jnz @addforyloop;

            // build mean
            divpd xmm7, xmm6;

            // calculate final standard deviation
            // for y := 0 to height - 1;
            // prepare for reverse loop
            xorpd xmm0, xmm0;
            mov eax, iters;
            @addforyloop2:
                movapd xmm1, [ecx + eax];
                subpd xmm1, xmm7;
                mulpd xmm1, xmm1;
                addpd xmm0, xmm1;
            add eax, srcLineWidth;
            jnz @addforyloop2;

            // check if we need to use the unbiased version
            cmp unbiased, 0;
            jz @@dobiased;

            movapd xmm4, xmm6;
            subpd xmm4, xmm2;
            maxpd xmm4, xmm2;

            divpd xmm0, xmm4;

            jmp @@writeRes;

            @@dobiased:
            divpd xmm0, xmm6;

            // write result
            @@writeRes:
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

procedure ASMMatrixVarColumnUnAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean);
var iters : TASMNativeInt;
begin
     Assert((width and 1) = 0, 'Error width must be even');

     assert((width > 0) and (height > 0) and (srcLineWidth >= width*sizeof(double)) and (destLineWidth >= width*sizeof(double)), 'Dimension error');

     iters := -height*srcLineWidth;

     asm
        push ebx;

        // helper registers for the mt1, mt2 and dest pointers
        mov ecx, src;
        sub ecx, iters;
        mov ebx, dest;

        movupd xmm2, cOnes;

        {$IFDEF FPC}
        mov eax, height;
        cvtsi2sd xmm5, eax;
        {$ELSE}
        cvtsi2sd xmm5, height;
        {$ENDIF}
        movddup xmm6, xmm5;

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

            // build mean
            divpd xmm7, xmm6;

            // calculate final standard deviation
            // for y := 0 to height - 1;
            // prepare for reverse loop
            xorpd xmm0, xmm0;
            mov eax, iters;
            @addforyloop2:
                movupd xmm1, [ecx + eax];
                subpd xmm1, xmm7;
                mulpd xmm1, xmm1;
                addpd xmm0, xmm1;
            add eax, srcLineWidth;
            jnz @addforyloop2;

            // check if we need to use the unbiased version
            cmp unbiased, 0;
            jz @@dobiased;

            movapd xmm4, xmm6;
            subpd xmm4, xmm2;
            maxpd xmm4, xmm2;

            divpd xmm0, xmm4;

            jmp @@writeRes;

            @@dobiased:
            divpd xmm0, xmm6;

            // write result
            @@writeRes:
            movupd [ebx], xmm0;

            // next columns:
            add ecx, 16;
            add ebx, 16;

        // loop x end
        dec edx;
        jnz @@addforxloop;

        pop ebx;
     end;
end;

procedure ASMMatrixVarColumnAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean);
var iters : TASMNativeInt;
begin
     Assert((Cardinal(Src) and $0000000F = 0) and ((srcLineWidth and $0000000F) = 0) and
            (Cardinal(dest) and $0000000F = 0) and ((destLineWidth and $0000000F) = 0), 'Error non aligned data');
     Assert((width and 1) = 1, 'Error width must be even');

     assert((width > 0) and (height > 0) and (srcLineWidth >= width*sizeof(double)) and (destLineWidth >= width*sizeof(double)), 'Dimension error');

     iters := -height*srcLineWidth;

     asm
        push ebx;

        // helper registers for the mt1, mt2 and dest pointers
        mov ecx, src;
        sub ecx, iters;
        mov ebx, dest;

        movupd xmm2, cOnes;

        {$IFDEF FPC}
        mov eax, height;
        cvtsi2sd xmm5, eax;
        {$ELSE}
        cvtsi2sd xmm5, height;
        {$ENDIF}
        movddup xmm6, xmm5;

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
                addpd xmm7, [ecx + eax];
            add eax, srcLineWidth;
            jnz @addforyloop;

            // build mean
            divpd xmm7, xmm6;

            // calculate final standard deviation
            // for y := 0 to height - 1;
            // prepare for reverse loop
            xorpd xmm0, xmm0;
            mov eax, iters;
            @addforyloop2:
                movapd xmm1, [ecx + eax];
                subpd xmm1, xmm7;
                mulpd xmm1, xmm1;
                addpd xmm0, xmm1;
            add eax, srcLineWidth;
            jnz @addforyloop2;

            // check if we need to use the unbiased version
            cmp unbiased, 0;
            jz @@dobiased;

            movapd xmm4, xmm6;
            subpd xmm4, xmm2;
            maxpd xmm4, xmm2;

            divpd xmm0, xmm4;

            jmp @@writeRes;

            @@dobiased:
            divpd xmm0, xmm6;

            // write result
            @@writeRes:
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

        // build mean
        divsd xmm7, xmm6;

        // calculate final standard deviation
        // for y := 0 to height - 1;
        // prepare for reverse loop
        xorpd xmm0, xmm0;
        mov eax, iters;
        @addforyloop4:
            movlpd xmm1, [ecx + eax];
            subsd xmm1, xmm7;
            mulsd xmm1, xmm1;
            addsd xmm0, xmm1;
        add eax, srcLineWidth;
        jnz @addforyloop4;

        // check if we need to use the unbiased version
        cmp unbiased, 0;
        jz @@dobiased2;

        movapd xmm4, xmm6;
        subsd xmm4, cOne;
        maxsd xmm4, cOne;

        divsd xmm0, xmm4;

        jmp @@writeRes2;

        @@dobiased2:
        divsd xmm0, xmm6;

        // write result
        @@writeRes2:
        movlpd [ebx], xmm0;

        pop ebx;
     end;
end;

procedure ASMMatrixVarColumnUnAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean);
var iters : TASMNativeInt;
begin
     Assert((width and 1) = 1, 'Error width must be even');

     assert((width > 0) and (height > 0) and (srcLineWidth >= width*sizeof(double)) and (destLineWidth >= width*sizeof(double)), 'Dimension error');

     iters := -height*srcLineWidth;

     asm
        push ebx;

        // helper registers for the mt1, mt2 and dest pointers
        mov ecx, src;
        sub ecx, iters;
        mov ebx, dest;

        movupd xmm2, cOnes;

        {$IFDEF FPC}
        mov eax, height;
        cvtsi2sd xmm5, eax;
        {$ELSE}
        cvtsi2sd xmm5, height;
        {$ENDIF}
        movddup xmm6, xmm5;

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

            // build mean
            divpd xmm7, xmm6;

            // calculate final standard deviation
            // for y := 0 to height - 1;
            // prepare for reverse loop
            xorpd xmm0, xmm0;
            mov eax, iters;
            @addforyloop2:
                movupd xmm1, [ecx + eax];
                subpd xmm1, xmm7;
                mulpd xmm1, xmm1;
                addpd xmm0, xmm1;
            add eax, srcLineWidth;
            jnz @addforyloop2;

            // check if we need to use the unbiased version
            cmp unbiased, 0;
            jz @@dobiased;

            movapd xmm4, xmm6;
            subpd xmm4, xmm2;
            maxpd xmm4, xmm2;

            divpd xmm0, xmm4;

            jmp @@writeRes;

            @@dobiased:
            divpd xmm0, xmm6;

            // write result
            @@writeRes:
            movupd [ebx], xmm0;

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

        // build mean
        divsd xmm7, xmm6;

        // calculate final standard deviation
        // for y := 0 to height - 1;
        // prepare for reverse loop
        xorpd xmm0, xmm0;
        mov eax, iters;
        @addforyloop4:
            movlpd xmm1, [ecx + eax];
            subsd xmm1, xmm7;
            mulsd xmm1, xmm1;
            addsd xmm0, xmm1;
        add eax, srcLineWidth;
        jnz @addforyloop4;

        // check if we need to use the unbiased version
        cmp unbiased, 0;
        jz @@dobiased2;

        movapd xmm4, xmm6;
        subsd xmm4, cOne;
        maxsd xmm4, cOne;

        divsd xmm0, xmm4;

        jmp @@writeRes2;

        @@dobiased2:
        divsd xmm0, xmm6;

        // write result
        @@writeRes2:
        movlpd [ebx], xmm0;

        pop ebx;
     end;
end;


{$ENDIF}

end.
