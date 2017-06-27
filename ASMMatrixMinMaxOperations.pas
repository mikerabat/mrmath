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


unit ASMMatrixMinMaxOperations;

interface

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}
{$IFNDEF x64}

uses MatrixConst;

function ASMMatrixMaxAlignedEvenW(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double;
function ASMMatrixMaxUnAlignedEvenW(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double;

function ASMMatrixMaxAlignedOddW(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double;
function ASMMatrixMaxUnAlignedOddW(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double;

function ASMMatrixMinAlignedEvenW(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double;
function ASMMatrixMinUnAlignedEvenW(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double;

function ASMMatrixMinAlignedOddW(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double;
function ASMMatrixMinUnAlignedOddW(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double;

{$ENDIF}

implementation

{$IFNDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$ENDIF}

function ASMMatrixMaxAlignedEvenW(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double;
var iters : TASMNativeInt;
begin
     Assert((Cardinal(mt) and $0000000F = 0), 'Error non aligned data');
     Assert((width and 1) = 0, 'Error width must be even');

     assert((width > 0) and (height > 0) and (LineWidth >= width*sizeof(double)), 'Dimension error');

     iters := -width*sizeof(double);

     asm
        // helper registers for the mt1, mt2 and dest pointers
        mov ecx, mt;
        sub ecx, iters;

        movddup xmm0, cNegMaxDouble;
        movapd xmm1, xmm0;
   					movapd xmm2, xmm0;
   					movapd xmm3, xmm0;

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

                // max:
                maxpd xmm0, [ecx + eax - 128];
                maxpd xmm1, [ecx + eax - 112];
                maxpd xmm2, [ecx + eax - 96];
                maxpd xmm1, [ecx + eax - 80];
                maxpd xmm0, [ecx + eax - 64];
                maxpd xmm1, [ecx + eax - 48];
                maxpd xmm2, [ecx + eax - 32];
                maxpd xmm3, [ecx + eax - 16];
            jmp @addforxloop

            @loopEnd:

            sub eax, 128;

            jz @nextLine;

            @addforxloop2:
                maxpd xmm0, [ecx + eax];
            add eax, 16;
            jnz @addforxloop2;

            @nextLine:

            // next line:
            add ecx, LineWidth;

        // loop y end
        dec edx;
        jnz @@addforyloop;

        // final max ->
        maxpd xmm0, xmm1;
   					maxpd xmm0, xmm2;
   					maxpd xmm0, xmm3;
        movhlps xmm1, xmm0;
        maxsd xmm0, xmm1;
        movsd Result, xmm0;
     end;
end;

function ASMMatrixMaxUnAlignedEvenW(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double;
var iters : TASMNativeInt;
begin
     Assert((width and 1) = 0, 'Error width must be even');
     assert((width > 0) and (height > 0) and (LineWidth >= width*sizeof(double)), 'Dimension error');

     iters := -width*sizeof(double);

     asm
        // helper registers for the mt1, mt2 and dest pointers
        mov ecx, mt;
        sub ecx, iters;

        movddup xmm0, cNegMaxDouble;
        movapd xmm3, xmm0;

        // for y := 0 to height - 1:
        mov edx, Height;
        @@addforyloop:
            // for x := 0 to w - 1;
            // prepare for reverse loop
            mov eax, iters;
            @addforxloop:
                add eax, 128;
                jg @loopEnd;

                // max:
                movupd xmm1, [ecx + eax - 128];
                maxpd xmm0, xmm1;
                movupd xmm2, [ecx + eax - 112];
                maxpd xmm3, xmm2;
                movupd xmm1, [ecx + eax - 96];
                maxpd xmm0, xmm1;
                movupd xmm2, [ecx + eax - 80];
                maxpd xmm3, xmm2;
                movupd xmm1, [ecx + eax - 64];
                maxpd xmm0, xmm1;
                movupd xmm2, [ecx + eax - 48];
                maxpd xmm3, xmm2;
                movupd xmm1, [ecx + eax - 32];
                maxpd xmm0, xmm1;
                movupd xmm2, [ecx + eax - 16];
                maxpd xmm3, xmm2;
            jmp @addforxloop

            @loopEnd:

            sub eax, 128;

            jz @nextLine;

            @addforxloop2:
                movupd xmm1, [ecx + eax];
                maxpd xmm0, xmm1;
            add eax, 16;
            jnz @addforxloop2;

            @nextLine:

            // next line:
            add ecx, LineWidth;

        // loop y end
        dec edx;
        jnz @@addforyloop;

        // final max ->
        maxpd xmm0, xmm3;
        movhlps xmm1, xmm0;
        maxsd xmm0, xmm1;
        movsd Result, xmm0;
     end;
end;

function ASMMatrixMaxAlignedOddW(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double;
var iters : TASMNativeInt;
begin
     Assert((Cardinal(mt) and $0000000F = 0), 'Error non aligned data');
     Assert((width and 1) = 1, 'Error width must be even');

     assert((width > 0) and (height > 0) and (LineWidth >= width*sizeof(double)), 'Dimension error');

     iters := -(width - 1)*sizeof(double);

     asm
        // helper registers for the mt1, mt2 and dest pointers
        mov ecx, mt;
        sub ecx, iters;

        movddup xmm0, cNegMaxDouble;
        movapd xmm1, xmm0;
   					movapd xmm2, xmm0;
   					movapd xmm3, xmm0;

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

                // max:
                maxpd xmm0, [ecx + eax - 128];
                maxpd xmm1, [ecx + eax - 112];
                maxpd xmm2, [ecx + eax - 96];
                maxpd xmm3, [ecx + eax - 80];
                maxpd xmm0, [ecx + eax - 64];
                maxpd xmm1, [ecx + eax - 48];
                maxpd xmm2, [ecx + eax - 32];
                maxpd xmm3, [ecx + eax - 16];
            jmp @addforxloop

            @loopEnd:

            sub eax, 128;

            jz @nextLine;

            @addforxloop2:
                maxpd xmm0, [ecx + eax];
            add eax, 16;
            jnz @addforxloop2;

            @nextLine:

            // special care of the last column:
            movsd xmm1, [ecx];
            maxsd xmm0, xmm1;

            // next line:
            add ecx, LineWidth;

        // loop y end
        dec edx;
        jnz @@addforyloop;

        // final max ->
        maxpd xmm0, xmm1;
   					maxpd xmm0, xmm2;
        maxpd xmm0, xmm3;
        movhlps xmm1, xmm0;
        maxsd xmm0, xmm1;
        movsd Result, xmm0;
     end;
end;

function ASMMatrixMaxUnAlignedOddW(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double;
var iters : TASMNativeInt;
begin
     Assert((width and 1) = 1, 'Error width must be even');
     assert((width > 0) and (height > 0) and (LineWidth >= width*sizeof(double)), 'Dimension error');

     iters := -(width - 1)*sizeof(double);

     asm
        // helper registers for the mt1, mt2 and dest pointers
        mov ecx, mt;
        sub ecx, iters;

        movddup xmm0, cNegMaxDouble;
        movapd xmm3, xmm0;

        // for y := 0 to height - 1:
        mov edx, Height;
        @@addforyloop:
            // for x := 0 to w - 1;
            // prepare for reverse loop
            mov eax, iters;
            @addforxloop:
                add eax, 128;
                jg @loopEnd;

                // max:
                movupd xmm1, [ecx + eax - 128];
                Maxpd xmm0, xmm1;
                movupd xmm2, [ecx + eax - 112];
                Maxpd xmm3, xmm2;
                movupd xmm1, [ecx + eax - 96];
                Maxpd xmm0, xmm1;
                movupd xmm2, [ecx + eax - 80];
                Maxpd xmm3, xmm2;
                movupd xmm1, [ecx + eax - 64];
                Maxpd xmm0, xmm1;
                movupd xmm2, [ecx + eax - 48];
                Maxpd xmm3, xmm2;
                movupd xmm1, [ecx + eax - 32];
                Maxpd xmm0, xmm1;
                movupd xmm2, [ecx + eax - 16];
                Maxpd xmm3, xmm2;
            jmp @addforxloop

            @loopEnd:

            sub eax, 128;

            jz @nextLine;

            @addforxloop2:
                movupd xmm1, [ecx + eax];
                Maxpd xmm0, xmm1;
            add eax, 16;
            jnz @addforxloop2;

            @nextLine:

            // special care of the last column:
            movsd xmm1, [ecx];
            Maxsd xmm0, xmm1;

            // next line:
            add ecx, LineWidth;

        // loop y end
        dec edx;
        jnz @@addforyloop;

        // final Max ->
        maxpd xmm0, xmm3;
        movhlps xmm1, xmm0;
        maxsd xmm0, xmm1;
        movsd Result, xmm0;
     end;
end;

function ASMMatrixMinAlignedEvenW(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double;
var iters : TASMNativeInt;
begin
     Assert((Cardinal(mt) and $0000000F = 0), 'Error non aligned data');
     Assert((width and 1) = 0, 'Error width must be even');

     assert((width > 0) and (height > 0) and (LineWidth >= width*sizeof(double)), 'Dimension error');

     iters := -width*sizeof(double);

     asm
        // helper registers for the mt1, mt2 and dest pointers
        mov ecx, mt;
        sub ecx, iters;

        movddup xmm0, cMaxDouble;
        movapd xmm1, xmm0;
   					movapd xmm2, xmm0;
   					movapd xmm3, xmm0;

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

                // min:
                minpd xmm0, [ecx + eax - 128];
                minpd xmm1, [ecx + eax - 112];
                minpd xmm2, [ecx + eax - 96];
                minpd xmm3, [ecx + eax - 80];
                minpd xmm0, [ecx + eax - 64];
                minpd xmm1, [ecx + eax - 48];
                minpd xmm2, [ecx + eax - 32];
                minpd xmm3, [ecx + eax - 16];
            jmp @addforxloop

            @loopEnd:

            sub eax, 128;

            jz @nextLine;

            @addforxloop2:
                minpd xmm0, [ecx + eax];
            add eax, 16;
            jnz @addforxloop2;

            @nextLine:

            // next line:
            add ecx, LineWidth;

        // loop y end
        dec edx;
        jnz @@addforyloop;

        // final min ->
        minpd xmm0, xmm1;
   					minpd xmm0, xmm2;
        minpd xmm0, xmm3;
        movhlps xmm1, xmm0;
        minsd xmm0, xmm1;
        movsd Result, xmm0;
     end;
end;

function ASMMatrixMinUnAlignedEvenW(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double;
var iters : TASMNativeInt;
begin
     Assert((width and 1) = 0, 'Error width must be even');
     assert((width > 0) and (height > 0) and (LineWidth >= width*sizeof(double)), 'Dimension error');

     iters := -width*sizeof(double);

     asm
        // helper registers for the mt1, mt2 and dest pointers
        mov ecx, mt;
        sub ecx, iters;

        movddup xmm0, cMaxDouble;
        movapd xmm3, xmm0;

        // for y := 0 to height - 1:
        mov edx, Height;
        @@addforyloop:
            // for x := 0 to w - 1;
            // prepare for reverse loop
            mov eax, iters;
            @addforxloop:
                add eax, 128;
                jg @loopEnd;

                // min:
                movupd xmm1, [ecx + eax - 128];
                minpd xmm0, xmm1;
                movupd xmm2, [ecx + eax - 112];
                minpd xmm3, xmm2;
                movupd xmm1, [ecx + eax - 96];
                minpd xmm0, xmm1;
                movupd xmm2, [ecx + eax - 80];
                minpd xmm3, xmm2;
                movupd xmm1, [ecx + eax - 64];
                minpd xmm0, xmm1;
                movupd xmm2, [ecx + eax - 48];
                minpd xmm3, xmm2;
                movupd xmm1, [ecx + eax - 32];
                minpd xmm0, xmm1;
                movupd xmm2, [ecx + eax - 16];
                minpd xmm3, xmm2;
            jmp @addforxloop

            @loopEnd:

            sub eax, 128;

            jz @nextLine;

            @addforxloop2:
                movupd xmm1, [ecx + eax];
                minpd xmm0, xmm1;
            add eax, 16;
            jnz @addforxloop2;

            @nextLine:

            // next line:
            add ecx, LineWidth;

        // loop y end
        dec edx;
        jnz @@addforyloop;

        // final min ->
        minpd xmm0, xmm3;
        movhlps xmm1, xmm0;
        minsd xmm0, xmm1;
        movsd Result, xmm0;
     end;
end;

function ASMMatrixMinAlignedOddW(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double;
var iters : TASMNativeInt;
begin
     Assert((Cardinal(mt) and $0000000F = 0), 'Error non aligned data');
     Assert((width and 1) = 1, 'Error width must be even');

     assert((width > 0) and (height > 0) and (LineWidth >= width*sizeof(double)), 'Dimension error');

     iters := -(width - 1)*sizeof(double);

     asm
        // helper registers for the mt1, mt2 and dest pointers
        mov ecx, mt;
        sub ecx, iters;

        movddup xmm0, cMaxDouble;
        movapd xmm1, xmm0;
   					movapd xmm2, xmm0;
   					movapd xmm3, xmm0;

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

                // min:
                minpd xmm0, [ecx + eax - 128];
                minpd xmm1, [ecx + eax - 112];
                minpd xmm2, [ecx + eax - 96];
                minpd xmm3, [ecx + eax - 80];
                minpd xmm0, [ecx + eax - 64];
                minpd xmm1, [ecx + eax - 48];
                minpd xmm2, [ecx + eax - 32];
                minpd xmm3, [ecx + eax - 16];
            jmp @addforxloop

            @loopEnd:

            sub eax, 128;

            jz @nextLine;

            @addforxloop2:
                minpd xmm0, [ecx + eax];
            add eax, 16;
            jnz @addforxloop2;

            @nextLine:

            // special care of the last column:
            movsd xmm1, [ecx];
            minsd xmm0, xmm1;

            // next line:
            add ecx, LineWidth;

        // loop y end
        dec edx;
        jnz @@addforyloop;

        // final Max ->
        minpd xmm0, xmm1;
   					minpd xmm0, xmm2;
        minpd xmm0, xmm3;
        movhlps xmm1, xmm0;
        minsd xmm0, xmm1;
        movsd Result, xmm0;
     end;
end;

function ASMMatrixMinUnAlignedOddW(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double;
var iters : TASMNativeInt;
begin
     Assert((width and 1) = 1, 'Error width must be odd');
     assert((width > 0) and (height > 0) and (LineWidth >= width*sizeof(double)), 'Dimension error');

     iters := -(width - 1)*sizeof(double);

     asm
        // helper registers for the mt1, mt2 and dest pointers
        mov ecx, mt;
        sub ecx, iters;

        movddup xmm0, cMaxDouble;
        movapd xmm3, xmm0;

        // for y := 0 to height - 1:
        mov edx, Height;
        @@addforyloop:
            // for x := 0 to w - 1;
            // prepare for reverse loop
            mov eax, iters;
            @addforxloop:
                add eax, 128;
                jg @loopEnd;

                // min:
                movupd xmm1, [ecx + eax - 128];
                minpd xmm0, xmm1;
                movupd xmm2, [ecx + eax - 112];
                minpd xmm3, xmm2;
                movupd xmm1, [ecx + eax - 96];
                minpd xmm0, xmm1;
                movupd xmm2, [ecx + eax - 80];
                minpd xmm3, xmm2;
                movupd xmm1, [ecx + eax - 64];
                minpd xmm0, xmm1;
                movupd xmm2, [ecx + eax - 48];
                minpd xmm3, xmm2;
                movupd xmm1, [ecx + eax - 32];
                minpd xmm0, xmm1;
                movupd xmm2, [ecx + eax - 16];
                minpd xmm3, xmm2;
            jmp @addforxloop

            @loopEnd:

            sub eax, 128;

            jz @nextLine;

            @addforxloop2:
                movupd xmm1, [ecx + eax];
                minpd xmm0, xmm1;
            add eax, 16;
            jnz @addforxloop2;

            @nextLine:

            // special care of the last column:
            movsd xmm1, [ecx];
            minsd xmm0, xmm1;

            // next line:
            add ecx, LineWidth;

        // loop y end
        dec edx;
        jnz @@addforyloop;

        // final min ->
        minpd xmm0, xmm3;
        movhlps xmm1, xmm0;
        minsd xmm0, xmm1;
        movsd Result, xmm0;
     end;
end;

{$ENDIF}

end.
