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


unit ASMMatrixTransposeOperations;

interface

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}
{$IFNDEF x64}

uses MatrixConst;

procedure ASMMatrixTransposeAlignedEvenWEvenH(dest : PDouble; const destLineWidth : TASMNativeInt; mt : PDouble; const LineWidth : TASMNativeInt; width : TASMNativeInt; height : TASMNativeInt);
procedure ASMMatrixTransposeUnAlignedEvenWEvenH(dest : PDouble; const destLineWidth : TASMNativeInt; mt : PDouble; const LineWidth : TASMNativeInt; width : TASMNativeInt; height : TASMNativeInt);

procedure ASMMatrixTransposeAlignedEvenWOddH(dest : PDouble; const destLineWidth : TASMNativeInt; mt : PDouble; const LineWidth : TASMNativeInt; width : TASMNativeInt; height : TASMNativeInt);
procedure ASMMatrixTransposeUnAlignedEvenWOddH(dest : PDouble; const destLineWidth : TASMNativeInt; mt : PDouble; const LineWidth : TASMNativeInt; width : TASMNativeInt; height : TASMNativeInt);

procedure ASMMatrixTransposeAlignedOddWEvenH(dest : PDouble; const destLineWidth : TASMNativeInt; mt : PDouble; const LineWidth : TASMNativeInt; width : TASMNativeInt; height : TASMNativeInt);
procedure ASMMatrixTransposeUnAlignedOddWEvenH(dest : PDouble; const destLineWidth : TASMNativeInt; mt : PDouble; const LineWidth : TASMNativeInt; width : TASMNativeInt; height : TASMNativeInt);

procedure ASMMatrixTransposeAlignedOddWOddH(dest : PDouble; const destLineWidth : TASMNativeInt; mt : PDouble; const LineWidth : TASMNativeInt; width : TASMNativeInt; height : TASMNativeInt);
procedure ASMMatrixTransposeUnAlignedOddWOddH(dest : PDouble; const destLineWidth : TASMNativeInt; mt : PDouble; const LineWidth : TASMNativeInt; width : TASMNativeInt; height : TASMNativeInt);

procedure ASMMatrixTransposeInplace(mt : PDouble; const LineWidth : TASMNativeInt; N : TASMNativeInt);

{$ENDIF}

implementation

{$IFNDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$S-} {$ENDIF}

procedure ASMMatrixTransposeAlignedEvenWEvenH(dest : PDouble; const destLineWidth : TASMNativeInt; mt : PDouble; const LineWidth : TASMNativeInt; width : TASMNativeInt; height : TASMNativeInt);
var iters : TASMNativeInt;
    destLineWidth2 : TASMNativeInt;
    y : TASMNativeInt;
begin
     Assert((Cardinal(dest) and $0000000F = 0) and (Cardinal(mt) and $0000000F = 0), 'Error non aligned data');
     assert((width > 1) and (height > 1) and (LineWidth >= width*sizeof(double)), 'Dimension error');
     assert(((width and 1) = 0) and ((height and 1) = 0), 'Error width and height must be even');

     destLineWidth2 := 2*destLineWidth;
     iters := -width*sizeof(double);
     y := height;
     asm
        push esi;
        push edi;
        push ebx;

        mov edx, destLineWidth2;
        mov ebx, destLineWidth;

        @foryloop:
            mov ecx, dest;

            mov esi, mt;
            mov edi, esi;
            sub esi, iters;
            add edi, LineWidth;
            sub edi, iters;

            // unrolled loop
            mov eax, iters;
            @forxloop:
                add eax, 128;
                jg @loopend;

                // prefetch [esi + eax];
                // prefetch [edi + eax];

                movapd xmm0, [esi + eax - 128];
                movapd xmm1, [edi + eax - 128];

                movapd xmm2, xmm0;
                movlhps xmm0, xmm1;
                movhlps xmm1, xmm2;

                movapd [ecx], xmm0;
                movapd [ecx + ebx], xmm1;

                add ecx, edx;

                movapd xmm0, [esi + eax - 112];
                movapd xmm1, [edi + eax - 112];

                movapd xmm2, xmm0;
                movlhps xmm0, xmm1;
                movhlps xmm1, xmm2;

                movapd [ecx], xmm0;
                movapd [ecx + ebx], xmm1;

                add ecx, edx;

                movapd xmm0, [esi + eax - 96];
                movapd xmm1, [edi + eax - 96];

                movapd xmm2, xmm0;
                movlhps xmm0, xmm1;
                movhlps xmm1, xmm2;

                movapd [ecx], xmm0;
                movapd [ecx + ebx], xmm1;

                add ecx, edx;

                movapd xmm0, [esi + eax - 80];
                movapd xmm1, [edi + eax - 80];

                movapd xmm2, xmm0;
                movlhps xmm0, xmm1;
                movhlps xmm1, xmm2;

                movapd [ecx], xmm0;
                movapd [ecx + ebx], xmm1;

                add ecx, edx;

                movapd xmm0, [esi + eax - 64];
                movapd xmm1, [edi + eax - 64];

                movapd xmm2, xmm0;
                movlhps xmm0, xmm1;
                movhlps xmm1, xmm2;

                movapd [ecx], xmm0;
                movapd [ecx + ebx], xmm1;

                add ecx, edx;

                movapd xmm0, [esi + eax - 48];
                movapd xmm1, [edi + eax - 48];

                movapd xmm2, xmm0;
                movlhps xmm0, xmm1;
                movhlps xmm1, xmm2;

                movapd [ecx], xmm0;
                movapd [ecx + ebx], xmm1;

                add ecx, edx;

                movapd xmm0, [esi + eax - 32];
                movapd xmm1, [edi + eax - 32];

                movapd xmm2, xmm0;
                movlhps xmm0, xmm1;
                movhlps xmm1, xmm2;

                movapd [ecx], xmm0;
                movapd [ecx + ebx], xmm1;

                add ecx, edx;

                movapd xmm0, [esi + eax - 16];
                movapd xmm1, [edi + eax - 16];

                movapd xmm2, xmm0;
                movlhps xmm0, xmm1;
                movhlps xmm1, xmm2;

                movapd [ecx], xmm0;
                movapd [ecx + ebx], xmm1;

                add ecx, edx;

            jmp @forxloop;

            @loopend:

            sub eax, 128;

            jz @nextLine;

            @forxloop2:
                movapd xmm0, [esi + eax];
                movapd xmm1, [edi + eax];

                movapd xmm2, xmm0;
                movlhps xmm0, xmm1;
                movhlps xmm1, xmm2;

                movapd [ecx], xmm0;
                movapd [ecx + ebx], xmm1;

                add ecx, destLineWidth2;
            add eax, 16;
            jnz @forxloop2;

            @nextLine:

            // increment pointers
            mov eax, dest;
            add eax, 16;
            mov dest, eax;

            mov eax, mt;
            add eax, LineWidth;
            add eax, LineWidth;
            mov mt, eax;

            sub y, 2;
        jnz @foryloop;

        pop ebx;
        pop edi;
        pop esi;
     end;
end;

procedure ASMMatrixTransposeUnAlignedEvenWEvenH(dest : PDouble; const destLineWidth : TASMNativeInt; mt : PDouble; const LineWidth : TASMNativeInt; width : TASMNativeInt; height : TASMNativeInt);
var iters : TASMNativeInt;
    destLineWidth2 : TASMNativeInt;
    y : TASMNativeInt;
begin
     assert((width > 1) and (height > 1) and (LineWidth >= width*sizeof(double)), 'Dimension error');
     assert(((width and 1) = 0) and ((height and 1) = 0), 'Error width and height must be even');

     destLineWidth2 := 2*destLineWidth;
     iters := -width*sizeof(double);
     y := height;
     asm
        push esi;
        push edi;
        push ebx;

        mov edx, destLineWidth2;
        mov ebx, destLineWidth;

        @foryloop:
            mov ecx, dest;

            mov esi, mt;
            mov edi, esi;
            sub esi, iters;
            add edi, LineWidth;
            sub edi, iters;

            // unrolled loop
            mov eax, iters;
            @forxloop:
                add eax, 128;
                jg @loopend;

                movupd xmm0, [esi + eax - 128];
                movupd xmm1, [edi + eax - 128];

                movupd xmm2, xmm0;
                movlhps xmm0, xmm1;
                movhlps xmm1, xmm2;

                movupd [ecx], xmm0;
                movupd [ecx + ebx], xmm1;

                add ecx, edx;

                movupd xmm0, [esi + eax - 112];
                movupd xmm1, [edi + eax - 112];

                movupd xmm2, xmm0;
                movlhps xmm0, xmm1;
                movhlps xmm1, xmm2;

                movupd [ecx], xmm0;
                movupd [ecx + ebx], xmm1;

                add ecx, edx;

                movupd xmm0, [esi + eax - 96];
                movupd xmm1, [edi + eax - 96];

                movupd xmm2, xmm0;
                movlhps xmm0, xmm1;
                movhlps xmm1, xmm2;

                movupd [ecx], xmm0;
                movupd [ecx + ebx], xmm1;

                add ecx, edx;

                movupd xmm0, [esi + eax - 80];
                movupd xmm1, [edi + eax - 80];

                movupd xmm2, xmm0;
                movlhps xmm0, xmm1;
                movhlps xmm1, xmm2;

                movupd [ecx], xmm0;
                movupd [ecx + ebx], xmm1;

                add ecx, edx;

                movupd xmm0, [esi + eax - 64];
                movupd xmm1, [edi + eax - 64];

                movupd xmm2, xmm0;
                movlhps xmm0, xmm1;
                movhlps xmm1, xmm2;

                movupd [ecx], xmm0;
                movupd [ecx + ebx], xmm1;

                add ecx, edx;

                movupd xmm0, [esi + eax - 48];
                movupd xmm1, [edi + eax - 48];

                movupd xmm2, xmm0;
                movlhps xmm0, xmm1;
                movhlps xmm1, xmm2;

                movupd [ecx], xmm0;
                movupd [ecx + ebx], xmm1;

                add ecx, edx;

                movupd xmm0, [esi + eax - 32];
                movupd xmm1, [edi + eax - 32];

                movupd xmm2, xmm0;
                movlhps xmm0, xmm1;
                movhlps xmm1, xmm2;

                movupd [ecx], xmm0;
                movupd [ecx + ebx], xmm1;

                add ecx, edx;

                movupd xmm0, [esi + eax - 16];
                movupd xmm1, [edi + eax - 16];

                movupd xmm2, xmm0;
                movlhps xmm0, xmm1;
                movhlps xmm1, xmm2;

                movupd [ecx], xmm0;
                movupd [ecx + ebx], xmm1;

                add ecx, edx;

            jmp @forxloop;

            @loopend:

            sub eax, 128;

            jz @nextLine;

            @forxloop2:
                movupd xmm0, [esi + eax];
                movupd xmm1, [edi + eax];

                movupd xmm2, xmm0;
                movlhps xmm0, xmm1;
                movhlps xmm1, xmm2;

                movupd [ecx], xmm0;
                movupd [ecx + ebx], xmm1;

                add ecx, destLineWidth2;
            add eax, 16;
            jnz @forxloop2;

            @nextLine:

            // increment pointers
            mov eax, dest;
            add eax, 16;
            mov dest, eax;

            mov eax, mt;
            add eax, LineWidth;
            add eax, LineWidth;
            mov mt, eax;

            sub y, 2;
        jnz @foryloop;

        pop ebx;
        pop edi;
        pop esi;
     end;
end;

procedure ASMMatrixTransposeAlignedOddWEvenH(dest : PDouble; const destLineWidth : TASMNativeInt; mt : PDouble; const LineWidth : TASMNativeInt; width : TASMNativeInt; height : TASMNativeInt);
var iters : TASMNativeInt;
    destLineWidth2 : TASMNativeInt;
    y : TASMNativeInt;
begin
     Assert((Cardinal(dest) and $0000000F = 0) and (Cardinal(mt) and $0000000F = 0), 'Error non aligned data');
     assert((width > 1) and (height > 1) and (LineWidth >= width*sizeof(double)), 'Dimension error');
     assert(((width and 1) = 1) and ((height and 1) = 0), 'Error width and height must be even');

     destLineWidth2 := 2*destLineWidth;
     iters := -(width - 1)*sizeof(double);
     y := height;
     asm
        push esi;
        push edi;
        push ebx;

        mov edx, destLineWidth2;
        mov ebx, destLineWidth;

        @foryloop:
            mov ecx, dest;

            mov esi, mt;
            mov edi, esi;
            sub esi, iters;
            add edi, LineWidth;
            sub edi, iters;

            // unrolled loop
            mov eax, iters;
            @forxloop:
                add eax, 128;
                jg @loopend;

                // prefetch [esi + eax];
                // prefetch [edi + eax];

                movapd xmm0, [esi + eax - 128];
                movapd xmm1, [edi + eax - 128];

                movapd xmm2, xmm0;
                movlhps xmm0, xmm1;
                movhlps xmm1, xmm2;

                movapd [ecx], xmm0;
                movapd [ecx + ebx], xmm1;

                add ecx, edx;

                movapd xmm0, [esi + eax - 112];
                movapd xmm1, [edi + eax - 112];

                movapd xmm2, xmm0;
                movlhps xmm0, xmm1;
                movhlps xmm1, xmm2;

                movapd [ecx], xmm0;
                movapd [ecx + ebx], xmm1;

                add ecx, edx;

                movapd xmm0, [esi + eax - 96];
                movapd xmm1, [edi + eax - 96];

                movapd xmm2, xmm0;
                movlhps xmm0, xmm1;
                movhlps xmm1, xmm2;

                movapd [ecx], xmm0;
                movapd [ecx + ebx], xmm1;

                add ecx, edx;

                movapd xmm0, [esi + eax - 80];
                movapd xmm1, [edi + eax - 80];

                movapd xmm2, xmm0;
                movlhps xmm0, xmm1;
                movhlps xmm1, xmm2;

                movapd [ecx], xmm0;
                movapd [ecx + ebx], xmm1;

                add ecx, edx;

                movapd xmm0, [esi + eax - 64];
                movapd xmm1, [edi + eax - 64];

                movapd xmm2, xmm0;
                movlhps xmm0, xmm1;
                movhlps xmm1, xmm2;

                movapd [ecx], xmm0;
                movapd [ecx + ebx], xmm1;

                add ecx, edx;

                movapd xmm0, [esi + eax - 48];
                movapd xmm1, [edi + eax - 48];

                movapd xmm2, xmm0;
                movlhps xmm0, xmm1;
                movhlps xmm1, xmm2;

                movapd [ecx], xmm0;
                movapd [ecx + ebx], xmm1;

                add ecx, edx;

                movapd xmm0, [esi + eax - 32];
                movapd xmm1, [edi + eax - 32];

                movapd xmm2, xmm0;
                movlhps xmm0, xmm1;
                movhlps xmm1, xmm2;

                movapd [ecx], xmm0;
                movapd [ecx + ebx], xmm1;

                add ecx, edx;

                movapd xmm0, [esi + eax - 16];
                movapd xmm1, [edi + eax - 16];

                movapd xmm2, xmm0;
                movlhps xmm0, xmm1;
                movhlps xmm1, xmm2;

                movapd [ecx], xmm0;
                movapd [ecx + ebx], xmm1;

                add ecx, edx;

            jmp @forxloop;

            @loopend:

            sub eax, 128;

            jz @nextLine;

            @forxloop2:
                movapd xmm0, [esi + eax];
                movapd xmm1, [edi + eax];

                movapd xmm2, xmm0;
                movlhps xmm0, xmm1;
                movhlps xmm1, xmm2;

                movapd [ecx], xmm0;
                movapd [ecx + ebx], xmm1;

                add ecx, destLineWidth2;
            add eax, 16;
            jnz @forxloop2;

            @nextLine:

            // handle last element differently
            movsd xmm0, [esi + eax];
            movsd xmm1, [edi + eax];

            movlhps xmm0, xmm1;
            movapd [ecx], xmm0;

            // increment pointers
            mov eax, dest;
            add eax, 16;
            mov dest, eax;

            mov eax, mt;
            add eax, LineWidth;
            add eax, LineWidth;
            mov mt, eax;

            sub y, 2;
        jnz @foryloop;

        pop ebx;
        pop edi;
        pop esi;
     end;
end;

procedure ASMMatrixTransposeUnAlignedOddWEvenH(dest : PDouble; const destLineWidth : TASMNativeInt; mt : PDouble; const LineWidth : TASMNativeInt; width : TASMNativeInt; height : TASMNativeInt);
var iters : TASMNativeInt;
    destLineWidth2 : TASMNativeInt;
    y : TASMNativeInt;
begin
     assert((width > 1) and (height > 1) and (LineWidth >= width*sizeof(double)), 'Dimension error');
     assert(((width and 1) = 1) and ((height and 1) = 0), 'Error width and height must be even');

     destLineWidth2 := 2*destLineWidth;
     iters := -(width - 1)*sizeof(double);
     y := height;
     asm
        push esi;
        push edi;
        push ebx;

        mov edx, destLineWidth2;
        mov ebx, destLineWidth;
        
        @foryloop:
            mov ecx, dest;

            mov esi, mt;
            mov edi, esi;
            sub esi, iters;
            add edi, LineWidth;
            sub edi, iters;

            // unrolled loop
            mov eax, iters;
            @forxloop:
                add eax, 128;
                jg @loopend;

                movupd xmm0, [esi + eax - 128];
                movupd xmm1, [edi + eax - 128];

                movupd xmm2, xmm0;
                movlhps xmm0, xmm1;
                movhlps xmm1, xmm2;

                movupd [ecx], xmm0;
                movupd [ecx + ebx], xmm1;

                add ecx, edx;

                movupd xmm0, [esi + eax - 112];
                movupd xmm1, [edi + eax - 112];

                movupd xmm2, xmm0;
                movlhps xmm0, xmm1;
                movhlps xmm1, xmm2;

                movupd [ecx], xmm0;
                movupd [ecx + ebx], xmm1;

                add ecx, edx;

                movupd xmm0, [esi + eax - 96];
                movupd xmm1, [edi + eax - 96];

                movupd xmm2, xmm0;
                movlhps xmm0, xmm1;
                movhlps xmm1, xmm2;

                movupd [ecx], xmm0;
                movupd [ecx + ebx], xmm1;

                add ecx, edx;

                movupd xmm0, [esi + eax - 80];
                movupd xmm1, [edi + eax - 80];

                movupd xmm2, xmm0;
                movlhps xmm0, xmm1;
                movhlps xmm1, xmm2;

                movupd [ecx], xmm0;
                movupd [ecx + ebx], xmm1;

                add ecx, edx;

                movupd xmm0, [esi + eax - 64];
                movupd xmm1, [edi + eax - 64];

                movupd xmm2, xmm0;
                movlhps xmm0, xmm1;
                movhlps xmm1, xmm2;

                movupd [ecx], xmm0;
                movupd [ecx + ebx], xmm1;

                add ecx, edx;

                movupd xmm0, [esi + eax - 48];
                movupd xmm1, [edi + eax - 48];

                movupd xmm2, xmm0;
                movlhps xmm0, xmm1;
                movhlps xmm1, xmm2;

                movupd [ecx], xmm0;
                movupd [ecx + ebx], xmm1;

                add ecx, edx;

                movupd xmm0, [esi + eax - 32];
                movupd xmm1, [edi + eax - 32];

                movupd xmm2, xmm0;
                movlhps xmm0, xmm1;
                movhlps xmm1, xmm2;

                movupd [ecx], xmm0;
                movupd [ecx + ebx], xmm1;

                add ecx, edx;

                movupd xmm0, [esi + eax - 16];
                movupd xmm1, [edi + eax - 16];

                movupd xmm2, xmm0;
                movlhps xmm0, xmm1;
                movhlps xmm1, xmm2;

                movupd [ecx], xmm0;
                movupd [ecx + ebx], xmm1;

                add ecx, edx;

            jmp @forxloop;

            @loopend:

            sub eax, 128;

            jz @nextLine;

            @forxloop2:
                movupd xmm0, [esi + eax];
                movupd xmm1, [edi + eax];

                movupd xmm2, xmm0;
                movlhps xmm0, xmm1;
                movhlps xmm1, xmm2;

                movupd [ecx], xmm0;
                movupd [ecx + ebx], xmm1;

                add ecx, destLineWidth2;
            add eax, 16;
            jnz @forxloop2;

            @nextLine:

            // handle last element differently
            movsd xmm0, [esi + eax];
            movsd xmm1, [edi + eax];

            movlhps xmm0, xmm1;
            movupd [ecx], xmm0;

            // increment pointers
            mov eax, dest;
            add eax, 16;
            mov dest, eax;

            mov eax, mt;
            add eax, LineWidth;
            add eax, LineWidth;
            mov mt, eax;

            sub y, 2;
        jnz @foryloop;

        pop ebx;
        pop edi;
        pop esi;
     end;
end;

procedure ASMMatrixTransposeAlignedEvenWOddH(dest : PDouble; const destLineWidth : TASMNativeInt; mt : PDouble; const LineWidth : TASMNativeInt; width : TASMNativeInt; height : TASMNativeInt);
var iters : TASMNativeInt;
    destLineWidth2 : TASMNativeInt;
    y : TASMNativeInt;
begin
     Assert((Cardinal(dest) and $0000000F = 0) and (Cardinal(mt) and $0000000F = 0), 'Error non aligned data');
     assert((width > 1) and (height > 1) and (LineWidth >= width*sizeof(double)), 'Dimension error');
     assert(((width and 1) = 0) and ((height and 1) = 1), 'Error width and height must be even');

     destLineWidth2 := 2*destLineWidth;
     iters := -(width)*sizeof(double);
     y := height - 1;
     asm
        push esi;
        push edi;
        push ebx;

        mov edx, destLineWidth2;
        mov ebx, destLineWidth;

        @foryloop:
            mov ecx, dest;

            mov esi, mt;
            mov edi, esi;
            sub esi, iters;
            add edi, LineWidth;
            sub edi, iters;

            // unrolled loop
            mov eax, iters;
            @forxloop:
                add eax, 128;
                jg @loopend;

                // prefetch [esi + eax];
                // prefetch [edi + eax];

                movapd xmm0, [esi + eax - 128];
                movapd xmm1, [edi + eax - 128];

                movapd xmm2, xmm0;
                movlhps xmm0, xmm1;
                movhlps xmm1, xmm2;

                movapd [ecx], xmm0;
                movapd [ecx + ebx], xmm1;

                add ecx, edx;

                movapd xmm0, [esi + eax - 112];
                movapd xmm1, [edi + eax - 112];

                movapd xmm2, xmm0;
                movlhps xmm0, xmm1;
                movhlps xmm1, xmm2;

                movapd [ecx], xmm0;
                movapd [ecx + ebx], xmm1;

                add ecx, edx;

                movapd xmm0, [esi + eax - 96];
                movapd xmm1, [edi + eax - 96];

                movapd xmm2, xmm0;
                movlhps xmm0, xmm1;
                movhlps xmm1, xmm2;

                movapd [ecx], xmm0;
                movapd [ecx + ebx], xmm1;

                add ecx, edx;

                movapd xmm0, [esi + eax - 80];
                movapd xmm1, [edi + eax - 80];

                movapd xmm2, xmm0;
                movlhps xmm0, xmm1;
                movhlps xmm1, xmm2;

                movapd [ecx], xmm0;
                movapd [ecx + ebx], xmm1;

                add ecx, edx;

                movapd xmm0, [esi + eax - 64];
                movapd xmm1, [edi + eax - 64];

                movapd xmm2, xmm0;
                movlhps xmm0, xmm1;
                movhlps xmm1, xmm2;

                movapd [ecx], xmm0;
                movapd [ecx + ebx], xmm1;

                add ecx, edx;

                movapd xmm0, [esi + eax - 48];
                movapd xmm1, [edi + eax - 48];

                movapd xmm2, xmm0;
                movlhps xmm0, xmm1;
                movhlps xmm1, xmm2;

                movapd [ecx], xmm0;
                movapd [ecx + ebx], xmm1;

                add ecx, edx;

                movapd xmm0, [esi + eax - 32];
                movapd xmm1, [edi + eax - 32];

                movapd xmm2, xmm0;
                movlhps xmm0, xmm1;
                movhlps xmm1, xmm2;

                movapd [ecx], xmm0;
                movapd [ecx + ebx], xmm1;

                add ecx, edx;

                movapd xmm0, [esi + eax - 16];
                movapd xmm1, [edi + eax - 16];

                movapd xmm2, xmm0;
                movlhps xmm0, xmm1;
                movhlps xmm1, xmm2;

                movapd [ecx], xmm0;
                movapd [ecx + ebx], xmm1;

                add ecx, edx;

            jmp @forxloop;

            @loopend:

            sub eax, 128;

            jz @nextLine;

            @forxloop2:
                movapd xmm0, [esi + eax];
                movapd xmm1, [edi + eax];

                movapd xmm2, xmm0;
                movlhps xmm0, xmm1;
                movhlps xmm1, xmm2;

                movapd [ecx], xmm0;
                movapd [ecx + ebx], xmm1;

                add ecx, destLineWidth2;
            add eax, 16;
            jnz @forxloop2;

            @nextLine:

            // increment pointers
            mov eax, dest;
            add eax, 16;
            mov dest, eax;

            mov eax, mt;
            add eax, LineWidth;
            add eax, LineWidth;
            mov mt, eax;

            sub y, 2;
        jnz @foryloop;

        // handle last line differently
        mov ecx, dest;

        mov esi, mt;
        sub esi, iters;

        mov eax, iters;
        @forxloop3:
            movapd xmm0, [esi + eax];

            movhlps xmm1, xmm0;
            movsd [ecx], xmm0;
            movsd [ecx + ebx], xmm1;

            add ecx, edx;
        add eax, 16;
        jnz @forxloop3;

        pop ebx;
        pop edi;
        pop esi;
     end;
end;

procedure ASMMatrixTransposeUnAlignedEvenWOddH(dest : PDouble; const destLineWidth : TASMNativeInt; mt : PDouble; const LineWidth : TASMNativeInt; width : TASMNativeInt; height : TASMNativeInt);
var iters : TASMNativeInt;
    destLineWidth2 : TASMNativeInt;
    y : TASMNativeInt;
begin
     assert((width > 1) and (height > 1) and (LineWidth >= width*sizeof(double)), 'Dimension error');
     assert(((width and 1) = 0) and ((height and 1) = 1), 'Error width and height must be even');

     destLineWidth2 := 2*destLineWidth;
     iters := -(width)*sizeof(double);
     y := height - 1;
     asm
        push esi;
        push edi;
        push ebx;

        mov edx, destLineWidth2;
        mov ebx, destLineWidth;

        @foryloop:
            mov ecx, dest;

            mov esi, mt;
            mov edi, esi;
            sub esi, iters;
            add edi, LineWidth;
            sub edi, iters;

            // unrolled loop
            mov eax, iters;
            @forxloop:
                add eax, 128;
                jg @loopend;

                movupd xmm0, [esi + eax - 128];
                movupd xmm1, [edi + eax - 128];

                movupd xmm2, xmm0;
                movlhps xmm0, xmm1;
                movhlps xmm1, xmm2;

                movupd [ecx], xmm0;
                movupd [ecx + ebx], xmm1;

                add ecx, edx;

                movupd xmm0, [esi + eax - 112];
                movupd xmm1, [edi + eax - 112];

                movupd xmm2, xmm0;
                movlhps xmm0, xmm1;
                movhlps xmm1, xmm2;

                movupd [ecx], xmm0;
                movupd [ecx + ebx], xmm1;

                add ecx, edx;

                movupd xmm0, [esi + eax - 96];
                movupd xmm1, [edi + eax - 96];

                movupd xmm2, xmm0;
                movlhps xmm0, xmm1;
                movhlps xmm1, xmm2;

                movupd [ecx], xmm0;
                movupd [ecx + ebx], xmm1;

                add ecx, edx;

                movupd xmm0, [esi + eax - 80];
                movupd xmm1, [edi + eax - 80];

                movupd xmm2, xmm0;
                movlhps xmm0, xmm1;
                movhlps xmm1, xmm2;

                movupd [ecx], xmm0;
                movupd [ecx + ebx], xmm1;

                add ecx, edx;

                movupd xmm0, [esi + eax - 64];
                movupd xmm1, [edi + eax - 64];

                movupd xmm2, xmm0;
                movlhps xmm0, xmm1;
                movhlps xmm1, xmm2;

                movupd [ecx], xmm0;
                movupd [ecx + ebx], xmm1;

                add ecx, edx;

                movupd xmm0, [esi + eax - 48];
                movupd xmm1, [edi + eax - 48];

                movupd xmm2, xmm0;
                movlhps xmm0, xmm1;
                movhlps xmm1, xmm2;

                movupd [ecx], xmm0;
                movupd [ecx + ebx], xmm1;

                add ecx, edx;

                movupd xmm0, [esi + eax - 32];
                movupd xmm1, [edi + eax - 32];

                movupd xmm2, xmm0;
                movlhps xmm0, xmm1;
                movhlps xmm1, xmm2;

                movupd [ecx], xmm0;
                movupd [ecx + ebx], xmm1;

                add ecx, edx;

                movupd xmm0, [esi + eax - 16];
                movupd xmm1, [edi + eax - 16];

                movupd xmm2, xmm0;
                movlhps xmm0, xmm1;
                movhlps xmm1, xmm2;

                movupd [ecx], xmm0;
                movupd [ecx + ebx], xmm1;

                add ecx, edx;

            jmp @forxloop;

            @loopend:

            sub eax, 128;

            jz @nextLine;

            @forxloop2:
                movupd xmm0, [esi + eax];
                movupd xmm1, [edi + eax];

                movupd xmm2, xmm0;
                movlhps xmm0, xmm1;
                movhlps xmm1, xmm2;

                movupd [ecx], xmm0;
                movupd [ecx + ebx], xmm1;

                add ecx, destLineWidth2;
            add eax, 16;
            jnz @forxloop2;

            @nextLine:

            // increment pointers
            mov eax, dest;
            add eax, 16;
            mov dest, eax;

            mov eax, mt;
            add eax, LineWidth;
            add eax, LineWidth;
            mov mt, eax;

            sub y, 2;
        jnz @foryloop;

        // handle last line differently
        mov ecx, dest;

        mov esi, mt;
        sub esi, iters;

        mov eax, iters;
        @forxloop3:
            movupd xmm0, [esi + eax];

            movhlps xmm1, xmm0;
            movsd [ecx], xmm0;
            movsd [ecx + ebx], xmm1;

            add ecx, edx;
        add eax, 16;
        jnz @forxloop3;

        pop ebx;
        pop edi;
        pop esi;
     end;
end;

procedure ASMMatrixTransposeAlignedOddWOddH(dest : PDouble; const destLineWidth : TASMNativeInt; mt : PDouble; const LineWidth : TASMNativeInt; width : TASMNativeInt; height : TASMNativeInt);
var iters : TASMNativeInt;
    destLineWidth2 : TASMNativeInt;
    y : TASMNativeInt;
begin
     Assert((Cardinal(dest) and $0000000F = 0) and (Cardinal(mt) and $0000000F = 0), 'Error non aligned data');
     assert((width > 1) and (height > 1) and (LineWidth >= width*sizeof(double)), 'Dimension error');
     assert(((width and 1) = 1) and ((height and 1) = 1), 'Error width and height must be even');

     destLineWidth2 := 2*destLineWidth;
     iters := -(width - 1)*sizeof(double);
     y := height - 1;
     asm
        push esi;
        push edi;
        push ebx;

        mov edx, destLineWidth2;
        mov ebx, destLineWidth;

        @foryloop:
            mov ecx, dest;

            mov esi, mt;
            mov edi, esi;
            sub esi, iters;
            add edi, LineWidth;
            sub edi, iters;

            // unrolled loop
            mov eax, iters;
            @forxloop:
                add eax, 128;
                jg @loopend;

                // prefetch [esi + eax];
                // prefetch [edi + eax];

                movapd xmm0, [esi + eax - 128];
                movapd xmm1, [edi + eax - 128];

                movapd xmm2, xmm0;
                movlhps xmm0, xmm1;
                movhlps xmm1, xmm2;

                movapd [ecx], xmm0;
                movapd [ecx + ebx], xmm1;

                add ecx, edx;

                movapd xmm0, [esi + eax - 112];
                movapd xmm1, [edi + eax - 112];

                movapd xmm2, xmm0;
                movlhps xmm0, xmm1;
                movhlps xmm1, xmm2;

                movapd [ecx], xmm0;
                movapd [ecx + ebx], xmm1;

                add ecx, edx;

                movapd xmm0, [esi + eax - 96];
                movapd xmm1, [edi + eax - 96];

                movapd xmm2, xmm0;
                movlhps xmm0, xmm1;
                movhlps xmm1, xmm2;

                movapd [ecx], xmm0;
                movapd [ecx + ebx], xmm1;

                add ecx, edx;

                movapd xmm0, [esi + eax - 80];
                movapd xmm1, [edi + eax - 80];

                movapd xmm2, xmm0;
                movlhps xmm0, xmm1;
                movhlps xmm1, xmm2;

                movapd [ecx], xmm0;
                movapd [ecx + ebx], xmm1;

                add ecx, edx;

                movapd xmm0, [esi + eax - 64];
                movapd xmm1, [edi + eax - 64];

                movapd xmm2, xmm0;
                movlhps xmm0, xmm1;
                movhlps xmm1, xmm2;

                movapd [ecx], xmm0;
                movapd [ecx + ebx], xmm1;

                add ecx, edx;

                movapd xmm0, [esi + eax - 48];
                movapd xmm1, [edi + eax - 48];

                movapd xmm2, xmm0;
                movlhps xmm0, xmm1;
                movhlps xmm1, xmm2;

                movapd [ecx], xmm0;
                movapd [ecx + ebx], xmm1;

                add ecx, edx;

                movapd xmm0, [esi + eax - 32];
                movapd xmm1, [edi + eax - 32];

                movapd xmm2, xmm0;
                movlhps xmm0, xmm1;
                movhlps xmm1, xmm2;

                movapd [ecx], xmm0;
                movapd [ecx + ebx], xmm1;

                add ecx, edx;

                movapd xmm0, [esi + eax - 16];
                movapd xmm1, [edi + eax - 16];

                movapd xmm2, xmm0;
                movlhps xmm0, xmm1;
                movhlps xmm1, xmm2;

                movapd [ecx], xmm0;
                movapd [ecx + ebx], xmm1;

                add ecx, edx;

            jmp @forxloop;

            @loopend:

            sub eax, 128;

            jz @nextLine;

            @forxloop2:
                movapd xmm0, [esi + eax];
                movapd xmm1, [edi + eax];

                movapd xmm2, xmm0;
                movlhps xmm0, xmm1;
                movhlps xmm1, xmm2;

                movapd [ecx], xmm0;
                movapd [ecx + ebx], xmm1;

                add ecx, destLineWidth2;
            add eax, 16;
            jnz @forxloop2;

            @nextLine:

            // handle last element differently
            movsd xmm0, [esi + eax];
            movsd xmm1, [edi + eax];

            movlhps xmm0, xmm1;
            movapd [ecx], xmm0;

            // increment pointers
            mov eax, dest;
            add eax, 16;
            mov dest, eax;

            mov eax, mt;
            add eax, LineWidth;
            add eax, LineWidth;
            mov mt, eax;

            sub y, 2;
        jnz @foryloop;

        // handle last line differently
        mov ecx, dest;

        mov esi, mt;
        sub esi, iters;

        mov eax, iters;
        @forxloop3:
            movapd xmm0, [esi + eax];

            movhlps xmm1, xmm0;
            movsd [ecx], xmm0;
            movsd [ecx + ebx], xmm1;

            add ecx, edx;
        add eax, 16;
        jnz @forxloop3;

        // handle last element differently
        movsd xmm0, [esi + eax];
        movsd [ecx], xmm0;

        pop ebx;
        pop edi;
        pop esi;
     end;
end;

procedure ASMMatrixTransposeUnAlignedOddWOddH(dest : PDouble; const destLineWidth : TASMNativeInt; mt : PDouble; const LineWidth : TASMNativeInt; width : TASMNativeInt; height : TASMNativeInt);
var iters : TASMNativeInt;
    destLineWidth2 : TASMNativeInt;
    y : TASMNativeInt;
begin
     assert((width > 1) and (height > 1) and (LineWidth >= width*sizeof(double)), 'Dimension error');
     assert(((width and 1) = 1) and ((height and 1) = 1), 'Error width and height must be even');

     destLineWidth2 := 2*destLineWidth;
     iters := -(width - 1)*sizeof(double);
     y := height - 1;
     asm
        push esi;
        push edi;
        push ebx;

        mov edx, destLineWidth2;
        mov ebx, destLineWidth;
        
        @foryloop:
            mov ecx, dest;

            mov esi, mt;
            mov edi, esi;
            sub esi, iters;
            add edi, LineWidth;
            sub edi, iters;

            // unrolled loop
            mov eax, iters;
            @forxloop:
                add eax, 128;
                jg @loopend;

                movupd xmm0, [esi + eax - 128];
                movupd xmm1, [edi + eax - 128];

                movupd xmm2, xmm0;
                movlhps xmm0, xmm1;
                movhlps xmm1, xmm2;

                movupd [ecx], xmm0;
                movupd [ecx + ebx], xmm1;

                add ecx, edx;

                movupd xmm0, [esi + eax - 112];
                movupd xmm1, [edi + eax - 112];

                movupd xmm2, xmm0;
                movlhps xmm0, xmm1;
                movhlps xmm1, xmm2;

                movupd [ecx], xmm0;
                movupd [ecx + ebx], xmm1;

                add ecx, edx;

                movupd xmm0, [esi + eax - 96];
                movupd xmm1, [edi + eax - 96];

                movupd xmm2, xmm0;
                movlhps xmm0, xmm1;
                movhlps xmm1, xmm2;

                movupd [ecx], xmm0;
                movupd [ecx + ebx], xmm1;

                add ecx, edx;

                movupd xmm0, [esi + eax - 80];
                movupd xmm1, [edi + eax - 80];

                movupd xmm2, xmm0;
                movlhps xmm0, xmm1;
                movhlps xmm1, xmm2;

                movupd [ecx], xmm0;
                movupd [ecx + ebx], xmm1;

                add ecx, edx;

                movupd xmm0, [esi + eax - 64];
                movupd xmm1, [edi + eax - 64];

                movupd xmm2, xmm0;
                movlhps xmm0, xmm1;
                movhlps xmm1, xmm2;

                movupd [ecx], xmm0;
                movupd [ecx + ebx], xmm1;

                add ecx, edx;

                movupd xmm0, [esi + eax - 48];
                movupd xmm1, [edi + eax - 48];

                movupd xmm2, xmm0;
                movlhps xmm0, xmm1;
                movhlps xmm1, xmm2;

                movupd [ecx], xmm0;
                movupd [ecx + ebx], xmm1;

                add ecx, edx;

                movupd xmm0, [esi + eax - 32];
                movupd xmm1, [edi + eax - 32];

                movupd xmm2, xmm0;
                movlhps xmm0, xmm1;
                movhlps xmm1, xmm2;

                movupd [ecx], xmm0;
                movupd [ecx + ebx], xmm1;

                add ecx, edx;

                movupd xmm0, [esi + eax - 16];
                movupd xmm1, [edi + eax - 16];

                movupd xmm2, xmm0;
                movlhps xmm0, xmm1;
                movhlps xmm1, xmm2;

                movupd [ecx], xmm0;
                movupd [ecx + ebx], xmm1;

                add ecx, edx;

            jmp @forxloop;

            @loopend:

            sub eax, 128;

            jz @nextLine;

            @forxloop2:
                movupd xmm0, [esi + eax];
                movupd xmm1, [edi + eax];

                movupd xmm2, xmm0;
                movlhps xmm0, xmm1;
                movhlps xmm1, xmm2;

                movupd [ecx], xmm0;
                movupd [ecx + ebx], xmm1;

                add ecx, destLineWidth2;
            add eax, 16;
            jnz @forxloop2;

            @nextLine:

            // handle last element differently
            movsd xmm0, [esi + eax];
            movsd xmm1, [edi + eax];

            movlhps xmm0, xmm1;
            movupd [ecx], xmm0;

            // increment pointers
            mov eax, dest;
            add eax, 16;
            mov dest, eax;

            mov eax, mt;
            add eax, LineWidth;
            add eax, LineWidth;
            mov mt, eax;

            sub y, 2;
        jnz @foryloop;

        // handle last line differently
        mov ecx, dest;

        mov esi, mt;
        sub esi, iters;

        mov eax, iters;
        @forxloop3:
            movupd xmm0, [esi + eax];

            movhlps xmm1, xmm0;
            movsd [ecx], xmm0;
            movsd [ecx + ebx], xmm1;

            add ecx, edx;
        add eax, 16;
        jnz @forxloop3;

        // handle last element differently
        movsd xmm0, [esi + eax];
        movsd [ecx], xmm0;

        pop ebx;
        pop edi;
        pop esi;
     end;
end;

// Inplace Trasnposition of an N x N matrix
procedure ASMMatrixTransposeInplace(mt : PDouble; const LineWidth : TASMNativeInt; N : TASMNativeInt);
begin
     assert(LineWidth >= N*sizeof(double), 'Linewidth error');
     asm
        push ebx;
        push edi;
        push esi;

        mov eax, N;
        cmp eax, 2;
        jl @@exitProc;

        // iter: -N*sizeof(Double)
        mov edx, eax;
        imul edx, -8;


        mov eax, mt;   // pDest
        mov ebx, eax;  // pDest1: genptr(mt, 0, 1, linewidth)
        add ebx, LineWidth;

        sub eax, edx;  // mt + iter

        // for y := 0 to n - 2
        mov ecx, N;
        dec ecx;
        mov N, ecx;

        mov ecx, LineWidth;
        @@foryloop:

           mov edi, edx; // iter aka x
           add edi, 8;
           mov esi, ebx;
           // for x := y + 1 to n-1 do
           @@forxloop:
              movsd xmm0, [eax + edi];
              movsd xmm1, [esi];

              movsd [eax + edi], xmm1;
              movsd [esi], xmm0;

              add esi, ecx;
           add edi, 8;
           jnz @@forxloop;

           add edx, 8;  // iter + sizeof(double);
           //pDest := PConstDoubleArr( GenPtr(dest, 0, y, destLineWidth) );
           add eax, ecx;
           // GenPtr(dest, y, y + 1, destLineWidth);
           add ebx, ecx;
           add ebx, 8;
        dec N;
        jnz @@foryloop;

        @@exitProc:

        pop esi;
        pop edi;
        pop ebx;
     end;
end;


{$ENDIF}

end.
