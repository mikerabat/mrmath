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


unit ASMMatrixAddSubOperations;

// ############################################################
// ##### Matrix addition/subtraction assembler optimized:
// ############################################################

interface

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}
{$IFNDEF x64}

uses MatrixConst;

procedure ASMMatrixAddAlignedEvenW(dest : PDouble; const destLinewidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, Linewidth2 : TASMNativeInt);
procedure ASMMatrixAddUnAlignedEvenW(dest : PDouble; const destLinewidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, Linewidth2 : TASMNativeInt);

procedure ASMMatrixAddAlignedOddW(dest : PDouble; const destLinewidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, Linewidth2 : TASMNativeInt);
procedure ASMMatrixAddUnAlignedOddW(dest : PDouble; const destLinewidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, Linewidth2 : TASMNativeInt);

procedure ASMMatrixSubAlignedEvenW(dest : PDouble; const destLinewidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, Linewidth2 : TASMNativeInt);
procedure ASMMatrixSubUnAlignedEvenW(dest : PDouble; const destLinewidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, Linewidth2 : TASMNativeInt);

procedure ASMMatrixSubAlignedOddW(dest : PDouble; const destLinewidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, Linewidth2 : TASMNativeInt);
procedure ASMMatrixSubUnAlignedOddW(dest : PDouble; const destLinewidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, Linewidth2 : TASMNativeInt);

procedure ASMMatrixSubT(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; LineWidthB : TASMNativeInt; width, height : TASMNativeInt);

procedure ASMMatrixSubVecAlignedVecRow(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; incX : TASMNativeInt; width, Height : TASMNativeInt);
procedure ASMMatrixSubVecAlignedRow(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; incX : TASMNativeInt; width, Height : TASMNativeInt);
procedure ASMMatrixSubVecAlignedCol(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; incX : TASMNativeInt; width, Height : TASMNativeInt);

procedure ASMMatrixSubVecUnalignedVecRow(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; incX : TASMNativeInt; width, Height : TASMNativeInt);
procedure ASMMatrixSubVecUnalignedRow(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; incX : TASMNativeInt; width, Height : TASMNativeInt);
procedure ASMMatrixSubVecUnalignedCol(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; incX : TASMNativeInt; width, Height : TASMNativeInt);

procedure ASMMatrixAddVecAlignedVecRow(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; incX : TASMNativeInt; width, Height : TASMNativeInt);
procedure ASMMatrixAddVecAlignedRow(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; incX : TASMNativeInt; width, Height : TASMNativeInt);
procedure ASMMatrixAddVecAlignedCol(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; incX : TASMNativeInt; width, Height : TASMNativeInt);

procedure ASMMatrixAddVecUnalignedVecRow(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; incX : TASMNativeInt; width, Height : TASMNativeInt);
procedure ASMMatrixAddVecUnalignedRow(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; incX : TASMNativeInt; width, Height : TASMNativeInt);
procedure ASMMatrixAddVecUnalignedCol(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; incX : TASMNativeInt; width, Height : TASMNativeInt);

{$ENDIF}

implementation

{$IFNDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$S-} {$ENDIF}

procedure ASMMatrixAddAlignedEvenW(dest : PDouble; const destLinewidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, Linewidth2 : TASMNativeInt);
var iters : TASMNativeInt;
begin
     Assert((Cardinal(mt1) and $0000000F = 0) and (Cardinal(mt2) and $0000000F = 0) and (Cardinal(dest) and $0000000F = 0), 'Error non aligned data');
     Assert((width and 1) = 0, 'Error width must be even');

     iters := -width*sizeof(double);

     asm
        push esi;
        push edi;

        // helper registers for the mt1, mt2 and dest pointers
        mov esi, mt1;
        sub esi, iters;
        mov edi, mt2;
        sub edi, iters;
        mov ecx, dest;
        sub ecx, iters;

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
                // prefetch [esi + eax];
                // prefetch [edi + eax];

                // addition:
                movapd xmm0, [esi + eax - 128];
                addpd xmm0, [edi + eax - 128];

                movapd [ecx + eax - 128], xmm0;

                movapd xmm1, [esi + eax - 112];
                addpd xmm1, [edi + eax - 112];

                movapd [ecx + eax - 112], xmm1;

                movapd xmm2, [esi + eax - 96];
                addpd xmm2, [edi + eax - 96];

                movapd [ecx + eax - 96], xmm2;

                movapd xmm3, [esi + eax - 80];
                addpd xmm3, [edi + eax - 80];

                movapd [ecx + eax - 80], xmm3;

                movapd xmm4, [esi + eax - 64];
                addpd xmm4, [edi + eax - 64];

                movapd [ecx + eax - 64], xmm4;

                movapd xmm5, [esi + eax - 48];
                addpd xmm5, [edi + eax - 48];

                movapd [ecx + eax - 48], xmm5;

                movapd xmm6, [esi + eax - 32];
                addpd xmm6, [edi + eax - 32];

                movapd [ecx + eax - 32], xmm6;

                movapd xmm7, [esi + eax - 16];
                addpd xmm7, [edi + eax - 16];

                movapd [ecx + eax - 16], xmm7;
            jmp @addforxloop

            @loopEnd:

            sub eax, 128;

            jz @nextLine;

            @addforxloop2:
                movapd xmm0, [esi + eax];
                addpd xmm0, [edi + eax];

                movapd [ecx + eax], xmm0;
            add eax, 16;
            jnz @addforxloop2;

            @nextLine:

            // next line:
            add esi, lineWidth1;
            add edi, lineWidth2;
            add ecx, destLineWidth;

        // loop y end
        dec edx;
        jnz @@addforyloop;

        pop edi;
        pop esi;
     end;
end;

procedure ASMMatrixAddUnAlignedEvenW(dest : PDouble; const destLinewidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, Linewidth2 : TASMNativeInt);
var iters : TASMNativeInt;
begin
     Assert((width and 1) = 0, 'Error width must be even');

     iters := -width*sizeof(double);

     asm
        push esi;
        push edi;
        
        // helper registers for the mt1, mt2 and dest pointers
        mov esi, mt1;
        sub esi, iters;
        mov edi, mt2;
        sub edi, iters;
        mov ecx, dest;
        sub ecx, iters;

        // for y := 0 to height - 1:
        mov edx, Height;
        @@addforyloop:
            // for x := 0 to w - 1;
            // prepare for reverse loop
            mov eax, iters;

            @addforxloop:
                add eax, 128;
                jg @loopEnd;

                // addition:
                movupd xmm0, [esi + eax - 128];
                movupd xmm1, [edi + eax - 128];
                addpd xmm0, xmm1;

                movupd [ecx + eax - 128], xmm0;

                movupd xmm0, [esi + eax - 112];
                movupd xmm1, [edi + eax - 112];
                addpd xmm0, xmm1;

                movupd [ecx + eax - 112], xmm0;

                movupd xmm0, [esi + eax - 96];
                movupd xmm1, [edi + eax - 96];
                addpd xmm0, xmm1;

                movupd [ecx + eax - 96], xmm0;

                movupd xmm0, [esi + eax - 80];
                movupd xmm1, [edi + eax - 80];
                addpd xmm0, xmm1;

                movupd [ecx + eax - 80], xmm0;

                movupd xmm0, [esi + eax - 64];
                movupd xmm1, [edi + eax - 64];
                addpd xmm0, xmm1;

                movupd [ecx + eax - 64], xmm0;

                movupd xmm0, [esi + eax - 48];
                movupd xmm1, [edi + eax - 48];
                addpd xmm0, xmm1;

                movupd [ecx + eax - 48], xmm0;

                movupd xmm0, [esi + eax - 32];
                movupd xmm1, [edi + eax - 32];
                addpd xmm0, xmm1;

                movupd [ecx + eax - 32], xmm0;

                movupd xmm0, [esi + eax - 16];
                movupd xmm1, [edi + eax - 16];
                addpd xmm0, xmm1;

                movupd [ecx + eax - 16], xmm0;
            // loop x end
            jmp @addforxloop

            @loopEnd:

            sub eax, 128;

            jz @nextLine;

            @addforxloop2:
                movupd xmm0, [esi + eax];
                movupd xmm1, [edi + eax];
                addpd xmm0, xmm1;

                movupd [ecx + eax], xmm0;
            add eax, 16;
            jnz @addforxloop2;

            @nextLine:

            // next line:
            add esi, lineWidth1;
            add edi, lineWidth2;
            add ecx, destLineWidth;

        // loop y end
        dec edx;
        jnz @@addforyloop;

        pop edi;
        pop esi;
     end;
end;

procedure ASMMatrixAddAlignedOddW(dest : PDouble; const destLinewidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, Linewidth2 : TASMNativeInt);
var iters : TASMNativeInt;
begin
     Assert((Cardinal(mt1) and $0000000F = 0) and (Cardinal(mt2) and $0000000F = 0) and (Cardinal(dest) and $0000000F = 0), 'Error non aligned data');
     Assert((width and 1) = 1, 'Error width must be odd');

     iters := -(width - 1)*sizeof(double);

     asm
        push esi;
        push edi;
        
        // helper registers for the mt1, mt2 and dest pointers
        mov esi, mt1;
        sub esi, iters;
        mov edi, mt2;
        sub edi, iters;
        mov ecx, dest;
        sub ecx, iters;

        // for y := 0 to height - 1:
        mov edx, Height;
        @@addforyloop:
            // for x := 0 to w - 1;
            mov eax, iters;
            @addforxloop:
                add eax, 128;
                jg @loopEnd;

                // prefetch data...
                // prefetch [esi + eax];
                // prefetch [edi + eax];

                // addition:
                movapd xmm0, [esi + eax - 128];
                addpd xmm0, [edi + eax - 128];

                movapd [ecx + eax - 128], xmm0;

                movapd xmm1, [esi + eax - 112];
                addpd xmm1, [edi + eax - 112];

                movapd [ecx + eax - 112], xmm1;

                movapd xmm2, [esi + eax - 96];
                addpd xmm2, [edi + eax - 96];

                movapd [ecx + eax - 96], xmm2;

                movapd xmm3, [esi + eax - 80];
                addpd xmm3, [edi + eax - 80];

                movapd [ecx + eax - 80], xmm3;

                movapd xmm4, [esi + eax - 64];
                addpd xmm4, [edi + eax - 64];

                movapd [ecx + eax - 64], xmm4;

                movapd xmm5, [esi + eax - 48];
                addpd xmm5, [edi + eax - 48];

                movapd [ecx + eax - 48], xmm5;

                movapd xmm6, [esi + eax - 32];
                addpd xmm6, [edi + eax - 32];

                movapd [ecx + eax - 32], xmm6;

                movapd xmm7, [esi + eax - 16];
                addpd xmm7, [edi + eax - 16];

                movapd [ecx + eax - 16], xmm7;
            jmp @addforxloop

            @loopEnd:

            sub eax, 128;

            jz @nextLine;

            @addforxloop2:
                movapd xmm0, [esi + eax];
                addpd xmm0, [edi + eax];

                movapd [ecx + eax], xmm0;
            add eax, 16;
            jnz @addforxloop2;

            @nextLine:

            // special care of the last column:
            movsd xmm0, [esi];
            addsd xmm0, [edi];

            movsd [ecx], xmm0;

            // next line:
            add esi, lineWidth1;
            add edi, lineWidth2;
            add ecx, destLineWidth;

        // loop y end
        dec edx;
        jnz @@addforyloop;

        pop edi;
        pop esi;
     end;
end;

procedure ASMMatrixAddUnAlignedOddW(dest : PDouble; const destLinewidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, Linewidth2 : TASMNativeInt);
var iters : TASMNativeInt;
begin
     Assert((width and 1) = 1, 'Error width must be odd');

     iters := -(width - 1)*sizeof(double);

     asm
        push esi;
        push edi;

        // helper registers for the mt1, mt2 and dest pointers
        mov esi, mt1;
        sub esi, iters;
        mov edi, mt2;
        sub edi, iters;
        mov ecx, dest;
        sub ecx, iters;

        // for y := 0 to height - 1:
        mov edx, Height;
        @@addforyloop:
            // for x := 0 to w - 1;
            // prepare for reverse loop
            mov eax, iters;
            @addforxloop:
                add eax, 128;
                jg @loopEnd;

                // addition:
                movupd xmm0, [esi + eax - 128];
                movupd xmm1, [edi + eax - 128];
                addpd xmm0, xmm1;

                movupd [ecx + eax - 128], xmm0;

                movupd xmm0, [esi + eax - 112];
                movupd xmm1, [edi + eax - 112];
                addpd xmm0, xmm1;

                movupd [ecx + eax - 112], xmm0;

                movupd xmm0, [esi + eax - 96];
                movupd xmm1, [edi + eax - 96];
                addpd xmm0, xmm1;

                movupd [ecx + eax - 96], xmm0;

                movupd xmm0, [esi + eax - 80];
                movupd xmm1, [edi + eax - 80];
                addpd xmm0, xmm1;

                movupd [ecx + eax - 80], xmm0;

                movupd xmm0, [esi + eax - 64];
                movupd xmm1, [edi + eax - 64];
                addpd xmm0, xmm1;

                movupd [ecx + eax - 64], xmm0;

                movupd xmm0, [esi + eax - 48];
                movupd xmm1, [edi + eax - 48];
                addpd xmm0, xmm1;

                movupd [ecx + eax - 48], xmm0;

                movupd xmm0, [esi + eax - 32];
                movupd xmm1, [edi + eax - 32];
                addpd xmm0, xmm1;

                movupd [ecx + eax - 32], xmm0;

                movupd xmm0, [esi + eax - 16];
                movupd xmm1, [edi + eax - 16];
                addpd xmm0, xmm1;

                movupd [ecx + eax - 16], xmm0;
            // loop x end
            jmp @addforxloop

            @loopEnd:

            sub eax, 128;

            jz @nextLine;

            @addforxloop2:
                movupd xmm0, [esi + eax];
                movupd xmm1, [edi + eax];
                addpd xmm0, xmm1;

                movupd [ecx + eax], xmm0;
            add eax, 16;
            jnz @addforxloop2;

            @nextLine:

            // special care of the last column:
            movsd xmm0, [esi];
            addsd xmm0, [edi];

            movsd [ecx], xmm0;

            // next line:
            add esi, lineWidth1;
            add edi, lineWidth2;
            add ecx, destLineWidth;

        // loop y end
        dec edx;
        jnz @@addforyloop;

        pop edi;
        pop esi;
     end;
end;

procedure ASMMatrixSubAlignedEvenW(dest : PDouble; const destLinewidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, Linewidth2 : TASMNativeInt);
var iters : TASMNativeInt;
begin
     asserT(width > 1, 'Error width must be greater than one');
     Assert((Cardinal(mt1) and $0000000F = 0) and (Cardinal(mt2) and $0000000F = 0) and (Cardinal(dest) and $0000000F = 0), 'Error non aligned data');
     Assert((width and 1) = 0, 'Error width must be even');

     iters := -width*sizeof(double);

     asm
        push esi;
        push edi;

        // helper registers for the mt1, mt2 and dest pointers
        mov esi, mt1;
        sub esi, iters;
        mov edi, mt2;
        sub edi, iters;
        mov ecx, dest;
        sub ecx, iters;

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
                // prefetch [esi + eax];
                // prefetch [edi + eax];

                // substraction:
                movapd xmm0, [esi + eax - 128];
                subpd xmm0, [edi + eax - 128];

                movapd [ecx + eax - 128], xmm0;

                movapd xmm1, [esi + eax - 112];
                subpd xmm1, [edi + eax - 112];

                movapd [ecx + eax - 112], xmm1;

                movapd xmm2, [esi + eax - 96];
                subpd xmm2, [edi + eax - 96];

                movapd [ecx + eax - 96], xmm2;

                movapd xmm3, [esi + eax - 80];
                subpd xmm3, [edi + eax - 80];

                movapd [ecx + eax - 80], xmm3;

                movapd xmm4, [esi + eax - 64];
                subpd xmm4, [edi + eax - 64];

                movapd [ecx + eax - 64], xmm4;

                movapd xmm5, [esi + eax - 48];
                subpd xmm5, [edi + eax - 48];

                movapd [ecx + eax - 48], xmm5;

                movapd xmm6, [esi + eax - 32];
                subpd xmm6, [edi + eax - 32];

                movapd [ecx + eax - 32], xmm6;

                movapd xmm7, [esi + eax - 16];
                subpd xmm7, [edi + eax - 16];

                movapd [ecx + eax - 16], xmm7;
            jmp @addforxloop

            @loopEnd:

            sub eax, 128;

            jz @nextLine;

            @addforxloop2:
                movapd xmm0, [esi + eax];
                subpd xmm0, [edi + eax];

                movapd [ecx + eax], xmm0;
            add eax, 16;
            jnz @addforxloop2;

            @nextLine:

            // next line:
            add esi, lineWidth1;
            add edi, lineWidth2;
            add ecx, destLineWidth;

        // loop y end
        dec edx;
        jnz @@addforyloop;

        pop edi;
        pop esi;
     end;
end;

procedure ASMMatrixSubUnAlignedEvenW(dest : PDouble; const destLinewidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, Linewidth2 : TASMNativeInt);
var iters : TASMNativeInt;
begin
     Assert((width and 1) = 0, 'Error width must be even');

     iters := -width*sizeof(double);

     asm
        push esi;
        push edi;
        
        // helper registers for the mt1, mt2 and dest pointers
        mov esi, mt1;
        sub esi, iters;
        mov edi, mt2;
        sub edi, iters;
        mov ecx, dest;
        sub ecx, iters;

        // for y := 0 to height - 1:
        mov edx, Height;
        @@addforyloop:
            // for x := 0 to w - 1;
            // prepare for reverse loop
            mov eax, iters;

            @addforxloop:
                add eax, 128;
                jg @loopEnd;

                // substraction:
                movupd xmm0, [esi + eax - 128];
                movupd xmm1, [edi + eax - 128];
                subpd xmm0, xmm1;

                movupd [ecx + eax - 128], xmm0;

                movupd xmm0, [esi + eax - 112];
                movupd xmm1, [edi + eax - 112];
                subpd xmm0, xmm1;

                movupd [ecx + eax - 112], xmm0;

                movupd xmm0, [esi + eax - 96];
                movupd xmm1, [edi + eax - 96];
                subpd xmm0, xmm1;

                movupd [ecx + eax - 96], xmm0;

                movupd xmm0, [esi + eax - 80];
                movupd xmm1, [edi + eax - 80];
                subpd xmm0, xmm1;

                movupd [ecx + eax - 80], xmm0;

                movupd xmm0, [esi + eax - 64];
                movupd xmm1, [edi + eax - 64];
                subpd xmm0, xmm1;

                movupd [ecx + eax - 64], xmm0;

                movupd xmm0, [esi + eax - 48];
                movupd xmm1, [edi + eax - 48];
                subpd xmm0, xmm1;

                movupd [ecx + eax - 48], xmm0;

                movupd xmm0, [esi + eax - 32];
                movupd xmm1, [edi + eax - 32];
                subpd xmm0, xmm1;

                movupd [ecx + eax - 32], xmm0;

                movupd xmm0, [esi + eax - 16];
                movupd xmm1, [edi + eax - 16];
                subpd xmm0, xmm1;

                movupd [ecx + eax - 16], xmm0;
            // loop x end
            jmp @addforxloop

            @loopEnd:

            sub eax, 128;

            jz @nextLine;

            @addforxloop2:
                movupd xmm0, [esi + eax];
                movupd xmm1, [edi + eax];
                subpd xmm0, xmm1;

                movupd [ecx + eax], xmm0;
            add eax, 16;
            jnz @addforxloop2;

            @nextLine:

            // next line:
            add esi, lineWidth1;
            add edi, lineWidth2;
            add ecx, destLineWidth;

        // loop y end
        dec edx;
        jnz @@addforyloop;

        pop edi;
        pop esi;
     end;
end;

procedure ASMMatrixSubAlignedOddW(dest : PDouble; const destLinewidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, Linewidth2 : TASMNativeInt);
var iters : TASMNativeInt;
begin
     Assert((Cardinal(mt1) and $0000000F = 0) and (Cardinal(mt2) and $0000000F = 0) and (Cardinal(dest) and $0000000F = 0), 'Error non aligned data');
     Assert((width and 1) = 1, 'Error width must be odd');

     iters := -(width - 1)*sizeof(double);

     asm
        push esi;
        push edi;

        // helper registers for the mt1, mt2 and dest pointers
        mov esi, mt1;
        sub esi, iters;
        mov edi, mt2;
        sub edi, iters;
        mov ecx, dest;
        sub ecx, iters;

        // for y := 0 to height - 1:
        mov edx, Height;
        @@addforyloop:
            // for x := 0 to w - 1;
            mov eax, iters;
            @addforxloop:
                add eax, 128;
                jg @loopEnd;

                // prefetch data...
                // prefetch [esi + eax];
                // prefetch [edi + eax];

                // substraction:
                movapd xmm0, [esi + eax - 128];
                subpd xmm0, [edi + eax - 128];

                movapd [ecx + eax - 128], xmm0;

                movapd xmm1, [esi + eax - 112];
                subpd xmm1, [edi + eax - 112];

                movapd [ecx + eax - 112], xmm1;

                movapd xmm2, [esi + eax - 96];
                subpd xmm2, [edi + eax - 96];

                movapd [ecx + eax - 96], xmm2;

                movapd xmm3, [esi + eax - 80];
                subpd xmm3, [edi + eax - 80];

                movapd [ecx + eax - 80], xmm3;

                movapd xmm4, [esi + eax - 64];
                subpd xmm4, [edi + eax - 64];

                movapd [ecx + eax - 64], xmm4;

                movapd xmm5, [esi + eax - 48];
                subpd xmm5, [edi + eax - 48];

                movapd [ecx + eax - 48], xmm5;

                movapd xmm6, [esi + eax - 32];
                subpd xmm6, [edi + eax - 32];

                movapd [ecx + eax - 32], xmm6;

                movapd xmm7, [esi + eax - 16];
                subpd xmm7, [edi + eax - 16];

                movapd [ecx + eax - 16], xmm7;
            jmp @addforxloop

            @loopEnd:

            sub eax, 128;

            jz @nextLine;

            @addforxloop2:
                movapd xmm0, [esi + eax];
                subpd xmm0, [edi + eax];

                movapd [ecx + eax], xmm0;
            add eax, 16;
            jnz @addforxloop2;

            @nextLine:

            // special care of the last column:
            movsd xmm0, [esi];
            subsd xmm0, [edi];

            movsd [ecx], xmm0;

            // next line:
            add esi, lineWidth1;
            add edi, lineWidth2;
            add ecx, destLineWidth;

        // loop y end
        dec edx;
        jnz @@addforyloop;

        pop edi;
        pop esi;
     end;
end;

procedure ASMMatrixSubUnAlignedOddW(dest : PDouble; const destLinewidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, Linewidth2 : TASMNativeInt);
var iters : TASMNativeInt;
begin
     Assert((width and 1) = 1, 'Error width must be odd');

     iters := -(width - 1)*sizeof(double);

     asm
        push esi;
        push edi;

        // helper registers for the mt1, mt2 and dest pointers
        mov esi, mt1;
        sub esi, iters;
        mov edi, mt2;
        sub edi, iters;
        mov ecx, dest;
        sub ecx, iters;

        // for y := 0 to height - 1:
        mov edx, Height;
        @@addforyloop:
            // for x := 0 to w - 1;
            // prepare for reverse loop
            mov eax, iters;
            @addforxloop:
                add eax, 128;
                jg @loopEnd;

                // substraction:
                movupd xmm0, [esi + eax - 128];
                movupd xmm1, [edi + eax - 128];
                subpd xmm0, xmm1;

                movupd [ecx + eax - 128], xmm0;

                movupd xmm0, [esi + eax - 112];
                movupd xmm1, [edi + eax - 112];
                subpd xmm0, xmm1;

                movupd [ecx + eax - 112], xmm0;

                movupd xmm0, [esi + eax - 96];
                movupd xmm1, [edi + eax - 96];
                subpd xmm0, xmm1;

                movupd [ecx + eax - 96], xmm0;

                movupd xmm0, [esi + eax - 80];
                movupd xmm1, [edi + eax - 80];
                subpd xmm0, xmm1;

                movupd [ecx + eax - 80], xmm0;

                movupd xmm0, [esi + eax - 64];
                movupd xmm1, [edi + eax - 64];
                subpd xmm0, xmm1;

                movupd [ecx + eax - 64], xmm0;

                movupd xmm0, [esi + eax - 48];
                movupd xmm1, [edi + eax - 48];
                subpd xmm0, xmm1;

                movupd [ecx + eax - 48], xmm0;

                movupd xmm0, [esi + eax - 32];
                movupd xmm1, [edi + eax - 32];
                subpd xmm0, xmm1;

                movupd [ecx + eax - 32], xmm0;

                movupd xmm0, [esi + eax - 16];
                movupd xmm1, [edi + eax - 16];
                subpd xmm0, xmm1;

                movupd [ecx + eax - 16], xmm0;
            // loop x end
            jmp @addforxloop

            @loopEnd:

            sub eax, 128;

            jz @nextLine;

            @addforxloop2:
                movupd xmm0, [esi + eax];
                movupd xmm1, [edi + eax];
                subpd xmm0, xmm1;

                movupd [ecx + eax], xmm0;
            add eax, 16;
            jnz @addforxloop2;

            @nextLine:

            // special care of the last column:
            movsd xmm0, [esi];
            subsd xmm0, [edi];

            movsd [ecx], xmm0;

            // next line:
            add esi, lineWidth1;
            add edi, lineWidth2;
            add ecx, destLineWidth;

        // loop y end
        dec edx;
        jnz @@addforyloop;

        pop edi;
        pop esi;
     end;
end;

// ##################################################
// #### Matrix substraction transposed
// ##################################################

procedure ASMMatrixSubT(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; LineWidthB : TASMNativeInt; width, height : TASMNativeInt);
begin
     asm
        push ebx;
        push edi;
        push esi;

        // iter := -width*sizeof(double)
        mov ebx, A;
        mov eax, width;
        imul eax, -8;
        sub ebx, eax;

        mov edi, B;
        mov esi, LineWidthB;

        // for y := 0 to height - 1
        @@foryloop:
           mov ecx, edi;
           mov edx, eax;

           // for x := 0 to width - 1
           @@forxloop:
              movsd xmm0, [ebx + edx];
              movsd xmm1, [ecx];

              subsd xmm0, xmm1;
              movsd [ebx + edx], xmm0;

              add ecx, esi;
           add edx, 8;
           jnz @@forxloop;

           add ebx, LineWidthA;
           add edi, 8;
        dec height;
        jnz @@foryloop;

        pop esi;
        pop edi;
        pop ebx;
     end;
end;

// ########################################################
// #### Matrix add, sub to vector operations
// ########################################################

procedure ASMMatrixSubVecAlignedVecRow(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; incX : TASMNativeInt; width, Height : TASMNativeInt);
begin
     asm
        push ebx;

        mov ebx, width;
        imul ebx, -8;

        mov ecx, A;
        mov edx, B;

        sub ecx, ebx;
        sub edx, ebx;

        @@foryloop:
           mov eax, ebx;

           @@forxloopUnrolled:
              add eax, 64;
              jg @@EndLoop1;

              movapd xmm0, [ecx + eax - 64];
              subpd xmm0, [edx + eax - 64];
              movapd [ecx + eax - 64], xmm0;

              movapd xmm1, [ecx + eax - 48];
              subpd xmm1, [edx + eax - 48];
              movapd [ecx + eax - 48], xmm1;

              movapd xmm2, [ecx + eax - 32];
              subpd xmm2, [edx + eax - 32];
              movapd [ecx + eax - 32], xmm2;

              movapd xmm3, [ecx + eax - 16];
              subpd xmm3, [edx + eax - 16];
              movapd [ecx + eax - 16], xmm3;

           jmp @@forxloopUnrolled;

           @@EndLoop1:

           sub eax, 64;

           jz @NextLine;

           @@forxloop:
              movsd xmm0, [ecx + eax];
              movsd xmm1, [edx + eax];

              subsd xmm0, xmm1;
              movsd [ecx + eax], xmm0;

              add eax, 8;
           jnz @@forxloop;

           @NextLine:

           add ecx, LineWidthA;
        dec Height;
        jnz @@foryloop;

        pop ebx;
     end;
end;

procedure ASMMatrixSubVecAlignedRow(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; incX : TASMNativeInt; width, Height : TASMNativeInt);
var vecIter : integer;
begin
     asm
        push ebx;
        push edi;
        push esi;

        mov edi, incx;
        mov esi, width;

        imul esi, edi;
        imul esi, -1;
        mov vecIter, esi;

        mov ebx, width;
        imul ebx, -8;

        mov ecx, A;
        mov edx, B;

        sub ecx, ebx;
        sub edx, esi;

        @@foryloop:
           mov eax, ebx;
           mov esi, vecIter;

           @@forxloop:
              movsd xmm0, [ecx + eax];
              movsd xmm1, [edx + esi];

              subsd xmm0, xmm1;
              movsd [ecx + eax], xmm0;

              add esi, edi;
              add eax, 8;
           jnz @@forxloop;

           @NextLine:

           add ecx, LineWidthA;
        dec Height;
        jnz @@foryloop;

        pop esi;
        pop edi;
        pop ebx;
     end;
end;

procedure ASMMatrixSubVecAlignedCol(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; incX : TASMNativeInt; width, Height : TASMNativeInt);
begin
     asm
        push ebx;

        mov ebx, width;
        imul ebx, -8;

        mov ecx, A;
        mov edx, B;

        sub ecx, ebx;

        @@foryloop:
           mov eax, ebx;

           movddup xmm1, [edx];

           @@forxloopUnrolled:
              add eax, 64;
              jg @@EndLoop1;

              movapd xmm0, [ecx + eax - 64];
              subpd xmm0, xmm1;
              movapd [ecx + eax - 64], xmm0;

              movapd xmm3, [ecx + eax - 48];
              subpd xmm3, xmm1;
              movapd [ecx + eax - 48], xmm3;

              movapd xmm2, [ecx + eax - 32];
              subpd xmm2, xmm1;
              movapd [ecx + eax - 32], xmm2;

              movapd xmm3, [ecx + eax - 16];
              subpd xmm3, xmm1;
              movapd [ecx + eax - 16], xmm3;

           jmp @@forxloopUnrolled;

           @@EndLoop1:

           sub eax, 64;

           jz @NextLine;

           @@forxloop:
              movsd xmm0, [ecx + eax];

              subsd xmm0, xmm1;
              movsd [ecx + eax], xmm0;

              add eax, 8;
           jnz @@forxloop;

           @NextLine:

           add ecx, LineWidthA;
           add edx, incX;
        dec Height;
        jnz @@foryloop;

        pop ebx;
     end;
end;

procedure ASMMatrixSubVecUnalignedVecRow(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; incX : TASMNativeInt; width, Height : TASMNativeInt);
begin
     asm
        push ebx;

        mov ebx, width;
        imul ebx, -8;

        mov ecx, A;
        mov edx, B;

        sub ecx, ebx;
        sub edx, ebx;

        @@foryloop:
           mov eax, ebx;

           @@forxloopUnrolled:
              add eax, 64;
              jg @@EndLoop1;

              movupd xmm0, [ecx + eax - 64];
              movupd xmm1, [edx + eax - 64];
              subpd xmm0, xmm1;
              movupd [ecx + eax - 64], xmm0;

              movupd xmm1, [ecx + eax - 48];
              movupd xmm2, [edx + eax - 48];
              subpd xmm1, xmm2;
              movupd [ecx + eax - 48], xmm1;

              movupd xmm2, [ecx + eax - 32];
              movupd xmm3, [edx + eax - 32];
              subpd xmm2, xmm3;
              movupd [ecx + eax - 32], xmm2;

              movupd xmm3, [ecx + eax - 16];
              movupd xmm0, [edx + eax - 16];
              subpd xmm3, xmm0;
              movupd [ecx + eax - 16], xmm3;

           jmp @@forxloopUnrolled;

           @@EndLoop1:

           sub eax, 64;

           jz @NextLine;

           @@forxloop:
              movsd xmm0, [ecx + eax];
              movsd xmm1, [edx + eax];

              subsd xmm0, xmm1;
              movsd [ecx + eax], xmm0;

              add eax, 8;
           jnz @@forxloop;

           @NextLine:

           add ecx, LineWidthA;
        dec Height;
        jnz @@foryloop;

        pop ebx;
     end;
end;

procedure ASMMatrixSubVecUnalignedRow(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; incX : TASMNativeInt; width, Height : TASMNativeInt);
var vecIter : integer;
begin
     asm
        push ebx;
        push edi;
        push esi;

        mov edi, incx;
        mov esi, width;

        imul esi, edi;
        imul esi, -1;
        mov vecIter, esi;

        mov ebx, width;
        imul ebx, -8;

        mov ecx, A;
        mov edx, B;

        sub ecx, ebx;
        sub edx, esi;

        @@foryloop:
           mov eax, ebx;
           mov esi, vecIter;

           @@forxloop:
              movsd xmm0, [ecx + eax];
              movsd xmm1, [edx + esi];

              subsd xmm0, xmm1;
              movsd [ecx + eax], xmm0;

              add esi, edi;
              add eax, 8;
           jnz @@forxloop;

           @NextLine:

           add ecx, LineWidthA;
        dec Height;
        jnz @@foryloop;

        pop esi;
        pop edi;
        pop ebx;
     end;
end;

procedure ASMMatrixSubVecUnalignedCol(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; incX : TASMNativeInt; width, Height : TASMNativeInt);
begin
     asm
        push ebx;

        mov ebx, width;
        imul ebx, -8;

        mov ecx, A;
        mov edx, B;

        sub ecx, ebx;

        @@foryloop:
           mov eax, ebx;

           movddup xmm1, [edx];

           @@forxloopUnrolled:
              add eax, 64;
              jg @@EndLoop1;

              movupd xmm0, [ecx + eax - 64];
              subpd xmm0, xmm1;
              movupd [ecx + eax - 64], xmm0;

              movupd xmm3, [ecx + eax - 48];
              subpd xmm3, xmm1;
              movupd [ecx + eax - 48], xmm3;

              movupd xmm2, [ecx + eax - 32];
              subpd xmm2, xmm1;
              movupd [ecx + eax - 32], xmm2;

              movupd xmm3, [ecx + eax - 16];
              subpd xmm3, xmm1;
              movupd [ecx + eax - 16], xmm3;

           jmp @@forxloopUnrolled;

           @@EndLoop1:

           sub eax, 64;

           jz @NextLine;

           @@forxloop:
              movsd xmm0, [ecx + eax];

              subsd xmm0, xmm1;
              movsd [ecx + eax], xmm0;

              add eax, 8;
           jnz @@forxloop;

           @NextLine:

           add ecx, LineWidthA;
           add edx, incX;
        dec Height;
        jnz @@foryloop;

        pop ebx;
     end;
end;


procedure ASMMatrixAddVecAlignedVecRow(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; incX : TASMNativeInt; width, Height : TASMNativeInt);
begin
     asm
        push ebx;

        mov ebx, width;
        imul ebx, -8;

        mov ecx, A;
        mov edx, B;

        sub ecx, ebx;
        sub edx, ebx;

        @@foryloop:
           mov eax, ebx;

           @@forxloopUnrolled:
              add eax, 64;
              jg @@EndLoop1;

              movapd xmm0, [ecx + eax - 64];
              addpd xmm0, [edx + eax - 64];
              movapd [ecx + eax - 64], xmm0;

              movapd xmm1, [ecx + eax - 48];
              addpd xmm1, [edx + eax - 48];
              movapd [ecx + eax - 48], xmm1;

              movapd xmm2, [ecx + eax - 32];
              addpd xmm2, [edx + eax - 32];
              movapd [ecx + eax - 32], xmm2;

              movapd xmm3, [ecx + eax - 16];
              addpd xmm3, [edx + eax - 16];
              movapd [ecx + eax - 16], xmm3;

           jmp @@forxloopUnrolled;

           @@EndLoop1:

           sub eax, 64;

           jz @NextLine;

           @@forxloop:
              movsd xmm0, [ecx + eax];
              movsd xmm1, [edx + eax];

              addsd xmm0, xmm1;
              movsd [ecx + eax], xmm0;

              add eax, 8;
           jnz @@forxloop;

           @NextLine:

           add ecx, LineWidthA;
        dec Height;
        jnz @@foryloop;

        pop ebx;
     end;
end;

procedure ASMMatrixAddVecAlignedRow(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; incX : TASMNativeInt; width, Height : TASMNativeInt);
var vecIter : integer;
begin
     asm
        push ebx;
        push edi;
        push esi;

        mov edi, incx;
        mov esi, width;

        imul esi, edi;
        imul esi, -1;
        mov vecIter, esi;

        mov ebx, width;
        imul ebx, -8;

        mov ecx, A;
        mov edx, B;

        sub ecx, ebx;
        sub edx, esi;

        @@foryloop:
           mov eax, ebx;
           mov esi, vecIter;

           @@forxloop:
              movsd xmm0, [ecx + eax];
              movsd xmm1, [edx + esi];

              addsd xmm0, xmm1;
              movsd [ecx + eax], xmm0;

              add esi, edi;
              add eax, 8;
           jnz @@forxloop;

           @NextLine:

           add ecx, LineWidthA;
        dec Height;
        jnz @@foryloop;

        pop esi;
        pop edi;
        pop ebx;
     end;
end;

procedure ASMMatrixAddVecAlignedCol(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; incX : TASMNativeInt; width, Height : TASMNativeInt);
begin
     asm
        push ebx;

        mov ebx, width;
        imul ebx, -8;

        mov ecx, A;
        mov edx, B;

        sub ecx, ebx;

        @@foryloop:
           mov eax, ebx;

           movddup xmm1, [edx];

           @@forxloopUnrolled:
              add eax, 64;
              jg @@EndLoop1;

              movapd xmm0, [ecx + eax - 64];
              addpd xmm0, xmm1;
              movapd [ecx + eax - 64], xmm0;

              movapd xmm3, [ecx + eax - 48];
              addpd xmm3, xmm1;
              movapd [ecx + eax - 48], xmm3;

              movapd xmm2, [ecx + eax - 32];
              addpd xmm2, xmm1;
              movapd [ecx + eax - 32], xmm2;

              movapd xmm3, [ecx + eax - 16];
              addpd xmm3, xmm1;
              movapd [ecx + eax - 16], xmm3;

           jmp @@forxloopUnrolled;

           @@EndLoop1:

           sub eax, 64;

           jz @NextLine;

           @@forxloop:
              movsd xmm0, [ecx + eax];

              addsd xmm0, xmm1;
              movsd [ecx + eax], xmm0;

              add eax, 8;
           jnz @@forxloop;

           @NextLine:

           add ecx, LineWidthA;
           add edx, incX;
        dec Height;
        jnz @@foryloop;

        pop ebx;
     end;
end;

procedure ASMMatrixAddVecUnalignedVecRow(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; incX : TASMNativeInt; width, Height : TASMNativeInt);
begin
     asm
        push ebx;

        mov ebx, width;
        imul ebx, -8;

        mov ecx, A;
        mov edx, B;

        sub ecx, ebx;
        sub edx, ebx;

        @@foryloop:
           mov eax, ebx;

           @@forxloopUnrolled:
              add eax, 64;
              jg @@EndLoop1;

              movupd xmm0, [ecx + eax - 64];
              movupd xmm1, [edx + eax - 64];
              addpd xmm0, xmm1;
              movupd [ecx + eax - 64], xmm0;

              movupd xmm1, [ecx + eax - 48];
              movupd xmm2, [edx + eax - 48];
              addpd xmm1, xmm2;
              movupd [ecx + eax - 48], xmm1;

              movupd xmm2, [ecx + eax - 32];
              movupd xmm3, [edx + eax - 32];
              addpd xmm2, xmm3;
              movupd [ecx + eax - 32], xmm2;

              movupd xmm3, [ecx + eax - 16];
              movupd xmm0, [edx + eax - 16];
              addpd xmm3, xmm0;
              movupd [ecx + eax - 16], xmm3;

           jmp @@forxloopUnrolled;

           @@EndLoop1:

           sub eax, 64;

           jz @NextLine;

           @@forxloop:
              movsd xmm0, [ecx + eax];
              movsd xmm1, [edx + eax];

              addsd xmm0, xmm1;
              movsd [ecx + eax], xmm0;

              add eax, 8;
           jnz @@forxloop;

           @NextLine:

           add ecx, LineWidthA;
        dec Height;
        jnz @@foryloop;

        pop ebx;
     end;
end;

procedure ASMMatrixAddVecUnalignedRow(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; incX : TASMNativeInt; width, Height : TASMNativeInt);
var vecIter : integer;
begin
     asm
        push ebx;
        push edi;
        push esi;

        mov edi, incx;
        mov esi, width;

        imul esi, edi;
        imul esi, -1;
        mov vecIter, esi;

        mov ebx, width;
        imul ebx, -8;

        mov ecx, A;
        mov edx, B;

        sub ecx, ebx;
        sub edx, esi;

        @@foryloop:
           mov eax, ebx;
           mov esi, vecIter;

           @@forxloop:
              movsd xmm0, [ecx + eax];
              movsd xmm1, [edx + esi];

              addsd xmm0, xmm1;
              movsd [ecx + eax], xmm0;

              add esi, edi;
              add eax, 8;
           jnz @@forxloop;

           @NextLine:

           add ecx, LineWidthA;
        dec Height;
        jnz @@foryloop;

        pop esi;
        pop edi;
        pop ebx;
     end;
end;

procedure ASMMatrixAddVecUnalignedCol(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; incX : TASMNativeInt; width, Height : TASMNativeInt);
begin
     asm
        push ebx;

        mov ebx, width;
        imul ebx, -8;

        mov ecx, A;
        mov edx, B;

        sub ecx, ebx;

        @@foryloop:
           mov eax, ebx;

           movddup xmm1, [edx];

           @@forxloopUnrolled:
              add eax, 64;
              jg @@EndLoop1;

              movupd xmm0, [ecx + eax - 64];
              addpd xmm0, xmm1;
              movupd [ecx + eax - 64], xmm0;

              movupd xmm3, [ecx + eax - 48];
              addpd xmm3, xmm1;
              movupd [ecx + eax - 48], xmm3;

              movupd xmm2, [ecx + eax - 32];
              addpd xmm2, xmm1;
              movupd [ecx + eax - 32], xmm2;

              movupd xmm3, [ecx + eax - 16];
              addpd xmm3, xmm1;
              movupd [ecx + eax - 16], xmm3;

           jmp @@forxloopUnrolled;

           @@EndLoop1:

           sub eax, 64;

           jz @NextLine;

           @@forxloop:
              movsd xmm0, [ecx + eax];

              addsd xmm0, xmm1;
              movsd [ecx + eax], xmm0;

              add eax, 8;
           jnz @@forxloop;

           @NextLine:

           add ecx, LineWidthA;
           add edx, incX;
        dec Height;
        jnz @@foryloop;

        pop ebx;
     end;
end;

{$ENDIF}

end.
