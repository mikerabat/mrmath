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


unit ASMMatrixElementwiseMultOperations;

interface

{$IFNDEF CPUX64}

uses ASMConsts;

procedure ASMMatrixElemMultAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
procedure ASMMatrixElemMultUnAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);

procedure ASMMatrixElemMultAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
procedure ASMMatrixElemMultUnAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);

{$ENDIF}

implementation

{$IFNDEF CPUX64}

procedure ASMMatrixElemMultAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var iters : integer;
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
                prefetch [esi + eax];
                prefetch [edi + eax];

                // addition:
                movapd xmm0, [esi + eax - 128];
                mulpd xmm0, [edi + eax - 128];

                movapd [ecx + eax - 128], xmm0;

                movapd xmm1, [esi + eax - 112];
                mulpd xmm1, [edi + eax - 112];

                movapd [ecx + eax - 112], xmm1;

                movapd xmm2, [esi + eax - 96];
                mulpd xmm2, [edi + eax - 96];

                movapd [ecx + eax - 96], xmm2;

                movapd xmm3, [esi + eax - 80];
                mulpd xmm3, [edi + eax - 80];

                movapd [ecx + eax - 80], xmm3;

                movapd xmm4, [esi + eax - 64];
                mulpd xmm4, [edi + eax - 64];

                movapd [ecx + eax - 64], xmm4;

                movapd xmm5, [esi + eax - 48];
                mulpd xmm5, [edi + eax - 48];

                movapd [ecx + eax - 48], xmm5;

                movapd xmm6, [esi + eax - 32];
                mulpd xmm6, [edi + eax - 32];

                movapd [ecx + eax - 32], xmm6;

                movapd xmm7, [esi + eax - 16];
                mulpd xmm7, [edi + eax - 16];

                movapd [ecx + eax - 16], xmm7;
            jmp @addforxloop

            @loopEnd:

            sub eax, 128;

            jz @nextLine;

            @addforxloop2:
                movapd xmm0, [esi + eax];
                mulpd xmm0, [edi + eax];

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

procedure ASMMatrixElemMultUnAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var iters : integer;
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
                mulpd xmm0, xmm1;

                movupd [ecx + eax - 128], xmm0;

                movupd xmm0, [esi + eax - 112];
                movupd xmm1, [edi + eax - 112];
                mulpd xmm0, xmm1;

                movupd [ecx + eax - 112], xmm0;

                movupd xmm0, [esi + eax - 96];
                movupd xmm1, [edi + eax - 96];
                mulpd xmm0, xmm1;

                movupd [ecx + eax - 96], xmm0;

                movupd xmm0, [esi + eax - 80];
                movupd xmm1, [edi + eax - 80];
                mulpd xmm0, xmm1;

                movupd [ecx + eax - 80], xmm0;

                movupd xmm0, [esi + eax - 64];
                movupd xmm1, [edi + eax - 64];
                mulpd xmm0, xmm1;

                movupd [ecx + eax - 64], xmm0;

                movupd xmm0, [esi + eax - 48];
                movupd xmm1, [edi + eax - 48];
                mulpd xmm0, xmm1;

                movupd [ecx + eax - 48], xmm0;

                movupd xmm0, [esi + eax - 32];
                movupd xmm1, [edi + eax - 32];
                mulpd xmm0, xmm1;

                movupd [ecx + eax - 32], xmm0;

                movupd xmm0, [esi + eax - 16];
                movupd xmm1, [edi + eax - 16];
                mulpd xmm0, xmm1;

                movupd [ecx + eax - 16], xmm0;
            // loop x end
            jmp @addforxloop

            @loopEnd:

            sub eax, 128;

            jz @nextLine;

            @addforxloop2:
                movupd xmm0, [esi + eax];
                movupd xmm1, [edi + eax];
                mulpd xmm0, xmm1;

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

procedure ASMMatrixElemMultAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var iters : integer;
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
                prefetch [esi + eax];
                prefetch [edi + eax];

                // mult:
                movapd xmm0, [esi + eax - 128];
                mulpd xmm0, [edi + eax - 128];

                movapd [ecx + eax - 128], xmm0;

                movapd xmm1, [esi + eax - 112];
                mulpd xmm1, [edi + eax - 112];

                movapd [ecx + eax - 112], xmm1;

                movapd xmm2, [esi + eax - 96];
                mulpd xmm2, [edi + eax - 96];

                movapd [ecx + eax - 96], xmm2;

                movapd xmm3, [esi + eax - 80];
                mulpd xmm3, [edi + eax - 80];

                movapd [ecx + eax - 80], xmm3;

                movapd xmm4, [esi + eax - 64];
                mulpd xmm4, [edi + eax - 64];

                movapd [ecx + eax - 64], xmm4;

                movapd xmm5, [esi + eax - 48];
                mulpd xmm5, [edi + eax - 48];

                movapd [ecx + eax - 48], xmm5;

                movapd xmm6, [esi + eax - 32];
                mulpd xmm6, [edi + eax - 32];

                movapd [ecx + eax - 32], xmm6;

                movapd xmm7, [esi + eax - 16];
                mulpd xmm7, [edi + eax - 16];

                movapd [ecx + eax - 16], xmm7;
            jmp @addforxloop

            @loopEnd:

            sub eax, 128;

            jz @nextLine;

            @addforxloop2:
                movapd xmm0, [esi + eax];
                mulpd xmm0, [edi + eax];

                movapd [ecx + eax], xmm0;
            add eax, 16;
            jnz @addforxloop2;

            @nextLine:

            // special care of the last column:
            movlpd xmm0, [esi];
            addsd xmm0, [edi];

            movlpd [ecx], xmm0;

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

procedure ASMMatrixElemMultUnAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var iters : integer;
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

                // mult:
                movupd xmm0, [esi + eax - 128];
                movupd xmm1, [edi + eax - 128];
                mulpd xmm0, xmm1;

                movupd [ecx + eax - 128], xmm0;

                movupd xmm0, [esi + eax - 112];
                movupd xmm1, [edi + eax - 112];
                mulpd xmm0, xmm1;

                movupd [ecx + eax - 112], xmm0;

                movupd xmm0, [esi + eax - 96];
                movupd xmm1, [edi + eax - 96];
                mulpd xmm0, xmm1;

                movupd [ecx + eax - 96], xmm0;

                movupd xmm0, [esi + eax - 80];
                movupd xmm1, [edi + eax - 80];
                mulpd xmm0, xmm1;

                movupd [ecx + eax - 80], xmm0;

                movupd xmm0, [esi + eax - 64];
                movupd xmm1, [edi + eax - 64];
                mulpd xmm0, xmm1;

                movupd [ecx + eax - 64], xmm0;

                movupd xmm0, [esi + eax - 48];
                movupd xmm1, [edi + eax - 48];
                mulpd xmm0, xmm1;

                movupd [ecx + eax - 48], xmm0;

                movupd xmm0, [esi + eax - 32];
                movupd xmm1, [edi + eax - 32];
                mulpd xmm0, xmm1;

                movupd [ecx + eax - 32], xmm0;

                movupd xmm0, [esi + eax - 16];
                movupd xmm1, [edi + eax - 16];
                mulpd xmm0, xmm1;

                movupd [ecx + eax - 16], xmm0;
            // loop x end
            jmp @addforxloop

            @loopEnd:

            sub eax, 128;

            jz @nextLine;

            @addforxloop2:
                movupd xmm0, [esi + eax];
                movupd xmm1, [edi + eax];
                mulpd xmm0, xmm1;

                movupd [ecx + eax], xmm0;
            add eax, 16;
            jnz @addforxloop2;

            @nextLine:

            // special care of the last column:
            movlpd xmm0, [esi];
            addsd xmm0, [edi];

            movlpd [ecx], xmm0;

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

{$ENDIF}

end.
