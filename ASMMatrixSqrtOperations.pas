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


unit ASMMatrixSqrtOperations;

// #####################################################
// #### SQRT opertaion applied to every element in a matrix
// #####################################################

interface

{$IFNDEF CPUX64}

uses ASMConsts;

procedure ASMMatrixSQRTAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt);
procedure ASMMatrixSQRTUnAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt);

procedure ASMMatrixSQRTAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt);
procedure ASMMatrixSQRTUnAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt);

{$ENDIF}

implementation

{$IFNDEF CPUX64}

procedure ASMMatrixSQRTAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt);
var iters : TASMNativeInt;
begin
     Assert((Cardinal(dest) and $0000000F = 0), 'Error non aligned data');
     assert((width > 0) and (height > 0) and (LineWidth >= width*sizeof(double)), 'Dimension error');

     iters := -width*sizeof(double);

     asm
        // helper registers for the mt1, mt2 and dest pointers
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
                prefetchw [ecx + eax];

                // sqrt:
                sqrtpd xmm0, [ecx + eax - 128];
                movntdq [ecx + eax - 128], xmm0;

                sqrtpd xmm1, [ecx + eax - 112];
                movntdq [ecx + eax - 112], xmm1;

                sqrtpd xmm2, [ecx + eax - 96];
                movntdq [ecx + eax - 96], xmm2;

                sqrtpd xmm3, [ecx + eax - 80];
                movntdq [ecx + eax - 80], xmm3;

                sqrtpd xmm4, [ecx + eax - 64];
                movntdq [ecx + eax - 64], xmm4;

                sqrtpd xmm5, [ecx + eax - 48];
                movntdq [ecx + eax - 48], xmm5;

                sqrtpd xmm6, [ecx + eax - 32];
                movntdq [ecx + eax - 32], xmm6;

                sqrtpd xmm7, [ecx + eax - 16];
                movntdq [ecx + eax - 16], xmm7;
            jmp @addforxloop

            @loopEnd:

            sub eax, 128;

            jz @nextLine;

            @addforxloop2:
                sqrtpd xmm0, [ecx + eax];
                movntdq [ecx + eax], xmm0;
            add eax, 16;
            jnz @addforxloop2;

            @nextLine:

            // next line:
            add ecx, LineWidth;

        // loop y end
        dec edx;
        jnz @@addforyloop;

        pop esi;
     end;
end;

procedure ASMMatrixSQRTUnAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt);
var iters : TASMNativeInt;
begin
     assert((width > 0) and (height > 0) and (LineWidth >= width*sizeof(double)), 'Dimension error');

     iters := -width*sizeof(double);

     asm
        // helper registers for the mt1, mt2 and dest pointers
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

                // sqrt:
                movupd xmm0, [ecx + eax - 128];
                sqrtpd xmm0, xmm0;
                movupd [ecx + eax - 128], xmm0;

                movupd xmm0, [ecx + eax - 112];
                sqrtpd xmm1, xmm1;
                movupd [ecx + eax - 112], xmm1;

                movupd xmm2, [ecx + eax - 96];
                sqrtpd xmm2, xmm2;
                movupd [ecx + eax - 96], xmm2;

                movupd xmm3, [ecx + eax - 80];
                sqrtpd xmm3, xmm3;
                movupd [ecx + eax - 80], xmm3;

                movupd xmm4, [ecx + eax - 64];
                sqrtpd xmm4, xmm4;
                movupd [ecx + eax - 64], xmm4;

                movupd xmm5, [ecx + eax - 48];
                sqrtpd xmm5, xmm5;
                movupd [ecx + eax - 48], xmm5;

                movupd xmm6, [ecx + eax - 32];
                sqrtpd xmm6, xmm6;
                movupd [ecx + eax - 32], xmm6;

                movupd xmm7, [ecx + eax - 16];
                sqrtpd xmm7, xmm7;
                movupd [ecx + eax - 16], xmm7;
            jmp @addforxloop

            @loopEnd:

            sub eax, 128;

            jz @nextLine;

            @addforxloop2:
                movupd xmm0, [ecx + eax];
                sqrtpd xmm0, xmm0;

                movupd [ecx + eax], xmm0;
            add eax, 16;
            jnz @addforxloop2;

            @nextLine:

            // next line:
            add ecx,LineWidth;

        // loop y end
        dec edx;
        jnz @@addforyloop;

        pop esi;
     end;
end;

procedure ASMMatrixSQRTAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt);
var iters : TASMNativeInt;
begin
     Assert((Cardinal(dest) and $0000000F = 0), 'Error non aligned data');
     Assert((width and 1) = 1, 'Error width must be odd');

     iters := -(width - 1)*sizeof(double);

     asm
        // helper registers for the mt1, mt2 and dest pointers
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
                prefetchw [ecx + eax];

                // addition:
                sqrtpd xmm0, [ecx + eax - 128];
                movntdq [ecx + eax - 128], xmm0;

                sqrtpd xmm1, [ecx + eax - 112];
                movntdq [ecx + eax - 112], xmm1;

                sqrtpd xmm2, [ecx + eax - 96];
                movntdq [ecx + eax - 96], xmm2;

                sqrtpd xmm3, [ecx + eax - 80];
                movntdq [ecx + eax - 80], xmm3;

                sqrtpd xmm4, [ecx + eax - 64];
                movntdq [ecx + eax - 64], xmm4;

                sqrtpd xmm5, [ecx + eax - 48];
                movntdq [ecx + eax - 48], xmm5;

                sqrtpd xmm6, [ecx + eax - 32];
                movntdq [ecx + eax - 32], xmm6;

                sqrtpd xmm7, [ecx + eax - 16];
                movntdq [ecx + eax - 16], xmm7;
            jmp @addforxloop

            @loopEnd:

            sub eax, 128;

            jz @nextLine;

            @addforxloop2:
                sqrtpd xmm0, [ecx + eax];
                movntdq [ecx + eax], xmm0;
            add eax, 16;
            jnz @addforxloop2;

            @nextLine:

            // special care of the last column:
            movlpd xmm0, [ecx];
            sqrtsd xmm0, xmm0;

            movlpd [ecx], xmm0;

            // next line:
            add ecx, LineWidth;

        // loop y end
        dec edx;
        jnz @@addforyloop;
     end;
end;

procedure ASMMatrixSQRTUnAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt);
var iters : TASMNativeInt;
begin
     Assert((Cardinal(dest) and $0000000F = 0), 'Error non aligned data');
     Assert((width and 1) = 1, 'Error width must be odd');

     iters := -(width - 1)*sizeof(double);

     // helper registers for the mt1, mt2 and dest pointers
     asm
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

                // sqrt:
                movupd xmm0, [ecx + eax - 128];
                sqrtpd xmm0, xmm0;
                movupd [ecx + eax - 128], xmm0;

                movupd xmm0, [ecx + eax - 112];
                sqrtpd xmm1, xmm1;
                movupd [ecx + eax - 112], xmm1;

                movupd xmm2, [ecx + eax - 96];
                sqrtpd xmm2, xmm2;
                movupd [ecx + eax - 96], xmm2;

                movupd xmm3, [ecx + eax - 80];
                sqrtpd xmm3, xmm3;
                movupd [ecx + eax - 80], xmm3;

                movupd xmm4, [ecx + eax - 64];
                sqrtpd xmm4, xmm4;
                movupd [ecx + eax - 64], xmm4;

                movupd xmm5, [ecx + eax - 48];
                sqrtpd xmm5, xmm5;
                movupd [ecx + eax - 48], xmm5;

                movupd xmm6, [ecx + eax - 32];
                sqrtpd xmm6, xmm6;
                movupd [ecx + eax - 32], xmm6;

                movupd xmm7, [ecx + eax - 16];
                sqrtpd xmm7, xmm7;
                movupd [ecx + eax - 16], xmm7;
            jmp @addforxloop

            @loopEnd:

            sub eax, 128;

            jz @nextLine;

            @addforxloop2:
                movupd xmm0, [ecx + eax];
                sqrtpd xmm0, xmm0;

                movupd [ecx + eax], xmm0;
            add eax, 16;
            jnz @addforxloop2;

            @nextLine:

            // special care of the last column:
            movlpd xmm0, [ecx];
            sqrtsd xmm0, xmm0;

            movlpd [ecx], xmm0;

            // next line:
            add ecx, LineWidth;

        // loop y end
        dec edx;
        jnz @@addforyloop;
     end;
end;

{$ENDIF}

end.
