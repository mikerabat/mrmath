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


unit ASMMatrixScaleOperations;

interface

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}
{$IFNDEF x64}

uses MatrixConst;

procedure ASMMatrixAddScaleAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);
procedure ASMMatrixAddScaleUnAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);

procedure ASMMatrixAddScaleAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);
procedure ASMMatrixAddScaleUnAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);

procedure ASMMatrixScaleAddAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);
procedure ASMMatrixScaleAddUnAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);

procedure ASMMatrixScaleAddAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);
procedure ASMMatrixScaleAddUnAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);

{$ENDIF}

implementation

{$IFNDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$ENDIF}

procedure ASMMatrixAddScaleAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);
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

        movddup xmm6, dOffset;
        movddup xmm7, Scale;

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

                // add scale:
                movapd xmm0, [ecx + eax - 128];
                addpd xmm0, xmm6;
                mulpd xmm0, xmm7;
                movntdq [ecx + eax - 128], xmm0;

                movapd xmm1, [ecx + eax - 112];
                addpd xmm1, xmm6;
                mulpd xmm1, xmm7;
                movntdq [ecx + eax - 112], xmm1;

                movapd xmm2, [ecx + eax - 96];
                addpd xmm2, xmm6;
                mulpd xmm2, xmm7;
                movntdq [ecx + eax - 96], xmm2;

                movapd xmm3, [ecx + eax - 80];
                addpd xmm3, xmm6;
                mulpd xmm3, xmm7;
                movntdq [ecx + eax - 80], xmm3;

                movapd xmm0, [ecx + eax - 64];
                addpd xmm0, xmm6;
                mulpd xmm0, xmm7;
                movntdq [ecx + eax - 64], xmm0;

                movapd xmm1, [ecx + eax - 48];
                addpd xmm1, xmm6;
                mulpd xmm1, xmm7;
                movntdq [ecx + eax - 48], xmm1;

                movapd xmm2, [ecx + eax - 32];
                addpd xmm2, xmm6;
                mulpd xmm2, xmm7;
                movntdq [ecx + eax - 32], xmm2;

                movapd xmm3, [ecx + eax - 16];
                addpd xmm3, xmm6;
                mulpd xmm3, xmm7;
                movntdq [ecx + eax - 16], xmm3;
            jmp @addforxloop

            @loopEnd:

            sub eax, 128;

            jz @nextLine;

            @addforxloop2:
                movapd xmm0, [ecx + eax];
                addpd xmm0, xmm6;
                mulpd xmm0, xmm7;
                movntdq [ecx + eax], xmm0;
            add eax, 16;
            jnz @addforxloop2;

            @nextLine:

            // next line:
            add ecx, LineWidth;

        // loop y end
        dec edx;
        jnz @@addforyloop;
     end;
end;

procedure ASMMatrixAddScaleUnAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);
var iters : TASMNativeInt;
begin
     Assert((width and 1) = 0, 'Error width must be even');

     assert((width > 0) and (height > 0) and (LineWidth >= width*sizeof(double)), 'Dimension error');

     iters := -width*sizeof(double);

     asm
        // helper registers for the mt1, mt2 and dest pointers
        mov ecx, dest;
        sub ecx, iters;

        movddup xmm6, dOffset;
        movddup xmm7, Scale;

        // for y := 0 to height - 1:
        mov edx, Height;
        @@addforyloop:
            // for x := 0 to w - 1;
            // prepare for reverse loop
            mov eax, iters;
            @addforxloop:
                add eax, 128;
                jg @loopEnd;

                // add scale:
                movupd xmm0, [ecx + eax - 128];
                addpd xmm0, xmm6;
                mulpd xmm0, xmm7;
                movupd [ecx + eax - 128], xmm0;

                movupd xmm1, [ecx + eax - 112];
                addpd xmm1, xmm6;
                mulpd xmm1, xmm7;
                movupd [ecx + eax - 112], xmm1;

                movupd xmm2, [ecx + eax - 96];
                addpd xmm2, xmm6;
                mulpd xmm2, xmm7;
                movupd [ecx + eax - 96], xmm2;

                movupd xmm3, [ecx + eax - 80];
                addpd xmm3, xmm6;
                mulpd xmm3, xmm7;
                movupd [ecx + eax - 80], xmm3;

                movupd xmm0, [ecx + eax - 64];
                addpd xmm0, xmm6;
                mulpd xmm0, xmm7;
                movupd [ecx + eax - 64], xmm0;

                movupd xmm1, [ecx + eax - 48];
                addpd xmm1, xmm6;
                mulpd xmm1, xmm7;
                movupd [ecx + eax - 48], xmm1;

                movupd xmm2, [ecx + eax - 32];
                addpd xmm2, xmm6;
                mulpd xmm2, xmm7;
                movupd [ecx + eax - 32], xmm2;

                movupd xmm3, [ecx + eax - 16];
                addpd xmm3, xmm6;
                mulpd xmm3, xmm7;
                movupd [ecx + eax - 16], xmm3;
            jmp @addforxloop

            @loopEnd:

            sub eax, 128;

            jz @nextLine;

            @addforxloop2:
                movupd xmm0, [ecx + eax];
                addpd xmm0, xmm6;
                mulpd xmm0, xmm7;
                movupd [ecx + eax], xmm0;
            add eax, 16;
            jnz @addforxloop2;

            @nextLine:

            // next line:
            add ecx, LineWidth;

        // loop y end
        dec edx;
        jnz @@addforyloop;
     end;
end;

procedure ASMMatrixAddScaleAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);
var iters : TASMNativeInt;
begin
     Assert((Cardinal(dest) and $0000000F = 0) and ((LineWidth and $0000000F) = 0), 'Error non aligned data');
     Assert((width and 1) = 1, 'Error width must be odd');

     iters := -(width - 1)*sizeof(double);

     asm
        // helper registers for the mt1, mt2 and dest pointers
        mov ecx, dest;
        sub ecx, iters;

        movddup xmm6, dOffset;
        movddup xmm7, Scale;

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

                // add mult:
                movapd xmm0, [ecx + eax - 128];
                addpd xmm0, xmm6;
                mulpd xmm0, xmm7;
                movntdq [ecx + eax - 128], xmm0;

                movapd xmm1, [ecx + eax - 112];
                addpd xmm1, xmm6;
                mulpd xmm1, xmm7;
                movntdq [ecx + eax - 112], xmm1;

                movapd xmm2, [ecx + eax - 96];
                addpd xmm2, xmm6;
                mulpd xmm2, xmm7;
                movntdq [ecx + eax - 96], xmm2;

                movapd xmm3, [ecx + eax - 80];
                addpd xmm3, xmm6;
                mulpd xmm3, xmm7;
                movntdq [ecx + eax - 80], xmm3;

                movapd xmm0, [ecx + eax - 64];
                addpd xmm0, xmm6;
                mulpd xmm0, xmm7;
                movntdq [ecx + eax - 64], xmm0;

                movapd xmm1, [ecx + eax - 48];
                addpd xmm1, xmm6;
                mulpd xmm1, xmm7;
                movntdq [ecx + eax - 48], xmm1;

                movapd xmm2, [ecx + eax - 32];
                addpd xmm2, xmm6;
                mulpd xmm2, xmm7;
                movntdq [ecx + eax - 32], xmm2;

                movapd xmm3, [ecx + eax - 16];
                addpd xmm3, xmm6;
                mulpd xmm3, xmm7;
                movntdq [ecx + eax - 16], xmm3;
            jmp @addforxloop

            @loopEnd:

            sub eax, 128;

            jz @nextLine;

            @addforxloop2:
                movapd xmm0, [ecx + eax];
                addpd xmm0, xmm6;
                mulpd xmm0, xmm7;
                movntdq [ecx + eax], xmm0;
            add eax, 16;
            jnz @addforxloop2;

            @nextLine:

            // special care of the last column:
            movsd xmm0, [ecx];
            addsd xmm0, xmm6;
            mulsd xmm0, xmm7;
            movsd [ecx], xmm0;

            // next line:
            add ecx, LineWidth;

        // loop y end
        dec edx;
        jnz @@addforyloop;
     end;
end;

procedure ASMMatrixAddScaleUnAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);
var iters : TASMNativeInt;
begin
     Assert((width and 1) = 1, 'Error width must be odd');

     iters := -(width - 1)*sizeof(double);

     asm
        // helper registers for the mt1, mt2 and dest pointers
        mov ecx, dest;
        sub ecx, iters;

        movddup xmm6, dOffset;
        movddup xmm7, Scale;

        // for y := 0 to height - 1:
        mov edx, Height;
        @@addforyloop:
            // for x := 0 to w - 1;
            // prepare for reverse loop
            mov eax, iters;
            @addforxloop:
                add eax, 128;
                jg @loopEnd;

                // add mult:
                movupd xmm0, [ecx + eax - 128];
                addpd xmm0, xmm6;
                mulpd xmm0, xmm7;
                movupd [ecx + eax - 128], xmm0;

                movupd xmm1, [ecx + eax - 112];
                addpd xmm1, xmm6;
                mulpd xmm1, xmm7;
                movupd [ecx + eax - 112], xmm1;

                movupd xmm2, [ecx + eax - 96];
                addpd xmm2, xmm6;
                mulpd xmm2, xmm7;
                movupd [ecx + eax - 96], xmm2;

                movupd xmm3, [ecx + eax - 80];
                addpd xmm3, xmm6;
                mulpd xmm3, xmm7;
                movupd [ecx + eax - 80], xmm3;

                movupd xmm0, [ecx + eax - 64];
                addpd xmm0, xmm6;
                mulpd xmm0, xmm7;
                movupd [ecx + eax - 64], xmm0;

                movupd xmm1, [ecx + eax - 48];
                addpd xmm1, xmm6;
                mulpd xmm1, xmm7;
                movupd [ecx + eax - 48], xmm1;

                movupd xmm2, [ecx + eax - 32];
                addpd xmm2, xmm6;
                mulpd xmm2, xmm7;
                movupd [ecx + eax - 32], xmm2;

                movupd xmm3, [ecx + eax - 16];
                addpd xmm3, xmm6;
                mulpd xmm3, xmm7;
                movupd [ecx + eax - 16], xmm3;
            jmp @addforxloop

            @loopEnd:

            sub eax, 128;

            jz @nextLine;

            @addforxloop2:
                movupd xmm0, [ecx + eax];
                addpd xmm0, xmm6;
                mulpd xmm0, xmm7;
                movupd [ecx + eax], xmm0;
            add eax, 16;
            jnz @addforxloop2;

            @nextLine:

            // special care of the last column:
            movsd xmm0, [ecx];
            addsd xmm0, xmm6;
            mulsd xmm0, xmm7;
            movsd [ecx], xmm0;

            // next line:
            add ecx, LineWidth;

        // loop y end
        dec edx;
        jnz @@addforyloop;
     end;
end;

procedure ASMMatrixScaleAddAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);
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

        movddup xmm6, dOffset;
        movddup xmm7, Scale;

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
                mulpd xmm0, xmm7;
                addpd xmm0, xmm6;
                movntdq [ecx + eax - 128], xmm0;

                movapd xmm1, [ecx + eax - 112];
                mulpd xmm1, xmm7;
                addpd xmm1, xmm6;
                movntdq [ecx + eax - 112], xmm1;

                movapd xmm2, [ecx + eax - 96];
                mulpd xmm2, xmm7;
                addpd xmm2, xmm6;
                movntdq [ecx + eax - 96], xmm2;

                movapd xmm3, [ecx + eax - 80];
                mulpd xmm3, xmm7;
                addpd xmm3, xmm6;
                movntdq [ecx + eax - 80], xmm3;

                movapd xmm0, [ecx + eax - 64];
                mulpd xmm0, xmm7;
                addpd xmm0, xmm6;
                movntdq [ecx + eax - 64], xmm0;

                movapd xmm1, [ecx + eax - 48];
                mulpd xmm1, xmm7;
                addpd xmm1, xmm6;
                movntdq [ecx + eax - 48], xmm1;

                movapd xmm2, [ecx + eax - 32];
                mulpd xmm2, xmm7;
                addpd xmm2, xmm6;
                movntdq [ecx + eax - 32], xmm2;

                movapd xmm3, [ecx + eax - 16];
                mulpd xmm3, xmm7;
                addpd xmm3, xmm6;
                movntdq [ecx + eax - 16], xmm3;
            jmp @addforxloop

            @loopEnd:

            sub eax, 128;

            jz @nextLine;

            @addforxloop2:
                movapd xmm0, [ecx + eax];
                mulpd xmm0, xmm7;
                addpd xmm0, xmm6;
                movntdq [ecx + eax], xmm0;
            add eax, 16;
            jnz @addforxloop2;

            @nextLine:

            // next line:
            add ecx, LineWidth;

        // loop y end
        dec edx;
        jnz @@addforyloop;
     end;
end;

procedure ASMMatrixScaleAddUnAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);
var iters : TASMNativeInt;
begin
     Assert((width and 1) = 0, 'Error width must be even');
     Assert((width > 0) and (height > 0) and (LineWidth >= width*sizeof(double)), 'Dimension error');

     iters := -width*sizeof(double);

     asm
        // helper registers for the mt1, mt2 and dest pointers
        mov ecx, dest;
        sub ecx, iters;

        movddup xmm6, dOffset;
        movddup xmm7, Scale;

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
                mulpd xmm0, xmm7;
                addpd xmm0, xmm6;
                movupd [ecx + eax - 128], xmm0;

                movupd xmm1, [ecx + eax - 112];
                mulpd xmm1, xmm7;
                addpd xmm1, xmm6;
                movupd [ecx + eax - 112], xmm1;

                movupd xmm2, [ecx + eax - 96];
                mulpd xmm2, xmm7;
                addpd xmm2, xmm6;
                movupd [ecx + eax - 96], xmm2;

                movupd xmm3, [ecx + eax - 80];
                mulpd xmm3, xmm7;
                addpd xmm3, xmm6;
                movupd [ecx + eax - 80], xmm3;

                movupd xmm0, [ecx + eax - 64];
                mulpd xmm0, xmm7;
                addpd xmm0, xmm6;
                movupd [ecx + eax - 64], xmm0;

                movupd xmm1, [ecx + eax - 48];
                mulpd xmm1, xmm7;
                addpd xmm1, xmm6;
                movupd [ecx + eax - 48], xmm1;

                movupd xmm2, [ecx + eax - 32];
                mulpd xmm2, xmm7;
                addpd xmm2, xmm6;
                movupd [ecx + eax - 32], xmm2;

                movupd xmm3, [ecx + eax - 16];
                mulpd xmm3, xmm7;
                addpd xmm3, xmm6;
                movupd [ecx + eax - 16], xmm3;
            jmp @addforxloop

            @loopEnd:

            sub eax, 128;

            jz @nextLine;

            @addforxloop2:
                movupd xmm0, [ecx + eax];
                mulpd xmm0, xmm7;
                addpd xmm0, xmm6;
                movupd [ecx + eax], xmm0;
            add eax, 16;
            jnz @addforxloop2;

            @nextLine:

            // next line:
            add ecx, LineWidth;

        // loop y end
        dec edx;
        jnz @@addforyloop;
     end;
end;

procedure ASMMatrixScaleAddAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);
var iters : TASMNativeInt;
begin
     Assert((Cardinal(dest) and $0000000F = 0) and ((LineWidth and $0000000F) = 0), 'Error non aligned data');
     Assert((width and 1) = 1, 'Error width must be odd');

     iters := -(width - 1)*sizeof(double);

     asm
        // helper registers for the mt1, mt2 and dest pointers
        mov ecx, dest;
        sub ecx, iters;

        movddup xmm6, dOffset;
        movddup xmm7, Scale;

        // for y := 0 to height - 1:
        mov edx, Height;
        @@addforyloop:
            // for x := 0 to w - 1;
            // prepare for reverse loop
            mov eax, iters;
            @addforxloop:
                add eax, 128;
                jg @loopEnd;

                // mul add
                movapd xmm0, [ecx + eax - 128];
                mulpd xmm0, xmm7;
                addpd xmm0, xmm6;
                movapd [ecx + eax - 128], xmm0;

                movapd xmm1, [ecx + eax - 112];
                mulpd xmm1, xmm7;
                addpd xmm1, xmm6;
                movapd [ecx + eax - 112], xmm1;

                movapd xmm2, [ecx + eax - 96];
                mulpd xmm2, xmm7;
                addpd xmm2, xmm6;
                movapd [ecx + eax - 96], xmm2;

                movapd xmm3, [ecx + eax - 80];
                mulpd xmm3, xmm7;
                addpd xmm3, xmm6;
                movapd [ecx + eax - 80], xmm3;

                movapd xmm0, [ecx + eax - 64];
                mulpd xmm0, xmm7;
                addpd xmm0, xmm6;
                movapd [ecx + eax - 64], xmm0;

                movapd xmm1, [ecx + eax - 48];
                mulpd xmm1, xmm7;
                addpd xmm1, xmm6;
                movapd [ecx + eax - 48], xmm1;

                movapd xmm2, [ecx + eax - 32];
                mulpd xmm2, xmm7;
                addpd xmm2, xmm6;
                movapd [ecx + eax - 32], xmm2;

                movapd xmm3, [ecx + eax - 16];
                mulpd xmm3, xmm7;
                addpd xmm3, xmm6;
                movapd [ecx + eax - 16], xmm3;
            jmp @addforxloop

            @loopEnd:

            sub eax, 128;

            jz @nextLine;

            @addforxloop2:
                movapd xmm0, [ecx + eax];
                mulpd xmm0, xmm7;
                addpd xmm0, xmm6;
                movapd [ecx + eax], xmm0;
            add eax, 16;
            jnz @addforxloop2;

            @nextLine:
            // special care of the last column:
            movsd xmm0, [ecx];
            mulsd xmm0, xmm7;
            addsd xmm0, xmm6;
            movsd [ecx], xmm0;

            // next line:
            add ecx, LineWidth;

        // loop y end
        dec edx;
        jnz @@addforyloop;
     end;
end;

procedure ASMMatrixScaleAddUnAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);
var iters : TASMNativeInt;
begin
     Assert((width and 1) = 1, 'Error width must be odd');

     iters := -(width - 1)*sizeof(double);

     asm
        // helper registers for the mt1, mt2 and dest pointers
        mov ecx, dest;
        sub ecx, iters;

        movddup xmm6, dOffset;
        movddup xmm7, Scale;

        // for y := 0 to height - 1:
        mov edx, Height;
        @@addforyloop:
            // for x := 0 to w - 1;
            // prepare for reverse loop
            mov eax, iters;
            @addforxloop:
                add eax, 128;
                jg @loopEnd;

                // mul add
                movupd xmm0, [ecx + eax - 128];
                mulpd xmm0, xmm7;
                addpd xmm0, xmm6;
                movupd [ecx + eax - 128], xmm0;

                movupd xmm1, [ecx + eax - 112];
                mulpd xmm1, xmm7;
                addpd xmm1, xmm6;
                movupd [ecx + eax - 112], xmm1;

                movupd xmm2, [ecx + eax - 96];
                mulpd xmm2, xmm7;
                addpd xmm2, xmm6;
                movupd [ecx + eax - 96], xmm2;

                movupd xmm3, [ecx + eax - 80];
                mulpd xmm3, xmm7;
                addpd xmm3, xmm6;
                movupd [ecx + eax - 80], xmm3;

                movupd xmm0, [ecx + eax - 64];
                mulpd xmm0, xmm7;
                addpd xmm0, xmm6;
                movupd [ecx + eax - 64], xmm0;

                movupd xmm1, [ecx + eax - 48];
                mulpd xmm1, xmm7;
                addpd xmm1, xmm6;
                movupd [ecx + eax - 48], xmm1;

                movupd xmm2, [ecx + eax - 32];
                mulpd xmm2, xmm7;
                addpd xmm2, xmm6;
                movupd [ecx + eax - 32], xmm2;

                movupd xmm3, [ecx + eax - 16];
                mulpd xmm3, xmm7;
                addpd xmm3, xmm6;
                movupd [ecx + eax - 16], xmm3;
            jmp @addforxloop

            @loopEnd:

            sub eax, 128;

            jz @nextLine;

            @addforxloop2:
                movupd xmm0, [ecx + eax];
                mulpd xmm0, xmm7;
                addpd xmm0, xmm6;
                movupd [ecx + eax], xmm0;
            add eax, 16;
            jnz @addforxloop2;

            @nextLine:
            // special care of the last column:
            movsd xmm0, [ecx];
            mulsd xmm0, xmm7;
            addsd xmm0, xmm6;
            movsd [ecx], xmm0;

            // next line:
            add ecx, LineWidth;

        // loop y end
        dec edx;
        jnz @@addforyloop;
     end;
end;

{$ENDIF}

end.
