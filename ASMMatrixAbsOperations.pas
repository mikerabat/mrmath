unit ASMMatrixAbsOperations;

// #####################################################
// #### SQRT opertaion applied to every element in a matrix
// #####################################################

interface

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}
{$IFNDEF x64}

uses MatrixConst;

procedure ASMMatrixAbsAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt);
procedure ASMMatrixAbsUnAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt);

procedure ASMMatrixAbsAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt);
procedure ASMMatrixAbsUnAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt);

{$ENDIF}

implementation

{$IFNDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$ENDIF}

procedure ASMMatrixAbsAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt);
var iters : TASMNativeInt;
begin
     Assert((Cardinal(dest) and $0000000F = 0), 'Error non aligned data');
     assert((width > 0) and (height > 0) and (LineWidth >= width*sizeof(double)), 'Dimension error');

     iters := -width*sizeof(double);

     asm
        // helper registers for the mt1, mt2 and dest pointers
        mov ecx, dest;
        sub ecx, iters;
        movupd xmm0, cSignBits;

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
                //prefetchw [ecx + eax];

                // Abs:
                movapd xmm4, [ecx + eax - 128];
                Andpd xmm4, xmm0;
                movntdq [ecx + eax - 128], xmm4;

                movapd xmm1, [ecx + eax - 112];
                andpd xmm1, xmm0;
                movntdq [ecx + eax - 112], xmm1;

                movapd xmm2, [ecx + eax - 96];
                andpd xmm2, xmm0;
                movntdq [ecx + eax - 96], xmm2;

                movapd xmm3, [ecx + eax - 80];
                andpd xmm3, xmm0;
                movntdq [ecx + eax - 80], xmm3;

                movapd xmm4, [ecx + eax - 64];
                andpd xmm4, xmm0;
                movntdq [ecx + eax - 64], xmm4;

                movapd xmm5, [ecx + eax - 48];
                andpd xmm5, xmm0;
                movntdq [ecx + eax - 48], xmm5;

                movapd xmm6, [ecx + eax - 32];
                andpd xmm6, xmm0;
                movntdq [ecx + eax - 32], xmm6;

                movapd xmm7, [ecx + eax - 16];
                andpd xmm7, xmm0;
                movntdq [ecx + eax - 16], xmm7;
            jmp @addforxloop

            @loopEnd:

            sub eax, 128;

            jz @nextLine;

            @addforxloop2:
                movapd xmm1, [ecx + eax];
                andpd xmm1, xmm0;
                movntdq [ecx + eax], xmm1;
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

procedure ASMMatrixAbsUnAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt);
var iters : TASMNativeInt;
begin
     assert((width > 0) and (height > 0) and (LineWidth >= width*sizeof(double)), 'Dimension error');

     iters := -width*sizeof(double);

     asm
        // helper registers for the mt1, mt2 and dest pointers
        mov ecx, dest;
        sub ecx, iters;

        movupd xmm0, cSignBits;

        // for y := 0 to height - 1:
        mov edx, Height;
        @@addforyloop:
            // for x := 0 to w - 1;
            // prepare for reverse loop
            mov eax, iters;
            @addforxloop:
                add eax, 128;
                jg @loopEnd;

                // Abs:
                movupd xmm4, [ecx + eax - 128];
                Andpd xmm4, xmm0;
                movupd [ecx + eax - 128], xmm4;

                movupd xmm1, [ecx + eax - 112];
                andpd xmm1, xmm0;
                movupd [ecx + eax - 112], xmm1;

                movupd xmm2, [ecx + eax - 96];
                andpd xmm2, xmm0;
                movupd [ecx + eax - 96], xmm2;

                movupd xmm3, [ecx + eax - 80];
                andpd xmm3, xmm0;
                movupd [ecx + eax - 80], xmm3;

                movupd xmm4, [ecx + eax - 64];
                andpd xmm4, xmm0;
                movupd [ecx + eax - 64], xmm4;

                movupd xmm5, [ecx + eax - 48];
                andpd xmm5, xmm0;
                movupd [ecx + eax - 48], xmm5;

                movupd xmm6, [ecx + eax - 32];
                andpd xmm6, xmm0;
                movupd [ecx + eax - 32], xmm6;

                movupd xmm7, [ecx + eax - 16];
                andpd xmm7, xmm0;
                movupd [ecx + eax - 16], xmm7;
            jmp @addforxloop

            @loopEnd:

            sub eax, 128;

            jz @nextLine;

            @addforxloop2:
                movupd xmm1, [ecx + eax];
                andpd xmm1, xmm0;

                movupd [ecx + eax], xmm1;
            add eax, 16;
            jnz @addforxloop2;

            @nextLine:

            // next line:
            add ecx,LineWidth;

        // loop y end
        dec edx;
        jnz @@addforyloop;
     end;
end;

procedure ASMMatrixAbsAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt);
var iters : TASMNativeInt;
begin
     Assert((Cardinal(dest) and $0000000F = 0), 'Error non aligned data');
     Assert((width and 1) = 1, 'Error width must be odd');

     iters := -(width - 1)*sizeof(double);

     asm
        // helper registers for the mt1, mt2 and dest pointers
        mov ecx, dest;
        sub ecx, iters;

        movupd xmm0, cSignBits;

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
                //prefetchw [ecx + eax];

                // Abs:
                movapd xmm4, [ecx + eax - 128];
                Andpd xmm4, xmm0;
                movntdq [ecx + eax - 128], xmm4;

                movapd xmm1, [ecx + eax - 112];
                andpd xmm1, xmm0;
                movntdq [ecx + eax - 112], xmm1;

                movapd xmm2, [ecx + eax - 96];
                andpd xmm2, xmm0;
                movntdq [ecx + eax - 96], xmm2;

                movapd xmm3, [ecx + eax - 80];
                andpd xmm3, xmm0;
                movntdq [ecx + eax - 80], xmm3;

                movapd xmm4, [ecx + eax - 64];
                andpd xmm4, xmm0;
                movntdq [ecx + eax - 64], xmm4;

                movapd xmm5, [ecx + eax - 48];
                andpd xmm5, xmm0;
                movntdq [ecx + eax - 48], xmm5;

                movapd xmm6, [ecx + eax - 32];
                andpd xmm6, xmm0;
                movntdq [ecx + eax - 32], xmm6;

                movapd xmm7, [ecx + eax - 16];
                andpd xmm7, xmm0;
                movntdq [ecx + eax - 16], xmm7;
            jmp @addforxloop

            @loopEnd:

            sub eax, 128;

            jz @nextLine;

            @addforxloop2:
                movapd xmm1, [ecx + eax - 16];
                andpd xmm1, xmm0;
                movntdq [ecx + eax], xmm1;
            add eax, 16;
            jnz @addforxloop2;

            @nextLine:

            // special care of the last column:
            movlpd xmm1, [ecx];
            andpd xmm1, xmm0;

            movlpd [ecx], xmm1;

            // next line:
            add ecx, LineWidth;

        // loop y end
        dec edx;
        jnz @@addforyloop;
     end;
end;

procedure ASMMatrixAbsUnAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt);
var iters : TASMNativeInt;
begin
     Assert((width and 1) = 1, 'Error width must be odd');

     iters := -(width - 1)*sizeof(double);

     // helper registers for the mt1, mt2 and dest pointers
     asm
        mov ecx, dest;
        sub ecx, iters;

        movupd xmm0, cSignBits;

        // for y := 0 to height - 1:
        mov edx, Height;
        @@addforyloop:
            // for x := 0 to w - 1;
            // prepare for reverse loop
            mov eax, iters;
            @addforxloop:
                add eax, 128;
                jg @loopEnd;

                // Abs:
                movupd xmm4, [ecx + eax - 128];
                Andpd xmm4, xmm0;
                movupd [ecx + eax - 128], xmm4;

                movupd xmm1, [ecx + eax - 112];
                andpd xmm1, xmm0;
                movupd [ecx + eax - 112], xmm1;

                movupd xmm2, [ecx + eax - 96];
                andpd xmm2, xmm0;
                movupd [ecx + eax - 96], xmm2;

                movupd xmm3, [ecx + eax - 80];
                andpd xmm3, xmm0;
                movupd [ecx + eax - 80], xmm3;

                movupd xmm4, [ecx + eax - 64];
                andpd xmm4, xmm0;
                movupd [ecx + eax - 64], xmm4;

                movupd xmm5, [ecx + eax - 48];
                andpd xmm5, xmm0;
                movupd [ecx + eax - 48], xmm5;

                movupd xmm6, [ecx + eax - 32];
                andpd xmm6, xmm0;
                movupd [ecx + eax - 32], xmm6;

                movupd xmm7, [ecx + eax - 16];
                andpd xmm7, xmm0;
                movupd [ecx + eax - 16], xmm7;
            jmp @addforxloop

            @loopEnd:

            sub eax, 128;

            jz @nextLine;

            @addforxloop2:
                movupd xmm1, [ecx + eax];
                andpd xmm1, xmm0;

                movupd [ecx + eax], xmm1;
            add eax, 16;
            jnz @addforxloop2;

            @nextLine:

            // special care of the last column:
            movlpd xmm1, [ecx];
            andpd xmm1, xmm0;

            movlpd [ecx], xmm1;

            // next line:
            add ecx, LineWidth;

        // loop y end
        dec edx;
        jnz @@addforyloop;
     end;
end;

{$ENDIF}

end.
