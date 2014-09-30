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


unit ASMMatrixMultOperationsx64;

interface

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF x64}

uses MatrixConst;

// full matrix operations -> there are no zero columns nor zero lines attached
procedure ASMMatrixMultAlignedEvenW1EvenW2(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1, height1, width2, height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
procedure ASMMatrixMultUnAlignedEvenW1EvenW2(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1, height1, width2, height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);

// the width of the second matrix is uneven -> the last column of mt2 must be handled differently
procedure ASMMatrixMultAlignedEvenW1OddW2(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1, height1, width2, height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
procedure ASMMatrixMultUnAlignedEvenW1OddW2(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1, height1, width2, height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);

// the width of mt1 is odd but the one of mt2 is even
procedure ASMMatrixMultAlignedOddW1EvenW2(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1, height1, width2, height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
procedure ASMMatrixMultUnAlignedOddW1EvenW2(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1, height1, width2, height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);

// both matrices have an odd width
procedure ASMMatrixMultAlignedOddW1OddW2(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1, height1, width2, height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
procedure ASMMatrixMultUnAlignedOddW1OddW2(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1, height1, width2, height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);

{$ENDIF}

implementation

{$IFDEF FPC} {$ASMMODE intel} {$ENDIF}

{$IFDEF x64}
procedure ASMMatrixMultAlignedEvenW1EvenW2(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1, height1, width2, height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var dXMM4, dXMM5, dXMM6, dXMM7 : Array[0..1] of double;
    iRBX, iRSI, iRDI, iR12, iR13, iR14, iR15 : NativeInt;

{$IFDEF FPC}
begin
  {$ENDIF}

asm
    // note: RCX = dest, RDX = destLineWidth, R8 = mt1, R9 = mt2
    // prolog - simulate stack
    mov iRBX, rbx;
    mov iRSI, rsi;
    mov iRDI, rdi;
    mov iR12, r12;
    mov iR13, r13;
    mov iR14, r14;
    mov iR15, r15;

    movupd dXMM4, xmm4;
    movupd dXMM5, xmm5;
    movupd dXMM6, xmm6;
    movupd dXMM7, xmm7;

    {
   .pushnv rbx;
   .pushnv rsi;
   .pushnv rdi;
   .pushnv r12;
   .pushnv r13;
   .pushnv r14;
   .pushnv r15;

   .savenv xmm4;
   .savenv xmm5;
   .savenv xmm6;
   .savenv xmm7;
   }

    //destOffset := destLineWidth - Width2*sizeof(double);
    mov rbx, Width2;
    shl rbx, 3;
    mov r15, destLineWidth;
    sub r15, rbx;

    //bytesWidth2 := width2*sizeof(double);
    mov r11, rbx;

    //iters1 := width2 div 2;
    mov r14, width2;
    shr r14, 1;

    //iters2 := -width1*sizeof(double);
    mov rbx, width1;
    shl rbx, 3;
    imul rbx, -1;
    mov r13, rbx;

    //Linewidth2_2 := 2*LineWidth2;
    mov r12, LineWidth2;
    shl r12, 1;

    // initalize matrices
    sub r8, r13;    // r8 = mt1

    // for y := 0 to height1 - 1:
    mov rsi, Height1;

    mov rax, LineWidth2;
    @@forylabel:
        // for x := 0 to width2 - 1:
        mov rbx, r14;
        @@forxlabel:

            xorpd xmm0, xmm0;   // dest^ := 0
            xorpd xmm7, xmm7;   // (dest + 1)^ := 0;

            mov rdi, r9;       // valcounter2

            // for idx := 0 to width1 div 2 do
            mov rdx, r13;

            @@InnerLoop:
                movapd xmm1, [r8 + rdx];

                // load 2x2 block
                movapd xmm3, [rdi];
                movapd xmm5, xmm3;

                movapd xmm4, [rdi + rax];
                add rdi, r12;
                movapd xmm6, xmm4;

                // swap such that we can immediately multiply
                movhlps xmm4, xmm5;
                movlhps xmm3, xmm6;

                // it's a bit faster if we reuse a register - seems the cpu can
                // then parallelize the instructions better
                movapd xmm6, xmm1;

                // multiply 2x2 and add
                mulpd xmm3, xmm1;
                mulpd xmm4, xmm6;
                addpd xmm0, xmm3;
                addpd xmm7, xmm4;

                // end for idx := 0 to width1 div 2
            //dec edx;
            add rdx, 16;
            jnz @@InnerLoop;

            // final horizontal addition
            xorpd xmm2, xmm2;
            haddpd xmm0, xmm2;
            haddpd xmm7, xmm2;

            // compact result
            movlhps xmm0, xmm7;

            // store back result
            movntdq [rcx], xmm0;

            // increment the pointers
            // inc(mt2), inc(dest);
            //add dword ptr [mt2], 8;
            add r9, 16;
            add rcx, 16;

        // end for x := 0 to width2 - 1
        dec rbx;
        jnz @@forxlabel;

        // dec(mt2, Width2);
        // inc(PByte(mt1), LineWidth1);
        // inc(PByte(dest), destOffset);
        //mov ebx, bytesWidth2;
        //sub dword ptr [mt2], ebx;
        sub r9, r11;
        add r8, LineWidth1;
        add rcx, r15;

    // end for y := 0 to height1 - 1
    //dec eax;
    dec rsi;
    jnz @@forylabel;


    // epilog
    mov rbx, iRBX;
    mov rsi, iRSI;
    mov rdi, iRDI;
    mov r12, iR12;
    mov r13, iR13;
    mov r14, iR14;
    mov r15, iR15;

    movupd xmm4, dXMM4;
    movupd xmm5, dXMM5;
    movupd xmm6, dXMM6;
    movupd xmm7, dXMM7;
end;
{$IFDEF FPC}
end;
{$ENDIF}

procedure ASMMatrixMultUnAlignedEvenW1EvenW2(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1, height1, width2, height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var dXMM4, dXMM5, dXMM6, dXMM7 : Array[0..1] of double;
    iRBX, iRSI, iRDI, iR12, iR13, iR14, iR15 : NativeInt;

{$IFDEF FPC}
begin
  {$ENDIF}

asm
    // note: RCX = dest, RDX = destLineWidth, R8 = mt1, R9 = mt2
    // prolog - simulate stack
    mov iRBX, rbx;
    mov iRSI, rsi;
    mov iRDI, rdi;
    mov iR12, r12;
    mov iR13, r13;
    mov iR14, r14;
    mov iR15, r15;

    movupd dXMM4, xmm4;
    movupd dXMM5, xmm5;
    movupd dXMM6, xmm6;
    movupd dXMM7, xmm7;
    {
   .pushnv rbx;
   .pushnv rsi;
   .pushnv rdi;
   .pushnv r12;
   .pushnv r13;
   .pushnv r14;
   .pushnv r15;

   .savenv xmm4;
   .savenv xmm5;
   .savenv xmm6;
   .savenv xmm7;
   }
    //destOffset := destLineWidth - Width2*sizeof(double);
    mov rbx, Width2;
    shl rbx, 3;
    mov r15, destLineWidth;
    sub r15, rbx;

    //bytesWidth2 := width2*sizeof(double);
    mov r11, rbx;

    //iters1 := width2 div 2;
    mov r14, width2;
    shr r14, 1;

    //iters2 := -width1*sizeof(double);
    mov rbx, width1;
    shl rbx, 3;
    imul rbx, -1;
    mov r13, rbx;

    //Linewidth2_2 := 2*LineWidth2;
    mov r12, LineWidth2;
    shl r12, 1;

    // initalize matrices
    sub r8, r13;    // r8 = mt1

    // for y := 0 to height1 - 1:
    mov rsi, Height1;

    mov rax, LineWidth2;
    @@forylabel:
        // for x := 0 to width2 - 1:
        mov rbx, r14;
        @@forxlabel:

            xorpd xmm0, xmm0;   // dest^ := 0
            xorpd xmm7, xmm7;   // (dest + 1)^ := 0;

            mov rdi, r9;       // valcounter2

            // for idx := 0 to width1 div 2 do
            mov rdx, r13;

            @@InnerLoop:
                movupd xmm1, [r8 + rdx];

                // load 2x2 block
                movupd xmm3, [rdi];
                movupd xmm5, xmm3;

                movupd xmm4, [rdi + rax];
                add rdi, r12;
                movupd xmm6, xmm4;

                // swap such that we can immediately multiply
                movhlps xmm4, xmm5;
                movlhps xmm3, xmm6;

                // it's a bit faster if we reuse a register - seems the cpu can
                // then parallelize the instructions better
                movupd xmm6, xmm1;

                // multiply 2x2 and add
                mulpd xmm3, xmm1;
                mulpd xmm4, xmm6;
                addpd xmm0, xmm3;
                addpd xmm7, xmm4;

                // end for idx := 0 to width1 div 2
            //dec edx;
            add rdx, 16;
            jnz @@InnerLoop;

            // final horizontal addition
            xorpd xmm2, xmm2;
            haddpd xmm0, xmm2;
            haddpd xmm7, xmm2;

            // compact result
            movlhps xmm0, xmm7;

            // store back result
            movupd [rcx], xmm0;

            // increment the pointers
            // inc(mt2), inc(dest);
            //add dword ptr [mt2], 8;
            add r9, 16;
            add rcx, 16;

        // end for x := 0 to width2 - 1
        dec rbx;
        jnz @@forxlabel;

        // dec(mt2, Width2);
        // inc(PByte(mt1), LineWidth1);
        // inc(PByte(dest), destOffset);
        //mov ebx, bytesWidth2;
        //sub dword ptr [mt2], ebx;
        sub r9, r11;
        add r8, LineWidth1;
        add rcx, r15;

    // end for y := 0 to height1 - 1
    //dec eax;
    dec rsi;
    jnz @@forylabel;

    // epilog
    mov rbx, iRBX;
    mov rsi, iRSI;
    mov rdi, iRDI;
    mov r12, iR12;
    mov r13, iR13;
    mov r14, iR14;
    mov r15, iR15;

    movupd xmm4, dXMM4;
    movupd xmm5, dXMM5;
    movupd xmm6, dXMM6;
    movupd xmm7, dXMM7;
end;
{$IFDEF FPC}
end;
{$ENDIF}

// the width of the second matrix is uneven -> the last column of mt2 must be handled differently
procedure ASMMatrixMultAlignedEvenW1OddW2(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1, height1, width2, height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var dXMM4, dXMM5, dXMM6, dXMM7 : Array[0..1] of double;
    iRBX, iRSI, iRDI, iR12, iR13, iR14, iR15 : NativeInt;

{$IFDEF FPC}
begin
  {$ENDIF}

asm
    // note: RCX = dest, RDX = destLineWidth, R8 = mt1, R9 = mt2
    // prolog - simulate stack
    mov iRBX, rbx;
    mov iRSI, rsi;
    mov iRDI, rdi;
    mov iR12, r12;
    mov iR13, r13;
    mov iR14, r14;
    mov iR15, r15;

    movupd dXMM4, xmm4;
    movupd dXMM5, xmm5;
    movupd dXMM6, xmm6;
    movupd dXMM7, xmm7;
    {
   .pushnv rbx;
   .pushnv rsi;
   .pushnv rdi;
   .pushnv r12;
   .pushnv r13;
   .pushnv r14;
   .pushnv r15;

   .savenv xmm4;
   .savenv xmm5;
   .savenv xmm6;
   .savenv xmm7;
   }

    //destOffset := destLineWidth - (Width2 - 1)*sizeof(double);
    mov rbx, Width2;
    dec rbx;
    shl rbx, 3;
    mov r15, destLineWidth;
    sub r15, rbx;

    //bytesWidth2 := (width2 - 1)*sizeof(double);
    mov r11, rbx;

    //iters1 := width2 div 2;
    mov r14, width2;
    shr r14, 1;

    //iters2 := -width1*sizeof(double);
    mov rbx, width1;
    shl rbx, 3;
    imul rbx, -1;
    mov r13, rbx;

    //Linewidth2_2 := 2*LineWidth2;
    mov r12, LineWidth2;
    shl r12, 1;

    // initalize matrices
    sub r8, r13;    // r8 = mt1

    // for y := 0 to height1 - 1:
    mov rsi, Height1;

    mov rax, LineWidth2;
    @@forylabel:
        // for x := 0 to width2 - 1:
        mov rbx, r14;
        @@forxlabel:

            xorpd xmm0, xmm0;   // dest^ := 0
            xorpd xmm7, xmm7;   // (dest + 1)^ := 0;

            mov rdi, r9;       // valcounter2

            // for idx := 0 to width1 div 2 do
            mov rdx, r13;

            @@InnerLoop:
                movapd xmm1, [r8 + rdx];

                // load 2x2 block
                movapd xmm3, [rdi];
                movapd xmm5, xmm3;

                movapd xmm4, [rdi + rax];
                add rdi, r12;
                movapd xmm6, xmm4;

                // swap such that we can immediately multiply
                movhlps xmm4, xmm5;
                movlhps xmm3, xmm6;

                // it's a bit faster if we reuse a register - seems the cpu can
                // then parallelize the instructions better
                movapd xmm6, xmm1;

                // multiply 2x2 and add
                mulpd xmm3, xmm1;
                mulpd xmm4, xmm6;
                addpd xmm0, xmm3;
                addpd xmm7, xmm4;

                // end for idx := 0 to width1 div 2
            //dec edx;
            add rdx, 16;
            jnz @@InnerLoop;

            // final horizontal addition
            xorpd xmm2, xmm2;
            haddpd xmm0, xmm2;
            haddpd xmm7, xmm2;

            // compact result
            movlhps xmm0, xmm7;

            // store back result
            movntdq [rcx], xmm0;

            // increment the pointers
            // inc(mt2), inc(dest);
            //add dword ptr [mt2], 8;
            add r9, 16;
            add rcx, 16;

        // end for x := 0 to width2 - 1
        dec rbx;
        jnz @@forxlabel;

        // handle the last column of mt2
        xorpd xmm0, xmm0;   // dest^ := 0
        mov rdi, r9;       // valcounter2

        mov rdx, r13;
        @@InnerLoop2:
            // load element from line
            movlpd xmm1, [r8 + rdx];

            // load, multiply and add
            mulsd xmm1, [rdi];
            add rdi, rax;       // next row

            addsd xmm0, xmm1;
        add rdx, 8;
        jnz @@InnerLoop2;

        movlpd [rcx], xmm0;

        // dec(mt2, Width2);
        // inc(PByte(mt1), LineWidth1);
        // inc(PByte(dest), destOffset);
        //mov ebx, bytesWidth2;
        //sub dword ptr [mt2], ebx;
        sub r9, r11;
        add r8, LineWidth1;
        add rcx, r15;

    // end for y := 0 to height1 - 1
    //dec eax;
    dec rsi;
    jnz @@forylabel;

    // epilog
    mov rbx, iRBX;
    mov rsi, iRSI;
    mov rdi, iRDI;
    mov r12, iR12;
    mov r13, iR13;
    mov r14, iR14;
    mov r15, iR15;

    movupd xmm4, dXMM4;
    movupd xmm5, dXMM5;
    movupd xmm6, dXMM6;
    movupd xmm7, dXMM7;
end;
{$IFDEF FPC}
end;
{$ENDIF}

procedure ASMMatrixMultUnAlignedEvenW1OddW2(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1, height1, width2, height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var dXMM4, dXMM5, dXMM6, dXMM7 : Array[0..1] of double;
    iRBX, iRSI, iRDI, iR12, iR13, iR14, iR15 : NativeInt;

{$IFDEF FPC}
begin
  {$ENDIF}

asm
    // note: RCX = dest, RDX = destLineWidth, R8 = mt1, R9 = mt2
    // prolog - simulate stack
    mov iRBX, rbx;
    mov iRSI, rsi;
    mov iRDI, rdi;
    mov iR12, r12;
    mov iR13, r13;
    mov iR14, r14;
    mov iR15, r15;

    movupd dXMM4, xmm4;
    movupd dXMM5, xmm5;
    movupd dXMM6, xmm6;
    movupd dXMM7, xmm7;
    {
   .pushnv rbx;
   .pushnv rsi;
   .pushnv rdi;
   .pushnv r12;
   .pushnv r13;
   .pushnv r14;
   .pushnv r15;

   .savenv xmm4;
   .savenv xmm5;
   .savenv xmm6;
   .savenv xmm7;
   }

    //destOffset := destLineWidth - (Width2 - 1)*sizeof(double);
    mov rbx, Width2;
    dec rbx;
    shl rbx, 3;
    mov r15, destLineWidth;
    sub r15, rbx;

    //bytesWidth2 := (width2 - 1)*sizeof(double);
    mov r11, rbx;

    //iters1 := width2 div 2;
    mov r14, width2;
    shr r14, 1;

    //iters2 := -width1*sizeof(double);
    mov rbx, width1;
    shl rbx, 3;
    imul rbx, -1;
    mov r13, rbx;

    //Linewidth2_2 := 2*LineWidth2;
    mov r12, LineWidth2;
    shl r12, 1;

    // initalize matrices
    sub r8, r13;    // r8 = mt1

    // for y := 0 to height1 - 1:
    mov rsi, Height1;

    mov rax, LineWidth2;
    @@forylabel:
        // for x := 0 to width2 - 1:
        mov rbx, r14;
        @@forxlabel:

            xorpd xmm0, xmm0;   // dest^ := 0
            xorpd xmm7, xmm7;   // (dest + 1)^ := 0;

            mov rdi, r9;       // valcounter2

            // for idx := 0 to width1 div 2 do
            mov rdx, r13;

            @@InnerLoop:
                movupd xmm1, [r8 + rdx];

                // load 2x2 block
                movupd xmm3, [rdi];
                movupd xmm5, xmm3;

                movupd xmm4, [rdi + rax];
                add rdi, r12;
                movupd xmm6, xmm4;

                // swap such that we can immediately multiply
                movhlps xmm4, xmm5;
                movlhps xmm3, xmm6;

                // it's a bit faster if we reuse a register - seems the cpu can
                // then parallelize the instructions better
                movupd xmm6, xmm1;

                // multiply 2x2 and add
                mulpd xmm3, xmm1;
                mulpd xmm4, xmm6;
                addpd xmm0, xmm3;
                addpd xmm7, xmm4;

                // end for idx := 0 to width1 div 2
            //dec edx;
            add rdx, 16;
            jnz @@InnerLoop;

            // final horizontal addition
            xorpd xmm2, xmm2;
            haddpd xmm0, xmm2;
            haddpd xmm7, xmm2;

            // compact result
            movlhps xmm0, xmm7;

            // store back result
            movupd [rcx], xmm0;

            // increment the pointers
            // inc(mt2), inc(dest);
            //add dword ptr [mt2], 8;
            add r9, 16;
            add rcx, 16;

        // end for x := 0 to width2 - 1
        dec rbx;
        jnz @@forxlabel;

        // handle the last column of mt2
        xorpd xmm0, xmm0;   // dest^ := 0
        mov rdi, r9;       // valcounter2

        mov rdx, r13;
        @@InnerLoop2:
            // load element from line
            movlpd xmm1, [r8 + rdx];

            // load, multiply and add
            mulsd xmm1, [rdi];
            add rdi, rax;       // next row

            addsd xmm0, xmm1;
        add rdx, 8;
        jnz @@InnerLoop2;

        movlpd [rcx], xmm0;

        // dec(mt2, Width2);
        // inc(PByte(mt1), LineWidth1);
        // inc(PByte(dest), destOffset);
        //mov ebx, bytesWidth2;
        //sub dword ptr [mt2], ebx;
        sub r9, r11;
        add r8, LineWidth1;
        add rcx, r15;

    // end for y := 0 to height1 - 1
    //dec eax;
    dec rsi;
    jnz @@forylabel;

    // epilog
    mov rbx, iRBX;
    mov rsi, iRSI;
    mov rdi, iRDI;
    mov r12, iR12;
    mov r13, iR13;
    mov r14, iR14;
    mov r15, iR15;

    movupd xmm4, dXMM4;
    movupd xmm5, dXMM5;
    movupd xmm6, dXMM6;
    movupd xmm7, dXMM7;
end;
{$IFDEF FPC}
end;
{$ENDIF}

// the width of mt1 is odd but the one of mt2 is even
procedure ASMMatrixMultAlignedOddW1EvenW2(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1, height1, width2, height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var dXMM4, dXMM5, dXMM6, dXMM7 : Array[0..1] of double;
    iRBX, iRSI, iRDI, iR12, iR13, iR14, iR15 : NativeInt;

{$IFDEF FPC}
begin
  {$ENDIF}

asm
    // note: RCX = dest, RDX = destLineWidth, R8 = mt1, R9 = mt2
    // prolog - simulate stack
    mov iRBX, rbx;
    mov iRSI, rsi;
    mov iRDI, rdi;
    mov iR12, r12;
    mov iR13, r13;
    mov iR14, r14;
    mov iR15, r15;

    movupd dXMM4, xmm4;
    movupd dXMM5, xmm5;
    movupd dXMM6, xmm6;
    movupd dXMM7, xmm7;
    {
   .pushnv rbx;
   .pushnv rsi;
   .pushnv rdi;
   .pushnv r12;
   .pushnv r13;
   .pushnv r14;
   .pushnv r15;

   .savenv xmm4;
   .savenv xmm5;
   .savenv xmm6;
   .savenv xmm7;
   }

    //destOffset := destLineWidth - Width2*sizeof(double);
    mov rbx, Width2;
    shl rbx, 3;
    mov r15, destLineWidth;
    sub r15, rbx;

    //bytesWidth2 := width2*sizeof(double);
    mov r11, rbx;

    //iters1 := width2 div 2;
    mov r14, width2;
    shr r14, 1;

    //iters2 := -(width1 - 1)*sizeof(double);
    mov rbx, width1;
    dec rbx;
    shl rbx, 3;
    imul rbx, -1;
    mov r13, rbx;

    //Linewidth2_2 := 2*LineWidth2;
    mov r12, LineWidth2;
    shl r12, 1;

    // initalize matrices
    sub r8, r13;    // r8 = mt1

    // for y := 0 to height1 - 1:
    mov rsi, Height1;

    mov rax, LineWidth2;
    @@forylabel:
        // for x := 0 to width2 - 1:
        mov rbx, r14;
        @@forxlabel:

            xorpd xmm0, xmm0;   // dest^ := 0
            xorpd xmm7, xmm7;   // (dest + 1)^ := 0;

            mov rdi, r9;       // valcounter2

            // for idx := 0 to width1 div 2 do
            mov rdx, r13;

            @@InnerLoop:
                movapd xmm1, [r8 + rdx];

                // load 2x2 block
                movapd xmm3, [rdi];
                movapd xmm5, xmm3;

                movapd xmm4, [rdi + rax];
                add rdi, r12;
                movapd xmm6, xmm4;

                // swap such that we can immediately multiply
                movhlps xmm4, xmm5;
                movlhps xmm3, xmm6;

                // it's a bit faster if we reuse a register - seems the cpu can
                // then parallelize the instructions better
                movapd xmm6, xmm1;

                // multiply 2x2 and add
                mulpd xmm3, xmm1;
                mulpd xmm4, xmm6;
                addpd xmm0, xmm3;
                addpd xmm7, xmm4;

                // end for idx := 0 to width1 div 2
            //dec edx;
            add rdx, 16;
            jnz @@InnerLoop;

            // special treatment of the last column of mt1:
            movddup xmm1, [r8];

            movapd xmm3, [rdi];
            mulpd xmm1, xmm3;

            // final horizontal addition
            xorpd xmm3, xmm3;
            movq xmm2, xmm1;
            addsd xmm0, xmm2;
            haddpd xmm0, xmm3;

            xorpd xmm2, xmm2;
            movhlps xmm2, xmm1;
            addsd xmm7, xmm2;
            haddpd xmm7, xmm3;

            // compact result
            movlhps xmm0, xmm7;
            // store back result
            movntdq [rcx], xmm0;

            // increment the pointers
            // inc(mt2), inc(dest);
            //add dword ptr [mt2], 8;
            add r9, 16;
            add rcx, 16;

        // end for x := 0 to width2 - 1
        dec rbx;
        jnz @@forxlabel;

        // dec(mt2, Width2);
        // inc(PByte(mt1), LineWidth1);
        // inc(PByte(dest), destOffset);
        //mov ebx, bytesWidth2;
        //sub dword ptr [mt2], ebx;
        sub r9, r11;
        add r8, LineWidth1;
        add rcx, r15;

    // end for y := 0 to height1 - 1
    //dec eax;
    dec rsi;
    jnz @@forylabel;

    // epilog
    mov rbx, iRBX;
    mov rsi, iRSI;
    mov rdi, iRDI;
    mov r12, iR12;
    mov r13, iR13;
    mov r14, iR14;
    mov r15, iR15;

    movupd xmm4, dXMM4;
    movupd xmm5, dXMM5;
    movupd xmm6, dXMM6;
    movupd xmm7, dXMM7;
end;
{$IFDEF FPC}
end;
{$ENDIF}

procedure ASMMatrixMultUnAlignedOddW1EvenW2(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1, height1, width2, height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var dXMM4, dXMM5, dXMM6, dXMM7 : Array[0..1] of double;
    iRBX, iRSI, iRDI, iR12, iR13, iR14, iR15 : NativeInt;

{$IFDEF FPC}
begin
  {$ENDIF}

asm
    // note: RCX = dest, RDX = destLineWidth, R8 = mt1, R9 = mt2
    // prolog - simulate stack
    mov iRBX, rbx;
    mov iRSI, rsi;
    mov iRDI, rdi;
    mov iR12, r12;
    mov iR13, r13;
    mov iR14, r14;
    mov iR15, r15;

    movupd dXMM4, xmm4;
    movupd dXMM5, xmm5;
    movupd dXMM6, xmm6;
    movupd dXMM7, xmm7;
    {
   .pushnv rbx;
   .pushnv rsi;
   .pushnv rdi;
   .pushnv r12;
   .pushnv r13;
   .pushnv r14;
   .pushnv r15;

   .savenv xmm4;
   .savenv xmm5;
   .savenv xmm6;
   .savenv xmm7;
   }

    //destOffset := destLineWidth - Width2*sizeof(double);
    mov rbx, Width2;
    shl rbx, 3;
    mov r15, destLineWidth;
    sub r15, rbx;

    //bytesWidth2 := width2*sizeof(double);
    mov r11, rbx;

    //iters1 := width2 div 2;
    mov r14, width2;
    shr r14, 1;

    //iters2 := -(width1 - 1)*sizeof(double);
    mov rbx, width1;
    dec rbx;
    shl rbx, 3;
    imul rbx, -1;
    mov r13, rbx;

    //Linewidth2_2 := 2*LineWidth2;
    mov r12, LineWidth2;
    shl r12, 1;

    // initalize matrices
    sub r8, r13;    // r8 = mt1

    // for y := 0 to height1 - 1:
    mov rsi, Height1;

    mov rax, LineWidth2;
    @@forylabel:
        // for x := 0 to width2 - 1:
        mov rbx, r14;
        @@forxlabel:

            xorpd xmm0, xmm0;   // dest^ := 0
            xorpd xmm7, xmm7;   // (dest + 1)^ := 0;

            mov rdi, r9;       // valcounter2

            // for idx := 0 to width1 div 2 do
            mov rdx, r13;

            @@InnerLoop:
                movupd xmm1, [r8 + rdx];

                // load 2x2 block
                movupd xmm3, [rdi];
                movupd xmm5, xmm3;

                movupd xmm4, [rdi + rax];
                add rdi, r12;
                movupd xmm6, xmm4;

                // swap such that we can immediately multiply
                movhlps xmm4, xmm5;
                movlhps xmm3, xmm6;

                // it's a bit faster if we reuse a register - seems the cpu can
                // then parallelize the instructions better
                movupd xmm6, xmm1;

                // multiply 2x2 and add
                mulpd xmm3, xmm1;
                mulpd xmm4, xmm6;
                addpd xmm0, xmm3;
                addpd xmm7, xmm4;

                // end for idx := 0 to width1 div 2
            //dec edx;
            add rdx, 16;
            jnz @@InnerLoop;

            // special treatment of the last column of mt1:
            movddup xmm1, [r8];

            movupd xmm3, [rdi];
            mulpd xmm1, xmm3;

            // final horizontal addition
            xorpd xmm3, xmm3;
            movq xmm2, xmm1;
            addsd xmm0, xmm2;
            haddpd xmm0, xmm3;

            xorpd xmm2, xmm2;
            movhlps xmm2, xmm1;
            addsd xmm7, xmm2;
            haddpd xmm7, xmm3;

            // compact result
            movlhps xmm0, xmm7;

            // store back result
            movupd [rcx], xmm0;

            // increment the pointers
            // inc(mt2), inc(dest);
            //add dword ptr [mt2], 8;
            add r9, 16;
            add rcx, 16;

        // end for x := 0 to width2 - 1
        dec rbx;
        jnz @@forxlabel;

        // dec(mt2, Width2);
        // inc(PByte(mt1), LineWidth1);
        // inc(PByte(dest), destOffset);
        //mov ebx, bytesWidth2;
        //sub dword ptr [mt2], ebx;
        sub r9, r11;
        add r8, LineWidth1;
        add rcx, r15;

    // end for y := 0 to height1 - 1
    //dec eax;
    dec rsi;
    jnz @@forylabel;

    // epilog
    mov rbx, iRBX;
    mov rsi, iRSI;
    mov rdi, iRDI;
    mov r12, iR12;
    mov r13, iR13;
    mov r14, iR14;
    mov r15, iR15;

    movupd xmm4, dXMM4;
    movupd xmm5, dXMM5;
    movupd xmm6, dXMM6;
    movupd xmm7, dXMM7;
end;
{$IFDEF FPC}
end;
{$ENDIF}

// both matrices have an odd width
procedure ASMMatrixMultAlignedOddW1OddW2(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1, height1, width2, height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var dXMM4, dXMM5, dXMM6, dXMM7, dXMM8 : Array[0..1] of double;
    iRBX, iRSI, iRDI, iR12, iR13, iR14, iR15 : NativeInt;

{$IFDEF FPC}
begin
  {$ENDIF}

asm
    // note: RCX = dest, RDX = destLineWidth, R8 = mt1, R9 = mt2
    // prolog - simulate stack
    mov iRBX, rbx;
    mov iRSI, rsi;
    mov iRDI, rdi;
    mov iR12, r12;
    mov iR13, r13;
    mov iR14, r14;
    mov iR15, r15;

    movupd dXMM4, xmm4;
    movupd dXMM5, xmm5;
    movupd dXMM6, xmm6;
    movupd dXMM7, xmm7;
    movupd dXMM8, xmm8;
    {
   .pushnv rbx;
   .pushnv rbx;
   .pushnv rsi;
   .pushnv rdi;
   .pushnv r12;
   .pushnv r13;
   .pushnv r14;
   .pushnv r15;

   .savenv xmm4;
   .savenv xmm5;
   .savenv xmm6;
   .savenv xmm7;
   .savenv xmm8;
   }

    //destOffset := destLineWidth - (Width2 - 1)*sizeof(double);
    mov rbx, Width2;
    dec rbx;
    shl rbx, 3;
    mov r15, destLineWidth;
    sub r15, rbx;

    //bytesWidth2 := (width2 - 1)*sizeof(double);
    mov r11, rbx;

    //iters1 := width2 div 2;
    mov r14, width2;
    shr r14, 1;

    //iters2 := -(width1 - 1)*sizeof(double);
    mov rbx, width1;
    dec rbx;
    shl rbx, 3;
    imul rbx, -1;
    mov r13, rbx;

    //Linewidth2_2 := 2*LineWidth2;
    mov r12, LineWidth2;
    shl r12, 1;

    xorpd xmm8, xmm8;

    // initalize matrices
    sub r8, r13;    // r8 = mt1

    // for y := 0 to height1 - 1:
    mov rsi, Height1;

    mov rax, LineWidth2;
    @@forylabel:
        // for x := 0 to width2 - 1:
        mov rbx, r14;
        @@forxlabel:

            xorpd xmm0, xmm0;   // dest^ := 0
            xorpd xmm7, xmm7;   // (dest + 1)^ := 0;

            mov rdi, r9;       // valcounter2

            // for idx := 0 to width1 div 2 do
            mov rdx, r13;

            @@InnerLoop:
                movupd xmm1, [r8 + rdx];

                // load 2x2 block
                movupd xmm3, [rdi];
                movupd xmm5, xmm3;

                movupd xmm4, [rdi + rax];
                add rdi, r12;
                movupd xmm6, xmm4;

                // swap such that we can immediately multiply
                movhlps xmm4, xmm5;
                movlhps xmm3, xmm6;

                // it's a bit faster if we reuse a register - seems the cpu can
                // then parallelize the instructions better
                movupd xmm6, xmm1;

                // multiply 2x2 and add
                mulpd xmm3, xmm1;
                mulpd xmm4, xmm6;
                addpd xmm0, xmm3;
                addpd xmm7, xmm4;

                // end for idx := 0 to width1 div 2
            //dec edx;
            add rdx, 16;
            jnz @@InnerLoop;

            // special treatment of the last column of mt1:
            movddup xmm1, [r8];

            movapd xmm3, [rdi];
            mulpd xmm1, xmm3;

            // final horizontal addition
            movq xmm2, xmm1;
            addsd xmm0, xmm2;
            haddpd xmm0, xmm8;

            movhlps xmm2, xmm1;
            addsd xmm7, xmm2;
            haddpd xmm7, xmm8;

            // compact result
            movlhps xmm0, xmm7;
            // store back result
            movntdq [rcx], xmm0;

            // increment the pointers
            // inc(mt2), inc(dest);
            //add dword ptr [mt2], 8;
            add r9, 16;
            add rcx, 16;

        // end for x := 0 to width2 - 1
        dec rbx;
        jnz @@forxlabel;

        // handle the last column of mt2
        xorpd xmm0, xmm0;   // dest^ := 0
        mov rdi, r9;       // valcounter2

        sub r13, 8;
        add r8, 8;
        mov rdx, r13;
        @@InnerLoop2:
            // load element from line
            movlpd xmm1, [r8 + rdx];

            // load, multiply and add
            mulsd xmm1, [rdi];
            add rdi, rax;       // next row

            addsd xmm0, xmm1;
        add rdx, 8;
        jnz @@InnerLoop2;
        add r13, 8;
        sub r8, 8;

        movlpd [rcx], xmm0;

        // dec(mt2, Width2);
        // inc(PByte(mt1), LineWidth1);
        // inc(PByte(dest), destOffset);
        //mov ebx, bytesWidth2;
        //sub dword ptr [mt2], ebx;
        sub r9, r11;
        add r8, LineWidth1;
        add rcx, r15;

    // end for y := 0 to height1 - 1
    //dec eax;
    dec rsi;
    jnz @@forylabel;

    // epilog
    mov rbx, iRBX;
    mov rsi, iRSI;
    mov rdi, iRDI;
    mov r12, iR12;
    mov r13, iR13;
    mov r14, iR14;
    mov r15, iR15;

    movupd xmm4, dXMM4;
    movupd xmm5, dXMM5;
    movupd xmm6, dXMM6;
    movupd xmm7, dXMM7;
    movupd xmm8, dXMM8;
end;
{$IFDEF FPC}
end;
{$ENDIF}

procedure ASMMatrixMultUnAlignedOddW1OddW2(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1, height1, width2, height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var dXMM4, dXMM5, dXMM6, dXMM7, dXMM8 : Array[0..1] of double;
    iRBX, iRSI, iRDI, iR12, iR13, iR14, iR15 : NativeInt;

{$IFDEF FPC}
begin
  {$ENDIF}

asm
    // note: RCX = dest, RDX = destLineWidth, R8 = mt1, R9 = mt2
    // prolog - simulate stack
    mov iRBX, rbx;
    mov iRSI, rsi;
    mov iRDI, rdi;
    mov iR12, r12;
    mov iR13, r13;
    mov iR14, r14;
    mov iR15, r15;

    movupd dXMM4, xmm4;
    movupd dXMM5, xmm5;
    movupd dXMM6, xmm6;
    movupd dXMM7, xmm7;
    movupd dXMM8, xmm8;
    {
   .pushnv rbx;
   .pushnv rbx;
   .pushnv rsi;
   .pushnv rdi;
   .pushnv r12;
   .pushnv r13;
   .pushnv r14;
   .pushnv r15;

   .savenv xmm4;
   .savenv xmm5;
   .savenv xmm6;
   .savenv xmm7;
   .savenv xmm8;
   }

    //destOffset := destLineWidth - (Width2 - 1)*sizeof(double);
    mov rbx, Width2;
    dec rbx;
    shl rbx, 3;
    mov r15, destLineWidth;
    sub r15, rbx;

    //bytesWidth2 := (width2 - 1)*sizeof(double);
    mov r11, rbx;

    //iters1 := width2 div 2;
    mov r14, width2;
    shr r14, 1;

    //iters2 := -(width1 - 1)*sizeof(double);
    mov rbx, width1;
    dec rbx;
    shl rbx, 3;
    imul rbx, -1;
    mov r13, rbx;

    //Linewidth2_2 := 2*LineWidth2;
    mov r12, LineWidth2;
    shl r12, 1;

    xorpd xmm8, xmm8;

    // initalize matrices
    sub r8, r13;    // r8 = mt1

    // for y := 0 to height1 - 1:
    mov rsi, Height1;

    mov rax, LineWidth2;
    @@forylabel:
        // for x := 0 to width2 - 1:
        mov rbx, r14;
        @@forxlabel:

            xorpd xmm0, xmm0;   // dest^ := 0
            xorpd xmm7, xmm7;   // (dest + 1)^ := 0;

            mov rdi, r9;       // valcounter2

            // for idx := 0 to width1 div 2 do
            mov rdx, r13;

            @@InnerLoop:
                movupd xmm1, [r8 + rdx];

                // load 2x2 block
                movupd xmm3, [rdi];
                movupd xmm5, xmm3;

                movupd xmm4, [rdi + rax];
                add rdi, r12;
                movupd xmm6, xmm4;

                // swap such that we can immediately multiply
                movhlps xmm4, xmm5;
                movlhps xmm3, xmm6;

                // it's a bit faster if we reuse a register - seems the cpu can
                // then parallelize the instructions better
                movupd xmm6, xmm1;

                // multiply 2x2 and add
                mulpd xmm3, xmm1;
                mulpd xmm4, xmm6;
                addpd xmm0, xmm3;
                addpd xmm7, xmm4;

                // end for idx := 0 to width1 div 2
            //dec edx;
            add rdx, 16;
            jnz @@InnerLoop;

            // special treatment of the last column of mt1:
            movddup xmm1, [r8];

            movupd xmm3, [rdi];
            mulpd xmm1, xmm3;

            // final horizontal addition
            movq xmm2, xmm1;
            addsd xmm0, xmm2;
            haddpd xmm0, xmm8;

            movhlps xmm2, xmm1;
            addsd xmm7, xmm2;
            haddpd xmm7, xmm8;

            // compact result
            movlhps xmm0, xmm7;
            // store back result
            movupd [rcx], xmm0;

            // increment the pointers
            // inc(mt2), inc(dest);
            //add dword ptr [mt2], 8;
            add r9, 16;
            add rcx, 16;

        // end for x := 0 to width2 - 1
        dec rbx;
        jnz @@forxlabel;

        // handle the last column of mt2
        xorpd xmm0, xmm0;   // dest^ := 0
        mov rdi, r9;       // valcounter2

        sub r13, 8;
        add r8, 8;
        mov rdx, r13;
        @@InnerLoop2:
            // load element from line
            movlpd xmm1, [r8 + rdx];

            // load, multiply and add
            movlpd xmm2, [rdi];
            mulsd xmm1, [rdi];
            add rdi, rax;       // next row

            addsd xmm0, xmm1;
        add rdx, 8;
        jnz @@InnerLoop2;
        add r13, 8;
        sub r8, 8;

        movlpd [rcx], xmm0;

        // dec(mt2, Width2);
        // inc(PByte(mt1), LineWidth1);
        // inc(PByte(dest), destOffset);
        //mov ebx, bytesWidth2;
        //sub dword ptr [mt2], ebx;
        sub r9, r11;
        add r8, LineWidth1;
        add rcx, r15;

    // end for y := 0 to height1 - 1
    //dec eax;
    dec rsi;
    jnz @@forylabel;

    mov rbx, iRBX;
    mov rsi, iRSI;
    mov rdi, iRDI;
    mov r12, iR12;
    mov r13, iR13;
    mov r14, iR14;
    mov r15, iR15;

    movupd xmm4, dXMM4;
    movupd xmm5, dXMM5;
    movupd xmm6, dXMM6;
    movupd xmm7, dXMM7;
    movupd xmm8, dXMM8;
end;
{$IFDEF FPC}
end;
{$ENDIF}

{$ENDIF}

end.
