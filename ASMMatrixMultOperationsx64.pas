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

{$IFDEF FPC}
{$MODE Delphi}
{$ASMMODE intel}
{$ENDIF}

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

// some special types of multiplications used e.g. in QR Decomposition
// dest = mt1'*mt2; where mt2 is a lower triangular matrix and the operation is transposition
// the function assumes a unit diagonal (does not touch the real middle elements)
// width and height values are assumed to be the "original" (non transposed) ones
procedure ASMMtxMultTria2T1(dest : PDouble; LineWidthDest : TASMNativeInt; mt1 : PDouble; LineWidth1 : TASMNativeInt; mt2 : PDouble; LineWidth2 : TASMNativeInt;
  width1, height1, width2, height2 : TASMNativeInt);
// mt1 = mt1*mt2'; where mt2 is an upper triangular matrix
procedure ASMMtxMultTria2T1StoreT1(mt1 : PDouble; LineWidth1 : TASMNativeInt; mt2 : PDouble; LineWidth2 : TASMNativeInt;
  width1, height1, width2, height2 : TASMNativeInt);

// W = C1*V1*T -> V1 is an upper triangular matrix with assumed unit diagonal entries. Operation on V1 transposition
procedure ASMMtxMultTria2TUpperUnit(dest : PDouble; LineWidthDest : TASMNativeInt; mt1 : PDouble; LineWidth1 : TASMNativeInt; mt2 : PDouble; LineWidth2 : TASMNativeInt;
  width1, height1, width2, height2 : TASMNativeInt);

// mt1 = mt1*mt2; where mt2 is an upper triangular matrix
procedure ASMMtxMultTria2Store1(mt1 : PDouble; LineWidth1 : TASMNativeInt; mt2 : PDouble; LineWidth2 : TASMNativeInt;
  width1, height1, width2, height2 : TASMNativeInt);

// calculates mt1 = mt1*mt2', mt2 = lower triangular matrix. diagonal elements are assumed to be 1!
procedure ASMMtxMultLowTria2T2Store1(mt1 : PDouble; LineWidth1 : TASMNativeInt; mt2 : PDouble; LineWidth2 : TASMNativeInt;
  width1, height1, width2, height2 : TASMNativeInt);

// mt1 = mt1*mt2; where mt2 is an upper triangular matrix - diagonal elements are unit
procedure ASMMtxMultTria2Store1Unit(mt1 : PDouble; LineWidth1 : TASMNativeInt; mt2 : PDouble; LineWidth2 : TASMNativeInt;
  width1, height1, width2, height2 : TASMNativeInt);


{$ENDIF}

implementation

{$IFDEF x64}
procedure ASMMatrixMultAlignedEvenW1EvenW2(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1, height1, width2, height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var dXMM4, dXMM5, dXMM6, dXMM7 : Array[0..1] of double;
    iRBX, iRSI, iRDI, iR12, iR13, iR14, iR15 : TASMNativeInt;
asm
   {$IFDEF LINUX}
   // Linux uses a diffrent ABI -> copy over the registers so they meet with winABI
   // (note that the 5th and 6th parameter are are on the stack)
   // The parameters are passed in the following order:
   // RDI, RSI, RDX, RCX -> mov to RCX, RDX, R8, R9
   mov r8, rdx;
   mov r9, rcx;
   mov rcx, rdi;
   mov rdx, rsi;
   {$ENDIF}

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
            haddpd xmm0, xmm0;
            haddpd xmm7, xmm7;

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

procedure ASMMatrixMultUnAlignedEvenW1EvenW2(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1, height1, width2, height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var dXMM4, dXMM5, dXMM6, dXMM7 : Array[0..1] of double;
    iRBX, iRSI, iRDI, iR12, iR13, iR14, iR15 : TASMNativeInt;
asm
   {$IFDEF LINUX}
   // Linux uses a diffrent ABI -> copy over the registers so they meet with winABI
   // (note that the 5th and 6th parameter are are on the stack)
   // The parameters are passed in the following order:
   // RDI, RSI, RDX, RCX -> mov to RCX, RDX, R8, R9
   mov r8, rdx;
   mov r9, rcx;
   mov rcx, rdi;
   mov rdx, rsi;
   {$ENDIF}

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
            haddpd xmm0, xmm0;
            haddpd xmm7, xmm7;

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

// the width of the second matrix is uneven -> the last column of mt2 must be handled differently
procedure ASMMatrixMultAlignedEvenW1OddW2(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1, height1, width2, height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var dXMM4, dXMM5, dXMM6, dXMM7 : Array[0..1] of double;
    iRBX, iRSI, iRDI, iR12, iR13, iR14, iR15 : TASMNativeInt;
asm
   {$IFDEF LINUX}
   // Linux uses a diffrent ABI -> copy over the registers so they meet with winABI
   // (note that the 5th and 6th parameter are are on the stack)
   // The parameters are passed in the following order:
   // RDI, RSI, RDX, RCX -> mov to RCX, RDX, R8, R9
   mov r8, rdx;
   mov r9, rcx;
   mov rcx, rdi;
   mov rdx, rsi;
   {$ENDIF}

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
            haddpd xmm0, xmm0;
            haddpd xmm7, xmm7;

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
            movsd xmm1, [r8 + rdx];

            // load, multiply and add
            mulsd xmm1, [rdi];
            add rdi, rax;       // next row

            addsd xmm0, xmm1;
        add rdx, 8;
        jnz @@InnerLoop2;

        movsd [rcx], xmm0;

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

procedure ASMMatrixMultUnAlignedEvenW1OddW2(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1, height1, width2, height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var dXMM4, dXMM5, dXMM6, dXMM7 : Array[0..1] of double;
    iRBX, iRSI, iRDI, iR12, iR13, iR14, iR15 : TASMNativeInt;
asm
   {$IFDEF LINUX}
   // Linux uses a diffrent ABI -> copy over the registers so they meet with winABI
   // (note that the 5th and 6th parameter are are on the stack)
   // The parameters are passed in the following order:
   // RDI, RSI, RDX, RCX -> mov to RCX, RDX, R8, R9
   mov r8, rdx;
   mov r9, rcx;
   mov rcx, rdi;
   mov rdx, rsi;
   {$ENDIF}

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
                movapd xmm5, xmm3;

                movupd xmm4, [rdi + rax];
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
            haddpd xmm0, xmm0;
            haddpd xmm7, xmm7;

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
            movsd xmm1, [r8 + rdx];

            // load, multiply and add
            mulsd xmm1, [rdi];
            add rdi, rax;       // next row

            addsd xmm0, xmm1;
        add rdx, 8;
        jnz @@InnerLoop2;

        movsd [rcx], xmm0;

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

// the width of mt1 is odd but the one of mt2 is even
procedure ASMMatrixMultAlignedOddW1EvenW2(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1, height1, width2, height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var dXMM4, dXMM5, dXMM6, dXMM7 : Array[0..1] of double;
    iRBX, iRSI, iRDI, iR12, iR13, iR14, iR15 : TASMNativeInt;
asm
   {$IFDEF LINUX}
   // Linux uses a diffrent ABI -> copy over the registers so they meet with winABI
   // (note that the 5th and 6th parameter are are on the stack)
   // The parameters are passed in the following order:
   // RDI, RSI, RDX, RCX -> mov to RCX, RDX, R8, R9
   mov r8, rdx;
   mov r9, rcx;
   mov rcx, rdi;
   mov rdx, rsi;
   {$ENDIF}

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
            movq xmm2, xmm1;
            addsd xmm0, xmm2;
            haddpd xmm0, xmm0;

            movhlps xmm2, xmm1;
            addsd xmm7, xmm2;
            haddpd xmm7, xmm7;

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

procedure ASMMatrixMultUnAlignedOddW1EvenW2(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1, height1, width2, height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var dXMM4, dXMM5, dXMM6, dXMM7 : Array[0..1] of double;
    iRBX, iRSI, iRDI, iR12, iR13, iR14, iR15 : TASMNativeInt;
asm
   {$IFDEF LINUX}
   // Linux uses a diffrent ABI -> copy over the registers so they meet with winABI
   // (note that the 5th and 6th parameter are are on the stack)
   // The parameters are passed in the following order:
   // RDI, RSI, RDX, RCX -> mov to RCX, RDX, R8, R9
   mov r8, rdx;
   mov r9, rcx;
   mov rcx, rdi;
   mov rdx, rsi;
   {$ENDIF}

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
                movapd xmm5, xmm3;

                movupd xmm4, [rdi + rax];
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

            movupd xmm3, [rdi];
            mulpd xmm1, xmm3;

            // final horizontal addition
            movq xmm2, xmm1;
            addsd xmm0, xmm2;
            haddpd xmm0, xmm0;

            movhlps xmm2, xmm1;
            addsd xmm7, xmm2;
            haddpd xmm7, xmm7;

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

// both matrices have an odd width
procedure ASMMatrixMultAlignedOddW1OddW2(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1, height1, width2, height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var dXMM4, dXMM5, dXMM6, dXMM7 : Array[0..1] of double;
    iRBX, iRSI, iRDI, iR12, iR13, iR14, iR15 : TASMNativeInt;
asm
   {$IFDEF LINUX}
   // Linux uses a diffrent ABI -> copy over the registers so they meet with winABI
   // (note that the 5th and 6th parameter are are on the stack)
   // The parameters are passed in the following order:
   // RDI, RSI, RDX, RCX -> mov to RCX, RDX, R8, R9
   mov r8, rdx;
   mov r9, rcx;
   mov rcx, rdi;
   mov rdx, rsi;
   {$ENDIF}

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
                movapd xmm5, xmm3;

                movupd xmm4, [rdi + rax];
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
            movq xmm2, xmm1;
            addsd xmm0, xmm2;
            haddpd xmm0, xmm0;

            movhlps xmm2, xmm1;
            addsd xmm7, xmm2;
            haddpd xmm7, xmm7;

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
            movsd xmm1, [r8 + rdx];

            // load, multiply and add
            mulsd xmm1, [rdi];
            add rdi, rax;       // next row

            addsd xmm0, xmm1;
        add rdx, 8;
        jnz @@InnerLoop2;
        add r13, 8;
        sub r8, 8;

        movsd [rcx], xmm0;

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

procedure ASMMatrixMultUnAlignedOddW1OddW2(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1, height1, width2, height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var dXMM4, dXMM5, dXMM6, dXMM7 : Array[0..1] of double;
    iRBX, iRSI, iRDI, iR12, iR13, iR14, iR15 : TASMNativeInt;
asm
   {$IFDEF LINUX}
   // Linux uses a diffrent ABI -> copy over the registers so they meet with winABI
   // (note that the 5th and 6th parameter are are on the stack)
   // The parameters are passed in the following order:
   // RDI, RSI, RDX, RCX -> mov to RCX, RDX, R8, R9
   mov r8, rdx;
   mov r9, rcx;
   mov rcx, rdi;
   mov rdx, rsi;
   {$ENDIF}

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
                movapd xmm5, xmm3;

                movupd xmm4, [rdi + rax];
                add rdi, r12;
                movapd xmm6, xmm4;

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
            haddpd xmm0, xmm0;

            movhlps xmm2, xmm1;
            addsd xmm7, xmm2;
            haddpd xmm7, xmm7;

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
            movsd xmm1, [r8 + rdx];

            // load, multiply and add
            movsd xmm2, [rdi];
            mulsd xmm1, [rdi];
            add rdi, rax;       // next row

            addsd xmm0, xmm1;
        add rdx, 8;
        jnz @@InnerLoop2;
        add r13, 8;
        sub r8, 8;

        movsd [rcx], xmm0;

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
end;


// ###########################################
// #### Special multiplication routines (for now only used in QR Decomposition)
// ###########################################

// note the result is stored in mt1 again!
// mt1 = mt1*mt2; where mt2 is an upper triangular matrix
procedure ASMMtxMultTria2Store1(mt1 : PDouble; LineWidth1 : TASMNativeInt; mt2 : PDouble; LineWidth2 : TASMNativeInt;
  width1, height1, width2, height2 : TASMNativeInt);
var iRBX, iRDI, iRSI, iR12, iR13 : TASMNativeInt;
// prolog init stack
// rcx = mt1, rdx = LineWidth1, r8 = mt2, r9 = LineWidth2
asm
   {$IFDEF LINUX}
   // Linux uses a diffrent ABI -> copy over the registers so they meet with winABI
   // (note that the 5th and 6th parameter are are on the stack)
   // The parameters are passed in the following order:
   // RDI, RSI, RDX, RCX -> mov to RCX, RDX, R8, R9
   mov r8, rdx;
   mov r9, rcx;
   mov rcx, rdi;
   mov rdx, rsi;
   {$ENDIF}

   // prolog: stack
   mov iRBX, rbx;
   mov iRDI, rdi;
   mov iRSI, rsi;
   mov iR12, r12;
   mov iR13, r13;

   // r13 = height1
   mov r13, height1;

   // wm1: r11
   mov r11, width2;
   dec r11;

   // iter: r10
   mov r10, width1;
   shl r10, 3;
   imul r10, -1;
   // init

   // inc(mt2, width2 - 1);
   mov rax, r11;
   shl rax, 3; //sizeof(double)
   add r8, rax;


   // for x := 0 to width2 - 1 do
   mov r12, width2;
   @@forxloop:
      mov rdi, r13;  // height1

      mov rax, rcx;
      sub rax, r10;

      // for y := 0 to height1 - 1
      @@foryloop:
         // tmp := 0;
         xorpd xmm0, xmm0;

         // r8, mt2
         mov rbx, r8;

         // for idx := 0 to width1 - x - 1
         mov rsi, r10;

         // check if we have enough iterations:
         test rsi, rsi;
         jz @@foridxloopend;

         @@foridxloop:
            movsd xmm1, [rbx];
            movsd xmm2, [rax + rsi]

            add rbx, r9;  // + linewidth2

            mulsd xmm1, xmm2;
            addsd xmm0, xmm1;

         add rsi, 8;
         jnz @@foridxloop;

         @@foridxloopend:

         // PConstDoubleArr(pmt1)^[width2 - 1 - x] := tmp;
         mov rbx, rax;
         add rbx, r10;
         mov rsi, r12; // r12 = width2
         dec rsi;
         movsd [rbx + 8*rsi], xmm0;

         // inc(PByte(pmT1), LineWidth1);
         add rax, rdx;

      dec rdi;
      jnz @@foryloop;

      // reduce for idx loop
      add r10, 8;
      // dec(mt2);
      sub r8, 8;

   // dec width2
   dec r12;
   jnz @@forxloop;

   // cleanup stack
   mov rbx, iRBX;
   mov rdi, iRDI;
   mov rsi, iRSI;
   mov r12, iR12;
   mov r13, iR13;
end;

procedure ASMMtxMultTria2T1StoreT1(mt1 : PDouble; LineWidth1 : TASMNativeInt; mt2 : PDouble; LineWidth2 : TASMNativeInt;
  width1, height1, width2, height2 : TASMNativeInt);
var iRBX, iRDI, iRSI : TASMNativeInt;
// rcx = mt1, rdx = LineWidth1, r8 = mt2, r9 = LineWidth2
asm
   {$IFDEF LINUX}
   // Linux uses a diffrent ABI -> copy over the registers so they meet with winABI
   // (note that the 5th and 6th parameter are are on the stack)
   // The parameters are passed in the following order:
   // RDI, RSI, RDX, RCX -> mov to RCX, RDX, R8, R9
   mov r8, rdx;
   mov r9, rcx;
   mov rcx, rdi;
   mov rdx, rsi;
   {$ENDIF}

   // prolog: stack
   mov iRBX, rbx;
   mov iRDI, rdi;
   mov iRSI, rsi;

   //iter := -width1*sizeof(double);
   mov r10, width1;
   imul r10, -8;

   // testExitLoopVal := height2*sizeof(double) + iter;
   mov r11, height2;
   shl r11, 3; //*8
   add r11, r10;

   // rcx := mt1
   sub rcx, r10;  // mt1 - iter

   // for y loop
   @@foryloop:
      mov rbx, r8;
      sub rbx, r10;
      mov rsi, r10;

      @@forxloop:
         xorpd xmm0, xmm0; // temp := 0

         mov rax, rsi;  // loop counter x;

         // test if height2 > width1 and loop counter > width1
         mov rdi, rax;
         test rdi, rdi;
         jge @@foriloopend;

         // in case x is odd -> handle the first element separately
         and rdi, $E;
         jz @@foriloopinit;

         // single element handling
         movsd xmm1, [rcx + rax];
         movsd xmm2, [rbx + rax];
         mulsd xmm1, xmm2;
         addsd xmm0, xmm1;
         add rax, 8;

         @@foriloopinit:

         // in case the last single x element was handled we do not need further looping
         test rax, rax;
         jz @@foriloopend;

         // for i := x to width1 - 1
         @@foriloop:
            // two elements at a time:
            movupd xmm1, [rcx + rax];
            movupd xmm2, [rbx + rax];
            mulpd xmm1, xmm2;
            addpd xmm0, xmm1;

         add rax, 16;
         jnz @@foriloop;

         @@foriloopend:

         // final result
         haddpd xmm0, xmm0;
         movsd [rcx + rsi], xmm0;

         add rbx, r9;
         add rsi, 8;

      mov rax, r11;
      cmp rsi, rax;
      jne @@forxloop;

      add rcx, rdx;
   dec height1;
   jnz @@foryloop;

   // epilog
   mov rbx, iRBX;
   mov rdi, iRDI;
   mov rsi, iRSI;

end;


procedure ASMMtxMultTria2TUpperUnit(dest : PDouble; LineWidthDest : TASMNativeInt; mt1 : PDouble; LineWidth1 : TASMNativeInt; mt2 : PDouble; LineWidth2 : TASMNativeInt;
  width1, height1, width2, height2 : TASMNativeInt);
var iRBX, iRDI, iRSI, iR12, iR13 : TASMNativeInt;
// rcx = dest, rdx = LineWidthDest, r8 = mt1, r9 = LineWidth1
asm
   {$IFDEF LINUX}
   // Linux uses a diffrent ABI -> copy over the registers so they meet with winABI
   // (note that the 5th and 6th parameter are are on the stack)
   // The parameters are passed in the following order:
   // RDI, RSI, RDX, RCX -> mov to RCX, RDX, R8, R9
   mov r8, rdx;
   mov r9, rcx;
   mov rcx, rdi;
   mov rdx, rsi;
   {$ENDIF}

   // prolog: stack
   mov iRBX, rbx;
   mov iRDI, rdi;
   mov iRSI, rsi;
   mov iR12, r12;
   mov iR13, r13;

   // r12: mt2
   mov r12, mt2;
   // r13: LineWidth2
   mov r13, LineWidth2;

   //iter := -width1*sizeof(double);
   mov r10, width1;
   imul r10, -8;

   // testExitLoopVal := height2*sizeof(double) + iter;
   mov r11, height2;
   shl r11, 3; //*8
   add r11, r10;

   // r8 := mt1
   sub r8, r10;  // mt1 - iter
   sub rcx, r10;

   // for y loop
   @@foryloop:
      mov rbx, r12;
      sub rbx, r10;
      mov rsi, r10;

      @@forxloop:
         xorpd xmm0, xmm0; // temp := 0

         mov rax, rsi;  // loop counter x;

         // test if height2 > width1 and loop counter > width1
         mov rdi, rax;
         test rdi, rdi;
         jge @@foriloopend;

         // in case x is odd -> handle the first element separately
         and rdi, $E;
         jz @@foriloopinit;

         // single element handling -> mt1 first element is assumed unit!
         movsd xmm0, [r8 + rax];
         add rax, 8;

         jmp @@AfterLoopInit;

         @@foriloopinit:

         test rax, rax;
         jz @@foriloopend;

         // two elements init at a time:
         movsd xmm0, [r8 + rax];
         movsd xmm1, [r8 + rax + 8];
         movsd xmm2, [rbx + rax + 8];
         mulsd xmm1, xmm2;
         addsd xmm0, xmm1;

         add rax, 16;

         @@AfterLoopInit:

         // in case the last single x element was handled we do not need further looping
         test rax, rax;
         jz @@foriloopend;

         // for i := x to width1 - 1
         @@foriloop:
            // two elements at a time:
            movupd xmm1, [r8 + rax];
            movupd xmm2, [rbx + rax];
            mulpd xmm1, xmm2;
            addpd xmm0, xmm1;

         add rax, 16;
         jnz @@foriloop;

         @@foriloopend:

         // final result
         haddpd xmm0, xmm0;
         movsd [rcx + rsi], xmm0;

         add rbx, r13;
         add rsi, 8;

      mov rax, r11;
      cmp rsi, rax;
      jne @@forxloop;

      add r8, r9;
      add rcx, rdx;
   dec height1;
   jnz @@foryloop;

   // epilog
   mov rbx, iRBX;
   mov rdi, iRDI;
   mov rsi, iRSI;
   mov r12, iR12;
   mov r13, iR13;
end;


// note the result is stored in mt2 again!
// dest = mt1'*mt2; where mt2 is a lower triangular matrix and the operation is transposition
// the function assumes a unit diagonal (does not touch the real middle elements)
// width and height values are assumed to be the "original" (non transposed) ones
procedure ASMMtxMultTria2T1(dest : PDouble; LineWidthDest : TASMNativeInt; mt1 : PDouble; LineWidth1 : TASMNativeInt;
  mt2 : PDouble; LineWidth2 : TASMNativeInt;
  width1, height1, width2, height2 : TASMNativeInt);
var pMt2 : PDouble;
    iRBX, iRSI, iRDI, iR12, iR13, iR14, iR15 : TASMNativeInt;
 // note: RCX = dest, RDX = destLineWidth, R8 = mt1, R9 = LineWidth1
 // prolog - simulate stack
asm
   {$IFDEF LINUX}
   // Linux uses a diffrent ABI -> copy over the registers so they meet with winABI
   // (note that the 5th and 6th parameter are are on the stack)
   // The parameters are passed in the following order:
   // RDI, RSI, RDX, RCX -> mov to RCX, RDX, R8, R9
   mov r8, rdx;
   mov r9, rcx;
   mov rcx, rdi;
   mov rdx, rsi;
   {$ENDIF}

   mov iR12, r12;
   mov iR13, r13;
   mov iR14, r14;
   mov iR15, r15;
   mov iRSI, rsi;
   mov iRDI, rdi;
   mov iRBX, rbx;

   mov r15, LineWidth2;   // Linewidth2 is on the stack -> use it in local register

   // width2D2 := width2 div 2;
   mov r14, width2;
   shr r14, 1;

   // for x := 0 to width1 - 1 do
   mov rax, width1;

   @@forxloop:

     // pMT2 := mt2;
     // pDest := dest;

     mov rbx, mt2;
     mov pMT2, rbx;

     mov r13, rcx;   // r13 is pDest


     // for y := 0 to width2D2 - 1 do
     mov r12, r14;
     test r12, r12;
     jz @@foryloopend;

     xor r12, r12;
     @@foryloop:

          // valCounter1 := PConstDoubleArr(mt1);
          // inc(PByte(valCounter1), 2*y*LineWidth1);
          mov rsi, mt1;
          mov rbx, r12;
          add rbx, rbx;
          imul rbx, LineWidth1;
          add rsi, rbx;

          // valCounter2 := PConstDoubleArr(pMT2);
          // inc(PByte(valCounter2), (2*y + 1)*LineWidth2);
          mov rdi, pMt2;
          mov rbx, r12;
          add rbx, rbx;
          imul rbx, r15;
          add rbx, r15;
          add rdi, rbx;

          // tmp[0] := valCounter1^[0];
          // inc(PByte(valCounter1), LineWidth1);
          movsd xmm0, [rsi];
          add rsi, LineWidth1;

          // if height2 - 2*y - 1 > 0 then
          mov rbx, r12;
          add rbx, rbx;
          inc rbx;

          cmp rbx, height2;
          jnl @@PreInnerLoop;
              // tmp[0] := tmp[0] + valCounter1^[0]*valCounter2^[0];
              // tmp[1] := valCounter1^[0];
              movsd xmm1, [rsi];
              movlhps xmm0, xmm1;

              mulsd xmm1, [rdi];
              addsd xmm0, xmm1;

              //inc(PByte(valCounter1), LineWidth1);
              //inc(PByte(valCounter2), LineWidth2);

              add rsi, LineWidth1;
              add rdi, r15;

          @@PreInnerLoop:

          // rest is a double column!

          // prepare loop
          mov rbx, height2;
          sub rbx, r12;
          sub rbx, r12;
          sub rbx, 2;

          test rbx, rbx;
          jle @@InnerLoopEnd;

          @@InnerLoop:
             // tmp[0] := tmp[0] + valCounter1^[0]*valCounter2^[0];
             // tmp[1] := tmp[1] + valCounter1^[0]*valCounter2^[1];
             movddup xmm1, [rsi];
             movupd xmm2, [rdi];

             mulpd xmm2, xmm1;
             addpd xmm0, xmm2;

             //inc(PByte(valCounter1), LineWidth1);
             //inc(PByte(valCounter2), LineWidth2);

             add rsi, LineWidth1;
             add rdi, r15;

          dec rbx;
          jnz @@InnerLoop;

          @@InnerLoopEnd:


          // write back result

          // pDest^ := tmp[0];
          // PDouble(PAnsiChar(pDest) + sizeof(double))^ := tmp[1];

          movupd [r13], xmm0;

          // inc(pDest, 2);
          // inc(pMT2, 2);
          add r13, 16;
          add pMT2, 16;

     // end foryloop
     inc r12;
     cmp r12, r14;
     jne @@foryloop;

     @@foryloopend:


     //if (width2 and $01) = 1 then
     mov r12, width2;
     and r12, 1;

     jz @@ifend1;

       // special handling of last column (just copy the value)

       // valCounter1 := PConstDoubleArr(mt1);
       mov r12, mt1;

       //inc(PByte(valCounter1), LineWidth1*(height1 - 1));
       mov rbx, height1;
       dec rbx;
       imul rbx, LineWidth1;

       // pDest^ := valCounter1^[0];
       movsd xmm0, [r12 + rbx];
       movsd [r13], xmm0;
     @@ifend1:


     //inc(mt1);
     //inc(PByte(dest), LineWidthDest);
     add mt1, 8;
     add rcx, rdx;    // note: this can be done since dest is rcx

   // end for loop
   dec rax;
   jnz @@forxloop;

   // epilog
   mov r12, iR12;
   mov r13, iR13;
   mov r14, iR14;
   mov r15, iR15;
   mov rsi, iRSI;
   mov rdi, iRDI;
   mov rbx, iRBX;
end;

// calculates mt1 = mt1*mt2', mt2 = lower triangular matrix. diagonal elements are assumed to be 1!
procedure ASMMtxMultLowTria2T2Store1(mt1 : PDouble; LineWidth1 : TASMNativeInt; mt2 : PDouble; LineWidth2 : TASMNativeInt;
  width1, height1, width2, height2 : TASMNativeInt);
var iRBX, iRDI, iRSI : TASMNativeInt;
// rcx: mt1, rdx: LineWidth1, r8 : mt2, r9 : LineWidth2
asm
   {$IFDEF LINUX}
   // Linux uses a diffrent ABI -> copy over the registers so they meet with winABI
   // (note that the 5th and 6th parameter are are on the stack)
   // The parameters are passed in the following order:
   // RDI, RSI, RDX, RCX -> mov to RCX, RDX, R8, R9
   mov r8, rdx;
   mov r9, rcx;
   mov rcx, rdi;
   mov rdx, rsi;
   {$ENDIF}

   // prolog - stack
   mov iRBX, rbx;
   mov iRDI, rdi;
   mov iRSI, rsi;

   // r11 : height1
   mov r11, height1;

   // r10 = iter := -(width2 - 1)*sizeof(double);
   mov r10, width2;
   dec r10;
   imul r10, -8;

   // start from bottom
   // r8: mt2
   // inc(PByte(mt2),(height2 - 1)*LineWidth2);
   mov rax, height2;
   dec rax;
   imul rax, r9;
   add r8, rax;
   sub r8, r10;

   // for x := 0 to width2 - 2
   mov rbx, width2;
   dec rbx;
   jz @@endproc;
   @@forxloop:
      mov rax, rcx;
      sub rax, r10;

      // for y := 0 to height1 - 1
      mov rsi, r11;
      @@foryloop:
         xorpd xmm0, xmm0;

         // for idx := 0 to width2 - x - 2
         mov rdi, r10;
         test rdi, rdi;
         jz @@foridxloopend;

         // unrolled loop 2x2
         add rdi, 32;
         jg @@foridxloopStart;

         @@foridxlongloop:
            movupd xmm1, [rax + rdi - 32];
            movupd xmm2, [r8 + rdi - 32];
            mulpd xmm1, xmm2;
            addpd xmm0, xmm1;

            movupd xmm1, [rax + rdi - 16];
            movupd xmm2, [r8 + rdi - 16];
            mulpd xmm1, xmm2;
            addpd xmm0, xmm1;
         add rdi, 32;
         jl @@foridxlongloop;

         haddpd xmm0, xmm0;

         @@foridxloopStart:
         sub rdi, 32;

         jz @@foridxloopend;

         @@foridxloop:
            movsd xmm1, [rax + rdi];
            movsd xmm2, [r8 + rdi];
            mulsd xmm1, xmm2;
            addsd xmm0, xmm1;

         add rdi, 8;
         jnz @@foridxloop;

         @@foridxloopend:

         // last element is unit:
         movsd xmm1, [rax];
         addsd xmm0, xmm1;

         // write back
         // PConstDoubleArr(pMt1)^[width2 - x - 1] := tmp + valCounter1^[width2 - x - 1];
         movsd [rax], xmm0;

         add rax, rdx;

      dec rsi;
      jnz @@foryloop;

      // dec(PByte(mt2), LineWidth2);
      sub r8, r9;
      sub r8, 8;

      // adjust iterator to the next x value for the idxloop
      add r10, 8;

   dec rbx;
   jnz @@forxloop;

   @@endproc:

   // epilog: stack fixing
   mov rbx, iRBX;
   mov rdi, iRDI;
   mov rsi, iRSI;
end;

procedure ASMMtxMultTria2Store1Unit(mt1 : PDouble; LineWidth1 : TASMNativeInt; mt2 : PDouble; LineWidth2 : TASMNativeInt;
  width1, height1, width2, height2 : TASMNativeInt);
var iRBX, iRDI, iRSI, iR12 : TASMNativeInt;
// rcx : mt1, rdx : LineWidth1, r8 : mt2, r9: LineWidth2
asm
   {$IFDEF LINUX}
   // Linux uses a diffrent ABI -> copy over the registers so they meet with winABI
   // (note that the 5th and 6th parameter are are on the stack)
   // The parameters are passed in the following order:
   // RDI, RSI, RDX, RCX -> mov to RCX, RDX, R8, R9
   mov r8, rdx;
   mov r9, rcx;
   mov rcx, rdi;
   mov rdx, rsi;
   {$ENDIF}

   // prolog - stack
   mov iRBX, rbx;
   mov iRDI, rdi;
   mov iRSI, rsi;
   mov iR12, r12;
   // init

   // r12: height1
   mov r12, height1;

   // r10 : iter := -(width1-1)*sizeof(double);
   mov r10, width1;
   dec r10;
   imul r10, -8;

   // inc(mt2, width2 - 1);
   mov rax, width2;
   dec rax;
   imul rax, 8; //sizeof(double)
   add r8, rax;

   // r11: width2
   mov r11, width2;
   dec r11;

   // for x := 0 to width2 - 2 do
   @@forxloop:
      mov rdi, r12;

      mov rax, rcx;
      sub rax, r10;

      // for y := 0 to height1 - 1
      @@foryloop:
         // tmp := 0;
         xorpd xmm0, xmm0;

         // r8, mt2
         mov rbx, r8;

         // for idx := 0 to width1 - x - 2
         mov rsi, r10;

         // check if we have enough r10ations:
         cmp rsi, 0;
         jge @@foridxloopend;

         @@foridxloop:
            movsd xmm1, [rbx];
            movsd xmm2, [rax + rsi]

            add rbx, r9;  // + linewidth2

            mulsd xmm1, xmm2;
            addsd xmm0, xmm1;

         add rsi, 8;
         jnz @@foridxloop;

         @@foridxloopend:

         // last element is unit
         addsd xmm0, [rax];

         // PConstDoubleArr(pmt1)^[width2 - 1 - x] := tmp;
         mov rbx, rax;
         add rbx, r10;
         mov rsi, r11;
         movsd [rbx + 8*rsi], xmm0;

         // inc(PByte(pmT1), LineWidth1);
         add rax, rdx;

      dec rdi;
      jnz @@foryloop;

      // reduce for idx loop
      add r10, 8;
      // dec(mt2);
      sub r8, 8;

   dec r11;
   jnz @@forxloop;

   @@endproc:

   // cleanup stack
   mov rbx, iRBX;
   mov rdi, iRDI;
   mov rsi, iRSI;
   mov r12, iR12;

end;

{$ENDIF}

end.
