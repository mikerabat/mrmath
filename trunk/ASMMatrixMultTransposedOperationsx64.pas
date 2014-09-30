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


unit ASMMatrixMultTransposedOperationsx64;

// ##############################################################
// #### Assembler optimized matrix multiplication assuming a transposed second
// #### matrix
// ##############################################################

interface

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF x64}

uses MatrixConst;

procedure ASMMatrixMultAlignedEvenW1EvenH2Transposed(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
procedure ASMMatrixMultAlignedEvenW1EvenH2TransposedMod16(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
procedure ASMMatrixMultUnAlignedEvenW1EvenH2Transposed(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);

procedure ASMMatrixMultAlignedEvenW1OddH2Transposed(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
procedure ASMMatrixMultUnAlignedEvenW1OddH2Transposed(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);

procedure ASMMatrixMultAlignedOddW1EvenH2Transposed(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
procedure ASMMatrixMultUnAlignedOddW1EvenH2Transposed(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);

procedure ASMMatrixMultAlignedOddW1OddH2Transposed(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
procedure ASMMatrixMultUnAlignedOddW1OddH2Transposed(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);

{$ENDIF}

implementation

{$IFDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$ENDIF}

procedure ASMMatrixMultUnAlignedEvenW1OddH2Transposed(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var dXMM4, dXMM5, dXMM6, dXMM7 : Array[0..1] of double;
    iRBX, iRSI, iRDI, iR12, iR13, iR14, iR15 : int64;

{$IFDEF FPC}
begin
{$ENDIF}
asm
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

   // iters1 := height2 div 2;
   mov r15, height2;
   shr r15, 1;

   // iters2 := -width1*sizeof(double);
   mov r14, width1;
   shl r14, 3;
   imul r14, -1;

   // LineWidth2_2 := 2*LineWidth2;
   mov r13, LineWidth2;

   mov r12, r13;
   shl r12, 1;

   // prepare matrix pointers - remove constant offset here instead each time in the loop:
   sub r8, r14;
   sub r9, r14;
   mov r10, r9;
   add r10, r13;

   xorpd xmm2, xmm2;
   xorpd xmm6, xmm6;

   // for y := 0 to height1 - 1:
   mov r11, Height1;
   @@forylabel:
       mov rdi, r9;
       mov rsi, r10;
       mov r13, rcx;

       // for x := 0 to width2 - 1:
       mov rbx, r15;
       cmp rbx, 0;
       je @@fory2labelexit;

       @@fory2label:
           xorpd xmm0, xmm0;   // dest^ := 0
           xorpd xmm7, xmm7;   // dest + 1 := 0;
           // for idx := 0 to width1 div 2 do
           mov rax, r14;

           @@InnerLoop:
               movupd xmm1, [r8 + rax];
               movupd xmm5, xmm1;

               // load 2x2 block
               movupd xmm3, [rdi + rax];
               movupd xmm4, [rsi + rax];

               // multiply 2x2 and add
               mulpd xmm3, xmm1;
               mulpd xmm4, xmm5;

               addpd xmm0, xmm3;
               addpd xmm7, xmm4;

               // end for idx := 0 to width1 div 2
           add rax, 16;
           jnz @@InnerLoop;

           haddpd xmm0, xmm2;
           haddpd xmm7, xmm6;

           // compact result
           movlhps xmm0, xmm7;

           // store back result
           movupd [r13], xmm0;

           // increment the pointers
           // inc(mt2), inc(dest);
           //add dword ptr [mt2], 8;
           add r13, 16;
           add rdi, r12;
           add rsi, r12;
       // end for x := 0 to width2 - 1
       dec rbx;
       jnz @@fory2label;

       @@fory2labelexit:

       // take care of the last line separatly
       xorpd xmm0, xmm0;   // dest^ := 0
       // for idx := 0 to width1 div 2 do
       mov rax, r14;

       @@InnerLoop2:
           movupd xmm3, [r8 + rax];
           movupd xmm4, [rdi + rax];

           // multiply 2x2 and add
           mulpd xmm3, xmm4;
           addpd xmm0, xmm3;

           // end for idx := 0 to width1 div 2
       add rax, 16;
       jnz @@InnerLoop2;

       haddpd xmm0, xmm2;

       // store back result
       movlpd [r13], xmm0;

       // dec(mt2, Width2);
       // inc(PByte(mt1), LineWidth1);
       // inc(PByte(dest), destOffset);
       //mov ebx, bytesWidth2;
       //sub dword ptr [mt2], ebx;
       add r8, LineWidth1;
       add rcx, rdx;

   // end for y := 0 to height1 - 1
   dec r11;
   jnz @@forylabel;

   // epilog - cleanup stack
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
{$IFDEF FPC}
end;
{$ENDIF}
end;

procedure ASMMatrixMultAlignedEvenW1OddH2Transposed(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var dXMM4, dXMM5, dXMM6, dXMM7 : Array[0..1] of double;
    iRBX, iRSI, iRDI, iR12, iR13, iR14, iR15 : int64;

{$IFDEF FPC}
begin
{$ENDIF}
asm
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

   // iters1 := height2 div 2;
   mov r15, height2;
   shr r15, 1;

	  // iters2 := -width1*sizeof(double);
   mov r14, width1;
   shl r14, 3;
   imul r14, -1;

   // LineWidth2_2 := 2*LineWidth2;
   mov r13, LineWidth2;

   mov r12, r13;
   shl r12, 1;

			// prepare matrix pointers - remove constant offset here instead each time in the loop:
   sub r8, r14;
   sub r9, r14;
   mov r10, r9;
   add r10, r13;

   xorpd xmm2, xmm2;
   xorpd xmm6, xmm6;

   // for y := 0 to height1 - 1:
   mov r11, Height1;
   @@forylabel:
       mov rdi, r9;
       mov rsi, r10;
       mov r13, rcx;

       // for x := 0 to width2 - 1:
       mov rbx, r15;
       cmp rbx, 0;
       je @@fory2labelexit;

       @@fory2label:
           xorpd xmm0, xmm0;   // dest^ := 0
           xorpd xmm7, xmm7;   // dest + 1 := 0;
           // for idx := 0 to width1 div 2 do
           mov rax, r14;

           @@InnerLoop:
               movapd xmm1, [r8 + rax];
               movapd xmm5, xmm1;

               // load 2x2 block
               movapd xmm3, [rdi + rax];
               movapd xmm4, [rsi + rax];

               // multiply 2x2 and add
               mulpd xmm3, xmm1;
               mulpd xmm4, xmm5;

               addpd xmm0, xmm3;
               addpd xmm7, xmm4;

               // end for idx := 0 to width1 div 2
           add rax, 16;
           jnz @@InnerLoop;

           haddpd xmm0, xmm2;
           haddpd xmm7, xmm6;

           // compact result
           movlhps xmm0, xmm7;

           // store back result
           movntdq [r13], xmm0;

           // increment the pointers
           // inc(mt2), inc(dest);
           //add dword ptr [mt2], 8;
           add r13, 16;
           add rdi, r12;
           add rsi, r12;
       // end for x := 0 to width2 - 1
       dec rbx;
       jnz @@fory2label;

       @@fory2labelexit:

       // take care of the last line separatly
       xorpd xmm0, xmm0;   // dest^ := 0
       // for idx := 0 to width1 div 2 do
       mov rax, r14;

       @@InnerLoop2:
           movapd xmm3, [r8 + rax];

           // multiply 2x2 and add
           mulpd xmm3, [rdi + rax];;
           addpd xmm0, xmm3;

           // end for idx := 0 to width1 div 2
       add rax, 16;
       jnz @@InnerLoop2;

       haddpd xmm0, xmm2;

       // store back result
       movlpd [r13], xmm0;

       // dec(mt2, Width2);
       // inc(PByte(mt1), LineWidth1);
       // inc(PByte(dest), destOffset);
       //mov ebx, bytesWidth2;
       //sub dword ptr [mt2], ebx;
       add r8, LineWidth1;
       add rcx, rdx;

   // end for y := 0 to height1 - 1
   dec r11;
   jnz @@forylabel;

   // epilog - cleanup stack
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
{$IFDEF FPC}
end;
{$ENDIF}
end;

procedure ASMMatrixMultAlignedEvenW1EvenH2TransposedMod16(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var dXMM4, dXMM5, dXMM6, dXMM7 : Array[0..1] of double;
    iRBX, iRSI, iRDI, iR12, iR13, iR14, iR15 : int64;

{$IFDEF FPC}
begin
{$ENDIF}
asm
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

   // iters1 := height2 div 2;
   mov r15, height2;
   shr r15, 1;

	  // iters2 := -width1*sizeof(double);
   mov r14, width1;
   shl r14, 3;
   imul r14, -1;

   // LineWidth2_2 := 2*LineWidth2;
   mov r13, LineWidth2;

   mov r12, r13;
   shl r12, 1;

			// prepare matrix pointers - remove constant offset here instead each time in the loop:
   sub r8, r14;
   sub r9, r14;
   mov r10, r9;
   add r10, r13;

   xorpd xmm2, xmm2;
   xorpd xmm6, xmm6;

   // for y := 0 to height1 - 1:
   mov r11, Height1;
   @@forylabel:
       mov rdi, r9;
       mov rsi, r10;
       mov r13, rcx;

       // for x := 0 to width2 - 1:
       mov rbx, r15;
       @@fory2label:
           xorpd xmm0, xmm0;   // dest^ := 0
           xorpd xmm7, xmm7;   // dest + 1 := 0;
           // for idx := 0 to width1 div 2 do
           mov rax, r14;

           @@InnerLoop:
               // prefetch [ecx + edx + 128];
               // prefetch [edi + edx + 128];
               // prefetch [eax + edx + 128];

               movapd xmm1, [r8 + rax];
               movapd xmm5, xmm1;

               // load 2x2 block
               movapd xmm3, [rdi + rax];
               movapd xmm4, [rsi + rax];

               // multiply 2x2 and add
               mulpd xmm3, xmm1;
               mulpd xmm4, xmm5;

               addpd xmm0, xmm3;
               addpd xmm7, xmm4;

               movapd xmm1, [r8 + rax + 16];
               movapd xmm5, xmm1;

               // load 2x2 block
               movapd xmm3, [rdi + rax + 16];
               movapd xmm4, [rsi + rax + 16];

               // multiply 2x2 and add
               mulpd xmm3, xmm1;
               mulpd xmm4, xmm5;

               addpd xmm0, xmm3;
               addpd xmm7, xmm4;

               movapd xmm1, [r8 + rax + 32];
               movapd xmm5, xmm1;

               // load 2x2 block
               movapd xmm3, [rdi + rax + 32];
               movapd xmm4, [rsi + rax + 32];

               // multiply 2x2 and add
               mulpd xmm3, xmm1;
               mulpd xmm4, xmm5;

               addpd xmm0, xmm3;
               addpd xmm7, xmm4;

               movapd xmm1, [r8 + rax + 48];
               movapd xmm5, xmm1;

               // load 2x2 block
               movapd xmm3, [rdi + rax + 48];
               movapd xmm4, [rsi + rax + 48];

               // multiply 2x2 and add
               mulpd xmm3, xmm1;
               mulpd xmm4, xmm5;

               addpd xmm0, xmm3;
               addpd xmm7, xmm4;

               movapd xmm1, [r8 + rax + 64];
               movapd xmm5, xmm1;

               // load 2x2 block
               movapd xmm3, [rdi + rax + 64];
               movapd xmm4, [rsi + rax + 64];

               // multiply 2x2 and add
               mulpd xmm3, xmm1;
               mulpd xmm4, xmm5;

               addpd xmm0, xmm3;
               addpd xmm7, xmm4;

               movapd xmm1, [r8 + rax + 80];
               movapd xmm5, xmm1;

               // load 2x2 block
               movapd xmm3, [rdi + rax + 80];
               movapd xmm4, [rsi + rax + 80];

               // multiply 2x2 and add
               mulpd xmm3, xmm1;
               mulpd xmm4, xmm5;

               addpd xmm0, xmm3;
               addpd xmm7, xmm4;

               movapd xmm1, [r8 + rax + 96];
               movapd xmm5, xmm1;

               // load 2x2 block
               movapd xmm3, [rdi + rax + 96];
               movapd xmm4, [rsi + rax + 96];

               // multiply 2x2 and add
               mulpd xmm3, xmm1;
               mulpd xmm4, xmm5;

               addpd xmm0, xmm3;
               addpd xmm7, xmm4;

               movapd xmm1, [r8 + rax + 112];
               movapd xmm5, xmm1;

               // load 2x2 block
               movapd xmm3, [rdi + rax + 112];
               movapd xmm4, [rsi + rax + 112];

               // multiply 2x2 and add
               mulpd xmm3, xmm1;
               mulpd xmm4, xmm5;

               addpd xmm0, xmm3;
               addpd xmm7, xmm4;

               // end for idx := 0 to width1 div 2
           add rax, 128;
           jnz @@InnerLoop;

           haddpd xmm0, xmm2;
           haddpd xmm7, xmm6;

           // compact result
           movlhps xmm0, xmm7;

           // store back result
           movntdq [r13], xmm0;

           // increment the pointers
           // inc(mt2), inc(dest);
           //add dword ptr [mt2], 8;
           add r13, 16;
           add rdi, r12;
           add rsi, r12;
       // end for x := 0 to width2 - 1
       dec rbx;
       jnz @@fory2label;

       // dec(mt2, Width2);
       // inc(PByte(mt1), LineWidth1);
       // inc(PByte(dest), destOffset);
       //mov ebx, bytesWidth2;
       //sub dword ptr [mt2], ebx;
       add r8, LineWidth1;
       add rcx, rdx;

   // end for y := 0 to height1 - 1
   dec r11;
   jnz @@forylabel;

   // epilog - cleanup stack
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
{$IFDEF FPC}
end;
{$ENDIF}
end;

// note mt2 is transposed this time -> width1 and width2 must be the same!
procedure ASMMatrixMultAlignedEvenW1EvenH2Transposed(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var dXMM4, dXMM5, dXMM6, dXMM7 : Array[0..1] of double;
    iRBX, iRSI, iRDI, iR12, iR13, iR14, iR15 : int64;

{$IFDEF FPC}
begin
{$ENDIF}
asm
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

   // iters1 := height2 div 2;
   mov r15, height2;
   shr r15, 1;

	  // iters2 := -width1*sizeof(double);
   mov r14, width1;
   shl r14, 3;
   imul r14, -1;

   // LineWidth2_2 := 2*LineWidth2;
   mov r13, LineWidth2;

   mov r12, r13;
   shl r12, 1;

			// prepare matrix pointers - remove constant offset here instead each time in the loop:
   sub r8, r14;
   sub r9, r14;
   mov r10, r9;
   add r10, r13;

   xorpd xmm2, xmm2;
   xorpd xmm6, xmm6;

   // for y := 0 to height1 - 1:
   mov r11, Height1;
   @@forylabel:
       mov rdi, r9;
       mov rsi, r10;
       mov r13, rcx;

       // for x := 0 to width2 - 1:
       mov rbx, r15;
       @@fory2label:
           xorpd xmm0, xmm0;   // dest^ := 0
           xorpd xmm7, xmm7;   // dest + 1 := 0;
           // for idx := 0 to width1 div 2 do
           mov rax, r14;

           @@InnerLoop:
               movapd xmm1, [r8 + rax];
               movapd xmm5, xmm1;

               // load 2x2 block
               movapd xmm3, [rdi + rax];
               movapd xmm4, [rsi + rax];

               // multiply 2x2 and add
               mulpd xmm3, xmm1;
               mulpd xmm4, xmm5;

               addpd xmm0, xmm3;
               addpd xmm7, xmm4;

               // end for idx := 0 to width1 div 2
           add rax, 16;
           jnz @@InnerLoop;

           haddpd xmm0, xmm2;
           haddpd xmm7, xmm6;

           // compact result
           movlhps xmm0, xmm7;

           // store back result
           movntdq [r13], xmm0;

           // increment the pointers
           // inc(mt2), inc(dest);
           //add dword ptr [mt2], 8;
           add r13, 16;
           add rdi, r12;
           add rsi, r12;
       // end for x := 0 to width2 - 1
       dec rbx;
       jnz @@fory2label;

       // dec(mt2, Width2);
       // inc(PByte(mt1), LineWidth1);
       // inc(PByte(dest), destOffset);
       //mov ebx, bytesWidth2;
       //sub dword ptr [mt2], ebx;
       add r8, LineWidth1;
       add rcx, rdx;

   // end for y := 0 to height1 - 1
   dec r11;
   jnz @@forylabel;

   // epilog - cleanup stack
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
{$IFDEF FPC}
end;
{$ENDIF}
end;

procedure ASMMatrixMultUnAlignedEvenW1EvenH2Transposed(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var dXMM4, dXMM5, dXMM6, dXMM7 : Array[0..1] of double;
    iRBX, iRSI, iRDI, iR12, iR13, iR14, iR15 : int64;

{$IFDEF FPC}
begin
{$ENDIF}
asm
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

   // iters1 := height2 div 2;
   mov r15, height2;
   shr r15, 1;

	  // iters2 := -width1*sizeof(double);
   mov r14, width1;
   shl r14, 3;
   imul r14, -1;

   // LineWidth2_2 := 2*LineWidth2;
   mov r13, LineWidth2;

   mov r12, r13;
   shl r12, 1;

			// prepare matrix pointers - remove constant offset here instead each time in the loop:
   sub r8, r14;
   sub r9, r14;
   mov r10, r9;
   add r10, r13;

   xorpd xmm2, xmm2;
   xorpd xmm6, xmm6;

   // for y := 0 to height1 - 1:
   mov r11, Height1;
   @@forylabel:
       mov rdi, r9;
       mov rsi, r10;
       mov r13, rcx;

       // for x := 0 to width2 - 1:
       mov rbx, r15;
       @@fory2label:
           xorpd xmm0, xmm0;   // dest^ := 0
           xorpd xmm7, xmm7;   // dest + 1 := 0;
           // for idx := 0 to width1 div 2 do
           mov rax, r14;

           @@InnerLoop:
               movupd xmm1, [r8 + rax];
               movupd xmm5, xmm1;

               // load 2x2 block
               movupd xmm3, [rdi + rax];
               movupd xmm4, [rsi + rax];

               // multiply 2x2 and add
               mulpd xmm3, xmm1;
               mulpd xmm4, xmm5;

               addpd xmm0, xmm3;
               addpd xmm7, xmm4;

               // end for idx := 0 to width1 div 2
           add rax, 16;
           jnz @@InnerLoop;

           haddpd xmm0, xmm2;
           haddpd xmm7, xmm6;

           // compact result
           movlhps xmm0, xmm7;

           // store back result
           movupd [r13], xmm0;

           // increment the pointers
           // inc(mt2), inc(dest);
           //add dword ptr [mt2], 8;
           add r13, 16;
           add rdi, r12;
           add rsi, r12;
       // end for x := 0 to width2 - 1
       dec rbx;
       jnz @@fory2label;

       // dec(mt2, Width2);
       // inc(PByte(mt1), LineWidth1);
       // inc(PByte(dest), destOffset);
       //mov ebx, bytesWidth2;
       //sub dword ptr [mt2], ebx;
       add r8, LineWidth1;
       add rcx, rdx;

   // end for y := 0 to height1 - 1
   dec r11;
   jnz @@forylabel;

   // epilog - cleanup stack
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
{$IFDEF FPC}
end;
{$ENDIF}
end;

procedure ASMMatrixMultUnAlignedOddW1EvenH2Transposed(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var dXMM4, dXMM5, dXMM6, dXMM7 : Array[0..1] of double;
    iRBX, iRSI, iRDI, iR12, iR13, iR14, iR15 : int64;

{$IFDEF FPC}
begin
{$ENDIF}
asm
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

   // iters1 := height2 div 2;
   mov r15, height2;
   shr r15, 1;

	  // iters2 := -width1*sizeof(double);
   mov r14, width1;
   dec r14;
   shl r14, 3;
   imul r14, -1;

   // LineWidth2_2 := 2*LineWidth2;
   mov r13, LineWidth2;

   mov r12, r13;
   shl r12, 1;

			// prepare matrix pointers - remove constant offset here instead each time in the loop:
   sub r8, r14;
   sub r9, r14;
   mov r10, r9;
   add r10, r13;

   xorpd xmm2, xmm2;
   xorpd xmm6, xmm6;

   // for y := 0 to height1 - 1:
   mov r11, Height1;
   @@forylabel:
       mov rdi, r9;
       mov rsi, r10;
       mov r13, rcx;

       // for x := 0 to width2 - 1:
       mov rbx, r15;
       @@fory2label:
           xorpd xmm0, xmm0;   // dest^ := 0
           xorpd xmm7, xmm7;   // dest + 1 := 0;
           // for idx := 0 to width1 div 2 do
           mov rax, r14;

           @@InnerLoop:
               movupd xmm1, [r8 + rax];
               movupd xmm5, xmm1;

               // load 2x2 block
               movupd xmm3, [rdi + rax];
               movupd xmm4, [rsi + rax];

               // multiply 2x2 and add
               mulpd xmm3, xmm1;
               mulpd xmm4, xmm5;

               addpd xmm0, xmm3;
               addpd xmm7, xmm4;

               // end for idx := 0 to width1 div 2
           add rax, 16;
           jnz @@InnerLoop;

           // multiply and add the last element
           movlpd xmm1, [r8];
           movupd xmm5, xmm1;

           movlpd xmm3, [rdi];
           movlpd xmm4, [rsi];

           mulsd xmm3, xmm1;
           mulsd xmm4, xmm5;

           haddpd xmm0, xmm2;
           haddpd xmm7, xmm6;

           addsd xmm0, xmm3;
           addsd xmm7, xmm4;

           // compact result
           movlhps xmm0, xmm7;

           // store back result
           movupd [r13], xmm0;

           // increment the pointers
           // inc(mt2), inc(dest);
           //add dword ptr [mt2], 8;
           add r13, 16;
           add rdi, r12;
           add rsi, r12;
       // end for x := 0 to width2 - 1
       dec rbx;
       jnz @@fory2label;

       // dec(mt2, Width2);
       // inc(PByte(mt1), LineWidth1);
       // inc(PByte(dest), destOffset);
       //mov ebx, bytesWidth2;
       //sub dword ptr [mt2], ebx;
       add r8, LineWidth1;
       add rcx, rdx;

   // end for y := 0 to height1 - 1
   dec r11;
   jnz @@forylabel;

   // epilog - cleanup stack
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
{$IFDEF FPC}
end;
{$ENDIF}
end;

procedure ASMMatrixMultAlignedOddW1EvenH2Transposed(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var dXMM4, dXMM5, dXMM6, dXMM7 : Array[0..1] of double;
    iRBX, iRSI, iRDI, iR12, iR13, iR14, iR15 : int64;

{$IFDEF FPC}
begin
{$ENDIF}
asm
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

   // iters1 := height2 div 2;
   mov r15, height2;
   shr r15, 1;

	  // iters2 := -width1*sizeof(double);
   mov r14, width1;
   dec r14;
   shl r14, 3;
   imul r14, -1;

   // LineWidth2_2 := 2*LineWidth2;
   mov r13, LineWidth2;

   mov r12, r13;
   shl r12, 1;

			// prepare matrix pointers - remove constant offset here instead each time in the loop:
   sub r8, r14;
   sub r9, r14;
   mov r10, r9;
   add r10, r13;

   xorpd xmm2, xmm2;
   xorpd xmm6, xmm6;

   // for y := 0 to height1 - 1:
   mov r11, Height1;
   @@forylabel:
       mov rdi, r9;
       mov rsi, r10;
       mov r13, rcx;

       // for x := 0 to width2 - 1:
       mov rbx, r15;
       @@fory2label:
           xorpd xmm0, xmm0;   // dest^ := 0
           xorpd xmm7, xmm7;   // dest + 1 := 0;
           // for idx := 0 to width1 div 2 do
           mov rax, r14;

           @@InnerLoop:
               movapd xmm1, [r8 + rax];
               movapd xmm5, xmm1;

               // load 2x2 block
               movapd xmm3, [rdi + rax];
               movapd xmm4, [rsi + rax];

               // multiply 2x2 and add
               mulpd xmm3, xmm1;
               mulpd xmm4, xmm5;

               addpd xmm0, xmm3;
               addpd xmm7, xmm4;

               // end for idx := 0 to width1 div 2
           add rax, 16;
           jnz @@InnerLoop;

           // multiply and add the last element
           movlpd xmm1, [r8];
           movapd xmm5, xmm1;

           movlpd xmm3, [rdi];
           movlpd xmm4, [rsi];

           mulsd xmm3, xmm1;
           mulsd xmm4, xmm5;

           haddpd xmm0, xmm2;
           haddpd xmm7, xmm6;

           addsd xmm0, xmm3;
           addsd xmm7, xmm4;

           // compact result
           movlhps xmm0, xmm7;

           // store back result
           movntdq [r13], xmm0;

           // increment the pointers
           // inc(mt2), inc(dest);
           //add dword ptr [mt2], 8;
           add r13, 16;
           add rdi, r12;
           add rsi, r12;
       // end for x := 0 to width2 - 1
       dec rbx;
       jnz @@fory2label;

       // dec(mt2, Width2);
       // inc(PByte(mt1), LineWidth1);
       // inc(PByte(dest), destOffset);
       //mov ebx, bytesWidth2;
       //sub dword ptr [mt2], ebx;
       add r8, LineWidth1;
       add rcx, rdx;

   // end for y := 0 to height1 - 1
   dec r11;
   jnz @@forylabel;

   // epilog - cleanup stack
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
{$IFDEF FPC}
end;
{$ENDIF}
end;

procedure ASMMatrixMultAlignedOddW1OddH2Transposed(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var dXMM4, dXMM5, dXMM6, dXMM7 : Array[0..1] of double;
    iRBX, iRSI, iRDI, iR12, iR13, iR14, iR15 : int64;

{$IFDEF FPC}
begin
{$ENDIF}
asm
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

   // iters1 := height2 div 2;
   mov r15, height2;
   shr r15, 1;

	  // iters2 := -(width1 - 1)*sizeof(double);
   mov r14, width1;
   dec r14;
   shl r14, 3;
   imul r14, -1;

   // LineWidth2_2 := 2*LineWidth2;
   mov r13, LineWidth2;

   mov r12, r13;
   shl r12, 1;

			// prepare matrix pointers - remove constant offset here instead each time in the loop:
   sub r8, r14;
   sub r9, r14;
   mov r10, r9;
   add r10, r13;

   xorpd xmm2, xmm2;
   xorpd xmm6, xmm6;

   // for y := 0 to height1 - 1:
   mov r11, Height1;
   @@forylabel:
       mov rdi, r9;
       mov rsi, r10;
       mov r13, rcx;

       // for x := 0 to width2 - 1:
       mov rbx, r15;
       cmp rbx, 0;
       je @@fory2labelexit;

       @@fory2label:
           xorpd xmm0, xmm0;   // dest^ := 0
           xorpd xmm7, xmm7;   // dest + 1 := 0;
           // for idx := 0 to width1 div 2 do
           mov rax, r14;

           @@InnerLoop:
               movapd xmm1, [r8 + rax];
               movapd xmm5, xmm1;

               // load 2x2 block
               movapd xmm3, [rdi + rax];
               movapd xmm4, [rsi + rax];

               // multiply 2x2 and add
               mulpd xmm3, xmm1;
               mulpd xmm4, xmm5;

               addpd xmm0, xmm3;
               addpd xmm7, xmm4;

               // end for idx := 0 to width1 div 2
           add rax, 16;
           jnz @@InnerLoop;

           // multiply and add the last element
           movlpd xmm1, [r8];
           movapd xmm5, xmm1;

           movlpd xmm3, [rdi];
           movlpd xmm4, [rsi];

           mulsd xmm3, xmm1;
           mulsd xmm4, xmm5;

           haddpd xmm0, xmm2;
           haddpd xmm7, xmm6;

           addsd xmm0, xmm3;
           addsd xmm7, xmm4;

           // compact result
           movlhps xmm0, xmm7;

           // store back result
           movntdq [r13], xmm0;

           // increment the pointers
           // inc(mt2), inc(dest);
           //add dword ptr [mt2], 8;
           add r13, 16;
           add rdi, r12;
           add rsi, r12;
       // end for x := 0 to width2 - 1
       dec rbx;
       jnz @@fory2label;

       @@fory2labelexit:

       // take care of the last line separatly
       xorpd xmm0, xmm0;   // dest^ := 0
       // for idx := 0 to width1 div 2 do
       mov rax, r14;

       @@InnerLoop2:
           movapd xmm3, [r8 + rax];

           // multiply 2x2 and add
           mulpd xmm3, [rdi + rax];;
           addpd xmm0, xmm3;

           // end for idx := 0 to width1 div 2
       add rax, 16;
       jnz @@InnerLoop2;

       // multiply and add the last element
   				movlpd xmm1, [r8];
       movlpd xmm3, [rdi];

       mulsd xmm3, xmm1;
       haddpd xmm0, xmm2;
       addsd xmm0, xmm3;

       // store back result
       movlpd [r13], xmm0;

       // dec(mt2, Width2);
       // inc(PByte(mt1), LineWidth1);
       // inc(PByte(dest), destOffset);
       //mov ebx, bytesWidth2;
       //sub dword ptr [mt2], ebx;
       add r8, LineWidth1;
       add rcx, rdx;

   // end for y := 0 to height1 - 1
   dec r11;
   jnz @@forylabel;

   // epilog - cleanup stack
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
{$IFDEF FPC}
end;
{$ENDIF}
end;

procedure ASMMatrixMultUnAlignedOddW1OddH2Transposed(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var dXMM4, dXMM5, dXMM6, dXMM7 : Array[0..1] of double;
    iRBX, iRSI, iRDI, iR12, iR13, iR14, iR15 : int64;

{$IFDEF FPC}
begin
{$ENDIF}
asm
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

   // iters1 := height2 div 2;
   mov r15, height2;
   shr r15, 1;

	  // iters2 := -(width1 - 1)*sizeof(double);
   mov r14, width1;
   dec r14;
   shl r14, 3;
   imul r14, -1;

   // LineWidth2_2 := 2*LineWidth2;
   mov r13, LineWidth2;

   mov r12, r13;
   shl r12, 1;

			// prepare matrix pointers - remove constant offset here instead each time in the loop:
   sub r8, r14;
   sub r9, r14;
   mov r10, r9;
   add r10, r13;

   xorpd xmm2, xmm2;
   xorpd xmm6, xmm6;

   // for y := 0 to height1 - 1:
   mov r11, Height1;
   @@forylabel:
       mov rdi, r9;
       mov rsi, r10;
       mov r13, rcx;

       // for x := 0 to width2 - 1:
       mov rbx, r15;
       cmp rbx, 0;
       je @@fory2labelexit;

       @@fory2label:
           xorpd xmm0, xmm0;   // dest^ := 0
           xorpd xmm7, xmm7;   // dest + 1 := 0;
           // for idx := 0 to width1 div 2 do
           mov rax, r14;

           @@InnerLoop:
               movupd xmm1, [r8 + rax];
               movapd xmm5, xmm1;

               // load 2x2 block
               movupd xmm3, [rdi + rax];
               movupd xmm4, [rsi + rax];

               // multiply 2x2 and add
               mulpd xmm3, xmm1;
               mulpd xmm4, xmm5;

               addpd xmm0, xmm3;
               addpd xmm7, xmm4;

               // end for idx := 0 to width1 div 2
           add rax, 16;
           jnz @@InnerLoop;

           // multiply and add the last element
           movlpd xmm1, [r8];
           movapd xmm5, xmm1;

           movlpd xmm3, [rdi];
           movlpd xmm4, [rsi];

           mulsd xmm3, xmm1;
           mulsd xmm4, xmm5;

           haddpd xmm0, xmm2;
           haddpd xmm7, xmm6;

           addsd xmm0, xmm3;
           addsd xmm7, xmm4;

           // compact result
           movlhps xmm0, xmm7;

           // store back result
           movupd [r13], xmm0;

           // increment the pointers
           // inc(mt2), inc(dest);
           //add dword ptr [mt2], 8;
           add r13, 16;
           add rdi, r12;
           add rsi, r12;
       // end for x := 0 to width2 - 1
       dec rbx;
       jnz @@fory2label;
       
       @@fory2labelexit:

       // take care of the last line separatly
       xorpd xmm0, xmm0;   // dest^ := 0
       // for idx := 0 to width1 div 2 do
       mov rax, r14;

       @@InnerLoop2:
           movupd xmm3, [r8 + rax];
           movupd xmm4, [rdi + rax];

           // multiply 2x2 and add
           mulpd xmm3, xmm4;
           addpd xmm0, xmm3;

           // end for idx := 0 to width1 div 2
       add rax, 16;
       jnz @@InnerLoop2;

       // multiply and add the last element
   				movlpd xmm1, [r8];
       movlpd xmm3, [rdi];

       mulsd xmm3, xmm1;
       haddpd xmm0, xmm2;
       addsd xmm0, xmm3;

       // store back result
       movlpd [r13], xmm0;

       // dec(mt2, Width2);
       // inc(PByte(mt1), LineWidth1);
       // inc(PByte(dest), destOffset);
       //mov ebx, bytesWidth2;
       //sub dword ptr [mt2], ebx;
       add r8, LineWidth1;
       add rcx, rdx;

   // end for y := 0 to height1 - 1
   dec r11;
   jnz @@forylabel;

   // epilog - cleanup stack
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
{$IFDEF FPC}
end;
{$ENDIF}
end;

{$ENDIF}

end.
