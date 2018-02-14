// ###################################################################
// #### This file is part of the mathematics library project, and is
// #### offered under the licence agreement described on
// #### http://www.mrsoft.org/
// ####
// #### Copyright:(c) 2017, Michael R. . All rights reserved.
// ####
// #### Unless required by applicable law or agreed to in writing, software
// #### distributed under the License is distributed on an "AS IS" BASIS,
// #### WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// #### See the License for the specific language governing permissions and
// #### limitations under the License.
// ###################################################################


unit AVXMatrixMultTransposedOperationsx64;

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



procedure AVXMatrixMultAlignedTransposed(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
procedure AVXMatrixMultAlignedEvenW1EvenH2TransposedMod16(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
procedure AVXMatrixMultAlignedEvenW1OddH2TransposedMod16(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
procedure AVXMatrixMultUnAlignedTransposed(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);

{$ENDIF}

implementation

{$IFDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$ENDIF}

procedure AVXMatrixMultAlignedEvenW1EvenH2TransposedMod16(dest : PDouble; const destLineWidth : TASMNativeInt;
    mt1, mt2 : PDouble; width1 : TASMNativeInt;
    height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt;
    const LineWidth1, LineWidth2 : TASMNativeInt);
var dxMM4 : Array[0..1] of double;
    iRBX, iRSI, iRDI, iR12, iR13, iR14, iR15 : int64;

{$IFDEF FPC}
begin
{$ENDIF}
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

   // prolog - simulate stack
   mov iRBX, rbx;
   mov iRSI, rsi;
   mov iRDI, rdi;
   mov iR12, r12;
   mov iR13, r13;
   mov iR14, r14;
   mov iR15, r15;

   vmovupd dXMM4, xmm4;

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

   // for y := 0 to height1 - 1:
   mov r11, Height1;
   @@forylabel:
       mov rdi, r9;
       mov rsi, r10;
       mov r13, rcx;

       // for y2 := 0 to height2 - 1:
       mov rbx, r15;
       @@fory2label:
           vxorpd ymm0, ymm0, ymm0;   // dest^ := 0
           vxorpd ymm2, ymm2, ymm2;   // dest + 1 := 0;
           // for idx := 0 to width1 div 2 do
           mov rax, r14;

           // for x := 0 to width1 - 1
           @@InnerLoop:
               // prefetch [ecx + edx + 128];
               // prefetch [edi + edx + 128];
               // prefetch [eax + edx + 128];

               vmovapd ymm1, [r8 + rax];

               // multiply 2x2 and add
               vmulpd ymm3, ymm1, [rdi + rax];
               vmulpd ymm4, ymm1, [rsi + rax];

               vaddpd ymm0, ymm0, ymm3;
               vaddpd ymm2, ymm2, ymm4;

               vmovapd ymm1, [r8 + rax + 32];

               // multiply 2x2 and add
               vmulpd ymm3, ymm1, [rdi + rax + 32];
               vmulpd ymm4, ymm1, [rsi + rax + 32];

               vaddpd ymm0, ymm0, ymm3;
               vaddpd ymm2, ymm2, ymm4;

               vmovapd ymm1, [r8 + rax + 64];

               // load 2x2 block
               vmovapd ymm3, [rdi + rax + 64];
               vmovapd ymm4, [rsi + rax + 64];

               // multiply 2x2 and add
               vmulpd ymm3, ymm3, ymm1;
               vmulpd ymm4, ymm4, ymm1;

               vaddpd ymm0, ymm0, ymm3;
               vaddpd ymm2, ymm2, ymm4;

               vmovapd ymm1, [r8 + rax + 96];

               // multiply 2x2 and add
               vmulpd ymm3, ymm1, [rdi + rax + 96];
               vmulpd ymm4, ymm1, [rsi + rax + 96];

               vaddpd ymm0, ymm0, ymm3;
               vaddpd ymm2, ymm2, ymm4;

               // end for idx := 0 to width1 div 2
           add rax, 128;
           jnz @@InnerLoop;

           vextractf128 xmm1, ymm0, 1;
           vextractf128 xmm3, ymm2, 1;

           vhaddpd xmm0, xmm0, xmm1;
           vhaddpd xmm2, xmm2, xmm3;

           vhaddpd xmm0, xmm0, xmm2;

           // store back result
           vmovapd [r13], xmm0;

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

   vmovupd xmm4, dXMM4;
   vzeroupper;
{$IFDEF FPC}
end;
{$ENDIF}
end;

procedure AVXMatrixMultAlignedEvenW1OddH2TransposedMod16(dest : PDouble; const destLineWidth : TASMNativeInt;
    mt1, mt2 : PDouble; width1 : TASMNativeInt;
    height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt;
    const LineWidth1, LineWidth2 : TASMNativeInt);
var dxMM4 : Array[0..1] of double;
    iRBX, iRSI, iRDI, iR12, iR13, iR14, iR15 : int64;

{$IFDEF FPC}
begin
{$ENDIF}
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

   // prolog - simulate stack
   mov iRBX, rbx;
   mov iRSI, rsi;
   mov iRDI, rdi;
   mov iR12, r12;
   mov iR13, r13;
   mov iR14, r14;
   mov iR15, r15;

   vmovupd dXMM4, xmm4;

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

   // for y := 0 to height1 - 1:
   mov r11, Height1;
   @@forylabel:
       mov rdi, r9;
       mov rsi, r10;
       mov r13, rcx;

       // for y2 := 0 to height2 div 2 - 1:
       mov rbx, r15;
       @@fory2label:
           vxorpd ymm0, ymm0, ymm0;   // dest^ := 0
           vxorpd ymm2, ymm2, ymm2;   // dest + 1 := 0;
           // for idx := 0 to width1 div 2 do
           mov rax, r14;

           // for x := 0 to width1 - 1
           @@InnerLoop:
               // prefetch [ecx + edx + 128];
               // prefetch [edi + edx + 128];
               // prefetch [eax + edx + 128];

               vmovapd ymm1, [r8 + rax];

               // load 2x2 block
               vmovapd ymm3, [rdi + rax];
               vmovapd ymm4, [rsi + rax];

               // multiply 2x2 and add
               vmulpd ymm3, ymm3, ymm1;
               vmulpd ymm4, ymm4, ymm1;

               vaddpd ymm0, ymm0, ymm3;
               vaddpd ymm2, ymm2, ymm4;

               vmovapd ymm1, [r8 + rax + 32];

               // load 2x2 block
               vmovapd ymm3, [rdi + rax + 32];
               vmovapd ymm4, [rsi + rax + 32];

               // multiply 2x2 and add
               vmulpd ymm3, ymm3, ymm1;
               vmulpd ymm4, ymm4, ymm1;

               vaddpd ymm0, ymm0, ymm3;
               vaddpd ymm2, ymm2, ymm4;

               vmovapd ymm1, [r8 + rax + 64];

               // load 2x2 block
               vmovapd ymm3, [rdi + rax + 64];
               vmovapd ymm4, [rsi + rax + 64];

               // multiply 2x2 and add
               vmulpd ymm3, ymm3, ymm1;
               vmulpd ymm4, ymm4, ymm1;

               vaddpd ymm0, ymm0, ymm3;
               vaddpd ymm2, ymm2, ymm4;

               vmovapd ymm1, [r8 + rax + 96];

               // load 2x2 block
               vmovapd ymm3, [rdi + rax + 96];
               vmovapd ymm4, [rsi + rax + 96];

               // multiply 2x2 and add
               vmulpd ymm3, ymm3, ymm1;
               vmulpd ymm4, ymm4, ymm1;

               vaddpd ymm0, ymm0, ymm3;
               vaddpd ymm2, ymm2, ymm4;

               // end for idx := 0 to width1 div 2
           add rax, 128;
           jnz @@InnerLoop;

           vextractf128 xmm1, ymm0, 1;
           vextractf128 xmm3, ymm2, 1;

           vhaddpd xmm0, xmm0, xmm1;
           vhaddpd xmm2, xmm2, xmm3;

           vhaddpd xmm0, xmm0, xmm2;

           // store back result
           vmovapd [r13], xmm0;

           // increment the pointers
           // inc(mt2), inc(dest);
           //add dword ptr [mt2], 8;
           add r13, 16;
           add rdi, r12;
           add rsi, r12;
       // end for y := 0 to height2 div 2 - 1
       dec rbx;
       jnz @@fory2label;

       // last odd line:
       vxorpd ymm0, ymm0, ymm0;   // dest^ := 0
       // for idx := 0 to width1 div 2 do
       mov rax, r14;

       // for x := 0 to width1 - 1
       @@InnerLoop2:
          // prefetch [ecx + edx + 128];
          // prefetch [edi + edx + 128];
          // prefetch [eax + edx + 128];

          vmovapd ymm1, [r8 + rax];
          vmovapd ymm3, [rdi + rax];

          // multiply 2x2 and add
          vmulpd ymm3, ymm3, ymm1;
          vaddpd ymm0, ymm0, ymm3;

          vmovapd ymm1, [r8 + rax + 32];
          vmovapd ymm3, [rdi + rax + 32];
          vmulpd ymm3, ymm3, ymm1;
          vaddpd ymm0, ymm0, ymm3;

          vmovapd ymm1, [r8 + rax + 64];
          vmovapd ymm3, [rdi + rax + 64];
          vmulpd ymm3, ymm3, ymm1;
          vaddpd ymm0, ymm0, ymm3;

          vmovapd ymm1, [r8 + rax + 96];
          vmovapd ymm3, [rdi + rax + 96];
          vmulpd ymm3, ymm3, ymm1;
          vaddpd ymm0, ymm0, ymm3;

          // end for idx := 0 to width1 div 2
       add rax, 128;
       jnz @@InnerLoop2;

       vextractf128 xmm1, ymm0, 1;
       vhaddpd xmm0, xmm0, xmm1;
       vhaddpd xmm0, xmm0, xmm0;

       // store back result
       vmovsd [r13], xmm0;

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

   vmovupd xmm4, dXMM4;
   vzeroupper;
{$IFDEF FPC}
end;
{$ENDIF}
end;


// note mt2 is transposed this time -> width1 and width2 must be the same!
procedure AVXMatrixMultUnAlignedTransposed(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var dXMM4 : Array[0..1] of double;
    iRBX, iRSI, iRDI, iR12, iR13, iR14, iR15 : int64;

{$IFDEF FPC}
begin
{$ENDIF}
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

   // prolog - simulate stack
   mov iRBX, rbx;
   mov iRSI, rsi;
   mov iRDI, rdi;
   mov iR12, r12;
   mov iR13, r13;
   mov iR14, r14;
   mov iR15, r15;

   vmovupd dXMM4, xmm4;

   // height 2
   mov r15, height2;

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

   // for y := 0 to height1 - 1:
   mov r11, Height1;
   @@forylabel:
       mov rdi, r9;
       mov rsi, r10;
       mov r13, rcx;

       // for x := 0 to width2 - 1:
       mov rbx, r15;
       sub rbx, 2;
       jl @@fory2loopend;

       @@fory2label:
           vxorpd ymm0, ymm0, ymm0;   // dest^ := 0
           vxorpd ymm2, ymm2, ymm2;   // dest + 1 := 0;
           // for idx := 0 to width1 div 4 do
           mov rax, r14;
           add rax, 32;
           jg @InnerLoopEnd;

           @@InnerLoop:
               vmovupd ymm1, [r8 + rax - 32];

               // load 2x2 block
               vmovupd ymm3, [rdi + rax - 32];
               vmovupd ymm4, [rsi + rax - 32];

               // multiply 2x2 and add
               vmulpd ymm3, ymm3, ymm1;
               vmulpd ymm4, ymm4, ymm1;

               vaddpd ymm0, ymm0, ymm3;
               vaddpd ymm2, ymm2, ymm4;

               // end for idx := 0 to width1 div 2
           add rax, 32;
           jle @@InnerLoop;

           vextractf128 xmm1, ymm0, 1;
           vextractf128 xmm3, ymm2, 1;

           vhaddpd xmm0, xmm0, xmm1;
           vhaddpd xmm2, xmm2, xmm3;

           @InnerLoopEnd:

           sub rax, 32;
           jz @@InnerLoopEnd2;

           // do the final few elements
           @@InnerLoop2:
               vmovsd xmm1, [r8 + rax];

               // load 2x2 block
               vmovsd xmm3, [rdi + rax];
               vmovsd xmm4, [rsi + rax];

               // multiply 2x2 and add
               vmulsd xmm3, xmm3, xmm1;
               vmulsd xmm4, xmm4, xmm1;

               vaddsd xmm0, xmm0, xmm3;
               vaddsd xmm2, xmm2, xmm4;

           add rax, 8;
           jnz @@InnerLoop2;

           @@InnerLoopEnd2:

           // final add and compact result
           vhaddpd xmm0, xmm0, xmm2;

           // store back result
           vmovupd [r13], xmm0;

           // increment the pointers
           // inc(mt2), inc(dest);
           //add dword ptr [mt2], 8;
           add r13, 16;
           add rdi, r12;
           add rsi, r12;
       // end for x := 0 to width2 - 1
       sub rbx, 2;
       jge @@fory2label;

       @@fory2loopend:
       add rbx, 2;

       // test for odd h2
       jz @@NextLine;

       // we have an odd height2 -> special treatment for the last line
       vxorpd ymm0, ymm0, ymm0;   // dest^ := 0

       // for idx := 0 to width1 div 4 do
       mov rax, r14;
       add rax, 32;
       jg @LastLineInnerLoopEnd;

       @@LastLineInnerLoop:
            vmovupd ymm1, [r8 + rax - 32];

            // load block
            vmovupd ymm3, [rdi + rax - 32];

            // multiply and add
            vmulpd ymm3, ymm3, ymm1;

            vaddpd ymm0, ymm0, ymm3;

            // end for idx := 0 to width1 div 2
       add rax, 32;
       jle @@LastLineInnerLoop;

       vextractf128 xmm1, ymm0, 1;
       vhaddpd xmm0, xmm0, xmm1;

       @LastLineInnerLoopEnd:
       sub rax, 32;
       jz @@LastLineInnerLoopEnd2;

       // do the final few elements
       @@LastLineInnerLoop2:
            vmovsd xmm1, [r8 + rax];
            vmovsd xmm3, [rdi + rax];

            // multiply and add
            vmulsd xmm3, xmm3, xmm1;
            vaddsd xmm0, xmm0, xmm3;
       // next element
       add rax, 8;
       jnz @@LastLineInnerLoop2;

       @@LastLineInnerLoopEnd2:

       // final add and compact result
       vhaddpd xmm0, xmm0, xmm0;

       // store back result
       vmovsd [r13], xmm0;

       // ################################################
       // #### next line of mt1
       @@NextLine:
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

   vmovupd xmm4, dXMM4;
   vzeroupper;
{$IFDEF FPC}
end;
{$ENDIF}
end;

// note mt2 is transposed this time -> width1 and width2 must be the same!
procedure AVXMatrixMultAlignedTransposed(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var dXMM4 : Array[0..1] of double;
    iRBX, iRSI, iRDI, iR12, iR13, iR14, iR15 : int64;

{$IFDEF FPC}
begin
{$ENDIF}
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

   // prolog - simulate stack
   mov iRBX, rbx;
   mov iRSI, rsi;
   mov iRDI, rdi;
   mov iR12, r12;
   mov iR13, r13;
   mov iR14, r14;
   mov iR15, r15;

   vmovupd dXMM4, xmm4;

   // height2 helper
   mov r15, height2;

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

   // for y := 0 to height1 - 1:
   mov r11, Height1;
   @@forylabel:
       mov rdi, r9;
       mov rsi, r10;
       mov r13, rcx;

       // for x := 0 to width2 - 1:
       mov rbx, r15;
       sub rbx, 2;
       jl @@fory2loopend;

       @@fory2label:
           vxorpd ymm0, ymm0, ymm0;   // dest^ := 0
           vxorpd ymm2, ymm2, ymm2;   // dest + 1 := 0;

           // for idx := 0 to width1 div 4 do
           mov rax, r14;
           add rax, 32;
           jg @InnerLoopEnd;

           @@InnerLoop:
               vmovapd ymm1, [r8 + rax - 32];

               // multiply 2x2 and add
               vmulpd ymm3, ymm1, [rdi + rax - 32];
               vmulpd ymm4, ymm1, [rsi + rax - 32];

               vaddpd ymm0, ymm0, ymm3;
               vaddpd ymm2, ymm2, ymm4;

               // end for idx := 0 to width1 div 2
           add rax, 32;
           jle @@InnerLoop;

           vextractf128 xmm1, ymm0, 1;
           vextractf128 xmm3, ymm2, 1;

           vhaddpd xmm0, xmm0, xmm1;
           vhaddpd xmm2, xmm2, xmm3;

           @InnerLoopEnd:

           sub rax, 32;
           jz @@InnerLoopEnd2;

           // do the final few elements
           @@InnerLoop2:
               vmovsd xmm1, [r8 + rax];

               // load 2x2 block
               vmovsd xmm3, [rdi + rax];
               vmovsd xmm4, [rsi + rax];

               // multiply 2x2 and add
               vmulsd xmm3, xmm3, xmm1;
               vmulsd xmm4, xmm4, xmm1;

               vaddsd xmm0, xmm0, xmm3;
               vaddsd xmm2, xmm2, xmm4;

           add rax, 8;
           jnz @@InnerLoop2;

           @@InnerLoopEnd2:

           // final add and compact result
           vhaddpd xmm0, xmm0, xmm2;

           // store back result
           vmovapd [r13], xmm0;

           // increment the pointers
           // inc(mt2), inc(dest);
           //add dword ptr [mt2], 8;
           add r13, 16;
           add rdi, r12;
           add rsi, r12;
       // end for x := 0 to width2 - 1
       sub rbx, 2;
       jge @@fory2label;

       @@fory2loopend:
       add rbx, 2;

       // test for odd h2
       jz @@NextLine;

       // we have an odd height2 -> special treatment for the last line
       vxorpd ymm0, ymm0, ymm0;   // dest^ := 0

       // for idx := 0 to width1 div 4 do
       mov rax, r14;
       add rax, 32;
       jg @LastLineInnerLoopEnd;

       @@LastLineInnerLoop:
            vmovapd ymm1, [r8 + rax - 32];

            // multiply and add
            vmulpd ymm3, ymm1, [rdi + rax - 32];

            vaddpd ymm0, ymm0, ymm3;

            // end for idx := 0 to width1 div 2
       add rax, 32;
       jle @@LastLineInnerLoop;

       vextractf128 xmm1, ymm0, 1;
       vhaddpd xmm0, xmm0, xmm1;

       @LastLineInnerLoopEnd:
       sub rax, 32;
       jz @@LastLineInnerLoopEnd2;

       // do the final few elements
       @@LastLineInnerLoop2:
            vmovsd xmm1, [r8 + rax];
            vmovsd xmm3, [rdi + rax];

            // multiply and add
            vmulsd xmm3, xmm3, xmm1;
            vaddsd xmm0, xmm0, xmm3;
       // next element
       add rax, 8;
       jnz @@LastLineInnerLoop2;

       @@LastLineInnerLoopEnd2:

       // final add and compact result
       vhaddpd xmm0, xmm0, xmm0;

       // store back result
       vmovsd [r13], xmm0;

       // ################################################
       // #### next line of mt1
       @@NextLine:
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

   vmovupd xmm4, dXMM4;
   vzeroupper;
{$IFDEF FPC}
end;
{$ENDIF}
end;

{$ENDIF}

end.
