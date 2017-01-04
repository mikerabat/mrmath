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


unit ASMMatrixVectorMultOperationsx64;

// #######################################################
// #### special routines for matrix vector multiplications.
// #######################################################

interface

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF x64}

uses MatrixConst;


procedure ASMMatrixVectMultT(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);
procedure ASMMatrixVectMult(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);

// routines with special input layouts: LineWidthV needs to be sizeof(double)
procedure ASMMatrixVectMultEvenAlignedVAligned(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);
procedure ASMMatrixVectMultEvenUnAlignedVAligned(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);
procedure ASMMatrixVectMultOddUnAlignedVAligned(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);


{$ENDIF}

implementation

{$IFDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$ENDIF}

procedure ASMMatrixVectorMultAlignedEvenW1(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1 : TASMNativeInt);
var iRBX, iRDI, iR12 : NativeInt;
{$IFDEF FPC}
begin
{$ENDIF}
asm
   // prolog - simulate stack
   mov iRBX, rbx;
   mov iRDI, rdi;
   mov iR12, r12;
   {
   .pushnv rbx;
   .pushnv rdi;
   .pushnv r12;
   }

   mov r10, width1;
   shl r10, 3;
   imul r10, -1;

   xorpd xmm3, xmm3;

   // prepare for reverse loop indexing
   mov r11, mt1;
   mov r12, mt2;
   sub r11, r10;
   sub r12, r10;

   mov rdi, LineWidth1;

   // init for y := 0 to height1 - 1:
   mov rbx, height1;
   @@foryloop:

       // init values:
       xorpd xmm0, xmm0;  // dest^ := 0;

       mov rax, r10;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [r11 + rax];
           // prefetch [r12 + rax];

           // addition:
           movapd xmm1, [r11 + rax - 128];
           mulpd xmm1, [r12 + rax - 128];

           addpd xmm0, xmm1;

           movapd xmm1, [r11 + rax - 112];
           mulpd xmm1, [r12 + rax - 112];

           addpd xmm0, xmm1;

           movapd xmm1, [r11 + rax - 96];
           mulpd xmm1, [r12 + rax - 96];

           addpd xmm0, xmm1;

           movapd xmm1, [r11 + rax - 80];
           mulpd xmm1, [r12 + rax - 80];

           addpd xmm0, xmm1;

           movapd xmm1, [r11 + rax - 64];
           mulpd xmm1, [r12 + rax - 64];

           addpd xmm0, xmm1;

           movapd xmm1, [r11 + rax - 48];
           mulpd xmm1, [r12 + rax - 48];

           addpd xmm0, xmm1;

           movapd xmm1, [r11 + rax - 32];
           mulpd xmm1, [r12 + rax - 32];

           addpd xmm0, xmm1;

           movapd xmm1, [r11 + rax - 16];
           mulpd xmm1, [r12 + rax - 16];

           addpd xmm0, xmm1;
       jmp @addforxloop;

       @loopEnd:
       sub rax, 128;

       @@forxloop2:
           movapd xmm1, [r11 + rax];
           mulpd xmm1, [r12 + rax];

           addpd xmm0, xmm1;
       add rax, 16;
       jnz @@forxloop2;

       // write back result (final addition and compactation)
       haddpd xmm0, xmm3;
       movlpd [rcx], xmm0;

       // next results:
       add rcx, rdx;
       add r11, rdi;
   dec rbx;
   jnz @@foryloop;

   // epilog - cleanup stack
   mov rbx, iRBX;
   mov rdi, iRDI;
   mov r12, iR12;
{$IFDEF FPC}
end;
{$ENDIF}
end;

procedure ASMMatrixVectorMultUnAlignedEvenW1(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1 : TASMNativeInt);
var iRBX, iRDI, iR12 : NativeInt;
{$IFDEF FPC}
begin
{$ENDIF}
asm
   // prolog - simulate stack
   mov iRBX, rbx;
   mov iRDI, rdi;
   mov iR12, r12;
   {
   .pushnv rbx;
   .pushnv rdi;
   .pushnv r12;
   }

   mov r10, width1;
   shl r10, 3;
   imul r10, -1;

   xorpd xmm3, xmm3;

   // prepare for reverse loop indexing
   mov r11, mt1;
   mov r12, mt2;
   sub r11, r10;
   sub r12, r10;

   mov rdi, LineWidth1;

   // init for y := 0 to height1 - 1:
   mov rbx, height1;
   @@foryloop:

       // init values:
       xorpd xmm0, xmm0;  // dest^ := 0;

       mov rax, r10;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // addition:
           movupd xmm1, [r11 + rax - 128];
           movupd xmm2, [r12 + rax - 128];
           mulpd xmm1, xmm2;
           addpd xmm0, xmm1;

           movupd xmm1, [r11 + rax - 112];
           movupd xmm2, [r12 + rax - 112];
           mulpd xmm1, xmm2;
           addpd xmm0, xmm1;

           movupd xmm1, [r11 + rax - 96];
           movupd xmm2, [r12 + rax - 96];
           mulpd xmm1, xmm2;
           addpd xmm0, xmm1;

           movupd xmm1, [r11 + rax - 80];
           movupd xmm2, [r12 + rax - 80];
           mulpd xmm1, xmm2;
           addpd xmm0, xmm1;

           movupd xmm1, [r11 + rax - 64];
           movupd xmm2, [r12 + rax - 64];
           mulpd xmm1, xmm2;
           addpd xmm0, xmm1;

           movupd xmm1, [r11 + rax - 48];
           movupd xmm2, [r12 + rax - 48];
           mulpd xmm1, xmm2;
           addpd xmm0, xmm1;

           movupd xmm1, [r11 + rax - 32];
           movupd xmm2, [r12 + rax - 32];
           mulpd xmm1, xmm2;
           addpd xmm0, xmm1;

           movupd xmm1, [r11 + rax - 16];
           movupd xmm2, [r12 + rax - 16];
           mulpd xmm1, xmm2;
           addpd xmm0, xmm1;
       jmp @addforxloop;

       @loopEnd:
       sub rax, 128;

       @@forxloop2:
           movupd xmm1, [r11 + rax];
           movupd xmm2, [r12 + rax];
           mulpd xmm1, xmm2;
           addpd xmm0, xmm1;
       add rax, 16;
       jnz @@forxloop2;

       // write back result (final addition and compactation)
       haddpd xmm0, xmm3;
       movlpd [rcx], xmm0;

       // next results:
       add rcx, rdx;
       add r11, rdi;
   dec rbx;
   jnz @@foryloop;

   // epilog - cleanup stack
   mov rbx, iRBX;
   mov rdi, iRDI;
   mov r12, iR12;
{$IFDEF FPC}
end;
{$ENDIF}
end;


procedure ASMMatrixVectorMultAlignedOddW1(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1 : TASMNativeInt);
var iRBX, iRDI, iR12 : NativeInt;
{$IFDEF FPC}
begin
  {$ENDIF}
asm
   // prolog - simulate stack
   mov iRBX, rbx;
   mov iRDI, rdi;
   mov iR12, r12;
   {
   .pushnv rbx;
   .pushnv rdi;
   .pushnv r12;
   }

   mov r10, width1;
   dec r10;
   shl r10, 3;
   imul r10, -1;

   xorpd xmm3, xmm3;

   // prepare for reverse loop indexing
   mov r11, mt1;
   mov r12, mt2;
   sub r11, r10;
   sub r12, r10;

   mov rdi, LineWidth1;

   // init for y := 0 to height1 - 1:
   mov rbx, height1;
   @@foryloop:

       // init values:
       xorpd xmm0, xmm0;  // dest^ := 0;

       mov rax, r10;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [r11 + rax];
           // prefetch [r12 + rax];

           // addition:
           movapd xmm1, [r11 + rax - 128];
           mulpd xmm1, [r12 + rax - 128];

           addpd xmm0, xmm1;

           movapd xmm1, [r11 + rax - 112];
           mulpd xmm1, [r12 + rax - 112];

           addpd xmm0, xmm1;

           movapd xmm1, [r11 + rax - 96];
           mulpd xmm1, [r12 + rax - 96];

           addpd xmm0, xmm1;

           movapd xmm1, [r11 + rax - 80];
           mulpd xmm1, [r12 + rax - 80];

           addpd xmm0, xmm1;

           movapd xmm1, [r11 + rax - 64];
           mulpd xmm1, [r12 + rax - 64];

           addpd xmm0, xmm1;

           movapd xmm1, [r11 + rax - 48];
           mulpd xmm1, [r12 + rax - 48];

           addpd xmm0, xmm1;

           movapd xmm1, [r11 + rax - 32];
           mulpd xmm1, [r12 + rax - 32];

           addpd xmm0, xmm1;

           movapd xmm1, [r11 + rax - 16];
           mulpd xmm1, [r12 + rax - 16];

           addpd xmm0, xmm1;
       jmp @addforxloop;

       @loopEnd:
       sub rax, 128;

       @@forxloop2:
           movapd xmm1, [r11 + rax];
           mulpd xmm1, [r12 + rax];

           addpd xmm0, xmm1;
       add rax, 16;
       jnz @@forxloop2;

       // special treatment for the last value:
       movlpd xmm1, [r11];
       movlpd xmm2, [r12];

       mulsd xmm1, xmm2;
       addsd xmm0, xmm1;

       // write back result (final addition and compactation)
       haddpd xmm0, xmm3;
       movlpd [rcx], xmm0;

       // next results:
       add rcx, rdx;
       add r11, rdi;
   dec rbx;
   jnz @@foryloop;

   // epilog - cleanup stack
   mov rbx, iRBX;
   mov rdi, iRDI;
   mov r12, iR12;
{$IFDEF FPC}
end;
{$ENDIF}
end;

procedure ASMMatrixVectorMultUnAlignedOddW1(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1 : TASMNativeInt);
var iRBX, iRDI, iR12 : NativeInt;
{$IFDEF FPC}
begin
  {$ENDIF}
asm
   // prolog - simulate stack
   mov iRBX, rbx;
   mov iRDI, rdi;
   mov iR12, r12;
   {
   .pushnv rbx;
   .pushnv rdi;
   .pushnv r12;
   }

   mov r10, width1;
   dec r10;
   shl r10, 3;
   imul r10, -1;

   xorpd xmm3, xmm3;

   // prepare for reverse loop indexing
   mov r11, mt1;
   mov r12, mt2;
   sub r11, r10;
   sub r12, r10;

   mov rdi, LineWidth1;

   // init for y := 0 to height1 - 1:
   mov rbx, height1;
   @@foryloop:

       // init values:
       xorpd xmm0, xmm0;  // dest^ := 0;

       mov rax, r10;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // addition:
           movupd xmm1, [r11 + rax - 128];
           movupd xmm2, [r12 + rax - 128];
           mulpd xmm1, xmm2;
           addpd xmm0, xmm1;

           movupd xmm1, [r11 + rax - 112];
           movupd xmm2, [r12 + rax - 112];
           mulpd xmm1, xmm2;
           addpd xmm0, xmm1;

           movupd xmm1, [r11 + rax - 96];
           movupd xmm2, [r12 + rax - 96];
           mulpd xmm1, xmm2;
           addpd xmm0, xmm1;

           movupd xmm1, [r11 + rax - 80];
           movupd xmm2, [r12 + rax - 80];
           mulpd xmm1, xmm2;
           addpd xmm0, xmm1;

           movupd xmm1, [r11 + rax - 64];
           movupd xmm2, [r12 + rax - 64];
           mulpd xmm1, xmm2;
           addpd xmm0, xmm1;

           movupd xmm1, [r11 + rax - 48];
           movupd xmm2, [r12 + rax - 48];
           mulpd xmm1, xmm2;
           addpd xmm0, xmm1;

           movupd xmm1, [r11 + rax - 32];
           movupd xmm2, [r12 + rax - 32];
           mulpd xmm1, xmm2;
           addpd xmm0, xmm1;

           movupd xmm1, [r11 + rax - 16];
           movupd xmm2, [r12 + rax - 16];
           mulpd xmm1, xmm2;
           addpd xmm0, xmm1;
       jmp @addforxloop;

       @loopEnd:
       sub rax, 128;

       @@forxloop2:
           movupd xmm1, [r11 + rax];
           movupd xmm2, [r12 + rax];
           mulpd xmm1, xmm2;
           addpd xmm0, xmm1;
       add rax, 16;
       jnz @@forxloop2;

       // special treatment for the last value:
       movlpd xmm1, [r11];
       movlpd xmm2, [r12];

       mulsd xmm1, xmm2;
       addsd xmm0, xmm1;

       // write back result (final addition and compactation)
       haddpd xmm0, xmm3;
       movlpd [rcx], xmm0;

       // next results:
       add rcx, rdx;
       add r11, rdi;
   dec rbx;
   jnz @@foryloop;

   // epilog - cleanup stack
   mov rbx, iRBX;
   mov rdi, iRDI;
   mov r12, iR12;
{$IFDEF FPC}
end;
{$ENDIF}
end;

procedure ASMMatrixVectMultAlignedOddW(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);
var dXMM6, dXMM7 : Array[0..1] of double;
    iRBX, iRSI, iRDI, iR12, iR13, iR14 : NativeInt;
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

   movupd dXMM6, xmm6;
   movupd dXMM7, xmm7;

   // global function inits (for haddpd)
   xorpd xmm7, xmm7;

   // for the final multiplication  (alpha, beta handling)
   movhpd xmm6, beta;
   movlpd xmm6, alpha;

   // prepare for loop
   mov rsi, LineWidthMT;
   mov rdi, LineWidthV;

   // iter := -(width - 1)*sizeof(double);
   mov r13, width;
   sub r13, 1;
   imul r13, -8;

   sub r8, r13;     // mt1 - iter

   mov r14, height;

   // init for y := 0 to height - 1:
   @@foryloop:

       // init values:
       xorpd xmm3, xmm3;  // res := 0;
       mov rax, r8;       // rax = first matrix element
       mov rbx, r9;       // rbx = first vector element

       mov r10, r13;      // iter
       @@forxloop:
           movlpd xmm2, [rbx]
           movhpd xmm2, [rbx + rdi]

           mulpd xmm2, [rax + r10];
           addpd xmm3, xmm2;

           add rbx, rdi;
           add rbx, rdi;

           add r10, 16;
       jnz @@forxloop;

       // special treatment for the last value:
       movlpd xmm1, [rax];
       movlpd xmm2, [rbx];

       mulsd xmm1, xmm2;
       addsd xmm3, xmm1;

       // result building
       // write back result (final addition and compactation)
       haddpd xmm3, xmm7;

       // result building
       // write back result (final addition and compactation)

       // calculate dest = beta*dest + alpha*xmm0
       movhpd xmm3, [rcx];

       mulpd xmm3, xmm6;
       haddpd xmm3, xmm7;

       // store destination
       movlpd [rcx], xmm3;

       // next results:
       add rcx, rdx;       // next dest element
       add r8, rsi;        // next mt1 row
   dec r14;
   jnz @@foryloop;

   // epilog pop "stack"
   mov rbx, iRBX;
   mov rsi, iRSI;
   mov rdi, iRDI;
   mov r12, iR12;
   mov r13, iR13;
   mov r14, iR14;

   movupd xmm6, dXMM6;
   movupd xmm7, dXMM7;
end;
{$IFDEF FPC}
end;
{$ENDIF}


// note: RCX = dest, RDX = destLineWidth, R8 = mt1, R9 = v
// note we need at lest width = 3 for this function!
procedure ASMMatrixVectMultUnalignedOddW(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);
var dXMM6, dXMM7 : Array[0..1] of double;
    iRBX, iRSI, iRDI, iR12, iR13, iR14 : NativeInt;
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

   movupd dXMM6, xmm6;
   movupd dXMM7, xmm7;

   // global function inits (for haddpd)
   xorpd xmm7, xmm7;

   // for the final multiplication  (alpha, beta handling)
   movhpd xmm6, beta;
   movlpd xmm6, alpha;

   // prepare for loop
   mov rsi, LineWidthMT;
   mov rdi, LineWidthV;

   // iter := -(width - 1)*sizeof(double);
   mov r13, width;
   sub r13, 1;
   imul r13, -8;

   sub r8, r13;     // mt1 - iter

   mov r14, height;

   // init for y := 0 to height - 1:
   @@foryloop:

       // init values:
       xorpd xmm3, xmm3;  // res := 0;
       mov rax, r8;       // rax = first matrix element
       mov rbx, r9;       // rbx = first vector element

       mov r10, r13;      // iter
       @@forxloop:
           movupd xmm1, [rax + r10];
           movlpd xmm2, [rbx]
           movhpd xmm2, [rbx + rdi]

           mulpd xmm1, xmm2;
           addpd xmm3, xmm1;

           add rbx, rdi;
           add rbx, rdi;

           add r10, 16;
       jnz @@forxloop;

       // special treatment for the last value:
       movlpd xmm1, [rax];
       movlpd xmm2, [rbx];

       mulsd xmm1, xmm2;
       addsd xmm3, xmm1;

       // result building
       // write back result (final addition and compactation)
       haddpd xmm3, xmm7;

       // result building
       // write back result (final addition and compactation)

       // calculate dest = beta*dest + alpha*xmm0
       movhpd xmm3, [rcx];

       mulpd xmm3, xmm6;
       haddpd xmm3, xmm7;

       // store destination
       movlpd [rcx], xmm3;

       // next results:
       add rcx, rdx;       // next dest element
       add r8, rsi;        // next mt1 row
   dec r14;
   jnz @@foryloop;

   // epilog pop "stack"
   mov rbx, iRBX;
   mov rsi, iRSI;
   mov rdi, iRDI;
   mov r12, iR12;
   mov r13, iR13;
   mov r14, iR14;

   movupd xmm6, dXMM6;
   movupd xmm7, dXMM7;
end;
{$IFDEF FPC}
end;
{$ENDIF}


procedure ASMMatrixVectMultUnalignedEvenW(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);
var dXMM6, dXMM7 : Array[0..1] of double;
    iRBX, iRSI, iRDI, iR12, iR13, iR14 : NativeInt;
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

   movupd dXMM6, xmm6;
   movupd dXMM7, xmm7;

   // global function inits (for haddpd)
   xorpd xmm7, xmm7;

   // for the final multiplication  (alpha, beta handling)
   movhpd xmm6, beta;
   movlpd xmm6, alpha;

   // prepare for loop
   mov rsi, LineWidthMT;
   mov rdi, LineWidthV;

   // iter := -width*sizeof(double);
   mov r13, width;
   imul r13, -8;

   sub r8, r13;     // mt1 - iter

   mov r14, height;

   // init for y := 0 to height - 1:
   @@foryloop:

       // init values:
       xorpd xmm3, xmm3;  // res := 0;
       mov rax, r8;       // rax = first matrix element
       mov rbx, r9;       // rbx = first vector element

       mov r10, r13;      // iter
       @@forxloop:
           movupd xmm1, [rax + r10];
           movlpd xmm2, [rbx]
           movhpd xmm2, [rbx + rdi]

           mulpd xmm1, xmm2;
           addpd xmm3, xmm1;

           add rbx, rdi;
           add rbx, rdi;

           add r10, 16;
       jnz @@forxloop;

       // result building
       // write back result (final addition and compactation)
       haddpd xmm3, xmm7;

       // result building
       // write back result (final addition and compactation)

       // calculate dest = beta*dest + alpha*xmm0
       movhpd xmm3, [rcx];

       mulpd xmm3, xmm6;
       haddpd xmm3, xmm7;

       // store destination
       movlpd [rcx], xmm3;

       // next results:
       add rcx, rdx;       // next dest element
       add r8, rsi;        // next mt1 row
   dec r14;
   jnz @@foryloop;

   // epilog pop "stack"
   mov rbx, iRBX;
   mov rsi, iRSI;
   mov rdi, iRDI;
   mov r12, iR12;
   mov r13, iR13;
   mov r14, iR14;

   movupd xmm6, dXMM6;
   movupd xmm7, dXMM7;
end;
{$IFDEF FPC}
end;
{$ENDIF}

procedure ASMMatrixVectMultAlignedEvenW(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);
var dXMM6, dXMM7 : Array[0..1] of double;
    iRBX, iRSI, iRDI, iR12, iR13, iR14 : NativeInt;
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

   movupd dXMM6, xmm6;
   movupd dXMM7, xmm7;

   // global function inits (for haddpd)
   xorpd xmm7, xmm7;

   // for the final multiplication  (alpha, beta handling)
   movhpd xmm6, beta;
   movlpd xmm6, alpha;

   // prepare for loop
   mov rsi, LineWidthMT;
   mov rdi, LineWidthV;

   // iter := -width*sizeof(double);
   mov r13, width;
   imul r13, -8;

   sub r8, r13;     // mt1 - iter

   mov r14, height;

   // init for y := 0 to height - 1:
   @@foryloop:

       // init values:
       xorpd xmm3, xmm3;  // res := 0;
       mov rax, r8;       // rax = first matrix element
       mov rbx, r9;       // rbx = first vector element

       mov r10, r13;      // iter
       @@forxloop:
           movlpd xmm2, [rbx];
           movhpd xmm2, [rbx + rdi];

           mulpd xmm2, [rax + r10];
           addpd xmm3, xmm2;

           add rbx, rdi;
           add rbx, rdi;

           add r10, 16;
       jnz @@forxloop;

       // result building
       // write back result (final addition and compactation)
       haddpd xmm3, xmm7;

       // result building
       // write back result (final addition and compactation)

       // calculate dest = beta*dest + alpha*xmm0
       movhpd xmm3, [rcx];

       mulpd xmm3, xmm6;
       haddpd xmm3, xmm7;

       // store destination
       movsd [rcx], xmm3;

       // next results:
       add rcx, rdx;       // next dest element
       add r8, rsi;        // next mt1 row
   dec r14;
   jnz @@foryloop;

   // epilog pop "stack"
   mov rbx, iRBX;
   mov rsi, iRSI;
   mov rdi, iRDI;
   mov r12, iR12;
   mov r13, iR13;
   mov r14, iR14;

   movupd xmm6, dXMM6;
   movupd xmm7, dXMM7;
end;
{$IFDEF FPC}
end;
{$ENDIF}



procedure ASMMatrixVectMult(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);
begin
     // todo..
end;

// routines with special input layouts: LineWidthV needs to be sizeof(double)
procedure ASMMatrixVectMultEvenAlignedVAligned(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);
begin
     // todo..
end;

procedure ASMMatrixVectMultEvenUnAlignedVAligned(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);
begin
     // todo..
end;

procedure ASMMatrixVectMultOddUnAlignedVAligned(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);
begin
     // todo..
end;


// this function is not that well suited for use of simd instructions...
// so only this version exists

// note: RCX = dest, RDX = destLineWidth, R8 = mt1, R9 = v
procedure ASMMatrixVectMultT(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);
var dXMM6, dXMM7 : Array[0..1] of double;
    iRBX, iRSI, iRDI, iR12, iR13, iR14 : NativeInt;
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

   movupd dXMM6, xmm6;
   movupd dXMM7, xmm7;

   // global function inits (for haddpd and alpha, beta handling)
   xorpd xmm7, xmm7;

   // for the final multiplication
   movhpd xmm6, beta;
   movlpd xmm6, alpha;

   // prepare for loop
   mov rsi, LineWidthMT;
   mov rdi, LineWidthV;
   mov r13, height;
   mov r14, width;

   // init for x := 0 to width - 1:
   @@forxloop:

       // init values:
       xorpd xmm3, xmm3;  // res := 0;
       mov rax, r8;       // rax = first matrix element
       mov rbx, r9;       // rbx = first vector element

       mov r10, r13;
       @@foryloop:
           movsd xmm1, [rax];
           movsd xmm2, [rbx];

           mulsd xmm1, xmm2;
           addsd xmm3, xmm1;

           add rax, rsi;
           add rbx, rdi;

       dec r10;
       jnz @@foryloop;

       // result building
       // write back result (final addition and compactation)

       // calculate dest = beta*dest + alpha*xmm0
       movhpd xmm3, [rcx];

       mulpd xmm3, xmm6;
       haddpd xmm3, xmm7;

       movlpd [rcx], xmm3;

       // next results:
       add rcx, destLineWidth;   // next dest element
       add r8, 8;                // next mt1 element
   dec r14;
   jnz @@forxloop;

   // epilog pop "stack"
   mov rbx, iRBX;
   mov rsi, iRSI;
   mov rdi, iRDI;
   mov r12, iR12;
   mov r13, iR13;
   mov r14, iR14;

   movupd xmm6, dXMM6;
   movupd xmm7, dXMM7;
end;
{$IFDEF FPC}
end;
{$ENDIF}


{$ENDIF}

end.
