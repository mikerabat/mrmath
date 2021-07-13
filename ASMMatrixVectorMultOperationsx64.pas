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


procedure ASMMatrixVectMultT(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; {$ifdef UNIX}unixLineWidthMT{$ELSE}LineWidthMT{$endif}, {$ifdef UNIX}unixLineWidthV{$ELSE}LineWidthV{$endif} : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double); {$IFDEF FPC}assembler;{$ENDIF}
procedure ASMMatrixVectMult(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; {$ifdef UNIX}unixLineWidthMT{$ELSE}LineWidthMT{$endif}, {$ifdef UNIX}unixLineWidthV{$ELSE}LineWidthV{$endif} : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double); {$IFDEF FPC}assembler;{$ENDIF}

// routines with special input layouts: LineWidthV needs to be sizeof(double)
procedure ASMMatrixVectMultEvenAlignedVAligned(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; {$ifdef UNIX}unixLineWidthMT{$ELSE}LineWidthMT{$endif}, {$ifdef UNIX}unixLineWidthV{$ELSE}LineWidthV{$endif} : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double); {$IFDEF FPC}assembler;{$ENDIF}
procedure ASMMatrixVectMultEvenUnAlignedVAligned(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble;{$ifdef UNIX}unixLineWidthMT{$ELSE}LineWidthMT{$endif}, {$ifdef UNIX}unixLineWidthV{$ELSE}LineWidthV{$endif} : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double); {$IFDEF FPC}assembler;{$ENDIF}
procedure ASMMatrixVectMultOddUnAlignedVAligned(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; {$ifdef UNIX}unixLineWidthMT{$ELSE}LineWidthMT{$endif}, {$ifdef UNIX}unixLineWidthV{$ELSE}LineWidthV{$endif} : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double); {$IFDEF FPC}assembler;{$ENDIF}

// destlinewidth needs to be sizeof(double)!
// no speed gain agains amsmatrixVectMultT
procedure ASMMatrixVectMultTDestVec(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; {$ifdef UNIX}unixLineWidthMT{$ELSE}LineWidthMT{$endif}, {$ifdef UNIX}unixLineWidthV{$ELSE}LineWidthV{$endif} : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double); {$IFDEF FPC}assembler;{$ENDIF}

// rank1 update: A = A + alpha*X*Y' where x and y are vectors. It's assumed that y is sequential
procedure ASMRank1UpdateSeq(A : PDouble; const LineWidthA : TASMNativeInt; width, height : TASMNativeInt;
  {$ifdef UNIX}unixX{$ELSE}X{$endif}, {$ifdef UNIX}unixY{$ELSE}Y{$ENDIF} : PDouble; incX, incY : TASMNativeInt; alpha : double);  {$IFDEF FPC}assembler;{$ENDIF}
procedure ASMRank1UpdateSeqAligned(A : PDouble; const LineWidthA : TASMNativeInt; width, height : TASMNativeInt;
  {$ifdef UNIX}unixX{$ELSE}X{$endif}, {$ifdef UNIX}unixY{$ELSE}Y{$ENDIF} : PDouble; incX, incY : TASMNativeInt; alpha : double);  {$IFDEF FPC}assembler;{$ENDIF}


procedure ASMRank1UpdateNonSeq(A : PDouble; const LineWidthA : TASMNativeInt; width, height : TASMNativeInt;
  {$ifdef UNIX}unixX{$ELSE}X{$endif}, {$ifdef UNIX}unixY{$ELSE}Y{$ENDIF} : PDouble; incX, incY : TASMNativeInt; alpha : double);  {$IFDEF FPC}assembler;{$ENDIF}

{$ENDIF}

implementation

{$IFDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$S-} {$ENDIF}

procedure ASMMatrixVectMult(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; {$ifdef UNIX}unixLineWidthMT{$ELSE}LineWidthMT{$endif}, {$ifdef UNIX}unixLineWidthV{$ELSE}LineWidthV{$endif} : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double); {$IFDEF FPC}assembler;{$ENDIF}
// note: RCX = dest, RDX = destLineWidth, R8 = mt1, R9 = v
var dXMM4, dXMM5, dXMM6, dXMM7 : Array[0..1] of double;
    iRBX, iRSI, iRDI, iR12, iR13, iR14 : TASMNativeInt;
    {$ifdef UNIX}
    LineWidthMT, LineWidthV : TASMNativeInt;
    {$endif}
asm
   {$IFDEF UNIX}
   // Linux uses a diffrent ABI -> copy over the registers so they meet with winABI
   // (note that the 5th and 6th parameter are are on the stack)
   // The parameters are passed in the following order:
   // RDI, RSI, RDX, RCX -> mov to RCX, RDX, R8, R9
   mov LineWidthMT, r8;
   mov LineWidthV, r9;
   mov r8, rdx;
   mov r9, rcx;
   mov rcx, rdi;
   mov rdx, rsi;
   {$ENDIF}

   // prolog - simulate stack
   movupd dXMM4, xmm4;
   movupd dXMM5, xmm5;
   movupd dXMM6, xmm6;
   movupd dXMM7, xmm7;

   mov iRBX, rbx;
   mov iRSI, rsi;
   mov iRDI, rdi;
   mov iR12, r12;
   mov iR13, r13;
   mov iR14, r14;


   // for the final multiplication
   {$IFDEF UNIX}
   movsd xmm6, alpha;
   movlhps xmm6, beta;
   {$ELSE}
   movhpd xmm6, beta;
   movlpd xmm6, alpha;
   {$ENDIF}

   // prepare for loop
   mov rsi, LineWidthMT;
   mov rdi, LineWidthV;
   mov r13, height;
   mov r14, width;

   sub r13, 4;
   js @@foryloopend;

   // init for x := 0 to width - 1:
   @@foryloop:

       // init values:
       xorpd xmm0, xmm0;
       xorpd xmm1, xmm1;
       xorpd xmm2, xmm2;
       xorpd xmm3, xmm3;  // res := 0;
       mov rax, r8;       // rax = first matrix element
       mov rbx, r9;       // rbx = first vector element

       mov r10, r14;      // r10 = width
       @@forxloop:
           movsd xmm4, [rbx];

           movsd xmm5, [rax];
           mulsd xmm5, xmm4;
           addsd xmm0, xmm5;

           movsd xmm5, [rax + rsi];
           mulsd xmm5, xmm4;
           addsd xmm1, xmm5;

           movsd xmm5, [rax + 2*rsi];
           mulsd xmm5, xmm4;
           addsd xmm2, xmm5;

           add rax, rsi;
           movsd xmm5, [rax + 2*rsi];
           mulsd xmm5, xmm4;
           addsd xmm3, xmm5;
           sub rax, rsi;

           add rax, 8;
           add rbx, rdi;

       dec r10;
       jnz @@forxloop;

       // result building
       // write back result (final addition and compactation)

       // calculate dest = beta*dest + alpha*xmm0
       movhpd xmm0, [rcx];
       mulpd xmm0, xmm6;
       haddpd xmm0, xmm0;
       movsd [rcx], xmm0;
       add rcx, rdx;
       add r8, rsi;              // next mt1 element

       movhpd xmm1, [rcx];
       mulpd xmm1, xmm6;
       haddpd xmm1, xmm1;
       movsd [rcx], xmm1;
       add rcx, rdx;
       add r8, rsi;              // next mt1 element

       movhpd xmm2, [rcx];
       mulpd xmm2, xmm6;
       haddpd xmm2, xmm2;
       movsd [rcx], xmm2;
       add rcx, rdx;
       add r8, rsi;              // next mt1 element

       movhpd xmm3, [rcx];
       mulpd xmm3, xmm6;
       haddpd xmm3, xmm3;
       movsd [rcx], xmm3;
       add rcx, rdx;             // next dest element
       add r8, rsi;              // next mt1 element

       // next rseult
   sub r13, 4;
   jns @@foryloop;

   // ###########################################
   // #### Remaining rows (max 4):
   @@foryloopend:
   add r13, 4;
   jz @@vecend;

   @@foryshortloop:
       // init values:
       xorpd xmm0, xmm0;
       mov rax, r8;       // rax = first matrix element
       mov rbx, r9;       // rbx = first vector element

       mov r10, r14;      // r10 = width
       @@forxshortloop:
           movsd xmm4, [rbx];

           movsd xmm5, [rax];
           mulsd xmm5, xmm4;
           addsd xmm0, xmm5;

           add rax, 8;
           add rbx, rdi;
       dec r10;
       jnz @@forxshortloop;

       // build result

       // calculate dest = beta*dest + alpha*xmm0
       movhpd xmm0, [rcx];
       mulpd xmm0, xmm6;
       haddpd xmm0, xmm0;
       movsd [rcx], xmm0;
       add rcx, rdx;
       add r8, rsi;

   sub r13, 1;
   jnz @@foryshortloop;

   @@vecend:

   // epilog pop "stack"
   mov rbx, iRBX;
   mov rsi, iRSI;
   mov rdi, iRDI;
   mov r12, iR12;
   mov r13, iR13;
   mov r14, iR14;

   movupd xmm4, dXMM4;
   movupd xmm5, dXMM5;
   movupd xmm6, dXMM6;
   movupd xmm7, dXMM7;
end;


// routines with special input layouts: LineWidthV needs to be sizeof(double)
procedure ASMMatrixVectMultEvenAlignedVAligned(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; {$ifdef UNIX}unixLineWidthMT{$ELSE}LineWidthMT{$endif}, {$ifdef UNIX}unixLineWidthV{$ELSE}LineWidthV{$endif} : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double); {$IFDEF FPC}assembler;{$ENDIF}
// note: RCX = dest, RDX = destLineWidth, R8 = mt1, R9 = v
var dXMM4, dXMM5, dXMM6 : Array[0..1] of double;
    iRBX, iRSI, iRDI, iR12, iR13, iR14 : TASMNativeInt;
    {$ifdef UNIX}
    LineWidthMT, LineWidthV : TASMNativeInt;
    {$endif}
asm
   {$IFDEF UNIX}
   // Linux uses a diffrent ABI -> copy over the registers so they meet with winABI
   // (note that the 5th and 6th parameter are are on the stack)
   // The parameters are passed in the following order:
   // RDI, RSI, RDX, RCX -> mov to RCX, RDX, R8, R9
   mov LineWidthMT, r8;
   mov LineWidthV, r9;
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

   movupd dXMM4, xmm4;
   movupd dXMM5, xmm5;
   movupd dXMM6, xmm6;

   // for the final multiplication
   {$IFDEF UNIX}
   movsd xmm6, alpha;
   movlhps xmm6, beta;
   {$ELSE}
   movhpd xmm6, beta;
   movlpd xmm6, alpha;
   {$ENDIF}

   // prepare for loop
   mov rsi, LineWidthMT;
   mov rdi, LineWidthV;
   mov r13, height;
   mov r14, width;

   sub r13, 4;
   js @@foryloopend;

   // init for x := 0 to width - 1:
   @@foryloop:

       // init values:
       xorpd xmm0, xmm0;
       xorpd xmm1, xmm1;
       xorpd xmm2, xmm2;
       xorpd xmm3, xmm3;  // res := 0;
       mov rax, r8;       // rax = first matrix element
       mov rbx, r9;       // rbx = first vector element

       mov r10, r14;      // r10 = width
       @@forxloop:
           movapd xmm4, [rbx];

           movapd xmm5, [rax];
           mulpd xmm5, xmm4;
           addpd xmm0, xmm5;

           movapd xmm5, [rax + rsi];
           mulpd xmm5, xmm4;
           addpd xmm1, xmm5;

           movapd xmm5, [rax + 2*rsi];
           mulpd xmm5, xmm4;
           addpd xmm2, xmm5;

           add rax, rsi;
           movapd xmm5, [rax + 2*rsi];
           mulpd xmm5, xmm4;
           addpd xmm3, xmm5;
           sub rax, rsi;

           add rax, 16;
           add rbx, 16;

       sub r10, 2;
       jnz @@forxloop;

       // result building
       // write back result (final addition and compactation)

       // calculate dest = beta*dest + alpha*xmm0
       movsd xmm5, [rcx];              // first element
       haddpd xmm0, xmm5;              // calculate xmm0_1 + xmm0_2 (store low) xmm5_1 + xmm5_2 (store high)
       mulpd xmm0, xmm6;               // beta * dest + alpha*xmm0
       haddpd xmm0, xmm0;              // final add
       movsd [rcx], xmm0;              // store back
       add rcx, rdx;
       add r8, rsi;

       movsd xmm5, [rcx];
       haddpd xmm1, xmm5;
       mulpd xmm1, xmm6;
       haddpd xmm1, xmm1;
       movsd [rcx], xmm1;
       add rcx, rdx;
       add r8, rsi;

       movsd xmm5, [rcx];
       haddpd xmm2, xmm5;
       mulpd xmm2, xmm6;
       haddpd xmm2, xmm2;
       movsd [rcx], xmm2;
       add rcx, rdx;
       add r8, rsi;

       movsd xmm5, [rcx];
       haddpd xmm3, xmm5;
       mulpd xmm3, xmm6;
       haddpd xmm3, xmm3;
       movsd [rcx], xmm3;
       add rcx, rdx;
       add r8, rsi;

       // next rseult
   sub r13, 4;
   jns @@foryloop;

   // ###########################################
   // #### Remaining rows (max 4):
   @@foryloopend:
   add r13, 4;
   jz @@vecend;

   @@foryshortloop:
       // init values:
       xorpd xmm0, xmm0;
       mov rax, r8;       // rax = first matrix element
       mov rbx, r9;       // rbx = first vector element

       mov r10, r14;      // r10 = width
       @@forxshortloop:
           movapd xmm4, [rbx];
           movapd xmm5, [rax];
           mulpd xmm5, xmm4;
           addpd xmm0, xmm5;

           add rax, 16;
           add rbx, 16;
       sub r10, 2;
       jnz @@forxshortloop;

       // build result

       // calculate dest = beta*dest + alpha*xmm0
       movsd xmm5, [rcx];
       haddpd xmm0, xmm5;
       mulpd xmm0, xmm6;
       haddpd xmm0, xmm0;
       movsd [rcx], xmm0;

       add rcx, rdx;
       add r8, rsi;

   dec r13;
   jnz @@foryshortloop;

   @@vecend:

   // epilog pop "stack"
   mov rbx, iRBX;
   mov rsi, iRSI;
   mov rdi, iRDI;
   mov r12, iR12;
   mov r13, iR13;
   mov r14, iR14;

   movupd xmm4, dXMM4;
   movupd xmm5, dXMM5;
   movupd xmm6, dXMM6;
end;

procedure ASMMatrixVectMultEvenUnAlignedVAligned(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; {$ifdef UNIX}unixLineWidthMT{$ELSE}LineWidthMT{$endif}, {$ifdef UNIX}unixLineWidthV{$ELSE}LineWidthV{$endif} : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double); {$IFDEF FPC}assembler;{$ENDIF}
// note: RCX = dest, RDX = destLineWidth, R8 = mt1, R9 = v
var dXMM4, dXMM5, dXMM6 : Array[0..1] of double;
    iRBX, iRSI, iRDI, iR12, iR13, iR14 : TASMNativeInt;
    {$ifdef UNIX}
    LineWidthMT, LineWidthV : TASMNativeInt;
    {$endif}
asm
   {$IFDEF UNIX}
   // Linux uses a diffrent ABI -> copy over the registers so they meet with winABI
   // (note that the 5th and 6th parameter are are on the stack)
   // The parameters are passed in the following order:
   // RDI, RSI, RDX, RCX -> mov to RCX, RDX, R8, R9
   mov LineWidthMT, r8;
   mov LineWidthV, r9;
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

   movupd dXMM4, xmm4;
   movupd dXMM5, xmm5;
   movupd dXMM6, xmm6;

   // for the final multiplication
   {$IFDEF UNIX}
   movsd xmm6, alpha;
   movlhps xmm6, beta;
   {$ELSE}
   movhpd xmm6, beta;
   movlpd xmm6, alpha;
   {$ENDIF}

   // prepare for loop
   mov rsi, LineWidthMT;
   mov rdi, LineWidthV;
   mov r13, height;
   mov r14, width;

   sub r13, 4;
   js @@foryloopend;

   // init for x := 0 to width - 1:
   @@foryloop:

       // init values:
       xorpd xmm0, xmm0;
       xorpd xmm1, xmm1;
       xorpd xmm2, xmm2;
       xorpd xmm3, xmm3;  // res := 0;
       mov rax, r8;       // rax = first matrix element
       mov rbx, r9;       // rbx = first vector element

       mov r10, r14;      // r10 = width
       @@forxloop:
           movupd xmm4, [rbx];

           movupd xmm5, [rax];
           mulpd xmm5, xmm4;
           addpd xmm0, xmm5;

           movupd xmm5, [rax + rsi];
           mulpd xmm5, xmm4;
           addpd xmm1, xmm5;

           movupd xmm5, [rax + 2*rsi];
           mulpd xmm5, xmm4;
           addpd xmm2, xmm5;

           add rax, rsi;
           movupd xmm5, [rax + 2*rsi];
           mulpd xmm5, xmm4;
           addpd xmm3, xmm5;
           sub rax, rsi;

           add rax, 16;
           add rbx, 16;

       sub r10, 2;
       jnz @@forxloop;

       // result building
       // write back result (final addition and compactation)

       // calculate dest = beta*dest + alpha*xmm0
       movsd xmm5, [rcx];              // first element
       haddpd xmm0, xmm5;              // calculate xmm0_1 + xmm0_2 (store low) xmm5_1 + xmm5_2 (store high)
       mulpd xmm0, xmm6;               // beta * dest + alpha*xmm0
       haddpd xmm0, xmm0;              // final add
       movsd [rcx], xmm0;              // store back
       add rcx, rdx;
       add r8, rsi;

       movsd xmm5, [rcx];
       haddpd xmm1, xmm5;
       mulpd xmm1, xmm6;
       haddpd xmm1, xmm1;
       movsd [rcx], xmm1;
       add rcx, rdx;
       add r8, rsi;

       movsd xmm5, [rcx];
       haddpd xmm2, xmm5;
       mulpd xmm2, xmm6;
       haddpd xmm2, xmm2;
       movsd [rcx], xmm2;
       add rcx, rdx;
       add r8, rsi;

       movsd xmm5, [rcx];
       haddpd xmm3, xmm5;
       mulpd xmm3, xmm6;
       haddpd xmm3, xmm3;
       movsd [rcx], xmm3;
       add rcx, rdx;
       add r8, rsi;

       // next rseult
   sub r13, 4;
   jns @@foryloop;

   // ###########################################
   // #### Remaining rows (max 4):
   @@foryloopend:
   add r13, 4;
   jz @@vecend;

   @@foryshortloop:
       // init values:
       xorpd xmm0, xmm0;
       mov rax, r8;       // rax = first matrix element
       mov rbx, r9;       // rbx = first vector element

       mov r10, r14;      // r10 = width
       @@forxshortloop:
           movupd xmm4, [rbx];
           movupd xmm5, [rax];
           mulpd xmm5, xmm4;
           addpd xmm0, xmm5;

           add rax, 16;
           add rbx, 16;
       sub r10, 2;
       jnz @@forxshortloop;

       // build result

       // calculate dest = beta*dest + alpha*xmm0
       movsd xmm5, [rcx];
       haddpd xmm0, xmm5;
       mulpd xmm0, xmm6;
       haddpd xmm0, xmm0;
       movsd [rcx], xmm0;

       add rcx, rdx;
       add r8, rsi;

   dec r13;
   jnz @@foryshortloop;

   @@vecend:

   // epilog pop "stack"
   mov rbx, iRBX;
   mov rsi, iRSI;
   mov rdi, iRDI;
   mov r12, iR12;
   mov r13, iR13;
   mov r14, iR14;

   movupd xmm4, dXMM4;
   movupd xmm5, dXMM5;
   movupd xmm6, dXMM6;
end;

procedure ASMMatrixVectMultOddUnAlignedVAligned(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; {$ifdef UNIX}unixLineWidthMT{$ELSE}LineWidthMT{$endif}, {$ifdef UNIX}unixLineWidthV{$ELSE}LineWidthV{$endif} : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double); {$IFDEF FPC}assembler;{$ENDIF}
// note: RCX = dest, RDX = destLineWidth, R8 = mt1, R9 = v
var dXMM4, dXMM5, dXMM6 : Array[0..1] of double;
    iRBX, iRSI, iRDI, iR12, iR13, iR14 : TASMNativeInt;
    {$ifdef UNIX}
    LineWidthMT, LineWidthV : TASMNativeInt;
    {$endif}
asm
   {$IFDEF UNIX}
   // Linux uses a diffrent ABI -> copy over the registers so they meet with winABI
   // (note that the 5th and 6th parameter are are on the stack)
   // The parameters are passed in the following order:
   // RDI, RSI, RDX, RCX -> mov to RCX, RDX, R8, R9
   mov LineWidthMT, r8;
   mov LineWidthV, r9;
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

   movupd dXMM4, xmm4;
   movupd dXMM5, xmm5;
   movupd dXMM6, xmm6;

   // for the final multiplication
   {$IFDEF UNIX}
   movsd xmm6, alpha;
   movlhps xmm6, beta;
   {$ELSE}
   movhpd xmm6, beta;
   movlpd xmm6, alpha;
   {$ENDIF}

   // prepare for loop
   mov rsi, LineWidthMT;
   mov rdi, LineWidthV;
   mov r13, height;
   mov r14, width;

   sub r13, 4;
   js @@foryloopend;

   // init for x := 0 to width - 1:
   @@foryloop:

       // init values:
       xorpd xmm0, xmm0;
       xorpd xmm1, xmm1;
       xorpd xmm2, xmm2;
       xorpd xmm3, xmm3;  // res := 0;
       mov rax, r8;       // rax = first matrix element
       mov rbx, r9;       // rbx = first vector element

       mov r10, r14;      // r10 = width
       dec r10;
       @@forxloop:
           movupd xmm4, [rbx];

           movupd xmm5, [rax];
           mulpd xmm5, xmm4;
           addpd xmm0, xmm5;

           movupd xmm5, [rax + rsi];
           mulpd xmm5, xmm4;
           addpd xmm1, xmm5;

           movupd xmm5, [rax + 2*rsi];
           mulpd xmm5, xmm4;
           addpd xmm2, xmm5;

           add rax, rsi;
           movupd xmm5, [rax + 2*rsi];
           mulpd xmm5, xmm4;
           addpd xmm3, xmm5;
           sub rax, rsi;

           add rax, 16;
           add rbx, 16;

       sub r10, 2;
       jnz @@forxloop;

       // last element handling
       movsd xmm4, [rbx];

       movsd xmm5, [rax];
       mulsd xmm5, xmm4;
       addsd xmm0, xmm5;

       movsd xmm5, [rax + rsi];
       mulsd xmm5, xmm4;
       addsd xmm1, xmm5;

       movsd xmm5, [rax + 2*rsi];
       mulsd xmm5, xmm4;
       addsd xmm2, xmm5;

       add rax, rsi;
       movsd xmm5, [rax + 2*rsi];
       mulsd xmm5, xmm4;
       addsd xmm3, xmm5;
       sub rax, rsi;

       // result building
       // write back result (final addition and compactation)

       // undo increment


       // calculate dest = beta*dest + alpha*xmm0
       movsd xmm5, [rcx];              // first element
       haddpd xmm0, xmm5;              // calculate xmm0_1 + xmm0_2 (store low) xmm5_1 + xmm5_2 (store high)
       mulpd xmm0, xmm6;               // beta * dest + alpha*xmm0
       haddpd xmm0, xmm0;              // final add
       movsd [rcx], xmm0;              // store back
       add rcx, rdx;
       add r8, rsi;

       movsd xmm5, [rcx];
       haddpd xmm1, xmm5;
       mulpd xmm1, xmm6;
       haddpd xmm1, xmm1;
       movsd [rcx], xmm1;
       add rcx, rdx;
       add r8, rsi;

       movsd xmm5, [rcx];
       haddpd xmm2, xmm5;
       mulpd xmm2, xmm6;
       haddpd xmm2, xmm2;
       movsd [rcx], xmm2;
       add rcx, rdx;
       add r8, rsi;

       movsd xmm5, [rcx];
       haddpd xmm3, xmm5;
       mulpd xmm3, xmm6;
       haddpd xmm3, xmm3;
       movsd [rcx], xmm3;
       add rcx, rdx;
       add r8, rsi;

       // next rseult
   sub r13, 4;
   jns @@foryloop;

   // ###########################################
   // #### Remaining rows (max 4):
   @@foryloopend:
   add r13, 4;
   jz @@vecend;

   @@foryshortloop:
       // init values:
       xorpd xmm0, xmm0;
       mov rax, r8;       // rax = first matrix element
       mov rbx, r9;       // rbx = first vector element

       mov r10, r14;      // r10 = width
       dec r10;
       @@forxshortloop:
           movupd xmm4, [rbx];
           movupd xmm5, [rax];
           mulpd xmm5, xmm4;
           addpd xmm0, xmm5;

           add rax, 16;
           add rbx, 16;
       sub r10, 2;
       jnz @@forxshortloop;

       // last element
       movsd xmm4, [rbx];
       movsd xmm5, [rax];
       mulsd xmm5, xmm4;
       addsd xmm0, xmm5;

       // build result

       // calculate dest = beta*dest + alpha*xmm0
       movsd xmm5, [rcx];
       haddpd xmm0, xmm5;
       mulpd xmm0, xmm6;
       haddpd xmm0, xmm0;
       movsd [rcx], xmm0;

       add rcx, rdx;
       add r8, rsi;

   dec r13;
   jnz @@foryshortloop;

   @@vecend:

   // epilog pop "stack"
   mov rbx, iRBX;
   mov rsi, iRSI;
   mov rdi, iRDI;
   mov r12, iR12;
   mov r13, iR13;
   mov r14, iR14;

   movupd xmm4, dXMM4;
   movupd xmm5, dXMM5;
   movupd xmm6, dXMM6;
end;


// this function is not that well suited for use of simd instructions...
// so only this version exists

procedure ASMMatrixVectMultT(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; {$ifdef UNIX}unixLineWidthMT{$ELSE}LineWidthMT{$endif}, {$ifdef UNIX}unixLineWidthV{$ELSE}LineWidthV{$endif} : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double); {$IFDEF FPC}assembler;{$ENDIF}
var dXMM4, dXMM5, dXMM6, dXMM7 : Array[0..1] of double;
    iRBX, iRSI, iRDI, iR12, iR13, iR14 : TASMNativeInt;

    res0, res1, res2, res3,
    res4, res5, res6, res7 : Array[0..1] of  double;
    {$ifdef UNIX}
    LineWidthMT, LineWidthV : TASMNativeInt;
    {$endif}
asm
   {$IFDEF UNIX}
   // Linux uses a diffrent ABI -> copy over the registers so they meet with winABI
   // (note that the 5th and 6th parameter are are on the stack)
   // The parameters are passed in the following order:
   // RDI, RSI, RDX, RCX -> mov to RCX, RDX, R8, R9
   mov LineWidthMT, r8;
   mov LineWidthV, r9;
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

   movupd dXMM4, xmm4;
   movupd dXMM5, xmm5;
   movupd dXMM6, xmm6;
   movupd dXMM7, xmm7;


   // for the final multiplication
   movddup xmm6, alpha;
   movddup xmm7, beta;

   // prepare for loop
   mov rsi, LineWidthMT;
   mov rdi, LineWidthV;
   mov r13, height;
   mov r14, width;

   // ###################################
   // #### unrolled loop (4 times)

   sub r14, 16;
   js @@forxloopend;

   // init for x := 0 to width - 1:
   @@forxloop:

       // init values:
       xorpd xmm0, xmm0;  // two elements
       // clear out res
       movupd res0, xmm0;
       movupd res1, xmm0;
       movupd res2, xmm0;
       movupd res3, xmm0;
       movupd res4, xmm0;
       movupd res5, xmm0;
       movupd res6, xmm0;
       movupd res7, xmm0;

       mov rax, r8;       // rax = first matrix element
       mov rbx, r9;       // rbx = first vector element

       mov r10, r13;
       @@foryloop:
           movddup xmm3, [rbx];

           movupd xmm4, [rax];
           movupd xmm0, res0;
           mulpd xmm4, xmm3;
           addpd xmm0, xmm4;
           movupd res0, xmm0;

           movupd xmm4, [rax + 16];
           movupd xmm0, res1;
           mulpd xmm4, xmm3;
           addpd xmm0, xmm4;
           movupd res1, xmm0;

           movupd xmm4, [rax + 32];
           movupd xmm0, res2;
           mulpd xmm4, xmm3;
           addpd xmm0, xmm4;
           movupd res2, xmm0;

           movupd xmm4, [rax + 48];
           movupd xmm0, res3;
           mulpd xmm4, xmm3;
           addpd xmm0, xmm4;
           movupd res3, xmm0;

           movupd xmm4, [rax + 64];
           movupd xmm0, res4;
           mulpd xmm4, xmm3;
           addpd xmm0, xmm4;
           movupd res4, xmm0;

           movupd xmm4, [rax + 80];
           movupd xmm0, res5;
           mulpd xmm4, xmm3;
           addpd xmm0, xmm4;
           movupd res5, xmm0;

           movupd xmm4, [rax + 96];
           movupd xmm0, res6;
           mulpd xmm4, xmm3;
           addpd xmm0, xmm4;
           movupd res6, xmm0;

           movupd xmm4, [rax + 112];
           movupd xmm0, res7;
           mulpd xmm4, xmm3;
           addpd xmm0, xmm4;
           movupd res7, xmm0;

           add rax, rsi;
           add rbx, rdi;

       dec r10;
       jnz @@foryloop;

       // result building
       // write back result (final addition and compactation)

       // calculate dest = beta*dest + alpha*xmm0
       // first two
       movsd xmm3, [rcx];
       movsd xmm4, [rcx + rdx];
       movlhps xmm3, xmm4;

       movupd xmm0, res0;
       mulpd xmm0, xmm6;   // alpha*res
       mulpd xmm3, xmm7;   // dest*beta

       addpd xmm3, xmm0;
       movhlps xmm4, xmm3;
       movsd [rcx], xmm3;
       movsd [rcx + rdx], xmm4;
       add rcx, rdx;
       add rcx, rdx;

       // second two
       movsd xmm3, [rcx];
       movsd xmm4, [rcx + rdx];
       movlhps xmm3, xmm4;

       movupd xmm0, res1;
       mulpd xmm0, xmm6;   // alpha*res
       mulpd xmm3, xmm7;   // dest*beta

       addpd xmm3, xmm0;
       movhlps xmm4, xmm3;
       movsd [rcx], xmm3;
       movsd [rcx + rdx], xmm4;

       add rcx, rdx;
       add rcx, rdx;

       // third two
       movsd xmm3, [rcx];
       movsd xmm4, [rcx + rdx];
       movlhps xmm3, xmm4;

       movupd xmm0, res2;
       mulpd xmm0, xmm6;   // alpha*res
       mulpd xmm3, xmm7;   // dest*beta

       addpd xmm3, xmm0;
       movhlps xmm4, xmm3;
       movsd [rcx], xmm3;
       movsd [rcx + rdx], xmm4;

       add rcx, rdx;
       add rcx, rdx;

       // forth two
       movsd xmm3, [rcx];
       movsd xmm4, [rcx + rdx];
       movlhps xmm3, xmm4;

       movupd xmm0, res3;
       mulpd xmm0, xmm6;   // alpha*res
       mulpd xmm3, xmm7;   // dest*beta

       addpd xmm3, xmm0;
       movhlps xmm4, xmm3;
       movsd [rcx], xmm3;
       movsd [rcx + rdx], xmm4;

       add rcx, rdx;
       add rcx, rdx;

       // fith two
       movsd xmm3, [rcx];
       movsd xmm4, [rcx + rdx];
       movlhps xmm3, xmm4;

       movupd xmm0, res4;
       mulpd xmm0, xmm6;   // alpha*res
       mulpd xmm3, xmm7;   // dest*beta

       addpd xmm3, xmm0;
       movhlps xmm4, xmm3;
       movsd [rcx], xmm3;
       movsd [rcx + rdx], xmm4;

       add rcx, rdx;
       add rcx, rdx;

       // sixth two
       movsd xmm3, [rcx];
       movsd xmm4, [rcx + rdx];
       movlhps xmm3, xmm4;

       movupd xmm0, res5;
       mulpd xmm0, xmm6;   // alpha*res
       mulpd xmm3, xmm7;   // dest*beta

       addpd xmm3, xmm0;
       movhlps xmm4, xmm3;
       movsd [rcx], xmm3;
       movsd [rcx + rdx], xmm4;

       add rcx, rdx;
       add rcx, rdx;

       // seventh two
       movsd xmm3, [rcx];
       movsd xmm4, [rcx + rdx];
       movlhps xmm3, xmm4;

       movupd xmm0, res6;
       mulpd xmm0, xmm6;   // alpha*res
       mulpd xmm3, xmm7;   // dest*beta

       addpd xmm3, xmm0;
       movhlps xmm4, xmm3;
       movsd [rcx], xmm3;
       movsd [rcx + rdx], xmm4;

       add rcx, rdx;
       add rcx, rdx;

       // eighth two
       movsd xmm3, [rcx];
       movsd xmm4, [rcx + rdx];
       movlhps xmm3, xmm4;

       movupd xmm0, res7;
       mulpd xmm0, xmm6;   // alpha*res
       mulpd xmm3, xmm7;   // dest*beta

       addpd xmm3, xmm0;
       movhlps xmm4, xmm3;
       movsd [rcx], xmm3;
       movsd [rcx + rdx], xmm4;

       add rcx, rdx;
       add rcx, rdx;

       // next results:
       add r8, 8*16;      // next mt1 element
   sub r14, 16;
   jns @@forxloop;

   @@forxloopend:

   // ###########################################
   // elements that not fit into mod 16
   add r14, 16;
   jz @@vecaddend;

   @@forxshortloop:
       xorpd xmm0, xmm0;  // first two elements

       mov rax, r8;       // rax = first matrix element
       mov rbx, r9;       // rbx = first vector element

       mov r10, r13;

       @@forshortyloop:
           movsd xmm1, [rax];
           movsd xmm2, [rbx];

           mulsd xmm1, xmm2;
           addsd xmm0, xmm1;

           add rax, rsi;
           add rbx, rdi;

       dec r10;
       jnz @@forshortyloop;

       mulsd xmm0, xmm6;  // alpha*res

       movsd xmm3, [rcx];
       mulsd xmm3, xmm7;  //dest*beta
       addsd xmm0, xmm3;
       movsd [rcx], xmm0;

       // next row
       add rcx, rdx;
       add r8, 8;

   dec r14;
   jnz @@forxshortloop;


   @@vecaddend:

   // epilog pop "stack"
   mov rbx, iRBX;
   mov rsi, iRSI;
   mov rdi, iRDI;
   mov r12, iR12;
   mov r13, iR13;
   mov r14, iR14;

   movupd xmm4, dXMM4;
   movupd xmm6, dXMM6;
   movupd xmm7, dXMM7;
end;


// simple routine... not used any more
// note: RCX = dest, RDX = destLineWidth, R8 = mt1, R9 = v
procedure ASMMatrixVectMultT1(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double); {$IFDEF FPC}assembler;{$ENDIF}
var dXMM6 : Array[0..1] of double;
    iRBX, iRSI, iRDI, iR12, iR13, iR14 : TASMNativeInt;
asm
{$IFDEF UNIX}
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

   movupd dXMM6, xmm6;

   // for the final multiplication
   {$IFDEF UNIX}
   movsd xmm6, alpha;
   movlhps xmm6, beta;
   {$ELSE}
   movhpd xmm6, beta;
   movlpd xmm6, alpha;
   {$ENDIF}

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
       haddpd xmm3, xmm3;

       movsd [rcx], xmm3;

       // next results:
       add rcx, rdx;   // next dest element
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
end;


procedure ASMMatrixVectMultTDestVec(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; {$ifdef UNIX}unixLineWidthMT{$ELSE}LineWidthMT{$endif}, {$ifdef UNIX}unixLineWidthV{$ELSE}LineWidthV{$endif} : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double); {$IFDEF FPC}assembler;{$ENDIF}
var dXMM4, dXMM6, dXMM7 : Array[0..1] of double;
    iRBX, iRSI, iRDI, iR12, iR13, iR14 : TASMNativeInt;

    res0, res1, res2, res3,
    res4, res5, res6, res7 : Array[0..1] of  double;
    {$ifdef UNIX}
    LineWidthMT, LineWidthV : TASMNativeInt;
    {$endif}
asm
   {$IFDEF UNIX}
   // Linux uses a diffrent ABI -> copy over the registers so they meet with winABI
   // (note that the 5th and 6th parameter are are on the stack)
   // The parameters are passed in the following order:
   // RDI, RSI, RDX, RCX -> mov to RCX, RDX, R8, R9
   mov LineWidthMT, r8;
   mov LineWidthV, r9;
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

   movupd dXMM4, xmm4;
   movupd dXMM6, xmm6;
   movupd dXMM7, xmm7;

   // for the final multiplication
   movddup xmm6, alpha;
   movddup xmm7, beta;

   // prepare for loop
   mov rsi, LineWidthMT;
   mov rdi, LineWidthV;
   mov r13, height;
   mov r14, width;

   // ###################################
   // #### unrolled loop (4 times)

   sub r14, 16;
   js @@forxloopend;

   // init for x := 0 to width - 1:
   @@forxloop:

       // init values:
       xorpd xmm0, xmm0;  // two elements
       // clear out res
       movupd res0, xmm0;
       movupd res1, xmm0;
       movupd res2, xmm0;
       movupd res3, xmm0;
       movupd res4, xmm0;
       movupd res5, xmm0;
       movupd res6, xmm0;
       movupd res7, xmm0;

       mov rax, r8;       // rax = first matrix element
       mov rbx, r9;       // rbx = first vector element

       mov r10, r13;
       @@foryloop:
           movddup xmm3, [rbx];

           movupd xmm4, [rax];
           movupd xmm0, res0;
           mulpd xmm4, xmm3;
           addpd xmm0, xmm4;
           movupd res0, xmm0;

           movupd xmm4, [rax + 16];
           movupd xmm0, res1;
           mulpd xmm4, xmm3;
           addpd xmm0, xmm4;
           movupd res1, xmm0;

           movupd xmm4, [rax + 32];
           movupd xmm0, res2;
           mulpd xmm4, xmm3;
           addpd xmm0, xmm4;
           movupd res2, xmm0;

           movupd xmm4, [rax + 48];
           movupd xmm0, res3;
           mulpd xmm4, xmm3;
           addpd xmm0, xmm4;
           movupd res3, xmm0;

           movupd xmm4, [rax + 64];
           movupd xmm0, res4;
           mulpd xmm4, xmm3;
           addpd xmm0, xmm4;
           movupd res4, xmm0;

           movupd xmm4, [rax + 80];
           movupd xmm0, res5;
           mulpd xmm4, xmm3;
           addpd xmm0, xmm4;
           movupd res5, xmm0;

           movupd xmm4, [rax + 96];
           movupd xmm0, res6;
           mulpd xmm4, xmm3;
           addpd xmm0, xmm4;
           movupd res6, xmm0;

           movupd xmm4, [rax + 112];
           movupd xmm0, res7;
           mulpd xmm4, xmm3;
           addpd xmm0, xmm4;
           movupd res7, xmm0;

           add rax, rsi;
           add rbx, rdi;

       dec r10;
       jnz @@foryloop;

       // result building
       // write back result (final addition and compactation)

       // calculate dest = beta*dest + alpha*xmm0
       // first two
       movupd xmm3, [rcx];

       movupd xmm0, res0;
       mulpd xmm0, xmm6;   // alpha*res
       mulpd xmm3, xmm7;   // dest*beta

       addpd xmm3, xmm0;
       movupd [rcx], xmm3;
       add rcx, 16;

       // second two
       movupd xmm3, [rcx];

       movupd xmm0, res1;
       mulpd xmm0, xmm6;   // alpha*res
       mulpd xmm3, xmm7;   // dest*beta

       addpd xmm3, xmm0;
       movupd [rcx], xmm3;
       add rcx, 16;

       // third two
       movupd xmm3, [rcx];

       movupd xmm0, res2;
       mulpd xmm0, xmm6;   // alpha*res
       mulpd xmm3, xmm7;   // dest*beta

       addpd xmm3, xmm0;
       movupd [rcx], xmm3;
       add rcx, 16;

       // forth two
       movupd xmm3, [rcx];

       movupd xmm0, res3;
       mulpd xmm0, xmm6;   // alpha*res
       mulpd xmm3, xmm7;   // dest*beta

       addpd xmm3, xmm0;
       movupd [rcx], xmm3;
       add rcx, 16;

       // fith two
       movupd xmm3, [rcx];

       movupd xmm0, res4;
       mulpd xmm0, xmm6;   // alpha*res
       mulpd xmm3, xmm7;   // dest*beta

       addpd xmm3, xmm0;
       movupd [rcx], xmm3;
       add rcx, 16;

       // sixth two
       movupd xmm3, [rcx];
       movupd xmm0, res5;
       mulpd xmm0, xmm6;   // alpha*res
       mulpd xmm3, xmm7;   // dest*beta

       addpd xmm3, xmm0;
       movupd [rcx], xmm3;
       add rcx, 16;

       // seventh two
       movupd xmm3, [rcx];
       movupd xmm0, res6;
       mulpd xmm0, xmm6;   // alpha*res
       mulpd xmm3, xmm7;   // dest*beta

       addpd xmm3, xmm0;
       movupd [rcx], xmm3;
       add rcx, 16;

       // eighth two
       movupd xmm3, [rcx];

       movupd xmm0, res7;
       mulpd xmm0, xmm6;   // alpha*res
       mulpd xmm3, xmm7;   // dest*beta

       addpd xmm3, xmm0;
       movupd [rcx], xmm3;
       add rcx, 16;

       // next results:
       add r8, 8*16;      // next mt1 element
   sub r14, 16;
   jns @@forxloop;

   @@forxloopend:

   // ###########################################
   // elements that not fit into mod 16
   add r14, 16;
   jz @@vecaddend;

   @@forxshortloop:
       xorpd xmm0, xmm0;  // first two elements

       mov rax, r8;       // rax = first matrix element
       mov rbx, r9;       // rbx = first vector element

       mov r10, r13;

       @@forshortyloop:
           movsd xmm1, [rax];
           movsd xmm2, [rbx];

           mulsd xmm1, xmm2;
           addsd xmm0, xmm1;

           add rax, rsi;
           add rbx, rdi;

       dec r10;
       jnz @@forshortyloop;

       mulsd xmm0, xmm6;  // alpha*res

       movsd xmm3, [rcx];
       mulsd xmm3, xmm7;  //dest*beta
       addsd xmm0, xmm3;
       movsd [rcx], xmm0;

       // next column
       add rcx, rdx;
       add r8, 8;

   dec r14;
   jnz @@forxshortloop;


   @@vecaddend:

   // epilog pop "stack"
   mov rbx, iRBX;
   mov rsi, iRSI;
   mov rdi, iRDI;
   mov r12, iR12;
   mov r13, iR13;
   mov r14, iR14;

   movupd xmm4, dXMM4;
   movupd xmm6, dXMM6;
   movupd xmm7, dXMM7;
end;


procedure ASMRank1UpdateSeq(A : PDouble; const LineWidthA : TASMNativeInt; width, height : TASMNativeInt;
  {$ifdef UNIX}unixX{$ELSE}X{$endif}, {$ifdef UNIX}unixY{$ELSE}Y{$ENDIF} : PDouble; incX, incY : TASMNativeInt; alpha : double); {$IFDEF FPC}assembler;{$ENDIF}
var iRBX, iRSI, iRDI, iR12, iR13, iR14 : TASMNativeInt;
    {$ifdef UNIX}
    X, Y : PDouble;
    {$endif}
asm
   {$IFDEF UNIX}
   // Linux uses a diffrent ABI -> copy over the registers so they meet with winABI
   // (note that the 5th and 6th parameter are are on the stack)
   // The parameters are passed in the following order:
   // RDI, RSI, RDX, RCX -> mov to RCX, RDX, R8, R9
   mov X, r8;
   mov Y, r9;
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

   mov r12, r8;
   sar r12, 1;   // width div 2

   // performs A = A + alpha*X*Y' in row major form

   mov rdi, X;
   mov r13, Y;

   // for the temp calculation
   movddup xmm3, alpha;

   // prepare for loop
   mov rsi, incx;
   // init for y := 0 to height - 1:
   @@foryloop:

      // init values:
      movddup xmm0, [rdi];  // res := 0;
      mulpd xmm0, xmm3;     // tmp := alpha*pX^
      mov rax, rcx;         // eax = first destination element A
      mov rbx, r13;           // ebx = first y vector element

      // for j := 0 to width - 1 do
      mov r14, r12;
      test r14, r14;
      jz @@lastElem;

      @@forxloop:
         movupd xmm1, [rax];
         movupd xmm2, [rbx];

         // pA^[j] := pA^[j] + tmp*pY1^[j];
         mulpd xmm2, xmm0;
         addpd xmm1, xmm2;

         movupd [rax], xmm1;

         add rax, 16;
         add rbx, 16;

      dec r14;
      jnz @@forxloop;

      // check if we need to handle the last element
      mov r14, r8;
      and r14, 1;
      jz @@nextline;

      @@lastElem:

      movsd xmm1, [rax];
      movsd xmm2, [rbx];

      mulsd xmm2, xmm0;
      addsd xmm1, xmm2;
      movsd [rax], xmm1;

      @@nextline:

      // next results:
      add rdi, rsi;
      add rcx, rdx;
   dec r9;          // r9 = height
   jnz @@foryloop;

   // epilog
   mov rbx, iRBX;
   mov rsi, iRSI;
   mov rdi, iRDI;
   mov r12, iR12;
   mov r13, iR13;
   mov r14, iR14;
end;

procedure ASMRank1UpdateNonSeq(A : PDouble; const LineWidthA : TASMNativeInt; width, height : TASMNativeInt;
  {$ifdef UNIX}unixX{$ELSE}X{$endif}, {$ifdef UNIX}unixY{$ELSE}Y{$ENDIF} : PDouble; incX, incY : TASMNativeInt; alpha : double);  {$IFDEF FPC}assembler;{$ENDIF}
var iRBX, iRSI, iRDI, iR12, iR13, iR14, iR15 : TASMNativeInt;
    {$ifdef UNIX}
    X, Y : PDouble;
    {$endif}
asm
   {$IFDEF UNIX}
   // Linux uses a diffrent ABI -> copy over the registers so they meet with winABI
   // (note that the 5th and 6th parameter are are on the stack)
   // The parameters are passed in the following order:
   // RDI, RSI, RDX, RCX -> mov to RCX, RDX, R8, R9
   mov X, r8;
   mov Y, r9;
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

   mov r15, incY;
   mov r12, r8;
   sar r12, 1;   // width div 2

   // performs A = A + alpha*X*Y' in row major form

   mov rdi, X;
   mov r13, Y;

   // for the temp calculation
   movddup xmm3, alpha;

   // prepare for loop
   mov rsi, incx;
   // init for y := 0 to height - 1:
   @@foryloop:

      // init values:
      movddup xmm0, [rdi];  // res := 0;
      mulpd xmm0, xmm3;     // tmp := alpha*pX^
      mov rax, rcx;         // eax = first destination element A
      mov rbx, r13;           // ebx = first y vector element

      // for j := 0 to width - 1 do
      mov r14, r12;
      test r14, r14;
      jz @@lastElem;

      @@forxloop:
         movupd xmm1, [rax];
         movlpd xmm2, [rbx];
         movhpd xmm2, [rbx + r15];

         // pA^[j] := pA^[j] + tmp*pY1^[j];
         mulpd xmm2, xmm0;
         addpd xmm1, xmm2;

         movupd [rax], xmm1;

         add rax, 16;
         add rbx, r15;
         add rbx, r15;

      dec r14;
      jnz @@forxloop;

      // check if we need to handle the last element
      mov r14, r8;
      and r14, 1;
      jz @@nextline;

      @@lastElem:

      movsd xmm1, [rax];
      movsd xmm2, [rbx];

      mulsd xmm2, xmm0;
      addsd xmm1, xmm2;
      movsd [rax], xmm1;

      @@nextline:

      // next results:
      add rdi, rsi;
      add rcx, rdx;
   dec r9;          // r9 = height
   jnz @@foryloop;

   // epilog
   mov rbx, iRBX;
   mov rsi, iRSI;
   mov rdi, iRDI;
   mov r12, iR12;
   mov r13, iR13;
   mov r14, iR14;
   mov r15, iR15;
end;

procedure ASMRank1UpdateSeqAligned(A : PDouble; const LineWidthA : TASMNativeInt; width, height : TASMNativeInt;
  {$ifdef UNIX}unixX{$ELSE}X{$endif}, {$ifdef UNIX}unixY{$ELSE}Y{$ENDIF} : PDouble; incX, incY : TASMNativeInt; alpha : double); {$IFDEF FPC}assembler;{$ENDIF}
// note: RCX = A, RDX = LineWidthA, R8 = width, R9 = height
var iRBX, iRSI, iRDI, iR12, iR13, iR14 : TASMNativeInt;
    {$ifdef UNIX}
    X, Y : PDouble;
    {$endif}
asm
   {$IFDEF UNIX}
   // Linux uses a diffrent ABI -> copy over the registers so they meet with winABI
   // (note that the 5th and 6th parameter are are on the stack)
   // The parameters are passed in the following order:
   // RDI, RSI, RDX, RCX -> mov to RCX, RDX, R8, R9
   mov X, r8;
   mov Y, r9;
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

   mov r12, r8;
   sar r12, 1;   // width div 2

   // performs A = A + alpha*X*Y' in row major form

   mov rdi, X;
   mov r13, Y;

   // for the temp calculation
   movddup xmm3, alpha;

   // prepare for loop
   mov rsi, incx;
   // init for y := 0 to height - 1:
   @@foryloop:

      // init values:
      movddup xmm0, [rdi];  // res := 0;
      mulpd xmm0, xmm3;     // tmp := alpha*pX^
      mov rax, rcx;         // eax = first destination element A
      mov rbx, r13;           // ebx = first y vector element

      // for j := 0 to width - 1 do
      mov r14, r12;
      test r14, r14;
      jz @@lastElem;

      @@forxloop:
      movapd xmm1, [rax];
      movapd xmm2, [rbx];

      // pA^[j] := pA^[j] + tmp*pY1^[j];
      mulpd xmm2, xmm0;
      addpd xmm1, xmm2;

      movupd [rax], xmm1;

      add rax, 16;
      add rbx, 16;

      dec r14;
      jnz @@forxloop;

      // check if we need to handle the last element
      mov r14, r8;
      and r14, 1;
      jz @@nextline;

      @@lastElem:

      movsd xmm1, [rax];
      movsd xmm2, [rbx];

      mulsd xmm2, xmm0;
      addsd xmm1, xmm2;
      movsd [rax], xmm1;

      @@nextline:

      // next results:
      add rdi, rsi;
      add rcx, rdx;
   dec r9;          // r9 = height
   jnz @@foryloop;

   // epilog
   mov rbx, iRBX;
   mov rsi, iRSI;
   mov rdi, iRDI;
   mov r12, iR12;
   mov r13, iR13;
   mov r14, iR14;
end;

{$ENDIF}

end.
