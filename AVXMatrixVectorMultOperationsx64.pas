// ###################################################################
// #### This file is part of the mathematics library project, and is
// #### offered under the licence agreement described on
// #### http://www.mrsoft.org/
// ####
// #### Copyright:(c) 2018, Michael R. . All rights reserved.
// ####
// #### Unless required by applicable law or agreed to in writing, software
// #### distributed under the License is distributed on an "AS IS" BASIS,
// #### WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// #### See the License for the specific language governing permissions and
// #### limitations under the License.
// ###################################################################


unit AVXMatrixVectorMultOperationsx64;

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


procedure AVXMatrixVectMultT(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);
procedure AVXMatrixVectMult(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);

// routines with special input layouts: LineWidthV needs to be sizeof(double)
procedure AVXMatrixVectMultAlignedVAligned(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);
procedure AVXMatrixVectMultUnAlignedVAligned(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);

// destlinewidth needs to be sizeof(double)!
// no speed gain agains amsmatrixVectMultT
procedure AVXMatrixVecMultTDestVec(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);

// rank1 update: A = A + alpha*X*Y' where x and y are vectors. It's assumed that y is sequential
procedure AVXRank1UpdateSeq(A : PDouble; const LineWidthA : TASMNativeInt; width, height : TASMNativeInt;
  const alpha : double; X, Y : PDouble; incX, incY : TASMNativeInt);
procedure AVXRank1UpdateSeqAligned(A : PDouble; const LineWidthA : TASMNativeInt; width, height : TASMNativeInt;
  const alpha : double; X, Y : PDouble; incX, incY : TASMNativeInt);

{$ENDIF}

implementation

{$IFDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$ENDIF}

procedure AVXMatrixVectMult(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);
// note: RCX = dest, RDX = destLineWidth, R8 = mt1, R9 = v
var dXMM4, dXMM5, dXMM6, dXMM7 : Array[0..1] of double;
    iRBX, iRSI, iRDI, iR12, iR13, iR14 : TASMNativeInt;
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
   vmovupd dXMM4, xmm4;
   vmovupd dXMM5, xmm5;
   vmovupd dXMM6, xmm6;
   vmovupd dXMM7, xmm7;

   mov iRBX, rbx;
   mov iRSI, rsi;
   mov iRDI, rdi;
   mov iR12, r12;
   mov iR13, r13;
   mov iR14, r14;


   // for the final multiplication
   lea rbx, alpha;
   vmovsd xmm6, [rbx];
   lea rax, beta;
   vmovhpd xmm6, xmm6, [rax];

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
       vxorpd xmm0, xmm0, xmm0;
       vxorpd xmm1, xmm1, xmm1;
       vxorpd xmm2, xmm2, xmm2;
       vxorpd xmm3, xmm3, xmm3;  // res := 0;
       mov rax, r8;       // rax = first matrix element
       mov rbx, r9;       // rbx = first vector element

       mov r10, r14;      // r10 = width
       @@forxloop:
           vmovsd xmm4, [rbx];

           vmovsd xmm5, [rax];
           vmulsd xmm5, xmm5, xmm4;
           vaddsd xmm0, xmm0, xmm5;

           vmovsd xmm5, [rax + rsi];
           vmulsd xmm5, xmm5, xmm4;
           vaddsd xmm1, xmm1, xmm5;

           vmovsd xmm5, [rax + 2*rsi];
           vmulsd xmm5, xmm5, xmm4;
           vaddsd xmm2, xmm2, xmm5;

           add rax, rsi;
           vmovsd xmm5, [rax + 2*rsi];
           vmulsd xmm5, xmm5, xmm4;
           vaddsd xmm3, xmm3, xmm5;
           sub rax, rsi;

           add rax, 8;
           add rbx, rdi;

       dec r10;
       jnz @@forxloop;

       // result building
       // write back result (final addition and compactation)

       // calculate dest = beta*dest + alpha*xmm0
       vmovhpd xmm0, xmm0, [rcx];
       vmulpd xmm0, xmm0, xmm6;
       vhaddpd xmm0, xmm0, xmm0;
       vmovsd [rcx], xmm0;
       add rcx, rdx;

       vmovhpd xmm1, xmm1, [rcx];
       vmulpd xmm1, xmm1, xmm6;
       vhaddpd xmm1, xmm1, xmm1;
       vmovsd [rcx], xmm1;
       add rcx, rdx;

       vmovhpd xmm2, xmm2, [rcx];
       vmulpd xmm2, xmm2, xmm6;
       vhaddpd xmm2, xmm2, xmm2;
       vmovsd [rcx], xmm2;
       add rcx, rdx;

       vmovhpd xmm3, xmm3, [rcx];
       vmulpd xmm3, xmm3, xmm6;
       vhaddpd xmm3, xmm3, xmm3;
       vmovsd [rcx], xmm3;
       add rcx, rdx;             // next dest element

       lea r8, [r8 + 4*rsi];

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
       vxorpd xmm0, xmm0, xmm0;
       mov rax, r8;       // rax = first matrix element
       mov rbx, r9;       // rbx = first vector element

       mov r10, r14;      // r10 = width
       @@forxshortloop:
           vmovsd xmm4, [rbx];

           vmovsd xmm5, [rax];
           vmulsd xmm5, xmm5, xmm4;
           vaddsd xmm0, xmm0, xmm5;

           add rax, 8;
           add rbx, rdi;
       dec r10;
       jnz @@forxshortloop;

       // build result

       // calculate dest = beta*dest + alpha*xmm0
       vmovhpd xmm0, xmm0, [rcx];
       vmulpd xmm0, xmm0, xmm6;
       vhaddpd xmm0, xmm0, xmm0;
       vmovsd [rcx], xmm0;
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

   vmovupd xmm4, dXMM4;
   vmovupd xmm5, dXMM5;
   vmovupd xmm6, dXMM6;
   vmovupd xmm7, dXMM7;
   vzeroupper;
end;
{$IFDEF FPC}
end;
{$ENDIF}


// routines with special input layouts: LineWidthV needs to be sizeof(double)
procedure AVXMatrixVectMultAlignedVAligned(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);
// note: RCX = dest, RDX = destLineWidth, R8 = mt1, R9 = v
var dXMM4, dXMM5, dXMM6 : Array[0..1] of double;
    iRBX, iRSI, iRDI, iR12, iR13, iR14 : TASMNativeInt;
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

   vmovupd dXMM4, xmm4;
   vmovupd dXMM5, xmm5;
   vmovupd dXMM6, xmm6;

   // for the final multiplication
   lea rbx, alpha;
   vmovsd xmm6, [rbx];
   lea rax, beta;
   vmovhpd xmm6, xmm6, [rax];


   // prepare for loop
   mov rsi, LineWidthMT;
   mov rdi, LineWidthV;
   mov r13, height;
   mov r14, width;

   sub r14, 4;
   sub r13, 4;
   js @@foryloopend;

   // we need at least a width of 4 for the fast unrolled loop
   cmp r14, 0;
   jl @@foryloopend;

   // init for x := 0 to width - 1:
   @@foryloop:

       // init values:
       vxorpd ymm0, ymm0, ymm0;
       vxorpd ymm1, ymm1, ymm1;
       vxorpd ymm2, ymm2, ymm2;
       vxorpd ymm3, ymm3, ymm3;  // res := 0;
       mov rax, r8;       // rax = first matrix element
       mov rbx, r9;       // rbx = first vector element

       mov r10, r14;      // r10 = width
       @@forxloop:
           vmovapd ymm4, [rbx];

           //vmovapd ymm5, [rax];
           vmulpd ymm5, ymm4, [rax];
           vaddpd ymm0, ymm0, ymm5;

           //vmovapd ymm5, [rax + rsi];
           vmulpd ymm5, ymm4, [rax + rsi];
           vaddpd ymm1, ymm1, ymm5;

           //vmovupd ymm5, [rax + 2*rsi];
           vmulpd ymm5, ymm4, [rax + 2*rsi];
           vaddpd ymm2, ymm2, ymm5;

           add rax, rsi;
           //vmovupd ymm5, [rax + 2*rsi];
           vmulpd ymm5, ymm4, [rax + 2*rsi];
           vaddpd ymm3, ymm3, ymm5;
           sub rax, rsi;

           add rax, 32;
           add rbx, 32;

       sub r10, 4;
       jge @@forxloop;

       vextractf128 xmm4, ymm0, 1;
       vhaddpd xmm0, xmm0, xmm4;
       vextractf128 xmm5, ymm1, 1;
       vhaddpd xmm1, xmm1, xmm5;
       vextractf128 xmm4, ymm2, 1;
       vhaddpd xmm2, xmm2, xmm4;
       vextractf128 xmm5, ymm3, 1;
       vhaddpd xmm3, xmm3, xmm5;

       // special treatment for the last value(s):
       add r10, 4;
       jz @@resbuild;

       @@shortloopx:
          vmovsd xmm4, [rbx];
          vmovsd xmm5, [rax];

          vmulsd xmm5, xmm5, xmm4;
          vaddsd xmm0, xmm0, xmm5;

          vmovsd xmm5, [rax + rsi];

          vmulsd xmm5, xmm5, xmm4;
          vaddsd xmm1, xmm1, xmm5;

          vmovsd xmm5, [rax + 2*rsi];

          vmulsd xmm5, xmm5, xmm4;
          vaddsd xmm2, xmm2, xmm5;

          add rax, rsi;
          vmovsd xmm5, [rax + 2*rsi];
          sub rax, rsi;

          vmulsd xmm5, xmm5, xmm4;
          vaddsd xmm3, xmm3, xmm5;

          add rax, 8;
          add rbx, 8;
       sub r10, 1;
       jnz @@shortloopx;


       @@resbuild:
       // result building
       // write back result (final addition and compactation)

       // calculate dest = beta*dest + alpha*xmm0
       vmovsd xmm5, [rcx];              // first element
       vhaddpd xmm0, xmm0, xmm5;        // calculate xmm0_1 + xmm0_2 (store low) xmm5_1 + xmm5_2 (store high)
       vmulpd xmm0, xmm0, xmm6;         // beta * dest + alpha*xmm0
       vhaddpd xmm0, xmm0, xmm0;        // final add
       vmovsd [rcx], xmm0;              // store back
       add rcx, rdx;
       add r8, rsi;

       vmovsd xmm5, [rcx];
       vhaddpd xmm1, xmm1, xmm5;
       vmulpd xmm1, xmm1, xmm6;
       vhaddpd xmm1, xmm1, xmm1;
       vmovsd [rcx], xmm1;
       add rcx, rdx;
       add r8, rsi;

       vmovsd xmm5, [rcx];
       vhaddpd xmm2, xmm2, xmm5;
       vmulpd xmm2, xmm2, xmm6;
       vhaddpd xmm2, xmm2, xmm2;
       vmovsd [rcx], xmm2;
       add rcx, rdx;
       add r8, rsi;

       vmovsd xmm5, [rcx];
       vhaddpd xmm3, xmm3, xmm5;
       vmulpd xmm3, xmm3, xmm6;
       vhaddpd xmm3, xmm3, xmm3;
       vmovsd [rcx], xmm3;
       add rcx, rdx;
       add r8, rsi;

       // next rseult
   sub r13, 4;
   jge @@foryloop;

   // ###########################################
   // #### Remaining rows (max 4 or more if width is <4):
   @@foryloopend:
   add r13, 4;
   jz @@vecend;

   @@foryshortloop:
       // init values:
       vxorpd ymm0, ymm0, ymm0;
       mov rax, r8;       // rax = first matrix element
       mov rbx, r9;       // rbx = first vector element

       mov r10, r14;      // r10 = width - 4
       cmp r10, 0;
       jl @@shortloopend;

       @@forxshortloop:
           vmovapd ymm4, [rbx];
           //vmovapd ymm5, [rax];
           vmulpd ymm5, ymm4, [rax];
           vaddpd ymm0, ymm0, ymm5;

           add rax, 32;
           add rbx, 32;
       sub r10, 4;
       jge @@forxshortloop;

       vextractf128 xmm4, ymm0, 1;
       vhaddpd xmm0, xmm0, xmm4;

       @@shortloopend:

       add r10, 4;
       // test if there are elements left
       jz @@resbuildshort;
       @@forxshortestloop:
           vmovsd xmm4, [rbx];
           vmovsd xmm5, [rax];

           vmulsd xmm5, xmm5, xmm4;
           vaddsd xmm0, xmm0, xmm5;

           add rax, 8;
           add rbx, 8;
       dec r10;
       jnz @@forxshortestloop;

       @@resbuildshort:

       // build result

       // calculate dest = beta*dest + alpha*xmm0
       vmovsd xmm5, [rcx];
       vhaddpd xmm0, xmm0, xmm5;
       vmulpd xmm0, xmm0, xmm6;
       vhaddpd xmm0, xmm0, xmm0;
       vmovsd [rcx], xmm0;

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

   vmovupd xmm4, dXMM4;
   vmovupd xmm5, dXMM5;
   vmovupd xmm6, dXMM6;
   vzeroupper;
end;
{$IFDEF FPC}
end;
{$ENDIF}

procedure AVXMatrixVectMultUnAlignedVAligned(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);
// note: RCX = dest, RDX = destLineWidth, R8 = mt1, R9 = v
var dXMM4, dXMM5, dXMM6 : Array[0..1] of double;
    iRBX, iRSI, iRDI, iR12, iR13, iR14 : TASMNativeInt;
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

   vmovupd dXMM4, xmm4;
   vmovupd dXMM5, xmm5;
   vmovupd dXMM6, xmm6;

   // for the final multiplication
   lea rbx, alpha;
   vmovsd xmm6, [rbx];
   lea rax, beta;
   vmovhpd xmm6, xmm6, [rax];


   // prepare for loop
   mov rsi, LineWidthMT;
   mov rdi, LineWidthV;
   mov r13, height;
   mov r14, width;

   sub r14, 4;
   sub r13, 4;
   js @@foryloopend;

   // we need at least a width of 4 for the fast unrolled loop
   cmp r14, 0;
   jl @@foryloopend;

   // init for x := 0 to width - 1:
   @@foryloop:

       // init values:
       vxorpd ymm0, ymm0, ymm0;
       vxorpd ymm1, ymm1, ymm1;
       vxorpd ymm2, ymm2, ymm2;
       vxorpd ymm3, ymm3, ymm3;  // res := 0;
       mov rax, r8;       // rax = first matrix element
       mov rbx, r9;       // rbx = first vector element

       mov r10, r14;      // r10 = width
       @@forxloop:
           vmovupd ymm4, [rbx];

           vmovupd ymm5, [rax];
           vmulpd ymm5, ymm5, ymm4;
           vaddpd ymm0, ymm0, ymm5;

           vmovupd ymm5, [rax + rsi];
           vmulpd ymm5, ymm5, ymm4;
           vaddpd ymm1, ymm1, ymm5;

           vmovupd ymm5, [rax + 2*rsi];
           vmulpd ymm5, ymm5, ymm4;
           vaddpd ymm2, ymm2, ymm5;

           add rax, rsi;
           vmovupd ymm5, [rax + 2*rsi];
           vmulpd ymm5, ymm5, ymm4;
           vaddpd ymm3, ymm3, ymm5;
           sub rax, rsi;

           add rax, 32;
           add rbx, 32;

       sub r10, 4;
       jge @@forxloop;

       vextractf128 xmm4, ymm0, 1;
       vhaddpd xmm0, xmm0, xmm4;
       vextractf128 xmm5, ymm1, 1;
       vhaddpd xmm1, xmm1, xmm5;
       vextractf128 xmm4, ymm2, 1;
       vhaddpd xmm2, xmm2, xmm4;
       vextractf128 xmm5, ymm3, 1;
       vhaddpd xmm3, xmm3, xmm5;

       // special treatment for the last value(s):
       add r10, 4;
       jz @@resbuild;

       @@shortloopx:
          vmovsd xmm4, [rbx];
          vmovsd xmm5, [rax];

          vmulsd xmm5, xmm5, xmm4;
          vaddsd xmm0, xmm0, xmm5;

          vmovsd xmm5, [rax + rsi];

          vmulsd xmm5, xmm5, xmm4;
          vaddsd xmm1, xmm1, xmm5;

          vmovsd xmm5, [rax + 2*rsi];

          vmulsd xmm5, xmm5, xmm4;
          vaddsd xmm2, xmm2, xmm5;

          add rax, rsi;
          vmovsd xmm5, [rax + 2*rsi];
          sub rax, rsi;

          vmulsd xmm5, xmm5, xmm4;
          vaddsd xmm3, xmm3, xmm5;

          add rax, 8;
          add rbx, 8;
       sub r10, 1;
       jnz @@shortloopx;

       @@resbuild:
       // result building
       // write back result (final addition and compactation)

       // calculate dest = beta*dest + alpha*xmm0
       vmovsd xmm5, [rcx];              // first element
       vhaddpd xmm0, xmm0, xmm5;        // calculate xmm0_1 + xmm0_2 (store low) xmm5_1 + xmm5_2 (store high)
       vmulpd xmm0, xmm0, xmm6;         // beta * dest + alpha*xmm0
       vhaddpd xmm0, xmm0, xmm0;        // final add
       vmovsd [rcx], xmm0;              // store back
       add rcx, rdx;
       add r8, rsi;

       vmovsd xmm5, [rcx];
       vhaddpd xmm1, xmm1, xmm5;
       vmulpd xmm1, xmm1, xmm6;
       vhaddpd xmm1, xmm1, xmm1;
       vmovsd [rcx], xmm1;
       add rcx, rdx;
       add r8, rsi;

       vmovsd xmm5, [rcx];
       vhaddpd xmm2, xmm2, xmm5;
       vmulpd xmm2, xmm2, xmm6;
       vhaddpd xmm2, xmm2, xmm2;
       vmovsd [rcx], xmm2;
       add rcx, rdx;
       add r8, rsi;

       vmovsd xmm5, [rcx];
       vhaddpd xmm3, xmm3, xmm5;
       vmulpd xmm3, xmm3, xmm6;
       vhaddpd xmm3, xmm3, xmm3;
       vmovsd [rcx], xmm3;
       add rcx, rdx;
       add r8, rsi;

       // next rseult
   sub r13, 4;
   jge @@foryloop;

   // ###########################################
   // #### Remaining rows (max 4 or more if width is <4):
   @@foryloopend:
   add r13, 4;
   jz @@vecend;

   @@foryshortloop:
       // init values:
       vxorpd ymm0, ymm0, ymm0;
       mov rax, r8;       // rax = first matrix element
       mov rbx, r9;       // rbx = first vector element

       mov r10, r14;      // r10 = width - 4
       cmp r10, 0;
       jl @@shortloopend;

       @@forxshortloop:
           vmovupd ymm4, [rbx];
           vmovupd ymm5, [rax];
           vmulpd ymm5, ymm5, ymm4;
           vaddpd ymm0, ymm0, ymm5;

           add rax, 32;
           add rbx, 32;
       sub r10, 4;
       jge @@forxshortloop;

       vextractf128 xmm4, ymm0, 1;
       vhaddpd xmm0, xmm0, xmm4;

       @@shortloopend:

       add r10, 4;
       // test if there are elements left
       jz @@resbuildshort;
       @@forxshortestloop:
           vmovsd xmm4, [rbx];
           vmovsd xmm5, [rax];

           vmulsd xmm5, xmm5, xmm4;
           vaddsd xmm0, xmm0, xmm5;

           add rax, 8;
           add rbx, 8;
       dec r10;
       jnz @@forxshortestloop;

       @@resbuildshort:

       // build result

       // calculate dest = beta*dest + alpha*xmm0
       vmovsd xmm5, [rcx];
       vhaddpd xmm0, xmm0, xmm5;
       vmulpd xmm0, xmm0, xmm6;
       vhaddpd xmm0, xmm0, xmm0;
       vmovsd [rcx], xmm0;

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

   vmovupd xmm4, dXMM4;
   vmovupd xmm5, dXMM5;
   vmovupd xmm6, dXMM6;
   vzeroupper;
end;
{$IFDEF FPC}
end;
{$ENDIF}

// this function is not that well suited for use of simd instructions...
// so only this version exists
procedure AVXMatrixVectMultT(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);
var dXMM4, dXMM5, dXMM6, dXMM7 : Array[0..1] of double;
    iRBX, iRSI, iRDI, iR12, iR13, iR14 : TASMNativeInt;
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

   vmovupd dXMM4, xmm4;
   vmovupd dXMM5, xmm5;
   vmovupd dXMM6, xmm6;
   vmovupd dXMM7, xmm7;


   // for the final multiplication
   lea rax, alpha;
   vbroadcastsd ymm6, [rax];
   lea rax, beta;
   vbroadcastsd ymm7, [rax];

   // prepare for loop
   mov rsi, LineWidthMT;
   mov rdi, LineWidthV;
   mov r13, height;
   mov r14, width;

   sub r14, 16;
   js @@forxloopend;

   // init for x := 0 to width - 1:
   @@forxloop:

       // init values:
       vxorpd ymm8, ymm8, ymm8;
       vxorpd ymm9, ymm9, ymm9;
       vxorpd ymm10, ymm10, ymm10;
       vxorpd ymm11, ymm11, ymm11;

       mov rax, r8;       // rax = first matrix element
       mov rbx, r9;       // rbx = first vector element

       mov r10, r13;
       @@foryloop:
           vbroadcastsd ymm3, [rbx];

           vmovupd ymm4, [rax];
           vmulpd ymm4, ymm4, ymm3;
           vaddpd ymm8, ymm8, ymm4;

           vmovupd ymm4, [rax + 32];
           vmulpd ymm4, ymm4, ymm3;
           vaddpd ymm9, ymm9, ymm4;

           vmovupd ymm4, [rax + 64];
           vmulpd ymm4, ymm4, ymm3;
           vaddpd ymm10, ymm10, ymm4;

           vmovupd ymm4, [rax + 96];
           vmulpd ymm4, ymm4, ymm3;
           vaddpd ymm11, ymm11, ymm4;

           add rax, rsi;
           add rbx, rdi;

       dec r10;
       jnz @@foryloop;

       // result building
       // write back result (final addition and compactation)

       // calculate dest = beta*dest + alpha*xmm0
       // first two
       vmovsd xmm3, [rcx];
       vmovsd xmm4, [rcx + rdx];
       vmovlhps xmm3, xmm3, xmm4;

       vmovapd xmm0, xmm8;
       vmulpd xmm0, xmm0, xmm6;   // alpha*res
       vmulpd xmm3, xmm3, xmm7;   // dest*beta

       vaddpd xmm3, xmm3, xmm0;
       vmovhlps xmm4, xmm4, xmm3;
       vmovsd [rcx], xmm3;
       vmovsd [rcx + rdx], xmm4;
       add rcx, rdx;
       add rcx, rdx;

       // second two
       vmovsd xmm3, [rcx];
       vmovsd xmm4, [rcx + rdx];
       vmovlhps xmm3, xmm3, xmm4;

       //movupd xmm0, res1;
       vextractf128 xmm0, ymm8, 1;
       vmulpd xmm0, xmm0, xmm6;   // alpha*res
       vmulpd xmm3, xmm3, xmm7;   // dest*beta

       vaddpd xmm3, xmm3, xmm0;
       vmovhlps xmm4, xmm4, xmm3;
       vmovsd [rcx], xmm3;
       vmovsd [rcx + rdx], xmm4;

       add rcx, rdx;
       add rcx, rdx;

       // third two
       vmovsd xmm3, [rcx];
       vmovsd xmm4, [rcx + rdx];
       vmovlhps xmm3, xmm3, xmm4;

       vmovapd xmm0, xmm9;
       vmulpd xmm0, xmm0, xmm6;   // alpha*res
       vmulpd xmm3, xmm3, xmm7;   // dest*beta

       vaddpd xmm3, xmm3, xmm0;
       vmovhlps xmm4, xmm4, xmm3;
       vmovsd [rcx], xmm3;
       vmovsd [rcx + rdx], xmm4;

       add rcx, rdx;
       add rcx, rdx;

       // forth two
       vmovsd xmm3, [rcx];
       vmovsd xmm4, [rcx + rdx];
       vmovlhps xmm3, xmm3, xmm4;

       //movupd xmm0, res3;
       vextractf128 xmm0, ymm9, 1;
       vmulpd xmm0, xmm0, xmm6;   // alpha*res
       vmulpd xmm3, xmm3, xmm7;   // dest*beta

       vaddpd xmm3, xmm3, xmm0;
       vmovhlps xmm4, xmm4, xmm3;
       vmovsd [rcx], xmm3;
       vmovsd [rcx + rdx], xmm4;

       add rcx, rdx;
       add rcx, rdx;

       // fith two
       vmovsd xmm3, [rcx];
       vmovsd xmm4, [rcx + rdx];
       vmovlhps xmm3, xmm3, xmm4;

       //movupd xmm0, res4;
       vmovapd xmm0, xmm10;
       vmulpd xmm0, xmm0, xmm6;   // alpha*res
       vmulpd xmm3, xmm3, xmm7;   // dest*beta

       vaddpd xmm3, xmm3, xmm0;
       vmovhlps xmm4, xmm4, xmm3;
       vmovsd [rcx], xmm3;
       vmovsd [rcx + rdx], xmm4;

       add rcx, rdx;
       add rcx, rdx;

       // sixth two
       vmovsd xmm3, [rcx];
       vmovsd xmm4, [rcx + rdx];
       vmovlhps xmm3, xmm3, xmm4;

       //movupd xmm0, res5;
       vextractf128 xmm0, ymm10, 1;
       vmulpd xmm0, xmm0, xmm6;   // alpha*res
       vmulpd xmm3, xmm3, xmm7;   // dest*beta

       vaddpd xmm3, xmm3, xmm0;
       vmovhlps xmm4, xmm4, xmm3;
       vmovsd [rcx], xmm3;
       vmovsd [rcx + rdx], xmm4;

       add rcx, rdx;
       add rcx, rdx;

       // seventh two
       vmovsd xmm3, [rcx];
       vmovsd xmm4, [rcx + rdx];
       vmovlhps xmm3, xmm3, xmm4;

       //movupd xmm0, res6;
       vmovapd xmm0, xmm11;
       vmulpd xmm0, xmm0, xmm6;   // alpha*res
       vmulpd xmm3, xmm3, xmm7;   // dest*beta

       vaddpd xmm3, xmm3, xmm0;
       vmovhlps xmm4, xmm4, xmm3;
       vmovsd [rcx], xmm3;
       vmovsd [rcx + rdx], xmm4;

       add rcx, rdx;
       add rcx, rdx;

       // eighth two
       vmovsd xmm3, [rcx];
       vmovsd xmm4, [rcx + rdx];
       vmovlhps xmm3, xmm3, xmm4;

       //movupd xmm0, res7;
       vextractf128 xmm0, ymm11, 1;
       vmulpd xmm0, xmm0, xmm6;   // alpha*res
       vmulpd xmm3, xmm3, xmm7;   // dest*beta

       vaddpd xmm3, xmm3, xmm0;
       vmovhlps xmm4, xmm4, xmm3;
       vmovsd [rcx], xmm3;
       vmovsd [rcx + rdx], xmm4;

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
       vxorpd xmm0, xmm0, xmm0;  // first two elements

       mov rax, r8;       // rax = first matrix element
       mov rbx, r9;       // rbx = first vector element

       mov r10, r13;

       @@forshortyloop:
           vmovsd xmm1, [rax];
           vmovsd xmm2, [rbx];

           vmulsd xmm1, xmm1, xmm2;
           vaddsd xmm0, xmm0, xmm1;

           add rax, rsi;
           add rbx, rdi;

       dec r10;
       jnz @@forshortyloop;

       vmulsd xmm0, xmm0, xmm6;  // alpha*res

       vmovsd xmm3, [rcx];
       vmulsd xmm3, xmm3, xmm7;  //dest*beta
       vaddsd xmm0, xmm0, xmm3;
       vmovsd [rcx], xmm0;

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

   vmovupd xmm4, dXMM4;
   vmovupd xmm6, dXMM6;
   vmovupd xmm7, dXMM7;
   vzeroupper;
end;
{$IFDEF FPC}
end;
{$ENDIF}


procedure AVXMatrixVecMultTDestVec(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);
var dXMM4, dXMM5, dXMM6, dXMM7 : Array[0..1] of double;
    iRBX, iRSI, iRDI, iR12, iR13, iR14 : TASMNativeInt;
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

   vmovupd dXMM4, xmm4;
   vmovupd dXMM5, xmm5;
   vmovupd dXMM6, xmm6;
   vmovupd dXMM7, xmm7;


   // for the final multiplication
   lea rax, alpha;
   vbroadcastsd ymm6, [rax];
   lea rax, beta;
   vbroadcastsd ymm7, [rax];

   // prepare for loop
   mov rsi, LineWidthMT;
   mov rdi, LineWidthV;
   mov r13, height;
   mov r14, width;

   sub r14, 16;
   js @@forxloopend;

   // init for x := 0 to width - 1:
   @@forxloop:

       // init values:
       vxorpd ymm8, ymm8, ymm8;
       vxorpd ymm9, ymm9, ymm9;
       vxorpd ymm10, ymm10, ymm10;
       vxorpd ymm11, ymm11, ymm11;

       mov rax, r8;       // rax = first matrix element
       mov rbx, r9;       // rbx = first vector element

       mov r10, r13;
       @@foryloop:
           vbroadcastsd ymm3, [rbx];

           vmovupd ymm4, [rax];
           vmulpd ymm4, ymm4, ymm3;
           vaddpd ymm8, ymm8, ymm4;

           vmovupd ymm4, [rax + 32];
           vmulpd ymm4, ymm4, ymm3;
           vaddpd ymm9, ymm9, ymm4;

           vmovupd ymm4, [rax + 64];
           vmulpd ymm4, ymm4, ymm3;
           vaddpd ymm10, ymm10, ymm4;

           vmovupd ymm4, [rax + 96];
           vmulpd ymm4, ymm4, ymm3;
           vaddpd ymm11, ymm11, ymm4;

           add rax, rsi;
           add rbx, rdi;

       dec r10;
       jnz @@foryloop;

       // result building
       // write back result (final addition and compactation)

       // calculate dest = beta*dest + alpha*xmm0
       // first 4
       vmovupd ymm3, [rcx];

       vmulpd ymm8, ymm8, ymm6; // alpha*res
       vmulpd ymm3, ymm3, ymm7; // dest*beta
       vaddpd ymm3, ymm3, ymm8;
       vmovupd [rcx], ymm3;
       add rcx, 32;

       // second 4
       vmovupd ymm3, [rcx];

       vmulpd ymm9, ymm9, ymm6; // alpha*res
       vmulpd ymm3, ymm3, ymm7; // dest*beta
       vaddpd ymm3, ymm3, ymm9;
       vmovupd [rcx], ymm3;
       add rcx, 32;

       // third 4
       vmovupd ymm3, [rcx];

       vmulpd ymm10, ymm10, ymm6; // alpha*res
       vmulpd ymm3, ymm3, ymm7; // dest*beta
       vaddpd ymm3, ymm3, ymm10;
       vmovupd [rcx], ymm3;
       add rcx, 32;

       // forth 4
       vmovupd ymm3, [rcx];

       vmulpd ymm11, ymm11, ymm6; // alpha*res
       vmulpd ymm3, ymm3, ymm7; // dest*beta
       vaddpd ymm3, ymm3, ymm11;
       vmovupd [rcx], ymm3;
       add rcx, 32;

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
       vxorpd xmm0, xmm0, xmm0;  // first two elements

       mov rax, r8;       // rax = first matrix element
       mov rbx, r9;       // rbx = first vector element

       mov r10, r13;

       @@forshortyloop:
           vmovsd xmm1, [rax];
           vmovsd xmm2, [rbx];

           vmulsd xmm1, xmm1, xmm2;
           vaddsd xmm0, xmm0, xmm1;

           add rax, rsi;
           add rbx, rdi;

       dec r10;
       jnz @@forshortyloop;

       vmulsd xmm0, xmm0, xmm6;  // alpha*res

       vmovsd xmm3, [rcx];
       vmulsd xmm3, xmm3, xmm7;  //dest*beta
       vaddsd xmm0, xmm0, xmm3;
       vmovsd [rcx], xmm0;

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

   vmovupd xmm4, dXMM4;
   vmovupd xmm6, dXMM6;
   vmovupd xmm7, dXMM7;
   vzeroupper;
end;
{$IFDEF FPC}
end;
{$ENDIF}


procedure AVXRank1UpdateSeq(A : PDouble; const LineWidthA : TASMNativeInt; width, height : TASMNativeInt;
  const alpha : double; X, Y : PDouble; incX, incY : TASMNativeInt);
// note: RCX = A, RDX = LineWidthA, R8 = width, R9 = height
var iRBX, iRSI, iRDI, iR12, iR13, iR14 : TASMNativeInt;
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

   mov r12, width;
   sar r12, 2;   // width div 4

   // performs A = A + alpha*X*Y' in row major form

   mov rdi, X;
   mov r13, Y;

   // for the temp calculation
   lea rax, alpha;
   vbroadcastsd ymm3, [rax];

   // prepare for loop
   mov rsi, incx;
   // init for y := 0 to height - 1:
   @@foryloop:
      // init values:
      vbroadcastsd ymm0, [rdi];  // res := 0;
      vmulpd ymm0, ymm0, ymm3;     // tmp := alpha*pX^
      mov rax, rcx;              // eax = first destination element A
      mov rbx, r13;           // ebx = first y vector element

      // for j := 0 to width - 1 do
      mov r14, r12;
      test r14, r14;
      jz @@last3Elem;

      @@forxloop:
         vmovupd ymm1, [rax];
         vmovupd ymm2, [rbx];

         // pA^[j] := pA^[j] + tmp*pY1^[j];
         vmulpd ymm2, ymm2, ymm0;
         vaddpd ymm1, ymm1, ymm2;

         vmovupd [rax], ymm1;

         add rax, 32;
         add rbx, 32;

      dec r14;
      jnz @@forxloop;

      @@last3Elem:
      mov r14, r8;
      and r14, $3;
      jz @@nextline;

      // check if there is only one element to process
      cmp r14, 1;
      je @@lastElem;

      // handle 2 elements
      vmovupd xmm1, [rax];
      vmovupd xmm2, [rbx];

      vmulpd xmm2, xmm2, xmm0;
      vaddpd xmm1, xmm1, xmm2;
      vmovupd [rax], xmm1;
      add rax, 16;
      add rbx, 16;

      cmp r14, 2;
      je @@nextline;

      @@lastElem:

      vmovsd xmm1, [rax];
      vmovsd xmm2, [rbx];

      vmulsd xmm2, xmm2, xmm0;
      vaddsd xmm1, xmm1, xmm2;
      vmovsd [rax], xmm1;

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
   vzeroupper;
end;
{$IFDEF FPC}
end;
{$ENDIF}


procedure AVXRank1UpdateSeqAligned(A : PDouble; const LineWidthA : TASMNativeInt; width, height : TASMNativeInt;
  const alpha : double; X, Y : PDouble; incX, incY : TASMNativeInt);
// note: RCX = A, RDX = LineWidthA, R8 = width, R9 = height
var iRBX, iRSI, iRDI, iR12, iR13, iR14 : TASMNativeInt;
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

   mov r12, width;
   sar r12, 2;   // width div 4

   // performs A = A + alpha*X*Y' in row major form

   mov rdi, X;
   mov r13, Y;

   // for the temp calculation
   lea rax, alpha;
   vbroadcastsd ymm3, [rax];

   // prepare for loop
   mov rsi, incx;
   // init for y := 0 to height - 1:
   @@foryloop:
      // init values:
      vbroadcastsd ymm0, [rdi];  // res := 0;
      vmulpd ymm0, ymm0, ymm3;     // tmp := alpha*pX^
      mov rax, rcx;              // eax = first destination element A
      mov rbx, r13;           // ebx = first y vector element

      // for j := 0 to width - 1 do
      mov r14, r12;
      test r14, r14;
      jz @@last3Elem;

      @@forxloop:
         vmovapd ymm1, [rax];
         vmovapd ymm2, [rbx];

         // pA^[j] := pA^[j] + tmp*pY1^[j];
         vmulpd ymm2, ymm2, ymm0;
         vaddpd ymm1, ymm1, ymm2;

         vmovapd [rax], ymm1;

         add rax, 32;
         add rbx, 32;

      dec r14;
      jnz @@forxloop;

      @@last3Elem:
      mov r14, r8;
      and r14, $3;
      jz @@nextline;

      // check if there is only one element to process
      cmp r14, 1;
      je @@lastElem;

      // handle 2 elements
      vmovapd xmm1, [rax];
      vmovapd xmm2, [rbx];

      vmulpd xmm2, xmm2, xmm0;
      vaddpd xmm1, xmm1, xmm2;
      vmovapd [rax], xmm1;
      add rax, 16;
      add rbx, 16;

      cmp r14, 2;
      je @@nextline;

      @@lastElem:

      vmovsd xmm1, [rax];
      vmovsd xmm2, [rbx];

      vmulsd xmm2, xmm2, xmm0;
      vaddsd xmm1, xmm1, xmm2;
      vmovsd [rax], xmm1;

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
   vzeroupper;
end;
{$IFDEF FPC}
end;
{$ENDIF}

{$ENDIF}

end.
