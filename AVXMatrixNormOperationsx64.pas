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


unit AVXMatrixNormOperationsx64;

interface

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF x64}

uses MatrixConst;

function AVXMatrixElementwiseNorm2Aligned(dest : PDouble; const LineWidth : TASMNativeInt; Width, height : TASMNativeInt) : double;
function AVXMatrixElementwiseNorm2UnAligned(dest : PDouble; const LineWidth : TASMNativeInt; Width, height : TASMNativeInt) : double;


procedure AVXMatrixNormalizeRowAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
procedure AVXMatrixNormalizeRowUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);

procedure AVXMatrixNormalizeColumnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
procedure AVXMatrixNormalizeColumnUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);

{$ENDIF}

implementation

{$IFDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$ENDIF}

function AVXMatrixElementwiseNorm2Aligned(dest : PDouble; const LineWidth : TASMNativeInt; Width, height : TASMNativeInt) : double;
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

   // note: RCX = dest, RDX = destLineWidth, R8 = widh, R9 = height
   // prolog - simulate stack

   //iters := -width*sizeof(double);
   mov r10, width;
   shl r10, 3;
   imul r10, -1;

   // helper registers for the mt1, mt2 and dest pointers
   sub rcx, r10;

   vxorpd xmm0, xmm0, xmm0;
   vxorpd ymm1, ymm1, ymm1;

   mov r11, r9
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [rcx + rax];

           // mul add:
           vmovapd ymm3, [rcx + rax - 128];
           vmulpd ymm3, ymm3, ymm3;
           vaddpd ymm1, ymm1, ymm3;

           vmovapd ymm2, [rcx + rax - 96];
           vmulpd ymm2, ymm2, ymm2;
           vaddpd ymm1, ymm1, ymm2;

           vmovapd ymm3, [rcx + rax - 64];
           vmulpd ymm3, ymm3, ymm3;
           vaddpd ymm1, ymm1, ymm3;

           vmovapd ymm2, [rcx + rax - 32];
           vmulpd ymm2, ymm2, ymm2;
           vaddpd ymm1, ymm1, ymm2;
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           add rax, 16;
           jg @@addforxloop2end;

           vmovapd xmm3, [rcx + rax - 16];
           vmulpd xmm3, xmm3, xmm3;
           vaddpd xmm0, xmm0, xmm3;
       jmp @addforxloop2;

       @@addforxloop2end:

       sub rax, 16;
       jz @nextLine;

       vmovsd xmm3, [rcx + rax];
       vmulsd xmm3, xmm3, xmm3;
       vaddsd xmm0, xmm0, xmm3;

       @nextLine:

       // next line:
       add rcx, rdx;

   // loop y end
   dec r11;
   jnz @@addforyloop;

   // build result
   vextractf128 xmm2, ymm1, 1;
   vhaddpd xmm1, xmm1, xmm2;
   vaddpd xmm0, xmm0, xmm1;
   vhaddpd xmm0, xmm0, xmm0;

   // epilog claenup stack
   vzeroupper;
{$IFDEF FPC}
end;
{$ENDIF}
end;

function AVXMatrixElementwiseNorm2UnAligned(dest : PDouble; const LineWidth : TASMNativeInt; Width, height : TASMNativeInt) : double;
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

   // note: RCX = dest, RDX = destLineWidth, R8 = widh, R9 = height
   // prolog - simulate stack

   //iters := -width*sizeof(double);
   mov r10, width;
   shl r10, 3;
   imul r10, -1;

   // helper registers for the mt1, mt2 and dest pointers
   sub rcx, r10;

   vxorpd xmm0, xmm0, xmm0;
   vxorpd ymm1, ymm1, ymm1;

   mov r11, r9
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [rcx + rax];

           // mul add:
           vmovupd ymm3, [rcx + rax - 128];
           vmulpd ymm3, ymm3, ymm3;
           vaddpd ymm1, ymm1, ymm3;

           vmovupd ymm2, [rcx + rax - 96];
           vmulpd ymm2, ymm2, ymm2;
           vaddpd ymm1, ymm1, ymm2;

           vmovupd ymm3, [rcx + rax - 64];
           vmulpd ymm3, ymm3, ymm3;
           vaddpd ymm1, ymm1, ymm3;

           vmovupd ymm2, [rcx + rax - 32];
           vmulpd ymm2, ymm2, ymm2;
           vaddpd ymm1, ymm1, ymm2;
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           add rax, 16;
           jg @@addforxloop2end;

           vmovupd xmm3, [rcx + rax - 16];
           vmulpd xmm3, xmm3, xmm3;
           vaddpd xmm0, xmm0, xmm3;
       jmp @addforxloop2;

       @@addforxloop2end:

       sub rax, 16;
       jz @nextLine;

       vmovsd xmm3, [rcx + rax];
       vmulsd xmm3, xmm3, xmm3;
       vaddsd xmm0, xmm0, xmm3;

       @nextLine:

       // next line:
       add rcx, rdx;

   // loop y end
   dec r11;
   jnz @@addforyloop;

   // build result
   vextractf128 xmm2, ymm1, 1;
   vhaddpd xmm1, xmm1, xmm2;
   vaddpd xmm0, xmm0, xmm1;
   vhaddpd xmm0, xmm0, xmm0;

   // epilog claenup stack
   vzeroupper;
{$IFDEF FPC}
end;
{$ENDIF}
end;

procedure AVXMatrixNormalizeRowAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
var tmp : double;
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

   // note: RCX = dest, RDX = destLineWidth, R8 = widh, R9 = height
   // prolog - simulate stack

   //iters := -width*sizeof(double);
   mov r10, width;
   shl r10, 3;
   imul r10, -1;

   // helper registers for the mt1, mt2 and dest pointers
   sub rcx, r10;
   sub r8, r10;

   mov r11, height;
   @@addforyloop:
       vxorpd xmm0, xmm0, xmm0;
       vxorpd ymm1, ymm1, ymm1;

       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [rcx + rax];

           // mul add:
           vmovapd ymm3, [r8 + rax - 128];
           vmulpd ymm3, ymm3, ymm3;
           vaddpd ymm1, ymm1, ymm3;

           vmovapd ymm2, [r8 + rax - 96];
           vmulpd ymm2, ymm2, ymm2;
           vaddpd ymm1, ymm1, ymm2;

           vmovapd ymm3, [r8 + rax - 64];
           vmulpd ymm3, ymm3, ymm3;
           vaddpd ymm1, ymm1, ymm3;

           vmovapd ymm2, [r8 + rax - 32];
           vmulpd ymm2, ymm2, ymm2;
           vaddpd ymm1, ymm1, ymm2;
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           add rax, 16;
           jg @@addforxloop2end;

           vmovapd xmm3, [r8 + rax - 16];
           vmulpd xmm3, xmm3, xmm3;
           vaddpd xmm0, xmm0, xmm3;
       jmp @addforxloop2;

       @@addforxloop2end:

       sub rax, 16;
       jz @nextLine;

       vmovsd xmm3, [r8 + rax];
       vmulsd xmm3, xmm3, xmm3;
       vaddsd xmm0, xmm0, xmm3;

       @nextLine:

       // build result
       vextractf128 xmm2, ymm1, 1;
       vhaddpd xmm1, xmm1, xmm2;
       vaddpd xmm0, xmm0, xmm1;
       vhaddpd xmm0, xmm0, xmm0;

       // build result
       vsqrtsd xmm0, xmm0, xmm0;

       //1/sqrt(norm)
       lea rax, [rip + cOne];
       vmovsd xmm1, [rax];
       vdivsd xmm1, xmm1, xmm0;

       lea rax, tmp;
       vmovsd [rax], xmm1;
       vbroadcastsd ymm2, [rax];   // need avx2

       //  normalize
       mov rax, r10;
       @addforxloop3:
           add rax, 128;
           jg @loopEnd2;

           // prefetch data...
           // prefetch [r8 + rax];
           // prefetchw [rcx + rax];

           // mult:
           vmovapd ymm0, [r8 + rax - 128];
           vmulpd ymm0, ymm0, ymm2;
           vmovapd [rcx + rax - 128], ymm0;

           vmovapd ymm1, [r8 + rax - 96];
           vmulpd ymm1, ymm1, ymm2;
           vmovapd [rcx + rax - 96], ymm1;

           vmovapd ymm0, [r8 + rax - 64];
           vmulpd ymm0, ymm0, ymm2;
           vmovapd [rcx + rax - 64], ymm0;

           vmovapd ymm1, [r8 + rax - 32];
           vmulpd ymm1, ymm1, ymm2;
           vmovapd [rcx + rax - 32], ymm1;

       jmp @addforxloop3

       @loopEnd2:

       sub rax, 128;

       jz @nextLine2;

       @addforxloop4:
           add rax, 16;
           jg @addforxloop4end;

           vmovapd xmm0, [r8 + rax - 16];
           vmulpd xmm0, xmm0, xmm2;
           vmovapd [rcx + rax - 16], xmm0;
       jmp @addforxloop4;

       @addforxloop4end:
       sub rax, 16;
       jz @nextLine2;

       vmovsd xmm0, [r8 + rax];
       vmulsd xmm0, xmm0, xmm2;
       vmovsd [rcx + rax], xmm0;

       @nextLine2:

       // next line:
       add rcx, rdx;
       add r8, r9;

   // loop y end
   dec r11;
   jnz @@addforyloop;

   // epilog claenup stack
   vzeroupper;
{$IFDEF FPC}
end;
{$ENDIF}
end;

procedure AVXMatrixNormalizeRowUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
var tmp : double;
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

   // note: RCX = dest, RDX = destLineWidth, R8 = widh, R9 = height
   // prolog - simulate stack

   //iters := -width*sizeof(double);
   mov r10, width;
   shl r10, 3;
   imul r10, -1;

   // helper registers for the mt1, mt2 and dest pointers
   sub rcx, r10;
   sub r8, r10;

   mov r11, height;
   @@addforyloop:
       vxorpd xmm0, xmm0, xmm0;
       vxorpd ymm1, ymm1, ymm1;

       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [rcx + rax];

           // mul add:
           vmovupd ymm3, [r8 + rax - 128];
           vmulpd ymm3, ymm3, ymm3;
           vaddpd ymm1, ymm1, ymm3;

           vmovupd ymm2, [r8 + rax - 96];
           vmulpd ymm2, ymm2, ymm2;
           vaddpd ymm1, ymm1, ymm2;

           vmovupd ymm3, [r8 + rax - 64];
           vmulpd ymm3, ymm3, ymm3;
           vaddpd ymm1, ymm1, ymm3;

           vmovupd ymm2, [r8 + rax - 32];
           vmulpd ymm2, ymm2, ymm2;
           vaddpd ymm1, ymm1, ymm2;
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           add rax, 16;
           jg @@addforxloop2end;

           vmovupd xmm3, [r8 + rax - 16];
           vmulpd xmm3, xmm3, xmm3;
           vaddpd xmm0, xmm0, xmm3;
       jmp @addforxloop2;

       @@addforxloop2end:

       sub rax, 16;
       jz @nextLine;

       vmovsd xmm3, [r8 + rax];
       vmulsd xmm3, xmm3, xmm3;
       vaddsd xmm0, xmm0, xmm3;

       @nextLine:

       // build result
       vextractf128 xmm2, ymm1, 1;
       vhaddpd xmm1, xmm1, xmm2;
       vaddpd xmm0, xmm0, xmm1;
       vhaddpd xmm0, xmm0, xmm0;

       // build result
       vsqrtsd xmm0, xmm0, xmm0;

       //1/sqrt(norm)
       lea rax, [rip + cOne];
       vmovsd xmm1, [rax];
       vdivsd xmm1, xmm1, xmm0;

       lea rax, tmp;
       vmovsd [rax], xmm1;
       vbroadcastsd ymm2, [rax];  // need avx2

       //  normalize
       mov rax, r10;
       @addforxloop3:
           add rax, 128;
           jg @loopEnd2;

           // prefetch data...
           // prefetch [r8 + rax];
           // prefetchw [rcx + rax];

           // mult:
           vmovupd ymm0, [r8 + rax - 128];
           vmulpd ymm0, ymm0, ymm2;
           vmovupd [rcx + rax - 128], ymm0;

           vmovupd ymm1, [r8 + rax - 96];
           vmulpd ymm1, ymm1, ymm2;
           vmovupd [rcx + rax - 96], ymm1;

           vmovupd ymm0, [r8 + rax - 64];
           vmulpd ymm0, ymm0, ymm2;
           vmovupd [rcx + rax - 64], ymm0;

           vmovupd ymm1, [r8 + rax - 32];
           vmulpd ymm1, ymm1, ymm2;
           vmovupd [rcx + rax - 32], ymm1;

       jmp @addforxloop3

       @loopEnd2:

       sub rax, 128;

       jz @nextLine2;

       @addforxloop4:
           add rax, 16;
           jg @addforxloop4end;

           vmovupd xmm0, [r8 + rax - 16];
           vmulpd xmm0, xmm0, xmm2;
           vmovupd [rcx + rax - 16], xmm0;
       jmp @addforxloop4;

       @addforxloop4end:
       sub rax, 16;
       jz @nextLine2;

       vmovsd xmm0, [r8 + rax];
       vmulsd xmm0, xmm0, xmm2;
       vmovsd [rcx + rax], xmm0;

       @nextLine2:

       // next line:
       add rcx, rdx;
       add r8, r9;

   // loop y end
   dec r11;
   jnz @@addforyloop;

   // epilog claenup stack
   vzeroupper;
{$IFDEF FPC}
end;
{$ENDIF}
end;

procedure AVXMatrixNormalizeColumnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
var iRBX : TASMNativeInt;
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

   // prolog
   mov iRBX, rbx;

   // note: RCX = dest, RDX = destLineWidth, R8 = src, R9 = srcLineWidth
   xor r10, r10;
   sub r10, height;
   imul r10, r9;

   // helper registers for the mt1 and dest pointers
   sub r8, r10;

   // for x := 0 to width - 1:
   mov r11, Width;
   sub r11, 4;
   jl @@addforxloop4end;

   // 4 columns at once
   @@addforxloop4:
       vxorpd ymm1, ymm1, ymm1;

       // for y := 0 to height - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforyloop4:
           vmovapd ymm3, [r8 + rax];
           vmulpd ymm3, ymm3, ymm3;
           vaddpd ymm1, ymm1, ymm3;
       add rax, r9;
       jnz @addforyloop4;

       // normalization factor
       lea rax, [rip + cOne];
       vbroadcastsd ymm2, [rax];
       vsqrtpd ymm1, ymm1;
       vdivpd ymm0, ymm2, ymm1;

       // normalize
       mov rax, r10;
       xor rbx, rbx;
       @addforyloop4_2:
           vmovapd ymm1, [r8 + rax];
           vmulpd ymm1, ymm1, ymm0;
           vmovapd [rcx + rbx], ymm1;

       add rbx, rdx;
       add rax, r9;
       jnz @addforyloop4_2;

       // next columns:
       add rcx, 32;
       add r8, 32;

   // loop x end
   sub r11, 4;
   jge @@addforxloop4;

   @@addforxloop4end:

   add r11, 4;
   jz @@endProc;

   sub r11, 2;
   jl @@lastcolumn;

   vxorpd xmm1, xmm1, xmm1;

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov rax, r10;
   @addforyloop2:
       vmovapd xmm0, [r8 + rax];
       vmulpd xmm0, xmm0, xmm0;
       vaddpd xmm1, xmm1, xmm0;
   add rax, r9;
   jnz @addforyloop2;

   // normalization factor
   lea rax, [rip + cOne];
   vmovddup xmm2, [rax];
   vsqrtpd xmm1, xmm1;
   vdivpd xmm0, xmm2, xmm1;

   // normalize
   mov rax, r10;
   xor rbx, rbx;
   @addforyloop2_2:
      vmovapd xmm1, [r8 + rax];
      vmulpd xmm1, xmm1, xmm0;
      vmovapd [rcx + rbx], xmm1;
   add rbx, rdx;
   add rax, r9;
   jnz @addforyloop2_2;

   // next columns:
   add rcx, 16;
   add r8, 16;

   dec r11;
   jnz @@endProc;

   @@lastcolumn:

   vxorpd xmm1, xmm1, xmm1;

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov rax, r10;
   @addforyloop_1:
       vmovsd xmm0, [r8 + rax];
       vmulsd xmm0, xmm0, xmm0;
       vaddsd xmm1, xmm1, xmm0;
   add rax, r9;
   jnz @addforyloop_1;


   // build result
   lea rax, [rip + cOne];
   vmovsd xmm2, [rax];
   vsqrtsd xmm1, xmm1, xmm1;
   vdivsd xmm0, xmm2, xmm1;

   // normalize last column
   mov rax, r10;
   xor rbx, rbx;
   @addforyloop2_1:
      vmovsd xmm1, [r8 + rax];
      vmulsd xmm1, xmm1, xmm0;
      vmovsd [rcx + rbx], xmm1;
   add rbx, rdx;
   add rax, r9;
   jnz @addforyloop2_1;

   @@endProc:

   // epilog
   mov rbx, iRBX;
   vzeroupper;
{$IFDEF FPC}
end;
{$ENDIF}
end;


procedure AVXMatrixNormalizeColumnUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
var iRBX : TASMNativeInt;
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

   // prolog
   mov iRBX, rbx;

   // note: RCX = dest, RDX = destLineWidth, R8 = src, R9 = srcLineWidth
   xor r10, r10;
   sub r10, height;
   imul r10, r9;

   // helper registers for the mt1 and dest pointers
   sub r8, r10;

   // for x := 0 to width - 1:
   mov r11, Width;
   sub r11, 4;
   jl @@addforxloop4end;

   // 4 columns at once
   @@addforxloop4:
       vxorpd ymm1, ymm1, ymm1;

       // for y := 0 to height - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforyloop4:
           vmovupd ymm3, [r8 + rax];
           vmulpd ymm3, ymm3, ymm3;
           vaddpd ymm1, ymm1, ymm3;
       add rax, r9;
       jnz @addforyloop4;

       // normalization factor
       lea rax, [rip + cOne];
       vbroadcastsd ymm2, [rax];
       vsqrtpd ymm1, ymm1;
       vdivpd ymm0, ymm2, ymm1;

       // normalize
       mov rax, r10;
       xor rbx, rbx;
       @addforyloop4_2:
           vmovupd ymm1, [r8 + rax];
           vmulpd ymm1, ymm1, ymm0;
           vmovupd [rcx + rbx], ymm1;

       add rbx, rdx;
       add rax, r9;
       jnz @addforyloop4_2;

       // next columns:
       add rcx, 32;
       add r8, 32;

   // loop x end
   sub r11, 4;
   jge @@addforxloop4;

   @@addforxloop4end:

   add r11, 4;
   jz @@endProc;

   sub r11, 2;
   jl @@lastcolumn;

   vxorpd xmm1, xmm1, xmm1;

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov rax, r10;
   @addforyloop2:
       vmovupd xmm0, [r8 + rax];
       vmulpd xmm0, xmm0, xmm0;
       vaddpd xmm1, xmm1, xmm0;
   add rax, r9;
   jnz @addforyloop2;

   // normalization factor
   lea rax, [rip + cOne];
   vmovddup xmm2, [rax];
   vsqrtpd xmm1, xmm1;
   vdivpd xmm0, xmm2, xmm1;

   // normalize
   mov rax, r10;
   xor rbx, rbx;
   @addforyloop2_2:
      vmovupd xmm1, [r8 + rax];
      vmulpd xmm1, xmm1, xmm0;
      vmovupd [rcx + rbx], xmm1;
   add rbx, rdx;
   add rax, r9;
   jnz @addforyloop2_2;

   // next columns:
   add rcx, 16;
   add r8, 16;

   dec r11;
   jnz @@endProc;

   @@lastcolumn:

   vxorpd xmm1, xmm1, xmm1;

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov rax, r10;
   @addforyloop_1:
       vmovsd xmm0, [r8 + rax];
       vmulsd xmm0, xmm0, xmm0;
       vaddsd xmm1, xmm1, xmm0;
   add rax, r9;
   jnz @addforyloop_1;


   // build result
   lea rax, [rip + cOne];
   vmovsd xmm2, [rax];
   vsqrtsd xmm1, xmm1, xmm1;
   vdivsd xmm0, xmm2, xmm1;

   // normalize last column
   mov rax, r10;
   xor rbx, rbx;
   @addforyloop2_1:
      vmovsd xmm1, [r8 + rax];
      vmulsd xmm1, xmm1, xmm0;
      vmovsd [rcx + rbx], xmm1;
   add rbx, rdx;
   add rax, r9;
   jnz @addforyloop2_1;

   @@endProc:

   // epilog
   mov rbx, iRBX;
   vzeroupper;
{$IFDEF FPC}
end;
{$ENDIF}
end;

{$ENDIF}

end.
