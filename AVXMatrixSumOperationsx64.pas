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


unit AVXMatrixSumOperationsx64;

interface

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF x64}

uses MatrixConst;

procedure AVXMatrixSumRowAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
procedure AVXMatrixSumRowUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);

procedure AVXMatrixSumColumnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
procedure AVXMatrixSumColumnUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);

{$ENDIF}

implementation

{$IFDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$ENDIF}

procedure AVXMatrixSumRowAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
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

   // note: RCX = dest, RDX = destLineWidth, R8 = src, R9 = srcLineWidth

   // prolog

   // iters := -width*sizeof(double)
   mov r10, width;
   shl r10, 3;
   imul r10, -1;

   // helper registers for the mt1, mt2 and dest pointers
   sub r8, r10;

   // for y := 0 to height - 1:
   mov r11, Height;
   @@addforyloop:
       vxorpd ymm0, ymm0, ymm0;
       vxorpd ymm1, ymm1, ymm1;

       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;
           // prefetch data...
           // prefetch [r8 + rax];

           // addition:
           vaddpd ymm0, ymm0, [r8 + rax - 128];
           vaddpd ymm1, ymm1, [r8 + rax - 96];
           vaddpd ymm0, ymm0, [r8 + rax - 64];
           vaddpd ymm1, ymm1, [r8 + rax - 32];
       jmp @addforxloop

       @loopEnd:

       vaddpd ymm0, ymm0, ymm1;
       vextractf128 xmm2, ymm0, 1;
       vhaddpd xmm0, xmm0, xmm2;

       sub rax, 128;

       jz @buildRes;

       @addforxloop2:
           add rax, 16;
           jg @@addforxloop2end;

           vaddpd xmm0, xmm0, [r8 + rax - 16];
       jmp @addforxloop2;

       @@addforxloop2end:

       sub rax, 16;
       jz @buildRes;

       vaddsd xmm0, xmm0, [r8 + rax];

       @buildRes:

       // build result
       vhaddpd xmm0, xmm0, xmm0;

       // write result
       vmovsd [rcx], xmm0;

       // next line:
       add r8, r9;
       add rcx, rdx;

   // loop y end
   dec r11;
   jnz @@addforyloop;

   // epilog
   vzeroupper;
{$IFDEF FPC}
end;
{$ENDIF}
end;

procedure AVXMatrixSumRowUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
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

   // note: RCX = dest, RDX = destLineWidth, R8 = src, R9 = srcLineWidth

   // prolog

   // iters := -width*sizeof(double)
   mov r10, width;
   shl r10, 3;
   imul r10, -1;

   // helper registers for the mt1, mt2 and dest pointers
   sub r8, r10;

   // for y := 0 to height - 1:
   mov r11, Height;
   @@addforyloop:
       vxorpd ymm0, ymm0, ymm0;
       vxorpd ymm1, ymm1, ymm1;

       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;
           // prefetch data...
           // prefetch [r8 + rax];

           // addition:
           vmovupd ymm2, [r8 + rax - 128];
           vaddpd ymm0, ymm0, ymm2;
           vmovupd ymm2, [r8 + rax - 96];
           vaddpd ymm1, ymm1, ymm2;
           vmovupd ymm2, [r8 + rax - 64];
           vaddpd ymm0, ymm0, ymm2;
           vmovupd ymm2, [r8 + rax - 32];
           vaddpd ymm1, ymm1, ymm2;
       jmp @addforxloop

       @loopEnd:

       vaddpd ymm0, ymm0, ymm1;
       vextractf128 xmm2, ymm0, 1;
       vhaddpd xmm0, xmm0, xmm2;

       sub rax, 128;

       jz @buildRes;

       @addforxloop2:
           add rax, 16;
           jg @@addforxloop2end;

           vmovupd xmm2, [r8 + rax - 16];
           vaddpd xmm0, xmm0, xmm2;
       jmp @addforxloop2;

       @@addforxloop2end:

       sub rax, 16;
       jz @buildRes;

       vaddsd xmm0, xmm0, [r8 + rax];

       @buildRes:

       // build result
       vhaddpd xmm0, xmm0, xmm0;

       // write result
       vmovsd [rcx], xmm0;

       // next line:
       add r8, r9;
       add rcx, rdx;

   // loop y end
   dec r11;
   jnz @@addforyloop;

   // epilog
   vzeroupper;
{$IFDEF FPC}
end;
{$ENDIF}
end;

procedure AVXMatrixSumColumnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
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

   // note: RCX = dest, RDX = destLineWidth, R8 = src, R9 = srcLineWidth
   xor r10, r10;
   sub r10, height;
   imul r10, srcLineWidth;

   // helper registers for the mt1, mt2 and dest pointers
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
           vaddpd ymm1, ymm1, [r8 + rax];
       add rax, r9;
       jnz @addforyloop4;

       // build result
       vmovapd [rcx], ymm1;

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
       vaddpd xmm1, xmm1, [r8 + rax];
   add rax, r9;
   jnz @addforyloop2;

   // build result
   vmovapd [rcx], xmm1;

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
   @addforyloop:
       vaddsd xmm1, xmm1, [r8 + rax];
   add rax, r9;
   jnz @addforyloop;

   // build result
   vmovsd [rcx], xmm1;

   @@endProc:
   vzeroupper;
{$IFDEF FPC}
end;
{$ENDIF}
end;

procedure AVXMatrixSumColumnUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
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

   // note: RCX = dest, RDX = destLineWidth, R8 = src, R9 = srcLineWidth
   xor r10, r10;
   sub r10, height;
   imul r10, srcLineWidth;

   // helper registers for the mt1, mt2 and dest pointers
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
           vmovupd ymm0, [r8 + rax];
           vaddpd ymm1, ymm1, ymm0;
       add rax, r9;
       jnz @addforyloop4;

       // build result
       vmovupd [rcx], ymm1;

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
       vaddpd xmm1, xmm1, [r8 + rax];
   add rax, r9;
   jnz @addforyloop2;

   // build result
   vmovupd [rcx], xmm1;

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
   @addforyloop:
       vaddsd xmm1, xmm1, [r8 + rax];
   add rax, r9;
   jnz @addforyloop;

   // build result
   vmovsd [rcx], xmm1;

   @@endProc:
   vzeroupper;
{$IFDEF FPC}
end;
{$ENDIF}
end;

{$ENDIF}

end.
