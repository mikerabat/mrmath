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


unit AVXMoveOperationsx64;

// #################################################
// #### SSE optimized move oprationes
// #################################################

interface

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF x64}

uses MatrixConst;

procedure AVXMatrixCopyAligned(Dest : PDouble; const destLineWidth : TASMNativeInt; src : PDouble; const srcLineWidth : TASMNativeInt; Width, Height : TASMNativeInt);
procedure AVXMatrixCopyUnAligned(Dest : PDouble; const destLineWidth : TASMNativeInt; src : PDouble; const srcLineWidth : TASMNativeInt; Width, Height : TASMNativeInt);

procedure AVXRowSwapAligned(A, B : PDouble; width : TASMNativeInt);
procedure AVXRowSwapUnAligned(A, B : PDouble; width : TASMNativeInt);

procedure AVXInitMemAligned(A : PDouble; NumBytes : TASMNativeInt; const Value : double);

{$ENDIF}

implementation

{$IFDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$ENDIF}

// uses non temporal moves so the cache is not poisned
// rcx = A, rdx = NumBytes;
procedure AVXInitMemAligned(A : PDouble; NumBytes : TASMNativeInt; const Value : double);
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

   lea rax, Value;
   vbroadcastsd ymm1, [rax];

   imul rdx, -1;
   sub rcx, rdx;

   @@loopUnrolled:
      add rdx, 128;
      jg @@loopUnrolledEnd;

      vmovntdq [rcx + rdx - 128], ymm1;
      vmovntdq [rcx + rdx - 96], ymm1;
      vmovntdq [rcx + rdx - 64], ymm1;
      vmovntdq [rcx + rdx - 32], ymm1;
   jmp @@loopUnrolled;

   @@loopUnrolledEnd:

   sub rdx, 256;
   jz @@exitProc;

   add rdx, 32;
   jg @@beginloop3;

   @@loop2:
      vmovntdq [rcx + rdx - 32], xmm1;
      vmovntdq [rcx + rdx - 16], xmm1;
   add rdx, 32;
   jl @@loop2;

   @@beginloop3:
   sub rdx, 32;
   jz @@exitProc;

   @@loop3:
      vmovsd [rcx + rdx], xmm1;
      add rdx, 8;
   jnz @@loop3;

   @@exitProc:

   vzeroupper;
end;
{$IFDEF FPC}
end;
{$ENDIF}

procedure AVXRowSwapAligned(A, B : PDouble; width : TASMNativeInt);
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

   // note: RCX = a, RDX = b, R8 = width
   imul r8, -8;

   sub rcx, r8;
   sub rdx, r8;

   @unrolloop:
     add r8, 128;
     jg @unrolloopend;

     // prefetch data...
     // prefetchw [rcx + r10];
     // prefetchw [rdx + r10];

     vmovdqa ymm0, [rcx + r8 - 128];
     vmovdqa ymm1, [rdx + r8 - 128];

     vmovdqa [rcx + r8 - 128], ymm1;
     vmovdqa [rdx + r8 - 128], ymm0;

     vmovdqa ymm2, [rcx + r8 - 96];
     vmovdqa ymm3, [rdx + r8 - 96];

     vmovdqa [rcx + r8 - 96], ymm3;
     vmovdqa [rdx + r8 - 96], ymm2;

     vmovdqa ymm0, [rcx + r8 - 64];
     vmovdqa ymm1, [rdx + r8 - 64];

     vmovdqa [rcx + r8 - 64], ymm1;
     vmovdqa [rdx + r8 - 64], ymm0;

     vmovdqa ymm2, [rcx + r8 - 32];
     vmovdqa ymm3, [rdx + r8 - 32];

     vmovdqa [rcx + r8 - 32], ymm3;
     vmovdqa [rdx + r8 - 32], ymm2;
   jmp @unrolloop;
   @unrolloopend:

   sub r8, 128;
   jz @endfunc;

   @loop2:
     add r8, 16;
     jg @loop2End;

     vmovdqa xmm0, [rcx + r8 - 16];
     vmovdqa xmm1, [rdx + r8 - 16];

     vmovdqa [rcx + r8 - 16], xmm1;
     vmovdqa [rdx + r8 - 16], xmm0;
   jmp @loop2;

   @loop2End:
   sub r8, 16;

   jz @endfunc;

   vmovsd xmm0, [rcx + r8];
   vmovsd xmm1, [rdx + r8];

   vmovsd [rcx + r8], xmm1;
   vmovsd [rdx + r8], xmm0;

   @endfunc:

   vzeroupper;
{$IFDEF FPC}
end;
{$ENDIF}
end;

procedure AVXRowSwapUnAligned(A, B : PDouble; width : TASMNativeInt);
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

   // note: RCX = a, RDX = b, R8 = width
   shl r8, 3;
   imul r8, -1;

   sub rcx, r8;
   sub rdx, r8;

   @unrolloop:
     add r8, 128;
     jg @unrolloopend;

     // prefetch data...
     // prefetchw [rcx + r10];
     // prefetchw [rdx + r10];

     vmovdqu ymm0, [rcx + r8 - 128];
     vmovdqu ymm1, [rdx + r8 - 128];

     vmovdqu [rcx + r8 - 128], ymm1;
     vmovdqu [rdx + r8 - 128], ymm0;

     vmovdqu ymm2, [rcx + r8 - 96];
     vmovdqu ymm3, [rdx + r8 - 96];

     vmovdqu [rcx + r8 - 96], ymm3;
     vmovdqu [rdx + r8 - 96], ymm2;

     vmovdqu ymm0, [rcx + r8 - 64];
     vmovdqu ymm1, [rdx + r8 - 64];

     vmovdqu [rcx + r8 - 64], ymm1;
     vmovdqu [rdx + r8 - 64], ymm0;

     vmovdqu ymm2, [rcx + r8 - 32];
     vmovdqu ymm3, [rdx + r8 - 32];

     vmovdqu [rcx + r8 - 32], ymm3;
     vmovdqu [rdx + r8 - 32], ymm2;
   jmp @unrolloop;
   @unrolloopend:

   sub r8, 128;
   jz @endfunc;

   @loop2:
     add r8, 16;
     jg @loop2End;

     vmovdqu xmm0, [rcx + r8 - 16];
     vmovdqu xmm1, [rdx + r8 - 16];

     vmovdqu [rcx + r8 - 16], xmm1;
     vmovdqu [rdx + r8 - 16], xmm0;
   jmp @loop2;

   @loop2End:
   sub r8, 16;

   jz @endfunc;

   vmovsd xmm0, [rcx + r8];
   vmovsd xmm1, [rdx + r8];

   vmovsd [rcx + r8], xmm1;
   vmovsd [rdx + r8], xmm0;

   @endfunc:

   vzeroupper;
{$IFDEF FPC}
end;
{$ENDIF}
end;

procedure AVXMatrixCopyAligned(Dest : PDouble; const destLineWidth : TASMNativeInt; src : PDouble; const srcLineWidth : TASMNativeInt; Width, Height : TASMNativeInt);
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
   //iters := -width*sizeof(double);
   mov r10, width;
   shl r10, 3;
   imul r10, -1;

   // helper registers for the mt1, mt2 and dest pointers
   sub r8, r10;
   sub rcx, r10;

   // for y := 0 to height - 1:
   mov r11, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [r8 + rax];
           // prefetchw [rcx + rax];

           // move:
           vmovdqa ymm0, [r8 + rax - 128];
           vmovdqa [rcx + rax - 128], ymm0;

           vmovdqa ymm1, [r8 + rax - 96];
           vmovdqa [rcx + rax - 96], ymm1;

           vmovdqa ymm2, [r8 + rax - 64];
           vmovdqa [rcx + rax - 64], ymm2;

           vmovdqa ymm3, [r8 + rax - 32];
           vmovdqa [rcx + rax - 32], ymm3;

       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
          add rax, 16;
          jg @loopEnd2;

          vmovdqa xmm0, [r8 + rax - 16];
          vmovdqa [rcx + rax - 16], xmm0;
       jmp @addforxloop2;

       @loopEnd2:
       sub rax, 16;

       jz @nextLine;

       // last element
       vmovsd xmm0, [r8 + rax];
       vmovsd [rcx + rax], xmm0;

       @nextLine:

       // next line:
       add r8, r9;
       add rcx, rdx;

   // loop y end
   dec r11;
   jnz @@addforyloop;

   vzeroupper;
end;
{$IFDEF FPC}
end;
{$ENDIF}

procedure AVXMatrixCopyUnAligned(Dest : PDouble; const destLineWidth : TASMNativeInt; src : PDouble; const srcLineWidth : TASMNativeInt; Width, Height : TASMNativeInt);
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
   //iters := -width*sizeof(double);
   mov r10, width;
   shl r10, 3;
   imul r10, -1;

   // helper registers for the mt1, mt2 and dest pointers
   sub r8, r10;
   sub rcx, r10;

   // for y := 0 to height - 1:
   mov r11, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [r8 + rax];
           // prefetchw [rcx + rax];

           // move:
           vmovdqu ymm0, [r8 + rax - 128];
           vmovdqu [rcx + rax - 128], ymm0;

           vmovdqu ymm1, [r8 + rax - 96];
           vmovdqu [rcx + rax - 96], ymm1;

           vmovdqu ymm2, [r8 + rax - 64];
           vmovdqu [rcx + rax - 64], ymm2;

           vmovdqu ymm3, [r8 + rax - 32];
           vmovdqu [rcx + rax - 32], ymm3;

       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
          add rax, 16;
          jg @loopEnd2;

          vmovdqu xmm0, [r8 + rax - 16];
          vmovdqu [rcx + rax - 16], xmm0;
       jmp @addforxloop2;

       @loopEnd2:
       sub rax, 16;

       jz @nextLine;

       // last element
       vmovsd xmm0, [r8 + rax];
       vmovsd [rcx + rax], xmm0;

       @nextLine:

       // next line:
       add r8, r9;
       add rcx, rdx;

   // loop y end
   dec r11;
   jnz @@addforyloop;

   vzeroupper;
end;
{$IFDEF FPC}
end;
{$ENDIF}

{$ENDIF}

end.
