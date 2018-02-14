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


unit AVXMatrixAddSubOperationsx64;

// ############################################################
// ##### Matrix addition/subtraction assembler optimized:
// ############################################################

interface

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF x64}

uses MatrixConst;

procedure AVXMatrixAddAligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
procedure AVXMatrixAddUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);

procedure AVXMatrixSubAligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
procedure AVXMatrixSubUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);

procedure AVXMatrixSubT(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; LineWidthB : TASMNativeInt; width, height : TASMNativeInt);
{$ENDIF}

implementation

{$IFDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$ENDIF}

procedure AVXMatrixAddAligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var iRBX, iR12 : TASMNativeInt;
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

   // note: RCX = dest, RDX = destLineWidth, R8 = mt1, R9 = mt2
   // prolog - simulate stack
   mov iRBX, rbx;
   mov iR12, r12;

   //iters := -width*sizeof(double);
   mov r10, width;
   shl r10, 3;
   imul r10, -1;

   mov r11, LineWidth1;
   mov r12, LineWidth2;

   // helper registers for the mt1, mt2 and dest pointers
   sub r8, r10;
   sub r9, r10;
   sub rcx, r10;

   // for y := 0 to height - 1:
   mov rbx, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [r8 + rax];
           // prefetch [r9 + rax];

           // addition:
           vmovapd ymm0, [r8 + rax - 128];
           vaddpd ymm0, ymm0, [r9 + rax - 128];

           vmovapd ymm1, [r8 + rax - 96];
           vaddpd ymm1, ymm1, [r9 + rax - 96];

           vmovapd ymm2, [r8 + rax - 64];
           vaddpd ymm2, ymm2, [r9 + rax - 64];

           vmovapd ymm3, [r8 + rax - 32];
           vaddpd ymm3, ymm3, [r9 + rax - 32];

           vmovapd [rcx + rax - 128], ymm0;
           vmovapd [rcx + rax - 96], ymm1;
           vmovapd [rcx + rax - 64], ymm2;
           vmovapd [rcx + rax - 32], ymm3;
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       add rax, 16;
       jg @@lastelem;

       @addforxloop2:
           vmovapd xmm0, [r8 + rax - 16];
           vaddpd xmm0, xmm0, [r9 + rax - 16];

           vmovapd [rcx + rax - 16], xmm0;
       add rax, 16;
       jle @addforxloop2;

       @@lastelem:

       sub rax, 16;
       jz @nextLine;

       vmovsd xmm0, [r8 - 8];
       vmovsd xmm1, [r9 - 8];
       vaddsd xmm0, xmm0, xmm1;
       vmovsd [rcx - 8], xmm0;

       @nextLine:

       // next line:
       add r8, r11;
       add r9, r12;
       add rcx, rdx;

   // loop y end
   dec rbx;
   jnz @@addforyloop;

   // epilog
   mov rbx, iRBX;
   mov r12, iR12;
   vzeroupper;
end;
{$IFDEF FPC}
end;
{$ENDIF}

procedure AVXMatrixAddUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var iRBX, iR12 : TASMNativeInt;
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

   // note: RCX = dest, RDX = destLineWidth, R8 = mt1, R9 = mt2
   // prolog - simulate stack
   mov iRBX, rbx;
   mov iR12, r12;

   //iters := -width*sizeof(double);
   mov r10, width;
   shl r10, 3;
   imul r10, -1;

   mov r11, LineWidth1;
   mov r12, LineWidth2;

   // helper registers for the mt1, mt2 and dest pointers
   sub r8, r10;
   sub r9, r10;
   sub rcx, r10;

   // for y := 0 to height - 1:
   mov rbx, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [r8 + rax];
           // prefetch [r9 + rax];

           // addition:
           vmovupd ymm0, [r8 + rax - 128];
           vmovupd ymm1, [r9 + rax - 128];
           vaddpd ymm0, ymm0, ymm1;
           vmovupd [rcx + rax - 128], ymm0;

           vmovupd ymm0, [r8 + rax - 96];
           vmovupd ymm1, [r9 + rax - 96];
           vaddpd ymm0, ymm0, ymm1;
           vmovupd [rcx + rax - 96], ymm0;

           vmovupd ymm0, [r8 + rax - 64];
           vmovupd ymm1, [r9 + rax - 64];
           vaddpd ymm0, ymm0, ymm1;
           vmovupd [rcx + rax - 64], ymm0;

           vmovupd ymm0, [r8 + rax - 32];
           vmovupd ymm1, [r9 + rax - 32];
           vaddpd ymm0, ymm0, ymm1;
           vmovupd [rcx + rax - 32], ymm0;

       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       add rax, 16;
       jg @@lastelem;

       @addforxloop2:
           vmovupd xmm0, [r8 + rax - 16];
           vmovupd xmm1, [r9 + rax - 16];
           vaddpd xmm0, xmm0, xmm1;

           vmovupd [rcx + rax - 16], xmm0;
       add rax, 16;
       jle @addforxloop2;

       @@lastelem:

       sub rax, 16;
       jz @nextLine;

       vmovsd xmm0, [r8 - 8];
       vmovsd xmm1, [r9 - 8];
       vaddsd xmm0, xmm0, xmm1;
       vmovsd [rcx - 8], xmm0;

       @nextLine:

       // next line:
       add r8, r11;
       add r9, r12;
       add rcx, rdx;

   // loop y end
   dec rbx;
   jnz @@addforyloop;

   // epilog
   mov rbx, iRBX;
   mov r12, iR12;
   vzeroupper;
end;
{$IFDEF FPC}
end;
{$ENDIF}

procedure AVXMatrixSubAligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var iRBX, iR12 : TASMNativeInt;
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

   // note: RCX = dest, RDX = destLineWidth, R8 = mt1, R9 = mt2
   // prolog - simulate stack
   mov iRBX, rbx;
   mov iR12, r12;

   //iters := -width*sizeof(double);
   mov r10, width;
   shl r10, 3;
   imul r10, -1;

   mov r11, LineWidth1;
   mov r12, LineWidth2;

   // helper registers for the mt1, mt2 and dest pointers
   sub r8, r10;
   sub r9, r10;
   sub rcx, r10;

   // for y := 0 to height - 1:
   mov rbx, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [r8 + rax];
           // prefetch [r9 + rax];

           // addition:
           vmovapd ymm0, [r8 + rax - 128];
           vsubpd ymm0, ymm0, [r9 + rax - 128];

           vmovapd ymm1, [r8 + rax - 96];
           vsubpd ymm1, ymm1, [r9 + rax - 96];

           vmovapd ymm2, [r8 + rax - 64];
           vsubpd ymm2, ymm2, [r9 + rax - 64];

           vmovapd ymm3, [r8 + rax - 32];
           vsubpd ymm3, ymm3, [r9 + rax - 32];

           vmovapd [rcx + rax - 128], ymm0;
           vmovapd [rcx + rax - 96], ymm1;
           vmovapd [rcx + rax - 64], ymm2;
           vmovapd [rcx + rax - 32], ymm3;
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       add rax, 16;
       jg @@lastelem;

       @addforxloop2:
           vmovapd xmm0, [r8 + rax - 16];
           vsubpd xmm0, xmm0, [r9 + rax - 16];

           vmovapd [rcx + rax - 16], xmm0;
       add rax, 16;
       jle @addforxloop2;

       @@lastelem:

       sub rax, 16;
       jz @nextLine;

       vmovsd xmm0, [r8 - 8];
       vmovsd xmm1, [r9 - 8];
       vsubsd xmm0, xmm0, xmm1;
       vmovsd [rcx - 8], xmm0;

       @nextLine:

       // next line:
       add r8, r11;
       add r9, r12;
       add rcx, rdx;

   // loop y end
   dec rbx;
   jnz @@addforyloop;

   // epilog
   mov rbx, iRBX;
   mov r12, iR12;
   vzeroupper;
end;
{$IFDEF FPC}
end;
{$ENDIF}

procedure AVXMatrixSubUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var iRBX, iR12 : TASMNativeInt;
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

   // note: RCX = dest, RDX = destLineWidth, R8 = mt1, R9 = mt2
   // prolog - simulate stack
   mov iRBX, rbx;
   mov iR12, r12;

   //iters := -width*sizeof(double);
   mov r10, width;
   shl r10, 3;
   imul r10, -1;

   mov r11, LineWidth1;
   mov r12, LineWidth2;

   // helper registers for the mt1, mt2 and dest pointers
   sub r8, r10;
   sub r9, r10;
   sub rcx, r10;

   // for y := 0 to height - 1:
   mov rbx, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [r8 + rax];
           // prefetch [r9 + rax];

           // addition:
           vmovupd ymm0, [r8 + rax - 128];
           vmovupd ymm1, [r9 + rax - 128];
           vsubpd ymm0, ymm0, ymm1;
           vmovupd [rcx + rax - 128], ymm0;

           vmovupd ymm0, [r8 + rax - 96];
           vmovupd ymm1, [r9 + rax - 96];
           vsubpd ymm0, ymm0, ymm1;
           vmovupd [rcx + rax - 96], ymm0;

           vmovupd ymm0, [r8 + rax - 64];
           vmovupd ymm1, [r9 + rax - 64];
           vsubpd ymm0, ymm0, ymm1;
           vmovupd [rcx + rax - 64], ymm0;

           vmovupd ymm0, [r8 + rax - 32];
           vmovupd ymm1, [r9 + rax - 32];
           vsubpd ymm0, ymm0, ymm1;
           vmovupd [rcx + rax - 32], ymm0;

       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       add rax, 16;
       jg @@lastelem;

       @addforxloop2:
           vmovupd xmm0, [r8 + rax - 16];
           vmovupd xmm1, [r9 + rax - 16];
           vsubpd xmm0, xmm0, xmm1;

           vmovupd [rcx + rax - 16], xmm0;
       add rax, 16;
       jle @addforxloop2;

       @@lastelem:

       sub rax, 16;
       jz @nextLine;

       vmovsd xmm0, [r8 - 8];
       vmovsd xmm1, [r9 - 8];
       vsubsd xmm0, xmm0, xmm1;
       vmovsd [rcx - 8], xmm0;

       @nextLine:

       // next line:
       add r8, r11;
       add r9, r12;
       add rcx, rdx;

   // loop y end
   dec rbx;
   jnz @@addforyloop;

   // epilog
   mov rbx, iRBX;
   mov r12, iR12;
   vzeroupper;
end;
{$IFDEF FPC}
end;
{$ENDIF}

procedure AVXMatrixSubT(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; LineWidthB : TASMNativeInt; width, height : TASMNativeInt);
var iRBX : TASMNativeInt;
{$IFDEF FPC}
begin
{$ENDIF}
// rcx : A, rdx : LineWidthA, r8 : B, r9 : LineWidthB;
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
   // maintain stack
   mov iRBX, rbx;

   // rax: iter := -width*sizeof(double)
   mov rcx, A;
   mov rax, width;
   imul rax, -8;
   sub rcx, rax;

   // for y := 0 to height - 1
   @@foryloop:
      mov r10, r8;
      mov rbx, rax;
              // for x := 0 to width - 1
      @@forxloop:
         vmovsd xmm0, [rcx + rbx];
         vmovsd xmm1, [r10];
         vsubsd xmm0, xmm0, xmm1;
         vmovsd [rcx + rbx], xmm0;

         add r10, r9;
      add rbx, 8;
      jnz @@forxloop;

      add rcx, rdx;
      add r8, 8;
   dec height;
   jnz @@foryloop;

   // epilog
   mov rbx, iRBX;
   vzeroupper;
end;
{$IFDEF FPC}
end;
{$ENDIF}


{$ENDIF}

end.
