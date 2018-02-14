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


unit AVXMatrixScaleOperationsx64;

interface

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF x64}

uses MatrixConst;

procedure AVXMatrixAddScaleAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);
procedure AVXMatrixAddScaleUnAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);

procedure AVXMatrixAddScaleAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);
procedure AVXMatrixAddScaleUnAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);

procedure AVXMatrixScaleAddAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);
procedure AVXMatrixScaleAddUnAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);

procedure AVXMatrixScaleAddAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);
procedure AVXMatrixScaleAddUnAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);

{$ENDIF}

implementation

{$IFDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$ENDIF}

procedure AVXMatrixAddScaleAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);
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

   // note: RCX = dest, RDX = destLineWidth, R8 = width, R9 = height
   // prolog - simulate stack

   //iters := -width*sizeof(double);
   mov r10, width;
   imul r10, -8;

   lea rax, dOffset;
   vbroadcastsd ymm3, [rax];
   lea rax, scale;
   vbroadcastsd ymm4, [rax];


   // helper registers for the mt1, mt2 and dest pointers
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

           // prefetchw data...
           // prefetchw [rcx + rax];

           // add mul
           vmovapd ymm0, [rcx + rax - 128];
           vaddpd ymm0, ymm0, ymm3;
           vmulpd ymm0, ymm0, ymm4;
           vmovapd [rcx + rax - 128], ymm0;

           vmovapd ymm2, [rcx + rax - 96];
           vaddpd ymm2, ymm2, ymm3;
           vmulpd ymm2, ymm2, ymm4;
           vmovapd [rcx + rax - 96], ymm2;

           vmovapd ymm0, [rcx + rax - 64];
           vaddpd ymm0, ymm0, ymm3;
           vmulpd ymm0, ymm0, ymm4;
           vmovapd [rcx + rax - 64], ymm0;

           vmovapd ymm2, [rcx + rax - 32];
           vaddpd ymm2, ymm2, ymm3;
           vmulpd ymm2, ymm2, ymm4;
           vmovapd [rcx + rax - 32], ymm2;

       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           vmovapd xmm0, [rcx + rax];
           vaddpd xmm0, xmm0, xmm3;
           vmulpd xmm0, xmm0, xmm4;

           vmovapd [rcx + rax], xmm0;
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // next line:
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

procedure AVXMatrixAddScaleUnAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);
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

   // note: RCX = dest, RDX = destLineWidth, R8 = width, R9 = height
   // prolog - simulate stack

   //iters := -width*sizeof(double);
   mov r10, width;
   imul r10, -8;

   lea rax, dOffset;
   vbroadcastsd ymm3, [rax];
   lea rax, scale;
   vbroadcastsd ymm4, [rax];

   // helper registers for the mt1, mt2 and dest pointers
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

           // prefetchw data...
           // prefetchw [rcx + rax];

           // add mul
           vmovupd ymm0, [rcx + rax - 128];
           vaddpd ymm0, ymm0, ymm3;
           vmulpd ymm0, ymm0, ymm4;
           vmovupd [rcx + rax - 128], ymm0;

           vmovupd ymm2, [rcx + rax - 96];
           vaddpd ymm2, ymm2, ymm3;
           vmulpd ymm2, ymm2, ymm4;
           vmovupd [rcx + rax - 96], ymm2;

           vmovupd ymm0, [rcx + rax - 64];
           vaddpd ymm0, ymm0, ymm3;
           vmulpd ymm0, ymm0, ymm4;
           vmovupd [rcx + rax - 64], ymm0;

           vmovupd ymm2, [rcx + rax - 32];
           vaddpd ymm2, ymm2, ymm3;
           vmulpd ymm2, ymm2, ymm4;
           vmovupd [rcx + rax - 32], ymm2;

       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           vmovupd xmm0, [rcx + rax];
           vaddpd xmm0, xmm0, xmm3;
           vmulpd xmm0, xmm0, xmm4;

           vmovupd [rcx + rax], xmm0;
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // next line:
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


procedure AVXMatrixAddScaleAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);
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

   // note: RCX = dest, RDX = destLineWidth, R8 = width, R9 = height
   // prolog - simulate stack

   //iters := -(width - 1)*sizeof(double);
   mov r10, width;
   dec r10;
   imul r10, -8;

   lea rax, dOffset;
   vbroadcastsd ymm3, [rax];
   lea rax, scale;
   vbroadcastsd ymm4, [rax];


   // helper registers for the mt1, mt2 and dest pointers
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

           // prefetchw data...
           // prefetchw [rcx + rax];

           // add mul
           vmovapd ymm0, [rcx + rax - 128];
           vaddpd ymm0, ymm0, ymm3;
           vmulpd ymm0, ymm0, ymm4;
           vmovapd [rcx + rax - 128], ymm0;

           vmovapd ymm2, [rcx + rax - 96];
           vaddpd ymm2, ymm2, ymm3;
           vmulpd ymm2, ymm2, ymm4;
           vmovapd [rcx + rax - 96], ymm2;

           vmovapd ymm0, [rcx + rax - 64];
           vaddpd ymm0, ymm0, ymm3;
           vmulpd ymm0, ymm0, ymm4;
           vmovapd [rcx + rax - 64], ymm0;

           vmovapd ymm2, [rcx + rax - 32];
           vaddpd ymm2, ymm2, ymm3;
           vmulpd ymm2, ymm2, ymm4;
           vmovapd [rcx + rax - 32], ymm2;

       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           vmovapd xmm0, [rcx + rax];
           vaddpd xmm0, xmm0, xmm3;
           vmulpd xmm0, xmm0, xmm4;

           vmovapd [rcx + rax], xmm0;
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last column:
       vmovsd xmm0, [rcx];
       vaddsd xmm0, xmm0, xmm3;
       vmulsd xmm0, xmm0, xmm4;
       vmovsd [rcx], xmm0;

       // next line:
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

procedure AVXMatrixAddScaleUnAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);
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

   // note: RCX = dest, RDX = destLineWidth, R8 = width, R9 = height
   // prolog - simulate stack

   //iters := -(width - 1)*sizeof(double);
   mov r10, width;
   dec r10;
   imul r10, -8;

   lea rax, dOffset;
   vbroadcastsd ymm3, [rax];
   lea rax, scale;
   vbroadcastsd ymm4, [rax];

   // helper registers for the mt1, mt2 and dest pointers
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

           // prefetchw data...
           // prefetchw [rcx + rax];

           // add mul
           vmovupd ymm0, [rcx + rax - 128];
           vaddpd ymm0, ymm0, ymm3;
           vmulpd ymm0, ymm0, ymm4;
           vmovupd [rcx + rax - 128], ymm0;

           vmovupd ymm2, [rcx + rax - 96];
           vaddpd ymm2, ymm2, ymm3;
           vmulpd ymm2, ymm2, ymm4;
           vmovupd [rcx + rax - 96], ymm2;

           vmovupd ymm0, [rcx + rax - 64];
           vaddpd ymm0, ymm0, ymm3;
           vmulpd ymm0, ymm0, ymm4;
           vmovupd [rcx + rax - 64], ymm0;

           vmovupd ymm2, [rcx + rax - 32];
           vaddpd ymm2, ymm2, ymm3;
           vmulpd ymm2, ymm2, ymm4;
           vmovupd [rcx + rax - 32], ymm2;

       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           vmovupd xmm0, [rcx + rax];
           vaddpd xmm0, xmm0, xmm3;
           vmulpd xmm0, xmm0, xmm4;

           vmovupd [rcx + rax], xmm0;
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last column:
       vmovsd xmm0, [rcx];
       vaddsd xmm0, xmm0, xmm3;
       vmulsd xmm0, xmm0, xmm4;
       vmovsd [rcx], xmm0;

       // next line:
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


procedure AVXMatrixScaleAddAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);
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

   // note: RCX = dest, RDX = destLineWidth, R8 = width, R9 = height
   // prolog - simulate stack

   //iters := -width*sizeof(double);
   mov r10, width;
   imul r10, -8;

   lea rax, dOffset;
   vbroadcastsd ymm3, [rax];
   lea rax, scale;
   vbroadcastsd ymm4, [rax];

   // helper registers for the mt1, mt2 and dest pointers
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

           // prefetchw data...
           // prefetchw [rcx + rax];

           // addition:
           vmovapd ymm0, [rcx + rax - 128];
           vmulpd ymm0, ymm0, ymm4;
           vaddpd ymm0, ymm0, ymm3;
           vmovapd [rcx + rax - 128], ymm0;

           vmovapd ymm2, [rcx + rax - 96];
           vmulpd ymm2, ymm2, ymm4;
           vaddpd ymm2, ymm2, ymm3;
           vmovapd [rcx + rax - 96], ymm2;

           vmovapd ymm0, [rcx + rax - 64];
           vmulpd ymm0, ymm0, ymm4;
           vaddpd ymm0, ymm0, ymm3;
           vmovapd [rcx + rax - 64], ymm0;

           vmovapd ymm2, [rcx + rax - 32];
           vmulpd ymm2, ymm2, ymm4;
           vaddpd ymm2, ymm2, ymm3;
           vmovapd [rcx + rax - 32], ymm2;

       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           vmovapd xmm0, [rcx + rax];
           vmulpd xmm0, xmm0, xmm4;
           vaddpd xmm0, xmm0, xmm3;

           vmovapd [rcx + rax], xmm0;
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // next line:
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

procedure AVXMatrixScaleAddUnAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);
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

   // note: RCX = dest, RDX = destLineWidth, R8 = width, R9 = height
   // prolog - simulate stack

   //iters := -width*sizeof(double);
   mov r10, width;
   imul r10, -8;

   lea rax, dOffset;
   vbroadcastsd ymm3, [rax];
   lea rax, scale;
   vbroadcastsd ymm4, [rax];

   // helper registers for the mt1, mt2 and dest pointers
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

           // prefetchw data...
           // prefetchw [rcx + rax];

           // addition:
           vmovupd ymm0, [rcx + rax - 128];
           vmulpd ymm0, ymm0, ymm4;
           vaddpd ymm0, ymm0, ymm3;
           vmovupd [rcx + rax - 128], ymm0;

           vmovupd ymm2, [rcx + rax - 96];
           vmulpd ymm2, ymm2, ymm4;
           vaddpd ymm2, ymm2, ymm3;
           vmovupd [rcx + rax - 96], ymm2;

           vmovupd ymm0, [rcx + rax - 64];
           vmulpd ymm0, ymm0, ymm4;
           vaddpd ymm0, ymm0, ymm3;
           vmovupd [rcx + rax - 64], ymm0;

           vmovupd ymm2, [rcx + rax - 32];
           vmulpd ymm2, ymm2, ymm4;
           vaddpd ymm2, ymm2, ymm3;
           vmovupd [rcx + rax - 32], ymm2;

       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           vmovupd xmm0, [rcx + rax];
           vmulpd xmm0, xmm0, xmm4;
           vaddpd xmm0, xmm0, xmm3;

           vmovupd [rcx + rax], xmm0;
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // next line:
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

procedure AVXMatrixScaleAddAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);
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

   // note: RCX = dest, RDX = destLineWidth, R8 = width, R9 = height
   // prolog - simulate stack

   //iters := -width*sizeof(double);
   mov r10, width;
   dec r10;
   imul r10, -8;

   lea rax, dOffset;
   vbroadcastsd ymm3, [rax];
   lea rax, scale;
   vbroadcastsd ymm4, [rax];

   // helper registers for the mt1, mt2 and dest pointers
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

           // prefetchw data...
           // prefetchw [rcx + rax];

           // addition:
           vmovapd ymm0, [rcx + rax - 128];
           vmulpd ymm0, ymm0, ymm4;
           vaddpd ymm0, ymm0, ymm3;
           vmovapd [rcx + rax - 128], ymm0;

           vmovapd ymm2, [rcx + rax - 96];
           vmulpd ymm2, ymm2, ymm4;
           vaddpd ymm2, ymm2, ymm3;
           vmovapd [rcx + rax - 96], ymm2;

           vmovapd ymm0, [rcx + rax - 64];
           vmulpd ymm0, ymm0, ymm4;
           vaddpd ymm0, ymm0, ymm3;
           vmovapd [rcx + rax - 64], ymm0;

           vmovapd ymm2, [rcx + rax - 32];
           vmulpd ymm2, ymm2, ymm4;
           vaddpd ymm2, ymm2, ymm3;
           vmovapd [rcx + rax - 32], ymm2;


       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           vmovapd xmm0, [rcx + rax];
           vmulpd xmm0, xmm0, xmm4;
           vaddpd xmm0, xmm0, xmm3;

           vmovapd [rcx + rax], xmm0;
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last column:
       vmovsd xmm0, [rcx];
       vmulsd xmm0, xmm0, xmm4;
       vaddsd xmm0, xmm0, xmm3;
       vmovsd [rcx], xmm0;

       // next line:
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

procedure AVXMatrixScaleAddUnAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);
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

   // note: RCX = dest, RDX = destLineWidth, R8 = width, R9 = height
   // prolog - simulate stack

   //iters := -width*sizeof(double);
   mov r10, width;
   dec r10;
   imul r10, -8;

   lea rax, dOffset;
   vbroadcastsd ymm3, [rax];
   lea rax, scale;
   vbroadcastsd ymm4, [rax];

   // helper registers for the mt1, mt2 and dest pointers
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

           // prefetchw data...
           // prefetchw [rcx + rax];

           // addition:
           vmovupd ymm0, [rcx + rax - 128];
           vmulpd ymm0, ymm0, ymm4;
           vaddpd ymm0, ymm0, ymm3;
           vmovupd [rcx + rax - 128], ymm0;

           vmovupd ymm2, [rcx + rax - 96];
           vmulpd ymm2, ymm2, ymm4;
           vaddpd ymm2, ymm2, ymm3;
           vmovupd [rcx + rax - 96], ymm2;

           vmovupd ymm0, [rcx + rax - 64];
           vmulpd ymm0, ymm0, ymm4;
           vaddpd ymm0, ymm0, ymm3;
           vmovupd [rcx + rax - 64], ymm0;

           vmovupd ymm2, [rcx + rax - 32];
           vmulpd ymm2, ymm2, ymm4;
           vaddpd ymm2, ymm2, ymm3;
           vmovupd [rcx + rax - 32], ymm2;

       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           vmovupd xmm0, [rcx + rax];
           vmulpd xmm0, xmm0, xmm4;
           vaddpd xmm0, xmm0, xmm3;

           vmovupd [rcx + rax], xmm0;
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last column:
       vmovsd xmm0, [rcx];
       vmulsd xmm0, xmm0, xmm4;
       vaddsd xmm0, xmm0, xmm3;
       vmovsd [rcx], xmm0;

       // next line:
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
{$ENDIF}

end.
