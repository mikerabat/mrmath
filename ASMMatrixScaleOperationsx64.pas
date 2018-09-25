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


unit ASMMatrixScaleOperationsx64;

interface

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF x64}

uses MatrixConst;

procedure ASMMatrixAddScaleAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; dOffset, Scale : double); {$IFDEF FPC}assembler;{$ENDIF}
procedure ASMMatrixAddScaleUnAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; dOffset, Scale : double); {$IFDEF FPC}assembler;{$ENDIF}

procedure ASMMatrixAddScaleAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; dOffset, Scale : double); {$IFDEF FPC}assembler;{$ENDIF}
procedure ASMMatrixAddScaleUnAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; dOffset, Scale : double); {$IFDEF FPC}assembler;{$ENDIF}

procedure ASMMatrixScaleAddAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; dOffset, Scale : double); {$IFDEF FPC}assembler;{$ENDIF}
procedure ASMMatrixScaleAddUnAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; dOffset, Scale : double); {$IFDEF FPC}assembler;{$ENDIF}

procedure ASMMatrixScaleAddAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; dOffset, Scale : double); {$IFDEF FPC}assembler;{$ENDIF}
procedure ASMMatrixScaleAddUnAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; dOffset, Scale : double); {$IFDEF FPC}assembler;{$ENDIF}

{$ENDIF}

implementation

{$IFDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$S-} {$ENDIF}

procedure ASMMatrixAddScaleAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; dOffset, Scale : double); {$IFDEF FPC}assembler;{$ENDIF}
var dXMM6, dXMM7 : Array[0..1] of double;
asm
   movupd dXMM6, xmm6;
   movupd dXMM7, xmm7;

   movddup xmm6, dOffset;
   movddup xmm7, Scale;
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

   //iters := -width*sizeof(double);
   imul r8, -8;

   // helper registers for the mt1, mt2 and dest pointers
   sub rcx, r8;

   // for y := 0 to height - 1:
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r8;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // prefetchw data...
           // prefetchw [rcx + rax];

           // add mul
           movapd xmm0, [rcx + rax - 128];
           addpd xmm0, xmm6;
           mulpd xmm0, xmm7;
           movapd [rcx + rax - 128], xmm0;

           movapd xmm1, [rcx + rax - 112];
           addpd xmm1, xmm6;
           mulpd xmm1, xmm7;
           movapd [rcx + rax - 112], xmm1;

           movapd xmm2, [rcx + rax - 96];
           addpd xmm2, xmm6;
           mulpd xmm2, xmm7;
           movapd [rcx + rax - 96], xmm2;

           movapd xmm3, [rcx + rax - 80];
           addpd xmm3, xmm6;
           mulpd xmm3, xmm7;
           movapd [rcx + rax - 80], xmm3;

           movapd xmm0, [rcx + rax - 64];
           addpd xmm0, xmm6;
           mulpd xmm0, xmm7;
           movapd [rcx + rax - 64], xmm0;

           movapd xmm1, [rcx + rax - 48];
           addpd xmm1, xmm6;
           mulpd xmm1, xmm7;
           movapd [rcx + rax - 48], xmm1;

           movapd xmm2, [rcx + rax - 32];
           addpd xmm2, xmm6;
           mulpd xmm2, xmm7;
           movapd [rcx + rax - 32], xmm2;

           movapd xmm3, [rcx + rax - 16];
           addpd xmm3, xmm6;
           mulpd xmm3, xmm7;
           movapd [rcx + rax - 16], xmm3;

       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           movapd xmm0, [rcx + rax];
           addpd xmm0, xmm6;
           mulpd xmm0, xmm7;

           movapd [rcx + rax], xmm0;
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // next line:
       add rcx, rdx;

   // loop y end
   dec r9;
   jnz @@addforyloop;

   // epilog
   movupd xmm6, dXMM6;
   movupd xmm7, dXMM7;
end;

procedure ASMMatrixAddScaleUnAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; dOffset, Scale : double); {$IFDEF FPC}assembler;{$ENDIF}
var dXMM6, dXMM7 : Array[0..1] of double;
asm
   movupd dXMM6, xmm6;
   movupd dXMM7, xmm7;
   movddup xmm6, dOffset;
   movddup xmm7, scale;
   
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

   //iters := -width*sizeof(double);
   imul r8, -8;

   // helper registers for the mt1, mt2 and dest pointers
   sub rcx, r8;

   // for y := 0 to height - 1:
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r8;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // add mul:
           movupd xmm0, [rcx + rax - 128];
           addpd xmm0, xmm6;
           mulpd xmm0, xmm7;
           movupd [rcx + rax - 128], xmm0;

           movupd xmm1, [rcx + rax - 112];
           addpd xmm1, xmm6;
           mulpd xmm1, xmm7;
           movupd [rcx + rax - 112], xmm1;

           movupd xmm2, [rcx + rax - 96];
           addpd xmm2, xmm6;
           mulpd xmm2, xmm7;
           movupd [rcx + rax - 96], xmm2;

           movupd xmm3, [rcx + rax - 80];
           addpd xmm3, xmm6;
           mulpd xmm3, xmm7;
           movupd [rcx + rax - 80], xmm3;

           movupd xmm0, [rcx + rax - 64];
           addpd xmm0, xmm6;
           mulpd xmm0, xmm7;
           movupd [rcx + rax - 64], xmm0;

           movupd xmm1, [rcx + rax - 48];
           addpd xmm1, xmm6;
           mulpd xmm1, xmm7;
           movupd [rcx + rax - 48], xmm1;

           movupd xmm2, [rcx + rax - 32];
           addpd xmm2, xmm6;
           mulpd xmm2, xmm7;
           movupd [rcx + rax - 32], xmm2;

           movupd xmm3, [rcx + rax - 16];
           addpd xmm3, xmm6;
           mulpd xmm3, xmm7;
           movupd [rcx + rax - 16], xmm3;

       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           movupd xmm0, [rcx + rax];
           addpd xmm0, xmm6;
           mulpd xmm0, xmm7;

           movupd [rcx + rax], xmm0;
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // next line:
       add rcx, rdx;

   // loop y end
   dec r9;
   jnz @@addforyloop;

   // epilog
   movupd xmm6, dXMM6;
   movupd xmm7, dXMM7;
end;


procedure ASMMatrixAddScaleAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; dOffset, Scale : double); {$IFDEF FPC}assembler;{$ENDIF}
var dXMM6, dXMM7 : Array[0..1] of double;
asm
   movupd dXMM6, xmm6;
   movupd dXMM7, xmm7;
   movddup xmm6, dOffset;
   movddup xmm7, scale;
   
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

   //iters := -(width - 1)*sizeof(double);
   dec r8;
   imul r8, -8;

   // helper registers for the mt1, mt2 and dest pointers
   sub rcx, r8;

   // for y := 0 to height - 1:
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r8;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // prefetchw data...
           // prefetchw [rcx + rax];

           // add mul:
           movapd xmm0, [rcx + rax - 128];
           addpd xmm0, xmm6;
           mulpd xmm0, xmm7;
           movapd [rcx + rax - 128], xmm0;

           movapd xmm1, [rcx + rax - 112];
           addpd xmm1, xmm6;
           mulpd xmm1, xmm7;
           movapd [rcx + rax - 112], xmm1;

           movapd xmm2, [rcx + rax - 96];
           addpd xmm2, xmm6;
           mulpd xmm2, xmm7;
           movapd [rcx + rax - 96], xmm2;

           movapd xmm3, [rcx + rax - 80];
           addpd xmm3, xmm6;
           mulpd xmm3, xmm7;
           movapd [rcx + rax - 80], xmm3;

           movapd xmm0, [rcx + rax - 64];
           addpd xmm0, xmm6;
           mulpd xmm0, xmm7;
           movapd [rcx + rax - 64], xmm0;

           movapd xmm1, [rcx + rax - 48];
           addpd xmm1, xmm6;
           mulpd xmm1, xmm7;
           movapd [rcx + rax - 48], xmm1;

           movapd xmm2, [rcx + rax - 32];
           addpd xmm2, xmm6;
           mulpd xmm2, xmm7;
           movapd [rcx + rax - 32], xmm2;

           movapd xmm3, [rcx + rax - 16];
           addpd xmm3, xmm6;
           mulpd xmm3, xmm7;
           movapd [rcx + rax - 16], xmm3;

       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           movapd xmm0, [rcx + rax];
           addpd xmm0, xmm6;
           mulpd xmm0, xmm7;

           movapd [rcx + rax], xmm0;
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last column:
       movsd xmm0, [rcx];
       addsd xmm0, xmm6;
       mulsd xmm0, xmm7;
       movsd [rcx], xmm0;

       // next line:
       add rcx, rdx;

   // loop y end
   dec r9;
   jnz @@addforyloop;

   // epilog
   movupd xmm6, dXMM6;
   movupd xmm7, dXMM7;
end;

procedure ASMMatrixAddScaleUnAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; dOffset, Scale : double); {$IFDEF FPC}assembler;{$ENDIF}
var dXMM6, dXMM7 : Array[0..1] of double;
asm
   movupd dXMM6, xmm6;
   movupd dXMM7, xmm7;
   movddup xmm6, dOffset;
   movddup xmm7, scale;

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

   // note: RCX = dest, RDX = destLineWidth, R8 = width, R9 = height
   // prolog - simulate stack


   //iters := -(width - 1)*sizeof(double);
   dec r8;
   imul r8, -8;

   // helper registers for the mt1, mt2 and dest pointers
   sub rcx, r8;

   // for y := 0 to height - 1:
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r8;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // add mul:
           movupd xmm0, [rcx + rax - 128];
           addpd xmm0, xmm6;
           mulpd xmm0, xmm7;
           movupd [rcx + rax - 128], xmm0;

           movupd xmm1, [rcx + rax - 112];
           addpd xmm1, xmm6;
           mulpd xmm1, xmm7;
           movupd [rcx + rax - 112], xmm1;

           movupd xmm2, [rcx + rax - 96];
           addpd xmm2, xmm6;
           mulpd xmm2, xmm7;
           movupd [rcx + rax - 96], xmm2;

           movupd xmm3, [rcx + rax - 80];
           addpd xmm3, xmm6;
           mulpd xmm3, xmm7;
           movupd [rcx + rax - 80], xmm3;

           movupd xmm0, [rcx + rax - 64];
           addpd xmm0, xmm6;
           mulpd xmm0, xmm7;
           movupd [rcx + rax - 64], xmm0;

           movupd xmm1, [rcx + rax - 48];
           addpd xmm1, xmm6;
           mulpd xmm1, xmm7;
           movupd [rcx + rax - 48], xmm1;

           movupd xmm2, [rcx + rax - 32];
           addpd xmm2, xmm6;
           mulpd xmm2, xmm7;
           movupd [rcx + rax - 32], xmm2;

           movupd xmm3, [rcx + rax - 16];
           addpd xmm3, xmm6;
           mulpd xmm3, xmm7;
           movupd [rcx + rax - 16], xmm3;

       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           movupd xmm0, [rcx + rax];
           addpd xmm0, xmm6;
           mulpd xmm0, xmm7;

           movupd [rcx + rax], xmm0;
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last column:
       movsd xmm0, [rcx];
       addsd xmm0, xmm6;
       mulsd xmm0, xmm7;
       movsd [rcx], xmm0;

       // next line:
       add rcx, rdx;

   // loop y end
   dec r9;
   jnz @@addforyloop;

   // epilog
   movupd xmm6, dXMM6;
   movupd xmm7, dXMM7;
end;


procedure ASMMatrixScaleAddAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; dOffset, Scale : double); {$IFDEF FPC}assembler;{$ENDIF}
var dXMM6, dXMM7 : Array[0..1] of double;
asm
   movupd dXMM6, xmm6;
   movupd dXMM7, xmm7;
   movddup xmm6, dOffset;
   movddup xmm7, scale;
   
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

   //iters := -width*sizeof(double);
   imul r8, -8;

   // helper registers for the mt1, mt2 and dest pointers
   sub rcx, r8;

   // for y := 0 to height - 1:
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r8;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // prefetchw data...
           // prefetchw [rcx + rax];

           // addition:
           movapd xmm0, [rcx + rax - 128];
           mulpd xmm0, xmm7;
           addpd xmm0, xmm6;
           movapd [rcx + rax - 128], xmm0;

           movapd xmm1, [rcx + rax - 112];
           mulpd xmm1, xmm7;
           addpd xmm1, xmm6;
           movapd [rcx + rax - 112], xmm1;

           movapd xmm2, [rcx + rax - 96];
           mulpd xmm2, xmm7;
           addpd xmm2, xmm6;
           movapd [rcx + rax - 96], xmm2;

           movapd xmm3, [rcx + rax - 80];
           mulpd xmm3, xmm7;
           addpd xmm3, xmm6;
           movapd [rcx + rax - 80], xmm3;

           movapd xmm0, [rcx + rax - 64];
           mulpd xmm0, xmm7;
           addpd xmm0, xmm6;
           movapd [rcx + rax - 64], xmm0;

           movapd xmm1, [rcx + rax - 48];
           mulpd xmm1, xmm7;
           addpd xmm1, xmm6;
           movapd [rcx + rax - 48], xmm1;

           movapd xmm2, [rcx + rax - 32];
           mulpd xmm2, xmm7;
           addpd xmm2, xmm6;
           movapd [rcx + rax - 32], xmm2;

           movapd xmm3, [rcx + rax - 16];
           mulpd xmm3, xmm7;
           addpd xmm3, xmm6;
           movapd [rcx + rax - 16], xmm3;

       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           movapd xmm0, [rcx + rax];
           mulpd xmm0, xmm7;
           addpd xmm0, xmm6;

           movapd [rcx + rax], xmm0;
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // next line:
       add rcx, rdx;

   // loop y end
   dec r9;
   jnz @@addforyloop;

   // epilog
   movupd xmm6, dXMM6;
   movupd xmm7, dXMM7;
end;

procedure ASMMatrixScaleAddUnAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; dOffset, Scale : double); {$IFDEF FPC}assembler;{$ENDIF}
var dXMM6, dXMM7 : Array[0..1] of double;
asm
   movupd dXMM6, xmm6;
   movupd dXMM7, xmm7;
   movddup xmm6, dOffset;
   movddup xmm7, scale;
   
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

   //iters := -width*sizeof(double);
   imul r8, -8;

   // helper registers for the mt1, mt2 and dest pointers
   sub rcx, r8;

   // for y := 0 to height - 1:
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r8;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // mul add:
           movupd xmm0, [rcx + rax - 128];
           mulpd xmm0, xmm7;
           addpd xmm0, xmm6;
           movupd [rcx + rax - 128], xmm0;

           movupd xmm1, [rcx + rax - 112];
           mulpd xmm1, xmm7;
           addpd xmm1, xmm6;
           movupd [rcx + rax - 112], xmm1;

           movupd xmm2, [rcx + rax - 96];
           mulpd xmm2, xmm7;
           addpd xmm2, xmm6;
           movupd [rcx + rax - 96], xmm2;

           movupd xmm3, [rcx + rax - 80];
           mulpd xmm3, xmm7;
           addpd xmm3, xmm6;
           movupd [rcx + rax - 80], xmm3;

           movupd xmm0, [rcx + rax - 64];
           mulpd xmm0, xmm7;
           addpd xmm0, xmm6;
           movupd [rcx + rax - 64], xmm0;

           movupd xmm1, [rcx + rax - 48];
           mulpd xmm1, xmm7;
           addpd xmm1, xmm6;
           movupd [rcx + rax - 48], xmm1;

           movupd xmm2, [rcx + rax - 32];
           mulpd xmm2, xmm7;
           addpd xmm2, xmm6;
           movupd [rcx + rax - 32], xmm2;

           movupd xmm3, [rcx + rax - 16];
           mulpd xmm3, xmm7;
           addpd xmm3, xmm6;
           movupd [rcx + rax - 16], xmm3;

       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           movupd xmm0, [rcx + rax];
           mulpd xmm0, xmm7;
           addpd xmm0, xmm6;

           movupd [rcx + rax], xmm0;
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // next line:
       add rcx, rdx;

   // loop y end
   dec r9;
   jnz @@addforyloop;

   // epilog
   movupd xmm6, dXMM6;
   movupd xmm7, dXMM7;
end;


procedure ASMMatrixScaleAddAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; dOffset, Scale : double); {$IFDEF FPC}assembler;{$ENDIF}
var dXMM6, dXMM7 : Array[0..1] of double;
asm
   movupd dXMM6, xmm6;
   movupd dXMM7, xmm7;
   movddup xmm6, dOffset;
   movddup xmm7, scale;
   
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

   //iters := -width*sizeof(double);
   dec r8;
   imul r8, -8;

   // helper registers for the mt1, mt2 and dest pointers
   sub rcx, r8;

   // for y := 0 to height - 1:
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r8;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // prefetchw data...
           // prefetchw [rcx + rax];

           // addition:
           movapd xmm0, [rcx + rax - 128];
           mulpd xmm0, xmm7;
           addpd xmm0, xmm6;
           movapd [rcx + rax - 128], xmm0;

           movapd xmm1, [rcx + rax - 112];
           mulpd xmm1, xmm7;
           addpd xmm1, xmm6;
           movapd [rcx + rax - 112], xmm1;

           movapd xmm2, [rcx + rax - 96];
           mulpd xmm2, xmm7;
           addpd xmm2, xmm6;
           movapd [rcx + rax - 96], xmm2;

           movapd xmm3, [rcx + rax - 80];
           mulpd xmm3, xmm7;
           addpd xmm3, xmm6;
           movapd [rcx + rax - 80], xmm3;

           movapd xmm0, [rcx + rax - 64];
           mulpd xmm0, xmm7;
           addpd xmm0, xmm6;
           movapd [rcx + rax - 64], xmm0;

           movapd xmm1, [rcx + rax - 48];
           mulpd xmm1, xmm7;
           addpd xmm1, xmm6;
           movapd [rcx + rax - 48], xmm1;

           movapd xmm2, [rcx + rax - 32];
           mulpd xmm2, xmm7;
           addpd xmm2, xmm6;
           movapd [rcx + rax - 32], xmm2;

           movapd xmm3, [rcx + rax - 16];
           mulpd xmm3, xmm7;
           addpd xmm3, xmm6;
           movapd [rcx + rax - 16], xmm3;

       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           movapd xmm0, [rcx + rax];
           mulpd xmm0, xmm7;
           addpd xmm0, xmm6;

           movapd [rcx + rax], xmm0;
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last column:
       movsd xmm0, [rcx];
       mulsd xmm0, xmm7;
       addsd xmm0, xmm6;
       movsd [rcx], xmm0;

       // next line:
       add rcx, rdx;

   // loop y end
   dec r9;
   jnz @@addforyloop;

   // epilog
   movupd xmm6, dXMM6;
   movupd xmm7, dXMM7;
end;

procedure ASMMatrixScaleAddUnAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; dOffset, Scale : double); {$IFDEF FPC}assembler;{$ENDIF}
var dXMM6, dXMM7 : Array[0..1] of double;
asm
   movupd dXMM6, xmm6;
   movupd dXMM7, xmm7;
   movddup xmm6, dOffset;
   movddup xmm7, scale;
   
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

   //iters := -(width - 1)*sizeof(double);
   dec r8;
   imul r8, -8;

   // helper registers for the mt1, mt2 and dest pointers
   sub rcx, r8;

   // for y := 0 to height - 1:
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r8;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // mul add:
           movupd xmm0, [rcx + rax - 128];
           mulpd xmm0, xmm7;
           addpd xmm0, xmm6;
           movupd [rcx + rax - 128], xmm0;

           movupd xmm1, [rcx + rax - 112];
           mulpd xmm1, xmm7;
           addpd xmm1, xmm6;
           movupd [rcx + rax - 112], xmm1;

           movupd xmm2, [rcx + rax - 96];
           mulpd xmm2, xmm7;
           addpd xmm2, xmm6;
           movupd [rcx + rax - 96], xmm2;

           movupd xmm3, [rcx + rax - 80];
           mulpd xmm3, xmm7;
           addpd xmm3, xmm6;
           movupd [rcx + rax - 80], xmm3;

           movupd xmm0, [rcx + rax - 64];
           mulpd xmm0, xmm7;
           addpd xmm0, xmm6;
           movupd [rcx + rax - 64], xmm0;

           movupd xmm1, [rcx + rax - 48];
           mulpd xmm1, xmm7;
           addpd xmm1, xmm6;
           movupd [rcx + rax - 48], xmm1;

           movupd xmm2, [rcx + rax - 32];
           mulpd xmm2, xmm7;
           addpd xmm2, xmm6;
           movupd [rcx + rax - 32], xmm2;

           movupd xmm3, [rcx + rax - 16];
           mulpd xmm3, xmm7;
           addpd xmm3, xmm6;
           movupd [rcx + rax - 16], xmm3;

       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           movupd xmm0, [rcx + rax];
           mulpd xmm0, xmm7;
           addpd xmm0, xmm6;

           movupd [rcx + rax], xmm0;
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last column:
       movsd xmm0, [rcx];
       mulsd xmm0, xmm7;
       addsd xmm0, xmm6;
       movsd [rcx], xmm0;

       // next line:
       add rcx, rdx;

   // loop y end
   dec r9;
   jnz @@addforyloop;

   // epilog
   movupd xmm6, dXMM6;
   movupd xmm7, dXMM7;
end;

{$ENDIF}

end.
