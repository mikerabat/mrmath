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


unit ASMMatrixTransposeOperationsx64;

interface

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF x64}

uses MatrixConst;

procedure ASMMatrixTransposeAlignedEvenWEvenH(dest : PDouble; const destLineWidth : TASMNativeInt; mt : PDouble; const LineWidth : TASMNativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}
procedure ASMMatrixTransposeUnAlignedEvenWEvenH(dest : PDouble; const destLineWidth : TASMNativeInt; mt : PDouble; const LineWidth : TASMNativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}

procedure ASMMatrixTransposeAlignedEvenWOddH(dest : PDouble; const destLineWidth : TASMNativeInt; mt : PDouble; const LineWidth : TASMNativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}
procedure ASMMatrixTransposeUnAlignedEvenWOddH(dest : PDouble; const destLineWidth : TASMNativeInt; mt : PDouble; const LineWidth : TASMNativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}

procedure ASMMatrixTransposeAlignedOddWEvenH(dest : PDouble; const destLineWidth : TASMNativeInt; mt : PDouble; const LineWidth : TASMNativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}
procedure ASMMatrixTransposeUnAlignedOddWEvenH(dest : PDouble; const destLineWidth : TASMNativeInt; mt : PDouble; const LineWidth : TASMNativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}

procedure ASMMatrixTransposeAlignedOddWOddH(dest : PDouble; const destLineWidth : TASMNativeInt; mt : PDouble; const LineWidth : TASMNativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}
procedure ASMMatrixTransposeUnAlignedOddWOddH(dest : PDouble; const destLineWidth : TASMNativeInt; mt : PDouble; const LineWidth : TASMNativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}

// Inplace Trasnposition of an N x N matrix
procedure ASMMatrixTransposeInplace(mt : PDouble; const LineWidth : TASMNativeInt; N : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}

{$ENDIF}

implementation

{$IFDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$S-} {$ENDIF}

procedure ASMMatrixTransposeAlignedEvenWEvenH(dest : PDouble; const destLineWidth : TASMNativeInt; mt : PDouble; const LineWidth : TASMNativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}
var iR12, iR13, iR14, iRDI : TASMNativeInt;
{$ifdef UNIX}
    width : TASMNativeInt;
    height : TASMNativeInt;
{$ENDIF}
asm
   {$IFDEF UNIX}
   // Linux uses a diffrent ABI -> copy over the registers so they meet with winABI
   // (note that the 5th and 6th parameter are are on the stack)
   // The parameters are passed in the following order:
   // RDI, RSI, RDX, RCX -> mov to RCX, RDX, R8, R9
   mov width, r8;
   mov height, r9;
   mov r8, rdx;
   mov r9, rcx;
   mov rcx, rdi;
   mov rdx, rsi;
   {$ENDIF}

   // rcx = dest, rdx = destLineWidth, r8 = mt, r9 = LineWidth
   mov iR12, r12;
   mov iR13, r13;
   mov iR14, r14;
   mov iRDI, rdi;

   // iters := -width*sizeof(double);
   mov r10, width;
   shl r10, 3;
   imul r10, -1;

   // destLineWidth2 := 2*destLineWidth;
   mov r11, destLineWidth;
   shl r11, 1;

   mov rdi, height;
   @foryloop:
       // prepare pointers
       mov r12, r8;
       sub r12, r10;
       mov r13, r12;
       add r13, r9;

       mov r14, rcx;

       // unrolled loop
       mov rax, r10;
       @forxloop:
           add rax, 128;
           jg @loopend;

           // prefetch [r12 + rax];
           // prefetch [r13 + rax];

           movapd xmm0, [r12 + rax - 128];
           movapd xmm1, [r13 + rax - 128];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movapd [r14], xmm0;
           movapd [r14 + rdx], xmm1;

           add r14, r11;

           movapd xmm0, [r12 + rax - 112];
           movapd xmm1, [r13 + rax - 112];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movapd [r14], xmm0;
           movapd [r14 + rdx], xmm1;

           add r14, r11;

           movapd xmm0, [r12 + rax - 96];
           movapd xmm1, [r13 + rax - 96];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movapd [r14], xmm0;
           movapd [r14 + rdx], xmm1;

           add r14, r11;

           movapd xmm0, [r12 + rax - 80];
           movapd xmm1, [r13 + rax - 80];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movapd [r14], xmm0;
           movapd [r14 + rdx], xmm1;

           add r14, r11;

           movapd xmm0, [r12 + rax - 64];
           movapd xmm1, [r13 + rax - 64];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movapd [r14], xmm0;
           movapd [r14 + rdx], xmm1;

           add r14, r11;

           movapd xmm0, [r12 + rax - 48];
           movapd xmm1, [r13 + rax - 48];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movapd [r14], xmm0;
           movapd [r14 + rdx], xmm1;

           add r14, r11;

           movapd xmm0, [r12 + rax - 32];
           movapd xmm1, [r13 + rax - 32];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movapd [r14], xmm0;
           movapd [r14 + rdx], xmm1;

           add r14, r11;

           movapd xmm0, [r12 + rax - 16];
           movapd xmm1, [r13 + rax - 16];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movapd [r14], xmm0;
           movapd [r14 + rdx], xmm1;

           add r14, r11;

       jmp @forxloop;

       @loopend:

       sub rax, 128;

       jz @nextLine;

       @forxloop2:
           movapd xmm0, [r12 + rax];
           movapd xmm1, [r13 + rax];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movapd [r14], xmm0;
           movapd [r14 + rdx], xmm1;

           add r14, r11;
       add rax, 16;
       jnz @forxloop2;

       @nextLine:

       // increment pointers
       add rcx, 16;

       add r8, r9;
       add r8, r9;

       sub rdi, 2;
   jnz @foryloop;

   // epilog - cleanup stack
   mov r12, iR12;
   mov r13, iR13;
   mov r14, iR14;
   mov rdi, iRDI;
end;

procedure ASMMatrixTransposeUnAlignedEvenWEvenH(dest : PDouble; const destLineWidth : TASMNativeInt; mt : PDouble; const LineWidth : TASMNativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}
var iR12, iR13, iR14, iRDI : TASMNativeInt;
{$ifdef UNIX}
    width : TASMNativeInt;
    height : TASMNativeInt;
{$ENDIF}
asm
   {$IFDEF UNIX}
   // Linux uses a diffrent ABI -> copy over the registers so they meet with winABI
   // (note that the 5th and 6th parameter are are on the stack)
   // The parameters are passed in the following order:
   // RDI, RSI, RDX, RCX -> mov to RCX, RDX, R8, R9
   mov width, r8;
   mov height, r9;
   mov r8, rdx;
   mov r9, rcx;
   mov rcx, rdi;
   mov rdx, rsi;
   {$ENDIF}

   // rcx = dest, rdx = destLineWidth, r8 = mt, r9 = LineWidth
   mov iR12, r12;
   mov iR13, r13;
   mov iR14, r14;
   mov iRDI, rdi;

   // iters := -width*sizeof(double);
   mov r10, width;
   shl r10, 3;
   imul r10, -1;

   // destLineWidth2 := 2*destLineWidth;
   mov r11, destLineWidth;
   shl r11, 1;

   mov rdi, height;
   @foryloop:
       // prepare pointers
       mov r12, r8;
       sub r12, r10;
       mov r13, r12;
       add r13, r9;

       mov r14, rcx;

       // unrolled loop
       mov rax, r10;
       @forxloop:
           add rax, 128;
           jg @loopend;

           movupd xmm0, [r12 + rax - 128];
           movupd xmm1, [r13 + rax - 128];

           movupd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movupd [r14], xmm0;
           movupd [r14 + rdx], xmm1;

           add r14, r11;

           movupd xmm0, [r12 + rax - 112];
           movupd xmm1, [r13 + rax - 112];

           movupd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movupd [r14], xmm0;
           movupd [r14 + rdx], xmm1;

           add r14, r11;

           movupd xmm0, [r12 + rax - 96];
           movupd xmm1, [r13 + rax - 96];

           movupd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movupd [r14], xmm0;
           movupd [r14 + rdx], xmm1;

           add r14, r11;

           movupd xmm0, [r12 + rax - 80];
           movupd xmm1, [r13 + rax - 80];

           movupd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movupd [r14], xmm0;
           movupd [r14 + rdx], xmm1;

           add r14, r11;

           movupd xmm0, [r12 + rax - 64];
           movupd xmm1, [r13 + rax - 64];

           movupd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movupd [r14], xmm0;
           movupd [r14 + rdx], xmm1;

           add r14, r11;

           movupd xmm0, [r12 + rax - 48];
           movupd xmm1, [r13 + rax - 48];

           movupd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movupd [r14], xmm0;
           movupd [r14 + rdx], xmm1;

           add r14, r11;

           movupd xmm0, [r12 + rax - 32];
           movupd xmm1, [r13 + rax - 32];

           movupd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movupd [r14], xmm0;
           movupd [r14 + rdx], xmm1;

           add r14, r11;

           movupd xmm0, [r12 + rax - 16];
           movupd xmm1, [r13 + rax - 16];

           movupd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movupd [r14], xmm0;
           movupd [r14 + rdx], xmm1;

           add r14, r11;

       jmp @forxloop;

       @loopend:

       sub rax, 128;

       jz @nextLine;

       @forxloop2:
           movupd xmm0, [r12 + rax];
           movupd xmm1, [r13 + rax];

           movupd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movupd [r14], xmm0;
           movupd [r14 + rdx], xmm1;

           add r14, r11;
       add rax, 16;
       jnz @forxloop2;

       @nextLine:

       // increment pointers
       add rcx, 16;

       add r8, r9;
       add r8, r9;

       sub rdi, 2;
   jnz @foryloop;

   // epilog - cleanup stack
   mov r12, iR12;
   mov r13, iR13;
   mov r14, iR14;
   mov rdi, iRDI;
end;


procedure ASMMatrixTransposeAlignedEvenWOddH(dest : PDouble; const destLineWidth : TASMNativeInt; mt : PDouble; const LineWidth : TASMNativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}
var iR12, iR13, iR14, iRDI : TASMNativeInt;
{$ifdef UNIX}
    width : TASMNativeInt;
    height : TASMNativeInt;
{$ENDIF}
asm
   {$IFDEF UNIX}
   // Linux uses a diffrent ABI -> copy over the registers so they meet with winABI
   // (note that the 5th and 6th parameter are are on the stack)
   // The parameters are passed in the following order:
   // RDI, RSI, RDX, RCX -> mov to RCX, RDX, R8, R9
   mov width, r8;
   mov height, r9;
   mov r8, rdx;
   mov r9, rcx;
   mov rcx, rdi;
   mov rdx, rsi;
   {$ENDIF}

   // rcx = dest, rdx = destLineWidth, r8 = mt, r9 = LineWidth
   mov iR12, r12;
   mov iR13, r13;
   mov iR14, r14;
   mov iRDI, rdi;

   // iters := -width*sizeof(double);
			mov r10, width;
   shl r10, 3;
   imul r10, -1;

   // destLineWidth2 := 2*destLineWidth;
   mov r11, destLineWidth;
   shl r11, 1;

   mov rdi, height;
   dec rdi;
   jz @lastline;
   @foryloop:
       // prepare pointers
       mov r12, r8;
       sub r12, r10;
       mov r13, r12;
       add r13, r9;

       mov r14, rcx;

       // unrolled loop
       mov rax, r10;
       @forxloop:
           add rax, 128;
           jg @loopend;

           // prefetch [r12 + rax];
           // prefetch [r13 + rax];

           movapd xmm0, [r12 + rax - 128];
           movapd xmm1, [r13 + rax - 128];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movapd [r14], xmm0;
           movapd [r14 + rdx], xmm1;

           add r14, r11;

           movapd xmm0, [r12 + rax - 112];
           movapd xmm1, [r13 + rax - 112];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movapd [r14], xmm0;
           movapd [r14 + rdx], xmm1;

           add r14, r11;

           movapd xmm0, [r12 + rax - 96];
           movapd xmm1, [r13 + rax - 96];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movapd [r14], xmm0;
           movapd [r14 + rdx], xmm1;

           add r14, r11;

           movapd xmm0, [r12 + rax - 80];
           movapd xmm1, [r13 + rax - 80];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movapd [r14], xmm0;
           movapd [r14 + rdx], xmm1;

           add r14, r11;

           movapd xmm0, [r12 + rax - 64];
           movapd xmm1, [r13 + rax - 64];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movapd [r14], xmm0;
           movapd [r14 + rdx], xmm1;

           add r14, r11;

           movapd xmm0, [r12 + rax - 48];
           movapd xmm1, [r13 + rax - 48];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movapd [r14], xmm0;
           movapd [r14 + rdx], xmm1;

           add r14, r11;

           movapd xmm0, [r12 + rax - 32];
           movapd xmm1, [r13 + rax - 32];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movapd [r14], xmm0;
           movapd [r14 + rdx], xmm1;

           add r14, r11;

           movapd xmm0, [r12 + rax - 16];
           movapd xmm1, [r13 + rax - 16];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movapd [r14], xmm0;
           movapd [r14 + rdx], xmm1;

           add r14, r11;

       jmp @forxloop;

       @loopend:

       sub rax, 128;

       jz @nextLine;

       @forxloop2:
           movapd xmm0, [r12 + rax];
           movapd xmm1, [r13 + rax];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movapd [r14], xmm0;
           movapd [r14 + rdx], xmm1;

           add r14, r11;
       add rax, 16;
       jnz @forxloop2;

       @nextLine:

       // increment pointers
       add rcx, 16;

       add r8, r9;
       add r8, r9;

       sub rdi, 2;
   jnz @foryloop;

   @lastline:

   // handle last line differently
   mov rax, r10;
   sub r8, r10;
   @forxloop3:
       movapd xmm0, [r8 + rax];

       movhlps xmm1, xmm0;
       movsd [rcx], xmm0;
       movsd [rcx + rdx], xmm1;

       add rcx, r11;
   add rax, 16;
   jnz @forxloop3;

   // epilog - cleanup stack
   mov r12, iR12;
   mov r13, iR13;
   mov r14, iR14;
   mov rdi, iRDI;
end;

procedure ASMMatrixTransposeUnAlignedEvenWOddH(dest : PDouble; const destLineWidth : TASMNativeInt; mt : PDouble; const LineWidth : TASMNativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}
var iR12, iR13, iR14, iRDI : TASMNativeInt;
{$ifdef UNIX}
    width : TASMNativeInt;
    height : TASMNativeInt;
{$ENDIF}
asm
   {$IFDEF UNIX}
   // Linux uses a diffrent ABI -> copy over the registers so they meet with winABI
   // (note that the 5th and 6th parameter are are on the stack)
   // The parameters are passed in the following order:
   // RDI, RSI, RDX, RCX -> mov to RCX, RDX, R8, R9
   mov width, r8;
   mov height, r9;
   mov r8, rdx;
   mov r9, rcx;
   mov rcx, rdi;
   mov rdx, rsi;
   {$ENDIF}

   // rcx = dest, rdx = destLineWidth, r8 = mt, r9 = LineWidth
   mov iR12, r12;
   mov iR13, r13;
   mov iR14, r14;
   mov iRDI, rdi;

   // iters := -width*sizeof(double);
			mov r10, width;
   shl r10, 3;
   imul r10, -1;

   // destLineWidth2 := 2*destLineWidth;
   mov r11, destLineWidth;
   shl r11, 1;

   mov rdi, height;
   dec rdi;
   jz @lastline;
   @foryloop:
       // prepare pointers
       mov r12, r8;
       sub r12, r10;
       mov r13, r12;
       add r13, r9;

       mov r14, rcx;

       // unrolled loop
       mov rax, r10;
       @forxloop:
           add rax, 128;
           jg @loopend;

           movupd xmm0, [r12 + rax - 128];
           movupd xmm1, [r13 + rax - 128];

           movupd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movupd [r14], xmm0;
           movupd [r14 + rdx], xmm1;

           add r14, r11;

           movupd xmm0, [r12 + rax - 112];
           movupd xmm1, [r13 + rax - 112];

           movupd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movupd [r14], xmm0;
           movupd [r14 + rdx], xmm1;

           add r14, r11;

           movupd xmm0, [r12 + rax - 96];
           movupd xmm1, [r13 + rax - 96];

           movupd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movupd [r14], xmm0;
           movupd [r14 + rdx], xmm1;

           add r14, r11;

           movupd xmm0, [r12 + rax - 80];
           movupd xmm1, [r13 + rax - 80];

           movupd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movupd [r14], xmm0;
           movupd [r14 + rdx], xmm1;

           add r14, r11;

           movupd xmm0, [r12 + rax - 64];
           movupd xmm1, [r13 + rax - 64];

           movupd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movupd [r14], xmm0;
           movupd [r14 + rdx], xmm1;

           add r14, r11;

           movupd xmm0, [r12 + rax - 48];
           movupd xmm1, [r13 + rax - 48];

           movupd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movupd [r14], xmm0;
           movupd [r14 + rdx], xmm1;

           add r14, r11;

           movupd xmm0, [r12 + rax - 32];
           movupd xmm1, [r13 + rax - 32];

           movupd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movupd [r14], xmm0;
           movupd [r14 + rdx], xmm1;

           add r14, r11;

           movupd xmm0, [r12 + rax - 16];
           movupd xmm1, [r13 + rax - 16];

           movupd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movupd [r14], xmm0;
           movupd [r14 + rdx], xmm1;

           add r14, r11;

       jmp @forxloop;

       @loopend:

       sub rax, 128;

       jz @nextLine;

       @forxloop2:
           movupd xmm0, [r12 + rax];
           movupd xmm1, [r13 + rax];

           movupd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movupd [r14], xmm0;
           movupd [r14 + rdx], xmm1;

           add r14, r11;
       add rax, 16;
       jnz @forxloop2;

       @nextLine:

       // increment pointers
       add rcx, 16;

       add r8, r9;
       add r8, r9;

       sub rdi, 2;
   jnz @foryloop;

   @lastline:
   // handle last line differently
   mov rax, r10;
   sub r8, r10;
   @forxloop3:
       movupd xmm0, [r8 + rax];

       movhlps xmm1, xmm0;
       movsd [rcx], xmm0;
       movsd [rcx + rdx], xmm1;

       add rcx, r11;
   add rax, 16;
   jnz @forxloop3;

   // epilog - cleanup stack
   mov r12, iR12;
   mov r13, iR13;
   mov r14, iR14;
   mov rdi, iRDI;
end;

procedure ASMMatrixTransposeAlignedOddWEvenH(dest : PDouble; const destLineWidth : TASMNativeInt; mt : PDouble; const LineWidth : TASMNativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}
var iR12, iR13, iR14, iRDI : TASMNativeInt;
{$ifdef UNIX}
    width : TASMNativeInt;
    height : TASMNativeInt;
{$ENDIF}
asm
   {$IFDEF UNIX}
   // Linux uses a diffrent ABI -> copy over the registers so they meet with winABI
   // (note that the 5th and 6th parameter are are on the stack)
   // The parameters are passed in the following order:
   // RDI, RSI, RDX, RCX -> mov to RCX, RDX, R8, R9
   mov width, r8;
   mov height, r9;
   mov r8, rdx;
   mov r9, rcx;
   mov rcx, rdi;
   mov rdx, rsi;
   {$ENDIF}

   // rcx = dest, rdx = destLineWidth, r8 = mt, r9 = LineWidth
   mov iR12, r12;
   mov iR13, r13;
   mov iR14, r14;
   mov iRDI, rdi;

   // iters := -(width - 1)*sizeof(double);
			mov r10, width;
   dec r10;
   shl r10, 3;
   imul r10, -1;

   // destLineWidth2 := 2*destLineWidth;
   mov r11, destLineWidth;
   shl r11, 1;

   mov rdi, height;
   @foryloop:
       // prepare pointers
       mov r12, r8;
       sub r12, r10;
       mov r13, r12;
       add r13, r9;

       mov r14, rcx;

       // unrolled loop
       mov rax, r10;
       @forxloop:
           add rax, 128;
           jg @loopend;

           // prefetch [r12 + rax];
           // prefetch [r13 + rax];

           movapd xmm0, [r12 + rax - 128];
           movapd xmm1, [r13 + rax - 128];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movapd [r14], xmm0;
           movapd [r14 + rdx], xmm1;

           add r14, r11;

           movapd xmm0, [r12 + rax - 112];
           movapd xmm1, [r13 + rax - 112];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movapd [r14], xmm0;
           movapd [r14 + rdx], xmm1;

           add r14, r11;

           movapd xmm0, [r12 + rax - 96];
           movapd xmm1, [r13 + rax - 96];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movapd [r14], xmm0;
           movapd [r14 + rdx], xmm1;

           add r14, r11;

           movapd xmm0, [r12 + rax - 80];
           movapd xmm1, [r13 + rax - 80];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movapd [r14], xmm0;
           movapd [r14 + rdx], xmm1;

           add r14, r11;

           movapd xmm0, [r12 + rax - 64];
           movapd xmm1, [r13 + rax - 64];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movapd [r14], xmm0;
           movapd [r14 + rdx], xmm1;

           add r14, r11;

           movapd xmm0, [r12 + rax - 48];
           movapd xmm1, [r13 + rax - 48];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movapd [r14], xmm0;
           movapd [r14 + rdx], xmm1;

           add r14, r11;

           movapd xmm0, [r12 + rax - 32];
           movapd xmm1, [r13 + rax - 32];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movapd [r14], xmm0;
           movapd [r14 + rdx], xmm1;

           add r14, r11;

           movapd xmm0, [r12 + rax - 16];
           movapd xmm1, [r13 + rax - 16];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movapd [r14], xmm0;
           movapd [r14 + rdx], xmm1;

           add r14, r11;

       jmp @forxloop;

       @loopend:

       sub rax, 128;

       jz @nextLine;

       @forxloop2:
           movapd xmm0, [r12 + rax];
           movapd xmm1, [r13 + rax];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movapd [r14], xmm0;
           movapd [r14 + rdx], xmm1;

           add r14, r11;
       add rax, 16;
       jnz @forxloop2;

       @nextLine:

       // handle last element differently
       movsd xmm0, [r12 + rax];
       movsd xmm1, [r13 + rax];

       movlhps xmm0, xmm1;
       movapd [r14], xmm0;

       // increment pointers
       add rcx, 16;

       add r8, r9;
       add r8, r9;

       sub rdi, 2;
   jnz @foryloop;

   // epilog - cleanup stack
   mov r12, iR12;
   mov r13, iR13;
   mov r14, iR14;
   mov rdi, iRDI;
end;

procedure ASMMatrixTransposeUnAlignedOddWEvenH(dest : PDouble; const destLineWidth : TASMNativeInt; mt : PDouble; const LineWidth : TASMNativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}
var iR12, iR13, iR14, iRDI : TASMNativeInt;
{$ifdef UNIX}
    width : TASMNativeInt;
    height : TASMNativeInt;
{$ENDIF}
asm
   {$IFDEF UNIX}
   // Linux uses a diffrent ABI -> copy over the registers so they meet with winABI
   // (note that the 5th and 6th parameter are are on the stack)
   // The parameters are passed in the following order:
   // RDI, RSI, RDX, RCX -> mov to RCX, RDX, R8, R9
   mov width, r8;
   mov height, r9;
   mov r8, rdx;
   mov r9, rcx;
   mov rcx, rdi;
   mov rdx, rsi;
   {$ENDIF}

   // rcx = dest, rdx = destLineWidth, r8 = mt, r9 = LineWidth
   mov iR12, r12;
   mov iR13, r13;
   mov iR14, r14;
   mov iRDI, rdi;

   // iters := -(width - 1)*sizeof(double);
			mov r10, width;
   dec r10;
   shl r10, 3;
   imul r10, -1;

   // destLineWidth2 := 2*destLineWidth;
   mov r11, destLineWidth;
   shl r11, 1;

   mov rdi, height;
   @foryloop:
       // prepare pointers
       mov r12, r8;
       sub r12, r10;
       mov r13, r12;
       add r13, r9;

       mov r14, rcx;

       // unrolled loop
       mov rax, r10;
       @forxloop:
           add rax, 128;
           jg @loopend;

           movupd xmm0, [r12 + rax - 128];
           movupd xmm1, [r13 + rax - 128];

           movupd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movupd [r14], xmm0;
           movupd [r14 + rdx], xmm1;

           add r14, r11;

           movupd xmm0, [r12 + rax - 112];
           movupd xmm1, [r13 + rax - 112];

           movupd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movupd [r14], xmm0;
           movupd [r14 + rdx], xmm1;

           add r14, r11;

           movupd xmm0, [r12 + rax - 96];
           movupd xmm1, [r13 + rax - 96];

           movupd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movupd [r14], xmm0;
           movupd [r14 + rdx], xmm1;

           add r14, r11;

           movupd xmm0, [r12 + rax - 80];
           movupd xmm1, [r13 + rax - 80];

           movupd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movupd [r14], xmm0;
           movupd [r14 + rdx], xmm1;

           add r14, r11;

           movupd xmm0, [r12 + rax - 64];
           movupd xmm1, [r13 + rax - 64];

           movupd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movupd [r14], xmm0;
           movupd [r14 + rdx], xmm1;

           add r14, r11;

           movupd xmm0, [r12 + rax - 48];
           movupd xmm1, [r13 + rax - 48];

           movupd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movupd [r14], xmm0;
           movupd [r14 + rdx], xmm1;

           add r14, r11;

           movupd xmm0, [r12 + rax - 32];
           movupd xmm1, [r13 + rax - 32];

           movupd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movupd [r14], xmm0;
           movupd [r14 + rdx], xmm1;

           add r14, r11;

           movupd xmm0, [r12 + rax - 16];
           movupd xmm1, [r13 + rax - 16];

           movupd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movupd [r14], xmm0;
           movupd [r14 + rdx], xmm1;

           add r14, r11;

       jmp @forxloop;

       @loopend:

       sub rax, 128;

       jz @nextLine;

       @forxloop2:
           movupd xmm0, [r12 + rax];
           movupd xmm1, [r13 + rax];

           movupd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movupd [r14], xmm0;
           movupd [r14 + rdx], xmm1;

           add r14, r11;
       add rax, 16;
       jnz @forxloop2;

       @nextLine:

       // handle last element differently
       movsd xmm0, [r12 + rax];
       movsd xmm1, [r13 + rax];

       movlhps xmm0, xmm1;
       movupd [r14], xmm0;

       // increment pointers
       add rcx, 16;

       add r8, r9;
       add r8, r9;

       sub rdi, 2;
   jnz @foryloop;

   // epilog - cleanup stack
   mov r12, iR12;
   mov r13, iR13;
   mov r14, iR14;
   mov rdi, iRDI;
end;


procedure ASMMatrixTransposeAlignedOddWOddH(dest : PDouble; const destLineWidth : TASMNativeInt; mt : PDouble; const LineWidth : TASMNativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}
var iR12, iR13, iR14, iRDI : TASMNativeInt;
{$ifdef UNIX}
    width : TASMNativeInt;
    height : TASMNativeInt;
{$ENDIF}
asm
   {$IFDEF UNIX}
   // Linux uses a diffrent ABI -> copy over the registers so they meet with winABI
   // (note that the 5th and 6th parameter are are on the stack)
   // The parameters are passed in the following order:
   // RDI, RSI, RDX, RCX -> mov to RCX, RDX, R8, R9
   mov width, r8;
   mov height, r9;
   mov r8, rdx;
   mov r9, rcx;
   mov rcx, rdi;
   mov rdx, rsi;
   {$ENDIF}

   // rcx = dest, rdx = destLineWidth, r8 = mt, r9 = LineWidth
   mov iR12, r12;
   mov iR13, r13;
   mov iR14, r14;
   mov iRDI, rdi;

   // iters := -width*sizeof(double);
			mov r10, width;
   dec r10;
   shl r10, 3;
   imul r10, -1;

   // destLineWidth2 := 2*destLineWidth;
   mov r11, destLineWidth;
   shl r11, 1;

   mov rdi, height;
   dec rdi;
   jz @lastline;
   @foryloop:
       // prepare pointers
       mov r12, r8;
       sub r12, r10;
       mov r13, r12;
       add r13, r9;

       mov r14, rcx;

       // unrolled loop
       mov rax, r10;
       @forxloop:
           add rax, 128;
           jg @loopend;

           // prefetch [r12 + rax];
           // prefetch [r13 + rax];

           movapd xmm0, [r12 + rax - 128];
           movapd xmm1, [r13 + rax - 128];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movapd [r14], xmm0;
           movapd [r14 + rdx], xmm1;

           add r14, r11;

           movapd xmm0, [r12 + rax - 112];
           movapd xmm1, [r13 + rax - 112];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movapd [r14], xmm0;
           movapd [r14 + rdx], xmm1;

           add r14, r11;

           movapd xmm0, [r12 + rax - 96];
           movapd xmm1, [r13 + rax - 96];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movapd [r14], xmm0;
           movapd [r14 + rdx], xmm1;

           add r14, r11;

           movapd xmm0, [r12 + rax - 80];
           movapd xmm1, [r13 + rax - 80];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movapd [r14], xmm0;
           movapd [r14 + rdx], xmm1;

           add r14, r11;

           movapd xmm0, [r12 + rax - 64];
           movapd xmm1, [r13 + rax - 64];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movapd [r14], xmm0;
           movapd [r14 + rdx], xmm1;

           add r14, r11;

           movapd xmm0, [r12 + rax - 48];
           movapd xmm1, [r13 + rax - 48];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movapd [r14], xmm0;
           movapd [r14 + rdx], xmm1;

           add r14, r11;

           movapd xmm0, [r12 + rax - 32];
           movapd xmm1, [r13 + rax - 32];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movapd [r14], xmm0;
           movapd [r14 + rdx], xmm1;

           add r14, r11;

           movapd xmm0, [r12 + rax - 16];
           movapd xmm1, [r13 + rax - 16];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movapd [r14], xmm0;
           movapd [r14 + rdx], xmm1;

           add r14, r11;

       jmp @forxloop;

       @loopend:

       sub rax, 128;

       jz @nextLine;

       @forxloop2:
           movapd xmm0, [r12 + rax];
           movapd xmm1, [r13 + rax];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movapd [r14], xmm0;
           movapd [r14 + rdx], xmm1;

           add r14, r11;
       add rax, 16;
       jnz @forxloop2;

       @nextLine:

       // handle last element differently
       movsd xmm0, [r12 + rax];
       movsd xmm1, [r13 + rax];

       movlhps xmm0, xmm1;
       movapd [r14], xmm0;

       // increment pointers
       add rcx, 16;

       add r8, r9;
       add r8, r9;

       sub rdi, 2;
   jnz @foryloop;

   @lastline:

   // handle last line differently
   mov rax, r10;
   sub r8, r10;
   @forxloop3:
       movapd xmm0, [r8 + rax];

       movhlps xmm1, xmm0;
       movsd [rcx], xmm0;
       movsd [rcx + rdx], xmm1;

       add rcx, r11;
   add rax, 16;
   jnz @forxloop3;

   // last element of last line
   movsd xmm0, [r8];
   movsd [rcx], xmm0;

   // epilog - cleanup stack
   mov r12, iR12;
   mov r13, iR13;
   mov r14, iR14;
   mov rdi, iRDI;
end;

procedure ASMMatrixTransposeUnAlignedOddWOddH(dest : PDouble; const destLineWidth : TASMNativeInt; mt : PDouble; const LineWidth : TASMNativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}
var iR12, iR13, iR14, iRDI : TASMNativeInt;
{$ifdef UNIX}
    width : TASMNativeInt;
    height : TASMNativeInt;
{$ENDIF}
asm
   {$IFDEF UNIX}
   // Linux uses a diffrent ABI -> copy over the registers so they meet with winABI
   // (note that the 5th and 6th parameter are are on the stack)
   // The parameters are passed in the following order:
   // RDI, RSI, RDX, RCX -> mov to RCX, RDX, R8, R9
   mov width, r8;
   mov height, r9;
   mov r8, rdx;
   mov r9, rcx;
   mov rcx, rdi;
   mov rdx, rsi;
   {$ENDIF}

   // rcx = dest, rdx = destLineWidth, r8 = mt, r9 = LineWidth
   mov iR12, r12;
   mov iR13, r13;
   mov iR14, r14;
   mov iRDI, rdi;

   // iters := -width*sizeof(double);
			mov r10, width;
   dec r10;
   shl r10, 3;
   imul r10, -1;

   // destLineWidth2 := 2*destLineWidth;
   mov r11, destLineWidth;
   shl r11, 1;

   mov rdi, height;
   dec rdi;
   jz @lastline;
   @foryloop:
       // prepare pointers
       mov r12, r8;
       sub r12, r10;
       mov r13, r12;
       add r13, r9;

       mov r14, rcx;

       // unrolled loop
       mov rax, r10;
       @forxloop:
           add rax, 128;
           jg @loopend;

           movupd xmm0, [r12 + rax - 128];
           movupd xmm1, [r13 + rax - 128];

           movupd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movupd [r14], xmm0;
           movupd [r14 + rdx], xmm1;

           add r14, r11;

           movupd xmm0, [r12 + rax - 112];
           movupd xmm1, [r13 + rax - 112];

           movupd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movupd [r14], xmm0;
           movupd [r14 + rdx], xmm1;

           add r14, r11;

           movupd xmm0, [r12 + rax - 96];
           movupd xmm1, [r13 + rax - 96];

           movupd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movupd [r14], xmm0;
           movupd [r14 + rdx], xmm1;

           add r14, r11;

           movupd xmm0, [r12 + rax - 80];
           movupd xmm1, [r13 + rax - 80];

           movupd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movupd [r14], xmm0;
           movupd [r14 + rdx], xmm1;

           add r14, r11;

           movupd xmm0, [r12 + rax - 64];
           movupd xmm1, [r13 + rax - 64];

           movupd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movupd [r14], xmm0;
           movupd [r14 + rdx], xmm1;

           add r14, r11;

           movupd xmm0, [r12 + rax - 48];
           movupd xmm1, [r13 + rax - 48];

           movupd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movupd [r14], xmm0;
           movupd [r14 + rdx], xmm1;

           add r14, r11;

           movupd xmm0, [r12 + rax - 32];
           movupd xmm1, [r13 + rax - 32];

           movupd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movupd [r14], xmm0;
           movupd [r14 + rdx], xmm1;

           add r14, r11;

           movupd xmm0, [r12 + rax - 16];
           movupd xmm1, [r13 + rax - 16];

           movupd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movupd [r14], xmm0;
           movupd [r14 + rdx], xmm1;

           add r14, r11;

       jmp @forxloop;

       @loopend:

       sub rax, 128;

       jz @nextLine;

       @forxloop2:
           movupd xmm0, [r12 + rax];
           movupd xmm1, [r13 + rax];

           movupd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movupd [r14], xmm0;
           movupd [r14 + rdx], xmm1;

           add r14, r11;
       add rax, 16;
       jnz @forxloop2;

       @nextLine:

       // handle last element differently
       movsd xmm0, [r12 + rax];
       movsd xmm1, [r13 + rax];

       movlhps xmm0, xmm1;
       movupd [r14], xmm0;

       // increment pointers
       add rcx, 16;

       add r8, r9;
       add r8, r9;

       sub rdi, 2;
   jnz @foryloop;

   @lastline:

   // handle last line differently
   mov rax, r10;
   sub r8, r10;
   @forxloop3:
       movupd xmm0, [r8 + rax];

       movhlps xmm1, xmm0;
       movsd [rcx], xmm0;
       movsd [rcx + rdx], xmm1;

       add rcx, r11;
   add rax, 16;
   jnz @forxloop3;

   // last element of last line
   movsd xmm0, [r8];
   movsd [rcx], xmm0;

   // epilog - cleanup stack
   mov r12, iR12;
   mov r13, iR13;
   mov r14, iR14;
   mov rdi, iRDI;
end;

// simple Inplace Trasnposition of an N x N matrix
procedure ASMMatrixTransposeInplace(mt : PDouble; const LineWidth : TASMNativeInt; N : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}
var iRBX, iRDI, iRSI : TASMNativeInt;
// rcx: mt, rdx, LineWidth, r8: N
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

    // prolog - stack
    mov iRBX, rbx;
    mov iRDI, rdi;
    mov iRSI, rsi;
           cmp r8, 2;
    jl @@exitProc;
           // iter: -N*sizeof(Double)
    mov rax, r8;
    imul rax, -8;
                  mov rbx, rcx;  // pDest1: genptr(mt, 0, 1, linewidth)
    add rbx, rdx;
           sub rcx, rax;  // mt + iter
           // for y := 0 to n - 2
    dec r8;
    @@foryloop:
              mov rdi, rax; // iter aka x
       add rdi, 8;
       mov rsi, rbx;
       // for x := y + 1 to n-1 do
       @@forxloop:
          movsd xmm0, [rcx + rdi];
          movsd xmm1, [rsi];
                 movsd [rcx + rdi], xmm1;
          movsd [rsi], xmm0;
                 add rsi, rdx;
       add rdi, 8;
       jnz @@forxloop;
              add rax, 8;  // iter + sizeof(double);
       //pDest := PConstDoubleArr( GenPtr(dest, 0, y, destLineWidth) );
       add rcx, rdx;
       // GenPtr(dest, y, y + 1, destLineWidth);
       add rbx, rdx;
       add rbx, 8;
    dec r8;
    jnz @@foryloop;
           @@exitProc:
           // epilog - cleanup stack
    mov rbx, iRBX;
    mov rdi, iRDI;
    mov rsi, iRSI;
end;

{$ENDIF}

end.
