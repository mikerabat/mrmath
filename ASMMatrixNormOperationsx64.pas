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


unit ASMMatrixNormOperationsx64;

interface

{$I 'mrMath_CPU.inc'}

{$IFDEF x64}

function ASMMatrixElementwiseNorm2AlignedEvenW(dest : PDouble; const LineWidth : NativeInt; Width, height : NativeInt) : double; {$IFDEF FPC}assembler;{$ENDIF}
function ASMMatrixElementwiseNorm2UnAlignedEvenW(dest : PDouble; const LineWidth : NativeInt; Width, height : NativeInt) : double; {$IFDEF FPC}assembler;{$ENDIF}

function ASMMatrixElementwiseNorm2AlignedOddW(dest : PDouble; const LineWidth : NativeInt; Width, height : NativeInt) : double; {$IFDEF FPC}assembler;{$ENDIF}
function ASMMatrixElementwiseNorm2UnAlignedOddW(dest : PDouble; const LineWidth : NativeInt; Width, height : NativeInt) : double; {$IFDEF FPC}assembler;{$ENDIF}


procedure ASMMatrixNormalizeRowAlignedEvenW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}
procedure ASMMatrixNormalizeRowUnAlignedEvenW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}
procedure ASMMatrixNormalizeRowAlignedOddW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}
procedure ASMMatrixNormalizeRowUnAlignedOddW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}

procedure ASMMatrixNormalizeColumnAlignedEvenW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}
procedure ASMMatrixNormalizeColumnUnAlignedEvenW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}
procedure ASMMatrixNormalizeColumnAlignedOddW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}
procedure ASMMatrixNormalizeColumnUnAlignedOddW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}

{$ENDIF}

implementation

{$IFDEF x64}

const cLocOnes : Array[0..1] of double = (1, 1);

function ASMMatrixElementwiseNorm2AlignedEvenW(dest : PDouble; const LineWidth : NativeInt; Width, height : NativeInt) : double; {$IFDEF FPC}assembler;{$ENDIF}
var dXMM4, dXMM7 : Array[0..1] of double;
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

   // note: RCX = dest, RDX = destLineWidth, R8 = widh, R9 = height
   // prolog - simulate stack
   movupd dXMM4, xmm4;
   movupd dXMM7, xmm7;

   //iters := -width*sizeof(double);
   imul r8, -8;

   // helper registers for the mt1, mt2 and dest pointers
   sub rcx, r8;

   xorpd xmm0, xmm0;

   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r8;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [rcx + rax];

           // mul add:
           movapd xmm7, [rcx + rax - 128];
           mulpd xmm7, xmm7;
           addpd xmm0, xmm7;

           movapd xmm1, [rcx + rax - 112];
           mulpd xmm1, xmm1;
           addpd xmm0, xmm1;

           movapd xmm2, [rcx + rax - 96];
           mulpd xmm2, xmm2;
           addpd xmm0, xmm2;

           movapd xmm3, [rcx + rax - 80];
           mulpd xmm3, xmm3;
           addpd xmm0, xmm3;

           movapd xmm7, [rcx + rax - 64];
           mulpd xmm7, xmm7;
           addpd xmm0, xmm7;

           movapd xmm1, [rcx + rax - 48];
           mulpd xmm1, xmm1;
           addpd xmm0, xmm1;

           movapd xmm2, [rcx + rax - 32];
           mulpd xmm2, xmm2;
           addpd xmm0, xmm2;

           movapd xmm3, [rcx + rax - 16];
           mulpd xmm3, xmm3;
           addpd xmm0, xmm3;
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           movapd xmm7, [rcx + rax];
           mulpd xmm7, xmm7;
           addpd xmm0, xmm7;
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // next line:
       add rcx, rdx;

   // loop y end
   dec r9;
   jnz @@addforyloop;

   // build result
   movhlps xmm1, xmm0;
   addsd xmm0, xmm1;

   // epilog claenup stack
   movupd xmm4, dXMM4;
   movupd xmm7, dXMM7;
end;

function ASMMatrixElementwiseNorm2UnAlignedEvenW(dest : PDouble; const LineWidth : NativeInt; Width, height : NativeInt) : double; {$IFDEF FPC}assembler;{$ENDIF}
var dXMM4, dXMM7 : Array[0..1] of double;
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

   // note: RCX = dest, RDX = destLineWidth, R8 = widh, R9 = height
   // prolog - simulate stack
   movupd dXMM4, xmm4;
   movupd dXMM7, xmm7;

   //iters := -width*sizeof(double);
   imul r8, -8;

   // helper registers for the mt1, mt2 and dest pointers
   sub rcx, r8;

   xorpd xmm0, xmm0;

   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r8;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // mul add:
           movupd xmm7, [rcx + rax - 128];
           mulpd xmm7, xmm7;
           addpd xmm0, xmm7;

           movupd xmm1, [rcx + rax - 112];
           mulpd xmm1, xmm1;
           addpd xmm0, xmm1;

           movupd xmm2, [rcx + rax - 96];
           mulpd xmm2, xmm2;
           addpd xmm0, xmm2;

           movupd xmm3, [rcx + rax - 80];
           mulpd xmm3, xmm3;
           addpd xmm0, xmm3;

           movupd xmm7, [rcx + rax - 64];
           mulpd xmm7, xmm7;
           addpd xmm0, xmm7;

           movupd xmm1, [rcx + rax - 48];
           mulpd xmm1, xmm1;
           addpd xmm0, xmm1;

           movupd xmm2, [rcx + rax - 32];
           mulpd xmm2, xmm2;
           addpd xmm0, xmm2;

           movupd xmm3, [rcx + rax - 16];
           mulpd xmm3, xmm3;
           addpd xmm0, xmm3;
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           movupd xmm7, [rcx + rax];
           mulpd xmm7, xmm7;
           addpd xmm0, xmm7;
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // next line:
       add rcx, rdx;

   // loop y end
   dec r9;
   jnz @@addforyloop;

   // build result
   movhlps xmm1, xmm0;
   addsd xmm0, xmm1;

   // epilog claenup stack
   movupd xmm4, dXMM4;
   movupd xmm7, dXMM7;
end;


function ASMMatrixElementwiseNorm2AlignedOddW(dest : PDouble; const LineWidth : NativeInt; Width, height : NativeInt) : double; {$IFDEF FPC}assembler;{$ENDIF}
var dXMM4, dXMM7 : Array[0..1] of double;
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

   // note: RCX = dest, RDX = destLineWidth, R8 = widh, R9 = height
   // prolog - simulate stack
   movupd dXMM4, xmm4;
   movupd dXMM7, xmm7;

   //iters := -(width - 1)*sizeof(double);
   dec r8;
   imul r8, -8;

   // helper registers for the mt1, mt2 and dest pointers
   sub rcx, r8;

   xorpd xmm0, xmm0;

   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r8;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;
           // prefetch data...
           // prefetch [rcx + rax];

           // mul add:
           movapd xmm7, [rcx + rax - 128];
           mulpd xmm7, xmm7;
           addpd xmm0, xmm7;

           movapd xmm1, [rcx + rax - 112];
           mulpd xmm1, xmm1;
           addpd xmm0, xmm1;

           movapd xmm2, [rcx + rax - 96];
           mulpd xmm2, xmm2;
           addpd xmm0, xmm2;

           movapd xmm3, [rcx + rax - 80];
           mulpd xmm3, xmm3;
           addpd xmm0, xmm3;

           movapd xmm7, [rcx + rax - 64];
           mulpd xmm7, xmm7;
           addpd xmm0, xmm7;

           movapd xmm1, [rcx + rax - 48];
           mulpd xmm1, xmm1;
           addpd xmm0, xmm1;

           movapd xmm2, [rcx + rax - 32];
           mulpd xmm2, xmm2;
           addpd xmm0, xmm2;

           movapd xmm3, [rcx + rax - 16];
           mulpd xmm3, xmm3;
           addpd xmm0, xmm3;
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           movapd xmm7, [rcx + rax];
           mulpd xmm7, xmm7;
           addpd xmm0, xmm7;
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // handle last element differently
       movsd xmm1, [rcx + rax];
       mulsd xmm1, xmm1;
       addsd xmm0, xmm1;

       // next line:
       add rcx, rdx;

   // loop y end
   dec r9;
   jnz @@addforyloop;

   // build result
   movhlps xmm1, xmm0;
   addsd xmm0, xmm1;

   // epilog claenup stack
   movupd xmm4, dXMM4;
   movupd xmm7, dXMM7;
end;

function ASMMatrixElementwiseNorm2UnAlignedOddW(dest : PDouble; const LineWidth : NativeInt; Width, height : NativeInt) : double; {$IFDEF FPC}assembler;{$ENDIF}
var dXMM4, dXMM7 : Array[0..1] of double;
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

   // note: RCX = dest, RDX = destLineWidth, R8 = widh, R9 = height
   // prolog - simulate stack
   movupd dXMM4, xmm4;
   movupd dXMM7, xmm7;

   //iters := -width*sizeof(double);
   dec r8;
   imul r8, -8;

   // helper registers for the mt1, mt2 and dest pointers
   sub rcx, r8;

   xorpd xmm0, xmm0;

   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r8;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // addition:
           movupd xmm7, [rcx + rax - 128];
           mulpd xmm7, xmm7;
           addpd xmm0, xmm7;

           movupd xmm1, [rcx + rax - 112];
           mulpd xmm1, xmm1;
           addpd xmm0, xmm1;

           movupd xmm2, [rcx + rax - 96];
           mulpd xmm2, xmm2;
           addpd xmm0, xmm2;

           movupd xmm3, [rcx + rax - 80];
           mulpd xmm3, xmm3;
           addpd xmm0, xmm3;

           movupd xmm7, [rcx + rax - 64];
           mulpd xmm7, xmm7;
           addpd xmm0, xmm7;

           movupd xmm1, [rcx + rax - 48];
           mulpd xmm1, xmm1;
           addpd xmm0, xmm1;

           movupd xmm2, [rcx + rax - 32];
           mulpd xmm2, xmm2;
           addpd xmm0, xmm2;

           movupd xmm3, [rcx + rax - 16];
           mulpd xmm3, xmm3;
           addpd xmm0, xmm3;
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           movupd xmm7, [rcx + rax];
           mulpd xmm7, xmm7;
           addpd xmm0, xmm7;
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // handle last element differently
       movsd xmm1, [rcx + rax];
       mulsd xmm1, xmm1;
       addsd xmm0, xmm1;

       // next line:
       add rcx, rdx;

   // loop y end
   dec r9;
   jnz @@addforyloop;

   // build result
   movhlps xmm1, xmm0;
   addsd xmm0, xmm1;

   // epilog claenup stack
   movupd xmm4, dXMM4;
   movupd xmm7, dXMM7;
end;

procedure ASMMatrixNormalizeRowAlignedEvenW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}
var dXMM4, dXMM7 : Array[0..1] of double;
    {$ifdef UNIX}
    width : NativeInt;
    height : NativeInt;
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

   // note: RCX = dest, RDX = destLineWidth, R8 = src
   // prolog - simulate stack
   movupd dXMM4, xmm4;
   movupd dXMM7, xmm7;

   //iters := -width*sizeof(double);
   mov r10, width;
   imul r10, -8;

   // helper registers for the mt1, mt2 and dest pointers
   sub rcx, r10;
   sub r8, r10;

   mov r11, Height;
   @@addforyloop:
       xorpd xmm0, xmm0;

       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [r8 + rax];

           // mul add:
           movapd xmm7, [r8 + rax - 128];
           mulpd xmm7, xmm7;
           addpd xmm0, xmm7;

           movapd xmm1, [r8 + rax - 112];
           mulpd xmm1, xmm1;
           addpd xmm0, xmm1;

           movapd xmm2, [r8 + rax - 96];
           mulpd xmm2, xmm2;
           addpd xmm0, xmm2;

           movapd xmm3, [r8 + rax - 80];
           mulpd xmm3, xmm3;
           addpd xmm0, xmm3;

           movapd xmm4, [r8 + rax - 64];
           mulpd xmm4, xmm4;
           addpd xmm0, xmm4;

           movapd xmm7, [r8 + rax - 48];
           mulpd xmm7, xmm7;
           addpd xmm0, xmm7;

           movapd xmm1, [r8 + rax - 32];
           mulpd xmm1, xmm1;
           addpd xmm0, xmm1;

           movapd xmm2, [r8 + rax - 16];
           mulpd xmm2, xmm2;
           addpd xmm0, xmm2;
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @buildResult;

       @addforxloop2:
           movapd xmm7, [r8 + rax];
           mulpd xmm7, xmm7;
           addpd xmm0, xmm7;
       add rax, 16;
       jnz @addforxloop2;

       @buildResult:

       // build result
       movhlps xmm1, xmm0;
       addsd xmm0, xmm1;
       sqrtsd xmm0, xmm0;

       movsd xmm1, [rip + cLocOnes];
       divsd xmm1, xmm0;

       movddup xmm2, xmm1;

       //  normalize
       mov rax, r10;
       @addforxloop3:
           add rax, 128;
           jg @loopEnd2;

           // mult:
           movapd xmm7, [r8 + rax - 128];
           mulpd xmm7, xmm2;
           movapd [rcx + rax - 128], xmm7;

           movapd xmm1, [r8 + rax - 112];
           mulpd xmm1, xmm2;
           movapd [rcx + rax - 112], xmm1;

           movapd xmm3, [r8 + rax - 96];
           mulpd xmm3, xmm2;
           movapd [rcx + rax - 96], xmm3;

           movapd xmm4, [r8 + rax - 80];
           mulpd xmm4, xmm2;
           movapd [rcx + rax - 80], xmm4;

           movapd xmm0, [r8 + rax - 64];
           mulpd xmm0, xmm2;
           movapd [rcx + rax - 64], xmm0;

           movapd xmm1, [r8 + rax - 48];
           mulpd xmm1, xmm2;
           movapd [rcx + rax - 48], xmm1;

           movapd xmm3, [r8 + rax - 32];
           mulpd xmm3, xmm2;
           movapd [rcx + rax - 32], xmm3;

           movapd xmm4, [r8 + rax - 16];
           mulpd xmm4, xmm2;
           movapd [rcx + rax - 16], xmm4;
       jmp @addforxloop3

       @loopEnd2:

       sub rax, 128;

       jz @nextLine2;

       @addforxloop4:
           movapd xmm7, [r8 + rax];
           mulpd xmm7, xmm2;
           movapd [rcx + rax], xmm7;
       add rax, 16;
       jnz @addforxloop4;

       @nextLine2:

       // next line:
       add rcx, rdx;
       add r8, r9;

   // loop y end
   dec r11;
   jnz @@addforyloop;

   // epilog claenup stack
   movupd xmm4, dXMM4;
   movupd xmm7, dXMM7;
end;

procedure ASMMatrixNormalizeRowUnAlignedEvenW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}
var dXMM4, dXMM7 : Array[0..1] of double;
    {$ifdef UNIX}
    width : NativeInt;
    height : NativeInt;
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

   // note: RCX = dest, RDX = destLineWidth, R8 = src
   // prolog - simulate stack
   movupd dXMM4, xmm4;
   movupd dXMM7, xmm7;

   //iters := -width*sizeof(double);
   mov r10, width;
   imul r10, -8;

   // helper registers for the mt1, mt2 and dest pointers
   sub rcx, r10;
   sub r8, r10;

   mov r11, Height;
   @@addforyloop:
       xorpd xmm0, xmm0;

       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // mul add:
           movupd xmm7, [r8 + rax - 128];
           mulpd xmm7, xmm7;
           addpd xmm0, xmm7;

           movupd xmm1, [r8 + rax - 112];
           mulpd xmm1, xmm1;
           addpd xmm0, xmm1;

           movupd xmm2, [r8 + rax - 96];
           mulpd xmm2, xmm2;
           addpd xmm0, xmm2;

           movupd xmm3, [r8 + rax - 80];
           mulpd xmm3, xmm3;
           addpd xmm0, xmm3;

           movupd xmm4, [r8 + rax - 64];
           mulpd xmm4, xmm4;
           addpd xmm0, xmm4;

           movupd xmm7, [r8 + rax - 48];
           mulpd xmm7, xmm7;
           addpd xmm0, xmm7;

           movupd xmm1, [r8 + rax - 32];
           mulpd xmm1, xmm1;
           addpd xmm0, xmm1;

           movupd xmm2, [r8 + rax - 16];
           mulpd xmm2, xmm2;
           addpd xmm0, xmm2;
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @buildResult;

       @addforxloop2:
           movupd xmm7, [r8 + rax];
           mulpd xmm7, xmm7;
           addpd xmm0, xmm7;
       add rax, 16;
       jnz @addforxloop2;

       @buildResult:

       // build result
       movhlps xmm1, xmm0;
       addsd xmm0, xmm1;
       sqrtsd xmm0, xmm0;

       movsd xmm1, [rip + cLocOnes];
       divsd xmm1, xmm0;

       movddup xmm2, xmm1;

       //  normalize
       mov rax, r10;
       @addforxloop3:
           add rax, 128;
           jg @loopEnd2;

           // mult:
           movupd xmm7, [r8 + rax - 128];
           mulpd xmm7, xmm2;
           movupd [rcx + rax - 128], xmm7;

           movupd xmm1, [r8 + rax - 112];
           mulpd xmm1, xmm2;
           movupd [rcx + rax - 112], xmm1;

           movupd xmm3, [r8 + rax - 96];
           mulpd xmm3, xmm2;
           movupd [rcx + rax - 96], xmm3;

           movupd xmm4, [r8 + rax - 80];
           mulpd xmm4, xmm2;
           movupd [rcx + rax - 80], xmm4;

           movupd xmm0, [r8 + rax - 64];
           mulpd xmm0, xmm2;
           movupd [rcx + rax - 64], xmm0;

           movupd xmm1, [r8 + rax - 48];
           mulpd xmm1, xmm2;
           movupd [rcx + rax - 48], xmm1;

           movupd xmm3, [r8 + rax - 32];
           mulpd xmm3, xmm2;
           movupd [rcx + rax - 32], xmm3;

           movupd xmm4, [r8 + rax - 16];
           mulpd xmm4, xmm2;
           movupd [rcx + rax - 16], xmm4;
       jmp @addforxloop3

       @loopEnd2:

       sub rax, 128;

       jz @nextLine2;

       @addforxloop4:
           movupd xmm7, [r8 + rax];
           mulpd xmm7, xmm2;
           movupd [rcx + rax], xmm7;
       add rax, 16;
       jnz @addforxloop4;

       @nextLine2:

       // next line:
       add rcx, rdx;
       add r8, r9;

   // loop y end
   dec r11;
   jnz @@addforyloop;


   // epilog claenup stack
   movupd xmm4, dXMM4;
   movupd xmm7, dXMM7;
end;

procedure ASMMatrixNormalizeRowAlignedOddW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}
var dXMM4, dXMM7 : Array[0..1] of double;
    {$ifdef UNIX}
    width : NativeInt;
    height : NativeInt;
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

   // note: RCX = dest, RDX = destLineWidth, R8 = widh, R9 = height
   // prolog - simulate stack
   movupd dXMM4, xmm4;
   movupd dXMM7, xmm7;

   //iters := -(width - 1)*sizeof(double);
   mov r10, width;
   dec r10;
   imul r10, -8;

   // helper registers for the mt1, mt2 and dest pointers
   sub rcx, r10;
   sub r8, r10;

   mov r11, Height;
   @@addforyloop:
       xorpd xmm0, xmm0;

       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [r8 + rax];

           // mult add:
           movapd xmm7, [r8 + rax - 128];
           mulpd xmm7, xmm7;
           addpd xmm0, xmm7;

           movapd xmm1, [r8 + rax - 112];
           mulpd xmm1, xmm1;
           addpd xmm0, xmm1;

           movapd xmm2, [r8 + rax - 96];
           mulpd xmm2, xmm2;
           addpd xmm0, xmm2;

           movapd xmm3, [r8 + rax - 80];
           mulpd xmm3, xmm3;
           addpd xmm0, xmm3;

           movapd xmm7, [r8 + rax - 64];
           mulpd xmm7, xmm7;
           addpd xmm0, xmm7;

           movapd xmm1, [r8 + rax - 48];
           mulpd xmm1, xmm1;
           addpd xmm0, xmm1;

           movapd xmm2, [r8 + rax - 32];
           mulpd xmm2, xmm2;
           addpd xmm0, xmm2;

           movapd xmm3, [r8 + rax - 16];
           mulpd xmm3, xmm3;
           addpd xmm0, xmm3;
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @buildResult;

       @addforxloop2:
           movapd xmm7, [r8 + rax];
           mulpd xmm7, xmm7;
           addpd xmm0, xmm7;
       add rax, 16;
       jnz @addforxloop2;

       @buildResult:

       // handle last element differently
       movsd xmm7, [r8 + rax];
       mulsd xmm7, xmm7;
       addsd xmm0, xmm7;

       // build result
       movhlps xmm1, xmm0;
       addsd xmm0, xmm1;
       sqrtsd xmm0, xmm0;

       movsd xmm1, [rip + cLocOnes];
       divsd xmm1, xmm0;

       movddup xmm4, xmm1;

       //  normalize
       mov rax, r10;
       @addforxloop3:
           add rax, 128;
           jg @loopEnd2;

           // prefetch data...
           // prefetch [r8 + rax];
           // prefetchw [rcx + rax];

           // mult:
           movapd xmm7, [r8 + rax - 128];
           mulpd xmm7, xmm4;
           movapd [rcx + rax - 128], xmm7;

           movapd xmm1, [r8 + rax - 112];
           mulpd xmm1, xmm4;
           movapd [rcx + rax - 112], xmm1;

           movapd xmm2, [r8 + rax - 96];
           mulpd xmm2, xmm4;
           movapd [rcx + rax - 96], xmm2;

           movapd xmm3, [r8 + rax - 80];
           mulpd xmm3, xmm4;
           movapd [rcx + rax - 80], xmm3;

           movapd xmm0, [r8 + rax - 64];
           mulpd xmm0, xmm4;
           movapd [rcx + rax - 64], xmm0;

           movapd xmm1, [r8 + rax - 48];
           mulpd xmm1, xmm4;
           movapd [rcx + rax - 48], xmm1;

           movapd xmm2, [r8 + rax - 32];
           mulpd xmm2, xmm4;
           movapd [rcx + rax - 32], xmm2;

           movapd xmm3, [r8 + rax - 16];
           mulpd xmm3, xmm4;
           movapd [rcx + rax - 16], xmm3;
       jmp @addforxloop3

       @loopEnd2:

       sub rax, 128;

       jz @nextLine2;

       @addforxloop4:
           movapd xmm7, [r8 + rax];
           mulpd xmm7, xmm4;
           movapd [rcx + rax], xmm7;
       add rax, 16;
       jnz @addforxloop4;

       @nextLine2:

       // handle last element differently
       movsd xmm7, [r8 + rax];
       mulsd xmm7, xmm4;
       movsd [rcx + rax], xmm7;

       // next line:
       add rcx, rdx;
       add r8, r9;

   // loop y end
   dec r11;
   jnz @@addforyloop;

   // epilog claenup stack
   movupd xmm4, dXMM4;
   movupd xmm7, dXMM7;
end;

procedure ASMMatrixNormalizeRowUnAlignedOddW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}
var dXMM4, dXMM7 : Array[0..1] of double;
    {$ifdef UNIX}
    width : NativeInt;
    height : NativeInt;
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

   // note: RCX = dest, RDX = destLineWidth, R8 = widh, R9 = height
   // prolog - simulate stack
   movupd dXMM4, xmm4;
   movupd dXMM7, xmm7;

   //iters := -(width - 1)*sizeof(double);
   mov r10, width;
   dec r10;
   imul r10, -8;

   // helper registers for the mt1, mt2 and dest pointers
   sub rcx, r10;
   sub r8, r10;

   mov r11, Height;
   @@addforyloop:
       xorpd xmm0, xmm0;

       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // mult add:
           movupd xmm7, [r8 + rax - 128];
           mulpd xmm7, xmm7;
           addpd xmm0, xmm7;

           movupd xmm1, [r8 + rax - 112];
           mulpd xmm1, xmm1;
           addpd xmm0, xmm1;

           movupd xmm2, [r8 + rax - 96];
           mulpd xmm2, xmm2;
           addpd xmm0, xmm2;

           movupd xmm3, [r8 + rax - 80];
           mulpd xmm3, xmm3;
           addpd xmm0, xmm3;

           movupd xmm7, [r8 + rax - 64];
           mulpd xmm7, xmm7;
           addpd xmm0, xmm7;

           movupd xmm1, [r8 + rax - 48];
           mulpd xmm1, xmm1;
           addpd xmm0, xmm1;

           movupd xmm2, [r8 + rax - 32];
           mulpd xmm2, xmm2;
           addpd xmm0, xmm2;

           movupd xmm3, [r8 + rax - 16];
           mulpd xmm3, xmm3;
           addpd xmm0, xmm3;
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @buildResult;

       @addforxloop2:
           movupd xmm7, [r8 + rax];
           mulpd xmm7, xmm7;
           addpd xmm0, xmm7;
       add rax, 16;
       jnz @addforxloop2;

       @buildResult:

       // handle last element differently
       movsd xmm7, [r8 + rax];
       mulsd xmm7, xmm7;
       addsd xmm0, xmm7;

       // build result
       movhlps xmm1, xmm0;
       addsd xmm0, xmm1;
       sqrtsd xmm0, xmm0;

       movsd xmm1, [rip + cLocOnes];
       divsd xmm1, xmm0;

       movddup xmm4, xmm1;

       //  normalize
       mov rax, r10;
       @addforxloop3:
           add rax, 128;
           jg @loopEnd2;

           // mult:
           movupd xmm7, [r8 + rax - 128];
           mulpd xmm7, xmm4;
           movupd [rcx + rax - 128], xmm7;

           movupd xmm1, [r8 + rax - 112];
           mulpd xmm1, xmm4;
           movupd [rcx + rax - 112], xmm1;

           movupd xmm2, [r8 + rax - 96];
           mulpd xmm2, xmm4;
           movupd [rcx + rax - 96], xmm2;

           movupd xmm3, [r8 + rax - 80];
           mulpd xmm3, xmm4;
           movupd [rcx + rax - 80], xmm3;

           movupd xmm0, [r8 + rax - 64];
           mulpd xmm0, xmm4;
           movupd [rcx + rax - 64], xmm0;

           movupd xmm1, [r8 + rax - 48];
           mulpd xmm1, xmm4;
           movupd [rcx + rax - 48], xmm1;

           movupd xmm2, [r8 + rax - 32];
           mulpd xmm2, xmm4;
           movupd [rcx + rax - 32], xmm2;

           movupd xmm3, [r8 + rax - 16];
           mulpd xmm3, xmm4;
           movupd [rcx + rax - 16], xmm3;
       jmp @addforxloop3

       @loopEnd2:

       sub rax, 128;

       jz @nextLine2;

       @addforxloop4:
           movupd xmm7, [r8 + rax];
           mulpd xmm7, xmm4;
           movupd [rcx + rax], xmm7;
       add rax, 16;
       jnz @addforxloop4;

       @nextLine2:

       // handle last element differently
       movsd xmm7, [r8 + rax];
       mulsd xmm7, xmm4;
       movsd [rcx + rax], xmm7;

       // next line:
       add rcx, rdx;
       add r8, r9;

   // loop y end
   dec r11;
   jnz @@addforyloop;

   // epilog claenup stack
   movupd xmm4, dXMM4;
   movupd xmm7, dXMM7;
end;

procedure ASMMatrixNormalizeColumnAlignedEvenW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}
var iRBX, iRSI : NativeInt;
    {$ifdef UNIX}
    width : NativeInt;
    height : NativeInt;
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

   // rcx = dest, rdx = destlinewidth, r8 = src, r9 = srclinewidth
   // prolog - simulate stack
   mov iRBX, rbx;
   mov iRSI, rsi;

   // iter := width*srcLineWidth
   mov r10, height;
   imul r10, r9;

   // iters2 := height*destLineWidth;
   mov r11, height;
   imul r11, rdx;

   //note: FPC does not like that syntax sub dest, destLineWidth;
   sub rcx, rdx;
   // sub src, srcLineWidth;
   sub r8, r9;

   mov rbx, Width;
   sar rbx, 1;
   @@addforxloop:
       xorpd xmm2, xmm2;

       // for y := 0 to height - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforyloop:
           movapd xmm0, [r8 + rax];
           mulpd xmm0, xmm0;
           addpd xmm2, xmm0;
       sub rax, r9;
       jnz @addforyloop;

       // build result
       sqrtpd xmm2, xmm2;
       movddup xmm3, [rip + cLocOnes];
       divpd xmm3, xmm2;

       // multiply the result and build the result
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r10;
       mov rsi, r11;

       @addforyloop2:
           movapd xmm0, [r8 + rax];
           mulpd xmm0, xmm3;
           movapd [rcx + rsi], xmm0;
           sub rsi, rdx;
       sub rax, r9;
       jnz @addforyloop2;

       // next columns:
       add rcx, 16;
       add r8, 16;

   // loop x end
   dec rbx;
   jnz @@addforxloop;

   // epliog cleanup stack
   mov rbx, iRBX;
   mov rsi, iRSI;
end;

procedure ASMMatrixNormalizeColumnUnAlignedEvenW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}
var iRBX, iRSI : NativeInt;
    {$ifdef UNIX}
    width : NativeInt;
    height : NativeInt;
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

   // rcx = dest, rdx = destlinewidth, r8 = src, r9 = srclinewidth
   // prolog - simulate stack
   mov iRBX, rbx;
   mov iRSI, rsi;

   // iter := width*srcLineWidth
   mov r10, height;
   imul r10, r9;

   // iters2 := height*destLineWidth;
   mov r11, height;
   imul r11, rdx;

   //note: FPC does not like that syntax sub dest, destLineWidth;
   sub rcx, rdx;
   // sub src, srcLineWidth;
   sub r8, r9;

   mov rbx, Width;
   sar rbx, 1;
   @@addforxloop:
       xorpd xmm2, xmm2;

       // for y := 0 to height - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforyloop:
           movupd xmm0, [r8 + rax];
           mulpd xmm0, xmm0;
           addpd xmm2, xmm0;
       sub rax, r9;
       jnz @addforyloop;

       // build result
       sqrtpd xmm2, xmm2;
       movddup xmm3, [rip + cLocOnes];
       divpd xmm3, xmm2;

       // multiply the result and build the result
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r10;
       mov rsi, r11;

       @addforyloop2:
           movupd xmm0, [r8 + rax];
           mulpd xmm0, xmm3;
           movupd [rcx + rsi], xmm0;
           sub rsi, rdx;
       sub rax, r9;
       jnz @addforyloop2;

       // next columns:
       add rcx, 16;
       add r8, 16;

   // loop x end
   dec rbx;
   jnz @@addforxloop;

   // epliog cleanup stack
   mov rbx, iRBX;
   mov rsi, iRSI;
end;


procedure ASMMatrixNormalizeColumnAlignedOddW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}
var iRBX, iRSI : NativeInt;
    {$ifdef UNIX}
    width : NativeInt;
    height : NativeInt;
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

   // rcx = dest, rdx = destlinewidth, r8 = src, r9 = srclinewidth
   // prolog - simulate stack
   mov iRBX, rbx;
   mov iRSI, rsi;

   // iter := width*srcLineWidth
   mov r10, height;
   imul r10, r9;

   // iters2 := height*destLineWidth;
   mov r11, height;
   imul r11, rdx;

   //note: FPC does not like that syntax sub dest, destLineWidth;
   sub rcx, rdx;
   // sub src, srcLineWidth;
   sub r8, r9;

   mov rbx, Width;
   sar rbx, 1;
   jz @lastColumn;
   @@addforxloop:
       xorpd xmm2, xmm2;

       // for y := 0 to height - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforyloop:
           movapd xmm0, [r8 + rax];
           mulpd xmm0, xmm0;
           addpd xmm2, xmm0;
       sub rax, r9;
       jnz @addforyloop;

       // build result
       sqrtpd xmm2, xmm2;
       movddup xmm3, [rip + cLocOnes];
       divpd xmm3, xmm2;

       // multiply the result and build the result
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r10;
       mov rsi, r11;

       @addforyloop2:
           movapd xmm0, [r8 + rax];
           mulpd xmm0, xmm3;
           movapd [rcx + rsi], xmm0;
           sub rsi, rdx;
       sub rax, r9;
       jnz @addforyloop2;

       // next columns:
       add rcx, 16;
       add r8, 16;

   // loop x end
   dec rbx;
   jnz @@addforxloop;

   // handle last column
   @lastColumn:
   xorpd xmm2, xmm2;

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov rax, r10;
   @addforyloop3:
       movsd xmm0, [r8 + rax];
       mulsd xmm0, xmm0;
       addsd xmm2, xmm0;
   sub rax, r9;
   jnz @addforyloop3;

   // build result
   sqrtsd xmm2, xmm2;
   movsd xmm3, [rip + cLocOnes];
   divsd xmm3, xmm2;

   // multiply the result and build the result
   // for x := 0 to w - 1;
   // prepare for reverse loop
   mov rax, r10;
   mov rsi, r11;

   @addforyloop4:
       movsd xmm0, [r8 + rax];
       mulsd xmm0, xmm3;
       movsd [rcx + rsi], xmm0;
       sub rsi, rdx;
   sub rax, r9;
   jnz @addforyloop4;

   // epliog cleanup stack
   mov rbx, iRBX;
   mov rsi, iRSI;
end;

procedure ASMMatrixNormalizeColumnUnAlignedOddW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}
var iRBX, iRSI : NativeInt;
    {$ifdef UNIX}
    width : NativeInt;
    height : NativeInt;
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

   // rcx = dest, rdx = destlinewidth, r8 = src, r9 = srclinewidth
   // prolog - simulate stack
   mov iRBX, rbx;
   mov iRSI, rsi;

   // iter := width*srcLineWidth
   mov r10, height;
   imul r10, r9;

   // iters2 := height*destLineWidth;
   mov r11, height;
   imul r11, rdx;

   //note: FPC does not like that syntax sub dest, destLineWidth;
   sub rcx, rdx;
   // sub src, srcLineWidth;
   sub r8, r9;

   mov rbx, Width;
   sar rbx, 1;
   jz @lastColumn;
   @@addforxloop:
       xorpd xmm2, xmm2;

       // for y := 0 to height - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforyloop:
           movupd xmm0, [r8 + rax];
           mulpd xmm0, xmm0;
           addpd xmm2, xmm0;
       sub rax, r9;
       jnz @addforyloop;

       // build result
       sqrtpd xmm2, xmm2;
       movddup xmm3, [rip + cLocOnes];
       divpd xmm3, xmm2;

       // multiply the result and build the result
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r10;
       mov rsi, r11;

       @addforyloop2:
           movupd xmm0, [r8 + rax];
           mulpd xmm0, xmm3;
           movupd [rcx + rsi], xmm0;
           sub rsi, rdx;
       sub rax, r9;
       jnz @addforyloop2;

       // next columns:
       add rcx, 16;
       add r8, 16;

   // loop x end
   dec rbx;
   jnz @@addforxloop;

   // handle last column
   @lastColumn:
   xorpd xmm2, xmm2;

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov rax, r10;
   @addforyloop3:
       movsd xmm0, [r8 + rax];
       mulsd xmm0, xmm0;
       addsd xmm2, xmm0;
   sub rax, r9;
   jnz @addforyloop3;

   // build result
   sqrtsd xmm2, xmm2;
   movsd xmm3, [rip + cLocOnes];
   divsd xmm3, xmm2;

   // multiply the result and build the result
   // for x := 0 to w - 1;
   // prepare for reverse loop
   mov rax, r10;
   mov rsi, r11;

   @addforyloop4:
       movsd xmm0, [r8 + rax];
       mulsd xmm0, xmm3;
       movsd [rcx + rsi], xmm0;
       sub rsi, rdx;
   sub rax, r9;
   jnz @addforyloop4;

   // epliog cleanup stack
   mov rbx, iRBX;
   mov rsi, iRSI;
end;

{$ENDIF}

end.
