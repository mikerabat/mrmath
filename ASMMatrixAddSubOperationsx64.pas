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


unit ASMMatrixAddSubOperationsx64;

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

procedure ASMMatrixAddAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; {$ifdef UNIX}unixWidth{$ELSE}width{$endif} : TASMNativeInt; {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}
procedure ASMMatrixAddUnAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; {$ifdef UNIX}unixWidth{$ELSE}width{$endif} : TASMNativeInt; {$ifdef UNIX}unixheight{$ELSE}height{$endif} : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}

procedure ASMMatrixAddAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; {$ifdef UNIX}unixWidth{$ELSE}width{$endif} : TASMNativeInt; {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}
procedure ASMMatrixAddUnAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; {$ifdef UNIX}unixWidth{$ELSE}width{$endif} : TASMNativeInt; {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}

procedure ASMMatrixSubAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; {$ifdef UNIX}unixWidth{$ELSE}width{$endif} : TASMNativeInt; {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}
procedure ASMMatrixSubUnAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; {$ifdef UNIX}unixWidth{$ELSE}width{$endif} : TASMNativeInt; {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}

procedure ASMMatrixSubAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; {$ifdef UNIX}unixWidth{$ELSE}width{$endif} : TASMNativeInt; {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}
procedure ASMMatrixSubUnAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; {$ifdef UNIX}unixWidth{$ELSE}width{$endif} : TASMNativeInt; {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}

procedure ASMMatrixSubT(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; LineWidthB : TASMNativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}

procedure ASMMatrixSubVecAlignedVecRow(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; incX : TASMNativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}
procedure ASMMatrixSubVecAlignedRow(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; incX : TASMNativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}
procedure ASMMatrixSubVecAlignedCol(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; incX : TASMNativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}

procedure ASMMatrixSubVecUnalignedVecRow(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; incX : TASMNativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}
procedure ASMMatrixSubVecUnalignedRow(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; incX : TASMNativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}
procedure ASMMatrixSubVecUnalignedCol(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; incX : TASMNativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}

procedure ASMMatrixAddVecAlignedVecRow(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; incX : TASMNativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}
procedure ASMMatrixAddVecAlignedRow(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; incX : TASMNativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}
procedure ASMMatrixAddVecAlignedCol(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; incX : TASMNativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}

procedure ASMMatrixAddVecUnalignedVecRow(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; incX : TASMNativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}
procedure ASMMatrixAddVecUnalignedRow(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; incX : TASMNativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}
procedure ASMMatrixAddVecUnalignedCol(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; incX : TASMNativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}

procedure ASMVecAddAlignedSeq( X : PDouble; y : PDouble; N : TASMNativeInt; alpha : double ); {$IFDEF FPC}assembler;{$ENDIF}
procedure ASMVecAddUnAlignedSeq( X : PDouble; y : PDouble; N : TASMNativeInt; alpha : double ); {$IFDEF FPC}assembler;{$ENDIF}

procedure ASMVecAddNonSeq( X : PDouble; y : PDouble; N : TASMNativeInt; incX, {$ifdef UNIX}unixIncY{$ELSE}incY {$ENDIF} : TASMNativeInt; alpha : double ); {$IFDEF FPC}assembler;{$ENDIF}


{$ENDIF}

implementation

{$IFDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$S-} {$ENDIF}

procedure ASMMatrixAddAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; {$ifdef UNIX}unixWidth{$ELSE}width{$endif} : TASMNativeInt; {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}
var iRBX, iR12 : TASMNativeInt;
    {$ifdef UNIX}
    width : TASMNativeInt;
    height : TASMNativeInt;
    {$ENDIF}
asm
   {$IFDEF UNIX}
   // Linux uses a diffrent ABI -> copy over the registers so they meet with winABI
   // The parameters are passed in the following order:
   // RDI, RSI, RDX, RCX, r8, r9 -> mov to RCX, RDX, R8, R9, width and height
   mov width, r8;
   mov height, r9;
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
   imul r10, -8;

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
           movapd xmm0, [r8 + rax - 128];
           addpd xmm0, [r9 + rax - 128];

           movapd [rcx + rax - 128], xmm0;

           movapd xmm1, [r8 + rax - 112];
           addpd xmm1, [r9 + rax - 112];

           movapd [rcx + rax - 112], xmm1;

           movapd xmm2, [r8 + rax - 96];
           addpd xmm2, [r9 + rax - 96];

           movapd [rcx + rax - 96], xmm2;

           movapd xmm3, [r8 + rax - 80];
           addpd xmm3, [r9 + rax - 80];

           movapd [rcx + rax - 80], xmm3;

           movapd xmm0, [r8 + rax - 64];
           addpd xmm0, [r9 + rax - 64];

           movapd [rcx + rax - 64], xmm0;

           movapd xmm1, [r8 + rax - 48];
           addpd xmm1, [r9 + rax - 48];

           movapd [rcx + rax - 48], xmm1;

           movapd xmm2, [r8 + rax - 32];
           addpd xmm2, [r9 + rax - 32];

           movapd [rcx + rax - 32], xmm2;

           movapd xmm3, [r8 + rax - 16];
           addpd xmm3, [r9 + rax - 16];

           movapd [rcx + rax - 16], xmm3;
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           movapd xmm0, [r8 + rax];
           addpd xmm0, [r9 + rax];

           movapd [rcx + rax], xmm0;
       add rax, 16;
       jnz @addforxloop2;

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
end;

procedure ASMMatrixAddUnAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; {$ifdef UNIX}unixWidth{$ELSE}width{$endif} : TASMNativeInt; {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}
var iRBX, iR12 : TASMNativeInt;
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

   // note: RCX = dest, RDX = destLineWidth, R8 = mt1, R9 = mt2
   // prolog - simulate stack
   mov iRBX, rbx;
   mov iR12, r12;

   mov r11, LineWidth1;
   mov r12, LineWidth2;

   //iters := -width*sizeof(double);
   mov r10, width;
   shl r10, 3;
   imul r10, -1;

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
           movupd xmm0, [r8 + rax - 128];
           movupd xmm1, [r9 + rax - 128];
           addpd xmm0, xmm1;

           movupd [rcx + rax - 128], xmm0;

           movupd xmm0, [r8 + rax - 112];
           movupd xmm1, [r9 + rax - 112];
           addpd xmm0, xmm1;

           movupd [rcx + rax - 112], xmm0;

           movupd xmm0, [r8 + rax - 96];
           movupd xmm1, [r9 + rax - 96];
           addpd xmm0, xmm1;

           movupd [rcx + rax - 96], xmm0;

           movupd xmm0, [r8 + rax - 80];
           movupd xmm1, [r9 + rax - 80];
           addpd xmm0, xmm1;

           movupd [rcx + rax - 80], xmm0;

           movupd xmm0, [r8 + rax - 64];
           movupd xmm1, [r9 + rax - 64];
           addpd xmm0, xmm1;

           movupd [rcx + rax - 64], xmm0;

           movupd xmm0, [r8 + rax - 48];
           movupd xmm1, [r9 + rax - 48];
           addpd xmm0, xmm1;

           movupd [rcx + rax - 48], xmm0;

           movupd xmm0, [r8 + rax - 32];
           movupd xmm1, [r9 + rax - 32];
           addpd xmm0, xmm1;

           movupd [rcx + rax - 32], xmm0;

           movupd xmm0, [r8 + rax - 16];
           movupd xmm1, [r9 + rax - 16];
           addpd xmm0, xmm1;

           movupd [rcx + rax - 16], xmm0;
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           movupd xmm0, [r8 + rax];
           movupd xmm1, [r9 + rax];
           addpd xmm0, xmm1;

           movupd [rcx + rax], xmm0;
       add rax, 16;
       jnz @addforxloop2;

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
end;

procedure ASMMatrixAddAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; {$ifdef UNIX}unixWidth{$ELSE}width{$endif} : TASMNativeInt; {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}
var iRBX, iR12 : TASMNativeInt;
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

   // note: RCX = dest, RDX = destLineWidth, R8 = mt1, R9 = mt2
   // prolog - simulate stack
   mov iRBX, rbx;
   mov iR12, r12;

   //iters := -(width - 1)*sizeof(double);
   mov r10, width;
   dec r10;
   imul r10, -8;

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
           movapd xmm0, [r8 + rax - 128];
           addpd xmm0, [r9 + rax - 128];

           movapd [rcx + rax - 128], xmm0;

           movapd xmm1, [r8 + rax - 112];
           addpd xmm1, [r9 + rax - 112];

           movapd [rcx + rax - 112], xmm1;

           movapd xmm2, [r8 + rax - 96];
           addpd xmm2, [r9 + rax - 96];

           movapd [rcx + rax - 96], xmm2;

           movapd xmm3, [r8 + rax - 80];
           addpd xmm3, [r9 + rax - 80];

           movapd [rcx + rax - 80], xmm3;

           movapd xmm0, [r8 + rax - 64];
           addpd xmm0, [r9 + rax - 64];

           movapd [rcx + rax - 64], xmm0;

           movapd xmm1, [r8 + rax - 48];
           addpd xmm1, [r9 + rax - 48];

           movapd [rcx + rax - 48], xmm1;

           movapd xmm2, [r8 + rax - 32];
           addpd xmm2, [r9 + rax - 32];

           movapd [rcx + rax - 32], xmm2;

           movapd xmm3, [r8 + rax - 16];
           addpd xmm3, [r9 + rax - 16];

           movapd [rcx + rax - 16], xmm3;
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           movapd xmm0, [r8 + rax];
           addpd xmm0, [r9 + rax];

           movapd [rcx + rax], xmm0;
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last column:
       movsd xmm0, [r8];
       addsd xmm0, [r9];

       movsd [rcx], xmm0;

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
end;

procedure ASMMatrixAddUnAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; {$ifdef UNIX}unixWidth{$ELSE}width{$endif} : TASMNativeInt; {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}
var iRBX, iR12 : TASMNativeInt;
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

   // note: RCX = dest, RDX = destLineWidth, R8 = mt1, R9 = mt2
   // prolog - simulate stack
   mov iRBX, rbx;
   mov iR12, r12;

   mov r11, LineWidth1;
   mov r12, LineWidth2;

   //iters := -(width - 1)*sizeof(double);
   mov r10, width;
   dec r10;
   imul r10, -8;

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
           movupd xmm0, [r8 + rax - 128];
           movupd xmm1, [r9 + rax - 128];
           addpd xmm0, xmm1;

           movupd [rcx + rax - 128], xmm0;

           movupd xmm0, [r8 + rax - 112];
           movupd xmm1, [r9 + rax - 112];
           addpd xmm0, xmm1;

           movupd [rcx + rax - 112], xmm0;

           movupd xmm0, [r8 + rax - 96];
           movupd xmm1, [r9 + rax - 96];
           addpd xmm0, xmm1;

           movupd [rcx + rax - 96], xmm0;

           movupd xmm0, [r8 + rax - 80];
           movupd xmm1, [r9 + rax - 80];
           addpd xmm0, xmm1;

           movupd [rcx + rax - 80], xmm0;

           movupd xmm0, [r8 + rax - 64];
           movupd xmm1, [r9 + rax - 64];
           addpd xmm0, xmm1;

           movupd [rcx + rax - 64], xmm0;

           movupd xmm0, [r8 + rax - 48];
           movupd xmm1, [r9 + rax - 48];
           addpd xmm0, xmm1;

           movupd [rcx + rax - 48], xmm0;

           movupd xmm0, [r8 + rax - 32];
           movupd xmm1, [r9 + rax - 32];
           addpd xmm0, xmm1;

           movupd [rcx + rax - 32], xmm0;

           movupd xmm0, [r8 + rax - 16];
           movupd xmm1, [r9 + rax - 16];
           addpd xmm0, xmm1;

           movupd [rcx + rax - 16], xmm0;
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           movupd xmm0, [r8 + rax];
           movupd xmm1, [r9 + rax];
           addpd xmm0, xmm1;

           movupd [rcx + rax], xmm0;
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last column:
       movsd xmm0, [r8];
       addsd xmm0, [r9];

       movsd [rcx], xmm0;

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
end;

procedure ASMMatrixSubAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; {$ifdef UNIX}unixWidth{$ELSE}width{$endif} : TASMNativeInt; {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}
var iRBX, iR12 : TASMNativeInt;
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

   // note: RCX = dest, RDX = destLineWidth, R8 = mt1, R9 = mt2
   // prolog - simulate stack
   mov iRBX, rbx;
   mov iR12, r12;

   //iters := -width*sizeof(double);
   mov r10, width;
   imul r10, -8;

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
           movapd xmm0, [r8 + rax - 128];
           subpd xmm0, [r9 + rax - 128];

           movapd [rcx + rax - 128], xmm0;

           movapd xmm1, [r8 + rax - 112];
           subpd xmm1, [r9 + rax - 112];

           movapd [rcx + rax - 112], xmm1;

           movapd xmm2, [r8 + rax - 96];
           subpd xmm2, [r9 + rax - 96];

           movapd [rcx + rax - 96], xmm2;

           movapd xmm3, [r8 + rax - 80];
           subpd xmm3, [r9 + rax - 80];

           movapd [rcx + rax - 80], xmm3;

           movapd xmm0, [r8 + rax - 64];
           subpd xmm0, [r9 + rax - 64];

           movapd [rcx + rax - 64], xmm0;

           movapd xmm1, [r8 + rax - 48];
           subpd xmm1, [r9 + rax - 48];

           movapd [rcx + rax - 48], xmm1;

           movapd xmm2, [r8 + rax - 32];
           subpd xmm2, [r9 + rax - 32];

           movapd [rcx + rax - 32], xmm2;

           movapd xmm3, [r8 + rax - 16];
           subpd xmm3, [r9 + rax - 16];

           movapd [rcx + rax - 16], xmm3;
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           movapd xmm0, [r8 + rax];
           subpd xmm0, [r9 + rax];

           movapd [rcx + rax], xmm0;
       add rax, 16;
       jnz @addforxloop2;

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
end;

procedure ASMMatrixSubUnAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; {$ifdef UNIX}unixWidth{$ELSE}width{$endif} : TASMNativeInt; {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}
var iRBX, iR12 : TASMNativeInt;
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

   // note: RCX = dest, RDX = destLineWidth, R8 = mt1, R9 = mt2
   // prolog - simulate stack
   mov iRBX, rbx;
   mov iR12, r12;

   mov r11, LineWidth1;
   mov r12, LineWidth2;

   //iters := -width*sizeof(double);
   mov r10, width;
   imul r10, -8;

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
           movupd xmm0, [r8 + rax - 128];
           movupd xmm1, [r9 + rax - 128];
           subpd xmm0, xmm1;

           movupd [rcx + rax - 128], xmm0;

           movupd xmm0, [r8 + rax - 112];
           movupd xmm1, [r9 + rax - 112];
           subpd xmm0, xmm1;

           movupd [rcx + rax - 112], xmm0;

           movupd xmm0, [r8 + rax - 96];
           movupd xmm1, [r9 + rax - 96];
           subpd xmm0, xmm1;

           movupd [rcx + rax - 96], xmm0;

           movupd xmm0, [r8 + rax - 80];
           movupd xmm1, [r9 + rax - 80];
           subpd xmm0, xmm1;

           movupd [rcx + rax - 80], xmm0;

           movupd xmm0, [r8 + rax - 64];
           movupd xmm1, [r9 + rax - 64];
           subpd xmm0, xmm1;

           movupd [rcx + rax - 64], xmm0;

           movupd xmm0, [r8 + rax - 48];
           movupd xmm1, [r9 + rax - 48];
           subpd xmm0, xmm1;

           movupd [rcx + rax - 48], xmm0;

           movupd xmm0, [r8 + rax - 32];
           movupd xmm1, [r9 + rax - 32];
           subpd xmm0, xmm1;

           movupd [rcx + rax - 32], xmm0;

           movupd xmm0, [r8 + rax - 16];
           movupd xmm1, [r9 + rax - 16];
           subpd xmm0, xmm1;

           movupd [rcx + rax - 16], xmm0;
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           movupd xmm0, [r8 + rax];
           movupd xmm1, [r9 + rax];
           subpd xmm0, xmm1;

           movupd [rcx + rax], xmm0;
       add rax, 16;
       jnz @addforxloop2;

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
end;

procedure ASMMatrixSubAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; {$ifdef UNIX}unixWidth{$ELSE}width{$endif} : TASMNativeInt; {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}
var iRBX, iR12 : TASMNativeInt;
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

   // note: RCX = dest, RDX = destLineWidth, R8 = mt1, R9 = mt2
   // prolog - simulate stack
   mov iRBX, rbx;
   mov iR12, r12;

   //iters := -(width - 1)*sizeof(double);
   mov r10, width;
   dec r10;
   imul r10, -8;

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
           movapd xmm0, [r8 + rax - 128];
           subpd xmm0, [r9 + rax - 128];

           movapd [rcx + rax - 128], xmm0;

           movapd xmm1, [r8 + rax - 112];
           subpd xmm1, [r9 + rax - 112];

           movapd [rcx + rax - 112], xmm1;

           movapd xmm2, [r8 + rax - 96];
           subpd xmm2, [r9 + rax - 96];

           movapd [rcx + rax - 96], xmm2;

           movapd xmm3, [r8 + rax - 80];
           subpd xmm3, [r9 + rax - 80];

           movapd [rcx + rax - 80], xmm3;

           movapd xmm0, [r8 + rax - 64];
           subpd xmm0, [r9 + rax - 64];

           movapd [rcx + rax - 64], xmm0;

           movapd xmm1, [r8 + rax - 48];
           subpd xmm1, [r9 + rax - 48];

           movapd [rcx + rax - 48], xmm1;

           movapd xmm2, [r8 + rax - 32];
           subpd xmm2, [r9 + rax - 32];

           movapd [rcx + rax - 32], xmm2;

           movapd xmm3, [r8 + rax - 16];
           subpd xmm3, [r9 + rax - 16];

           movapd [rcx + rax - 16], xmm3;
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           movapd xmm0, [r8 + rax];
           subpd xmm0, [r9 + rax];

           movapd [rcx + rax], xmm0;
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last column:
       movsd xmm0, [r8];
       subsd xmm0, [r9];

       movsd [rcx], xmm0;

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
end;

procedure ASMMatrixSubUnAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; {$ifdef UNIX}unixWidth{$ELSE}width{$endif} : TASMNativeInt; {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}
var iRBX, iR12 : TASMNativeInt;
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

   // note: RCX = dest, RDX = destLineWidth, R8 = mt1, R9 = mt2
   // prolog - simulate stack
   mov iRBX, rbx;
   mov iR12, r12;

   mov r11, LineWidth1;
   mov r12, LineWidth2;

   //iters := -(width - 1)*sizeof(double);
   mov r10, width;
   dec r10;
   imul r10, -8;

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
           movupd xmm0, [r8 + rax - 128];
           movupd xmm1, [r9 + rax - 128];
           subpd xmm0, xmm1;

           movupd [rcx + rax - 128], xmm0;

           movupd xmm0, [r8 + rax - 112];
           movupd xmm1, [r9 + rax - 112];
           subpd xmm0, xmm1;

           movupd [rcx + rax - 112], xmm0;

           movupd xmm0, [r8 + rax - 96];
           movupd xmm1, [r9 + rax - 96];
           subpd xmm0, xmm1;

           movupd [rcx + rax - 96], xmm0;

           movupd xmm0, [r8 + rax - 80];
           movupd xmm1, [r9 + rax - 80];
           subpd xmm0, xmm1;

           movupd [rcx + rax - 80], xmm0;

           movupd xmm0, [r8 + rax - 64];
           movupd xmm1, [r9 + rax - 64];
           subpd xmm0, xmm1;

           movupd [rcx + rax - 64], xmm0;

           movupd xmm0, [r8 + rax - 48];
           movupd xmm1, [r9 + rax - 48];
           subpd xmm0, xmm1;

           movupd [rcx + rax - 48], xmm0;

           movupd xmm0, [r8 + rax - 32];
           movupd xmm1, [r9 + rax - 32];
           subpd xmm0, xmm1;

           movupd [rcx + rax - 32], xmm0;

           movupd xmm0, [r8 + rax - 16];
           movupd xmm1, [r9 + rax - 16];
           subpd xmm0, xmm1;

           movupd [rcx + rax - 16], xmm0;
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           movupd xmm0, [r8 + rax];
           movupd xmm1, [r9 + rax];
           subpd xmm0, xmm1;

           movupd [rcx + rax], xmm0;
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last column:
       movsd xmm0, [r8];
       subsd xmm0, [r9];

       movsd [rcx], xmm0;

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
end;

procedure ASMMatrixSubT(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; LineWidthB : TASMNativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}
{$ifdef UNIX}
var width : TASMNativeInt;
    height : TASMNativeInt;
{$ENDIF}
     // rcx : A, rdx : LineWidthA, r8 : B, r9 : LineWidthB;
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

        // rax: iter := -width*sizeof(double)
        mov rcx, A;
        mov rax, width;
        imul rax, -8;
        sub rcx, rax;

        // for y := 0 to height - 1
        @@foryloop:
           mov r10, r8;
           mov r11, rax;

           // for x := 0 to width - 1
           @@forxloop:
              movsd xmm0, [rcx + r11];
              movsd xmm1, [r10];

              subsd xmm0, xmm1;
              movsd [rcx + r11], xmm0;

              add r10, r9;
           add r11, 8;
           jnz @@forxloop;

           add rcx, rdx;
           add r8, 8;
        dec height;
        jnz @@foryloop;
     end;

// ########################################################
// #### Matrix add, sub to vector operations
// ########################################################

procedure ASMMatrixSubVecAlignedVecRow(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; incX : TASMNativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}
{$ifdef UNIX}
var width : TASMNativeInt;
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
   // rcx = a, rdx = LineWidthA, r8 = B, r9 = incX
   mov r10, width;
   imul r10, -8;

   sub rcx, r10;
   sub r8, r10;

   @@foryloop:
      mov rax, r10;

      @@forxloopUnrolled:
         add rax, 64;
         jg @@EndLoop1;

         movapd xmm0, [rcx + rax - 64];
         subpd xmm0, [r8 + rax - 64];
         movapd [rcx + rax - 64], xmm0;

         movapd xmm1, [rcx + rax - 48];
         subpd xmm1, [r8 + rax - 48];
         movapd [rcx + rax - 48], xmm1;

         movapd xmm2, [rcx + rax - 32];
         subpd xmm2, [r8 + rax - 32];
         movapd [rcx + rax - 32], xmm2;

         movapd xmm3, [rcx + rax - 16];
         subpd xmm3, [r8 + rax - 16];
         movapd [rcx + rax - 16], xmm3;

      jmp @@forxloopUnrolled;

      @@EndLoop1:

      sub rax, 64;

      jz @NextLine;

      @@forxloop:
         movsd xmm0, [rcx + rax];
         movsd xmm1, [r8 + rax];

         subsd xmm0, xmm1;
         movsd [rcx + rax], xmm0;

         add rax, 8;
      jnz @@forxloop;

      @NextLine:

      add rcx, rdx;
   dec Height;
   jnz @@foryloop;
end;

procedure ASMMatrixSubVecAlignedRow(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; incX : TASMNativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}
var vecIter : TASMNativeInt;
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
   // rcx = a, rdx = LineWidthA, r8 = B, r9 = incX
   mov r11, width;

   imul r11, r9;
   imul r11, -1;
   mov vecIter, r11;

   mov r10, width;
   imul r10, -8;

   sub rcx, r10;
   sub r8, r11;

   @@foryloop:
      mov rax, r10;
      mov r11, vecIter;

      @@forxloop:
         movsd xmm0, [rcx + rax];
         movsd xmm1, [r8 + r11];

         subsd xmm0, xmm1;
         movsd [rcx + rax], xmm0;

         add r11, r9;
         add rax, 8;
      jnz @@forxloop;

      add rcx, rdx;
   dec Height;
   jnz @@foryloop;
end;

procedure ASMMatrixSubVecAlignedCol(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; incX : TASMNativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}
{$ifdef UNIX}
var width : TASMNativeInt;
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
   // rcx = a, rdx = LineWidthA, r8 = B, r9 = incX
   mov r10, width;
   imul r10, -8;

   sub rcx, r10;

   @@foryloop:
      mov rax, r10;

      movddup xmm1, [r8];

      @@forxloopUnrolled:
         add rax, 64;
         jg @@EndLoop1;

         movapd xmm0, [rcx + rax - 64];
         subpd xmm0, xmm1;
         movapd [rcx + rax - 64], xmm0;

         movapd xmm3, [rcx + rax - 48];
         subpd xmm3, xmm1;
         movapd [rcx + rax - 48], xmm3;

         movapd xmm2, [rcx + rax - 32];
         subpd xmm2, xmm1;
         movapd [rcx + rax - 32], xmm2;

         movapd xmm3, [rcx + rax - 16];
         subpd xmm3, xmm1;
         movapd [rcx + rax - 16], xmm3;

      jmp @@forxloopUnrolled;

      @@EndLoop1:

      sub rax, 64;

      jz @NextLine;

      @@forxloop:
         movsd xmm0, [rcx + rax];

         subsd xmm0, xmm1;
         movsd [rcx + rax], xmm0;

         add rax, 8;
      jnz @@forxloop;

      @NextLine:

      add rcx, rdx;
      add r8, r9;
   dec Height;
   jnz @@foryloop;
end;

procedure ASMMatrixSubVecUnalignedVecRow(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; incX : TASMNativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}
{$ifdef UNIX}
var width : TASMNativeInt;
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

   // rcx = a, rdx = LineWidthA, r8 = B, r9 = incX
   mov r10, width;
   imul r10, -8;

   sub rcx, r10;
   sub r8, r10;

   @@foryloop:
      mov rax, r10;

      @@forxloopUnrolled:
         add rax, 64;
         jg @@EndLoop1;

         movupd xmm0, [rcx + rax - 64];
         movupd xmm1, [r8 + rax - 64];
         subpd xmm0, xmm1;
         movupd [rcx + rax - 64], xmm0;

         movupd xmm1, [rcx + rax - 48];
         movupd xmm2, [r8 + rax - 48];
         subpd xmm1, xmm2;
         movupd [rcx + rax - 48], xmm1;

         movupd xmm2, [rcx + rax - 32];
         movupd xmm3, [r8 + rax - 32];
         subpd xmm2, xmm3;
         movupd [rcx + rax - 32], xmm2;

         movupd xmm3, [rcx + rax - 16];
         movupd xmm0, [r8 + rax - 16];
         subpd xmm3, xmm0;
         movupd [rcx + rax - 16], xmm3;

      jmp @@forxloopUnrolled;

      @@EndLoop1:

      sub rax, 64;

      jz @NextLine;

      @@forxloop:
         movsd xmm0, [rcx + rax];
         movsd xmm1, [r8 + rax];

         subsd xmm0, xmm1;
         movsd [rcx + rax], xmm0;

         add rax, 8;
      jnz @@forxloop;

      @NextLine:

      add rcx, rdx;
   dec Height;
   jnz @@foryloop;
end;

procedure ASMMatrixSubVecUnalignedRow(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; incX : TASMNativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}
var vecIter : TASMNativeInt;
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
   // rcx = a, rdx = LineWidthA, r8 = B, r9 = incX
   mov r11, width;

   imul r11, r9;
   imul r11, -1;
   mov vecIter, r11;

   mov r10, width;
   imul r10, -8;

   sub rcx, r10;
   sub r8, r11;

   @@foryloop:
      mov rax, r10;
      mov r11, vecIter;

      @@forxloop:
         movsd xmm0, [rcx + rax];
         movsd xmm1, [r8 + r11];

         subsd xmm0, xmm1;
         movsd [rcx + rax], xmm0;

         add r11, r9;
         add rax, 8;
      jnz @@forxloop;

      add rcx, rdx;
   dec Height;
   jnz @@foryloop;
end;

procedure ASMMatrixSubVecUnalignedCol(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; incX : TASMNativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}
{$ifdef UNIX}
var width : TASMNativeInt;
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

   // rcx = a, rdx = LineWidthA, r8 = B, r9 = incX
   mov r10, width;
   imul r10, -8;

   sub rcx, r10;

   @@foryloop:
      mov rax, r10;

      movddup xmm1, [r8];

      @@forxloopUnrolled:
         add rax, 64;
         jg @@EndLoop1;

         movupd xmm0, [rcx + rax - 64];
         subpd xmm0, xmm1;
         movupd [rcx + rax - 64], xmm0;

         movupd xmm3, [rcx + rax - 48];
         subpd xmm3, xmm1;
         movupd [rcx + rax - 48], xmm3;

         movupd xmm2, [rcx + rax - 32];
         subpd xmm2, xmm1;
         movupd [rcx + rax - 32], xmm2;

         movupd xmm3, [rcx + rax - 16];
         subpd xmm3, xmm1;
         movupd [rcx + rax - 16], xmm3;

      jmp @@forxloopUnrolled;

      @@EndLoop1:

      sub rax, 64;

      jz @NextLine;

      @@forxloop:
         movsd xmm0, [rcx + rax];

         subsd xmm0, xmm1;
         movsd [rcx + rax], xmm0;

         add rax, 8;
      jnz @@forxloop;

      @NextLine:

      add rcx, rdx;
      add r8, r9;
   dec Height;
   jnz @@foryloop;
end;


procedure ASMMatrixAddVecAlignedVecRow(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; incX : TASMNativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}
{$ifdef UNIX}
var width : TASMNativeInt;
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

   // rcx = a, rdx = LineWidthA, r8 = B, r9 = incX
   mov r10, width;
   imul r10, -8;

   sub rcx, r10;
   sub r8, r10;

   @@foryloop:
      mov rax, r10;

      @@forxloopUnrolled:
         add rax, 64;
         jg @@EndLoop1;

         movapd xmm0, [rcx + rax - 64];
         addpd xmm0, [r8 + rax - 64];
         movapd [rcx + rax - 64], xmm0;

         movapd xmm1, [rcx + rax - 48];
         addpd xmm1, [r8 + rax - 48];
         movapd [rcx + rax - 48], xmm1;

         movapd xmm2, [rcx + rax - 32];
         addpd xmm2, [r8 + rax - 32];
         movapd [rcx + rax - 32], xmm2;

         movapd xmm3, [rcx + rax - 16];
         addpd xmm3, [r8 + rax - 16];
         movapd [rcx + rax - 16], xmm3;

      jmp @@forxloopUnrolled;

      @@EndLoop1:

      sub rax, 64;

      jz @NextLine;

      @@forxloop:
         movsd xmm0, [rcx + rax];
         movsd xmm1, [r8 + rax];

         addsd xmm0, xmm1;
         movsd [rcx + rax], xmm0;

         add rax, 8;
      jnz @@forxloop;

      @NextLine:

      add rcx, rdx;
   dec Height;
   jnz @@foryloop;
end;

procedure ASMMatrixAddVecAlignedRow(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; incX : TASMNativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}
var vecIter : TASMNativeInt;
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

   // rcx = a, rdx = LineWidthA, r8 = B, r9 = incX
   mov r11, width;

   imul r11, r9;
   imul r11, -1;
   mov vecIter, r11;

   mov r10, width;
   imul r10, -8;

   sub rcx, r10;
   sub r8, r11;

   @@foryloop:
      mov rax, r10;
      mov r11, vecIter;

      @@forxloop:
         movsd xmm0, [rcx + rax];
         movsd xmm1, [r8 + r11];

         addsd xmm0, xmm1;
         movsd [rcx + rax], xmm0;

         add r11, r9;
         add rax, 8;
      jnz @@forxloop;

      add rcx, rdx;
   dec Height;
   jnz @@foryloop;
end;

procedure ASMMatrixAddVecAlignedCol(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; incX : TASMNativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}
{$ifdef UNIX}
var width : TASMNativeInt;
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

   // rcx = a, rdx = LineWidthA, r8 = B, r9 = incX
   mov r10, width;
   imul r10, -8;

   sub rcx, r10;

   @@foryloop:
      mov rax, r10;

      movddup xmm1, [r8];

      @@forxloopUnrolled:
         add rax, 64;
         jg @@EndLoop1;

         movapd xmm0, [rcx + rax - 64];
         addpd xmm0, xmm1;
         movapd [rcx + rax - 64], xmm0;

         movapd xmm3, [rcx + rax - 48];
         addpd xmm3, xmm1;
         movapd [rcx + rax - 48], xmm3;

         movapd xmm2, [rcx + rax - 32];
         addpd xmm2, xmm1;
         movapd [rcx + rax - 32], xmm2;

         movapd xmm3, [rcx + rax - 16];
         addpd xmm3, xmm1;
         movapd [rcx + rax - 16], xmm3;

      jmp @@forxloopUnrolled;

      @@EndLoop1:

      sub rax, 64;

      jz @NextLine;

      @@forxloop:
         movsd xmm0, [rcx + rax];

         addsd xmm0, xmm1;
         movsd [rcx + rax], xmm0;

         add rax, 8;
      jnz @@forxloop;

      @NextLine:

      add rcx, rdx;
      add r8, r9;
   dec Height;
   jnz @@foryloop;
end;

procedure ASMMatrixAddVecUnalignedVecRow(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; incX : TASMNativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}
{$ifdef UNIX}
var width : TASMNativeInt;
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

   // rcx = a, rdx = LineWidthA, r8 = B, r9 = incX
   mov r10, width;
   imul r10, -8;

   sub rcx, r10;
   sub r8, r10;

   @@foryloop:
      mov rax, r10;

      @@forxloopUnrolled:
         add rax, 64;
         jg @@EndLoop1;

         movupd xmm0, [rcx + rax - 64];
         movupd xmm1, [r8 + rax - 64];
         addpd xmm0, xmm1;
         movupd [rcx + rax - 64], xmm0;

         movupd xmm1, [rcx + rax - 48];
         movupd xmm2, [r8 + rax - 48];
         addpd xmm1, xmm2;
         movupd [rcx + rax - 48], xmm1;

         movupd xmm2, [rcx + rax - 32];
         movupd xmm3, [r8 + rax - 32];
         addpd xmm2, xmm3;
         movupd [rcx + rax - 32], xmm2;

         movupd xmm3, [rcx + rax - 16];
         movupd xmm0, [r8 + rax - 16];
         addpd xmm3, xmm0;
         movupd [rcx + rax - 16], xmm3;

      jmp @@forxloopUnrolled;

      @@EndLoop1:

      sub rax, 64;

      jz @NextLine;

      @@forxloop:
         movsd xmm0, [rcx + rax];
         movsd xmm1, [r8 + rax];

         addsd xmm0, xmm1;
         movsd [rcx + rax], xmm0;

         add rax, 8;
      jnz @@forxloop;

      @NextLine:

      add rcx, rdx;
   dec Height;
   jnz @@foryloop;
end;

procedure ASMMatrixAddVecUnalignedRow(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; incX : TASMNativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}
var vecIter : TASMNativeInt;
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

   // rcx = a, rdx = LineWidthA, r8 = B, r9 = incX
   mov r11, width;

   imul r11, r9;
   imul r11, -1;
   mov vecIter, r11;

   mov r10, width;
   imul r10, -8;

   sub rcx, r10;
   sub r8, r11;

   @@foryloop:
      mov rax, r10;
      mov r11, vecIter;

      @@forxloop:
         movsd xmm0, [rcx + rax];
         movsd xmm1, [r8 + r11];

         addsd xmm0, xmm1;
         movsd [rcx + rax], xmm0;

         add r11, r9;
         add rax, 8;
      jnz @@forxloop;

      add rcx, rdx;
   dec Height;
   jnz @@foryloop;
end;

procedure ASMMatrixAddVecUnalignedCol(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; incX : TASMNativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}
{$ifdef UNIX}
var width : TASMNativeInt;
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

   // rcx = a, rdx = LineWidthA, r8 = B, r9 = incX
   mov r10, width;
   imul r10, -8;

   sub rcx, r10;

   @@foryloop:
      mov rax, r10;

      movddup xmm1, [r8];

      @@forxloopUnrolled:
         add rax, 64;
         jg @@EndLoop1;

         movupd xmm0, [rcx + rax - 64];
         addpd xmm0, xmm1;
         movupd [rcx + rax - 64], xmm0;

         movupd xmm3, [rcx + rax - 48];
         addpd xmm3, xmm1;
         movupd [rcx + rax - 48], xmm3;

         movupd xmm2, [rcx + rax - 32];
         addpd xmm2, xmm1;
         movupd [rcx + rax - 32], xmm2;

         movupd xmm3, [rcx + rax - 16];
         addpd xmm3, xmm1;
         movupd [rcx + rax - 16], xmm3;

      jmp @@forxloopUnrolled;

      @@EndLoop1:

      sub rax, 64;

      jz @NextLine;

      @@forxloop:
         movsd xmm0, [rcx + rax];

         addsd xmm0, xmm1;
         movsd [rcx + rax], xmm0;

         add rax, 8;
      jnz @@forxloop;

      @NextLine:

      add rcx, rdx;
      add r8, r9;
   dec Height;
   jnz @@foryloop;
end;

// perform y[i] = y[i] + alpha * x[i]
// unrolled 4 times
procedure ASMVecAddUnAlignedSeq( X : PDouble; y : PDouble; N : TASMNativeInt; alpha : double );
// rcx = X, rdx = Y, r8 = N, xmm3 = alpha
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

   movddup xmm0, alpha;

   imul r8, -8;
   sub rcx, r8;
   sub rdx, r8;

   @@forNLoop:
     add r8, 128;
     jg @loopEnd;

     movupd xmm1, [rcx + r8 - 128];
     movupd xmm2, [rdx + r8 - 128];
     mulpd xmm1, xmm0;
     addpd xmm1, xmm2;
     movupd [rdx + r8 - 128], xmm1;

     movupd xmm1, [rcx + r8 - 112];
     movupd xmm2, [rdx + r8 - 112];
     mulpd xmm1, xmm0;
     addpd xmm1, xmm2;
     movupd [rdx + r8 - 112], xmm1;

     movupd xmm3, [rcx + r8 - 96];
     movupd xmm4, [rdx + r8 - 96];
     mulpd xmm3, xmm0;
     addpd xmm3, xmm4;
     movupd [rdx + r8 - 96], xmm3;

     movupd xmm2, [rcx + r8 - 80];
     movupd xmm1, [rdx + r8 - 80];
     mulpd xmm2, xmm0;
     addpd xmm1, xmm2;
     movupd [rdx + r8 - 80], xmm1;

     movupd xmm4, [rcx + r8 - 64];
     movupd xmm3, [rdx + r8 - 64];
     mulpd xmm4, xmm0;
     addpd xmm3, xmm4;
     movupd [rdx + r8 - 64], xmm3;

     movupd xmm2, [rcx + r8 - 48];
     movupd xmm1, [rdx + r8 - 48];
     mulpd xmm2, xmm0;
     addpd xmm1, xmm2;
     movupd [rdx + r8 - 48], xmm1;

     movupd xmm4, [rcx + r8 - 32];
     movupd xmm3, [rdx + r8 - 32];
     mulpd xmm4, xmm0;
     addpd xmm3, xmm4;
     movupd [rdx + r8 - 32], xmm3;

     movupd xmm4, [rcx + r8 - 16];
     movupd xmm3, [rdx + r8 - 16];
     mulpd xmm4, xmm0;
     addpd xmm3, xmm4;
     movupd [rdx + r8 - 16], xmm3;

   jmp @@forNLoop

   @loopEnd:

   sub r8, 128;
   jz @endLine;

   @forNLoop2:
      add r8, 16;
      jg @endLine2;

      movupd xmm4, [rcx + r8 - 16];
      movupd xmm3, [rdx + r8 - 16];
      mulpd xmm4, xmm0;
      addpd xmm3, xmm4;
      movupd [rdx + r8 - 16], xmm3;
   jmp @forNLoop2;

   @endLine2:

   // last odd element
   sub r8, 8;
   jnz @endLine;

   movsd xmm4, [rcx - 8];
   movsd xmm3, [rdx - 8];
   mulsd xmm4, xmm0;
   addsd xmm3, xmm4;
   movsd [rdx - 8], xmm3;

   @endLine:
end;

procedure ASMVecAddAlignedSeq( X : PDouble; y : PDouble; N : TASMNativeInt; alpha : double );
// rcx = X, rdx = Y, r8 = N, xmm3 = alpha
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

   movddup xmm0, alpha;

   imul r8, -8;
   sub rcx, r8;
   sub rdx, r8;

   @@forNLoop:
     add r8, 128;
     jg @loopEnd;

     movapd xmm1, [rcx + r8 - 128];
     movapd xmm2, [rdx + r8 - 128];
     mulpd xmm1, xmm0;
     addpd xmm1, xmm2;
     movapd [rdx + r8 - 128], xmm1;

     movupd xmm1, [rcx + r8 - 112];
     movupd xmm2, [rdx + r8 - 112];
     mulpd xmm1, xmm0;
     addpd xmm1, xmm2;
     movupd [rdx + r8 - 112], xmm1;

     movapd xmm3, [rcx + r8 - 96];
     movapd xmm4, [rdx + r8 - 96];
     mulpd xmm3, xmm0;
     addpd xmm3, xmm4;
     movapd [rdx + r8 - 96], xmm3;

     movapd xmm2, [rcx + r8 - 80];
     movapd xmm1, [rdx + r8 - 80];
     mulpd xmm2, xmm0;
     addpd xmm1, xmm2;
     movapd [rdx + r8 - 80], xmm1;

     movapd xmm4, [rcx + r8 - 64];
     movapd xmm3, [rdx + r8 - 64];
     mulpd xmm4, xmm0;
     addpd xmm3, xmm4;
     movapd [rdx + r8 - 64], xmm3;

     movapd xmm2, [rcx + r8 - 48];
     movapd xmm1, [rdx + r8 - 48];
     mulpd xmm2, xmm0;
     addpd xmm1, xmm2;
     movapd [rdx + r8 - 48], xmm1;

     movapd xmm4, [rcx + r8 - 32];
     movapd xmm3, [rdx + r8 - 32];
     mulpd xmm4, xmm0;
     addpd xmm3, xmm4;
     movapd [rdx + r8 - 32], xmm3;

     movapd xmm4, [rcx + r8 - 16];
     movapd xmm3, [rdx + r8 - 16];
     mulpd xmm4, xmm0;
     addpd xmm3, xmm4;
     movapd [rdx + r8 - 16], xmm3;

   jmp @@forNLoop

   @loopEnd:

   sub r8, 128;
   jz @endLine;

   @forNLoop2:
      add r8, 16;
      jg @endLine2;

      movapd xmm4, [rcx + r8 - 16];
      movapd xmm3, [rdx + r8 - 16];
      mulpd xmm4, xmm0;
      addpd xmm3, xmm4;
      movapd [rdx + r8 - 16], xmm3;
   jmp @forNLoop2;

   @endLine2:

   // last odd element
   sub r8, 8;
   jnz @endLine;

   movsd xmm4, [rcx - 8];
   movsd xmm3, [rdx - 8];
   mulsd xmm4, xmm0;
   addsd xmm3, xmm4;
   movsd [rdx - 8], xmm3;

   @endLine:
end;

procedure ASMVecAddNonSeq( X : PDouble; y : PDouble; N : TASMNativeInt; incX, {$ifdef UNIX}unixIncY{$ELSE}incY {$ENDIF} : TASMNativeInt; alpha : double );
// rcx = X, rdx = Y, r8 = N, incX = r9
{$IFDEF UNIX}
var incY : TASMNativeInt;
{$ENDIF}
asm
   {$IFDEF UNIX}
   // Linux uses a diffrent ABI -> copy over the registers so they meet with winABI
   // (note that the 5th and 6th parameter are are on the stack)
   // The parameters are passed in the following order:
   // RDI, RSI, RDX, RCX -> mov to RCX, RDX, R8, R9
   mov incY, unixIncY;
   mov r8, rdx;
   mov r9, rcx;
   mov rcx, rdi;
   mov rdx, rsi;
   {$ENDIF}

   mov rax, incY;

   {$IFNDEF UNIX}
   movsd xmm0, alpha;
   {$ENDIF}

   @@loopN:
      movsd xmm1, [rcx];
      movsd xmm2, [rdx];
      mulsd xmm1, xmm0;
      addsd xmm2, xmm1;

      movsd [rdx], xmm2;
      add rdx, rax;
      add rcx, r9;
   dec r8;
   jnz @@loopN;
end;

{$ENDIF}

end.
