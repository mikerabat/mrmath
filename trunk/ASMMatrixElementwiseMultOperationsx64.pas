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


unit ASMMatrixElementwiseMultOperationsx64;

interface

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF x64}

uses MatrixConst;

procedure ASMMatrixElemMultAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
procedure ASMMatrixElemMultUnAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);

procedure ASMMatrixElemMultAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
procedure ASMMatrixElemMultUnAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);

procedure ASMMatrixElemDivAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
procedure ASMMatrixElemDivUnAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);

procedure ASMMatrixElemDivAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
procedure ASMMatrixElemDivUnAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);


{$ENDIF}

implementation

{$IFDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$ENDIF}

procedure ASMMatrixElemMultAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var iRBX, iR11, iR12 : NativeInt;
{$IFDEF FPC}
begin
{$ENDIF}
asm
   // note: RCX = dest, RDX = destLineWidth, R8 = mt1, R9 = mt2
   // prolog - simulate stack
   mov iRBX, rbx;
   mov iR11, r11;
   mov iR12, r12;
   {
   .pushnv rbx;
   .pushnv r11;
   .pushnv r12;
   }

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

           // mult:
           movapd xmm0, [r8 + rax - 128];
           mulpd xmm0, [r9 + rax - 128];

           movapd [rcx + rax - 128], xmm0;

           movapd xmm1, [r8 + rax - 112];
           mulpd xmm1, [r9 + rax - 112];

           movapd [rcx + rax - 112], xmm1;

           movapd xmm2, [r8 + rax - 96];
           mulpd xmm2, [r9 + rax - 96];

           movapd [rcx + rax - 96], xmm2;

           movapd xmm3, [r8 + rax - 80];
           mulpd xmm3, [r9 + rax - 80];

           movapd [rcx + rax - 80], xmm3;

           movapd xmm0, [r8 + rax - 64];
           mulpd xmm0, [r9 + rax - 64];

           movapd [rcx + rax - 64], xmm0;

           movapd xmm1, [r8 + rax - 48];
           mulpd xmm1, [r9 + rax - 48];

           movapd [rcx + rax - 48], xmm1;

           movapd xmm2, [r8 + rax - 32];
           mulpd xmm2, [r9 + rax - 32];

           movapd [rcx + rax - 32], xmm2;

           movapd xmm3, [r8 + rax - 16];
           mulpd xmm3, [r9 + rax - 16];

           movapd [rcx + rax - 16], xmm3;
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           movapd xmm0, [r8 + rax];
           mulpd xmm0, [r9 + rax];

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
   mov r11, iR11;
   mov r12, iR12;
{$IFDEF FPC}
end;
{$ENDIF}
end;

procedure ASMMatrixElemMultUnAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var iRBX, iR11, iR12 : NativeInt;
{$IFDEF FPC}
begin
  {$ENDIF}
asm
   // note: RCX = dest, RDX = destLineWidth, R8 = mt1, R9 = mt2
   // prolog - simulate stack
   mov iRBX, rbx;
   mov iR11, r11;
   mov iR12, r12;
   {
   .pushnv rbx;
   .pushnv r11;
   .pushnv r12;
   }

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

           // mult:
           movupd xmm0, [r8 + rax - 128];
           movupd xmm1, [r9 + rax - 128];
           mulpd xmm0, xmm1;

           movupd [rcx + rax - 128], xmm0;

           movupd xmm0, [r8 + rax - 112];
           movupd xmm1, [r9 + rax - 112];
           mulpd xmm0, xmm1;

           movupd [rcx + rax - 112], xmm0;

           movupd xmm0, [r8 + rax - 96];
           movupd xmm1, [r9 + rax - 96];
           mulpd xmm0, xmm1;

           movupd [rcx + rax - 96], xmm0;

           movupd xmm0, [r8 + rax - 80];
           movupd xmm1, [r9 + rax - 80];
           mulpd xmm0, xmm1;

           movupd [rcx + rax - 80], xmm0;

           movupd xmm0, [r8 + rax - 64];
           movupd xmm1, [r9 + rax - 64];
           mulpd xmm0, xmm1;

           movupd [rcx + rax - 64], xmm0;

           movupd xmm0, [r8 + rax - 48];
           movupd xmm1, [r9 + rax - 48];
           mulpd xmm0, xmm1;

           movupd [rcx + rax - 48], xmm0;

           movupd xmm0, [r8 + rax - 32];
           movupd xmm1, [r9 + rax - 32];
           mulpd xmm0, xmm1;

           movupd [rcx + rax - 32], xmm0;

           movupd xmm0, [r8 + rax - 16];
           movupd xmm1, [r9 + rax - 16];
           mulpd xmm0, xmm1;

           movupd [rcx + rax - 16], xmm0;
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           movupd xmm0, [r8 + rax];
           movupd xmm1, [r9 + rax];
           mulpd xmm0, xmm1;

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
   mov r11, iR11;
   mov r12, iR12;
{$IFDEF FPC}
end;
{$ENDIF}
end;

procedure ASMMatrixElemMultAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var iRBX, iR11, iR12 : NativeInt;
{$IFDEF FPC}
begin
  {$ENDIF}
asm
   // note: RCX = dest, RDX = destLineWidth, R8 = mt1, R9 = mt2
   // prolog - simulate stack
   mov iRBX, rbx;
   mov iR11, r11;
   mov iR12, r12;
   {
   .pushnv rbx;
   .pushnv r11;
   .pushnv r12;
   }

   //iters := -(width - 1)*sizeof(double);
   mov r10, width;
   dec r10;
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

           // mult:
           movapd xmm0, [r8 + rax - 128];
           mulpd xmm0, [r9 + rax - 128];

           movapd [rcx + rax - 128], xmm0;

           movapd xmm1, [r8 + rax - 112];
           mulpd xmm1, [r9 + rax - 112];

           movapd [rcx + rax - 112], xmm1;

           movapd xmm2, [r8 + rax - 96];
           mulpd xmm2, [r9 + rax - 96];

           movapd [rcx + rax - 96], xmm2;

           movapd xmm3, [r8 + rax - 80];
           mulpd xmm3, [r9 + rax - 80];

           movapd [rcx + rax - 80], xmm3;

           movapd xmm0, [r8 + rax - 64];
           mulpd xmm0, [r9 + rax - 64];

           movapd [rcx + rax - 64], xmm0;

           movapd xmm1, [r8 + rax - 48];
           mulpd xmm1, [r9 + rax - 48];

           movapd [rcx + rax - 48], xmm1;

           movapd xmm2, [r8 + rax - 32];
           mulpd xmm2, [r9 + rax - 32];

           movapd [rcx + rax - 32], xmm2;

           movapd xmm3, [r8 + rax - 16];
           mulpd xmm3, [r9 + rax - 16];

           movapd [rcx + rax - 16], xmm3;
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           movapd xmm0, [r8 + rax];
           mulpd xmm0, [r9 + rax];

           movapd [rcx + rax], xmm0;
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last column:
       movlpd xmm0, [r8];
       movlpd xmm1, [r9];
       mulsd xmm0, xmm1;

       movlpd [rcx], xmm0;

       // next line:
       add r8, r11;
       add r9, r12;
       add rcx, rdx;

   // loop y end
   dec rbx;
   jnz @@addforyloop;

   // epilog
   mov rbx, iRBX;
   mov r11, iR11;
   mov r12, iR12;
{$IFDEF FPC}
end;
{$ENDIF}
end;

procedure ASMMatrixElemMultUnAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var iRBX, iR11, iR12 : NativeInt;
{$IFDEF FPC}
begin
  {$ENDIF}
asm
   // note: RCX = dest, RDX = destLineWidth, R8 = mt1, R9 = mt2
   // prolog - simulate stack
   mov iRBX, rbx;
   mov iR11, r11;
   mov iR12, r12;
   {
   .pushnv rbx;
   .pushnv r11;
   .pushnv r12;
   }

   mov r11, LineWidth1;
   mov r12, LineWidth2;

   //iters := -(width - 1)*sizeof(double);
   mov r10, width;
   dec r10;
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

           // addition:
           movupd xmm0, [r8 + rax - 128];
           movupd xmm1, [r9 + rax - 128];
           mulpd xmm0, xmm1;

           movupd [rcx + rax - 128], xmm0;

           movupd xmm0, [r8 + rax - 112];
           movupd xmm1, [r9 + rax - 112];
           mulpd xmm0, xmm1;

           movupd [rcx + rax - 112], xmm0;

           movupd xmm0, [r8 + rax - 96];
           movupd xmm1, [r9 + rax - 96];
           mulpd xmm0, xmm1;

           movupd [rcx + rax - 96], xmm0;

           movupd xmm0, [r8 + rax - 80];
           movupd xmm1, [r9 + rax - 80];
           mulpd xmm0, xmm1;

           movupd [rcx + rax - 80], xmm0;

           movupd xmm0, [r8 + rax - 64];
           movupd xmm1, [r9 + rax - 64];
           mulpd xmm0, xmm1;

           movupd [rcx + rax - 64], xmm0;

           movupd xmm0, [r8 + rax - 48];
           movupd xmm1, [r9 + rax - 48];
           mulpd xmm0, xmm1;

           movupd [rcx + rax - 48], xmm0;

           movupd xmm0, [r8 + rax - 32];
           movupd xmm1, [r9 + rax - 32];
           mulpd xmm0, xmm1;

           movupd [rcx + rax - 32], xmm0;

           movupd xmm0, [r8 + rax - 16];
           movupd xmm1, [r9 + rax - 16];
           mulpd xmm0, xmm1;

           movupd [rcx + rax - 16], xmm0;
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           movupd xmm0, [r8 + rax];
           movupd xmm1, [r9 + rax];
           mulpd xmm0, xmm1;

           movupd [rcx + rax], xmm0;
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last column:
       movlpd xmm0, [r8];
       movlpd xmm1, [r9];
       mulsd xmm0, xmm1;

       movlpd [rcx], xmm0;

       // next line:
       add r8, r11;
       add r9, r12;
       add rcx, rdx;

   // loop y end
   dec rbx;
   jnz @@addforyloop;

   // epilog
   mov rbx, iRBX;
   mov r11, iR11;
   mov r12, iR12;
{$IFDEF FPC}
end;
{$ENDIF}
end;

// ##############################################################
// #### Elementwise divide of matrix 1 to matrix 2
// ##############################################################

procedure ASMMatrixElemDivAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var iRBX, iR11, iR12 : NativeInt;
{$IFDEF FPC}
begin
  {$ENDIF}
asm
   // note: RCX = dest, RDX = destLineWidth, R8 = mt1, R9 = mt2
   // prolog - simulate stack
   mov iRBX, rbx;
   mov iR11, r11;
   mov iR12, r12;
   {
   .pushnv rbx;
   .pushnv r11;
   .pushnv r12;
   }

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

           // Div:
           movapd xmm0, [r8 + rax - 128];
           divpd xmm0, [r9 + rax - 128];

           movapd [rcx + rax - 128], xmm0;

           movapd xmm1, [r8 + rax - 112];
           divpd xmm1, [r9 + rax - 112];

           movapd [rcx + rax - 112], xmm1;

           movapd xmm2, [r8 + rax - 96];
           divpd xmm2, [r9 + rax - 96];

           movapd [rcx + rax - 96], xmm2;

           movapd xmm3, [r8 + rax - 80];
           divpd xmm3, [r9 + rax - 80];

           movapd [rcx + rax - 80], xmm3;

           movapd xmm0, [r8 + rax - 64];
           divpd xmm0, [r9 + rax - 64];

           movapd [rcx + rax - 64], xmm0;

           movapd xmm1, [r8 + rax - 48];
           divpd xmm1, [r9 + rax - 48];

           movapd [rcx + rax - 48], xmm1;

           movapd xmm2, [r8 + rax - 32];
           divpd xmm2, [r9 + rax - 32];

           movapd [rcx + rax - 32], xmm2;

           movapd xmm3, [r8 + rax - 16];
           divpd xmm3, [r9 + rax - 16];

           movapd [rcx + rax - 16], xmm3;
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           movapd xmm0, [r8 + rax];
           divpd xmm0, [r9 + rax];

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
   mov r11, iR11;
   mov r12, iR12;
{$IFDEF FPC}
end;
{$ENDIF}
end;

procedure ASMMatrixElemDivUnAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var iRBX, iR11, iR12 : NativeInt;
{$IFDEF FPC}
begin
  {$ENDIF}
asm
   // note: RCX = dest, RDX = destLineWidth, R8 = mt1, R9 = mt2
   // prolog - simulate stack
   mov iRBX, rbx;
   mov iR11, r11;
   mov iR12, r12;
   {
   .pushnv rbx;
   .pushnv r11;
   .pushnv r12;
   }

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

           // Div:
           movupd xmm0, [r8 + rax - 128];
           movupd xmm1, [r9 + rax - 128];
           divpd xmm0, xmm1;

           movupd [rcx + rax - 128], xmm0;

           movupd xmm0, [r8 + rax - 112];
           movupd xmm1, [r9 + rax - 112];
           divpd xmm0, xmm1;

           movupd [rcx + rax - 112], xmm0;

           movupd xmm0, [r8 + rax - 96];
           movupd xmm1, [r9 + rax - 96];
           divpd xmm0, xmm1;

           movupd [rcx + rax - 96], xmm0;

           movupd xmm0, [r8 + rax - 80];
           movupd xmm1, [r9 + rax - 80];
           divpd xmm0, xmm1;

           movupd [rcx + rax - 80], xmm0;

           movupd xmm0, [r8 + rax - 64];
           movupd xmm1, [r9 + rax - 64];
           divpd xmm0, xmm1;

           movupd [rcx + rax - 64], xmm0;

           movupd xmm0, [r8 + rax - 48];
           movupd xmm1, [r9 + rax - 48];
           divpd xmm0, xmm1;

           movupd [rcx + rax - 48], xmm0;

           movupd xmm0, [r8 + rax - 32];
           movupd xmm1, [r9 + rax - 32];
           divpd xmm0, xmm1;

           movupd [rcx + rax - 32], xmm0;

           movupd xmm0, [r8 + rax - 16];
           movupd xmm1, [r9 + rax - 16];
           divpd xmm0, xmm1;

           movupd [rcx + rax - 16], xmm0;
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           movupd xmm0, [r8 + rax];
           movupd xmm1, [r9 + rax];
           divpd xmm0, xmm1;

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
   mov r11, iR11;
   mov r12, iR12;
{$IFDEF FPC}
end;
{$ENDIF}
end;

procedure ASMMatrixElemDivAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var iRBX, iR11, iR12 : NativeInt;
{$IFDEF FPC}
begin
  {$ENDIF}
asm
   // note: RCX = dest, RDX = destLineWidth, R8 = mt1, R9 = mt2
   // prolog - simulate stack
   mov iRBX, rbx;
   mov iR11, r11;
   mov iR12, r12;
   {
   .pushnv rbx;
   .pushnv r11;
   .pushnv r12;
   }

   //iters := -(width - 1)*sizeof(double);
   mov r10, width;
   dec r10;
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

           // Div:
           movapd xmm0, [r8 + rax - 128];
           divpd xmm0, [r9 + rax - 128];

           movapd [rcx + rax - 128], xmm0;

           movapd xmm1, [r8 + rax - 112];
           divpd xmm1, [r9 + rax - 112];

           movapd [rcx + rax - 112], xmm1;

           movapd xmm2, [r8 + rax - 96];
           divpd xmm2, [r9 + rax - 96];

           movapd [rcx + rax - 96], xmm2;

           movapd xmm3, [r8 + rax - 80];
           divpd xmm3, [r9 + rax - 80];

           movapd [rcx + rax - 80], xmm3;

           movapd xmm0, [r8 + rax - 64];
           divpd xmm0, [r9 + rax - 64];

           movapd [rcx + rax - 64], xmm0;

           movapd xmm1, [r8 + rax - 48];
           divpd xmm1, [r9 + rax - 48];

           movapd [rcx + rax - 48], xmm1;

           movapd xmm2, [r8 + rax - 32];
           divpd xmm2, [r9 + rax - 32];

           movapd [rcx + rax - 32], xmm2;

           movapd xmm3, [r8 + rax - 16];
           divpd xmm3, [r9 + rax - 16];

           movapd [rcx + rax - 16], xmm3;
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           movapd xmm0, [r8 + rax];
           divpd xmm0, [r9 + rax];

           movapd [rcx + rax], xmm0;
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last column:
       movlpd xmm0, [r8];
       movlpd xmm1, [r9];
       divsd xmm0, xmm1;

       movlpd [rcx], xmm0;

       // next line:
       add r8, r11;
       add r9, r12;
       add rcx, rdx;

   // loop y end
   dec rbx;
   jnz @@addforyloop;

   // epilog
   mov rbx, iRBX;
   mov r11, iR11;
   mov r12, iR12;
{$IFDEF FPC}
end;
{$ENDIF}
end;

procedure ASMMatrixElemDivUnAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var iRBX, iR11, iR12 : NativeInt;
{$IFDEF FPC}
begin
  {$ENDIF}
asm
   // note: RCX = dest, RDX = destLineWidth, R8 = mt1, R9 = mt2
   // prolog - simulate stack
   mov iRBX, rbx;
   mov iR11, r11;
   mov iR12, r12;
   {
   .pushnv rbx;
   .pushnv r11;
   .pushnv r12;
   }

   mov r11, LineWidth1;
   mov r12, LineWidth2;

   //iters := -(width - 1)*sizeof(double);
   mov r10, width;
   dec r10;
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

           // addition:
           movupd xmm0, [r8 + rax - 128];
           movupd xmm1, [r9 + rax - 128];
           divpd xmm0, xmm1;

           movupd [rcx + rax - 128], xmm0;

           movupd xmm0, [r8 + rax - 112];
           movupd xmm1, [r9 + rax - 112];
           divpd xmm0, xmm1;

           movupd [rcx + rax - 112], xmm0;

           movupd xmm0, [r8 + rax - 96];
           movupd xmm1, [r9 + rax - 96];
           divpd xmm0, xmm1;

           movupd [rcx + rax - 96], xmm0;

           movupd xmm0, [r8 + rax - 80];
           movupd xmm1, [r9 + rax - 80];
           divpd xmm0, xmm1;

           movupd [rcx + rax - 80], xmm0;

           movupd xmm0, [r8 + rax - 64];
           movupd xmm1, [r9 + rax - 64];
           divpd xmm0, xmm1;

           movupd [rcx + rax - 64], xmm0;

           movupd xmm0, [r8 + rax - 48];
           movupd xmm1, [r9 + rax - 48];
           divpd xmm0, xmm1;

           movupd [rcx + rax - 48], xmm0;

           movupd xmm0, [r8 + rax - 32];
           movupd xmm1, [r9 + rax - 32];
           divpd xmm0, xmm1;

           movupd [rcx + rax - 32], xmm0;

           movupd xmm0, [r8 + rax - 16];
           movupd xmm1, [r9 + rax - 16];
           divpd xmm0, xmm1;

           movupd [rcx + rax - 16], xmm0;
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           movupd xmm0, [r8 + rax];
           movupd xmm1, [r9 + rax];
           divpd xmm0, xmm1;

           movupd [rcx + rax], xmm0;
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last column:
       movlpd xmm0, [r8];
       movlpd xmm1, [r9];
       divsd xmm0, xmm1;

       movlpd [rcx], xmm0;

       // next line:
       add r8, r11;
       add r9, r12;
       add rcx, rdx;

   // loop y end
   dec rbx;
   jnz @@addforyloop;

   // epilog
   mov rbx, iRBX;
   mov r11, iR11;
   mov r12, iR12;
{$IFDEF FPC}
end;
{$ENDIF}
end;

{$ENDIF}

end.
