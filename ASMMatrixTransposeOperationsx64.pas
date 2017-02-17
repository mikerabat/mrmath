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

procedure ASMMatrixTransposeAlignedEvenWEvenH(dest : PDouble; const destLineWidth : TASMNativeInt; mt : PDouble; const LineWidth : TASMNativeInt; width : TASMNativeInt; height : TASMNativeInt);
procedure ASMMatrixTransposeUnAlignedEvenWEvenH(dest : PDouble; const destLineWidth : TASMNativeInt; mt : PDouble; const LineWidth : TASMNativeInt; width : TASMNativeInt; height : TASMNativeInt);

procedure ASMMatrixTransposeAlignedEvenWOddH(dest : PDouble; const destLineWidth : TASMNativeInt; mt : PDouble; const LineWidth : TASMNativeInt; width : TASMNativeInt; height : TASMNativeInt);
procedure ASMMatrixTransposeUnAlignedEvenWOddH(dest : PDouble; const destLineWidth : TASMNativeInt; mt : PDouble; const LineWidth : TASMNativeInt; width : TASMNativeInt; height : TASMNativeInt);

procedure ASMMatrixTransposeAlignedOddWEvenH(dest : PDouble; const destLineWidth : TASMNativeInt; mt : PDouble; const LineWidth : TASMNativeInt; width : TASMNativeInt; height : TASMNativeInt);
procedure ASMMatrixTransposeUnAlignedOddWEvenH(dest : PDouble; const destLineWidth : TASMNativeInt; mt : PDouble; const LineWidth : TASMNativeInt; width : TASMNativeInt; height : TASMNativeInt);

procedure ASMMatrixTransposeAlignedOddWOddH(dest : PDouble; const destLineWidth : TASMNativeInt; mt : PDouble; const LineWidth : TASMNativeInt; width : TASMNativeInt; height : TASMNativeInt);
procedure ASMMatrixTransposeUnAlignedOddWOddH(dest : PDouble; const destLineWidth : TASMNativeInt; mt : PDouble; const LineWidth : TASMNativeInt; width : TASMNativeInt; height : TASMNativeInt);

{$ENDIF}

implementation

{$IFDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$ENDIF}

procedure ASMMatrixTransposeAlignedEvenWEvenH(dest : PDouble; const destLineWidth : TASMNativeInt; mt : PDouble; const LineWidth : TASMNativeInt; width : TASMNativeInt; height : TASMNativeInt);
var iR12, iR13, iR14, iRDI : TASMNativeInt;
{$IFDEF FPC}
begin
{$ENDIF}
asm
   // rcx = dest, rdx = destLineWidth, r8 = mt, r9 = LineWidth
   mov iR12, r12;
   mov iR13, r13;
   mov iR14, r14;
   mov iRDI, rdi;
   {
   .pushnv r12;
   .pushnv r13;
   .pushnv r14;
   .pushnv rdi;
   }

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
{$IFDEF FPC}
end;
{$ENDIF}
end;

procedure ASMMatrixTransposeUnAlignedEvenWEvenH(dest : PDouble; const destLineWidth : TASMNativeInt; mt : PDouble; const LineWidth : TASMNativeInt; width : TASMNativeInt; height : TASMNativeInt);
var iR12, iR13, iR14, iRDI : TASMNativeInt;
{$IFDEF FPC}
begin
{$ENDIF}
asm
   // rcx = dest, rdx = destLineWidth, r8 = mt, r9 = LineWidth
   mov iR12, r12;
   mov iR13, r13;
   mov iR14, r14;
   mov iRDI, rdi;
   {
   .pushnv r12;
   .pushnv r13;
   .pushnv r14;
   .pushnv rdi;
   }

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
{$IFDEF FPC}
end;
{$ENDIF}
end;


procedure ASMMatrixTransposeAlignedEvenWOddH(dest : PDouble; const destLineWidth : TASMNativeInt; mt : PDouble; const LineWidth : TASMNativeInt; width : TASMNativeInt; height : TASMNativeInt);
var iR12, iR13, iR14, iRDI : TASMNativeInt;
{$IFDEF FPC}
begin
{$ENDIF}
asm
   // rcx = dest, rdx = destLineWidth, r8 = mt, r9 = LineWidth
   mov iR12, r12;
   mov iR13, r13;
   mov iR14, r14;
   mov iRDI, rdi;
   {
   .pushnv r12;
   .pushnv r13;
   .pushnv r14;
   .pushnv rdi;
   }

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
{$IFDEF FPC}
end;
{$ENDIF}
end;

procedure ASMMatrixTransposeUnAlignedEvenWOddH(dest : PDouble; const destLineWidth : TASMNativeInt; mt : PDouble; const LineWidth : TASMNativeInt; width : TASMNativeInt; height : TASMNativeInt);
var iR12, iR13, iR14, iRDI : TASMNativeInt;
{$IFDEF FPC}
begin
{$ENDIF}
asm
   // rcx = dest, rdx = destLineWidth, r8 = mt, r9 = LineWidth
   mov iR12, r12;
   mov iR13, r13;
   mov iR14, r14;
   mov iRDI, rdi;
   {
   .pushnv r12;
   .pushnv r13;
   .pushnv r14;
   .pushnv rdi;
   }

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
{$IFDEF FPC}
end;
{$ENDIF}
end;

procedure ASMMatrixTransposeAlignedOddWEvenH(dest : PDouble; const destLineWidth : TASMNativeInt; mt : PDouble; const LineWidth : TASMNativeInt; width : TASMNativeInt; height : TASMNativeInt);
var iR12, iR13, iR14, iRDI : TASMNativeInt;
{$IFDEF FPC}
begin
{$ENDIF}
asm
   // rcx = dest, rdx = destLineWidth, r8 = mt, r9 = LineWidth
   mov iR12, r12;
   mov iR13, r13;
   mov iR14, r14;
   mov iRDI, rdi;
   {
   .pushnv r12;
   .pushnv r13;
   .pushnv r14;
   .pushnv rdi;
   }

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
{$IFDEF FPC}
end;
{$ENDIF}
end;

procedure ASMMatrixTransposeUnAlignedOddWEvenH(dest : PDouble; const destLineWidth : TASMNativeInt; mt : PDouble; const LineWidth : TASMNativeInt; width : TASMNativeInt; height : TASMNativeInt);
var iR12, iR13, iR14, iRDI : TASMNativeInt;
{$IFDEF FPC}
begin
{$ENDIF}
asm
   // rcx = dest, rdx = destLineWidth, r8 = mt, r9 = LineWidth
   mov iR12, r12;
   mov iR13, r13;
   mov iR14, r14;
   mov iRDI, rdi;
   {
   .pushnv r12;
   .pushnv r13;
   .pushnv r14;
   .pushnv rdi;
   }

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
{$IFDEF FPC}
end;
{$ENDIF}
end;


procedure ASMMatrixTransposeAlignedOddWOddH(dest : PDouble; const destLineWidth : TASMNativeInt; mt : PDouble; const LineWidth : TASMNativeInt; width : TASMNativeInt; height : TASMNativeInt);
var iR12, iR13, iR14, iRDI : TASMNativeInt;
{$IFDEF FPC}
begin
{$ENDIF}
asm
   // rcx = dest, rdx = destLineWidth, r8 = mt, r9 = LineWidth
   mov iR12, r12;
   mov iR13, r13;
   mov iR14, r14;
   mov iRDI, rdi;
   {
   .pushnv r12;
   .pushnv r13;
   .pushnv r14;
   .pushnv rdi;
   }

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
{$IFDEF FPC}
end;
{$ENDIF}
end;

procedure ASMMatrixTransposeUnAlignedOddWOddH(dest : PDouble; const destLineWidth : TASMNativeInt; mt : PDouble; const LineWidth : TASMNativeInt; width : TASMNativeInt; height : TASMNativeInt);
var iR12, iR13, iR14, iRDI : TASMNativeInt;
{$IFDEF FPC}
begin
{$ENDIF}
asm
   // rcx = dest, rdx = destLineWidth, r8 = mt, r9 = LineWidth
   mov iR12, r12;
   mov iR13, r13;
   mov iR14, r14;
   mov iRDI, rdi;
   {
   .pushnv r12;
   .pushnv r13;
   .pushnv r14;
   .pushnv rdi;
   }

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
{$IFDEF FPC}
end;
{$ENDIF}
end;


{$ENDIF}

end.
