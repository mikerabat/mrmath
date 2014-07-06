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


unit ASMMatrixVectorMultOperationsx64;

// #######################################################
// #### special routines for matrix vector multiplications.
// #######################################################

interface

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF x64}

uses MatrixConst;

procedure ASMMatrixVectorMultAlignedEvenW1(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1 : TASMNativeInt);
procedure ASMMatrixVectorMultUnAlignedEvenW1(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1 : TASMNativeInt);

procedure ASMMatrixVectorMultAlignedOddW1(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1 : TASMNativeInt);
procedure ASMMatrixVectorMultUnAlignedOddW1(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1 : TASMNativeInt);

{$ENDIF}

implementation

{$IFDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$ENDIF}

procedure ASMMatrixVectorMultAlignedEvenW1(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1 : TASMNativeInt);
var iRBX, iRDI, iR12 : NativeInt;
{$IFDEF FPC}
begin
{$ENDIF}
asm
   // prolog - simulate stack
   mov iRBX, rbx;
   mov iRDI, rdi;
   mov iR12, r12;
   {
   .pushnv rbx;
   .pushnv rdi;
   .pushnv r12;
   }

   mov r10, width1;
   shl r10, 3;
   imul r10, -1;

   xorpd xmm3, xmm3;

   // prepare for reverse loop indexing
   mov r11, mt1;
   mov r12, mt2;
   sub r11, r10;
   sub r12, r10;

   mov rdi, LineWidth1;

   // init for y := 0 to height1 - 1:
   mov rbx, height1;
   @@foryloop:

       // init values:
       xorpd xmm0, xmm0;  // dest^ := 0;

       mov rax, r10;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [r11 + rax];
           // prefetch [r12 + rax];

           // addition:
           movapd xmm1, [r11 + rax - 128];
           mulpd xmm1, [r12 + rax - 128];

           addpd xmm0, xmm1;

           movapd xmm1, [r11 + rax - 112];
           mulpd xmm1, [r12 + rax - 112];

           addpd xmm0, xmm1;

           movapd xmm1, [r11 + rax - 96];
           mulpd xmm1, [r12 + rax - 96];

           addpd xmm0, xmm1;

           movapd xmm1, [r11 + rax - 80];
           mulpd xmm1, [r12 + rax - 80];

           addpd xmm0, xmm1;

           movapd xmm1, [r11 + rax - 64];
           mulpd xmm1, [r12 + rax - 64];

           addpd xmm0, xmm1;

           movapd xmm1, [r11 + rax - 48];
           mulpd xmm1, [r12 + rax - 48];

           addpd xmm0, xmm1;

           movapd xmm1, [r11 + rax - 32];
           mulpd xmm1, [r12 + rax - 32];

           addpd xmm0, xmm1;

           movapd xmm1, [r11 + rax - 16];
           mulpd xmm1, [r12 + rax - 16];

           addpd xmm0, xmm1;
       jmp @addforxloop;

       @loopEnd:
       sub rax, 128;

       @@forxloop2:
           movapd xmm1, [r11 + rax];
           mulpd xmm1, [r12 + rax];

           addpd xmm0, xmm1;
       add rax, 16;
       jnz @@forxloop2;

       // write back result (final addition and compactation)
       haddpd xmm0, xmm3;
       movlpd [rcx], xmm0;

       // next results:
       add rcx, rdx;
       add r11, rdi;
   dec rbx;
   jnz @@foryloop;

   // epilog - cleanup stack
   mov rbx, iRBX;
   mov rdi, iRDI;
   mov r12, iR12;
{$IFDEF FPC}
end;
{$ENDIF}
end;

procedure ASMMatrixVectorMultUnAlignedEvenW1(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1 : TASMNativeInt);
var iRBX, iRDI, iR12 : NativeInt;
{$IFDEF FPC}
begin
{$ENDIF}
asm
   // prolog - simulate stack
   mov iRBX, rbx;
   mov iRDI, rdi;
   mov iR12, r12;
   {
   .pushnv rbx;
   .pushnv rdi;
   .pushnv r12;
   }

   mov r10, width1;
   shl r10, 3;
   imul r10, -1;

   xorpd xmm3, xmm3;

   // prepare for reverse loop indexing
   mov r11, mt1;
   mov r12, mt2;
   sub r11, r10;
   sub r12, r10;

   mov rdi, LineWidth1;

   // init for y := 0 to height1 - 1:
   mov rbx, height1;
   @@foryloop:

       // init values:
       xorpd xmm0, xmm0;  // dest^ := 0;

       mov rax, r10;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // addition:
           movupd xmm1, [r11 + rax - 128];
           movupd xmm2, [r12 + rax - 128];
           mulpd xmm1, xmm2;
           addpd xmm0, xmm1;

           movupd xmm1, [r11 + rax - 112];
           movupd xmm2, [r12 + rax - 112];
           mulpd xmm1, xmm2;
           addpd xmm0, xmm1;

           movupd xmm1, [r11 + rax - 96];
           movupd xmm2, [r12 + rax - 96];
           mulpd xmm1, xmm2;
           addpd xmm0, xmm1;

           movupd xmm1, [r11 + rax - 80];
           movupd xmm2, [r12 + rax - 80];
           mulpd xmm1, xmm2;
           addpd xmm0, xmm1;

           movupd xmm1, [r11 + rax - 64];
           movupd xmm2, [r12 + rax - 64];
           mulpd xmm1, xmm2;
           addpd xmm0, xmm1;

           movupd xmm1, [r11 + rax - 48];
           movupd xmm2, [r12 + rax - 48];
           mulpd xmm1, xmm2;
           addpd xmm0, xmm1;

           movupd xmm1, [r11 + rax - 32];
           movupd xmm2, [r12 + rax - 32];
           mulpd xmm1, xmm2;
           addpd xmm0, xmm1;

           movupd xmm1, [r11 + rax - 16];
           movupd xmm2, [r12 + rax - 16];
           mulpd xmm1, xmm2;
           addpd xmm0, xmm1;
       jmp @addforxloop;

       @loopEnd:
       sub rax, 128;

       @@forxloop2:
           movupd xmm1, [r11 + rax];
           movupd xmm2, [r12 + rax];
           mulpd xmm1, xmm2;
           addpd xmm0, xmm1;
       add rax, 16;
       jnz @@forxloop2;

       // write back result (final addition and compactation)
       haddpd xmm0, xmm3;
       movlpd [rcx], xmm0;

       // next results:
       add rcx, rdx;
       add r11, rdi;
   dec rbx;
   jnz @@foryloop;

   // epilog - cleanup stack
   mov rbx, iRBX;
   mov rdi, iRDI;
   mov r12, iR12;
{$IFDEF FPC}
end;
{$ENDIF}
end;


procedure ASMMatrixVectorMultAlignedOddW1(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1 : TASMNativeInt);
var iRBX, iRDI, iR12 : NativeInt;
{$IFDEF FPC}
begin
  {$ENDIF}
asm
   // prolog - simulate stack
   mov iRBX, rbx;
   mov iRDI, rdi;
   mov iR12, r12;
   {
   .pushnv rbx;
   .pushnv rdi;
   .pushnv r12;
   }

   mov r10, width1;
   dec r10;
   shl r10, 3;
   imul r10, -1;

   xorpd xmm3, xmm3;

   // prepare for reverse loop indexing
   mov r11, mt1;
   mov r12, mt2;
   sub r11, r10;
   sub r12, r10;

   mov rdi, LineWidth1;

   // init for y := 0 to height1 - 1:
   mov rbx, height1;
   @@foryloop:

       // init values:
       xorpd xmm0, xmm0;  // dest^ := 0;

       mov rax, r10;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [r11 + rax];
           // prefetch [r12 + rax];

           // addition:
           movapd xmm1, [r11 + rax - 128];
           mulpd xmm1, [r12 + rax - 128];

           addpd xmm0, xmm1;

           movapd xmm1, [r11 + rax - 112];
           mulpd xmm1, [r12 + rax - 112];

           addpd xmm0, xmm1;

           movapd xmm1, [r11 + rax - 96];
           mulpd xmm1, [r12 + rax - 96];

           addpd xmm0, xmm1;

           movapd xmm1, [r11 + rax - 80];
           mulpd xmm1, [r12 + rax - 80];

           addpd xmm0, xmm1;

           movapd xmm1, [r11 + rax - 64];
           mulpd xmm1, [r12 + rax - 64];

           addpd xmm0, xmm1;

           movapd xmm1, [r11 + rax - 48];
           mulpd xmm1, [r12 + rax - 48];

           addpd xmm0, xmm1;

           movapd xmm1, [r11 + rax - 32];
           mulpd xmm1, [r12 + rax - 32];

           addpd xmm0, xmm1;

           movapd xmm1, [r11 + rax - 16];
           mulpd xmm1, [r12 + rax - 16];

           addpd xmm0, xmm1;
       jmp @addforxloop;

       @loopEnd:
       sub rax, 128;

       @@forxloop2:
           movapd xmm1, [r11 + rax];
           mulpd xmm1, [r12 + rax];

           addpd xmm0, xmm1;
       add rax, 16;
       jnz @@forxloop2;

       // special treatment for the last value:
       movlpd xmm1, [r11];
       movlpd xmm2, [r12];

       mulsd xmm1, xmm2;
       addsd xmm0, xmm1;

       // write back result (final addition and compactation)
       haddpd xmm0, xmm3;
       movlpd [rcx], xmm0;

       // next results:
       add rcx, rdx;
       add r11, rdi;
   dec rbx;
   jnz @@foryloop;

   // epilog - cleanup stack
   mov rbx, iRBX;
   mov rdi, iRDI;
   mov r12, iR12;
{$IFDEF FPC}
end;
{$ENDIF}
end;

procedure ASMMatrixVectorMultUnAlignedOddW1(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1 : TASMNativeInt);
var iRBX, iRDI, iR12 : NativeInt;
{$IFDEF FPC}
begin
  {$ENDIF}
asm
   // prolog - simulate stack
   mov iRBX, rbx;
   mov iRDI, rdi;
   mov iR12, r12;
   {
   .pushnv rbx;
   .pushnv rdi;
   .pushnv r12;
   }

   mov r10, width1;
   dec r10;
   shl r10, 3;
   imul r10, -1;

   xorpd xmm3, xmm3;

   // prepare for reverse loop indexing
   mov r11, mt1;
   mov r12, mt2;
   sub r11, r10;
   sub r12, r10;

   mov rdi, LineWidth1;

   // init for y := 0 to height1 - 1:
   mov rbx, height1;
   @@foryloop:

       // init values:
       xorpd xmm0, xmm0;  // dest^ := 0;

       mov rax, r10;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // addition:
           movupd xmm1, [r11 + rax - 128];
           movupd xmm2, [r12 + rax - 128];
           mulpd xmm1, xmm2;
           addpd xmm0, xmm1;

           movupd xmm1, [r11 + rax - 112];
           movupd xmm2, [r12 + rax - 112];
           mulpd xmm1, xmm2;
           addpd xmm0, xmm1;

           movupd xmm1, [r11 + rax - 96];
           movupd xmm2, [r12 + rax - 96];
           mulpd xmm1, xmm2;
           addpd xmm0, xmm1;

           movupd xmm1, [r11 + rax - 80];
           movupd xmm2, [r12 + rax - 80];
           mulpd xmm1, xmm2;
           addpd xmm0, xmm1;

           movupd xmm1, [r11 + rax - 64];
           movupd xmm2, [r12 + rax - 64];
           mulpd xmm1, xmm2;
           addpd xmm0, xmm1;

           movupd xmm1, [r11 + rax - 48];
           movupd xmm2, [r12 + rax - 48];
           mulpd xmm1, xmm2;
           addpd xmm0, xmm1;

           movupd xmm1, [r11 + rax - 32];
           movupd xmm2, [r12 + rax - 32];
           mulpd xmm1, xmm2;
           addpd xmm0, xmm1;

           movupd xmm1, [r11 + rax - 16];
           movupd xmm2, [r12 + rax - 16];
           mulpd xmm1, xmm2;
           addpd xmm0, xmm1;
       jmp @addforxloop;

       @loopEnd:
       sub rax, 128;

       @@forxloop2:
           movupd xmm1, [r11 + rax];
           movupd xmm2, [r12 + rax];
           mulpd xmm1, xmm2;
           addpd xmm0, xmm1;
       add rax, 16;
       jnz @@forxloop2;

       // special treatment for the last value:
       movlpd xmm1, [r11];
       movlpd xmm2, [r12];

       mulsd xmm1, xmm2;
       addsd xmm0, xmm1;

       // write back result (final addition and compactation)
       haddpd xmm0, xmm3;
       movlpd [rcx], xmm0;

       // next results:
       add rcx, rdx;
       add r11, rdi;
   dec rbx;
   jnz @@foryloop;

   // epilog - cleanup stack
   mov rbx, iRBX;
   mov rdi, iRDI;
   mov r12, iR12;
{$IFDEF FPC}
end;
{$ENDIF}
end;


{$ENDIF}

end.
