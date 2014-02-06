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

uses MatrixConst;

procedure ASMMatrixAddAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
procedure ASMMatrixAddUnAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);

procedure ASMMatrixAddAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
procedure ASMMatrixAddUnAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);

procedure ASMMatrixSubAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
procedure ASMMatrixSubUnAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);

procedure ASMMatrixSubAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
procedure ASMMatrixSubUnAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);

{$ENDIF}

implementation

{$IFDEF CPUX64}

procedure ASMMatrixAddAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
asm
   // note: RCX = dest, RDX = destLineWidth, R8 = mt1, R9 = mt2
   .pushnv rbx;
   .pushnv r12;

   //iters := -width*sizeof(double);
   mov r10, width;
   shl r10, 3;
   imul r10, -1;

   mov r11, LineWidth1;
   mov r12, LineWidth2;

   // helper registers for the mt1, mt2 and dest pointers
   sub mt1, r10;
   sub mt2, r10;
   sub dest, r10;

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
end;

procedure ASMMatrixAddUnAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
asm
   // note: RCX = dest, RDX = destLineWidth, R8 = mt1, R9 = mt2
   .pushnv rbx;
   .pushnv r12;

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
end;

procedure ASMMatrixAddAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
asm
   // note: RCX = dest, RDX = destLineWidth, R8 = mt1, R9 = mt2
   .pushnv rbx;
   .pushnv r12;

   //iters := -(width - 1)*sizeof(double);
   mov r10, width;
   dec r10;
   shl r10, 3;
   imul r10, -1;

   mov r11, LineWidth1;
   mov r12, LineWidth2;

   // helper registers for the mt1, mt2 and dest pointers
   sub mt1, r10;
   sub mt2, r10;
   sub dest, r10;

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
       movlpd xmm0, [r8];
       addsd xmm0, [r9];

       movlpd [rcx], xmm0;

       // next line:
       add r8, r11;
       add r9, r12;
       add rcx, rdx;

   // loop y end
   dec rbx;
   jnz @@addforyloop;
end;

procedure ASMMatrixAddUnAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
asm
   // note: RCX = dest, RDX = destLineWidth, R8 = mt1, R9 = mt2
   .pushnv rbx;
   .pushnv r12;

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
       movlpd xmm0, [r8];
       addsd xmm0, [r9];

       movlpd [rcx], xmm0;

       // next line:
       add r8, r11;
       add r9, r12;
       add rcx, rdx;

   // loop y end
   dec rbx;
   jnz @@addforyloop;
end;

procedure ASMMatrixSubAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
asm
   // note: RCX = dest, RDX = destLineWidth, R8 = mt1, R9 = mt2
   .pushnv rbx;
   .pushnv r12;

   //iters := -width*sizeof(double);
   mov r10, width;
   shl r10, 3;
   imul r10, -1;

   mov r11, LineWidth1;
   mov r12, LineWidth2;

   // helper registers for the mt1, mt2 and dest pointers
   sub mt1, r10;
   sub mt2, r10;
   sub dest, r10;

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
end;

procedure ASMMatrixSubUnAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
asm
   // note: RCX = dest, RDX = destLineWidth, R8 = mt1, R9 = mt2
   .pushnv rbx;
   .pushnv r12;

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
end;

procedure ASMMatrixSubAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
asm
   // note: RCX = dest, RDX = destLineWidth, R8 = mt1, R9 = mt2
   .pushnv rbx;
   .pushnv r12;

   //iters := -(width - 1)*sizeof(double);
   mov r10, width;
   dec r10;
   shl r10, 3;
   imul r10, -1;

   mov r11, LineWidth1;
   mov r12, LineWidth2;

   // helper registers for the mt1, mt2 and dest pointers
   sub mt1, r10;
   sub mt2, r10;
   sub dest, r10;

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
       movlpd xmm0, [r8];
       subsd xmm0, [r9];

       movlpd [rcx], xmm0;

       // next line:
       add r8, r11;
       add r9, r12;
       add rcx, rdx;

   // loop y end
   dec rbx;
   jnz @@addforyloop;
end;

procedure ASMMatrixSubUnAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
asm
   // note: RCX = dest, RDX = destLineWidth, R8 = mt1, R9 = mt2
   .pushnv rbx;
   .pushnv r12;

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
       movlpd xmm0, [r8];
       subsd xmm0, [r9];

       movlpd [rcx], xmm0;

       // next line:
       add r8, r11;
       add r9, r12;
       add rcx, rdx;

   // loop y end
   dec rbx;
   jnz @@addforyloop;
end;

{$ENDIF}

end.
