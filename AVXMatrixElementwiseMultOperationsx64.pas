// ###################################################################
// #### This file is part of the mathematics library project, and is
// #### offered under the licence agreement described on
// #### http://www.mrsoft.org/
// ####
// #### Copyright:(c) 2018, Michael R. . All rights reserved.
// ####
// #### Unless required by applicable law or agreed to in writing, software
// #### distributed under the License is distributed on an "AS IS" BASIS,
// #### WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// #### See the License for the specific language governing permissions and
// #### limitations under the License.
// ###################################################################


unit AVXMatrixElementwiseMultOperationsx64;

interface

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF x64}

uses MatrixConst;

procedure AVXMatrixElemMultAligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
procedure AVXMatrixElemMultUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);

procedure AVXMatrixElemDivAligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
procedure AVXMatrixElemDivUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);

{$ENDIF}

implementation

{$IFDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$ENDIF}

procedure AVXMatrixElemMultAligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var iRBX, iR11, iR12 : TASMNativeInt;
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
   mov iR11, r11;
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

           // mult:
           vmovapd ymm0, [r8 + rax - 128];
           vmulpd ymm0, ymm0, [r9 + rax - 128];

           vmovapd [rcx + rax - 128], ymm0;

           vmovapd ymm1, [r8 + rax - 96];
           vmulpd ymm1, ymm1, [r9 + rax - 96];

           vmovapd [rcx + rax - 96], ymm1;

           vmovapd ymm2, [r8 + rax - 64];
           vmulpd ymm2, ymm2, [r9 + rax - 64];

           vmovapd [rcx + rax - 64], ymm2;

           vmovapd ymm3, [r8 + rax - 32];
           vmulpd ymm3, ymm3, [r9 + rax - 32];

           vmovapd [rcx + rax - 32], ymm3;
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           add rax, 16;
           jg @@lastElem;

           vmovapd xmm0, [r8 + rax - 16];
           vmulpd xmm0, xmm0, [r9 + rax - 16];

           vmovapd [rcx + rax - 16], xmm0;
       jmp @addforxloop2;

       @@lastElem:
       sub rax, 16;

       jz @nextLine;

       vmovsd xmm0, [r8 + rax];
       vmulsd xmm0, xmm0, [r9 + rax];
       vmovsd [rcx + rax], xmm0;

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
   vzeroupper;
{$IFDEF FPC}
end;
{$ENDIF}
end;

procedure AVXMatrixElemMultUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var iRBX, iR11, iR12 : TASMNativeInt;
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
   mov iR11, r11;
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

           // mult:
           vmovupd ymm0, [r8 + rax - 128];
           vmovupd ymm1, [r9 + rax - 128];
           vmulpd ymm0, ymm0, ymm1;

           vmovupd [rcx + rax - 128], ymm0;

           vmovupd ymm2, [r8 + rax - 96];
           vmovupd ymm3, [r9 + rax - 96];
           vmulpd ymm2, ymm2, ymm3;

           vmovupd [rcx + rax - 96], ymm2;

           vmovupd ymm0, [r8 + rax - 64];
           vmovupd ymm1, [r9 + rax - 64];
           vmulpd ymm0, ymm0, ymm1;

           vmovupd [rcx + rax - 64], ymm0;

           vmovupd ymm2, [r8 + rax - 32];
           vmovupd ymm3, [r9 + rax - 32];
           vmulpd ymm2, ymm2, ymm3;

           vmovupd [rcx + rax - 32], ymm2;
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           add rax, 16;
           jg @@lastElem;

           vmovupd xmm0, [r8 + rax - 16];
           vmovupd xmm1, [r9 + rax - 16];
           vmulpd xmm0, xmm0, xmm1;

           vmovupd [rcx + rax - 16], xmm0;
       jmp @addforxloop2;

       @@lastElem:
       sub rax, 16;

       jz @nextLine;

       vmovsd xmm0, [r8 + rax];
       vmulsd xmm0, xmm0, [r9 + rax];
       vmovsd [rcx + rax], xmm0;

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
   vzeroupper;
{$IFDEF FPC}
end;
{$ENDIF}
end;

// ##############################################################
// #### Elementwise divide of matrix 1 to matrix 2
// ##############################################################

procedure AVXMatrixElemDivAligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var iRBX, iR11, iR12 : TASMNativeInt;
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
   mov iR11, r11;
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

           // mult:
           vmovapd ymm0, [r8 + rax - 128];
           vdivpd ymm0, ymm0, [r9 + rax - 128];

           vmovapd [rcx + rax - 128], ymm0;

           vmovapd ymm1, [r8 + rax - 96];
           vdivpd ymm1, ymm1, [r9 + rax - 96];

           vmovapd [rcx + rax - 96], ymm1;

           vmovapd ymm2, [r8 + rax - 64];
           vdivpd ymm2, ymm2, [r9 + rax - 64];

           vmovapd [rcx + rax - 64], ymm2;

           vmovapd ymm3, [r8 + rax - 32];
           vdivpd ymm3, ymm3, [r9 + rax - 32];

           vmovapd [rcx + rax - 32], ymm3;
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           add rax, 16;
           jg @@lastElem;

           vmovapd xmm0, [r8 + rax - 16];
           vdivpd xmm0, xmm0, [r9 + rax - 16];

           vmovapd [rcx + rax - 16], xmm0;
       jmp @addforxloop2;

       @@lastElem:
       sub rax, 16;

       jz @nextLine;

       vmovsd xmm0, [r8 + rax];
       vdivsd xmm0, xmm0, [r9 + rax];
       vmovsd [rcx + rax], xmm0;

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
   vzeroupper;
{$IFDEF FPC}
end;
{$ENDIF}
end;

procedure AVXMatrixElemDivUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var iRBX, iR11, iR12 : TASMNativeInt;
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
   mov iR11, r11;
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

           // mult:
           vmovupd ymm0, [r8 + rax - 128];
           vmovupd ymm1, [r9 + rax - 128];
           vdivpd ymm0, ymm0, ymm1;

           vmovupd [rcx + rax - 128], ymm0;

           vmovupd ymm2, [r8 + rax - 96];
           vmovupd ymm3, [r9 + rax - 96];
           vdivpd ymm2, ymm2, ymm3;

           vmovupd [rcx + rax - 96], ymm2;

           vmovupd ymm0, [r8 + rax - 64];
           vmovupd ymm1, [r9 + rax - 64];
           vdivpd ymm0, ymm0, ymm1;

           vmovupd [rcx + rax - 64], ymm0;

           vmovupd ymm2, [r8 + rax - 32];
           vmovupd ymm3, [r9 + rax - 32];
           vdivpd ymm2, ymm2, ymm3;

           vmovupd [rcx + rax - 32], ymm2;
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           add rax, 16;
           jg @@lastElem;

           vmovupd xmm0, [r8 + rax - 16];
           vmovupd xmm1, [r9 + rax - 16];
           vdivpd xmm0, xmm0, xmm1;

           vmovupd [rcx + rax - 16], xmm0;
       jmp @addforxloop2;

       @@lastElem:
       sub rax, 16;

       jz @nextLine;

       vmovsd xmm0, [r8 + rax];
       vdivsd xmm0, xmm0, [r9 + rax];
       vmovsd [rcx + rax], xmm0;

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
   vzeroupper;
{$IFDEF FPC}
end;
{$ENDIF}
end;

{$ENDIF}

end.
