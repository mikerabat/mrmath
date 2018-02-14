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

unit AVXMatrixCumSumDiffOperationsx64;

interface


{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF x64}

uses MatrixConst;

procedure AVXMatrixCumulativeSumRow(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);

procedure AVXMatrixCumulativeSumColumnUnaligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
procedure AVXMatrixCumulativeSumColumnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);

procedure AVXMatrixDifferentiateRow(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);

procedure AVXMatrixDifferentiateColumnUnaligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
procedure AVXMatrixDifferentiateColumnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);

{$ENDIF}

implementation

{$IFDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$ENDIF}

// ##################################################
// #### Cumulative sum
// ##################################################

procedure AVXMatrixCumulativeSumRow(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
var iRBX, iRDI, iRSI : TASMNativeint;
{$IFDEF FPC}
begin
{$ENDIF}
// rcx: dest, rdx: destlinewidth; r8: src; r9 : srclinewidth
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

   // prolog - maintain stack
   mov iRBX, rbx;
   mov iRDI, rdi;
   mov iRSI, rsi;

   // if (width <= 0) or (height <= 0) then exit;
   mov rbx, width;
   cmp rbx, 0;
   jle @@exitproc;
   mov rsi, height;
   cmp rsi, 0;
   jle @@exitproc;

   // iter := -width*sizeof(Double)
   imul rbx, -8;
   // prepare counters
   sub rcx, rbx;
   sub r8, rbx;
   @@foryloop:
      mov rax, rbx;

      vxorpd xmm0, xmm0, xmm0;
      @@forxloop:
         vmovsd xmm1, [r8 + rax];
         vaddsd xmm0, xmm0, xmm1;
         vmovsd [rcx + rax], xmm0;
      add rax, 8;
      jnz @@forxloop;

      add rcx, rdx;
      add r8, r9;
   dec rsi;
   jnz @@foryloop;
   @@exitProc:

   // epilog - stack cleanup
   mov rbx, iRBX;
   mov rdi, iRDI;
   mov rsi, iRSI;
   vzeroupper;
end;
{$IFDEF FPC}
end;
{$ENDIF}

procedure AVXMatrixCumulativeSumColumnUnaligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
var iRDI, iRSI : TASMNativeint;
{$IFDEF FPC}
begin
{$ENDIF}
// rcx: dest, rdx: destlinewidth; r8: src; r9 : srclinewidth
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

   // prolog - maintain stack
   mov iRDI, rdi;
   mov iRSI, rsi;

   // if (width <= 1) or (height <= 0) then exit;
   mov r11, height;
   cmp r11, 0;

   jle @@exitproc;
   mov r10, width;
   sub r10, 4;
   jl @@lastColumns;

   @@forxloop:
       mov rax, r11;
       vxorpd ymm0, ymm0, ymm0;
       xor rdi, rdi;
       xor rsi, rsi;

       // 4 values at once
       @@foryloop:
           vmovupd ymm1, [r8 + rdi];
           vaddpd ymm0, ymm0, ymm1;
           vmovupd [rcx + rsi], ymm0;

           add rdi, r9;
           add rsi, rdx;
       dec rax;
       jnz @@foryloop;

       add r8, 32;
       add rcx, 32;
   sub r10, 4;
   jge @@forxloop;

   @@lastColumns:
   add r10, 4;
   jz @@exitProc;

   @@forxloopshort:
       mov rax, r11;
       vxorpd xmm0, xmm0, xmm0;
       xor rdi, rdi;
       xor rsi, rsi;

       // 4 values at once
       @@foryloopshort:
           vmovsd xmm1, [r8 + rdi];
           vaddsd xmm0, xmm0, xmm1;
           vmovsd [rcx + rsi], xmm0;

           add rdi, r9;
           add rsi, rdx;
       dec rax;
       jnz @@foryloopshort;

       add r8, 8;
       add rcx, 8;
   sub r10, 1;
   jg @@forxloopshort;

   @@exitProc:
   // epilog - stack cleanup
   mov rdi, iRDI;
   mov rsi, iRSI;
   vzeroupper;
end;
{$IFDEF FPC}
end;
{$ENDIF}

procedure AVXMatrixCumulativeSumColumnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
var iRDI, iRSI : TASMNativeint;
{$IFDEF FPC}
begin
{$ENDIF}
// rcx: dest, rdx: destlinewidth; r8: src; r9 : srclinewidth
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

   // prolog - maintain stack
   mov iRDI, rdi;
   mov iRSI, rsi;

   // if (width <= 1) or (height <= 0) then exit;
   mov r11, height;
   cmp r11, 0;

   jle @@exitproc;
   mov r10, width;
   sub r10, 4;
   jl @@lastColumns;

   @@forxloop:
       mov rax, r11;
       vxorpd ymm0, ymm0, ymm0;
       xor rdi, rdi;
       xor rsi, rsi;

       // 4 values at once
       @@foryloop:
           vmovapd ymm1, [r8 + rdi];
           vaddpd ymm0, ymm0, ymm1;
           vmovapd [rcx + rsi], ymm0;

           add rdi, r9;
           add rsi, rdx;
       dec rax;
       jnz @@foryloop;

       add r8, 32;
       add rcx, 32;
   sub r10, 4;
   jge @@forxloop;

   @@lastColumns:
   add r10, 4;
   jz @@exitProc;

   @@forxloopshort:
       mov rax, r11;
       vxorpd xmm0, xmm0, xmm0;
       xor rdi, rdi;
       xor rsi, rsi;

       // 4 values at once
       @@foryloopshort:
           vmovsd xmm1, [r8 + rdi];
           vaddsd xmm0, xmm0, xmm1;
           vmovsd [rcx + rsi], xmm0;

           add rdi, r9;
           add rsi, rdx;
       dec rax;
       jnz @@foryloopshort;

       add r8, 8;
       add rcx, 8;
   sub r10, 1;
   jg @@forxloopshort;

   @@exitProc:
   // epilog - stack cleanup
   mov rdi, iRDI;
   mov rsi, iRSI;
   vzeroupper;
end;
{$IFDEF FPC}
end;
{$ENDIF}

// ###############################################
// #### Differentiate
// ###############################################

procedure AVXMatrixDifferentiateRow(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
var iRBX, iRDI, iRSI : TASMNativeint;
{$IFDEF FPC}
begin
{$ENDIF}
// rcx: dest, rdx: destlinewidth; r8: src; r9 : srclinewidth
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

   // prolog - maintain stack
   mov iRBX, rbx;
   mov iRSI, rsi;

   // if (width <= 1) or (height <= 0) then exit;
   mov rbx, width;
   cmp rbx, 1;
   jle @@exitproc;
   mov rsi, height;
   cmp rsi, 0;
   jle @@exitproc;
   // iter := -width*sizeof(Double)
   imul rbx, -8;
   // prepare counters
   sub rcx, rbx;
   sub r8, rbx;
   add rbx, 8;
   @@foryloop:
       mov rax, rbx;
       vmovsd xmm1, [r8 + rax - 8];
       @@forxloop:
           vmovsd xmm0, [r8 + rax];
           vsubsd xmm2, xmm0, xmm1;
           vmovsd [rcx + rax - 8], xmm2;
           vmovapd xmm1, xmm0;
       add rax, 8;
       jnz @@forxloop;

       add rcx, rdx;
       add r8, r9;
   dec rsi;
   jnz @@foryloop;

   @@exitProc:
   // epilog - stack cleanup
   mov rbx, iRBX;
   mov rsi, iRSI;
   vzeroupper;
end;
{$IFDEF FPC}
end;
{$ENDIF}

procedure AVXMatrixDifferentiateColumnUnaligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
var iRDI, iRSI : TASMNativeint;
{$IFDEF FPC}
begin
{$ENDIF}
// rcx: dest, rdx: destlinewidth; r8: src; r9 : srclinewidth
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
   // prolog - maintain stack
   mov iRDI, rdi;
   mov iRSI, rsi;

   // if (width <= 1) or (height <= 0) then exit;
   mov r11, height;
   cmp r11, 1;
   jle @@exitproc;

   mov r10, width;
   cmp r10, 1;
   jle @@exitproc;

   dec r11;
   sub r10, 4;
   jl @@forxloopend;

   @@forxloop:
       mov rax, r11;
       vxorpd ymm0, ymm0, ymm0;
       mov rdi, r9;
       xor rsi, rsi;

       vmovupd ymm0, [r8];

       // 4 values at once
       @@foryloop:
           vmovupd ymm1, [r8 + rdi];
           vsubpd ymm2, ymm1, ymm0;
           vmovupd [rcx + rsi], ymm2;

           vmovapd ymm0, ymm1;

           add rdi, r9;
           add rsi, rdx;
       dec rax;
       jnz @@foryloop;

       add r8, 32;
       add rcx, 32;
   sub r10, 4;
   jge @@forxloop;

   @@forxloopend:
   add r10, 4;
   jz @@exitProc;

   // #####################################
   // #### last < 4 columns
   @@forxloopshort:
      mov rax, r11;
      vxorpd xmm0, xmm0, xmm0;
      mov rdi, r9;
      xor rsi, rsi;

      vmovsd xmm0, [r8];

      // one column value:
      @@foryloopshort:
          vmovsd xmm1, [r8 + rdi];
          vsubsd xmm2, xmm1, xmm0;
          vmovsd [rcx + rsi], xmm2;

          vmovsd xmm0, xmm0, xmm1;

          add rdi, r9;
          add rsi, rdx;
      dec rax;
      jnz @@foryloopshort;

      add r8, 8;
      add rcx, 8;

   dec r10;
   jnz @@forxloopshort;

   @@exitProc:

   // epilog - stack cleanup
   mov rdi, iRDI;
   mov rsi, iRSI;
end;
{$IFDEF FPC}
end;
{$ENDIF}

procedure AVXMatrixDifferentiateColumnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
var iRDI, iRSI : TASMNativeint;
{$IFDEF FPC}
begin
{$ENDIF}
// rcx: dest, rdx: destlinewidth; r8: src; r9 : srclinewidth
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
   // prolog - maintain stack
   mov iRDI, rdi;
   mov iRSI, rsi;

   // if (width <= 1) or (height <= 0) then exit;
   mov r11, height;
   cmp r11, 1;
   jle @@exitproc;

   mov r10, width;
   cmp r10, 1;
   jle @@exitproc;

   dec r11;
   sub r10, 4;
   jl @@forxloopend;

   @@forxloop:
       mov rax, r11;
       vxorpd ymm0, ymm0, ymm0;
       mov rdi, r9;
       xor rsi, rsi;

       vmovupd ymm0, [r8];

       // 4 values at once
       @@foryloop:
           vmovapd ymm1, [r8 + rdi];
           vsubpd ymm2, ymm1, ymm0;
           vmovapd [rcx + rsi], ymm2;

           vmovapd ymm0, ymm1;

           add rdi, r9;
           add rsi, rdx;
       dec rax;
       jnz @@foryloop;

       add r8, 32;
       add rcx, 32;
   sub r10, 4;
   jge @@forxloop;

   @@forxloopend:
   add r10, 4;
   jz @@exitProc;

   // #####################################
   // #### last < 4 columns
   @@forxloopshort:
      mov rax, r11;
      vxorpd xmm0, xmm0, xmm0;
      mov rdi, r9;
      xor rsi, rsi;

      vmovsd xmm0, [r8];

      // one column value:
      @@foryloopshort:
          vmovsd xmm1, [r8 + rdi];
          vsubsd xmm2, xmm1, xmm0;
          vmovsd [rcx + rsi], xmm2;

          vmovsd xmm0, xmm0, xmm1;

          add rdi, r9;
          add rsi, rdx;
      dec rax;
      jnz @@foryloopshort;

      add r8, 8;
      add rcx, 8;

   dec r10;
   jnz @@forxloopshort;

   @@exitProc:

   // epilog - stack cleanup
   mov rdi, iRDI;
   mov rsi, iRSI;
end;
{$IFDEF FPC}
end;
{$ENDIF}


{$ENDIF}

end.
