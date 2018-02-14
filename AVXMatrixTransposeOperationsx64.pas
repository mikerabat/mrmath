// ###################################################################
// #### This file is part of the mathematics library project, and is
// #### offered under the licence agreement described on
// #### http://www.mrsoft.org/
// ####
// #### Copyright:(c) 2017, Michael R. . All rights reserved.
// ####
// #### Unless required by applicable law or agreed to in writing, software
// #### distributed under the License is distributed on an "AS IS" BASIS,
// #### WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// #### See the License for the specific language governing permissions and
// #### limitations under the License.
// ###################################################################


unit AVXMatrixTransposeOperationsx64;

interface

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF x64}

uses MatrixConst;

procedure AVXMatrixTransposeAligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt : PDouble; const LineWidth : TASMNativeInt; width : TASMNativeInt; height : TASMNativeInt);
procedure AVXMatrixTransposeUnaligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt : PDouble; const LineWidth : TASMNativeInt; width : TASMNativeInt; height : TASMNativeInt);

// Inplace Trasnposition of an N x N matrix
procedure AVXMatrixTransposeInplace(mt : PDouble; const LineWidth : TASMNativeInt; N : TASMNativeInt);

{$ENDIF}

implementation

{$IFDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$ENDIF}

procedure AVXMatrixTransposeAligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt : PDouble; const LineWidth : TASMNativeInt; width : TASMNativeInt; height : TASMNativeInt);
var iR10, iR11, iR12, iR13, iR14, iR15, iRSI, iRDI : TASMNativeInt;
    dXMM5, dXMM6, dXMM7 : Array[0..1] of double;
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

   // rcx = dest, rdx = destLineWidth, r8 = mt, r9 = LineWidth
   mov iR10, r10;
   mov iR11, r11;
   mov iR12, r12;
   mov iR13, r13;
   mov iR14, r14;
   mov iR15, r15;
   mov iRSI, rsi;
   mov iRDI, rdi;

   vmovupd dXMM5, xmm5;
   vmovupd dXMM6, xmm6;
   vmovupd dXMM7, xmm7;


   // rcx = dest, rdx = destLineWidth, r8 = mt, r9 = LineWidth
   mov iR12, r12;
   mov iR13, r13;
   mov iR14, r14;
   mov iRDI, rdi;

   // iters := -width*sizeof(double);
   mov r10, width;
   shl r10, 3;
   imul r10, -1;

   sub r8, r10;

   // mtLineWidth2 := 4**LineWidth
   mov r11, r9;
   shl r11, 2;

   mov rdi, height;
   sub rdi, 4;
   jl @@foryloop4End;

   // we have at least a height of 4 -> try to transpose 4x4 blocks
   @foryloop4:
      // prepare pointers to mt
      mov r12, r8;
      mov r13, r12;
      add r13, r9;   // + linewidth
      mov r14, r13;
      add r14, r9;
      mov r15, r14;
      add r15, r9;

      mov rsi, rcx;

      // 4x4 blockwise transposition
      mov rax, r10;
      @forxloop4:
         add rax, 32;
         jg @loopend4;

         vmovapd ymm0, [r12 + rax - 32];
         vmovapd ymm1, [r13 + rax - 32];
         vmovapd ymm2, [r14 + rax - 32];
         vmovapd ymm3, [r15 + rax - 32];

         vunpckhpd ymm4, ymm0, ymm1;
         vunpckhpd ymm5, ymm2, ymm3;
         vunpcklpd ymm7, ymm2, ymm3;

         vperm2f128 ymm3, ymm4, ymm5, $31;

         vunpcklpd ymm6, ymm0, ymm1;
         vinsertf128 ymm1, ymm4, xmm5, 1;
         vperm2f128 ymm2, ymm6, ymm7, $31;
         vinsertf128 ymm0, ymm6, xmm7, 1;

         vmovapd [rsi], ymm0;
         add rsi, rdx;
         vmovapd [rsi], ymm1;
         add rsi, rdx;
         vmovapd [rsi], ymm2;
         add rsi, rdx;
         vmovapd [rsi], ymm3;
         add rsi, rdx;
      jmp @forxloop4;

      @loopend4:
      sub rax, 32;
      jz @nextline4;

       // handle the missing columns
      @forxloop:
         vmovsd xmm0, [r12 + rax];
         vmovsd [rsi], xmm0;

         vmovsd xmm0, [r13 + rax];
         vmovsd [rsi + 8], xmm0;

         vmovsd xmm0, [r14 + rax];
         vmovsd [rsi + 16], xmm0;

         vmovsd xmm0, [r15 + rax];
         vmovsd [rsi + 24], xmm0;
         add rsi, rdx;

      add rax, 8;
      jnz @forxloop;

      @nextline4:

      // dest := dest + 4*sizeof(double)
      add rcx, 32;
      add r8, r11; // src := src + 4*LineWidth
   sub rdi, 4;
   jge @foryloop4;

   @@foryloop4End:

   add rdi, 4;
   jz @endproc;

   // #############################
   // #### handle the last max 4 rows
   @@foryloop:
      mov rsi, rcx;
      mov rax, r10;

      @@forxloop:
         vmovsd xmm0, [r8 + rax];
         vmovsd [rsi], xmm0;
         add rsi, rdx;
      add rax, 8;
      jnz @@forxloop;

      add rcx, 8;
      add r8, r9;
   dec rdi;
   jnz @@foryloop;

   @endproc:

   // epilog
   mov r10, iR10;
   mov r11, iR11;
   mov r12, iR12;
   mov r13, iR13;
   mov r14, iR14;
   mov r15, iR15;
   mov rsi, iRSI;
   mov rdi, iRDI;

   vmovupd xmm5, dXMM5;
   vmovupd xmm6, dXMM6;
   vmovupd xmm7, dXMM7;
   vzeroupper;
end;
{$IFDEF FPC}
end;
{$ENDIF}

procedure AVXMatrixTransposeUnaligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt : PDouble; const LineWidth : TASMNativeInt; width : TASMNativeInt; height : TASMNativeInt);
var iR10, iR11, iR12, iR13, iR14, iR15, iRSI, iRDI : TASMNativeInt;
    dXMM5, dXMM6, dXMM7 : Array[0..1] of double;
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

   // rcx = dest, rdx = destLineWidth, r8 = mt, r9 = LineWidth
   mov iR10, r10;
   mov iR11, r11;
   mov iR12, r12;
   mov iR13, r13;
   mov iR14, r14;
   mov iR15, r15;
   mov iRSI, rsi;
   mov iRDI, rdi;

   vmovupd dXMM5, xmm5;
   vmovupd dXMM6, xmm6;
   vmovupd dXMM7, xmm7;


   // rcx = dest, rdx = destLineWidth, r8 = mt, r9 = LineWidth
   mov iR12, r12;
   mov iR13, r13;
   mov iR14, r14;
   mov iRDI, rdi;

   // iters := -width*sizeof(double);
   mov r10, width;
   shl r10, 3;
   imul r10, -1;

   sub r8, r10;

   // mtLineWidth2 := 4**LineWidth
   mov r11, r9;
   shl r11, 2;

   mov rdi, height;
   sub rdi, 4;
   jl @@foryloop4End;

   // we have at least a height of 4 -> try to transpose 4x4 blocks
   @foryloop4:
      // prepare pointers to mt
      mov r12, r8;
      mov r13, r12;
      add r13, r9;   // + linewidth
      mov r14, r13;
      add r14, r9;
      mov r15, r14;
      add r15, r9;

      mov rsi, rcx;

      // 4x4 blockwise transposition
      mov rax, r10;
      @forxloop4:
         add rax, 32;
         jg @loopend4;

         vmovupd ymm0, [r12 + rax - 32];
         vmovupd ymm1, [r13 + rax - 32];
         vmovupd ymm2, [r14 + rax - 32];
         vmovupd ymm3, [r15 + rax - 32];

         vunpckhpd ymm4, ymm0, ymm1;
         vunpckhpd ymm5, ymm2, ymm3;
         vunpcklpd ymm7, ymm2, ymm3;

         vperm2f128 ymm3, ymm4, ymm5, $31;

         vunpcklpd ymm6, ymm0, ymm1;
         vinsertf128 ymm1, ymm4, xmm5, 1;
         vperm2f128 ymm2, ymm6, ymm7, $31;
         vinsertf128 ymm0, ymm6, xmm7, 1;

         vmovupd [rsi], ymm0;
         add rsi, rdx;
         vmovupd [rsi], ymm1;
         add rsi, rdx;
         vmovupd [rsi], ymm2;
         add rsi, rdx;
         vmovupd [rsi], ymm3;
         add rsi, rdx;
      jmp @forxloop4;

      @loopend4:
      sub rax, 32;
      jz @nextline4;

       // handle the missing columns
      @forxloop:
         vmovsd xmm0, [r12 + rax];
         vmovsd [rsi], xmm0;

         vmovsd xmm0, [r13 + rax];
         vmovsd [rsi + 8], xmm0;

         vmovsd xmm0, [r14 + rax];
         vmovsd [rsi + 16], xmm0;

         vmovsd xmm0, [r15 + rax];
         vmovsd [rsi + 24], xmm0;
         add rsi, rdx;

      add rax, 8;
      jnz @forxloop;

      @nextline4:

      // dest := dest + 4*sizeof(double)
      add rcx, 32;
      add r8, r11; // src := src + 4*LineWidth
   sub rdi, 4;
   jge @foryloop4;

   @@foryloop4End:

   add rdi, 4;
   jz @endproc;

   // #############################
   // #### handle the last max 4 rows
   @@foryloop:
      mov rsi, rcx;
      mov rax, r10;

      @@forxloop:
         vmovsd xmm0, [r8 + rax];
         vmovsd [rsi], xmm0;
         add rsi, rdx;
      add rax, 8;
      jnz @@forxloop;

      add rcx, 8;
      add r8, r9;
   dec rdi;
   jnz @@foryloop;

   @endproc:

   // epilog
   mov r10, iR10;
   mov r11, iR11;
   mov r12, iR12;
   mov r13, iR13;
   mov r14, iR14;
   mov r15, iR15;
   mov rsi, iRSI;
   mov rdi, iRDI;

   vmovupd xmm5, dXMM5;
   vmovupd xmm6, dXMM6;
   vmovupd xmm7, dXMM7;
   vzeroupper;
end;
{$IFDEF FPC}
end;
{$ENDIF}

// simple Inplace Trasnposition of an N x N matrix
procedure AVXMatrixTransposeInplace(mt : PDouble; const LineWidth : TASMNativeInt; N : TASMNativeInt);
var iRBX, iRDI, iRSI : TASMNativeInt;
{$IFDEF FPC}
begin
{$ENDIF}
// rcx: mt, rdx, LineWidth, r8: N
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
         vmovsd xmm0, [rcx + rdi];
         vmovsd xmm1, [rsi];

         vmovsd [rcx + rdi], xmm1;
         vmovsd [rsi], xmm0;

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

   vzeroupper;
end;
{$IFDEF FPC}
end;
{$ENDIF}

{$ENDIF}

end.
