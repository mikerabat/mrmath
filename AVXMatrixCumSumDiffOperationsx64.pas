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

      {$IFDEF FPC}vxorpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$57,$C0;{$ENDIF} 
      @@forxloop:
         {$IFDEF FPC}vmovsd xmm1, [r8 + rax];{$ELSE}db $C4,$C1,$7B,$10,$0C,$00;{$ENDIF} 
         {$IFDEF FPC}vaddsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$58,$C1;{$ENDIF} 
         {$IFDEF FPC}vmovsd [rcx + rax], xmm0;{$ELSE}db $C5,$FB,$11,$04,$01;{$ENDIF} 
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
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 
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
       {$IFDEF FPC}vxorpd ymm0, ymm0, ymm0;{$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} 
       xor rdi, rdi;
       xor rsi, rsi;

       // 4 values at once
       @@foryloop:
           {$IFDEF FPC}vmovupd ymm1, [r8 + rdi];{$ELSE}db $C4,$C1,$7D,$10,$0C,$38;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 
           {$IFDEF FPC}vmovupd [rcx + rsi], ymm0;{$ELSE}db $C5,$FD,$11,$04,$31;{$ENDIF} 

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
       {$IFDEF FPC}vxorpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$57,$C0;{$ENDIF} 
       xor rdi, rdi;
       xor rsi, rsi;

       // 4 values at once
       @@foryloopshort:
           {$IFDEF FPC}vmovsd xmm1, [r8 + rdi];{$ELSE}db $C4,$C1,$7B,$10,$0C,$38;{$ENDIF} 
           {$IFDEF FPC}vaddsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$58,$C1;{$ENDIF} 
           {$IFDEF FPC}vmovsd [rcx + rsi], xmm0;{$ELSE}db $C5,$FB,$11,$04,$31;{$ENDIF} 

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
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 
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
       {$IFDEF FPC}vxorpd ymm0, ymm0, ymm0;{$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} 
       xor rdi, rdi;
       xor rsi, rsi;

       // 4 values at once
       @@foryloop:
           {$IFDEF FPC}vmovapd ymm1, [r8 + rdi];{$ELSE}db $C4,$C1,$7D,$28,$0C,$38;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 
           {$IFDEF FPC}vmovapd [rcx + rsi], ymm0;{$ELSE}db $C5,$FD,$29,$04,$31;{$ENDIF} 

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
       {$IFDEF FPC}vxorpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$57,$C0;{$ENDIF} 
       xor rdi, rdi;
       xor rsi, rsi;

       // 4 values at once
       @@foryloopshort:
           {$IFDEF FPC}vmovsd xmm1, [r8 + rdi];{$ELSE}db $C4,$C1,$7B,$10,$0C,$38;{$ENDIF} 
           {$IFDEF FPC}vaddsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$58,$C1;{$ENDIF} 
           {$IFDEF FPC}vmovsd [rcx + rsi], xmm0;{$ELSE}db $C5,$FB,$11,$04,$31;{$ENDIF} 

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
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 
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
       {$IFDEF FPC}vmovsd xmm1, [r8 + rax - 8];{$ELSE}db $C4,$C1,$7B,$10,$4C,$00,$F8;{$ENDIF} 
       @@forxloop:
           {$IFDEF FPC}vmovsd xmm0, [r8 + rax];{$ELSE}db $C4,$C1,$7B,$10,$04,$00;{$ENDIF} 
           {$IFDEF FPC}vsubsd xmm2, xmm0, xmm1;{$ELSE}db $C5,$FB,$5C,$D1;{$ENDIF} 
           {$IFDEF FPC}vmovsd [rcx + rax - 8], xmm2;{$ELSE}db $C5,$FB,$11,$54,$01,$F8;{$ENDIF} 
           {$IFDEF FPC}vmovapd xmm1, xmm0;{$ELSE}db $C5,$F9,$28,$C8;{$ENDIF} 
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
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 
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
       {$IFDEF FPC}vxorpd ymm0, ymm0, ymm0;{$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} 
       mov rdi, r9;
       xor rsi, rsi;

       {$IFDEF FPC}vmovupd ymm0, [r8];{$ELSE}db $C4,$C1,$7D,$10,$00;{$ENDIF} 

       // 4 values at once
       @@foryloop:
           {$IFDEF FPC}vmovupd ymm1, [r8 + rdi];{$ELSE}db $C4,$C1,$7D,$10,$0C,$38;{$ENDIF} 
           {$IFDEF FPC}vsubpd ymm2, ymm1, ymm0;{$ELSE}db $C5,$F5,$5C,$D0;{$ENDIF} 
           {$IFDEF FPC}vmovupd [rcx + rsi], ymm2;{$ELSE}db $C5,$FD,$11,$14,$31;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm0, ymm1;{$ELSE}db $C5,$FD,$28,$C1;{$ENDIF} 

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
      {$IFDEF FPC}vxorpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$57,$C0;{$ENDIF} 
      mov rdi, r9;
      xor rsi, rsi;

      {$IFDEF FPC}vmovsd xmm0, [r8];{$ELSE}db $C4,$C1,$7B,$10,$00;{$ENDIF} 

      // one column value:
      @@foryloopshort:
          {$IFDEF FPC}vmovsd xmm1, [r8 + rdi];{$ELSE}db $C4,$C1,$7B,$10,$0C,$38;{$ENDIF} 
          {$IFDEF FPC}vsubsd xmm2, xmm1, xmm0;{$ELSE}db $C5,$F3,$5C,$D0;{$ENDIF} 
          {$IFDEF FPC}vmovsd [rcx + rsi], xmm2;{$ELSE}db $C5,$FB,$11,$14,$31;{$ENDIF} 

          {$IFDEF FPC}vmovsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$10,$C1;{$ENDIF} 

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
       {$IFDEF FPC}vxorpd ymm0, ymm0, ymm0;{$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} 
       mov rdi, r9;
       xor rsi, rsi;

       {$IFDEF FPC}vmovupd ymm0, [r8];{$ELSE}db $C4,$C1,$7D,$10,$00;{$ENDIF} 

       // 4 values at once
       @@foryloop:
           {$IFDEF FPC}vmovapd ymm1, [r8 + rdi];{$ELSE}db $C4,$C1,$7D,$28,$0C,$38;{$ENDIF} 
           {$IFDEF FPC}vsubpd ymm2, ymm1, ymm0;{$ELSE}db $C5,$F5,$5C,$D0;{$ENDIF} 
           {$IFDEF FPC}vmovapd [rcx + rsi], ymm2;{$ELSE}db $C5,$FD,$29,$14,$31;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm0, ymm1;{$ELSE}db $C5,$FD,$28,$C1;{$ENDIF} 

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
      {$IFDEF FPC}vxorpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$57,$C0;{$ENDIF} 
      mov rdi, r9;
      xor rsi, rsi;

      {$IFDEF FPC}vmovsd xmm0, [r8];{$ELSE}db $C4,$C1,$7B,$10,$00;{$ENDIF} 

      // one column value:
      @@foryloopshort:
          {$IFDEF FPC}vmovsd xmm1, [r8 + rdi];{$ELSE}db $C4,$C1,$7B,$10,$0C,$38;{$ENDIF} 
          {$IFDEF FPC}vsubsd xmm2, xmm1, xmm0;{$ELSE}db $C5,$F3,$5C,$D0;{$ENDIF} 
          {$IFDEF FPC}vmovsd [rcx + rsi], xmm2;{$ELSE}db $C5,$FB,$11,$14,$31;{$ENDIF} 

          {$IFDEF FPC}vmovsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$10,$C1;{$ENDIF} 

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
