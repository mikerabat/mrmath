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

procedure AVXMatrixTransposeAligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt : PDouble; const LineWidth : TASMNativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}
procedure AVXMatrixTransposeUnaligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt : PDouble; const LineWidth : TASMNativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}

// Inplace Trasnposition of an N x N matrix
procedure AVXMatrixTransposeInplace(mt : PDouble; const LineWidth : TASMNativeInt; N : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}

{$ENDIF}

implementation

{$IFDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$S-}  {$warnings off} {$ENDIF}

procedure AVXMatrixTransposeAligned(dest : PDouble; const destLineWidth : TASMNativeInt;
  mt : PDouble; const LineWidth : TASMNativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}
var iR12, iR13, iR14, iR15, iRSI, iRDI : TASMNativeInt;
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

   // rcx = dest, rdx = destLineWidth, r8 = mt, r9 = LineWidth
   mov iR12, r12;
   mov iR13, r13;
   mov iR14, r14;
   mov iR15, r15;
   mov iRSI, rsi;
   mov iRDI, rdi;

   // stack handling
   sub rsp, $40;
   {$IFDEF FPC}vmovupd [rsp + $00], xmm4;{$ELSE}db $C5,$F9,$11,$24,$24;{$ENDIF} 
   {$IFDEF FPC}vmovupd [rsp + $10], xmm5;{$ELSE}db $C5,$F9,$11,$6C,$24,$10;{$ENDIF} 
   {$IFDEF FPC}vmovupd [rsp + $20], xmm6;{$ELSE}db $C5,$F9,$11,$74,$24,$20;{$ENDIF} 
   {$IFDEF FPC}vmovupd [rsp + $30], xmm7;{$ELSE}db $C5,$F9,$11,$7C,$24,$30;{$ENDIF} 

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

         {$IFDEF FPC}vmovapd ymm0, [r12 + rax - 32];{$ELSE}db $C4,$C1,$7D,$28,$44,$04,$E0;{$ENDIF} 
         {$IFDEF FPC}vmovapd ymm1, [r13 + rax - 32];{$ELSE}db $C4,$C1,$7D,$28,$4C,$05,$E0;{$ENDIF} 
         {$IFDEF FPC}vmovapd ymm2, [r14 + rax - 32];{$ELSE}db $C4,$C1,$7D,$28,$54,$06,$E0;{$ENDIF} 
         {$IFDEF FPC}vmovapd ymm3, [r15 + rax - 32];{$ELSE}db $C4,$C1,$7D,$28,$5C,$07,$E0;{$ENDIF} 

         {$IFDEF FPC}vunpckhpd ymm4, ymm0, ymm1;{$ELSE}db $C5,$FD,$15,$E1;{$ENDIF} 
         {$IFDEF FPC}vunpckhpd ymm5, ymm2, ymm3;{$ELSE}db $C5,$ED,$15,$EB;{$ENDIF} 
         {$IFDEF FPC}vunpcklpd ymm7, ymm2, ymm3;{$ELSE}db $C5,$ED,$14,$FB;{$ENDIF} 

         {$IFDEF FPC}vperm2f128 ymm3, ymm4, ymm5, $31;{$ELSE}db $C4,$E3,$5D,$06,$DD,$31;{$ENDIF} 

         {$IFDEF FPC}vunpcklpd ymm6, ymm0, ymm1;{$ELSE}db $C5,$FD,$14,$F1;{$ENDIF} 
         {$IFDEF FPC}vinsertf128 ymm1, ymm4, xmm5, 1;{$ELSE}db $C4,$E3,$5D,$18,$CD,$01;{$ENDIF} 
         {$IFDEF FPC}vperm2f128 ymm2, ymm6, ymm7, $31;{$ELSE}db $C4,$E3,$4D,$06,$D7,$31;{$ENDIF} 
         {$IFDEF FPC}vinsertf128 ymm0, ymm6, xmm7, 1;{$ELSE}db $C4,$E3,$4D,$18,$C7,$01;{$ENDIF} 

         {$IFDEF FPC}vmovapd [rsi], ymm0;{$ELSE}db $C5,$FD,$29,$06;{$ENDIF} 
         add rsi, rdx;
         {$IFDEF FPC}vmovapd [rsi], ymm1;{$ELSE}db $C5,$FD,$29,$0E;{$ENDIF} 
         add rsi, rdx;
         {$IFDEF FPC}vmovapd [rsi], ymm2;{$ELSE}db $C5,$FD,$29,$16;{$ENDIF} 
         add rsi, rdx;
         {$IFDEF FPC}vmovapd [rsi], ymm3;{$ELSE}db $C5,$FD,$29,$1E;{$ENDIF} 
         add rsi, rdx;
      jmp @forxloop4;

      @loopend4:
      sub rax, 32;
      jz @nextline4;

       // handle the missing columns
      @forxloop:
         {$IFDEF FPC}vmovsd xmm0, [r12 + rax];{$ELSE}db $C4,$C1,$7B,$10,$04,$04;{$ENDIF} 
         {$IFDEF FPC}vmovsd [rsi], xmm0;{$ELSE}db $C5,$FB,$11,$06;{$ENDIF} 

         {$IFDEF FPC}vmovsd xmm0, [r13 + rax];{$ELSE}db $C4,$A1,$7B,$10,$04,$28;{$ENDIF} 
         {$IFDEF FPC}vmovsd [rsi + 8], xmm0;{$ELSE}db $C5,$FB,$11,$46,$08;{$ENDIF} 

         {$IFDEF FPC}vmovsd xmm0, [r14 + rax];{$ELSE}db $C4,$C1,$7B,$10,$04,$06;{$ENDIF} 
         {$IFDEF FPC}vmovsd [rsi + 16], xmm0;{$ELSE}db $C5,$FB,$11,$46,$10;{$ENDIF} 

         {$IFDEF FPC}vmovsd xmm0, [r15 + rax];{$ELSE}db $C4,$C1,$7B,$10,$04,$07;{$ENDIF} 
         {$IFDEF FPC}vmovsd [rsi + 24], xmm0;{$ELSE}db $C5,$FB,$11,$46,$18;{$ENDIF} 
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
         {$IFDEF FPC}vmovsd xmm0, [r8 + rax];{$ELSE}db $C4,$C1,$7B,$10,$04,$00;{$ENDIF} 
         {$IFDEF FPC}vmovsd [rsi], xmm0;{$ELSE}db $C5,$FB,$11,$06;{$ENDIF} 
         add rsi, rdx;
      add rax, 8;
      jnz @@forxloop;

      add rcx, 8;
      add r8, r9;
   dec rdi;
   jnz @@foryloop;

   @endproc:

   // epilog
   mov r12, iR12;
   mov r13, iR13;
   mov r14, iR14;
   mov r15, iR15;
   mov rsi, iRSI;
   mov rdi, iRDI;

   {$IFDEF FPC}vmovupd xmm4, [rsp + $00];{$ELSE}db $C5,$F9,$10,$24,$24;{$ENDIF} 
   {$IFDEF FPC}vmovupd xmm5, [rsp + $10];{$ELSE}db $C5,$F9,$10,$6C,$24,$10;{$ENDIF} 
   {$IFDEF FPC}vmovupd xmm6, [rsp + $20];{$ELSE}db $C5,$F9,$10,$74,$24,$20;{$ENDIF} 
   {$IFDEF FPC}vmovupd xmm7, [rsp + $30];{$ELSE}db $C5,$F9,$10,$7C,$24,$30;{$ENDIF} 
   add rsp, $40;
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;

procedure AVXMatrixTransposeUnaligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt : PDouble; const LineWidth : TASMNativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}
var iR12, iR13, iR14, iR15, iRSI, iRDI : TASMNativeInt;
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

   // rcx = dest, rdx = destLineWidth, r8 = mt, r9 = LineWidth
   mov iR12, r12;
   mov iR13, r13;
   mov iR14, r14;
   mov iR15, r15;
   mov iRSI, rsi;
   mov iRDI, rdi;

   sub rsp, $40;
   {$IFDEF FPC}vmovupd [rsp + $00], xmm4;{$ELSE}db $C5,$F9,$11,$24,$24;{$ENDIF} 
   {$IFDEF FPC}vmovupd [rsp + $10], xmm5;{$ELSE}db $C5,$F9,$11,$6C,$24,$10;{$ENDIF} 
   {$IFDEF FPC}vmovupd [rsp + $20], xmm6;{$ELSE}db $C5,$F9,$11,$74,$24,$20;{$ENDIF} 
   {$IFDEF FPC}vmovupd [rsp + $30], xmm7;{$ELSE}db $C5,$F9,$11,$7C,$24,$30;{$ENDIF} 

   // iters := -width*sizeof(double);
   mov r10, width;
   imul r10, -8;

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

         {$IFDEF FPC}vmovupd ymm0, [r12 + rax - 32];{$ELSE}db $C4,$C1,$7D,$10,$44,$04,$E0;{$ENDIF} 
         {$IFDEF FPC}vmovupd ymm1, [r13 + rax - 32];{$ELSE}db $C4,$C1,$7D,$10,$4C,$05,$E0;{$ENDIF} 
         {$IFDEF FPC}vmovupd ymm2, [r14 + rax - 32];{$ELSE}db $C4,$C1,$7D,$10,$54,$06,$E0;{$ENDIF} 
         {$IFDEF FPC}vmovupd ymm3, [r15 + rax - 32];{$ELSE}db $C4,$C1,$7D,$10,$5C,$07,$E0;{$ENDIF} 

         {$IFDEF FPC}vunpckhpd ymm4, ymm0, ymm1;{$ELSE}db $C5,$FD,$15,$E1;{$ENDIF} 
         {$IFDEF FPC}vunpckhpd ymm5, ymm2, ymm3;{$ELSE}db $C5,$ED,$15,$EB;{$ENDIF} 
         {$IFDEF FPC}vunpcklpd ymm7, ymm2, ymm3;{$ELSE}db $C5,$ED,$14,$FB;{$ENDIF} 

         {$IFDEF FPC}vperm2f128 ymm3, ymm4, ymm5, $31;{$ELSE}db $C4,$E3,$5D,$06,$DD,$31;{$ENDIF} 

         {$IFDEF FPC}vunpcklpd ymm6, ymm0, ymm1;{$ELSE}db $C5,$FD,$14,$F1;{$ENDIF} 
         {$IFDEF FPC}vinsertf128 ymm1, ymm4, xmm5, 1;{$ELSE}db $C4,$E3,$5D,$18,$CD,$01;{$ENDIF} 
         {$IFDEF FPC}vperm2f128 ymm2, ymm6, ymm7, $31;{$ELSE}db $C4,$E3,$4D,$06,$D7,$31;{$ENDIF} 
         {$IFDEF FPC}vinsertf128 ymm0, ymm6, xmm7, 1;{$ELSE}db $C4,$E3,$4D,$18,$C7,$01;{$ENDIF} 

         {$IFDEF FPC}vmovupd [rsi], ymm0;{$ELSE}db $C5,$FD,$11,$06;{$ENDIF} 
         add rsi, rdx;
         {$IFDEF FPC}vmovupd [rsi], ymm1;{$ELSE}db $C5,$FD,$11,$0E;{$ENDIF} 
         add rsi, rdx;
         {$IFDEF FPC}vmovupd [rsi], ymm2;{$ELSE}db $C5,$FD,$11,$16;{$ENDIF} 
         add rsi, rdx;
         {$IFDEF FPC}vmovupd [rsi], ymm3;{$ELSE}db $C5,$FD,$11,$1E;{$ENDIF} 
         add rsi, rdx;
      jmp @forxloop4;

      @loopend4:
      sub rax, 32;
      jz @nextline4;

       // handle the missing columns
      @forxloop:
         {$IFDEF FPC}vmovsd xmm0, [r12 + rax];{$ELSE}db $C4,$C1,$7B,$10,$04,$04;{$ENDIF} 
         {$IFDEF FPC}vmovsd [rsi], xmm0;{$ELSE}db $C5,$FB,$11,$06;{$ENDIF} 

         {$IFDEF FPC}vmovsd xmm0, [r13 + rax];{$ELSE}db $C4,$A1,$7B,$10,$04,$28;{$ENDIF} 
         {$IFDEF FPC}vmovsd [rsi + 8], xmm0;{$ELSE}db $C5,$FB,$11,$46,$08;{$ENDIF} 

         {$IFDEF FPC}vmovsd xmm0, [r14 + rax];{$ELSE}db $C4,$C1,$7B,$10,$04,$06;{$ENDIF} 
         {$IFDEF FPC}vmovsd [rsi + 16], xmm0;{$ELSE}db $C5,$FB,$11,$46,$10;{$ENDIF} 

         {$IFDEF FPC}vmovsd xmm0, [r15 + rax];{$ELSE}db $C4,$C1,$7B,$10,$04,$07;{$ENDIF} 
         {$IFDEF FPC}vmovsd [rsi + 24], xmm0;{$ELSE}db $C5,$FB,$11,$46,$18;{$ENDIF} 
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
         {$IFDEF FPC}vmovsd xmm0, [r8 + rax];{$ELSE}db $C4,$C1,$7B,$10,$04,$00;{$ENDIF} 
         {$IFDEF FPC}vmovsd [rsi], xmm0;{$ELSE}db $C5,$FB,$11,$06;{$ENDIF} 
         add rsi, rdx;
      add rax, 8;
      jnz @@forxloop;

      add rcx, 8;
      add r8, r9;
   dec rdi;
   jnz @@foryloop;

   @endproc:

   // epilog
   mov r12, iR12;
   mov r13, iR13;
   mov r14, iR14;
   mov r15, iR15;
   mov rsi, iRSI;
   mov rdi, iRDI;

   {$IFDEF FPC}vmovupd xmm4, [rsp + $00];{$ELSE}db $C5,$F9,$10,$24,$24;{$ENDIF} 
   {$IFDEF FPC}vmovupd xmm5, [rsp + $10];{$ELSE}db $C5,$F9,$10,$6C,$24,$10;{$ENDIF} 
   {$IFDEF FPC}vmovupd xmm6, [rsp + $20];{$ELSE}db $C5,$F9,$10,$74,$24,$20;{$ENDIF} 
   {$IFDEF FPC}vmovupd xmm7, [rsp + $30];{$ELSE}db $C5,$F9,$10,$7C,$24,$30;{$ENDIF} 
   add rsp, $40;
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;

// simple Inplace Trasnposition of an N x N matrix
procedure AVXMatrixTransposeInplace(mt : PDouble; const LineWidth : TASMNativeInt; N : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}
// rcx: mt, rdx, LineWidth, r8: N
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
   // check if N < 2 -> do nothing an a 1x1 matrix
   cmp r8, 2;
   jl @@exitProc;

   // iter: -N*sizeof(Double)
   mov rax, r8;
   imul rax, -8;

   mov r9, rcx;  // pDest1: genptr(mt, 0, 1, linewidth)
   add r9, rdx;

   sub rcx, rax;  // mt + iter

   // for y := 0 to n - 2
   dec r8;
   @@foryloop:

      mov r10, rax; // iter aka x
      add r10, 8;
      mov r11, r9;
      // for x := y + 1 to n-1 do
      @@forxloop:
         {$IFDEF FPC}vmovsd xmm0, [rcx + r10];{$ELSE}db $C4,$A1,$7B,$10,$04,$11;{$ENDIF} 
         {$IFDEF FPC}vmovsd xmm1, [r11];{$ELSE}db $C4,$C1,$7B,$10,$0B;{$ENDIF} 

         {$IFDEF FPC}vmovsd [rcx + r10], xmm1;{$ELSE}db $C4,$A1,$7B,$11,$0C,$11;{$ENDIF} 
         {$IFDEF FPC}vmovsd [r11], xmm0;{$ELSE}db $C4,$C1,$7B,$11,$03;{$ENDIF} 

         add r11, rdx;
      add r10, 8;
      jnz @@forxloop;

      add rax, 8;  // iter + sizeof(double);
      //pDest := PConstDoubleArr( GenPtr(dest, 0, y, destLineWidth) );
      add rcx, rdx;
      // GenPtr(dest, y, y + 1, destLineWidth);
      add r9, rdx;
      add r9, 8;
   dec r8;
   jnz @@foryloop;

   @@exitProc:

   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;

{$ENDIF}

end.
