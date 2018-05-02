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


unit AVXMatrixAddSubOperationsx64;

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

procedure AVXMatrixAddAligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
procedure AVXMatrixAddUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);

procedure AVXMatrixSubAligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
procedure AVXMatrixSubUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);

procedure AVXMatrixSubT(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; LineWidthB : TASMNativeInt; width, height : TASMNativeInt);
{$ENDIF}

implementation

{$IFDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$S-} {$ENDIF}

procedure AVXMatrixAddAligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var iRBX, iR12 : TASMNativeInt;
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
           {$IFDEF FPC}vmovapd ymm0, [r8 + rax - 128];{$ELSE}db $C4,$C1,$7D,$28,$44,$00,$80;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, [r9 + rax - 128];{$ELSE}db $C4,$C1,$7D,$58,$44,$01,$80;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm1, [r8 + rax - 96];{$ELSE}db $C4,$C1,$7D,$28,$4C,$00,$A0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm1, ymm1, [r9 + rax - 96];{$ELSE}db $C4,$C1,$75,$58,$4C,$01,$A0;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm2, [r8 + rax - 64];{$ELSE}db $C4,$C1,$7D,$28,$54,$00,$C0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm2, ymm2, [r9 + rax - 64];{$ELSE}db $C4,$C1,$6D,$58,$54,$01,$C0;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm3, [r8 + rax - 32];{$ELSE}db $C4,$C1,$7D,$28,$5C,$00,$E0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm3, ymm3, [r9 + rax - 32];{$ELSE}db $C4,$C1,$65,$58,$5C,$01,$E0;{$ENDIF} 

           {$IFDEF FPC}vmovapd [rcx + rax - 128], ymm0;{$ELSE}db $C5,$FD,$29,$44,$01,$80;{$ENDIF} 
           {$IFDEF FPC}vmovapd [rcx + rax - 96], ymm1;{$ELSE}db $C5,$FD,$29,$4C,$01,$A0;{$ENDIF} 
           {$IFDEF FPC}vmovapd [rcx + rax - 64], ymm2;{$ELSE}db $C5,$FD,$29,$54,$01,$C0;{$ENDIF} 
           {$IFDEF FPC}vmovapd [rcx + rax - 32], ymm3;{$ELSE}db $C5,$FD,$29,$5C,$01,$E0;{$ENDIF} 
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           add rax, 16;
           jg @@loopEnd2;

           {$IFDEF FPC}vmovapd xmm0, [r8 + rax - 16];{$ELSE}db $C4,$C1,$79,$28,$44,$00,$F0;{$ENDIF} 
           {$IFDEF FPC}vaddpd xmm0, xmm0, [r9 + rax - 16];{$ELSE}db $C4,$C1,$79,$58,$44,$01,$F0;{$ENDIF} 

           {$IFDEF FPC}vmovapd [rcx + rax - 16], xmm0;{$ELSE}db $C5,$F9,$29,$44,$01,$F0;{$ENDIF} 
       jmp @addforxloop2;

       @@loopEnd2:

       sub rax, 16;
       jz @nextLine;

       {$IFDEF FPC}vmovsd xmm0, [r8 - 8];{$ELSE}db $C4,$C1,$7B,$10,$40,$F8;{$ENDIF} 
       {$IFDEF FPC}vmovsd xmm1, [r9 - 8];{$ELSE}db $C4,$C1,$7B,$10,$49,$F8;{$ENDIF} 
       {$IFDEF FPC}vaddsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$58,$C1;{$ENDIF} 
       {$IFDEF FPC}vmovsd [rcx - 8], xmm0;{$ELSE}db $C5,$FB,$11,$41,$F8;{$ENDIF} 

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
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;
{$IFDEF FPC}
end;
{$ENDIF}

procedure AVXMatrixAddUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var iRBX, iR12 : TASMNativeInt;
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

           // addition:
           {$IFDEF FPC}vmovupd ymm0, [r8 + rax - 128];{$ELSE}db $C4,$C1,$7D,$10,$44,$00,$80;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm1, [r9 + rax - 128];{$ELSE}db $C4,$C1,$7D,$10,$4C,$01,$80;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 
           {$IFDEF FPC}vmovupd [rcx + rax - 128], ymm0;{$ELSE}db $C5,$FD,$11,$44,$01,$80;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm0, [r8 + rax - 96];{$ELSE}db $C4,$C1,$7D,$10,$44,$00,$A0;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm1, [r9 + rax - 96];{$ELSE}db $C4,$C1,$7D,$10,$4C,$01,$A0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 
           {$IFDEF FPC}vmovupd [rcx + rax - 96], ymm0;{$ELSE}db $C5,$FD,$11,$44,$01,$A0;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm0, [r8 + rax - 64];{$ELSE}db $C4,$C1,$7D,$10,$44,$00,$C0;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm1, [r9 + rax - 64];{$ELSE}db $C4,$C1,$7D,$10,$4C,$01,$C0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 
           {$IFDEF FPC}vmovupd [rcx + rax - 64], ymm0;{$ELSE}db $C5,$FD,$11,$44,$01,$C0;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm0, [r8 + rax - 32];{$ELSE}db $C4,$C1,$7D,$10,$44,$00,$E0;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm1, [r9 + rax - 32];{$ELSE}db $C4,$C1,$7D,$10,$4C,$01,$E0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 
           {$IFDEF FPC}vmovupd [rcx + rax - 32], ymm0;{$ELSE}db $C5,$FD,$11,$44,$01,$E0;{$ENDIF} 

       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           add rax, 16;
           jg @@loopEnd2;

           {$IFDEF FPC}vmovupd xmm0, [r8 + rax - 16];{$ELSE}db $C4,$C1,$79,$10,$44,$00,$F0;{$ENDIF} 
           {$IFDEF FPC}vmovupd xmm1, [r9 + rax - 16];{$ELSE}db $C4,$C1,$79,$10,$4C,$01,$F0;{$ENDIF} 
           {$IFDEF FPC}vaddpd xmm0, xmm0, xmm1;{$ELSE}db $C5,$F9,$58,$C1;{$ENDIF} 

           {$IFDEF FPC}vmovupd [rcx + rax - 16], xmm0;{$ELSE}db $C5,$F9,$11,$44,$01,$F0;{$ENDIF} 
       jmp @addforxloop2;

       @@loopEnd2:

       sub rax, 16;
       jz @nextLine;

       {$IFDEF FPC}vmovsd xmm0, [r8 - 8];{$ELSE}db $C4,$C1,$7B,$10,$40,$F8;{$ENDIF} 
       {$IFDEF FPC}vmovsd xmm1, [r9 - 8];{$ELSE}db $C4,$C1,$7B,$10,$49,$F8;{$ENDIF} 
       {$IFDEF FPC}vaddsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$58,$C1;{$ENDIF} 
       {$IFDEF FPC}vmovsd [rcx - 8], xmm0;{$ELSE}db $C5,$FB,$11,$41,$F8;{$ENDIF} 

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
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;
{$IFDEF FPC}
end;
{$ENDIF}

procedure AVXMatrixSubAligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var iRBX, iR12 : TASMNativeInt;
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
           {$IFDEF FPC}vmovapd ymm0, [r8 + rax - 128];{$ELSE}db $C4,$C1,$7D,$28,$44,$00,$80;{$ENDIF} 
           {$IFDEF FPC}vsubpd ymm0, ymm0, [r9 + rax - 128];{$ELSE}db $C4,$C1,$7D,$5C,$44,$01,$80;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm1, [r8 + rax - 96];{$ELSE}db $C4,$C1,$7D,$28,$4C,$00,$A0;{$ENDIF} 
           {$IFDEF FPC}vsubpd ymm1, ymm1, [r9 + rax - 96];{$ELSE}db $C4,$C1,$75,$5C,$4C,$01,$A0;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm2, [r8 + rax - 64];{$ELSE}db $C4,$C1,$7D,$28,$54,$00,$C0;{$ENDIF} 
           {$IFDEF FPC}vsubpd ymm2, ymm2, [r9 + rax - 64];{$ELSE}db $C4,$C1,$6D,$5C,$54,$01,$C0;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm3, [r8 + rax - 32];{$ELSE}db $C4,$C1,$7D,$28,$5C,$00,$E0;{$ENDIF} 
           {$IFDEF FPC}vsubpd ymm3, ymm3, [r9 + rax - 32];{$ELSE}db $C4,$C1,$65,$5C,$5C,$01,$E0;{$ENDIF} 

           {$IFDEF FPC}vmovapd [rcx + rax - 128], ymm0;{$ELSE}db $C5,$FD,$29,$44,$01,$80;{$ENDIF} 
           {$IFDEF FPC}vmovapd [rcx + rax - 96], ymm1;{$ELSE}db $C5,$FD,$29,$4C,$01,$A0;{$ENDIF} 
           {$IFDEF FPC}vmovapd [rcx + rax - 64], ymm2;{$ELSE}db $C5,$FD,$29,$54,$01,$C0;{$ENDIF} 
           {$IFDEF FPC}vmovapd [rcx + rax - 32], ymm3;{$ELSE}db $C5,$FD,$29,$5C,$01,$E0;{$ENDIF} 
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           add rax, 16;
           jg @@loopEnd2;

           {$IFDEF FPC}vmovapd xmm0, [r8 + rax - 16];{$ELSE}db $C4,$C1,$79,$28,$44,$00,$F0;{$ENDIF} 
           {$IFDEF FPC}vsubpd xmm0, xmm0, [r9 + rax - 16];{$ELSE}db $C4,$C1,$79,$5C,$44,$01,$F0;{$ENDIF} 

           {$IFDEF FPC}vmovapd [rcx + rax - 16], xmm0;{$ELSE}db $C5,$F9,$29,$44,$01,$F0;{$ENDIF} 
       jmp @addforxloop2;

       @@loopEnd2:

       sub rax, 16;
       jz @nextLine;

       {$IFDEF FPC}vmovsd xmm0, [r8 - 8];{$ELSE}db $C4,$C1,$7B,$10,$40,$F8;{$ENDIF} 
       {$IFDEF FPC}vmovsd xmm1, [r9 - 8];{$ELSE}db $C4,$C1,$7B,$10,$49,$F8;{$ENDIF} 
       {$IFDEF FPC}vsubsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$5C,$C1;{$ENDIF} 
       {$IFDEF FPC}vmovsd [rcx - 8], xmm0;{$ELSE}db $C5,$FB,$11,$41,$F8;{$ENDIF} 

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
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;
{$IFDEF FPC}
end;
{$ENDIF}

procedure AVXMatrixSubUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var iRBX, iR12 : TASMNativeInt;
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
           {$IFDEF FPC}vmovupd ymm0, [r8 + rax - 128];{$ELSE}db $C4,$C1,$7D,$10,$44,$00,$80;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm1, [r9 + rax - 128];{$ELSE}db $C4,$C1,$7D,$10,$4C,$01,$80;{$ENDIF} 
           {$IFDEF FPC}vsubpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$5C,$C1;{$ENDIF} 
           {$IFDEF FPC}vmovupd [rcx + rax - 128], ymm0;{$ELSE}db $C5,$FD,$11,$44,$01,$80;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm0, [r8 + rax - 96];{$ELSE}db $C4,$C1,$7D,$10,$44,$00,$A0;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm1, [r9 + rax - 96];{$ELSE}db $C4,$C1,$7D,$10,$4C,$01,$A0;{$ENDIF} 
           {$IFDEF FPC}vsubpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$5C,$C1;{$ENDIF} 
           {$IFDEF FPC}vmovupd [rcx + rax - 96], ymm0;{$ELSE}db $C5,$FD,$11,$44,$01,$A0;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm0, [r8 + rax - 64];{$ELSE}db $C4,$C1,$7D,$10,$44,$00,$C0;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm1, [r9 + rax - 64];{$ELSE}db $C4,$C1,$7D,$10,$4C,$01,$C0;{$ENDIF} 
           {$IFDEF FPC}vsubpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$5C,$C1;{$ENDIF} 
           {$IFDEF FPC}vmovupd [rcx + rax - 64], ymm0;{$ELSE}db $C5,$FD,$11,$44,$01,$C0;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm0, [r8 + rax - 32];{$ELSE}db $C4,$C1,$7D,$10,$44,$00,$E0;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm1, [r9 + rax - 32];{$ELSE}db $C4,$C1,$7D,$10,$4C,$01,$E0;{$ENDIF} 
           {$IFDEF FPC}vsubpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$5C,$C1;{$ENDIF} 
           {$IFDEF FPC}vmovupd [rcx + rax - 32], ymm0;{$ELSE}db $C5,$FD,$11,$44,$01,$E0;{$ENDIF} 

       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           add rax, 16;
           jg @@loopEnd2;

           {$IFDEF FPC}vmovupd xmm0, [r8 + rax - 16];{$ELSE}db $C4,$C1,$79,$10,$44,$00,$F0;{$ENDIF} 
           {$IFDEF FPC}vmovupd xmm1, [r9 + rax - 16];{$ELSE}db $C4,$C1,$79,$10,$4C,$01,$F0;{$ENDIF} 
           {$IFDEF FPC}vsubpd xmm0, xmm0, xmm1;{$ELSE}db $C5,$F9,$5C,$C1;{$ENDIF} 

           {$IFDEF FPC}vmovupd [rcx + rax - 16], xmm0;{$ELSE}db $C5,$F9,$11,$44,$01,$F0;{$ENDIF} 
       jmp @addforxloop2;

       @@loopEnd2:

       sub rax, 16;
       jz @nextLine;

       {$IFDEF FPC}vmovsd xmm0, [r8 - 8];{$ELSE}db $C4,$C1,$7B,$10,$40,$F8;{$ENDIF} 
       {$IFDEF FPC}vmovsd xmm1, [r9 - 8];{$ELSE}db $C4,$C1,$7B,$10,$49,$F8;{$ENDIF} 
       {$IFDEF FPC}vsubsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$5C,$C1;{$ENDIF} 
       {$IFDEF FPC}vmovsd [rcx - 8], xmm0;{$ELSE}db $C5,$FB,$11,$41,$F8;{$ENDIF} 

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
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;
{$IFDEF FPC}
end;
{$ENDIF}

procedure AVXMatrixSubT(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; LineWidthB : TASMNativeInt; width, height : TASMNativeInt);
var iRBX : TASMNativeInt;
{$IFDEF FPC}
begin
{$ENDIF}
// rcx : A, rdx : LineWidthA, r8 : B, r9 : LineWidthB;
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
   // maintain stack
   mov iRBX, rbx;

   // rax: iter := -width*sizeof(double)
   mov rcx, A;
   mov rax, width;
   imul rax, -8;
   sub rcx, rax;

   // for y := 0 to height - 1
   @@foryloop:
      mov r10, r8;
      mov rbx, rax;
              // for x := 0 to width - 1
      @@forxloop:
         {$IFDEF FPC}vmovsd xmm0, [rcx + rbx];{$ELSE}db $C5,$FB,$10,$04,$19;{$ENDIF} 
         {$IFDEF FPC}vmovsd xmm1, [r10];{$ELSE}db $C4,$C1,$7B,$10,$0A;{$ENDIF} 
         {$IFDEF FPC}vsubsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$5C,$C1;{$ENDIF} 
         {$IFDEF FPC}vmovsd [rcx + rbx], xmm0;{$ELSE}db $C5,$FB,$11,$04,$19;{$ENDIF} 

         add r10, r9;
      add rbx, 8;
      jnz @@forxloop;

      add rcx, rdx;
      add r8, 8;
   dec height;
   jnz @@foryloop;

   // epilog
   mov rbx, iRBX;
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;
{$IFDEF FPC}
end;
{$ENDIF}


{$ENDIF}

end.
