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

{$I 'mrMath_CPU.inc'}

{$IFDEF x64}

procedure AVXMatrixAddAligned(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; {$ifdef UNIX}unixWidth{$ELSE}width{$endif} : NativeInt; {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt; const LineWidth1, LineWidth2 : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}
procedure AVXMatrixAddUnAligned(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; {$ifdef UNIX}unixWidth{$ELSE}width{$endif} : NativeInt; {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt; const LineWidth1, LineWidth2 : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}

procedure AVXMatrixSubAligned(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; {$ifdef UNIX}unixWidth{$ELSE}width{$endif} : NativeInt; {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt; const LineWidth1, LineWidth2 : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}
procedure AVXMatrixSubUnAligned(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; {$ifdef UNIX}unixWidth{$ELSE}width{$endif} : NativeInt; {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt; const LineWidth1, LineWidth2 : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}

procedure AVXMatrixSubT(A : PDouble; LineWidthA : NativeInt; B : PDouble; LineWidthB : NativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}

procedure AVXMatrixSubVecAlignedVecRow(A : PDouble; LineWidthA : NativeInt; B : PDouble; incX : NativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}
procedure AVXMatrixSubVecAlignedRow(A : PDouble; LineWidthA : NativeInt; B : PDouble; incX : NativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}
procedure AVXMatrixSubVecAlignedCol(A : PDouble; LineWidthA : NativeInt; B : PDouble; incX : NativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}

procedure AVXMatrixSubVecUnalignedVecRow(A : PDouble; LineWidthA : NativeInt; B : PDouble; incX : NativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}
procedure AVXMatrixSubVecUnalignedRow(A : PDouble; LineWidthA : NativeInt; B : PDouble; incX : NativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}
procedure AVXMatrixSubVecUnalignedCol(A : PDouble; LineWidthA : NativeInt; B : PDouble; incX : NativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}

procedure AVXMatrixAddVecAlignedVecRow(A : PDouble; LineWidthA : NativeInt; B : PDouble; incX : NativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}
procedure AVXMatrixAddVecAlignedRow(A : PDouble; LineWidthA : NativeInt; B : PDouble; incX : NativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}
procedure AVXMatrixAddVecAlignedCol(A : PDouble; LineWidthA : NativeInt; B : PDouble; incX : NativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}

procedure AVXMatrixAddVecUnalignedVecRow(A : PDouble; LineWidthA : NativeInt; B : PDouble; incX : NativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}
procedure AVXMatrixAddVecUnalignedRow(A : PDouble; LineWidthA : NativeInt; B : PDouble; incX : NativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}
procedure AVXMatrixAddVecUnalignedCol(A : PDouble; LineWidthA : NativeInt; B : PDouble; incX : NativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}

{$ENDIF}

implementation

{$IFDEF x64}

procedure AVXMatrixAddAligned(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; {$ifdef UNIX}unixWidth{$ELSE}width{$endif} : NativeInt; {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt; const LineWidth1, LineWidth2 : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}
var iRBX, iR12 : NativeInt;
    {$ifdef UNIX}
    width : NativeInt;
    height : NativeInt;
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
           {$IFDEF AVXSUP}vmovapd ymm0, [r8 + rax - 128];             {$ELSE}db $C4,$C1,$7D,$28,$44,$00,$80;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, [r9 + rax - 128];        {$ELSE}db $C4,$C1,$7D,$58,$44,$01,$80;{$ENDIF} 

           {$IFDEF AVXSUP}vmovapd ymm1, [r8 + rax - 96];              {$ELSE}db $C4,$C1,$7D,$28,$4C,$00,$A0;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm1, ymm1, [r9 + rax - 96];         {$ELSE}db $C4,$C1,$75,$58,$4C,$01,$A0;{$ENDIF} 

           {$IFDEF AVXSUP}vmovapd ymm2, [r8 + rax - 64];              {$ELSE}db $C4,$C1,$7D,$28,$54,$00,$C0;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm2, ymm2, [r9 + rax - 64];         {$ELSE}db $C4,$C1,$6D,$58,$54,$01,$C0;{$ENDIF} 

           {$IFDEF AVXSUP}vmovapd ymm3, [r8 + rax - 32];              {$ELSE}db $C4,$C1,$7D,$28,$5C,$00,$E0;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm3, ymm3, [r9 + rax - 32];         {$ELSE}db $C4,$C1,$65,$58,$5C,$01,$E0;{$ENDIF} 

           {$IFDEF AVXSUP}vmovapd [rcx + rax - 128], ymm0;            {$ELSE}db $C5,$FD,$29,$44,$01,$80;{$ENDIF} 
           {$IFDEF AVXSUP}vmovapd [rcx + rax - 96], ymm1;             {$ELSE}db $C5,$FD,$29,$4C,$01,$A0;{$ENDIF} 
           {$IFDEF AVXSUP}vmovapd [rcx + rax - 64], ymm2;             {$ELSE}db $C5,$FD,$29,$54,$01,$C0;{$ENDIF} 
           {$IFDEF AVXSUP}vmovapd [rcx + rax - 32], ymm3;             {$ELSE}db $C5,$FD,$29,$5C,$01,$E0;{$ENDIF} 
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           add rax, 16;
           jg @@loopEnd2;

           {$IFDEF AVXSUP}vmovapd xmm0, [r8 + rax - 16];              {$ELSE}db $C4,$C1,$79,$28,$44,$00,$F0;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd xmm0, xmm0, [r9 + rax - 16];         {$ELSE}db $C4,$C1,$79,$58,$44,$01,$F0;{$ENDIF} 

           {$IFDEF AVXSUP}vmovapd [rcx + rax - 16], xmm0;             {$ELSE}db $C5,$F9,$29,$44,$01,$F0;{$ENDIF} 
       jmp @addforxloop2;

       @@loopEnd2:

       sub rax, 16;
       jz @nextLine;

       {$IFDEF AVXSUP}vmovsd xmm0, [r8 - 8];                          {$ELSE}db $C4,$C1,$7B,$10,$40,$F8;{$ENDIF} 
       {$IFDEF AVXSUP}vmovsd xmm1, [r9 - 8];                          {$ELSE}db $C4,$C1,$7B,$10,$49,$F8;{$ENDIF} 
       {$IFDEF AVXSUP}vaddsd xmm0, xmm0, xmm1;                        {$ELSE}db $C5,$FB,$58,$C1;{$ENDIF} 
       {$IFDEF AVXSUP}vmovsd [rcx - 8], xmm0;                         {$ELSE}db $C5,$FB,$11,$41,$F8;{$ENDIF} 

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
   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;

procedure AVXMatrixAddUnAligned(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; {$ifdef UNIX}unixWidth{$ELSE}width{$endif} : NativeInt; {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt; const LineWidth1, LineWidth2 : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}
var iRBX, iR12 : NativeInt;
    {$ifdef UNIX}
    width : NativeInt;
    height : NativeInt;
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
           {$IFDEF AVXSUP}vmovupd ymm0, [r8 + rax - 128];             {$ELSE}db $C4,$C1,$7D,$10,$44,$00,$80;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd ymm1, [r9 + rax - 128];             {$ELSE}db $C4,$C1,$7D,$10,$4C,$01,$80;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm1;                    {$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd [rcx + rax - 128], ymm0;            {$ELSE}db $C5,$FD,$11,$44,$01,$80;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd ymm0, [r8 + rax - 96];              {$ELSE}db $C4,$C1,$7D,$10,$44,$00,$A0;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd ymm1, [r9 + rax - 96];              {$ELSE}db $C4,$C1,$7D,$10,$4C,$01,$A0;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm1;                    {$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd [rcx + rax - 96], ymm0;             {$ELSE}db $C5,$FD,$11,$44,$01,$A0;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd ymm0, [r8 + rax - 64];              {$ELSE}db $C4,$C1,$7D,$10,$44,$00,$C0;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd ymm1, [r9 + rax - 64];              {$ELSE}db $C4,$C1,$7D,$10,$4C,$01,$C0;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm1;                    {$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd [rcx + rax - 64], ymm0;             {$ELSE}db $C5,$FD,$11,$44,$01,$C0;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd ymm0, [r8 + rax - 32];              {$ELSE}db $C4,$C1,$7D,$10,$44,$00,$E0;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd ymm1, [r9 + rax - 32];              {$ELSE}db $C4,$C1,$7D,$10,$4C,$01,$E0;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm1;                    {$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd [rcx + rax - 32], ymm0;             {$ELSE}db $C5,$FD,$11,$44,$01,$E0;{$ENDIF} 

       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           add rax, 16;
           jg @@loopEnd2;

           {$IFDEF AVXSUP}vmovupd xmm0, [r8 + rax - 16];              {$ELSE}db $C4,$C1,$79,$10,$44,$00,$F0;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd xmm1, [r9 + rax - 16];              {$ELSE}db $C4,$C1,$79,$10,$4C,$01,$F0;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd xmm0, xmm0, xmm1;                    {$ELSE}db $C5,$F9,$58,$C1;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd [rcx + rax - 16], xmm0;             {$ELSE}db $C5,$F9,$11,$44,$01,$F0;{$ENDIF} 
       jmp @addforxloop2;

       @@loopEnd2:

       sub rax, 16;
       jz @nextLine;

       {$IFDEF AVXSUP}vmovsd xmm0, [r8 - 8];                          {$ELSE}db $C4,$C1,$7B,$10,$40,$F8;{$ENDIF} 
       {$IFDEF AVXSUP}vmovsd xmm1, [r9 - 8];                          {$ELSE}db $C4,$C1,$7B,$10,$49,$F8;{$ENDIF} 
       {$IFDEF AVXSUP}vaddsd xmm0, xmm0, xmm1;                        {$ELSE}db $C5,$FB,$58,$C1;{$ENDIF} 
       {$IFDEF AVXSUP}vmovsd [rcx - 8], xmm0;                         {$ELSE}db $C5,$FB,$11,$41,$F8;{$ENDIF} 

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
   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;

procedure AVXMatrixSubAligned(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; {$ifdef UNIX}unixWidth{$ELSE}width{$endif} : NativeInt; {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt; const LineWidth1, LineWidth2 : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}
var iRBX, iR12 : NativeInt;
    {$ifdef UNIX}
    width : NativeInt;
    height : NativeInt;
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
           {$IFDEF AVXSUP}vmovapd ymm0, [r8 + rax - 128];             {$ELSE}db $C4,$C1,$7D,$28,$44,$00,$80;{$ENDIF} 
           {$IFDEF AVXSUP}vsubpd ymm0, ymm0, [r9 + rax - 128];        {$ELSE}db $C4,$C1,$7D,$5C,$44,$01,$80;{$ENDIF} 

           {$IFDEF AVXSUP}vmovapd ymm1, [r8 + rax - 96];              {$ELSE}db $C4,$C1,$7D,$28,$4C,$00,$A0;{$ENDIF} 
           {$IFDEF AVXSUP}vsubpd ymm1, ymm1, [r9 + rax - 96];         {$ELSE}db $C4,$C1,$75,$5C,$4C,$01,$A0;{$ENDIF} 

           {$IFDEF AVXSUP}vmovapd ymm2, [r8 + rax - 64];              {$ELSE}db $C4,$C1,$7D,$28,$54,$00,$C0;{$ENDIF} 
           {$IFDEF AVXSUP}vsubpd ymm2, ymm2, [r9 + rax - 64];         {$ELSE}db $C4,$C1,$6D,$5C,$54,$01,$C0;{$ENDIF} 

           {$IFDEF AVXSUP}vmovapd ymm3, [r8 + rax - 32];              {$ELSE}db $C4,$C1,$7D,$28,$5C,$00,$E0;{$ENDIF} 
           {$IFDEF AVXSUP}vsubpd ymm3, ymm3, [r9 + rax - 32];         {$ELSE}db $C4,$C1,$65,$5C,$5C,$01,$E0;{$ENDIF} 

           {$IFDEF AVXSUP}vmovapd [rcx + rax - 128], ymm0;            {$ELSE}db $C5,$FD,$29,$44,$01,$80;{$ENDIF} 
           {$IFDEF AVXSUP}vmovapd [rcx + rax - 96], ymm1;             {$ELSE}db $C5,$FD,$29,$4C,$01,$A0;{$ENDIF} 
           {$IFDEF AVXSUP}vmovapd [rcx + rax - 64], ymm2;             {$ELSE}db $C5,$FD,$29,$54,$01,$C0;{$ENDIF} 
           {$IFDEF AVXSUP}vmovapd [rcx + rax - 32], ymm3;             {$ELSE}db $C5,$FD,$29,$5C,$01,$E0;{$ENDIF} 
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           add rax, 16;
           jg @@loopEnd2;

           {$IFDEF AVXSUP}vmovapd xmm0, [r8 + rax - 16];              {$ELSE}db $C4,$C1,$79,$28,$44,$00,$F0;{$ENDIF} 
           {$IFDEF AVXSUP}vsubpd xmm0, xmm0, [r9 + rax - 16];         {$ELSE}db $C4,$C1,$79,$5C,$44,$01,$F0;{$ENDIF} 

           {$IFDEF AVXSUP}vmovapd [rcx + rax - 16], xmm0;             {$ELSE}db $C5,$F9,$29,$44,$01,$F0;{$ENDIF} 
       jmp @addforxloop2;

       @@loopEnd2:

       sub rax, 16;
       jz @nextLine;

       {$IFDEF AVXSUP}vmovsd xmm0, [r8 - 8];                          {$ELSE}db $C4,$C1,$7B,$10,$40,$F8;{$ENDIF} 
       {$IFDEF AVXSUP}vmovsd xmm1, [r9 - 8];                          {$ELSE}db $C4,$C1,$7B,$10,$49,$F8;{$ENDIF} 
       {$IFDEF AVXSUP}vsubsd xmm0, xmm0, xmm1;                        {$ELSE}db $C5,$FB,$5C,$C1;{$ENDIF} 
       {$IFDEF AVXSUP}vmovsd [rcx - 8], xmm0;                         {$ELSE}db $C5,$FB,$11,$41,$F8;{$ENDIF} 

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
   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;

procedure AVXMatrixSubUnAligned(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; {$ifdef UNIX}unixWidth{$ELSE}width{$endif} : NativeInt; {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt; const LineWidth1, LineWidth2 : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}
var iRBX, iR12 : NativeInt;
    {$ifdef UNIX}
    width : NativeInt;
    height : NativeInt;
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
           {$IFDEF AVXSUP}vmovupd ymm0, [r8 + rax - 128];             {$ELSE}db $C4,$C1,$7D,$10,$44,$00,$80;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd ymm1, [r9 + rax - 128];             {$ELSE}db $C4,$C1,$7D,$10,$4C,$01,$80;{$ENDIF} 
           {$IFDEF AVXSUP}vsubpd ymm0, ymm0, ymm1;                    {$ELSE}db $C5,$FD,$5C,$C1;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd [rcx + rax - 128], ymm0;            {$ELSE}db $C5,$FD,$11,$44,$01,$80;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd ymm0, [r8 + rax - 96];              {$ELSE}db $C4,$C1,$7D,$10,$44,$00,$A0;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd ymm1, [r9 + rax - 96];              {$ELSE}db $C4,$C1,$7D,$10,$4C,$01,$A0;{$ENDIF} 
           {$IFDEF AVXSUP}vsubpd ymm0, ymm0, ymm1;                    {$ELSE}db $C5,$FD,$5C,$C1;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd [rcx + rax - 96], ymm0;             {$ELSE}db $C5,$FD,$11,$44,$01,$A0;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd ymm0, [r8 + rax - 64];              {$ELSE}db $C4,$C1,$7D,$10,$44,$00,$C0;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd ymm1, [r9 + rax - 64];              {$ELSE}db $C4,$C1,$7D,$10,$4C,$01,$C0;{$ENDIF} 
           {$IFDEF AVXSUP}vsubpd ymm0, ymm0, ymm1;                    {$ELSE}db $C5,$FD,$5C,$C1;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd [rcx + rax - 64], ymm0;             {$ELSE}db $C5,$FD,$11,$44,$01,$C0;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd ymm0, [r8 + rax - 32];              {$ELSE}db $C4,$C1,$7D,$10,$44,$00,$E0;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd ymm1, [r9 + rax - 32];              {$ELSE}db $C4,$C1,$7D,$10,$4C,$01,$E0;{$ENDIF} 
           {$IFDEF AVXSUP}vsubpd ymm0, ymm0, ymm1;                    {$ELSE}db $C5,$FD,$5C,$C1;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd [rcx + rax - 32], ymm0;             {$ELSE}db $C5,$FD,$11,$44,$01,$E0;{$ENDIF} 

       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           add rax, 16;
           jg @@loopEnd2;

           {$IFDEF AVXSUP}vmovupd xmm0, [r8 + rax - 16];              {$ELSE}db $C4,$C1,$79,$10,$44,$00,$F0;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd xmm1, [r9 + rax - 16];              {$ELSE}db $C4,$C1,$79,$10,$4C,$01,$F0;{$ENDIF} 
           {$IFDEF AVXSUP}vsubpd xmm0, xmm0, xmm1;                    {$ELSE}db $C5,$F9,$5C,$C1;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd [rcx + rax - 16], xmm0;             {$ELSE}db $C5,$F9,$11,$44,$01,$F0;{$ENDIF} 
       jmp @addforxloop2;

       @@loopEnd2:

       sub rax, 16;
       jz @nextLine;

       {$IFDEF AVXSUP}vmovsd xmm0, [r8 - 8];                          {$ELSE}db $C4,$C1,$7B,$10,$40,$F8;{$ENDIF} 
       {$IFDEF AVXSUP}vmovsd xmm1, [r9 - 8];                          {$ELSE}db $C4,$C1,$7B,$10,$49,$F8;{$ENDIF} 
       {$IFDEF AVXSUP}vsubsd xmm0, xmm0, xmm1;                        {$ELSE}db $C5,$FB,$5C,$C1;{$ENDIF} 
       {$IFDEF AVXSUP}vmovsd [rcx - 8], xmm0;                         {$ELSE}db $C5,$FB,$11,$41,$F8;{$ENDIF} 

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
   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;

procedure AVXMatrixSubT(A : PDouble; LineWidthA : NativeInt; B : PDouble; LineWidthB : NativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}
var iRBX : NativeInt;
    {$ifdef UNIX}
    width : NativeInt;
    height : NativeInt;
    {$ENDIF}
// rcx : A, rdx : LineWidthA, r8 : B, r9 : LineWidthB;
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
         {$IFDEF AVXSUP}vmovsd xmm0, [rcx + rbx];                     {$ELSE}db $C5,$FB,$10,$04,$19;{$ENDIF} 
         {$IFDEF AVXSUP}vmovsd xmm1, [r10];                           {$ELSE}db $C4,$C1,$7B,$10,$0A;{$ENDIF} 
         {$IFDEF AVXSUP}vsubsd xmm0, xmm0, xmm1;                      {$ELSE}db $C5,$FB,$5C,$C1;{$ENDIF} 
         {$IFDEF AVXSUP}vmovsd [rcx + rbx], xmm0;                     {$ELSE}db $C5,$FB,$11,$04,$19;{$ENDIF} 

         add r10, r9;
      add rbx, 8;
      jnz @@forxloop;

      add rcx, rdx;
      add r8, 8;
   dec height;
   jnz @@foryloop;

   // epilog
   mov rbx, iRBX;
   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;

// ########################################################
// #### Matrix add, sub to vector operations
// ########################################################

procedure AVXMatrixSubVecAlignedVecRow(A : PDouble; LineWidthA : NativeInt; B : PDouble; incX : NativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}
{$ifdef UNIX}
var width : NativeInt;
    height : NativeInt;
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
   // rcx = a, rdx = LineWidthA, r8 = B, r9 = incX
   mov r10, width;
   imul r10, -8;

   sub rcx, r10;
   sub r8, r10;

   @@foryloop:
      mov rax, r10;

      @@forxloopUnrolled:
         add rax, 64;
         jg @@EndLoop1;

         {$IFDEF AVXSUP}vmovapd ymm0, [rcx + rax - 64];               {$ELSE}db $C5,$FD,$28,$44,$01,$C0;{$ENDIF} 
         {$IFDEF AVXSUP}vsubpd ymm0, ymm0, [r8 + rax - 64];           {$ELSE}db $C4,$C1,$7D,$5C,$44,$00,$C0;{$ENDIF} 
         {$IFDEF AVXSUP}vmovapd [rcx + rax - 64], ymm0;               {$ELSE}db $C5,$FD,$29,$44,$01,$C0;{$ENDIF} 

         {$IFDEF AVXSUP}vmovapd ymm2, [rcx + rax - 32];               {$ELSE}db $C5,$FD,$28,$54,$01,$E0;{$ENDIF} 
         {$IFDEF AVXSUP}vsubpd ymm2, ymm2, [r8 + rax - 32];           {$ELSE}db $C4,$C1,$6D,$5C,$54,$00,$E0;{$ENDIF} 
         {$IFDEF AVXSUP}vmovapd [rcx + rax - 32], ymm2;               {$ELSE}db $C5,$FD,$29,$54,$01,$E0;{$ENDIF} 

      jmp @@forxloopUnrolled;

      @@EndLoop1:

      sub rax, 64;

      jz @NextLine;

      @@forxloop:
         {$IFDEF AVXSUP}vmovsd xmm0, [rcx + rax];                     {$ELSE}db $C5,$FB,$10,$04,$01;{$ENDIF} 
         {$IFDEF AVXSUP}vmovsd xmm1, [r8 + rax];                      {$ELSE}db $C4,$C1,$7B,$10,$0C,$00;{$ENDIF} 

         {$IFDEF AVXSUP}vsubsd xmm0, xmm0, xmm1;                      {$ELSE}db $C5,$FB,$5C,$C1;{$ENDIF} 
         {$IFDEF AVXSUP}vmovsd [rcx + rax], xmm0;                     {$ELSE}db $C5,$FB,$11,$04,$01;{$ENDIF} 

         add rax, 8;
      jnz @@forxloop;

      @NextLine:

      add rcx, rdx;
   dec Height;
   jnz @@foryloop;

   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;

procedure AVXMatrixSubVecAlignedRow(A : PDouble; LineWidthA : NativeInt; B : PDouble; incX : NativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}
var vecIter : NativeInt;
{$ifdef UNIX}
    width : NativeInt;
    height : NativeInt;
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
   // rcx = a, rdx = LineWidthA, r8 = B, r9 = incX
   mov r11, width;

   imul r11, r9;
   imul r11, -1;
   mov vecIter, r11;

   mov r10, width;
   imul r10, -8;

   sub rcx, r10;
   sub r8, r11;

   @@foryloop:
      mov rax, r10;
      mov r11, vecIter;

      @@forxloop:
         {$IFDEF AVXSUP}vmovsd xmm0, [rcx + rax];                     {$ELSE}db $C5,$FB,$10,$04,$01;{$ENDIF} 
         {$IFDEF AVXSUP}vmovsd xmm1, [r8 + r11];                      {$ELSE}db $C4,$81,$7B,$10,$0C,$18;{$ENDIF} 

         {$IFDEF AVXSUP}vsubsd xmm0, xmm0, xmm1;                      {$ELSE}db $C5,$FB,$5C,$C1;{$ENDIF} 
         {$IFDEF AVXSUP}vmovsd [rcx + rax], xmm0;                     {$ELSE}db $C5,$FB,$11,$04,$01;{$ENDIF} 

         add r11, r9;
         add rax, 8;
      jnz @@forxloop;

      add rcx, rdx;
   dec Height;
   jnz @@foryloop;

   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;

procedure AVXMatrixSubVecAlignedCol(A : PDouble; LineWidthA : NativeInt; B : PDouble; incX : NativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}
{$ifdef UNIX}
var width : NativeInt;
    height : NativeInt;
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
   // rcx = a, rdx = LineWidthA, r8 = B, r9 = incX
   mov r10, width;
   imul r10, -8;

   sub rcx, r10;

   @@foryloop:
      mov rax, r10;

      {$IFDEF AVXSUP}vbroadcastsd ymm1, [r8];                         {$ELSE}db $C4,$C2,$7D,$19,$08;{$ENDIF} 

      @@forxloopUnrolled:
         add rax, 64;
         jg @@EndLoop1;

         {$IFDEF AVXSUP}vmovapd ymm0, [rcx + rax - 64];               {$ELSE}db $C5,$FD,$28,$44,$01,$C0;{$ENDIF} 
         {$IFDEF AVXSUP}vsubpd ymm0, ymm0, ymm1;                      {$ELSE}db $C5,$FD,$5C,$C1;{$ENDIF} 
         {$IFDEF AVXSUP}vmovapd [rcx + rax - 64], ymm0;               {$ELSE}db $C5,$FD,$29,$44,$01,$C0;{$ENDIF} 

         {$IFDEF AVXSUP}vmovapd ymm2, [rcx + rax - 32];               {$ELSE}db $C5,$FD,$28,$54,$01,$E0;{$ENDIF} 
         {$IFDEF AVXSUP}vsubpd ymm2, ymm2, ymm1;                      {$ELSE}db $C5,$ED,$5C,$D1;{$ENDIF} 
         {$IFDEF AVXSUP}vmovapd [rcx + rax - 32], ymm2;               {$ELSE}db $C5,$FD,$29,$54,$01,$E0;{$ENDIF} 

      jmp @@forxloopUnrolled;

      @@EndLoop1:

      sub rax, 64;

      jz @NextLine;

      @@forxloop:
         {$IFDEF AVXSUP}vmovsd xmm0, [rcx + rax];                     {$ELSE}db $C5,$FB,$10,$04,$01;{$ENDIF} 

         {$IFDEF AVXSUP}vsubsd xmm0, xmm0, xmm1;                      {$ELSE}db $C5,$FB,$5C,$C1;{$ENDIF} 
         {$IFDEF AVXSUP}vmovsd [rcx + rax], xmm0;                     {$ELSE}db $C5,$FB,$11,$04,$01;{$ENDIF} 

         add rax, 8;
      jnz @@forxloop;

      @NextLine:

      add rcx, rdx;
      add r8, r9;
   dec Height;
   jnz @@foryloop;

   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;

procedure AVXMatrixSubVecUnalignedVecRow(A : PDouble; LineWidthA : NativeInt; B : PDouble; incX : NativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}
{$ifdef UNIX}
var width : NativeInt;
    height : NativeInt;
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

   // rcx = a, rdx = LineWidthA, r8 = B, r9 = incX
   mov r10, width;
   imul r10, -8;

   sub rcx, r10;
   sub r8, r10;

   @@foryloop:
      mov rax, r10;

      @@forxloopUnrolled:
         add rax, 64;
         jg @@EndLoop1;

         {$IFDEF AVXSUP}vmovupd ymm0, [rcx + rax - 64];               {$ELSE}db $C5,$FD,$10,$44,$01,$C0;{$ENDIF} 
         {$IFDEF AVXSUP}vmovupd ymm1, [r8 + rax - 64];                {$ELSE}db $C4,$C1,$7D,$10,$4C,$00,$C0;{$ENDIF} 
         {$IFDEF AVXSUP}vsubpd ymm0, ymm0, ymm1;                      {$ELSE}db $C5,$FD,$5C,$C1;{$ENDIF} 
         {$IFDEF AVXSUP}vmovupd [rcx + rax - 64], ymm0;               {$ELSE}db $C5,$FD,$11,$44,$01,$C0;{$ENDIF} 

         {$IFDEF AVXSUP}vmovupd ymm2, [rcx + rax - 32];               {$ELSE}db $C5,$FD,$10,$54,$01,$E0;{$ENDIF} 
         {$IFDEF AVXSUP}vmovupd ymm3, [r8 + rax - 32];                {$ELSE}db $C4,$C1,$7D,$10,$5C,$00,$E0;{$ENDIF} 
         {$IFDEF AVXSUP}vsubpd ymm2, ymm2, ymm3;                      {$ELSE}db $C5,$ED,$5C,$D3;{$ENDIF} 
         {$IFDEF AVXSUP}vmovupd [rcx + rax - 32], ymm2;               {$ELSE}db $C5,$FD,$11,$54,$01,$E0;{$ENDIF} 

      jmp @@forxloopUnrolled;

      @@EndLoop1:

      sub rax, 64;

      jz @NextLine;

      @@forxloop:
         {$IFDEF AVXSUP}vmovsd xmm0, [rcx + rax];                     {$ELSE}db $C5,$FB,$10,$04,$01;{$ENDIF} 
         {$IFDEF AVXSUP}vmovsd xmm1, [r8 + rax];                      {$ELSE}db $C4,$C1,$7B,$10,$0C,$00;{$ENDIF} 

         {$IFDEF AVXSUP}vsubsd xmm0, xmm0, xmm1;                      {$ELSE}db $C5,$FB,$5C,$C1;{$ENDIF} 
         {$IFDEF AVXSUP}vmovsd [rcx + rax], xmm0;                     {$ELSE}db $C5,$FB,$11,$04,$01;{$ENDIF} 

         add rax, 8;
      jnz @@forxloop;

      @NextLine:

      add rcx, rdx;
   dec Height;
   jnz @@foryloop;

   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;

procedure AVXMatrixSubVecUnalignedRow(A : PDouble; LineWidthA : NativeInt; B : PDouble; incX : NativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}
var vecIter : NativeInt;
{$ifdef UNIX}
    width : NativeInt;
    height : NativeInt;
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
   // rcx = a, rdx = LineWidthA, r8 = B, r9 = incX
   mov r11, width;

   imul r11, r9;
   imul r11, -1;
   mov vecIter, r11;

   mov r10, width;
   imul r10, -8;

   sub rcx, r10;
   sub r8, r11;

   @@foryloop:
      mov rax, r10;
      mov r11, vecIter;

      @@forxloop:
         {$IFDEF AVXSUP}vmovsd xmm0, [rcx + rax];                     {$ELSE}db $C5,$FB,$10,$04,$01;{$ENDIF} 
         {$IFDEF AVXSUP}vmovsd xmm1, [r8 + r11];                      {$ELSE}db $C4,$81,$7B,$10,$0C,$18;{$ENDIF} 

         {$IFDEF AVXSUP}vsubsd xmm0, xmm0, xmm1;                      {$ELSE}db $C5,$FB,$5C,$C1;{$ENDIF} 
         {$IFDEF AVXSUP}vmovsd [rcx + rax], xmm0;                     {$ELSE}db $C5,$FB,$11,$04,$01;{$ENDIF} 

         add r11, r9;
         add rax, 8;
      jnz @@forxloop;

      add rcx, rdx;
   dec Height;
   jnz @@foryloop;

   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;

procedure AVXMatrixSubVecUnalignedCol(A : PDouble; LineWidthA : NativeInt; B : PDouble; incX : NativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}
{$ifdef UNIX}
var width : NativeInt;
    height : NativeInt;
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

   // rcx = a, rdx = LineWidthA, r8 = B, r9 = incX
   mov r10, width;
   imul r10, -8;

   sub rcx, r10;

   @@foryloop:
      mov rax, r10;

      {$IFDEF AVXSUP}vbroadcastsd ymm1, [r8];                         {$ELSE}db $C4,$C2,$7D,$19,$08;{$ENDIF} 

      @@forxloopUnrolled:
         add rax, 64;
         jg @@EndLoop1;

         {$IFDEF AVXSUP}vmovupd ymm0, [rcx + rax - 64];               {$ELSE}db $C5,$FD,$10,$44,$01,$C0;{$ENDIF} 
         {$IFDEF AVXSUP}vsubpd ymm0, ymm0, ymm1;                      {$ELSE}db $C5,$FD,$5C,$C1;{$ENDIF} 
         {$IFDEF AVXSUP}vmovupd [rcx + rax - 64], ymm0;               {$ELSE}db $C5,$FD,$11,$44,$01,$C0;{$ENDIF} 

         {$IFDEF AVXSUP}vmovupd ymm2, [rcx + rax - 32];               {$ELSE}db $C5,$FD,$10,$54,$01,$E0;{$ENDIF} 
         {$IFDEF AVXSUP}vsubpd ymm2, ymm2, ymm1;                      {$ELSE}db $C5,$ED,$5C,$D1;{$ENDIF} 
         {$IFDEF AVXSUP}vmovupd [rcx + rax - 32], ymm2;               {$ELSE}db $C5,$FD,$11,$54,$01,$E0;{$ENDIF} 

      jmp @@forxloopUnrolled;

      @@EndLoop1:

      sub rax, 64;

      jz @NextLine;

      @@forxloop:
         {$IFDEF AVXSUP}vmovsd xmm0, [rcx + rax];                     {$ELSE}db $C5,$FB,$10,$04,$01;{$ENDIF} 

         {$IFDEF AVXSUP}vsubsd xmm0, xmm0, xmm1;                      {$ELSE}db $C5,$FB,$5C,$C1;{$ENDIF} 
         {$IFDEF AVXSUP}vmovsd [rcx + rax], xmm0;                     {$ELSE}db $C5,$FB,$11,$04,$01;{$ENDIF} 

         add rax, 8;
      jnz @@forxloop;

      @NextLine:

      add rcx, rdx;
      add r8, r9;
   dec Height;
   jnz @@foryloop;

   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;


procedure AVXMatrixAddVecAlignedVecRow(A : PDouble; LineWidthA : NativeInt; B : PDouble; incX : NativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}
{$ifdef UNIX}
var width : NativeInt;
    height : NativeInt;
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

   // rcx = a, rdx = LineWidthA, r8 = B, r9 = incX
   mov r10, width;
   imul r10, -8;

   sub rcx, r10;
   sub r8, r10;

   @@foryloop:
      mov rax, r10;

      @@forxloopUnrolled:
         add rax, 64;
         jg @@EndLoop1;

         {$IFDEF AVXSUP}vmovapd ymm0, [rcx + rax - 64];               {$ELSE}db $C5,$FD,$28,$44,$01,$C0;{$ENDIF} 
         {$IFDEF AVXSUP}vaddpd ymm0, ymm0, [r8 + rax - 64];           {$ELSE}db $C4,$C1,$7D,$58,$44,$00,$C0;{$ENDIF} 
         {$IFDEF AVXSUP}vmovapd [rcx + rax - 64], ymm0;               {$ELSE}db $C5,$FD,$29,$44,$01,$C0;{$ENDIF} 

         {$IFDEF AVXSUP}vmovapd ymm2, [rcx + rax - 32];               {$ELSE}db $C5,$FD,$28,$54,$01,$E0;{$ENDIF} 
         {$IFDEF AVXSUP}vaddpd ymm2, ymm2, [r8 + rax - 32];           {$ELSE}db $C4,$C1,$6D,$58,$54,$00,$E0;{$ENDIF} 
         {$IFDEF AVXSUP}vmovapd [rcx + rax - 32], ymm2;               {$ELSE}db $C5,$FD,$29,$54,$01,$E0;{$ENDIF} 
      jmp @@forxloopUnrolled;

      @@EndLoop1:

      sub rax, 64;

      jz @NextLine;

      @@forxloop:
         {$IFDEF AVXSUP}vmovsd xmm0, [rcx + rax];                     {$ELSE}db $C5,$FB,$10,$04,$01;{$ENDIF} 
         {$IFDEF AVXSUP}vmovsd xmm1, [r8 + rax];                      {$ELSE}db $C4,$C1,$7B,$10,$0C,$00;{$ENDIF} 

         {$IFDEF AVXSUP}vaddsd xmm0, xmm0, xmm1;                      {$ELSE}db $C5,$FB,$58,$C1;{$ENDIF} 
         {$IFDEF AVXSUP}vmovsd [rcx + rax], xmm0;                     {$ELSE}db $C5,$FB,$11,$04,$01;{$ENDIF} 

         add rax, 8;
      jnz @@forxloop;

      @NextLine:

      add rcx, rdx;
   dec Height;
   jnz @@foryloop;

   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;

procedure AVXMatrixAddVecAlignedRow(A : PDouble; LineWidthA : NativeInt; B : PDouble; incX : NativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}
var vecIter : NativeInt;
{$ifdef UNIX}
    width : NativeInt;
    height : NativeInt;
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

   // rcx = a, rdx = LineWidthA, r8 = B, r9 = incX
   mov r11, width;

   imul r11, r9;
   imul r11, -1;
   mov vecIter, r11;

   mov r10, width;
   imul r10, -8;

   sub rcx, r10;
   sub r8, r11;

   @@foryloop:
      mov rax, r10;
      mov r11, vecIter;

      @@forxloop:
         {$IFDEF AVXSUP}vmovsd xmm0, [rcx + rax];                     {$ELSE}db $C5,$FB,$10,$04,$01;{$ENDIF} 
         {$IFDEF AVXSUP}vmovsd xmm1, [r8 + r11];                      {$ELSE}db $C4,$81,$7B,$10,$0C,$18;{$ENDIF} 

         {$IFDEF AVXSUP}vaddsd xmm0, xmm0, xmm1;                      {$ELSE}db $C5,$FB,$58,$C1;{$ENDIF} 
         {$IFDEF AVXSUP}vmovsd [rcx + rax], xmm0;                     {$ELSE}db $C5,$FB,$11,$04,$01;{$ENDIF} 

         add r11, r9;
         add rax, 8;
      jnz @@forxloop;

      add rcx, rdx;
   dec Height;
   jnz @@foryloop;

   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;

procedure AVXMatrixAddVecAlignedCol(A : PDouble; LineWidthA : NativeInt; B : PDouble; incX : NativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}
{$ifdef UNIX}
var width : NativeInt;
    height : NativeInt;
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

   // rcx = a, rdx = LineWidthA, r8 = B, r9 = incX
   mov r10, width;
   imul r10, -8;

   sub rcx, r10;

   @@foryloop:
      mov rax, r10;

      {$IFDEF AVXSUP}vbroadcastsd ymm1, [r8];                         {$ELSE}db $C4,$C2,$7D,$19,$08;{$ENDIF} 

      @@forxloopUnrolled:
         add rax, 64;
         jg @@EndLoop1;

         {$IFDEF AVXSUP}vmovapd ymm0, [rcx + rax - 64];               {$ELSE}db $C5,$FD,$28,$44,$01,$C0;{$ENDIF} 
         {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm1;                      {$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 
         {$IFDEF AVXSUP}vmovapd [rcx + rax - 64], ymm0;               {$ELSE}db $C5,$FD,$29,$44,$01,$C0;{$ENDIF} 

         {$IFDEF AVXSUP}vmovapd ymm2, [rcx + rax - 32];               {$ELSE}db $C5,$FD,$28,$54,$01,$E0;{$ENDIF} 
         {$IFDEF AVXSUP}vaddpd ymm2, ymm2, ymm1;                      {$ELSE}db $C5,$ED,$58,$D1;{$ENDIF} 
         {$IFDEF AVXSUP}vmovapd [rcx + rax - 32], ymm2;               {$ELSE}db $C5,$FD,$29,$54,$01,$E0;{$ENDIF} 

      jmp @@forxloopUnrolled;

      @@EndLoop1:

      sub rax, 64;

      jz @NextLine;

      @@forxloop:
         {$IFDEF AVXSUP}vmovsd xmm0, [rcx + rax];                     {$ELSE}db $C5,$FB,$10,$04,$01;{$ENDIF} 

         {$IFDEF AVXSUP}vaddsd xmm0, xmm0, xmm1;                      {$ELSE}db $C5,$FB,$58,$C1;{$ENDIF} 
         {$IFDEF AVXSUP}vmovsd [rcx + rax], xmm0;                     {$ELSE}db $C5,$FB,$11,$04,$01;{$ENDIF} 

         add rax, 8;
      jnz @@forxloop;

      @NextLine:

      add rcx, rdx;
      add r8, r9;
   dec Height;
   jnz @@foryloop;

   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;

procedure AVXMatrixAddVecUnalignedVecRow(A : PDouble; LineWidthA : NativeInt; B : PDouble; incX : NativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}
{$ifdef UNIX}
var width : NativeInt;
    height : NativeInt;
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

   // rcx = a, rdx = LineWidthA, r8 = B, r9 = incX
   mov r10, width;
   imul r10, -8;

   sub rcx, r10;
   sub r8, r10;

   @@foryloop:
      mov rax, r10;

      @@forxloopUnrolled:
         add rax, 64;
         jg @@EndLoop1;

         {$IFDEF AVXSUP}vmovupd ymm0, [rcx + rax - 64];               {$ELSE}db $C5,$FD,$10,$44,$01,$C0;{$ENDIF} 
         {$IFDEF AVXSUP}vmovupd ymm1, [r8 + rax - 64];                {$ELSE}db $C4,$C1,$7D,$10,$4C,$00,$C0;{$ENDIF} 
         {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm1;                      {$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 
         {$IFDEF AVXSUP}vmovupd [rcx + rax - 64], ymm0;               {$ELSE}db $C5,$FD,$11,$44,$01,$C0;{$ENDIF} 

         {$IFDEF AVXSUP}vmovupd ymm2, [rcx + rax - 32];               {$ELSE}db $C5,$FD,$10,$54,$01,$E0;{$ENDIF} 
         {$IFDEF AVXSUP}vmovupd ymm3, [r8 + rax - 32];                {$ELSE}db $C4,$C1,$7D,$10,$5C,$00,$E0;{$ENDIF} 
         {$IFDEF AVXSUP}vaddpd ymm2, ymm2, ymm3;                      {$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
         {$IFDEF AVXSUP}vmovupd [rcx + rax - 32], ymm2;               {$ELSE}db $C5,$FD,$11,$54,$01,$E0;{$ENDIF} 

      jmp @@forxloopUnrolled;

      @@EndLoop1:

      sub rax, 64;

      jz @NextLine;

      @@forxloop:
         {$IFDEF AVXSUP}vmovsd xmm0, [rcx + rax];                     {$ELSE}db $C5,$FB,$10,$04,$01;{$ENDIF} 
         {$IFDEF AVXSUP}vmovsd xmm1, [r8 + rax];                      {$ELSE}db $C4,$C1,$7B,$10,$0C,$00;{$ENDIF} 

         {$IFDEF AVXSUP}vaddsd xmm0, xmm0, xmm1;                      {$ELSE}db $C5,$FB,$58,$C1;{$ENDIF} 
         {$IFDEF AVXSUP}vmovsd [rcx + rax], xmm0;                     {$ELSE}db $C5,$FB,$11,$04,$01;{$ENDIF} 

         add rax, 8;
      jnz @@forxloop;

      @NextLine:

      add rcx, rdx;
   dec Height;
   jnz @@foryloop;

   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;

procedure AVXMatrixAddVecUnalignedRow(A : PDouble; LineWidthA : NativeInt; B : PDouble; incX : NativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}
var vecIter : NativeInt;
{$ifdef UNIX}
    width : NativeInt;
    height : NativeInt;
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

   // rcx = a, rdx = LineWidthA, r8 = B, r9 = incX
   mov r11, width;

   imul r11, r9;
   imul r11, -1;
   mov vecIter, r11;

   mov r10, width;
   imul r10, -8;

   sub rcx, r10;
   sub r8, r11;

   @@foryloop:
      mov rax, r10;
      mov r11, vecIter;

      @@forxloop:
         {$IFDEF AVXSUP}vmovsd xmm0, [rcx + rax];                     {$ELSE}db $C5,$FB,$10,$04,$01;{$ENDIF} 
         {$IFDEF AVXSUP}vmovsd xmm1, [r8 + r11];                      {$ELSE}db $C4,$81,$7B,$10,$0C,$18;{$ENDIF} 

         {$IFDEF AVXSUP}vaddsd xmm0, xmm0, xmm1;                      {$ELSE}db $C5,$FB,$58,$C1;{$ENDIF} 
         {$IFDEF AVXSUP}vmovsd [rcx + rax], xmm0;                     {$ELSE}db $C5,$FB,$11,$04,$01;{$ENDIF} 

         add r11, r9;
         add rax, 8;
      jnz @@forxloop;

      add rcx, rdx;
   dec Height;
   jnz @@foryloop;

   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;

procedure AVXMatrixAddVecUnalignedCol(A : PDouble; LineWidthA : NativeInt; B : PDouble; incX : NativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}
{$ifdef UNIX}
var width : NativeInt;
    height : NativeInt;
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

   // rcx = a, rdx = LineWidthA, r8 = B, r9 = incX
   mov r10, width;
   imul r10, -8;

   sub rcx, r10;

   @@foryloop:
      mov rax, r10;

      {$IFDEF AVXSUP}vbroadcastsd ymm1, [r8];                         {$ELSE}db $C4,$C2,$7D,$19,$08;{$ENDIF} 

      @@forxloopUnrolled:
         add rax, 64;
         jg @@EndLoop1;

         {$IFDEF AVXSUP}vmovupd ymm0, [rcx + rax - 64];               {$ELSE}db $C5,$FD,$10,$44,$01,$C0;{$ENDIF} 
         {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm1;                      {$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 
         {$IFDEF AVXSUP}vmovupd [rcx + rax - 64], ymm0;               {$ELSE}db $C5,$FD,$11,$44,$01,$C0;{$ENDIF} 

         {$IFDEF AVXSUP}vmovupd ymm2, [rcx + rax - 32];               {$ELSE}db $C5,$FD,$10,$54,$01,$E0;{$ENDIF} 
         {$IFDEF AVXSUP}vaddpd ymm2, ymm2, ymm1;                      {$ELSE}db $C5,$ED,$58,$D1;{$ENDIF} 
         {$IFDEF AVXSUP}vmovupd [rcx + rax - 32], ymm2;               {$ELSE}db $C5,$FD,$11,$54,$01,$E0;{$ENDIF} 

      jmp @@forxloopUnrolled;

      @@EndLoop1:

      sub rax, 64;

      jz @NextLine;

      @@forxloop:
         {$IFDEF AVXSUP}vmovsd xmm0, [rcx + rax];                     {$ELSE}db $C5,$FB,$10,$04,$01;{$ENDIF} 

         {$IFDEF AVXSUP}vaddsd xmm0, xmm0, xmm1;                      {$ELSE}db $C5,$FB,$58,$C1;{$ENDIF} 
         {$IFDEF AVXSUP}vmovsd [rcx + rax], xmm0;                     {$ELSE}db $C5,$FB,$11,$04,$01;{$ENDIF} 

         add rax, 8;
      jnz @@forxloop;

      @NextLine:

      add rcx, rdx;
      add r8, r9;
   dec Height;
   jnz @@foryloop;

   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;


{$ENDIF}

end.
