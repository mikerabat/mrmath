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

{$I 'mrMath_CPU.inc'}

{$IFDEF x64}

uses MatrixConst;

procedure AVXMatrixElemMultAligned(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; {$ifdef UNIX}unixWidth{$ELSE}width{$endif} : NativeInt; {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt; const LineWidth1, LineWidth2 : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}
procedure AVXMatrixElemMultUnAligned(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; {$ifdef UNIX}unixWidth{$ELSE}width{$endif} : NativeInt; {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt; const LineWidth1, LineWidth2 : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}

procedure AVXMatrixElemDivAligned(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; {$ifdef UNIX}unixWidth{$ELSE}width{$endif} : NativeInt; {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt; const LineWidth1, LineWidth2 : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}
procedure AVXMatrixElemDivUnAligned(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; {$ifdef UNIX}unixWidth{$ELSE}width{$endif} : NativeInt; {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt; const LineWidth1, LineWidth2 : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}

{$ENDIF}

implementation

{$IFDEF x64}

procedure AVXMatrixElemMultAligned(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; {$ifdef UNIX}unixWidth{$ELSE}width{$endif} : NativeInt; {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt; const LineWidth1, LineWidth2 : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}
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

           // mult:
           {$IFDEF FPC}vmovapd ymm0, [r8 + rax - 128];{$ELSE}db $C4,$C1,$7D,$28,$44,$00,$80;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm0, ymm0, [r9 + rax - 128];{$ELSE}db $C4,$C1,$7D,$59,$44,$01,$80;{$ENDIF} 

           {$IFDEF FPC}vmovapd [rcx + rax - 128], ymm0;{$ELSE}db $C5,$FD,$29,$44,$01,$80;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm1, [r8 + rax - 96];{$ELSE}db $C4,$C1,$7D,$28,$4C,$00,$A0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm1, ymm1, [r9 + rax - 96];{$ELSE}db $C4,$C1,$75,$59,$4C,$01,$A0;{$ENDIF} 

           {$IFDEF FPC}vmovapd [rcx + rax - 96], ymm1;{$ELSE}db $C5,$FD,$29,$4C,$01,$A0;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm2, [r8 + rax - 64];{$ELSE}db $C4,$C1,$7D,$28,$54,$00,$C0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm2, ymm2, [r9 + rax - 64];{$ELSE}db $C4,$C1,$6D,$59,$54,$01,$C0;{$ENDIF} 

           {$IFDEF FPC}vmovapd [rcx + rax - 64], ymm2;{$ELSE}db $C5,$FD,$29,$54,$01,$C0;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm3, [r8 + rax - 32];{$ELSE}db $C4,$C1,$7D,$28,$5C,$00,$E0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm3, ymm3, [r9 + rax - 32];{$ELSE}db $C4,$C1,$65,$59,$5C,$01,$E0;{$ENDIF} 

           {$IFDEF FPC}vmovapd [rcx + rax - 32], ymm3;{$ELSE}db $C5,$FD,$29,$5C,$01,$E0;{$ENDIF} 
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           add rax, 16;
           jg @@lastElem;

           {$IFDEF FPC}vmovapd xmm0, [r8 + rax - 16];{$ELSE}db $C4,$C1,$79,$28,$44,$00,$F0;{$ENDIF} 
           {$IFDEF FPC}vmulpd xmm0, xmm0, [r9 + rax - 16];{$ELSE}db $C4,$C1,$79,$59,$44,$01,$F0;{$ENDIF} 

           {$IFDEF FPC}vmovapd [rcx + rax - 16], xmm0;{$ELSE}db $C5,$F9,$29,$44,$01,$F0;{$ENDIF} 
       jmp @addforxloop2;

       @@lastElem:
       sub rax, 16;

       jz @nextLine;

       {$IFDEF FPC}vmovsd xmm0, [r8 + rax];{$ELSE}db $C4,$C1,$7B,$10,$04,$00;{$ENDIF} 
       {$IFDEF FPC}vmulsd xmm0, xmm0, [r9 + rax];{$ELSE}db $C4,$C1,$7B,$59,$04,$01;{$ENDIF} 
       {$IFDEF FPC}vmovsd [rcx + rax], xmm0;{$ELSE}db $C5,$FB,$11,$04,$01;{$ENDIF} 

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

procedure AVXMatrixElemMultUnAligned(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; {$ifdef UNIX}unixWidth{$ELSE}width{$endif} : NativeInt; {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt; const LineWidth1, LineWidth2 : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}
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

           // mult:
           {$IFDEF FPC}vmovupd ymm0, [r8 + rax - 128];{$ELSE}db $C4,$C1,$7D,$10,$44,$00,$80;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm1, [r9 + rax - 128];{$ELSE}db $C4,$C1,$7D,$10,$4C,$01,$80;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$59,$C1;{$ENDIF} 

           {$IFDEF FPC}vmovupd [rcx + rax - 128], ymm0;{$ELSE}db $C5,$FD,$11,$44,$01,$80;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm2, [r8 + rax - 96];{$ELSE}db $C4,$C1,$7D,$10,$54,$00,$A0;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm3, [r9 + rax - 96];{$ELSE}db $C4,$C1,$7D,$10,$5C,$01,$A0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm2, ymm2, ymm3;{$ELSE}db $C5,$ED,$59,$D3;{$ENDIF} 

           {$IFDEF FPC}vmovupd [rcx + rax - 96], ymm2;{$ELSE}db $C5,$FD,$11,$54,$01,$A0;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm0, [r8 + rax - 64];{$ELSE}db $C4,$C1,$7D,$10,$44,$00,$C0;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm1, [r9 + rax - 64];{$ELSE}db $C4,$C1,$7D,$10,$4C,$01,$C0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$59,$C1;{$ENDIF} 

           {$IFDEF FPC}vmovupd [rcx + rax - 64], ymm0;{$ELSE}db $C5,$FD,$11,$44,$01,$C0;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm2, [r8 + rax - 32];{$ELSE}db $C4,$C1,$7D,$10,$54,$00,$E0;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm3, [r9 + rax - 32];{$ELSE}db $C4,$C1,$7D,$10,$5C,$01,$E0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm2, ymm2, ymm3;{$ELSE}db $C5,$ED,$59,$D3;{$ENDIF} 

           {$IFDEF FPC}vmovupd [rcx + rax - 32], ymm2;{$ELSE}db $C5,$FD,$11,$54,$01,$E0;{$ENDIF} 
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           add rax, 16;
           jg @@lastElem;

           {$IFDEF FPC}vmovupd xmm0, [r8 + rax - 16];{$ELSE}db $C4,$C1,$79,$10,$44,$00,$F0;{$ENDIF} 
           {$IFDEF FPC}vmovupd xmm1, [r9 + rax - 16];{$ELSE}db $C4,$C1,$79,$10,$4C,$01,$F0;{$ENDIF} 
           {$IFDEF FPC}vmulpd xmm0, xmm0, xmm1;{$ELSE}db $C5,$F9,$59,$C1;{$ENDIF} 

           {$IFDEF FPC}vmovupd [rcx + rax - 16], xmm0;{$ELSE}db $C5,$F9,$11,$44,$01,$F0;{$ENDIF} 
       jmp @addforxloop2;

       @@lastElem:
       sub rax, 16;

       jz @nextLine;

       {$IFDEF FPC}vmovsd xmm0, [r8 + rax];{$ELSE}db $C4,$C1,$7B,$10,$04,$00;{$ENDIF} 
       {$IFDEF FPC}vmulsd xmm0, xmm0, [r9 + rax];{$ELSE}db $C4,$C1,$7B,$59,$04,$01;{$ENDIF} 
       {$IFDEF FPC}vmovsd [rcx + rax], xmm0;{$ELSE}db $C5,$FB,$11,$04,$01;{$ENDIF} 

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

// ##############################################################
// #### Elementwise divide of matrix 1 to matrix 2
// ##############################################################

procedure AVXMatrixElemDivAligned(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; {$ifdef UNIX}unixWidth{$ELSE}width{$endif} : NativeInt; {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt; const LineWidth1, LineWidth2 : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}
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

           // mult:
           {$IFDEF FPC}vmovapd ymm0, [r8 + rax - 128];{$ELSE}db $C4,$C1,$7D,$28,$44,$00,$80;{$ENDIF} 
           {$IFDEF FPC}vdivpd ymm0, ymm0, [r9 + rax - 128];{$ELSE}db $C4,$C1,$7D,$5E,$44,$01,$80;{$ENDIF} 

           {$IFDEF FPC}vmovapd [rcx + rax - 128], ymm0;{$ELSE}db $C5,$FD,$29,$44,$01,$80;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm1, [r8 + rax - 96];{$ELSE}db $C4,$C1,$7D,$28,$4C,$00,$A0;{$ENDIF} 
           {$IFDEF FPC}vdivpd ymm1, ymm1, [r9 + rax - 96];{$ELSE}db $C4,$C1,$75,$5E,$4C,$01,$A0;{$ENDIF} 

           {$IFDEF FPC}vmovapd [rcx + rax - 96], ymm1;{$ELSE}db $C5,$FD,$29,$4C,$01,$A0;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm2, [r8 + rax - 64];{$ELSE}db $C4,$C1,$7D,$28,$54,$00,$C0;{$ENDIF} 
           {$IFDEF FPC}vdivpd ymm2, ymm2, [r9 + rax - 64];{$ELSE}db $C4,$C1,$6D,$5E,$54,$01,$C0;{$ENDIF} 

           {$IFDEF FPC}vmovapd [rcx + rax - 64], ymm2;{$ELSE}db $C5,$FD,$29,$54,$01,$C0;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm3, [r8 + rax - 32];{$ELSE}db $C4,$C1,$7D,$28,$5C,$00,$E0;{$ENDIF} 
           {$IFDEF FPC}vdivpd ymm3, ymm3, [r9 + rax - 32];{$ELSE}db $C4,$C1,$65,$5E,$5C,$01,$E0;{$ENDIF} 

           {$IFDEF FPC}vmovapd [rcx + rax - 32], ymm3;{$ELSE}db $C5,$FD,$29,$5C,$01,$E0;{$ENDIF} 
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           add rax, 16;
           jg @@lastElem;

           {$IFDEF FPC}vmovapd xmm0, [r8 + rax - 16];{$ELSE}db $C4,$C1,$79,$28,$44,$00,$F0;{$ENDIF} 
           {$IFDEF FPC}vdivpd xmm0, xmm0, [r9 + rax - 16];{$ELSE}db $C4,$C1,$79,$5E,$44,$01,$F0;{$ENDIF} 

           {$IFDEF FPC}vmovapd [rcx + rax - 16], xmm0;{$ELSE}db $C5,$F9,$29,$44,$01,$F0;{$ENDIF} 
       jmp @addforxloop2;

       @@lastElem:
       sub rax, 16;

       jz @nextLine;

       {$IFDEF FPC}vmovsd xmm0, [r8 + rax];{$ELSE}db $C4,$C1,$7B,$10,$04,$00;{$ENDIF} 
       {$IFDEF FPC}vdivsd xmm0, xmm0, [r9 + rax];{$ELSE}db $C4,$C1,$7B,$5E,$04,$01;{$ENDIF} 
       {$IFDEF FPC}vmovsd [rcx + rax], xmm0;{$ELSE}db $C5,$FB,$11,$04,$01;{$ENDIF} 

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

procedure AVXMatrixElemDivUnAligned(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; {$ifdef UNIX}unixWidth{$ELSE}width{$endif} : NativeInt; {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt; const LineWidth1, LineWidth2 : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}
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

           // mult:
           {$IFDEF FPC}vmovupd ymm0, [r8 + rax - 128];{$ELSE}db $C4,$C1,$7D,$10,$44,$00,$80;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm1, [r9 + rax - 128];{$ELSE}db $C4,$C1,$7D,$10,$4C,$01,$80;{$ENDIF} 
           {$IFDEF FPC}vdivpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$5E,$C1;{$ENDIF} 

           {$IFDEF FPC}vmovupd [rcx + rax - 128], ymm0;{$ELSE}db $C5,$FD,$11,$44,$01,$80;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm2, [r8 + rax - 96];{$ELSE}db $C4,$C1,$7D,$10,$54,$00,$A0;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm3, [r9 + rax - 96];{$ELSE}db $C4,$C1,$7D,$10,$5C,$01,$A0;{$ENDIF} 
           {$IFDEF FPC}vdivpd ymm2, ymm2, ymm3;{$ELSE}db $C5,$ED,$5E,$D3;{$ENDIF} 

           {$IFDEF FPC}vmovupd [rcx + rax - 96], ymm2;{$ELSE}db $C5,$FD,$11,$54,$01,$A0;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm0, [r8 + rax - 64];{$ELSE}db $C4,$C1,$7D,$10,$44,$00,$C0;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm1, [r9 + rax - 64];{$ELSE}db $C4,$C1,$7D,$10,$4C,$01,$C0;{$ENDIF} 
           {$IFDEF FPC}vdivpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$5E,$C1;{$ENDIF} 

           {$IFDEF FPC}vmovupd [rcx + rax - 64], ymm0;{$ELSE}db $C5,$FD,$11,$44,$01,$C0;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm2, [r8 + rax - 32];{$ELSE}db $C4,$C1,$7D,$10,$54,$00,$E0;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm3, [r9 + rax - 32];{$ELSE}db $C4,$C1,$7D,$10,$5C,$01,$E0;{$ENDIF} 
           {$IFDEF FPC}vdivpd ymm2, ymm2, ymm3;{$ELSE}db $C5,$ED,$5E,$D3;{$ENDIF} 

           {$IFDEF FPC}vmovupd [rcx + rax - 32], ymm2;{$ELSE}db $C5,$FD,$11,$54,$01,$E0;{$ENDIF} 
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           add rax, 16;
           jg @@lastElem;

           {$IFDEF FPC}vmovupd xmm0, [r8 + rax - 16];{$ELSE}db $C4,$C1,$79,$10,$44,$00,$F0;{$ENDIF} 
           {$IFDEF FPC}vmovupd xmm1, [r9 + rax - 16];{$ELSE}db $C4,$C1,$79,$10,$4C,$01,$F0;{$ENDIF} 
           {$IFDEF FPC}vdivpd xmm0, xmm0, xmm1;{$ELSE}db $C5,$F9,$5E,$C1;{$ENDIF} 

           {$IFDEF FPC}vmovupd [rcx + rax - 16], xmm0;{$ELSE}db $C5,$F9,$11,$44,$01,$F0;{$ENDIF} 
       jmp @addforxloop2;

       @@lastElem:
       sub rax, 16;

       jz @nextLine;

       {$IFDEF FPC}vmovsd xmm0, [r8 + rax];{$ELSE}db $C4,$C1,$7B,$10,$04,$00;{$ENDIF} 
       {$IFDEF FPC}vdivsd xmm0, xmm0, [r9 + rax];{$ELSE}db $C4,$C1,$7B,$5E,$04,$01;{$ENDIF} 
       {$IFDEF FPC}vmovsd [rcx + rax], xmm0;{$ELSE}db $C5,$FB,$11,$04,$01;{$ENDIF} 

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

{$ENDIF}

end.
