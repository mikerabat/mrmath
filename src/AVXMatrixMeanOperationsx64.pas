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


unit AVXMatrixMeanOperationsx64;

interface

{$I 'mrMath_CPU.inc'}

{$IFDEF x64}

uses MatrixConst;

procedure AVXMatrixMeanRowAligned(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}
procedure AVXMatrixMeanRowUnAligned(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}

procedure AVXMatrixMeanColumnAligned(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}
procedure AVXMatrixMeanColumnUnAligned(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}

procedure AVXMatrixVarRowAligned(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt; unbiased : boolean); {$IFDEF FPC}assembler;{$ENDIF}
procedure AVXMatrixVarRowUnAligned(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt; unbiased : boolean); {$IFDEF FPC}assembler;{$ENDIF}

procedure AVXMatrixVarColumnAligned(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt; unbiased : boolean); {$IFDEF FPC}assembler;{$ENDIF}
procedure AVXMatrixVarColumnUnAligned(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt; unbiased : boolean); {$IFDEF FPC}assembler;{$ENDIF}


// combined methods
procedure AVXMatrixMeanVarRowAligned(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt; unbiased : boolean); {$IFDEF FPC}assembler;{$ENDIF}
procedure AVXMatrixMeanVarRowUnAligned(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt; unbiased : boolean); {$IFDEF FPC}assembler;{$ENDIF}

procedure AVXMatrixMeanVarColumnAligned(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt; unbiased : boolean); {$IFDEF FPC}assembler;{$ENDIF}
procedure AVXMatrixMeanVarColumnUnAligned(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt; unbiased : boolean); {$IFDEF FPC}assembler;{$ENDIF}

{$ENDIF}

implementation

{$IFDEF x64}

procedure AVXMatrixMeanRowAligned(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}
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

   // note: RCX = dest, RDX = destLineWidth, R8 = src, R9 = srcLineWidth

   // prolog

   // iters := -width*sizeof(double)
   mov r10, width;
   imul r10, -8;

   // helper registers for the mt1, mt2 and dest pointers
   sub r8, r10;

   // fpc seems to have a problem with this opcode
   {$IFDEF FPC}
   mov rax, width;
   {$IFDEF AVXSUP}vcvtsi2sd xmm3, xmm3, rax;                          {$ELSE}db $C4,$E1,$E3,$2A,$D8;{$ENDIF} 
   {$ELSE}
   cvtsi2sd xmm3, width;
   {$ENDIF}

   // for y := 0 to height - 1:
   mov r11, Height;
   @@addforyloop:
       {$IFDEF AVXSUP}vxorpd ymm0, ymm0, ymm0;                        {$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} 
       {$IFDEF AVXSUP}vxorpd ymm1, ymm1, ymm1;                        {$ELSE}db $C5,$F5,$57,$C9;{$ENDIF} 

       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;
           // prefetch data...
           // prefetch [r8 + rax];

           // addition:
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, [r8 + rax - 128];        {$ELSE}db $C4,$C1,$7D,$58,$44,$00,$80;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm1, ymm1, [r8 + rax - 96];         {$ELSE}db $C4,$C1,$75,$58,$4C,$00,$A0;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, [r8 + rax - 64];         {$ELSE}db $C4,$C1,$7D,$58,$44,$00,$C0;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm1, ymm1, [r8 + rax - 32];         {$ELSE}db $C4,$C1,$75,$58,$4C,$00,$E0;{$ENDIF} 
       jmp @addforxloop

       @loopEnd:

       {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm1;                        {$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 
       {$IFDEF AVXSUP}vextractf128 xmm2, ymm0, 1;                     {$ELSE}db $C4,$E3,$7D,$19,$C2,$01;{$ENDIF} 
       {$IFDEF AVXSUP}vhaddpd xmm0, xmm0, xmm2;                       {$ELSE}db $C5,$F9,$7C,$C2;{$ENDIF} 

       sub rax, 128;

       jz @buildRes;

       @addforxloop2:
           add rax, 16;
           jg @@addforxloop2end;

           {$IFDEF AVXSUP}vaddpd xmm0, xmm0, [r8 + rax - 16];         {$ELSE}db $C4,$C1,$79,$58,$44,$00,$F0;{$ENDIF} 
       jmp @addforxloop2;

       @@addforxloop2end:

       sub rax, 16;
       jz @buildRes;

       {$IFDEF AVXSUP}vaddsd xmm0, xmm0, [r8 + rax];                  {$ELSE}db $C4,$C1,$7B,$58,$04,$00;{$ENDIF} 

       @buildRes:

       // build result
       {$IFDEF AVXSUP}vhaddpd xmm0, xmm0, xmm0;                       {$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} 

       {$IFDEF AVXSUP}vdivsd xmm0, xmm0, xmm3;                        {$ELSE}db $C5,$FB,$5E,$C3;{$ENDIF} 

       // write result
       {$IFDEF AVXSUP}vmovsd [rcx], xmm0;                             {$ELSE}db $C5,$FB,$11,$01;{$ENDIF} 

       // next line:
       add r8, r9;
       add rcx, rdx;

   // loop y end
   dec r11;
   jnz @@addforyloop;

   // epilog
   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;

procedure AVXMatrixMeanRowUnAligned(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}
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

   // note: RCX = dest, RDX = destLineWidth, R8 = src, R9 = srcLineWidth

   // prolog

   // iters := -width*sizeof(double)
   mov r10, width;
   imul r10, -8;

   // helper registers for the mt1, mt2 and dest pointers
   sub r8, r10;

   // fpc seems to have a problem with this opcode
   {$IFDEF FPC}
   mov rax, width;
   {$IFDEF AVXSUP}vcvtsi2sd xmm3, xmm3, rax;                          {$ELSE}db $C4,$E1,$E3,$2A,$D8;{$ENDIF} 
   {$ELSE}
   cvtsi2sd xmm3, width;
   {$ENDIF}

   // for y := 0 to height - 1:
   mov r11, Height;
   @@addforyloop:
       {$IFDEF AVXSUP}vxorpd ymm0, ymm0, ymm0;                        {$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} 
       {$IFDEF AVXSUP}vxorpd ymm1, ymm1, ymm1;                        {$ELSE}db $C5,$F5,$57,$C9;{$ENDIF} 

       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;
           // prefetch data...
           // prefetch [r8 + rax];

           // addition:
           {$IFDEF AVXSUP}vmovupd ymm2, [r8 + rax - 128];             {$ELSE}db $C4,$C1,$7D,$10,$54,$00,$80;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm2;                    {$ELSE}db $C5,$FD,$58,$C2;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd ymm2, [r8 + rax - 96];              {$ELSE}db $C4,$C1,$7D,$10,$54,$00,$A0;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm1, ymm1, ymm2;                    {$ELSE}db $C5,$F5,$58,$CA;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd ymm2, [r8 + rax - 64];              {$ELSE}db $C4,$C1,$7D,$10,$54,$00,$C0;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm2;                    {$ELSE}db $C5,$FD,$58,$C2;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd ymm2, [r8 + rax - 32];              {$ELSE}db $C4,$C1,$7D,$10,$54,$00,$E0;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm1, ymm1, ymm2;                    {$ELSE}db $C5,$F5,$58,$CA;{$ENDIF} 
       jmp @addforxloop

       @loopEnd:

       {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm1;                        {$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 
       {$IFDEF AVXSUP}vextractf128 xmm2, ymm0, 1;                     {$ELSE}db $C4,$E3,$7D,$19,$C2,$01;{$ENDIF} 
       {$IFDEF AVXSUP}vhaddpd xmm0, xmm0, xmm2;                       {$ELSE}db $C5,$F9,$7C,$C2;{$ENDIF} 

       sub rax, 128;

       jz @buildRes;

       @addforxloop2:
           add rax, 16;
           jg @@addforxloop2end;

           {$IFDEF AVXSUP}vmovupd xmm2, [r8 + rax - 16];              {$ELSE}db $C4,$C1,$79,$10,$54,$00,$F0;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd xmm0, xmm0, xmm2;                    {$ELSE}db $C5,$F9,$58,$C2;{$ENDIF} 
       jmp @addforxloop2;

       @@addforxloop2end:

       sub rax, 16;
       jz @buildRes;

       {$IFDEF AVXSUP}vaddsd xmm0, xmm0, [r8 + rax];                  {$ELSE}db $C4,$C1,$7B,$58,$04,$00;{$ENDIF} 

       @buildRes:

       // build result
       {$IFDEF AVXSUP}vhaddpd xmm0, xmm0, xmm0;                       {$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} 

       {$IFDEF AVXSUP}vdivsd xmm0, xmm0, xmm3;                        {$ELSE}db $C5,$FB,$5E,$C3;{$ENDIF} 

       // write result
       {$IFDEF AVXSUP}vmovsd [rcx], xmm0;                             {$ELSE}db $C5,$FB,$11,$01;{$ENDIF} 

       // next line:
       add r8, r9;
       add rcx, rdx;

   // loop y end
   dec r11;
   jnz @@addforyloop;

   // epilog
   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;

procedure AVXMatrixMeanColumnAligned(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}
var tmp : double;
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

   // note: RCX = dest, RDX = destLineWidth, R8 = src, R9 = srcLineWidth
   xor r10, r10;
   sub r10, height;
   imul r10, r9;

   // helper registers for the mt1, mt2 and dest pointers
   sub r8, r10;

   // fpc seems to have a problem with this opcode
   {$IFDEF FPC}
   mov rax, height;
   {$IFDEF AVXSUP}vcvtsi2sd xmm0, xmm0, rax;                          {$ELSE}db $C4,$E1,$FB,$2A,$C0;{$ENDIF} 
   {$ELSE}
   cvtsi2sd xmm0, height;
   {$ENDIF}
   lea rax, tmp;
   {$IFDEF AVXSUP}vmovsd [rax], xmm0;                                 {$ELSE}db $C5,$FB,$11,$00;{$ENDIF} 
   {$IFDEF AVXSUP}vbroadcastsd ymm2, [rax];                           {$ELSE}db $C4,$E2,$7D,$19,$10;{$ENDIF} // vbroadcastsd ymm2, xmm0; // avx2

   // for x := 0 to width - 1:
   mov r11, Width;
   sub r11, 4;
   jl @@addforxloop4end;

   // 4 columns at once
   @@addforxloop4:
       {$IFDEF AVXSUP}vxorpd ymm1, ymm1, ymm1;                        {$ELSE}db $C5,$F5,$57,$C9;{$ENDIF} 

       // for y := 0 to height - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforyloop4:
           {$IFDEF AVXSUP}vaddpd ymm1, ymm1, [r8 + rax];              {$ELSE}db $C4,$C1,$75,$58,$0C,$00;{$ENDIF} 
       add rax, r9;
       jnz @addforyloop4;

       // build result
       {$IFDEF AVXSUP}vdivpd ymm1, ymm1, ymm2;                        {$ELSE}db $C5,$F5,$5E,$CA;{$ENDIF} 
       {$IFDEF AVXSUP}vmovapd [rcx], ymm1;                            {$ELSE}db $C5,$FD,$29,$09;{$ENDIF} 

       // next columns:
       add rcx, 32;
       add r8, 32;

   // loop x end
   sub r11, 4;
   jge @@addforxloop4;

   @@addforxloop4end:

   add r11, 4;
   jz @@endProc;

   sub r11, 2;
   jl @@lastcolumn;

   {$IFDEF AVXSUP}vxorpd xmm1, xmm1, xmm1;                            {$ELSE}db $C5,$F1,$57,$C9;{$ENDIF} 

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov rax, r10;
   @addforyloop2:
       {$IFDEF AVXSUP}vaddpd xmm1, xmm1, [r8 + rax];                  {$ELSE}db $C4,$C1,$71,$58,$0C,$00;{$ENDIF} 
   add rax, r9;
   jnz @addforyloop2;

   // build result
   {$IFDEF AVXSUP}vdivpd xmm1, xmm1, xmm2;                            {$ELSE}db $C5,$F1,$5E,$CA;{$ENDIF} 
   {$IFDEF AVXSUP}vmovapd [rcx], xmm1;                                {$ELSE}db $C5,$F9,$29,$09;{$ENDIF} 

   // next columns:
   add rcx, 16;
   add r8, 16;

   dec r11;
   jnz @@endProc;

   @@lastcolumn:

   {$IFDEF AVXSUP}vxorpd xmm1, xmm1, xmm1;                            {$ELSE}db $C5,$F1,$57,$C9;{$ENDIF} 

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov rax, r10;
   @addforyloop:
       {$IFDEF AVXSUP}vaddsd xmm1, xmm1, [r8 + rax];                  {$ELSE}db $C4,$C1,$73,$58,$0C,$00;{$ENDIF} 
   add rax, r9;
   jnz @addforyloop;

   // build result
   {$IFDEF AVXSUP}vdivsd xmm1, xmm1, xmm2;                            {$ELSE}db $C5,$F3,$5E,$CA;{$ENDIF} 
   {$IFDEF AVXSUP}vmovsd [rcx], xmm1;                                 {$ELSE}db $C5,$FB,$11,$09;{$ENDIF} 

   @@endProc:
   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;

procedure AVXMatrixMeanColumnUnAligned(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}
var tmp : double; // for broadcastsd
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

   // note: RCX = dest, RDX = destLineWidth, R8 = src, R9 = srcLineWidth
   xor r10, r10;
   sub r10, height;
   imul r10, r9;

   // helper registers for the mt1, mt2 and dest pointers
   sub r8, r10;

   // fpc seems to have a problem with this opcode
   {$IFDEF FPC}
   mov rax, height;
   {$IFDEF AVXSUP}vcvtsi2sd xmm0, xmm0, rax;                          {$ELSE}db $C4,$E1,$FB,$2A,$C0;{$ENDIF} 
   {$ELSE}
   cvtsi2sd xmm0, height;
   {$ENDIF}
   lea rax, tmp;
   {$IFDEF AVXSUP}vmovsd [rax], xmm0;                                 {$ELSE}db $C5,$FB,$11,$00;{$ENDIF} 
   {$IFDEF AVXSUP}vbroadcastsd ymm2, [rax];                           {$ELSE}db $C4,$E2,$7D,$19,$10;{$ENDIF} 

   // for x := 0 to width - 1:
   mov r11, Width;
   sub r11, 4;
   jl @@addforxloop4end;

   // 4 columns at once
   @@addforxloop4:
       {$IFDEF AVXSUP}vxorpd ymm1, ymm1, ymm1;                        {$ELSE}db $C5,$F5,$57,$C9;{$ENDIF} 

       // for y := 0 to height - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforyloop4:
           {$IFDEF AVXSUP}vmovupd ymm0, [r8 + rax];                   {$ELSE}db $C4,$C1,$7D,$10,$04,$00;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm1, ymm1, ymm0;                    {$ELSE}db $C5,$F5,$58,$C8;{$ENDIF} 
       add rax, r9;
       jnz @addforyloop4;

       // build result
       {$IFDEF AVXSUP}vdivpd ymm1, ymm1, ymm2;                        {$ELSE}db $C5,$F5,$5E,$CA;{$ENDIF} 
       {$IFDEF AVXSUP}vmovupd [rcx], ymm1;                            {$ELSE}db $C5,$FD,$29,$09;{$ENDIF}

       // next columns:
       add rcx, 32;
       add r8, 32;

   // loop x end
   sub r11, 4;
   jge @@addforxloop4;

   @@addforxloop4end:

   add r11, 4;
   jz @@endProc;

   sub r11, 2;
   jl @@lastcolumn;

   {$IFDEF AVXSUP}vxorpd xmm1, xmm1, xmm1;                            {$ELSE}db $C5,$F1,$57,$C9;{$ENDIF} 

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov rax, r10;
   @addforyloop2:
       {$IFDEF AVXSUP}vmovupd xmm0, [r8 + rax];                       {$ELSE}db $C4,$C1,$79,$10,$04,$00;{$ENDIF} 
       {$IFDEF AVXSUP}vaddpd xmm1, xmm1, [r8 + rax];                  {$ELSE}db $C4,$C1,$71,$58,$0C,$00;{$ENDIF} 
   add rax, r9;
   jnz @addforyloop2;

   // build result
   {$IFDEF AVXSUP}vdivpd xmm1, xmm1, xmm2;                            {$ELSE}db $C5,$F1,$5E,$CA;{$ENDIF} 
   {$IFDEF AVXSUP}vmovupd [rcx], xmm1;                                {$ELSE}db $C5,$F9,$11,$09;{$ENDIF} 

   // next columns:
   add rcx, 16;
   add r8, 16;

   dec r11;
   jnz @@endProc;

   @@lastcolumn:

   {$IFDEF AVXSUP}vxorpd xmm1, xmm1, xmm1;                            {$ELSE}db $C5,$F1,$57,$C9;{$ENDIF} 

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov rax, r10;
   @addforyloop:
       {$IFDEF AVXSUP}vaddsd xmm1, xmm1, [r8 + rax];                  {$ELSE}db $C4,$C1,$73,$58,$0C,$00;{$ENDIF} 
   add rax, r9;
   jnz @addforyloop;

   // build result
   {$IFDEF AVXSUP}vdivsd xmm1, xmm1, xmm2;                            {$ELSE}db $C5,$F3,$5E,$CA;{$ENDIF} 
   {$IFDEF AVXSUP}vmovsd [rcx], xmm1;                                 {$ELSE}db $C5,$FB,$11,$09;{$ENDIF} 


   @@endProc:
   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;

// ###########################################
// #### Variance calculation
// ###########################################


procedure AVXMatrixVarRowAligned(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt; unbiased : boolean); {$IFDEF FPC}assembler;{$ENDIF}
var tmp : double;
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

   // note: RCX = dest, RDX = destLineWidth, R8 = src, R9 = srcLineWidth

   // prolog
   sub rsp, $10;
   {$IFDEF AVXSUP}vmovupd [rsp + $00], xmm4;                          {$ELSE}db $C5,$F9,$11,$24,$24;{$ENDIF} 

   // iters := -width*sizeof(double)
   mov r10, width;
   shl r10, 3;
   imul r10, -1;

   // helper registers for the mt1, mt2 and dest pointers
   sub r8, r10;

   // fpc seems to have a problem with this opcode
   {$IFDEF FPC}
   mov rax, width;
   {$IFDEF AVXSUP}vcvtsi2sd xmm3, xmm3, rax;                          {$ELSE}db $C4,$E1,$E3,$2A,$D8;{$ENDIF} 
   {$ELSE}
   cvtsi2sd xmm3, width;
   {$ENDIF}

   // for y := 0 to height - 1:
   mov r11, Height;
   @@addforyloop:
       {$IFDEF AVXSUP}vxorpd ymm0, ymm0, ymm0;                        {$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} 
       {$IFDEF AVXSUP}vxorpd ymm1, ymm1, ymm1;                        {$ELSE}db $C5,$F5,$57,$C9;{$ENDIF} 

       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;
           // prefetch data...
           // prefetch [r8 + rax];

           // addition:
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, [r8 + rax - 128];        {$ELSE}db $C4,$C1,$7D,$58,$44,$00,$80;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm1, ymm1, [r8 + rax - 96];         {$ELSE}db $C4,$C1,$75,$58,$4C,$00,$A0;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, [r8 + rax - 64];         {$ELSE}db $C4,$C1,$7D,$58,$44,$00,$C0;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm1, ymm1, [r8 + rax - 32];         {$ELSE}db $C4,$C1,$75,$58,$4C,$00,$E0;{$ENDIF} 
       jmp @addforxloop

       @loopEnd:

       {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm1;                        {$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 
       {$IFDEF AVXSUP}vextractf128 xmm2, ymm0, 1;                     {$ELSE}db $C4,$E3,$7D,$19,$C2,$01;{$ENDIF} 
       {$IFDEF AVXSUP}vhaddpd xmm0, xmm0, xmm2;                       {$ELSE}db $C5,$F9,$7C,$C2;{$ENDIF} 

       sub rax, 128;

       jz @buildRes;

       @addforxloop2:
           add rax, 16;
           jg @@addforxloop2end;

           {$IFDEF AVXSUP}vaddpd xmm0, xmm0, [r8 + rax - 16];         {$ELSE}db $C4,$C1,$79,$58,$44,$00,$F0;{$ENDIF} 
       jmp @addforxloop2;

       @@addforxloop2end:

       sub rax, 16;
       jz @buildRes;

       {$IFDEF AVXSUP}vaddsd xmm0, xmm0, [r8 + rax];                  {$ELSE}db $C4,$C1,$7B,$58,$04,$00;{$ENDIF} 

       @buildRes:

       // build result
       {$IFDEF AVXSUP}vhaddpd xmm0, xmm0, xmm0;                       {$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} 
       {$IFDEF AVXSUP}vdivsd xmm0, xmm0, xmm3;                        {$ELSE}db $C5,$FB,$5E,$C3;{$ENDIF} 

       // we have calculated the mean ->
       // repeat the loop to calculate the variance
       lea rax, tmp;
       {$IFDEF AVXSUP}vmovsd [rax], xmm0;                             {$ELSE}db $C5,$FB,$11,$00;{$ENDIF} 
       {$IFDEF AVXSUP}vbroadcastsd ymm4, [rax];                       {$ELSE}db $C4,$E2,$7D,$19,$20;{$ENDIF} 
       {$IFDEF AVXSUP}vxorpd xmm0, xmm0, xmm0;                        {$ELSE}db $C5,$F9,$57,$C0;{$ENDIF} 

       // for x := 0 to w - 1;
       // prepare for reverse loop
       // variance = sum (x - mean)^2
       mov rax, r10;
       @addforxloop3:
           add rax, 128;
           jg @loopEnd2;
           // prefetch data...
           // prefetch [ecx + rax];

           // addition:
           {$IFDEF AVXSUP}vmovapd ymm1, [r8 + rax - 128];             {$ELSE}db $C4,$C1,$7D,$28,$4C,$00,$80;{$ENDIF} 
           {$IFDEF AVXSUP}vsubpd ymm1, ymm1, ymm4;                    {$ELSE}db $C5,$F5,$5C,$CC;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm1, ymm1, ymm1;                    {$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm1;                    {$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 

           {$IFDEF AVXSUP}vmovapd ymm1, [r8 + rax - 96];              {$ELSE}db $C4,$C1,$7D,$28,$4C,$00,$A0;{$ENDIF} 
           {$IFDEF AVXSUP}vsubpd ymm1, ymm1, ymm4;                    {$ELSE}db $C5,$F5,$5C,$CC;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm1, ymm1, ymm1;                    {$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm1;                    {$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 

           {$IFDEF AVXSUP}vmovapd ymm1, [r8 + rax - 64];              {$ELSE}db $C4,$C1,$7D,$28,$4C,$00,$C0;{$ENDIF} 
           {$IFDEF AVXSUP}vsubpd ymm1, ymm1, ymm4;                    {$ELSE}db $C5,$F5,$5C,$CC;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm1, ymm1, ymm1;                    {$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm1;                    {$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 

           {$IFDEF AVXSUP}vmovapd ymm1, [r8 + rax - 32];              {$ELSE}db $C4,$C1,$7D,$28,$4C,$00,$E0;{$ENDIF} 
           {$IFDEF AVXSUP}vsubpd ymm1, ymm1, ymm4;                    {$ELSE}db $C5,$F5,$5C,$CC;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm1, ymm1, ymm1;                    {$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm1;                    {$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 

       jmp @addforxloop3

       @loopEnd2:

       sub rax, 128;

       {$IFDEF AVXSUP}vextractf128 xmm2, ymm0, 1;                     {$ELSE}db $C4,$E3,$7D,$19,$C2,$01;{$ENDIF} 
       {$IFDEF AVXSUP}vhaddpd xmm0, xmm0, xmm2;                       {$ELSE}db $C5,$F9,$7C,$C2;{$ENDIF} 

       jz @buildRes2;

       @addforxloop4:
           add rax, 16;
           jg @addforxloop4end;

           {$IFDEF AVXSUP}vmovapd xmm1, [r8 + rax - 16];              {$ELSE}db $C4,$C1,$79,$28,$4C,$00,$F0;{$ENDIF} 
           {$IFDEF AVXSUP}vsubpd xmm1, xmm1, xmm4;                    {$ELSE}db $C5,$F1,$5C,$CC;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd xmm1, xmm1, xmm1;                    {$ELSE}db $C5,$F1,$59,$C9;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd xmm0, xmm0, xmm1;                    {$ELSE}db $C5,$F9,$58,$C1;{$ENDIF} 
       jmp @addforxloop4;

       @addforxloop4end:

       sub rax, 16;
       jz @buildRes2;

       // last column
       {$IFDEF AVXSUP}vmovsd xmm1, [r8 + rax];                        {$ELSE}db $C4,$C1,$7B,$10,$0C,$00;{$ENDIF} 
       {$IFDEF AVXSUP}vsubsd xmm1, xmm1, xmm4;                        {$ELSE}db $C5,$F3,$5C,$CC;{$ENDIF} 
       {$IFDEF AVXSUP}vmulsd xmm1, xmm1, xmm1;                        {$ELSE}db $C5,$F3,$59,$C9;{$ENDIF} 
       {$IFDEF AVXSUP}vaddsd xmm0, xmm0, xmm1;                        {$ELSE}db $C5,$FB,$58,$C1;{$ENDIF} 


       @buildRes2:

       // build result
       {$IFDEF AVXSUP}vhaddpd xmm0, xmm0, xmm0;                       {$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} 

       // check if we need to use the unbiased version
       cmp unbiased, 0;
       jz @@dobiased;

       lea rax, [rip + cOne];
       {$IFDEF AVXSUP}vmovsd xmm2, [rax];                             {$ELSE}db $C5,$FB,$10,$10;{$ENDIF} 
       {$IFDEF AVXSUP}vsubsd xmm4, xmm3, xmm2;                        {$ELSE}db $C5,$E3,$5C,$E2;{$ENDIF} 
       {$IFDEF AVXSUP}vmaxsd xmm4, xmm4, xmm2;                        {$ELSE}db $C5,$DB,$5F,$E2;{$ENDIF} 

       {$IFDEF AVXSUP}vdivsd xmm0, xmm0, xmm4;                        {$ELSE}db $C5,$FB,$5E,$C4;{$ENDIF} 

       jmp @@writeRes;

       @@dobiased:
       {$IFDEF AVXSUP}vdivsd xmm0, xmm0, xmm3;                        {$ELSE}db $C5,$FB,$5E,$C3;{$ENDIF} 

       // write result
       @@writeRes:
       {$IFDEF AVXSUP}vmovsd [rcx], xmm0;                             {$ELSE}db $C5,$FB,$11,$01;{$ENDIF} 

       // next line:
       add r8, r9;
       add rcx, rdx;

   // loop y end
   dec r11;
   jnz @@addforyloop;

   // epilog
   {$IFDEF AVXSUP}vmovupd xmm4, [rsp + $00];                          {$ELSE}db $C5,$F9,$10,$24,$24;{$ENDIF} 
   add rsp, $10;
   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;

procedure AVXMatrixVarRowUnAligned(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt; unbiased : boolean); {$IFDEF FPC}assembler;{$ENDIF}
var tmp : double;
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

   // note: RCX = dest, RDX = destLineWidth, R8 = src, R9 = srcLineWidth

   // prolog
   sub rsp, $10;
   {$IFDEF AVXSUP}vmovupd [rsp + $00], xmm4;                          {$ELSE}db $C5,$F9,$11,$24,$24;{$ENDIF} 

   // iters := -width*sizeof(double)
   mov r10, width;
   shl r10, 3;
   imul r10, -1;

   // helper registers for the mt1, mt2 and dest pointers
   sub r8, r10;

   // fpc seems to have a problem with this opcode
   {$IFDEF FPC}
   mov rax, width;
   {$IFDEF AVXSUP}vcvtsi2sd xmm3, xmm3, rax;                          {$ELSE}db $C4,$E1,$E3,$2A,$D8;{$ENDIF} 
   {$ELSE}
   cvtsi2sd xmm3, width;
   {$ENDIF}

   // for y := 0 to height - 1:
   mov r11, Height;
   @@addforyloop:
       {$IFDEF AVXSUP}vxorpd ymm0, ymm0, ymm0;                        {$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} 
       {$IFDEF AVXSUP}vxorpd ymm1, ymm1, ymm1;                        {$ELSE}db $C5,$F5,$57,$C9;{$ENDIF} 

       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;
           // prefetch data...
           // prefetch [r8 + rax];

           // addition:
           {$IFDEF AVXSUP}vmovupd ymm2, [r8 + rax - 128];             {$ELSE}db $C4,$C1,$7D,$10,$54,$00,$80;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm2;                    {$ELSE}db $C5,$FD,$58,$C2;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd ymm2, [r8 + rax - 96];              {$ELSE}db $C4,$C1,$7D,$10,$54,$00,$A0;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm1, ymm1, ymm2;                    {$ELSE}db $C5,$F5,$58,$CA;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd ymm2, [r8 + rax - 64];              {$ELSE}db $C4,$C1,$7D,$10,$54,$00,$C0;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm2;                    {$ELSE}db $C5,$FD,$58,$C2;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd ymm2, [r8 + rax - 32];              {$ELSE}db $C4,$C1,$7D,$10,$54,$00,$E0;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm1, ymm1, ymm2;                    {$ELSE}db $C5,$F5,$58,$CA;{$ENDIF} 
       jmp @addforxloop

       @loopEnd:

       {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm1;                        {$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 
       {$IFDEF AVXSUP}vextractf128 xmm2, ymm0, 1;                     {$ELSE}db $C4,$E3,$7D,$19,$C2,$01;{$ENDIF} 
       {$IFDEF AVXSUP}vhaddpd xmm0, xmm0, xmm2;                       {$ELSE}db $C5,$F9,$7C,$C2;{$ENDIF} 

       sub rax, 128;

       jz @buildRes;

       @addforxloop2:
           add rax, 16;
           jg @@addforxloop2end;

           {$IFDEF AVXSUP}vmovupd xmm2, [r8 + rax - 16];              {$ELSE}db $C4,$C1,$79,$10,$54,$00,$F0;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd xmm0, xmm0, xmm2;                    {$ELSE}db $C5,$F9,$58,$C2;{$ENDIF} 
       jmp @addforxloop2;

       @@addforxloop2end:

       sub rax, 16;
       jz @buildRes;

       {$IFDEF AVXSUP}vaddsd xmm0, xmm0, [r8 + rax];                  {$ELSE}db $C4,$C1,$7B,$58,$04,$00;{$ENDIF} 

       @buildRes:

       // build result
       {$IFDEF AVXSUP}vhaddpd xmm0, xmm0, xmm0;                       {$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} 
       {$IFDEF AVXSUP}vdivsd xmm0, xmm0, xmm3;                        {$ELSE}db $C5,$FB,$5E,$C3;{$ENDIF} 

       // we have calculated the mean ->
       // repeat the loop to calculate the variance
       lea rax, tmp;
       {$IFDEF AVXSUP}vmovsd [rax], xmm0;                             {$ELSE}db $C5,$FB,$11,$00;{$ENDIF} 
       {$IFDEF AVXSUP}vbroadcastsd ymm4, [rax];                       {$ELSE}db $C4,$E2,$7D,$19,$20;{$ENDIF} 
       {$IFDEF AVXSUP}vxorpd xmm0, xmm0, xmm0;                        {$ELSE}db $C5,$F9,$57,$C0;{$ENDIF} 

       // for x := 0 to w - 1;
       // prepare for reverse loop
       // variance = sum (x - mean)^2
       mov rax, r10;
       @addforxloop3:
           add rax, 128;
           jg @loopEnd2;
           // prefetch data...
           // prefetch [ecx + rax];

           // addition:
           {$IFDEF AVXSUP}vmovupd ymm1, [r8 + rax - 128];             {$ELSE}db $C4,$C1,$7D,$10,$4C,$00,$80;{$ENDIF} 
           {$IFDEF AVXSUP}vsubpd ymm1, ymm1, ymm4;                    {$ELSE}db $C5,$F5,$5C,$CC;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm1, ymm1, ymm1;                    {$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm1;                    {$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd ymm1, [r8 + rax - 96];              {$ELSE}db $C4,$C1,$7D,$10,$4C,$00,$A0;{$ENDIF} 
           {$IFDEF AVXSUP}vsubpd ymm1, ymm1, ymm4;                    {$ELSE}db $C5,$F5,$5C,$CC;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm1, ymm1, ymm1;                    {$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm1;                    {$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd ymm1, [r8 + rax - 64];              {$ELSE}db $C4,$C1,$7D,$10,$4C,$00,$C0;{$ENDIF} 
           {$IFDEF AVXSUP}vsubpd ymm1, ymm1, ymm4;                    {$ELSE}db $C5,$F5,$5C,$CC;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm1, ymm1, ymm1;                    {$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm1;                    {$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd ymm1, [r8 + rax - 32];              {$ELSE}db $C4,$C1,$7D,$10,$4C,$00,$E0;{$ENDIF} 
           {$IFDEF AVXSUP}vsubpd ymm1, ymm1, ymm4;                    {$ELSE}db $C5,$F5,$5C,$CC;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm1, ymm1, ymm1;                    {$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm1;                    {$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 

       jmp @addforxloop3

       @loopEnd2:

       sub rax, 128;

       {$IFDEF AVXSUP}vextractf128 xmm2, ymm0, 1;                     {$ELSE}db $C4,$E3,$7D,$19,$C2,$01;{$ENDIF} 
       {$IFDEF AVXSUP}vhaddpd xmm0, xmm0, xmm2;                       {$ELSE}db $C5,$F9,$7C,$C2;{$ENDIF} 

       jz @buildRes2;

       @addforxloop4:
           add rax, 16;
           jg @addforxloop4end;

           {$IFDEF AVXSUP}vmovupd xmm1, [r8 + rax - 16];              {$ELSE}db $C4,$C1,$79,$10,$4C,$00,$F0;{$ENDIF} 
           {$IFDEF AVXSUP}vsubpd xmm1, xmm1, xmm4;                    {$ELSE}db $C5,$F1,$5C,$CC;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd xmm1, xmm1, xmm1;                    {$ELSE}db $C5,$F1,$59,$C9;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd xmm0, xmm0, xmm1;                    {$ELSE}db $C5,$F9,$58,$C1;{$ENDIF} 
       jmp @addforxloop4;

       @addforxloop4end:

       sub rax, 16;
       jz @buildRes2;

       // last column
       {$IFDEF AVXSUP}vmovsd xmm1, [r8 + rax];                        {$ELSE}db $C4,$C1,$7B,$10,$0C,$00;{$ENDIF} 
       {$IFDEF AVXSUP}vsubsd xmm1, xmm1, xmm4;                        {$ELSE}db $C5,$F3,$5C,$CC;{$ENDIF} 
       {$IFDEF AVXSUP}vmulsd xmm1, xmm1, xmm1;                        {$ELSE}db $C5,$F3,$59,$C9;{$ENDIF} 
       {$IFDEF AVXSUP}vaddsd xmm0, xmm0, xmm1;                        {$ELSE}db $C5,$FB,$58,$C1;{$ENDIF} 


       @buildRes2:

       // build result
       {$IFDEF AVXSUP}vhaddpd xmm0, xmm0, xmm0;                       {$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} 

       // check if we need to use the unbiased version
       cmp unbiased, 0;
       jz @@dobiased;

       lea rax, [rip + cOne];
       {$IFDEF AVXSUP}vmovsd xmm2, [rax];                             {$ELSE}db $C5,$FB,$10,$10;{$ENDIF} 
       {$IFDEF AVXSUP}vsubsd xmm4, xmm3, xmm2;                        {$ELSE}db $C5,$E3,$5C,$E2;{$ENDIF} 
       {$IFDEF AVXSUP}vmaxsd xmm4, xmm4, xmm2;                        {$ELSE}db $C5,$DB,$5F,$E2;{$ENDIF} 

       {$IFDEF AVXSUP}vdivsd xmm0, xmm0, xmm4;                        {$ELSE}db $C5,$FB,$5E,$C4;{$ENDIF} 

       jmp @@writeRes;

       @@dobiased:
       {$IFDEF AVXSUP}vdivsd xmm0, xmm0, xmm3;                        {$ELSE}db $C5,$FB,$5E,$C3;{$ENDIF} 

       // write result
       @@writeRes:
       {$IFDEF AVXSUP}vmovsd [rcx], xmm0;                             {$ELSE}db $C5,$FB,$11,$01;{$ENDIF} 

       // next line:
       add r8, r9;
       add rcx, rdx;

   // loop y end
   dec r11;
   jnz @@addforyloop;

   // epilog
   {$IFDEF AVXSUP}vmovupd xmm4, [rsp + $00];                          {$ELSE}db $C5,$F9,$10,$24,$24;{$ENDIF} 
   add rsp, $10;
   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;

procedure AVXMatrixVarColumnAligned(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt; unbiased : boolean); {$IFDEF FPC}assembler;{$ENDIF}
var tmp : double;
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

   // prolog
   sub rsp, $20;
   {$IFDEF AVXSUP}vmovupd [rsp + $00], xmm4;                          {$ELSE}db $C5,$F9,$11,$24,$24;{$ENDIF} 
   {$IFDEF AVXSUP}vmovupd [rsp + $10], xmm5;                          {$ELSE}db $C5,$F9,$11,$6C,$24,$10;{$ENDIF} 

   // note: RCX = dest, RDX = destLineWidth, R8 = src, R9 = srcLineWidth
   xor r10, r10;
   sub r10, height;
   imul r10, r9;

   // helper registers for the mt1, mt2 and dest pointers
   sub r8, r10;

   // fpc seems to have a problem with this opcode
   {$IFDEF FPC}
   mov rax, height;
   {$IFDEF AVXSUP}vcvtsi2sd xmm0, xmm0, rax;                          {$ELSE}db $C4,$E1,$FB,$2A,$C0;{$ENDIF} 
   {$ELSE}
   cvtsi2sd xmm0, height;
   {$ENDIF}
   lea rax, tmp;
   {$IFDEF AVXSUP}vmovsd [rax], xmm0;                                 {$ELSE}db $C5,$FB,$11,$00;{$ENDIF} 
   {$IFDEF AVXSUP}vbroadcastsd ymm2, [rax];                           {$ELSE}db $C4,$E2,$7D,$19,$10;{$ENDIF} // vbroadcastsd ymm2, xmm0;  // is avx2

   lea rax, [rip + cOne];
   {$IFDEF AVXSUP}vbroadcastsd ymm5, [rax];                           {$ELSE}db $C4,$E2,$7D,$19,$28;{$ENDIF} 


   // for x := 0 to width - 1:
   mov r11, Width;
   sub r11, 4;
   jl @@addforxloop4end;

   // 4 columns at once
   @@addforxloop4:
       {$IFDEF AVXSUP}vxorpd ymm1, ymm1, ymm1;                        {$ELSE}db $C5,$F5,$57,$C9;{$ENDIF} 

       // for y := 0 to height - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforyloop4:
           {$IFDEF AVXSUP}vaddpd ymm1, ymm1, [r8 + rax];              {$ELSE}db $C4,$C1,$75,$58,$0C,$00;{$ENDIF} 
       add rax, r9;
       jnz @addforyloop4;

       // build result
       {$IFDEF AVXSUP}vdivpd ymm0, ymm1, ymm2;                        {$ELSE}db $C5,$F5,$5E,$C2;{$ENDIF} 

       // calculate final standard deviation
       // for y := 0 to height - 1;
       // prepare for reverse loop
       {$IFDEF AVXSUP}vxorpd ymm4, ymm4, ymm4;                        {$ELSE}db $C5,$DD,$57,$E4;{$ENDIF} 
       mov rax, r10;
       @addforyloop4_2:
           {$IFDEF AVXSUP}vmovapd ymm1, [r8 + rax];                   {$ELSE}db $C4,$C1,$7D,$28,$0C,$00;{$ENDIF} 
           {$IFDEF AVXSUP}vsubpd ymm1, ymm1, ymm0;                    {$ELSE}db $C5,$F5,$5C,$C8;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm1, ymm1, ymm1;                    {$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm4, ymm4, ymm1;                    {$ELSE}db $C5,$DD,$58,$E1;{$ENDIF} 
       add rax, r9;
       jnz @addforyloop4_2;

       // check if we need to use the unbiased version
       cmp unbiased, 0;
       jz @@dobiased_4;

       lea rax, [rip + cOne];

       {$IFDEF AVXSUP}vbroadcastsd ymm1, [rax];                       {$ELSE}db $C4,$E2,$7D,$19,$08;{$ENDIF} 
       {$IFDEF AVXSUP}vsubpd ymm3, ymm2, ymm1;                        {$ELSE}db $C5,$ED,$5C,$D9;{$ENDIF} 
       {$IFDEF AVXSUP}vmaxpd ymm3, ymm3, ymm1;                        {$ELSE}db $C5,$E5,$5F,$D9;{$ENDIF} 

       {$IFDEF AVXSUP}vdivpd ymm4, ymm4, ymm3;                        {$ELSE}db $C5,$DD,$5E,$E3;{$ENDIF} 

       jmp @@writeRes_4;

       @@dobiased_4:
       {$IFDEF AVXSUP}vdivpd ymm4, ymm4, ymm2;                        {$ELSE}db $C5,$DD,$5E,$E2;{$ENDIF} 

       // write result
       @@writeRes_4:
       {$IFDEF AVXSUP}vmovapd [rcx], ymm4;                            {$ELSE}db $C5,$FD,$29,$21;{$ENDIF} 

       // next columns:
       add rcx, 32;
       add r8, 32;

   // loop x end
   sub r11, 4;
   jge @@addforxloop4;

   @@addforxloop4end:

   add r11, 4;
   jz @@endProc;

   sub r11, 2;
   jl @@lastcolumn;

   {$IFDEF AVXSUP}vxorpd xmm1, xmm1, xmm1;                            {$ELSE}db $C5,$F1,$57,$C9;{$ENDIF} 

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov rax, r10;
   @addforyloop2:
       {$IFDEF AVXSUP}vaddpd xmm1, xmm1, [r8 + rax];                  {$ELSE}db $C4,$C1,$71,$58,$0C,$00;{$ENDIF} 
   add rax, r9;
   jnz @addforyloop2;

   // build result
   {$IFDEF AVXSUP}vdivpd xmm0, xmm1, xmm2;                            {$ELSE}db $C5,$F1,$5E,$C2;{$ENDIF} 

   // calculate final standard deviation
   // for y := 0 to height - 1;
   // prepare for reverse loop
   {$IFDEF AVXSUP}vxorpd xmm4, xmm4, xmm4;                            {$ELSE}db $C5,$D9,$57,$E4;{$ENDIF} 
   mov rax, r10;
   @addforyloop2_2:
      {$IFDEF AVXSUP}vmovapd xmm1, [r8 + rax];                        {$ELSE}db $C4,$C1,$79,$28,$0C,$00;{$ENDIF} 
      {$IFDEF AVXSUP}vsubpd xmm1, xmm1, xmm0;                         {$ELSE}db $C5,$F1,$5C,$C8;{$ENDIF} 
      {$IFDEF AVXSUP}vmulpd xmm1, xmm1, xmm1;                         {$ELSE}db $C5,$F1,$59,$C9;{$ENDIF} 
      {$IFDEF AVXSUP}vaddpd xmm4, xmm4, xmm1;                         {$ELSE}db $C5,$D9,$58,$E1;{$ENDIF} 
   add rax, r9;
   jnz @addforyloop2_2;

   // check if we need to use the unbiased version
   cmp unbiased, 0;
   jz @@dobiased_2;

   {$IFDEF AVXSUP}vsubpd xmm3, xmm2, xmm5;                            {$ELSE}db $C5,$E9,$5C,$DD;{$ENDIF} 
   {$IFDEF AVXSUP}vmaxpd xmm3, xmm3, xmm5;                            {$ELSE}db $C5,$E1,$5F,$DD;{$ENDIF} 

   {$IFDEF AVXSUP}vdivpd xmm4, xmm4, xmm3;                            {$ELSE}db $C5,$D9,$5E,$E3;{$ENDIF} 

   jmp @@writeRes2;

   @@dobiased_2:
   {$IFDEF AVXSUP}vdivpd xmm4, xmm4, xmm2;                            {$ELSE}db $C5,$D9,$5E,$E2;{$ENDIF} 

   // write result
   @@writeRes2:
   {$IFDEF AVXSUP}vmovapd [rcx], xmm4;                                {$ELSE}db $C5,$F9,$29,$21;{$ENDIF} 

   // next columns:
   add rcx, 16;
   add r8, 16;

   dec r11;
   jnz @@endProc;

   @@lastcolumn:

   {$IFDEF AVXSUP}vxorpd xmm1, xmm1, xmm1;                            {$ELSE}db $C5,$F1,$57,$C9;{$ENDIF} 

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov rax, r10;
   @addforyloop_1:
       {$IFDEF AVXSUP}vaddsd xmm1, xmm1, [r8 + rax];                  {$ELSE}db $C4,$C1,$73,$58,$0C,$00;{$ENDIF} 
   add rax, r9;
   jnz @addforyloop_1;

   // build result
   {$IFDEF AVXSUP}vdivsd xmm0, xmm1, xmm2;                            {$ELSE}db $C5,$F3,$5E,$C2;{$ENDIF} 

   // calculate final standard deviation
   // for y := 0 to height - 1;
   // prepare for reverse loop
   {$IFDEF AVXSUP}vxorpd xmm4, xmm4, xmm4;                            {$ELSE}db $C5,$D9,$57,$E4;{$ENDIF} 
   mov rax, r10;
   @addforyloop1_2:
      {$IFDEF AVXSUP}vmovsd xmm1, [r8 + rax];                         {$ELSE}db $C4,$C1,$7B,$10,$0C,$00;{$ENDIF} 
      {$IFDEF AVXSUP}vsubsd xmm1, xmm1, xmm0;                         {$ELSE}db $C5,$F3,$5C,$C8;{$ENDIF} 
      {$IFDEF AVXSUP}vmulsd xmm1, xmm1, xmm1;                         {$ELSE}db $C5,$F3,$59,$C9;{$ENDIF} 
      {$IFDEF AVXSUP}vaddsd xmm4, xmm4, xmm1;                         {$ELSE}db $C5,$DB,$58,$E1;{$ENDIF} 
   add rax, r9;
   jnz @addforyloop1_2;

   // check if we need to use the unbiased version
   cmp unbiased, 0;
   jz @@dobiased_2;

   {$IFDEF AVXSUP}vmovsd xmm3, xmm3, xmm2;                            {$ELSE}db $C5,$E3,$11,$D3;{$ENDIF} 
   {$IFDEF AVXSUP}vsubsd xmm3, xmm3, xmm5;                            {$ELSE}db $C5,$E3,$5C,$DD;{$ENDIF} 
   {$IFDEF AVXSUP}vmaxsd xmm3, xmm3, xmm5;                            {$ELSE}db $C5,$E3,$5F,$DD;{$ENDIF} 

   {$IFDEF AVXSUP}vdivsd xmm4, xmm4, xmm3;                            {$ELSE}db $C5,$DB,$5E,$E3;{$ENDIF} 

   jmp @@writeRes_1;

   @@dobiased_1:
   {$IFDEF AVXSUP}vdivsd xmm4, xmm4, xmm2;                            {$ELSE}db $C5,$DB,$5E,$E2;{$ENDIF} 

   // write result
   @@writeRes_1:
   {$IFDEF AVXSUP}vmovsd [rcx], xmm4;                                 {$ELSE}db $C5,$FB,$11,$21;{$ENDIF} 


   @@endProc:

   // epilog
   {$IFDEF AVXSUP}vmovupd xmm4, [rsp + $00];                          {$ELSE}db $C5,$F9,$10,$24,$24;{$ENDIF} 
   {$IFDEF AVXSUP}vmovupd xmm5, [rsp + $10];                          {$ELSE}db $C5,$F9,$10,$6C,$24,$10;{$ENDIF} 
   add rsp, $20;
   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;

procedure AVXMatrixVarColumnUnAligned(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt; unbiased : boolean); {$IFDEF FPC}assembler;{$ENDIF}
var tmp : double;
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

   // prolog
   sub rsp, $20;
   {$IFDEF AVXSUP}vmovupd [rsp + $00], xmm4;                          {$ELSE}db $C5,$F9,$11,$24,$24;{$ENDIF} 
   {$IFDEF AVXSUP}vmovupd [rsp + $10], xmm5;                          {$ELSE}db $C5,$F9,$11,$6C,$24,$10;{$ENDIF} 

   // note: RCX = dest, RDX = destLineWidth, R8 = src, R9 = srcLineWidth
   xor r10, r10;
   sub r10, height;
   imul r10, r9;

   // helper registers for the mt1, mt2 and dest pointers
   sub r8, r10;

   // fpc seems to have a problem with this opcode
   {$IFDEF FPC}
   mov rax, height;
   {$IFDEF AVXSUP}vcvtsi2sd xmm0, xmm0, rax;                          {$ELSE}db $C4,$E1,$FB,$2A,$C0;{$ENDIF} 
   {$ELSE}
   cvtsi2sd xmm0, height;
   {$ENDIF}
   lea rax, tmp;
   {$IFDEF AVXSUP}vmovsd [rax], xmm0;                                 {$ELSE}db $C5,$FB,$11,$00;{$ENDIF} 
   {$IFDEF AVXSUP}vbroadcastsd ymm2, [rax];                           {$ELSE}db $C4,$E2,$7D,$19,$10;{$ENDIF} // vbroadcastsd ymm2, xmm0; // avx2

   lea rax, [rip + cOne];
   {$IFDEF AVXSUP}vbroadcastsd ymm5, [rax];                           {$ELSE}db $C4,$E2,$7D,$19,$28;{$ENDIF} 

   // for x := 0 to width - 1:
   mov r11, Width;
   sub r11, 4;
   jl @@addforxloop4end;

   // 4 columns at once
   @@addforxloop4:
       {$IFDEF AVXSUP}vxorpd ymm1, ymm1, ymm1;                        {$ELSE}db $C5,$F5,$57,$C9;{$ENDIF} 

       // for y := 0 to height - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforyloop4:
           {$IFDEF AVXSUP}vmovupd ymm3, [r8 + rax];                   {$ELSE}db $C4,$C1,$7D,$10,$1C,$00;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm1, ymm1, ymm3;                    {$ELSE}db $C5,$F5,$58,$CB;{$ENDIF} 
       add rax, r9;
       jnz @addforyloop4;

       // build result
       {$IFDEF AVXSUP}vdivpd ymm0, ymm1, ymm2;                        {$ELSE}db $C5,$F5,$5E,$C2;{$ENDIF} 

       // calculate final standard deviation
       // for y := 0 to height - 1;
       // prepare for reverse loop
       {$IFDEF AVXSUP}vxorpd ymm4, ymm4, ymm4;                        {$ELSE}db $C5,$DD,$57,$E4;{$ENDIF} 
       mov rax, r10;
       @addforyloop4_2:
           {$IFDEF AVXSUP}vmovupd ymm1, [r8 + rax];                   {$ELSE}db $C4,$C1,$7D,$10,$0C,$00;{$ENDIF} 
           {$IFDEF AVXSUP}vsubpd ymm1, ymm1, ymm0;                    {$ELSE}db $C5,$F5,$5C,$C8;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm1, ymm1, ymm1;                    {$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm4, ymm4, ymm1;                    {$ELSE}db $C5,$DD,$58,$E1;{$ENDIF} 
       add rax, r9;
       jnz @addforyloop4_2;

       // check if we need to use the unbiased version
       cmp unbiased, 0;
       jz @@dobiased_4;

       {$IFDEF AVXSUP}vsubpd ymm3, ymm2, ymm5;                        {$ELSE}db $C5,$ED,$5C,$DD;{$ENDIF} 
       {$IFDEF AVXSUP}vmaxpd ymm3, ymm3, ymm5;                        {$ELSE}db $C5,$E5,$5F,$DD;{$ENDIF} 

       {$IFDEF AVXSUP}vdivpd ymm4, ymm4, ymm3;                        {$ELSE}db $C5,$DD,$5E,$E3;{$ENDIF} 

       jmp @@writeRes_4;

       @@dobiased_4:
       {$IFDEF AVXSUP}vdivpd ymm4, ymm4, ymm2;                        {$ELSE}db $C5,$DD,$5E,$E2;{$ENDIF} 

       // write result
       @@writeRes_4:
       {$IFDEF AVXSUP}vmovupd [rcx], ymm4;                            {$ELSE}db $C5,$FD,$11,$21;{$ENDIF} 

       // next columns:
       add rcx, 32;
       add r8, 32;

   // loop x end
   sub r11, 4;
   jge @@addforxloop4;

   @@addforxloop4end:

   add r11, 4;
   jz @@endProc;

   sub r11, 2;
   jl @@lastcolumn;

   {$IFDEF AVXSUP}vxorpd xmm1, xmm1, xmm1;                            {$ELSE}db $C5,$F1,$57,$C9;{$ENDIF} 

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov rax, r10;
   @addforyloop2:
       {$IFDEF AVXSUP}vmovupd xmm3, [r8 + rax];                       {$ELSE}db $C4,$C1,$79,$10,$1C,$00;{$ENDIF} 
       {$IFDEF AVXSUP}vaddpd xmm1, xmm1, xmm3;                        {$ELSE}db $C5,$F1,$58,$CB;{$ENDIF} 
   add rax, r9;
   jnz @addforyloop2;

   // build result
   {$IFDEF AVXSUP}vdivpd xmm0, xmm1, xmm2;                            {$ELSE}db $C5,$F1,$5E,$C2;{$ENDIF} 

   // calculate final standard deviation
   // for y := 0 to height - 1;
   // prepare for reverse loop
   {$IFDEF AVXSUP}vxorpd xmm4, xmm4, xmm4;                            {$ELSE}db $C5,$D9,$57,$E4;{$ENDIF} 
   mov rax, r10;
   @addforyloop2_2:
      {$IFDEF AVXSUP}vmovupd xmm1, [r8 + rax];                        {$ELSE}db $C4,$C1,$79,$10,$0C,$00;{$ENDIF} 
      {$IFDEF AVXSUP}vsubpd xmm1, xmm1, xmm0;                         {$ELSE}db $C5,$F1,$5C,$C8;{$ENDIF} 
      {$IFDEF AVXSUP}vmulpd xmm1, xmm1, xmm1;                         {$ELSE}db $C5,$F1,$59,$C9;{$ENDIF} 
      {$IFDEF AVXSUP}vaddpd xmm4, xmm4, xmm1;                         {$ELSE}db $C5,$D9,$58,$E1;{$ENDIF} 
   add rax, r9;
   jnz @addforyloop2_2;

   // check if we need to use the unbiased version
   cmp unbiased, 0;
   jz @@dobiased_2;

   {$IFDEF AVXSUP}vsubpd xmm3, xmm2, xmm5;                            {$ELSE}db $C5,$E9,$5C,$DD;{$ENDIF} 
   {$IFDEF AVXSUP}vmaxpd xmm3, xmm3, xmm5;                            {$ELSE}db $C5,$E1,$5F,$DD;{$ENDIF} 

   {$IFDEF AVXSUP}vdivpd xmm4, xmm4, xmm3;                            {$ELSE}db $C5,$D9,$5E,$E3;{$ENDIF} 

   jmp @@writeRes2;

   @@dobiased_2:
   {$IFDEF AVXSUP}vdivpd xmm4, xmm4, xmm2;                            {$ELSE}db $C5,$D9,$5E,$E2;{$ENDIF} 

   // write result
   @@writeRes2:
   {$IFDEF AVXSUP}vmovupd [rcx], xmm4;                                {$ELSE}db $C5,$F9,$11,$21;{$ENDIF} 

   // next columns:
   add rcx, 16;
   add r8, 16;

   dec r11;
   jnz @@endProc;

   @@lastcolumn:

   {$IFDEF AVXSUP}vxorpd xmm1, xmm1, xmm1;                            {$ELSE}db $C5,$F1,$57,$C9;{$ENDIF} 

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov rax, r10;
   @addforyloop_1:
       {$IFDEF AVXSUP}vaddsd xmm1, xmm1, [r8 + rax];                  {$ELSE}db $C4,$C1,$73,$58,$0C,$00;{$ENDIF} 
   add rax, r9;
   jnz @addforyloop_1;

   // build result
   {$IFDEF AVXSUP}vdivsd xmm0, xmm1, xmm2;                            {$ELSE}db $C5,$F3,$5E,$C2;{$ENDIF} 

   // calculate final standard deviation
   // for y := 0 to height - 1;
   // prepare for reverse loop
   {$IFDEF AVXSUP}vxorpd xmm4, xmm4, xmm4;                            {$ELSE}db $C5,$D9,$57,$E4;{$ENDIF} 
   mov rax, r10;
   @addforyloop1_2:
      {$IFDEF AVXSUP}vmovsd xmm1, [r8 + rax];                         {$ELSE}db $C4,$C1,$7B,$10,$0C,$00;{$ENDIF} 
      {$IFDEF AVXSUP}vsubsd xmm1, xmm1, xmm0;                         {$ELSE}db $C5,$F3,$5C,$C8;{$ENDIF} 
      {$IFDEF AVXSUP}vmulsd xmm1, xmm1, xmm1;                         {$ELSE}db $C5,$F3,$59,$C9;{$ENDIF} 
      {$IFDEF AVXSUP}vaddsd xmm4, xmm4, xmm1;                         {$ELSE}db $C5,$DB,$58,$E1;{$ENDIF} 
   add rax, r9;
   jnz @addforyloop1_2;

   // check if we need to use the unbiased version
   cmp unbiased, 0;
   jz @@dobiased_2;

   {$IFDEF AVXSUP}vsubsd xmm3, xmm2, xmm5;                            {$ELSE}db $C5,$EB,$5C,$DD;{$ENDIF} 
   {$IFDEF AVXSUP}vmaxsd xmm3, xmm3, xmm5;                            {$ELSE}db $C5,$E3,$5F,$DD;{$ENDIF} 

   {$IFDEF AVXSUP}vdivsd xmm4, xmm4, xmm3;                            {$ELSE}db $C5,$DB,$5E,$E3;{$ENDIF} 

   jmp @@writeRes_1;

   @@dobiased_1:
   {$IFDEF AVXSUP}vdivsd xmm4, xmm4, xmm2;                            {$ELSE}db $C5,$DB,$5E,$E2;{$ENDIF} 

   // write result
   @@writeRes_1:
   {$IFDEF AVXSUP}vmovsd [rcx], xmm4;                                 {$ELSE}db $C5,$FB,$11,$21;{$ENDIF} 

   @@endProc:

   {$IFDEF AVXSUP}vmovupd xmm4, [rsp + $00];                          {$ELSE}db $C5,$F9,$10,$24,$24;{$ENDIF} 
   {$IFDEF AVXSUP}vmovupd xmm5, [rsp + $10];                          {$ELSE}db $C5,$F9,$10,$6C,$24,$10;{$ENDIF} 
   add rsp, $20;
   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;

// #####################################################
// #### Combined mean variance calculation
// #####################################################

procedure AVXMatrixMeanVarRowAligned(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt; unbiased : boolean); {$IFDEF FPC}assembler;{$ENDIF}
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

   // note: RCX = dest, RDX = destLineWidth, R8 = src, R9 = srcLineWidth

   // prolog
   sub rsp, $10;
   {$IFDEF AVXSUP}vmovupd [rsp + $00], xmm4;                          {$ELSE}db $C5,$F9,$11,$24,$24;{$ENDIF} 

   // iters := -width*sizeof(double)
   mov r10, width;
   imul r10, -8;

   // helper registers for the mt1, mt2 and dest pointers
   sub r8, r10;

   // fpc seems to have a problem with this opcode
   {$IFDEF FPC}
   mov rax, width;
   {$IFDEF AVXSUP}vcvtsi2sd xmm3, xmm3, rax;                          {$ELSE}db $C4,$E1,$E3,$2A,$D8;{$ENDIF} 
   {$ELSE}
   cvtsi2sd xmm3, width;
   {$ENDIF}

   // for y := 0 to height - 1:
   mov r11, Height;
   @@addforyloop:
       {$IFDEF AVXSUP}vxorpd ymm0, ymm0, ymm0;                        {$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} 
       {$IFDEF AVXSUP}vxorpd ymm1, ymm1, ymm1;                        {$ELSE}db $C5,$F5,$57,$C9;{$ENDIF} 

       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;
           // prefetch data...
           // prefetch [r8 + rax];

           // addition:
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, [r8 + rax - 128];        {$ELSE}db $C4,$C1,$7D,$58,$44,$00,$80;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm1, ymm1, [r8 + rax - 96];         {$ELSE}db $C4,$C1,$75,$58,$4C,$00,$A0;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, [r8 + rax - 64];         {$ELSE}db $C4,$C1,$7D,$58,$44,$00,$C0;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm1, ymm1, [r8 + rax - 32];         {$ELSE}db $C4,$C1,$75,$58,$4C,$00,$E0;{$ENDIF} 
       jmp @addforxloop

       @loopEnd:

       {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm1;                        {$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 
       {$IFDEF AVXSUP}vextractf128 xmm2, ymm0, 1;                     {$ELSE}db $C4,$E3,$7D,$19,$C2,$01;{$ENDIF} 
       {$IFDEF AVXSUP}vhaddpd xmm0, xmm0, xmm2;                       {$ELSE}db $C5,$F9,$7C,$C2;{$ENDIF} 

       sub rax, 128;

       jz @buildRes;

       @addforxloop2:
           add rax, 16;
           jg @@addforxloop2end;

           {$IFDEF AVXSUP}vaddpd xmm0, xmm0, [r8 + rax - 16];         {$ELSE}db $C4,$C1,$79,$58,$44,$00,$F0;{$ENDIF} 
       jmp @addforxloop2;

       @@addforxloop2end:

       sub rax, 16;
       jz @buildRes;

       {$IFDEF AVXSUP}vaddsd xmm0, xmm0, [r8 + rax];                  {$ELSE}db $C4,$C1,$7B,$58,$04,$00;{$ENDIF} 

       @buildRes:

       // build result
       {$IFDEF AVXSUP}vhaddpd xmm0, xmm0, xmm0;                       {$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} 
       {$IFDEF AVXSUP}vdivsd xmm0, xmm0, xmm3;                        {$ELSE}db $C5,$FB,$5E,$C3;{$ENDIF} 
       {$IFDEF AVXSUP}vmovsd [rcx], xmm0;                             {$ELSE}db $C5,$FB,$11,$01;{$ENDIF} 

       // we have calculated the mean ->
       // repeat the loop to calculate the variance
       {$IFDEF AVXSUP}vbroadcastsd ymm4, [rcx];                       {$ELSE}db $C4,$E2,$7D,$19,$21;{$ENDIF} // need avx2
       {$IFDEF AVXSUP}vxorpd xmm0, xmm0, xmm0;                        {$ELSE}db $C5,$F9,$57,$C0;{$ENDIF} 

       // for x := 0 to w - 1;
       // prepare for reverse loop
       // variance = sum (x - mean)^2
       mov rax, r10;
       @addforxloop3:
           add rax, 128;
           jg @loopEnd2;
           // prefetch data...
           // prefetch [ecx + rax];

           // addition:
           {$IFDEF AVXSUP}vmovapd ymm1, [r8 + rax - 128];             {$ELSE}db $C4,$C1,$7D,$28,$4C,$00,$80;{$ENDIF} 
           {$IFDEF AVXSUP}vsubpd ymm1, ymm1, ymm4;                    {$ELSE}db $C5,$F5,$5C,$CC;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm1, ymm1, ymm1;                    {$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm1;                    {$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 

           {$IFDEF AVXSUP}vmovapd ymm1, [r8 + rax - 96];              {$ELSE}db $C4,$C1,$7D,$28,$4C,$00,$A0;{$ENDIF} 
           {$IFDEF AVXSUP}vsubpd ymm1, ymm1, ymm4;                    {$ELSE}db $C5,$F5,$5C,$CC;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm1, ymm1, ymm1;                    {$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm1;                    {$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 

           {$IFDEF AVXSUP}vmovapd ymm1, [r8 + rax - 64];              {$ELSE}db $C4,$C1,$7D,$28,$4C,$00,$C0;{$ENDIF} 
           {$IFDEF AVXSUP}vsubpd ymm1, ymm1, ymm4;                    {$ELSE}db $C5,$F5,$5C,$CC;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm1, ymm1, ymm1;                    {$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm1;                    {$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 

           {$IFDEF AVXSUP}vmovapd ymm1, [r8 + rax - 32];              {$ELSE}db $C4,$C1,$7D,$28,$4C,$00,$E0;{$ENDIF} 
           {$IFDEF AVXSUP}vsubpd ymm1, ymm1, ymm4;                    {$ELSE}db $C5,$F5,$5C,$CC;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm1, ymm1, ymm1;                    {$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm1;                    {$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 

       jmp @addforxloop3

       @loopEnd2:

       {$IFDEF AVXSUP}vextractf128 xmm2, ymm0, 1;                     {$ELSE}db $C4,$E3,$7D,$19,$C2,$01;{$ENDIF} 
       {$IFDEF AVXSUP}vhaddpd xmm0, xmm0, xmm2;                       {$ELSE}db $C5,$F9,$7C,$C2;{$ENDIF} 

       sub rax, 128;
       jz @buildRes2;

       @addforxloop4:
           add rax, 16;
           jg @addforxloop4end;

           {$IFDEF AVXSUP}vmovapd xmm1, [r8 + rax - 16];              {$ELSE}db $C4,$C1,$79,$28,$4C,$00,$F0;{$ENDIF} 
           {$IFDEF AVXSUP}vsubpd xmm1, xmm1, xmm4;                    {$ELSE}db $C5,$F1,$5C,$CC;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd xmm1, xmm1, xmm1;                    {$ELSE}db $C5,$F1,$59,$C9;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd xmm0, xmm0, xmm1;                    {$ELSE}db $C5,$F9,$58,$C1;{$ENDIF} 
       jmp @addforxloop4;

       @addforxloop4end:

       sub rax, 16;
       jz @buildRes2;

       // last column
       {$IFDEF AVXSUP}vmovsd xmm1, [r8 + rax];                        {$ELSE}db $C4,$C1,$7B,$10,$0C,$00;{$ENDIF} 
       {$IFDEF AVXSUP}vsubsd xmm1, xmm1, xmm4;                        {$ELSE}db $C5,$F3,$5C,$CC;{$ENDIF} 
       {$IFDEF AVXSUP}vmulsd xmm1, xmm1, xmm1;                        {$ELSE}db $C5,$F3,$59,$C9;{$ENDIF} 
       {$IFDEF AVXSUP}vaddsd xmm0, xmm0, xmm1;                        {$ELSE}db $C5,$FB,$58,$C1;{$ENDIF} 


       @buildRes2:

       // build result
       {$IFDEF AVXSUP}vhaddpd xmm0, xmm0, xmm0;                       {$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} 

       // check if we need to use the unbiased version
       cmp unbiased, 0;
       jz @@dobiased;

       lea rax, [rip + cOne];
       {$IFDEF AVXSUP}vmovsd xmm2, [rax];                             {$ELSE}db $C5,$FB,$10,$10;{$ENDIF} 
       {$IFDEF AVXSUP}vsubsd xmm4, xmm3, xmm2;                        {$ELSE}db $C5,$E3,$5C,$E2;{$ENDIF} 
       {$IFDEF AVXSUP}vmaxsd xmm4, xmm4, xmm2;                        {$ELSE}db $C5,$DB,$5F,$E2;{$ENDIF} 

       {$IFDEF AVXSUP}vdivsd xmm0, xmm0, xmm4;                        {$ELSE}db $C5,$FB,$5E,$C4;{$ENDIF} 

       jmp @@writeRes;

       @@dobiased:
       {$IFDEF AVXSUP}vdivsd xmm0, xmm0, xmm3;                        {$ELSE}db $C5,$FB,$5E,$C3;{$ENDIF} 

       // write result
       @@writeRes:
       {$IFDEF AVXSUP}vmovsd [rcx + 8], xmm0;                         {$ELSE}db $C5,$FB,$11,$41,$08;{$ENDIF} 

       // next line:
       add r8, r9;
       add rcx, rdx;

   // loop y end
   dec r11;
   jnz @@addforyloop;

   // epilog
   {$IFDEF AVXSUP}vmovupd xmm4, [rsp + $00];                          {$ELSE}db $C5,$F9,$10,$24,$24;{$ENDIF} 
   add rsp, $10;
   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;

procedure AVXMatrixMeanVarRowUnAligned(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt; unbiased : boolean); {$IFDEF FPC}assembler;{$ENDIF}
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

   // note: RCX = dest, RDX = destLineWidth, R8 = src, R9 = srcLineWidth

   // prolog
   sub rsp, $10;
   {$IFDEF AVXSUP}vmovupd [rsp + $00], xmm4;                          {$ELSE}db $C5,$F9,$11,$24,$24;{$ENDIF} 

   // iters := -width*sizeof(double)
   mov r10, width;
   imul r10, -8;

   // helper registers for the mt1, mt2 and dest pointers
   sub r8, r10;

   // fpc seems to have a problem with this opcode
   {$IFDEF FPC}
   mov rax, width;
   {$IFDEF AVXSUP}vcvtsi2sd xmm3, xmm3, rax;                          {$ELSE}db $C4,$E1,$E3,$2A,$D8;{$ENDIF} 
   {$ELSE}
   cvtsi2sd xmm3, width;
   {$ENDIF}

   // for y := 0 to height - 1:
   mov r11, Height;
   @@addforyloop:
       {$IFDEF AVXSUP}vxorpd ymm0, ymm0, ymm0;                        {$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} 
       {$IFDEF AVXSUP}vxorpd ymm1, ymm1, ymm1;                        {$ELSE}db $C5,$F5,$57,$C9;{$ENDIF} 

       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;
           // prefetch data...
           // prefetch [r8 + rax];

           // addition:
           {$IFDEF AVXSUP}vmovupd ymm2, [r8 + rax - 128];             {$ELSE}db $C4,$C1,$7D,$10,$54,$00,$80;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm2;                    {$ELSE}db $C5,$FD,$58,$C2;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd ymm2, [r8 + rax - 96];              {$ELSE}db $C4,$C1,$7D,$10,$54,$00,$A0;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm1, ymm1, ymm2;                    {$ELSE}db $C5,$F5,$58,$CA;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd ymm2, [r8 + rax - 64];              {$ELSE}db $C4,$C1,$7D,$10,$54,$00,$C0;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm2;                    {$ELSE}db $C5,$FD,$58,$C2;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd ymm2, [r8 + rax - 32];              {$ELSE}db $C4,$C1,$7D,$10,$54,$00,$E0;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm1, ymm1, ymm2;                    {$ELSE}db $C5,$F5,$58,$CA;{$ENDIF} 
       jmp @addforxloop

       @loopEnd:

       {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm1;                        {$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 
       {$IFDEF AVXSUP}vextractf128 xmm2, ymm0, 1;                     {$ELSE}db $C4,$E3,$7D,$19,$C2,$01;{$ENDIF} 
       {$IFDEF AVXSUP}vhaddpd xmm0, xmm0, xmm2;                       {$ELSE}db $C5,$F9,$7C,$C2;{$ENDIF} 

       sub rax, 128;

       jz @buildRes;

       @addforxloop2:
           add rax, 16;
           jg @@addforxloop2end;

           {$IFDEF AVXSUP}vmovupd xmm2, [r8 + rax - 16];              {$ELSE}db $C4,$C1,$79,$10,$54,$00,$F0;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd xmm0, xmm0, xmm2;                    {$ELSE}db $C5,$F9,$58,$C2;{$ENDIF} 
       jmp @addforxloop2;

       @@addforxloop2end:

       sub rax, 16;
       jz @buildRes;

       {$IFDEF AVXSUP}vaddsd xmm0, xmm0, [r8 + rax];                  {$ELSE}db $C4,$C1,$7B,$58,$04,$00;{$ENDIF} 

       @buildRes:

       // build result
       {$IFDEF AVXSUP}vhaddpd xmm0, xmm0, xmm0;                       {$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} 
       {$IFDEF AVXSUP}vdivsd xmm0, xmm0, xmm3;                        {$ELSE}db $C5,$FB,$5E,$C3;{$ENDIF} 
       {$IFDEF AVXSUP}vmovsd [rcx], xmm0;                             {$ELSE}db $C5,$FB,$11,$01;{$ENDIF} 

       // we have calculated the mean ->
       // repeat the loop to calculate the variance

       {$IFDEF AVXSUP}vbroadcastsd ymm4, [rcx];                       {$ELSE}db $C4,$E2,$7D,$19,$21;{$ENDIF} // need avx2
       {$IFDEF AVXSUP}vxorpd xmm0, xmm0, xmm0;                        {$ELSE}db $C5,$F9,$57,$C0;{$ENDIF} 

       // for x := 0 to w - 1;
       // prepare for reverse loop
       // variance = sum (x - mean)^2
       mov rax, r10;
       @addforxloop3:
           add rax, 128;
           jg @loopEnd2;
           // prefetch data...
           // prefetch [ecx + rax];

           // addition:
           {$IFDEF AVXSUP}vmovupd ymm1, [r8 + rax - 128];             {$ELSE}db $C4,$C1,$7D,$10,$4C,$00,$80;{$ENDIF} 
           {$IFDEF AVXSUP}vsubpd ymm1, ymm1, ymm4;                    {$ELSE}db $C5,$F5,$5C,$CC;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm1, ymm1, ymm1;                    {$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm1;                    {$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd ymm1, [r8 + rax - 96];              {$ELSE}db $C4,$C1,$7D,$10,$4C,$00,$A0;{$ENDIF} 
           {$IFDEF AVXSUP}vsubpd ymm1, ymm1, ymm4;                    {$ELSE}db $C5,$F5,$5C,$CC;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm1, ymm1, ymm1;                    {$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm1;                    {$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd ymm1, [r8 + rax - 64];              {$ELSE}db $C4,$C1,$7D,$10,$4C,$00,$C0;{$ENDIF} 
           {$IFDEF AVXSUP}vsubpd ymm1, ymm1, ymm4;                    {$ELSE}db $C5,$F5,$5C,$CC;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm1, ymm1, ymm1;                    {$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm1;                    {$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd ymm1, [r8 + rax - 32];              {$ELSE}db $C4,$C1,$7D,$10,$4C,$00,$E0;{$ENDIF} 
           {$IFDEF AVXSUP}vsubpd ymm1, ymm1, ymm4;                    {$ELSE}db $C5,$F5,$5C,$CC;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm1, ymm1, ymm1;                    {$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm1;                    {$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 

       jmp @addforxloop3

       @loopEnd2:

       sub rax, 128;

       {$IFDEF AVXSUP}vextractf128 xmm2, ymm0, 1;                     {$ELSE}db $C4,$E3,$7D,$19,$C2,$01;{$ENDIF} 
       {$IFDEF AVXSUP}vhaddpd xmm0, xmm0, xmm2;                       {$ELSE}db $C5,$F9,$7C,$C2;{$ENDIF} 

       jz @buildRes2;

       @addforxloop4:
           add rax, 16;
           jg @addforxloop4end;

           {$IFDEF AVXSUP}vmovupd xmm1, [r8 + rax - 16];              {$ELSE}db $C4,$C1,$79,$10,$4C,$00,$F0;{$ENDIF} 
           {$IFDEF AVXSUP}vsubpd xmm1, xmm1, xmm4;                    {$ELSE}db $C5,$F1,$5C,$CC;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd xmm1, xmm1, xmm1;                    {$ELSE}db $C5,$F1,$59,$C9;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd xmm0, xmm0, xmm1;                    {$ELSE}db $C5,$F9,$58,$C1;{$ENDIF} 
       jmp @addforxloop4;

       @addforxloop4end:

       sub rax, 16;
       jz @buildRes2;

       // last column
       {$IFDEF AVXSUP}vmovsd xmm1, [r8 + rax];                        {$ELSE}db $C4,$C1,$7B,$10,$0C,$00;{$ENDIF} 
       {$IFDEF AVXSUP}vsubsd xmm1, xmm1, xmm4;                        {$ELSE}db $C5,$F3,$5C,$CC;{$ENDIF} 
       {$IFDEF AVXSUP}vmulsd xmm1, xmm1, xmm1;                        {$ELSE}db $C5,$F3,$59,$C9;{$ENDIF} 
       {$IFDEF AVXSUP}vaddsd xmm0, xmm0, xmm1;                        {$ELSE}db $C5,$FB,$58,$C1;{$ENDIF} 


       @buildRes2:

       // build result
       {$IFDEF AVXSUP}vhaddpd xmm0, xmm0, xmm0;                       {$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} 

       // check if we need to use the unbiased version
       cmp unbiased, 0;
       jz @@dobiased;

       lea rax, [rip + cOne];
       {$IFDEF AVXSUP}vmovsd xmm2, [rax];                             {$ELSE}db $C5,$FB,$10,$10;{$ENDIF} 
       {$IFDEF AVXSUP}vsubsd xmm4, xmm3, xmm2;                        {$ELSE}db $C5,$E3,$5C,$E2;{$ENDIF} 
       {$IFDEF AVXSUP}vmaxsd xmm4, xmm4, xmm2;                        {$ELSE}db $C5,$DB,$5F,$E2;{$ENDIF} 

       {$IFDEF AVXSUP}vdivsd xmm0, xmm0, xmm4;                        {$ELSE}db $C5,$FB,$5E,$C4;{$ENDIF} 

       jmp @@writeRes;

       @@dobiased:
       {$IFDEF AVXSUP}vdivsd xmm0, xmm0, xmm3;                        {$ELSE}db $C5,$FB,$5E,$C3;{$ENDIF} 

       // write result
       @@writeRes:
       {$IFDEF AVXSUP}vmovsd [rcx + 8], xmm0;                         {$ELSE}db $C5,$FB,$11,$41,$08;{$ENDIF} 

       // next line:
       add r8, r9;
       add rcx, rdx;

   // loop y end
   dec r11;
   jnz @@addforyloop;

   // epilog
   {$IFDEF AVXSUP}vmovupd xmm4, [rsp + $00];                          {$ELSE}db $C5,$F9,$10,$24,$24;{$ENDIF} 
   add rsp, $10;
   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;

procedure AVXMatrixMeanVarColumnAligned(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt; unbiased : boolean); {$IFDEF FPC}assembler;{$ENDIF}
var tmp : double;
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

   // prolog
   sub rsp, $20;
   {$IFDEF AVXSUP}vmovupd [rsp + $00], xmm4;                          {$ELSE}db $C5,$F9,$11,$24,$24;{$ENDIF} 
   {$IFDEF AVXSUP}vmovupd [rsp + $10], xmm5;                          {$ELSE}db $C5,$F9,$11,$6C,$24,$10;{$ENDIF} 

   // note: RCX = dest, RDX = destLineWidth, R8 = src, R9 = srcLineWidth
   xor r10, r10;
   sub r10, height;
   imul r10, r9;

   // helper registers for the mt1, mt2 and dest pointers
   sub r8, r10;

   // fpc seems to have a problem with this opcode
   {$IFDEF FPC}
   mov rax, height;
   {$IFDEF AVXSUP}vcvtsi2sd xmm0, xmm0, rax;                          {$ELSE}db $C4,$E1,$FB,$2A,$C0;{$ENDIF} 
   {$ELSE}
   cvtsi2sd xmm0, height;
   {$ENDIF}
   lea rax, tmp;
   {$IFDEF AVXSUP}vmovsd [rax], xmm0;                                 {$ELSE}db $C5,$FB,$11,$00;{$ENDIF} 
   {$IFDEF AVXSUP}vbroadcastsd ymm2, [rax];                           {$ELSE}db $C4,$E2,$7D,$19,$10;{$ENDIF} // vbroadcastsd ymm2, xmm0;  // avx2

   lea rax, [rip + cOne];
   {$IFDEF AVXSUP}vbroadcastsd ymm5, [rax];                           {$ELSE}db $C4,$E2,$7D,$19,$28;{$ENDIF} 


   // for x := 0 to width - 1:
   mov r11, Width;
   sub r11, 4;
   jl @@addforxloop4end;

   // 4 columns at once
   @@addforxloop4:
       {$IFDEF AVXSUP}vxorpd ymm1, ymm1, ymm1;                        {$ELSE}db $C5,$F5,$57,$C9;{$ENDIF} 

       // for y := 0 to height - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforyloop4:
           {$IFDEF AVXSUP}vaddpd ymm1, ymm1, [r8 + rax];              {$ELSE}db $C4,$C1,$75,$58,$0C,$00;{$ENDIF} 
       add rax, r9;
       jnz @addforyloop4;

       // build result
       {$IFDEF AVXSUP}vdivpd ymm0, ymm1, ymm2;                        {$ELSE}db $C5,$F5,$5E,$C2;{$ENDIF} 
       {$IFDEF AVXSUP}vmovapd [rcx], ymm0;                            {$ELSE}db $C5,$FD,$29,$01;{$ENDIF} 

       // calculate final standard deviation
       // for y := 0 to height - 1;
       // prepare for reverse loop
       {$IFDEF AVXSUP}vxorpd ymm4, ymm4, ymm4;                        {$ELSE}db $C5,$DD,$57,$E4;{$ENDIF} 
       mov rax, r10;
       @addforyloop4_2:
           {$IFDEF AVXSUP}vmovapd ymm1, [r8 + rax];                   {$ELSE}db $C4,$C1,$7D,$28,$0C,$00;{$ENDIF} 
           {$IFDEF AVXSUP}vsubpd ymm1, ymm1, ymm0;                    {$ELSE}db $C5,$F5,$5C,$C8;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm1, ymm1, ymm1;                    {$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm4, ymm4, ymm1;                    {$ELSE}db $C5,$DD,$58,$E1;{$ENDIF} 
       add rax, r9;
       jnz @addforyloop4_2;

       // check if we need to use the unbiased version
       cmp unbiased, 0;
       jz @@dobiased_4;

       lea rax, [rip + cOne];

       {$IFDEF AVXSUP}vbroadcastsd ymm1, [rax];                       {$ELSE}db $C4,$E2,$7D,$19,$08;{$ENDIF} 
       {$IFDEF AVXSUP}vsubpd ymm3, ymm2, ymm1;                        {$ELSE}db $C5,$ED,$5C,$D9;{$ENDIF} 
       {$IFDEF AVXSUP}vmaxpd ymm3, ymm3, ymm1;                        {$ELSE}db $C5,$E5,$5F,$D9;{$ENDIF} 

       {$IFDEF AVXSUP}vdivpd ymm4, ymm4, ymm3;                        {$ELSE}db $C5,$DD,$5E,$E3;{$ENDIF} 

       jmp @@writeRes_4;

       @@dobiased_4:
       {$IFDEF AVXSUP}vdivpd ymm4, ymm4, ymm2;                        {$ELSE}db $C5,$DD,$5E,$E2;{$ENDIF} 

       // write result
       @@writeRes_4:
       {$IFDEF AVXSUP}vmovapd [rcx + rdx], ymm4;                      {$ELSE}db $C5,$FD,$29,$24,$11;{$ENDIF} 

       // next columns:
       add rcx, 32;
       add r8, 32;

   // loop x end
   sub r11, 4;
   jge @@addforxloop4;

   @@addforxloop4end:

   add r11, 4;
   jz @@endProc;

   sub r11, 2;
   jl @@lastcolumn;

   {$IFDEF AVXSUP}vxorpd xmm1, xmm1, xmm1;                            {$ELSE}db $C5,$F1,$57,$C9;{$ENDIF} 

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov rax, r10;
   @addforyloop2:
       {$IFDEF AVXSUP}vaddpd xmm1, xmm1, [r8 + rax];                  {$ELSE}db $C4,$C1,$71,$58,$0C,$00;{$ENDIF} 
   add rax, r9;
   jnz @addforyloop2;

   // build result
   {$IFDEF AVXSUP}vdivpd xmm0, xmm1, xmm2;                            {$ELSE}db $C5,$F1,$5E,$C2;{$ENDIF} 
   {$IFDEF AVXSUP}vmovapd [rcx], xmm0;                                {$ELSE}db $C5,$F9,$29,$01;{$ENDIF} 

   // calculate final standard deviation
   // for y := 0 to height - 1;
   // prepare for reverse loop
   {$IFDEF AVXSUP}vxorpd xmm4, xmm4, xmm4;                            {$ELSE}db $C5,$D9,$57,$E4;{$ENDIF} 
   mov rax, r10;
   @addforyloop2_2:
      {$IFDEF AVXSUP}vmovapd xmm1, [r8 + rax];                        {$ELSE}db $C4,$C1,$79,$28,$0C,$00;{$ENDIF} 
      {$IFDEF AVXSUP}vsubpd xmm1, xmm1, xmm0;                         {$ELSE}db $C5,$F1,$5C,$C8;{$ENDIF} 
      {$IFDEF AVXSUP}vmulpd xmm1, xmm1, xmm1;                         {$ELSE}db $C5,$F1,$59,$C9;{$ENDIF} 
      {$IFDEF AVXSUP}vaddpd xmm4, xmm4, xmm1;                         {$ELSE}db $C5,$D9,$58,$E1;{$ENDIF} 
   add rax, r9;
   jnz @addforyloop2_2;

   // check if we need to use the unbiased version
   cmp unbiased, 0;
   jz @@dobiased_2;

   {$IFDEF AVXSUP}vsubpd xmm3, xmm2, xmm5;                            {$ELSE}db $C5,$E9,$5C,$DD;{$ENDIF} 
   {$IFDEF AVXSUP}vmaxpd xmm3, xmm3, xmm5;                            {$ELSE}db $C5,$E1,$5F,$DD;{$ENDIF} 

   {$IFDEF AVXSUP}vdivpd xmm4, xmm4, xmm3;                            {$ELSE}db $C5,$D9,$5E,$E3;{$ENDIF} 

   jmp @@writeRes2;

   @@dobiased_2:
   {$IFDEF AVXSUP}vdivpd xmm4, xmm4, xmm2;                            {$ELSE}db $C5,$D9,$5E,$E2;{$ENDIF} 

   // write result
   @@writeRes2:
   {$IFDEF AVXSUP}vmovapd [rcx + rdx], xmm4;                          {$ELSE}db $C5,$F9,$29,$24,$11;{$ENDIF} 

   // next columns:
   add rcx, 16;
   add r8, 16;

   dec r11;
   jnz @@endProc;

   @@lastcolumn:

   {$IFDEF AVXSUP}vxorpd xmm1, xmm1, xmm1;                            {$ELSE}db $C5,$F1,$57,$C9;{$ENDIF} 

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov rax, r10;
   @addforyloop_1:
       {$IFDEF AVXSUP}vaddsd xmm1, xmm1, [r8 + rax];                  {$ELSE}db $C4,$C1,$73,$58,$0C,$00;{$ENDIF} 
   add rax, r9;
   jnz @addforyloop_1;

   // build result
   {$IFDEF AVXSUP}vdivsd xmm0, xmm1, xmm2;                            {$ELSE}db $C5,$F3,$5E,$C2;{$ENDIF} 
   {$IFDEF AVXSUP}vmovsd [rcx], xmm0;                                 {$ELSE}db $C5,$FB,$11,$01;{$ENDIF} 

   // calculate final standard deviation
   // for y := 0 to height - 1;
   // prepare for reverse loop
   {$IFDEF AVXSUP}vxorpd xmm4, xmm4, xmm4;                            {$ELSE}db $C5,$D9,$57,$E4;{$ENDIF} 
   mov rax, r10;
   @addforyloop2_1:
      {$IFDEF AVXSUP}vmovsd xmm1, [r8 + rax];                         {$ELSE}db $C4,$C1,$7B,$10,$0C,$00;{$ENDIF} 
      {$IFDEF AVXSUP}vsubsd xmm1, xmm1, xmm0;                         {$ELSE}db $C5,$F3,$5C,$C8;{$ENDIF} 
      {$IFDEF AVXSUP}vmulsd xmm1, xmm1, xmm1;                         {$ELSE}db $C5,$F3,$59,$C9;{$ENDIF} 
      {$IFDEF AVXSUP}vaddsd xmm4, xmm4, xmm1;                         {$ELSE}db $C5,$DB,$58,$E1;{$ENDIF} 
   add rax, r9;
   jnz @addforyloop2_1;

   // check if we need to use the unbiased version
   cmp unbiased, 0;
   jz @@dobiased_1;

   {$IFDEF AVXSUP}vmovsd xmm3, xmm3, xmm2;                            {$ELSE}db $C5,$E3,$11,$D3;{$ENDIF} 
   {$IFDEF AVXSUP}vsubsd xmm3, xmm3, xmm5;                            {$ELSE}db $C5,$E3,$5C,$DD;{$ENDIF} 
   {$IFDEF AVXSUP}vmaxsd xmm3, xmm3, xmm5;                            {$ELSE}db $C5,$E3,$5F,$DD;{$ENDIF} 

   {$IFDEF AVXSUP}vdivsd xmm4, xmm4, xmm3;                            {$ELSE}db $C5,$DB,$5E,$E3;{$ENDIF} 

   jmp @@writeRes_1;

   @@dobiased_1:
   {$IFDEF AVXSUP}vdivsd xmm4, xmm4, xmm2;                            {$ELSE}db $C5,$DB,$5E,$E2;{$ENDIF} 

   // write result
   @@writeRes_1:
   {$IFDEF AVXSUP}vmovsd [rcx + rdx], xmm4;                           {$ELSE}db $C5,$FB,$11,$24,$11;{$ENDIF} 

   @@endProc:

   // epilog
   {$IFDEF AVXSUP}vmovupd xmm4, [rsp + $00];                          {$ELSE}db $C5,$F9,$10,$24,$24;{$ENDIF} 
   {$IFDEF AVXSUP}vmovupd xmm5, [rsp + $10];                          {$ELSE}db $C5,$F9,$10,$6C,$24,$10;{$ENDIF} 
   add rsp, $20;
   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;

procedure AVXMatrixMeanVarColumnUnAligned(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; {$ifdef UNIX}unixWidth{$ELSE}width{$endif}, {$ifdef UNIX}unixHeight{$ELSE}height{$endif} : NativeInt; unbiased : boolean); {$IFDEF FPC}assembler;{$ENDIF}
var tmp : double;
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

   // prolog
   sub rsp, $20;
   {$IFDEF AVXSUP}vmovupd [rsp + $00], xmm4;                          {$ELSE}db $C5,$F9,$11,$24,$24;{$ENDIF} 
   {$IFDEF AVXSUP}vmovupd [rsp + $10], xmm5;                          {$ELSE}db $C5,$F9,$11,$6C,$24,$10;{$ENDIF} 

   // note: RCX = dest, RDX = destLineWidth, R8 = src, R9 = srcLineWidth
   xor r10, r10;
   sub r10, height;
   imul r10, r9;

   // helper registers for the mt1, mt2 and dest pointers
   sub r8, r10;

   // fpc seems to have a problem with this opcode
   {$IFDEF FPC}
   mov rax, height;
   {$IFDEF AVXSUP}vcvtsi2sd xmm0, xmm0, rax;                          {$ELSE}db $C4,$E1,$FB,$2A,$C0;{$ENDIF} 
   {$ELSE}
   cvtsi2sd xmm0, height;
   {$ENDIF}
   lea rax, tmp;
   {$IFDEF AVXSUP}vmovsd [rax], xmm0;                                 {$ELSE}db $C5,$FB,$11,$00;{$ENDIF} 
   {$IFDEF AVXSUP}vbroadcastsd ymm2, [rax];                           {$ELSE}db $C4,$E2,$7D,$19,$10;{$ENDIF} //vbroadcastsd ymm2, xmm0; // avx2

   lea rax, [rip + cOne];
   {$IFDEF AVXSUP}vbroadcastsd ymm5, [rax];                           {$ELSE}db $C4,$E2,$7D,$19,$28;{$ENDIF} 

   // for x := 0 to width - 1:
   mov r11, Width;
   sub r11, 4;
   jl @@addforxloop4end;

   // 4 columns at once
   @@addforxloop4:
       {$IFDEF AVXSUP}vxorpd ymm1, ymm1, ymm1;                        {$ELSE}db $C5,$F5,$57,$C9;{$ENDIF} 

       // for y := 0 to height - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforyloop4:
           {$IFDEF AVXSUP}vmovupd ymm3, [r8 + rax];                   {$ELSE}db $C4,$C1,$7D,$10,$1C,$00;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm1, ymm1, ymm3;                    {$ELSE}db $C5,$F5,$58,$CB;{$ENDIF} 
       add rax, r9;
       jnz @addforyloop4;

       // build result
       {$IFDEF AVXSUP}vdivpd ymm0, ymm1, ymm2;                        {$ELSE}db $C5,$F5,$5E,$C2;{$ENDIF} 
       {$IFDEF AVXSUP}vmovupd [rcx], ymm0;                            {$ELSE}db $C5,$FD,$11,$01;{$ENDIF} 

       // calculate final standard deviation
       // for y := 0 to height - 1;
       // prepare for reverse loop
       {$IFDEF AVXSUP}vxorpd ymm4, ymm4, ymm4;                        {$ELSE}db $C5,$DD,$57,$E4;{$ENDIF} 
       mov rax, r10;
       @addforyloop4_2:
           {$IFDEF AVXSUP}vmovupd ymm1, [r8 + rax];                   {$ELSE}db $C4,$C1,$7D,$10,$0C,$00;{$ENDIF} 
           {$IFDEF AVXSUP}vsubpd ymm1, ymm1, ymm0;                    {$ELSE}db $C5,$F5,$5C,$C8;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm1, ymm1, ymm1;                    {$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm4, ymm4, ymm1;                    {$ELSE}db $C5,$DD,$58,$E1;{$ENDIF} 
       add rax, r9;
       jnz @addforyloop4_2;

       // check if we need to use the unbiased version
       cmp unbiased, 0;
       jz @@dobiased_4;

       {$IFDEF AVXSUP}vsubpd ymm3, ymm2, ymm5;                        {$ELSE}db $C5,$ED,$5C,$DD;{$ENDIF} 
       {$IFDEF AVXSUP}vmaxpd ymm3, ymm3, ymm5;                        {$ELSE}db $C5,$E5,$5F,$DD;{$ENDIF} 

       {$IFDEF AVXSUP}vdivpd ymm4, ymm4, ymm3;                        {$ELSE}db $C5,$DD,$5E,$E3;{$ENDIF} 

       jmp @@writeRes_4;

       @@dobiased_4:
       {$IFDEF AVXSUP}vdivpd ymm4, ymm4, ymm2;                        {$ELSE}db $C5,$DD,$5E,$E2;{$ENDIF} 

       // write result
       @@writeRes_4:
       {$IFDEF AVXSUP}vmovupd [rcx + rdx], ymm4;                      {$ELSE}db $C5,$FD,$11,$24,$11;{$ENDIF} 

       // next columns:
       add rcx, 32;
       add r8, 32;

   // loop x end
   sub r11, 4;
   jge @@addforxloop4;

   @@addforxloop4end:

   add r11, 4;
   jz @@endProc;

   sub r11, 2;
   jl @@lastcolumn;

   {$IFDEF AVXSUP}vxorpd xmm1, xmm1, xmm1;                            {$ELSE}db $C5,$F1,$57,$C9;{$ENDIF} 

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov rax, r10;
   @addforyloop2:
       {$IFDEF AVXSUP}vmovupd xmm3, [r8 + rax];                       {$ELSE}db $C4,$C1,$79,$10,$1C,$00;{$ENDIF} 
       {$IFDEF AVXSUP}vaddpd xmm1, xmm1, xmm3;                        {$ELSE}db $C5,$F1,$58,$CB;{$ENDIF} 
   add rax, r9;
   jnz @addforyloop2;

   // build result
   {$IFDEF AVXSUP}vdivpd xmm0, xmm1, xmm2;                            {$ELSE}db $C5,$F1,$5E,$C2;{$ENDIF} 
   {$IFDEF AVXSUP}vmovupd [rcx], xmm0;                                {$ELSE}db $C5,$F9,$11,$01;{$ENDIF} 

   // calculate final standard deviation
   // for y := 0 to height - 1;
   // prepare for reverse loop
   {$IFDEF AVXSUP}vxorpd xmm4, xmm4, xmm4;                            {$ELSE}db $C5,$D9,$57,$E4;{$ENDIF} 
   mov rax, r10;
   @addforyloop2_2:
      {$IFDEF AVXSUP}vmovupd xmm1, [r8 + rax];                        {$ELSE}db $C4,$C1,$79,$10,$0C,$00;{$ENDIF} 
      {$IFDEF AVXSUP}vsubpd xmm1, xmm1, xmm0;                         {$ELSE}db $C5,$F1,$5C,$C8;{$ENDIF} 
      {$IFDEF AVXSUP}vmulpd xmm1, xmm1, xmm1;                         {$ELSE}db $C5,$F1,$59,$C9;{$ENDIF} 
      {$IFDEF AVXSUP}vaddpd xmm4, xmm4, xmm1;                         {$ELSE}db $C5,$D9,$58,$E1;{$ENDIF} 
   add rax, r9;
   jnz @addforyloop2_2;

   // check if we need to use the unbiased version
   cmp unbiased, 0;
   jz @@dobiased_2;

   {$IFDEF AVXSUP}vsubpd xmm3, xmm2, xmm5;                            {$ELSE}db $C5,$E9,$5C,$DD;{$ENDIF} 
   {$IFDEF AVXSUP}vmaxpd xmm3, xmm3, xmm5;                            {$ELSE}db $C5,$E1,$5F,$DD;{$ENDIF} 

   {$IFDEF AVXSUP}vdivpd xmm4, xmm4, xmm3;                            {$ELSE}db $C5,$D9,$5E,$E3;{$ENDIF} 

   jmp @@writeRes2;

   @@dobiased_2:
   {$IFDEF AVXSUP}vdivpd xmm4, xmm4, xmm2;                            {$ELSE}db $C5,$D9,$5E,$E2;{$ENDIF} 

   // write result
   @@writeRes2:
   {$IFDEF AVXSUP}vmovupd [rcx + rdx], xmm4;                          {$ELSE}db $C5,$F9,$11,$24,$11;{$ENDIF} 

   // next columns:
   add rcx, 16;
   add r8, 16;

   dec r11;
   jnz @@endProc;

   @@lastcolumn:

   {$IFDEF AVXSUP}vxorpd xmm1, xmm1, xmm1;                            {$ELSE}db $C5,$F1,$57,$C9;{$ENDIF} 

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov rax, r10;
   @addforyloop_1:
       {$IFDEF AVXSUP}vaddsd xmm1, xmm1, [r8 + rax];                  {$ELSE}db $C4,$C1,$73,$58,$0C,$00;{$ENDIF} 
   add rax, r9;
   jnz @addforyloop_1;

   // build result
   {$IFDEF AVXSUP}vdivsd xmm0, xmm1, xmm2;                            {$ELSE}db $C5,$F3,$5E,$C2;{$ENDIF} 
   {$IFDEF AVXSUP}vmovsd [rcx], xmm0;                                 {$ELSE}db $C5,$FB,$11,$01;{$ENDIF} 

   // calculate final standard deviation
   // for y := 0 to height - 1;
   // prepare for reverse loop
   {$IFDEF AVXSUP}vxorpd xmm4, xmm4, xmm4;                            {$ELSE}db $C5,$D9,$57,$E4;{$ENDIF} 
   mov rax, r10;
   @addforyloop2_1:
      {$IFDEF AVXSUP}vmovsd xmm1, [r8 + rax];                         {$ELSE}db $C4,$C1,$7B,$10,$0C,$00;{$ENDIF} 
      {$IFDEF AVXSUP}vsubsd xmm1, xmm1, xmm0;                         {$ELSE}db $C5,$F3,$5C,$C8;{$ENDIF} 
      {$IFDEF AVXSUP}vmulsd xmm1, xmm1, xmm1;                         {$ELSE}db $C5,$F3,$59,$C9;{$ENDIF} 
      {$IFDEF AVXSUP}vaddsd xmm4, xmm4, xmm1;                         {$ELSE}db $C5,$DB,$58,$E1;{$ENDIF} 
   add rax, r9;
   jnz @addforyloop2_1;

   // check if we need to use the unbiased version
   cmp unbiased, 0;
   jz @@dobiased_1;

   {$IFDEF AVXSUP}vsubsd xmm3, xmm2, xmm5;                            {$ELSE}db $C5,$EB,$5C,$DD;{$ENDIF} 
   {$IFDEF AVXSUP}vmaxsd xmm3, xmm3, xmm5;                            {$ELSE}db $C5,$E3,$5F,$DD;{$ENDIF} 

   {$IFDEF AVXSUP}vdivsd xmm4, xmm4, xmm3;                            {$ELSE}db $C5,$DB,$5E,$E3;{$ENDIF} 

   jmp @@writeRes_1;

   @@dobiased_1:
   {$IFDEF AVXSUP}vdivsd xmm4, xmm4, xmm2;                            {$ELSE}db $C5,$DB,$5E,$E2;{$ENDIF} 

   // write result
   @@writeRes_1:
   {$IFDEF AVXSUP}vmovsd [rcx + rdx], xmm4;                           {$ELSE}db $C5,$FB,$11,$24,$11;{$ENDIF} 

   @@endProc:

   {$IFDEF AVXSUP}vmovupd xmm4, [rsp + $00];                          {$ELSE}db $C5,$F9,$10,$24,$24;{$ENDIF} 
   {$IFDEF AVXSUP}vmovupd xmm5, [rsp + $10];                          {$ELSE}db $C5,$F9,$10,$6C,$24,$10;{$ENDIF} 
   add rsp, $20;
   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;

{$ENDIF}

end.
