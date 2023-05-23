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


unit AVXMatrixScaleOperationsx64;

interface

{$I 'mrMath_CPU.inc'}

{$IFDEF x64}

procedure AVXMatrixAddscaleAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : NativeInt; {$ifdef UNIX}unixdOffset{$ELSE}dOffset{$endif}, {$ifdef UNIX}unixScale{$ELSE}Scale{$endif} : double); {$IFDEF FPC}assembler;{$ENDIF}
procedure AVXMatrixAddscaleUnAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : NativeInt; {$ifdef UNIX}unixdOffset{$ELSE}dOffset{$endif}, {$ifdef UNIX}unixScale{$ELSE}Scale{$endif} : double); {$IFDEF FPC}assembler;{$ENDIF}

procedure AVXMatrixAddscaleAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : NativeInt; {$ifdef UNIX}unixdOffset{$ELSE}dOffset{$endif}, {$ifdef UNIX}unixScale{$ELSE}Scale{$endif} : double); {$IFDEF FPC}assembler;{$ENDIF}
procedure AVXMatrixAddscaleUnAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : NativeInt; {$ifdef UNIX}unixdOffset{$ELSE}dOffset{$endif}, {$ifdef UNIX}unixScale{$ELSE}Scale{$endif} : double); {$IFDEF FPC}assembler;{$ENDIF}

procedure AVXMatrixScaleAddAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : NativeInt; {$ifdef UNIX}unixdOffset{$ELSE}dOffset{$endif}, {$ifdef UNIX}unixScale{$ELSE}Scale{$endif} : double); {$IFDEF FPC}assembler;{$ENDIF}
procedure AVXMatrixScaleAddUnAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : NativeInt; {$ifdef UNIX}unixdOffset{$ELSE}dOffset{$endif}, {$ifdef UNIX}unixScale{$ELSE}Scale{$endif} : double); {$IFDEF FPC}assembler;{$ENDIF}

procedure AVXMatrixScaleAddAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : NativeInt; {$ifdef UNIX}unixdOffset{$ELSE}dOffset{$endif}, {$ifdef UNIX}unixScale{$ELSE}Scale{$endif} : double); {$IFDEF FPC}assembler;{$ENDIF}
procedure AVXMatrixScaleAddUnAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : NativeInt; {$ifdef UNIX}unixdOffset{$ELSE}dOffset{$endif}, {$ifdef UNIX}unixScale{$ELSE}Scale{$endif} : double); {$IFDEF FPC}assembler;{$ENDIF}

procedure AVXMatrixAddAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : NativeInt; {$ifdef UNIX}unixdOffset{$ELSE}dOffset{$endif} : double); {$IFDEF FPC}assembler;{$ENDIF}
procedure AVXMatrixAddUnAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : NativeInt; {$ifdef UNIX}unixdOffset{$ELSE}dOffset{$endif} : double); {$IFDEF FPC}assembler;{$ENDIF}

procedure AVXMatrixAddAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : NativeInt; {$ifdef UNIX}unixdOffset{$ELSE}dOffset{$endif} : double); {$IFDEF FPC}assembler;{$ENDIF}
procedure AVXMatrixAddUnAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : NativeInt; {$ifdef UNIX}unixdOffset{$ELSE}dOffset{$endif} : double); {$IFDEF FPC}assembler;{$ENDIF}

{$ENDIF}

implementation

{$IFDEF x64}

procedure AVXMatrixAddscaleAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : NativeInt; {$ifdef UNIX}unixdOffset{$ELSE}dOffset{$endif}, {$ifdef UNIX}unixScale{$ELSE}Scale{$endif} : double); {$IFDEF FPC}assembler;{$ENDIF}
{$IFDEF UNIX}
var dOffset, Scale : double;
{$ENDIF}
asm
   {$IFDEF UNIX}
   // Linux uses a diffrent ABI -> copy over the registers so they meet with winABI
   // (note that the 5th and 6th parameter are are on the stack)
   // The parameters are passed in the following order:
   // RDI, RSI, RDX, RCX -> mov to RCX, RDX, R8, R9
   movsd xmm3, unixdOffset;
   movsd dOffset, xmm3;
   movsd xmm3, unixScale;
   movsd scale, xmm3;
   mov r8, rdx;
   mov r9, rcx;
   mov rcx, rdi;
   mov rdx, rsi;
   {$ENDIF}

   // note: RCX = dest, RDX = destLineWidth, R8 = width, R9 = height
   // prolog - simulate stack

   //iters := -width*sizeof(double);
   imul r8, -8;

   lea rax, dOffset;
   {$IFDEF AVXSUP}vbroadcastsd ymm3, [rax];                           {$ELSE}db $C4,$E2,$7D,$19,$18;{$ENDIF} 
   lea rax, scale;
   {$IFDEF AVXSUP}vbroadcastsd ymm1, [rax];                           {$ELSE}db $C4,$E2,$7D,$19,$08;{$ENDIF} 


   // helper registers for the mt1, mt2 and dest pointers
   sub rcx, r8;

   // for y := 0 to height - 1:
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r8;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // prefetchw data...
           // prefetchw [rcx + rax];

           // add mul
           {$IFDEF AVXSUP}vmovapd ymm0, [rcx + rax - 128];            {$ELSE}db $C5,$FD,$28,$44,$01,$80;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm3;                    {$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm0, ymm0, ymm1;                    {$ELSE}db $C5,$FD,$59,$C1;{$ENDIF} 
           {$IFDEF AVXSUP}vmovapd [rcx + rax - 128], ymm0;            {$ELSE}db $C5,$FD,$29,$44,$01,$80;{$ENDIF} 

           {$IFDEF AVXSUP}vmovapd ymm2, [rcx + rax - 96];             {$ELSE}db $C5,$FD,$28,$54,$01,$A0;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm2, ymm2, ymm3;                    {$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm2, ymm2, ymm1;                    {$ELSE}db $C5,$ED,$59,$D1;{$ENDIF} 
           {$IFDEF AVXSUP}vmovapd [rcx + rax - 96], ymm2;             {$ELSE}db $C5,$FD,$29,$54,$01,$A0;{$ENDIF} 

           {$IFDEF AVXSUP}vmovapd ymm0, [rcx + rax - 64];             {$ELSE}db $C5,$FD,$28,$44,$01,$C0;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm3;                    {$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm0, ymm0, ymm1;                    {$ELSE}db $C5,$FD,$59,$C1;{$ENDIF} 
           {$IFDEF AVXSUP}vmovapd [rcx + rax - 64], ymm0;             {$ELSE}db $C5,$FD,$29,$44,$01,$C0;{$ENDIF} 

           {$IFDEF AVXSUP}vmovapd ymm2, [rcx + rax - 32];             {$ELSE}db $C5,$FD,$28,$54,$01,$E0;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm2, ymm2, ymm3;                    {$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm2, ymm2, ymm1;                    {$ELSE}db $C5,$ED,$59,$D1;{$ENDIF} 
           {$IFDEF AVXSUP}vmovapd [rcx + rax - 32], ymm2;             {$ELSE}db $C5,$FD,$29,$54,$01,$E0;{$ENDIF} 

       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           {$IFDEF AVXSUP}vmovapd xmm0, [rcx + rax];                  {$ELSE}db $C5,$F9,$28,$04,$01;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd xmm0, xmm0, xmm3;                    {$ELSE}db $C5,$F9,$58,$C3;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd xmm0, xmm0, xmm1;                    {$ELSE}db $C5,$F9,$59,$C1;{$ENDIF} 

           {$IFDEF AVXSUP}vmovapd [rcx + rax], xmm0;                  {$ELSE}db $C5,$F9,$29,$04,$01;{$ENDIF} 
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // next line:
       add rcx, rdx;

   // loop y end
   dec r9;
   jnz @@addforyloop;

   // epilog
   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;

procedure AVXMatrixAddscaleUnAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : NativeInt; {$ifdef UNIX}unixdOffset{$ELSE}dOffset{$endif}, {$ifdef UNIX}unixScale{$ELSE}Scale{$endif} : double); {$IFDEF FPC}assembler;{$ENDIF}
{$IFDEF UNIX}
var dOffset, scale : double;
{$ENDIF}
asm
   {$IFDEF UNIX}
   // Linux uses a diffrent ABI -> copy over the registers so they meet with winABI
   // (note that the 5th and 6th parameter are are on the stack)
   // The parameters are passed in the following order:
   // RDI, RSI, RDX, RCX -> mov to RCX, RDX, R8, R9
   movsd xmm3, unixdOffset;
   movsd dOffset, xmm3;
   movsd xmm3, unixscale;
   movsd scale, xmm3;
   mov r8, rdx;
   mov r9, rcx;
   mov rcx, rdi;
   mov rdx, rsi;
   {$ENDIF}

   // note: RCX = dest, RDX = destLineWidth, R8 = width, R9 = height
   // prolog - simulate stack

   //iters := -width*sizeof(double);
   imul r8, -8;

   lea rax, dOffset;
   {$IFDEF AVXSUP}vbroadcastsd ymm3, [rax];                           {$ELSE}db $C4,$E2,$7D,$19,$18;{$ENDIF} 
   lea rax, scale;
   {$IFDEF AVXSUP}vbroadcastsd ymm1, [rax];                           {$ELSE}db $C4,$E2,$7D,$19,$08;{$ENDIF} 

   // helper registers for the mt1, mt2 and dest pointers
   sub rcx, r8;

   // for y := 0 to height - 1:
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r8;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // prefetchw data...
           // prefetchw [rcx + rax];

           // add mul
           {$IFDEF AVXSUP}vmovupd ymm0, [rcx + rax - 128];            {$ELSE}db $C5,$FD,$10,$44,$01,$80;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm3;                    {$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm0, ymm0, ymm1;                    {$ELSE}db $C5,$FD,$59,$C1;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd [rcx + rax - 128], ymm0;            {$ELSE}db $C5,$FD,$11,$44,$01,$80;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd ymm2, [rcx + rax - 96];             {$ELSE}db $C5,$FD,$10,$54,$01,$A0;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm2, ymm2, ymm3;                    {$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm2, ymm2, ymm1;                    {$ELSE}db $C5,$ED,$59,$D1;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd [rcx + rax - 96], ymm2;             {$ELSE}db $C5,$FD,$11,$54,$01,$A0;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd ymm0, [rcx + rax - 64];             {$ELSE}db $C5,$FD,$10,$44,$01,$C0;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm3;                    {$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm0, ymm0, ymm1;                    {$ELSE}db $C5,$FD,$59,$C1;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd [rcx + rax - 64], ymm0;             {$ELSE}db $C5,$FD,$11,$44,$01,$C0;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd ymm2, [rcx + rax - 32];             {$ELSE}db $C5,$FD,$10,$54,$01,$E0;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm2, ymm2, ymm3;                    {$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm2, ymm2, ymm1;                    {$ELSE}db $C5,$ED,$59,$D1;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd [rcx + rax - 32], ymm2;             {$ELSE}db $C5,$FD,$11,$54,$01,$E0;{$ENDIF} 

       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           {$IFDEF AVXSUP}vmovupd xmm0, [rcx + rax];                  {$ELSE}db $C5,$F9,$10,$04,$01;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd xmm0, xmm0, xmm3;                    {$ELSE}db $C5,$F9,$58,$C3;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd xmm0, xmm0, xmm1;                    {$ELSE}db $C5,$F9,$59,$C1;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd [rcx + rax], xmm0;                  {$ELSE}db $C5,$F9,$11,$04,$01;{$ENDIF} 
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // next line:
       add rcx, rdx;

   // loop y end
   dec r9;
   jnz @@addforyloop;

   // epilog
   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;


procedure AVXMatrixAddscaleAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : NativeInt; {$ifdef UNIX}unixdOffset{$ELSE}dOffset{$endif}, {$ifdef UNIX}unixScale{$ELSE}Scale{$endif} : double); {$IFDEF FPC}assembler;{$ENDIF}
{$IFDEF UNIX}
var dOffset, scale : double;
{$ENDIF}
asm
   {$IFDEF UNIX}
   // Linux uses a diffrent ABI -> copy over the registers so they meet with winABI
   // (note that the 5th and 6th parameter are are on the stack)
   // The parameters are passed in the following order:
   // RDI, RSI, RDX, RCX -> mov to RCX, RDX, R8, R9
   movsd xmm3, unixdOffset;
   movsd dOffset, xmm3;
   movsd xmm3, unixscale;
   movsd scale, xmm3;
   mov r8, rdx;
   mov r9, rcx;
   mov rcx, rdi;
   mov rdx, rsi;
   {$ENDIF}

   // note: RCX = dest, RDX = destLineWidth, R8 = width, R9 = height
   // prolog - simulate stack

   //iters := -(width - 1)*sizeof(double);
   dec r8;
   imul r8, -8;

   lea rax, dOffset;
   {$IFDEF AVXSUP}vbroadcastsd ymm3, [rax];                           {$ELSE}db $C4,$E2,$7D,$19,$18;{$ENDIF} 
   lea rax, scale;
   {$IFDEF AVXSUP}vbroadcastsd ymm1, [rax];                           {$ELSE}db $C4,$E2,$7D,$19,$08;{$ENDIF} 


   // helper registers for the mt1, mt2 and dest pointers
   sub rcx, r8;

   // for y := 0 to height - 1:
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r8;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // prefetchw data...
           // prefetchw [rcx + rax];

           // add mul
           {$IFDEF AVXSUP}vmovapd ymm0, [rcx + rax - 128];            {$ELSE}db $C5,$FD,$28,$44,$01,$80;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm3;                    {$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm0, ymm0, ymm1;                    {$ELSE}db $C5,$FD,$59,$C1;{$ENDIF} 
           {$IFDEF AVXSUP}vmovapd [rcx + rax - 128], ymm0;            {$ELSE}db $C5,$FD,$29,$44,$01,$80;{$ENDIF} 

           {$IFDEF AVXSUP}vmovapd ymm2, [rcx + rax - 96];             {$ELSE}db $C5,$FD,$28,$54,$01,$A0;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm2, ymm2, ymm3;                    {$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm2, ymm2, ymm1;                    {$ELSE}db $C5,$ED,$59,$D1;{$ENDIF} 
           {$IFDEF AVXSUP}vmovapd [rcx + rax - 96], ymm2;             {$ELSE}db $C5,$FD,$29,$54,$01,$A0;{$ENDIF} 

           {$IFDEF AVXSUP}vmovapd ymm0, [rcx + rax - 64];             {$ELSE}db $C5,$FD,$28,$44,$01,$C0;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm3;                    {$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm0, ymm0, ymm1;                    {$ELSE}db $C5,$FD,$59,$C1;{$ENDIF} 
           {$IFDEF AVXSUP}vmovapd [rcx + rax - 64], ymm0;             {$ELSE}db $C5,$FD,$29,$44,$01,$C0;{$ENDIF} 

           {$IFDEF AVXSUP}vmovapd ymm2, [rcx + rax - 32];             {$ELSE}db $C5,$FD,$28,$54,$01,$E0;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm2, ymm2, ymm3;                    {$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm2, ymm2, ymm1;                    {$ELSE}db $C5,$ED,$59,$D1;{$ENDIF} 
           {$IFDEF AVXSUP}vmovapd [rcx + rax - 32], ymm2;             {$ELSE}db $C5,$FD,$29,$54,$01,$E0;{$ENDIF} 

       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           {$IFDEF AVXSUP}vmovapd xmm0, [rcx + rax];                  {$ELSE}db $C5,$F9,$28,$04,$01;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd xmm0, xmm0, xmm3;                    {$ELSE}db $C5,$F9,$58,$C3;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd xmm0, xmm0, xmm1;                    {$ELSE}db $C5,$F9,$59,$C1;{$ENDIF} 

           {$IFDEF AVXSUP}vmovapd [rcx + rax], xmm0;                  {$ELSE}db $C5,$F9,$29,$04,$01;{$ENDIF} 
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last column:
       {$IFDEF AVXSUP}vmovsd xmm0, [rcx];                             {$ELSE}db $C5,$FB,$10,$01;{$ENDIF} 
       {$IFDEF AVXSUP}vaddsd xmm0, xmm0, xmm3;                        {$ELSE}db $C5,$FB,$58,$C3;{$ENDIF} 
       {$IFDEF AVXSUP}vmulsd xmm0, xmm0, xmm1;                        {$ELSE}db $C5,$FB,$59,$C1;{$ENDIF} 
       {$IFDEF AVXSUP}vmovsd [rcx], xmm0;                             {$ELSE}db $C5,$FB,$11,$01;{$ENDIF} 

       // next line:
       add rcx, rdx;

   // loop y end
   dec r9;
   jnz @@addforyloop;

   // epilog
   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;

procedure AVXMatrixAddscaleUnAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : NativeInt; {$ifdef UNIX}unixdOffset{$ELSE}dOffset{$endif}, {$ifdef UNIX}unixScale{$ELSE}Scale{$endif} : double); {$IFDEF FPC}assembler;{$ENDIF}
{$IFDEF UNIX}
var dOffset, scale : double;
{$ENDIF}
asm
   {$IFDEF UNIX}
   // Linux uses a diffrent ABI -> copy over the registers so they meet with winABI
   // (note that the 5th and 6th parameter are are on the stack)
   // The parameters are passed in the following order:
   // RDI, RSI, RDX, RCX -> mov to RCX, RDX, R8, R9
   movsd xmm3, unixdOffset;
   movsd dOffset, xmm3;
   movsd xmm3, unixscale;
   movsd scale, xmm3;
   mov r8, rdx;
   mov r9, rcx;
   mov rcx, rdi;
   mov rdx, rsi;
   {$ENDIF}

   // note: RCX = dest, RDX = destLineWidth, R8 = width, R9 = height
   // prolog - simulate stack

   //iters := -(width - 1)*sizeof(double);
   dec r8;
   imul r8, -8;

   lea rax, dOffset;
   {$IFDEF AVXSUP}vbroadcastsd ymm3, [rax];                           {$ELSE}db $C4,$E2,$7D,$19,$18;{$ENDIF} 
   lea rax, scale;
   {$IFDEF AVXSUP}vbroadcastsd ymm1, [rax];                           {$ELSE}db $C4,$E2,$7D,$19,$08;{$ENDIF} 

   // helper registers for the mt1, mt2 and dest pointers
   sub rcx, r8;

   // for y := 0 to height - 1:
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r8;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // prefetchw data...
           // prefetchw [rcx + rax];

           // add mul
           {$IFDEF AVXSUP}vmovupd ymm0, [rcx + rax - 128];            {$ELSE}db $C5,$FD,$10,$44,$01,$80;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm3;                    {$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm0, ymm0, ymm1;                    {$ELSE}db $C5,$FD,$59,$C1;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd [rcx + rax - 128], ymm0;            {$ELSE}db $C5,$FD,$11,$44,$01,$80;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd ymm2, [rcx + rax - 96];             {$ELSE}db $C5,$FD,$10,$54,$01,$A0;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm2, ymm2, ymm3;                    {$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm2, ymm2, ymm1;                    {$ELSE}db $C5,$ED,$59,$D1;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd [rcx + rax - 96], ymm2;             {$ELSE}db $C5,$FD,$11,$54,$01,$A0;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd ymm0, [rcx + rax - 64];             {$ELSE}db $C5,$FD,$10,$44,$01,$C0;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm3;                    {$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm0, ymm0, ymm1;                    {$ELSE}db $C5,$FD,$59,$C1;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd [rcx + rax - 64], ymm0;             {$ELSE}db $C5,$FD,$11,$44,$01,$C0;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd ymm2, [rcx + rax - 32];             {$ELSE}db $C5,$FD,$10,$54,$01,$E0;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm2, ymm2, ymm3;                    {$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm2, ymm2, ymm1;                    {$ELSE}db $C5,$ED,$59,$D1;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd [rcx + rax - 32], ymm2;             {$ELSE}db $C5,$FD,$11,$54,$01,$E0;{$ENDIF} 

       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           {$IFDEF AVXSUP}vmovupd xmm0, [rcx + rax];                  {$ELSE}db $C5,$F9,$10,$04,$01;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd xmm0, xmm0, xmm3;                    {$ELSE}db $C5,$F9,$58,$C3;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd xmm0, xmm0, xmm1;                    {$ELSE}db $C5,$F9,$59,$C1;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd [rcx + rax], xmm0;                  {$ELSE}db $C5,$F9,$11,$04,$01;{$ENDIF} 
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last column:
       {$IFDEF AVXSUP}vmovsd xmm0, [rcx];                             {$ELSE}db $C5,$FB,$10,$01;{$ENDIF} 
       {$IFDEF AVXSUP}vaddsd xmm0, xmm0, xmm3;                        {$ELSE}db $C5,$FB,$58,$C3;{$ENDIF} 
       {$IFDEF AVXSUP}vmulsd xmm0, xmm0, xmm1;                        {$ELSE}db $C5,$FB,$59,$C1;{$ENDIF} 
       {$IFDEF AVXSUP}vmovsd [rcx], xmm0;                             {$ELSE}db $C5,$FB,$11,$01;{$ENDIF} 

       // next line:
       add rcx, rdx;

   // loop y end
   dec r9;
   jnz @@addforyloop;

   // epilog
   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;


procedure AVXMatrixScaleAddAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : NativeInt; {$ifdef UNIX}unixdOffset{$ELSE}dOffset{$endif}, {$ifdef UNIX}unixScale{$ELSE}Scale{$endif} : double); {$IFDEF FPC}assembler;{$ENDIF}
{$IFDEF UNIX}
var dOffset, scale : double;
{$ENDIF}
asm
   {$IFDEF UNIX}
   // Linux uses a diffrent ABI -> copy over the registers so they meet with winABI
   // (note that the 5th and 6th parameter are are on the stack)
   // The parameters are passed in the following order:
   // RDI, RSI, RDX, RCX -> mov to RCX, RDX, R8, R9
   movsd xmm3, unixdOffset;
   movsd dOffset, xmm3;
   movsd xmm3, unixscale;
   movsd scale, xmm3;
   mov r8, rdx;
   mov r9, rcx;
   mov rcx, rdi;
   mov rdx, rsi;
   {$ENDIF}

   // note: RCX = dest, RDX = destLineWidth, R8 = width, R9 = height
   // prolog - simulate stack

   //iters := -width*sizeof(double);
   imul r8, -8;

   lea rax, dOffset;
   {$IFDEF AVXSUP}vbroadcastsd ymm3, [rax];                           {$ELSE}db $C4,$E2,$7D,$19,$18;{$ENDIF} 
   lea rax, scale;
   {$IFDEF AVXSUP}vbroadcastsd ymm1, [rax];                           {$ELSE}db $C4,$E2,$7D,$19,$08;{$ENDIF} 

   // helper registers for the mt1, mt2 and dest pointers
   sub rcx, r8;

   // for y := 0 to height - 1:
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r8;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // prefetchw data...
           // prefetchw [rcx + rax];

           // addition:
           {$IFDEF AVXSUP}vmovapd ymm0, [rcx + rax - 128];            {$ELSE}db $C5,$FD,$28,$44,$01,$80;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm0, ymm0, ymm1;                    {$ELSE}db $C5,$FD,$59,$C1;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm3;                    {$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF AVXSUP}vmovapd [rcx + rax - 128], ymm0;            {$ELSE}db $C5,$FD,$29,$44,$01,$80;{$ENDIF} 

           {$IFDEF AVXSUP}vmovapd ymm2, [rcx + rax - 96];             {$ELSE}db $C5,$FD,$28,$54,$01,$A0;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm2, ymm2, ymm1;                    {$ELSE}db $C5,$ED,$59,$D1;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm2, ymm2, ymm3;                    {$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF AVXSUP}vmovapd [rcx + rax - 96], ymm2;             {$ELSE}db $C5,$FD,$29,$54,$01,$A0;{$ENDIF} 

           {$IFDEF AVXSUP}vmovapd ymm0, [rcx + rax - 64];             {$ELSE}db $C5,$FD,$28,$44,$01,$C0;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm0, ymm0, ymm1;                    {$ELSE}db $C5,$FD,$59,$C1;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm3;                    {$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF AVXSUP}vmovapd [rcx + rax - 64], ymm0;             {$ELSE}db $C5,$FD,$29,$44,$01,$C0;{$ENDIF} 

           {$IFDEF AVXSUP}vmovapd ymm2, [rcx + rax - 32];             {$ELSE}db $C5,$FD,$28,$54,$01,$E0;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm2, ymm2, ymm1;                    {$ELSE}db $C5,$ED,$59,$D1;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm2, ymm2, ymm3;                    {$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF AVXSUP}vmovapd [rcx + rax - 32], ymm2;             {$ELSE}db $C5,$FD,$29,$54,$01,$E0;{$ENDIF} 

       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           {$IFDEF AVXSUP}vmovapd xmm0, [rcx + rax];                  {$ELSE}db $C5,$F9,$28,$04,$01;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd xmm0, xmm0, xmm1;                    {$ELSE}db $C5,$F9,$59,$C1;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd xmm0, xmm0, xmm3;                    {$ELSE}db $C5,$F9,$58,$C3;{$ENDIF} 

           {$IFDEF AVXSUP}vmovapd [rcx + rax], xmm0;                  {$ELSE}db $C5,$F9,$29,$04,$01;{$ENDIF} 
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // next line:
       add rcx, rdx;

   // loop y end
   dec r9;
   jnz @@addforyloop;

   // epilog
   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;

procedure AVXMatrixScaleAddUnAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : NativeInt; {$ifdef UNIX}unixdOffset{$ELSE}dOffset{$endif}, {$ifdef UNIX}unixScale{$ELSE}Scale{$endif} : double); {$IFDEF FPC}assembler;{$ENDIF}
{$IFDEF UNIX}
var dOffset, scale : double;
{$ENDIF}
asm
   {$IFDEF UNIX}
   // Linux uses a diffrent ABI -> copy over the registers so they meet with winABI
   // (note that the 5th and 6th parameter are are on the stack)
   // The parameters are passed in the following order:
   // RDI, RSI, RDX, RCX -> mov to RCX, RDX, R8, R9
   movsd xmm3, unixdOffset;
   movsd dOffset, xmm3;
   movsd xmm3, unixscale;
   movsd scale, xmm3;
   mov r8, rdx;
   mov r9, rcx;
   mov rcx, rdi;
   mov rdx, rsi;
   {$ENDIF}

   // note: RCX = dest, RDX = destLineWidth, R8 = width, R9 = height
   // prolog - simulate stack

   //iters := -width*sizeof(double);
   imul r8, -8;

   lea rax, dOffset;
   {$IFDEF AVXSUP}vbroadcastsd ymm3, [rax];                           {$ELSE}db $C4,$E2,$7D,$19,$18;{$ENDIF} 
   lea rax, scale;
   {$IFDEF AVXSUP}vbroadcastsd ymm1, [rax];                           {$ELSE}db $C4,$E2,$7D,$19,$08;{$ENDIF} 

   // helper registers for the mt1, mt2 and dest pointers
   sub rcx, r8;

   // for y := 0 to height - 1:
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r8;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // prefetchw data...
           // prefetchw [rcx + rax];

           // addition:
           {$IFDEF AVXSUP}vmovupd ymm0, [rcx + rax - 128];            {$ELSE}db $C5,$FD,$10,$44,$01,$80;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm0, ymm0, ymm1;                    {$ELSE}db $C5,$FD,$59,$C1;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm3;                    {$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd [rcx + rax - 128], ymm0;            {$ELSE}db $C5,$FD,$11,$44,$01,$80;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd ymm2, [rcx + rax - 96];             {$ELSE}db $C5,$FD,$10,$54,$01,$A0;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm2, ymm2, ymm1;                    {$ELSE}db $C5,$ED,$59,$D1;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm2, ymm2, ymm3;                    {$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd [rcx + rax - 96], ymm2;             {$ELSE}db $C5,$FD,$11,$54,$01,$A0;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd ymm0, [rcx + rax - 64];             {$ELSE}db $C5,$FD,$10,$44,$01,$C0;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm0, ymm0, ymm1;                    {$ELSE}db $C5,$FD,$59,$C1;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm3;                    {$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd [rcx + rax - 64], ymm0;             {$ELSE}db $C5,$FD,$11,$44,$01,$C0;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd ymm2, [rcx + rax - 32];             {$ELSE}db $C5,$FD,$10,$54,$01,$E0;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm2, ymm2, ymm1;                    {$ELSE}db $C5,$ED,$59,$D1;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm2, ymm2, ymm3;                    {$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd [rcx + rax - 32], ymm2;             {$ELSE}db $C5,$FD,$11,$54,$01,$E0;{$ENDIF} 

       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           {$IFDEF AVXSUP}vmovupd xmm0, [rcx + rax];                  {$ELSE}db $C5,$F9,$10,$04,$01;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd xmm0, xmm0, xmm1;                    {$ELSE}db $C5,$F9,$59,$C1;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd xmm0, xmm0, xmm3;                    {$ELSE}db $C5,$F9,$58,$C3;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd [rcx + rax], xmm0;                  {$ELSE}db $C5,$F9,$11,$04,$01;{$ENDIF} 
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // next line:
       add rcx, rdx;

   // loop y end
   dec r9;
   jnz @@addforyloop;

   // epilog
   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;

procedure AVXMatrixScaleAddAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : NativeInt; {$ifdef UNIX}unixdOffset{$ELSE}dOffset{$endif}, {$ifdef UNIX}unixScale{$ELSE}Scale{$endif} : double); {$IFDEF FPC}assembler;{$ENDIF}
{$IFDEF UNIX}
var dOffset, scale : double;
{$ENDIF}
asm
   {$IFDEF UNIX}
   // Linux uses a diffrent ABI -> copy over the registers so they meet with winABI
   // (note that the 5th and 6th parameter are are on the stack)
   // The parameters are passed in the following order:
   // RDI, RSI, RDX, RCX -> mov to RCX, RDX, R8, R9
   movsd xmm3, unixdOffset;
   movsd dOffset, xmm3;
   movsd xmm3, unixscale;
   movsd scale, xmm3;
   mov r8, rdx;
   mov r9, rcx;
   mov rcx, rdi;
   mov rdx, rsi;
   {$ENDIF}

   // note: RCX = dest, RDX = destLineWidth, R8 = width, R9 = height
   // prolog - simulate stack

   //iters := -width*sizeof(double);
   dec r8;
   imul r8, -8;

   lea rax, dOffset;
   {$IFDEF AVXSUP}vbroadcastsd ymm3, [rax];                           {$ELSE}db $C4,$E2,$7D,$19,$18;{$ENDIF} 
   lea rax, scale;
   {$IFDEF AVXSUP}vbroadcastsd ymm1, [rax];                           {$ELSE}db $C4,$E2,$7D,$19,$08;{$ENDIF} 

   // helper registers for the mt1, mt2 and dest pointers
   sub rcx, r8;

   // for y := 0 to height - 1:
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r8;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // prefetchw data...
           // prefetchw [rcx + rax];

           // addition:
           {$IFDEF AVXSUP}vmovapd ymm0, [rcx + rax - 128];            {$ELSE}db $C5,$FD,$28,$44,$01,$80;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm0, ymm0, ymm1;                    {$ELSE}db $C5,$FD,$59,$C1;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm3;                    {$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF AVXSUP}vmovapd [rcx + rax - 128], ymm0;            {$ELSE}db $C5,$FD,$29,$44,$01,$80;{$ENDIF} 

           {$IFDEF AVXSUP}vmovapd ymm2, [rcx + rax - 96];             {$ELSE}db $C5,$FD,$28,$54,$01,$A0;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm2, ymm2, ymm1;                    {$ELSE}db $C5,$ED,$59,$D1;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm2, ymm2, ymm3;                    {$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF AVXSUP}vmovapd [rcx + rax - 96], ymm2;             {$ELSE}db $C5,$FD,$29,$54,$01,$A0;{$ENDIF} 

           {$IFDEF AVXSUP}vmovapd ymm0, [rcx + rax - 64];             {$ELSE}db $C5,$FD,$28,$44,$01,$C0;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm0, ymm0, ymm1;                    {$ELSE}db $C5,$FD,$59,$C1;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm3;                    {$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF AVXSUP}vmovapd [rcx + rax - 64], ymm0;             {$ELSE}db $C5,$FD,$29,$44,$01,$C0;{$ENDIF} 

           {$IFDEF AVXSUP}vmovapd ymm2, [rcx + rax - 32];             {$ELSE}db $C5,$FD,$28,$54,$01,$E0;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm2, ymm2, ymm1;                    {$ELSE}db $C5,$ED,$59,$D1;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm2, ymm2, ymm3;                    {$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF AVXSUP}vmovapd [rcx + rax - 32], ymm2;             {$ELSE}db $C5,$FD,$29,$54,$01,$E0;{$ENDIF} 


       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           {$IFDEF AVXSUP}vmovapd xmm0, [rcx + rax];                  {$ELSE}db $C5,$F9,$28,$04,$01;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd xmm0, xmm0, xmm1;                    {$ELSE}db $C5,$F9,$59,$C1;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd xmm0, xmm0, xmm3;                    {$ELSE}db $C5,$F9,$58,$C3;{$ENDIF} 

           {$IFDEF AVXSUP}vmovapd [rcx + rax], xmm0;                  {$ELSE}db $C5,$F9,$29,$04,$01;{$ENDIF} 
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last column:
       {$IFDEF AVXSUP}vmovsd xmm0, [rcx];                             {$ELSE}db $C5,$FB,$10,$01;{$ENDIF} 
       {$IFDEF AVXSUP}vmulsd xmm0, xmm0, xmm1;                        {$ELSE}db $C5,$FB,$59,$C1;{$ENDIF} 
       {$IFDEF AVXSUP}vaddsd xmm0, xmm0, xmm3;                        {$ELSE}db $C5,$FB,$58,$C3;{$ENDIF} 
       {$IFDEF AVXSUP}vmovsd [rcx], xmm0;                             {$ELSE}db $C5,$FB,$11,$01;{$ENDIF} 

       // next line:
       add rcx, rdx;

   // loop y end
   dec r9;
   jnz @@addforyloop;

   // epilog
   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;

procedure AVXMatrixScaleAddUnAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : NativeInt; {$ifdef UNIX}unixdOffset{$ELSE}dOffset{$endif}, {$ifdef UNIX}unixScale{$ELSE}Scale{$endif} : double); {$IFDEF FPC}assembler;{$ENDIF}
{$IFDEF UNIX}
var dOffset, scale : double;
{$ENDIF}
asm
   {$IFDEF UNIX}
   // Linux uses a diffrent ABI -> copy over the registers so they meet with winABI
   // (note that the 5th and 6th parameter are are on the stack)
   // The parameters are passed in the following order:
   // RDI, RSI, RDX, RCX -> mov to RCX, RDX, R8, R9
   movsd xmm3, unixdOffset;
   movsd dOffset, xmm3;
   movsd xmm3, unixscale;
   movsd scale, xmm3;
   mov r8, rdx;
   mov r9, rcx;
   mov rcx, rdi;
   mov rdx, rsi;
   {$ENDIF}

   // note: RCX = dest, RDX = destLineWidth, R8 = width, R9 = height
   // prolog - simulate stack

   //iters := -width*sizeof(double);
   dec r8;
   imul r8, -8;

   lea rax, dOffset;
   {$IFDEF AVXSUP}vbroadcastsd ymm3, [rax];                           {$ELSE}db $C4,$E2,$7D,$19,$18;{$ENDIF} 
   lea rax, scale;
   {$IFDEF AVXSUP}vbroadcastsd ymm1, [rax];                           {$ELSE}db $C4,$E2,$7D,$19,$08;{$ENDIF} 

   // helper registers for the mt1, mt2 and dest pointers
   sub rcx, r8;

   // for y := 0 to height - 1:
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r8;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // prefetchw data...
           // prefetchw [rcx + rax];

           // addition:
           {$IFDEF AVXSUP}vmovupd ymm0, [rcx + rax - 128];            {$ELSE}db $C5,$FD,$10,$44,$01,$80;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm0, ymm0, ymm1;                    {$ELSE}db $C5,$FD,$59,$C1;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm3;                    {$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd [rcx + rax - 128], ymm0;            {$ELSE}db $C5,$FD,$11,$44,$01,$80;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd ymm2, [rcx + rax - 96];             {$ELSE}db $C5,$FD,$10,$54,$01,$A0;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm2, ymm2, ymm1;                    {$ELSE}db $C5,$ED,$59,$D1;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm2, ymm2, ymm3;                    {$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd [rcx + rax - 96], ymm2;             {$ELSE}db $C5,$FD,$11,$54,$01,$A0;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd ymm0, [rcx + rax - 64];             {$ELSE}db $C5,$FD,$10,$44,$01,$C0;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm0, ymm0, ymm1;                    {$ELSE}db $C5,$FD,$59,$C1;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm3;                    {$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd [rcx + rax - 64], ymm0;             {$ELSE}db $C5,$FD,$11,$44,$01,$C0;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd ymm2, [rcx + rax - 32];             {$ELSE}db $C5,$FD,$10,$54,$01,$E0;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm2, ymm2, ymm1;                    {$ELSE}db $C5,$ED,$59,$D1;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm2, ymm2, ymm3;                    {$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd [rcx + rax - 32], ymm2;             {$ELSE}db $C5,$FD,$11,$54,$01,$E0;{$ENDIF} 

       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           {$IFDEF AVXSUP}vmovupd xmm0, [rcx + rax];                  {$ELSE}db $C5,$F9,$10,$04,$01;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd xmm0, xmm0, xmm1;                    {$ELSE}db $C5,$F9,$59,$C1;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd xmm0, xmm0, xmm3;                    {$ELSE}db $C5,$F9,$58,$C3;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd [rcx + rax], xmm0;                  {$ELSE}db $C5,$F9,$11,$04,$01;{$ENDIF} 
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last column:
       {$IFDEF AVXSUP}vmovsd xmm0, [rcx];                             {$ELSE}db $C5,$FB,$10,$01;{$ENDIF} 
       {$IFDEF AVXSUP}vmulsd xmm0, xmm0, xmm1;                        {$ELSE}db $C5,$FB,$59,$C1;{$ENDIF} 
       {$IFDEF AVXSUP}vaddsd xmm0, xmm0, xmm3;                        {$ELSE}db $C5,$FB,$58,$C3;{$ENDIF} 
       {$IFDEF AVXSUP}vmovsd [rcx], xmm0;                             {$ELSE}db $C5,$FB,$11,$01;{$ENDIF} 

       // next line:
       add rcx, rdx;

   // loop y end
   dec r9;
   jnz @@addforyloop;

   // epilog
   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;


// ###########################################
// #### Offset a matrix
// ###########################################

procedure AVXMatrixAddAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : NativeInt; {$ifdef UNIX}unixdOffset{$ELSE}dOffset{$endif} : double); {$IFDEF FPC}assembler;{$ENDIF}
{$IFDEF UNIX}
var dOffset : double;
{$ENDIF}
asm
   {$IFDEF UNIX}
   // Linux uses a diffrent ABI -> copy over the registers so they meet with winABI
   // (note that the 5th and 6th parameter are are on the stack)
   // The parameters are passed in the following order:
   // RDI, RSI, RDX, RCX -> mov to RCX, RDX, R8, R9
   movsd xmm3, unixdOffset;
   movsd dOffset, xmm3;
   mov r8, rdx;
   mov r9, rcx;
   mov rcx, rdi;
   mov rdx, rsi;
   {$ENDIF}

   // note: RCX = dest, RDX = destLineWidth, R8 = width, R9 = height
   // prolog - simulate stack

   //iters := -width*sizeof(double);
   imul r8, -8;

   lea rax, dOffset;
   {$IFDEF AVXSUP}vbroadcastsd ymm3, [rax];                           {$ELSE}db $C4,$E2,$7D,$19,$18;{$ENDIF} 

   // helper registers for the mt1, mt2 and dest pointers
   sub rcx, r8;

   // for y := 0 to height - 1:
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r8;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // prefetchw data...
           // prefetchw [rcx + rax];

           // add mul
           {$IFDEF AVXSUP}vmovapd ymm0, [rcx + rax - 128];            {$ELSE}db $C5,$FD,$28,$44,$01,$80;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm3;                    {$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF AVXSUP}vmovapd [rcx + rax - 128], ymm0;            {$ELSE}db $C5,$FD,$29,$44,$01,$80;{$ENDIF} 

           {$IFDEF AVXSUP}vmovapd ymm2, [rcx + rax - 96];             {$ELSE}db $C5,$FD,$28,$54,$01,$A0;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm2, ymm2, ymm3;                    {$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF AVXSUP}vmovapd [rcx + rax - 96], ymm2;             {$ELSE}db $C5,$FD,$29,$54,$01,$A0;{$ENDIF} 

           {$IFDEF AVXSUP}vmovapd ymm0, [rcx + rax - 64];             {$ELSE}db $C5,$FD,$28,$44,$01,$C0;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm3;                    {$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF AVXSUP}vmovapd [rcx + rax - 64], ymm0;             {$ELSE}db $C5,$FD,$29,$44,$01,$C0;{$ENDIF} 

           {$IFDEF AVXSUP}vmovapd ymm2, [rcx + rax - 32];             {$ELSE}db $C5,$FD,$28,$54,$01,$E0;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm2, ymm2, ymm3;                    {$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF AVXSUP}vmovapd [rcx + rax - 32], ymm2;             {$ELSE}db $C5,$FD,$29,$54,$01,$E0;{$ENDIF} 

       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           {$IFDEF AVXSUP}vmovapd xmm0, [rcx + rax];                  {$ELSE}db $C5,$F9,$28,$04,$01;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd xmm0, xmm0, xmm3;                    {$ELSE}db $C5,$F9,$58,$C3;{$ENDIF} 

           {$IFDEF AVXSUP}vmovapd [rcx + rax], xmm0;                  {$ELSE}db $C5,$F9,$29,$04,$01;{$ENDIF} 
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // next line:
       add rcx, rdx;

   // loop y end
   dec r9;
   jnz @@addforyloop;

   // epilog
   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;

procedure AVXMatrixAddUnAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : NativeInt; {$ifdef UNIX}unixdOffset{$ELSE}dOffset{$endif} : double); {$IFDEF FPC}assembler;{$ENDIF}
{$IFDEF UNIX}
var dOffset : double;
{$ENDIF}
asm
   {$IFDEF UNIX}
   // Linux uses a diffrent ABI -> copy over the registers so they meet with winABI
   // (note that the 5th and 6th parameter are are on the stack)
   // The parameters are passed in the following order:
   // RDI, RSI, RDX, RCX -> mov to RCX, RDX, R8, R9
   movsd xmm3, unixdOffset;
   movsd dOffset, xmm3;
   mov r8, rdx;
   mov r9, rcx;
   mov rcx, rdi;
   mov rdx, rsi;
   {$ENDIF}

   // note: RCX = dest, RDX = destLineWidth, R8 = width, R9 = height
   // prolog - simulate stack

   //iters := -width*sizeof(double);
   imul r8, -8;

   lea rax, dOffset;
   {$IFDEF AVXSUP}vbroadcastsd ymm3, [rax];                           {$ELSE}db $C4,$E2,$7D,$19,$18;{$ENDIF} 

   // helper registers for the mt1, mt2 and dest pointers
   sub rcx, r8;

   // for y := 0 to height - 1:
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r8;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // prefetchw data...
           // prefetchw [rcx + rax];

           // add mul
           {$IFDEF AVXSUP}vmovupd ymm0, [rcx + rax - 128];            {$ELSE}db $C5,$FD,$10,$44,$01,$80;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm3;                    {$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd [rcx + rax - 128], ymm0;            {$ELSE}db $C5,$FD,$11,$44,$01,$80;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd ymm2, [rcx + rax - 96];             {$ELSE}db $C5,$FD,$10,$54,$01,$A0;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm2, ymm2, ymm3;                    {$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd [rcx + rax - 96], ymm2;             {$ELSE}db $C5,$FD,$11,$54,$01,$A0;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd ymm0, [rcx + rax - 64];             {$ELSE}db $C5,$FD,$10,$44,$01,$C0;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm3;                    {$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd [rcx + rax - 64], ymm0;             {$ELSE}db $C5,$FD,$11,$44,$01,$C0;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd ymm2, [rcx + rax - 32];             {$ELSE}db $C5,$FD,$10,$54,$01,$E0;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm2, ymm2, ymm3;                    {$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd [rcx + rax - 32], ymm2;             {$ELSE}db $C5,$FD,$11,$54,$01,$E0;{$ENDIF} 

       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           {$IFDEF AVXSUP}vmovupd xmm0, [rcx + rax];                  {$ELSE}db $C5,$F9,$10,$04,$01;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd xmm0, xmm0, xmm3;                    {$ELSE}db $C5,$F9,$58,$C3;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd [rcx + rax], xmm0;                  {$ELSE}db $C5,$F9,$11,$04,$01;{$ENDIF} 
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // next line:
       add rcx, rdx;

   // loop y end
   dec r9;
   jnz @@addforyloop;

   // epilog
   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;


procedure AVXMatrixAddAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : NativeInt; {$ifdef UNIX}unixdOffset{$ELSE}dOffset{$endif} : double); {$IFDEF FPC}assembler;{$ENDIF}
{$IFDEF UNIX}
var dOffset : double;
{$ENDIF}
asm
   {$IFDEF UNIX}
   // Linux uses a diffrent ABI -> copy over the registers so they meet with winABI
   // (note that the 5th and 6th parameter are are on the stack)
   // The parameters are passed in the following order:
   // RDI, RSI, RDX, RCX -> mov to RCX, RDX, R8, R9
   movsd xmm3, unixdOffset;
   movsd dOffset, xmm3;
   mov r8, rdx;
   mov r9, rcx;
   mov rcx, rdi;
   mov rdx, rsi;
   {$ENDIF}

   // note: RCX = dest, RDX = destLineWidth, R8 = width, R9 = height
   // prolog - simulate stack

   //iters := -(width - 1)*sizeof(double);
   dec r8;
   imul r8, -8;

   lea rax, dOffset;
   {$IFDEF AVXSUP}vbroadcastsd ymm3, [rax];                           {$ELSE}db $C4,$E2,$7D,$19,$18;{$ENDIF} 

   // helper registers for the mt1, mt2 and dest pointers
   sub rcx, r8;

   // for y := 0 to height - 1:
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r8;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // prefetchw data...
           // prefetchw [rcx + rax];

           // add mul
           {$IFDEF AVXSUP}vmovapd ymm0, [rcx + rax - 128];            {$ELSE}db $C5,$FD,$28,$44,$01,$80;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm3;                    {$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF AVXSUP}vmovapd [rcx + rax - 128], ymm0;            {$ELSE}db $C5,$FD,$29,$44,$01,$80;{$ENDIF} 

           {$IFDEF AVXSUP}vmovapd ymm2, [rcx + rax - 96];             {$ELSE}db $C5,$FD,$28,$54,$01,$A0;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm2, ymm2, ymm3;                    {$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF AVXSUP}vmovapd [rcx + rax - 96], ymm2;             {$ELSE}db $C5,$FD,$29,$54,$01,$A0;{$ENDIF} 

           {$IFDEF AVXSUP}vmovapd ymm0, [rcx + rax - 64];             {$ELSE}db $C5,$FD,$28,$44,$01,$C0;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm3;                    {$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF AVXSUP}vmovapd [rcx + rax - 64], ymm0;             {$ELSE}db $C5,$FD,$29,$44,$01,$C0;{$ENDIF} 

           {$IFDEF AVXSUP}vmovapd ymm2, [rcx + rax - 32];             {$ELSE}db $C5,$FD,$28,$54,$01,$E0;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm2, ymm2, ymm3;                    {$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF AVXSUP}vmovapd [rcx + rax - 32], ymm2;             {$ELSE}db $C5,$FD,$29,$54,$01,$E0;{$ENDIF} 

       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           {$IFDEF AVXSUP}vmovapd xmm0, [rcx + rax];                  {$ELSE}db $C5,$F9,$28,$04,$01;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd xmm0, xmm0, xmm3;                    {$ELSE}db $C5,$F9,$58,$C3;{$ENDIF} 

           {$IFDEF AVXSUP}vmovapd [rcx + rax], xmm0;                  {$ELSE}db $C5,$F9,$29,$04,$01;{$ENDIF} 
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last column:
       {$IFDEF AVXSUP}vmovsd xmm0, [rcx];                             {$ELSE}db $C5,$FB,$10,$01;{$ENDIF} 
       {$IFDEF AVXSUP}vaddsd xmm0, xmm0, xmm3;                        {$ELSE}db $C5,$FB,$58,$C3;{$ENDIF} 
       {$IFDEF AVXSUP}vmovsd [rcx], xmm0;                             {$ELSE}db $C5,$FB,$11,$01;{$ENDIF} 

       // next line:
       add rcx, rdx;

   // loop y end
   dec r9;
   jnz @@addforyloop;

   // epilog
   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;

procedure AVXMatrixAddUnAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : NativeInt; {$ifdef UNIX}unixdOffset{$ELSE}dOffset{$endif} : double); {$IFDEF FPC}assembler;{$ENDIF}
{$IFDEF UNIX}
var dOffset : double;
{$ENDIF}
asm
   {$IFDEF UNIX}
   // Linux uses a diffrent ABI -> copy over the registers so they meet with winABI
   // (note that the 5th and 6th parameter are are on the stack)
   // The parameters are passed in the following order:
   // RDI, RSI, RDX, RCX -> mov to RCX, RDX, R8, R9
   movsd xmm3, unixdOffset;
   movsd dOffset, xmm3;
   mov r8, rdx;
   mov r9, rcx;
   mov rcx, rdi;
   mov rdx, rsi;
   {$ENDIF}

   // note: RCX = dest, RDX = destLineWidth, R8 = width, R9 = height
   // prolog - simulate stack

   //iters := -(width - 1)*sizeof(double);
   dec r8;
   imul r8, -8;

   lea rax, dOffset;
   {$IFDEF AVXSUP}vbroadcastsd ymm3, [rax];                           {$ELSE}db $C4,$E2,$7D,$19,$18;{$ENDIF} 

   // helper registers for the mt1, mt2 and dest pointers
   sub rcx, r8;

   // for y := 0 to height - 1:
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r8;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // prefetchw data...
           // prefetchw [rcx + rax];

           // add mul
           {$IFDEF AVXSUP}vmovupd ymm0, [rcx + rax - 128];            {$ELSE}db $C5,$FD,$10,$44,$01,$80;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm3;                    {$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd [rcx + rax - 128], ymm0;            {$ELSE}db $C5,$FD,$11,$44,$01,$80;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd ymm2, [rcx + rax - 96];             {$ELSE}db $C5,$FD,$10,$54,$01,$A0;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm2, ymm2, ymm3;                    {$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd [rcx + rax - 96], ymm2;             {$ELSE}db $C5,$FD,$11,$54,$01,$A0;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd ymm0, [rcx + rax - 64];             {$ELSE}db $C5,$FD,$10,$44,$01,$C0;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm3;                    {$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd [rcx + rax - 64], ymm0;             {$ELSE}db $C5,$FD,$11,$44,$01,$C0;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd ymm2, [rcx + rax - 32];             {$ELSE}db $C5,$FD,$10,$54,$01,$E0;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm2, ymm2, ymm3;                    {$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd [rcx + rax - 32], ymm2;             {$ELSE}db $C5,$FD,$11,$54,$01,$E0;{$ENDIF} 

       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           {$IFDEF AVXSUP}vmovupd xmm0, [rcx + rax];                  {$ELSE}db $C5,$F9,$10,$04,$01;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd xmm0, xmm0, xmm3;                    {$ELSE}db $C5,$F9,$58,$C3;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd [rcx + rax], xmm0;                  {$ELSE}db $C5,$F9,$11,$04,$01;{$ENDIF} 
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last column:
       {$IFDEF AVXSUP}vmovsd xmm0, [rcx];                             {$ELSE}db $C5,$FB,$10,$01;{$ENDIF} 
       {$IFDEF AVXSUP}vaddsd xmm0, xmm0, xmm3;                        {$ELSE}db $C5,$FB,$58,$C3;{$ENDIF} 
       {$IFDEF AVXSUP}vmovsd [rcx], xmm0;                             {$ELSE}db $C5,$FB,$11,$01;{$ENDIF} 

       // next line:
       add rcx, rdx;

   // loop y end
   dec r9;
   jnz @@addforyloop;

   // epilog
   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;

{$ENDIF}

end.
