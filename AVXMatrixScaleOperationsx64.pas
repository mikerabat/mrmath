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

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF x64}

uses MatrixConst;

procedure AVXMatrixAddScaleAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);
procedure AVXMatrixAddScaleUnAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);

procedure AVXMatrixAddScaleAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);
procedure AVXMatrixAddScaleUnAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);

procedure AVXMatrixScaleAddAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);
procedure AVXMatrixScaleAddUnAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);

procedure AVXMatrixScaleAddAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);
procedure AVXMatrixScaleAddUnAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);

{$ENDIF}

implementation

{$IFDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$S-} {$ENDIF}

procedure AVXMatrixAddScaleAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);
{$IFDEF FPC}
begin
{$ENDIF}
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

   // note: RCX = dest, RDX = destLineWidth, R8 = width, R9 = height
   // prolog - simulate stack

   //iters := -width*sizeof(double);
   mov r10, width;
   imul r10, -8;

   lea rax, dOffset;
   {$IFDEF FPC}vbroadcastsd ymm3, [rax];{$ELSE}db $C4,$E2,$7D,$19,$18;{$ENDIF} 
   lea rax, scale;
   {$IFDEF FPC}vbroadcastsd ymm1, [rax];{$ELSE}db $C4,$E2,$7D,$19,$08;{$ENDIF} 


   // helper registers for the mt1, mt2 and dest pointers
   sub rcx, r10;

   // for y := 0 to height - 1:
   mov r11, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // prefetchw data...
           // prefetchw [rcx + rax];

           // add mul
           {$IFDEF FPC}vmovapd ymm0, [rcx + rax - 128];{$ELSE}db $C5,$FD,$28,$44,$01,$80;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$59,$C1;{$ENDIF} 
           {$IFDEF FPC}vmovapd [rcx + rax - 128], ymm0;{$ELSE}db $C5,$FD,$29,$44,$01,$80;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm2, [rcx + rax - 96];{$ELSE}db $C5,$FD,$28,$54,$01,$A0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm2, ymm2, ymm3;{$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm2, ymm2, ymm1;{$ELSE}db $C5,$ED,$59,$D1;{$ENDIF} 
           {$IFDEF FPC}vmovapd [rcx + rax - 96], ymm2;{$ELSE}db $C5,$FD,$29,$54,$01,$A0;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm0, [rcx + rax - 64];{$ELSE}db $C5,$FD,$28,$44,$01,$C0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$59,$C1;{$ENDIF} 
           {$IFDEF FPC}vmovapd [rcx + rax - 64], ymm0;{$ELSE}db $C5,$FD,$29,$44,$01,$C0;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm2, [rcx + rax - 32];{$ELSE}db $C5,$FD,$28,$54,$01,$E0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm2, ymm2, ymm3;{$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm2, ymm2, ymm1;{$ELSE}db $C5,$ED,$59,$D1;{$ENDIF} 
           {$IFDEF FPC}vmovapd [rcx + rax - 32], ymm2;{$ELSE}db $C5,$FD,$29,$54,$01,$E0;{$ENDIF} 

       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           {$IFDEF FPC}vmovapd xmm0, [rcx + rax];{$ELSE}db $C5,$F9,$28,$04,$01;{$ENDIF} 
           {$IFDEF FPC}vaddpd xmm0, xmm0, xmm3;{$ELSE}db $C5,$F9,$58,$C3;{$ENDIF} 
           {$IFDEF FPC}vmulpd xmm0, xmm0, xmm1;{$ELSE}db $C5,$F9,$59,$C1;{$ENDIF} 

           {$IFDEF FPC}vmovapd [rcx + rax], xmm0;{$ELSE}db $C5,$F9,$29,$04,$01;{$ENDIF} 
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // next line:
       add rcx, rdx;

   // loop y end
   dec r11;
   jnz @@addforyloop;

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 
{$IFDEF FPC}
end;
{$ENDIF}
end;

procedure AVXMatrixAddScaleUnAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);
{$IFDEF FPC}
begin
{$ENDIF}
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

   // note: RCX = dest, RDX = destLineWidth, R8 = width, R9 = height
   // prolog - simulate stack

   //iters := -width*sizeof(double);
   mov r10, width;
   imul r10, -8;

   lea rax, dOffset;
   {$IFDEF FPC}vbroadcastsd ymm3, [rax];{$ELSE}db $C4,$E2,$7D,$19,$18;{$ENDIF} 
   lea rax, scale;
   {$IFDEF FPC}vbroadcastsd ymm1, [rax];{$ELSE}db $C4,$E2,$7D,$19,$08;{$ENDIF} 

   // helper registers for the mt1, mt2 and dest pointers
   sub rcx, r10;

   // for y := 0 to height - 1:
   mov r11, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // prefetchw data...
           // prefetchw [rcx + rax];

           // add mul
           {$IFDEF FPC}vmovupd ymm0, [rcx + rax - 128];{$ELSE}db $C5,$FD,$10,$44,$01,$80;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$59,$C1;{$ENDIF} 
           {$IFDEF FPC}vmovupd [rcx + rax - 128], ymm0;{$ELSE}db $C5,$FD,$11,$44,$01,$80;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm2, [rcx + rax - 96];{$ELSE}db $C5,$FD,$10,$54,$01,$A0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm2, ymm2, ymm3;{$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm2, ymm2, ymm1;{$ELSE}db $C5,$ED,$59,$D1;{$ENDIF} 
           {$IFDEF FPC}vmovupd [rcx + rax - 96], ymm2;{$ELSE}db $C5,$FD,$11,$54,$01,$A0;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm0, [rcx + rax - 64];{$ELSE}db $C5,$FD,$10,$44,$01,$C0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$59,$C1;{$ENDIF} 
           {$IFDEF FPC}vmovupd [rcx + rax - 64], ymm0;{$ELSE}db $C5,$FD,$11,$44,$01,$C0;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm2, [rcx + rax - 32];{$ELSE}db $C5,$FD,$10,$54,$01,$E0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm2, ymm2, ymm3;{$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm2, ymm2, ymm1;{$ELSE}db $C5,$ED,$59,$D1;{$ENDIF} 
           {$IFDEF FPC}vmovupd [rcx + rax - 32], ymm2;{$ELSE}db $C5,$FD,$11,$54,$01,$E0;{$ENDIF} 

       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           {$IFDEF FPC}vmovupd xmm0, [rcx + rax];{$ELSE}db $C5,$F9,$10,$04,$01;{$ENDIF} 
           {$IFDEF FPC}vaddpd xmm0, xmm0, xmm3;{$ELSE}db $C5,$F9,$58,$C3;{$ENDIF} 
           {$IFDEF FPC}vmulpd xmm0, xmm0, xmm1;{$ELSE}db $C5,$F9,$59,$C1;{$ENDIF} 

           {$IFDEF FPC}vmovupd [rcx + rax], xmm0;{$ELSE}db $C5,$F9,$11,$04,$01;{$ENDIF} 
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // next line:
       add rcx, rdx;

   // loop y end
   dec r11;
   jnz @@addforyloop;

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 
{$IFDEF FPC}
end;
{$ENDIF}
end;


procedure AVXMatrixAddScaleAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);
{$IFDEF FPC}
begin
{$ENDIF}
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

   // note: RCX = dest, RDX = destLineWidth, R8 = width, R9 = height
   // prolog - simulate stack

   //iters := -(width - 1)*sizeof(double);
   mov r10, width;
   dec r10;
   imul r10, -8;

   lea rax, dOffset;
   {$IFDEF FPC}vbroadcastsd ymm3, [rax];{$ELSE}db $C4,$E2,$7D,$19,$18;{$ENDIF} 
   lea rax, scale;
   {$IFDEF FPC}vbroadcastsd ymm1, [rax];{$ELSE}db $C4,$E2,$7D,$19,$08;{$ENDIF} 


   // helper registers for the mt1, mt2 and dest pointers
   sub rcx, r10;

   // for y := 0 to height - 1:
   mov r11, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // prefetchw data...
           // prefetchw [rcx + rax];

           // add mul
           {$IFDEF FPC}vmovapd ymm0, [rcx + rax - 128];{$ELSE}db $C5,$FD,$28,$44,$01,$80;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$59,$C1;{$ENDIF} 
           {$IFDEF FPC}vmovapd [rcx + rax - 128], ymm0;{$ELSE}db $C5,$FD,$29,$44,$01,$80;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm2, [rcx + rax - 96];{$ELSE}db $C5,$FD,$28,$54,$01,$A0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm2, ymm2, ymm3;{$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm2, ymm2, ymm1;{$ELSE}db $C5,$ED,$59,$D1;{$ENDIF} 
           {$IFDEF FPC}vmovapd [rcx + rax - 96], ymm2;{$ELSE}db $C5,$FD,$29,$54,$01,$A0;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm0, [rcx + rax - 64];{$ELSE}db $C5,$FD,$28,$44,$01,$C0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$59,$C1;{$ENDIF} 
           {$IFDEF FPC}vmovapd [rcx + rax - 64], ymm0;{$ELSE}db $C5,$FD,$29,$44,$01,$C0;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm2, [rcx + rax - 32];{$ELSE}db $C5,$FD,$28,$54,$01,$E0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm2, ymm2, ymm3;{$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm2, ymm2, ymm1;{$ELSE}db $C5,$ED,$59,$D1;{$ENDIF} 
           {$IFDEF FPC}vmovapd [rcx + rax - 32], ymm2;{$ELSE}db $C5,$FD,$29,$54,$01,$E0;{$ENDIF} 

       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           {$IFDEF FPC}vmovapd xmm0, [rcx + rax];{$ELSE}db $C5,$F9,$28,$04,$01;{$ENDIF} 
           {$IFDEF FPC}vaddpd xmm0, xmm0, xmm3;{$ELSE}db $C5,$F9,$58,$C3;{$ENDIF} 
           {$IFDEF FPC}vmulpd xmm0, xmm0, xmm1;{$ELSE}db $C5,$F9,$59,$C1;{$ENDIF} 

           {$IFDEF FPC}vmovapd [rcx + rax], xmm0;{$ELSE}db $C5,$F9,$29,$04,$01;{$ENDIF} 
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last column:
       {$IFDEF FPC}vmovsd xmm0, [rcx];{$ELSE}db $C5,$FB,$10,$01;{$ENDIF} 
       {$IFDEF FPC}vaddsd xmm0, xmm0, xmm3;{$ELSE}db $C5,$FB,$58,$C3;{$ENDIF} 
       {$IFDEF FPC}vmulsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$59,$C1;{$ENDIF} 
       {$IFDEF FPC}vmovsd [rcx], xmm0;{$ELSE}db $C5,$FB,$11,$01;{$ENDIF} 

       // next line:
       add rcx, rdx;

   // loop y end
   dec r11;
   jnz @@addforyloop;

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 
{$IFDEF FPC}
end;
{$ENDIF}
end;

procedure AVXMatrixAddScaleUnAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);
{$IFDEF FPC}
begin
{$ENDIF}
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

   // note: RCX = dest, RDX = destLineWidth, R8 = width, R9 = height
   // prolog - simulate stack

   //iters := -(width - 1)*sizeof(double);
   mov r10, width;
   dec r10;
   imul r10, -8;

   lea rax, dOffset;
   {$IFDEF FPC}vbroadcastsd ymm3, [rax];{$ELSE}db $C4,$E2,$7D,$19,$18;{$ENDIF} 
   lea rax, scale;
   {$IFDEF FPC}vbroadcastsd ymm1, [rax];{$ELSE}db $C4,$E2,$7D,$19,$08;{$ENDIF} 

   // helper registers for the mt1, mt2 and dest pointers
   sub rcx, r10;

   // for y := 0 to height - 1:
   mov r11, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // prefetchw data...
           // prefetchw [rcx + rax];

           // add mul
           {$IFDEF FPC}vmovupd ymm0, [rcx + rax - 128];{$ELSE}db $C5,$FD,$10,$44,$01,$80;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$59,$C1;{$ENDIF} 
           {$IFDEF FPC}vmovupd [rcx + rax - 128], ymm0;{$ELSE}db $C5,$FD,$11,$44,$01,$80;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm2, [rcx + rax - 96];{$ELSE}db $C5,$FD,$10,$54,$01,$A0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm2, ymm2, ymm3;{$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm2, ymm2, ymm1;{$ELSE}db $C5,$ED,$59,$D1;{$ENDIF} 
           {$IFDEF FPC}vmovupd [rcx + rax - 96], ymm2;{$ELSE}db $C5,$FD,$11,$54,$01,$A0;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm0, [rcx + rax - 64];{$ELSE}db $C5,$FD,$10,$44,$01,$C0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$59,$C1;{$ENDIF} 
           {$IFDEF FPC}vmovupd [rcx + rax - 64], ymm0;{$ELSE}db $C5,$FD,$11,$44,$01,$C0;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm2, [rcx + rax - 32];{$ELSE}db $C5,$FD,$10,$54,$01,$E0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm2, ymm2, ymm3;{$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm2, ymm2, ymm1;{$ELSE}db $C5,$ED,$59,$D1;{$ENDIF} 
           {$IFDEF FPC}vmovupd [rcx + rax - 32], ymm2;{$ELSE}db $C5,$FD,$11,$54,$01,$E0;{$ENDIF} 

       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           {$IFDEF FPC}vmovupd xmm0, [rcx + rax];{$ELSE}db $C5,$F9,$10,$04,$01;{$ENDIF} 
           {$IFDEF FPC}vaddpd xmm0, xmm0, xmm3;{$ELSE}db $C5,$F9,$58,$C3;{$ENDIF} 
           {$IFDEF FPC}vmulpd xmm0, xmm0, xmm1;{$ELSE}db $C5,$F9,$59,$C1;{$ENDIF} 

           {$IFDEF FPC}vmovupd [rcx + rax], xmm0;{$ELSE}db $C5,$F9,$11,$04,$01;{$ENDIF} 
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last column:
       {$IFDEF FPC}vmovsd xmm0, [rcx];{$ELSE}db $C5,$FB,$10,$01;{$ENDIF} 
       {$IFDEF FPC}vaddsd xmm0, xmm0, xmm3;{$ELSE}db $C5,$FB,$58,$C3;{$ENDIF} 
       {$IFDEF FPC}vmulsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$59,$C1;{$ENDIF} 
       {$IFDEF FPC}vmovsd [rcx], xmm0;{$ELSE}db $C5,$FB,$11,$01;{$ENDIF} 

       // next line:
       add rcx, rdx;

   // loop y end
   dec r11;
   jnz @@addforyloop;

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 
{$IFDEF FPC}
end;
{$ENDIF}
end;


procedure AVXMatrixScaleAddAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);
{$IFDEF FPC}
begin
{$ENDIF}
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

   // note: RCX = dest, RDX = destLineWidth, R8 = width, R9 = height
   // prolog - simulate stack

   //iters := -width*sizeof(double);
   mov r10, width;
   imul r10, -8;

   lea rax, dOffset;
   {$IFDEF FPC}vbroadcastsd ymm3, [rax];{$ELSE}db $C4,$E2,$7D,$19,$18;{$ENDIF} 
   lea rax, scale;
   {$IFDEF FPC}vbroadcastsd ymm1, [rax];{$ELSE}db $C4,$E2,$7D,$19,$08;{$ENDIF} 

   // helper registers for the mt1, mt2 and dest pointers
   sub rcx, r10;

   // for y := 0 to height - 1:
   mov r11, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // prefetchw data...
           // prefetchw [rcx + rax];

           // addition:
           {$IFDEF FPC}vmovapd ymm0, [rcx + rax - 128];{$ELSE}db $C5,$FD,$28,$44,$01,$80;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$59,$C1;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF FPC}vmovapd [rcx + rax - 128], ymm0;{$ELSE}db $C5,$FD,$29,$44,$01,$80;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm2, [rcx + rax - 96];{$ELSE}db $C5,$FD,$28,$54,$01,$A0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm2, ymm2, ymm1;{$ELSE}db $C5,$ED,$59,$D1;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm2, ymm2, ymm3;{$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF FPC}vmovapd [rcx + rax - 96], ymm2;{$ELSE}db $C5,$FD,$29,$54,$01,$A0;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm0, [rcx + rax - 64];{$ELSE}db $C5,$FD,$28,$44,$01,$C0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$59,$C1;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF FPC}vmovapd [rcx + rax - 64], ymm0;{$ELSE}db $C5,$FD,$29,$44,$01,$C0;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm2, [rcx + rax - 32];{$ELSE}db $C5,$FD,$28,$54,$01,$E0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm2, ymm2, ymm1;{$ELSE}db $C5,$ED,$59,$D1;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm2, ymm2, ymm3;{$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF FPC}vmovapd [rcx + rax - 32], ymm2;{$ELSE}db $C5,$FD,$29,$54,$01,$E0;{$ENDIF} 

       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           {$IFDEF FPC}vmovapd xmm0, [rcx + rax];{$ELSE}db $C5,$F9,$28,$04,$01;{$ENDIF} 
           {$IFDEF FPC}vmulpd xmm0, xmm0, xmm1;{$ELSE}db $C5,$F9,$59,$C1;{$ENDIF} 
           {$IFDEF FPC}vaddpd xmm0, xmm0, xmm3;{$ELSE}db $C5,$F9,$58,$C3;{$ENDIF} 

           {$IFDEF FPC}vmovapd [rcx + rax], xmm0;{$ELSE}db $C5,$F9,$29,$04,$01;{$ENDIF} 
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // next line:
       add rcx, rdx;

   // loop y end
   dec r11;
   jnz @@addforyloop;

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 
{$IFDEF FPC}
end;
{$ENDIF}
end;

procedure AVXMatrixScaleAddUnAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);
{$IFDEF FPC}
begin
{$ENDIF}
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

   // note: RCX = dest, RDX = destLineWidth, R8 = width, R9 = height
   // prolog - simulate stack

   //iters := -width*sizeof(double);
   mov r10, width;
   imul r10, -8;

   lea rax, dOffset;
   {$IFDEF FPC}vbroadcastsd ymm3, [rax];{$ELSE}db $C4,$E2,$7D,$19,$18;{$ENDIF} 
   lea rax, scale;
   {$IFDEF FPC}vbroadcastsd ymm1, [rax];{$ELSE}db $C4,$E2,$7D,$19,$08;{$ENDIF} 

   // helper registers for the mt1, mt2 and dest pointers
   sub rcx, r10;

   // for y := 0 to height - 1:
   mov r11, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // prefetchw data...
           // prefetchw [rcx + rax];

           // addition:
           {$IFDEF FPC}vmovupd ymm0, [rcx + rax - 128];{$ELSE}db $C5,$FD,$10,$44,$01,$80;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$59,$C1;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF FPC}vmovupd [rcx + rax - 128], ymm0;{$ELSE}db $C5,$FD,$11,$44,$01,$80;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm2, [rcx + rax - 96];{$ELSE}db $C5,$FD,$10,$54,$01,$A0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm2, ymm2, ymm1;{$ELSE}db $C5,$ED,$59,$D1;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm2, ymm2, ymm3;{$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF FPC}vmovupd [rcx + rax - 96], ymm2;{$ELSE}db $C5,$FD,$11,$54,$01,$A0;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm0, [rcx + rax - 64];{$ELSE}db $C5,$FD,$10,$44,$01,$C0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$59,$C1;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF FPC}vmovupd [rcx + rax - 64], ymm0;{$ELSE}db $C5,$FD,$11,$44,$01,$C0;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm2, [rcx + rax - 32];{$ELSE}db $C5,$FD,$10,$54,$01,$E0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm2, ymm2, ymm1;{$ELSE}db $C5,$ED,$59,$D1;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm2, ymm2, ymm3;{$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF FPC}vmovupd [rcx + rax - 32], ymm2;{$ELSE}db $C5,$FD,$11,$54,$01,$E0;{$ENDIF} 

       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           {$IFDEF FPC}vmovupd xmm0, [rcx + rax];{$ELSE}db $C5,$F9,$10,$04,$01;{$ENDIF} 
           {$IFDEF FPC}vmulpd xmm0, xmm0, xmm1;{$ELSE}db $C5,$F9,$59,$C1;{$ENDIF} 
           {$IFDEF FPC}vaddpd xmm0, xmm0, xmm3;{$ELSE}db $C5,$F9,$58,$C3;{$ENDIF} 

           {$IFDEF FPC}vmovupd [rcx + rax], xmm0;{$ELSE}db $C5,$F9,$11,$04,$01;{$ENDIF} 
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // next line:
       add rcx, rdx;

   // loop y end
   dec r11;
   jnz @@addforyloop;

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 
{$IFDEF FPC}
end;
{$ENDIF}
end;

procedure AVXMatrixScaleAddAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);
{$IFDEF FPC}
begin
{$ENDIF}
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

   // note: RCX = dest, RDX = destLineWidth, R8 = width, R9 = height
   // prolog - simulate stack

   //iters := -width*sizeof(double);
   mov r10, width;
   dec r10;
   imul r10, -8;

   lea rax, dOffset;
   {$IFDEF FPC}vbroadcastsd ymm3, [rax];{$ELSE}db $C4,$E2,$7D,$19,$18;{$ENDIF} 
   lea rax, scale;
   {$IFDEF FPC}vbroadcastsd ymm1, [rax];{$ELSE}db $C4,$E2,$7D,$19,$08;{$ENDIF} 

   // helper registers for the mt1, mt2 and dest pointers
   sub rcx, r10;

   // for y := 0 to height - 1:
   mov r11, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // prefetchw data...
           // prefetchw [rcx + rax];

           // addition:
           {$IFDEF FPC}vmovapd ymm0, [rcx + rax - 128];{$ELSE}db $C5,$FD,$28,$44,$01,$80;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$59,$C1;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF FPC}vmovapd [rcx + rax - 128], ymm0;{$ELSE}db $C5,$FD,$29,$44,$01,$80;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm2, [rcx + rax - 96];{$ELSE}db $C5,$FD,$28,$54,$01,$A0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm2, ymm2, ymm1;{$ELSE}db $C5,$ED,$59,$D1;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm2, ymm2, ymm3;{$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF FPC}vmovapd [rcx + rax - 96], ymm2;{$ELSE}db $C5,$FD,$29,$54,$01,$A0;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm0, [rcx + rax - 64];{$ELSE}db $C5,$FD,$28,$44,$01,$C0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$59,$C1;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF FPC}vmovapd [rcx + rax - 64], ymm0;{$ELSE}db $C5,$FD,$29,$44,$01,$C0;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm2, [rcx + rax - 32];{$ELSE}db $C5,$FD,$28,$54,$01,$E0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm2, ymm2, ymm1;{$ELSE}db $C5,$ED,$59,$D1;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm2, ymm2, ymm3;{$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF FPC}vmovapd [rcx + rax - 32], ymm2;{$ELSE}db $C5,$FD,$29,$54,$01,$E0;{$ENDIF} 


       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           {$IFDEF FPC}vmovapd xmm0, [rcx + rax];{$ELSE}db $C5,$F9,$28,$04,$01;{$ENDIF} 
           {$IFDEF FPC}vmulpd xmm0, xmm0, xmm1;{$ELSE}db $C5,$F9,$59,$C1;{$ENDIF} 
           {$IFDEF FPC}vaddpd xmm0, xmm0, xmm3;{$ELSE}db $C5,$F9,$58,$C3;{$ENDIF} 

           {$IFDEF FPC}vmovapd [rcx + rax], xmm0;{$ELSE}db $C5,$F9,$29,$04,$01;{$ENDIF} 
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last column:
       {$IFDEF FPC}vmovsd xmm0, [rcx];{$ELSE}db $C5,$FB,$10,$01;{$ENDIF} 
       {$IFDEF FPC}vmulsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$59,$C1;{$ENDIF} 
       {$IFDEF FPC}vaddsd xmm0, xmm0, xmm3;{$ELSE}db $C5,$FB,$58,$C3;{$ENDIF} 
       {$IFDEF FPC}vmovsd [rcx], xmm0;{$ELSE}db $C5,$FB,$11,$01;{$ENDIF} 

       // next line:
       add rcx, rdx;

   // loop y end
   dec r11;
   jnz @@addforyloop;

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 
{$IFDEF FPC}
end;
{$ENDIF}
end;

procedure AVXMatrixScaleAddUnAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);
{$IFDEF FPC}
begin
{$ENDIF}
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

   // note: RCX = dest, RDX = destLineWidth, R8 = width, R9 = height
   // prolog - simulate stack

   //iters := -width*sizeof(double);
   mov r10, width;
   dec r10;
   imul r10, -8;

   lea rax, dOffset;
   {$IFDEF FPC}vbroadcastsd ymm3, [rax];{$ELSE}db $C4,$E2,$7D,$19,$18;{$ENDIF} 
   lea rax, scale;
   {$IFDEF FPC}vbroadcastsd ymm1, [rax];{$ELSE}db $C4,$E2,$7D,$19,$08;{$ENDIF} 

   // helper registers for the mt1, mt2 and dest pointers
   sub rcx, r10;

   // for y := 0 to height - 1:
   mov r11, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // prefetchw data...
           // prefetchw [rcx + rax];

           // addition:
           {$IFDEF FPC}vmovupd ymm0, [rcx + rax - 128];{$ELSE}db $C5,$FD,$10,$44,$01,$80;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$59,$C1;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF FPC}vmovupd [rcx + rax - 128], ymm0;{$ELSE}db $C5,$FD,$11,$44,$01,$80;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm2, [rcx + rax - 96];{$ELSE}db $C5,$FD,$10,$54,$01,$A0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm2, ymm2, ymm1;{$ELSE}db $C5,$ED,$59,$D1;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm2, ymm2, ymm3;{$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF FPC}vmovupd [rcx + rax - 96], ymm2;{$ELSE}db $C5,$FD,$11,$54,$01,$A0;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm0, [rcx + rax - 64];{$ELSE}db $C5,$FD,$10,$44,$01,$C0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$59,$C1;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF FPC}vmovupd [rcx + rax - 64], ymm0;{$ELSE}db $C5,$FD,$11,$44,$01,$C0;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm2, [rcx + rax - 32];{$ELSE}db $C5,$FD,$10,$54,$01,$E0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm2, ymm2, ymm1;{$ELSE}db $C5,$ED,$59,$D1;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm2, ymm2, ymm3;{$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF FPC}vmovupd [rcx + rax - 32], ymm2;{$ELSE}db $C5,$FD,$11,$54,$01,$E0;{$ENDIF} 

       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           {$IFDEF FPC}vmovupd xmm0, [rcx + rax];{$ELSE}db $C5,$F9,$10,$04,$01;{$ENDIF} 
           {$IFDEF FPC}vmulpd xmm0, xmm0, xmm1;{$ELSE}db $C5,$F9,$59,$C1;{$ENDIF} 
           {$IFDEF FPC}vaddpd xmm0, xmm0, xmm3;{$ELSE}db $C5,$F9,$58,$C3;{$ENDIF} 

           {$IFDEF FPC}vmovupd [rcx + rax], xmm0;{$ELSE}db $C5,$F9,$11,$04,$01;{$ENDIF} 
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last column:
       {$IFDEF FPC}vmovsd xmm0, [rcx];{$ELSE}db $C5,$FB,$10,$01;{$ENDIF} 
       {$IFDEF FPC}vmulsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$59,$C1;{$ENDIF} 
       {$IFDEF FPC}vaddsd xmm0, xmm0, xmm3;{$ELSE}db $C5,$FB,$58,$C3;{$ENDIF} 
       {$IFDEF FPC}vmovsd [rcx], xmm0;{$ELSE}db $C5,$FB,$11,$01;{$ENDIF} 

       // next line:
       add rcx, rdx;

   // loop y end
   dec r11;
   jnz @@addforyloop;

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 
{$IFDEF FPC}
end;
{$ENDIF}
end;
{$ENDIF}

end.
