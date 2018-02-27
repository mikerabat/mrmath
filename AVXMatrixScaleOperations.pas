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


unit AVXMatrixScaleOperations;

interface

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}
{$IFNDEF x64}

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

{$IFNDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$S-} {$ENDIF}

procedure AVXMatrixAddScaleAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);
begin
asm
   // prolog - simulate stack
   push ebx;
   push edi;
   push esi;

   mov edx, LineWidth;

   //iters := -width*sizeof(double);
   mov edi, width;
   imul edi, -8;

   lea eax, dOffset;
   {$IFDEF FPC}vbroadcastsd ymm3, [eax];{$ELSE}db $C4,$E2,$7D,$19,$18;{$ENDIF} 
   lea eax, scale;
   {$IFDEF FPC}vbroadcastsd ymm4, [eax];{$ELSE}db $C4,$E2,$7D,$19,$20;{$ENDIF} 


   // helper registers for the mt1, mt2 and dest pointers
   mov ecx, dest;
   sub ecx, edi;

   // for y := 0 to height - 1:
   mov esi, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov eax, edi;
       @addforxloop:
           add eax, 128;
           jg @loopEnd;

           // prefetchw data...
           // prefetchw [ecx + eax];

           // add mul
           {$IFDEF FPC}vmovapd ymm0, [ecx + eax - 128];{$ELSE}db $C5,$FD,$28,$44,$01,$80;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm0, ymm0, ymm4;{$ELSE}db $C5,$FD,$59,$C4;{$ENDIF} 
           {$IFDEF FPC}vmovapd [ecx + eax - 128], ymm0;{$ELSE}db $C5,$FD,$29,$44,$01,$80;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm2, [ecx + eax - 96];{$ELSE}db $C5,$FD,$28,$54,$01,$A0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm2, ymm2, ymm3;{$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm2, ymm2, ymm4;{$ELSE}db $C5,$ED,$59,$D4;{$ENDIF} 
           {$IFDEF FPC}vmovapd [ecx + eax - 96], ymm2;{$ELSE}db $C5,$FD,$29,$54,$01,$A0;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm0, [ecx + eax - 64];{$ELSE}db $C5,$FD,$28,$44,$01,$C0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm0, ymm0, ymm4;{$ELSE}db $C5,$FD,$59,$C4;{$ENDIF} 
           {$IFDEF FPC}vmovapd [ecx + eax - 64], ymm0;{$ELSE}db $C5,$FD,$29,$44,$01,$C0;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm2, [ecx + eax - 32];{$ELSE}db $C5,$FD,$28,$54,$01,$E0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm2, ymm2, ymm3;{$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm2, ymm2, ymm4;{$ELSE}db $C5,$ED,$59,$D4;{$ENDIF} 
           {$IFDEF FPC}vmovapd [ecx + eax - 32], ymm2;{$ELSE}db $C5,$FD,$29,$54,$01,$E0;{$ENDIF} 

       jmp @addforxloop

       @loopEnd:

       sub eax, 128;

       jz @nextLine;

       @addforxloop2:
           {$IFDEF FPC}vmovapd xmm0, [ecx + eax];{$ELSE}db $C5,$F9,$28,$04,$01;{$ENDIF} 
           {$IFDEF FPC}vaddpd xmm0, xmm0, xmm3;{$ELSE}db $C5,$F9,$58,$C3;{$ENDIF} 
           {$IFDEF FPC}vmulpd xmm0, xmm0, xmm4;{$ELSE}db $C5,$F9,$59,$C4;{$ENDIF} 

           {$IFDEF FPC}vmovapd [ecx + eax], xmm0;{$ELSE}db $C5,$F9,$29,$04,$01;{$ENDIF} 
       add eax, 16;
       jnz @addforxloop2;

       @nextLine:

       // next line:
       add ecx, edx;

   // loop y end
   dec esi;
   jnz @@addforyloop;

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;
end;

procedure AVXMatrixAddScaleUnAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);
begin
asm
   // prolog - simulate stack
   push ebx;
   push edi;
   push esi;

   mov edx, LineWidth;

   //iters := -width*sizeof(double);
   mov edi, width;
   imul edi, -8;

   lea eax, dOffset;
   {$IFDEF FPC}vbroadcastsd ymm3, [eax];{$ELSE}db $C4,$E2,$7D,$19,$18;{$ENDIF} 
   lea eax, scale;
   {$IFDEF FPC}vbroadcastsd ymm4, [eax];{$ELSE}db $C4,$E2,$7D,$19,$20;{$ENDIF} 

   // helper registers for the mt1, mt2 and dest pointers
   mov ecx, dest;
   sub ecx, edi;

   // for y := 0 to height - 1:
   mov esi, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov eax, edi;
       @addforxloop:
           add eax, 128;
           jg @loopEnd;

           // prefetchw data...
           // prefetchw [ecx + eax];

           // add mul
           {$IFDEF FPC}vmovupd ymm0, [ecx + eax - 128];{$ELSE}db $C5,$FD,$10,$44,$01,$80;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm0, ymm0, ymm4;{$ELSE}db $C5,$FD,$59,$C4;{$ENDIF} 
           {$IFDEF FPC}vmovupd [ecx + eax - 128], ymm0;{$ELSE}db $C5,$FD,$11,$44,$01,$80;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm2, [ecx + eax - 96];{$ELSE}db $C5,$FD,$10,$54,$01,$A0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm2, ymm2, ymm3;{$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm2, ymm2, ymm4;{$ELSE}db $C5,$ED,$59,$D4;{$ENDIF} 
           {$IFDEF FPC}vmovupd [ecx + eax - 96], ymm2;{$ELSE}db $C5,$FD,$11,$54,$01,$A0;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm0, [ecx + eax - 64];{$ELSE}db $C5,$FD,$10,$44,$01,$C0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm0, ymm0, ymm4;{$ELSE}db $C5,$FD,$59,$C4;{$ENDIF} 
           {$IFDEF FPC}vmovupd [ecx + eax - 64], ymm0;{$ELSE}db $C5,$FD,$11,$44,$01,$C0;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm2, [ecx + eax - 32];{$ELSE}db $C5,$FD,$10,$54,$01,$E0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm2, ymm2, ymm3;{$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm2, ymm2, ymm4;{$ELSE}db $C5,$ED,$59,$D4;{$ENDIF} 
           {$IFDEF FPC}vmovupd [ecx + eax - 32], ymm2;{$ELSE}db $C5,$FD,$11,$54,$01,$E0;{$ENDIF} 

       jmp @addforxloop

       @loopEnd:

       sub eax, 128;

       jz @nextLine;

       @addforxloop2:
           {$IFDEF FPC}vmovupd xmm0, [ecx + eax];{$ELSE}db $C5,$F9,$10,$04,$01;{$ENDIF} 
           {$IFDEF FPC}vaddpd xmm0, xmm0, xmm3;{$ELSE}db $C5,$F9,$58,$C3;{$ENDIF} 
           {$IFDEF FPC}vmulpd xmm0, xmm0, xmm4;{$ELSE}db $C5,$F9,$59,$C4;{$ENDIF} 

           {$IFDEF FPC}vmovupd [ecx + eax], xmm0;{$ELSE}db $C5,$F9,$11,$04,$01;{$ENDIF} 
       add eax, 16;
       jnz @addforxloop2;

       @nextLine:

       // next line:
       add ecx, edx;

   // loop y end
   dec esi;
   jnz @@addforyloop;

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;
end;


procedure AVXMatrixAddScaleAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);
begin
asm
// prolog - simulate stack
   push ebx;
   push edi;
   push esi;

   mov edx, LineWidth;

   //iters := -width*sizeof(double);
   mov edi, width;
   dec edi;
   imul edi, -8;

   lea eax, dOffset;
   {$IFDEF FPC}vbroadcastsd ymm3, [eax];{$ELSE}db $C4,$E2,$7D,$19,$18;{$ENDIF} 
   lea eax, scale;
   {$IFDEF FPC}vbroadcastsd ymm4, [eax];{$ELSE}db $C4,$E2,$7D,$19,$20;{$ENDIF} 

   // helper registers for the mt1, mt2 and dest pointers
   mov ecx, dest;
   sub ecx, edi;

   // for y := 0 to height - 1:
   mov esi, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov eax, edi;
       @addforxloop:
           add eax, 128;
           jg @loopEnd;

           // prefetchw data...
           // prefetchw [ecx + eax];

           // add mul
           {$IFDEF FPC}vmovapd ymm0, [ecx + eax - 128];{$ELSE}db $C5,$FD,$28,$44,$01,$80;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm0, ymm0, ymm4;{$ELSE}db $C5,$FD,$59,$C4;{$ENDIF} 
           {$IFDEF FPC}vmovapd [ecx + eax - 128], ymm0;{$ELSE}db $C5,$FD,$29,$44,$01,$80;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm2, [ecx + eax - 96];{$ELSE}db $C5,$FD,$28,$54,$01,$A0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm2, ymm2, ymm3;{$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm2, ymm2, ymm4;{$ELSE}db $C5,$ED,$59,$D4;{$ENDIF} 
           {$IFDEF FPC}vmovapd [ecx + eax - 96], ymm2;{$ELSE}db $C5,$FD,$29,$54,$01,$A0;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm0, [ecx + eax - 64];{$ELSE}db $C5,$FD,$28,$44,$01,$C0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm0, ymm0, ymm4;{$ELSE}db $C5,$FD,$59,$C4;{$ENDIF} 
           {$IFDEF FPC}vmovapd [ecx + eax - 64], ymm0;{$ELSE}db $C5,$FD,$29,$44,$01,$C0;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm2, [ecx + eax - 32];{$ELSE}db $C5,$FD,$28,$54,$01,$E0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm2, ymm2, ymm3;{$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm2, ymm2, ymm4;{$ELSE}db $C5,$ED,$59,$D4;{$ENDIF} 
           {$IFDEF FPC}vmovapd [ecx + eax - 32], ymm2;{$ELSE}db $C5,$FD,$29,$54,$01,$E0;{$ENDIF} 

       jmp @addforxloop

       @loopEnd:

       sub eax, 128;

       jz @nextLine;

       @addforxloop2:
           {$IFDEF FPC}vmovapd xmm0, [ecx + eax];{$ELSE}db $C5,$F9,$28,$04,$01;{$ENDIF} 
           {$IFDEF FPC}vaddpd xmm0, xmm0, xmm3;{$ELSE}db $C5,$F9,$58,$C3;{$ENDIF} 
           {$IFDEF FPC}vmulpd xmm0, xmm0, xmm4;{$ELSE}db $C5,$F9,$59,$C4;{$ENDIF} 

           {$IFDEF FPC}vmovapd [ecx + eax], xmm0;{$ELSE}db $C5,$F9,$29,$04,$01;{$ENDIF} 
       add eax, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last column:
       {$IFDEF FPC}vmovsd xmm0, [ecx];{$ELSE}db $C5,$FB,$10,$01;{$ENDIF} 
       {$IFDEF FPC}vaddsd xmm0, xmm0, xmm3;{$ELSE}db $C5,$FB,$58,$C3;{$ENDIF} 
       {$IFDEF FPC}vmulsd xmm0, xmm0, xmm4;{$ELSE}db $C5,$FB,$59,$C4;{$ENDIF} 
       {$IFDEF FPC}vmovsd [ecx], xmm0;{$ELSE}db $C5,$FB,$11,$01;{$ENDIF} 

       // next line:
       add ecx, edx;

   // loop y end
   dec esi;
   jnz @@addforyloop;

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;
end;

procedure AVXMatrixAddScaleUnAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);
begin
asm
   // prolog - simulate stack
   push ebx;
   push edi;
   push esi;

   mov edx, LineWidth;

   //iters := -width*sizeof(double);
   mov edi, width;
   dec edi;
   imul edi, -8;

   lea eax, dOffset;
   {$IFDEF FPC}vbroadcastsd ymm3, [eax];{$ELSE}db $C4,$E2,$7D,$19,$18;{$ENDIF} 
   lea eax, scale;
   {$IFDEF FPC}vbroadcastsd ymm4, [eax];{$ELSE}db $C4,$E2,$7D,$19,$20;{$ENDIF} 

   // helper registers for the mt1, mt2 and dest pointers
   mov ecx, dest;
   sub ecx, edi;

   // for y := 0 to height - 1:
   mov esi, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov eax, edi;
       @addforxloop:
           add eax, 128;
           jg @loopEnd;

           // prefetchw data...
           // prefetchw [ecx + eax];

           // add mul
           {$IFDEF FPC}vmovupd ymm0, [ecx + eax - 128];{$ELSE}db $C5,$FD,$10,$44,$01,$80;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm0, ymm0, ymm4;{$ELSE}db $C5,$FD,$59,$C4;{$ENDIF} 
           {$IFDEF FPC}vmovupd [ecx + eax - 128], ymm0;{$ELSE}db $C5,$FD,$11,$44,$01,$80;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm2, [ecx + eax - 96];{$ELSE}db $C5,$FD,$10,$54,$01,$A0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm2, ymm2, ymm3;{$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm2, ymm2, ymm4;{$ELSE}db $C5,$ED,$59,$D4;{$ENDIF} 
           {$IFDEF FPC}vmovupd [ecx + eax - 96], ymm2;{$ELSE}db $C5,$FD,$11,$54,$01,$A0;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm0, [ecx + eax - 64];{$ELSE}db $C5,$FD,$10,$44,$01,$C0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm0, ymm0, ymm4;{$ELSE}db $C5,$FD,$59,$C4;{$ENDIF} 
           {$IFDEF FPC}vmovupd [ecx + eax - 64], ymm0;{$ELSE}db $C5,$FD,$11,$44,$01,$C0;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm2, [ecx + eax - 32];{$ELSE}db $C5,$FD,$10,$54,$01,$E0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm2, ymm2, ymm3;{$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm2, ymm2, ymm4;{$ELSE}db $C5,$ED,$59,$D4;{$ENDIF} 
           {$IFDEF FPC}vmovupd [ecx + eax - 32], ymm2;{$ELSE}db $C5,$FD,$11,$54,$01,$E0;{$ENDIF} 

       jmp @addforxloop

       @loopEnd:

       sub eax, 128;

       jz @nextLine;

       @addforxloop2:
           {$IFDEF FPC}vmovupd xmm0, [ecx + eax];{$ELSE}db $C5,$F9,$10,$04,$01;{$ENDIF} 
           {$IFDEF FPC}vaddpd xmm0, xmm0, xmm3;{$ELSE}db $C5,$F9,$58,$C3;{$ENDIF} 
           {$IFDEF FPC}vmulpd xmm0, xmm0, xmm4;{$ELSE}db $C5,$F9,$59,$C4;{$ENDIF} 

           {$IFDEF FPC}vmovupd [ecx + eax], xmm0;{$ELSE}db $C5,$F9,$11,$04,$01;{$ENDIF} 
       add eax, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last column:
       {$IFDEF FPC}vmovsd xmm0, [ecx];{$ELSE}db $C5,$FB,$10,$01;{$ENDIF} 
       {$IFDEF FPC}vaddsd xmm0, xmm0, xmm3;{$ELSE}db $C5,$FB,$58,$C3;{$ENDIF} 
       {$IFDEF FPC}vmulsd xmm0, xmm0, xmm4;{$ELSE}db $C5,$FB,$59,$C4;{$ENDIF} 
       {$IFDEF FPC}vmovsd [ecx], xmm0;{$ELSE}db $C5,$FB,$11,$01;{$ENDIF} 

       // next line:
       add ecx, edx;

   // loop y end
   dec esi;
   jnz @@addforyloop;

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;
end;


procedure AVXMatrixScaleAddAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);
begin
asm
   // prolog - simulate stack
   push ebx;
   push edi;
   push esi;

   mov edx, LineWidth;

   //iters := -width*sizeof(double);
   mov edi, width;
   imul edi, -8;

   lea eax, dOffset;
   {$IFDEF FPC}vbroadcastsd ymm3, [eax];{$ELSE}db $C4,$E2,$7D,$19,$18;{$ENDIF} 
   lea eax, scale;
   {$IFDEF FPC}vbroadcastsd ymm4, [eax];{$ELSE}db $C4,$E2,$7D,$19,$20;{$ENDIF} 

   // helper registers for the mt1, mt2 and dest pointers
   mov ecx, dest;
   sub ecx, edi;

   // for y := 0 to height - 1:
   mov esi, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov eax, edi;
       @addforxloop:
           add eax, 128;
           jg @loopEnd;

           // prefetchw data...
           // prefetchw [ecx + eax];

           // addition:
           {$IFDEF FPC}vmovapd ymm0, [ecx + eax - 128];{$ELSE}db $C5,$FD,$28,$44,$01,$80;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm0, ymm0, ymm4;{$ELSE}db $C5,$FD,$59,$C4;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF FPC}vmovapd [ecx + eax - 128], ymm0;{$ELSE}db $C5,$FD,$29,$44,$01,$80;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm2, [ecx + eax - 96];{$ELSE}db $C5,$FD,$28,$54,$01,$A0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm2, ymm2, ymm4;{$ELSE}db $C5,$ED,$59,$D4;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm2, ymm2, ymm3;{$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF FPC}vmovapd [ecx + eax - 96], ymm2;{$ELSE}db $C5,$FD,$29,$54,$01,$A0;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm0, [ecx + eax - 64];{$ELSE}db $C5,$FD,$28,$44,$01,$C0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm0, ymm0, ymm4;{$ELSE}db $C5,$FD,$59,$C4;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF FPC}vmovapd [ecx + eax - 64], ymm0;{$ELSE}db $C5,$FD,$29,$44,$01,$C0;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm2, [ecx + eax - 32];{$ELSE}db $C5,$FD,$28,$54,$01,$E0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm2, ymm2, ymm4;{$ELSE}db $C5,$ED,$59,$D4;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm2, ymm2, ymm3;{$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF FPC}vmovapd [ecx + eax - 32], ymm2;{$ELSE}db $C5,$FD,$29,$54,$01,$E0;{$ENDIF} 

       jmp @addforxloop

       @loopEnd:

       sub eax, 128;

       jz @nextLine;

       @addforxloop2:
           {$IFDEF FPC}vmovapd xmm0, [ecx + eax];{$ELSE}db $C5,$F9,$28,$04,$01;{$ENDIF} 
           {$IFDEF FPC}vmulpd xmm0, xmm0, xmm4;{$ELSE}db $C5,$F9,$59,$C4;{$ENDIF} 
           {$IFDEF FPC}vaddpd xmm0, xmm0, xmm3;{$ELSE}db $C5,$F9,$58,$C3;{$ENDIF} 

           {$IFDEF FPC}vmovapd [ecx + eax], xmm0;{$ELSE}db $C5,$F9,$29,$04,$01;{$ENDIF} 
       add eax, 16;
       jnz @addforxloop2;

       @nextLine:

       // next line:
       add ecx, edx;

   // loop y end
   dec esi;
   jnz @@addforyloop;

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;
end;

procedure AVXMatrixScaleAddUnAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);
begin
asm
   // prolog - simulate stack
   push ebx;
   push edi;
   push esi;

   mov edx, LineWidth;

   //iters := -width*sizeof(double);
   mov edi, width;
   imul edi, -8;

   lea eax, dOffset;
   {$IFDEF FPC}vbroadcastsd ymm3, [eax];{$ELSE}db $C4,$E2,$7D,$19,$18;{$ENDIF} 
   lea eax, scale;
   {$IFDEF FPC}vbroadcastsd ymm4, [eax];{$ELSE}db $C4,$E2,$7D,$19,$20;{$ENDIF} 

   // helper registers for the mt1, mt2 and dest pointers
   mov ecx, dest;
   sub ecx, edi;

   // for y := 0 to height - 1:
   mov esi, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov eax, edi;
       @addforxloop:
           add eax, 128;
           jg @loopEnd;

           // prefetchw data...
           // prefetchw [ecx + eax];

           // addition:
           {$IFDEF FPC}vmovupd ymm0, [ecx + eax - 128];{$ELSE}db $C5,$FD,$10,$44,$01,$80;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm0, ymm0, ymm4;{$ELSE}db $C5,$FD,$59,$C4;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF FPC}vmovupd [ecx + eax - 128], ymm0;{$ELSE}db $C5,$FD,$11,$44,$01,$80;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm2, [ecx + eax - 96];{$ELSE}db $C5,$FD,$10,$54,$01,$A0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm2, ymm2, ymm4;{$ELSE}db $C5,$ED,$59,$D4;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm2, ymm2, ymm3;{$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF FPC}vmovupd [ecx + eax - 96], ymm2;{$ELSE}db $C5,$FD,$11,$54,$01,$A0;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm0, [ecx + eax - 64];{$ELSE}db $C5,$FD,$10,$44,$01,$C0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm0, ymm0, ymm4;{$ELSE}db $C5,$FD,$59,$C4;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF FPC}vmovupd [ecx + eax - 64], ymm0;{$ELSE}db $C5,$FD,$11,$44,$01,$C0;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm2, [ecx + eax - 32];{$ELSE}db $C5,$FD,$10,$54,$01,$E0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm2, ymm2, ymm4;{$ELSE}db $C5,$ED,$59,$D4;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm2, ymm2, ymm3;{$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF FPC}vmovupd [ecx + eax - 32], ymm2;{$ELSE}db $C5,$FD,$11,$54,$01,$E0;{$ENDIF} 

       jmp @addforxloop

       @loopEnd:

       sub eax, 128;

       jz @nextLine;

       @addforxloop2:
           {$IFDEF FPC}vmovupd xmm0, [ecx + eax];{$ELSE}db $C5,$F9,$10,$04,$01;{$ENDIF} 
           {$IFDEF FPC}vmulpd xmm0, xmm0, xmm4;{$ELSE}db $C5,$F9,$59,$C4;{$ENDIF} 
           {$IFDEF FPC}vaddpd xmm0, xmm0, xmm3;{$ELSE}db $C5,$F9,$58,$C3;{$ENDIF} 

           {$IFDEF FPC}vmovupd [ecx + eax], xmm0;{$ELSE}db $C5,$F9,$11,$04,$01;{$ENDIF} 
       add eax, 16;
       jnz @addforxloop2;

       @nextLine:

       // next line:
       add ecx, edx;

   // loop y end
   dec esi;
   jnz @@addforyloop;

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;
end;

procedure AVXMatrixScaleAddAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);
begin
asm
   // prolog - simulate stack
   push ebx;
   push edi;
   push esi;

   mov edx, LineWidth;

   //iters := -width*sizeof(double);
   mov edi, width;
   dec edi;
   imul edi, -8;

   lea eax, dOffset;
   {$IFDEF FPC}vbroadcastsd ymm3, [eax];{$ELSE}db $C4,$E2,$7D,$19,$18;{$ENDIF} 
   lea eax, scale;
   {$IFDEF FPC}vbroadcastsd ymm4, [eax];{$ELSE}db $C4,$E2,$7D,$19,$20;{$ENDIF} 

   // helper registers for the mt1, mt2 and dest pointers
   mov ecx, dest;
   sub ecx, edi;

   // for y := 0 to height - 1:
   mov esi, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov eax, edi;
       @addforxloop:
           add eax, 128;
           jg @loopEnd;

           // prefetchw data...
           // prefetchw [ecx + eax];

           // addition:
           {$IFDEF FPC}vmovapd ymm0, [ecx + eax - 128];{$ELSE}db $C5,$FD,$28,$44,$01,$80;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm0, ymm0, ymm4;{$ELSE}db $C5,$FD,$59,$C4;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF FPC}vmovapd [ecx + eax - 128], ymm0;{$ELSE}db $C5,$FD,$29,$44,$01,$80;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm2, [ecx + eax - 96];{$ELSE}db $C5,$FD,$28,$54,$01,$A0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm2, ymm2, ymm4;{$ELSE}db $C5,$ED,$59,$D4;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm2, ymm2, ymm3;{$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF FPC}vmovapd [ecx + eax - 96], ymm2;{$ELSE}db $C5,$FD,$29,$54,$01,$A0;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm0, [ecx + eax - 64];{$ELSE}db $C5,$FD,$28,$44,$01,$C0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm0, ymm0, ymm4;{$ELSE}db $C5,$FD,$59,$C4;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF FPC}vmovapd [ecx + eax - 64], ymm0;{$ELSE}db $C5,$FD,$29,$44,$01,$C0;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm2, [ecx + eax - 32];{$ELSE}db $C5,$FD,$28,$54,$01,$E0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm2, ymm2, ymm4;{$ELSE}db $C5,$ED,$59,$D4;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm2, ymm2, ymm3;{$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF FPC}vmovapd [ecx + eax - 32], ymm2;{$ELSE}db $C5,$FD,$29,$54,$01,$E0;{$ENDIF} 


       jmp @addforxloop

       @loopEnd:

       sub eax, 128;

       jz @nextLine;

       @addforxloop2:
           {$IFDEF FPC}vmovapd xmm0, [ecx + eax];{$ELSE}db $C5,$F9,$28,$04,$01;{$ENDIF} 
           {$IFDEF FPC}vmulpd xmm0, xmm0, xmm4;{$ELSE}db $C5,$F9,$59,$C4;{$ENDIF} 
           {$IFDEF FPC}vaddpd xmm0, xmm0, xmm3;{$ELSE}db $C5,$F9,$58,$C3;{$ENDIF} 

           {$IFDEF FPC}vmovapd [ecx + eax], xmm0;{$ELSE}db $C5,$F9,$29,$04,$01;{$ENDIF} 
       add eax, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last column:
       {$IFDEF FPC}vmovsd xmm0, [ecx];{$ELSE}db $C5,$FB,$10,$01;{$ENDIF} 
       {$IFDEF FPC}vmulsd xmm0, xmm0, xmm4;{$ELSE}db $C5,$FB,$59,$C4;{$ENDIF} 
       {$IFDEF FPC}vaddsd xmm0, xmm0, xmm3;{$ELSE}db $C5,$FB,$58,$C3;{$ENDIF} 
       {$IFDEF FPC}vmovsd [ecx], xmm0;{$ELSE}db $C5,$FB,$11,$01;{$ENDIF} 

       // next line:
       add ecx, edx;

   // loop y end
   dec esi;
   jnz @@addforyloop;

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;
end;

procedure AVXMatrixScaleAddUnAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);
begin
asm
   // prolog - simulate stack
   push ebx;
   push edi;
   push esi;

   mov edx, LineWidth;

   //iters := -width*sizeof(double);
   mov edi, width;
   dec edi;
   imul edi, -8;

   lea eax, dOffset;
   {$IFDEF FPC}vbroadcastsd ymm3, [eax];{$ELSE}db $C4,$E2,$7D,$19,$18;{$ENDIF} 
   lea eax, scale;
   {$IFDEF FPC}vbroadcastsd ymm4, [eax];{$ELSE}db $C4,$E2,$7D,$19,$20;{$ENDIF} 

   // helper registers for the mt1, mt2 and dest pointers
   mov ecx, dest;
   sub ecx, edi;

   // for y := 0 to height - 1:
   mov esi, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov eax, edi;
       @addforxloop:
           add eax, 128;
           jg @loopEnd;

           // prefetchw data...
           // prefetchw [ecx + eax];

           // addition:
           {$IFDEF FPC}vmovupd ymm0, [ecx + eax - 128];{$ELSE}db $C5,$FD,$10,$44,$01,$80;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm0, ymm0, ymm4;{$ELSE}db $C5,$FD,$59,$C4;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF FPC}vmovupd [ecx + eax - 128], ymm0;{$ELSE}db $C5,$FD,$11,$44,$01,$80;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm2, [ecx + eax - 96];{$ELSE}db $C5,$FD,$10,$54,$01,$A0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm2, ymm2, ymm4;{$ELSE}db $C5,$ED,$59,$D4;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm2, ymm2, ymm3;{$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF FPC}vmovupd [ecx + eax - 96], ymm2;{$ELSE}db $C5,$FD,$11,$54,$01,$A0;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm0, [ecx + eax - 64];{$ELSE}db $C5,$FD,$10,$44,$01,$C0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm0, ymm0, ymm4;{$ELSE}db $C5,$FD,$59,$C4;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF FPC}vmovupd [ecx + eax - 64], ymm0;{$ELSE}db $C5,$FD,$11,$44,$01,$C0;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm2, [ecx + eax - 32];{$ELSE}db $C5,$FD,$10,$54,$01,$E0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm2, ymm2, ymm4;{$ELSE}db $C5,$ED,$59,$D4;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm2, ymm2, ymm3;{$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF FPC}vmovupd [ecx + eax - 32], ymm2;{$ELSE}db $C5,$FD,$11,$54,$01,$E0;{$ENDIF} 

       jmp @addforxloop

       @loopEnd:

       sub eax, 128;

       jz @nextLine;

       @addforxloop2:
           {$IFDEF FPC}vmovupd xmm0, [ecx + eax];{$ELSE}db $C5,$F9,$10,$04,$01;{$ENDIF} 
           {$IFDEF FPC}vmulpd xmm0, xmm0, xmm4;{$ELSE}db $C5,$F9,$59,$C4;{$ENDIF} 
           {$IFDEF FPC}vaddpd xmm0, xmm0, xmm3;{$ELSE}db $C5,$F9,$58,$C3;{$ENDIF} 

           {$IFDEF FPC}vmovupd [ecx + eax], xmm0;{$ELSE}db $C5,$F9,$11,$04,$01;{$ENDIF} 
       add eax, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last column:
       {$IFDEF FPC}vmovsd xmm0, [ecx];{$ELSE}db $C5,$FB,$10,$01;{$ENDIF} 
       {$IFDEF FPC}vmulsd xmm0, xmm0, xmm4;{$ELSE}db $C5,$FB,$59,$C4;{$ENDIF} 
       {$IFDEF FPC}vaddsd xmm0, xmm0, xmm3;{$ELSE}db $C5,$FB,$58,$C3;{$ENDIF} 
       {$IFDEF FPC}vmovsd [ecx], xmm0;{$ELSE}db $C5,$FB,$11,$01;{$ENDIF} 

       // next line:
       add ecx, edx;

   // loop y end
   dec esi;
   jnz @@addforyloop;

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;
end;

{$ENDIF}

end.
