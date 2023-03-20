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

{$I 'mrMath_CPU.inc'}

{$IFNDEF x64}

procedure AVXMatrixAddScaleAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : NativeInt; const dOffset, Scale : double); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure AVXMatrixAddScaleUnAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : NativeInt; const dOffset, Scale : double); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

procedure AVXMatrixAddScaleAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : NativeInt; const dOffset, Scale : double); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure AVXMatrixAddScaleUnAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : NativeInt; const dOffset, Scale : double); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

procedure AVXMatrixScaleAddAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : NativeInt; const dOffset, Scale : double); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure AVXMatrixScaleAddUnAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : NativeInt; const dOffset, Scale : double); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

procedure AVXMatrixScaleAddAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : NativeInt; const dOffset, Scale : double); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure AVXMatrixScaleAddUnAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : NativeInt; const dOffset, Scale : double); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

procedure AVXMatrixAddAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : NativeInt; const dOffset : double); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure AVXMatrixAddUnAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : NativeInt; const dOffset : double); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

procedure AVXMatrixAddAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : NativeInt; const dOffset : double); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure AVXMatrixAddUnAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : NativeInt; const dOffset : double); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

{$ENDIF}

implementation

{$IFNDEF x64}

procedure AVXMatrixAddScaleAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : NativeInt; const dOffset, Scale : double);
asm
   // prolog - simulate stack
   push ebx;
   push edi;
   push esi;

   //iters := -width*sizeof(double);
   mov ecx, width;
   imul ecx, -8;

   lea edi, dOffset;
   {$IFDEF FPC}vbroadcastsd ymm3, [edi];{$ELSE}db $C4,$E2,$7D,$19,$1F;{$ENDIF} 
   lea edi, scale;
   {$IFDEF FPC}vbroadcastsd ymm4, [edi];{$ELSE}db $C4,$E2,$7D,$19,$27;{$ENDIF} 

   // helper registers for the mt1, mt2 and dest pointers
   sub eax, ecx;

   // for y := 0 to height - 1:
   mov esi, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, ecx;
       @addforxloop:
           add edi, 128;
           jg @loopEnd;

           // prefetchw data...
           // prefetchw [eax + edi];

           // add mul
           {$IFDEF FPC}vmovapd ymm0, [eax + edi - 128];{$ELSE}db $C5,$FD,$28,$44,$38,$80;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm0, ymm0, ymm4;{$ELSE}db $C5,$FD,$59,$C4;{$ENDIF} 
           {$IFDEF FPC}vmovapd [eax + edi - 128], ymm0;{$ELSE}db $C5,$FD,$29,$44,$38,$80;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm2, [eax + edi - 96];{$ELSE}db $C5,$FD,$28,$54,$38,$A0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm2, ymm2, ymm3;{$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm2, ymm2, ymm4;{$ELSE}db $C5,$ED,$59,$D4;{$ENDIF} 
           {$IFDEF FPC}vmovapd [eax + edi - 96], ymm2;{$ELSE}db $C5,$FD,$29,$54,$38,$A0;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm0, [eax + edi - 64];{$ELSE}db $C5,$FD,$28,$44,$38,$C0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm0, ymm0, ymm4;{$ELSE}db $C5,$FD,$59,$C4;{$ENDIF} 
           {$IFDEF FPC}vmovapd [eax + edi - 64], ymm0;{$ELSE}db $C5,$FD,$29,$44,$38,$C0;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm2, [eax + edi - 32];{$ELSE}db $C5,$FD,$28,$54,$38,$E0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm2, ymm2, ymm3;{$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm2, ymm2, ymm4;{$ELSE}db $C5,$ED,$59,$D4;{$ENDIF} 
           {$IFDEF FPC}vmovapd [eax + edi - 32], ymm2;{$ELSE}db $C5,$FD,$29,$54,$38,$E0;{$ENDIF} 

       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @nextLine;

       @addforxloop2:
           {$IFDEF FPC}vmovapd xmm0, [eax + edi];{$ELSE}db $C5,$F9,$28,$04,$38;{$ENDIF} 
           {$IFDEF FPC}vaddpd xmm0, xmm0, xmm3;{$ELSE}db $C5,$F9,$58,$C3;{$ENDIF} 
           {$IFDEF FPC}vmulpd xmm0, xmm0, xmm4;{$ELSE}db $C5,$F9,$59,$C4;{$ENDIF} 

           {$IFDEF FPC}vmovapd [eax + edi], xmm0;{$ELSE}db $C5,$F9,$29,$04,$38;{$ENDIF} 
       add edi, 16;
       jnz @addforxloop2;

       @nextLine:

       // next line:
       add eax, edx;

   // loop y end
   dec esi;
   jnz @@addforyloop;

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;

procedure AVXMatrixAddScaleUnAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : NativeInt; const dOffset, Scale : double);
asm
   // prolog - simulate stack
   push ebx;
   push edi;
   push esi;

   //iters := -width*sizeof(double);
   imul ecx, -8;

   lea edi, dOffset;
   {$IFDEF FPC}vbroadcastsd ymm3, [edi];{$ELSE}db $C4,$E2,$7D,$19,$1F;{$ENDIF} 
   lea edi, scale;
   {$IFDEF FPC}vbroadcastsd ymm4, [edi];{$ELSE}db $C4,$E2,$7D,$19,$27;{$ENDIF} 

   // helper registers for the mt1, mt2 and dest pointers
   sub eax, ecx;

   // for y := 0 to height - 1:
   mov esi, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, ecx;
       @addforxloop:
           add edi, 128;
           jg @loopEnd;

           // prefetchw data...
           // prefetchw [eax + edi];

           // add mul
           {$IFDEF FPC}vmovupd ymm0, [eax + edi - 128];{$ELSE}db $C5,$FD,$10,$44,$38,$80;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm0, ymm0, ymm4;{$ELSE}db $C5,$FD,$59,$C4;{$ENDIF} 
           {$IFDEF FPC}vmovupd [eax + edi - 128], ymm0;{$ELSE}db $C5,$FD,$11,$44,$38,$80;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm2, [eax + edi - 96];{$ELSE}db $C5,$FD,$10,$54,$38,$A0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm2, ymm2, ymm3;{$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm2, ymm2, ymm4;{$ELSE}db $C5,$ED,$59,$D4;{$ENDIF} 
           {$IFDEF FPC}vmovupd [eax + edi - 96], ymm2;{$ELSE}db $C5,$FD,$11,$54,$38,$A0;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm0, [eax + edi - 64];{$ELSE}db $C5,$FD,$10,$44,$38,$C0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm0, ymm0, ymm4;{$ELSE}db $C5,$FD,$59,$C4;{$ENDIF} 
           {$IFDEF FPC}vmovupd [eax + edi - 64], ymm0;{$ELSE}db $C5,$FD,$11,$44,$38,$C0;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm2, [eax + edi - 32];{$ELSE}db $C5,$FD,$10,$54,$38,$E0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm2, ymm2, ymm3;{$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm2, ymm2, ymm4;{$ELSE}db $C5,$ED,$59,$D4;{$ENDIF} 
           {$IFDEF FPC}vmovupd [eax + edi - 32], ymm2;{$ELSE}db $C5,$FD,$11,$54,$38,$E0;{$ENDIF} 

       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @nextLine;

       @addforxloop2:
           {$IFDEF FPC}vmovupd xmm0, [eax + edi];{$ELSE}db $C5,$F9,$10,$04,$38;{$ENDIF} 
           {$IFDEF FPC}vaddpd xmm0, xmm0, xmm3;{$ELSE}db $C5,$F9,$58,$C3;{$ENDIF} 
           {$IFDEF FPC}vmulpd xmm0, xmm0, xmm4;{$ELSE}db $C5,$F9,$59,$C4;{$ENDIF} 

           {$IFDEF FPC}vmovupd [eax + edi], xmm0;{$ELSE}db $C5,$F9,$11,$04,$38;{$ENDIF} 
       add edi, 16;
       jnz @addforxloop2;

       @nextLine:

       // next line:
       add eax, edx;

   // loop y end
   dec esi;
   jnz @@addforyloop;

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;


procedure AVXMatrixAddScaleAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : NativeInt; const dOffset, Scale : double);
asm
// prolog - simulate stack
   push ebx;
   push edi;
   push esi;

   //iters := -width*sizeof(double);
   dec ecx;
   imul ecx, -8;

   lea edi, dOffset;
   {$IFDEF FPC}vbroadcastsd ymm3, [edi];{$ELSE}db $C4,$E2,$7D,$19,$1F;{$ENDIF} 
   lea edi, scale;
   {$IFDEF FPC}vbroadcastsd ymm4, [edi];{$ELSE}db $C4,$E2,$7D,$19,$27;{$ENDIF} 

   // helper registers for the mt1, mt2 and dest pointers
   sub eax, ecx;

   // for y := 0 to height - 1:
   mov esi, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, ecx;
       @addforxloop:
           add edi, 128;
           jg @loopEnd;

           // prefetchw data...
           // prefetchw [eax + edi];

           // add mul
           {$IFDEF FPC}vmovapd ymm0, [eax + edi - 128];{$ELSE}db $C5,$FD,$28,$44,$38,$80;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm0, ymm0, ymm4;{$ELSE}db $C5,$FD,$59,$C4;{$ENDIF} 
           {$IFDEF FPC}vmovapd [eax + edi - 128], ymm0;{$ELSE}db $C5,$FD,$29,$44,$38,$80;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm2, [eax + edi - 96];{$ELSE}db $C5,$FD,$28,$54,$38,$A0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm2, ymm2, ymm3;{$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm2, ymm2, ymm4;{$ELSE}db $C5,$ED,$59,$D4;{$ENDIF} 
           {$IFDEF FPC}vmovapd [eax + edi - 96], ymm2;{$ELSE}db $C5,$FD,$29,$54,$38,$A0;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm0, [eax + edi - 64];{$ELSE}db $C5,$FD,$28,$44,$38,$C0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm0, ymm0, ymm4;{$ELSE}db $C5,$FD,$59,$C4;{$ENDIF} 
           {$IFDEF FPC}vmovapd [eax + edi - 64], ymm0;{$ELSE}db $C5,$FD,$29,$44,$38,$C0;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm2, [eax + edi - 32];{$ELSE}db $C5,$FD,$28,$54,$38,$E0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm2, ymm2, ymm3;{$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm2, ymm2, ymm4;{$ELSE}db $C5,$ED,$59,$D4;{$ENDIF} 
           {$IFDEF FPC}vmovapd [eax + edi - 32], ymm2;{$ELSE}db $C5,$FD,$29,$54,$38,$E0;{$ENDIF} 

       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @nextLine;

       @addforxloop2:
           {$IFDEF FPC}vmovapd xmm0, [eax + edi];{$ELSE}db $C5,$F9,$28,$04,$38;{$ENDIF} 
           {$IFDEF FPC}vaddpd xmm0, xmm0, xmm3;{$ELSE}db $C5,$F9,$58,$C3;{$ENDIF} 
           {$IFDEF FPC}vmulpd xmm0, xmm0, xmm4;{$ELSE}db $C5,$F9,$59,$C4;{$ENDIF} 

           {$IFDEF FPC}vmovapd [eax + edi], xmm0;{$ELSE}db $C5,$F9,$29,$04,$38;{$ENDIF} 
       add edi, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last column:
       {$IFDEF FPC}vmovsd xmm0, [eax];{$ELSE}db $C5,$FB,$10,$00;{$ENDIF} 
       {$IFDEF FPC}vaddsd xmm0, xmm0, xmm3;{$ELSE}db $C5,$FB,$58,$C3;{$ENDIF} 
       {$IFDEF FPC}vmulsd xmm0, xmm0, xmm4;{$ELSE}db $C5,$FB,$59,$C4;{$ENDIF} 
       {$IFDEF FPC}vmovsd [eax], xmm0;{$ELSE}db $C5,$FB,$11,$00;{$ENDIF} 

       // next line:
       add eax, edx;

   // loop y end
   dec esi;
   jnz @@addforyloop;

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;

procedure AVXMatrixAddScaleUnAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : NativeInt; const dOffset, Scale : double);
asm
   // prolog - simulate stack
   push ebx;
   push edi;
   push esi;

   //iters := -width*sizeof(double);
   dec ecx;
   imul ecx, -8;

   lea edi, dOffset;
   {$IFDEF FPC}vbroadcastsd ymm3, [edi];{$ELSE}db $C4,$E2,$7D,$19,$1F;{$ENDIF} 
   lea edi, scale;
   {$IFDEF FPC}vbroadcastsd ymm4, [edi];{$ELSE}db $C4,$E2,$7D,$19,$27;{$ENDIF} 

   // helper registers for the mt1, mt2 and dest pointers
   sub eax, ecx;

   // for y := 0 to height - 1:
   mov esi, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, ecx;
       @addforxloop:
           add edi, 128;
           jg @loopEnd;

           // prefetchw data...
           // prefetchw [eax + edi];

           // add mul
           {$IFDEF FPC}vmovupd ymm0, [eax + edi - 128];{$ELSE}db $C5,$FD,$10,$44,$38,$80;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm0, ymm0, ymm4;{$ELSE}db $C5,$FD,$59,$C4;{$ENDIF} 
           {$IFDEF FPC}vmovupd [eax + edi - 128], ymm0;{$ELSE}db $C5,$FD,$11,$44,$38,$80;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm2, [eax + edi - 96];{$ELSE}db $C5,$FD,$10,$54,$38,$A0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm2, ymm2, ymm3;{$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm2, ymm2, ymm4;{$ELSE}db $C5,$ED,$59,$D4;{$ENDIF} 
           {$IFDEF FPC}vmovupd [eax + edi - 96], ymm2;{$ELSE}db $C5,$FD,$11,$54,$38,$A0;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm0, [eax + edi - 64];{$ELSE}db $C5,$FD,$10,$44,$38,$C0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm0, ymm0, ymm4;{$ELSE}db $C5,$FD,$59,$C4;{$ENDIF} 
           {$IFDEF FPC}vmovupd [eax + edi - 64], ymm0;{$ELSE}db $C5,$FD,$11,$44,$38,$C0;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm2, [eax + edi - 32];{$ELSE}db $C5,$FD,$10,$54,$38,$E0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm2, ymm2, ymm3;{$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm2, ymm2, ymm4;{$ELSE}db $C5,$ED,$59,$D4;{$ENDIF} 
           {$IFDEF FPC}vmovupd [eax + edi - 32], ymm2;{$ELSE}db $C5,$FD,$11,$54,$38,$E0;{$ENDIF} 

       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @nextLine;

       @addforxloop2:
           {$IFDEF FPC}vmovupd xmm0, [eax + edi];{$ELSE}db $C5,$F9,$10,$04,$38;{$ENDIF} 
           {$IFDEF FPC}vaddpd xmm0, xmm0, xmm3;{$ELSE}db $C5,$F9,$58,$C3;{$ENDIF} 
           {$IFDEF FPC}vmulpd xmm0, xmm0, xmm4;{$ELSE}db $C5,$F9,$59,$C4;{$ENDIF} 

           {$IFDEF FPC}vmovupd [eax + edi], xmm0;{$ELSE}db $C5,$F9,$11,$04,$38;{$ENDIF} 
       add edi, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last column:
       {$IFDEF FPC}vmovsd xmm0, [eax];{$ELSE}db $C5,$FB,$10,$00;{$ENDIF} 
       {$IFDEF FPC}vaddsd xmm0, xmm0, xmm3;{$ELSE}db $C5,$FB,$58,$C3;{$ENDIF} 
       {$IFDEF FPC}vmulsd xmm0, xmm0, xmm4;{$ELSE}db $C5,$FB,$59,$C4;{$ENDIF} 
       {$IFDEF FPC}vmovsd [eax], xmm0;{$ELSE}db $C5,$FB,$11,$00;{$ENDIF} 

       // next line:
       add eax, edx;

   // loop y end
   dec esi;
   jnz @@addforyloop;

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;


procedure AVXMatrixScaleAddAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : NativeInt; const dOffset, Scale : double);
asm
   // prolog - simulate stack
   push ebx;
   push edi;
   push esi;

   //iters := -width*sizeof(double);
   imul ecx, -8;

   lea edi, dOffset;
   {$IFDEF FPC}vbroadcastsd ymm3, [edi];{$ELSE}db $C4,$E2,$7D,$19,$1F;{$ENDIF} 
   lea edi, scale;
   {$IFDEF FPC}vbroadcastsd ymm4, [edi];{$ELSE}db $C4,$E2,$7D,$19,$27;{$ENDIF} 

   // helper registers for the mt1, mt2 and dest pointers
   sub eax, ecx;

   // for y := 0 to height - 1:
   mov esi, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, ecx;
       @addforxloop:
           add edi, 128;
           jg @loopEnd;

           // prefetchw data...
           // prefetchw [eax + edi];

           // addition:
           {$IFDEF FPC}vmovapd ymm0, [eax + edi - 128];{$ELSE}db $C5,$FD,$28,$44,$38,$80;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm0, ymm0, ymm4;{$ELSE}db $C5,$FD,$59,$C4;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF FPC}vmovapd [eax + edi - 128], ymm0;{$ELSE}db $C5,$FD,$29,$44,$38,$80;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm2, [eax + edi - 96];{$ELSE}db $C5,$FD,$28,$54,$38,$A0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm2, ymm2, ymm4;{$ELSE}db $C5,$ED,$59,$D4;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm2, ymm2, ymm3;{$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF FPC}vmovapd [eax + edi - 96], ymm2;{$ELSE}db $C5,$FD,$29,$54,$38,$A0;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm0, [eax + edi - 64];{$ELSE}db $C5,$FD,$28,$44,$38,$C0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm0, ymm0, ymm4;{$ELSE}db $C5,$FD,$59,$C4;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF FPC}vmovapd [eax + edi - 64], ymm0;{$ELSE}db $C5,$FD,$29,$44,$38,$C0;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm2, [eax + edi - 32];{$ELSE}db $C5,$FD,$28,$54,$38,$E0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm2, ymm2, ymm4;{$ELSE}db $C5,$ED,$59,$D4;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm2, ymm2, ymm3;{$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF FPC}vmovapd [eax + edi - 32], ymm2;{$ELSE}db $C5,$FD,$29,$54,$38,$E0;{$ENDIF} 

       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @nextLine;

       @addforxloop2:
           {$IFDEF FPC}vmovapd xmm0, [eax + edi];{$ELSE}db $C5,$F9,$28,$04,$38;{$ENDIF} 
           {$IFDEF FPC}vmulpd xmm0, xmm0, xmm4;{$ELSE}db $C5,$F9,$59,$C4;{$ENDIF} 
           {$IFDEF FPC}vaddpd xmm0, xmm0, xmm3;{$ELSE}db $C5,$F9,$58,$C3;{$ENDIF} 

           {$IFDEF FPC}vmovapd [eax + edi], xmm0;{$ELSE}db $C5,$F9,$29,$04,$38;{$ENDIF} 
       add edi, 16;
       jnz @addforxloop2;

       @nextLine:

       // next line:
       add eax, edx;

   // loop y end
   dec esi;
   jnz @@addforyloop;

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;

procedure AVXMatrixScaleAddUnAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : NativeInt; const dOffset, Scale : double);
asm
   // prolog - simulate stack
   push ebx;
   push edi;
   push esi;

   //iters := -width*sizeof(double);
   imul ecx, -8;

   lea edi, dOffset;
   {$IFDEF FPC}vbroadcastsd ymm3, [edi];{$ELSE}db $C4,$E2,$7D,$19,$1F;{$ENDIF} 
   lea edi, scale;
   {$IFDEF FPC}vbroadcastsd ymm4, [edi];{$ELSE}db $C4,$E2,$7D,$19,$27;{$ENDIF} 

   // helper registers for the mt1, mt2 and dest pointers
   sub eax, ecx;

   // for y := 0 to height - 1:
   mov esi, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, ecx;
       @addforxloop:
           add edi, 128;
           jg @loopEnd;

           // prefetchw data...
           // prefetchw [eax + edi];

           // addition:
           {$IFDEF FPC}vmovupd ymm0, [eax + edi - 128];{$ELSE}db $C5,$FD,$10,$44,$38,$80;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm0, ymm0, ymm4;{$ELSE}db $C5,$FD,$59,$C4;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF FPC}vmovupd [eax + edi - 128], ymm0;{$ELSE}db $C5,$FD,$11,$44,$38,$80;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm2, [eax + edi - 96];{$ELSE}db $C5,$FD,$10,$54,$38,$A0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm2, ymm2, ymm4;{$ELSE}db $C5,$ED,$59,$D4;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm2, ymm2, ymm3;{$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF FPC}vmovupd [eax + edi - 96], ymm2;{$ELSE}db $C5,$FD,$11,$54,$38,$A0;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm0, [eax + edi - 64];{$ELSE}db $C5,$FD,$10,$44,$38,$C0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm0, ymm0, ymm4;{$ELSE}db $C5,$FD,$59,$C4;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF FPC}vmovupd [eax + edi - 64], ymm0;{$ELSE}db $C5,$FD,$11,$44,$38,$C0;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm2, [eax + edi - 32];{$ELSE}db $C5,$FD,$10,$54,$38,$E0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm2, ymm2, ymm4;{$ELSE}db $C5,$ED,$59,$D4;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm2, ymm2, ymm3;{$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF FPC}vmovupd [eax + edi - 32], ymm2;{$ELSE}db $C5,$FD,$11,$54,$38,$E0;{$ENDIF} 

       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @nextLine;

       @addforxloop2:
           {$IFDEF FPC}vmovupd xmm0, [eax + edi];{$ELSE}db $C5,$F9,$10,$04,$38;{$ENDIF} 
           {$IFDEF FPC}vmulpd xmm0, xmm0, xmm4;{$ELSE}db $C5,$F9,$59,$C4;{$ENDIF} 
           {$IFDEF FPC}vaddpd xmm0, xmm0, xmm3;{$ELSE}db $C5,$F9,$58,$C3;{$ENDIF} 

           {$IFDEF FPC}vmovupd [eax + edi], xmm0;{$ELSE}db $C5,$F9,$11,$04,$38;{$ENDIF} 
       add edi, 16;
       jnz @addforxloop2;

       @nextLine:

       // next line:
       add eax, edx;

   // loop y end
   dec esi;
   jnz @@addforyloop;

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;

procedure AVXMatrixScaleAddAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : NativeInt; const dOffset, Scale : double);
asm
   // prolog - simulate stack
   push ebx;
   push edi;
   push esi;

   //iters := -width*sizeof(double);
   dec ecx;
   imul ecx, -8;

   lea edi, dOffset;
   {$IFDEF FPC}vbroadcastsd ymm3, [edi];{$ELSE}db $C4,$E2,$7D,$19,$1F;{$ENDIF} 
   lea edi, scale;
   {$IFDEF FPC}vbroadcastsd ymm4, [edi];{$ELSE}db $C4,$E2,$7D,$19,$27;{$ENDIF} 

   // helper registers for the mt1, mt2 and dest pointers
   sub eax, ecx;

   // for y := 0 to height - 1:
   mov esi, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, ecx;
       @addforxloop:
           add edi, 128;
           jg @loopEnd;

           // prefetchw data...
           // prefetchw [eax + edi];

           // addition:
           {$IFDEF FPC}vmovapd ymm0, [eax + edi - 128];{$ELSE}db $C5,$FD,$28,$44,$38,$80;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm0, ymm0, ymm4;{$ELSE}db $C5,$FD,$59,$C4;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF FPC}vmovapd [eax + edi - 128], ymm0;{$ELSE}db $C5,$FD,$29,$44,$38,$80;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm2, [eax + edi - 96];{$ELSE}db $C5,$FD,$28,$54,$38,$A0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm2, ymm2, ymm4;{$ELSE}db $C5,$ED,$59,$D4;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm2, ymm2, ymm3;{$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF FPC}vmovapd [eax + edi - 96], ymm2;{$ELSE}db $C5,$FD,$29,$54,$38,$A0;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm0, [eax + edi - 64];{$ELSE}db $C5,$FD,$28,$44,$38,$C0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm0, ymm0, ymm4;{$ELSE}db $C5,$FD,$59,$C4;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF FPC}vmovapd [eax + edi - 64], ymm0;{$ELSE}db $C5,$FD,$29,$44,$38,$C0;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm2, [eax + edi - 32];{$ELSE}db $C5,$FD,$28,$54,$38,$E0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm2, ymm2, ymm4;{$ELSE}db $C5,$ED,$59,$D4;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm2, ymm2, ymm3;{$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF FPC}vmovapd [eax + edi - 32], ymm2;{$ELSE}db $C5,$FD,$29,$54,$38,$E0;{$ENDIF} 


       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @nextLine;

       @addforxloop2:
           {$IFDEF FPC}vmovapd xmm0, [eax + edi];{$ELSE}db $C5,$F9,$28,$04,$38;{$ENDIF} 
           {$IFDEF FPC}vmulpd xmm0, xmm0, xmm4;{$ELSE}db $C5,$F9,$59,$C4;{$ENDIF} 
           {$IFDEF FPC}vaddpd xmm0, xmm0, xmm3;{$ELSE}db $C5,$F9,$58,$C3;{$ENDIF} 

           {$IFDEF FPC}vmovapd [eax + edi], xmm0;{$ELSE}db $C5,$F9,$29,$04,$38;{$ENDIF} 
       add edi, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last column:
       {$IFDEF FPC}vmovsd xmm0, [eax];{$ELSE}db $C5,$FB,$10,$00;{$ENDIF} 
       {$IFDEF FPC}vmulsd xmm0, xmm0, xmm4;{$ELSE}db $C5,$FB,$59,$C4;{$ENDIF} 
       {$IFDEF FPC}vaddsd xmm0, xmm0, xmm3;{$ELSE}db $C5,$FB,$58,$C3;{$ENDIF} 
       {$IFDEF FPC}vmovsd [eax], xmm0;{$ELSE}db $C5,$FB,$11,$00;{$ENDIF} 

       // next line:
       add eax, edx;

   // loop y end
   dec esi;
   jnz @@addforyloop;

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;

procedure AVXMatrixScaleAddUnAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : NativeInt; const dOffset, Scale : double);
asm
   // prolog - simulate stack
   push ebx;
   push edi;
   push esi;

   //iters := -width*sizeof(double);
   dec ecx;
   imul ecx, -8;

   lea edi, dOffset;
   {$IFDEF FPC}vbroadcastsd ymm3, [edi];{$ELSE}db $C4,$E2,$7D,$19,$1F;{$ENDIF} 
   lea edi, scale;
   {$IFDEF FPC}vbroadcastsd ymm4, [edi];{$ELSE}db $C4,$E2,$7D,$19,$27;{$ENDIF} 

   // helper registers for the mt1, mt2 and dest pointers
   sub eax, ecx;

   // for y := 0 to height - 1:
   mov esi, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, ecx;
       @addforxloop:
           add edi, 128;
           jg @loopEnd;

           // prefetchw data...
           // prefetchw [eax + edi];

           // addition:
           {$IFDEF FPC}vmovupd ymm0, [eax + edi - 128];{$ELSE}db $C5,$FD,$10,$44,$38,$80;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm0, ymm0, ymm4;{$ELSE}db $C5,$FD,$59,$C4;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF FPC}vmovupd [eax + edi - 128], ymm0;{$ELSE}db $C5,$FD,$11,$44,$38,$80;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm2, [eax + edi - 96];{$ELSE}db $C5,$FD,$10,$54,$38,$A0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm2, ymm2, ymm4;{$ELSE}db $C5,$ED,$59,$D4;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm2, ymm2, ymm3;{$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF FPC}vmovupd [eax + edi - 96], ymm2;{$ELSE}db $C5,$FD,$11,$54,$38,$A0;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm0, [eax + edi - 64];{$ELSE}db $C5,$FD,$10,$44,$38,$C0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm0, ymm0, ymm4;{$ELSE}db $C5,$FD,$59,$C4;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF FPC}vmovupd [eax + edi - 64], ymm0;{$ELSE}db $C5,$FD,$11,$44,$38,$C0;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm2, [eax + edi - 32];{$ELSE}db $C5,$FD,$10,$54,$38,$E0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm2, ymm2, ymm4;{$ELSE}db $C5,$ED,$59,$D4;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm2, ymm2, ymm3;{$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF FPC}vmovupd [eax + edi - 32], ymm2;{$ELSE}db $C5,$FD,$11,$54,$38,$E0;{$ENDIF} 

       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @nextLine;

       @addforxloop2:
           {$IFDEF FPC}vmovupd xmm0, [eax + edi];{$ELSE}db $C5,$F9,$10,$04,$38;{$ENDIF} 
           {$IFDEF FPC}vmulpd xmm0, xmm0, xmm4;{$ELSE}db $C5,$F9,$59,$C4;{$ENDIF} 
           {$IFDEF FPC}vaddpd xmm0, xmm0, xmm3;{$ELSE}db $C5,$F9,$58,$C3;{$ENDIF} 

           {$IFDEF FPC}vmovupd [eax + edi], xmm0;{$ELSE}db $C5,$F9,$11,$04,$38;{$ENDIF} 
       add edi, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last column:
       {$IFDEF FPC}vmovsd xmm0, [eax];{$ELSE}db $C5,$FB,$10,$00;{$ENDIF} 
       {$IFDEF FPC}vmulsd xmm0, xmm0, xmm4;{$ELSE}db $C5,$FB,$59,$C4;{$ENDIF} 
       {$IFDEF FPC}vaddsd xmm0, xmm0, xmm3;{$ELSE}db $C5,$FB,$58,$C3;{$ENDIF} 
       {$IFDEF FPC}vmovsd [eax], xmm0;{$ELSE}db $C5,$FB,$11,$00;{$ENDIF} 

       // next line:
       add eax, edx;

   // loop y end
   dec esi;
   jnz @@addforyloop;

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;


// ###########################################
// #### Just add (without scale)
procedure AVXMatrixAddAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : NativeInt; const dOffset : double);
asm
   // prolog - simulate stack
   push ebx;
   push edi;
   push esi;

   //iters := -width*sizeof(double);
   mov ecx, width;
   imul ecx, -8;

   lea edi, dOffset;
   {$IFDEF FPC}vbroadcastsd ymm3, [edi];{$ELSE}db $C4,$E2,$7D,$19,$1F;{$ENDIF} 

   // helper registers for the mt1, mt2 and dest pointers
   sub eax, ecx;

   // for y := 0 to height - 1:
   mov esi, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, ecx;
       @addforxloop:
           add edi, 128;
           jg @loopEnd;

           // prefetchw data...
           // prefetchw [eax + edi];

           // add mul
           {$IFDEF FPC}vmovapd ymm0, [eax + edi - 128];{$ELSE}db $C5,$FD,$28,$44,$38,$80;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF FPC}vmovapd [eax + edi - 128], ymm0;{$ELSE}db $C5,$FD,$29,$44,$38,$80;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm2, [eax + edi - 96];{$ELSE}db $C5,$FD,$28,$54,$38,$A0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm2, ymm2, ymm3;{$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF FPC}vmovapd [eax + edi - 96], ymm2;{$ELSE}db $C5,$FD,$29,$54,$38,$A0;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm0, [eax + edi - 64];{$ELSE}db $C5,$FD,$28,$44,$38,$C0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF FPC}vmovapd [eax + edi - 64], ymm0;{$ELSE}db $C5,$FD,$29,$44,$38,$C0;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm2, [eax + edi - 32];{$ELSE}db $C5,$FD,$28,$54,$38,$E0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm2, ymm2, ymm3;{$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF FPC}vmovapd [eax + edi - 32], ymm2;{$ELSE}db $C5,$FD,$29,$54,$38,$E0;{$ENDIF} 

       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @nextLine;

       @addforxloop2:
           {$IFDEF FPC}vmovapd xmm0, [eax + edi];{$ELSE}db $C5,$F9,$28,$04,$38;{$ENDIF} 
           {$IFDEF FPC}vaddpd xmm0, xmm0, xmm3;{$ELSE}db $C5,$F9,$58,$C3;{$ENDIF} 

           {$IFDEF FPC}vmovapd [eax + edi], xmm0;{$ELSE}db $C5,$F9,$29,$04,$38;{$ENDIF} 
       add edi, 16;
       jnz @addforxloop2;

       @nextLine:

       // next line:
       add eax, edx;

   // loop y end
   dec esi;
   jnz @@addforyloop;

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;

procedure AVXMatrixAddUnAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : NativeInt; const dOffset : double);
asm
   // prolog - simulate stack
   push ebx;
   push edi;
   push esi;

   //iters := -width*sizeof(double);
   imul ecx, -8;

   lea edi, dOffset;
   {$IFDEF FPC}vbroadcastsd ymm3, [edi];{$ELSE}db $C4,$E2,$7D,$19,$1F;{$ENDIF} 

   // helper registers for the mt1, mt2 and dest pointers
   sub eax, ecx;

   // for y := 0 to height - 1:
   mov esi, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, ecx;
       @addforxloop:
           add edi, 128;
           jg @loopEnd;

           // prefetchw data...
           // prefetchw [eax + edi];

           // add mul
           {$IFDEF FPC}vmovupd ymm0, [eax + edi - 128];{$ELSE}db $C5,$FD,$10,$44,$38,$80;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF FPC}vmovupd [eax + edi - 128], ymm0;{$ELSE}db $C5,$FD,$11,$44,$38,$80;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm2, [eax + edi - 96];{$ELSE}db $C5,$FD,$10,$54,$38,$A0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm2, ymm2, ymm3;{$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF FPC}vmovupd [eax + edi - 96], ymm2;{$ELSE}db $C5,$FD,$11,$54,$38,$A0;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm0, [eax + edi - 64];{$ELSE}db $C5,$FD,$10,$44,$38,$C0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF FPC}vmovupd [eax + edi - 64], ymm0;{$ELSE}db $C5,$FD,$11,$44,$38,$C0;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm2, [eax + edi - 32];{$ELSE}db $C5,$FD,$10,$54,$38,$E0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm2, ymm2, ymm3;{$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF FPC}vmovupd [eax + edi - 32], ymm2;{$ELSE}db $C5,$FD,$11,$54,$38,$E0;{$ENDIF} 

       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @nextLine;

       @addforxloop2:
           {$IFDEF FPC}vmovupd xmm0, [eax + edi];{$ELSE}db $C5,$F9,$10,$04,$38;{$ENDIF} 
           {$IFDEF FPC}vaddpd xmm0, xmm0, xmm3;{$ELSE}db $C5,$F9,$58,$C3;{$ENDIF} 
           {$IFDEF FPC}vmovupd [eax + edi], xmm0;{$ELSE}db $C5,$F9,$11,$04,$38;{$ENDIF} 
       add edi, 16;
       jnz @addforxloop2;

       @nextLine:

       // next line:
       add eax, edx;

   // loop y end
   dec esi;
   jnz @@addforyloop;

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;


procedure AVXMatrixAddAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : NativeInt; const dOffset : double);
asm
// prolog - simulate stack
   push ebx;
   push edi;
   push esi;

   //iters := -width*sizeof(double);
   dec ecx;
   imul ecx, -8;

   lea edi, dOffset;
   {$IFDEF FPC}vbroadcastsd ymm3, [edi];{$ELSE}db $C4,$E2,$7D,$19,$1F;{$ENDIF} 

   // helper registers for the mt1, mt2 and dest pointers
   sub eax, ecx;

   // for y := 0 to height - 1:
   mov esi, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, ecx;
       @addforxloop:
           add edi, 128;
           jg @loopEnd;

           // prefetchw data...
           // prefetchw [eax + edi];

           // add mul
           {$IFDEF FPC}vmovapd ymm0, [eax + edi - 128];{$ELSE}db $C5,$FD,$28,$44,$38,$80;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF FPC}vmovapd [eax + edi - 128], ymm0;{$ELSE}db $C5,$FD,$29,$44,$38,$80;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm2, [eax + edi - 96];{$ELSE}db $C5,$FD,$28,$54,$38,$A0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm2, ymm2, ymm3;{$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF FPC}vmovapd [eax + edi - 96], ymm2;{$ELSE}db $C5,$FD,$29,$54,$38,$A0;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm0, [eax + edi - 64];{$ELSE}db $C5,$FD,$28,$44,$38,$C0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF FPC}vmovapd [eax + edi - 64], ymm0;{$ELSE}db $C5,$FD,$29,$44,$38,$C0;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm2, [eax + edi - 32];{$ELSE}db $C5,$FD,$28,$54,$38,$E0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm2, ymm2, ymm3;{$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF FPC}vmovapd [eax + edi - 32], ymm2;{$ELSE}db $C5,$FD,$29,$54,$38,$E0;{$ENDIF} 

       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @nextLine;

       @addforxloop2:
           {$IFDEF FPC}vmovapd xmm0, [eax + edi];{$ELSE}db $C5,$F9,$28,$04,$38;{$ENDIF} 
           {$IFDEF FPC}vaddpd xmm0, xmm0, xmm3;{$ELSE}db $C5,$F9,$58,$C3;{$ENDIF} 

           {$IFDEF FPC}vmovapd [eax + edi], xmm0;{$ELSE}db $C5,$F9,$29,$04,$38;{$ENDIF} 
       add edi, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last column:
       {$IFDEF FPC}vmovsd xmm0, [eax];{$ELSE}db $C5,$FB,$10,$00;{$ENDIF} 
       {$IFDEF FPC}vaddsd xmm0, xmm0, xmm3;{$ELSE}db $C5,$FB,$58,$C3;{$ENDIF} 
       {$IFDEF FPC}vmovsd [eax], xmm0;{$ELSE}db $C5,$FB,$11,$00;{$ENDIF} 

       // next line:
       add eax, edx;

   // loop y end
   dec esi;
   jnz @@addforyloop;

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;

procedure AVXMatrixAddUnAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : NativeInt; const dOffset : double);
asm
   // prolog - simulate stack
   push ebx;
   push edi;
   push esi;

   //iters := -width*sizeof(double);
   dec ecx;
   imul ecx, -8;

   lea edi, dOffset;
   {$IFDEF FPC}vbroadcastsd ymm3, [edi];{$ELSE}db $C4,$E2,$7D,$19,$1F;{$ENDIF} 

   // helper registers for the mt1, mt2 and dest pointers
   sub eax, ecx;

   // for y := 0 to height - 1:
   mov esi, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, ecx;
       @addforxloop:
           add edi, 128;
           jg @loopEnd;

           // prefetchw data...
           // prefetchw [eax + edi];

           // add mul
           {$IFDEF FPC}vmovupd ymm0, [eax + edi - 128];{$ELSE}db $C5,$FD,$10,$44,$38,$80;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF FPC}vmovupd [eax + edi - 128], ymm0;{$ELSE}db $C5,$FD,$11,$44,$38,$80;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm2, [eax + edi - 96];{$ELSE}db $C5,$FD,$10,$54,$38,$A0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm2, ymm2, ymm3;{$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF FPC}vmovupd [eax + edi - 96], ymm2;{$ELSE}db $C5,$FD,$11,$54,$38,$A0;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm0, [eax + edi - 64];{$ELSE}db $C5,$FD,$10,$44,$38,$C0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
           {$IFDEF FPC}vmovupd [eax + edi - 64], ymm0;{$ELSE}db $C5,$FD,$11,$44,$38,$C0;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm2, [eax + edi - 32];{$ELSE}db $C5,$FD,$10,$54,$38,$E0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm2, ymm2, ymm3;{$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
           {$IFDEF FPC}vmovupd [eax + edi - 32], ymm2;{$ELSE}db $C5,$FD,$11,$54,$38,$E0;{$ENDIF} 

       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @nextLine;

       @addforxloop2:
           {$IFDEF FPC}vmovupd xmm0, [eax + edi];{$ELSE}db $C5,$F9,$10,$04,$38;{$ENDIF} 
           {$IFDEF FPC}vaddpd xmm0, xmm0, xmm3;{$ELSE}db $C5,$F9,$58,$C3;{$ENDIF} 

           {$IFDEF FPC}vmovupd [eax + edi], xmm0;{$ELSE}db $C5,$F9,$11,$04,$38;{$ENDIF} 
       add edi, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last column:
       {$IFDEF FPC}vmovsd xmm0, [eax];{$ELSE}db $C5,$FB,$10,$00;{$ENDIF} 
       {$IFDEF FPC}vaddsd xmm0, xmm0, xmm3;{$ELSE}db $C5,$FB,$58,$C3;{$ENDIF} 
       {$IFDEF FPC}vmovsd [eax], xmm0;{$ELSE}db $C5,$FB,$11,$00;{$ENDIF} 

       // next line:
       add eax, edx;

   // loop y end
   dec esi;
   jnz @@addforyloop;

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;

{$ENDIF}

end.
