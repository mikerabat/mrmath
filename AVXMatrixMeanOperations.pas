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


unit AVXMatrixMeanOperations;

interface

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}
{$IFNDEF x64}

uses MatrixConst;

procedure AVXMatrixMeanRowAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure AVXMatrixMeanRowUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

procedure AVXMatrixMeanColumnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure AVXMatrixMeanColumnUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

procedure AVXMatrixVarRowAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure AVXMatrixVarRowUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

procedure AVXMatrixVarColumnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure AVXMatrixVarColumnUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}


// combined methods
procedure AVXMatrixMeanVarRowAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure AVXMatrixMeanVarRowUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

procedure AVXMatrixMeanVarColumnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure AVXMatrixMeanVarColumnUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

{$ENDIF}

implementation

{$IFNDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$S-} {$ENDIF}

const cLocOne : double = 1;

procedure AVXMatrixMeanRowAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
asm
   // prolog
   push edi;
   push esi;
   push ebx;

   // iters := -width*sizeof(double)
   mov edi, width;
   {$IFDEF FPC}vcvtsi2sd xmm4, xmm4, edi;{$ELSE}db $C5,$DB,$2A,$E7;{$ENDIF} 
   imul edi, -8;

   // helper registers for the mt1, mt2 and dest pointers
   sub ecx, edi;

   // for y := 0 to height - 1:
   mov ebx, height;
   @@addforyloop:
       {$IFDEF FPC}vxorpd ymm0, ymm0, ymm0;{$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} 
       {$IFDEF FPC}vxorpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$57,$C9;{$ENDIF} 

       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov esi, edi;
       @addforxloop:
           add esi, 128;
           jg @loopEnd;
           // prefetch data...
           // prefetch [r8 + esi];

           // addition:
           {$IFDEF FPC}vaddpd ymm0, ymm0, [ecx + esi - 128];{$ELSE}db $C5,$FD,$58,$44,$31,$80;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm1, ymm1, [ecx + esi - 96];{$ELSE}db $C5,$F5,$58,$4C,$31,$A0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, [ecx + esi - 64];{$ELSE}db $C5,$FD,$58,$44,$31,$C0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm1, ymm1, [ecx + esi - 32];{$ELSE}db $C5,$F5,$58,$4C,$31,$E0;{$ENDIF} 
       jmp @addforxloop

       @loopEnd:

       {$IFDEF FPC}vaddpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 
       {$IFDEF FPC}vextractf128 xmm2, ymm0, 1;{$ELSE}db $C4,$E3,$7D,$19,$C2,$01;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm2;{$ELSE}db $C5,$F9,$7C,$C2;{$ENDIF} 

       sub esi, 128;

       jz @buildRes;

       @addforxloop2:
           add esi, 16;
           jg @@addforxloop2end;

           {$IFDEF FPC}vaddpd xmm0, xmm0, [ecx + esi - 16];{$ELSE}db $C5,$F9,$58,$44,$31,$F0;{$ENDIF} 
       jmp @addforxloop2;

       @@addforxloop2end:

       sub esi, 16;
       jz @buildRes;

       {$IFDEF FPC}vaddsd xmm0, xmm0, [ecx + esi];{$ELSE}db $C5,$FB,$58,$04,$31;{$ENDIF} 

       @buildRes:

       // build result
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} 

       {$IFDEF FPC}vdivsd xmm0, xmm0, xmm4;{$ELSE}db $C5,$FB,$5E,$C4;{$ENDIF} 

       // write result
       {$IFDEF FPC}vmovsd [eax], xmm0;{$ELSE}db $C5,$FB,$11,$00;{$ENDIF} 

       // next line:
       add ecx, srcLineWidth;
       add eax, edx;

   // loop y end
   dec ebx;
   jnz @@addforyloop;

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop ebx;
   pop esi;
   pop edi;
end;

procedure AVXMatrixMeanRowUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
asm
   // prolog
   push edi;
   push esi;
   push ebx;

   // iters := -width*sizeof(double)
   mov edi, width;
   {$IFDEF FPC}vcvtsi2sd xmm4, xmm4, edi;{$ELSE}db $C5,$DB,$2A,$E7;{$ENDIF} 
   imul edi, -8;

   // helper registers for the mt1, mt2 and dest pointers
   sub ecx, edi;

   // for y := 0 to height - 1:
   mov ebx, height;
   @@addforyloop:
       {$IFDEF FPC}vxorpd ymm0, ymm0, ymm0;{$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} 
       {$IFDEF FPC}vxorpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$57,$C9;{$ENDIF} 

       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov esi, edi;
       @addforxloop:
           add esi, 128;
           jg @loopEnd;
           // prefetch data...
           // prefetch [r8 + esi];

           // addition:
           {$IFDEF FPC}vmovupd ymm2, [ecx + esi - 128];{$ELSE}db $C5,$FD,$10,$54,$31,$80;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm3, [ecx + esi - 96];{$ELSE}db $C5,$FD,$10,$5C,$31,$A0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm2;{$ELSE}db $C5,$FD,$58,$C2;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm1, ymm1, ymm3;{$ELSE}db $C5,$F5,$58,$CB;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm2, [ecx + esi - 64];{$ELSE}db $C5,$FD,$10,$54,$31,$C0;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm3, [ecx + esi - 32];{$ELSE}db $C5,$FD,$10,$5C,$31,$E0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm2;{$ELSE}db $C5,$FD,$58,$C2;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm1, ymm1, ymm3;{$ELSE}db $C5,$F5,$58,$CB;{$ENDIF} 
       jmp @addforxloop

       @loopEnd:

       {$IFDEF FPC}vaddpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 
       {$IFDEF FPC}vextractf128 xmm2, ymm0, 1;{$ELSE}db $C4,$E3,$7D,$19,$C2,$01;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm2;{$ELSE}db $C5,$F9,$7C,$C2;{$ENDIF} 

       sub esi, 128;

       jz @buildRes;

       @addforxloop2:
           add esi, 16;
           jg @@addforxloop2end;

           {$IFDEF FPC}vmovupd xmm1, [ecx + esi - 16];{$ELSE}db $C5,$F9,$10,$4C,$31,$F0;{$ENDIF} 
           {$IFDEF FPC}vaddpd xmm0, xmm0, xmm1;{$ELSE}db $C5,$F9,$58,$C1;{$ENDIF} 
       jmp @addforxloop2;

       @@addforxloop2end:

       sub esi, 16;
       jz @buildRes;

       {$IFDEF FPC}vaddsd xmm0, xmm0, [ecx + esi];{$ELSE}db $C5,$FB,$58,$04,$31;{$ENDIF} 

       @buildRes:

       // build result
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} 

       {$IFDEF FPC}vdivsd xmm0, xmm0, xmm4;{$ELSE}db $C5,$FB,$5E,$C4;{$ENDIF} 

       // write result
       {$IFDEF FPC}vmovsd [eax], xmm0;{$ELSE}db $C5,$FB,$11,$00;{$ENDIF} 

       // next line:
       add ecx, srcLineWidth;
       add eax, edx;

   // loop y end
   dec ebx;
   jnz @@addforyloop;

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop ebx;
   pop esi;
   pop edi;
end;

procedure AVXMatrixMeanColumnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
var tmp : double;
asm
   // prolog
   push ebx;
   push edi;
   push esi;

   // prepare
   mov ebx, height;
   {$IFDEF FPC}vcvtsi2sd xmm0, xmm0, ebx;{$ELSE}db $C5,$FB,$2A,$C3;{$ENDIF} 
   imul ebx, srcLineWidth;
   imul ebx, -1;

   // helper registers for the mt1, mt2 and dest pointers
   sub ecx, ebx;

   mov edi, srcLineWidth;

   lea edx, tmp;
   {$IFDEF FPC}vmovsd [edx], xmm0;{$ELSE}db $C5,$FB,$11,$02;{$ENDIF} 
   {$IFDEF FPC}vbroadcastsd ymm2, [edx];{$ELSE}db $C4,$E2,$7D,$19,$12;{$ENDIF} 
   // vbroadcastsd ymm2, xmm0; // avx2


   // for x := 0 to width - 1:
   mov esi, Width;
   sub esi, 4;
   jl @@addforxloop4end;

   // 4 columns at once
   @@addforxloop4:
       {$IFDEF FPC}vxorpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$57,$C9;{$ENDIF} 

       // for y := 0 to height - 1;
       // prepare for reverse loop
       mov edx, ebx;
       @addforyloop4:
           {$IFDEF FPC}vaddpd ymm1, ymm1, [ecx + edx];{$ELSE}db $C5,$F5,$58,$0C,$11;{$ENDIF} 
       add edx, edi;
       jnz @addforyloop4;

       // build result
       {$IFDEF FPC}vdivpd ymm1, ymm1, ymm2;{$ELSE}db $C5,$F5,$5E,$CA;{$ENDIF} 
       {$IFDEF FPC}vmovapd [eax], ymm1;{$ELSE}db $C5,$FD,$29,$08;{$ENDIF} 

       // next columns:
       add eax, 32;
       add ecx, 32;

   // loop x end
   sub esi, 4;
   jge @@addforxloop4;

   @@addforxloop4end:

   add esi, 4;
   jz @@endProc;

   sub esi, 2;
   jl @@lastcolumn;

   {$IFDEF FPC}vxorpd xmm1, xmm1, xmm1;{$ELSE}db $C5,$F1,$57,$C9;{$ENDIF} 

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov edx, ebx;
   @addforyloop2:
       {$IFDEF FPC}vaddpd xmm1, xmm1, [ecx + edx];{$ELSE}db $C5,$F1,$58,$0C,$11;{$ENDIF} 
   add edx, edi;
   jnz @addforyloop2;

   // build result
   {$IFDEF FPC}vdivpd xmm1, xmm1, xmm2;{$ELSE}db $C5,$F1,$5E,$CA;{$ENDIF} 
   {$IFDEF FPC}vmovapd [eax], xmm1;{$ELSE}db $C5,$F9,$29,$08;{$ENDIF} 

   // next columns:
   add eax, 16;
   add ecx, 16;

   dec esi;
   jnz @@endProc;

   @@lastcolumn:

   {$IFDEF FPC}vxorpd xmm1, xmm1, xmm1;{$ELSE}db $C5,$F1,$57,$C9;{$ENDIF} 

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov edx, ebx;
   @addforyloop:
       {$IFDEF FPC}vaddsd xmm1, xmm1, [ecx + edx];{$ELSE}db $C5,$F3,$58,$0C,$11;{$ENDIF} 
   add edx, edi;
   jnz @addforyloop;

   // build result
   {$IFDEF FPC}vdivsd xmm1, xmm1, xmm2;{$ELSE}db $C5,$F3,$5E,$CA;{$ENDIF} 
   {$IFDEF FPC}vmovsd [eax], xmm1;{$ELSE}db $C5,$FB,$11,$08;{$ENDIF} 

   @@endProc:
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   // epilog
   pop esi;
   pop edi;
   pop ebx;
end;

procedure AVXMatrixMeanColumnUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
var tmp : double;
asm
   // prolog
   push ebx;
   push edi;
   push esi;

   // prepare
   mov ebx, height;
   {$IFDEF FPC}vcvtsi2sd xmm0, xmm0, ebx;{$ELSE}db $C5,$FB,$2A,$C3;{$ENDIF} 
   imul ebx, srcLineWidth;
   imul ebx, -1;

   // helper registers for the mt1, mt2 and dest pointers
   sub ecx, ebx;

   mov edi, srcLineWidth;

   lea edx, tmp;
   {$IFDEF FPC}vmovsd [edx], xmm0;{$ELSE}db $C5,$FB,$11,$02;{$ENDIF} 
   {$IFDEF FPC}vbroadcastsd ymm2, [edx];{$ELSE}db $C4,$E2,$7D,$19,$12;{$ENDIF} 
   // vbroadcastsd ymm2, xmm0; // avx2


   // for x := 0 to width - 1:
   mov esi, Width;
   sub esi, 4;
   jl @@addforxloop4end;

   // 4 columns at once
   @@addforxloop4:
       {$IFDEF FPC}vxorpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$57,$C9;{$ENDIF} 

       // for y := 0 to height - 1;
       // prepare for reverse loop
       mov edx, ebx;
       @addforyloop4:
           {$IFDEF FPC}vmovupd ymm0, [ecx + edx];{$ELSE}db $C5,$FD,$10,$04,$11;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm1, ymm1, ymm0;{$ELSE}db $C5,$F5,$58,$C8;{$ENDIF} 
       add edx, edi;
       jnz @addforyloop4;

       // build result
       {$IFDEF FPC}vdivpd ymm1, ymm1, ymm2;{$ELSE}db $C5,$F5,$5E,$CA;{$ENDIF} 
       {$IFDEF FPC}vmovapd [eax], ymm1;{$ELSE}db $C5,$FD,$29,$08;{$ENDIF} 

       // next columns:
       add eax, 32;
       add ecx, 32;

   // loop x end
   sub esi, 4;
   jge @@addforxloop4;

   @@addforxloop4end:

   add esi, 4;
   jz @@endProc;

   sub esi, 2;
   jl @@lastcolumn;

   {$IFDEF FPC}vxorpd xmm1, xmm1, xmm1;{$ELSE}db $C5,$F1,$57,$C9;{$ENDIF} 

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov edx, ebx;
   @addforyloop2:
       {$IFDEF FPC}vmovupd xmm0, [ecx + edx];{$ELSE}db $C5,$F9,$10,$04,$11;{$ENDIF} 
       {$IFDEF FPC}vaddpd xmm1, xmm1, xmm0;{$ELSE}db $C5,$F1,$58,$C8;{$ENDIF} 
   add edx, edi;
   jnz @addforyloop2;

   // build result
   {$IFDEF FPC}vdivpd xmm1, xmm1, xmm2;{$ELSE}db $C5,$F1,$5E,$CA;{$ENDIF} 
   {$IFDEF FPC}vmovapd [eax], xmm1;{$ELSE}db $C5,$F9,$29,$08;{$ENDIF} 

   // next columns:
   add eax, 16;
   add ecx, 16;

   dec esi;
   jnz @@endProc;

   @@lastcolumn:

   {$IFDEF FPC}vxorpd xmm1, xmm1, xmm1;{$ELSE}db $C5,$F1,$57,$C9;{$ENDIF} 

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov edx, ebx;
   @addforyloop:
       {$IFDEF FPC}vaddsd xmm1, xmm1, [ecx + edx];{$ELSE}db $C5,$F3,$58,$0C,$11;{$ENDIF} 
   add edx, edi;
   jnz @addforyloop;

   // build result
   {$IFDEF FPC}vdivsd xmm1, xmm1, xmm2;{$ELSE}db $C5,$F3,$5E,$CA;{$ENDIF} 
   {$IFDEF FPC}vmovsd [eax], xmm1;{$ELSE}db $C5,$FB,$11,$08;{$ENDIF} 

   @@endProc:
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   // epilog
   pop esi;
   pop edi;
   pop ebx;
end;

// ###########################################
// #### Variance calculation
// ###########################################


procedure AVXMatrixVarRowAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean);
var tmp : double;
    aDestLineWidth : TASMNativeInt;
asm
   // prolog
   push ebx;
   push edi;
   push esi;

   // iters := -width*sizeof(double)
   mov edi, width;
   {$IFDEF FPC}vcvtsi2sd xmm3, xmm3, edi;{$ELSE}db $C5,$E3,$2A,$DF;{$ENDIF} 
   imul edi, -8;

   // helper registers for the src and dest pointers
   sub ecx, edi;

   // for y := 0 to height - 1:
   @@addforyloop:
       {$IFDEF FPC}vxorpd ymm0, ymm0, ymm0;{$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} 
       {$IFDEF FPC}vxorpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$57,$C9;{$ENDIF} 

       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov esi, edi;
       @addforxloop:
           add esi, 128;
           jg @loopEnd;
           // prefetch data...
           // prefetch [r8 + esi];

           // addition:
           {$IFDEF FPC}vaddpd ymm0, ymm0, [ecx + esi - 128];{$ELSE}db $C5,$FD,$58,$44,$31,$80;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm1, ymm1, [ecx + esi - 96];{$ELSE}db $C5,$F5,$58,$4C,$31,$A0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, [ecx + esi - 64];{$ELSE}db $C5,$FD,$58,$44,$31,$C0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm1, ymm1, [ecx + esi - 32];{$ELSE}db $C5,$F5,$58,$4C,$31,$E0;{$ENDIF} 
       jmp @addforxloop

       @loopEnd:

       {$IFDEF FPC}vaddpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 
       {$IFDEF FPC}vextractf128 xmm2, ymm0, 1;{$ELSE}db $C4,$E3,$7D,$19,$C2,$01;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm2;{$ELSE}db $C5,$F9,$7C,$C2;{$ENDIF} 

       sub esi, 128;

       jz @buildRes;

       @addforxloop2:
           add esi, 16;
           jg @@addforxloop2end;

           {$IFDEF FPC}vaddpd xmm0, xmm0, [ecx + esi - 16];{$ELSE}db $C5,$F9,$58,$44,$31,$F0;{$ENDIF} 
       jmp @addforxloop2;

       @@addforxloop2end:

       sub esi, 16;
       jz @buildRes;

       {$IFDEF FPC}vaddsd xmm0, xmm0, [ecx + esi];{$ELSE}db $C5,$FB,$58,$04,$31;{$ENDIF} 

       @buildRes:

       // build result
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} 
       {$IFDEF FPC}vdivsd xmm0, xmm0, xmm3;{$ELSE}db $C5,$FB,$5E,$C3;{$ENDIF} 

       // we have calculated the mean -> 
       // repeat the loop to calculate the variance
       lea esi, tmp;
       {$IFDEF FPC}vmovsd [esi], xmm0;{$ELSE}db $C5,$FB,$11,$06;{$ENDIF} 
       {$IFDEF FPC}vbroadcastsd ymm4, [esi];{$ELSE}db $C4,$E2,$7D,$19,$26;{$ENDIF} 
       // vbroadcastsd ymm4, xmm0; //avx2
       {$IFDEF FPC}vxorpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$57,$C0;{$ENDIF} 
            
       // for x := 0 to w - 1;
       // prepare for reverse loop
       // variance = sum (x - mean)^2
       mov esi, edi;
       @addforxloop3:
           add esi, 128;
           jg @loopEnd2;
           // prefetch data...
           // prefetch [eax + esi];

           // addition:
           {$IFDEF FPC}vmovapd ymm1, [ecx + esi - 128];{$ELSE}db $C5,$FD,$28,$4C,$31,$80;{$ENDIF} 
           {$IFDEF FPC}vsubpd ymm1, ymm1, ymm4;{$ELSE}db $C5,$F5,$5C,$CC;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 
                
           {$IFDEF FPC}vmovapd ymm1, [ecx + esi - 96];{$ELSE}db $C5,$FD,$28,$4C,$31,$A0;{$ENDIF} 
           {$IFDEF FPC}vsubpd ymm1, ymm1, ymm4;{$ELSE}db $C5,$F5,$5C,$CC;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm1, [ecx + esi - 64];{$ELSE}db $C5,$FD,$28,$4C,$31,$C0;{$ENDIF} 
           {$IFDEF FPC}vsubpd ymm1, ymm1, ymm4;{$ELSE}db $C5,$F5,$5C,$CC;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm1, [ecx + esi - 32];{$ELSE}db $C5,$FD,$28,$4C,$31,$E0;{$ENDIF} 
           {$IFDEF FPC}vsubpd ymm1, ymm1, ymm4;{$ELSE}db $C5,$F5,$5C,$CC;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 

       jmp @addforxloop3

       @loopEnd2:

       sub esi, 128;

       {$IFDEF FPC}vextractf128 xmm2, ymm0, 1;{$ELSE}db $C4,$E3,$7D,$19,$C2,$01;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm2;{$ELSE}db $C5,$F9,$7C,$C2;{$ENDIF} 

       jz @buildRes2;

       @addforxloop4:
           add esi, 16;
           jg @addforxloop4end;

           {$IFDEF FPC}vmovapd xmm1, [ecx + esi - 16];{$ELSE}db $C5,$F9,$28,$4C,$31,$F0;{$ENDIF} 
           {$IFDEF FPC}vsubpd xmm1, xmm1, xmm4;{$ELSE}db $C5,$F1,$5C,$CC;{$ENDIF} 
           {$IFDEF FPC}vmulpd xmm1, xmm1, xmm1;{$ELSE}db $C5,$F1,$59,$C9;{$ENDIF} 
           {$IFDEF FPC}vaddpd xmm0, xmm0, xmm1;{$ELSE}db $C5,$F9,$58,$C1;{$ENDIF} 
       jmp @addforxloop4;

       @addforxloop4end:

       sub esi, 16;
       jz @buildRes2;

       // last column
       {$IFDEF FPC}vmovsd xmm1, [ecx + esi];{$ELSE}db $C5,$FB,$10,$0C,$31;{$ENDIF} 
       {$IFDEF FPC}vsubsd xmm1, xmm1, xmm4;{$ELSE}db $C5,$F3,$5C,$CC;{$ENDIF} 
       {$IFDEF FPC}vmulsd xmm1, xmm1, xmm1;{$ELSE}db $C5,$F3,$59,$C9;{$ENDIF} 
       {$IFDEF FPC}vaddsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$58,$C1;{$ENDIF} 

       @buildRes2:

       // build result
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} 
            
       // check if we need to use the unbiased version
       cmp unbiased, 0;
       jz @@dobiased;

       lea esi, cLocOne;
       {$IFDEF FPC}vmovsd xmm2, [esi];{$ELSE}db $C5,$FB,$10,$16;{$ENDIF} 
       {$IFDEF FPC}vsubsd xmm4, xmm3, xmm2;{$ELSE}db $C5,$E3,$5C,$E2;{$ENDIF} 
       {$IFDEF FPC}vmaxsd xmm4, xmm4, xmm2;{$ELSE}db $C5,$DB,$5F,$E2;{$ENDIF} 

       {$IFDEF FPC}vdivsd xmm0, xmm0, xmm4;{$ELSE}db $C5,$FB,$5E,$C4;{$ENDIF} 

       jmp @@writeRes;

       @@dobiased:
       {$IFDEF FPC}vdivsd xmm0, xmm0, xmm3;{$ELSE}db $C5,$FB,$5E,$C3;{$ENDIF} 
            
       // write result
       @@writeRes:
       {$IFDEF FPC}vmovsd [eax], xmm0;{$ELSE}db $C5,$FB,$11,$00;{$ENDIF} 

       // next line:
       add ecx, srcLineWidth;
       add eax, edx;

   // loop y end
   dec Height;
   jnz @@addforyloop;

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;

procedure AVXMatrixVarRowUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean);
var tmp : double;
asm
   // prolog
   push ebx;
   push edi;
   push esi;

   // iters := -width*sizeof(double)
   mov edi, width;
   {$IFDEF FPC}vcvtsi2sd xmm3, xmm3, edi;{$ELSE}db $C5,$E3,$2A,$DF;{$ENDIF} 
   imul edi, -8;

   // helper registers for the src and dest pointers
   sub ecx, edi;

   // for y := 0 to height - 1:
   @@addforyloop:
       {$IFDEF FPC}vxorpd ymm0, ymm0, ymm0;{$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} 
       {$IFDEF FPC}vxorpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$57,$C9;{$ENDIF} 

       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov esi, edi;
       @addforxloop:
           add esi, 128;
           jg @loopEnd;
           // prefetch data...
           // prefetch [r8 + esi];

           // addition:
           {$IFDEF FPC}vmovupd ymm2, [ecx + esi - 128];{$ELSE}db $C5,$FD,$10,$54,$31,$80;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm2;{$ELSE}db $C5,$FD,$58,$C2;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm4, [ecx + esi - 96];{$ELSE}db $C5,$FD,$10,$64,$31,$A0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm1, ymm1, ymm4;{$ELSE}db $C5,$F5,$58,$CC;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm2, [ecx + esi - 64];{$ELSE}db $C5,$FD,$10,$54,$31,$C0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm2;{$ELSE}db $C5,$FD,$58,$C2;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm4, [ecx + esi - 32];{$ELSE}db $C5,$FD,$10,$64,$31,$E0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm1, ymm1, ymm4;{$ELSE}db $C5,$F5,$58,$CC;{$ENDIF} 
       jmp @addforxloop

       @loopEnd:

       {$IFDEF FPC}vaddpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 
       {$IFDEF FPC}vextractf128 xmm2, ymm0, 1;{$ELSE}db $C4,$E3,$7D,$19,$C2,$01;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm2;{$ELSE}db $C5,$F9,$7C,$C2;{$ENDIF} 

       sub esi, 128;

       jz @buildRes;

       @addforxloop2:
           add esi, 16;
           jg @@addforxloop2end;

           {$IFDEF FPC}vmovupd xmm1, [ecx + esi - 16];{$ELSE}db $C5,$F9,$10,$4C,$31,$F0;{$ENDIF} 
           {$IFDEF FPC}vaddpd xmm0, xmm0, xmm1;{$ELSE}db $C5,$F9,$58,$C1;{$ENDIF} 
       jmp @addforxloop2;

       @@addforxloop2end:

       sub esi, 16;
       jz @buildRes;

       {$IFDEF FPC}vaddsd xmm0, xmm0, [ecx + esi];{$ELSE}db $C5,$FB,$58,$04,$31;{$ENDIF} 

       @buildRes:

       // build result
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} 
       {$IFDEF FPC}vdivsd xmm0, xmm0, xmm3;{$ELSE}db $C5,$FB,$5E,$C3;{$ENDIF} 

       // we have calculated the mean ->
       // repeat the loop to calculate the variance
       lea esi, tmp;
       {$IFDEF FPC}vmovsd [esi], xmm0;{$ELSE}db $C5,$FB,$11,$06;{$ENDIF} 
       {$IFDEF FPC}vbroadcastsd ymm4, [esi];{$ELSE}db $C4,$E2,$7D,$19,$26;{$ENDIF} 
       // vbroadcastsd ymm4, xmm0;  // avx2 instruction
       {$IFDEF FPC}vxorpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$57,$C0;{$ENDIF} 

       // for x := 0 to w - 1;
       // prepare for reverse loop
       // variance = sum (x - mean)^2
       mov esi, edi;
       @addforxloop3:
           add esi, 128;
           jg @loopEnd2;
           // prefetch data...
           // prefetch [eax + esi];

           // addition:
           {$IFDEF FPC}vmovupd ymm1, [ecx + esi - 128];{$ELSE}db $C5,$FD,$10,$4C,$31,$80;{$ENDIF} 
           {$IFDEF FPC}vsubpd ymm1, ymm1, ymm4;{$ELSE}db $C5,$F5,$5C,$CC;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm1, [ecx + esi - 96];{$ELSE}db $C5,$FD,$10,$4C,$31,$A0;{$ENDIF} 
           {$IFDEF FPC}vsubpd ymm1, ymm1, ymm4;{$ELSE}db $C5,$F5,$5C,$CC;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm1, [ecx + esi - 64];{$ELSE}db $C5,$FD,$10,$4C,$31,$C0;{$ENDIF} 
           {$IFDEF FPC}vsubpd ymm1, ymm1, ymm4;{$ELSE}db $C5,$F5,$5C,$CC;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm1, [ecx + esi - 32];{$ELSE}db $C5,$FD,$10,$4C,$31,$E0;{$ENDIF} 
           {$IFDEF FPC}vsubpd ymm1, ymm1, ymm4;{$ELSE}db $C5,$F5,$5C,$CC;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 

       jmp @addforxloop3

       @loopEnd2:

       sub esi, 128;

       {$IFDEF FPC}vextractf128 xmm2, ymm0, 1;{$ELSE}db $C4,$E3,$7D,$19,$C2,$01;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm2;{$ELSE}db $C5,$F9,$7C,$C2;{$ENDIF} 

       jz @buildRes2;

       @addforxloop4:
           add esi, 16;
           jg @addforxloop4end;

           {$IFDEF FPC}vmovupd xmm1, [ecx + esi - 16];{$ELSE}db $C5,$F9,$10,$4C,$31,$F0;{$ENDIF} 
           {$IFDEF FPC}vsubpd xmm1, xmm1, xmm4;{$ELSE}db $C5,$F1,$5C,$CC;{$ENDIF} 
           {$IFDEF FPC}vmulpd xmm1, xmm1, xmm1;{$ELSE}db $C5,$F1,$59,$C9;{$ENDIF} 
           {$IFDEF FPC}vaddpd xmm0, xmm0, xmm1;{$ELSE}db $C5,$F9,$58,$C1;{$ENDIF} 
       jmp @addforxloop4;

       @addforxloop4end:

       sub esi, 16;
       jz @buildRes2;

       // last column
       {$IFDEF FPC}vmovsd xmm1, [ecx + esi];{$ELSE}db $C5,$FB,$10,$0C,$31;{$ENDIF} 
       {$IFDEF FPC}vsubsd xmm1, xmm1, xmm4;{$ELSE}db $C5,$F3,$5C,$CC;{$ENDIF} 
       {$IFDEF FPC}vmulsd xmm1, xmm1, xmm1;{$ELSE}db $C5,$F3,$59,$C9;{$ENDIF} 
       {$IFDEF FPC}vaddsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$58,$C1;{$ENDIF} 

       @buildRes2:

       // build result
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} 

       // check if we need to use the unbiased version
       cmp unbiased, 0;
       jz @@dobiased;

       lea esi, cLocOne;
       {$IFDEF FPC}vmovsd xmm2, [esi];{$ELSE}db $C5,$FB,$10,$16;{$ENDIF} 
       {$IFDEF FPC}vsubsd xmm4, xmm3, xmm2;{$ELSE}db $C5,$E3,$5C,$E2;{$ENDIF} 
       {$IFDEF FPC}vmaxsd xmm4, xmm4, xmm2;{$ELSE}db $C5,$DB,$5F,$E2;{$ENDIF} 

       {$IFDEF FPC}vdivsd xmm0, xmm0, xmm4;{$ELSE}db $C5,$FB,$5E,$C4;{$ENDIF} 

       jmp @@writeRes;

       @@dobiased:
       {$IFDEF FPC}vdivsd xmm0, xmm0, xmm3;{$ELSE}db $C5,$FB,$5E,$C3;{$ENDIF} 

       // write result
       @@writeRes:
       {$IFDEF FPC}vmovsd [eax], xmm0;{$ELSE}db $C5,$FB,$11,$00;{$ENDIF} 

       // next line:
       add ecx, srcLineWidth;
       add eax, edx;

   // loop y end
   dec Height;
   jnz @@addforyloop;

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;

procedure AVXMatrixVarColumnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean);
var tmp : double;
asm
   // prolog
   push ebx;
   push edi;
   push esi;

   // preparations
   mov edi, height;
   {$IFDEF FPC}vcvtsi2sd xmm0, xmm0, edi;{$ELSE}db $C5,$FB,$2A,$C7;{$ENDIF} 
   imul edi, srcLineWidth;
   imul edi, -1;

   // helper registers for the mt1, mt2 and dest pointers
   sub ecx, edi;
   lea esi, tmp;
   {$IFDEF FPC}vmovsd [esi], xmm0;{$ELSE}db $C5,$FB,$11,$06;{$ENDIF} 
   {$IFDEF FPC}vbroadcastsd ymm2, [esi]; {$ELSE}db $C4,$E2,$7D,$19,$16;{$ENDIF} // vbroadcastsd ymm2, xmm0; // avx2

   mov ebx, srcLineWidth;
   lea esi, cLocOne;
   {$IFDEF FPC}vbroadcastsd ymm5, [esi];{$ELSE}db $C4,$E2,$7D,$19,$2E;{$ENDIF} 

   // for x := 0 to width - 1:
   mov esi, Width;
   sub esi, 4;
   jl @@addforxloop4end;

   // 4 columns at once
   @@addforxloop4:
       {$IFDEF FPC}vxorpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$57,$C9;{$ENDIF} 

       // for y := 0 to height - 1;
       // prepare for reverse loop
       mov edx, edi;
       @addforyloop4:
           {$IFDEF FPC}vaddpd ymm1, ymm1, [ecx + edx];{$ELSE}db $C5,$F5,$58,$0C,$11;{$ENDIF} 
       add edx, ebx;
       jnz @addforyloop4;

       // build result
       {$IFDEF FPC}vdivpd ymm0, ymm1, ymm2;{$ELSE}db $C5,$F5,$5E,$C2;{$ENDIF} 

       // calculate final standard deviation
       // for y := 0 to height - 1;
       // prepare for reverse loop
       {$IFDEF FPC}vxorpd ymm4, ymm4, ymm4;{$ELSE}db $C5,$DD,$57,$E4;{$ENDIF} 
       mov edx, edi;
       @addforyloop4_2:
           {$IFDEF FPC}vmovapd ymm1, [ecx + edx];{$ELSE}db $C5,$FD,$28,$0C,$11;{$ENDIF} 
           {$IFDEF FPC}vsubpd ymm1, ymm1, ymm0;{$ELSE}db $C5,$F5,$5C,$C8;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm4, ymm4, ymm1;{$ELSE}db $C5,$DD,$58,$E1;{$ENDIF} 
       add edx, ebx;
       jnz @addforyloop4_2;

       // check if we need to use the unbiased version
       cmp unbiased, 0;
       jz @@dobiased_4;

       lea edx, cLocOne;

       {$IFDEF FPC}vbroadcastsd ymm1, [edx];{$ELSE}db $C4,$E2,$7D,$19,$0A;{$ENDIF} 
       {$IFDEF FPC}vsubpd ymm3, ymm2, ymm1;{$ELSE}db $C5,$ED,$5C,$D9;{$ENDIF} 
       {$IFDEF FPC}vmaxpd ymm3, ymm3, ymm1;{$ELSE}db $C5,$E5,$5F,$D9;{$ENDIF} 

       {$IFDEF FPC}vdivpd ymm4, ymm4, ymm3;{$ELSE}db $C5,$DD,$5E,$E3;{$ENDIF} 

       jmp @@writeRes_4;

       @@dobiased_4:
       {$IFDEF FPC}vdivpd ymm4, ymm4, ymm2;{$ELSE}db $C5,$DD,$5E,$E2;{$ENDIF} 

       // write result
       @@writeRes_4:
       {$IFDEF FPC}vmovapd [eax], ymm4;{$ELSE}db $C5,$FD,$29,$20;{$ENDIF} 

       // next columns:
       add eax, 32;
       add ecx, 32;

   // loop x end
   sub esi, 4;
   jge @@addforxloop4;

   @@addforxloop4end:

   add esi, 4;
   jz @@endProc;

   sub esi, 2;
   jl @@lastcolumn;

   {$IFDEF FPC}vxorpd xmm1, xmm1, xmm1;{$ELSE}db $C5,$F1,$57,$C9;{$ENDIF} 

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov edx, edi;
   @addforyloop2:
       {$IFDEF FPC}vaddpd xmm1, xmm1, [ecx + edx];{$ELSE}db $C5,$F1,$58,$0C,$11;{$ENDIF} 
   add edx, ebx;
   jnz @addforyloop2;

   // build result
   {$IFDEF FPC}vdivpd xmm0, xmm1, xmm2;{$ELSE}db $C5,$F1,$5E,$C2;{$ENDIF} 

   // calculate final standard deviation
   // for y := 0 to height - 1;
   // prepare for reverse loop
   {$IFDEF FPC}vxorpd xmm4, xmm4, xmm4;{$ELSE}db $C5,$D9,$57,$E4;{$ENDIF} 
   mov edx, edi;
   @addforyloop2_2:
      {$IFDEF FPC}vmovapd xmm1, [ecx + edx];{$ELSE}db $C5,$F9,$28,$0C,$11;{$ENDIF} 
      {$IFDEF FPC}vsubpd xmm1, xmm1, xmm0;{$ELSE}db $C5,$F1,$5C,$C8;{$ENDIF} 
      {$IFDEF FPC}vmulpd xmm1, xmm1, xmm1;{$ELSE}db $C5,$F1,$59,$C9;{$ENDIF} 
      {$IFDEF FPC}vaddpd xmm4, xmm4, xmm1;{$ELSE}db $C5,$D9,$58,$E1;{$ENDIF} 
   add edx, ebx;
   jnz @addforyloop2_2;

   // check if we need to use the unbiased version
   cmp unbiased, 0;
   jz @@dobiased_2;

   {$IFDEF FPC}vsubpd xmm3, xmm2, xmm5;{$ELSE}db $C5,$E9,$5C,$DD;{$ENDIF} 
   {$IFDEF FPC}vmaxpd xmm3, xmm3, xmm5;{$ELSE}db $C5,$E1,$5F,$DD;{$ENDIF} 

   {$IFDEF FPC}vdivpd xmm4, xmm4, xmm3;{$ELSE}db $C5,$D9,$5E,$E3;{$ENDIF} 

   jmp @@writeRes2;

   @@dobiased_2:
   {$IFDEF FPC}vdivpd xmm4, xmm4, xmm2;{$ELSE}db $C5,$D9,$5E,$E2;{$ENDIF} 

   // write result
   @@writeRes2:
   {$IFDEF FPC}vmovapd [eax], xmm4;{$ELSE}db $C5,$F9,$29,$20;{$ENDIF} 

   // next columns:
   add eax, 16;
   add ecx, 16;

   dec esi;
   jnz @@endProc;

   @@lastcolumn:

   {$IFDEF FPC}vxorpd xmm1, xmm1, xmm1;{$ELSE}db $C5,$F1,$57,$C9;{$ENDIF} 

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov edx, edi;
   @addforyloop_1:
       {$IFDEF FPC}vaddsd xmm1, xmm1, [ecx + edx];{$ELSE}db $C5,$F3,$58,$0C,$11;{$ENDIF} 
   add edx, ebx;
   jnz @addforyloop_1;

   // build result
   {$IFDEF FPC}vdivsd xmm0, xmm1, xmm2;{$ELSE}db $C5,$F3,$5E,$C2;{$ENDIF} 

   // calculate final standard deviation
   // for y := 0 to height - 1;
   // prepare for reverse loop
   {$IFDEF FPC}vxorpd xmm4, xmm4, xmm4;{$ELSE}db $C5,$D9,$57,$E4;{$ENDIF} 
   mov edx, edi;
   @addforyloop1_2:
      {$IFDEF FPC}vmovsd xmm1, [ecx + edx];{$ELSE}db $C5,$FB,$10,$0C,$11;{$ENDIF} 
      {$IFDEF FPC}vsubsd xmm1, xmm1, xmm0;{$ELSE}db $C5,$F3,$5C,$C8;{$ENDIF} 
      {$IFDEF FPC}vmulsd xmm1, xmm1, xmm1;{$ELSE}db $C5,$F3,$59,$C9;{$ENDIF} 
      {$IFDEF FPC}vaddsd xmm4, xmm4, xmm1;{$ELSE}db $C5,$DB,$58,$E1;{$ENDIF} 
   add edx, ebx;
   jnz @addforyloop1_2;

   // check if we need to use the unbiased version
   cmp unbiased, 0;
   jz @@dobiased_2;

   {$IFDEF FPC}vmovsd xmm3, xmm3, xmm2;{$ELSE}db $C5,$E3,$11,$D3;{$ENDIF} 
   {$IFDEF FPC}vsubsd xmm3, xmm3, xmm5;{$ELSE}db $C5,$E3,$5C,$DD;{$ENDIF} 
   {$IFDEF FPC}vmaxsd xmm3, xmm3, xmm5;{$ELSE}db $C5,$E3,$5F,$DD;{$ENDIF} 

   {$IFDEF FPC}vdivsd xmm4, xmm4, xmm3;{$ELSE}db $C5,$DB,$5E,$E3;{$ENDIF} 

   jmp @@writeRes_1;

   @@dobiased_1:
   {$IFDEF FPC}vdivsd xmm4, xmm4, xmm2;{$ELSE}db $C5,$DB,$5E,$E2;{$ENDIF} 

   // write result
   @@writeRes_1:
   {$IFDEF FPC}vmovsd [eax], xmm4;{$ELSE}db $C5,$FB,$11,$20;{$ENDIF} 

   @@endProc:

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;

procedure AVXMatrixVarColumnUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean);
var tmp : double;
asm
   // prolog
   push ebx;
   push edi;
   push esi;

   // preparations
   mov edi, height;
   {$IFDEF FPC}vcvtsi2sd xmm0, xmm0, edi;{$ELSE}db $C5,$FB,$2A,$C7;{$ENDIF} 
   imul edi, srcLineWidth;
   imul edi, -1;

   // helper registers for the mt1, mt2 and dest pointers
   sub ecx, edi;
   lea esi, tmp;
   {$IFDEF FPC}vmovsd [esi], xmm0;{$ELSE}db $C5,$FB,$11,$06;{$ENDIF} 
   {$IFDEF FPC}vbroadcastsd ymm2, [esi]; {$ELSE}db $C4,$E2,$7D,$19,$16;{$ENDIF} // vbroadcastsd ymm2, xmm0; // avx2

   mov ebx, srcLineWidth;
   lea esi, cLocOne;
   {$IFDEF FPC}vbroadcastsd ymm5, [esi];{$ELSE}db $C4,$E2,$7D,$19,$2E;{$ENDIF} 

   // for x := 0 to width - 1:
   mov esi, Width;
   sub esi, 4;
   jl @@addforxloop4end;

   // 4 columns at once
   @@addforxloop4:
       {$IFDEF FPC}vxorpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$57,$C9;{$ENDIF} 

       // for y := 0 to height - 1;
       // prepare for reverse loop
       mov edx, edi;
       @addforyloop4:
           {$IFDEF FPC}vmovupd ymm3, [ecx + edx];{$ELSE}db $C5,$FD,$10,$1C,$11;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm1, ymm1, ymm3;{$ELSE}db $C5,$F5,$58,$CB;{$ENDIF} 
       add edx, ebx;
       jnz @addforyloop4;

       // build result
       {$IFDEF FPC}vdivpd ymm0, ymm1, ymm2;{$ELSE}db $C5,$F5,$5E,$C2;{$ENDIF} 

       // calculate final standard deviation
       // for y := 0 to height - 1;
       // prepare for reverse loop
       {$IFDEF FPC}vxorpd ymm4, ymm4, ymm4;{$ELSE}db $C5,$DD,$57,$E4;{$ENDIF} 
       mov edx, edi;
       @addforyloop4_2:
           {$IFDEF FPC}vmovupd ymm1, [ecx + edx];{$ELSE}db $C5,$FD,$10,$0C,$11;{$ENDIF} 
           {$IFDEF FPC}vsubpd ymm1, ymm1, ymm0;{$ELSE}db $C5,$F5,$5C,$C8;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm4, ymm4, ymm1;{$ELSE}db $C5,$DD,$58,$E1;{$ENDIF} 
       add edx, ebx;
       jnz @addforyloop4_2;

       // check if we need to use the unbiased version
       cmp unbiased, 0;
       jz @@dobiased_4;

       lea edx, cLocOne;

       {$IFDEF FPC}vbroadcastsd ymm1, [edx];{$ELSE}db $C4,$E2,$7D,$19,$0A;{$ENDIF} 
       {$IFDEF FPC}vsubpd ymm3, ymm2, ymm1;{$ELSE}db $C5,$ED,$5C,$D9;{$ENDIF} 
       {$IFDEF FPC}vmaxpd ymm3, ymm3, ymm1;{$ELSE}db $C5,$E5,$5F,$D9;{$ENDIF} 

       {$IFDEF FPC}vdivpd ymm4, ymm4, ymm3;{$ELSE}db $C5,$DD,$5E,$E3;{$ENDIF} 

       jmp @@writeRes_4;

       @@dobiased_4:
       {$IFDEF FPC}vdivpd ymm4, ymm4, ymm2;{$ELSE}db $C5,$DD,$5E,$E2;{$ENDIF} 

       // write result
       @@writeRes_4:
       {$IFDEF FPC}vmovupd [eax], ymm4;{$ELSE}db $C5,$FD,$11,$20;{$ENDIF} 

       // next columns:
       add eax, 32;
       add ecx, 32;

   // loop x end
   sub esi, 4;
   jge @@addforxloop4;

   @@addforxloop4end:

   add esi, 4;
   jz @@endProc;

   sub esi, 2;
   jl @@lastcolumn;

   {$IFDEF FPC}vxorpd xmm1, xmm1, xmm1;{$ELSE}db $C5,$F1,$57,$C9;{$ENDIF} 

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov edx, edi;
   @addforyloop2:
       {$IFDEF FPC}vmovupd xmm3, [ecx + edx];{$ELSE}db $C5,$F9,$10,$1C,$11;{$ENDIF} 
       {$IFDEF FPC}vaddpd xmm1, xmm1, xmm3;{$ELSE}db $C5,$F1,$58,$CB;{$ENDIF} 
   add edx, ebx;
   jnz @addforyloop2;

   // build result
   {$IFDEF FPC}vdivpd xmm0, xmm1, xmm2;{$ELSE}db $C5,$F1,$5E,$C2;{$ENDIF} 

   // calculate final standard deviation
   // for y := 0 to height - 1;
   // prepare for reverse loop
   {$IFDEF FPC}vxorpd xmm4, xmm4, xmm4;{$ELSE}db $C5,$D9,$57,$E4;{$ENDIF} 
   mov edx, edi;
   @addforyloop2_2:
      {$IFDEF FPC}vmovupd xmm1, [ecx + edx];{$ELSE}db $C5,$F9,$10,$0C,$11;{$ENDIF} 
      {$IFDEF FPC}vsubpd xmm1, xmm1, xmm0;{$ELSE}db $C5,$F1,$5C,$C8;{$ENDIF} 
      {$IFDEF FPC}vmulpd xmm1, xmm1, xmm1;{$ELSE}db $C5,$F1,$59,$C9;{$ENDIF} 
      {$IFDEF FPC}vaddpd xmm4, xmm4, xmm1;{$ELSE}db $C5,$D9,$58,$E1;{$ENDIF} 
   add edx, ebx;
   jnz @addforyloop2_2;

   // check if we need to use the unbiased version
   cmp unbiased, 0;
   jz @@dobiased_2;

   {$IFDEF FPC}vsubpd xmm3, xmm2, xmm5;{$ELSE}db $C5,$E9,$5C,$DD;{$ENDIF} 
   {$IFDEF FPC}vmaxpd xmm3, xmm3, xmm5;{$ELSE}db $C5,$E1,$5F,$DD;{$ENDIF} 

   {$IFDEF FPC}vdivpd xmm4, xmm4, xmm3;{$ELSE}db $C5,$D9,$5E,$E3;{$ENDIF} 

   jmp @@writeRes2;

   @@dobiased_2:
   {$IFDEF FPC}vdivpd xmm4, xmm4, xmm2;{$ELSE}db $C5,$D9,$5E,$E2;{$ENDIF} 

   // write result
   @@writeRes2:
   {$IFDEF FPC}vmovapd [eax], xmm4;{$ELSE}db $C5,$F9,$29,$20;{$ENDIF} 

   // next columns:
   add eax, 16;
   add ecx, 16;

   dec esi;
   jnz @@endProc;

   @@lastcolumn:

   {$IFDEF FPC}vxorpd xmm1, xmm1, xmm1;{$ELSE}db $C5,$F1,$57,$C9;{$ENDIF} 

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov edx, edi;
   @addforyloop_1:
       {$IFDEF FPC}vaddsd xmm1, xmm1, [ecx + edx];{$ELSE}db $C5,$F3,$58,$0C,$11;{$ENDIF} 
   add edx, ebx;
   jnz @addforyloop_1;

   // build result
   {$IFDEF FPC}vdivsd xmm0, xmm1, xmm2;{$ELSE}db $C5,$F3,$5E,$C2;{$ENDIF} 

   // calculate final standard deviation
   // for y := 0 to height - 1;
   // prepare for reverse loop
   {$IFDEF FPC}vxorpd xmm4, xmm4, xmm4;{$ELSE}db $C5,$D9,$57,$E4;{$ENDIF} 
   mov edx, edi;
   @addforyloop1_2:
      {$IFDEF FPC}vmovsd xmm1, [ecx + edx];{$ELSE}db $C5,$FB,$10,$0C,$11;{$ENDIF} 
      {$IFDEF FPC}vsubsd xmm1, xmm1, xmm0;{$ELSE}db $C5,$F3,$5C,$C8;{$ENDIF} 
      {$IFDEF FPC}vmulsd xmm1, xmm1, xmm1;{$ELSE}db $C5,$F3,$59,$C9;{$ENDIF} 
      {$IFDEF FPC}vaddsd xmm4, xmm4, xmm1;{$ELSE}db $C5,$DB,$58,$E1;{$ENDIF} 
   add edx, ebx;
   jnz @addforyloop1_2;

   // check if we need to use the unbiased version
   cmp unbiased, 0;
   jz @@dobiased_2;

   {$IFDEF FPC}vmovsd xmm3, xmm3, xmm2;{$ELSE}db $C5,$E3,$11,$D3;{$ENDIF} 
   {$IFDEF FPC}vsubsd xmm3, xmm3, xmm5;{$ELSE}db $C5,$E3,$5C,$DD;{$ENDIF} 
   {$IFDEF FPC}vmaxsd xmm3, xmm3, xmm5;{$ELSE}db $C5,$E3,$5F,$DD;{$ENDIF} 

   {$IFDEF FPC}vdivsd xmm4, xmm4, xmm3;{$ELSE}db $C5,$DB,$5E,$E3;{$ENDIF} 

   jmp @@writeRes_1;

   @@dobiased_1:
   {$IFDEF FPC}vdivsd xmm4, xmm4, xmm2;{$ELSE}db $C5,$DB,$5E,$E2;{$ENDIF} 

   // write result
   @@writeRes_1:
   {$IFDEF FPC}vmovsd [eax], xmm4;{$ELSE}db $C5,$FB,$11,$20;{$ENDIF} 

   @@endProc:

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;

// #####################################################
// #### Combined mean variance calculation
// #####################################################

procedure AVXMatrixMeanVarRowAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean);
var tmp : double;
    aDestLineWidth : TASMNativeInt;
asm
   // prolog
   push ebx;
   push edi;
   push esi;

   mov aDestLineWidth, edx;

   // iters := -width*sizeof(double)
   mov edi, width;
   {$IFDEF FPC}vcvtsi2sd xmm3, xmm3, edi;{$ELSE}db $C5,$E3,$2A,$DF;{$ENDIF} 
   imul edi, -8;

   // helper registers for the src and dest pointers
   sub ecx, edi;

   // for y := 0 to height - 1:
   mov edx, Height;
   @@addforyloop:
       {$IFDEF FPC}vxorpd ymm0, ymm0, ymm0;{$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} 
       {$IFDEF FPC}vxorpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$57,$C9;{$ENDIF} 

       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov esi, edi;
       @addforxloop:
           add esi, 128;
           jg @loopEnd;
           // prefetch data...
           // prefetch [r8 + esi];

           // addition:
           {$IFDEF FPC}vaddpd ymm0, ymm0, [ecx + esi - 128];{$ELSE}db $C5,$FD,$58,$44,$31,$80;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm1, ymm1, [ecx + esi - 96];{$ELSE}db $C5,$F5,$58,$4C,$31,$A0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, [ecx + esi - 64];{$ELSE}db $C5,$FD,$58,$44,$31,$C0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm1, ymm1, [ecx + esi - 32];{$ELSE}db $C5,$F5,$58,$4C,$31,$E0;{$ENDIF} 
       jmp @addforxloop

       @loopEnd:

       {$IFDEF FPC}vaddpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 
       {$IFDEF FPC}vextractf128 xmm2, ymm0, 1;{$ELSE}db $C4,$E3,$7D,$19,$C2,$01;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm2;{$ELSE}db $C5,$F9,$7C,$C2;{$ENDIF} 

       sub esi, 128;

       jz @buildRes;

       @addforxloop2:
           add esi, 16;
           jg @@addforxloop2end;

           {$IFDEF FPC}vaddpd xmm0, xmm0, [ecx + esi - 16];{$ELSE}db $C5,$F9,$58,$44,$31,$F0;{$ENDIF} 
       jmp @addforxloop2;

       @@addforxloop2end:

       sub esi, 16;
       jz @buildRes;

       {$IFDEF FPC}vaddsd xmm0, xmm0, [ecx + esi];{$ELSE}db $C5,$FB,$58,$04,$31;{$ENDIF} 

       @buildRes:

       // build result
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} 
       {$IFDEF FPC}vdivsd xmm0, xmm0, xmm3;{$ELSE}db $C5,$FB,$5E,$C3;{$ENDIF} 
       {$IFDEF FPC}vmovsd [eax], xmm0;{$ELSE}db $C5,$FB,$11,$00;{$ENDIF} 

       // we have calculated the mean ->
       // repeat the loop to calculate the variance

       lea esi, tmp;
       {$IFDEF FPC}vmovsd [esi], xmm0;{$ELSE}db $C5,$FB,$11,$06;{$ENDIF} 
       {$IFDEF FPC}vbroadcastsd ymm4, [esi];{$ELSE}db $C4,$E2,$7D,$19,$26;{$ENDIF} 
       // vbroadcastsd ymm4, xmm0; // avx2 instruction
       {$IFDEF FPC}vxorpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$57,$C0;{$ENDIF} 

       // for x := 0 to w - 1;
       // prepare for reverse loop
       // variance = sum (x - mean)^2
       mov esi, edi;
       @addforxloop3:
           add esi, 128;
           jg @loopEnd2;
           // prefetch data...
           // prefetch [eax + esi];

           // addition:
           {$IFDEF FPC}vmovapd ymm1, [ecx + esi - 128];{$ELSE}db $C5,$FD,$28,$4C,$31,$80;{$ENDIF} 
           {$IFDEF FPC}vsubpd ymm1, ymm1, ymm4;{$ELSE}db $C5,$F5,$5C,$CC;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm1, [ecx + esi - 96];{$ELSE}db $C5,$FD,$28,$4C,$31,$A0;{$ENDIF} 
           {$IFDEF FPC}vsubpd ymm1, ymm1, ymm4;{$ELSE}db $C5,$F5,$5C,$CC;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm1, [ecx + esi - 64];{$ELSE}db $C5,$FD,$28,$4C,$31,$C0;{$ENDIF} 
           {$IFDEF FPC}vsubpd ymm1, ymm1, ymm4;{$ELSE}db $C5,$F5,$5C,$CC;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm1, [ecx + esi - 32];{$ELSE}db $C5,$FD,$28,$4C,$31,$E0;{$ENDIF} 
           {$IFDEF FPC}vsubpd ymm1, ymm1, ymm4;{$ELSE}db $C5,$F5,$5C,$CC;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 

       jmp @addforxloop3

       @loopEnd2:

       sub esi, 128;

       {$IFDEF FPC}vextractf128 xmm2, ymm0, 1;{$ELSE}db $C4,$E3,$7D,$19,$C2,$01;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm2;{$ELSE}db $C5,$F9,$7C,$C2;{$ENDIF} 

       jz @buildRes2;

       @addforxloop4:
           add esi, 16;
           jg @addforxloop4end;

           {$IFDEF FPC}vmovapd xmm1, [ecx + esi - 16];{$ELSE}db $C5,$F9,$28,$4C,$31,$F0;{$ENDIF} 
           {$IFDEF FPC}vsubpd xmm1, xmm1, xmm4;{$ELSE}db $C5,$F1,$5C,$CC;{$ENDIF} 
           {$IFDEF FPC}vmulpd xmm1, xmm1, xmm1;{$ELSE}db $C5,$F1,$59,$C9;{$ENDIF} 
           {$IFDEF FPC}vaddpd xmm0, xmm0, xmm1;{$ELSE}db $C5,$F9,$58,$C1;{$ENDIF} 
       jmp @addforxloop4;

       @addforxloop4end:

       sub esi, 16;
       jz @buildRes2;

       // last column
       {$IFDEF FPC}vmovsd xmm1, [ecx + esi];{$ELSE}db $C5,$FB,$10,$0C,$31;{$ENDIF} 
       {$IFDEF FPC}vsubsd xmm1, xmm1, xmm4;{$ELSE}db $C5,$F3,$5C,$CC;{$ENDIF} 
       {$IFDEF FPC}vmulsd xmm1, xmm1, xmm1;{$ELSE}db $C5,$F3,$59,$C9;{$ENDIF} 
       {$IFDEF FPC}vaddsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$58,$C1;{$ENDIF} 

       @buildRes2:

       // build result
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} 

       // check if we need to use the unbiased version
       cmp unbiased, 0;
       jz @@dobiased;

       lea esi, cLocOne;
       {$IFDEF FPC}vmovsd xmm2, [esi];{$ELSE}db $C5,$FB,$10,$16;{$ENDIF} 
       {$IFDEF FPC}vsubsd xmm4, xmm3, xmm2;{$ELSE}db $C5,$E3,$5C,$E2;{$ENDIF} 
       {$IFDEF FPC}vmaxsd xmm4, xmm4, xmm2;{$ELSE}db $C5,$DB,$5F,$E2;{$ENDIF} 

       {$IFDEF FPC}vdivsd xmm0, xmm0, xmm4;{$ELSE}db $C5,$FB,$5E,$C4;{$ENDIF} 

       jmp @@writeRes;

       @@dobiased:
       {$IFDEF FPC}vdivsd xmm0, xmm0, xmm3;{$ELSE}db $C5,$FB,$5E,$C3;{$ENDIF} 

       // write result
       @@writeRes:
       {$IFDEF FPC}vmovsd [eax + 8], xmm0;{$ELSE}db $C5,$FB,$11,$40,$08;{$ENDIF} 

       // next line:
       add ecx, srcLineWidth;
       add eax, adestLineWidth;

   // loop y end
   dec edx;
   jnz @@addforyloop;

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;

procedure AVXMatrixMeanVarRowUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean);
var tmp : double;
    aDestLineWidth : TASMNativeInt;
asm
   // prolog
   push ebx;
   push edi;
   push esi;

   mov aDestLineWidth, edx;

   // iters := -width*sizeof(double)
   mov edi, width;
   {$IFDEF FPC}vcvtsi2sd xmm3, xmm3, edi;{$ELSE}db $C5,$E3,$2A,$DF;{$ENDIF} 
   imul edi, -8;

   // helper registers for the src and dest pointers
   sub ecx, edi;

   // for y := 0 to height - 1:
   mov edx, Height;
   @@addforyloop:
       {$IFDEF FPC}vxorpd ymm0, ymm0, ymm0;{$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} 
       {$IFDEF FPC}vxorpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$57,$C9;{$ENDIF} 

       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov esi, edi;
       @addforxloop:
           add esi, 128;
           jg @loopEnd;
           // prefetch data...
           // prefetch [r8 + esi];

           // addition:
           {$IFDEF FPC}vmovupd ymm2, [ecx + esi - 128];{$ELSE}db $C5,$FD,$10,$54,$31,$80;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm2;{$ELSE}db $C5,$FD,$58,$C2;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm4, [ecx + esi - 96];{$ELSE}db $C5,$FD,$10,$64,$31,$A0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm1, ymm1, ymm4;{$ELSE}db $C5,$F5,$58,$CC;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm2, [ecx + esi - 64];{$ELSE}db $C5,$FD,$10,$54,$31,$C0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm2;{$ELSE}db $C5,$FD,$58,$C2;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm4, [ecx + esi - 32];{$ELSE}db $C5,$FD,$10,$64,$31,$E0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm1, ymm1, ymm4;{$ELSE}db $C5,$F5,$58,$CC;{$ENDIF} 
       jmp @addforxloop

       @loopEnd:

       {$IFDEF FPC}vaddpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 
       {$IFDEF FPC}vextractf128 xmm2, ymm0, 1;{$ELSE}db $C4,$E3,$7D,$19,$C2,$01;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm2;{$ELSE}db $C5,$F9,$7C,$C2;{$ENDIF} 

       sub esi, 128;

       jz @buildRes;

       @addforxloop2:
           add esi, 16;
           jg @@addforxloop2end;

           {$IFDEF FPC}vmovupd xmm2, [ecx + esi - 16];{$ELSE}db $C5,$F9,$10,$54,$31,$F0;{$ENDIF} 
           {$IFDEF FPC}vaddpd xmm0, xmm0, xmm2;{$ELSE}db $C5,$F9,$58,$C2;{$ENDIF} 
       jmp @addforxloop2;

       @@addforxloop2end:

       sub esi, 16;
       jz @buildRes;

       {$IFDEF FPC}vaddsd xmm0, xmm0, [ecx + esi];{$ELSE}db $C5,$FB,$58,$04,$31;{$ENDIF} 

       @buildRes:

       // build result
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} 
       {$IFDEF FPC}vdivsd xmm0, xmm0, xmm3;{$ELSE}db $C5,$FB,$5E,$C3;{$ENDIF} 
       {$IFDEF FPC}vmovsd [eax], xmm0;{$ELSE}db $C5,$FB,$11,$00;{$ENDIF} 

       // we have calculated the mean ->
       // repeat the loop to calculate the variance

       lea esi, tmp;
       {$IFDEF FPC}vmovsd [esi], xmm0;{$ELSE}db $C5,$FB,$11,$06;{$ENDIF} 
       {$IFDEF FPC}vbroadcastsd ymm4, [esi];{$ELSE}db $C4,$E2,$7D,$19,$26;{$ENDIF} 
       // vbroadcastsd ymm4, xmm0; // avx2 instruction
       {$IFDEF FPC}vxorpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$57,$C0;{$ENDIF} 

       // for x := 0 to w - 1;
       // prepare for reverse loop
       // variance = sum (x - mean)^2
       mov esi, edi;
       @addforxloop3:
           add esi, 128;
           jg @loopEnd2;
           // prefetch data...
           // prefetch [eax + esi];

           // addition:
           {$IFDEF FPC}vmovupd ymm1, [ecx + esi - 128];{$ELSE}db $C5,$FD,$10,$4C,$31,$80;{$ENDIF} 
           {$IFDEF FPC}vsubpd ymm1, ymm1, ymm4;{$ELSE}db $C5,$F5,$5C,$CC;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm1, [ecx + esi - 96];{$ELSE}db $C5,$FD,$10,$4C,$31,$A0;{$ENDIF} 
           {$IFDEF FPC}vsubpd ymm1, ymm1, ymm4;{$ELSE}db $C5,$F5,$5C,$CC;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm1, [ecx + esi - 64];{$ELSE}db $C5,$FD,$10,$4C,$31,$C0;{$ENDIF} 
           {$IFDEF FPC}vsubpd ymm1, ymm1, ymm4;{$ELSE}db $C5,$F5,$5C,$CC;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm1, [ecx + esi - 32];{$ELSE}db $C5,$FD,$10,$4C,$31,$E0;{$ENDIF} 
           {$IFDEF FPC}vsubpd ymm1, ymm1, ymm4;{$ELSE}db $C5,$F5,$5C,$CC;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 

       jmp @addforxloop3

       @loopEnd2:

       sub esi, 128;

       {$IFDEF FPC}vextractf128 xmm2, ymm0, 1;{$ELSE}db $C4,$E3,$7D,$19,$C2,$01;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm2;{$ELSE}db $C5,$F9,$7C,$C2;{$ENDIF} 

       jz @buildRes2;

       @addforxloop4:
           add esi, 16;
           jg @addforxloop4end;

           {$IFDEF FPC}vmovupd xmm1, [ecx + esi - 16];{$ELSE}db $C5,$F9,$10,$4C,$31,$F0;{$ENDIF} 
           {$IFDEF FPC}vsubpd xmm1, xmm1, xmm4;{$ELSE}db $C5,$F1,$5C,$CC;{$ENDIF} 
           {$IFDEF FPC}vmulpd xmm1, xmm1, xmm1;{$ELSE}db $C5,$F1,$59,$C9;{$ENDIF} 
           {$IFDEF FPC}vaddpd xmm0, xmm0, xmm1;{$ELSE}db $C5,$F9,$58,$C1;{$ENDIF} 
       jmp @addforxloop4;

       @addforxloop4end:

       sub esi, 16;
       jz @buildRes2;

       // last column
       {$IFDEF FPC}vmovsd xmm1, [ecx + esi];{$ELSE}db $C5,$FB,$10,$0C,$31;{$ENDIF} 
       {$IFDEF FPC}vsubsd xmm1, xmm1, xmm4;{$ELSE}db $C5,$F3,$5C,$CC;{$ENDIF} 
       {$IFDEF FPC}vmulsd xmm1, xmm1, xmm1;{$ELSE}db $C5,$F3,$59,$C9;{$ENDIF} 
       {$IFDEF FPC}vaddsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$58,$C1;{$ENDIF} 

       @buildRes2:

       // build result
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} 

       // check if we need to use the unbiased version
       cmp unbiased, 0;
       jz @@dobiased;

       lea esi, cLocOne;
       {$IFDEF FPC}vmovsd xmm2, [esi];{$ELSE}db $C5,$FB,$10,$16;{$ENDIF} 
       {$IFDEF FPC}vsubsd xmm4, xmm3, xmm2;{$ELSE}db $C5,$E3,$5C,$E2;{$ENDIF} 
       {$IFDEF FPC}vmaxsd xmm4, xmm4, xmm2;{$ELSE}db $C5,$DB,$5F,$E2;{$ENDIF} 

       {$IFDEF FPC}vdivsd xmm0, xmm0, xmm4;{$ELSE}db $C5,$FB,$5E,$C4;{$ENDIF} 

       jmp @@writeRes;

       @@dobiased:
       {$IFDEF FPC}vdivsd xmm0, xmm0, xmm3;{$ELSE}db $C5,$FB,$5E,$C3;{$ENDIF} 

       // write result
       @@writeRes:
       {$IFDEF FPC}vmovsd [eax + 8], xmm0;{$ELSE}db $C5,$FB,$11,$40,$08;{$ENDIF} 

       // next line:
       add ecx, srcLineWidth;
       add eax, adestLineWidth;

   // loop y end
   dec edx;
   jnz @@addforyloop;

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;

procedure AVXMatrixMeanVarColumnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean);
var tmp : double;
    aDestLineWidth : TASMNativeInt;
asm
   // prolog
   push ebx;
   push edi;
   push esi;

   mov aDestLineWidth, edx;

   // preparations
   mov edi, height;
   {$IFDEF FPC}vcvtsi2sd xmm0, xmm0, edi;{$ELSE}db $C5,$FB,$2A,$C7;{$ENDIF} 
   imul edi, srcLineWidth;
   imul edi, -1;

   // helper registers for the mt1, mt2 and dest pointers
   sub ecx, edi;

   mov ebx, srcLineWidth;

   lea edx, tmp;
   {$IFDEF FPC}vmovsd [edx], xmm0;{$ELSE}db $C5,$FB,$11,$02;{$ENDIF} 
   {$IFDEF FPC}vbroadcastsd ymm2, [edx]; {$ELSE}db $C4,$E2,$7D,$19,$12;{$ENDIF} // vbroadcastsd ymm2, xmm0; // avx2
   

   lea edx, cLocOne;
   {$IFDEF FPC}vbroadcastsd ymm5, [edx];{$ELSE}db $C4,$E2,$7D,$19,$2A;{$ENDIF} 

   // for x := 0 to width - 1:
   mov esi, Width;
   sub esi, 4;
   jl @@addforxloop4end;

   // 4 columns at once
   @@addforxloop4:
       {$IFDEF FPC}vxorpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$57,$C9;{$ENDIF} 

       // for y := 0 to height - 1;
       // prepare for reverse loop
       mov edx, edi;
       @addforyloop4:
           {$IFDEF FPC}vaddpd ymm1, ymm1, [ecx + edx];{$ELSE}db $C5,$F5,$58,$0C,$11;{$ENDIF} 
       add edx, ebx;
       jnz @addforyloop4;

       // build result
       {$IFDEF FPC}vdivpd ymm0, ymm1, ymm2;{$ELSE}db $C5,$F5,$5E,$C2;{$ENDIF} 
       {$IFDEF FPC}vmovapd [eax], ymm0;{$ELSE}db $C5,$FD,$29,$00;{$ENDIF} 

       // calculate final standard deviation
       // for y := 0 to height - 1;
       // prepare for reverse loop
       {$IFDEF FPC}vxorpd ymm4, ymm4, ymm4;{$ELSE}db $C5,$DD,$57,$E4;{$ENDIF} 
       mov edx, edi;
       @addforyloop4_2:
           {$IFDEF FPC}vmovapd ymm1, [ecx + edx];{$ELSE}db $C5,$FD,$28,$0C,$11;{$ENDIF} 
           {$IFDEF FPC}vsubpd ymm1, ymm1, ymm0;{$ELSE}db $C5,$F5,$5C,$C8;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm4, ymm4, ymm1;{$ELSE}db $C5,$DD,$58,$E1;{$ENDIF} 
       add edx, ebx;
       jnz @addforyloop4_2;

       // check if we need to use the unbiased version
       cmp unbiased, 0;
       jz @@dobiased_4;

       lea edx, cLocOne;

       {$IFDEF FPC}vbroadcastsd ymm1, [edx];{$ELSE}db $C4,$E2,$7D,$19,$0A;{$ENDIF} 
       {$IFDEF FPC}vsubpd ymm3, ymm2, ymm1;{$ELSE}db $C5,$ED,$5C,$D9;{$ENDIF} 
       {$IFDEF FPC}vmaxpd ymm3, ymm3, ymm1;{$ELSE}db $C5,$E5,$5F,$D9;{$ENDIF} 

       {$IFDEF FPC}vdivpd ymm4, ymm4, ymm3;{$ELSE}db $C5,$DD,$5E,$E3;{$ENDIF} 

       jmp @@writeRes_4;

       @@dobiased_4:
       {$IFDEF FPC}vdivpd ymm4, ymm4, ymm2;{$ELSE}db $C5,$DD,$5E,$E2;{$ENDIF} 

       // write result
       @@writeRes_4:
       mov edx, adestLineWidth;
       {$IFDEF FPC}vmovapd [eax + edx], ymm4;{$ELSE}db $C5,$FD,$29,$24,$10;{$ENDIF} 

       // next columns:
       add eax, 32;
       add ecx, 32;

   // loop x end
   sub esi, 4;
   jge @@addforxloop4;

   @@addforxloop4end:

   add esi, 4;
   jz @@endProc;

   sub esi, 2;
   jl @@lastcolumn;

   {$IFDEF FPC}vxorpd xmm1, xmm1, xmm1;{$ELSE}db $C5,$F1,$57,$C9;{$ENDIF} 

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov edx, edi;
   @addforyloop2:
       {$IFDEF FPC}vaddpd xmm1, xmm1, [ecx + edx];{$ELSE}db $C5,$F1,$58,$0C,$11;{$ENDIF} 
   add edx, ebx;
   jnz @addforyloop2;

   // build result
   {$IFDEF FPC}vdivpd xmm0, xmm1, xmm2;{$ELSE}db $C5,$F1,$5E,$C2;{$ENDIF} 
   {$IFDEF FPC}vmovapd [eax], xmm0;{$ELSE}db $C5,$F9,$29,$00;{$ENDIF} 

   // calculate final standard deviation
   // for y := 0 to height - 1;
   // prepare for reverse loop
   {$IFDEF FPC}vxorpd xmm4, xmm4, xmm4;{$ELSE}db $C5,$D9,$57,$E4;{$ENDIF} 
   mov edx, edi;
   @addforyloop2_2:
      {$IFDEF FPC}vmovapd xmm1, [ecx + edx];{$ELSE}db $C5,$F9,$28,$0C,$11;{$ENDIF} 
      {$IFDEF FPC}vsubpd xmm1, xmm1, xmm0;{$ELSE}db $C5,$F1,$5C,$C8;{$ENDIF} 
      {$IFDEF FPC}vmulpd xmm1, xmm1, xmm1;{$ELSE}db $C5,$F1,$59,$C9;{$ENDIF} 
      {$IFDEF FPC}vaddpd xmm4, xmm4, xmm1;{$ELSE}db $C5,$D9,$58,$E1;{$ENDIF} 
   add edx, ebx;
   jnz @addforyloop2_2;

   // check if we need to use the unbiased version
   cmp unbiased, 0;
   jz @@dobiased_2;

   {$IFDEF FPC}vsubpd xmm3, xmm2, xmm5;{$ELSE}db $C5,$E9,$5C,$DD;{$ENDIF} 
   {$IFDEF FPC}vmaxpd xmm3, xmm3, xmm5;{$ELSE}db $C5,$E1,$5F,$DD;{$ENDIF} 

   {$IFDEF FPC}vdivpd xmm4, xmm4, xmm3;{$ELSE}db $C5,$D9,$5E,$E3;{$ENDIF} 

   jmp @@writeRes2;

   @@dobiased_2:
   {$IFDEF FPC}vdivpd xmm4, xmm4, xmm2;{$ELSE}db $C5,$D9,$5E,$E2;{$ENDIF} 

   // write result
   @@writeRes2:
   mov edx, adestLineWidth;
   {$IFDEF FPC}vmovapd [eax + edx], xmm4;{$ELSE}db $C5,$F9,$29,$24,$10;{$ENDIF} 

   // next columns:
   add eax, 16;
   add ecx, 16;

   dec esi;
   jnz @@endProc;

   @@lastcolumn:

   {$IFDEF FPC}vxorpd xmm1, xmm1, xmm1;{$ELSE}db $C5,$F1,$57,$C9;{$ENDIF} 

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov edx, edi;
   @addforyloop_1:
       {$IFDEF FPC}vaddsd xmm1, xmm1, [ecx + edx];{$ELSE}db $C5,$F3,$58,$0C,$11;{$ENDIF} 
   add edx, ebx;
   jnz @addforyloop_1;

   // build result
   {$IFDEF FPC}vdivsd xmm0, xmm1, xmm2;{$ELSE}db $C5,$F3,$5E,$C2;{$ENDIF} 
   {$IFDEF FPC}vmovsd [eax], xmm0;{$ELSE}db $C5,$FB,$11,$00;{$ENDIF} 

   // calculate final standard deviation
   // for y := 0 to height - 1;
   // prepare for reverse loop
   {$IFDEF FPC}vxorpd xmm4, xmm4, xmm4;{$ELSE}db $C5,$D9,$57,$E4;{$ENDIF} 
   mov edx, edi;
   @addforyloop1_2:
      {$IFDEF FPC}vmovsd xmm1, [ecx + edx];{$ELSE}db $C5,$FB,$10,$0C,$11;{$ENDIF} 
      {$IFDEF FPC}vsubsd xmm1, xmm1, xmm0;{$ELSE}db $C5,$F3,$5C,$C8;{$ENDIF} 
      {$IFDEF FPC}vmulsd xmm1, xmm1, xmm1;{$ELSE}db $C5,$F3,$59,$C9;{$ENDIF} 
      {$IFDEF FPC}vaddsd xmm4, xmm4, xmm1;{$ELSE}db $C5,$DB,$58,$E1;{$ENDIF} 
   add edx, ebx;
   jnz @addforyloop1_2;

   // check if we need to use the unbiased version
   cmp unbiased, 0;
   jz @@dobiased_2;

   {$IFDEF FPC}vmovsd xmm3, xmm3, xmm2;{$ELSE}db $C5,$E3,$11,$D3;{$ENDIF} 
   {$IFDEF FPC}vsubsd xmm3, xmm3, xmm5;{$ELSE}db $C5,$E3,$5C,$DD;{$ENDIF} 
   {$IFDEF FPC}vmaxsd xmm3, xmm3, xmm5;{$ELSE}db $C5,$E3,$5F,$DD;{$ENDIF} 

   {$IFDEF FPC}vdivsd xmm4, xmm4, xmm3;{$ELSE}db $C5,$DB,$5E,$E3;{$ENDIF} 

   jmp @@writeRes_1;

   @@dobiased_1:
   {$IFDEF FPC}vdivsd xmm4, xmm4, xmm2;{$ELSE}db $C5,$DB,$5E,$E2;{$ENDIF} 

   // write result
   @@writeRes_1:
   mov edx, adestLineWidth;
   {$IFDEF FPC}vmovsd [eax + edx], xmm4;{$ELSE}db $C5,$FB,$11,$24,$10;{$ENDIF} 

   @@endProc:

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;

procedure AVXMatrixMeanVarColumnUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean);
var tmp : double;
    aDestLineWidth : TASMNativeInt;
asm
   // prolog
   push ebx;
   push edi;
   push esi;

   mov aDestLineWidth, edx;

   // preparations
   mov edi, height;
   {$IFDEF FPC}vcvtsi2sd xmm0, xmm0, edi;{$ELSE}db $C5,$FB,$2A,$C7;{$ENDIF} 
   imul edi, srcLineWidth;
   imul edi, -1;

   // helper registers for the mt1, mt2 and dest pointers
   sub ecx, edi;

   mov ebx, srcLineWidth;

   lea edx, tmp;
   {$IFDEF FPC}vmovsd [edx], xmm0;{$ELSE}db $C5,$FB,$11,$02;{$ENDIF} 
   {$IFDEF FPC}vbroadcastsd ymm2, [edx]; {$ELSE}db $C4,$E2,$7D,$19,$12;{$ENDIF} // vbroadcastsd ymm2, xmm0; // avx2

   lea edx, cLocOne;
   {$IFDEF FPC}vbroadcastsd ymm5, [edx];{$ELSE}db $C4,$E2,$7D,$19,$2A;{$ENDIF} 

   // for x := 0 to width - 1:
   mov esi, Width;
   sub esi, 4;
   jl @@addforxloop4end;

   // 4 columns at once
   @@addforxloop4:
       {$IFDEF FPC}vxorpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$57,$C9;{$ENDIF} 

       // for y := 0 to height - 1;
       // prepare for reverse loop
       mov edx, edi;
       @addforyloop4:
           {$IFDEF FPC}vmovupd ymm0, [ecx + edx];{$ELSE}db $C5,$FD,$10,$04,$11;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm1, ymm1, ymm0;{$ELSE}db $C5,$F5,$58,$C8;{$ENDIF} 
       add edx, ebx;
       jnz @addforyloop4;

       // build result
       {$IFDEF FPC}vdivpd ymm0, ymm1, ymm2;{$ELSE}db $C5,$F5,$5E,$C2;{$ENDIF} 
       {$IFDEF FPC}vmovupd [eax], ymm0;{$ELSE}db $C5,$FD,$11,$00;{$ENDIF} 

       // calculate final standard deviation
       // for y := 0 to height - 1;
       // prepare for reverse loop
       {$IFDEF FPC}vxorpd ymm4, ymm4, ymm4;{$ELSE}db $C5,$DD,$57,$E4;{$ENDIF} 
       mov edx, edi;
       @addforyloop4_2:
           {$IFDEF FPC}vmovupd ymm1, [ecx + edx];{$ELSE}db $C5,$FD,$10,$0C,$11;{$ENDIF} 
           {$IFDEF FPC}vsubpd ymm1, ymm1, ymm0;{$ELSE}db $C5,$F5,$5C,$C8;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm4, ymm4, ymm1;{$ELSE}db $C5,$DD,$58,$E1;{$ENDIF} 
       add edx, ebx;
       jnz @addforyloop4_2;

       // check if we need to use the unbiased version
       cmp unbiased, 0;
       jz @@dobiased_4;

       lea edx, cLocOne;
       {$IFDEF FPC}vbroadcastsd ymm1, [edx];{$ELSE}db $C4,$E2,$7D,$19,$0A;{$ENDIF} 
       {$IFDEF FPC}vsubpd ymm3, ymm2, ymm1;{$ELSE}db $C5,$ED,$5C,$D9;{$ENDIF} 
       {$IFDEF FPC}vmaxpd ymm3, ymm3, ymm1;{$ELSE}db $C5,$E5,$5F,$D9;{$ENDIF} 

       {$IFDEF FPC}vdivpd ymm4, ymm4, ymm3;{$ELSE}db $C5,$DD,$5E,$E3;{$ENDIF} 

       jmp @@writeRes_4;

       @@dobiased_4:
       {$IFDEF FPC}vdivpd ymm4, ymm4, ymm2;{$ELSE}db $C5,$DD,$5E,$E2;{$ENDIF} 

       // write result
       @@writeRes_4:
       mov edx, adestLineWidth;
       {$IFDEF FPC}vmovupd [eax + edx], ymm4;{$ELSE}db $C5,$FD,$11,$24,$10;{$ENDIF} 

       // next columns:
       add eax, 32;
       add ecx, 32;

   // loop x end
   sub esi, 4;
   jge @@addforxloop4;

   @@addforxloop4end:

   add esi, 4;
   jz @@endProc;

   sub esi, 2;
   jl @@lastcolumn;

   {$IFDEF FPC}vxorpd xmm1, xmm1, xmm1;{$ELSE}db $C5,$F1,$57,$C9;{$ENDIF} 

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov edx, edi;
   @addforyloop2:
       {$IFDEF FPC}vmovupd xmm0, [ecx + edx];{$ELSE}db $C5,$F9,$10,$04,$11;{$ENDIF} 
       {$IFDEF FPC}vaddpd xmm1, xmm1, xmm0;{$ELSE}db $C5,$F1,$58,$C8;{$ENDIF} 
   add edx, ebx;
   jnz @addforyloop2;

   // build result
   {$IFDEF FPC}vdivpd xmm0, xmm1, xmm2;{$ELSE}db $C5,$F1,$5E,$C2;{$ENDIF} 
   {$IFDEF FPC}vmovupd [eax], xmm0;{$ELSE}db $C5,$F9,$11,$00;{$ENDIF} 

   // calculate final standard deviation
   // for y := 0 to height - 1;
   // prepare for reverse loop
   {$IFDEF FPC}vxorpd xmm4, xmm4, xmm4;{$ELSE}db $C5,$D9,$57,$E4;{$ENDIF} 
   mov edx, edi;
   @addforyloop2_2:
      {$IFDEF FPC}vmovupd xmm1, [ecx + edx];{$ELSE}db $C5,$F9,$10,$0C,$11;{$ENDIF} 
      {$IFDEF FPC}vsubpd xmm1, xmm1, xmm0;{$ELSE}db $C5,$F1,$5C,$C8;{$ENDIF} 
      {$IFDEF FPC}vmulpd xmm1, xmm1, xmm1;{$ELSE}db $C5,$F1,$59,$C9;{$ENDIF} 
      {$IFDEF FPC}vaddpd xmm4, xmm4, xmm1;{$ELSE}db $C5,$D9,$58,$E1;{$ENDIF} 
   add edx, ebx;
   jnz @addforyloop2_2;

   // check if we need to use the unbiased version
   cmp unbiased, 0;
   jz @@dobiased_2;

   {$IFDEF FPC}vsubpd xmm3, xmm2, xmm5;{$ELSE}db $C5,$E9,$5C,$DD;{$ENDIF} 
   {$IFDEF FPC}vmaxpd xmm3, xmm3, xmm5;{$ELSE}db $C5,$E1,$5F,$DD;{$ENDIF} 

   {$IFDEF FPC}vdivpd xmm4, xmm4, xmm3;{$ELSE}db $C5,$D9,$5E,$E3;{$ENDIF} 

   jmp @@writeRes2;

   @@dobiased_2:
   {$IFDEF FPC}vdivpd xmm4, xmm4, xmm2;{$ELSE}db $C5,$D9,$5E,$E2;{$ENDIF} 

   // write result
   @@writeRes2:
   mov edx, adestLineWidth;
   {$IFDEF FPC}vmovupd [eax + edx], xmm4;{$ELSE}db $C5,$F9,$11,$24,$10;{$ENDIF} 

   // next columns:
   add eax, 16;
   add ecx, 16;

   dec esi;
   jnz @@endProc;

   @@lastcolumn:

   {$IFDEF FPC}vxorpd xmm1, xmm1, xmm1;{$ELSE}db $C5,$F1,$57,$C9;{$ENDIF} 

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov edx, edi;
   @addforyloop_1:
       {$IFDEF FPC}vaddsd xmm1, xmm1, [ecx + edx];{$ELSE}db $C5,$F3,$58,$0C,$11;{$ENDIF} 
   add edx, ebx;
   jnz @addforyloop_1;

   // build result
   {$IFDEF FPC}vdivsd xmm0, xmm1, xmm2;{$ELSE}db $C5,$F3,$5E,$C2;{$ENDIF} 
   {$IFDEF FPC}vmovsd [eax], xmm0;{$ELSE}db $C5,$FB,$11,$00;{$ENDIF} 

   // calculate final standard deviation
   // for y := 0 to height - 1;
   // prepare for reverse loop
   {$IFDEF FPC}vxorpd xmm4, xmm4, xmm4;{$ELSE}db $C5,$D9,$57,$E4;{$ENDIF} 
   mov edx, edi;
   @addforyloop1_2:
      {$IFDEF FPC}vmovsd xmm1, [ecx + edx];{$ELSE}db $C5,$FB,$10,$0C,$11;{$ENDIF} 
      {$IFDEF FPC}vsubsd xmm1, xmm1, xmm0;{$ELSE}db $C5,$F3,$5C,$C8;{$ENDIF} 
      {$IFDEF FPC}vmulsd xmm1, xmm1, xmm1;{$ELSE}db $C5,$F3,$59,$C9;{$ENDIF} 
      {$IFDEF FPC}vaddsd xmm4, xmm4, xmm1;{$ELSE}db $C5,$DB,$58,$E1;{$ENDIF} 
   add edx, ebx;
   jnz @addforyloop1_2;

   // check if we need to use the unbiased version
   cmp unbiased, 0;
   jz @@dobiased_2;

   {$IFDEF FPC}vmovsd xmm3, xmm3, xmm2;{$ELSE}db $C5,$E3,$11,$D3;{$ENDIF} 
   {$IFDEF FPC}vsubsd xmm3, xmm3, xmm5;{$ELSE}db $C5,$E3,$5C,$DD;{$ENDIF} 
   {$IFDEF FPC}vmaxsd xmm3, xmm3, xmm5;{$ELSE}db $C5,$E3,$5F,$DD;{$ENDIF} 

   {$IFDEF FPC}vdivsd xmm4, xmm4, xmm3;{$ELSE}db $C5,$DB,$5E,$E3;{$ENDIF} 

   jmp @@writeRes_1;

   @@dobiased_1:
   {$IFDEF FPC}vdivsd xmm4, xmm4, xmm2;{$ELSE}db $C5,$DB,$5E,$E2;{$ENDIF} 

   // write result
   @@writeRes_1:
   mov edx, adestLineWidth;
   {$IFDEF FPC}vmovsd [eax + edx], xmm4;{$ELSE}db $C5,$FB,$11,$24,$10;{$ENDIF} 

   @@endProc:

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;

{$ENDIF}

end.
