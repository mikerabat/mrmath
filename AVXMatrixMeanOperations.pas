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

procedure AVXMatrixMeanRowAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
procedure AVXMatrixMeanRowUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);

procedure AVXMatrixMeanColumnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
procedure AVXMatrixMeanColumnUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);

procedure AVXMatrixVarRowAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean);
procedure AVXMatrixVarRowUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean);

procedure AVXMatrixVarColumnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean);
procedure AVXMatrixVarColumnUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean);


// combined methods
procedure AVXMatrixMeanVarRowAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean);
procedure AVXMatrixMeanVarRowUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean);

procedure AVXMatrixMeanVarColumnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean);
procedure AVXMatrixMeanVarColumnUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean);

{$ENDIF}

implementation

{$IFNDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$S-} {$ENDIF}

const cLocOne : double = 1;

procedure AVXMatrixMeanRowAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
begin
asm
   // prolog
   push edi;
   push esi;
   push ebx;

   // iters := -width*sizeof(double)
   mov edi, width;
   imul edi, -8;

   // helper registers for the mt1, mt2 and dest pointers
   mov edx, src;
   sub edx, edi;
   mov ecx, dest;

   // fpc seems to have a problem with this opcode
   {$IFDEF FPC}
   mov eax, width;
   {$IFDEF FPC}vcvtsi2sd xmm3, xmm3, eax;{$ELSE}db $C5,$E3,$2A,$D8;{$ENDIF} 
   {$ELSE}
   cvtsi2sd xmm3, width;
   {$ENDIF}

   // for y := 0 to height - 1:
   mov esi, Height;
   @@addforyloop:
       {$IFDEF FPC}vxorpd ymm0, ymm0, ymm0;{$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} 
       {$IFDEF FPC}vxorpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$57,$C9;{$ENDIF} 

       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov eax, edi;
       @addforxloop:
           add eax, 128;
           jg @loopEnd;
           // prefetch data...
           // prefetch [r8 + eax];

           // addition:
           {$IFDEF FPC}vaddpd ymm0, ymm0, [edx + eax - 128];{$ELSE}db $C5,$FD,$58,$44,$02,$80;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm1, ymm1, [edx + eax - 96];{$ELSE}db $C5,$F5,$58,$4C,$02,$A0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, [edx + eax - 64];{$ELSE}db $C5,$FD,$58,$44,$02,$C0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm1, ymm1, [edx + eax - 32];{$ELSE}db $C5,$F5,$58,$4C,$02,$E0;{$ENDIF} 
       jmp @addforxloop

       @loopEnd:

       {$IFDEF FPC}vaddpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 
       {$IFDEF FPC}vextractf128 xmm2, ymm0, 1;{$ELSE}db $C4,$E3,$7D,$19,$C2,$01;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm2;{$ELSE}db $C5,$F9,$7C,$C2;{$ENDIF} 

       sub eax, 128;

       jz @buildRes;

       @addforxloop2:
           add eax, 16;
           jg @@addforxloop2end;

           {$IFDEF FPC}vaddpd xmm0, xmm0, [edx + eax - 16];{$ELSE}db $C5,$F9,$58,$44,$02,$F0;{$ENDIF} 
       jmp @addforxloop2;

       @@addforxloop2end:

       sub eax, 16;
       jz @buildRes;

       {$IFDEF FPC}vaddsd xmm0, xmm0, [edx + eax];{$ELSE}db $C5,$FB,$58,$04,$02;{$ENDIF} 

       @buildRes:

       // build result
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} 

       {$IFDEF FPC}vdivsd xmm0, xmm0, xmm3;{$ELSE}db $C5,$FB,$5E,$C3;{$ENDIF} 

       // write result
       {$IFDEF FPC}vmovsd [ecx], xmm0;{$ELSE}db $C5,$FB,$11,$01;{$ENDIF} 

       // next line:
       add edx, srcLineWidth;
       add ecx, destLineWidth;

   // loop y end
   dec esi;
   jnz @@addforyloop;

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop ebx;
   pop esi;
   pop edi;
end;
end;

procedure AVXMatrixMeanRowUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
begin
asm
   // prolog
   push edi;
   push esi;
   push ebx;

   // iters := -width*sizeof(double)
   mov edi, width;
   imul edi, -8;

   // helper registers for the mt1, mt2 and dest pointers
   mov edx, src;
   sub edx, edi;
   mov ecx, dest;

   // fpc seems to have a problem with this opcode
   {$IFDEF FPC}
   mov eax, width;
   {$IFDEF FPC}vcvtsi2sd xmm3, xmm3, eax;{$ELSE}db $C5,$E3,$2A,$D8;{$ENDIF} 
   {$ELSE}
   cvtsi2sd xmm3, width;
   {$ENDIF}

   // for y := 0 to height - 1:
   mov esi, Height;
   @@addforyloop:
       {$IFDEF FPC}vxorpd ymm0, ymm0, ymm0;{$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} 
       {$IFDEF FPC}vxorpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$57,$C9;{$ENDIF} 

       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov eax, edi;
       @addforxloop:
           add eax, 128;
           jg @loopEnd;
           // prefetch data...
           // prefetch [r8 + eax];

           // addition:
           {$IFDEF FPC}vmovupd ymm2, [edx + eax - 128];{$ELSE}db $C5,$FD,$10,$54,$02,$80;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm2;{$ELSE}db $C5,$FD,$58,$C2;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm2, [edx + eax - 96];{$ELSE}db $C5,$FD,$10,$54,$02,$A0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm1, ymm1, ymm2;{$ELSE}db $C5,$F5,$58,$CA;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm2, [edx + eax - 64];{$ELSE}db $C5,$FD,$10,$54,$02,$C0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm2;{$ELSE}db $C5,$FD,$58,$C2;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm2, [edx + eax - 32];{$ELSE}db $C5,$FD,$10,$54,$02,$E0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm1, ymm1, ymm2;{$ELSE}db $C5,$F5,$58,$CA;{$ENDIF} 
       jmp @addforxloop

       @loopEnd:

       {$IFDEF FPC}vaddpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 
       {$IFDEF FPC}vextractf128 xmm2, ymm0, 1;{$ELSE}db $C4,$E3,$7D,$19,$C2,$01;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm2;{$ELSE}db $C5,$F9,$7C,$C2;{$ENDIF} 

       sub eax, 128;

       jz @buildRes;

       @addforxloop2:
           add eax, 16;
           jg @@addforxloop2end;

           {$IFDEF FPC}vmovupd xmm2, [edx + eax - 16];{$ELSE}db $C5,$F9,$10,$54,$02,$F0;{$ENDIF} 
           {$IFDEF FPC}vaddpd xmm0, xmm0, xmm2;{$ELSE}db $C5,$F9,$58,$C2;{$ENDIF} 
       jmp @addforxloop2;

       @@addforxloop2end:

       sub eax, 16;
       jz @buildRes;

       {$IFDEF FPC}vaddsd xmm0, xmm0, [edx + eax];{$ELSE}db $C5,$FB,$58,$04,$02;{$ENDIF} 

       @buildRes:

       // build result
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} 

       {$IFDEF FPC}vdivsd xmm0, xmm0, xmm3;{$ELSE}db $C5,$FB,$5E,$C3;{$ENDIF} 

       // write result
       {$IFDEF FPC}vmovsd [ecx], xmm0;{$ELSE}db $C5,$FB,$11,$01;{$ENDIF} 

       // next line:
       add edx, srcLineWidth;
       add ecx, destLineWidth;

   // loop y end
   dec esi;
   jnz @@addforyloop;

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop ebx;
   pop esi;
   pop edi;
end;
end;

procedure AVXMatrixMeanColumnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
var tmp : double;
begin
asm
   // prolog
   push ebx;
   push edi;
   push esi;

   // prepare
   xor ebx, ebx;
   sub ebx, height;
   imul ebx, srcLineWidth;

   // helper registers for the mt1, mt2 and dest pointers
   mov ecx, dest;
   mov edx, src;
   sub edx, ebx;

   mov edi, srcLineWidth;

   // fpc seems to have a problem with this opcode
   {$IFDEF FPC}
   mov eax, height;
   {$IFDEF FPC}vcvtsi2sd xmm0, xmm0, eax;{$ELSE}db $C5,$FB,$2A,$C0;{$ENDIF} 
   {$ELSE}
   cvtsi2sd xmm0, height;
   {$ENDIF}
   lea eax, tmp;
   {$IFDEF FPC}vmovsd [eax], xmm0;{$ELSE}db $C5,$FB,$11,$00;{$ENDIF} 
   {$IFDEF FPC}vbroadcastsd ymm2, [eax];{$ELSE}db $C4,$E2,$7D,$19,$10;{$ENDIF} 
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
       mov eax, ebx;
       @addforyloop4:
           {$IFDEF FPC}vaddpd ymm1, ymm1, [edx + eax];{$ELSE}db $C5,$F5,$58,$0C,$02;{$ENDIF} 
       add eax, edi;
       jnz @addforyloop4;

       // build result
       {$IFDEF FPC}vdivpd ymm1, ymm1, ymm2;{$ELSE}db $C5,$F5,$5E,$CA;{$ENDIF} 
       {$IFDEF FPC}vmovapd [ecx], ymm1;{$ELSE}db $C5,$FD,$29,$09;{$ENDIF} 

       // next columns:
       add ecx, 32;
       add edx, 32;

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
   mov eax, ebx;
   @addforyloop2:
       {$IFDEF FPC}vaddpd xmm1, xmm1, [edx + eax];{$ELSE}db $C5,$F1,$58,$0C,$02;{$ENDIF} 
   add eax, edi;
   jnz @addforyloop2;

   // build result
   {$IFDEF FPC}vdivpd xmm1, xmm1, xmm2;{$ELSE}db $C5,$F1,$5E,$CA;{$ENDIF} 
   {$IFDEF FPC}vmovapd [ecx], xmm1;{$ELSE}db $C5,$F9,$29,$09;{$ENDIF} 

   // next columns:
   add ecx, 16;
   add edx, 16;

   dec esi;
   jnz @@endProc;

   @@lastcolumn:

   {$IFDEF FPC}vxorpd xmm1, xmm1, xmm1;{$ELSE}db $C5,$F1,$57,$C9;{$ENDIF} 

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov eax, ebx;
   @addforyloop:
       {$IFDEF FPC}vaddsd xmm1, xmm1, [edx + eax];{$ELSE}db $C5,$F3,$58,$0C,$02;{$ENDIF} 
   add eax, edi;
   jnz @addforyloop;

   // build result
   {$IFDEF FPC}vdivsd xmm1, xmm1, xmm2;{$ELSE}db $C5,$F3,$5E,$CA;{$ENDIF} 
   {$IFDEF FPC}vmovsd [ecx], xmm1;{$ELSE}db $C5,$FB,$11,$09;{$ENDIF} 

   @@endProc:
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   // epilog
   pop esi;
   pop edi;
   pop ebx;
end;
end;

procedure AVXMatrixMeanColumnUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
var tmp : double;
begin
asm
   // prolog
   push ebx;
   push edi;
   push esi;

   // prepare
   xor ebx, ebx;
   sub ebx, height;
   imul ebx, srcLineWidth;

   // helper registers for the mt1, mt2 and dest pointers
   mov ecx, dest;
   mov edx, src;
   sub edx, ebx;

   mov edi, srcLineWidth;

   // fpc seems to have a problem with this opcode
   {$IFDEF FPC}
   mov eax, height;
   {$IFDEF FPC}vcvtsi2sd xmm0, xmm0, eax;{$ELSE}db $C5,$FB,$2A,$C0;{$ENDIF} 
   {$ELSE}
   cvtsi2sd xmm0, height;
   {$ENDIF}
   lea eax, tmp;
   {$IFDEF FPC}vmovsd [eax], xmm0;{$ELSE}db $C5,$FB,$11,$00;{$ENDIF} 
   {$IFDEF FPC}vbroadcastsd ymm2, [eax];{$ELSE}db $C4,$E2,$7D,$19,$10;{$ENDIF} 
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
       mov eax, ebx;
       @addforyloop4:
           {$IFDEF FPC}vmovupd ymm0, [edx + eax];{$ELSE}db $C5,$FD,$10,$04,$02;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm1, ymm1, ymm0;{$ELSE}db $C5,$F5,$58,$C8;{$ENDIF} 
       add eax, edi;
       jnz @addforyloop4;

       // build result
       {$IFDEF FPC}vdivpd ymm1, ymm1, ymm2;{$ELSE}db $C5,$F5,$5E,$CA;{$ENDIF} 
       {$IFDEF FPC}vmovupd [ecx], ymm1;{$ELSE}db $C5,$FD,$11,$09;{$ENDIF} 

       // next columns:
       add ecx, 32;
       add edx, 32;

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
   mov eax, ebx;
   @addforyloop2:
       {$IFDEF FPC}vmovupd xmm0, [edx + eax];{$ELSE}db $C5,$F9,$10,$04,$02;{$ENDIF} 
       {$IFDEF FPC}vaddpd xmm1, xmm1, xmm0;{$ELSE}db $C5,$F1,$58,$C8;{$ENDIF} 
   add eax, edi;
   jnz @addforyloop2;

   // build result
   {$IFDEF FPC}vdivpd xmm1, xmm1, xmm2;{$ELSE}db $C5,$F1,$5E,$CA;{$ENDIF} 
   {$IFDEF FPC}vmovupd [ecx], xmm1;{$ELSE}db $C5,$F9,$11,$09;{$ENDIF} 

   // next columns:
   add ecx, 16;
   add edx, 16;

   dec esi;
   jnz @@endProc;

   @@lastcolumn:

   {$IFDEF FPC}vxorpd xmm1, xmm1, xmm1;{$ELSE}db $C5,$F1,$57,$C9;{$ENDIF} 

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov eax, ebx;
   @addforyloop:
       {$IFDEF FPC}vaddsd xmm1, xmm1, [edx + eax];{$ELSE}db $C5,$F3,$58,$0C,$02;{$ENDIF} 
   add eax, edi;
   jnz @addforyloop;

   // build result
   {$IFDEF FPC}vdivsd xmm1, xmm1, xmm2;{$ELSE}db $C5,$F3,$5E,$CA;{$ENDIF} 
   {$IFDEF FPC}vmovsd [ecx], xmm1;{$ELSE}db $C5,$FB,$11,$09;{$ENDIF} 

   @@endProc:
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   // epilog
   pop esi;
   pop edi;
   pop ebx;
end;
end;

// ###########################################
// #### Variance calculation
// ###########################################


procedure AVXMatrixVarRowAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean);
var tmp : double;
begin
asm
   // prolog
   push ebx;
   push edi;
   push esi;

   // iters := -width*sizeof(double)
   mov edi, width;
   imul edi, -8;

   // helper registers for the src and dest pointers
   mov edx, src;
   sub edx, edi;
   mov ecx, dest;

   // fpc seems to have a problem with this opcode
   {$IFDEF FPC}
   mov eax, width;
   {$IFDEF FPC}vcvtsi2sd xmm3, xmm3, eax;{$ELSE}db $C5,$E3,$2A,$D8;{$ENDIF} 
   {$ELSE}
   cvtsi2sd xmm3, width;
   {$ENDIF}

   // for y := 0 to height - 1:
   mov esi, Height;
   @@addforyloop:
       {$IFDEF FPC}vxorpd ymm0, ymm0, ymm0;{$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} 
       {$IFDEF FPC}vxorpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$57,$C9;{$ENDIF} 

       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov eax, edi;
       @addforxloop:
           add eax, 128;
           jg @loopEnd;
           // prefetch data...
           // prefetch [r8 + eax];

           // addition:
           {$IFDEF FPC}vaddpd ymm0, ymm0, [edx + eax - 128];{$ELSE}db $C5,$FD,$58,$44,$02,$80;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm1, ymm1, [edx + eax - 96];{$ELSE}db $C5,$F5,$58,$4C,$02,$A0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, [edx + eax - 64];{$ELSE}db $C5,$FD,$58,$44,$02,$C0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm1, ymm1, [edx + eax - 32];{$ELSE}db $C5,$F5,$58,$4C,$02,$E0;{$ENDIF} 
       jmp @addforxloop

       @loopEnd:

       {$IFDEF FPC}vaddpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 
       {$IFDEF FPC}vextractf128 xmm2, ymm0, 1;{$ELSE}db $C4,$E3,$7D,$19,$C2,$01;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm2;{$ELSE}db $C5,$F9,$7C,$C2;{$ENDIF} 

       sub eax, 128;

       jz @buildRes;

       @addforxloop2:
           add eax, 16;
           jg @@addforxloop2end;

           {$IFDEF FPC}vaddpd xmm0, xmm0, [edx + eax - 16];{$ELSE}db $C5,$F9,$58,$44,$02,$F0;{$ENDIF} 
       jmp @addforxloop2;

       @@addforxloop2end:

       sub eax, 16;
       jz @buildRes;

       {$IFDEF FPC}vaddsd xmm0, xmm0, [edx + eax];{$ELSE}db $C5,$FB,$58,$04,$02;{$ENDIF} 

       @buildRes:

       // build result
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} 
       {$IFDEF FPC}vdivsd xmm0, xmm0, xmm3;{$ELSE}db $C5,$FB,$5E,$C3;{$ENDIF} 

       // we have calculated the mean -> 
       // repeat the loop to calculate the variance
       lea eax, tmp;
       {$IFDEF FPC}vmovsd [eax], xmm0;{$ELSE}db $C5,$FB,$11,$00;{$ENDIF} 
       {$IFDEF FPC}vbroadcastsd ymm4, [eax];{$ELSE}db $C4,$E2,$7D,$19,$20;{$ENDIF} 
       // vbroadcastsd ymm4, xmm0; //avx2
       {$IFDEF FPC}vxorpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$57,$C0;{$ENDIF} 
            
       // for x := 0 to w - 1;
       // prepare for reverse loop
       // variance = sum (x - mean)^2
       mov eax, edi;
       @addforxloop3:
           add eax, 128;
           jg @loopEnd2;
           // prefetch data...
           // prefetch [ecx + eax];

           // addition:
           {$IFDEF FPC}vmovapd ymm1, [edx + eax - 128];{$ELSE}db $C5,$FD,$28,$4C,$02,$80;{$ENDIF} 
           {$IFDEF FPC}vsubpd ymm1, ymm1, ymm4;{$ELSE}db $C5,$F5,$5C,$CC;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 
                
           {$IFDEF FPC}vmovapd ymm1, [edx + eax - 96];{$ELSE}db $C5,$FD,$28,$4C,$02,$A0;{$ENDIF} 
           {$IFDEF FPC}vsubpd ymm1, ymm1, ymm4;{$ELSE}db $C5,$F5,$5C,$CC;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm1, [edx + eax - 64];{$ELSE}db $C5,$FD,$28,$4C,$02,$C0;{$ENDIF} 
           {$IFDEF FPC}vsubpd ymm1, ymm1, ymm4;{$ELSE}db $C5,$F5,$5C,$CC;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm1, [edx + eax - 32];{$ELSE}db $C5,$FD,$28,$4C,$02,$E0;{$ENDIF} 
           {$IFDEF FPC}vsubpd ymm1, ymm1, ymm4;{$ELSE}db $C5,$F5,$5C,$CC;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 

       jmp @addforxloop3

       @loopEnd2:

       sub eax, 128;

       {$IFDEF FPC}vextractf128 xmm2, ymm0, 1;{$ELSE}db $C4,$E3,$7D,$19,$C2,$01;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm2;{$ELSE}db $C5,$F9,$7C,$C2;{$ENDIF} 

       jz @buildRes2;

       @addforxloop4:
           add eax, 16;
           jg @addforxloop4end;

           {$IFDEF FPC}vmovapd xmm1, [edx + eax - 16];{$ELSE}db $C5,$F9,$28,$4C,$02,$F0;{$ENDIF} 
           {$IFDEF FPC}vsubpd xmm1, xmm1, xmm4;{$ELSE}db $C5,$F1,$5C,$CC;{$ENDIF} 
           {$IFDEF FPC}vmulpd xmm1, xmm1, xmm1;{$ELSE}db $C5,$F1,$59,$C9;{$ENDIF} 
           {$IFDEF FPC}vaddpd xmm0, xmm0, xmm1;{$ELSE}db $C5,$F9,$58,$C1;{$ENDIF} 
       jmp @addforxloop4;

       @addforxloop4end:

       sub eax, 16;
       jz @buildRes2;

       // last column
       {$IFDEF FPC}vmovsd xmm1, [edx + eax];{$ELSE}db $C5,$FB,$10,$0C,$02;{$ENDIF} 
       {$IFDEF FPC}vsubsd xmm1, xmm1, xmm4;{$ELSE}db $C5,$F3,$5C,$CC;{$ENDIF} 
       {$IFDEF FPC}vmulsd xmm1, xmm1, xmm1;{$ELSE}db $C5,$F3,$59,$C9;{$ENDIF} 
       {$IFDEF FPC}vaddsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$58,$C1;{$ENDIF} 

       @buildRes2:

       // build result
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} 
            
       // check if we need to use the unbiased version
       cmp unbiased, 0;
       jz @@dobiased;

       lea eax, cLocOne;
       {$IFDEF FPC}vmovsd xmm2, [eax];{$ELSE}db $C5,$FB,$10,$10;{$ENDIF} 
       {$IFDEF FPC}vsubsd xmm4, xmm3, xmm2;{$ELSE}db $C5,$E3,$5C,$E2;{$ENDIF} 
       {$IFDEF FPC}vmaxsd xmm4, xmm4, xmm2;{$ELSE}db $C5,$DB,$5F,$E2;{$ENDIF} 

       {$IFDEF FPC}vdivsd xmm0, xmm0, xmm4;{$ELSE}db $C5,$FB,$5E,$C4;{$ENDIF} 

       jmp @@writeRes;

       @@dobiased:
       {$IFDEF FPC}vdivsd xmm0, xmm0, xmm3;{$ELSE}db $C5,$FB,$5E,$C3;{$ENDIF} 
            
       // write result
       @@writeRes:
       {$IFDEF FPC}vmovsd [ecx], xmm0;{$ELSE}db $C5,$FB,$11,$01;{$ENDIF} 

       // next line:
       add edx, srcLineWidth;
       add ecx, destLineWidth;

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

procedure AVXMatrixVarRowUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean);
var tmp : double;
begin
asm
   // prolog
   push ebx;
   push edi;
   push esi;

   // iters := -width*sizeof(double)
   mov edi, width;
   imul edi, -8;

   // helper registers for the src and dest pointers
   mov edx, src;
   sub edx, edi;
   mov ecx, dest;

   // fpc seems to have a problem with this opcode
   {$IFDEF FPC}
   mov eax, width;
   {$IFDEF FPC}vcvtsi2sd xmm3, xmm3, eax;{$ELSE}db $C5,$E3,$2A,$D8;{$ENDIF} 
   {$ELSE}
   cvtsi2sd xmm3, width;
   {$ENDIF}

   // for y := 0 to height - 1:
   mov esi, Height;
   @@addforyloop:
       {$IFDEF FPC}vxorpd ymm0, ymm0, ymm0;{$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} 
       {$IFDEF FPC}vxorpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$57,$C9;{$ENDIF} 

       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov eax, edi;
       @addforxloop:
           add eax, 128;
           jg @loopEnd;
           // prefetch data...
           // prefetch [r8 + eax];

           // addition:
           {$IFDEF FPC}vmovupd ymm2, [edx + eax - 128];{$ELSE}db $C5,$FD,$10,$54,$02,$80;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm2;{$ELSE}db $C5,$FD,$58,$C2;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm4, [edx + eax - 96];{$ELSE}db $C5,$FD,$10,$64,$02,$A0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm1, ymm1, ymm4;{$ELSE}db $C5,$F5,$58,$CC;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm2, [edx + eax - 64];{$ELSE}db $C5,$FD,$10,$54,$02,$C0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm2;{$ELSE}db $C5,$FD,$58,$C2;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm4, [edx + eax - 32];{$ELSE}db $C5,$FD,$10,$64,$02,$E0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm1, ymm1, ymm4;{$ELSE}db $C5,$F5,$58,$CC;{$ENDIF} 
       jmp @addforxloop

       @loopEnd:

       {$IFDEF FPC}vaddpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 
       {$IFDEF FPC}vextractf128 xmm2, ymm0, 1;{$ELSE}db $C4,$E3,$7D,$19,$C2,$01;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm2;{$ELSE}db $C5,$F9,$7C,$C2;{$ENDIF} 

       sub eax, 128;

       jz @buildRes;

       @addforxloop2:
           add eax, 16;
           jg @@addforxloop2end;

           {$IFDEF FPC}vmovupd xmm1, [edx + eax - 16];{$ELSE}db $C5,$F9,$10,$4C,$02,$F0;{$ENDIF} 
           {$IFDEF FPC}vaddpd xmm0, xmm0, xmm1;{$ELSE}db $C5,$F9,$58,$C1;{$ENDIF} 
       jmp @addforxloop2;

       @@addforxloop2end:

       sub eax, 16;
       jz @buildRes;

       {$IFDEF FPC}vaddsd xmm0, xmm0, [edx + eax];{$ELSE}db $C5,$FB,$58,$04,$02;{$ENDIF} 

       @buildRes:

       // build result
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} 
       {$IFDEF FPC}vdivsd xmm0, xmm0, xmm3;{$ELSE}db $C5,$FB,$5E,$C3;{$ENDIF} 

       // we have calculated the mean ->
       // repeat the loop to calculate the variance
       lea eax, tmp;
       {$IFDEF FPC}vmovsd [eax], xmm0;{$ELSE}db $C5,$FB,$11,$00;{$ENDIF} 
       {$IFDEF FPC}vbroadcastsd ymm4, [eax];{$ELSE}db $C4,$E2,$7D,$19,$20;{$ENDIF} 
       // vbroadcastsd ymm4, xmm0;  // avx2 instruction
       {$IFDEF FPC}vxorpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$57,$C0;{$ENDIF} 

       // for x := 0 to w - 1;
       // prepare for reverse loop
       // variance = sum (x - mean)^2
       mov eax, edi;
       @addforxloop3:
           add eax, 128;
           jg @loopEnd2;
           // prefetch data...
           // prefetch [ecx + eax];

           // addition:
           {$IFDEF FPC}vmovupd ymm1, [edx + eax - 128];{$ELSE}db $C5,$FD,$10,$4C,$02,$80;{$ENDIF} 
           {$IFDEF FPC}vsubpd ymm1, ymm1, ymm4;{$ELSE}db $C5,$F5,$5C,$CC;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm1, [edx + eax - 96];{$ELSE}db $C5,$FD,$10,$4C,$02,$A0;{$ENDIF} 
           {$IFDEF FPC}vsubpd ymm1, ymm1, ymm4;{$ELSE}db $C5,$F5,$5C,$CC;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm1, [edx + eax - 64];{$ELSE}db $C5,$FD,$10,$4C,$02,$C0;{$ENDIF} 
           {$IFDEF FPC}vsubpd ymm1, ymm1, ymm4;{$ELSE}db $C5,$F5,$5C,$CC;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm1, [edx + eax - 32];{$ELSE}db $C5,$FD,$10,$4C,$02,$E0;{$ENDIF} 
           {$IFDEF FPC}vsubpd ymm1, ymm1, ymm4;{$ELSE}db $C5,$F5,$5C,$CC;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 

       jmp @addforxloop3

       @loopEnd2:

       sub eax, 128;

       {$IFDEF FPC}vextractf128 xmm2, ymm0, 1;{$ELSE}db $C4,$E3,$7D,$19,$C2,$01;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm2;{$ELSE}db $C5,$F9,$7C,$C2;{$ENDIF} 

       jz @buildRes2;

       @addforxloop4:
           add eax, 16;
           jg @addforxloop4end;

           {$IFDEF FPC}vmovupd xmm1, [edx + eax - 16];{$ELSE}db $C5,$F9,$10,$4C,$02,$F0;{$ENDIF} 
           {$IFDEF FPC}vsubpd xmm1, xmm1, xmm4;{$ELSE}db $C5,$F1,$5C,$CC;{$ENDIF} 
           {$IFDEF FPC}vmulpd xmm1, xmm1, xmm1;{$ELSE}db $C5,$F1,$59,$C9;{$ENDIF} 
           {$IFDEF FPC}vaddpd xmm0, xmm0, xmm1;{$ELSE}db $C5,$F9,$58,$C1;{$ENDIF} 
       jmp @addforxloop4;

       @addforxloop4end:

       sub eax, 16;
       jz @buildRes2;

       // last column
       {$IFDEF FPC}vmovsd xmm1, [edx + eax];{$ELSE}db $C5,$FB,$10,$0C,$02;{$ENDIF} 
       {$IFDEF FPC}vsubsd xmm1, xmm1, xmm4;{$ELSE}db $C5,$F3,$5C,$CC;{$ENDIF} 
       {$IFDEF FPC}vmulsd xmm1, xmm1, xmm1;{$ELSE}db $C5,$F3,$59,$C9;{$ENDIF} 
       {$IFDEF FPC}vaddsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$58,$C1;{$ENDIF} 

       @buildRes2:

       // build result
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} 

       // check if we need to use the unbiased version
       cmp unbiased, 0;
       jz @@dobiased;

       lea eax, cLocOne;
       {$IFDEF FPC}vmovsd xmm2, [eax];{$ELSE}db $C5,$FB,$10,$10;{$ENDIF} 
       {$IFDEF FPC}vsubsd xmm4, xmm3, xmm2;{$ELSE}db $C5,$E3,$5C,$E2;{$ENDIF} 
       {$IFDEF FPC}vmaxsd xmm4, xmm4, xmm2;{$ELSE}db $C5,$DB,$5F,$E2;{$ENDIF} 

       {$IFDEF FPC}vdivsd xmm0, xmm0, xmm4;{$ELSE}db $C5,$FB,$5E,$C4;{$ENDIF} 

       jmp @@writeRes;

       @@dobiased:
       {$IFDEF FPC}vdivsd xmm0, xmm0, xmm3;{$ELSE}db $C5,$FB,$5E,$C3;{$ENDIF} 

       // write result
       @@writeRes:
       {$IFDEF FPC}vmovsd [ecx], xmm0;{$ELSE}db $C5,$FB,$11,$01;{$ENDIF} 

       // next line:
       add edx, srcLineWidth;
       add ecx, destLineWidth;

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

procedure AVXMatrixVarColumnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean);
var tmp : double;
begin
asm
   // prolog
   push ebx;
   push edi;
   push esi;

   // preparations
   xor edi, edi;
   sub edi, height;
   imul edi, srcLineWidth;

   // helper registers for the mt1, mt2 and dest pointers
   mov ecx, dest;
   mov edx, src;
   sub edx, edi;

   mov ebx, srcLineWidth;

   // fpc seems to have a problem with this opcode
   {$IFDEF FPC}
   mov eax, height;
   {$IFDEF FPC}vcvtsi2sd xmm0, xmm0, eax;{$ELSE}db $C5,$FB,$2A,$C0;{$ENDIF} 
   {$ELSE}
   cvtsi2sd xmm0, height;
   {$ENDIF}
   lea eax, tmp;
   {$IFDEF FPC}vmovsd [eax], xmm0;{$ELSE}db $C5,$FB,$11,$00;{$ENDIF} 
   {$IFDEF FPC}vbroadcastsd ymm2, [eax];    {$ELSE}db $C4,$E2,$7D,$19,$10;{$ENDIF} // vbroadcastsd ymm2, xmm0; // avx2


   lea eax, cLocOne;
   {$IFDEF FPC}vbroadcastsd ymm5, [eax];{$ELSE}db $C4,$E2,$7D,$19,$28;{$ENDIF} 

   // for x := 0 to width - 1:
   mov esi, Width;
   sub esi, 4;
   jl @@addforxloop4end;

   // 4 columns at once
   @@addforxloop4:
       {$IFDEF FPC}vxorpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$57,$C9;{$ENDIF} 

       // for y := 0 to height - 1;
       // prepare for reverse loop
       mov eax, edi;
       @addforyloop4:
           {$IFDEF FPC}vaddpd ymm1, ymm1, [edx + eax];{$ELSE}db $C5,$F5,$58,$0C,$02;{$ENDIF} 
       add eax, ebx;
       jnz @addforyloop4;

       // build result
       {$IFDEF FPC}vdivpd ymm0, ymm1, ymm2;{$ELSE}db $C5,$F5,$5E,$C2;{$ENDIF} 

       // calculate final standard deviation
       // for y := 0 to height - 1;
       // prepare for reverse loop
       {$IFDEF FPC}vxorpd ymm4, ymm4, ymm4;{$ELSE}db $C5,$DD,$57,$E4;{$ENDIF} 
       mov eax, edi;
       @addforyloop4_2:
           {$IFDEF FPC}vmovapd ymm1, [edx + eax];{$ELSE}db $C5,$FD,$28,$0C,$02;{$ENDIF} 
           {$IFDEF FPC}vsubpd ymm1, ymm1, ymm0;{$ELSE}db $C5,$F5,$5C,$C8;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm4, ymm4, ymm1;{$ELSE}db $C5,$DD,$58,$E1;{$ENDIF} 
       add eax, ebx;
       jnz @addforyloop4_2;

       // check if we need to use the unbiased version
       cmp unbiased, 0;
       jz @@dobiased_4;

       lea eax, cLocOne;

       {$IFDEF FPC}vbroadcastsd ymm1, [eax];{$ELSE}db $C4,$E2,$7D,$19,$08;{$ENDIF} 
       {$IFDEF FPC}vsubpd ymm3, ymm2, ymm1;{$ELSE}db $C5,$ED,$5C,$D9;{$ENDIF} 
       {$IFDEF FPC}vmaxpd ymm3, ymm3, ymm1;{$ELSE}db $C5,$E5,$5F,$D9;{$ENDIF} 

       {$IFDEF FPC}vdivpd ymm4, ymm4, ymm3;{$ELSE}db $C5,$DD,$5E,$E3;{$ENDIF} 

       jmp @@writeRes_4;

       @@dobiased_4:
       {$IFDEF FPC}vdivpd ymm4, ymm4, ymm2;{$ELSE}db $C5,$DD,$5E,$E2;{$ENDIF} 

       // write result
       @@writeRes_4:
       {$IFDEF FPC}vmovapd [ecx], ymm4;{$ELSE}db $C5,$FD,$29,$21;{$ENDIF} 

       // next columns:
       add ecx, 32;
       add edx, 32;

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
   mov eax, edi;
   @addforyloop2:
       {$IFDEF FPC}vaddpd xmm1, xmm1, [edx + eax];{$ELSE}db $C5,$F1,$58,$0C,$02;{$ENDIF} 
   add eax, ebx;
   jnz @addforyloop2;

   // build result
   {$IFDEF FPC}vdivpd xmm0, xmm1, xmm2;{$ELSE}db $C5,$F1,$5E,$C2;{$ENDIF} 

   // calculate final standard deviation
   // for y := 0 to height - 1;
   // prepare for reverse loop
   {$IFDEF FPC}vxorpd xmm4, xmm4, xmm4;{$ELSE}db $C5,$D9,$57,$E4;{$ENDIF} 
   mov eax, edi;
   @addforyloop2_2:
      {$IFDEF FPC}vmovapd xmm1, [edx + eax];{$ELSE}db $C5,$F9,$28,$0C,$02;{$ENDIF} 
      {$IFDEF FPC}vsubpd xmm1, xmm1, xmm0;{$ELSE}db $C5,$F1,$5C,$C8;{$ENDIF} 
      {$IFDEF FPC}vmulpd xmm1, xmm1, xmm1;{$ELSE}db $C5,$F1,$59,$C9;{$ENDIF} 
      {$IFDEF FPC}vaddpd xmm4, xmm4, xmm1;{$ELSE}db $C5,$D9,$58,$E1;{$ENDIF} 
   add eax, ebx;
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
   {$IFDEF FPC}vmovapd [ecx], xmm4;{$ELSE}db $C5,$F9,$29,$21;{$ENDIF} 

   // next columns:
   add ecx, 16;
   add edx, 16;

   dec esi;
   jnz @@endProc;

   @@lastcolumn:

   {$IFDEF FPC}vxorpd xmm1, xmm1, xmm1;{$ELSE}db $C5,$F1,$57,$C9;{$ENDIF} 

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov eax, edi;
   @addforyloop_1:
       {$IFDEF FPC}vaddsd xmm1, xmm1, [edx + eax];{$ELSE}db $C5,$F3,$58,$0C,$02;{$ENDIF} 
   add eax, ebx;
   jnz @addforyloop_1;

   // build result
   {$IFDEF FPC}vdivsd xmm0, xmm1, xmm2;{$ELSE}db $C5,$F3,$5E,$C2;{$ENDIF} 

   // calculate final standard deviation
   // for y := 0 to height - 1;
   // prepare for reverse loop
   {$IFDEF FPC}vxorpd xmm4, xmm4, xmm4;{$ELSE}db $C5,$D9,$57,$E4;{$ENDIF} 
   mov eax, edi;
   @addforyloop1_2:
      {$IFDEF FPC}vmovsd xmm1, [edx + eax];{$ELSE}db $C5,$FB,$10,$0C,$02;{$ENDIF} 
      {$IFDEF FPC}vsubsd xmm1, xmm1, xmm0;{$ELSE}db $C5,$F3,$5C,$C8;{$ENDIF} 
      {$IFDEF FPC}vmulsd xmm1, xmm1, xmm1;{$ELSE}db $C5,$F3,$59,$C9;{$ENDIF} 
      {$IFDEF FPC}vaddsd xmm4, xmm4, xmm1;{$ELSE}db $C5,$DB,$58,$E1;{$ENDIF} 
   add eax, ebx;
   jnz @addforyloop1_2;

   // check if we need to use the unbiased version
   cmp unbiased, 0;
   jz @@dobiased_2;

   {$IFDEF FPC}vmovsd xmm3, xmm3, xmm2;{$ELSE}db $C5,$E3,$10,$DA;{$ENDIF} 
   {$IFDEF FPC}vsubsd xmm3, xmm3, xmm5;{$ELSE}db $C5,$E3,$5C,$DD;{$ENDIF} 
   {$IFDEF FPC}vmaxsd xmm3, xmm3, xmm5;{$ELSE}db $C5,$E3,$5F,$DD;{$ENDIF} 

   {$IFDEF FPC}vdivsd xmm4, xmm4, xmm3;{$ELSE}db $C5,$DB,$5E,$E3;{$ENDIF} 

   jmp @@writeRes_1;

   @@dobiased_1:
   {$IFDEF FPC}vdivsd xmm4, xmm4, xmm2;{$ELSE}db $C5,$DB,$5E,$E2;{$ENDIF} 

   // write result
   @@writeRes_1:
   {$IFDEF FPC}vmovsd [ecx], xmm4;{$ELSE}db $C5,$FB,$11,$21;{$ENDIF} 

   @@endProc:

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;
end;

procedure AVXMatrixVarColumnUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean);
var tmp : double;
begin
asm
   // prolog
   push ebx;
   push edi;
   push esi;


   // preparations
   xor edi, edi;
   sub edi, height;
   imul edi, srcLineWidth;

   // helper registers for the mt1, mt2 and dest pointers
   mov ecx, dest;
   mov edx, src;
   sub edx, edi;

   mov ebx, srcLineWidth;

   // fpc seems to have a problem with this opcode
   {$IFDEF FPC}
   mov eax, height;
   {$IFDEF FPC}vcvtsi2sd xmm0, xmm0, eax;{$ELSE}db $C5,$FB,$2A,$C0;{$ENDIF} 
   {$ELSE}
   cvtsi2sd xmm0, height;
   {$ENDIF}
   lea eax, tmp;
   {$IFDEF FPC}vmovsd [eax], xmm0;{$ELSE}db $C5,$FB,$11,$00;{$ENDIF} 
   {$IFDEF FPC}vbroadcastsd ymm2, [eax]; {$ELSE}db $C4,$E2,$7D,$19,$10;{$ENDIF} // vbroadcastsd ymm2, xmm0; // avx2
   

   lea eax, cLocOne;
   {$IFDEF FPC}vbroadcastsd ymm5, [eax];{$ELSE}db $C4,$E2,$7D,$19,$28;{$ENDIF} 

   // for x := 0 to width - 1:
   mov esi, Width;
   sub esi, 4;
   jl @@addforxloop4end;

   // 4 columns at once
   @@addforxloop4:
       {$IFDEF FPC}vxorpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$57,$C9;{$ENDIF} 

       // for y := 0 to height - 1;
       // prepare for reverse loop
       mov eax, edi;
       @addforyloop4:
           {$IFDEF FPC}vmovupd ymm0, [edx + eax];{$ELSE}db $C5,$FD,$10,$04,$02;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm1, ymm1, ymm0;{$ELSE}db $C5,$F5,$58,$C8;{$ENDIF} 
       add eax, ebx;
       jnz @addforyloop4;

       // build result
       {$IFDEF FPC}vdivpd ymm0, ymm1, ymm2;{$ELSE}db $C5,$F5,$5E,$C2;{$ENDIF} 

       // calculate final standard deviation
       // for y := 0 to height - 1;
       // prepare for reverse loop
       {$IFDEF FPC}vxorpd ymm4, ymm4, ymm4;{$ELSE}db $C5,$DD,$57,$E4;{$ENDIF} 
       mov eax, edi;
       @addforyloop4_2:
           {$IFDEF FPC}vmovupd ymm1, [edx + eax];{$ELSE}db $C5,$FD,$10,$0C,$02;{$ENDIF} 
           {$IFDEF FPC}vsubpd ymm1, ymm1, ymm0;{$ELSE}db $C5,$F5,$5C,$C8;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm4, ymm4, ymm1;{$ELSE}db $C5,$DD,$58,$E1;{$ENDIF} 
       add eax, ebx;
       jnz @addforyloop4_2;

       // check if we need to use the unbiased version
       cmp unbiased, 0;
       jz @@dobiased_4;

       lea eax, cLocOne;
       {$IFDEF FPC}vbroadcastsd ymm1, [eax];{$ELSE}db $C4,$E2,$7D,$19,$08;{$ENDIF} 
       {$IFDEF FPC}vsubpd ymm3, ymm2, ymm1;{$ELSE}db $C5,$ED,$5C,$D9;{$ENDIF} 
       {$IFDEF FPC}vmaxpd ymm3, ymm3, ymm1;{$ELSE}db $C5,$E5,$5F,$D9;{$ENDIF} 

       {$IFDEF FPC}vdivpd ymm4, ymm4, ymm3;{$ELSE}db $C5,$DD,$5E,$E3;{$ENDIF} 

       jmp @@writeRes_4;

       @@dobiased_4:
       {$IFDEF FPC}vdivpd ymm4, ymm4, ymm2;{$ELSE}db $C5,$DD,$5E,$E2;{$ENDIF} 

       // write result
       @@writeRes_4:
       {$IFDEF FPC}vmovupd [ecx], ymm4;{$ELSE}db $C5,$FD,$11,$21;{$ENDIF} 

       // next columns:
       add ecx, 32;
       add edx, 32;

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
   mov eax, edi;
   @addforyloop2:
       {$IFDEF FPC}vmovupd xmm0, [edx + eax];{$ELSE}db $C5,$F9,$10,$04,$02;{$ENDIF} 
       {$IFDEF FPC}vaddpd xmm1, xmm1, xmm0;{$ELSE}db $C5,$F1,$58,$C8;{$ENDIF} 
   add eax, ebx;
   jnz @addforyloop2;

   // build result
   {$IFDEF FPC}vdivpd xmm0, xmm1, xmm2;{$ELSE}db $C5,$F1,$5E,$C2;{$ENDIF} 

   // calculate final standard deviation
   // for y := 0 to height - 1;
   // prepare for reverse loop
   {$IFDEF FPC}vxorpd xmm4, xmm4, xmm4;{$ELSE}db $C5,$D9,$57,$E4;{$ENDIF} 
   mov eax, edi;
   @addforyloop2_2:
      {$IFDEF FPC}vmovupd xmm1, [edx + eax];{$ELSE}db $C5,$F9,$10,$0C,$02;{$ENDIF} 
      {$IFDEF FPC}vsubpd xmm1, xmm1, xmm0;{$ELSE}db $C5,$F1,$5C,$C8;{$ENDIF} 
      {$IFDEF FPC}vmulpd xmm1, xmm1, xmm1;{$ELSE}db $C5,$F1,$59,$C9;{$ENDIF} 
      {$IFDEF FPC}vaddpd xmm4, xmm4, xmm1;{$ELSE}db $C5,$D9,$58,$E1;{$ENDIF} 
   add eax, ebx;
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
   {$IFDEF FPC}vmovupd [ecx], xmm4;{$ELSE}db $C5,$F9,$11,$21;{$ENDIF} 

   // next columns:
   add ecx, 16;
   add edx, 16;

   dec esi;
   jnz @@endProc;

   @@lastcolumn:

   {$IFDEF FPC}vxorpd xmm1, xmm1, xmm1;{$ELSE}db $C5,$F1,$57,$C9;{$ENDIF} 

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov eax, edi;
   @addforyloop_1:
       {$IFDEF FPC}vaddsd xmm1, xmm1, [edx + eax];{$ELSE}db $C5,$F3,$58,$0C,$02;{$ENDIF} 
   add eax, ebx;
   jnz @addforyloop_1;

   // build result
   {$IFDEF FPC}vdivsd xmm0, xmm1, xmm2;{$ELSE}db $C5,$F3,$5E,$C2;{$ENDIF} 

   // calculate final standard deviation
   // for y := 0 to height - 1;
   // prepare for reverse loop
   {$IFDEF FPC}vxorpd xmm4, xmm4, xmm4;{$ELSE}db $C5,$D9,$57,$E4;{$ENDIF} 
   mov eax, edi;
   @addforyloop1_2:
      {$IFDEF FPC}vmovsd xmm1, [edx + eax];{$ELSE}db $C5,$FB,$10,$0C,$02;{$ENDIF} 
      {$IFDEF FPC}vsubsd xmm1, xmm1, xmm0;{$ELSE}db $C5,$F3,$5C,$C8;{$ENDIF} 
      {$IFDEF FPC}vmulsd xmm1, xmm1, xmm1;{$ELSE}db $C5,$F3,$59,$C9;{$ENDIF} 
      {$IFDEF FPC}vaddsd xmm4, xmm4, xmm1;{$ELSE}db $C5,$DB,$58,$E1;{$ENDIF} 
   add eax, ebx;
   jnz @addforyloop1_2;

   // check if we need to use the unbiased version
   cmp unbiased, 0;
   jz @@dobiased_2;

   {$IFDEF FPC}vmovsd xmm3, xmm3, xmm2;{$ELSE}db $C5,$E3,$10,$DA;{$ENDIF} 
   {$IFDEF FPC}vsubsd xmm3, xmm3, xmm5;{$ELSE}db $C5,$E3,$5C,$DD;{$ENDIF} 
   {$IFDEF FPC}vmaxsd xmm3, xmm3, xmm5;{$ELSE}db $C5,$E3,$5F,$DD;{$ENDIF} 

   {$IFDEF FPC}vdivsd xmm4, xmm4, xmm3;{$ELSE}db $C5,$DB,$5E,$E3;{$ENDIF} 

   jmp @@writeRes_1;

   @@dobiased_1:
   {$IFDEF FPC}vdivsd xmm4, xmm4, xmm2;{$ELSE}db $C5,$DB,$5E,$E2;{$ENDIF} 

   // write result
   @@writeRes_1:
   {$IFDEF FPC}vmovsd [ecx], xmm4;{$ELSE}db $C5,$FB,$11,$21;{$ENDIF} 

   @@endProc:

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;
end;

// #####################################################
// #### Combined mean variance calculation
// #####################################################

procedure AVXMatrixMeanVarRowAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean);
var tmp : double;
begin
asm
   // prolog
   push ebx;
   push edi;
   push esi;

   // iters := -width*sizeof(double)
   mov edi, width;
   imul edi, -8;

   // helper registers for the src and dest pointers
   mov edx, src;
   sub edx, edi;
   mov ecx, dest;

   // fpc seems to have a problem with this opcode
   {$IFDEF FPC}
   mov eax, width;
   {$IFDEF FPC}vcvtsi2sd xmm3, xmm3, eax;{$ELSE}db $C5,$E3,$2A,$D8;{$ENDIF} 
   {$ELSE}
   cvtsi2sd xmm3, width;
   {$ENDIF}

   // for y := 0 to height - 1:
   mov esi, Height;
   @@addforyloop:
       {$IFDEF FPC}vxorpd ymm0, ymm0, ymm0;{$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} 
       {$IFDEF FPC}vxorpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$57,$C9;{$ENDIF} 

       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov eax, edi;
       @addforxloop:
           add eax, 128;
           jg @loopEnd;
           // prefetch data...
           // prefetch [r8 + eax];

           // addition:
           {$IFDEF FPC}vaddpd ymm0, ymm0, [edx + eax - 128];{$ELSE}db $C5,$FD,$58,$44,$02,$80;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm1, ymm1, [edx + eax - 96];{$ELSE}db $C5,$F5,$58,$4C,$02,$A0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, [edx + eax - 64];{$ELSE}db $C5,$FD,$58,$44,$02,$C0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm1, ymm1, [edx + eax - 32];{$ELSE}db $C5,$F5,$58,$4C,$02,$E0;{$ENDIF} 
       jmp @addforxloop

       @loopEnd:

       {$IFDEF FPC}vaddpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 
       {$IFDEF FPC}vextractf128 xmm2, ymm0, 1;{$ELSE}db $C4,$E3,$7D,$19,$C2,$01;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm2;{$ELSE}db $C5,$F9,$7C,$C2;{$ENDIF} 

       sub eax, 128;

       jz @buildRes;

       @addforxloop2:
           add eax, 16;
           jg @@addforxloop2end;

           {$IFDEF FPC}vaddpd xmm0, xmm0, [edx + eax - 16];{$ELSE}db $C5,$F9,$58,$44,$02,$F0;{$ENDIF} 
       jmp @addforxloop2;

       @@addforxloop2end:

       sub eax, 16;
       jz @buildRes;

       {$IFDEF FPC}vaddsd xmm0, xmm0, [edx + eax];{$ELSE}db $C5,$FB,$58,$04,$02;{$ENDIF} 

       @buildRes:

       // build result
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} 
       {$IFDEF FPC}vdivsd xmm0, xmm0, xmm3;{$ELSE}db $C5,$FB,$5E,$C3;{$ENDIF} 
       {$IFDEF FPC}vmovsd [ecx], xmm0;{$ELSE}db $C5,$FB,$11,$01;{$ENDIF} 

       // we have calculated the mean ->
       // repeat the loop to calculate the variance

       lea eax, tmp;
       {$IFDEF FPC}vmovsd [eax], xmm0;{$ELSE}db $C5,$FB,$11,$00;{$ENDIF} 
       {$IFDEF FPC}vbroadcastsd ymm4, [eax];{$ELSE}db $C4,$E2,$7D,$19,$20;{$ENDIF} 
       // vbroadcastsd ymm4, xmm0; // avx2 instruction
       {$IFDEF FPC}vxorpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$57,$C0;{$ENDIF} 

       // for x := 0 to w - 1;
       // prepare for reverse loop
       // variance = sum (x - mean)^2
       mov eax, edi;
       @addforxloop3:
           add eax, 128;
           jg @loopEnd2;
           // prefetch data...
           // prefetch [ecx + eax];

           // addition:
           {$IFDEF FPC}vmovapd ymm1, [edx + eax - 128];{$ELSE}db $C5,$FD,$28,$4C,$02,$80;{$ENDIF} 
           {$IFDEF FPC}vsubpd ymm1, ymm1, ymm4;{$ELSE}db $C5,$F5,$5C,$CC;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm1, [edx + eax - 96];{$ELSE}db $C5,$FD,$28,$4C,$02,$A0;{$ENDIF} 
           {$IFDEF FPC}vsubpd ymm1, ymm1, ymm4;{$ELSE}db $C5,$F5,$5C,$CC;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm1, [edx + eax - 64];{$ELSE}db $C5,$FD,$28,$4C,$02,$C0;{$ENDIF} 
           {$IFDEF FPC}vsubpd ymm1, ymm1, ymm4;{$ELSE}db $C5,$F5,$5C,$CC;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm1, [edx + eax - 32];{$ELSE}db $C5,$FD,$28,$4C,$02,$E0;{$ENDIF} 
           {$IFDEF FPC}vsubpd ymm1, ymm1, ymm4;{$ELSE}db $C5,$F5,$5C,$CC;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 

       jmp @addforxloop3

       @loopEnd2:

       sub eax, 128;

       {$IFDEF FPC}vextractf128 xmm2, ymm0, 1;{$ELSE}db $C4,$E3,$7D,$19,$C2,$01;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm2;{$ELSE}db $C5,$F9,$7C,$C2;{$ENDIF} 

       jz @buildRes2;

       @addforxloop4:
           add eax, 16;
           jg @addforxloop4end;

           {$IFDEF FPC}vmovapd xmm1, [edx + eax - 16];{$ELSE}db $C5,$F9,$28,$4C,$02,$F0;{$ENDIF} 
           {$IFDEF FPC}vsubpd xmm1, xmm1, xmm4;{$ELSE}db $C5,$F1,$5C,$CC;{$ENDIF} 
           {$IFDEF FPC}vmulpd xmm1, xmm1, xmm1;{$ELSE}db $C5,$F1,$59,$C9;{$ENDIF} 
           {$IFDEF FPC}vaddpd xmm0, xmm0, xmm1;{$ELSE}db $C5,$F9,$58,$C1;{$ENDIF} 
       jmp @addforxloop4;

       @addforxloop4end:

       sub eax, 16;
       jz @buildRes2;

       // last column
       {$IFDEF FPC}vmovsd xmm1, [edx + eax];{$ELSE}db $C5,$FB,$10,$0C,$02;{$ENDIF} 
       {$IFDEF FPC}vsubsd xmm1, xmm1, xmm4;{$ELSE}db $C5,$F3,$5C,$CC;{$ENDIF} 
       {$IFDEF FPC}vmulsd xmm1, xmm1, xmm1;{$ELSE}db $C5,$F3,$59,$C9;{$ENDIF} 
       {$IFDEF FPC}vaddsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$58,$C1;{$ENDIF} 

       @buildRes2:

       // build result
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} 

       // check if we need to use the unbiased version
       cmp unbiased, 0;
       jz @@dobiased;

       lea eax, cLocOne;
       {$IFDEF FPC}vmovsd xmm2, [eax];{$ELSE}db $C5,$FB,$10,$10;{$ENDIF} 
       {$IFDEF FPC}vsubsd xmm4, xmm3, xmm2;{$ELSE}db $C5,$E3,$5C,$E2;{$ENDIF} 
       {$IFDEF FPC}vmaxsd xmm4, xmm4, xmm2;{$ELSE}db $C5,$DB,$5F,$E2;{$ENDIF} 

       {$IFDEF FPC}vdivsd xmm0, xmm0, xmm4;{$ELSE}db $C5,$FB,$5E,$C4;{$ENDIF} 

       jmp @@writeRes;

       @@dobiased:
       {$IFDEF FPC}vdivsd xmm0, xmm0, xmm3;{$ELSE}db $C5,$FB,$5E,$C3;{$ENDIF} 

       // write result
       @@writeRes:
       {$IFDEF FPC}vmovsd [ecx + 8], xmm0;{$ELSE}db $C5,$FB,$11,$41,$08;{$ENDIF} 

       // next line:
       add edx, srcLineWidth;
       add ecx, destLineWidth;

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

procedure AVXMatrixMeanVarRowUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean);
var tmp : double;
begin
asm
   // prolog
   push ebx;
   push edi;
   push esi;

   // iters := -width*sizeof(double)
   mov edi, width;
   imul edi, -8;

   // helper registers for the src and dest pointers
   mov edx, src;
   sub edx, edi;
   mov ecx, dest;

   // fpc seems to have a problem with this opcode
   {$IFDEF FPC}
   mov eax, width;
   {$IFDEF FPC}vcvtsi2sd xmm3, xmm3, eax;{$ELSE}db $C5,$E3,$2A,$D8;{$ENDIF} 
   {$ELSE}
   cvtsi2sd xmm3, width;
   {$ENDIF}

   // for y := 0 to height - 1:
   mov esi, Height;
   @@addforyloop:
       {$IFDEF FPC}vxorpd ymm0, ymm0, ymm0;{$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} 
       {$IFDEF FPC}vxorpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$57,$C9;{$ENDIF} 

       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov eax, edi;
       @addforxloop:
           add eax, 128;
           jg @loopEnd;
           // prefetch data...
           // prefetch [r8 + eax];

           // addition:
           {$IFDEF FPC}vmovupd ymm2, [edx + eax - 128];{$ELSE}db $C5,$FD,$10,$54,$02,$80;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm2;{$ELSE}db $C5,$FD,$58,$C2;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm4, [edx + eax - 96];{$ELSE}db $C5,$FD,$10,$64,$02,$A0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm1, ymm1, ymm4;{$ELSE}db $C5,$F5,$58,$CC;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm2, [edx + eax - 64];{$ELSE}db $C5,$FD,$10,$54,$02,$C0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm2;{$ELSE}db $C5,$FD,$58,$C2;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm4, [edx + eax - 32];{$ELSE}db $C5,$FD,$10,$64,$02,$E0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm1, ymm1, ymm4;{$ELSE}db $C5,$F5,$58,$CC;{$ENDIF} 
       jmp @addforxloop

       @loopEnd:

       {$IFDEF FPC}vaddpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 
       {$IFDEF FPC}vextractf128 xmm2, ymm0, 1;{$ELSE}db $C4,$E3,$7D,$19,$C2,$01;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm2;{$ELSE}db $C5,$F9,$7C,$C2;{$ENDIF} 

       sub eax, 128;

       jz @buildRes;

       @addforxloop2:
           add eax, 16;
           jg @@addforxloop2end;

           {$IFDEF FPC}vmovupd xmm1, [edx + eax - 16];{$ELSE}db $C5,$F9,$10,$4C,$02,$F0;{$ENDIF} 
           {$IFDEF FPC}vaddpd xmm0, xmm0, xmm1;{$ELSE}db $C5,$F9,$58,$C1;{$ENDIF} 
       jmp @addforxloop2;

       @@addforxloop2end:

       sub eax, 16;
       jz @buildRes;

       {$IFDEF FPC}vaddsd xmm0, xmm0, [edx + eax];{$ELSE}db $C5,$FB,$58,$04,$02;{$ENDIF} 

       @buildRes:

       // build result
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} 
       {$IFDEF FPC}vdivsd xmm0, xmm0, xmm3;{$ELSE}db $C5,$FB,$5E,$C3;{$ENDIF} 
       {$IFDEF FPC}vmovsd [ecx], xmm0;{$ELSE}db $C5,$FB,$11,$01;{$ENDIF} 

       // we have calculated the mean ->
       // repeat the loop to calculate the variance
       lea eax, tmp;
       {$IFDEF FPC}vmovsd [eax], xmm0;{$ELSE}db $C5,$FB,$11,$00;{$ENDIF} 
       {$IFDEF FPC}vbroadcastsd ymm4, [eax];{$ELSE}db $C4,$E2,$7D,$19,$20;{$ENDIF} 
       // vbroadcastsd ymm4, xmm0; // avx2
       {$IFDEF FPC}vxorpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$57,$C0;{$ENDIF} 

       // for x := 0 to w - 1;
       // prepare for reverse loop
       // variance = sum (x - mean)^2
       mov eax, edi;
       @addforxloop3:
           add eax, 128;
           jg @loopEnd2;
           // prefetch data...
           // prefetch [ecx + eax];

           // addition:
           {$IFDEF FPC}vmovupd ymm1, [edx + eax - 128];{$ELSE}db $C5,$FD,$10,$4C,$02,$80;{$ENDIF} 
           {$IFDEF FPC}vsubpd ymm1, ymm1, ymm4;{$ELSE}db $C5,$F5,$5C,$CC;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm1, [edx + eax - 96];{$ELSE}db $C5,$FD,$10,$4C,$02,$A0;{$ENDIF} 
           {$IFDEF FPC}vsubpd ymm1, ymm1, ymm4;{$ELSE}db $C5,$F5,$5C,$CC;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm1, [edx + eax - 64];{$ELSE}db $C5,$FD,$10,$4C,$02,$C0;{$ENDIF} 
           {$IFDEF FPC}vsubpd ymm1, ymm1, ymm4;{$ELSE}db $C5,$F5,$5C,$CC;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm1, [edx + eax - 32];{$ELSE}db $C5,$FD,$10,$4C,$02,$E0;{$ENDIF} 
           {$IFDEF FPC}vsubpd ymm1, ymm1, ymm4;{$ELSE}db $C5,$F5,$5C,$CC;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 

       jmp @addforxloop3

       @loopEnd2:

       sub eax, 128;

       {$IFDEF FPC}vextractf128 xmm2, ymm0, 1;{$ELSE}db $C4,$E3,$7D,$19,$C2,$01;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm2;{$ELSE}db $C5,$F9,$7C,$C2;{$ENDIF} 

       jz @buildRes2;

       @addforxloop4:
           add eax, 16;
           jg @addforxloop4end;

           {$IFDEF FPC}vmovupd xmm1, [edx + eax - 16];{$ELSE}db $C5,$F9,$10,$4C,$02,$F0;{$ENDIF} 
           {$IFDEF FPC}vsubpd xmm1, xmm1, xmm4;{$ELSE}db $C5,$F1,$5C,$CC;{$ENDIF} 
           {$IFDEF FPC}vmulpd xmm1, xmm1, xmm1;{$ELSE}db $C5,$F1,$59,$C9;{$ENDIF} 
           {$IFDEF FPC}vaddpd xmm0, xmm0, xmm1;{$ELSE}db $C5,$F9,$58,$C1;{$ENDIF} 
       jmp @addforxloop4;

       @addforxloop4end:

       sub eax, 16;
       jz @buildRes2;

       // last column
       {$IFDEF FPC}vmovsd xmm1, [edx + eax];{$ELSE}db $C5,$FB,$10,$0C,$02;{$ENDIF} 
       {$IFDEF FPC}vsubsd xmm1, xmm1, xmm4;{$ELSE}db $C5,$F3,$5C,$CC;{$ENDIF} 
       {$IFDEF FPC}vmulsd xmm1, xmm1, xmm1;{$ELSE}db $C5,$F3,$59,$C9;{$ENDIF} 
       {$IFDEF FPC}vaddsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$58,$C1;{$ENDIF} 

       @buildRes2:

       // build result
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} 

       // check if we need to use the unbiased version
       cmp unbiased, 0;
       jz @@dobiased;

       lea eax, cLocOne;
       {$IFDEF FPC}vmovsd xmm2, [eax];{$ELSE}db $C5,$FB,$10,$10;{$ENDIF} 
       {$IFDEF FPC}vsubsd xmm4, xmm3, xmm2;{$ELSE}db $C5,$E3,$5C,$E2;{$ENDIF} 
       {$IFDEF FPC}vmaxsd xmm4, xmm4, xmm2;{$ELSE}db $C5,$DB,$5F,$E2;{$ENDIF} 

       {$IFDEF FPC}vdivsd xmm0, xmm0, xmm4;{$ELSE}db $C5,$FB,$5E,$C4;{$ENDIF} 

       jmp @@writeRes;

       @@dobiased:
       {$IFDEF FPC}vdivsd xmm0, xmm0, xmm3;{$ELSE}db $C5,$FB,$5E,$C3;{$ENDIF} 

       // write result
       @@writeRes:
       {$IFDEF FPC}vmovsd [ecx + 8], xmm0;{$ELSE}db $C5,$FB,$11,$41,$08;{$ENDIF} 

       // next line:
       add edx, srcLineWidth;
       add ecx, destLineWidth;

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

procedure AVXMatrixMeanVarColumnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean);
var tmp : double;
begin
asm
   // prolog
   push ebx;
   push edi;
   push esi;

   // Preparations
   xor edi, edi;
   sub edi, height;
   imul edi, srcLineWidth;

   // helper registers for the mt1, mt2 and dest pointers
   mov ecx, dest;
   mov edx, src;
   sub edx, edi;

   mov ebx, srcLineWidth;

   // fpc seems to have a problem with this opcode
   mov eax, height;
   {$IFDEF FPC}vcvtsi2sd xmm0, xmm0, eax;{$ELSE}db $C5,$FB,$2A,$C0;{$ENDIF} 

   lea eax, tmp;
   {$IFDEF FPC}vmovsd [eax], xmm0;{$ELSE}db $C5,$FB,$11,$00;{$ENDIF} 
   {$IFDEF FPC}vbroadcastsd ymm2, [eax]; {$ELSE}db $C4,$E2,$7D,$19,$10;{$ENDIF} // vbroadcastsd ymm2, xmm0; // avx2
   

   lea eax, cLocOne;
   {$IFDEF FPC}vbroadcastsd ymm5, [eax];{$ELSE}db $C4,$E2,$7D,$19,$28;{$ENDIF} 

   // for x := 0 to width - 1:
   mov esi, Width;
   sub esi, 4;
   jl @@addforxloop4end;

   // 4 columns at once
   @@addforxloop4:
       {$IFDEF FPC}vxorpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$57,$C9;{$ENDIF} 

       // for y := 0 to height - 1;
       // prepare for reverse loop
       mov eax, edi;
       @addforyloop4:
           {$IFDEF FPC}vaddpd ymm1, ymm1, [edx + eax];{$ELSE}db $C5,$F5,$58,$0C,$02;{$ENDIF} 
       add eax, ebx;
       jnz @addforyloop4;

       // build result
       {$IFDEF FPC}vdivpd ymm0, ymm1, ymm2;{$ELSE}db $C5,$F5,$5E,$C2;{$ENDIF} 
       {$IFDEF FPC}vmovapd [ecx], ymm0;{$ELSE}db $C5,$FD,$29,$01;{$ENDIF} 

       // calculate final standard deviation
       // for y := 0 to height - 1;
       // prepare for reverse loop
       {$IFDEF FPC}vxorpd ymm4, ymm4, ymm4;{$ELSE}db $C5,$DD,$57,$E4;{$ENDIF} 
       mov eax, edi;
       @addforyloop4_2:
           {$IFDEF FPC}vmovapd ymm1, [edx + eax];{$ELSE}db $C5,$FD,$28,$0C,$02;{$ENDIF} 
           {$IFDEF FPC}vsubpd ymm1, ymm1, ymm0;{$ELSE}db $C5,$F5,$5C,$C8;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm4, ymm4, ymm1;{$ELSE}db $C5,$DD,$58,$E1;{$ENDIF} 
       add eax, ebx;
       jnz @addforyloop4_2;

       // check if we need to use the unbiased version
       cmp unbiased, 0;
       jz @@dobiased_4;

       lea eax, cLocOne;

       {$IFDEF FPC}vbroadcastsd ymm1, [eax];{$ELSE}db $C4,$E2,$7D,$19,$08;{$ENDIF} 
       {$IFDEF FPC}vsubpd ymm3, ymm2, ymm1;{$ELSE}db $C5,$ED,$5C,$D9;{$ENDIF} 
       {$IFDEF FPC}vmaxpd ymm3, ymm3, ymm1;{$ELSE}db $C5,$E5,$5F,$D9;{$ENDIF} 

       {$IFDEF FPC}vdivpd ymm4, ymm4, ymm3;{$ELSE}db $C5,$DD,$5E,$E3;{$ENDIF} 

       jmp @@writeRes_4;

       @@dobiased_4:
       {$IFDEF FPC}vdivpd ymm4, ymm4, ymm2;{$ELSE}db $C5,$DD,$5E,$E2;{$ENDIF} 

       // write result
       @@writeRes_4:
       mov eax, destLineWidth;
       {$IFDEF FPC}vmovapd [ecx + eax], ymm4;{$ELSE}db $C5,$FD,$29,$24,$01;{$ENDIF} 

       // next columns:
       add ecx, 32;
       add edx, 32;

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
   mov eax, edi;
   @addforyloop2:
       {$IFDEF FPC}vaddpd xmm1, xmm1, [edx + eax];{$ELSE}db $C5,$F1,$58,$0C,$02;{$ENDIF} 
   add eax, ebx;
   jnz @addforyloop2;

   // build result
   {$IFDEF FPC}vdivpd xmm0, xmm1, xmm2;{$ELSE}db $C5,$F1,$5E,$C2;{$ENDIF} 
   {$IFDEF FPC}vmovapd [ecx], xmm0;{$ELSE}db $C5,$F9,$29,$01;{$ENDIF} 

   // calculate final standard deviation
   // for y := 0 to height - 1;
   // prepare for reverse loop
   {$IFDEF FPC}vxorpd xmm4, xmm4, xmm4;{$ELSE}db $C5,$D9,$57,$E4;{$ENDIF} 
   mov eax, edi;
   @addforyloop2_2:
      {$IFDEF FPC}vmovapd xmm1, [edx + eax];{$ELSE}db $C5,$F9,$28,$0C,$02;{$ENDIF} 
      {$IFDEF FPC}vsubpd xmm1, xmm1, xmm0;{$ELSE}db $C5,$F1,$5C,$C8;{$ENDIF} 
      {$IFDEF FPC}vmulpd xmm1, xmm1, xmm1;{$ELSE}db $C5,$F1,$59,$C9;{$ENDIF} 
      {$IFDEF FPC}vaddpd xmm4, xmm4, xmm1;{$ELSE}db $C5,$D9,$58,$E1;{$ENDIF} 
   add eax, ebx;
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
   mov eax, destLineWidth;
   {$IFDEF FPC}vmovapd [ecx + eax], xmm4;{$ELSE}db $C5,$F9,$29,$24,$01;{$ENDIF} 

   // next columns:
   add ecx, 16;
   add edx, 16;

   dec esi;
   jnz @@endProc;

   @@lastcolumn:

   {$IFDEF FPC}vxorpd xmm1, xmm1, xmm1;{$ELSE}db $C5,$F1,$57,$C9;{$ENDIF} 

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov eax, edi;
   @addforyloop_1:
       {$IFDEF FPC}vaddsd xmm1, xmm1, [edx + eax];{$ELSE}db $C5,$F3,$58,$0C,$02;{$ENDIF} 
   add eax, ebx;
   jnz @addforyloop_1;

   // build result
   {$IFDEF FPC}vdivsd xmm0, xmm1, xmm2;{$ELSE}db $C5,$F3,$5E,$C2;{$ENDIF} 
   {$IFDEF FPC}vmovsd [ecx], xmm0;{$ELSE}db $C5,$FB,$11,$01;{$ENDIF} 

   // calculate final standard deviation
   // for y := 0 to height - 1;
   // prepare for reverse loop
   {$IFDEF FPC}vxorpd xmm4, xmm4, xmm4;{$ELSE}db $C5,$D9,$57,$E4;{$ENDIF} 
   mov eax, edi;
   @addforyloop1_2:
      {$IFDEF FPC}vmovsd xmm1, [edx + eax];{$ELSE}db $C5,$FB,$10,$0C,$02;{$ENDIF} 
      {$IFDEF FPC}vsubsd xmm1, xmm1, xmm0;{$ELSE}db $C5,$F3,$5C,$C8;{$ENDIF} 
      {$IFDEF FPC}vmulsd xmm1, xmm1, xmm1;{$ELSE}db $C5,$F3,$59,$C9;{$ENDIF} 
      {$IFDEF FPC}vaddsd xmm4, xmm4, xmm1;{$ELSE}db $C5,$DB,$58,$E1;{$ENDIF} 
   add eax, ebx;
   jnz @addforyloop1_2;

   // check if we need to use the unbiased version
   cmp unbiased, 0;
   jz @@dobiased_2;

   {$IFDEF FPC}vmovsd xmm3, xmm3, xmm2;{$ELSE}db $C5,$E3,$10,$DA;{$ENDIF} 
   {$IFDEF FPC}vsubsd xmm3, xmm3, xmm5;{$ELSE}db $C5,$E3,$5C,$DD;{$ENDIF} 
   {$IFDEF FPC}vmaxsd xmm3, xmm3, xmm5;{$ELSE}db $C5,$E3,$5F,$DD;{$ENDIF} 

   {$IFDEF FPC}vdivsd xmm4, xmm4, xmm3;{$ELSE}db $C5,$DB,$5E,$E3;{$ENDIF} 

   jmp @@writeRes_1;

   @@dobiased_1:
   {$IFDEF FPC}vdivsd xmm4, xmm4, xmm2;{$ELSE}db $C5,$DB,$5E,$E2;{$ENDIF} 

   // write result
   @@writeRes_1:
   mov eax, destLineWidth;
   {$IFDEF FPC}vmovsd [ecx + eax], xmm4;{$ELSE}db $C5,$FB,$11,$24,$01;{$ENDIF} 

   @@endProc:

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;
end;

procedure AVXMatrixMeanVarColumnUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean);
var tmp : double;
begin
asm
   // prolog
   push ebx;
   push edi;
   push esi;

   // preparations
   xor edi, edi;
   sub edi, height;
   imul edi, srcLineWidth;

   // helper registers for the mt1, mt2 and dest pointers
   mov ecx, dest;
   mov edx, src;
   sub edx, edi;

   mov ebx, srcLineWidth;

   // fpc seems to have a problem with this opcode
   mov eax, height;
   {$IFDEF FPC}vcvtsi2sd xmm0, xmm0, eax;{$ELSE}db $C5,$FB,$2A,$C0;{$ENDIF} 

   lea eax, tmp;
   {$IFDEF FPC}vmovsd [eax], xmm0;{$ELSE}db $C5,$FB,$11,$00;{$ENDIF} 
   {$IFDEF FPC}vbroadcastsd ymm2, [eax]; {$ELSE}db $C4,$E2,$7D,$19,$10;{$ENDIF} // vbroadcastsd ymm2, xmm0; // avx2
   

   lea eax, cLocOne;
   {$IFDEF FPC}vbroadcastsd ymm5, [eax];{$ELSE}db $C4,$E2,$7D,$19,$28;{$ENDIF} 

   // for x := 0 to width - 1:
   mov esi, Width;
   sub esi, 4;
   jl @@addforxloop4end;

   // 4 columns at once
   @@addforxloop4:
       {$IFDEF FPC}vxorpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$57,$C9;{$ENDIF} 

       // for y := 0 to height - 1;
       // prepare for reverse loop
       mov eax, edi;
       @addforyloop4:
           {$IFDEF FPC}vmovupd ymm0, [edx + eax];{$ELSE}db $C5,$FD,$10,$04,$02;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm1, ymm1, ymm0;{$ELSE}db $C5,$F5,$58,$C8;{$ENDIF} 
       add eax, ebx;
       jnz @addforyloop4;

       // build result
       {$IFDEF FPC}vdivpd ymm0, ymm1, ymm2;{$ELSE}db $C5,$F5,$5E,$C2;{$ENDIF} 
       {$IFDEF FPC}vmovupd [ecx], ymm0;{$ELSE}db $C5,$FD,$11,$01;{$ENDIF} 

       // calculate final standard deviation
       // for y := 0 to height - 1;
       // prepare for reverse loop
       {$IFDEF FPC}vxorpd ymm4, ymm4, ymm4;{$ELSE}db $C5,$DD,$57,$E4;{$ENDIF} 
       mov eax, edi;
       @addforyloop4_2:
           {$IFDEF FPC}vmovupd ymm1, [edx + eax];{$ELSE}db $C5,$FD,$10,$0C,$02;{$ENDIF} 
           {$IFDEF FPC}vsubpd ymm1, ymm1, ymm0;{$ELSE}db $C5,$F5,$5C,$C8;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm4, ymm4, ymm1;{$ELSE}db $C5,$DD,$58,$E1;{$ENDIF} 
       add eax, ebx;
       jnz @addforyloop4_2;

       // check if we need to use the unbiased version
       cmp unbiased, 0;
       jz @@dobiased_4;

       lea eax, cLocOne;
       {$IFDEF FPC}vbroadcastsd ymm1, [eax];{$ELSE}db $C4,$E2,$7D,$19,$08;{$ENDIF} 
       {$IFDEF FPC}vsubpd ymm3, ymm2, ymm1;{$ELSE}db $C5,$ED,$5C,$D9;{$ENDIF} 
       {$IFDEF FPC}vmaxpd ymm3, ymm3, ymm1;{$ELSE}db $C5,$E5,$5F,$D9;{$ENDIF} 

       {$IFDEF FPC}vdivpd ymm4, ymm4, ymm3;{$ELSE}db $C5,$DD,$5E,$E3;{$ENDIF} 

       jmp @@writeRes_4;

       @@dobiased_4:
       {$IFDEF FPC}vdivpd ymm4, ymm4, ymm2;{$ELSE}db $C5,$DD,$5E,$E2;{$ENDIF} 

       // write result
       @@writeRes_4:
       mov eax, destLineWidth;
       {$IFDEF FPC}vmovupd [ecx + eax], ymm4;{$ELSE}db $C5,$FD,$11,$24,$01;{$ENDIF} 

       // next columns:
       add ecx, 32;
       add edx, 32;

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
   mov eax, edi;
   @addforyloop2:
       {$IFDEF FPC}vmovupd xmm0, [edx + eax];{$ELSE}db $C5,$F9,$10,$04,$02;{$ENDIF} 
       {$IFDEF FPC}vaddpd xmm1, xmm1, xmm0;{$ELSE}db $C5,$F1,$58,$C8;{$ENDIF} 
   add eax, ebx;
   jnz @addforyloop2;

   // build result
   {$IFDEF FPC}vdivpd xmm0, xmm1, xmm2;{$ELSE}db $C5,$F1,$5E,$C2;{$ENDIF} 
   {$IFDEF FPC}vmovupd [ecx], xmm0;{$ELSE}db $C5,$F9,$11,$01;{$ENDIF} 

   // calculate final standard deviation
   // for y := 0 to height - 1;
   // prepare for reverse loop
   {$IFDEF FPC}vxorpd xmm4, xmm4, xmm4;{$ELSE}db $C5,$D9,$57,$E4;{$ENDIF} 
   mov eax, edi;
   @addforyloop2_2:
      {$IFDEF FPC}vmovupd xmm1, [edx + eax];{$ELSE}db $C5,$F9,$10,$0C,$02;{$ENDIF} 
      {$IFDEF FPC}vsubpd xmm1, xmm1, xmm0;{$ELSE}db $C5,$F1,$5C,$C8;{$ENDIF} 
      {$IFDEF FPC}vmulpd xmm1, xmm1, xmm1;{$ELSE}db $C5,$F1,$59,$C9;{$ENDIF} 
      {$IFDEF FPC}vaddpd xmm4, xmm4, xmm1;{$ELSE}db $C5,$D9,$58,$E1;{$ENDIF} 
   add eax, ebx;
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
   mov eax, destLineWidth;
   {$IFDEF FPC}vmovupd [ecx + eax], xmm4;{$ELSE}db $C5,$F9,$11,$24,$01;{$ENDIF} 

   // next columns:
   add ecx, 16;
   add edx, 16;

   dec esi;
   jnz @@endProc;

   @@lastcolumn:

   {$IFDEF FPC}vxorpd xmm1, xmm1, xmm1;{$ELSE}db $C5,$F1,$57,$C9;{$ENDIF} 

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov eax, edi;
   @addforyloop_1:
       {$IFDEF FPC}vaddsd xmm1, xmm1, [edx + eax];{$ELSE}db $C5,$F3,$58,$0C,$02;{$ENDIF} 
   add eax, ebx;
   jnz @addforyloop_1;

   // build result
   {$IFDEF FPC}vdivsd xmm0, xmm1, xmm2;{$ELSE}db $C5,$F3,$5E,$C2;{$ENDIF} 
   {$IFDEF FPC}vmovsd [ecx], xmm0;{$ELSE}db $C5,$FB,$11,$01;{$ENDIF} 

   // calculate final standard deviation
   // for y := 0 to height - 1;
   // prepare for reverse loop
   {$IFDEF FPC}vxorpd xmm4, xmm4, xmm4;{$ELSE}db $C5,$D9,$57,$E4;{$ENDIF} 
   mov eax, edi;
   @addforyloop1_2:
      {$IFDEF FPC}vmovsd xmm1, [edx + eax];{$ELSE}db $C5,$FB,$10,$0C,$02;{$ENDIF} 
      {$IFDEF FPC}vsubsd xmm1, xmm1, xmm0;{$ELSE}db $C5,$F3,$5C,$C8;{$ENDIF} 
      {$IFDEF FPC}vmulsd xmm1, xmm1, xmm1;{$ELSE}db $C5,$F3,$59,$C9;{$ENDIF} 
      {$IFDEF FPC}vaddsd xmm4, xmm4, xmm1;{$ELSE}db $C5,$DB,$58,$E1;{$ENDIF} 
   add eax, ebx;
   jnz @addforyloop1_2;

   // check if we need to use the unbiased version
   cmp unbiased, 0;
   jz @@dobiased_2;

   {$IFDEF FPC}vmovsd xmm3, xmm3, xmm2;{$ELSE}db $C5,$E3,$10,$DA;{$ENDIF} 
   {$IFDEF FPC}vsubsd xmm3, xmm3, xmm5;{$ELSE}db $C5,$E3,$5C,$DD;{$ENDIF} 
   {$IFDEF FPC}vmaxsd xmm3, xmm3, xmm5;{$ELSE}db $C5,$E3,$5F,$DD;{$ENDIF} 

   {$IFDEF FPC}vdivsd xmm4, xmm4, xmm3;{$ELSE}db $C5,$DB,$5E,$E3;{$ENDIF} 

   jmp @@writeRes_1;

   @@dobiased_1:
   {$IFDEF FPC}vdivsd xmm4, xmm4, xmm2;{$ELSE}db $C5,$DB,$5E,$E2;{$ENDIF} 

   // write result
   @@writeRes_1:
   mov eax, destLineWidth;
   {$IFDEF FPC}vmovsd [ecx + eax], xmm4;{$ELSE}db $C5,$FB,$11,$24,$01;{$ENDIF} 

   @@endProc:

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;
end;

{$ENDIF}

end.
