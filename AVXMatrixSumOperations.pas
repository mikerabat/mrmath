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


unit AVXMatrixSumOperations;

interface

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}
{$IFNDEF x64}

uses MatrixConst;

procedure AVXMatrixSumRowAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
procedure AVXMatrixSumRowUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);

procedure AVXMatrixSumColumnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
procedure AVXMatrixSumColumnUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);

{$ENDIF}

implementation

{$IFNDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$S-} {$ENDIF}

procedure AVXMatrixSumRowAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
begin
asm
   // prolog
   push ebx;
   push edi;
   push esi;

   // iters := -width*sizeof(double)
   mov edi, width;
   imul edi, -8;

   mov ebx, srcLineWidth;
   mov edx, src;
   mov ecx, dest;

   // helper registers for the mt1, mt2 and dest pointers
   sub edx, edi;

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
           // prefetch [edx + eax];

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

       // write result
       {$IFDEF FPC}vmovsd [ecx], xmm0;{$ELSE}db $C5,$FB,$11,$01;{$ENDIF} 

       // next line:
       add edx, ebx;
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

procedure AVXMatrixSumRowUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
begin
asm
   // prolog
   push ebx;
   push edi;
   push esi;

   // iters := -width*sizeof(double)
   mov edi, width;
   imul edi, -8;

   mov edx, src;
   mov ecx, dest;

   // helper registers for the mt1, mt2 and dest pointers
   sub edx, edi;

   mov ebx, srcLineWidth;
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
           // prefetch [edx + eax];

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

       // write result
       {$IFDEF FPC}vmovsd [ecx], xmm0;{$ELSE}db $C5,$FB,$11,$01;{$ENDIF} 

       // next line:
       add edx, ebx;
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

procedure AVXMatrixSumColumnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
begin
asm
   // prolog
   push ebx;
   push edi;
   push esi;

   // init
   mov ecx, dest;
   mov edx, src;

   xor edi, edi;
   sub edi, height;
   imul edi, srcLineWidth;

   mov ebx, srcLineWidth;

   // helper registers for the mt1, mt2 and dest pointers
   sub edx, edi;

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
   mov eax, edi;
   @addforyloop2:
       {$IFDEF FPC}vaddpd xmm1, xmm1, [edx + eax];{$ELSE}db $C5,$F1,$58,$0C,$02;{$ENDIF} 
   add eax, ebx;
   jnz @addforyloop2;

   // build result
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
   mov eax, edi;
   @addforyloop:
       {$IFDEF FPC}vaddsd xmm1, xmm1, [edx + eax];{$ELSE}db $C5,$F3,$58,$0C,$02;{$ENDIF} 
   add eax, ebx;
   jnz @addforyloop;

   // build result
   {$IFDEF FPC}vmovsd [ecx], xmm1;{$ELSE}db $C5,$FB,$11,$09;{$ENDIF} 

   @@endProc:
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 
   pop esi;
   pop edi;
   pop ebx;
end;
end;

procedure AVXMatrixSumColumnUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
begin
asm
   // prolog
   push ebx;
   push edi;
   push esi;

   // prepare
   mov ecx, dest;
   mov edx, src;

   xor edi, edi;
   sub edi, height;
   imul edi, srcLineWidth;

   // helper registers for the mt1, mt2 and dest pointers
   sub edx, edi;

   mov ebx, srcLineWidth;

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
   mov eax, edi;
   @addforyloop2:
       {$IFDEF FPC}vmovupd xmm0, [edx + eax];{$ELSE}db $C5,$F9,$10,$04,$02;{$ENDIF} 
       {$IFDEF FPC}vaddpd xmm1, xmm1, [edx + eax];{$ELSE}db $C5,$F1,$58,$0C,$02;{$ENDIF} 
   add eax, ebx;
   jnz @addforyloop2;

   // build result
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
   mov eax, edi;
   @addforyloop:
       {$IFDEF FPC}vaddsd xmm1, xmm1, [edx + eax];{$ELSE}db $C5,$F3,$58,$0C,$02;{$ENDIF} 
   add eax, ebx;
   jnz @addforyloop;

   // build result
   {$IFDEF FPC}vmovsd [ecx], xmm1;{$ELSE}db $C5,$FB,$11,$09;{$ENDIF} 

   @@endProc:
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;
end;

{$ENDIF}

end.
