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


unit AVXMatrixAddSubOperations;

// ############################################################
// ##### Matrix addition/subtraction assembler optimized:
// ############################################################

interface

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}
{$IFNDEF x64}

uses MatrixConst;

procedure AVXMatrixAddAligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
procedure AVXMatrixAddUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);

procedure AVXMatrixSubAligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
procedure AVXMatrixSubUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);

procedure AVXMatrixSubT(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; LineWidthB : TASMNativeInt; width, height : TASMNativeInt);
{$ENDIF}

implementation

{$IFNDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$S-} {$ENDIF}

procedure AVXMatrixAddAligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var iters : TASMNativeInt;
begin
asm
   push ebx;
   push esi;
   push edi;

   //iters := -width*sizeof(double);
   mov eax, width;
   imul eax, -8;
   mov iters, eax;

   // helper registers for the mt1, mt2 and dest pointers
   mov ecx, dest;
   mov edi, mt2;
   mov esi, mt1;

   sub esi, eax;
   sub edi, eax;
   sub ecx, eax;

   // for y := 0 to height - 1:
   mov ebx, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov eax, iters;
       @addforxloop:
           add eax, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [esi + eax];
           // prefetch [edi + eax];

           // addition:
           {$IFDEF FPC}vmovapd ymm0, [esi + eax - 128];{$ELSE}db $C5,$FD,$28,$44,$06,$80;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, [edi + eax - 128];{$ELSE}db $C5,$FD,$58,$44,$07,$80;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm1, [esi + eax - 96];{$ELSE}db $C5,$FD,$28,$4C,$06,$A0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm1, ymm1, [edi + eax - 96];{$ELSE}db $C5,$F5,$58,$4C,$07,$A0;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm2, [esi + eax - 64];{$ELSE}db $C5,$FD,$28,$54,$06,$C0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm2, ymm2, [edi + eax - 64];{$ELSE}db $C5,$ED,$58,$54,$07,$C0;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm3, [esi + eax - 32];{$ELSE}db $C5,$FD,$28,$5C,$06,$E0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm3, ymm3, [edi + eax - 32];{$ELSE}db $C5,$E5,$58,$5C,$07,$E0;{$ENDIF} 

           {$IFDEF FPC}vmovapd [ecx + eax - 128], ymm0;{$ELSE}db $C5,$FD,$29,$44,$01,$80;{$ENDIF} 
           {$IFDEF FPC}vmovapd [ecx + eax - 96], ymm1;{$ELSE}db $C5,$FD,$29,$4C,$01,$A0;{$ENDIF} 
           {$IFDEF FPC}vmovapd [ecx + eax - 64], ymm2;{$ELSE}db $C5,$FD,$29,$54,$01,$C0;{$ENDIF} 
           {$IFDEF FPC}vmovapd [ecx + eax - 32], ymm3;{$ELSE}db $C5,$FD,$29,$5C,$01,$E0;{$ENDIF} 
       jmp @addforxloop

       @loopEnd:

       sub eax, 128;

       jz @nextLine;

       add eax, 16;
       jg @@lastelem;

       @addforxloop2:
           {$IFDEF FPC}vmovapd xmm0, [esi + eax - 16];{$ELSE}db $C5,$F9,$28,$44,$06,$F0;{$ENDIF} 
           {$IFDEF FPC}vaddpd xmm0, xmm0, [edi + eax - 16];{$ELSE}db $C5,$F9,$58,$44,$07,$F0;{$ENDIF} 

           {$IFDEF FPC}vmovapd [ecx + eax - 16], xmm0;{$ELSE}db $C5,$F9,$29,$44,$01,$F0;{$ENDIF} 
       add eax, 16;
       jle @addforxloop2;

       @@lastelem:

       sub eax, 16;
       jz @nextLine;

       {$IFDEF FPC}vmovsd xmm0, [esi - 8];{$ELSE}db $C5,$FB,$10,$46,$F8;{$ENDIF} 
       {$IFDEF FPC}vmovsd xmm1, [edi - 8];{$ELSE}db $C5,$FB,$10,$4F,$F8;{$ENDIF} 
       {$IFDEF FPC}vaddsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$58,$C1;{$ENDIF} 
       {$IFDEF FPC}vmovsd [ecx - 8], xmm0;{$ELSE}db $C5,$FB,$11,$41,$F8;{$ENDIF} 

       @nextLine:

       // next line:
       add esi, LineWidth1;
       add edi, LineWidth2;
       add ecx, destLineWidth;

   // loop y end
   dec ebx;
   jnz @@addforyloop;

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop edi;
   pop esi;
   pop ebx;
end;
end;

procedure AVXMatrixAddUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var iters : TASMNativeInt;
begin
asm
   push ebx;
   push esi;
   push edi;

   //iters := -width*sizeof(double);
   mov eax, width;
   imul eax, -8;
   mov iters, eax;

   // helper registers for the mt1, mt2 and dest pointers
   mov ecx, dest;
   mov edi, mt2;
   mov esi, mt1;

   sub esi, eax;
   sub edi, eax;
   sub ecx, eax;

   // for y := 0 to height - 1:
   mov ebx, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov eax, iters;
       @addforxloop:
           add eax, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [esi + eax];
           // prefetch [edi + eax];

           // addition:
           {$IFDEF FPC}vmovupd ymm0, [esi + eax - 128];{$ELSE}db $C5,$FD,$10,$44,$06,$80;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm1, [edi + eax - 128];{$ELSE}db $C5,$FD,$10,$4C,$07,$80;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 
           {$IFDEF FPC}vmovupd [ecx + eax - 128], ymm0;{$ELSE}db $C5,$FD,$11,$44,$01,$80;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm0, [esi + eax - 96];{$ELSE}db $C5,$FD,$10,$44,$06,$A0;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm1, [edi + eax - 96];{$ELSE}db $C5,$FD,$10,$4C,$07,$A0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 
           {$IFDEF FPC}vmovupd [ecx + eax - 96], ymm0;{$ELSE}db $C5,$FD,$11,$44,$01,$A0;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm0, [esi + eax - 64];{$ELSE}db $C5,$FD,$10,$44,$06,$C0;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm1, [edi + eax - 64];{$ELSE}db $C5,$FD,$10,$4C,$07,$C0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 
           {$IFDEF FPC}vmovupd [ecx + eax - 64], ymm0;{$ELSE}db $C5,$FD,$11,$44,$01,$C0;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm0, [esi + eax - 32];{$ELSE}db $C5,$FD,$10,$44,$06,$E0;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm1, [edi + eax - 32];{$ELSE}db $C5,$FD,$10,$4C,$07,$E0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 
           {$IFDEF FPC}vmovupd [ecx + eax - 32], ymm0;{$ELSE}db $C5,$FD,$11,$44,$01,$E0;{$ENDIF} 

       jmp @addforxloop

       @loopEnd:

       sub eax, 128;

       jz @nextLine;

       add eax, 16;
       jg @@lastelem;

       @addforxloop2:
           {$IFDEF FPC}vmovupd xmm0, [esi + eax - 16];{$ELSE}db $C5,$F9,$10,$44,$06,$F0;{$ENDIF} 
           {$IFDEF FPC}vmovupd xmm1, [edi + eax - 16];{$ELSE}db $C5,$F9,$10,$4C,$07,$F0;{$ENDIF} 
           {$IFDEF FPC}vaddpd xmm0, xmm0, xmm1;{$ELSE}db $C5,$F9,$58,$C1;{$ENDIF} 

           {$IFDEF FPC}vmovupd [ecx + eax - 16], xmm0;{$ELSE}db $C5,$F9,$11,$44,$01,$F0;{$ENDIF} 
       add eax, 16;
       jle @addforxloop2;

       @@lastelem:

       sub eax, 16;
       jz @nextLine;

       {$IFDEF FPC}vmovsd xmm0, [esi - 8];{$ELSE}db $C5,$FB,$10,$46,$F8;{$ENDIF} 
       {$IFDEF FPC}vmovsd xmm1, [edi - 8];{$ELSE}db $C5,$FB,$10,$4F,$F8;{$ENDIF} 
       {$IFDEF FPC}vaddsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$58,$C1;{$ENDIF} 
       {$IFDEF FPC}vmovsd [ecx - 8], xmm0;{$ELSE}db $C5,$FB,$11,$41,$F8;{$ENDIF} 

       @nextLine:

       // next line:
       add esi, LineWidth1;
       add edi, LineWidth2;
       add ecx, destLineWidth;

   // loop y end
   dec ebx;
   jnz @@addforyloop;

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop edi;
   pop esi;
   pop ebx;
end;
end;

procedure AVXMatrixSubAligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var iters : TASMNativeInt;
begin
asm
   push ebx;
   push esi;
   push edi;

   //iters := -width*sizeof(double);
   mov eax, width;
   imul eax, -8;
   mov iters, eax;

   // helper registers for the mt1, mt2 and dest pointers
   mov ecx, dest;
   mov edi, mt2;
   mov esi, mt1;

   sub esi, eax;
   sub edi, eax;
   sub ecx, eax;

   // for y := 0 to height - 1:
   mov ebx, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov eax, iters;
       @addforxloop:
           add eax, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [esi + eax];
           // prefetch [edi + eax];

           // addition:
           {$IFDEF FPC}vmovapd ymm0, [esi + eax - 128];{$ELSE}db $C5,$FD,$28,$44,$06,$80;{$ENDIF} 
           {$IFDEF FPC}vsubpd ymm0, ymm0, [edi + eax - 128];{$ELSE}db $C5,$FD,$5C,$44,$07,$80;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm1, [esi + eax - 96];{$ELSE}db $C5,$FD,$28,$4C,$06,$A0;{$ENDIF} 
           {$IFDEF FPC}vsubpd ymm1, ymm1, [edi + eax - 96];{$ELSE}db $C5,$F5,$5C,$4C,$07,$A0;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm2, [esi + eax - 64];{$ELSE}db $C5,$FD,$28,$54,$06,$C0;{$ENDIF} 
           {$IFDEF FPC}vsubpd ymm2, ymm2, [edi + eax - 64];{$ELSE}db $C5,$ED,$5C,$54,$07,$C0;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm3, [esi + eax - 32];{$ELSE}db $C5,$FD,$28,$5C,$06,$E0;{$ENDIF} 
           {$IFDEF FPC}vsubpd ymm3, ymm3, [edi + eax - 32];{$ELSE}db $C5,$E5,$5C,$5C,$07,$E0;{$ENDIF} 

           {$IFDEF FPC}vmovapd [ecx + eax - 128], ymm0;{$ELSE}db $C5,$FD,$29,$44,$01,$80;{$ENDIF} 
           {$IFDEF FPC}vmovapd [ecx + eax - 96], ymm1;{$ELSE}db $C5,$FD,$29,$4C,$01,$A0;{$ENDIF} 
           {$IFDEF FPC}vmovapd [ecx + eax - 64], ymm2;{$ELSE}db $C5,$FD,$29,$54,$01,$C0;{$ENDIF} 
           {$IFDEF FPC}vmovapd [ecx + eax - 32], ymm3;{$ELSE}db $C5,$FD,$29,$5C,$01,$E0;{$ENDIF} 
       jmp @addforxloop

       @loopEnd:

       sub eax, 128;

       jz @nextLine;

       add eax, 16;
       jg @@lastelem;

       @addforxloop2:
           {$IFDEF FPC}vmovapd xmm0, [esi + eax - 16];{$ELSE}db $C5,$F9,$28,$44,$06,$F0;{$ENDIF} 
           {$IFDEF FPC}vsubpd xmm0, xmm0, [edi + eax - 16];{$ELSE}db $C5,$F9,$5C,$44,$07,$F0;{$ENDIF} 

           {$IFDEF FPC}vmovapd [ecx + eax - 16], xmm0;{$ELSE}db $C5,$F9,$29,$44,$01,$F0;{$ENDIF} 
       add eax, 16;
       jle @addforxloop2;

       @@lastelem:

       sub eax, 16;
       jz @nextLine;

       {$IFDEF FPC}vmovsd xmm0, [esi - 8];{$ELSE}db $C5,$FB,$10,$46,$F8;{$ENDIF} 
       {$IFDEF FPC}vmovsd xmm1, [edi - 8];{$ELSE}db $C5,$FB,$10,$4F,$F8;{$ENDIF} 
       {$IFDEF FPC}vsubsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$5C,$C1;{$ENDIF} 
       {$IFDEF FPC}vmovsd [ecx - 8], xmm0;{$ELSE}db $C5,$FB,$11,$41,$F8;{$ENDIF} 

       @nextLine:

       // update pointers
       add esi, LineWidth1;
       add edi, LineWidth2;
       add ecx, destLineWidth;

   // loop y end
   dec ebx;
   jnz @@addforyloop;

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop edi;
   pop esi;
   pop ebx;
end;
end;

procedure AVXMatrixSubUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var iters : TASMNativeInt;
begin
asm
   push ebx;
   push esi;
   push edi;

   //iters := -width*sizeof(double);
   mov eax, width;
   imul eax, -8;
   mov iters, eax;

   // helper registers for the mt1, mt2 and dest pointers
   mov ecx, dest;
   mov edi, mt2;
   mov esi, mt1;

   sub esi, eax;
   sub edi, eax;
   sub ecx, eax;

   // for y := 0 to height - 1:
   mov ebx, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov eax, iters;
       @addforxloop:
           add eax, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [esi + eax];
           // prefetch [edi + eax];

           // addition:
           {$IFDEF FPC}vmovupd ymm0, [esi + eax - 128];{$ELSE}db $C5,$FD,$10,$44,$06,$80;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm1, [edi + eax - 128];{$ELSE}db $C5,$FD,$10,$4C,$07,$80;{$ENDIF} 
           {$IFDEF FPC}vsubpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$5C,$C1;{$ENDIF} 
           {$IFDEF FPC}vmovupd [ecx + eax - 128], ymm0;{$ELSE}db $C5,$FD,$11,$44,$01,$80;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm0, [esi + eax - 96];{$ELSE}db $C5,$FD,$10,$44,$06,$A0;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm1, [edi + eax - 96];{$ELSE}db $C5,$FD,$10,$4C,$07,$A0;{$ENDIF} 
           {$IFDEF FPC}vsubpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$5C,$C1;{$ENDIF} 
           {$IFDEF FPC}vmovupd [ecx + eax - 96], ymm0;{$ELSE}db $C5,$FD,$11,$44,$01,$A0;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm0, [esi + eax - 64];{$ELSE}db $C5,$FD,$10,$44,$06,$C0;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm1, [edi + eax - 64];{$ELSE}db $C5,$FD,$10,$4C,$07,$C0;{$ENDIF} 
           {$IFDEF FPC}vsubpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$5C,$C1;{$ENDIF} 
           {$IFDEF FPC}vmovupd [ecx + eax - 64], ymm0;{$ELSE}db $C5,$FD,$11,$44,$01,$C0;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm0, [esi + eax - 32];{$ELSE}db $C5,$FD,$10,$44,$06,$E0;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm1, [edi + eax - 32];{$ELSE}db $C5,$FD,$10,$4C,$07,$E0;{$ENDIF} 
           {$IFDEF FPC}vsubpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$5C,$C1;{$ENDIF} 
           {$IFDEF FPC}vmovupd [ecx + eax - 32], ymm0;{$ELSE}db $C5,$FD,$11,$44,$01,$E0;{$ENDIF} 

       jmp @addforxloop

       @loopEnd:

       sub eax, 128;

       jz @nextLine;

       add eax, 16;
       jg @@lastelem;

       @addforxloop2:
           {$IFDEF FPC}vmovupd xmm0, [esi + eax - 16];{$ELSE}db $C5,$F9,$10,$44,$06,$F0;{$ENDIF} 
           {$IFDEF FPC}vmovupd xmm1, [edi + eax - 16];{$ELSE}db $C5,$F9,$10,$4C,$07,$F0;{$ENDIF} 
           {$IFDEF FPC}vsubpd xmm0, xmm0, xmm1;{$ELSE}db $C5,$F9,$5C,$C1;{$ENDIF} 

           {$IFDEF FPC}vmovupd [ecx + eax - 16], xmm0;{$ELSE}db $C5,$F9,$11,$44,$01,$F0;{$ENDIF} 
       add eax, 16;
       jle @addforxloop2;

       @@lastelem:

       sub eax, 16;
       jz @nextLine;

       {$IFDEF FPC}vmovsd xmm0, [esi - 8];{$ELSE}db $C5,$FB,$10,$46,$F8;{$ENDIF} 
       {$IFDEF FPC}vmovsd xmm1, [edi - 8];{$ELSE}db $C5,$FB,$10,$4F,$F8;{$ENDIF} 
       {$IFDEF FPC}vsubsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$5C,$C1;{$ENDIF} 
       {$IFDEF FPC}vmovsd [ecx - 8], xmm0;{$ELSE}db $C5,$FB,$11,$41,$F8;{$ENDIF} 

       @nextLine:

       // next line:
       add esi, LineWidth1;
       add edi, LineWidth2;
       add ecx, destLineWidth;

   // loop y end
   dec ebx;
   jnz @@addforyloop;

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop edi;
   pop esi;
   pop ebx;
end;
end;

procedure AVXMatrixSubT(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; LineWidthB : TASMNativeInt; width, height : TASMNativeInt);
begin
asm
   push esi;
   push edi;
   push ebx;

   // eax: iter := -width*sizeof(double)
   mov ecx, A;
   mov eax, width;
   imul eax, -8;
   sub ecx, eax;

   mov esi, B;
   mov edx, LineWidthB;

   // for y := 0 to height - 1
   @@foryloop:
      mov edi, esi;
      mov ebx, eax;

      // for x := 0 to width - 1
      @@forxloop:
         {$IFDEF FPC}vmovsd xmm0, [ecx + ebx];{$ELSE}db $C5,$FB,$10,$04,$19;{$ENDIF} 
         {$IFDEF FPC}vmovsd xmm1, [edi];{$ELSE}db $C5,$FB,$10,$0F;{$ENDIF} 
         {$IFDEF FPC}vsubsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$5C,$C1;{$ENDIF} 
         {$IFDEF FPC}vmovsd [ecx + ebx], xmm0;{$ELSE}db $C5,$FB,$11,$04,$19;{$ENDIF} 

         add edi, edx;
      add ebx, 8;
      jnz @@forxloop;

      add ecx, LineWidthA;
      add esi, 8;
   dec height;
   jnz @@foryloop;

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop ebx;
   pop edi;
   pop esi;
end;
end;

{$ENDIF}

end.
