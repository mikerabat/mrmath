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

{$I 'mrMath_CPU.inc'}

{$IFNDEF x64}

uses MatrixConst;

procedure AVXMatrixAddAligned(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width : NativeInt; height : NativeInt; const LineWidth1, LineWidth2 : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure AVXMatrixAddUnAligned(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width : NativeInt; height : NativeInt; const LineWidth1, LineWidth2 : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

procedure AVXMatrixSubAligned(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width : NativeInt; height : NativeInt; const LineWidth1, LineWidth2 : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure AVXMatrixSubUnAligned(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width : NativeInt; height : NativeInt; const LineWidth1, LineWidth2 : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

procedure AVXMatrixSubT(A : PDouble; LineWidthA : NativeInt; B : PDouble; LineWidthB : NativeInt; width, height : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

procedure AVXMatrixSubVecAlignedVecRow(A : PDouble; LineWidthA : NativeInt; B : PDouble; incX : NativeInt; width, Height : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure AVXMatrixSubVecAlignedRow(A : PDouble; LineWidthA : NativeInt; B : PDouble; incX : NativeInt; width, Height : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure AVXMatrixSubVecAlignedCol(A : PDouble; LineWidthA : NativeInt; B : PDouble; incX : NativeInt; width, Height : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

procedure AVXMatrixSubVecUnalignedVecRow(A : PDouble; LineWidthA : NativeInt; B : PDouble; incX : NativeInt; width, Height : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure AVXMatrixSubVecUnalignedRow(A : PDouble; LineWidthA : NativeInt; B : PDouble; incX : NativeInt; width, Height : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure AVXMatrixSubVecUnalignedCol(A : PDouble; LineWidthA : NativeInt; B : PDouble; incX : NativeInt; width, Height : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

procedure AVXMatrixAddVecAlignedVecRow(A : PDouble; LineWidthA : NativeInt; B : PDouble; incX : NativeInt; width, Height : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure AVXMatrixAddVecAlignedRow(A : PDouble; LineWidthA : NativeInt; B : PDouble; incX : NativeInt; width, Height : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure AVXMatrixAddVecAlignedCol(A : PDouble; LineWidthA : NativeInt; B : PDouble; incX : NativeInt; width, Height : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

procedure AVXMatrixAddVecUnalignedVecRow(A : PDouble; LineWidthA : NativeInt; B : PDouble; incX : NativeInt; width, Height : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure AVXMatrixAddVecUnalignedRow(A : PDouble; LineWidthA : NativeInt; B : PDouble; incX : NativeInt; width, Height : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure AVXMatrixAddVecUnalignedCol(A : PDouble; LineWidthA : NativeInt; B : PDouble; incX : NativeInt; width, Height : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

{$ENDIF}

implementation

{$IFNDEF x64}

procedure AVXMatrixAddAligned(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width : NativeInt; height : NativeInt; const LineWidth1, LineWidth2 : NativeInt);
// eax = dest, edx = destLineWidth, mt1 = ecx
asm
   push ebx;
   push esi;
   push edi;

   //iters := -width*sizeof(double);
   mov ebx, width;
   imul ebx, -8;

   // helper registers for the mt1, mt2 and dest pointers
   mov edi, mt2;

   sub ecx, ebx;
   sub edi, ebx;
   sub eax, ebx;

   // for y := 0 to height - 1:
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov esi, ebx;
       @addforxloop:
           add esi, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [esi + esi];
           // prefetch [edi + esi];

           // addition:
           {$IFDEF FPC}vmovapd ymm0, [ecx + esi - 128];{$ELSE}db $C5,$FD,$28,$44,$31,$80;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, [edi + esi - 128];{$ELSE}db $C5,$FD,$58,$44,$37,$80;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm1, [ecx + esi - 96];{$ELSE}db $C5,$FD,$28,$4C,$31,$A0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm1, ymm1, [edi + esi - 96];{$ELSE}db $C5,$F5,$58,$4C,$37,$A0;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm2, [ecx + esi - 64];{$ELSE}db $C5,$FD,$28,$54,$31,$C0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm2, ymm2, [edi + esi - 64];{$ELSE}db $C5,$ED,$58,$54,$37,$C0;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm3, [ecx + esi - 32];{$ELSE}db $C5,$FD,$28,$5C,$31,$E0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm3, ymm3, [edi + esi - 32];{$ELSE}db $C5,$E5,$58,$5C,$37,$E0;{$ENDIF} 

           {$IFDEF FPC}vmovapd [eax + esi - 128], ymm0;{$ELSE}db $C5,$FD,$29,$44,$30,$80;{$ENDIF} 
           {$IFDEF FPC}vmovapd [eax + esi - 96], ymm1;{$ELSE}db $C5,$FD,$29,$4C,$30,$A0;{$ENDIF} 
           {$IFDEF FPC}vmovapd [eax + esi - 64], ymm2;{$ELSE}db $C5,$FD,$29,$54,$30,$C0;{$ENDIF} 
           {$IFDEF FPC}vmovapd [eax + esi - 32], ymm3;{$ELSE}db $C5,$FD,$29,$5C,$30,$E0;{$ENDIF} 
       jmp @addforxloop

       @loopEnd:

       sub esi, 128;

       jz @nextLine;

       @addforxloop2:
           add esi, 16;
           jg @loopEnd2;
           {$IFDEF FPC}vmovapd xmm0, [ecx + esi - 16];{$ELSE}db $C5,$F9,$28,$44,$31,$F0;{$ENDIF} 
           {$IFDEF FPC}vaddpd xmm0, xmm0, [edi + esi - 16];{$ELSE}db $C5,$F9,$58,$44,$37,$F0;{$ENDIF} 

           {$IFDEF FPC}vmovapd [eax + esi - 16], xmm0;{$ELSE}db $C5,$F9,$29,$44,$30,$F0;{$ENDIF} 
       jmp @addforxloop2;

       @loopEnd2:

       sub esi, 16;
       jz @nextLine;

       {$IFDEF FPC}vmovsd xmm0, [ecx - 8];{$ELSE}db $C5,$FB,$10,$41,$F8;{$ENDIF} 
       {$IFDEF FPC}vmovsd xmm1, [edi - 8];{$ELSE}db $C5,$FB,$10,$4F,$F8;{$ENDIF} 
       {$IFDEF FPC}vaddsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$58,$C1;{$ENDIF} 
       {$IFDEF FPC}vmovsd [eax - 8], xmm0;{$ELSE}db $C5,$FB,$11,$40,$F8;{$ENDIF} 

       @nextLine:

       // next line:
       add ecx, LineWidth1;
       add edi, LineWidth2;
       add eax, edx;

   // loop y end
   dec height;
   jnz @@addforyloop;

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop edi;
   pop esi;
   pop ebx;
end;

procedure AVXMatrixAddUnAligned(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width : NativeInt; height : NativeInt; const LineWidth1, LineWidth2 : NativeInt);
asm
   push ebx;
   push esi;
   push edi;

   //iters := -width*sizeof(double);
   mov ebx, width;
   imul ebx, -8;

   // helper registers for the mt1, mt2 and dest pointers
   mov edi, mt2;

   sub ecx, ebx;
   sub edi, ebx;
   sub eax, ebx;

   // for y := 0 to height - 1:
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov esi, ebx;
       @addforxloop:
           add esi, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [esi + esi];
           // prefetch [edi + esi];

           // addition:
           {$IFDEF FPC}vmovupd ymm0, [ecx + esi - 128];{$ELSE}db $C5,$FD,$10,$44,$31,$80;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm1, [edi + esi - 128];{$ELSE}db $C5,$FD,$10,$4C,$37,$80;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 
           {$IFDEF FPC}vmovupd [eax + esi - 128], ymm0;{$ELSE}db $C5,$FD,$11,$44,$30,$80;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm0, [ecx + esi - 96];{$ELSE}db $C5,$FD,$10,$44,$31,$A0;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm1, [edi + esi - 96];{$ELSE}db $C5,$FD,$10,$4C,$37,$A0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 
           {$IFDEF FPC}vmovupd [eax + esi - 96], ymm0;{$ELSE}db $C5,$FD,$11,$44,$30,$A0;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm0, [ecx + esi - 64];{$ELSE}db $C5,$FD,$10,$44,$31,$C0;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm1, [edi + esi - 64];{$ELSE}db $C5,$FD,$10,$4C,$37,$C0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 
           {$IFDEF FPC}vmovupd [eax + esi - 64], ymm0;{$ELSE}db $C5,$FD,$11,$44,$30,$C0;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm0, [ecx + esi - 32];{$ELSE}db $C5,$FD,$10,$44,$31,$E0;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm1, [edi + esi - 32];{$ELSE}db $C5,$FD,$10,$4C,$37,$E0;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 
           {$IFDEF FPC}vmovupd [eax + esi - 32], ymm0;{$ELSE}db $C5,$FD,$11,$44,$30,$E0;{$ENDIF} 

       jmp @addforxloop

       @loopEnd:

       sub esi, 128;

       jz @nextLine;

       @addforxloop2:
           add esi, 16;
           jg @loopEnd2;

           {$IFDEF FPC}vmovupd xmm0, [ecx + esi - 16];{$ELSE}db $C5,$F9,$10,$44,$31,$F0;{$ENDIF} 
           {$IFDEF FPC}vmovupd xmm1, [edi + esi - 16];{$ELSE}db $C5,$F9,$10,$4C,$37,$F0;{$ENDIF} 
           {$IFDEF FPC}vaddpd xmm0, xmm0, xmm1;{$ELSE}db $C5,$F9,$58,$C1;{$ENDIF} 

           {$IFDEF FPC}vmovupd [eax + esi - 16], xmm0;{$ELSE}db $C5,$F9,$11,$44,$30,$F0;{$ENDIF} 
       jmp @addforxloop2;

       @loopEnd2:

       sub esi, 16;
       jz @nextLine;

       {$IFDEF FPC}vmovsd xmm0, [ecx - 8];{$ELSE}db $C5,$FB,$10,$41,$F8;{$ENDIF} 
       {$IFDEF FPC}vmovsd xmm1, [edi - 8];{$ELSE}db $C5,$FB,$10,$4F,$F8;{$ENDIF} 
       {$IFDEF FPC}vaddsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$58,$C1;{$ENDIF} 
       {$IFDEF FPC}vmovsd [eax - 8], xmm0;{$ELSE}db $C5,$FB,$11,$40,$F8;{$ENDIF} 

       @nextLine:

       // next line:
       add ecx, LineWidth1;
       add edi, LineWidth2;
       add eax, edx;

   // loop y end
   dec height;
   jnz @@addforyloop;

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop edi;
   pop esi;
   pop ebx;
end;

procedure AVXMatrixSubAligned(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width : NativeInt; height : NativeInt; const LineWidth1, LineWidth2 : NativeInt);
asm
   push ebx;
   push esi;
   push edi;

   //iters := -width*sizeof(double);
   mov ebx, width;
   imul ebx, -8;

   // helper registers for the mt1, mt2 and dest pointers
   mov edi, mt2;

   sub ecx, ebx;
   sub edi, ebx;
   sub eax, ebx;

   // for y := 0 to height - 1:
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov esi, ebx;
       @addforxloop:
           add esi, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [esi + esi];
           // prefetch [edi + esi];

           // addition:
           {$IFDEF FPC}vmovapd ymm0, [ecx + esi - 128];{$ELSE}db $C5,$FD,$28,$44,$31,$80;{$ENDIF} 
           {$IFDEF FPC}vsubpd ymm0, ymm0, [edi + esi - 128];{$ELSE}db $C5,$FD,$5C,$44,$37,$80;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm1, [ecx + esi - 96];{$ELSE}db $C5,$FD,$28,$4C,$31,$A0;{$ENDIF} 
           {$IFDEF FPC}vsubpd ymm1, ymm1, [edi + esi - 96];{$ELSE}db $C5,$F5,$5C,$4C,$37,$A0;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm2, [ecx + esi - 64];{$ELSE}db $C5,$FD,$28,$54,$31,$C0;{$ENDIF} 
           {$IFDEF FPC}vsubpd ymm2, ymm2, [edi + esi - 64];{$ELSE}db $C5,$ED,$5C,$54,$37,$C0;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm3, [ecx + esi - 32];{$ELSE}db $C5,$FD,$28,$5C,$31,$E0;{$ENDIF} 
           {$IFDEF FPC}vsubpd ymm3, ymm3, [edi + esi - 32];{$ELSE}db $C5,$E5,$5C,$5C,$37,$E0;{$ENDIF} 

           {$IFDEF FPC}vmovapd [eax + esi - 128], ymm0;{$ELSE}db $C5,$FD,$29,$44,$30,$80;{$ENDIF} 
           {$IFDEF FPC}vmovapd [eax + esi - 96], ymm1;{$ELSE}db $C5,$FD,$29,$4C,$30,$A0;{$ENDIF} 
           {$IFDEF FPC}vmovapd [eax + esi - 64], ymm2;{$ELSE}db $C5,$FD,$29,$54,$30,$C0;{$ENDIF} 
           {$IFDEF FPC}vmovapd [eax + esi - 32], ymm3;{$ELSE}db $C5,$FD,$29,$5C,$30,$E0;{$ENDIF} 
       jmp @addforxloop

       @loopEnd:

       sub esi, 128;

       jz @nextLine;

       @addforxloop2:
           add esi, 16;
           jg @loopEnd2;

           {$IFDEF FPC}vmovapd xmm0, [ecx + esi - 16];{$ELSE}db $C5,$F9,$28,$44,$31,$F0;{$ENDIF} 
           {$IFDEF FPC}vsubpd xmm0, xmm0, [edi + esi - 16];{$ELSE}db $C5,$F9,$5C,$44,$37,$F0;{$ENDIF} 

           {$IFDEF FPC}vmovapd [eax + esi - 16], xmm0;{$ELSE}db $C5,$F9,$29,$44,$30,$F0;{$ENDIF} 
       jmp @addforxloop2;

       @loopEnd2:

       sub esi, 16;
       jz @nextLine;

       {$IFDEF FPC}vmovsd xmm0, [ecx - 8];{$ELSE}db $C5,$FB,$10,$41,$F8;{$ENDIF} 
       {$IFDEF FPC}vmovsd xmm1, [edi - 8];{$ELSE}db $C5,$FB,$10,$4F,$F8;{$ENDIF} 
       {$IFDEF FPC}vsubsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$5C,$C1;{$ENDIF} 
       {$IFDEF FPC}vmovsd [eax - 8], xmm0;{$ELSE}db $C5,$FB,$11,$40,$F8;{$ENDIF} 

       @nextLine:

       // update pointers
       add ecx, LineWidth1;
       add edi, LineWidth2;
       add eax, edx;

   // loop y end
   dec Height;
   jnz @@addforyloop;

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop edi;
   pop esi;
   pop ebx;
end;

procedure AVXMatrixSubUnAligned(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width : NativeInt; height : NativeInt; const LineWidth1, LineWidth2 : NativeInt);
asm
   push ebx;
   push esi;
   push edi;

   //iters := -width*sizeof(double);
   mov ebx, width;
   imul ebx, -8;

   // helper registers for the mt1, mt2 and dest pointers
   mov edi, mt2;

   sub ecx, ebx;
   sub edi, ebx;
   sub eax, ebx;

   // for y := 0 to height - 1:
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov esi, ebx;
       @addforxloop:
           add esi, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [esi + esi];
           // prefetch [edi + esi];

           // addition:
           {$IFDEF FPC}vmovupd ymm0, [ecx + esi - 128];{$ELSE}db $C5,$FD,$10,$44,$31,$80;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm1, [edi + esi - 128];{$ELSE}db $C5,$FD,$10,$4C,$37,$80;{$ENDIF} 
           {$IFDEF FPC}vsubpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$5C,$C1;{$ENDIF} 
           {$IFDEF FPC}vmovupd [eax + esi - 128], ymm0;{$ELSE}db $C5,$FD,$11,$44,$30,$80;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm0, [ecx + esi - 96];{$ELSE}db $C5,$FD,$10,$44,$31,$A0;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm1, [edi + esi - 96];{$ELSE}db $C5,$FD,$10,$4C,$37,$A0;{$ENDIF} 
           {$IFDEF FPC}vsubpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$5C,$C1;{$ENDIF} 
           {$IFDEF FPC}vmovupd [eax + esi - 96], ymm0;{$ELSE}db $C5,$FD,$11,$44,$30,$A0;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm0, [ecx + esi - 64];{$ELSE}db $C5,$FD,$10,$44,$31,$C0;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm1, [edi + esi - 64];{$ELSE}db $C5,$FD,$10,$4C,$37,$C0;{$ENDIF} 
           {$IFDEF FPC}vsubpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$5C,$C1;{$ENDIF} 
           {$IFDEF FPC}vmovupd [eax + esi - 64], ymm0;{$ELSE}db $C5,$FD,$11,$44,$30,$C0;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm0, [ecx + esi - 32];{$ELSE}db $C5,$FD,$10,$44,$31,$E0;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm1, [edi + esi - 32];{$ELSE}db $C5,$FD,$10,$4C,$37,$E0;{$ENDIF} 
           {$IFDEF FPC}vsubpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$5C,$C1;{$ENDIF} 
           {$IFDEF FPC}vmovupd [eax + esi - 32], ymm0;{$ELSE}db $C5,$FD,$11,$44,$30,$E0;{$ENDIF} 

       jmp @addforxloop

       @loopEnd:

       sub esi, 128;

       jz @nextLine;

       @addforxloop2:
           add esi, 16;
           jg @loopEnd2;

           {$IFDEF FPC}vmovupd xmm0, [ecx + esi - 16];{$ELSE}db $C5,$F9,$10,$44,$31,$F0;{$ENDIF} 
           {$IFDEF FPC}vmovupd xmm1, [edi + esi - 16];{$ELSE}db $C5,$F9,$10,$4C,$37,$F0;{$ENDIF} 
           {$IFDEF FPC}vsubpd xmm0, xmm0, xmm1;{$ELSE}db $C5,$F9,$5C,$C1;{$ENDIF} 

           {$IFDEF FPC}vmovupd [eax + esi - 16], xmm0;{$ELSE}db $C5,$F9,$11,$44,$30,$F0;{$ENDIF} 
       jmp @addforxloop2;

       @loopEnd2:

       sub esi, 16;
       jz @nextLine;

       {$IFDEF FPC}vmovsd xmm0, [ecx - 8];{$ELSE}db $C5,$FB,$10,$41,$F8;{$ENDIF} 
       {$IFDEF FPC}vmovsd xmm1, [edi - 8];{$ELSE}db $C5,$FB,$10,$4F,$F8;{$ENDIF} 
       {$IFDEF FPC}vsubsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$5C,$C1;{$ENDIF} 
       {$IFDEF FPC}vmovsd [eax - 8], xmm0;{$ELSE}db $C5,$FB,$11,$40,$F8;{$ENDIF} 

       @nextLine:

       // next line:
       add ecx, LineWidth1;
       add edi, LineWidth2;
       add eax, edx;

   // loop y end
   dec height;
   jnz @@addforyloop;

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop edi;
   pop esi;
   pop ebx;
end;

procedure AVXMatrixSubT(A : PDouble; LineWidthA : NativeInt; B : PDouble; LineWidthB : NativeInt; width, height : NativeInt);
// eax = A, edx = LineWidthA, ecx = B
var aLineWidthA : NativeInt;
asm
   push esi;
   push edi;
   push ebx;

   mov aLineWidthA, edx;

   // eax: iter := -width*sizeof(double)
   mov edx, width;
   imul edx, -8;
   sub eax, edx;

   mov esi, LineWidthB;

   // for y := 0 to height - 1
   @@foryloop:
      mov edi, ecx;
      mov ebx, edx;

      // for x := 0 to width - 1
      @@forxloop:
         {$IFDEF FPC}vmovsd xmm0, [eax + ebx];{$ELSE}db $C5,$FB,$10,$04,$18;{$ENDIF} 
         {$IFDEF FPC}vmovsd xmm1, [edi];{$ELSE}db $C5,$FB,$10,$0F;{$ENDIF} 
         {$IFDEF FPC}vsubsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$5C,$C1;{$ENDIF} 
         {$IFDEF FPC}vmovsd [eax + ebx], xmm0;{$ELSE}db $C5,$FB,$11,$04,$18;{$ENDIF} 

         add edi, esi;
      add ebx, 8;
      jnz @@forxloop;

      add eax, aLineWidthA;
      add ecx, 8;
   dec height;
   jnz @@foryloop;

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop ebx;
   pop edi;
   pop esi;
end;

// ########################################################
// #### Matrix add, sub to vector operations
// ########################################################

procedure AVXMatrixSubVecAlignedVecRow(A : PDouble; LineWidthA : NativeInt; B : PDouble; incX : NativeInt; width, Height : NativeInt);
// eax = A, edx = LineWidthA, ecx = B
asm
   push ebx;
   push esi;

   mov ebx, width;
   imul ebx, -8;

   sub eax, ebx;
   sub ecx, ebx;

   @@foryloop:
      mov esi, ebx;

      @@forxloopUnrolled:
         add esi, 64;
         jg @@EndLoop1;

         {$IFDEF FPC}vmovapd ymm0, [eax + esi - 64];{$ELSE}db $C5,$FD,$28,$44,$30,$C0;{$ENDIF} 
         {$IFDEF FPC}vsubpd ymm0, ymm0, [ecx + esi - 64];{$ELSE}db $C5,$FD,$5C,$44,$31,$C0;{$ENDIF} 
         {$IFDEF FPC}vmovapd [eax + esi - 64], ymm0;{$ELSE}db $C5,$FD,$29,$44,$30,$C0;{$ENDIF} 

         {$IFDEF FPC}vmovapd ymm2, [eax + esi - 32];{$ELSE}db $C5,$FD,$28,$54,$30,$E0;{$ENDIF} 
         {$IFDEF FPC}vsubpd ymm2, ymm2, [ecx + esi - 32];{$ELSE}db $C5,$ED,$5C,$54,$31,$E0;{$ENDIF} 
         {$IFDEF FPC}vmovapd [eax + esi - 32], ymm2;{$ELSE}db $C5,$FD,$29,$54,$30,$E0;{$ENDIF} 

      jmp @@forxloopUnrolled;

      @@EndLoop1:

      sub esi, 64;

      jz @NextLine;

      @@forxloop:
         {$IFDEF FPC}vmovsd xmm0, [eax + esi];{$ELSE}db $C5,$FB,$10,$04,$30;{$ENDIF} 
         {$IFDEF FPC}vmovsd xmm1, [ecx + esi];{$ELSE}db $C5,$FB,$10,$0C,$31;{$ENDIF} 

         {$IFDEF FPC}vsubsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$5C,$C1;{$ENDIF} 
         {$IFDEF FPC}vmovsd [eax + esi], xmm0;{$ELSE}db $C5,$FB,$11,$04,$30;{$ENDIF} 

         add esi, 8;
      jnz @@forxloop;

      @NextLine:

      add eax, edx;
   dec Height;
   jnz @@foryloop;

   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop ebx;
end;

procedure AVXMatrixSubVecAlignedRow(A : PDouble; LineWidthA : NativeInt; B : PDouble; incX : NativeInt; width, Height : NativeInt);
var vecIter : integer;
    aLineWidthA : NativeInt;
asm
   push ebx;
   push edi;
   push esi;

   mov aLineWidthA, edx;
   mov edi, incx;
   mov esi, width;

   imul esi, edi;
   imul esi, -1;
   mov vecIter, esi;

   mov ebx, width;
   imul ebx, -8;

   sub eax, ebx;
   sub ecx, esi;

   @@foryloop:
      mov edx, ebx;
      mov esi, vecIter;

      @@forxloop:
         {$IFDEF FPC}vmovsd xmm0, [eax + edx];{$ELSE}db $C5,$FB,$10,$04,$10;{$ENDIF} 
         {$IFDEF FPC}vmovsd xmm1, [ecx + esi];{$ELSE}db $C5,$FB,$10,$0C,$31;{$ENDIF} 

         {$IFDEF FPC}vsubsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$5C,$C1;{$ENDIF} 
         {$IFDEF FPC}vmovsd [eax + edx], xmm0;{$ELSE}db $C5,$FB,$11,$04,$10;{$ENDIF} 

         add esi, edi;
         add edx, 8;
      jnz @@forxloop;

      @NextLine:

      add eax, aLineWidthA;
   dec Height;
   jnz @@foryloop;

   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;

procedure AVXMatrixSubVecAlignedCol(A : PDouble; LineWidthA : NativeInt; B : PDouble; incX : NativeInt; width, Height : NativeInt);
asm
   push ebx;
   push esi;

   mov ebx, width;
   imul ebx, -8;

   sub eax, ebx;

   @@foryloop:
      mov esi, ebx;

      {$IFDEF FPC}vbroadcastsd ymm1, [ecx];{$ELSE}db $C4,$E2,$7D,$19,$09;{$ENDIF} 

      @@forxloopUnrolled:
         add esi, 64;
         jg @@EndLoop1;

         {$IFDEF FPC}vmovapd ymm0, [eax + esi - 64];{$ELSE}db $C5,$FD,$28,$44,$30,$C0;{$ENDIF} 
         {$IFDEF FPC}vsubpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$5C,$C1;{$ENDIF} 
         {$IFDEF FPC}vmovapd [eax + esi - 64], ymm0;{$ELSE}db $C5,$FD,$29,$44,$30,$C0;{$ENDIF} 

         {$IFDEF FPC}vmovapd ymm2, [eax + esi - 32];{$ELSE}db $C5,$FD,$28,$54,$30,$E0;{$ENDIF} 
         {$IFDEF FPC}vsubpd ymm2, ymm2, ymm1;{$ELSE}db $C5,$ED,$5C,$D1;{$ENDIF} 
         {$IFDEF FPC}vmovapd [eax + esi - 32], ymm2;{$ELSE}db $C5,$FD,$29,$54,$30,$E0;{$ENDIF} 

      jmp @@forxloopUnrolled;

      @@EndLoop1:

      sub esi, 64;

      jz @NextLine;

      @@forxloop:
         {$IFDEF FPC}vmovsd xmm0, [eax + esi];{$ELSE}db $C5,$FB,$10,$04,$30;{$ENDIF} 

         {$IFDEF FPC}vsubsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$5C,$C1;{$ENDIF} 
         {$IFDEF FPC}vmovsd [eax + esi], xmm0;{$ELSE}db $C5,$FB,$11,$04,$30;{$ENDIF} 

         add esi, 8;
      jnz @@forxloop;

      @NextLine:

      add eax, edx;
      add ecx, incX;
   dec Height;
   jnz @@foryloop;

   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop ebx;
end;

procedure AVXMatrixSubVecUnalignedVecRow(A : PDouble; LineWidthA : NativeInt; B : PDouble; incX : NativeInt; width, Height : NativeInt);
asm
   push ebx;
   push esi;

   mov ebx, width;
   imul ebx, -8;

   sub eax, ebx;
   sub ecx, ebx;

   @@foryloop:
      mov esi, ebx;

      @@forxloopUnrolled:
         add esi, 64;
         jg @@EndLoop1;

         {$IFDEF FPC}vmovupd ymm0, [eax + esi - 64];{$ELSE}db $C5,$FD,$10,$44,$30,$C0;{$ENDIF} 
         {$IFDEF FPC}vmovupd ymm1, [ecx + esi - 64];{$ELSE}db $C5,$FD,$10,$4C,$31,$C0;{$ENDIF} 
         {$IFDEF FPC}vsubpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$5C,$C1;{$ENDIF} 
         {$IFDEF FPC}vmovupd [eax + esi - 64], ymm0;{$ELSE}db $C5,$FD,$11,$44,$30,$C0;{$ENDIF} 

         {$IFDEF FPC}vmovupd ymm2, [eax + esi - 32];{$ELSE}db $C5,$FD,$10,$54,$30,$E0;{$ENDIF} 
         {$IFDEF FPC}vmovupd ymm3, [ecx + esi - 32];{$ELSE}db $C5,$FD,$10,$5C,$31,$E0;{$ENDIF} 
         {$IFDEF FPC}vsubpd ymm2, ymm2, ymm3;{$ELSE}db $C5,$ED,$5C,$D3;{$ENDIF} 
         {$IFDEF FPC}vmovupd [eax + esi - 32], ymm2;{$ELSE}db $C5,$FD,$11,$54,$30,$E0;{$ENDIF} 

      jmp @@forxloopUnrolled;

      @@EndLoop1:

      sub esi, 64;

      jz @NextLine;

      @@forxloop:
         {$IFDEF FPC}vmovsd xmm0, [eax + esi];{$ELSE}db $C5,$FB,$10,$04,$30;{$ENDIF} 
         {$IFDEF FPC}vmovsd xmm1, [ecx + esi];{$ELSE}db $C5,$FB,$10,$0C,$31;{$ENDIF} 

         {$IFDEF FPC}vsubsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$5C,$C1;{$ENDIF} 
         {$IFDEF FPC}vmovsd [eax + esi], xmm0;{$ELSE}db $C5,$FB,$11,$04,$30;{$ENDIF} 

         add esi, 8;
      jnz @@forxloop;

      @NextLine:

      add eax, edx;
   dec Height;
   jnz @@foryloop;

   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop ebx;
end;

procedure AVXMatrixSubVecUnalignedRow(A : PDouble; LineWidthA : NativeInt; B : PDouble; incX : NativeInt; width, Height : NativeInt);
var vecIter : integer;
    aLineWidthA : NativeInt;
asm
   push ebx;
   push edi;
   push esi;

   mov aLineWidthA, edx;
   mov edi, incx;
   mov esi, width;

   imul esi, edi;
   imul esi, -1;
   mov vecIter, esi;

   mov ebx, width;
   imul ebx, -8;

   sub eax, ebx;
   sub ecx, esi;

   @@foryloop:
      mov edx, ebx;
      mov esi, vecIter;

      @@forxloop:
         {$IFDEF FPC}vmovsd xmm0, [eax + edx];{$ELSE}db $C5,$FB,$10,$04,$10;{$ENDIF} 
         {$IFDEF FPC}vmovsd xmm1, [ecx + esi];{$ELSE}db $C5,$FB,$10,$0C,$31;{$ENDIF} 

         {$IFDEF FPC}vsubsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$5C,$C1;{$ENDIF} 
         {$IFDEF FPC}vmovsd [eax + edx], xmm0;{$ELSE}db $C5,$FB,$11,$04,$10;{$ENDIF} 

         add esi, edi;
         add edx, 8;
      jnz @@forxloop;

      add eax, aLineWidthA;
   dec Height;
   jnz @@foryloop;

   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;

procedure AVXMatrixSubVecUnalignedCol(A : PDouble; LineWidthA : NativeInt; B : PDouble; incX : NativeInt; width, Height : NativeInt);
asm
   push ebx;
   push esi;

   mov ebx, width;
   imul ebx, -8;

   sub eax, ebx;

   @@foryloop:
      mov esi, ebx;

      {$IFDEF FPC}vbroadcastsd ymm1, [ecx];{$ELSE}db $C4,$E2,$7D,$19,$09;{$ENDIF} 

      @@forxloopUnrolled:
         add esi, 64;
         jg @@EndLoop1;

         {$IFDEF FPC}vmovupd ymm0, [eax + esi - 64];{$ELSE}db $C5,$FD,$10,$44,$30,$C0;{$ENDIF} 
         {$IFDEF FPC}vsubpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$5C,$C1;{$ENDIF} 
         {$IFDEF FPC}vmovupd [eax + esi - 64], ymm0;{$ELSE}db $C5,$FD,$11,$44,$30,$C0;{$ENDIF} 

         {$IFDEF FPC}vmovupd ymm2, [eax + esi - 32];{$ELSE}db $C5,$FD,$10,$54,$30,$E0;{$ENDIF} 
         {$IFDEF FPC}vsubpd ymm2, ymm2, ymm1;{$ELSE}db $C5,$ED,$5C,$D1;{$ENDIF} 
         {$IFDEF FPC}vmovupd [eax + esi - 32], ymm2;{$ELSE}db $C5,$FD,$11,$54,$30,$E0;{$ENDIF} 

      jmp @@forxloopUnrolled;

      @@EndLoop1:

      sub esi, 64;

      jz @NextLine;

      @@forxloop:
         {$IFDEF FPC}vmovsd xmm0, [eax + esi];{$ELSE}db $C5,$FB,$10,$04,$30;{$ENDIF} 

         {$IFDEF FPC}vsubsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$5C,$C1;{$ENDIF} 
         {$IFDEF FPC}vmovsd [eax + esi], xmm0;{$ELSE}db $C5,$FB,$11,$04,$30;{$ENDIF} 

         add esi, 8;
      jnz @@forxloop;

      @NextLine:

      add eax, edx;
      add ecx, incX;
   dec Height;
   jnz @@foryloop;

   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop ebx;
end;


procedure AVXMatrixAddVecAlignedVecRow(A : PDouble; LineWidthA : NativeInt; B : PDouble; incX : NativeInt; width, Height : NativeInt);
asm
   push ebx;
   push esi;

   mov ebx, width;
   imul ebx, -8;

   sub eax, ebx;
   sub ecx, ebx;

   @@foryloop:
      mov esi, ebx;

      @@forxloopUnrolled:
         add esi, 64;
         jg @@EndLoop1;

         {$IFDEF FPC}vmovapd ymm0, [eax + esi - 64];{$ELSE}db $C5,$FD,$28,$44,$30,$C0;{$ENDIF} 
         {$IFDEF FPC}vaddpd ymm0, ymm0, [ecx + esi - 64];{$ELSE}db $C5,$FD,$58,$44,$31,$C0;{$ENDIF} 
         {$IFDEF FPC}vmovapd [eax + esi - 64], ymm0;{$ELSE}db $C5,$FD,$29,$44,$30,$C0;{$ENDIF} 

         {$IFDEF FPC}vmovapd ymm2, [eax + esi - 32];{$ELSE}db $C5,$FD,$28,$54,$30,$E0;{$ENDIF} 
         {$IFDEF FPC}vaddpd ymm2, ymm2, [ecx + esi - 32];{$ELSE}db $C5,$ED,$58,$54,$31,$E0;{$ENDIF} 
         {$IFDEF FPC}vmovapd [eax + esi - 32], ymm2;{$ELSE}db $C5,$FD,$29,$54,$30,$E0;{$ENDIF} 

      jmp @@forxloopUnrolled;

      @@EndLoop1:

      sub esi, 64;

      jz @NextLine;

      @@forxloop:
         {$IFDEF FPC}vmovsd xmm0, [eax + esi];{$ELSE}db $C5,$FB,$10,$04,$30;{$ENDIF} 
         {$IFDEF FPC}vmovsd xmm1, [ecx + esi];{$ELSE}db $C5,$FB,$10,$0C,$31;{$ENDIF} 

         {$IFDEF FPC}vaddsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$58,$C1;{$ENDIF} 
         {$IFDEF FPC}vmovsd [eax + esi], xmm0;{$ELSE}db $C5,$FB,$11,$04,$30;{$ENDIF} 

         add esi, 8;
      jnz @@forxloop;

      @NextLine:

      add eax, edx;
   dec Height;
   jnz @@foryloop;

   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop ebx;
end;

procedure AVXMatrixAddVecAlignedRow(A : PDouble; LineWidthA : NativeInt; B : PDouble; incX : NativeInt; width, Height : NativeInt);
var vecIter : integer;
    aLineWidthA : NativeInt;
asm
   push ebx;
   push edi;
   push esi;

   mov aLineWidthA, edx;
   mov edi, incx;
   mov esi, width;

   imul esi, edi;
   imul esi, -1;
   mov vecIter, esi;

   mov ebx, width;
   imul ebx, -8;

   sub eax, ebx;
   sub ecx, esi;

   @@foryloop:
      mov edx, ebx;
      mov esi, vecIter;

      @@forxloop:
         {$IFDEF FPC}vmovsd xmm0, [eax + edx];{$ELSE}db $C5,$FB,$10,$04,$10;{$ENDIF} 
         {$IFDEF FPC}vmovsd xmm1, [ecx + esi];{$ELSE}db $C5,$FB,$10,$0C,$31;{$ENDIF} 

         {$IFDEF FPC}vaddsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$58,$C1;{$ENDIF} 
         {$IFDEF FPC}vmovsd [eax + edx], xmm0;{$ELSE}db $C5,$FB,$11,$04,$10;{$ENDIF} 

         add esi, edi;
         add edx, 8;
      jnz @@forxloop;

      add eax, aLineWidthA;
   dec Height;
   jnz @@foryloop;

   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;

procedure AVXMatrixAddVecAlignedCol(A : PDouble; LineWidthA : NativeInt; B : PDouble; incX : NativeInt; width, Height : NativeInt);
asm
   push ebx;
   push esi;

   mov ebx, width;
   imul ebx, -8;

   sub eax, ebx;

   @@foryloop:
      mov esi, ebx;

      {$IFDEF FPC}vbroadcastsd ymm1, [ecx];{$ELSE}db $C4,$E2,$7D,$19,$09;{$ENDIF} 

      @@forxloopUnrolled:
         add esi, 64;
         jg @@EndLoop1;

         {$IFDEF FPC}vmovapd ymm0, [eax + esi - 64];{$ELSE}db $C5,$FD,$28,$44,$30,$C0;{$ENDIF} 
         {$IFDEF FPC}vaddpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 
         {$IFDEF FPC}vmovapd [eax + esi - 64], ymm0;{$ELSE}db $C5,$FD,$29,$44,$30,$C0;{$ENDIF} 

         {$IFDEF FPC}vmovapd ymm2, [eax + esi - 32];{$ELSE}db $C5,$FD,$28,$54,$30,$E0;{$ENDIF} 
         {$IFDEF FPC}vaddpd ymm2, ymm2, ymm1;{$ELSE}db $C5,$ED,$58,$D1;{$ENDIF} 
         {$IFDEF FPC}vmovapd [eax + esi - 32], ymm2;{$ELSE}db $C5,$FD,$29,$54,$30,$E0;{$ENDIF} 

      jmp @@forxloopUnrolled;

      @@EndLoop1:

      sub esi, 64;

      jz @NextLine;

      @@forxloop:
         {$IFDEF FPC}vmovsd xmm0, [eax + esi];{$ELSE}db $C5,$FB,$10,$04,$30;{$ENDIF} 

         {$IFDEF FPC}vaddsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$58,$C1;{$ENDIF} 
         {$IFDEF FPC}vmovsd [eax + esi], xmm0;{$ELSE}db $C5,$FB,$11,$04,$30;{$ENDIF} 

         add esi, 8;
      jnz @@forxloop;

      @NextLine:

      add eax, edx;
      add ecx, incX;
   dec Height;
   jnz @@foryloop;

   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop ebx;
end;

procedure AVXMatrixAddVecUnalignedVecRow(A : PDouble; LineWidthA : NativeInt; B : PDouble; incX : NativeInt; width, Height : NativeInt);
asm
   push ebx;
   push esi;

   mov ebx, width;
   imul ebx, -8;

   sub eax, ebx;
   sub ecx, ebx;

   @@foryloop:
      mov esi, ebx;

      @@forxloopUnrolled:
         add esi, 64;
         jg @@EndLoop1;

         {$IFDEF FPC}vmovupd ymm0, [eax + esi - 64];{$ELSE}db $C5,$FD,$10,$44,$30,$C0;{$ENDIF} 
         {$IFDEF FPC}vmovupd ymm1, [ecx + esi - 64];{$ELSE}db $C5,$FD,$10,$4C,$31,$C0;{$ENDIF} 
         {$IFDEF FPC}vaddpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 
         {$IFDEF FPC}vmovupd [eax + esi - 64], ymm0;{$ELSE}db $C5,$FD,$11,$44,$30,$C0;{$ENDIF} 

         {$IFDEF FPC}vmovupd ymm2, [eax + esi - 32];{$ELSE}db $C5,$FD,$10,$54,$30,$E0;{$ENDIF} 
         {$IFDEF FPC}vmovupd ymm3, [ecx + esi - 32];{$ELSE}db $C5,$FD,$10,$5C,$31,$E0;{$ENDIF} 
         {$IFDEF FPC}vaddpd ymm2, ymm2, ymm3;{$ELSE}db $C5,$ED,$58,$D3;{$ENDIF} 
         {$IFDEF FPC}vmovupd [eax + esi - 32], ymm2;{$ELSE}db $C5,$FD,$11,$54,$30,$E0;{$ENDIF} 

      jmp @@forxloopUnrolled;

      @@EndLoop1:

      sub esi, 64;

      jz @NextLine;

      @@forxloop:
         {$IFDEF FPC}vmovsd xmm0, [eax + esi];{$ELSE}db $C5,$FB,$10,$04,$30;{$ENDIF} 
         {$IFDEF FPC}vmovsd xmm1, [ecx + esi];{$ELSE}db $C5,$FB,$10,$0C,$31;{$ENDIF} 

         {$IFDEF FPC}vaddsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$58,$C1;{$ENDIF} 
         {$IFDEF FPC}vmovsd [eax + esi], xmm0;{$ELSE}db $C5,$FB,$11,$04,$30;{$ENDIF} 

         add esi, 8;
      jnz @@forxloop;

      @NextLine:

      add eax, edx;
   dec Height;
   jnz @@foryloop;

   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop ebx;
end;

procedure AVXMatrixAddVecUnalignedRow(A : PDouble; LineWidthA : NativeInt; B : PDouble; incX : NativeInt; width, Height : NativeInt);
var vecIter : integer;
    aLineWidthA : NativeInt;
asm
   push ebx;
   push edi;
   push esi;

   mov aLineWidthA, edx;
   mov edi, incx;
   mov esi, width;

   imul esi, edi;
   imul esi, -1;
   mov vecIter, esi;

   mov ebx, width;
   imul ebx, -8;

   sub eax, ebx;
   sub ecx, esi;

   @@foryloop:
      mov edx, ebx;
      mov esi, vecIter;

      @@forxloop:
         {$IFDEF FPC}vmovsd xmm0, [eax + edx];{$ELSE}db $C5,$FB,$10,$04,$10;{$ENDIF} 
         {$IFDEF FPC}vmovsd xmm1, [ecx + esi];{$ELSE}db $C5,$FB,$10,$0C,$31;{$ENDIF} 

         {$IFDEF FPC}vaddsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$58,$C1;{$ENDIF} 
         {$IFDEF FPC}vmovsd [eax + edx], xmm0;{$ELSE}db $C5,$FB,$11,$04,$10;{$ENDIF} 

         add esi, edi;
         add edx, 8;
      jnz @@forxloop;

      add eax, aLineWidthA;
   dec Height;
   jnz @@foryloop;

   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;

procedure AVXMatrixAddVecUnalignedCol(A : PDouble; LineWidthA : NativeInt; B : PDouble; incX : NativeInt; width, Height : NativeInt);
asm
   push ebx;
   push esi;

   mov ebx, width;
   imul ebx, -8;

   sub eax, ebx;

   @@foryloop:
      mov esi, ebx;

      {$IFDEF FPC}vbroadcastsd ymm1, [ecx];{$ELSE}db $C4,$E2,$7D,$19,$09;{$ENDIF} 

      @@forxloopUnrolled:
         add esi, 64;
         jg @@EndLoop1;

         {$IFDEF FPC}vmovupd ymm0, [eax + esi - 64];{$ELSE}db $C5,$FD,$10,$44,$30,$C0;{$ENDIF} 
         {$IFDEF FPC}vaddpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 
         {$IFDEF FPC}vmovupd [eax + esi - 64], ymm0;{$ELSE}db $C5,$FD,$11,$44,$30,$C0;{$ENDIF} 

         {$IFDEF FPC}vmovupd ymm2, [eax + esi - 32];{$ELSE}db $C5,$FD,$10,$54,$30,$E0;{$ENDIF} 
         {$IFDEF FPC}vaddpd ymm2, ymm2, ymm1;{$ELSE}db $C5,$ED,$58,$D1;{$ENDIF} 
         {$IFDEF FPC}vmovupd [eax + esi - 32], ymm2;{$ELSE}db $C5,$FD,$11,$54,$30,$E0;{$ENDIF} 

      jmp @@forxloopUnrolled;

      @@EndLoop1:

      sub esi, 64;

      jz @NextLine;

      @@forxloop:
         {$IFDEF FPC}vmovsd xmm0, [eax + esi];{$ELSE}db $C5,$FB,$10,$04,$30;{$ENDIF} 

         {$IFDEF FPC}vaddsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$58,$C1;{$ENDIF} 
         {$IFDEF FPC}vmovsd [eax + esi], xmm0;{$ELSE}db $C5,$FB,$11,$04,$30;{$ENDIF} 

         add esi, 8;
      jnz @@forxloop;

      @NextLine:

      add eax, edx;
      add ecx, incX;
   dec Height;
   jnz @@foryloop;

   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop ebx;
end;

{$ENDIF}

end.
