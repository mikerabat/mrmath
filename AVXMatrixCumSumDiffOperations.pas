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

unit AVXMatrixCumSumDiffOperations;

interface


{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}
{$IFNDEF x64}

uses MatrixConst;

procedure AVXMatrixCumulativeSumRow(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

procedure AVXMatrixCumulativeSumColumnUnaligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure AVXMatrixCumulativeSumColumnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

procedure AVXMatrixDifferentiateRow(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

procedure AVXMatrixDifferentiateColumnUnaligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure AVXMatrixDifferentiateColumnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

{$ENDIF}

implementation

{$IFNDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$S-} {$ENDIF}

// ##################################################
// #### Cumulative sum
// ##################################################

procedure AVXMatrixCumulativeSumRow(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
asm
   // prolog - maintain stack
   push ebx;
   push edi;
   push esi;

   // if (width <= 0) or (height <= 0) then exit;
   mov edi, width;
   cmp edi, 0;
   jle @@exitproc;
   mov esi, height;
   cmp esi, 0;
   jle @@exitproc;

   // iter := -width*sizeof(Double)
   imul edi, -8;

   // prepare counters
   sub eax, edi;
   sub ecx, edi;

   mov esi, height;
   @@foryloop:
      mov ebx, edi;

      {$IFDEF FPC}vxorpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$57,$C0;{$ENDIF} 
      @@forxloop:
         {$IFDEF FPC}vmovsd xmm1, [ecx + ebx];{$ELSE}db $C5,$FB,$10,$0C,$19;{$ENDIF} 
         {$IFDEF FPC}vaddsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$58,$C1;{$ENDIF} 
         {$IFDEF FPC}vmovsd [eax + ebx], xmm0;{$ELSE}db $C5,$FB,$11,$04,$18;{$ENDIF} 
      add ebx, 8;
      jnz @@forxloop;

      add eax, edx;
      add ecx, srcLineWidth;
   dec esi;
   jnz @@foryloop;
   @@exitProc:

   // epilog - stack cleanup
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;

procedure AVXMatrixCumulativeSumColumnUnaligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
asm
   // prolog - maintain stack
   push edi;
   push esi;
   push ebx;

   // if (width <= 1) or (height <= 0) then exit;
   mov ebx, height;
   cmp ebx, 0;

   jle @@exitproc;

   mov eax, dest;
   mov ecx, src;

   sub width, 4;
   jl @@lastColumns;

   @@forxloop:
       mov ebx, height;
       {$IFDEF FPC}vxorpd ymm0, ymm0, ymm0;{$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} 
       xor edi, edi;
       xor esi, esi;

       // 4 values at once
       @@foryloop:
           {$IFDEF FPC}vmovupd ymm1, [ecx + edi];{$ELSE}db $C5,$FD,$10,$0C,$39;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 
           {$IFDEF FPC}vmovupd [eax + esi], ymm0;{$ELSE}db $C5,$FD,$11,$04,$30;{$ENDIF} 

           add edi, srcLineWidth;
           add esi, edx;
       dec ebx;
       jnz @@foryloop;

       add ecx, 32;
       add eax, 32;
   sub width, 4;
   jge @@forxloop;

   @@lastColumns:
   add width, 4;
   jz @@exitProc;

   @@forxloopshort:
       mov ebx, height;
       {$IFDEF FPC}vxorpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$57,$C0;{$ENDIF} 
       xor edi, edi;
       xor esi, esi;

       // 4 values at once
       @@foryloopshort:
           {$IFDEF FPC}vmovsd xmm1, [ecx + edi];{$ELSE}db $C5,$FB,$10,$0C,$39;{$ENDIF} 
           {$IFDEF FPC}vaddsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$58,$C1;{$ENDIF} 
           {$IFDEF FPC}vmovsd [eax + esi], xmm0;{$ELSE}db $C5,$FB,$11,$04,$30;{$ENDIF} 

           add edi, srcLineWidth;
           add esi, edx;
       dec ebx;
       jnz @@foryloopshort;

       add ecx, 8;
       add eax, 8;
   sub width, 1;
   jg @@forxloopshort;

   @@exitProc:

   // epilog - stack cleanup
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop ebx;
   pop esi;
   pop edi;
end;

procedure AVXMatrixCumulativeSumColumnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
asm
   // prolog - maintain stack
   push edi;
   push esi;
   push ebx;

   // if (width <= 1) or (height <= 0) then exit;
   mov ebx, height;
   cmp ebx, 0;

   jle @@exitproc;

   mov eax, dest;
   mov ecx, src;

   sub width, 4;
   jl @@lastColumns;

   @@forxloop:
       mov ebx, height;
       {$IFDEF FPC}vxorpd ymm0, ymm0, ymm0;{$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} 
       xor edi, edi;
       xor esi, esi;

       // 4 values at once
       @@foryloop:
           {$IFDEF FPC}vmovapd ymm1, [ecx + edi];{$ELSE}db $C5,$FD,$28,$0C,$39;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 
           {$IFDEF FPC}vmovapd [eax + esi], ymm0;{$ELSE}db $C5,$FD,$29,$04,$30;{$ENDIF} 

           add edi, srcLineWidth;
           add esi, edx;
       dec ebx;
       jnz @@foryloop;

       add ecx, 32;
       add eax, 32;
   sub width, 4;
   jge @@forxloop;

   @@lastColumns:
   add width, 4;
   jz @@exitProc;

   @@forxloopshort:
       mov ebx, height;
       {$IFDEF FPC}vxorpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$57,$C0;{$ENDIF} 
       xor edi, edi;
       xor esi, esi;

       // 4 values at once
       @@foryloopshort:
           {$IFDEF FPC}vmovsd xmm1, [ecx + edi];{$ELSE}db $C5,$FB,$10,$0C,$39;{$ENDIF} 
           {$IFDEF FPC}vaddsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$58,$C1;{$ENDIF} 
           {$IFDEF FPC}vmovsd [eax + esi], xmm0;{$ELSE}db $C5,$FB,$11,$04,$30;{$ENDIF} 

           add edi, srcLineWidth;
           add esi, edx;
       dec ebx;
       jnz @@foryloopshort;

       add ecx, 8;
       add eax, 8;
   sub width, 1;
   jg @@forxloopshort;

   @@exitProc:

   // epilog - stack cleanup
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop ebx;
   pop esi;
   pop edi;
end;

// ###############################################
// #### Differentiate
// ###############################################

procedure AVXMatrixDifferentiateRow(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
asm
   // prolog - maintain stack
   push ebx;
   push edi;
   push esi;

   // if (width <= 1) or (height <= 0) then exit;
   mov ebx, width;
   cmp ebx, 1;
   jle @@exitproc;
   mov esi, height;
   cmp esi, 0;
   jle @@exitproc;

   // iter := -width*sizeof(Double)
   imul ebx, -8;

   mov edi, srcLineWidth;

   // prepare counters
   sub eax, ebx;
   sub ecx, ebx;

   add ebx, 8;
   @@foryloop:
       mov esi, ebx;
       {$IFDEF FPC}vmovsd xmm1, [ecx + esi - 8];{$ELSE}db $C5,$FB,$10,$4C,$31,$F8;{$ENDIF} 
       @@forxloop:
           {$IFDEF FPC}vmovsd xmm0, [ecx + esi];{$ELSE}db $C5,$FB,$10,$04,$31;{$ENDIF} 
           {$IFDEF FPC}vsubsd xmm2, xmm0, xmm1;{$ELSE}db $C5,$FB,$5C,$D1;{$ENDIF} 
           {$IFDEF FPC}vmovsd [eax + esi - 8], xmm2;{$ELSE}db $C5,$FB,$11,$54,$30,$F8;{$ENDIF} 
           {$IFDEF FPC}vmovapd xmm1, xmm0;{$ELSE}db $C5,$F9,$29,$C1;{$ENDIF} 
       add esi, 8;
       jnz @@forxloop;

       add eax, edx;
       add ecx, edi;
   dec height;
   jnz @@foryloop;

   @@exitProc:
   // epilog - stack cleanup
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;

procedure AVXMatrixDifferentiateColumnUnaligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
asm
   // prolog - maintain stack
   push ebx;
   push edi;
   push esi;

   // if (width <= 1) or (height <= 0) then exit;
   mov ebx, height;
   cmp ebx, 1;
   jle @@exitproc;

   mov ebx, width;
   cmp ebx, 1;
   jle @@exitproc;

   dec height;
   sub width, 4;
   jl @@forxloopend;

   @@forxloop:
       mov ebx, height;
       {$IFDEF FPC}vxorpd ymm0, ymm0, ymm0;{$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} 
       mov edi, srcLineWidth;
       xor esi, esi;

       {$IFDEF FPC}vmovupd ymm0, [ecx];{$ELSE}db $C5,$FD,$10,$01;{$ENDIF} 

       // 4 values at once
       @@foryloop:
           {$IFDEF FPC}vmovupd ymm1, [ecx + edi];{$ELSE}db $C5,$FD,$10,$0C,$39;{$ENDIF} 
           {$IFDEF FPC}vsubpd ymm2, ymm1, ymm0;{$ELSE}db $C5,$F5,$5C,$D0;{$ENDIF} 
           {$IFDEF FPC}vmovupd [eax + esi], ymm2;{$ELSE}db $C5,$FD,$11,$14,$30;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm0, ymm1;{$ELSE}db $C5,$FD,$29,$C8;{$ENDIF} 

           add edi, srcLineWidth;
           add esi, edx;
       dec ebx;
       jnz @@foryloop;

       add ecx, 32;
       add eax, 32;
   sub width, 4;
   jge @@forxloop;

   @@forxloopend:
   add width, 4;
   jz @@exitProc;

   // #####################################
   // #### last < 4 columns
   @@forxloopshort:
      mov ebx, height;
      {$IFDEF FPC}vxorpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$57,$C0;{$ENDIF} 
      mov edi, srcLineWidth;
      xor esi, esi;

      {$IFDEF FPC}vmovsd xmm0, [ecx];{$ELSE}db $C5,$FB,$10,$01;{$ENDIF} 

      // one column value:
      @@foryloopshort:
          {$IFDEF FPC}vmovsd xmm1, [ecx + edi];{$ELSE}db $C5,$FB,$10,$0C,$39;{$ENDIF} 
          {$IFDEF FPC}vsubsd xmm2, xmm1, xmm0;{$ELSE}db $C5,$F3,$5C,$D0;{$ENDIF} 
          {$IFDEF FPC}vmovsd [eax + esi], xmm2;{$ELSE}db $C5,$FB,$11,$14,$30;{$ENDIF} 

          {$IFDEF FPC}vmovsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$11,$C8;{$ENDIF} 

          add edi, srcLineWidth;
          add esi, edx;
      dec ebx;
      jnz @@foryloopshort;

      add ecx, 8;
      add eax, 8;

   dec width;
   jnz @@forxloopshort;

   @@exitProc:

   // epilog - stack cleanup
   pop esi;
   pop edi;
   pop ebx;
end;

procedure AVXMatrixDifferentiateColumnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
asm
   // prolog - maintain stack
   push ebx;
   push edi;
   push esi;

   // if (width <= 1) or (height <= 0) then exit;
   mov ebx, height;
   cmp ebx, 1;
   jle @@exitproc;

   mov ebx, width;
   cmp ebx, 1;
   jle @@exitproc;

   dec height;
   sub width, 4;
   jl @@forxloopend;

   @@forxloop:
       mov ebx, height;
       {$IFDEF FPC}vxorpd ymm0, ymm0, ymm0;{$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} 
       mov edi, srcLineWidth;
       xor esi, esi;

       {$IFDEF FPC}vmovupd ymm0, [ecx];{$ELSE}db $C5,$FD,$10,$01;{$ENDIF} 

       // 4 values at once
       @@foryloop:
           {$IFDEF FPC}vmovapd ymm1, [ecx + edi];{$ELSE}db $C5,$FD,$28,$0C,$39;{$ENDIF} 
           {$IFDEF FPC}vsubpd ymm2, ymm1, ymm0;{$ELSE}db $C5,$F5,$5C,$D0;{$ENDIF} 
           {$IFDEF FPC}vmovapd [eax + esi], ymm2;{$ELSE}db $C5,$FD,$29,$14,$30;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm0, ymm1;{$ELSE}db $C5,$FD,$29,$C8;{$ENDIF} 

           add edi, srcLineWidth;
           add esi, edx;
       dec ebx;
       jnz @@foryloop;

       add ecx, 32;
       add eax, 32;
   sub width, 4;
   jge @@forxloop;

   @@forxloopend:
   add width, 4;
   jz @@exitProc;

   // #####################################
   // #### last < 4 columns
   @@forxloopshort:
      mov ebx, height;
      {$IFDEF FPC}vxorpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$57,$C0;{$ENDIF} 
      mov edi, srcLineWidth;
      xor esi, esi;

      {$IFDEF FPC}vmovsd xmm0, [ecx];{$ELSE}db $C5,$FB,$10,$01;{$ENDIF} 

      // one column value:
      @@foryloopshort:
          {$IFDEF FPC}vmovsd xmm1, [ecx + edi];{$ELSE}db $C5,$FB,$10,$0C,$39;{$ENDIF} 
          {$IFDEF FPC}vsubsd xmm2, xmm1, xmm0;{$ELSE}db $C5,$F3,$5C,$D0;{$ENDIF} 
          {$IFDEF FPC}vmovsd [eax + esi], xmm2;{$ELSE}db $C5,$FB,$11,$14,$30;{$ENDIF} 

          {$IFDEF FPC}vmovsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$11,$C8;{$ENDIF} 

          add edi, srcLineWidth;
          add esi, edx;
      dec ebx;
      jnz @@foryloopshort;

      add ecx, 8;
      add eax, 8;

   dec width;
   jnz @@forxloopshort;

   @@exitProc:

   // epilog - stack cleanup
   pop esi;
   pop edi;
   pop ebx;
end;
{$ENDIF}

end.
