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

procedure AVXMatrixCumulativeSumRow(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);

procedure AVXMatrixCumulativeSumColumnUnaligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
procedure AVXMatrixCumulativeSumColumnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);

procedure AVXMatrixDifferentiateRow(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);

procedure AVXMatrixDifferentiateColumnUnaligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
procedure AVXMatrixDifferentiateColumnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);

{$ENDIF}

implementation

{$IFNDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$S-} {$ENDIF}

// ##################################################
// #### Cumulative sum
// ##################################################

procedure AVXMatrixCumulativeSumRow(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
begin
asm
   // prolog - maintain stack
   push ebx;
   push edi;
   push esi;

   // if (width <= 0) or (height <= 0) then exit;
   mov edi, width;
   cmp edi, 0;
   jle @@exitproc;
   mov eax, height;
   cmp eax, 0;
   jle @@exitproc;

   // iter := -width*sizeof(Double)
   imul edi, -8;

   mov ebx, destLineWidth;

   // prepare counters
   mov ecx, dest;
   mov edx, src;
   sub ecx, edi;
   sub edx, edi;

   mov esi, height;
   @@foryloop:
      mov eax, edi;

      {$IFDEF FPC}vxorpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$57,$C0;{$ENDIF} 
      @@forxloop:
         {$IFDEF FPC}vmovsd xmm1, [edx + eax];{$ELSE}db $C5,$FB,$10,$0C,$02;{$ENDIF} 
         {$IFDEF FPC}vaddsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$58,$C1;{$ENDIF} 
         {$IFDEF FPC}vmovsd [ecx + eax], xmm0;{$ELSE}db $C5,$FB,$11,$04,$01;{$ENDIF} 
      add eax, 8;
      jnz @@forxloop;

      add ecx, ebx;
      add edx, srcLineWidth;
   dec esi;
   jnz @@foryloop;
   @@exitProc:

   // epilog - stack cleanup
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;
end;

procedure AVXMatrixCumulativeSumColumnUnaligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
begin
asm
   // prolog - maintain stack
   push edi;
   push esi;
   push ebx;

   // if (width <= 1) or (height <= 0) then exit;
   mov eax, height;
   cmp eax, 0;

   jle @@exitproc;

   mov ecx, dest;
   mov edx, src;

   mov ebx, width;
   sub ebx, 4;
   jl @@lastColumns;

   @@forxloop:
       mov eax, height;
       {$IFDEF FPC}vxorpd ymm0, ymm0, ymm0;{$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} 
       xor edi, edi;
       xor esi, esi;

       // 4 values at once
       @@foryloop:
           {$IFDEF FPC}vmovupd ymm1, [edx + edi];{$ELSE}db $C5,$FD,$10,$0C,$3A;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 
           {$IFDEF FPC}vmovupd [ecx + esi], ymm0;{$ELSE}db $C5,$FD,$11,$04,$31;{$ENDIF} 

           add edi, srcLineWidth;
           add esi, destLineWidth;
       dec eax;
       jnz @@foryloop;

       add edx, 32;
       add ecx, 32;
   sub ebx, 4;
   jge @@forxloop;

   @@lastColumns:
   add ebx, 4;
   jz @@exitProc;

   @@forxloopshort:
       mov eax, height;
       {$IFDEF FPC}vxorpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$57,$C0;{$ENDIF} 
       xor edi, edi;
       xor esi, esi;

       // 4 values at once
       @@foryloopshort:
           {$IFDEF FPC}vmovsd xmm1, [edx + edi];{$ELSE}db $C5,$FB,$10,$0C,$3A;{$ENDIF} 
           {$IFDEF FPC}vaddsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$58,$C1;{$ENDIF} 
           {$IFDEF FPC}vmovsd [ecx + esi], xmm0;{$ELSE}db $C5,$FB,$11,$04,$31;{$ENDIF} 

           add edi, srcLineWidth;
           add esi, destLineWidth;
       dec eax;
       jnz @@foryloopshort;

       add edx, 8;
       add ecx, 8;
   sub ebx, 1;
   jg @@forxloopshort;

   @@exitProc:

   // epilog - stack cleanup
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop ebx;
   pop esi;
   pop edi;
end;
end;

procedure AVXMatrixCumulativeSumColumnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
begin
asm
   // prolog - maintain stack
   push edi;
   push esi;
   push ebx;

   // if (width <= 1) or (height <= 0) then exit;
   mov eax, height;
   cmp eax, 0;

   jle @@exitproc;

   mov ecx, dest;
   mov edx, src;

   mov ebx, width;
   sub ebx, 4;
   jl @@lastColumns;

   @@forxloop:
       mov eax, height;
       {$IFDEF FPC}vxorpd ymm0, ymm0, ymm0;{$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} 
       xor edi, edi;
       xor esi, esi;

       // 4 values at once
       @@foryloop:
           {$IFDEF FPC}vmovapd ymm1, [edx + edi];{$ELSE}db $C5,$FD,$28,$0C,$3A;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 
           {$IFDEF FPC}vmovapd [ecx + esi], ymm0;{$ELSE}db $C5,$FD,$29,$04,$31;{$ENDIF} 

           add edi, srcLineWidth;
           add esi, destLineWidth;
       dec eax;
       jnz @@foryloop;

       add edx, 32;
       add ecx, 32;
   sub ebx, 4;
   jge @@forxloop;

   @@lastColumns:
   add ebx, 4;
   jz @@exitProc;

   @@forxloopshort:
       mov eax, height;
       {$IFDEF FPC}vxorpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$57,$C0;{$ENDIF} 
       xor edi, edi;
       xor esi, esi;

       // 4 values at once
       @@foryloopshort:
           {$IFDEF FPC}vmovsd xmm1, [edx + edi];{$ELSE}db $C5,$FB,$10,$0C,$3A;{$ENDIF} 
           {$IFDEF FPC}vaddsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$58,$C1;{$ENDIF} 
           {$IFDEF FPC}vmovsd [ecx + esi], xmm0;{$ELSE}db $C5,$FB,$11,$04,$31;{$ENDIF} 

           add edi, srcLineWidth;
           add esi, destLineWidth;
       dec eax;
       jnz @@foryloopshort;

       add edx, 8;
       add ecx, 8;
   sub ebx, 1;
   jg @@forxloopshort;

   @@exitProc:

   // epilog - stack cleanup
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop ebx;
   pop esi;
   pop edi;
end;
end;

// ###############################################
// #### Differentiate
// ###############################################

procedure AVXMatrixDifferentiateRow(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
begin
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
   mov ecx, dest;
   mov edx, src;
   sub ecx, ebx;
   sub edx, ebx;

   add ebx, 8;
   @@foryloop:
       mov eax, ebx;
       {$IFDEF FPC}vmovsd xmm1, [edx + eax - 8];{$ELSE}db $C5,$FB,$10,$4C,$02,$F8;{$ENDIF} 
       @@forxloop:
           {$IFDEF FPC}vmovsd xmm0, [edx + eax];{$ELSE}db $C5,$FB,$10,$04,$02;{$ENDIF} 
           {$IFDEF FPC}vsubsd xmm2, xmm0, xmm1;{$ELSE}db $C5,$FB,$5C,$D1;{$ENDIF} 
           {$IFDEF FPC}vmovsd [ecx + eax - 8], xmm2;{$ELSE}db $C5,$FB,$11,$54,$01,$F8;{$ENDIF} 
           {$IFDEF FPC}vmovapd xmm1, xmm0;{$ELSE}db $C5,$F9,$28,$C8;{$ENDIF} 
       add eax, 8;
       jnz @@forxloop;

       add ecx, destLineWidth;
       add edx, edi;
   dec esi;
   jnz @@foryloop;

   @@exitProc:
   // epilog - stack cleanup
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;
end;

procedure AVXMatrixDifferentiateColumnUnaligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
begin
asm
   // prolog - maintain stack
   push ebx;
   push edi;
   push esi;

   mov ecx, dest;
   mov edx, src;

   // if (width <= 1) or (height <= 0) then exit;
   mov eax, height;
   cmp eax, 1;
   jle @@exitproc;

   mov ebx, width;
   cmp ebx, 1;
   jle @@exitproc;

   dec height;
   sub ebx, 4;
   jl @@forxloopend;

   @@forxloop:
       mov eax, height;
       {$IFDEF FPC}vxorpd ymm0, ymm0, ymm0;{$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} 
       mov edi, srcLineWidth;
       xor esi, esi;

       {$IFDEF FPC}vmovupd ymm0, [edx];{$ELSE}db $C5,$FD,$10,$02;{$ENDIF} 

       // 4 values at once
       @@foryloop:
           {$IFDEF FPC}vmovupd ymm1, [edx + edi];{$ELSE}db $C5,$FD,$10,$0C,$3A;{$ENDIF} 
           {$IFDEF FPC}vsubpd ymm2, ymm1, ymm0;{$ELSE}db $C5,$F5,$5C,$D0;{$ENDIF} 
           {$IFDEF FPC}vmovupd [ecx + esi], ymm2;{$ELSE}db $C5,$FD,$11,$14,$31;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm0, ymm1;{$ELSE}db $C5,$FD,$28,$C1;{$ENDIF} 

           add edi, srcLineWidth;
           add esi, destLineWidth;
       dec eax;
       jnz @@foryloop;

       add edx, 32;
       add ecx, 32;
   sub ebx, 4;
   jge @@forxloop;

   @@forxloopend:
   add ebx, 4;
   jz @@exitProc;

   // #####################################
   // #### last < 4 columns
   @@forxloopshort:
      mov eax, height;
      {$IFDEF FPC}vxorpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$57,$C0;{$ENDIF} 
      mov edi, srcLineWidth;
      xor esi, esi;

      {$IFDEF FPC}vmovsd xmm0, [edx];{$ELSE}db $C5,$FB,$10,$02;{$ENDIF} 

      // one column value:
      @@foryloopshort:
          {$IFDEF FPC}vmovsd xmm1, [edx + edi];{$ELSE}db $C5,$FB,$10,$0C,$3A;{$ENDIF} 
          {$IFDEF FPC}vsubsd xmm2, xmm1, xmm0;{$ELSE}db $C5,$F3,$5C,$D0;{$ENDIF} 
          {$IFDEF FPC}vmovsd [ecx + esi], xmm2;{$ELSE}db $C5,$FB,$11,$14,$31;{$ENDIF} 

          {$IFDEF FPC}vmovsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$10,$C1;{$ENDIF} 

          add edi, srcLineWidth;
          add esi, destLineWidth;
      dec eax;
      jnz @@foryloopshort;

      add edx, 8;
      add ecx, 8;

   dec ebx;
   jnz @@forxloopshort;

   @@exitProc:

   // epilog - stack cleanup
   pop esi;
   pop edi;
   pop ebx;
end;
end;

procedure AVXMatrixDifferentiateColumnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
begin
asm
   // prolog - maintain stack
   push ebx;
   push edi;
   push esi;

   mov ecx, dest;
   mov edx, src;

   // if (width <= 1) or (height <= 0) then exit;
   mov eax, height;
   cmp eax, 1;
   jle @@exitproc;

   mov ebx, width;
   cmp ebx, 1;
   jle @@exitproc;

   dec height;
   sub ebx, 4;
   jl @@forxloopend;

   @@forxloop:
       mov eax, height;
       {$IFDEF FPC}vxorpd ymm0, ymm0, ymm0;{$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} 
       mov edi, srcLineWidth;
       xor esi, esi;

       {$IFDEF FPC}vmovupd ymm0, [edx];{$ELSE}db $C5,$FD,$10,$02;{$ENDIF} 

       // 4 values at once
       @@foryloop:
           {$IFDEF FPC}vmovapd ymm1, [edx + edi];{$ELSE}db $C5,$FD,$28,$0C,$3A;{$ENDIF} 
           {$IFDEF FPC}vsubpd ymm2, ymm1, ymm0;{$ELSE}db $C5,$F5,$5C,$D0;{$ENDIF} 
           {$IFDEF FPC}vmovapd [ecx + esi], ymm2;{$ELSE}db $C5,$FD,$29,$14,$31;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm0, ymm1;{$ELSE}db $C5,$FD,$28,$C1;{$ENDIF} 

           add edi, srcLineWidth;
           add esi, destLineWidth;
       dec eax;
       jnz @@foryloop;

       add edx, 32;
       add ecx, 32;
   sub ebx, 4;
   jge @@forxloop;

   @@forxloopend:
   add ebx, 4;
   jz @@exitProc;

   // #####################################
   // #### last < 4 columns
   @@forxloopshort:
      mov eax, height;
      {$IFDEF FPC}vxorpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$57,$C0;{$ENDIF} 
      mov edi, srcLineWidth;
      xor esi, esi;

      {$IFDEF FPC}vmovsd xmm0, [edx];{$ELSE}db $C5,$FB,$10,$02;{$ENDIF} 

      // one column value:
      @@foryloopshort:
          {$IFDEF FPC}vmovsd xmm1, [edx + edi];{$ELSE}db $C5,$FB,$10,$0C,$3A;{$ENDIF} 
          {$IFDEF FPC}vsubsd xmm2, xmm1, xmm0;{$ELSE}db $C5,$F3,$5C,$D0;{$ENDIF} 
          {$IFDEF FPC}vmovsd [ecx + esi], xmm2;{$ELSE}db $C5,$FB,$11,$14,$31;{$ENDIF} 

          {$IFDEF FPC}vmovsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$10,$C1;{$ENDIF} 

          add edi, srcLineWidth;
          add esi, destLineWidth;
      dec eax;
      jnz @@foryloopshort;

      add edx, 8;
      add ecx, 8;

   dec ebx;
   jnz @@forxloopshort;

   @@exitProc:

   // epilog - stack cleanup
   pop esi;
   pop edi;
   pop ebx;
end;
end;

{$ENDIF}

end.
