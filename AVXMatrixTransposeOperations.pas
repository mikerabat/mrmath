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


unit AVXMatrixTransposeOperations;

interface

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}
{$IFNDEF x64}

uses MatrixConst;

procedure AVXMatrixTransposeAligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt : PDouble; const LineWidth : TASMNativeInt; width : TASMNativeInt; height : TASMNativeInt);
procedure AVXMatrixTransposeUnaligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt : PDouble; const LineWidth : TASMNativeInt; width : TASMNativeInt; height : TASMNativeInt);

// Inplace Trasnposition of an N x N matrix
procedure AVXMatrixTransposeInplace(mt : PDouble; const LineWidth : TASMNativeInt; N : TASMNativeInt);

{$ENDIF}

implementation

{$IFNDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$S-} {$ENDIF}

procedure AVXMatrixTransposeAligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt : PDouble; const LineWidth : TASMNativeInt; width : TASMNativeInt; height : TASMNativeInt);
begin
asm
   // prolog:
   push ebx;
   push edi;
   push esi;

   // iters := -width*sizeof(double);
   mov edi, width;
   imul edi, -8;

   mov edx, mt;
   sub edx, edi;
   mov mt, edx;

   mov esi, height;
   sub esi, 4;
   jl @@foryloop4End;

   // we have at least a height of 4 -> try to transpose 4x4 blocks
   @foryloop4:
      // prepare pointers to mt
      mov ecx, dest;

      // 4x4 blockwise transposition
      mov eax, edi;
      @forxloop4:
         add eax, 32;
         jg @loopend4;

         mov edx, mt;
         mov ebx, LineWidth;
         {$IFDEF FPC}vmovapd ymm0, [edx + eax - 32];{$ELSE}db $C5,$FD,$28,$44,$02,$E0;{$ENDIF} 
         add edx, ebx;
         {$IFDEF FPC}vmovapd ymm1, [edx + eax - 32];{$ELSE}db $C5,$FD,$28,$4C,$02,$E0;{$ENDIF} 
         add edx, ebx;
         {$IFDEF FPC}vmovapd ymm2, [edx + eax - 32];{$ELSE}db $C5,$FD,$28,$54,$02,$E0;{$ENDIF} 
         add edx, ebx;
         {$IFDEF FPC}vmovapd ymm3, [edx + eax - 32];{$ELSE}db $C5,$FD,$28,$5C,$02,$E0;{$ENDIF} 

         {$IFDEF FPC}vunpckhpd ymm4, ymm0, ymm1;{$ELSE}db $C5,$FD,$15,$E1;{$ENDIF} 
         {$IFDEF FPC}vunpckhpd ymm5, ymm2, ymm3;{$ELSE}db $C5,$ED,$15,$EB;{$ENDIF} 
         {$IFDEF FPC}vunpcklpd ymm7, ymm2, ymm3;{$ELSE}db $C5,$ED,$14,$FB;{$ENDIF} 

         {$IFDEF FPC}vperm2f128 ymm3, ymm4, ymm5, $31;{$ELSE}db $C4,$E3,$5D,$06,$DD,$31;{$ENDIF} 

         {$IFDEF FPC}vunpcklpd ymm6, ymm0, ymm1;{$ELSE}db $C5,$FD,$14,$F1;{$ENDIF} 
         {$IFDEF FPC}vinsertf128 ymm1, ymm4, xmm5, 1;{$ELSE}db $C4,$E3,$5D,$18,$CD,$01;{$ENDIF} 
         {$IFDEF FPC}vperm2f128 ymm2, ymm6, ymm7, $31;{$ELSE}db $C4,$E3,$4D,$06,$D7,$31;{$ENDIF} 
         {$IFDEF FPC}vinsertf128 ymm0, ymm6, xmm7, 1;{$ELSE}db $C4,$E3,$4D,$18,$C7,$01;{$ENDIF} 

         mov ebx, destLineWidth;
         {$IFDEF FPC}vmovapd [ecx], ymm0;{$ELSE}db $C5,$FD,$29,$01;{$ENDIF} 
         add ecx, ebx;
         {$IFDEF FPC}vmovapd [ecx], ymm1;{$ELSE}db $C5,$FD,$29,$09;{$ENDIF} 
         add ecx, ebx;
         {$IFDEF FPC}vmovapd [ecx], ymm2;{$ELSE}db $C5,$FD,$29,$11;{$ENDIF} 
         add ecx, ebx;
         {$IFDEF FPC}vmovapd [ecx], ymm3;{$ELSE}db $C5,$FD,$29,$19;{$ENDIF} 
         add ecx, ebx;

      jmp @forxloop4;

      @loopend4:
      sub eax, 32;
      jz @nextline4;

      // handle the missing columns
      @forxloop:
         mov edx, mt;
         mov ebx, LineWidth;
         {$IFDEF FPC}vmovsd xmm0, [edx + eax];{$ELSE}db $C5,$FB,$10,$04,$02;{$ENDIF} 
         add edx, ebx;
         {$IFDEF FPC}vmovsd [ecx], xmm0;{$ELSE}db $C5,$FB,$11,$01;{$ENDIF} 

         {$IFDEF FPC}vmovsd xmm0, [edx + eax];{$ELSE}db $C5,$FB,$10,$04,$02;{$ENDIF} 
         {$IFDEF FPC}vmovsd [ecx + 8], xmm0;{$ELSE}db $C5,$FB,$11,$41,$08;{$ENDIF} 
         add edx, ebx;

         {$IFDEF FPC}vmovsd xmm0, [edx + eax];{$ELSE}db $C5,$FB,$10,$04,$02;{$ENDIF} 
         {$IFDEF FPC}vmovsd [ecx + 16], xmm0;{$ELSE}db $C5,$FB,$11,$41,$10;{$ENDIF} 
         add edx, ebx;

         {$IFDEF FPC}vmovsd xmm0, [edx + eax];{$ELSE}db $C5,$FB,$10,$04,$02;{$ENDIF} 
         {$IFDEF FPC}vmovsd [ecx + 24], xmm0;{$ELSE}db $C5,$FB,$11,$41,$18;{$ENDIF} 
         add ecx, destLineWidth;

      add eax, 8;
      jnz @forxloop;

      @nextline4:

      // dest := dest + 4*sizeof(double)
      add dest, 32;
      mov ebx, LineWidth;
      mov edx, mt; // src := src + 4*LineWidth
      lea edx, [edx + 4*ebx];
      mov mt, edx;
   sub esi, 4;
   jge @foryloop4;

   @@foryloop4End:

   add esi, 4;
   jz @endproc;

   // #############################
   // #### handle the last max 4 rows
   mov ecx, dest;
   mov edx, mt;
   mov ebx, destLineWidth;
   @@foryloop:
      mov eax, edi;

      @@forxloop:
         {$IFDEF FPC}vmovsd xmm0, [edx + eax];{$ELSE}db $C5,$FB,$10,$04,$02;{$ENDIF} 
         {$IFDEF FPC}vmovsd [ecx], xmm0;{$ELSE}db $C5,$FB,$11,$01;{$ENDIF} 
         add ecx, ebx;
      add eax, 8;
      jnz @@forxloop;

      add dest, 8;
      mov ecx, dest;
      add edx, LineWidth;
   dec esi;
   jnz @@foryloop;

   @endproc:

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;
end;

procedure AVXMatrixTransposeUnaligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt : PDouble; const LineWidth : TASMNativeInt; width : TASMNativeInt; height : TASMNativeInt);
begin
asm
   // prolog:
   push ebx;
   push edi;
   push esi;

   // iters := -width*sizeof(double);
   mov edi, width;
   imul edi, -8;

   mov edx, mt;
   sub edx, edi;
   mov mt, edx;

   mov ebx, LineWidth;

   mov esi, height;
   sub esi, 4;
   jl @@foryloop4End;

   // we have at least a height of 4 -> try to transpose 4x4 blocks
   @foryloop4:
      // prepare pointers to mt
      mov ecx, dest;

      // 4x4 blockwise transposition
      mov eax, edi;
      @forxloop4:
         add eax, 32;
         jg @loopend4;

         mov edx, mt;
         mov ebx, LineWidth;
         {$IFDEF FPC}vmovupd ymm0, [edx + eax - 32];{$ELSE}db $C5,$FD,$10,$44,$02,$E0;{$ENDIF} 
         add edx, ebx;
         {$IFDEF FPC}vmovupd ymm1, [edx + eax - 32];{$ELSE}db $C5,$FD,$10,$4C,$02,$E0;{$ENDIF} 
         add edx, ebx;
         {$IFDEF FPC}vmovupd ymm2, [edx + eax - 32];{$ELSE}db $C5,$FD,$10,$54,$02,$E0;{$ENDIF} 
         add edx, ebx;
         {$IFDEF FPC}vmovupd ymm3, [edx + eax - 32];{$ELSE}db $C5,$FD,$10,$5C,$02,$E0;{$ENDIF} 

         {$IFDEF FPC}vunpckhpd ymm4, ymm0, ymm1;{$ELSE}db $C5,$FD,$15,$E1;{$ENDIF} 
         {$IFDEF FPC}vunpckhpd ymm5, ymm2, ymm3;{$ELSE}db $C5,$ED,$15,$EB;{$ENDIF} 
         {$IFDEF FPC}vunpcklpd ymm7, ymm2, ymm3;{$ELSE}db $C5,$ED,$14,$FB;{$ENDIF} 

         {$IFDEF FPC}vperm2f128 ymm3, ymm4, ymm5, $31;{$ELSE}db $C4,$E3,$5D,$06,$DD,$31;{$ENDIF} 

         {$IFDEF FPC}vunpcklpd ymm6, ymm0, ymm1;{$ELSE}db $C5,$FD,$14,$F1;{$ENDIF} 
         {$IFDEF FPC}vinsertf128 ymm1, ymm4, xmm5, 1;{$ELSE}db $C4,$E3,$5D,$18,$CD,$01;{$ENDIF} 
         {$IFDEF FPC}vperm2f128 ymm2, ymm6, ymm7, $31;{$ELSE}db $C4,$E3,$4D,$06,$D7,$31;{$ENDIF} 
         {$IFDEF FPC}vinsertf128 ymm0, ymm6, xmm7, 1;{$ELSE}db $C4,$E3,$4D,$18,$C7,$01;{$ENDIF} 

         mov ebx, destLineWidth;
         {$IFDEF FPC}vmovupd [ecx], ymm0;{$ELSE}db $C5,$FD,$11,$01;{$ENDIF} 
         add ecx, ebx;
         {$IFDEF FPC}vmovupd [ecx], ymm1;{$ELSE}db $C5,$FD,$11,$09;{$ENDIF} 
         add ecx, ebx;
         {$IFDEF FPC}vmovupd [ecx], ymm2;{$ELSE}db $C5,$FD,$11,$11;{$ENDIF} 
         add ecx, ebx;
         {$IFDEF FPC}vmovupd [ecx], ymm3;{$ELSE}db $C5,$FD,$11,$19;{$ENDIF} 
         add ecx, ebx;

      jmp @forxloop4;

      @loopend4:
      sub eax, 32;
      jz @nextline4;

       // handle the missing columns
      @forxloop:
         mov edx, mt;
         mov ebx, LineWidth;
         {$IFDEF FPC}vmovsd xmm0, [edx + eax];{$ELSE}db $C5,$FB,$10,$04,$02;{$ENDIF} 
         add edx, ebx;
         {$IFDEF FPC}vmovsd [ecx], xmm0;{$ELSE}db $C5,$FB,$11,$01;{$ENDIF} 

         {$IFDEF FPC}vmovsd xmm0, [edx + eax];{$ELSE}db $C5,$FB,$10,$04,$02;{$ENDIF} 
         {$IFDEF FPC}vmovsd [ecx + 8], xmm0;{$ELSE}db $C5,$FB,$11,$41,$08;{$ENDIF} 
         add edx, ebx;

         {$IFDEF FPC}vmovsd xmm0, [edx + eax];{$ELSE}db $C5,$FB,$10,$04,$02;{$ENDIF} 
         {$IFDEF FPC}vmovsd [ecx + 16], xmm0;{$ELSE}db $C5,$FB,$11,$41,$10;{$ENDIF} 
         add edx, ebx;

         {$IFDEF FPC}vmovsd xmm0, [edx + eax];{$ELSE}db $C5,$FB,$10,$04,$02;{$ENDIF} 
         {$IFDEF FPC}vmovsd [ecx + 24], xmm0;{$ELSE}db $C5,$FB,$11,$41,$18;{$ENDIF} 
         add ecx, destLineWidth;

      add eax, 8;
      jnz @forxloop;

      @nextline4:

      // dest := dest + 4*sizeof(double)
      add dest, 32;
      mov edx, mt; // src := src + 4*LineWidth
      mov ebx, LineWidth;
      lea edx, [edx + 4*ebx];
      mov mt, edx;
   sub esi, 4;
   jge @foryloop4;

   @@foryloop4End:

   add esi, 4;
   jz @endproc;

   // #############################
   // #### handle the last max 4 rows
   mov ecx, dest;
   mov edx, mt;
   mov ebx, destLineWidth;
   @@foryloop:
      mov eax, edi;

      @@forxloop:
         {$IFDEF FPC}vmovsd xmm0, [edx + eax];{$ELSE}db $C5,$FB,$10,$04,$02;{$ENDIF} 
         {$IFDEF FPC}vmovsd [ecx], xmm0;{$ELSE}db $C5,$FB,$11,$01;{$ENDIF} 
         add ecx, ebx;
      add eax, 8;
      jnz @@forxloop;

      add dest, 8;
      mov ecx, dest;
      add edx, LineWidth;
   dec esi;
   jnz @@foryloop;

   @endproc:

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;
end;

// simple Inplace Trasnposition of an N x N matrix
procedure AVXMatrixTransposeInplace(mt : PDouble; const LineWidth : TASMNativeInt; N : TASMNativeInt);
begin
asm
   // prolog
   push ebx;
   push edi;
   push esi;

   mov eax, N;
   cmp eax, 2;
   jl @@exitProc;

   // iter: -N*sizeof(Double)
   mov edx, eax;
   imul edx, -8;

   mov eax, mt;   // pDest
   mov ebx, eax;  // pDest1: genptr(mt, 0, 1, linewidth)
   add ebx, LineWidth;

   sub eax, edx;  // mt + iter

   // for y := 0 to n - 2
   dec N;

   mov ecx, LineWidth;
   @@foryloop:
       mov edi, edx; // iter aka x
       add edi, 8;
       mov esi, ebx;
       // for x := y + 1 to n-1 do
       @@forxloop:
           {$IFDEF FPC}vmovsd xmm0, [eax + edi];{$ELSE}db $C5,$FB,$10,$04,$38;{$ENDIF} 
           {$IFDEF FPC}vmovsd xmm1, [esi];{$ELSE}db $C5,$FB,$10,$0E;{$ENDIF} 

           {$IFDEF FPC}vmovsd [eax + edi], xmm1;{$ELSE}db $C5,$FB,$11,$0C,$38;{$ENDIF} 
           {$IFDEF FPC}vmovsd [esi], xmm0;{$ELSE}db $C5,$FB,$11,$06;{$ENDIF} 

           add esi, ecx;
       add edi, 8;
       jnz @@forxloop;

       add edx, 8;  // iter + sizeof(double);
       //pDest := PConstDoubleArr( GenPtr(dest, 0, y, destLineWidth) );
       add eax, ecx;
       // GenPtr(dest, y, y + 1, destLineWidth);
       add ebx, ecx;
       add ebx, 8;
   dec N;
   jnz @@foryloop;

   @@exitProc:

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 
   pop esi;
   pop edi;
   pop ebx;
end;
end;

{$ENDIF}

end.
