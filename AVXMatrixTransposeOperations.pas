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

procedure AVXMatrixTransposeAligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt : PDouble; const LineWidth : TASMNativeInt; width : TASMNativeInt; height : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure AVXMatrixTransposeUnaligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt : PDouble; const LineWidth : TASMNativeInt; width : TASMNativeInt; height : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

// Inplace Trasnposition of an N x N matrix
procedure AVXMatrixTransposeInplace(mt : PDouble; const LineWidth : TASMNativeInt; N : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

{$ENDIF}

implementation

{$IFNDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$S-} {$ENDIF}

procedure AVXMatrixTransposeAligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt : PDouble; const LineWidth : TASMNativeInt; width : TASMNativeInt; height : TASMNativeInt);
var iter : TASMNativeInt;
asm
   // prolog:
   push ebx;
   push edi;
   push esi;

   // iters := -width*sizeof(double);
   mov edi, width;
   imul edi, -8;

   mov iter, edi;

   sub ecx, edi;
   mov ebx, LineWidth;

   mov edi, Height;
   sub edi, 4;
   jl @@foryloop4End;

   // we have at least a height of 4 -> try to transpose 4x4 blocks
   @foryloop4:
      // prepare pointers to mt
      push eax;

      // 4x4 blockwise transposition
      mov esi, iter;
      @forxloop4:
         add esi, 32;
         jg @loopend4;

         {$IFDEF FPC}vmovapd ymm0, [ecx + esi - 32];{$ELSE}db $C5,$FD,$28,$44,$31,$E0;{$ENDIF} 
         add ecx, ebx;
         {$IFDEF FPC}vmovapd ymm1, [ecx + esi - 32];{$ELSE}db $C5,$FD,$28,$4C,$31,$E0;{$ENDIF} 
         add ecx, ebx;
         {$IFDEF FPC}vmovapd ymm2, [ecx + esi - 32];{$ELSE}db $C5,$FD,$28,$54,$31,$E0;{$ENDIF} 
         add ecx, ebx;
         {$IFDEF FPC}vmovapd ymm3, [ecx + esi - 32];{$ELSE}db $C5,$FD,$28,$5C,$31,$E0;{$ENDIF} 
         add ecx, ebx;

         {$IFDEF FPC}vunpckhpd ymm4, ymm0, ymm1;{$ELSE}db $C5,$FD,$15,$E1;{$ENDIF} 
         {$IFDEF FPC}vunpckhpd ymm5, ymm2, ymm3;{$ELSE}db $C5,$ED,$15,$EB;{$ENDIF} 
         {$IFDEF FPC}vunpcklpd ymm7, ymm2, ymm3;{$ELSE}db $C5,$ED,$14,$FB;{$ENDIF} 

         shl ebx, 2;

         {$IFDEF FPC}vperm2f128 ymm3, ymm4, ymm5, $31;{$ELSE}db $C4,$E3,$5D,$06,$DD,$31;{$ENDIF} 

         {$IFDEF FPC}vunpcklpd ymm6, ymm0, ymm1;{$ELSE}db $C5,$FD,$14,$F1;{$ENDIF} 
         {$IFDEF FPC}vinsertf128 ymm1, ymm4, xmm5, 1;{$ELSE}db $C4,$E3,$5D,$18,$CD,$01;{$ENDIF} 
         {$IFDEF FPC}vperm2f128 ymm2, ymm6, ymm7, $31;{$ELSE}db $C4,$E3,$4D,$06,$D7,$31;{$ENDIF} 
         {$IFDEF FPC}vinsertf128 ymm0, ymm6, xmm7, 1;{$ELSE}db $C4,$E3,$4D,$18,$C7,$01;{$ENDIF} 

         // recover ecx
         sub ecx, ebx;

         {$IFDEF FPC}vmovapd [eax], ymm0;{$ELSE}db $C5,$FD,$29,$00;{$ENDIF} 
         add eax, edx;
         {$IFDEF FPC}vmovapd [eax], ymm1;{$ELSE}db $C5,$FD,$29,$08;{$ENDIF} 
         add eax, edx;

         shr ebx, 2;

         {$IFDEF FPC}vmovapd [eax], ymm2;{$ELSE}db $C5,$FD,$29,$10;{$ENDIF} 
         add eax, edx;
         {$IFDEF FPC}vmovapd [eax], ymm3;{$ELSE}db $C5,$FD,$29,$18;{$ENDIF} 
         add eax, edx;
      jmp @forxloop4;

      @loopend4:
      sub esi, 32;
      jz @nextline4;

      // handle the missing columns
      @forxloop:
         {$IFDEF FPC}vmovsd xmm0, [ecx + esi];{$ELSE}db $C5,$FB,$10,$04,$31;{$ENDIF} 
         add ecx, ebx;
         {$IFDEF FPC}vmovsd [eax], xmm0;{$ELSE}db $C5,$FB,$11,$00;{$ENDIF} 

         {$IFDEF FPC}vmovsd xmm0, [ecx + esi];{$ELSE}db $C5,$FB,$10,$04,$31;{$ENDIF} 
         {$IFDEF FPC}vmovsd [eax + 8], xmm0;{$ELSE}db $C5,$FB,$11,$40,$08;{$ENDIF} 
         add ecx, ebx;

         {$IFDEF FPC}vmovsd xmm0, [ecx + esi];{$ELSE}db $C5,$FB,$10,$04,$31;{$ENDIF} 
         {$IFDEF FPC}vmovsd [eax + 16], xmm0;{$ELSE}db $C5,$FB,$11,$40,$10;{$ENDIF} 
         add ecx, ebx;

         {$IFDEF FPC}vmovsd xmm0, [ecx + esi];{$ELSE}db $C5,$FB,$10,$04,$31;{$ENDIF} 
         add ecx, ebx;
         {$IFDEF FPC}vmovsd [eax + 24], xmm0;{$ELSE}db $C5,$FB,$11,$40,$18;{$ENDIF} 
         add eax, edx;

         shl ebx, 2;
         sub ecx, ebx;
         shr ebx, 2;
      add esi, 8;
      jnz @forxloop;

      @nextline4:

      pop eax;
      lea ecx, [ecx + 4*ebx];
      // dest := dest + 4*sizeof(double)
      add eax, 32;

   sub edi, 4;
   jge @foryloop4;

   @@foryloop4End:

   add edi, 4;
   jz @endproc;

   // #############################
   // #### handle the last max 4 rows
   @@foryloop:
      mov esi, iter;
      push eax;

      @@forxloop:
         {$IFDEF FPC}vmovsd xmm0, [ecx + esi];{$ELSE}db $C5,$FB,$10,$04,$31;{$ENDIF} 
         {$IFDEF FPC}vmovsd [eax], xmm0;{$ELSE}db $C5,$FB,$11,$00;{$ENDIF} 
         add eax, edx;
      add esi, 8;
      jnz @@forxloop;

      pop eax;
      add ecx, LineWidth;
      add eax, 8;
   dec edi;
   jnz @@foryloop;

   @endproc:

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;

procedure AVXMatrixTransposeUnaligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt : PDouble; const LineWidth : TASMNativeInt; width : TASMNativeInt; height : TASMNativeInt);
var iter : TASMNativeInt;
asm
   // prolog:
   push ebx;
   push edi;
   push esi;

   // iters := -width*sizeof(double);
   mov edi, width;
   imul edi, -8;
   mov iter, edi;

   sub ecx, edi;
   mov ebx, LineWidth;

   mov edi, Height;
   sub edi, 4;
   jl @@foryloop4End;

   // we have at least a height of 4 -> try to transpose 4x4 blocks
   @foryloop4:
      // prepare pointers to mt
      push eax;

      // 4x4 blockwise transposition
      mov esi, iter;
      @forxloop4:
         add esi, 32;
         jg @loopend4;

         {$IFDEF FPC}vmovupd ymm0, [ecx + esi - 32];{$ELSE}db $C5,$FD,$10,$44,$31,$E0;{$ENDIF} 
         add ecx, ebx;
         {$IFDEF FPC}vmovupd ymm1, [ecx + esi - 32];{$ELSE}db $C5,$FD,$10,$4C,$31,$E0;{$ENDIF} 
         add ecx, ebx;
         {$IFDEF FPC}vmovupd ymm2, [ecx + esi - 32];{$ELSE}db $C5,$FD,$10,$54,$31,$E0;{$ENDIF} 
         add ecx, ebx;
         {$IFDEF FPC}vmovupd ymm3, [ecx + esi - 32];{$ELSE}db $C5,$FD,$10,$5C,$31,$E0;{$ENDIF} 
         add ecx, ebx;

         {$IFDEF FPC}vunpckhpd ymm4, ymm0, ymm1;{$ELSE}db $C5,$FD,$15,$E1;{$ENDIF} 
         {$IFDEF FPC}vunpckhpd ymm5, ymm2, ymm3;{$ELSE}db $C5,$ED,$15,$EB;{$ENDIF} 
         {$IFDEF FPC}vunpcklpd ymm7, ymm2, ymm3;{$ELSE}db $C5,$ED,$14,$FB;{$ENDIF} 

         shl ebx, 2;
         {$IFDEF FPC}vperm2f128 ymm3, ymm4, ymm5, $31;{$ELSE}db $C4,$E3,$5D,$06,$DD,$31;{$ENDIF} 

         {$IFDEF FPC}vunpcklpd ymm6, ymm0, ymm1;{$ELSE}db $C5,$FD,$14,$F1;{$ENDIF} 
         {$IFDEF FPC}vinsertf128 ymm1, ymm4, xmm5, 1;{$ELSE}db $C4,$E3,$5D,$18,$CD,$01;{$ENDIF} 
         {$IFDEF FPC}vperm2f128 ymm2, ymm6, ymm7, $31;{$ELSE}db $C4,$E3,$4D,$06,$D7,$31;{$ENDIF} 
         {$IFDEF FPC}vinsertf128 ymm0, ymm6, xmm7, 1;{$ELSE}db $C4,$E3,$4D,$18,$C7,$01;{$ENDIF} 

         sub ecx, ebx;

         // invert so lea works
         {$IFDEF FPC}vmovupd [eax], ymm0;{$ELSE}db $C5,$FD,$11,$00;{$ENDIF} 
         add eax, edx;
         {$IFDEF FPC}vmovupd [eax], ymm1;{$ELSE}db $C5,$FD,$11,$08;{$ENDIF} 
         add eax, edx;
         {$IFDEF FPC}vmovupd [eax], ymm2;{$ELSE}db $C5,$FD,$11,$10;{$ENDIF} 
         add eax, edx;
         {$IFDEF FPC}vmovupd [eax], ymm3;{$ELSE}db $C5,$FD,$11,$18;{$ENDIF} 
         add eax, edx;

         shr ebx, 2;
      jmp @forxloop4;

      @loopend4:
      sub esi, 32;
      jz @nextline4;

      // handle the missing columns
      @forxloop:
         {$IFDEF FPC}vmovsd xmm0, [ecx + esi];{$ELSE}db $C5,$FB,$10,$04,$31;{$ENDIF} 
         add ecx, ebx;
         {$IFDEF FPC}vmovsd [eax], xmm0;{$ELSE}db $C5,$FB,$11,$00;{$ENDIF} 

         {$IFDEF FPC}vmovsd xmm0, [ecx + esi];{$ELSE}db $C5,$FB,$10,$04,$31;{$ENDIF} 
         add ecx, ebx;
         {$IFDEF FPC}vmovsd [eax + 8], xmm0;{$ELSE}db $C5,$FB,$11,$40,$08;{$ENDIF} 


         {$IFDEF FPC}vmovsd xmm0, [ecx + esi];{$ELSE}db $C5,$FB,$10,$04,$31;{$ENDIF} 
         add ecx, ebx;
         {$IFDEF FPC}vmovsd [eax + 16], xmm0;{$ELSE}db $C5,$FB,$11,$40,$10;{$ENDIF} 

         {$IFDEF FPC}vmovsd xmm0, [ecx + esi];{$ELSE}db $C5,$FB,$10,$04,$31;{$ENDIF} 
         add ecx, ebx;
         {$IFDEF FPC}vmovsd [eax + 24], xmm0;{$ELSE}db $C5,$FB,$11,$40,$18;{$ENDIF} 
         add eax, edx;

         shl ebx, 2;
         sub ecx, ebx;
         shr ebx, 2;

      add esi, 8;
      jnz @forxloop;

      @nextline4:

      pop eax;
      lea ecx, [ecx + 4*ebx];
      // dest := dest + 4*sizeof(double)
      add eax, 32;

   sub edi, 4;
   jge @foryloop4;

   @@foryloop4End:

   add edi, 4;
   jz @endproc;

   // #############################
   // #### handle the last max 4 rows
   @@foryloop:
      mov esi, iter;
      push eax;

      @@forxloop:
         {$IFDEF FPC}vmovsd xmm0, [ecx + esi];{$ELSE}db $C5,$FB,$10,$04,$31;{$ENDIF} 
         {$IFDEF FPC}vmovsd [eax], xmm0;{$ELSE}db $C5,$FB,$11,$00;{$ENDIF} 
         add eax, edx;
      add esi, 8;
      jnz @@forxloop;

      pop eax;
      add ecx, LineWidth;
      add eax, 8;
   dec edi;
   jnz @@foryloop;

   @endproc:

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;

// simple Inplace Trasnposition of an N x N matrix
procedure AVXMatrixTransposeInplace(mt : PDouble; const LineWidth : TASMNativeInt; N : TASMNativeInt);
// eax = mt, edx = LineWidth, ecx = N
var aN : TASMNativeInt;
asm
   push ebx;
   push edi;
   push esi;

   mov ecx, N;
   cmp ecx, 2;
   jl @@exitProc;

   // iter: -N*sizeof(Double)
   mov edi, ecx;
   imul edi, -8;

   dec ecx;
   mov aN, ecx;

   mov ebx, eax;  // pDest1: genptr(mt, 0, 1, linewidth)
   add ebx, edx;
   sub eax, edi;  // mt + iter

   // for y := 0 to n - 2

   @@foryloop:

      mov esi, edi; // iter aka x
      add esi, 8;
      mov ecx, ebx;
      // for x := y + 1 to n-1 do
      @@forxloop:
         {$IFDEF FPC}vmovsd xmm0, [eax + esi];{$ELSE}db $C5,$FB,$10,$04,$30;{$ENDIF} 
         {$IFDEF FPC}vmovsd xmm1, [ecx];{$ELSE}db $C5,$FB,$10,$09;{$ENDIF} 

         {$IFDEF FPC}vmovsd [eax + esi], xmm1;{$ELSE}db $C5,$FB,$11,$0C,$30;{$ENDIF} 
         {$IFDEF FPC}vmovsd [ecx], xmm0;{$ELSE}db $C5,$FB,$11,$01;{$ENDIF} 

         add ecx, edx;
      add esi, 8;
      jnz @@forxloop;

      add edi, 8;  // iter + sizeof(double);
      //pDest := PConstDoubleArr( GenPtr(dest, 0, y, destLineWidth) );
      add eax, edx;
      // GenPtr(dest, y, y + 1, destLineWidth);
      add ebx, edx;
      add ebx, 8;
   dec aN;
   jnz @@foryloop;

   @@exitProc:

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;

{$ENDIF}

end.
