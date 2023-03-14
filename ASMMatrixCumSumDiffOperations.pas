// ###################################################################
// #### This file is part of the mathematics library project, and is
// #### offered under the licence agreement described on
// #### http://www.mrsoft.org/
// ####
// #### Copyright:(c) 2011, Michael R. . All rights reserved.
// ####
// #### Unless required by applicable law or agreed to in writing, software
// #### distributed under the License is distributed on an "AS IS" BASIS,
// #### WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// #### See the License for the specific language governing permissions and
// #### limitations under the License.
// ###################################################################

unit ASMMatrixCumSumDiffOperations;

interface


{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}
{$IFNDEF x64}

uses MatrixConst;

procedure ASMMatrixCumulativeSumRow(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

procedure ASMMatrixCumulativeSumColumnEvenWUnaligned(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixCumulativeSumColumnOddWUnaligned(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixCumulativeSumColumnEvenWAligned(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixCumulativeSumColumnOddWAligned(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

procedure ASMMatrixDifferentiateRow(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

procedure ASMMatrixDifferentiateColumnEvenWUnaligned(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixDifferentiateColumnOddWUnaligned(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixDifferentiateColumnEvenWAligned(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixDifferentiateColumnOddWAligned(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

{$ENDIF}

implementation

{$IFNDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$S-} {$ENDIF}

// ##################################################
// #### Cumulative sum
// ##################################################

procedure ASMMatrixCumulativeSumRow(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt);
asm
   push ebx;
   push esi;
   push edi;

   // if (width <= 0) or (height <= 0) then exit;
   mov edi, width;
   cmp edi, 0;
   jle @@exitproc;
   mov esi, height;
   cmp esi, 0;
   jle @@exitproc;

   // iter := -width*sizeof(Double)
   mov ebx, width;
   imul ebx, -8;

   // prepare counters
   sub eax, ebx;
   sub ecx, ebx;

   @@foryloop:
      mov edi, ebx;
      xorpd xmm0, xmm0;

      @@forxloop:
         addsd xmm0, [ecx + edi];
         movsd [eax + edi], xmm0;
      add edi, 8;
      jnz @@forxloop;

      add ecx, srcLineWidth;
      add eax, edx;
   dec esi;
   jnz @@foryloop;


   @@exitProc:
        
   pop edi;
   pop esi;
   pop ebx;
end;

procedure ASMMatrixCumulativeSumColumnEvenWUnaligned(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt);
asm
   push ebx;
   push edi;
   push esi;

   // if (width <= 0) or (height <= 0) then exit;
   mov edi, height;
   cmp edi, 0;
   jle @@exitproc;
   mov esi, width;
   cmp esi, 1;
   jle @@exitproc;

   sar esi, 1;  // width div 2
   mov width, esi;

   // prepare counters
   mov ebx, srcLineWidth;

   @@forxloop:
      mov edi, height;
      xorpd xmm0, xmm0;
      xor esi, esi;

      push ecx;
      
      // two values at once
      @@foryloop:
         movupd xmm1, [ecx];
         addpd xmm0, xmm1;
         movupd [eax + esi], xmm0;

         add ecx, ebx;
         add esi, edx;
      dec edi;
      jnz @@foryloop;

      pop ecx;
      
      add ecx, 16;
      add eax, 16;
   dec width;
   jnz @@forxloop;

   @@exitProc:
   pop esi;
   pop edi;
   pop ebx;
end;

procedure ASMMatrixCumulativeSumColumnOddWUnaligned(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt);
asm
   push ebx;
   push edi;
   push esi;

   // if (width <= 0) or (height <= 0) then exit;
   mov edi, height;
   cmp edi, 0;
   jle @@exitproc;
   mov esi, width;
   cmp esi, 0;
   jle @@exitproc;

   sar esi, 1;  // width div 2
   mov width, esi;

   // prepare counters
   mov ebx, srcLineWidth;

   mov esi, width;
   test esi, esi;
   jz @@lastColumn;

   @@forxloop:
      mov edi, height;
      xorpd xmm0, xmm0;
      xor esi, esi;

      push ecx;
      
      // two values at once
      @@foryloop:
         movupd xmm1, [ecx];
         addpd xmm0, xmm1;
         movupd [eax + esi], xmm0;

         add ecx, ebx;
         add esi, edx;
      dec edi;
      jnz @@foryloop;

      pop ecx;
      add ecx, 16;
      add eax, 16;
   dec width;
   jnz @@forxloop;

   @@lastColumn:

   mov edi, height;
   xorpd xmm0, xmm0;

   // last column
   @@forycolumnloop:
      movsd xmm1, [ecx];
      addsd xmm0, xmm1;
      movsd [eax], xmm0;

      add ecx, ebx;
      add eax, edx;
   dec edi;
   jnz @@forycolumnloop;

   @@exitProc:

   pop esi;
   pop edi;
   pop ebx;
end;

procedure ASMMatrixCumulativeSumColumnEvenWAligned(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt);
asm
   push ebx;
   push edi;
   push esi;

   // if (width <= 0) or (height <= 0) then exit;
   mov edi, height;
   cmp edi, 0;
   jle @@exitproc;
   mov esi, width;
   cmp esi, 1;
   jle @@exitproc;

   sar esi, 1;  // width div 2
   mov width, esi;

   // prepare counters
   mov ebx, srcLineWidth;

   @@forxloop:
      mov edi, height;
      xorpd xmm0, xmm0;
      xor esi, esi;

      push ecx;

      // two values at once
      @@foryloop:
         movapd xmm1, [ecx];
         addpd xmm0, xmm1;
         movapd [eax + esi], xmm0;

         add ecx, ebx;
         add esi, edx;
      dec edi;
      jnz @@foryloop;

      pop ecx;
      add ecx, 16;
      add eax, 16;
   dec width;
   jnz @@forxloop;

   @@exitProc:
   
   pop esi;
   pop edi;
   pop ebx;
end;


procedure ASMMatrixCumulativeSumColumnOddWAligned(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt);
asm
   push ebx;
   push edi;
   push esi;

   // if (width <= 0) or (height <= 0) then exit;
   mov edi, height;
   cmp edi, 0;
   jle @@exitproc;
   mov esi, width;
   cmp esi, 0;
   jle @@exitproc;

   sar esi, 1;  // width div 2
   mov width, esi;

   // prepare counters
   mov eax, dest;
   mov ecx, src;
   mov ebx, srcLineWidth;

   mov esi, width;
   test esi, esi;
   jz @@lastColumn;

   @@forxloop:
      mov edi, height;
      xorpd xmm0, xmm0;
      xor esi, esi;
      push ecx;

      // two values at once
      @@foryloop:
         movapd xmm1, [ecx];
         addpd xmm0, xmm1;
         movapd [eax + esi], xmm0;

         add ecx, ebx;
         add esi, edx;
      dec edi;
      jnz @@foryloop;

      pop ecx;
      add ecx, 16;
      add eax, 16;
   dec width;
   jnz @@forxloop;

   @@lastColumn:

   mov edi, height;
   xorpd xmm0, xmm0;

   // last column
   @@forycolumnloop:
      movsd xmm1, [ecx];
      addsd xmm0, xmm1;
      movsd [eax], xmm0;

      add ecx, ebx;
      add eax, edx;
   dec edi;
   jnz @@forycolumnloop;

   @@exitProc:

   pop esi;
   pop edi;
   pop ebx;
end;

procedure ASMMatrixDifferentiateRow(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt);
asm
   push ebx;
   push edi;
   push esi;
        
   // if (width <= 1) or (height <= 0) then exit;
   mov edi, width;
   cmp edi, 1;
   jle @@exitproc;
   mov esi, height;
   cmp esi, 0;
   jle @@exitproc;

   // iter := -width*sizeof(Double)
   mov ebx, width;
   imul ebx, -8;

   // prepare counters
   sub eax, ebx;
   sub ecx, ebx;
   add ebx, 8;

   @@foryloop:
      mov edi, ebx;
      movsd xmm1, [ecx + edi - 8];

      @@forxloop:
         movsd xmm0, [ecx + edi];
         movsd xmm2, xmm0;
         subsd xmm0, xmm1;
         movsd [eax + edi - 8], xmm0;

         movsd xmm1, xmm2;
      add edi, 8;
      jnz @@forxloop;

      add ecx, srcLineWidth;
      add eax, edx;
   dec esi;
   jnz @@foryloop;

   @@exitProc:
   pop esi;
   pop edi;
   pop ebx;
end;

procedure ASMMatrixDifferentiateColumnEvenWUnaligned(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt);
asm
   push ebx;
   push edi;
   push esi;

   // if (width <= 0) or (height <= 1) then exit;
   mov edi, height;
   dec edi;
   cmp edi, 0;
   jle @@exitproc;
   mov esi, width;
   cmp esi, 1;
   jle @@exitproc;

   mov height, edi;

   sar esi, 1;  // width div 2
   mov width, esi;

   // prepare counters
   mov ebx, srcLineWidth;

   @@forxloop:
      mov edi, height;
      xorpd xmm0, xmm0;
      push ecx;
      xor esi, esi;

      movupd xmm0, [ecx];
      add ecx, ebx;

      // two values at once
      @@foryloop:
         movupd xmm1, [ecx];
         movapd xmm2, xmm1;
         subpd xmm1, xmm0;
         movupd [eax + esi], xmm1;

         movapd xmm0, xmm2;
         add ecx, ebx;
         add esi, edx;
      dec edi;
      jnz @@foryloop;

      pop ecx;
      add ecx, 16;
      add eax, 16;
   dec width;
   jnz @@forxloop;

   @@exitProc:

   pop esi;
   pop edi;
   pop ebx;
end;


procedure ASMMatrixDifferentiateColumnOddWUnaligned(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt);
asm
   push ebx;
   push edi;
   push esi;

   // if (width <= 0) or (height <= 1) then exit;
   mov edi, height;
   dec edi;
   cmp edi, 0;
   jle @@exitproc;
   mov esi, width;
   cmp esi, 0;
   jle @@exitproc;

   mov height, edi;

   // prepare counters
   mov ebx, srcLineWidth;

   sar esi, 1;  // width div 2
   cmp esi, 0;
   je @@lastcolumn;
   mov width, esi;

   @@forxloop:
      mov edi, height;
      xorpd xmm0, xmm0;
      push ecx;
      xor esi, esi;

      movupd xmm0, [ecx];
      add ecx, ebx;

      // two values at once
      @@foryloop:
         movupd xmm1, [ecx];
         movapd xmm2, xmm1;
         subpd xmm1, xmm0;
         movupd [eax + esi], xmm1;

         movapd xmm0, xmm2;
         add ecx, ebx;
         add esi, edx;
      dec edi;
      jnz @@foryloop;

      pop ecx;
      add ecx, 16;
      add eax, 16;
   dec width;
   jnz @@forxloop;

   @@lastcolumn:

   mov edi, height;
   xorpd xmm0, xmm0;
   xor esi, esi;

   movsd xmm0, [ecx];
   add ecx, ebx;

   // two values at once
   @@foryloop1:
      movsd xmm1, [ecx];
      movapd xmm2, xmm1;
      subsd xmm1, xmm0;
      movsd [eax + esi], xmm1;

      movapd xmm0, xmm2;
      add ecx, ebx;
      add esi, edx;
   dec edi;
   jnz @@foryloop1;

   @@exitProc:

   pop esi;
   pop edi;
   pop ebx;
end;

procedure ASMMatrixDifferentiateColumnEvenWAligned(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt);
asm
   push ebx;
   push edi;
   push esi;

   // if (width <= 0) or (height <= 1) then exit;
   mov edi, height;
   dec edi;
   cmp edi, 0;
   jle @@exitproc;
   mov esi, width;
   cmp esi, 1;
   jle @@exitproc;

   mov height, edi;

   sar esi, 1;  // width div 2
   mov width, esi;

   // prepare counters
   mov ebx, srcLineWidth;

   @@forxloop:
      mov edi, height;
      xorpd xmm0, xmm0;
      push ecx;
      xor esi, esi;

      movapd xmm0, [ecx];
      add ecx, ebx;

      // two values at once
      @@foryloop:
         movapd xmm1, [ecx];
         movapd xmm2, xmm1;
         subpd xmm1, xmm0;
         movapd [eax + esi], xmm1;

         movapd xmm0, xmm2;
         add ecx, ebx;
         add esi, edx;
      dec edi;
      jnz @@foryloop;

      pop ecx;
      add ecx, 16;
      add eax, 16;
   dec width;
   jnz @@forxloop;

   @@exitProc:
   
   pop esi;
   pop edi;
   pop ebx;
end;

procedure ASMMatrixDifferentiateColumnOddWAligned(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt);
asm
   push ebx;
   push edi;
   push esi;

   // if (width <= 0) or (height <= 1) then exit;
   mov edi, height;
   dec edi;
   cmp edi, 0;
   jle @@exitproc;
   mov esi, width;
   cmp esi, 0;
   jle @@exitproc;

   mov height, edi;

   // prepare counters
   mov ebx, srcLineWidth;

   sar esi, 1;  // width div 2
   cmp esi, 0;
   je @@lastcolumn;
   mov width, esi;

   @@forxloop:
      mov edi, height;
      xorpd xmm0, xmm0;
      push ecx;
      xor esi, esi;

      movapd xmm0, [ecx];
      add ecx, ebx;

      // two values at once
      @@foryloop:
         movapd xmm1, [ecx];
         movapd xmm2, xmm1;
         subpd xmm1, xmm0;
         movapd [eax + esi], xmm1;

         movapd xmm0, xmm2;
         add ecx, ebx;
         add esi, edx;
      dec edi;
      jnz @@foryloop;

      pop ecx;
      add ecx, 16;
      add eax, 16;
   dec width;
   jnz @@forxloop;

   @@lastcolumn:

   mov edi, height;
   xorpd xmm0, xmm0;
   xor esi, esi;

   movsd xmm0, [ecx];
   add ecx, ebx;

   // two values at once
   @@foryloop1:
      movsd xmm1, [ecx];
      movapd xmm2, xmm1;
      subsd xmm1, xmm0;
      movsd [eax + esi], xmm1;

      movapd xmm0, xmm2;
      add ecx, ebx;
      add esi, edx;
   dec edi;
   jnz @@foryloop1;

   @@exitProc:

   pop esi;
   pop edi;
   pop ebx;
end;

{$ENDIF}

end.
