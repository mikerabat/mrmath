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


unit ASMMatrixMeanOperations;

interface

{$I 'mrMath_CPU.inc'}

{$IFNDEF x64}

uses MatrixConst;

procedure ASMMatrixMeanRowAlignedEvenW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixMeanRowUnAlignedEvenW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixMeanRowAlignedOddW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixMeanRowUnAlignedOddW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

procedure ASMMatrixMeanColumnAlignedEvenW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixMeanColumnUnAlignedEvenW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixMeanColumnAlignedOddW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixMeanColumnUnAlignedOddW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

procedure ASMMatrixVarRowAlignedEvenW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; unbiased : boolean); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixVarRowUnAlignedEvenW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; unbiased : boolean); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixVarRowAlignedOddW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; unbiased : boolean); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixVarRowUnAlignedOddW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; unbiased : boolean); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

procedure ASMMatrixVarColumnAlignedEvenW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; unbiased : boolean); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixVarColumnUnAlignedEvenW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; unbiased : boolean); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixVarColumnAlignedOddW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; unbiased : boolean); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixVarColumnUnAlignedOddW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; unbiased : boolean); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}


// combined mean and variance calculation
procedure ASMMatrixMeanVarRowAlignedEvenW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; unbiased : boolean); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixMeanVarRowUnAlignedEvenW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; unbiased : boolean); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixMeanVarRowAlignedOddW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; unbiased : boolean); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixMeanVarRowUnAlignedOddW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; unbiased : boolean); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

procedure ASMMatrixMeanVarColumnAlignedEvenW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; unbiased : boolean); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixMeanVarColumnUnAlignedEvenW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; unbiased : boolean); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixMeanVarColumnAlignedOddW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; unbiased : boolean); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixMeanVarColumnUnAlignedOddW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; unbiased : boolean); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}


{$ENDIF}

implementation

{$IFNDEF x64}

const cLocOne : double = 1;
      cLocOnes : Array[0..1] of double = (1, 1);

procedure ASMMatrixMeanRowAlignedEvenW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt);
asm
   push ebx;
   push edi;
   push esi;
        
   // iters := -width*sizeof(double);
   mov ebx, width;
   cvtsi2sd xmm5, ebx;
   imul ebx, -8;
        
   // helper registers for the mt1, mt2 and dest pointers
   sub ecx, ebx;

   // for y := 0 to height - 1:
   mov esi, Height;
   @@addforyloop:
       xorpd xmm0, xmm0;
       xorpd xmm1, xmm1;
       xorpd xmm2, xmm2;
       xorpd xmm3, xmm3;

       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, ebx;
       @addforxloop:
           add edi, 128;
           jg @loopEnd;
           // prefetch data...
           // prefetch [ecx + edi];

           // addition:
           addpd xmm0, [ecx + edi - 128];
           addpd xmm1, [ecx + edi - 112];
           addpd xmm2, [ecx + edi - 96];
           addpd xmm3, [ecx + edi - 80];
           addpd xmm0, [ecx + edi - 64];
           addpd xmm1, [ecx + edi - 48];
           addpd xmm2, [ecx + edi - 32];
           addpd xmm3, [ecx + edi - 16];
       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @buildRes;

       @addforxloop2:
           addpd xmm0, [ecx + edi];
       add edi, 16;
       jnz @addforxloop2;

       @buildRes:

       addpd xmm0, xmm1;
       addpd xmm2, xmm3;
       addpd xmm0, xmm2;

       // build result
       haddpd xmm0, xmm0;

       divsd xmm0, xmm5;

       // write result
       movsd [eax], xmm0;

       // next line:
       add ecx, srcLineWidth;
       add eax, edx;

   // loop y end
   dec esi;
   jnz @@addforyloop;

   pop esi;
   pop edi;
   pop ebx;
end;

procedure ASMMatrixMeanRowUnAlignedEvenW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt);
asm
   push ebx;
   push edi;
   push esi;
        
   // iters := -width*sizeof(double);
   mov ebx, width;
   cvtsi2sd xmm5, ebx;
   imul ebx, -8;
        
   // helper registers for the mt1, mt2 and dest pointers
   sub ecx, ebx;

   // for y := 0 to height - 1:
   mov esi, Height;
   @@addforyloop:
       xorpd xmm7, xmm7;

       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, ebx;
       @addforxloop:
           add edi, 128;
           jg @loopEnd;

           // addition:
           movupd xmm0, [ecx + edi - 128];
           addpd xmm7, xmm0;

           movupd xmm1, [ecx + edi - 112];
           addpd xmm7, xmm1;

           movupd xmm2, [ecx + edi - 96];
           addpd xmm7, xmm2;

           movupd xmm3, [ecx + edi - 80];
           addpd xmm7, xmm3;

           movupd xmm0, [ecx + edi - 64];
           addpd xmm7, xmm0;

           movupd xmm1, [ecx + edi - 48];
           addpd xmm7, xmm1;

           movupd xmm2, [ecx + edi - 32];
           addpd xmm7, xmm2;

           movupd xmm3, [ecx + edi - 16];
           addpd xmm7, xmm3;
       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @buildRes;

       @addforxloop2:
           movupd xmm0, [ecx + edi];
           addpd xmm7, xmm0;
       add edi, 16;
       jnz @addforxloop2;

       @buildRes:

       // build result
       haddpd xmm7, xmm7;
       divsd xmm7, xmm5;

       movsd [eax], xmm7;

       // next line:
       add ecx, srcLineWidth;
       add eax, edx;

   // loop y end
   dec esi;
   jnz @@addforyloop;

   pop esi;
   pop edi;
   pop ebx;
end;

procedure ASMMatrixMeanRowAlignedOddW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt);
asm
   push ebx;
   push edi;
   push esi;
        
   // iters := -width*sizeof(double);
   mov ebx, width;
   cvtsi2sd xmm5, ebx;
   dec ebx;
   imul ebx, -8;
        
   // helper registers for the mt1, mt2 and dest pointers
   sub ecx, ebx;
   // for y := 0 to height - 1:
   mov esi, Height;
   @@addforyloop:
       xorpd xmm0, xmm0;
       xorpd xmm1, xmm1;
       xorpd xmm2, xmm2;
       xorpd xmm3, xmm3;

       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, ebx;
       @addforxloop:
           add edi, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [ecx + edi];

           // addition:
           addpd xmm0, [ecx + edi - 128];
           addpd xmm1, [ecx + edi - 112];
           addpd xmm2, [ecx + edi - 96];
           addpd xmm3, [ecx + edi - 80];
           addpd xmm0, [ecx + edi - 64];
           addpd xmm1, [ecx + edi - 48];
           addpd xmm2, [ecx + edi - 32];
           addpd xmm3, [ecx + edi - 16];
       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @buildRes;

       @addforxloop2:
           addpd xmm0, [ecx + edi];
       add edi, 16;
       jnz @addforxloop2;

       @buildRes:

       addpd xmm0, xmm1;
       addpd xmm2, xmm3;
       addpd xmm0, xmm2;

       // handle last element differently
       movsd xmm2, [ecx + edi];
       addsd xmm0, xmm2;

       // build result
       haddpd xmm0, xmm0;

       divsd xmm0, xmm5;

       movsd [eax], xmm0;

       // next line:
       add ecx, srcLineWidth;
       add eax, edx;

   // loop y end
   dec esi;
   jnz @@addforyloop;

   pop esi;
   pop edi;
   pop ebx;
end;

procedure ASMMatrixMeanRowUnAlignedOddW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt);
asm
   push ebx;
   push edi;
   push esi;
        
   // iters := -width*sizeof(double);
   mov ebx, width;
   cvtsi2sd xmm5, ebx;
   dec ebx;
   imul ebx, -8;
        
   // helper registers for the mt1, mt2 and dest pointers
   sub ecx, ebx;

   // for y := 0 to height - 1:
   mov esi, Height;
   @@addforyloop:
       xorpd xmm7, xmm7;

       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, ebx;
       @addforxloop:
           add edi, 128;
           jg @loopEnd;

           // addition:
           movupd xmm0, [ecx + edi - 128];
           addpd xmm7, xmm0;

           movupd xmm1, [ecx + edi - 112];
           addpd xmm7, xmm1;

           movupd xmm2, [ecx + edi - 96];
           addpd xmm7, xmm2;

           movupd xmm3, [ecx + edi - 80];
           addpd xmm7, xmm3;

           movupd xmm0, [ecx + edi - 64];
           addpd xmm7, xmm0;

           movupd xmm1, [ecx + edi - 48];
           addpd xmm7, xmm1;

           movupd xmm2, [ecx + edi - 32];
           addpd xmm7, xmm2;

           movupd xmm3, [ecx + edi - 16];
           addpd xmm7, xmm3;
       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @buildRes;

       @addforxloop2:
           movupd xmm0, [ecx + edi];
           addpd xmm7, xmm0;
       add edi, 16;
       jnz @addforxloop2;

       @buildRes:

       // handle last element differently
       movsd xmm2, [ecx + edi];
       addsd xmm7, xmm2;

       // build result
       haddpd xmm7, xmm7;

       divsd xmm7, xmm5;

       movsd [eax], xmm7;

       // next line:
       add ecx, srcLineWidth;
       add eax, edx;

   // loop y end
   dec esi;
   jnz @@addforyloop;

   pop esi;
   pop edi;
   pop ebx;
end;

procedure ASMMatrixMeanColumnAlignedEvenW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt);
asm
   push ebx;
   push edi;
   push esi;
        
   // iters := -width*sizeof(double);
   mov edx, srcLineWidth;
   mov ebx, height;
   cvtsi2sd xmm5, ebx;
   imul ebx, edx;
   imul ebx, -1;
        
   // helper registers for the mt1, mt2 and dest pointers
   sub ecx, ebx;

   // helper registers for the mt1, mt2 and dest pointers
   movddup xmm6, xmm5;

   // for x := 0 to width - 1:
   mov esi, Width;
   sar esi, 1;
   @@addforxloop:
       xorpd xmm7, xmm7;

       // for y := 0 to height - 1;
       // prepare for reverse loop
       mov edi, ebx;
       @addforyloop:
           addpd xmm7, [ecx + edi];
       add edi, edx;
       jnz @addforyloop;

       // build result
       divpd xmm7, xmm6;
       movapd [eax], xmm7;

       // next columns:
       add ecx, 16;
       add eax, 16;

   // loop x end
   dec esi;
   jnz @@addforxloop;

   pop esi;
   pop edi;
   pop ebx;
end;

procedure ASMMatrixMeanColumnUnAlignedEvenW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt);
asm
   push ebx;
   push edi;
   push esi;
        
   // iters := -width*sizeof(double);
   mov edx, srcLineWidth;
   mov ebx, height;
   cvtsi2sd xmm5, ebx;
   imul ebx, edx;
   imul ebx, -1;
        
   // helper registers for the mt1, mt2 and dest pointers
   sub ecx, ebx;

   // helper registers for the mt1, mt2 and dest pointers
   movddup xmm6, xmm5;

   // for x := 0 to width - 1:
   mov esi, Width;
   sar esi, 1;
   @@addforxloop:
       xorpd xmm7, xmm7;

       // for y := 0 to height - 1;
       // prepare for reverse loop
       mov edi, ebx;
       @addforyloop:
           movupd xmm0, [ecx + edi];
           addpd xmm7, xmm0;
       add edi, edx;
       jnz @addforyloop;

       // build result
       divpd xmm7, xmm6;
       movupd [eax], xmm7;

       // next columns:
       add ecx, 16;
       add eax, 16;

   // loop x end
   dec esi;
   jnz @@addforxloop;

   pop esi;
   pop edi;
   pop ebx;
end;

procedure ASMMatrixMeanColumnAlignedOddW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt);
asm
   push ebx;
   push edi;
   push esi;
        
   // iters := -width*sizeof(double);
   mov edx, srcLineWidth;
   mov ebx, height;
   cvtsi2sd xmm5, ebx;
   imul ebx, edx;
   imul ebx, -1;
        
   sub ecx, ebx;
   
   movddup xmm6, xmm5;

   // for x := 0 to width - 1:
   mov esi, Width;
   sar esi, 1;
   jz @lastColumn;
   @@addforxloop:
       xorpd xmm7, xmm7;

       // for y := 0 to height - 1;
       // prepare for reverse loop
       mov edi, ebx;
       @addforyloop:
           addpd xmm7, [ecx + edi];
       add edi, edx;
       jnz @addforyloop;

       // build result
       divpd xmm7, xmm6;
       movapd [eax], xmm7;

       // next columns:
       add ecx, 16;
       add eax, 16;

   // loop x end
   dec esi;
   jnz @@addforxloop;

   @lastColumn:
   // handle last column
   xorpd xmm7, xmm7;

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov edi, ebx;
   @addforyloop3:
       movsd xmm0, [ecx + edi];
       addsd xmm7, xmm0;
   add edi, edx;
   jnz @addforyloop3;

   // build result
   divsd xmm7, xmm6;

   movsd [eax], xmm7;

   pop esi;
   pop edi;
   pop ebx;
end;

procedure ASMMatrixMeanColumnUnAlignedOddW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt);
asm
   push ebx;
   push edi;
   push esi;
        
   // iters := -width*sizeof(double);
   mov edx, srcLineWidth;
   mov ebx, height;
   cvtsi2sd xmm5, ebx;
   imul ebx, edx;
   imul ebx, -1;
        
   sub ecx, ebx;
   movddup xmm6, xmm5;

   // for x := 0 to width - 1:
   mov esi, Width;
   sar esi, 1;
   jz @lastColumn;
   @@addforxloop:
       xorpd xmm7, xmm7;

       // for y := 0 to height - 1;
       // prepare for reverse loop
       mov edi, ebx;
       @addforyloop:
           movupd xmm0, [ecx + edi];
           addpd xmm7, xmm0;
       add edi, edx;
       jnz @addforyloop;

       // build result
       divpd xmm7, xmm6;
       movupd [eax], xmm7;

       // next columns:
       add ecx, 16;
       add eax, 16;

   // loop x end
   dec esi;
   jnz @@addforxloop;

   @lastColumn:
   // handle last column
   xorpd xmm7, xmm7;

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov edi, ebx;
   @addforyloop3:
       movsd xmm0, [ecx + edi];
       addsd xmm7, xmm0;
   add edi, edx;
   jnz @addforyloop3;

   // build result
   divsd xmm7, xmm6;
   movsd [eax], xmm7;

   pop esi;
   pop edi;
   pop ebx;
end;


// ###########################################
// #### Variance calculation
// ###########################################

procedure ASMMatrixVarRowAlignedEvenW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; unbiased : boolean);
asm
   push ebx;
   push esi;
   push edi;

   mov ebx, width;
   cvtsi2sd xmm5, ebx;
   imul ebx, -8;
        
   // helper registers for the mt1, mt2 and dest pointers
   sub ecx, ebx;

   // for y := 0 to height - 1:
   mov esi, Height;
   @@addforyloop:
       xorpd xmm0, xmm0;
       xorpd xmm1, xmm1;
       xorpd xmm2, xmm2;
       xorpd xmm3, xmm3;

       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, ebx;
       @addforxloop:
           add edi, 128;
           jg @loopEnd;
           // prefetch data...
           // prefetch [ecx + edi];

           // addition:
           addpd xmm0, [ecx + edi - 128];
           addpd xmm1, [ecx + edi - 112];
           addpd xmm2, [ecx + edi - 96];
           addpd xmm3, [ecx + edi - 80];
           addpd xmm0, [ecx + edi - 64];
           addpd xmm1, [ecx + edi - 48];
           addpd xmm2, [ecx + edi - 32];
           addpd xmm3, [ecx + edi - 16];
       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @buildRes;

       @addforxloop2:
           addpd xmm0, [ecx + edi];
       add edi, 16;
       jnz @addforxloop2;

       @buildRes:

       addpd xmm0, xmm1;
       addpd xmm2, xmm3;
       addpd xmm0, xmm2;

       // build result
       haddpd xmm0, xmm0;

       divsd xmm0, xmm5;
            
       // we have calculated the mean -> 
       // repeat the loop to calculate the variance

       movddup xmm4, xmm0;
       xorpd xmm0, xmm0;
            
       // for x := 0 to w - 1;
       // prepare for reverse loop
       // variance = sum (x - mean)^2
       mov edi, ebx;
       @addforxloop3:
           add edi, 128;
           jg @loopEnd2;
           // prefetch data...
           // prefetch [ecx + edi];

           // addition:
           movapd xmm1, [ecx + edi - 128];
           movapd xmm2, [ecx + edi - 112];
           subpd xmm1, xmm4;
           subpd xmm2, xmm4;
           mulpd xmm1, xmm1;
           mulpd xmm2, xmm2;
           addpd xmm0, xmm1;
           addpd xmm0, xmm2;
                
           movapd xmm1, [ecx + edi - 96];
           movapd xmm2, [ecx + edi - 80];
           subpd xmm1, xmm4;
           subpd xmm2, xmm4;
           mulpd xmm1, xmm1;
           mulpd xmm2, xmm2;
           addpd xmm0, xmm1;
           addpd xmm0, xmm2;
                
           movapd xmm1, [ecx + edi - 64];
           movapd xmm2, [ecx + edi - 48];
           subpd xmm1, xmm4;
           subpd xmm2, xmm4;
           mulpd xmm1, xmm1;
           mulpd xmm2, xmm2;
           addpd xmm0, xmm1;
           addpd xmm0, xmm2;
                
           movapd xmm1, [ecx + edi - 32];
           movapd xmm2, [ecx + edi - 16];
           subpd xmm1, xmm4;
           subpd xmm2, xmm4;
           mulpd xmm1, xmm1;
           mulpd xmm2, xmm2;
           addpd xmm0, xmm1;
           addpd xmm0, xmm2;
                
       jmp @addforxloop3

       @loopEnd2:

       sub edi, 128;

       jz @buildRes2;

       @addforxloop4:
           movapd xmm1, [ecx + edi];
           subpd xmm1, xmm4;
           mulpd xmm1, xmm1;
           addpd xmm0, xmm1;
       add edi, 16;
       jnz @addforxloop4;

       @buildRes2:

       // build result
       movhlps xmm1, xmm0;
       addsd xmm0, xmm1;
            
       // check if we need to use the unbiased version
       cmp unbiased, 0;
       jz @@dobiased;

       movsd xmm4, xmm5;
       subsd xmm4, cLocOne;
       maxsd xmm4, cLocOne;

       divsd xmm0, xmm4;

       jmp @@writeRes;

       @@dobiased:
       divsd xmm0, xmm5;
            
       // write result
       @@writeRes:
       movsd [eax], xmm0;

       // next line:
       add ecx, srcLineWidth;
       add eax, edx;

   // loop y end
   dec esi;
   jnz @@addforyloop;

   pop edi;
   pop esi;
   pop ebx;
end;

procedure ASMMatrixVarRowUnAlignedEvenW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; unbiased : boolean);
asm
   push ebx;
   push esi;
   push edi;

   mov ebx, width;
   cvtsi2sd xmm5, ebx;
   imul ebx, -8;
        
   // helper registers for the mt1, mt2 and dest pointers
   sub ecx, ebx;

   // for y := 0 to height - 1:
   mov esi, Height;
   @@addforyloop:
       xorpd xmm7, xmm7;

       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, ebx;
       @addforxloop:
           add edi, 128;
           jg @loopEnd;

           // addition:
           movupd xmm0, [ecx + edi - 128];
           addpd xmm7, xmm0;

           movupd xmm1, [ecx + edi - 112];
           addpd xmm7, xmm1;

           movupd xmm2, [ecx + edi - 96];
           addpd xmm7, xmm2;

           movupd xmm3, [ecx + edi - 80];
           addpd xmm7, xmm3;

           movupd xmm0, [ecx + edi - 64];
           addpd xmm7, xmm0;

           movupd xmm1, [ecx + edi - 48];
           addpd xmm7, xmm1;

           movupd xmm2, [ecx + edi - 32];
           addpd xmm7, xmm2;

           movupd xmm3, [ecx + edi - 16];
           addpd xmm7, xmm3;
       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @buildRes;

       @addforxloop2:
           movupd xmm0, [ecx + edi];
           addpd xmm7, xmm0;
       add edi, 16;
       jnz @addforxloop2;

       @buildRes:

       // build result
       haddpd xmm7, xmm7;
       divsd xmm7, xmm5;

       // we have calculated the mean -> 
       // repeat the loop to calculate the variance
       movddup xmm4, xmm7;
       xorpd xmm0, xmm0;
            
       // for x := 0 to w - 1;
       // prepare for reverse loop
       // variance = sum (x - mean)^2
       mov edi, ebx;
       @addforxloop3:
           add edi, 128;
           jg @loopEnd2;
           // prefetch data...
           // prefetch [ecx + edi];

           // addition:
           movupd xmm1, [ecx + edi - 128];
           movupd xmm2, [ecx + edi - 112];
           subpd xmm1, xmm4;
           subpd xmm2, xmm4;
           mulpd xmm1, xmm1;
           mulpd xmm2, xmm2;
           addpd xmm0, xmm1;
           addpd xmm0, xmm2;
                
           movupd xmm1, [ecx + edi - 96];
           movupd xmm2, [ecx + edi - 80];
           subpd xmm1, xmm4;
           subpd xmm2, xmm4;
           mulpd xmm1, xmm1;
           mulpd xmm2, xmm2;
           addpd xmm0, xmm1;
           addpd xmm0, xmm2;
                
           movupd xmm1, [ecx + edi - 64];
           movupd xmm2, [ecx + edi - 48];
           subpd xmm1, xmm4;
           subpd xmm2, xmm4;
           mulpd xmm1, xmm1;
           mulpd xmm2, xmm2;
           addpd xmm0, xmm1;
           addpd xmm0, xmm2;
                
           movupd xmm1, [ecx + edi - 32];
           movupd xmm2, [ecx + edi - 16];
           subpd xmm1, xmm4;
           subpd xmm2, xmm4;
           mulpd xmm1, xmm1;
           mulpd xmm2, xmm2;
           addpd xmm0, xmm1;
           addpd xmm0, xmm2;
                
       jmp @addforxloop3

       @loopEnd2:

       sub edi, 128;

       jz @buildRes2;

       @addforxloop4:
           movupd xmm1, [ecx + edi];
           subpd xmm1, xmm4;
           mulpd xmm1, xmm1;
           addpd xmm0, xmm1;
       add edi, 16;
       jnz @addforxloop4;

       @buildRes2:

       // build result
       movhlps xmm1, xmm0;
       addsd xmm0, xmm1;
            
       // check if we need to use the unbiased version
       cmp unbiased, 0;
       jz @@dobiased;

       movsd xmm4, xmm5;
       subsd xmm4, cLocOne;
       maxsd xmm4, cLocOne;

       divsd xmm0, xmm4;

       jmp @@writeRes;

       @@dobiased:
       divsd xmm0, xmm5;

       // write result
       @@writeRes:
       movsd [eax], xmm0;

       // next line:
       add ecx, srcLineWidth;
       add eax, edx;

   // loop y end
   dec esi;
   jnz @@addforyloop;

   pop edi;
   pop esi;
   pop ebx;
end;

procedure ASMMatrixVarRowAlignedOddW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; unbiased : boolean);
asm
   push ebx;
   push esi;
   push edi;

   mov ebx, width;
   cvtsi2sd xmm5, ebx;
   dec ebx;
   imul ebx, -8;
        
   // helper registers for the mt1, mt2 and dest pointers
   sub ecx, ebx;

   // for y := 0 to height - 1:
   mov esi, Height;
   @@addforyloop:
       xorpd xmm0, xmm0;
       xorpd xmm1, xmm1;
       xorpd xmm2, xmm2;
       xorpd xmm3, xmm3;

       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, ebx;
       @addforxloop:
           add edi, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [ecx + edi];

           // addition:
           addpd xmm0, [ecx + edi - 128];
           addpd xmm1, [ecx + edi - 112];
           addpd xmm2, [ecx + edi - 96];
           addpd xmm3, [ecx + edi - 80];
           addpd xmm0, [ecx + edi - 64];
           addpd xmm1, [ecx + edi - 48];
           addpd xmm2, [ecx + edi - 32];
           addpd xmm3, [ecx + edi - 16];
       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @buildRes;

       @addforxloop2:
           addpd xmm0, [ecx + edi];
       add edi, 16;
       jnz @addforxloop2;

       @buildRes:

       addpd xmm0, xmm1;
       addpd xmm2, xmm3;
       addpd xmm0, xmm2;

       // handle last element differently
       movsd xmm2, [ecx + edi];
       addsd xmm0, xmm2;

       // build result
       movhlps xmm1, xmm0;
       addsd xmm0, xmm1;

       divsd xmm0, xmm5;


       // we have calculated the mean -> 
       // repeat the loop to calculate the variance

       movddup xmm4, xmm0;
       xorpd xmm0, xmm0;
            
       // for x := 0 to w - 1;
       // prepare for reverse loop
       // variance = sum (x - mean)^2
       mov edi, ebx;
       @addforxloop3:
           add edi, 128;
           jg @loopEnd2;
           // prefetch data...
           // prefetch [ecx + edi];

           // addition:
           movapd xmm1, [ecx + edi - 128];
           movapd xmm2, [ecx + edi - 112];
           subpd xmm1, xmm4;
           subpd xmm2, xmm4;
           mulpd xmm1, xmm1;
           mulpd xmm2, xmm2;
           addpd xmm0, xmm1;
           addpd xmm0, xmm2;
                
           movapd xmm1, [ecx + edi - 96];
           movapd xmm2, [ecx + edi - 80];
           subpd xmm1, xmm4;
           subpd xmm2, xmm4;
           mulpd xmm1, xmm1;
           mulpd xmm2, xmm2;
           addpd xmm0, xmm1;
           addpd xmm0, xmm2;
                
           movapd xmm1, [ecx + edi - 64];
           movapd xmm2, [ecx + edi - 48];
           subpd xmm1, xmm4;
           subpd xmm2, xmm4;
           mulpd xmm1, xmm1;
           mulpd xmm2, xmm2;
           addpd xmm0, xmm1;
           addpd xmm0, xmm2;
                
           movapd xmm1, [ecx + edi - 32];
           movapd xmm2, [ecx + edi - 16];
           subpd xmm1, xmm4;
           subpd xmm2, xmm4;
           mulpd xmm1, xmm1;
           mulpd xmm2, xmm2;
           addpd xmm0, xmm1;
           addpd xmm0, xmm2;
                
       jmp @addforxloop3

       @loopEnd2:

       sub edi, 128;

       jz @buildRes2;

       @addforxloop4:
           movapd xmm1, [ecx + edi];
           subpd xmm1, xmm4;
           mulpd xmm1, xmm1;
           addpd xmm0, xmm1;
       add edi, 16;
       jnz @addforxloop4;

       @buildRes2:

       // handle last element differently
       movsd xmm1, [ecx + edi];
       subsd xmm1, xmm4;
       mulsd xmm1, xmm1;
       addsd xmm0, xmm1;

       // build result
       haddpd xmm0, xmm0;
            
       // check if we need to use the unbiased version
       cmp unbiased, 0;
       jz @@dobiased;

       movsd xmm4, xmm5;
       subsd xmm4, cLocOne;
       maxsd xmm4, cLocOne;

       divsd xmm0, xmm4;

       jmp @@writeRes;

       @@dobiased:
       divsd xmm0, xmm5;
            
       // write result
       @@writeRes:
       movsd [eax], xmm0;

       // next line:
       add ecx, srcLineWidth;
       add eax, edx;

   // loop y end
   dec esi;
   jnz @@addforyloop;

   pop edi;
   pop esi;
   pop ebx;
end;

procedure ASMMatrixVarRowUnAlignedOddW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; unbiased : boolean);
asm
   push ebx;
   push esi;
   push edi;

   mov ebx, width;
   cvtsi2sd xmm5, ebx;
   dec ebx;
   imul ebx, -8;
        
   // helper registers for the mt1, mt2 and dest pointers
   sub ecx, ebx;

   // for y := 0 to height - 1:
   mov esi, Height;
   @@addforyloop:
       xorpd xmm7, xmm7;

       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, ebx;
       @addforxloop:
           add edi, 128;
           jg @loopEnd;

           // addition:
           movupd xmm0, [ecx + edi - 128];
           addpd xmm7, xmm0;

           movupd xmm1, [ecx + edi - 112];
           addpd xmm7, xmm1;

           movupd xmm2, [ecx + edi - 96];
           addpd xmm7, xmm2;

           movupd xmm3, [ecx + edi - 80];
           addpd xmm7, xmm3;

           movupd xmm0, [ecx + edi - 64];
           addpd xmm7, xmm0;

           movupd xmm1, [ecx + edi - 48];
           addpd xmm7, xmm1;

           movupd xmm2, [ecx + edi - 32];
           addpd xmm7, xmm2;

           movupd xmm3, [ecx + edi - 16];
           addpd xmm7, xmm3;
       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @buildRes;

       @addforxloop2:
           movupd xmm0, [ecx + edi];
           addpd xmm7, xmm0;
       add edi, 16;
       jnz @addforxloop2;

       @buildRes:

       // handle last element differently
       movsd xmm2, [ecx + edi];
       addsd xmm7, xmm2;

       // build result
       haddpd xmm7, xmm7;

       divsd xmm7, xmm5;


       // we have calculated the mean -> 
       // repeat the loop to calculate the variance

       movddup xmm4, xmm7;
       xorpd xmm0, xmm0;
            
       // for x := 0 to w - 1;
       // prepare for reverse loop
       // variance = sum (x - mean)^2
       mov edi, ebx;
       @addforxloop3:
           add edi, 128;
           jg @loopEnd2;
           // prefetch data...
           // prefetch [ecx + edi];

           // addition:
           movupd xmm1, [ecx + edi - 128];
           movupd xmm2, [ecx + edi - 112];
           subpd xmm1, xmm4;
           subpd xmm2, xmm4;
           mulpd xmm1, xmm1;
           mulpd xmm2, xmm2;
           addpd xmm0, xmm1;
           addpd xmm0, xmm2;
                
           movupd xmm1, [ecx + edi - 96];
           movupd xmm2, [ecx + edi - 80];
           subpd xmm1, xmm4;
           subpd xmm2, xmm4;
           mulpd xmm1, xmm1;
           mulpd xmm2, xmm2;
           addpd xmm0, xmm1;
           addpd xmm0, xmm2;
                
           movupd xmm1, [ecx + edi - 64];
           movupd xmm2, [ecx + edi - 48];
           subpd xmm1, xmm4;
           subpd xmm2, xmm4;
           mulpd xmm1, xmm1;
           mulpd xmm2, xmm2;
           addpd xmm0, xmm1;
           addpd xmm0, xmm2;
                
           movupd xmm1, [ecx + edi - 32];
           movupd xmm2, [ecx + edi - 16];
           subpd xmm1, xmm4;
           subpd xmm2, xmm4;
           mulpd xmm1, xmm1;
           mulpd xmm2, xmm2;
           addpd xmm0, xmm1;
           addpd xmm0, xmm2;
                
       jmp @addforxloop3

       @loopEnd2:

       sub edi, 128;

       jz @buildRes2;

       @addforxloop4:
           movupd xmm1, [ecx + edi];
           subpd xmm1, xmm4;
           mulpd xmm1, xmm1;
           addpd xmm0, xmm1;
       add edi, 16;
       jnz @addforxloop4;

       @buildRes2:

       // handle last element differently
       movsd xmm1, [ecx + edi];
       subsd xmm1, xmm4;
       mulsd xmm1, xmm1;
       addsd xmm0, xmm1;
            
       // build result
       haddpd xmm0, xmm0;
            
       // check if we need to use the unbiased version
       cmp unbiased, 0;
       jz @@dobiased;

       movsd xmm4, xmm5;
       subsd xmm4, cLocOne;
       maxsd xmm4, cLocOne;

       divsd xmm0, xmm4;

       jmp @@writeRes;

       @@dobiased:
       divsd xmm0, xmm5;

       // write result
       @@writeRes:
       movsd [eax], xmm0;

       // next line:
       add ecx, srcLineWidth;
       add eax, edx;

   // loop y end
   dec esi;
   jnz @@addforyloop;

   pop edi;
   pop esi;
   pop ebx;
end;

procedure ASMMatrixVarColumnAlignedEvenW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; unbiased : boolean);
asm
   push ebx;
   push esi;
   push edi;

   mov edx, srcLineWidth;
   mov ebx, height;
   cvtsi2sd xmm5, ebx;
   imul ebx, edx;
   imul ebx, -1;

   // helper registers for the mt1, mt2 and dest pointers
   sub ecx, ebx;
   movupd xmm2, cLocOnes;

   movddup xmm6, xmm5;

   // for x := 0 to width - 1:
   mov esi, Width;
   sar esi, 1;
   @@addforxloop:
       xorpd xmm7, xmm7;

       // calculate mean
            
       // for y := 0 to height - 1;
       // prepare for reverse loop
       mov edi, ebx;
       @addforyloop:
           addpd xmm7, [ecx + edi];
       add edi, edx;
       jnz @addforyloop;

       // build mean
       divpd xmm7, xmm6;

       // calculate final standard deviation
       // for y := 0 to height - 1;
       // prepare for reverse loop
       xorpd xmm0, xmm0;
       mov edi, ebx;
       @addforyloop2:
           movapd xmm1, [ecx + edi];
           subpd xmm1, xmm7;
           mulpd xmm1, xmm1;
           addpd xmm0, xmm1;
       add edi, edx;
       jnz @addforyloop2;

       // check if we need to use the unbiased version
       cmp unbiased, 0;
       jz @@dobiased;

       movapd xmm4, xmm6;
       subpd xmm4, xmm2;
       maxpd xmm4, xmm2;

       divpd xmm0, xmm4;

       jmp @@writeRes;

       @@dobiased:
       divpd xmm0, xmm6;

       // write result
       @@writeRes:
       movapd [eax], xmm0;

       // next columns:
       add ecx, 16;
       add eax, 16;

   // loop x end
   dec esi;
   jnz @@addforxloop;

   pop edi;
   pop esi;
   pop ebx;
end;

procedure ASMMatrixVarColumnUnAlignedEvenW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; unbiased : boolean);
asm
   push ebx;
   push esi;
   push edi;

   mov edx, srcLineWidth;
   mov ebx, height;
   cvtsi2sd xmm5, ebx;
   imul ebx, edx;
   imul ebx, -1;

   // helper registers for the mt1, mt2 and dest pointers
   sub ecx, ebx;
   movupd xmm2, cLocOnes;

   movddup xmm6, xmm5;

   // for x := 0 to width - 1:
   mov esi, Width;
   sar esi, 1;
   @@addforxloop:
       xorpd xmm7, xmm7;

       // for y := 0 to height - 1;
       // prepare for reverse loop
       mov edi, ebx;
       @addforyloop:
           movupd xmm0, [ecx + edi];
           addpd xmm7, xmm0;
       add edi, edx;
       jnz @addforyloop;

       // build mean
       divpd xmm7, xmm6;

       // calculate final standard deviation
       // for y := 0 to height - 1;
       // prepare for reverse loop
       xorpd xmm0, xmm0;
       mov edi, ebx;
       @addforyloop2:
           movupd xmm1, [ecx + edi];
           subpd xmm1, xmm7;
           mulpd xmm1, xmm1;
           addpd xmm0, xmm1;
       add edi, edx;
       jnz @addforyloop2;

       // check if we need to use the unbiased version
       cmp unbiased, 0;
       jz @@dobiased;

       movapd xmm4, xmm6;
       subpd xmm4, xmm2;
       maxpd xmm4, xmm2;

       divpd xmm0, xmm4;

       jmp @@writeRes;

       @@dobiased:
       divpd xmm0, xmm6;

       // write result
       @@writeRes:
       movupd [eax], xmm0;

       // next columns:
       add ecx, 16;
       add eax, 16;

   // loop x end
   dec esi;
   jnz @@addforxloop;

   pop edi;
   pop esi;
   pop ebx;
end;

procedure ASMMatrixVarColumnAlignedOddW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; unbiased : boolean);
asm
   push ebx;
   push esi;
   push edi;

   mov edx, srcLineWidth;
   mov ebx, height;
   cvtsi2sd xmm5, ebx;
   imul ebx, edx;
   imul ebx, -1;

   // helper registers for the mt1, mt2 and dest pointers
   sub ecx, ebx;
   movupd xmm2, cLocOnes;

   movddup xmm6, xmm5;

   // for x := 0 to width - 1:
   mov esi, Width;
   sar esi, 1;
   jz @lastColumn;
   @@addforxloop:
       xorpd xmm7, xmm7;

       // for y := 0 to height - 1;
       // prepare for reverse loop
       mov edi, ebx;
       @addforyloop:
           addpd xmm7, [ecx + edi];
       add edi, edx;
       jnz @addforyloop;

       // build mean
       divpd xmm7, xmm6;

       // calculate final standard deviation
       // for y := 0 to height - 1;
       // prepare for reverse loop
       xorpd xmm0, xmm0;
       mov edi, ebx;
       @addforyloop2:
           movapd xmm1, [ecx + edi];
           subpd xmm1, xmm7;
           mulpd xmm1, xmm1;
           addpd xmm0, xmm1;
       add edi, edx;
       jnz @addforyloop2;

       // check if we need to use the unbiased version
       cmp unbiased, 0;
       jz @@dobiased;

       movapd xmm4, xmm6;
       subpd xmm4, xmm2;
       maxpd xmm4, xmm2;

       divpd xmm0, xmm4;

       jmp @@writeRes;

       @@dobiased:
       divpd xmm0, xmm6;

       // write result
       @@writeRes:
       movapd [eax], xmm0;

       // next columns:
       add ecx, 16;
       add eax, 16;

   // loop x end
   dec esi;
   jnz @@addforxloop;

   @lastColumn:
   // handle last column
   xorpd xmm7, xmm7;

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov edi, ebx;
   @addforyloop3:
       movsd xmm0, [ecx + edi];
       addsd xmm7, xmm0;
   add edi, edx;
   jnz @addforyloop3;

   // build mean
   divsd xmm7, xmm6;

   // calculate final standard deviation
   // for y := 0 to height - 1;
   // prepare for reverse loop
   xorpd xmm0, xmm0;
   mov edi, ebx;
   @addforyloop4:
       movsd xmm1, [ecx + edi];
       subsd xmm1, xmm7;
       mulsd xmm1, xmm1;
       addsd xmm0, xmm1;
   add edi, edx;
   jnz @addforyloop4;

   // check if we need to use the unbiased version
   cmp unbiased, 0;
   jz @@dobiased2;

   movapd xmm4, xmm6;
   subsd xmm4, cLocOne;
   maxsd xmm4, cLocOne;

   divsd xmm0, xmm4;

   jmp @@writeRes2;

   @@dobiased2:
   divsd xmm0, xmm6;

   // write result
   @@writeRes2:
   movsd [eax], xmm0;

   pop edi;
   pop esi;
   pop ebx;
end;

procedure ASMMatrixVarColumnUnAlignedOddW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; unbiased : boolean);
asm
   push ebx;
   push esi;
   push edi;

   mov edx, srcLineWidth;
   mov ebx, height;
   cvtsi2sd xmm5, ebx;
   imul ebx, edx;
   imul ebx, -1;

   // helper registers for the mt1, mt2 and dest pointers
   sub ecx, ebx;
   movupd xmm2, cLocOnes;

   movddup xmm6, xmm5;

   // for x := 0 to width - 1:
   mov esi, Width;
   sar esi, 1;
   jz @lastColumn;
   @@addforxloop:
       xorpd xmm7, xmm7;

       // for y := 0 to height - 1;
       // prepare for reverse loop
       mov edi, ebx;
       @addforyloop:
           movupd xmm0, [ecx + edi];
           addpd xmm7, xmm0;
       add edi, edx;
       jnz @addforyloop;

       // build mean
       divpd xmm7, xmm6;

       // calculate final standard deviation
       // for y := 0 to height - 1;
       // prepare for reverse loop
       xorpd xmm0, xmm0;
       mov edi, ebx;
       @addforyloop2:
           movupd xmm1, [ecx + edi];
           subpd xmm1, xmm7;
           mulpd xmm1, xmm1;
           addpd xmm0, xmm1;
       add edi, edx;
       jnz @addforyloop2;

       // check if we need to use the unbiased version
       cmp unbiased, 0;
       jz @@dobiased;

       movapd xmm4, xmm6;
       subpd xmm4, xmm2;
       maxpd xmm4, xmm2;

       divpd xmm0, xmm4;

       jmp @@writeRes;

       @@dobiased:
       divpd xmm0, xmm6;

       // write result
       @@writeRes:
       movupd [eax], xmm0;

       // next columns:
       add ecx, 16;
       add eax, 16;

   // loop x end
   dec esi;
   jnz @@addforxloop;

   @lastColumn:
   // handle last column
   xorpd xmm7, xmm7;

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov edi, ebx;
   @addforyloop3:
       movsd xmm0, [ecx + edi];
       addsd xmm7, xmm0;
   add edi, edx;
   jnz @addforyloop3;

   // build mean
   divsd xmm7, xmm6;

   // calculate final standard deviation
   // for y := 0 to height - 1;
   // prepare for reverse loop
   xorpd xmm0, xmm0;
   mov edi, ebx;
   @addforyloop4:
       movsd xmm1, [ecx + edi];
       subsd xmm1, xmm7;
       mulsd xmm1, xmm1;
       addsd xmm0, xmm1;
   add edi, edx;
   jnz @addforyloop4;

   // check if we need to use the unbiased version
   cmp unbiased, 0;
   jz @@dobiased2;

   movapd xmm4, xmm6;
   subsd xmm4, cLocOne;
   maxsd xmm4, cLocOne;

   divsd xmm0, xmm4;

   jmp @@writeRes2;

   @@dobiased2:
   divsd xmm0, xmm6;

   // write result
   @@writeRes2:
   movsd [eax], xmm0;

   pop edi;
   pop esi;
   pop ebx;
end;

// ###########################################
// #### Combined Mean and Variance calculation
// ###########################################

procedure ASMMatrixMeanVarRowAlignedEvenW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; unbiased : boolean);
// eax = dest, edx = destLineWidth, ecx = Src
asm
   push ebx;
   push esi;
   push edi;

   mov ebx, width;
   cvtsi2sd xmm5, ebx;
   imul ebx, -8;
        
   // helper registers for the mt1, mt2 and dest pointers
   sub ecx, ebx;

   // for y := 0 to height - 1:
   mov esi, Height;
   @@addforyloop:
       xorpd xmm0, xmm0;
       xorpd xmm1, xmm1;
       xorpd xmm2, xmm2;
       xorpd xmm3, xmm3;

       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, ebx;
       @addforxloop:
           add edi, 128;
           jg @loopEnd;
           // prefetch data...
           // prefetch [ecx + edi];

           // addition:
           addpd xmm0, [ecx + edi - 128];
           addpd xmm1, [ecx + edi - 112];
           addpd xmm2, [ecx + edi - 96];
           addpd xmm3, [ecx + edi - 80];
           addpd xmm0, [ecx + edi - 64];
           addpd xmm1, [ecx + edi - 48];
           addpd xmm2, [ecx + edi - 32];
           addpd xmm3, [ecx + edi - 16];
       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @buildRes;

       @addforxloop2:
           addpd xmm0, [ecx + edi];
       add edi, 16;
       jnz @addforxloop2;

       @buildRes:

       addpd xmm0, xmm1;
       addpd xmm2, xmm3;
       addpd xmm0, xmm2;

       // build result
       haddpd xmm0, xmm0;

       divsd xmm0, xmm5;
       movsd [eax], xmm0;

       // we have calculated the mean ->
       // repeat the loop to calculate the variance

       movddup xmm4, xmm0;
       xorpd xmm0, xmm0;

       // for x := 0 to w - 1;
       // prepare for reverse loop
       // variance = sum (x - mean)^2
       mov edi, ebx;
       @addforxloop3:
           add edi, 128;
           jg @loopEnd2;
           // prefetch data...
           // prefetch [ecx + edi];

           // addition:
           movapd xmm1, [ecx + edi - 128];
           movapd xmm2, [ecx + edi - 112];
           subpd xmm1, xmm4;
           subpd xmm2, xmm4;
           mulpd xmm1, xmm1;
           mulpd xmm2, xmm2;
           addpd xmm0, xmm1;
           addpd xmm0, xmm2;

           movapd xmm1, [ecx + edi - 96];
           movapd xmm2, [ecx + edi - 80];
           subpd xmm1, xmm4;
           subpd xmm2, xmm4;
           mulpd xmm1, xmm1;
           mulpd xmm2, xmm2;
           addpd xmm0, xmm1;
           addpd xmm0, xmm2;

           movapd xmm1, [ecx + edi - 64];
           movapd xmm2, [ecx + edi - 48];
           subpd xmm1, xmm4;
           subpd xmm2, xmm4;
           mulpd xmm1, xmm1;
           mulpd xmm2, xmm2;
           addpd xmm0, xmm1;
           addpd xmm0, xmm2;

           movapd xmm1, [ecx + edi - 32];
           movapd xmm2, [ecx + edi - 16];
           subpd xmm1, xmm4;
           subpd xmm2, xmm4;
           mulpd xmm1, xmm1;
           mulpd xmm2, xmm2;
           addpd xmm0, xmm1;
           addpd xmm0, xmm2;

       jmp @addforxloop3

       @loopEnd2:

       sub edi, 128;

       jz @buildRes2;

       @addforxloop4:
           movapd xmm1, [ecx + edi];
           subpd xmm1, xmm4;
           mulpd xmm1, xmm1;
           addpd xmm0, xmm1;
       add edi, 16;
       jnz @addforxloop4;

       @buildRes2:

       // build result
       movhlps xmm1, xmm0;
       addsd xmm0, xmm1;

       // check if we need to use the unbiased version
       cmp unbiased, 0;
       jz @@dobiased;

       movsd xmm4, xmm5;
       subsd xmm4, cLocOne;
       maxsd xmm4, cLocOne;

       divsd xmm0, xmm4;

       jmp @@writeRes;

       @@dobiased:
       divsd xmm0, xmm5;

       // write result
       @@writeRes:
       movsd [eax + 8], xmm0;

       // next line:
       add ecx, srcLineWidth;
       add eax, edx;

   // loop y end
   dec esi;
   jnz @@addforyloop;

   pop edi;
   pop esi;
   pop ebx;
end;

procedure ASMMatrixMeanVarRowUnAlignedEvenW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; unbiased : boolean);
// eax = dest, edx = destLineWidth, ecx = Src
asm
   push ebx;
   push esi;
   push edi;

   mov ebx, width;
   cvtsi2sd xmm5, ebx;
   imul ebx, -8;
        
   // helper registers for the mt1, mt2 and dest pointers
   sub ecx, ebx;
   
   // for y := 0 to height - 1:
   mov esi, Height;
   @@addforyloop:
       xorpd xmm7, xmm7;

       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, ebx;
       @addforxloop:
           add edi, 128;
           jg @loopEnd;

           // addition:
           movupd xmm0, [ecx + edi - 128];
           addpd xmm7, xmm0;

           movupd xmm1, [ecx + edi - 112];
           addpd xmm7, xmm1;

           movupd xmm2, [ecx + edi - 96];
           addpd xmm7, xmm2;

           movupd xmm3, [ecx + edi - 80];
           addpd xmm7, xmm3;

           movupd xmm0, [ecx + edi - 64];
           addpd xmm7, xmm0;

           movupd xmm1, [ecx + edi - 48];
           addpd xmm7, xmm1;

           movupd xmm2, [ecx + edi - 32];
           addpd xmm7, xmm2;

           movupd xmm3, [ecx + edi - 16];
           addpd xmm7, xmm3;
       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @buildRes;

       @addforxloop2:
           movupd xmm0, [ecx + edi];
           addpd xmm7, xmm0;
       add edi, 16;
       jnz @addforxloop2;

       @buildRes:

       // build result
       haddpd xmm7, xmm7;
       divsd xmm7, xmm5;

       movsd [eax], xmm7;

       // we have calculated the mean ->
       // repeat the loop to calculate the variance

       movddup xmm4, xmm7;
       xorpd xmm0, xmm0;

       // for x := 0 to w - 1;
       // prepare for reverse loop
       // variance = sum (x - mean)^2
       mov edi, ebx;
       @addforxloop3:
           add edi, 128;
           jg @loopEnd2;
           // prefetch data...
           // prefetch [ecx + edi];

           // addition:
           movupd xmm1, [ecx + edi - 128];
           movupd xmm2, [ecx + edi - 112];
           subpd xmm1, xmm4;
           subpd xmm2, xmm4;
           mulpd xmm1, xmm1;
           mulpd xmm2, xmm2;
           addpd xmm0, xmm1;
           addpd xmm0, xmm2;

           movupd xmm1, [ecx + edi - 96];
           movupd xmm2, [ecx + edi - 80];
           subpd xmm1, xmm4;
           subpd xmm2, xmm4;
           mulpd xmm1, xmm1;
           mulpd xmm2, xmm2;
           addpd xmm0, xmm1;
           addpd xmm0, xmm2;

           movupd xmm1, [ecx + edi - 64];
           movupd xmm2, [ecx + edi - 48];
           subpd xmm1, xmm4;
           subpd xmm2, xmm4;
           mulpd xmm1, xmm1;
           mulpd xmm2, xmm2;
           addpd xmm0, xmm1;
           addpd xmm0, xmm2;

           movupd xmm1, [ecx + edi - 32];
           movupd xmm2, [ecx + edi - 16];
           subpd xmm1, xmm4;
           subpd xmm2, xmm4;
           mulpd xmm1, xmm1;
           mulpd xmm2, xmm2;
           addpd xmm0, xmm1;
           addpd xmm0, xmm2;

       jmp @addforxloop3

       @loopEnd2:

       sub edi, 128;

       jz @buildRes2;

       @addforxloop4:
           movupd xmm1, [ecx + edi];
           subpd xmm1, xmm4;
           mulpd xmm1, xmm1;
           addpd xmm0, xmm1;
       add edi, 16;
       jnz @addforxloop4;

       @buildRes2:

       // build result
       haddpd xmm0, xmm0;

       // check if we need to use the unbiased version
       cmp unbiased, 0;
       jz @@dobiased;

       movsd xmm4, xmm5;
       subsd xmm4, cLocOne;
       maxsd xmm4, cLocOne;

       divsd xmm0, xmm4;

       jmp @@writeRes;

       @@dobiased:
       divsd xmm0, xmm5;

       // write result
       @@writeRes:
       movsd [eax + 8], xmm0;

       // next line:
       add ecx, srcLineWidth;
       add eax, edx;

   // loop y end
   dec esi;
   jnz @@addforyloop;

   pop edi;
   pop esi;
   pop ebx;
end;

procedure ASMMatrixMeanVarRowAlignedOddW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; unbiased : boolean);
// eax = dest, edx = destLineWidth, ecx = Src
asm
   push ebx;
   push esi;
   push edi;

   mov ebx, width;
   cvtsi2sd xmm5, ebx;
   dec ebx;
   imul ebx, -8;
        
   // helper registers for the mt1, mt2 and dest pointers
   sub ecx, ebx;

   // for y := 0 to height - 1:
   mov esi, Height;
   @@addforyloop:
       xorpd xmm0, xmm0;
       xorpd xmm1, xmm1;
       xorpd xmm2, xmm2;
       xorpd xmm3, xmm3;

       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, ebx;
       @addforxloop:
           add edi, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [ecx + edi];

           // addition:
           addpd xmm0, [ecx + edi - 128];
           addpd xmm1, [ecx + edi - 112];
           addpd xmm2, [ecx + edi - 96];
           addpd xmm3, [ecx + edi - 80];
           addpd xmm0, [ecx + edi - 64];
           addpd xmm1, [ecx + edi - 48];
           addpd xmm2, [ecx + edi - 32];
           addpd xmm3, [ecx + edi - 16];
       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @buildRes;

       @addforxloop2:
           addpd xmm0, [ecx + edi];
       add edi, 16;
       jnz @addforxloop2;

       @buildRes:

       addpd xmm0, xmm1;
       addpd xmm2, xmm3;
       addpd xmm0, xmm2;

       // handle last element differently
       movsd xmm2, [ecx + edi];
       addsd xmm0, xmm2;

       // build result
       haddpd xmm0, xmm0;

       divsd xmm0, xmm5;

       movsd [eax], xmm0;

       // we have calculated the mean ->
       // repeat the loop to calculate the variance

       movddup xmm4, xmm0;
       xorpd xmm0, xmm0;

       // for x := 0 to w - 1;
       // prepare for reverse loop
       // variance = sum (x - mean)^2
       mov edi, ebx;
       @addforxloop3:
           add edi, 128;
           jg @loopEnd2;
           // prefetch data...
           // prefetch [ecx + edi];

           // addition:
           movapd xmm1, [ecx + edi - 128];
           movapd xmm2, [ecx + edi - 112];
           subpd xmm1, xmm4;
           subpd xmm2, xmm4;
           mulpd xmm1, xmm1;
           mulpd xmm2, xmm2;
           addpd xmm0, xmm1;
           addpd xmm0, xmm2;

           movapd xmm1, [ecx + edi - 96];
           movapd xmm2, [ecx + edi - 80];
           subpd xmm1, xmm4;
           subpd xmm2, xmm4;
           mulpd xmm1, xmm1;
           mulpd xmm2, xmm2;
           addpd xmm0, xmm1;
           addpd xmm0, xmm2;

           movapd xmm1, [ecx + edi - 64];
           movapd xmm2, [ecx + edi - 48];
           subpd xmm1, xmm4;
           subpd xmm2, xmm4;
           mulpd xmm1, xmm1;
           mulpd xmm2, xmm2;
           addpd xmm0, xmm1;
           addpd xmm0, xmm2;

           movapd xmm1, [ecx + edi - 32];
           movapd xmm2, [ecx + edi - 16];
           subpd xmm1, xmm4;
           subpd xmm2, xmm4;
           mulpd xmm1, xmm1;
           mulpd xmm2, xmm2;
           addpd xmm0, xmm1;
           addpd xmm0, xmm2;

       jmp @addforxloop3

       @loopEnd2:

       sub edi, 128;

       jz @buildRes2;

       @addforxloop4:
           movapd xmm1, [ecx + edi];
           subpd xmm1, xmm4;
           mulpd xmm1, xmm1;
           addpd xmm0, xmm1;
       add edi, 16;
       jnz @addforxloop4;

       @buildRes2:

       // handle last element differently
       movsd xmm1, [ecx + edi];
       subsd xmm1, xmm4;
       mulsd xmm1, xmm1;
       addsd xmm0, xmm1;

       // build result
       haddpd xmm0, xmm0;

       // check if we need to use the unbiased version
       cmp unbiased, 0;
       jz @@dobiased;

       movsd xmm4, xmm5;
       subsd xmm4, cLocOne;
       maxsd xmm4, cLocOne;

       divsd xmm0, xmm4;

       jmp @@writeRes;

       @@dobiased:
       divsd xmm0, xmm5;

       // write result
       @@writeRes:
       movsd [eax + 8], xmm0;

       // next line:
       add ecx, srcLineWidth;
       add eax, edx;

   // loop y end
   dec esi;
   jnz @@addforyloop;

   pop edi;
   pop esi;
   pop ebx;
end;

procedure ASMMatrixMeanVarRowUnAlignedOddW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; unbiased : boolean);
// eax = dest, edx = destLineWidth, ecx = Src
asm
   push ebx;
   push esi;
   push edi;

   mov ebx, width;
   cvtsi2sd xmm5, ebx;
   dec ebx;
   imul ebx, -8;
        
   // helper registers for the mt1, mt2 and dest pointers
   sub ecx, ebx;

   // for y := 0 to height - 1:
   mov esi, Height;
   @@addforyloop:
       xorpd xmm7, xmm7;

       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, ebx;
       @addforxloop:
           add edi, 128;
           jg @loopEnd;

           // addition:
           movupd xmm0, [ecx + edi - 128];
           addpd xmm7, xmm0;

           movupd xmm1, [ecx + edi - 112];
           addpd xmm7, xmm1;

           movupd xmm2, [ecx + edi - 96];
           addpd xmm7, xmm2;

           movupd xmm3, [ecx + edi - 80];
           addpd xmm7, xmm3;

           movupd xmm0, [ecx + edi - 64];
           addpd xmm7, xmm0;

           movupd xmm1, [ecx + edi - 48];
           addpd xmm7, xmm1;

           movupd xmm2, [ecx + edi - 32];
           addpd xmm7, xmm2;

           movupd xmm3, [ecx + edi - 16];
           addpd xmm7, xmm3;
       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @buildRes;

       @addforxloop2:
           movupd xmm0, [ecx + edi];
           addpd xmm7, xmm0;
       add edi, 16;
       jnz @addforxloop2;

       @buildRes:

       // handle last element differently
       movsd xmm2, [ecx + edi];
       addsd xmm7, xmm2;

       // build result
       haddpd xmm7, xmm7;

       divsd xmm7, xmm5;
       movsd [eax], xmm7;

       // we have calculated the mean ->
       // repeat the loop to calculate the variance

       movddup xmm4, xmm7;
       xorpd xmm0, xmm0;

       // for x := 0 to w - 1;
       // prepare for reverse loop
       // variance = sum (x - mean)^2
       mov edi, ebx;
       @addforxloop3:
           add edi, 128;
           jg @loopEnd2;
           // prefetch data...
           // prefetch [ecx + edi];

           // addition:
           movupd xmm1, [ecx + edi - 128];
           movupd xmm2, [ecx + edi - 112];
           subpd xmm1, xmm4;
           subpd xmm2, xmm4;
           mulpd xmm1, xmm1;
           mulpd xmm2, xmm2;
           addpd xmm0, xmm1;
           addpd xmm0, xmm2;

           movupd xmm1, [ecx + edi - 96];
           movupd xmm2, [ecx + edi - 80];
           subpd xmm1, xmm4;
           subpd xmm2, xmm4;
           mulpd xmm1, xmm1;
           mulpd xmm2, xmm2;
           addpd xmm0, xmm1;
           addpd xmm0, xmm2;

           movupd xmm1, [ecx + edi - 64];
           movupd xmm2, [ecx + edi - 48];
           subpd xmm1, xmm4;
           subpd xmm2, xmm4;
           mulpd xmm1, xmm1;
           mulpd xmm2, xmm2;
           addpd xmm0, xmm1;
           addpd xmm0, xmm2;

           movupd xmm1, [ecx + edi - 32];
           movupd xmm2, [ecx + edi - 16];
           subpd xmm1, xmm4;
           subpd xmm2, xmm4;
           mulpd xmm1, xmm1;
           mulpd xmm2, xmm2;
           addpd xmm0, xmm1;
           addpd xmm0, xmm2;

       jmp @addforxloop3

       @loopEnd2:

       sub edi, 128;

       jz @buildRes2;

       @addforxloop4:
           movupd xmm1, [ecx + edi];
           subpd xmm1, xmm4;
           mulpd xmm1, xmm1;
           addpd xmm0, xmm1;
       add edi, 16;
       jnz @addforxloop4;

       @buildRes2:

       // handle last element differently
       movsd xmm1, [ecx + edi];
       subsd xmm1, xmm4;
       mulsd xmm1, xmm1;
       addsd xmm0, xmm1;

       // build result
       movhlps xmm1, xmm0;
       addsd xmm0, xmm1;

       // check if we need to use the unbiased version
       cmp unbiased, 0;
       jz @@dobiased;

       movsd xmm4, xmm5;
       subsd xmm4, cLocOne;
       maxsd xmm4, cLocOne;

       divsd xmm0, xmm4;

       jmp @@writeRes;

       @@dobiased:
       divsd xmm0, xmm5;

       // write result
       @@writeRes:
       movsd [eax + 8], xmm0;

       // next line:
       add ecx, srcLineWidth;
       add eax, edx;

   // loop y end
   dec esi;
   jnz @@addforyloop;

   pop edi;
   pop esi;
   pop ebx;
end;

procedure ASMMatrixMeanVarColumnAlignedEvenW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; unbiased : boolean);
// eax = dest, edx = destLineWidth, ecx = Src
asm
   push ebx;
   push esi;
   push edi;

   mov esi, srcLineWidth;
   mov ebx, height;
   cvtsi2sd xmm5, ebx;
   imul ebx, esi;
   imul ebx, -1;

   movupd xmm2, cLocOnes;
   movddup xmm6, xmm5;

   sub ecx, ebx;

   // for x := 0 to width - 1:
   @@addforxloop:
       xorpd xmm7, xmm7;

       // calculate mean

       // for y := 0 to height - 1;
       // prepare for reverse loop
       mov edi, ebx;
       @addforyloop:
           addpd xmm7, [ecx + edi];
       add edi, esi;
       jnz @addforyloop;

       // build mean
       divpd xmm7, xmm6;
       movapd [eax], xmm7;

       // calculate final standard deviation
       // for y := 0 to height - 1;
       // prepare for reverse loop
       xorpd xmm0, xmm0;
       mov edi, ebx;
       @addforyloop2:
           movapd xmm1, [ecx + edi];
           subpd xmm1, xmm7;
           mulpd xmm1, xmm1;
           addpd xmm0, xmm1;
       add edi, esi;
       jnz @addforyloop2;

       // check if we need to use the unbiased version
       cmp unbiased, 0;
       jz @@dobiased;

       movapd xmm4, xmm6;
       subpd xmm4, xmm2;
       maxpd xmm4, xmm2;

       divpd xmm0, xmm4;

       jmp @@writeRes;

       @@dobiased:
       divpd xmm0, xmm6;

       // write result
       @@writeRes:
       movapd [eax + edx], xmm0;

       // next columns:
       add ecx, 16;
       add eax, 16;

   // loop x end
   sub Width, 2;
   jnz @@addforxloop;

   pop edi;
   pop esi;
   pop ebx;
end;

procedure ASMMatrixMeanVarColumnUnAlignedEvenW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; unbiased : boolean);
// eax = dest, edx = destLineWidth, ecx = Src
asm
   push ebx;
   push esi;
   push edi;

   mov esi, srcLineWidth;
   mov ebx, height;
   cvtsi2sd xmm5, ebx;
   imul ebx, esi;
   imul ebx, -1;

   movupd xmm2, cLocOnes;
   movddup xmm6, xmm5;

   sub ecx, ebx;

   // for x := 0 to width - 1:
   @@addforxloop:
       xorpd xmm7, xmm7;

       // for y := 0 to height - 1;
       // prepare for reverse loop
       mov edi, ebx;
       @addforyloop:
           movupd xmm0, [ecx + edi];
           addpd xmm7, xmm0;
       add edi, esi;
       jnz @addforyloop;

       // build mean
       divpd xmm7, xmm6;
       movupd [eax], xmm7;

       // calculate final standard deviation
       // for y := 0 to height - 1;
       // prepare for reverse loop
       xorpd xmm0, xmm0;
       mov edi, ebx;
       @addforyloop2:
           movupd xmm1, [ecx + edi];
           subpd xmm1, xmm7;
           mulpd xmm1, xmm1;
           addpd xmm0, xmm1;
       add edi, esi;
       jnz @addforyloop2;

       // check if we need to use the unbiased version
       cmp unbiased, 0;
       jz @@dobiased;

       movapd xmm4, xmm6;
       subpd xmm4, xmm2;
       maxpd xmm4, xmm2;

       divpd xmm0, xmm4;

       jmp @@writeRes;

       @@dobiased:
       divpd xmm0, xmm6;

       // write result
       @@writeRes:
       movupd [eax + edx], xmm0;

       // next columns:
       add ecx, 16;
       add eax, 16;

   // loop x end
   sub width, 2;
   jnz @@addforxloop;

   pop edi;
   pop esi;
   pop ebx;
end;

procedure ASMMatrixMeanVarColumnAlignedOddW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; unbiased : boolean);
asm
   push ebx;
   push esi;
   push edi;

   mov ebx, height;
   mov esi, srcLineWidth;
   cvtsi2sd xmm5, ebx;
   imul ebx, esi;
   imul ebx, -1;
   
   // helper registers for the mt1, mt2 and dest pointers
   sub ecx, ebx;

   movupd xmm2, cLocOnes;
   movddup xmm6, xmm5;

   // for x := 0 to width - 1:
   sub width, 2;
   jl @lastColumn;
   @@addforxloop:
       xorpd xmm7, xmm7;

       // for y := 0 to height - 1;
       // prepare for reverse loop
       mov edi, ebx;
       @addforyloop:
           addpd xmm7, [ecx + edi];
       add edi, esi;
       jnz @addforyloop;

       // build mean
       divpd xmm7, xmm6;
       movapd [eax], xmm7;

       // calculate final standard deviation
       // for y := 0 to height - 1;
       // prepare for reverse loop
       xorpd xmm0, xmm0;
       mov edi, ebx;
       @addforyloop2:
           movapd xmm1, [ecx + edi];
           subpd xmm1, xmm7;
           mulpd xmm1, xmm1;
           addpd xmm0, xmm1;
       add edi, esi;
       jnz @addforyloop2;

       // check if we need to use the unbiased version
       cmp unbiased, 0;
       jz @@dobiased;

       movapd xmm4, xmm6;
       subpd xmm4, xmm2;
       maxpd xmm4, xmm2;

       divpd xmm0, xmm4;

       jmp @@writeRes;

       @@dobiased:
       divpd xmm0, xmm6;

       // write result
       @@writeRes:
       movapd [eax + edx], xmm0;

       // next columns:
       add ecx, 16;
       add eax, 16;

   // loop x end
   sub width, 2;
   jge @@addforxloop;

   @lastColumn:
   // handle last column
   xorpd xmm7, xmm7;

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov edi, ebx;
   @addforyloop3:
       movsd xmm0, [ecx + edi];
       addsd xmm7, xmm0;
   add edi, esi;
   jnz @addforyloop3;

   // build mean
   divsd xmm7, xmm6;
   movsd [eax], xmm7;

   // calculate final standard deviation
   // for y := 0 to height - 1;
   // prepare for reverse loop
   xorpd xmm0, xmm0;
   mov edi, ebx;
   @addforyloop4:
       movsd xmm1, [ecx + edi];
       subsd xmm1, xmm7;
       mulsd xmm1, xmm1;
       addsd xmm0, xmm1;
   add edi, esi;
   jnz @addforyloop4;

   // check if we need to use the unbiased version
   cmp unbiased, 0;
   jz @@dobiased2;

   movapd xmm4, xmm6;
   subsd xmm4, xmm2;
   maxsd xmm4, xmm2;

   divsd xmm0, xmm4;

   jmp @@writeRes2;

   @@dobiased2:
   divsd xmm0, xmm6;

   // write result
   @@writeRes2:
   movsd [eax + edx], xmm0;

   pop edi;
   pop esi;
   pop ebx;
end;

procedure ASMMatrixMeanVarColumnUnAlignedOddW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; unbiased : boolean);
asm
   push ebx;
   push esi;
   push edi;

   mov ebx, height;
   mov esi, srcLineWidth;
   cvtsi2sd xmm5, ebx;
   imul ebx, esi;
   imul ebx, -1;

   // helper registers for the mt1, mt2 and dest pointers
   sub ecx, ebx;

   movupd xmm2, cLocOnes;
   movddup xmm6, xmm5;

   // for x := 0 to width - 1:
   sub width, 2;
   jl @lastColumn;
   @@addforxloop:
       xorpd xmm7, xmm7;

       // for y := 0 to height - 1;
       // prepare for reverse loop
       mov edi, ebx;
       @addforyloop:
           movupd xmm0, [ecx + edi];
           addpd xmm7, xmm0;
       add edi, esi;
       jnz @addforyloop;

       // build mean
       divpd xmm7, xmm6;
       movupd [eax], xmm7;

       // calculate final standard deviation
       // for y := 0 to height - 1;
       // prepare for reverse loop
       xorpd xmm0, xmm0;
       mov edi, ebx;
       @addforyloop2:
           movupd xmm1, [ecx + edi];
           subpd xmm1, xmm7;
           mulpd xmm1, xmm1;
           addpd xmm0, xmm1;
       add edi, esi;
       jnz @addforyloop2;

       // check if we need to use the unbiased version
       cmp unbiased, 0;
       jz @@dobiased;

       movapd xmm4, xmm6;
       subpd xmm4, xmm2;
       maxpd xmm4, xmm2;

       divpd xmm0, xmm4;

       jmp @@writeRes;

       @@dobiased:
       divpd xmm0, xmm6;

       // write result
       @@writeRes:
       movupd [eax + edx], xmm0;

       // next columns:
       add ecx, 16;
       add eax, 16;

   // loop x end
   sub width, 2;
   jge @@addforxloop;

   @lastColumn:
   // handle last column
   xorpd xmm7, xmm7;

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov edi, ebx;
   @addforyloop3:
       movsd xmm0, [ecx + edi];
       addsd xmm7, xmm0;
   add edi, esi;
   jnz @addforyloop3;

   // build mean
   divsd xmm7, xmm6;
   movsd [eax], xmm7;

   // calculate final standard deviation
   // for y := 0 to height - 1;
   // prepare for reverse loop
   xorpd xmm0, xmm0;
   mov edi, ebx;
   @addforyloop4:
       movsd xmm1, [ecx + edi];
       subsd xmm1, xmm7;
       mulsd xmm1, xmm1;
       addsd xmm0, xmm1;
   add edi, esi;
   jnz @addforyloop4;

   // check if we need to use the unbiased version
   cmp unbiased, 0;
   jz @@dobiased2;

   movapd xmm4, xmm6;
   subsd xmm4, xmm2;
   maxsd xmm4, xmm2;

   divsd xmm0, xmm4;

   jmp @@writeRes2;

   @@dobiased2:
   divsd xmm0, xmm6;

   // write result
   @@writeRes2:
   movsd [eax + edx], xmm0;

   pop edi;
   pop esi;
   pop ebx;
end;


{$ENDIF}

end.
