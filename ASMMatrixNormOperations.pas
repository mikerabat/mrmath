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


unit ASMMatrixNormOperations;

interface

{$I 'mrMath_CPU.inc'}

{$IFNDEF x64}

function ASMMatrixElementwiseNorm2AlignedEvenW(dest : PDouble; const LineWidth : NativeInt; Width, height : NativeInt) : double; {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
function ASMMatrixElementwiseNorm2UnAlignedEvenW(dest : PDouble; const LineWidth : NativeInt; Width, height : NativeInt) : double; {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

function ASMMatrixElementwiseNorm2AlignedOddW(dest : PDouble; const LineWidth : NativeInt; Width, height : NativeInt) : double; {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
function ASMMatrixElementwiseNorm2UnAlignedOddW(dest : PDouble; const LineWidth : NativeInt; Width, height : NativeInt) : double; {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}


procedure ASMMatrixNormalizeRowAlignedEvenW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixNormalizeRowUnAlignedEvenW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixNormalizeRowAlignedOddW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixNormalizeRowUnAlignedOddW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

procedure ASMMatrixNormalizeColumnAlignedEvenW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixNormalizeColumnUnAlignedEvenW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF} 
procedure ASMMatrixNormalizeColumnAlignedOddW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixNormalizeColumnUnAlignedOddW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

{$ENDIF}

implementation

{$IFNDEF x64}

const cLocOne : double = 1;

function ASMMatrixElementwiseNorm2AlignedEvenW(dest : PDouble; const LineWidth : NativeInt; Width, height : NativeInt) : double;
asm
   push ebx;
   push edi;
   push esi;

   // iters
   mov ebx, width;
   imul ebx, -8;
        
   // helper registers for the mt1, mt2 and dest pointers
   sub eax, ebx;

   xorpd xmm7, xmm7;

   // for y := 0 to height - 1:
   mov esi, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, ebx;
       @addforxloop:
           add edi, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [eax + edi];

           // mul add:
           movapd xmm0, [eax + edi - 128];
           mulpd xmm0, xmm0;
           addpd xmm7, xmm0;

           movapd xmm1, [eax + edi - 112];
           mulpd xmm1, xmm1;
           addpd xmm7, xmm1;

           movapd xmm2, [eax + edi - 96];
           mulpd xmm2, xmm2;
           addpd xmm7, xmm2;

           movapd xmm3, [eax + edi - 80];
           mulpd xmm3, xmm3;
           addpd xmm7, xmm3;

           movapd xmm0, [eax + edi - 64];
           mulpd xmm0, xmm0;
           addpd xmm7, xmm0;

           movapd xmm1, [eax + edi - 48];
           mulpd xmm1, xmm1;
           addpd xmm7, xmm1;

           movapd xmm2, [eax + edi - 32];
           mulpd xmm2, xmm2;
           addpd xmm7, xmm2;

           movapd xmm3, [eax + edi - 16];
           mulpd xmm3, xmm3;
           addpd xmm7, xmm3;
       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @nextLine;

       // prefetchw [eax + edi + 128];

       @addforxloop2:
           movapd xmm0, [eax + edi];
           mulpd xmm0, xmm0;
           addpd xmm7, xmm0;
       add edi, 16;
       jnz @addforxloop2;

       @nextLine:

       // next line:
       add eax, edx;

   // loop y end
   dec esi;
   jnz @@addforyloop;

   // build result
   movhlps xmm1, xmm7;
   addsd xmm7, xmm1;
   movsd Result, xmm7;

   pop esi;
   pop edi;
   pop ebx;
end;

function ASMMatrixElementwiseNorm2UnAlignedEvenW(dest : PDouble; const LineWidth : NativeInt; Width, height : NativeInt) : double;
asm
   push ebx;
   push edi;
   push esi;

   // iters
   mov ebx, width;
   imul ebx, -8;
        
   // helper registers for the mt1, mt2 and dest pointers
   sub eax, ebx;

   xorpd xmm7, xmm7;

   // for y := 0 to height - 1:
   mov esi, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, ebx;
       @addforxloop:
           add edi, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [eax + edi];

           // mul add:
           movupd xmm0, [eax + edi - 128];
           mulpd xmm0, xmm0;
           addpd xmm7, xmm0;

           movupd xmm1, [eax + edi - 112];
           mulpd xmm1, xmm1;
           addpd xmm7, xmm1;

           movupd xmm2, [eax + edi - 96];
           mulpd xmm2, xmm2;
           addpd xmm7, xmm2;

           movupd xmm3, [eax + edi - 80];
           mulpd xmm3, xmm3;
           addpd xmm7, xmm3;

           movupd xmm0, [eax + edi - 64];
           mulpd xmm0, xmm0;
           addpd xmm7, xmm0;

           movupd xmm1, [eax + edi - 48];
           mulpd xmm1, xmm1;
           addpd xmm7, xmm1;

           movupd xmm2, [eax + edi - 32];
           mulpd xmm2, xmm2;
           addpd xmm7, xmm2;

           movupd xmm3, [eax + edi - 16];
           mulpd xmm3, xmm3;
           addpd xmm7, xmm3;
       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @nextLine;

       // prefetchw [eax + edi + 128];

       @addforxloop2:
           movupd xmm0, [eax + edi];
           mulpd xmm0, xmm0;
           addpd xmm7, xmm0;
       add edi, 16;
       jnz @addforxloop2;

       @nextLine:

       // next line:
       add eax, edx;

   // loop y end
   dec esi;
   jnz @@addforyloop;

   // build result
   movhlps xmm1, xmm7;
   addsd xmm7, xmm1;
   movsd Result, xmm7;

   pop esi;
   pop edi;
   pop ebx;
end;

function ASMMatrixElementwiseNorm2AlignedOddW(dest : PDouble; const LineWidth : NativeInt; Width, height : NativeInt) : double;
asm
   push ebx;
   push edi;
   push esi;

   // iters
   mov ebx, width;
   dec ebx;
   imul ebx, -8;
        
   // helper registers for the mt1, mt2 and dest pointers
   sub eax, ebx;

   xorpd xmm7, xmm7;

   // for y := 0 to height - 1:
   mov esi, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, ebx;
       @addforxloop:
           add edi, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [eax + edi];

           // mul add:
           movapd xmm0, [eax + edi - 128];
           mulpd xmm0, xmm0;
           addpd xmm7, xmm0;

           movapd xmm1, [eax + edi - 112];
           mulpd xmm1, xmm1;
           addpd xmm7, xmm1;

           movapd xmm2, [eax + edi - 96];
           mulpd xmm2, xmm2;
           addpd xmm7, xmm2;

           movapd xmm3, [eax + edi - 80];
           mulpd xmm3, xmm3;
           addpd xmm7, xmm3;

           movapd xmm0, [eax + edi - 64];
           mulpd xmm0, xmm0;
           addpd xmm7, xmm0;

           movapd xmm1, [eax + edi - 48];
           mulpd xmm1, xmm1;
           addpd xmm7, xmm1;

           movapd xmm2, [eax + edi - 32];
           mulpd xmm2, xmm2;
           addpd xmm7, xmm2;

           movapd xmm3, [eax + edi - 16];
           mulpd xmm3, xmm3;
           addpd xmm7, xmm3;
       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @nextLine;

       // prefetchw [eax + edi + 128];

       @addforxloop2:
           movapd xmm0, [eax + edi];
           mulpd xmm0, xmm0;
           addpd xmm7, xmm0;
       add edi, 16;
       jnz @addforxloop2;

       @nextLine:

       // handle last element differently
       movsd xmm0, [eax];
       mulsd xmm0, xmm0;
       addsd xmm7, xmm0;

       // next line:
       add eax, edx;

   // loop y end
   dec esi;
   jnz @@addforyloop;

   // build result
   movhlps xmm1, xmm7;
   addsd xmm7, xmm1;
   movsd Result, xmm7;

   pop esi;
   pop edi;
   pop ebx;
end;

function ASMMatrixElementwiseNorm2UnAlignedOddW(dest : PDouble; const LineWidth : NativeInt; Width, height : NativeInt) : double;
asm
   push ebx;
   push edi;
   push esi;

   // iters
   mov ebx, width;
   dec ebx;
   imul ebx, -8;
        
   // helper registers for the mt1, mt2 and dest pointers
   sub eax, ebx;

   xorpd xmm7, xmm7;

   // for y := 0 to height - 1:
   mov esi, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, ebx;
       @addforxloop:
           add edi, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [eax + edi];

           // mul add:
           movupd xmm0, [eax + edi - 128];
           mulpd xmm0, xmm0;
           addpd xmm7, xmm0;

           movupd xmm1, [eax + edi - 112];
           mulpd xmm1, xmm1;
           addpd xmm7, xmm1;

           movupd xmm2, [eax + edi - 96];
           mulpd xmm2, xmm2;
           addpd xmm7, xmm2;

           movupd xmm3, [eax + edi - 80];
           mulpd xmm3, xmm3;
           addpd xmm7, xmm3;

           movupd xmm0, [eax + edi - 64];
           mulpd xmm0, xmm0;
           addpd xmm7, xmm0;

           movupd xmm1, [eax + edi - 48];
           mulpd xmm1, xmm1;
           addpd xmm7, xmm1;

           movupd xmm2, [eax + edi - 32];
           mulpd xmm2, xmm2;
           addpd xmm7, xmm2;

           movupd xmm3, [eax + edi - 16];
           mulpd xmm3, xmm3;
           addpd xmm7, xmm3;
       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @nextLine;

       // prefetchw [eax + edi + 128];

       @addforxloop2:
           movupd xmm0, [eax + edi];
           mulpd xmm0, xmm0;
           addpd xmm7, xmm0;
       add edi, 16;
       jnz @addforxloop2;

       @nextLine:

       // handle last element differently
       movsd xmm0, [eax];
       mulsd xmm0, xmm0;
       addsd xmm7, xmm0;

       // next line:
       add eax, edx;

   // loop y end
   dec esi;
   jnz @@addforyloop;

   // build result
   movhlps xmm1, xmm7;
   addsd xmm7, xmm1;
   movsd Result, xmm7;

   pop esi;
   pop edi;
   pop ebx;
end;

procedure ASMMatrixNormalizeRowAlignedEvenW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt);
asm
   push edi;
   push esi;

   // iters := -width*sizeof(double);
   mov esi, width;
   imul esi, -8;
        
   // helper registers for the mt1, mt2 and dest pointers
   sub ecx, esi;
   sub eax, esi;

   // for y := 0 to height - 1:
   @@addforyloop:
       xorpd xmm7, xmm7;

       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, esi;
       @addforxloop:
           add edi, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [ecx + edi];

           // mul add:
           movapd xmm0, [ecx + edi - 128];
           movapd xmm1, [ecx + edi - 112];
           movapd xmm2, [ecx + edi - 96];
           movapd xmm3, [ecx + edi - 80];
           
           mulpd xmm0, xmm0;
           addpd xmm7, xmm0;
           
           mulpd xmm1, xmm1;
           addpd xmm7, xmm1;
           
           mulpd xmm2, xmm2;
           addpd xmm7, xmm2;

           mulpd xmm3, xmm3;
           addpd xmm7, xmm3;

           movapd xmm0, [ecx + edi - 64];
           movapd xmm1, [ecx + edi - 48];
           movapd xmm2, [ecx + edi - 32];
           movapd xmm3, [ecx + edi - 16];
           
           mulpd xmm0, xmm0;
           addpd xmm7, xmm0;

           mulpd xmm1, xmm1;
           addpd xmm7, xmm1;

           mulpd xmm2, xmm2;
           addpd xmm7, xmm2;

           mulpd xmm3, xmm3;
           addpd xmm7, xmm3;
       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @buildRes;

       @addforxloop2:
           movapd xmm0, [ecx + edi];
           mulpd xmm0, xmm0;
           addpd xmm7, xmm0;
       add edi, 16;
       jnz @addforxloop2;

       @buildRes:

       // build result
       movhlps xmm1, xmm7;
       addsd xmm7, xmm1;
       sqrtsd xmm7, xmm7;
       movsd xmm0, cLocOne;
       divsd xmm0, xmm7;

       movddup xmm6, xmm0;

       // multiply the result and build the result
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, esi;
       @addforxloop3:
           add edi, 128;
           jg @loopEnd2;

           // prefetch data...
           // prefetch [ecx + edi];
           // prefetchw [eax + edi];

           // mul add:
           movapd xmm0, [ecx + edi - 128];
           mulpd xmm0, xmm6;
           movapd [eax + edi - 128], xmm0;

           movapd xmm1, [ecx + edi - 112];
           mulpd xmm1, xmm6;
           movapd [eax + edi - 112], xmm1;

           movapd xmm2, [ecx + edi - 96];
           mulpd xmm2, xmm6;
           movapd [eax + edi - 96], xmm2;

           movapd xmm3, [ecx + edi - 80];
           mulpd xmm3, xmm6;
           movapd [eax + edi - 80], xmm3;

           movapd xmm0, [ecx + edi - 64];
           mulpd xmm0, xmm6;
           movapd [eax + edi - 64], xmm0;

           movapd xmm1, [ecx + edi - 48];
           mulpd xmm1, xmm6;
           movapd [eax + edi - 48], xmm1;

           movapd xmm2, [ecx + edi - 32];
           mulpd xmm2, xmm6;
           movapd [eax + edi - 32], xmm2;

           movapd xmm3, [ecx + edi - 16];
           mulpd xmm3, xmm6;
           movapd [eax + edi - 16], xmm3;
       jmp @addforxloop3

       @loopEnd2:

       sub edi, 128;

       jz @nextLine;

       @addforxloop4:
           movapd xmm0, [ecx + edi];
           mulpd xmm0, xmm6;
           movapd [eax + edi], xmm0;
       add edi, 16;
       jnz @addforxloop4;

       @nextLine:

       // next line:
       add ecx, srcLineWidth;
       add eax, edx;

   // loop y end
   dec Height;
   jnz @@addforyloop;

   // epilog
   pop esi;
   pop edi;
end;

procedure ASMMatrixNormalizeRowUnAlignedEvenW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt);
asm
   push edi;
   push esi;

   // iters := -width*sizeof(double);
   mov esi, width;
   imul esi, -8;
        
   // helper registers for the mt1, mt2 and dest pointers
   sub ecx, esi;
   sub eax, esi;

   // for y := 0 to height - 1:
   @@addforyloop:
       xorpd xmm7, xmm7;

       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, esi;
       @addforxloop:
           add edi, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [ecx + edi];

           // mul add:
           movupd xmm0, [ecx + edi - 128];
           mulpd xmm0, xmm0;
           addpd xmm7, xmm0;

           movupd xmm1, [ecx + edi - 112];
           mulpd xmm1, xmm1;
           addpd xmm7, xmm1;

           movupd xmm2, [ecx + edi - 96];
           mulpd xmm2, xmm2;
           addpd xmm7, xmm2;

           movupd xmm3, [ecx + edi - 80];
           mulpd xmm3, xmm3;
           addpd xmm7, xmm3;

           movupd xmm0, [ecx + edi - 64];
           mulpd xmm0, xmm0;
           addpd xmm7, xmm0;

           movupd xmm1, [ecx + edi - 48];
           mulpd xmm1, xmm1;
           addpd xmm7, xmm1;

           movupd xmm2, [ecx + edi - 32];
           mulpd xmm2, xmm2;
           addpd xmm7, xmm2;

           movupd xmm3, [ecx + edi - 16];
           mulpd xmm3, xmm3;
           addpd xmm7, xmm3;
       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @buildRes;

       @addforxloop2:
           movupd xmm0, [ecx + edi];
           mulpd xmm0, xmm0;
           addpd xmm7, xmm0;
       add edi, 16;
       jnz @addforxloop2;

       @buildRes:

       // build result
       movhlps xmm1, xmm7;
       addsd xmm7, xmm1;
       sqrtsd xmm7, xmm7;
       movsd xmm0, cLocOne;
       divsd xmm0, xmm7;

       movddup xmm6, xmm0;

       // multiply the result and build the result
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, esi;
       @addforxloop3:
           add edi, 128;
           jg @loopEnd2;

           // prefetch data...
           // prefetch [ecx + edi];
           // prefetchw [eax + edi];

           // mul add:
           movupd xmm0, [ecx + edi - 128];
           mulpd xmm0, xmm6;
           movupd [eax + edi - 128], xmm0;

           movupd xmm1, [ecx + edi - 112];
           mulpd xmm1, xmm6;
           movupd [eax + edi - 112], xmm1;

           movupd xmm2, [ecx + edi - 96];
           mulpd xmm2, xmm6;
           movupd [eax + edi - 96], xmm2;

           movupd xmm3, [ecx + edi - 80];
           mulpd xmm3, xmm6;
           movupd [eax + edi - 80], xmm3;

           movupd xmm0, [ecx + edi - 64];
           mulpd xmm0, xmm6;
           movupd [eax + edi - 64], xmm0;

           movupd xmm1, [ecx + edi - 48];
           mulpd xmm1, xmm6;
           movupd [eax + edi - 48], xmm1;

           movupd xmm2, [ecx + edi - 32];
           mulpd xmm2, xmm6;
           movupd [eax + edi - 32], xmm2;

           movupd xmm3, [ecx + edi - 16];
           mulpd xmm3, xmm6;
           movupd [eax + edi - 16], xmm3;
       jmp @addforxloop3

       @loopEnd2:

       sub edi, 128;

       jz @nextLine;

       @addforxloop4:
           movupd xmm0, [ecx + edi];
           mulpd xmm0, xmm6;
           movupd [eax + edi], xmm0;
       add edi, 16;
       jnz @addforxloop4;

       @nextLine:

       // next line:
       add ecx, srcLineWidth;
       add eax, edx;

   // loop y end
   dec Height;
   jnz @@addforyloop;

   // epilog
   pop esi;
   pop edi;
end;

procedure ASMMatrixNormalizeRowAlignedOddW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt);
asm
   push edi;
   push esi;

   // iters := -width*sizeof(double);
   mov esi, width;
   dec esi;
   imul esi, -8;
        
   // helper registers for the mt1, mt2 and dest pointers
   sub ecx, esi;
   sub eax, esi;

   // for y := 0 to height - 1:
   @@addforyloop:
       xorpd xmm7, xmm7;

       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, esi;
       @addforxloop:
           add edi, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [ecx + edi];

           // mul add:
           movapd xmm0, [ecx + edi - 128];
           mulpd xmm0, xmm0;
           addpd xmm7, xmm0;

           movapd xmm1, [ecx + edi - 112];
           mulpd xmm1, xmm1;
           addpd xmm7, xmm1;

           movapd xmm2, [ecx + edi - 96];
           mulpd xmm2, xmm2;
           addpd xmm7, xmm2;

           movapd xmm3, [ecx + edi - 80];
           mulpd xmm3, xmm3;
           addpd xmm7, xmm3;

           movapd xmm0, [ecx + edi - 64];
           mulpd xmm0, xmm0;
           addpd xmm7, xmm0;

           movapd xmm1, [ecx + edi - 48];
           mulpd xmm1, xmm1;
           addpd xmm7, xmm1;

           movapd xmm2, [ecx + edi - 32];
           mulpd xmm2, xmm2;
           addpd xmm7, xmm2;

           movapd xmm3, [ecx + edi - 16];
           mulpd xmm3, xmm3;
           addpd xmm7, xmm3;
       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @buildRes;

       @addforxloop2:
           movapd xmm0, [ecx + edi];
           mulpd xmm0, xmm0;
           addpd xmm7, xmm0;
       add edi, 16;
       jnz @addforxloop2;

       @buildRes:

       // handle last element differently
       movsd xmm2, [ecx];
       mulsd xmm2, xmm2;
       addsd xmm7, xmm2;
       
       // build result
       movhlps xmm1, xmm7;
       addsd xmm7, xmm1;
       sqrtsd xmm7, xmm7;
       movsd xmm0, cLocOne;
       divsd xmm0, xmm7;

       movddup xmm6, xmm0;

       // multiply the result and build the result
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, esi;
       @addforxloop3:
           add edi, 128;
           jg @loopEnd2;

           // prefetch data...
           // prefetch [ecx + edi];
           // prefetchw [eax + edi];

           // mul add:
           movapd xmm0, [ecx + edi - 128];
           mulpd xmm0, xmm6;
           movapd [eax + edi - 128], xmm0;

           movapd xmm1, [ecx + edi - 112];
           mulpd xmm1, xmm6;
           movapd [eax + edi - 112], xmm1;

           movapd xmm2, [ecx + edi - 96];
           mulpd xmm2, xmm6;
           movapd [eax + edi - 96], xmm2;

           movapd xmm3, [ecx + edi - 80];
           mulpd xmm3, xmm6;
           movapd [eax + edi - 80], xmm3;

           movapd xmm0, [ecx + edi - 64];
           mulpd xmm0, xmm6;
           movapd [eax + edi - 64], xmm0;

           movapd xmm1, [ecx + edi - 48];
           mulpd xmm1, xmm6;
           movapd [eax + edi - 48], xmm1;

           movapd xmm2, [ecx + edi - 32];
           mulpd xmm2, xmm6;
           movapd [eax + edi - 32], xmm2;

           movapd xmm3, [ecx + edi - 16];
           mulpd xmm3, xmm6;
           movapd [eax + edi - 16], xmm3;
       jmp @addforxloop3

       @loopEnd2:

       sub edi, 128;

       jz @nextLine;

       @addforxloop4:
           movapd xmm0, [ecx + edi];
           mulpd xmm0, xmm6;
           movapd [eax + edi], xmm0;
       add edi, 16;
       jnz @addforxloop4;

       @nextLine:

       // handle last element
       movsd xmm0, [ecx];
       mulsd xmm0, xmm6;
       movsd [eax], xmm0;

       // next line:
       add ecx, srcLineWidth;
       add eax, edx;

   // loop y end
   dec Height;
   jnz @@addforyloop;

   // epilog
   pop esi;
   pop edi;
end;

procedure ASMMatrixNormalizeRowUnAlignedOddW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt);
asm
   push edi;
   push esi;

   // iters := -width*sizeof(double);
   mov esi, width;
   dec esi;
   imul esi, -8;
        
   // helper registers for the mt1, mt2 and dest pointers
   sub ecx, esi;
   sub eax, esi;

   // for y := 0 to height - 1:
   @@addforyloop:
       xorpd xmm7, xmm7;

       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, esi;
       @addforxloop:
           add edi, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [ecx + edi];

           // mul add:
           movupd xmm0, [ecx + edi - 128];
           mulpd xmm0, xmm0;
           addpd xmm7, xmm0;

           movupd xmm1, [ecx + edi - 112];
           mulpd xmm1, xmm1;
           addpd xmm7, xmm1;

           movupd xmm2, [ecx + edi - 96];
           mulpd xmm2, xmm2;
           addpd xmm7, xmm2;

           movupd xmm3, [ecx + edi - 80];
           mulpd xmm3, xmm3;
           addpd xmm7, xmm3;

           movupd xmm0, [ecx + edi - 64];
           mulpd xmm0, xmm0;
           addpd xmm7, xmm0;

           movupd xmm1, [ecx + edi - 48];
           mulpd xmm1, xmm1;
           addpd xmm7, xmm1;

           movupd xmm2, [ecx + edi - 32];
           mulpd xmm2, xmm2;
           addpd xmm7, xmm2;

           movupd xmm3, [ecx + edi - 16];
           mulpd xmm3, xmm3;
           addpd xmm7, xmm3;
       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @buildRes;

       @addforxloop2:
           movupd xmm0, [ecx + edi];
           mulpd xmm0, xmm0;
           addpd xmm7, xmm0;
       add edi, 16;
       jnz @addforxloop2;

       @buildRes:

       // handle last element differently
       movsd xmm2, [ecx];
       mulsd xmm2, xmm2;
       addsd xmm7, xmm2;
       
       // build result
       movhlps xmm1, xmm7;
       addsd xmm7, xmm1;
       sqrtsd xmm7, xmm7;
       movsd xmm0, cLocOne;
       divsd xmm0, xmm7;

       movddup xmm6, xmm0;

       // multiply the result and build the result
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, esi;
       @addforxloop3:
           add edi, 128;
           jg @loopEnd2;

           // prefetch data...
           // prefetch [ecx + edi];
           // prefetchw [eax + edi];

           // mul add:
           movupd xmm0, [ecx + edi - 128];
           mulpd xmm0, xmm6;
           movupd [eax + edi - 128], xmm0;

           movupd xmm1, [ecx + edi - 112];
           mulpd xmm1, xmm6;
           movupd [eax + edi - 112], xmm1;

           movupd xmm2, [ecx + edi - 96];
           mulpd xmm2, xmm6;
           movupd [eax + edi - 96], xmm2;

           movupd xmm3, [ecx + edi - 80];
           mulpd xmm3, xmm6;
           movupd [eax + edi - 80], xmm3;

           movupd xmm0, [ecx + edi - 64];
           mulpd xmm0, xmm6;
           movupd [eax + edi - 64], xmm0;

           movupd xmm1, [ecx + edi - 48];
           mulpd xmm1, xmm6;
           movupd [eax + edi - 48], xmm1;

           movupd xmm2, [ecx + edi - 32];
           mulpd xmm2, xmm6;
           movupd [eax + edi - 32], xmm2;

           movupd xmm3, [ecx + edi - 16];
           mulpd xmm3, xmm6;
           movupd [eax + edi - 16], xmm3;
       jmp @addforxloop3

       @loopEnd2:

       sub edi, 128;

       jz @nextLine;

       @addforxloop4:
           movupd xmm0, [ecx + edi];
           mulpd xmm0, xmm6;
           movupd [eax + edi], xmm0;
       add edi, 16;
       jnz @addforxloop4;

       @nextLine:

       // handle last element
       movsd xmm0, [ecx];
       mulsd xmm0, xmm6;
       movsd [eax], xmm0;

       // next line:
       add ecx, srcLineWidth;
       add eax, edx;

   // loop y end
   dec Height;
   jnz @@addforyloop;

   // epilog
   pop esi;
   pop edi;
end;

procedure ASMMatrixNormalizeColumnAlignedEvenW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt);
var iters : NativeInt;
asm
   push ebx;
   push edi;
   push esi;

   // iters := height*srcLineWidth;
   mov esi, height;
   imul esi, srcLineWidth;
   imul esi, -1;
   mov ebx, srcLineWidth;
   mov iters, esi;
   
           
   // helper registers for the mt1, mt2 and dest pointers
   sub ecx, esi;

   // for x := 0 to width - 1:
   @@addforxloop:
       xorpd xmm7, xmm7;

       // for y := 0 to height - 1;
       // prepare for reverse loop
       mov edi, iters;
       @addforyloop:
           movapd xmm0, [ecx + edi];
           mulpd xmm0, xmm0;
           addpd xmm7, xmm0;
       add edi, ebx;
       jnz @addforyloop;

       // build result
       sqrtpd xmm7, xmm7;
       movddup xmm6, cLocOne;
       divpd xmm6, xmm7;

       // multiply the result and build the result
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, iters;
       xor esi, esi;

       @addforyloop2:
           movapd xmm0, [ecx + edi];
           mulpd xmm0, xmm6;
           movapd [eax + esi], xmm0;
           add esi, edx;
       add edi, ebx;
       jnz @addforyloop2;

       // next columns:
       add eax, 16;
       add ecx, 16;

   // loop x end
   sub Width, 2;
   jnz @@addforxloop;

   pop esi;
   pop edi;
   pop ebx;
end;

procedure ASMMatrixNormalizeColumnUnAlignedEvenW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt);
var iters : NativeInt;
asm
   push ebx;
   push edi;
   push esi;

   // iters := height*srcLineWidth;
   mov esi, height;
   imul esi, srcLineWidth;
   imul esi, -1;
   mov ebx, srcLineWidth;
   mov iters, esi;
        
   // helper registers for the mt1, mt2 and dest pointers
   sub ecx, esi;

   // for x := 0 to width - 1:
   @@addforxloop:
       xorpd xmm7, xmm7;

       // for y := 0 to height - 1;
       // prepare for reverse loop
       mov edi, iters;
       @addforyloop:
           movupd xmm0, [ecx + edi];
           mulpd xmm0, xmm0;
           addpd xmm7, xmm0;
       add edi, ebx;
       jnz @addforyloop;

       // build result
       sqrtpd xmm7, xmm7;
       movddup xmm6, cLocOne;
       divpd xmm6, xmm7;

       // multiply the result and build the result
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, iters;
       xor esi, esi;

       @addforyloop2:
           movupd xmm0, [ecx + edi];
           mulpd xmm0, xmm6;
           movupd [eax + esi], xmm0;
           add esi, edx;
       add edi, ebx;
       jnz @addforyloop2;

       // next columns:
       add eax, 16;
       add ecx, 16;

   // loop x end
   sub Width, 2;
   jnz @@addforxloop;

   pop esi;
   pop edi;
   pop ebx;
end;

procedure ASMMatrixNormalizeColumnAlignedOddW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt);
var iters : NativeInt;
asm
   push ebx;
   push edi;
   push esi;

   // iters := height*srcLineWidth;
   mov esi, height;
   imul esi, srcLineWidth;
   imul esi, -1;
   mov ebx, srcLineWidth;        
   mov iters, esi;
        
   // helper registers for the mt1, mt2 and dest pointers
   sub ecx, esi;
   
   dec Width;
   jz @@lastColumn;
   
   // for x := 0 to width - 1:
   @@addforxloop:
       xorpd xmm7, xmm7;

       // for y := 0 to height - 1;
       // prepare for reverse loop
       mov edi, iters;
       @addforyloop:
           movapd xmm0, [ecx + edi];
           mulpd xmm0, xmm0;
           addpd xmm7, xmm0;
       add edi, ebx;
       jnz @addforyloop;

       // build result
       sqrtpd xmm7, xmm7;
       movddup xmm6, cLocOne;
       divpd xmm6, xmm7;

       // multiply the result and build the result
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, iters;
       xor esi, esi;

       @addforyloop2:
           movapd xmm0, [ecx + edi];
           mulpd xmm0, xmm6;
           movapd [eax + esi], xmm0;
           add esi, edx;
       add edi, ebx;
       jnz @addforyloop2;

       // next columns:
       add eax, 16;
       add ecx, 16;

   // loop x end
   sub Width, 2;
   jnz @@addforxloop;

   @@lastColumn:
   // handle last column
   xorpd xmm7, xmm7;

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov edi, iters;
   @addforyloop3:
       movsd xmm0, [ecx + edi];
       mulsd xmm0, xmm0;
       addsd xmm7, xmm0;
   add edi, ebx;
   jnz @addforyloop3;

   // build result
   sqrtsd xmm7, xmm7;
   movsd xmm6, cLocOne;
   divsd xmm6, xmm7;

   // multiply the result and build the result
   // for x := 0 to w - 1;
   // prepare for reverse loop
   mov edi, iters;
   xor esi, esi;

   @addforyloop4:
       movsd xmm0, [ecx + edi];
       mulsd xmm0, xmm6;
       movsd [eax + esi], xmm0;
       add esi, edx;
   add edi, ebx;
   jnz @addforyloop4;

   pop esi;
   pop edi;
   pop ebx;
end;

procedure ASMMatrixNormalizeColumnUnAlignedOddW(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt);
var iters : NativeInt;
asm
   push ebx;
   push edi;
   push esi;

   // iters := height*srcLineWidth;
   mov esi, height;
   imul esi, srcLineWidth;
   imul esi, -1;
        
   mov ebx, srcLineWidth;
   mov iters, esi;
        
   // helper registers for the mt1, mt2 and dest pointers
   sub ecx, esi;
   
   dec Width;
   jz @@lastColumn;
   
   // for x := 0 to width - 1:
   @@addforxloop:
       xorpd xmm7, xmm7;

       // for y := 0 to height - 1;
       // prepare for reverse loop
       mov edi, iters;
       @addforyloop:
           movupd xmm0, [ecx + edi];
           mulpd xmm0, xmm0;
           addpd xmm7, xmm0;
       add edi, ebx;
       jnz @addforyloop;

       // build result
       sqrtpd xmm7, xmm7;
       movddup xmm6, cLocOne;
       divpd xmm6, xmm7;

       // multiply the result and build the result
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, iters;
       xor esi, esi;

       @addforyloop2:
           movupd xmm0, [ecx + edi];
           mulpd xmm0, xmm6;
           movupd [eax + esi], xmm0;
           add esi, edx;
       add edi, ebx;
       jnz @addforyloop2;

       // next columns:
       add eax, 16;
       add ecx, 16;

   // loop x end
   sub Width, 2;
   jnz @@addforxloop;

   @@lastColumn:
   // handle last column
   xorpd xmm7, xmm7;

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov edi, iters;
   @addforyloop3:
       movsd xmm0, [ecx + edi];
       mulsd xmm0, xmm0;
       addsd xmm7, xmm0;
   add edi, ebx;
   jnz @addforyloop3;

   // build result
   sqrtsd xmm7, xmm7;
   movsd xmm6, cLocOne;
   divsd xmm6, xmm7;

   // multiply the result and build the result
   // for x := 0 to w - 1;
   // prepare for reverse loop
   mov edi, iters;
   xor esi, esi;

   @addforyloop4:
       movsd xmm0, [ecx + edi];
       mulsd xmm0, xmm6;
       movsd [eax + esi], xmm0;
       add esi, edx;
   add edi, ebx;
   jnz @addforyloop4;

   pop esi;
   pop edi;
   pop ebx;
end;

{$ENDIF}

end.
