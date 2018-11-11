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


unit ASMMatrixSumOperations;

interface

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}
{$IFNDEF x64}

uses MatrixConst;

procedure ASMMatrixSumRowAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixSumRowUnAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixSumRowAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixSumRowUnAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

procedure ASMMatrixSumColumnAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixSumColumnUnAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixSumColumnAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixSumColumnUnAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

{$ENDIF}

implementation

{$IFNDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$S-} {$ENDIF}

procedure ASMMatrixSumRowAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
asm
   push ebx;
   push edi;
   push esi;

   // iters := -width*sizeof(double)
   mov esi, width;
   imul esi, -8;
        
   // helper registers for the mt1, mt2 and dest pointers
   sub ecx, esi;

   // for y := 0 to height - 1:
   mov ebx, Height;
   @@addforyloop:
       xorpd xmm0, xmm0;
       xorpd xmm1, xmm1;
       xorpd xmm2, xmm2;
       xorpd xmm3, xmm3;

       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, esi;
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

       // build result
       addpd xmm0, xmm1;
       addpd xmm2, xmm3;
       addpd xmm0, xmm2;

       movhlps xmm1, xmm0;
       addsd xmm0, xmm1;

       // write result
       movsd [eax], xmm0;

       // next line:
       add ecx, srcLineWidth;
       add eax, edx;

   // loop y end
   dec ebx;
   jnz @@addforyloop;

   pop esi;
   pop edi;
   pop ebx;
end;

procedure ASMMatrixSumRowUnAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
asm
   push ebx;
   push edi;
   push esi;

   // iters := -width*sizeof(double)
   mov esi, width;
   imul esi, -8;
        
   // helper registers for the mt1, mt2 and dest pointers
   sub ecx, esi;

   // for y := 0 to height - 1:
   mov ebx, Height;
   @@addforyloop:
       xorpd xmm7, xmm7;

       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, esi;
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
       movhlps xmm1, xmm7;
       addsd xmm7, xmm1;

       movsd [eax], xmm7;

       // next line:
       add ecx, srcLineWidth;
       add eax, edx;

   // loop y end
   dec ebx;
   jnz @@addforyloop;

   pop esi;
   pop edi;
   pop ebx;
end;

procedure ASMMatrixSumRowAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
asm
   push ebx;
   push edi;
   push esi;

   // iters := -(width - 1)*sizeof(double)
   mov esi, width;
   dec esi;
   imul esi, -8;
        
   // helper registers for the mt1, mt2 and dest pointers
   sub ecx, esi;

   // for y := 0 to height - 1:
   mov ebx, Height;
   @@addforyloop:
       xorpd xmm0, xmm0;
       xorpd xmm1, xmm1;
       xorpd xmm2, xmm2;
       xorpd xmm3, xmm3;

       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, esi;
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
           movapd xmm0, [ecx + edi];
           addpd xmm7, xmm0;
       add edi, 16;
       jnz @addforxloop2;

       @buildRes:

       // handle last element differently
       movsd xmm2, [ecx + edi];
       addsd xmm7, xmm2;

       // build result
       addpd xmm0, xmm1;
       addpd xmm2, xmm3;
       addpd xmm0, xmm2;

       movhlps xmm1, xmm0;
       addsd xmm0, xmm1;

       // write result
       movsd [eax], xmm0;

       // next line:
       add ecx, srcLineWidth;
       add eax, edx;

   // loop y end
   dec ebx;
   jnz @@addforyloop;

   pop esi;
   pop edi;
   pop ebx;
end;

procedure ASMMatrixSumRowUnAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
asm
   push ebx;
   push edi;
   push esi;

   // iters := -(width - 1)*sizeof(double)
   mov esi, width;
   dec esi;
   imul esi, -8;
        
   // helper registers for the mt1, mt2 and dest pointers
   sub ecx, esi;

   // for y := 0 to height - 1:
   mov ebx, Height;
   @@addforyloop:
       xorpd xmm7, xmm7;

       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, esi;
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
       movhlps xmm1, xmm7;
       addsd xmm7, xmm1;

       movsd [eax], xmm7;

       // next line:
       add ecx, srcLineWidth;
       add eax, edx;

   // loop y end
   dec ebx;
   jnz @@addforyloop;

   pop esi;
   pop edi;
   pop ebx;
end;

procedure ASMMatrixSumColumnAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
asm
   push ebx;
   push edi;
   push esi;

   // init
   mov edx, srcLineWidth;
   mov esi, height;
   imul esi, edx;
   imul esi, -1;
   
   // helper registers for the mt1, mt2 and dest pointers
   sub ecx, esi;

   // for x := 0 to width - 1:
   mov ebx, Width;
   sar ebx, 1;
   @@addforxloop:
       xorpd xmm0, xmm0;

       // for y := 0 to height - 1;
       // prepare for reverse loop
       mov edi, esi;
       @addforyloop:
           addpd xmm0, [ecx + edi];
       add edi, edx;
       jnz @addforyloop;

       // build result
       movapd [eax], xmm0;

       // next columns:
       add ecx, 16;
       add eax, 16;

   // loop x end
   dec ebx;
   jnz @@addforxloop;

   pop esi;
   pop edi;
   pop ebx;
end;

procedure ASMMatrixSumColumnUnAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
asm
   push ebx;
   push edi;
   push esi;

   // init
   mov edx, srcLineWidth;
   mov esi, height;
   imul esi, edx;
   imul esi, -1;
   
   // helper registers for the mt1, mt2 and dest pointers
   sub ecx, esi;

   // for x := 0 to width - 1:
   mov ebx, Width;
   sar ebx, 1;
   @@addforxloop:
       xorpd xmm7, xmm7;

       // for y := 0 to height - 1;
       // prepare for reverse loop
       mov edi, esi;
       @addforyloop:
           movupd xmm0, [ecx + edi];
           addpd xmm7, xmm0;
       add edi, edx;
       jnz @addforyloop;

       // build result
       movupd [eax], xmm7;

       // next columns:
       add ecx, 16;
       add eax, 16;

   // loop x end
   dec ebx;
   jnz @@addforxloop;

   pop esi;
   pop edi;
   pop ebx;
end;

procedure ASMMatrixSumColumnAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
asm
   push ebx;
   push edi;
   push esi;

   // init
   mov edx, srcLineWidth;
   mov esi, height;
   imul esi, edx;
   imul esi, -1;
   
   // helper registers for the mt1, mt2 and dest pointers
   sub ecx, esi;

   // for x := 0 to width - 1:
   mov ebx, Width;
   sar ebx, 1;
   @@addforxloop:
       xorpd xmm0, xmm0;

       // for y := 0 to height - 1;
       // prepare for reverse loop
       mov edi, esi;
       @addforyloop:
           addpd xmm0, [ecx + edi];
       add edi, edx;
       jnz @addforyloop;

       // build result
       movapd [eax], xmm0;

       // next columns:
       add ecx, 16;
       add eax, 16;

   // loop x end
   dec ebx;
   jnz @@addforxloop;

   @lastColumn:
   // handle last column
   xorpd xmm7, xmm7;

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov edi, esi;
   @addforyloop3:
       movsd xmm0, [ecx + edi];
       addsd xmm7, xmm0;
   add edi, edx;
   jnz @addforyloop3;

   // build result
   movsd [eax], xmm7;

   pop esi;
   pop edi;
   pop ebx;
end;

procedure ASMMatrixSumColumnUnAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
asm
   push ebx;
   push edi;
   push esi;

   // init
   mov edx, srcLineWidth;
   mov esi, height;
   imul esi, edx;
   imul esi, -1;
   
   // helper registers for the mt1, mt2 and dest pointers
   sub ecx, esi;

   // for x := 0 to width - 1:
   mov ebx, Width;
   sar ebx, 1;
   @@addforxloop:
       xorpd xmm7, xmm7;

       // for y := 0 to height - 1;
       // prepare for reverse loop
       mov edi, esi;
       @addforyloop:
           movupd xmm0, [ecx + edi];
           addpd xmm7, xmm0;
       add edi, edx;
       jnz @addforyloop;

       // build result
       movupd [eax], xmm7;

       // next columns:
       add ecx, 16;
       add eax, 16;

   // loop x end
   dec ebx;
   jnz @@addforxloop;

   @lastColumn:
   // handle last column
   xorpd xmm7, xmm7;

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov edi, esi;
   @addforyloop3:
       movsd xmm0, [ecx + edi];
       addsd xmm7, xmm0;
   add edi, edx;
   jnz @addforyloop3;

   // build result
   movsd [eax], xmm7;

   pop esi;
   pop edi;
   pop ebx;
end;

{$ENDIF}

end.
