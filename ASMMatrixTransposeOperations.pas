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


unit ASMMatrixTransposeOperations;

interface

{$I 'mrMath_CPU.inc'}

{$IFNDEF x64}

uses MatrixConst;

procedure ASMMatrixTransposeAlignedEvenWEvenH(dest : PDouble; const destLineWidth : NativeInt; mt : PDouble; const LineWidth : NativeInt; width : NativeInt; height : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixTransposeUnAlignedEvenWEvenH(dest : PDouble; const destLineWidth : NativeInt; mt : PDouble; const LineWidth : NativeInt; width : NativeInt; height : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

procedure ASMMatrixTransposeAlignedEvenWOddH(dest : PDouble; const destLineWidth : NativeInt; mt : PDouble; const LineWidth : NativeInt; width : NativeInt; height : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixTransposeUnAlignedEvenWOddH(dest : PDouble; const destLineWidth : NativeInt; mt : PDouble; const LineWidth : NativeInt; width : NativeInt; height : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

procedure ASMMatrixTransposeAlignedOddWEvenH(dest : PDouble; const destLineWidth : NativeInt; mt : PDouble; const LineWidth : NativeInt; width : NativeInt; height : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixTransposeUnAlignedOddWEvenH(dest : PDouble; const destLineWidth : NativeInt; mt : PDouble; const LineWidth : NativeInt; width : NativeInt; height : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

procedure ASMMatrixTransposeAlignedOddWOddH(dest : PDouble; const destLineWidth : NativeInt; mt : PDouble; const LineWidth : NativeInt; width : NativeInt; height : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixTransposeUnAlignedOddWOddH(dest : PDouble; const destLineWidth : NativeInt; mt : PDouble; const LineWidth : NativeInt; width : NativeInt; height : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

procedure ASMMatrixTransposeInplace(mt : PDouble; const LineWidth : NativeInt; N : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

{$ENDIF}

implementation

{$IFNDEF x64}

procedure ASMMatrixTransposeAlignedEvenWEvenH(dest : PDouble; const destLineWidth : NativeInt; mt : PDouble; const LineWidth : NativeInt; width : NativeInt; height : NativeInt);
var iters : NativeInt;
asm
   push esi;
   push edi;
   push ebx;

   // init
   // iters := -width*sizeof(double); 
   mov esi, width;
   imul esi, -8;
   mov iters, esi;

   sub ecx, esi;

   @foryloop:
       push eax;

       mov edi, ecx;
       add edi, LineWidth;

       // unrolled loop
       mov esi, iters;
       @forxloop:
           add esi, 128;
           jg @loopend;

           // prefetch [ecx + esi];
           // prefetch [edi + esi];

           movapd xmm0, [ecx + esi - 128];
           movapd xmm1, [edi + esi - 128];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movapd [eax], xmm0;
           movapd [eax + edx], xmm1;

           lea eax, [eax + 2*edx];

           movapd xmm0, [ecx + esi - 112];
           movapd xmm1, [edi + esi - 112];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movapd [eax], xmm0;
           movapd [eax + edx], xmm1;

           lea eax, [eax + 2*edx];

           movapd xmm0, [ecx + esi - 96];
           movapd xmm1, [edi + esi - 96];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movapd [eax], xmm0;
           movapd [eax + edx], xmm1;

           lea eax, [eax + 2*edx];

           movapd xmm0, [ecx + esi - 80];
           movapd xmm1, [edi + esi - 80];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movapd [eax], xmm0;
           movapd [eax + edx], xmm1;

           lea eax, [eax + 2*edx];

           movapd xmm0, [ecx + esi - 64];
           movapd xmm1, [edi + esi - 64];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movapd [eax], xmm0;
           movapd [eax + edx], xmm1;

           lea eax, [eax + 2*edx];

           movapd xmm0, [ecx + esi - 48];
           movapd xmm1, [edi + esi - 48];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movapd [eax], xmm0;
           movapd [eax + edx], xmm1;

           lea eax, [eax + 2*edx];

           movapd xmm0, [ecx + esi - 32];
           movapd xmm1, [edi + esi - 32];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movapd [eax], xmm0;
           movapd [eax + edx], xmm1;

           lea eax, [eax + 2*edx];

           movapd xmm0, [ecx + esi - 16];
           movapd xmm1, [edi + esi - 16];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movapd [eax], xmm0;
           movapd [eax + edx], xmm1;

           lea eax, [eax + 2*edx];

       jmp @forxloop;

       @loopend:

       sub esi, 128;

       jz @nextLine;

       @forxloop2:
           movapd xmm0, [ecx + esi];
           movapd xmm1, [edi + esi];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movapd [eax], xmm0;
           movapd [eax + edx], xmm1;

           lea eax, [eax + 2*edx];
       add esi, 16;
       jnz @forxloop2;

       @nextLine:

       // increment pointers
       pop eax;
       add eax, 16;

       mov esi, LineWidth;
       lea ecx, [ecx + 2*esi];

   sub height, 2;
   jnz @foryloop;

   pop ebx;
   pop edi;
   pop esi;
end;

procedure ASMMatrixTransposeUnAlignedEvenWEvenH(dest : PDouble; const destLineWidth : NativeInt; mt : PDouble; const LineWidth : NativeInt; width : NativeInt; height : NativeInt);
var iters : NativeInt;
asm
   push esi;
   push edi;
   push ebx;

   // init
   // iters := -width*sizeof(double); 
   mov esi, width;
   imul esi, -8;
   mov iters, esi;

   sub ecx, esi;

   @foryloop:
       push eax;

       mov edi, ecx;
       add edi, LineWidth;

       // unrolled loop
       mov esi, iters;
       @forxloop:
           add esi, 128;
           jg @loopend;

           // prefetch [ecx + esi];
           // prefetch [edi + esi];

           movupd xmm0, [ecx + esi - 128];
           movupd xmm1, [edi + esi - 128];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movupd [eax], xmm0;
           movupd [eax + edx], xmm1;

           lea eax, [eax + 2*edx];

           movupd xmm0, [ecx + esi - 112];
           movupd xmm1, [edi + esi - 112];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movupd [eax], xmm0;
           movupd [eax + edx], xmm1;

           lea eax, [eax + 2*edx];

           movupd xmm0, [ecx + esi - 96];
           movupd xmm1, [edi + esi - 96];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movupd [eax], xmm0;
           movupd [eax + edx], xmm1;

           lea eax, [eax + 2*edx];

           movupd xmm0, [ecx + esi - 80];
           movupd xmm1, [edi + esi - 80];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movupd [eax], xmm0;
           movupd [eax + edx], xmm1;

           lea eax, [eax + 2*edx];

           movupd xmm0, [ecx + esi - 64];
           movupd xmm1, [edi + esi - 64];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movupd [eax], xmm0;
           movupd [eax + edx], xmm1;

           lea eax, [eax + 2*edx];

           movupd xmm0, [ecx + esi - 48];
           movupd xmm1, [edi + esi - 48];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movupd [eax], xmm0;
           movupd [eax + edx], xmm1;

           lea eax, [eax + 2*edx];

           movupd xmm0, [ecx + esi - 32];
           movupd xmm1, [edi + esi - 32];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movupd [eax], xmm0;
           movupd [eax + edx], xmm1;

           lea eax, [eax + 2*edx];

           movupd xmm0, [ecx + esi - 16];
           movupd xmm1, [edi + esi - 16];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movupd [eax], xmm0;
           movupd [eax + edx], xmm1;

           lea eax, [eax + 2*edx];

       jmp @forxloop;

       @loopend:

       sub esi, 128;

       jz @nextLine;

       @forxloop2:
           movupd xmm0, [ecx + esi];
           movupd xmm1, [edi + esi];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movupd [eax], xmm0;
           movupd [eax + edx], xmm1;

           lea eax, [eax + 2*edx];
       add esi, 16;
       jnz @forxloop2;

       @nextLine:

       // increment pointers
       pop eax;
       add eax, 16;

       mov esi, LineWidth;
       lea ecx, [ecx + 2*esi];

   sub height, 2;
   jnz @foryloop;

   pop ebx;
   pop edi;
   pop esi;
end;

procedure ASMMatrixTransposeAlignedOddWEvenH(dest : PDouble; const destLineWidth : NativeInt; mt : PDouble; const LineWidth : NativeInt; width : NativeInt; height : NativeInt);
var iters : NativeInt;
asm
   push esi;
   push edi;
   push ebx;

   // init
   // iters := -width*sizeof(double); 
   mov esi, width;
   dec esi;
   imul esi, -8;
   mov iters, esi;

   sub ecx, esi;

   @foryloop:
       push eax;

       mov edi, ecx;
       add edi, LineWidth;

       // unrolled loop
       mov esi, iters;
       @forxloop:
           add esi, 128;
           jg @loopend;

           // prefetch [ecx + esi];
           // prefetch [edi + esi];

           movapd xmm0, [ecx + esi - 128];
           movapd xmm1, [edi + esi - 128];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movapd [eax], xmm0;
           movapd [eax + edx], xmm1;

           lea eax, [eax + 2*edx];

           movapd xmm0, [ecx + esi - 112];
           movapd xmm1, [edi + esi - 112];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movapd [eax], xmm0;
           movapd [eax + edx], xmm1;

           lea eax, [eax + 2*edx];

           movapd xmm0, [ecx + esi - 96];
           movapd xmm1, [edi + esi - 96];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movapd [eax], xmm0;
           movapd [eax + edx], xmm1;

           lea eax, [eax + 2*edx];

           movapd xmm0, [ecx + esi - 80];
           movapd xmm1, [edi + esi - 80];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movapd [eax], xmm0;
           movapd [eax + edx], xmm1;

           lea eax, [eax + 2*edx];

           movapd xmm0, [ecx + esi - 64];
           movapd xmm1, [edi + esi - 64];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movapd [eax], xmm0;
           movapd [eax + edx], xmm1;

           lea eax, [eax + 2*edx];

           movapd xmm0, [ecx + esi - 48];
           movapd xmm1, [edi + esi - 48];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movapd [eax], xmm0;
           movapd [eax + edx], xmm1;

           lea eax, [eax + 2*edx];

           movapd xmm0, [ecx + esi - 32];
           movapd xmm1, [edi + esi - 32];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movapd [eax], xmm0;
           movapd [eax + edx], xmm1;

           lea eax, [eax + 2*edx];

           movapd xmm0, [ecx + esi - 16];
           movapd xmm1, [edi + esi - 16];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movapd [eax], xmm0;
           movapd [eax + edx], xmm1;

           lea eax, [eax + 2*edx];

       jmp @forxloop;

       @loopend:

       sub esi, 128;

       jz @nextLine;

       @forxloop2:
           movapd xmm0, [ecx + esi];
           movapd xmm1, [edi + esi];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movapd [eax], xmm0;
           movapd [eax + edx], xmm1;

           lea eax, [eax + 2*edx];
       add esi, 16;
       jnz @forxloop2;

       @nextLine:

       // handle last element differently
       movsd xmm0, [ecx + esi];
       movsd xmm1, [edi + esi];

       movlhps xmm0, xmm1;
       movapd [eax], xmm0;

       // increment pointers
       pop eax;
       mov esi, LineWidth;
       
       add eax, 16;
       lea ecx, [ecx + 2*esi];

   sub height, 2;
   jnz @foryloop;

   pop ebx;
   pop edi;
   pop esi;
end;

procedure ASMMatrixTransposeUnAlignedOddWEvenH(dest : PDouble; const destLineWidth : NativeInt; mt : PDouble; const LineWidth : NativeInt; width : NativeInt; height : NativeInt);
var iters : NativeInt;
asm
   push esi;
   push edi;
   push ebx;

   // init
   // iters := -width*sizeof(double); 
   mov esi, width;
   dec esi;
   imul esi, -8;
   mov iters, esi;

   sub ecx, esi;

   @foryloop:
       push eax;

       mov edi, ecx;
       add edi, LineWidth;

       // unrolled loop
       mov esi, iters;
       @forxloop:
           add esi, 128;
           jg @loopend;

           // prefetch [ecx + esi];
           // prefetch [edi + esi];

           movupd xmm0, [ecx + esi - 128];
           movupd xmm1, [edi + esi - 128];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movupd [eax], xmm0;
           movupd [eax + edx], xmm1;

           lea eax, [eax + 2*edx];

           movupd xmm0, [ecx + esi - 112];
           movupd xmm1, [edi + esi - 112];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movupd [eax], xmm0;
           movupd [eax + edx], xmm1;

           lea eax, [eax + 2*edx];

           movupd xmm0, [ecx + esi - 96];
           movupd xmm1, [edi + esi - 96];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movupd [eax], xmm0;
           movupd [eax + edx], xmm1;

           lea eax, [eax + 2*edx];

           movupd xmm0, [ecx + esi - 80];
           movupd xmm1, [edi + esi - 80];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movupd [eax], xmm0;
           movupd [eax + edx], xmm1;

           lea eax, [eax + 2*edx];

           movupd xmm0, [ecx + esi - 64];
           movupd xmm1, [edi + esi - 64];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movupd [eax], xmm0;
           movupd [eax + edx], xmm1;

           lea eax, [eax + 2*edx];

           movupd xmm0, [ecx + esi - 48];
           movupd xmm1, [edi + esi - 48];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movupd [eax], xmm0;
           movupd [eax + edx], xmm1;

           lea eax, [eax + 2*edx];

           movupd xmm0, [ecx + esi - 32];
           movupd xmm1, [edi + esi - 32];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movupd [eax], xmm0;
           movupd [eax + edx], xmm1;

           lea eax, [eax + 2*edx];

           movupd xmm0, [ecx + esi - 16];
           movupd xmm1, [edi + esi - 16];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movupd [eax], xmm0;
           movupd [eax + edx], xmm1;

           lea eax, [eax + 2*edx];

       jmp @forxloop;

       @loopend:

       sub esi, 128;

       jz @nextLine;

       @forxloop2:
           movupd xmm0, [ecx + esi];
           movupd xmm1, [edi + esi];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movupd [eax], xmm0;
           movupd [eax + edx], xmm1;

           lea eax, [eax + 2*edx];
       add esi, 16;
       jnz @forxloop2;

       @nextLine:

       // handle last element differently
       movsd xmm0, [ecx + esi];
       movsd xmm1, [edi + esi];

       movlhps xmm0, xmm1;
       movupd [eax], xmm0;

       // increment pointers
       pop eax;
       mov esi, LineWidth;
       
       add eax, 16;
       lea ecx, [ecx + 2*esi];

   sub height, 2;
   jnz @foryloop;

   pop ebx;
   pop edi;
   pop esi;
end;

procedure ASMMatrixTransposeAlignedEvenWOddH(dest : PDouble; const destLineWidth : NativeInt; mt : PDouble; const LineWidth : NativeInt; width : NativeInt; height : NativeInt);
var iters : NativeInt;
asm
   push esi;
   push edi;
   push ebx;

   // init
   // iters := -width*sizeof(double); 
   mov esi, width;
   imul esi, -8;
   mov iters, esi;

   sub ecx, esi;
   sub height, 1;

   @foryloop:
       push eax;

       mov edi, ecx;
       add edi, LineWidth;

       // unrolled loop
       mov esi, iters;
       @forxloop:
           add esi, 128;
           jg @loopend;

           // prefetch [ecx + esi];
           // prefetch [edi + esi];

           movapd xmm0, [ecx + esi - 128];
           movapd xmm1, [edi + esi - 128];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movapd [eax], xmm0;
           movapd [eax + edx], xmm1;

           lea eax, [eax + 2*edx];

           movapd xmm0, [ecx + esi - 112];
           movapd xmm1, [edi + esi - 112];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movapd [eax], xmm0;
           movapd [eax + edx], xmm1;

           lea eax, [eax + 2*edx];

           movapd xmm0, [ecx + esi - 96];
           movapd xmm1, [edi + esi - 96];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movapd [eax], xmm0;
           movapd [eax + edx], xmm1;

           lea eax, [eax + 2*edx];

           movapd xmm0, [ecx + esi - 80];
           movapd xmm1, [edi + esi - 80];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movapd [eax], xmm0;
           movapd [eax + edx], xmm1;

           lea eax, [eax + 2*edx];

           movapd xmm0, [ecx + esi - 64];
           movapd xmm1, [edi + esi - 64];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movapd [eax], xmm0;
           movapd [eax + edx], xmm1;

           lea eax, [eax + 2*edx];

           movapd xmm0, [ecx + esi - 48];
           movapd xmm1, [edi + esi - 48];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movapd [eax], xmm0;
           movapd [eax + edx], xmm1;

           lea eax, [eax + 2*edx];

           movapd xmm0, [ecx + esi - 32];
           movapd xmm1, [edi + esi - 32];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movapd [eax], xmm0;
           movapd [eax + edx], xmm1;

           lea eax, [eax + 2*edx];

           movapd xmm0, [ecx + esi - 16];
           movapd xmm1, [edi + esi - 16];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movapd [eax], xmm0;
           movapd [eax + edx], xmm1;

           lea eax, [eax + 2*edx];

       jmp @forxloop;

       @loopend:

       sub esi, 128;

       jz @nextLine;

       @forxloop2:
           movapd xmm0, [ecx + esi];
           movapd xmm1, [edi + esi];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movapd [eax], xmm0;
           movapd [eax + edx], xmm1;

           lea eax, [eax + 2*edx];
       add esi, 16;
       jnz @forxloop2;

       @nextLine:

       // increment pointers
       pop eax;
       add eax, 16;

       mov esi, LineWidth;
       lea ecx, [ecx + 2*esi];

   sub height, 2;
   jnz @foryloop;

   // handle last line differently
   mov esi, iters;
   @forxloop3:
       movapd xmm0, [ecx + esi];

       movhlps xmm1, xmm0;
       movsd [eax], xmm0;
       movsd [eax + edx], xmm1;

       lea eax, [eax + 2*edx];
   add esi, 16;
   jnz @forxloop3;

   pop ebx;
   pop edi;
   pop esi;
end;

procedure ASMMatrixTransposeUnAlignedEvenWOddH(dest : PDouble; const destLineWidth : NativeInt; mt : PDouble; const LineWidth : NativeInt; width : NativeInt; height : NativeInt);
var iters : NativeInt;
asm
   push esi;
   push edi;
   push ebx;

   // init
   // iters := -width*sizeof(double); 
   mov esi, width;
   imul esi, -8;
   mov iters, esi;

   sub ecx, esi;
   sub Height, 1;

   @foryloop:
       push eax;

       mov edi, ecx;
       add edi, LineWidth;

       // unrolled loop
       mov esi, iters;
       @forxloop:
           add esi, 128;
           jg @loopend;

           // prefetch [ecx + esi];
           // prefetch [edi + esi];

           movupd xmm0, [ecx + esi - 128];
           movupd xmm1, [edi + esi - 128];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movupd [eax], xmm0;
           movupd [eax + edx], xmm1;

           lea eax, [eax + 2*edx];

           movupd xmm0, [ecx + esi - 112];
           movupd xmm1, [edi + esi - 112];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movupd [eax], xmm0;
           movupd [eax + edx], xmm1;

           lea eax, [eax + 2*edx];

           movupd xmm0, [ecx + esi - 96];
           movupd xmm1, [edi + esi - 96];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movupd [eax], xmm0;
           movupd [eax + edx], xmm1;

           lea eax, [eax + 2*edx];

           movupd xmm0, [ecx + esi - 80];
           movupd xmm1, [edi + esi - 80];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movupd [eax], xmm0;
           movupd [eax + edx], xmm1;

           lea eax, [eax + 2*edx];

           movupd xmm0, [ecx + esi - 64];
           movupd xmm1, [edi + esi - 64];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movupd [eax], xmm0;
           movupd [eax + edx], xmm1;

           lea eax, [eax + 2*edx];

           movupd xmm0, [ecx + esi - 48];
           movupd xmm1, [edi + esi - 48];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movupd [eax], xmm0;
           movupd [eax + edx], xmm1;

           lea eax, [eax + 2*edx];

           movupd xmm0, [ecx + esi - 32];
           movupd xmm1, [edi + esi - 32];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movupd [eax], xmm0;
           movupd [eax + edx], xmm1;

           lea eax, [eax + 2*edx];

           movupd xmm0, [ecx + esi - 16];
           movupd xmm1, [edi + esi - 16];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movupd [eax], xmm0;
           movupd [eax + edx], xmm1;

           lea eax, [eax + 2*edx];

       jmp @forxloop;

       @loopend:

       sub esi, 128;

       jz @nextLine;

       @forxloop2:
           movupd xmm0, [ecx + esi];
           movupd xmm1, [edi + esi];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movupd [eax], xmm0;
           movupd [eax + edx], xmm1;

           lea eax, [eax + 2*edx];
       add esi, 16;
       jnz @forxloop2;

       @nextLine:

       // increment pointers
       pop eax;
       add eax, 16;

       mov esi, LineWidth;
       lea ecx, [ecx + 2*esi];

   sub height, 2;
   jnz @foryloop;

   // handle last line differently
   mov esi, iters;
   @forxloop3:
       movupd xmm0, [ecx + esi];

       movhlps xmm1, xmm0;
       movsd [eax], xmm0;
       movsd [eax + edx], xmm1;

       lea eax, [eax + 2*edx];
   add esi, 16;
   jnz @forxloop3;

   pop ebx;
   pop edi;
   pop esi;
end;

procedure ASMMatrixTransposeAlignedOddWOddH(dest : PDouble; const destLineWidth : NativeInt; mt : PDouble; const LineWidth : NativeInt; width : NativeInt; height : NativeInt);
var iters : NativeInt;
asm
   push esi;
   push edi;
   push ebx;

   // init
   // iters := -width*sizeof(double); 
   mov esi, width;
   dec esi;
   imul esi, -8;
   mov iters, esi;

   sub ecx, esi;
   sub height, 1;

   @foryloop:
       push eax;

       mov edi, ecx;
       add edi, LineWidth;

       // unrolled loop
       mov esi, iters;
       @forxloop:
           add esi, 128;
           jg @loopend;

           // prefetch [ecx + esi];
           // prefetch [edi + esi];

           movapd xmm0, [ecx + esi - 128];
           movapd xmm1, [edi + esi - 128];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movapd [eax], xmm0;
           movapd [eax + edx], xmm1;

           lea eax, [eax + 2*edx];

           movapd xmm0, [ecx + esi - 112];
           movapd xmm1, [edi + esi - 112];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movapd [eax], xmm0;
           movapd [eax + edx], xmm1;

           lea eax, [eax + 2*edx];

           movapd xmm0, [ecx + esi - 96];
           movapd xmm1, [edi + esi - 96];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movapd [eax], xmm0;
           movapd [eax + edx], xmm1;

           lea eax, [eax + 2*edx];

           movapd xmm0, [ecx + esi - 80];
           movapd xmm1, [edi + esi - 80];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movapd [eax], xmm0;
           movapd [eax + edx], xmm1;

           lea eax, [eax + 2*edx];

           movapd xmm0, [ecx + esi - 64];
           movapd xmm1, [edi + esi - 64];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movapd [eax], xmm0;
           movapd [eax + edx], xmm1;

           lea eax, [eax + 2*edx];

           movapd xmm0, [ecx + esi - 48];
           movapd xmm1, [edi + esi - 48];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movapd [eax], xmm0;
           movapd [eax + edx], xmm1;

           lea eax, [eax + 2*edx];

           movapd xmm0, [ecx + esi - 32];
           movapd xmm1, [edi + esi - 32];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movapd [eax], xmm0;
           movapd [eax + edx], xmm1;

           lea eax, [eax + 2*edx];

           movapd xmm0, [ecx + esi - 16];
           movapd xmm1, [edi + esi - 16];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movapd [eax], xmm0;
           movapd [eax + edx], xmm1;

           lea eax, [eax + 2*edx];

       jmp @forxloop;

       @loopend:

       sub esi, 128;

       jz @nextLine;

       @forxloop2:
           movapd xmm0, [ecx + esi];
           movapd xmm1, [edi + esi];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movapd [eax], xmm0;
           movapd [eax + edx], xmm1;

           lea eax, [eax + 2*edx];
       add esi, 16;
       jnz @forxloop2;

       @nextLine:

       // handle last element differently
       movsd xmm0, [ecx];
       movsd xmm1, [edi];

       movlhps xmm0, xmm1;
       movapd [eax], xmm0;

       // increment pointers
       pop eax;
       mov esi, LineWidth;
       
       add eax, 16;
       lea ecx, [ecx + 2*esi];

   sub height, 2;
   jnz @foryloop;

   // handle last line differently
   mov esi, iters;
   @forxloop3:
       movapd xmm0, [ecx + esi];

       movhlps xmm1, xmm0;
       movsd [eax], xmm0;
       movsd [eax + edx], xmm1;

       lea eax, [eax + 2*edx];
   add esi, 16;
   jnz @forxloop3;

   // last element last line
   movsd xmm0, [ecx];
   movsd [eax], xmm0;

   pop ebx;
   pop edi;
   pop esi;
end;

procedure ASMMatrixTransposeUnAlignedOddWOddH(dest : PDouble; const destLineWidth : NativeInt; mt : PDouble; const LineWidth : NativeInt; width : NativeInt; height : NativeInt);
var iters : NativeInt;
asm
   push esi;
   push edi;
   push ebx;

   // init
   // iters := -width*sizeof(double); 
   mov esi, width;
   dec esi;
   imul esi, -8;
   mov iters, esi;

   sub ecx, esi;
   sub height, 1;

   @foryloop:
       push eax;

       mov edi, ecx;
       add edi, LineWidth;

       // unrolled loop
       mov esi, iters;
       @forxloop:
           add esi, 128;
           jg @loopend;

           // prefetch [ecx + esi];
           // prefetch [edi + esi];

           movupd xmm0, [ecx + esi - 128];
           movupd xmm1, [edi + esi - 128];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movupd [eax], xmm0;
           movupd [eax + edx], xmm1;

           lea eax, [eax + 2*edx];

           movupd xmm0, [ecx + esi - 112];
           movupd xmm1, [edi + esi - 112];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movupd [eax], xmm0;
           movupd [eax + edx], xmm1;

           lea eax, [eax + 2*edx];

           movupd xmm0, [ecx + esi - 96];
           movupd xmm1, [edi + esi - 96];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movupd [eax], xmm0;
           movupd [eax + edx], xmm1;

           lea eax, [eax + 2*edx];

           movupd xmm0, [ecx + esi - 80];
           movupd xmm1, [edi + esi - 80];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movupd [eax], xmm0;
           movupd [eax + edx], xmm1;

           lea eax, [eax + 2*edx];

           movupd xmm0, [ecx + esi - 64];
           movupd xmm1, [edi + esi - 64];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movupd [eax], xmm0;
           movupd [eax + edx], xmm1;

           lea eax, [eax + 2*edx];

           movupd xmm0, [ecx + esi - 48];
           movupd xmm1, [edi + esi - 48];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movupd [eax], xmm0;
           movupd [eax + edx], xmm1;

           lea eax, [eax + 2*edx];

           movupd xmm0, [ecx + esi - 32];
           movupd xmm1, [edi + esi - 32];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movupd [eax], xmm0;
           movupd [eax + edx], xmm1;

           lea eax, [eax + 2*edx];

           movupd xmm0, [ecx + esi - 16];
           movupd xmm1, [edi + esi - 16];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movupd [eax], xmm0;
           movupd [eax + edx], xmm1;

           lea eax, [eax + 2*edx];

       jmp @forxloop;

       @loopend:

       sub esi, 128;

       jz @nextLine;

       @forxloop2:
           movupd xmm0, [ecx + esi];
           movupd xmm1, [edi + esi];

           movapd xmm2, xmm0;
           movlhps xmm0, xmm1;
           movhlps xmm1, xmm2;

           movupd [eax], xmm0;
           movupd [eax + edx], xmm1;

           lea eax, [eax + 2*edx];
       add esi, 16;
       jnz @forxloop2;

       @nextLine:

       // handle last element differently
       movsd xmm0, [ecx];
       movsd xmm1, [edi];

       movlhps xmm0, xmm1;
       movupd [eax], xmm0;

       // increment pointers
       pop eax;
       mov esi, LineWidth;
       
       add eax, 16;
       lea ecx, [ecx + 2*esi];

   sub height, 2;
   jnz @foryloop;

   // handle last line differently
   mov esi, iters;
   @forxloop3:
       movupd xmm0, [ecx + esi];

       movhlps xmm1, xmm0;
       movsd [eax], xmm0;
       movsd [eax + edx], xmm1;

       lea eax, [eax + 2*edx];
   add esi, 16;
   jnz @forxloop3;

   // last element last line
   movsd xmm0, [ecx];
   movsd [eax], xmm0;

   pop ebx;
   pop edi;
   pop esi;
end;

// Inplace Trasnposition of an N x N matrix
procedure ASMMatrixTransposeInplace(mt : PDouble; const LineWidth : NativeInt; N : NativeInt);
// eax = MT, edx = LineWidth, ecx = N
var aN : NativeInt;
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
         movsd xmm0, [eax + esi];
         movsd xmm1, [ecx];

         movsd [eax + esi], xmm1;
         movsd [ecx], xmm0;

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

   pop esi;
   pop edi;
   pop ebx;
end;

{$ENDIF}

end.
