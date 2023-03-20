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


unit ASMMatrixElementwiseMultOperations;

interface

{$I 'mrMath_CPU.inc'}

{$IFNDEF x64}

uses MatrixConst;

procedure ASMMatrixElemMultAlignedEvenW(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width : NativeInt; height : NativeInt; const LineWidth1, LineWidth2 : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixElemMultUnAlignedEvenW(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width : NativeInt; height : NativeInt; const LineWidth1, LineWidth2 : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

procedure ASMMatrixElemMultAlignedOddW(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width : NativeInt; height : NativeInt; const LineWidth1, LineWidth2 : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixElemMultUnAlignedOddW(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width : NativeInt; height : NativeInt; const LineWidth1, LineWidth2 : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

procedure ASMMatrixElemDivAlignedEvenW(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width : NativeInt; height : NativeInt; const LineWidth1, LineWidth2 : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixElemDivUnAlignedEvenW(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width : NativeInt; height : NativeInt; const LineWidth1, LineWidth2 : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

procedure ASMMatrixElemDivAlignedOddW(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width : NativeInt; height : NativeInt; const LineWidth1, LineWidth2 : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixElemDivUnAlignedOddW(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width : NativeInt; height : NativeInt; const LineWidth1, LineWidth2 : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}


{$ENDIF}

implementation

{$IFNDEF x64}

procedure ASMMatrixElemMultAlignedEvenW(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width : NativeInt; height : NativeInt; const LineWidth1, LineWidth2 : NativeInt);
asm
   push ebx;
   push esi;
   push edi;

   mov ebx, width;
   imul ebx, -8;
        
   // helper registers for the mt1, mt2 and dest pointers
   sub ecx, ebx;
   mov edi, mt2;
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
           // prefetch [ecx + esi];
           // prefetch [edi + esi];

           // addition:
           movapd xmm0, [ecx + esi - 128];
           mulpd xmm0, [edi + esi - 128];
           movapd [eax + esi - 128], xmm0;

           movapd xmm1, [ecx + esi - 112];
           mulpd xmm1, [edi + esi - 112];
           movapd [eax + esi - 112], xmm1;

           movapd xmm2, [ecx + esi - 96];
           mulpd xmm2, [edi + esi - 96];
           movapd [eax + esi - 96], xmm2;

           movapd xmm3, [ecx + esi - 80];
           mulpd xmm3, [edi + esi - 80];
           movapd [eax + esi - 80], xmm3;

           movapd xmm4, [ecx + esi - 64];
           mulpd xmm4, [edi + esi - 64];
           movapd [eax + esi - 64], xmm4;

           movapd xmm5, [ecx + esi - 48];
           mulpd xmm5, [edi + esi - 48];
           movapd [eax + esi - 48], xmm5;

           movapd xmm6, [ecx + esi - 32];
           mulpd xmm6, [edi + esi - 32];
           movapd [eax + esi - 32], xmm6;

           movapd xmm7, [ecx + esi - 16];
           mulpd xmm7, [edi + esi - 16];
           movapd [eax + esi - 16], xmm7;
       jmp @addforxloop

       @loopEnd:

       sub esi, 128;

       jz @nextLine;

       @addforxloop2:
           movapd xmm0, [ecx + esi];
           mulpd xmm0, [edi + esi];

           movapd [eax + esi], xmm0;
       add esi, 16;
       jnz @addforxloop2;

       @nextLine:

       // next line:
       add ecx, lineWidth1;
       add edi, lineWidth2;
       add eax, edx;

   // loop y end
   dec Height;
   jnz @@addforyloop;

   pop edi;
   pop esi;
   pop ebx;
end;

procedure ASMMatrixElemMultUnAlignedEvenW(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width : NativeInt; height : NativeInt; const LineWidth1, LineWidth2 : NativeInt);
asm
   push ebx;
   push esi;
   push edi;

   mov ebx, width;
   imul ebx, -8;
        
   // helper registers for the mt1, mt2 and dest pointers
   sub ecx, ebx;
   mov edi, mt2;
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

           // addition:
           movupd xmm0, [ecx + esi - 128];
           movupd xmm1, [edi + esi - 128];
           mulpd xmm0, xmm1;

           movupd [eax + esi - 128], xmm0;

           movupd xmm0, [ecx + esi - 112];
           movupd xmm1, [edi + esi - 112];
           mulpd xmm0, xmm1;

           movupd [eax + esi - 112], xmm0;

           movupd xmm0, [ecx + esi - 96];
           movupd xmm1, [edi + esi - 96];
           mulpd xmm0, xmm1;

           movupd [eax + esi - 96], xmm0;

           movupd xmm0, [ecx + esi - 80];
           movupd xmm1, [edi + esi - 80];
           mulpd xmm0, xmm1;

           movupd [eax + esi - 80], xmm0;

           movupd xmm0, [ecx + esi - 64];
           movupd xmm1, [edi + esi - 64];
           mulpd xmm0, xmm1;

           movupd [eax + esi - 64], xmm0;

           movupd xmm0, [ecx + esi - 48];
           movupd xmm1, [edi + esi - 48];
           mulpd xmm0, xmm1;

           movupd [eax + esi - 48], xmm0;

           movupd xmm0, [ecx + esi - 32];
           movupd xmm1, [edi + esi - 32];
           mulpd xmm0, xmm1;

           movupd [eax + esi - 32], xmm0;

           movupd xmm0, [ecx + esi - 16];
           movupd xmm1, [edi + esi - 16];
           mulpd xmm0, xmm1;

           movupd [eax + esi - 16], xmm0;
       // loop x end
       jmp @addforxloop

       @loopEnd:

       sub esi, 128;

       jz @nextLine;

       @addforxloop2:
           movupd xmm0, [ecx + esi];
           movupd xmm1, [edi + esi];
           mulpd xmm0, xmm1;

           movupd [eax + esi], xmm0;
       add esi, 16;
       jnz @addforxloop2;

       @nextLine:

       // next line:
       add ecx, lineWidth1;
       add edi, lineWidth2;
       add eax, edx;

   // loop y end
   dec Height;
   jnz @@addforyloop;

   pop edi;
   pop esi;
   pop ebx;
end;

procedure ASMMatrixElemMultAlignedOddW(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width : NativeInt; height : NativeInt; const LineWidth1, LineWidth2 : NativeInt);
asm
   push ebx;
   push esi;
   push edi;
        
   mov ebx, width;
   dec ebx;
   imul ebx, -8;
        
   // helper registers for the mt1, mt2 and dest pointers
   sub ecx, ebx;
   mov edi, mt2;
   sub edi, ebx;
   sub eax, ebx;

   // for y := 0 to height - 1:
   @@addforyloop:
       // for x := 0 to w - 1;
       mov esi, ebx;
       @addforxloop:
           add esi, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [ecx + esi];
           // prefetch [edi + esi];

           // mult:
           movapd xmm0, [ecx + esi - 128];
           mulpd xmm0, [edi + esi - 128];

           movapd [eax + esi - 128], xmm0;

           movapd xmm1, [ecx + esi - 112];
           mulpd xmm1, [edi + esi - 112];

           movapd [eax + esi - 112], xmm1;

           movapd xmm2, [ecx + esi - 96];
           mulpd xmm2, [edi + esi - 96];

           movapd [eax + esi - 96], xmm2;

           movapd xmm3, [ecx + esi - 80];
           mulpd xmm3, [edi + esi - 80];

           movapd [eax + esi - 80], xmm3;

           movapd xmm4, [ecx + esi - 64];
           mulpd xmm4, [edi + esi - 64];

           movapd [eax + esi - 64], xmm4;

           movapd xmm5, [ecx + esi - 48];
           mulpd xmm5, [edi + esi - 48];

           movapd [eax + esi - 48], xmm5;

           movapd xmm6, [ecx + esi - 32];
           mulpd xmm6, [edi + esi - 32];

           movapd [eax + esi - 32], xmm6;

           movapd xmm7, [ecx + esi - 16];
           mulpd xmm7, [edi + esi - 16];

           movapd [eax + esi - 16], xmm7;
       jmp @addforxloop

       @loopEnd:

       sub esi, 128;

       jz @nextLine;

       @addforxloop2:
           movapd xmm0, [ecx + esi];
           mulpd xmm0, [edi + esi];

           movapd [eax + esi], xmm0;
       add esi, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last column:
       movsd xmm0, [ecx];
       mulsd xmm0, [edi];

       movsd [eax], xmm0;

       // next line:
       add ecx, lineWidth1;
       add edi, lineWidth2;
       add eax, edx;

   // loop y end
   dec Height;
   jnz @@addforyloop;

   pop edi;
   pop esi;
   pop ebx;
end;

procedure ASMMatrixElemMultUnAlignedOddW(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width : NativeInt; height : NativeInt; const LineWidth1, LineWidth2 : NativeInt);
asm
   push ebx;
   push esi;
   push edi;
        
   mov ebx, width;
   dec ebx;
   imul ebx, -8;
        
   // helper registers for the mt1, mt2 and dest pointers
   sub ecx, ebx;
   mov edi, mt2;
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

           // mult:
           movupd xmm0, [ecx + esi - 128];
           movupd xmm1, [edi + esi - 128];
           mulpd xmm0, xmm1;

           movupd [eax + esi - 128], xmm0;

           movupd xmm0, [ecx + esi - 112];
           movupd xmm1, [edi + esi - 112];
           mulpd xmm0, xmm1;

           movupd [eax + esi - 112], xmm0;

           movupd xmm0, [ecx + esi - 96];
           movupd xmm1, [edi + esi - 96];
           mulpd xmm0, xmm1;

           movupd [eax + esi - 96], xmm0;

           movupd xmm0, [ecx + esi - 80];
           movupd xmm1, [edi + esi - 80];
           mulpd xmm0, xmm1;

           movupd [eax + esi - 80], xmm0;

           movupd xmm0, [ecx + esi - 64];
           movupd xmm1, [edi + esi - 64];
           mulpd xmm0, xmm1;

           movupd [eax + esi - 64], xmm0;

           movupd xmm0, [ecx + esi - 48];
           movupd xmm1, [edi + esi - 48];
           mulpd xmm0, xmm1;

           movupd [eax + esi - 48], xmm0;

           movupd xmm0, [ecx + esi - 32];
           movupd xmm1, [edi + esi - 32];
           mulpd xmm0, xmm1;

           movupd [eax + esi - 32], xmm0;

           movupd xmm0, [ecx + esi - 16];
           movupd xmm1, [edi + esi - 16];
           mulpd xmm0, xmm1;

           movupd [eax + esi - 16], xmm0;
       // loop x end
       jmp @addforxloop

       @loopEnd:

       sub esi, 128;

       jz @nextLine;

       @addforxloop2:
           movupd xmm0, [ecx + esi];
           movupd xmm1, [edi + esi];
           mulpd xmm0, xmm1;

           movupd [eax + esi], xmm0;
       add esi, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last column:
       movsd xmm0, [ecx];
       movsd xmm1, [edi];
       mulsd xmm0, xmm1;

       movsd [eax], xmm0;

       // next line:
       add ecx, lineWidth1;
       add edi, lineWidth2;
       add eax, edx;

   // loop y end
   dec Height;
   jnz @@addforyloop;

   pop edi;
   pop esi;
   pop ebx;
end;

// ##############################################################
// #### Elementwise divide of matrix 1 and matrix 2
// ##############################################################

procedure ASMMatrixElemDivAlignedEvenW(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width : NativeInt; height : NativeInt; const LineWidth1, LineWidth2 : NativeInt);
asm
   push ebx;
   push esi;
   push edi;

   mov ebx, width;
   imul ebx, -8;
        
   // helper registers for the mt1, mt2 and dest pointers
   sub ecx, ebx;
   mov edi, mt2;
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
           // prefetch [ecx + esi];
           // prefetch [edi + esi];

           // addition:
           movapd xmm0, [ecx + esi - 128];
           divpd xmm0, [edi + esi - 128];

           movapd [eax + esi - 128], xmm0;

           movapd xmm1, [ecx + esi - 112];
           divpd xmm1, [edi + esi - 112];

           movapd [eax + esi - 112], xmm1;

           movapd xmm2, [ecx + esi - 96];
           divpd xmm2, [edi + esi - 96];

           movapd [eax + esi - 96], xmm2;

           movapd xmm3, [ecx + esi - 80];
           divpd xmm3, [edi + esi - 80];

           movapd [eax + esi - 80], xmm3;

           movapd xmm4, [ecx + esi - 64];
           divpd xmm4, [edi + esi - 64];

           movapd [eax + esi - 64], xmm4;

           movapd xmm5, [ecx + esi - 48];
           divpd xmm5, [edi + esi - 48];

           movapd [eax + esi - 48], xmm5;

           movapd xmm6, [ecx + esi - 32];
           divpd xmm6, [edi + esi - 32];

           movapd [eax + esi - 32], xmm6;

           movapd xmm7, [ecx + esi - 16];
           divpd xmm7, [edi + esi - 16];

           movapd [eax + esi - 16], xmm7;
       jmp @addforxloop

       @loopEnd:

       sub esi, 128;

       jz @nextLine;

       @addforxloop2:
           movapd xmm0, [ecx + esi];
           divpd xmm0, [edi + esi];

           movapd [eax + esi], xmm0;
       add esi, 16;
       jnz @addforxloop2;

       @nextLine:

       // next line:
       add ecx, lineWidth1;
       add edi, lineWidth2;
       add eax, edx;

   // loop y end
   dec Height;
   jnz @@addforyloop;

   pop edi;
   pop esi;
   pop ebx;
end;

procedure ASMMatrixElemDivUnAlignedEvenW(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width : NativeInt; height : NativeInt; const LineWidth1, LineWidth2 : NativeInt);
asm
   push ebx;
   push esi;
   push edi;

   mov ebx, width;
   imul ebx, -8;
        
   // helper registers for the mt1, mt2 and dest pointers
   sub ecx, ebx;
   mov edi, mt2;
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

           // addition:
           movupd xmm0, [ecx + esi - 128];
           movupd xmm1, [edi + esi - 128];
           divpd xmm0, xmm1;

           movupd [eax + esi - 128], xmm0;

           movupd xmm0, [ecx + esi - 112];
           movupd xmm1, [edi + esi - 112];
           divpd xmm0, xmm1;

           movupd [eax + esi - 112], xmm0;

           movupd xmm0, [ecx + esi - 96];
           movupd xmm1, [edi + esi - 96];
           divpd xmm0, xmm1;

           movupd [eax + esi - 96], xmm0;

           movupd xmm0, [ecx + esi - 80];
           movupd xmm1, [edi + esi - 80];
           divpd xmm0, xmm1;

           movupd [eax + esi - 80], xmm0;

           movupd xmm0, [ecx + esi - 64];
           movupd xmm1, [edi + esi - 64];
           divpd xmm0, xmm1;

           movupd [eax + esi - 64], xmm0;

           movupd xmm0, [ecx + esi - 48];
           movupd xmm1, [edi + esi - 48];
           divpd xmm0, xmm1;

           movupd [eax + esi - 48], xmm0;

           movupd xmm0, [ecx + esi - 32];
           movupd xmm1, [edi + esi - 32];
           divpd xmm0, xmm1;

           movupd [eax + esi - 32], xmm0;

           movupd xmm0, [ecx + esi - 16];
           movupd xmm1, [edi + esi - 16];
           divpd xmm0, xmm1;

           movupd [eax + esi - 16], xmm0;
       // loop x end
       jmp @addforxloop

       @loopEnd:

       sub esi, 128;

       jz @nextLine;

       @addforxloop2:
           movupd xmm0, [ecx + esi];
           movupd xmm1, [edi + esi];
           divpd xmm0, xmm1;

           movupd [eax + esi], xmm0;
       add esi, 16;
       jnz @addforxloop2;

       @nextLine:

       // next line:
       add ecx, lineWidth1;
       add edi, lineWidth2;
       add eax, edx;

   // loop y end
   dec Height;
   jnz @@addforyloop;

   pop edi;
   pop esi;
   pop ebx;
end;

procedure ASMMatrixElemDivAlignedOddW(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width : NativeInt; height : NativeInt; const LineWidth1, LineWidth2 : NativeInt);
asm
   push ebx;
   push esi;
   push edi;

   mov ebx, width;
   dec ebx;
   imul ebx, -8;
        
   // helper registers for the mt1, mt2 and dest pointers
   sub ecx, ebx;
   mov edi, mt2;
   sub edi, ebx;
   sub eax, ebx;

   // for y := 0 to height - 1:
   @@addforyloop:
       // for x := 0 to w - 1;
       mov esi, ebx;
       @addforxloop:
           add esi, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [ecx + esi];
           // prefetch [edi + esi];

           // mult:
           movapd xmm0, [ecx + esi - 128];
           divpd xmm0, [edi + esi - 128];

           movapd [eax + esi - 128], xmm0;

           movapd xmm1, [ecx + esi - 112];
           divpd xmm1, [edi + esi - 112];

           movapd [eax + esi - 112], xmm1;

           movapd xmm2, [ecx + esi - 96];
           divpd xmm2, [edi + esi - 96];

           movapd [eax + esi - 96], xmm2;

           movapd xmm3, [ecx + esi - 80];
           divpd xmm3, [edi + esi - 80];

           movapd [eax + esi - 80], xmm3;

           movapd xmm4, [ecx + esi - 64];
           divpd xmm4, [edi + esi - 64];

           movapd [eax + esi - 64], xmm4;

           movapd xmm5, [ecx + esi - 48];
           divpd xmm5, [edi + esi - 48];

           movapd [eax + esi - 48], xmm5;

           movapd xmm6, [ecx + esi - 32];
           divpd xmm6, [edi + esi - 32];

           movapd [eax + esi - 32], xmm6;

           movapd xmm7, [ecx + esi - 16];
           divpd xmm7, [edi + esi - 16];

           movapd [eax + esi - 16], xmm7;
       jmp @addforxloop

       @loopEnd:

       sub esi, 128;

       jz @nextLine;

       @addforxloop2:
           movapd xmm0, [ecx + esi];
           divpd xmm0, [edi + esi];

           movapd [eax + esi], xmm0;
       add esi, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last column:
       movsd xmm0, [ecx];
       divsd xmm0, [edi];

       movsd [eax], xmm0;

       // next line:
       add ecx, lineWidth1;
       add edi, lineWidth2;
       add eax, edx;

   // loop y end
   dec Height;
   jnz @@addforyloop;

   pop edi;
   pop esi;
   pop ebx;
end;

procedure ASMMatrixElemDivUnAlignedOddW(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width : NativeInt; height : NativeInt; const LineWidth1, LineWidth2 : NativeInt);
asm
   push ebx;
   push esi;
   push edi;

   mov ebx, width;
   dec ebx;
   imul ebx, -8;
        
   // helper registers for the mt1, mt2 and dest pointers
   sub ecx, ebx;
   mov edi, mt2;
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

           // mult:
           movupd xmm0, [ecx + esi - 128];
           movupd xmm1, [edi + esi - 128];
           divpd xmm0, xmm1;

           movupd [eax + esi - 128], xmm0;

           movupd xmm0, [ecx + esi - 112];
           movupd xmm1, [edi + esi - 112];
           divpd xmm0, xmm1;

           movupd [eax + esi - 112], xmm0;

           movupd xmm0, [ecx + esi - 96];
           movupd xmm1, [edi + esi - 96];
           divpd xmm0, xmm1;

           movupd [eax + esi - 96], xmm0;

           movupd xmm0, [ecx + esi - 80];
           movupd xmm1, [edi + esi - 80];
           divpd xmm0, xmm1;

           movupd [eax + esi - 80], xmm0;

           movupd xmm0, [ecx + esi - 64];
           movupd xmm1, [edi + esi - 64];
           divpd xmm0, xmm1;

           movupd [eax + esi - 64], xmm0;

           movupd xmm0, [ecx + esi - 48];
           movupd xmm1, [edi + esi - 48];
           divpd xmm0, xmm1;

           movupd [eax + esi - 48], xmm0;

           movupd xmm0, [ecx + esi - 32];
           movupd xmm1, [edi + esi - 32];
           divpd xmm0, xmm1;

           movupd [eax + esi - 32], xmm0;

           movupd xmm0, [ecx + esi - 16];
           movupd xmm1, [edi + esi - 16];
           divpd xmm0, xmm1;

           movupd [eax + esi - 16], xmm0;
       // loop x end
       jmp @addforxloop

       @loopEnd:

       sub esi, 128;

       jz @nextLine;

       @addforxloop2:
           movupd xmm0, [ecx + esi];
           movupd xmm1, [edi + esi];
           divpd xmm0, xmm1;

           movupd [eax + esi], xmm0;
       add esi, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last column:
       movsd xmm0, [ecx];
       movsd xmm1, [edi];
       divsd xmm0, xmm1;

       movsd [eax], xmm0;

       // next line:
       add ecx, lineWidth1;
       add edi, lineWidth2;
       add eax, edx;

   // loop y end
   dec Height;
   jnz @@addforyloop;

   pop edi;
   pop esi;
   pop ebx;
end;

{$ENDIF}

end.
