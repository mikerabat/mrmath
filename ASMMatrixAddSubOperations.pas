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


unit ASMMatrixAddSubOperations;

// ############################################################
// ##### Matrix addition/subtraction assembler optimized:
// ############################################################

interface

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}
{$IFNDEF x64}

uses MatrixConst;

procedure ASMMatrixAddAlignedEvenW(dest : PDouble; const destLinewidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, Linewidth2 : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixAddUnAlignedEvenW(dest : PDouble; const destLinewidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, Linewidth2 : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

procedure ASMMatrixAddAlignedOddW(dest : PDouble; const destLinewidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, Linewidth2 : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixAddUnAlignedOddW(dest : PDouble; const destLinewidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, Linewidth2 : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

procedure ASMMatrixSubAlignedEvenW(dest : PDouble; const destLinewidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, Linewidth2 : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixSubUnAlignedEvenW(dest : PDouble; const destLinewidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, Linewidth2 : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

procedure ASMMatrixSubAlignedOddW(dest : PDouble; const destLinewidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, Linewidth2 : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixSubUnAlignedOddW(dest : PDouble; const destLinewidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, Linewidth2 : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

procedure ASMMatrixSubT(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; LineWidthB : TASMNativeInt; width, height : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

procedure ASMMatrixSubVecAlignedVecRow(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; incX : TASMNativeInt; width, Height : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixSubVecAlignedRow(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; incX : TASMNativeInt; width, Height : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixSubVecAlignedCol(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; incX : TASMNativeInt; width, Height : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

procedure ASMMatrixSubVecUnalignedVecRow(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; incX : TASMNativeInt; width, Height : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixSubVecUnalignedRow(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; incX : TASMNativeInt; width, Height : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixSubVecUnalignedCol(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; incX : TASMNativeInt; width, Height : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

procedure ASMMatrixAddVecAlignedVecRow(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; incX : TASMNativeInt; width, Height : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixAddVecAlignedRow(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; incX : TASMNativeInt; width, Height : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixAddVecAlignedCol(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; incX : TASMNativeInt; width, Height : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

procedure ASMMatrixAddVecUnalignedVecRow(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; incX : TASMNativeInt; width, Height : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixAddVecUnalignedRow(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; incX : TASMNativeInt; width, Height : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixAddVecUnalignedCol(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; incX : TASMNativeInt; width, Height : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

procedure ASMVecAddAlignedSeq( X : PDouble; y : PDouble; N : TASMNativeInt; alpha : double ); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMVecAddUnAlignedSeq( X : PDouble; y : PDouble; N : TASMNativeInt; alpha : double ); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

procedure ASMVecAddNonSeq( X : PDouble; y : PDouble; N : TASMNativeInt; incX, incY : TASMNativeInt; alpha : double ); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}


{$ENDIF}

implementation

{$IFNDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$S-} {$ENDIF}

procedure ASMMatrixAddAlignedEvenW(dest : PDouble; const destLinewidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, Linewidth2 : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
// eax = dest; edx = destLineWidth; mt1 = ecx
asm
   push esi;
   push edi;
   push ebx;

   // iters = -width*sizeof(double)
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
           addpd xmm0, [edi + esi - 128];

           movapd [eax + esi - 128], xmm0;

           movapd xmm1, [ecx + esi - 112];
           addpd xmm1, [edi + esi - 112];

           movapd [eax + esi - 112], xmm1;

           movapd xmm2, [ecx + esi - 96];
           addpd xmm2, [edi + esi - 96];

           movapd [eax + esi - 96], xmm2;

           movapd xmm3, [ecx + esi - 80];
           addpd xmm3, [edi + esi - 80];

           movapd [eax + esi - 80], xmm3;

           movapd xmm4, [ecx + esi - 64];
           addpd xmm4, [edi + esi - 64];

           movapd [eax + esi - 64], xmm4;

           movapd xmm5, [ecx + esi - 48];
           addpd xmm5, [edi + esi - 48];

           movapd [eax + esi - 48], xmm5;

           movapd xmm6, [ecx + esi - 32];
           addpd xmm6, [edi + esi - 32];

           movapd [eax + esi - 32], xmm6;

           movapd xmm7, [ecx + esi - 16];
           addpd xmm7, [edi + esi - 16];

           movapd [eax + esi - 16], xmm7;
       jmp @addforxloop

       @loopEnd:

       sub esi, 128;

       jz @nextLine;

       @addforxloop2:
           movapd xmm0, [ecx + esi];
           addpd xmm0, [edi + esi];

           movapd [eax + esi], xmm0;
       add esi, 16;
       jnz @addforxloop2;

       @nextLine:

       // next line:
       add ecx, lineWidth1;
       add edi, lineWidth2;
       add eax, edx;

   // loop y end
   dec height;
   jnz @@addforyloop;

   pop ebx;
   pop edi;
   pop esi;
end;

procedure ASMMatrixAddUnAlignedEvenW(dest : PDouble; const destLinewidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, Linewidth2 : TASMNativeInt);
asm
   push esi;
   push edi;
   push ebx;
        
   // iters = -width*sizeof(double)
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
           addpd xmm0, xmm1;

           movupd [eax + esi - 128], xmm0;

           movupd xmm0, [ecx + esi - 112];
           movupd xmm1, [edi + esi - 112];
           addpd xmm0, xmm1;

           movupd [eax + esi - 112], xmm0;

           movupd xmm0, [ecx + esi - 96];
           movupd xmm1, [edi + esi - 96];
           addpd xmm0, xmm1;

           movupd [eax + esi - 96], xmm0;

           movupd xmm0, [ecx + esi - 80];
           movupd xmm1, [edi + esi - 80];
           addpd xmm0, xmm1;

           movupd [eax + esi - 80], xmm0;

           movupd xmm0, [ecx + esi - 64];
           movupd xmm1, [edi + esi - 64];
           addpd xmm0, xmm1;

           movupd [eax + esi - 64], xmm0;

           movupd xmm0, [ecx + esi - 48];
           movupd xmm1, [edi + esi - 48];
           addpd xmm0, xmm1;

           movupd [eax + esi - 48], xmm0;

           movupd xmm0, [ecx + esi - 32];
           movupd xmm1, [edi + esi - 32];
           addpd xmm0, xmm1;

           movupd [eax + esi - 32], xmm0;

           movupd xmm0, [ecx + esi - 16];
           movupd xmm1, [edi + esi - 16];
           addpd xmm0, xmm1;

           movupd [eax + esi - 16], xmm0;
       // loop x end
       jmp @addforxloop

       @loopEnd:

       sub esi, 128;

       jz @nextLine;

       @addforxloop2:
           movupd xmm0, [ecx + esi];
           movupd xmm1, [edi + esi];
           addpd xmm0, xmm1;

           movupd [eax + esi], xmm0;
       add esi, 16;
       jnz @addforxloop2;

       @nextLine:

       // next line:
       add ecx, lineWidth1;
       add edi, lineWidth2;
       add eax, edx;

   // loop y end
   dec height;
   jnz @@addforyloop;

   pop ebx;
   pop edi;
   pop esi;
end;

procedure ASMMatrixAddAlignedOddW(dest : PDouble; const destLinewidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, Linewidth2 : TASMNativeInt);
asm
   push esi;
   push edi;
   push ebx;
        
        
   // iters = -(width-1)*sizeof(double)
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

           // addition:
           movapd xmm0, [ecx + esi - 128];
           addpd xmm0, [edi + esi - 128];

           movapd [eax + esi - 128], xmm0;

           movapd xmm1, [ecx + esi - 112];
           addpd xmm1, [edi + esi - 112];

           movapd [eax + esi - 112], xmm1;

           movapd xmm2, [ecx + esi - 96];
           addpd xmm2, [edi + esi - 96];

           movapd [eax + esi - 96], xmm2;

           movapd xmm3, [ecx + esi - 80];
           addpd xmm3, [edi + esi - 80];

           movapd [eax + esi - 80], xmm3;

           movapd xmm4, [ecx + esi - 64];
           addpd xmm4, [edi + esi - 64];

           movapd [eax + esi - 64], xmm4;

           movapd xmm5, [ecx + esi - 48];
           addpd xmm5, [edi + esi - 48];

           movapd [eax + esi - 48], xmm5;

           movapd xmm6, [ecx + esi - 32];
           addpd xmm6, [edi + esi - 32];

           movapd [eax + esi - 32], xmm6;

           movapd xmm7, [ecx + esi - 16];
           addpd xmm7, [edi + esi - 16];

           movapd [eax + esi - 16], xmm7;
       jmp @addforxloop

       @loopEnd:

       sub esi, 128;

       jz @nextLine;

       @addforxloop2:
           movapd xmm0, [ecx + esi];
           addpd xmm0, [edi + esi];

           movapd [eax + esi], xmm0;
       add esi, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last column:
       movsd xmm0, [ecx];
       addsd xmm0, [edi];

       movsd [eax], xmm0;

       // next line:
       add ecx, lineWidth1;
       add edi, lineWidth2;
       add eax, edx;

   // loop y end
   dec height;
   jnz @@addforyloop;

   pop ebx;
   pop edi;
   pop esi;
end;

procedure ASMMatrixAddUnAlignedOddW(dest : PDouble; const destLinewidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, Linewidth2 : TASMNativeInt);
asm
   push esi;
   push edi;
   push ebx;
        
        
   // iters = -(width-1)*sizeof(double)
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

           // addition:
           movupd xmm0, [ecx + esi - 128];
           movupd xmm1, [edi + esi - 128];
           addpd xmm0, xmm1;

           movupd [eax + esi - 128], xmm0;

           movupd xmm0, [ecx + esi - 112];
           movupd xmm1, [edi + esi - 112];
           addpd xmm0, xmm1;

           movupd [eax + esi - 112], xmm0;

           movupd xmm0, [ecx + esi - 96];
           movupd xmm1, [edi + esi - 96];
           addpd xmm0, xmm1;

           movupd [eax + esi - 96], xmm0;

           movupd xmm0, [ecx + esi - 80];
           movupd xmm1, [edi + esi - 80];
           addpd xmm0, xmm1;

           movupd [eax + esi - 80], xmm0;

           movupd xmm0, [ecx + esi - 64];
           movupd xmm1, [edi + esi - 64];
           addpd xmm0, xmm1;

           movupd [eax + esi - 64], xmm0;

           movupd xmm0, [ecx + esi - 48];
           movupd xmm1, [edi + esi - 48];
           addpd xmm0, xmm1;

           movupd [eax + esi - 48], xmm0;

           movupd xmm0, [ecx + esi - 32];
           movupd xmm1, [edi + esi - 32];
           addpd xmm0, xmm1;

           movupd [eax + esi - 32], xmm0;

           movupd xmm0, [ecx + esi - 16];
           movupd xmm1, [edi + esi - 16];
           addpd xmm0, xmm1;

           movupd [eax + esi - 16], xmm0;
       // loop x end
       jmp @addforxloop

       @loopEnd:

       sub esi, 128;

       jz @nextLine;

       @addforxloop2:
           movupd xmm0, [ecx + esi];
           movupd xmm1, [edi + esi];
           addpd xmm0, xmm1;

           movupd [eax + esi], xmm0;
       add esi, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last column:
       movsd xmm0, [ecx];
       addsd xmm0, [edi];

       movsd [eax], xmm0;

       // next line:
       add ecx, lineWidth1;
       add edi, lineWidth2;
       add eax, edx;

   // loop y end
   dec height;
   jnz @@addforyloop;

   pop ebx;
   pop edi;
   pop esi;
end;

procedure ASMMatrixSubAlignedEvenW(dest : PDouble; const destLinewidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, Linewidth2 : TASMNativeInt);
asm
   push esi;
   push edi;
   push ebx;
        
   // iters = -(width-1)*sizeof(double)
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

           // substraction:
           movapd xmm0, [ecx + esi - 128];
           subpd xmm0, [edi + esi - 128];

           movapd [eax + esi - 128], xmm0;

           movapd xmm1, [ecx + esi - 112];
           subpd xmm1, [edi + esi - 112];

           movapd [eax + esi - 112], xmm1;

           movapd xmm2, [ecx + esi - 96];
           subpd xmm2, [edi + esi - 96];

           movapd [eax + esi - 96], xmm2;

           movapd xmm3, [ecx + esi - 80];
           subpd xmm3, [edi + esi - 80];

           movapd [eax + esi - 80], xmm3;

           movapd xmm4, [ecx + esi - 64];
           subpd xmm4, [edi + esi - 64];

           movapd [eax + esi - 64], xmm4;

           movapd xmm5, [ecx + esi - 48];
           subpd xmm5, [edi + esi - 48];

           movapd [eax + esi - 48], xmm5;

           movapd xmm6, [ecx + esi - 32];
           subpd xmm6, [edi + esi - 32];

           movapd [eax + esi - 32], xmm6;

           movapd xmm7, [ecx + esi - 16];
           subpd xmm7, [edi + esi - 16];

           movapd [eax + esi - 16], xmm7;
       jmp @addforxloop

       @loopEnd:

       sub esi, 128;

       jz @nextLine;

       @addforxloop2:
           movapd xmm0, [ecx + esi];
           subpd xmm0, [edi + esi];

           movapd [eax + esi], xmm0;
       add esi, 16;
       jnz @addforxloop2;

       @nextLine:

       // next line:
       add ecx, lineWidth1;
       add edi, lineWidth2;
       add eax, edx;

   // loop y end
   dec height;
   jnz @@addforyloop;

   pop ebx;
   pop edi;
   pop esi;
end;

procedure ASMMatrixSubUnAlignedEvenW(dest : PDouble; const destLinewidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, Linewidth2 : TASMNativeInt);
asm
   push esi;
   push edi;
   push ebx;
        
        
   // iters = -(width-1)*sizeof(double)
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

           // substraction:
           movupd xmm0, [ecx + esi - 128];
           movupd xmm1, [edi + esi - 128];
           subpd xmm0, xmm1;

           movupd [eax + esi - 128], xmm0;

           movupd xmm0, [ecx + esi - 112];
           movupd xmm1, [edi + esi - 112];
           subpd xmm0, xmm1;

           movupd [eax + esi - 112], xmm0;

           movupd xmm0, [ecx + esi - 96];
           movupd xmm1, [edi + esi - 96];
           subpd xmm0, xmm1;

           movupd [eax + esi - 96], xmm0;

           movupd xmm0, [ecx + esi - 80];
           movupd xmm1, [edi + esi - 80];
           subpd xmm0, xmm1;

           movupd [eax + esi - 80], xmm0;

           movupd xmm0, [ecx + esi - 64];
           movupd xmm1, [edi + esi - 64];
           subpd xmm0, xmm1;

           movupd [eax + esi - 64], xmm0;

           movupd xmm0, [ecx + esi - 48];
           movupd xmm1, [edi + esi - 48];
           subpd xmm0, xmm1;

           movupd [eax + esi - 48], xmm0;

           movupd xmm0, [ecx + esi - 32];
           movupd xmm1, [edi + esi - 32];
           subpd xmm0, xmm1;

           movupd [eax + esi - 32], xmm0;

           movupd xmm0, [ecx + esi - 16];
           movupd xmm1, [edi + esi - 16];
           subpd xmm0, xmm1;

           movupd [eax + esi - 16], xmm0;
       // loop x end
       jmp @addforxloop

       @loopEnd:

       sub esi, 128;

       jz @nextLine;

       @addforxloop2:
           movupd xmm0, [ecx + esi];
           movupd xmm1, [edi + esi];
           subpd xmm0, xmm1;

           movupd [eax + esi], xmm0;
       add esi, 16;
       jnz @addforxloop2;

       @nextLine:

       // next line:
       add ecx, lineWidth1;
       add edi, lineWidth2;
       add eax, edx;

   // loop y end
   dec height;
   jnz @@addforyloop;

   pop ebx;
   pop edi;
   pop esi;
end;

procedure ASMMatrixSubAlignedOddW(dest : PDouble; const destLinewidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, Linewidth2 : TASMNativeInt);
asm
   push esi;
   push edi;
   push ebx;
        
        
   // iters = -(width-1)*sizeof(double)
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

           // substraction:
           movapd xmm0, [ecx + esi - 128];
           subpd xmm0, [edi + esi - 128];

           movapd [eax + esi - 128], xmm0;

           movapd xmm1, [ecx + esi - 112];
           subpd xmm1, [edi + esi - 112];

           movapd [eax + esi - 112], xmm1;

           movapd xmm2, [ecx + esi - 96];
           subpd xmm2, [edi + esi - 96];

           movapd [eax + esi - 96], xmm2;

           movapd xmm3, [ecx + esi - 80];
           subpd xmm3, [edi + esi - 80];

           movapd [eax + esi - 80], xmm3;

           movapd xmm4, [ecx + esi - 64];
           subpd xmm4, [edi + esi - 64];

           movapd [eax + esi - 64], xmm4;

           movapd xmm5, [ecx + esi - 48];
           subpd xmm5, [edi + esi - 48];

           movapd [eax + esi - 48], xmm5;

           movapd xmm6, [ecx + esi - 32];
           subpd xmm6, [edi + esi - 32];

           movapd [eax + esi - 32], xmm6;

           movapd xmm7, [ecx + esi - 16];
           subpd xmm7, [edi + esi - 16];

           movapd [eax + esi - 16], xmm7;
       jmp @addforxloop

       @loopEnd:

       sub esi, 128;

       jz @nextLine;

       @addforxloop2:
           movapd xmm0, [ecx + esi];
           subpd xmm0, [edi + esi];

           movapd [eax + esi], xmm0;
       add esi, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last column:
       movsd xmm0, [ecx];
       subsd xmm0, [edi];

       movsd [eax], xmm0;

       // next line:
       add ecx, lineWidth1;
       add edi, lineWidth2;
       add eax, edx;

   // loop y end
   dec height;
   jnz @@addforyloop;

   pop ebx;
   pop edi;
   pop esi;
end;

procedure ASMMatrixSubUnAlignedOddW(dest : PDouble; const destLinewidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, Linewidth2 : TASMNativeInt);
asm
   push esi;
   push edi;
   push ebx;
        
   // iters = -(width-1)*sizeof(double)
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

           // substraction:
           movupd xmm0, [ecx + esi - 128];
           movupd xmm1, [edi + esi - 128];
           subpd xmm0, xmm1;

           movupd [eax + esi - 128], xmm0;

           movupd xmm0, [ecx + esi - 112];
           movupd xmm1, [edi + esi - 112];
           subpd xmm0, xmm1;

           movupd [eax + esi - 112], xmm0;

           movupd xmm0, [ecx + esi - 96];
           movupd xmm1, [edi + esi - 96];
           subpd xmm0, xmm1;

           movupd [eax + esi - 96], xmm0;

           movupd xmm0, [ecx + esi - 80];
           movupd xmm1, [edi + esi - 80];
           subpd xmm0, xmm1;

           movupd [eax + esi - 80], xmm0;

           movupd xmm0, [ecx + esi - 64];
           movupd xmm1, [edi + esi - 64];
           subpd xmm0, xmm1;

           movupd [eax + esi - 64], xmm0;

           movupd xmm0, [ecx + esi - 48];
           movupd xmm1, [edi + esi - 48];
           subpd xmm0, xmm1;

           movupd [eax + esi - 48], xmm0;

           movupd xmm0, [ecx + esi - 32];
           movupd xmm1, [edi + esi - 32];
           subpd xmm0, xmm1;

           movupd [eax + esi - 32], xmm0;

           movupd xmm0, [ecx + esi - 16];
           movupd xmm1, [edi + esi - 16];
           subpd xmm0, xmm1;

           movupd [eax + esi - 16], xmm0;
       // loop x end
       jmp @addforxloop

       @loopEnd:

       sub esi, 128;

       jz @nextLine;

       @addforxloop2:
           movupd xmm0, [ecx + esi];
           movupd xmm1, [edi + esi];
           subpd xmm0, xmm1;

           movupd [eax + esi], xmm0;
       add esi, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last column:
       movsd xmm0, [ecx];
       subsd xmm0, [edi];

       movsd [eax], xmm0;

       // next line:
       add ecx, lineWidth1;
       add edi, lineWidth2;
       add eax, edx;

   // loop y end
   dec height;
   jnz @@addforyloop;

   pop ebx;
   pop edi;
   pop esi;
end;

// ##################################################
// #### Matrix substraction transposed
// ##################################################

procedure ASMMatrixSubT(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; LineWidthB : TASMNativeInt; width, height : TASMNativeInt);
// eax=A, edx = LineWidthA, ecx = B
var iter : TASMNativeInt;
asm
   push ebx;
   push edi;
   push esi;

   // iter := -width*sizeof(double)
   mov ebx, width;
   imul ebx, -8;
   mov iter, ebx;
   
   sub eax, ebx;
   mov esi, LineWidthB;

   // for y := 0 to height - 1
   @@foryloop:
      mov edi, ecx;
      mov ebx, iter;

      // for x := 0 to width - 1
      @@forxloop:
         movsd xmm0, [eax + ebx];
         movsd xmm1, [edi];

         subsd xmm0, xmm1;
         movsd [eax + ebx], xmm0;

         add edi, esi;
      add ebx, 8;
      jnz @@forxloop;

      add eax, edx;
      add ecx, 8;
   dec height;
   jnz @@foryloop;

   pop esi;
   pop edi;
   pop ebx;
end;

// ########################################################
// #### Matrix add, sub to vector operations
// ########################################################

procedure ASMMatrixSubVecAlignedVecRow(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; incX : TASMNativeInt; width, Height : TASMNativeInt);
// eax = A, edx = LineWidthA, ecx = B
asm
   push ebx;
   push edi;
   push esi;

   mov ebx, width;
   imul ebx, -8;

   sub eax, ebx;
   sub ecx, ebx;

   mov edi, height;
   @@foryloop:
      mov esi, ebx;

      @@forxloopUnrolled:
         add esi, 64;
         jg @@EndLoop1;

         movapd xmm0, [eax + esi - 64];
         subpd xmm0, [ecx + esi - 64];
         movapd [eax + esi - 64], xmm0;

         movapd xmm1, [eax + esi - 48];
         subpd xmm1, [ecx + esi - 48];
         movapd [eax + esi - 48], xmm1;

         movapd xmm2, [eax + esi - 32];
         subpd xmm2, [ecx + esi - 32];
         movapd [eax + esi - 32], xmm2;

         movapd xmm3, [eax + esi - 16];
         subpd xmm3, [ecx + esi - 16];
         movapd [eax + esi - 16], xmm3;

      jmp @@forxloopUnrolled;

      @@EndLoop1:

      sub esi, 64;

      jz @NextLine;

      @@forxloop:
         movsd xmm0, [eax + esi];
         movsd xmm1, [ecx + esi];

         subsd xmm0, xmm1;
         movsd [eax + esi], xmm0;

         add esi, 8;
      jnz @@forxloop;

      @NextLine:

      add eax, edx;
   dec edi;
   jnz @@foryloop;

   pop esi;
   pop edi;
   pop ebx;
end;

procedure ASMMatrixSubVecAlignedRow(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; incX : TASMNativeInt; width, Height : TASMNativeInt);
var vecIter : integer;
    aLineWidth : TASMNativeInt;
asm
   push ebx;
   push edi;
   push esi;

   mov aLineWidth, edx;

   mov edi, incx;
   mov ebx, width;

   imul ebx, edi;
   imul ebx, -1;
   mov vecIter, ebx;
   sub ecx, ebx;

   mov ebx, width;
   imul ebx, -8;

   sub eax, ebx;
   

   @@foryloop:
      mov esi, ebx;
      mov edx, vecIter;

      @@forxloop:
         movsd xmm0, [eax + esi];
         movsd xmm1, [ecx + edx];

         subsd xmm0, xmm1;
         movsd [eax + esi], xmm0;

         add edx, edi;
         add esi, 8;
      jnz @@forxloop;

      @NextLine:

      add eax, aLineWidth;
   dec Height;
   jnz @@foryloop;

   pop esi;
   pop edi;
   pop ebx;
end;

procedure ASMMatrixSubVecAlignedCol(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; incX : TASMNativeInt; width, Height : TASMNativeInt);
// eax=A, edx=LinewidthA, ecx=B
asm
   push ebx;
   push esi;
   push edi;

   mov edi, height;
   
   mov ebx, width;
   imul ebx, -8;

   sub eax, ebx;

   @@foryloop:
      mov esi, ebx;

      movddup xmm1, [ecx];

      @@forxloopUnrolled:
         add esi, 64;
         jg @@EndLoop1;

         movapd xmm0, [eax + esi - 64];
         subpd xmm0, xmm1;
         movapd [eax + esi - 64], xmm0;

         movapd xmm3, [eax + esi - 48];
         subpd xmm3, xmm1;
         movapd [eax + esi - 48], xmm3;

         movapd xmm2, [eax + esi - 32];
         subpd xmm2, xmm1;
         movapd [eax + esi - 32], xmm2;

         movapd xmm3, [eax + esi - 16];
         subpd xmm3, xmm1;
         movapd [eax + esi - 16], xmm3;

      jmp @@forxloopUnrolled;

      @@EndLoop1:

      sub esi, 64;

      jz @NextLine;

      @@forxloop:
         movsd xmm0, [eax + esi];

         subsd xmm0, xmm1;
         movsd [eax + esi], xmm0;

         add esi, 8;
      jnz @@forxloop;

      @NextLine:

      add eax, edx;
      add ecx, incX;
   dec edi;
   jnz @@foryloop;

   pop edi;
   pop esi;
   pop ebx;
end;

procedure ASMMatrixSubVecUnalignedVecRow(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; incX : TASMNativeInt; width, Height : TASMNativeInt);
// eax=A, edx=LinewidthA, ecx=B
asm
   push ebx;
   push esi;
   push edi;

   mov edi, height;

   mov ebx, width;
   imul ebx, -8;

   sub eax, ebx;
   sub ecx, ebx;

   @@foryloop:
      mov esi, ebx;

      @@forxloopUnrolled:
         add esi, 64;
         jg @@EndLoop1;

         movupd xmm0, [eax + esi - 64];
         movupd xmm1, [ecx + esi - 64];
         subpd xmm0, xmm1;
         movupd [eax + esi - 64], xmm0;

         movupd xmm1, [eax + esi - 48];
         movupd xmm2, [ecx + esi - 48];
         subpd xmm1, xmm2;
         movupd [eax + esi - 48], xmm1;

         movupd xmm2, [eax + esi - 32];
         movupd xmm3, [ecx + esi - 32];
         subpd xmm2, xmm3;
         movupd [eax + esi - 32], xmm2;

         movupd xmm3, [eax + esi - 16];
         movupd xmm0, [ecx + esi - 16];
         subpd xmm3, xmm0;
         movupd [eax + esi - 16], xmm3;

      jmp @@forxloopUnrolled;

      @@EndLoop1:

      sub esi, 64;

      jz @NextLine;

      @@forxloop:
         movsd xmm0, [eax + esi];
         movsd xmm1, [ecx + esi];

         subsd xmm0, xmm1;
         movsd [eax + esi], xmm0;

         add esi, 8;
      jnz @@forxloop;

      @NextLine:

      add eax, edx;
   dec edi;
   jnz @@foryloop;

   pop edi;
   pop esi;
   pop ebx;
end;

procedure ASMMatrixSubVecUnalignedRow(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; incX : TASMNativeInt; width, Height : TASMNativeInt);
// eax=A, edx=LineWidthA, ecx=B
var vecIter : integer;
    aLineWidth : TASMNativeInt;
asm
   push ebx;
   push edi;
   push esi;

   mov edi, incx;
   mov ebx, width;
   mov aLineWidth, edx;

   imul ebx, edi;
   imul ebx, -1;
   mov vecIter, ebx;
   sub ecx, ebx;

   mov ebx, width;
   imul ebx, -8;

   sub eax, ebx;
   

   @@foryloop:
      mov esi, ebx;
      mov edx, vecIter;

      @@forxloop:
         movsd xmm0, [eax + esi];
         movsd xmm1, [ecx + edx];

         subsd xmm0, xmm1;
         movsd [eax + esi], xmm0;

         add edx, edi;
         add esi, 8;
      jnz @@forxloop;

      @NextLine:

      add eax, aLineWidth;
   dec Height;
   jnz @@foryloop;

   pop esi;
   pop edi;
   pop ebx;
end;

procedure ASMMatrixSubVecUnalignedCol(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; incX : TASMNativeInt; width, Height : TASMNativeInt);
asm
   push ebx;
   push esi;
   push edi;

   mov ebx, width;
   imul ebx, -8;

   sub eax, ebx;

   mov edi, height;
   @@foryloop:
      mov esi, ebx;

      movddup xmm1, [ecx];

      @@forxloopUnrolled:
         add esi, 64;
         jg @@EndLoop1;

         movupd xmm0, [eax + esi - 64];
         subpd xmm0, xmm1;
         movupd [eax + esi - 64], xmm0;

         movupd xmm3, [eax + esi - 48];
         subpd xmm3, xmm1;
         movupd [eax + esi - 48], xmm3;

         movupd xmm2, [eax + esi - 32];
         subpd xmm2, xmm1;
         movupd [eax + esi - 32], xmm2;

         movupd xmm3, [eax + esi - 16];
         subpd xmm3, xmm1;
         movupd [eax + esi - 16], xmm3;

      jmp @@forxloopUnrolled;

      @@EndLoop1:

      sub esi, 64;

      jz @NextLine;

      @@forxloop:
         movsd xmm0, [eax + esi];

         subsd xmm0, xmm1;
         movsd [eax + esi], xmm0;

         add esi, 8;
      jnz @@forxloop;

      @NextLine:

      add eax, edx;
      add ecx, incX;
   dec edi;
   jnz @@foryloop;

   pop edi;
   pop esi;
   pop ebx;
end;


procedure ASMMatrixAddVecAlignedVecRow(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; incX : TASMNativeInt; width, Height : TASMNativeInt);
asm
   push ebx;
   push edi;
   push esi;

   mov ebx, width;
   imul ebx, -8;

   sub eax, ebx;
   sub ecx, ebx;

   mov edi, height;
   @@foryloop:
      mov esi, ebx;

      @@forxloopUnrolled:
         add esi, 64;
         jg @@EndLoop1;

         movapd xmm0, [eax + esi - 64];
         addpd xmm0, [ecx + esi - 64];
         movapd [eax + esi - 64], xmm0;

         movapd xmm1, [eax + esi - 48];
         addpd xmm1, [ecx + esi - 48];
         movapd [eax + esi - 48], xmm1;

         movapd xmm2, [eax + esi - 32];
         addpd xmm2, [ecx + esi - 32];
         movapd [eax + esi - 32], xmm2;

         movapd xmm3, [eax + esi - 16];
         addpd xmm3, [ecx + esi - 16];
         movapd [eax + esi - 16], xmm3;

      jmp @@forxloopUnrolled;

      @@EndLoop1:

      sub esi, 64;

      jz @NextLine;

      @@forxloop:
         movsd xmm0, [eax + esi];
         movsd xmm1, [ecx + esi];

         addsd xmm0, xmm1;
         movsd [eax + esi], xmm0;

         add esi, 8;
      jnz @@forxloop;

      @NextLine:

      add eax, edx;
   dec edi;
   jnz @@foryloop;

   pop esi;
   pop edi;
   pop ebx;
end;

procedure ASMMatrixAddVecAlignedRow(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; incX : TASMNativeInt; width, Height : TASMNativeInt);
var vecIter : integer;
    aLineWidth : TASMNativeInt;
asm
   push ebx;
   push edi;
   push esi;

   mov aLineWidth, edx;

   mov edi, incx;
   mov edx, width;

   imul edx, edi;
   imul edx, -1;
   mov vecIter, edx;

   mov ebx, width;
   imul ebx, -8;

   sub eax, ebx;
   sub ecx, edx;

   @@foryloop:
      mov esi, ebx;
      mov edx, vecIter;

      @@forxloop:
         movsd xmm0, [eax + esi];
         movsd xmm1, [ecx + edx];

         addsd xmm0, xmm1;
         movsd [eax + esi], xmm0;

         add edx, edi;
         add esi, 8;
      jnz @@forxloop;

      @NextLine:

      add eax, aLineWidth;
   dec Height;
   jnz @@foryloop;

   pop esi;
   pop edi;
   pop ebx;
end;

procedure ASMMatrixAddVecAlignedCol(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; incX : TASMNativeInt; width, Height : TASMNativeInt);
asm
   push ebx;
   push edi;
   push esi;

   mov ebx, width;
   imul ebx, -8;

   sub eax, ebx;

   mov edi, height;
   @@foryloop:
      mov esi, ebx;

      movddup xmm1, [ecx];

      @@forxloopUnrolled:
         add esi, 64;
         jg @@EndLoop1;

         movapd xmm0, [eax + esi - 64];
         addpd xmm0, xmm1;
         movapd [eax + esi - 64], xmm0;

         movapd xmm3, [eax + esi - 48];
         addpd xmm3, xmm1;
         movapd [eax + esi - 48], xmm3;

         movapd xmm2, [eax + esi - 32];
         addpd xmm2, xmm1;
         movapd [eax + esi - 32], xmm2;

         movapd xmm3, [eax + esi - 16];
         addpd xmm3, xmm1;
         movapd [eax + esi - 16], xmm3;

      jmp @@forxloopUnrolled;

      @@EndLoop1:

      sub esi, 64;

      jz @NextLine;

      @@forxloop:
         movsd xmm0, [eax + esi];

         addsd xmm0, xmm1;
         movsd [eax + esi], xmm0;

         add esi, 8;
      jnz @@forxloop;

      @NextLine:

      add eax, edx;
      add ecx, incX;
   dec edi;
   jnz @@foryloop;

   pop esi;
   pop edi;
   pop ebx;
end;

procedure ASMMatrixAddVecUnalignedVecRow(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; incX : TASMNativeInt; width, Height : TASMNativeInt);
asm
   push ebx;
   push edi;
   push esi;
   
   mov ebx, width;
   imul ebx, -8;

   sub eax, ebx;
   sub ecx, ebx;

   mov edi, height;
   @@foryloop:
      mov esi, ebx;

      @@forxloopUnrolled:
         add esi, 64;
         jg @@EndLoop1;

         movupd xmm0, [eax + esi - 64];
         movupd xmm1, [ecx + esi - 64];
         addpd xmm0, xmm1;
         movupd [eax + esi - 64], xmm0;

         movupd xmm1, [eax + esi - 48];
         movupd xmm2, [ecx + esi - 48];
         addpd xmm1, xmm2;
         movupd [eax + esi - 48], xmm1;

         movupd xmm2, [eax + esi - 32];
         movupd xmm3, [ecx + esi - 32];
         addpd xmm2, xmm3;
         movupd [eax + esi - 32], xmm2;

         movupd xmm3, [eax + esi - 16];
         movupd xmm0, [ecx + esi - 16];
         addpd xmm3, xmm0;
         movupd [eax + esi - 16], xmm3;

      jmp @@forxloopUnrolled;

      @@EndLoop1:

      sub esi, 64;

      jz @NextLine;

      @@forxloop:
         movsd xmm0, [eax + esi];
         movsd xmm1, [ecx + esi];

         addsd xmm0, xmm1;
         movsd [eax + esi], xmm0;

         add esi, 8;
      jnz @@forxloop;

      @NextLine:

      add eax, edx;
   dec edi;
   jnz @@foryloop;

   pop esi;
   pop edi;
   pop ebx;
end;

procedure ASMMatrixAddVecUnalignedRow(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; incX : TASMNativeInt; width, Height : TASMNativeInt);
var vecIter : integer;
    aLineWidth : TASMNativeInt;
asm
   push ebx;
   push edi;
   push esi;

   mov aLineWidth, edx;

   mov edi, incx;
   mov ebx, width;

   imul ebx, edi;
   imul ebx, -1;
   mov vecIter, ebx;
   sub ecx, ebx;
   
   mov ebx, width;
   imul ebx, -8;

   sub eax, ebx;
   

   @@foryloop:
      mov esi, ebx;
      mov edx, vecIter;

      @@forxloop:
         movsd xmm0, [eax + esi];
         movsd xmm1, [ecx + edx];

         addsd xmm0, xmm1;
         movsd [eax + esi], xmm0;

         add edx, edi;
         add esi, 8;
      jnz @@forxloop;

      @NextLine:

      add eax, aLineWidth;
   dec Height;
   jnz @@foryloop;

   pop esi;
   pop edi;
   pop ebx;
end;

procedure ASMMatrixAddVecUnalignedCol(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; incX : TASMNativeInt; width, Height : TASMNativeInt);
asm
   push ebx;
   push edi;
   push esi;
   
   mov ebx, width;
   imul ebx, -8;

   sub eax, ebx;

   mov edi, height;
   @@foryloop:
      mov esi, ebx;

      movddup xmm1, [ecx];

      @@forxloopUnrolled:
         add esi, 64;
         jg @@EndLoop1;

         movupd xmm0, [eax + esi - 64];
         addpd xmm0, xmm1;
         movupd [eax + esi - 64], xmm0;

         movupd xmm3, [eax + esi - 48];
         addpd xmm3, xmm1;
         movupd [eax + esi - 48], xmm3;

         movupd xmm2, [eax + esi - 32];
         addpd xmm2, xmm1;
         movupd [eax + esi - 32], xmm2;

         movupd xmm3, [eax + esi - 16];
         addpd xmm3, xmm1;
         movupd [eax + esi - 16], xmm3;

      jmp @@forxloopUnrolled;

      @@EndLoop1:

      sub esi, 64;

      jz @NextLine;

      @@forxloop:
         movsd xmm0, [eax + esi];

         addsd xmm0, xmm1;
         movsd [eax + esi], xmm0;

         add esi, 8;
      jnz @@forxloop;

      @NextLine:

      add eax, edx;
      add ecx, incX;
   dec edi;
   jnz @@foryloop;

   pop esi;
   pop edi;
   pop ebx;
end;

// perform y[i] = y[i] + alpha * x[i]
// unrolled 4 times
procedure ASMVecAddUnAlignedSeq( X : PDouble; y : PDouble; N : TASMNativeInt; alpha : double );
// eax = X, edx = Y, ecx = N
asm
   movddup xmm0, alpha;

   imul ecx, -8;
   sub eax, ecx;
   sub edx, ecx;

   @@forNLoop:
     add ecx, 128;
     jg @loopEnd;

     movupd xmm1, [eax + ecx - 128];
     movupd xmm2, [edx + ecx - 128];
     mulpd xmm1, xmm0;
     addpd xmm1, xmm2;
     movupd [edx + ecx - 128], xmm1;

     movupd xmm1, [eax + ecx - 112];
     movupd xmm2, [edx + ecx - 112];
     mulpd xmm1, xmm0;
     addpd xmm1, xmm2;
     movupd [edx + ecx - 112], xmm1;

     movupd xmm3, [eax + ecx - 96];
     movupd xmm4, [edx + ecx - 96];
     mulpd xmm3, xmm0;
     addpd xmm3, xmm4;
     movupd [edx + ecx - 96], xmm3;

     movupd xmm2, [eax + ecx - 80];
     movupd xmm1, [edx + ecx - 80];
     mulpd xmm2, xmm0;
     addpd xmm1, xmm2;
     movupd [edx + ecx - 80], xmm1;

     movupd xmm4, [eax + ecx - 64];
     movupd xmm3, [edx + ecx - 64];
     mulpd xmm4, xmm0;
     addpd xmm3, xmm4;
     movupd [edx + ecx - 64], xmm3;

     movupd xmm2, [eax + ecx - 48];
     movupd xmm1, [edx + ecx - 48];
     mulpd xmm2, xmm0;
     addpd xmm1, xmm2;
     movupd [edx + ecx - 48], xmm1;

     movupd xmm4, [eax + ecx - 32];
     movupd xmm3, [edx + ecx - 32];
     mulpd xmm4, xmm0;
     addpd xmm3, xmm4;
     movupd [edx + ecx - 32], xmm3;

     movupd xmm4, [eax + ecx - 16];
     movupd xmm3, [edx + ecx - 16];
     mulpd xmm4, xmm0;
     addpd xmm3, xmm4;
     movupd [edx + ecx - 16], xmm3;

   jmp @@forNLoop

   @loopEnd:

   sub ecx, 128;
   jz @endLine;

   @forNLoop2:
      add ecx, 16;
      jg @endLine2;

      movupd xmm4, [eax + ecx - 16];
      movupd xmm3, [edx + ecx - 16];
      mulpd xmm4, xmm0;
      addpd xmm3, xmm4;
      movupd [edx + ecx - 16], xmm3;
   jmp @forNLoop2;

   @endLine2:

   // last odd element
   sub ecx, 8;
   jnz @endLine;

   movsd xmm4, [eax - 8];
   movsd xmm3, [edx - 8];
   mulsd xmm4, xmm0;
   addsd xmm3, xmm4;
   movsd [edx - 8], xmm3;

   @endLine:
end;

procedure ASMVecAddAlignedSeq( X : PDouble; y : PDouble; N : TASMNativeInt; alpha : double );
// eax = X, edx = Y, ecx = N
asm
   movddup xmm0, alpha;

   imul ecx, -8;
   sub eax, ecx;
   sub edx, ecx;

   @@forNLoop:
     add ecx, 128;
     jg @loopEnd;

     movapd xmm1, [eax + ecx - 128];
     movapd xmm2, [edx + ecx - 128];
     mulpd xmm1, xmm0;
     addpd xmm1, xmm2;
     movapd [edx + ecx - 128], xmm1;

     movapd xmm1, [eax + ecx - 112];
     movapd xmm2, [edx + ecx - 112];
     mulpd xmm1, xmm0;
     addpd xmm1, xmm2;
     movapd [edx + ecx - 112], xmm1;

     movapd xmm3, [eax + ecx - 96];
     movapd xmm4, [edx + ecx - 96];
     mulpd xmm3, xmm0;
     addpd xmm3, xmm4;
     movapd [edx + ecx - 96], xmm3;

     movapd xmm2, [eax + ecx - 80];
     movapd xmm1, [edx + ecx - 80];
     mulpd xmm2, xmm0;
     addpd xmm1, xmm2;
     movapd [edx + ecx - 80], xmm1;

     movapd xmm4, [eax + ecx - 64];
     movapd xmm3, [edx + ecx - 64];
     mulpd xmm4, xmm0;
     addpd xmm3, xmm4;
     movapd [edx + ecx - 64], xmm3;

     movapd xmm2, [eax + ecx - 48];
     movapd xmm1, [edx + ecx - 48];
     mulpd xmm2, xmm0;
     addpd xmm1, xmm2;
     movapd [edx + ecx - 48], xmm1;

     movapd xmm4, [eax + ecx - 32];
     movapd xmm3, [edx + ecx - 32];
     mulpd xmm4, xmm0;
     addpd xmm3, xmm4;
     movapd [edx + ecx - 32], xmm3;

     movapd xmm4, [eax + ecx - 16];
     movapd xmm3, [edx + ecx - 16];
     mulpd xmm4, xmm0;
     addpd xmm3, xmm4;
     movapd [edx + ecx - 16], xmm3;

   jmp @@forNLoop

   @loopEnd:

   sub ecx, 128;
   jz @endLine;

   @forNLoop2:
      add ecx, 16;
      jg @endLine2;

      movapd xmm4, [eax + ecx - 16];
      movapd xmm3, [edx + ecx - 16];
      mulpd xmm4, xmm0;
      addpd xmm3, xmm4;
      movapd [edx + ecx - 16], xmm3;
   jmp @forNLoop2;

   @endLine2:

   // last odd element
   sub ecx, 8;
   jnz @endLine;

   movsd xmm4, [eax - 8];
   movsd xmm3, [edx - 8];
   mulsd xmm4, xmm0;
   addsd xmm3, xmm4;
   movsd [edx - 8], xmm3;

   @endLine:
end;

procedure ASMVecAddNonSeq( X : PDouble; y : PDouble; N : TASMNativeInt; incX, incY : TASMNativeInt; alpha : double );
// x = eax; y = edx; N = ecx
asm
   push edi;
   push esi;

   mov edi, incX;
   mov esi, incY;

   movsd xmm0, alpha;

   @@loopN:
      movsd xmm1, [eax];
      movsd xmm2, [edx];
      mulsd xmm1, xmm0;
      addsd xmm2, xmm1;

      movsd [edx], xmm2;
      add edx, esi;
      add eax, edi;
   dec ecx;
   jnz @@loopN;

   pop esi;
   pop edi;
end;


{$ENDIF}

end.
