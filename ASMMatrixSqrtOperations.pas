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


unit ASMMatrixSqrtOperations;

// #####################################################
// #### SQRT opertaion applied to every element in a matrix
// #####################################################

interface

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}
{$IFNDEF x64}

uses MatrixConst;

procedure ASMMatrixSQRTAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixSQRTUnAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

procedure ASMMatrixSQRTAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixSQRTUnAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

{$ENDIF}

implementation

{$IFNDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$S-} {$ENDIF}

procedure ASMMatrixSQRTAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt);
asm
   push ebx;
   push edi;

   // ecx
   imul ecx, -8;

   // helper registers for the mt1, mt2 and dest pointers
   sub eax, ecx;

   // for y := 0 to height - 1:
   mov ebx, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, ecx;
       @addforxloop:
           add edi, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetchw [eax + edi];

           // sqrt:
           sqrtpd xmm0, [eax + edi - 128];
           movapd [eax + edi - 128], xmm0;

           sqrtpd xmm1, [eax + edi - 112];
           movapd [eax + edi - 112], xmm1;

           sqrtpd xmm2, [eax + edi - 96];
           movapd [eax + edi - 96], xmm2;

           sqrtpd xmm3, [eax + edi - 80];
           movapd [eax + edi - 80], xmm3;

           sqrtpd xmm4, [eax + edi - 64];
           movapd [eax + edi - 64], xmm4;

           sqrtpd xmm5, [eax + edi - 48];
           movapd [eax + edi - 48], xmm5;

           sqrtpd xmm6, [eax + edi - 32];
           movapd [eax + edi - 32], xmm6;

           sqrtpd xmm7, [eax + edi - 16];
           movapd [eax + edi - 16], xmm7;
       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @nextLine;

       @addforxloop2:
           sqrtpd xmm0, [eax + edi];
           movapd [eax + edi], xmm0;
       add edi, 16;
       jnz @addforxloop2;

       @nextLine:

       // next line:
       add eax, edx;

   // loop y end
   dec ebx;
   jnz @@addforyloop;

   pop edi;
   pop ebx;
end;

procedure ASMMatrixSQRTUnAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt);
asm
   push ebx;
   push edi;

   // ecx
   imul ecx, -8;

   // helper registers for the mt1, mt2 and dest pointers
   sub eax, ecx;

   // for y := 0 to height - 1:
   mov ebx, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, ecx;
       @addforxloop:
           add edi, 128;
           jg @loopEnd;

           // sqrt:
           movupd xmm0, [eax + edi - 128];
           sqrtpd xmm0, xmm0;
           movupd [eax + edi - 128], xmm0;

           movupd xmm0, [eax + edi - 112];
           sqrtpd xmm1, xmm1;
           movupd [eax + edi - 112], xmm1;

           movupd xmm2, [eax + edi - 96];
           sqrtpd xmm2, xmm2;
           movupd [eax + edi - 96], xmm2;

           movupd xmm3, [eax + edi - 80];
           sqrtpd xmm3, xmm3;
           movupd [eax + edi - 80], xmm3;

           movupd xmm4, [eax + edi - 64];
           sqrtpd xmm4, xmm4;
           movupd [eax + edi - 64], xmm4;

           movupd xmm5, [eax + edi - 48];
           sqrtpd xmm5, xmm5;
           movupd [eax + edi - 48], xmm5;

           movupd xmm6, [eax + edi - 32];
           sqrtpd xmm6, xmm6;
           movupd [eax + edi - 32], xmm6;

           movupd xmm7, [eax + edi - 16];
           sqrtpd xmm7, xmm7;
           movupd [eax + edi - 16], xmm7;
       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @nextLine;

       @addforxloop2:
           movupd xmm0, [eax + edi];
           sqrtpd xmm0, xmm0;

           movupd [eax + edi], xmm0;
       add edi, 16;
       jnz @addforxloop2;

       @nextLine:

       // next line:
       add eax, edx;

   // loop y end
   dec ebx;
   jnz @@addforyloop;

   pop edi;
   pop ebx;
end;

procedure ASMMatrixSQRTAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt);
asm
   push ebx;
   push edi;

   // ecx
   dec ecx;
   imul ecx, -8;

   // helper registers for the mt1, mt2 and dest pointers
   sub eax, ecx;

   // for y := 0 to height - 1:
   mov ebx, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, ecx;
       @addforxloop:
           add edi, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetchw [eax + edi];

           // addition:
           sqrtpd xmm0, [eax + edi - 128];
           movapd [eax + edi - 128], xmm0;

           sqrtpd xmm1, [eax + edi - 112];
           movapd [eax + edi - 112], xmm1;

           sqrtpd xmm2, [eax + edi - 96];
           movapd [eax + edi - 96], xmm2;

           sqrtpd xmm3, [eax + edi - 80];
           movapd [eax + edi - 80], xmm3;

           sqrtpd xmm4, [eax + edi - 64];
           movapd [eax + edi - 64], xmm4;

           sqrtpd xmm5, [eax + edi - 48];
           movapd [eax + edi - 48], xmm5;

           sqrtpd xmm6, [eax + edi - 32];
           movapd [eax + edi - 32], xmm6;

           sqrtpd xmm7, [eax + edi - 16];
           movapd [eax + edi - 16], xmm7;
       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @nextLine;

       @addforxloop2:
           sqrtpd xmm0, [eax + edi];
           movapd [eax + edi], xmm0;
       add edi, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last column:
       movsd xmm0, [eax];
       sqrtsd xmm0, xmm0;

       movsd [eax], xmm0;

       // next line:
       add eax, edx;

   // loop y end
   dec ebx;
   jnz @@addforyloop;

   pop edi;
   pop ebx;
end;

procedure ASMMatrixSQRTUnAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt);
asm
   push ebx;
   push edi;

   // ecx
   dec ecx;
   imul ecx, -8;

   // helper registers for the mt1, mt2 and dest pointers
   sub eax, ecx;

   // for y := 0 to height - 1:
   mov ebx, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, ecx;
       @addforxloop:
           add edi, 128;
           jg @loopEnd;

           // sqrt:
           movupd xmm0, [eax + edi - 128];
           sqrtpd xmm0, xmm0;
           movupd [eax + edi - 128], xmm0;

           movupd xmm0, [eax + edi - 112];
           sqrtpd xmm1, xmm1;
           movupd [eax + edi - 112], xmm1;

           movupd xmm2, [eax + edi - 96];
           sqrtpd xmm2, xmm2;
           movupd [eax + edi - 96], xmm2;

           movupd xmm3, [eax + edi - 80];
           sqrtpd xmm3, xmm3;
           movupd [eax + edi - 80], xmm3;

           movupd xmm4, [eax + edi - 64];
           sqrtpd xmm4, xmm4;
           movupd [eax + edi - 64], xmm4;

           movupd xmm5, [eax + edi - 48];
           sqrtpd xmm5, xmm5;
           movupd [eax + edi - 48], xmm5;

           movupd xmm6, [eax + edi - 32];
           sqrtpd xmm6, xmm6;
           movupd [eax + edi - 32], xmm6;

           movupd xmm7, [eax + edi - 16];
           sqrtpd xmm7, xmm7;
           movupd [eax + edi - 16], xmm7;
       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @nextLine;

       @addforxloop2:
           movupd xmm0, [eax + edi];
           sqrtpd xmm0, xmm0;

           movupd [eax + edi], xmm0;
       add edi, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last column:
       movsd xmm0, [eax];
       sqrtsd xmm0, xmm0;

       movsd [eax], xmm0;

       // next line:
       add eax, edx;

   // loop y end
   dec ebx;
   jnz @@addforyloop;

   pop edi;
   pop ebx;
end;

{$ENDIF}

end.
