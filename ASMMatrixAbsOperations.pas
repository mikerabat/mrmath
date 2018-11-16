// ###################################################################
// #### This file is part of the mathematics library project, and is
// #### offered under the licence agreement described on
// #### http://www.mrsoft.org/
// ####
// #### Copyright:(c) 2017, Michael R. . All rights reserved.
// ####
// #### Unless required by applicable law or agreed to in writing, software
// #### distributed under the License is distributed on an "AS IS" BASIS,
// #### WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// #### See the License for the specific language governing permissions and
// #### limitations under the License.
// ###################################################################

unit ASMMatrixAbsOperations;

// #####################################################
// #### Abs opertaion applied to every element in a matrix
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

procedure ASMMatrixAbsAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixAbsUnAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

procedure ASMMatrixAbsAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixAbsUnAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

{$ENDIF}

implementation

{$IFNDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$S-} {$ENDIF}

const cLocSignBits : Array[0..1] of int64 = ($7FFFFFFFFFFFFFFF, $7FFFFFFFFFFFFFFF);

procedure ASMMatrixAbsAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt);
// eax=dest, edx=LineWidth, width=ecx
asm
   push ebx;

   // width* -8
   imul ecx, -8;

   // helper registers for the mt1, mt2 and dest pointers
   sub eax, ecx;
   movupd xmm0, cLocSignBits;

   // for y := 0 to height - 1:
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov ebx, ecx;
       @addforxloop:
           add ebx, 128;
           jg @loopEnd;

           // Abs:
           movapd xmm4, [eax + ebx - 128];
           Andpd xmm4, xmm0;
           movapd [eax + ebx - 128], xmm4;

           movapd xmm1, [eax + ebx - 112];
           andpd xmm1, xmm0;
           movapd [eax + ebx - 112], xmm1;

           movapd xmm2, [eax + ebx - 96];
           andpd xmm2, xmm0;
           movapd [eax + ebx - 96], xmm2;

           movapd xmm3, [eax + ebx - 80];
           andpd xmm3, xmm0;
           movapd [eax + ebx - 80], xmm3;

           movapd xmm4, [eax + ebx - 64];
           andpd xmm4, xmm0;
           movapd [eax + ebx - 64], xmm4;

           movapd xmm5, [eax + ebx - 48];
           andpd xmm5, xmm0;
           movapd [eax + ebx - 48], xmm5;

           movapd xmm6, [eax + ebx - 32];
           andpd xmm6, xmm0;
           movapd [eax + ebx - 32], xmm6;

           movapd xmm7, [eax + ebx - 16];
           andpd xmm7, xmm0;
           movapd [eax + ebx - 16], xmm7;
       jmp @addforxloop

       @loopEnd:

       sub ebx, 128;

       jz @nextLine;

       @addforxloop2:
           movapd xmm1, [eax + ebx];
           andpd xmm1, xmm0;
           movapd [eax + ebx], xmm1;
       add ebx, 16;
       jnz @addforxloop2;

       @nextLine:

       // next line:
       add eax, edx;

   // loop y end
   dec height;
   jnz @@addforyloop;

   pop ebx;
end;

procedure ASMMatrixAbsUnAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt);
asm
   push ebx;

   imul ecx, -8;

   // helper registers for the mt1, mt2 and dest pointers
   sub eax, ecx;

   movupd xmm0, cLocSignBits;

   // for y := 0 to height - 1:
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov ebx, ecx;
       @addforxloop:
           add ebx, 128;
           jg @loopEnd;

           // Abs:
           movupd xmm4, [eax + ebx - 128];
           Andpd xmm4, xmm0;
           movupd [eax + ebx - 128], xmm4;

           movupd xmm1, [eax + ebx - 112];
           andpd xmm1, xmm0;
           movupd [eax + ebx - 112], xmm1;

           movupd xmm2, [eax + ebx - 96];
           andpd xmm2, xmm0;
           movupd [eax + ebx - 96], xmm2;

           movupd xmm3, [eax + ebx - 80];
           andpd xmm3, xmm0;
           movupd [eax + ebx - 80], xmm3;

           movupd xmm4, [eax + ebx - 64];
           andpd xmm4, xmm0;
           movupd [eax + ebx - 64], xmm4;

           movupd xmm5, [eax + ebx - 48];
           andpd xmm5, xmm0;
           movupd [eax + ebx - 48], xmm5;

           movupd xmm6, [eax + ebx - 32];
           andpd xmm6, xmm0;
           movupd [eax + ebx - 32], xmm6;

           movupd xmm7, [eax + ebx - 16];
           andpd xmm7, xmm0;
           movupd [eax + ebx - 16], xmm7;
       jmp @addforxloop

       @loopEnd:

       sub ebx, 128;

       jz @nextLine;

       @addforxloop2:
           movupd xmm1, [eax + ebx];
           andpd xmm1, xmm0;

           movupd [eax + ebx], xmm1;
       add ebx, 16;
       jnz @addforxloop2;

       @nextLine:

       // next line:
       add eax, edx;

   // loop y end
   dec Height;
   jnz @@addforyloop;

   pop ebx;
end;

procedure ASMMatrixAbsAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt);
asm
   push ebx;

   dec ecx;
   imul ecx, -8;

   sub eax, ecx;

   movupd xmm0, cLocSignBits;

   // for y := 0 to height - 1:
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov ebx, ecx;
       @addforxloop:
           add ebx, 128;
           jg @loopEnd;

           // Abs:
           movapd xmm4, [eax + ebx - 128];
           Andpd xmm4, xmm0;
           movapd [eax + ebx - 128], xmm4;

           movapd xmm1, [eax + ebx - 112];
           andpd xmm1, xmm0;
           movapd [eax + ebx - 112], xmm1;

           movapd xmm2, [eax + ebx - 96];
           andpd xmm2, xmm0;
           movapd [eax + ebx - 96], xmm2;

           movapd xmm3, [eax + ebx - 80];
           andpd xmm3, xmm0;
           movapd [eax + ebx - 80], xmm3;

           movapd xmm4, [eax + ebx - 64];
           andpd xmm4, xmm0;
           movapd [eax + ebx - 64], xmm4;

           movapd xmm5, [eax + ebx - 48];
           andpd xmm5, xmm0;
           movapd [eax + ebx - 48], xmm5;

           movapd xmm6, [eax + ebx - 32];
           andpd xmm6, xmm0;
           movapd [eax + ebx - 32], xmm6;

           movapd xmm7, [eax + ebx - 16];
           andpd xmm7, xmm0;
           movapd [eax + ebx - 16], xmm7;
       jmp @addforxloop

       @loopEnd:

       sub ebx, 128;

       jz @nextLine;

       @addforxloop2:
           movapd xmm1, [eax + ebx - 16];
           andpd xmm1, xmm0;
           movapd [eax + ebx], xmm1;
       add ebx, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last column:
       movsd xmm1, [eax];
       andpd xmm1, xmm0;

       movsd [eax], xmm1;

       // next line:
       add eax, edx;

   // loop y end
   dec Height;
   jnz @@addforyloop;

   pop ebx;
end;

procedure ASMMatrixAbsUnAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt);
asm
   push ebx;

   dec ecx;
   imul ecx, -8;
   sub eax, ecx;

   movupd xmm0, cLocSignBits;

   // for y := 0 to height - 1:
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov ebx, ecx;
       @addforxloop:
           add ebx, 128;
           jg @loopEnd;

           // Abs:
           movupd xmm4, [eax + ebx - 128];
           Andpd xmm4, xmm0;
           movupd [eax + ebx - 128], xmm4;

           movupd xmm1, [eax + ebx - 112];
           andpd xmm1, xmm0;
           movupd [eax + ebx - 112], xmm1;

           movupd xmm2, [eax + ebx - 96];
           andpd xmm2, xmm0;
           movupd [eax + ebx - 96], xmm2;

           movupd xmm3, [eax + ebx - 80];
           andpd xmm3, xmm0;
           movupd [eax + ebx - 80], xmm3;

           movupd xmm4, [eax + ebx - 64];
           andpd xmm4, xmm0;
           movupd [eax + ebx - 64], xmm4;

           movupd xmm5, [eax + ebx - 48];
           andpd xmm5, xmm0;
           movupd [eax + ebx - 48], xmm5;

           movupd xmm6, [eax + ebx - 32];
           andpd xmm6, xmm0;
           movupd [eax + ebx - 32], xmm6;

           movupd xmm7, [eax + ebx - 16];
           andpd xmm7, xmm0;
           movupd [eax + ebx - 16], xmm7;
       jmp @addforxloop

       @loopEnd:

       sub ebx, 128;

       jz @nextLine;

       @addforxloop2:
           movupd xmm1, [eax + ebx];
           andpd xmm1, xmm0;

           movupd [eax + ebx], xmm1;
       add ebx, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last column:
       movsd xmm1, [eax];
       andpd xmm1, xmm0;

       movsd [eax], xmm1;

       // next line:
       add eax, edx;

   // loop y end
   dec Height;
   jnz @@addforyloop;

   pop ebx;
end;

{$ENDIF}

end.
