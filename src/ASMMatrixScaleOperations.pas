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


unit ASMMatrixScaleOperations;

interface

{$I 'mrMath_CPU.inc'}

{$IFNDEF x64}

procedure ASMMatrixAddScaleAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : NativeInt; const dOffset, Scale : double); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixAddScaleUnAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : NativeInt; const dOffset, Scale : double); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

procedure ASMMatrixAddScaleAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : NativeInt; const dOffset, Scale : double); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixAddScaleUnAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : NativeInt; const dOffset, Scale : double); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

procedure ASMMatrixScaleAddAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : NativeInt; const dOffset, Scale : double); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixScaleAddUnAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : NativeInt; const dOffset, Scale : double); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

procedure ASMMatrixScaleAddAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : NativeInt; const dOffset, Scale : double); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixScaleAddUnAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : NativeInt; const dOffset, Scale : double); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

procedure ASMMatrixElemAddAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : NativeInt; const dOffset : double); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixElemAddUnAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : NativeInt; const dOffset : double); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

procedure ASMMatrixElemAddAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : NativeInt; const dOffset : double); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixElemAddUnAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : NativeInt; const dOffset : double); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

{$ENDIF}

implementation

{$IFNDEF x64}

procedure ASMMatrixAddScaleAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : NativeInt; const dOffset, Scale : double);
// eax = dest, edx = lineWidt, ecx = width
asm
   push ebx;
   push edi;

   // iters
   imul ecx, -8;

   // helper registers for the mt1, mt2 and dest pointers
   sub eax, ecx;

   movddup xmm6, dOffset;
   movddup xmm7, Scale;

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
           // prefetch [eax + edi];

           // add scale:
           movapd xmm0, [eax + edi - 128];
           addpd xmm0, xmm6;
           mulpd xmm0, xmm7;
           movapd [eax + edi - 128], xmm0;

           movapd xmm1, [eax + edi - 112];
           addpd xmm1, xmm6;
           mulpd xmm1, xmm7;
           movapd [eax + edi - 112], xmm1;

           movapd xmm2, [eax + edi - 96];
           addpd xmm2, xmm6;
           mulpd xmm2, xmm7;
           movapd [eax + edi - 96], xmm2;

           movapd xmm3, [eax + edi - 80];
           addpd xmm3, xmm6;
           mulpd xmm3, xmm7;
           movapd [eax + edi - 80], xmm3;

           movapd xmm0, [eax + edi - 64];
           addpd xmm0, xmm6;
           mulpd xmm0, xmm7;
           movapd [eax + edi - 64], xmm0;

           movapd xmm1, [eax + edi - 48];
           addpd xmm1, xmm6;
           mulpd xmm1, xmm7;
           movapd [eax + edi - 48], xmm1;

           movapd xmm2, [eax + edi - 32];
           addpd xmm2, xmm6;
           mulpd xmm2, xmm7;
           movapd [eax + edi - 32], xmm2;

           movapd xmm3, [eax + edi - 16];
           addpd xmm3, xmm6;
           mulpd xmm3, xmm7;
           movapd [eax + edi - 16], xmm3;
       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @nextLine;

       @addforxloop2:
           movapd xmm0, [eax + edi];
           addpd xmm0, xmm6;
           mulpd xmm0, xmm7;
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

procedure ASMMatrixAddScaleUnAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : NativeInt; const dOffset, Scale : double);
asm
   push ebx;
   push edi;

   // iters
   imul ecx, -8;

   // helper registers for the mt1, mt2 and dest pointers
   sub eax, ecx;

   movddup xmm6, dOffset;
   movddup xmm7, Scale;

   // for y := 0 to height - 1:
   mov ebx, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, ecx;
       @addforxloop:
           add edi, 128;
           jg @loopEnd;

           // add scale:
           movupd xmm0, [eax + edi - 128];
           addpd xmm0, xmm6;
           mulpd xmm0, xmm7;
           movupd [eax + edi - 128], xmm0;

           movupd xmm1, [eax + edi - 112];
           addpd xmm1, xmm6;
           mulpd xmm1, xmm7;
           movupd [eax + edi - 112], xmm1;

           movupd xmm2, [eax + edi - 96];
           addpd xmm2, xmm6;
           mulpd xmm2, xmm7;
           movupd [eax + edi - 96], xmm2;

           movupd xmm3, [eax + edi - 80];
           addpd xmm3, xmm6;
           mulpd xmm3, xmm7;
           movupd [eax + edi - 80], xmm3;

           movupd xmm0, [eax + edi - 64];
           addpd xmm0, xmm6;
           mulpd xmm0, xmm7;
           movupd [eax + edi - 64], xmm0;

           movupd xmm1, [eax + edi - 48];
           addpd xmm1, xmm6;
           mulpd xmm1, xmm7;
           movupd [eax + edi - 48], xmm1;

           movupd xmm2, [eax + edi - 32];
           addpd xmm2, xmm6;
           mulpd xmm2, xmm7;
           movupd [eax + edi - 32], xmm2;

           movupd xmm3, [eax + edi - 16];
           addpd xmm3, xmm6;
           mulpd xmm3, xmm7;
           movupd [eax + edi - 16], xmm3;
       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @nextLine;

       @addforxloop2:
           movupd xmm0, [eax + edi];
           addpd xmm0, xmm6;
           mulpd xmm0, xmm7;
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

procedure ASMMatrixAddScaleAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : NativeInt; const dOffset, Scale : double);
asm
   push ebx;
   push edi;

   // iters
   dec ecx;
   imul ecx, -8;

   // helper registers for the mt1, mt2 and dest pointers
   sub eax, ecx;

   movddup xmm6, dOffset;
   movddup xmm7, Scale;

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
           // prefetch [eax + edi];

           // add mult:
           movapd xmm0, [eax + edi - 128];
           addpd xmm0, xmm6;
           mulpd xmm0, xmm7;
           movapd [eax + edi - 128], xmm0;

           movapd xmm1, [eax + edi - 112];
           addpd xmm1, xmm6;
           mulpd xmm1, xmm7;
           movapd [eax + edi - 112], xmm1;

           movapd xmm2, [eax + edi - 96];
           addpd xmm2, xmm6;
           mulpd xmm2, xmm7;
           movapd [eax + edi - 96], xmm2;

           movapd xmm3, [eax + edi - 80];
           addpd xmm3, xmm6;
           mulpd xmm3, xmm7;
           movapd [eax + edi - 80], xmm3;

           movapd xmm0, [eax + edi - 64];
           addpd xmm0, xmm6;
           mulpd xmm0, xmm7;
           movapd [eax + edi - 64], xmm0;

           movapd xmm1, [eax + edi - 48];
           addpd xmm1, xmm6;
           mulpd xmm1, xmm7;
           movapd [eax + edi - 48], xmm1;

           movapd xmm2, [eax + edi - 32];
           addpd xmm2, xmm6;
           mulpd xmm2, xmm7;
           movapd [eax + edi - 32], xmm2;

           movapd xmm3, [eax + edi - 16];
           addpd xmm3, xmm6;
           mulpd xmm3, xmm7;
           movapd [eax + edi - 16], xmm3;
       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @nextLine;

       @addforxloop2:
           movapd xmm0, [eax + edi];
           addpd xmm0, xmm6;
           mulpd xmm0, xmm7;
           movapd [eax + edi], xmm0;
       add edi, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last column:
       movsd xmm0, [eax];
       addsd xmm0, xmm6;
       mulsd xmm0, xmm7;
       movsd [eax], xmm0;

       // next line:
       add eax, edx;

   // loop y end
   dec ebx;
   jnz @@addforyloop;

   pop edi;
   pop ebx;
end;

procedure ASMMatrixAddScaleUnAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : NativeInt; const dOffset, Scale : double);
asm
   push ebx;
   push edi;

   // iters
   dec ecx;
   imul ecx, -8;

   // helper registers for the mt1, mt2 and dest pointers
   sub eax, ecx;

   movddup xmm6, dOffset;
   movddup xmm7, Scale;

   // for y := 0 to height - 1:
   mov ebx, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, ecx;
       @addforxloop:
           add edi, 128;
           jg @loopEnd;

           // add mult:
           movupd xmm0, [eax + edi - 128];
           addpd xmm0, xmm6;
           mulpd xmm0, xmm7;
           movupd [eax + edi - 128], xmm0;

           movupd xmm1, [eax + edi - 112];
           addpd xmm1, xmm6;
           mulpd xmm1, xmm7;
           movupd [eax + edi - 112], xmm1;

           movupd xmm2, [eax + edi - 96];
           addpd xmm2, xmm6;
           mulpd xmm2, xmm7;
           movupd [eax + edi - 96], xmm2;

           movupd xmm3, [eax + edi - 80];
           addpd xmm3, xmm6;
           mulpd xmm3, xmm7;
           movupd [eax + edi - 80], xmm3;

           movupd xmm0, [eax + edi - 64];
           addpd xmm0, xmm6;
           mulpd xmm0, xmm7;
           movupd [eax + edi - 64], xmm0;

           movupd xmm1, [eax + edi - 48];
           addpd xmm1, xmm6;
           mulpd xmm1, xmm7;
           movupd [eax + edi - 48], xmm1;

           movupd xmm2, [eax + edi - 32];
           addpd xmm2, xmm6;
           mulpd xmm2, xmm7;
           movupd [eax + edi - 32], xmm2;

           movupd xmm3, [eax + edi - 16];
           addpd xmm3, xmm6;
           mulpd xmm3, xmm7;
           movupd [eax + edi - 16], xmm3;
       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @nextLine;

       @addforxloop2:
           movupd xmm0, [eax + edi];
           addpd xmm0, xmm6;
           mulpd xmm0, xmm7;
           movupd [eax + edi], xmm0;
       add edi, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last column:
       movsd xmm0, [eax];
       addsd xmm0, xmm6;
       mulsd xmm0, xmm7;
       movsd [eax], xmm0;

       // next line:
       add eax, edx;

   // loop y end
   dec ebx;
   jnz @@addforyloop;

   pop edi;
   pop ebx;
end;

procedure ASMMatrixScaleAddAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : NativeInt; const dOffset, Scale : double);
asm
   push ebx;
   push edi;

   // iters
   imul ecx, -8;

   // helper registers for the mt1, mt2 and dest pointers
   sub eax, ecx;

   movddup xmm6, dOffset;
   movddup xmm7, Scale;

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
           // prefetch [eax + edi];

           // mul add:
           movapd xmm0, [eax + edi - 128];
           mulpd xmm0, xmm7;
           addpd xmm0, xmm6;
           movapd [eax + edi - 128], xmm0;

           movapd xmm1, [eax + edi - 112];
           mulpd xmm1, xmm7;
           addpd xmm1, xmm6;
           movapd [eax + edi - 112], xmm1;

           movapd xmm2, [eax + edi - 96];
           mulpd xmm2, xmm7;
           addpd xmm2, xmm6;
           movapd [eax + edi - 96], xmm2;

           movapd xmm3, [eax + edi - 80];
           mulpd xmm3, xmm7;
           addpd xmm3, xmm6;
           movapd [eax + edi - 80], xmm3;

           movapd xmm0, [eax + edi - 64];
           mulpd xmm0, xmm7;
           addpd xmm0, xmm6;
           movapd [eax + edi - 64], xmm0;

           movapd xmm1, [eax + edi - 48];
           mulpd xmm1, xmm7;
           addpd xmm1, xmm6;
           movapd [eax + edi - 48], xmm1;

           movapd xmm2, [eax + edi - 32];
           mulpd xmm2, xmm7;
           addpd xmm2, xmm6;
           movapd [eax + edi - 32], xmm2;

           movapd xmm3, [eax + edi - 16];
           mulpd xmm3, xmm7;
           addpd xmm3, xmm6;
           movapd [eax + edi - 16], xmm3;
       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @nextLine;

       @addforxloop2:
           movapd xmm0, [eax + edi];
           mulpd xmm0, xmm7;
           addpd xmm0, xmm6;
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

procedure ASMMatrixScaleAddUnAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : NativeInt; const dOffset, Scale : double);
asm
   push ebx;
   push edi;

   // iters
   imul ecx, -8;

   // helper registers for the mt1, mt2 and dest pointers
   sub eax, ecx;

   movddup xmm6, dOffset;
   movddup xmm7, Scale;

   // for y := 0 to height - 1:
   mov ebx, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, ecx;
       @addforxloop:
           add edi, 128;
           jg @loopEnd;

           // mul add:
           movupd xmm0, [eax + edi - 128];
           mulpd xmm0, xmm7;
           addpd xmm0, xmm6;
           movupd [eax + edi - 128], xmm0;

           movupd xmm1, [eax + edi - 112];
           mulpd xmm1, xmm7;
           addpd xmm1, xmm6;
           movupd [eax + edi - 112], xmm1;

           movupd xmm2, [eax + edi - 96];
           mulpd xmm2, xmm7;
           addpd xmm2, xmm6;
           movupd [eax + edi - 96], xmm2;

           movupd xmm3, [eax + edi - 80];
           mulpd xmm3, xmm7;
           addpd xmm3, xmm6;
           movupd [eax + edi - 80], xmm3;

           movupd xmm0, [eax + edi - 64];
           mulpd xmm0, xmm7;
           addpd xmm0, xmm6;
           movupd [eax + edi - 64], xmm0;

           movupd xmm1, [eax + edi - 48];
           mulpd xmm1, xmm7;
           addpd xmm1, xmm6;
           movupd [eax + edi - 48], xmm1;

           movupd xmm2, [eax + edi - 32];
           mulpd xmm2, xmm7;
           addpd xmm2, xmm6;
           movupd [eax + edi - 32], xmm2;

           movupd xmm3, [eax + edi - 16];
           mulpd xmm3, xmm7;
           addpd xmm3, xmm6;
           movupd [eax + edi - 16], xmm3;
       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @nextLine;

       @addforxloop2:
           movupd xmm0, [eax + edi];
           mulpd xmm0, xmm7;
           addpd xmm0, xmm6;
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

procedure ASMMatrixScaleAddAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : NativeInt; const dOffset, Scale : double);
asm
   push ebx;
   push edi;

   // iters
   dec ecx;
   imul ecx, -8;

   // helper registers for the mt1, mt2 and dest pointers
   sub eax, ecx;

   movddup xmm6, dOffset;
   movddup xmm7, Scale;

   // for y := 0 to height - 1:
   mov ebx, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, ecx;
       @addforxloop:
           add edi, 128;
           jg @loopEnd;

           // mul add
           movapd xmm0, [eax + edi - 128];
           mulpd xmm0, xmm7;
           addpd xmm0, xmm6;
           movapd [eax + edi - 128], xmm0;

           movapd xmm1, [eax + edi - 112];
           mulpd xmm1, xmm7;
           addpd xmm1, xmm6;
           movapd [eax + edi - 112], xmm1;

           movapd xmm2, [eax + edi - 96];
           mulpd xmm2, xmm7;
           addpd xmm2, xmm6;
           movapd [eax + edi - 96], xmm2;

           movapd xmm3, [eax + edi - 80];
           mulpd xmm3, xmm7;
           addpd xmm3, xmm6;
           movapd [eax + edi - 80], xmm3;

           movapd xmm0, [eax + edi - 64];
           mulpd xmm0, xmm7;
           addpd xmm0, xmm6;
           movapd [eax + edi - 64], xmm0;

           movapd xmm1, [eax + edi - 48];
           mulpd xmm1, xmm7;
           addpd xmm1, xmm6;
           movapd [eax + edi - 48], xmm1;

           movapd xmm2, [eax + edi - 32];
           mulpd xmm2, xmm7;
           addpd xmm2, xmm6;
           movapd [eax + edi - 32], xmm2;

           movapd xmm3, [eax + edi - 16];
           mulpd xmm3, xmm7;
           addpd xmm3, xmm6;
           movapd [eax + edi - 16], xmm3;
       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @nextLine;

       @addforxloop2:
           movapd xmm0, [eax + edi];
           mulpd xmm0, xmm7;
           addpd xmm0, xmm6;
           movapd [eax + edi], xmm0;
       add edi, 16;
       jnz @addforxloop2;

       @nextLine:
       // special care of the last column:
       movsd xmm0, [eax];
       mulsd xmm0, xmm7;
       addsd xmm0, xmm6;
       movsd [eax], xmm0;

       // next line:
       add eax, edx;

   // loop y end
   dec ebx;
   jnz @@addforyloop;

   pop edi;
   pop ebx;
end;

procedure ASMMatrixScaleAddUnAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : NativeInt; const dOffset, Scale : double);
asm
   push ebx;
   push edi;

   // iters
   dec ecx;
   imul ecx, -8;

   // helper registers for the mt1, mt2 and dest pointers
   sub eax, ecx;

   movddup xmm6, dOffset;
   movddup xmm7, Scale;

   // for y := 0 to height - 1:
   mov ebx, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, ecx;
       @addforxloop:
           add edi, 128;
           jg @loopEnd;

           // mul add
           movupd xmm0, [eax + edi - 128];
           mulpd xmm0, xmm7;
           addpd xmm0, xmm6;
           movupd [eax + edi - 128], xmm0;

           movupd xmm1, [eax + edi - 112];
           mulpd xmm1, xmm7;
           addpd xmm1, xmm6;
           movupd [eax + edi - 112], xmm1;

           movupd xmm2, [eax + edi - 96];
           mulpd xmm2, xmm7;
           addpd xmm2, xmm6;
           movupd [eax + edi - 96], xmm2;

           movupd xmm3, [eax + edi - 80];
           mulpd xmm3, xmm7;
           addpd xmm3, xmm6;
           movupd [eax + edi - 80], xmm3;

           movupd xmm0, [eax + edi - 64];
           mulpd xmm0, xmm7;
           addpd xmm0, xmm6;
           movupd [eax + edi - 64], xmm0;

           movupd xmm1, [eax + edi - 48];
           mulpd xmm1, xmm7;
           addpd xmm1, xmm6;
           movupd [eax + edi - 48], xmm1;

           movupd xmm2, [eax + edi - 32];
           mulpd xmm2, xmm7;
           addpd xmm2, xmm6;
           movupd [eax + edi - 32], xmm2;

           movupd xmm3, [eax + edi - 16];
           mulpd xmm3, xmm7;
           addpd xmm3, xmm6;
           movupd [eax + edi - 16], xmm3;
       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @nextLine;

       @addforxloop2:
           movupd xmm0, [eax + edi];
           mulpd xmm0, xmm7;
           addpd xmm0, xmm6;
           movupd [eax + edi], xmm0;
       add edi, 16;
       jnz @addforxloop2;

       @nextLine:
       // special care of the last column:
       movsd xmm0, [eax];
       mulsd xmm0, xmm7;
       addsd xmm0, xmm6;
       movsd [eax], xmm0;

       // next line:
       add eax, edx;

   // loop y end
   dec ebx;
   jnz @@addforyloop;

   pop edi;
   pop ebx;
end;

// ###########################################
// #### Offset matrix
// ###########################################

procedure ASMMatrixElemAddAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : NativeInt; const dOffset : double);
// eax = dest, edx = lineWidt, ecx = width
asm
   push ebx;
   push edi;

   // iters
   imul ecx, -8;

   // helper registers for the mt1, mt2 and dest pointers
   sub eax, ecx;

   movddup xmm6, dOffset;

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
           // prefetch [eax + edi];

           // add scale:
           movapd xmm0, [eax + edi - 128];
           addpd xmm0, xmm6;
           movapd [eax + edi - 128], xmm0;

           movapd xmm1, [eax + edi - 112];
           addpd xmm1, xmm6;
           movapd [eax + edi - 112], xmm1;

           movapd xmm2, [eax + edi - 96];
           addpd xmm2, xmm6;
           movapd [eax + edi - 96], xmm2;

           movapd xmm3, [eax + edi - 80];
           addpd xmm3, xmm6;
           movapd [eax + edi - 80], xmm3;

           movapd xmm0, [eax + edi - 64];
           addpd xmm0, xmm6;
           movapd [eax + edi - 64], xmm0;

           movapd xmm1, [eax + edi - 48];
           addpd xmm1, xmm6;
           movapd [eax + edi - 48], xmm1;

           movapd xmm2, [eax + edi - 32];
           addpd xmm2, xmm6;
           movapd [eax + edi - 32], xmm2;

           movapd xmm3, [eax + edi - 16];
           addpd xmm3, xmm6;
           movapd [eax + edi - 16], xmm3;
       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @nextLine;

       @addforxloop2:
           movapd xmm0, [eax + edi];
           addpd xmm0, xmm6;
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

procedure ASMMatrixElemAddUnAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : NativeInt; const dOffset : double);
asm
   push ebx;
   push edi;

   // iters
   imul ecx, -8;

   // helper registers for the mt1, mt2 and dest pointers
   sub eax, ecx;

   movddup xmm6, dOffset;

   // for y := 0 to height - 1:
   mov ebx, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, ecx;
       @addforxloop:
           add edi, 128;
           jg @loopEnd;

           // add scale:
           movupd xmm0, [eax + edi - 128];
           addpd xmm0, xmm6;
           movupd [eax + edi - 128], xmm0;

           movupd xmm1, [eax + edi - 112];
           addpd xmm1, xmm6;
           movupd [eax + edi - 112], xmm1;

           movupd xmm2, [eax + edi - 96];
           addpd xmm2, xmm6;
           movupd [eax + edi - 96], xmm2;

           movupd xmm3, [eax + edi - 80];
           addpd xmm3, xmm6;
           movupd [eax + edi - 80], xmm3;

           movupd xmm0, [eax + edi - 64];
           addpd xmm0, xmm6;
           movupd [eax + edi - 64], xmm0;

           movupd xmm1, [eax + edi - 48];
           addpd xmm1, xmm6;
           movupd [eax + edi - 48], xmm1;

           movupd xmm2, [eax + edi - 32];
           addpd xmm2, xmm6;
           movupd [eax + edi - 32], xmm2;

           movupd xmm3, [eax + edi - 16];
           addpd xmm3, xmm6;
           movupd [eax + edi - 16], xmm3;
       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @nextLine;

       @addforxloop2:
           movupd xmm0, [eax + edi];
           addpd xmm0, xmm6;
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

procedure ASMMatrixElemAddAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : NativeInt; const dOffset : double);
asm
   push ebx;
   push edi;

   // iters
   dec ecx;
   imul ecx, -8;

   // helper registers for the mt1, mt2 and dest pointers
   sub eax, ecx;

   movddup xmm6, dOffset;

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
           // prefetch [eax + edi];

           // add mult:
           movapd xmm0, [eax + edi - 128];
           addpd xmm0, xmm6;
           movapd [eax + edi - 128], xmm0;

           movapd xmm1, [eax + edi - 112];
           addpd xmm1, xmm6;
           movapd [eax + edi - 112], xmm1;

           movapd xmm2, [eax + edi - 96];
           addpd xmm2, xmm6;
           movapd [eax + edi - 96], xmm2;

           movapd xmm3, [eax + edi - 80];
           addpd xmm3, xmm6;
           movapd [eax + edi - 80], xmm3;

           movapd xmm0, [eax + edi - 64];
           addpd xmm0, xmm6;
           movapd [eax + edi - 64], xmm0;

           movapd xmm1, [eax + edi - 48];
           addpd xmm1, xmm6;
           movapd [eax + edi - 48], xmm1;

           movapd xmm2, [eax + edi - 32];
           addpd xmm2, xmm6;
           movapd [eax + edi - 32], xmm2;

           movapd xmm3, [eax + edi - 16];
           addpd xmm3, xmm6;
           movapd [eax + edi - 16], xmm3;
       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @nextLine;

       @addforxloop2:
           movapd xmm0, [eax + edi];
           addpd xmm0, xmm6;
           movapd [eax + edi], xmm0;
       add edi, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last column:
       movsd xmm0, [eax];
       addsd xmm0, xmm6;
       movsd [eax], xmm0;

       // next line:
       add eax, edx;

   // loop y end
   dec ebx;
   jnz @@addforyloop;

   pop edi;
   pop ebx;
end;

procedure ASMMatrixElemAddUnAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : NativeInt; const dOffset : double);
asm
   push ebx;
   push edi;

   // iters
   dec ecx;
   imul ecx, -8;

   // helper registers for the mt1, mt2 and dest pointers
   sub eax, ecx;

   movddup xmm6, dOffset;

   // for y := 0 to height - 1:
   mov ebx, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, ecx;
       @addforxloop:
           add edi, 128;
           jg @loopEnd;

           // add mult:
           movupd xmm0, [eax + edi - 128];
           addpd xmm0, xmm6;
           movupd [eax + edi - 128], xmm0;

           movupd xmm1, [eax + edi - 112];
           addpd xmm1, xmm6;
           movupd [eax + edi - 112], xmm1;

           movupd xmm2, [eax + edi - 96];
           addpd xmm2, xmm6;
           movupd [eax + edi - 96], xmm2;

           movupd xmm3, [eax + edi - 80];
           addpd xmm3, xmm6;
           movupd [eax + edi - 80], xmm3;

           movupd xmm0, [eax + edi - 64];
           addpd xmm0, xmm6;
           movupd [eax + edi - 64], xmm0;

           movupd xmm1, [eax + edi - 48];
           addpd xmm1, xmm6;
           movupd [eax + edi - 48], xmm1;

           movupd xmm2, [eax + edi - 32];
           addpd xmm2, xmm6;
           movupd [eax + edi - 32], xmm2;

           movupd xmm3, [eax + edi - 16];
           addpd xmm3, xmm6;
           movupd [eax + edi - 16], xmm3;
       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @nextLine;

       @addforxloop2:
           movupd xmm0, [eax + edi];
           addpd xmm0, xmm6;
           movupd [eax + edi], xmm0;
       add edi, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last column:
       movsd xmm0, [eax];
       addsd xmm0, xmm6;
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
