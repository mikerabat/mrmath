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


unit ASMMoveOperations;

// #################################################
// #### SSE optimized move oprationes
// #################################################

interface

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}
{$IFNDEF x64}

uses MatrixConst;

procedure ASMMatrixCopyAlignedEvenW(Dest : PDouble; const destLineWidth : TASMNativeInt; src : PDouble; const srcLineWidth : TASMNativeInt; Width, Height : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixCopyUnAlignedEvenW(Dest : PDouble; const destLineWidth : TASMNativeInt; src : PDouble; const srcLineWidth : TASMNativeInt; Width, Height : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

procedure ASMMatrixCopyAlignedOddW(Dest : PDouble; const destLineWidth : TASMNativeInt; src : PDouble; const srcLineWidth : TASMNativeInt; Width, Height : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixCopyUnAlignedOddW(Dest : PDouble; const destLineWidth : TASMNativeInt; src : PDouble; const srcLineWidth : TASMNativeInt; Width, Height : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

procedure ASMRowSwapAlignedEvenW(A, B : PDouble; width : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMRowSwapUnAlignedEvenW(A, B : PDouble; width : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

procedure ASMRowSwapAlignedOddW(A, B : PDouble; width : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMRowSwapUnAlignedOddW(A, B : PDouble; width : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

// it is assumed that this function has multiple of sizeof(double) as numbytes
procedure ASMInitMemAligned(A : PDouble; NumBytes : TASMNativeInt; Value : double); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

{$ENDIF}

implementation

{$IFNDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$S-} {$ENDIF}

// uses non temporal moves so the cache is not poisned
// standard calling convention is register
// -> try to avoid extra stack allocations
procedure ASMInitMemAligned(A : PDouble; NumBytes : TASMNativeInt; Value : double); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
asm
   movddup xmm0, Value;

   imul edx, -1;
   sub eax, edx;

   @@loopUnrolled:
     add edx, 128;
     jg @@loopUnrolledEnd;

     movapd [eax + edx - 128], xmm0;
     movapd [eax + edx - 112], xmm0;
     movapd [eax + edx - 96], xmm0;
     movapd [eax + edx - 80], xmm0;
     movapd [eax + edx - 64], xmm0;
     movapd [eax + edx - 48], xmm0;
     movapd [eax + edx - 32], xmm0;
     movapd [eax + edx - 16], xmm0;

   jmp @@loopUnrolled;

   @@loopUnrolledEnd:

   // last few bytes
   sub edx, 128;
   jz @@exitProc;

   @@loop:
     movsd [eax + edx], xmm0;
     add edx, 8;
   jnz @@loop;

   @@exitProc:
end;

// A=eax, B=edx, width=ecx
procedure ASMRowSwapAlignedEvenW(A, B : PDouble; width : TASMNativeInt);  {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
asm
   imul ecx, -8;
   jz @endfunc;

   sub eax, ecx;
   sub edx, ecx;

   @unrolloop:
     add ecx, 64;
     jg @unrolloopend;

     movdqa xmm0, [eax + ecx - 64];
     movdqa xmm1, [edx + ecx - 64];

     movdqa [eax + ecx - 64], xmm1;
     movdqa [edx + ecx - 64], xmm0;

     movdqa xmm2, [eax + ecx - 48];
     movdqa xmm3, [edx + ecx - 48];

     movdqa [eax + ecx - 48], xmm3;
     movdqa [edx + ecx - 48], xmm2;

     movdqa xmm4, [eax + ecx - 32];
     movdqa xmm5, [edx + ecx - 32];

     movdqa [eax + ecx - 32], xmm5;
     movdqa [edx + ecx - 32], xmm4;

     movdqa xmm6, [eax + ecx - 16];
     movdqa xmm7, [edx + ecx - 16];

     movdqa [eax + ecx - 16], xmm7;
     movdqa [edx + ecx - 16], xmm6;
   jmp @unrolloop;
   @unrolloopend:

   sub ecx, 64;
   jz @endfunc;

   @loop:
     movdqa xmm0, [eax + ecx];
     movdqa xmm1, [edx + ecx];

     movdqa [eax + ecx], xmm1;
     movdqa [edx + ecx], xmm0;

     add ecx, 16;
   jnz @loop;

   @endfunc:
end;

procedure ASMRowSwapUnAlignedEvenW(A, B : PDouble; width : TASMNativeInt);
asm
   imul ecx, -8;
   jz @endfunc;

   sub eax, ecx;
   sub edx, ecx;

   @unrolloop:
     add ecx, 64;
     jg @unrolloopend;

     movdqu xmm0, [eax + ecx - 64];
     movdqu xmm1, [edx + ecx - 64];

     movdqu [eax + ecx - 64], xmm1;
     movdqu [edx + ecx - 64], xmm0;

     movdqu xmm2, [eax + ecx - 48];
     movdqu xmm3, [edx + ecx - 48];

     movdqu [eax + ecx - 48], xmm3;
     movdqu [edx + ecx - 48], xmm2;

     movdqu xmm4, [eax + ecx - 32];
     movdqu xmm5, [edx + ecx - 32];

     movdqu [eax + ecx - 32], xmm5;
     movdqu [edx + ecx - 32], xmm4;

     movdqu xmm6, [eax + ecx - 16];
     movdqu xmm7, [edx + ecx - 16];

     movdqu [eax + ecx - 16], xmm7;
     movdqu [edx + ecx - 16], xmm6;
   jmp @unrolloop;
   @unrolloopend:

   sub ecx, 64;
   jz @endfunc;

   @loop:
     movdqu xmm0, [eax + ecx];
     movdqu xmm1, [edx + ecx];

     movdqu [eax + ecx], xmm1;
     movdqu [edx + ecx], xmm0;

     add ecx, 16;
   jnz @loop;

   @endfunc:
end;

procedure ASMRowSwapAlignedOddW(A, B : PDouble; width : TASMNativeInt);
asm
   imul ecx, -8;
   jz @endfunc;

   add ecx, 8;
   jz @lastelem;

   sub eax, ecx;
   sub edx, ecx;

   @unrolloop:
     add ecx, 64;
     jg @unrolloopend;

     movdqa xmm0, [eax + ecx - 64];
     movdqa xmm1, [edx + ecx - 64];

     movdqa [eax + ecx - 64], xmm1;
     movdqa [edx + ecx - 64], xmm0;

     movdqa xmm2, [eax + ecx - 48];
     movdqa xmm3, [edx + ecx - 48];

     movdqa [eax + ecx - 48], xmm3;
     movdqa [edx + ecx - 48], xmm2;

     movdqa xmm4, [eax + ecx - 32];
     movdqa xmm5, [edx + ecx - 32];

     movdqa [eax + ecx - 32], xmm5;
     movdqa [edx + ecx - 32], xmm4;

     movdqa xmm6, [eax + ecx - 16];
     movdqa xmm7, [edx + ecx - 16];

     movdqa [eax + ecx - 16], xmm7;
     movdqa [edx + ecx - 16], xmm6;

   jmp @unrolloop;
   @unrolloopend:

   sub ecx, 64;
   jz @lastelem;


   @loop:
     movdqa xmm0, [eax + ecx];
     movdqa xmm1, [edx + ecx];

     movdqa [eax + ecx], xmm1;
     movdqa [edx + ecx], xmm0;

     add ecx, 16;
   jnz @loop;

   @lastelem:

   // last swap
   movsd xmm0, [eax];
   movsd xmm1, [edx];

   movsd [eax], xmm1;
   movsd [edx], xmm0;
   
   @endfunc:
end;

procedure ASMRowSwapUnAlignedOddW(A, B : PDouble; width : TASMNativeInt);
asm
   imul ecx, -8;
   jz @endfunc;

   add ecx, 8;
   jz @lastelem;

   sub eax, ecx;
   sub edx, ecx;

   @unrolloop:
     add ecx, 64;
     jg @unrolloopend;

     movdqu xmm0, [eax + ecx - 64];
     movdqu xmm1, [edx + ecx - 64];

     movdqu [eax + ecx - 64], xmm1;
     movdqu [edx + ecx - 64], xmm0;

     movdqu xmm2, [eax + ecx - 48];
     movdqu xmm3, [edx + ecx - 48];

     movdqu [eax + ecx - 48], xmm3;
     movdqu [edx + ecx - 48], xmm2;

     movdqu xmm4, [eax + ecx - 32];
     movdqu xmm5, [edx + ecx - 32];

     movdqu [eax + ecx - 32], xmm5;
     movdqu [edx + ecx - 32], xmm4;

     movdqu xmm6, [eax + ecx - 16];
     movdqu xmm7, [edx + ecx - 16];

     movdqu [eax + ecx - 16], xmm7;
     movdqu [edx + ecx - 16], xmm6;
   jmp @unrolloop;
   @unrolloopend:

   sub ecx, 64;
   jz @lastelem;

   @loop:
     movdqu xmm0, [eax + ecx];
     movdqu xmm1, [edx + ecx];

     movdqu [eax + ecx], xmm1;
     movdqu [edx + ecx], xmm0;

     add ecx, 16;
   jnz @loop;

   @lastelem:

   // last swap
   movsd xmm0, [eax];
   movsd xmm1, [edx];

   movsd [eax], xmm1;
   movsd [edx], xmm0;

   @endfunc:
end;

procedure ASMMatrixCopyAlignedEvenW(Dest : PDouble; const destLineWidth : TASMNativeInt; src : PDouble; const srcLineWidth : TASMNativeInt; Width, Height : TASMNativeInt);
// eax=dest, edx=destLineWidth, ecx=src
asm
   push esi;
   push edi;

   mov esi, width;
   imul esi, -8;

   // helper registers for the mt1, mt2 and dest pointers
   sub ecx, esi;
   sub eax, esi;

   // for y := 0 to height - 1:
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, esi;
       @addforxloop:
           add edi, 128;
           jg @loopEnd;

           // move:
           movdqa xmm0, [ecx + edi - 128];
           movdqa [eax + edi - 128], xmm0;

           movdqa xmm1, [ecx + edi - 112];
           movdqa [eax + edi - 112], xmm1;

           movdqa xmm2, [ecx + edi - 96];
           movdqa [eax + edi - 96], xmm2;

           movdqa xmm3, [ecx + edi - 80];
           movdqa [eax + edi - 80], xmm3;

           movdqa xmm4, [ecx + edi - 64];
           movdqa [eax + edi - 64], xmm4;

           movdqa xmm5, [ecx + edi - 48];
           movdqa [eax + edi - 48], xmm5;

           movdqa xmm6, [ecx + edi - 32];
           movdqa [eax + edi - 32], xmm6;

           movdqa xmm7, [ecx + edi - 16];
           movdqa [eax + edi - 16], xmm7;
       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @nextLine;

       @addforxloop2:
           movdqa xmm0, [ecx + edi];
           movdqa [eax + edi], xmm0;
       add edi, 16;
       jnz @addforxloop2;

       @nextLine:

       // next line:
       add ecx, srcLineWidth;
       add eax, edx;

   // loop y end
   dec height;
   jnz @@addforyloop;

   // epilog
   pop edi;
   pop esi;
end;

procedure ASMMatrixCopyUnAlignedEvenW(Dest : PDouble; const destLineWidth : TASMNativeInt; src : PDouble; const srcLineWidth : TASMNativeInt; Width, Height : TASMNativeInt);
// eax=dest, edx=destLineWidth, ecx=src
asm
   push esi;
   push edi;

   mov esi, width;
   imul esi, -8;

   // helper registers for the mt1, mt2 and dest pointers
   sub ecx, esi;
   sub eax, esi;

   // for y := 0 to height - 1:
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, esi;
       @addforxloop:
           add edi, 128;
           jg @loopEnd;

           // move:
           movdqu xmm0, [ecx + edi - 128];
           movdqu [eax + edi - 128], xmm0;

           movdqu xmm1, [ecx + edi - 112];
           movdqu [eax + edi - 112], xmm1;

           movdqu xmm2, [ecx + edi - 96];
           movdqu [eax + edi - 96], xmm2;

           movdqu xmm3, [ecx + edi - 80];
           movdqu [eax + edi - 80], xmm3;

           movdqu xmm4, [ecx + edi - 64];
           movdqu [eax + edi - 64], xmm4;

           movdqu xmm5, [ecx + edi - 48];
           movdqu [eax + edi - 48], xmm5;

           movdqu xmm6, [ecx + edi - 32];
           movdqu [eax + edi - 32], xmm6;

           movdqu xmm7, [ecx + edi - 16];
           movdqu [eax + edi - 16], xmm7;
       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @nextLine;

       @addforxloop2:
           movdqu xmm0, [ecx + edi];
           movdqu [eax + edi], xmm0;
       add edi, 16;
       jnz @addforxloop2;

       @nextLine:

       // next line:
       add ecx, srcLineWidth;
       add eax, edx;

   // loop y end
   dec height;
   jnz @@addforyloop;

   pop edi;
   pop esi;
end;

procedure ASMMatrixCopyAlignedOddW(Dest : PDouble; const destLineWidth : TASMNativeInt; src : PDouble; const srcLineWidth : TASMNativeInt; Width, Height : TASMNativeInt);
asm
   push esi;
   push edi;

   mov esi, width;
   dec esi;
   imul esi, -8;

   // helper registers for the mt1, mt2 and dest pointers
   sub ecx, esi;
   sub eax, esi;

   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, esi;
       @addforxloop:
           add edi, 128;
           jg @loopEnd;

           // addition:
           movdqa xmm0, [ecx + edi - 128];
           movdqa [eax + edi - 128], xmm0;

           movdqa xmm1, [ecx + edi - 112];
           movdqa [eax + edi - 112], xmm1;

           movdqa xmm2, [ecx + edi - 96];
           movdqa [eax + edi - 96], xmm2;

           movdqa xmm3, [ecx + edi - 80];
           movdqa [eax + edi - 80], xmm3;

           movdqa xmm4, [ecx + edi - 64];
           movdqa [eax + edi - 64], xmm4;

           movdqa xmm5, [ecx + edi - 48];
           movdqa [eax + edi - 48], xmm5;

           movdqa xmm6, [ecx + edi - 32];
           movdqa [eax + edi - 32], xmm6;

           movdqa xmm7, [ecx + edi - 16];
           movdqa [eax + edi - 16], xmm7;
       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @nextLine;

       @addforxloop2:
           movdqa xmm0, [ecx + edi];
           movdqa [eax + edi], xmm0;
       add edi, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last element
       movsd xmm0, [ecx];
       movsd [eax], xmm0;

       // next line:
       add ecx, srcLineWidth;
       add eax, edx;

   // loop y end
   dec height;
   jnz @@addforyloop;

   // epilog
   pop edi;
   pop esi;
end;

procedure ASMMatrixCopyUnAlignedOddW(Dest : PDouble; const destLineWidth : TASMNativeInt; src : PDouble; const srcLineWidth : TASMNativeInt; Width, Height : TASMNativeInt);
asm
   push esi;
   push edi;

   mov esi, width;
   dec esi;
   imul esi, -8;

   // helper registers for the mt1, mt2 and dest pointers
   sub ecx, esi;
   sub eax, esi;

   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, esi;
       @addforxloop:
           add edi, 128;
           jg @loopEnd;

           // addition:
           movdqu xmm0, [ecx + edi - 128];
           movdqu [eax + edi - 128], xmm0;

           movdqu xmm1, [ecx + edi - 112];
           movdqu [eax + edi - 112], xmm1;

           movdqu xmm2, [ecx + edi - 96];
           movdqu [eax + edi - 96], xmm2;

           movdqu xmm3, [ecx + edi - 80];
           movdqu [eax + edi - 80], xmm3;

           movdqu xmm4, [ecx + edi - 64];
           movdqu [eax + edi - 64], xmm4;

           movdqu xmm5, [ecx + edi - 48];
           movdqu [eax + edi - 48], xmm5;

           movdqu xmm6, [ecx + edi - 32];
           movdqu [eax + edi - 32], xmm6;

           movdqu xmm7, [ecx + edi - 16];
           movdqu [eax + edi - 16], xmm7;
       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @nextLine;

       @addforxloop2:
           movdqu xmm0, [ecx + edi];
           movdqu [eax + edi], xmm0;
       add edi, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last element
       movsd xmm0, [ecx];
       movsd [eax], xmm0;

       // next line:
       add ecx, srcLineWidth;
       add eax, edx;

   // loop y end
   dec height;
   jnz @@addforyloop;

   // epilog
   pop edi;
   pop esi;
end;

{$ENDIF}

end.
