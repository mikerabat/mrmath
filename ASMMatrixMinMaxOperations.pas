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


unit ASMMatrixMinMaxOperations;

interface

{$I 'mrMath_CPU.inc'}

{$IFNDEF x64}

function ASMMatrixMaxAlignedEvenW(mt : PDouble; width, height : NativeInt; const LineWidth : NativeInt) : double; {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
function ASMMatrixMaxUnAlignedEvenW(mt : PDouble; width, height : NativeInt; const LineWidth : NativeInt) : double; {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

function ASMMatrixMaxAlignedOddW(mt : PDouble; width, height : NativeInt; const LineWidth : NativeInt) : double; {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
function ASMMatrixMaxUnAlignedOddW(mt : PDouble; width, height : NativeInt; const LineWidth : NativeInt) : double; {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

function ASMMatrixMinAlignedEvenW(mt : PDouble; width, height : NativeInt; const LineWidth : NativeInt) : double; {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
function ASMMatrixMinUnAlignedEvenW(mt : PDouble; width, height : NativeInt; const LineWidth : NativeInt) : double; {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

function ASMMatrixMinAlignedOddW(mt : PDouble; width, height : NativeInt; const LineWidth : NativeInt) : double; {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
function ASMMatrixMinUnAlignedOddW(mt : PDouble; width, height : NativeInt; const LineWidth : NativeInt) : double; {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

// same as above but added Abs operation before taking max/min
function ASMMatrixAbsMaxAlignedEvenW(mt : PDouble; width, height : NativeInt; const LineWidth : NativeInt) : double; {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
function ASMMatrixAbsMaxUnAlignedEvenW(mt : PDouble; width, height : NativeInt; const LineWidth : NativeInt) : double; {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

function ASMMatrixAbsMaxAlignedOddW(mt : PDouble; width, height : NativeInt; const LineWidth : NativeInt) : double; {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
function ASMMatrixAbsMaxUnAlignedOddW(mt : PDouble; width, height : NativeInt; const LineWidth : NativeInt) : double; {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

function ASMMatrixAbsMinAlignedEvenW(mt : PDouble; width, height : NativeInt; const LineWidth : NativeInt) : double; {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
function ASMMatrixAbsMinUnAlignedEvenW(mt : PDouble; width, height : NativeInt; const LineWidth : NativeInt) : double; {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

function ASMMatrixAbsMinAlignedOddW(mt : PDouble; width, height : NativeInt; const LineWidth : NativeInt) : double; {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
function ASMMatrixAbsMinUnAlignedOddW(mt : PDouble; width, height : NativeInt; const LineWidth : NativeInt) : double; {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

procedure ASMVecMinAligned( dest : PDouble; mt : PDouble; N : NativeInt ); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMVecMinUnAligned( dest : PDouble; mt : PDOuble; N : NativeInt ); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

procedure ASMVecMaxAligned( dest : PDouble; mt : PDOuble; N : NativeInt ); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMVecMaxUnAligned( dest : PDouble; mt : PDOuble; N : NativeInt ); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

{$ENDIF}

implementation

{$IFNDEF x64}

const cLocNegMaxDouble : double = -1.7e+308;
      cLocMaxDouble : double = 1.7e+308;
      cLocAbsMask : Int64 = ($7FFFFFFFFFFFFFFF);

procedure ASMVecMinAligned( dest : PDouble; mt : PDouble; N : NativeInt ); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
asm
   imul ecx, -8;
   sub eax, ecx;
   sub edx, ecx;

   // unrolled loop
   @Loop1:
      add ecx, 64;
      jg @LoopEnd1;

      movapd xmm0, [eax + ecx - 64];
      movapd xmm1, [edx + ecx - 64];

      minpd xmm0, xmm1;
      movapd [eax + ecx - 64], xmm0;

      movapd xmm2, [eax + ecx - 48];
      movapd xmm3, [edx + ecx - 48];

      minpd xmm2, xmm3;
      movapd [eax + ecx - 48], xmm2;

      movapd xmm0, [eax + ecx - 32];
      movapd xmm1, [edx + ecx - 32];

      minpd xmm0, xmm1;
      movapd [eax + ecx - 32], xmm0;

      movapd xmm2, [eax + ecx - 16];
      movapd xmm3, [edx + ecx - 16];

      minpd xmm2, xmm3;
      movapd [eax + ecx - 16], xmm2;
   jmp @Loop1;

   @LoopEnd1:
   sub ecx, 64;
   jz @retProc;

   @Loop2:
      add ecx, 16;
      jg @LoopEnd2;

      movapd xmm0, [eax + ecx - 16];
      movapd xmm1, [edx + ecx - 16];

      minpd xmm0, xmm1;
      movapd [eax + ecx - 16], xmm0;
   jmp @Loop2;

   @LoopEnd2:

   sub ecx, 16;
   jz @retProc;

   // handle uneven last element...
   movsd xmm0, [eax - 8];
   movsd xmm1, [edx - 8];
   minsd xmm0, xmm1;
   movsd [eax - 8], xmm0;

   @retProc:
end;

procedure ASMVecMinUnaligned( dest : PDouble; mt : PDouble; N : NativeInt );
asm
   imul ecx, -8;
   sub eax, ecx;
   sub edx, ecx;

   // unrolled loop
   @Loop1:
      add ecx, 64;
      jg @LoopEnd1;

      movupd xmm0, [eax + ecx - 64];
      movupd xmm1, [edx + ecx - 64];

      minpd xmm0, xmm1;
      movupd [eax + ecx - 64], xmm0;

      movupd xmm2, [eax + ecx - 48];
      movupd xmm3, [edx + ecx - 48];

      minpd xmm2, xmm3;
      movupd [eax + ecx - 48], xmm2;

      movupd xmm0, [eax + ecx - 32];
      movupd xmm1, [edx + ecx - 32];

      minpd xmm0, xmm1;
      movupd [eax + ecx - 32], xmm0;

      movupd xmm2, [eax + ecx - 16];
      movupd xmm3, [edx + ecx - 16];

      minpd xmm2, xmm3;
      movupd [eax + ecx - 16], xmm2;
   jmp @Loop1;
   @LoopEnd1:

   sub ecx, 64;
   jz @retProc;

   @Loop2:
      add ecx, 16;
      jg @LoopEnd2;

      movupd xmm0, [eax + ecx - 16];
      movupd xmm1, [edx + ecx - 16];

      minpd xmm0, xmm1;
      movupd [eax + ecx - 16], xmm0;
   jmp @Loop2;

   @LoopEnd2:

   sub ecx, 16;
   jz @retProc;

   // handle uneven element...
   movsd xmm0, [eax - 8];
   movsd xmm1, [edx - 8];
   minsd xmm0, xmm1;
   movsd [eax - 8], xmm0;

   @retProc:
end;

procedure ASMVecMaxAligned( dest : PDouble; mt : PDOuble; N : NativeInt ); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
asm
   imul ecx, -8;
   sub eax, ecx;
   sub edx, ecx;

   // unrolled loop
   @Loop1:
      add ecx, 64;
      jg @LoopEnd1;

      movapd xmm0, [eax + ecx - 64];
      movapd xmm1, [edx + ecx - 64];

      maxpd xmm0, xmm1;
      movapd [eax + ecx - 64], xmm0;

      movapd xmm2, [eax + ecx - 48];
      movapd xmm3, [edx + ecx - 48];

      maxpd xmm2, xmm3;
      movapd [eax + ecx - 48], xmm2;

      movapd xmm0, [eax + ecx - 32];
      movapd xmm1, [edx + ecx - 32];

      maxpd xmm0, xmm1;
      movapd [eax + ecx - 32], xmm0;

      movapd xmm2, [eax + ecx - 16];
      movapd xmm3, [edx + ecx - 16];

      maxpd xmm2, xmm3;
      movapd [eax + ecx - 16], xmm2;
   jmp @Loop1;

   @LoopEnd1:
   sub ecx, 64;
   jz @retProc;

   @Loop2:
      add ecx, 16;
      jg @LoopEnd2;

      movapd xmm0, [eax + ecx - 16];
      movapd xmm1, [edx + ecx - 16];

      maxpd xmm0, xmm1;
      movapd [eax + ecx - 16], xmm0;
   jmp @Loop2;

   @LoopEnd2:

   sub ecx, 16;
   jz @retProc;

   // handle uneven last element...
   movsd xmm0, [eax - 8];
   movsd xmm1, [edx - 8];
   maxsd xmm0, xmm1;
   movsd [eax - 8], xmm0;

   @retProc:
end;

procedure ASMVecMaxUnaligned( dest : PDouble; mt : PDouble; N : NativeInt );
asm
   imul ecx, -8;
   sub eax, ecx;
   sub edx, ecx;

   // unrolled loop
   @Loop1:
      add ecx, 64;
      jg @LoopEnd1;

      movupd xmm0, [eax + ecx - 64];
      movupd xmm1, [edx + ecx - 64];

      maxpd xmm0, xmm1;
      movupd [eax + ecx - 64], xmm0;

      movupd xmm2, [eax + ecx - 48];
      movupd xmm3, [edx + ecx - 48];

      maxpd xmm2, xmm3;
      movupd [eax + ecx - 48], xmm2;

      movupd xmm0, [eax + ecx - 32];
      movupd xmm1, [edx + ecx - 32];

      maxpd xmm0, xmm1;
      movupd [eax + ecx - 32], xmm0;

      movupd xmm2, [eax + ecx - 16];
      movupd xmm3, [edx + ecx - 16];

      maxpd xmm2, xmm3;
      movupd [eax + ecx - 16], xmm2;
   jmp @Loop1;
   @LoopEnd1:

   sub ecx, 64;
   jz @retProc;

   @Loop2:
      add ecx, 16;
      jg @LoopEnd2;

      movupd xmm0, [eax + ecx - 16];
      movupd xmm1, [edx + ecx - 16];

      maxpd xmm0, xmm1;
      movupd [eax + ecx - 16], xmm0;
   jmp @Loop2;

   @LoopEnd2:

   sub ecx, 16;
   jz @retProc;

   // handle uneven element...
   movsd xmm0, [eax - 8];
   movsd xmm1, [edx - 8];
   maxsd xmm0, xmm1;
   movsd [eax - 8], xmm0;

   @retProc:
end;


function ASMMatrixMaxAlignedEvenW(mt : PDouble; width, height : NativeInt; const LineWidth : NativeInt) : double;
asm
   push edi;
        
   // iter
   imul edx, -8;
        
   // helper registers for the mt1, mt2 and dest pointers
   sub eax, edx;

   movddup xmm0, cLocNegMaxDouble;
   movapd xmm1, xmm0;

   // for y := 0 to height - 1:
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, edx;
       @addforxloop:
           add edi, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [eax + edi];

           // max:
           maxpd xmm0, [eax + edi - 128];
           maxpd xmm1, [eax + edi - 112];
           maxpd xmm0, [eax + edi - 96];
           maxpd xmm1, [eax + edi - 80];
           maxpd xmm0, [eax + edi - 64];
           maxpd xmm1, [eax + edi - 48];
           maxpd xmm0, [eax + edi - 32];
           maxpd xmm1, [eax + edi - 16];
       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @nextLine;

       @addforxloop2:
           maxpd xmm0, [eax + edi];
       add edi, 16;
       jnz @addforxloop2;

       @nextLine:

       // next line:
       add eax, LineWidth;

   // loop y end
   dec ecx;
   jnz @@addforyloop;

   // final max ->
   maxpd xmm0, xmm1;
   movhlps xmm1, xmm0;
   maxsd xmm0, xmm1;
   movsd Result, xmm0;

   pop edi;
end;

function ASMMatrixMaxUnAlignedEvenW(mt : PDouble; width, height : NativeInt; const LineWidth : NativeInt) : double;
asm
   push edi;
        
   // iter
   imul edx, -8;
        
   // helper registers for the mt1, mt2 and dest pointers
   sub eax, edx;

   movddup xmm0, cLocNegMaxDouble;
   movapd xmm3, xmm0;

   // for y := 0 to height - 1:
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, edx;
       @addforxloop:
           add edi, 128;
           jg @loopEnd;

           // max:
           movupd xmm1, [eax + edi - 128];
           maxpd xmm0, xmm1;
           movupd xmm2, [eax + edi - 112];
           maxpd xmm3, xmm2;
           movupd xmm1, [eax + edi - 96];
           maxpd xmm0, xmm1;
           movupd xmm2, [eax + edi - 80];
           maxpd xmm3, xmm2;
           movupd xmm1, [eax + edi - 64];
           maxpd xmm0, xmm1;
           movupd xmm2, [eax + edi - 48];
           maxpd xmm3, xmm2;
           movupd xmm1, [eax + edi - 32];
           maxpd xmm0, xmm1;
           movupd xmm2, [eax + edi - 16];
           maxpd xmm3, xmm2;
       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @nextLine;

       @addforxloop2:
           movupd xmm1, [eax + edi];
           maxpd xmm0, xmm1;
       add edi, 16;
       jnz @addforxloop2;

       @nextLine:

       // next line:
       add eax, LineWidth;

   // loop y end
   dec ecx;
   jnz @@addforyloop;

   // final max ->
   maxpd xmm0, xmm3;
   movhlps xmm1, xmm0;
   maxsd xmm0, xmm1;
   movsd Result, xmm0;

   pop edi;
end;

function ASMMatrixMaxAlignedOddW(mt : PDouble; width, height : NativeInt; const LineWidth : NativeInt) : double;
asm
   push edi;
        
   // iter
   dec edx;
   imul edx, -8;
        
   // helper registers for the mt1, mt2 and dest pointers
   sub eax, edx;

   movddup xmm0, cLocNegMaxDouble;
   movapd xmm1, xmm0;

   // for y := 0 to height - 1:
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, edx;
       @addforxloop:
           add edi, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [eax + edi];

           // max:
           maxpd xmm0, [eax + edi - 128];
           maxpd xmm1, [eax + edi - 112];
           maxpd xmm0, [eax + edi - 96];
           maxpd xmm1, [eax + edi - 80];
           maxpd xmm0, [eax + edi - 64];
           maxpd xmm1, [eax + edi - 48];
           maxpd xmm0, [eax + edi - 32];
           maxpd xmm1, [eax + edi - 16];
       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @nextLine;

       @addforxloop2:
           maxpd xmm0, [eax + edi];
       add edi, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last column:
       movsd xmm4, [eax];
       maxsd xmm0, xmm4;

       // next line:
       add eax, LineWidth;

   // loop y end
   dec ecx;
   jnz @@addforyloop;

   // final max ->
   maxpd xmm0, xmm1;
   movhlps xmm1, xmm0;
   maxsd xmm0, xmm1;
   movsd Result, xmm0;

   pop edi;
end;

function ASMMatrixMaxUnAlignedOddW(mt : PDouble; width, height : NativeInt; const LineWidth : NativeInt) : double;
asm
   push edi;
        
   // iter
   dec edx;
   imul edx, -8;
        
   // helper registers for the mt1, mt2 and dest pointers
   sub eax, edx;

   movddup xmm0, cLocNegMaxDouble;
   movapd xmm3, xmm0;

   // for y := 0 to height - 1:
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, edx;
       @addforxloop:
           add edi, 128;
           jg @loopEnd;

           // max:
           movupd xmm1, [eax + edi - 128];
           Maxpd xmm0, xmm1;
           movupd xmm2, [eax + edi - 112];
           Maxpd xmm3, xmm2;
           movupd xmm1, [eax + edi - 96];
           Maxpd xmm0, xmm1;
           movupd xmm2, [eax + edi - 80];
           Maxpd xmm3, xmm2;
           movupd xmm1, [eax + edi - 64];
           Maxpd xmm0, xmm1;
           movupd xmm2, [eax + edi - 48];
           Maxpd xmm3, xmm2;
           movupd xmm1, [eax + edi - 32];
           Maxpd xmm0, xmm1;
           movupd xmm2, [eax + edi - 16];
           Maxpd xmm3, xmm2;
       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @nextLine;

       @addforxloop2:
           movupd xmm1, [eax + edi];
           Maxpd xmm0, xmm1;
       add edi, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last column:
       movsd xmm1, [eax];
       Maxsd xmm0, xmm1;

       // next line:
       add eax, LineWidth;

   // loop y end
   dec ecx;
   jnz @@addforyloop;

   // final Max ->
   maxpd xmm0, xmm3;
   movhlps xmm1, xmm0;
   maxsd xmm0, xmm1;
   movsd Result, xmm0;

   pop edi;
end;

function ASMMatrixMinAlignedEvenW(mt : PDouble; width, height : NativeInt; const LineWidth : NativeInt) : double;
asm
   push edi;
        
   // iter
   imul edx, -8;
        
   // helper registers for the mt1, mt2 and dest pointers
   sub eax, edx;

   movddup xmm0, cLocMaxDouble;
   movapd xmm1, xmm0;

   // for y := 0 to height - 1:
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, edx;
       @addforxloop:
           add edi, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [eax + edi];

           // min:
           minpd xmm0, [eax + edi - 128];
           minpd xmm1, [eax + edi - 112];
           minpd xmm0, [eax + edi - 96];
           minpd xmm1, [eax + edi - 80];
           minpd xmm0, [eax + edi - 64];
           minpd xmm1, [eax + edi - 48];
           minpd xmm0, [eax + edi - 32];
           minpd xmm1, [eax + edi - 16];
       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @nextLine;

       @addforxloop2:
           minpd xmm0, [eax + edi];
       add edi, 16;
       jnz @addforxloop2;

       @nextLine:

       // next line:
       add eax, LineWidth;

   // loop y end
   dec ecx;
   jnz @@addforyloop;

   // final min ->
   minpd xmm0, xmm1;
   movhlps xmm1, xmm0;
   minsd xmm0, xmm1;
   movsd Result, xmm0;

   pop edi;
end;

function ASMMatrixMinUnAlignedEvenW(mt : PDouble; width, height : NativeInt; const LineWidth : NativeInt) : double;
asm
   push edi;
        
   // iter
   imul edx, -8;
        
   // helper registers for the mt1, mt2 and dest pointers
   sub eax, edx;

   movddup xmm0, cLocMaxDouble;
   movapd xmm3, xmm0;

   // for y := 0 to height - 1:
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, edx;
       @addforxloop:
           add edi, 128;
           jg @loopEnd;

           // min:
           movupd xmm1, [eax + edi - 128];
           minpd xmm0, xmm1;
           movupd xmm2, [eax + edi - 112];
           minpd xmm3, xmm2;
           movupd xmm1, [eax + edi - 96];
           minpd xmm0, xmm1;
           movupd xmm2, [eax + edi - 80];
           minpd xmm3, xmm2;
           movupd xmm1, [eax + edi - 64];
           minpd xmm0, xmm1;
           movupd xmm2, [eax + edi - 48];
           minpd xmm3, xmm2;
           movupd xmm1, [eax + edi - 32];
           minpd xmm0, xmm1;
           movupd xmm2, [eax + edi - 16];
           minpd xmm3, xmm2;
       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @nextLine;

       @addforxloop2:
           movupd xmm1, [eax + edi];
           minpd xmm0, xmm1;
       add edi, 16;
       jnz @addforxloop2;

       @nextLine:

       // next line:
       add eax, LineWidth;

   // loop y end
   dec ecx;
   jnz @@addforyloop;

   // final min ->
   minpd xmm0, xmm3;
   movhlps xmm1, xmm0;
   minsd xmm0, xmm1;
   movsd Result, xmm0;
   
   pop edi;
end;

function ASMMatrixMinAlignedOddW(mt : PDouble; width, height : NativeInt; const LineWidth : NativeInt) : double;
asm
   push edi;
        
   // iter
   dec edx;
   imul edx, -8;
        
   sub eax, edx;

   movddup xmm0, cLocMaxDouble;
   movapd xmm1, xmm0;

   // for y := 0 to height - 1:
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, edx;
       @addforxloop:
           add edi, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [eax + edi];

           // min:
           minpd xmm0, [eax + edi - 128];
           minpd xmm1, [eax + edi - 112];
           minpd xmm0, [eax + edi - 96];
           minpd xmm1, [eax + edi - 80];
           minpd xmm0, [eax + edi - 64];
           minpd xmm1, [eax + edi - 48];
           minpd xmm0, [eax + edi - 32];
           minpd xmm1, [eax + edi - 16];
       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @nextLine;

       @addforxloop2:
           minpd xmm0, [eax + edi];
       add edi, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last column:
       movsd xmm4, [eax];
       minsd xmm0, xmm4;

       // next line:
       add eax, LineWidth;

   // loop y end
   dec ecx;
   jnz @@addforyloop;

   // final Max ->
   minpd xmm0, xmm1;
   movhlps xmm1, xmm0;
   minsd xmm0, xmm1;
   movsd Result, xmm0;

   pop edi;
end;

function ASMMatrixMinUnAlignedOddW(mt : PDouble; width, height : NativeInt; const LineWidth : NativeInt) : double;
asm
   push edi;
        
   // iter
   dec edx;
   imul edx, -8;

   sub eax, edx;
   
   movddup xmm0, cLocMaxDouble;
   movapd xmm3, xmm0;

   // for y := 0 to height - 1:
   mov ecx, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, edx;
       @addforxloop:
           add edi, 128;
           jg @loopEnd;

           // min:
           movupd xmm1, [eax + edi - 128];
           minpd xmm0, xmm1;
           movupd xmm2, [eax + edi - 112];
           minpd xmm3, xmm2;
           movupd xmm1, [eax + edi - 96];
           minpd xmm0, xmm1;
           movupd xmm2, [eax + edi - 80];
           minpd xmm3, xmm2;
           movupd xmm1, [eax + edi - 64];
           minpd xmm0, xmm1;
           movupd xmm2, [eax + edi - 48];
           minpd xmm3, xmm2;
           movupd xmm1, [eax + edi - 32];
           minpd xmm0, xmm1;
           movupd xmm2, [eax + edi - 16];
           minpd xmm3, xmm2;
       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @nextLine;

       @addforxloop2:
           movupd xmm1, [eax + edi];
           minpd xmm0, xmm1;
       add edi, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last column:
       movsd xmm1, [eax];
       minsd xmm0, xmm1;

       // next line:
       add eax, LineWidth;

   // loop y end
   dec ecx;
   jnz @@addforyloop;

   // final min ->
   minpd xmm0, xmm3;
   movhlps xmm1, xmm0;
   minsd xmm0, xmm1;
   movsd Result, xmm0;

   pop edi;
end;

// ###########################################
// #### abs max/min operations
// ###########################################

function ASMMatrixAbsMaxAlignedEvenW(mt : PDouble; width, height : NativeInt; const LineWidth : NativeInt) : double;
asm
   push edi;

   // iter
   imul edx, -8;

   // helper registers for the mt1, mt2 and dest pointers
   sub eax, edx;

   movddup xmm0, cLocNegMaxDouble;
   movapd xmm1, xmm0;
   movddup xmm4, cLocAbsMask;

   // for y := 0 to height - 1:
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, edx;
       @addforxloop:
           add edi, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [eax + edi];

           // max:
           movapd xmm2, [eax + edi - 128];
           andpd xmm2, xmm4;
           maxpd xmm0, xmm2;
           movapd xmm3, [eax + edi - 112];
           andpd xmm3, xmm4;
           maxpd xmm1, xmm3;
           movapd xmm2, [eax + edi - 96];
           andpd xmm2, xmm4;
           maxpd xmm0, xmm2;
           movapd xmm3, [eax + edi - 80];
           andpd xmm3, xmm4;
           maxpd xmm1, xmm3;
           movapd xmm2, [eax + edi - 64];
           andpd xmm2, xmm4;
           maxpd xmm0, xmm2;
           movapd xmm3, [eax + edi - 48];
           andpd xmm3, xmm4;
           maxpd xmm1, xmm3;
           movapd xmm2, [eax + edi - 32];
           andpd xmm2, xmm4;
           maxpd xmm0, xmm2;
           movapd xmm3, [eax + edi - 16];
           andpd xmm3, xmm4;
           maxpd xmm1, xmm3;
       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @nextLine;

       @addforxloop2:
           movapd xmm2, [eax + edi];
           andpd xmm2, xmm4;
           maxpd xmm0, xmm2;
       add edi, 16;
       jnz @addforxloop2;

       @nextLine:

       // next line:
       add eax, LineWidth;

   // loop y end
   dec ecx;
   jnz @@addforyloop;

   // final max ->
   maxpd xmm0, xmm1;
   movhlps xmm1, xmm0;
   maxsd xmm0, xmm1;
   movsd Result, xmm0;

   pop edi;
end;

function ASMMatrixAbsMaxUnAlignedEvenW(mt : PDouble; width, height : NativeInt; const LineWidth : NativeInt) : double;
asm
   push edi;

   // iter
   imul edx, -8;

   // helper registers for the mt1, mt2 and dest pointers
   sub eax, edx;

   movddup xmm0, cLocNegMaxDouble;
   movapd xmm1, xmm0;
   movddup xmm4, cLocAbsMask;

   // for y := 0 to height - 1:
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, edx;
       @addforxloop:
           add edi, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [eax + edi];

           // max:
           movupd xmm2, [eax + edi - 128];
           andpd xmm2, xmm4;
           maxpd xmm0, xmm2;
           movupd xmm3, [eax + edi - 112];
           andpd xmm3, xmm4;
           maxpd xmm1, xmm3;
           movupd xmm2, [eax + edi - 96];
           andpd xmm2, xmm4;
           maxpd xmm0, xmm2;
           movupd xmm3, [eax + edi - 80];
           andpd xmm3, xmm4;
           maxpd xmm1, xmm3;
           movupd xmm2, [eax + edi - 64];
           andpd xmm2, xmm4;
           maxpd xmm0, xmm2;
           movupd xmm3, [eax + edi - 48];
           andpd xmm3, xmm4;
           maxpd xmm1, xmm3;
           movupd xmm2, [eax + edi - 32];
           andpd xmm2, xmm4;
           maxpd xmm0, xmm2;
           movupd xmm3, [eax + edi - 16];
           andpd xmm3, xmm4;
           maxpd xmm1, xmm3;
       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @nextLine;

       @addforxloop2:
           movupd xmm2, [eax + edi];
           andpd xmm2, xmm4;
           maxpd xmm0, xmm2;
       add edi, 16;
       jnz @addforxloop2;

       @nextLine:

       // next line:
       add eax, LineWidth;

   // loop y end
   dec ecx;
   jnz @@addforyloop;

   // final max ->
   maxpd xmm0, xmm1;
   movhlps xmm1, xmm0;
   maxsd xmm0, xmm1;
   movsd Result, xmm0;

   pop edi;
end;

function ASMMatrixAbsMaxAlignedOddW(mt : PDouble; width, height : NativeInt; const LineWidth : NativeInt) : double;
asm
   push edi;

   // iter
   dec edx;
   imul edx, -8;

   // helper registers for the mt1, mt2 and dest pointers
   sub eax, edx;

   movddup xmm0, cLocNegMaxDouble;
   movapd xmm1, xmm0;
   movddup xmm4, cLocAbsMask;

   // for y := 0 to height - 1:
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, edx;
       @addforxloop:
           add edi, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [eax + edi];

           // max:
           movapd xmm2, [eax + edi - 128];
           andpd xmm2, xmm4;
           maxpd xmm0, xmm2;
           movapd xmm3, [eax + edi - 112];
           andpd xmm3, xmm4;
           maxpd xmm1, xmm3;
           movapd xmm2, [eax + edi - 96];
           andpd xmm2, xmm4;
           maxpd xmm0, xmm2;
           movapd xmm3, [eax + edi - 80];
           andpd xmm3, xmm4;
           maxpd xmm1, xmm3;
           movapd xmm2, [eax + edi - 64];
           andpd xmm2, xmm4;
           maxpd xmm0, xmm2;
           movapd xmm3, [eax + edi - 48];
           andpd xmm3, xmm4;
           maxpd xmm1, xmm3;
           movapd xmm2, [eax + edi - 32];
           andpd xmm2, xmm4;
           maxpd xmm0, xmm2;
           movapd xmm3, [eax + edi - 16];
           andpd xmm3, xmm4;
           maxpd xmm1, xmm3;
       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @nextLine;

       @addforxloop2:
           movapd xmm2, [eax + edi];
           andpd xmm2, xmm4;
           maxpd xmm0, xmm2;
       add edi, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last column:
       movsd xmm2, [eax];
       andpd xmm2, xmm4
       maxsd xmm0, xmm2;

       // next line:
       add eax, LineWidth;

   // loop y end
   dec ecx;
   jnz @@addforyloop;

   // final max ->
   maxpd xmm0, xmm1;
   movhlps xmm1, xmm0;
   maxsd xmm0, xmm1;
   movsd Result, xmm0;

   pop edi;
end;

function ASMMatrixAbsMaxUnAlignedOddW(mt : PDouble; width, height : NativeInt; const LineWidth : NativeInt) : double;
asm
   push edi;

   // iter
   dec edx;
   imul edx, -8;

   // helper registers for the mt1, mt2 and dest pointers
   sub eax, edx;

   movddup xmm0, cLocNegMaxDouble;
   movapd xmm1, xmm0;
   movddup xmm4, cLocAbsMask;

   // for y := 0 to height - 1:
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, edx;
       @addforxloop:
           add edi, 128;
           jg @loopEnd;

           // max:
           movupd xmm2, [eax + edi - 128];
           andpd xmm2, xmm4;
           maxpd xmm0, xmm2;
           movupd xmm3, [eax + edi - 112];
           andpd xmm3, xmm4;
           maxpd xmm1, xmm3;
           movupd xmm2, [eax + edi - 96];
           andpd xmm2, xmm4;
           maxpd xmm0, xmm2;
           movupd xmm3, [eax + edi - 80];
           andpd xmm3, xmm4;
           maxpd xmm1, xmm3;
           movupd xmm2, [eax + edi - 64];
           andpd xmm2, xmm4;
           maxpd xmm0, xmm2;
           movupd xmm3, [eax + edi - 48];
           andpd xmm3, xmm4;
           maxpd xmm1, xmm3;
           movupd xmm2, [eax + edi - 32];
           andpd xmm2, xmm4;
           maxpd xmm0, xmm2;
           movupd xmm3, [eax + edi - 16];
           andpd xmm3, xmm4;
           maxpd xmm1, xmm3;
       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @nextLine;

       @addforxloop2:
           movupd xmm2, [eax + edi];
           andpd xmm2, xmm4;
           maxpd xmm0, xmm2;
       add edi, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last column:
       movsd xmm2, [eax];
       andpd xmm2, xmm4;
       maxsd xmm0, xmm2;

       // next line:
       add eax, LineWidth;

   // loop y end
   dec ecx;
   jnz @@addforyloop;

   // final Max ->
   maxpd xmm0, xmm1;
   movhlps xmm1, xmm0;
   maxsd xmm0, xmm1;
   movsd Result, xmm0;

   pop edi;
end;

function ASMMatrixAbsMinAlignedEvenW(mt : PDouble; width, height : NativeInt; const LineWidth : NativeInt) : double;
asm
   push edi;

   // iter
   imul edx, -8;

   // helper registers for the mt1, mt2 and dest pointers
   sub eax, edx;

   movddup xmm0, cLocMaxDouble;
   movapd xmm1, xmm0;
   movddup xmm4, cLocAbsMask;

   // for y := 0 to height - 1:
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, edx;
       @addforxloop:
           add edi, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [eax + edi];

           // max:
           movapd xmm2, [eax + edi - 128];
           andpd xmm2, xmm4;
           minpd xmm0, xmm2;
           movapd xmm3, [eax + edi - 112];
           andpd xmm3, xmm4;
           minpd xmm1, xmm3;
           movapd xmm2, [eax + edi - 96];
           andpd xmm2, xmm4;
           minpd xmm0, xmm2;
           movapd xmm3, [eax + edi - 80];
           andpd xmm3, xmm4;
           minpd xmm1, xmm3;
           movapd xmm2, [eax + edi - 64];
           andpd xmm2, xmm4;
           minpd xmm0, xmm2;
           movapd xmm3, [eax + edi - 48];
           andpd xmm3, xmm4;
           minpd xmm1, xmm3;
           movapd xmm2, [eax + edi - 32];
           andpd xmm2, xmm4;
           minpd xmm0, xmm2;
           movapd xmm3, [eax + edi - 16];
           andpd xmm3, xmm4;
           minpd xmm1, xmm3;
       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @nextLine;

       @addforxloop2:
           movapd xmm2, [eax + edi];
           andpd xmm2, xmm4;
           minpd xmm0, xmm2;
       add edi, 16;
       jnz @addforxloop2;

       @nextLine:

       // next line:
       add eax, LineWidth;

   // loop y end
   dec ecx;
   jnz @@addforyloop;

   // final max ->
   minpd xmm0, xmm1;
   movhlps xmm1, xmm0;
   minsd xmm0, xmm1;
   movsd Result, xmm0;

   pop edi;
end;

function ASMMatrixAbsMinUnAlignedEvenW(mt : PDouble; width, height : NativeInt; const LineWidth : NativeInt) : double;
asm
   push edi;

   // iter
   imul edx, -8;

   // helper registers for the mt1, mt2 and dest pointers
   sub eax, edx;

   movddup xmm0, cLocMaxDouble;
   movapd xmm1, xmm0;
   movddup xmm4, cLocAbsMask;

   // for y := 0 to height - 1:
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, edx;
       @addforxloop:
           add edi, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [eax + edi];

           // min:
           movupd xmm2, [eax + edi - 128];
           andpd xmm2, xmm4;
           minpd xmm0, xmm2;
           movupd xmm3, [eax + edi - 112];
           andpd xmm3, xmm4;
           minpd xmm1, xmm3;
           movupd xmm2, [eax + edi - 96];
           andpd xmm2, xmm4;
           minpd xmm0, xmm2;
           movupd xmm3, [eax + edi - 80];
           andpd xmm3, xmm4;
           minpd xmm1, xmm3;
           movupd xmm2, [eax + edi - 64];
           andpd xmm2, xmm4;
           minpd xmm0, xmm2;
           movupd xmm3, [eax + edi - 48];
           andpd xmm3, xmm4;
           minpd xmm1, xmm3;
           movupd xmm2, [eax + edi - 32];
           andpd xmm2, xmm4;
           minpd xmm0, xmm2;
           movupd xmm3, [eax + edi - 16];
           andpd xmm3, xmm4;
           minpd xmm1, xmm3;
       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @nextLine;

       @addforxloop2:
           movupd xmm2, [eax + edi];
           andpd xmm2, xmm4;
           minpd xmm0, xmm2;
       add edi, 16;
       jnz @addforxloop2;

       @nextLine:

       // next line:
       add eax, LineWidth;

   // loop y end
   dec ecx;
   jnz @@addforyloop;

   // final min ->
   minpd xmm0, xmm1;
   movhlps xmm1, xmm0;
   minsd xmm0, xmm1;
   movsd Result, xmm0;

   pop edi;
end;

function ASMMatrixAbsMinAlignedOddW(mt : PDouble; width, height : NativeInt; const LineWidth : NativeInt) : double;
asm
   push edi;

   // iter
   dec edx;
   imul edx, -8;

   // helper registers for the mt1, mt2 and dest pointers
   sub eax, edx;

   movddup xmm0, cLocMaxDouble;
   movapd xmm1, xmm0;
   movddup xmm4, cLocAbsMask;

   // for y := 0 to height - 1:
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, edx;
       @addforxloop:
           add edi, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [eax + edi];

           // min:
           movapd xmm2, [eax + edi - 128];
           andpd xmm2, xmm4;
           minpd xmm0, xmm2;
           movapd xmm3, [eax + edi - 112];
           andpd xmm3, xmm4;
           minpd xmm1, xmm3;
           movapd xmm2, [eax + edi - 96];
           andpd xmm2, xmm4;
           minpd xmm0, xmm2;
           movapd xmm3, [eax + edi - 80];
           andpd xmm3, xmm4;
           minpd xmm1, xmm3;
           movapd xmm2, [eax + edi - 64];
           andpd xmm2, xmm4;
           minpd xmm0, xmm2;
           movapd xmm3, [eax + edi - 48];
           andpd xmm3, xmm4;
           minpd xmm1, xmm3;
           movapd xmm2, [eax + edi - 32];
           andpd xmm2, xmm4;
           minpd xmm0, xmm2;
           movapd xmm3, [eax + edi - 16];
           andpd xmm3, xmm4;
           minpd xmm1, xmm3;
       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @nextLine;

       @addforxloop2:
           movapd xmm2, [eax + edi];
           andpd xmm2, xmm4;
           minpd xmm0, xmm2;
       add edi, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last column:
       movsd xmm2, [eax];
       andpd xmm2, xmm4
       minsd xmm0, xmm2;

       // next line:
       add eax, LineWidth;

   // loop y end
   dec ecx;
   jnz @@addforyloop;

   // final min ->
   minpd xmm0, xmm1;
   movhlps xmm1, xmm0;
   minsd xmm0, xmm1;
   movsd Result, xmm0;

   pop edi;
end;


function ASMMatrixAbsMinUnAlignedOddW(mt : PDouble; width, height : NativeInt; const LineWidth : NativeInt) : double;
asm
   push edi;

   // iter
   dec edx;
   imul edx, -8;

   // helper registers for the mt1, mt2 and dest pointers
   sub eax, edx;

   movddup xmm0, cLocMaxDouble;
   movapd xmm1, xmm0;
   movddup xmm4, cLocAbsMask;

   // for y := 0 to height - 1:
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, edx;
       @addforxloop:
           add edi, 128;
           jg @loopEnd;

           // min:
           movupd xmm2, [eax + edi - 128];
           andpd xmm2, xmm4;
           minpd xmm0, xmm2;
           movupd xmm3, [eax + edi - 112];
           andpd xmm3, xmm4;
           minpd xmm1, xmm3;
           movupd xmm2, [eax + edi - 96];
           andpd xmm2, xmm4;
           minpd xmm0, xmm2;
           movupd xmm3, [eax + edi - 80];
           andpd xmm3, xmm4;
           minpd xmm1, xmm3;
           movupd xmm2, [eax + edi - 64];
           andpd xmm2, xmm4;
           minpd xmm0, xmm2;
           movupd xmm3, [eax + edi - 48];
           andpd xmm3, xmm4;
           minpd xmm1, xmm3;
           movupd xmm2, [eax + edi - 32];
           andpd xmm2, xmm4;
           minpd xmm0, xmm2;
           movupd xmm3, [eax + edi - 16];
           andpd xmm3, xmm4;
           minpd xmm1, xmm3;
       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @nextLine;

       @addforxloop2:
           movupd xmm2, [eax + edi];
           andpd xmm2, xmm4;
           minpd xmm0, xmm2;
       add edi, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last column:
       movsd xmm2, [eax];
       andpd xmm2, xmm4;
       minsd xmm0, xmm2;

       // next line:
       add eax, LineWidth;

   // loop y end
   dec ecx;
   jnz @@addforyloop;

   // final min ->
   minpd xmm0, xmm1;
   movhlps xmm1, xmm0;
   minsd xmm0, xmm1;
   movsd Result, xmm0;

   pop edi;
end;

{$ENDIF}

end.
