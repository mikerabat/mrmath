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


unit ASMMatrixVectorMultOperations;

// #######################################################
// #### special routines for matrix vector multiplications.
// #######################################################

interface

{$I 'mrMath_CPU.inc'}

{$IFNDEF x64}

uses MatrixConst;

procedure ASMMatrixVectMultT(dest : PDouble; destLineWidth : NativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : NativeInt; width, height : NativeInt; alpha, beta : double); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixVectMult(dest : PDouble; destLineWidth : NativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : NativeInt; width, height : NativeInt; alpha, beta : double); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

// routines with special input layouts: LineWidthV needs to be sizeof(double)
procedure ASMMatrixVectMultEvenAlignedVAligned(dest : PDouble; destLineWidth : NativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : NativeInt; width, height : NativeInt; alpha, beta : double); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixVectMultEvenUnAlignedVAligned(dest : PDouble; destLineWidth : NativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : NativeInt; width, height : NativeInt; alpha, beta : double); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixVectMultOddUnAlignedVAligned(dest : PDouble; destLineWidth : NativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : NativeInt; width, height : NativeInt; alpha, beta : double); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

function ASMMatrixVecDotMultUneven( x : PDouble; Y : PDouble; incX : NativeInt; incY : NativeInt; N : NativeInt) : Double; {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
function ASMMatrixVecDotMultUnAligned( x : PDouble; y : PDouble; N : NativeInt ) : double; {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
function ASMMatrixVecDotMultAligned( x : PDouble; y : PDouble; N : NativeInt ) : double; {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

// destlinewidth needs to be sizeof(double)!
// no speed gain agains amsmatrixVectMultT
procedure ASMMatrixVectMultTDestVec(dest : PDouble; destLineWidth : NativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : NativeInt; width, height : NativeInt; alpha, beta : double); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

// rank1 update: A = A + alpha*X*Y' where x and y are vectors. It's assumed that y is sequential
procedure ASMRank1UpdateSeq(A : PDouble; const LineWidthA : NativeInt; width, height : NativeInt;
  X, Y : PDouble; incX, incY : NativeInt; alpha : double); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMRank1UpdateSeqAligned(A : PDouble; const LineWidthA : NativeInt; width, height : NativeInt;
  X, Y : PDouble; incX, incY : NativeInt; alpha : double); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

// same as above but it's assumed that the vector y is non sequential
procedure ASMRank1UpdateNonSeq(A : PDouble; const LineWidthA : NativeInt; width, height : NativeInt;
  X, Y : PDouble; incX, incY : NativeInt; alpha : double); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

{$ENDIF}

implementation

{$IFNDEF x64}

procedure ASMMatrixVectMult(dest : PDouble; destLineWidth : NativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : NativeInt; width, height : NativeInt; alpha, beta : double);
var iter : NativeInt;
    aDestLineWidth : NativeInt;
asm
   push ebx;
   push esi;
   push edi;

   mov edi, width;
   imul edi, -8;
   mov iter, edi;

   mov aDestLineWidth, edx;

   // for the final multiplication
   movhpd xmm6, beta;
   movlpd xmm6, alpha;

   // prepare for "Reverse loop indexing"

   mov edx, LineWidthV;
   mov esi, LineWidthMT;

   // init for y := 0 to Height div 4 - 1 do
   sub height, 4;
   js @@foryloopend;

   @@foryloop:

       // init values: // unrolled loop - 4 times
       xorpd xmm0, xmm0;  // dest^ := 0;
       xorpd xmm1, xmm1;
       xorpd xmm2, xmm2;
       xorpd xmm3, xmm3;
       mov ebx, v;        // ebx = first vector element
       mov edi, iter;

       @@forxloop:
           movsd xmm4, [ebx];

           movsd xmm5, [ecx];
           mulsd xmm5, xmm4;
           addsd xmm0, xmm5;

           movsd xmm5, [ecx + esi];
           mulsd xmm5, xmm4;
           addsd xmm1, xmm5;

           movsd xmm5, [ecx + 2*esi];
           mulsd xmm5, xmm4;
           addsd xmm2, xmm5;

           add ecx, esi;
           movsd xmm5, [ecx + 2*esi];
           mulsd xmm5, xmm4;
           addsd xmm3, xmm5;
           sub ecx, esi;

           add ebx, edx;
           add ecx, 8;
       add edi, 8;
       jnz @@forxloop;

       add ecx, iter; // undo increment

       // calculate dest = beta*dest + alpha*xmm0
       // -> 4 times since the loop is unrolled
       movhpd xmm0, [eax];
       mulpd xmm0, xmm6;
       haddpd xmm0, xmm0;
       movsd [eax], xmm0;
       add eax, adestLineWidth;
       add ecx, LineWidthMT;

       movhpd xmm1, [eax];
       mulpd xmm1, xmm6;
       haddpd xmm1, xmm1;
       movsd [eax], xmm1;
       add eax, adestLineWidth;
       add ecx, LineWidthMT;

       movhpd xmm2, [eax];
       mulpd xmm2, xmm6;
       haddpd xmm2, xmm2;
       movsd [eax], xmm2;
       add eax, adestLineWidth;
       add ecx, LineWidthMT;

       movhpd xmm3, [eax];
       mulpd xmm3, xmm6;
       haddpd xmm3, xmm3;
       movsd [eax], xmm3;
       add eax, adestLineWidth;
       add ecx, LineWidthMT;

   sub height, 4;
   jns @@foryloop;   // jump not signed...

   // ###########################################
   // #### Remaining rows (max 4):
   @@foryloopend:

   add height, 4;
   jz @@endmult;

   sub ecx, iter;
   @@foryshortloop:

       // init values: // unrolled loop - 4 times
       xorpd xmm0, xmm0;  // dest^ := 0;
       mov ebx, v;        // ebx = first vector element
       mov edi, iter;

       @@forxshortloop:
           movsd xmm4, [ebx];
           mulsd xmm4, [ecx + edi];
           addsd xmm0, xmm4;

           add ebx, edx;
       add edi, 8;
       jnz @@forxshortloop;

       // calculate dest = beta*dest + alpha*xmm0
       movhpd xmm0, [eax];
       mulpd xmm0, xmm6;
       haddpd xmm0, xmm0;
       movsd [eax], xmm0;

       // next line
       add eax, adestLineWidth;
       add ecx, LineWidthMT;

   dec height;
   jnz @@foryshortloop;

   @@endmult:

   // epilog
   pop edi;
   pop esi;
   pop ebx;
end;

procedure ASMMatrixVectMultEvenAlignedVAligned(dest : PDouble; destLineWidth : NativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : NativeInt; width, height : NativeInt; alpha, beta : double);
var iter : NativeInt;
asm
   push ebx;
   push esi;
   push edi;

   mov edi, width;
   imul edi, -8;
   mov iter, edi;

   // for the final multiplication
   movhpd xmm6, beta;
   movlpd xmm6, alpha;

   // prepare for "Reverse loop indexing"
   mov esi, LineWidthMT;

   // init for y := 0 to Height div 4 - 1 do
   sub height, 4;
   js @@foryloopend;

   @@foryloop:

       // init values: // unrolled loop - 4 times
       xorpd xmm0, xmm0;  // dest^ := 0;
       xorpd xmm1, xmm1;
       xorpd xmm2, xmm2;
       xorpd xmm3, xmm3;
       mov ebx, v;        // ebx = first vector element
       mov edi, iter;

       @@forxloop:
           movapd xmm4, [ebx];

           movapd xmm5, [ecx];
           mulpd xmm5, xmm4;
           addpd xmm0, xmm5;

           movapd xmm5, [ecx + esi];
           mulpd xmm5, xmm4;
           addpd xmm1, xmm5;

           movapd xmm5, [ecx + 2*esi];
           mulpd xmm5, xmm4;
           addpd xmm2, xmm5;

           add ecx, esi;
           movapd xmm5, [ecx + 2*esi];
           mulpd xmm5, xmm4;
           addpd xmm3, xmm5;
           sub ecx, esi;

           add ebx, 16;
           add ecx, 16;
       add edi, 16;
       jnz @@forxloop;

       add ecx, iter; // undo increment

       // calculate dest = beta*dest + alpha*xmm0
       // -> 4 times since the loop is unrolled
       movsd xmm5, [eax];
       haddpd xmm0, xmm5;
       mulpd xmm0, xmm6;
       haddpd xmm0, xmm0;
       movsd [eax], xmm0;
       add eax, edx;
       add ecx, LineWidthMT;

       movsd xmm5, [eax];
       haddpd xmm1, xmm5;
       mulpd xmm1, xmm6;
       haddpd xmm1, xmm1;
       movsd [eax], xmm1;
       add eax, edx;
       add ecx, LineWidthMT;

       movsd xmm5, [eax];
       haddpd xmm2, xmm5;
       mulpd xmm2, xmm6;
       haddpd xmm2, xmm2;
       movsd [eax], xmm2;
       add eax, edx;
       add ecx, LineWidthMT;

       movsd xmm5, [eax];
       haddpd xmm3, xmm5;
       mulpd xmm3, xmm6;
       haddpd xmm3, xmm3;
       movsd [eax], xmm3;
       add eax, edx;
       add ecx, LineWidthMT;

   sub height, 4;
   jns @@foryloop;   // jump not signed...

   // ###########################################
   // #### Remaining rows (max 4):
   @@foryloopend:

   add height, 4;
   jz @@endmult;

   sub ecx, iter;
   @@foryshortloop:

       // init values:
       xorpd xmm0, xmm0;  // dest^ := 0;
       mov ebx, v;        // ebx = first vector element
       mov edi, iter;

       @@forxshortloop:
           movapd xmm4, [ebx];
           movapd xmm5, [ecx + edi];
           mulpd xmm5, xmm4;
           addpd xmm0, xmm5;

           add ebx, 16;
       add edi, 16;
       jnz @@forxshortloop;

       // calculate dest = beta*dest + alpha*xmm0
       movsd xmm5, [eax];
       haddpd xmm0, xmm5;
       mulpd xmm0, xmm6;
       haddpd xmm0, xmm7;
       movsd [eax], xmm0;

       // next line
       add eax, edx;
       add ecx, LineWidthMT;

   dec height;
   jnz @@foryshortloop;

   @@endmult:

   // epilog
   pop edi;
   pop esi;
   pop ebx;
end;

procedure ASMMatrixVectMultEvenUnAlignedVAligned(dest : PDouble; destLineWidth : NativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : NativeInt; width, height : NativeInt; alpha, beta : double);
var iter : NativeInt;
asm
   push ebx;
   push esi;
   push edi;

   mov edi, width;
   imul edi, -8;
   mov iter, edi;

   // for the final multiplication
   movhpd xmm6, beta;
   movlpd xmm6, alpha;

   // prepare for "Reverse loop indexing"
   mov esi, LineWidthMT;

   // init for y := 0 to Height div 4 - 1 do
   sub height, 4;
   js @@foryloopend;

   @@foryloop:

       // init values: // unrolled loop - 4 times
       xorpd xmm0, xmm0;  // dest^ := 0;
       xorpd xmm1, xmm1;
       xorpd xmm2, xmm2;
       xorpd xmm3, xmm3;
       mov ebx, v;        // ebx = first vector element
       mov edi, iter;

       @@forxloop:
           movupd xmm4, [ebx];

           movupd xmm5, [ecx];
           mulpd xmm5, xmm4;
           addpd xmm0, xmm5;

           movupd xmm5, [ecx + esi];
           mulpd xmm5, xmm4;
           addpd xmm1, xmm5;

           movupd xmm5, [ecx + 2*esi];
           mulpd xmm5, xmm4;
           addpd xmm2, xmm5;

           add ecx, esi;
           movupd xmm5, [ecx + 2*esi];
           mulpd xmm5, xmm4;
           addpd xmm3, xmm5;
           sub ecx, esi;

           add ebx, 16;
           add ecx, 16;
       add edi, 16;
       jnz @@forxloop;

       add ecx, iter; // undo increment

       // calculate dest = beta*dest + alpha*xmm0
       // -> 4 times since the loop is unrolled
       movsd xmm5, [eax];
       haddpd xmm0, xmm5;
       mulpd xmm0, xmm6;
       haddpd xmm0, xmm0;
       movsd [eax], xmm0;
       add eax, edx;
       add ecx, LineWidthMT;

       movsd xmm5, [eax];
       haddpd xmm1, xmm5;
       mulpd xmm1, xmm6;
       haddpd xmm1, xmm1;
       movsd [eax], xmm1;
       add eax, edx;
       add ecx, LineWidthMT;

       movsd xmm5, [eax];
       haddpd xmm2, xmm5;
       mulpd xmm2, xmm6;
       haddpd xmm2, xmm2;
       movsd [eax], xmm2;
       add eax, edx;
       add ecx, LineWidthMT;

       movsd xmm5, [eax];
       haddpd xmm3, xmm5;
       mulpd xmm3, xmm6;
       haddpd xmm3, xmm3;
       movsd [eax], xmm3;
       add eax, edx;
       add ecx, LineWidthMT;

   sub height, 4;
   jns @@foryloop;   // jump not signed...

   // ###########################################
   // #### Remaining rows (max 4):
   @@foryloopend:

   add height, 4;
   jz @@endmult;

   sub ecx, iter;
   @@foryshortloop:

       // init values:
       xorpd xmm0, xmm0;  // dest^ := 0;
       mov ebx, v;        // ebx = first vector element
       mov edi, iter;

       @@forxshortloop:
           movupd xmm4, [ebx];
           movupd xmm5, [ecx + edi];
           mulpd xmm5, xmm4;
           addpd xmm0, xmm5;

           add ebx, 16;
       add edi, 16;
       jnz @@forxshortloop;

       // calculate dest = beta*dest + alpha*xmm0
       movsd xmm5, [eax];
       haddpd xmm0, xmm5;
       mulpd xmm0, xmm6;
       haddpd xmm0, xmm7;
       movsd [eax], xmm0;

       // next line
       add eax, edx;
       add ecx, LineWidthMT;

   dec height;
   jnz @@foryshortloop;

   @@endmult:

   // epilog
   pop edi;
   pop esi;
   pop ebx;
end;

procedure ASMMatrixVectMultOddUnAlignedVAligned(dest : PDouble; destLineWidth : NativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : NativeInt; width, height : NativeInt; alpha, beta : double);
var iter : NativeInt;
asm
   push ebx;
   push esi;
   push edi;

   mov edi, width;
   dec edi;
   imul edi, -8;
   mov iter, edi;

   // for the final multiplication
   movhpd xmm6, beta;
   movlpd xmm6, alpha;

   // prepare for "Reverse loop indexing"
   mov esi, LineWidthMT;

   // init for y := 0 to Height div 4 - 1 do
   sub height, 4;
   js @@foryloopend;

   @@foryloop:

       // init values: // unrolled loop - 4 times
       xorpd xmm0, xmm0;  // dest^ := 0;
       xorpd xmm1, xmm1;
       xorpd xmm2, xmm2;
       xorpd xmm3, xmm3;
       mov ebx, v;        // ebx = first vector element
       mov edi, iter;

       @@forxloop:
           movupd xmm4, [ebx];

           movupd xmm5, [ecx];
           mulpd xmm5, xmm4;
           addpd xmm0, xmm5;

           movupd xmm5, [ecx + esi];
           mulpd xmm5, xmm4;
           addpd xmm1, xmm5;

           movupd xmm5, [ecx + 2*esi];
           mulpd xmm5, xmm4;
           addpd xmm2, xmm5;

           add ecx, esi;
           movupd xmm5, [ecx + 2*esi];
           mulpd xmm5, xmm4;
           addpd xmm3, xmm5;
           sub ecx, esi;

           add ebx, 16;
           add ecx, 16;
       add edi, 16;
       jnz @@forxloop;

       // last element handling
       movsd xmm4, [ebx];

       movsd xmm5, [ecx];
       mulsd xmm5, xmm4;
       addsd xmm0, xmm5;

       movsd xmm5, [ecx + esi];
       mulsd xmm5, xmm4;
       addsd xmm1, xmm5;

       movsd xmm5, [ecx + 2*esi];
       mulsd xmm5, xmm4;
       addsd xmm2, xmm5;

       add ecx, esi;
       movsd xmm5, [ecx + 2*esi];
       mulsd xmm5, xmm4;
       addsd xmm3, xmm5;
       sub ecx, esi;

       add ecx, iter; // undo increment

       // calculate dest = beta*dest + alpha*xmm0
       // -> 4 times since the loop is unrolled
       movsd xmm5, [eax];
       haddpd xmm0, xmm5;
       mulpd xmm0, xmm6;
       haddpd xmm0, xmm0;
       movsd [eax], xmm0;
       add eax, edx;
       add ecx, LineWidthMT;

       movsd xmm5, [eax];
       haddpd xmm1, xmm5;
       mulpd xmm1, xmm6;
       haddpd xmm1, xmm1;
       movsd [eax], xmm1;
       add eax, edx;
       add ecx, LineWidthMT;

       movsd xmm5, [eax];
       haddpd xmm2, xmm5;
       mulpd xmm2, xmm6;
       haddpd xmm2, xmm2;
       movsd [eax], xmm2;
       add eax, edx;
       add ecx, LineWidthMT;

       movsd xmm5, [eax];
       haddpd xmm3, xmm5;
       mulpd xmm3, xmm6;
       haddpd xmm3, xmm3;
       movsd [eax], xmm3;
       add eax, edx;
       add ecx, LineWidthMT;

   sub height, 4;
   jns @@foryloop;   // jump not signed...

   // ###########################################
   // #### Remaining rows (max 4):
   @@foryloopend:

   add height, 4;
   jz @@endmult;

   sub ecx, iter;
   @@foryshortloop:

       // init values:
       xorpd xmm0, xmm0;  // dest^ := 0;
       mov ebx, v;        // ebx = first vector element
       mov edi, iter;

       @@forxshortloop:
           movupd xmm4, [ebx];
           movupd xmm5, [ecx + edi];
           mulpd xmm5, xmm4;
           addpd xmm0, xmm5;

           add ebx, 16;
       add edi, 16;
       jnz @@forxshortloop;

       // handle last element
       movsd xmm4, [ebx];
       movsd xmm5, [ecx];
       mulsd xmm5, xmm4;
       addsd xmm0, xmm5;

       // calculate dest = beta*dest + alpha*xmm0
       movsd xmm5, [eax];
       haddpd xmm0, xmm5;
       mulpd xmm0, xmm6;
       haddpd xmm0, xmm7;
       movsd [eax], xmm0;

       // next line
       add eax, edx;
       add ecx, LineWidthMT;

   dec height;
   jnz @@foryshortloop;

   @@endmult:

   // epilog
   pop edi;
   pop esi;
   pop ebx;
end;

// optimized transposed function
// -> does 16 operations in one loop
// (basically an unrolled simd version)
procedure ASMMatrixVectMultT(dest : PDouble; destLineWidth : NativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : NativeInt; width, height : NativeInt; alpha, beta : double);
// eax = dest, destLineWidth = edx, mt1 = ecx
var res0, res1, res2, res3,
    res4, res5, res6, res7 : Array[0..1] of  double;
    aDestLineWidth : NativeInt;
    aMt1 : PDouble;
asm
   push ebx;
   push esi;
   push edi;

   // if width = 0 or height = 0 then exit
   mov edi, width;
   or edi, height;
   cmp edi, 0;
   je @@vecaddend;

   mov aDestLineWidth, edx;
   mov aMt1, ecx;
   //mov eax, dest;

   // for the final multiplication
   movddup xmm6, alpha;
   movddup xmm7, beta;

   // prepare for loop
   mov esi, LineWidthMT;
   mov edx, LineWidthV;

   // ###################################
   // #### unrolled loop (4 times)

   // init for y := 0 to Height div 4 - 1 do

   sub width, 16;
   js @@forxloopend;

   @@forxloop:

       xorpd xmm0, xmm0;  // two elements
       // clear out res
       movupd res0, xmm0;
       movupd res1, xmm0;
       movupd res2, xmm0;
       movupd res3, xmm0;
       movupd res4, xmm0;
       movupd res5, xmm0;
       movupd res6, xmm0;
       movupd res7, xmm0;

       mov ebx, v;        // ebx = first vector element
       mov ecx, amt1;

       mov edi, height;
       @@foryloop:
           movddup xmm3, [ebx];

           movupd xmm4, [ecx];
           movupd xmm0, res0;
           mulpd xmm4, xmm3;
           addpd xmm0, xmm4;
           movupd res0, xmm0;

           movupd xmm4, [ecx + 16];
           movupd xmm0, res1;
           mulpd xmm4, xmm3;
           addpd xmm0, xmm4;
           movupd res1, xmm0;

           movupd xmm4, [ecx + 32];
           movupd xmm0, res2;
           mulpd xmm4, xmm3;
           addpd xmm0, xmm4;
           movupd res2, xmm0;

           movupd xmm4, [ecx + 48];
           movupd xmm0, res3;
           mulpd xmm4, xmm3;
           addpd xmm0, xmm4;
           movupd res3, xmm0;

           movupd xmm4, [ecx + 64];
           movupd xmm0, res4;
           mulpd xmm4, xmm3;
           addpd xmm0, xmm4;
           movupd res4, xmm0;

           movupd xmm4, [ecx + 80];
           movupd xmm0, res5;
           mulpd xmm4, xmm3;
           addpd xmm0, xmm4;
           movupd res5, xmm0;

           movupd xmm4, [ecx + 96];
           movupd xmm0, res6;
           mulpd xmm4, xmm3;
           addpd xmm0, xmm4;
           movupd res6, xmm0;

           movupd xmm4, [ecx + 112];
           movupd xmm0, res7;
           mulpd xmm4, xmm3;
           addpd xmm0, xmm4;
           movupd res7, xmm0;

           add ecx, esi;
           add ebx, edx;

       dec edi;
       jnz @@foryloop;

       // build result of 16 values:
       mov ebx, adestLineWidth;

       // first two
       movsd xmm3, [eax];
       movsd xmm4, [eax + ebx];
       movlhps xmm3, xmm4;

       movupd xmm0, res0;
       mulpd xmm0, xmm6;   // alpha*res
       mulpd xmm3, xmm7;   // dest*beta

       addpd xmm3, xmm0;
       movhlps xmm4, xmm3;
       movsd [eax], xmm3;
       movsd [eax + ebx], xmm4;
       add eax, ebx;
       add eax, ebx;

       // second two
       movsd xmm3, [eax];
       movsd xmm4, [eax + ebx];
       movlhps xmm3, xmm4;

       movupd xmm0, res1;
       mulpd xmm0, xmm6;   // alpha*res
       mulpd xmm3, xmm7;   // dest*beta

       addpd xmm3, xmm0;
       movhlps xmm4, xmm3;
       movsd [eax], xmm3;
       movsd [eax + ebx], xmm4;

       add eax, ebx;
       add eax, ebx;

       // third two
       movsd xmm3, [eax];
       movsd xmm4, [eax + ebx];
       movlhps xmm3, xmm4;

       movupd xmm0, res2;
       mulpd xmm0, xmm6;   // alpha*res
       mulpd xmm3, xmm7;   // dest*beta

       addpd xmm3, xmm0;
       movhlps xmm4, xmm3;
       movsd [eax], xmm3;
       movsd [eax + ebx], xmm4;

       add eax, ebx;
       add eax, ebx;

       // forth two
       movsd xmm3, [eax];
       movsd xmm4, [eax + ebx];
       movlhps xmm3, xmm4;

       movupd xmm0, res3;
       mulpd xmm0, xmm6;   // alpha*res
       mulpd xmm3, xmm7;   // dest*beta

       addpd xmm3, xmm0;
       movhlps xmm4, xmm3;
       movsd [eax], xmm3;
       movsd [eax + ebx], xmm4;

       add eax, ebx;
       add eax, ebx;

       // fith two
       movsd xmm3, [eax];
       movsd xmm4, [eax + ebx];
       movlhps xmm3, xmm4;

       movupd xmm0, res4;
       mulpd xmm0, xmm6;   // alpha*res
       mulpd xmm3, xmm7;   // dest*beta

       addpd xmm3, xmm0;
       movhlps xmm4, xmm3;
       movsd [eax], xmm3;
       movsd [eax + ebx], xmm4;

       add eax, ebx;
       add eax, ebx;

       // sixth two
       movsd xmm3, [eax];
       movsd xmm4, [eax + ebx];
       movlhps xmm3, xmm4;

       movupd xmm0, res5;
       mulpd xmm0, xmm6;   // alpha*res
       mulpd xmm3, xmm7;   // dest*beta

       addpd xmm3, xmm0;
       movhlps xmm4, xmm3;
       movsd [eax], xmm3;
       movsd [eax + ebx], xmm4;

       add eax, ebx;
       add eax, ebx;

       // seventh two
       movsd xmm3, [eax];
       movsd xmm4, [eax + ebx];
       movlhps xmm3, xmm4;

       movupd xmm0, res6;
       mulpd xmm0, xmm6;   // alpha*res
       mulpd xmm3, xmm7;   // dest*beta

       addpd xmm3, xmm0;
       movhlps xmm4, xmm3;
       movsd [eax], xmm3;
       movsd [eax + ebx], xmm4;

       add eax, ebx;
       add eax, ebx;

       // eighth two
       movsd xmm3, [eax];
       movsd xmm4, [eax + ebx];
       movlhps xmm3, xmm4;

       movupd xmm0, res7;
       mulpd xmm0, xmm6;   // alpha*res
       mulpd xmm3, xmm7;   // dest*beta

       addpd xmm3, xmm0;
       movhlps xmm4, xmm3;
       movsd [eax], xmm3;
       movsd [eax + ebx], xmm4;

       add eax, ebx;
       add eax, ebx;

       // next loop element
       add amt1, 16*8; // inc(mt1, 16)
       sub width, 16;

       jns @@forxloop;

   @@forxloopend:

   // ###########################################
   // elements that not fit into mod 32
   add width, 16;
   jz @@vecaddend;

   @@forxshortloop:
       xorpd xmm0, xmm0;  // first two elements

       mov ebx, v;        // ebx = first vector element
       mov ecx, amt1;

       mov edi, height;
       @@forshortyloop:
           movsd xmm3, [ebx];
           movsd xmm4, [ecx];

           mulsd xmm4, xmm3;
           addsd xmm0, xmm4;

           add ecx, esi;
           add ebx, edx;

       dec edi;
       jnz @@forshortyloop;

       mulsd xmm0, xmm6;  // alpha*res

       movsd xmm3, [eax];
       mulsd xmm3, xmm7;  //dest*beta
       addsd xmm0, xmm3;
       movsd [eax], xmm0;

       // next row
       add eax, adestLineWidth;
       add amt1, 8;

   dec width;
   jnz @@forxshortloop;

   @@vecaddend:

   // ###########################################
   // #### epilog
   pop edi;
   pop esi;
   pop ebx;
end;

{no speed gain}
procedure ASMMatrixVectMultTDestVec(dest : PDouble; destLineWidth : NativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : NativeInt; width, height : NativeInt; alpha, beta : double);
// eax = dest, edx = destLineWidth, ecx = mt1
var aDestLineWidth : NativeInt;
asm
   push ebx;
   push esi;
   push edi;

   mov aDestLineWidth, edx;

   // if width = 0 or height = 0 then exit;
   mov ebx, height;
   or ebx, width;
   jz @@vecaddend;

   // for the final multiplication
   movddup xmm6, alpha;
   movddup xmm7, beta;

   // prepare for loop
   mov esi, LineWidthMT;
   mov edx, LineWidthV;

   // ###################################
   // #### unrolled loop (4 times)

   // init for y := 0 to Height div 16 - 1 do

   sub width, 16;
   js @@forxloopend;

   @@forxloop:

       // dest*beta
       movupd xmm0, [eax];
       mulpd xmm0, xmm7;
       movupd [eax], xmm0;
       movupd xmm0, [eax + 16];
       mulpd xmm0, xmm7;
       movupd [eax + 16], xmm0;
       movupd xmm0, [eax + 32];
       mulpd xmm0, xmm7;
       movupd [eax + 32], xmm0;
       movupd xmm0, [eax + 48];
       mulpd xmm0, xmm7;
       movupd [eax + 48], xmm0;
       movupd xmm0, [eax + 64];
       mulpd xmm0, xmm7;
       movupd [eax + 64], xmm0;
       movupd xmm0, [eax + 80];
       mulpd xmm0, xmm7;
       movupd [eax + 80], xmm0;
       movupd xmm0, [eax + 96];
       mulpd xmm0, xmm7;
       movupd [eax + 96], xmm0;
       movupd xmm0, [eax + 112];
       mulpd xmm0, xmm7;
       movupd [eax + 112], xmm0;

       mov ebx, v;        // ebx = first vector element
       push ecx;

       mov edi, height;
       @@foryloop:
           movddup xmm3, [ebx];
           mulpd xmm3, xmm6;   // alpha

           movupd xmm4, [ecx];
           movupd xmm0, [eax];
           mulpd xmm4, xmm3;
           addpd xmm0, xmm4;
           movupd [eax], xmm0;

           movupd xmm4, [ecx + 16];
           movupd xmm0, [eax + 16];
           mulpd xmm4, xmm3;
           addpd xmm0, xmm4;
           movupd [eax + 16], xmm0;

           movupd xmm4, [ecx + 32];
           movupd xmm0, [eax + 32];
           mulpd xmm4, xmm3;
           addpd xmm0, xmm4;
           movupd [eax + 32], xmm0;

           movupd xmm4, [ecx + 48];
           movupd xmm0, [eax + 48];
           mulpd xmm4, xmm3;
           addpd xmm0, xmm4;
           movupd [eax + 48], xmm0;

           movupd xmm4, [ecx + 64];
           movupd xmm0, [eax + 64];
           mulpd xmm4, xmm3;
           addpd xmm0, xmm4;
           movupd [eax + 64], xmm0;

           movupd xmm4, [ecx + 80];
           movupd xmm0, [eax + 80];
           mulpd xmm4, xmm3;
           addpd xmm0, xmm4;
           movupd [eax + 80], xmm0;

           movupd xmm4, [ecx + 96];
           movupd xmm0, [eax + 96];
           mulpd xmm4, xmm3;
           addpd xmm0, xmm4;
           movupd [eax + 96], xmm0;

           movupd xmm4, [ecx + 112];
           movupd xmm0, [eax + 112];
           mulpd xmm4, xmm3;
           addpd xmm0, xmm4;
           movupd [eax + 112], xmm0;

           add ecx, esi;
           add ebx, edx;

       dec edi;
       jnz @@foryloop;

       // next loop element
       pop ecx;
       add eax, 16*8;
       add ecx, 16*8; // inc(mt1, 16)
       sub width, 16;

       jns @@forxloop;

   @@forxloopend:

   // ###########################################
   // elements that not fit into mod 32
   add width, 16;
   jz @@vecaddend;

   @@forxshortloop:
       xorpd xmm0, xmm0;  // clear out accumulator

       mov ebx, v;        // ebx = first vector element
       push ecx;

       mov edi, height;
       @@forshortyloop:
           movsd xmm3, [ebx];
           movsd xmm4, [ecx];

           mulsd xmm4, xmm3;
           addsd xmm0, xmm4;

           add ecx, esi;
           add ebx, edx;

       dec edi;
       jnz @@forshortyloop;

       mulsd xmm0, xmm6;  // alpha*res

       movsd xmm3, [eax];
       mulsd xmm3, xmm7;  //dest*beta
       addsd xmm0, xmm3;
       movsd [eax], xmm0;

       // next row
       pop ecx;
       add eax, aDestLineWidth;
       add ecx, 8;

   dec width;
   jnz @@forxshortloop;

   @@vecaddend:

   // ###########################################
   // #### epilog
   pop edi;
   pop esi;
   pop ebx;
end;

// simple routine.. not used any more
procedure ASMMatrixVectMultT1(dest : PDouble; destLineWidth : NativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : NativeInt; width, height : NativeInt; alpha, beta : double);
begin
     asm
        push ebx;
        push esi;
        push edi;

        mov edi, dest;

        // for the final multiplication
        movhpd xmm6, beta;
        movlpd xmm6, alpha;

        // prepare for loop
        mov esi, LineWidthMT;
        mov edx, LineWidthV;

        // init for x := 0 to width - 1:
        @@forxloop:

            // init values:
            xorpd xmm0, xmm0;  // res := 0;
            mov ebx, v;        // ebx = first vector element
            mov eax, mt1;

            mov ecx, height;
            @@foryloop:
                movsd xmm1, [eax];
                movsd xmm2, [ebx];

                mulsd xmm1, xmm2;
                addsd xmm0, xmm1;

                add eax, esi;
                add ebx, edx;

            dec ecx;
            jnz @@foryloop;

            // result building
            // write back result (final addition and compactation)

            // calculate dest = beta*dest + alpha*xmm0
            movhpd xmm0, [edi];

            mulpd xmm0, xmm6;
            haddpd xmm0, xmm0;

            movsd [edi], xmm0;

            // next results:
            add edi, destLineWidth;
            add mt1, 8;
        dec width;
        jnz @@forxloop;

        // epilog
        pop edi;
        pop esi;
        pop ebx;
     end;
end;

procedure ASMRank1UpdateSeq(A : PDouble; const LineWidthA : NativeInt; width, height : NativeInt;
  X, Y : PDouble; incX, incY : NativeInt; alpha : double);
var wd2 : NativeInt;
    aWidth : NativeInt;
// performs A = A + alpha*X*Y' in row major form
asm
   push ebx;
   push edi;
   push esi;

   mov aWidth, ecx;
   shr ecx, 1;
   mov wd2, ecx;
        
   mov edi, X;

   // for the temp calculation
   movddup xmm7, alpha;

   // prepare for loop

   // init for y := 0 to height - 1:
   @@foryloop:

       // init values:
       movddup xmm0, [edi];  // res := 0;
       mulpd xmm0, xmm7;     // tmp := alpha*pX^
       mov esi, eax;         // esi = first destination element
       mov ebx, Y;           // ebx = first y vector element

       // for j := 0 to width - 1 do
       mov ecx, wd2;
       test ecx, ecx;
       jz @@lastElem;

       @@forxloop:
           movupd xmm1, [esi];
           movupd xmm2, [ebx];

           // pA^[j] := pA^[j] + tmp*pY1^[j];
           mulpd xmm2, xmm0;
           addpd xmm1, xmm2;

           movupd [esi], xmm1;

           add esi, 16;
           add ebx, 16;

       dec ecx;
       jnz @@forxloop;

       // check if we need to handle the last element
       mov ecx, aWidth;
       and ecx, 1;
       jz @@nextline;

       @@lastElem:

       movsd xmm1, [esi];
       movsd xmm2, [ebx];

       mulsd xmm2, xmm0;
       addsd xmm1, xmm2;
       movsd [esi], xmm1;

       @@nextline:

       // next results:
       add edi, incx;
       add eax, edx;
   dec height;
   jnz @@foryloop;

   // epilog
   pop esi;
   pop edi;
   pop ebx;
end;

procedure ASMRank1UpdateNonSeq(A : PDouble; const LineWidthA : NativeInt; width, height : NativeInt;
  X, Y : PDouble; incX, incY : NativeInt; alpha : double); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
var wd2 : NativeInt;
    aWidth : NativeInt;
    aLineWidthTmp : NativeInt;
// performs A = A + alpha*X*Y' in row major form
asm
   push ebx;
   push edi;
   push esi;

   mov aLineWidthTmp, edx;
   mov aWidth, ecx;
   shr ecx, 1;
   mov wd2, ecx;
        
   mov edi, X;
   mov edx, incY;

   // for the temp calculation
   movddup xmm7, alpha;

   // prepare for loop

   // init for y := 0 to height - 1:
   @@foryloop:

       // init values:
       movddup xmm0, [edi];  // res := 0;
       mulpd xmm0, xmm7;     // tmp := alpha*pX^
       mov esi, eax;         // esi = first destination element
       mov ebx, Y;           // ebx = first y vector element

       // for j := 0 to width - 1 do
       mov ecx, wd2;
       test ecx, ecx;
       jz @@lastElem;

       @@forxloop:
           movupd xmm1, [esi];
           movlpd xmm2, [ebx];
           movhpd xmm2, [ebx + edx];

           // pA^[j] := pA^[j] + tmp*pY1^[j];
           mulpd xmm2, xmm0;
           addpd xmm1, xmm2;

           movupd [esi], xmm1;

           add esi, 16;
           add ebx, edx;
           add ebx, edx;

       dec ecx;
       jnz @@forxloop;

       // check if we need to handle the last element
       mov ecx, aWidth;
       and ecx, 1;
       jz @@nextline;

       @@lastElem:

       movsd xmm1, [esi];
       movsd xmm2, [ebx];

       mulsd xmm2, xmm0;
       addsd xmm1, xmm2;
       movsd [esi], xmm1;

       @@nextline:

       // next results:
       add edi, incx;
       add eax, aLineWidthTmp;
   dec height;
   jnz @@foryloop;

   // epilog
   pop esi;
   pop edi;
   pop ebx;
end;

procedure ASMRank1UpdateSeqAligned(A : PDouble; const LineWidthA : NativeInt; width, height : NativeInt;
  X, Y : PDouble; incX, incY : NativeInt; alpha : double);
var wd2 : NativeInt;
    aWidth : NativeInt;
// performs A = A + alpha*X*Y' in row major form
asm
   push ebx;
   push edi;
   push esi;

   mov aWidth, ecx;
   shr ecx, 1;
   mov wd2, ecx;
        
   mov edi, X;

   // for the temp calculation
   movddup xmm7, alpha;

   // prepare for loop

   // init for y := 0 to height - 1:
   @@foryloop:

       // init values:
       movddup xmm0, [edi];  // res := 0;
       mulpd xmm0, xmm7;     // tmp := alpha*pX^
       mov esi, eax;         // esi = first destination element
       mov ebx, Y;           // ebx = first y vector element

       // for j := 0 to width - 1 do
       mov ecx, wd2;
       test ecx, ecx;
       jz @@lastElem;

       @@forxloop:
           movapd xmm1, [esi];
           movapd xmm2, [ebx];

           // pA^[j] := pA^[j] + tmp*pY1^[j];
           mulpd xmm2, xmm0;
           addpd xmm1, xmm2;

           movupd [esi], xmm1;

           add esi, 16;
           add ebx, 16;

       dec ecx;
       jnz @@forxloop;

       // check if we need to handle the last element
       mov ecx, aWidth;
       and ecx, 1;
       jz @@nextline;

       @@lastElem:

       movsd xmm1, [esi];
       movsd xmm2, [ebx];

       mulsd xmm2, xmm0;
       addsd xmm1, xmm2;
       movsd [esi], xmm1;

       @@nextline:

       // next results:
       add edi, incx;
       add eax, edx;
   dec height;
   jnz @@foryloop;

   // epilog
   pop esi;
   pop edi;
   pop ebx;
end;

function ASMMatrixVecDotMultAligned( x : PDouble; y : PDouble; N : NativeInt ) : double;
// eax = x, edx = y, ecx = N
asm
   push edi;

   // iters
   imul ecx, -8;

   // helper registers for the mt1, mt2 and dest pointers
   sub eax, ecx;
   sub edx, ecx;

   xorpd xmm0, xmm0;

   // unrolled loop
   @Loop1:
       add ecx, 128;
       jg @loopEnd1;

       movapd xmm1, [eax + ecx - 128];
       movapd xmm2, [edx + ecx - 128];
       mulpd xmm1, xmm2;
       addpd xmm0, xmm1;

       movapd xmm3, [eax + ecx - 112];
       movapd xmm4, [edx + ecx - 112];
       mulpd xmm3, xmm4;
       addpd xmm0, xmm3;

       movapd xmm1, [eax + ecx - 96];
       movapd xmm2, [edx + ecx - 96];
       mulpd xmm1, xmm2;
       addpd xmm0, xmm1;

       movapd xmm3, [eax + ecx - 80];
       movapd xmm4, [edx + ecx - 80];
       mulpd xmm3, xmm4;
       addpd xmm0, xmm3;

       movapd xmm1, [eax + ecx - 64];
       movapd xmm2, [edx + ecx - 64];
       mulpd xmm1, xmm2;
       addpd xmm0, xmm1;

       movapd xmm3, [eax + ecx - 48];
       movapd xmm4, [edx + ecx - 48];
       mulpd xmm3, xmm4;
       addpd xmm0, xmm3;

       movapd xmm1, [eax + ecx - 32];
       movapd xmm2, [edx + ecx - 32];
       mulpd xmm1, xmm2;
       addpd xmm0, xmm1;

       movapd xmm3, [eax + ecx - 16];
       movapd xmm4, [edx + ecx - 16];
       mulpd xmm3, xmm4;
       addpd xmm0, xmm3;
   jmp @Loop1;

   @loopEnd1:

   sub ecx, 128;
   jz @loopEnd2;

   // loop to get all fitting into an array of 2
   @loop2:
      add ecx, 16;
      jg @loop2End;

      movapd xmm3, [eax + ecx - 16];
      movapd xmm4, [edx + ecx - 16];
      mulpd xmm3, xmm4;
      addpd xmm0, xmm3;
   jmp @loop2;

   @loop2End:

   // handle last element
   sub ecx, 16;
   jz @loopEnd2;

   movsd xmm3, [eax - 8];
   movsd xmm4, [edx - 8];
   mulsd xmm3, xmm4;
   addsd xmm0, xmm3;

   @loopEnd2:

   // build result
   haddpd xmm0, xmm0;
   movsd Result, xmm0;

   pop edi;
end;

function ASMMatrixVecDotMultUnAligned( x : PDouble; y : PDouble; N : NativeInt ) : double;
// eax = x, edx = y, ecx = N
asm
   push edi;

   // iters
   imul ecx, -8;

   // helper registers for the mt1, mt2 and dest pointers
   sub eax, ecx;
   sub edx, ecx;

   xorpd xmm0, xmm0;

   // unrolled loop
   @Loop1:
       add ecx, 128;
       jg @loopEnd1;

       movupd xmm1, [eax + ecx - 128];
       movupd xmm2, [edx + ecx - 128];
       mulpd xmm1, xmm2;
       addpd xmm0, xmm1;

       movupd xmm3, [eax + ecx - 112];
       movupd xmm4, [edx + ecx - 112];
       mulpd xmm3, xmm4;
       addpd xmm0, xmm3;

       movupd xmm1, [eax + ecx - 96];
       movupd xmm2, [edx + ecx - 96];
       mulpd xmm1, xmm2;
       addpd xmm0, xmm1;

       movupd xmm3, [eax + ecx - 80];
       movupd xmm4, [edx + ecx - 80];
       mulpd xmm3, xmm4;
       addpd xmm0, xmm3;

       movupd xmm1, [eax + ecx - 64];
       movupd xmm2, [edx + ecx - 64];
       mulpd xmm1, xmm2;
       addpd xmm0, xmm1;

       movupd xmm3, [eax + ecx - 48];
       movupd xmm4, [edx + ecx - 48];
       mulpd xmm3, xmm4;
       addpd xmm0, xmm3;

       movupd xmm1, [eax + ecx - 32];
       movupd xmm2, [edx + ecx - 32];
       mulpd xmm1, xmm2;
       addpd xmm0, xmm1;

       movupd xmm3, [eax + ecx - 16];
       movupd xmm4, [edx + ecx - 16];
       mulpd xmm3, xmm4;
       addpd xmm0, xmm3;
   jmp @Loop1;

   @loopEnd1:

   sub ecx, 128;
   jz @loopEnd2;

   // loop to get all fitting into an array of 2
   @loop2:
      add ecx, 16;
      jg @loop2End;

      movupd xmm3, [eax + ecx - 16];
      movupd xmm4, [edx + ecx - 16];
      mulpd xmm3, xmm4;
      addpd xmm0, xmm3;
   jmp @loop2;

   @loop2End:

   // handle last element
   sub ecx, 8;
   jg @loopEnd2;

   movsd xmm3, [eax - 8];
   movsd xmm4, [edx - 8];
   mulsd xmm3, xmm4;
   addsd xmm0, xmm3;

   @loopEnd2:

   // build result
   haddpd xmm0, xmm0;
   movsd Result, xmm0;

   pop edi;
end;

function ASMMatrixVecDotMultUneven( x : PDouble; Y : PDouble; incX : NativeInt; incY : NativeInt;
 N : NativeInt) : Double;
// eax = x, edx = y, ecx = incx
asm
   push edi;
   push esi;
   push ebx;

   mov ebx, N;
   mov esi, incY;
   xorpd xmm0, xmm0;

   test ebx, ebx;
   jz @loopEnd;
   @loop:
      movsd xmm1, [eax];
      movsd xmm2, [edx];
      mulsd xmm1, xmm2;
      addsd xmm0, xmm1;

      // next element
      add eax, ecx;
      add edx, esi;

      // counter
      dec ebx;
      jnz @loop;
   @loopEnd:

   movsd Result, xmm0;

   // restore register
   pop ebx;
   pop esi;
   pop edi;
end;

{$ENDIF}

end.
