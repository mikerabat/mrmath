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


unit ASMMatrixMultOperations;

interface

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}
{$IFNDEF x64}

uses MatrixConst;

// full matrix operations -> there are no zero columns nor zero lines attached
procedure ASMMatrixMultAlignedEvenW1EvenW2(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1, height1, width2, height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt);  {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixMultUnAlignedEvenW1EvenW2(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1, height1, width2, height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

// the width of the second matrix is uneven -> the last column of mt2 must be handled differently
procedure ASMMatrixMultAlignedEvenW1OddW2(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1, height1, width2, height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixMultUnAlignedEvenW1OddW2(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1, height1, width2, height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

// the width of mt1 is odd but the one of mt2 is even
procedure ASMMatrixMultAlignedOddW1EvenW2(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1, height1, width2, height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixMultUnAlignedOddW1EvenW2(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1, height1, width2, height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

// both matrices have an odd width
procedure ASMMatrixMultAlignedOddW1OddW2(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1, height1, width2, height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixMultUnAlignedOddW1OddW2(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1, height1, width2, height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

// some special types of multiplications used e.g. in QR Decomposition
// note the result is stored in mt2 again!
// dest = mt1'*mt2; where mt2 is a lower triangular matrix and the operation is transposition
// the function assumes a unit diagonal (does not touch the real middle elements)
// width and height values are assumed to be the "original" (non transposed) ones
procedure ASMMtxMultTria2T1(dest : PDouble; LineWidthDest : NativeInt; mt1 : PDouble; LineWidth1 : NativeInt; mt2 : PDouble; LineWidth2 : NativeInt;
  width1, height1, width2, height2 : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
// mt1 = mt1*mt2'; where mt2 is an upper triangular matrix
procedure ASMMtxMultTria2T1StoreT1(mt1 : PDouble; LineWidth1 : NativeInt; mt2 : PDouble; LineWidth2 : NativeInt;
  width1, height1, width2, height2 : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
// calculates mt1 = mt1*mt2', mt2 = lower triangular matrix. diagonal elements are assumed to be 1!
procedure ASMMtxMultLowTria2T2Store1(mt1 : PDouble; LineWidth1 : NativeInt; mt2 : PDouble; LineWidth2 : NativeInt;
  width1, height1, width2, height2 : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

// W = C1*V1*T -> V1 is an upper triangular matrix with assumed unit diagonal entries. Operation on V1 transposition
procedure ASMMtxMultTria2TUpperUnit(dest : PDouble; LineWidthDest : NativeInt; mt1 : PDouble; LineWidth1 : NativeInt; mt2 : PDouble; LineWidth2 : NativeInt;
  width1, height1, width2, height2 : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

// mt1 = mt1*mt2; where mt2 is an upper triangular matrix
procedure ASMMtxMultTria2Store1(mt1 : PDouble; LineWidth1 : NativeInt; mt2 : PDouble; LineWidth2 : NativeInt;
  width1, height1, width2, height2 : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

// mt1 = mt1*mt2; where mt2 is an upper triangular matrix - diagonal elements are unit
procedure ASMMtxMultTria2Store1Unit(mt1 : PDouble; LineWidth1 : NativeInt; mt2 : PDouble; LineWidth2 : NativeInt;
  width1, height1, width2, height2 : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}


// performs a rank2 update in the form
// C = C - A*B' - B*A'
// N...order of C (N x N matrix)
// k... number of columns of A and B
// the lower triangle of C is not referenced
procedure ASMSymRank2UpdateUpperUnaligned( C : PDouble; LineWidthC : NativeInt; A : PDouble; LineWidthA : NativeInt;
  B : PDouble; LineWidthB : NativeInt; N : NativeInt; k : NativeInt ); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

{$ENDIF}

implementation

{$IFNDEF x64}
{$IFDEF FPC} {$ASMMODE intel} {$S-} {$ENDIF}
procedure ASMMatrixMultAlignedEvenW1EvenW2(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1, height1, width2, height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt);
var destOffset : NativeInt;
    iters : NativeInt;
    bytesWidth2 : NativeInt;
// eax = dest, edx = destLineWidth, ecx = mt1
asm
   push ebx;
   push esi;
   push edi;

   //destOffset := destLineWidth - Width2*sizeof(double);
   mov ebx, Width2;
   shl ebx, 3;
   sub edx, ebx;
   mov destOffset, edx;

   //iters1 := width2 div 2;
   mov edx, width2;
   shr edx, 1;
   mov width2, edx;

   //iters2 := -width1*sizeof(double);
   mov edx, width1;
   imul edx, -8;
   mov iters, edx;

   //bytesWidth2 := width2*sizeof(double);
   mov bytesWidth2, ebx;

   // initalize matrices
   sub ecx, iters;

   // for y := 0 to height1 - 1:
   mov esi, LineWidth2;
   @@forylabel:
       // for x := 0 to width2 - 1:
       mov ebx, width2;
       @@forxlabel:

           xorpd xmm0, xmm0;   // dest^ := 0
           xorpd xmm7, xmm7;   // (dest + 1)^ := 0;

           mov edi, mt2;       // valcounter2

           // for idx := 0 to width1 div 2 do
           mov edx, iters;

           @@InnerLoop:
               movapd xmm1, [ecx + edx];

               // load 2x2 block
               movapd xmm3, [edi];
               movapd xmm5, xmm3;

               movapd xmm4, [edi + esi];
               movapd xmm6, xmm4;

               // swap such that we can immediately multiply
               movhlps xmm4, xmm5;
               movlhps xmm3, xmm6;

               // eventually we can use 2 cpu ports when replicated
               movapd xmm6, xmm1;
               add edi, esi;

               // multiply 2x2 and add
               mulpd xmm3, xmm1;
               mulpd xmm4, xmm6;
               addpd xmm0, xmm3;
               addpd xmm7, xmm4;
               add edi, esi;      // now edi points two rows further

               // end for idx := 0 to width1 div 2
           //dec edx;
           add edx, 16;
           jnz @@InnerLoop;

           // final horizontal addition and compact
           haddpd xmm0, xmm7;

           // store back result
           movapd [eax], xmm0;

           // increment the pointers
           // inc(mt2), inc(dest);
           //add dword ptr [mt2], 8;
           add mt2, 16;
           add eax, 16;

       // end for x := 0 to width2 - 1
       dec ebx;
       jnz @@forxlabel;

       // dec(mt2, Width2);
       // inc(PByte(mt1), LineWidth1);
       // inc(PByte(dest), destOffset);
       //mov ebx, bytesWidth2;
       //sub dword ptr [mt2], ebx;
       mov edx, bytesWidth2;       
       sub mt2, edx;
       add ecx, LineWidth1;
       add eax, destOffset;

   // end for y := 0 to height1 - 1
   //dec eax;
   dec Height1;
   jnz @@forylabel

   pop edi;
   pop esi;
   pop ebx;
end;

procedure ASMMatrixMultUnAlignedEvenW1EvenW2(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1, height1, width2, height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt);
var destOffset : NativeInt;
    iters : NativeInt;
    bytesWidth2 : NativeInt;
// eax = dest, edx = destLineWidth, ecx = mt1
asm
   push ebx;
   push esi;
   push edi;

   //destOffset := destLineWidth - Width2*sizeof(double);
   mov ebx, Width2;
   shl ebx, 3;
   sub edx, ebx;
   mov destOffset, edx;

   //iters1 := width2 div 2;
   mov edx, width2;
   shr edx, 1;
   mov width2, edx;

   //iters2 := -width1*sizeof(double);
   mov edx, width1;
   imul edx, -8;
   mov iters, edx;

   //bytesWidth2 := width2*sizeof(double);
   mov bytesWidth2, ebx;

   // initalize matrices
   sub ecx, iters;

   // for y := 0 to height1 - 1:
   mov esi, LineWidth2;
   @@forylabel:
       // for x := 0 to width2 - 1:
       mov ebx, width2;
       @@forxlabel:

           xorpd xmm0, xmm0;   // dest^ := 0
           xorpd xmm7, xmm7;   // (dest + 1)^ := 0;

           mov edi, mt2;       // valcounter2

           // for idx := 0 to width1 div 2 do
           mov edx, iters;

           @@InnerLoop:
               movupd xmm1, [ecx + edx];

               // load 2x2 block
               movupd xmm3, [edi];
               movapd xmm5, xmm3;

               movupd xmm4, [edi + esi];
               movapd xmm6, xmm4;

               // swap such that we can immediately multiply
               movhlps xmm4, xmm5;
               movlhps xmm3, xmm6;

               // eventually we can use 2 cpu ports when replicated
               movapd xmm6, xmm1;
               add edi, esi;

               // multiply 2x2 and add
               mulpd xmm3, xmm1;
               mulpd xmm4, xmm6;
               addpd xmm0, xmm3;
               addpd xmm7, xmm4;
               add edi, esi;      // now edi points two rows further

               // end for idx := 0 to width1 div 2
           //dec edx;
           add edx, 16;
           jnz @@InnerLoop;

           // final horizontal addition and compact
           haddpd xmm0, xmm7;

           // store back result
           movupd [eax], xmm0;

           // increment the pointers
           // inc(mt2), inc(dest);
           //add dword ptr [mt2], 8;
           add mt2, 16;
           add eax, 16;

       // end for x := 0 to width2 - 1
       dec ebx;
       jnz @@forxlabel;

       // dec(mt2, Width2);
       // inc(PByte(mt1), LineWidth1);
       // inc(PByte(dest), destOffset);
       //mov ebx, bytesWidth2;
       //sub dword ptr [mt2], ebx;
       mov edx, bytesWidth2;       
       sub mt2, edx;
       add ecx, LineWidth1;
       add eax, destOffset;

   // end for y := 0 to height1 - 1
   //dec eax;
   dec Height1;
   jnz @@forylabel

   pop edi;
   pop esi;
   pop ebx;
end;

procedure ASMMatrixMultUnAlignedOddW1EvenW2(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1, height1, width2, height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt);
var destOffset : NativeInt;
    iters : NativeInt;
    bytesWidth2 : NativeInt;
// eax = dest, edx = destLineWidth, ecx = mt1
asm
   push ebx;
   push esi;
   push edi;

   //destOffset := destLineWidth - Width2*sizeof(double);
   mov ebx, Width2;
   shl ebx, 3;
   sub edx, ebx;
   mov destOffset, edx;

   //iters1 := width2 div 2;
   mov edx, width2;
   shr edx, 1;
   mov width2, edx;

   //-(width1 - 1)*sizeof(double)
   mov edx, width1;
   dec edx;
   imul edx, -8;
   mov iters, edx;

   //bytesWidth2 := width2*sizeof(double);
   mov bytesWidth2, ebx;

   // initalize matrices
   mov ecx, mt1;    // prepare ecx for the "reversed" for loop 
   sub ecx, iters;

   // for y := 0 to height1 - 1:
   mov esi, LineWidth2;
   @@forylabel:
       // for x := 0 to width2 - 1:
       mov ebx, width2;
       @@forxlabel:

           xorpd xmm0, xmm0;   // dest^ := 0
           xorpd xmm7, xmm7;   // (dest + 1)^ := 0;

           mov edi, mt2;       // valcounter2

           // for idx := 0 to width1 div 2 do
           mov edx, iters;

           @@InnerLoop:
               movupd xmm1, [ecx + edx];

               // load 2x2 block
               movupd xmm3, [edi];
               movapd xmm5, xmm3;

               movupd xmm4, [edi + esi];
               movapd xmm6, xmm4;

               // swap such that we can immediately multiply
               movhlps xmm4, xmm5;
               movlhps xmm3, xmm6;

               // eventually we can use 2 cpu ports when replicated
               movapd xmm6, xmm1;
               add edi, esi;

               // multiply 2x2 and add
               mulpd xmm3, xmm1;
               mulpd xmm4, xmm6;
               addpd xmm0, xmm3;
               addpd xmm7, xmm4;
               add edi, esi;      // now edi points two rows further

               // end for idx := 0 to width1 div 2
           //dec edx;
           add edx, 16;
           jnz @@InnerLoop;

           movddup xmm1, [ecx];

           movupd xmm3, [edi];
           mulpd xmm1, xmm3;

           // final horizontal addition
           xorpd xmm3, xmm3;
           movq xmm2, xmm1;
           addsd xmm0, xmm2;
           haddpd xmm0, xmm3;

           movhlps xmm2, xmm1;
           addsd xmm7, xmm2;
           haddpd xmm7, xmm3;
           
           // final horizontal addition and compact
           haddpd xmm0, xmm7;

           // store back result
           movupd [eax], xmm0;

           // increment the pointers
           // inc(mt2), inc(dest);
           //add dword ptr [mt2], 8;
           add mt2, 16;
           add eax, 16;

       // end for x := 0 to width2 - 1
       dec ebx;
       jnz @@forxlabel;

       // dec(mt2, Width2);
       // inc(PByte(mt1), LineWidth1);
       // inc(PByte(dest), destOffset);
       //mov ebx, bytesWidth2;
       //sub dword ptr [mt2], ebx;
       mov edx, bytesWidth2;       
       sub mt2, edx;
       add ecx, LineWidth1;
       add eax, destOffset;

   // end for y := 0 to height1 - 1
   //dec eax;
   dec Height1;
   jnz @@forylabel

   pop edi;
   pop esi;
   pop ebx;
end;

procedure ASMMatrixMultAlignedOddW1EvenW2(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1, height1, width2, height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt);
var destOffset : NativeInt;
    iters : NativeInt;
    bytesWidth2 : NativeInt;
// eax = dest, edx = destLineWidth, ecx = mt1
asm
   push ebx;
   push esi;
   push edi;

   //destOffset := destLineWidth - Width2*sizeof(double);
   mov ebx, Width2;
   shl ebx, 3;
   sub edx, ebx;
   mov destOffset, edx;

   //iters1 := width2 div 2;
   mov edx, width2;
   shr edx, 1;
   mov width2, edx;

   //-(width1 - 1)*sizeof(double)
   mov edx, width1;
   dec edx;
   imul edx, -8;
   mov iters, edx;

   //bytesWidth2 := width2*sizeof(double);
   mov bytesWidth2, ebx;

   // initalize matrices
   mov ecx, mt1;    // prepare ecx for the "reversed" for loop 
   sub ecx, iters;

   // for y := 0 to height1 - 1:
   mov esi, LineWidth2;
   @@forylabel:
       // for x := 0 to width2 - 1:
       mov ebx, width2;
       @@forxlabel:

           xorpd xmm0, xmm0;   // dest^ := 0
           xorpd xmm7, xmm7;   // (dest + 1)^ := 0;

           mov edi, mt2;       // valcounter2

           // for idx := 0 to width1 div 2 do
           mov edx, iters;

           @@InnerLoop:
               movapd xmm1, [ecx + edx];

               // load 2x2 block
               movapd xmm3, [edi];
               movapd xmm5, xmm3;

               movupd xmm4, [edi + esi];
               movapd xmm6, xmm4;

               // swap such that we can immediately multiply
               movhlps xmm4, xmm5;
               movlhps xmm3, xmm6;

               // eventually we can use 2 cpu ports when replicated
               movapd xmm6, xmm1;
               add edi, esi;

               // multiply 2x2 and add
               mulpd xmm3, xmm1;
               mulpd xmm4, xmm6;
               addpd xmm0, xmm3;
               addpd xmm7, xmm4;
               add edi, esi;      // now edi points two rows further

               // end for idx := 0 to width1 div 2
           //dec edx;
           add edx, 16;
           jnz @@InnerLoop;

           movddup xmm1, [ecx];

           movapd xmm3, [edi];
           mulpd xmm1, xmm3;

           // final horizontal addition
           xorpd xmm3, xmm3;
           movq xmm2, xmm1;
           addsd xmm0, xmm2;
           haddpd xmm0, xmm3;

           movhlps xmm2, xmm1;
           addsd xmm7, xmm2;
           haddpd xmm7, xmm3;
           
           // final horizontal addition and compact
           haddpd xmm0, xmm7;

           // store back result
           movapd [eax], xmm0;

           // increment the pointers
           // inc(mt2), inc(dest);
           //add dword ptr [mt2], 8;
           add mt2, 16;
           add eax, 16;

       // end for x := 0 to width2 - 1
       dec ebx;
       jnz @@forxlabel;

       // dec(mt2, Width2);
       // inc(PByte(mt1), LineWidth1);
       // inc(PByte(dest), destOffset);
       //mov ebx, bytesWidth2;
       //sub dword ptr [mt2], ebx;
       mov edx, bytesWidth2;       
       sub mt2, edx;
       add ecx, LineWidth1;
       add eax, destOffset;

   // end for y := 0 to height1 - 1
   //dec eax;
   dec Height1;
   jnz @@forylabel

   pop edi;
   pop esi;
   pop ebx;
end;

procedure ASMMatrixMultAlignedEvenW1OddW2(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1, height1, width2, height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt);
var destOffset : NativeInt;
    iters : NativeInt;
    bytesWidth2 : NativeInt;
// eax = dest, edx = destLineWidth, ecx = mt1
asm
   push ebx;
   push esi;
   push edi;

   //destOffset := destLineWidth - (Width2 - 1)*sizeof(double);
   mov ebx, Width2;
   dec ebx;
   shl ebx, 3;
   sub edx, ebx;
   mov destOffset, edx;

   //iters1 := width2 div 2;
   mov edx, width2;
   shr edx, 1;
   mov width2, edx;

   //iters2 := -width1*sizeof(double);
   mov edx, width1;
   imul edx, -8;
   mov iters, edx;

   //bytesWidth2 := (width2 - 1)*sizeof(double);
   mov bytesWidth2, ebx;

   // initalize matrix pointer
   sub ecx, iters;
   mov esi, LineWidth2;
   
   // for y := 0 to height1 - 1:
   @@forylabel:
       // for x := 0 to width2 - 1:
       mov ebx, width2;
       @@forxlabel:

           xorpd xmm0, xmm0;   // dest^ := 0
           xorpd xmm7, xmm7;   // (dest + 1)^ := 0;

           mov edi, mt2;       // valcounter2

           // for idx := 0 to width1 div 2 do
           mov edx, iters;

           @@InnerLoop:
               movapd xmm1, [ecx + edx];

               // load 2x2 block
               movapd xmm3, [edi];
               movapd xmm5, xmm3;

               movapd xmm4, [edi + esi];
               movapd xmm6, xmm4;

               // swap such that we can immediately multiply
               movhlps xmm4, xmm5;
               movlhps xmm3, xmm6;

               movapd xmm6, xmm1;
               add edi, esi;      // first row

               // multiply 2x2 and add
               mulpd xmm3, xmm1;
               mulpd xmm4, xmm6;
               addpd xmm0, xmm3;
               addpd xmm7, xmm4;
               add edi, esi;      // 2nd row

               // end for idx := 0 to width1 div 2
           //dec edx;
           add edx, 16;
           jnz @@InnerLoop;

           // final horizontal addition and compact
           haddpd xmm0, xmm7;

           // store back result
           movapd [eax], xmm0;

           // increment the pointers
           // inc(mt2), inc(dest);
           //add dword ptr [mt2], 8;
           add mt2, 16;
           add eax, 16;

       // end for x := 0 to width2 - 1
       dec ebx;
       jnz @@forxlabel;

       // handle the last column of mt2
       xorpd xmm0, xmm0;   // dest^ := 0
       mov edi, mt2;       // valcounter2

       mov edx, iters;
       @@InnerLoop2:
           // load element from line
           movsd xmm1, [ecx + edx];

           // load, multiply and add
           mulsd xmm1, [edi];
           add edi, esi;       // next row

           addsd xmm0, xmm1;
       add edx, 8;
       jnz @@InnerLoop2;

       movsd [eax], xmm0;

       // dec(mt2, Width2);
       // inc(PByte(mt1), LineWidth1);
       // inc(PByte(dest), destOffset);
       //mov ebx, bytesWidth2;
       //sub dword ptr [mt2], ebx;
       mov edx, bytesWidth2;       
       sub mt2, edx;
       add ecx, LineWidth1;
       add eax, destOffset;

   // end for y := 0 to height1 - 1
   dec Height1;
   jnz @@forylabel

   pop edi;
   pop esi;
   pop ebx;
end;

procedure ASMMatrixMultUnAlignedEvenW1OddW2(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1, height1, width2, height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt);
var destOffset : NativeInt;
    iters : NativeInt;
    bytesWidth2 : NativeInt;
// eax = dest, edx = destLineWidth, ecx = mt1
asm
   push ebx;
   push esi;
   push edi;

   //destOffset := destLineWidth - (Width2 - 1)*sizeof(double);
   mov ebx, Width2;
   dec ebx;
   shl ebx, 3;
   sub edx, ebx;
   mov destOffset, edx;

   //iters1 := width2 div 2;
   mov edx, width2;
   shr edx, 1;
   mov width2, edx;

   //iters2 := -width1*sizeof(double);
   mov edx, width1;
   imul edx, -8;
   mov iters, edx;

   //bytesWidth2 := (width2 - 1)*sizeof(double);
   mov bytesWidth2, ebx;

   // initalize matrix pointer
   sub ecx, iters;
   mov esi, LineWidth2;
   
   // for y := 0 to height1 - 1:
   @@forylabel:
       // for x := 0 to width2 - 1:
       mov ebx, width2;
       @@forxlabel:

           xorpd xmm0, xmm0;   // dest^ := 0
           xorpd xmm7, xmm7;   // (dest + 1)^ := 0;

           mov edi, mt2;       // valcounter2

           // for idx := 0 to width1 div 2 do
           mov edx, iters;

           @@InnerLoop:
               movupd xmm1, [ecx + edx];

               // load 2x2 block
               movupd xmm3, [edi];
               movapd xmm5, xmm3;

               movupd xmm4, [edi + esi];
               movapd xmm6, xmm4;

               // swap such that we can immediately multiply
               movhlps xmm4, xmm5;
               movlhps xmm3, xmm6;

               movapd xmm6, xmm1;
               add edi, esi;      // first row

               // multiply 2x2 and add
               mulpd xmm3, xmm1;
               mulpd xmm4, xmm6;
               addpd xmm0, xmm3;
               addpd xmm7, xmm4;
               add edi, esi;      // 2nd row

               // end for idx := 0 to width1 div 2
           //dec edx;
           add edx, 16;
           jnz @@InnerLoop;

           // final horizontal addition and compact
           haddpd xmm0, xmm7;

           // store back result
           movupd [eax], xmm0;

           // increment the pointers
           // inc(mt2), inc(dest);
           //add dword ptr [mt2], 8;
           add mt2, 16;
           add eax, 16;

       // end for x := 0 to width2 - 1
       dec ebx;
       jnz @@forxlabel;

       // handle the last column of mt2
       xorpd xmm0, xmm0;   // dest^ := 0
       mov edi, mt2;       // valcounter2

       mov edx, iters;
       @@InnerLoop2:
           // load element from line
           movsd xmm1, [ecx + edx];

           // load, multiply and add
           mulsd xmm1, [edi];
           add edi, esi;       // next row

           addsd xmm0, xmm1;
       add edx, 8;
       jnz @@InnerLoop2;

       movsd [eax], xmm0;

       // dec(mt2, Width2);
       // inc(PByte(mt1), LineWidth1);
       // inc(PByte(dest), destOffset);
       //mov ebx, bytesWidth2;
       //sub dword ptr [mt2], ebx;
       mov edx, bytesWidth2;       
       sub mt2, edx;
       add ecx, LineWidth1;
       add eax, destOffset;

   // end for y := 0 to height1 - 1
   dec Height1;
   jnz @@forylabel

   pop edi;
   pop esi;
   pop ebx;
end;

procedure ASMMatrixMultAlignedOddW1OddW2(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1, height1, width2, height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt);
var destOffset : NativeInt;
    iters : NativeInt;
    bytesWidth2 : NativeInt;
// eax = dest, edx = destLineWidth, ecx = mt1
asm
   push ebx;
   push esi;
   push edi;

   //destOffset := destLineWidth - (Width2 - 1)*sizeof(double);
   mov ebx, Width2;
   dec ebx;
   mov width2, ebx;
   shl ebx, 3;
   sub edx, ebx;
   mov destOffset, edx;

   //iters1 := width2 div 2;
   mov edx, width2;
   shr edx, 1;
   mov width2, edx;

   //iters2 := -width1*sizeof(double);
   mov edx, width1;
   dec edx;
   imul edx, -8;
   mov iters, edx;

   //bytesWidth2 := (width2 - 1)*sizeof(double);
   mov bytesWidth2, ebx;

   // initalize matrix pointer
   sub ecx, iters;
   mov esi, LineWidth2;
   
   // for y := 0 to height1 - 1:
   mov esi, LineWidth2;
   @@forylabel:
       // for x := 0 to width2 - 1:
       mov ebx, width2;
       @@forxlabel:

           xorpd xmm0, xmm0;   // dest^ := 0
           xorpd xmm7, xmm7;   // (dest + 1)^ := 0;

           mov edi, mt2;       // valcounter2

           // for idx := 0 to width1 div 2 do
           mov edx, iters;

           @@InnerLoop:
               movapd xmm1, [ecx + edx];

               // load 2x2 block
               movapd xmm3, [edi];
               movapd xmm5, xmm3;

               movupd xmm4, [edi + esi];
               movapd xmm6, xmm4;

               // swap such that we can immediately multiply
               movhlps xmm4, xmm5;
               movlhps xmm3, xmm6;

               // eventually we can use 2 cpu ports when replicated
               movapd xmm6, xmm1;
               add edi, esi;

               // multiply 2x2 and add
               mulpd xmm3, xmm1;
               mulpd xmm4, xmm6;
               addpd xmm0, xmm3;
               addpd xmm7, xmm4;
               add edi, esi;      // now edi points two rows further

               // end for idx := 0 to width1 div 2
           //dec edx;
           add edx, 16;
           jnz @@InnerLoop;

           movddup xmm1, [ecx];

           movapd xmm3, [edi];
           mulpd xmm1, xmm3;

           // final horizontal addition
           xorpd xmm3, xmm3;
           movq xmm2, xmm1;
           addsd xmm0, xmm2;
           haddpd xmm0, xmm3;

           movhlps xmm2, xmm1;
           addsd xmm7, xmm2;
           haddpd xmm7, xmm3;
           
           // final horizontal addition and compact
           haddpd xmm0, xmm7;

           // store back result
           movapd [eax], xmm0;

           // increment the pointers
           // inc(mt2), inc(dest);
           //add dword ptr [mt2], 8;
           add mt2, 16;
           add eax, 16;

       // end for x := 0 to width2 - 1
       dec ebx;
       jnz @@forxlabel;


       // handle the last column of mt2
       xorpd xmm0, xmm0;   // dest^ := 0
       mov edi, mt2;       // valcounter2

       mov edx, iters;
       @@InnerLoop2:
           // load element from line
           movsd xmm1, [ecx + edx];

           // load, multiply and add
           mulsd xmm1, [edi];
           add edi, esi;       // next row

           addsd xmm0, xmm1;
       add edx, 8;
       jnz @@InnerLoop2;

       movsd xmm1, [ecx];
       mulsd xmm1, [edi];
       addsd xmm0, xmm1;
       
       movsd [eax], xmm0;
       
       // dec(mt2, Width2);
       // inc(PByte(mt1), LineWidth1);
       // inc(PByte(dest), destOffset);
       //mov ebx, bytesWidth2;
       //sub dword ptr [mt2], ebx;
       mov edx, bytesWidth2;       
       sub mt2, edx;
       add ecx, LineWidth1;
       add eax, destOffset;

   // end for y := 0 to height1 - 1
   //dec eax;
   dec Height1;
   jnz @@forylabel

   pop edi;
   pop esi;
   pop ebx;
end;

procedure ASMMatrixMultUnAlignedOddW1OddW2(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1, height1, width2, height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt);
var destOffset : NativeInt;
    iters : NativeInt;
    bytesWidth2 : NativeInt;
// eax = dest, edx = destLineWidth, ecx = mt1
asm
   push ebx;
   push esi;
   push edi;

   //destOffset := destLineWidth - (Width2 - 1)*sizeof(double);
   mov ebx, Width2;
   dec ebx;
   mov width2, ebx;
   shl ebx, 3;
   sub edx, ebx;
   mov destOffset, edx;

   //iters1 := width2 div 2;
   mov edx, width2;
   shr edx, 1;
   mov width2, edx;

   //iters2 := -width1*sizeof(double);
   mov edx, width1;
   dec edx;
   imul edx, -8;
   mov iters, edx;

   //bytesWidth2 := (width2 - 1)*sizeof(double);
   mov bytesWidth2, ebx;

   // initalize matrix pointer
   sub ecx, iters;
   mov esi, LineWidth2;
   
   // for y := 0 to height1 - 1:
   mov esi, LineWidth2;
   @@forylabel:
       // for x := 0 to width2 - 1:
       mov ebx, width2;
       @@forxlabel:

           xorpd xmm0, xmm0;   // dest^ := 0
           xorpd xmm7, xmm7;   // (dest + 1)^ := 0;

           mov edi, mt2;       // valcounter2

           // for idx := 0 to width1 div 2 do
           mov edx, iters;

           @@InnerLoop:
               movupd xmm1, [ecx + edx];

               // load 2x2 block
               movupd xmm3, [edi];
               movapd xmm5, xmm3;

               movupd xmm4, [edi + esi];
               movapd xmm6, xmm4;

               // swap such that we can immediately multiply
               movhlps xmm4, xmm5;
               movlhps xmm3, xmm6;

               // eventually we can use 2 cpu ports when replicated
               movapd xmm6, xmm1;
               add edi, esi;

               // multiply 2x2 and add
               mulpd xmm3, xmm1;
               mulpd xmm4, xmm6;
               addpd xmm0, xmm3;
               addpd xmm7, xmm4;
               add edi, esi;      // now edi points two rows further

               // end for idx := 0 to width1 div 2
           //dec edx;
           add edx, 16;
           jnz @@InnerLoop;

           movddup xmm1, [ecx];

           movupd xmm3, [edi];
           mulpd xmm1, xmm3;

           // final horizontal addition
           xorpd xmm3, xmm3;
           movq xmm2, xmm1;
           addsd xmm0, xmm2;
           haddpd xmm0, xmm3;

           movhlps xmm2, xmm1;
           addsd xmm7, xmm2;
           haddpd xmm7, xmm3;
           
           // final horizontal addition and compact
           haddpd xmm0, xmm7;

           // store back result
           movupd [eax], xmm0;

           // increment the pointers
           // inc(mt2), inc(dest);
           //add dword ptr [mt2], 8;
           add mt2, 16;
           add eax, 16;

       // end for x := 0 to width2 - 1
       dec ebx;
       jnz @@forxlabel;


       // handle the last column of mt2
       xorpd xmm0, xmm0;   // dest^ := 0
       mov edi, mt2;       // valcounter2

       mov edx, iters;
       @@InnerLoop2:
           // load element from line
           movsd xmm1, [ecx + edx];

           // load, multiply and add
           mulsd xmm1, [edi];
           add edi, esi;       // next row

           addsd xmm0, xmm1;
       add edx, 8;
       jnz @@InnerLoop2;

       movsd xmm1, [ecx];
       mulsd xmm1, [edi];
       addsd xmm0, xmm1;
       
       movsd [eax], xmm0;
       
       // dec(mt2, Width2);
       // inc(PByte(mt1), LineWidth1);
       // inc(PByte(dest), destOffset);
       //mov ebx, bytesWidth2;
       //sub dword ptr [mt2], ebx;
       mov edx, bytesWidth2;       
       sub mt2, edx;
       add ecx, LineWidth1;
       add eax, destOffset;

   // end for y := 0 to height1 - 1
   //dec eax;
   dec Height1;
   jnz @@forylabel

   pop edi;
   pop esi;
   pop ebx;
end;

// ###########################################
// #### Special multiplication routines (for now only used in QR Decomposition)
// ###########################################

// note the result is stored in mt2 again!
// dest = mt1'*mt2; where mt2 is a lower triangular matrix and the operation is transposition
// the function assumes a unit diagonal (does not touch the real middle elements)
// width and height values are assumed to be the "original" (non transposed) ones
procedure ASMMtxMultTria2T1(dest : PDouble; LineWidthDest : NativeInt; mt1 : PDouble; LineWidth1 : NativeInt;
  mt2 : PDouble; LineWidth2 : NativeInt;
  width1, height1, width2, height2 : NativeInt);
var pMt2 : PDouble;
    width2D2 : NativeInt;
asm
   push ebx;
   push esi;
   push edi;

   // width2D2 := width2 div 2;
   mov esi, width2;
   shr esi, 1;
   mov width2D2, esi;

   // for x := 0 to width1 - 1 do
   @@forxloop:

     push eax;  // store "dest"
     push ecx;  // store mt1
     // pMT2 := mt2;
     // pDest := dest;

     mov ebx, mt2;
     mov pMT2, ebx;

     // for y := 0 to width2D2 - 1 do
     mov esi, width2D2;
     test esi, esi;
     jz @@foryloopend;

     xor esi, esi;
     @@foryloop:

          // valCounter1 := PConstDoubleArr(mt1);
          // inc(PByte(valCounter1), 2*y*LineWidth1);
          //mov ecx, mt1;
          mov ecx, [esp];
          
          mov ebx, esi;
          add ebx, ebx;
          imul ebx, LineWidth1;
          add ecx, ebx;

          // valCounter2 := PConstDoubleArr(pMT2);
          // inc(PByte(valCounter2), (2*y + 1)*LineWidth2);
          mov edi, pMt2;
          mov ebx, esi;
          add ebx, ebx;
          imul ebx, LineWidth2;
          add ebx, LineWidth2;
          add edi, ebx;

          // tmp[0] := valCounter1^[0];
          // inc(PByte(valCounter1), LineWidth1);
          movsd xmm0, [ecx];
          add ecx, LineWidth1;

          // if height2 - 2*y - 1 > 0 then
          mov ebx, esi;
          add ebx, ebx;
          inc ebx;

          cmp ebx, height2;
          jnl @@PreInnerLoop;
          
              // tmp[0] := tmp[0] + valCounter1^[0]*valCounter2^[0];
              // tmp[1] := valCounter1^[0];
              movsd xmm1, [ecx];
              movlhps xmm0, xmm1;

              mulsd xmm1, [edi];
              addsd xmm0, xmm1;

              //inc(PByte(valCounter1), LineWidth1);
              //inc(PByte(valCounter2), LineWidth2);

              add ecx, LineWidth1;
              add edi, LineWidth2;

          @@PreInnerLoop:

          // rest is a double column!

          // prepare loop
          mov ebx, height2;
          sub ebx, esi;
          sub ebx, esi;
          sub ebx, 2;

          test ebx, ebx;
          jle @@InnerLoopEnd;

          @@InnerLoop:
             // tmp[0] := tmp[0] + valCounter1^[0]*valCounter2^[0];
             // tmp[1] := tmp[1] + valCounter1^[0]*valCounter2^[1];
             movddup xmm1, [ecx];
             movupd xmm2, [edi];

             mulpd xmm2, xmm1;
             addpd xmm0, xmm2;

             //inc(PByte(valCounter1), LineWidth1);
             //inc(PByte(valCounter2), LineWidth2);

             add ecx, LineWidth1;
             add edi, LineWidth2;

          dec ebx;
          jnz @@InnerLoop;

          @@InnerLoopEnd:


          // write back result

          // pDest^ := tmp[0];
          // PDouble(NativeUint(pDest) + sizeof(double))^ := tmp[1];

          movupd [eax], xmm0;

          // inc(pDest, 2);
          // inc(pMT2, 2);
          add eax, 16;
          add pMT2, 16;

     // end foryloop
     inc esi;
     cmp esi, width2D2;
     jne @@foryloop;

     @@foryloopend:


     //if (width2 and $01) = 1 then
     mov esi, width2;
     and esi, 1;

     jz @@ifend1;

     // special handling of last column (just copy the value)

     // valCounter1 := PConstDoubleArr(mt1);
     //mov ecx, mt1;
     mov ecx, [esp];

     //inc(PByte(valCounter1), LineWidth1*(height1 - 1));
     mov ebx, height1;
     dec ebx;
     imul ebx, LineWidth1;

     // pDest^ := valCounter1^[0];
     movsd xmm0, [ecx + ebx];
     movsd [eax], xmm0;

     @@ifend1:

     //inc(mt1);
     //inc(PByte(dest), LineWidthDest);
     pop ecx;
     add ecx, 8;
     
     //inc(mt1);
     //inc(PByte(dest), LineWidthDest);
     //add mt1, 8;

     pop eax;
     add eax, edx;
     //mov ebx, LineWidthDest;
     //add dest, ebx;

  // end for loop
  dec width1;
  jnz @@forxloop;

  // epilog
  pop edi;
  pop esi;
  pop ebx;
end;

procedure ASMMtxMultTria2Store1Unit(mt1 : PDouble; LineWidth1 : NativeInt; mt2 : PDouble; LineWidth2 : NativeInt;
  width1, height1, width2, height2 : NativeInt);
var iter : NativeInt;
    aMT1 : PDouble;
asm
   push ebx;
   push edi;
   push esi;

   mov aMT1, eax;
   
   // init
   // iter := -(width1-1)*sizeof(double);
   mov ebx, width1;
   dec ebx;
   imul ebx, -8;
   mov iter, ebx;
   
   // inc(mt2, width2 - 1);
   mov ebx, width2;
   dec ebx;
   imul ebx, 8; //sizeof(double)
   add ecx, ebx;

   mov ebx, width2;
   dec ebx;
   mov width2, ebx;

   mov ebx, LineWidth2;

   // for x := 0 to width2 - 2 do
   @@forxloop:
      mov edi, height1;

      mov eax, aMT1;
      sub eax, iter;

      // for y := 0 to height1 - 1
      @@foryloop:
         // tmp := 0;
         xorpd xmm0, xmm0;

         // ecx, mt2
         push ecx;

         // for idx := 0 to width1 - x - 2
         mov esi, iter;

         // check if we have enough iterations:
         cmp esi, 0;
         jge @@foridxloopend;

         @@foridxloop:
            movsd xmm1, [ecx];
            movsd xmm2, [eax + esi];

            add ecx, ebx;  // + linewidth2

            mulsd xmm1, xmm2;
            addsd xmm0, xmm1;

         add esi, 8;
         jnz @@foridxloop;

         @@foridxloopend:

         // last element is unit
         addsd xmm0, [eax];

         // PConstDoubleArr(pmt1)^[width2 - 1 - x] := tmp;
         mov ecx, eax;
         add ecx, iter;
         mov esi, width2;
         //dec esi;
         movsd [ecx + 8*esi], xmm0;

         pop ecx;

         // inc(PByte(pmT1), LineWidth1);
         add eax, edx;

      dec edi;
      jnz @@foryloop;

      // reduce for idx loop
      add iter, 8;
      // dec(mt2);
      sub ecx, 8;

   dec width2;
   jnz @@forxloop;

   @@endproc:

   // cleanup stack
   pop esi;
   pop edi;
   pop ebx;
end;

procedure ASMMtxMultTria2Store1(mt1 : PDouble; LineWidth1 : NativeInt; mt2 : PDouble; LineWidth2 : NativeInt;
  width1, height1, width2, height2 : NativeInt);
var wm1 : NativeInt;
    iter : NativeInt;
    aLineWidth1 : NativeInt;
// eax = mt1, edx = LineWidth1, ecx = mt2
asm
   push ebx;
   push edi;
   push esi;
   // init

   mov aLineWidth1, edx;
   
   //wm1 := width2 - 1;
   mov ebx, width2;
   dec ebx;
   mov wm1, ebx;

   // iter := -width1*sizeof(double);
   mov edi, width1;
   imul edi, -8;
   mov iter, edi;
   
   // inc(mt2, width2 - 1);
   imul ebx, 8;
   add ecx, ebx;

   mov edx, LineWidth2;

   // for x := 0 to width2 - 1 do
   @@forxloop:
      mov edi, height1;

      push eax;
      sub eax, iter;

      // for y := 0 to height1 - 1
      @@foryloop:
         // tmp := 0;
         xorpd xmm0, xmm0;

         // mt2
         push ecx;

         // for idx := 0 to width1 - x - 1
         mov esi, iter;

         // check if we have enough iterations:
         test esi, esi;
         jz @@foridxloopend;

         @@foridxloop:
            movsd xmm1, [ecx];
            movsd xmm2, [eax + esi]

            add ecx, edx;  // + linewidth2

            mulsd xmm1, xmm2;
            addsd xmm0, xmm1;

         add esi, 8;
         jnz @@foridxloop;

         @@foridxloopend:

         // PConstDoubleArr(pmt1)^[width2 - 1 - x] := tmp;
         mov ecx, eax;
         add ecx, iter;
         mov esi, width2;
         dec esi;
         movsd [ecx + 8*esi], xmm0;

         // restore mt2
         pop ecx;
         
         // inc(PByte(pmT1), LineWidth1);
         add eax, aLineWidth1;

      dec edi;
      jnz @@foryloop;

      // reduce for idx loop
      add iter, 8;
      pop eax;
      
      // dec(mt2);
      sub ecx, 8;

   dec width2;
   jnz @@forxloop;

   // cleanup stack
   pop esi;
   pop edi;
   pop ebx;
end;


// calculates mt1 = mt1*mt2', mt2 = lower triangular matrix. diagonal elements are assumed to be 1!
procedure ASMMtxMultLowTria2T2Store1(mt1 : PDouble; LineWidth1 : NativeInt; mt2 : PDouble; LineWidth2 : NativeInt;
  width1, height1, width2, height2 : NativeInt);
var aLineWidth1 : NativeInt;
    aMt1 : PDouble;
// eax = mt1, edx = LineWidth1, ecx = mt2
asm
   push ebx;
   push edi;
   push esi;

   // init
   mov aMt1, eax;
   mov aLineWidth1, edx;

   // iter := -(width2 - 1)*sizeof(double);
   mov edx, width2;
   dec edx;
   imul edx, -8;
        
   // start from bottom
   // ebx: mt2
   // inc(PByte(mt2),(height2 - 1)*LineWidth2);
   mov esi, height2;
   dec esi;
   imul esi, LineWidth2;
   add ecx, esi;
   sub ecx, edx;

   // for x := 0 to width2 - 2
   dec width2;
   jz @@endproc;
   @@forxloop:
      mov eax, aMt1;
      sub eax, edx;

      // for y := 0 to height1 - 1
      mov esi, height1;
      @@foryloop:
         xorpd xmm0, xmm0;

         // for idx := 0 to width2 - x - 2
         mov edi, edx;
         test edi, edi;
         jz @@foridxloopend;

         // unrolled loop 2x2
         add edi, 32;
         jg @@foridxloopStart;

         @@foridxlongloop:
            movupd xmm1, [eax + edi - 32];
            movupd xmm3, [eax + edi - 16];

            movupd xmm2, [ecx + edi - 32];
            movupd xmm4, [ecx + edi - 16];
            mulpd xmm1, xmm2;
            addpd xmm0, xmm1;
            
            mulpd xmm3, xmm4;
            addpd xmm0, xmm3;
         add edi, 32;
         jl @@foridxlongloop;

         haddpd xmm0, xmm0;

         @@foridxloopStart:
         sub edi, 32;

         jz @@foridxloopend;

         @@foridxloop:
            movsd xmm1, [eax + edi];
            movsd xmm2, [ecx + edi];
            mulsd xmm1, xmm2;
            addsd xmm0, xmm1;

         add edi, 8;
         jnz @@foridxloop;

         @@foridxloopend:

         // last element is unit:
         movsd xmm1, [eax];
         addsd xmm0, xmm1;

         // write back
         // PConstDoubleArr(pMt1)^[width2 - x - 1] := tmp + valCounter1^[width2 - x - 1];
         movsd [eax], xmm0;

         add eax, aLineWidth1;

      dec esi;
      jnz @@foryloop;

      // dec(PByte(mt2), LineWidth2);
      sub ecx, LineWidth2;
      sub ecx, 8;

      // adjust iterator to the next x value for the idxloop
      add edx, 8;

   dec width2;
   jnz @@forxloop;

   @@endproc:

   pop esi;
   pop edi;
   pop ebx;
end;


procedure ASMMtxMultTria2T1StoreT1(mt1 : PDouble; LineWidth1 : NativeInt; mt2 : PDouble; LineWidth2 : NativeInt;
  width1, height1, width2, height2 : NativeInt);
var iter : NativeInt;
    testExitLoopVal : NativeInt;
    aLineWidth1 : NativeInt;
    aMt2 : PDouble;
asm
   push ebx;
   push esi;
   push edi;

   mov aLineWidth1, edx;
   mov aMt2, ecx;

   // iter := -width1*sizeof(double);
   mov ebx, width1;
   imul ebx, -8;
   mov iter, ebx;

   // testExitLoopVal := height2*sizeof(double) + iter;
   mov edi, height2;
   imul edi, 8;
   add ebx, edi;
   mov testExitLoopVal, ebx;

   // eax := mt1
   sub eax, iter;

   // for y loop
   @@foryloop:
      // store mt2
      mov ecx, aMt2;
      sub ecx, iter;
      mov esi, iter;

      @@forxloop:
         xorpd xmm0, xmm0; // temp := 0

         mov edx, esi;  // loop counter x;

         // test if height2 > width1 and loop counter > width1
         mov edi, edx;
         test edi, edi;
         jge @@foriloopend;

         // in case x is odd -> handle the first element separately
         and edi, $E;
         jz @@foriloopinit;

         // single element handling
         movsd xmm1, [eax + edx];
         movsd xmm2, [ecx + edx];
         mulsd xmm1, xmm2;
         addsd xmm0, xmm1;
         add edx, 8;

         @@foriloopinit:

         // in case the last single x element was handled we do not need further looping
         test edx, edx;
         jz @@foriloopend;

         // for i := x to width1 - 1
         @@foriloop:
            // two elements at a time:
            movupd xmm1, [eax + edx];
            movupd xmm2, [ecx + edx];
            mulpd xmm1, xmm2;
            addpd xmm0, xmm1;

         add edx, 16;
         jnz @@foriloop;

         @@foriloopend:

         // final result
         haddpd xmm0, xmm0;
         movsd [eax + esi], xmm0;

         add ecx, LineWidth2;
         add esi, 8;

      mov edx, testExitLoopVal;
      cmp esi, edx;
      jne @@forxloop;

      add eax, aLineWidth1;
   dec height1;
   jnz @@foryloop;

   pop edi;
   pop esi;
   pop ebx;
end;

// W = C1*V1*T -> V1 is an upper triangular matrix with assumed unit diagonal entries. Operation on V1 transposition
procedure ASMMtxMultTria2TUpperUnit(dest : PDouble; LineWidthDest : NativeInt; mt1 : PDouble; LineWidth1 : NativeInt; mt2 : PDouble; LineWidth2 : NativeInt;
  width1, height1, width2, height2 : NativeInt);
var iter : NativeInt;
    testExitLoopVal : NativeInt;
    aLineWidthDest : NativeInt; 
// eax = dest, edx = LineWidthDest, ecx = mt1
asm
   push ebx;
   push esi;
   push edi;

   // init 
   mov aLineWidthDest, edx;
        
   // iter := -width1*sizeof(double);
   mov ebx, width1;
   imul ebx, -8;
   mov iter, ebx;
        
   //testExitLoopVal := height2*sizeof(double) + iter;
   mov edi, height2;
   shl edi, 3;
   add edi, ebx;
   mov testExitLoopVal, edi;

   // rax := mt1
   sub ecx, ebx;
   sub eax, ebx;

   // for y loop
   @@foryloop:
      mov ebx, mt2;
      sub ebx, iter;
      mov esi, iter;

      @@forxloop:
         xorpd xmm0, xmm0; // temp := 0

         mov edx, esi;  // loop counter x;

         // test if height2 > width1 and loop counter > width1
         mov edi, edx;
         test edi, edi;
         jge @@foriloopend;

         // in case x is odd -> handle the first element separately
         and edi, $E;
         jz @@foriloopinit;

         // single element handling -> mt1 first element is assumed unit!
         movsd xmm0, [ecx + edx];
         add edx, 8;

         jmp @@AfterLoopInit;

         @@foriloopinit:

         test edx, edx;
         jz @@foriloopend;

         // two elements init at a time:
         movsd xmm0, [ecx + edx];
         movsd xmm1, [ecx + edx + 8];
         movsd xmm2, [ebx + edx + 8];
         mulsd xmm1, xmm2;
         addsd xmm0, xmm1;

         add edx, 16;

         @@AfterLoopInit:

         // in case the last single x element was handled we do not need further looping
         test edx, edx;
         jz @@foriloopend;

         // for i := x to width1 - 1
         @@foriloop:
            // two elements at a time:
            movupd xmm1, [ecx + edx];
            movupd xmm2, [ebx + edx];
            mulpd xmm1, xmm2;
            addpd xmm0, xmm1;

         add edx, 16;
         jnz @@foriloop;

         @@foriloopend:

         // final result
         haddpd xmm0, xmm0;
         movsd [eax + esi], xmm0;

         add ebx, LineWidth2;
         add esi, 8;

      mov edx, testExitLoopVal;
      cmp esi, edx;
      jne @@forxloop;

      add ecx, LineWidth1;
      add eax, aLineWidthDest;
   dec height1;
   jnz @@foryloop;

   // epilog
   pop edi;
   pop esi;
   pop ebx;
end;

procedure ASMSymRank2UpdateUpperUnaligned( C : PDouble; LineWidthC : NativeInt; A : PDouble; LineWidthA : NativeInt;
  B : PDouble; LineWidthB : NativeInt; N : NativeInt; k : NativeInt );
// eax = c; edx = LineWidthC; ecx : A;
var i, j : integer;
    lC : Cardinal;
asm
   // save register
   push ebx;
   push edi;
   push esi;

   // switch register
   mov lC, edx;

   // eax -> pointer to C
   // ecx -> ponter to pA1
   // esi -> pointer to pB1
   mov esi, B;

   mov edi, k;
   imul edi, -8;
   mov k, edi;

   sub ecx, edi;
   sub esi, edi;

   mov edi, N;
   shl edi, 3;
   add eax, edi;


   // for j := 0 to N - 1 do
   //
   neg edi;
   mov j, edi;
   @@forNLoop:
       mov edx, j;
       mov i, edx;

       // ###########################################
       // #### Init A2 and B2
       // pA2
       mov ebx, ecx;
       // pB2
       mov edi, esi;


       @@forjNLoop:
            xorpd xmm0, xmm0;

            // ###########################################
            // #### Init loop
            mov edx, k;

            // accumulate
            @@forlLoop:
                add edx, 16;
                jg @@forlLoopEnd;

                movupd xmm1, [ecx + edx - 16]; // pA1;
                movupd xmm2, [edi + edx - 16]; // pB2
                movupd xmm3, [esi + edx - 16]; // pB1;
                movupd xmm4, [ebx + edx - 16]; // pA2;

                mulpd xmm1, xmm2;
                addpd xmm0, xmm1;
                mulpd xmm3, xmm4;
                addpd xmm0, xmm3;
            jmp @@forlLoop;

            @@forlLoopEnd:

            // is iterator actualy zero? (aka even k)
            cmp edx, 16;
            je @@NextN;

            // last element
            movsd xmm1, [ecx - 8]; // pA1;
            movsd xmm2, [edi - 8]; // pB2
            movsd xmm3, [esi - 8]; // pB1;
            movsd xmm4, [ebx - 8]; // pA2;

            mulsd xmm1, xmm2;
            addsd xmm0, xmm1;
            mulsd xmm3, xmm4;
            addsd xmm0, xmm3;

            @@NextN:

            // ###########################################
            // #### pC^[i] := pC^[i] - xmm0
            mov edx, i;
            haddpd xmm0, xmm0;
            movsd xmm1, [eax + edx];

            subsd xmm1, xmm0;
            movsd [eax + edx], xmm1;

            add ebx, LineWidthA;  // increment pA2
            add edi, LineWidthB;  // increment pB2

       add i, 8;
       jnz @@forjNLoop;

       // next line
       add eax, lC;
       add ecx, LineWidthA;
       add esi, LineWidthB;
   add j, 8;
   jnz @@forNLoop;

   // ###########################################
   // #### Finalize stack
   pop esi;
   pop edi;
   pop ebx;
end;


{$ENDIF}

end.
