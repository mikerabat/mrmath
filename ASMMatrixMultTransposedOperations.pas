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


unit ASMMatrixMultTransposedOperations;

// ##############################################################
// #### Assembler optimized matrix multiplication assuming a transposed second
// #### matrix
// ##############################################################

interface

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}
{$IFNDEF x64}

uses MatrixConst;

procedure ASMMatrixMultAlignedEvenW1EvenH2Transposed(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixMultAlignedEvenW1EvenH2TransposedMod16(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixMultUnAlignedEvenW1EvenH2Transposed(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

procedure ASMMatrixMultAlignedEvenW1OddH2Transposed(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixMultUnAlignedEvenW1OddH2Transposed(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

procedure ASMMatrixMultAlignedOddW1EvenH2Transposed(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixMultUnAlignedOddW1EvenH2Transposed(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

procedure ASMMatrixMultAlignedOddW1OddH2Transposed(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMatrixMultUnAlignedOddW1OddH2Transposed(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

{$ENDIF}

implementation

{$IFNDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$S-} {$ENDIF}

procedure ASMMatrixMultUnAlignedEvenW1OddH2Transposed(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var iters : TASMNativeInt;
    LineWidth2_2 : TASMNativeInt;
    destOffset : TASMNativeInt;
// eax = dest, edx = destlinewidth, ecx = mt1
asm
   push ebx;
   push esi;
   push edi;
   
   // init
   // destOffset := destLineWidth - height2*sizeof(double);
   mov esi, height2;
   shl esi, 3;
   sub edx, esi;
   add edx, 8;
   mov destOffset, edx;

   
   // height2 div 2;
   sar esi, 4;
   mov height2, esi;
   
   // iters := -width1*sizeof(double);
   mov ebx, width1;
   imul ebx, -8;
   mov iters, ebx;

   
   // LineWidth2_2 := 2*LineWidth2;
   mov esi, LineWidth2;
   shl esi, 1;
   mov LineWidth2_2, esi;
   
   // prepare matrix pointers - remove constant offset here instead each time in the loop:
   mov edi, mt2;
   sub ecx, ebx;
   sub edi, ebx;
   mov mt2, edi;

   // for y := 0 to height1 - 1:
   @@forylabel:
       // store pointer to mt2
       mov edi, mt2;

       // for x := 0 to height2 div 2 - 1:
       mov ebx, height2;
       cmp ebx, 0;
       je @@fory2labelexit;
       @@fory2label:
           // esi points to next row of mt2
           mov esi, edi;
           add esi, LineWidth2;
                
           xorpd xmm0, xmm0;   // dest^ := 0
           xorpd xmm7, xmm7;   // dest + 1 := 0;
           // for idx := 0 to width1 div 2 do
           mov edx, iters;

           @@InnerLoop:
               // prefetch [rcx + rdx + 128];
               // prefetch [edi + rdx + 128];
               // prefetch [esi + rdx + 128];

               movupd xmm1, [ecx + edx];

               // load 2x2 block
               movupd xmm3, [edi + edx];
               movupd xmm4, [esi + edx];

               // multiply 2x2 and add
               mulpd xmm3, xmm1;
               mulpd xmm4, xmm1;

               addpd xmm0, xmm3;
               addpd xmm7, xmm4;

               // end for idx := 0 to width1 div 2
           add edx, 16;
           jnz @@InnerLoop;

           // add an compact result
           haddpd xmm0, xmm7;

           // store back result
           movupd [eax], xmm0;

           // increment the pointers
           add eax, 16;
           add edi, LineWidth2_2;
       // end for x := 0 to width2 - 1
       dec ebx;
       jnz @@fory2label;

       @@fory2labelexit:

       // take care of the last line separatly
       xorpd xmm0, xmm0;   // dest^ := 0
       // for idx := 0 to width1 div 2 do
       mov edx, iters;

       @@InnerLoop2:
           movupd xmm1, [ecx + edx];
           movupd xmm3, [edi + edx];

           // multiply 2x2 and add
           mulpd xmm3, xmm1;
           addpd xmm0, xmm3;

           // end for idx := 0 to width1 div 2
       add edx, 16;
       jnz @@InnerLoop2;

       haddpd xmm0, xmm0;

       // store back result
       movsd [eax], xmm0;

       // dec(mt2, Width2);
       // inc(PByte(mt1), LineWidth1);
       // inc(PByte(dest), destOffset);
       add ecx, LineWidth1;
       add eax, destOffset;

   // end for y := 0 to height1 - 1
   dec Height1;
   jnz @@forylabel;

   pop edi;
   pop esi;
   pop ebx;
end;

procedure ASMMatrixMultAlignedEvenW1OddH2Transposed(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var iters : TASMNativeInt;
    LineWidth2_2 : TASMNativeInt;
    destOffset : TASMNativeInt;
// eax = dest, edx = destlinewidth, ecx = mt1
asm
   push ebx;
   push esi;
   push edi;
   
   // init
   // destOffset := destLineWidth - height2*sizeof(double);
   mov esi, height2;
   shl esi, 3;
   sub edx, esi;
   add edx, 8;
   mov destOffset, edx;

   
   // height2 div 2;
   sar esi, 4;
   mov height2, esi;
   
   // iters := -width1*sizeof(double);
   mov ebx, width1;
   imul ebx, -8;
   mov iters, ebx;

   
   // LineWidth2_2 := 2*LineWidth2;
   mov esi, LineWidth2;
   shl esi, 1;
   mov LineWidth2_2, esi;
   
   // prepare matrix pointers - remove constant offset here instead each time in the loop:
   mov edi, mt2;
   sub ecx, ebx;
   sub edi, ebx;
   mov mt2, edi;

   // for y := 0 to height1 - 1:
   @@forylabel:
       // store pointer to mt2
       mov edi, mt2;

       // for x := 0 to height2 div 2 - 1:
       mov ebx, height2;
       cmp ebx, 0;
       je @@fory2labelexit;
       @@fory2label:
           // esi points to next row of mt2
           mov esi, edi;
           add esi, LineWidth2;
                
           xorpd xmm0, xmm0;   // dest^ := 0
           xorpd xmm7, xmm7;   // dest + 1 := 0;
           // for idx := 0 to width1 div 2 do
           mov edx, iters;

           @@InnerLoop:
               // prefetch [rcx + rdx + 128];
               // prefetch [edi + rdx + 128];
               // prefetch [esi + rdx + 128];

               movapd xmm1, [ecx + edx];

               // load 2x2 block
               movapd xmm3, [edi + edx];
               movapd xmm4, [esi + edx];

               // multiply 2x2 and add
               mulpd xmm3, xmm1;
               mulpd xmm4, xmm1;

               addpd xmm0, xmm3;
               addpd xmm7, xmm4;

               // end for idx := 0 to width1 div 2
           add edx, 16;
           jnz @@InnerLoop;

           // add an compact result
           haddpd xmm0, xmm7;

           // store back result
           movapd [eax], xmm0;

           // increment the pointers
           add eax, 16;
           add edi, LineWidth2_2;
       // end for x := 0 to width2 - 1
       dec ebx;
       jnz @@fory2label;

       @@fory2labelexit:

       // take care of the last line separatly
       xorpd xmm0, xmm0;   // dest^ := 0
       // for idx := 0 to width1 div 2 do
       mov edx, iters;

       @@InnerLoop2:
           movapd xmm1, [ecx + edx];
           movapd xmm3, [edi + edx];

           // multiply 2x2 and add
           mulpd xmm3, xmm1;
           addpd xmm0, xmm3;

           // end for idx := 0 to width1 div 2
       add edx, 16;
       jnz @@InnerLoop2;

       haddpd xmm0, xmm0;

       // store back result
       movsd [eax], xmm0;

       // dec(mt2, Width2);
       // inc(PByte(mt1), LineWidth1);
       // inc(PByte(dest), destOffset);
       add ecx, LineWidth1;
       add eax, destOffset;

   // end for y := 0 to height1 - 1
   dec Height1;
   jnz @@forylabel;

   pop edi;
   pop esi;
   pop ebx;
end;

procedure ASMMatrixMultAlignedEvenW1EvenH2TransposedMod16(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var iters : TASMNativeInt;
    LineWidth2_2 : TASMNativeInt;
    destOffset : TASMNativeInt;
// eax = dest, edx = destlinewidth, ecx = mt1
asm
   push ebx;
   push esi;
   push edi;
   
   // init
   // destOffset := destLineWidth - height2*sizeof(double);
   mov esi, height2;
   shl esi, 3;
   sub edx, esi;
   mov destOffset, edx;

   
   // height2 div 2;
   sar esi, 4;
   mov height2, esi;
   
   // iters := -width1*sizeof(double);
   mov ebx, width1;
   imul ebx, -8;
   mov iters, ebx;

   
   // LineWidth2_2 := 2*LineWidth2;
   mov esi, LineWidth2;
   shl esi, 1;
   mov LineWidth2_2, esi;
   
   // prepare matrix pointers - remove constant offset here instead each time in the loop:
   mov edi, mt2;
   sub ecx, ebx;
   sub edi, ebx;
   mov mt2, edi;

   // for y := 0 to height1 - 1:
   @@forylabel:
       // store pointer to mt2
       mov edi, mt2;

       // for x := 0 to height2 div 2 - 1:
       mov ebx, height2;
       @@fory2label:
           // esi points to next row of mt2
           mov esi, edi;
           add esi, LineWidth2;
                
           xorpd xmm0, xmm0;   // dest^ := 0
           xorpd xmm7, xmm7;   // dest + 1 := 0;
           // for idx := 0 to width1 div 2 do
           mov edx, iters;

           @@InnerLoop:
               // prefetch [rcx + rdx + 128];
               // prefetch [edi + rdx + 128];
               // prefetch [esi + rdx + 128];

               movapd xmm1, [ecx + edx];

               // load 2x2 block
               movapd xmm3, [edi + edx];
               movapd xmm4, [esi + edx];

               // multiply 2x2 and add
               mulpd xmm3, xmm1;
               mulpd xmm4, xmm1;

               addpd xmm0, xmm3;
               addpd xmm7, xmm4;

               movapd xmm1, [ecx + edx + 16];

               // load 2x2 block
               movapd xmm3, [edi + edx + 16];
               movapd xmm4, [esi + edx + 16];

               // multiply 2x2 and add
               mulpd xmm3, xmm1;
               mulpd xmm4, xmm1;

               addpd xmm0, xmm3;
               addpd xmm7, xmm4;

               movapd xmm1, [ecx + edx + 32];

               // load 2x2 block
               movapd xmm3, [edi + edx + 32];
               movapd xmm4, [esi + edx + 32];

               // multiply 2x2 and add
               mulpd xmm3, xmm1;
               mulpd xmm4, xmm1;

               addpd xmm0, xmm3;
               addpd xmm7, xmm4;

               movapd xmm1, [ecx + edx + 48];

               // load 2x2 block
               movapd xmm3, [edi + edx + 48];
               movapd xmm4, [esi + edx + 48];

               // multiply 2x2 and add
               mulpd xmm3, xmm1;
               mulpd xmm4, xmm1;

               addpd xmm0, xmm3;
               addpd xmm7, xmm4;


               movapd xmm1, [ecx + edx + 64];

               // load 2x2 block
               movapd xmm3, [edi + edx + 64];
               movapd xmm4, [esi + edx + 64];

               // multiply 2x2 and add
               mulpd xmm3, xmm1;
               mulpd xmm4, xmm1;

               addpd xmm0, xmm3;
               addpd xmm7, xmm4;

               movapd xmm1, [ecx + edx + 80];

               // load 2x2 block
               movapd xmm3, [edi + edx + 80];
               movapd xmm4, [esi + edx + 80];

               // multiply 2x2 and add
               mulpd xmm3, xmm1;
               mulpd xmm4, xmm1;

               addpd xmm0, xmm3;
               addpd xmm7, xmm4;

               movapd xmm1, [ecx + edx + 96];

               // load 2x2 block
               movapd xmm3, [edi + edx + 96];
               movapd xmm4, [esi + edx + 96];

               // multiply 2x2 and add
               mulpd xmm3, xmm1;
               mulpd xmm4, xmm1;

               addpd xmm0, xmm3;
               addpd xmm7, xmm4;

               movapd xmm1, [ecx + edx + 112];

               // load 2x2 block
               movapd xmm3, [edi + edx + 112];
               movapd xmm4, [esi + edx + 112];

               // multiply 2x2 and add
               mulpd xmm3, xmm1;
               mulpd xmm4, xmm1;

               addpd xmm0, xmm3;
               addpd xmm7, xmm4;

               // end for idx := 0 to width1 div 2
           add edx, 128;
           jnz @@InnerLoop;

           // add an compact result
           haddpd xmm0, xmm7;

           // store back result
           movapd [eax], xmm0;

           // increment the pointers
           add eax, 16;
           add edi, LineWidth2_2;
       // end for x := 0 to width2 - 1
       dec ebx;
       jnz @@fory2label;

       // dec(mt2, Width2);
       // inc(PByte(mt1), LineWidth1);
       // inc(PByte(dest), destOffset);
       add ecx, LineWidth1;
       add eax, destOffset;

   // end for y := 0 to height1 - 1
   dec Height1;
   jnz @@forylabel;

   pop edi;
   pop esi;
   pop ebx;
end;

// note mt2 is transposed this time -> width1 and width2 must be the same!
procedure ASMMatrixMultAlignedEvenW1EvenH2Transposed(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var iters : TASMNativeInt;
    LineWidth2_2 : TASMNativeInt;
    destOffset : TASMNativeInt;
// eax = dest, edx = destlinewidth, ecx = mt1
asm
   push ebx;
   push esi;
   push edi;
   
   // init
   // destOffset := destLineWidth - height2*sizeof(double);
   mov esi, height2;
   shl esi, 3;
   sub edx, esi;
   mov destOffset, edx;

   
   // height2 div 2;
   sar esi, 4;
   mov height2, esi;
   
   // iters := -width1*sizeof(double);
   mov ebx, width1;
   imul ebx, -8;
   mov iters, ebx;

   
   // LineWidth2_2 := 2*LineWidth2;
   mov esi, LineWidth2;
   shl esi, 1;
   mov LineWidth2_2, esi;
   
   // prepare matrix pointers - remove constant offset here instead each time in the loop:
   mov edi, mt2;
   sub ecx, ebx;
   sub edi, ebx;
   mov mt2, edi;

   // for y := 0 to height1 - 1:
   @@forylabel:
       // store pointer to mt2
       mov edi, mt2;

       // for x := 0 to height2 div 2 - 1:
       mov ebx, height2;
       @@fory2label:
           // esi points to next row of mt2
           mov esi, edi;
           add esi, LineWidth2;
                
           xorpd xmm0, xmm0;   // dest^ := 0
           xorpd xmm7, xmm7;   // dest + 1 := 0;
           // for idx := 0 to width1 div 2 do
           mov edx, iters;

           @@InnerLoop:
               // prefetch [rcx + rdx + 128];
               // prefetch [edi + rdx + 128];
               // prefetch [esi + rdx + 128];

               movapd xmm1, [ecx + edx];

               // load 2x2 block
               movapd xmm3, [edi + edx];
               movapd xmm4, [esi + edx];

               // multiply 2x2 and add
               mulpd xmm3, xmm1;
               mulpd xmm4, xmm1;

               addpd xmm0, xmm3;
               addpd xmm7, xmm4;

               // end for idx := 0 to width1 div 2
           add edx, 16;
           jnz @@InnerLoop;

           // add an compact result
           haddpd xmm0, xmm7;

           // store back result
           movapd [eax], xmm0;

           // increment the pointers
           add eax, 16;
           add edi, LineWidth2_2;
       // end for x := 0 to width2 - 1
       dec ebx;
       jnz @@fory2label;

       // dec(mt2, Width2);
       // inc(PByte(mt1), LineWidth1);
       // inc(PByte(dest), destOffset);
       add ecx, LineWidth1;
       add eax, destOffset;

   // end for y := 0 to height1 - 1
   dec Height1;
   jnz @@forylabel;

   pop edi;
   pop esi;
   pop ebx;
end;

procedure ASMMatrixMultUnAlignedEvenW1EvenH2Transposed(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var iters : TASMNativeInt;
    LineWidth2_2 : TASMNativeInt;
    destOffset : TASMNativeInt;
// eax = dest, edx = destlinewidth, ecx = mt1
asm
   push ebx;
   push esi;
   push edi;
   
   // init
   // destOffset := destLineWidth - height2*sizeof(double);
   mov esi, height2;
   shl esi, 3;
   sub edx, esi;
   mov destOffset, edx;

   
   // height2 div 2;
   sar esi, 4;
   mov height2, esi;
   
   // iters := -width1*sizeof(double);
   mov ebx, width1;
   imul ebx, -8;
   mov iters, ebx;

   
   // LineWidth2_2 := 2*LineWidth2;
   mov esi, LineWidth2;
   shl esi, 1;
   mov LineWidth2_2, esi;
   
   // prepare matrix pointers - remove constant offset here instead each time in the loop:
   mov edi, mt2;
   sub ecx, ebx;
   sub edi, ebx;
   mov mt2, edi;

   // for y := 0 to height1 - 1:
   @@forylabel:
       // store pointer to mt2
       mov edi, mt2;

       // for x := 0 to height2 div 2 - 1:
       mov ebx, height2;
       @@fory2label:
           // esi points to next row of mt2
           mov esi, edi;
           add esi, LineWidth2;
                
           xorpd xmm0, xmm0;   // dest^ := 0
           xorpd xmm7, xmm7;   // dest + 1 := 0;
           // for idx := 0 to width1 div 2 do
           mov edx, iters;

           @@InnerLoop:
               // prefetch [rcx + rdx + 128];
               // prefetch [edi + rdx + 128];
               // prefetch [esi + rdx + 128];

               movupd xmm1, [ecx + edx];

               // load 2x2 block
               movupd xmm3, [edi + edx];
               movupd xmm4, [esi + edx];

               // multiply 2x2 and add
               mulpd xmm3, xmm1;
               mulpd xmm4, xmm1;

               addpd xmm0, xmm3;
               addpd xmm7, xmm4;

               // end for idx := 0 to width1 div 2
           add edx, 16;
           jnz @@InnerLoop;

           // add an compact result
           haddpd xmm0, xmm7;

           // store back result
           movupd [eax], xmm0;

           // increment the pointers
           add eax, 16;
           add edi, LineWidth2_2;
       // end for x := 0 to width2 - 1
       dec ebx;
       jnz @@fory2label;

       // dec(mt2, Width2);
       // inc(PByte(mt1), LineWidth1);
       // inc(PByte(dest), destOffset);
       add ecx, LineWidth1;
       add eax, destOffset;

   // end for y := 0 to height1 - 1
   dec Height1;
   jnz @@forylabel;

   pop edi;
   pop esi;
   pop ebx;
end;

procedure ASMMatrixMultUnAlignedOddW1EvenH2Transposed(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var iters : TASMNativeInt;
    LineWidth2_2 : TASMNativeInt;
    destOffset : TASMNativeInt;
// eax = dest, edx = destlinewidth, ecx = mt1
asm
   push ebx;
   push esi;
   push edi;
   
   // init
   // destOffset := destLineWidth - height2*sizeof(double);
   mov esi, height2;
   shl esi, 3;
   sub edx, esi;
   mov destOffset, edx;

   
   // height2 div 2;
   sar esi, 4;
   mov height2, esi;
   
   // iters := -(width1 - 1)*sizeof(double);
   mov ebx, width1;
   dec ebx;
   imul ebx, -8;
   mov iters, ebx;

   
   // LineWidth2_2 := 2*LineWidth2;
   mov esi, LineWidth2;
   shl esi, 1;
   mov LineWidth2_2, esi;
   
   // prepare matrix pointers - remove constant offset here instead each time in the loop:
   mov edi, mt2;
   sub ecx, ebx;
   sub edi, ebx;
   mov mt2, edi;

   // for y := 0 to height1 - 1:
   @@forylabel:
       // store pointer to mt2
       mov edi, mt2;

       // for x := 0 to height2 div 2 - 1:
       mov ebx, height2;
       @@fory2label:
           // esi points to next row of mt2
           mov esi, edi;
           add esi, LineWidth2;
                
           xorpd xmm0, xmm0;   // dest^ := 0
           xorpd xmm7, xmm7;   // dest + 1 := 0;
           // for idx := 0 to width1 div 2 do
           mov edx, iters;

           @@InnerLoop:
               // prefetch [rcx + rdx + 128];
               // prefetch [edi + rdx + 128];
               // prefetch [esi + rdx + 128];

               movupd xmm1, [ecx + edx];

               // load 2x2 block
               movupd xmm3, [edi + edx];
               movupd xmm4, [esi + edx];

               // multiply 2x2 and add
               mulpd xmm3, xmm1;
               mulpd xmm4, xmm1;

               addpd xmm0, xmm3;
               addpd xmm7, xmm4;

               // end for idx := 0 to width1 div 2
           add edx, 16;
           jnz @@InnerLoop;

           // multiply and add the last element
           movsd xmm1, [ecx];

           movsd xmm3, [edi];
           movsd xmm4, [esi];

           mulsd xmm3, xmm1;
           mulsd xmm4, xmm1;

           addsd xmm0, xmm3;
           addsd xmm7, xmm4;

           // add an compact result
           haddpd xmm0, xmm7;

           // store back result
           movupd [eax], xmm0;

           // increment the pointers
           add eax, 16;
           add edi, LineWidth2_2;
       // end for x := 0 to width2 - 1
       dec ebx;
       jnz @@fory2label;

       // dec(mt2, Width2);
       // inc(PByte(mt1), LineWidth1);
       // inc(PByte(dest), destOffset);
       add ecx, LineWidth1;
       add eax, destOffset;

   // end for y := 0 to height1 - 1
   dec Height1;
   jnz @@forylabel;

   pop edi;
   pop esi;
   pop ebx;
end;

procedure ASMMatrixMultAlignedOddW1EvenH2Transposed(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var iters : TASMNativeInt;
    LineWidth2_2 : TASMNativeInt;
    destOffset : TASMNativeInt;
// eax = dest, edx = destlinewidth, ecx = mt1
asm
   push ebx;
   push esi;
   push edi;
   
   // init
   // destOffset := destLineWidth - height2*sizeof(double);
   mov esi, height2;
   shl esi, 3;
   sub edx, esi;
   mov destOffset, edx;

   
   // height2 div 2;
   sar esi, 4;
   mov height2, esi;
   
   // iters := -(width1 - 1)*sizeof(double);
   mov ebx, width1;
   dec ebx;
   imul ebx, -8;
   mov iters, ebx;

   
   // LineWidth2_2 := 2*LineWidth2;
   mov esi, LineWidth2;
   shl esi, 1;
   mov LineWidth2_2, esi;
   
   // prepare matrix pointers - remove constant offset here instead each time in the loop:
   mov edi, mt2;
   sub ecx, ebx;
   sub edi, ebx;
   mov mt2, edi;

   // for y := 0 to height1 - 1:
   @@forylabel:
       // store pointer to mt2
       mov edi, mt2;

       // for x := 0 to height2 div 2 - 1:
       mov ebx, height2;
       @@fory2label:
           // esi points to next row of mt2
           mov esi, edi;
           add esi, LineWidth2;
                
           xorpd xmm0, xmm0;   // dest^ := 0
           xorpd xmm7, xmm7;   // dest + 1 := 0;
           // for idx := 0 to width1 div 2 do
           mov edx, iters;

           @@InnerLoop:
               // prefetch [rcx + rdx + 128];
               // prefetch [edi + rdx + 128];
               // prefetch [esi + rdx + 128];

               movapd xmm1, [ecx + edx];

               // load 2x2 block
               movapd xmm3, [edi + edx];
               movapd xmm4, [esi + edx];

               // multiply 2x2 and add
               mulpd xmm3, xmm1;
               mulpd xmm4, xmm1;

               addpd xmm0, xmm3;
               addpd xmm7, xmm4;

               // end for idx := 0 to width1 div 2
           add edx, 16;
           jnz @@InnerLoop;

           // multiply and add the last element
           movsd xmm1, [ecx];

           movsd xmm3, [edi];
           movsd xmm4, [esi];

           mulsd xmm3, xmm1;
           mulsd xmm4, xmm1;

           addsd xmm0, xmm3;
           addsd xmm7, xmm4;

           // add an compact result
           haddpd xmm0, xmm7;

           // store back result
           movapd [eax], xmm0;

           // increment the pointers
           add eax, 16;
           add edi, LineWidth2_2;
       // end for x := 0 to width2 - 1
       dec ebx;
       jnz @@fory2label;

       // dec(mt2, Width2);
       // inc(PByte(mt1), LineWidth1);
       // inc(PByte(dest), destOffset);
       add ecx, LineWidth1;
       add eax, destOffset;

   // end for y := 0 to height1 - 1
   dec Height1;
   jnz @@forylabel;

   pop edi;
   pop esi;
   pop ebx;
end;

procedure ASMMatrixMultAlignedOddW1OddH2Transposed(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var iters : TASMNativeInt;
    LineWidth2_2 : TASMNativeInt;
    destOffset : TASMNativeInt;
// eax = dest, edx = destlinewidth, ecx = mt1
asm
   push ebx;
   push esi;
   push edi;
   
   // init
   // destOffset := destLineWidth - height2*sizeof(double);
   mov esi, height2;
   shl esi, 3;
   sub edx, esi;
   add edx, 8;
   mov destOffset, edx;

   // height2 div 2;
   sar esi, 4;
   mov height2, esi;
   
   // iters := -(width1 - 1)*sizeof(double);
   mov ebx, width1;
   dec ebx;
   imul ebx, -8;
   mov iters, ebx;

   // LineWidth2_2 := 2*LineWidth2;
   mov esi, LineWidth2;
   shl esi, 1;
   mov LineWidth2_2, esi;
   
   // prepare matrix pointers - remove constant offset here instead each time in the loop:
   mov edi, mt2;
   sub ecx, ebx;
   sub edi, ebx;
   mov mt2, edi;

   // for y := 0 to height1 - 1:
   @@forylabel:
       // store pointer to mt2
       mov edi, mt2;

       // for x := 0 to height2 div 2 - 1:
       mov ebx, height2;
       cmp ebx, 0;
       je @@fory2labelexit;
       @@fory2label:
           // esi points to next row of mt2
           mov esi, edi;
           add esi, LineWidth2;
                
           xorpd xmm0, xmm0;   // dest^ := 0
           xorpd xmm7, xmm7;   // dest + 1 := 0;
           // for idx := 0 to width1 div 2 do
           mov edx, iters;

           @@InnerLoop:
               // prefetch [rcx + rdx + 128];
               // prefetch [edi + rdx + 128];
               // prefetch [esi + rdx + 128];

               movapd xmm1, [ecx + edx];

               // load 2x2 block
               movapd xmm3, [edi + edx];
               movapd xmm4, [esi + edx];

               // multiply 2x2 and add
               mulpd xmm3, xmm1;
               mulpd xmm4, xmm1;

               addpd xmm0, xmm3;
               addpd xmm7, xmm4;

               // end for idx := 0 to width1 div 2
           add edx, 16;
           jnz @@InnerLoop;

           // multiply and add the last element
           movsd xmm1, [ecx];

           movsd xmm3, [edi];
           movsd xmm4, [esi];

           mulsd xmm3, xmm1;
           mulsd xmm4, xmm1;

           addsd xmm0, xmm3;
           addsd xmm7, xmm4;
           
           // add an compact result
           haddpd xmm0, xmm7;

           // store back result
           movapd [eax], xmm0;

           // increment the pointers
           add eax, 16;
           add edi, LineWidth2_2;
       // end for x := 0 to width2 - 1
       dec ebx;
       jnz @@fory2label;

       @@fory2labelexit:
       
       // take care of the last line separatly
       xorpd xmm0, xmm0;   // dest^ := 0
       // for idx := 0 to width1 div 2 do
       mov edx, iters;

       @@InnerLoop2:
           movapd xmm1, [ecx + edx];
           movapd xmm3, [edi + edx];

           // multiply 2x2 and add
           mulpd xmm3, xmm1;
           addpd xmm0, xmm3;

           // end for idx := 0 to width1 div 2
       add edx, 16;
       jnz @@InnerLoop2;

       haddpd xmm0, xmm0;
       
       // last element
       movsd xmm1, [ecx];
       movsd xmm3, [edi];

       mulsd xmm3, xmm1;
       addsd xmm0, xmm3;

       // store back result
       movsd [eax], xmm0;

       // dec(mt2, Width2);
       // inc(PByte(mt1), LineWidth1);
       // inc(PByte(dest), destOffset);
       add ecx, LineWidth1;
       add eax, destOffset;

   // end for y := 0 to height1 - 1
   dec Height1;
   jnz @@forylabel;

   pop edi;
   pop esi;
   pop ebx;
end;

procedure ASMMatrixMultUnAlignedOddW1OddH2Transposed(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var iters : TASMNativeInt;
    LineWidth2_2 : TASMNativeInt;
    destOffset : TASMNativeInt;
// eax = dest, edx = destlinewidth, ecx = mt1
asm
   push ebx;
   push esi;
   push edi;
   
   // init
   // destOffset := destLineWidth - height2*sizeof(double);
   mov esi, height2;
   shl esi, 3;
   sub edx, esi;
   add edx, 8;
   mov destOffset, edx;

   // height2 div 2;
   sar esi, 4;
   mov height2, esi;
   
   // iters := -(width1 - 1)*sizeof(double);
   mov ebx, width1;
   dec ebx;
   imul ebx, -8;
   mov iters, ebx;

   // LineWidth2_2 := 2*LineWidth2;
   mov esi, LineWidth2;
   shl esi, 1;
   mov LineWidth2_2, esi;
   
   // prepare matrix pointers - remove constant offset here instead each time in the loop:
   mov edi, mt2;
   sub ecx, ebx;
   sub edi, ebx;
   mov mt2, edi;

   // for y := 0 to height1 - 1:
   @@forylabel:
       // store pointer to mt2
       mov edi, mt2;

       // for x := 0 to height2 div 2 - 1:
       mov ebx, height2;
       cmp ebx, 0;
       je @@fory2labelexit;
       @@fory2label:
           // esi points to next row of mt2
           mov esi, edi;
           add esi, LineWidth2;
                
           xorpd xmm0, xmm0;   // dest^ := 0
           xorpd xmm7, xmm7;   // dest + 1 := 0;
           // for idx := 0 to width1 div 2 do
           mov edx, iters;

           @@InnerLoop:
               // prefetch [rcx + rdx + 128];
               // prefetch [edi + rdx + 128];
               // prefetch [esi + rdx + 128];

               movupd xmm1, [ecx + edx];

               // load 2x2 block
               movupd xmm3, [edi + edx];
               movupd xmm4, [esi + edx];

               // multiply 2x2 and add
               mulpd xmm3, xmm1;
               mulpd xmm4, xmm1;

               addpd xmm0, xmm3;
               addpd xmm7, xmm4;

               // end for idx := 0 to width1 div 2
           add edx, 16;
           jnz @@InnerLoop;

           // multiply and add the last element
           movsd xmm1, [ecx];

           movsd xmm3, [edi];
           movsd xmm4, [esi];

           mulsd xmm3, xmm1;
           mulsd xmm4, xmm1;

           addsd xmm0, xmm3;
           addsd xmm7, xmm4;
           
           // add an compact result
           haddpd xmm0, xmm7;

           // store back result
           movupd [eax], xmm0;

           // increment the pointers
           add eax, 16;
           add edi, LineWidth2_2;
       // end for x := 0 to width2 - 1
       dec ebx;
       jnz @@fory2label;

       @@fory2labelexit:
       
       // take care of the last line separatly
       xorpd xmm0, xmm0;   // dest^ := 0
       // for idx := 0 to width1 div 2 do
       mov edx, iters;

       @@InnerLoop2:
           movupd xmm1, [ecx + edx];
           movupd xmm3, [edi + edx];

           // multiply 2x2 and add
           mulpd xmm3, xmm1;
           addpd xmm0, xmm3;

           // end for idx := 0 to width1 div 2
       add edx, 16;
       jnz @@InnerLoop2;

       haddpd xmm0, xmm0;
       
       // last element
       movsd xmm1, [ecx];
       movsd xmm3, [edi];

       mulsd xmm3, xmm1;
       addsd xmm0, xmm3;

       // store back result
       movsd [eax], xmm0;

       // dec(mt2, Width2);
       // inc(PByte(mt1), LineWidth1);
       // inc(PByte(dest), destOffset);
       add ecx, LineWidth1;
       add eax, destOffset;

   // end for y := 0 to height1 - 1
   dec Height1;
   jnz @@forylabel;

   pop edi;
   pop esi;
   pop ebx;
end;

{$ENDIF}

end.
