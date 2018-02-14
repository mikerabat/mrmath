// ###################################################################
// #### This file is part of the mathematics library project, and is
// #### offered under the licence agreement described on
// #### http://www.mrsoft.org/
// ####
// #### Copyright:(c) 2018, Michael R. . All rights reserved.
// ####
// #### Unless required by applicable law or agreed to in writing, software
// #### distributed under the License is distributed on an "AS IS" BASIS,
// #### WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// #### See the License for the specific language governing permissions and
// #### limitations under the License.
// ###################################################################


unit AVXMatrixMultTransposedOperations;

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



procedure AVXMatrixMultAlignedTransposed(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
procedure AVXMatrixMultAlignedEvenW1EvenH2TransposedMod16(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
procedure AVXMatrixMultAlignedEvenW1OddH2TransposedMod16(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
procedure AVXMatrixMultUnAlignedTransposed(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);

{$ENDIF}

implementation

{$IFNDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$ENDIF}

procedure AVXMatrixMultAlignedEvenW1EvenH2TransposedMod16(dest : PDouble; const destLineWidth : TASMNativeInt;
    mt1, mt2 : PDouble; width1 : TASMNativeInt;
    height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt;
    const LineWidth1, LineWidth2 : TASMNativeInt);
var iter : TASMNativeInt;
    LineWidth2x2 : TASMNativeInt;
{$IFDEF FPC}
begin
{$ENDIF}
asm
   // prolog - simulate stack
   push ebx;
   push edi;
   push esi;

   mov eax, LineWidth2;
   add eax, eax;
   mov LineWidth2x2, eax;

   // iters2 := -width1*sizeof(double);
   mov eax, width1;
   imul eax, -8;
   mov iter, eax;

   // prepare matrix pointers - remove constant offset here instead each time in the loop:

   // ecx=dest, edx=mt1, ebx=mt2, edi = mt2 + linewidth2
   sub mt1, eax;
   sub mt2, eax;

   // for y := 0 to height1 - 1:
   @@forylabel:
       mov ecx, dest;
       mov edx, mt1;
       mov ebx, mt2;
       mov edi, ebx;
       add edi, LineWidth2;

       // for y2 := 0 to height2 - 1:
       mov esi, height2;
       @@fory2label:
           vxorpd ymm0, ymm0, ymm0;   // dest^ := 0
           vxorpd ymm2, ymm2, ymm2;   // dest + 1 := 0;
           // for idx := 0 to width1 div 2 do
           mov eax, iter;

           // for x := 0 to width1 - 1
           @@InnerLoop:
               // prefetch [ecx + edx + 128];
               // prefetch [edi + edx + 128];
               // prefetch [eax + edx + 128];

               vmovapd ymm1, [edx + eax];

               // multiply 2x2 and add
               vmulpd ymm3, ymm1, [ebx + eax];
               vmulpd ymm4, ymm1, [edi + eax];

               vaddpd ymm0, ymm0, ymm3;
               vaddpd ymm2, ymm2, ymm4;

               vmovapd ymm1, [edx + eax + 32];

               // multiply 2x2 and add
               vmulpd ymm3, ymm1, [ebx + eax + 32];
               vmulpd ymm4, ymm1, [edi + eax + 32];

               vaddpd ymm0, ymm0, ymm3;
               vaddpd ymm2, ymm2, ymm4;

               vmovapd ymm1, [edx + eax + 64];

               // multiply 2x2 and add
               vmulpd ymm3, ymm1, [ebx + eax + 64];
               vmulpd ymm4, ymm1, [edi + eax + 64];

               vaddpd ymm0, ymm0, ymm3;
               vaddpd ymm2, ymm2, ymm4;

               vmovapd ymm1, [edx + eax + 96];

               // multiply 2x2 and add
               vmulpd ymm3, ymm1, [ebx + eax + 96];
               vmulpd ymm4, ymm1, [edi + eax + 96];

               vaddpd ymm0, ymm0, ymm3;
               vaddpd ymm2, ymm2, ymm4;

               // end for idx := 0 to width1 div 2
           add eax, 128;
           jnz @@InnerLoop;

           vextractf128 xmm1, ymm0, 1;
           vextractf128 xmm3, ymm2, 1;

           vhaddpd xmm0, xmm0, xmm1;
           vhaddpd xmm2, xmm2, xmm3;

           vhaddpd xmm0, xmm0, xmm2;

           // store back result
           vmovapd [ecx], xmm0;

           // increment the pointers
           add ecx, 16;
           add edi, LineWidth2x2;
           add ebx, LineWidth2x2;
       // end for x := 0 to width2 - 1
       sub esi, 2;
       jnz @@fory2label;

       // dec(mt2, Width2);
       // inc(PByte(mt1), LineWidth1);
       // inc(PByte(dest), destOffset);
       mov eax, LineWidth1;
       add mt1, eax;
       mov eax, destLineWidth;
       add dest, eax;

   // end for y := 0 to height1 - 1
   dec Height1;
   jnz @@forylabel;

   // epilog - cleanup stack
   vzeroupper;

   pop esi;
   pop edi;
   pop ebx;
{$IFDEF FPC}
end;
{$ENDIF}
end;

procedure AVXMatrixMultAlignedEvenW1OddH2TransposedMod16(dest : PDouble; const destLineWidth : TASMNativeInt;
    mt1, mt2 : PDouble; width1 : TASMNativeInt;
    height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt;
    const LineWidth1, LineWidth2 : TASMNativeInt);
var iter : TASMNativeInt;
    LineWidth2x2 : TASMNativeInt;
{$IFDEF FPC}
begin
{$ENDIF}
asm
   // prolog - simulate stack
   push ebx;
   push edi;
   push esi;

   mov eax, LineWidth2;
   add eax, eax;
   mov LineWidth2x2, eax;

   dec height2;

   // iters2 := -width1*sizeof(double);
   mov eax, width1;
   imul eax, -8;
   mov iter, eax;

   // prepare matrix pointers - remove constant offset here instead each time in the loop:

   // ecx=dest, edx=mt1, edi = mt1 + linewidth1, ebx=mt2
   sub mt1, eax;
   sub mt2, eax;

   // for y := 0 to height1 - 1:
   @@forylabel:
       mov ecx, dest;
       mov edx, mt1;
       mov ebx, mt2;
       mov edi, ebx;
       add edi, LineWidth2;

       // for y2 := 0 to height2 - 1:
       mov esi, height2;
       @@fory2label:
           vxorpd ymm0, ymm0, ymm0;   // dest^ := 0
           vxorpd ymm2, ymm2, ymm2;   // dest + 1 := 0;
           // for idx := 0 to width1 div 2 do
           mov eax, iter;

           // for x := 0 to width1 - 1
           @@InnerLoop:
               // prefetch [ecx + edx + 128];
               // prefetch [edi + edx + 128];
               // prefetch [eax + edx + 128];

               vmovapd ymm1, [edx + eax];

               // multiply 2x2 and add
               vmulpd ymm3, ymm1, [ebx + eax];
               vmulpd ymm4, ymm1, [edi + eax];

               vaddpd ymm0, ymm0, ymm3;
               vaddpd ymm2, ymm2, ymm4;

               vmovapd ymm1, [edx + eax + 32];

               // multiply 2x2 and add
               vmulpd ymm3, ymm1, [ebx + eax + 32];
               vmulpd ymm4, ymm1, [edi + eax + 32];

               vaddpd ymm0, ymm0, ymm3;
               vaddpd ymm2, ymm2, ymm4;

               vmovapd ymm1, [edx + eax + 64];

               // multiply 2x2 and add
               vmulpd ymm3, ymm1, [ebx + eax + 64];
               vmulpd ymm4, ymm1, [edi + eax + 64];

               vaddpd ymm0, ymm0, ymm3;
               vaddpd ymm2, ymm2, ymm4;

               vmovapd ymm1, [edx + eax + 96];

               // multiply 2x2 and add
               vmulpd ymm3, ymm1, [ebx + eax + 96];
               vmulpd ymm4, ymm1, [edi + eax + 96];

               vaddpd ymm0, ymm0, ymm3;
               vaddpd ymm2, ymm2, ymm4;

               // end for idx := 0 to width1 div 2
           add eax, 128;
           jnz @@InnerLoop;

           vextractf128 xmm1, ymm0, 1;
           vextractf128 xmm3, ymm2, 1;

           vhaddpd xmm0, xmm0, xmm1;
           vhaddpd xmm2, xmm2, xmm3;

           vhaddpd xmm0, xmm0, xmm2;

           // store back result
           vmovapd [ecx], xmm0;

           // increment the pointers
           add ecx, 16;
           add edi, LineWidth2x2;
           add ebx, LineWidth2x2;
       // end for x := 0 to width2 - 1
       sub esi, 2;
       jg @@fory2label;

       // last odd line:
       vxorpd ymm0, ymm0, ymm0;   // dest^ := 0

       // for idx := 0 to width1 div 2 do
       mov eax, iter;

       // for x := 0 to width1 - 1
       @@InnerLoop2:
          // prefetch [ecx + edx + 128];
          // prefetch [edi + edx + 128];
          // prefetch [eax + edx + 128];

          vmovapd ymm1, [edx + eax];

          // multiply 2x2 and add
          vmulpd ymm3, ymm1, [ebx + eax];
          vaddpd ymm0, ymm0, ymm3;

          vmovapd ymm1, [edx + eax + 32];
          vmulpd ymm3, ymm1, [ebx + eax + 32];
          vaddpd ymm0, ymm0, ymm3;

          vmovapd ymm1, [edx + eax + 64];
          vmulpd ymm3, ymm1, [ebx + eax + 64];
          vaddpd ymm0, ymm0, ymm3;

          vmovapd ymm1, [edx + eax + 96];
          vmulpd ymm3, ymm1, [ebx + eax + 96];
          vaddpd ymm0, ymm0, ymm3;

          // end for idx := 0 to width1 div 2
       add eax, 128;
       jnz @@InnerLoop2;

       vextractf128 xmm1, ymm0, 1;
       vhaddpd xmm0, xmm0, xmm1;
       vhaddpd xmm0, xmm0, xmm0;

       // store back result
       vmovsd [ecx], xmm0;

       // dec(mt2, Width2);
       // inc(PByte(mt1), LineWidth1);
       // inc(PByte(dest), destOffset);
       mov eax, LineWidth1;
       add mt1, eax;
       mov eax, destLineWidth;
       add dest, eax;

   // end for y := 0 to height1 - 1
   dec Height1;
   jnz @@forylabel;

   // epilog - cleanup stack
   vzeroupper;

   pop esi;
   pop edi;
   pop ebx;
{$IFDEF FPC}
end;
{$ENDIF}
end;

// note mt2 is transposed this time -> width1 and width2 must be the same!
procedure AVXMatrixMultUnAlignedTransposed(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var iter : TASMNativeInt;
{$IFDEF FPC}
begin
{$ENDIF}
asm
   push ebx;
   push edi;
   push esi;

   // iters2 := -width1*sizeof(double);
   mov eax, width1;
   imul eax, -8;
   mov iter, eax;

   // prepare matrix pointers - remove constant offset here instead each time in the loop:
   sub mt1, eax;
   sub mt2, eax;

   // for y := 0 to height1 - 1:
   @@forylabel:
       mov edx, mt1;
       mov edi, mt2;
       mov esi, edi;
       add esi, LineWidth2;
       mov ecx, dest;

       // for x := 0 to width2 - 1:
       mov ebx, height2;
       sub ebx, 2;
       jl @@fory2loopend;

       @@fory2label:
           vxorpd ymm0, ymm0, ymm0;   // dest^ := 0
           vxorpd ymm2, ymm2, ymm2;   // dest + 1 := 0;
           // for idx := 0 to width1 div 4 do
           mov eax, iter;
           add eax, 32;
           jg @InnerLoopEnd;

           @@InnerLoop:
               vmovupd ymm1, [edx + eax - 32];

               // load 2x2 block
               vmovupd ymm3, [edi + eax - 32];
               vmovupd ymm4, [esi + eax - 32];

               // multiply 2x2 and add
               vmulpd ymm3, ymm3, ymm1;
               vmulpd ymm4, ymm4, ymm1;

               vaddpd ymm0, ymm0, ymm3;
               vaddpd ymm2, ymm2, ymm4;

               // end for idx := 0 to width1 div 2
           add eax, 32;
           jle @@InnerLoop;

           vextractf128 xmm1, ymm0, 1;
           vextractf128 xmm3, ymm2, 1;

           vhaddpd xmm0, xmm0, xmm1;
           vhaddpd xmm2, xmm2, xmm3;

           @InnerLoopEnd:

           sub eax, 32;
           jz @@InnerLoopEnd2;

           // do the final few elements
           @@InnerLoop2:
               vmovsd xmm1, [edx + eax];

               // load 2x2 block
               vmovsd xmm3, [edi + eax];
               vmovsd xmm4, [esi + eax];

               // multiply 2x2 and add
               vmulsd xmm3, xmm3, xmm1;
               vmulsd xmm4, xmm4, xmm1;

               vaddsd xmm0, xmm0, xmm3;
               vaddsd xmm2, xmm2, xmm4;

           add eax, 8;
           jnz @@InnerLoop2;

           @@InnerLoopEnd2:

           // final add and compact result
           vhaddpd xmm0, xmm0, xmm2;

           // store back result
           vmovupd [ecx], xmm0;

           // increment the pointers
           // inc(mt2), inc(dest);
           //add dword ptr [mt2], 8;
           add ecx, 16;
           mov edi, esi;
           add edi, LineWidth2;
           mov esi, edi;
           add esi, LineWidth2;
       // end for x := 0 to width2 - 1
       sub ebx, 2;
       jge @@fory2label;

       //
       @@fory2loopend:
       add ebx, 2;

       // test for odd h2
       jz @@NextLine;

       // we have an odd height2 -> special treatment for the last line
       vxorpd ymm0, ymm0, ymm0;   // dest^ := 0

       // for idx := 0 to width1 div 4 do
       mov eax, iter;
       add eax, 32;
       jg @LastLineInnerLoopEnd;

       @@LastLineInnerLoop:
            vmovupd ymm1, [edx + eax - 32];

            // load block
            vmovupd ymm3, [edi + eax - 32];

            // multiply and add
            vmulpd ymm3, ymm3, ymm1;

            vaddpd ymm0, ymm0, ymm3;

            // end for idx := 0 to width1 div 2
       add eax, 32;
       jle @@LastLineInnerLoop;

       vextractf128 xmm1, ymm0, 1;
       vhaddpd xmm0, xmm0, xmm1;

       @LastLineInnerLoopEnd:
       sub eax, 32;
       jz @@LastLineInnerLoopEnd2;

       // do the final few elements
       @@LastLineInnerLoop2:
            vmovsd xmm1, [edx + eax];
            vmovsd xmm3, [edi + eax];

            // multiply and add
            vmulsd xmm3, xmm3, xmm1;
            vaddsd xmm0, xmm0, xmm3;
       // next element
       add eax, 8;
       jnz @@LastLineInnerLoop2;

       @@LastLineInnerLoopEnd2:

       // final add and compact result
       vhaddpd xmm0, xmm0, xmm0;

       // store back result
       vmovsd [ecx], xmm0;

       // ################################################
       // #### next line of mt1
       @@NextLine:
       // dec(mt2, Width2);
       // inc(PByte(mt1), LineWidth1);
       // inc(PByte(dest), destOffset);
       mov eax, LineWidth1;
       add mt1, eax;
       mov eax, destLineWidth;
       add dest, eax;

   // end for y := 0 to height1 - 1
   dec Height1;
   jnz @@forylabel;

   // epilog - cleanup stack
   vzeroupper;

   pop esi;
   pop edi;
   pop ebx;
{$IFDEF FPC}
end;
{$ENDIF}
end;

// note mt2 is transposed this time -> width1 and width2 must be the same!
procedure AVXMatrixMultAlignedTransposed(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var iter : TASMNativeInt;
{$IFDEF FPC}
begin
{$ENDIF}
asm
   push ebx;
   push edi;
   push esi;

   // iters2 := -width1*sizeof(double);
   mov eax, width1;
   imul eax, -8;
   mov iter, eax;

   // prepare matrix pointers - remove constant offset here instead each time in the loop:
   sub mt1, eax;
   sub mt2, eax;

   // for y := 0 to height1 - 1:
   @@forylabel:
       mov edx, mt1;
       mov edi, mt2;
       mov esi, edi;
       add esi, LineWidth2;
       mov ecx, dest;

       // for x := 0 to width2 - 1:
       mov ebx, height2;
       sub ebx, 2;
       jl @@fory2loopend;

       @@fory2label:
           vxorpd ymm0, ymm0, ymm0;   // dest^ := 0
           vxorpd ymm2, ymm2, ymm2;   // dest + 1 := 0;
           // for idx := 0 to width1 div 4 do
           mov eax, iter;
           add eax, 32;
           jg @InnerLoopEnd;

           @@InnerLoop:
               vmovapd ymm1, [edx + eax - 32];

               // multiply 2x2 and add
               vmulpd ymm3, ymm1, [edi + eax - 32];
               vmulpd ymm4, ymm1, [esi + eax - 32];

               vaddpd ymm0, ymm0, ymm3;
               vaddpd ymm2, ymm2, ymm4;

               // end for idx := 0 to width1 div 2
           add eax, 32;
           jle @@InnerLoop;

           vextractf128 xmm1, ymm0, 1;
           vextractf128 xmm3, ymm2, 1;

           vhaddpd xmm0, xmm0, xmm1;
           vhaddpd xmm2, xmm2, xmm3;

           @InnerLoopEnd:

           sub eax, 32;
           jz @@InnerLoopEnd2;

           // do the final few elements
           @@InnerLoop2:
               vmovsd xmm1, [edx + eax];

               // load 2x2 block
               vmovsd xmm3, [edi + eax];
               vmovsd xmm4, [esi + eax];

               // multiply 2x2 and add
               vmulsd xmm3, xmm3, xmm1;
               vmulsd xmm4, xmm4, xmm1;

               vaddsd xmm0, xmm0, xmm3;
               vaddsd xmm2, xmm2, xmm4;

           add eax, 8;
           jnz @@InnerLoop2;

           @@InnerLoopEnd2:

           // final add and compact result
           vhaddpd xmm0, xmm0, xmm2;

           // store back result
           vmovapd [ecx], xmm0;

           // increment the pointers
           // inc(mt2), inc(dest);
           //add dword ptr [mt2], 8;
           add ecx, 16;
           mov edi, esi;
           add edi, LineWidth2;
           mov esi, edi;
           add esi, LineWidth2;
       // end for x := 0 to width2 - 1
       sub ebx, 2;
       jge @@fory2label;

       //
       @@fory2loopend:
       add ebx, 2;

       // test for odd h2
       jz @@NextLine;

       // we have an odd height2 -> special treatment for the last line
       vxorpd ymm0, ymm0, ymm0;   // dest^ := 0

       // for idx := 0 to width1 div 4 do
       mov eax, iter;
       add eax, 32;
       jg @LastLineInnerLoopEnd;

       @@LastLineInnerLoop:
            vmovapd ymm1, [edx + eax - 32];

            // load block
            vmovapd ymm3, [edi + eax - 32];

            // multiply and add
            vmulpd ymm3, ymm3, ymm1;
            vaddpd ymm0, ymm0, ymm3;
            // end for idx := 0 to width1 div 2
       add eax, 32;
       jle @@LastLineInnerLoop;

       vextractf128 xmm1, ymm0, 1;
       vhaddpd xmm0, xmm0, xmm1;

       @LastLineInnerLoopEnd:
       sub eax, 32;
       jz @@LastLineInnerLoopEnd2;

       // do the final few elements
       @@LastLineInnerLoop2:
            vmovsd xmm1, [edx + eax];
            vmovsd xmm3, [edi + eax];

            // multiply and add
            vmulsd xmm3, xmm3, xmm1;
            vaddsd xmm0, xmm0, xmm3;
       // next element
       add eax, 8;
       jnz @@LastLineInnerLoop2;

       @@LastLineInnerLoopEnd2:

       // final add and compact result
       vhaddpd xmm0, xmm0, xmm0;

       // store back result
       vmovsd [ecx], xmm0;

       // ################################################
       // #### next line of mt1
       @@NextLine:
       // dec(mt2, Width2);
       // inc(PByte(mt1), LineWidth1);
       // inc(PByte(dest), destOffset);
       //mov ebx, bytesWidth2;
       //sub dword ptr [mt2], ebx;
       mov eax, LineWidth1;
       add mt1, eax;
       mov eax, destLineWidth;
       add dest, eax;

   // end for y := 0 to height1 - 1
   dec Height1;
   jnz @@forylabel;

   // epilog - cleanup stack
   vzeroupper;

   pop esi;
   pop edi;
   pop ebx;
{$IFDEF FPC}
end;
{$ENDIF}
end;

{$ENDIF}

end.
