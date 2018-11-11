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

procedure AVXMatrixMultAlignedTransposed(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure AVXMatrixMultAlignedEvenW1EvenH2TransposedMod16(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure AVXMatrixMultAlignedEvenW1OddH2TransposedMod16(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure AVXMatrixMultUnAlignedTransposed(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

{$ENDIF}

implementation

{$IFNDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$S-} {$ENDIF}

procedure AVXMatrixMultAlignedEvenW1EvenH2TransposedMod16(dest : PDouble; const destLineWidth : TASMNativeInt;
    mt1, mt2 : PDouble; width1 : TASMNativeInt;
    height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt;
    const LineWidth1, LineWidth2 : TASMNativeInt);
var iter : TASMNativeInt;
    LineWidth2x2 : TASMNativeInt;
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
   mov iter, ebx;

   // LineWidth2x2 := 2*LineWidth2;
   mov esi, LineWidth2;
   shl esi, 1;
   mov LineWidth2x2, esi;

   // prepare matrix pointers - remove constant offset here instead each time in the loop:
   mov edi, mt2;
   sub ecx, ebx;
   sub edi, ebx;
   mov mt2, edi;

   // for y := 0 to height1 - 1:
   @@forylabel:
       mov edi, mt2;

       // for y2 := 0 to height2 - 1:
       mov ebx, height2;
       @@fory2label:
           // esi points to next row of mt2
           mov esi, edi;
           add esi, LineWidth2;

           {$IFDEF FPC}vxorpd ymm0, ymm0, ymm0;   {$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} // dest^ := 0
           {$IFDEF FPC}vxorpd ymm2, ymm2, ymm2;   {$ELSE}db $C5,$ED,$57,$D2;{$ENDIF} // dest + 1 := 0;
           // for idx := 0 to width1 div 2 do
           mov edx, iter;

           // for x := 0 to width1 - 1
           @@InnerLoop:
               // prefetch [rcx + edx + 128];
               // prefetch [edi + edx + 128];
               // prefetch [rax + edx + 128];

               {$IFDEF FPC}vmovapd ymm1, [ecx + edx];{$ELSE}db $C5,$FD,$28,$0C,$11;{$ENDIF} 

               // multiply 2x2 and add
               {$IFDEF FPC}vmulpd ymm3, ymm1, [edi + edx];{$ELSE}db $C5,$F5,$59,$1C,$17;{$ENDIF} 
               {$IFDEF FPC}vmulpd ymm4, ymm1, [esi + edx];{$ELSE}db $C5,$F5,$59,$24,$16;{$ENDIF} 

               {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
               {$IFDEF FPC}vaddpd ymm2, ymm2, ymm4;{$ELSE}db $C5,$ED,$58,$D4;{$ENDIF} 

               {$IFDEF FPC}vmovapd ymm1, [ecx + edx + 32];{$ELSE}db $C5,$FD,$28,$4C,$11,$20;{$ENDIF} 

               // multiply 2x2 and add
               {$IFDEF FPC}vmulpd ymm3, ymm1, [edi + edx + 32];{$ELSE}db $C5,$F5,$59,$5C,$17,$20;{$ENDIF} 
               {$IFDEF FPC}vmulpd ymm4, ymm1, [esi + edx + 32];{$ELSE}db $C5,$F5,$59,$64,$16,$20;{$ENDIF} 

               {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
               {$IFDEF FPC}vaddpd ymm2, ymm2, ymm4;{$ELSE}db $C5,$ED,$58,$D4;{$ENDIF} 

               {$IFDEF FPC}vmovapd ymm1, [ecx + edx + 64];{$ELSE}db $C5,$FD,$28,$4C,$11,$40;{$ENDIF} 

               // multiply 2x2 and add
               {$IFDEF FPC}vmulpd ymm3, ymm1, [edi + edx + 64];{$ELSE}db $C5,$F5,$59,$5C,$17,$40;{$ENDIF} 
               {$IFDEF FPC}vmulpd ymm4, ymm1, [esi + edx + 64];{$ELSE}db $C5,$F5,$59,$64,$16,$40;{$ENDIF} 

               {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
               {$IFDEF FPC}vaddpd ymm2, ymm2, ymm4;{$ELSE}db $C5,$ED,$58,$D4;{$ENDIF} 

               {$IFDEF FPC}vmovapd ymm1, [ecx + edx + 96];{$ELSE}db $C5,$FD,$28,$4C,$11,$60;{$ENDIF} 

               // multiply 2x2 and add
               {$IFDEF FPC}vmulpd ymm3, ymm1, [edi + edx + 96];{$ELSE}db $C5,$F5,$59,$5C,$17,$60;{$ENDIF} 
               {$IFDEF FPC}vmulpd ymm4, ymm1, [esi + edx + 96];{$ELSE}db $C5,$F5,$59,$64,$16,$60;{$ENDIF} 

               {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
               {$IFDEF FPC}vaddpd ymm2, ymm2, ymm4;{$ELSE}db $C5,$ED,$58,$D4;{$ENDIF} 

               // end for idx := 0 to width1 div 2
           add edx, 128;
           jnz @@InnerLoop;

           {$IFDEF FPC}vextractf128 xmm1, ymm0, 1;{$ELSE}db $C4,$E3,$7D,$19,$C1,$01;{$ENDIF} 
           {$IFDEF FPC}vextractf128 xmm3, ymm2, 1;{$ELSE}db $C4,$E3,$7D,$19,$D3,$01;{$ENDIF} 

           {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm1;{$ELSE}db $C5,$F9,$7C,$C1;{$ENDIF} 
           {$IFDEF FPC}vhaddpd xmm2, xmm2, xmm3;{$ELSE}db $C5,$E9,$7C,$D3;{$ENDIF} 

           {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm2;{$ELSE}db $C5,$F9,$7C,$C2;{$ENDIF} 

           // store back result
           {$IFDEF FPC}vmovapd [eax], xmm0;{$ELSE}db $C5,$F9,$29,$00;{$ENDIF} 

           // increment the pointers
           add eax, 16;
           add edi, LineWidth2x2;
       // end for x := 0 to width2 - 1
       dec ebx;
       jnz @@fory2label;

       // update pointers
       add ecx, LineWidth1;
       add eax, destOffset;

   // end for y := 0 to height1 - 1
   dec Height1;
   jnz @@forylabel;

   // epilog - cleanup stack
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop edi;
   pop esi;
   pop ebx;
end;

procedure AVXMatrixMultAlignedEvenW1OddH2TransposedMod16(dest : PDouble; const destLineWidth : TASMNativeInt;
    mt1, mt2 : PDouble; width1 : TASMNativeInt;
    height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt;
    const LineWidth1, LineWidth2 : TASMNativeInt);
var iter : TASMNativeInt;
    LineWidth2x2 : TASMNativeInt;
    destOffset : TASMNativeInt;
// eax = dest, edx = destlinewidth, ecx = mt1
asm
   push ebx;
   push esi;
   push edi;

   // init
   dec Height2;    // last line is different

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
   mov iter, ebx;

   // LineWidth2x2 := 2*LineWidth2;
   mov esi, LineWidth2;
   shl esi, 1;
   mov LineWidth2x2, esi;

   // prepare matrix pointers - remove constant offset here instead each time in the loop:
   mov edi, mt2;
   sub ecx, ebx;
   sub edi, ebx;
   mov mt2, edi;

   // for y := 0 to height1 - 1:
   @@forylabel:
       mov edi, mt2;

       // for y2 := 0 to height2 - 1:
       mov ebx, height2;
       @@fory2label:
           // esi points to next row of mt2
           mov esi, edi;
           add esi, LineWidth2;

           {$IFDEF FPC}vxorpd ymm0, ymm0, ymm0;   {$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} // dest^ := 0
           {$IFDEF FPC}vxorpd ymm2, ymm2, ymm2;   {$ELSE}db $C5,$ED,$57,$D2;{$ENDIF} // dest + 1 := 0;
           // for idx := 0 to width1 div 2 do
           mov edx, iter;

           // for x := 0 to width1 - 1
           @@InnerLoop:
               // prefetch [rcx + edx + 128];
               // prefetch [edi + edx + 128];
               // prefetch [rax + edx + 128];

               {$IFDEF FPC}vmovapd ymm1, [ecx + edx];{$ELSE}db $C5,$FD,$28,$0C,$11;{$ENDIF} 

               // multiply 2x2 and add
               {$IFDEF FPC}vmulpd ymm3, ymm1, [edi + edx];{$ELSE}db $C5,$F5,$59,$1C,$17;{$ENDIF} 
               {$IFDEF FPC}vmulpd ymm4, ymm1, [esi + edx];{$ELSE}db $C5,$F5,$59,$24,$16;{$ENDIF} 

               {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
               {$IFDEF FPC}vaddpd ymm2, ymm2, ymm4;{$ELSE}db $C5,$ED,$58,$D4;{$ENDIF} 

               {$IFDEF FPC}vmovapd ymm1, [ecx + edx + 32];{$ELSE}db $C5,$FD,$28,$4C,$11,$20;{$ENDIF} 

               // multiply 2x2 and add
               {$IFDEF FPC}vmulpd ymm3, ymm1, [edi + edx + 32];{$ELSE}db $C5,$F5,$59,$5C,$17,$20;{$ENDIF} 
               {$IFDEF FPC}vmulpd ymm4, ymm1, [esi + edx + 32];{$ELSE}db $C5,$F5,$59,$64,$16,$20;{$ENDIF} 

               {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
               {$IFDEF FPC}vaddpd ymm2, ymm2, ymm4;{$ELSE}db $C5,$ED,$58,$D4;{$ENDIF} 

               {$IFDEF FPC}vmovapd ymm1, [ecx + edx + 64];{$ELSE}db $C5,$FD,$28,$4C,$11,$40;{$ENDIF} 

               // multiply 2x2 and add
               {$IFDEF FPC}vmulpd ymm3, ymm1, [edi + edx + 64];{$ELSE}db $C5,$F5,$59,$5C,$17,$40;{$ENDIF} 
               {$IFDEF FPC}vmulpd ymm4, ymm1, [esi + edx + 64];{$ELSE}db $C5,$F5,$59,$64,$16,$40;{$ENDIF} 

               {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
               {$IFDEF FPC}vaddpd ymm2, ymm2, ymm4;{$ELSE}db $C5,$ED,$58,$D4;{$ENDIF} 

               {$IFDEF FPC}vmovapd ymm1, [ecx + edx + 96];{$ELSE}db $C5,$FD,$28,$4C,$11,$60;{$ENDIF} 

               // multiply 2x2 and add
               {$IFDEF FPC}vmulpd ymm3, ymm1, [edi + edx + 96];{$ELSE}db $C5,$F5,$59,$5C,$17,$60;{$ENDIF} 
               {$IFDEF FPC}vmulpd ymm4, ymm1, [esi + edx + 96];{$ELSE}db $C5,$F5,$59,$64,$16,$60;{$ENDIF} 

               {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
               {$IFDEF FPC}vaddpd ymm2, ymm2, ymm4;{$ELSE}db $C5,$ED,$58,$D4;{$ENDIF} 

               // end for idx := 0 to width1 div 2
           add edx, 128;
           jnz @@InnerLoop;

           {$IFDEF FPC}vextractf128 xmm1, ymm0, 1;{$ELSE}db $C4,$E3,$7D,$19,$C1,$01;{$ENDIF} 
           {$IFDEF FPC}vextractf128 xmm3, ymm2, 1;{$ELSE}db $C4,$E3,$7D,$19,$D3,$01;{$ENDIF} 

           {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm1;{$ELSE}db $C5,$F9,$7C,$C1;{$ENDIF} 
           {$IFDEF FPC}vhaddpd xmm2, xmm2, xmm3;{$ELSE}db $C5,$E9,$7C,$D3;{$ENDIF} 

           {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm2;{$ELSE}db $C5,$F9,$7C,$C2;{$ENDIF} 

           // store back result
           {$IFDEF FPC}vmovapd [eax], xmm0;{$ELSE}db $C5,$F9,$29,$00;{$ENDIF} 

           // increment the pointers
           add eax, 16;
           add edi, LineWidth2x2;
       // end for x := 0 to width2 - 1
       dec ebx;
       jnz @@fory2label;

       // last odd line:
       {$IFDEF FPC}vxorpd ymm0, ymm0, ymm0;   {$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} // dest^ := 0

       // for idx := 0 to width1 div 2 do
       mov edx, iter;

       // for x := 0 to width1 - 1
       @@InnerLoop2:
          // prefetch [ecx + edx + 128];
          // prefetch [edi + edx + 128];
          // prefetch [eax + edx + 128];

          {$IFDEF FPC}vmovapd ymm1, [ecx + edx];{$ELSE}db $C5,$FD,$28,$0C,$11;{$ENDIF} 

          // multiply 2x2 and add
          {$IFDEF FPC}vmulpd ymm3, ymm1, [edi + edx];{$ELSE}db $C5,$F5,$59,$1C,$17;{$ENDIF} 
          {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 

          {$IFDEF FPC}vmovapd ymm1, [ecx + edx + 32];{$ELSE}db $C5,$FD,$28,$4C,$11,$20;{$ENDIF} 
          {$IFDEF FPC}vmulpd ymm3, ymm1, [edi + edx + 32];{$ELSE}db $C5,$F5,$59,$5C,$17,$20;{$ENDIF} 
          {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 

          {$IFDEF FPC}vmovapd ymm1, [ecx + edx + 64];{$ELSE}db $C5,$FD,$28,$4C,$11,$40;{$ENDIF} 
          {$IFDEF FPC}vmulpd ymm3, ymm1, [edi + edx + 64];{$ELSE}db $C5,$F5,$59,$5C,$17,$40;{$ENDIF} 
          {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 

          {$IFDEF FPC}vmovapd ymm1, [ecx + edx + 96];{$ELSE}db $C5,$FD,$28,$4C,$11,$60;{$ENDIF} 
          {$IFDEF FPC}vmulpd ymm3, ymm1, [edi + edx + 96];{$ELSE}db $C5,$F5,$59,$5C,$17,$60;{$ENDIF} 
          {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 

          // end for idx := 0 to width1 div 2
       add edx, 128;
       jnz @@InnerLoop2;

       {$IFDEF FPC}vextractf128 xmm1, ymm0, 1;{$ELSE}db $C4,$E3,$7D,$19,$C1,$01;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm1;{$ELSE}db $C5,$F9,$7C,$C1;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} 

       // store back result
       {$IFDEF FPC}vmovsd [eax], xmm0;{$ELSE}db $C5,$FB,$11,$00;{$ENDIF} 

       // update pointers
       add ecx, LineWidth1;
       add eax, destOffset;

   // end for y := 0 to height1 - 1
   dec Height1;
   jnz @@forylabel;

   // epilog - cleanup stack
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop edi;
   pop esi;
   pop ebx;
end;

// note mt2 is transposed this time -> width1 and width2 must be the same!
procedure AVXMatrixMultUnAlignedTransposed(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var iter : TASMNativeInt;
    destOffset : TASMNativeInt;
asm
   push ebx;
   push edi;
   push esi;

   // iters2 := -width1*sizeof(double);
   mov esi, width1;
   imul esi, -8;
   mov iter, esi;

   mov edi, height2;
   shl edi, 3;
   sub edx, edi;
   mov destOffset, edx;

   // prepare matrix pointers - remove constant offset here instead each time in the loop:
   sub ecx, esi;
   sub mt2, esi;

   // for y := 0 to height1 - 1:
   @@forylabel:
       mov edi, mt2;

       mov esi, edi;
       add esi, LineWidth2;

       // for x := 0 to width2 - 1:
       mov ebx, height2;
       sub ebx, 2;
       jl @@fory2loopend;

       @@fory2label:
           {$IFDEF FPC}vxorpd ymm0, ymm0, ymm0;   {$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} // dest^ := 0
           {$IFDEF FPC}vxorpd ymm2, ymm2, ymm2;   {$ELSE}db $C5,$ED,$57,$D2;{$ENDIF} // dest + 1 := 0;
           // for idx := 0 to width1 div 4 do
           mov edx, iter;
           add edx, 32;
           jg @InnerLoopEnd;

           @@InnerLoop:
               {$IFDEF FPC}vmovupd ymm1, [ecx + edx - 32];{$ELSE}db $C5,$FD,$10,$4C,$11,$E0;{$ENDIF} 

               // multiply 2x2 and add
               {$IFDEF FPC}vmulpd ymm3, ymm1, [edi + edx - 32];{$ELSE}db $C5,$F5,$59,$5C,$17,$E0;{$ENDIF} 
               {$IFDEF FPC}vmulpd ymm4, ymm1, [esi + edx - 32];{$ELSE}db $C5,$F5,$59,$64,$16,$E0;{$ENDIF} 

               {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
               {$IFDEF FPC}vaddpd ymm2, ymm2, ymm4;{$ELSE}db $C5,$ED,$58,$D4;{$ENDIF} 

               // end for idx := 0 to width1 div 2
           add edx, 32;
           jle @@InnerLoop;

           {$IFDEF FPC}vextractf128 xmm1, ymm0, 1;{$ELSE}db $C4,$E3,$7D,$19,$C1,$01;{$ENDIF} 
           {$IFDEF FPC}vextractf128 xmm3, ymm2, 1;{$ELSE}db $C4,$E3,$7D,$19,$D3,$01;{$ENDIF} 

           {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm1;{$ELSE}db $C5,$F9,$7C,$C1;{$ENDIF} 
           {$IFDEF FPC}vhaddpd xmm2, xmm2, xmm3;{$ELSE}db $C5,$E9,$7C,$D3;{$ENDIF} 

           @InnerLoopEnd:

           sub edx, 32;
           jz @@InnerLoopEnd2;

           // do the final few elements
           @@InnerLoop2:
               {$IFDEF FPC}vmovsd xmm1, [ecx + edx];{$ELSE}db $C5,$FB,$10,$0C,$11;{$ENDIF} 

               // load 2x2 block
               {$IFDEF FPC}vmovsd xmm3, [edi + edx];{$ELSE}db $C5,$FB,$10,$1C,$17;{$ENDIF} 
               {$IFDEF FPC}vmovsd xmm4, [esi + edx];{$ELSE}db $C5,$FB,$10,$24,$16;{$ENDIF} 

               // multiply 2x2 and add
               {$IFDEF FPC}vmulsd xmm3, xmm3, xmm1;{$ELSE}db $C5,$E3,$59,$D9;{$ENDIF} 
               {$IFDEF FPC}vmulsd xmm4, xmm4, xmm1;{$ELSE}db $C5,$DB,$59,$E1;{$ENDIF} 

               {$IFDEF FPC}vaddsd xmm0, xmm0, xmm3;{$ELSE}db $C5,$FB,$58,$C3;{$ENDIF} 
               {$IFDEF FPC}vaddsd xmm2, xmm2, xmm4;{$ELSE}db $C5,$EB,$58,$D4;{$ENDIF} 

           add edx, 8;
           jnz @@InnerLoop2;

           @@InnerLoopEnd2:

           // final add and compact result
           {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm2;{$ELSE}db $C5,$F9,$7C,$C2;{$ENDIF} 

           // store back result
           {$IFDEF FPC}vmovupd [eax], xmm0;{$ELSE}db $C5,$F9,$11,$00;{$ENDIF} 

           // increment the pointers
           // inc(mt2), inc(dest);
           //add dword ptr [mt2], 8;
           add eax, 16;
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
       {$IFDEF FPC}vxorpd ymm0, ymm0, ymm0;   {$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} // dest^ := 0

       // for idx := 0 to width1 div 4 do
       mov edx, iter;
       add edx, 32;
       jg @LastLineInnerLoopEnd;

       @@LastLineInnerLoop:
            {$IFDEF FPC}vmovupd ymm1, [ecx + edx - 32];{$ELSE}db $C5,$FD,$10,$4C,$11,$E0;{$ENDIF} 

            // load block
            {$IFDEF FPC}vmovupd ymm3, [edi + edx - 32];{$ELSE}db $C5,$FD,$10,$5C,$17,$E0;{$ENDIF} 

            // multiply and add
            {$IFDEF FPC}vmulpd ymm3, ymm3, ymm1;{$ELSE}db $C5,$E5,$59,$D9;{$ENDIF} 
            {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
            // end for idx := 0 to width1 div 2
       add edx, 32;
       jle @@LastLineInnerLoop;

       {$IFDEF FPC}vextractf128 xmm1, ymm0, 1;{$ELSE}db $C4,$E3,$7D,$19,$C1,$01;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm1;{$ELSE}db $C5,$F9,$7C,$C1;{$ENDIF} 

       @LastLineInnerLoopEnd:
       sub edx, 32;
       jz @@LastLineInnerLoopEnd2;

       // do the final few elements
       @@LastLineInnerLoop2:
            {$IFDEF FPC}vmovsd xmm1, [ecx + edx];{$ELSE}db $C5,$FB,$10,$0C,$11;{$ENDIF} 
            {$IFDEF FPC}vmovsd xmm3, [edi + edx];{$ELSE}db $C5,$FB,$10,$1C,$17;{$ENDIF} 

            // multiply and add
            {$IFDEF FPC}vmulsd xmm3, xmm3, xmm1;{$ELSE}db $C5,$E3,$59,$D9;{$ENDIF} 
            {$IFDEF FPC}vaddsd xmm0, xmm0, xmm3;{$ELSE}db $C5,$FB,$58,$C3;{$ENDIF} 
       // next element
       add edx, 8;
       jnz @@LastLineInnerLoop2;

       @@LastLineInnerLoopEnd2:

       // final add and compact result
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} 

       // store back result
       {$IFDEF FPC}vmovsd [eax], xmm0;{$ELSE}db $C5,$FB,$11,$00;{$ENDIF} 
       add eax, 8;

       // ################################################
       // #### next line of mt1
       @@NextLine:
       // dec(mt2, Width2);
       // inc(PByte(mt1), LineWidth1);
       // inc(PByte(dest), destOffset);
       //mov ebx, bytesWidth2;
       //sub dword ptr [mt2], ebx;
       add ecx, LineWidth1;
       add eax, destOffset;

   // end for y := 0 to height1 - 1
   dec Height1;
   jnz @@forylabel;

   // epilog - cleanup stack
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;

// note mt2 is transposed this time -> width1 and width2 must be the same!
procedure AVXMatrixMultAlignedTransposed(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var iter : TASMNativeInt;
    destOffset : TASMNativeInt;
asm
   push ebx;
   push edi;
   push esi;

   // iters2 := -width1*sizeof(double);
   mov esi, width1;
   imul esi, -8;
   mov iter, esi;

   mov edi, height2;
   shl edi, 3;
   sub edx, edi;
   mov destOffset, edx;

   // prepare matrix pointers - remove constant offset here instead each time in the loop:
   sub ecx, esi;
   sub mt2, esi;

   // for y := 0 to height1 - 1:
   @@forylabel:
       mov edi, mt2;

       mov esi, edi;
       add esi, LineWidth2;

       // for x := 0 to width2 - 1:
       mov ebx, height2;
       sub ebx, 2;
       jl @@fory2loopend;

       @@fory2label:
           {$IFDEF FPC}vxorpd ymm0, ymm0, ymm0;   {$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} // dest^ := 0
           {$IFDEF FPC}vxorpd ymm2, ymm2, ymm2;   {$ELSE}db $C5,$ED,$57,$D2;{$ENDIF} // dest + 1 := 0;
           // for idx := 0 to width1 div 4 do
           mov edx, iter;
           add edx, 32;
           jg @InnerLoopEnd;

           @@InnerLoop:
               {$IFDEF FPC}vmovapd ymm1, [ecx + edx - 32];{$ELSE}db $C5,$FD,$28,$4C,$11,$E0;{$ENDIF} 

               // multiply 2x2 and add
               {$IFDEF FPC}vmulpd ymm3, ymm1, [edi + edx - 32];{$ELSE}db $C5,$F5,$59,$5C,$17,$E0;{$ENDIF} 
               {$IFDEF FPC}vmulpd ymm4, ymm1, [esi + edx - 32];{$ELSE}db $C5,$F5,$59,$64,$16,$E0;{$ENDIF} 

               {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
               {$IFDEF FPC}vaddpd ymm2, ymm2, ymm4;{$ELSE}db $C5,$ED,$58,$D4;{$ENDIF} 

               // end for idx := 0 to width1 div 2
           add edx, 32;
           jle @@InnerLoop;

           {$IFDEF FPC}vextractf128 xmm1, ymm0, 1;{$ELSE}db $C4,$E3,$7D,$19,$C1,$01;{$ENDIF} 
           {$IFDEF FPC}vextractf128 xmm3, ymm2, 1;{$ELSE}db $C4,$E3,$7D,$19,$D3,$01;{$ENDIF} 

           {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm1;{$ELSE}db $C5,$F9,$7C,$C1;{$ENDIF} 
           {$IFDEF FPC}vhaddpd xmm2, xmm2, xmm3;{$ELSE}db $C5,$E9,$7C,$D3;{$ENDIF} 

           @InnerLoopEnd:

           sub edx, 32;
           jz @@InnerLoopEnd2;

           // do the final few elements
           @@InnerLoop2:
               {$IFDEF FPC}vmovsd xmm1, [ecx + edx];{$ELSE}db $C5,$FB,$10,$0C,$11;{$ENDIF} 

               // load 2x2 block
               {$IFDEF FPC}vmovsd xmm3, [edi + edx];{$ELSE}db $C5,$FB,$10,$1C,$17;{$ENDIF} 
               {$IFDEF FPC}vmovsd xmm4, [esi + edx];{$ELSE}db $C5,$FB,$10,$24,$16;{$ENDIF} 

               // multiply 2x2 and add
               {$IFDEF FPC}vmulsd xmm3, xmm3, xmm1;{$ELSE}db $C5,$E3,$59,$D9;{$ENDIF} 
               {$IFDEF FPC}vmulsd xmm4, xmm4, xmm1;{$ELSE}db $C5,$DB,$59,$E1;{$ENDIF} 

               {$IFDEF FPC}vaddsd xmm0, xmm0, xmm3;{$ELSE}db $C5,$FB,$58,$C3;{$ENDIF} 
               {$IFDEF FPC}vaddsd xmm2, xmm2, xmm4;{$ELSE}db $C5,$EB,$58,$D4;{$ENDIF} 

           add edx, 8;
           jnz @@InnerLoop2;

           @@InnerLoopEnd2:

           // final add and compact result
           {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm2;{$ELSE}db $C5,$F9,$7C,$C2;{$ENDIF} 

           // store back result
           {$IFDEF FPC}vmovapd [eax], xmm0;{$ELSE}db $C5,$F9,$29,$00;{$ENDIF} 

           // increment the pointers
           // inc(mt2), inc(dest);
           //add dword ptr [mt2], 8;
           add eax, 16;
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
       {$IFDEF FPC}vxorpd ymm0, ymm0, ymm0;   {$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} // dest^ := 0

       // for idx := 0 to width1 div 4 do
       mov edx, iter;
       add edx, 32;
       jg @LastLineInnerLoopEnd;

       @@LastLineInnerLoop:
            {$IFDEF FPC}vmovapd ymm1, [ecx + edx - 32];{$ELSE}db $C5,$FD,$28,$4C,$11,$E0;{$ENDIF} 

            // load block
            {$IFDEF FPC}vmovapd ymm3, [edi + edx - 32];{$ELSE}db $C5,$FD,$28,$5C,$17,$E0;{$ENDIF} 

            // multiply and add
            {$IFDEF FPC}vmulpd ymm3, ymm3, ymm1;{$ELSE}db $C5,$E5,$59,$D9;{$ENDIF} 
            {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
            // end for idx := 0 to width1 div 2
       add edx, 32;
       jle @@LastLineInnerLoop;

       {$IFDEF FPC}vextractf128 xmm1, ymm0, 1;{$ELSE}db $C4,$E3,$7D,$19,$C1,$01;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm1;{$ELSE}db $C5,$F9,$7C,$C1;{$ENDIF} 

       @LastLineInnerLoopEnd:
       sub edx, 32;
       jz @@LastLineInnerLoopEnd2;

       // do the final few elements
       @@LastLineInnerLoop2:
            {$IFDEF FPC}vmovsd xmm1, [ecx + edx];{$ELSE}db $C5,$FB,$10,$0C,$11;{$ENDIF} 
            {$IFDEF FPC}vmovsd xmm3, [edi + edx];{$ELSE}db $C5,$FB,$10,$1C,$17;{$ENDIF} 

            // multiply and add
            {$IFDEF FPC}vmulsd xmm3, xmm3, xmm1;{$ELSE}db $C5,$E3,$59,$D9;{$ENDIF} 
            {$IFDEF FPC}vaddsd xmm0, xmm0, xmm3;{$ELSE}db $C5,$FB,$58,$C3;{$ENDIF} 
       // next element
       add edx, 8;
       jnz @@LastLineInnerLoop2;

       @@LastLineInnerLoopEnd2:

       // final add and compact result
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} 

       // store back result
       {$IFDEF FPC}vmovsd [eax], xmm0;{$ELSE}db $C5,$FB,$11,$00;{$ENDIF} 
       add eax, 8;

       // ################################################
       // #### next line of mt1
       @@NextLine:
       // dec(mt2, Width2);
       // inc(PByte(mt1), LineWidth1);
       // inc(PByte(dest), destOffset);
       //mov ebx, bytesWidth2;
       //sub dword ptr [mt2], ebx;
       add ecx, LineWidth1;
       add eax, destOffset;

   // end for y := 0 to height1 - 1
   dec Height1;
   jnz @@forylabel;

   // epilog - cleanup stack
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;

{$ENDIF}

end.
