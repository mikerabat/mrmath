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
begin
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
           {$IFDEF FPC}vxorpd ymm0, ymm0, ymm0;   {$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} // dest^ := 0
           {$IFDEF FPC}vxorpd ymm2, ymm2, ymm2;   {$ELSE}db $C5,$ED,$57,$D2;{$ENDIF} // dest + 1 := 0;
           // for idx := 0 to width1 div 2 do
           mov eax, iter;

           // for x := 0 to width1 - 1
           @@InnerLoop:
               // prefetch [ecx + edx + 128];
               // prefetch [edi + edx + 128];
               // prefetch [eax + edx + 128];

               {$IFDEF FPC}vmovapd ymm1, [edx + eax];{$ELSE}db $C5,$FD,$28,$0C,$02;{$ENDIF} 

               // multiply 2x2 and add
               {$IFDEF FPC}vmulpd ymm3, ymm1, [ebx + eax];{$ELSE}db $C5,$F5,$59,$1C,$03;{$ENDIF} 
               {$IFDEF FPC}vmulpd ymm4, ymm1, [edi + eax];{$ELSE}db $C5,$F5,$59,$24,$07;{$ENDIF} 

               {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
               {$IFDEF FPC}vaddpd ymm2, ymm2, ymm4;{$ELSE}db $C5,$ED,$58,$D4;{$ENDIF} 

               {$IFDEF FPC}vmovapd ymm1, [edx + eax + 32];{$ELSE}db $C5,$FD,$28,$4C,$02,$20;{$ENDIF} 

               // multiply 2x2 and add
               {$IFDEF FPC}vmulpd ymm3, ymm1, [ebx + eax + 32];{$ELSE}db $C5,$F5,$59,$5C,$03,$20;{$ENDIF} 
               {$IFDEF FPC}vmulpd ymm4, ymm1, [edi + eax + 32];{$ELSE}db $C5,$F5,$59,$64,$07,$20;{$ENDIF} 

               {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
               {$IFDEF FPC}vaddpd ymm2, ymm2, ymm4;{$ELSE}db $C5,$ED,$58,$D4;{$ENDIF} 

               {$IFDEF FPC}vmovapd ymm1, [edx + eax + 64];{$ELSE}db $C5,$FD,$28,$4C,$02,$40;{$ENDIF} 

               // multiply 2x2 and add
               {$IFDEF FPC}vmulpd ymm3, ymm1, [ebx + eax + 64];{$ELSE}db $C5,$F5,$59,$5C,$03,$40;{$ENDIF} 
               {$IFDEF FPC}vmulpd ymm4, ymm1, [edi + eax + 64];{$ELSE}db $C5,$F5,$59,$64,$07,$40;{$ENDIF} 

               {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
               {$IFDEF FPC}vaddpd ymm2, ymm2, ymm4;{$ELSE}db $C5,$ED,$58,$D4;{$ENDIF} 

               {$IFDEF FPC}vmovapd ymm1, [edx + eax + 96];{$ELSE}db $C5,$FD,$28,$4C,$02,$60;{$ENDIF} 

               // multiply 2x2 and add
               {$IFDEF FPC}vmulpd ymm3, ymm1, [ebx + eax + 96];{$ELSE}db $C5,$F5,$59,$5C,$03,$60;{$ENDIF} 
               {$IFDEF FPC}vmulpd ymm4, ymm1, [edi + eax + 96];{$ELSE}db $C5,$F5,$59,$64,$07,$60;{$ENDIF} 

               {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
               {$IFDEF FPC}vaddpd ymm2, ymm2, ymm4;{$ELSE}db $C5,$ED,$58,$D4;{$ENDIF} 

               // end for idx := 0 to width1 div 2
           add eax, 128;
           jnz @@InnerLoop;

           {$IFDEF FPC}vextractf128 xmm1, ymm0, 1;{$ELSE}db $C4,$E3,$7D,$19,$C1,$01;{$ENDIF} 
           {$IFDEF FPC}vextractf128 xmm3, ymm2, 1;{$ELSE}db $C4,$E3,$7D,$19,$D3,$01;{$ENDIF} 

           {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm1;{$ELSE}db $C5,$F9,$7C,$C1;{$ENDIF} 
           {$IFDEF FPC}vhaddpd xmm2, xmm2, xmm3;{$ELSE}db $C5,$E9,$7C,$D3;{$ENDIF} 

           {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm2;{$ELSE}db $C5,$F9,$7C,$C2;{$ENDIF} 

           // store back result
           {$IFDEF FPC}vmovapd [ecx], xmm0;{$ELSE}db $C5,$F9,$29,$01;{$ENDIF} 

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
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;
end;

procedure AVXMatrixMultAlignedEvenW1OddH2TransposedMod16(dest : PDouble; const destLineWidth : TASMNativeInt;
    mt1, mt2 : PDouble; width1 : TASMNativeInt;
    height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt;
    const LineWidth1, LineWidth2 : TASMNativeInt);
var iter : TASMNativeInt;
    LineWidth2x2 : TASMNativeInt;
begin
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
           {$IFDEF FPC}vxorpd ymm0, ymm0, ymm0;   {$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} // dest^ := 0
           {$IFDEF FPC}vxorpd ymm2, ymm2, ymm2;   {$ELSE}db $C5,$ED,$57,$D2;{$ENDIF} // dest + 1 := 0;
           // for idx := 0 to width1 div 2 do
           mov eax, iter;

           // for x := 0 to width1 - 1
           @@InnerLoop:
               // prefetch [ecx + edx + 128];
               // prefetch [edi + edx + 128];
               // prefetch [eax + edx + 128];

               {$IFDEF FPC}vmovapd ymm1, [edx + eax];{$ELSE}db $C5,$FD,$28,$0C,$02;{$ENDIF} 

               // multiply 2x2 and add
               {$IFDEF FPC}vmulpd ymm3, ymm1, [ebx + eax];{$ELSE}db $C5,$F5,$59,$1C,$03;{$ENDIF} 
               {$IFDEF FPC}vmulpd ymm4, ymm1, [edi + eax];{$ELSE}db $C5,$F5,$59,$24,$07;{$ENDIF} 

               {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
               {$IFDEF FPC}vaddpd ymm2, ymm2, ymm4;{$ELSE}db $C5,$ED,$58,$D4;{$ENDIF} 

               {$IFDEF FPC}vmovapd ymm1, [edx + eax + 32];{$ELSE}db $C5,$FD,$28,$4C,$02,$20;{$ENDIF} 

               // multiply 2x2 and add
               {$IFDEF FPC}vmulpd ymm3, ymm1, [ebx + eax + 32];{$ELSE}db $C5,$F5,$59,$5C,$03,$20;{$ENDIF} 
               {$IFDEF FPC}vmulpd ymm4, ymm1, [edi + eax + 32];{$ELSE}db $C5,$F5,$59,$64,$07,$20;{$ENDIF} 

               {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
               {$IFDEF FPC}vaddpd ymm2, ymm2, ymm4;{$ELSE}db $C5,$ED,$58,$D4;{$ENDIF} 

               {$IFDEF FPC}vmovapd ymm1, [edx + eax + 64];{$ELSE}db $C5,$FD,$28,$4C,$02,$40;{$ENDIF} 

               // multiply 2x2 and add
               {$IFDEF FPC}vmulpd ymm3, ymm1, [ebx + eax + 64];{$ELSE}db $C5,$F5,$59,$5C,$03,$40;{$ENDIF} 
               {$IFDEF FPC}vmulpd ymm4, ymm1, [edi + eax + 64];{$ELSE}db $C5,$F5,$59,$64,$07,$40;{$ENDIF} 

               {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
               {$IFDEF FPC}vaddpd ymm2, ymm2, ymm4;{$ELSE}db $C5,$ED,$58,$D4;{$ENDIF} 

               {$IFDEF FPC}vmovapd ymm1, [edx + eax + 96];{$ELSE}db $C5,$FD,$28,$4C,$02,$60;{$ENDIF} 

               // multiply 2x2 and add
               {$IFDEF FPC}vmulpd ymm3, ymm1, [ebx + eax + 96];{$ELSE}db $C5,$F5,$59,$5C,$03,$60;{$ENDIF} 
               {$IFDEF FPC}vmulpd ymm4, ymm1, [edi + eax + 96];{$ELSE}db $C5,$F5,$59,$64,$07,$60;{$ENDIF} 

               {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
               {$IFDEF FPC}vaddpd ymm2, ymm2, ymm4;{$ELSE}db $C5,$ED,$58,$D4;{$ENDIF} 

               // end for idx := 0 to width1 div 2
           add eax, 128;
           jnz @@InnerLoop;

           {$IFDEF FPC}vextractf128 xmm1, ymm0, 1;{$ELSE}db $C4,$E3,$7D,$19,$C1,$01;{$ENDIF} 
           {$IFDEF FPC}vextractf128 xmm3, ymm2, 1;{$ELSE}db $C4,$E3,$7D,$19,$D3,$01;{$ENDIF} 

           {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm1;{$ELSE}db $C5,$F9,$7C,$C1;{$ENDIF} 
           {$IFDEF FPC}vhaddpd xmm2, xmm2, xmm3;{$ELSE}db $C5,$E9,$7C,$D3;{$ENDIF} 

           {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm2;{$ELSE}db $C5,$F9,$7C,$C2;{$ENDIF} 

           // store back result
           {$IFDEF FPC}vmovapd [ecx], xmm0;{$ELSE}db $C5,$F9,$29,$01;{$ENDIF} 

           // increment the pointers
           add ecx, 16;
           add edi, LineWidth2x2;
           add ebx, LineWidth2x2;
       // end for x := 0 to width2 - 1
       sub esi, 2;
       jg @@fory2label;

       // last odd line:
       {$IFDEF FPC}vxorpd ymm0, ymm0, ymm0;   {$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} // dest^ := 0

       // for idx := 0 to width1 div 2 do
       mov eax, iter;

       // for x := 0 to width1 - 1
       @@InnerLoop2:
          // prefetch [ecx + edx + 128];
          // prefetch [edi + edx + 128];
          // prefetch [eax + edx + 128];

          {$IFDEF FPC}vmovapd ymm1, [edx + eax];{$ELSE}db $C5,$FD,$28,$0C,$02;{$ENDIF} 

          // multiply 2x2 and add
          {$IFDEF FPC}vmulpd ymm3, ymm1, [ebx + eax];{$ELSE}db $C5,$F5,$59,$1C,$03;{$ENDIF} 
          {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 

          {$IFDEF FPC}vmovapd ymm1, [edx + eax + 32];{$ELSE}db $C5,$FD,$28,$4C,$02,$20;{$ENDIF} 
          {$IFDEF FPC}vmulpd ymm3, ymm1, [ebx + eax + 32];{$ELSE}db $C5,$F5,$59,$5C,$03,$20;{$ENDIF} 
          {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 

          {$IFDEF FPC}vmovapd ymm1, [edx + eax + 64];{$ELSE}db $C5,$FD,$28,$4C,$02,$40;{$ENDIF} 
          {$IFDEF FPC}vmulpd ymm3, ymm1, [ebx + eax + 64];{$ELSE}db $C5,$F5,$59,$5C,$03,$40;{$ENDIF} 
          {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 

          {$IFDEF FPC}vmovapd ymm1, [edx + eax + 96];{$ELSE}db $C5,$FD,$28,$4C,$02,$60;{$ENDIF} 
          {$IFDEF FPC}vmulpd ymm3, ymm1, [ebx + eax + 96];{$ELSE}db $C5,$F5,$59,$5C,$03,$60;{$ENDIF} 
          {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 

          // end for idx := 0 to width1 div 2
       add eax, 128;
       jnz @@InnerLoop2;

       {$IFDEF FPC}vextractf128 xmm1, ymm0, 1;{$ELSE}db $C4,$E3,$7D,$19,$C1,$01;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm1;{$ELSE}db $C5,$F9,$7C,$C1;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} 

       // store back result
       {$IFDEF FPC}vmovsd [ecx], xmm0;{$ELSE}db $C5,$FB,$11,$01;{$ENDIF} 

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
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;
end;

// note mt2 is transposed this time -> width1 and width2 must be the same!
procedure AVXMatrixMultUnAlignedTransposed(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var iter : TASMNativeInt;
begin
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
           {$IFDEF FPC}vxorpd ymm0, ymm0, ymm0;   {$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} // dest^ := 0
           {$IFDEF FPC}vxorpd ymm2, ymm2, ymm2;   {$ELSE}db $C5,$ED,$57,$D2;{$ENDIF} // dest + 1 := 0;
           // for idx := 0 to width1 div 4 do
           mov eax, iter;
           add eax, 32;
           jg @InnerLoopEnd;

           @@InnerLoop:
               {$IFDEF FPC}vmovupd ymm1, [edx + eax - 32];{$ELSE}db $C5,$FD,$10,$4C,$02,$E0;{$ENDIF} 

               // load 2x2 block
               {$IFDEF FPC}vmovupd ymm3, [edi + eax - 32];{$ELSE}db $C5,$FD,$10,$5C,$07,$E0;{$ENDIF} 
               {$IFDEF FPC}vmovupd ymm4, [esi + eax - 32];{$ELSE}db $C5,$FD,$10,$64,$06,$E0;{$ENDIF} 

               // multiply 2x2 and add
               {$IFDEF FPC}vmulpd ymm3, ymm3, ymm1;{$ELSE}db $C5,$E5,$59,$D9;{$ENDIF} 
               {$IFDEF FPC}vmulpd ymm4, ymm4, ymm1;{$ELSE}db $C5,$DD,$59,$E1;{$ENDIF} 

               {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
               {$IFDEF FPC}vaddpd ymm2, ymm2, ymm4;{$ELSE}db $C5,$ED,$58,$D4;{$ENDIF} 

               // end for idx := 0 to width1 div 2
           add eax, 32;
           jle @@InnerLoop;

           {$IFDEF FPC}vextractf128 xmm1, ymm0, 1;{$ELSE}db $C4,$E3,$7D,$19,$C1,$01;{$ENDIF} 
           {$IFDEF FPC}vextractf128 xmm3, ymm2, 1;{$ELSE}db $C4,$E3,$7D,$19,$D3,$01;{$ENDIF} 

           {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm1;{$ELSE}db $C5,$F9,$7C,$C1;{$ENDIF} 
           {$IFDEF FPC}vhaddpd xmm2, xmm2, xmm3;{$ELSE}db $C5,$E9,$7C,$D3;{$ENDIF} 

           @InnerLoopEnd:

           sub eax, 32;
           jz @@InnerLoopEnd2;

           // do the final few elements
           @@InnerLoop2:
               {$IFDEF FPC}vmovsd xmm1, [edx + eax];{$ELSE}db $C5,$FB,$10,$0C,$02;{$ENDIF} 

               // load 2x2 block
               {$IFDEF FPC}vmovsd xmm3, [edi + eax];{$ELSE}db $C5,$FB,$10,$1C,$07;{$ENDIF} 
               {$IFDEF FPC}vmovsd xmm4, [esi + eax];{$ELSE}db $C5,$FB,$10,$24,$06;{$ENDIF} 

               // multiply 2x2 and add
               {$IFDEF FPC}vmulsd xmm3, xmm3, xmm1;{$ELSE}db $C5,$E3,$59,$D9;{$ENDIF} 
               {$IFDEF FPC}vmulsd xmm4, xmm4, xmm1;{$ELSE}db $C5,$DB,$59,$E1;{$ENDIF} 

               {$IFDEF FPC}vaddsd xmm0, xmm0, xmm3;{$ELSE}db $C5,$FB,$58,$C3;{$ENDIF} 
               {$IFDEF FPC}vaddsd xmm2, xmm2, xmm4;{$ELSE}db $C5,$EB,$58,$D4;{$ENDIF} 

           add eax, 8;
           jnz @@InnerLoop2;

           @@InnerLoopEnd2:

           // final add and compact result
           {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm2;{$ELSE}db $C5,$F9,$7C,$C2;{$ENDIF} 

           // store back result
           {$IFDEF FPC}vmovupd [ecx], xmm0;{$ELSE}db $C5,$F9,$11,$01;{$ENDIF} 

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
       {$IFDEF FPC}vxorpd ymm0, ymm0, ymm0;   {$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} // dest^ := 0

       // for idx := 0 to width1 div 4 do
       mov eax, iter;
       add eax, 32;
       jg @LastLineInnerLoopEnd;

       @@LastLineInnerLoop:
            {$IFDEF FPC}vmovupd ymm1, [edx + eax - 32];{$ELSE}db $C5,$FD,$10,$4C,$02,$E0;{$ENDIF} 

            // load block
            {$IFDEF FPC}vmovupd ymm3, [edi + eax - 32];{$ELSE}db $C5,$FD,$10,$5C,$07,$E0;{$ENDIF} 

            // multiply and add
            {$IFDEF FPC}vmulpd ymm3, ymm3, ymm1;{$ELSE}db $C5,$E5,$59,$D9;{$ENDIF} 

            {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 

            // end for idx := 0 to width1 div 2
       add eax, 32;
       jle @@LastLineInnerLoop;

       {$IFDEF FPC}vextractf128 xmm1, ymm0, 1;{$ELSE}db $C4,$E3,$7D,$19,$C1,$01;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm1;{$ELSE}db $C5,$F9,$7C,$C1;{$ENDIF} 

       @LastLineInnerLoopEnd:
       sub eax, 32;
       jz @@LastLineInnerLoopEnd2;

       // do the final few elements
       @@LastLineInnerLoop2:
            {$IFDEF FPC}vmovsd xmm1, [edx + eax];{$ELSE}db $C5,$FB,$10,$0C,$02;{$ENDIF} 
            {$IFDEF FPC}vmovsd xmm3, [edi + eax];{$ELSE}db $C5,$FB,$10,$1C,$07;{$ENDIF} 

            // multiply and add
            {$IFDEF FPC}vmulsd xmm3, xmm3, xmm1;{$ELSE}db $C5,$E3,$59,$D9;{$ENDIF} 
            {$IFDEF FPC}vaddsd xmm0, xmm0, xmm3;{$ELSE}db $C5,$FB,$58,$C3;{$ENDIF} 
       // next element
       add eax, 8;
       jnz @@LastLineInnerLoop2;

       @@LastLineInnerLoopEnd2:

       // final add and compact result
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} 

       // store back result
       {$IFDEF FPC}vmovsd [ecx], xmm0;{$ELSE}db $C5,$FB,$11,$01;{$ENDIF} 

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
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;
end;

// note mt2 is transposed this time -> width1 and width2 must be the same!
procedure AVXMatrixMultAlignedTransposed(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var iter : TASMNativeInt;
begin
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
           {$IFDEF FPC}vxorpd ymm0, ymm0, ymm0;   {$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} // dest^ := 0
           {$IFDEF FPC}vxorpd ymm2, ymm2, ymm2;   {$ELSE}db $C5,$ED,$57,$D2;{$ENDIF} // dest + 1 := 0;
           // for idx := 0 to width1 div 4 do
           mov eax, iter;
           add eax, 32;
           jg @InnerLoopEnd;

           @@InnerLoop:
               {$IFDEF FPC}vmovapd ymm1, [edx + eax - 32];{$ELSE}db $C5,$FD,$28,$4C,$02,$E0;{$ENDIF} 

               // multiply 2x2 and add
               {$IFDEF FPC}vmulpd ymm3, ymm1, [edi + eax - 32];{$ELSE}db $C5,$F5,$59,$5C,$07,$E0;{$ENDIF} 
               {$IFDEF FPC}vmulpd ymm4, ymm1, [esi + eax - 32];{$ELSE}db $C5,$F5,$59,$64,$06,$E0;{$ENDIF} 

               {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
               {$IFDEF FPC}vaddpd ymm2, ymm2, ymm4;{$ELSE}db $C5,$ED,$58,$D4;{$ENDIF} 

               // end for idx := 0 to width1 div 2
           add eax, 32;
           jle @@InnerLoop;

           {$IFDEF FPC}vextractf128 xmm1, ymm0, 1;{$ELSE}db $C4,$E3,$7D,$19,$C1,$01;{$ENDIF} 
           {$IFDEF FPC}vextractf128 xmm3, ymm2, 1;{$ELSE}db $C4,$E3,$7D,$19,$D3,$01;{$ENDIF} 

           {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm1;{$ELSE}db $C5,$F9,$7C,$C1;{$ENDIF} 
           {$IFDEF FPC}vhaddpd xmm2, xmm2, xmm3;{$ELSE}db $C5,$E9,$7C,$D3;{$ENDIF} 

           @InnerLoopEnd:

           sub eax, 32;
           jz @@InnerLoopEnd2;

           // do the final few elements
           @@InnerLoop2:
               {$IFDEF FPC}vmovsd xmm1, [edx + eax];{$ELSE}db $C5,$FB,$10,$0C,$02;{$ENDIF} 

               // load 2x2 block
               {$IFDEF FPC}vmovsd xmm3, [edi + eax];{$ELSE}db $C5,$FB,$10,$1C,$07;{$ENDIF} 
               {$IFDEF FPC}vmovsd xmm4, [esi + eax];{$ELSE}db $C5,$FB,$10,$24,$06;{$ENDIF} 

               // multiply 2x2 and add
               {$IFDEF FPC}vmulsd xmm3, xmm3, xmm1;{$ELSE}db $C5,$E3,$59,$D9;{$ENDIF} 
               {$IFDEF FPC}vmulsd xmm4, xmm4, xmm1;{$ELSE}db $C5,$DB,$59,$E1;{$ENDIF} 

               {$IFDEF FPC}vaddsd xmm0, xmm0, xmm3;{$ELSE}db $C5,$FB,$58,$C3;{$ENDIF} 
               {$IFDEF FPC}vaddsd xmm2, xmm2, xmm4;{$ELSE}db $C5,$EB,$58,$D4;{$ENDIF} 

           add eax, 8;
           jnz @@InnerLoop2;

           @@InnerLoopEnd2:

           // final add and compact result
           {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm2;{$ELSE}db $C5,$F9,$7C,$C2;{$ENDIF} 

           // store back result
           {$IFDEF FPC}vmovapd [ecx], xmm0;{$ELSE}db $C5,$F9,$29,$01;{$ENDIF} 

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
       {$IFDEF FPC}vxorpd ymm0, ymm0, ymm0;   {$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} // dest^ := 0

       // for idx := 0 to width1 div 4 do
       mov eax, iter;
       add eax, 32;
       jg @LastLineInnerLoopEnd;

       @@LastLineInnerLoop:
            {$IFDEF FPC}vmovapd ymm1, [edx + eax - 32];{$ELSE}db $C5,$FD,$28,$4C,$02,$E0;{$ENDIF} 

            // load block
            {$IFDEF FPC}vmovapd ymm3, [edi + eax - 32];{$ELSE}db $C5,$FD,$28,$5C,$07,$E0;{$ENDIF} 

            // multiply and add
            {$IFDEF FPC}vmulpd ymm3, ymm3, ymm1;{$ELSE}db $C5,$E5,$59,$D9;{$ENDIF} 
            {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 
            // end for idx := 0 to width1 div 2
       add eax, 32;
       jle @@LastLineInnerLoop;

       {$IFDEF FPC}vextractf128 xmm1, ymm0, 1;{$ELSE}db $C4,$E3,$7D,$19,$C1,$01;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm1;{$ELSE}db $C5,$F9,$7C,$C1;{$ENDIF} 

       @LastLineInnerLoopEnd:
       sub eax, 32;
       jz @@LastLineInnerLoopEnd2;

       // do the final few elements
       @@LastLineInnerLoop2:
            {$IFDEF FPC}vmovsd xmm1, [edx + eax];{$ELSE}db $C5,$FB,$10,$0C,$02;{$ENDIF} 
            {$IFDEF FPC}vmovsd xmm3, [edi + eax];{$ELSE}db $C5,$FB,$10,$1C,$07;{$ENDIF} 

            // multiply and add
            {$IFDEF FPC}vmulsd xmm3, xmm3, xmm1;{$ELSE}db $C5,$E3,$59,$D9;{$ENDIF} 
            {$IFDEF FPC}vaddsd xmm0, xmm0, xmm3;{$ELSE}db $C5,$FB,$58,$C3;{$ENDIF} 
       // next element
       add eax, 8;
       jnz @@LastLineInnerLoop2;

       @@LastLineInnerLoopEnd2:

       // final add and compact result
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} 

       // store back result
       {$IFDEF FPC}vmovsd [ecx], xmm0;{$ELSE}db $C5,$FB,$11,$01;{$ENDIF} 

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
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;
end;

{$ENDIF}

end.
