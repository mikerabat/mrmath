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


unit FMAMatrixMultTransposedOperations;

// ##############################################################
// #### Assembler optimized matrix multiplication assuming a transposed second
// #### matrix
// ##############################################################

interface

{$I 'mrMath_CPU.inc'}

{$IFNDEF x64}

procedure FMAMatrixMultAlignedTransposed(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure FMAMatrixMultAlignedEvenW1EvenH2TransposedMod16(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure FMAMatrixMultAlignedEvenW1OddH2TransposedMod16(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure FMAMatrixMultUnAlignedTransposed(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

{$ENDIF}

implementation

{$IFNDEF x64}

procedure FMAMatrixMultAlignedEvenW1EvenH2TransposedMod16(dest : PDouble; const destLineWidth : NativeInt;
    mt1, mt2 : PDouble; width1 : NativeInt;
    height1 : NativeInt; width2 : NativeInt; height2 : NativeInt;
    const LineWidth1, LineWidth2 : NativeInt);
var iter : NativeInt;
    LineWidth2x2 : NativeInt;
    destOffset : NativeInt;
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

           {$IFDEF AVXSUP}vxorpd ymm0, ymm0, ymm0;                    {$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} // dest^ := 0
           {$IFDEF AVXSUP}vxorpd ymm2, ymm2, ymm2;                    {$ELSE}db $C5,$ED,$57,$D2;{$ENDIF} // dest + 1 := 0;
           // for idx := 0 to width1 div 2 do
           mov edx, iter;

           // for x := 0 to width1 - 1
           @@InnerLoop:
               // prefetch [ecx + edx + 128];
               // prefetch [edi + edx + 128];
               // prefetch [eax + edx + 128];

               {$IFDEF AVXSUP}vmovapd ymm1, [ecx + edx];              {$ELSE}db $C5,$FD,$28,$0C,$11;{$ENDIF} 

               // multiply 2x2 and add
               {$IFDEF AVXSUP}vfmadd231pd ymm0, ymm1, [edi + edx];    {$ELSE}db $C4,$E2,$F5,$B8,$04,$17;{$ENDIF} 
               {$IFDEF AVXSUP}vfmadd231pd ymm2, ymm1, [esi + edx];    {$ELSE}db $C4,$E2,$F5,$B8,$14,$16;{$ENDIF} 

               {$IFDEF AVXSUP}vmovapd ymm1, [ecx + edx + 32];         {$ELSE}db $C5,$FD,$28,$4C,$11,$20;{$ENDIF} 

               // multiply 2x2 and add
               {$IFDEF AVXSUP}vfmadd231pd ymm0, ymm1, [edi + edx + 32];{$ELSE}db $C4,$E2,$F5,$B8,$44,$17,$20;{$ENDIF} 
               {$IFDEF AVXSUP}vfmadd231pd ymm2, ymm1, [esi + edx + 32];{$ELSE}db $C4,$E2,$F5,$B8,$54,$16,$20;{$ENDIF} 

               {$IFDEF AVXSUP}vmovapd ymm1, [ecx + edx + 64];         {$ELSE}db $C5,$FD,$28,$4C,$11,$40;{$ENDIF} 

               // multiply 2x2 and add
               {$IFDEF AVXSUP}vfmadd231pd ymm0, ymm1, [edi + edx + 64];{$ELSE}db $C4,$E2,$F5,$B8,$44,$17,$40;{$ENDIF} 
               {$IFDEF AVXSUP}vfmadd231pd ymm2, ymm1, [esi + edx + 64];{$ELSE}db $C4,$E2,$F5,$B8,$54,$16,$40;{$ENDIF} 


               {$IFDEF AVXSUP}vmovapd ymm1, [ecx + edx + 96];         {$ELSE}db $C5,$FD,$28,$4C,$11,$60;{$ENDIF} 

               // multiply 2x2 and add
               {$IFDEF AVXSUP}vfmadd231pd ymm0, ymm1, [edi + edx + 96];{$ELSE}db $C4,$E2,$F5,$B8,$44,$17,$60;{$ENDIF} 
               {$IFDEF AVXSUP}vfmadd231pd ymm2, ymm1, [esi + edx + 96];{$ELSE}db $C4,$E2,$F5,$B8,$54,$16,$60;{$ENDIF} 

               // end for idx := 0 to width1 div 2
           add edx, 128;
           jnz @@InnerLoop;

           {$IFDEF AVXSUP}vextractf128 xmm1, ymm0, 1;                 {$ELSE}db $C4,$E3,$7D,$19,$C1,$01;{$ENDIF} 
           {$IFDEF AVXSUP}vextractf128 xmm3, ymm2, 1;                 {$ELSE}db $C4,$E3,$7D,$19,$D3,$01;{$ENDIF} 

           {$IFDEF AVXSUP}vhaddpd xmm0, xmm0, xmm1;                   {$ELSE}db $C5,$F9,$7C,$C1;{$ENDIF} 
           {$IFDEF AVXSUP}vhaddpd xmm2, xmm2, xmm3;                   {$ELSE}db $C5,$E9,$7C,$D3;{$ENDIF} 

           {$IFDEF AVXSUP}vhaddpd xmm0, xmm0, xmm2;                   {$ELSE}db $C5,$F9,$7C,$C2;{$ENDIF} 

           // store back result
           {$IFDEF AVXSUP}vmovapd [eax], xmm0;                        {$ELSE}db $C5,$F9,$29,$00;{$ENDIF} 

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
   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop edi;
   pop esi;
   pop ebx;
end;

procedure FMAMatrixMultAlignedEvenW1OddH2TransposedMod16(dest : PDouble; const destLineWidth : NativeInt;
    mt1, mt2 : PDouble; width1 : NativeInt;
    height1 : NativeInt; width2 : NativeInt; height2 : NativeInt;
    const LineWidth1, LineWidth2 : NativeInt);
var iter : NativeInt;
    LineWidth2x2 : NativeInt;
    destOffset : NativeInt;
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

           {$IFDEF AVXSUP}vxorpd ymm0, ymm0, ymm0;                    {$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} // dest^ := 0
           {$IFDEF AVXSUP}vxorpd ymm2, ymm2, ymm2;                    {$ELSE}db $C5,$ED,$57,$D2;{$ENDIF} // dest + 1 := 0;
           // for idx := 0 to width1 div 2 do
           mov edx, iter;

           // for x := 0 to width1 - 1
           @@InnerLoop:
               // prefetch [ecx + edx + 128];
               // prefetch [edi + edx + 128];
               // prefetch [eax + edx + 128];

               {$IFDEF AVXSUP}vmovapd ymm1, [ecx + edx];              {$ELSE}db $C5,$FD,$28,$0C,$11;{$ENDIF} 

               // multiply 2x2 and add
               {$IFDEF AVXSUP}vfmadd231pd ymm0, ymm1, [edi + edx];    {$ELSE}db $C4,$E2,$F5,$B8,$04,$17;{$ENDIF} 
               {$IFDEF AVXSUP}vfmadd231pd ymm2, ymm1, [esi + edx];    {$ELSE}db $C4,$E2,$F5,$B8,$14,$16;{$ENDIF} 


               {$IFDEF AVXSUP}vmovapd ymm1, [ecx + edx + 32];         {$ELSE}db $C5,$FD,$28,$4C,$11,$20;{$ENDIF} 

               // multiply 2x2 and add
               {$IFDEF AVXSUP}vfmadd231pd ymm0, ymm1, [edi + edx + 32];{$ELSE}db $C4,$E2,$F5,$B8,$44,$17,$20;{$ENDIF} 
               {$IFDEF AVXSUP}vfmadd231pd ymm2, ymm1, [esi + edx + 32];{$ELSE}db $C4,$E2,$F5,$B8,$54,$16,$20;{$ENDIF} 


               {$IFDEF AVXSUP}vmovapd ymm1, [ecx + edx + 64];         {$ELSE}db $C5,$FD,$28,$4C,$11,$40;{$ENDIF} 

               // multiply 2x2 and add
               {$IFDEF AVXSUP}vfmadd231pd ymm0, ymm1, [edi + edx + 64];{$ELSE}db $C4,$E2,$F5,$B8,$44,$17,$40;{$ENDIF} 
               {$IFDEF AVXSUP}vfmadd231pd ymm2, ymm1, [esi + edx + 64];{$ELSE}db $C4,$E2,$F5,$B8,$54,$16,$40;{$ENDIF} 


               {$IFDEF AVXSUP}vmovapd ymm1, [ecx + edx + 96];         {$ELSE}db $C5,$FD,$28,$4C,$11,$60;{$ENDIF} 

               // multiply 2x2 and add
               {$IFDEF AVXSUP}vfmadd231pd ymm0, ymm1, [edi + edx + 96];{$ELSE}db $C4,$E2,$F5,$B8,$44,$17,$60;{$ENDIF} 
               {$IFDEF AVXSUP}vfmadd231pd ymm2, ymm1, [esi + edx + 96];{$ELSE}db $C4,$E2,$F5,$B8,$54,$16,$60;{$ENDIF} 

               // end for idx := 0 to width1 div 2
           add edx, 128;
           jnz @@InnerLoop;

           {$IFDEF AVXSUP}vextractf128 xmm1, ymm0, 1;                 {$ELSE}db $C4,$E3,$7D,$19,$C1,$01;{$ENDIF} 
           {$IFDEF AVXSUP}vextractf128 xmm3, ymm2, 1;                 {$ELSE}db $C4,$E3,$7D,$19,$D3,$01;{$ENDIF} 

           {$IFDEF AVXSUP}vhaddpd xmm0, xmm0, xmm1;                   {$ELSE}db $C5,$F9,$7C,$C1;{$ENDIF} 
           {$IFDEF AVXSUP}vhaddpd xmm2, xmm2, xmm3;                   {$ELSE}db $C5,$E9,$7C,$D3;{$ENDIF} 

           {$IFDEF AVXSUP}vhaddpd xmm0, xmm0, xmm2;                   {$ELSE}db $C5,$F9,$7C,$C2;{$ENDIF} 

           // store back result
           {$IFDEF AVXSUP}vmovapd [eax], xmm0;                        {$ELSE}db $C5,$F9,$29,$00;{$ENDIF} 

           // increment the pointers
           add eax, 16;
           add edi, LineWidth2x2;
       // end for x := 0 to width2 - 1
       dec ebx;
       jnz @@fory2label;

       // last odd line:
       {$IFDEF AVXSUP}vxorpd ymm0, ymm0, ymm0;                        {$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} // dest^ := 0

       // for idx := 0 to width1 div 2 do
       mov edx, iter;

       // for x := 0 to width1 - 1
       @@InnerLoop2:
          // prefetch [ecx + edx + 128];
          // prefetch [edi + edx + 128];
          // prefetch [eax + edx + 128];

          {$IFDEF AVXSUP}vmovapd ymm1, [ecx + edx];                   {$ELSE}db $C5,$FD,$28,$0C,$11;{$ENDIF} 

          // multiply 2x2 and add
          {$IFDEF AVXSUP}vfmadd231pd ymm0, ymm1, [edi + edx];         {$ELSE}db $C4,$E2,$F5,$B8,$04,$17;{$ENDIF} 

          {$IFDEF AVXSUP}vmovapd ymm1, [ecx + edx + 32];              {$ELSE}db $C5,$FD,$28,$4C,$11,$20;{$ENDIF} 
          {$IFDEF AVXSUP}vfmadd231pd ymm0, ymm1, [edi + edx + 32];    {$ELSE}db $C4,$E2,$F5,$B8,$44,$17,$20;{$ENDIF} 

          {$IFDEF AVXSUP}vmovapd ymm1, [ecx + edx + 64];              {$ELSE}db $C5,$FD,$28,$4C,$11,$40;{$ENDIF} 
          {$IFDEF AVXSUP}vfmadd231pd ymm0, ymm1, [edi + edx + 64];    {$ELSE}db $C4,$E2,$F5,$B8,$44,$17,$40;{$ENDIF} 

          {$IFDEF AVXSUP}vmovapd ymm1, [ecx + edx + 96];              {$ELSE}db $C5,$FD,$28,$4C,$11,$60;{$ENDIF} 
          {$IFDEF AVXSUP}vfmadd231pd ymm0, ymm1, [edi + edx + 96];    {$ELSE}db $C4,$E2,$F5,$B8,$44,$17,$60;{$ENDIF} 

          // end for idx := 0 to width1 div 2
       add edx, 128;
       jnz @@InnerLoop2;

       {$IFDEF AVXSUP}vextractf128 xmm1, ymm0, 1;                     {$ELSE}db $C4,$E3,$7D,$19,$C1,$01;{$ENDIF} 
       {$IFDEF AVXSUP}vhaddpd xmm0, xmm0, xmm1;                       {$ELSE}db $C5,$F9,$7C,$C1;{$ENDIF} 
       {$IFDEF AVXSUP}vhaddpd xmm0, xmm0, xmm0;                       {$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} 

       // store back result
       {$IFDEF AVXSUP}vmovsd [eax], xmm0;                             {$ELSE}db $C5,$FB,$11,$00;{$ENDIF} 

       // update pointers
       add ecx, LineWidth1;
       add eax, destOffset;

   // end for y := 0 to height1 - 1
   dec Height1;
   jnz @@forylabel;

   // epilog - cleanup stack
   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop edi;
   pop esi;
   pop ebx;
end;

// note mt2 is transposed this time -> width1 and width2 must be the same!
procedure FMAMatrixMultUnAlignedTransposed(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt);
var iter : NativeInt;
    destOffset : NativeInt;
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
           {$IFDEF AVXSUP}vxorpd ymm0, ymm0, ymm0;                    {$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} // dest^ := 0
           {$IFDEF AVXSUP}vxorpd ymm2, ymm2, ymm2;                    {$ELSE}db $C5,$ED,$57,$D2;{$ENDIF} // dest + 1 := 0;
           // for idx := 0 to width1 div 4 do
           mov edx, iter;
           add edx, 32;
           jg @InnerLoopEnd;

           @@InnerLoop:
               {$IFDEF AVXSUP}vmovupd ymm1, [ecx + edx - 32];         {$ELSE}db $C5,$FD,$10,$4C,$11,$E0;{$ENDIF} 

               // load 2x2 block
               {$IFDEF AVXSUP}vmovupd ymm3, [edi + edx - 32];         {$ELSE}db $C5,$FD,$10,$5C,$17,$E0;{$ENDIF} 
               {$IFDEF AVXSUP}vmovupd ymm4, [esi + edx - 32];         {$ELSE}db $C5,$FD,$10,$64,$16,$E0;{$ENDIF} 

               // multiply 2x2 and add
               {$IFDEF AVXSUP}vfmadd231pd ymm0, ymm1, ymm3;           {$ELSE}db $C4,$E2,$F5,$B8,$C3;{$ENDIF} 
               {$IFDEF AVXSUP}vfmadd231pd ymm2, ymm1, ymm4;           {$ELSE}db $C4,$E2,$F5,$B8,$D4;{$ENDIF} 

               // end for idx := 0 to width1 div 2
           add edx, 32;
           jle @@InnerLoop;

           {$IFDEF AVXSUP}vextractf128 xmm1, ymm0, 1;                 {$ELSE}db $C4,$E3,$7D,$19,$C1,$01;{$ENDIF} 
           {$IFDEF AVXSUP}vextractf128 xmm3, ymm2, 1;                 {$ELSE}db $C4,$E3,$7D,$19,$D3,$01;{$ENDIF} 

           {$IFDEF AVXSUP}vhaddpd xmm0, xmm0, xmm1;                   {$ELSE}db $C5,$F9,$7C,$C1;{$ENDIF} 
           {$IFDEF AVXSUP}vhaddpd xmm2, xmm2, xmm3;                   {$ELSE}db $C5,$E9,$7C,$D3;{$ENDIF} 

           @InnerLoopEnd:

           sub edx, 32;
           jz @@InnerLoopEnd2;

           // do the final few elements
           @@InnerLoop2:
               {$IFDEF AVXSUP}vmovsd xmm1, [ecx + edx];               {$ELSE}db $C5,$FB,$10,$0C,$11;{$ENDIF} 

               // load 2x2 block
               {$IFDEF AVXSUP}vmovsd xmm3, [edi + edx];               {$ELSE}db $C5,$FB,$10,$1C,$17;{$ENDIF} 
               {$IFDEF AVXSUP}vmovsd xmm4, [esi + edx];               {$ELSE}db $C5,$FB,$10,$24,$16;{$ENDIF} 

               // multiply 2x2 and add
               {$IFDEF AVXSUP}vmulsd xmm3, xmm3, xmm1;                {$ELSE}db $C5,$E3,$59,$D9;{$ENDIF} 
               {$IFDEF AVXSUP}vmulsd xmm4, xmm4, xmm1;                {$ELSE}db $C5,$DB,$59,$E1;{$ENDIF} 

               {$IFDEF AVXSUP}vaddsd xmm0, xmm0, xmm3;                {$ELSE}db $C5,$FB,$58,$C3;{$ENDIF} 
               {$IFDEF AVXSUP}vaddsd xmm2, xmm2, xmm4;                {$ELSE}db $C5,$EB,$58,$D4;{$ENDIF} 

           add edx, 8;
           jnz @@InnerLoop2;

           @@InnerLoopEnd2:

           // final add and compact result
           {$IFDEF AVXSUP}vhaddpd xmm0, xmm0, xmm2;                   {$ELSE}db $C5,$F9,$7C,$C2;{$ENDIF} 

           // store back result
           {$IFDEF AVXSUP}vmovupd [eax], xmm0;                        {$ELSE}db $C5,$F9,$11,$00;{$ENDIF} 

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
       {$IFDEF AVXSUP}vxorpd ymm0, ymm0, ymm0;                        {$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} // dest^ := 0

       // for idx := 0 to width1 div 4 do
       mov edx, iter;
       add edx, 32;
       jg @LastLineInnerLoopEnd;

       @@LastLineInnerLoop:
            {$IFDEF AVXSUP}vmovupd ymm1, [ecx + edx - 32];            {$ELSE}db $C5,$FD,$10,$4C,$11,$E0;{$ENDIF} 

            // load block
            {$IFDEF AVXSUP}vmovupd ymm3, [edi + edx - 32];            {$ELSE}db $C5,$FD,$10,$5C,$17,$E0;{$ENDIF} 

            // multiply and add
            {$IFDEF AVXSUP}vfmadd231pd ymm0, ymm1, ymm3;              {$ELSE}db $C4,$E2,$F5,$B8,$C3;{$ENDIF} 

            // end for idx := 0 to width1 div 2
       add edx, 32;
       jle @@LastLineInnerLoop;

       {$IFDEF AVXSUP}vextractf128 xmm1, ymm0, 1;                     {$ELSE}db $C4,$E3,$7D,$19,$C1,$01;{$ENDIF} 
       {$IFDEF AVXSUP}vhaddpd xmm0, xmm0, xmm1;                       {$ELSE}db $C5,$F9,$7C,$C1;{$ENDIF} 

       @LastLineInnerLoopEnd:
       sub edx, 32;
       jz @@LastLineInnerLoopEnd2;

       // do the final few elements
       @@LastLineInnerLoop2:
            {$IFDEF AVXSUP}vmovsd xmm1, [ecx + edx];                  {$ELSE}db $C5,$FB,$10,$0C,$11;{$ENDIF} 
            {$IFDEF AVXSUP}vmovsd xmm3, [edi + edx];                  {$ELSE}db $C5,$FB,$10,$1C,$17;{$ENDIF} 

            // multiply and add
            {$IFDEF AVXSUP}vmulsd xmm3, xmm3, xmm1;                   {$ELSE}db $C5,$E3,$59,$D9;{$ENDIF} 
            {$IFDEF AVXSUP}vaddsd xmm0, xmm0, xmm3;                   {$ELSE}db $C5,$FB,$58,$C3;{$ENDIF} 
       // next element
       add edx, 8;
       jnz @@LastLineInnerLoop2;

       @@LastLineInnerLoopEnd2:

       // final add and compact result
       {$IFDEF AVXSUP}vhaddpd xmm0, xmm0, xmm0;                       {$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} 

       // store back result
       {$IFDEF AVXSUP}vmovsd [eax], xmm0;                             {$ELSE}db $C5,$FB,$11,$00;{$ENDIF} 
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
   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;

// note mt2 is transposed this time -> width1 and width2 must be the same!
procedure FMAMatrixMultAlignedTransposed(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt);
var iter : NativeInt;
    destOffset : NativeInt;
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
           {$IFDEF AVXSUP}vxorpd ymm0, ymm0, ymm0;                    {$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} // dest^ := 0
           {$IFDEF AVXSUP}vxorpd ymm2, ymm2, ymm2;                    {$ELSE}db $C5,$ED,$57,$D2;{$ENDIF} // dest + 1 := 0;
           // for idx := 0 to width1 div 4 do
           mov edx, iter;
           add edx, 32;
           jg @InnerLoopEnd;

           @@InnerLoop:
               {$IFDEF AVXSUP}vmovapd ymm1, [ecx + edx - 32];         {$ELSE}db $C5,$FD,$28,$4C,$11,$E0;{$ENDIF} 

               // multiply 2x2 and add
               {$IFDEF AVXSUP}vfmadd231pd ymm0, ymm1, [edi + edx - 32];{$ELSE}db $C4,$E2,$F5,$B8,$44,$17,$E0;{$ENDIF} 
               {$IFDEF AVXSUP}vfmadd231pd ymm2, ymm1, [esi + edx - 32];{$ELSE}db $C4,$E2,$F5,$B8,$54,$16,$E0;{$ENDIF} 

               // end for idx := 0 to width1 div 2
           add edx, 32;
           jle @@InnerLoop;

           {$IFDEF AVXSUP}vextractf128 xmm1, ymm0, 1;                 {$ELSE}db $C4,$E3,$7D,$19,$C1,$01;{$ENDIF} 
           {$IFDEF AVXSUP}vextractf128 xmm3, ymm2, 1;                 {$ELSE}db $C4,$E3,$7D,$19,$D3,$01;{$ENDIF} 

           {$IFDEF AVXSUP}vhaddpd xmm0, xmm0, xmm1;                   {$ELSE}db $C5,$F9,$7C,$C1;{$ENDIF} 
           {$IFDEF AVXSUP}vhaddpd xmm2, xmm2, xmm3;                   {$ELSE}db $C5,$E9,$7C,$D3;{$ENDIF} 

           @InnerLoopEnd:

           sub edx, 32;
           jz @@InnerLoopEnd2;

           // do the final few elements
           @@InnerLoop2:
               {$IFDEF AVXSUP}vmovsd xmm1, [ecx + edx];               {$ELSE}db $C5,$FB,$10,$0C,$11;{$ENDIF} 

               // load 2x2 block
               {$IFDEF AVXSUP}vmovsd xmm3, [edi + edx];               {$ELSE}db $C5,$FB,$10,$1C,$17;{$ENDIF} 
               {$IFDEF AVXSUP}vmovsd xmm4, [esi + edx];               {$ELSE}db $C5,$FB,$10,$24,$16;{$ENDIF} 

               // multiply 2x2 and add
               {$IFDEF AVXSUP}vmulsd xmm3, xmm3, xmm1;                {$ELSE}db $C5,$E3,$59,$D9;{$ENDIF} 
               {$IFDEF AVXSUP}vmulsd xmm4, xmm4, xmm1;                {$ELSE}db $C5,$DB,$59,$E1;{$ENDIF} 

               {$IFDEF AVXSUP}vaddsd xmm0, xmm0, xmm3;                {$ELSE}db $C5,$FB,$58,$C3;{$ENDIF} 
               {$IFDEF AVXSUP}vaddsd xmm2, xmm2, xmm4;                {$ELSE}db $C5,$EB,$58,$D4;{$ENDIF} 

           add edx, 8;
           jnz @@InnerLoop2;

           @@InnerLoopEnd2:

           // final add and compact result
           {$IFDEF AVXSUP}vhaddpd xmm0, xmm0, xmm2;                   {$ELSE}db $C5,$F9,$7C,$C2;{$ENDIF} 

           // store back result
           {$IFDEF AVXSUP}vmovapd [eax], xmm0;                        {$ELSE}db $C5,$F9,$29,$00;{$ENDIF} 

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
       {$IFDEF AVXSUP}vxorpd ymm0, ymm0, ymm0;                        {$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} // dest^ := 0

       // for idx := 0 to width1 div 4 do
       mov edx, iter;
       add edx, 32;
       jg @LastLineInnerLoopEnd;

       @@LastLineInnerLoop:
            {$IFDEF AVXSUP}vmovapd ymm1, [ecx + edx - 32];            {$ELSE}db $C5,$FD,$28,$4C,$11,$E0;{$ENDIF} 

            // multiply and add
            {$IFDEF AVXSUP}vfmadd231pd ymm0, ymm1, [edi + edx - 32];  {$ELSE}db $C4,$E2,$F5,$B8,$44,$17,$E0;{$ENDIF} 
            // end for idx := 0 to width1 div 2
       add edx, 32;
       jle @@LastLineInnerLoop;

       {$IFDEF AVXSUP}vextractf128 xmm1, ymm0, 1;                     {$ELSE}db $C4,$E3,$7D,$19,$C1,$01;{$ENDIF} 
       {$IFDEF AVXSUP}vhaddpd xmm0, xmm0, xmm1;                       {$ELSE}db $C5,$F9,$7C,$C1;{$ENDIF} 

       @LastLineInnerLoopEnd:
       sub edx, 32;
       jz @@LastLineInnerLoopEnd2;

       // do the final few elements
       @@LastLineInnerLoop2:
            {$IFDEF AVXSUP}vmovsd xmm1, [ecx + edx];                  {$ELSE}db $C5,$FB,$10,$0C,$11;{$ENDIF} 
            {$IFDEF AVXSUP}vmovsd xmm3, [edi + edx];                  {$ELSE}db $C5,$FB,$10,$1C,$17;{$ENDIF} 

            // multiply and add
            {$IFDEF AVXSUP}vmulsd xmm3, xmm3, xmm1;                   {$ELSE}db $C5,$E3,$59,$D9;{$ENDIF} 
            {$IFDEF AVXSUP}vaddsd xmm0, xmm0, xmm3;                   {$ELSE}db $C5,$FB,$58,$C3;{$ENDIF} 
       // next element
       add edx, 8;
       jnz @@LastLineInnerLoop2;

       @@LastLineInnerLoopEnd2:

       // final add and compact result
       {$IFDEF AVXSUP}vhaddpd xmm0, xmm0, xmm0;                       {$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} 

       // store back result
       {$IFDEF AVXSUP}vmovsd [eax], xmm0;                             {$ELSE}db $C5,$FB,$11,$00;{$ENDIF} 
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
   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;

{$ENDIF}

end.
