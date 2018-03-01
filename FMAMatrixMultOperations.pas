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


unit FMAMatrixMultOperations;

interface

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}
{$IFNDEF x64}

uses MatrixConst;

// full matrix operations
procedure FMAMatrixMultAligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1, height1, width2, height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
procedure FMAMatrixMultUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1, height1, width2, height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);


// some special types of multiplications used e.g. in QR Decomposition
// dest = mt1'*mt2; where mt2 is a lower triangular matrix and the operation is transposition
// the function assumes a unit diagonal (does not touch the real middle elements)
// width and height values are assumed to be the "original" (non transposed) ones
procedure FMAMtxMultTria2T1(dest : PDouble; LineWidthDest : TASMNativeInt; mt1 : PDouble; LineWidth1 : TASMNativeInt; mt2 : PDouble; LineWidth2 : TASMNativeInt;
  width1, height1, width2, height2 : TASMNativeInt);

// mt1 = mt1*mt2'; where mt2 is an upper triangular matrix
procedure FMAMtxMultTria2T1StoreT1(mt1 : PDouble; LineWidth1 : TASMNativeInt; mt2 : PDouble; LineWidth2 : TASMNativeInt;
  width1, height1, width2, height2 : TASMNativeInt);

// W = C1*V1*T -> V1 is an upper triangular matrix with assumed unit diagonal entries. Operation on V1 transposition
procedure FMAMtxMultTria2TUpperUnit(dest : PDouble; LineWidthDest : TASMNativeInt; mt1 : PDouble; LineWidth1 : TASMNativeInt; mt2 : PDouble; LineWidth2 : TASMNativeInt;
  width1, height1, width2, height2 : TASMNativeInt);

// calculates mt1 = mt1*mt2', mt2 = lower triangular matrix. diagonal elements are assumed to be 1!
procedure FMAMtxMultLowTria2T2Store1(mt1 : PDouble; LineWidth1 : TASMNativeInt; mt2 : PDouble; LineWidth2 : TASMNativeInt;
  width1, height1, width2, height2 : TASMNativeInt);

{$ENDIF}

implementation

{$IFDEF FPC} {$ASMMODE intel} {$S-} {$ENDIF}

{$IFNDEF x64}

procedure FMAMatrixMultAligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1, height1, width2, height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var bytesWidth2, destOffset : TASMNativeInt;
    iter : TASMNativeInt;
begin
asm
   // prolog - simulate stack
   push ebx;
   push edi;
   push esi;

   mov ecx, dest;

   mov edi, width1;
   imul edi, -8;
   mov iter, edi;

   sub mt1, edi;

   //destOffset := destLineWidth - Width2*sizeof(double);
   mov ebx, Width2;
   shl ebx, 3;
   mov eax, destLineWidth;
   sub eax, ebx;
   mov destOffset, eax;

   //bytesWidth2 := width2*sizeof(double);
   mov bytesWidth2, ebx;

   // for y := 0 to height1 - 1 do
   @@foryloop:

      // r12 -> counter to width2
      mov esi, width2;
      sub esi, 2;
      jl @LastXColumn;

      @@forxloop:
      // for x := 0 to width2 div 2 - 1
          // esi: mt1 - width1*sizeof(double)
          // mt2: mt2
          mov edx, mt1;
          mov ebx, mt2;
          mov eax, iter;
          mov edi, LineWidth2;

          {$IFDEF FPC}vxorpd ymm0, ymm0, ymm0;{$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} 
          {$IFDEF FPC}vxorpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$57,$C9;{$ENDIF} 

          cmp eax, -32;
          jg @@Innerloop2Begin;

          // for z := 0 to width1 - 1do
          // FMA part:
          @@InnerLoop1:
             // 4x4 block
             {$IFDEF FPC}vmovapd xmm2, [ebx];{$ELSE}db $C5,$F9,$28,$13;{$ENDIF} 
             add ebx, edi;
             {$IFDEF FPC}vmovapd xmm4, xmm2;{$ELSE}db $C5,$F9,$28,$E2;{$ENDIF} 

             {$IFDEF FPC}vmovapd xmm3, [ebx];{$ELSE}db $C5,$F9,$28,$1B;{$ENDIF} 
             add ebx, edi;

             // shuffle so we can multiply

             // swap such that we can immediately multiply
             {$IFDEF FPC}vmovlhps xmm2, xmm2, xmm3;{$ELSE}db $C5,$E8,$16,$D3;{$ENDIF} 
             {$IFDEF FPC}vmovhlps xmm3, xmm3, xmm4;{$ELSE}db $C5,$E0,$12,$DC;{$ENDIF} 

             // next 4 elements
             {$IFDEF FPC}vmovapd xmm4, [ebx];{$ELSE}db $C5,$F9,$28,$23;{$ENDIF} 
             add ebx, edi;
             {$IFDEF FPC}vmovapd xmm6, xmm4;{$ELSE}db $C5,$F9,$28,$F4;{$ENDIF} 

             {$IFDEF FPC}vmovapd xmm5, [ebx];{$ELSE}db $C5,$F9,$28,$2B;{$ENDIF} 
             add ebx, edi;

             {$IFDEF FPC}vmovapd ymm7, [edx + eax]{$ELSE}db $C5,$FD,$28,$3C,$02;{$ENDIF} 

             {$IFDEF FPC}vmovlhps xmm4, xmm4, xmm5;{$ELSE}db $C5,$D8,$16,$E5;{$ENDIF} 
             {$IFDEF FPC}vmovhlps xmm5, xmm5, xmm6;{$ELSE}db $C5,$D0,$12,$EE;{$ENDIF} 

             {$IFDEF FPC}vinsertf128 ymm2, ymm2, xmm4, 1;{$ELSE}db $C4,$E3,$6D,$18,$D4,$01;{$ENDIF} 
             {$IFDEF FPC}vinsertf128 ymm3, ymm3, xmm5, 1;{$ELSE}db $C4,$E3,$65,$18,$DD,$01;{$ENDIF} 

             // now multiply and add
             vfmadd231pd ymm0, ymm2, ymm7;
             vfmadd231pd ymm1, ymm3, ymm7;
          add eax, 32;
          jl @@InnerLoop1;

          {$IFDEF FPC}vextractf128 xmm2, ymm0, 1;{$ELSE}db $C4,$E3,$7D,$19,$C2,$01;{$ENDIF} 
          {$IFDEF FPC}vextractf128 xmm3, ymm1, 1;{$ELSE}db $C4,$E3,$7D,$19,$CB,$01;{$ENDIF} 

          {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm2;{$ELSE}db $C5,$F9,$7C,$C2;{$ENDIF} 
          {$IFDEF FPC}vhaddpd xmm1, xmm1, xmm3;{$ELSE}db $C5,$F1,$7C,$CB;{$ENDIF} 

          test eax, eax;
          jz @@InnerLoopEnd2;

          @@Innerloop2Begin:

          // rest in single elements
          @@InnerLoop2:
             {$IFDEF FPC}vmovapd xmm2, [ebx];{$ELSE}db $C5,$F9,$28,$13;{$ENDIF} 
             add ebx, edi;

             {$IFDEF FPC}vmovddup xmm3, [edx + eax];{$ELSE}db $C5,$FB,$12,$1C,$02;{$ENDIF} 

             {$IFDEF FPC}vmulpd xmm2, xmm2, xmm3;{$ELSE}db $C5,$E9,$59,$D3;{$ENDIF} 
             {$IFDEF FPC}vmovhlps xmm4, xmm4, xmm2;{$ELSE}db $C5,$D8,$12,$E2;{$ENDIF} 

             {$IFDEF FPC}vaddsd xmm0, xmm0, xmm2;{$ELSE}db $C5,$FB,$58,$C2;{$ENDIF} 
             {$IFDEF FPC}vaddsd xmm1, xmm1, xmm4;{$ELSE}db $C5,$F3,$58,$CC;{$ENDIF} 
          add eax, 8;
          jnz @@InnerLoop2;

          @@InnerLoopEnd2:

          // finall horizontal addition
          {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm1;{$ELSE}db $C5,$F9,$7C,$C1;{$ENDIF} 

          {$IFDEF FPC}vmovapd [ecx], xmm0;{$ELSE}db $C5,$F9,$29,$01;{$ENDIF} 

          // increment the pointers
          // inc(mt2), inc(dest);
          //add dword ptr [mt2], 8;
          add mt2, 16;
          add ecx, 16;

      // end for x := 0 to width2 div 2 - 1
      sub esi, 2;
      jge @@forxloop;

      @LastXColumn:

      cmp esi, -1;
      jne @NextLine;

      // last column of mt2
      mov eax, iter;
      mov ebx, mt2;

      {$IFDEF FPC}vxorpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$57,$C0;{$ENDIF} 

      @InnerLoop2:
         {$IFDEF FPC}vmovsd xmm1, [edx + eax];{$ELSE}db $C5,$FB,$10,$0C,$02;{$ENDIF} 
         {$IFDEF FPC}vmovsd xmm2, [ebx];{$ELSE}db $C5,$FB,$10,$13;{$ENDIF} 

         {$IFDEF FPC}vmulsd xmm1, xmm1, xmm2;{$ELSE}db $C5,$F3,$59,$CA;{$ENDIF} 
         {$IFDEF FPC}vaddsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$58,$C1;{$ENDIF} 

         add ebx, edi;
      add eax, 8;
      jnz @InnerLoop2;

      {$IFDEF FPC}vmovsd [ecx], xmm0;{$ELSE}db $C5,$FB,$11,$01;{$ENDIF} 
      add ecx, 8;
      add mt2, 8;

      @NextLine:
      // dec(mt2, Width2);
      // inc(PByte(mt1), LineWidth1);
      // inc(PByte(dest), destOffset);
      //mov ebx, bytesWidth2;
      //sub dword ptr [mt2], ebx;
      mov eax, bytesWidth2;
      sub mt2, eax;
      mov eax, LineWidth1;
      add mt1, eax;
      add ecx, destOffset;

   // end for y := 0 to height1 - 1
   //dec eax;
   dec height1;
   jnz @@foryloop;

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;
end;

procedure FMAMatrixMultUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1, height1, width2, height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var bytesWidth2, destOffset : TASMNativeInt;
    iter : TASMNativeInt;
begin
asm
   // prolog - simulate stack
   push ebx;
   push edi;
   push esi;

   mov ecx, dest;

   mov edi, width1;
   imul edi, -8;
   mov iter, edi;

   sub mt1, edi;

   //destOffset := destLineWidth - Width2*sizeof(double);
   mov ebx, Width2;
   shl ebx, 3;
   mov eax, destLineWidth;
   sub eax, ebx;
   mov destOffset, eax;

   //bytesWidth2 := width2*sizeof(double);
   mov bytesWidth2, ebx;

   // for y := 0 to height1 - 1 do
   @@foryloop:

      // esi -> counter to width2
      mov esi, width2;
      sub esi, 2;
      jl @LastXColumn;

      @@forxloop:
      // for x := 0 to width2 div 2 - 1
          // esi: mt1 - width1*sizeof(double)
          // mt2: mt2
          mov edx, mt1;
          mov ebx, mt2;
          mov eax, iter;
          mov edi, LineWidth2;

          {$IFDEF FPC}vxorpd ymm0, ymm0, ymm0;{$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} 
          {$IFDEF FPC}vxorpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$57,$C9;{$ENDIF} 

          cmp eax, -32;
          jg @@Innerloop2Begin;

          // for z := 0 to width1 - 1do
          // FMA part:
          @@InnerLoop1:
             // 4x4 block
             {$IFDEF FPC}vmovupd xmm2, [ebx];{$ELSE}db $C5,$F9,$10,$13;{$ENDIF} 
             add ebx, edi;
             {$IFDEF FPC}vmovupd xmm4, xmm2;{$ELSE}db $C5,$F9,$10,$E2;{$ENDIF} 

             {$IFDEF FPC}vmovupd xmm3, [ebx];{$ELSE}db $C5,$F9,$10,$1B;{$ENDIF} 
             add ebx, edi;

             // shuffle so we can multiply

             // swap such that we can immediately multiply
             {$IFDEF FPC}vmovlhps xmm2, xmm2, xmm3;{$ELSE}db $C5,$E8,$16,$D3;{$ENDIF} 
             {$IFDEF FPC}vmovhlps xmm3, xmm3, xmm4;{$ELSE}db $C5,$E0,$12,$DC;{$ENDIF} 

             // next 4 elements
             {$IFDEF FPC}vmovupd xmm4, [ebx];{$ELSE}db $C5,$F9,$10,$23;{$ENDIF} 
             add ebx, edi;
             {$IFDEF FPC}vmovupd xmm6, xmm4;{$ELSE}db $C5,$F9,$10,$F4;{$ENDIF} 

             {$IFDEF FPC}vmovupd xmm5, [ebx];{$ELSE}db $C5,$F9,$10,$2B;{$ENDIF} 
             add ebx, edi;

             {$IFDEF FPC}vmovupd ymm7, [edx + eax]{$ELSE}db $C5,$FD,$10,$3C,$02;{$ENDIF} 

             {$IFDEF FPC}vmovlhps xmm4, xmm4, xmm5;{$ELSE}db $C5,$D8,$16,$E5;{$ENDIF} 
             {$IFDEF FPC}vmovhlps xmm5, xmm5, xmm6;{$ELSE}db $C5,$D0,$12,$EE;{$ENDIF} 

             {$IFDEF FPC}vinsertf128 ymm2, ymm2, xmm4, 1;{$ELSE}db $C4,$E3,$6D,$18,$D4,$01;{$ENDIF} 
             {$IFDEF FPC}vinsertf128 ymm3, ymm3, xmm5, 1;{$ELSE}db $C4,$E3,$65,$18,$DD,$01;{$ENDIF} 

             // now multiply and add
             vfmadd231pd ymm0, ymm2, ymm7;
             vfmadd231pd ymm1, ymm3, ymm7;

          add eax, 32;
          jl @@InnerLoop1;

          {$IFDEF FPC}vextractf128 xmm2, ymm0, 1;{$ELSE}db $C4,$E3,$7D,$19,$C2,$01;{$ENDIF} 
          {$IFDEF FPC}vextractf128 xmm3, ymm1, 1;{$ELSE}db $C4,$E3,$7D,$19,$CB,$01;{$ENDIF} 

          {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm2;{$ELSE}db $C5,$F9,$7C,$C2;{$ENDIF} 
          {$IFDEF FPC}vhaddpd xmm1, xmm1, xmm3;{$ELSE}db $C5,$F1,$7C,$CB;{$ENDIF} 

          test eax, eax;
          jz @@InnerLoopEnd2;

          @@Innerloop2Begin:

          // rest in single elements
          @@InnerLoop2:
             {$IFDEF FPC}vmovupd xmm2, [ebx];{$ELSE}db $C5,$F9,$10,$13;{$ENDIF} 
             add ebx, edi;

             {$IFDEF FPC}vmovddup xmm3, [edx + eax];{$ELSE}db $C5,$FB,$12,$1C,$02;{$ENDIF} 

             {$IFDEF FPC}vmulpd xmm2, xmm2, xmm3;{$ELSE}db $C5,$E9,$59,$D3;{$ENDIF} 
             {$IFDEF FPC}vmovhlps xmm4, xmm4, xmm2;{$ELSE}db $C5,$D8,$12,$E2;{$ENDIF} 

             {$IFDEF FPC}vaddsd xmm0, xmm0, xmm2;{$ELSE}db $C5,$FB,$58,$C2;{$ENDIF} 
             {$IFDEF FPC}vaddsd xmm1, xmm1, xmm4;{$ELSE}db $C5,$F3,$58,$CC;{$ENDIF} 
          add eax, 8;
          jnz @@InnerLoop2;

          @@InnerLoopEnd2:

          // finall horizontal addition
          {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm1;{$ELSE}db $C5,$F9,$7C,$C1;{$ENDIF} 

          {$IFDEF FPC}vmovupd [ecx], xmm0;{$ELSE}db $C5,$F9,$11,$01;{$ENDIF} 

          // increment the pointers
          // inc(mt2), inc(dest);
          //add dword ptr [mt2], 8;
          add mt2, 16;
          add ecx, 16;

      // end for x := 0 to width2 div 2 - 1
      sub esi, 2;
      jge @@forxloop;

      @LastXColumn:

      cmp esi, -1;
      jne @NextLine;

      // last column of mt2
      mov eax, iter;
      mov ebx, mt2;

      {$IFDEF FPC}vxorpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$57,$C0;{$ENDIF} 

      @InnerLoop2:
         {$IFDEF FPC}vmovsd xmm1, [edx + eax];{$ELSE}db $C5,$FB,$10,$0C,$02;{$ENDIF} 
         {$IFDEF FPC}vmovsd xmm2, [ebx];{$ELSE}db $C5,$FB,$10,$13;{$ENDIF} 

         {$IFDEF FPC}vmulsd xmm1, xmm1, xmm2;{$ELSE}db $C5,$F3,$59,$CA;{$ENDIF} 
         {$IFDEF FPC}vaddsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$58,$C1;{$ENDIF} 

         add ebx, edi;
      add eax, 8;
      jnz @InnerLoop2;

      {$IFDEF FPC}vmovsd [ecx], xmm0;{$ELSE}db $C5,$FB,$11,$01;{$ENDIF} 
      add ecx, 8;
      add mt2, 8;

      @NextLine:
      // dec(mt2, Width2);
      // inc(PByte(mt1), LineWidth1);
      // inc(PByte(dest), destOffset);
      //mov ebx, bytesWidth2;
      //sub dword ptr [mt2], ebx;
      mov eax, bytesWidth2;
      sub mt2, eax;
      mov eax, LineWidth1;
      add mt1, eax;
      add ecx, destOffset;

   // end for y := 0 to height1 - 1
   //dec eax;
   dec height1;
   jnz @@foryloop;

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;
end;


// ###########################################
// #### Special multiplication routines (for now only used in QR Decomposition)
// ###########################################

procedure FMAMtxMultTria2T1StoreT1(mt1 : PDouble; LineWidth1 : TASMNativeInt; mt2 : PDouble; LineWidth2 : TASMNativeInt;
  width1, height1, width2, height2 : TASMNativeInt);
var iter : TASMNativeInt;
    testExitLoopVal : TASMNativeInt;
begin
asm
   // prolog: stack
   push ebx;
   push edi;
   push esi;

   //iter := -width1*sizeof(double);
   mov eax, width1;
   imul eax, -8;
   mov iter, eax;

   // testExitLoopVal := height2*sizeof(double) + iter;
   mov edi, height2;
   shl edi, 3; //*8
   add edi, eax;
   mov testExitLoopVal, edi;

   // ecx := mt1
   mov ecx, mt1;
   sub ecx, eax;  // mt1 - iter


   // for y loop
   @@foryloop:
      mov ebx, mt2;
      sub ebx, iter;
      mov esi, iter;

      @@forxloop:
         {$IFDEF FPC}vxorpd ymm0, ymm0, ymm0; {$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} // temp := 0

         mov eax, esi;  // loop counter x;

         // test if height2 > width1 and loop counter > width1
         mov edx, eax;
         test edx, edx;
         jge @@foriloopend;

         // in case x is odd -> handle the first element separately
         and edx, $E;
         jz @@foriloopFMA;

         // single element handling
         {$IFDEF FPC}vmovsd xmm1, [ecx + eax];{$ELSE}db $C5,$FB,$10,$0C,$01;{$ENDIF} 
         {$IFDEF FPC}vmovsd xmm2, [ebx + eax];{$ELSE}db $C5,$FB,$10,$14,$03;{$ENDIF} 
         {$IFDEF FPC}vmulsd xmm1, xmm1, xmm2;{$ELSE}db $C5,$F3,$59,$CA;{$ENDIF} 
         {$IFDEF FPC}vaddsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$58,$C1;{$ENDIF} 
         add eax, 8;

         @@foriloopFMA:
            // 4 elements at a time
            add eax, 32;
            jg @@foriloopFMAend;

            {$IFDEF FPC}vmovupd ymm1, [ecx + eax - 32];{$ELSE}db $C5,$FD,$10,$4C,$01,$E0;{$ENDIF} 
            {$IFDEF FPC}vmovupd ymm2, [ebx + eax - 32];{$ELSE}db $C5,$FD,$10,$54,$03,$E0;{$ENDIF}
            vfmadd231pd ymm0, ymm1, ymm2;

         jmp @@foriloopFMA;

         @@foriloopFMAend:

         {$IFDEF FPC}vextractf128 xmm2, ymm0, 1;{$ELSE}db $C4,$E3,$7D,$19,$C2,$01;{$ENDIF} 
         {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm2;{$ELSE}db $C5,$F9,$7C,$C2;{$ENDIF} 

         sub eax, 32;
         jz @@foriloopend;

         // for i := x to width1 - 1
         @@foriloop:
            // two elements at a time:
            {$IFDEF FPC}vmovupd xmm1, [ecx + eax];{$ELSE}db $C5,$F9,$10,$0C,$01;{$ENDIF} 
            {$IFDEF FPC}vmovupd xmm2, [ebx + eax];{$ELSE}db $C5,$F9,$10,$14,$03;{$ENDIF} 
            vfmadd231pd xmm0, xmm1, xmm2;

            add eax, 16;
         jnz @@foriloop;

         @@foriloopend:

         // final result
         {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} 
         {$IFDEF FPC}vmovsd [ecx + esi], xmm0;{$ELSE}db $C5,$FB,$11,$04,$31;{$ENDIF} 

         add ebx, LineWidth1;
         add esi, 8;

      cmp esi, testExitLoopVal;
      jne @@forxloop;

      add ecx, LineWidth1;
   dec height1;
   jnz @@foryloop;

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;
end;


procedure FMAMtxMultTria2TUpperUnit(dest : PDouble; LineWidthDest : TASMNativeInt; mt1 : PDouble; LineWidth1 : TASMNativeInt; mt2 : PDouble; LineWidth2 : TASMNativeInt;
  width1, height1, width2, height2 : TASMNativeInt);
var iter, testExitLoopVal : TASMNativeInt;
begin
asm
   // prolog: stack
   push ebx;
   push edi;
   push esi;

   //iter := -width1*sizeof(double);
   mov eax, width1;
   imul eax, -8;
   mov iter, eax;

   sub mt2, eax;

   // edx := mt1
   mov ecx, dest;
   sub ecx, eax;

   mov edx, mt1;
   sub edx, eax;  // mt1 - iter

   // testExitLoopVal := height2*sizeof(double) + iter;
   mov eax, height2;
   shl eax, 3; //*8
   add eax, iter;
   mov testExitLoopVal, eax;

   // for y loop
   @@foryloop:
      mov ebx, mt2;
      //sub ebx, iter;
      mov esi, iter;

      @@forxloop:
         {$IFDEF FPC}vxorpd ymm0, ymm0, ymm0; {$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} // temp := 0

         mov eax, esi;  // loop counter x;

         // test if height2 > width1 and loop counter > width1
         mov edi, eax;
         test edi, edi;
         jge @@foriloopend;

         // in case x is odd -> handle the first element separately
         and edi, $E;
         jz @@foriloopinit;

         // single element handling -> mt1 first element is assumed unit!
         {$IFDEF FPC}vmovsd xmm0, [edx + eax];{$ELSE}db $C5,$FB,$10,$04,$02;{$ENDIF} 
         add eax, 8;

         jmp @@AfterLoopInit;

         @@foriloopinit:

         test eax, eax;
         jz @@foriloopend;

         // two elements init at a time:
         {$IFDEF FPC}vmovsd xmm0, [edx + eax];{$ELSE}db $C5,$FB,$10,$04,$02;{$ENDIF} 
         {$IFDEF FPC}vmovsd xmm1, [edx + eax + 8];{$ELSE}db $C5,$FB,$10,$4C,$02,$08;{$ENDIF} 
         {$IFDEF FPC}vmovsd xmm2, [ebx + eax + 8];{$ELSE}db $C5,$FB,$10,$54,$03,$08;{$ENDIF} 
         {$IFDEF FPC}vmulsd xmm1, xmm1, xmm2;{$ELSE}db $C5,$F3,$59,$CA;{$ENDIF} 
         {$IFDEF FPC}vaddsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$58,$C1;{$ENDIF} 

         add eax, 16;

         @@AfterLoopInit:

         // in case the last single x element was handled we do not need further looping
         test eax, eax;
         jz @@finalizeloop;

         // for i := x to width1 - 1
         @@foriloop:
             add eax, 32;

             jg @@foriloopend;

             // 4 elements at a time:
             {$IFDEF FPC}vmovupd ymm1, [edx + eax - 32];{$ELSE}db $C5,$FD,$10,$4C,$02,$E0;{$ENDIF} 
             {$IFDEF FPC}vmovupd ymm2, [ebx + eax - 32];{$ELSE}db $C5,$FD,$10,$54,$03,$E0;{$ENDIF} 
             vfmadd231pd ymm0, ymm1, ymm2;
         jmp @@foriloop;

         @@foriloopend:
         {$IFDEF FPC}vextractf128 xmm1, ymm0, 1;{$ELSE}db $C4,$E3,$7D,$19,$C1,$01;{$ENDIF} 
         {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm1;{$ELSE}db $C5,$F9,$7C,$C1;{$ENDIF} 
         sub eax, 32;

         // test if we missed 2 elements
         jz @@finalizeloop;

         // need to process two more elements:
         {$IFDEF FPC}vmovupd xmm1, [edx + eax];{$ELSE}db $C5,$F9,$10,$0C,$02;{$ENDIF} 
         {$IFDEF FPC}vmovupd xmm2, [ebx + eax];{$ELSE}db $C5,$F9,$10,$14,$03;{$ENDIF} 
         vfmadd231pd xmm0, xmm1, xmm2;

         @@finalizeloop:

         // final result
         {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} 
         {$IFDEF FPC}vmovsd [ecx + esi], xmm0;{$ELSE}db $C5,$FB,$11,$04,$31;{$ENDIF} 

         add ebx, LineWidth2;
         add esi, 8;

      cmp esi, testExitLoopVal;
      jne @@forxloop;

      add edx, LineWidth1;
      add ecx, LineWidthDest;
   dec height1;
   jnz @@foryloop;

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;
end;

// note the result is stored in mt2 again!
// dest = mt1'*mt2; where mt2 is a lower triangular matrix and the operation is transposition
// the function assumes a unit diagonal (does not touch the real middle elements)
// width and height values are assumed to be the "original" (non transposed) ones
procedure FMAMtxMultTria2T1(dest : PDouble; LineWidthDest : TASMNativeInt; mt1 : PDouble; LineWidth1 : TASMNativeInt;
  mt2 : PDouble; LineWidth2 : TASMNativeInt;
  width1, height1, width2, height2 : TASMNativeInt);
var pMt2 : PDouble;
    width2D2 : TASMNativeInt;
begin
asm
   // Prolog
   push ebx;
   push esi;
   push edi;

   // width2D2 := width2 div 2;
   mov eax, width2;
   shr eax, 1;
   mov width2D2, eax;

   // for x := 0 to width1 - 1 do
   mov eax, width1;

   @@forxloop:

      // pMT2 := mt2;
      // pDest := dest;
      mov ebx, mt2;
      mov pMT2, ebx;

      mov edx, dest;   // edx is pDest


      // for y := 0 to width2D2 - 1 do
      mov ecx, width2D2;
      test ecx, ecx;
      jz @@foryloopend;

      xor ecx, ecx;
      @@foryloop:
           // valCounter1 := PConstDoubleArr(mt1);
           // inc(PByte(valCounter1), 2*y*LineWidth1);
           mov esi, mt1;
           mov ebx, ecx;
           add ebx, ebx;
           imul ebx, LineWidth1;
           add esi, ebx;

           // valCounter2 := PConstDoubleArr(pMT2);
           // inc(PByte(valCounter2), (2*y + 1)*LineWidth2);
           mov edi, pMt2;
           mov ebx, ecx;
           add ebx, ebx;
           imul ebx, LineWidth2;
           add ebx, LineWidth2;
           add edi, ebx;

           // tmp[0] := valCounter1^[0];
           // inc(PByte(valCounter1), LineWidth1);
           {$IFDEF FPC}vmovsd xmm0, [esi];{$ELSE}db $C5,$FB,$10,$06;{$ENDIF} 
           add esi, LineWidth1;

           // if height2 - 2*y - 1 > 0 then
           mov ebx, ecx;
           add ebx, ebx;
           inc ebx;

           cmp ebx, height2;
           jnl @@PreInnerLoop;
               // tmp[0] := tmp[0] + valCounter1^[0]*valCounter2^[0];
               // tmp[1] := valCounter1^[0];
               {$IFDEF FPC}vmovsd xmm1, [esi];{$ELSE}db $C5,$FB,$10,$0E;{$ENDIF} 
               {$IFDEF FPC}vmovlhps xmm0, xmm0, xmm1;{$ELSE}db $C5,$F8,$16,$C1;{$ENDIF} 

               {$IFDEF FPC}vmulsd xmm1, xmm1, [edi];{$ELSE}db $C5,$F3,$59,$0F;{$ENDIF} 
               {$IFDEF FPC}vaddsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$58,$C1;{$ENDIF} 

               //inc(PByte(valCounter1), LineWidth1);
               //inc(PByte(valCounter2), LineWidth2);

               add esi, LineWidth1;
               add edi, LineWidth2;

           @@PreInnerLoop:

           // rest is a double column!

           // prepare loop
           mov ebx, height2;
           sub ebx, ecx;
           sub ebx, ecx;
           sub ebx, 2;

           test ebx, ebx;
           jle @@InnerLoopEnd;

           @@InnerLoop:
               // tmp[0] := tmp[0] + valCounter1^[0]*valCounter2^[0];
               // tmp[1] := tmp[1] + valCounter1^[0]*valCounter2^[1];
               {$IFDEF FPC}vmovddup xmm1, [esi];{$ELSE}db $C5,$FB,$12,$0E;{$ENDIF} 
               {$IFDEF FPC}vmovupd xmm2, [edi];{$ELSE}db $C5,$F9,$10,$17;{$ENDIF} 

               vfmadd231pd xmm0, xmm1, xmm2;

               //inc(PByte(valCounter1), LineWidth1);
               //inc(PByte(valCounter2), LineWidth2);

               add esi, LineWidth1;
               add edi, LineWidth2;

           dec ebx;
           jnz @@InnerLoop;

           @@InnerLoopEnd:


           // write back result

           // pDest^ := tmp[0];
           // PDouble(TASMNativeUInt(pDest) + sizeof(double))^ := tmp[1];

           {$IFDEF FPC}vmovupd [edx], xmm0;{$ELSE}db $C5,$F9,$11,$02;{$ENDIF} 

           // inc(pDest, 2);
           // inc(pMT2, 2);
           add edx, 16;
           add pMT2, 16;

      // end foryloop
      inc ecx;
      cmp ecx, width2D2;
      jne @@foryloop;

      @@foryloopend:


      //if (width2 and $01) = 1 then
      mov ecx, width2;
      and ecx, 1;

      jz @@ifend1;

      // special handling of last column (just copy the value)

      // valCounter1 := PConstDoubleArr(mt1);
      mov ecx, mt1;

      //inc(PByte(valCounter1), LineWidth1*(height1 - 1));
      mov ebx, height1;
      dec ebx;
      imul ebx, LineWidth1;

      // pDest^ := valCounter1^[0];
      {$IFDEF FPC}vmovsd xmm0, [ecx + ebx];{$ELSE}db $C5,$FB,$10,$04,$19;{$ENDIF} 
      {$IFDEF FPC}vmovsd [edx], xmm0;{$ELSE}db $C5,$FB,$11,$02;{$ENDIF} 
      @@ifend1:


      //inc(mt1);
      //inc(PByte(dest), LineWidthDest);
      add mt1, 8;
      mov ebx, LineWidthDest;
      add dest, ebx;

   // end for loop
   dec eax;
   jnz @@forxloop;

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop edi;
   pop esi;
   pop ebx;
end;
end;

// calculates mt1 = mt1*mt2', mt2 = lower triangular matrix. diagonal elements are assumed to be 1!
procedure FMAMtxMultLowTria2T2Store1(mt1 : PDouble; LineWidth1 : TASMNativeInt; mt2 : PDouble; LineWidth2 : TASMNativeInt;
  width1, height1, width2, height2 : TASMNativeInt);
begin
asm
   // prolog - stack
   push ebx;
   push edi;
   push esi;

   // iter := -(width2 - 1)*sizeof(double);
   mov ebx, width2;
   dec ebx;
   imul ebx, -8;

   // start from bottom
   // r8: mt2
   // inc(PByte(mt2),(height2 - 1)*LineWidth2);
   mov eax, height2;
   dec eax;
   imul eax, LineWidth2;
   sub eax, ebx;
   mov edx, mt2;
   add edx, eax;

   // for x := 0 to width2 - 2
   dec width2;
   jz @@endproc;
   @@forxloop:
      mov eax, mt1;
      sub eax, ebx;

      // for y := 0 to height1 - 1
      mov esi, height1;
      @@foryloop:
         {$IFDEF FPC}vxorpd ymm0, ymm0, ymm0;{$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} 
         // for idx := 0 to width2 - x - 2
         mov edi, ebx;
         test edi, edi;
         jz @@foridxloopend;

         // unrolled loop 4x2
         add edi, 64;
         jg @@foridxloopSSE;
         @@foridxlongloop:
            {$IFDEF FPC}vmovupd ymm1, [eax + edi - 64];{$ELSE}db $C5,$FD,$10,$4C,$38,$C0;{$ENDIF} 
            {$IFDEF FPC}vmovupd ymm2, [edx + edi - 64];{$ELSE}db $C5,$FD,$10,$54,$3A,$C0;{$ENDIF} 
            vfmadd231pd ymm0, ymm1, ymm2;

            {$IFDEF FPC}vmovupd ymm1, [eax + edi - 32];{$ELSE}db $C5,$FD,$10,$4C,$38,$E0;{$ENDIF} 
            {$IFDEF FPC}vmovupd ymm2, [edx + edi - 32];{$ELSE}db $C5,$FD,$10,$54,$3A,$E0;{$ENDIF} 
            vfmadd231pd ymm0, ymm1, ymm2;
         add edi, 64;
         jl @@foridxlongloop;

         {$IFDEF FPC}vextractf128 xmm1, ymm0, 1;{$ELSE}db $C4,$E3,$7D,$19,$C1,$01;{$ENDIF} 
         {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm1;{$ELSE}db $C5,$F9,$7C,$C1;{$ENDIF} 

         // sse part
         @@foridxloopSSE:
         sub edi, 48;
         jg @@foridxloopstart;

         @@foridxSSEloop:
            {$IFDEF FPC}vmovupd xmm1, [eax + edi - 16];{$ELSE}db $C5,$F9,$10,$4C,$38,$F0;{$ENDIF} 
            {$IFDEF FPC}vmovupd xmm2, [edx + edi - 16];{$ELSE}db $C5,$F9,$10,$54,$3A,$F0;{$ENDIF} 
            vfmadd231pd xmm0, xmm1, xmm2;
         add edi, 16;
         jl @@foridxSSEloop;

         @@foridxloopStart:
         {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} 
         sub edi, 16;
         jz @@foridxloopend;

         @@foridxloop:
            {$IFDEF FPC}vmovsd xmm1, [eax + edi];{$ELSE}db $C5,$FB,$10,$0C,$38;{$ENDIF} 
            {$IFDEF FPC}vmovsd xmm2, [edx + edi];{$ELSE}db $C5,$FB,$10,$14,$3A;{$ENDIF} 
            {$IFDEF FPC}vmulsd xmm1, xmm1, xmm2;{$ELSE}db $C5,$F3,$59,$CA;{$ENDIF} 
            {$IFDEF FPC}vaddsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$58,$C1;{$ENDIF} 
         add edi, 8;
         jnz @@foridxloop;

         @@foridxloopend:

         // last element is unit:
         {$IFDEF FPC}vmovsd xmm1, [eax];{$ELSE}db $C5,$FB,$10,$08;{$ENDIF} 
         {$IFDEF FPC}vaddsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$58,$C1;{$ENDIF} 

         // write back
         // PConstDoubleArr(pMt1)^[width2 - x - 1] := tmp + valCounter1^[width2 - x - 1];
         {$IFDEF FPC}vmovsd [eax], xmm0;{$ELSE}db $C5,$FB,$11,$00;{$ENDIF} 
         add eax, LineWidth1;
      dec esi;
      jnz @@foryloop;

      // dec(PByte(mt2), LineWidth2);
      sub edx, LineWidth2;
      sub edx, 8;

      // adjust iterator to the next x value for the idxloop
      add ebx, 8;
   dec width2;
   jnz @@forxloop;

   @@endproc:

   // epilog: stack fixing
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 
   pop esi;
   pop edi;
   pop ebx;
end;
end;

{$ENDIF}

end.
