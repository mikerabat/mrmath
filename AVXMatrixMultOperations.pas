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


unit AVXMatrixMultOperations;

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
procedure AVXMatrixMultAligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1, height1, width2, height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
procedure AVXMatrixMultUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1, height1, width2, height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);


// some special types of multiplications used e.g. in QR Decomposition
// dest = mt1'*mt2; where mt2 is a lower triangular matrix and the operation is transposition
// the function assumes a unit diagonal (does not touch the real middle elements)
// width and height values are assumed to be the "original" (non transposed) ones
procedure AVXMtxMultTria2T1(dest : PDouble; LineWidthDest : TASMNativeInt; mt1 : PDouble; LineWidth1 : TASMNativeInt; mt2 : PDouble; LineWidth2 : TASMNativeInt;
  width1, height1, width2, height2 : TASMNativeInt);

// mt1 = mt1*mt2'; where mt2 is an upper triangular matrix
procedure AVXMtxMultTria2T1StoreT1(mt1 : PDouble; LineWidth1 : TASMNativeInt; mt2 : PDouble; LineWidth2 : TASMNativeInt;
  width1, height1, width2, height2 : TASMNativeInt);

// W = C1*V1*T -> V1 is an upper triangular matrix with assumed unit diagonal entries. Operation on V1 transposition
procedure AVXMtxMultTria2TUpperUnit(dest : PDouble; LineWidthDest : TASMNativeInt; mt1 : PDouble; LineWidth1 : TASMNativeInt; mt2 : PDouble; LineWidth2 : TASMNativeInt;
  width1, height1, width2, height2 : TASMNativeInt);

// mt1 = mt1*mt2; where mt2 is an upper triangular matrix
procedure AVXMtxMultTria2Store1(mt1 : PDouble; LineWidth1 : TASMNativeInt; mt2 : PDouble; LineWidth2 : TASMNativeInt;
  width1, height1, width2, height2 : TASMNativeInt);

// calculates mt1 = mt1*mt2', mt2 = lower triangular matrix. diagonal elements are assumed to be 1!
procedure AVXMtxMultLowTria2T2Store1(mt1 : PDouble; LineWidth1 : TASMNativeInt; mt2 : PDouble; LineWidth2 : TASMNativeInt;
  width1, height1, width2, height2 : TASMNativeInt);

// mt1 = mt1*mt2; where mt2 is an upper triangular matrix - diagonal elements are unit
procedure AVXMtxMultTria2Store1Unit(mt1 : PDouble; LineWidth1 : TASMNativeInt; mt2 : PDouble; LineWidth2 : TASMNativeInt;
  width1, height1, width2, height2 : TASMNativeInt);


{$ENDIF}

implementation

{$IFDEF FPC} {$ASMMODE intel} {$ENDIF}

{$IFNDEF x64}

procedure AVXMatrixMultAligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1, height1, width2, height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var bytesWidth2, destOffset : TASMNativeInt;
    iter : TASMNativeInt;
{$IFDEF FPC}
begin
{$ENDIF}
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

          vxorpd ymm0, ymm0, ymm0;
          vxorpd ymm1, ymm1, ymm1;

          cmp eax, -32;
          jg @@Innerloop2Begin;

          // for z := 0 to width1 - 1do
          // AVX part:
          @@InnerLoop1:
             // 4x4 block
             vmovapd xmm2, [ebx];
             add ebx, edi;
             vmovapd xmm4, xmm2;

             vmovapd xmm3, [ebx];
             add ebx, edi;

             // shuffle so we can multiply

             // swap such that we can immediately multiply
             vmovlhps xmm2, xmm2, xmm3;
             vmovhlps xmm3, xmm3, xmm4;

             // next 4 elements
             vmovapd xmm4, [ebx];
             add ebx, edi;
             vmovapd xmm6, xmm4;

             vmovapd xmm5, [ebx];
             add ebx, edi;

             vmovapd ymm7, [edx + eax]

             vmovlhps xmm4, xmm4, xmm5;
             vmovhlps xmm5, xmm5, xmm6;

             vinsertf128 ymm2, ymm2, xmm4, 1;
             vinsertf128 ymm3, ymm3, xmm5, 1;

             // now multiply and add
             vmulpd ymm2, ymm2, ymm7;
             vmulpd ymm3, ymm3, ymm7;

             vaddpd ymm0, ymm0, ymm2;
             vaddpd ymm1, ymm1, ymm3;
          add eax, 32;
          jl @@InnerLoop1;

          vextractf128 xmm2, ymm0, 1;
          vextractf128 xmm3, ymm1, 1;

          vhaddpd xmm0, xmm0, xmm2;
          vhaddpd xmm1, xmm1, xmm3;

          test eax, eax;
          jz @@InnerLoopEnd2;

          @@Innerloop2Begin:

          // rest in single elements
          @@InnerLoop2:
             vmovapd xmm2, [ebx];
             add ebx, edi;

             vmovddup xmm3, [edx + eax];

             vmulpd xmm2, xmm2, xmm3;
             vmovhlps xmm4, xmm4, xmm2;

             vaddsd xmm0, xmm0, xmm2;
             vaddsd xmm1, xmm1, xmm4;
          add eax, 8;
          jnz @@InnerLoop2;

          @@InnerLoopEnd2:

          // finall horizontal addition
          vhaddpd xmm0, xmm0, xmm1;

          vmovapd [ecx], xmm0;

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

      vxorpd xmm0, xmm0, xmm0;

      @InnerLoop2:
         vmovsd xmm1, [edx + eax];
         vmovsd xmm2, [ebx];

         vmulsd xmm1, xmm1, xmm2;
         vaddsd xmm0, xmm0, xmm1;

         add ebx, edi;
      add eax, 8;
      jnz @InnerLoop2;

      vmovsd [ecx], xmm0;
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
   vzeroupper;

   pop esi;
   pop edi;
   pop ebx;
end;
{$IFDEF FPC}
end;
{$ENDIF}

procedure AVXMatrixMultUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1, height1, width2, height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var bytesWidth2, destOffset : TASMNativeInt;
    iter : TASMNativeInt;
{$IFDEF FPC}
begin
{$ENDIF}
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

          vxorpd ymm0, ymm0, ymm0;
          vxorpd ymm1, ymm1, ymm1;

          cmp eax, -32;
          jg @@Innerloop2Begin;

          // for z := 0 to width1 - 1do
          // AVX part:
          @@InnerLoop1:
             // 4x4 block
             vmovupd xmm2, [ebx];
             add ebx, edi;
             vmovupd xmm4, xmm2;

             vmovupd xmm3, [ebx];
             add ebx, edi;

             // shuffle so we can multiply

             // swap such that we can immediately multiply
             vmovlhps xmm2, xmm2, xmm3;
             vmovhlps xmm3, xmm3, xmm4;

             // next 4 elements
             vmovupd xmm4, [ebx];
             add ebx, edi;
             vmovupd xmm6, xmm4;

             vmovupd xmm5, [ebx];
             add ebx, edi;

             vmovupd ymm7, [edx + eax]

             vmovlhps xmm4, xmm4, xmm5;
             vmovhlps xmm5, xmm5, xmm6;

             vinsertf128 ymm2, ymm2, xmm4, 1;
             vinsertf128 ymm3, ymm3, xmm5, 1;

             // now multiply and add
             vmulpd ymm2, ymm2, ymm7;
             vmulpd ymm3, ymm3, ymm7;

             vaddpd ymm0, ymm0, ymm2;
             vaddpd ymm1, ymm1, ymm3;
          add eax, 32;
          jl @@InnerLoop1;

          vextractf128 xmm2, ymm0, 1;
          vextractf128 xmm3, ymm1, 1;

          vhaddpd xmm0, xmm0, xmm2;
          vhaddpd xmm1, xmm1, xmm3;

          test eax, eax;
          jz @@InnerLoopEnd2;

          @@Innerloop2Begin:

          // rest in single elements
          @@InnerLoop2:
             vmovupd xmm2, [ebx];
             add ebx, edi;

             vmovddup xmm3, [edx + eax];

             vmulpd xmm2, xmm2, xmm3;
             vmovhlps xmm4, xmm4, xmm2;

             vaddsd xmm0, xmm0, xmm2;
             vaddsd xmm1, xmm1, xmm4;
          add eax, 8;
          jnz @@InnerLoop2;

          @@InnerLoopEnd2:

          // finall horizontal addition
          vhaddpd xmm0, xmm0, xmm1;

          vmovupd [ecx], xmm0;

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

      vxorpd xmm0, xmm0, xmm0;

      @InnerLoop2:
         vmovsd xmm1, [edx + eax];
         vmovsd xmm2, [ebx];

         vmulsd xmm1, xmm1, xmm2;
         vaddsd xmm0, xmm0, xmm1;

         add ebx, edi;
      add eax, 8;
      jnz @InnerLoop2;

      vmovsd [ecx], xmm0;
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
   vzeroupper;

   pop esi;
   pop edi;
   pop ebx;
end;
{$IFDEF FPC}
end;
{$ENDIF}


// ###########################################
// #### Special multiplication routines (for now only used in QR Decomposition)
// ###########################################

// note the result is stored in mt1 again!
// mt1 = mt1*mt2; where mt2 is an upper triangular matrix
procedure AVXMtxMultTria2Store1(mt1 : PDouble; LineWidth1 : TASMNativeInt; mt2 : PDouble; LineWidth2 : TASMNativeInt;
  width1, height1, width2, height2 : TASMNativeInt);
{$IFDEF FPC}
begin
{$ENDIF}
asm
   // prolog: stack
   push ebx;
   push edi;
   push esi;

   // iter
   mov ecx, width1;
   imul ecx, -8;
   // init

   // inc(mt2, width2 - 1);
   mov eax, width2;
   dec eax;
   shl eax, 3; //sizeof(double)
   add mt2, eax;

   // for x := 0 to width2 - 1 do
   @@forxloop:
      mov edi, height1;  // height1

      mov eax, mt1;
      sub eax, ecx;

      // for y := 0 to height1 - 1
      @@foryloop:
         // tmp := 0;
         vxorpd xmm0, xmm0, xmm0;

         // ebx, mt2
         mov ebx, mt2;

         // for idx := 0 to width1 - x - 1
         mov esi, ecx;

         // check if we have enough iterations:
         test esi, esi;
         jz @@foridxloopend;

         @@foridxloop:
            vmovsd xmm1, [ebx];
            vmovsd xmm2, [eax + esi]

            add ebx, LineWidth2;  // + linewidth2

            vmulsd xmm1, xmm1, xmm2;
            vaddsd xmm0, xmm0, xmm1;

         add esi, 8;
         jnz @@foridxloop;

         @@foridxloopend:

         // PConstDoubleArr(pmt1)^[width2 - 1 - x] := tmp;
         mov ebx, eax;
         add ebx, ecx;
         mov esi, width2;
         dec esi;
         vmovsd [ebx + 8*esi], xmm0;

         // inc(PByte(pmT1), LineWidth1);
         add eax, LineWidth1;

      dec edi;
      jnz @@foryloop;

      // reduce for idx loop
      add ecx, 8;
      // dec(mt2);
      sub mt2, 8;

   // dec width2
   dec width2;
   jnz @@forxloop;

   // cleanup stack
   vzeroupper;
   pop esi;
   pop edi;
   pop ebx;
end;
{$IFDEF FPC}
end;
{$ENDIF}

procedure AVXMtxMultTria2T1StoreT1(mt1 : PDouble; LineWidth1 : TASMNativeInt; mt2 : PDouble; LineWidth2 : TASMNativeInt;
  width1, height1, width2, height2 : TASMNativeInt);
var iter : TASMNativeInt;
    testExitLoopVal : TASMNativeInt;
{$IFDEF FPC}
begin
{$ENDIF}
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
         vxorpd ymm0, ymm0, ymm0; // temp := 0

         mov eax, esi;  // loop counter x;

         // test if height2 > width1 and loop counter > width1
         mov edx, eax;
         test edx, edx;
         jge @@foriloopend;

         // in case x is odd -> handle the first element separately
         and edx, $E;
         jz @@foriloopAVX;

         // single element handling
         vmovsd xmm1, [ecx + eax];
         vmovsd xmm2, [ebx + eax];
         vmulsd xmm1, xmm1, xmm2;
         vaddsd xmm0, xmm0, xmm1;
         add eax, 8;

         @@foriloopAVX:
            // 4 elements at a time
            add eax, 32;
            jg @@foriloopAVXend;

            vmovupd ymm1, [ecx + eax - 32];
            vmovupd ymm2, [ebx + eax - 32];
            vmulpd ymm1, ymm1, ymm2;
            vaddpd ymm0, ymm0, ymm1;

         jmp @@foriloopAVX;

         @@foriloopAVXend:

         vextractf128 xmm2, ymm0, 1;
         vhaddpd xmm0, xmm0, xmm2;

         sub eax, 32;
         jz @@foriloopend;

         // for i := x to width1 - 1
         @@foriloop:
            // two elements at a time:
            vmovupd xmm1, [ecx + eax];
            vmovupd xmm2, [ebx + eax];
            vmulpd xmm1, xmm1, xmm2;
            vaddpd xmm0, xmm0, xmm1;

            add eax, 16;
         jnz @@foriloop;

         @@foriloopend:

         // final result
         vhaddpd xmm0, xmm0, xmm0;
         vmovsd [ecx + esi], xmm0;

         add ebx, LineWidth1;
         add esi, 8;

      cmp esi, testExitLoopVal;
      jne @@forxloop;

      add ecx, LineWidth1;
   dec height1;
   jnz @@foryloop;

   // epilog
   vzeroupper;

   pop esi;
   pop edi;
   pop ebx;
end;
{$IFDEF FPC}
end;
{$ENDIF}


procedure AVXMtxMultTria2TUpperUnit(dest : PDouble; LineWidthDest : TASMNativeInt; mt1 : PDouble; LineWidth1 : TASMNativeInt; mt2 : PDouble; LineWidth2 : TASMNativeInt;
  width1, height1, width2, height2 : TASMNativeInt);
var iter, testExitLoopVal : TASMNativeInt;
{$IFDEF FPC}
begin
{$ENDIF}
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
         vxorpd ymm0, ymm0, ymm0; // temp := 0

         mov eax, esi;  // loop counter x;

         // test if height2 > width1 and loop counter > width1
         mov edi, eax;
         test edi, edi;
         jge @@foriloopend;

         // in case x is odd -> handle the first element separately
         and edi, $E;
         jz @@foriloopinit;

         // single element handling -> mt1 first element is assumed unit!
         vmovsd xmm0, [edx + eax];
         add eax, 8;

         jmp @@AfterLoopInit;

         @@foriloopinit:

         test eax, eax;
         jz @@foriloopend;

         // two elements init at a time:
         vmovsd xmm0, [edx + eax];
         vmovsd xmm1, [edx + eax + 8];
         vmovsd xmm2, [ebx + eax + 8];
         vmulsd xmm1, xmm1, xmm2;
         vaddsd xmm0, xmm0, xmm1;

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
             vmovupd ymm1, [edx + eax - 32];
             vmovupd ymm2, [ebx + eax - 32];
             vmulpd ymm1, ymm1, ymm2;
             vaddpd ymm0, ymm0, ymm1;
         jmp @@foriloop;

         @@foriloopend:
         vextractf128 xmm1, ymm0, 1;
         vhaddpd xmm0, xmm0, xmm1;
         sub eax, 32;

         // test if we missed 2 elements
         jz @@finalizeloop;

         // need to process two more elements:
         vmovupd xmm1, [edx + eax];
         vmovupd xmm2, [ebx + eax];
         vmulpd xmm1, xmm1, xmm2;
         vaddpd xmm0, xmm0, xmm1;

         @@finalizeloop:

         // final result
         vhaddpd xmm0, xmm0, xmm0;
         vmovsd [ecx + esi], xmm0;

         add ebx, LineWidth2;
         add esi, 8;

      cmp esi, testExitLoopVal;
      jne @@forxloop;

      add edx, LineWidth1;
      add ecx, LineWidthDest;
   dec height1;
   jnz @@foryloop;

   // epilog
   vzeroupper;

   pop esi;
   pop edi;
   pop ebx;
end;
{$IFDEF FPC}
end;
{$ENDIF}


// note the result is stored in mt2 again!
// dest = mt1'*mt2; where mt2 is a lower triangular matrix and the operation is transposition
// the function assumes a unit diagonal (does not touch the real middle elements)
// width and height values are assumed to be the "original" (non transposed) ones
procedure AVXMtxMultTria2T1(dest : PDouble; LineWidthDest : TASMNativeInt; mt1 : PDouble; LineWidth1 : TASMNativeInt;
  mt2 : PDouble; LineWidth2 : TASMNativeInt;
  width1, height1, width2, height2 : TASMNativeInt);
var pMt2 : PDouble;
    width2D2 : TASMNativeInt;
{$IFDEF FPC}
begin
{$ENDIF}
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
           vmovsd xmm0, [esi];
           add esi, LineWidth1;

           // if height2 - 2*y - 1 > 0 then
           mov ebx, ecx;
           add ebx, ebx;
           inc ebx;

           cmp ebx, height2;
           jnl @@PreInnerLoop;
               // tmp[0] := tmp[0] + valCounter1^[0]*valCounter2^[0];
               // tmp[1] := valCounter1^[0];
               vmovsd xmm1, [esi];
               vmovlhps xmm0, xmm0, xmm1;

               vmulsd xmm1, xmm1, [edi];
               vaddsd xmm0, xmm0, xmm1;

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
               vmovddup xmm1, [esi];
               vmovupd xmm2, [edi];

               vmulpd xmm2, xmm2, xmm1;
               vaddpd xmm0, xmm0, xmm2;

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

           vmovupd [edx], xmm0;

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
      vmovsd xmm0, [ecx + ebx];
      vmovsd [edx], xmm0;
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
   vzeroupper;

   pop edi;
   pop esi;
   pop ebx;
end;
{$IFDEF FPC}
end;
{$ENDIF}

// calculates mt1 = mt1*mt2', mt2 = lower triangular matrix. diagonal elements are assumed to be 1!
procedure AVXMtxMultLowTria2T2Store1(mt1 : PDouble; LineWidth1 : TASMNativeInt; mt2 : PDouble; LineWidth2 : TASMNativeInt;
  width1, height1, width2, height2 : TASMNativeInt);
{$IFDEF FPC}
begin
{$ENDIF}
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
         vxorpd ymm0, ymm0, ymm0;
         // for idx := 0 to width2 - x - 2
         mov edi, ebx;
         test edi, edi;
         jz @@foridxloopend;

         // unrolled loop 4x2
         add edi, 64;
         jg @@foridxloopSSE;
         @@foridxlongloop:
            vmovupd ymm1, [eax + edi - 64];
            vmovupd ymm2, [edx + edi - 64];
            vmulpd ymm1, ymm1, ymm2;
            vaddpd ymm0, ymm0, ymm1;

            vmovupd ymm1, [eax + edi - 32];
            vmovupd ymm2, [edx + edi - 32];
            vmulpd ymm1, ymm1, ymm2;
            vaddpd ymm0, ymm0, ymm1;
         add edi, 64;
         jl @@foridxlongloop;

         vextractf128 xmm1, ymm0, 1;
         vhaddpd xmm0, xmm0, xmm1;

         // sse part
         @@foridxloopSSE:
         sub edi, 48;
         jg @@foridxloopstart;

         @@foridxSSEloop:
            vmovupd xmm1, [eax + edi - 16];
            vmovupd xmm2, [edx + edi - 16];
            vmulpd xmm1, xmm1, xmm2;
            vaddpd xmm0, xmm0, xmm1;
         add edi, 16;
         jl @@foridxSSEloop;

         @@foridxloopStart:
         vhaddpd xmm0, xmm0, xmm0;
         sub edi, 16;
         jz @@foridxloopend;

         @@foridxloop:
            vmovsd xmm1, [eax + edi];
            vmovsd xmm2, [edx + edi];
            vmulsd xmm1, xmm1, xmm2;
            vaddsd xmm0, xmm0, xmm1;
         add edi, 8;
         jnz @@foridxloop;

         @@foridxloopend:

         // last element is unit:
         vmovsd xmm1, [eax];
         vaddsd xmm0, xmm0, xmm1;

         // write back
         // PConstDoubleArr(pMt1)^[width2 - x - 1] := tmp + valCounter1^[width2 - x - 1];
         vmovsd [eax], xmm0;
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
   vzeroupper;
   pop esi;
   pop edi;
   pop ebx;
end;
{$IFDEF FPC}
end;
{$ENDIF}

// no avx optimization yet...
procedure AVXMtxMultTria2Store1Unit(mt1 : PDouble; LineWidth1 : TASMNativeInt; mt2 : PDouble; LineWidth2 : TASMNativeInt;
  width1, height1, width2, height2 : TASMNativeInt);
var iter : TASMNativeInt;
{$IFDEF FPC}
begin
{$ENDIF}
     asm
        // prolog
        push ebx;
        push edi;
        push esi;
        // init

        mov eax, width1;
        dec eax;
        imul eax, -8;
        mov iter, eax;

        // inc(mt2, width2 - 1);
        mov ebx, mt2;
        mov edx, width2;
        dec edx;
        imul edx, 8; //sizeof(double)
        add ebx, edx;

        mov edx, width2;
        dec edx;
        mov width2, edx;

        mov edx, LineWidth2;

        // for x := 0 to width2 - 2 do
        @@forxloop:
           mov edi, height1;

           mov eax, mt1;
           sub eax, iter;

           // for y := 0 to height1 - 1
           @@foryloop:
              // tmp := 0;
              vxorpd xmm0, xmm0, xmm0;

              // ebx, mt2
              mov ecx, ebx;

              // for idx := 0 to width1 - x - 2
              mov esi, iter;

              // check if we have enough iterations:
              cmp esi, 0;
              jge @@foridxloopend;

              @@foridxloop:
                 vmovsd xmm1, [ecx];
                 vmovsd xmm2, [eax + esi];

                 add ecx, edx;  // + linewidth2

                 vmulsd xmm1, xmm1, xmm2;
                 vaddsd xmm0, xmm0, xmm1;

              add esi, 8;
              jnz @@foridxloop;

              @@foridxloopend:

              // last element is unit
              vaddsd xmm0, xmm0, [eax];

              // PConstDoubleArr(pmt1)^[width2 - 1 - x] := tmp;
              mov ecx, eax;
              add ecx, iter;
              mov esi, width2;
              //dec esi;
              vmovsd [ecx + 8*esi], xmm0;

              // inc(PByte(pmT1), LineWidth1);
              add eax, LineWidth1;

           dec edi;
           jnz @@foryloop;

           // reduce for idx loop
           add iter, 8;
           // dec(mt2);
           sub ebx, 8;

        dec width2;
        jnz @@forxloop;

        @@endproc:

        // cleanup stack
        vzeroupper;

        pop esi;
        pop edi;
        pop ebx;
     end;
{$IFDEF FPC}
end;
{$ENDIF}

{$ENDIF}

end.
