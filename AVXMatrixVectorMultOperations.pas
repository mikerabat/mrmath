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


unit AVXMatrixVectorMultOperations;

// #######################################################
// #### special routines for matrix vector multiplications.
// #######################################################

interface

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}
{$IFNDEF x64}

uses MatrixConst;

procedure AVXMatrixVectMultT(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);

procedure AVXMatrixVectMult(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);

// routines with special input layouts: LineWidthV needs to be sizeof(double)
procedure AVXMatrixVectMultAlignedVAligned(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);
procedure AVXMatrixVectMultUnAlignedVAligned(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);

// destlinewidth needs to be sizeof(double)!
// no speed gain agains amsmatrixVectMultT
procedure AVXMatrixVecMultTDestVec(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);

// rank1 update: A = A + alpha*X*Y' where x and y are vectors. It's assumed that y is sequential
procedure AVXRank1UpdateSeq(A : PDouble; const LineWidthA : TASMNativeInt; width, height : TASMNativeInt;
  const alpha : double; X, Y : PDouble; incX, incY : TASMNativeInt);

procedure AVXRank1UpdateSeqAligned(A : PDouble; const LineWidthA : TASMNativeInt; width, height : TASMNativeInt;
  const alpha : double; X, Y : PDouble; incX, incY : TASMNativeInt);

{$ENDIF}

implementation

{$IFNDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$ENDIF}

procedure AVXMatrixVectMult(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);
// note: RCX = dest, RDX = destLineWidth, R8 = mt1, R9 = v
var dXMM4, dXMM5, dXMM6, dXMM7 : Array[0..1] of double;
    iRBX, iRSI, iRDI, iR12, iR13, iR14 : TASMNativeInt;
{$IFDEF FPC}
begin
{$ENDIF}
asm
   // prolog - simulate stack
   push ebx;
   push edi;
   push esi;

   mov ecx, dest;

   // for the final multiplication
   lea ebx, alpha;
   vmovsd xmm6, [ebx];
   lea eax, beta;
   vmovhpd xmm6, xmm6, [eax];

   // prepare for loop
   mov esi, LineWidthMT;
   mov edi, LineWidthV;
   //mov r13, height;
   //mov r14, width;

   sub height, 4;
   js @@foryloopend;

   // init for x := 0 to width - 1:
   @@foryloop:

       // init values:
       vxorpd xmm0, xmm0, xmm0;
       vxorpd xmm1, xmm1, xmm1;
       vxorpd xmm2, xmm2, xmm2;
       vxorpd xmm3, xmm3, xmm3;  // res := 0;
       mov eax, mt1;       // eax = first matrix element
       mov ebx, v;         // ebx = first vector element

       mov edx, width;      // edx = width
       @@forxloop:
           vmovsd xmm4, [ebx];

           vmovsd xmm5, [eax];
           vmulsd xmm5, xmm5, xmm4;
           vaddsd xmm0, xmm0, xmm5;

           vmovsd xmm5, [eax + esi];
           vmulsd xmm5, xmm5, xmm4;
           vaddsd xmm1, xmm1, xmm5;

           vmovsd xmm5, [eax + 2*esi];
           vmulsd xmm5, xmm5, xmm4;
           vaddsd xmm2, xmm2, xmm5;

           add eax, esi;
           vmovsd xmm5, [eax + 2*esi];
           vmulsd xmm5, xmm5, xmm4;
           vaddsd xmm3, xmm3, xmm5;
           sub eax, esi;

           add eax, 8;
           add ebx, edi;

       dec edx;
       jnz @@forxloop;

       // result building
       // write back result (final addition and compactation)

       // calculate dest = beta*dest + alpha*xmm0
       mov eax, mt1;
       vmovhpd xmm0, xmm0, [ecx];
       vmulpd xmm0, xmm0, xmm6;
       vhaddpd xmm0, xmm0, xmm0;
       vmovsd [ecx], xmm0;
       add ecx, destLineWidth;

       vmovhpd xmm1, xmm1, [ecx];
       vmulpd xmm1, xmm1, xmm6;
       vhaddpd xmm1, xmm1, xmm1;
       vmovsd [ecx], xmm1;
       add ecx, destLineWidth;

       vmovhpd xmm2, xmm2, [ecx];
       vmulpd xmm2, xmm2, xmm6;
       vhaddpd xmm2, xmm2, xmm2;
       vmovsd [ecx], xmm2;
       add ecx, destLineWidth;

       vmovhpd xmm3, xmm3, [ecx];
       vmulpd xmm3, xmm3, xmm6;
       vhaddpd xmm3, xmm3, xmm3;
       vmovsd [ecx], xmm3;
       add ecx, destLineWidth;             // next dest element

       // next mt1
       mov eax, mt1;
       lea eax, [eax + 4*esi];
       mov mt1, eax;

       // next rseult
   sub height, 4;
   jns @@foryloop;

   // ###########################################
   // #### Remaining rows (max 4):
   @@foryloopend:
   add height, 4;
   jz @@vecend;

   @@foryshortloop:
       // init values:
       vxorpd xmm0, xmm0, xmm0;
       mov eax, mt1;       // eax = first matrix element
       mov ebx, v;       // ebx = first vector element

       mov edx, width;      // edx = width
       @@forxshortloop:
           vmovsd xmm4, [ebx];

           vmovsd xmm5, [eax];
           vmulsd xmm5, xmm5, xmm4;
           vaddsd xmm0, xmm0, xmm5;

           add eax, 8;
           add ebx, edi;
       dec edx;
       jnz @@forxshortloop;

       // build result

       // calculate dest = beta*dest + alpha*xmm0
       vmovhpd xmm0, xmm0, [ecx];
       vmulpd xmm0, xmm0, xmm6;
       vhaddpd xmm0, xmm0, xmm0;
       vmovsd [ecx], xmm0;
       add ecx, destLineWidth;
       mov eax, mt1;
       add eax, LineWidthMT;
       mov mt1, eax;

   sub height, 1;
   jnz @@foryshortloop;

   @@vecend:

   // epilog pop "stack"
   vzeroupper;

   pop esi;
   pop edi;
   pop ebx;
end;
{$IFDEF FPC}
end;
{$ENDIF}


// routines with special input layouts: LineWidthV needs to be sizeof(double)
procedure AVXMatrixVectMultAlignedVAligned(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);
{$IFDEF FPC}
begin
{$ENDIF}
asm
   // prolog - simulate stack
   push ebx;
   push edi;
   push esi;

   // for the final multiplication
   lea ebx, alpha;
   vmovsd xmm6, [ebx];
   lea eax, beta;
   vmovhpd xmm6, xmm6, [eax];


   // prepare for loop
   mov ecx, dest;
   mov esi, LineWidthMT;

   sub width, 4;
   sub height, 4;
   js @@foryloopend;

   // we need at least a width of 4 for the fast unrolled loop
   cmp width, 0;
   jl @@foryloopend;

   // init for x := 0 to width - 1:
   @@foryloop:

       // init values:
       vxorpd ymm0, ymm0, ymm0;
       vxorpd ymm1, ymm1, ymm1;
       vxorpd ymm2, ymm2, ymm2;
       vxorpd ymm3, ymm3, ymm3;  // res := 0;
       mov eax, mt1;       // eax = first matrix element
       mov ebx, v;       // ebx = first vector element

       mov edx, width;      // edx = width
       @@forxloop:
           vmovapd ymm4, [ebx];

           //vmovapd ymm5, [eax];
           vmulpd ymm5, ymm4, [eax];
           vaddpd ymm0, ymm0, ymm5;

           //vmovapd ymm5, [eax + esi];
           vmulpd ymm5, ymm4, [eax + esi];
           vaddpd ymm1, ymm1, ymm5;

           //vmovapd ymm5, [eax + 2*esi];
           vmulpd ymm5, ymm4, [eax + 2*esi];
           vaddpd ymm2, ymm2, ymm5;

           add eax, esi;
           //vmovapd ymm5, [eax + 2*esi];
           vmulpd ymm5, ymm4, [eax + 2*esi];
           vaddpd ymm3, ymm3, ymm5;
           sub eax, esi;

           add eax, 32;
           add ebx, 32;

       sub edx, 4;
       jge @@forxloop;

       vextractf128 xmm4, ymm0, 1;
       vhaddpd xmm0, xmm0, xmm4;
       vextractf128 xmm5, ymm1, 1;
       vhaddpd xmm1, xmm1, xmm5;
       vextractf128 xmm4, ymm2, 1;
       vhaddpd xmm2, xmm2, xmm4;
       vextractf128 xmm5, ymm3, 1;
       vhaddpd xmm3, xmm3, xmm5;

       // special treatment for the last value(s):
       add edx, 4;
       jz @@resbuild;

       @@shortloopx:
          vmovsd xmm4, [ebx];
          vmovsd xmm5, [eax];

          vmulsd xmm5, xmm5, xmm4;
          vaddsd xmm0, xmm0, xmm5;

          vmovsd xmm5, [eax + esi];

          vmulsd xmm5, xmm5, xmm4;
          vaddsd xmm1, xmm1, xmm5;

          vmovsd xmm5, [eax + 2*esi];

          vmulsd xmm5, xmm5, xmm4;
          vaddsd xmm2, xmm2, xmm5;

          add eax, esi;
          vmovsd xmm5, [eax + 2*esi];
          sub eax, esi;

          vmulsd xmm5, xmm5, xmm4;
          vaddsd xmm3, xmm3, xmm5;

          add eax, 8;
          add ebx, 8;
       sub edx, 1;
       jnz @@shortloopx;


       @@resbuild:
       // result building
       // write back result (final addition and compactation)

       // calculate dest = beta*dest + alpha*xmm0
       vmovsd xmm5, [ecx];              // first element
       vhaddpd xmm0, xmm0, xmm5;        // calculate xmm0_1 + xmm0_2 (store low) xmm5_1 + xmm5_2 (store high)
       vmulpd xmm0, xmm0, xmm6;         // beta * dest + alpha*xmm0
       vhaddpd xmm0, xmm0, xmm0;        // final add
       vmovsd [ecx], xmm0;              // store back
       add ecx, destLineWidth;

       vmovsd xmm5, [ecx];
       vhaddpd xmm1, xmm1, xmm5;
       vmulpd xmm1, xmm1, xmm6;
       vhaddpd xmm1, xmm1, xmm1;
       vmovsd [ecx], xmm1;
       add ecx, destLineWidth;

       vmovsd xmm5, [ecx];
       vhaddpd xmm2, xmm2, xmm5;
       vmulpd xmm2, xmm2, xmm6;
       vhaddpd xmm2, xmm2, xmm2;
       vmovsd [ecx], xmm2;
       add ecx, destLineWidth;

       vmovsd xmm5, [ecx];
       vhaddpd xmm3, xmm3, xmm5;
       vmulpd xmm3, xmm3, xmm6;
       vhaddpd xmm3, xmm3, xmm3;
       vmovsd [ecx], xmm3;
       add ecx, destLineWidth;

       mov eax, mt1;
       lea eax, [eax + 4*esi];
       mov mt1, eax;

       // next rseult
   sub height, 4;
   jge @@foryloop;

   // ###########################################
   // #### Remaining rows (max 4 or more if width is <4):
   @@foryloopend:
   add height, 4;
   jz @@vecend;

   @@foryshortloop:
       // init values:
       vxorpd ymm0, ymm0, ymm0;
       mov eax, mt1;       // eax = first matrix element
       mov ebx, v;       // ebx = first vector element

       mov edx, width;      // r10 = width - 4
       cmp edx, 0;
       jl @@shortloopend;

       @@forxshortloop:
           vmovapd ymm4, [ebx];
           //vmovapd ymm5, [eax];
           vmulpd ymm5, ymm4, [eax];
           vaddpd ymm0, ymm0, ymm5;

           add eax, 32;
           add ebx, 32;
       sub edx, 4;
       jge @@forxshortloop;

       vextractf128 xmm4, ymm0, 1;
       vhaddpd xmm0, xmm0, xmm4;

       @@shortloopend:

       add edx, 4;
       // test if there are elements left
       jz @@resbuildshort;
       @@forxshortestloop:
           vmovsd xmm4, [ebx];
           vmovsd xmm5, [eax];

           vmulsd xmm5, xmm5, xmm4;
           vaddsd xmm0, xmm0, xmm5;

           add eax, 8;
           add ebx, 8;
       dec edx;
       jnz @@forxshortestloop;

       @@resbuildshort:

       // build result

       // calculate dest = beta*dest + alpha*xmm0
       vmovsd xmm5, [ecx];
       vhaddpd xmm0, xmm0, xmm5;
       vmulpd xmm0, xmm0, xmm6;
       vhaddpd xmm0, xmm0, xmm0;
       vmovsd [ecx], xmm0;

       add ecx, destLineWidth;
       mov eax, mt1;
       add eax, esi;
       mov mt1, eax;

   dec height;
   jnz @@foryshortloop;

   @@vecend:

   // epilog pop "stack"
   vzeroupper;
   pop esi;
   pop edi;
   pop ebx;
end;
{$IFDEF FPC}
end;
{$ENDIF}

procedure AVXMatrixVectMultUnAlignedVAligned(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);
{$IFDEF FPC}
begin
{$ENDIF}
asm
   // prolog - simulate stack
   push ebx;
   push edi;
   push esi;

   // for the final multiplication
   lea ebx, alpha;
   vmovsd xmm6, [ebx];
   lea eax, beta;
   vmovhpd xmm6, xmm6, [eax];

   // prepare for loop
   mov ecx, dest;
   mov esi, LineWidthMT;

   sub width, 4;
   sub height, 4;
   js @@foryloopend;

   // we need at least a width of 4 for the fast unrolled loop
   cmp width, 0;
   jl @@foryloopend;

   // init for x := 0 to width - 1:
   @@foryloop:

       // init values:
       vxorpd ymm0, ymm0, ymm0;
       vxorpd ymm1, ymm1, ymm1;
       vxorpd ymm2, ymm2, ymm2;
       vxorpd ymm3, ymm3, ymm3;  // res := 0;
       mov eax, mt1;       // eax = first matrix element
       mov ebx, v;       // ebx = first vector element

       mov edx, width;      // edx = width
       @@forxloop:
           vmovupd ymm4, [ebx];

           vmovupd ymm5, [eax];
           vmulpd ymm5, ymm4, ymm5;
           vaddpd ymm0, ymm0, ymm5;

           vmovupd ymm5, [eax + esi];
           vmulpd ymm5, ymm4, ymm5;
           vaddpd ymm1, ymm1, ymm5;

           vmovupd ymm5, [eax + 2*esi];
           vmulpd ymm5, ymm4, ymm5;
           vaddpd ymm2, ymm2, ymm5;

           add eax, esi;
           vmovupd ymm5, [eax + 2*esi];
           vmulpd ymm5, ymm4, ymm5;
           vaddpd ymm3, ymm3, ymm5;
           sub eax, esi;

           add eax, 32;
           add ebx, 32;

       sub edx, 4;
       jge @@forxloop;

       vextractf128 xmm4, ymm0, 1;
       vhaddpd xmm0, xmm0, xmm4;
       vextractf128 xmm5, ymm1, 1;
       vhaddpd xmm1, xmm1, xmm5;
       vextractf128 xmm4, ymm2, 1;
       vhaddpd xmm2, xmm2, xmm4;
       vextractf128 xmm5, ymm3, 1;
       vhaddpd xmm3, xmm3, xmm5;

       // special treatment for the last value(s):
       add edx, 4;
       jz @@resbuild;

       @@shortloopx:
          vmovsd xmm4, [ebx];
          vmovsd xmm5, [eax];

          vmulsd xmm5, xmm5, xmm4;
          vaddsd xmm0, xmm0, xmm5;

          vmovsd xmm5, [eax + esi];

          vmulsd xmm5, xmm5, xmm4;
          vaddsd xmm1, xmm1, xmm5;

          vmovsd xmm5, [eax + 2*esi];

          vmulsd xmm5, xmm5, xmm4;
          vaddsd xmm2, xmm2, xmm5;

          add eax, esi;
          vmovsd xmm5, [eax + 2*esi];
          sub eax, esi;

          vmulsd xmm5, xmm5, xmm4;
          vaddsd xmm3, xmm3, xmm5;

          add eax, 8;
          add ebx, 8;
       sub edx, 1;
       jnz @@shortloopx;


       @@resbuild:
       // result building
       // write back result (final addition and compactation)

       // calculate dest = beta*dest + alpha*xmm0
       vmovsd xmm5, [ecx];              // first element
       vhaddpd xmm0, xmm0, xmm5;        // calculate xmm0_1 + xmm0_2 (store low) xmm5_1 + xmm5_2 (store high)
       vmulpd xmm0, xmm0, xmm6;         // beta * dest + alpha*xmm0
       vhaddpd xmm0, xmm0, xmm0;        // final add
       vmovsd [ecx], xmm0;              // store back
       add ecx, destLineWidth;

       vmovsd xmm5, [ecx];
       vhaddpd xmm1, xmm1, xmm5;
       vmulpd xmm1, xmm1, xmm6;
       vhaddpd xmm1, xmm1, xmm1;
       vmovsd [ecx], xmm1;
       add ecx, destLineWidth;

       vmovsd xmm5, [ecx];
       vhaddpd xmm2, xmm2, xmm5;
       vmulpd xmm2, xmm2, xmm6;
       vhaddpd xmm2, xmm2, xmm2;
       vmovsd [ecx], xmm2;
       add ecx, destLineWidth;

       vmovsd xmm5, [ecx];
       vhaddpd xmm3, xmm3, xmm5;
       vmulpd xmm3, xmm3, xmm6;
       vhaddpd xmm3, xmm3, xmm3;
       vmovsd [ecx], xmm3;
       add ecx, destLineWidth;

       mov eax, mt1;
       lea eax, [eax + 4*esi];
       mov mt1, eax;

       // next rseult
   sub height, 4;
   jge @@foryloop;

   // ###########################################
   // #### Remaining rows (max 4 or more if width is <4):
   @@foryloopend:
   add height, 4;
   jz @@vecend;

   @@foryshortloop:
       // init values:
       vxorpd ymm0, ymm0, ymm0;
       mov eax, mt1;       // eax = first matrix element
       mov ebx, v;       // ebx = first vector element

       mov edx, width;      // edx = width - 4
       cmp edx, 0;
       jl @@shortloopend;

       @@forxshortloop:
           vmovupd ymm4, [ebx];
           vmovupd ymm5, [eax];
           vmulpd ymm5, ymm4, ymm5;
           vaddpd ymm0, ymm0, ymm5;

           add eax, 32;
           add ebx, 32;
       sub edx, 4;
       jge @@forxshortloop;

       vextractf128 xmm4, ymm0, 1;
       vhaddpd xmm0, xmm0, xmm4;

       @@shortloopend:

       add edx, 4;
       // test if there are elements left
       jz @@resbuildshort;
       @@forxshortestloop:
           vmovsd xmm4, [ebx];
           vmovsd xmm5, [eax];

           vmulsd xmm5, xmm5, xmm4;
           vaddsd xmm0, xmm0, xmm5;

           add eax, 8;
           add ebx, 8;
       dec edx;
       jnz @@forxshortestloop;

       @@resbuildshort:

       // build result

       // calculate dest = beta*dest + alpha*xmm0
       vmovsd xmm5, [ecx];
       vhaddpd xmm0, xmm0, xmm5;
       vmulpd xmm0, xmm0, xmm6;
       vhaddpd xmm0, xmm0, xmm0;
       vmovsd [ecx], xmm0;

       add ecx, destLineWidth;
       add mt1, esi;
   dec height;
   jnz @@foryshortloop;

   @@vecend:

   // epilog pop "stack"
   vzeroupper;
   pop esi;
   pop edi;
   pop ebx;
end;
{$IFDEF FPC}
end;
{$ENDIF}

// this function is not that well suited for use of simd instructions...
// so only this version exists
procedure AVXMatrixVectMultT(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);
var dymm8, dymm9, dymm10, dymm11 : TYMMArr;
{$IFDEF FPC}
begin
{$ENDIF}
asm
   // prolog - simulate stack
   push ebx;
   push edi;
   push esi;

   // for the final multiplication
   lea eax, alpha;
   vbroadcastsd ymm6, [eax];
   lea eax, beta;
   vbroadcastsd ymm7, [eax];

   // prepare for loop
   mov ecx, dest;
   mov esi, LineWidthMT;
   mov edi, LineWidthV;
   //mov r13, height;
   //mov r14, width;

   sub width, 16;
   js @@forxloopend;

   // init for x := 0 to width - 1:
   @@forxloop:

       // init values:
       vxorpd ymm0, ymm0, ymm0;
       vmovupd dymm8, ymm0;
       vmovupd dymm9, ymm0;
       vmovupd dymm10, ymm0;
       vmovupd dymm11, ymm0;

       mov eax, mt1;       // eax = first matrix element
       mov ebx, V;       // ebx = first vector element

       mov edx, height;
       @@foryloop:
           vbroadcastsd ymm3, [ebx];

           vmovupd ymm0, dymm8;
           vmovupd ymm4, [eax];
           vmulpd ymm4, ymm4, ymm3;
           vaddpd ymm0, ymm0, ymm4;
           vmovupd dymm8, ymm0;

           vmovupd ymm1, dymm9;
           vmovupd ymm4, [eax + 32];
           vmulpd ymm4, ymm4, ymm3;
           vaddpd ymm1, ymm1, ymm4;
           vmovupd dymm9, ymm1;

           vmovupd ymm0, dymm10;
           vmovupd ymm4, [eax + 64];
           vmulpd ymm4, ymm4, ymm3;
           vaddpd ymm0, ymm0, ymm4;
           vmovupd dymm10, ymm0;

           vmovupd ymm1, dymm11;
           vmovupd ymm4, [eax + 96];
           vmulpd ymm4, ymm4, ymm3;
           vaddpd ymm1, ymm1, ymm4;
           vmovupd dymm11, ymm1;

           add eax, esi;
           add ebx, edi;

       dec edx;
       jnz @@foryloop;

       // result building
       // write back result (final addition and compactation)

       // calculate dest = beta*dest + alpha*xmm0
       // first two
       mov edx, destLineWidth;
       vmovsd xmm3, [ecx];
       vmovsd xmm4, [ecx + edx];
       vmovlhps xmm3, xmm3, xmm4;

       lea eax, dymm8;
       vmovupd xmm0, [eax];
       vmulpd xmm0, xmm0, xmm6;   // alpha*res
       vmulpd xmm3, xmm3, xmm7;   // dest*beta

       vaddpd xmm3, xmm3, xmm0;
       vmovhlps xmm4, xmm4, xmm3;
       vmovsd [ecx], xmm3;
       vmovsd [ecx + edx], xmm4;
       add ecx, edx;
       add ecx, edx;

       // second two
       vmovsd xmm3, [ecx];
       vmovsd xmm4, [ecx + edx];
       vmovlhps xmm3, xmm3, xmm4;

       //movupd xmm0, res1;
       vmovupd xmm0, [eax + 16];
       vmulpd xmm0, xmm0, xmm6;   // alpha*res
       vmulpd xmm3, xmm3, xmm7;   // dest*beta

       vaddpd xmm3, xmm3, xmm0;
       vmovhlps xmm4, xmm4, xmm3;
       vmovsd [ecx], xmm3;
       vmovsd [ecx + edx], xmm4;

       add ecx, edx;
       add ecx, edx;

       // third two
       vmovsd xmm3, [ecx];
       vmovsd xmm4, [ecx + edx];
       vmovlhps xmm3, xmm3, xmm4;

       lea eax, dymm9;
       vmovupd xmm0, [eax];
       vmulpd xmm0, xmm0, xmm6;   // alpha*res
       vmulpd xmm3, xmm3, xmm7;   // dest*beta

       vaddpd xmm3, xmm3, xmm0;
       vmovhlps xmm4, xmm4, xmm3;
       vmovsd [ecx], xmm3;
       vmovsd [ecx + edx], xmm4;

       add ecx, edx;
       add ecx, edx;

       // forth two
       vmovsd xmm3, [ecx];
       vmovsd xmm4, [ecx + edx];
       vmovlhps xmm3, xmm3, xmm4;

       //movupd xmm0, res3;
       vmovupd xmm0, [eax + 16];
       vmulpd xmm0, xmm0, xmm6;   // alpha*res
       vmulpd xmm3, xmm3, xmm7;   // dest*beta

       vaddpd xmm3, xmm3, xmm0;
       vmovhlps xmm4, xmm4, xmm3;
       vmovsd [ecx], xmm3;
       vmovsd [ecx + edx], xmm4;

       add ecx, edx;
       add ecx, edx;

       // fith two
       vmovsd xmm3, [ecx];
       vmovsd xmm4, [ecx + edx];
       vmovlhps xmm3, xmm3, xmm4;

       //movupd xmm0, res4;
       lea eax, dymm10;
       vmovupd xmm0, [eax];
       vmulpd xmm0, xmm0, xmm6;   // alpha*res
       vmulpd xmm3, xmm3, xmm7;   // dest*beta

       vaddpd xmm3, xmm3, xmm0;
       vmovhlps xmm4, xmm4, xmm3;
       vmovsd [ecx], xmm3;
       vmovsd [ecx + edx], xmm4;

       add ecx, edx;
       add ecx, edx;

       // sixth two
       vmovsd xmm3, [ecx];
       vmovsd xmm4, [ecx + edx];
       vmovlhps xmm3, xmm3, xmm4;

       //movupd xmm0, res5;
       vmovupd xmm0, [eax + 16];
       vmulpd xmm0, xmm0, xmm6;   // alpha*res
       vmulpd xmm3, xmm3, xmm7;   // dest*beta

       vaddpd xmm3, xmm3, xmm0;
       vmovhlps xmm4, xmm4, xmm3;
       vmovsd [ecx], xmm3;
       vmovsd [ecx + edx], xmm4;

       add ecx, edx;
       add ecx, edx;

       // seventh two
       vmovsd xmm3, [ecx];
       vmovsd xmm4, [ecx + edx];
       vmovlhps xmm3, xmm3, xmm4;

       //movupd xmm0, res6;
       lea eax, dymm11;
       vmovupd xmm0, [eax];
       vmulpd xmm0, xmm0, xmm6;   // alpha*res
       vmulpd xmm3, xmm3, xmm7;   // dest*beta

       vaddpd xmm3, xmm3, xmm0;
       vmovhlps xmm4, xmm4, xmm3;
       vmovsd [ecx], xmm3;
       vmovsd [ecx + edx], xmm4;

       add ecx, edx;
       add ecx, edx;

       // eighth two
       vmovsd xmm3, [ecx];
       vmovsd xmm4, [ecx + edx];
       vmovlhps xmm3, xmm3, xmm4;

       //movupd xmm0, res7;
       vmovupd xmm0, [eax + 16];
       vmulpd xmm0, xmm0, xmm6;   // alpha*res
       vmulpd xmm3, xmm3, xmm7;   // dest*beta

       vaddpd xmm3, xmm3, xmm0;
       vmovhlps xmm4, xmm4, xmm3;
       vmovsd [ecx], xmm3;
       vmovsd [ecx + edx], xmm4;

       add ecx, edx;
       add ecx, edx;

       // next results:
       mov eax, mt1;
       add eax, 8*16;      // next mt1 element
       mov mt1, eax;
   sub width, 16;
   jns @@forxloop;

   @@forxloopend:

   // ###########################################
   // elements that not fit into mod 16
   add width, 16;
   jz @@vecaddend;

   @@forxshortloop:
       vxorpd xmm0, xmm0, xmm0;  // first two elements

       mov eax, mt1;       // eax = first matrix element
       mov ebx, v;       // ebx = first vector element

       mov edx, height;

       @@forshortyloop:
           vmovsd xmm1, [eax];
           vmovsd xmm2, [ebx];

           vmulsd xmm1, xmm1, xmm2;
           vaddsd xmm0, xmm0, xmm1;

           add eax, esi;
           add ebx, edi;

       dec edx;
       jnz @@forshortyloop;

       vmulsd xmm0, xmm0, xmm6;  // alpha*res

       vmovsd xmm3, [ecx];
       vmulsd xmm3, xmm3, xmm7;  //dest*beta
       vaddsd xmm0, xmm0, xmm3;
       vmovsd [ecx], xmm0;

       // next row
       add ecx, destLineWidth;
       mov eax, mt1;
       add eax, 8;
       mov mt1, eax;

   dec width;
   jnz @@forxshortloop;


   @@vecaddend:

   // epilog pop "stack"
   vzeroupper;
   pop esi;
   pop edi;
   pop ebx;
end;
{$IFDEF FPC}
end;
{$ENDIF}


procedure AVXMatrixVecMultTDestVec(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);
var dymm8, dymm9, dymm10, dymm11 : TYMMArr;
{$IFDEF FPC}
begin
{$ENDIF}
asm
   // prolog - simulate stack
   push ebx;
   push edi;
   push esi;

   // for the final multiplication
   lea eax, alpha;
   vbroadcastsd ymm6, [eax];
   lea eax, beta;
   vbroadcastsd ymm7, [eax];

   // prepare for loop
   mov ecx, dest;
   mov esi, LineWidthMT;
   mov edi, LineWidthV;

   //mov r13, height;
   //mov r14, width;

   sub width, 16;
   js @@forxloopend;

   // init for x := 0 to width - 1:
   @@forxloop:

       // init values:
       vxorpd ymm0, ymm0, ymm0;
       vmovupd dymm8, ymm0;
       vmovupd dymm9, ymm0;
       vmovupd dymm10, ymm0;
       vmovupd dymm11, ymm0;

       mov eax, mt1;       // eax = first matrix element
       mov ebx, v;         // ebx = first vector element

       mov edx, height;
       @@foryloop:
           vbroadcastsd ymm3, [ebx];

           vmovupd ymm0, dymm8;
           vmovupd ymm4, [eax];
           vmulpd ymm4, ymm4, ymm3;
           vaddpd ymm0, ymm0, ymm4;
           vmovupd dymm8, ymm0;

           vmovupd ymm1, dymm9;
           vmovupd ymm4, [eax + 32];
           vmulpd ymm4, ymm4, ymm3;
           vaddpd ymm1, ymm1, ymm4;
           vmovupd dymm9, ymm1;

           vmovupd ymm0, dymm10;
           vmovupd ymm4, [eax + 64];
           vmulpd ymm4, ymm4, ymm3;
           vaddpd ymm0, ymm0, ymm4;
           vmovupd dymm10, ymm0;

           vmovupd ymm1, dymm11;
           vmovupd ymm4, [eax + 96];
           vmulpd ymm4, ymm4, ymm3;
           vaddpd ymm1, ymm1, ymm4;
           vmovupd dymm11, ymm1;

           add eax, esi;
           add ebx, edi;

       dec edx;
       jnz @@foryloop;

       // result building
       // write back result (final addition and compactation)

       // calculate dest = beta*dest + alpha*xmm0
       // first 4
       vmovupd ymm3, [ecx];

       vmovupd ymm0, dymm8;
       vmulpd ymm0, ymm0, ymm6; // alpha*res
       vmulpd ymm3, ymm3, ymm7; // dest*beta
       vaddpd ymm3, ymm3, ymm0;
       vmovupd [ecx], ymm3;
       add ecx, 32;

       // second 4
       vmovupd ymm3, [ecx];

       vmovupd ymm0, dymm9;
       vmulpd ymm0, ymm0, ymm6; // alpha*res
       vmulpd ymm3, ymm3, ymm7; // dest*beta
       vaddpd ymm3, ymm3, ymm0;
       vmovupd [ecx], ymm3;
       add ecx, 32;

       // third 4
       vmovupd ymm3, [ecx];

       vmovupd ymm0, dymm10;
       vmulpd ymm0, ymm0, ymm6; // alpha*res
       vmulpd ymm3, ymm3, ymm7; // dest*beta
       vaddpd ymm3, ymm3, ymm0;
       vmovupd [ecx], ymm3;
       add ecx, 32;

       // forth 4
       vmovupd ymm3, [ecx];

       vmovupd ymm0, dymm11;
       vmulpd ymm0, ymm0, ymm6; // alpha*res
       vmulpd ymm3, ymm3, ymm7; // dest*beta
       vaddpd ymm3, ymm3, ymm0;
       vmovupd [ecx], ymm3;
       add ecx, 32;

       // next results:
       mov edx, mt1;
       add edx, 8*16;      // next mt1 element
       mov mt1, edx;
   sub width, 16;
   jns @@forxloop;

   @@forxloopend:

   // ###########################################
   // elements that not fit into mod 16
   add width, 16;
   jz @@vecaddend;

   @@forxshortloop:
       vxorpd xmm0, xmm0, xmm0;  // first two elements

       mov eax, mt1;       // eax = first matrix element
       mov ebx, v;       // ebx = first vector element

       mov edx, height;

       @@forshortyloop:
           vmovsd xmm1, [eax];
           vmovsd xmm2, [ebx];

           vmulsd xmm1, xmm1, xmm2;
           vaddsd xmm0, xmm0, xmm1;

           add eax, esi;
           add ebx, edi;

       dec edx;
       jnz @@forshortyloop;

       vmulsd xmm0, xmm0, xmm6;  // alpha*res

       vmovsd xmm3, [ecx];
       vmulsd xmm3, xmm3, xmm7;  //dest*beta
       vaddsd xmm0, xmm0, xmm3;
       vmovsd [ecx], xmm0;

       // next column
       add ecx, 8;
       mov eax, mt1;
       add eax, 8;
       mov mt1, eax;

   dec Width;
   jnz @@forxshortloop;


   @@vecaddend:

   // epilog pop "stack"
   vzeroupper;

   pop esi;
   pop edi;
   pop ebx;
end;
{$IFDEF FPC}
end;
{$ENDIF}


procedure AVXRank1UpdateSeq(A : PDouble; const LineWidthA : TASMNativeInt; width, height : TASMNativeInt;
  const alpha : double; X, Y : PDouble; incX, incY : TASMNativeInt);
{$IFDEF FPC}
begin
{$ENDIF}
asm
   // prolog - simulate stack
   push ebx;
   push edi;
   push esi;

   // performs A = A + alpha*X*Y' in row major form
   mov ecx, A;
   mov edi, X;

   // for the temp calculation
   lea eax, alpha;
   vbroadcastsd ymm3, [eax];

   // prepare for loop
   mov esi, incx;
   // init for y := 0 to height - 1:
   @@foryloop:
      // init values:
      vbroadcastsd ymm0, [edi];  // res := 0;
      vmulpd ymm0, ymm0, ymm3;   // tmp := alpha*pX^
      mov eax, ecx;              // eax = first destination element A
      mov ebx, Y;                // ebx = first y vector element

      // for j := 0 to width - 1 do
      mov edx, width;
      sub edx, 4;
      jl @@last3Elem;

      @@forxloop:
         vmovupd ymm1, [eax];
         vmovupd ymm2, [ebx];

         // pA^[j] := pA^[j] + tmp*pY1^[j];
         vmulpd ymm2, ymm2, ymm0;
         vaddpd ymm1, ymm1, ymm2;

         vmovupd [eax], ymm1;

         add eax, 32;
         add ebx, 32;

      sub edx, 4;
      jge @@forxloop;

      @@last3Elem:

      add edx, 4;
      jz @@nextline;

      // check if there is only one element to process
      cmp edx, 1;
      je @@lastElem;

      // handle 2 elements
      vmovupd xmm1, [eax];
      vmovupd xmm2, [ebx];

      vmulpd xmm2, xmm2, xmm0;
      vaddpd xmm1, xmm1, xmm2;
      vmovupd [eax], xmm1;
      add eax, 16;
      add ebx, 16;

      cmp edx, 2;
      je @@nextline;

      @@lastElem:

      vmovsd xmm1, [eax];
      vmovsd xmm2, [ebx];

      vmulsd xmm2, xmm2, xmm0;
      vaddsd xmm1, xmm1, xmm2;
      vmovsd [eax], xmm1;

      @@nextline:

      // next results:
      add edi, esi;
      add ecx, LineWidthA;
   dec height;          // r9 = height
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


procedure AVXRank1UpdateSeqAligned(A : PDouble; const LineWidthA : TASMNativeInt; width, height : TASMNativeInt;
  const alpha : double; X, Y : PDouble; incX, incY : TASMNativeInt);
{$IFDEF FPC}
begin
{$ENDIF}
asm
   // prolog - simulate stack
   push ebx;
   push edi;
   push esi;

   // performs A = A + alpha*X*Y' in row major form
   mov ecx, A;
   mov edi, X;

   // for the temp calculation
   lea eax, alpha;
   vbroadcastsd ymm3, [eax];

   // prepare for loop
   mov esi, incx;
   // init for y := 0 to height - 1:
   @@foryloop:
      // init values:
      vbroadcastsd ymm0, [edi];  // res := 0;
      vmulpd ymm0, ymm0, ymm3;   // tmp := alpha*pX^
      mov eax, ecx;              // eax = first destination element A
      mov ebx, Y;                // ebx = first y vector element

      // for j := 0 to width - 1 do
      mov edx, width;
      sub edx, 4;
      jl @@last3Elem;

      @@forxloop:
         vmovapd ymm1, [eax];
         vmovapd ymm2, [ebx];

         // pA^[j] := pA^[j] + tmp*pY1^[j];
         vmulpd ymm2, ymm2, ymm0;
         vaddpd ymm1, ymm1, ymm2;

         vmovapd [eax], ymm1;

         add eax, 32;
         add ebx, 32;

      sub edx, 4;
      jge @@forxloop;

      @@last3Elem:
      add edx, 4;

      jz @@nextline;

      // check if there is only one element to process
      cmp edx, 1;
      je @@lastElem;

      // handle 2 elements
      vmovapd xmm1, [eax];
      vmovapd xmm2, [ebx];

      vmulpd xmm2, xmm2, xmm0;
      vaddpd xmm1, xmm1, xmm2;
      vmovapd [eax], xmm1;
      add eax, 16;
      add ebx, 16;

      cmp edx, 2;
      je @@nextline;

      @@lastElem:

      vmovsd xmm1, [eax];
      vmovsd xmm2, [ebx];

      vmulsd xmm2, xmm2, xmm0;
      vaddsd xmm1, xmm1, xmm2;
      vmovsd [eax], xmm1;

      @@nextline:

      // next results:
      add edi, esi;
      add ecx, LineWidthA;
   dec height;
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

{$ENDIF}

end.
