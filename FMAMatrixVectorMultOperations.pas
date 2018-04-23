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


unit FMAMatrixVectorMultOperations;

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

procedure FMAMatrixVectMultT(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);

// routines with special input layouts: LineWidthV needs to be sizeof(double)
procedure FMAMatrixVectMultAlignedVAligned(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);
procedure FMAMatrixVectMultUnAlignedVAligned(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);

// destlinewidth needs to be sizeof(double)!
// no speed gain agains amsmatrixVectMultT
procedure FMAMatrixVecMultTDestVec(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);

// rank1 update: A = A + alpha*X*Y' where x and y are vectors. It's assumed that y is sequential
procedure FMARank1UpdateSeq(A : PDouble; const LineWidthA : TASMNativeInt; width, height : TASMNativeInt;
  const alpha : double; X, Y : PDouble; incX, incY : TASMNativeInt);

procedure FMARank1UpdateSeqAligned(A : PDouble; const LineWidthA : TASMNativeInt; width, height : TASMNativeInt;
  const alpha : double; X, Y : PDouble; incX, incY : TASMNativeInt);

{$ENDIF}

implementation

{$IFNDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$S-} {$ENDIF}

// routines with special input layouts: LineWidthV needs to be sizeof(double)
procedure FMAMatrixVectMultAlignedVAligned(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);
begin
asm
   // prolog - simulate stack
   push ebx;
   push edi;
   push esi;

   // for the final multiplication
   lea ebx, alpha;
   {$IFDEF FPC}vmovsd xmm6, [ebx];{$ELSE}db $C5,$FB,$10,$33;{$ENDIF} 
   lea eax, beta;
   {$IFDEF FPC}vmovhpd xmm6, xmm6, [eax];{$ELSE}db $C5,$C9,$16,$30;{$ENDIF} 


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
       {$IFDEF FPC}vxorpd ymm0, ymm0, ymm0;{$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} 
       {$IFDEF FPC}vxorpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$57,$C9;{$ENDIF} 
       {$IFDEF FPC}vxorpd ymm2, ymm2, ymm2;{$ELSE}db $C5,$ED,$57,$D2;{$ENDIF} 
       {$IFDEF FPC}vxorpd ymm3, ymm3, ymm3;  {$ELSE}db $C5,$E5,$57,$DB;{$ENDIF} // res := 0;
       mov eax, mt1;       // eax = first matrix element
       mov ebx, v;       // ebx = first vector element

       mov edx, width;      // edx = width
       @@forxloop:
           {$IFDEF FPC}vmovapd ymm4, [ebx];{$ELSE}db $C5,$FD,$28,$23;{$ENDIF} 

           {$IFDEF FPC}vfmadd231pd ymm0, ymm4, [eax];{$ELSE}db $C4,$E2,$DD,$B8,$00;{$ENDIF} 
           {$IFDEF FPC}vfmadd231pd ymm1, ymm4, [eax + esi];{$ELSE}db $C4,$E2,$DD,$B8,$0C,$30;{$ENDIF} 
           {$IFDEF FPC}vfmadd231pd ymm2, ymm4, [eax + 2*esi];{$ELSE}db $C4,$E2,$DD,$B8,$14,$70;{$ENDIF} 
           add eax, esi;
           {$IFDEF FPC}vfmadd231pd ymm3, ymm4, [eax + 2*esi];{$ELSE}db $C4,$E2,$DD,$B8,$1C,$70;{$ENDIF} 
           sub eax, esi;

           add eax, 32;
           add ebx, 32;

       sub edx, 4;
       jge @@forxloop;

       {$IFDEF FPC}vextractf128 xmm4, ymm0, 1;{$ELSE}db $C4,$E3,$7D,$19,$C4,$01;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm4;{$ELSE}db $C5,$F9,$7C,$C4;{$ENDIF} 
       {$IFDEF FPC}vextractf128 xmm5, ymm1, 1;{$ELSE}db $C4,$E3,$7D,$19,$CD,$01;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm1, xmm1, xmm5;{$ELSE}db $C5,$F1,$7C,$CD;{$ENDIF} 
       {$IFDEF FPC}vextractf128 xmm4, ymm2, 1;{$ELSE}db $C4,$E3,$7D,$19,$D4,$01;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm2, xmm2, xmm4;{$ELSE}db $C5,$E9,$7C,$D4;{$ENDIF} 
       {$IFDEF FPC}vextractf128 xmm5, ymm3, 1;{$ELSE}db $C4,$E3,$7D,$19,$DD,$01;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm3, xmm3, xmm5;{$ELSE}db $C5,$E1,$7C,$DD;{$ENDIF} 

       // special treatment for the last value(s):
       add edx, 4;
       jz @@resbuild;

       @@shortloopx:
          {$IFDEF FPC}vmovsd xmm4, [ebx];{$ELSE}db $C5,$FB,$10,$23;{$ENDIF} 
          {$IFDEF FPC}vmovsd xmm5, [eax];{$ELSE}db $C5,$FB,$10,$28;{$ENDIF} 

          {$IFDEF FPC}vmulsd xmm5, xmm5, xmm4;{$ELSE}db $C5,$D3,$59,$EC;{$ENDIF} 
          {$IFDEF FPC}vaddsd xmm0, xmm0, xmm5;{$ELSE}db $C5,$FB,$58,$C5;{$ENDIF} 

          {$IFDEF FPC}vmovsd xmm5, [eax + esi];{$ELSE}db $C5,$FB,$10,$2C,$30;{$ENDIF} 

          {$IFDEF FPC}vmulsd xmm5, xmm5, xmm4;{$ELSE}db $C5,$D3,$59,$EC;{$ENDIF} 
          {$IFDEF FPC}vaddsd xmm1, xmm1, xmm5;{$ELSE}db $C5,$F3,$58,$CD;{$ENDIF} 

          {$IFDEF FPC}vmovsd xmm5, [eax + 2*esi];{$ELSE}db $C5,$FB,$10,$2C,$70;{$ENDIF} 

          {$IFDEF FPC}vmulsd xmm5, xmm5, xmm4;{$ELSE}db $C5,$D3,$59,$EC;{$ENDIF} 
          {$IFDEF FPC}vaddsd xmm2, xmm2, xmm5;{$ELSE}db $C5,$EB,$58,$D5;{$ENDIF} 

          add eax, esi;
          {$IFDEF FPC}vmovsd xmm5, [eax + 2*esi];{$ELSE}db $C5,$FB,$10,$2C,$70;{$ENDIF} 
          sub eax, esi;

          {$IFDEF FPC}vmulsd xmm5, xmm5, xmm4;{$ELSE}db $C5,$D3,$59,$EC;{$ENDIF} 
          {$IFDEF FPC}vaddsd xmm3, xmm3, xmm5;{$ELSE}db $C5,$E3,$58,$DD;{$ENDIF} 

          add eax, 8;
          add ebx, 8;
       sub edx, 1;
       jnz @@shortloopx;


       @@resbuild:
       // result building
       // write back result (final addition and compactation)

       // calculate dest = beta*dest + alpha*xmm0
       {$IFDEF FPC}vmovsd xmm5, [ecx];              {$ELSE}db $C5,$FB,$10,$29;{$ENDIF} // first element
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm5;        {$ELSE}db $C5,$F9,$7C,$C5;{$ENDIF} // calculate xmm0_1 + xmm0_2 (store low) xmm5_1 + xmm5_2 (store high)
       {$IFDEF FPC}vmulpd xmm0, xmm0, xmm6;         {$ELSE}db $C5,$F9,$59,$C6;{$ENDIF} // beta * dest + alpha*xmm0
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm0;        {$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} // final add
       {$IFDEF FPC}vmovsd [ecx], xmm0;              {$ELSE}db $C5,$FB,$11,$01;{$ENDIF} // store back
       add ecx, destLineWidth;

       {$IFDEF FPC}vmovsd xmm5, [ecx];{$ELSE}db $C5,$FB,$10,$29;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm1, xmm1, xmm5;{$ELSE}db $C5,$F1,$7C,$CD;{$ENDIF} 
       {$IFDEF FPC}vmulpd xmm1, xmm1, xmm6;{$ELSE}db $C5,$F1,$59,$CE;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm1, xmm1, xmm1;{$ELSE}db $C5,$F1,$7C,$C9;{$ENDIF} 
       {$IFDEF FPC}vmovsd [ecx], xmm1;{$ELSE}db $C5,$FB,$11,$09;{$ENDIF} 
       add ecx, destLineWidth;

       {$IFDEF FPC}vmovsd xmm5, [ecx];{$ELSE}db $C5,$FB,$10,$29;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm2, xmm2, xmm5;{$ELSE}db $C5,$E9,$7C,$D5;{$ENDIF} 
       {$IFDEF FPC}vmulpd xmm2, xmm2, xmm6;{$ELSE}db $C5,$E9,$59,$D6;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm2, xmm2, xmm2;{$ELSE}db $C5,$E9,$7C,$D2;{$ENDIF} 
       {$IFDEF FPC}vmovsd [ecx], xmm2;{$ELSE}db $C5,$FB,$11,$11;{$ENDIF} 
       add ecx, destLineWidth;

       {$IFDEF FPC}vmovsd xmm5, [ecx];{$ELSE}db $C5,$FB,$10,$29;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm3, xmm3, xmm5;{$ELSE}db $C5,$E1,$7C,$DD;{$ENDIF} 
       {$IFDEF FPC}vmulpd xmm3, xmm3, xmm6;{$ELSE}db $C5,$E1,$59,$DE;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm3, xmm3, xmm3;{$ELSE}db $C5,$E1,$7C,$DB;{$ENDIF} 
       {$IFDEF FPC}vmovsd [ecx], xmm3;{$ELSE}db $C5,$FB,$11,$19;{$ENDIF} 
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
       {$IFDEF FPC}vxorpd ymm0, ymm0, ymm0;{$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} 
       mov eax, mt1;       // eax = first matrix element
       mov ebx, v;       // ebx = first vector element

       mov edx, width;      // r10 = width - 4
       cmp edx, 0;
       jl @@shortloopend;

       @@forxshortloop:
           {$IFDEF FPC}vmovapd ymm4, [ebx];{$ELSE}db $C5,$FD,$28,$23;{$ENDIF} 
           {$IFDEF FPC}vfmadd231pd ymm0, ymm4, [eax];{$ELSE}db $C4,$E2,$DD,$B8,$00;{$ENDIF} 

           add eax, 32;
           add ebx, 32;
       sub edx, 4;
       jge @@forxshortloop;

       {$IFDEF FPC}vextractf128 xmm4, ymm0, 1;{$ELSE}db $C4,$E3,$7D,$19,$C4,$01;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm4;{$ELSE}db $C5,$F9,$7C,$C4;{$ENDIF} 

       @@shortloopend:

       add edx, 4;
       // test if there are elements left
       jz @@resbuildshort;
       @@forxshortestloop:
           {$IFDEF FPC}vmovsd xmm4, [ebx];{$ELSE}db $C5,$FB,$10,$23;{$ENDIF} 
           {$IFDEF FPC}vmovsd xmm5, [eax];{$ELSE}db $C5,$FB,$10,$28;{$ENDIF} 

           {$IFDEF FPC}vmulsd xmm5, xmm5, xmm4;{$ELSE}db $C5,$D3,$59,$EC;{$ENDIF} 
           {$IFDEF FPC}vaddsd xmm0, xmm0, xmm5;{$ELSE}db $C5,$FB,$58,$C5;{$ENDIF} 

           add eax, 8;
           add ebx, 8;
       dec edx;
       jnz @@forxshortestloop;

       @@resbuildshort:

       // build result

       // calculate dest = beta*dest + alpha*xmm0
       {$IFDEF FPC}vmovsd xmm5, [ecx];{$ELSE}db $C5,$FB,$10,$29;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm5;{$ELSE}db $C5,$F9,$7C,$C5;{$ENDIF} 
       {$IFDEF FPC}vmulpd xmm0, xmm0, xmm6;{$ELSE}db $C5,$F9,$59,$C6;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} 
       {$IFDEF FPC}vmovsd [ecx], xmm0;{$ELSE}db $C5,$FB,$11,$01;{$ENDIF} 

       add ecx, destLineWidth;
       mov eax, mt1;
       add eax, esi;
       mov mt1, eax;

   dec height;
   jnz @@foryshortloop;

   @@vecend:

   // epilog pop "stack"
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 
   pop esi;
   pop edi;
   pop ebx;
end;
end;

procedure FMAMatrixVectMultUnAlignedVAligned(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);
begin
asm
   // prolog - simulate stack
   push ebx;
   push edi;
   push esi;

   // for the final multiplication
   lea ebx, alpha;
   {$IFDEF FPC}vmovsd xmm6, [ebx];{$ELSE}db $C5,$FB,$10,$33;{$ENDIF} 
   lea eax, beta;
   {$IFDEF FPC}vmovhpd xmm6, xmm6, [eax];{$ELSE}db $C5,$C9,$16,$30;{$ENDIF} 

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
       {$IFDEF FPC}vxorpd ymm0, ymm0, ymm0;{$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} 
       {$IFDEF FPC}vxorpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$57,$C9;{$ENDIF} 
       {$IFDEF FPC}vxorpd ymm2, ymm2, ymm2;{$ELSE}db $C5,$ED,$57,$D2;{$ENDIF} 
       {$IFDEF FPC}vxorpd ymm3, ymm3, ymm3;  {$ELSE}db $C5,$E5,$57,$DB;{$ENDIF} // res := 0;
       mov eax, mt1;       // eax = first matrix element
       mov ebx, v;       // ebx = first vector element

       mov edx, width;      // edx = width
       @@forxloop:
           {$IFDEF FPC}vmovupd ymm4, [ebx];{$ELSE}db $C5,$FD,$10,$23;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm5, [eax];{$ELSE}db $C5,$FD,$10,$28;{$ENDIF} 
           {$IFDEF FPC}vfmadd231pd ymm0, ymm4, ymm5;{$ELSE}db $C4,$E2,$DD,$B8,$C5;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm5, [eax + esi];{$ELSE}db $C5,$FD,$10,$2C,$30;{$ENDIF} 
           {$IFDEF FPC}vfmadd231pd ymm1, ymm4, ymm5;{$ELSE}db $C4,$E2,$DD,$B8,$CD;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm5, [eax + 2*esi];{$ELSE}db $C5,$FD,$10,$2C,$70;{$ENDIF} 
           {$IFDEF FPC}vfmadd231pd ymm2, ymm4, ymm5;{$ELSE}db $C4,$E2,$DD,$B8,$D5;{$ENDIF} 

           add eax, esi;
           {$IFDEF FPC}vmovupd ymm5, [eax + 2*esi];{$ELSE}db $C5,$FD,$10,$2C,$70;{$ENDIF} 
           {$IFDEF FPC}vfmadd231pd ymm3, ymm4, ymm5;{$ELSE}db $C4,$E2,$DD,$B8,$DD;{$ENDIF} 
           sub eax, esi;

           add eax, 32;
           add ebx, 32;

       sub edx, 4;
       jge @@forxloop;

       {$IFDEF FPC}vextractf128 xmm4, ymm0, 1;{$ELSE}db $C4,$E3,$7D,$19,$C4,$01;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm4;{$ELSE}db $C5,$F9,$7C,$C4;{$ENDIF} 
       {$IFDEF FPC}vextractf128 xmm5, ymm1, 1;{$ELSE}db $C4,$E3,$7D,$19,$CD,$01;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm1, xmm1, xmm5;{$ELSE}db $C5,$F1,$7C,$CD;{$ENDIF} 
       {$IFDEF FPC}vextractf128 xmm4, ymm2, 1;{$ELSE}db $C4,$E3,$7D,$19,$D4,$01;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm2, xmm2, xmm4;{$ELSE}db $C5,$E9,$7C,$D4;{$ENDIF} 
       {$IFDEF FPC}vextractf128 xmm5, ymm3, 1;{$ELSE}db $C4,$E3,$7D,$19,$DD,$01;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm3, xmm3, xmm5;{$ELSE}db $C5,$E1,$7C,$DD;{$ENDIF} 

       // special treatment for the last value(s):
       add edx, 4;
       jz @@resbuild;

       @@shortloopx:
          {$IFDEF FPC}vmovsd xmm4, [ebx];{$ELSE}db $C5,$FB,$10,$23;{$ENDIF} 
          {$IFDEF FPC}vmovsd xmm5, [eax];{$ELSE}db $C5,$FB,$10,$28;{$ENDIF} 

          {$IFDEF FPC}vmulsd xmm5, xmm5, xmm4;{$ELSE}db $C5,$D3,$59,$EC;{$ENDIF} 
          {$IFDEF FPC}vaddsd xmm0, xmm0, xmm5;{$ELSE}db $C5,$FB,$58,$C5;{$ENDIF} 

          {$IFDEF FPC}vmovsd xmm5, [eax + esi];{$ELSE}db $C5,$FB,$10,$2C,$30;{$ENDIF} 

          {$IFDEF FPC}vmulsd xmm5, xmm5, xmm4;{$ELSE}db $C5,$D3,$59,$EC;{$ENDIF} 
          {$IFDEF FPC}vaddsd xmm1, xmm1, xmm5;{$ELSE}db $C5,$F3,$58,$CD;{$ENDIF} 

          {$IFDEF FPC}vmovsd xmm5, [eax + 2*esi];{$ELSE}db $C5,$FB,$10,$2C,$70;{$ENDIF} 

          {$IFDEF FPC}vmulsd xmm5, xmm5, xmm4;{$ELSE}db $C5,$D3,$59,$EC;{$ENDIF} 
          {$IFDEF FPC}vaddsd xmm2, xmm2, xmm5;{$ELSE}db $C5,$EB,$58,$D5;{$ENDIF} 

          add eax, esi;
          {$IFDEF FPC}vmovsd xmm5, [eax + 2*esi];{$ELSE}db $C5,$FB,$10,$2C,$70;{$ENDIF} 
          sub eax, esi;

          {$IFDEF FPC}vmulsd xmm5, xmm5, xmm4;{$ELSE}db $C5,$D3,$59,$EC;{$ENDIF} 
          {$IFDEF FPC}vaddsd xmm3, xmm3, xmm5;{$ELSE}db $C5,$E3,$58,$DD;{$ENDIF} 

          add eax, 8;
          add ebx, 8;
       sub edx, 1;
       jnz @@shortloopx;


       @@resbuild:
       // result building
       // write back result (final addition and compactation)

       // calculate dest = beta*dest + alpha*xmm0
       {$IFDEF FPC}vmovsd xmm5, [ecx];              {$ELSE}db $C5,$FB,$10,$29;{$ENDIF} // first element
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm5;        {$ELSE}db $C5,$F9,$7C,$C5;{$ENDIF} // calculate xmm0_1 + xmm0_2 (store low) xmm5_1 + xmm5_2 (store high)
       {$IFDEF FPC}vmulpd xmm0, xmm0, xmm6;         {$ELSE}db $C5,$F9,$59,$C6;{$ENDIF} // beta * dest + alpha*xmm0
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm0;        {$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} // final add
       {$IFDEF FPC}vmovsd [ecx], xmm0;              {$ELSE}db $C5,$FB,$11,$01;{$ENDIF} // store back
       add ecx, destLineWidth;

       {$IFDEF FPC}vmovsd xmm5, [ecx];{$ELSE}db $C5,$FB,$10,$29;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm1, xmm1, xmm5;{$ELSE}db $C5,$F1,$7C,$CD;{$ENDIF} 
       {$IFDEF FPC}vmulpd xmm1, xmm1, xmm6;{$ELSE}db $C5,$F1,$59,$CE;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm1, xmm1, xmm1;{$ELSE}db $C5,$F1,$7C,$C9;{$ENDIF} 
       {$IFDEF FPC}vmovsd [ecx], xmm1;{$ELSE}db $C5,$FB,$11,$09;{$ENDIF} 
       add ecx, destLineWidth;

       {$IFDEF FPC}vmovsd xmm5, [ecx];{$ELSE}db $C5,$FB,$10,$29;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm2, xmm2, xmm5;{$ELSE}db $C5,$E9,$7C,$D5;{$ENDIF} 
       {$IFDEF FPC}vmulpd xmm2, xmm2, xmm6;{$ELSE}db $C5,$E9,$59,$D6;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm2, xmm2, xmm2;{$ELSE}db $C5,$E9,$7C,$D2;{$ENDIF} 
       {$IFDEF FPC}vmovsd [ecx], xmm2;{$ELSE}db $C5,$FB,$11,$11;{$ENDIF} 
       add ecx, destLineWidth;

       {$IFDEF FPC}vmovsd xmm5, [ecx];{$ELSE}db $C5,$FB,$10,$29;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm3, xmm3, xmm5;{$ELSE}db $C5,$E1,$7C,$DD;{$ENDIF} 
       {$IFDEF FPC}vmulpd xmm3, xmm3, xmm6;{$ELSE}db $C5,$E1,$59,$DE;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm3, xmm3, xmm3;{$ELSE}db $C5,$E1,$7C,$DB;{$ENDIF} 
       {$IFDEF FPC}vmovsd [ecx], xmm3;{$ELSE}db $C5,$FB,$11,$19;{$ENDIF} 
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
       {$IFDEF FPC}vxorpd ymm0, ymm0, ymm0;{$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} 
       mov eax, mt1;       // eax = first matrix element
       mov ebx, v;       // ebx = first vector element

       mov edx, width;      // edx = width - 4
       cmp edx, 0;
       jl @@shortloopend;

       @@forxshortloop:
           {$IFDEF FPC}vmovupd ymm4, [ebx];{$ELSE}db $C5,$FD,$10,$23;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm5, [eax];{$ELSE}db $C5,$FD,$10,$28;{$ENDIF} 
           {$IFDEF FPC}vfmadd231pd ymm0, ymm4, ymm5;{$ELSE}db $C4,$E2,$DD,$B8,$C5;{$ENDIF} 

           add eax, 32;
           add ebx, 32;
       sub edx, 4;
       jge @@forxshortloop;

       {$IFDEF FPC}vextractf128 xmm4, ymm0, 1;{$ELSE}db $C4,$E3,$7D,$19,$C4,$01;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm4;{$ELSE}db $C5,$F9,$7C,$C4;{$ENDIF} 

       @@shortloopend:

       add edx, 4;
       // test if there are elements left
       jz @@resbuildshort;
       @@forxshortestloop:
           {$IFDEF FPC}vmovsd xmm4, [ebx];{$ELSE}db $C5,$FB,$10,$23;{$ENDIF} 
           {$IFDEF FPC}vmovsd xmm5, [eax];{$ELSE}db $C5,$FB,$10,$28;{$ENDIF} 

           {$IFDEF FPC}vmulsd xmm5, xmm5, xmm4;{$ELSE}db $C5,$D3,$59,$EC;{$ENDIF} 
           {$IFDEF FPC}vaddsd xmm0, xmm0, xmm5;{$ELSE}db $C5,$FB,$58,$C5;{$ENDIF} 

           add eax, 8;
           add ebx, 8;
       dec edx;
       jnz @@forxshortestloop;

       @@resbuildshort:

       // build result

       // calculate dest = beta*dest + alpha*xmm0
       {$IFDEF FPC}vmovsd xmm5, [ecx];{$ELSE}db $C5,$FB,$10,$29;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm5;{$ELSE}db $C5,$F9,$7C,$C5;{$ENDIF} 
       {$IFDEF FPC}vmulpd xmm0, xmm0, xmm6;{$ELSE}db $C5,$F9,$59,$C6;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} 
       {$IFDEF FPC}vmovsd [ecx], xmm0;{$ELSE}db $C5,$FB,$11,$01;{$ENDIF} 

       add ecx, destLineWidth;
       add mt1, esi;
   dec height;
   jnz @@foryshortloop;

   @@vecend:

   // epilog pop "stack"
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 
   pop esi;
   pop edi;
   pop ebx;
end;
end;

// this function is not that well suited for use of simd instructions...
// so only this version exists
procedure FMAMatrixVectMultT(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);
// var dymm8, dymm9, dymm10, dymm11 : TYMMArr;
begin
asm
   // prolog - simulate stack
   push ebx;
   push edi;
   push esi;

   // reserve memory on the stack for dymm8 to dymm11
   sub esp, 4*4*8;

   // for the final multiplication
   lea eax, alpha;
   {$IFDEF FPC}vbroadcastsd ymm6, [eax];{$ELSE}db $C4,$E2,$7D,$19,$30;{$ENDIF} 
   lea eax, beta;
   {$IFDEF FPC}vbroadcastsd ymm7, [eax];{$ELSE}db $C4,$E2,$7D,$19,$38;{$ENDIF} 

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
       {$IFDEF FPC}vxorpd ymm0, ymm0, ymm0;{$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} 
       {$IFDEF FPC}vmovupd [esp + 128], ymm0;{$ELSE}db $C5,$FD,$11,$44,$24,$80;{$ENDIF}
       {$IFDEF FPC}vmovupd [esp + 96], ymm0;{$ELSE}db $C5,$FD,$11,$44,$24,$A0;{$ENDIF}
       {$IFDEF FPC}vmovupd [esp + 64], ymm0;{$ELSE}db $C5,$FD,$11,$44,$24,$C0;{$ENDIF}
       {$IFDEF FPC}vmovupd [esp + 32], ymm0;{$ELSE}db $C5,$FD,$11,$44,$24,$E0;{$ENDIF}

       mov eax, mt1;       // eax = first matrix element
       mov ebx, V;       // ebx = first vector element

       mov edx, height;
       @@foryloop:
           {$IFDEF FPC}vbroadcastsd ymm3, [ebx];{$ELSE}db $C4,$E2,$7D,$19,$1B;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm0, [esp + 128];{$ELSE}db $C5,$FD,$10,$44,$24,$80;{$ENDIF}
           {$IFDEF FPC}vmovupd ymm4, [eax];{$ELSE}db $C5,$FD,$10,$20;{$ENDIF} 
           {$IFDEF FPC}vfmadd231pd ymm0, ymm3, ymm4;{$ELSE}db $C4,$E2,$E5,$B8,$C4;{$ENDIF} 
           {$IFDEF FPC}vmovupd [esp + 128], ymm0;{$ELSE}db $C5,$FD,$11,$44,$24,$80;{$ENDIF}

           {$IFDEF FPC}vmovupd ymm1, [esp + 96];{$ELSE}db $C5,$FD,$10,$4C,$24,$A0;{$ENDIF}
           {$IFDEF FPC}vmovupd ymm4, [eax + 32];{$ELSE}db $C5,$FD,$10,$60,$20;{$ENDIF} 
           {$IFDEF FPC}vfmadd231pd ymm1, ymm3, ymm4;{$ELSE}db $C4,$E2,$E5,$B8,$CC;{$ENDIF} 
           {$IFDEF FPC}vmovupd [esp + 96], ymm1;{$ELSE}db $C5,$FD,$11,$4C,$24,$A0;{$ENDIF}

           {$IFDEF FPC}vmovupd ymm0, [esp + 64];{$ELSE}db $C5,$FD,$10,$44,$24,$C0;{$ENDIF}
           {$IFDEF FPC}vmovupd ymm4, [eax + 64];{$ELSE}db $C5,$FD,$10,$60,$40;{$ENDIF} 
           {$IFDEF FPC}vfmadd231pd ymm0, ymm3, ymm4;{$ELSE}db $C4,$E2,$E5,$B8,$C4;{$ENDIF} 
           {$IFDEF FPC}vmovupd [esp + 64], ymm0;{$ELSE}db $C5,$FD,$11,$44,$24,$C0;{$ENDIF}

           {$IFDEF FPC}vmovupd ymm1, [esp + 32];{$ELSE}db $C5,$FD,$10,$4C,$24,$E0;{$ENDIF}
           {$IFDEF FPC}vmovupd ymm4, [eax + 96];{$ELSE}db $C5,$FD,$10,$60,$60;{$ENDIF} 
           {$IFDEF FPC}vfmadd231pd ymm1, ymm3, ymm4;{$ELSE}db $C4,$E2,$E5,$B8,$CC;{$ENDIF} 
           {$IFDEF FPC}vmovupd [esp + 32], ymm1;{$ELSE}db $C5,$FD,$11,$4C,$24,$E0;{$ENDIF}

           add eax, esi;
           add ebx, edi;

       dec edx;
       jnz @@foryloop;

       // result building
       // write back result (final addition and compactation)

       // calculate dest = beta*dest + alpha*xmm0
       // first two
       mov edx, destLineWidth;
       {$IFDEF FPC}vmovsd xmm3, [ecx];{$ELSE}db $C5,$FB,$10,$19;{$ENDIF} 
       {$IFDEF FPC}vmovsd xmm4, [ecx + edx];{$ELSE}db $C5,$FB,$10,$24,$11;{$ENDIF} 
       {$IFDEF FPC}vmovlhps xmm3, xmm3, xmm4;{$ELSE}db $C5,$E0,$16,$DC;{$ENDIF} 

       {$IFDEF FPC}vmovupd xmm0, [esp + 128];{$ELSE}db $C5,$F9,$10,$44,$24,$80;{$ENDIF}
       {$IFDEF FPC}vmulpd xmm0, xmm0, xmm6;   {$ELSE}db $C5,$F9,$59,$C6;{$ENDIF} // alpha*res
       {$IFDEF FPC}vmulpd xmm3, xmm3, xmm7;   {$ELSE}db $C5,$E1,$59,$DF;{$ENDIF} // dest*beta

       {$IFDEF FPC}vaddpd xmm3, xmm3, xmm0;{$ELSE}db $C5,$E1,$58,$D8;{$ENDIF} 
       {$IFDEF FPC}vmovhlps xmm4, xmm4, xmm3;{$ELSE}db $C5,$D8,$12,$E3;{$ENDIF} 
       {$IFDEF FPC}vmovsd [ecx], xmm3;{$ELSE}db $C5,$FB,$11,$19;{$ENDIF} 
       {$IFDEF FPC}vmovsd [ecx + edx], xmm4;{$ELSE}db $C5,$FB,$11,$24,$11;{$ENDIF} 
       add ecx, edx;
       add ecx, edx;

       // second two
       {$IFDEF FPC}vmovsd xmm3, [ecx];{$ELSE}db $C5,$FB,$10,$19;{$ENDIF} 
       {$IFDEF FPC}vmovsd xmm4, [ecx + edx];{$ELSE}db $C5,$FB,$10,$24,$11;{$ENDIF} 
       {$IFDEF FPC}vmovlhps xmm3, xmm3, xmm4;{$ELSE}db $C5,$E0,$16,$DC;{$ENDIF} 

       //movupd xmm0, res1;
       {$IFDEF FPC}vmovupd xmm0, [esp + 112];{$ELSE}db $C5,$F9,$10,$44,$24,$90;{$ENDIF}
       {$IFDEF FPC}vmulpd xmm0, xmm0, xmm6;   {$ELSE}db $C5,$F9,$59,$C6;{$ENDIF} // alpha*res
       {$IFDEF FPC}vmulpd xmm3, xmm3, xmm7;   {$ELSE}db $C5,$E1,$59,$DF;{$ENDIF} // dest*beta

       {$IFDEF FPC}vaddpd xmm3, xmm3, xmm0;{$ELSE}db $C5,$E1,$58,$D8;{$ENDIF} 
       {$IFDEF FPC}vmovhlps xmm4, xmm4, xmm3;{$ELSE}db $C5,$D8,$12,$E3;{$ENDIF} 
       {$IFDEF FPC}vmovsd [ecx], xmm3;{$ELSE}db $C5,$FB,$11,$19;{$ENDIF} 
       {$IFDEF FPC}vmovsd [ecx + edx], xmm4;{$ELSE}db $C5,$FB,$11,$24,$11;{$ENDIF} 

       add ecx, edx;
       add ecx, edx;

       // third two
       {$IFDEF FPC}vmovsd xmm3, [ecx];{$ELSE}db $C5,$FB,$10,$19;{$ENDIF} 
       {$IFDEF FPC}vmovsd xmm4, [ecx + edx];{$ELSE}db $C5,$FB,$10,$24,$11;{$ENDIF} 
       {$IFDEF FPC}vmovlhps xmm3, xmm3, xmm4;{$ELSE}db $C5,$E0,$16,$DC;{$ENDIF} 

       {$IFDEF FPC}vmovupd xmm0, [esp + 96];{$ELSE}db $C5,$F9,$10,$44,$24,$A0;{$ENDIF}
       {$IFDEF FPC}vmulpd xmm0, xmm0, xmm6;   {$ELSE}db $C5,$F9,$59,$C6;{$ENDIF} // alpha*res
       {$IFDEF FPC}vmulpd xmm3, xmm3, xmm7;   {$ELSE}db $C5,$E1,$59,$DF;{$ENDIF} // dest*beta

       {$IFDEF FPC}vaddpd xmm3, xmm3, xmm0;{$ELSE}db $C5,$E1,$58,$D8;{$ENDIF} 
       {$IFDEF FPC}vmovhlps xmm4, xmm4, xmm3;{$ELSE}db $C5,$D8,$12,$E3;{$ENDIF} 
       {$IFDEF FPC}vmovsd [ecx], xmm3;{$ELSE}db $C5,$FB,$11,$19;{$ENDIF} 
       {$IFDEF FPC}vmovsd [ecx + edx], xmm4;{$ELSE}db $C5,$FB,$11,$24,$11;{$ENDIF} 

       add ecx, edx;
       add ecx, edx;

       // forth two
       {$IFDEF FPC}vmovsd xmm3, [ecx];{$ELSE}db $C5,$FB,$10,$19;{$ENDIF} 
       {$IFDEF FPC}vmovsd xmm4, [ecx + edx];{$ELSE}db $C5,$FB,$10,$24,$11;{$ENDIF} 
       {$IFDEF FPC}vmovlhps xmm3, xmm3, xmm4;{$ELSE}db $C5,$E0,$16,$DC;{$ENDIF} 

       //movupd xmm0, res3;
       {$IFDEF FPC}vmovupd xmm0, [esp + 80];{$ELSE}db $C5,$F9,$10,$44,$24,$B0;{$ENDIF}
       {$IFDEF FPC}vmulpd xmm0, xmm0, xmm6;   {$ELSE}db $C5,$F9,$59,$C6;{$ENDIF} // alpha*res
       {$IFDEF FPC}vmulpd xmm3, xmm3, xmm7;   {$ELSE}db $C5,$E1,$59,$DF;{$ENDIF} // dest*beta

       {$IFDEF FPC}vaddpd xmm3, xmm3, xmm0;{$ELSE}db $C5,$E1,$58,$D8;{$ENDIF} 
       {$IFDEF FPC}vmovhlps xmm4, xmm4, xmm3;{$ELSE}db $C5,$D8,$12,$E3;{$ENDIF} 
       {$IFDEF FPC}vmovsd [ecx], xmm3;{$ELSE}db $C5,$FB,$11,$19;{$ENDIF} 
       {$IFDEF FPC}vmovsd [ecx + edx], xmm4;{$ELSE}db $C5,$FB,$11,$24,$11;{$ENDIF} 

       add ecx, edx;
       add ecx, edx;

       // fith two
       {$IFDEF FPC}vmovsd xmm3, [ecx];{$ELSE}db $C5,$FB,$10,$19;{$ENDIF} 
       {$IFDEF FPC}vmovsd xmm4, [ecx + edx];{$ELSE}db $C5,$FB,$10,$24,$11;{$ENDIF} 
       {$IFDEF FPC}vmovlhps xmm3, xmm3, xmm4;{$ELSE}db $C5,$E0,$16,$DC;{$ENDIF} 

       {$IFDEF FPC}vmovupd xmm0, [esp + 64];{$ELSE}db $C5,$F9,$10,$44,$24,$C0;{$ENDIF}
       {$IFDEF FPC}vmulpd xmm0, xmm0, xmm6;   {$ELSE}db $C5,$F9,$59,$C6;{$ENDIF} // alpha*res
       {$IFDEF FPC}vmulpd xmm3, xmm3, xmm7;   {$ELSE}db $C5,$E1,$59,$DF;{$ENDIF} // dest*beta

       {$IFDEF FPC}vaddpd xmm3, xmm3, xmm0;{$ELSE}db $C5,$E1,$58,$D8;{$ENDIF} 
       {$IFDEF FPC}vmovhlps xmm4, xmm4, xmm3;{$ELSE}db $C5,$D8,$12,$E3;{$ENDIF} 
       {$IFDEF FPC}vmovsd [ecx], xmm3;{$ELSE}db $C5,$FB,$11,$19;{$ENDIF} 
       {$IFDEF FPC}vmovsd [ecx + edx], xmm4;{$ELSE}db $C5,$FB,$11,$24,$11;{$ENDIF} 

       add ecx, edx;
       add ecx, edx;

       // sixth two
       {$IFDEF FPC}vmovsd xmm3, [ecx];{$ELSE}db $C5,$FB,$10,$19;{$ENDIF} 
       {$IFDEF FPC}vmovsd xmm4, [ecx + edx];{$ELSE}db $C5,$FB,$10,$24,$11;{$ENDIF} 
       {$IFDEF FPC}vmovlhps xmm3, xmm3, xmm4;{$ELSE}db $C5,$E0,$16,$DC;{$ENDIF} 

       //movupd xmm0, res5;
       {$IFDEF FPC}vmovupd xmm0, [esp + 48];{$ELSE}db $C5,$F9,$10,$44,$24,$D0;{$ENDIF}
       {$IFDEF FPC}vmulpd xmm0, xmm0, xmm6;   {$ELSE}db $C5,$F9,$59,$C6;{$ENDIF} // alpha*res
       {$IFDEF FPC}vmulpd xmm3, xmm3, xmm7;   {$ELSE}db $C5,$E1,$59,$DF;{$ENDIF} // dest*beta

       {$IFDEF FPC}vaddpd xmm3, xmm3, xmm0;{$ELSE}db $C5,$E1,$58,$D8;{$ENDIF} 
       {$IFDEF FPC}vmovhlps xmm4, xmm4, xmm3;{$ELSE}db $C5,$D8,$12,$E3;{$ENDIF} 
       {$IFDEF FPC}vmovsd [ecx], xmm3;{$ELSE}db $C5,$FB,$11,$19;{$ENDIF} 
       {$IFDEF FPC}vmovsd [ecx + edx], xmm4;{$ELSE}db $C5,$FB,$11,$24,$11;{$ENDIF} 

       add ecx, edx;
       add ecx, edx;

       // seventh two
       {$IFDEF FPC}vmovsd xmm3, [ecx];{$ELSE}db $C5,$FB,$10,$19;{$ENDIF} 
       {$IFDEF FPC}vmovsd xmm4, [ecx + edx];{$ELSE}db $C5,$FB,$10,$24,$11;{$ENDIF} 
       {$IFDEF FPC}vmovlhps xmm3, xmm3, xmm4;{$ELSE}db $C5,$E0,$16,$DC;{$ENDIF} 

       {$IFDEF FPC}vmovupd xmm0, [esp + 32];{$ELSE}db $C5,$F9,$10,$44,$24,$E0;{$ENDIF}
       {$IFDEF FPC}vmulpd xmm0, xmm0, xmm6;   {$ELSE}db $C5,$F9,$59,$C6;{$ENDIF} // alpha*res
       {$IFDEF FPC}vmulpd xmm3, xmm3, xmm7;   {$ELSE}db $C5,$E1,$59,$DF;{$ENDIF} // dest*beta

       {$IFDEF FPC}vaddpd xmm3, xmm3, xmm0;{$ELSE}db $C5,$E1,$58,$D8;{$ENDIF} 
       {$IFDEF FPC}vmovhlps xmm4, xmm4, xmm3;{$ELSE}db $C5,$D8,$12,$E3;{$ENDIF} 
       {$IFDEF FPC}vmovsd [ecx], xmm3;{$ELSE}db $C5,$FB,$11,$19;{$ENDIF} 
       {$IFDEF FPC}vmovsd [ecx + edx], xmm4;{$ELSE}db $C5,$FB,$11,$24,$11;{$ENDIF} 

       add ecx, edx;
       add ecx, edx;

       // eighth two
       {$IFDEF FPC}vmovsd xmm3, [ecx];{$ELSE}db $C5,$FB,$10,$19;{$ENDIF} 
       {$IFDEF FPC}vmovsd xmm4, [ecx + edx];{$ELSE}db $C5,$FB,$10,$24,$11;{$ENDIF} 
       {$IFDEF FPC}vmovlhps xmm3, xmm3, xmm4;{$ELSE}db $C5,$E0,$16,$DC;{$ENDIF} 

       //movupd xmm0, res7;
       {$IFDEF FPC}vmovupd xmm0, [esp + 16];{$ELSE}db $C5,$F9,$10,$44,$24,$F0;{$ENDIF}
       {$IFDEF FPC}vmulpd xmm0, xmm0, xmm6;   {$ELSE}db $C5,$F9,$59,$C6;{$ENDIF} // alpha*res
       {$IFDEF FPC}vmulpd xmm3, xmm3, xmm7;   {$ELSE}db $C5,$E1,$59,$DF;{$ENDIF} // dest*beta

       {$IFDEF FPC}vaddpd xmm3, xmm3, xmm0;{$ELSE}db $C5,$E1,$58,$D8;{$ENDIF} 
       {$IFDEF FPC}vmovhlps xmm4, xmm4, xmm3;{$ELSE}db $C5,$D8,$12,$E3;{$ENDIF} 
       {$IFDEF FPC}vmovsd [ecx], xmm3;{$ELSE}db $C5,$FB,$11,$19;{$ENDIF} 
       {$IFDEF FPC}vmovsd [ecx + edx], xmm4;{$ELSE}db $C5,$FB,$11,$24,$11;{$ENDIF} 

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
       {$IFDEF FPC}vxorpd xmm0, xmm0, xmm0;  {$ELSE}db $C5,$F9,$57,$C0;{$ENDIF} // first two elements

       mov eax, mt1;       // eax = first matrix element
       mov ebx, v;       // ebx = first vector element

       mov edx, height;

       @@forshortyloop:
           {$IFDEF FPC}vmovsd xmm1, [eax];{$ELSE}db $C5,$FB,$10,$08;{$ENDIF} 
           {$IFDEF FPC}vmovsd xmm2, [ebx];{$ELSE}db $C5,$FB,$10,$13;{$ENDIF} 

           {$IFDEF FPC}vmulsd xmm1, xmm1, xmm2;{$ELSE}db $C5,$F3,$59,$CA;{$ENDIF} 
           {$IFDEF FPC}vaddsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$58,$C1;{$ENDIF} 

           add eax, esi;
           add ebx, edi;

       dec edx;
       jnz @@forshortyloop;

       {$IFDEF FPC}vmulsd xmm0, xmm0, xmm6;  {$ELSE}db $C5,$FB,$59,$C6;{$ENDIF} // alpha*res

       {$IFDEF FPC}vmovsd xmm3, [ecx];{$ELSE}db $C5,$FB,$10,$19;{$ENDIF} 
       {$IFDEF FPC}vmulsd xmm3, xmm3, xmm7;  {$ELSE}db $C5,$E3,$59,$DF;{$ENDIF} //dest*beta
       {$IFDEF FPC}vaddsd xmm0, xmm0, xmm3;{$ELSE}db $C5,$FB,$58,$C3;{$ENDIF} 
       {$IFDEF FPC}vmovsd [ecx], xmm0;{$ELSE}db $C5,$FB,$11,$01;{$ENDIF} 

       // next row
       add ecx, destLineWidth;
       mov eax, mt1;
       add eax, 8;
       mov mt1, eax;

   dec width;
   jnz @@forxshortloop;


   @@vecaddend:

   // epilog pop "stack"
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   // free reserved stack space
   add esp, 4*4*8;
   pop esi;
   pop edi;
   pop ebx;
end;
end;


procedure FMAMatrixVecMultTDestVec(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);
begin
asm
   // prolog - simulate stack
   push ebx;
   push edi;
   push esi;

   // simulate local vars dymm8 - dymm11
   sub esp, 4*4*8;  // 4*sizeof ymm register

   // for the final multiplication
   lea eax, alpha;
   {$IFDEF FPC}vbroadcastsd ymm6, [eax];{$ELSE}db $C4,$E2,$7D,$19,$30;{$ENDIF} 
   lea eax, beta;
   {$IFDEF FPC}vbroadcastsd ymm7, [eax];{$ELSE}db $C4,$E2,$7D,$19,$38;{$ENDIF} 

   // prepare for loop
   mov ecx, dest;
   mov esi, LineWidthMT;
   mov edi, LineWidthV;

   sub width, 16;
   js @@forxloopend;

   // init for x := 0 to width - 1:
   @@forxloop:

       // init values:
       {$IFDEF FPC}vxorpd ymm0, ymm0, ymm0;{$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} 
       {$IFDEF FPC}vmovupd [esp + 128], ymm0;{$ELSE}db $C5,$FD,$11,$44,$24,$80;{$ENDIF}
       {$IFDEF FPC}vmovupd [esp + 96], ymm0;{$ELSE}db $C5,$FD,$11,$44,$24,$A0;{$ENDIF}
       {$IFDEF FPC}vmovupd [esp + 64], ymm0;{$ELSE}db $C5,$FD,$11,$44,$24,$C0;{$ENDIF}
       {$IFDEF FPC}vmovupd [esp + 32], ymm0;{$ELSE}db $C5,$FD,$11,$44,$24,$E0;{$ENDIF}

       mov eax, mt1;       // eax = first matrix element
       mov ebx, v;         // ebx = first vector element

       mov edx, height;
       @@foryloop:
           {$IFDEF FPC}vbroadcastsd ymm3, [ebx];{$ELSE}db $C4,$E2,$7D,$19,$1B;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm0, [esp + 128];{$ELSE}db $C5,$FD,$10,$44,$24,$80;{$ENDIF}
           {$IFDEF FPC}vmovupd ymm4, [eax];{$ELSE}db $C5,$FD,$10,$20;{$ENDIF} 
           {$IFDEF FPC}vfmadd231pd ymm0, ymm3, ymm4;{$ELSE}db $C4,$E2,$E5,$B8,$C4;{$ENDIF} 
           {$IFDEF FPC}vmovupd [esp + 128], ymm0;{$ELSE}db $C5,$FD,$11,$44,$24,$80;{$ENDIF}

           {$IFDEF FPC}vmovupd ymm1, [esp + 96];{$ELSE}db $C5,$FD,$10,$4C,$24,$A0;{$ENDIF}
           {$IFDEF FPC}vmovupd ymm4, [eax + 32];{$ELSE}db $C5,$FD,$10,$60,$20;{$ENDIF} 
           {$IFDEF FPC}vfmadd231pd ymm1, ymm3, ymm4;{$ELSE}db $C4,$E2,$E5,$B8,$CC;{$ENDIF} 
           {$IFDEF FPC}vmovupd [esp + 96], ymm1;{$ELSE}db $C5,$FD,$11,$4C,$24,$A0;{$ENDIF}

           {$IFDEF FPC}vmovupd ymm0, [esp + 64];{$ELSE}db $C5,$FD,$10,$44,$24,$C0;{$ENDIF}
           {$IFDEF FPC}vmovupd ymm4, [eax + 64];{$ELSE}db $C5,$FD,$10,$60,$40;{$ENDIF} 
           {$IFDEF FPC}vfmadd231pd ymm0, ymm3, ymm4;{$ELSE}db $C4,$E2,$E5,$B8,$C4;{$ENDIF} 
           {$IFDEF FPC}vmovupd [esp + 64], ymm0;{$ELSE}db $C5,$FD,$11,$44,$24,$C0;{$ENDIF}

           {$IFDEF FPC}vmovupd ymm1, [esp + 32];{$ELSE}db $C5,$FD,$10,$4C,$24,$E0;{$ENDIF}
           {$IFDEF FPC}vmovupd ymm4, [eax + 96];{$ELSE}db $C5,$FD,$10,$60,$60;{$ENDIF} 
           {$IFDEF FPC}vfmadd231pd ymm1, ymm3, ymm4;{$ELSE}db $C4,$E2,$E5,$B8,$CC;{$ENDIF} 
           {$IFDEF FPC}vmovupd [esp + 32], ymm1;{$ELSE}db $C5,$FD,$11,$4C,$24,$E0;{$ENDIF}

           add eax, esi;
           add ebx, edi;

       dec edx;
       jnz @@foryloop;

       // result building
       // write back result (final addition and compactation)

       // calculate dest = beta*dest + alpha*xmm0
       // first 4
       {$IFDEF FPC}vmovupd ymm3, [ecx];{$ELSE}db $C5,$FD,$10,$19;{$ENDIF} 

       {$IFDEF FPC}vmovupd ymm0, [esp + 128];{$ELSE}db $C5,$FD,$10,$44,$24,$80;{$ENDIF}
       {$IFDEF FPC}vmulpd ymm0, ymm0, ymm6; {$ELSE}db $C5,$FD,$59,$C6;{$ENDIF} // alpha*res
       {$IFDEF FPC}vmulpd ymm3, ymm3, ymm7; {$ELSE}db $C5,$E5,$59,$DF;{$ENDIF} // dest*beta
       {$IFDEF FPC}vaddpd ymm3, ymm3, ymm0;{$ELSE}db $C5,$E5,$58,$D8;{$ENDIF} 
       {$IFDEF FPC}vmovupd [ecx], ymm3;{$ELSE}db $C5,$FD,$11,$19;{$ENDIF} 
       add ecx, 32;

       // second 4
       {$IFDEF FPC}vmovupd ymm3, [ecx];{$ELSE}db $C5,$FD,$10,$19;{$ENDIF} 

       {$IFDEF FPC}vmovupd ymm0, [esp + 96];{$ELSE}db $C5,$FD,$10,$44,$24,$A0;{$ENDIF}
       {$IFDEF FPC}vmulpd ymm0, ymm0, ymm6; {$ELSE}db $C5,$FD,$59,$C6;{$ENDIF} // alpha*res
       {$IFDEF FPC}vmulpd ymm3, ymm3, ymm7; {$ELSE}db $C5,$E5,$59,$DF;{$ENDIF} // dest*beta
       {$IFDEF FPC}vaddpd ymm3, ymm3, ymm0;{$ELSE}db $C5,$E5,$58,$D8;{$ENDIF} 
       {$IFDEF FPC}vmovupd [ecx], ymm3;{$ELSE}db $C5,$FD,$11,$19;{$ENDIF} 
       add ecx, 32;

       // third 4
       {$IFDEF FPC}vmovupd ymm3, [ecx];{$ELSE}db $C5,$FD,$10,$19;{$ENDIF} 

       {$IFDEF FPC}vmovupd ymm0, [esp + 64];{$ELSE}db $C5,$FD,$10,$44,$24,$C0;{$ENDIF}
       {$IFDEF FPC}vmulpd ymm0, ymm0, ymm6; {$ELSE}db $C5,$FD,$59,$C6;{$ENDIF} // alpha*res
       {$IFDEF FPC}vmulpd ymm3, ymm3, ymm7; {$ELSE}db $C5,$E5,$59,$DF;{$ENDIF} // dest*beta
       {$IFDEF FPC}vaddpd ymm3, ymm3, ymm0;{$ELSE}db $C5,$E5,$58,$D8;{$ENDIF} 
       {$IFDEF FPC}vmovupd [ecx], ymm3;{$ELSE}db $C5,$FD,$11,$19;{$ENDIF} 
       add ecx, 32;

       // forth 4
       {$IFDEF FPC}vmovupd ymm3, [ecx];{$ELSE}db $C5,$FD,$10,$19;{$ENDIF} 

       {$IFDEF FPC}vmovupd ymm0, [esp + 32];{$ELSE}db $C5,$FD,$10,$44,$24,$E0;{$ENDIF}
       {$IFDEF FPC}vmulpd ymm0, ymm0, ymm6; {$ELSE}db $C5,$FD,$59,$C6;{$ENDIF} // alpha*res
       {$IFDEF FPC}vmulpd ymm3, ymm3, ymm7; {$ELSE}db $C5,$E5,$59,$DF;{$ENDIF} // dest*beta
       {$IFDEF FPC}vaddpd ymm3, ymm3, ymm0;{$ELSE}db $C5,$E5,$58,$D8;{$ENDIF} 
       {$IFDEF FPC}vmovupd [ecx], ymm3;{$ELSE}db $C5,$FD,$11,$19;{$ENDIF} 
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
       {$IFDEF FPC}vxorpd xmm0, xmm0, xmm0;  {$ELSE}db $C5,$F9,$57,$C0;{$ENDIF} // first two elements

       mov eax, mt1;       // eax = first matrix element
       mov ebx, v;       // ebx = first vector element

       mov edx, height;

       @@forshortyloop:
           {$IFDEF FPC}vmovsd xmm1, [eax];{$ELSE}db $C5,$FB,$10,$08;{$ENDIF} 
           {$IFDEF FPC}vmovsd xmm2, [ebx];{$ELSE}db $C5,$FB,$10,$13;{$ENDIF} 

           {$IFDEF FPC}vmulsd xmm1, xmm1, xmm2;{$ELSE}db $C5,$F3,$59,$CA;{$ENDIF} 
           {$IFDEF FPC}vaddsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$58,$C1;{$ENDIF} 

           add eax, esi;
           add ebx, edi;

       dec edx;
       jnz @@forshortyloop;

       {$IFDEF FPC}vmulsd xmm0, xmm0, xmm6;  {$ELSE}db $C5,$FB,$59,$C6;{$ENDIF} // alpha*res

       {$IFDEF FPC}vmovsd xmm3, [ecx];{$ELSE}db $C5,$FB,$10,$19;{$ENDIF} 
       {$IFDEF FPC}vmulsd xmm3, xmm3, xmm7;  {$ELSE}db $C5,$E3,$59,$DF;{$ENDIF} //dest*beta
       {$IFDEF FPC}vaddsd xmm0, xmm0, xmm3;{$ELSE}db $C5,$FB,$58,$C3;{$ENDIF} 
       {$IFDEF FPC}vmovsd [ecx], xmm0;{$ELSE}db $C5,$FB,$11,$01;{$ENDIF} 

       // next column
       add ecx, 8;
       mov eax, mt1;
       add eax, 8;
       mov mt1, eax;

   dec Width;
   jnz @@forxshortloop;


   @@vecaddend:

   // epilog pop "stack"
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   // undo local mem
   add esp, 4*4*8;

   pop esi;
   pop edi;
   pop ebx;
end;
end;


procedure FMARank1UpdateSeq(A : PDouble; const LineWidthA : TASMNativeInt; width, height : TASMNativeInt;
  const alpha : double; X, Y : PDouble; incX, incY : TASMNativeInt);
begin
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
   {$IFDEF FPC}vbroadcastsd ymm3, [eax];{$ELSE}db $C4,$E2,$7D,$19,$18;{$ENDIF} 

   // prepare for loop
   mov esi, incx;
   // init for y := 0 to height - 1:
   @@foryloop:
      // init values:
      {$IFDEF FPC}vbroadcastsd ymm0, [edi];  {$ELSE}db $C4,$E2,$7D,$19,$07;{$ENDIF} // res := 0;
      {$IFDEF FPC}vmulpd ymm0, ymm0, ymm3;   {$ELSE}db $C5,$FD,$59,$C3;{$ENDIF} // tmp := alpha*pX^
      mov eax, ecx;              // eax = first destination element A
      mov ebx, Y;                // ebx = first y vector element

      // for j := 0 to width - 1 do
      mov edx, width;
      sub edx, 4;
      jl @@last3Elem;

      @@forxloop:
         {$IFDEF FPC}vmovupd ymm1, [eax];{$ELSE}db $C5,$FD,$10,$08;{$ENDIF} 
         {$IFDEF FPC}vmovupd ymm2, [ebx];{$ELSE}db $C5,$FD,$10,$13;{$ENDIF} 

         // pA^[j] := pA^[j] + tmp*pY1^[j];
         {$IFDEF FPC}vfmadd231pd ymm1, ymm2, ymm0;{$ELSE}db $C4,$E2,$ED,$B8,$C8;{$ENDIF} 

         {$IFDEF FPC}vmovupd [eax], ymm1;{$ELSE}db $C5,$FD,$11,$08;{$ENDIF} 

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
      {$IFDEF FPC}vmovupd xmm1, [eax];{$ELSE}db $C5,$F9,$10,$08;{$ENDIF} 
      {$IFDEF FPC}vmovupd xmm2, [ebx];{$ELSE}db $C5,$F9,$10,$13;{$ENDIF} 

      {$IFDEF FPC}vfmadd231pd xmm1, xmm2, xmm0;{$ELSE}db $C4,$E2,$E9,$B8,$C8;{$ENDIF} 
      {$IFDEF FPC}vmovupd [eax], xmm1;{$ELSE}db $C5,$F9,$11,$08;{$ENDIF} 
      add eax, 16;
      add ebx, 16;

      cmp edx, 2;
      je @@nextline;

      @@lastElem:

      {$IFDEF FPC}vmovsd xmm1, [eax];{$ELSE}db $C5,$FB,$10,$08;{$ENDIF} 
      {$IFDEF FPC}vmovsd xmm2, [ebx];{$ELSE}db $C5,$FB,$10,$13;{$ENDIF} 

      {$IFDEF FPC}vmulsd xmm2, xmm2, xmm0;{$ELSE}db $C5,$EB,$59,$D0;{$ENDIF} 
      {$IFDEF FPC}vaddsd xmm1, xmm1, xmm2;{$ELSE}db $C5,$F3,$58,$CA;{$ENDIF} 
      {$IFDEF FPC}vmovsd [eax], xmm1;{$ELSE}db $C5,$FB,$11,$08;{$ENDIF} 

      @@nextline:

      // next results:
      add edi, esi;
      add ecx, LineWidthA;
   dec height;          // r9 = height
   jnz @@foryloop;

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 
   pop esi;
   pop edi;
   pop ebx;
end;
end;


procedure FMARank1UpdateSeqAligned(A : PDouble; const LineWidthA : TASMNativeInt; width, height : TASMNativeInt;
  const alpha : double; X, Y : PDouble; incX, incY : TASMNativeInt);
begin
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
   {$IFDEF FPC}vbroadcastsd ymm3, [eax];{$ELSE}db $C4,$E2,$7D,$19,$18;{$ENDIF} 

   // prepare for loop
   mov esi, incx;
   // init for y := 0 to height - 1:
   @@foryloop:
      // init values:
      {$IFDEF FPC}vbroadcastsd ymm0, [edi];  {$ELSE}db $C4,$E2,$7D,$19,$07;{$ENDIF} // res := 0;
      {$IFDEF FPC}vmulpd ymm0, ymm0, ymm3;   {$ELSE}db $C5,$FD,$59,$C3;{$ENDIF} // tmp := alpha*pX^
      mov eax, ecx;              // eax = first destination element A
      mov ebx, Y;                // ebx = first y vector element

      // for j := 0 to width - 1 do
      mov edx, width;
      sub edx, 4;
      jl @@last3Elem;

      @@forxloop:
         {$IFDEF FPC}vmovapd ymm1, [eax];{$ELSE}db $C5,$FD,$28,$08;{$ENDIF} 

         // pA^[j] := pA^[j] + tmp*pY1^[j];
         {$IFDEF FPC}vfmadd231pd ymm1, ymm0, [ebx];{$ELSE}db $C4,$E2,$FD,$B8,$0B;{$ENDIF} 

         {$IFDEF FPC}vmovapd [eax], ymm1;{$ELSE}db $C5,$FD,$29,$08;{$ENDIF} 

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
      {$IFDEF FPC}vmovapd xmm1, [eax];{$ELSE}db $C5,$F9,$28,$08;{$ENDIF} 
      {$IFDEF FPC}vfmadd231pd xmm1, xmm0, [ebx];{$ELSE}db $C4,$E2,$F9,$B8,$0B;{$ENDIF} 
      {$IFDEF FPC}vmovapd [eax], xmm1;{$ELSE}db $C5,$F9,$29,$08;{$ENDIF} 
      add eax, 16;
      add ebx, 16;

      cmp edx, 2;
      je @@nextline;

      @@lastElem:

      {$IFDEF FPC}vmovsd xmm1, [eax];{$ELSE}db $C5,$FB,$10,$08;{$ENDIF} 
      {$IFDEF FPC}vmovsd xmm2, [ebx];{$ELSE}db $C5,$FB,$10,$13;{$ENDIF} 

      {$IFDEF FPC}vmulsd xmm2, xmm2, xmm0;{$ELSE}db $C5,$EB,$59,$D0;{$ENDIF} 
      {$IFDEF FPC}vaddsd xmm1, xmm1, xmm2;{$ELSE}db $C5,$F3,$58,$CA;{$ENDIF} 
      {$IFDEF FPC}vmovsd [eax], xmm1;{$ELSE}db $C5,$FB,$11,$08;{$ENDIF} 

      @@nextline:

      // next results:
      add edi, esi;
      add ecx, LineWidthA;
   dec height;
   jnz @@foryloop;

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 
   pop esi;
   pop edi;
   pop ebx;
end;
end;

{$ENDIF}

end.
