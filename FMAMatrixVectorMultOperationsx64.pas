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


unit FMAMatrixVectorMultOperationsx64;

// #######################################################
// #### special routines for matrix vector multiplications.
// #######################################################

interface

{$I 'mrMath_CPU.inc'}

{$IFDEF x64}

procedure FMAMatrixVectMultT(dest : PDouble; destLineWidth : NativeInt; mt1, v : PDouble;{$ifdef UNIX}unixLineWidthMT{$ELSE}LineWidthMT{$endif}, {$ifdef UNIX}unixLineWidthV{$ELSE}LineWidthV{$endif} : NativeInt; width, height : NativeInt; {$ifdef UNIX}unixalpha {$ELSE}alpha{$ENDIF}, {$ifdef UNIX}unixbeta {$ELSE}beta{$ENDIF} : double); {$IFDEF FPC}assembler;{$ENDIF}

// routines with special input layouts: LineWidthV needs to be sizeof(double)
procedure FMAMatrixVectMultAlignedVAligned(dest : PDouble; destLineWidth : NativeInt; mt1, v : PDouble; {$ifdef UNIX}unixLineWidthMT{$ELSE}LineWidthMT{$endif}, {$ifdef UNIX}unixLineWidthV{$ELSE}LineWidthV{$endif} : NativeInt; width, height : NativeInt; {$ifdef UNIX}unixalpha {$ELSE}alpha{$ENDIF}, {$ifdef UNIX}unixbeta {$ELSE}beta{$ENDIF} : double); {$IFDEF FPC}assembler;{$ENDIF}
procedure FMAMatrixVectMultUnAlignedVAligned(dest : PDouble; destLineWidth : NativeInt; mt1, v : PDouble; {$ifdef UNIX}unixLineWidthMT{$ELSE}LineWidthMT{$endif}, {$ifdef UNIX}unixLineWidthV{$ELSE}LineWidthV{$endif} : NativeInt; width, height : NativeInt; {$ifdef UNIX}unixalpha {$ELSE}alpha{$ENDIF}, {$ifdef UNIX}unixbeta {$ELSE}beta{$ENDIF} : double); {$IFDEF FPC}assembler;{$ENDIF}

// destlinewidth needs to be sizeof(double)!
// no speed gain agains amsmatrixVectMultT
procedure FMAMatrixVecMultTDestVec(dest : PDouble; destLineWidth : NativeInt; mt1, v : PDouble; {$ifdef UNIX}unixLineWidthMT{$ELSE}LineWidthMT{$endif}, {$ifdef UNIX}unixLineWidthV{$ELSE}LineWidthV{$endif} : NativeInt; width, height : NativeInt; {$ifdef UNIX}unixalpha {$ELSE}alpha{$ENDIF}, {$ifdef UNIX}unixbeta {$ELSE}beta{$ENDIF} : double); {$IFDEF FPC}assembler;{$ENDIF}

// rank1 update: A = A + alpha*X*Y' where x and y are vectors. It's assumed that y is sequential
procedure FMARank1UpdateSeq(A : PDouble; const LineWidthA : NativeInt; width, height : NativeInt;
  {$ifdef UNIX}unixX{$ELSE}X{$endif}, {$ifdef UNIX}unixY{$ELSE}Y{$ENDIF} : PDouble; incX, incY : NativeInt; {$ifdef UNIX}unixalpha {$ELSE}alpha{$ENDIF} : double); {$IFDEF FPC}assembler;{$ENDIF}
procedure FMARank1UpdateSeqAligned(A : PDouble; const LineWidthA : NativeInt; width, height : NativeInt;
  {$ifdef UNIX}unixX{$ELSE}X{$endif}, {$ifdef UNIX}unixY{$ELSE}Y{$ENDIF} : PDouble; incX, incY : NativeInt; {$ifdef UNIX}unixalpha {$ELSE}alpha{$ENDIF} : double); {$IFDEF FPC}assembler;{$ENDIF}

{$ENDIF}

implementation

{$IFDEF x64}

// routines with special input layouts: LineWidthV needs to be sizeof(double)
procedure FMAMatrixVectMultAlignedVAligned(dest : PDouble; destLineWidth : NativeInt; mt1, v : PDouble; {$ifdef UNIX}unixLineWidthMT{$ELSE}LineWidthMT{$endif}, {$ifdef UNIX}unixLineWidthV{$ELSE}LineWidthV{$endif} : NativeInt; width, height : NativeInt; {$ifdef UNIX}unixalpha {$ELSE}alpha{$ENDIF}, {$ifdef UNIX}unixbeta {$ELSE}beta{$ENDIF} : double); {$IFDEF FPC}assembler;{$ENDIF}
// note: RCX = dest, RDX = destLineWidth, R8 = mt1, R9 = v
var iRBX, iRSI, iRDI, iR12, iR13, iR14 : NativeInt;
    {$ifdef UNIX}
    LineWidthMT, LineWidthV : NativeInt;
    alpha, beta : double;
    {$endif}
asm
   {$IFDEF UNIX}
   // Linux uses a diffrent ABI -> copy over the registers so they meet with winABI
   // (note that the 5th and 6th parameter are are on the stack)
   // The parameters are passed in the following order:
   // RDI, RSI, RDX, RCX -> mov to RCX, RDX, R8, R9
   mov LineWidthMT, r8;
   mov LineWidthV, r9;
   mov r8, rdx;
   mov r9, rcx;
   mov rcx, rdi;
   mov rdx, rsi;
   movsd alpha, unixalpha;
   movsd beta, unixbeta;
   {$ENDIF}

   // prolog - simulate stack
   mov iRBX, rbx;
   mov iRSI, rsi;
   mov iRDI, rdi;
   mov iR12, r12;
   mov iR13, r13;
   mov iR14, r14;

   sub rsp, $30;
   {$IFDEF FPC}vmovupd [rsp + $00], xmm4;{$ELSE}db $C5,$F9,$11,$24,$24;{$ENDIF} 
   {$IFDEF FPC}vmovupd [rsp + $10], xmm5;{$ELSE}db $C5,$F9,$11,$6C,$24,$10;{$ENDIF} 
   {$IFDEF FPC}vmovupd [rsp + $20], xmm6;{$ELSE}db $C5,$F9,$11,$74,$24,$20;{$ENDIF} 

   // for the final multiplication
   lea rbx, alpha;
   {$IFDEF FPC}vmovsd xmm6, [rbx];{$ELSE}db $C5,$FB,$10,$33;{$ENDIF} 
   lea rax, beta;
   {$IFDEF FPC}vmovhpd xmm6, xmm6, [rax];{$ELSE}db $C5,$C9,$16,$30;{$ENDIF} 


   // prepare for loop
   mov rsi, LineWidthMT;
   mov rdi, LineWidthV;
   mov r13, height;
   mov r14, width;

   sub r14, 4;
   sub r13, 4;
   js @@foryloopend;

   // we need at least a width of 4 for the fast unrolled loop
   cmp r14, 0;
   jl @@foryloopend;

   // init for x := 0 to width - 1:
   @@foryloop:

       // init values:
       {$IFDEF FPC}vxorpd ymm0, ymm0, ymm0;{$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} 
       {$IFDEF FPC}vxorpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$57,$C9;{$ENDIF} 
       {$IFDEF FPC}vxorpd ymm2, ymm2, ymm2;{$ELSE}db $C5,$ED,$57,$D2;{$ENDIF} 
       {$IFDEF FPC}vxorpd ymm3, ymm3, ymm3;  {$ELSE}db $C5,$E5,$57,$DB;{$ENDIF} // res := 0;
       mov rax, r8;       // rax = first matrix element
       mov rbx, r9;       // rbx = first vector element

       mov r10, r14;      // r10 = width
       @@forxloop:
           {$IFDEF FPC}vmovapd ymm4, [rbx];{$ELSE}db $C5,$FD,$28,$23;{$ENDIF} 
           {$IFDEF FPC}vfmadd231pd ymm0, ymm4, [rax];{$ELSE}db $C4,$E2,$DD,$B8,$00;{$ENDIF} 
           {$IFDEF FPC}vfmadd231pd ymm1, ymm4, [rax + rsi];{$ELSE}db $C4,$E2,$DD,$B8,$0C,$30;{$ENDIF} 
           {$IFDEF FPC}vfmadd231pd ymm2, ymm4, [rax + 2*rsi];{$ELSE}db $C4,$E2,$DD,$B8,$14,$70;{$ENDIF} 
           add rax, rsi;
           {$IFDEF FPC}vfmadd231pd ymm3, ymm4, [rax + 2*rsi];{$ELSE}db $C4,$E2,$DD,$B8,$1C,$70;{$ENDIF} 
           sub rax, rsi;

           add rax, 32;
           add rbx, 32;

       sub r10, 4;
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
       add r10, 4;
       jz @@resbuild;

       @@shortloopx:
          {$IFDEF FPC}vmovsd xmm4, [rbx];{$ELSE}db $C5,$FB,$10,$23;{$ENDIF} 
          {$IFDEF FPC}vmovsd xmm5, [rax];{$ELSE}db $C5,$FB,$10,$28;{$ENDIF} 

          {$IFDEF FPC}vmulsd xmm5, xmm5, xmm4;{$ELSE}db $C5,$D3,$59,$EC;{$ENDIF} 
          {$IFDEF FPC}vaddsd xmm0, xmm0, xmm5;{$ELSE}db $C5,$FB,$58,$C5;{$ENDIF} 

          {$IFDEF FPC}vmovsd xmm5, [rax + rsi];{$ELSE}db $C5,$FB,$10,$2C,$30;{$ENDIF} 

          {$IFDEF FPC}vmulsd xmm5, xmm5, xmm4;{$ELSE}db $C5,$D3,$59,$EC;{$ENDIF} 
          {$IFDEF FPC}vaddsd xmm1, xmm1, xmm5;{$ELSE}db $C5,$F3,$58,$CD;{$ENDIF} 

          {$IFDEF FPC}vmovsd xmm5, [rax + 2*rsi];{$ELSE}db $C5,$FB,$10,$2C,$70;{$ENDIF} 

          {$IFDEF FPC}vmulsd xmm5, xmm5, xmm4;{$ELSE}db $C5,$D3,$59,$EC;{$ENDIF} 
          {$IFDEF FPC}vaddsd xmm2, xmm2, xmm5;{$ELSE}db $C5,$EB,$58,$D5;{$ENDIF} 

          add rax, rsi;
          {$IFDEF FPC}vmovsd xmm5, [rax + 2*rsi];{$ELSE}db $C5,$FB,$10,$2C,$70;{$ENDIF} 
          sub rax, rsi;

          {$IFDEF FPC}vmulsd xmm5, xmm5, xmm4;{$ELSE}db $C5,$D3,$59,$EC;{$ENDIF} 
          {$IFDEF FPC}vaddsd xmm3, xmm3, xmm5;{$ELSE}db $C5,$E3,$58,$DD;{$ENDIF} 

          add rax, 8;
          add rbx, 8;
       sub r10, 1;
       jnz @@shortloopx;


       @@resbuild:
       // result building
       // write back result (final addition and compactation)

       // calculate dest = beta*dest + alpha*xmm0
       {$IFDEF FPC}vmovsd xmm5, [rcx];              {$ELSE}db $C5,$FB,$10,$29;{$ENDIF} // first element
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm5;        {$ELSE}db $C5,$F9,$7C,$C5;{$ENDIF} // calculate xmm0_1 + xmm0_2 (store low) xmm5_1 + xmm5_2 (store high)
       {$IFDEF FPC}vmulpd xmm0, xmm0, xmm6;         {$ELSE}db $C5,$F9,$59,$C6;{$ENDIF} // beta * dest + alpha*xmm0
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm0;        {$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} // final add
       {$IFDEF FPC}vmovsd [rcx], xmm0;              {$ELSE}db $C5,$FB,$11,$01;{$ENDIF} // store back
       add rcx, rdx;
       add r8, rsi;

       {$IFDEF FPC}vmovsd xmm5, [rcx];{$ELSE}db $C5,$FB,$10,$29;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm1, xmm1, xmm5;{$ELSE}db $C5,$F1,$7C,$CD;{$ENDIF} 
       {$IFDEF FPC}vmulpd xmm1, xmm1, xmm6;{$ELSE}db $C5,$F1,$59,$CE;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm1, xmm1, xmm1;{$ELSE}db $C5,$F1,$7C,$C9;{$ENDIF} 
       {$IFDEF FPC}vmovsd [rcx], xmm1;{$ELSE}db $C5,$FB,$11,$09;{$ENDIF} 
       add rcx, rdx;
       add r8, rsi;

       {$IFDEF FPC}vmovsd xmm5, [rcx];{$ELSE}db $C5,$FB,$10,$29;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm2, xmm2, xmm5;{$ELSE}db $C5,$E9,$7C,$D5;{$ENDIF} 
       {$IFDEF FPC}vmulpd xmm2, xmm2, xmm6;{$ELSE}db $C5,$E9,$59,$D6;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm2, xmm2, xmm2;{$ELSE}db $C5,$E9,$7C,$D2;{$ENDIF} 
       {$IFDEF FPC}vmovsd [rcx], xmm2;{$ELSE}db $C5,$FB,$11,$11;{$ENDIF} 
       add rcx, rdx;
       add r8, rsi;

       {$IFDEF FPC}vmovsd xmm5, [rcx];{$ELSE}db $C5,$FB,$10,$29;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm3, xmm3, xmm5;{$ELSE}db $C5,$E1,$7C,$DD;{$ENDIF} 
       {$IFDEF FPC}vmulpd xmm3, xmm3, xmm6;{$ELSE}db $C5,$E1,$59,$DE;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm3, xmm3, xmm3;{$ELSE}db $C5,$E1,$7C,$DB;{$ENDIF} 
       {$IFDEF FPC}vmovsd [rcx], xmm3;{$ELSE}db $C5,$FB,$11,$19;{$ENDIF} 
       add rcx, rdx;
       add r8, rsi;

       // next rseult
   sub r13, 4;
   jge @@foryloop;

   // ###########################################
   // #### Remaining rows (max 4 or more if width is <4):
   @@foryloopend:
   add r13, 4;
   jz @@vecend;

   @@foryshortloop:
       // init values:
       {$IFDEF FPC}vxorpd ymm0, ymm0, ymm0;{$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} 
       mov rax, r8;       // rax = first matrix element
       mov rbx, r9;       // rbx = first vector element

       mov r10, r14;      // r10 = width - 4
       cmp r10, 0;
       jl @@shortloopend;

       @@forxshortloop:
           {$IFDEF FPC}vmovapd ymm4, [rbx];{$ELSE}db $C5,$FD,$28,$23;{$ENDIF} 
           {$IFDEF FPC}vfmadd231pd ymm0, ymm4, [rax];{$ELSE}db $C4,$E2,$DD,$B8,$00;{$ENDIF} 
           add rax, 32;
           add rbx, 32;
       sub r10, 4;
       jge @@forxshortloop;

       {$IFDEF FPC}vextractf128 xmm4, ymm0, 1;{$ELSE}db $C4,$E3,$7D,$19,$C4,$01;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm4;{$ELSE}db $C5,$F9,$7C,$C4;{$ENDIF} 

       @@shortloopend:

       add r10, 4;
       // test if there are elements left
       jz @@resbuildshort;
       @@forxshortestloop:
           {$IFDEF FPC}vmovsd xmm4, [rbx];{$ELSE}db $C5,$FB,$10,$23;{$ENDIF} 
           {$IFDEF FPC}vmovsd xmm5, [rax];{$ELSE}db $C5,$FB,$10,$28;{$ENDIF} 

           {$IFDEF FPC}vmulsd xmm5, xmm5, xmm4;{$ELSE}db $C5,$D3,$59,$EC;{$ENDIF} 
           {$IFDEF FPC}vaddsd xmm0, xmm0, xmm5;{$ELSE}db $C5,$FB,$58,$C5;{$ENDIF} 

           add rax, 8;
           add rbx, 8;
       dec r10;
       jnz @@forxshortestloop;

       @@resbuildshort:

       // build result

       // calculate dest = beta*dest + alpha*xmm0
       {$IFDEF FPC}vmovsd xmm5, [rcx];{$ELSE}db $C5,$FB,$10,$29;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm5;{$ELSE}db $C5,$F9,$7C,$C5;{$ENDIF} 
       {$IFDEF FPC}vmulpd xmm0, xmm0, xmm6;{$ELSE}db $C5,$F9,$59,$C6;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} 
       {$IFDEF FPC}vmovsd [rcx], xmm0;{$ELSE}db $C5,$FB,$11,$01;{$ENDIF} 

       add rcx, rdx;
       add r8, rsi;

   dec r13;
   jnz @@foryshortloop;

   @@vecend:

   // epilog pop "stack"
   mov rbx, iRBX;
   mov rsi, iRSI;
   mov rdi, iRDI;
   mov r12, iR12;
   mov r13, iR13;
   mov r14, iR14;

   {$IFDEF FPC}vmovupd xmm4, [rsp + $00];{$ELSE}db $C5,$F9,$10,$24,$24;{$ENDIF} 
   {$IFDEF FPC}vmovupd xmm5, [rsp + $10];{$ELSE}db $C5,$F9,$10,$6C,$24,$10;{$ENDIF} 
   {$IFDEF FPC}vmovupd xmm6, [rsp + $20];{$ELSE}db $C5,$F9,$10,$74,$24,$20;{$ENDIF} 
   add rsp, $30;
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;

procedure FMAMatrixVectMultUnAlignedVAligned(dest : PDouble; destLineWidth : NativeInt; mt1, v : PDouble; {$ifdef UNIX}unixLineWidthMT{$ELSE}LineWidthMT{$endif}, {$ifdef UNIX}unixLineWidthV{$ELSE}LineWidthV{$endif} : NativeInt; width, height : NativeInt; {$ifdef UNIX}unixalpha {$ELSE}alpha{$ENDIF}, {$ifdef UNIX}unixbeta {$ELSE}beta{$ENDIF} : double); {$IFDEF FPC}assembler;{$ENDIF}
// note: RCX = dest, RDX = destLineWidth, R8 = mt1, R9 = v
var iRBX, iRSI, iRDI, iR12, iR13, iR14 : NativeInt;
    {$ifdef UNIX}
    LineWidthMT, LineWidthV : NativeInt;
    alpha, beta : double;
    {$endif}
asm
   {$IFDEF UNIX}
   // Linux uses a diffrent ABI -> copy over the registers so they meet with winABI
   // (note that the 5th and 6th parameter are are on the stack)
   // The parameters are passed in the following order:
   // RDI, RSI, RDX, RCX -> mov to RCX, RDX, R8, R9
   mov LineWidthMT, r8;
   mov LineWidthV, r9;
   mov r8, rdx;
   mov r9, rcx;
   mov rcx, rdi;
   mov rdx, rsi;
   movsd alpha, unixalpha;
   movsd beta, unixbeta;
   {$ENDIF}

   // prolog - simulate stack
   mov iRBX, rbx;
   mov iRSI, rsi;
   mov iRDI, rdi;
   mov iR12, r12;
   mov iR13, r13;
   mov iR14, r14;

   sub rsp, $30;
   {$IFDEF FPC}vmovupd [rsp + $00], xmm4;{$ELSE}db $C5,$F9,$11,$24,$24;{$ENDIF} 
   {$IFDEF FPC}vmovupd [rsp + $10], xmm5;{$ELSE}db $C5,$F9,$11,$6C,$24,$10;{$ENDIF} 
   {$IFDEF FPC}vmovupd [rsp + $20], xmm6;{$ELSE}db $C5,$F9,$11,$74,$24,$20;{$ENDIF} 

   // for the final multiplication
   lea rbx, alpha;
   {$IFDEF FPC}vmovsd xmm6, [rbx];{$ELSE}db $C5,$FB,$10,$33;{$ENDIF} 
   lea rax, beta;
   {$IFDEF FPC}vmovhpd xmm6, xmm6, [rax];{$ELSE}db $C5,$C9,$16,$30;{$ENDIF} 


   // prepare for loop
   mov rsi, LineWidthMT;
   mov rdi, LineWidthV;
   mov r13, height;
   mov r14, width;

   sub r14, 4;
   sub r13, 4;
   js @@foryloopend;

   // we need at least a width of 4 for the fast unrolled loop
   cmp r14, 0;
   jl @@foryloopend;

   // init for x := 0 to width - 1:
   @@foryloop:

       // init values:
       {$IFDEF FPC}vxorpd ymm0, ymm0, ymm0;{$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} 
       {$IFDEF FPC}vxorpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$57,$C9;{$ENDIF} 
       {$IFDEF FPC}vxorpd ymm2, ymm2, ymm2;{$ELSE}db $C5,$ED,$57,$D2;{$ENDIF} 
       {$IFDEF FPC}vxorpd ymm3, ymm3, ymm3;  {$ELSE}db $C5,$E5,$57,$DB;{$ENDIF} // res := 0;
       mov rax, r8;       // rax = first matrix element
       mov rbx, r9;       // rbx = first vector element

       mov r10, r14;      // r10 = width
       @@forxloop:
           {$IFDEF FPC}vmovupd ymm4, [rbx];{$ELSE}db $C5,$FD,$10,$23;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm5, [rax];{$ELSE}db $C5,$FD,$10,$28;{$ENDIF} 
           {$IFDEF FPC}vfmadd231pd ymm0, ymm4, ymm5;{$ELSE}db $C4,$E2,$DD,$B8,$C5;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm5, [rax + rsi];{$ELSE}db $C5,$FD,$10,$2C,$30;{$ENDIF} 
           {$IFDEF FPC}vfmadd231pd ymm1, ymm4, ymm5;{$ELSE}db $C4,$E2,$DD,$B8,$CD;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm5, [rax + 2*rsi];{$ELSE}db $C5,$FD,$10,$2C,$70;{$ENDIF} 
           {$IFDEF FPC}vfmadd231pd ymm2, ymm4, ymm5;{$ELSE}db $C4,$E2,$DD,$B8,$D5;{$ENDIF} 

           add rax, rsi;
           {$IFDEF FPC}vmovupd ymm5, [rax + 2*rsi];{$ELSE}db $C5,$FD,$10,$2C,$70;{$ENDIF} 
           {$IFDEF FPC}vfmadd231pd ymm3, ymm4, ymm5;{$ELSE}db $C4,$E2,$DD,$B8,$DD;{$ENDIF} 
           sub rax, rsi;

           add rax, 32;
           add rbx, 32;

       sub r10, 4;
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
       add r10, 4;
       jz @@resbuild;

       @@shortloopx:
          {$IFDEF FPC}vmovsd xmm4, [rbx];{$ELSE}db $C5,$FB,$10,$23;{$ENDIF} 
          {$IFDEF FPC}vmovsd xmm5, [rax];{$ELSE}db $C5,$FB,$10,$28;{$ENDIF} 

          {$IFDEF FPC}vmulsd xmm5, xmm5, xmm4;{$ELSE}db $C5,$D3,$59,$EC;{$ENDIF} 
          {$IFDEF FPC}vaddsd xmm0, xmm0, xmm5;{$ELSE}db $C5,$FB,$58,$C5;{$ENDIF} 

          {$IFDEF FPC}vmovsd xmm5, [rax + rsi];{$ELSE}db $C5,$FB,$10,$2C,$30;{$ENDIF} 

          {$IFDEF FPC}vmulsd xmm5, xmm5, xmm4;{$ELSE}db $C5,$D3,$59,$EC;{$ENDIF} 
          {$IFDEF FPC}vaddsd xmm1, xmm1, xmm5;{$ELSE}db $C5,$F3,$58,$CD;{$ENDIF} 

          {$IFDEF FPC}vmovsd xmm5, [rax + 2*rsi];{$ELSE}db $C5,$FB,$10,$2C,$70;{$ENDIF} 

          {$IFDEF FPC}vmulsd xmm5, xmm5, xmm4;{$ELSE}db $C5,$D3,$59,$EC;{$ENDIF} 
          {$IFDEF FPC}vaddsd xmm2, xmm2, xmm5;{$ELSE}db $C5,$EB,$58,$D5;{$ENDIF} 

          add rax, rsi;
          {$IFDEF FPC}vmovsd xmm5, [rax + 2*rsi];{$ELSE}db $C5,$FB,$10,$2C,$70;{$ENDIF} 
          sub rax, rsi;

          {$IFDEF FPC}vmulsd xmm5, xmm5, xmm4;{$ELSE}db $C5,$D3,$59,$EC;{$ENDIF} 
          {$IFDEF FPC}vaddsd xmm3, xmm3, xmm5;{$ELSE}db $C5,$E3,$58,$DD;{$ENDIF} 

          add rax, 8;
          add rbx, 8;
       sub r10, 1;
       jnz @@shortloopx;

       @@resbuild:
       // result building
       // write back result (final addition and compactation)

       // calculate dest = beta*dest + alpha*xmm0
       {$IFDEF FPC}vmovsd xmm5, [rcx];              {$ELSE}db $C5,$FB,$10,$29;{$ENDIF} // first element
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm5;        {$ELSE}db $C5,$F9,$7C,$C5;{$ENDIF} // calculate xmm0_1 + xmm0_2 (store low) xmm5_1 + xmm5_2 (store high)
       {$IFDEF FPC}vmulpd xmm0, xmm0, xmm6;         {$ELSE}db $C5,$F9,$59,$C6;{$ENDIF} // beta * dest + alpha*xmm0
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm0;        {$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} // final add
       {$IFDEF FPC}vmovsd [rcx], xmm0;              {$ELSE}db $C5,$FB,$11,$01;{$ENDIF} // store back
       add rcx, rdx;
       add r8, rsi;

       {$IFDEF FPC}vmovsd xmm5, [rcx];{$ELSE}db $C5,$FB,$10,$29;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm1, xmm1, xmm5;{$ELSE}db $C5,$F1,$7C,$CD;{$ENDIF} 
       {$IFDEF FPC}vmulpd xmm1, xmm1, xmm6;{$ELSE}db $C5,$F1,$59,$CE;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm1, xmm1, xmm1;{$ELSE}db $C5,$F1,$7C,$C9;{$ENDIF} 
       {$IFDEF FPC}vmovsd [rcx], xmm1;{$ELSE}db $C5,$FB,$11,$09;{$ENDIF} 
       add rcx, rdx;
       add r8, rsi;

       {$IFDEF FPC}vmovsd xmm5, [rcx];{$ELSE}db $C5,$FB,$10,$29;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm2, xmm2, xmm5;{$ELSE}db $C5,$E9,$7C,$D5;{$ENDIF} 
       {$IFDEF FPC}vmulpd xmm2, xmm2, xmm6;{$ELSE}db $C5,$E9,$59,$D6;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm2, xmm2, xmm2;{$ELSE}db $C5,$E9,$7C,$D2;{$ENDIF} 
       {$IFDEF FPC}vmovsd [rcx], xmm2;{$ELSE}db $C5,$FB,$11,$11;{$ENDIF} 
       add rcx, rdx;
       add r8, rsi;

       {$IFDEF FPC}vmovsd xmm5, [rcx];{$ELSE}db $C5,$FB,$10,$29;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm3, xmm3, xmm5;{$ELSE}db $C5,$E1,$7C,$DD;{$ENDIF} 
       {$IFDEF FPC}vmulpd xmm3, xmm3, xmm6;{$ELSE}db $C5,$E1,$59,$DE;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm3, xmm3, xmm3;{$ELSE}db $C5,$E1,$7C,$DB;{$ENDIF} 
       {$IFDEF FPC}vmovsd [rcx], xmm3;{$ELSE}db $C5,$FB,$11,$19;{$ENDIF} 
       add rcx, rdx;
       add r8, rsi;

       // next rseult
   sub r13, 4;
   jge @@foryloop;

   // ###########################################
   // #### Remaining rows (max 4 or more if width is <4):
   @@foryloopend:
   add r13, 4;
   jz @@vecend;

   @@foryshortloop:
       // init values:
       {$IFDEF FPC}vxorpd ymm0, ymm0, ymm0;{$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} 
       mov rax, r8;       // rax = first matrix element
       mov rbx, r9;       // rbx = first vector element

       mov r10, r14;      // r10 = width - 4
       cmp r10, 0;
       jl @@shortloopend;

       @@forxshortloop:
           {$IFDEF FPC}vmovupd ymm4, [rbx];{$ELSE}db $C5,$FD,$10,$23;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm5, [rax];{$ELSE}db $C5,$FD,$10,$28;{$ENDIF} 
           {$IFDEF FPC}vfmadd231pd ymm0, ymm4, ymm5;{$ELSE}db $C4,$E2,$DD,$B8,$C5;{$ENDIF} 

           add rax, 32;
           add rbx, 32;
       sub r10, 4;
       jge @@forxshortloop;

       {$IFDEF FPC}vextractf128 xmm4, ymm0, 1;{$ELSE}db $C4,$E3,$7D,$19,$C4,$01;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm4;{$ELSE}db $C5,$F9,$7C,$C4;{$ENDIF} 

       @@shortloopend:

       add r10, 4;
       // test if there are elements left
       jz @@resbuildshort;
       @@forxshortestloop:
           {$IFDEF FPC}vmovsd xmm4, [rbx];{$ELSE}db $C5,$FB,$10,$23;{$ENDIF} 
           {$IFDEF FPC}vmovsd xmm5, [rax];{$ELSE}db $C5,$FB,$10,$28;{$ENDIF} 

           {$IFDEF FPC}vmulsd xmm5, xmm5, xmm4;{$ELSE}db $C5,$D3,$59,$EC;{$ENDIF} 
           {$IFDEF FPC}vaddsd xmm0, xmm0, xmm5;{$ELSE}db $C5,$FB,$58,$C5;{$ENDIF} 

           add rax, 8;
           add rbx, 8;
       dec r10;
       jnz @@forxshortestloop;

       @@resbuildshort:

       // build result

       // calculate dest = beta*dest + alpha*xmm0
       {$IFDEF FPC}vmovsd xmm5, [rcx];{$ELSE}db $C5,$FB,$10,$29;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm5;{$ELSE}db $C5,$F9,$7C,$C5;{$ENDIF} 
       {$IFDEF FPC}vmulpd xmm0, xmm0, xmm6;{$ELSE}db $C5,$F9,$59,$C6;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} 
       {$IFDEF FPC}vmovsd [rcx], xmm0;{$ELSE}db $C5,$FB,$11,$01;{$ENDIF} 

       add rcx, rdx;
       add r8, rsi;

   dec r13;
   jnz @@foryshortloop;

   @@vecend:

   // epilog pop "stack"
   mov rbx, iRBX;
   mov rsi, iRSI;
   mov rdi, iRDI;
   mov r12, iR12;
   mov r13, iR13;
   mov r14, iR14;

   {$IFDEF FPC}vmovupd xmm4, [rsp + $00];{$ELSE}db $C5,$F9,$10,$24,$24;{$ENDIF} 
   {$IFDEF FPC}vmovupd xmm5, [rsp + $10];{$ELSE}db $C5,$F9,$10,$6C,$24,$10;{$ENDIF} 
   {$IFDEF FPC}vmovupd xmm6, [rsp + $20];{$ELSE}db $C5,$F9,$10,$74,$24,$20;{$ENDIF} 
   add rsp, $30;
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;

// this function is not that well suited for use of simd instructions...
// so only this version exists
procedure FMAMatrixVectMultT(dest : PDouble; destLineWidth : NativeInt; mt1, v : PDouble; {$ifdef UNIX}unixLineWidthMT{$ELSE}LineWidthMT{$endif}, {$ifdef UNIX}unixLineWidthV{$ELSE}LineWidthV{$endif} : NativeInt; width, height : NativeInt; {$ifdef UNIX}unixalpha {$ELSE}alpha{$ENDIF}, {$ifdef UNIX}unixbeta {$ELSE}beta{$ENDIF} : double); {$IFDEF FPC}assembler;{$ENDIF}
var iRBX, iRSI, iRDI, iR12, iR13, iR14 : NativeInt;
    {$ifdef UNIX}
    LineWidthMT, LineWidthV : NativeInt;
    alpha, beta : double;
    {$endif}
asm
   {$IFDEF UNIX}
   // Linux uses a diffrent ABI -> copy over the registers so they meet with winABI
   // (note that the 5th and 6th parameter are are on the stack)
   // The parameters are passed in the following order:
   // RDI, RSI, RDX, RCX -> mov to RCX, RDX, R8, R9
   movsd alpha, unixalpha;
   movsd beta, unixbeta;
   mov LineWidthMT, r8;
   mov LineWidthV, r9;
   mov r8, rdx;
   mov r9, rcx;
   mov rcx, rdi;
   mov rdx, rsi;
   {$ENDIF}

   // prolog - simulate stack
   mov iRBX, rbx;
   mov iRSI, rsi;
   mov iRDI, rdi;
   mov iR12, r12;
   mov iR13, r13;
   mov iR14, r14;

   sub rsp, $40;
   {$IFDEF FPC}vmovupd [rsp + $00], xmm4;{$ELSE}db $C5,$F9,$11,$24,$24;{$ENDIF} 
   {$IFDEF FPC}vmovupd [rsp + $10], xmm5;{$ELSE}db $C5,$F9,$11,$6C,$24,$10;{$ENDIF} 
   {$IFDEF FPC}vmovupd [rsp + $20], xmm6;{$ELSE}db $C5,$F9,$11,$74,$24,$20;{$ENDIF} 
   {$IFDEF FPC}vmovupd [rsp + $30], xmm7;{$ELSE}db $C5,$F9,$11,$7C,$24,$30;{$ENDIF} 


   // for the final multiplication
   lea rax, alpha;
   {$IFDEF FPC}vbroadcastsd ymm6, [rax];{$ELSE}db $C4,$E2,$7D,$19,$30;{$ENDIF} 
   lea rax, beta;
   {$IFDEF FPC}vbroadcastsd ymm7, [rax];{$ELSE}db $C4,$E2,$7D,$19,$38;{$ENDIF} 

   // prepare for loop
   mov rsi, LineWidthMT;
   mov rdi, LineWidthV;
   mov r13, height;
   mov r14, width;

   sub r14, 16;
   js @@forxloopend;

   sub rsp, $40;
   {$IFDEF FPC}vmovupd [rsp + $00], ymm8;{$ELSE}db $C5,$7D,$11,$04,$24;{$ENDIF} 
   {$IFDEF FPC}vmovupd [rsp + $10], ymm9;{$ELSE}db $C5,$7D,$11,$4C,$24,$10;{$ENDIF} 
   {$IFDEF FPC}vmovupd [rsp + $20], ymm10;{$ELSE}db $C5,$7D,$11,$54,$24,$20;{$ENDIF} 
   {$IFDEF FPC}vmovupd [rsp + $30], ymm11;{$ELSE}db $C5,$7D,$11,$5C,$24,$30;{$ENDIF} 

   // init for x := 0 to width - 1:
   @@forxloop:

       // init values:
       {$IFDEF FPC}vxorpd ymm8, ymm8, ymm8;{$ELSE}db $C4,$41,$3D,$57,$C0;{$ENDIF} 
       {$IFDEF FPC}vxorpd ymm9, ymm9, ymm9;{$ELSE}db $C4,$41,$35,$57,$C9;{$ENDIF} 
       {$IFDEF FPC}vxorpd ymm10, ymm10, ymm10;{$ELSE}db $C4,$41,$2D,$57,$D2;{$ENDIF} 
       {$IFDEF FPC}vxorpd ymm11, ymm11, ymm11;{$ELSE}db $C4,$41,$25,$57,$DB;{$ENDIF} 

       mov rax, r8;       // rax = first matrix element
       mov rbx, r9;       // rbx = first vector element

       mov r10, r13;
       @@foryloop:
           {$IFDEF FPC}vbroadcastsd ymm3, [rbx];{$ELSE}db $C4,$E2,$7D,$19,$1B;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm4, [rax];{$ELSE}db $C5,$FD,$10,$20;{$ENDIF} 
           {$IFDEF FPC}vfmadd231pd ymm8, ymm3, ymm4;{$ELSE}db $C4,$62,$E5,$B8,$C4;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm4, [rax + 32];{$ELSE}db $C5,$FD,$10,$60,$20;{$ENDIF} 
           {$IFDEF FPC}vfmadd231pd ymm9, ymm3, ymm4;{$ELSE}db $C4,$62,$E5,$B8,$CC;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm4, [rax + 64];{$ELSE}db $C5,$FD,$10,$60,$40;{$ENDIF} 
           {$IFDEF FPC}vfmadd231pd ymm10, ymm3, ymm4;{$ELSE}db $C4,$62,$E5,$B8,$D4;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm4, [rax + 96];{$ELSE}db $C5,$FD,$10,$60,$60;{$ENDIF} 
           {$IFDEF FPC}vfmadd231pd ymm11, ymm3, ymm4;{$ELSE}db $C4,$62,$E5,$B8,$DC;{$ENDIF} 

           add rax, rsi;
           add rbx, rdi;

       dec r10;
       jnz @@foryloop;

       // result building
       // write back result (final addition and compactation)

       // calculate dest = beta*dest + alpha*xmm0
       // first two
       {$IFDEF FPC}vmovsd xmm3, [rcx];{$ELSE}db $C5,$FB,$10,$19;{$ENDIF} 
       {$IFDEF FPC}vmovsd xmm4, [rcx + rdx];{$ELSE}db $C5,$FB,$10,$24,$11;{$ENDIF} 
       {$IFDEF FPC}vmovlhps xmm3, xmm3, xmm4;{$ELSE}db $C5,$E0,$16,$DC;{$ENDIF} 

       {$IFDEF FPC}vmovapd xmm0, xmm8;{$ELSE}db $C5,$79,$29,$C0;{$ENDIF} 
       {$IFDEF FPC}vmulpd xmm0, xmm0, xmm6;   {$ELSE}db $C5,$F9,$59,$C6;{$ENDIF} // alpha*res
       {$IFDEF FPC}vmulpd xmm3, xmm3, xmm7;   {$ELSE}db $C5,$E1,$59,$DF;{$ENDIF} // dest*beta

       {$IFDEF FPC}vaddpd xmm3, xmm3, xmm0;{$ELSE}db $C5,$E1,$58,$D8;{$ENDIF} 
       {$IFDEF FPC}vmovhlps xmm4, xmm4, xmm3;{$ELSE}db $C5,$D8,$12,$E3;{$ENDIF} 
       {$IFDEF FPC}vmovsd [rcx], xmm3;{$ELSE}db $C5,$FB,$11,$19;{$ENDIF} 
       {$IFDEF FPC}vmovsd [rcx + rdx], xmm4;{$ELSE}db $C5,$FB,$11,$24,$11;{$ENDIF} 
       add rcx, rdx;
       add rcx, rdx;

       // second two
       {$IFDEF FPC}vmovsd xmm3, [rcx];{$ELSE}db $C5,$FB,$10,$19;{$ENDIF} 
       {$IFDEF FPC}vmovsd xmm4, [rcx + rdx];{$ELSE}db $C5,$FB,$10,$24,$11;{$ENDIF} 
       {$IFDEF FPC}vmovlhps xmm3, xmm3, xmm4;{$ELSE}db $C5,$E0,$16,$DC;{$ENDIF} 

       //movupd xmm0, res1;
       {$IFDEF FPC}vextractf128 xmm0, ymm8, 1;{$ELSE}db $C4,$63,$7D,$19,$C0,$01;{$ENDIF} 
       {$IFDEF FPC}vmulpd xmm0, xmm0, xmm6;   {$ELSE}db $C5,$F9,$59,$C6;{$ENDIF} // alpha*res
       {$IFDEF FPC}vmulpd xmm3, xmm3, xmm7;   {$ELSE}db $C5,$E1,$59,$DF;{$ENDIF} // dest*beta

       {$IFDEF FPC}vaddpd xmm3, xmm3, xmm0;{$ELSE}db $C5,$E1,$58,$D8;{$ENDIF} 
       {$IFDEF FPC}vmovhlps xmm4, xmm4, xmm3;{$ELSE}db $C5,$D8,$12,$E3;{$ENDIF} 
       {$IFDEF FPC}vmovsd [rcx], xmm3;{$ELSE}db $C5,$FB,$11,$19;{$ENDIF} 
       {$IFDEF FPC}vmovsd [rcx + rdx], xmm4;{$ELSE}db $C5,$FB,$11,$24,$11;{$ENDIF} 

       add rcx, rdx;
       add rcx, rdx;

       // third two
       {$IFDEF FPC}vmovsd xmm3, [rcx];{$ELSE}db $C5,$FB,$10,$19;{$ENDIF} 
       {$IFDEF FPC}vmovsd xmm4, [rcx + rdx];{$ELSE}db $C5,$FB,$10,$24,$11;{$ENDIF} 
       {$IFDEF FPC}vmovlhps xmm3, xmm3, xmm4;{$ELSE}db $C5,$E0,$16,$DC;{$ENDIF} 

       {$IFDEF FPC}vmovapd xmm0, xmm9;{$ELSE}db $C5,$79,$29,$C8;{$ENDIF} 
       {$IFDEF FPC}vmulpd xmm0, xmm0, xmm6;   {$ELSE}db $C5,$F9,$59,$C6;{$ENDIF} // alpha*res
       {$IFDEF FPC}vmulpd xmm3, xmm3, xmm7;   {$ELSE}db $C5,$E1,$59,$DF;{$ENDIF} // dest*beta

       {$IFDEF FPC}vaddpd xmm3, xmm3, xmm0;{$ELSE}db $C5,$E1,$58,$D8;{$ENDIF} 
       {$IFDEF FPC}vmovhlps xmm4, xmm4, xmm3;{$ELSE}db $C5,$D8,$12,$E3;{$ENDIF} 
       {$IFDEF FPC}vmovsd [rcx], xmm3;{$ELSE}db $C5,$FB,$11,$19;{$ENDIF} 
       {$IFDEF FPC}vmovsd [rcx + rdx], xmm4;{$ELSE}db $C5,$FB,$11,$24,$11;{$ENDIF} 

       add rcx, rdx;
       add rcx, rdx;

       // forth two
       {$IFDEF FPC}vmovsd xmm3, [rcx];{$ELSE}db $C5,$FB,$10,$19;{$ENDIF} 
       {$IFDEF FPC}vmovsd xmm4, [rcx + rdx];{$ELSE}db $C5,$FB,$10,$24,$11;{$ENDIF} 
       {$IFDEF FPC}vmovlhps xmm3, xmm3, xmm4;{$ELSE}db $C5,$E0,$16,$DC;{$ENDIF} 

       //movupd xmm0, res3;
       {$IFDEF FPC}vextractf128 xmm0, ymm9, 1;{$ELSE}db $C4,$63,$7D,$19,$C8,$01;{$ENDIF} 
       {$IFDEF FPC}vmulpd xmm0, xmm0, xmm6;   {$ELSE}db $C5,$F9,$59,$C6;{$ENDIF} // alpha*res
       {$IFDEF FPC}vmulpd xmm3, xmm3, xmm7;   {$ELSE}db $C5,$E1,$59,$DF;{$ENDIF} // dest*beta

       {$IFDEF FPC}vaddpd xmm3, xmm3, xmm0;{$ELSE}db $C5,$E1,$58,$D8;{$ENDIF} 
       {$IFDEF FPC}vmovhlps xmm4, xmm4, xmm3;{$ELSE}db $C5,$D8,$12,$E3;{$ENDIF} 
       {$IFDEF FPC}vmovsd [rcx], xmm3;{$ELSE}db $C5,$FB,$11,$19;{$ENDIF} 
       {$IFDEF FPC}vmovsd [rcx + rdx], xmm4;{$ELSE}db $C5,$FB,$11,$24,$11;{$ENDIF} 

       add rcx, rdx;
       add rcx, rdx;

       // fith two
       {$IFDEF FPC}vmovsd xmm3, [rcx];{$ELSE}db $C5,$FB,$10,$19;{$ENDIF} 
       {$IFDEF FPC}vmovsd xmm4, [rcx + rdx];{$ELSE}db $C5,$FB,$10,$24,$11;{$ENDIF} 
       {$IFDEF FPC}vmovlhps xmm3, xmm3, xmm4;{$ELSE}db $C5,$E0,$16,$DC;{$ENDIF} 

       //movupd xmm0, res4;
       {$IFDEF FPC}vmovapd xmm0, xmm10;{$ELSE}db $C5,$79,$29,$D0;{$ENDIF} 
       {$IFDEF FPC}vmulpd xmm0, xmm0, xmm6;   {$ELSE}db $C5,$F9,$59,$C6;{$ENDIF} // alpha*res
       {$IFDEF FPC}vmulpd xmm3, xmm3, xmm7;   {$ELSE}db $C5,$E1,$59,$DF;{$ENDIF} // dest*beta

       {$IFDEF FPC}vaddpd xmm3, xmm3, xmm0;{$ELSE}db $C5,$E1,$58,$D8;{$ENDIF} 
       {$IFDEF FPC}vmovhlps xmm4, xmm4, xmm3;{$ELSE}db $C5,$D8,$12,$E3;{$ENDIF} 
       {$IFDEF FPC}vmovsd [rcx], xmm3;{$ELSE}db $C5,$FB,$11,$19;{$ENDIF} 
       {$IFDEF FPC}vmovsd [rcx + rdx], xmm4;{$ELSE}db $C5,$FB,$11,$24,$11;{$ENDIF} 

       add rcx, rdx;
       add rcx, rdx;

       // sixth two
       {$IFDEF FPC}vmovsd xmm3, [rcx];{$ELSE}db $C5,$FB,$10,$19;{$ENDIF} 
       {$IFDEF FPC}vmovsd xmm4, [rcx + rdx];{$ELSE}db $C5,$FB,$10,$24,$11;{$ENDIF} 
       {$IFDEF FPC}vmovlhps xmm3, xmm3, xmm4;{$ELSE}db $C5,$E0,$16,$DC;{$ENDIF} 

       //movupd xmm0, res5;
       {$IFDEF FPC}vextractf128 xmm0, ymm10, 1;{$ELSE}db $C4,$63,$7D,$19,$D0,$01;{$ENDIF} 
       {$IFDEF FPC}vmulpd xmm0, xmm0, xmm6;   {$ELSE}db $C5,$F9,$59,$C6;{$ENDIF} // alpha*res
       {$IFDEF FPC}vmulpd xmm3, xmm3, xmm7;   {$ELSE}db $C5,$E1,$59,$DF;{$ENDIF} // dest*beta

       {$IFDEF FPC}vaddpd xmm3, xmm3, xmm0;{$ELSE}db $C5,$E1,$58,$D8;{$ENDIF} 
       {$IFDEF FPC}vmovhlps xmm4, xmm4, xmm3;{$ELSE}db $C5,$D8,$12,$E3;{$ENDIF} 
       {$IFDEF FPC}vmovsd [rcx], xmm3;{$ELSE}db $C5,$FB,$11,$19;{$ENDIF} 
       {$IFDEF FPC}vmovsd [rcx + rdx], xmm4;{$ELSE}db $C5,$FB,$11,$24,$11;{$ENDIF} 

       add rcx, rdx;
       add rcx, rdx;

       // seventh two
       {$IFDEF FPC}vmovsd xmm3, [rcx];{$ELSE}db $C5,$FB,$10,$19;{$ENDIF} 
       {$IFDEF FPC}vmovsd xmm4, [rcx + rdx];{$ELSE}db $C5,$FB,$10,$24,$11;{$ENDIF} 
       {$IFDEF FPC}vmovlhps xmm3, xmm3, xmm4;{$ELSE}db $C5,$E0,$16,$DC;{$ENDIF} 

       //movupd xmm0, res6;
       {$IFDEF FPC}vmovapd xmm0, xmm11;{$ELSE}db $C5,$79,$29,$D8;{$ENDIF} 
       {$IFDEF FPC}vmulpd xmm0, xmm0, xmm6;   {$ELSE}db $C5,$F9,$59,$C6;{$ENDIF} // alpha*res
       {$IFDEF FPC}vmulpd xmm3, xmm3, xmm7;   {$ELSE}db $C5,$E1,$59,$DF;{$ENDIF} // dest*beta

       {$IFDEF FPC}vaddpd xmm3, xmm3, xmm0;{$ELSE}db $C5,$E1,$58,$D8;{$ENDIF} 
       {$IFDEF FPC}vmovhlps xmm4, xmm4, xmm3;{$ELSE}db $C5,$D8,$12,$E3;{$ENDIF} 
       {$IFDEF FPC}vmovsd [rcx], xmm3;{$ELSE}db $C5,$FB,$11,$19;{$ENDIF} 
       {$IFDEF FPC}vmovsd [rcx + rdx], xmm4;{$ELSE}db $C5,$FB,$11,$24,$11;{$ENDIF} 

       add rcx, rdx;
       add rcx, rdx;

       // eighth two
       {$IFDEF FPC}vmovsd xmm3, [rcx];{$ELSE}db $C5,$FB,$10,$19;{$ENDIF} 
       {$IFDEF FPC}vmovsd xmm4, [rcx + rdx];{$ELSE}db $C5,$FB,$10,$24,$11;{$ENDIF} 
       {$IFDEF FPC}vmovlhps xmm3, xmm3, xmm4;{$ELSE}db $C5,$E0,$16,$DC;{$ENDIF} 

       //movupd xmm0, res7;
       {$IFDEF FPC}vextractf128 xmm0, ymm11, 1;{$ELSE}db $C4,$63,$7D,$19,$D8,$01;{$ENDIF} 
       {$IFDEF FPC}vmulpd xmm0, xmm0, xmm6;   {$ELSE}db $C5,$F9,$59,$C6;{$ENDIF} // alpha*res
       {$IFDEF FPC}vmulpd xmm3, xmm3, xmm7;   {$ELSE}db $C5,$E1,$59,$DF;{$ENDIF} // dest*beta

       {$IFDEF FPC}vaddpd xmm3, xmm3, xmm0;{$ELSE}db $C5,$E1,$58,$D8;{$ENDIF} 
       {$IFDEF FPC}vmovhlps xmm4, xmm4, xmm3;{$ELSE}db $C5,$D8,$12,$E3;{$ENDIF} 
       {$IFDEF FPC}vmovsd [rcx], xmm3;{$ELSE}db $C5,$FB,$11,$19;{$ENDIF} 
       {$IFDEF FPC}vmovsd [rcx + rdx], xmm4;{$ELSE}db $C5,$FB,$11,$24,$11;{$ENDIF} 

       add rcx, rdx;
       add rcx, rdx;

       // next results:
       add r8, 8*16;      // next mt1 element
   sub r14, 16;
   jns @@forxloop;

   {$IFDEF FPC}vmovupd ymm8, [rsp + $00];{$ELSE}db $C5,$7D,$10,$04,$24;{$ENDIF} 
   {$IFDEF FPC}vmovupd ymm9, [rsp + $10];{$ELSE}db $C5,$7D,$10,$4C,$24,$10;{$ENDIF} 
   {$IFDEF FPC}vmovupd ymm10, [rsp + $20];{$ELSE}db $C5,$7D,$10,$54,$24,$20;{$ENDIF} 
   {$IFDEF FPC}vmovupd ymm11, [rsp + $30];{$ELSE}db $C5,$7D,$10,$5C,$24,$30;{$ENDIF} 
   add rsp, $40;

   @@forxloopend:

   // ###########################################
   // elements that not fit into mod 16
   add r14, 16;
   jz @@vecaddend;

   @@forxshortloop:
       {$IFDEF FPC}vxorpd xmm0, xmm0, xmm0;  {$ELSE}db $C5,$F9,$57,$C0;{$ENDIF} // first two elements

       mov rax, r8;       // rax = first matrix element
       mov rbx, r9;       // rbx = first vector element

       mov r10, r13;

       @@forshortyloop:
           {$IFDEF FPC}vmovsd xmm1, [rax];{$ELSE}db $C5,$FB,$10,$08;{$ENDIF} 
           {$IFDEF FPC}vmovsd xmm2, [rbx];{$ELSE}db $C5,$FB,$10,$13;{$ENDIF} 

           {$IFDEF FPC}vmulsd xmm1, xmm1, xmm2;{$ELSE}db $C5,$F3,$59,$CA;{$ENDIF} 
           {$IFDEF FPC}vaddsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$58,$C1;{$ENDIF} 

           add rax, rsi;
           add rbx, rdi;

       dec r10;
       jnz @@forshortyloop;

       {$IFDEF FPC}vmulsd xmm0, xmm0, xmm6;  {$ELSE}db $C5,$FB,$59,$C6;{$ENDIF} // alpha*res

       {$IFDEF FPC}vmovsd xmm3, [rcx];{$ELSE}db $C5,$FB,$10,$19;{$ENDIF} 
       {$IFDEF FPC}vmulsd xmm3, xmm3, xmm7;  {$ELSE}db $C5,$E3,$59,$DF;{$ENDIF} //dest*beta
       {$IFDEF FPC}vaddsd xmm0, xmm0, xmm3;{$ELSE}db $C5,$FB,$58,$C3;{$ENDIF} 
       {$IFDEF FPC}vmovsd [rcx], xmm0;{$ELSE}db $C5,$FB,$11,$01;{$ENDIF} 

       // next row
       add rcx, rdx;
       add r8, 8;

   dec r14;
   jnz @@forxshortloop;


   @@vecaddend:

   // epilog pop "stack"
   mov rbx, iRBX;
   mov rsi, iRSI;
   mov rdi, iRDI;
   mov r12, iR12;
   mov r13, iR13;
   mov r14, iR14;

   {$IFDEF FPC}vmovupd xmm4, [rsp + $00];{$ELSE}db $C5,$F9,$10,$24,$24;{$ENDIF} 
   {$IFDEF FPC}vmovupd xmm5, [rsp + $10];{$ELSE}db $C5,$F9,$10,$6C,$24,$10;{$ENDIF} 
   {$IFDEF FPC}vmovupd xmm6, [rsp + $20];{$ELSE}db $C5,$F9,$10,$74,$24,$20;{$ENDIF} 
   {$IFDEF FPC}vmovupd xmm7, [rsp + $30];{$ELSE}db $C5,$F9,$10,$7C,$24,$30;{$ENDIF} 
   add rsp, $40;
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;


procedure FMAMatrixVecMultTDestVec(dest : PDouble; destLineWidth : NativeInt; mt1, v : PDouble; {$ifdef UNIX}unixLineWidthMT{$ELSE}LineWidthMT{$endif}, {$ifdef UNIX}unixLineWidthV{$ELSE}LineWidthV{$endif} : NativeInt; width, height : NativeInt; {$ifdef UNIX}unixalpha {$ELSE}alpha{$ENDIF}, {$ifdef UNIX}unixbeta {$ELSE}beta{$ENDIF} : double); {$IFDEF FPC}assembler;{$ENDIF}
var iRBX, iRSI, iRDI, iR12, iR13, iR14 : NativeInt;
    {$ifdef UNIX}
    LineWidthMT, LineWidthV : NativeInt;
    alpha, beta : double;
    {$endif}
asm
   {$IFDEF UNIX}
   // Linux uses a diffrent ABI -> copy over the registers so they meet with winABI
   // (note that the 5th and 6th parameter are are on the stack)
   // The parameters are passed in the following order:
   // RDI, RSI, RDX, RCX -> mov to RCX, RDX, R8, R9
   mov LineWidthMT, r8;
   mov LineWidthV, r9;
   mov r8, rdx;
   mov r9, rcx;
   mov rcx, rdi;
   mov rdx, rsi;
   movsd alpha, unixalpha;
   movsd beta, unixbeta;
   {$ENDIF}

   // prolog - simulate stack
   mov iRBX, rbx;
   mov iRSI, rsi;
   mov iRDI, rdi;
   mov iR12, r12;
   mov iR13, r13;
   mov iR14, r14;

   sub rsp, $40;
   {$IFDEF FPC}vmovupd [rsp + $00], xmm4;{$ELSE}db $C5,$F9,$11,$24,$24;{$ENDIF} 
   {$IFDEF FPC}vmovupd [rsp + $10], xmm5;{$ELSE}db $C5,$F9,$11,$6C,$24,$10;{$ENDIF} 
   {$IFDEF FPC}vmovupd [rsp + $20], xmm6;{$ELSE}db $C5,$F9,$11,$74,$24,$20;{$ENDIF} 
   {$IFDEF FPC}vmovupd [rsp + $30], xmm7;{$ELSE}db $C5,$F9,$11,$7C,$24,$30;{$ENDIF} 


   // for the final multiplication
   lea rax, alpha;
   {$IFDEF FPC}vbroadcastsd ymm6, [rax];{$ELSE}db $C4,$E2,$7D,$19,$30;{$ENDIF} 
   lea rax, beta;
   {$IFDEF FPC}vbroadcastsd ymm7, [rax];{$ELSE}db $C4,$E2,$7D,$19,$38;{$ENDIF} 

   // prepare for loop
   mov rsi, LineWidthMT;
   mov rdi, LineWidthV;
   mov r13, height;
   mov r14, width;

   sub r14, 16;
   js @@forxloopend;

   sub rsp, $40;
   {$IFDEF FPC}vmovupd [rsp + $00], ymm8;{$ELSE}db $C5,$7D,$11,$04,$24;{$ENDIF} 
   {$IFDEF FPC}vmovupd [rsp + $10], ymm9;{$ELSE}db $C5,$7D,$11,$4C,$24,$10;{$ENDIF} 
   {$IFDEF FPC}vmovupd [rsp + $20], ymm10;{$ELSE}db $C5,$7D,$11,$54,$24,$20;{$ENDIF} 
   {$IFDEF FPC}vmovupd [rsp + $30], ymm11;{$ELSE}db $C5,$7D,$11,$5C,$24,$30;{$ENDIF} 

   // init for x := 0 to width - 1:
   @@forxloop:

       // init values:
       {$IFDEF FPC}vxorpd ymm8, ymm8, ymm8;{$ELSE}db $C4,$41,$3D,$57,$C0;{$ENDIF} 
       {$IFDEF FPC}vxorpd ymm9, ymm9, ymm9;{$ELSE}db $C4,$41,$35,$57,$C9;{$ENDIF} 
       {$IFDEF FPC}vxorpd ymm10, ymm10, ymm10;{$ELSE}db $C4,$41,$2D,$57,$D2;{$ENDIF} 
       {$IFDEF FPC}vxorpd ymm11, ymm11, ymm11;{$ELSE}db $C4,$41,$25,$57,$DB;{$ENDIF} 

       mov rax, r8;       // rax = first matrix element
       mov rbx, r9;       // rbx = first vector element

       mov r10, r13;
       @@foryloop:
           {$IFDEF FPC}vbroadcastsd ymm3, [rbx];{$ELSE}db $C4,$E2,$7D,$19,$1B;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm4, [rax];{$ELSE}db $C5,$FD,$10,$20;{$ENDIF} 
           {$IFDEF FPC}vfmadd231pd ymm8, ymm3, ymm4;{$ELSE}db $C4,$62,$E5,$B8,$C4;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm4, [rax + 32];{$ELSE}db $C5,$FD,$10,$60,$20;{$ENDIF} 
           {$IFDEF FPC}vfmadd231pd ymm9, ymm3, ymm4;{$ELSE}db $C4,$62,$E5,$B8,$CC;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm4, [rax + 64];{$ELSE}db $C5,$FD,$10,$60,$40;{$ENDIF} 
           {$IFDEF FPC}vfmadd231pd ymm10, ymm3, ymm4;{$ELSE}db $C4,$62,$E5,$B8,$D4;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm4, [rax + 96];{$ELSE}db $C5,$FD,$10,$60,$60;{$ENDIF} 
           {$IFDEF FPC}vfmadd231pd ymm11, ymm3, ymm4;{$ELSE}db $C4,$62,$E5,$B8,$DC;{$ENDIF} 

           add rax, rsi;
           add rbx, rdi;

       dec r10;
       jnz @@foryloop;

       // result building
       // write back result (final addition and compactation)

       // calculate dest = beta*dest + alpha*xmm0
       // first 4
       {$IFDEF FPC}vmovupd ymm3, [rcx];{$ELSE}db $C5,$FD,$10,$19;{$ENDIF} 

       {$IFDEF FPC}vmulpd ymm8, ymm8, ymm6; {$ELSE}db $C5,$3D,$59,$C6;{$ENDIF} // alpha*res
       {$IFDEF FPC}vmulpd ymm3, ymm3, ymm7; {$ELSE}db $C5,$E5,$59,$DF;{$ENDIF} // dest*beta
       {$IFDEF FPC}vaddpd ymm3, ymm3, ymm8;{$ELSE}db $C4,$C1,$65,$58,$D8;{$ENDIF} 
       {$IFDEF FPC}vmovupd [rcx], ymm3;{$ELSE}db $C5,$FD,$11,$19;{$ENDIF} 
       add rcx, 32;

       // second 4
       {$IFDEF FPC}vmovupd ymm3, [rcx];{$ELSE}db $C5,$FD,$10,$19;{$ENDIF} 

       {$IFDEF FPC}vmulpd ymm9, ymm9, ymm6; {$ELSE}db $C5,$35,$59,$CE;{$ENDIF} // alpha*res
       {$IFDEF FPC}vmulpd ymm3, ymm3, ymm7; {$ELSE}db $C5,$E5,$59,$DF;{$ENDIF} // dest*beta
       {$IFDEF FPC}vaddpd ymm3, ymm3, ymm9;{$ELSE}db $C4,$C1,$65,$58,$D9;{$ENDIF} 
       {$IFDEF FPC}vmovupd [rcx], ymm3;{$ELSE}db $C5,$FD,$11,$19;{$ENDIF} 
       add rcx, 32;

       // third 4
       {$IFDEF FPC}vmovupd ymm3, [rcx];{$ELSE}db $C5,$FD,$10,$19;{$ENDIF} 

       {$IFDEF FPC}vmulpd ymm10, ymm10, ymm6; {$ELSE}db $C5,$2D,$59,$D6;{$ENDIF} // alpha*res
       {$IFDEF FPC}vmulpd ymm3, ymm3, ymm7; {$ELSE}db $C5,$E5,$59,$DF;{$ENDIF} // dest*beta
       {$IFDEF FPC}vaddpd ymm3, ymm3, ymm10;{$ELSE}db $C4,$C1,$65,$58,$DA;{$ENDIF} 
       {$IFDEF FPC}vmovupd [rcx], ymm3;{$ELSE}db $C5,$FD,$11,$19;{$ENDIF} 
       add rcx, 32;

       // forth 4
       {$IFDEF FPC}vmovupd ymm3, [rcx];{$ELSE}db $C5,$FD,$10,$19;{$ENDIF} 

       {$IFDEF FPC}vmulpd ymm11, ymm11, ymm6; {$ELSE}db $C5,$25,$59,$DE;{$ENDIF} // alpha*res
       {$IFDEF FPC}vmulpd ymm3, ymm3, ymm7; {$ELSE}db $C5,$E5,$59,$DF;{$ENDIF} // dest*beta
       {$IFDEF FPC}vaddpd ymm3, ymm3, ymm11;{$ELSE}db $C4,$C1,$65,$58,$DB;{$ENDIF} 
       {$IFDEF FPC}vmovupd [rcx], ymm3;{$ELSE}db $C5,$FD,$11,$19;{$ENDIF} 
       add rcx, 32;

       // next results:
       add r8, 8*16;      // next mt1 element
   sub r14, 16;
   jns @@forxloop;

   {$IFDEF FPC}vmovupd ymm8, [rsp + $00];{$ELSE}db $C5,$7D,$10,$04,$24;{$ENDIF} 
   {$IFDEF FPC}vmovupd ymm9, [rsp + $10];{$ELSE}db $C5,$7D,$10,$4C,$24,$10;{$ENDIF} 
   {$IFDEF FPC}vmovupd ymm10, [rsp + $20];{$ELSE}db $C5,$7D,$10,$54,$24,$20;{$ENDIF} 
   {$IFDEF FPC}vmovupd ymm11, [rsp + $30];{$ELSE}db $C5,$7D,$10,$5C,$24,$30;{$ENDIF} 
   add rsp, $40;

   @@forxloopend:

   // ###########################################
   // elements that not fit into mod 16
   add r14, 16;
   jz @@vecaddend;

   @@forxshortloop:
       {$IFDEF FPC}vxorpd xmm0, xmm0, xmm0;  {$ELSE}db $C5,$F9,$57,$C0;{$ENDIF} // first two elements

       mov rax, r8;       // rax = first matrix element
       mov rbx, r9;       // rbx = first vector element

       mov r10, r13;

       @@forshortyloop:
           {$IFDEF FPC}vmovsd xmm1, [rax];{$ELSE}db $C5,$FB,$10,$08;{$ENDIF} 
           {$IFDEF FPC}vmovsd xmm2, [rbx];{$ELSE}db $C5,$FB,$10,$13;{$ENDIF} 

           {$IFDEF FPC}vmulsd xmm1, xmm1, xmm2;{$ELSE}db $C5,$F3,$59,$CA;{$ENDIF} 
           {$IFDEF FPC}vaddsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$58,$C1;{$ENDIF} 

           add rax, rsi;
           add rbx, rdi;

       dec r10;
       jnz @@forshortyloop;

       {$IFDEF FPC}vmulsd xmm0, xmm0, xmm6;  {$ELSE}db $C5,$FB,$59,$C6;{$ENDIF} // alpha*res

       {$IFDEF FPC}vmovsd xmm3, [rcx];{$ELSE}db $C5,$FB,$10,$19;{$ENDIF} 
       {$IFDEF FPC}vmulsd xmm3, xmm3, xmm7;  {$ELSE}db $C5,$E3,$59,$DF;{$ENDIF} //dest*beta
       {$IFDEF FPC}vaddsd xmm0, xmm0, xmm3;{$ELSE}db $C5,$FB,$58,$C3;{$ENDIF} 
       {$IFDEF FPC}vmovsd [rcx], xmm0;{$ELSE}db $C5,$FB,$11,$01;{$ENDIF} 

       // next column
       add rcx, rdx;
       add r8, 8;

   dec r14;
   jnz @@forxshortloop;


   @@vecaddend:

   // epilog pop "stack"
   mov rbx, iRBX;
   mov rsi, iRSI;
   mov rdi, iRDI;
   mov r12, iR12;
   mov r13, iR13;
   mov r14, iR14;

   {$IFDEF FPC}vmovupd xmm4, [rsp + $00];{$ELSE}db $C5,$F9,$10,$24,$24;{$ENDIF} 
   {$IFDEF FPC}vmovupd xmm5, [rsp + $10];{$ELSE}db $C5,$F9,$10,$6C,$24,$10;{$ENDIF} 
   {$IFDEF FPC}vmovupd xmm6, [rsp + $20];{$ELSE}db $C5,$F9,$10,$74,$24,$20;{$ENDIF} 
   {$IFDEF FPC}vmovupd xmm7, [rsp + $30];{$ELSE}db $C5,$F9,$10,$7C,$24,$30;{$ENDIF} 
   add rsp, $40;
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;


procedure FMARank1UpdateSeq(A : PDouble; const LineWidthA : NativeInt; width, height : NativeInt;
  {$ifdef UNIX}unixX{$ELSE}X{$endif}, {$ifdef UNIX}unixY{$ELSE}Y{$ENDIF} : PDouble; incX, incY : NativeInt; {$ifdef UNIX}unixalpha {$ELSE}alpha{$ENDIF} : double); {$IFDEF FPC}assembler;{$ENDIF}
// note: RCX = A, RDX = LineWidthA, R8 = width, R9 = height
var iRBX, iRSI, iRDI, iR12, iR13, iR14 : NativeInt;
    {$ifdef UNIX}
    X, Y : PDouble;
    alpha : double;
    {$endif}
asm
   {$IFDEF UNIX}
   // Linux uses a diffrent ABI -> copy over the registers so they meet with winABI
   // (note that the 5th and 6th parameter are are on the stack)
   // The parameters are passed in the following order:
   // RDI, RSI, RDX, RCX -> mov to RCX, RDX, R8, R9
   mov X, r8;
   mov Y, r9;
   mov r8, rdx;
   mov r9, rcx;
   mov rcx, rdi;
   mov rdx, rsi;
   movsd alpha, unixalpha;
   {$ENDIF}

   // prolog - simulate stack
   mov iRBX, rbx;
   mov iRSI, rsi;
   mov iRDI, rdi;
   mov iR12, r12;
   mov iR13, r13;
   mov iR14, r14;

   mov r12, r8;
   sar r12, 2;   // width div 4

   // performs A = A + alpha*X*Y' in row major form

   mov rdi, X;
   mov r13, Y;

   // for the temp calculation
   lea rax, alpha;
   {$IFDEF FPC}vbroadcastsd ymm3, [rax];{$ELSE}db $C4,$E2,$7D,$19,$18;{$ENDIF} 

   // prepare for loop
   mov rsi, incx;
   // init for y := 0 to height - 1:
   @@foryloop:
      // init values:
      {$IFDEF FPC}vbroadcastsd ymm0, [rdi];  {$ELSE}db $C4,$E2,$7D,$19,$07;{$ENDIF} // res := 0;
      {$IFDEF FPC}vmulpd ymm0, ymm0, ymm3;     {$ELSE}db $C5,$FD,$59,$C3;{$ENDIF} // tmp := alpha*pX^
      mov rax, rcx;              // eax = first destination element A
      mov rbx, r13;           // ebx = first y vector element

      // for j := 0 to width - 1 do
      mov r14, r12;
      test r14, r14;
      jz @@last3Elem;

      @@forxloop:
         {$IFDEF FPC}vmovupd ymm1, [rax];{$ELSE}db $C5,$FD,$10,$08;{$ENDIF} 
         {$IFDEF FPC}vmovupd ymm2, [rbx];{$ELSE}db $C5,$FD,$10,$13;{$ENDIF} 

         // pA^[j] := pA^[j] + tmp*pY1^[j];
         {$IFDEF FPC}vfmadd231pd ymm1, ymm0, ymm2;{$ELSE}db $C4,$E2,$FD,$B8,$CA;{$ENDIF} 

         {$IFDEF FPC}vmovupd [rax], ymm1;{$ELSE}db $C5,$FD,$11,$08;{$ENDIF} 

         add rax, 32;
         add rbx, 32;

      dec r14;
      jnz @@forxloop;

      @@last3Elem:
      mov r14, r8;
      and r14, $3;
      jz @@nextline;

      // check if there is only one element to process
      cmp r14, 1;
      je @@lastElem;

      // handle 2 elements
      {$IFDEF FPC}vmovupd xmm1, [rax];{$ELSE}db $C5,$F9,$10,$08;{$ENDIF} 
      {$IFDEF FPC}vmovupd xmm2, [rbx];{$ELSE}db $C5,$F9,$10,$13;{$ENDIF} 

      {$IFDEF FPC}vfmadd231pd xmm1, xmm0, xmm2;{$ELSE}db $C4,$E2,$F9,$B8,$CA;{$ENDIF} 
      {$IFDEF FPC}vmovupd [rax], xmm1;{$ELSE}db $C5,$F9,$11,$08;{$ENDIF} 
      add rax, 16;
      add rbx, 16;

      cmp r14, 2;
      je @@nextline;

      @@lastElem:

      {$IFDEF FPC}vmovsd xmm1, [rax];{$ELSE}db $C5,$FB,$10,$08;{$ENDIF} 
      {$IFDEF FPC}vmovsd xmm2, [rbx];{$ELSE}db $C5,$FB,$10,$13;{$ENDIF} 

      {$IFDEF FPC}vmulsd xmm2, xmm2, xmm0;{$ELSE}db $C5,$EB,$59,$D0;{$ENDIF} 
      {$IFDEF FPC}vaddsd xmm1, xmm1, xmm2;{$ELSE}db $C5,$F3,$58,$CA;{$ENDIF} 
      {$IFDEF FPC}vmovsd [rax], xmm1;{$ELSE}db $C5,$FB,$11,$08;{$ENDIF} 

      @@nextline:

      // next results:
      add rdi, rsi;
      add rcx, rdx;
   dec r9;          // r9 = height
   jnz @@foryloop;

   // epilog
   mov rbx, iRBX;
   mov rsi, iRSI;
   mov rdi, iRDI;
   mov r12, iR12;
   mov r13, iR13;
   mov r14, iR14;
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;


procedure FMARank1UpdateSeqAligned(A : PDouble; const LineWidthA : NativeInt; width, height : NativeInt;
  {$ifdef UNIX}unixX{$ELSE}X{$endif}, {$ifdef UNIX}unixY{$ELSE}Y{$ENDIF} : PDouble; incX, incY : NativeInt; {$ifdef UNIX}unixalpha {$ELSE}alpha{$ENDIF} : double); {$IFDEF FPC}assembler;{$ENDIF}
// note: RCX = A, RDX = LineWidthA, R8 = width, R9 = height
var iRBX, iRSI, iRDI, iR12, iR13, iR14 : NativeInt;
    {$ifdef UNIX}
    X, Y : PDouble;
    alpha : double;
    {$endif}
asm
   {$IFDEF UNIX}
   // Linux uses a diffrent ABI -> copy over the registers so they meet with winABI
   // (note that the 5th and 6th parameter are are on the stack)
   // The parameters are passed in the following order:
   // RDI, RSI, RDX, RCX -> mov to RCX, RDX, R8, R9
   mov X, r8;
   mov Y, r9;
   mov r8, rdx;
   mov r9, rcx;
   mov rcx, rdi;
   mov rdx, rsi;
   movsd alpha, unixalpha;
   {$ENDIF}

   // prolog - simulate stack
   mov iRBX, rbx;
   mov iRSI, rsi;
   mov iRDI, rdi;
   mov iR12, r12;
   mov iR13, r13;
   mov iR14, r14;

   mov r12, r8;
   sar r12, 2;   // width div 4

   // performs A = A + alpha*X*Y' in row major form

   mov rdi, X;
   mov r13, Y;

   // for the temp calculation
   lea rax, alpha;
   {$IFDEF FPC}vbroadcastsd ymm3, [rax];{$ELSE}db $C4,$E2,$7D,$19,$18;{$ENDIF} 

   // prepare for loop
   mov rsi, incx;
   // init for y := 0 to height - 1:
   @@foryloop:
      // init values:
      {$IFDEF FPC}vbroadcastsd ymm0, [rdi];  {$ELSE}db $C4,$E2,$7D,$19,$07;{$ENDIF} // res := 0;
      {$IFDEF FPC}vmulpd ymm0, ymm0, ymm3;     {$ELSE}db $C5,$FD,$59,$C3;{$ENDIF} // tmp := alpha*pX^
      mov rax, rcx;              // eax = first destination element A
      mov rbx, r13;           // ebx = first y vector element

      // for j := 0 to width - 1 do
      mov r14, r12;
      test r14, r14;
      jz @@last3Elem;

      @@forxloop:
         {$IFDEF FPC}vmovapd ymm1, [rax];{$ELSE}db $C5,$FD,$28,$08;{$ENDIF} 
         {$IFDEF FPC}vmovapd ymm2, [rbx];{$ELSE}db $C5,$FD,$28,$13;{$ENDIF} 

         // pA^[j] := pA^[j] + tmp*pY1^[j];
         {$IFDEF FPC}vfmadd231pd ymm1, ymm0, ymm2;{$ELSE}db $C4,$E2,$FD,$B8,$CA;{$ENDIF} 

         {$IFDEF FPC}vmovapd [rax], ymm1;{$ELSE}db $C5,$FD,$29,$08;{$ENDIF} 

         add rax, 32;
         add rbx, 32;

      dec r14;
      jnz @@forxloop;

      @@last3Elem:
      mov r14, r8;
      and r14, $3;
      jz @@nextline;

      // check if there is only one element to process
      cmp r14, 1;
      je @@lastElem;

      // handle 2 elements
      {$IFDEF FPC}vmovapd xmm1, [rax];{$ELSE}db $C5,$F9,$28,$08;{$ENDIF} 
      {$IFDEF FPC}vmovapd xmm2, [rbx];{$ELSE}db $C5,$F9,$28,$13;{$ENDIF} 

      {$IFDEF FPC}vfmadd231pd xmm1, xmm0, xmm2;{$ELSE}db $C4,$E2,$F9,$B8,$CA;{$ENDIF} 
      {$IFDEF FPC}vmovapd [rax], xmm1;{$ELSE}db $C5,$F9,$29,$08;{$ENDIF} 
      add rax, 16;
      add rbx, 16;

      cmp r14, 2;
      je @@nextline;

      @@lastElem:

      {$IFDEF FPC}vmovsd xmm1, [rax];{$ELSE}db $C5,$FB,$10,$08;{$ENDIF} 
      {$IFDEF FPC}vmovsd xmm2, [rbx];{$ELSE}db $C5,$FB,$10,$13;{$ENDIF} 

      {$IFDEF FPC}vmulsd xmm2, xmm2, xmm0;{$ELSE}db $C5,$EB,$59,$D0;{$ENDIF} 
      {$IFDEF FPC}vaddsd xmm1, xmm1, xmm2;{$ELSE}db $C5,$F3,$58,$CA;{$ENDIF} 
      {$IFDEF FPC}vmovsd [rax], xmm1;{$ELSE}db $C5,$FB,$11,$08;{$ENDIF} 

      @@nextline:

      // next results:
      add rdi, rsi;
      add rcx, rdx;
   dec r9;          // r9 = height
   jnz @@foryloop;

   // epilog
   mov rbx, iRBX;
   mov rsi, iRSI;
   mov rdi, iRDI;
   mov r12, iR12;
   mov r13, iR13;
   mov r14, iR14;
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;

{$ENDIF}

end.
