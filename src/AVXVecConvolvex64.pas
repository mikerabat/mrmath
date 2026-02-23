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

unit AVXVecConvolvex64;

interface

{$I 'mrMath_CPU.inc'}

{$IFDEF x64}

// simple convolution: the input and output parameter are assumed to be vectors!
// it's also assumed that memory before A is accessible for at least bLen elements
// -> these elements are used for the convulution calculation
// -> needs an aligned B and blen mod 2 needs to be zero
procedure AVXVecConvolveRevB(dest : PDouble; A, B : PDouble; aLen, {$ifdef UNIX}unixbLen{$ELSE}bLen{$endif} : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}

{$ENDIF}

implementation

{$IFDEF x64}

procedure AVXVecConvolveRevB(dest : PDouble; A, B : PDouble; aLen, {$ifdef UNIX}unixbLen{$ELSE}bLen{$endif} : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}
{$ifdef UNIX}
var bLen : NativeInt;
{$endif}
asm
   {$IFDEF UNIX}
   // Linux uses a diffrent ABI -> copy over the registers so they meet with winABI
   // (note that the 5th and 6th parameter are are on the stack)
   // The parameters are passed in the following order:
   // RDI, RSI, RDX, RCX -> mov to RCX, RDX, R8, R9
   mov bLen, r8;
   mov r8, rdx;
   mov r9, rcx;
   mov rcx, rdi;
   mov rdx, rsi;
   {$ENDIF}

   mov r10, bLen;
   imul r10, -8;

   // rdx=A, r8 = B;
   add rdx, 8;
   sub r8, r10;

   @@forxloop:
      mov rax, r10;

      {$IFDEF AVXSUP}vxorpd ymm0, ymm0, ymm0;                         {$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} 

      // unrolled part
      @@innerLoopUnrolled:
         add rax, 128;
         jg @@innerLoopStart;

         {$IFDEF AVXSUP}vmovupd ymm1, [rdx + rax - 128];              {$ELSE}db $C5,$FD,$10,$4C,$02,$80;{$ENDIF} 
         {$IFDEF AVXSUP}vmovapd ymm2, [r8 + rax - 128];               {$ELSE}db $C4,$C1,$7D,$28,$54,$00,$80;{$ENDIF} 

         {$IFDEF AVXSUP}vmulpd ymm1, ymm1, ymm2;                      {$ELSE}db $C5,$F5,$59,$CA;{$ENDIF} 
         {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm1;                      {$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 

         {$IFDEF AVXSUP}vmovupd ymm3, [rdx + rax - 96];               {$ELSE}db $C5,$FD,$10,$5C,$02,$A0;{$ENDIF} 
         {$IFDEF AVXSUP}vmovapd ymm4, [r8 + rax - 96];                {$ELSE}db $C4,$C1,$7D,$28,$64,$00,$A0;{$ENDIF} 

         {$IFDEF AVXSUP}vmulpd ymm3, ymm3, ymm4;                      {$ELSE}db $C5,$E5,$59,$DC;{$ENDIF} 
         {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm3;                      {$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 

         {$IFDEF AVXSUP}vmovupd ymm1, [rdx + rax - 64];               {$ELSE}db $C5,$FD,$10,$4C,$02,$C0;{$ENDIF} 
         {$IFDEF AVXSUP}vmovapd ymm2, [r8 + rax - 64];                {$ELSE}db $C4,$C1,$7D,$28,$54,$00,$C0;{$ENDIF} 

         {$IFDEF AVXSUP}vmulpd ymm1, ymm1, ymm2;                      {$ELSE}db $C5,$F5,$59,$CA;{$ENDIF} 
         {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm1;                      {$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 

         {$IFDEF AVXSUP}vmovupd ymm3, [rdx + rax - 32];               {$ELSE}db $C5,$FD,$10,$5C,$02,$E0;{$ENDIF} 
         {$IFDEF AVXSUP}vmovapd ymm4, [r8 + rax - 32];                {$ELSE}db $C4,$C1,$7D,$28,$64,$00,$E0;{$ENDIF} 

         {$IFDEF AVXSUP}vmulpd ymm3, ymm3, ymm4;                      {$ELSE}db $C5,$E5,$59,$DC;{$ENDIF} 
         {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm3;                      {$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 

      jmp @@innerLoopUnrolled;

      @@innerLoopStart:
      sub rax, 128;
      jz @@innerLoopEnd;

      @@innerLoop:
         add rax, 32;
         jg @@innerLoop2Start;

         {$IFDEF AVXSUP}vmovupd ymm1, [rdx + rax - 32];               {$ELSE}db $C5,$FD,$10,$4C,$02,$E0;{$ENDIF} 
         {$IFDEF AVXSUP}vmovapd ymm2, [r8 + rax - 32];                {$ELSE}db $C4,$C1,$7D,$28,$54,$00,$E0;{$ENDIF} 

         {$IFDEF AVXSUP}vmulpd ymm1, ymm1, ymm2;                      {$ELSE}db $C5,$F5,$59,$CA;{$ENDIF} 
         {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm1;                      {$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 

      jmp @@innerLoop;

      // last two elements
      @@innerLoop2Start:
      sub rax, 32;
      jz @@innerLoopEnd;

      {$IFDEF AVXSUP}vmovupd xmm3, [rdx];                             {$ELSE}db $C5,$F9,$10,$1A;{$ENDIF} 
      {$IFDEF AVXSUP}vmovapd xmm4, [r8];                              {$ELSE}db $C4,$C1,$79,$28,$20;{$ENDIF} 
      {$IFDEF AVXSUP}vmulpd xmm3, xmm3, xmm4;                         {$ELSE}db $C5,$E1,$59,$DC;{$ENDIF} 
      {$IFDEF AVXSUP}vaddpd xmm0, xmm0, xmm3;                         {$ELSE}db $C5,$F9,$58,$C3;{$ENDIF} 

      @@innerLoopEnd:

      {$IFDEF AVXSUP}vextractf128 xmm1, ymm0, 1;                      {$ELSE}db $C4,$E3,$7D,$19,$C1,$01;{$ENDIF} 
      {$IFDEF AVXSUP}vaddpd xmm0, xmm0, xmm1;                         {$ELSE}db $C5,$F9,$58,$C1;{$ENDIF} 
      {$IFDEF AVXSUP}vhaddpd xmm0, xmm0, xmm0;                        {$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} 
      {$IFDEF AVXSUP}vmovsd [rcx], xmm0;                              {$ELSE}db $C5,$FB,$11,$01;{$ENDIF} 

      // next element
      add rcx, 8;
      add rdx, 8;
   dec r9;
   jnz @@forxloop;

   // ########################################
   // #### epilog
   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;

{$ENDIF}

end.
