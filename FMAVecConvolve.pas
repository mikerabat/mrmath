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

unit FMAVecConvolve;

interface

{$I 'mrMath_CPU.inc'}

{$IFNDEF x64}

// simple convolution: the input and output parameter are assumed to be vectors!
// it's also assumed that memory before A is accessible for at least bLen elements
// -> these elements are used for the convulution calculation
// -> needs an aligned B and blen mod 2 needs to be zero
procedure FMAVecConvolveRevB(dest : PDouble; A, B : PDouble; aLen, bLen : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

{$ENDIF}

implementation

{$IFNDEF x64}

procedure FMAVecConvolveRevB(dest : PDouble; A, B : PDouble; aLen, bLen : NativeInt);
// eax = dest, edx = A, ecx = B
asm
   push ebx;
   push esi;
   push edi;

   mov esi, bLen;
   imul esi, -8;

   add edx, 8;
   sub ecx, esi;

   // one element convolution
   @@forxloop:
      mov edi, esi;

           {$IFDEF AVXSUP}vxorpd ymm0, ymm0, ymm0;                    {$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} 

      // unrolled part
      @@innerLoopUnrolled:
         add edi, 128;
         jg @@innerLoopStart;

         {$IFDEF AVXSUP}vmovupd ymm1, [edx + edi - 128];              {$ELSE}db $C5,$FD,$10,$4C,$3A,$80;{$ENDIF} 
         {$IFDEF AVXSUP}vmovapd ymm2, [ecx + edi - 128];              {$ELSE}db $C5,$FD,$28,$54,$39,$80;{$ENDIF} 

              {$IFDEF AVXSUP}vfmadd231pd ymm0, ymm1, ymm2;            {$ELSE}db $C4,$E2,$F5,$B8,$C2;{$ENDIF} 

         {$IFDEF AVXSUP}vmovupd ymm3, [edx + edi - 96];               {$ELSE}db $C5,$FD,$10,$5C,$3A,$A0;{$ENDIF} 
         {$IFDEF AVXSUP}vmovapd ymm4, [ecx + edi - 96];               {$ELSE}db $C5,$FD,$28,$64,$39,$A0;{$ENDIF} 

              {$IFDEF AVXSUP}vfmadd231pd ymm0, ymm3, ymm4;            {$ELSE}db $C4,$E2,$E5,$B8,$C4;{$ENDIF} 


         {$IFDEF AVXSUP}vmovupd ymm1, [edx + edi - 64];               {$ELSE}db $C5,$FD,$10,$4C,$3A,$C0;{$ENDIF} 
         {$IFDEF AVXSUP}vmovapd ymm2, [ecx + edi - 64];               {$ELSE}db $C5,$FD,$28,$54,$39,$C0;{$ENDIF} 

              {$IFDEF AVXSUP}vfmadd231pd ymm0, ymm1, ymm2;            {$ELSE}db $C4,$E2,$F5,$B8,$C2;{$ENDIF} 

         {$IFDEF AVXSUP}vmovupd ymm3, [edx + edi - 32];               {$ELSE}db $C5,$FD,$10,$5C,$3A,$E0;{$ENDIF} 
         {$IFDEF AVXSUP}vmovapd ymm4, [ecx + edi - 32];               {$ELSE}db $C5,$FD,$28,$64,$39,$E0;{$ENDIF} 

              {$IFDEF AVXSUP}vfmadd231pd ymm0, ymm3, ymm4;            {$ELSE}db $C4,$E2,$E5,$B8,$C4;{$ENDIF} 

      jmp @@innerLoopUnrolled;

      @@innerLoopStart:
      sub edi, 128;
      jz @@innerLoopEnd;

      @@innerLoop:
         {$IFDEF AVXSUP}vmovupd ymm1, [edx + edi];                    {$ELSE}db $C5,$FD,$10,$0C,$3A;{$ENDIF} 
         {$IFDEF AVXSUP}vmovapd ymm2, [ecx + edi];                    {$ELSE}db $C5,$FD,$28,$14,$39;{$ENDIF} 

              {$IFDEF AVXSUP}vfmadd231pd ymm0, ymm1, ymm2;            {$ELSE}db $C4,$E2,$F5,$B8,$C2;{$ENDIF} 
         add edi, 32;
      jnz @@innerLoop;

      @@innerLoopEnd:

      {$IFDEF AVXSUP}vextractf128 xmm1, ymm0, 1;                      {$ELSE}db $C4,$E3,$7D,$19,$C1,$01;{$ENDIF} 
      {$IFDEF AVXSUP}vaddpd xmm0, xmm0, xmm1;                         {$ELSE}db $C5,$F9,$58,$C1;{$ENDIF} 
      {$IFDEF AVXSUP}vhaddpd xmm0, xmm0, xmm0;                        {$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} 
      {$IFDEF AVXSUP}vmovsd [eax], xmm0;                              {$ELSE}db $C5,$FB,$11,$00;{$ENDIF} 

      // next element
      add eax, 8;
      add edx, 8;
   dec aLen;
   jnz @@forxloop;

   // ########################################
   // #### epilog
        {$IFDEF AVXSUP}vzeroupper;                                    {$ELSE}db $C5,$F8,$77;{$ENDIF} 
   pop edi;
   pop esi;
   pop ebx;
end;

{$ENDIF}

end.
