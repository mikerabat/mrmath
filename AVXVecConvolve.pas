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

unit AVXVecConvolve;

interface

{$I 'mrMath_CPU.inc'}

{$IFNDEF x64}

uses MatrixConst;

// simple convolution: the input and output parameter are assumed to be vectors!
// it's also assumed that memory before A is accessible for at least bLen elements
// -> these elements are used for the convulution calculation
// -> needs an aligned B and blen mod 2 needs to be zero
procedure AVXVecConvolveRevB(dest : PDouble; A, B : PDouble; aLen, bLen : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

{$ENDIF}

implementation

{$IFNDEF x64}

procedure AVXVecConvolveRevB(dest : PDouble; A, B : PDouble; aLen, bLen : NativeInt);
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

      {$IFDEF FPC}vxorpd ymm0, ymm0, ymm0;{$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} 

      // unrolled part
      @@innerLoopUnrolled:
         add edi, 128;
         jg @@innerLoopStart;

         {$IFDEF FPC}vmovupd ymm1, [edx + edi - 128];{$ELSE}db $C5,$FD,$10,$4C,$3A,$80;{$ENDIF} 
         {$IFDEF FPC}vmovapd ymm2, [ecx + edi - 128];{$ELSE}db $C5,$FD,$28,$54,$39,$80;{$ENDIF} 

         {$IFDEF FPC}vmulpd ymm1, ymm1, ymm2;{$ELSE}db $C5,$F5,$59,$CA;{$ENDIF} 
         {$IFDEF FPC}vaddpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 

         {$IFDEF FPC}vmovupd ymm3, [edx + edi - 96];{$ELSE}db $C5,$FD,$10,$5C,$3A,$A0;{$ENDIF} 
         {$IFDEF FPC}vmovapd ymm4, [ecx + edi - 96];{$ELSE}db $C5,$FD,$28,$64,$39,$A0;{$ENDIF} 

         {$IFDEF FPC}vmulpd ymm3, ymm3, ymm4;{$ELSE}db $C5,$E5,$59,$DC;{$ENDIF} 
         {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 


         {$IFDEF FPC}vmovupd ymm1, [edx + edi - 64];{$ELSE}db $C5,$FD,$10,$4C,$3A,$C0;{$ENDIF} 
         {$IFDEF FPC}vmovapd ymm2, [ecx + edi - 64];{$ELSE}db $C5,$FD,$28,$54,$39,$C0;{$ENDIF} 

         {$IFDEF FPC}vmulpd ymm1, ymm1, ymm2;{$ELSE}db $C5,$F5,$59,$CA;{$ENDIF} 
         {$IFDEF FPC}vaddpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 

         {$IFDEF FPC}vmovupd ymm3, [edx + edi - 32];{$ELSE}db $C5,$FD,$10,$5C,$3A,$E0;{$ENDIF} 
         {$IFDEF FPC}vmovapd ymm4, [ecx + edi - 32];{$ELSE}db $C5,$FD,$28,$64,$39,$E0;{$ENDIF} 

         {$IFDEF FPC}vmulpd ymm3, ymm3, ymm4;{$ELSE}db $C5,$E5,$59,$DC;{$ENDIF} 
         {$IFDEF FPC}vaddpd ymm0, ymm0, ymm3;{$ELSE}db $C5,$FD,$58,$C3;{$ENDIF} 

      jmp @@innerLoopUnrolled;

      @@innerLoopStart:
      sub edi, 128;
      jz @@innerLoopEnd;

      @@innerLoop:
         {$IFDEF FPC}vmovupd ymm1, [edx + edi];{$ELSE}db $C5,$FD,$10,$0C,$3A;{$ENDIF} 
         {$IFDEF FPC}vmovapd ymm2, [ecx + edi];{$ELSE}db $C5,$FD,$28,$14,$39;{$ENDIF} 

         {$IFDEF FPC}vmulpd ymm1, ymm1, ymm2;{$ELSE}db $C5,$F5,$59,$CA;{$ENDIF} 
         {$IFDEF FPC}vaddpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 
         add edi, 32;
      jnz @@innerLoop;

      @@innerLoopEnd:

      {$IFDEF FPC}vextractf128 xmm1, ymm0, 1;{$ELSE}db $C4,$E3,$7D,$19,$C1,$01;{$ENDIF} 
      {$IFDEF FPC}vaddpd xmm0, xmm0, xmm1;{$ELSE}db $C5,$F9,$58,$C1;{$ENDIF} 
      {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} 
      {$IFDEF FPC}vmovsd [eax], xmm0;{$ELSE}db $C5,$FB,$11,$00;{$ENDIF} 

      // next element
      add eax, 8;
      add edx, 8;
   dec aLen;
   jnz @@forxloop;

   // ########################################
   // #### epilog
   pop edi;
   pop esi;
   pop ebx;
end;

{$ENDIF}

end.
