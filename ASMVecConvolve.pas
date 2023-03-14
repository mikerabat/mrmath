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

unit ASMVecConvolve;

interface

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}
{$IFNDEF x64}

uses MatrixConst;

// simple convolution: the input and output parameter are assumed to be vectors!
// it's also assumed that memory before A is accessible for at least bLen elements
// -> these elements are used for the convulution calculation
// -> needs an aligned B and blen mod 2 needs to be zero
procedure ASMVecConvolveRevB(dest : PDouble; A, B : PDouble; aLen, bLen : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

{$ENDIF}

implementation

{$IFNDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$S-} {$ENDIF}

procedure ASMVecConvolveRevB(dest : PDouble; A, B : PDouble; aLen, bLen : NativeInt);
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

      xorpd xmm0, xmm0;

      // unrolled part
      @@innerLoopUnrolled:
         add edi, 64;
         jg @@innerLoopStart;

         movupd xmm1, [edx + edi - 64];
         movapd xmm2, [ecx + edi - 64];

         mulpd xmm1, xmm2;
         addpd xmm0, xmm1;

         movupd xmm3, [edx + edi - 48];
         movapd xmm4, [ecx + edi - 48];

         mulpd xmm3, xmm4;
         addpd xmm0, xmm3;

         movupd xmm1, [edx + edi - 32];
         movapd xmm2, [ecx + edi - 32];

         mulpd xmm1, xmm2;
         addpd xmm0, xmm1;

         movupd xmm3, [edx + edi - 16];
         movapd xmm4, [ecx + edi - 16];

         mulpd xmm3, xmm4;
         addpd xmm0, xmm3;

      jmp @@innerLoopUnrolled;

      @@innerLoopStart:
      sub edi, 64;
      jz @@innerLoopEnd;

      @@innerLoop:
         movupd xmm1, [edx + edi];
         movapd xmm2, [ecx + edi];

         mulpd xmm1, xmm2;
         addpd xmm0, xmm1;
         add edi, 16;
      jnz @@innerLoop;

      @@innerLoopEnd:

      haddpd xmm0, xmm0;
      movsd [eax], xmm0;

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
