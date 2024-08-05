// ###################################################################
// #### This file is part of the mathematics library project, and is
// #### offered under the licence agreement described on
// #### http://www.mrsoft.org/
// ####
// #### Copyright:(c) 2024, Michael R. . All rights reserved.
// ####
// #### Unless required by applicable law or agreed to in writing, software
// #### distributed under the License is distributed on an "AS IS" BASIS,
// #### WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// #### See the License for the specific language governing permissions and
// #### limitations under the License.
// ###################################################################

unit ASMVecDist;

interface

{$I 'mrMath_CPU.inc'}

{$IFNDEF x64}

procedure ASMMtxDistanceSqr(dist : PDouble; LineWidthDist : NativeInt; X, Y : PDouble; xLen, yLen : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure ASMMtxDistanceAbs(dist : PDouble; LineWidthDist : NativeInt; X, Y : PDouble; xLen, yLen : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

{$ENDIF}

implementation

{$IFNDEF x64}

procedure ASMMtxDistanceSqr(dist : PDouble; LineWidthDist : NativeInt; X, Y : PDouble; xLen, yLen : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
// eax = dest, edx = LineWidthDist, ecx = X
asm
   push ebx;
   push esi;
   push edi;

   mov esi, yLen;
   imul esi, -8;
   sub eax, esi;

   mov edi, Y;
   sub edi, esi;

   @@foriloop:

      // load x[i] -> double
      movddup xmm0, [ecx];
      mov ebx, esi;
      add ebx, 16;
      // todo unroll loop...
      jg @@lastelem;

      @@forjloop:
         // dist[i, j] = sqr( x[i] - y[j])
         movupd xmm1, [edi + ebx - 16];
         subpd xmm1, xmm0;
         mulpd xmm1, xmm1;

         movupd [eax + ebx - 16], xmm1;
      add ebx, 16;
      jle @@forjloop;

      @@lastelem:
      sub ebx, 8;
      jg @@linend;

      // last element
      movsd xmm1, [edi - 8];
      subsd xmm1, xmm0;
      mulsd xmm1, xmm1;
      movsd [eax - 8], xmm1;

      @@linend:

      add ecx, 8;  // sizeof double
      add eax, edx; // next line
   dec xLen;
   jnz @@foriloop;

   pop edi;
   pop esi;
   pop ebx;
end;

const cLocSignBits : Array[0..1] of int64 = ($7FFFFFFFFFFFFFFF, $7FFFFFFFFFFFFFFF);

procedure ASMMtxDistanceAbs(dist : PDouble; LineWidthDist : NativeInt; X, Y : PDouble; xLen, yLen : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
// eax = dest, edx = LineWidthDist, ecx = X
asm
   push ebx;
   push esi;
   push edi;

   mov esi, yLen;
   imul esi, -8;
   sub eax, esi;

   mov edi, Y;
   sub edi, esi;

   movupd xmm2, cLocSignBits;

   @@foriloop:

      // load x[i] -> double
      movddup xmm0, [ecx];
      mov ebx, esi;
      add ebx, 16;
      // todo unroll loop...
      jg @@lastelem;

      @@forjloop:
         // dist[i, j] = sqr( x[i] - y[j])
         movupd xmm1, [edi + ebx - 16];
         subpd xmm1, xmm0;
         andpd xmm1, xmm2;

         movupd [eax + ebx - 16], xmm1;
      add ebx, 16;
      jle @@forjloop;

      @@lastelem:
      sub ebx, 8;
      jg @@linend;

      // last element
      movsd xmm1, [edi - 8];
      subsd xmm1, xmm0;
      andpd xmm1, xmm2;
      movsd [eax - 8], xmm1;

      @@linend:

      add ecx, 8;  // sizeof double
      add eax, edx; // next line
   dec xLen;
   jnz @@foriloop;

   pop edi;
   pop esi;
   pop ebx;
end;

{$ENDIF}


end.
