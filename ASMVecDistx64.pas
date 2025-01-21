unit ASMVecDistx64;

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

interface

{$I 'mrMath_CPU.inc'}

{$IFDEF x64}

procedure ASMMtxDistanceSqr(dist : PDouble; LineWidthDist : NativeInt; X, Y : PDouble; {$ifdef UNIX}unixxLen{$ELSE}xLen {$ENDIF}, {$ifdef UNIX}unixyLen{$ELSE}yLen{$ENDIF} : NativeInt); {$IFDEF FPC} assembler; {$ENDIF}
procedure ASMMtxDistanceAbs(dist : PDouble; LineWidthDist : NativeInt; X, Y : PDouble; {$ifdef UNIX}unixxLen{$ELSE}xLen {$ENDIF}, {$ifdef UNIX}unixyLen{$ELSE}yLen{$ENDIF} : NativeInt); {$IFDEF FPC} assembler; {$ENDIF}

{$ENDIF}

implementation

{$IFDEF x64}

procedure ASMMtxDistanceSqr(dist : PDouble; LineWidthDist : NativeInt; X, Y : PDouble; {$ifdef UNIX}unixxLen{$ELSE}
  xLen {$ENDIF}, {$ifdef UNIX}unixyLen{$ELSE}yLen{$ENDIF} : NativeInt); {$IFDEF FPC} assembler; {$ENDIF}
{$ifdef UNIX}
var xLen, yLen : NativeInt;
{$ENDIF}
// rcx: dest, rdx: destlinewidth; r8: x; r9 : y
asm
   {$IFDEF UNIX}
   // Linux uses a diffrent ABI -> copy over the registers so they meet with winABI
   // (note that the 5th and 6th parameter are are on the stack)
   // The parameters are passed in the following order:
   // RDI, RSI, RDX, RCX -> mov to RCX, RDX, R8, R9
   mov xLen, r8;
   mov yLen, r9;

   mov r8, rdx;
   mov r9, rcx;
   mov rcx, rdi;
   mov rdx, rsi;
   {$ENDIF}

   mov r11, xLen;
   mov rax, yLen;
   imul rax, -8;
   sub rcx, rax;

   sub r9, rax;

   @@foriloop:

      // load x[i] -> double
      movddup xmm0, [r8];
      mov r10, rax;
      add r10, 16;
      // todo unroll loop...
      jg @@lastelem;

      @@forjloop:
         // dist[i, j] = sqr( x[i] - y[j])
         movupd xmm1, [r9 + r10 - 16];
         subpd xmm1, xmm0;
         mulpd xmm1, xmm1;

         movupd [rcx + r10 - 16], xmm1;
      add r10, 16;
      jle @@forjloop;

      @@lastelem:
      sub r10, 8;
      jg @@linend;

      // last element
      movsd xmm1, [r9 - 8];
      subsd xmm1, xmm0;
      mulsd xmm1, xmm1;
      movsd [rcx - 8], xmm1;

      @@linend:

      add r8, 8;  // sizeof double
      add rcx, rdx; // next line
   dec r11;
   jnz @@foriloop;
end;

const cLocSignBits : Array[0..1] of int64 = ($7FFFFFFFFFFFFFFF, $7FFFFFFFFFFFFFFF);

procedure ASMMtxDistanceAbs(dist : PDouble; LineWidthDist : NativeInt; X, Y : PDouble;
  {$ifdef UNIX}unixxLen{$ELSE}xLen {$ENDIF}, {$ifdef UNIX}unixyLen{$ELSE}yLen{$ENDIF} : NativeInt); {$IFDEF FPC} assembler; {$ENDIF}

{$ifdef UNIX}
var xLen, yLen : NativeInt;
{$ENDIF}
// rcx: dest, rdx: destlinewidth; r8: x; r9 : y
asm
   {$IFDEF UNIX}
   // Linux uses a diffrent ABI -> copy over the registers so they meet with winABI
   // (note that the 5th and 6th parameter are are on the stack)
   // The parameters are passed in the following order:
   // RDI, RSI, RDX, RCX -> mov to RCX, RDX, R8, R9
   mov xLen, r8;
   mov yLen, r9;

   mov r8, rdx;
   mov r9, rcx;
   mov rcx, rdi;
   mov rdx, rsi;
   {$ENDIF}

   mov r11, xLen;
   mov rax, yLen;
   imul rax, -8;
   sub rcx, rax;

   movupd xmm2, [rip + cLocSignBits];

   sub r9, rax;

   @@foriloop:

      // load x[i] -> double
      movddup xmm0, [r8];
      mov r10, rax;
      add r10, 16;
      // todo unroll loop...
      jg @@lastelem;

      @@forjloop:
         // dist[i, j] = sqr( x[i] - y[j])
         movupd xmm1, [r9 + r10 - 16];
         subpd xmm1, xmm0;
         andpd xmm1, xmm2;

         movupd [rcx + r10 - 16], xmm1;
      add r10, 16;
      jle @@forjloop;

      @@lastelem:
      sub r10, 8;
      jg @@linend;

      // last element
      movsd xmm1, [r9 - 8];
      subsd xmm1, xmm0;
      andpd xmm1, xmm2;
      movsd [rcx - 8], xmm1;

      @@linend:

      add r8, 8;  // sizeof double
      add rcx, rdx; // next line
   dec r11;
   jnz @@foriloop;
end;

{$ENDIF}


end.
