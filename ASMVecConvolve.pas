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
procedure ASMVecConvolveRevB(dest : PDouble; A, B : PDouble; aLen, bLen : TASMNativeInt);

{$ENDIF}

implementation

{$IFNDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$S-} {$ENDIF}

procedure ASMVecConvolveRevB(dest : PDouble; A, B : PDouble; aLen, bLen : TASMNativeInt);
var iter : integer;
begin
     assert(blen and $01 = 0, 'Error vector len needs to be dividable by 2');
     assert( Cardinal(B) and $F = 0, 'Need an aligned B');

     iter := -bLen*sizeof(double);
     asm
        push ebx;
        push esi;

        mov edx, dest;
        mov ecx, A;
        add ecx, 8;
        mov ebx, B;
        sub ebx, iter;

        mov esi, aLen;

        // one element convolution
        @@forxloop:
           mov eax, iter;

           xorpd xmm0, xmm0;

           // unrolled part
           @@innerLoopUnrolled:
              add eax, 64;
              jg @@innerLoopStart;

              movupd xmm1, [ecx + eax - 64];
              movapd xmm2, [ebx + eax - 64];

              mulpd xmm1, xmm2;
              addpd xmm0, xmm1;

              movupd xmm3, [ecx + eax - 48];
              movapd xmm4, [ebx + eax - 48];

              mulpd xmm3, xmm4;
              addpd xmm0, xmm3;

              movupd xmm1, [ecx + eax - 32];
              movapd xmm2, [ebx + eax - 32];

              mulpd xmm1, xmm2;
              addpd xmm0, xmm1;

              movupd xmm3, [ecx + eax - 16];
              movapd xmm4, [ebx + eax - 16];

              mulpd xmm3, xmm4;
              addpd xmm0, xmm3;

           jmp @@innerLoopUnrolled;

           @@innerLoopStart:
           sub eax, 64;
           jz @@innerLoopEnd;

           @@innerLoop:
              movupd xmm1, [ecx + eax];
              movapd xmm2, [ebx + eax];

              mulpd xmm1, xmm2;
              addpd xmm0, xmm1;
              add eax, 16;
           jnz @@innerLoop;

           @@innerLoopEnd:

           haddpd xmm0, xmm0;
           movsd [edx], xmm0;

           // next element
           add edx, 8;
           add ecx, 8;
        dec esi;
        jnz @@forxloop;

        // ########################################
        // #### epilog
        pop esi;
        pop ebx;
     end;
end;

{$ENDIF}

end.
