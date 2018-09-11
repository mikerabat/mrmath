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
procedure FMAVecConvolveRevB(dest : PDouble; A, B : PDouble; aLen, bLen : TASMNativeInt);

{$ENDIF}

implementation

{$IFNDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$S-} {$ENDIF}

procedure FMAVecConvolveRevB(dest : PDouble; A, B : PDouble; aLen, bLen : TASMNativeInt);
var iter : integer;
begin
     assert(blen and $03 = 0, 'Error vector len needs to be dividable by 2');
     assert( Cardinal(B) and $1F = 0, 'Need an aligned B');

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

           {$IFDEF FPC}vxorpd ymm0, ymm0, ymm0;{$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} 

           // unrolled part
           @@innerLoopUnrolled:
              add eax, 128;
              jg @@innerLoopStart;

              {$IFDEF FPC}vmovupd ymm1, [ecx + eax - 128];{$ELSE}db $C5,$FD,$10,$4C,$01,$80;{$ENDIF} 
              {$IFDEF FPC}vmovapd ymm2, [ebx + eax - 128];{$ELSE}db $C5,$FD,$28,$54,$03,$80;{$ENDIF} 

              {$IFDEF FPC}vfmadd231pd ymm0, ymm1, ymm2;{$ELSE}db $C4,$E2,$F5,$B8,$C2;{$ENDIF} 

              {$IFDEF FPC}vmovupd ymm3, [ecx + eax - 96];{$ELSE}db $C5,$FD,$10,$5C,$01,$A0;{$ENDIF} 
              {$IFDEF FPC}vmovapd ymm4, [ebx + eax - 96];{$ELSE}db $C5,$FD,$28,$64,$03,$A0;{$ENDIF} 

              {$IFDEF FPC}vfmadd231pd ymm0, ymm3, ymm4;{$ELSE}db $C4,$E2,$E5,$B8,$C4;{$ENDIF} 

              {$IFDEF FPC}vmovupd ymm1, [ecx + eax - 64];{$ELSE}db $C5,$FD,$10,$4C,$01,$C0;{$ENDIF} 
              {$IFDEF FPC}vmovapd ymm2, [ebx + eax - 64];{$ELSE}db $C5,$FD,$28,$54,$03,$C0;{$ENDIF} 

              {$IFDEF FPC}vfmadd231pd ymm0, ymm1, ymm2;{$ELSE}db $C4,$E2,$F5,$B8,$C2;{$ENDIF} 

              {$IFDEF FPC}vmovupd ymm3, [ecx + eax - 32];{$ELSE}db $C5,$FD,$10,$5C,$01,$E0;{$ENDIF} 
              {$IFDEF FPC}vmovapd ymm4, [ebx + eax - 32];{$ELSE}db $C5,$FD,$28,$64,$03,$E0;{$ENDIF} 

              {$IFDEF FPC}vfmadd231pd ymm0, ymm3, ymm4;{$ELSE}db $C4,$E2,$E5,$B8,$C4;{$ENDIF} 

           jmp @@innerLoopUnrolled;

           @@innerLoopStart:
           sub eax, 128;
           jz @@innerLoopEnd;

           @@innerLoop:
              {$IFDEF FPC}vmovupd ymm1, [ecx + eax];{$ELSE}db $C5,$FD,$10,$0C,$01;{$ENDIF} 
              {$IFDEF FPC}vmovapd ymm2, [ebx + eax];{$ELSE}db $C5,$FD,$28,$14,$03;{$ENDIF} 

              {$IFDEF FPC}vfmadd231pd ymm0, ymm1, ymm2;{$ELSE}db $C4,$E2,$F5,$B8,$C2;{$ENDIF} 
              add eax, 32;
           jnz @@innerLoop;

           @@innerLoopEnd:

           {$IFDEF FPC}vextractf128 xmm1, ymm0, 1;{$ELSE}db $C4,$E3,$7D,$19,$C1,$01;{$ENDIF} 
           {$IFDEF FPC}vaddpd xmm0, xmm0, xmm1;{$ELSE}db $C5,$F9,$58,$C1;{$ENDIF} 
           {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} 
           {$IFDEF FPC}vmovsd [edx], xmm0;{$ELSE}db $C5,$FB,$11,$02;{$ENDIF} 

           // next element
           add edx, 8;
           add ecx, 8;
        dec esi;
        jnz @@forxloop;

        // ########################################
        // #### epilog
        {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 
        pop esi;
        pop ebx;
     end;
end;

{$ENDIF}

end.
