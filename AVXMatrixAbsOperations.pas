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

unit AVXMatrixAbsOperations;

// #####################################################
// #### Abs opertaion applied to every element in a matrix
// #####################################################

interface

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}
{$IFNDEF x64}

uses MatrixConst;

procedure AVXMatrixAbsAligned(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt);
procedure AVXMatrixAbsUnAligned(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt);

{$ENDIF}

implementation

{$IFNDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$ENDIF}

procedure AVXMatrixAbsAligned(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt);
var iters : TASMNativeInt;
{$IFDEF FPC}
begin
{$ENDIF}
asm
   mov eax, width;
   shl eax, 3;
   imul eax, -1;
   mov iters, eax;

   // helper registers for the dest pointer
   mov ecx, dest;
   sub ecx, eax;

   lea edx, cSignBits4;
   vmovupd ymm0, [edx];

   // for y := 0 to height - 1:
   mov edx, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov eax, iters;
       @addforxloop:
           add eax, 128;
           jg @loopEnd;

           // prefetch data...
           //prefetchw [rcx + rax];

           // Abs:
           vmovapd ymm1, [ecx + eax - 128];
           vAndpd ymm1, ymm1, ymm0;
           vmovntdq [ecx + eax - 128], ymm1;

           vmovapd ymm2, [ecx + eax - 96];
           vandpd ymm2, ymm2, ymm0;
           vmovntdq [ecx + eax - 96], ymm2;

           vmovapd ymm3, [ecx + eax - 64];
           vandpd ymm3, ymm3, ymm0;
           vmovntdq [ecx + eax - 64], ymm3;

           vmovapd ymm4, [ecx + eax - 32];
           vandpd ymm4, ymm4, ymm0;
           vmovntdq [ecx + eax - 32], ymm4;
       jmp @addforxloop

       @loopEnd:

       sub eax, 128;

       jz @nextLine;

       @addforxloop2:
           add eax, 16;
           jg @loopEnd2;

           vmovapd xmm1, [ecx + eax - 16];
           vandpd xmm1, xmm1, xmm0;
           vmovntdq [ecx + eax - 16], xmm1;
       jmp @addforxloop2;

       @loopEnd2:

       sub eax, 16;
       jz @nextLine;

       vmovsd xmm1, [ecx + eax];
       vandpd xmm1, xmm7, xmm0;
       vmovsd [ecx + eax], xmm1;

       @nextLine:

       // next line:
       add ecx, linewidth;

   // loop y end
   dec edx;
   jnz @@addforyloop;

   vzeroupper;
end;
{$IFDEF FPC}
end;
{$ENDIF}

procedure AVXMatrixAbsUnAligned(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt);
var iters : TASMNativeInt;
{$IFDEF FPC}
begin
{$ENDIF}
asm
   mov eax, width;
   shl eax, 3;
   imul eax, -1;
   mov iters, eax;

   // helper registers for the dest pointer
   mov ecx, Dest;
   sub ecx, eax;

   lea edx, cSignBits4;
   vmovupd ymm0, [edx];

   // for y := 0 to height - 1:
   mov edx, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov eax, iters;
       @addforxloop:
           add eax, 128;
           jg @loopEnd;

           // prefetch data...
           //prefetchw [ecx + rax];

           // Abs:
           vmovupd ymm1, [ecx + eax - 128];
           vAndpd ymm1, ymm1, ymm0;
           vmovupd [ecx + eax - 128], ymm1;

           vmovupd ymm2, [ecx + eax - 96];
           vandpd ymm2, ymm2, ymm0;
           vmovupd [ecx + eax - 96], ymm2;

           vmovupd ymm3, [ecx + eax - 64];
           vandpd ymm3, ymm3, ymm0;
           vmovupd [ecx + eax - 64], ymm3;

           vmovupd ymm4, [ecx + eax - 32];
           vandpd ymm4, ymm4, ymm0;
           vmovupd [ecx + eax - 32], ymm4;
       jmp @addforxloop

       @loopEnd:

       sub eax, 128;

       jz @nextLine;

       @addforxloop2:
           add eax, 16;
           jg @loopEnd2;

           vmovupd xmm1, [ecx + eax - 16];
           vandpd xmm1, xmm1, xmm0;
           vmovupd [ecx + eax - 16], xmm1;
       jmp @addforxloop2;

       @loopEnd2:

       sub eax, 16;
       jz @nextLine;

       vmovsd xmm1, [ecx + eax];
       vandpd xmm1, xmm1, xmm0;
       vmovsd [ecx + eax], xmm1;

       @nextLine:

       // next line:
       add ecx, LineWidth;

   // loop y end
   dec edx;
   jnz @@addforyloop;

   vzeroupper;
end;
{$IFDEF FPC}
end;
{$ENDIF}

{$ENDIF}

end.
