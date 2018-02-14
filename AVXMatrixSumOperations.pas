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


unit AVXMatrixSumOperations;

interface

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}
{$IFNDEF x64}

uses MatrixConst;

procedure AVXMatrixSumRowAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
procedure AVXMatrixSumRowUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);

procedure AVXMatrixSumColumnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
procedure AVXMatrixSumColumnUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);

{$ENDIF}

implementation

{$IFNDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$ENDIF}

procedure AVXMatrixSumRowAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
{$IFDEF FPC}
begin
{$ENDIF}
asm
   // prolog
   push ebx;
   push edi;
   push esi;

   // iters := -width*sizeof(double)
   mov edi, width;
   imul edi, -8;

   mov ebx, srcLineWidth;
   mov edx, src;
   mov ecx, dest;

   // helper registers for the mt1, mt2 and dest pointers
   sub edx, edi;

   // for y := 0 to height - 1:
   mov esi, Height;
   @@addforyloop:
       vxorpd ymm0, ymm0, ymm0;
       vxorpd ymm1, ymm1, ymm1;

       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov eax, edi;
       @addforxloop:
           add eax, 128;
           jg @loopEnd;
           // prefetch data...
           // prefetch [edx + eax];

           // addition:
           vaddpd ymm0, ymm0, [edx + eax - 128];
           vaddpd ymm1, ymm1, [edx + eax - 96];
           vaddpd ymm0, ymm0, [edx + eax - 64];
           vaddpd ymm1, ymm1, [edx + eax - 32];
       jmp @addforxloop

       @loopEnd:

       vaddpd ymm0, ymm0, ymm1;
       vextractf128 xmm2, ymm0, 1;
       vhaddpd xmm0, xmm0, xmm2;

       sub eax, 128;

       jz @buildRes;

       @addforxloop2:
           add eax, 16;
           jg @@addforxloop2end;

           vaddpd xmm0, xmm0, [edx + eax - 16];
       jmp @addforxloop2;

       @@addforxloop2end:

       sub eax, 16;
       jz @buildRes;

       vaddsd xmm0, xmm0, [edx + eax];

       @buildRes:

       // build result
       vhaddpd xmm0, xmm0, xmm0;

       // write result
       vmovsd [ecx], xmm0;

       // next line:
       add edx, ebx;
       add ecx, destLineWidth;

   // loop y end
   dec esi;
   jnz @@addforyloop;

   // epilog
   vzeroupper;
   pop esi;
   pop edi;
   pop ebx;
{$IFDEF FPC}
end;
{$ENDIF}
end;

procedure AVXMatrixSumRowUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
{$IFDEF FPC}
begin
{$ENDIF}
asm
   // prolog
   push ebx;
   push edi;
   push esi;

   // iters := -width*sizeof(double)
   mov edi, width;
   imul edi, -8;

   mov edx, src;
   mov ecx, dest;

   // helper registers for the mt1, mt2 and dest pointers
   sub edx, edi;

   mov ebx, srcLineWidth;
   // for y := 0 to height - 1:
   mov esi, Height;
   @@addforyloop:
       vxorpd ymm0, ymm0, ymm0;
       vxorpd ymm1, ymm1, ymm1;

       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov eax, edi;
       @addforxloop:
           add eax, 128;
           jg @loopEnd;
           // prefetch data...
           // prefetch [edx + eax];

           // addition:
           vmovupd ymm2, [edx + eax - 128];
           vaddpd ymm0, ymm0, ymm2;
           vmovupd ymm2, [edx + eax - 96];
           vaddpd ymm1, ymm1, ymm2;
           vmovupd ymm2, [edx + eax - 64];
           vaddpd ymm0, ymm0, ymm2;
           vmovupd ymm2, [edx + eax - 32];
           vaddpd ymm1, ymm1, ymm2;
       jmp @addforxloop

       @loopEnd:

       vaddpd ymm0, ymm0, ymm1;
       vextractf128 xmm2, ymm0, 1;
       vhaddpd xmm0, xmm0, xmm2;

       sub eax, 128;

       jz @buildRes;

       @addforxloop2:
           add eax, 16;
           jg @@addforxloop2end;

           vmovupd xmm2, [edx + eax - 16];
           vaddpd xmm0, xmm0, xmm2;
       jmp @addforxloop2;

       @@addforxloop2end:

       sub eax, 16;
       jz @buildRes;

       vaddsd xmm0, xmm0, [edx + eax];

       @buildRes:

       // build result
       vhaddpd xmm0, xmm0, xmm0;

       // write result
       vmovsd [ecx], xmm0;

       // next line:
       add edx, ebx;
       add ecx, destLineWidth;

   // loop y end
   dec esi;
   jnz @@addforyloop;

   // epilog
   vzeroupper;
   pop esi;
   pop edi;
   pop ebx;
{$IFDEF FPC}
end;
{$ENDIF}
end;

procedure AVXMatrixSumColumnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
{$IFDEF FPC}
begin
{$ENDIF}
asm
   // prolog
   push ebx;
   push edi;
   push esi;

   // init
   mov ecx, dest;
   mov edx, src;

   xor edi, edi;
   sub edi, height;
   imul edi, srcLineWidth;

   mov ebx, srcLineWidth;

   // helper registers for the mt1, mt2 and dest pointers
   sub edx, edi;

   // for x := 0 to width - 1:
   mov esi, Width;
   sub esi, 4;
   jl @@addforxloop4end;

   // 4 columns at once
   @@addforxloop4:
       vxorpd ymm1, ymm1, ymm1;

       // for y := 0 to height - 1;
       // prepare for reverse loop
       mov eax, edi;
       @addforyloop4:
           vaddpd ymm1, ymm1, [edx + eax];
       add eax, ebx;
       jnz @addforyloop4;

       // build result
       vmovapd [ecx], ymm1;

       // next columns:
       add ecx, 32;
       add edx, 32;

   // loop x end
   sub esi, 4;
   jge @@addforxloop4;

   @@addforxloop4end:

   add esi, 4;
   jz @@endProc;

   sub esi, 2;
   jl @@lastcolumn;

   vxorpd xmm1, xmm1, xmm1;

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov eax, edi;
   @addforyloop2:
       vaddpd xmm1, xmm1, [edx + eax];
   add eax, ebx;
   jnz @addforyloop2;

   // build result
   vmovapd [ecx], xmm1;

   // next columns:
   add ecx, 16;
   add edx, 16;

   dec esi;
   jnz @@endProc;

   @@lastcolumn:

   vxorpd xmm1, xmm1, xmm1;

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov eax, edi;
   @addforyloop:
       vaddsd xmm1, xmm1, [edx + eax];
   add eax, ebx;
   jnz @addforyloop;

   // build result
   vmovsd [ecx], xmm1;

   @@endProc:
   vzeroupper;
   pop esi;
   pop edi;
   pop ebx;
{$IFDEF FPC}
end;
{$ENDIF}
end;

procedure AVXMatrixSumColumnUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
{$IFDEF FPC}
begin
{$ENDIF}
asm
   // prolog
   push ebx;
   push edi;
   push esi;

   // prepare
   mov ecx, dest;
   mov edx, src;

   xor edi, edi;
   sub edi, height;
   imul edi, srcLineWidth;

   // helper registers for the mt1, mt2 and dest pointers
   sub edx, edi;

   mov ebx, srcLineWidth;

   // for x := 0 to width - 1:
   mov esi, Width;
   sub esi, 4;
   jl @@addforxloop4end;

   // 4 columns at once
   @@addforxloop4:
       vxorpd ymm1, ymm1, ymm1;

       // for y := 0 to height - 1;
       // prepare for reverse loop
       mov eax, edi;
       @addforyloop4:
           vmovupd ymm0, [edx + eax];
           vaddpd ymm1, ymm1, ymm0;
       add eax, ebx;
       jnz @addforyloop4;

       // build result
       vmovupd [ecx], ymm1;

       // next columns:
       add ecx, 32;
       add edx, 32;

   // loop x end
   sub esi, 4;
   jge @@addforxloop4;

   @@addforxloop4end:

   add esi, 4;
   jz @@endProc;

   sub esi, 2;
   jl @@lastcolumn;

   vxorpd xmm1, xmm1, xmm1;

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov eax, edi;
   @addforyloop2:
       vmovupd xmm0, [edx + eax];
       vaddpd xmm1, xmm1, [edx + eax];
   add eax, ebx;
   jnz @addforyloop2;

   // build result
   vmovupd [ecx], xmm1;

   // next columns:
   add ecx, 16;
   add edx, 16;

   dec esi;
   jnz @@endProc;

   @@lastcolumn:

   vxorpd xmm1, xmm1, xmm1;

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov eax, edi;
   @addforyloop:
       vaddsd xmm1, xmm1, [edx + eax];
   add eax, ebx;
   jnz @addforyloop;

   // build result
   vmovsd [ecx], xmm1;

   @@endProc:
   vzeroupper;

   pop esi;
   pop edi;
   pop ebx;
{$IFDEF FPC}
end;
{$ENDIF}
end;

{$ENDIF}

end.
