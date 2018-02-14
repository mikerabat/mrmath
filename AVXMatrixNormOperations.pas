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


unit AVXMatrixNormOperations;

interface

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}
{$IFNDEF x64}

uses MatrixConst;

function AVXMatrixElementwiseNorm2Aligned(dest : PDouble; const LineWidth : TASMNativeInt; Width, height : TASMNativeInt) : double;
function AVXMatrixElementwiseNorm2UnAligned(dest : PDouble; const LineWidth : TASMNativeInt; Width, height : TASMNativeInt) : double;


procedure AVXMatrixNormalizeRowAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
procedure AVXMatrixNormalizeRowUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);

procedure AVXMatrixNormalizeColumnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
procedure AVXMatrixNormalizeColumnUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);

{$ENDIF}

implementation

{$IFNDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$ENDIF}

function AVXMatrixElementwiseNorm2Aligned(dest : PDouble; const LineWidth : TASMNativeInt; Width, height : TASMNativeInt) : double;
{$IFDEF FPC}
begin
{$ENDIF}
asm
   // prolog - simulate stack
   push ebx;
   push edi;
   push esi;

   mov esi, LineWidth;
   //iters := -width*sizeof(double);
   mov edi, width;
   imul edi, -8;

   // helper registers for the mt1, mt2 and dest pointers
   mov ecx, dest;
   sub ecx, edi;

   vxorpd xmm0, xmm0, xmm0;
   vxorpd ymm1, ymm1, ymm1;

   mov ebx, height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov eax, edi;
       @addforxloop:
           add eax, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [ecx + eax];

           // mul add:
           vmovapd ymm3, [ecx + eax - 128];
           vmulpd ymm3, ymm3, ymm3;
           vaddpd ymm1, ymm1, ymm3;

           vmovapd ymm2, [ecx + eax - 96];
           vmulpd ymm2, ymm2, ymm2;
           vaddpd ymm1, ymm1, ymm2;

           vmovapd ymm3, [ecx + eax - 64];
           vmulpd ymm3, ymm3, ymm3;
           vaddpd ymm1, ymm1, ymm3;

           vmovapd ymm2, [ecx + eax - 32];
           vmulpd ymm2, ymm2, ymm2;
           vaddpd ymm1, ymm1, ymm2;
       jmp @addforxloop

       @loopEnd:

       sub eax, 128;

       jz @nextLine;

       @addforxloop2:
           add eax, 16;
           jg @@addforxloop2end;

           vmovapd xmm3, [ecx + eax - 16];
           vmulpd xmm3, xmm3, xmm3;
           vaddpd xmm0, xmm0, xmm3;
       jmp @addforxloop2;

       @@addforxloop2end:

       sub eax, 16;
       jz @nextLine;

       vmovsd xmm3, [ecx + eax];
       vmulsd xmm3, xmm3, xmm3;
       vaddsd xmm0, xmm0, xmm3;

       @nextLine:

       // next line:
       add ecx, esi;

   // loop y end
   dec ebx;
   jnz @@addforyloop;

   // build result
   vextractf128 xmm2, ymm1, 1;
   vhaddpd xmm1, xmm1, xmm2;
   vaddpd xmm0, xmm0, xmm1;
   vhaddpd xmm0, xmm0, xmm0;

   vmovsd Result, xmm0;

   // epilog claenup stack
   vzeroupper;

   pop esi;
   pop edi;
   pop ebx;
{$IFDEF FPC}
end;
{$ENDIF}
end;

function AVXMatrixElementwiseNorm2UnAligned(dest : PDouble; const LineWidth : TASMNativeInt; Width, height : TASMNativeInt) : double;
{$IFDEF FPC}
begin
{$ENDIF}
asm
   // prolog - simulate stack
   push ebx;
   push edi;
   push esi;

   mov esi, lineWidth;
   //iters := -width*sizeof(double);
   mov edi, width;
   imul edi, -8;

   // helper registers for the mt1, mt2 and dest pointers
   mov ecx, dest;
   sub ecx, edi;

   vxorpd xmm0, xmm0, xmm0;
   vxorpd ymm1, ymm1, ymm1;

   mov ebx, height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov eax, edi;
       @addforxloop:
           add eax, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [ecx + eax];

           // mul add:
           vmovupd ymm3, [ecx + eax - 128];
           vmulpd ymm3, ymm3, ymm3;
           vaddpd ymm1, ymm1, ymm3;

           vmovupd ymm2, [ecx + eax - 96];
           vmulpd ymm2, ymm2, ymm2;
           vaddpd ymm1, ymm1, ymm2;

           vmovupd ymm3, [ecx + eax - 64];
           vmulpd ymm3, ymm3, ymm3;
           vaddpd ymm1, ymm1, ymm3;

           vmovupd ymm2, [ecx + eax - 32];
           vmulpd ymm2, ymm2, ymm2;
           vaddpd ymm1, ymm1, ymm2;
       jmp @addforxloop

       @loopEnd:

       sub eax, 128;

       jz @nextLine;

       @addforxloop2:
           add eax, 16;
           jg @@addforxloop2end;

           vmovupd xmm3, [ecx + eax - 16];
           vmulpd xmm3, xmm3, xmm3;
           vaddpd xmm0, xmm0, xmm3;
       jmp @addforxloop2;

       @@addforxloop2end:

       sub eax, 16;
       jz @nextLine;

       vmovsd xmm3, [ecx + eax];
       vmulsd xmm3, xmm3, xmm3;
       vaddsd xmm0, xmm0, xmm3;

       @nextLine:

       // next line:
       add ecx, esi;

   // loop y end
   dec ebx;
   jnz @@addforyloop;

   // build result
   vextractf128 xmm2, ymm1, 1;
   vhaddpd xmm1, xmm1, xmm2;
   vaddpd xmm0, xmm0, xmm1;
   vhaddpd xmm0, xmm0, xmm0;

   vmovsd Result, xmm0;

   // epilog claenup stack
   vzeroupper;

   pop esi;
   pop edi;
   pop ebx;
{$IFDEF FPC}
end;
{$ENDIF}
end;

procedure AVXMatrixNormalizeRowAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
var tmp : double;
{$IFDEF FPC}
begin
{$ENDIF}
asm
   // prolog - simulate stack
   push ebx;
   push edi;

   //iters := -width*sizeof(double);
   mov edi, width;
   imul edi, -8;

   // helper registers for the mt1, mt2 and dest pointers
   mov ecx, dest;
   mov edx, src;
   sub ecx, edi;
   sub edx, edi;

   mov ebx, height;
   @@addforyloop:
       vxorpd xmm0, xmm0, xmm0;
       vxorpd ymm1, ymm1, ymm1;

       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov eax, edi;
       @addforxloop:
           add eax, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [ecx + eax];

           // mul add:
           vmovapd ymm3, [edx + eax - 128];
           vmulpd ymm3, ymm3, ymm3;
           vaddpd ymm1, ymm1, ymm3;

           vmovapd ymm2, [edx + eax - 96];
           vmulpd ymm2, ymm2, ymm2;
           vaddpd ymm1, ymm1, ymm2;

           vmovapd ymm3, [edx + eax - 64];
           vmulpd ymm3, ymm3, ymm3;
           vaddpd ymm1, ymm1, ymm3;

           vmovapd ymm2, [edx + eax - 32];
           vmulpd ymm2, ymm2, ymm2;
           vaddpd ymm1, ymm1, ymm2;
       jmp @addforxloop

       @loopEnd:

       sub eax, 128;

       jz @nextLine;

       @addforxloop2:
           add eax, 16;
           jg @@addforxloop2end;

           vmovapd xmm3, [edx + eax - 16];
           vmulpd xmm3, xmm3, xmm3;
           vaddpd xmm0, xmm0, xmm3;
       jmp @addforxloop2;

       @@addforxloop2end:

       sub eax, 16;
       jz @nextLine;

       vmovsd xmm3, [edx + eax];
       vmulsd xmm3, xmm3, xmm3;
       vaddsd xmm0, xmm0, xmm3;

       @nextLine:

       // build result
       vextractf128 xmm2, ymm1, 1;
       vhaddpd xmm1, xmm1, xmm2;
       vaddpd xmm0, xmm0, xmm1;
       vhaddpd xmm0, xmm0, xmm0;

       // build result
       vsqrtsd xmm0, xmm0, xmm0;

       //1/sqrt(norm)
       lea eax, cOne;
       vmovsd xmm1, [eax];
       vdivsd xmm1, xmm1, xmm0;

       lea eax, tmp;
       vmovsd [eax], xmm1;
       vbroadcastsd ymm2, [eax];   // need avx2

       //  normalize
       mov eax, edi;
       @addforxloop3:
           add eax, 128;
           jg @loopEnd2;

           // prefetch data...
           // prefetch [edx + eax];
           // prefetchw [ecx + eax];

           // mult:
           vmovapd ymm0, [edx + eax - 128];
           vmulpd ymm0, ymm0, ymm2;
           vmovapd [ecx + eax - 128], ymm0;

           vmovapd ymm1, [edx + eax - 96];
           vmulpd ymm1, ymm1, ymm2;
           vmovapd [ecx + eax - 96], ymm1;

           vmovapd ymm0, [edx + eax - 64];
           vmulpd ymm0, ymm0, ymm2;
           vmovapd [ecx + eax - 64], ymm0;

           vmovapd ymm1, [edx + eax - 32];
           vmulpd ymm1, ymm1, ymm2;
           vmovapd [ecx + eax - 32], ymm1;

       jmp @addforxloop3

       @loopEnd2:

       sub eax, 128;

       jz @nextLine2;

       @addforxloop4:
           add eax, 16;
           jg @addforxloop4end;

           vmovapd xmm0, [edx + eax - 16];
           vmulpd xmm0, xmm0, xmm2;
           vmovapd [ecx + eax - 16], xmm0;
       jmp @addforxloop4;

       @addforxloop4end:
       sub eax, 16;
       jz @nextLine2;

       vmovsd xmm0, [edx + eax];
       vmulsd xmm0, xmm0, xmm2;
       vmovsd [ecx + eax], xmm0;

       @nextLine2:

       // next line:
       add ecx, destlineWidth;
       add edx, srcLineWidth;

   // loop y end
   dec ebx;
   jnz @@addforyloop;

   // epilog claenup stack
   vzeroupper;

   pop edi;
   pop ebx;
{$IFDEF FPC}
end;
{$ENDIF}
end;

procedure AVXMatrixNormalizeRowUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
var tmp : double;
{$IFDEF FPC}
begin
{$ENDIF}
asm
   // prolog - simulate stack
   push ebx;
   push edi;

   //iters := -width*sizeof(double);
   mov edi, width;
   imul edi, -8;

   // helper registers for the mt1, mt2 and dest pointers
   mov ecx, dest;
   mov edx, src;
   sub ecx, edi;
   sub edx, edi;

   mov ebx, height;
   @@addforyloop:
       vxorpd xmm0, xmm0, xmm0;
       vxorpd ymm1, ymm1, ymm1;

       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov eax, edi;
       @addforxloop:
           add eax, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [ecx + eax];

           // mul add:
           vmovupd ymm3, [edx + eax - 128];
           vmulpd ymm3, ymm3, ymm3;
           vaddpd ymm1, ymm1, ymm3;

           vmovupd ymm2, [edx + eax - 96];
           vmulpd ymm2, ymm2, ymm2;
           vaddpd ymm1, ymm1, ymm2;

           vmovupd ymm3, [edx + eax - 64];
           vmulpd ymm3, ymm3, ymm3;
           vaddpd ymm1, ymm1, ymm3;

           vmovupd ymm2, [edx + eax - 32];
           vmulpd ymm2, ymm2, ymm2;
           vaddpd ymm1, ymm1, ymm2;
       jmp @addforxloop

       @loopEnd:

       sub eax, 128;

       jz @nextLine;

       @addforxloop2:
           add eax, 16;
           jg @@addforxloop2end;

           vmovupd xmm3, [edx + eax - 16];
           vmulpd xmm3, xmm3, xmm3;
           vaddpd xmm0, xmm0, xmm3;
       jmp @addforxloop2;

       @@addforxloop2end:

       sub eax, 16;
       jz @nextLine;

       vmovsd xmm3, [edx + eax];
       vmulsd xmm3, xmm3, xmm3;
       vaddsd xmm0, xmm0, xmm3;

       @nextLine:

       // build result
       vextractf128 xmm2, ymm1, 1;
       vhaddpd xmm1, xmm1, xmm2;
       vaddpd xmm0, xmm0, xmm1;
       vhaddpd xmm0, xmm0, xmm0;

       // build result
       vsqrtsd xmm0, xmm0, xmm0;

       //1/sqrt(norm)
       lea eax, cOne;
       vmovsd xmm1, [eax];
       vdivsd xmm1, xmm1, xmm0;

       lea eax, tmp;
       vmovsd [eax], xmm1;
       vbroadcastsd ymm2, [eax];   // need avx2

       //  normalize
       mov eax, edi;
       @addforxloop3:
           add eax, 128;
           jg @loopEnd2;

           // prefetch data...
           // prefetch [edx + eax];
           // prefetchw [ecx + eax];

           // mult:
           vmovupd ymm0, [edx + eax - 128];
           vmulpd ymm0, ymm0, ymm2;
           vmovupd [ecx + eax - 128], ymm0;

           vmovupd ymm1, [edx + eax - 96];
           vmulpd ymm1, ymm1, ymm2;
           vmovupd [ecx + eax - 96], ymm1;

           vmovupd ymm0, [edx + eax - 64];
           vmulpd ymm0, ymm0, ymm2;
           vmovupd [ecx + eax - 64], ymm0;

           vmovupd ymm1, [edx + eax - 32];
           vmulpd ymm1, ymm1, ymm2;
           vmovupd [ecx + eax - 32], ymm1;

       jmp @addforxloop3

       @loopEnd2:

       sub eax, 128;

       jz @nextLine2;

       @addforxloop4:
           add eax, 16;
           jg @addforxloop4end;

           vmovupd xmm0, [edx + eax - 16];
           vmulpd xmm0, xmm0, xmm2;
           vmovupd [ecx + eax - 16], xmm0;
       jmp @addforxloop4;

       @addforxloop4end:
       sub eax, 16;
       jz @nextLine2;

       vmovsd xmm0, [edx + eax];
       vmulsd xmm0, xmm0, xmm2;
       vmovsd [ecx + eax], xmm0;

       @nextLine2:

       // next line:
       add ecx, destlineWidth;
       add edx, srcLineWidth;

   // loop y end
   dec ebx;
   jnz @@addforyloop;

   // epilog claenup stack
   vzeroupper;

   pop edi;
   pop ebx;
{$IFDEF FPC}
end;
{$ENDIF}
end;

procedure AVXMatrixNormalizeColumnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
{$IFDEF FPC}
begin
{$ENDIF}
asm
   // prolog
   push ebx;
   push edi;
   push esi;

   // note: ecx = dest, RDX = destLineWidth, edx = src, R9 = srcLineWidth
   xor edi, edi;
   sub edi, height;
   imul edi, srcLineWidth;

   // helper registers for the mt1 and dest pointers
   mov ecx, dest;
   mov edx, src;
   sub edx, edi;

   mov esi, srcLineWidth;

   // for x := 0 to width - 1:
   mov ebx, Width;
   sub ebx, 4;
   jl @@addforxloop4end;

   // 4 columns at once
   @@addforxloop4:
       vxorpd ymm1, ymm1, ymm1;

       // for y := 0 to height - 1;
       // prepare for reverse loop
       mov eax, edi;
       @addforyloop4:
           vmovapd ymm3, [edx + eax];
           vmulpd ymm3, ymm3, ymm3;
           vaddpd ymm1, ymm1, ymm3;
       add eax, esi;
       jnz @addforyloop4;

       // normalization factor
       lea eax, cOne;
       vbroadcastsd ymm2, [eax];
       vsqrtpd ymm1, ymm1;
       vdivpd ymm0, ymm2, ymm1;

       // normalize
       mov eax, edi;
       @addforyloop4_2:
           vmovapd ymm1, [edx + eax];
           vmulpd ymm1, ymm1, ymm0;
           vmovapd [ecx], ymm1;

       add ecx, destLineWidth;
       add eax, esi;
       jnz @addforyloop4_2;

       // next columns:
       mov ecx, dest;
       add ecx, 32;
       mov dest, ecx;
       add edx, 32;

   // loop x end
   sub ebx, 4;
   jge @@addforxloop4;

   @@addforxloop4end:

   add ebx, 4;
   jz @@endProc;

   sub ebx, 2;
   jl @@lastcolumn;

   vxorpd xmm1, xmm1, xmm1;

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov eax, edi;
   @addforyloop2:
       vmovapd xmm0, [edx + eax];
       vmulpd xmm0, xmm0, xmm0;
       vaddpd xmm1, xmm1, xmm0;
   add eax, esi;
   jnz @addforyloop2;

   // normalization factor
   lea eax, cOne;
   vmovddup xmm2, [eax];
   vsqrtpd xmm1, xmm1;
   vdivpd xmm0, xmm2, xmm1;

   // normalize
   mov eax, edi;
   @addforyloop2_2:
      vmovapd xmm1, [edx + eax];
      vmulpd xmm1, xmm1, xmm0;
      vmovapd [ecx], xmm1;
   add ecx, destLineWidth;
   add eax, esi;
   jnz @addforyloop2_2;

   // next columns:
   mov ecx, dest;
   add ecx, 16;
   mov dest, ecx;
   add edx, 16;

   dec ebx;
   jnz @@endProc;

   @@lastcolumn:

   vxorpd xmm1, xmm1, xmm1;

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov eax, edi;
   @addforyloop_1:
       vmovsd xmm0, [edx + eax];
       vmulsd xmm0, xmm0, xmm0;
       vaddsd xmm1, xmm1, xmm0;
   add eax, esi;
   jnz @addforyloop_1;


   // build result
   lea eax, cOne;
   vmovsd xmm2, [eax];
   vsqrtsd xmm1, xmm1, xmm1;
   vdivsd xmm0, xmm2, xmm1;

   // normalize last column
   mov eax, edi;
   @addforyloop2_1:
      vmovsd xmm1, [edx + eax];
      vmulsd xmm1, xmm1, xmm0;
      vmovsd [ecx], xmm1;
   add ecx, destLineWidth;
   add eax, esi;
   jnz @addforyloop2_1;

   @@endProc:

   // epilog
   vzeroupper;

   pop esi;
   pop edi;
   pop ebx;
{$IFDEF FPC}
end;
{$ENDIF}
end;


procedure AVXMatrixNormalizeColumnUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
{$IFDEF FPC}
begin
{$ENDIF}
asm
   // prolog
   push ebx;
   push edi;
   push esi;

   // note: ecx = dest, RDX = destLineWidth, edx = src, R9 = srcLineWidth
   xor edi, edi;
   sub edi, height;
   imul edi, srcLineWidth;

   // helper registers for the mt1 and dest pointers
   mov ecx, dest;
   mov edx, src;
   sub edx, edi;

   mov esi, srcLineWidth;

   // for x := 0 to width - 1:
   mov ebx, Width;
   sub ebx, 4;
   jl @@addforxloop4end;

   // 4 columns at once
   @@addforxloop4:
       vxorpd ymm1, ymm1, ymm1;

       // for y := 0 to height - 1;
       // prepare for reverse loop
       mov eax, edi;
       @addforyloop4:
           vmovupd ymm3, [edx + eax];
           vmulpd ymm3, ymm3, ymm3;
           vaddpd ymm1, ymm1, ymm3;
       add eax, esi;
       jnz @addforyloop4;

       // normalization factor
       lea eax, cOne;
       vbroadcastsd ymm2, [eax];
       vsqrtpd ymm1, ymm1;
       vdivpd ymm0, ymm2, ymm1;

       // normalize
       mov eax, edi;
       @addforyloop4_2:
           vmovupd ymm1, [edx + eax];
           vmulpd ymm1, ymm1, ymm0;
           vmovupd [ecx], ymm1;

       add ecx, destLineWidth;
       add eax, esi;
       jnz @addforyloop4_2;

       // next columns:
       mov ecx, dest;
       add ecx, 32;
       mov dest, ecx;
       add edx, 32;

   // loop x end
   sub ebx, 4;
   jge @@addforxloop4;

   @@addforxloop4end:

   add ebx, 4;
   jz @@endProc;

   sub ebx, 2;
   jl @@lastcolumn;

   vxorpd xmm1, xmm1, xmm1;

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov eax, edi;
   @addforyloop2:
       vmovupd xmm0, [edx + eax];
       vmulpd xmm0, xmm0, xmm0;
       vaddpd xmm1, xmm1, xmm0;
   add eax, esi;
   jnz @addforyloop2;

   // normalization factor
   lea eax, [cOne];
   vmovddup xmm2, [eax];
   vsqrtpd xmm1, xmm1;
   vdivpd xmm0, xmm2, xmm1;

   // normalize
   mov eax, edi;
   @addforyloop2_2:
      vmovupd xmm1, [edx + eax];
      vmulpd xmm1, xmm1, xmm0;
      vmovupd [ecx], xmm1;
   add ecx, destLineWidth;
   add eax, esi;
   jnz @addforyloop2_2;

   // next columns:
   mov ecx, dest;
   add ecx, 16;
   mov dest, ecx;
   add edx, 16;

   dec ebx;
   jnz @@endProc;

   @@lastcolumn:

   vxorpd xmm1, xmm1, xmm1;

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov eax, edi;
   @addforyloop_1:
       vmovsd xmm0, [edx + eax];
       vmulsd xmm0, xmm0, xmm0;
       vaddsd xmm1, xmm1, xmm0;
   add eax, esi;
   jnz @addforyloop_1;


   // build result
   lea eax, cOne;
   vmovsd xmm2, [eax];
   vsqrtsd xmm1, xmm1, xmm1;
   vdivsd xmm0, xmm2, xmm1;

   // normalize last column
   mov eax, edi;
   @addforyloop2_1:
      vmovsd xmm1, [edx + eax];
      vmulsd xmm1, xmm1, xmm0;
      vmovsd [ecx], xmm1;
   add ecx, destLineWidth;
   add eax, esi;
   jnz @addforyloop2_1;

   @@endProc:

   // epilog
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
