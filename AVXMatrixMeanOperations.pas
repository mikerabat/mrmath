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


unit AVXMatrixMeanOperations;

interface

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}
{$IFNDEF x64}

uses MatrixConst;

procedure AVXMatrixMeanRowAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
procedure AVXMatrixMeanRowUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);

procedure AVXMatrixMeanColumnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
procedure AVXMatrixMeanColumnUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);

procedure AVXMatrixVarRowAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean);
procedure AVXMatrixVarRowUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean);

procedure AVXMatrixVarColumnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean);
procedure AVXMatrixVarColumnUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean);


// combined methods
procedure AVXMatrixMeanVarRowAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean);
procedure AVXMatrixMeanVarRowUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean);

procedure AVXMatrixMeanVarColumnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean);
procedure AVXMatrixMeanVarColumnUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean);

{$ENDIF}

implementation

{$IFNDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$ENDIF}


procedure AVXMatrixMeanRowAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
{$IFDEF FPC}
begin
{$ENDIF}
asm
   // prolog
   push edi;
   push esi;
   push ebx;

   // iters := -width*sizeof(double)
   mov edi, width;
   imul edi, -8;

   // helper registers for the mt1, mt2 and dest pointers
   mov edx, src;
   sub edx, edi;
   mov ecx, dest;

   // fpc seems to have a problem with this opcode
   {$IFDEF FPC}
   mov eax, width;
   vcvtsi2sd xmm3, xmm3, eax;
   {$ELSE}
   cvtsi2sd xmm3, width;
   {$ENDIF}

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
           // prefetch [r8 + eax];

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

       vdivsd xmm0, xmm0, xmm3;

       // write result
       vmovsd [ecx], xmm0;

       // next line:
       add edx, srcLineWidth;
       add ecx, destLineWidth;

   // loop y end
   dec esi;
   jnz @@addforyloop;

   // epilog
   vzeroupper;

   pop ebx;
   pop esi;
   pop edi;
{$IFDEF FPC}
end;
{$ENDIF}
end;

procedure AVXMatrixMeanRowUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
{$IFDEF FPC}
begin
{$ENDIF}
asm
   // prolog
   push edi;
   push esi;
   push ebx;

   // iters := -width*sizeof(double)
   mov edi, width;
   imul edi, -8;

   // helper registers for the mt1, mt2 and dest pointers
   mov edx, src;
   sub edx, edi;
   mov ecx, dest;

   // fpc seems to have a problem with this opcode
   {$IFDEF FPC}
   mov eax, width;
   vcvtsi2sd xmm3, xmm3, eax;
   {$ELSE}
   cvtsi2sd xmm3, width;
   {$ENDIF}

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
           // prefetch [r8 + eax];

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

       vdivsd xmm0, xmm0, xmm3;

       // write result
       vmovsd [ecx], xmm0;

       // next line:
       add edx, srcLineWidth;
       add ecx, destLineWidth;

   // loop y end
   dec esi;
   jnz @@addforyloop;

   // epilog
   vzeroupper;

   pop ebx;
   pop esi;
   pop edi;
{$IFDEF FPC}
end;
{$ENDIF}
end;

procedure AVXMatrixMeanColumnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
var tmp : double;
{$IFDEF FPC}
begin
{$ENDIF}
asm
   // prolog
   push ebx;
   push edi;
   push esi;

   // prepare
   xor ebx, ebx;
   sub ebx, height;
   imul ebx, srcLineWidth;

   // helper registers for the mt1, mt2 and dest pointers
   mov ecx, dest;
   mov edx, src;
   sub edx, ebx;

   mov edi, srcLineWidth;

   // fpc seems to have a problem with this opcode
   {$IFDEF FPC}
   mov eax, height;
   vcvtsi2sd xmm0, xmm0, eax;
   {$ELSE}
   cvtsi2sd xmm0, height;
   {$ENDIF}
   lea eax, tmp;
   vmovsd [eax], xmm0;
   vbroadcastsd ymm2, [eax];
   // vbroadcastsd ymm2, xmm0; // avx2


   // for x := 0 to width - 1:
   mov esi, Width;
   sub esi, 4;
   jl @@addforxloop4end;

   // 4 columns at once
   @@addforxloop4:
       vxorpd ymm1, ymm1, ymm1;

       // for y := 0 to height - 1;
       // prepare for reverse loop
       mov eax, ebx;
       @addforyloop4:
           vaddpd ymm1, ymm1, [edx + eax];
       add eax, edi;
       jnz @addforyloop4;

       // build result
       vdivpd ymm1, ymm1, ymm2;
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
   mov eax, ebx;
   @addforyloop2:
       vaddpd xmm1, xmm1, [edx + eax];
   add eax, edi;
   jnz @addforyloop2;

   // build result
   vdivpd xmm1, xmm1, xmm2;
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
   mov eax, ebx;
   @addforyloop:
       vaddsd xmm1, xmm1, [edx + eax];
   add eax, edi;
   jnz @addforyloop;

   // build result
   vdivsd xmm1, xmm1, xmm2;
   vmovsd [ecx], xmm1;

   @@endProc:
   vzeroupper;

   // epilog
   pop esi;
   pop edi;
   pop ebx;
{$IFDEF FPC}
end;
{$ENDIF}
end;

procedure AVXMatrixMeanColumnUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
var tmp : double;
{$IFDEF FPC}
begin
{$ENDIF}
asm
   // prolog
   push ebx;
   push edi;
   push esi;

   // prepare
   xor ebx, ebx;
   sub ebx, height;
   imul ebx, srcLineWidth;

   // helper registers for the mt1, mt2 and dest pointers
   mov ecx, dest;
   mov edx, src;
   sub edx, ebx;

   mov edi, srcLineWidth;

   // fpc seems to have a problem with this opcode
   {$IFDEF FPC}
   mov eax, height;
   vcvtsi2sd xmm0, xmm0, eax;
   {$ELSE}
   cvtsi2sd xmm0, height;
   {$ENDIF}
   lea eax, tmp;
   vmovsd [eax], xmm0;
   vbroadcastsd ymm2, [eax];
   // vbroadcastsd ymm2, xmm0; // avx2


   // for x := 0 to width - 1:
   mov esi, Width;
   sub esi, 4;
   jl @@addforxloop4end;

   // 4 columns at once
   @@addforxloop4:
       vxorpd ymm1, ymm1, ymm1;

       // for y := 0 to height - 1;
       // prepare for reverse loop
       mov eax, ebx;
       @addforyloop4:
           vmovupd ymm0, [edx + eax];
           vaddpd ymm1, ymm1, ymm0;
       add eax, edi;
       jnz @addforyloop4;

       // build result
       vdivpd ymm1, ymm1, ymm2;
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
   mov eax, ebx;
   @addforyloop2:
       vmovupd xmm0, [edx + eax];
       vaddpd xmm1, xmm1, xmm0;
   add eax, edi;
   jnz @addforyloop2;

   // build result
   vdivpd xmm1, xmm1, xmm2;
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
   mov eax, ebx;
   @addforyloop:
       vaddsd xmm1, xmm1, [edx + eax];
   add eax, edi;
   jnz @addforyloop;

   // build result
   vdivsd xmm1, xmm1, xmm2;
   vmovsd [ecx], xmm1;

   @@endProc:
   vzeroupper;

   // epilog
   pop esi;
   pop edi;
   pop ebx;
{$IFDEF FPC}
end;
{$ENDIF}
end;

// ###########################################
// #### Variance calculation
// ###########################################


procedure AVXMatrixVarRowAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean);
var tmp : double;
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

   // helper registers for the src and dest pointers
   mov edx, src;
   sub edx, edi;
   mov ecx, dest;

   // fpc seems to have a problem with this opcode
   {$IFDEF FPC}
   mov eax, width;
   vcvtsi2sd xmm3, xmm3, eax;
   {$ELSE}
   cvtsi2sd xmm3, width;
   {$ENDIF}

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
           // prefetch [r8 + eax];

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
       vdivsd xmm0, xmm0, xmm3;

       // we have calculated the mean -> 
       // repeat the loop to calculate the variance
       lea eax, tmp;
       vmovsd [eax], xmm0;
       vbroadcastsd ymm4, [eax];
       // vbroadcastsd ymm4, xmm0; //avx2
       vxorpd xmm0, xmm0, xmm0;
            
       // for x := 0 to w - 1;
       // prepare for reverse loop
       // variance = sum (x - mean)^2
       mov eax, edi;
       @addforxloop3:
           add eax, 128;
           jg @loopEnd2;
           // prefetch data...
           // prefetch [ecx + eax];

           // addition:
           vmovapd ymm1, [edx + eax - 128];
           vsubpd ymm1, ymm1, ymm4;
           vmulpd ymm1, ymm1, ymm1;
           vaddpd ymm0, ymm0, ymm1;
                
           vmovapd ymm1, [edx + eax - 96];
           vsubpd ymm1, ymm1, ymm4;
           vmulpd ymm1, ymm1, ymm1;
           vaddpd ymm0, ymm0, ymm1;

           vmovapd ymm1, [edx + eax - 64];
           vsubpd ymm1, ymm1, ymm4;
           vmulpd ymm1, ymm1, ymm1;
           vaddpd ymm0, ymm0, ymm1;

           vmovapd ymm1, [edx + eax - 32];
           vsubpd ymm1, ymm1, ymm4;
           vmulpd ymm1, ymm1, ymm1;
           vaddpd ymm0, ymm0, ymm1;

       jmp @addforxloop3

       @loopEnd2:

       sub eax, 128;

       vextractf128 xmm2, ymm0, 1;
       vhaddpd xmm0, xmm0, xmm2;

       jz @buildRes2;

       @addforxloop4:
           add eax, 16;
           jg @addforxloop4end;

           vmovapd xmm1, [edx + eax - 16];
           vsubpd xmm1, xmm1, xmm4;
           vmulpd xmm1, xmm1, xmm1;
           vaddpd xmm0, xmm0, xmm1;
       jmp @addforxloop4;

       @addforxloop4end:

       sub eax, 16;
       jz @buildRes2;

       // last column
       vmovsd xmm1, [edx + eax];
       vsubsd xmm1, xmm1, xmm4;
       vmulsd xmm1, xmm1, xmm1;
       vaddsd xmm0, xmm0, xmm1;

       @buildRes2:

       // build result
       vhaddpd xmm0, xmm0, xmm0;
            
       // check if we need to use the unbiased version
       cmp unbiased, 0;
       jz @@dobiased;

       lea eax, cOne;
       vmovsd xmm2, [eax];
       vsubsd xmm4, xmm3, xmm2;
       vmaxsd xmm4, xmm4, xmm2;

       vdivsd xmm0, xmm0, xmm4;

       jmp @@writeRes;

       @@dobiased:
       vdivsd xmm0, xmm0, xmm3;
            
       // write result
       @@writeRes:
       vmovsd [ecx], xmm0;

       // next line:
       add edx, srcLineWidth;
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

procedure AVXMatrixVarRowUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean);
var tmp : double;
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

   // helper registers for the src and dest pointers
   mov edx, src;
   sub edx, edi;
   mov ecx, dest;

   // fpc seems to have a problem with this opcode
   {$IFDEF FPC}
   mov eax, width;
   vcvtsi2sd xmm3, xmm3, eax;
   {$ELSE}
   cvtsi2sd xmm3, width;
   {$ENDIF}

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
           // prefetch [r8 + eax];

           // addition:
           vmovupd ymm2, [edx + eax - 128];
           vaddpd ymm0, ymm0, ymm2;
           vmovupd ymm4, [edx + eax - 96];
           vaddpd ymm1, ymm1, ymm4;
           vmovupd ymm2, [edx + eax - 64];
           vaddpd ymm0, ymm0, ymm2;
           vmovupd ymm4, [edx + eax - 32];
           vaddpd ymm1, ymm1, ymm4;
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

           vmovupd xmm1, [edx + eax - 16];
           vaddpd xmm0, xmm0, xmm1;
       jmp @addforxloop2;

       @@addforxloop2end:

       sub eax, 16;
       jz @buildRes;

       vaddsd xmm0, xmm0, [edx + eax];

       @buildRes:

       // build result
       vhaddpd xmm0, xmm0, xmm0;
       vdivsd xmm0, xmm0, xmm3;

       // we have calculated the mean ->
       // repeat the loop to calculate the variance
       lea eax, tmp;
       vmovsd [eax], xmm0;
       vbroadcastsd ymm4, [eax];
       // vbroadcastsd ymm4, xmm0;  // avx2 instruction
       vxorpd xmm0, xmm0, xmm0;

       // for x := 0 to w - 1;
       // prepare for reverse loop
       // variance = sum (x - mean)^2
       mov eax, edi;
       @addforxloop3:
           add eax, 128;
           jg @loopEnd2;
           // prefetch data...
           // prefetch [ecx + eax];

           // addition:
           vmovupd ymm1, [edx + eax - 128];
           vsubpd ymm1, ymm1, ymm4;
           vmulpd ymm1, ymm1, ymm1;
           vaddpd ymm0, ymm0, ymm1;

           vmovupd ymm1, [edx + eax - 96];
           vsubpd ymm1, ymm1, ymm4;
           vmulpd ymm1, ymm1, ymm1;
           vaddpd ymm0, ymm0, ymm1;

           vmovupd ymm1, [edx + eax - 64];
           vsubpd ymm1, ymm1, ymm4;
           vmulpd ymm1, ymm1, ymm1;
           vaddpd ymm0, ymm0, ymm1;

           vmovupd ymm1, [edx + eax - 32];
           vsubpd ymm1, ymm1, ymm4;
           vmulpd ymm1, ymm1, ymm1;
           vaddpd ymm0, ymm0, ymm1;

       jmp @addforxloop3

       @loopEnd2:

       sub eax, 128;

       vextractf128 xmm2, ymm0, 1;
       vhaddpd xmm0, xmm0, xmm2;

       jz @buildRes2;

       @addforxloop4:
           add eax, 16;
           jg @addforxloop4end;

           vmovupd xmm1, [edx + eax - 16];
           vsubpd xmm1, xmm1, xmm4;
           vmulpd xmm1, xmm1, xmm1;
           vaddpd xmm0, xmm0, xmm1;
       jmp @addforxloop4;

       @addforxloop4end:

       sub eax, 16;
       jz @buildRes2;

       // last column
       vmovsd xmm1, [edx + eax];
       vsubsd xmm1, xmm1, xmm4;
       vmulsd xmm1, xmm1, xmm1;
       vaddsd xmm0, xmm0, xmm1;

       @buildRes2:

       // build result
       vhaddpd xmm0, xmm0, xmm0;

       // check if we need to use the unbiased version
       cmp unbiased, 0;
       jz @@dobiased;

       lea eax, cOne;
       vmovsd xmm2, [eax];
       vsubsd xmm4, xmm3, xmm2;
       vmaxsd xmm4, xmm4, xmm2;

       vdivsd xmm0, xmm0, xmm4;

       jmp @@writeRes;

       @@dobiased:
       vdivsd xmm0, xmm0, xmm3;

       // write result
       @@writeRes:
       vmovsd [ecx], xmm0;

       // next line:
       add edx, srcLineWidth;
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

procedure AVXMatrixVarColumnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean);
var tmp : double;
{$IFDEF FPC}
begin
{$ENDIF}
asm
   // prolog
   push ebx;
   push edi;
   push esi;

   // preparations
   xor edi, edi;
   sub edi, height;
   imul edi, srcLineWidth;

   // helper registers for the mt1, mt2 and dest pointers
   mov ecx, dest;
   mov edx, src;
   sub edx, edi;

   mov ebx, srcLineWidth;

   // fpc seems to have a problem with this opcode
   {$IFDEF FPC}
   mov eax, height;
   vcvtsi2sd xmm0, xmm0, eax;
   {$ELSE}
   cvtsi2sd xmm0, height;
   {$ENDIF}
   lea eax, tmp;
   vmovsd [eax], xmm0;
   vbroadcastsd ymm2, [eax];    // vbroadcastsd ymm2, xmm0; // avx2


   lea eax, cOne;
   vbroadcastsd ymm5, [eax];

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
       vdivpd ymm0, ymm1, ymm2;

       // calculate final standard deviation
       // for y := 0 to height - 1;
       // prepare for reverse loop
       vxorpd ymm4, ymm4, ymm4;
       mov eax, edi;
       @addforyloop4_2:
           vmovapd ymm1, [edx + eax];
           vsubpd ymm1, ymm1, ymm0;
           vmulpd ymm1, ymm1, ymm1;
           vaddpd ymm4, ymm4, ymm1;
       add eax, ebx;
       jnz @addforyloop4_2;

       // check if we need to use the unbiased version
       cmp unbiased, 0;
       jz @@dobiased_4;

       lea eax, cOne;

       vbroadcastsd ymm1, [eax];
       vsubpd ymm3, ymm2, ymm1;
       vmaxpd ymm3, ymm3, ymm1;

       vdivpd ymm4, ymm4, ymm3;

       jmp @@writeRes_4;

       @@dobiased_4:
       vdivpd ymm4, ymm4, ymm2;

       // write result
       @@writeRes_4:
       vmovapd [ecx], ymm4;

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
   vdivpd xmm0, xmm1, xmm2;

   // calculate final standard deviation
   // for y := 0 to height - 1;
   // prepare for reverse loop
   vxorpd xmm4, xmm4, xmm4;
   mov eax, edi;
   @addforyloop2_2:
      vmovapd xmm1, [edx + eax];
      vsubpd xmm1, xmm1, xmm0;
      vmulpd xmm1, xmm1, xmm1;
      vaddpd xmm4, xmm4, xmm1;
   add eax, ebx;
   jnz @addforyloop2_2;

   // check if we need to use the unbiased version
   cmp unbiased, 0;
   jz @@dobiased_2;

   vsubpd xmm3, xmm2, xmm5;
   vmaxpd xmm3, xmm3, xmm5;

   vdivpd xmm4, xmm4, xmm3;

   jmp @@writeRes2;

   @@dobiased_2:
   vdivpd xmm4, xmm4, xmm2;

   // write result
   @@writeRes2:
   vmovapd [ecx], xmm4;

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
   @addforyloop_1:
       vaddsd xmm1, xmm1, [edx + eax];
   add eax, ebx;
   jnz @addforyloop_1;

   // build result
   vdivsd xmm0, xmm1, xmm2;

   // calculate final standard deviation
   // for y := 0 to height - 1;
   // prepare for reverse loop
   vxorpd xmm4, xmm4, xmm4;
   mov eax, edi;
   @addforyloop1_2:
      vmovsd xmm1, [edx + eax];
      vsubsd xmm1, xmm1, xmm0;
      vmulsd xmm1, xmm1, xmm1;
      vaddsd xmm4, xmm4, xmm1;
   add eax, ebx;
   jnz @addforyloop1_2;

   // check if we need to use the unbiased version
   cmp unbiased, 0;
   jz @@dobiased_2;

   vmovsd xmm3, xmm3, xmm2;
   vsubsd xmm3, xmm3, xmm5;
   vmaxsd xmm3, xmm3, xmm5;

   vdivsd xmm4, xmm4, xmm3;

   jmp @@writeRes_1;

   @@dobiased_1:
   vdivsd xmm4, xmm4, xmm2;

   // write result
   @@writeRes_1:
   vmovsd [ecx], xmm4;

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

procedure AVXMatrixVarColumnUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean);
var tmp : double;
{$IFDEF FPC}
begin
{$ENDIF}
asm
   // prolog
   push ebx;
   push edi;
   push esi;


   // preparations
   xor edi, edi;
   sub edi, height;
   imul edi, srcLineWidth;

   // helper registers for the mt1, mt2 and dest pointers
   mov ecx, dest;
   mov edx, src;
   sub edx, edi;

   mov ebx, srcLineWidth;

   // fpc seems to have a problem with this opcode
   {$IFDEF FPC}
   mov eax, height;
   vcvtsi2sd xmm0, xmm0, eax;
   {$ELSE}
   cvtsi2sd xmm0, height;
   {$ENDIF}
   lea eax, tmp;
   vmovsd [eax], xmm0;
   vbroadcastsd ymm2, [eax]; // vbroadcastsd ymm2, xmm0; // avx2
   

   lea eax, cOne;
   vbroadcastsd ymm5, [eax];

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
       vdivpd ymm0, ymm1, ymm2;

       // calculate final standard deviation
       // for y := 0 to height - 1;
       // prepare for reverse loop
       vxorpd ymm4, ymm4, ymm4;
       mov eax, edi;
       @addforyloop4_2:
           vmovupd ymm1, [edx + eax];
           vsubpd ymm1, ymm1, ymm0;
           vmulpd ymm1, ymm1, ymm1;
           vaddpd ymm4, ymm4, ymm1;
       add eax, ebx;
       jnz @addforyloop4_2;

       // check if we need to use the unbiased version
       cmp unbiased, 0;
       jz @@dobiased_4;

       lea eax, cOne;
       vbroadcastsd ymm1, [eax];
       vsubpd ymm3, ymm2, ymm1;
       vmaxpd ymm3, ymm3, ymm1;

       vdivpd ymm4, ymm4, ymm3;

       jmp @@writeRes_4;

       @@dobiased_4:
       vdivpd ymm4, ymm4, ymm2;

       // write result
       @@writeRes_4:
       vmovupd [ecx], ymm4;

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
       vaddpd xmm1, xmm1, xmm0;
   add eax, ebx;
   jnz @addforyloop2;

   // build result
   vdivpd xmm0, xmm1, xmm2;

   // calculate final standard deviation
   // for y := 0 to height - 1;
   // prepare for reverse loop
   vxorpd xmm4, xmm4, xmm4;
   mov eax, edi;
   @addforyloop2_2:
      vmovupd xmm1, [edx + eax];
      vsubpd xmm1, xmm1, xmm0;
      vmulpd xmm1, xmm1, xmm1;
      vaddpd xmm4, xmm4, xmm1;
   add eax, ebx;
   jnz @addforyloop2_2;

   // check if we need to use the unbiased version
   cmp unbiased, 0;
   jz @@dobiased_2;

   vsubpd xmm3, xmm2, xmm5;
   vmaxpd xmm3, xmm3, xmm5;

   vdivpd xmm4, xmm4, xmm3;

   jmp @@writeRes2;

   @@dobiased_2:
   vdivpd xmm4, xmm4, xmm2;

   // write result
   @@writeRes2:
   vmovupd [ecx], xmm4;

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
   @addforyloop_1:
       vaddsd xmm1, xmm1, [edx + eax];
   add eax, ebx;
   jnz @addforyloop_1;

   // build result
   vdivsd xmm0, xmm1, xmm2;

   // calculate final standard deviation
   // for y := 0 to height - 1;
   // prepare for reverse loop
   vxorpd xmm4, xmm4, xmm4;
   mov eax, edi;
   @addforyloop1_2:
      vmovsd xmm1, [edx + eax];
      vsubsd xmm1, xmm1, xmm0;
      vmulsd xmm1, xmm1, xmm1;
      vaddsd xmm4, xmm4, xmm1;
   add eax, ebx;
   jnz @addforyloop1_2;

   // check if we need to use the unbiased version
   cmp unbiased, 0;
   jz @@dobiased_2;

   vmovsd xmm3, xmm3, xmm2;
   vsubsd xmm3, xmm3, xmm5;
   vmaxsd xmm3, xmm3, xmm5;

   vdivsd xmm4, xmm4, xmm3;

   jmp @@writeRes_1;

   @@dobiased_1:
   vdivsd xmm4, xmm4, xmm2;

   // write result
   @@writeRes_1:
   vmovsd [ecx], xmm4;

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

// #####################################################
// #### Combined mean variance calculation
// #####################################################

procedure AVXMatrixMeanVarRowAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean);
var tmp : double;
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

   // helper registers for the src and dest pointers
   mov edx, src;
   sub edx, edi;
   mov ecx, dest;

   // fpc seems to have a problem with this opcode
   {$IFDEF FPC}
   mov eax, width;
   vcvtsi2sd xmm3, xmm3, eax;
   {$ELSE}
   cvtsi2sd xmm3, width;
   {$ENDIF}

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
           // prefetch [r8 + eax];

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
       vdivsd xmm0, xmm0, xmm3;
       vmovsd [ecx], xmm0;

       // we have calculated the mean ->
       // repeat the loop to calculate the variance

       lea eax, tmp;
       vmovsd [eax], xmm0;
       vbroadcastsd ymm4, [eax];
       // vbroadcastsd ymm4, xmm0; // avx2 instruction
       vxorpd xmm0, xmm0, xmm0;

       // for x := 0 to w - 1;
       // prepare for reverse loop
       // variance = sum (x - mean)^2
       mov eax, edi;
       @addforxloop3:
           add eax, 128;
           jg @loopEnd2;
           // prefetch data...
           // prefetch [ecx + eax];

           // addition:
           vmovapd ymm1, [edx + eax - 128];
           vsubpd ymm1, ymm1, ymm4;
           vmulpd ymm1, ymm1, ymm1;
           vaddpd ymm0, ymm0, ymm1;

           vmovapd ymm1, [edx + eax - 96];
           vsubpd ymm1, ymm1, ymm4;
           vmulpd ymm1, ymm1, ymm1;
           vaddpd ymm0, ymm0, ymm1;

           vmovapd ymm1, [edx + eax - 64];
           vsubpd ymm1, ymm1, ymm4;
           vmulpd ymm1, ymm1, ymm1;
           vaddpd ymm0, ymm0, ymm1;

           vmovapd ymm1, [edx + eax - 32];
           vsubpd ymm1, ymm1, ymm4;
           vmulpd ymm1, ymm1, ymm1;
           vaddpd ymm0, ymm0, ymm1;

       jmp @addforxloop3

       @loopEnd2:

       sub eax, 128;

       vextractf128 xmm2, ymm0, 1;
       vhaddpd xmm0, xmm0, xmm2;

       jz @buildRes2;

       @addforxloop4:
           add eax, 16;
           jg @addforxloop4end;

           vmovapd xmm1, [edx + eax - 16];
           vsubpd xmm1, xmm1, xmm4;
           vmulpd xmm1, xmm1, xmm1;
           vaddpd xmm0, xmm0, xmm1;
       jmp @addforxloop4;

       @addforxloop4end:

       sub eax, 16;
       jz @buildRes2;

       // last column
       vmovsd xmm1, [edx + eax];
       vsubsd xmm1, xmm1, xmm4;
       vmulsd xmm1, xmm1, xmm1;
       vaddsd xmm0, xmm0, xmm1;

       @buildRes2:

       // build result
       vhaddpd xmm0, xmm0, xmm0;

       // check if we need to use the unbiased version
       cmp unbiased, 0;
       jz @@dobiased;

       lea eax, cOne;
       vmovsd xmm2, [eax];
       vsubsd xmm4, xmm3, xmm2;
       vmaxsd xmm4, xmm4, xmm2;

       vdivsd xmm0, xmm0, xmm4;

       jmp @@writeRes;

       @@dobiased:
       vdivsd xmm0, xmm0, xmm3;

       // write result
       @@writeRes:
       vmovsd [ecx + 8], xmm0;

       // next line:
       add edx, srcLineWidth;
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

procedure AVXMatrixMeanVarRowUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean);
var tmp : double;
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

   // helper registers for the src and dest pointers
   mov edx, src;
   sub edx, edi;
   mov ecx, dest;

   // fpc seems to have a problem with this opcode
   {$IFDEF FPC}
   mov eax, width;
   vcvtsi2sd xmm3, xmm3, eax;
   {$ELSE}
   cvtsi2sd xmm3, width;
   {$ENDIF}

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
           // prefetch [r8 + eax];

           // addition:
           vmovupd ymm2, [edx + eax - 128];
           vaddpd ymm0, ymm0, ymm2;
           vmovupd ymm4, [edx + eax - 96];
           vaddpd ymm1, ymm1, ymm4;
           vmovupd ymm2, [edx + eax - 64];
           vaddpd ymm0, ymm0, ymm2;
           vmovupd ymm4, [edx + eax - 32];
           vaddpd ymm1, ymm1, ymm4;
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

           vmovupd xmm1, [edx + eax - 16];
           vaddpd xmm0, xmm0, xmm1;
       jmp @addforxloop2;

       @@addforxloop2end:

       sub eax, 16;
       jz @buildRes;

       vaddsd xmm0, xmm0, [edx + eax];

       @buildRes:

       // build result
       vhaddpd xmm0, xmm0, xmm0;
       vdivsd xmm0, xmm0, xmm3;
       vmovsd [ecx], xmm0;

       // we have calculated the mean ->
       // repeat the loop to calculate the variance
       lea eax, tmp;
       vmovsd [eax], xmm0;
       vbroadcastsd ymm4, [eax];
       // vbroadcastsd ymm4, xmm0; // avx2
       vxorpd xmm0, xmm0, xmm0;

       // for x := 0 to w - 1;
       // prepare for reverse loop
       // variance = sum (x - mean)^2
       mov eax, edi;
       @addforxloop3:
           add eax, 128;
           jg @loopEnd2;
           // prefetch data...
           // prefetch [ecx + eax];

           // addition:
           vmovupd ymm1, [edx + eax - 128];
           vsubpd ymm1, ymm1, ymm4;
           vmulpd ymm1, ymm1, ymm1;
           vaddpd ymm0, ymm0, ymm1;

           vmovupd ymm1, [edx + eax - 96];
           vsubpd ymm1, ymm1, ymm4;
           vmulpd ymm1, ymm1, ymm1;
           vaddpd ymm0, ymm0, ymm1;

           vmovupd ymm1, [edx + eax - 64];
           vsubpd ymm1, ymm1, ymm4;
           vmulpd ymm1, ymm1, ymm1;
           vaddpd ymm0, ymm0, ymm1;

           vmovupd ymm1, [edx + eax - 32];
           vsubpd ymm1, ymm1, ymm4;
           vmulpd ymm1, ymm1, ymm1;
           vaddpd ymm0, ymm0, ymm1;

       jmp @addforxloop3

       @loopEnd2:

       sub eax, 128;

       vextractf128 xmm2, ymm0, 1;
       vhaddpd xmm0, xmm0, xmm2;

       jz @buildRes2;

       @addforxloop4:
           add eax, 16;
           jg @addforxloop4end;

           vmovupd xmm1, [edx + eax - 16];
           vsubpd xmm1, xmm1, xmm4;
           vmulpd xmm1, xmm1, xmm1;
           vaddpd xmm0, xmm0, xmm1;
       jmp @addforxloop4;

       @addforxloop4end:

       sub eax, 16;
       jz @buildRes2;

       // last column
       vmovsd xmm1, [edx + eax];
       vsubsd xmm1, xmm1, xmm4;
       vmulsd xmm1, xmm1, xmm1;
       vaddsd xmm0, xmm0, xmm1;

       @buildRes2:

       // build result
       vhaddpd xmm0, xmm0, xmm0;

       // check if we need to use the unbiased version
       cmp unbiased, 0;
       jz @@dobiased;

       lea eax, cOne;
       vmovsd xmm2, [eax];
       vsubsd xmm4, xmm3, xmm2;
       vmaxsd xmm4, xmm4, xmm2;

       vdivsd xmm0, xmm0, xmm4;

       jmp @@writeRes;

       @@dobiased:
       vdivsd xmm0, xmm0, xmm3;

       // write result
       @@writeRes:
       vmovsd [ecx + 8], xmm0;

       // next line:
       add edx, srcLineWidth;
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

procedure AVXMatrixMeanVarColumnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean);
var tmp : double;
{$IFDEF FPC}
begin
{$ENDIF}
asm
   // prolog
   push ebx;
   push edi;
   push esi;

   // Preparations
   xor edi, edi;
   sub edi, height;
   imul edi, srcLineWidth;

   // helper registers for the mt1, mt2 and dest pointers
   mov ecx, dest;
   mov edx, src;
   sub edx, edi;

   mov ebx, srcLineWidth;

   // fpc seems to have a problem with this opcode
   {$IFDEF FPC}
   mov eax, height;
   vcvtsi2sd xmm0, xmm0, eax;
   {$ELSE}
   cvtsi2sd xmm0, height;
   {$ENDIF}

   lea eax, tmp;
   vmovsd [tmp], xmm0;
   vbroadcastsd ymm2, [eax]; // vbroadcastsd ymm2, xmm0; // avx2
   

   lea eax, cOne;
   vbroadcastsd ymm5, [eax];

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
       vdivpd ymm0, ymm1, ymm2;
       vmovapd [ecx], ymm0;

       // calculate final standard deviation
       // for y := 0 to height - 1;
       // prepare for reverse loop
       vxorpd ymm4, ymm4, ymm4;
       mov eax, edi;
       @addforyloop4_2:
           vmovapd ymm1, [edx + eax];
           vsubpd ymm1, ymm1, ymm0;
           vmulpd ymm1, ymm1, ymm1;
           vaddpd ymm4, ymm4, ymm1;
       add eax, ebx;
       jnz @addforyloop4_2;

       // check if we need to use the unbiased version
       cmp unbiased, 0;
       jz @@dobiased_4;

       lea eax, cOne;

       vbroadcastsd ymm1, [eax];
       vsubpd ymm3, ymm2, ymm1;
       vmaxpd ymm3, ymm3, ymm1;

       vdivpd ymm4, ymm4, ymm3;

       jmp @@writeRes_4;

       @@dobiased_4:
       vdivpd ymm4, ymm4, ymm2;

       // write result
       @@writeRes_4:
       mov eax, destLineWidth;
       vmovapd [ecx + eax], ymm4;

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
   vdivpd xmm0, xmm1, xmm2;
   vmovapd [ecx], xmm0;

   // calculate final standard deviation
   // for y := 0 to height - 1;
   // prepare for reverse loop
   vxorpd xmm4, xmm4, xmm4;
   mov eax, edi;
   @addforyloop2_2:
      vmovapd xmm1, [edx + eax];
      vsubpd xmm1, xmm1, xmm0;
      vmulpd xmm1, xmm1, xmm1;
      vaddpd xmm4, xmm4, xmm1;
   add eax, ebx;
   jnz @addforyloop2_2;

   // check if we need to use the unbiased version
   cmp unbiased, 0;
   jz @@dobiased_2;

   vsubpd xmm3, xmm2, xmm5;
   vmaxpd xmm3, xmm3, xmm5;

   vdivpd xmm4, xmm4, xmm3;

   jmp @@writeRes2;

   @@dobiased_2:
   vdivpd xmm4, xmm4, xmm2;

   // write result
   @@writeRes2:
   mov eax, destLineWidth;
   vmovapd [ecx + eax], xmm4;

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
   @addforyloop_1:
       vaddsd xmm1, xmm1, [edx + eax];
   add eax, ebx;
   jnz @addforyloop_1;

   // build result
   vdivsd xmm0, xmm1, xmm2;
   vmovsd [ecx], xmm0;

   // calculate final standard deviation
   // for y := 0 to height - 1;
   // prepare for reverse loop
   vxorpd xmm4, xmm4, xmm4;
   mov eax, edi;
   @addforyloop1_2:
      vmovsd xmm1, [edx + eax];
      vsubsd xmm1, xmm1, xmm0;
      vmulsd xmm1, xmm1, xmm1;
      vaddsd xmm4, xmm4, xmm1;
   add eax, ebx;
   jnz @addforyloop1_2;

   // check if we need to use the unbiased version
   cmp unbiased, 0;
   jz @@dobiased_2;

   vmovsd xmm3, xmm3, xmm2;
   vsubsd xmm3, xmm3, xmm5;
   vmaxsd xmm3, xmm3, xmm5;

   vdivsd xmm4, xmm4, xmm3;

   jmp @@writeRes_1;

   @@dobiased_1:
   vdivsd xmm4, xmm4, xmm2;

   // write result
   @@writeRes_1:
   mov eax, destLineWidth;
   vmovsd [ecx + eax], xmm4;

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

procedure AVXMatrixMeanVarColumnUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean);
var tmp : double;
{$IFDEF FPC}
begin
{$ENDIF}
asm
   // prolog
   push ebx;
   push edi;
   push esi;

   // preparations
   xor edi, edi;
   sub edi, height;
   imul edi, srcLineWidth;

   // helper registers for the mt1, mt2 and dest pointers
   mov ecx, dest;
   mov edx, src;
   sub edx, edi;

   mov ebx, srcLineWidth;

   // fpc seems to have a problem with this opcode
   {$IFDEF FPC}
   mov eax, height;
   vcvtsi2sd xmm0, xmm0, eax;
   {$ELSE}
   cvtsi2sd xmm0, height;
   {$ENDIF}
   lea eax, tmp;
   vmovsd [eax], xmm0;
   vbroadcastsd ymm2, [eax]; // vbroadcastsd ymm2, xmm0; // avx2
   

   lea eax, cOne;
   vbroadcastsd ymm5, [eax];

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
       vdivpd ymm0, ymm1, ymm2;
       vmovupd [ecx], ymm0;

       // calculate final standard deviation
       // for y := 0 to height - 1;
       // prepare for reverse loop
       vxorpd ymm4, ymm4, ymm4;
       mov eax, edi;
       @addforyloop4_2:
           vmovupd ymm1, [edx + eax];
           vsubpd ymm1, ymm1, ymm0;
           vmulpd ymm1, ymm1, ymm1;
           vaddpd ymm4, ymm4, ymm1;
       add eax, ebx;
       jnz @addforyloop4_2;

       // check if we need to use the unbiased version
       cmp unbiased, 0;
       jz @@dobiased_4;

       lea eax, cOne;
       vbroadcastsd ymm1, [eax];
       vsubpd ymm3, ymm2, ymm1;
       vmaxpd ymm3, ymm3, ymm1;

       vdivpd ymm4, ymm4, ymm3;

       jmp @@writeRes_4;

       @@dobiased_4:
       vdivpd ymm4, ymm4, ymm2;

       // write result
       @@writeRes_4:
       mov eax, destLineWidth;
       vmovupd [ecx + eax], ymm4;

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
       vaddpd xmm1, xmm1, xmm0;
   add eax, ebx;
   jnz @addforyloop2;

   // build result
   vdivpd xmm0, xmm1, xmm2;
   vmovupd [ecx], xmm0;

   // calculate final standard deviation
   // for y := 0 to height - 1;
   // prepare for reverse loop
   vxorpd xmm4, xmm4, xmm4;
   mov eax, edi;
   @addforyloop2_2:
      vmovupd xmm1, [edx + eax];
      vsubpd xmm1, xmm1, xmm0;
      vmulpd xmm1, xmm1, xmm1;
      vaddpd xmm4, xmm4, xmm1;
   add eax, ebx;
   jnz @addforyloop2_2;

   // check if we need to use the unbiased version
   cmp unbiased, 0;
   jz @@dobiased_2;

   vsubpd xmm3, xmm2, xmm5;
   vmaxpd xmm3, xmm3, xmm5;

   vdivpd xmm4, xmm4, xmm3;

   jmp @@writeRes2;

   @@dobiased_2:
   vdivpd xmm4, xmm4, xmm2;

   // write result
   @@writeRes2:
   mov eax, destLineWidth;
   vmovupd [ecx + eax], xmm4;

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
   @addforyloop_1:
       vaddsd xmm1, xmm1, [edx + eax];
   add eax, ebx;
   jnz @addforyloop_1;

   // build result
   vdivsd xmm0, xmm1, xmm2;
   vmovsd [ecx], xmm0;

   // calculate final standard deviation
   // for y := 0 to height - 1;
   // prepare for reverse loop
   vxorpd xmm4, xmm4, xmm4;
   mov eax, edi;
   @addforyloop1_2:
      vmovsd xmm1, [edx + eax];
      vsubsd xmm1, xmm1, xmm0;
      vmulsd xmm1, xmm1, xmm1;
      vaddsd xmm4, xmm4, xmm1;
   add eax, ebx;
   jnz @addforyloop1_2;

   // check if we need to use the unbiased version
   cmp unbiased, 0;
   jz @@dobiased_2;

   vmovsd xmm3, xmm3, xmm2;
   vsubsd xmm3, xmm3, xmm5;
   vmaxsd xmm3, xmm3, xmm5;

   vdivsd xmm4, xmm4, xmm3;

   jmp @@writeRes_1;

   @@dobiased_1:
   vdivsd xmm4, xmm4, xmm2;

   // write result
   @@writeRes_1:
   mov eax, destLineWidth;
   vmovsd [ecx + eax], xmm4;

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
