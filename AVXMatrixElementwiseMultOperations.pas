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


unit AVXMatrixElementwiseMultOperations;

interface

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}
{$IFNDEF x64}

uses MatrixConst;

procedure AVXMatrixElemMultAligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
procedure AVXMatrixElemMultUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);

procedure AVXMatrixElemDivAligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
procedure AVXMatrixElemDivUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);

{$ENDIF}

implementation

{$IFNDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$ENDIF}

procedure AVXMatrixElemMultAligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
{$IFDEF FPC}
begin
{$ENDIF}
asm
   // prolog - simulate stack
   push ebx;
   push edi;
   push esi;

   //iters := -width*sizeof(double);
   mov edi, width;
   imul edi, -8;

   // helper registers for the mt1, mt2 and dest pointers
   mov ecx, dest;
   mov edx, mt1;
   mov esi, mt2;

   sub ecx, edi;
   sub edx, edi;
   sub esi, edi;

   // for y := 0 to height - 1:
   mov ebx, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov eax, edi;
       @addforxloop:
           add eax, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [r8 + rax];
           // prefetch [r9 + rax];

           // mult:
           vmovapd ymm0, [edx + eax - 128];
           vmulpd ymm0, ymm0, [esi + eax - 128];

           vmovapd [ecx + eax - 128], ymm0;

           vmovapd ymm1, [edx + eax - 96];
           vmulpd ymm1, ymm1, [esi + eax - 96];

           vmovapd [ecx + eax - 96], ymm1;

           vmovapd ymm2, [edx + eax - 64];
           vmulpd ymm2, ymm2, [esi + eax - 64];

           vmovapd [ecx + eax - 64], ymm2;

           vmovapd ymm3, [edx + eax - 32];
           vmulpd ymm3, ymm3, [esi + eax - 32];

           vmovapd [ecx + eax - 32], ymm3;
       jmp @addforxloop

       @loopEnd:

       sub eax, 128;

       jz @nextLine;

       @addforxloop2:
           add eax, 16;
           jg @@lastElem;

           vmovapd xmm0, [edx + eax - 16];
           vmulpd xmm0, xmm0, [esi + eax - 16];

           vmovapd [ecx + eax - 16], xmm0;
       jmp @addforxloop2;

       @@lastElem:
       sub eax, 16;

       jz @nextLine;

       vmovsd xmm0, [edx + eax];
       vmulsd xmm0, xmm0, [esi + eax];
       vmovsd [ecx + eax], xmm0;

       @nextLine:

       // next line:
       add edx, LineWidth1;
       add esi, LineWidth2;
       add ecx, destLineWidth;

   // loop y end
   dec ebx;
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

procedure AVXMatrixElemMultUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
{$IFDEF FPC}
begin
{$ENDIF}
asm
   // prolog - simulate stack
   push ebx;
   push edi;
   push esi;

   //iters := -width*sizeof(double);
   mov edi, width;
   imul edi, -8;

   // helper registers for the mt1, mt2 and dest pointers
   mov ecx, dest;
   mov edx, mt1;
   mov esi, mt2;

   sub ecx, edi;
   sub edx, edi;
   sub esi, edi;

   // for y := 0 to height - 1:
   mov ebx, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov eax, edi;
       @addforxloop:
           add eax, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [edx + eax];
           // prefetch [esi + eax];

           // mult:
           vmovupd ymm0, [edx + eax - 128];
           vmovupd ymm1, [esi + eax - 128];
           vmulpd ymm0, ymm0, ymm1;

           vmovupd [ecx + eax - 128], ymm0;

           vmovupd ymm2, [edx + eax - 96];
           vmovupd ymm3, [esi + eax - 96];
           vmulpd ymm2, ymm2, ymm3;

           vmovupd [ecx + eax - 96], ymm2;

           vmovupd ymm0, [edx + eax - 64];
           vmovupd ymm1, [esi + eax - 64];
           vmulpd ymm0, ymm0, ymm1;

           vmovupd [ecx + eax - 64], ymm0;

           vmovupd ymm2, [edx + eax - 32];
           vmovupd ymm3, [esi + eax - 32];
           vmulpd ymm2, ymm2, ymm3;

           vmovupd [ecx + eax - 32], ymm2;
       jmp @addforxloop

       @loopEnd:

       sub eax, 128;

       jz @nextLine;

       @addforxloop2:
           add eax, 16;
           jg @@lastElem;

           vmovupd xmm0, [edx + eax - 16];
           vmovupd xmm1, [esi + eax - 16];
           vmulpd xmm0, xmm0, xmm1;

           vmovupd [ecx + eax - 16], xmm0;
       jmp @addforxloop2;

       @@lastElem:
       sub eax, 16;

       jz @nextLine;

       vmovsd xmm0, [edx + eax];
       vmulsd xmm0, xmm0, [esi + eax];
       vmovsd [ecx + eax], xmm0;

       @nextLine:

       // next line:
       add edx, LineWidth1;
       add esi, LineWidth2;
       add ecx, destLineWidth;

   // loop y end
   dec ebx;
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

// ##############################################################
// #### Elementwise divide of matrix 1 to matrix 2
// ##############################################################

procedure AVXMatrixElemDivAligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var iebx, iLineWidth1, iR12 : TASMNativeInt;
{$IFDEF FPC}
begin
{$ENDIF}
asm
   // prolog - simulate stack
   push ebx;
   push edi;
   push esi;

   //iters := -width*sizeof(double);
   mov edi, width;
   imul edi, -8;

   // helper registers for the mt1, mt2 and dest pointers
   mov ecx, dest;
   mov edx, mt1;
   mov esi, mt2;

   sub edx, edi;
   sub esi, edi;
   sub ecx, edi;

   // for y := 0 to height - 1:
   mov ebx, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov eax, edi;
       @addforxloop:
           add eax, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [edx + eax];
           // prefetch [esi + eax];

           // mult:
           vmovapd ymm0, [edx + eax - 128];
           vdivpd ymm0, ymm0, [esi + eax - 128];

           vmovapd [ecx + eax - 128], ymm0;

           vmovapd ymm1, [edx + eax - 96];
           vdivpd ymm1, ymm1, [esi + eax - 96];

           vmovapd [ecx + eax - 96], ymm1;

           vmovapd ymm2, [edx + eax - 64];
           vdivpd ymm2, ymm2, [esi + eax - 64];

           vmovapd [ecx + eax - 64], ymm2;

           vmovapd ymm3, [edx + eax - 32];
           vdivpd ymm3, ymm3, [esi + eax - 32];

           vmovapd [ecx + eax - 32], ymm3;
       jmp @addforxloop

       @loopEnd:

       sub eax, 128;

       jz @nextLine;

       @addforxloop2:
           add eax, 16;
           jg @@lastElem;

           vmovapd xmm0, [edx + eax - 16];
           vdivpd xmm0, xmm0, [esi + eax - 16];

           vmovapd [ecx + eax - 16], xmm0;
       jmp @addforxloop2;

       @@lastElem:
       sub eax, 16;

       jz @nextLine;

       vmovsd xmm0, [edx + eax];
       vdivsd xmm0, xmm0, [esi + eax];
       vmovsd [ecx + eax], xmm0;

       @nextLine:

       // next line:
       add edx, LineWidth1;
       add esi, LineWidth2;
       add ecx, destLineWidth;

   // loop y end
   dec ebx;
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

procedure AVXMatrixElemDivUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var iebx, iLineWidth1, iR12 : TASMNativeInt;
{$IFDEF FPC}
begin
{$ENDIF}
asm
   // prolog - simulate stack
   push ebx;
   push edi;
   push esi;

   //iters := -width*sizeof(double);
   mov edi, width;
   imul edi, -8;

   // helper registers for the mt1, mt2 and dest pointers
   mov ecx, dest;
   mov edx, mt1;
   mov esi, mt2;

   sub edx, edi;
   sub esi, edi;
   sub ecx, edi;

   // for y := 0 to height - 1:
   mov ebx, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov eax, edi;
       @addforxloop:
           add eax, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [edx + eax];
           // prefetch [esi + eax];

           // mult:
           vmovupd ymm0, [edx + eax - 128];
           vmovupd ymm1, [esi + eax - 128];
           vdivpd ymm0, ymm0, ymm1;

           vmovupd [ecx + eax - 128], ymm0;

           vmovupd ymm2, [edx + eax - 96];
           vmovupd ymm3, [esi + eax - 96];
           vdivpd ymm2, ymm2, ymm3;

           vmovupd [ecx + eax - 96], ymm2;

           vmovupd ymm0, [edx + eax - 64];
           vmovupd ymm1, [esi + eax - 64];
           vdivpd ymm0, ymm0, ymm1;

           vmovupd [ecx + eax - 64], ymm0;

           vmovupd ymm2, [edx + eax - 32];
           vmovupd ymm3, [esi + eax - 32];
           vdivpd ymm2, ymm2, ymm3;

           vmovupd [ecx + eax - 32], ymm2;
       jmp @addforxloop

       @loopEnd:

       sub eax, 128;

       jz @nextLine;

       @addforxloop2:
           add eax, 16;
           jg @@lastElem;

           vmovupd xmm0, [edx + eax - 16];
           vmovupd xmm1, [esi + eax - 16];
           vdivpd xmm0, xmm0, xmm1;

           vmovupd [ecx + eax - 16], xmm0;
       jmp @addforxloop2;

       @@lastElem:
       sub eax, 16;

       jz @nextLine;

       vmovsd xmm0, [edx + eax];
       vdivsd xmm0, xmm0, [esi + eax];
       vmovsd [ecx + eax], xmm0;

       @nextLine:

       // next line:
       add edx, LineWidth1;
       add esi, LineWidth2;
       add ecx, destLineWidth;

   // loop y end
   dec ebx;
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

{$ENDIF}

end.
