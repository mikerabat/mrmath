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


unit AVXMatrixAddSubOperations;

// ############################################################
// ##### Matrix addition/subtraction assembler optimized:
// ############################################################

interface

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}
{$IFNDEF x64}

uses MatrixConst;

procedure AVXMatrixAddAligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
procedure AVXMatrixAddUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);

procedure AVXMatrixSubAligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
procedure AVXMatrixSubUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);

procedure AVXMatrixSubT(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; LineWidthB : TASMNativeInt; width, height : TASMNativeInt);
{$ENDIF}

implementation

{$IFNDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$ENDIF}

procedure AVXMatrixAddAligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var iters : TASMNativeInt;
{$IFDEF FPC}
begin
{$ENDIF}
asm
   push ebx;
   push esi;
   push edi;

   //iters := -width*sizeof(double);
   mov eax, width;
   imul eax, -8;
   mov iters, eax;

   // helper registers for the mt1, mt2 and dest pointers
   mov ecx, dest;
   mov edi, mt2;
   mov esi, mt1;

   sub esi, eax;
   sub edi, eax;
   sub ecx, eax;

   // for y := 0 to height - 1:
   mov ebx, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov eax, iters;
       @addforxloop:
           add eax, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [esi + eax];
           // prefetch [edi + eax];

           // addition:
           vmovapd ymm0, [esi + eax - 128];
           vaddpd ymm0, ymm0, [edi + eax - 128];

           vmovapd ymm1, [esi + eax - 96];
           vaddpd ymm1, ymm1, [edi + eax - 96];

           vmovapd ymm2, [esi + eax - 64];
           vaddpd ymm2, ymm2, [edi + eax - 64];

           vmovapd ymm3, [esi + eax - 32];
           vaddpd ymm3, ymm3, [edi + eax - 32];

           vmovapd [ecx + eax - 128], ymm0;
           vmovapd [ecx + eax - 96], ymm1;
           vmovapd [ecx + eax - 64], ymm2;
           vmovapd [ecx + eax - 32], ymm3;
       jmp @addforxloop

       @loopEnd:

       sub eax, 128;

       jz @nextLine;

       add eax, 16;
       jg @@lastelem;

       @addforxloop2:
           vmovapd xmm0, [esi + eax - 16];
           vaddpd xmm0, xmm0, [edi + eax - 16];

           vmovapd [ecx + eax - 16], xmm0;
       add eax, 16;
       jle @addforxloop2;

       @@lastelem:

       sub eax, 16;
       jz @nextLine;

       vmovsd xmm0, [esi - 8];
       vmovsd xmm1, [edi - 8];
       vaddsd xmm0, xmm0, xmm1;
       vmovsd [ecx - 8], xmm0;

       @nextLine:

       // next line:
       add esi, LineWidth1;
       add edi, LineWidth2;
       add ecx, destLineWidth;

   // loop y end
   dec ebx;
   jnz @@addforyloop;

   // epilog
   vzeroupper;

   pop edi;
   pop esi;
   pop ebx;
end;
{$IFDEF FPC}
end;
{$ENDIF}

procedure AVXMatrixAddUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var iters : TASMNativeInt;
{$IFDEF FPC}
begin
{$ENDIF}
asm
   push ebx;
   push esi;
   push edi;

   //iters := -width*sizeof(double);
   mov eax, width;
   imul eax, -8;
   mov iters, eax;

   // helper registers for the mt1, mt2 and dest pointers
   mov ecx, dest;
   mov edi, mt2;
   mov esi, mt1;

   sub esi, eax;
   sub edi, eax;
   sub ecx, eax;

   // for y := 0 to height - 1:
   mov ebx, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov eax, iters;
       @addforxloop:
           add eax, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [esi + eax];
           // prefetch [edi + eax];

           // addition:
           vmovupd ymm0, [esi + eax - 128];
           vmovupd ymm1, [edi + eax - 128];
           vaddpd ymm0, ymm0, ymm1;
           vmovupd [ecx + eax - 128], ymm0;

           vmovupd ymm0, [esi + eax - 96];
           vmovupd ymm1, [edi + eax - 96];
           vaddpd ymm0, ymm0, ymm1;
           vmovupd [ecx + eax - 96], ymm0;

           vmovupd ymm0, [esi + eax - 64];
           vmovupd ymm1, [edi + eax - 64];
           vaddpd ymm0, ymm0, ymm1;
           vmovupd [ecx + eax - 64], ymm0;

           vmovupd ymm0, [esi + eax - 32];
           vmovupd ymm1, [edi + eax - 32];
           vaddpd ymm0, ymm0, ymm1;
           vmovupd [ecx + eax - 32], ymm0;

       jmp @addforxloop

       @loopEnd:

       sub eax, 128;

       jz @nextLine;

       add eax, 16;
       jg @@lastelem;

       @addforxloop2:
           vmovupd xmm0, [esi + eax - 16];
           vmovupd xmm1, [edi + eax - 16];
           vaddpd xmm0, xmm0, xmm1;

           vmovupd [ecx + eax - 16], xmm0;
       add eax, 16;
       jle @addforxloop2;

       @@lastelem:

       sub eax, 16;
       jz @nextLine;

       vmovsd xmm0, [esi - 8];
       vmovsd xmm1, [edi - 8];
       vaddsd xmm0, xmm0, xmm1;
       vmovsd [ecx - 8], xmm0;

       @nextLine:

       // next line:
       add esi, LineWidth1;
       add edi, LineWidth2;
       add ecx, destLineWidth;

   // loop y end
   dec ebx;
   jnz @@addforyloop;

   // epilog
   vzeroupper;

   pop edi;
   pop esi;
   pop ebx;
end;
{$IFDEF FPC}
end;
{$ENDIF}

procedure AVXMatrixSubAligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var iters : TASMNativeInt;
{$IFDEF FPC}
begin
{$ENDIF}
asm
   push ebx;
   push esi;
   push edi;

   //iters := -width*sizeof(double);
   mov eax, width;
   imul eax, -8;
   mov iters, eax;

   // helper registers for the mt1, mt2 and dest pointers
   mov ecx, dest;
   mov edi, mt2;
   mov esi, mt1;

   sub esi, eax;
   sub edi, eax;
   sub ecx, eax;

   // for y := 0 to height - 1:
   mov ebx, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov eax, iters;
       @addforxloop:
           add eax, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [esi + eax];
           // prefetch [edi + eax];

           // addition:
           vmovapd ymm0, [esi + eax - 128];
           vsubpd ymm0, ymm0, [edi + eax - 128];

           vmovapd ymm1, [esi + eax - 96];
           vsubpd ymm1, ymm1, [edi + eax - 96];

           vmovapd ymm2, [esi + eax - 64];
           vsubpd ymm2, ymm2, [edi + eax - 64];

           vmovapd ymm3, [esi + eax - 32];
           vsubpd ymm3, ymm3, [edi + eax - 32];

           vmovapd [ecx + eax - 128], ymm0;
           vmovapd [ecx + eax - 96], ymm1;
           vmovapd [ecx + eax - 64], ymm2;
           vmovapd [ecx + eax - 32], ymm3;
       jmp @addforxloop

       @loopEnd:

       sub eax, 128;

       jz @nextLine;

       add eax, 16;
       jg @@lastelem;

       @addforxloop2:
           vmovapd xmm0, [esi + eax - 16];
           vsubpd xmm0, xmm0, [edi + eax - 16];

           vmovapd [ecx + eax - 16], xmm0;
       add eax, 16;
       jle @addforxloop2;

       @@lastelem:

       sub eax, 16;
       jz @nextLine;

       vmovsd xmm0, [esi - 8];
       vmovsd xmm1, [edi - 8];
       vsubsd xmm0, xmm0, xmm1;
       vmovsd [ecx - 8], xmm0;

       @nextLine:

       // update pointers
       add esi, LineWidth1;
       add edi, LineWidth2;
       add ecx, destLineWidth;

   // loop y end
   dec ebx;
   jnz @@addforyloop;

   // epilog
   vzeroupper;

   pop edi;
   pop esi;
   pop ebx;
end;
{$IFDEF FPC}
end;
{$ENDIF}

procedure AVXMatrixSubUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var iters : TASMNativeInt;
{$IFDEF FPC}
begin
{$ENDIF}
asm
   push ebx;
   push esi;
   push edi;

   //iters := -width*sizeof(double);
   mov eax, width;
   imul eax, -8;
   mov iters, eax;

   // helper registers for the mt1, mt2 and dest pointers
   mov ecx, dest;
   mov edi, mt2;
   mov esi, mt1;

   sub esi, eax;
   sub edi, eax;
   sub ecx, eax;

   // for y := 0 to height - 1:
   mov ebx, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov eax, iters;
       @addforxloop:
           add eax, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [esi + eax];
           // prefetch [edi + eax];

           // addition:
           vmovupd ymm0, [esi + eax - 128];
           vmovupd ymm1, [edi + eax - 128];
           vsubpd ymm0, ymm0, ymm1;
           vmovupd [ecx + eax - 128], ymm0;

           vmovupd ymm0, [esi + eax - 96];
           vmovupd ymm1, [edi + eax - 96];
           vsubpd ymm0, ymm0, ymm1;
           vmovupd [ecx + eax - 96], ymm0;

           vmovupd ymm0, [esi + eax - 64];
           vmovupd ymm1, [edi + eax - 64];
           vsubpd ymm0, ymm0, ymm1;
           vmovupd [ecx + eax - 64], ymm0;

           vmovupd ymm0, [esi + eax - 32];
           vmovupd ymm1, [edi + eax - 32];
           vsubpd ymm0, ymm0, ymm1;
           vmovupd [ecx + eax - 32], ymm0;

       jmp @addforxloop

       @loopEnd:

       sub eax, 128;

       jz @nextLine;

       add eax, 16;
       jg @@lastelem;

       @addforxloop2:
           vmovupd xmm0, [esi + eax - 16];
           vmovupd xmm1, [edi + eax - 16];
           vsubpd xmm0, xmm0, xmm1;

           vmovupd [ecx + eax - 16], xmm0;
       add eax, 16;
       jle @addforxloop2;

       @@lastelem:

       sub eax, 16;
       jz @nextLine;

       vmovsd xmm0, [esi - 8];
       vmovsd xmm1, [edi - 8];
       vsubsd xmm0, xmm0, xmm1;
       vmovsd [ecx - 8], xmm0;

       @nextLine:

       // next line:
       add esi, LineWidth1;
       add edi, LineWidth2;
       add ecx, destLineWidth;

   // loop y end
   dec ebx;
   jnz @@addforyloop;

   // epilog
   vzeroupper;

   pop edi;
   pop esi;
   pop ebx;
end;
{$IFDEF FPC}
end;
{$ENDIF}

procedure AVXMatrixSubT(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; LineWidthB : TASMNativeInt; width, height : TASMNativeInt);
var iebx : TASMNativeInt;
{$IFDEF FPC}
begin
{$ENDIF}
asm
   push esi;
   push edi;
   push ebx;

   // eax: iter := -width*sizeof(double)
   mov ecx, A;
   mov eax, width;
   imul eax, -8;
   sub ecx, eax;

   mov esi, B;
   mov edx, LineWidthB;

   // for y := 0 to height - 1
   @@foryloop:
      mov edi, esi;
      mov ebx, eax;

      // for x := 0 to width - 1
      @@forxloop:
         vmovsd xmm0, [ecx + ebx];
         vmovsd xmm1, [edi];
         vsubsd xmm0, xmm0, xmm1;
         vmovsd [ecx + ebx], xmm0;

         add edi, edx;
      add ebx, 8;
      jnz @@forxloop;

      add ecx, LineWidthA;
      add esi, 8;
   dec height;
   jnz @@foryloop;

   // epilog
   vzeroupper;

   pop ebx;
   pop edi;
   pop esi;
end;
{$IFDEF FPC}
end;
{$ENDIF}


{$ENDIF}

end.
