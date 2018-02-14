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


unit AVXMatrixMinMaxOperations;

interface

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}
{$IFNDEF x64}

uses MatrixConst;

function AVXMatrixMaxAligned(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double;
function AVXMatrixMaxUnAligned(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double;

function AVXMatrixMinAligned(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double;
function AVXMatrixMinUnAligned(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double;

{$ENDIF}

implementation

{$IFNDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$ENDIF}

function AVXMatrixMaxAligned(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double;
{$IFDEF FPC}
begin
{$ENDIF}
asm
   // prolog - simulate stack
   push ebx;
   push edi;

   // prepare pointers
   mov edi, LineWidth;
   mov ecx, mt;
   mov edx, width;
   imul edx, -8;

   sub ecx, edx;

   // init result
   lea eax, cNegMaxDouble;
   vbroadcastsd ymm0, [eax];
   vmovapd ymm3, ymm0;
   vmovapd ymm4, ymm0;

   // for y := 0 to height - 1:
   mov ebx, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov eax, edx;
       add eax, 128;
       jg @loopEnd;

       @addforxloop:
           // prefetch data...
           // prefetch [ecx + eax];

           // max:
           vmaxpd ymm3, ymm3, [ecx + eax - 128];
           vmaxpd ymm4, ymm4, [ecx + eax - 96];
           vmaxpd ymm3, ymm3, [ecx + eax - 64];
           vmaxpd ymm4, ymm4, [ecx + eax - 32];

       add eax, 128;
       jle @addforxloop;

       vextractf128 xmm2, ymm3, 1;
       vmaxpd xmm2, xmm2, xmm3;
       vmaxpd xmm0, xmm0, xmm2;
       vextractf128 xmm2, ymm4, 1;
       vmaxpd xmm2, xmm2, xmm4;
       vmaxpd xmm0, xmm0, xmm2;

       @loopEnd:

       sub eax, 128;

       jz @nextLine;

       @addforxloop2:
           add eax, 16;
           jg @addforxloop2end;

           vmaxpd xmm0, xmm0, [ecx + eax - 16];
       jmp @addforxloop2;

       @addforxloop2end:

       sub eax, 16;
       jz @nextLine;

       vmovsd xmm2, [ecx + eax];
       vmaxsd xmm0, xmm0, xmm2;

       @nextLine:

       // next line:
       add ecx, edi;

   // loop y end
   dec ebx;
   jnz @@addforyloop;

   // final max ->
   vmovhlps xmm1, xmm1, xmm0;
   vmaxsd xmm0, xmm0, xmm1;

   vmovsd Result, xmm0;

   // epilog - cleanup stack
   vzeroupper;

   pop ebx;
   pop edi;
{$IFDEF FPC}
end;
{$ENDIF}
end;

function AVXMatrixMaxUnAligned(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double;
{$IFDEF FPC}
begin
{$ENDIF}
asm
   // prolog - simulate stack
   push ebx;
   push edi;

   // prepare pointers
   mov edi, LineWidth;
   mov ecx, mt;
   mov edx, width;
   imul edx, -8;

   sub ecx, edx;

   // init result
   lea eax, cNegMaxDouble;
   vbroadcastsd ymm0, [eax];
   vmovapd ymm3, ymm0;
   vmovapd ymm4, ymm0;

   // for y := 0 to height - 1:
   mov ebx, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov eax, edx;

       add eax, 128;
       jg @loopEnd;

       @addforxloop:
           // prefetch data...
           // prefetch [ecx + eax];

           // max:
           vmovupd ymm2, [ecx + eax - 128];
           vmaxpd ymm3, ymm3, ymm2;
           vmovupd ymm2, [ecx + eax - 96];
           vmaxpd ymm4, ymm4, ymm2;
           vmovupd ymm2, [ecx + eax - 64];
           vmaxpd ymm3, ymm3, ymm2;
           vmovupd ymm2, [ecx + eax - 32];
           vmaxpd ymm4, ymm4, ymm2;
       add eax, 128;
       jle @addforxloop;

       vextractf128 xmm2, ymm3, 1;
       vmaxpd xmm2, xmm2, xmm3;
       vmaxpd xmm0, xmm0, xmm2;
       vextractf128 xmm2, ymm4, 1;
       vmaxpd xmm2, xmm2, xmm4;
       vmaxpd xmm0, xmm0, xmm2;

       @loopEnd:

       sub eax, 128;

       jz @nextLine;

       @addforxloop2:
           add eax, 16;
           jg @addforxloop2end;

           vmovupd xmm2, [ecx + eax - 16];
           vmaxpd xmm0, xmm0, xmm2;
       jmp @addforxloop2;

       @addforxloop2end:

       sub eax, 16;
       jz @nextLine;

       vmovsd xmm2, [ecx + eax];
       vmaxsd xmm0, xmm0, xmm2;

       @nextLine:

       // next line:
       add ecx, edi;

   // loop y end
   dec ebx;
   jnz @@addforyloop;

   // final max ->
   vmovhlps xmm1, xmm1, xmm0;
   vmaxsd xmm0, xmm0, xmm1;
   vmovsd Result, xmm0;

   // epilog - cleanup stack
   vzeroupper;

   pop ebx;
   pop edi;
{$IFDEF FPC}
end;
{$ENDIF}
end;

function AVXMatrixMinAligned(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double;
{$IFDEF FPC}
begin
{$ENDIF}
asm
   // prolog - simulate stack
   push ebx;
   push edi;

   // prepare pointers
   mov edi, LineWidth;
   mov ecx, mt;
   mov edx, width;
   imul edx, -8;

   sub ecx, edx;

   // init result
   lea eax, cMaxDouble;
   vbroadcastsd ymm0, [eax];
   vmovapd ymm3, ymm0;
   vmovapd ymm4, ymm0;

   // for y := 0 to height - 1:
   mov ebx, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov eax, edx;
       add eax, 128;
       jg @loopEnd;

       @addforxloop:
           // prefetch data...
           // prefetch [ecx + eax];

           // max:
           vminpd ymm3, ymm3, [ecx + eax - 128];
           vminpd ymm4, ymm4, [ecx + eax - 96];
           vminpd ymm3, ymm3, [ecx + eax - 64];
           vminpd ymm4, ymm4, [ecx + eax - 32];

       add eax, 128;
       jle @addforxloop;

       vextractf128 xmm2, ymm3, 1;
       vminpd xmm2, xmm2, xmm3;
       vminpd xmm0, xmm0, xmm2;
       vextractf128 xmm2, ymm4, 1;
       vminpd xmm2, xmm2, xmm4;
       vminpd xmm0, xmm0, xmm2;

       @loopEnd:

       sub eax, 128;

       jz @nextLine;

       @addforxloop2:
           add eax, 16;
           jg @addforxloop2end;

           vminpd xmm0, xmm0, [ecx + eax - 16];
       jmp @addforxloop2;

       @addforxloop2end:

       sub eax, 16;
       jz @nextLine;

       vmovsd xmm2, [ecx + eax];
       vminsd xmm0, xmm0, xmm2;

       @nextLine:

       // next line:
       add ecx, edi;

   // loop y end
   dec ebx;
   jnz @@addforyloop;

   // final max ->
   vmovhlps xmm1, xmm1, xmm0;
   vminsd xmm0, xmm0, xmm1;
   vmovsd Result, xmm0;

   // epilog - cleanup stack
   vzeroupper;

   pop ebx;
   pop edi;
{$IFDEF FPC}
end;
{$ENDIF}
end;

function AVXMatrixMinUnAligned(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double;
{$IFDEF FPC}
begin
{$ENDIF}
asm
   // prolog - simulate stack
   push ebx;
   push edi;

   // prepare pointers
   mov edi, LineWidth;
   mov ecx, mt;
   mov edx, width;
   imul edx, -8;

   sub ecx, edx;

   // init result
   lea eax, cMaxDouble;
   vbroadcastsd ymm0, [eax];
   vmovapd ymm3, ymm0;
   vmovapd ymm4, ymm0;

   // for y := 0 to height - 1:
   mov ebx, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov eax, edx;

       add eax, 128;
       jg @loopEnd;

       @addforxloop:
           // prefetch data...
           // prefetch [ecx + eax];

           // max:
           vmovupd ymm2, [ecx + eax - 128];
           vminpd ymm3, ymm3, ymm2;
           vmovupd ymm2, [ecx + eax - 96];
           vminpd ymm4, ymm4, ymm2;
           vmovupd ymm2, [ecx + eax - 64];
           vminpd ymm3, ymm3, ymm2;
           vmovupd ymm2, [ecx + eax - 32];
           vminpd ymm4, ymm4, ymm2;
       add eax, 128;
       jle @addforxloop;

       vextractf128 xmm2, ymm3, 1;
       vminpd xmm2, xmm2, xmm3;
       vminpd xmm0, xmm0, xmm2;
       vextractf128 xmm2, ymm4, 1;
       vminpd xmm2, xmm2, xmm4;
       vminpd xmm0, xmm0, xmm2;

       @loopEnd:

       sub eax, 128;

       jz @nextLine;

       @addforxloop2:
           add eax, 16;
           jg @addforxloop2end;

           vmovupd xmm2, [ecx + eax - 16];
           vminpd xmm0, xmm0, xmm2;
       jmp @addforxloop2;

       @addforxloop2end:

       sub eax, 16;
       jz @nextLine;

       vmovsd xmm2, [ecx + eax];
       vminsd xmm0, xmm0, xmm2;

       @nextLine:

       // next line:
       add ecx, edi;

   // loop y end
   dec ebx;
   jnz @@addforyloop;

   // final max ->
   vmovhlps xmm1, xmm1, xmm0;
   vminsd xmm0, xmm0, xmm1;
   vmovsd Result, xmm0;

   // epilog - cleanup stack
   vzeroupper;

   pop ebx;
   pop edi;
{$IFDEF FPC}
end;
{$ENDIF}
end;

{$ENDIF}

end.
