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


unit AVXMatrixSqrtOperations;

// #####################################################
// #### SQRT opertaion applied to every element in a matrix
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

procedure AVXMatrixSQRTAligned(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt);
procedure AVXMatrixSQRTUnAligned(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt);

{$ENDIF}

implementation

{$IFNDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$ENDIF}

procedure AVXMatrixSQRTAligned(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt);
{$IFDEF FPC}
begin
{$ENDIF}
asm
   push edi;
   push esi;

   //iters := -width*sizeof(double);
   mov edi, width;
   imul edi, -8;

   // helper registers dest pointers
   mov edx, LineWidth;
   mov ecx, dest;
   sub ecx, edi;

   // for y := 0 to height - 1:
   mov esi, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov eax, edi;
       @addforxloop:
           add eax, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetchw [ecx + eax];

           // elementwise sqrt
           vsqrtpd ymm0, [ecx + eax - 128];
           vmovapd [ecx + eax - 128], ymm0;

           vsqrtpd ymm1, [ecx + eax - 96];
           vmovapd [ecx + eax - 96], ymm1;

           vsqrtpd ymm0, [ecx + eax - 64];
           vmovapd [ecx + eax - 64], ymm0;

           vsqrtpd ymm1, [ecx + eax - 32];
           vmovapd [ecx + eax - 32], ymm1;

       jmp @addforxloop

       @loopEnd:

       sub eax, 128;

       jz @nextLine;

       @addforxloop2:
           add eax, 16;
           jg @addforxloop2end;

           vsqrtpd xmm0, [ecx + eax - 16];
           vmovapd [ecx + eax - 16], xmm0;
       jmp @addforxloop2;

       @addforxloop2end:

       sub eax, 16;

       jz @nextLine;

       vmovsd xmm0, [ecx + eax];
       vsqrtsd xmm0, xmm0, xmm0;
       vmovsd [ecx + eax], xmm0;

       @nextLine:

       // next line:
       add ecx, edx;

   // loop y end
   dec esi;
   jnz @@addforyloop;

   vzeroupper;
   pop esi;
   pop edi;
{$IFDEF FPC}
end;
{$ENDIF}
end;

procedure AVXMatrixSQRTUnAligned(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt);
{$IFDEF FPC}
begin
{$ENDIF}
asm
   push edi;
   push esi

   //iters := -width*sizeof(double);
   mov edi, width;
   imul edi, -8;

   // helper registers dest pointers
   mov ecx, dest;
   sub ecx, edi;
   mov edx, LineWidth;

   // for y := 0 to height - 1:
   mov esi, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov eax, edi;
       @addforxloop:
           add eax, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetchw [ecx + eax];

           // elementwise sqrt
           vmovupd ymm2, [ecx + eax - 128];
           vsqrtpd ymm0, ymm2;
           vmovupd [ecx + eax - 128], ymm0;

           vmovupd ymm3, [ecx + eax - 96];
           vsqrtpd ymm1, ymm3;
           vmovupd [ecx + eax - 96], ymm1;

           vmovupd ymm2, [ecx + eax - 64];
           vsqrtpd ymm0, ymm2;
           vmovupd [ecx + eax - 64], ymm0;

           vmovupd ymm3, [ecx + eax - 32];
           vsqrtpd ymm1, ymm3;
           vmovupd [ecx + eax - 32], ymm1;

       jmp @addforxloop

       @loopEnd:

       sub eax, 128;

       jz @nextLine;

       @addforxloop2:
           add eax, 16;
           jg @addforxloop2end;

           vmovupd xmm1, [ecx + eax - 16];
           vsqrtpd xmm0, xmm1;
           vmovupd [ecx + eax - 16], xmm0;
       jmp @addforxloop2;

       @addforxloop2end:

       sub eax, 16;

       jz @nextLine;

       vmovsd xmm0, [ecx + eax];
       vsqrtsd xmm0, xmm0, xmm0;
       vmovsd [ecx + eax], xmm0;

       @nextLine:

       // next line:
       add ecx, edx;

   // loop y end
   dec esi;
   jnz @@addforyloop;

   vzeroupper;
   pop esi;
   pop edi;
{$IFDEF FPC}
end;
{$ENDIF}
end;

{$ENDIF}

end.
