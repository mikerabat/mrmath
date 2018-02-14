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


unit AVXMatrixScaleOperations;

interface

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}
{$IFNDEF x64}

uses MatrixConst;

procedure AVXMatrixAddScaleAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);
procedure AVXMatrixAddScaleUnAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);

procedure AVXMatrixAddScaleAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);
procedure AVXMatrixAddScaleUnAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);

procedure AVXMatrixScaleAddAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);
procedure AVXMatrixScaleAddUnAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);

procedure AVXMatrixScaleAddAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);
procedure AVXMatrixScaleAddUnAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);

{$ENDIF}

implementation

{$IFNDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$ENDIF}

procedure AVXMatrixAddScaleAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);
{$IFDEF FPC}
begin
{$ENDIF}
asm
   // prolog - simulate stack
   push ebx;
   push edi;
   push esi;

   mov edx, LineWidth;

   //iters := -width*sizeof(double);
   mov edi, width;
   imul edi, -8;

   lea eax, dOffset;
   vbroadcastsd ymm3, [eax];
   lea eax, scale;
   vbroadcastsd ymm4, [eax];


   // helper registers for the mt1, mt2 and dest pointers
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

           // prefetchw data...
           // prefetchw [ecx + eax];

           // add mul
           vmovapd ymm0, [ecx + eax - 128];
           vaddpd ymm0, ymm0, ymm3;
           vmulpd ymm0, ymm0, ymm4;
           vmovapd [ecx + eax - 128], ymm0;

           vmovapd ymm2, [ecx + eax - 96];
           vaddpd ymm2, ymm2, ymm3;
           vmulpd ymm2, ymm2, ymm4;
           vmovapd [ecx + eax - 96], ymm2;

           vmovapd ymm0, [ecx + eax - 64];
           vaddpd ymm0, ymm0, ymm3;
           vmulpd ymm0, ymm0, ymm4;
           vmovapd [ecx + eax - 64], ymm0;

           vmovapd ymm2, [ecx + eax - 32];
           vaddpd ymm2, ymm2, ymm3;
           vmulpd ymm2, ymm2, ymm4;
           vmovapd [ecx + eax - 32], ymm2;

       jmp @addforxloop

       @loopEnd:

       sub eax, 128;

       jz @nextLine;

       @addforxloop2:
           vmovapd xmm0, [ecx + eax];
           vaddpd xmm0, xmm0, xmm3;
           vmulpd xmm0, xmm0, xmm4;

           vmovapd [ecx + eax], xmm0;
       add eax, 16;
       jnz @addforxloop2;

       @nextLine:

       // next line:
       add ecx, edx;

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

procedure AVXMatrixAddScaleUnAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);
{$IFDEF FPC}
begin
{$ENDIF}
asm
   // prolog - simulate stack
   push ebx;
   push edi;
   push esi;

   mov edx, LineWidth;

   //iters := -width*sizeof(double);
   mov edi, width;
   imul edi, -8;

   lea eax, dOffset;
   vbroadcastsd ymm3, [eax];
   lea eax, scale;
   vbroadcastsd ymm4, [eax];

   // helper registers for the mt1, mt2 and dest pointers
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

           // prefetchw data...
           // prefetchw [ecx + eax];

           // add mul
           vmovupd ymm0, [ecx + eax - 128];
           vaddpd ymm0, ymm0, ymm3;
           vmulpd ymm0, ymm0, ymm4;
           vmovupd [ecx + eax - 128], ymm0;

           vmovupd ymm2, [ecx + eax - 96];
           vaddpd ymm2, ymm2, ymm3;
           vmulpd ymm2, ymm2, ymm4;
           vmovupd [ecx + eax - 96], ymm2;

           vmovupd ymm0, [ecx + eax - 64];
           vaddpd ymm0, ymm0, ymm3;
           vmulpd ymm0, ymm0, ymm4;
           vmovupd [ecx + eax - 64], ymm0;

           vmovupd ymm2, [ecx + eax - 32];
           vaddpd ymm2, ymm2, ymm3;
           vmulpd ymm2, ymm2, ymm4;
           vmovupd [ecx + eax - 32], ymm2;

       jmp @addforxloop

       @loopEnd:

       sub eax, 128;

       jz @nextLine;

       @addforxloop2:
           vmovupd xmm0, [ecx + eax];
           vaddpd xmm0, xmm0, xmm3;
           vmulpd xmm0, xmm0, xmm4;

           vmovupd [ecx + eax], xmm0;
       add eax, 16;
       jnz @addforxloop2;

       @nextLine:

       // next line:
       add ecx, edx;

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


procedure AVXMatrixAddScaleAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);
{$IFDEF FPC}
begin
{$ENDIF}
asm
// prolog - simulate stack
   push ebx;
   push edi;
   push esi;

   mov edx, LineWidth;

   //iters := -width*sizeof(double);
   mov edi, width;
   dec edi;
   imul edi, -8;

   lea eax, dOffset;
   vbroadcastsd ymm3, [eax];
   lea eax, scale;
   vbroadcastsd ymm4, [eax];

   // helper registers for the mt1, mt2 and dest pointers
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

           // prefetchw data...
           // prefetchw [ecx + eax];

           // add mul
           vmovapd ymm0, [ecx + eax - 128];
           vaddpd ymm0, ymm0, ymm3;
           vmulpd ymm0, ymm0, ymm4;
           vmovapd [ecx + eax - 128], ymm0;

           vmovapd ymm2, [ecx + eax - 96];
           vaddpd ymm2, ymm2, ymm3;
           vmulpd ymm2, ymm2, ymm4;
           vmovapd [ecx + eax - 96], ymm2;

           vmovapd ymm0, [ecx + eax - 64];
           vaddpd ymm0, ymm0, ymm3;
           vmulpd ymm0, ymm0, ymm4;
           vmovapd [ecx + eax - 64], ymm0;

           vmovapd ymm2, [ecx + eax - 32];
           vaddpd ymm2, ymm2, ymm3;
           vmulpd ymm2, ymm2, ymm4;
           vmovapd [ecx + eax - 32], ymm2;

       jmp @addforxloop

       @loopEnd:

       sub eax, 128;

       jz @nextLine;

       @addforxloop2:
           vmovapd xmm0, [ecx + eax];
           vaddpd xmm0, xmm0, xmm3;
           vmulpd xmm0, xmm0, xmm4;

           vmovapd [ecx + eax], xmm0;
       add eax, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last column:
       vmovsd xmm0, [ecx];
       vaddsd xmm0, xmm0, xmm3;
       vmulsd xmm0, xmm0, xmm4;
       vmovsd [ecx], xmm0;

       // next line:
       add ecx, edx;

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

procedure AVXMatrixAddScaleUnAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);
{$IFDEF FPC}
begin
{$ENDIF}
asm
   // prolog - simulate stack
   push ebx;
   push edi;
   push esi;

   mov edx, LineWidth;

   //iters := -width*sizeof(double);
   mov edi, width;
   dec edi;
   imul edi, -8;

   lea eax, dOffset;
   vbroadcastsd ymm3, [eax];
   lea eax, scale;
   vbroadcastsd ymm4, [eax];

   // helper registers for the mt1, mt2 and dest pointers
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

           // prefetchw data...
           // prefetchw [ecx + eax];

           // add mul
           vmovupd ymm0, [ecx + eax - 128];
           vaddpd ymm0, ymm0, ymm3;
           vmulpd ymm0, ymm0, ymm4;
           vmovupd [ecx + eax - 128], ymm0;

           vmovupd ymm2, [ecx + eax - 96];
           vaddpd ymm2, ymm2, ymm3;
           vmulpd ymm2, ymm2, ymm4;
           vmovupd [ecx + eax - 96], ymm2;

           vmovupd ymm0, [ecx + eax - 64];
           vaddpd ymm0, ymm0, ymm3;
           vmulpd ymm0, ymm0, ymm4;
           vmovupd [ecx + eax - 64], ymm0;

           vmovupd ymm2, [ecx + eax - 32];
           vaddpd ymm2, ymm2, ymm3;
           vmulpd ymm2, ymm2, ymm4;
           vmovupd [ecx + eax - 32], ymm2;

       jmp @addforxloop

       @loopEnd:

       sub eax, 128;

       jz @nextLine;

       @addforxloop2:
           vmovupd xmm0, [ecx + eax];
           vaddpd xmm0, xmm0, xmm3;
           vmulpd xmm0, xmm0, xmm4;

           vmovupd [ecx + eax], xmm0;
       add eax, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last column:
       vmovsd xmm0, [ecx];
       vaddsd xmm0, xmm0, xmm3;
       vmulsd xmm0, xmm0, xmm4;
       vmovsd [ecx], xmm0;

       // next line:
       add ecx, edx;

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


procedure AVXMatrixScaleAddAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);
{$IFDEF FPC}
begin
{$ENDIF}
asm
   // prolog - simulate stack
   push ebx;
   push edi;
   push esi;

   mov edx, LineWidth;

   //iters := -width*sizeof(double);
   mov edi, width;
   imul edi, -8;

   lea eax, dOffset;
   vbroadcastsd ymm3, [eax];
   lea eax, scale;
   vbroadcastsd ymm4, [eax];

   // helper registers for the mt1, mt2 and dest pointers
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

           // prefetchw data...
           // prefetchw [ecx + eax];

           // addition:
           vmovapd ymm0, [ecx + eax - 128];
           vmulpd ymm0, ymm0, ymm4;
           vaddpd ymm0, ymm0, ymm3;
           vmovapd [ecx + eax - 128], ymm0;

           vmovapd ymm2, [ecx + eax - 96];
           vmulpd ymm2, ymm2, ymm4;
           vaddpd ymm2, ymm2, ymm3;
           vmovapd [ecx + eax - 96], ymm2;

           vmovapd ymm0, [ecx + eax - 64];
           vmulpd ymm0, ymm0, ymm4;
           vaddpd ymm0, ymm0, ymm3;
           vmovapd [ecx + eax - 64], ymm0;

           vmovapd ymm2, [ecx + eax - 32];
           vmulpd ymm2, ymm2, ymm4;
           vaddpd ymm2, ymm2, ymm3;
           vmovapd [ecx + eax - 32], ymm2;

       jmp @addforxloop

       @loopEnd:

       sub eax, 128;

       jz @nextLine;

       @addforxloop2:
           vmovapd xmm0, [ecx + eax];
           vmulpd xmm0, xmm0, xmm4;
           vaddpd xmm0, xmm0, xmm3;

           vmovapd [ecx + eax], xmm0;
       add eax, 16;
       jnz @addforxloop2;

       @nextLine:

       // next line:
       add ecx, edx;

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

procedure AVXMatrixScaleAddUnAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);
{$IFDEF FPC}
begin
{$ENDIF}
asm
   // prolog - simulate stack
   push ebx;
   push edi;
   push esi;

   mov edx, LineWidth;

   //iters := -width*sizeof(double);
   mov edi, width;
   imul edi, -8;

   lea eax, dOffset;
   vbroadcastsd ymm3, [eax];
   lea eax, scale;
   vbroadcastsd ymm4, [eax];

   // helper registers for the mt1, mt2 and dest pointers
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

           // prefetchw data...
           // prefetchw [ecx + eax];

           // addition:
           vmovupd ymm0, [ecx + eax - 128];
           vmulpd ymm0, ymm0, ymm4;
           vaddpd ymm0, ymm0, ymm3;
           vmovupd [ecx + eax - 128], ymm0;

           vmovupd ymm2, [ecx + eax - 96];
           vmulpd ymm2, ymm2, ymm4;
           vaddpd ymm2, ymm2, ymm3;
           vmovupd [ecx + eax - 96], ymm2;

           vmovupd ymm0, [ecx + eax - 64];
           vmulpd ymm0, ymm0, ymm4;
           vaddpd ymm0, ymm0, ymm3;
           vmovupd [ecx + eax - 64], ymm0;

           vmovupd ymm2, [ecx + eax - 32];
           vmulpd ymm2, ymm2, ymm4;
           vaddpd ymm2, ymm2, ymm3;
           vmovupd [ecx + eax - 32], ymm2;

       jmp @addforxloop

       @loopEnd:

       sub eax, 128;

       jz @nextLine;

       @addforxloop2:
           vmovupd xmm0, [ecx + eax];
           vmulpd xmm0, xmm0, xmm4;
           vaddpd xmm0, xmm0, xmm3;

           vmovupd [ecx + eax], xmm0;
       add eax, 16;
       jnz @addforxloop2;

       @nextLine:

       // next line:
       add ecx, edx;

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

procedure AVXMatrixScaleAddAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);
{$IFDEF FPC}
begin
{$ENDIF}
asm
   // prolog - simulate stack
   push ebx;
   push edi;
   push esi;

   mov edx, LineWidth;

   //iters := -width*sizeof(double);
   mov edi, width;
   dec edi;
   imul edi, -8;

   lea eax, dOffset;
   vbroadcastsd ymm3, [eax];
   lea eax, scale;
   vbroadcastsd ymm4, [eax];

   // helper registers for the mt1, mt2 and dest pointers
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

           // prefetchw data...
           // prefetchw [ecx + eax];

           // addition:
           vmovapd ymm0, [ecx + eax - 128];
           vmulpd ymm0, ymm0, ymm4;
           vaddpd ymm0, ymm0, ymm3;
           vmovapd [ecx + eax - 128], ymm0;

           vmovapd ymm2, [ecx + eax - 96];
           vmulpd ymm2, ymm2, ymm4;
           vaddpd ymm2, ymm2, ymm3;
           vmovapd [ecx + eax - 96], ymm2;

           vmovapd ymm0, [ecx + eax - 64];
           vmulpd ymm0, ymm0, ymm4;
           vaddpd ymm0, ymm0, ymm3;
           vmovapd [ecx + eax - 64], ymm0;

           vmovapd ymm2, [ecx + eax - 32];
           vmulpd ymm2, ymm2, ymm4;
           vaddpd ymm2, ymm2, ymm3;
           vmovapd [ecx + eax - 32], ymm2;


       jmp @addforxloop

       @loopEnd:

       sub eax, 128;

       jz @nextLine;

       @addforxloop2:
           vmovapd xmm0, [ecx + eax];
           vmulpd xmm0, xmm0, xmm4;
           vaddpd xmm0, xmm0, xmm3;

           vmovapd [ecx + eax], xmm0;
       add eax, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last column:
       vmovsd xmm0, [ecx];
       vmulsd xmm0, xmm0, xmm4;
       vaddsd xmm0, xmm0, xmm3;
       vmovsd [ecx], xmm0;

       // next line:
       add ecx, edx;

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

procedure AVXMatrixScaleAddUnAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);
{$IFDEF FPC}
begin
{$ENDIF}
asm
   // prolog - simulate stack
   push ebx;
   push edi;
   push esi;

   mov edx, LineWidth;

   //iters := -width*sizeof(double);
   mov edi, width;
   dec edi;
   imul edi, -8;

   lea eax, dOffset;
   vbroadcastsd ymm3, [eax];
   lea eax, scale;
   vbroadcastsd ymm4, [eax];

   // helper registers for the mt1, mt2 and dest pointers
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

           // prefetchw data...
           // prefetchw [ecx + eax];

           // addition:
           vmovupd ymm0, [ecx + eax - 128];
           vmulpd ymm0, ymm0, ymm4;
           vaddpd ymm0, ymm0, ymm3;
           vmovupd [ecx + eax - 128], ymm0;

           vmovupd ymm2, [ecx + eax - 96];
           vmulpd ymm2, ymm2, ymm4;
           vaddpd ymm2, ymm2, ymm3;
           vmovupd [ecx + eax - 96], ymm2;

           vmovupd ymm0, [ecx + eax - 64];
           vmulpd ymm0, ymm0, ymm4;
           vaddpd ymm0, ymm0, ymm3;
           vmovupd [ecx + eax - 64], ymm0;

           vmovupd ymm2, [ecx + eax - 32];
           vmulpd ymm2, ymm2, ymm4;
           vaddpd ymm2, ymm2, ymm3;
           vmovupd [ecx + eax - 32], ymm2;

       jmp @addforxloop

       @loopEnd:

       sub eax, 128;

       jz @nextLine;

       @addforxloop2:
           vmovupd xmm0, [ecx + eax];
           vmulpd xmm0, xmm0, xmm4;
           vaddpd xmm0, xmm0, xmm3;

           vmovupd [ecx + eax], xmm0;
       add eax, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last column:
       vmovsd xmm0, [ecx];
       vmulsd xmm0, xmm0, xmm4;
       vaddsd xmm0, xmm0, xmm3;
       vmovsd [ecx], xmm0;

       // next line:
       add ecx, edx;

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
{$ENDIF}

end.
