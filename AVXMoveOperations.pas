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


unit AVXMoveOperations;

interface

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}
{$IFNDEF x64}

uses MatrixConst;

procedure AVXMatrixCopyAligned(Dest : PDouble; const destLineWidth : TASMNativeInt; src : PDouble; const srcLineWidth : TASMNativeInt; Width, Height : TASMNativeInt);
procedure AVXMatrixCopyUnAligned(Dest : PDouble; const destLineWidth : TASMNativeInt; src : PDouble; const srcLineWidth : TASMNativeInt; Width, Height : TASMNativeInt);

procedure AVXRowSwapAligned(A, B : PDouble; width : TASMNativeInt);
procedure AVXRowSwapUnAligned(A, B : PDouble; width : TASMNativeInt);

procedure AVXInitMemAligned(A : PDouble; NumBytes : TASMNativeInt; const Value : double);

{$ENDIF}

implementation

{$IFNDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$ENDIF}

// uses non temporal moves so the cache is not poisned
// rcx = A, rdx = NumBytes;
procedure AVXInitMemAligned(A : PDouble; NumBytes : TASMNativeInt; const Value : double);
{$IFDEF FPC}
begin
{$ENDIF}
asm
   lea eax, Value;
   vbroadcastsd ymm1, [eax];

   mov edx, NumBytes;
   imul edx, -1;
   mov ecx, A;
   sub ecx, edx;

   @@loopUnrolled:
      add edx, 128;
      jg @@loopUnrolledEnd;

      vmovntdq [ecx + edx - 128], ymm1;
      vmovntdq [ecx + edx - 96], ymm1;
      vmovntdq [ecx + edx - 64], ymm1;
      vmovntdq [ecx + edx - 32], ymm1;
   jmp @@loopUnrolled;

   @@loopUnrolledEnd:

   sub edx, 256;
   jz @@exitProc;

   add edx, 32;
   jg @@beginloop3;

   @@loop2:
      vmovntdq [ecx + edx - 32], xmm1;
      vmovntdq [ecx + edx - 16], xmm1;
   add edx, 32;
   jl @@loop2;

   @@beginloop3:
   sub edx, 32;
   jz @@exitProc;

   @@loop3:
      vmovsd [ecx + edx], xmm1;
      add edx, 8;
   jnz @@loop3;

   @@exitProc:

   vzeroupper;
end;
{$IFDEF FPC}
end;
{$ENDIF}

procedure AVXRowSwapAligned(A, B : PDouble; width : TASMNativeInt);
{$IFDEF FPC}
begin
{$ENDIF}
asm
   // prolog
   push ebx;

   // prepare
   mov ecx, A;
   mov edx, B;

   mov ebx, width;
   imul ebx, -8;

   sub ecx, ebx;
   sub edx, ebx;

   @unrolloop:
     add ebx, 128;
     jg @unrolloopend;

     // prefetch data...
     // prefetchw [ecx + r10];
     // prefetchw [edx + r10];

     vmovdqa ymm0, [ecx + ebx - 128];
     vmovdqa ymm1, [edx + ebx - 128];

     vmovdqa [ecx + ebx - 128], ymm1;
     vmovdqa [edx + ebx - 128], ymm0;

     vmovdqa ymm2, [ecx + ebx - 96];
     vmovdqa ymm3, [edx + ebx - 96];

     vmovdqa [ecx + ebx - 96], ymm3;
     vmovdqa [edx + ebx - 96], ymm2;

     vmovdqa ymm0, [ecx + ebx - 64];
     vmovdqa ymm1, [edx + ebx - 64];

     vmovdqa [ecx + ebx - 64], ymm1;
     vmovdqa [edx + ebx - 64], ymm0;

     vmovdqa ymm2, [ecx + ebx - 32];
     vmovdqa ymm3, [edx + ebx - 32];

     vmovdqa [ecx + ebx - 32], ymm3;
     vmovdqa [edx + ebx - 32], ymm2;
   jmp @unrolloop;
   @unrolloopend:

   sub ebx, 128;
   jz @endfunc;

   @loop2:
     add ebx, 16;
     jg @loop2End;

     vmovdqa xmm0, [ecx + ebx - 16];
     vmovdqa xmm1, [edx + ebx - 16];

     vmovdqa [ecx + ebx - 16], xmm1;
     vmovdqa [edx + ebx - 16], xmm0;
   jmp @loop2;

   @loop2End:
   sub ebx, 16;

   jz @endfunc;

   vmovsd xmm0, [ecx + ebx];
   vmovsd xmm1, [edx + ebx];

   vmovsd [ecx + ebx], xmm1;
   vmovsd [edx + ebx], xmm0;

   @endfunc:

   vzeroupper;
   pop ebx;
{$IFDEF FPC}
end;
{$ENDIF}
end;

procedure AVXRowSwapUnAligned(A, B : PDouble; width : TASMNativeInt);
{$IFDEF FPC}
begin
{$ENDIF}
asm
   // prolog
   push ebx;

   // prepare
   mov ecx, A;
   mov edx, B;

   mov ebx, width;
   imul ebx, -8;

   sub ecx, ebx;
   sub edx, ebx;

   @unrolloop:
     add ebx, 128;
     jg @unrolloopend;

     // prefetch data...
     // prefetchw [ecx + r10];
     // prefetchw [edx + r10];

     vmovdqu ymm0, [ecx + ebx - 128];
     vmovdqu ymm1, [edx + ebx - 128];

     vmovdqu [ecx + ebx - 128], ymm1;
     vmovdqu [edx + ebx - 128], ymm0;

     vmovdqu ymm2, [ecx + ebx - 96];
     vmovdqu ymm3, [edx + ebx - 96];

     vmovdqu [ecx + ebx - 96], ymm3;
     vmovdqu [edx + ebx - 96], ymm2;

     vmovdqu ymm0, [ecx + ebx - 64];
     vmovdqu ymm1, [edx + ebx - 64];

     vmovdqu [ecx + ebx - 64], ymm1;
     vmovdqu [edx + ebx - 64], ymm0;

     vmovdqu ymm2, [ecx + ebx - 32];
     vmovdqu ymm3, [edx + ebx - 32];

     vmovdqu [ecx + ebx - 32], ymm3;
     vmovdqu [edx + ebx - 32], ymm2;
   jmp @unrolloop;
   @unrolloopend:

   sub ebx, 128;
   jz @endfunc;

   @loop2:
     add ebx, 16;
     jg @loop2End;

     vmovdqu xmm0, [ecx + ebx - 16];
     vmovdqu xmm1, [edx + ebx - 16];

     vmovdqu [ecx + ebx - 16], xmm1;
     vmovdqu [edx + ebx - 16], xmm0;
   jmp @loop2;

   @loop2End:
   sub ebx, 16;

   jz @endfunc;

   vmovsd xmm0, [ecx + ebx];
   vmovsd xmm1, [edx + ebx];

   vmovsd [ecx + ebx], xmm1;
   vmovsd [edx + ebx], xmm0;

   @endfunc:

   vzeroupper;
   pop ebx;
{$IFDEF FPC}
end;
{$ENDIF}
end;

procedure AVXMatrixCopyAligned(Dest : PDouble; const destLineWidth : TASMNativeInt; src : PDouble; const srcLineWidth : TASMNativeInt; Width, Height : TASMNativeInt);
{$IFDEF FPC}
begin
{$ENDIF}
asm
   push ebx;
   push edi;
   push esi;

   //iters := -width*sizeof(double);
   mov esi, width;
   imul esi, -8;

   // helper registers for the src and dest pointers
   mov ebx, src;
   mov ecx, dest;
   sub ebx, esi;
   sub ecx, esi;

   // for y := 0 to height - 1:
   mov edi, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov eax, esi;
       @addforxloop:
           add eax, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [ebx + eax];
           // prefetchw [ecx + eax];

           // move:
           vmovdqa ymm0, [ebx + eax - 128];
           vmovdqa [ecx + eax - 128], ymm0;

           vmovdqa ymm1, [ebx + eax - 96];
           vmovdqa [ecx + eax - 96], ymm1;

           vmovdqa ymm2, [ebx + eax - 64];
           vmovdqa [ecx + eax - 64], ymm2;

           vmovdqa ymm3, [ebx + eax - 32];
           vmovdqa [ecx + eax - 32], ymm3;

       jmp @addforxloop

       @loopEnd:

       sub eax, 128;

       jz @nextLine;

       @addforxloop2:
          add eax, 16;
          jg @loopEnd2;

          vmovdqa xmm0, [ebx + eax - 16];
          vmovdqa [ecx + eax - 16], xmm0;
       jmp @addforxloop2;

       @loopEnd2:
       sub eax, 16;

       jz @nextLine;

       // last element
       vmovsd xmm0, [ebx + eax];
       vmovsd [ecx + eax], xmm0;

       @nextLine:

       // next line:
       add ebx, srcLineWidth;
       add ecx, destLineWidth;

   // loop y end
   dec edi;
   jnz @@addforyloop;

   // epilog
   vzeroupper;
   pop esi;
   pop edi;
   pop ebx;
end;
{$IFDEF FPC}
end;
{$ENDIF}

procedure AVXMatrixCopyUnAligned(Dest : PDouble; const destLineWidth : TASMNativeInt; src : PDouble; const srcLineWidth : TASMNativeInt; Width, Height : TASMNativeInt);
{$IFDEF FPC}
begin
{$ENDIF}
asm
   push ebx;
   push edi;
   push esi;

   // note: ecx = dest, edx = destLineWidth, ebx = src, R9 = srcLineWidth
   //iters := -width*sizeof(double);
   mov esi, width;
   imul esi, -8;

   // helper registers for the src and dest pointers
   mov ebx, src;
   mov ecx, dest;
   sub ebx, esi;
   sub ecx, esi;

   // for y := 0 to height - 1:
   mov edi, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov eax, esi;
       @addforxloop:
           add eax, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [ebx + eax];
           // prefetchw [ecx + eax];

           // move:
           vmovdqu ymm0, [ebx + eax - 128];
           vmovdqu [ecx + eax - 128], ymm0;

           vmovdqu ymm1, [ebx + eax - 96];
           vmovdqu [ecx + eax - 96], ymm1;

           vmovdqu ymm2, [ebx + eax - 64];
           vmovdqu [ecx + eax - 64], ymm2;

           vmovdqu ymm3, [ebx + eax - 32];
           vmovdqu [ecx + eax - 32], ymm3;

       jmp @addforxloop

       @loopEnd:

       sub eax, 128;

       jz @nextLine;

       @addforxloop2:
          add eax, 16;
          jg @loopEnd2;

          vmovdqu xmm0, [ebx + eax - 16];
          vmovdqu [ecx + eax - 16], xmm0;
       jmp @addforxloop2;

       @loopEnd2:
       sub eax, 16;

       jz @nextLine;

       // last element
       vmovsd xmm0, [ebx + eax];
       vmovsd [ecx + eax], xmm0;

       @nextLine:

       // next line:
       add ebx, srcLineWidth;
       add ecx, destLineWidth;

   // loop y end
   dec edi;
   jnz @@addforyloop;

   // epilog
   vzeroupper;
   pop esi;
   pop edi;
   pop ebx;
end;
{$IFDEF FPC}
end;
{$ENDIF}

{$ENDIF}

end.
