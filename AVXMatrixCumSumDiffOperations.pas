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

unit AVXMatrixCumSumDiffOperations;

interface


{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}
{$IFNDEF x64}

uses MatrixConst;

procedure AVXMatrixCumulativeSumRow(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);

procedure AVXMatrixCumulativeSumColumnUnaligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
procedure AVXMatrixCumulativeSumColumnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);

procedure AVXMatrixDifferentiateRow(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);

procedure AVXMatrixDifferentiateColumnUnaligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
procedure AVXMatrixDifferentiateColumnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);

{$ENDIF}

implementation

{$IFNDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$ENDIF}

// ##################################################
// #### Cumulative sum
// ##################################################

procedure AVXMatrixCumulativeSumRow(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
{$IFDEF FPC}
begin
{$ENDIF}
asm
   // prolog - maintain stack
   push ebx;
   push edi;
   push esi;

   // if (width <= 0) or (height <= 0) then exit;
   mov edi, width;
   cmp edi, 0;
   jle @@exitproc;
   mov eax, height;
   cmp eax, 0;
   jle @@exitproc;

   // iter := -width*sizeof(Double)
   imul edi, -8;

   mov ebx, destLineWidth;

   // prepare counters
   mov ecx, dest;
   mov edx, src;
   sub ecx, edi;
   sub edx, edi;

   mov esi, height;
   @@foryloop:
      mov eax, edi;

      vxorpd xmm0, xmm0, xmm0;
      @@forxloop:
         vmovsd xmm1, [edx + eax];
         vaddsd xmm0, xmm0, xmm1;
         vmovsd [ecx + eax], xmm0;
      add eax, 8;
      jnz @@forxloop;

      add ecx, ebx;
      add edx, srcLineWidth;
   dec esi;
   jnz @@foryloop;
   @@exitProc:

   // epilog - stack cleanup
   vzeroupper;

   pop esi;
   pop edi;
   pop ebx;
end;
{$IFDEF FPC}
end;
{$ENDIF}

procedure AVXMatrixCumulativeSumColumnUnaligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
{$IFDEF FPC}
begin
{$ENDIF}
asm
   // prolog - maintain stack
   push edi;
   push esi;
   push ebx;

   // if (width <= 1) or (height <= 0) then exit;
   mov eax, height;
   cmp eax, 0;

   jle @@exitproc;

   mov ecx, dest;
   mov edx, src;

   mov ebx, width;
   sub ebx, 4;
   jl @@lastColumns;

   @@forxloop:
       mov eax, height;
       vxorpd ymm0, ymm0, ymm0;
       xor edi, edi;
       xor esi, esi;

       // 4 values at once
       @@foryloop:
           vmovupd ymm1, [edx + edi];
           vaddpd ymm0, ymm0, ymm1;
           vmovupd [ecx + esi], ymm0;

           add edi, srcLineWidth;
           add esi, destLineWidth;
       dec eax;
       jnz @@foryloop;

       add edx, 32;
       add ecx, 32;
   sub ebx, 4;
   jge @@forxloop;

   @@lastColumns:
   add ebx, 4;
   jz @@exitProc;

   @@forxloopshort:
       mov eax, height;
       vxorpd xmm0, xmm0, xmm0;
       xor edi, edi;
       xor esi, esi;

       // 4 values at once
       @@foryloopshort:
           vmovsd xmm1, [edx + edi];
           vaddsd xmm0, xmm0, xmm1;
           vmovsd [ecx + esi], xmm0;

           add edi, srcLineWidth;
           add esi, destLineWidth;
       dec eax;
       jnz @@foryloopshort;

       add edx, 8;
       add ecx, 8;
   sub ebx, 1;
   jg @@forxloopshort;

   @@exitProc:

   // epilog - stack cleanup
   vzeroupper;

   pop ebx;
   pop esi;
   pop edi;
end;
{$IFDEF FPC}
end;
{$ENDIF}

procedure AVXMatrixCumulativeSumColumnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
{$IFDEF FPC}
begin
{$ENDIF}
asm
   // prolog - maintain stack
   push edi;
   push esi;
   push ebx;

   // if (width <= 1) or (height <= 0) then exit;
   mov eax, height;
   cmp eax, 0;

   jle @@exitproc;

   mov ecx, dest;
   mov edx, src;

   mov ebx, width;
   sub ebx, 4;
   jl @@lastColumns;

   @@forxloop:
       mov eax, height;
       vxorpd ymm0, ymm0, ymm0;
       xor edi, edi;
       xor esi, esi;

       // 4 values at once
       @@foryloop:
           vmovapd ymm1, [edx + edi];
           vaddpd ymm0, ymm0, ymm1;
           vmovapd [ecx + esi], ymm0;

           add edi, srcLineWidth;
           add esi, destLineWidth;
       dec eax;
       jnz @@foryloop;

       add edx, 32;
       add ecx, 32;
   sub ebx, 4;
   jge @@forxloop;

   @@lastColumns:
   add ebx, 4;
   jz @@exitProc;

   @@forxloopshort:
       mov eax, height;
       vxorpd xmm0, xmm0, xmm0;
       xor edi, edi;
       xor esi, esi;

       // 4 values at once
       @@foryloopshort:
           vmovsd xmm1, [edx + edi];
           vaddsd xmm0, xmm0, xmm1;
           vmovsd [ecx + esi], xmm0;

           add edi, srcLineWidth;
           add esi, destLineWidth;
       dec eax;
       jnz @@foryloopshort;

       add edx, 8;
       add ecx, 8;
   sub ebx, 1;
   jg @@forxloopshort;

   @@exitProc:

   // epilog - stack cleanup
   vzeroupper;

   pop ebx;
   pop esi;
   pop edi;
end;
{$IFDEF FPC}
end;
{$ENDIF}

// ###############################################
// #### Differentiate
// ###############################################

procedure AVXMatrixDifferentiateRow(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
{$IFDEF FPC}
begin
{$ENDIF}
asm
   // prolog - maintain stack
   push ebx;
   push edi;
   push esi;

   // if (width <= 1) or (height <= 0) then exit;
   mov ebx, width;
   cmp ebx, 1;
   jle @@exitproc;
   mov esi, height;
   cmp esi, 0;
   jle @@exitproc;

   // iter := -width*sizeof(Double)
   imul ebx, -8;

   mov edi, srcLineWidth;

   // prepare counters
   mov ecx, dest;
   mov edx, src;
   sub ecx, ebx;
   sub edx, ebx;

   add ebx, 8;
   @@foryloop:
       mov eax, ebx;
       vmovsd xmm1, [edx + eax - 8];
       @@forxloop:
           vmovsd xmm0, [edx + eax];
           vsubsd xmm2, xmm0, xmm1;
           vmovsd [ecx + eax - 8], xmm2;
           vmovapd xmm1, xmm0;
       add eax, 8;
       jnz @@forxloop;

       add ecx, destLineWidth;
       add edx, edi;
   dec esi;
   jnz @@foryloop;

   @@exitProc:
   // epilog - stack cleanup
   vzeroupper;

   pop esi;
   pop edi;
   pop ebx;
end;
{$IFDEF FPC}
end;
{$ENDIF}

procedure AVXMatrixDifferentiateColumnUnaligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
{$IFDEF FPC}
begin
{$ENDIF}
asm
   // prolog - maintain stack
   push ebx;
   push edi;
   push esi;

   mov ecx, dest;
   mov edx, src;

   // if (width <= 1) or (height <= 0) then exit;
   mov eax, height;
   cmp eax, 1;
   jle @@exitproc;

   mov ebx, width;
   cmp ebx, 1;
   jle @@exitproc;

   dec height;
   sub ebx, 4;
   jl @@forxloopend;

   @@forxloop:
       mov eax, height;
       vxorpd ymm0, ymm0, ymm0;
       mov edi, srcLineWidth;
       xor esi, esi;

       vmovupd ymm0, [edx];

       // 4 values at once
       @@foryloop:
           vmovupd ymm1, [edx + edi];
           vsubpd ymm2, ymm1, ymm0;
           vmovupd [ecx + esi], ymm2;

           vmovapd ymm0, ymm1;

           add edi, srcLineWidth;
           add esi, destLineWidth;
       dec eax;
       jnz @@foryloop;

       add edx, 32;
       add ecx, 32;
   sub ebx, 4;
   jge @@forxloop;

   @@forxloopend:
   add ebx, 4;
   jz @@exitProc;

   // #####################################
   // #### last < 4 columns
   @@forxloopshort:
      mov eax, height;
      vxorpd xmm0, xmm0, xmm0;
      mov edi, srcLineWidth;
      xor esi, esi;

      vmovsd xmm0, [edx];

      // one column value:
      @@foryloopshort:
          vmovsd xmm1, [edx + edi];
          vsubsd xmm2, xmm1, xmm0;
          vmovsd [ecx + esi], xmm2;

          vmovsd xmm0, xmm0, xmm1;

          add edi, srcLineWidth;
          add esi, destLineWidth;
      dec eax;
      jnz @@foryloopshort;

      add edx, 8;
      add ecx, 8;

   dec ebx;
   jnz @@forxloopshort;

   @@exitProc:

   // epilog - stack cleanup
   pop esi;
   pop edi;
   pop ebx;
end;
{$IFDEF FPC}
end;
{$ENDIF}

procedure AVXMatrixDifferentiateColumnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
{$IFDEF FPC}
begin
{$ENDIF}
asm
   // prolog - maintain stack
   push ebx;
   push edi;
   push esi;

   mov ecx, dest;
   mov edx, src;

   // if (width <= 1) or (height <= 0) then exit;
   mov eax, height;
   cmp eax, 1;
   jle @@exitproc;

   mov ebx, width;
   cmp ebx, 1;
   jle @@exitproc;

   dec height;
   sub ebx, 4;
   jl @@forxloopend;

   @@forxloop:
       mov eax, height;
       vxorpd ymm0, ymm0, ymm0;
       mov edi, srcLineWidth;
       xor esi, esi;

       vmovupd ymm0, [edx];

       // 4 values at once
       @@foryloop:
           vmovapd ymm1, [edx + edi];
           vsubpd ymm2, ymm1, ymm0;
           vmovapd [ecx + esi], ymm2;

           vmovapd ymm0, ymm1;

           add edi, srcLineWidth;
           add esi, destLineWidth;
       dec eax;
       jnz @@foryloop;

       add edx, 32;
       add ecx, 32;
   sub ebx, 4;
   jge @@forxloop;

   @@forxloopend:
   add ebx, 4;
   jz @@exitProc;

   // #####################################
   // #### last < 4 columns
   @@forxloopshort:
      mov eax, height;
      vxorpd xmm0, xmm0, xmm0;
      mov edi, srcLineWidth;
      xor esi, esi;

      vmovsd xmm0, [edx];

      // one column value:
      @@foryloopshort:
          vmovsd xmm1, [edx + edi];
          vsubsd xmm2, xmm1, xmm0;
          vmovsd [ecx + esi], xmm2;

          vmovsd xmm0, xmm0, xmm1;

          add edi, srcLineWidth;
          add esi, destLineWidth;
      dec eax;
      jnz @@foryloopshort;

      add edx, 8;
      add ecx, 8;

   dec ebx;
   jnz @@forxloopshort;

   @@exitProc:

   // epilog - stack cleanup
   pop esi;
   pop edi;
   pop ebx;
end;
{$IFDEF FPC}
end;
{$ENDIF}


{$ENDIF}

end.
