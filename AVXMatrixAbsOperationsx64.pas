// ###################################################################
// #### This file is part of the mathematics library project, and is
// #### offered under the licence agreement described on
// #### http://www.mrsoft.org/
// ####
// #### Copyright:(c) 2017, Michael R. . All rights reserved.
// ####
// #### Unless required by applicable law or agreed to in writing, software
// #### distributed under the License is distributed on an "AS IS" BASIS,
// #### WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// #### See the License for the specific language governing permissions and
// #### limitations under the License.
// ###################################################################

unit AVXMatrixAbsOperationsx64;

// #####################################################
// #### Abs opertaion applied to every element in a matrix
// #####################################################

interface

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF x64}

uses MatrixConst;

procedure AVXMatrixAbsAligned(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt);
procedure AVXMatrixAbsUnAligned(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt);

{$ENDIF}

implementation

{$IFDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$ENDIF}

procedure AVXMatrixAbsAligned(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt);
{$IFDEF FPC}
begin
{$ENDIF}
asm
   {$IFDEF LINUX}
   // Linux uses a diffrent ABI -> copy over the registers so they meet with winABI
   // (note that the 5th and 6th parameter are are on the stack)
   // The parameters are passed in the following order:
   // RDI, RSI, RDX, RCX -> mov to RCX, RDX, R8, R9
   mov r8, rdx;
   mov r9, rcx;
   mov rcx, rdi;
   mov rdx, rsi;
   {$ENDIF}

   // note: RCX = dest, RDX = destLineWidth, R8 = width, R9 = height
   //iters := -width*sizeof(double);
   shl r8, 3;
   imul r8, -1;

   // helper registers for the dest pointer
   sub rcx, r8;

   vmovupd ymm0, [rip + cSignBits4];

   // for y := 0 to height - 1:
   mov r11, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r8;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // prefetch data...
           //prefetchw [rcx + rax];

           // Abs:
           vmovapd ymm1, [rcx + rax - 128];
           vAndpd ymm1, ymm1, ymm0;
           vmovntdq [rcx + rax - 128], ymm1;

           vmovapd ymm2, [rcx + rax - 96];
           vandpd ymm2, ymm2, ymm0;
           vmovntdq [rcx + rax - 96], ymm2;

           vmovapd ymm3, [rcx + rax - 64];
           vandpd ymm3, ymm3, ymm0;
           vmovntdq [rcx + rax - 64], ymm3;

           vmovapd ymm4, [rcx + rax - 32];
           vandpd ymm4, ymm4, ymm0;
           vmovntdq [rcx + rax - 32], ymm4;
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           add rax, 16;
           jg @loopEnd2;

           vmovapd xmm1, [rcx + rax - 16];
           vandpd xmm1, xmm1, xmm0;
           vmovntdq [rcx + rax - 16], xmm1;
       jmp @addforxloop2;

       @loopEnd2:

       sub rax, 16;
       jz @nextLine;

       vmovsd xmm1, [rcx + rax];
       vandpd xmm1, xmm7, xmm0;
       vmovsd [rcx + rax], xmm1;

       @nextLine:

       // next line:
       add rcx, rdx;

   // loop y end
   dec r11;
   jnz @@addforyloop;

   vzeroupper;
{$IFDEF FPC}
end;
{$ENDIF}
end;

procedure AVXMatrixAbsUnAligned(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt);
{$IFDEF FPC}
begin
{$ENDIF}
asm
   {$IFDEF LINUX}
   // Linux uses a diffrent ABI -> copy over the registers so they meet with winABI
   // (note that the 5th and 6th parameter are are on the stack)
   // The parameters are passed in the following order:
   // RDI, RSI, RDX, RCX -> mov to RCX, RDX, R8, R9
   mov r8, rdx;
   mov r9, rcx;
   mov rcx, rdi;
   mov rdx, rsi;
   {$ENDIF}

   // note: RCX = dest, RDX = destLineWidth, R8 = width, R9 = height
   //iters := -width*sizeof(double);
   shl r8, 3;
   imul r8, -1;

   // helper registers for the dest pointer
   sub rcx, r8;

   vmovupd ymm0, [rip + cSignBits4];

   // for y := 0 to height - 1:
   mov r11, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r8;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // prefetch data...
           //prefetchw [rcx + rax];

           // Abs:
           vmovupd ymm1, [rcx + rax - 128];
           vAndpd ymm1, ymm1, ymm0;
           vmovupd [rcx + rax - 128], ymm1;

           vmovupd ymm2, [rcx + rax - 96];
           vandpd ymm2, ymm2, ymm0;
           vmovupd [rcx + rax - 96], ymm2;

           vmovupd ymm3, [rcx + rax - 64];
           vandpd ymm3, ymm3, ymm0;
           vmovupd [rcx + rax - 64], ymm3;

           vmovupd ymm4, [rcx + rax - 32];
           vandpd ymm4, ymm4, ymm0;
           vmovupd [rcx + rax - 32], ymm4;
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           add rax, 16;
           jg @loopEnd2;

           vmovupd xmm1, [rcx + rax - 16];
           vandpd xmm1, xmm1, xmm0;
           vmovupd [rcx + rax - 16], xmm1;
       jmp @addforxloop2;

       @loopEnd2:

       sub rax, 16;
       jz @nextLine;

       vmovsd xmm1, [rcx + rax];
       vandpd xmm1, xmm1, xmm0;
       vmovsd [rcx + rax], xmm1;

       @nextLine:

       // next line:
       add rcx, rdx;

   // loop y end
   dec r11;
   jnz @@addforyloop;

   vzeroupper;
{$IFDEF FPC}
end;
{$ENDIF}
end;

{$ENDIF}

end.
