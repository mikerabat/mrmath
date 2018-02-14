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


unit AVXMatrixSqrtOperationsx64;

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
{$IFDEF x64}

uses MatrixConst;

procedure AVXMatrixSQRTAligned(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt);
procedure AVXMatrixSQRTUnAligned(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt);

{$ENDIF}

implementation

{$IFDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$ENDIF}

procedure AVXMatrixSQRTAligned(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt);
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
   mov r10, width;
   shl r10, 3;
   imul r10, -1;

   // helper registers dest pointers
   sub rcx, r10;

   // for y := 0 to height - 1:
   mov r11, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetchw [rcx + rax];

           // elementwise sqrt
           vsqrtpd ymm0, [rcx + rax - 128];
           vmovapd [rcx + rax - 128], ymm0;

           vsqrtpd ymm1, [rcx + rax - 96];
           vmovapd [rcx + rax - 96], ymm1;

           vsqrtpd ymm0, [rcx + rax - 64];
           vmovapd [rcx + rax - 64], ymm0;

           vsqrtpd ymm1, [rcx + rax - 32];
           vmovapd [rcx + rax - 32], ymm1;

       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           add rax, 16;
           jg @addforxloop2end;

           vsqrtpd xmm0, [rcx + rax - 16];
           vmovapd [rcx + rax - 16], xmm0;
       jmp @addforxloop2;

       @addforxloop2end:

       sub rax, 16;

       jz @nextLine;

       vmovsd xmm0, [rcx + rax];
       vsqrtsd xmm0, xmm0, xmm0;
       vmovsd [rcx + rax], xmm0;

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

procedure AVXMatrixSQRTUnAligned(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt);
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
   mov r10, width;
   shl r10, 3;
   imul r10, -1;

   // helper registers dest pointers
   sub rcx, r10;

   // for y := 0 to height - 1:
   mov r11, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetchw [rcx + rax];

           // elementwise sqrt
           vmovupd ymm2, [rcx + rax - 128];
           vsqrtpd ymm0, ymm2;
           vmovupd [rcx + rax - 128], ymm0;

           vmovupd ymm3, [rcx + rax - 96];
           vsqrtpd ymm1, ymm3;
           vmovupd [rcx + rax - 96], ymm1;

           vmovupd ymm2, [rcx + rax - 64];
           vsqrtpd ymm0, ymm2;
           vmovupd [rcx + rax - 64], ymm0;

           vmovupd ymm3, [rcx + rax - 32];
           vsqrtpd ymm1, ymm3;
           vmovupd [rcx + rax - 32], ymm1;

       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           add rax, 16;
           jg @addforxloop2end;

           vmovupd xmm1, [rcx + rax - 16];
           vsqrtpd xmm0, xmm1;
           vmovupd [rcx + rax - 16], xmm0;
       jmp @addforxloop2;

       @addforxloop2end:

       sub rax, 16;

       jz @nextLine;

       vmovsd xmm0, [rcx + rax];
       vsqrtsd xmm0, xmm0, xmm0;
       vmovsd [rcx + rax], xmm0;

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
