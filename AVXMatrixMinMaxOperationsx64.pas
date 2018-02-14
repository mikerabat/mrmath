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


unit AVXMatrixMinMaxOperationsx64;

interface

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF x64}

uses MatrixConst;

function AVXMatrixMaxAligned(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double;
function AVXMatrixMaxUnAligned(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double;

function AVXMatrixMinAligned(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double;
function AVXMatrixMinUnAligned(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double;

{$ENDIF}

implementation

{$IFDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$ENDIF}

function AVXMatrixMaxAligned(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double;
var iRBX : TASMNativeInt;
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

   // note: RCX = mt, RDX = width, R8 = height, R9 = LineWidth
   // prolog - simulate stack
   mov iRBX, rbx;

   imul rdx, -8;

   sub rcx, rdx;

   // init result
   lea rax, [rip + cNegMaxDouble];
   vbroadcastsd ymm0, [rax];
   vmovapd ymm3, ymm0;
   vmovapd ymm4, ymm0;

   // for y := 0 to height - 1:
   mov rbx, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, rdx;
       add rax, 128;
       jg @loopEnd;

       @addforxloop:
           // prefetch data...
           // prefetch [rcx + rax];

           // max:
           vmaxpd ymm3, ymm3, [rcx + rax - 128];
           vmaxpd ymm4, ymm4, [rcx + rax - 96];
           vmaxpd ymm3, ymm3, [rcx + rax - 64];
           vmaxpd ymm4, ymm4, [rcx + rax - 32];

       add rax, 128;
       jle @addforxloop;

       vextractf128 xmm2, ymm3, 1;
       vmaxpd xmm2, xmm2, xmm3;
       vmaxpd xmm0, xmm0, xmm2;
       vextractf128 xmm2, ymm4, 1;
       vmaxpd xmm2, xmm2, xmm4;
       vmaxpd xmm0, xmm0, xmm2;

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           add rax, 16;
           jg @addforxloop2end;

           vmaxpd xmm0, xmm0, [rcx + rax - 16];
       jmp @addforxloop2;

       @addforxloop2end:

       sub rax, 16;
       jz @nextLine;

       vmovsd xmm2, [rcx + rax];
       vmaxsd xmm0, xmm0, xmm2;

       @nextLine:

       // next line:
       add rcx, r9;

   // loop y end
   dec rbx;
   jnz @@addforyloop;

   // final max ->
   vmovhlps xmm1, xmm1, xmm0;
   vmaxsd xmm0, xmm0, xmm1;

   // epilog - cleanup stack
   mov rbx, iRBX;
   vzeroupper;
{$IFDEF FPC}
end;
{$ENDIF}
end;

function AVXMatrixMaxUnAligned(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double;
var iRBX : TASMNativeInt;
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

   // note: RCX = mt, RDX = width, R8 = height, R9 = LineWidth
   // prolog - simulate stack
   mov iRBX, rbx;

   imul rdx, -8;

   sub rcx, rdx;

   // init result
   lea rax, [rip + cNegMaxDouble];
   vbroadcastsd ymm0, [rax];
   vmovapd ymm3, ymm0;
   vmovapd ymm4, ymm0;

   // for y := 0 to height - 1:
   mov rbx, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, rdx;

       add rax, 128;
       jg @loopEnd;

       @addforxloop:
           // prefetch data...
           // prefetch [rcx + rax];

           // max:
           vmovupd ymm2, [rcx + rax - 128];
           vmaxpd ymm3, ymm3, ymm2;
           vmovupd ymm2, [rcx + rax - 96];
           vmaxpd ymm4, ymm4, ymm2;
           vmovupd ymm2, [rcx + rax - 64];
           vmaxpd ymm3, ymm3, ymm2;
           vmovupd ymm2, [rcx + rax - 32];
           vmaxpd ymm4, ymm4, ymm2;
       add rax, 128;
       jle @addforxloop;

       vextractf128 xmm2, ymm3, 1;
       vmaxpd xmm2, xmm2, xmm3;
       vmaxpd xmm0, xmm0, xmm2;
       vextractf128 xmm2, ymm4, 1;
       vmaxpd xmm2, xmm2, xmm4;
       vmaxpd xmm0, xmm0, xmm2;

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           add rax, 16;
           jg @addforxloop2end;

           vmovupd xmm2, [rcx + rax - 16];
           vmaxpd xmm0, xmm0, xmm2;
       jmp @addforxloop2;

       @addforxloop2end:

       sub rax, 16;
       jz @nextLine;

       vmovsd xmm2, [rcx + rax];
       vmaxsd xmm0, xmm0, xmm2;

       @nextLine:

       // next line:
       add rcx, r9;

   // loop y end
   dec rbx;
   jnz @@addforyloop;

   // final max ->
   vmovhlps xmm1, xmm1, xmm0;
   vmaxsd xmm0, xmm0, xmm1;

   // epilog - cleanup stack
   mov rbx, iRBX;
   vzeroupper;
{$IFDEF FPC}
end;
{$ENDIF}
end;

function AVXMatrixMinAligned(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double;
var iRBX : TASMNativeInt;
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

   // note: RCX = mt, RDX = width, R8 = height, R9 = LineWidth
   // prolog - simulate stack
   mov iRBX, rbx;

   imul rdx, -8;

   sub rcx, rdx;

   // init result
   lea rax, [rip + cMaxDouble];
   vbroadcastsd ymm0, [rax];
   vmovapd ymm3, ymm0;
   vmovapd ymm4, ymm0;

   // for y := 0 to height - 1:
   mov rbx, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, rdx;
       add rax, 128;
       jg @loopEnd;

       @addforxloop:
           // prefetch data...
           // prefetch [rcx + rax];

           // max:
           vminpd ymm3, ymm3, [rcx + rax - 128];
           vminpd ymm4, ymm4, [rcx + rax - 96];
           vminpd ymm3, ymm3, [rcx + rax - 64];
           vminpd ymm4, ymm4, [rcx + rax - 32];
       add rax, 128;
       jle @addforxloop

       vextractf128 xmm2, ymm3, 1;
       vminpd xmm2, xmm2, xmm3;
       vminpd xmm0, xmm0, xmm2;
       vextractf128 xmm2, ymm4, 1;
       vminpd xmm2, xmm2, xmm4;
       vminpd xmm0, xmm0, xmm2;

       @loopEnd:
       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           add rax, 16;
           jg @addforxloop2end;

           vminpd xmm0, xmm0, [rcx + rax - 16];
       jmp @addforxloop2;

       @addforxloop2end:

       sub rax, 16;
       jz @nextLine;

       vmovsd xmm2, [rcx + rax];
       vminsd xmm0, xmm0, xmm2;

       @nextLine:

       // next line:
       add rcx, r9;

   // loop y end
   dec rbx;
   jnz @@addforyloop;

   // final max ->
   vmovhlps xmm1, xmm1, xmm0;
   vminsd xmm0, xmm0, xmm1;

   // epilog - cleanup stack
   mov rbx, iRBX;
   vzeroupper;
{$IFDEF FPC}
end;
{$ENDIF}
end;

function AVXMatrixMinUnAligned(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double;
var iRBX : TASMNativeInt;
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

   // note: RCX = mt, RDX = width, R8 = height, R9 = LineWidth
   // prolog - simulate stack
   mov iRBX, rbx;

   imul rdx, -8;

   sub rcx, rdx;

   // init result
   lea rax, [rip + cMaxDouble];
   vbroadcastsd ymm0, [rax];
   vmovapd ymm3, ymm0;
   vmovapd ymm4, ymm0;

   // for y := 0 to height - 1:
   mov rbx, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, rdx;
       add rax, 128;
       jg @loopEnd;

       @addforxloop:
           // prefetch data...
           // prefetch [rcx + rax];

           // max:
           vmovupd ymm2, [rcx + rax - 128];
           vminpd ymm3, ymm3, ymm2;
           vmovupd ymm2, [rcx + rax - 96];
           vminpd ymm4, ymm4, ymm2;
           vmovupd ymm2, [rcx + rax - 64];
           vminpd ymm3, ymm3, ymm2;
           vmovupd ymm2, [rcx + rax - 32];
           vminpd ymm4, ymm4, ymm2;
       add rax, 128;
       jle @addforxloop

       vextractf128 xmm2, ymm3, 1;
       vminpd xmm2, xmm2, xmm3;
       vminpd xmm0, xmm0, xmm2;
       vextractf128 xmm2, ymm4, 1;
       vminpd xmm2, xmm2, xmm4;
       vminpd xmm0, xmm0, xmm2;

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           add rax, 16;
           jg @addforxloop2end;

           vmovupd xmm2, [rcx + rax - 16];
           vminpd xmm0, xmm0, xmm2;
       jmp @addforxloop2;

       @addforxloop2end:
       sub rax, 16;
       jz @nextLine;

       vmovsd xmm2, [rcx + rax];
       vminsd xmm0, xmm0, xmm2;

       @nextLine:

       // next line:
       add rcx, r9;

   // loop y end
   dec rbx;
   jnz @@addforyloop;

   // final max ->
   vmovhlps xmm1, xmm1, xmm0;
   vminsd xmm0, xmm0, xmm1;

   // epilog - cleanup stack
   mov rbx, iRBX;
   vzeroupper;
{$IFDEF FPC}
end;
{$ENDIF}
end;

{$ENDIF}

end.
