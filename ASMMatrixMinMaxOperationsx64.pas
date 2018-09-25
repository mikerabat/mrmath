// ###################################################################
// #### This file is part of the mathematics library project, and is
// #### offered under the licence agreement described on
// #### http://www.mrsoft.org/
// ####
// #### Copyright:(c) 2011, Michael R. . All rights reserved.
// ####
// #### Unless required by applicable law or agreed to in writing, software
// #### distributed under the License is distributed on an "AS IS" BASIS,
// #### WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// #### See the License for the specific language governing permissions and
// #### limitations under the License.
// ###################################################################


unit ASMMatrixMinMaxOperationsx64;

interface

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF x64}

uses MatrixConst;

function ASMMatrixMaxAlignedEvenW(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double; {$IFDEF FPC}assembler;{$ENDIF}
function ASMMatrixMaxUnAlignedEvenW(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double; {$IFDEF FPC}assembler;{$ENDIF}

function ASMMatrixMaxAlignedOddW(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double; {$IFDEF FPC}assembler;{$ENDIF}
function ASMMatrixMaxUnAlignedOddW(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double; {$IFDEF FPC}assembler;{$ENDIF}

function ASMMatrixMinAlignedEvenW(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double; {$IFDEF FPC}assembler;{$ENDIF}
function ASMMatrixMinUnAlignedEvenW(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double; {$IFDEF FPC}assembler;{$ENDIF}

function ASMMatrixMinAlignedOddW(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double; {$IFDEF FPC}assembler;{$ENDIF}
function ASMMatrixMinUnAlignedOddW(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double; {$IFDEF FPC}assembler;{$ENDIF}

{$ENDIF}

implementation

{$IFDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$S-} {$ENDIF}

function ASMMatrixMaxAlignedEvenW(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double; {$IFDEF FPC}assembler;{$ENDIF}
asm
   {$IFDEF UNIX}
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
   imul rdx, -8;

   sub rcx, rdx;

	  // init result
   movupd xmm0, [rip + cNegMaxDoubles];
   movapd xmm1, xmm0;
   movapd xmm2, xmm0;
   movapd xmm3, xmm0;

   // for y := 0 to height - 1:
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, rdx;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [rcx + rax];

           // max:
           maxpd xmm0, [rcx + rax - 128];
           maxpd xmm1, [rcx + rax - 112];
           maxpd xmm2, [rcx + rax - 96];
           maxpd xmm3, [rcx + rax - 80];
           maxpd xmm0, [rcx + rax - 64];
           maxpd xmm1, [rcx + rax - 48];
           maxpd xmm2, [rcx + rax - 32];
           maxpd xmm3, [rcx + rax - 16];
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           maxpd xmm0, [rcx + rax];
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // next line:
       add rcx, r9;

   // loop y end
   dec r8;
   jnz @@addforyloop;

   // final max ->
   maxpd xmm0, xmm1;
   maxpd xmm0, xmm2;
   maxpd xmm0, xmm3;
   movhlps xmm1, xmm0;
   maxsd xmm0, xmm1;

   // epilog - cleanup stack
end;

function ASMMatrixMaxUnAlignedEvenW(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double; {$IFDEF FPC}assembler;{$ENDIF}
asm
   {$IFDEF UNIX}
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

   imul rdx, -8;

   sub rcx, rdx;

	  // init result
   movupd xmm0, [rip + cNegMaxDoubles];
   movapd xmm3, xmm0;

   // for y := 0 to height - 1:
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, rdx;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // max:
           movupd xmm1, [rcx + rax - 128];
           maxpd xmm0, xmm1;
           movupd xmm2, [rcx + rax - 112];
           maxpd xmm3, xmm2;
           movupd xmm1, [rcx + rax - 96];
       				maxpd xmm0, xmm1;
           movupd xmm2, [rcx + rax - 80];
           maxpd xmm3, xmm2;
           movupd xmm1, [rcx + rax - 64];
           maxpd xmm0, xmm1;
           movupd xmm2, [rcx + rax - 48];
           maxpd xmm3, xmm2;
           movupd xmm1, [rcx + rax - 32];
           maxpd xmm0, xmm1;
           movupd xmm2, [rcx + rax - 16];
           maxpd xmm3, xmm2;
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           movupd xmm1, [rcx + rax];
           maxpd xmm0, xmm1;
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // next line:
       add rcx, r9;

   // loop y end
   dec r8;
   jnz @@addforyloop;

   // final max ->
   maxpd xmm0, xmm3;
   movhlps xmm1, xmm0;
   maxsd xmm0, xmm1;

   // epilog - cleanup stack
end;


function ASMMatrixMaxAlignedOddW(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double; {$IFDEF FPC}assembler;{$ENDIF}
asm
   {$IFDEF UNIX}
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
   dec rdx;
   imul rdx, -8;

   sub rcx, rdx;

   // init result
   movupd xmm0, [rip + cNegMaxDoubles];
   movapd xmm1, xmm0;
   movapd xmm2, xmm0;
   movapd xmm3, xmm0;

   // for y := 0 to height - 1:
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, rdx;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [rcx + rax];

           // max:
           maxpd xmm0, [rcx + rax - 128];
           maxpd xmm1, [rcx + rax - 112];
           maxpd xmm2, [rcx + rax - 96];
           maxpd xmm3, [rcx + rax - 80];
           maxpd xmm0, [rcx + rax - 64];
           maxpd xmm1, [rcx + rax - 48];
           maxpd xmm2, [rcx + rax - 32];
           maxpd xmm3, [rcx + rax - 16];
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           maxpd xmm0, [rcx + rax];
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last column:
       movsd xmm1, [rcx];
       maxsd xmm0, xmm1;

       // next line:
       add rcx, r9;

   // loop y end
   dec r8;
   jnz @@addforyloop;

   // final max ->
   maxpd xmm0, xmm1;
   maxpd xmm0, xmm2;
   maxpd xmm0, xmm3;
   movhlps xmm1, xmm0;
   maxsd xmm0, xmm1;

   // epilog - cleanup stack
end;

function ASMMatrixMaxUnAlignedOddW(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double; {$IFDEF FPC}assembler;{$ENDIF}
asm
   {$IFDEF UNIX}
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
   dec rdx;
   imul rdx, -8;

   sub rcx, rdx;

   // init result
   movupd xmm0, [rip + cNegMaxDoubles];
   movapd xmm3, xmm0;

   // for y := 0 to height - 1:
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, rdx;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // max
           movupd xmm1, [rcx + rax - 128];
           maxpd xmm0, xmm1;
           movupd xmm2, [rcx + rax - 112];
           maxpd xmm3, xmm2;
           movupd xmm1, [rcx + rax - 96];
       				maxpd xmm0, xmm1;
           movupd xmm2, [rcx + rax - 80];
           maxpd xmm3, xmm2;
           movupd xmm1, [rcx + rax - 64];
           maxpd xmm0, xmm1;
           movupd xmm2, [rcx + rax - 48];
           maxpd xmm3, xmm2;
           movupd xmm1, [rcx + rax - 32];
           maxpd xmm0, xmm1;
           movupd xmm2, [rcx + rax - 16];
           maxpd xmm3, xmm2;
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           movupd xmm1, [rcx + rax];
           maxpd xmm0, xmm1;
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last column:
       movsd xmm1, [rcx];
       maxsd xmm0, xmm1;

       // next line:
       add rcx, r9;

   // loop y end
   dec r8;
   jnz @@addforyloop;

   // final max ->
   maxpd xmm0, xmm3;
   movhlps xmm1, xmm0;
   maxsd xmm0, xmm1;

   // epilog - cleanup stack
end;


function ASMMatrixMinAlignedEvenW(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double; {$IFDEF FPC}assembler;{$ENDIF}
asm
   {$IFDEF UNIX}
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
   imul rdx, -8;

   sub rcx, rdx;

	  // init result
   movupd xmm0, [rip + cMaxDoubles];
   movapd xmm1, xmm0;
   movapd xmm2, xmm0;
   movapd xmm3, xmm0;

   // for y := 0 to height - 1:
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, rdx;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [rcx + rax];

           // min:
           minpd xmm0, [rcx + rax - 128];
           minpd xmm1, [rcx + rax - 112];
           minpd xmm2, [rcx + rax - 96];
           minpd xmm3, [rcx + rax - 80];
           minpd xmm0, [rcx + rax - 64];
           minpd xmm1, [rcx + rax - 48];
           minpd xmm2, [rcx + rax - 32];
           minpd xmm3, [rcx + rax - 16];
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           minpd xmm0, [rcx + rax];
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // next line:
       add rcx, r9;

   // loop y end
   dec r8;
   jnz @@addforyloop;

   // final Min ->
   minpd xmm0, xmm1;
   minpd xmm0, xmm2;
   minpd xmm0, xmm3;
   movhlps xmm1, xmm0;
   Minsd xmm0, xmm1;

   // epilog - cleanup stack
end;

function ASMMatrixMinUnAlignedEvenW(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double; {$IFDEF FPC}assembler;{$ENDIF}
asm
   {$IFDEF UNIX}
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
   imul rdx, -8;

   sub rcx, rdx;

	  // init result
   movupd xmm0, [rip + cMaxDoubles];
   movapd xmm3, xmm0;

   // for y := 0 to height - 1:
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, rdx;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // min:
           movupd xmm1, [rcx + rax - 128];
           minpd xmm0, xmm1;
           movupd xmm2, [rcx + rax - 112];
           minpd xmm3, xmm2;
           movupd xmm1, [rcx + rax - 96];
       	   minpd xmm0, xmm1;
           movupd xmm2, [rcx + rax - 80];
           minpd xmm3, xmm2;
           movupd xmm1, [rcx + rax - 64];
           minpd xmm0, xmm1;
           movupd xmm2, [rcx + rax - 48];
           minpd xmm3, xmm2;
           movupd xmm1, [rcx + rax - 32];
           minpd xmm0, xmm1;
           movupd xmm2, [rcx + rax - 16];
           minpd xmm3, xmm2;
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           movupd xmm1, [rcx + rax];
           minpd xmm0, xmm1;
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // next line:
       add rcx, r9;

   // loop y end
   dec r8;
   jnz @@addforyloop;

   // final Min ->
   minpd xmm0, xmm3;
   movhlps xmm1, xmm0;
   Minsd xmm0, xmm1;

   // epilog - cleanup stack
end;


function ASMMatrixMinAlignedOddW(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double; {$IFDEF FPC}assembler;{$ENDIF}
asm
   {$IFDEF UNIX}
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
   dec rdx;
   imul rdx, -8;

   sub rcx, rdx;

   // init result
   movupd xmm0, [rip + cMaxDoubles];
   movapd xmm1, xmm0;
   movapd xmm2, xmm0;
   movapd xmm3, xmm0;

   // for y := 0 to height - 1:
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, rdx;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [rcx + rax];

           // min:
           minpd xmm0, [rcx + rax - 128];
           minpd xmm1, [rcx + rax - 112];
           minpd xmm2, [rcx + rax - 96];
           minpd xmm3, [rcx + rax - 80];
           minpd xmm0, [rcx + rax - 64];
           minpd xmm1, [rcx + rax - 48];
           minpd xmm2, [rcx + rax - 32];
           minpd xmm3, [rcx + rax - 16];
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           minpd xmm0, [rcx + rax];
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last column:
       movsd xmm1, [rcx];
       Minsd xmm0, xmm1;

       // next line:
       add rcx, r9;

   // loop y end
   dec r8;
   jnz @@addforyloop;

   // final Min ->
   minpd xmm0, xmm1;
   minpd xmm0, xmm2;
   minpd xmm0, xmm3;
   movhlps xmm1, xmm0;
   Minsd xmm0, xmm1;

   // epilog - cleanup stack
end;

function ASMMatrixMinUnAlignedOddW(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double; {$IFDEF FPC}assembler;{$ENDIF}
asm
   {$IFDEF UNIX}
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
   dec rdx;
   imul rdx, -8;

   sub rcx, rdx;

	  // init result
   movupd xmm0, [rip + cMaxDoubles];
   movapd xmm3, xmm0;

   // for y := 0 to height - 1:
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, rdx;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // Min
           movupd xmm1, [rcx + rax - 128];
           minpd xmm0, xmm1;
           movupd xmm2, [rcx + rax - 112];
           minpd xmm3, xmm2;
           movupd xmm1, [rcx + rax - 96];
       	   minpd xmm0, xmm1;
           movupd xmm2, [rcx + rax - 80];
           minpd xmm3, xmm2;
           movupd xmm1, [rcx + rax - 64];
           minpd xmm0, xmm1;
           movupd xmm2, [rcx + rax - 48];
           minpd xmm3, xmm2;
           movupd xmm1, [rcx + rax - 32];
           minpd xmm0, xmm1;
           movupd xmm2, [rcx + rax - 16];
           minpd xmm3, xmm2;
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           movupd xmm1, [rcx + rax];
           minpd xmm0, xmm1;
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last column:
       movsd xmm1, [rcx];
       Minsd xmm0, xmm1;

       // next line:
       add rcx, r9;

   // loop y end
   dec r8;
   jnz @@addforyloop;

   // final Min ->
   minpd xmm0, xmm3;
   movhlps xmm1, xmm0;
   Minsd xmm0, xmm1;

   // epilog - cleanup stack
end;

{$ENDIF}

end.
