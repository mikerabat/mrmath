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


unit ASMMatrixSumOperationsx64;

interface

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF x64}

{$IFDEF FPC}
{$MODE Delphi}
{$ASMMODE intel}
{$ENDIF}

uses MatrixConst;

procedure ASMMatrixSumRowAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
procedure ASMMatrixSumRowUnAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
procedure ASMMatrixSumRowAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
procedure ASMMatrixSumRowUnAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);

procedure ASMMatrixSumColumnAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
procedure ASMMatrixSumColumnUnAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
procedure ASMMatrixSumColumnAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
procedure ASMMatrixSumColumnUnAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);

{$ENDIF}

implementation

{$IFDEF x64}

procedure ASMMatrixSumRowAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
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

   // note: RCX = dest, RDX = destLineWidth, R8 = widh, R9 = height

   //iters := -width*sizeof(double);
   mov r10, width;
   shl r10, 3;
   imul r10, -1;

   // helper registers for the mt1, mt2 and dest pointers
   sub r8, r10;

   mov r11, height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       xorpd xmm0, xmm0;
       xorpd xmm1, xmm1;
       xorpd xmm2, xmm2;
       xorpd xmm3, xmm3;

       mov rax, r10;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [r8 + rax];

           // addition:
           addpd xmm0, [r8 + rax - 128];
           addpd xmm1, [r8 + rax - 112];
           addpd xmm2, [r8 + rax - 96];
           addpd xmm3, [r8 + rax - 80];
           addpd xmm0, [r8 + rax - 64];
           addpd xmm1, [r8 + rax - 48];
           addpd xmm2, [r8 + rax - 32];
           addpd xmm3, [r8 + rax - 16];
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           addpd xmm0, [r8 + rax];
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // build result
       addpd xmm0, xmm1;
       addpd xmm2, xmm3;
       addpd xmm0, xmm2;

       movhlps xmm1, xmm0;
       addsd xmm0, xmm1;

       // write result
       movsd [rcx], xmm0;

       // next line:
       add r8, r9;
       add rcx, rdx;
   // loop y end
   dec r11;
   jnz @@addforyloop;
end;

procedure ASMMatrixSumRowUnAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
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

   // note: RCX = dest, RDX = destLineWidth, R8 = widh, R9 = height

   //iters := -width*sizeof(double);
   mov r10, width;
   shl r10, 3;
   imul r10, -1;

   // helper registers for the mt1, mt2 and dest pointers
   sub r8, r10;

   mov r11, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       xorpd xmm0, xmm0;

       mov rax, r10;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // addition:
           movupd xmm1, [r8 + rax - 128];
           addpd xmm0, xmm1;
           movupd xmm1, [r8 + rax - 112];
           addpd xmm0, xmm1;
           movupd xmm1, [r8 + rax - 96];
           addpd xmm0, xmm1;
           movupd xmm1, [r8 + rax - 80];
           addpd xmm0, xmm1;
           movupd xmm1, [r8 + rax - 64];
           addpd xmm0, xmm1;
           movupd xmm1, [r8 + rax - 48];
           addpd xmm0, xmm1;
           movupd xmm1, [r8 + rax - 32];
           addpd xmm0, xmm1;
           movupd xmm1, [r8 + rax - 16];
           addpd xmm0, xmm1;

       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           movupd xmm1, [r8 + rax];
           addpd xmm0, xmm1;
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // build result
       movhlps xmm1, xmm0;
       addsd xmm0, xmm1;

       // write result
       movsd [rcx], xmm0;

       // next line:
       add r8, r9;
       add rcx, rdx;
   // loop y end
   dec r11;
   jnz @@addforyloop;
end;

procedure ASMMatrixSumRowAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
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

   // note: RCX = dest, RDX = destLineWidth, R8 = widh, R9 = height

   //iters := -(width - 1)*sizeof(double);
   mov r10, width;
   dec r10;
   shl r10, 3;
   imul r10, -1;

   // helper registers for the mt1, mt2 and dest pointers
   sub r8, r10;

   mov r11, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       xorpd xmm0, xmm0;
       xorpd xmm1, xmm1;
       xorpd xmm2, xmm2;
       xorpd xmm3, xmm3;

       mov rax, r10;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [r8 + rax];

           // addition:
           addpd xmm0, [r8 + rax - 128];
           addpd xmm1, [r8 + rax - 112];
           addpd xmm2, [r8 + rax - 96];
           addpd xmm3, [r8 + rax - 80];
           addpd xmm0, [r8 + rax - 64];
           addpd xmm1, [r8 + rax - 48];
           addpd xmm2, [r8 + rax - 32];
           addpd xmm3, [r8 + rax - 16];
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           addpd xmm0, [r8 + rax];
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // build result
       addpd xmm0, xmm1;
       addpd xmm2, xmm3;
       addpd xmm0, xmm2;

       // special handling of the last element
       movsd xmm3, [r8];
       addsd xmm0, xmm3;

       movhlps xmm1, xmm0;
       addsd xmm0, xmm1;

       // write result
       movsd [rcx], xmm0;

       // next line:
       add r8, r9;
       add rcx, rdx;
   // loop y end
   dec r11;
   jnz @@addforyloop;
end;

procedure ASMMatrixSumRowUnAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
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

   // note: RCX = dest, RDX = destLineWidth, R8 = widh, R9 = height

   //iters := -(width - 1)*sizeof(double);
   mov r10, width;
   dec r10;
   shl r10, 3;
   imul r10, -1;

   // helper registers for the mt1, mt2 and dest pointers
   sub r8, r10;

   mov r11, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       xorpd xmm0, xmm0;

       mov rax, r10;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // addition:
           movupd xmm1, [r8 + rax - 128];
           addpd xmm0, xmm1;
           movupd xmm1, [r8 + rax - 112];
           addpd xmm0, xmm1;
           movupd xmm1, [r8 + rax - 96];
           addpd xmm0, xmm1;
           movupd xmm1, [r8 + rax - 80];
           addpd xmm0, xmm1;
           movupd xmm1, [r8 + rax - 64];
           addpd xmm0, xmm1;
           movupd xmm1, [r8 + rax - 48];
           addpd xmm0, xmm1;
           movupd xmm1, [r8 + rax - 32];
           addpd xmm0, xmm1;
           movupd xmm1, [r8 + rax - 16];
           addpd xmm0, xmm1;

       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           movupd xmm1, [r8 + rax];
           addpd xmm0, xmm1;
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // special handling of the last element
       movsd xmm1, [r8];
       addsd xmm0, xmm1;

       // build result
       movhlps xmm1, xmm0;
       addsd xmm0, xmm1;

       // write result
       movsd [rcx], xmm0;

       // next line:
       add r8, r9;
       add rcx, rdx;
   // loop y end
   dec r11;
   jnz @@addforyloop;
end;


procedure ASMMatrixSumColumnAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
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

   // note: RCX = dest, RDX = destLineWidth, R8 = src, R9 = srcLineWidth
   xor r10, r10;
   sub r10, height;
   imul r10, srcLineWidth;

   // helper registers for the mt1, mt2 and dest pointers
   sub r8, r10;

   // for x := 0 to width - 1:
   mov r11, Width;
   sar r11, 1;
   @@addforxloop:
       xorpd xmm1, xmm1;

       // for y := 0 to height - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforyloop:
           addpd xmm1, [r8 + rax];
       add rax, r9;
       jnz @addforyloop;

       // build result
       movapd [rcx], xmm1;

       // next columns:
       add rcx, 16;
       add r8, 16;

   // loop x end
   dec r11;
   jnz @@addforxloop;
end;

procedure ASMMatrixSumColumnUnAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
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

   // note: RCX = dest, RDX = destLineWidth, R8 = src, R9 = srcLineWidth
   xor r10, r10;
   sub r10, height;
   imul r10, srcLineWidth;

   // helper registers for the mt1, mt2 and dest pointers
   sub r8, r10;

   // for x := 0 to width - 1:
   mov r11, Width;
   sar r11, 1;
   @@addforxloop:
       xorpd xmm1, xmm1;

       // for y := 0 to height - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforyloop:
           movupd xmm0, [r8 + rax];
           addpd xmm1, xmm0;
       add rax, r9;
       jnz @addforyloop;

       // build result
       movupd [rcx], xmm1;

       // next columns:
       add rcx, 16;
       add r8, 16;

   // loop x end
   dec r11;
   jnz @@addforxloop;
end;

procedure ASMMatrixSumColumnAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
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

   // note: RCX = dest, RDX = destLineWidth, R8 = src, R9 = srcLineWidth
   xor r10, r10;
   sub r10, height;
   imul r10, srcLineWidth;

   // helper registers for the mt1, mt2 and dest pointers
   sub r8, r10;

   // for x := 0 to width - 1:
   mov r11, Width;
   sar r11, 1;
   jz @lastColumn;
   @@addforxloop:
       xorpd xmm1, xmm1;

       // for y := 0 to height - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforyloop:
           addpd xmm1, [r8 + rax];
       add rax, r9;
       jnz @addforyloop;

       // build result
       movapd [rcx], xmm1;

       // next columns:
       add rcx, 16;
       add r8, 16;

   // loop x end
   dec r11;
   jnz @@addforxloop;

   @lastColumn:
   // handle last column
   xorpd xmm1, xmm1;

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov rax, r10;
   @addforyloop3:
       movsd xmm0, [r8 + rax];
       addsd xmm1, xmm0;
   add rax, r9;
   jnz @addforyloop3;

   // build result
   movsd [rcx], xmm1;
end;

procedure ASMMatrixSumColumnUnAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
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

   // note: RCX = dest, RDX = destLineWidth, R8 = src, R9 = srcLineWidth
   xor r10, r10;
   sub r10, height;
   imul r10, srcLineWidth;

   // helper registers for the mt1, mt2 and dest pointers
   sub r8, r10;

   // for x := 0 to width - 1:
   mov r11, Width;
   sar r11, 1;
   jz @lastColumn;
   @@addforxloop:
       xorpd xmm1, xmm1;

       // for y := 0 to height - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforyloop:
           movupd xmm0, [r8 + rax];
           addpd xmm1, xmm0;
       add rax, r9;
       jnz @addforyloop;

       // build result
       movupd [rcx], xmm1;

       // next columns:
       add rcx, 16;
       add r8, 16;

   // loop x end
   dec r11;
   jnz @@addforxloop;

   @lastColumn:
   // handle last column
   xorpd xmm1, xmm1;

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov rax, r10;
   @addforyloop3:
       movsd xmm0, [r8 + rax];
       addsd xmm1, xmm0;
   add rax, r9;
   jnz @addforyloop3;

   // build result
   movsd [rcx], xmm1;
end;

{$ENDIF}

end.
