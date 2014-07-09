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


unit ASMMatrixMeanOperationsx64;

interface

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF x64}

uses MatrixConst;

procedure ASMMatrixMeanRowAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
procedure ASMMatrixMeanRowUnAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
procedure ASMMatrixMeanRowAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
procedure ASMMatrixMeanRowUnAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);

procedure ASMMatrixMeanColumnAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
procedure ASMMatrixMeanColumnUnAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
procedure ASMMatrixMeanColumnAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
procedure ASMMatrixMeanColumnUnAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);

procedure ASMMatrixVarRowAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean);
procedure ASMMatrixVarRowUnAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean);
procedure ASMMatrixVarRowAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean);
procedure ASMMatrixVarRowUnAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean);

procedure ASMMatrixVarColumnAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean);
procedure ASMMatrixVarColumnUnAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean);
procedure ASMMatrixVarColumnAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean);
procedure ASMMatrixVarColumnUnAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean);

{$ENDIF}

implementation

{$IFDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$ENDIF}


procedure ASMMatrixMeanRowAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
var dXMM5 : Array[0..1] of double;
{$IFDEF FPC}
begin
{$ENDIF}
asm
   // note: RCX = dest, RDX = destLineWidth, R8 = src, R9 = srcLineWidth

   // prolog
   movapd dXMM5, xmm5;
   {
   .savenv xmm5;
   }

   // iters := -width*sizeof(double)
   mov r10, width;
   shl r10, 3;
   imul r10, -1;

   // helper registers for the mt1, mt2 and dest pointers
   sub r8, r10;

   cvtsi2sd xmm5, width;

   // for y := 0 to height - 1:
   mov r11, Height;
   @@addforyloop:
       xorpd xmm0, xmm0;
       xorpd xmm1, xmm1;
       xorpd xmm2, xmm2;
       xorpd xmm3, xmm3;

       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;
           // prefetch data...
           // prefetch [r8 + rax];

           // addition:
           addpd xmm0, [r8 + rax - 128];
           addpd xmm1, [r8 + rax - 112];
           addpd xmm1, [r8 + rax - 96];
           addpd xmm3, [r8 + rax - 80];
           addpd xmm0, [r8 + rax - 64];
           addpd xmm1, [r8 + rax - 48];
           addpd xmm2, [r8 + rax - 32];
           addpd xmm3, [r8 + rax - 16];
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @buildRes;

       @addforxloop2:
           addpd xmm0, [r8 + rax];
       add rax, 16;
       jnz @addforxloop2;

       @buildRes:

       addpd xmm0, xmm1;
       addpd xmm2, xmm3;
       addpd xmm0, xmm2;

       // build result
       movhlps xmm1, xmm0;
       addsd xmm0, xmm1;

       divsd xmm0, xmm5;

       // write result
       movlpd [rcx], xmm0;

       // next line:
       add r8, r9;
       add rcx, rdx;

   // loop y end
   dec r11;
   jnz @@addforyloop;

   // epilog
   movapd xmm5, dXMM5;
{$IFDEF FPC}
end;
{$ENDIF}
end;

procedure ASMMatrixMeanRowUnAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
var dXMM5, dXMM4 : Array[0..1] of double;
{$IFDEF FPC}
begin
  {$ENDIF}
asm
   // note: RCX = dest, RDX = destLineWidth, R8 = src, R9 = srcLineWidth

   // prolog
   movapd dXMM5, xmm5;
   movapd dXMM4, xmm4;
   {
   .savenv xmm5;
   .savenv xmm4;
   }

   // iters := -width*sizeof(double)
   mov r10, width;
   shl r10, 3;
   imul r10, -1;

   // helper registers for the mt1, mt2 and dest pointers
   sub r8, r10;

   cvtsi2sd xmm5, width;

   // for y := 0 to height - 1:
   mov r11, Height;
   @@addforyloop:
       xorpd xmm4, xmm4;

       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // addition:
           movupd xmm0, [r8 + rax - 128];
           addpd xmm4, xmm0;

           movupd xmm1, [r8 + rax - 112];
           addpd xmm4, xmm1;

           movupd xmm2, [r8 + rax - 96];
           addpd xmm4, xmm2;

           movupd xmm3, [r8 + rax - 80];
           addpd xmm4, xmm3;

           movupd xmm0, [r8 + rax - 64];
           addpd xmm4, xmm0;

           movupd xmm1, [r8 + rax - 48];
           addpd xmm4, xmm1;

           movupd xmm2, [r8 + rax - 32];
           addpd xmm4, xmm2;

           movupd xmm3, [r8 + rax - 16];
           addpd xmm4, xmm3;
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @buildRes;

       @addforxloop2:
           movupd xmm0, [r8 + rax];
           addpd xmm4, xmm0;
       add rax, 16;
       jnz @addforxloop2;

       @buildRes:

       // build result
       movhlps xmm1, xmm4;
       addsd xmm4, xmm1;

       divsd xmm4, xmm5;

       // write result
       movlpd [rcx], xmm4;

       // next line:
       add r8, r9;
       add rcx, rdx;

   // loop y end
   dec r11;
   jnz @@addforyloop;

   // epilog
   movapd xmm5, dXMM5;
   movapd xmm4, dXMM4
{$IFDEF FPC}
end;
{$ENDIF}
end;

procedure ASMMatrixMeanRowAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
var dXMM5 : Array[0..1] of double;
{$IFDEF FPC}
begin
  {$ENDIF}
asm
   // note: RCX = dest, RDX = destLineWidth, R8 = src, R9 = srcLineWidth

   // prolog
   movapd dXMM5, xmm5;
   {
   .savenv xmm5;
   }

   // iters := -width*sizeof(double)
   mov r10, width;
   dec r10;
   shl r10, 3;
   imul r10, -1;

   // helper registers for the mt1, mt2 and dest pointers
   sub r8, r10;

   cvtsi2sd xmm5, width;

   // for y := 0 to height - 1:
   mov r11, Height;
   @@addforyloop:
       xorpd xmm0, xmm0;
       xorpd xmm1, xmm1;
       xorpd xmm2, xmm2;
       xorpd xmm3, xmm3;

       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;
            // prefetch data...
           // prefetch [r8 + rax];

           // addition:
           addpd xmm0, [r8 + rax - 128];
           addpd xmm1, [r8 + rax - 112];
           addpd xmm1, [r8 + rax - 96];
           addpd xmm3, [r8 + rax - 80];
           addpd xmm0, [r8 + rax - 64];
           addpd xmm1, [r8 + rax - 48];
           addpd xmm2, [r8 + rax - 32];
           addpd xmm3, [r8 + rax - 16];
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @buildRes;

       @addforxloop2:
           addpd xmm0, [r8 + rax];
       add rax, 16;
       jnz @addforxloop2;

       @buildRes:

       addpd xmm0, xmm1;
       addpd xmm2, xmm3;
       addpd xmm0, xmm2;

       // handle last element differently
       movlpd xmm2, [r8 + rax];
       addsd xmm0, xmm2;

       // build result
       movhlps xmm1, xmm0;
       addsd xmm0, xmm1;

       divsd xmm0, xmm5;

       // write result
       movlpd [rcx], xmm0;

       // next line:
       add r8, r9;
       add rcx, rdx;

   // loop y end
   dec r11;
   jnz @@addforyloop;

   // epilog
   movapd xmm5, dXMM5;
{$IFDEF FPC}
end;
{$ENDIF}
end;

procedure ASMMatrixMeanRowUnAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
var dXMM5, dXMM4 : Array[0..1] of double;
{$IFDEF FPC}
begin
  {$ENDIF}
asm
   // note: RCX = dest, RDX = destLineWidth, R8 = src, R9 = srcLineWidth

   // prolog
   movapd dXMM5, xmm5;
   movapd dXMM4, xmm4;
   {
   .savenv xmm5;
   .savenv xmm4;
   }

   // iters := -width*sizeof(double)
   mov r10, width;
   dec r10;
   shl r10, 3;
   imul r10, -1;

   // helper registers for the mt1, mt2 and dest pointers
   sub r8, r10;

   cvtsi2sd xmm5, width;

   // for y := 0 to height - 1:
   mov r11, Height;
   @@addforyloop:
       xorpd xmm4, xmm4;

       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // addition:
           movupd xmm0, [r8 + rax - 128];
           addpd xmm4, xmm0;

           movupd xmm1, [r8 + rax - 112];
           addpd xmm4, xmm1;

           movupd xmm2, [r8 + rax - 96];
           addpd xmm4, xmm2;

           movupd xmm3, [r8 + rax - 80];
           addpd xmm4, xmm3;

           movupd xmm0, [r8 + rax - 64];
           addpd xmm4, xmm0;

           movupd xmm1, [r8 + rax - 48];
           addpd xmm4, xmm1;

           movupd xmm2, [r8 + rax - 32];
           addpd xmm4, xmm2;

           movupd xmm3, [r8 + rax - 16];
           addpd xmm4, xmm3;
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @buildRes;

       @addforxloop2:
           movupd xmm0, [r8 + rax];
           addpd xmm4, xmm0;
       add rax, 16;
       jnz @addforxloop2;

       @buildRes:

       // handle last element differently
       movlpd xmm2, [r8 + rax];
       addsd xmm4, xmm2;

       // build result
       movhlps xmm1, xmm4;
       addsd xmm4, xmm1;

       divsd xmm4, xmm5;

       // write result
       movlpd [rcx], xmm4;

       // next line:
       add r8, r9;
       add rcx, rdx;

   // loop y end
   dec r11;
   jnz @@addforyloop;

   // epilog
   movapd xmm5, dXMM5;
   movapd xmm4, dXMM4;
{$IFDEF FPC}
end;
{$ENDIF}
end;

procedure ASMMatrixMeanColumnAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
{$IFDEF FPC}
begin
  {$ENDIF}
asm
   // note: RCX = dest, RDX = destLineWidth, R8 = src, R9 = srcLineWidth
   xor r10, r10;
   sub r10, height;
   imul r10, srcLineWidth;

   // helper registers for the mt1, mt2 and dest pointers
   sub r8, r10;

   cvtsi2sd xmm0, height;
   movddup xmm2, xmm0;

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
       divpd xmm1, xmm2;
       movapd [rcx], xmm1;

       // next columns:
       add rcx, 16;
       add r8, 16;

   // loop x end
   dec r11;
   jnz @@addforxloop;
{$IFDEF FPC}
end;
{$ENDIF}
end;

procedure ASMMatrixMeanColumnUnAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
{$IFDEF FPC}
begin
  {$ENDIF}
asm
   // note: RCX = dest, RDX = destLineWidth, R8 = src, R9 = srcLineWidth
   xor r10, r10;
   sub r10, height;
   imul r10, srcLineWidth;

   // helper registers for the mt1, mt2 and dest pointers
   sub r8, r10;

   cvtsi2sd xmm0, height;
   movddup xmm2, xmm0;

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
       divpd xmm1, xmm2;
       movupd [rcx], xmm1;

       // next columns:
       add rcx, 16;
       add r8, 16;

   // loop x end
   dec r11;
   jnz @@addforxloop;
{$IFDEF FPC}
end;
{$ENDIF}
end;

procedure ASMMatrixMeanColumnAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
{$IFDEF FPC}
begin
  {$ENDIF}
asm
   // note: RCX = dest, RDX = destLineWidth, R8 = src, R9 = srcLineWidth
   xor r10, r10;
   sub r10, height;
   imul r10, srcLineWidth;

   // helper registers for the mt1, mt2 and dest pointers
   sub r8, r10;

   cvtsi2sd xmm0, height;
   movddup xmm2, xmm0;

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
       divpd xmm1, xmm2;
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
       movlpd xmm0, [r8 + rax];
       addsd xmm1, xmm0;
   add rax, r9;
   jnz @addforyloop3;

   // build result
   divsd xmm1, xmm2;

   movlpd [rcx], xmm1;
{$IFDEF FPC}
end;
{$ENDIF}
end;

procedure ASMMatrixMeanColumnUnAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
{$IFDEF FPC}
begin
  {$ENDIF}
asm
   // note: RCX = dest, RDX = destLineWidth, R8 = src, R9 = srcLineWidth
   xor r10, r10;
   sub r10, height;
   imul r10, srcLineWidth;

   // helper registers for the mt1, mt2 and dest pointers
   sub r8, r10;

   cvtsi2sd xmm0, height;
   movddup xmm2, xmm0;

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
       divpd xmm1, xmm2;
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
       movlpd xmm0, [r8 + rax];
       addsd xmm1, xmm0;
   add rax, r9;
   jnz @addforyloop3;

   // build result
   divsd xmm1, xmm2;

   movlpd [rcx], xmm1;
{$IFDEF FPC}
end;
{$ENDIF}
end;


// ###########################################
// #### Variance calculation
// ###########################################


// ###########################################
// #### Variance calculation
// ###########################################

const cOnes : Array[0..1] of double = (1, 1);
      cOne : double = 1;
      

procedure ASMMatrixVarRowAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean);
var dXMM5 : Array[0..1] of double;
{$IFDEF FPC}
begin
{$ENDIF}
asm
   // note: RCX = dest, RDX = destLineWidth, R8 = src, R9 = srcLineWidth

   // prolog
   movapd dXMM5, xmm5;
   {
   .savenv xmm5;
   }

   // iters := -width*sizeof(double)
   mov r10, width;
   shl r10, 3;
   imul r10, -1;

   // helper registers for the mt1, mt2 and dest pointers
   sub r8, r10;

   cvtsi2sd xmm5, width;

   // for y := 0 to height - 1:
   mov r11, Height;
   @@addforyloop:
       xorpd xmm0, xmm0;
       xorpd xmm1, xmm1;
       xorpd xmm2, xmm2;
       xorpd xmm3, xmm3;

       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;
           // prefetch data...
           // prefetch [r8 + rax];

           // addition:
           addpd xmm0, [r8 + rax - 128];
           addpd xmm1, [r8 + rax - 112];
           addpd xmm1, [r8 + rax - 96];
           addpd xmm3, [r8 + rax - 80];
           addpd xmm0, [r8 + rax - 64];
           addpd xmm1, [r8 + rax - 48];
           addpd xmm2, [r8 + rax - 32];
           addpd xmm3, [r8 + rax - 16];
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @buildRes;

       @addforxloop2:
           addpd xmm0, [r8 + rax];
       add rax, 16;
       jnz @addforxloop2;

       @buildRes:

       addpd xmm0, xmm1;
       addpd xmm2, xmm3;
       addpd xmm0, xmm2;

       // build result
       movhlps xmm1, xmm0;
       addsd xmm0, xmm1;

       divsd xmm0, xmm5;

       // we have calculated the mean -> 
       // repeat the loop to calculate the variance

       movddup xmm4, xmm0;
       xorpd xmm0, xmm0;
            
       // for x := 0 to w - 1;
       // prepare for reverse loop
       // variance = sum (x - mean)^2
       mov rax, r10;
       @addforxloop3:
           add rax, 128;
           jg @loopEnd2;
           // prefetch data...
           // prefetch [ecx + rax];

           // addition:
           movapd xmm1, [r8 + rax - 128];
           movapd xmm2, [r8 + rax - 112];
           subpd xmm1, xmm4;
           subpd xmm2, xmm4;
           mulpd xmm1, xmm1;
           mulpd xmm2, xmm2;
           addpd xmm0, xmm1;
           addpd xmm0, xmm2;
                
           movapd xmm1, [r8 + rax - 96];
           movapd xmm2, [r8 + rax - 80];
           subpd xmm1, xmm4;
           subpd xmm2, xmm4;
           mulpd xmm1, xmm1;
           mulpd xmm2, xmm2;
           addpd xmm0, xmm1;
           addpd xmm0, xmm2;
                
           movapd xmm1, [r8 + rax - 64];
           movapd xmm2, [r8 + rax - 48];
           subpd xmm1, xmm4;
           subpd xmm2, xmm4;
           mulpd xmm1, xmm1;
           mulpd xmm2, xmm2;
           addpd xmm0, xmm1;
           addpd xmm0, xmm2;
                
           movapd xmm1, [r8 + rax - 32];
           movapd xmm2, [r8 + rax - 16];
           subpd xmm1, xmm4;
           subpd xmm2, xmm4;
           mulpd xmm1, xmm1;
           mulpd xmm2, xmm2;
           addpd xmm0, xmm1;
           addpd xmm0, xmm2;
                
       jmp @addforxloop3

       @loopEnd2:

       sub rax, 128;

       jz @buildRes2;

       @addforxloop4:
           movapd xmm1, [r8 + rax];
           subpd xmm1, xmm4;
           mulpd xmm1, xmm1;
           addpd xmm0, xmm1;
       add rax, 16;
       jnz @addforxloop4;

       @buildRes2:

       // build result
       movhlps xmm1, xmm0;
       addsd xmm0, xmm1;
            
       // check if we need to use the unbiased version
       cmp unbiased, 0;
       jz @@dobiased;

       movsd xmm4, xmm5;
       subsd xmm4, cOne;
       maxsd xmm4, cOne;

       divsd xmm0, xmm4;

       jmp @@writeRes;

       @@dobiased:
       divsd xmm0, xmm5;
            
       // write result
       @@writeRes:
       movlpd [rcx], xmm0;

       // next line:
       add r8, r9;
       add rcx, rdx;

   // loop y end
   dec r11;
   jnz @@addforyloop;

   // epilog
   movapd xmm5, dXMM5;
{$IFDEF FPC}
end;
{$ENDIF}
end;

procedure ASMMatrixVarRowUnAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean);
var dXMM5 : Array[0..1] of double;
{$IFDEF FPC}
begin
{$ENDIF}
asm
   // note: RCX = dest, RDX = destLineWidth, R8 = src, R9 = srcLineWidth

   // prolog
   movapd dXMM5, xmm5;
   {
   .savenv xmm5;
   }

   // iters := -width*sizeof(double)
   mov r10, width;
   shl r10, 3;
   imul r10, -1;

   // helper registers for the mt1, mt2 and dest pointers
   sub r8, r10;

   cvtsi2sd xmm5, width;

   // for y := 0 to height - 1:
   mov r11, Height;
   @@addforyloop:
       xorpd xmm0, xmm0;

       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;
           // prefetch data...
           // prefetch [r8 + rax];

           // addition:
           movupd xmm1, [r8 + rax - 128];
           addpd xmm0, xmm1;

           movupd xmm2, [r8 + rax - 112];
           addpd xmm0, xmm2;

           movupd xmm3, [r8 + rax - 96];
           addpd xmm0, xmm3;

           movupd xmm4, [r8 + rax - 80];
           addpd xmm0, xmm4;

           movupd xmm1, [r8 + rax - 64];
           addpd xmm0, xmm1;

           movupd xmm2, [r8 + rax - 48];
           addpd xmm0, xmm2;

           movupd xmm3, [r8 + rax - 32];
           addpd xmm0, xmm3;

           movupd xmm4, [r8 + rax - 16];
           addpd xmm0, xmm4;
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @buildRes;

       @addforxloop2:
           movupd xmm1, [r8 + rax];
           addpd xmm0, movupd;
       add rax, 16;
       jnz @addforxloop2;

       @buildRes:

       // build result
       movhlps xmm1, xmm0;
       addsd xmm0, xmm1;

       divsd xmm0, xmm5;

       // we have calculated the mean -> 
       // repeat the loop to calculate the variance

       movddup xmm4, xmm0;
       xorpd xmm0, xmm0;
            
       // for x := 0 to w - 1;
       // prepare for reverse loop
       // variance = sum (x - mean)^2
       mov rax, r10;
       @addforxloop3:
           add rax, 128;
           jg @loopEnd2;
           // prefetch data...
           // prefetch [r8 + rax];

           // addition:
           movupd xmm1, [r8 + rax - 128];
           movupd xmm2, [r8 + rax - 112];
           subpd xmm1, xmm4;
           subpd xmm2, xmm4;
           mulpd xmm1, xmm1;
           mulpd xmm2, xmm2;
           addpd xmm0, xmm1;
           addpd xmm0, xmm2;
                
           movupd xmm1, [r8 + rax - 96];
           movupd xmm2, [r8 + rax - 80];
           subpd xmm1, xmm4;
           subpd xmm2, xmm4;
           mulpd xmm1, xmm1;
           mulpd xmm2, xmm2;
           addpd xmm0, xmm1;
           addpd xmm0, xmm2;
                
           movupd xmm1, [r8 + rax - 64];
           movupd xmm2, [r8 + rax - 48];
           subpd xmm1, xmm4;
           subpd xmm2, xmm4;
           mulpd xmm1, xmm1;
           mulpd xmm2, xmm2;
           addpd xmm0, xmm1;
           addpd xmm0, xmm2;
                
           movupd xmm1, [r8 + rax - 32];
           movupd xmm2, [r8 + rax - 16];
           subpd xmm1, xmm4;
           subpd xmm2, xmm4;
           mulpd xmm1, xmm1;
           mulpd xmm2, xmm2;
           addpd xmm0, xmm1;
           addpd xmm0, xmm2;
                
       jmp @addforxloop3

       @loopEnd2:

       sub rax, 128;

       jz @buildRes2;

       @addforxloop4:
           movupd xmm1, [r8 + rax];
           subpd xmm1, xmm4;
           mulpd xmm1, xmm1;
           addpd xmm0, xmm1;
       add rax, 16;
       jnz @addforxloop4;

       @buildRes2:

       // build result
       movhlps xmm1, xmm0;
       addsd xmm0, xmm1;
            
       // check if we need to use the unbiased version
       cmp unbiased, 0;
       jz @@dobiased;

       movsd xmm4, xmm5;
       subsd xmm4, cOne;
       maxsd xmm4, cOne;

       divsd xmm0, xmm4;

       jmp @@writeRes;

       @@dobiased:
       divsd xmm0, xmm5;
            
       // write result
       @@writeRes:
       movlpd [rcx], xmm0;

       // next line:
       add r8, r9;
       add rcx, rdx;

   // loop y end
   dec r11;
   jnz @@addforyloop;

   // epilog
   movapd xmm5, dXMM5;
{$IFDEF FPC}
end;
{$ENDIF}
end;

procedure ASMMatrixVarRowAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean);
var dXMM5 : Array[0..1] of double;
{$IFDEF FPC}
begin
  {$ENDIF}
asm
   // note: RCX = dest, RDX = destLineWidth, R8 = src, R9 = srcLineWidth

   // prolog
   movapd dXMM5, xmm5;
   {
   .savenv xmm5;
   }

   // iters := -width*sizeof(double)
   mov r10, width;
   dec r10;
   shl r10, 3;
   imul r10, -1;

   // helper registers for the mt1, mt2 and dest pointers
   sub r8, r10;

   cvtsi2sd xmm5, width;

   // for y := 0 to height - 1:
   mov r11, Height;
   @@addforyloop:
       xorpd xmm0, xmm0;
       xorpd xmm1, xmm1;
       xorpd xmm2, xmm2;
       xorpd xmm3, xmm3;

       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;
            // prefetch data...
           // prefetch [r8 + rax];

           // addition:
           addpd xmm0, [r8 + rax - 128];
           addpd xmm1, [r8 + rax - 112];
           addpd xmm1, [r8 + rax - 96];
           addpd xmm3, [r8 + rax - 80];
           addpd xmm0, [r8 + rax - 64];
           addpd xmm1, [r8 + rax - 48];
           addpd xmm2, [r8 + rax - 32];
           addpd xmm3, [r8 + rax - 16];
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @buildRes;

       @addforxloop2:
           addpd xmm0, [r8 + rax];
       add rax, 16;
       jnz @addforxloop2;

       @buildRes:

       addpd xmm0, xmm1;
       addpd xmm2, xmm3;
       addpd xmm0, xmm2;

       // handle last element differently
       movlpd xmm2, [r8 + rax];
       addsd xmm0, xmm2;

       // build result
       movhlps xmm1, xmm0;
       addsd xmm0, xmm1;

       divsd xmm0, xmm5;

       // we have calculated the mean -> 
       // repeat the loop to calculate the variance

       movddup xmm4, xmm0;
       xorpd xmm0, xmm0;
            
       // for x := 0 to w - 1;
       // prepare for reverse loop
       // variance = sum (x - mean)^2
       mov rax, r10;
       @addforxloop3:
           add rax, 128;
           jg @loopEnd2;
           // prefetch data...
           // prefetch [ecx + rax];

           // addition:
           movapd xmm1, [r8 + rax - 128];
           movapd xmm2, [r8 + rax - 112];
           subpd xmm1, xmm4;
           subpd xmm2, xmm4;
           mulpd xmm1, xmm1;
           mulpd xmm2, xmm2;
           addpd xmm0, xmm1;
           addpd xmm0, xmm2;
                
           movapd xmm1, [r8 + rax - 96];
           movapd xmm2, [r8 + rax - 80];
           subpd xmm1, xmm4;
           subpd xmm2, xmm4;
           mulpd xmm1, xmm1;
           mulpd xmm2, xmm2;
           addpd xmm0, xmm1;
           addpd xmm0, xmm2;
                
           movapd xmm1, [r8 + rax - 64];
           movapd xmm2, [r8 + rax - 48];
           subpd xmm1, xmm4;
           subpd xmm2, xmm4;
           mulpd xmm1, xmm1;
           mulpd xmm2, xmm2;
           addpd xmm0, xmm1;
           addpd xmm0, xmm2;
                
           movapd xmm1, [r8 + rax - 32];
           movapd xmm2, [r8 + rax - 16];
           subpd xmm1, xmm4;
           subpd xmm2, xmm4;
           mulpd xmm1, xmm1;
           mulpd xmm2, xmm2;
           addpd xmm0, xmm1;
           addpd xmm0, xmm2;
                
       jmp @addforxloop3

       @loopEnd2:

       sub rax, 128;

       jz @buildRes2;

       @addforxloop4:
           movapd xmm1, [r8 + rax];
           subpd xmm1, xmm4;
           mulpd xmm1, xmm1;
           addpd xmm0, xmm1;
       add rax, 16;
       jnz @addforxloop4;

       @buildRes2:

       // handle last element differently
       movlpd xmm1, [r8 + rax];
       subsd xmm1, xmm4;
       mulsd xmm1, xmm1;
       addsd xmm0, xmm1;

       // build result
       movhlps xmm1, xmm0;
       addsd xmm0, xmm1;
            
       // check if we need to use the unbiased version
       cmp unbiased, 0;
       jz @@dobiased;

       movsd xmm4, xmm5;
       subsd xmm4, cOne;
       maxsd xmm4, cOne;

       divsd xmm0, xmm4;

       jmp @@writeRes;

       @@dobiased:
       divsd xmm0, xmm5;
            
       // write result
       @@writeRes:
       movlpd [rcx], xmm0;

       // next line:
       add r8, r9;
       add rcx, rdx;

   // loop y end
   dec r11;
   jnz @@addforyloop;

   // epilog
   movapd xmm5, dXMM5;
{$IFDEF FPC}
end;
{$ENDIF}
end;


procedure ASMMatrixVarRowUnAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean);
var dXMM5, dXMM4 : Array[0..1] of double;
{$IFDEF FPC}
begin
  {$ENDIF}
asm
   // note: RCX = dest, RDX = destLineWidth, R8 = src, R9 = srcLineWidth

   // prolog
   movapd dXMM5, xmm5;
   movapd dXMM4, xmm4;
   {
   .savenv xmm5;
   .savenv xmm4;
   }

   // iters := -width*sizeof(double)
   mov r10, width;
   dec r10;
   shl r10, 3;
   imul r10, -1;

   // helper registers for the mt1, mt2 and dest pointers
   sub r8, r10;

   cvtsi2sd xmm5, width;

   // for y := 0 to height - 1:
   mov r11, Height;
   @@addforyloop:
       xorpd xmm0, xmm0;

       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // addition:
           movupd xmm1, [r8 + rax - 128];
           addpd xmm0, xmm1;

           movupd xmm2, [r8 + rax - 112];
           addpd xmm0, xmm2;

           movupd xmm3, [r8 + rax - 96];
           addpd xmm0, xmm3;

           movupd xmm4, [r8 + rax - 80];
           addpd xmm0, xmm4;

           movupd xmm1, [r8 + rax - 64];
           addpd xmm0, xmm1;

           movupd xmm2, [r8 + rax - 48];
           addpd xmm0, xmm2;

           movupd xmm3, [r8 + rax - 32];
           addpd xmm0, xmm3;

           movupd xmm4, [r8 + rax - 16];
           addpd xmm0, xmm4;
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @buildRes;

       @addforxloop2:
           movupd xmm1, [r8 + rax];
           addpd xmm0, xmm1;
       add rax, 16;
       jnz @addforxloop2;

       @buildRes:

       // handle last element differently
       movlpd xmm2, [r8 + rax];
       addsd xmm0, xmm2;

       // build result
       movhlps xmm1, xmm0;
       addsd xmm0, xmm1;

       divsd xmm0, xmm5;

       // we have calculated the mean -> 
       // repeat the loop to calculate the variance

       movddup xmm4, xmm0;
       xorpd xmm0, xmm0;
            
       // for x := 0 to w - 1;
       // prepare for reverse loop
       // variance = sum (x - mean)^2
       mov rax, r8;
       @addforxloop3:
           add rax, 128;
           jg @loopEnd2;
           // prefetch data...
           // prefetch [ecx + rax];

           // addition:
           movupd xmm1, [r8 + rax - 128];
           movupd xmm2, [r8 + rax - 112];
           subpd xmm1, xmm4;
           subpd xmm2, xmm4;
           mulpd xmm1, xmm1;
           mulpd xmm2, xmm2;
           addpd xmm0, xmm1;
           addpd xmm0, xmm2;
                
           movupd xmm1, [r8 + rax - 96];
           movupd xmm2, [r8 + rax - 80];
           subpd xmm1, xmm4;
           subpd xmm2, xmm4;
           mulpd xmm1, xmm1;
           mulpd xmm2, xmm2;
           addpd xmm0, xmm1;
           addpd xmm0, xmm2;
                
           movupd xmm1, [r8 + rax - 64];
           movupd xmm2, [r8 + rax - 48];
           subpd xmm1, xmm4;
           subpd xmm2, xmm4;
           mulpd xmm1, xmm1;
           mulpd xmm2, xmm2;
           addpd xmm0, xmm1;
           addpd xmm0, xmm2;
                
           movupd xmm1, [r8 + rax - 32];
           movupd xmm2, [r8 + rax - 16];
           subpd xmm1, xmm4;
           subpd xmm2, xmm4;
           mulpd xmm1, xmm1;
           mulpd xmm2, xmm2;
           addpd xmm0, xmm1;
           addpd xmm0, xmm2;
                
       jmp @addforxloop3

       @loopEnd2:

       sub rax, 128;

       jz @buildRes2;

       @addforxloop4:
           movupd xmm1, [r8 + rax];
           subpd xmm1, xmm4;
           mulpd xmm1, xmm1;
           addpd xmm0, xmm1;
       add rax, 16;
       jnz @addforxloop4;

       @buildRes2:

       // handle last element differently
       movlpd xmm1, [r8 + rax];
       subsd xmm1, xmm4;
       mulsd xmm1, xmm1;
       addsd xmm0, xmm1;

       // build result
       movhlps xmm1, xmm0;
       addsd xmm0, xmm1;
            
       // check if we need to use the unbiased version
       cmp unbiased, 0;
       jz @@dobiased;

       movsd xmm4, xmm5;
       subsd xmm4, cOne;
       maxsd xmm4, cOne;

       divsd xmm0, xmm4;

       jmp @@writeRes;

       @@dobiased:
       divsd xmm0, xmm5;
            
       // write result
       @@writeRes:
       movlpd [rcx], xmm4;

       // next line:
       add r8, r9;
       add rcx, rdx;

   // loop y end
   dec r11;
   jnz @@addforyloop;

   // epilog
   movapd xmm5, dXMM5;
   movapd xmm4, dXMM4;
{$IFDEF FPC}
end;
{$ENDIF}
end;

procedure ASMMatrixVarColumnAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean);
{$IFDEF FPC}
begin
  {$ENDIF}
asm
   // note: RCX = dest, RDX = destLineWidth, R8 = src, R9 = srcLineWidth
   xor r10, r10;
   sub r10, height;
   imul r10, srcLineWidth;

   // helper registers for the mt1, mt2 and dest pointers
   sub r8, r10;

   cvtsi2sd xmm0, height;
   movddup xmm2, xmm0;

   // for x := 0 to width - 1:
   mov r11, Width;
   sar r11, 1;
   @@addforxloop:
       xorpd xmm0, xmm0;

       // for y := 0 to height - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforyloop:
           addpd xmm0, [r8 + rax];
       add rax, r9;
       jnz @addforyloop;

       // build mean
       divpd xmm0, xmm2;

       // calculate final standard deviation
       // for y := 0 to height - 1;
       // prepare for reverse loop
       xorpd xmm4, xmm4;
       mov rax, r10;
       @addforyloop2:
           movapd xmm1, [r8 + rax];
           subpd xmm1, xmm0;
           mulpd xmm1, xmm1;
           addpd xmm4, xmm1;
       add rax, r9;
       jnz @addforyloop2;

       // check if we need to use the unbiased version
       cmp unbiased, 0;
       jz @@dobiased;

       movapd xmm3, xmm2;
       subpd xmm3, cOnes;
       maxpd xmm3, cOnes;

       divpd xmm4, xmm3;

       jmp @@writeRes;

       @@dobiased:
       divpd xmm4, xmm2;

       // write result
       @@writeRes:
       movapd [rcx], xmm4;

       // next columns:
       add rcx, 16;
       add r8, 16;

   // loop x end
   dec r11;
   jnz @@addforxloop;
{$IFDEF FPC}
end;
{$ENDIF}
end;

procedure ASMMatrixVarColumnUnAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean);
{$IFDEF FPC}
begin
  {$ENDIF}
asm
   // note: RCX = dest, RDX = destLineWidth, R8 = src, R9 = srcLineWidth
   xor r10, r10;
   sub r10, height;
   imul r10, srcLineWidth;

   // helper registers for the mt1, mt2 and dest pointers
   sub r8, r10;

   cvtsi2sd xmm0, height;
   movddup xmm2, xmm0;

   // for x := 0 to width - 1:
   mov r11, Width;
   sar r11, 1;
   @@addforxloop:
       xorpd xmm0, xmm0;

       // for y := 0 to height - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforyloop:
           movupd xmm1, [r8 + rax];
           addpd xmm0, xmm1;
       add rax, r9;
       jnz @addforyloop;

       // build mean
       divpd xmm0, xmm2;

       // calculate final standard deviation
       // for y := 0 to height - 1;
       // prepare for reverse loop
       xorpd xmm4, xmm4;
       mov rax, r10;
       @addforyloop2:
           movupd xmm1, [r8 + rax];
           subpd xmm1, xmm0;
           mulpd xmm1, xmm1;
           addpd xmm4, xmm1;
       add rax, r9;
       jnz @addforyloop2;

       // check if we need to use the unbiased version
       cmp unbiased, 0;
       jz @@dobiased;

       movapd xmm3, xmm2;
       subpd xmm3, cOnes;
       maxpd xmm3, cOnes;

       divpd xmm4, xmm3;

       jmp @@writeRes;

       @@dobiased:
       divpd xmm4, xmm2;

       // write result
       @@writeRes:
       movupd [rcx], xmm4;

       // next columns:
       add rcx, 16;
       add r8, 16;

   // loop x end
   dec r11;
   jnz @@addforxloop;
{$IFDEF FPC}
end;
{$ENDIF}
end;

procedure ASMMatrixVarColumnAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean);
{$IFDEF FPC}
begin
  {$ENDIF}
asm
   // note: RCX = dest, RDX = destLineWidth, R8 = src, R9 = srcLineWidth
   xor r10, r10;
   sub r10, height;
   imul r10, srcLineWidth;

   // helper registers for the mt1, mt2 and dest pointers
   sub r8, r10;

   cvtsi2sd xmm0, height;
   movddup xmm2, xmm0;

   // for x := 0 to width - 1:
   mov r11, Width;
   sar r11, 1;
   jz @lastColumn;
   @@addforxloop:
       xorpd xmm0, xmm0;

       // for y := 0 to height - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforyloop:
           addpd xmm0, [r8 + rax];
       add rax, r9;
       jnz @addforyloop;

       // build result
       divpd xmm0, xmm2;

       // calculate final standard deviation
       // for y := 0 to height - 1;
       // prepare for reverse loop
       xorpd xmm4, xmm4;
       mov rax, r10;
       @addforyloop2:
           movapd xmm1, [r8 + rax];
           subpd xmm1, xmm0;
           mulpd xmm1, xmm1;
           addpd xmm4, xmm1;
       add rax, r9;
       jnz @addforyloop2;

       // check if we need to use the unbiased version
       cmp unbiased, 0;
       jz @@dobiased;

       movapd xmm1, xmm2;
       subpd xmm1, cOnes;
       maxpd xmm1, cOnes;

       divpd xmm4, xmm1;

       jmp @@writeRes;

       @@dobiased:
       divpd xmm4, xmm2;

       // write result
       @@writeRes:
       movapd [rcx], xmm4;

       // next columns:
       add rcx, 16;
       add r8, 16;

   // loop x end
   dec r11;
   jnz @@addforxloop;

   @lastColumn:
   
   // handle last column
   xorpd xmm0, xmm0;

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov rax, r10;
   @addforyloop3:
       movlpd xmm1, [r8 + rax];
       addsd xmm0, xmm1;
   add rax, r9;
   jnz @addforyloop3;

   // build result
   divsd xmm0, xmm2;

   // calculate final standard deviation
   // for y := 0 to height - 1;
   // prepare for reverse loop
   xorpd xmm4, xmm4;
   mov rax, r10;
   @addforyloop4:
       movlpd xmm1, [r8 + rax];
       subsd xmm1, xmm0;
       mulsd xmm1, xmm1;
       addsd xmm4, xmm1;
   add rax, r9;
   jnz @addforyloop4;

   // check if we need to use the unbiased version
   cmp unbiased, 0;
   jz @@dobiased2;

   movapd xmm1, xmm2;
   subsd xmm1, cOne;
   maxsd xmm1, cOne;

   divsd xmm4, xmm1;

   jmp @@writeRes2;

   @@dobiased2:
   divsd xmm4, xmm2;

   // write result
   @@writeRes2:
   movlpd [rcx], xmm4;
{$IFDEF FPC}
end;
{$ENDIF}
end;

procedure ASMMatrixVarColumnUnAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; unbiased : boolean);
{$IFDEF FPC}
begin
  {$ENDIF}
asm
   // note: RCX = dest, RDX = destLineWidth, R8 = src, R9 = srcLineWidth
   xor r10, r10;
   sub r10, height;
   imul r10, srcLineWidth;

   // helper registers for the mt1, mt2 and dest pointers
   sub r8, r10;

   cvtsi2sd xmm0, height;
   movddup xmm2, xmm0;

   // for x := 0 to width - 1:
   mov r11, Width;
   sar r11, 1;
   jz @lastColumn;
   @@addforxloop:
       xorpd xmm0, xmm0;

       // for y := 0 to height - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforyloop:
           movupd xmm1, [r8 + rax];
           addpd xmm0, xmm1;
       add rax, r9;
       jnz @addforyloop;

       // build result
       divpd xmm0, xmm2;

       // calculate final standard deviation
       // for y := 0 to height - 1;
       // prepare for reverse loop
       xorpd xmm4, xmm4;
       mov rax, r10;
       @addforyloop2:
           movupd xmm1, [r8 + rax];
           subpd xmm1, xmm0;
           mulpd xmm1, xmm1;
           addpd xmm4, xmm1;
       add rax, r9;
       jnz @addforyloop2;

       // check if we need to use the unbiased version
       cmp unbiased, 0;
       jz @@dobiased;

       movapd xmm1, xmm2;
       subpd xmm1, cOnes;
       maxpd xmm1, cOnes;

       divpd xmm4, xmm1;

       jmp @@writeRes;

       @@dobiased:
       divpd xmm4, xmm2;

       // write result
       @@writeRes:
       movupd [rcx], xmm4;

       // next columns:
       add rcx, 16;
       add r8, 16;

   // loop x end
   dec r11;
   jnz @@addforxloop;

   @lastColumn:
   
   // handle last column
   xorpd xmm0, xmm0;

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov rax, r10;
   @addforyloop3:
       movlpd xmm1, [r8 + rax];
       addsd xmm0, xmm1;
   add rax, r9;
   jnz @addforyloop3;

   // build result
   divsd xmm0, xmm2;

   // calculate final standard deviation
   // for y := 0 to height - 1;
   // prepare for reverse loop
   xorpd xmm4, xmm4;
   mov rax, r10;
   @addforyloop4:
       movlpd xmm1, [r8 + rax];
       subsd xmm1, xmm0;
       mulsd xmm1, xmm1;
       addsd xmm4, xmm1;
   add rax, r9;
   jnz @addforyloop4;

   // check if we need to use the unbiased version
   cmp unbiased, 0;
   jz @@dobiased2;

   movapd xmm1, xmm2;
   subsd xmm1, cOne;
   maxsd xmm1, cOne;

   divsd xmm4, xmm1;

   jmp @@writeRes2;

   @@dobiased2:
   divsd xmm4, xmm2;

   // write result
   @@writeRes2:
   movlpd [rcx], xmm4;
{$IFDEF FPC}
end;
{$ENDIF}
end;

{$ENDIF}

end.
