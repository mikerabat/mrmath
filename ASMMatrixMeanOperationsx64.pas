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

uses MatrixConst;

procedure ASMMatrixMeanRowAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
procedure ASMMatrixMeanRowUnAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
procedure ASMMatrixMeanRowAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
procedure ASMMatrixMeanRowUnAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);

procedure ASMMatrixMeanColumnAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
procedure ASMMatrixMeanColumnUnAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
procedure ASMMatrixMeanColumnAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
procedure ASMMatrixMeanColumnUnAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);

{$ENDIF}

implementation

{$IFDEF CPUX64}


procedure ASMMatrixMeanRowAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
asm
	  // note: RCX = dest, RDX = destLineWidth, R8 = src, R9 = srcLineWidth
   .savenv xmm5;

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
end;

procedure ASMMatrixMeanRowUnAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
asm
	  // note: RCX = dest, RDX = destLineWidth, R8 = src, R9 = srcLineWidth
   .savenv xmm4;
   .savenv xmm5;

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
end;

procedure ASMMatrixMeanRowAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
asm
	  // note: RCX = dest, RDX = destLineWidth, R8 = src, R9 = srcLineWidth
   .savenv xmm5;

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
end;

procedure ASMMatrixMeanRowUnAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
asm
	  // note: RCX = dest, RDX = destLineWidth, R8 = src, R9 = srcLineWidth
   .savenv xmm4;
   .savenv xmm5;

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
end;

procedure ASMMatrixMeanColumnAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
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
end;

procedure ASMMatrixMeanColumnUnAlignedEvenW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
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
end;

procedure ASMMatrixMeanColumnAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
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
end;

procedure ASMMatrixMeanColumnUnAlignedOddW(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
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
end;

{$ENDIF}

end.
