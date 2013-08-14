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


unit ASMMatrixScaleOperationsx64;

interface

{$IFDEF CPUX64}

uses MatrixConst;

procedure ASMMatrixAddScaleAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);
procedure ASMMatrixAddScaleUnAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);

procedure ASMMatrixAddScaleAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);
procedure ASMMatrixAddScaleUnAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);

procedure ASMMatrixScaleAddAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);
procedure ASMMatrixScaleAddUnAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);

procedure ASMMatrixScaleAddAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);
procedure ASMMatrixScaleAddUnAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);

{$ENDIF}

implementation

{$IFDEF CPUX64}

procedure ASMMatrixAddScaleAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);
asm
	  // note: RCX = dest, RDX = destLineWidth, R8 = width, R9 = height
   .savenv xmm6;
   .savenv xmm7;

   //iters := -width*sizeof(double);
   mov r10, width;
   shl r10, 3;
   imul r10, -1;

   movddup xmm6, dOffset;
   movddup xmm7, Scale;

   // helper registers for the mt1, mt2 and dest pointers
   sub dest, r10;

   // for y := 0 to height - 1:
   mov r11, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // prefetchw data...
           prefetchw [rcx + rax];

           // add mul
           movapd xmm0, [rcx + rax - 128];
           addpd xmm0, xmm6;
           mulpd xmm0, xmm7;
           movapd [rcx + rax - 128], xmm0;

           movapd xmm1, [rcx + rax - 112];
           addpd xmm1, xmm6;
           mulpd xmm1, xmm7;
           movapd [rcx + rax - 112], xmm1;

           movapd xmm2, [rcx + rax - 96];
           addpd xmm2, xmm6;
           mulpd xmm2, xmm7;
           movapd [rcx + rax - 96], xmm2;

           movapd xmm3, [rcx + rax - 80];
           addpd xmm3, xmm6;
           mulpd xmm3, xmm7;
           movapd [rcx + rax - 80], xmm3;

           movapd xmm0, [rcx + rax - 64];
           addpd xmm0, xmm6;
           mulpd xmm0, xmm7;
           movapd [rcx + rax - 64], xmm0;

           movapd xmm1, [rcx + rax - 48];
           addpd xmm1, xmm6;
           mulpd xmm1, xmm7;
           movapd [rcx + rax - 48], xmm1;

           movapd xmm2, [rcx + rax - 32];
           addpd xmm2, xmm6;
           mulpd xmm2, xmm7;
           movapd [rcx + rax - 32], xmm2;

           movapd xmm3, [rcx + rax - 16];
           addpd xmm3, xmm6;
           mulpd xmm3, xmm7;
           movapd [rcx + rax - 16], xmm3;

       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           movapd xmm0, [rcx + rax];
           addpd xmm0, xmm6;
           mulpd xmm0, xmm7;

           movapd [rcx + rax], xmm0;
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // next line:
       add rcx, rdx;

   // loop y end
   dec r11;
   jnz @@addforyloop;
end;

procedure ASMMatrixAddScaleUnAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);
asm
	  // note: RCX = dest, RDX = destLineWidth, R8 = width, R9 = height
   .savenv xmm6;
   .savenv xmm7;

   //iters := -width*sizeof(double);
   mov r10, width;
   shl r10, 3;
   imul r10, -1;

   movddup xmm6, dOffset;
   movddup xmm7, Scale;

   // helper registers for the mt1, mt2 and dest pointers
   sub dest, r10;

   // for y := 0 to height - 1:
   mov r11, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // add mul:
           movupd xmm0, [rcx + rax - 128];
           addpd xmm0, xmm6;
           mulpd xmm0, xmm7;
           movupd [rcx + rax - 128], xmm0;

           movupd xmm1, [rcx + rax - 112];
           addpd xmm1, xmm6;
           mulpd xmm1, xmm7;
           movupd [rcx + rax - 112], xmm1;

           movupd xmm2, [rcx + rax - 96];
           addpd xmm2, xmm6;
           mulpd xmm2, xmm7;
           movupd [rcx + rax - 96], xmm2;

           movupd xmm3, [rcx + rax - 80];
           addpd xmm3, xmm6;
           mulpd xmm3, xmm7;
           movupd [rcx + rax - 80], xmm3;

           movupd xmm0, [rcx + rax - 64];
           addpd xmm0, xmm6;
           mulpd xmm0, xmm7;
           movupd [rcx + rax - 64], xmm0;

           movupd xmm1, [rcx + rax - 48];
           addpd xmm1, xmm6;
           mulpd xmm1, xmm7;
           movupd [rcx + rax - 48], xmm1;

           movupd xmm2, [rcx + rax - 32];
           addpd xmm2, xmm6;
           mulpd xmm2, xmm7;
           movupd [rcx + rax - 32], xmm2;

           movupd xmm3, [rcx + rax - 16];
           addpd xmm3, xmm6;
           mulpd xmm3, xmm7;
           movupd [rcx + rax - 16], xmm3;

       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           movupd xmm0, [rcx + rax];
           addpd xmm0, xmm6;
           mulpd xmm0, xmm7;

           movupd [rcx + rax], xmm0;
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // next line:
       add rcx, rdx;

   // loop y end
   dec r11;
   jnz @@addforyloop;
end;


procedure ASMMatrixAddScaleAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);
asm
	  // note: RCX = dest, RDX = destLineWidth, R8 = width, R9 = height
   .savenv xmm6;
   .savenv xmm7;

   //iters := -(width - 1)*sizeof(double);
   mov r10, width;
   dec r10;
   shl r10, 3;
   imul r10, -1;

   movddup xmm6, dOffset;
   movddup xmm7, Scale;

   // helper registers for the mt1, mt2 and dest pointers
   sub dest, r10;

   // for y := 0 to height - 1:
   mov r11, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // prefetchw data...
           prefetchw [rcx + rax];

           // add mul:
           movapd xmm0, [rcx + rax - 128];
           addpd xmm0, xmm6;
           mulpd xmm0, xmm7;
           movapd [rcx + rax - 128], xmm0;

           movapd xmm1, [rcx + rax - 112];
           addpd xmm1, xmm6;
           mulpd xmm1, xmm7;
           movapd [rcx + rax - 112], xmm1;

           movapd xmm2, [rcx + rax - 96];
           addpd xmm2, xmm6;
           mulpd xmm2, xmm7;
           movapd [rcx + rax - 96], xmm2;

           movapd xmm3, [rcx + rax - 80];
           addpd xmm3, xmm6;
           mulpd xmm3, xmm7;
           movapd [rcx + rax - 80], xmm3;

           movapd xmm0, [rcx + rax - 64];
           addpd xmm0, xmm6;
           mulpd xmm0, xmm7;
           movapd [rcx + rax - 64], xmm0;

           movapd xmm1, [rcx + rax - 48];
           addpd xmm1, xmm6;
           mulpd xmm1, xmm7;
           movapd [rcx + rax - 48], xmm1;

           movapd xmm2, [rcx + rax - 32];
           addpd xmm2, xmm6;
           mulpd xmm2, xmm7;
           movapd [rcx + rax - 32], xmm2;

           movapd xmm3, [rcx + rax - 16];
           addpd xmm3, xmm6;
           mulpd xmm3, xmm7;
           movapd [rcx + rax - 16], xmm3;

       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           movapd xmm0, [rcx + rax];
           addpd xmm0, xmm6;
           mulpd xmm0, xmm7;

           movapd [rcx + rax], xmm0;
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last column:
       movlpd xmm0, [rcx];
       addsd xmm0, xmm6;
       mulsd xmm0, xmm7;
       movlpd [rcx], xmm0;

       // next line:
       add rcx, rdx;

   // loop y end
   dec r11;
   jnz @@addforyloop;
end;

procedure ASMMatrixAddScaleUnAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);
asm
	  // note: RCX = dest, RDX = destLineWidth, R8 = width, R9 = height
   .savenv xmm6;
   .savenv xmm7;

   //iters := -(width - 1)*sizeof(double);
   mov r10, width;
   dec r10;
   shl r10, 3;
   imul r10, -1;

   movddup xmm6, dOffset;
   movddup xmm7, Scale;

   // helper registers for the mt1, mt2 and dest pointers
   sub dest, r10;

   // for y := 0 to height - 1:
   mov r11, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // add mul:
           movupd xmm0, [rcx + rax - 128];
           addpd xmm0, xmm6;
           mulpd xmm0, xmm7;
           movupd [rcx + rax - 128], xmm0;

           movupd xmm1, [rcx + rax - 112];
           addpd xmm1, xmm6;
           mulpd xmm1, xmm7;
           movupd [rcx + rax - 112], xmm1;

           movupd xmm2, [rcx + rax - 96];
           addpd xmm2, xmm6;
           mulpd xmm2, xmm7;
           movupd [rcx + rax - 96], xmm2;

           movupd xmm3, [rcx + rax - 80];
           addpd xmm3, xmm6;
           mulpd xmm3, xmm7;
           movupd [rcx + rax - 80], xmm3;

           movupd xmm0, [rcx + rax - 64];
           addpd xmm0, xmm6;
           mulpd xmm0, xmm7;
           movupd [rcx + rax - 64], xmm0;

           movupd xmm1, [rcx + rax - 48];
           addpd xmm1, xmm6;
           mulpd xmm1, xmm7;
           movupd [rcx + rax - 48], xmm1;

           movupd xmm2, [rcx + rax - 32];
           addpd xmm2, xmm6;
           mulpd xmm2, xmm7;
           movupd [rcx + rax - 32], xmm2;

           movupd xmm3, [rcx + rax - 16];
           addpd xmm3, xmm6;
           mulpd xmm3, xmm7;
           movupd [rcx + rax - 16], xmm3;

       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           movupd xmm0, [rcx + rax];
           addpd xmm0, xmm6;
           mulpd xmm0, xmm7;

           movupd [rcx + rax], xmm0;
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last column:
       movlpd xmm0, [rcx];
       addsd xmm0, xmm6;
       mulsd xmm0, xmm7;
       movlpd [rcx], xmm0;

       // next line:
       add rcx, rdx;

   // loop y end
   dec r11;
   jnz @@addforyloop;
end;


procedure ASMMatrixScaleAddAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);
asm
	  // note: RCX = dest, RDX = destLineWidth, R8 = width, R9 = height
   .savenv xmm6;
   .savenv xmm7;

   //iters := -width*sizeof(double);
   mov r10, width;
   shl r10, 3;
   imul r10, -1;

   movddup xmm6, dOffset;
   movddup xmm7, Scale;

   // helper registers for the mt1, mt2 and dest pointers
   sub dest, r10;

   // for y := 0 to height - 1:
   mov r11, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // prefetchw data...
           prefetchw [rcx + rax];

           // addition:
           movapd xmm0, [rcx + rax - 128];
           mulpd xmm0, xmm7;
           addpd xmm0, xmm6;
           movapd [rcx + rax - 128], xmm0;

           movapd xmm1, [rcx + rax - 112];
           mulpd xmm1, xmm7;
           addpd xmm1, xmm6;
           movapd [rcx + rax - 112], xmm1;

           movapd xmm2, [rcx + rax - 96];
           mulpd xmm2, xmm7;
           addpd xmm2, xmm6;
           movapd [rcx + rax - 96], xmm2;

           movapd xmm3, [rcx + rax - 80];
           mulpd xmm3, xmm7;
           addpd xmm3, xmm6;
           movapd [rcx + rax - 80], xmm3;

           movapd xmm0, [rcx + rax - 64];
           mulpd xmm0, xmm7;
           addpd xmm0, xmm6;
           movapd [rcx + rax - 64], xmm0;

           movapd xmm1, [rcx + rax - 48];
           mulpd xmm1, xmm7;
           addpd xmm1, xmm6;
           movapd [rcx + rax - 48], xmm1;

           movapd xmm2, [rcx + rax - 32];
           mulpd xmm2, xmm7;
           addpd xmm2, xmm6;
           movapd [rcx + rax - 32], xmm2;

           movapd xmm3, [rcx + rax - 16];
           mulpd xmm3, xmm7;
           addpd xmm3, xmm6;
           movapd [rcx + rax - 16], xmm3;

       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           movapd xmm0, [rcx + rax];
           mulpd xmm0, xmm7;
           addpd xmm0, xmm6;

           movapd [rcx + rax], xmm0;
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // next line:
       add rcx, rdx;

   // loop y end
   dec r11;
   jnz @@addforyloop;
end;

procedure ASMMatrixScaleAddUnAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);
asm
	  // note: RCX = dest, RDX = destLineWidth, R8 = width, R9 = height
   .savenv xmm6;
   .savenv xmm7;

   //iters := -width*sizeof(double);
   mov r10, width;
   shl r10, 3;
   imul r10, -1;

   movddup xmm6, dOffset;
   movddup xmm7, Scale;

   // helper registers for the mt1, mt2 and dest pointers
   sub dest, r10;

   // for y := 0 to height - 1:
   mov r11, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // mul add:
           movupd xmm0, [rcx + rax - 128];
           mulpd xmm0, xmm7;
           addpd xmm0, xmm6;
           movupd [rcx + rax - 128], xmm0;

           movupd xmm1, [rcx + rax - 112];
           mulpd xmm1, xmm7;
           addpd xmm1, xmm6;
           movupd [rcx + rax - 112], xmm1;

           movupd xmm2, [rcx + rax - 96];
           mulpd xmm2, xmm7;
           addpd xmm2, xmm6;
           movupd [rcx + rax - 96], xmm2;

           movupd xmm3, [rcx + rax - 80];
           mulpd xmm3, xmm7;
           addpd xmm3, xmm6;
           movupd [rcx + rax - 80], xmm3;

           movupd xmm0, [rcx + rax - 64];
           mulpd xmm0, xmm7;
           addpd xmm0, xmm6;
           movupd [rcx + rax - 64], xmm0;

           movupd xmm1, [rcx + rax - 48];
           mulpd xmm1, xmm7;
           addpd xmm1, xmm6;
           movupd [rcx + rax - 48], xmm1;

           movupd xmm2, [rcx + rax - 32];
           mulpd xmm2, xmm7;
           addpd xmm2, xmm6;
           movupd [rcx + rax - 32], xmm2;

           movupd xmm3, [rcx + rax - 16];
           mulpd xmm3, xmm7;
           addpd xmm3, xmm6;
           movupd [rcx + rax - 16], xmm3;

       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           movupd xmm0, [rcx + rax];
           mulpd xmm0, xmm7;
           addpd xmm0, xmm6;

           movupd [rcx + rax], xmm0;
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // next line:
       add rcx, rdx;

   // loop y end
   dec r11;
   jnz @@addforyloop;
end;


procedure ASMMatrixScaleAddAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);
asm
	  // note: RCX = dest, RDX = destLineWidth, R8 = width, R9 = height
   .savenv xmm6;
   .savenv xmm7;

   //iters := -width*sizeof(double);
   mov r10, width;
   dec r10;
   shl r10, 3;
   imul r10, -1;

   movddup xmm6, dOffset;
   movddup xmm7, Scale;

   // helper registers for the mt1, mt2 and dest pointers
   sub dest, r10;

   // for y := 0 to height - 1:
   mov r11, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // prefetchw data...
           prefetchw [rcx + rax];

           // addition:
           movapd xmm0, [rcx + rax - 128];
           mulpd xmm0, xmm7;
           addpd xmm0, xmm6;
           movapd [rcx + rax - 128], xmm0;

           movapd xmm1, [rcx + rax - 112];
           mulpd xmm1, xmm7;
           addpd xmm1, xmm6;
           movapd [rcx + rax - 112], xmm1;

           movapd xmm2, [rcx + rax - 96];
           mulpd xmm2, xmm7;
           addpd xmm2, xmm6;
           movapd [rcx + rax - 96], xmm2;

           movapd xmm3, [rcx + rax - 80];
           mulpd xmm3, xmm7;
           addpd xmm3, xmm6;
           movapd [rcx + rax - 80], xmm3;

           movapd xmm0, [rcx + rax - 64];
           mulpd xmm0, xmm7;
           addpd xmm0, xmm6;
           movapd [rcx + rax - 64], xmm0;

           movapd xmm1, [rcx + rax - 48];
           mulpd xmm1, xmm7;
           addpd xmm1, xmm6;
           movapd [rcx + rax - 48], xmm1;

           movapd xmm2, [rcx + rax - 32];
           mulpd xmm2, xmm7;
           addpd xmm2, xmm6;
           movapd [rcx + rax - 32], xmm2;

           movapd xmm3, [rcx + rax - 16];
           mulpd xmm3, xmm7;
           addpd xmm3, xmm6;
           movapd [rcx + rax - 16], xmm3;

       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           movapd xmm0, [rcx + rax];
           mulpd xmm0, xmm7;
           addpd xmm0, xmm6;

           movapd [rcx + rax], xmm0;
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last column:
       movlpd xmm0, [rcx];
       addsd xmm0, xmm6;
       mulsd xmm0, xmm7;
       movlpd [rcx], xmm0;

       // next line:
       add rcx, rdx;

   // loop y end
   dec r11;
   jnz @@addforyloop;
end;

procedure ASMMatrixScaleAddUnAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);
asm
	  // note: RCX = dest, RDX = destLineWidth, R8 = width, R9 = height
   .savenv xmm6;
   .savenv xmm7;

   //iters := -width*sizeof(double);
   mov r10, width;
   dec r10;
   shl r10, 3;
   imul r10, -1;

   movddup xmm6, dOffset;
   movddup xmm7, Scale;

   // helper registers for the mt1, mt2 and dest pointers
   sub dest, r10;

   // for y := 0 to height - 1:
   mov r11, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // mul add:
           movupd xmm0, [rcx + rax - 128];
           mulpd xmm0, xmm7;
           addpd xmm0, xmm6;
           movupd [rcx + rax - 128], xmm0;

           movupd xmm1, [rcx + rax - 112];
           mulpd xmm1, xmm7;
           addpd xmm1, xmm6;
           movupd [rcx + rax - 112], xmm1;

           movupd xmm2, [rcx + rax - 96];
           mulpd xmm2, xmm7;
           addpd xmm2, xmm6;
           movupd [rcx + rax - 96], xmm2;

           movupd xmm3, [rcx + rax - 80];
           mulpd xmm3, xmm7;
           addpd xmm3, xmm6;
           movupd [rcx + rax - 80], xmm3;

           movupd xmm0, [rcx + rax - 64];
           mulpd xmm0, xmm7;
           addpd xmm0, xmm6;
           movupd [rcx + rax - 64], xmm0;

           movupd xmm1, [rcx + rax - 48];
           mulpd xmm1, xmm7;
           addpd xmm1, xmm6;
           movupd [rcx + rax - 48], xmm1;

           movupd xmm2, [rcx + rax - 32];
           mulpd xmm2, xmm7;
           addpd xmm2, xmm6;
           movupd [rcx + rax - 32], xmm2;

           movupd xmm3, [rcx + rax - 16];
           mulpd xmm3, xmm7;
           addpd xmm3, xmm6;
           movupd [rcx + rax - 16], xmm3;

       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           movupd xmm0, [rcx + rax];
           mulpd xmm0, xmm7;
           addpd xmm0, xmm6;

           movupd [rcx + rax], xmm0;
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last column:
       movlpd xmm0, [rcx];
       addsd xmm0, xmm6;
       mulsd xmm0, xmm7;
       movlpd [rcx], xmm0;

       // next line:
       add rcx, rdx;

   // loop y end
   dec r11;
   jnz @@addforyloop;
end;

{$ENDIF}

end.
