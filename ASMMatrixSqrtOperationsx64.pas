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


unit ASMMatrixSqrtOperationsx64;

// #####################################################
// #### SQRT opertaion applied to every element in a matrix
// #####################################################

interface

{$I 'mrMath_CPU.inc'}

{$IFDEF x64}

uses MatrixConst;

procedure ASMMatrixSQRTAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}
procedure ASMMatrixSQRTUnAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}

procedure ASMMatrixSQRTAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}
procedure ASMMatrixSQRTUnAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}

{$ENDIF}

implementation

{$IFDEF x64}

procedure ASMMatrixSQRTAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}
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
           sqrtpd xmm0, [rcx + rax - 128];
           movapd [rcx + rax - 128], xmm0;

           sqrtpd xmm1, [rcx + rax - 112];
           movapd [rcx + rax - 112], xmm1;

           sqrtpd xmm2, [rcx + rax - 96];
           movapd [rcx + rax - 96], xmm2;

           sqrtpd xmm3, [rcx + rax - 80];
           movapd [rcx + rax - 80], xmm3;

           sqrtpd xmm0, [rcx + rax - 64];
           movapd [rcx + rax - 64], xmm0;

           sqrtpd xmm1, [rcx + rax - 48];
           movapd [rcx + rax - 48], xmm1;

           sqrtpd xmm2, [rcx + rax - 32];
           movapd [rcx + rax - 32], xmm2;

           sqrtpd xmm3, [rcx + rax - 16];
           movapd [rcx + rax - 16], xmm3;

       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           sqrtpd xmm0, [rcx + rax];
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

procedure ASMMatrixSQRTUnAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}
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

           // elementwise sqrt
           movupd xmm0, [rcx + rax - 128];
           sqrtpd xmm0, xmm0;
           movupd [rcx + rax - 128], xmm0;

           movupd xmm1, [rcx + rax - 112];
           sqrtpd xmm1, xmm1;
           movupd [rcx + rax - 112], xmm1;

           movupd xmm2, [rcx + rax - 96];
           sqrtpd xmm2, xmm2;
           movupd [rcx + rax - 96], xmm2;

           movupd xmm3, [rcx + rax - 80];
           sqrtpd xmm3, xmm3;
           movupd [rcx + rax - 80], xmm3;

           movupd xmm0, [rcx + rax - 64];
           sqrtpd xmm0, xmm0;
           movupd [rcx + rax - 64], xmm0;

           movupd xmm1, [rcx + rax - 48];
           sqrtpd xmm1, xmm1;
           movupd [rcx + rax - 48], xmm1;

           movupd xmm2, [rcx + rax - 32];
           sqrtpd xmm2, xmm2;
           movupd [rcx + rax - 32], xmm2;

           movupd xmm3, [rcx + rax - 16];
           sqrtpd xmm3, xmm3;
           movupd [rcx + rax - 16], xmm3;

       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           movupd xmm0, [rcx + rax];
           sqrtpd xmm0, xmm0;
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

procedure ASMMatrixSQRTAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}
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

   // note: RCX = dest, RDX = destLineWidth, R8 = width, R9 = height
   //iters := -(width - 1)*sizeof(double);
   mov r10, width;
   dec r10;
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
           sqrtpd xmm0, [rcx + rax - 128];
           movapd [rcx + rax - 128], xmm0;

           sqrtpd xmm1, [rcx + rax - 112];
           movapd [rcx + rax - 112], xmm1;

           sqrtpd xmm2, [rcx + rax - 96];
           movapd [rcx + rax - 96], xmm2;

           sqrtpd xmm3, [rcx + rax - 80];
           movapd [rcx + rax - 80], xmm3;

           sqrtpd xmm0, [rcx + rax - 64];
           movapd [rcx + rax - 64], xmm0;

           sqrtpd xmm1, [rcx + rax - 48];
           movapd [rcx + rax - 48], xmm1;

           sqrtpd xmm2, [rcx + rax - 32];
           movapd [rcx + rax - 32], xmm2;

           sqrtpd xmm3, [rcx + rax - 16];
           movapd [rcx + rax - 16], xmm3;

       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           sqrtpd xmm0, [rcx + rax];
           movapd [rcx + rax], xmm0;
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last element
       movsd xmm0, [rcx + rax];
       sqrtsd xmm0, xmm0;
       movsd [rcx + rax], xmm0;

       // next line:
       add rcx, rdx;

   // loop y end
   dec r11;
   jnz @@addforyloop;

end;

procedure ASMMatrixSQRTUnAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}
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

           // elementwise sqrt
           movupd xmm0, [rcx + rax - 128];
           sqrtpd xmm0, xmm0;
           movupd [rcx + rax - 128], xmm0;

           movupd xmm1, [rcx + rax - 112];
           sqrtpd xmm1, xmm1;
           movupd [rcx + rax - 112], xmm1;

           movupd xmm2, [rcx + rax - 96];
           sqrtpd xmm2, xmm2;
           movupd [rcx + rax - 96], xmm2;

           movupd xmm3, [rcx + rax - 80];
           sqrtpd xmm3, xmm3;
           movupd [rcx + rax - 80], xmm3;

           movupd xmm0, [rcx + rax - 64];
           sqrtpd xmm0, xmm0;
           movupd [rcx + rax - 64], xmm0;

           movupd xmm1, [rcx + rax - 48];
           sqrtpd xmm1, xmm1;
           movupd [rcx + rax - 48], xmm1;

           movupd xmm2, [rcx + rax - 32];
           sqrtpd xmm2, xmm2;
           movupd [rcx + rax - 32], xmm2;

           movupd xmm3, [rcx + rax - 16];
           sqrtpd xmm3, xmm3;
           movupd [rcx + rax - 16], xmm3;

       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           movupd xmm0, [rcx + rax];
           sqrtpd xmm0, xmm0;
           movupd [rcx + rax], xmm0;
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last element
       movsd xmm0, [rcx + rax];
       sqrtsd xmm0, xmm0;
       movsd [rcx + rax], xmm0;

       // next line:
       add rcx, rdx;

   // loop y end
   dec r11;
   jnz @@addforyloop;
end;

{$ENDIF}

end.
