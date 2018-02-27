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

unit ASMMatrixCumSumDiffOperationsx64;

interface


{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF x64}

uses MatrixConst;

procedure ASMMatrixCumulativeSumRow(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);

procedure ASMMatrixCumulativeSumColumnEvenWUnaligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
procedure ASMMatrixCumulativeSumColumnOddWUnaligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
procedure ASMMatrixCumulativeSumColumnEvenWAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
procedure ASMMatrixCumulativeSumColumnOddWAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);

procedure ASMMatrixDifferentiateRow(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);

procedure ASMMatrixDifferentiateColumnEvenWUnaligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
procedure ASMMatrixDifferentiateColumnOddWUnaligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
procedure ASMMatrixDifferentiateColumnEvenWAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
procedure ASMMatrixDifferentiateColumnOddWAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);

{$ENDIF}

implementation

{$IFDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$S-} {$ENDIF}

// ##################################################
// #### Cumulative sum
// ##################################################

procedure ASMMatrixCumulativeSumRow(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
var iRBX, iRDI, iRSI : TASMNativeint;
{$IFDEF FPC}
begin
{$ENDIF}
     // rcx: dest, rdx: destlinewidth; r8: src; r9 : srclinewidth
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

        // prolog - maintain stack
        mov iRBX, rbx;
        mov iRDI, rdi;
        mov iRSI, rsi;

        // if (width <= 0) or (height <= 0) then exit;
        mov rbx, width;
        cmp rbx, 0;
        jle @@exitproc;
        mov rsi, height;
        cmp rsi, 0;
        jle @@exitproc;

        // iter := -width*sizeof(Double)
        imul rbx, -8;

        // prepare counters
        sub rcx, rbx;
        sub r8, rbx;

        @@foryloop:
           mov rax, rbx;
           xorpd xmm0, xmm0;

           @@forxloop:
              movsd xmm1, [r8 + rax];
              addsd xmm0, xmm1;
              movsd [rcx + rax], xmm0;
           add rax, 8;
           jnz @@forxloop;

           add rcx, rdx;
           add r8, r9;
        dec rsi;
        jnz @@foryloop;

        @@exitProc:

        // epilog - stack cleanup
        mov rbx, iRBX;
        mov rdi, iRDI;
        mov rsi, iRSI;
     end;
{$IFDEF FPC}
end;
{$ENDIF}


procedure ASMMatrixCumulativeSumColumnEvenWUnaligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
var iRDI, iRSI : TASMNativeint;
{$IFDEF FPC}
begin
{$ENDIF}
     // rcx: dest, rdx: destlinewidth; r8: src; r9 : srclinewidth
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

        // prolog - maintain stack
        mov iRDI, rdi;
        mov iRSI, rsi;

        // if (width <= 1) or (height <= 0) then exit;
        mov r11, height;
        cmp r11, 0;
        jle @@exitproc;
        mov r10, width;
        cmp r10, 1;
        jle @@exitproc;

        sar r10, 1;  // width div 2

        @@forxloop:
           mov rax, r11;
           xorpd xmm0, xmm0;
           xor rdi, rdi;
           xor rsi, rsi;

           // two values at once
           @@foryloop:
              movupd xmm1, [r8 + rdi];
              addpd xmm0, xmm1;
              movupd [rcx + rsi], xmm0;

              add rdi, r9;
              add rsi, rdx;
           dec rax;
           jnz @@foryloop;

           add r8, 16;
           add rcx, 16;
        dec r10;
        jnz @@forxloop;

        @@exitProc:
        // epilog - stack cleanup
        mov rdi, iRDI;
        mov rsi, iRSI;
     end;
{$IFDEF FPC}
end;
{$ENDIF}

procedure ASMMatrixCumulativeSumColumnOddWUnaligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
var iRDI, iRSI : TASMNativeint;
{$IFDEF FPC}
begin
{$ENDIF}
     // rcx: dest, rdx: destlinewidth; r8: src; r9 : srclinewidth
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
        // prolog - maintain stack
        mov iRDI, rdi;
        mov iRSI, rsi;

        // if (width <= 1) or (height <= 0) then exit;
        mov r11, height;
        cmp r11, 0;
        jle @@exitproc;
        mov r10, width;
        cmp r10, 1;
        jle @@exitproc;

        sar r10, 1;  // width div 2

        test r10, r10;
        jz @@lastColumn;

        @@forxloop:
           mov rax, r11;
           xorpd xmm0, xmm0;
           xor rdi, rdi;
           xor rsi, rsi;

           // two values at once
           @@foryloop:
              movupd xmm1, [r8 + rdi];
              addpd xmm0, xmm1;
              movupd [rcx + rsi], xmm0;

              add rdi, r9;
              add rsi, rdx;
           dec rax;
           jnz @@foryloop;

           add r8, 16;
           add rcx, 16;
        dec r10;
        jnz @@forxloop;

        @@lastColumn:

        mov rax, r11;
        xorpd xmm0, xmm0;

        // last column
        @@forycolumnloop:
           movsd xmm1, [r8];
           addsd xmm0, xmm1;
           movsd [rcx], xmm0;

           add r8, r9;
           add rcx, rdx;
        dec rax;
        jnz @@forycolumnloop;

        @@exitProc:
        // epilog - stack cleanup
        mov rdi, iRDI;
        mov rsi, iRSI;
     end;
{$IFDEF FPC}
end;
{$ENDIF}


procedure ASMMatrixCumulativeSumColumnEvenWAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
var iRDI, iRSI : TASMNativeint;
{$IFDEF FPC}
begin
{$ENDIF}
     // rcx: dest, rdx: destlinewidth; r8: src; r9 : srclinewidth
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
        // prolog - maintain stack
        mov iRDI, rdi;
        mov iRSI, rsi;

        // if (width <= 1) or (height <= 0) then exit;
        mov r11, height;
        cmp r11, 0;
        jle @@exitproc;
        mov r10, width;
        cmp r10, 1;
        jle @@exitproc;

        sar r10, 1;  // width div 2

        @@forxloop:
           mov rax, r11;
           xorpd xmm0, xmm0;
           xor rdi, rdi;
           xor rsi, rsi;

           // two values at once
           @@foryloop:
              movapd xmm1, [r8 + rdi];
              addpd xmm0, xmm1;
              movapd [rcx + rsi], xmm0;

              add rdi, r9;
              add rsi, rdx;
           dec rax;
           jnz @@foryloop;

           add r8, 16;
           add rcx, 16;
        dec r10;
        jnz @@forxloop;

        @@exitProc:
        // epilog - stack cleanup
        mov rdi, iRDI;
        mov rsi, iRSI;
     end;
{$IFDEF FPC}
end;
{$ENDIF}

procedure ASMMatrixCumulativeSumColumnOddWAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
var iRDI, iRSI : TASMNativeint;
{$IFDEF FPC}
begin
{$ENDIF}
     // rcx: dest, rdx: destlinewidth; r8: src; r9 : srclinewidth
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
        // prolog - maintain stack
        mov iRDI, rdi;
        mov iRSI, rsi;

        // if (width <= 1) or (height <= 0) then exit;
        mov r11, height;
        cmp r11, 0;
        jle @@exitproc;
        mov r10, width;
        cmp r10, 1;
        jle @@exitproc;

        sar r10, 1;  // width div 2

        test r10, r10;
        jz @@lastColumn;

        @@forxloop:
           mov rax, r11;
           xorpd xmm0, xmm0;
           xor rdi, rdi;
           xor rsi, rsi;

           // two values at once
           @@foryloop:
              movapd xmm1, [r8 + rdi];
              addpd xmm0, xmm1;
              movapd [rcx + rsi], xmm0;

              add rdi, r9;
              add rsi, rdx;
           dec rax;
           jnz @@foryloop;

           add r8, 16;
           add rcx, 16;
        dec r10;
        jnz @@forxloop;

        @@lastColumn:

        mov rax, r11;
        xorpd xmm0, xmm0;

        // last column
        @@forycolumnloop:
           movsd xmm1, [r8];
           addsd xmm0, xmm1;
           movsd [rcx], xmm0;

           add r8, r9;
           add rcx, rdx;
        dec rax;
        jnz @@forycolumnloop;

        @@exitProc:
        // epilog - stack cleanup
        mov rdi, iRDI;
        mov rsi, iRSI;
     end;
{$IFDEF FPC}
end;
{$ENDIF}

// ###############################################
// #### Differentiate
// ###############################################

procedure ASMMatrixDifferentiateRow(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
var iRBX, iRDI, iRSI : TASMNativeint;
{$IFDEF FPC}
begin
{$ENDIF}
     // rcx: dest, rdx: destlinewidth; r8: src; r9 : srclinewidth
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
        // prolog - maintain stack
        mov iRBX, rbx;
        mov iRDI, rdi;
        mov iRSI, rsi;

        // if (width <= 1) or (height <= 0) then exit;
        mov rbx, width;
        cmp rbx, 1;
        jle @@exitproc;
        mov rsi, height;
        cmp rsi, 0;
        jle @@exitproc;

        // iter := -width*sizeof(Double)
        imul rbx, -8;

        // prepare counters
        sub rcx, rbx;
        sub r8, rbx;
        add rbx, 8;

        @@foryloop:
           mov rax, rbx;
           movsd xmm1, [r8 + rax - 8];

           @@forxloop:
              movsd xmm0, [r8 + rax];
              movsd xmm2, xmm0;

              subsd xmm1, xmm0;
              movsd [rcx + rax - 8], xmm0;
              movsd xmm1, xmm2;
           add rax, 8;
           jnz @@forxloop;

           add rcx, rdx;
           add r8, r9;
        dec rsi;
        jnz @@foryloop;

        @@exitProc:

        // epilog - stack cleanup
        mov rbx, iRBX;
        mov rdi, iRDI;
        mov rsi, iRSI;
     end;
{$IFDEF FPC}
end;
{$ENDIF}

procedure ASMMatrixDifferentiateColumnEvenWUnaligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
var iRDI, iRSI : TASMNativeint;
{$IFDEF FPC}
begin
{$ENDIF}
     // rcx: dest, rdx: destlinewidth; r8: src; r9 : srclinewidth
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
        // prolog - maintain stack
        mov iRDI, rdi;
        mov iRSI, rsi;

        // if (width <= 0) or (height <= 1) then exit;
        mov r11, height;
        cmp r11, 1;
        jle @@exitproc;
        mov r10, width;
        cmp r10, 0;
        jle @@exitproc;

        dec r11;
        sar r10, 1;  // width div 2

        @@forxloop:
           mov rax, r11;
           xorpd xmm0, xmm0;
           mov rdi, r9;
           xor rsi, rsi;

           movupd xmm0, [r8];

           // two values at once
           @@foryloop:
              movupd xmm1, [r8 + rdi];
              movapd xmm2, xmm1;
              subpd xmm1, xmm0;
              movupd [rcx + rsi], xmm1;

              movapd xmm0, xmm2;

              add rdi, r9;
              add rsi, rdx;
           dec rax;
           jnz @@foryloop;

           add r8, 16;
           add rcx, 16;
        dec r10;
        jnz @@forxloop;

        @@exitProc:
        // epilog - stack cleanup
        mov rdi, iRDI;
        mov rsi, iRSI;
     end;
{$IFDEF FPC}
end;
{$ENDIF}

procedure ASMMatrixDifferentiateColumnOddWUnaligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
var iRDI, iRSI : TASMNativeint;
{$IFDEF FPC}
begin
{$ENDIF}
     // rcx: dest, rdx: destlinewidth; r8: src; r9 : srclinewidth
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
        // prolog - maintain stack
        mov iRDI, rdi;
        mov iRSI, rsi;

        // if (width <= 0) or (height <= 1) then exit;
        mov r11, height;
        cmp r11, 1;
        jle @@exitproc;
        mov r10, width;
        cmp r10, 0;
        jle @@exitproc;

        dec r11;     // height - 1

        sar r10, 1;  // width div 2

        test r10, r10;
        jz @@lastColumn;

        @@forxloop:
           mov rax, r11;
           xorpd xmm0, xmm0;
           mov rdi, r9;
           xor rsi, rsi;

           movupd xmm0, [r8];
           // two values at once
           @@foryloop:
              movupd xmm1, [r8 + rdi];
              movapd xmm2, xmm1;
              subpd xmm1, xmm0;
              movupd [rcx + rsi], xmm1;

              movapd xmm0, xmm2;
              add rdi, r9;
              add rsi, rdx;
           dec rax;
           jnz @@foryloop;

           add r8, 16;
           add rcx, 16;
        dec r10;
        jnz @@forxloop;

        @@lastColumn:

        mov rax, r11;

        movsd xmm0, [r8];
        add r8, r9;

        // last column
        @@forycolumnloop:
           movsd xmm1, [r8];
           movsd xmm2, xmm1;
           subsd xmm1, xmm0;
           movsd [rcx], xmm1;
           movsd xmm0, xmm2;

           add r8, r9;
           add rcx, rdx;
        dec rax;
        jnz @@forycolumnloop;

        @@exitProc:
        // epilog - stack cleanup
        mov rdi, iRDI;
        mov rsi, iRSI;
     end;
{$IFDEF FPC}
end;
{$ENDIF}

procedure ASMMatrixDifferentiateColumnEvenWAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
var iRDI, iRSI : TASMNativeint;
{$IFDEF FPC}
begin
{$ENDIF}
     // rcx: dest, rdx: destlinewidth; r8: src; r9 : srclinewidth
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
        // prolog - maintain stack
        mov iRDI, rdi;
        mov iRSI, rsi;

        // if (width <= 0) or (height <= 1) then exit;
        mov r11, height;
        cmp r11, 1;
        jle @@exitproc;
        mov r10, width;
        cmp r10, 0;
        jle @@exitproc;

        dec r11;     // height - 1
        sar r10, 1;  // width div 2

        @@forxloop:
           mov rax, r11;
           xorpd xmm0, xmm0;
           mov rdi, r9;
           xor rsi, rsi;

           movapd xmm0, [r8];

           // two values at once
           @@foryloop:
              movapd xmm1, [r8 + rdi];
              movapd xmm2, xmm1;
              subpd xmm1, xmm0;
              movapd [rcx + rsi], xmm1;

              movapd xmm0, xmm2;

              add rdi, r9;
              add rsi, rdx;
           dec rax;
           jnz @@foryloop;

           add r8, 16;
           add rcx, 16;
        dec r10;
        jnz @@forxloop;

        @@exitProc:
        // epilog - stack cleanup
        mov rdi, iRDI;
        mov rsi, iRSI;
     end;
{$IFDEF FPC}
end;
{$ENDIF}


procedure ASMMatrixDifferentiateColumnOddWAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
var iRDI, iRSI : TASMNativeint;
{$IFDEF FPC}
begin
{$ENDIF}
     // rcx: dest, rdx: destlinewidth; r8: src; r9 : srclinewidth
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
        // prolog - maintain stack
        mov iRDI, rdi;
        mov iRSI, rsi;

        // if (width <= 0) or (height <= 1) then exit;
        mov r11, height;
        cmp r11, 1;
        jle @@exitproc;
        mov r10, width;
        cmp r10, 0;
        jle @@exitproc;

        dec r11;     // height - 1
        sar r10, 1;  // width div 2

        test r10, r10;
        jz @@lastColumn;

        @@forxloop:
           mov rax, r11;
           xorpd xmm0, xmm0;
           mov rdi, r9;
           xor rsi, rsi;

           movapd xmm0, [r8];
           // two values at once
           @@foryloop:
              movapd xmm1, [r8 + rdi];
              movapd xmm2, xmm1;
              subpd xmm1, xmm0;
              movapd [rcx + rsi], xmm1;

              movapd xmm0, xmm2;
              add rdi, r9;
              add rsi, rdx;
           dec rax;
           jnz @@foryloop;

           add r8, 16;
           add rcx, 16;
        dec r10;
        jnz @@forxloop;

        @@lastColumn:

        mov rax, r11;

        movsd xmm0, [r8];
        add r8, r9;

        // last column
        @@forycolumnloop:
           movsd xmm1, [r8];
           movsd xmm2, xmm1;
           subsd xmm1, xmm0;
           movsd [rcx], xmm1;
           movsd xmm0, xmm2;

           add r8, r9;
           add rcx, rdx;
        dec rax;
        jnz @@forycolumnloop;

        @@exitProc:
        // epilog - stack cleanup
        mov rdi, iRDI;
        mov rsi, iRSI;
     end;
{$IFDEF FPC}
end;
{$ENDIF}

{$ENDIF}

end.
