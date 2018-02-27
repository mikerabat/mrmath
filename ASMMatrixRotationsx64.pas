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

// Vector/Matrix rotation routines mainly used for the SVD

unit ASMMatrixRotationsx64;

interface

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF x64}


uses MatrixConst;

procedure ASMApplyPlaneRotSeqRVB(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);
procedure ASMApplyPlaneRotSeqRVF(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);
procedure ASMApplyPlaneRotSeqLVF(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);
procedure ASMApplyPlaneRotSeqLVB(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);


procedure ASMMatrixRotate(N : TASMNativeInt; X : PDouble; const LineWidthDX : TASMNativeInt; Y : PDouble; LineWidthDY : TASMNativeInt; const c, s : double);

{$ENDIF}

implementation

{$IFDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$S-} {$ENDIF}

// rcx = width, rdx = height, r8 : A, r9 = LineWidthA
procedure ASMApplyPlaneRotSeqLVB(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);
var iRBX, iRDI : TASMNativeInt;
    dXMM4, dXMM5, dXMM6, dXMM7 : Array[0..1] of double;
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

        // ##########################################
        // #### Prolog - stack and base variable init
        //if (height < 2) or (width < 1) then
        //exit;
        cmp rdx, 2;
        jl @@endproc;

        cmp rcx, 1;
        jl @@endproc;

        mov iRBX, rbx;
        mov iRDI, rdi;
        movupd dXMM4, xmm4;
        movupd dXMM5, xmm5;
        movupd dXMM6, xmm6;
        movupd dXMM7, xmm7;


        // iter := -(width and $FFFFFFFE)*sizeof(double);
        mov r10, rcx;
        and r10, $FFFFFFFFFFFFFFFE;
        imul r10, -8;

        //y2 := height - 1;
        dec rdx;

        // rcx = width, rdx = height, r8 : A, r9 = LineWidthA
        mov rax, c;  // point to y (aka the end)
        mov rbx, s;

        mov rdi, rdx;
        dec rdi;
        shl rdi, 3;  // y2*sizeof(double)
        add rax, rdi;
        add rbx, rdi;

        mov rdi, r9;      // A[y + 1][x]
        imul rdi, rdx;
        add r8, rdi;
        sub r8, r10;

        mov r11, r8;   // A[y][x]
        sub r11, r9;

        movsd xmm7, [rip + cOne];
        xorpd xmm6, xmm6;  // haddpd zero extend

        @@foryloop:
           movddup xmm0, [rax];  // c[y]
           movddup xmm1, [rbx];  // s[y]

           // ###########################################
           // #### if (ctemp <> 1) or (stemp <> 0) then
           comisd xmm0, xmm7; // = 1
           jne @@beginXLoop;

           comisd xmm1, xmm6; // = 0
           jne @@beginXLoop;

           jmp @@nextLine; // c=1 and stemp=0 next line -> the statement


           // ###########################################
           // #### for x := 0 to width - 1 do
           @@beginXLoop:

           // init
           mov rdi, r10;
           test rdi, rdi;
           jz @@LastElem;

           @@forxloop:
              //temp := pcAy1^[x];
              //     pcAy1^[x] := cTemp*temp - stemp*pcAy^[x];
              //     pcAy1^[x + 1] := cTemp*temp1 - stemp*pcAy1[x + 1];

              // evaluate 2 values
              movupd xmm2, [r11 + rdi];
              movupd xmm3, [r8 + rdi];

              // temp store...
              movapd xmm4, xmm2
              movapd xmm5, xmm3;

              mulpd xmm3, xmm0; // ctemp*pcay1^[x] and ctemp*a[x+1]
              mulpd xmm2, xmm1;  // stemp*pcAy^[x] and stemp*a[x+1]

              subpd xmm3, xmm2;

              //     pcAy^[x] := stemp*temp + ctemp*pcAy^[x];
              //     pcAy^[x + 1] := stemp*temp1 + ctemp*pcAy^[x + 1]
              mulpd xmm4, xmm0;
              mulpd xmm5, xmm1;

              addpd xmm5, xmm4;

              // write back...
              movupd [r11 + rdi], xmm5;
              movupd [r8 + rdi], xmm3;

              add rdi, 16;
           jnz @@forxloop;

           @@LastElem:

           // ###########################################
           // #### Last element handling
           mov rdi, rcx;  // width
           and rdi, 1;
           jz @@nextLine;

           // same as above but with single elements
           movsd xmm2, [r11];
           movsd xmm3, [r8];

           movsd xmm4, xmm2;
           movsd xmm5, xmm3;

           mulsd xmm3, xmm0;
           mulsd xmm2, xmm1;

           subsd xmm3, xmm2;

           mulsd xmm4, xmm0;
           mulsd xmm5, xmm1;

           addsd xmm5, xmm4;

           movsd [r11], xmm5;
           movsd [r8], xmm3;

           // ###########################################
           // #### next y
           @@nextLine:

           sub rbx, 8;   // sizeof(double)
           sub rax, 8;
           sub r11, r9;  // LineWidthA
           sub r8, r9;
        dec rdx;
        jnz @@foryloop;

        // ###############################################
        // #### epilog - restore stack
        mov rbx, iRBX;
        mov rdi, iRDI;
        movupd xmm4, dXMM4;
        movupd xmm5, dXMM5;
        movupd xmm6, dXMM6;
        movupd xmm7, dXMM7;

        @@endproc:
     end;
{$IFDEF FPC}
end;
{$ENDIF}


procedure ASMApplyPlaneRotSeqLVF(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);
var iRBX, iRDI : TASMNativeInt;
    dXMM4, dXMM5, dXMM6, dXMM7 : Array[0..1] of double;
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

        // ##########################################
        // #### Prolog - stack and base variable init
        //if (height < 2) or (width < 1) then
        //exit;
        cmp rdx, 2;
        jl @@endproc;

        cmp rcx, 1;
        jl @@endproc;

        mov iRBX, rbx;
        mov iRDI, rdi;
        movupd dXMM4, xmm4;
        movupd dXMM5, xmm5;
        movupd dXMM6, xmm6;
        movupd dXMM7, xmm7;


        // iter := -(width and $FFFFFFFE)*sizeof(double);
        mov r10, rcx;
        and r10, $FFFFFFFFFFFFFFFE;
        imul r10, -8;

        //y2 := height - 1;
        dec rdx;

        mov rax, c;  // point to y (aka the end)
        mov rbx, s;

        sub r8, r10;
        mov r11, r8;   // A[y][x]
        add r11, r9;

        movsd xmm7, [rip + cOne];
        xorpd xmm6, xmm6;  // haddpd zero extend

        @@foryloop:
           movddup xmm0, [rax];  // c[y]
           movddup xmm1, [rbx];  // s[y]

           // ###########################################
           // #### if (ctemp <> 1) or (stemp <> 0) then
           comisd xmm0, xmm7; // = 1
           jne @@beginXLoop;

           comisd xmm1, xmm6; // = 0
           jne @@beginXLoop;

           jmp @@nextLine; // c=1 and stemp=0 next line -> the statement


           // ###########################################
           // #### for x := 0 to width - 1 do
           @@beginXLoop:

           // init
           mov rdi, r10;
           test rdi, rdi;
           jz @@LastElem;

           @@forxloop:
              //temp := pcAy1^[x];
              //     pcAy1^[x] := cTemp*temp - stemp*pcAy^[x];
              //     pcAy1^[x + 1] := cTemp*temp1 - stemp*pcAy1[x + 1];

              // evaluate 2 values
              movupd xmm2, [r8 + rdi];
              movupd xmm3, [r11 + rdi];

              // temp store...
              movapd xmm4, xmm2
              movapd xmm5, xmm3;

              mulpd xmm3, xmm0;  // ctemp*pcay1^[x] and ctemp*a[x+1]
              mulpd xmm2, xmm1;  // stemp*pcAy^[x] and stemp*a[x+1]

              subpd xmm3, xmm2;

              //     pcAy^[x] := stemp*temp + ctemp*pcAy^[x];
              //     pcAy^[x + 1] := stemp*temp1 + ctemp*pcAy^[x + 1]
              mulpd xmm4, xmm0;
              mulpd xmm5, xmm1;

              addpd xmm5, xmm4;

              // write back...
              movupd [r8 + rdi], xmm5;
              movupd [r11 + rdi], xmm3;

              add rdi, 16;
           jnz @@forxloop;

           @@LastElem:

           // ###########################################
           // #### Last element handling
           mov rdi, rcx;  // width
           and rdi, 1;
           jz @@nextLine;

           // same as above but with single elements
           movsd xmm2, [r8];
           movsd xmm3, [r11];

           movsd xmm4, xmm2;
           movsd xmm5, xmm3;

           mulsd xmm3, xmm0;
           mulsd xmm2, xmm1;

           subsd xmm3, xmm2;

           mulsd xmm4, xmm0;
           mulsd xmm5, xmm1;

           addsd xmm5, xmm4;

           movsd [r8], xmm5;
           movsd [r11], xmm3;

           // ###########################################
           // #### next y
           @@nextLine:

           add rbx, 8;   // sizeof(double)
           add rax, 8;
           add r11, r9;  // LineWidthA
           add r8, r9;
        dec rdx;
        jnz @@foryloop;

        // ###############################################
        // #### epilog - restore stack
        mov rbx, iRBX;
        mov rdi, iRDI;
        movupd xmm4, dXMM4;
        movupd xmm5, dXMM5;
        movupd xmm6, dXMM6;
        movupd xmm7, dXMM7;

        @@endproc:
     end;
{$IFDEF FPC}
end;
{$ENDIF}

// rcx = width, rdx = height, r8 : A, r9 = LineWidthA
procedure ASMApplyPlaneRotSeqRVB(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);
var iRBX, iRDI : TASMNativeInt;
    dXMM4, dXMM5, dXMM7 : Array[0..1] of double;
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

   // ##########################################
   // #### Prolog - stack and base variable init
   //if (height < 2) or (width < 1) then
   //exit;
   cmp rcx, 2;
   jl @@endproc;

   cmp rdx, 1;
   jl @@endproc;

   mov iRBX, rbx;
   mov iRDI, rdi;
   movupd dXMM4, xmm4;
   movupd dXMM5, xmm5;
   movupd dXMM7, xmm7;

   dec rcx;
   imul rcx, 8; //iter := (width - 1)*sizeof(double)

   mov rax, c;
   mov rbx, s;

   movupd xmm7, [rip + cMulM1Bits];

   @@foryloop:

       mov rdi, rcx;
       movhpd xmm2, [r8 + rdi];

       // for x := width - 2 downto 0
       @@forxloop:
           movsd xmm4, [rax + rdi - 8];  // store c
           movsd xmm3, [rbx + rdi - 8];  // store s

           movlpd xmm2, [r8 + rdi - 8]; // a[x], a[x+1]

           // handle x, x+1
           // ####################################
           // #### x, x+ 1
           movlhps xmm3, xmm4;
           movlhps xmm4, xmm3;

           xorpd xmm3, xmm7;  // -s, c
           mulpd xmm3, xmm2; // a[x+1)*c[x] - s[x]*a[x]
           haddpd xmm3, xmm3;

           mulpd xmm4, xmm2; // a[x+1]*s[x] + a[x]*c[x]
           haddpd xmm4, xmm4;

           // write back first two values
           movlhps xmm2, xmm4;
           movsd [r8 + rdi], xmm3;

       // next one
       sub rdi, 8;
       jnz @@forxloop;

       movsd [r8 + rdi], xmm4;

       add r8, LineWidthA;

   dec rdx;
   jnz @@foryloop;

   // epilog
   mov rbx, iRBX;
   mov rdi, iRDI;
   movupd xmm4, dXMM4;
   movupd xmm5, dXMM5;
   movupd xmm7, dXMM7;

   @@endproc:
end;
{$IFDEF FPC}
end;
{$ENDIF}

procedure ASMApplyPlaneRotSeqRVF(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);
var iRBX, iRDI : TASMNativeInt;
    dXMM4, dXMM5, dXMM7 : Array[0..1] of double;
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

        // ##########################################
        // #### Prolog - stack and base variable init
        //if (height < 2) or (width < 1) then
        //exit;
        cmp rcx, 2;
        jl @@endproc;

        cmp rdx, 1;
        jl @@endproc;

        mov iRBX, rbx;
        mov iRDI, rdi;
        movupd dXMM4, xmm4;
        movupd dXMM5, xmm5;
        movupd dXMM7, xmm7;

        dec rcx;
        imul rcx, -8; //iter := (width - 1)*sizeof(double)

        mov rax, c;
        mov rbx, s;

        sub rax, rcx; // iter
        sub rbx, rcx;
        sub r8, rcx;

        movupd xmm7, [rip + cMulM1Bits];

        @@foryloop:

           mov rdi, rcx;
           movsd xmm2, [r8 + rdi];

           @@forxloop:
              movsd xmm4, [rax + rdi];  // store c
              movsd xmm3, [rbx + rdi];  // store s

              movsd xmm0, [r8 + rdi + 8]; // a[x], a[x+1]
              movlhps xmm2, xmm0;

              // handle x, x+1
              // ####################################
              // #### x, x+ 1
              movlhps xmm3, xmm4;
              movlhps xmm4, xmm3;

              xorpd xmm3, xmm7;  // -s, c
              mulpd xmm3, xmm2; // a[x+1)*c[x] - s[x]*a[x]
              haddpd xmm3, xmm3;

              mulpd xmm4, xmm2; // a[x+1]*s[x] + a[x]*c[x]
              haddpd xmm4, xmm4;

              // write back first two values
              movsd xmm2, xmm3;
              movsd [r8 + rdi], xmm4;

           // next one
           add rdi, 8;
           jnz @@forxloop;

           movsd [r8 + rdi], xmm2;

           add r8, LineWidthA;

        dec rdx;
        jnz @@foryloop;

        // epilog
        mov rbx, iRBX;
        mov rdi, iRDI;
        movupd xmm4, dXMM4;
        movupd xmm5, dXMM5;
        movupd xmm7, dXMM7;

        @@endproc:
     end;
{$IFDEF FPC}
end;
{$ENDIF}

// its assumed that Linewidthdx and linewidthdy = sizeof(double)
// rcx = N, RDX = X, R8 = Y,
procedure ASMMatrixRotateAligned(N : TASMNativeInt; X : PDouble;
  Y : PDouble; const c, s : double);
var dXMM4, dXMM5, dXMM6 : Array[0..1] of double;
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

        // ###########################################
        // #### Stack push
        movupd dXMM4, xmm4;
        movupd dXMM5, xmm5;
        movupd dXMM6, xmm6;

        // init
        movsd xmm2, s;
        mulsd xmm2, [rip + cMinusOne];

        movddup xmm0, xmm2;
        movddup xmm1, c;

        movddup xmm2, s;

        xor r10, r10;

        mov rax, rcx;
        shr rax, 1;
        test rax, rax;
        jz @@exitLoop;

           @@forNloop:
              // do a full load -> intermediate store in xmm5, and xmm6
              movupd xmm5, [rdx + r10];      // x, x+1
              movupd xmm6, [r8 + r10];      // y, y+1

              movapd xmm3, xmm5;
              movapd xmm4, xmm6;

              mulpd xmm3, xmm0;  // x, x+1 * -s
              mulpd xmm5, xmm1;  // x, x+1 * c
              mulpd xmm6, xmm2;  // y, y+1 * s
              mulpd xmm4, xmm1;  // y, y+1 * c

              addpd xmm5, xmm6;  // c*x + s*y  , c*(x+1) + s*(y+1)
              addpd xmm3, xmm4;  // -s*x + c*y, -s(x+1) + c*(y+1)

              // write back
              movupd [rdx + r10], xmm5;
              movupd [r8 + r10], xmm3;

              add r10, 16;

           dec rax;
           jnz @@forNloop;

        @@exitLoop:

        // test for an odd N
        mov rax, rcx;
        and rax, 1;
        jz @@endProc;

        // handle last element
        movsd xmm5, [rdx + r10];
        movsd xmm6, [r8 + r10];

        //dtemp := c*pX^[i] + s*pY^[i];
        //pY^[i] := - s*pX^[i] + c*pY^[i];
        //px^[i] := dtemp;
        movsd xmm3, xmm5;
        movsd xmm4, xmm6;

        mulsd xmm3, xmm0;  // x * -s
        mulsd xmm5, xmm1;  // x * c
        mulsd xmm6, xmm2;  // y * s
        mulsd xmm4, xmm1;  // y * c

        addsd xmm5, xmm6;  // c*x + s*y
        addsd xmm3, xmm4;  // -s*x + c*y

        // write back
        movsd [rdx + r10], xmm5;
        movsd [r8 + r10], xmm3;

        @@endProc:

        // ###########################################
        // #### epilog
        movupd xmm4, dXMM4;
        movupd xmm5, dXMM5;
        movupd xmm6, dXMM6;
     end;
{$IFDEF FPC}
end;
{$ENDIF}

// rcx = N, RDX = X, R8 = LineWidthDX, R9 = Y; 
procedure ASMMatrixRotateUnaligned(N : TASMNativeInt; X : PDouble; const LineWidthDX : TASMNativeInt;
  Y : PDouble; LineWidthDY : TASMNativeInt; const c, s : double);
var dXMM4, dXMM5, dXMM6 : Array[0..1] of double;
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

        // ###########################################
        // #### Stack push
        movupd dXMM4, xmm4;
        movupd dXMM5, xmm5;
        movupd dXMM6, xmm6;
     
        movsd xmm2, s;
        mulsd xmm2, [rip + cMinusOne];

        movddup xmm0, xmm2;
        movddup xmm1, c;

        movddup xmm2, s;

        mov r10, LineWidthDY;

        // ###########################################
        // #### loop
        mov rax, rcx;
        shr rax, 1;
        test rax, rax;
        jz @@exitLoop;

           @@forNloop:
              // do a full load -> intermediate store in xmm5, and xmm6
              movlpd xmm5, [rdx];
              movhpd xmm5, [rdx + r8];
              movlpd xmm6, [r9];
              movhpd xmm6, [r9 + r10];

              movapd xmm3, xmm5;
              movapd xmm4, xmm6;

              mulpd xmm3, xmm0;  // x, x+1 * -s
              mulpd xmm5, xmm1;  // x, x+1 * c
              mulpd xmm6, xmm2;  // y, y+1 * s
              mulpd xmm4, xmm1;  // y, y+1 * c

              addpd xmm5, xmm6;  // c*x + s*y  , c*(x+1) + s*(y+1)
              addpd xmm3, xmm4;  // -s*x + c*y, -s(x+1) + c*(y+1)


              // write back
              movlpd [rdx], xmm5;
              movhpd [rdx + r8], xmm5;

              movlpd [r9], xmm3;
              movhpd [r9 + r10], xmm3;

              add rdx, r8;
              add rdx, r8;
              add r9, r10;
              add r9, r10;

           dec rax;
           jnz @@forNloop;

        @@exitLoop:

        // test for an odd N
        mov rax, rcx;
        and rax, 1;
        jz @@endProc;

        // handle last element
        movsd xmm5, [rdx];
        movsd xmm6, [r9];

        //dtemp := c*pX^[i] + s*pY^[i];
        //pY^[i] := - s*pX^[i] + c*pY^[i];
        //px^[i] := dtemp;
        movsd xmm3, xmm5;
        movsd xmm4, xmm6;

        mulsd xmm3, xmm0;  // x * -s
        mulsd xmm5, xmm1;  // x * c
        mulsd xmm6, xmm2;  // y * s
        mulsd xmm4, xmm1;  // y * c

        addsd xmm5, xmm6;  // c*x + s*y
        addsd xmm3, xmm4;  // -s*x + c*y

        // write back
        movsd [rdx], xmm5;
        movsd [r9], xmm3;

        @@endProc:

        // ###########################################
        // #### Epilog
        movupd xmm4, dXMM4;
        movupd xmm5, dXMM5;
        movupd xmm6, dXMM6;
     end;
{$IFDEF FPC}
end;
{$ENDIF}

procedure ASMMatrixRotate(N : TASMNativeInt; X : PDouble; const LineWidthDX : TASMNativeInt; Y : PDouble; LineWidthDY : TASMNativeInt; const c, s : double);
begin
     if N <= 0 then
        exit;
        
     if (LineWidthDX = sizeof(double)) and (LineWidthDY = sizeof(double))
     then
         ASMMatrixRotateAligned(N, X, Y, c, s)
     else
         ASMMatrixRotateUnAligned(N, X, LineWidthDX, Y, LineWidthDY, c, s)
end;


{$ENDIF}

end.
