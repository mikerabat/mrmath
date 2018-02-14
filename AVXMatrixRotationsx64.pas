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

unit AVXMatrixRotationsx64;

interface

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF x64}


uses MatrixConst;

procedure AVXApplyPlaneRotSeqRVB(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);
procedure AVXApplyPlaneRotSeqRVF(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);
procedure AVXApplyPlaneRotSeqLVF(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);
procedure AVXApplyPlaneRotSeqLVB(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);


procedure AVXMatrixRotate(N : TASMNativeInt; X : PDouble; const LineWidthDX : TASMNativeInt; Y : PDouble; LineWidthDY : TASMNativeInt; const c, s : double);

{$ENDIF}

implementation

{$IFDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$ENDIF}

// rcx = width, rdx = height, r8 : A, r9 = LineWidthA
procedure AVXApplyPlaneRotSeqLVB(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);
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
   vmovupd dXMM4, xmm4;
   vmovupd dXMM5, xmm5;
   vmovupd dXMM6, xmm6;
   vmovupd dXMM7, xmm7;


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

   lea rdi, [rip + cOne];
   vmovsd xmm7, [rdi];
   vxorpd xmm6, xmm6, xmm6;  // reference

   @@foryloop:
       vmovddup xmm0, [rax];  // c[y]
       vmovddup xmm1, [rbx];  // s[y]

       // ###########################################
       // #### if (ctemp <> 1) or (stemp <> 0) then
       vcomisd xmm0, xmm7; // = 1
       jne @@beginXLoop;

       vcomisd xmm1, xmm6; // = 0
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
           vmovupd xmm2, [r11 + rdi];
           vmovupd xmm3, [r8 + rdi];

           // temp store...
           vmovapd xmm4, xmm2
           vmovapd xmm5, xmm3;

           vmulpd xmm3, xmm3, xmm0; // ctemp*pcay1^[x] and ctemp*a[x+1]
           vmulpd xmm2, xmm2, xmm1;  // stemp*pcAy^[x] and stemp*a[x+1]

           vsubpd xmm3, xmm3, xmm2;

           //     pcAy^[x] := stemp*temp + ctemp*pcAy^[x];
           //     pcAy^[x + 1] := stemp*temp1 + ctemp*pcAy^[x + 1]
           vmulpd xmm4, xmm4, xmm0;
           vmulpd xmm5, xmm5, xmm1;

           vaddpd xmm5, xmm5, xmm4;

           // write back...
           vmovupd [r11 + rdi], xmm5;
           vmovupd [r8 + rdi], xmm3;

           add rdi, 16;
       jnz @@forxloop;

       @@LastElem:

       // ###########################################
       // #### Last element handling
       mov rdi, rcx;  // width
       and rdi, 1;
       jz @@nextLine;

       // same as above but with single elements
       vmovsd xmm2, [r11];
       vmovsd xmm3, [r8];

       vmovsd xmm4, xmm4, xmm2;
       vmovsd xmm5, xmm5, xmm3;

       vmulsd xmm3, xmm3, xmm0;
       vmulsd xmm2, xmm2, xmm1;

       vsubsd xmm3, xmm3, xmm2;

       vmulsd xmm4, xmm4, xmm0;
       vmulsd xmm5, xmm5, xmm1;

       vaddsd xmm5, xmm5, xmm4;

       vmovsd [r11], xmm5;
       vmovsd [r8], xmm3;

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
   vmovupd xmm4, dXMM4;
   vmovupd xmm5, dXMM5;
   vmovupd xmm6, dXMM6;
   vmovupd xmm7, dXMM7;

   @@endproc:
   vzeroupper;
end;
{$IFDEF FPC}
end;
{$ENDIF}


procedure AVXApplyPlaneRotSeqLVF(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);
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
   vmovupd dXMM4, xmm4;
   vmovupd dXMM5, xmm5;
   vmovupd dXMM6, xmm6;
   vmovupd dXMM7, xmm7;


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

   lea rdi, [rip + cOne];
   vmovsd xmm7, [rdi];
   vxorpd xmm6, xmm6, xmm6;  // compare against 0

   @@foryloop:
       vmovddup xmm0, [rax];  // c[y]
       vmovddup xmm1, [rbx];  // s[y]

       // ###########################################
       // #### if (ctemp <> 1) or (stemp <> 0) then
       vcomisd xmm0, xmm7; // = 1
       jne @@beginXLoop;

       vcomisd xmm1, xmm6; // = 0
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
              vmovupd xmm2, [r8 + rdi];
              vmovupd xmm3, [r11 + rdi];

              vmulpd xmm5, xmm3, xmm0;  // ctemp*pcay1^[x] and ctemp*a[x+1]
              vmulpd xmm4, xmm2, xmm1;  // stemp*pcAy^[x] and stemp*a[x+1]

              //subpd xmm3, xmm2;
              vsubpd xmm5, xmm5, xmm4;

              //     pcAy^[x] := stemp*temp + ctemp*pcAy^[x];
              //     pcAy^[x + 1] := stemp*temp1 + ctemp*pcAy^[x + 1]
              vmulpd xmm2, xmm2, xmm0;
              vmulpd xmm3, xmm3, xmm1;

              //addpd xmm5, xmm4;
              vaddpd xmm3, xmm3, xmm2;

              // write back...
              vmovupd [r8 + rdi], xmm3;
              vmovupd [r11 + rdi], xmm5;

              add rdi, 16;
           jnz @@forxloop;

           @@LastElem:

           // ###########################################
           // #### Last element handling
           mov rdi, rcx;  // width
           and rdi, 1;
           jz @@nextLine;

           // same as above but with single elements
           vmovsd xmm4, [r8];
           vmovsd xmm5, [r11];

           //vmovsd xmm4, xmm2;
           //vmovsd xmm5, xmm3;

           vmulsd xmm3, xmm5, xmm0;
           vmulsd xmm2, xmm4, xmm1;

           vsubsd xmm3, xmm3, xmm2;

           vmulsd xmm4, xmm4, xmm0;
           vmulsd xmm5, xmm5, xmm1;

           vaddsd xmm5, xmm5, xmm4;

           vmovsd [r8], xmm5;
           vmovsd [r11], xmm3;

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
        vmovupd xmm4, dXMM4;
        vmovupd xmm5, dXMM5;
        vmovupd xmm6, dXMM6;
        vmovupd xmm7, dXMM7;
        vzeroupper;

        @@endproc:
     end;
{$IFDEF FPC}
end;
{$ENDIF}

// rcx = width, rdx = height, r8 : A, r9 = LineWidthA
procedure AVXApplyPlaneRotSeqRVB(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);
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
   //   exit;
   cmp rcx, 2;
   jl @@endproc;

   cmp rdx, 1;
   jl @@endproc;

   mov iRBX, rbx;
   mov iRDI, rdi;
   vmovupd dXMM4, xmm4;
   vmovupd dXMM5, xmm5;
   vmovupd dXMM7, xmm7;

   dec rcx;
   imul rcx, 8; //iter := (width - 1)*sizeof(double)

   mov rax, c;
   mov rbx, s;

   vmovupd xmm7, [rip + cMulM1Bits];

   @@foryloop:

       mov rdi, rcx;
       vmovhpd xmm2, xmm2, [r8 + rdi];

       // for x := width - 2 downto 0
       @@forxloop:
           vmovsd xmm4, [rax + rdi - 8];  // store c
           vmovsd xmm3, [rbx + rdi - 8];  // store s

           vmovlpd xmm2, xmm2, [r8 + rdi - 8]; // a[x], a[x+1]

           // handle x, x+1
           // ####################################
           // #### x, x+ 1
           vmovlhps xmm3, xmm3, xmm4;
           vmovlhps xmm4, xmm4, xmm3;

           vxorpd xmm3, xmm3, xmm7;  // -s, c
           vmulpd xmm3, xmm3, xmm2; // a[x+1)*c[x] - s[x]*a[x]
           vhaddpd xmm3, xmm3, xmm3;

           vmulpd xmm4, xmm4, xmm2; // a[x+1]*s[x] + a[x]*c[x]
           vhaddpd xmm4, xmm4, xmm4;

           // write back first two values
           vmovlhps xmm2, xmm2, xmm4;
           vmovsd [r8 + rdi], xmm3;

       // next one
       sub rdi, 8;
       jnz @@forxloop;

       vmovsd [r8 + rdi], xmm4;

       add r8, LineWidthA;

   dec rdx;
   jnz @@foryloop;

   // epilog
   mov rbx, iRBX;
   mov rdi, iRDI;
   vmovupd xmm4, dXMM4;
   vmovupd xmm5, dXMM5;
   vmovupd xmm7, dXMM7;
   vzeroupper;

   @@endproc:
end;
{$IFDEF FPC}
end;
{$ENDIF}

procedure AVXApplyPlaneRotSeqRVF(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);
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
   vmovupd dXMM4, xmm4;
   vmovupd dXMM5, xmm5;
   vmovupd dXMM7, xmm7;

   dec rcx;
   imul rcx, -8; //iter := (width - 1)*sizeof(double)

   mov rax, c;
   mov rbx, s;

   sub rax, rcx; // iter
   sub rbx, rcx;
   sub r8, rcx;

   vmovupd xmm7, [rip + cMulM1Bits];

   @@foryloop:

       mov rdi, rcx;
       vmovsd xmm2, [r8 + rdi];

       @@forxloop:
           vmovsd xmm4, [rax + rdi];  // store c
           vmovhpd xmm4, xmm4, [rbx + rdi];  // store s

           vshufpd xmm3, xmm4, xmm4, 1;

           vmovhpd xmm2, xmm2, [r8 + rdi + 8]; // a[x], a[x + 1];

           // handle x, x+1
           // ####################################
           // #### x, x+ 1

           vxorpd xmm3, xmm3, xmm7;  // -s, c
           vmulpd xmm3, xmm3, xmm2; // a[x+1)*c[x] - s[x]*a[x]
           vhaddpd xmm3, xmm3, xmm3;

           vmulpd xmm4, xmm4, xmm2; // a[x+1]*s[x] + a[x]*c[x]
           vhaddpd xmm4, xmm4, xmm4;

           // write back first two values
           vmovsd xmm2, xmm2, xmm3;
           vmovsd [r8 + rdi], xmm4;

           // next one
       add rdi, 8;
       jnz @@forxloop;

       vmovsd [r8 + rdi], xmm2;

       add r8, LineWidthA;

   dec rdx;
   jnz @@foryloop;

   // epilog
   mov rbx, iRBX;
   mov rdi, iRDI;
   vmovupd xmm4, dXMM4;
   vmovupd xmm5, dXMM5;
   vmovupd xmm7, dXMM7;
   vzeroupper;

   @@endproc:
end;
{$IFDEF FPC}
end;
{$ENDIF}

// its assumed that Linewidthdx and linewidthdy = sizeof(double)
// rcx = N, RDX = X, R8 = Y,
procedure AVXMatrixRotateAligned(N : TASMNativeInt; X : PDouble;
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
   vmovupd dXMM4, xmm4;
   vmovupd dXMM5, xmm5;
   vmovupd dXMM6, xmm6;

   // init
   lea rax, s;
   vmovsd xmm2, [rax];
   lea rax, [rip + cMinusOne];
   vmulsd xmm2, xmm2, [rax];

   vmovddup xmm0, xmm2;
   lea rax, c;
   vmovddup xmm1, [rax];

   lea rax, s;
   vmovddup xmm2, [rax];

   xor r10, r10;

   mov rax, rcx;
   shr rax, 1;
   test rax, rax;
   jz @@exitLoop;

   @@forNloop:
       // do a full load -> intermediate store in xmm5, and xmm6
       vmovupd xmm5, [rdx + r10];      // x, x+1
       vmovupd xmm6, [r8 + r10];      // y, y+1

       vmulpd xmm3, xmm5, xmm0;  // x, x+1 * -s
       vmulpd xmm5, xmm5, xmm1;  // x, x+1 * c
       vmulpd xmm4, xmm6, xmm1;  // y, y+1 * c
       vmulpd xmm6, xmm6, xmm2;  // y, y+1 * s

       vaddpd xmm5, xmm5, xmm6;  // c*x + s*y  , c*(x+1) + s*(y+1)
       vaddpd xmm3, xmm3, xmm4;  // -s*x + c*y, -s(x+1) + c*(y+1)

       // write back
       vmovupd [rdx + r10], xmm5;
       vmovupd [r8 + r10], xmm3;

       add r10, 16;

       dec rax;
   jnz @@forNloop;

   @@exitLoop:

   // test for an odd N
   mov rax, rcx;
   and rax, 1;
   jz @@endProc;

   // handle last element
   vmovsd xmm5, [rdx + r10];
   vmovsd xmm6, [r8 + r10];

   //dtemp := c*pX^[i] + s*pY^[i];
   //pY^[i] := - s*pX^[i] + c*pY^[i];
   //px^[i] := dtemp;

   vmulsd xmm3, xmm5, xmm0;  // x * -s
   vmulsd xmm4, xmm6, xmm1;  // y * c
   vmulsd xmm5, xmm5, xmm1;  // x * c
   vmulsd xmm6, xmm6, xmm2;  // y * s

   vaddsd xmm5, xmm5, xmm6;  // c*x + s*y
   vaddsd xmm3, xmm3, xmm4;  // -s*x + c*y

   // write back
   vmovsd [rdx + r10], xmm5;
   vmovsd [r8 + r10], xmm3;

   @@endProc:

   // ###########################################
   // #### epilog
   vmovupd xmm4, dXMM4;
   vmovupd xmm5, dXMM5;
   vmovupd xmm6, dXMM6;
   vzeroupper;
end;
{$IFDEF FPC}
end;
{$ENDIF}

// rcx = N, RDX = X, R8 = LineWidthDX, R9 = Y; 
procedure AVXMatrixRotateUnaligned(N : TASMNativeInt; X : PDouble; const LineWidthDX : TASMNativeInt;
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
   vmovupd dXMM4, xmm4;
   vmovupd dXMM5, xmm5;
   vmovupd dXMM6, xmm6;

   lea rax, s;
   vmovsd xmm2, [rax];
   lea rax, [rip + cMinusOne];
   vmulsd xmm2, xmm2, [rax];

   vmovddup xmm0, xmm2;
   lea rax, c;
   vmovddup xmm1, [rax];

   lea rax, s;
   vmovddup xmm2, [rax];

   mov r10, LineWidthDY;

   // ###########################################
   // #### loop
   mov rax, rcx;
   shr rax, 1;
   test rax, rax;
   jz @@exitLoop;

   @@forNloop:
       // do a full load -> intermediate store in xmm5, and xmm6
       vmovlpd xmm5, xmm5, [rdx];    // load x, x+1
       vmovhpd xmm5, xmm5, [rdx + r8];
       vmovlpd xmm6, xmm6, [r9];    // load y, y+1
       vmovhpd xmm6, xmm6, [r9 + r10];

       vmulpd xmm3, xmm5, xmm0;  // x, x+1 * -s
       vmulpd xmm5, xmm5, xmm1;  // x, x+1 * c
       vmulpd xmm4, xmm6, xmm1;  // y, y+1 * c
       vmulpd xmm6, xmm6, xmm2;  // y, y+1 * s


       vaddpd xmm5, xmm5, xmm6;  // c*x + s*y  , c*(x+1) + s*(y+1)
       vaddpd xmm3, xmm3, xmm4;  // -s*x + c*y, -s(x+1) + c*(y+1)


       // write back
       vmovlpd [rdx], xmm5;
       vmovhpd [rdx + r8], xmm5;

       vmovlpd [r9], xmm3;
       vmovhpd [r9 + r10], xmm3;

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
   vmovsd xmm5, [rdx];
   vmovsd xmm6, [r9];

   //dtemp := c*pX^[i] + s*pY^[i];
   //pY^[i] := - s*pX^[i] + c*pY^[i];
   //px^[i] := dtemp;
   //vmovsd xmm3, xmm5;
   //vmovsd xmm4, xmm6;

   vmulsd xmm3, xmm5, xmm0;  // x * -s
   vmulsd xmm5, xmm5, xmm1;  // x * c
   vmulsd xmm4, xmm6, xmm1;  // y * c
   vmulsd xmm6, xmm6, xmm2;  // y * s

   vaddsd xmm5, xmm5, xmm6;  // c*x + s*y
   vaddsd xmm3, xmm3, xmm4;  // -s*x + c*y

   // write back
   vmovsd [rdx], xmm5;
   vmovsd [r9], xmm3;

   @@endProc:

   // ###########################################
   // #### Epilog
   vmovupd xmm4, dXMM4;
   vmovupd xmm5, dXMM5;
   vmovupd xmm6, dXMM6;
   vzeroupper;
end;
{$IFDEF FPC}
end;
{$ENDIF}

procedure AVXMatrixRotate(N : TASMNativeInt; X : PDouble; const LineWidthDX : TASMNativeInt; Y : PDouble; LineWidthDY : TASMNativeInt; const c, s : double);
begin
     if N <= 0 then
        exit;

     if (LineWidthDX = sizeof(double)) and (LineWidthDY = sizeof(double))
     then
         AVXMatrixRotateAligned(N, X, Y, c, s)
     else
         AVXMatrixRotateUnAligned(N, X, LineWidthDX, Y, LineWidthDY, c, s)
end;


{$ENDIF}

end.
