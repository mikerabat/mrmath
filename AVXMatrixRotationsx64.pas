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

{$IFDEF FPC} {$ASMMODE intel} {$S-} {$ENDIF}

// rcx = width, rdx = height, r8 : A, r9 = LineWidthA
procedure AVXApplyPlaneRotSeqLVB(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);
var iRBX, iRDI : TASMNativeInt;
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
   sub rsp, $40;
   {$IFDEF FPC}vmovupd [rsp + $10], xmm4;{$ELSE}db $C5,$F9,$11,$64,$24,$10;{$ENDIF} 
   {$IFDEF FPC}vmovupd [rsp + $20], xmm5;{$ELSE}db $C5,$F9,$11,$6C,$24,$20;{$ENDIF} 
   {$IFDEF FPC}vmovupd [rsp + $30], xmm6;{$ELSE}db $C5,$F9,$11,$74,$24,$30;{$ENDIF} 
   {$IFDEF FPC}vmovupd [rsp + $40], xmm7;{$ELSE}db $C5,$F9,$11,$7C,$24,$40;{$ENDIF} 


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
   {$IFDEF FPC}vmovsd xmm7, [rdi];{$ELSE}db $C5,$FB,$10,$3F;{$ENDIF} 
   {$IFDEF FPC}vxorpd xmm6, xmm6, xmm6;  {$ELSE}db $C5,$C9,$57,$F6;{$ENDIF} // reference

   @@foryloop:
       {$IFDEF FPC}vmovddup xmm0, [rax];  {$ELSE}db $C5,$FB,$12,$00;{$ENDIF} // c[y]
       {$IFDEF FPC}vmovddup xmm1, [rbx];  {$ELSE}db $C5,$FB,$12,$0B;{$ENDIF} // s[y]

       // ###########################################
       // #### if (ctemp <> 1) or (stemp <> 0) then
       {$IFDEF FPC}vcomisd xmm0, xmm7; {$ELSE}db $C5,$F9,$2F,$C7;{$ENDIF} // = 1
       jne @@beginXLoop;

       {$IFDEF FPC}vcomisd xmm1, xmm6; {$ELSE}db $C5,$F9,$2F,$CE;{$ENDIF} // = 0
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
           {$IFDEF FPC}vmovupd xmm2, [r11 + rdi];{$ELSE}db $C4,$C1,$79,$10,$14,$3B;{$ENDIF} 
           {$IFDEF FPC}vmovupd xmm3, [r8 + rdi];{$ELSE}db $C4,$C1,$79,$10,$1C,$38;{$ENDIF} 

           // temp store...
           {$IFDEF FPC}vmovapd xmm4, xmm2{$ELSE}db $C5,$F9,$28,$E2;{$ENDIF} 
           {$IFDEF FPC}vmovapd xmm5, xmm3;{$ELSE}db $C5,$F9,$28,$EB;{$ENDIF} 

           {$IFDEF FPC}vmulpd xmm3, xmm3, xmm0; {$ELSE}db $C5,$E1,$59,$D8;{$ENDIF} // ctemp*pcay1^[x] and ctemp*a[x+1]
           {$IFDEF FPC}vmulpd xmm2, xmm2, xmm1;  {$ELSE}db $C5,$E9,$59,$D1;{$ENDIF} // stemp*pcAy^[x] and stemp*a[x+1]

           {$IFDEF FPC}vsubpd xmm3, xmm3, xmm2;{$ELSE}db $C5,$E1,$5C,$DA;{$ENDIF} 

           //     pcAy^[x] := stemp*temp + ctemp*pcAy^[x];
           //     pcAy^[x + 1] := stemp*temp1 + ctemp*pcAy^[x + 1]
           {$IFDEF FPC}vmulpd xmm4, xmm4, xmm0;{$ELSE}db $C5,$D9,$59,$E0;{$ENDIF} 
           {$IFDEF FPC}vmulpd xmm5, xmm5, xmm1;{$ELSE}db $C5,$D1,$59,$E9;{$ENDIF} 

           {$IFDEF FPC}vaddpd xmm5, xmm5, xmm4;{$ELSE}db $C5,$D1,$58,$EC;{$ENDIF} 

           // write back...
           {$IFDEF FPC}vmovupd [r11 + rdi], xmm5;{$ELSE}db $C4,$C1,$79,$11,$2C,$3B;{$ENDIF} 
           {$IFDEF FPC}vmovupd [r8 + rdi], xmm3;{$ELSE}db $C4,$C1,$79,$11,$1C,$38;{$ENDIF} 

           add rdi, 16;
       jnz @@forxloop;

       @@LastElem:

       // ###########################################
       // #### Last element handling
       mov rdi, rcx;  // width
       and rdi, 1;
       jz @@nextLine;

       // same as above but with single elements
       {$IFDEF FPC}vmovsd xmm2, [r11];{$ELSE}db $C4,$C1,$7B,$10,$13;{$ENDIF} 
       {$IFDEF FPC}vmovsd xmm3, [r8];{$ELSE}db $C4,$C1,$7B,$10,$18;{$ENDIF} 

       {$IFDEF FPC}vmovsd xmm4, xmm4, xmm2;{$ELSE}db $C5,$DB,$10,$E2;{$ENDIF} 
       {$IFDEF FPC}vmovsd xmm5, xmm5, xmm3;{$ELSE}db $C5,$D3,$10,$EB;{$ENDIF} 

       {$IFDEF FPC}vmulsd xmm3, xmm3, xmm0;{$ELSE}db $C5,$E3,$59,$D8;{$ENDIF} 
       {$IFDEF FPC}vmulsd xmm2, xmm2, xmm1;{$ELSE}db $C5,$EB,$59,$D1;{$ENDIF} 

       {$IFDEF FPC}vsubsd xmm3, xmm3, xmm2;{$ELSE}db $C5,$E3,$5C,$DA;{$ENDIF} 

       {$IFDEF FPC}vmulsd xmm4, xmm4, xmm0;{$ELSE}db $C5,$DB,$59,$E0;{$ENDIF} 
       {$IFDEF FPC}vmulsd xmm5, xmm5, xmm1;{$ELSE}db $C5,$D3,$59,$E9;{$ENDIF} 

       {$IFDEF FPC}vaddsd xmm5, xmm5, xmm4;{$ELSE}db $C5,$D3,$58,$EC;{$ENDIF} 

       {$IFDEF FPC}vmovsd [r11], xmm5;{$ELSE}db $C4,$C1,$7B,$11,$2B;{$ENDIF} 
       {$IFDEF FPC}vmovsd [r8], xmm3;{$ELSE}db $C4,$C1,$7B,$11,$18;{$ENDIF} 

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
   {$IFDEF FPC}vmovupd xmm4, [rsp + $10];{$ELSE}db $C5,$F9,$10,$64,$24,$10;{$ENDIF} 
   {$IFDEF FPC}vmovupd xmm5, [rsp + $20];{$ELSE}db $C5,$F9,$10,$6C,$24,$20;{$ENDIF} 
   {$IFDEF FPC}vmovupd xmm6, [rsp + $30];{$ELSE}db $C5,$F9,$10,$74,$24,$30;{$ENDIF} 
   {$IFDEF FPC}vmovupd xmm7, [rsp + $40];{$ELSE}db $C5,$F9,$10,$7C,$24,$40;{$ENDIF} 
   add rsp, $40;

   @@endproc:
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;
{$IFDEF FPC}
end;
{$ENDIF}


procedure AVXApplyPlaneRotSeqLVF(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);
var iRBX, iRDI : TASMNativeInt;
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
   sub rsp, $40;
   {$IFDEF FPC}vmovupd [rsp + $10], xmm4;{$ELSE}db $C5,$F9,$11,$64,$24,$10;{$ENDIF} 
   {$IFDEF FPC}vmovupd [rsp + $20], xmm5;{$ELSE}db $C5,$F9,$11,$6C,$24,$20;{$ENDIF} 
   {$IFDEF FPC}vmovupd [rsp + $30], xmm6;{$ELSE}db $C5,$F9,$11,$74,$24,$30;{$ENDIF} 
   {$IFDEF FPC}vmovupd [rsp + $40], xmm7;{$ELSE}db $C5,$F9,$11,$7C,$24,$40;{$ENDIF} 


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
   {$IFDEF FPC}vmovsd xmm7, [rdi];{$ELSE}db $C5,$FB,$10,$3F;{$ENDIF} 
   {$IFDEF FPC}vxorpd xmm6, xmm6, xmm6;  {$ELSE}db $C5,$C9,$57,$F6;{$ENDIF} // compare against 0

   @@foryloop:
       {$IFDEF FPC}vmovddup xmm0, [rax];  {$ELSE}db $C5,$FB,$12,$00;{$ENDIF} // c[y]
       {$IFDEF FPC}vmovddup xmm1, [rbx];  {$ELSE}db $C5,$FB,$12,$0B;{$ENDIF} // s[y]

       // ###########################################
       // #### if (ctemp <> 1) or (stemp <> 0) then
       {$IFDEF FPC}vcomisd xmm0, xmm7; {$ELSE}db $C5,$F9,$2F,$C7;{$ENDIF} // = 1
       jne @@beginXLoop;

       {$IFDEF FPC}vcomisd xmm1, xmm6; {$ELSE}db $C5,$F9,$2F,$CE;{$ENDIF} // = 0
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
              {$IFDEF FPC}vmovupd xmm2, [r8 + rdi];{$ELSE}db $C4,$C1,$79,$10,$14,$38;{$ENDIF} 
              {$IFDEF FPC}vmovupd xmm3, [r11 + rdi];{$ELSE}db $C4,$C1,$79,$10,$1C,$3B;{$ENDIF} 

              {$IFDEF FPC}vmulpd xmm5, xmm3, xmm0;  {$ELSE}db $C5,$E1,$59,$E8;{$ENDIF} // ctemp*pcay1^[x] and ctemp*a[x+1]
              {$IFDEF FPC}vmulpd xmm4, xmm2, xmm1;  {$ELSE}db $C5,$E9,$59,$E1;{$ENDIF} // stemp*pcAy^[x] and stemp*a[x+1]

              //subpd xmm3, xmm2;
              {$IFDEF FPC}vsubpd xmm5, xmm5, xmm4;{$ELSE}db $C5,$D1,$5C,$EC;{$ENDIF} 

              //     pcAy^[x] := stemp*temp + ctemp*pcAy^[x];
              //     pcAy^[x + 1] := stemp*temp1 + ctemp*pcAy^[x + 1]
              {$IFDEF FPC}vmulpd xmm2, xmm2, xmm0;{$ELSE}db $C5,$E9,$59,$D0;{$ENDIF} 
              {$IFDEF FPC}vmulpd xmm3, xmm3, xmm1;{$ELSE}db $C5,$E1,$59,$D9;{$ENDIF} 

              //addpd xmm5, xmm4;
              {$IFDEF FPC}vaddpd xmm3, xmm3, xmm2;{$ELSE}db $C5,$E1,$58,$DA;{$ENDIF} 

              // write back...
              {$IFDEF FPC}vmovupd [r8 + rdi], xmm3;{$ELSE}db $C4,$C1,$79,$11,$1C,$38;{$ENDIF} 
              {$IFDEF FPC}vmovupd [r11 + rdi], xmm5;{$ELSE}db $C4,$C1,$79,$11,$2C,$3B;{$ENDIF} 

              add rdi, 16;
           jnz @@forxloop;

           @@LastElem:

           // ###########################################
           // #### Last element handling
           mov rdi, rcx;  // width
           and rdi, 1;
           jz @@nextLine;

           // same as above but with single elements
           {$IFDEF FPC}vmovsd xmm4, [r8];{$ELSE}db $C4,$C1,$7B,$10,$20;{$ENDIF} 
           {$IFDEF FPC}vmovsd xmm5, [r11];{$ELSE}db $C4,$C1,$7B,$10,$2B;{$ENDIF} 

           //vmovsd xmm4, xmm2;
           //vmovsd xmm5, xmm3;

           {$IFDEF FPC}vmulsd xmm3, xmm5, xmm0;{$ELSE}db $C5,$D3,$59,$D8;{$ENDIF} 
           {$IFDEF FPC}vmulsd xmm2, xmm4, xmm1;{$ELSE}db $C5,$DB,$59,$D1;{$ENDIF} 

           {$IFDEF FPC}vsubsd xmm3, xmm3, xmm2;{$ELSE}db $C5,$E3,$5C,$DA;{$ENDIF} 

           {$IFDEF FPC}vmulsd xmm4, xmm4, xmm0;{$ELSE}db $C5,$DB,$59,$E0;{$ENDIF} 
           {$IFDEF FPC}vmulsd xmm5, xmm5, xmm1;{$ELSE}db $C5,$D3,$59,$E9;{$ENDIF} 

           {$IFDEF FPC}vaddsd xmm5, xmm5, xmm4;{$ELSE}db $C5,$D3,$58,$EC;{$ENDIF} 

           {$IFDEF FPC}vmovsd [r8], xmm5;{$ELSE}db $C4,$C1,$7B,$11,$28;{$ENDIF} 
           {$IFDEF FPC}vmovsd [r11], xmm3;{$ELSE}db $C4,$C1,$7B,$11,$1B;{$ENDIF} 

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
        {$IFDEF FPC}vmovupd xmm4, [rsp + $10];{$ELSE}db $C5,$F9,$10,$64,$24,$10;{$ENDIF} 
        {$IFDEF FPC}vmovupd xmm5, [rsp + $20];{$ELSE}db $C5,$F9,$10,$6C,$24,$20;{$ENDIF} 
        {$IFDEF FPC}vmovupd xmm6, [rsp + $30];{$ELSE}db $C5,$F9,$10,$74,$24,$30;{$ENDIF} 
        {$IFDEF FPC}vmovupd xmm7, [rsp + $40];{$ELSE}db $C5,$F9,$10,$7C,$24,$40;{$ENDIF} 
        add rsp, $40;
        {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

        @@endproc:
     end;
{$IFDEF FPC}
end;
{$ENDIF}

// rcx = width, rdx = height, r8 : A, r9 = LineWidthA
procedure AVXApplyPlaneRotSeqRVB(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);
var iRBX, iRDI : TASMNativeInt;
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
   sub rsp, $30;
   {$IFDEF FPC}vmovupd [rsp + $10], xmm4;{$ELSE}db $C5,$F9,$11,$64,$24,$10;{$ENDIF} 
   {$IFDEF FPC}vmovupd [rsp + $20], xmm5;{$ELSE}db $C5,$F9,$11,$6C,$24,$20;{$ENDIF} 
   {$IFDEF FPC}vmovupd [rsp + $30], xmm7;{$ELSE}db $C5,$F9,$11,$7C,$24,$30;{$ENDIF} 

   dec rcx;
   shl rcx, 3; //iter := (width - 1)*sizeof(double)

   mov rax, c;
   mov rbx, s;

   lea rdi, [rip + cMulM1Bits];
   {$IFDEF FPC}vmovupd xmm7, [rdi];{$ELSE}db $C5,$F9,$10,$3F;{$ENDIF} 

   @@foryloop:

       mov rdi, rcx;
       {$IFDEF FPC}vmovhpd xmm2, xmm2, [r8 + rdi];{$ELSE}db $C4,$C1,$69,$16,$14,$38;{$ENDIF} 

       // for x := width - 2 downto 0
       @@forxloop:
           {$IFDEF FPC}vmovsd xmm4, [rax + rdi - 8];  {$ELSE}db $C5,$FB,$10,$64,$38,$F8;{$ENDIF} // store c
           {$IFDEF FPC}vmovsd xmm3, [rbx + rdi - 8];  {$ELSE}db $C5,$FB,$10,$5C,$3B,$F8;{$ENDIF} // store s

           {$IFDEF FPC}vmovlpd xmm2, xmm2, [r8 + rdi - 8]; {$ELSE}db $C4,$C1,$69,$12,$54,$38,$F8;{$ENDIF} // a[x], a[x+1]

           // handle x, x+1
           // ####################################
           // #### x, x+ 1
           {$IFDEF FPC}vmovlhps xmm3, xmm3, xmm4;{$ELSE}db $C5,$E0,$16,$DC;{$ENDIF} 
           {$IFDEF FPC}vmovlhps xmm4, xmm4, xmm3;{$ELSE}db $C5,$D8,$16,$E3;{$ENDIF} 

           {$IFDEF FPC}vxorpd xmm3, xmm3, xmm7;  {$ELSE}db $C5,$E1,$57,$DF;{$ENDIF} // -s, c
           {$IFDEF FPC}vmulpd xmm3, xmm3, xmm2; {$ELSE}db $C5,$E1,$59,$DA;{$ENDIF} // a[x+1)*c[x] - s[x]*a[x]
           {$IFDEF FPC}vhaddpd xmm3, xmm3, xmm3;{$ELSE}db $C5,$E1,$7C,$DB;{$ENDIF} 

           {$IFDEF FPC}vmulpd xmm4, xmm4, xmm2; {$ELSE}db $C5,$D9,$59,$E2;{$ENDIF} // a[x+1]*s[x] + a[x]*c[x]
           {$IFDEF FPC}vhaddpd xmm4, xmm4, xmm4;{$ELSE}db $C5,$D9,$7C,$E4;{$ENDIF} 

           // write back first two values
           {$IFDEF FPC}vmovlhps xmm2, xmm2, xmm4;{$ELSE}db $C5,$E8,$16,$D4;{$ENDIF} 
           {$IFDEF FPC}vmovsd [r8 + rdi], xmm3;{$ELSE}db $C4,$C1,$7B,$11,$1C,$38;{$ENDIF} 

       // next one
       sub rdi, 8;
       jnz @@forxloop;

       {$IFDEF FPC}vmovsd [r8 + rdi], xmm4;{$ELSE}db $C4,$C1,$7B,$11,$24,$38;{$ENDIF} 

       add r8, LineWidthA;

   dec rdx;
   jnz @@foryloop;

   // epilog
   mov rbx, iRBX;
   mov rdi, iRDI;
   {$IFDEF FPC}vmovupd xmm4, [rsp + $10];{$ELSE}db $C5,$F9,$10,$64,$24,$10;{$ENDIF} 
   {$IFDEF FPC}vmovupd xmm5, [rsp + $20];{$ELSE}db $C5,$F9,$10,$6C,$24,$20;{$ENDIF} 
   {$IFDEF FPC}vmovupd xmm7, [rsp + $30];{$ELSE}db $C5,$F9,$10,$7C,$24,$30;{$ENDIF} 
   add rsp, $30;
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   @@endproc:
end;
{$IFDEF FPC}
end;
{$ENDIF}

procedure AVXApplyPlaneRotSeqRVF(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);
var iRBX, iRDI : TASMNativeInt;
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
   sub rsp, $30;
   {$IFDEF FPC}vmovupd [rsp + $10], xmm4;{$ELSE}db $C5,$F9,$11,$64,$24,$10;{$ENDIF} 
   {$IFDEF FPC}vmovupd [rsp + $20], xmm5;{$ELSE}db $C5,$F9,$11,$6C,$24,$20;{$ENDIF} 
   {$IFDEF FPC}vmovupd [rsp + $30], xmm7;{$ELSE}db $C5,$F9,$11,$7C,$24,$30;{$ENDIF} 

   dec rcx;
   imul rcx, -8; //iter := (width - 1)*sizeof(double)

   mov rax, c;
   mov rbx, s;

   sub rax, rcx; // iter
   sub rbx, rcx;
   sub r8, rcx;

   lea rdi, [rip + cMulM1Bits];
   {$IFDEF FPC}vmovupd xmm7, [rdi];{$ELSE}db $C5,$F9,$10,$3F;{$ENDIF} 

   @@foryloop:

       mov rdi, rcx;
       {$IFDEF FPC}vmovsd xmm2, [r8 + rdi];{$ELSE}db $C4,$C1,$7B,$10,$14,$38;{$ENDIF} 

       @@forxloop:
           {$IFDEF FPC}vmovsd xmm4, [rax + rdi];  {$ELSE}db $C5,$FB,$10,$24,$38;{$ENDIF} // store c
           {$IFDEF FPC}vmovhpd xmm4, xmm4, [rbx + rdi];  {$ELSE}db $C5,$D9,$16,$24,$3B;{$ENDIF} // store s

           {$IFDEF FPC}vshufpd xmm3, xmm4, xmm4, 1;{$ELSE}db $C5,$D9,$C6,$DC,$01;{$ENDIF} 

           {$IFDEF FPC}vmovhpd xmm2, xmm2, [r8 + rdi + 8]; {$ELSE}db $C4,$C1,$69,$16,$54,$38,$08;{$ENDIF} // a[x], a[x + 1];

           // handle x, x+1
           // ####################################
           // #### x, x+ 1

           {$IFDEF FPC}vxorpd xmm3, xmm3, xmm7;  {$ELSE}db $C5,$E1,$57,$DF;{$ENDIF} // -s, c
           {$IFDEF FPC}vmulpd xmm3, xmm3, xmm2; {$ELSE}db $C5,$E1,$59,$DA;{$ENDIF} // a[x+1)*c[x] - s[x]*a[x]
           {$IFDEF FPC}vhaddpd xmm3, xmm3, xmm3;{$ELSE}db $C5,$E1,$7C,$DB;{$ENDIF} 

           {$IFDEF FPC}vmulpd xmm4, xmm4, xmm2; {$ELSE}db $C5,$D9,$59,$E2;{$ENDIF} // a[x+1]*s[x] + a[x]*c[x]
           {$IFDEF FPC}vhaddpd xmm4, xmm4, xmm4;{$ELSE}db $C5,$D9,$7C,$E4;{$ENDIF} 

           // write back first two values
           {$IFDEF FPC}vmovsd xmm2, xmm2, xmm3;{$ELSE}db $C5,$EB,$10,$D3;{$ENDIF} 
           {$IFDEF FPC}vmovsd [r8 + rdi], xmm4;{$ELSE}db $C4,$C1,$7B,$11,$24,$38;{$ENDIF} 

           // next one
       add rdi, 8;
       jnz @@forxloop;

       {$IFDEF FPC}vmovsd [r8 + rdi], xmm2;{$ELSE}db $C4,$C1,$7B,$11,$14,$38;{$ENDIF} 

       add r8, LineWidthA;

   dec rdx;
   jnz @@foryloop;

   // epilog
   mov rbx, iRBX;
   mov rdi, iRDI;
   {$IFDEF FPC}vmovupd xmm4, [rsp + $10];{$ELSE}db $C5,$F9,$10,$64,$24,$10;{$ENDIF} 
   {$IFDEF FPC}vmovupd xmm5, [rsp + $20];{$ELSE}db $C5,$F9,$10,$6C,$24,$20;{$ENDIF} 
   {$IFDEF FPC}vmovupd xmm7, [rsp + $30];{$ELSE}db $C5,$F9,$10,$7C,$24,$30;{$ENDIF} 
   add rsp, $30;
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   @@endproc:
end;
{$IFDEF FPC}
end;
{$ENDIF}

// its assumed that Linewidthdx and linewidthdy = sizeof(double)
// rcx = N, RDX = X, R8 = Y,
procedure AVXMatrixRotateAligned(N : TASMNativeInt; X : PDouble;
  Y : PDouble; LineWidthDY : TASMNativeInt; const c, s : double);
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

   // note c: xmm0

   // ###########################################
   // #### Stack push
   sub rsp, $30;
   {$IFDEF FPC}vmovupd [rsp + $10], xmm4;{$ELSE}db $C5,$F9,$11,$64,$24,$10;{$ENDIF} 
   {$IFDEF FPC}vmovupd [rsp + $20], xmm5;{$ELSE}db $C5,$F9,$11,$6C,$24,$20;{$ENDIF} 
   {$IFDEF FPC}vmovupd [rsp + $30], xmm6;{$ELSE}db $C5,$F9,$11,$74,$24,$30;{$ENDIF} 

   // init
   lea rax, c;
   {$IFDEF FPC}vmovddup xmm1, [rax];{$ELSE}db $C5,$FB,$12,$08;{$ENDIF} 

   lea rax, s;
   {$IFDEF FPC}vmovsd xmm2, [rax];{$ELSE}db $C5,$FB,$10,$10;{$ENDIF} 
   lea rax, [rip + cMinusOne];
   {$IFDEF FPC}vmulsd xmm2, xmm2, [rax];{$ELSE}db $C5,$EB,$59,$10;{$ENDIF} 

   {$IFDEF FPC}vmovddup xmm0, xmm2;{$ELSE}db $C5,$FB,$12,$C2;{$ENDIF} 
   lea rax, s;
   {$IFDEF FPC}vmovddup xmm2, [rax];{$ELSE}db $C5,$FB,$12,$10;{$ENDIF} 

   xor r10, r10;

   mov rax, rcx;
   shr rax, 1;
   test rax, rax;
   jz @@exitLoop;

   @@forNloop:
       // do a full load -> intermediate store in xmm5, and xmm6
       {$IFDEF FPC}vmovupd xmm5, [rdx + r10];      {$ELSE}db $C4,$A1,$79,$10,$2C,$12;{$ENDIF} // x, x+1
       {$IFDEF FPC}vmovupd xmm6, [r8 + r10];      {$ELSE}db $C4,$81,$79,$10,$34,$10;{$ENDIF} // y, y+1

       {$IFDEF FPC}vmulpd xmm3, xmm5, xmm0;  {$ELSE}db $C5,$D1,$59,$D8;{$ENDIF} // x, x+1 * -s
       {$IFDEF FPC}vmulpd xmm5, xmm5, xmm1;  {$ELSE}db $C5,$D1,$59,$E9;{$ENDIF} // x, x+1 * c
       {$IFDEF FPC}vmulpd xmm4, xmm6, xmm1;  {$ELSE}db $C5,$C9,$59,$E1;{$ENDIF} // y, y+1 * c
       {$IFDEF FPC}vmulpd xmm6, xmm6, xmm2;  {$ELSE}db $C5,$C9,$59,$F2;{$ENDIF} // y, y+1 * s

       {$IFDEF FPC}vaddpd xmm5, xmm5, xmm6;  {$ELSE}db $C5,$D1,$58,$EE;{$ENDIF} // c*x + s*y  , c*(x+1) + s*(y+1)
       {$IFDEF FPC}vaddpd xmm3, xmm3, xmm4;  {$ELSE}db $C5,$E1,$58,$DC;{$ENDIF} // -s*x + c*y, -s(x+1) + c*(y+1)

       // write back
       {$IFDEF FPC}vmovupd [rdx + r10], xmm5;{$ELSE}db $C4,$A1,$79,$11,$2C,$12;{$ENDIF} 
       {$IFDEF FPC}vmovupd [r8 + r10], xmm3;{$ELSE}db $C4,$81,$79,$11,$1C,$10;{$ENDIF} 

       add r10, 16;

       dec rax;
   jnz @@forNloop;

   @@exitLoop:

   // test for an odd N
   mov rax, rcx;
   and rax, 1;
   jz @@endProc;

   // handle last element
   {$IFDEF FPC}vmovsd xmm5, [rdx + r10];{$ELSE}db $C4,$A1,$7B,$10,$2C,$12;{$ENDIF} 
   {$IFDEF FPC}vmovsd xmm6, [r8 + r10];{$ELSE}db $C4,$81,$7B,$10,$34,$10;{$ENDIF} 

   //dtemp := c*pX^[i] + s*pY^[i];
   //pY^[i] := - s*pX^[i] + c*pY^[i];
   //px^[i] := dtemp;

   {$IFDEF FPC}vmulsd xmm3, xmm5, xmm0;  {$ELSE}db $C5,$D3,$59,$D8;{$ENDIF} // x * -s
   {$IFDEF FPC}vmulsd xmm4, xmm6, xmm1;  {$ELSE}db $C5,$CB,$59,$E1;{$ENDIF} // y * c
   {$IFDEF FPC}vmulsd xmm5, xmm5, xmm1;  {$ELSE}db $C5,$D3,$59,$E9;{$ENDIF} // x * c
   {$IFDEF FPC}vmulsd xmm6, xmm6, xmm2;  {$ELSE}db $C5,$CB,$59,$F2;{$ENDIF} // y * s

   {$IFDEF FPC}vaddsd xmm5, xmm5, xmm6;  {$ELSE}db $C5,$D3,$58,$EE;{$ENDIF} // c*x + s*y
   {$IFDEF FPC}vaddsd xmm3, xmm3, xmm4;  {$ELSE}db $C5,$E3,$58,$DC;{$ENDIF} // -s*x + c*y

   // write back
   {$IFDEF FPC}vmovsd [rdx + r10], xmm5;{$ELSE}db $C4,$A1,$7B,$11,$2C,$12;{$ENDIF} 
   {$IFDEF FPC}vmovsd [r8 + r10], xmm3;{$ELSE}db $C4,$81,$7B,$11,$1C,$10;{$ENDIF} 

   @@endProc:

   // ###########################################
   // #### epilog
   {$IFDEF FPC}vmovupd xmm4, [rsp + $10];{$ELSE}db $C5,$F9,$10,$64,$24,$10;{$ENDIF} 
   {$IFDEF FPC}vmovupd xmm5, [rsp + $20];{$ELSE}db $C5,$F9,$10,$6C,$24,$20;{$ENDIF} 
   {$IFDEF FPC}vmovupd xmm6, [rsp + $30];{$ELSE}db $C5,$F9,$10,$74,$24,$30;{$ENDIF} 
   add rsp, $30;
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;
{$IFDEF FPC}
end;
{$ENDIF}

// rcx = N, RDX = X, R8 = LineWidthDX, R9 = Y;
procedure AVXMatrixRotateUnaligned(N : TASMNativeInt; X : PDouble; const LineWidthDX : TASMNativeInt;
  Y : PDouble; LineWidthDY : TASMNativeInt; const c, s : double);
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
   sub rsp, $30;
   {$IFDEF FPC}vmovupd [rsp + $10], xmm4;{$ELSE}db $C5,$F9,$11,$64,$24,$10;{$ENDIF} 
   {$IFDEF FPC}vmovupd [rsp + $20], xmm5;{$ELSE}db $C5,$F9,$11,$6C,$24,$20;{$ENDIF} 
   {$IFDEF FPC}vmovupd [rsp + $30], xmm6;{$ELSE}db $C5,$F9,$11,$74,$24,$30;{$ENDIF} 

   lea rax, c;
   {$IFDEF FPC}vmovddup xmm1, [rax];{$ELSE}db $C5,$FB,$12,$08;{$ENDIF} 

   lea rax, s;
   {$IFDEF FPC}vmovsd xmm2, [rax];{$ELSE}db $C5,$FB,$10,$10;{$ENDIF} 
   lea rax, [rip + cMinusOne];
   {$IFDEF FPC}vmulsd xmm2, xmm2, [rax];{$ELSE}db $C5,$EB,$59,$10;{$ENDIF} 

   {$IFDEF FPC}vmovddup xmm0, xmm2;{$ELSE}db $C5,$FB,$12,$C2;{$ENDIF} 

   lea rax, s;
   {$IFDEF FPC}vmovddup xmm2, [rax];{$ELSE}db $C5,$FB,$12,$10;{$ENDIF} 

   mov r10, LineWidthDY;

   // ###########################################
   // #### loop
   mov rax, rcx;
   shr rax, 1;
   test rax, rax;
   jz @@exitLoop;

   @@forNloop:
       // do a full load -> intermediate store in xmm5, and xmm6
       {$IFDEF FPC}vmovlpd xmm5, xmm5, [rdx];    {$ELSE}db $C5,$D1,$12,$2A;{$ENDIF} // load x, x+1
       {$IFDEF FPC}vmovhpd xmm5, xmm5, [rdx + r8];{$ELSE}db $C4,$A1,$51,$16,$2C,$02;{$ENDIF} 
       {$IFDEF FPC}vmovlpd xmm6, xmm6, [r9];    {$ELSE}db $C4,$C1,$49,$12,$31;{$ENDIF} // load y, y+1
       {$IFDEF FPC}vmovhpd xmm6, xmm6, [r9 + r10];{$ELSE}db $C4,$81,$49,$16,$34,$11;{$ENDIF} 

       {$IFDEF FPC}vmulpd xmm3, xmm5, xmm0;  {$ELSE}db $C5,$D1,$59,$D8;{$ENDIF} // x, x+1 * -s
       {$IFDEF FPC}vmulpd xmm5, xmm5, xmm1;  {$ELSE}db $C5,$D1,$59,$E9;{$ENDIF} // x, x+1 * c
       {$IFDEF FPC}vmulpd xmm4, xmm6, xmm1;  {$ELSE}db $C5,$C9,$59,$E1;{$ENDIF} // y, y+1 * c
       {$IFDEF FPC}vmulpd xmm6, xmm6, xmm2;  {$ELSE}db $C5,$C9,$59,$F2;{$ENDIF} // y, y+1 * s


       {$IFDEF FPC}vaddpd xmm5, xmm5, xmm6;  {$ELSE}db $C5,$D1,$58,$EE;{$ENDIF} // c*x + s*y  , c*(x+1) + s*(y+1)
       {$IFDEF FPC}vaddpd xmm3, xmm3, xmm4;  {$ELSE}db $C5,$E1,$58,$DC;{$ENDIF} // -s*x + c*y, -s(x+1) + c*(y+1)


       // write back
       {$IFDEF FPC}vmovlpd [rdx], xmm5;{$ELSE}db $C5,$F9,$13,$2A;{$ENDIF} 
       {$IFDEF FPC}vmovhpd [rdx + r8], xmm5;{$ELSE}db $C4,$A1,$79,$17,$2C,$02;{$ENDIF} 

       {$IFDEF FPC}vmovlpd [r9], xmm3;{$ELSE}db $C4,$C1,$79,$13,$19;{$ENDIF} 
       {$IFDEF FPC}vmovhpd [r9 + r10], xmm3;{$ELSE}db $C4,$81,$79,$17,$1C,$11;{$ENDIF} 

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
   {$IFDEF FPC}vmovsd xmm5, [rdx];{$ELSE}db $C5,$FB,$10,$2A;{$ENDIF} 
   {$IFDEF FPC}vmovsd xmm6, [r9];{$ELSE}db $C4,$C1,$7B,$10,$31;{$ENDIF} 

   //dtemp := c*pX^[i] + s*pY^[i];
   //pY^[i] := - s*pX^[i] + c*pY^[i];
   //px^[i] := dtemp;
   //vmovsd xmm3, xmm5;
   //vmovsd xmm4, xmm6;

   {$IFDEF FPC}vmulsd xmm3, xmm5, xmm0;  {$ELSE}db $C5,$D3,$59,$D8;{$ENDIF} // x * -s
   {$IFDEF FPC}vmulsd xmm5, xmm5, xmm1;  {$ELSE}db $C5,$D3,$59,$E9;{$ENDIF} // x * c
   {$IFDEF FPC}vmulsd xmm4, xmm6, xmm1;  {$ELSE}db $C5,$CB,$59,$E1;{$ENDIF} // y * c
   {$IFDEF FPC}vmulsd xmm6, xmm6, xmm2;  {$ELSE}db $C5,$CB,$59,$F2;{$ENDIF} // y * s

   {$IFDEF FPC}vaddsd xmm5, xmm5, xmm6;  {$ELSE}db $C5,$D3,$58,$EE;{$ENDIF} // c*x + s*y
   {$IFDEF FPC}vaddsd xmm3, xmm3, xmm4;  {$ELSE}db $C5,$E3,$58,$DC;{$ENDIF} // -s*x + c*y

   // write back
   {$IFDEF FPC}vmovsd [rdx], xmm5;{$ELSE}db $C5,$FB,$11,$2A;{$ENDIF} 
   {$IFDEF FPC}vmovsd [r9], xmm3;{$ELSE}db $C4,$C1,$7B,$11,$19;{$ENDIF} 

   @@endProc:

   // ###########################################
   // #### Epilog
   {$IFDEF FPC}vmovupd xmm4, [rsp + $10];{$ELSE}db $C5,$F9,$10,$64,$24,$10;{$ENDIF} 
   {$IFDEF FPC}vmovupd xmm5, [rsp + $20];{$ELSE}db $C5,$F9,$10,$6C,$24,$20;{$ENDIF} 
   {$IFDEF FPC}vmovupd xmm6, [rsp + $30];{$ELSE}db $C5,$F9,$10,$74,$24,$30;{$ENDIF} 
   add rsp, $30;
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 
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
         AVXMatrixRotateAligned(N, X, Y, LineWidthDY, c, s)
     else
         AVXMatrixRotateUnAligned(N, X, LineWidthDX, Y, LineWidthDY, c, s);
end;


{$ENDIF}

end.
