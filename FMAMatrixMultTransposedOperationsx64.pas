// ###################################################################
// #### This file is part of the mathematics library project, and is
// #### offered under the licence agreement described on
// #### http://www.mrsoft.org/
// ####
// #### Copyright:(c) 2018, Michael R. . All rights reserved.
// ####
// #### Unless required by applicable law or agreed to in writing, software
// #### distributed under the License is distributed on an "AS IS" BASIS,
// #### WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// #### See the License for the specific language governing permissions and
// #### limitations under the License.
// ###################################################################

unit FMAMatrixMultTransposedOperationsx64;


// ##############################################################
// #### Assembler optimized matrix multiplication assuming a transposed second
// #### matrix
// ##############################################################

interface

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF x64}

uses MatrixConst;

procedure FMAMatrixMultAlignedTransposed(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; {$ifdef UNIX}unixWidth1{$ELSE}width1{$endif} : TASMNativeInt; {$ifdef UNIX}unixHeight1{$ELSE}height1{$endif}  : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}
procedure FMAMatrixMultAlignedEvenW1EvenH2TransposedMod16(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; {$ifdef UNIX}unixWidth1{$ELSE}width1{$endif} : TASMNativeInt; {$ifdef UNIX}unixHeight1{$ELSE}height1{$endif}  : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}
procedure FMAMatrixMultAlignedEvenW1OddH2TransposedMod16(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; {$ifdef UNIX}unixWidth1{$ELSE}width1{$endif} : TASMNativeInt; {$ifdef UNIX}unixHeight1{$ELSE}height1{$endif}  : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}
procedure FMAMatrixMultUnAlignedTransposed(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; {$ifdef UNIX}unixWidth1{$ELSE}width1{$endif} : TASMNativeInt; {$ifdef UNIX}unixHeight1{$ELSE}height1{$endif}  : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}

{$ENDIF}

implementation

{$IFDEF x64}

{$IFDEF FPC} {$ASMMODE intel}  {$warnings off} {$ENDIF}

procedure FMAMatrixMultAlignedEvenW1EvenH2TransposedMod16(dest : PDouble; const destLineWidth : TASMNativeInt;
    mt1, mt2 : PDouble; {$ifdef UNIX}unixWidth1{$ELSE}width1{$endif} : TASMNativeInt; {$ifdef UNIX}unixHeight1{$ELSE}height1{$endif}  : TASMNativeInt;
    width2 : TASMNativeInt; height2 : TASMNativeInt;
    const LineWidth1, LineWidth2 : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}
var iRBX, iRSI, iRDI, iR12, iR13, iR14, iR15 : int64;
    {$IFDEF UNIX}
    width1 : TASMNativeInt;
    height1 : TASMNativeInt;
    {$ENDIF}
asm
   {$IFDEF UNIX}
   // Linux uses a diffrent ABI -> copy over the registers so they meet with winABI
   // (note that the 5th and 6th parameter are are on the stack)
   // The parameters are passed in the following order:
   // RDI, RSI, RDX, RCX -> mov to RCX, RDX, R8, R9
   mov width1, r8;
   mov height1, r9;
   mov r8, rdx;
   mov r9, rcx;
   mov rcx, rdi;
   mov rdx, rsi;
   {$ENDIF}

   // prolog - simulate stack
   mov iRBX, rbx;
   mov iRSI, rsi;
   mov iRDI, rdi;
   mov iR12, r12;
   mov iR13, r13;
   mov iR14, r14;
   mov iR15, r15;

   sub rsp, $10;
   {$IFDEF FPC}vmovupd [rsp + $00], xmm4;{$ELSE}db $C5,$F9,$11,$24,$24;{$ENDIF} 

   // iters1 := height2 div 2;
   mov r15, height2;
   shr r15, 1;

   // iters2 := -width1*sizeof(double);
   mov r14, width1;
   shl r14, 3;
   imul r14, -1;

   // LineWidth2_2 := 2*LineWidth2;
   mov r13, LineWidth2;

   mov r12, r13;
   shl r12, 1;

   // prepare matrix pointers - remove constant offset here instead each time in the loop:
   sub r8, r14;
   sub r9, r14;
   mov r10, r9;
   add r10, r13;

   // for y := 0 to height1 - 1:
   mov r11, Height1;
   @@forylabel:
       mov rdi, r9;
       mov rsi, r10;
       mov r13, rcx;

       // for y2 := 0 to height2 - 1:
       mov rbx, r15;
       @@fory2label:
           {$IFDEF FPC}vxorpd ymm0, ymm0, ymm0;   {$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} // dest^ := 0
           {$IFDEF FPC}vxorpd ymm2, ymm2, ymm2;   {$ELSE}db $C5,$ED,$57,$D2;{$ENDIF} // dest + 1 := 0;
           // for idx := 0 to width1 div 2 do
           mov rax, r14;

           // for x := 0 to width1 - 1
           @@InnerLoop:
               // prefetch [ecx + edx + 128];
               // prefetch [edi + edx + 128];
               // prefetch [eax + edx + 128];

               {$IFDEF FPC}vmovapd ymm1, [r8 + rax];{$ELSE}db $C4,$C1,$7D,$28,$0C,$00;{$ENDIF} 

               // multiply 2x2 and add
               {$IFDEF FPC}vfmadd231pd ymm0, ymm1, [rdi + rax];{$ELSE}db $C4,$E2,$F5,$B8,$04,$07;{$ENDIF} 
               {$IFDEF FPC}vfmadd231pd ymm2, ymm1, [rsi + rax];{$ELSE}db $C4,$E2,$F5,$B8,$14,$06;{$ENDIF} 

               {$IFDEF FPC}vmovapd ymm1, [r8 + rax + 32];{$ELSE}db $C4,$C1,$7D,$28,$4C,$00,$20;{$ENDIF} 

               // multiply 2x2 and add
               {$IFDEF FPC}vfmadd231pd ymm0, ymm1, [rdi + rax + 32];{$ELSE}db $C4,$E2,$F5,$B8,$44,$07,$20;{$ENDIF} 
               {$IFDEF FPC}vfmadd231pd ymm2, ymm1, [rsi + rax + 32];{$ELSE}db $C4,$E2,$F5,$B8,$54,$06,$20;{$ENDIF} 

               {$IFDEF FPC}vmovapd ymm1, [r8 + rax + 64];{$ELSE}db $C4,$C1,$7D,$28,$4C,$00,$40;{$ENDIF} 

               // load 2x2 block
               {$IFDEF FPC}vfmadd231pd ymm0, ymm1, [rdi + rax + 64];{$ELSE}db $C4,$E2,$F5,$B8,$44,$07,$40;{$ENDIF} 
               {$IFDEF FPC}vfmadd231pd ymm2, ymm1, [rsi + rax + 64];{$ELSE}db $C4,$E2,$F5,$B8,$54,$06,$40;{$ENDIF} 

               {$IFDEF FPC}vmovapd ymm1, [r8 + rax + 96];{$ELSE}db $C4,$C1,$7D,$28,$4C,$00,$60;{$ENDIF} 

               // multiply 2x2 and add
               {$IFDEF FPC}vfmadd231pd ymm0, ymm1, [rdi + rax + 96];{$ELSE}db $C4,$E2,$F5,$B8,$44,$07,$60;{$ENDIF} 
               {$IFDEF FPC}vfmadd231pd ymm2, ymm1, [rsi + rax + 96];{$ELSE}db $C4,$E2,$F5,$B8,$54,$06,$60;{$ENDIF} 
               // end for idx := 0 to width1 div 2
           add rax, 128;
           jnz @@InnerLoop;

           {$IFDEF FPC}vextractf128 xmm1, ymm0, 1;{$ELSE}db $C4,$E3,$7D,$19,$C1,$01;{$ENDIF} 
           {$IFDEF FPC}vextractf128 xmm3, ymm2, 1;{$ELSE}db $C4,$E3,$7D,$19,$D3,$01;{$ENDIF} 

           {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm1;{$ELSE}db $C5,$F9,$7C,$C1;{$ENDIF} 
           {$IFDEF FPC}vhaddpd xmm2, xmm2, xmm3;{$ELSE}db $C5,$E9,$7C,$D3;{$ENDIF} 

           {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm2;{$ELSE}db $C5,$F9,$7C,$C2;{$ENDIF} 

           // store back result
           {$IFDEF FPC}vmovapd [r13], xmm0;{$ELSE}db $C4,$C1,$79,$29,$45,$00;{$ENDIF} 

           // increment the pointers
           // inc(mt2), inc(dest);
           //add dword ptr [mt2], 8;
           add r13, 16;
           add rdi, r12;
           add rsi, r12;
       // end for x := 0 to width2 - 1
       dec rbx;
       jnz @@fory2label;

       // dec(mt2, Width2);
       // inc(PByte(mt1), LineWidth1);
       // inc(PByte(dest), destOffset);
       //mov ebx, bytesWidth2;
       //sub dword ptr [mt2], ebx;
       add r8, LineWidth1;
       add rcx, rdx;

   // end for y := 0 to height1 - 1
   dec r11;
   jnz @@forylabel;

   // epilog - cleanup stack
   mov rbx, iRBX;
   mov rsi, iRSI;
   mov rdi, iRDI;
   mov r12, iR12;
   mov r13, iR13;
   mov r14, iR14;
   mov r15, iR15;

   {$IFDEF FPC}vmovupd xmm4, [rsp + $00];{$ELSE}db $C5,$F9,$10,$24,$24;{$ENDIF} 
   add rsp, $10;
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;

procedure FMAMatrixMultAlignedEvenW1OddH2TransposedMod16(dest : PDouble; const destLineWidth : TASMNativeInt;
    mt1, mt2 : PDouble; {$ifdef UNIX}unixWidth1{$ELSE}width1{$endif} : TASMNativeInt; {$ifdef UNIX}unixHeight1{$ELSE}height1{$endif}  : TASMNativeInt;
    width2 : TASMNativeInt; height2 : TASMNativeInt;
    const LineWidth1, LineWidth2 : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}
var iRBX, iRSI, iRDI, iR12, iR13, iR14, iR15 : int64;
    {$IFDEF UNIX}
    width1 : TASMNativeInt;
    height1 : TASMNativeInt;
    {$ENDIF}

asm
   {$IFDEF UNIX}
   // Linux uses a diffrent ABI -> copy over the registers so they meet with winABI
   // (note that the 5th and 6th parameter are are on the stack)
   // The parameters are passed in the following order:
   // RDI, RSI, RDX, RCX -> mov to RCX, RDX, R8, R9
   mov width1, r8;
   mov height1, r9;
   mov r8, rdx;
   mov r9, rcx;
   mov rcx, rdi;
   mov rdx, rsi;
   {$ENDIF}

   // prolog - simulate stack
   mov iRBX, rbx;
   mov iRSI, rsi;
   mov iRDI, rdi;
   mov iR12, r12;
   mov iR13, r13;
   mov iR14, r14;
   mov iR15, r15;

   sub rsp, $10;
   {$IFDEF FPC}vmovupd [rsp + $00], xmm4;{$ELSE}db $C5,$F9,$11,$24,$24;{$ENDIF} 

   // iters1 := height2 div 2;
   mov r15, height2;
   shr r15, 1;

   // iters2 := -width1*sizeof(double);
   mov r14, width1;
   shl r14, 3;
   imul r14, -1;

   // LineWidth2_2 := 2*LineWidth2;
   mov r13, LineWidth2;

   mov r12, r13;
   shl r12, 1;

   // prepare matrix pointers - remove constant offset here instead each time in the loop:
   sub r8, r14;
   sub r9, r14;
   mov r10, r9;
   add r10, r13;

   // for y := 0 to height1 - 1:
   mov r11, Height1;
   @@forylabel:
       mov rdi, r9;
       mov rsi, r10;
       mov r13, rcx;

       // for y2 := 0 to height2 div 2 - 1:
       mov rbx, r15;
       @@fory2label:
           {$IFDEF FPC}vxorpd ymm0, ymm0, ymm0;   {$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} // dest^ := 0
           {$IFDEF FPC}vxorpd ymm2, ymm2, ymm2;   {$ELSE}db $C5,$ED,$57,$D2;{$ENDIF} // dest + 1 := 0;
           // for idx := 0 to width1 div 2 do
           mov rax, r14;

           // for x := 0 to width1 - 1
           @@InnerLoop:
               {$IFDEF FPC}vmovapd ymm1, [r8 + rax];{$ELSE}db $C4,$C1,$7D,$28,$0C,$00;{$ENDIF} 

               // multiply 2x2 and add
               {$IFDEF FPC}vfmadd231pd ymm0, ymm1, [rdi + rax];{$ELSE}db $C4,$E2,$F5,$B8,$04,$07;{$ENDIF} 
               {$IFDEF FPC}vfmadd231pd ymm2, ymm1, [rsi + rax];{$ELSE}db $C4,$E2,$F5,$B8,$14,$06;{$ENDIF} 

               {$IFDEF FPC}vmovapd ymm1, [r8 + rax + 32];{$ELSE}db $C4,$C1,$7D,$28,$4C,$00,$20;{$ENDIF} 

               // multiply 2x2 and add
               {$IFDEF FPC}vfmadd231pd ymm0, ymm1, [rdi + rax + 32];{$ELSE}db $C4,$E2,$F5,$B8,$44,$07,$20;{$ENDIF} 
               {$IFDEF FPC}vfmadd231pd ymm2, ymm1, [rsi + rax + 32];{$ELSE}db $C4,$E2,$F5,$B8,$54,$06,$20;{$ENDIF} 

               {$IFDEF FPC}vmovapd ymm1, [r8 + rax + 64];{$ELSE}db $C4,$C1,$7D,$28,$4C,$00,$40;{$ENDIF} 

               // load 2x2 block
               {$IFDEF FPC}vfmadd231pd ymm0, ymm1, [rdi + rax + 64];{$ELSE}db $C4,$E2,$F5,$B8,$44,$07,$40;{$ENDIF} 
               {$IFDEF FPC}vfmadd231pd ymm2, ymm1, [rsi + rax + 64];{$ELSE}db $C4,$E2,$F5,$B8,$54,$06,$40;{$ENDIF} 

               {$IFDEF FPC}vmovapd ymm1, [r8 + rax + 96];{$ELSE}db $C4,$C1,$7D,$28,$4C,$00,$60;{$ENDIF} 

               // multiply 2x2 and add
               {$IFDEF FPC}vfmadd231pd ymm0, ymm1, [rdi + rax + 96];{$ELSE}db $C4,$E2,$F5,$B8,$44,$07,$60;{$ENDIF} 
               {$IFDEF FPC}vfmadd231pd ymm2, ymm1, [rsi + rax + 96];{$ELSE}db $C4,$E2,$F5,$B8,$54,$06,$60;{$ENDIF} 
               // end for idx := 0 to width1 div 2

               // end for idx := 0 to width1 div 2
           add rax, 128;
           jnz @@InnerLoop;

           {$IFDEF FPC}vextractf128 xmm1, ymm0, 1;{$ELSE}db $C4,$E3,$7D,$19,$C1,$01;{$ENDIF} 
           {$IFDEF FPC}vextractf128 xmm3, ymm2, 1;{$ELSE}db $C4,$E3,$7D,$19,$D3,$01;{$ENDIF} 

           {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm1;{$ELSE}db $C5,$F9,$7C,$C1;{$ENDIF} 
           {$IFDEF FPC}vhaddpd xmm2, xmm2, xmm3;{$ELSE}db $C5,$E9,$7C,$D3;{$ENDIF} 

           {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm2;{$ELSE}db $C5,$F9,$7C,$C2;{$ENDIF} 

           // store back result
           {$IFDEF FPC}vmovapd [r13], xmm0;{$ELSE}db $C4,$C1,$79,$29,$45,$00;{$ENDIF} 

           // increment the pointers
           // inc(mt2), inc(dest);
           //add dword ptr [mt2], 8;
           add r13, 16;
           add rdi, r12;
           add rsi, r12;
       // end for y := 0 to height2 div 2 - 1
       dec rbx;
       jnz @@fory2label;

       // last odd line:
       {$IFDEF FPC}vxorpd ymm0, ymm0, ymm0;   {$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} // dest^ := 0
       // for idx := 0 to width1 div 2 do
       mov rax, r14;

       // for x := 0 to width1 - 1
       @@InnerLoop2:
          // prefetch [ecx + edx + 128];
          // prefetch [edi + edx + 128];
          // prefetch [eax + edx + 128];

          {$IFDEF FPC}vmovapd ymm1, [r8 + rax];{$ELSE}db $C4,$C1,$7D,$28,$0C,$00;{$ENDIF} 

          {$IFDEF FPC}vfmadd231pd ymm0, ymm1, [rdi + rax];{$ELSE}db $C4,$E2,$F5,$B8,$04,$07;{$ENDIF} 

          {$IFDEF FPC}vmovapd ymm1, [r8 + rax + 32];{$ELSE}db $C4,$C1,$7D,$28,$4C,$00,$20;{$ENDIF} 
          {$IFDEF FPC}vfmadd231pd ymm0, ymm1, [rdi + rax + 32];{$ELSE}db $C4,$E2,$F5,$B8,$44,$07,$20;{$ENDIF} 

          {$IFDEF FPC}vmovapd ymm1, [r8 + rax + 64];{$ELSE}db $C4,$C1,$7D,$28,$4C,$00,$40;{$ENDIF} 
          {$IFDEF FPC}vfmadd231pd ymm0, ymm1, [rdi +rax + 64];{$ELSE}db $C4,$E2,$F5,$B8,$44,$07,$40;{$ENDIF} 

          {$IFDEF FPC}vmovapd ymm1, [r8 + rax + 96];{$ELSE}db $C4,$C1,$7D,$28,$4C,$00,$60;{$ENDIF} 
          {$IFDEF FPC}vfmadd231pd ymm0, ymm1, [rdi + rax +96];{$ELSE}db $C4,$E2,$F5,$B8,$44,$07,$60;{$ENDIF} 

          // end for idx := 0 to width1 div 2
       add rax, 128;
       jnz @@InnerLoop2;

       {$IFDEF FPC}vextractf128 xmm1, ymm0, 1;{$ELSE}db $C4,$E3,$7D,$19,$C1,$01;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm1;{$ELSE}db $C5,$F9,$7C,$C1;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} 

       // store back result
       {$IFDEF FPC}vmovsd [r13], xmm0;{$ELSE}db $C4,$C1,$7B,$11,$45,$00;{$ENDIF} 

       // dec(mt2, Width2);
       // inc(PByte(mt1), LineWidth1);
       // inc(PByte(dest), destOffset);
       //mov ebx, bytesWidth2;
       //sub dword ptr [mt2], ebx;
       add r8, LineWidth1;
       add rcx, rdx;

   // end for y := 0 to height1 - 1
   dec r11;
   jnz @@forylabel;

   // epilog - cleanup stack
   mov rbx, iRBX;
   mov rsi, iRSI;
   mov rdi, iRDI;
   mov r12, iR12;
   mov r13, iR13;
   mov r14, iR14;
   mov r15, iR15;

   {$IFDEF FPC}vmovupd xmm4, [rsp + $00];{$ELSE}db $C5,$F9,$10,$24,$24;{$ENDIF} 
   add rsp, $10;
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;


// note mt2 is transposed this time -> width1 and width2 must be the same!
procedure FMAMatrixMultUnAlignedTransposed(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; {$ifdef UNIX}unixWidth1{$ELSE}width1{$endif} : TASMNativeInt; {$ifdef UNIX}unixHeight1{$ELSE}height1{$endif}  : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}
var iRBX, iRSI, iRDI, iR12, iR13, iR14, iR15 : int64;
    {$IFDEF UNIX}
    width1 : TASMNativeInt;
    height1 : TASMNativeInt;
    {$ENDIF}
asm
   {$IFDEF UNIX}
   // Linux uses a diffrent ABI -> copy over the registers so they meet with winABI
   // (note that the 5th and 6th parameter are are on the stack)
   // The parameters are passed in the following order:
   // RDI, RSI, RDX, RCX -> mov to RCX, RDX, R8, R9
   mov width1, r8;
   mov height1, r9;
   mov r8, rdx;
   mov r9, rcx;
   mov rcx, rdi;
   mov rdx, rsi;
   {$ENDIF}

   // prolog - simulate stack
   mov iRBX, rbx;
   mov iRSI, rsi;
   mov iRDI, rdi;
   mov iR12, r12;
   mov iR13, r13;
   mov iR14, r14;
   mov iR15, r15;

   sub rsp, $10;
   {$IFDEF FPC}vmovupd [rsp + $00], xmm4;{$ELSE}db $C5,$F9,$11,$24,$24;{$ENDIF} 

   // height 2
   mov r15, height2;

   // iters2 := -width1*sizeof(double);
   mov r14, width1;
   shl r14, 3;
   imul r14, -1;

   // LineWidth2_2 := 2*LineWidth2;
   mov r13, LineWidth2;

   mov r12, r13;
   shl r12, 1;

   // prepare matrix pointers - remove constant offset here instead each time in the loop:
   sub r8, r14;
   sub r9, r14;
   mov r10, r9;
   add r10, r13;

   // for y := 0 to height1 - 1:
   mov r11, Height1;
   @@forylabel:
       mov rdi, r9;
       mov rsi, r10;
       mov r13, rcx;

       // for x := 0 to width2 - 1:
       mov rbx, r15;
       sub rbx, 2;
       jl @@fory2loopend;

       @@fory2label:
           {$IFDEF FPC}vxorpd ymm0, ymm0, ymm0;   {$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} // dest^ := 0
           {$IFDEF FPC}vxorpd ymm2, ymm2, ymm2;   {$ELSE}db $C5,$ED,$57,$D2;{$ENDIF} // dest + 1 := 0;
           // for idx := 0 to width1 div 4 do
           mov rax, r14;
           add rax, 32;
           jg @InnerLoopEnd;

           @@InnerLoop:
               {$IFDEF FPC}vmovupd ymm1, [r8 + rax - 32];{$ELSE}db $C4,$C1,$7D,$10,$4C,$00,$E0;{$ENDIF} 

               // load 2x2 block
               {$IFDEF FPC}vmovupd ymm3, [rdi + rax - 32];{$ELSE}db $C5,$FD,$10,$5C,$07,$E0;{$ENDIF} 
               {$IFDEF FPC}vmovupd ymm4, [rsi + rax - 32];{$ELSE}db $C5,$FD,$10,$64,$06,$E0;{$ENDIF} 

               {$IFDEF FPC}vfmadd231pd ymm0, ymm1, ymm3;{$ELSE}db $C4,$E2,$F5,$B8,$C3;{$ENDIF} 
               {$IFDEF FPC}vfmadd231pd ymm2, ymm1, ymm4;{$ELSE}db $C4,$E2,$F5,$B8,$D4;{$ENDIF} 
               // end for idx := 0 to width1 div 2
           add rax, 32;
           jle @@InnerLoop;

           {$IFDEF FPC}vextractf128 xmm1, ymm0, 1;{$ELSE}db $C4,$E3,$7D,$19,$C1,$01;{$ENDIF} 
           {$IFDEF FPC}vextractf128 xmm3, ymm2, 1;{$ELSE}db $C4,$E3,$7D,$19,$D3,$01;{$ENDIF} 

           {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm1;{$ELSE}db $C5,$F9,$7C,$C1;{$ENDIF} 
           {$IFDEF FPC}vhaddpd xmm2, xmm2, xmm3;{$ELSE}db $C5,$E9,$7C,$D3;{$ENDIF} 

           @InnerLoopEnd:

           sub rax, 32;
           jz @@InnerLoopEnd2;

           // do the final few elements
           @@InnerLoop2:
               {$IFDEF FPC}vmovsd xmm1, [r8 + rax];{$ELSE}db $C4,$C1,$7B,$10,$0C,$00;{$ENDIF} 

               // load 2x2 block
               {$IFDEF FPC}vmovsd xmm3, [rdi + rax];{$ELSE}db $C5,$FB,$10,$1C,$07;{$ENDIF} 
               {$IFDEF FPC}vmovsd xmm4, [rsi + rax];{$ELSE}db $C5,$FB,$10,$24,$06;{$ENDIF} 

               // multiply 2x2 and add
               {$IFDEF FPC}vmulsd xmm3, xmm3, xmm1;{$ELSE}db $C5,$E3,$59,$D9;{$ENDIF} 
               {$IFDEF FPC}vmulsd xmm4, xmm4, xmm1;{$ELSE}db $C5,$DB,$59,$E1;{$ENDIF} 

               {$IFDEF FPC}vaddsd xmm0, xmm0, xmm3;{$ELSE}db $C5,$FB,$58,$C3;{$ENDIF} 
               {$IFDEF FPC}vaddsd xmm2, xmm2, xmm4;{$ELSE}db $C5,$EB,$58,$D4;{$ENDIF} 

           add rax, 8;
           jnz @@InnerLoop2;

           @@InnerLoopEnd2:

           // final add and compact result
           {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm2;{$ELSE}db $C5,$F9,$7C,$C2;{$ENDIF} 

           // store back result
           {$IFDEF FPC}vmovupd [r13], xmm0;{$ELSE}db $C4,$C1,$79,$11,$45,$00;{$ENDIF} 

           // increment the pointers
           // inc(mt2), inc(dest);
           //add dword ptr [mt2], 8;
           add r13, 16;
           add rdi, r12;
           add rsi, r12;
       // end for x := 0 to width2 - 1
       sub rbx, 2;
       jge @@fory2label;

       @@fory2loopend:
       add rbx, 2;

       // test for odd h2
       jz @@NextLine;

       // we have an odd height2 -> special treatment for the last line
       {$IFDEF FPC}vxorpd ymm0, ymm0, ymm0;   {$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} // dest^ := 0

       // for idx := 0 to width1 div 4 do
       mov rax, r14;
       add rax, 32;
       jg @LastLineInnerLoopEnd;

       @@LastLineInnerLoop:
            {$IFDEF FPC}vmovupd ymm1, [r8 + rax - 32];{$ELSE}db $C4,$C1,$7D,$10,$4C,$00,$E0;{$ENDIF} 

            // load block
            {$IFDEF FPC}vmovupd ymm3, [rdi + rax - 32];{$ELSE}db $C5,$FD,$10,$5C,$07,$E0;{$ENDIF} 

            // multiply and add
            {$IFDEF FPC}vfmadd231pd ymm0, ymm1, ymm3;{$ELSE}db $C4,$E2,$F5,$B8,$C3;{$ENDIF} 
            // end for idx := 0 to width1 div 2
       add rax, 32;
       jle @@LastLineInnerLoop;

       {$IFDEF FPC}vextractf128 xmm1, ymm0, 1;{$ELSE}db $C4,$E3,$7D,$19,$C1,$01;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm1;{$ELSE}db $C5,$F9,$7C,$C1;{$ENDIF} 

       @LastLineInnerLoopEnd:
       sub rax, 32;
       jz @@LastLineInnerLoopEnd2;

       // do the final few elements
       @@LastLineInnerLoop2:
            {$IFDEF FPC}vmovsd xmm1, [r8 + rax];{$ELSE}db $C4,$C1,$7B,$10,$0C,$00;{$ENDIF} 
            {$IFDEF FPC}vmovsd xmm3, [rdi + rax];{$ELSE}db $C5,$FB,$10,$1C,$07;{$ENDIF} 

            // multiply and add
            {$IFDEF FPC}vmulsd xmm3, xmm3, xmm1;{$ELSE}db $C5,$E3,$59,$D9;{$ENDIF} 
            {$IFDEF FPC}vaddsd xmm0, xmm0, xmm3;{$ELSE}db $C5,$FB,$58,$C3;{$ENDIF} 
       // next element
       add rax, 8;
       jnz @@LastLineInnerLoop2;

       @@LastLineInnerLoopEnd2:

       // final add and compact result
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} 

       // store back result
       {$IFDEF FPC}vmovsd [r13], xmm0;{$ELSE}db $C4,$C1,$7B,$11,$45,$00;{$ENDIF} 

       // ################################################
       // #### next line of mt1
       @@NextLine:
       // dec(mt2, Width2);
       // inc(PByte(mt1), LineWidth1);
       // inc(PByte(dest), destOffset);
       //mov ebx, bytesWidth2;
       //sub dword ptr [mt2], ebx;
       add r8, LineWidth1;
       add rcx, rdx;

   // end for y := 0 to height1 - 1
   dec r11;
   jnz @@forylabel;

   // epilog - cleanup stack
   mov rbx, iRBX;
   mov rsi, iRSI;
   mov rdi, iRDI;
   mov r12, iR12;
   mov r13, iR13;
   mov r14, iR14;
   mov r15, iR15;

   {$IFDEF FPC}vmovupd xmm4, [rsp + $00];{$ELSE}db $C5,$F9,$10,$24,$24;{$ENDIF} 
   add rsp, $10;
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;

// note mt2 is transposed this time -> width1 and width2 must be the same!
procedure FMAMatrixMultAlignedTransposed(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; {$ifdef UNIX}unixWidth1{$ELSE}width1{$endif} : TASMNativeInt; {$ifdef UNIX}unixHeight1{$ELSE}height1{$endif}  : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}
var iRBX, iRSI, iRDI, iR12, iR13, iR14, iR15 : int64;
{$IFDEF UNIX}
width1 : TASMNativeInt;
height1 : TASMNativeInt;
{$ENDIF}

asm
   {$IFDEF UNIX}
   // Linux uses a diffrent ABI -> copy over the registers so they meet with winABI
   // (note that the 5th and 6th parameter are are on the stack)
   // The parameters are passed in the following order:
   // RDI, RSI, RDX, RCX -> mov to RCX, RDX, R8, R9
   mov width1, r8;
   mov height1, r9;
   mov r8, rdx;
   mov r9, rcx;
   mov rcx, rdi;
   mov rdx, rsi;
   {$ENDIF}

   // prolog - simulate stack
   mov iRBX, rbx;
   mov iRSI, rsi;
   mov iRDI, rdi;
   mov iR12, r12;
   mov iR13, r13;
   mov iR14, r14;
   mov iR15, r15;

   sub rsp, $10;
   {$IFDEF FPC}vmovupd [rsp + $00], xmm4;{$ELSE}db $C5,$F9,$11,$24,$24;{$ENDIF} 

   // height2 helper
   mov r15, height2;

   // iters2 := -width1*sizeof(double);
   mov r14, width1;
   shl r14, 3;
   imul r14, -1;

   // LineWidth2_2 := 2*LineWidth2;
   mov r13, LineWidth2;

   mov r12, r13;
   shl r12, 1;

   // prepare matrix pointers - remove constant offset here instead each time in the loop:
   sub r8, r14;
   sub r9, r14;
   mov r10, r9;
   add r10, r13;

   // for y := 0 to height1 - 1:
   mov r11, Height1;
   @@forylabel:
       mov rdi, r9;
       mov rsi, r10;
       mov r13, rcx;

       // for x := 0 to width2 - 1:
       mov rbx, r15;
       sub rbx, 2;
       jl @@fory2loopend;

       @@fory2label:
           {$IFDEF FPC}vxorpd ymm0, ymm0, ymm0;   {$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} // dest^ := 0
           {$IFDEF FPC}vxorpd ymm2, ymm2, ymm2;   {$ELSE}db $C5,$ED,$57,$D2;{$ENDIF} // dest + 1 := 0;

           // for idx := 0 to width1 div 4 do
           mov rax, r14;
           add rax, 32;
           jg @InnerLoopEnd;

           @@InnerLoop:
               {$IFDEF FPC}vmovapd ymm1, [r8 + rax - 32];{$ELSE}db $C4,$C1,$7D,$28,$4C,$00,$E0;{$ENDIF} 

               // multiply 2x2 and add
               {$IFDEF FPC}vfmadd231pd ymm0, ymm1, [rdi + rax - 32];{$ELSE}db $C4,$E2,$F5,$B8,$44,$07,$E0;{$ENDIF} 
               {$IFDEF FPC}vfmadd231pd ymm2, ymm1, [rsi + rax - 32];{$ELSE}db $C4,$E2,$F5,$B8,$54,$06,$E0;{$ENDIF} 

               // end for idx := 0 to width1 div 2
           add rax, 32;
           jle @@InnerLoop;

           {$IFDEF FPC}vextractf128 xmm1, ymm0, 1;{$ELSE}db $C4,$E3,$7D,$19,$C1,$01;{$ENDIF} 
           {$IFDEF FPC}vextractf128 xmm3, ymm2, 1;{$ELSE}db $C4,$E3,$7D,$19,$D3,$01;{$ENDIF} 

           {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm1;{$ELSE}db $C5,$F9,$7C,$C1;{$ENDIF} 
           {$IFDEF FPC}vhaddpd xmm2, xmm2, xmm3;{$ELSE}db $C5,$E9,$7C,$D3;{$ENDIF} 

           @InnerLoopEnd:

           sub rax, 32;
           jz @@InnerLoopEnd2;

           // do the final few elements
           @@InnerLoop2:
               {$IFDEF FPC}vmovsd xmm1, [r8 + rax];{$ELSE}db $C4,$C1,$7B,$10,$0C,$00;{$ENDIF} 

               // load 2x2 block
               {$IFDEF FPC}vmovsd xmm3, [rdi + rax];{$ELSE}db $C5,$FB,$10,$1C,$07;{$ENDIF} 
               {$IFDEF FPC}vmovsd xmm4, [rsi + rax];{$ELSE}db $C5,$FB,$10,$24,$06;{$ENDIF} 

               // multiply 2x2 and add
               {$IFDEF FPC}vmulsd xmm3, xmm3, xmm1;{$ELSE}db $C5,$E3,$59,$D9;{$ENDIF} 
               {$IFDEF FPC}vmulsd xmm4, xmm4, xmm1;{$ELSE}db $C5,$DB,$59,$E1;{$ENDIF} 

               {$IFDEF FPC}vaddsd xmm0, xmm0, xmm3;{$ELSE}db $C5,$FB,$58,$C3;{$ENDIF} 
               {$IFDEF FPC}vaddsd xmm2, xmm2, xmm4;{$ELSE}db $C5,$EB,$58,$D4;{$ENDIF} 

           add rax, 8;
           jnz @@InnerLoop2;

           @@InnerLoopEnd2:

           // final add and compact result
           {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm2;{$ELSE}db $C5,$F9,$7C,$C2;{$ENDIF} 

           // store back result
           {$IFDEF FPC}vmovapd [r13], xmm0;{$ELSE}db $C4,$C1,$79,$29,$45,$00;{$ENDIF} 

           // increment the pointers
           // inc(mt2), inc(dest);
           //add dword ptr [mt2], 8;
           add r13, 16;
           add rdi, r12;
           add rsi, r12;
       // end for x := 0 to width2 - 1
       sub rbx, 2;
       jge @@fory2label;

       @@fory2loopend:
       add rbx, 2;

       // test for odd h2
       jz @@NextLine;

       // we have an odd height2 -> special treatment for the last line
       {$IFDEF FPC}vxorpd ymm0, ymm0, ymm0;   {$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} // dest^ := 0

       // for idx := 0 to width1 div 4 do
       mov rax, r14;
       add rax, 32;
       jg @LastLineInnerLoopEnd;

       @@LastLineInnerLoop:
            {$IFDEF FPC}vmovapd ymm1, [r8 + rax - 32];{$ELSE}db $C4,$C1,$7D,$28,$4C,$00,$E0;{$ENDIF} 

            // multiply and add
            {$IFDEF FPC}vfmadd231pd ymm0, ymm1, [rdi + rax - 32];{$ELSE}db $C4,$E2,$F5,$B8,$44,$07,$E0;{$ENDIF} 
            // end for idx := 0 to width1 div 2
       add rax, 32;
       jle @@LastLineInnerLoop;

       {$IFDEF FPC}vextractf128 xmm1, ymm0, 1;{$ELSE}db $C4,$E3,$7D,$19,$C1,$01;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm1;{$ELSE}db $C5,$F9,$7C,$C1;{$ENDIF} 

       @LastLineInnerLoopEnd:
       sub rax, 32;
       jz @@LastLineInnerLoopEnd2;

       // do the final few elements
       @@LastLineInnerLoop2:
            {$IFDEF FPC}vmovsd xmm1, [r8 + rax];{$ELSE}db $C4,$C1,$7B,$10,$0C,$00;{$ENDIF} 
            {$IFDEF FPC}vmovsd xmm3, [rdi + rax];{$ELSE}db $C5,$FB,$10,$1C,$07;{$ENDIF} 

            // multiply and add
            {$IFDEF FPC}vmulsd xmm3, xmm3, xmm1;{$ELSE}db $C5,$E3,$59,$D9;{$ENDIF} 
            {$IFDEF FPC}vaddsd xmm0, xmm0, xmm3;{$ELSE}db $C5,$FB,$58,$C3;{$ENDIF} 
       // next element
       add rax, 8;
       jnz @@LastLineInnerLoop2;

       @@LastLineInnerLoopEnd2:

       // final add and compact result
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} 

       // store back result
       {$IFDEF FPC}vmovsd [r13], xmm0;{$ELSE}db $C4,$C1,$7B,$11,$45,$00;{$ENDIF} 

       // ################################################
       // #### next line of mt1
       @@NextLine:
       // dec(mt2, Width2);
       // inc(PByte(mt1), LineWidth1);
       // inc(PByte(dest), destOffset);
       //mov ebx, bytesWidth2;
       //sub dword ptr [mt2], ebx;
       add r8, LineWidth1;
       add rcx, rdx;

   // end for y := 0 to height1 - 1
   dec r11;
   jnz @@forylabel;

   // epilog - cleanup stack
   mov rbx, iRBX;
   mov rsi, iRSI;
   mov rdi, iRDI;
   mov r12, iR12;
   mov r13, iR13;
   mov r14, iR14;
   mov r15, iR15;

   {$IFDEF FPC}vmovupd xmm4, [rsp + $00];{$ELSE}db $C5,$F9,$10,$24,$24;{$ENDIF} 
   add rsp, $10;
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;

{$ENDIF}

end.
