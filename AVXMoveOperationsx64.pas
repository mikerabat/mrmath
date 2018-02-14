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


unit AVXMoveOperationsx64;

// #################################################
// #### SSE optimized move oprationes
// #################################################

interface

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF x64}

uses MatrixConst;

procedure AVXMatrixCopyAligned(Dest : PDouble; const destLineWidth : TASMNativeInt; src : PDouble; const srcLineWidth : TASMNativeInt; Width, Height : TASMNativeInt);
procedure AVXMatrixCopyUnAligned(Dest : PDouble; const destLineWidth : TASMNativeInt; src : PDouble; const srcLineWidth : TASMNativeInt; Width, Height : TASMNativeInt);

procedure AVXRowSwapAligned(A, B : PDouble; width : TASMNativeInt);
procedure AVXRowSwapUnAligned(A, B : PDouble; width : TASMNativeInt);

procedure AVXInitMemAligned(A : PDouble; NumBytes : TASMNativeInt; const Value : double);

{$ENDIF}

implementation

{$IFDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$ENDIF}

// uses non temporal moves so the cache is not poisned
// rcx = A, rdx = NumBytes;
procedure AVXInitMemAligned(A : PDouble; NumBytes : TASMNativeInt; const Value : double);
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

   lea rax, Value;
   {$IFDEF FPC}vbroadcastsd ymm1, [rax];{$ELSE}db $C4,$E2,$7D,$19,$08;{$ENDIF} 

   imul rdx, -1;
   sub rcx, rdx;

   @@loopUnrolled:
      add rdx, 128;
      jg @@loopUnrolledEnd;

      {$IFDEF FPC}vmovntdq [rcx + rdx - 128], ymm1;{$ELSE}db $C5,$FD,$E7,$4C,$11,$80;{$ENDIF} 
      {$IFDEF FPC}vmovntdq [rcx + rdx - 96], ymm1;{$ELSE}db $C5,$FD,$E7,$4C,$11,$A0;{$ENDIF} 
      {$IFDEF FPC}vmovntdq [rcx + rdx - 64], ymm1;{$ELSE}db $C5,$FD,$E7,$4C,$11,$C0;{$ENDIF} 
      {$IFDEF FPC}vmovntdq [rcx + rdx - 32], ymm1;{$ELSE}db $C5,$FD,$E7,$4C,$11,$E0;{$ENDIF} 
   jmp @@loopUnrolled;

   @@loopUnrolledEnd:

   sub rdx, 256;
   jz @@exitProc;

   add rdx, 32;
   jg @@beginloop3;

   @@loop2:
      {$IFDEF FPC}vmovntdq [rcx + rdx - 32], xmm1;{$ELSE}db $C5,$F9,$E7,$4C,$11,$E0;{$ENDIF} 
      {$IFDEF FPC}vmovntdq [rcx + rdx - 16], xmm1;{$ELSE}db $C5,$F9,$E7,$4C,$11,$F0;{$ENDIF} 
   add rdx, 32;
   jl @@loop2;

   @@beginloop3:
   sub rdx, 32;
   jz @@exitProc;

   @@loop3:
      {$IFDEF FPC}vmovsd [rcx + rdx], xmm1;{$ELSE}db $C5,$FB,$11,$0C,$11;{$ENDIF} 
      add rdx, 8;
   jnz @@loop3;

   @@exitProc:

   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;
{$IFDEF FPC}
end;
{$ENDIF}

procedure AVXRowSwapAligned(A, B : PDouble; width : TASMNativeInt);
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

   // note: RCX = a, RDX = b, R8 = width
   imul r8, -8;

   sub rcx, r8;
   sub rdx, r8;

   @unrolloop:
     add r8, 128;
     jg @unrolloopend;

     // prefetch data...
     // prefetchw [rcx + r10];
     // prefetchw [rdx + r10];

     {$IFDEF FPC}vmovdqa ymm0, [rcx + r8 - 128];{$ELSE}db $C4,$A1,$7D,$6F,$44,$01,$80;{$ENDIF} 
     {$IFDEF FPC}vmovdqa ymm1, [rdx + r8 - 128];{$ELSE}db $C4,$A1,$7D,$6F,$4C,$02,$80;{$ENDIF} 

     {$IFDEF FPC}vmovdqa [rcx + r8 - 128], ymm1;{$ELSE}db $C4,$A1,$7D,$7F,$4C,$01,$80;{$ENDIF} 
     {$IFDEF FPC}vmovdqa [rdx + r8 - 128], ymm0;{$ELSE}db $C4,$A1,$7D,$7F,$44,$02,$80;{$ENDIF} 

     {$IFDEF FPC}vmovdqa ymm2, [rcx + r8 - 96];{$ELSE}db $C4,$A1,$7D,$6F,$54,$01,$A0;{$ENDIF} 
     {$IFDEF FPC}vmovdqa ymm3, [rdx + r8 - 96];{$ELSE}db $C4,$A1,$7D,$6F,$5C,$02,$A0;{$ENDIF} 

     {$IFDEF FPC}vmovdqa [rcx + r8 - 96], ymm3;{$ELSE}db $C4,$A1,$7D,$7F,$5C,$01,$A0;{$ENDIF} 
     {$IFDEF FPC}vmovdqa [rdx + r8 - 96], ymm2;{$ELSE}db $C4,$A1,$7D,$7F,$54,$02,$A0;{$ENDIF} 

     {$IFDEF FPC}vmovdqa ymm0, [rcx + r8 - 64];{$ELSE}db $C4,$A1,$7D,$6F,$44,$01,$C0;{$ENDIF} 
     {$IFDEF FPC}vmovdqa ymm1, [rdx + r8 - 64];{$ELSE}db $C4,$A1,$7D,$6F,$4C,$02,$C0;{$ENDIF} 

     {$IFDEF FPC}vmovdqa [rcx + r8 - 64], ymm1;{$ELSE}db $C4,$A1,$7D,$7F,$4C,$01,$C0;{$ENDIF} 
     {$IFDEF FPC}vmovdqa [rdx + r8 - 64], ymm0;{$ELSE}db $C4,$A1,$7D,$7F,$44,$02,$C0;{$ENDIF} 

     {$IFDEF FPC}vmovdqa ymm2, [rcx + r8 - 32];{$ELSE}db $C4,$A1,$7D,$6F,$54,$01,$E0;{$ENDIF} 
     {$IFDEF FPC}vmovdqa ymm3, [rdx + r8 - 32];{$ELSE}db $C4,$A1,$7D,$6F,$5C,$02,$E0;{$ENDIF} 

     {$IFDEF FPC}vmovdqa [rcx + r8 - 32], ymm3;{$ELSE}db $C4,$A1,$7D,$7F,$5C,$01,$E0;{$ENDIF} 
     {$IFDEF FPC}vmovdqa [rdx + r8 - 32], ymm2;{$ELSE}db $C4,$A1,$7D,$7F,$54,$02,$E0;{$ENDIF} 
   jmp @unrolloop;
   @unrolloopend:

   sub r8, 128;
   jz @endfunc;

   @loop2:
     add r8, 16;
     jg @loop2End;

     {$IFDEF FPC}vmovdqa xmm0, [rcx + r8 - 16];{$ELSE}db $C4,$A1,$79,$6F,$44,$01,$F0;{$ENDIF} 
     {$IFDEF FPC}vmovdqa xmm1, [rdx + r8 - 16];{$ELSE}db $C4,$A1,$79,$6F,$4C,$02,$F0;{$ENDIF} 

     {$IFDEF FPC}vmovdqa [rcx + r8 - 16], xmm1;{$ELSE}db $C4,$A1,$79,$7F,$4C,$01,$F0;{$ENDIF} 
     {$IFDEF FPC}vmovdqa [rdx + r8 - 16], xmm0;{$ELSE}db $C4,$A1,$79,$7F,$44,$02,$F0;{$ENDIF} 
   jmp @loop2;

   @loop2End:
   sub r8, 16;

   jz @endfunc;

   {$IFDEF FPC}vmovsd xmm0, [rcx + r8];{$ELSE}db $C4,$A1,$7B,$10,$04,$01;{$ENDIF} 
   {$IFDEF FPC}vmovsd xmm1, [rdx + r8];{$ELSE}db $C4,$A1,$7B,$10,$0C,$02;{$ENDIF} 

   {$IFDEF FPC}vmovsd [rcx + r8], xmm1;{$ELSE}db $C4,$A1,$7B,$11,$0C,$01;{$ENDIF} 
   {$IFDEF FPC}vmovsd [rdx + r8], xmm0;{$ELSE}db $C4,$A1,$7B,$11,$04,$02;{$ENDIF} 

   @endfunc:

   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 
{$IFDEF FPC}
end;
{$ENDIF}
end;

procedure AVXRowSwapUnAligned(A, B : PDouble; width : TASMNativeInt);
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

   // note: RCX = a, RDX = b, R8 = width
   shl r8, 3;
   imul r8, -1;

   sub rcx, r8;
   sub rdx, r8;

   @unrolloop:
     add r8, 128;
     jg @unrolloopend;

     // prefetch data...
     // prefetchw [rcx + r10];
     // prefetchw [rdx + r10];

     {$IFDEF FPC}vmovdqu ymm0, [rcx + r8 - 128];{$ELSE}db $C4,$A1,$7E,$6F,$44,$01,$80;{$ENDIF} 
     {$IFDEF FPC}vmovdqu ymm1, [rdx + r8 - 128];{$ELSE}db $C4,$A1,$7E,$6F,$4C,$02,$80;{$ENDIF} 

     {$IFDEF FPC}vmovdqu [rcx + r8 - 128], ymm1;{$ELSE}db $C4,$A1,$7E,$7F,$4C,$01,$80;{$ENDIF} 
     {$IFDEF FPC}vmovdqu [rdx + r8 - 128], ymm0;{$ELSE}db $C4,$A1,$7E,$7F,$44,$02,$80;{$ENDIF} 

     {$IFDEF FPC}vmovdqu ymm2, [rcx + r8 - 96];{$ELSE}db $C4,$A1,$7E,$6F,$54,$01,$A0;{$ENDIF} 
     {$IFDEF FPC}vmovdqu ymm3, [rdx + r8 - 96];{$ELSE}db $C4,$A1,$7E,$6F,$5C,$02,$A0;{$ENDIF} 

     {$IFDEF FPC}vmovdqu [rcx + r8 - 96], ymm3;{$ELSE}db $C4,$A1,$7E,$7F,$5C,$01,$A0;{$ENDIF} 
     {$IFDEF FPC}vmovdqu [rdx + r8 - 96], ymm2;{$ELSE}db $C4,$A1,$7E,$7F,$54,$02,$A0;{$ENDIF} 

     {$IFDEF FPC}vmovdqu ymm0, [rcx + r8 - 64];{$ELSE}db $C4,$A1,$7E,$6F,$44,$01,$C0;{$ENDIF} 
     {$IFDEF FPC}vmovdqu ymm1, [rdx + r8 - 64];{$ELSE}db $C4,$A1,$7E,$6F,$4C,$02,$C0;{$ENDIF} 

     {$IFDEF FPC}vmovdqu [rcx + r8 - 64], ymm1;{$ELSE}db $C4,$A1,$7E,$7F,$4C,$01,$C0;{$ENDIF} 
     {$IFDEF FPC}vmovdqu [rdx + r8 - 64], ymm0;{$ELSE}db $C4,$A1,$7E,$7F,$44,$02,$C0;{$ENDIF} 

     {$IFDEF FPC}vmovdqu ymm2, [rcx + r8 - 32];{$ELSE}db $C4,$A1,$7E,$6F,$54,$01,$E0;{$ENDIF} 
     {$IFDEF FPC}vmovdqu ymm3, [rdx + r8 - 32];{$ELSE}db $C4,$A1,$7E,$6F,$5C,$02,$E0;{$ENDIF} 

     {$IFDEF FPC}vmovdqu [rcx + r8 - 32], ymm3;{$ELSE}db $C4,$A1,$7E,$7F,$5C,$01,$E0;{$ENDIF} 
     {$IFDEF FPC}vmovdqu [rdx + r8 - 32], ymm2;{$ELSE}db $C4,$A1,$7E,$7F,$54,$02,$E0;{$ENDIF} 
   jmp @unrolloop;
   @unrolloopend:

   sub r8, 128;
   jz @endfunc;

   @loop2:
     add r8, 16;
     jg @loop2End;

     {$IFDEF FPC}vmovdqu xmm0, [rcx + r8 - 16];{$ELSE}db $C4,$A1,$7A,$6F,$44,$01,$F0;{$ENDIF} 
     {$IFDEF FPC}vmovdqu xmm1, [rdx + r8 - 16];{$ELSE}db $C4,$A1,$7A,$6F,$4C,$02,$F0;{$ENDIF} 

     {$IFDEF FPC}vmovdqu [rcx + r8 - 16], xmm1;{$ELSE}db $C4,$A1,$7A,$7F,$4C,$01,$F0;{$ENDIF} 
     {$IFDEF FPC}vmovdqu [rdx + r8 - 16], xmm0;{$ELSE}db $C4,$A1,$7A,$7F,$44,$02,$F0;{$ENDIF} 
   jmp @loop2;

   @loop2End:
   sub r8, 16;

   jz @endfunc;

   {$IFDEF FPC}vmovsd xmm0, [rcx + r8];{$ELSE}db $C4,$A1,$7B,$10,$04,$01;{$ENDIF} 
   {$IFDEF FPC}vmovsd xmm1, [rdx + r8];{$ELSE}db $C4,$A1,$7B,$10,$0C,$02;{$ENDIF} 

   {$IFDEF FPC}vmovsd [rcx + r8], xmm1;{$ELSE}db $C4,$A1,$7B,$11,$0C,$01;{$ENDIF} 
   {$IFDEF FPC}vmovsd [rdx + r8], xmm0;{$ELSE}db $C4,$A1,$7B,$11,$04,$02;{$ENDIF} 

   @endfunc:

   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 
{$IFDEF FPC}
end;
{$ENDIF}
end;

procedure AVXMatrixCopyAligned(Dest : PDouble; const destLineWidth : TASMNativeInt; src : PDouble; const srcLineWidth : TASMNativeInt; Width, Height : TASMNativeInt);
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

   // note: RCX = dest, RDX = destLineWidth, R8 = src, R9 = srcLineWidth
   //iters := -width*sizeof(double);
   mov r10, width;
   shl r10, 3;
   imul r10, -1;

   // helper registers for the mt1, mt2 and dest pointers
   sub r8, r10;
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
           // prefetch [r8 + rax];
           // prefetchw [rcx + rax];

           // move:
           {$IFDEF FPC}vmovdqa ymm0, [r8 + rax - 128];{$ELSE}db $C4,$C1,$7D,$6F,$44,$00,$80;{$ENDIF} 
           {$IFDEF FPC}vmovdqa [rcx + rax - 128], ymm0;{$ELSE}db $C5,$FD,$7F,$44,$01,$80;{$ENDIF} 

           {$IFDEF FPC}vmovdqa ymm1, [r8 + rax - 96];{$ELSE}db $C4,$C1,$7D,$6F,$4C,$00,$A0;{$ENDIF} 
           {$IFDEF FPC}vmovdqa [rcx + rax - 96], ymm1;{$ELSE}db $C5,$FD,$7F,$4C,$01,$A0;{$ENDIF} 

           {$IFDEF FPC}vmovdqa ymm2, [r8 + rax - 64];{$ELSE}db $C4,$C1,$7D,$6F,$54,$00,$C0;{$ENDIF} 
           {$IFDEF FPC}vmovdqa [rcx + rax - 64], ymm2;{$ELSE}db $C5,$FD,$7F,$54,$01,$C0;{$ENDIF} 

           {$IFDEF FPC}vmovdqa ymm3, [r8 + rax - 32];{$ELSE}db $C4,$C1,$7D,$6F,$5C,$00,$E0;{$ENDIF} 
           {$IFDEF FPC}vmovdqa [rcx + rax - 32], ymm3;{$ELSE}db $C5,$FD,$7F,$5C,$01,$E0;{$ENDIF} 

       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
          add rax, 16;
          jg @loopEnd2;

          {$IFDEF FPC}vmovdqa xmm0, [r8 + rax - 16];{$ELSE}db $C4,$C1,$79,$6F,$44,$00,$F0;{$ENDIF} 
          {$IFDEF FPC}vmovdqa [rcx + rax - 16], xmm0;{$ELSE}db $C5,$F9,$7F,$44,$01,$F0;{$ENDIF} 
       jmp @addforxloop2;

       @loopEnd2:
       sub rax, 16;

       jz @nextLine;

       // last element
       {$IFDEF FPC}vmovsd xmm0, [r8 + rax];{$ELSE}db $C4,$C1,$7B,$10,$04,$00;{$ENDIF} 
       {$IFDEF FPC}vmovsd [rcx + rax], xmm0;{$ELSE}db $C5,$FB,$11,$04,$01;{$ENDIF} 

       @nextLine:

       // next line:
       add r8, r9;
       add rcx, rdx;

   // loop y end
   dec r11;
   jnz @@addforyloop;

   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;
{$IFDEF FPC}
end;
{$ENDIF}

procedure AVXMatrixCopyUnAligned(Dest : PDouble; const destLineWidth : TASMNativeInt; src : PDouble; const srcLineWidth : TASMNativeInt; Width, Height : TASMNativeInt);
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

   // note: RCX = dest, RDX = destLineWidth, R8 = src, R9 = srcLineWidth
   //iters := -width*sizeof(double);
   mov r10, width;
   shl r10, 3;
   imul r10, -1;

   // helper registers for the mt1, mt2 and dest pointers
   sub r8, r10;
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
           // prefetch [r8 + rax];
           // prefetchw [rcx + rax];

           // move:
           {$IFDEF FPC}vmovdqu ymm0, [r8 + rax - 128];{$ELSE}db $C4,$C1,$7E,$6F,$44,$00,$80;{$ENDIF} 
           {$IFDEF FPC}vmovdqu [rcx + rax - 128], ymm0;{$ELSE}db $C5,$FE,$7F,$44,$01,$80;{$ENDIF} 

           {$IFDEF FPC}vmovdqu ymm1, [r8 + rax - 96];{$ELSE}db $C4,$C1,$7E,$6F,$4C,$00,$A0;{$ENDIF} 
           {$IFDEF FPC}vmovdqu [rcx + rax - 96], ymm1;{$ELSE}db $C5,$FE,$7F,$4C,$01,$A0;{$ENDIF} 

           {$IFDEF FPC}vmovdqu ymm2, [r8 + rax - 64];{$ELSE}db $C4,$C1,$7E,$6F,$54,$00,$C0;{$ENDIF} 
           {$IFDEF FPC}vmovdqu [rcx + rax - 64], ymm2;{$ELSE}db $C5,$FE,$7F,$54,$01,$C0;{$ENDIF} 

           {$IFDEF FPC}vmovdqu ymm3, [r8 + rax - 32];{$ELSE}db $C4,$C1,$7E,$6F,$5C,$00,$E0;{$ENDIF} 
           {$IFDEF FPC}vmovdqu [rcx + rax - 32], ymm3;{$ELSE}db $C5,$FE,$7F,$5C,$01,$E0;{$ENDIF} 

       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
          add rax, 16;
          jg @loopEnd2;

          {$IFDEF FPC}vmovdqu xmm0, [r8 + rax - 16];{$ELSE}db $C4,$C1,$7A,$6F,$44,$00,$F0;{$ENDIF} 
          {$IFDEF FPC}vmovdqu [rcx + rax - 16], xmm0;{$ELSE}db $C5,$FA,$7F,$44,$01,$F0;{$ENDIF} 
       jmp @addforxloop2;

       @loopEnd2:
       sub rax, 16;

       jz @nextLine;

       // last element
       {$IFDEF FPC}vmovsd xmm0, [r8 + rax];{$ELSE}db $C4,$C1,$7B,$10,$04,$00;{$ENDIF} 
       {$IFDEF FPC}vmovsd [rcx + rax], xmm0;{$ELSE}db $C5,$FB,$11,$04,$01;{$ENDIF} 

       @nextLine:

       // next line:
       add r8, r9;
       add rcx, rdx;

   // loop y end
   dec r11;
   jnz @@addforyloop;

   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;
{$IFDEF FPC}
end;
{$ENDIF}

{$ENDIF}

end.
