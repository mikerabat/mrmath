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

unit AVXMatrixAbsOperationsx64;

// #####################################################
// #### Abs opertaion applied to every element in a matrix
// #####################################################

interface

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF x64}

uses MatrixConst;

procedure AVXMatrixAbsAligned(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}
procedure AVXMatrixAbsUnAligned(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}

{$ENDIF}

implementation

{$IFDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$S-} {$ENDIF}

procedure AVXMatrixAbsAligned(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}
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
   shl r8, 3;
   imul r8, -1;

   // helper registers for the dest pointer
   sub rcx, r8;

   lea rax, [rip + cSignBits4];
   {$IFDEF FPC}vmovupd ymm0, [rax];{$ELSE}db $C5,$FD,$10,$00;{$ENDIF} 

   // for y := 0 to height - 1:
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r8;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // prefetch data...
           //prefetchw [rcx + rax];

           // Abs:
           {$IFDEF FPC}vmovapd ymm1, [rcx + rax - 128];{$ELSE}db $C5,$FD,$28,$4C,$01,$80;{$ENDIF} 
           {$IFDEF FPC}vAndpd ymm1, ymm1, ymm0;{$ELSE}db $C5,$F5,$54,$C8;{$ENDIF} 
           {$IFDEF FPC}vmovntdq [rcx + rax - 128], ymm1;{$ELSE}db $C5,$FD,$E7,$4C,$01,$80;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm2, [rcx + rax - 96];{$ELSE}db $C5,$FD,$28,$54,$01,$A0;{$ENDIF} 
           {$IFDEF FPC}vandpd ymm2, ymm2, ymm0;{$ELSE}db $C5,$ED,$54,$D0;{$ENDIF} 
           {$IFDEF FPC}vmovntdq [rcx + rax - 96], ymm2;{$ELSE}db $C5,$FD,$E7,$54,$01,$A0;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm3, [rcx + rax - 64];{$ELSE}db $C5,$FD,$28,$5C,$01,$C0;{$ENDIF} 
           {$IFDEF FPC}vandpd ymm3, ymm3, ymm0;{$ELSE}db $C5,$E5,$54,$D8;{$ENDIF} 
           {$IFDEF FPC}vmovntdq [rcx + rax - 64], ymm3;{$ELSE}db $C5,$FD,$E7,$5C,$01,$C0;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm2, [rcx + rax - 32];{$ELSE}db $C5,$FD,$28,$54,$01,$E0;{$ENDIF} 
           {$IFDEF FPC}vandpd ymm2, ymm2, ymm0;{$ELSE}db $C5,$ED,$54,$D0;{$ENDIF} 
           {$IFDEF FPC}vmovntdq [rcx + rax - 32], ymm2;{$ELSE}db $C5,$FD,$E7,$54,$01,$E0;{$ENDIF} 
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           add rax, 16;
           jg @loopEnd2;

           {$IFDEF FPC}vmovapd xmm1, [rcx + rax - 16];{$ELSE}db $C5,$F9,$28,$4C,$01,$F0;{$ENDIF} 
           {$IFDEF FPC}vandpd xmm1, xmm1, xmm0;{$ELSE}db $C5,$F1,$54,$C8;{$ENDIF} 
           {$IFDEF FPC}vmovntdq [rcx + rax - 16], xmm1;{$ELSE}db $C5,$F9,$E7,$4C,$01,$F0;{$ENDIF} 
       jmp @addforxloop2;

       @loopEnd2:

       sub rax, 16;
       jz @nextLine;

       {$IFDEF FPC}vmovsd xmm1, [rcx + rax];{$ELSE}db $C5,$FB,$10,$0C,$01;{$ENDIF} 
       {$IFDEF FPC}vandpd xmm1, xmm1, xmm0;{$ELSE}db $C5,$F1,$54,$C8;{$ENDIF} 
       {$IFDEF FPC}vmovsd [rcx + rax], xmm1;{$ELSE}db $C5,$FB,$11,$0C,$01;{$ENDIF} 

       @nextLine:

       // next line:
       add rcx, rdx;

   // loop y end
   dec r9;
   jnz @@addforyloop;

   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;

procedure AVXMatrixAbsUnAligned(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt); {$IFDEF FPC}assembler;{$ENDIF}
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
   shl r8, 3;
   imul r8, -1;

   // helper registers for the dest pointer
   sub rcx, r8;

   lea rax, [rip + cSignBits4];
   {$IFDEF FPC}vmovupd ymm0, [rax];{$ELSE}db $C5,$FD,$10,$00;{$ENDIF} 

   // for y := 0 to height - 1:
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r8;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // prefetch data...
           //prefetchw [rcx + rax];

           // Abs:
           {$IFDEF FPC}vmovupd ymm1, [rcx + rax - 128];{$ELSE}db $C5,$FD,$10,$4C,$01,$80;{$ENDIF} 
           {$IFDEF FPC}vAndpd ymm1, ymm1, ymm0;{$ELSE}db $C5,$F5,$54,$C8;{$ENDIF} 
           {$IFDEF FPC}vmovupd [rcx + rax - 128], ymm1;{$ELSE}db $C5,$FD,$11,$4C,$01,$80;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm2, [rcx + rax - 96];{$ELSE}db $C5,$FD,$10,$54,$01,$A0;{$ENDIF} 
           {$IFDEF FPC}vandpd ymm2, ymm2, ymm0;{$ELSE}db $C5,$ED,$54,$D0;{$ENDIF} 
           {$IFDEF FPC}vmovupd [rcx + rax - 96], ymm2;{$ELSE}db $C5,$FD,$11,$54,$01,$A0;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm3, [rcx + rax - 64];{$ELSE}db $C5,$FD,$10,$5C,$01,$C0;{$ENDIF} 
           {$IFDEF FPC}vandpd ymm3, ymm3, ymm0;{$ELSE}db $C5,$E5,$54,$D8;{$ENDIF} 
           {$IFDEF FPC}vmovupd [rcx + rax - 64], ymm3;{$ELSE}db $C5,$FD,$11,$5C,$01,$C0;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm2, [rcx + rax - 32];{$ELSE}db $C5,$FD,$10,$54,$01,$E0;{$ENDIF} 
           {$IFDEF FPC}vandpd ymm2, ymm2, ymm0;{$ELSE}db $C5,$ED,$54,$D0;{$ENDIF} 
           {$IFDEF FPC}vmovupd [rcx + rax - 32], ymm2;{$ELSE}db $C5,$FD,$11,$54,$01,$E0;{$ENDIF} 
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           add rax, 16;
           jg @loopEnd2;

           {$IFDEF FPC}vmovupd xmm1, [rcx + rax - 16];{$ELSE}db $C5,$F9,$10,$4C,$01,$F0;{$ENDIF} 
           {$IFDEF FPC}vandpd xmm1, xmm1, xmm0;{$ELSE}db $C5,$F1,$54,$C8;{$ENDIF} 
           {$IFDEF FPC}vmovupd [rcx + rax - 16], xmm1;{$ELSE}db $C5,$F9,$11,$4C,$01,$F0;{$ENDIF} 
       jmp @addforxloop2;

       @loopEnd2:

       sub rax, 16;
       jz @nextLine;

       {$IFDEF FPC}vmovsd xmm1, [rcx + rax];{$ELSE}db $C5,$FB,$10,$0C,$01;{$ENDIF} 
       {$IFDEF FPC}vandpd xmm1, xmm1, xmm0;{$ELSE}db $C5,$F1,$54,$C8;{$ENDIF} 
       {$IFDEF FPC}vmovsd [rcx + rax], xmm1;{$ELSE}db $C5,$FB,$11,$0C,$01;{$ENDIF} 

       @nextLine:

       // next line:
       add rcx, rdx;

   // loop y end
   dec r9;
   jnz @@addforyloop;

   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;

{$ENDIF}

end.
