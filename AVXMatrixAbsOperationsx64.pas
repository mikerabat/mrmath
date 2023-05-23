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

{$I 'mrMath_CPU.inc'}

{$IFDEF x64}

uses MatrixConst;

procedure AVXMatrixAbsAligned(Dest : PDouble; const LineWidth, Width, Height : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}
procedure AVXMatrixAbsUnAligned(Dest : PDouble; const LineWidth, Width, Height : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}

{$ENDIF}

implementation

{$IFDEF x64}

procedure AVXMatrixAbsAligned(Dest : PDouble; const LineWidth, Width, Height : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}
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
   {$IFDEF AVXSUP}vmovupd ymm0, [rax];                                {$ELSE}db $C5,$FD,$10,$00;{$ENDIF} 

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
           {$IFDEF AVXSUP}vmovapd ymm1, [rcx + rax - 128];            {$ELSE}db $C5,$FD,$28,$4C,$01,$80;{$ENDIF} 
           {$IFDEF AVXSUP}vAndpd ymm1, ymm1, ymm0;                    {$ELSE}db $C5,$F5,$54,$C8;{$ENDIF} 
           {$IFDEF AVXSUP}vmovapd [rcx + rax - 128], ymm1;            {$ELSE}db $C5,$FD,$29,$4C,$01,$80;{$ENDIF} 

           {$IFDEF AVXSUP}vmovapd ymm2, [rcx + rax - 96];             {$ELSE}db $C5,$FD,$28,$54,$01,$A0;{$ENDIF} 
           {$IFDEF AVXSUP}vandpd ymm2, ymm2, ymm0;                    {$ELSE}db $C5,$ED,$54,$D0;{$ENDIF} 
           {$IFDEF AVXSUP}vmovapd [rcx + rax - 96], ymm2;             {$ELSE}db $C5,$FD,$29,$54,$01,$A0;{$ENDIF} 

           {$IFDEF AVXSUP}vmovapd ymm3, [rcx + rax - 64];             {$ELSE}db $C5,$FD,$28,$5C,$01,$C0;{$ENDIF} 
           {$IFDEF AVXSUP}vandpd ymm3, ymm3, ymm0;                    {$ELSE}db $C5,$E5,$54,$D8;{$ENDIF} 
           {$IFDEF AVXSUP}vmovapd [rcx + rax - 64], ymm3;             {$ELSE}db $C5,$FD,$29,$5C,$01,$C0;{$ENDIF} 

           {$IFDEF AVXSUP}vmovapd ymm2, [rcx + rax - 32];             {$ELSE}db $C5,$FD,$28,$54,$01,$E0;{$ENDIF} 
           {$IFDEF AVXSUP}vandpd ymm2, ymm2, ymm0;                    {$ELSE}db $C5,$ED,$54,$D0;{$ENDIF} 
           {$IFDEF AVXSUP}vmovapd [rcx + rax - 32], ymm2;             {$ELSE}db $C5,$FD,$29,$54,$01,$E0;{$ENDIF} 
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           add rax, 16;
           jg @loopEnd2;

           {$IFDEF AVXSUP}vmovapd xmm1, [rcx + rax - 16];             {$ELSE}db $C5,$F9,$28,$4C,$01,$F0;{$ENDIF} 
           {$IFDEF AVXSUP}vandpd xmm1, xmm1, xmm0;                    {$ELSE}db $C5,$F1,$54,$C8;{$ENDIF} 
           {$IFDEF AVXSUP}vmovapd [rcx + rax - 16], xmm1;             {$ELSE}db $C5,$F9,$29,$4C,$01,$F0;{$ENDIF} 
       jmp @addforxloop2;

       @loopEnd2:

       sub rax, 16;
       jz @nextLine;

       {$IFDEF AVXSUP}vmovsd xmm1, [rcx + rax];                       {$ELSE}db $C5,$FB,$10,$0C,$01;{$ENDIF} 
       {$IFDEF AVXSUP}vandpd xmm1, xmm1, xmm0;                        {$ELSE}db $C5,$F1,$54,$C8;{$ENDIF} 
       {$IFDEF AVXSUP}vmovsd [rcx + rax], xmm1;                       {$ELSE}db $C5,$FB,$11,$0C,$01;{$ENDIF} 

       @nextLine:

       // next line:
       add rcx, rdx;

   // loop y end
   dec r9;
   jnz @@addforyloop;

   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;

procedure AVXMatrixAbsUnAligned(Dest : PDouble; const LineWidth, Width, Height : NativeInt); {$IFDEF FPC}assembler;{$ENDIF}
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
   {$IFDEF AVXSUP}vmovupd ymm0, [rax];                                {$ELSE}db $C5,$FD,$10,$00;{$ENDIF} 

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
           {$IFDEF AVXSUP}vmovupd ymm1, [rcx + rax - 128];            {$ELSE}db $C5,$FD,$10,$4C,$01,$80;{$ENDIF} 
           {$IFDEF AVXSUP}vAndpd ymm1, ymm1, ymm0;                    {$ELSE}db $C5,$F5,$54,$C8;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd [rcx + rax - 128], ymm1;            {$ELSE}db $C5,$FD,$11,$4C,$01,$80;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd ymm2, [rcx + rax - 96];             {$ELSE}db $C5,$FD,$10,$54,$01,$A0;{$ENDIF} 
           {$IFDEF AVXSUP}vandpd ymm2, ymm2, ymm0;                    {$ELSE}db $C5,$ED,$54,$D0;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd [rcx + rax - 96], ymm2;             {$ELSE}db $C5,$FD,$11,$54,$01,$A0;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd ymm3, [rcx + rax - 64];             {$ELSE}db $C5,$FD,$10,$5C,$01,$C0;{$ENDIF} 
           {$IFDEF AVXSUP}vandpd ymm3, ymm3, ymm0;                    {$ELSE}db $C5,$E5,$54,$D8;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd [rcx + rax - 64], ymm3;             {$ELSE}db $C5,$FD,$11,$5C,$01,$C0;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd ymm2, [rcx + rax - 32];             {$ELSE}db $C5,$FD,$10,$54,$01,$E0;{$ENDIF} 
           {$IFDEF AVXSUP}vandpd ymm2, ymm2, ymm0;                    {$ELSE}db $C5,$ED,$54,$D0;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd [rcx + rax - 32], ymm2;             {$ELSE}db $C5,$FD,$11,$54,$01,$E0;{$ENDIF} 
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           add rax, 16;
           jg @loopEnd2;

           {$IFDEF AVXSUP}vmovupd xmm1, [rcx + rax - 16];             {$ELSE}db $C5,$F9,$10,$4C,$01,$F0;{$ENDIF} 
           {$IFDEF AVXSUP}vandpd xmm1, xmm1, xmm0;                    {$ELSE}db $C5,$F1,$54,$C8;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd [rcx + rax - 16], xmm1;             {$ELSE}db $C5,$F9,$11,$4C,$01,$F0;{$ENDIF} 
       jmp @addforxloop2;

       @loopEnd2:

       sub rax, 16;
       jz @nextLine;

       {$IFDEF AVXSUP}vmovsd xmm1, [rcx + rax];                       {$ELSE}db $C5,$FB,$10,$0C,$01;{$ENDIF} 
       {$IFDEF AVXSUP}vandpd xmm1, xmm1, xmm0;                        {$ELSE}db $C5,$F1,$54,$C8;{$ENDIF} 
       {$IFDEF AVXSUP}vmovsd [rcx + rax], xmm1;                       {$ELSE}db $C5,$FB,$11,$0C,$01;{$ENDIF} 

       @nextLine:

       // next line:
       add rcx, rdx;

   // loop y end
   dec r9;
   jnz @@addforyloop;

   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;

{$ENDIF}

end.
