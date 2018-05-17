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


unit AVXMatrixMinMaxOperationsx64;

interface

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF x64}

uses MatrixConst;

function AVXMatrixMaxAligned(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double;
function AVXMatrixMaxUnAligned(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double;

function AVXMatrixMinAligned(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double;
function AVXMatrixMinUnAligned(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double;

{$ENDIF}

implementation

{$IFDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$S-} {$warnings off} {$ENDIF}

function AVXMatrixMaxAligned(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double;
{$IFDEF FPC}
begin
{$ENDIF}
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
   // note: RCX = mt, RDX = width, R8 = height, R9 = LineWidth
   // prolog - simulate stack
   sub rsp, $10;
   {$IFDEF FPC}vmovupd [rsp + $00], xmm4;{$ELSE}db $C5,$F9,$11,$24,$24;{$ENDIF} 

   imul rdx, -8;

   sub rcx, rdx;

   // init result
   lea rax, [rip + cNegMaxDouble];
   {$IFDEF FPC}vbroadcastsd ymm0, [rax];{$ELSE}db $C4,$E2,$7D,$19,$00;{$ENDIF} 
   {$IFDEF FPC}vmovapd ymm3, ymm0;{$ELSE}db $C5,$FD,$28,$D8;{$ENDIF} 
   {$IFDEF FPC}vmovapd ymm4, ymm0;{$ELSE}db $C5,$FD,$28,$E0;{$ENDIF} 

   // for y := 0 to height - 1:
   mov r10, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, rdx;
       add rax, 128;
       jg @loopEnd;

       @addforxloop:
           // prefetch data...
           // prefetch [rcx + rax];

           // max:
           {$IFDEF FPC}vmaxpd ymm3, ymm3, [rcx + rax - 128];{$ELSE}db $C5,$E5,$5F,$5C,$01,$80;{$ENDIF} 
           {$IFDEF FPC}vmaxpd ymm4, ymm4, [rcx + rax - 96];{$ELSE}db $C5,$DD,$5F,$64,$01,$A0;{$ENDIF} 
           {$IFDEF FPC}vmaxpd ymm3, ymm3, [rcx + rax - 64];{$ELSE}db $C5,$E5,$5F,$5C,$01,$C0;{$ENDIF} 
           {$IFDEF FPC}vmaxpd ymm4, ymm4, [rcx + rax - 32];{$ELSE}db $C5,$DD,$5F,$64,$01,$E0;{$ENDIF} 

       add rax, 128;
       jle @addforxloop;

       {$IFDEF FPC}vextractf128 xmm2, ymm3, 1;{$ELSE}db $C4,$E3,$7D,$19,$DA,$01;{$ENDIF} 
       {$IFDEF FPC}vmaxpd xmm2, xmm2, xmm3;{$ELSE}db $C5,$E9,$5F,$D3;{$ENDIF} 
       {$IFDEF FPC}vmaxpd xmm0, xmm0, xmm2;{$ELSE}db $C5,$F9,$5F,$C2;{$ENDIF} 
       {$IFDEF FPC}vextractf128 xmm2, ymm4, 1;{$ELSE}db $C4,$E3,$7D,$19,$E2,$01;{$ENDIF} 
       {$IFDEF FPC}vmaxpd xmm2, xmm2, xmm4;{$ELSE}db $C5,$E9,$5F,$D4;{$ENDIF} 
       {$IFDEF FPC}vmaxpd xmm0, xmm0, xmm2;{$ELSE}db $C5,$F9,$5F,$C2;{$ENDIF} 

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           add rax, 16;
           jg @addforxloop2end;

           {$IFDEF FPC}vmaxpd xmm0, xmm0, [rcx + rax - 16];{$ELSE}db $C5,$F9,$5F,$44,$01,$F0;{$ENDIF} 
       jmp @addforxloop2;

       @addforxloop2end:

       sub rax, 16;
       jz @nextLine;

       {$IFDEF FPC}vmovsd xmm2, [rcx + rax];{$ELSE}db $C5,$FB,$10,$14,$01;{$ENDIF} 
       {$IFDEF FPC}vmaxsd xmm0, xmm0, xmm2;{$ELSE}db $C5,$FB,$5F,$C2;{$ENDIF} 

       @nextLine:

       // next line:
       add rcx, r9;

   // loop y end
   dec r10;
   jnz @@addforyloop;

   // final max ->
   {$IFDEF FPC}vmovhlps xmm1, xmm1, xmm0;{$ELSE}db $C5,$F0,$12,$C8;{$ENDIF} 
   {$IFDEF FPC}vmaxsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$5F,$C1;{$ENDIF} 

   // epilog - cleanup stack
   {$IFDEF FPC}vmovupd xmm4, [rsp + $00];{$ELSE}db $C5,$F9,$10,$24,$24;{$ENDIF} 
   add rsp, $10;
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 
{$IFDEF FPC}
end;
{$ENDIF}
end;

function AVXMatrixMaxUnAligned(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double;
{$IFDEF FPC}
begin
{$ENDIF}
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

   // note: RCX = mt, RDX = width, R8 = height, R9 = LineWidth
   // prolog - simulate stack
   sub rsp, $10;
   {$IFDEF FPC}vmovupd [rsp + $00], xmm4;{$ELSE}db $C5,$F9,$11,$24,$24;{$ENDIF} 

   imul rdx, -8;

   sub rcx, rdx;

   // init result
   lea rax, [rip + cNegMaxDouble];
   {$IFDEF FPC}vbroadcastsd ymm0, [rax];{$ELSE}db $C4,$E2,$7D,$19,$00;{$ENDIF} 
   {$IFDEF FPC}vmovapd ymm3, ymm0;{$ELSE}db $C5,$FD,$28,$D8;{$ENDIF} 
   {$IFDEF FPC}vmovapd ymm4, ymm0;{$ELSE}db $C5,$FD,$28,$E0;{$ENDIF} 

   // for y := 0 to height - 1:
   mov r10, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, rdx;

       add rax, 128;
       jg @loopEnd;

       @addforxloop:
           // prefetch data...
           // prefetch [rcx + rax];

           // max:
           {$IFDEF FPC}vmovupd ymm2, [rcx + rax - 128];{$ELSE}db $C5,$FD,$10,$54,$01,$80;{$ENDIF} 
           {$IFDEF FPC}vmaxpd ymm3, ymm3, ymm2;{$ELSE}db $C5,$E5,$5F,$DA;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm2, [rcx + rax - 96];{$ELSE}db $C5,$FD,$10,$54,$01,$A0;{$ENDIF} 
           {$IFDEF FPC}vmaxpd ymm4, ymm4, ymm2;{$ELSE}db $C5,$DD,$5F,$E2;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm2, [rcx + rax - 64];{$ELSE}db $C5,$FD,$10,$54,$01,$C0;{$ENDIF} 
           {$IFDEF FPC}vmaxpd ymm3, ymm3, ymm2;{$ELSE}db $C5,$E5,$5F,$DA;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm2, [rcx + rax - 32];{$ELSE}db $C5,$FD,$10,$54,$01,$E0;{$ENDIF} 
           {$IFDEF FPC}vmaxpd ymm4, ymm4, ymm2;{$ELSE}db $C5,$DD,$5F,$E2;{$ENDIF} 
       add rax, 128;
       jle @addforxloop;

       {$IFDEF FPC}vextractf128 xmm2, ymm3, 1;{$ELSE}db $C4,$E3,$7D,$19,$DA,$01;{$ENDIF} 
       {$IFDEF FPC}vmaxpd xmm2, xmm2, xmm3;{$ELSE}db $C5,$E9,$5F,$D3;{$ENDIF} 
       {$IFDEF FPC}vmaxpd xmm0, xmm0, xmm2;{$ELSE}db $C5,$F9,$5F,$C2;{$ENDIF} 
       {$IFDEF FPC}vextractf128 xmm2, ymm4, 1;{$ELSE}db $C4,$E3,$7D,$19,$E2,$01;{$ENDIF} 
       {$IFDEF FPC}vmaxpd xmm2, xmm2, xmm4;{$ELSE}db $C5,$E9,$5F,$D4;{$ENDIF} 
       {$IFDEF FPC}vmaxpd xmm0, xmm0, xmm2;{$ELSE}db $C5,$F9,$5F,$C2;{$ENDIF} 

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           add rax, 16;
           jg @addforxloop2end;

           {$IFDEF FPC}vmovupd xmm2, [rcx + rax - 16];{$ELSE}db $C5,$F9,$10,$54,$01,$F0;{$ENDIF} 
           {$IFDEF FPC}vmaxpd xmm0, xmm0, xmm2;{$ELSE}db $C5,$F9,$5F,$C2;{$ENDIF} 
       jmp @addforxloop2;

       @addforxloop2end:

       sub rax, 16;
       jz @nextLine;

       {$IFDEF FPC}vmovsd xmm2, [rcx + rax];{$ELSE}db $C5,$FB,$10,$14,$01;{$ENDIF} 
       {$IFDEF FPC}vmaxsd xmm0, xmm0, xmm2;{$ELSE}db $C5,$FB,$5F,$C2;{$ENDIF} 

       @nextLine:

       // next line:
       add rcx, r9;

   // loop y end
   dec r10;
   jnz @@addforyloop;

   // final max ->
   {$IFDEF FPC}vmovhlps xmm1, xmm1, xmm0;{$ELSE}db $C5,$F0,$12,$C8;{$ENDIF} 
   {$IFDEF FPC}vmaxsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$5F,$C1;{$ENDIF} 

   // epilog - cleanup stack
   {$IFDEF FPC}vmovupd xmm4, [rsp + $00];{$ELSE}db $C5,$F9,$10,$24,$24;{$ENDIF} 
   add rsp, $10;
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 
{$IFDEF FPC}
end;
{$ENDIF}
end;

function AVXMatrixMinAligned(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double;
{$IFDEF FPC}
begin
{$ENDIF}
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

   // note: RCX = mt, RDX = width, R8 = height, R9 = LineWidth
   // prolog - simulate stack
   sub rsp, $10;
   {$IFDEF FPC}vmovupd [rsp + $00], xmm4;{$ELSE}db $C5,$F9,$11,$24,$24;{$ENDIF} 

   imul rdx, -8;

   sub rcx, rdx;

   // init result
   lea rax, [rip + cMaxDouble];
   {$IFDEF FPC}vbroadcastsd ymm0, [rax];{$ELSE}db $C4,$E2,$7D,$19,$00;{$ENDIF} 
   {$IFDEF FPC}vmovapd ymm3, ymm0;{$ELSE}db $C5,$FD,$28,$D8;{$ENDIF} 
   {$IFDEF FPC}vmovapd ymm4, ymm0;{$ELSE}db $C5,$FD,$28,$E0;{$ENDIF} 

   // for y := 0 to height - 1:
   mov r10, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, rdx;
       add rax, 128;
       jg @loopEnd;

       @addforxloop:
           // prefetch data...
           // prefetch [rcx + rax];

           // max:
           {$IFDEF FPC}vminpd ymm3, ymm3, [rcx + rax - 128];{$ELSE}db $C5,$E5,$5D,$5C,$01,$80;{$ENDIF} 
           {$IFDEF FPC}vminpd ymm4, ymm4, [rcx + rax - 96];{$ELSE}db $C5,$DD,$5D,$64,$01,$A0;{$ENDIF} 
           {$IFDEF FPC}vminpd ymm3, ymm3, [rcx + rax - 64];{$ELSE}db $C5,$E5,$5D,$5C,$01,$C0;{$ENDIF} 
           {$IFDEF FPC}vminpd ymm4, ymm4, [rcx + rax - 32];{$ELSE}db $C5,$DD,$5D,$64,$01,$E0;{$ENDIF} 
       add rax, 128;
       jle @addforxloop

       {$IFDEF FPC}vextractf128 xmm2, ymm3, 1;{$ELSE}db $C4,$E3,$7D,$19,$DA,$01;{$ENDIF} 
       {$IFDEF FPC}vminpd xmm2, xmm2, xmm3;{$ELSE}db $C5,$E9,$5D,$D3;{$ENDIF} 
       {$IFDEF FPC}vminpd xmm0, xmm0, xmm2;{$ELSE}db $C5,$F9,$5D,$C2;{$ENDIF} 
       {$IFDEF FPC}vextractf128 xmm2, ymm4, 1;{$ELSE}db $C4,$E3,$7D,$19,$E2,$01;{$ENDIF} 
       {$IFDEF FPC}vminpd xmm2, xmm2, xmm4;{$ELSE}db $C5,$E9,$5D,$D4;{$ENDIF} 
       {$IFDEF FPC}vminpd xmm0, xmm0, xmm2;{$ELSE}db $C5,$F9,$5D,$C2;{$ENDIF} 

       @loopEnd:
       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           add rax, 16;
           jg @addforxloop2end;

           {$IFDEF FPC}vminpd xmm0, xmm0, [rcx + rax - 16];{$ELSE}db $C5,$F9,$5D,$44,$01,$F0;{$ENDIF} 
       jmp @addforxloop2;

       @addforxloop2end:

       sub rax, 16;
       jz @nextLine;

       {$IFDEF FPC}vmovsd xmm2, [rcx + rax];{$ELSE}db $C5,$FB,$10,$14,$01;{$ENDIF} 
       {$IFDEF FPC}vminsd xmm0, xmm0, xmm2;{$ELSE}db $C5,$FB,$5D,$C2;{$ENDIF} 

       @nextLine:

       // next line:
       add rcx, r9;

   // loop y end
   dec r10;
   jnz @@addforyloop;

   // final min ->
   {$IFDEF FPC}vmovhlps xmm1, xmm1, xmm0;{$ELSE}db $C5,$F0,$12,$C8;{$ENDIF} 
   {$IFDEF FPC}vminsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$5D,$C1;{$ENDIF} 

   // epilog - cleanup stack
   {$IFDEF FPC}vmovupd xmm4, [rsp + $00];{$ELSE}db $C5,$F9,$10,$24,$24;{$ENDIF} 
   add rsp, $10;
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 
{$IFDEF FPC}
end;
{$ENDIF}
end;

function AVXMatrixMinUnAligned(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double;
{$IFDEF FPC}
begin
{$ENDIF}
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

   // note: RCX = mt, RDX = width, R8 = height, R9 = LineWidth
   // prolog - simulate stack
   sub rsp, $10;
   {$IFDEF FPC}vmovupd [rsp + $00], xmm4;{$ELSE}db $C5,$F9,$11,$24,$24;{$ENDIF} 

   imul rdx, -8;

   sub rcx, rdx;

   // init result
   lea rax, [rip + cMaxDouble];
   {$IFDEF FPC}vbroadcastsd ymm0, [rax];{$ELSE}db $C4,$E2,$7D,$19,$00;{$ENDIF} 
   {$IFDEF FPC}vmovapd ymm3, ymm0;{$ELSE}db $C5,$FD,$28,$D8;{$ENDIF} 
   {$IFDEF FPC}vmovapd ymm4, ymm0;{$ELSE}db $C5,$FD,$28,$E0;{$ENDIF} 

   // for y := 0 to height - 1:
   mov r10, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, rdx;
       add rax, 128;
       jg @loopEnd;

       @addforxloop:
           // prefetch data...
           // prefetch [rcx + rax];

           // max:
           {$IFDEF FPC}vmovupd ymm2, [rcx + rax - 128];{$ELSE}db $C5,$FD,$10,$54,$01,$80;{$ENDIF} 
           {$IFDEF FPC}vminpd ymm3, ymm3, ymm2;{$ELSE}db $C5,$E5,$5D,$DA;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm2, [rcx + rax - 96];{$ELSE}db $C5,$FD,$10,$54,$01,$A0;{$ENDIF} 
           {$IFDEF FPC}vminpd ymm4, ymm4, ymm2;{$ELSE}db $C5,$DD,$5D,$E2;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm2, [rcx + rax - 64];{$ELSE}db $C5,$FD,$10,$54,$01,$C0;{$ENDIF} 
           {$IFDEF FPC}vminpd ymm3, ymm3, ymm2;{$ELSE}db $C5,$E5,$5D,$DA;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm2, [rcx + rax - 32];{$ELSE}db $C5,$FD,$10,$54,$01,$E0;{$ENDIF} 
           {$IFDEF FPC}vminpd ymm4, ymm4, ymm2;{$ELSE}db $C5,$DD,$5D,$E2;{$ENDIF} 
       add rax, 128;
       jle @addforxloop

       {$IFDEF FPC}vextractf128 xmm2, ymm3, 1;{$ELSE}db $C4,$E3,$7D,$19,$DA,$01;{$ENDIF} 
       {$IFDEF FPC}vminpd xmm2, xmm2, xmm3;{$ELSE}db $C5,$E9,$5D,$D3;{$ENDIF} 
       {$IFDEF FPC}vminpd xmm0, xmm0, xmm2;{$ELSE}db $C5,$F9,$5D,$C2;{$ENDIF} 
       {$IFDEF FPC}vextractf128 xmm2, ymm4, 1;{$ELSE}db $C4,$E3,$7D,$19,$E2,$01;{$ENDIF} 
       {$IFDEF FPC}vminpd xmm2, xmm2, xmm4;{$ELSE}db $C5,$E9,$5D,$D4;{$ENDIF} 
       {$IFDEF FPC}vminpd xmm0, xmm0, xmm2;{$ELSE}db $C5,$F9,$5D,$C2;{$ENDIF} 

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           add rax, 16;
           jg @addforxloop2end;

           {$IFDEF FPC}vmovupd xmm2, [rcx + rax - 16];{$ELSE}db $C5,$F9,$10,$54,$01,$F0;{$ENDIF} 
           {$IFDEF FPC}vminpd xmm0, xmm0, xmm2;{$ELSE}db $C5,$F9,$5D,$C2;{$ENDIF} 
       jmp @addforxloop2;

       @addforxloop2end:
       sub rax, 16;
       jz @nextLine;

       {$IFDEF FPC}vmovsd xmm2, [rcx + rax];{$ELSE}db $C5,$FB,$10,$14,$01;{$ENDIF} 
       {$IFDEF FPC}vminsd xmm0, xmm0, xmm2;{$ELSE}db $C5,$FB,$5D,$C2;{$ENDIF} 

       @nextLine:

       // next line:
       add rcx, r9;

   // loop y end
   dec r10;
   jnz @@addforyloop;

   // final max ->
   {$IFDEF FPC}vmovhlps xmm1, xmm1, xmm0;{$ELSE}db $C5,$F0,$12,$C8;{$ENDIF} 
   {$IFDEF FPC}vminsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$5D,$C1;{$ENDIF} 

   // epilog - cleanup stack
   {$IFDEF FPC}vmovupd xmm4, [rsp + $00];{$ELSE}db $C5,$F9,$10,$24,$24;{$ENDIF} 
   add rsp, $10;
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 
{$IFDEF FPC}
end;
{$ENDIF}
end;

{$ENDIF}

end.
