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

{$I 'mrMath_CPU.inc'}

{$IFDEF x64}

uses MatrixConst;

function AVXMatrixMaxAligned(mt : PDouble; width, height : NativeInt; const LineWidth : NativeInt) : double; {$IFDEF FPC}assembler;{$ENDIF}
function AVXMatrixMaxUnAligned(mt : PDouble; width, height : NativeInt; const LineWidth : NativeInt) : double; {$IFDEF FPC}assembler;{$ENDIF}

function AVXMatrixMinAligned(mt : PDouble; width, height : NativeInt; const LineWidth : NativeInt) : double; {$IFDEF FPC}assembler;{$ENDIF}
function AVXMatrixMinUnAligned(mt : PDouble; width, height : NativeInt; const LineWidth : NativeInt) : double; {$IFDEF FPC}assembler;{$ENDIF}

// in matrix max/min
procedure AVXMatrixMinValUnAligned( dest : PDouble; const LineWidth : NativeInt; width, height : NativeInt; minVal : double ); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure AVXMatrixMaxValUnAligned( dest : PDouble; const LineWidth : NativeInt; width, height : NativeInt; minVal : double ); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

procedure AVXMatrixMinValAligned( dest : PDouble; const LineWidth : NativeInt; width, height : NativeInt; minVal : double ); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure AVXMatrixMaxValAligned( dest : PDouble; const LineWidth : NativeInt; width, height : NativeInt; minVal : double ); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}


{$ENDIF}

implementation

{$IFDEF x64}

function AVXMatrixMaxAligned(mt : PDouble; width, height : NativeInt; const LineWidth : NativeInt) : double; {$IFDEF FPC}assembler;{$ENDIF}
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
   {$IFDEF AVXSUP}vmovupd [rsp + $00], xmm4;                          {$ELSE}db $C5,$F9,$11,$24,$24;{$ENDIF} 

   imul rdx, -8;

   sub rcx, rdx;

   // init result
   lea rax, [rip + cNegMaxDouble];
   {$IFDEF AVXSUP}vbroadcastsd ymm0, [rax];                           {$ELSE}db $C4,$E2,$7D,$19,$00;{$ENDIF} 
   {$IFDEF AVXSUP}vmovapd ymm3, ymm0;                                 {$ELSE}db $C5,$FD,$29,$C3;{$ENDIF} 
   {$IFDEF AVXSUP}vmovapd ymm4, ymm0;                                 {$ELSE}db $C5,$FD,$29,$C4;{$ENDIF} 

   // for y := 0 to height - 1:
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
           {$IFDEF AVXSUP}vmaxpd ymm3, ymm3, [rcx + rax - 128];       {$ELSE}db $C5,$E5,$5F,$5C,$01,$80;{$ENDIF} 
           {$IFDEF AVXSUP}vmaxpd ymm4, ymm4, [rcx + rax - 96];        {$ELSE}db $C5,$DD,$5F,$64,$01,$A0;{$ENDIF} 
           {$IFDEF AVXSUP}vmaxpd ymm3, ymm3, [rcx + rax - 64];        {$ELSE}db $C5,$E5,$5F,$5C,$01,$C0;{$ENDIF} 
           {$IFDEF AVXSUP}vmaxpd ymm4, ymm4, [rcx + rax - 32];        {$ELSE}db $C5,$DD,$5F,$64,$01,$E0;{$ENDIF} 

       add rax, 128;
       jle @addforxloop;

       {$IFDEF AVXSUP}vextractf128 xmm2, ymm3, 1;                     {$ELSE}db $C4,$E3,$7D,$19,$DA,$01;{$ENDIF} 
       {$IFDEF AVXSUP}vmaxpd xmm2, xmm2, xmm3;                        {$ELSE}db $C5,$E9,$5F,$D3;{$ENDIF} 
       {$IFDEF AVXSUP}vmaxpd xmm0, xmm0, xmm2;                        {$ELSE}db $C5,$F9,$5F,$C2;{$ENDIF} 
       {$IFDEF AVXSUP}vextractf128 xmm2, ymm4, 1;                     {$ELSE}db $C4,$E3,$7D,$19,$E2,$01;{$ENDIF} 
       {$IFDEF AVXSUP}vmaxpd xmm2, xmm2, xmm4;                        {$ELSE}db $C5,$E9,$5F,$D4;{$ENDIF} 
       {$IFDEF AVXSUP}vmaxpd xmm0, xmm0, xmm2;                        {$ELSE}db $C5,$F9,$5F,$C2;{$ENDIF} 

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           add rax, 16;
           jg @addforxloop2end;

           {$IFDEF AVXSUP}vmaxpd xmm0, xmm0, [rcx + rax - 16];        {$ELSE}db $C5,$F9,$5F,$44,$01,$F0;{$ENDIF} 
       jmp @addforxloop2;

       @addforxloop2end:

       sub rax, 16;
       jz @nextLine;

       {$IFDEF AVXSUP}vmovsd xmm2, [rcx + rax];                       {$ELSE}db $C5,$FB,$10,$14,$01;{$ENDIF} 
       {$IFDEF AVXSUP}vmaxsd xmm0, xmm0, xmm2;                        {$ELSE}db $C5,$FB,$5F,$C2;{$ENDIF} 

       @nextLine:

       // next line:
       add rcx, r9;

   // loop y end
   dec r8;
   jnz @@addforyloop;

   // final max ->
   {$IFDEF AVXSUP}vmovhlps xmm1, xmm1, xmm0;                          {$ELSE}db $C5,$F0,$12,$C8;{$ENDIF} 
   {$IFDEF AVXSUP}vmaxsd xmm0, xmm0, xmm1;                            {$ELSE}db $C5,$FB,$5F,$C1;{$ENDIF} 

   // epilog - cleanup stack
   {$IFDEF AVXSUP}vmovupd xmm4, [rsp + $00];                          {$ELSE}db $C5,$F9,$10,$24,$24;{$ENDIF} 
   add rsp, $10;
   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;

function AVXMatrixMaxUnAligned(mt : PDouble; width, height : NativeInt; const LineWidth : NativeInt) : double; {$IFDEF FPC}assembler;{$ENDIF}
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
   {$IFDEF AVXSUP}vmovupd [rsp + $00], xmm4;                          {$ELSE}db $C5,$F9,$11,$24,$24;{$ENDIF} 

   imul rdx, -8;

   sub rcx, rdx;

   // init result
   lea rax, [rip + cNegMaxDouble];
   {$IFDEF AVXSUP}vbroadcastsd ymm0, [rax];                           {$ELSE}db $C4,$E2,$7D,$19,$00;{$ENDIF} 
   {$IFDEF AVXSUP}vmovapd ymm3, ymm0;                                 {$ELSE}db $C5,$FD,$29,$C3;{$ENDIF} 
   {$IFDEF AVXSUP}vmovapd ymm4, ymm0;                                 {$ELSE}db $C5,$FD,$29,$C4;{$ENDIF} 

   // for y := 0 to height - 1:
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
           {$IFDEF AVXSUP}vmovupd ymm2, [rcx + rax - 128];            {$ELSE}db $C5,$FD,$10,$54,$01,$80;{$ENDIF} 
           {$IFDEF AVXSUP}vmaxpd ymm3, ymm3, ymm2;                    {$ELSE}db $C5,$E5,$5F,$DA;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd ymm2, [rcx + rax - 96];             {$ELSE}db $C5,$FD,$10,$54,$01,$A0;{$ENDIF} 
           {$IFDEF AVXSUP}vmaxpd ymm4, ymm4, ymm2;                    {$ELSE}db $C5,$DD,$5F,$E2;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd ymm2, [rcx + rax - 64];             {$ELSE}db $C5,$FD,$10,$54,$01,$C0;{$ENDIF} 
           {$IFDEF AVXSUP}vmaxpd ymm3, ymm3, ymm2;                    {$ELSE}db $C5,$E5,$5F,$DA;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd ymm2, [rcx + rax - 32];             {$ELSE}db $C5,$FD,$10,$54,$01,$E0;{$ENDIF} 
           {$IFDEF AVXSUP}vmaxpd ymm4, ymm4, ymm2;                    {$ELSE}db $C5,$DD,$5F,$E2;{$ENDIF} 
       add rax, 128;
       jle @addforxloop;

       {$IFDEF AVXSUP}vextractf128 xmm2, ymm3, 1;                     {$ELSE}db $C4,$E3,$7D,$19,$DA,$01;{$ENDIF} 
       {$IFDEF AVXSUP}vmaxpd xmm2, xmm2, xmm3;                        {$ELSE}db $C5,$E9,$5F,$D3;{$ENDIF} 
       {$IFDEF AVXSUP}vmaxpd xmm0, xmm0, xmm2;                        {$ELSE}db $C5,$F9,$5F,$C2;{$ENDIF} 
       {$IFDEF AVXSUP}vextractf128 xmm2, ymm4, 1;                     {$ELSE}db $C4,$E3,$7D,$19,$E2,$01;{$ENDIF} 
       {$IFDEF AVXSUP}vmaxpd xmm2, xmm2, xmm4;                        {$ELSE}db $C5,$E9,$5F,$D4;{$ENDIF} 
       {$IFDEF AVXSUP}vmaxpd xmm0, xmm0, xmm2;                        {$ELSE}db $C5,$F9,$5F,$C2;{$ENDIF} 

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           add rax, 16;
           jg @addforxloop2end;

           {$IFDEF AVXSUP}vmovupd xmm2, [rcx + rax - 16];             {$ELSE}db $C5,$F9,$10,$54,$01,$F0;{$ENDIF} 
           {$IFDEF AVXSUP}vmaxpd xmm0, xmm0, xmm2;                    {$ELSE}db $C5,$F9,$5F,$C2;{$ENDIF} 
       jmp @addforxloop2;

       @addforxloop2end:

       sub rax, 16;
       jz @nextLine;

       {$IFDEF AVXSUP}vmovsd xmm2, [rcx + rax];                       {$ELSE}db $C5,$FB,$10,$14,$01;{$ENDIF} 
       {$IFDEF AVXSUP}vmaxsd xmm0, xmm0, xmm2;                        {$ELSE}db $C5,$FB,$5F,$C2;{$ENDIF} 

       @nextLine:

       // next line:
       add rcx, r9;

   // loop y end
   dec r8;
   jnz @@addforyloop;

   // final max ->
   {$IFDEF AVXSUP}vmovhlps xmm1, xmm1, xmm0;                          {$ELSE}db $C5,$F0,$12,$C8;{$ENDIF} 
   {$IFDEF AVXSUP}vmaxsd xmm0, xmm0, xmm1;                            {$ELSE}db $C5,$FB,$5F,$C1;{$ENDIF} 

   // epilog - cleanup stack
   {$IFDEF AVXSUP}vmovupd xmm4, [rsp + $00];                          {$ELSE}db $C5,$F9,$10,$24,$24;{$ENDIF} 
   add rsp, $10;
   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;

function AVXMatrixMinAligned(mt : PDouble; width, height : NativeInt; const LineWidth : NativeInt) : double; {$IFDEF FPC}assembler;{$ENDIF}
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
   {$IFDEF AVXSUP}vmovupd [rsp + $00], xmm4;                          {$ELSE}db $C5,$F9,$11,$24,$24;{$ENDIF} 

   imul rdx, -8;

   sub rcx, rdx;

   // init result
   lea rax, [rip + cMaxDouble];
   {$IFDEF AVXSUP}vbroadcastsd ymm0, [rax];                           {$ELSE}db $C4,$E2,$7D,$19,$00;{$ENDIF} 
   {$IFDEF AVXSUP}vmovapd ymm3, ymm0;                                 {$ELSE}db $C5,$FD,$29,$C3;{$ENDIF} 
   {$IFDEF AVXSUP}vmovapd ymm4, ymm0;                                 {$ELSE}db $C5,$FD,$29,$C4;{$ENDIF} 

   // for y := 0 to height - 1:
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
           {$IFDEF AVXSUP}vminpd ymm3, ymm3, [rcx + rax - 128];       {$ELSE}db $C5,$E5,$5D,$5C,$01,$80;{$ENDIF} 
           {$IFDEF AVXSUP}vminpd ymm4, ymm4, [rcx + rax - 96];        {$ELSE}db $C5,$DD,$5D,$64,$01,$A0;{$ENDIF} 
           {$IFDEF AVXSUP}vminpd ymm3, ymm3, [rcx + rax - 64];        {$ELSE}db $C5,$E5,$5D,$5C,$01,$C0;{$ENDIF} 
           {$IFDEF AVXSUP}vminpd ymm4, ymm4, [rcx + rax - 32];        {$ELSE}db $C5,$DD,$5D,$64,$01,$E0;{$ENDIF} 
       add rax, 128;
       jle @addforxloop

       {$IFDEF AVXSUP}vextractf128 xmm2, ymm3, 1;                     {$ELSE}db $C4,$E3,$7D,$19,$DA,$01;{$ENDIF} 
       {$IFDEF AVXSUP}vminpd xmm2, xmm2, xmm3;                        {$ELSE}db $C5,$E9,$5D,$D3;{$ENDIF} 
       {$IFDEF AVXSUP}vminpd xmm0, xmm0, xmm2;                        {$ELSE}db $C5,$F9,$5D,$C2;{$ENDIF} 
       {$IFDEF AVXSUP}vextractf128 xmm2, ymm4, 1;                     {$ELSE}db $C4,$E3,$7D,$19,$E2,$01;{$ENDIF} 
       {$IFDEF AVXSUP}vminpd xmm2, xmm2, xmm4;                        {$ELSE}db $C5,$E9,$5D,$D4;{$ENDIF} 
       {$IFDEF AVXSUP}vminpd xmm0, xmm0, xmm2;                        {$ELSE}db $C5,$F9,$5D,$C2;{$ENDIF} 

       @loopEnd:
       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           add rax, 16;
           jg @addforxloop2end;

           {$IFDEF AVXSUP}vminpd xmm0, xmm0, [rcx + rax - 16];        {$ELSE}db $C5,$F9,$5D,$44,$01,$F0;{$ENDIF} 
       jmp @addforxloop2;

       @addforxloop2end:

       sub rax, 16;
       jz @nextLine;

       {$IFDEF AVXSUP}vmovsd xmm2, [rcx + rax];                       {$ELSE}db $C5,$FB,$10,$14,$01;{$ENDIF} 
       {$IFDEF AVXSUP}vminsd xmm0, xmm0, xmm2;                        {$ELSE}db $C5,$FB,$5D,$C2;{$ENDIF} 

       @nextLine:

       // next line:
       add rcx, r9;

   // loop y end
   dec r8;
   jnz @@addforyloop;

   // final min ->
   {$IFDEF AVXSUP}vmovhlps xmm1, xmm1, xmm0;                          {$ELSE}db $C5,$F0,$12,$C8;{$ENDIF} 
   {$IFDEF AVXSUP}vminsd xmm0, xmm0, xmm1;                            {$ELSE}db $C5,$FB,$5D,$C1;{$ENDIF} 

   // epilog - cleanup stack
   {$IFDEF AVXSUP}vmovupd xmm4, [rsp + $00];                          {$ELSE}db $C5,$F9,$10,$24,$24;{$ENDIF} 
   add rsp, $10;
   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;

function AVXMatrixMinUnAligned(mt : PDouble; width, height : NativeInt; const LineWidth : NativeInt) : double; {$IFDEF FPC}assembler;{$ENDIF}
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
   {$IFDEF AVXSUP}vmovupd [rsp + $00], xmm4;                          {$ELSE}db $C5,$F9,$11,$24,$24;{$ENDIF} 

   imul rdx, -8;

   sub rcx, rdx;

   // init result
   lea rax, [rip + cMaxDouble];
   {$IFDEF AVXSUP}vbroadcastsd ymm0, [rax];                           {$ELSE}db $C4,$E2,$7D,$19,$00;{$ENDIF} 
   {$IFDEF AVXSUP}vmovapd ymm3, ymm0;                                 {$ELSE}db $C5,$FD,$29,$C3;{$ENDIF} 
   {$IFDEF AVXSUP}vmovapd ymm4, ymm0;                                 {$ELSE}db $C5,$FD,$29,$C4;{$ENDIF} 

   // for y := 0 to height - 1:
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
           {$IFDEF AVXSUP}vmovupd ymm2, [rcx + rax - 128];            {$ELSE}db $C5,$FD,$10,$54,$01,$80;{$ENDIF} 
           {$IFDEF AVXSUP}vminpd ymm3, ymm3, ymm2;                    {$ELSE}db $C5,$E5,$5D,$DA;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd ymm2, [rcx + rax - 96];             {$ELSE}db $C5,$FD,$10,$54,$01,$A0;{$ENDIF} 
           {$IFDEF AVXSUP}vminpd ymm4, ymm4, ymm2;                    {$ELSE}db $C5,$DD,$5D,$E2;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd ymm2, [rcx + rax - 64];             {$ELSE}db $C5,$FD,$10,$54,$01,$C0;{$ENDIF} 
           {$IFDEF AVXSUP}vminpd ymm3, ymm3, ymm2;                    {$ELSE}db $C5,$E5,$5D,$DA;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd ymm2, [rcx + rax - 32];             {$ELSE}db $C5,$FD,$10,$54,$01,$E0;{$ENDIF} 
           {$IFDEF AVXSUP}vminpd ymm4, ymm4, ymm2;                    {$ELSE}db $C5,$DD,$5D,$E2;{$ENDIF} 
       add rax, 128;
       jle @addforxloop

       {$IFDEF AVXSUP}vextractf128 xmm2, ymm3, 1;                     {$ELSE}db $C4,$E3,$7D,$19,$DA,$01;{$ENDIF} 
       {$IFDEF AVXSUP}vminpd xmm2, xmm2, xmm3;                        {$ELSE}db $C5,$E9,$5D,$D3;{$ENDIF} 
       {$IFDEF AVXSUP}vminpd xmm0, xmm0, xmm2;                        {$ELSE}db $C5,$F9,$5D,$C2;{$ENDIF} 
       {$IFDEF AVXSUP}vextractf128 xmm2, ymm4, 1;                     {$ELSE}db $C4,$E3,$7D,$19,$E2,$01;{$ENDIF} 
       {$IFDEF AVXSUP}vminpd xmm2, xmm2, xmm4;                        {$ELSE}db $C5,$E9,$5D,$D4;{$ENDIF} 
       {$IFDEF AVXSUP}vminpd xmm0, xmm0, xmm2;                        {$ELSE}db $C5,$F9,$5D,$C2;{$ENDIF} 

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           add rax, 16;
           jg @addforxloop2end;

           {$IFDEF AVXSUP}vmovupd xmm2, [rcx + rax - 16];             {$ELSE}db $C5,$F9,$10,$54,$01,$F0;{$ENDIF} 
           {$IFDEF AVXSUP}vminpd xmm0, xmm0, xmm2;                    {$ELSE}db $C5,$F9,$5D,$C2;{$ENDIF} 
       jmp @addforxloop2;

       @addforxloop2end:
       sub rax, 16;
       jz @nextLine;

       {$IFDEF AVXSUP}vmovsd xmm2, [rcx + rax];                       {$ELSE}db $C5,$FB,$10,$14,$01;{$ENDIF} 
       {$IFDEF AVXSUP}vminsd xmm0, xmm0, xmm2;                        {$ELSE}db $C5,$FB,$5D,$C2;{$ENDIF} 

       @nextLine:

       // next line:
       add rcx, r9;

   // loop y end
   dec r8;
   jnz @@addforyloop;

   // final max ->
   {$IFDEF AVXSUP}vmovhlps xmm1, xmm1, xmm0;                          {$ELSE}db $C5,$F0,$12,$C8;{$ENDIF} 
   {$IFDEF AVXSUP}vminsd xmm0, xmm0, xmm1;                            {$ELSE}db $C5,$FB,$5D,$C1;{$ENDIF} 

   // epilog - cleanup stack
   {$IFDEF AVXSUP}vmovupd xmm4, [rsp + $00];                          {$ELSE}db $C5,$F9,$10,$24,$24;{$ENDIF} 
   add rsp, $10;
   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;

procedure AVXMatrixMinValUnAligned( dest : PDouble; const LineWidth : NativeInt; width, height : NativeInt; minVal : double ); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
// rcx = dest; rdx = LineWidth, r8 = Width, r9 = height
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

   imul r8, -8;

   // helper registers for the dest pointer
   sub rcx, r8;

   lea rax, minVal;
   {$IFDEF AVXSUP}vbroadcastsd ymm0, [rax];                           {$ELSE}db $C4,$E2,$7D,$19,$00;{$ENDIF} 

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
           {$IFDEF AVXSUP}vminpd ymm1, ymm1, ymm0;                    {$ELSE}db $C5,$F5,$5D,$C8;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd [rcx + rax - 128], ymm1;            {$ELSE}db $C5,$FD,$11,$4C,$01,$80;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd ymm2, [rcx + rax - 96];             {$ELSE}db $C5,$FD,$10,$54,$01,$A0;{$ENDIF} 
           {$IFDEF AVXSUP}vminpd ymm2, ymm2, ymm0;                    {$ELSE}db $C5,$ED,$5D,$D0;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd [rcx + rax - 96], ymm2;             {$ELSE}db $C5,$FD,$11,$54,$01,$A0;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd ymm3, [rcx + rax - 64];             {$ELSE}db $C5,$FD,$10,$5C,$01,$C0;{$ENDIF} 
           {$IFDEF AVXSUP}vminpd ymm3, ymm3, ymm0;                    {$ELSE}db $C5,$E5,$5D,$D8;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd [rcx + rax - 64], ymm3;             {$ELSE}db $C5,$FD,$11,$5C,$01,$C0;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd ymm4, [rcx + rax - 32];             {$ELSE}db $C5,$FD,$10,$64,$01,$E0;{$ENDIF} 
           {$IFDEF AVXSUP}vminpd ymm4, ymm4, ymm0;                    {$ELSE}db $C5,$DD,$5D,$E0;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd [rcx + rax - 32], ymm4;             {$ELSE}db $C5,$FD,$11,$64,$01,$E0;{$ENDIF} 
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           add rax, 16;
           jg @loopEnd2;

           {$IFDEF AVXSUP}vmovupd xmm1, [rcx + rax - 16];             {$ELSE}db $C5,$F9,$10,$4C,$01,$F0;{$ENDIF} 
           {$IFDEF AVXSUP}vminpd xmm1, xmm1, xmm0;                    {$ELSE}db $C5,$F1,$5D,$C8;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd [rcx + rax - 16], xmm1;             {$ELSE}db $C5,$F9,$11,$4C,$01,$F0;{$ENDIF} 
       jmp @addforxloop2;

       @loopEnd2:

       sub rax, 16;
       jz @nextLine;

       {$IFDEF AVXSUP}vmovsd xmm1, [rcx + rax];                       {$ELSE}db $C5,$FB,$10,$0C,$01;{$ENDIF} 
       {$IFDEF AVXSUP}vminsd xmm1, xmm1, xmm0;                        {$ELSE}db $C5,$F3,$5D,$C8;{$ENDIF} 
       {$IFDEF AVXSUP}vmovsd [rcx + rax], xmm1;                       {$ELSE}db $C5,$FB,$11,$0C,$01;{$ENDIF} 

       @nextLine:

       // next line:
       add rcx, rdx;

   // loop y end
   dec r9;
   jnz @@addforyloop;

   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;


procedure AVXMatrixMaxValUnAligned( dest : PDouble; const LineWidth : NativeInt; width, height : NativeInt; minVal : double ); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
// rcx = dest; rdx = LineWidth, r8 = Width, r9 = height
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

   imul r8, -8;

   // helper registers for the dest pointer
   sub rcx, r8;

   lea rax, minVal;
   {$IFDEF AVXSUP}vbroadcastsd ymm0, [rax];                           {$ELSE}db $C4,$E2,$7D,$19,$00;{$ENDIF} 

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
           {$IFDEF AVXSUP}vmaxpd ymm1, ymm1, ymm0;                    {$ELSE}db $C5,$F5,$5F,$C8;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd [rcx + rax - 128], ymm1;            {$ELSE}db $C5,$FD,$11,$4C,$01,$80;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd ymm2, [rcx + rax - 96];             {$ELSE}db $C5,$FD,$10,$54,$01,$A0;{$ENDIF} 
           {$IFDEF AVXSUP}vmaxpd ymm2, ymm2, ymm0;                    {$ELSE}db $C5,$ED,$5F,$D0;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd [rcx + rax - 96], ymm2;             {$ELSE}db $C5,$FD,$11,$54,$01,$A0;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd ymm3, [rcx + rax - 64];             {$ELSE}db $C5,$FD,$10,$5C,$01,$C0;{$ENDIF} 
           {$IFDEF AVXSUP}vmaxpd ymm3, ymm3, ymm0;                    {$ELSE}db $C5,$E5,$5F,$D8;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd [rcx + rax - 64], ymm3;             {$ELSE}db $C5,$FD,$11,$5C,$01,$C0;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd ymm4, [rcx + rax - 32];             {$ELSE}db $C5,$FD,$10,$64,$01,$E0;{$ENDIF} 
           {$IFDEF AVXSUP}vmaxpd ymm4, ymm4, ymm0;                    {$ELSE}db $C5,$DD,$5F,$E0;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd [rcx + rax - 32], ymm4;             {$ELSE}db $C5,$FD,$11,$64,$01,$E0;{$ENDIF} 
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           add rax, 16;
           jg @loopEnd2;

           {$IFDEF AVXSUP}vmovupd xmm1, [rcx + rax - 16];             {$ELSE}db $C5,$F9,$10,$4C,$01,$F0;{$ENDIF} 
           {$IFDEF AVXSUP}vmaxpd xmm1, xmm1, xmm0;                    {$ELSE}db $C5,$F1,$5F,$C8;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd [rcx + rax - 16], xmm1;             {$ELSE}db $C5,$F9,$11,$4C,$01,$F0;{$ENDIF} 
       jmp @addforxloop2;

       @loopEnd2:

       sub rax, 16;
       jz @nextLine;

       {$IFDEF AVXSUP}vmovsd xmm1, [rcx + rax];                       {$ELSE}db $C5,$FB,$10,$0C,$01;{$ENDIF} 
       {$IFDEF AVXSUP}vmaxsd xmm1, xmm1, xmm0;                        {$ELSE}db $C5,$F3,$5F,$C8;{$ENDIF} 
       {$IFDEF AVXSUP}vmovsd [rcx + rax], xmm1;                       {$ELSE}db $C5,$FB,$11,$0C,$01;{$ENDIF} 

       @nextLine:

       // next line:
       add rcx, rdx;

   // loop y end
   dec r9;
   jnz @@addforyloop;

   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;


procedure AVXMatrixMinValAligned( dest : PDouble; const LineWidth : NativeInt; width, height : NativeInt; minVal : double ); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
// rcx = dest; rdx = LineWidth, r8 = Width, r9 = height
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

   imul r8, -8;

   // helper registers for the dest pointer
   sub rcx, r8;

   lea rax, minVal;
   {$IFDEF AVXSUP}vbroadcastsd ymm0, [rax];                           {$ELSE}db $C4,$E2,$7D,$19,$00;{$ENDIF} 

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
           {$IFDEF AVXSUP}vminpd ymm1, ymm0, [rcx + rax - 128];       {$ELSE}db $C5,$FD,$5D,$4C,$01,$80;{$ENDIF} 
           {$IFDEF AVXSUP}vmovapd [rcx + rax - 128], ymm1;            {$ELSE}db $C5,$FD,$29,$4C,$01,$80;{$ENDIF} 

           {$IFDEF AVXSUP}vminpd ymm2, ymm0, [rcx + rax - 96];        {$ELSE}db $C5,$FD,$5D,$54,$01,$A0;{$ENDIF} 
           {$IFDEF AVXSUP}vmovapd [rcx + rax - 96], ymm2;             {$ELSE}db $C5,$FD,$29,$54,$01,$A0;{$ENDIF} 

           {$IFDEF AVXSUP}vminpd ymm3, ymm0, [rcx + rax - 64];        {$ELSE}db $C5,$FD,$5D,$5C,$01,$C0;{$ENDIF} 
           {$IFDEF AVXSUP}vmovapd [rcx + rax - 64], ymm3;             {$ELSE}db $C5,$FD,$29,$5C,$01,$C0;{$ENDIF} 

           {$IFDEF AVXSUP}vminpd ymm4, ymm0, [rcx + rax - 32];        {$ELSE}db $C5,$FD,$5D,$64,$01,$E0;{$ENDIF} 
           {$IFDEF AVXSUP}vmovapd [rcx + rax - 32], ymm4;             {$ELSE}db $C5,$FD,$29,$64,$01,$E0;{$ENDIF} 
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           add rax, 16;
           jg @loopEnd2;

           {$IFDEF AVXSUP}vminpd xmm1, xmm0, [rcx + rax - 16];        {$ELSE}db $C5,$F9,$5D,$4C,$01,$F0;{$ENDIF} 
           {$IFDEF AVXSUP}vmovapd [rcx + rax - 16], xmm1;             {$ELSE}db $C5,$F9,$29,$4C,$01,$F0;{$ENDIF} 
       jmp @addforxloop2;

       @loopEnd2:

       sub rax, 16;
       jz @nextLine;

       {$IFDEF AVXSUP}vmovsd xmm1, [rcx + rax];                       {$ELSE}db $C5,$FB,$10,$0C,$01;{$ENDIF} 
       {$IFDEF AVXSUP}vminsd xmm1, xmm1, xmm0;                        {$ELSE}db $C5,$F3,$5D,$C8;{$ENDIF} 
       {$IFDEF AVXSUP}vmovsd [rcx + rax], xmm1;                       {$ELSE}db $C5,$FB,$11,$0C,$01;{$ENDIF} 

       @nextLine:

       // next line:
       add rcx, rdx;

   // loop y end
   dec r9;
   jnz @@addforyloop;

   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;

procedure AVXMatrixMaxValAligned( dest : PDouble; const LineWidth : NativeInt; width, height : NativeInt; minVal : double ); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
// rcx = dest; rdx = LineWidth, r8 = Width, r9 = height
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
   imul r8, -8;

   // helper registers for the dest pointer
   sub rcx, r8;

   lea rax, minVal;
   {$IFDEF AVXSUP}vbroadcastsd ymm0, [rax];                           {$ELSE}db $C4,$E2,$7D,$19,$00;{$ENDIF} 

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
           {$IFDEF AVXSUP}vmaxpd ymm1, ymm0, [rcx + rax - 128];       {$ELSE}db $C5,$FD,$5F,$4C,$01,$80;{$ENDIF} 
           {$IFDEF AVXSUP}vmovapd [rcx + rax - 128], ymm1;            {$ELSE}db $C5,$FD,$29,$4C,$01,$80;{$ENDIF} 

           {$IFDEF AVXSUP}vmaxpd ymm2, ymm0, [rcx + rax - 96];        {$ELSE}db $C5,$FD,$5F,$54,$01,$A0;{$ENDIF} 
           {$IFDEF AVXSUP}vmovapd [rcx + rax - 96], ymm2;             {$ELSE}db $C5,$FD,$29,$54,$01,$A0;{$ENDIF} 

           {$IFDEF AVXSUP}vmaxpd ymm3, ymm0, [rcx + rax - 64];        {$ELSE}db $C5,$FD,$5F,$5C,$01,$C0;{$ENDIF} 
           {$IFDEF AVXSUP}vmovapd [rcx + rax - 64], ymm3;             {$ELSE}db $C5,$FD,$29,$5C,$01,$C0;{$ENDIF} 

           {$IFDEF AVXSUP}vmaxpd ymm4, ymm0, [rcx + rax - 32];        {$ELSE}db $C5,$FD,$5F,$64,$01,$E0;{$ENDIF} 
           {$IFDEF AVXSUP}vmovapd [rcx + rax - 32], ymm4;             {$ELSE}db $C5,$FD,$29,$64,$01,$E0;{$ENDIF} 
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           add rax, 16;
           jg @loopEnd2;

           {$IFDEF AVXSUP}vmaxpd xmm1, xmm0, [rcx + rax - 16];        {$ELSE}db $C5,$F9,$5F,$4C,$01,$F0;{$ENDIF} 
           {$IFDEF AVXSUP}vmovapd [rcx + rax - 16], xmm1;             {$ELSE}db $C5,$F9,$29,$4C,$01,$F0;{$ENDIF} 
       jmp @addforxloop2;

       @loopEnd2:

       sub rax, 16;
       jz @nextLine;

       {$IFDEF AVXSUP}vmovsd xmm1, [rcx + rax];                       {$ELSE}db $C5,$FB,$10,$0C,$01;{$ENDIF} 
       {$IFDEF AVXSUP}vmaxsd xmm1, xmm1, xmm0;                        {$ELSE}db $C5,$F3,$5F,$C8;{$ENDIF} 
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
