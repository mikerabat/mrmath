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


unit AVXMatrixMinMaxOperations;

interface

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}
{$IFNDEF x64}

uses MatrixConst;

function AVXMatrixMaxAligned(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double;
function AVXMatrixMaxUnAligned(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double;

function AVXMatrixMinAligned(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double;
function AVXMatrixMinUnAligned(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double;

{$ENDIF}

implementation

{$IFNDEF x64}

{$WARNINGS OFF}

{$IFDEF FPC} {$ASMMODE intel} {$ENDIF}

function AVXMatrixMaxAligned(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double;
begin
asm
   // prolog - simulate stack
   push ebx;
   push edi;

   // prepare pointers
   mov edi, LineWidth;
   mov ecx, mt;
   mov edx, width;
   imul edx, -8;

   sub ecx, edx;

   // init result
   lea eax, cNegMaxDouble;
   {$IFDEF FPC}vbroadcastsd ymm0, [eax];{$ELSE}db $C4,$E2,$7D,$19,$00;{$ENDIF} 
   {$IFDEF FPC}vmovapd ymm3, ymm0;{$ELSE}db $C5,$FD,$28,$D8;{$ENDIF} 
   {$IFDEF FPC}vmovapd ymm4, ymm0;{$ELSE}db $C5,$FD,$28,$E0;{$ENDIF} 

   // for y := 0 to height - 1:
   mov ebx, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov eax, edx;
       add eax, 128;
       jg @loopEnd;

       @addforxloop:
           // prefetch data...
           // prefetch [ecx + eax];

           // max:
           {$IFDEF FPC}vmaxpd ymm3, ymm3, [ecx + eax - 128];{$ELSE}db $C5,$E5,$5F,$5C,$01,$80;{$ENDIF} 
           {$IFDEF FPC}vmaxpd ymm4, ymm4, [ecx + eax - 96];{$ELSE}db $C5,$DD,$5F,$64,$01,$A0;{$ENDIF} 
           {$IFDEF FPC}vmaxpd ymm3, ymm3, [ecx + eax - 64];{$ELSE}db $C5,$E5,$5F,$5C,$01,$C0;{$ENDIF} 
           {$IFDEF FPC}vmaxpd ymm4, ymm4, [ecx + eax - 32];{$ELSE}db $C5,$DD,$5F,$64,$01,$E0;{$ENDIF} 

       add eax, 128;
       jle @addforxloop;

       {$IFDEF FPC}vextractf128 xmm2, ymm3, 1;{$ELSE}db $C4,$E3,$7D,$19,$DA,$01;{$ENDIF} 
       {$IFDEF FPC}vmaxpd xmm2, xmm2, xmm3;{$ELSE}db $C5,$E9,$5F,$D3;{$ENDIF} 
       {$IFDEF FPC}vmaxpd xmm0, xmm0, xmm2;{$ELSE}db $C5,$F9,$5F,$C2;{$ENDIF} 
       {$IFDEF FPC}vextractf128 xmm2, ymm4, 1;{$ELSE}db $C4,$E3,$7D,$19,$E2,$01;{$ENDIF} 
       {$IFDEF FPC}vmaxpd xmm2, xmm2, xmm4;{$ELSE}db $C5,$E9,$5F,$D4;{$ENDIF} 
       {$IFDEF FPC}vmaxpd xmm0, xmm0, xmm2;{$ELSE}db $C5,$F9,$5F,$C2;{$ENDIF} 

       @loopEnd:

       sub eax, 128;

       jz @nextLine;

       @addforxloop2:
           add eax, 16;
           jg @addforxloop2end;

           {$IFDEF FPC}vmaxpd xmm0, xmm0, [ecx + eax - 16];{$ELSE}db $C5,$F9,$5F,$44,$01,$F0;{$ENDIF} 
       jmp @addforxloop2;

       @addforxloop2end:

       sub eax, 16;
       jz @nextLine;

       {$IFDEF FPC}vmovsd xmm2, [ecx + eax];{$ELSE}db $C5,$FB,$10,$14,$01;{$ENDIF} 
       {$IFDEF FPC}vmaxsd xmm0, xmm0, xmm2;{$ELSE}db $C5,$FB,$5F,$C2;{$ENDIF} 

       @nextLine:

       // next line:
       add ecx, edi;

   // loop y end
   dec ebx;
   jnz @@addforyloop;

   // final max ->
   {$IFDEF FPC}vmovhlps xmm1, xmm1, xmm0;{$ELSE}db $C5,$F0,$12,$C8;{$ENDIF} 
   {$IFDEF FPC}vmaxsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$5F,$C1;{$ENDIF} 

   {$IFDEF FPC}vmovsd Result, xmm0;{$ELSE}db $C5,$FB,$11,$45,$E8;{$ENDIF} 

   // epilog - cleanup stack
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop ebx;
   pop edi;
end;
end;

function AVXMatrixMaxUnAligned(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double;
begin
asm
   // prolog - simulate stack
   push ebx;
   push edi;

   // prepare pointers
   mov edi, LineWidth;
   mov ecx, mt;
   mov edx, width;
   imul edx, -8;

   sub ecx, edx;

   // init result
   lea eax, cNegMaxDouble;
   {$IFDEF FPC}vbroadcastsd ymm0, [eax];{$ELSE}db $C4,$E2,$7D,$19,$00;{$ENDIF} 
   {$IFDEF FPC}vmovapd ymm3, ymm0;{$ELSE}db $C5,$FD,$28,$D8;{$ENDIF} 
   {$IFDEF FPC}vmovapd ymm4, ymm0;{$ELSE}db $C5,$FD,$28,$E0;{$ENDIF} 

   // for y := 0 to height - 1:
   mov ebx, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov eax, edx;

       add eax, 128;
       jg @loopEnd;

       @addforxloop:
           // prefetch data...
           // prefetch [ecx + eax];

           // max:
           {$IFDEF FPC}vmovupd ymm2, [ecx + eax - 128];{$ELSE}db $C5,$FD,$10,$54,$01,$80;{$ENDIF} 
           {$IFDEF FPC}vmaxpd ymm3, ymm3, ymm2;{$ELSE}db $C5,$E5,$5F,$DA;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm2, [ecx + eax - 96];{$ELSE}db $C5,$FD,$10,$54,$01,$A0;{$ENDIF} 
           {$IFDEF FPC}vmaxpd ymm4, ymm4, ymm2;{$ELSE}db $C5,$DD,$5F,$E2;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm2, [ecx + eax - 64];{$ELSE}db $C5,$FD,$10,$54,$01,$C0;{$ENDIF} 
           {$IFDEF FPC}vmaxpd ymm3, ymm3, ymm2;{$ELSE}db $C5,$E5,$5F,$DA;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm2, [ecx + eax - 32];{$ELSE}db $C5,$FD,$10,$54,$01,$E0;{$ENDIF} 
           {$IFDEF FPC}vmaxpd ymm4, ymm4, ymm2;{$ELSE}db $C5,$DD,$5F,$E2;{$ENDIF} 
       add eax, 128;
       jle @addforxloop;

       {$IFDEF FPC}vextractf128 xmm2, ymm3, 1;{$ELSE}db $C4,$E3,$7D,$19,$DA,$01;{$ENDIF} 
       {$IFDEF FPC}vmaxpd xmm2, xmm2, xmm3;{$ELSE}db $C5,$E9,$5F,$D3;{$ENDIF} 
       {$IFDEF FPC}vmaxpd xmm0, xmm0, xmm2;{$ELSE}db $C5,$F9,$5F,$C2;{$ENDIF} 
       {$IFDEF FPC}vextractf128 xmm2, ymm4, 1;{$ELSE}db $C4,$E3,$7D,$19,$E2,$01;{$ENDIF} 
       {$IFDEF FPC}vmaxpd xmm2, xmm2, xmm4;{$ELSE}db $C5,$E9,$5F,$D4;{$ENDIF} 
       {$IFDEF FPC}vmaxpd xmm0, xmm0, xmm2;{$ELSE}db $C5,$F9,$5F,$C2;{$ENDIF} 

       @loopEnd:

       sub eax, 128;

       jz @nextLine;

       @addforxloop2:
           add eax, 16;
           jg @addforxloop2end;

           {$IFDEF FPC}vmovupd xmm2, [ecx + eax - 16];{$ELSE}db $C5,$F9,$10,$54,$01,$F0;{$ENDIF} 
           {$IFDEF FPC}vmaxpd xmm0, xmm0, xmm2;{$ELSE}db $C5,$F9,$5F,$C2;{$ENDIF} 
       jmp @addforxloop2;

       @addforxloop2end:

       sub eax, 16;
       jz @nextLine;

       {$IFDEF FPC}vmovsd xmm2, [ecx + eax];{$ELSE}db $C5,$FB,$10,$14,$01;{$ENDIF} 
       {$IFDEF FPC}vmaxsd xmm0, xmm0, xmm2;{$ELSE}db $C5,$FB,$5F,$C2;{$ENDIF} 

       @nextLine:

       // next line:
       add ecx, edi;

   // loop y end
   dec ebx;
   jnz @@addforyloop;

   // final max ->
   {$IFDEF FPC}vmovhlps xmm1, xmm1, xmm0;{$ELSE}db $C5,$F0,$12,$C8;{$ENDIF} 
   {$IFDEF FPC}vmaxsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$5F,$C1;{$ENDIF} 
   {$IFDEF FPC}vmovsd Result, xmm0;{$ELSE}db $C5,$FB,$11,$45,$E8;{$ENDIF} 

   // epilog - cleanup stack
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop ebx;
   pop edi;
end;
end;

function AVXMatrixMinAligned(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double;
begin
asm
   // prolog - simulate stack
   push ebx;
   push edi;

   // prepare pointers
   mov edi, LineWidth;
   mov ecx, mt;
   mov edx, width;
   imul edx, -8;

   sub ecx, edx;

   // init result
   lea eax, cMaxDouble;
   {$IFDEF FPC}vbroadcastsd ymm0, [eax];{$ELSE}db $C4,$E2,$7D,$19,$00;{$ENDIF} 
   {$IFDEF FPC}vmovapd ymm3, ymm0;{$ELSE}db $C5,$FD,$28,$D8;{$ENDIF} 
   {$IFDEF FPC}vmovapd ymm4, ymm0;{$ELSE}db $C5,$FD,$28,$E0;{$ENDIF} 

   // for y := 0 to height - 1:
   mov ebx, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov eax, edx;
       add eax, 128;
       jg @loopEnd;

       @addforxloop:
           // prefetch data...
           // prefetch [ecx + eax];

           // max:
           {$IFDEF FPC}vminpd ymm3, ymm3, [ecx + eax - 128];{$ELSE}db $C5,$E5,$5D,$5C,$01,$80;{$ENDIF} 
           {$IFDEF FPC}vminpd ymm4, ymm4, [ecx + eax - 96];{$ELSE}db $C5,$DD,$5D,$64,$01,$A0;{$ENDIF} 
           {$IFDEF FPC}vminpd ymm3, ymm3, [ecx + eax - 64];{$ELSE}db $C5,$E5,$5D,$5C,$01,$C0;{$ENDIF} 
           {$IFDEF FPC}vminpd ymm4, ymm4, [ecx + eax - 32];{$ELSE}db $C5,$DD,$5D,$64,$01,$E0;{$ENDIF} 

       add eax, 128;
       jle @addforxloop;

       {$IFDEF FPC}vextractf128 xmm2, ymm3, 1;{$ELSE}db $C4,$E3,$7D,$19,$DA,$01;{$ENDIF} 
       {$IFDEF FPC}vminpd xmm2, xmm2, xmm3;{$ELSE}db $C5,$E9,$5D,$D3;{$ENDIF} 
       {$IFDEF FPC}vminpd xmm0, xmm0, xmm2;{$ELSE}db $C5,$F9,$5D,$C2;{$ENDIF} 
       {$IFDEF FPC}vextractf128 xmm2, ymm4, 1;{$ELSE}db $C4,$E3,$7D,$19,$E2,$01;{$ENDIF} 
       {$IFDEF FPC}vminpd xmm2, xmm2, xmm4;{$ELSE}db $C5,$E9,$5D,$D4;{$ENDIF} 
       {$IFDEF FPC}vminpd xmm0, xmm0, xmm2;{$ELSE}db $C5,$F9,$5D,$C2;{$ENDIF} 

       @loopEnd:

       sub eax, 128;

       jz @nextLine;

       @addforxloop2:
           add eax, 16;
           jg @addforxloop2end;

           {$IFDEF FPC}vminpd xmm0, xmm0, [ecx + eax - 16];{$ELSE}db $C5,$F9,$5D,$44,$01,$F0;{$ENDIF} 
       jmp @addforxloop2;

       @addforxloop2end:

       sub eax, 16;
       jz @nextLine;

       {$IFDEF FPC}vmovsd xmm2, [ecx + eax];{$ELSE}db $C5,$FB,$10,$14,$01;{$ENDIF} 
       {$IFDEF FPC}vminsd xmm0, xmm0, xmm2;{$ELSE}db $C5,$FB,$5D,$C2;{$ENDIF} 

       @nextLine:

       // next line:
       add ecx, edi;

   // loop y end
   dec ebx;
   jnz @@addforyloop;

   // final max ->
   {$IFDEF FPC}vmovhlps xmm1, xmm1, xmm0;{$ELSE}db $C5,$F0,$12,$C8;{$ENDIF} 
   {$IFDEF FPC}vminsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$5D,$C1;{$ENDIF} 
   {$IFDEF FPC}vmovsd Result, xmm0;{$ELSE}db $C5,$FB,$11,$45,$E8;{$ENDIF} 

   // epilog - cleanup stack
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop ebx;
   pop edi;
end;
end;

function AVXMatrixMinUnAligned(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double;
begin
asm
   // prolog - simulate stack
   push ebx;
   push edi;

   // prepare pointers
   mov edi, LineWidth;
   mov ecx, mt;
   mov edx, width;
   imul edx, -8;

   sub ecx, edx;

   // init result
   lea eax, cMaxDouble;
   {$IFDEF FPC}vbroadcastsd ymm0, [eax];{$ELSE}db $C4,$E2,$7D,$19,$00;{$ENDIF} 
   {$IFDEF FPC}vmovapd ymm3, ymm0;{$ELSE}db $C5,$FD,$28,$D8;{$ENDIF} 
   {$IFDEF FPC}vmovapd ymm4, ymm0;{$ELSE}db $C5,$FD,$28,$E0;{$ENDIF} 

   // for y := 0 to height - 1:
   mov ebx, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov eax, edx;

       add eax, 128;
       jg @loopEnd;

       @addforxloop:
           // prefetch data...
           // prefetch [ecx + eax];

           // max:
           {$IFDEF FPC}vmovupd ymm2, [ecx + eax - 128];{$ELSE}db $C5,$FD,$10,$54,$01,$80;{$ENDIF} 
           {$IFDEF FPC}vminpd ymm3, ymm3, ymm2;{$ELSE}db $C5,$E5,$5D,$DA;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm2, [ecx + eax - 96];{$ELSE}db $C5,$FD,$10,$54,$01,$A0;{$ENDIF} 
           {$IFDEF FPC}vminpd ymm4, ymm4, ymm2;{$ELSE}db $C5,$DD,$5D,$E2;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm2, [ecx + eax - 64];{$ELSE}db $C5,$FD,$10,$54,$01,$C0;{$ENDIF} 
           {$IFDEF FPC}vminpd ymm3, ymm3, ymm2;{$ELSE}db $C5,$E5,$5D,$DA;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm2, [ecx + eax - 32];{$ELSE}db $C5,$FD,$10,$54,$01,$E0;{$ENDIF} 
           {$IFDEF FPC}vminpd ymm4, ymm4, ymm2;{$ELSE}db $C5,$DD,$5D,$E2;{$ENDIF} 
       add eax, 128;
       jle @addforxloop;

       {$IFDEF FPC}vextractf128 xmm2, ymm3, 1;{$ELSE}db $C4,$E3,$7D,$19,$DA,$01;{$ENDIF} 
       {$IFDEF FPC}vminpd xmm2, xmm2, xmm3;{$ELSE}db $C5,$E9,$5D,$D3;{$ENDIF} 
       {$IFDEF FPC}vminpd xmm0, xmm0, xmm2;{$ELSE}db $C5,$F9,$5D,$C2;{$ENDIF} 
       {$IFDEF FPC}vextractf128 xmm2, ymm4, 1;{$ELSE}db $C4,$E3,$7D,$19,$E2,$01;{$ENDIF} 
       {$IFDEF FPC}vminpd xmm2, xmm2, xmm4;{$ELSE}db $C5,$E9,$5D,$D4;{$ENDIF} 
       {$IFDEF FPC}vminpd xmm0, xmm0, xmm2;{$ELSE}db $C5,$F9,$5D,$C2;{$ENDIF} 

       @loopEnd:

       sub eax, 128;

       jz @nextLine;

       @addforxloop2:
           add eax, 16;
           jg @addforxloop2end;

           {$IFDEF FPC}vmovupd xmm2, [ecx + eax - 16];{$ELSE}db $C5,$F9,$10,$54,$01,$F0;{$ENDIF} 
           {$IFDEF FPC}vminpd xmm0, xmm0, xmm2;{$ELSE}db $C5,$F9,$5D,$C2;{$ENDIF} 
       jmp @addforxloop2;

       @addforxloop2end:

       sub eax, 16;
       jz @nextLine;

       {$IFDEF FPC}vmovsd xmm2, [ecx + eax];{$ELSE}db $C5,$FB,$10,$14,$01;{$ENDIF} 
       {$IFDEF FPC}vminsd xmm0, xmm0, xmm2;{$ELSE}db $C5,$FB,$5D,$C2;{$ENDIF} 

       @nextLine:

       // next line:
       add ecx, edi;

   // loop y end
   dec ebx;
   jnz @@addforyloop;

   // final max ->
   {$IFDEF FPC}vmovhlps xmm1, xmm1, xmm0;{$ELSE}db $C5,$F0,$12,$C8;{$ENDIF} 
   {$IFDEF FPC}vminsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$5D,$C1;{$ENDIF} 
   {$IFDEF FPC}vmovsd Result, xmm0;{$ELSE}db $C5,$FB,$11,$45,$E8;{$ENDIF} 

   // epilog - cleanup stack
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop ebx;
   pop edi;
end;
end;

{$ENDIF}

end.
