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

{$I 'mrMath_CPU.inc'}

{$IFNDEF x64}

function AVXMatrixMaxAligned(mt : PDouble; width, height : NativeInt; const LineWidth : NativeInt) : double; {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
function AVXMatrixMaxUnAligned(mt : PDouble; width, height : NativeInt; const LineWidth : NativeInt) : double; {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

function AVXMatrixMinAligned(mt : PDouble; width, height : NativeInt; const LineWidth : NativeInt) : double; {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
function AVXMatrixMinUnAligned(mt : PDouble; width, height : NativeInt; const LineWidth : NativeInt) : double; {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

{$ENDIF}

implementation

{$IFNDEF x64}

{$WARNINGS OFF}

const cLocNegMaxDouble : double = -1.7e+308;
      cLocMaxDouble : double = 1.7e+308;

function AVXMatrixMaxAligned(mt : PDouble; width, height : NativeInt; const LineWidth : NativeInt) : double;
asm
   // prolog - simulate stack
   push edi;
   push esi;

   // prepare pointers
   mov edi, LineWidth;
   imul edx, -8;

   sub eax, edx;

   // init result
   lea esi, cLocNegMaxDouble;
   {$IFDEF FPC}vbroadcastsd ymm0, [esi];{$ELSE}db $C4,$E2,$7D,$19,$06;{$ENDIF} 
   {$IFDEF FPC}vmovapd ymm3, ymm0;{$ELSE}db $C5,$FD,$29,$C3;{$ENDIF} 
   {$IFDEF FPC}vmovapd ymm4, ymm0;{$ELSE}db $C5,$FD,$29,$C4;{$ENDIF} 

   // for y := 0 to height - 1:
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov esi, edx;
       add esi, 128;
       jg @loopEnd;

       @addforxloop:
           // prefetch data...
           // prefetch [eax + esi];

           // max:
           {$IFDEF FPC}vmaxpd ymm3, ymm3, [eax + esi - 128];{$ELSE}db $C5,$E5,$5F,$5C,$30,$80;{$ENDIF} 
           {$IFDEF FPC}vmaxpd ymm4, ymm4, [eax + esi - 96];{$ELSE}db $C5,$DD,$5F,$64,$30,$A0;{$ENDIF} 
           {$IFDEF FPC}vmaxpd ymm3, ymm3, [eax + esi - 64];{$ELSE}db $C5,$E5,$5F,$5C,$30,$C0;{$ENDIF} 
           {$IFDEF FPC}vmaxpd ymm4, ymm4, [eax + esi - 32];{$ELSE}db $C5,$DD,$5F,$64,$30,$E0;{$ENDIF} 

       add esi, 128;
       jle @addforxloop;

       {$IFDEF FPC}vextractf128 xmm2, ymm3, 1;{$ELSE}db $C4,$E3,$7D,$19,$DA,$01;{$ENDIF} 
       {$IFDEF FPC}vmaxpd xmm2, xmm2, xmm3;{$ELSE}db $C5,$E9,$5F,$D3;{$ENDIF} 
       {$IFDEF FPC}vmaxpd xmm0, xmm0, xmm2;{$ELSE}db $C5,$F9,$5F,$C2;{$ENDIF} 
       {$IFDEF FPC}vextractf128 xmm2, ymm4, 1;{$ELSE}db $C4,$E3,$7D,$19,$E2,$01;{$ENDIF} 
       {$IFDEF FPC}vmaxpd xmm2, xmm2, xmm4;{$ELSE}db $C5,$E9,$5F,$D4;{$ENDIF} 
       {$IFDEF FPC}vmaxpd xmm0, xmm0, xmm2;{$ELSE}db $C5,$F9,$5F,$C2;{$ENDIF} 

       @loopEnd:

       sub esi, 128;

       jz @nextLine;

       @addforxloop2:
           add esi, 16;
           jg @addforxloop2end;

           {$IFDEF FPC}vmaxpd xmm0, xmm0, [eax + esi - 16];{$ELSE}db $C5,$F9,$5F,$44,$30,$F0;{$ENDIF} 
       jmp @addforxloop2;

       @addforxloop2end:

       sub esi, 16;
       jz @nextLine;

       {$IFDEF FPC}vmovsd xmm2, [eax + esi];{$ELSE}db $C5,$FB,$10,$14,$30;{$ENDIF} 
       {$IFDEF FPC}vmaxsd xmm0, xmm0, xmm2;{$ELSE}db $C5,$FB,$5F,$C2;{$ENDIF} 

       @nextLine:

       // next line:
       add eax, edi;

   // loop y end
   dec ecx;
   jnz @@addforyloop;

   // final max ->
   {$IFDEF FPC}vmovhlps xmm1, xmm1, xmm0;{$ELSE}db $C5,$F0,$12,$C8;{$ENDIF} 
   {$IFDEF FPC}vmaxsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$5F,$C1;{$ENDIF} 

   // epilog - cleanup stack
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   movsd Result, xmm0;
   
   pop esi;
   pop edi;
end;

function AVXMatrixMaxUnAligned(mt : PDouble; width, height : NativeInt; const LineWidth : NativeInt) : double;
asm
   // prolog - simulate stack
   push edi;
   push esi;

   // prepare pointers
   mov edi, LineWidth;
   imul edx, -8;

   sub eax, edx;

   // init result
   lea esi, cLocNegMaxDouble;
   {$IFDEF FPC}vbroadcastsd ymm0, [esi];{$ELSE}db $C4,$E2,$7D,$19,$06;{$ENDIF} 
   {$IFDEF FPC}vmovapd ymm3, ymm0;{$ELSE}db $C5,$FD,$29,$C3;{$ENDIF} 
   {$IFDEF FPC}vmovapd ymm4, ymm0;{$ELSE}db $C5,$FD,$29,$C4;{$ENDIF} 

   // for y := 0 to height - 1:
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov esi, edx;

       add esi, 128;
       jg @loopEnd;

       @addforxloop:
           // prefetch data...
           // prefetch [eax + esi];

           // max:
           {$IFDEF FPC}vmovupd ymm2, [eax + esi - 128];{$ELSE}db $C5,$FD,$10,$54,$30,$80;{$ENDIF} 
           {$IFDEF FPC}vmaxpd ymm3, ymm3, ymm2;{$ELSE}db $C5,$E5,$5F,$DA;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm2, [eax + esi - 96];{$ELSE}db $C5,$FD,$10,$54,$30,$A0;{$ENDIF} 
           {$IFDEF FPC}vmaxpd ymm4, ymm4, ymm2;{$ELSE}db $C5,$DD,$5F,$E2;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm2, [eax + esi - 64];{$ELSE}db $C5,$FD,$10,$54,$30,$C0;{$ENDIF} 
           {$IFDEF FPC}vmaxpd ymm3, ymm3, ymm2;{$ELSE}db $C5,$E5,$5F,$DA;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm2, [eax + esi - 32];{$ELSE}db $C5,$FD,$10,$54,$30,$E0;{$ENDIF} 
           {$IFDEF FPC}vmaxpd ymm4, ymm4, ymm2;{$ELSE}db $C5,$DD,$5F,$E2;{$ENDIF} 
       add esi, 128;
       jle @addforxloop;

       {$IFDEF FPC}vextractf128 xmm2, ymm3, 1;{$ELSE}db $C4,$E3,$7D,$19,$DA,$01;{$ENDIF} 
       {$IFDEF FPC}vmaxpd xmm2, xmm2, xmm3;{$ELSE}db $C5,$E9,$5F,$D3;{$ENDIF} 
       {$IFDEF FPC}vmaxpd xmm0, xmm0, xmm2;{$ELSE}db $C5,$F9,$5F,$C2;{$ENDIF} 
       {$IFDEF FPC}vextractf128 xmm2, ymm4, 1;{$ELSE}db $C4,$E3,$7D,$19,$E2,$01;{$ENDIF} 
       {$IFDEF FPC}vmaxpd xmm2, xmm2, xmm4;{$ELSE}db $C5,$E9,$5F,$D4;{$ENDIF} 
       {$IFDEF FPC}vmaxpd xmm0, xmm0, xmm2;{$ELSE}db $C5,$F9,$5F,$C2;{$ENDIF} 

       @loopEnd:

       sub esi, 128;

       jz @nextLine;

       @addforxloop2:
           add esi, 16;
           jg @addforxloop2end;

           {$IFDEF FPC}vmovupd xmm2, [eax + esi - 16];{$ELSE}db $C5,$F9,$10,$54,$30,$F0;{$ENDIF} 
           {$IFDEF FPC}vmaxpd xmm0, xmm0, xmm2;{$ELSE}db $C5,$F9,$5F,$C2;{$ENDIF} 
       jmp @addforxloop2;

       @addforxloop2end:

       sub esi, 16;
       jz @nextLine;

       {$IFDEF FPC}vmovsd xmm2, [eax + esi];{$ELSE}db $C5,$FB,$10,$14,$30;{$ENDIF} 
       {$IFDEF FPC}vmaxsd xmm0, xmm0, xmm2;{$ELSE}db $C5,$FB,$5F,$C2;{$ENDIF} 

       @nextLine:

       // next line:
       add eax, edi;

   // loop y end
   dec ecx;
   jnz @@addforyloop;

   // final max ->
   {$IFDEF FPC}vmovhlps xmm1, xmm1, xmm0;{$ELSE}db $C5,$F0,$12,$C8;{$ENDIF} 
   {$IFDEF FPC}vmaxsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$5F,$C1;{$ENDIF} 

   // epilog - cleanup stack
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 
   movsd Result, xmm0;
   
   pop esi;
   pop edi;
end;

function AVXMatrixMinAligned(mt : PDouble; width, height : NativeInt; const LineWidth : NativeInt) : double;
asm
   // prolog - simulate stack
   push edi;
   push esi;

   // prepare pointers
   mov edi, LineWidth;
   imul edx, -8;

   sub eax, edx;

   // init result
   lea esi, cLocMaxDouble;
   {$IFDEF FPC}vbroadcastsd ymm0, [esi];{$ELSE}db $C4,$E2,$7D,$19,$06;{$ENDIF} 
   {$IFDEF FPC}vmovapd ymm3, ymm0;{$ELSE}db $C5,$FD,$29,$C3;{$ENDIF} 
   {$IFDEF FPC}vmovapd ymm4, ymm0;{$ELSE}db $C5,$FD,$29,$C4;{$ENDIF} 

   // for y := 0 to height - 1:
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov esi, edx;
       add esi, 128;
       jg @loopEnd;

       @addforxloop:
           // prefetch data...
           // prefetch [eax + esi];

           // max:
           {$IFDEF FPC}vminpd ymm3, ymm3, [eax + esi - 128];{$ELSE}db $C5,$E5,$5D,$5C,$30,$80;{$ENDIF} 
           {$IFDEF FPC}vminpd ymm4, ymm4, [eax + esi - 96];{$ELSE}db $C5,$DD,$5D,$64,$30,$A0;{$ENDIF} 
           {$IFDEF FPC}vminpd ymm3, ymm3, [eax + esi - 64];{$ELSE}db $C5,$E5,$5D,$5C,$30,$C0;{$ENDIF} 
           {$IFDEF FPC}vminpd ymm4, ymm4, [eax + esi - 32];{$ELSE}db $C5,$DD,$5D,$64,$30,$E0;{$ENDIF} 

       add esi, 128;
       jle @addforxloop;

       {$IFDEF FPC}vextractf128 xmm2, ymm3, 1;{$ELSE}db $C4,$E3,$7D,$19,$DA,$01;{$ENDIF} 
       {$IFDEF FPC}vminpd xmm2, xmm2, xmm3;{$ELSE}db $C5,$E9,$5D,$D3;{$ENDIF} 
       {$IFDEF FPC}vminpd xmm0, xmm0, xmm2;{$ELSE}db $C5,$F9,$5D,$C2;{$ENDIF} 
       {$IFDEF FPC}vextractf128 xmm2, ymm4, 1;{$ELSE}db $C4,$E3,$7D,$19,$E2,$01;{$ENDIF} 
       {$IFDEF FPC}vminpd xmm2, xmm2, xmm4;{$ELSE}db $C5,$E9,$5D,$D4;{$ENDIF} 
       {$IFDEF FPC}vminpd xmm0, xmm0, xmm2;{$ELSE}db $C5,$F9,$5D,$C2;{$ENDIF} 

       @loopEnd:

       sub esi, 128;

       jz @nextLine;

       @addforxloop2:
           add esi, 16;
           jg @addforxloop2end;

           {$IFDEF FPC}vminpd xmm0, xmm0, [eax + esi - 16];{$ELSE}db $C5,$F9,$5D,$44,$30,$F0;{$ENDIF} 
       jmp @addforxloop2;

       @addforxloop2end:

       sub esi, 16;
       jz @nextLine;

       {$IFDEF FPC}vmovsd xmm2, [eax + esi];{$ELSE}db $C5,$FB,$10,$14,$30;{$ENDIF} 
       {$IFDEF FPC}vminsd xmm0, xmm0, xmm2;{$ELSE}db $C5,$FB,$5D,$C2;{$ENDIF} 

       @nextLine:

       // next line:
       add eax, edi;

   // loop y end
   dec ecx;
   jnz @@addforyloop;

   // final max ->
   {$IFDEF FPC}vmovhlps xmm1, xmm1, xmm0;{$ELSE}db $C5,$F0,$12,$C8;{$ENDIF} 
   {$IFDEF FPC}vminsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$5D,$C1;{$ENDIF} 

   // epilog - cleanup stack
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   movsd Result, xmm0;

   pop esi;
   pop edi;
end;

function AVXMatrixMinUnAligned(mt : PDouble; width, height : NativeInt; const LineWidth : NativeInt) : double;
asm
   // prolog - simulate stack
   push edi;
   push esi;

   // prepare pointers
   mov edi, LineWidth;
   imul edx, -8;

   sub eax, edx;

   // init result
   lea esi, cLocMaxDouble;
   {$IFDEF FPC}vbroadcastsd ymm0, [esi];{$ELSE}db $C4,$E2,$7D,$19,$06;{$ENDIF} 
   {$IFDEF FPC}vmovapd ymm3, ymm0;{$ELSE}db $C5,$FD,$29,$C3;{$ENDIF} 
   {$IFDEF FPC}vmovapd ymm4, ymm0;{$ELSE}db $C5,$FD,$29,$C4;{$ENDIF} 

   // for y := 0 to height - 1:
   mov ecx, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov esi, edx;

       add esi, 128;
       jg @loopEnd;

       @addforxloop:
           // prefetch data...
           // prefetch [eax + esi];

           // max:
           {$IFDEF FPC}vmovupd ymm2, [eax + esi - 128];{$ELSE}db $C5,$FD,$10,$54,$30,$80;{$ENDIF} 
           {$IFDEF FPC}vminpd ymm3, ymm3, ymm2;{$ELSE}db $C5,$E5,$5D,$DA;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm2, [eax + esi - 96];{$ELSE}db $C5,$FD,$10,$54,$30,$A0;{$ENDIF} 
           {$IFDEF FPC}vminpd ymm4, ymm4, ymm2;{$ELSE}db $C5,$DD,$5D,$E2;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm2, [eax + esi - 64];{$ELSE}db $C5,$FD,$10,$54,$30,$C0;{$ENDIF} 
           {$IFDEF FPC}vminpd ymm3, ymm3, ymm2;{$ELSE}db $C5,$E5,$5D,$DA;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm2, [eax + esi - 32];{$ELSE}db $C5,$FD,$10,$54,$30,$E0;{$ENDIF} 
           {$IFDEF FPC}vminpd ymm4, ymm4, ymm2;{$ELSE}db $C5,$DD,$5D,$E2;{$ENDIF} 
       add esi, 128;
       jle @addforxloop;

       {$IFDEF FPC}vextractf128 xmm2, ymm3, 1;{$ELSE}db $C4,$E3,$7D,$19,$DA,$01;{$ENDIF} 
       {$IFDEF FPC}vminpd xmm2, xmm2, xmm3;{$ELSE}db $C5,$E9,$5D,$D3;{$ENDIF} 
       {$IFDEF FPC}vminpd xmm0, xmm0, xmm2;{$ELSE}db $C5,$F9,$5D,$C2;{$ENDIF} 
       {$IFDEF FPC}vextractf128 xmm2, ymm4, 1;{$ELSE}db $C4,$E3,$7D,$19,$E2,$01;{$ENDIF} 
       {$IFDEF FPC}vminpd xmm2, xmm2, xmm4;{$ELSE}db $C5,$E9,$5D,$D4;{$ENDIF} 
       {$IFDEF FPC}vminpd xmm0, xmm0, xmm2;{$ELSE}db $C5,$F9,$5D,$C2;{$ENDIF} 

       @loopEnd:

       sub esi, 128;

       jz @nextLine;

       @addforxloop2:
           add esi, 16;
           jg @addforxloop2end;

           {$IFDEF FPC}vmovupd xmm2, [eax + esi - 16];{$ELSE}db $C5,$F9,$10,$54,$30,$F0;{$ENDIF} 
           {$IFDEF FPC}vminpd xmm0, xmm0, xmm2;{$ELSE}db $C5,$F9,$5D,$C2;{$ENDIF} 
       jmp @addforxloop2;

       @addforxloop2end:

       sub esi, 16;
       jz @nextLine;

       {$IFDEF FPC}vmovsd xmm2, [eax + esi];{$ELSE}db $C5,$FB,$10,$14,$30;{$ENDIF} 
       {$IFDEF FPC}vminsd xmm0, xmm0, xmm2;{$ELSE}db $C5,$FB,$5D,$C2;{$ENDIF} 

       @nextLine:

       // next line:
       add eax, edi;

   // loop y end
   dec ecx;
   jnz @@addforyloop;

   // final max ->
   {$IFDEF FPC}vmovhlps xmm1, xmm1, xmm0;{$ELSE}db $C5,$F0,$12,$C8;{$ENDIF} 
   {$IFDEF FPC}vminsd xmm0, xmm0, xmm1;{$ELSE}db $C5,$FB,$5D,$C1;{$ENDIF} 

   // epilog - cleanup stack
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 
   movsd Result, xmm0;

   pop esi;
   pop edi;
end;

{$ENDIF}

end.
