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

// in matrix max/min
procedure AVXMatrixMinValUnAligned( dest : PDouble; const LineWidth : NativeInt; width, height : NativeInt; minVal : double ); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure AVXMatrixMaxValUnAligned( dest : PDouble; const LineWidth : NativeInt; width, height : NativeInt; minVal : double ); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

procedure AVXMatrixMinValAligned( dest : PDouble; const LineWidth : NativeInt; width, height : NativeInt; minVal : double ); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure AVXMatrixMaxValAligned( dest : PDouble; const LineWidth : NativeInt; width, height : NativeInt; minVal : double ); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

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
   {$IFDEF AVXSUP}vbroadcastsd ymm0, [esi];                           {$ELSE}db $C4,$E2,$7D,$19,$06;{$ENDIF} 
   {$IFDEF AVXSUP}vmovapd ymm3, ymm0;                                 {$ELSE}db $C5,$FD,$29,$C3;{$ENDIF} 
   {$IFDEF AVXSUP}vmovapd ymm4, ymm0;                                 {$ELSE}db $C5,$FD,$29,$C4;{$ENDIF} 

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
           {$IFDEF AVXSUP}vmaxpd ymm3, ymm3, [eax + esi - 128];       {$ELSE}db $C5,$E5,$5F,$5C,$30,$80;{$ENDIF} 
           {$IFDEF AVXSUP}vmaxpd ymm4, ymm4, [eax + esi - 96];        {$ELSE}db $C5,$DD,$5F,$64,$30,$A0;{$ENDIF} 
           {$IFDEF AVXSUP}vmaxpd ymm3, ymm3, [eax + esi - 64];        {$ELSE}db $C5,$E5,$5F,$5C,$30,$C0;{$ENDIF} 
           {$IFDEF AVXSUP}vmaxpd ymm4, ymm4, [eax + esi - 32];        {$ELSE}db $C5,$DD,$5F,$64,$30,$E0;{$ENDIF} 

       add esi, 128;
       jle @addforxloop;

       {$IFDEF AVXSUP}vextractf128 xmm2, ymm3, 1;                     {$ELSE}db $C4,$E3,$7D,$19,$DA,$01;{$ENDIF} 
       {$IFDEF AVXSUP}vmaxpd xmm2, xmm2, xmm3;                        {$ELSE}db $C5,$E9,$5F,$D3;{$ENDIF} 
       {$IFDEF AVXSUP}vmaxpd xmm0, xmm0, xmm2;                        {$ELSE}db $C5,$F9,$5F,$C2;{$ENDIF} 
       {$IFDEF AVXSUP}vextractf128 xmm2, ymm4, 1;                     {$ELSE}db $C4,$E3,$7D,$19,$E2,$01;{$ENDIF} 
       {$IFDEF AVXSUP}vmaxpd xmm2, xmm2, xmm4;                        {$ELSE}db $C5,$E9,$5F,$D4;{$ENDIF} 
       {$IFDEF AVXSUP}vmaxpd xmm0, xmm0, xmm2;                        {$ELSE}db $C5,$F9,$5F,$C2;{$ENDIF} 

       @loopEnd:

       sub esi, 128;

       jz @nextLine;

       @addforxloop2:
           add esi, 16;
           jg @addforxloop2end;

           {$IFDEF AVXSUP}vmaxpd xmm0, xmm0, [eax + esi - 16];        {$ELSE}db $C5,$F9,$5F,$44,$30,$F0;{$ENDIF} 
       jmp @addforxloop2;

       @addforxloop2end:

       sub esi, 16;
       jz @nextLine;

       {$IFDEF AVXSUP}vmovsd xmm2, [eax + esi];                       {$ELSE}db $C5,$FB,$10,$14,$30;{$ENDIF} 
       {$IFDEF AVXSUP}vmaxsd xmm0, xmm0, xmm2;                        {$ELSE}db $C5,$FB,$5F,$C2;{$ENDIF} 

       @nextLine:

       // next line:
       add eax, edi;

   // loop y end
   dec ecx;
   jnz @@addforyloop;

   // final max ->
   {$IFDEF AVXSUP}vmovhlps xmm1, xmm1, xmm0;                          {$ELSE}db $C5,$F0,$12,$C8;{$ENDIF} 
   {$IFDEF AVXSUP}vmaxsd xmm0, xmm0, xmm1;                            {$ELSE}db $C5,$FB,$5F,$C1;{$ENDIF} 

   // epilog - cleanup stack
   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 

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
   {$IFDEF AVXSUP}vbroadcastsd ymm0, [esi];                           {$ELSE}db $C4,$E2,$7D,$19,$06;{$ENDIF} 
   {$IFDEF AVXSUP}vmovapd ymm3, ymm0;                                 {$ELSE}db $C5,$FD,$29,$C3;{$ENDIF} 
   {$IFDEF AVXSUP}vmovapd ymm4, ymm0;                                 {$ELSE}db $C5,$FD,$29,$C4;{$ENDIF} 

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
           {$IFDEF AVXSUP}vmovupd ymm2, [eax + esi - 128];            {$ELSE}db $C5,$FD,$10,$54,$30,$80;{$ENDIF} 
           {$IFDEF AVXSUP}vmaxpd ymm3, ymm3, ymm2;                    {$ELSE}db $C5,$E5,$5F,$DA;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd ymm2, [eax + esi - 96];             {$ELSE}db $C5,$FD,$10,$54,$30,$A0;{$ENDIF} 
           {$IFDEF AVXSUP}vmaxpd ymm4, ymm4, ymm2;                    {$ELSE}db $C5,$DD,$5F,$E2;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd ymm2, [eax + esi - 64];             {$ELSE}db $C5,$FD,$10,$54,$30,$C0;{$ENDIF} 
           {$IFDEF AVXSUP}vmaxpd ymm3, ymm3, ymm2;                    {$ELSE}db $C5,$E5,$5F,$DA;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd ymm2, [eax + esi - 32];             {$ELSE}db $C5,$FD,$10,$54,$30,$E0;{$ENDIF} 
           {$IFDEF AVXSUP}vmaxpd ymm4, ymm4, ymm2;                    {$ELSE}db $C5,$DD,$5F,$E2;{$ENDIF} 
       add esi, 128;
       jle @addforxloop;

       {$IFDEF AVXSUP}vextractf128 xmm2, ymm3, 1;                     {$ELSE}db $C4,$E3,$7D,$19,$DA,$01;{$ENDIF} 
       {$IFDEF AVXSUP}vmaxpd xmm2, xmm2, xmm3;                        {$ELSE}db $C5,$E9,$5F,$D3;{$ENDIF} 
       {$IFDEF AVXSUP}vmaxpd xmm0, xmm0, xmm2;                        {$ELSE}db $C5,$F9,$5F,$C2;{$ENDIF} 
       {$IFDEF AVXSUP}vextractf128 xmm2, ymm4, 1;                     {$ELSE}db $C4,$E3,$7D,$19,$E2,$01;{$ENDIF} 
       {$IFDEF AVXSUP}vmaxpd xmm2, xmm2, xmm4;                        {$ELSE}db $C5,$E9,$5F,$D4;{$ENDIF} 
       {$IFDEF AVXSUP}vmaxpd xmm0, xmm0, xmm2;                        {$ELSE}db $C5,$F9,$5F,$C2;{$ENDIF} 

       @loopEnd:

       sub esi, 128;

       jz @nextLine;

       @addforxloop2:
           add esi, 16;
           jg @addforxloop2end;

           {$IFDEF AVXSUP}vmovupd xmm2, [eax + esi - 16];             {$ELSE}db $C5,$F9,$10,$54,$30,$F0;{$ENDIF} 
           {$IFDEF AVXSUP}vmaxpd xmm0, xmm0, xmm2;                    {$ELSE}db $C5,$F9,$5F,$C2;{$ENDIF} 
       jmp @addforxloop2;

       @addforxloop2end:

       sub esi, 16;
       jz @nextLine;

       {$IFDEF AVXSUP}vmovsd xmm2, [eax + esi];                       {$ELSE}db $C5,$FB,$10,$14,$30;{$ENDIF} 
       {$IFDEF AVXSUP}vmaxsd xmm0, xmm0, xmm2;                        {$ELSE}db $C5,$FB,$5F,$C2;{$ENDIF} 

       @nextLine:

       // next line:
       add eax, edi;

   // loop y end
   dec ecx;
   jnz @@addforyloop;

   // final max ->
   {$IFDEF AVXSUP}vmovhlps xmm1, xmm1, xmm0;                          {$ELSE}db $C5,$F0,$12,$C8;{$ENDIF} 
   {$IFDEF AVXSUP}vmaxsd xmm0, xmm0, xmm1;                            {$ELSE}db $C5,$FB,$5F,$C1;{$ENDIF} 

   // epilog - cleanup stack
   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 
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
   {$IFDEF AVXSUP}vbroadcastsd ymm0, [esi];                           {$ELSE}db $C4,$E2,$7D,$19,$06;{$ENDIF} 
   {$IFDEF AVXSUP}vmovapd ymm3, ymm0;                                 {$ELSE}db $C5,$FD,$29,$C3;{$ENDIF} 
   {$IFDEF AVXSUP}vmovapd ymm4, ymm0;                                 {$ELSE}db $C5,$FD,$29,$C4;{$ENDIF} 

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
           {$IFDEF AVXSUP}vminpd ymm3, ymm3, [eax + esi - 128];       {$ELSE}db $C5,$E5,$5D,$5C,$30,$80;{$ENDIF} 
           {$IFDEF AVXSUP}vminpd ymm4, ymm4, [eax + esi - 96];        {$ELSE}db $C5,$DD,$5D,$64,$30,$A0;{$ENDIF} 
           {$IFDEF AVXSUP}vminpd ymm3, ymm3, [eax + esi - 64];        {$ELSE}db $C5,$E5,$5D,$5C,$30,$C0;{$ENDIF} 
           {$IFDEF AVXSUP}vminpd ymm4, ymm4, [eax + esi - 32];        {$ELSE}db $C5,$DD,$5D,$64,$30,$E0;{$ENDIF} 

       add esi, 128;
       jle @addforxloop;

       {$IFDEF AVXSUP}vextractf128 xmm2, ymm3, 1;                     {$ELSE}db $C4,$E3,$7D,$19,$DA,$01;{$ENDIF} 
       {$IFDEF AVXSUP}vminpd xmm2, xmm2, xmm3;                        {$ELSE}db $C5,$E9,$5D,$D3;{$ENDIF} 
       {$IFDEF AVXSUP}vminpd xmm0, xmm0, xmm2;                        {$ELSE}db $C5,$F9,$5D,$C2;{$ENDIF} 
       {$IFDEF AVXSUP}vextractf128 xmm2, ymm4, 1;                     {$ELSE}db $C4,$E3,$7D,$19,$E2,$01;{$ENDIF} 
       {$IFDEF AVXSUP}vminpd xmm2, xmm2, xmm4;                        {$ELSE}db $C5,$E9,$5D,$D4;{$ENDIF} 
       {$IFDEF AVXSUP}vminpd xmm0, xmm0, xmm2;                        {$ELSE}db $C5,$F9,$5D,$C2;{$ENDIF} 

       @loopEnd:

       sub esi, 128;

       jz @nextLine;

       @addforxloop2:
           add esi, 16;
           jg @addforxloop2end;

           {$IFDEF AVXSUP}vminpd xmm0, xmm0, [eax + esi - 16];        {$ELSE}db $C5,$F9,$5D,$44,$30,$F0;{$ENDIF} 
       jmp @addforxloop2;

       @addforxloop2end:

       sub esi, 16;
       jz @nextLine;

       {$IFDEF AVXSUP}vmovsd xmm2, [eax + esi];                       {$ELSE}db $C5,$FB,$10,$14,$30;{$ENDIF} 
       {$IFDEF AVXSUP}vminsd xmm0, xmm0, xmm2;                        {$ELSE}db $C5,$FB,$5D,$C2;{$ENDIF} 

       @nextLine:

       // next line:
       add eax, edi;

   // loop y end
   dec ecx;
   jnz @@addforyloop;

   // final max ->
   {$IFDEF AVXSUP}vmovhlps xmm1, xmm1, xmm0;                          {$ELSE}db $C5,$F0,$12,$C8;{$ENDIF} 
   {$IFDEF AVXSUP}vminsd xmm0, xmm0, xmm1;                            {$ELSE}db $C5,$FB,$5D,$C1;{$ENDIF} 

   // epilog - cleanup stack
   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 

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
   {$IFDEF AVXSUP}vbroadcastsd ymm0, [esi];                           {$ELSE}db $C4,$E2,$7D,$19,$06;{$ENDIF} 
   {$IFDEF AVXSUP}vmovapd ymm3, ymm0;                                 {$ELSE}db $C5,$FD,$29,$C3;{$ENDIF} 
   {$IFDEF AVXSUP}vmovapd ymm4, ymm0;                                 {$ELSE}db $C5,$FD,$29,$C4;{$ENDIF} 

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
           {$IFDEF AVXSUP}vmovupd ymm2, [eax + esi - 128];            {$ELSE}db $C5,$FD,$10,$54,$30,$80;{$ENDIF} 
           {$IFDEF AVXSUP}vminpd ymm3, ymm3, ymm2;                    {$ELSE}db $C5,$E5,$5D,$DA;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd ymm2, [eax + esi - 96];             {$ELSE}db $C5,$FD,$10,$54,$30,$A0;{$ENDIF} 
           {$IFDEF AVXSUP}vminpd ymm4, ymm4, ymm2;                    {$ELSE}db $C5,$DD,$5D,$E2;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd ymm2, [eax + esi - 64];             {$ELSE}db $C5,$FD,$10,$54,$30,$C0;{$ENDIF} 
           {$IFDEF AVXSUP}vminpd ymm3, ymm3, ymm2;                    {$ELSE}db $C5,$E5,$5D,$DA;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd ymm2, [eax + esi - 32];             {$ELSE}db $C5,$FD,$10,$54,$30,$E0;{$ENDIF} 
           {$IFDEF AVXSUP}vminpd ymm4, ymm4, ymm2;                    {$ELSE}db $C5,$DD,$5D,$E2;{$ENDIF} 
       add esi, 128;
       jle @addforxloop;

       {$IFDEF AVXSUP}vextractf128 xmm2, ymm3, 1;                     {$ELSE}db $C4,$E3,$7D,$19,$DA,$01;{$ENDIF} 
       {$IFDEF AVXSUP}vminpd xmm2, xmm2, xmm3;                        {$ELSE}db $C5,$E9,$5D,$D3;{$ENDIF} 
       {$IFDEF AVXSUP}vminpd xmm0, xmm0, xmm2;                        {$ELSE}db $C5,$F9,$5D,$C2;{$ENDIF} 
       {$IFDEF AVXSUP}vextractf128 xmm2, ymm4, 1;                     {$ELSE}db $C4,$E3,$7D,$19,$E2,$01;{$ENDIF} 
       {$IFDEF AVXSUP}vminpd xmm2, xmm2, xmm4;                        {$ELSE}db $C5,$E9,$5D,$D4;{$ENDIF} 
       {$IFDEF AVXSUP}vminpd xmm0, xmm0, xmm2;                        {$ELSE}db $C5,$F9,$5D,$C2;{$ENDIF} 

       @loopEnd:

       sub esi, 128;

       jz @nextLine;

       @addforxloop2:
           add esi, 16;
           jg @addforxloop2end;

           {$IFDEF AVXSUP}vmovupd xmm2, [eax + esi - 16];             {$ELSE}db $C5,$F9,$10,$54,$30,$F0;{$ENDIF} 
           {$IFDEF AVXSUP}vminpd xmm0, xmm0, xmm2;                    {$ELSE}db $C5,$F9,$5D,$C2;{$ENDIF} 
       jmp @addforxloop2;

       @addforxloop2end:

       sub esi, 16;
       jz @nextLine;

       {$IFDEF AVXSUP}vmovsd xmm2, [eax + esi];                       {$ELSE}db $C5,$FB,$10,$14,$30;{$ENDIF} 
       {$IFDEF AVXSUP}vminsd xmm0, xmm0, xmm2;                        {$ELSE}db $C5,$FB,$5D,$C2;{$ENDIF} 

       @nextLine:

       // next line:
       add eax, edi;

   // loop y end
   dec ecx;
   jnz @@addforyloop;

   // final max ->
   {$IFDEF AVXSUP}vmovhlps xmm1, xmm1, xmm0;                          {$ELSE}db $C5,$F0,$12,$C8;{$ENDIF} 
   {$IFDEF AVXSUP}vminsd xmm0, xmm0, xmm1;                            {$ELSE}db $C5,$FB,$5D,$C1;{$ENDIF} 

   // epilog - cleanup stack
   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 
   movsd Result, xmm0;

   pop esi;
   pop edi;
end;

procedure AVXMatrixMinValUnAligned( dest : PDouble; const LineWidth : NativeInt; width, height : NativeInt; minVal : double ); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
// eax = dest; edx = LineWidth, ecx = Width
asm
   push edi;
   push ebx;

   imul ecx, -8;

   // helper registers for the dest pointer
   sub eax, ecx;

   lea edi, minVal;
   {$IFDEF AVXSUP}vbroadcastsd ymm0, [edi];                           {$ELSE}db $C4,$E2,$7D,$19,$07;{$ENDIF} 

   // for y := 0 to height - 1:
   mov ebx, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, ecx;
       @addforxloop:
           add edi, 128;
           jg @loopEnd;

           // prefetch data...
           //prefetchw [eax + rax];

           // Abs:
           {$IFDEF AVXSUP}vmovupd ymm1, [eax + edi - 128];            {$ELSE}db $C5,$FD,$10,$4C,$38,$80;{$ENDIF} 
           {$IFDEF AVXSUP}vminpd ymm1, ymm1, ymm0;                    {$ELSE}db $C5,$F5,$5D,$C8;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd [eax + edi - 128], ymm1;            {$ELSE}db $C5,$FD,$11,$4C,$38,$80;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd ymm2, [eax + edi - 96];             {$ELSE}db $C5,$FD,$10,$54,$38,$A0;{$ENDIF} 
           {$IFDEF AVXSUP}vminpd ymm2, ymm2, ymm0;                    {$ELSE}db $C5,$ED,$5D,$D0;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd [eax + edi - 96], ymm2;             {$ELSE}db $C5,$FD,$11,$54,$38,$A0;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd ymm3, [eax + edi - 64];             {$ELSE}db $C5,$FD,$10,$5C,$38,$C0;{$ENDIF} 
           {$IFDEF AVXSUP}vminpd ymm3, ymm3, ymm0;                    {$ELSE}db $C5,$E5,$5D,$D8;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd [eax + edi - 64], ymm3;             {$ELSE}db $C5,$FD,$11,$5C,$38,$C0;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd ymm4, [eax + edi - 32];             {$ELSE}db $C5,$FD,$10,$64,$38,$E0;{$ENDIF} 
           {$IFDEF AVXSUP}vminpd ymm4, ymm4, ymm0;                    {$ELSE}db $C5,$DD,$5D,$E0;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd [eax + edi - 32], ymm4;             {$ELSE}db $C5,$FD,$11,$64,$38,$E0;{$ENDIF} 
       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @nextLine;

       @addforxloop2:
           add edi, 16;
           jg @loopEnd2;

           {$IFDEF AVXSUP}vmovupd xmm1, [eax + edi - 16];             {$ELSE}db $C5,$F9,$10,$4C,$38,$F0;{$ENDIF} 
           {$IFDEF AVXSUP}vminpd xmm1, xmm1, xmm0;                    {$ELSE}db $C5,$F1,$5D,$C8;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd [eax + edi - 16], xmm1;             {$ELSE}db $C5,$F9,$11,$4C,$38,$F0;{$ENDIF} 
       jmp @addforxloop2;

       @loopEnd2:

       sub edi, 16;
       jz @nextLine;

       {$IFDEF AVXSUP}vmovsd xmm1, [eax + edi];                       {$ELSE}db $C5,$FB,$10,$0C,$38;{$ENDIF} 
       {$IFDEF AVXSUP}vminsd xmm1, xmm1, xmm0;                        {$ELSE}db $C5,$F3,$5D,$C8;{$ENDIF} 
       {$IFDEF AVXSUP}vmovsd [eax + edi], xmm1;                       {$ELSE}db $C5,$FB,$11,$0C,$38;{$ENDIF} 

       @nextLine:

       // next line:
       add eax, edx;

   // loop y end
   dec ebx;
   jnz @@addforyloop;

   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop ebx;
   pop edi;
end;


procedure AVXMatrixMaxValUnAligned( dest : PDouble; const LineWidth : NativeInt; width, height : NativeInt; minVal : double ); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
// eax = dest; edx = LineWidth, ecx = Width
asm
   push edi;
   push ebx;

   imul ecx, -8;

   // helper registers for the dest pointer
   sub eax, ecx;

   lea edi, minVal;
   {$IFDEF AVXSUP}vbroadcastsd ymm0, [edi];                           {$ELSE}db $C4,$E2,$7D,$19,$07;{$ENDIF} 

   // for y := 0 to height - 1:
   mov ebx, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, ecx;
       @addforxloop:
           add edi, 128;
           jg @loopEnd;

           // prefetch data...
           //prefetchw [eax + rax];

           // Abs:
           {$IFDEF AVXSUP}vmovupd ymm1, [eax + edi - 128];            {$ELSE}db $C5,$FD,$10,$4C,$38,$80;{$ENDIF} 
           {$IFDEF AVXSUP}vmaxpd ymm1, ymm1, ymm0;                    {$ELSE}db $C5,$F5,$5F,$C8;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd [eax + edi - 128], ymm1;            {$ELSE}db $C5,$FD,$11,$4C,$38,$80;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd ymm2, [eax + edi - 96];             {$ELSE}db $C5,$FD,$10,$54,$38,$A0;{$ENDIF} 
           {$IFDEF AVXSUP}vmaxpd ymm2, ymm2, ymm0;                    {$ELSE}db $C5,$ED,$5F,$D0;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd [eax + edi - 96], ymm2;             {$ELSE}db $C5,$FD,$11,$54,$38,$A0;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd ymm3, [eax + edi - 64];             {$ELSE}db $C5,$FD,$10,$5C,$38,$C0;{$ENDIF} 
           {$IFDEF AVXSUP}vmaxpd ymm3, ymm3, ymm0;                    {$ELSE}db $C5,$E5,$5F,$D8;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd [eax + edi - 64], ymm3;             {$ELSE}db $C5,$FD,$11,$5C,$38,$C0;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd ymm4, [eax + edi - 32];             {$ELSE}db $C5,$FD,$10,$64,$38,$E0;{$ENDIF} 
           {$IFDEF AVXSUP}vmaxpd ymm4, ymm4, ymm0;                    {$ELSE}db $C5,$DD,$5F,$E0;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd [eax + edi - 32], ymm4;             {$ELSE}db $C5,$FD,$11,$64,$38,$E0;{$ENDIF} 
       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @nextLine;

       @addforxloop2:
           add edi, 16;
           jg @loopEnd2;

           {$IFDEF AVXSUP}vmovupd xmm1, [eax + edi - 16];             {$ELSE}db $C5,$F9,$10,$4C,$38,$F0;{$ENDIF} 
           {$IFDEF AVXSUP}vmaxpd xmm1, xmm1, xmm0;                    {$ELSE}db $C5,$F1,$5F,$C8;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd [eax + edi - 16], xmm1;             {$ELSE}db $C5,$F9,$11,$4C,$38,$F0;{$ENDIF} 
       jmp @addforxloop2;

       @loopEnd2:

       sub edi, 16;
       jz @nextLine;

       {$IFDEF AVXSUP}vmovsd xmm1, [eax + edi];                       {$ELSE}db $C5,$FB,$10,$0C,$38;{$ENDIF} 
       {$IFDEF AVXSUP}vmaxsd xmm1, xmm1, xmm0;                        {$ELSE}db $C5,$F3,$5F,$C8;{$ENDIF} 
       {$IFDEF AVXSUP}vmovsd [eax + edi], xmm1;                       {$ELSE}db $C5,$FB,$11,$0C,$38;{$ENDIF} 

       @nextLine:

       // next line:
       add eax, edx;

   // loop y end
   dec ebx;
   jnz @@addforyloop;

   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop ebx;
   pop edi;
end;


procedure AVXMatrixMinValAligned( dest : PDouble; const LineWidth : NativeInt; width, height : NativeInt; minVal : double ); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
// eax = dest; edx = LineWidth, ecx = Width
asm
   push edi;
   push ebx;

   imul ecx, -8;

   // helper registers for the dest pointer
   sub eax, ecx;

   lea edi, minVal;
   {$IFDEF AVXSUP}vbroadcastsd ymm0, [edi];                           {$ELSE}db $C4,$E2,$7D,$19,$07;{$ENDIF} 

   // for y := 0 to height - 1:
   mov ebx, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, ecx;
       @addforxloop:
           add edi, 128;
           jg @loopEnd;

           // prefetch data...
           //prefetchw [eax + rax];

           // Abs:
           {$IFDEF AVXSUP}vminpd ymm1, ymm0, [eax + edi - 128];       {$ELSE}db $C5,$FD,$5D,$4C,$38,$80;{$ENDIF} 
           {$IFDEF AVXSUP}vmovapd [eax + edi - 128], ymm1;            {$ELSE}db $C5,$FD,$29,$4C,$38,$80;{$ENDIF} 

           {$IFDEF AVXSUP}vminpd ymm2, ymm0, [eax + edi - 96];        {$ELSE}db $C5,$FD,$5D,$54,$38,$A0;{$ENDIF} 
           {$IFDEF AVXSUP}vmovapd [eax + edi - 96], ymm2;             {$ELSE}db $C5,$FD,$29,$54,$38,$A0;{$ENDIF} 

           {$IFDEF AVXSUP}vminpd ymm3, ymm0, [eax + edi - 64];        {$ELSE}db $C5,$FD,$5D,$5C,$38,$C0;{$ENDIF} 
           {$IFDEF AVXSUP}vmovapd [eax + edi - 64], ymm3;             {$ELSE}db $C5,$FD,$29,$5C,$38,$C0;{$ENDIF} 

           {$IFDEF AVXSUP}vminpd ymm4, ymm0, [eax + edi - 32];        {$ELSE}db $C5,$FD,$5D,$64,$38,$E0;{$ENDIF} 
           {$IFDEF AVXSUP}vmovapd [eax + edi - 32], ymm4;             {$ELSE}db $C5,$FD,$29,$64,$38,$E0;{$ENDIF} 
       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @nextLine;

       @addforxloop2:
           add edi, 16;
           jg @loopEnd2;

           {$IFDEF AVXSUP}vminpd xmm1, xmm0, [eax + edi - 16];        {$ELSE}db $C5,$F9,$5D,$4C,$38,$F0;{$ENDIF} 
           {$IFDEF AVXSUP}vmovapd [eax + edi - 16], xmm1;             {$ELSE}db $C5,$F9,$29,$4C,$38,$F0;{$ENDIF} 
       jmp @addforxloop2;

       @loopEnd2:

       sub edi, 16;
       jz @nextLine;

       {$IFDEF AVXSUP}vmovsd xmm1, [eax + edi];                       {$ELSE}db $C5,$FB,$10,$0C,$38;{$ENDIF} 
       {$IFDEF AVXSUP}vminsd xmm1, xmm1, xmm0;                        {$ELSE}db $C5,$F3,$5D,$C8;{$ENDIF} 
       {$IFDEF AVXSUP}vmovsd [eax + edi], xmm1;                       {$ELSE}db $C5,$FB,$11,$0C,$38;{$ENDIF} 

       @nextLine:

       // next line:
       add eax, edx;

   // loop y end
   dec ebx;
   jnz @@addforyloop;

   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop ebx;
   pop edi;
end;

procedure AVXMatrixMaxValAligned( dest : PDouble; const LineWidth : NativeInt; width, height : NativeInt; minVal : double ); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
// eax = dest; edx = LineWidth, ecx = Width
asm
   push edi;
   push ebx;

   imul ecx, -8;

   // helper registers for the dest pointer
   sub eax, ecx;

   lea edi, minVal;
   {$IFDEF AVXSUP}vbroadcastsd ymm0, [edi];                           {$ELSE}db $C4,$E2,$7D,$19,$07;{$ENDIF} 

   // for y := 0 to height - 1:
   mov ebx, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, ecx;
       @addforxloop:
           add edi, 128;
           jg @loopEnd;

           // prefetch data...
           //prefetchw [eax + rax];

           // Abs:
           {$IFDEF AVXSUP}vmaxpd ymm1, ymm0, [eax + edi - 128];       {$ELSE}db $C5,$FD,$5F,$4C,$38,$80;{$ENDIF} 
           {$IFDEF AVXSUP}vmovapd [eax + edi - 128], ymm1;            {$ELSE}db $C5,$FD,$29,$4C,$38,$80;{$ENDIF} 

           {$IFDEF AVXSUP}vmaxpd ymm2, ymm0, [eax + edi - 96];        {$ELSE}db $C5,$FD,$5F,$54,$38,$A0;{$ENDIF} 
           {$IFDEF AVXSUP}vmovapd [eax + edi - 96], ymm2;             {$ELSE}db $C5,$FD,$29,$54,$38,$A0;{$ENDIF} 

           {$IFDEF AVXSUP}vmaxpd ymm3, ymm0, [eax + edi - 64];        {$ELSE}db $C5,$FD,$5F,$5C,$38,$C0;{$ENDIF} 
           {$IFDEF AVXSUP}vmovapd [eax + edi - 64], ymm3;             {$ELSE}db $C5,$FD,$29,$5C,$38,$C0;{$ENDIF} 

           {$IFDEF AVXSUP}vmaxpd ymm4, ymm0, [eax + edi - 32];        {$ELSE}db $C5,$FD,$5F,$64,$38,$E0;{$ENDIF} 
           {$IFDEF AVXSUP}vmovapd [eax + edi - 32], ymm4;             {$ELSE}db $C5,$FD,$29,$64,$38,$E0;{$ENDIF} 
       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @nextLine;

       @addforxloop2:
           add edi, 16;
           jg @loopEnd2;

           {$IFDEF AVXSUP}vmaxpd xmm1, xmm0, [eax + edi - 16];        {$ELSE}db $C5,$F9,$5F,$4C,$38,$F0;{$ENDIF} 
           {$IFDEF AVXSUP}vmovapd [eax + edi - 16], xmm1;             {$ELSE}db $C5,$F9,$29,$4C,$38,$F0;{$ENDIF} 
       jmp @addforxloop2;

       @loopEnd2:

       sub edi, 16;
       jz @nextLine;

       {$IFDEF AVXSUP}vmovsd xmm1, [eax + edi];                       {$ELSE}db $C5,$FB,$10,$0C,$38;{$ENDIF} 
       {$IFDEF AVXSUP}vmaxsd xmm1, xmm1, xmm0;                        {$ELSE}db $C5,$F3,$5F,$C8;{$ENDIF} 
       {$IFDEF AVXSUP}vmovsd [eax + edi], xmm1;                       {$ELSE}db $C5,$FB,$11,$0C,$38;{$ENDIF} 

       @nextLine:

       // next line:
       add eax, edx;

   // loop y end
   dec ebx;
   jnz @@addforyloop;

   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop ebx;
   pop edi;
end;

{$ENDIF}

end.
