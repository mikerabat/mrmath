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


unit AVXMatrixElementwiseMultOperations;

interface

{$I 'mrMath_CPU.inc'}

{$IFNDEF x64}

procedure AVXMatrixElemMultAligned(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width : NativeInt; height : NativeInt; const LineWidth1, LineWidth2 : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure AVXMatrixElemMultUnAligned(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width : NativeInt; height : NativeInt; const LineWidth1, LineWidth2 : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

procedure AVXMatrixElemDivAligned(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width : NativeInt; height : NativeInt; const LineWidth1, LineWidth2 : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure AVXMatrixElemDivUnAligned(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width : NativeInt; height : NativeInt; const LineWidth1, LineWidth2 : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

{$ENDIF}

implementation

{$IFNDEF x64}

procedure AVXMatrixElemMultAligned(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width : NativeInt; height : NativeInt; const LineWidth1, LineWidth2 : NativeInt);
asm
   // prolog - simulate stack
   push ebx;
   push edi;
   push esi;

   //iters := -width*sizeof(double);
   mov edi, width;
   imul edi, -8;

   // helper registers for the mt1, mt2 and dest pointers
   mov esi, mt2;

   sub eax, edi;
   sub ecx, edi;
   sub esi, edi;

   // for y := 0 to height - 1:
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov ebx, edi;
       @addforxloop:
           add ebx, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [r8 + ebx];
           // prefetch [r9 + ebx];

           // mult:
           {$IFDEF AVXSUP}vmovapd ymm0, [ecx + ebx - 128];            {$ELSE}db $C5,$FD,$28,$44,$19,$80;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm0, ymm0, [esi + ebx - 128];       {$ELSE}db $C5,$FD,$59,$44,$1E,$80;{$ENDIF} 

           {$IFDEF AVXSUP}vmovapd [eax + ebx - 128], ymm0;            {$ELSE}db $C5,$FD,$29,$44,$18,$80;{$ENDIF} 

           {$IFDEF AVXSUP}vmovapd ymm1, [ecx + ebx - 96];             {$ELSE}db $C5,$FD,$28,$4C,$19,$A0;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm1, ymm1, [esi + ebx - 96];        {$ELSE}db $C5,$F5,$59,$4C,$1E,$A0;{$ENDIF} 

           {$IFDEF AVXSUP}vmovapd [eax + ebx - 96], ymm1;             {$ELSE}db $C5,$FD,$29,$4C,$18,$A0;{$ENDIF} 

           {$IFDEF AVXSUP}vmovapd ymm2, [ecx + ebx - 64];             {$ELSE}db $C5,$FD,$28,$54,$19,$C0;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm2, ymm2, [esi + ebx - 64];        {$ELSE}db $C5,$ED,$59,$54,$1E,$C0;{$ENDIF} 

           {$IFDEF AVXSUP}vmovapd [eax + ebx - 64], ymm2;             {$ELSE}db $C5,$FD,$29,$54,$18,$C0;{$ENDIF} 

           {$IFDEF AVXSUP}vmovapd ymm3, [ecx + ebx - 32];             {$ELSE}db $C5,$FD,$28,$5C,$19,$E0;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm3, ymm3, [esi + ebx - 32];        {$ELSE}db $C5,$E5,$59,$5C,$1E,$E0;{$ENDIF} 

           {$IFDEF AVXSUP}vmovapd [eax + ebx - 32], ymm3;             {$ELSE}db $C5,$FD,$29,$5C,$18,$E0;{$ENDIF} 
       jmp @addforxloop

       @loopEnd:

       sub ebx, 128;

       jz @nextLine;

       @addforxloop2:
           add ebx, 16;
           jg @@lastElem;

           {$IFDEF AVXSUP}vmovapd xmm0, [ecx + ebx - 16];             {$ELSE}db $C5,$F9,$28,$44,$19,$F0;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd xmm0, xmm0, [esi + ebx - 16];        {$ELSE}db $C5,$F9,$59,$44,$1E,$F0;{$ENDIF} 

           {$IFDEF AVXSUP}vmovapd [eax + ebx - 16], xmm0;             {$ELSE}db $C5,$F9,$29,$44,$18,$F0;{$ENDIF} 
       jmp @addforxloop2;

       @@lastElem:
       sub ebx, 16;

       jz @nextLine;

       {$IFDEF AVXSUP}vmovsd xmm0, [ecx + ebx];                       {$ELSE}db $C5,$FB,$10,$04,$19;{$ENDIF} 
       {$IFDEF AVXSUP}vmulsd xmm0, xmm0, [esi + ebx];                 {$ELSE}db $C5,$FB,$59,$04,$1E;{$ENDIF} 
       {$IFDEF AVXSUP}vmovsd [eax + ebx], xmm0;                       {$ELSE}db $C5,$FB,$11,$04,$18;{$ENDIF} 

       @nextLine:

       // next line:
       add ecx, LineWidth1;
       add esi, LineWidth2;
       add eax, edx;

   // loop y end
   dec Height;
   jnz @@addforyloop;

   // epilog
   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;

procedure AVXMatrixElemMultUnAligned(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width : NativeInt; height : NativeInt; const LineWidth1, LineWidth2 : NativeInt);
asm
   // prolog - simulate stack
   push ebx;
   push edi;
   push esi;

   //iters := -width*sizeof(double);
   mov edi, width;
   imul edi, -8;

   // helper registers for the mt1, mt2 and dest pointers
   mov esi, mt2;

   sub eax, edi;
   sub ecx, edi;
   sub esi, edi;

   // for y := 0 to height - 1:
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov ebx, edi;
       @addforxloop:
           add ebx, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [ecx + ebx];
           // prefetch [esi + ebx];

           // mult:
           {$IFDEF AVXSUP}vmovupd ymm0, [ecx + ebx - 128];            {$ELSE}db $C5,$FD,$10,$44,$19,$80;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd ymm1, [esi + ebx - 128];            {$ELSE}db $C5,$FD,$10,$4C,$1E,$80;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm0, ymm0, ymm1;                    {$ELSE}db $C5,$FD,$59,$C1;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd [eax + ebx - 128], ymm0;            {$ELSE}db $C5,$FD,$11,$44,$18,$80;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd ymm2, [ecx + ebx - 96];             {$ELSE}db $C5,$FD,$10,$54,$19,$A0;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd ymm3, [esi + ebx - 96];             {$ELSE}db $C5,$FD,$10,$5C,$1E,$A0;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm2, ymm2, ymm3;                    {$ELSE}db $C5,$ED,$59,$D3;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd [eax + ebx - 96], ymm2;             {$ELSE}db $C5,$FD,$11,$54,$18,$A0;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd ymm0, [ecx + ebx - 64];             {$ELSE}db $C5,$FD,$10,$44,$19,$C0;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd ymm1, [esi + ebx - 64];             {$ELSE}db $C5,$FD,$10,$4C,$1E,$C0;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm0, ymm0, ymm1;                    {$ELSE}db $C5,$FD,$59,$C1;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd [eax + ebx - 64], ymm0;             {$ELSE}db $C5,$FD,$11,$44,$18,$C0;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd ymm2, [ecx + ebx - 32];             {$ELSE}db $C5,$FD,$10,$54,$19,$E0;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd ymm3, [esi + ebx - 32];             {$ELSE}db $C5,$FD,$10,$5C,$1E,$E0;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm2, ymm2, ymm3;                    {$ELSE}db $C5,$ED,$59,$D3;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd [eax + ebx - 32], ymm2;             {$ELSE}db $C5,$FD,$11,$54,$18,$E0;{$ENDIF} 
       jmp @addforxloop

       @loopEnd:

       sub ebx, 128;

       jz @nextLine;

       @addforxloop2:
           add ebx, 16;
           jg @@lastElem;

           {$IFDEF AVXSUP}vmovupd xmm0, [ecx + ebx - 16];             {$ELSE}db $C5,$F9,$10,$44,$19,$F0;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd xmm1, [esi + ebx - 16];             {$ELSE}db $C5,$F9,$10,$4C,$1E,$F0;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd xmm0, xmm0, xmm1;                    {$ELSE}db $C5,$F9,$59,$C1;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd [eax + ebx - 16], xmm0;             {$ELSE}db $C5,$F9,$11,$44,$18,$F0;{$ENDIF} 
       jmp @addforxloop2;

       @@lastElem:
       sub ebx, 16;

       jz @nextLine;

       {$IFDEF AVXSUP}vmovsd xmm0, [ecx + ebx];                       {$ELSE}db $C5,$FB,$10,$04,$19;{$ENDIF} 
       {$IFDEF AVXSUP}vmulsd xmm0, xmm0, [esi + ebx];                 {$ELSE}db $C5,$FB,$59,$04,$1E;{$ENDIF} 
       {$IFDEF AVXSUP}vmovsd [eax + ebx], xmm0;                       {$ELSE}db $C5,$FB,$11,$04,$18;{$ENDIF} 

       @nextLine:

       // next line:
       add ecx, LineWidth1;
       add esi, LineWidth2;
       add eax, edx;

   // loop y end
   dec Height;
   jnz @@addforyloop;

   // epilog
   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;

// ##############################################################
// #### Elementwise divide of matrix 1 to matrix 2
// ##############################################################

procedure AVXMatrixElemDivAligned(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width : NativeInt; height : NativeInt; const LineWidth1, LineWidth2 : NativeInt);
asm
   // prolog - simulate stack
   push ebx;
   push edi;
   push esi;

   //iters := -width*sizeof(double);
   mov edi, width;
   imul edi, -8;

   // helper registers for the mt1, mt2 and dest pointers
   mov esi, mt2;

   sub ecx, edi;
   sub esi, edi;
   sub eax, edi;

   // for y := 0 to height - 1:
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov ebx, edi;
       @addforxloop:
           add ebx, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [ecx + ebx];
           // prefetch [esi + ebx];

           // mult:
           {$IFDEF AVXSUP}vmovapd ymm0, [ecx + ebx - 128];            {$ELSE}db $C5,$FD,$28,$44,$19,$80;{$ENDIF} 
           {$IFDEF AVXSUP}vdivpd ymm0, ymm0, [esi + ebx - 128];       {$ELSE}db $C5,$FD,$5E,$44,$1E,$80;{$ENDIF} 

           {$IFDEF AVXSUP}vmovapd [eax + ebx - 128], ymm0;            {$ELSE}db $C5,$FD,$29,$44,$18,$80;{$ENDIF} 

           {$IFDEF AVXSUP}vmovapd ymm1, [ecx + ebx - 96];             {$ELSE}db $C5,$FD,$28,$4C,$19,$A0;{$ENDIF} 
           {$IFDEF AVXSUP}vdivpd ymm1, ymm1, [esi + ebx - 96];        {$ELSE}db $C5,$F5,$5E,$4C,$1E,$A0;{$ENDIF} 

           {$IFDEF AVXSUP}vmovapd [eax + ebx - 96], ymm1;             {$ELSE}db $C5,$FD,$29,$4C,$18,$A0;{$ENDIF} 

           {$IFDEF AVXSUP}vmovapd ymm2, [ecx + ebx - 64];             {$ELSE}db $C5,$FD,$28,$54,$19,$C0;{$ENDIF} 
           {$IFDEF AVXSUP}vdivpd ymm2, ymm2, [esi + ebx - 64];        {$ELSE}db $C5,$ED,$5E,$54,$1E,$C0;{$ENDIF} 

           {$IFDEF AVXSUP}vmovapd [eax + ebx - 64], ymm2;             {$ELSE}db $C5,$FD,$29,$54,$18,$C0;{$ENDIF} 

           {$IFDEF AVXSUP}vmovapd ymm3, [ecx + ebx - 32];             {$ELSE}db $C5,$FD,$28,$5C,$19,$E0;{$ENDIF} 
           {$IFDEF AVXSUP}vdivpd ymm3, ymm3, [esi + ebx - 32];        {$ELSE}db $C5,$E5,$5E,$5C,$1E,$E0;{$ENDIF} 

           {$IFDEF AVXSUP}vmovapd [eax + ebx - 32], ymm3;             {$ELSE}db $C5,$FD,$29,$5C,$18,$E0;{$ENDIF} 
       jmp @addforxloop

       @loopEnd:

       sub ebx, 128;

       jz @nextLine;

       @addforxloop2:
           add ebx, 16;
           jg @@lastElem;

           {$IFDEF AVXSUP}vmovapd xmm0, [ecx + ebx - 16];             {$ELSE}db $C5,$F9,$28,$44,$19,$F0;{$ENDIF} 
           {$IFDEF AVXSUP}vdivpd xmm0, xmm0, [esi + ebx - 16];        {$ELSE}db $C5,$F9,$5E,$44,$1E,$F0;{$ENDIF} 

           {$IFDEF AVXSUP}vmovapd [eax + ebx - 16], xmm0;             {$ELSE}db $C5,$F9,$29,$44,$18,$F0;{$ENDIF} 
       jmp @addforxloop2;

       @@lastElem:
       sub ebx, 16;

       jz @nextLine;

       {$IFDEF AVXSUP}vmovsd xmm0, [ecx + ebx];                       {$ELSE}db $C5,$FB,$10,$04,$19;{$ENDIF} 
       {$IFDEF AVXSUP}vdivsd xmm0, xmm0, [esi + ebx];                 {$ELSE}db $C5,$FB,$5E,$04,$1E;{$ENDIF} 
       {$IFDEF AVXSUP}vmovsd [eax + ebx], xmm0;                       {$ELSE}db $C5,$FB,$11,$04,$18;{$ENDIF} 

       @nextLine:

       // next line:
       add ecx, LineWidth1;
       add esi, LineWidth2;
       add eax, edx;

   // loop y end
   dec Height;
   jnz @@addforyloop;

   // epilog
   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;

procedure AVXMatrixElemDivUnAligned(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width : NativeInt; height : NativeInt; const LineWidth1, LineWidth2 : NativeInt);
asm
   // prolog - simulate stack
   push ebx;
   push edi;
   push esi;

   //iters := -width*sizeof(double);
   mov edi, width;
   imul edi, -8;

   // helper registers for the mt1, mt2 and dest pointers
   mov esi, mt2;

   sub ecx, edi;
   sub esi, edi;
   sub eax, edi;

   // for y := 0 to height - 1:
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov ebx, edi;
       @addforxloop:
           add ebx, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [ecx + ebx];
           // prefetch [esi + ebx];

           // mult:
           {$IFDEF AVXSUP}vmovupd ymm0, [ecx + ebx - 128];            {$ELSE}db $C5,$FD,$10,$44,$19,$80;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd ymm1, [esi + ebx - 128];            {$ELSE}db $C5,$FD,$10,$4C,$1E,$80;{$ENDIF} 
           {$IFDEF AVXSUP}vdivpd ymm0, ymm0, ymm1;                    {$ELSE}db $C5,$FD,$5E,$C1;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd [eax + ebx - 128], ymm0;            {$ELSE}db $C5,$FD,$11,$44,$18,$80;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd ymm2, [ecx + ebx - 96];             {$ELSE}db $C5,$FD,$10,$54,$19,$A0;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd ymm3, [esi + ebx - 96];             {$ELSE}db $C5,$FD,$10,$5C,$1E,$A0;{$ENDIF} 
           {$IFDEF AVXSUP}vdivpd ymm2, ymm2, ymm3;                    {$ELSE}db $C5,$ED,$5E,$D3;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd [eax + ebx - 96], ymm2;             {$ELSE}db $C5,$FD,$11,$54,$18,$A0;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd ymm0, [ecx + ebx - 64];             {$ELSE}db $C5,$FD,$10,$44,$19,$C0;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd ymm1, [esi + ebx - 64];             {$ELSE}db $C5,$FD,$10,$4C,$1E,$C0;{$ENDIF} 
           {$IFDEF AVXSUP}vdivpd ymm0, ymm0, ymm1;                    {$ELSE}db $C5,$FD,$5E,$C1;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd [eax + ebx - 64], ymm0;             {$ELSE}db $C5,$FD,$11,$44,$18,$C0;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd ymm2, [ecx + ebx - 32];             {$ELSE}db $C5,$FD,$10,$54,$19,$E0;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd ymm3, [esi + ebx - 32];             {$ELSE}db $C5,$FD,$10,$5C,$1E,$E0;{$ENDIF} 
           {$IFDEF AVXSUP}vdivpd ymm2, ymm2, ymm3;                    {$ELSE}db $C5,$ED,$5E,$D3;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd [eax + ebx - 32], ymm2;             {$ELSE}db $C5,$FD,$11,$54,$18,$E0;{$ENDIF} 
       jmp @addforxloop

       @loopEnd:

       sub ebx, 128;

       jz @nextLine;

       @addforxloop2:
           add ebx, 16;
           jg @@lastElem;

           {$IFDEF AVXSUP}vmovupd xmm0, [ecx + ebx - 16];             {$ELSE}db $C5,$F9,$10,$44,$19,$F0;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd xmm1, [esi + ebx - 16];             {$ELSE}db $C5,$F9,$10,$4C,$1E,$F0;{$ENDIF} 
           {$IFDEF AVXSUP}vdivpd xmm0, xmm0, xmm1;                    {$ELSE}db $C5,$F9,$5E,$C1;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd [eax + ebx - 16], xmm0;             {$ELSE}db $C5,$F9,$11,$44,$18,$F0;{$ENDIF} 
       jmp @addforxloop2;

       @@lastElem:
       sub ebx, 16;

       jz @nextLine;

       {$IFDEF AVXSUP}vmovsd xmm0, [ecx + ebx];                       {$ELSE}db $C5,$FB,$10,$04,$19;{$ENDIF} 
       {$IFDEF AVXSUP}vdivsd xmm0, xmm0, [esi + ebx];                 {$ELSE}db $C5,$FB,$5E,$04,$1E;{$ENDIF} 
       {$IFDEF AVXSUP}vmovsd [eax + ebx], xmm0;                       {$ELSE}db $C5,$FB,$11,$04,$18;{$ENDIF} 

       @nextLine:

       // next line:
       add ecx, LineWidth1;
       add esi, LineWidth2;
       add eax, edx;

   // loop y end
   dec height;
   jnz @@addforyloop;

   // epilog
   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;

{$ENDIF}

end.
