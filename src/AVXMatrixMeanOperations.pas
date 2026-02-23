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


unit AVXMatrixMeanOperations;

interface

{$I 'mrMath_CPU.inc'}

{$IFNDEF x64}

procedure AVXMatrixMeanRowAligned(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure AVXMatrixMeanRowUnAligned(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

procedure AVXMatrixMeanColumnAligned(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure AVXMatrixMeanColumnUnAligned(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

procedure AVXMatrixVarRowAligned(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; unbiased : boolean); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure AVXMatrixVarRowUnAligned(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; unbiased : boolean); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

procedure AVXMatrixVarColumnAligned(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; unbiased : boolean); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure AVXMatrixVarColumnUnAligned(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; unbiased : boolean); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}


// combined methods
procedure AVXMatrixMeanVarRowAligned(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; unbiased : boolean); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure AVXMatrixMeanVarRowUnAligned(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; unbiased : boolean); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

procedure AVXMatrixMeanVarColumnAligned(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; unbiased : boolean); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure AVXMatrixMeanVarColumnUnAligned(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; unbiased : boolean); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

{$ENDIF}

implementation

{$IFNDEF x64}

const cLocOne : double = 1;

procedure AVXMatrixMeanRowAligned(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt);
asm
   // prolog
   push edi;
   push esi;
   push ebx;

   // iters := -width*sizeof(double)
   mov edi, width;
   {$IFDEF AVXSUP}vcvtsi2sd xmm4, xmm4, edi;                          {$ELSE}db $C5,$DB,$2A,$E7;{$ENDIF} 
   imul edi, -8;

   // helper registers for the mt1, mt2 and dest pointers
   sub ecx, edi;

   // for y := 0 to height - 1:
   mov ebx, height;
   @@addforyloop:
       {$IFDEF AVXSUP}vxorpd ymm0, ymm0, ymm0;                        {$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} 
       {$IFDEF AVXSUP}vxorpd ymm1, ymm1, ymm1;                        {$ELSE}db $C5,$F5,$57,$C9;{$ENDIF} 

       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov esi, edi;
       @addforxloop:
           add esi, 128;
           jg @loopEnd;
           // prefetch data...
           // prefetch [r8 + esi];

           // addition:
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, [ecx + esi - 128];       {$ELSE}db $C5,$FD,$58,$44,$31,$80;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm1, ymm1, [ecx + esi - 96];        {$ELSE}db $C5,$F5,$58,$4C,$31,$A0;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, [ecx + esi - 64];        {$ELSE}db $C5,$FD,$58,$44,$31,$C0;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm1, ymm1, [ecx + esi - 32];        {$ELSE}db $C5,$F5,$58,$4C,$31,$E0;{$ENDIF} 
       jmp @addforxloop

       @loopEnd:

       {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm1;                        {$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 
       {$IFDEF AVXSUP}vextractf128 xmm2, ymm0, 1;                     {$ELSE}db $C4,$E3,$7D,$19,$C2,$01;{$ENDIF} 
       {$IFDEF AVXSUP}vhaddpd xmm0, xmm0, xmm2;                       {$ELSE}db $C5,$F9,$7C,$C2;{$ENDIF} 

       sub esi, 128;

       jz @buildRes;

       @addforxloop2:
           add esi, 16;
           jg @@addforxloop2end;

           {$IFDEF AVXSUP}vaddpd xmm0, xmm0, [ecx + esi - 16];        {$ELSE}db $C5,$F9,$58,$44,$31,$F0;{$ENDIF} 
       jmp @addforxloop2;

       @@addforxloop2end:

       sub esi, 16;
       jz @buildRes;

       {$IFDEF AVXSUP}vaddsd xmm0, xmm0, [ecx + esi];                 {$ELSE}db $C5,$FB,$58,$04,$31;{$ENDIF} 

       @buildRes:

       // build result
       {$IFDEF AVXSUP}vhaddpd xmm0, xmm0, xmm0;                       {$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} 

       {$IFDEF AVXSUP}vdivsd xmm0, xmm0, xmm4;                        {$ELSE}db $C5,$FB,$5E,$C4;{$ENDIF} 

       // write result
       {$IFDEF AVXSUP}vmovsd [eax], xmm0;                             {$ELSE}db $C5,$FB,$11,$00;{$ENDIF} 

       // next line:
       add ecx, srcLineWidth;
       add eax, edx;

   // loop y end
   dec ebx;
   jnz @@addforyloop;

   // epilog
   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop ebx;
   pop esi;
   pop edi;
end;

procedure AVXMatrixMeanRowUnAligned(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt);
asm
   // prolog
   push edi;
   push esi;
   push ebx;

   // iters := -width*sizeof(double)
   mov edi, width;
   {$IFDEF AVXSUP}vcvtsi2sd xmm4, xmm4, edi;                          {$ELSE}db $C5,$DB,$2A,$E7;{$ENDIF} 
   imul edi, -8;

   // helper registers for the mt1, mt2 and dest pointers
   sub ecx, edi;

   // for y := 0 to height - 1:
   mov ebx, height;
   @@addforyloop:
       {$IFDEF AVXSUP}vxorpd ymm0, ymm0, ymm0;                        {$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} 
       {$IFDEF AVXSUP}vxorpd ymm1, ymm1, ymm1;                        {$ELSE}db $C5,$F5,$57,$C9;{$ENDIF} 

       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov esi, edi;
       @addforxloop:
           add esi, 128;
           jg @loopEnd;
           // prefetch data...
           // prefetch [r8 + esi];

           // addition:
           {$IFDEF AVXSUP}vmovupd ymm2, [ecx + esi - 128];            {$ELSE}db $C5,$FD,$10,$54,$31,$80;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd ymm3, [ecx + esi - 96];             {$ELSE}db $C5,$FD,$10,$5C,$31,$A0;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm2;                    {$ELSE}db $C5,$FD,$58,$C2;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm1, ymm1, ymm3;                    {$ELSE}db $C5,$F5,$58,$CB;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd ymm2, [ecx + esi - 64];             {$ELSE}db $C5,$FD,$10,$54,$31,$C0;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd ymm3, [ecx + esi - 32];             {$ELSE}db $C5,$FD,$10,$5C,$31,$E0;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm2;                    {$ELSE}db $C5,$FD,$58,$C2;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm1, ymm1, ymm3;                    {$ELSE}db $C5,$F5,$58,$CB;{$ENDIF} 
       jmp @addforxloop

       @loopEnd:

       {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm1;                        {$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 
       {$IFDEF AVXSUP}vextractf128 xmm2, ymm0, 1;                     {$ELSE}db $C4,$E3,$7D,$19,$C2,$01;{$ENDIF} 
       {$IFDEF AVXSUP}vhaddpd xmm0, xmm0, xmm2;                       {$ELSE}db $C5,$F9,$7C,$C2;{$ENDIF} 

       sub esi, 128;

       jz @buildRes;

       @addforxloop2:
           add esi, 16;
           jg @@addforxloop2end;

           {$IFDEF AVXSUP}vmovupd xmm1, [ecx + esi - 16];             {$ELSE}db $C5,$F9,$10,$4C,$31,$F0;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd xmm0, xmm0, xmm1;                    {$ELSE}db $C5,$F9,$58,$C1;{$ENDIF} 
       jmp @addforxloop2;

       @@addforxloop2end:

       sub esi, 16;
       jz @buildRes;

       {$IFDEF AVXSUP}vaddsd xmm0, xmm0, [ecx + esi];                 {$ELSE}db $C5,$FB,$58,$04,$31;{$ENDIF} 

       @buildRes:

       // build result
       {$IFDEF AVXSUP}vhaddpd xmm0, xmm0, xmm0;                       {$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} 

       {$IFDEF AVXSUP}vdivsd xmm0, xmm0, xmm4;                        {$ELSE}db $C5,$FB,$5E,$C4;{$ENDIF} 

       // write result
       {$IFDEF AVXSUP}vmovsd [eax], xmm0;                             {$ELSE}db $C5,$FB,$11,$00;{$ENDIF} 

       // next line:
       add ecx, srcLineWidth;
       add eax, edx;

   // loop y end
   dec ebx;
   jnz @@addforyloop;

   // epilog
   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop ebx;
   pop esi;
   pop edi;
end;

procedure AVXMatrixMeanColumnAligned(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt);
var tmp : double;
asm
   // prolog
   push ebx;
   push edi;
   push esi;

   // prepare
   mov ebx, height;
   {$IFDEF AVXSUP}vcvtsi2sd xmm0, xmm0, ebx;                          {$ELSE}db $C5,$FB,$2A,$C3;{$ENDIF} 
   imul ebx, srcLineWidth;
   imul ebx, -1;

   // helper registers for the mt1, mt2 and dest pointers
   sub ecx, ebx;

   mov edi, srcLineWidth;

   lea edx, tmp;
   {$IFDEF AVXSUP}vmovsd [edx], xmm0;                                 {$ELSE}db $C5,$FB,$11,$02;{$ENDIF} 
   {$IFDEF AVXSUP}vbroadcastsd ymm2, [edx];                           {$ELSE}db $C4,$E2,$7D,$19,$12;{$ENDIF} 
   // vbroadcastsd ymm2, xmm0; // avx2


   // for x := 0 to width - 1:
   mov esi, Width;
   sub esi, 4;
   jl @@addforxloop4end;

   // 4 columns at once
   @@addforxloop4:
       {$IFDEF AVXSUP}vxorpd ymm1, ymm1, ymm1;                        {$ELSE}db $C5,$F5,$57,$C9;{$ENDIF} 

       // for y := 0 to height - 1;
       // prepare for reverse loop
       mov edx, ebx;
       @addforyloop4:
           {$IFDEF AVXSUP}vaddpd ymm1, ymm1, [ecx + edx];             {$ELSE}db $C5,$F5,$58,$0C,$11;{$ENDIF} 
       add edx, edi;
       jnz @addforyloop4;

       // build result
       {$IFDEF AVXSUP}vdivpd ymm1, ymm1, ymm2;                        {$ELSE}db $C5,$F5,$5E,$CA;{$ENDIF} 
       {$IFDEF AVXSUP}vmovapd [eax], ymm1;                            {$ELSE}db $C5,$FD,$29,$08;{$ENDIF} 

       // next columns:
       add eax, 32;
       add ecx, 32;

   // loop x end
   sub esi, 4;
   jge @@addforxloop4;

   @@addforxloop4end:

   add esi, 4;
   jz @@endProc;

   sub esi, 2;
   jl @@lastcolumn;

   {$IFDEF AVXSUP}vxorpd xmm1, xmm1, xmm1;                            {$ELSE}db $C5,$F1,$57,$C9;{$ENDIF} 

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov edx, ebx;
   @addforyloop2:
       {$IFDEF AVXSUP}vaddpd xmm1, xmm1, [ecx + edx];                 {$ELSE}db $C5,$F1,$58,$0C,$11;{$ENDIF} 
   add edx, edi;
   jnz @addforyloop2;

   // build result
   {$IFDEF AVXSUP}vdivpd xmm1, xmm1, xmm2;                            {$ELSE}db $C5,$F1,$5E,$CA;{$ENDIF} 
   {$IFDEF AVXSUP}vmovapd [eax], xmm1;                                {$ELSE}db $C5,$F9,$29,$08;{$ENDIF} 

   // next columns:
   add eax, 16;
   add ecx, 16;

   dec esi;
   jnz @@endProc;

   @@lastcolumn:

   {$IFDEF AVXSUP}vxorpd xmm1, xmm1, xmm1;                            {$ELSE}db $C5,$F1,$57,$C9;{$ENDIF} 

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov edx, ebx;
   @addforyloop:
       {$IFDEF AVXSUP}vaddsd xmm1, xmm1, [ecx + edx];                 {$ELSE}db $C5,$F3,$58,$0C,$11;{$ENDIF} 
   add edx, edi;
   jnz @addforyloop;

   // build result
   {$IFDEF AVXSUP}vdivsd xmm1, xmm1, xmm2;                            {$ELSE}db $C5,$F3,$5E,$CA;{$ENDIF} 
   {$IFDEF AVXSUP}vmovsd [eax], xmm1;                                 {$ELSE}db $C5,$FB,$11,$08;{$ENDIF} 

   @@endProc:
   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 

   // epilog
   pop esi;
   pop edi;
   pop ebx;
end;

procedure AVXMatrixMeanColumnUnAligned(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt);
var tmp : double;
asm
   // prolog
   push ebx;
   push edi;
   push esi;

   // prepare
   mov ebx, height;
   {$IFDEF AVXSUP}vcvtsi2sd xmm0, xmm0, ebx;                          {$ELSE}db $C5,$FB,$2A,$C3;{$ENDIF} 
   imul ebx, srcLineWidth;
   imul ebx, -1;

   // helper registers for the mt1, mt2 and dest pointers
   sub ecx, ebx;

   mov edi, srcLineWidth;

   lea edx, tmp;
   {$IFDEF AVXSUP}vmovsd [edx], xmm0;                                 {$ELSE}db $C5,$FB,$11,$02;{$ENDIF} 
   {$IFDEF AVXSUP}vbroadcastsd ymm2, [edx];                           {$ELSE}db $C4,$E2,$7D,$19,$12;{$ENDIF} 
   // vbroadcastsd ymm2, xmm0; // avx2


   // for x := 0 to width - 1:
   mov esi, Width;
   sub esi, 4;
   jl @@addforxloop4end;

   // 4 columns at once
   @@addforxloop4:
       {$IFDEF AVXSUP}vxorpd ymm1, ymm1, ymm1;                        {$ELSE}db $C5,$F5,$57,$C9;{$ENDIF} 

       // for y := 0 to height - 1;
       // prepare for reverse loop
       mov edx, ebx;
       @addforyloop4:
           {$IFDEF AVXSUP}vmovupd ymm0, [ecx + edx];                  {$ELSE}db $C5,$FD,$10,$04,$11;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm1, ymm1, ymm0;                    {$ELSE}db $C5,$F5,$58,$C8;{$ENDIF} 
       add edx, edi;
       jnz @addforyloop4;

       // build result
       {$IFDEF AVXSUP}vdivpd ymm1, ymm1, ymm2;                        {$ELSE}db $C5,$F5,$5E,$CA;{$ENDIF} 
       {$IFDEF AVXSUP}vmovupd [eax], ymm1;                            {$ELSE}db $C5,$FD,$11,$08;{$ENDIF} 

       // next columns:
       add eax, 32;
       add ecx, 32;

   // loop x end
   sub esi, 4;
   jge @@addforxloop4;

   @@addforxloop4end:

   add esi, 4;
   jz @@endProc;

   sub esi, 2;
   jl @@lastcolumn;

   {$IFDEF AVXSUP}vxorpd xmm1, xmm1, xmm1;                            {$ELSE}db $C5,$F1,$57,$C9;{$ENDIF} 

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov edx, ebx;
   @addforyloop2:
       {$IFDEF AVXSUP}vmovupd xmm0, [ecx + edx];                      {$ELSE}db $C5,$F9,$10,$04,$11;{$ENDIF} 
       {$IFDEF AVXSUP}vaddpd xmm1, xmm1, xmm0;                        {$ELSE}db $C5,$F1,$58,$C8;{$ENDIF} 
   add edx, edi;
   jnz @addforyloop2;

   // build result
   {$IFDEF AVXSUP}vdivpd xmm1, xmm1, xmm2;                            {$ELSE}db $C5,$F1,$5E,$CA;{$ENDIF} 
   {$IFDEF AVXSUP}vmovupd [eax], xmm1;                                {$ELSE}db $C5,$F9,$11,$08;{$ENDIF} 

   // next columns:
   add eax, 16;
   add ecx, 16;

   dec esi;
   jnz @@endProc;

   @@lastcolumn:

   {$IFDEF AVXSUP}vxorpd xmm1, xmm1, xmm1;                            {$ELSE}db $C5,$F1,$57,$C9;{$ENDIF} 

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov edx, ebx;
   @addforyloop:
       {$IFDEF AVXSUP}vaddsd xmm1, xmm1, [ecx + edx];                 {$ELSE}db $C5,$F3,$58,$0C,$11;{$ENDIF} 
   add edx, edi;
   jnz @addforyloop;

   // build result
   {$IFDEF AVXSUP}vdivsd xmm1, xmm1, xmm2;                            {$ELSE}db $C5,$F3,$5E,$CA;{$ENDIF} 
   {$IFDEF AVXSUP}vmovsd [eax], xmm1;                                 {$ELSE}db $C5,$FB,$11,$08;{$ENDIF} 

   @@endProc:
   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 

   // epilog
   pop esi;
   pop edi;
   pop ebx;
end;

// ###########################################
// #### Variance calculation
// ###########################################


procedure AVXMatrixVarRowAligned(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; unbiased : boolean);
var tmp : double;
    aDestLineWidth : NativeInt;
asm
   // prolog
   push ebx;
   push edi;
   push esi;

   // iters := -width*sizeof(double)
   mov edi, width;
   {$IFDEF AVXSUP}vcvtsi2sd xmm3, xmm3, edi;                          {$ELSE}db $C5,$E3,$2A,$DF;{$ENDIF} 
   imul edi, -8;

   // helper registers for the src and dest pointers
   sub ecx, edi;

   // for y := 0 to height - 1:
   @@addforyloop:
       {$IFDEF AVXSUP}vxorpd ymm0, ymm0, ymm0;                        {$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} 
       {$IFDEF AVXSUP}vxorpd ymm1, ymm1, ymm1;                        {$ELSE}db $C5,$F5,$57,$C9;{$ENDIF} 

       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov esi, edi;
       @addforxloop:
           add esi, 128;
           jg @loopEnd;
           // prefetch data...
           // prefetch [r8 + esi];

           // addition:
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, [ecx + esi - 128];       {$ELSE}db $C5,$FD,$58,$44,$31,$80;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm1, ymm1, [ecx + esi - 96];        {$ELSE}db $C5,$F5,$58,$4C,$31,$A0;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, [ecx + esi - 64];        {$ELSE}db $C5,$FD,$58,$44,$31,$C0;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm1, ymm1, [ecx + esi - 32];        {$ELSE}db $C5,$F5,$58,$4C,$31,$E0;{$ENDIF} 
       jmp @addforxloop

       @loopEnd:

       {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm1;                        {$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 
       {$IFDEF AVXSUP}vextractf128 xmm2, ymm0, 1;                     {$ELSE}db $C4,$E3,$7D,$19,$C2,$01;{$ENDIF} 
       {$IFDEF AVXSUP}vhaddpd xmm0, xmm0, xmm2;                       {$ELSE}db $C5,$F9,$7C,$C2;{$ENDIF} 

       sub esi, 128;

       jz @buildRes;

       @addforxloop2:
           add esi, 16;
           jg @@addforxloop2end;

           {$IFDEF AVXSUP}vaddpd xmm0, xmm0, [ecx + esi - 16];        {$ELSE}db $C5,$F9,$58,$44,$31,$F0;{$ENDIF} 
       jmp @addforxloop2;

       @@addforxloop2end:

       sub esi, 16;
       jz @buildRes;

       {$IFDEF AVXSUP}vaddsd xmm0, xmm0, [ecx + esi];                 {$ELSE}db $C5,$FB,$58,$04,$31;{$ENDIF} 

       @buildRes:

       // build result
       {$IFDEF AVXSUP}vhaddpd xmm0, xmm0, xmm0;                       {$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} 
       {$IFDEF AVXSUP}vdivsd xmm0, xmm0, xmm3;                        {$ELSE}db $C5,$FB,$5E,$C3;{$ENDIF} 

       // we have calculated the mean -> 
       // repeat the loop to calculate the variance
       lea esi, tmp;
       {$IFDEF AVXSUP}vmovsd [esi], xmm0;                             {$ELSE}db $C5,$FB,$11,$06;{$ENDIF} 
       {$IFDEF AVXSUP}vbroadcastsd ymm4, [esi];                       {$ELSE}db $C4,$E2,$7D,$19,$26;{$ENDIF} 
       // vbroadcastsd ymm4, xmm0; //avx2
       {$IFDEF AVXSUP}vxorpd xmm0, xmm0, xmm0;                        {$ELSE}db $C5,$F9,$57,$C0;{$ENDIF} 
            
       // for x := 0 to w - 1;
       // prepare for reverse loop
       // variance = sum (x - mean)^2
       mov esi, edi;
       @addforxloop3:
           add esi, 128;
           jg @loopEnd2;
           // prefetch data...
           // prefetch [eax + esi];

           // addition:
           {$IFDEF AVXSUP}vmovapd ymm1, [ecx + esi - 128];            {$ELSE}db $C5,$FD,$28,$4C,$31,$80;{$ENDIF} 
           {$IFDEF AVXSUP}vsubpd ymm1, ymm1, ymm4;                    {$ELSE}db $C5,$F5,$5C,$CC;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm1, ymm1, ymm1;                    {$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm1;                    {$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 
                
           {$IFDEF AVXSUP}vmovapd ymm1, [ecx + esi - 96];             {$ELSE}db $C5,$FD,$28,$4C,$31,$A0;{$ENDIF} 
           {$IFDEF AVXSUP}vsubpd ymm1, ymm1, ymm4;                    {$ELSE}db $C5,$F5,$5C,$CC;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm1, ymm1, ymm1;                    {$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm1;                    {$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 

           {$IFDEF AVXSUP}vmovapd ymm1, [ecx + esi - 64];             {$ELSE}db $C5,$FD,$28,$4C,$31,$C0;{$ENDIF} 
           {$IFDEF AVXSUP}vsubpd ymm1, ymm1, ymm4;                    {$ELSE}db $C5,$F5,$5C,$CC;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm1, ymm1, ymm1;                    {$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm1;                    {$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 

           {$IFDEF AVXSUP}vmovapd ymm1, [ecx + esi - 32];             {$ELSE}db $C5,$FD,$28,$4C,$31,$E0;{$ENDIF} 
           {$IFDEF AVXSUP}vsubpd ymm1, ymm1, ymm4;                    {$ELSE}db $C5,$F5,$5C,$CC;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm1, ymm1, ymm1;                    {$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm1;                    {$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 

       jmp @addforxloop3

       @loopEnd2:

       sub esi, 128;

       {$IFDEF AVXSUP}vextractf128 xmm2, ymm0, 1;                     {$ELSE}db $C4,$E3,$7D,$19,$C2,$01;{$ENDIF} 
       {$IFDEF AVXSUP}vhaddpd xmm0, xmm0, xmm2;                       {$ELSE}db $C5,$F9,$7C,$C2;{$ENDIF} 

       jz @buildRes2;

       @addforxloop4:
           add esi, 16;
           jg @addforxloop4end;

           {$IFDEF AVXSUP}vmovapd xmm1, [ecx + esi - 16];             {$ELSE}db $C5,$F9,$28,$4C,$31,$F0;{$ENDIF} 
           {$IFDEF AVXSUP}vsubpd xmm1, xmm1, xmm4;                    {$ELSE}db $C5,$F1,$5C,$CC;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd xmm1, xmm1, xmm1;                    {$ELSE}db $C5,$F1,$59,$C9;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd xmm0, xmm0, xmm1;                    {$ELSE}db $C5,$F9,$58,$C1;{$ENDIF} 
       jmp @addforxloop4;

       @addforxloop4end:

       sub esi, 16;
       jz @buildRes2;

       // last column
       {$IFDEF AVXSUP}vmovsd xmm1, [ecx + esi];                       {$ELSE}db $C5,$FB,$10,$0C,$31;{$ENDIF} 
       {$IFDEF AVXSUP}vsubsd xmm1, xmm1, xmm4;                        {$ELSE}db $C5,$F3,$5C,$CC;{$ENDIF} 
       {$IFDEF AVXSUP}vmulsd xmm1, xmm1, xmm1;                        {$ELSE}db $C5,$F3,$59,$C9;{$ENDIF} 
       {$IFDEF AVXSUP}vaddsd xmm0, xmm0, xmm1;                        {$ELSE}db $C5,$FB,$58,$C1;{$ENDIF} 

       @buildRes2:

       // build result
       {$IFDEF AVXSUP}vhaddpd xmm0, xmm0, xmm0;                       {$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} 
            
       // check if we need to use the unbiased version
       cmp unbiased, 0;
       jz @@dobiased;

       lea esi, cLocOne;
       {$IFDEF AVXSUP}vmovsd xmm2, [esi];                             {$ELSE}db $C5,$FB,$10,$16;{$ENDIF} 
       {$IFDEF AVXSUP}vsubsd xmm4, xmm3, xmm2;                        {$ELSE}db $C5,$E3,$5C,$E2;{$ENDIF} 
       {$IFDEF AVXSUP}vmaxsd xmm4, xmm4, xmm2;                        {$ELSE}db $C5,$DB,$5F,$E2;{$ENDIF} 

       {$IFDEF AVXSUP}vdivsd xmm0, xmm0, xmm4;                        {$ELSE}db $C5,$FB,$5E,$C4;{$ENDIF} 

       jmp @@writeRes;

       @@dobiased:
       {$IFDEF AVXSUP}vdivsd xmm0, xmm0, xmm3;                        {$ELSE}db $C5,$FB,$5E,$C3;{$ENDIF} 
            
       // write result
       @@writeRes:
       {$IFDEF AVXSUP}vmovsd [eax], xmm0;                             {$ELSE}db $C5,$FB,$11,$00;{$ENDIF} 

       // next line:
       add ecx, srcLineWidth;
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

procedure AVXMatrixVarRowUnAligned(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; unbiased : boolean);
var tmp : double;
asm
   // prolog
   push ebx;
   push edi;
   push esi;

   // iters := -width*sizeof(double)
   mov edi, width;
   {$IFDEF AVXSUP}vcvtsi2sd xmm3, xmm3, edi;                          {$ELSE}db $C5,$E3,$2A,$DF;{$ENDIF} 
   imul edi, -8;

   // helper registers for the src and dest pointers
   sub ecx, edi;

   // for y := 0 to height - 1:
   @@addforyloop:
       {$IFDEF AVXSUP}vxorpd ymm0, ymm0, ymm0;                        {$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} 
       {$IFDEF AVXSUP}vxorpd ymm1, ymm1, ymm1;                        {$ELSE}db $C5,$F5,$57,$C9;{$ENDIF} 

       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov esi, edi;
       @addforxloop:
           add esi, 128;
           jg @loopEnd;
           // prefetch data...
           // prefetch [r8 + esi];

           // addition:
           {$IFDEF AVXSUP}vmovupd ymm2, [ecx + esi - 128];            {$ELSE}db $C5,$FD,$10,$54,$31,$80;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm2;                    {$ELSE}db $C5,$FD,$58,$C2;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd ymm4, [ecx + esi - 96];             {$ELSE}db $C5,$FD,$10,$64,$31,$A0;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm1, ymm1, ymm4;                    {$ELSE}db $C5,$F5,$58,$CC;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd ymm2, [ecx + esi - 64];             {$ELSE}db $C5,$FD,$10,$54,$31,$C0;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm2;                    {$ELSE}db $C5,$FD,$58,$C2;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd ymm4, [ecx + esi - 32];             {$ELSE}db $C5,$FD,$10,$64,$31,$E0;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm1, ymm1, ymm4;                    {$ELSE}db $C5,$F5,$58,$CC;{$ENDIF} 
       jmp @addforxloop

       @loopEnd:

       {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm1;                        {$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 
       {$IFDEF AVXSUP}vextractf128 xmm2, ymm0, 1;                     {$ELSE}db $C4,$E3,$7D,$19,$C2,$01;{$ENDIF} 
       {$IFDEF AVXSUP}vhaddpd xmm0, xmm0, xmm2;                       {$ELSE}db $C5,$F9,$7C,$C2;{$ENDIF} 

       sub esi, 128;

       jz @buildRes;

       @addforxloop2:
           add esi, 16;
           jg @@addforxloop2end;

           {$IFDEF AVXSUP}vmovupd xmm1, [ecx + esi - 16];             {$ELSE}db $C5,$F9,$10,$4C,$31,$F0;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd xmm0, xmm0, xmm1;                    {$ELSE}db $C5,$F9,$58,$C1;{$ENDIF} 
       jmp @addforxloop2;

       @@addforxloop2end:

       sub esi, 16;
       jz @buildRes;

       {$IFDEF AVXSUP}vaddsd xmm0, xmm0, [ecx + esi];                 {$ELSE}db $C5,$FB,$58,$04,$31;{$ENDIF} 

       @buildRes:

       // build result
       {$IFDEF AVXSUP}vhaddpd xmm0, xmm0, xmm0;                       {$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} 
       {$IFDEF AVXSUP}vdivsd xmm0, xmm0, xmm3;                        {$ELSE}db $C5,$FB,$5E,$C3;{$ENDIF} 

       // we have calculated the mean ->
       // repeat the loop to calculate the variance
       lea esi, tmp;
       {$IFDEF AVXSUP}vmovsd [esi], xmm0;                             {$ELSE}db $C5,$FB,$11,$06;{$ENDIF} 
       {$IFDEF AVXSUP}vbroadcastsd ymm4, [esi];                       {$ELSE}db $C4,$E2,$7D,$19,$26;{$ENDIF} 
       // vbroadcastsd ymm4, xmm0;  // avx2 instruction
       {$IFDEF AVXSUP}vxorpd xmm0, xmm0, xmm0;                        {$ELSE}db $C5,$F9,$57,$C0;{$ENDIF} 

       // for x := 0 to w - 1;
       // prepare for reverse loop
       // variance = sum (x - mean)^2
       mov esi, edi;
       @addforxloop3:
           add esi, 128;
           jg @loopEnd2;
           // prefetch data...
           // prefetch [eax + esi];

           // addition:
           {$IFDEF AVXSUP}vmovupd ymm1, [ecx + esi - 128];            {$ELSE}db $C5,$FD,$10,$4C,$31,$80;{$ENDIF} 
           {$IFDEF AVXSUP}vsubpd ymm1, ymm1, ymm4;                    {$ELSE}db $C5,$F5,$5C,$CC;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm1, ymm1, ymm1;                    {$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm1;                    {$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd ymm1, [ecx + esi - 96];             {$ELSE}db $C5,$FD,$10,$4C,$31,$A0;{$ENDIF} 
           {$IFDEF AVXSUP}vsubpd ymm1, ymm1, ymm4;                    {$ELSE}db $C5,$F5,$5C,$CC;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm1, ymm1, ymm1;                    {$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm1;                    {$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd ymm1, [ecx + esi - 64];             {$ELSE}db $C5,$FD,$10,$4C,$31,$C0;{$ENDIF} 
           {$IFDEF AVXSUP}vsubpd ymm1, ymm1, ymm4;                    {$ELSE}db $C5,$F5,$5C,$CC;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm1, ymm1, ymm1;                    {$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm1;                    {$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd ymm1, [ecx + esi - 32];             {$ELSE}db $C5,$FD,$10,$4C,$31,$E0;{$ENDIF} 
           {$IFDEF AVXSUP}vsubpd ymm1, ymm1, ymm4;                    {$ELSE}db $C5,$F5,$5C,$CC;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm1, ymm1, ymm1;                    {$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm1;                    {$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 

       jmp @addforxloop3

       @loopEnd2:

       sub esi, 128;

       {$IFDEF AVXSUP}vextractf128 xmm2, ymm0, 1;                     {$ELSE}db $C4,$E3,$7D,$19,$C2,$01;{$ENDIF} 
       {$IFDEF AVXSUP}vhaddpd xmm0, xmm0, xmm2;                       {$ELSE}db $C5,$F9,$7C,$C2;{$ENDIF} 

       jz @buildRes2;

       @addforxloop4:
           add esi, 16;
           jg @addforxloop4end;

           {$IFDEF AVXSUP}vmovupd xmm1, [ecx + esi - 16];             {$ELSE}db $C5,$F9,$10,$4C,$31,$F0;{$ENDIF} 
           {$IFDEF AVXSUP}vsubpd xmm1, xmm1, xmm4;                    {$ELSE}db $C5,$F1,$5C,$CC;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd xmm1, xmm1, xmm1;                    {$ELSE}db $C5,$F1,$59,$C9;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd xmm0, xmm0, xmm1;                    {$ELSE}db $C5,$F9,$58,$C1;{$ENDIF} 
       jmp @addforxloop4;

       @addforxloop4end:

       sub esi, 16;
       jz @buildRes2;

       // last column
       {$IFDEF AVXSUP}vmovsd xmm1, [ecx + esi];                       {$ELSE}db $C5,$FB,$10,$0C,$31;{$ENDIF} 
       {$IFDEF AVXSUP}vsubsd xmm1, xmm1, xmm4;                        {$ELSE}db $C5,$F3,$5C,$CC;{$ENDIF} 
       {$IFDEF AVXSUP}vmulsd xmm1, xmm1, xmm1;                        {$ELSE}db $C5,$F3,$59,$C9;{$ENDIF} 
       {$IFDEF AVXSUP}vaddsd xmm0, xmm0, xmm1;                        {$ELSE}db $C5,$FB,$58,$C1;{$ENDIF} 

       @buildRes2:

       // build result
       {$IFDEF AVXSUP}vhaddpd xmm0, xmm0, xmm0;                       {$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} 

       // check if we need to use the unbiased version
       cmp unbiased, 0;
       jz @@dobiased;

       lea esi, cLocOne;
       {$IFDEF AVXSUP}vmovsd xmm2, [esi];                             {$ELSE}db $C5,$FB,$10,$16;{$ENDIF} 
       {$IFDEF AVXSUP}vsubsd xmm4, xmm3, xmm2;                        {$ELSE}db $C5,$E3,$5C,$E2;{$ENDIF} 
       {$IFDEF AVXSUP}vmaxsd xmm4, xmm4, xmm2;                        {$ELSE}db $C5,$DB,$5F,$E2;{$ENDIF} 

       {$IFDEF AVXSUP}vdivsd xmm0, xmm0, xmm4;                        {$ELSE}db $C5,$FB,$5E,$C4;{$ENDIF} 

       jmp @@writeRes;

       @@dobiased:
       {$IFDEF AVXSUP}vdivsd xmm0, xmm0, xmm3;                        {$ELSE}db $C5,$FB,$5E,$C3;{$ENDIF} 

       // write result
       @@writeRes:
       {$IFDEF AVXSUP}vmovsd [eax], xmm0;                             {$ELSE}db $C5,$FB,$11,$00;{$ENDIF} 

       // next line:
       add ecx, srcLineWidth;
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

procedure AVXMatrixVarColumnAligned(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; unbiased : boolean);
var tmp : double;
asm
   // prolog
   push ebx;
   push edi;
   push esi;

   // preparations
   mov edi, height;
   {$IFDEF AVXSUP}vcvtsi2sd xmm0, xmm0, edi;                          {$ELSE}db $C5,$FB,$2A,$C7;{$ENDIF} 
   imul edi, srcLineWidth;
   imul edi, -1;

   // helper registers for the mt1, mt2 and dest pointers
   sub ecx, edi;
   lea esi, tmp;
   {$IFDEF AVXSUP}vmovsd [esi], xmm0;                                 {$ELSE}db $C5,$FB,$11,$06;{$ENDIF} 
   {$IFDEF AVXSUP}vbroadcastsd ymm2, [esi];                           {$ELSE}db $C4,$E2,$7D,$19,$16;{$ENDIF} // vbroadcastsd ymm2, xmm0; // avx2

   mov ebx, srcLineWidth;
   lea esi, cLocOne;
   {$IFDEF AVXSUP}vbroadcastsd ymm5, [esi];                           {$ELSE}db $C4,$E2,$7D,$19,$2E;{$ENDIF} 

   // for x := 0 to width - 1:
   mov esi, Width;
   sub esi, 4;
   jl @@addforxloop4end;

   // 4 columns at once
   @@addforxloop4:
       {$IFDEF AVXSUP}vxorpd ymm1, ymm1, ymm1;                        {$ELSE}db $C5,$F5,$57,$C9;{$ENDIF} 

       // for y := 0 to height - 1;
       // prepare for reverse loop
       mov edx, edi;
       @addforyloop4:
           {$IFDEF AVXSUP}vaddpd ymm1, ymm1, [ecx + edx];             {$ELSE}db $C5,$F5,$58,$0C,$11;{$ENDIF} 
       add edx, ebx;
       jnz @addforyloop4;

       // build result
       {$IFDEF AVXSUP}vdivpd ymm0, ymm1, ymm2;                        {$ELSE}db $C5,$F5,$5E,$C2;{$ENDIF} 

       // calculate final standard deviation
       // for y := 0 to height - 1;
       // prepare for reverse loop
       {$IFDEF AVXSUP}vxorpd ymm4, ymm4, ymm4;                        {$ELSE}db $C5,$DD,$57,$E4;{$ENDIF} 
       mov edx, edi;
       @addforyloop4_2:
           {$IFDEF AVXSUP}vmovapd ymm1, [ecx + edx];                  {$ELSE}db $C5,$FD,$28,$0C,$11;{$ENDIF} 
           {$IFDEF AVXSUP}vsubpd ymm1, ymm1, ymm0;                    {$ELSE}db $C5,$F5,$5C,$C8;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm1, ymm1, ymm1;                    {$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm4, ymm4, ymm1;                    {$ELSE}db $C5,$DD,$58,$E1;{$ENDIF} 
       add edx, ebx;
       jnz @addforyloop4_2;

       // check if we need to use the unbiased version
       cmp unbiased, 0;
       jz @@dobiased_4;

       lea edx, cLocOne;

       {$IFDEF AVXSUP}vbroadcastsd ymm1, [edx];                       {$ELSE}db $C4,$E2,$7D,$19,$0A;{$ENDIF} 
       {$IFDEF AVXSUP}vsubpd ymm3, ymm2, ymm1;                        {$ELSE}db $C5,$ED,$5C,$D9;{$ENDIF} 
       {$IFDEF AVXSUP}vmaxpd ymm3, ymm3, ymm1;                        {$ELSE}db $C5,$E5,$5F,$D9;{$ENDIF} 

       {$IFDEF AVXSUP}vdivpd ymm4, ymm4, ymm3;                        {$ELSE}db $C5,$DD,$5E,$E3;{$ENDIF} 

       jmp @@writeRes_4;

       @@dobiased_4:
       {$IFDEF AVXSUP}vdivpd ymm4, ymm4, ymm2;                        {$ELSE}db $C5,$DD,$5E,$E2;{$ENDIF} 

       // write result
       @@writeRes_4:
       {$IFDEF AVXSUP}vmovapd [eax], ymm4;                            {$ELSE}db $C5,$FD,$29,$20;{$ENDIF} 

       // next columns:
       add eax, 32;
       add ecx, 32;

   // loop x end
   sub esi, 4;
   jge @@addforxloop4;

   @@addforxloop4end:

   add esi, 4;
   jz @@endProc;

   sub esi, 2;
   jl @@lastcolumn;

   {$IFDEF AVXSUP}vxorpd xmm1, xmm1, xmm1;                            {$ELSE}db $C5,$F1,$57,$C9;{$ENDIF} 

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov edx, edi;
   @addforyloop2:
       {$IFDEF AVXSUP}vaddpd xmm1, xmm1, [ecx + edx];                 {$ELSE}db $C5,$F1,$58,$0C,$11;{$ENDIF} 
   add edx, ebx;
   jnz @addforyloop2;

   // build result
   {$IFDEF AVXSUP}vdivpd xmm0, xmm1, xmm2;                            {$ELSE}db $C5,$F1,$5E,$C2;{$ENDIF} 

   // calculate final standard deviation
   // for y := 0 to height - 1;
   // prepare for reverse loop
   {$IFDEF AVXSUP}vxorpd xmm4, xmm4, xmm4;                            {$ELSE}db $C5,$D9,$57,$E4;{$ENDIF} 
   mov edx, edi;
   @addforyloop2_2:
      {$IFDEF AVXSUP}vmovapd xmm1, [ecx + edx];                       {$ELSE}db $C5,$F9,$28,$0C,$11;{$ENDIF} 
      {$IFDEF AVXSUP}vsubpd xmm1, xmm1, xmm0;                         {$ELSE}db $C5,$F1,$5C,$C8;{$ENDIF} 
      {$IFDEF AVXSUP}vmulpd xmm1, xmm1, xmm1;                         {$ELSE}db $C5,$F1,$59,$C9;{$ENDIF} 
      {$IFDEF AVXSUP}vaddpd xmm4, xmm4, xmm1;                         {$ELSE}db $C5,$D9,$58,$E1;{$ENDIF} 
   add edx, ebx;
   jnz @addforyloop2_2;

   // check if we need to use the unbiased version
   cmp unbiased, 0;
   jz @@dobiased_2;

   {$IFDEF AVXSUP}vsubpd xmm3, xmm2, xmm5;                            {$ELSE}db $C5,$E9,$5C,$DD;{$ENDIF} 
   {$IFDEF AVXSUP}vmaxpd xmm3, xmm3, xmm5;                            {$ELSE}db $C5,$E1,$5F,$DD;{$ENDIF} 

   {$IFDEF AVXSUP}vdivpd xmm4, xmm4, xmm3;                            {$ELSE}db $C5,$D9,$5E,$E3;{$ENDIF} 

   jmp @@writeRes2;

   @@dobiased_2:
   {$IFDEF AVXSUP}vdivpd xmm4, xmm4, xmm2;                            {$ELSE}db $C5,$D9,$5E,$E2;{$ENDIF} 

   // write result
   @@writeRes2:
   {$IFDEF AVXSUP}vmovapd [eax], xmm4;                                {$ELSE}db $C5,$F9,$29,$20;{$ENDIF} 

   // next columns:
   add eax, 16;
   add ecx, 16;

   dec esi;
   jnz @@endProc;

   @@lastcolumn:

   {$IFDEF AVXSUP}vxorpd xmm1, xmm1, xmm1;                            {$ELSE}db $C5,$F1,$57,$C9;{$ENDIF} 

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov edx, edi;
   @addforyloop_1:
       {$IFDEF AVXSUP}vaddsd xmm1, xmm1, [ecx + edx];                 {$ELSE}db $C5,$F3,$58,$0C,$11;{$ENDIF} 
   add edx, ebx;
   jnz @addforyloop_1;

   // build result
   {$IFDEF AVXSUP}vdivsd xmm0, xmm1, xmm2;                            {$ELSE}db $C5,$F3,$5E,$C2;{$ENDIF} 

   // calculate final standard deviation
   // for y := 0 to height - 1;
   // prepare for reverse loop
   {$IFDEF AVXSUP}vxorpd xmm4, xmm4, xmm4;                            {$ELSE}db $C5,$D9,$57,$E4;{$ENDIF} 
   mov edx, edi;
   @addforyloop1_2:
      {$IFDEF AVXSUP}vmovsd xmm1, [ecx + edx];                        {$ELSE}db $C5,$FB,$10,$0C,$11;{$ENDIF} 
      {$IFDEF AVXSUP}vsubsd xmm1, xmm1, xmm0;                         {$ELSE}db $C5,$F3,$5C,$C8;{$ENDIF} 
      {$IFDEF AVXSUP}vmulsd xmm1, xmm1, xmm1;                         {$ELSE}db $C5,$F3,$59,$C9;{$ENDIF} 
      {$IFDEF AVXSUP}vaddsd xmm4, xmm4, xmm1;                         {$ELSE}db $C5,$DB,$58,$E1;{$ENDIF} 
   add edx, ebx;
   jnz @addforyloop1_2;

   // check if we need to use the unbiased version
   cmp unbiased, 0;
   jz @@dobiased_2;

   {$IFDEF AVXSUP}vmovsd xmm3, xmm3, xmm2;                            {$ELSE}db $C5,$E3,$11,$D3;{$ENDIF} 
   {$IFDEF AVXSUP}vsubsd xmm3, xmm3, xmm5;                            {$ELSE}db $C5,$E3,$5C,$DD;{$ENDIF} 
   {$IFDEF AVXSUP}vmaxsd xmm3, xmm3, xmm5;                            {$ELSE}db $C5,$E3,$5F,$DD;{$ENDIF} 

   {$IFDEF AVXSUP}vdivsd xmm4, xmm4, xmm3;                            {$ELSE}db $C5,$DB,$5E,$E3;{$ENDIF} 

   jmp @@writeRes_1;

   @@dobiased_1:
   {$IFDEF AVXSUP}vdivsd xmm4, xmm4, xmm2;                            {$ELSE}db $C5,$DB,$5E,$E2;{$ENDIF} 

   // write result
   @@writeRes_1:
   {$IFDEF AVXSUP}vmovsd [eax], xmm4;                                 {$ELSE}db $C5,$FB,$11,$20;{$ENDIF} 

   @@endProc:

   // epilog
   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;

procedure AVXMatrixVarColumnUnAligned(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; unbiased : boolean);
var tmp : double;
asm
   // prolog
   push ebx;
   push edi;
   push esi;

   // preparations
   mov edi, height;
   {$IFDEF AVXSUP}vcvtsi2sd xmm0, xmm0, edi;                          {$ELSE}db $C5,$FB,$2A,$C7;{$ENDIF} 
   imul edi, srcLineWidth;
   imul edi, -1;

   // helper registers for the mt1, mt2 and dest pointers
   sub ecx, edi;
   lea esi, tmp;
   {$IFDEF AVXSUP}vmovsd [esi], xmm0;                                 {$ELSE}db $C5,$FB,$11,$06;{$ENDIF} 
   {$IFDEF AVXSUP}vbroadcastsd ymm2, [esi];                           {$ELSE}db $C4,$E2,$7D,$19,$16;{$ENDIF} // vbroadcastsd ymm2, xmm0; // avx2

   mov ebx, srcLineWidth;
   lea esi, cLocOne;
   {$IFDEF AVXSUP}vbroadcastsd ymm5, [esi];                           {$ELSE}db $C4,$E2,$7D,$19,$2E;{$ENDIF} 

   // for x := 0 to width - 1:
   mov esi, Width;
   sub esi, 4;
   jl @@addforxloop4end;

   // 4 columns at once
   @@addforxloop4:
       {$IFDEF AVXSUP}vxorpd ymm1, ymm1, ymm1;                        {$ELSE}db $C5,$F5,$57,$C9;{$ENDIF} 

       // for y := 0 to height - 1;
       // prepare for reverse loop
       mov edx, edi;
       @addforyloop4:
           {$IFDEF AVXSUP}vmovupd ymm3, [ecx + edx];                  {$ELSE}db $C5,$FD,$10,$1C,$11;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm1, ymm1, ymm3;                    {$ELSE}db $C5,$F5,$58,$CB;{$ENDIF} 
       add edx, ebx;
       jnz @addforyloop4;

       // build result
       {$IFDEF AVXSUP}vdivpd ymm0, ymm1, ymm2;                        {$ELSE}db $C5,$F5,$5E,$C2;{$ENDIF} 

       // calculate final standard deviation
       // for y := 0 to height - 1;
       // prepare for reverse loop
       {$IFDEF AVXSUP}vxorpd ymm4, ymm4, ymm4;                        {$ELSE}db $C5,$DD,$57,$E4;{$ENDIF} 
       mov edx, edi;
       @addforyloop4_2:
           {$IFDEF AVXSUP}vmovupd ymm1, [ecx + edx];                  {$ELSE}db $C5,$FD,$10,$0C,$11;{$ENDIF} 
           {$IFDEF AVXSUP}vsubpd ymm1, ymm1, ymm0;                    {$ELSE}db $C5,$F5,$5C,$C8;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm1, ymm1, ymm1;                    {$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm4, ymm4, ymm1;                    {$ELSE}db $C5,$DD,$58,$E1;{$ENDIF} 
       add edx, ebx;
       jnz @addforyloop4_2;

       // check if we need to use the unbiased version
       cmp unbiased, 0;
       jz @@dobiased_4;

       lea edx, cLocOne;

       {$IFDEF AVXSUP}vbroadcastsd ymm1, [edx];                       {$ELSE}db $C4,$E2,$7D,$19,$0A;{$ENDIF} 
       {$IFDEF AVXSUP}vsubpd ymm3, ymm2, ymm1;                        {$ELSE}db $C5,$ED,$5C,$D9;{$ENDIF} 
       {$IFDEF AVXSUP}vmaxpd ymm3, ymm3, ymm1;                        {$ELSE}db $C5,$E5,$5F,$D9;{$ENDIF} 

       {$IFDEF AVXSUP}vdivpd ymm4, ymm4, ymm3;                        {$ELSE}db $C5,$DD,$5E,$E3;{$ENDIF} 

       jmp @@writeRes_4;

       @@dobiased_4:
       {$IFDEF AVXSUP}vdivpd ymm4, ymm4, ymm2;                        {$ELSE}db $C5,$DD,$5E,$E2;{$ENDIF} 

       // write result
       @@writeRes_4:
       {$IFDEF AVXSUP}vmovupd [eax], ymm4;                            {$ELSE}db $C5,$FD,$11,$20;{$ENDIF} 

       // next columns:
       add eax, 32;
       add ecx, 32;

   // loop x end
   sub esi, 4;
   jge @@addforxloop4;

   @@addforxloop4end:

   add esi, 4;
   jz @@endProc;

   sub esi, 2;
   jl @@lastcolumn;

   {$IFDEF AVXSUP}vxorpd xmm1, xmm1, xmm1;                            {$ELSE}db $C5,$F1,$57,$C9;{$ENDIF} 

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov edx, edi;
   @addforyloop2:
       {$IFDEF AVXSUP}vmovupd xmm3, [ecx + edx];                      {$ELSE}db $C5,$F9,$10,$1C,$11;{$ENDIF} 
       {$IFDEF AVXSUP}vaddpd xmm1, xmm1, xmm3;                        {$ELSE}db $C5,$F1,$58,$CB;{$ENDIF} 
   add edx, ebx;
   jnz @addforyloop2;

   // build result
   {$IFDEF AVXSUP}vdivpd xmm0, xmm1, xmm2;                            {$ELSE}db $C5,$F1,$5E,$C2;{$ENDIF} 

   // calculate final standard deviation
   // for y := 0 to height - 1;
   // prepare for reverse loop
   {$IFDEF AVXSUP}vxorpd xmm4, xmm4, xmm4;                            {$ELSE}db $C5,$D9,$57,$E4;{$ENDIF} 
   mov edx, edi;
   @addforyloop2_2:
      {$IFDEF AVXSUP}vmovupd xmm1, [ecx + edx];                       {$ELSE}db $C5,$F9,$10,$0C,$11;{$ENDIF} 
      {$IFDEF AVXSUP}vsubpd xmm1, xmm1, xmm0;                         {$ELSE}db $C5,$F1,$5C,$C8;{$ENDIF} 
      {$IFDEF AVXSUP}vmulpd xmm1, xmm1, xmm1;                         {$ELSE}db $C5,$F1,$59,$C9;{$ENDIF} 
      {$IFDEF AVXSUP}vaddpd xmm4, xmm4, xmm1;                         {$ELSE}db $C5,$D9,$58,$E1;{$ENDIF} 
   add edx, ebx;
   jnz @addforyloop2_2;

   // check if we need to use the unbiased version
   cmp unbiased, 0;
   jz @@dobiased_2;

   {$IFDEF AVXSUP}vsubpd xmm3, xmm2, xmm5;                            {$ELSE}db $C5,$E9,$5C,$DD;{$ENDIF} 
   {$IFDEF AVXSUP}vmaxpd xmm3, xmm3, xmm5;                            {$ELSE}db $C5,$E1,$5F,$DD;{$ENDIF} 

   {$IFDEF AVXSUP}vdivpd xmm4, xmm4, xmm3;                            {$ELSE}db $C5,$D9,$5E,$E3;{$ENDIF} 

   jmp @@writeRes2;

   @@dobiased_2:
   {$IFDEF AVXSUP}vdivpd xmm4, xmm4, xmm2;                            {$ELSE}db $C5,$D9,$5E,$E2;{$ENDIF} 

   // write result
   @@writeRes2:
   {$IFDEF AVXSUP}vmovapd [eax], xmm4;                                {$ELSE}db $C5,$F9,$29,$20;{$ENDIF} 

   // next columns:
   add eax, 16;
   add ecx, 16;

   dec esi;
   jnz @@endProc;

   @@lastcolumn:

   {$IFDEF AVXSUP}vxorpd xmm1, xmm1, xmm1;                            {$ELSE}db $C5,$F1,$57,$C9;{$ENDIF} 

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov edx, edi;
   @addforyloop_1:
       {$IFDEF AVXSUP}vaddsd xmm1, xmm1, [ecx + edx];                 {$ELSE}db $C5,$F3,$58,$0C,$11;{$ENDIF} 
   add edx, ebx;
   jnz @addforyloop_1;

   // build result
   {$IFDEF AVXSUP}vdivsd xmm0, xmm1, xmm2;                            {$ELSE}db $C5,$F3,$5E,$C2;{$ENDIF} 

   // calculate final standard deviation
   // for y := 0 to height - 1;
   // prepare for reverse loop
   {$IFDEF AVXSUP}vxorpd xmm4, xmm4, xmm4;                            {$ELSE}db $C5,$D9,$57,$E4;{$ENDIF} 
   mov edx, edi;
   @addforyloop1_2:
      {$IFDEF AVXSUP}vmovsd xmm1, [ecx + edx];                        {$ELSE}db $C5,$FB,$10,$0C,$11;{$ENDIF} 
      {$IFDEF AVXSUP}vsubsd xmm1, xmm1, xmm0;                         {$ELSE}db $C5,$F3,$5C,$C8;{$ENDIF} 
      {$IFDEF AVXSUP}vmulsd xmm1, xmm1, xmm1;                         {$ELSE}db $C5,$F3,$59,$C9;{$ENDIF} 
      {$IFDEF AVXSUP}vaddsd xmm4, xmm4, xmm1;                         {$ELSE}db $C5,$DB,$58,$E1;{$ENDIF} 
   add edx, ebx;
   jnz @addforyloop1_2;

   // check if we need to use the unbiased version
   cmp unbiased, 0;
   jz @@dobiased_2;

   {$IFDEF AVXSUP}vmovsd xmm3, xmm3, xmm2;                            {$ELSE}db $C5,$E3,$11,$D3;{$ENDIF} 
   {$IFDEF AVXSUP}vsubsd xmm3, xmm3, xmm5;                            {$ELSE}db $C5,$E3,$5C,$DD;{$ENDIF} 
   {$IFDEF AVXSUP}vmaxsd xmm3, xmm3, xmm5;                            {$ELSE}db $C5,$E3,$5F,$DD;{$ENDIF} 

   {$IFDEF AVXSUP}vdivsd xmm4, xmm4, xmm3;                            {$ELSE}db $C5,$DB,$5E,$E3;{$ENDIF} 

   jmp @@writeRes_1;

   @@dobiased_1:
   {$IFDEF AVXSUP}vdivsd xmm4, xmm4, xmm2;                            {$ELSE}db $C5,$DB,$5E,$E2;{$ENDIF} 

   // write result
   @@writeRes_1:
   {$IFDEF AVXSUP}vmovsd [eax], xmm4;                                 {$ELSE}db $C5,$FB,$11,$20;{$ENDIF} 

   @@endProc:

   // epilog
   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;

// #####################################################
// #### Combined mean variance calculation
// #####################################################

procedure AVXMatrixMeanVarRowAligned(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; unbiased : boolean);
var tmp : double;
    aDestLineWidth : NativeInt;
asm
   // prolog
   push ebx;
   push edi;
   push esi;

   mov aDestLineWidth, edx;

   // iters := -width*sizeof(double)
   mov edi, width;
   {$IFDEF AVXSUP}vcvtsi2sd xmm3, xmm3, edi;                          {$ELSE}db $C5,$E3,$2A,$DF;{$ENDIF} 
   imul edi, -8;

   // helper registers for the src and dest pointers
   sub ecx, edi;

   // for y := 0 to height - 1:
   mov edx, Height;
   @@addforyloop:
       {$IFDEF AVXSUP}vxorpd ymm0, ymm0, ymm0;                        {$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} 
       {$IFDEF AVXSUP}vxorpd ymm1, ymm1, ymm1;                        {$ELSE}db $C5,$F5,$57,$C9;{$ENDIF} 

       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov esi, edi;
       @addforxloop:
           add esi, 128;
           jg @loopEnd;
           // prefetch data...
           // prefetch [r8 + esi];

           // addition:
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, [ecx + esi - 128];       {$ELSE}db $C5,$FD,$58,$44,$31,$80;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm1, ymm1, [ecx + esi - 96];        {$ELSE}db $C5,$F5,$58,$4C,$31,$A0;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, [ecx + esi - 64];        {$ELSE}db $C5,$FD,$58,$44,$31,$C0;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm1, ymm1, [ecx + esi - 32];        {$ELSE}db $C5,$F5,$58,$4C,$31,$E0;{$ENDIF} 
       jmp @addforxloop

       @loopEnd:

       {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm1;                        {$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 
       {$IFDEF AVXSUP}vextractf128 xmm2, ymm0, 1;                     {$ELSE}db $C4,$E3,$7D,$19,$C2,$01;{$ENDIF} 
       {$IFDEF AVXSUP}vhaddpd xmm0, xmm0, xmm2;                       {$ELSE}db $C5,$F9,$7C,$C2;{$ENDIF} 

       sub esi, 128;

       jz @buildRes;

       @addforxloop2:
           add esi, 16;
           jg @@addforxloop2end;

           {$IFDEF AVXSUP}vaddpd xmm0, xmm0, [ecx + esi - 16];        {$ELSE}db $C5,$F9,$58,$44,$31,$F0;{$ENDIF} 
       jmp @addforxloop2;

       @@addforxloop2end:

       sub esi, 16;
       jz @buildRes;

       {$IFDEF AVXSUP}vaddsd xmm0, xmm0, [ecx + esi];                 {$ELSE}db $C5,$FB,$58,$04,$31;{$ENDIF} 

       @buildRes:

       // build result
       {$IFDEF AVXSUP}vhaddpd xmm0, xmm0, xmm0;                       {$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} 
       {$IFDEF AVXSUP}vdivsd xmm0, xmm0, xmm3;                        {$ELSE}db $C5,$FB,$5E,$C3;{$ENDIF} 
       {$IFDEF AVXSUP}vmovsd [eax], xmm0;                             {$ELSE}db $C5,$FB,$11,$00;{$ENDIF} 

       // we have calculated the mean ->
       // repeat the loop to calculate the variance

       lea esi, tmp;
       {$IFDEF AVXSUP}vmovsd [esi], xmm0;                             {$ELSE}db $C5,$FB,$11,$06;{$ENDIF} 
       {$IFDEF AVXSUP}vbroadcastsd ymm4, [esi];                       {$ELSE}db $C4,$E2,$7D,$19,$26;{$ENDIF} 
       // vbroadcastsd ymm4, xmm0; // avx2 instruction
       {$IFDEF AVXSUP}vxorpd xmm0, xmm0, xmm0;                        {$ELSE}db $C5,$F9,$57,$C0;{$ENDIF} 

       // for x := 0 to w - 1;
       // prepare for reverse loop
       // variance = sum (x - mean)^2
       mov esi, edi;
       @addforxloop3:
           add esi, 128;
           jg @loopEnd2;
           // prefetch data...
           // prefetch [eax + esi];

           // addition:
           {$IFDEF AVXSUP}vmovapd ymm1, [ecx + esi - 128];            {$ELSE}db $C5,$FD,$28,$4C,$31,$80;{$ENDIF} 
           {$IFDEF AVXSUP}vsubpd ymm1, ymm1, ymm4;                    {$ELSE}db $C5,$F5,$5C,$CC;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm1, ymm1, ymm1;                    {$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm1;                    {$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 

           {$IFDEF AVXSUP}vmovapd ymm1, [ecx + esi - 96];             {$ELSE}db $C5,$FD,$28,$4C,$31,$A0;{$ENDIF} 
           {$IFDEF AVXSUP}vsubpd ymm1, ymm1, ymm4;                    {$ELSE}db $C5,$F5,$5C,$CC;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm1, ymm1, ymm1;                    {$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm1;                    {$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 

           {$IFDEF AVXSUP}vmovapd ymm1, [ecx + esi - 64];             {$ELSE}db $C5,$FD,$28,$4C,$31,$C0;{$ENDIF} 
           {$IFDEF AVXSUP}vsubpd ymm1, ymm1, ymm4;                    {$ELSE}db $C5,$F5,$5C,$CC;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm1, ymm1, ymm1;                    {$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm1;                    {$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 

           {$IFDEF AVXSUP}vmovapd ymm1, [ecx + esi - 32];             {$ELSE}db $C5,$FD,$28,$4C,$31,$E0;{$ENDIF} 
           {$IFDEF AVXSUP}vsubpd ymm1, ymm1, ymm4;                    {$ELSE}db $C5,$F5,$5C,$CC;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm1, ymm1, ymm1;                    {$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm1;                    {$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 

       jmp @addforxloop3

       @loopEnd2:

       sub esi, 128;

       {$IFDEF AVXSUP}vextractf128 xmm2, ymm0, 1;                     {$ELSE}db $C4,$E3,$7D,$19,$C2,$01;{$ENDIF} 
       {$IFDEF AVXSUP}vhaddpd xmm0, xmm0, xmm2;                       {$ELSE}db $C5,$F9,$7C,$C2;{$ENDIF} 

       jz @buildRes2;

       @addforxloop4:
           add esi, 16;
           jg @addforxloop4end;

           {$IFDEF AVXSUP}vmovapd xmm1, [ecx + esi - 16];             {$ELSE}db $C5,$F9,$28,$4C,$31,$F0;{$ENDIF} 
           {$IFDEF AVXSUP}vsubpd xmm1, xmm1, xmm4;                    {$ELSE}db $C5,$F1,$5C,$CC;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd xmm1, xmm1, xmm1;                    {$ELSE}db $C5,$F1,$59,$C9;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd xmm0, xmm0, xmm1;                    {$ELSE}db $C5,$F9,$58,$C1;{$ENDIF} 
       jmp @addforxloop4;

       @addforxloop4end:

       sub esi, 16;
       jz @buildRes2;

       // last column
       {$IFDEF AVXSUP}vmovsd xmm1, [ecx + esi];                       {$ELSE}db $C5,$FB,$10,$0C,$31;{$ENDIF} 
       {$IFDEF AVXSUP}vsubsd xmm1, xmm1, xmm4;                        {$ELSE}db $C5,$F3,$5C,$CC;{$ENDIF} 
       {$IFDEF AVXSUP}vmulsd xmm1, xmm1, xmm1;                        {$ELSE}db $C5,$F3,$59,$C9;{$ENDIF} 
       {$IFDEF AVXSUP}vaddsd xmm0, xmm0, xmm1;                        {$ELSE}db $C5,$FB,$58,$C1;{$ENDIF} 

       @buildRes2:

       // build result
       {$IFDEF AVXSUP}vhaddpd xmm0, xmm0, xmm0;                       {$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} 

       // check if we need to use the unbiased version
       cmp unbiased, 0;
       jz @@dobiased;

       lea esi, cLocOne;
       {$IFDEF AVXSUP}vmovsd xmm2, [esi];                             {$ELSE}db $C5,$FB,$10,$16;{$ENDIF} 
       {$IFDEF AVXSUP}vsubsd xmm4, xmm3, xmm2;                        {$ELSE}db $C5,$E3,$5C,$E2;{$ENDIF} 
       {$IFDEF AVXSUP}vmaxsd xmm4, xmm4, xmm2;                        {$ELSE}db $C5,$DB,$5F,$E2;{$ENDIF} 

       {$IFDEF AVXSUP}vdivsd xmm0, xmm0, xmm4;                        {$ELSE}db $C5,$FB,$5E,$C4;{$ENDIF} 

       jmp @@writeRes;

       @@dobiased:
       {$IFDEF AVXSUP}vdivsd xmm0, xmm0, xmm3;                        {$ELSE}db $C5,$FB,$5E,$C3;{$ENDIF} 

       // write result
       @@writeRes:
       {$IFDEF AVXSUP}vmovsd [eax + 8], xmm0;                         {$ELSE}db $C5,$FB,$11,$40,$08;{$ENDIF} 

       // next line:
       add ecx, srcLineWidth;
       add eax, adestLineWidth;

   // loop y end
   dec edx;
   jnz @@addforyloop;

   // epilog
   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;

procedure AVXMatrixMeanVarRowUnAligned(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; unbiased : boolean);
var tmp : double;
    aDestLineWidth : NativeInt;
asm
   // prolog
   push ebx;
   push edi;
   push esi;

   mov aDestLineWidth, edx;

   // iters := -width*sizeof(double)
   mov edi, width;
   {$IFDEF AVXSUP}vcvtsi2sd xmm3, xmm3, edi;                          {$ELSE}db $C5,$E3,$2A,$DF;{$ENDIF} 
   imul edi, -8;

   // helper registers for the src and dest pointers
   sub ecx, edi;

   // for y := 0 to height - 1:
   mov edx, Height;
   @@addforyloop:
       {$IFDEF AVXSUP}vxorpd ymm0, ymm0, ymm0;                        {$ELSE}db $C5,$FD,$57,$C0;{$ENDIF} 
       {$IFDEF AVXSUP}vxorpd ymm1, ymm1, ymm1;                        {$ELSE}db $C5,$F5,$57,$C9;{$ENDIF} 

       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov esi, edi;
       @addforxloop:
           add esi, 128;
           jg @loopEnd;
           // prefetch data...
           // prefetch [r8 + esi];

           // addition:
           {$IFDEF AVXSUP}vmovupd ymm2, [ecx + esi - 128];            {$ELSE}db $C5,$FD,$10,$54,$31,$80;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm2;                    {$ELSE}db $C5,$FD,$58,$C2;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd ymm4, [ecx + esi - 96];             {$ELSE}db $C5,$FD,$10,$64,$31,$A0;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm1, ymm1, ymm4;                    {$ELSE}db $C5,$F5,$58,$CC;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd ymm2, [ecx + esi - 64];             {$ELSE}db $C5,$FD,$10,$54,$31,$C0;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm2;                    {$ELSE}db $C5,$FD,$58,$C2;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd ymm4, [ecx + esi - 32];             {$ELSE}db $C5,$FD,$10,$64,$31,$E0;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm1, ymm1, ymm4;                    {$ELSE}db $C5,$F5,$58,$CC;{$ENDIF} 
       jmp @addforxloop

       @loopEnd:

       {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm1;                        {$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 
       {$IFDEF AVXSUP}vextractf128 xmm2, ymm0, 1;                     {$ELSE}db $C4,$E3,$7D,$19,$C2,$01;{$ENDIF} 
       {$IFDEF AVXSUP}vhaddpd xmm0, xmm0, xmm2;                       {$ELSE}db $C5,$F9,$7C,$C2;{$ENDIF} 

       sub esi, 128;

       jz @buildRes;

       @addforxloop2:
           add esi, 16;
           jg @@addforxloop2end;

           {$IFDEF AVXSUP}vmovupd xmm2, [ecx + esi - 16];             {$ELSE}db $C5,$F9,$10,$54,$31,$F0;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd xmm0, xmm0, xmm2;                    {$ELSE}db $C5,$F9,$58,$C2;{$ENDIF} 
       jmp @addforxloop2;

       @@addforxloop2end:

       sub esi, 16;
       jz @buildRes;

       {$IFDEF AVXSUP}vaddsd xmm0, xmm0, [ecx + esi];                 {$ELSE}db $C5,$FB,$58,$04,$31;{$ENDIF} 

       @buildRes:

       // build result
       {$IFDEF AVXSUP}vhaddpd xmm0, xmm0, xmm0;                       {$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} 
       {$IFDEF AVXSUP}vdivsd xmm0, xmm0, xmm3;                        {$ELSE}db $C5,$FB,$5E,$C3;{$ENDIF} 
       {$IFDEF AVXSUP}vmovsd [eax], xmm0;                             {$ELSE}db $C5,$FB,$11,$00;{$ENDIF} 

       // we have calculated the mean ->
       // repeat the loop to calculate the variance

       lea esi, tmp;
       {$IFDEF AVXSUP}vmovsd [esi], xmm0;                             {$ELSE}db $C5,$FB,$11,$06;{$ENDIF} 
       {$IFDEF AVXSUP}vbroadcastsd ymm4, [esi];                       {$ELSE}db $C4,$E2,$7D,$19,$26;{$ENDIF} 
       // vbroadcastsd ymm4, xmm0; // avx2 instruction
       {$IFDEF AVXSUP}vxorpd xmm0, xmm0, xmm0;                        {$ELSE}db $C5,$F9,$57,$C0;{$ENDIF} 

       // for x := 0 to w - 1;
       // prepare for reverse loop
       // variance = sum (x - mean)^2
       mov esi, edi;
       @addforxloop3:
           add esi, 128;
           jg @loopEnd2;
           // prefetch data...
           // prefetch [eax + esi];

           // addition:
           {$IFDEF AVXSUP}vmovupd ymm1, [ecx + esi - 128];            {$ELSE}db $C5,$FD,$10,$4C,$31,$80;{$ENDIF} 
           {$IFDEF AVXSUP}vsubpd ymm1, ymm1, ymm4;                    {$ELSE}db $C5,$F5,$5C,$CC;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm1, ymm1, ymm1;                    {$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm1;                    {$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd ymm1, [ecx + esi - 96];             {$ELSE}db $C5,$FD,$10,$4C,$31,$A0;{$ENDIF} 
           {$IFDEF AVXSUP}vsubpd ymm1, ymm1, ymm4;                    {$ELSE}db $C5,$F5,$5C,$CC;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm1, ymm1, ymm1;                    {$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm1;                    {$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd ymm1, [ecx + esi - 64];             {$ELSE}db $C5,$FD,$10,$4C,$31,$C0;{$ENDIF} 
           {$IFDEF AVXSUP}vsubpd ymm1, ymm1, ymm4;                    {$ELSE}db $C5,$F5,$5C,$CC;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm1, ymm1, ymm1;                    {$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm1;                    {$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd ymm1, [ecx + esi - 32];             {$ELSE}db $C5,$FD,$10,$4C,$31,$E0;{$ENDIF} 
           {$IFDEF AVXSUP}vsubpd ymm1, ymm1, ymm4;                    {$ELSE}db $C5,$F5,$5C,$CC;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm1, ymm1, ymm1;                    {$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm0, ymm0, ymm1;                    {$ELSE}db $C5,$FD,$58,$C1;{$ENDIF} 

       jmp @addforxloop3

       @loopEnd2:

       sub esi, 128;

       {$IFDEF AVXSUP}vextractf128 xmm2, ymm0, 1;                     {$ELSE}db $C4,$E3,$7D,$19,$C2,$01;{$ENDIF} 
       {$IFDEF AVXSUP}vhaddpd xmm0, xmm0, xmm2;                       {$ELSE}db $C5,$F9,$7C,$C2;{$ENDIF} 

       jz @buildRes2;

       @addforxloop4:
           add esi, 16;
           jg @addforxloop4end;

           {$IFDEF AVXSUP}vmovupd xmm1, [ecx + esi - 16];             {$ELSE}db $C5,$F9,$10,$4C,$31,$F0;{$ENDIF} 
           {$IFDEF AVXSUP}vsubpd xmm1, xmm1, xmm4;                    {$ELSE}db $C5,$F1,$5C,$CC;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd xmm1, xmm1, xmm1;                    {$ELSE}db $C5,$F1,$59,$C9;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd xmm0, xmm0, xmm1;                    {$ELSE}db $C5,$F9,$58,$C1;{$ENDIF} 
       jmp @addforxloop4;

       @addforxloop4end:

       sub esi, 16;
       jz @buildRes2;

       // last column
       {$IFDEF AVXSUP}vmovsd xmm1, [ecx + esi];                       {$ELSE}db $C5,$FB,$10,$0C,$31;{$ENDIF} 
       {$IFDEF AVXSUP}vsubsd xmm1, xmm1, xmm4;                        {$ELSE}db $C5,$F3,$5C,$CC;{$ENDIF} 
       {$IFDEF AVXSUP}vmulsd xmm1, xmm1, xmm1;                        {$ELSE}db $C5,$F3,$59,$C9;{$ENDIF} 
       {$IFDEF AVXSUP}vaddsd xmm0, xmm0, xmm1;                        {$ELSE}db $C5,$FB,$58,$C1;{$ENDIF} 

       @buildRes2:

       // build result
       {$IFDEF AVXSUP}vhaddpd xmm0, xmm0, xmm0;                       {$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} 

       // check if we need to use the unbiased version
       cmp unbiased, 0;
       jz @@dobiased;

       lea esi, cLocOne;
       {$IFDEF AVXSUP}vmovsd xmm2, [esi];                             {$ELSE}db $C5,$FB,$10,$16;{$ENDIF} 
       {$IFDEF AVXSUP}vsubsd xmm4, xmm3, xmm2;                        {$ELSE}db $C5,$E3,$5C,$E2;{$ENDIF} 
       {$IFDEF AVXSUP}vmaxsd xmm4, xmm4, xmm2;                        {$ELSE}db $C5,$DB,$5F,$E2;{$ENDIF} 

       {$IFDEF AVXSUP}vdivsd xmm0, xmm0, xmm4;                        {$ELSE}db $C5,$FB,$5E,$C4;{$ENDIF} 

       jmp @@writeRes;

       @@dobiased:
       {$IFDEF AVXSUP}vdivsd xmm0, xmm0, xmm3;                        {$ELSE}db $C5,$FB,$5E,$C3;{$ENDIF} 

       // write result
       @@writeRes:
       {$IFDEF AVXSUP}vmovsd [eax + 8], xmm0;                         {$ELSE}db $C5,$FB,$11,$40,$08;{$ENDIF} 

       // next line:
       add ecx, srcLineWidth;
       add eax, adestLineWidth;

   // loop y end
   dec edx;
   jnz @@addforyloop;

   // epilog
   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;

procedure AVXMatrixMeanVarColumnAligned(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; unbiased : boolean);
var tmp : double;
    aDestLineWidth : NativeInt;
asm
   // prolog
   push ebx;
   push edi;
   push esi;

   mov aDestLineWidth, edx;

   // preparations
   mov edi, height;
   {$IFDEF AVXSUP}vcvtsi2sd xmm0, xmm0, edi;                          {$ELSE}db $C5,$FB,$2A,$C7;{$ENDIF} 
   imul edi, srcLineWidth;
   imul edi, -1;

   // helper registers for the mt1, mt2 and dest pointers
   sub ecx, edi;

   mov ebx, srcLineWidth;

   lea edx, tmp;
   {$IFDEF AVXSUP}vmovsd [edx], xmm0;                                 {$ELSE}db $C5,$FB,$11,$02;{$ENDIF} 
   {$IFDEF AVXSUP}vbroadcastsd ymm2, [edx];                           {$ELSE}db $C4,$E2,$7D,$19,$12;{$ENDIF} // vbroadcastsd ymm2, xmm0; // avx2
   

   lea edx, cLocOne;
   {$IFDEF AVXSUP}vbroadcastsd ymm5, [edx];                           {$ELSE}db $C4,$E2,$7D,$19,$2A;{$ENDIF} 

   // for x := 0 to width - 1:
   mov esi, Width;
   sub esi, 4;
   jl @@addforxloop4end;

   // 4 columns at once
   @@addforxloop4:
       {$IFDEF AVXSUP}vxorpd ymm1, ymm1, ymm1;                        {$ELSE}db $C5,$F5,$57,$C9;{$ENDIF} 

       // for y := 0 to height - 1;
       // prepare for reverse loop
       mov edx, edi;
       @addforyloop4:
           {$IFDEF AVXSUP}vaddpd ymm1, ymm1, [ecx + edx];             {$ELSE}db $C5,$F5,$58,$0C,$11;{$ENDIF} 
       add edx, ebx;
       jnz @addforyloop4;

       // build result
       {$IFDEF AVXSUP}vdivpd ymm0, ymm1, ymm2;                        {$ELSE}db $C5,$F5,$5E,$C2;{$ENDIF} 
       {$IFDEF AVXSUP}vmovapd [eax], ymm0;                            {$ELSE}db $C5,$FD,$29,$00;{$ENDIF} 

       // calculate final standard deviation
       // for y := 0 to height - 1;
       // prepare for reverse loop
       {$IFDEF AVXSUP}vxorpd ymm4, ymm4, ymm4;                        {$ELSE}db $C5,$DD,$57,$E4;{$ENDIF} 
       mov edx, edi;
       @addforyloop4_2:
           {$IFDEF AVXSUP}vmovapd ymm1, [ecx + edx];                  {$ELSE}db $C5,$FD,$28,$0C,$11;{$ENDIF} 
           {$IFDEF AVXSUP}vsubpd ymm1, ymm1, ymm0;                    {$ELSE}db $C5,$F5,$5C,$C8;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm1, ymm1, ymm1;                    {$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm4, ymm4, ymm1;                    {$ELSE}db $C5,$DD,$58,$E1;{$ENDIF} 
       add edx, ebx;
       jnz @addforyloop4_2;

       // check if we need to use the unbiased version
       cmp unbiased, 0;
       jz @@dobiased_4;

       lea edx, cLocOne;

       {$IFDEF AVXSUP}vbroadcastsd ymm1, [edx];                       {$ELSE}db $C4,$E2,$7D,$19,$0A;{$ENDIF} 
       {$IFDEF AVXSUP}vsubpd ymm3, ymm2, ymm1;                        {$ELSE}db $C5,$ED,$5C,$D9;{$ENDIF} 
       {$IFDEF AVXSUP}vmaxpd ymm3, ymm3, ymm1;                        {$ELSE}db $C5,$E5,$5F,$D9;{$ENDIF} 

       {$IFDEF AVXSUP}vdivpd ymm4, ymm4, ymm3;                        {$ELSE}db $C5,$DD,$5E,$E3;{$ENDIF} 

       jmp @@writeRes_4;

       @@dobiased_4:
       {$IFDEF AVXSUP}vdivpd ymm4, ymm4, ymm2;                        {$ELSE}db $C5,$DD,$5E,$E2;{$ENDIF} 

       // write result
       @@writeRes_4:
       mov edx, adestLineWidth;
       {$IFDEF AVXSUP}vmovapd [eax + edx], ymm4;                      {$ELSE}db $C5,$FD,$29,$24,$10;{$ENDIF} 

       // next columns:
       add eax, 32;
       add ecx, 32;

   // loop x end
   sub esi, 4;
   jge @@addforxloop4;

   @@addforxloop4end:

   add esi, 4;
   jz @@endProc;

   sub esi, 2;
   jl @@lastcolumn;

   {$IFDEF AVXSUP}vxorpd xmm1, xmm1, xmm1;                            {$ELSE}db $C5,$F1,$57,$C9;{$ENDIF} 

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov edx, edi;
   @addforyloop2:
       {$IFDEF AVXSUP}vaddpd xmm1, xmm1, [ecx + edx];                 {$ELSE}db $C5,$F1,$58,$0C,$11;{$ENDIF} 
   add edx, ebx;
   jnz @addforyloop2;

   // build result
   {$IFDEF AVXSUP}vdivpd xmm0, xmm1, xmm2;                            {$ELSE}db $C5,$F1,$5E,$C2;{$ENDIF} 
   {$IFDEF AVXSUP}vmovapd [eax], xmm0;                                {$ELSE}db $C5,$F9,$29,$00;{$ENDIF} 

   // calculate final standard deviation
   // for y := 0 to height - 1;
   // prepare for reverse loop
   {$IFDEF AVXSUP}vxorpd xmm4, xmm4, xmm4;                            {$ELSE}db $C5,$D9,$57,$E4;{$ENDIF} 
   mov edx, edi;
   @addforyloop2_2:
      {$IFDEF AVXSUP}vmovapd xmm1, [ecx + edx];                       {$ELSE}db $C5,$F9,$28,$0C,$11;{$ENDIF} 
      {$IFDEF AVXSUP}vsubpd xmm1, xmm1, xmm0;                         {$ELSE}db $C5,$F1,$5C,$C8;{$ENDIF} 
      {$IFDEF AVXSUP}vmulpd xmm1, xmm1, xmm1;                         {$ELSE}db $C5,$F1,$59,$C9;{$ENDIF} 
      {$IFDEF AVXSUP}vaddpd xmm4, xmm4, xmm1;                         {$ELSE}db $C5,$D9,$58,$E1;{$ENDIF} 
   add edx, ebx;
   jnz @addforyloop2_2;

   // check if we need to use the unbiased version
   cmp unbiased, 0;
   jz @@dobiased_2;

   {$IFDEF AVXSUP}vsubpd xmm3, xmm2, xmm5;                            {$ELSE}db $C5,$E9,$5C,$DD;{$ENDIF} 
   {$IFDEF AVXSUP}vmaxpd xmm3, xmm3, xmm5;                            {$ELSE}db $C5,$E1,$5F,$DD;{$ENDIF} 

   {$IFDEF AVXSUP}vdivpd xmm4, xmm4, xmm3;                            {$ELSE}db $C5,$D9,$5E,$E3;{$ENDIF} 

   jmp @@writeRes2;

   @@dobiased_2:
   {$IFDEF AVXSUP}vdivpd xmm4, xmm4, xmm2;                            {$ELSE}db $C5,$D9,$5E,$E2;{$ENDIF} 

   // write result
   @@writeRes2:
   mov edx, adestLineWidth;
   {$IFDEF AVXSUP}vmovapd [eax + edx], xmm4;                          {$ELSE}db $C5,$F9,$29,$24,$10;{$ENDIF} 

   // next columns:
   add eax, 16;
   add ecx, 16;

   dec esi;
   jnz @@endProc;

   @@lastcolumn:

   {$IFDEF AVXSUP}vxorpd xmm1, xmm1, xmm1;                            {$ELSE}db $C5,$F1,$57,$C9;{$ENDIF} 

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov edx, edi;
   @addforyloop_1:
       {$IFDEF AVXSUP}vaddsd xmm1, xmm1, [ecx + edx];                 {$ELSE}db $C5,$F3,$58,$0C,$11;{$ENDIF} 
   add edx, ebx;
   jnz @addforyloop_1;

   // build result
   {$IFDEF AVXSUP}vdivsd xmm0, xmm1, xmm2;                            {$ELSE}db $C5,$F3,$5E,$C2;{$ENDIF} 
   {$IFDEF AVXSUP}vmovsd [eax], xmm0;                                 {$ELSE}db $C5,$FB,$11,$00;{$ENDIF} 

   // calculate final standard deviation
   // for y := 0 to height - 1;
   // prepare for reverse loop
   {$IFDEF AVXSUP}vxorpd xmm4, xmm4, xmm4;                            {$ELSE}db $C5,$D9,$57,$E4;{$ENDIF} 
   mov edx, edi;
   @addforyloop1_2:
      {$IFDEF AVXSUP}vmovsd xmm1, [ecx + edx];                        {$ELSE}db $C5,$FB,$10,$0C,$11;{$ENDIF} 
      {$IFDEF AVXSUP}vsubsd xmm1, xmm1, xmm0;                         {$ELSE}db $C5,$F3,$5C,$C8;{$ENDIF} 
      {$IFDEF AVXSUP}vmulsd xmm1, xmm1, xmm1;                         {$ELSE}db $C5,$F3,$59,$C9;{$ENDIF} 
      {$IFDEF AVXSUP}vaddsd xmm4, xmm4, xmm1;                         {$ELSE}db $C5,$DB,$58,$E1;{$ENDIF} 
   add edx, ebx;
   jnz @addforyloop1_2;

   // check if we need to use the unbiased version
   cmp unbiased, 0;
   jz @@dobiased_2;

   {$IFDEF AVXSUP}vmovsd xmm3, xmm3, xmm2;                            {$ELSE}db $C5,$E3,$11,$D3;{$ENDIF} 
   {$IFDEF AVXSUP}vsubsd xmm3, xmm3, xmm5;                            {$ELSE}db $C5,$E3,$5C,$DD;{$ENDIF} 
   {$IFDEF AVXSUP}vmaxsd xmm3, xmm3, xmm5;                            {$ELSE}db $C5,$E3,$5F,$DD;{$ENDIF} 

   {$IFDEF AVXSUP}vdivsd xmm4, xmm4, xmm3;                            {$ELSE}db $C5,$DB,$5E,$E3;{$ENDIF} 

   jmp @@writeRes_1;

   @@dobiased_1:
   {$IFDEF AVXSUP}vdivsd xmm4, xmm4, xmm2;                            {$ELSE}db $C5,$DB,$5E,$E2;{$ENDIF} 

   // write result
   @@writeRes_1:
   mov edx, adestLineWidth;
   {$IFDEF AVXSUP}vmovsd [eax + edx], xmm4;                           {$ELSE}db $C5,$FB,$11,$24,$10;{$ENDIF} 

   @@endProc:

   // epilog
   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;

procedure AVXMatrixMeanVarColumnUnAligned(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; unbiased : boolean);
var tmp : double;
    aDestLineWidth : NativeInt;
asm
   // prolog
   push ebx;
   push edi;
   push esi;

   mov aDestLineWidth, edx;

   // preparations
   mov edi, height;
   {$IFDEF AVXSUP}vcvtsi2sd xmm0, xmm0, edi;                          {$ELSE}db $C5,$FB,$2A,$C7;{$ENDIF} 
   imul edi, srcLineWidth;
   imul edi, -1;

   // helper registers for the mt1, mt2 and dest pointers
   sub ecx, edi;

   mov ebx, srcLineWidth;

   lea edx, tmp;
   {$IFDEF AVXSUP}vmovsd [edx], xmm0;                                 {$ELSE}db $C5,$FB,$11,$02;{$ENDIF} 
   {$IFDEF AVXSUP}vbroadcastsd ymm2, [edx];                           {$ELSE}db $C4,$E2,$7D,$19,$12;{$ENDIF} // vbroadcastsd ymm2, xmm0; // avx2

   lea edx, cLocOne;
   {$IFDEF AVXSUP}vbroadcastsd ymm5, [edx];                           {$ELSE}db $C4,$E2,$7D,$19,$2A;{$ENDIF} 

   // for x := 0 to width - 1:
   mov esi, Width;
   sub esi, 4;
   jl @@addforxloop4end;

   // 4 columns at once
   @@addforxloop4:
       {$IFDEF AVXSUP}vxorpd ymm1, ymm1, ymm1;                        {$ELSE}db $C5,$F5,$57,$C9;{$ENDIF} 

       // for y := 0 to height - 1;
       // prepare for reverse loop
       mov edx, edi;
       @addforyloop4:
           {$IFDEF AVXSUP}vmovupd ymm0, [ecx + edx];                  {$ELSE}db $C5,$FD,$10,$04,$11;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm1, ymm1, ymm0;                    {$ELSE}db $C5,$F5,$58,$C8;{$ENDIF} 
       add edx, ebx;
       jnz @addforyloop4;

       // build result
       {$IFDEF AVXSUP}vdivpd ymm0, ymm1, ymm2;                        {$ELSE}db $C5,$F5,$5E,$C2;{$ENDIF} 
       {$IFDEF AVXSUP}vmovupd [eax], ymm0;                            {$ELSE}db $C5,$FD,$11,$00;{$ENDIF} 

       // calculate final standard deviation
       // for y := 0 to height - 1;
       // prepare for reverse loop
       {$IFDEF AVXSUP}vxorpd ymm4, ymm4, ymm4;                        {$ELSE}db $C5,$DD,$57,$E4;{$ENDIF} 
       mov edx, edi;
       @addforyloop4_2:
           {$IFDEF AVXSUP}vmovupd ymm1, [ecx + edx];                  {$ELSE}db $C5,$FD,$10,$0C,$11;{$ENDIF} 
           {$IFDEF AVXSUP}vsubpd ymm1, ymm1, ymm0;                    {$ELSE}db $C5,$F5,$5C,$C8;{$ENDIF} 
           {$IFDEF AVXSUP}vmulpd ymm1, ymm1, ymm1;                    {$ELSE}db $C5,$F5,$59,$C9;{$ENDIF} 
           {$IFDEF AVXSUP}vaddpd ymm4, ymm4, ymm1;                    {$ELSE}db $C5,$DD,$58,$E1;{$ENDIF} 
       add edx, ebx;
       jnz @addforyloop4_2;

       // check if we need to use the unbiased version
       cmp unbiased, 0;
       jz @@dobiased_4;

       lea edx, cLocOne;
       {$IFDEF AVXSUP}vbroadcastsd ymm1, [edx];                       {$ELSE}db $C4,$E2,$7D,$19,$0A;{$ENDIF} 
       {$IFDEF AVXSUP}vsubpd ymm3, ymm2, ymm1;                        {$ELSE}db $C5,$ED,$5C,$D9;{$ENDIF} 
       {$IFDEF AVXSUP}vmaxpd ymm3, ymm3, ymm1;                        {$ELSE}db $C5,$E5,$5F,$D9;{$ENDIF} 

       {$IFDEF AVXSUP}vdivpd ymm4, ymm4, ymm3;                        {$ELSE}db $C5,$DD,$5E,$E3;{$ENDIF} 

       jmp @@writeRes_4;

       @@dobiased_4:
       {$IFDEF AVXSUP}vdivpd ymm4, ymm4, ymm2;                        {$ELSE}db $C5,$DD,$5E,$E2;{$ENDIF} 

       // write result
       @@writeRes_4:
       mov edx, adestLineWidth;
       {$IFDEF AVXSUP}vmovupd [eax + edx], ymm4;                      {$ELSE}db $C5,$FD,$11,$24,$10;{$ENDIF} 

       // next columns:
       add eax, 32;
       add ecx, 32;

   // loop x end
   sub esi, 4;
   jge @@addforxloop4;

   @@addforxloop4end:

   add esi, 4;
   jz @@endProc;

   sub esi, 2;
   jl @@lastcolumn;

   {$IFDEF AVXSUP}vxorpd xmm1, xmm1, xmm1;                            {$ELSE}db $C5,$F1,$57,$C9;{$ENDIF} 

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov edx, edi;
   @addforyloop2:
       {$IFDEF AVXSUP}vmovupd xmm0, [ecx + edx];                      {$ELSE}db $C5,$F9,$10,$04,$11;{$ENDIF} 
       {$IFDEF AVXSUP}vaddpd xmm1, xmm1, xmm0;                        {$ELSE}db $C5,$F1,$58,$C8;{$ENDIF} 
   add edx, ebx;
   jnz @addforyloop2;

   // build result
   {$IFDEF AVXSUP}vdivpd xmm0, xmm1, xmm2;                            {$ELSE}db $C5,$F1,$5E,$C2;{$ENDIF} 
   {$IFDEF AVXSUP}vmovupd [eax], xmm0;                                {$ELSE}db $C5,$F9,$11,$00;{$ENDIF} 

   // calculate final standard deviation
   // for y := 0 to height - 1;
   // prepare for reverse loop
   {$IFDEF AVXSUP}vxorpd xmm4, xmm4, xmm4;                            {$ELSE}db $C5,$D9,$57,$E4;{$ENDIF} 
   mov edx, edi;
   @addforyloop2_2:
      {$IFDEF AVXSUP}vmovupd xmm1, [ecx + edx];                       {$ELSE}db $C5,$F9,$10,$0C,$11;{$ENDIF} 
      {$IFDEF AVXSUP}vsubpd xmm1, xmm1, xmm0;                         {$ELSE}db $C5,$F1,$5C,$C8;{$ENDIF} 
      {$IFDEF AVXSUP}vmulpd xmm1, xmm1, xmm1;                         {$ELSE}db $C5,$F1,$59,$C9;{$ENDIF} 
      {$IFDEF AVXSUP}vaddpd xmm4, xmm4, xmm1;                         {$ELSE}db $C5,$D9,$58,$E1;{$ENDIF} 
   add edx, ebx;
   jnz @addforyloop2_2;

   // check if we need to use the unbiased version
   cmp unbiased, 0;
   jz @@dobiased_2;

   {$IFDEF AVXSUP}vsubpd xmm3, xmm2, xmm5;                            {$ELSE}db $C5,$E9,$5C,$DD;{$ENDIF} 
   {$IFDEF AVXSUP}vmaxpd xmm3, xmm3, xmm5;                            {$ELSE}db $C5,$E1,$5F,$DD;{$ENDIF} 

   {$IFDEF AVXSUP}vdivpd xmm4, xmm4, xmm3;                            {$ELSE}db $C5,$D9,$5E,$E3;{$ENDIF} 

   jmp @@writeRes2;

   @@dobiased_2:
   {$IFDEF AVXSUP}vdivpd xmm4, xmm4, xmm2;                            {$ELSE}db $C5,$D9,$5E,$E2;{$ENDIF} 

   // write result
   @@writeRes2:
   mov edx, adestLineWidth;
   {$IFDEF AVXSUP}vmovupd [eax + edx], xmm4;                          {$ELSE}db $C5,$F9,$11,$24,$10;{$ENDIF} 

   // next columns:
   add eax, 16;
   add ecx, 16;

   dec esi;
   jnz @@endProc;

   @@lastcolumn:

   {$IFDEF AVXSUP}vxorpd xmm1, xmm1, xmm1;                            {$ELSE}db $C5,$F1,$57,$C9;{$ENDIF} 

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov edx, edi;
   @addforyloop_1:
       {$IFDEF AVXSUP}vaddsd xmm1, xmm1, [ecx + edx];                 {$ELSE}db $C5,$F3,$58,$0C,$11;{$ENDIF} 
   add edx, ebx;
   jnz @addforyloop_1;

   // build result
   {$IFDEF AVXSUP}vdivsd xmm0, xmm1, xmm2;                            {$ELSE}db $C5,$F3,$5E,$C2;{$ENDIF} 
   {$IFDEF AVXSUP}vmovsd [eax], xmm0;                                 {$ELSE}db $C5,$FB,$11,$00;{$ENDIF} 

   // calculate final standard deviation
   // for y := 0 to height - 1;
   // prepare for reverse loop
   {$IFDEF AVXSUP}vxorpd xmm4, xmm4, xmm4;                            {$ELSE}db $C5,$D9,$57,$E4;{$ENDIF} 
   mov edx, edi;
   @addforyloop1_2:
      {$IFDEF AVXSUP}vmovsd xmm1, [ecx + edx];                        {$ELSE}db $C5,$FB,$10,$0C,$11;{$ENDIF} 
      {$IFDEF AVXSUP}vsubsd xmm1, xmm1, xmm0;                         {$ELSE}db $C5,$F3,$5C,$C8;{$ENDIF} 
      {$IFDEF AVXSUP}vmulsd xmm1, xmm1, xmm1;                         {$ELSE}db $C5,$F3,$59,$C9;{$ENDIF} 
      {$IFDEF AVXSUP}vaddsd xmm4, xmm4, xmm1;                         {$ELSE}db $C5,$DB,$58,$E1;{$ENDIF} 
   add edx, ebx;
   jnz @addforyloop1_2;

   // check if we need to use the unbiased version
   cmp unbiased, 0;
   jz @@dobiased_2;

   {$IFDEF AVXSUP}vmovsd xmm3, xmm3, xmm2;                            {$ELSE}db $C5,$E3,$11,$D3;{$ENDIF} 
   {$IFDEF AVXSUP}vsubsd xmm3, xmm3, xmm5;                            {$ELSE}db $C5,$E3,$5C,$DD;{$ENDIF} 
   {$IFDEF AVXSUP}vmaxsd xmm3, xmm3, xmm5;                            {$ELSE}db $C5,$E3,$5F,$DD;{$ENDIF} 

   {$IFDEF AVXSUP}vdivsd xmm4, xmm4, xmm3;                            {$ELSE}db $C5,$DB,$5E,$E3;{$ENDIF} 

   jmp @@writeRes_1;

   @@dobiased_1:
   {$IFDEF AVXSUP}vdivsd xmm4, xmm4, xmm2;                            {$ELSE}db $C5,$DB,$5E,$E2;{$ENDIF} 

   // write result
   @@writeRes_1:
   mov edx, adestLineWidth;
   {$IFDEF AVXSUP}vmovsd [eax + edx], xmm4;                           {$ELSE}db $C5,$FB,$11,$24,$10;{$ENDIF} 

   @@endProc:

   // epilog
   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;

{$ENDIF}

end.
