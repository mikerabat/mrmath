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


unit AVXMatrixNormOperations;

interface

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}
{$IFNDEF x64}

uses MatrixConst;

function AVXMatrixElementwiseNorm2Aligned(dest : PDouble; const LineWidth : NativeInt; Width, height : NativeInt) : double; {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
function AVXMatrixElementwiseNorm2UnAligned(dest : PDouble; const LineWidth : NativeInt; Width, height : NativeInt) : double; {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

procedure AVXMatrixNormalizeRowAligned(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure AVXMatrixNormalizeRowUnAligned(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

procedure AVXMatrixNormalizeColumnAligned(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure AVXMatrixNormalizeColumnUnAligned(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

{$ENDIF}

implementation

{$IFNDEF x64}

{$WARNINGS OFF}

{$IFDEF FPC} {$ASMMODE intel} {$S-} {$ENDIF}

const cLocOne : double = 1;

function AVXMatrixElementwiseNorm2Aligned(dest : PDouble; const LineWidth : NativeInt; Width, height : NativeInt) : double;
// eax = dest, edx = lineWidth, ecx = width
asm
   // prolog - simulate stack
   push ebx;
   push esi;

   //iters := -width*sizeof(double);
   imul ecx, -8;

   // helper registers for the mt1, mt2 and dest pointers
   sub eax, ecx;

   {$IFDEF FPC}vxorpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$57,$C0;{$ENDIF} 
   {$IFDEF FPC}vxorpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$57,$C9;{$ENDIF} 

   mov ebx, height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov esi, ecx;
       @addforxloop:
           add esi, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [eax + rax];

           // mul add:
           {$IFDEF FPC}vmovapd ymm3, [eax + esi - 128];{$ELSE}db $C5,$FD,$28,$5C,$30,$80;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm3, ymm3, ymm3;{$ELSE}db $C5,$E5,$59,$DB;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm1, ymm1, ymm3;{$ELSE}db $C5,$F5,$58,$CB;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm2, [eax + esi - 96];{$ELSE}db $C5,$FD,$28,$54,$30,$A0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm2, ymm2, ymm2;{$ELSE}db $C5,$ED,$59,$D2;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm1, ymm1, ymm2;{$ELSE}db $C5,$F5,$58,$CA;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm3, [eax + esi - 64];{$ELSE}db $C5,$FD,$28,$5C,$30,$C0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm3, ymm3, ymm3;{$ELSE}db $C5,$E5,$59,$DB;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm1, ymm1, ymm3;{$ELSE}db $C5,$F5,$58,$CB;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm2, [eax + esi - 32];{$ELSE}db $C5,$FD,$28,$54,$30,$E0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm2, ymm2, ymm2;{$ELSE}db $C5,$ED,$59,$D2;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm1, ymm1, ymm2;{$ELSE}db $C5,$F5,$58,$CA;{$ENDIF} 
       jmp @addforxloop

       @loopEnd:

       sub esi, 128;

       jz @nextLine;

       @addforxloop2:
           add esi, 16;
           jg @@addforxloop2end;

           {$IFDEF FPC}vmovapd xmm3, [eax + esi - 16];{$ELSE}db $C5,$F9,$28,$5C,$30,$F0;{$ENDIF} 
           {$IFDEF FPC}vmulpd xmm3, xmm3, xmm3;{$ELSE}db $C5,$E1,$59,$DB;{$ENDIF} 
           {$IFDEF FPC}vaddpd xmm0, xmm0, xmm3;{$ELSE}db $C5,$F9,$58,$C3;{$ENDIF} 
       jmp @addforxloop2;

       @@addforxloop2end:

       sub esi, 16;
       jz @nextLine;

       {$IFDEF FPC}vmovsd xmm3, [eax + esi];{$ELSE}db $C5,$FB,$10,$1C,$30;{$ENDIF} 
       {$IFDEF FPC}vmulsd xmm3, xmm3, xmm3;{$ELSE}db $C5,$E3,$59,$DB;{$ENDIF} 
       {$IFDEF FPC}vaddsd xmm0, xmm0, xmm3;{$ELSE}db $C5,$FB,$58,$C3;{$ENDIF} 

       @nextLine:

       // next line:
       add eax, edx;

   // loop y end
   dec ebx;
   jnz @@addforyloop;

   // build result
   {$IFDEF FPC}vextractf128 xmm2, ymm1, 1;{$ELSE}db $C4,$E3,$7D,$19,$CA,$01;{$ENDIF} 
   {$IFDEF FPC}vhaddpd xmm1, xmm1, xmm2;{$ELSE}db $C5,$F1,$7C,$CA;{$ENDIF} 
   {$IFDEF FPC}vaddpd xmm0, xmm0, xmm1;{$ELSE}db $C5,$F9,$58,$C1;{$ENDIF} 
   {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} 

   // epilog claenup stack
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   movsd Result, xmm0;

   pop esi;
   pop ebx;
end;

function AVXMatrixElementwiseNorm2UnAligned(dest : PDouble; const LineWidth : NativeInt; Width, height : NativeInt) : double;
asm
   // prolog - simulate stack
   push ebx;
   push esi;

   //iters := -width*sizeof(double);
   imul ecx, -8;

   // helper registers for the mt1, mt2 and dest pointers
   sub eax, ecx;

   {$IFDEF FPC}vxorpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$57,$C0;{$ENDIF} 
   {$IFDEF FPC}vxorpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$57,$C9;{$ENDIF} 

   mov ebx, height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov esi, ecx;
       @addforxloop:
           add esi, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [eax + esi];

           // mul add:
           {$IFDEF FPC}vmovupd ymm3, [eax + esi - 128];{$ELSE}db $C5,$FD,$10,$5C,$30,$80;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm3, ymm3, ymm3;{$ELSE}db $C5,$E5,$59,$DB;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm1, ymm1, ymm3;{$ELSE}db $C5,$F5,$58,$CB;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm2, [eax + esi - 96];{$ELSE}db $C5,$FD,$10,$54,$30,$A0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm2, ymm2, ymm2;{$ELSE}db $C5,$ED,$59,$D2;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm1, ymm1, ymm2;{$ELSE}db $C5,$F5,$58,$CA;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm3, [eax + esi - 64];{$ELSE}db $C5,$FD,$10,$5C,$30,$C0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm3, ymm3, ymm3;{$ELSE}db $C5,$E5,$59,$DB;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm1, ymm1, ymm3;{$ELSE}db $C5,$F5,$58,$CB;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm2, [eax + esi - 32];{$ELSE}db $C5,$FD,$10,$54,$30,$E0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm2, ymm2, ymm2;{$ELSE}db $C5,$ED,$59,$D2;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm1, ymm1, ymm2;{$ELSE}db $C5,$F5,$58,$CA;{$ENDIF} 
       jmp @addforxloop

       @loopEnd:

       sub esi, 128;

       jz @nextLine;

       @addforxloop2:
           add esi, 16;
           jg @@addforxloop2end;

           {$IFDEF FPC}vmovupd xmm3, [eax + esi - 16];{$ELSE}db $C5,$F9,$10,$5C,$30,$F0;{$ENDIF} 
           {$IFDEF FPC}vmulpd xmm3, xmm3, xmm3;{$ELSE}db $C5,$E1,$59,$DB;{$ENDIF} 
           {$IFDEF FPC}vaddpd xmm0, xmm0, xmm3;{$ELSE}db $C5,$F9,$58,$C3;{$ENDIF} 
       jmp @addforxloop2;

       @@addforxloop2end:

       sub esi, 16;
       jz @nextLine;

       {$IFDEF FPC}vmovsd xmm3, [eax + esi];{$ELSE}db $C5,$FB,$10,$1C,$30;{$ENDIF} 
       {$IFDEF FPC}vmulsd xmm3, xmm3, xmm3;{$ELSE}db $C5,$E3,$59,$DB;{$ENDIF} 
       {$IFDEF FPC}vaddsd xmm0, xmm0, xmm3;{$ELSE}db $C5,$FB,$58,$C3;{$ENDIF} 

       @nextLine:

       // next line:
       add eax, edx;

   // loop y end
   dec ebx;
   jnz @@addforyloop;

   // build result
   {$IFDEF FPC}vextractf128 xmm2, ymm1, 1;{$ELSE}db $C4,$E3,$7D,$19,$CA,$01;{$ENDIF} 
   {$IFDEF FPC}vhaddpd xmm1, xmm1, xmm2;{$ELSE}db $C5,$F1,$7C,$CA;{$ENDIF} 
   {$IFDEF FPC}vaddpd xmm0, xmm0, xmm1;{$ELSE}db $C5,$F9,$58,$C1;{$ENDIF} 
   {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} 

   // epilog claenup stack
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 
   movsd Result, xmm0;
   
   pop esi;
   pop ebx;
end;

procedure AVXMatrixNormalizeRowAligned(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt);
var tmp : double;
asm
   // prolog - simulate stack
   push ebx;
   push edi;
   push esi;

   //iters := -width*sizeof(double);
   mov edi, width;
   imul edi, -8;

   // helper registers for the mt1, mt2 and dest pointers
   sub eax, edi;
   sub ecx, edi;

   mov ebx, height;
   @@addforyloop:
       {$IFDEF FPC}vxorpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$57,$C0;{$ENDIF} 
       {$IFDEF FPC}vxorpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$57,$C9;{$ENDIF} 

       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov esi, edi;
       @addforxloop:
           add esi, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [eax + esi];

           // mul add:
           {$IFDEF FPC}vmovapd ymm3, [ecx + esi - 128];{$ELSE}db $C5,$FD,$28,$5C,$31,$80;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm3, ymm3, ymm3;{$ELSE}db $C5,$E5,$59,$DB;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm1, ymm1, ymm3;{$ELSE}db $C5,$F5,$58,$CB;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm2, [ecx + esi - 96];{$ELSE}db $C5,$FD,$28,$54,$31,$A0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm2, ymm2, ymm2;{$ELSE}db $C5,$ED,$59,$D2;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm1, ymm1, ymm2;{$ELSE}db $C5,$F5,$58,$CA;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm3, [ecx + esi - 64];{$ELSE}db $C5,$FD,$28,$5C,$31,$C0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm3, ymm3, ymm3;{$ELSE}db $C5,$E5,$59,$DB;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm1, ymm1, ymm3;{$ELSE}db $C5,$F5,$58,$CB;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm2, [ecx + esi - 32];{$ELSE}db $C5,$FD,$28,$54,$31,$E0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm2, ymm2, ymm2;{$ELSE}db $C5,$ED,$59,$D2;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm1, ymm1, ymm2;{$ELSE}db $C5,$F5,$58,$CA;{$ENDIF} 
       jmp @addforxloop

       @loopEnd:

       sub esi, 128;

       jz @nextLine;

       @addforxloop2:
           add esi, 16;
           jg @@addforxloop2end;

           {$IFDEF FPC}vmovapd xmm3, [ecx + esi - 16];{$ELSE}db $C5,$F9,$28,$5C,$31,$F0;{$ENDIF} 
           {$IFDEF FPC}vmulpd xmm3, xmm3, xmm3;{$ELSE}db $C5,$E1,$59,$DB;{$ENDIF} 
           {$IFDEF FPC}vaddpd xmm0, xmm0, xmm3;{$ELSE}db $C5,$F9,$58,$C3;{$ENDIF} 
       jmp @addforxloop2;

       @@addforxloop2end:

       sub esi, 16;
       jz @nextLine;

       {$IFDEF FPC}vmovsd xmm3, [ecx + esi];{$ELSE}db $C5,$FB,$10,$1C,$31;{$ENDIF} 
       {$IFDEF FPC}vmulsd xmm3, xmm3, xmm3;{$ELSE}db $C5,$E3,$59,$DB;{$ENDIF} 
       {$IFDEF FPC}vaddsd xmm0, xmm0, xmm3;{$ELSE}db $C5,$FB,$58,$C3;{$ENDIF} 

       @nextLine:

       // build result
       {$IFDEF FPC}vextractf128 xmm2, ymm1, 1;{$ELSE}db $C4,$E3,$7D,$19,$CA,$01;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm1, xmm1, xmm2;{$ELSE}db $C5,$F1,$7C,$CA;{$ENDIF} 
       {$IFDEF FPC}vaddpd xmm0, xmm0, xmm1;{$ELSE}db $C5,$F9,$58,$C1;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} 

       // build result
       {$IFDEF FPC}vsqrtsd xmm0, xmm0, xmm0;{$ELSE}db $C5,$FB,$51,$C0;{$ENDIF} 

       //1/sqrt(norm)
       lea esi, cLocOne;
       {$IFDEF FPC}vmovsd xmm1, [esi];{$ELSE}db $C5,$FB,$10,$0E;{$ENDIF} 
       {$IFDEF FPC}vdivsd xmm1, xmm1, xmm0;{$ELSE}db $C5,$F3,$5E,$C8;{$ENDIF} 

       lea esi, tmp;
       {$IFDEF FPC}vmovsd [esi], xmm1;{$ELSE}db $C5,$FB,$11,$0E;{$ENDIF} 
       {$IFDEF FPC}vbroadcastsd ymm2, [esi];   {$ELSE}db $C4,$E2,$7D,$19,$16;{$ENDIF} // need avx2

       //  normalize
       mov esi, edi;
       @addforxloop3:
           add esi, 128;
           jg @loopEnd2;

           // prefetch data...
           // prefetch [ecx + esi];
           // prefetchw [eax + esi];

           // mult:
           {$IFDEF FPC}vmovapd ymm0, [ecx + esi - 128];{$ELSE}db $C5,$FD,$28,$44,$31,$80;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm0, ymm0, ymm2;{$ELSE}db $C5,$FD,$59,$C2;{$ENDIF} 
           {$IFDEF FPC}vmovapd [eax + esi - 128], ymm0;{$ELSE}db $C5,$FD,$29,$44,$30,$80;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm1, [ecx + esi - 96];{$ELSE}db $C5,$FD,$28,$4C,$31,$A0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm1, ymm1, ymm2;{$ELSE}db $C5,$F5,$59,$CA;{$ENDIF} 
           {$IFDEF FPC}vmovapd [eax + esi - 96], ymm1;{$ELSE}db $C5,$FD,$29,$4C,$30,$A0;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm0, [ecx + esi - 64];{$ELSE}db $C5,$FD,$28,$44,$31,$C0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm0, ymm0, ymm2;{$ELSE}db $C5,$FD,$59,$C2;{$ENDIF} 
           {$IFDEF FPC}vmovapd [eax + esi - 64], ymm0;{$ELSE}db $C5,$FD,$29,$44,$30,$C0;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm1, [ecx + esi - 32];{$ELSE}db $C5,$FD,$28,$4C,$31,$E0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm1, ymm1, ymm2;{$ELSE}db $C5,$F5,$59,$CA;{$ENDIF} 
           {$IFDEF FPC}vmovapd [eax + esi - 32], ymm1;{$ELSE}db $C5,$FD,$29,$4C,$30,$E0;{$ENDIF} 

       jmp @addforxloop3

       @loopEnd2:

       sub esi, 128;

       jz @nextLine2;

       @addforxloop4:
           add esi, 16;
           jg @addforxloop4end;

           {$IFDEF FPC}vmovapd xmm0, [ecx + esi - 16];{$ELSE}db $C5,$F9,$28,$44,$31,$F0;{$ENDIF} 
           {$IFDEF FPC}vmulpd xmm0, xmm0, xmm2;{$ELSE}db $C5,$F9,$59,$C2;{$ENDIF} 
           {$IFDEF FPC}vmovapd [eax + esi - 16], xmm0;{$ELSE}db $C5,$F9,$29,$44,$30,$F0;{$ENDIF} 
       jmp @addforxloop4;

       @addforxloop4end:
       sub esi, 16;
       jz @nextLine2;

       {$IFDEF FPC}vmovsd xmm0, [ecx + esi];{$ELSE}db $C5,$FB,$10,$04,$31;{$ENDIF} 
       {$IFDEF FPC}vmulsd xmm0, xmm0, xmm2;{$ELSE}db $C5,$FB,$59,$C2;{$ENDIF} 
       {$IFDEF FPC}vmovsd [eax + esi], xmm0;{$ELSE}db $C5,$FB,$11,$04,$30;{$ENDIF} 

       @nextLine2:

       // next line:
       add eax, edx;
       add ecx, srcLineWidth;

   // loop y end
   dec ebx;
   jnz @@addforyloop;

   // epilog claenup stack
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;

procedure AVXMatrixNormalizeRowUnAligned(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt);
var tmp : double;
asm
   // prolog - simulate stack
   push ebx;
   push edi;
   push esi;

   //iters := -width*sizeof(double);
   mov edi, width;
   imul edi, -8;

   // helper registers for the mt1, mt2 and dest pointers
   sub eax, edi;
   sub ecx, edi;

   mov ebx, height;
   @@addforyloop:
       {$IFDEF FPC}vxorpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$57,$C0;{$ENDIF} 
       {$IFDEF FPC}vxorpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$57,$C9;{$ENDIF} 

       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov esi, edi;
       @addforxloop:
           add esi, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [eax + esi];

           // mul add:
           {$IFDEF FPC}vmovupd ymm3, [ecx + esi - 128];{$ELSE}db $C5,$FD,$10,$5C,$31,$80;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm3, ymm3, ymm3;{$ELSE}db $C5,$E5,$59,$DB;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm1, ymm1, ymm3;{$ELSE}db $C5,$F5,$58,$CB;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm2, [ecx + esi - 96];{$ELSE}db $C5,$FD,$10,$54,$31,$A0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm2, ymm2, ymm2;{$ELSE}db $C5,$ED,$59,$D2;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm1, ymm1, ymm2;{$ELSE}db $C5,$F5,$58,$CA;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm3, [ecx + esi - 64];{$ELSE}db $C5,$FD,$10,$5C,$31,$C0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm3, ymm3, ymm3;{$ELSE}db $C5,$E5,$59,$DB;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm1, ymm1, ymm3;{$ELSE}db $C5,$F5,$58,$CB;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm2, [ecx + esi - 32];{$ELSE}db $C5,$FD,$10,$54,$31,$E0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm2, ymm2, ymm2;{$ELSE}db $C5,$ED,$59,$D2;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm1, ymm1, ymm2;{$ELSE}db $C5,$F5,$58,$CA;{$ENDIF} 
       jmp @addforxloop

       @loopEnd:

       sub esi, 128;

       jz @nextLine;

       @addforxloop2:
           add esi, 16;
           jg @@addforxloop2end;

           {$IFDEF FPC}vmovupd xmm3, [ecx + esi - 16];{$ELSE}db $C5,$F9,$10,$5C,$31,$F0;{$ENDIF} 
           {$IFDEF FPC}vmulpd xmm3, xmm3, xmm3;{$ELSE}db $C5,$E1,$59,$DB;{$ENDIF} 
           {$IFDEF FPC}vaddpd xmm0, xmm0, xmm3;{$ELSE}db $C5,$F9,$58,$C3;{$ENDIF} 
       jmp @addforxloop2;

       @@addforxloop2end:

       sub esi, 16;
       jz @nextLine;

       {$IFDEF FPC}vmovsd xmm3, [ecx + esi];{$ELSE}db $C5,$FB,$10,$1C,$31;{$ENDIF} 
       {$IFDEF FPC}vmulsd xmm3, xmm3, xmm3;{$ELSE}db $C5,$E3,$59,$DB;{$ENDIF} 
       {$IFDEF FPC}vaddsd xmm0, xmm0, xmm3;{$ELSE}db $C5,$FB,$58,$C3;{$ENDIF} 

       @nextLine:

       // build result
       {$IFDEF FPC}vextractf128 xmm2, ymm1, 1;{$ELSE}db $C4,$E3,$7D,$19,$CA,$01;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm1, xmm1, xmm2;{$ELSE}db $C5,$F1,$7C,$CA;{$ENDIF} 
       {$IFDEF FPC}vaddpd xmm0, xmm0, xmm1;{$ELSE}db $C5,$F9,$58,$C1;{$ENDIF} 
       {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} 

       // build result
       {$IFDEF FPC}vsqrtsd xmm0, xmm0, xmm0;{$ELSE}db $C5,$FB,$51,$C0;{$ENDIF} 

       //1/sqrt(norm)
       lea esi, cLocOne;
       {$IFDEF FPC}vmovsd xmm1, [esi];{$ELSE}db $C5,$FB,$10,$0E;{$ENDIF} 
       {$IFDEF FPC}vdivsd xmm1, xmm1, xmm0;{$ELSE}db $C5,$F3,$5E,$C8;{$ENDIF} 

       lea esi, tmp;
       {$IFDEF FPC}vmovsd [esi], xmm1;{$ELSE}db $C5,$FB,$11,$0E;{$ENDIF} 
       {$IFDEF FPC}vbroadcastsd ymm2, [esi];   {$ELSE}db $C4,$E2,$7D,$19,$16;{$ENDIF} // need avx2

       //  normalize
       mov esi, edi;
       @addforxloop3:
           add esi, 128;
           jg @loopEnd2;

           // prefetch data...
           // prefetch [ecx + esi];
           // prefetchw [eax + esi];

           // mult:
           {$IFDEF FPC}vmovupd ymm0, [ecx + esi - 128];{$ELSE}db $C5,$FD,$10,$44,$31,$80;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm0, ymm0, ymm2;{$ELSE}db $C5,$FD,$59,$C2;{$ENDIF} 
           {$IFDEF FPC}vmovupd [eax + esi - 128], ymm0;{$ELSE}db $C5,$FD,$11,$44,$30,$80;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm1, [ecx + esi - 96];{$ELSE}db $C5,$FD,$10,$4C,$31,$A0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm1, ymm1, ymm2;{$ELSE}db $C5,$F5,$59,$CA;{$ENDIF} 
           {$IFDEF FPC}vmovupd [eax + esi - 96], ymm1;{$ELSE}db $C5,$FD,$11,$4C,$30,$A0;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm0, [ecx + esi - 64];{$ELSE}db $C5,$FD,$10,$44,$31,$C0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm0, ymm0, ymm2;{$ELSE}db $C5,$FD,$59,$C2;{$ENDIF} 
           {$IFDEF FPC}vmovupd [eax + esi - 64], ymm0;{$ELSE}db $C5,$FD,$11,$44,$30,$C0;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm1, [ecx + esi - 32];{$ELSE}db $C5,$FD,$10,$4C,$31,$E0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm1, ymm1, ymm2;{$ELSE}db $C5,$F5,$59,$CA;{$ENDIF} 
           {$IFDEF FPC}vmovupd [eax + esi - 32], ymm1;{$ELSE}db $C5,$FD,$11,$4C,$30,$E0;{$ENDIF} 

       jmp @addforxloop3

       @loopEnd2:

       sub esi, 128;

       jz @nextLine2;

       @addforxloop4:
           add esi, 16;
           jg @addforxloop4end;

           {$IFDEF FPC}vmovupd xmm0, [ecx + esi - 16];{$ELSE}db $C5,$F9,$10,$44,$31,$F0;{$ENDIF} 
           {$IFDEF FPC}vmulpd xmm0, xmm0, xmm2;{$ELSE}db $C5,$F9,$59,$C2;{$ENDIF} 
           {$IFDEF FPC}vmovupd [eax + esi - 16], xmm0;{$ELSE}db $C5,$F9,$11,$44,$30,$F0;{$ENDIF} 
       jmp @addforxloop4;

       @addforxloop4end:
       sub esi, 16;
       jz @nextLine2;

       {$IFDEF FPC}vmovsd xmm0, [ecx + esi];{$ELSE}db $C5,$FB,$10,$04,$31;{$ENDIF} 
       {$IFDEF FPC}vmulsd xmm0, xmm0, xmm2;{$ELSE}db $C5,$FB,$59,$C2;{$ENDIF} 
       {$IFDEF FPC}vmovsd [eax + esi], xmm0;{$ELSE}db $C5,$FB,$11,$04,$30;{$ENDIF} 

       @nextLine2:

       // next line:
       add eax, edx;
       add ecx, srcLineWidth;

   // loop y end
   dec ebx;
   jnz @@addforyloop;

   // epilog claenup stack
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;

procedure AVXMatrixNormalizeColumnAligned(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt);
var aDestLineWidth : NativeInt;
asm
   // prolog
   push ebx;
   push edi;
   push esi;

   // init
   mov aDestLineWidth, edx;

   // note: eax = dest, RDX = destLineWidth, edx = src
   xor edi, edi;
   sub edi, height;
   imul edi, srcLineWidth;

   // helper registers for the mt1 and dest pointers
   sub ecx, edi;

   mov esi, srcLineWidth;

   // for x := 0 to width - 1:
   mov ebx, Width;
   sub ebx, 4;
   jl @@addforxloop4end;

   // 4 columns at once
   @@addforxloop4:
       {$IFDEF FPC}vxorpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$57,$C9;{$ENDIF} 

       // for y := 0 to height - 1;
       // prepare for reverse loop
       mov edx, edi;
       @addforyloop4:
           {$IFDEF FPC}vmovapd ymm3, [ecx + edx];{$ELSE}db $C5,$FD,$28,$1C,$11;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm3, ymm3, ymm3;{$ELSE}db $C5,$E5,$59,$DB;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm1, ymm1, ymm3;{$ELSE}db $C5,$F5,$58,$CB;{$ENDIF} 
       add edx, esi;
       jnz @addforyloop4;

       // normalization factor
       lea edx, cLocOne;
       {$IFDEF FPC}vbroadcastsd ymm2, [edx];{$ELSE}db $C4,$E2,$7D,$19,$12;{$ENDIF} 
       {$IFDEF FPC}vsqrtpd ymm1, ymm1;{$ELSE}db $C5,$FD,$51,$C9;{$ENDIF} 
       {$IFDEF FPC}vdivpd ymm0, ymm2, ymm1;{$ELSE}db $C5,$ED,$5E,$C1;{$ENDIF} 

       push eax;

       // normalize
       mov edx, edi;
       @addforyloop4_2:
           {$IFDEF FPC}vmovapd ymm1, [ecx + edx];{$ELSE}db $C5,$FD,$28,$0C,$11;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm1, ymm1, ymm0;{$ELSE}db $C5,$F5,$59,$C8;{$ENDIF} 
           {$IFDEF FPC}vmovapd [eax], ymm1;{$ELSE}db $C5,$FD,$29,$08;{$ENDIF} 

       add eax, adestLineWidth;
       add edx, esi;
       jnz @addforyloop4_2;

       // next columns:
       pop eax;
       add eax, 32;
       add ecx, 32;

   // loop x end
   sub ebx, 4;
   jge @@addforxloop4;

   @@addforxloop4end:

   add ebx, 4;
   jz @@endProc;

   sub ebx, 2;
   jl @@lastcolumn;

   {$IFDEF FPC}vxorpd xmm1, xmm1, xmm1;{$ELSE}db $C5,$F1,$57,$C9;{$ENDIF} 

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov edx, edi;
   @addforyloop2:
       {$IFDEF FPC}vmovapd xmm0, [ecx + edx];{$ELSE}db $C5,$F9,$28,$04,$11;{$ENDIF} 
       {$IFDEF FPC}vmulpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$59,$C0;{$ENDIF} 
       {$IFDEF FPC}vaddpd xmm1, xmm1, xmm0;{$ELSE}db $C5,$F1,$58,$C8;{$ENDIF} 
   add edx, esi;
   jnz @addforyloop2;

   // normalization factor
   lea edx, cLocOne;
   {$IFDEF FPC}vmovddup xmm2, [edx];{$ELSE}db $C5,$FB,$12,$12;{$ENDIF} 
   {$IFDEF FPC}vsqrtpd xmm1, xmm1;{$ELSE}db $C5,$F9,$51,$C9;{$ENDIF} 
   {$IFDEF FPC}vdivpd xmm0, xmm2, xmm1;{$ELSE}db $C5,$E9,$5E,$C1;{$ENDIF} 

   // normalize
   push eax;
   mov edx, edi;
   @addforyloop2_2:
      {$IFDEF FPC}vmovapd xmm1, [ecx + edx];{$ELSE}db $C5,$F9,$28,$0C,$11;{$ENDIF} 
      {$IFDEF FPC}vmulpd xmm1, xmm1, xmm0;{$ELSE}db $C5,$F1,$59,$C8;{$ENDIF} 
      {$IFDEF FPC}vmovapd [eax], xmm1;{$ELSE}db $C5,$F9,$29,$08;{$ENDIF} 
   add eax, adestLineWidth;
   add edx, esi;
   jnz @addforyloop2_2;

   // next columns:
   pop eax;
   add eax, 16;
   add ecx, 16;

   dec ebx;
   jnz @@endProc;

   @@lastcolumn:

   {$IFDEF FPC}vxorpd xmm1, xmm1, xmm1;{$ELSE}db $C5,$F1,$57,$C9;{$ENDIF} 

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov edx, edi;
   @addforyloop_1:
       {$IFDEF FPC}vmovsd xmm0, [ecx + edx];{$ELSE}db $C5,$FB,$10,$04,$11;{$ENDIF} 
       {$IFDEF FPC}vmulsd xmm0, xmm0, xmm0;{$ELSE}db $C5,$FB,$59,$C0;{$ENDIF} 
       {$IFDEF FPC}vaddsd xmm1, xmm1, xmm0;{$ELSE}db $C5,$F3,$58,$C8;{$ENDIF} 
   add edx, esi;
   jnz @addforyloop_1;


   // build result
   lea edx, cLocOne;
   {$IFDEF FPC}vmovsd xmm2, [edx];{$ELSE}db $C5,$FB,$10,$12;{$ENDIF} 
   {$IFDEF FPC}vsqrtsd xmm1, xmm1, xmm1;{$ELSE}db $C5,$F3,$51,$C9;{$ENDIF} 
   {$IFDEF FPC}vdivsd xmm0, xmm2, xmm1;{$ELSE}db $C5,$EB,$5E,$C1;{$ENDIF} 

   // normalize last column
   mov edx, edi;
   @addforyloop2_1:
      {$IFDEF FPC}vmovsd xmm1, [ecx + edx];{$ELSE}db $C5,$FB,$10,$0C,$11;{$ENDIF} 
      {$IFDEF FPC}vmulsd xmm1, xmm1, xmm0;{$ELSE}db $C5,$F3,$59,$C8;{$ENDIF} 
      {$IFDEF FPC}vmovsd [eax], xmm1;{$ELSE}db $C5,$FB,$11,$08;{$ENDIF} 
   add eax, adestLineWidth;
   add edx, esi;
   jnz @addforyloop2_1;

   @@endProc:

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;


procedure AVXMatrixNormalizeColumnUnAligned(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt);
var aDestLineWidth : NativeInt;
asm
   // prolog
   push ebx;
   push edi;
   push esi;

   // init
   mov aDestLineWidth, edx;

   // note: eax = dest, RDX = destLineWidth, edx = src
   xor edi, edi;
   sub edi, height;
   imul edi, srcLineWidth;

   // helper registers for the mt1 and dest pointers
   sub ecx, edi;

   mov esi, srcLineWidth;

   // for x := 0 to width - 1:
   mov ebx, Width;
   sub ebx, 4;
   jl @@addforxloop4end;

   // 4 columns at once
   @@addforxloop4:
       {$IFDEF FPC}vxorpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$57,$C9;{$ENDIF} 

       // for y := 0 to height - 1;
       // prepare for reverse loop
       mov edx, edi;
       @addforyloop4:
           {$IFDEF FPC}vmovupd ymm3, [ecx + edx];{$ELSE}db $C5,$FD,$10,$1C,$11;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm3, ymm3, ymm3;{$ELSE}db $C5,$E5,$59,$DB;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm1, ymm1, ymm3;{$ELSE}db $C5,$F5,$58,$CB;{$ENDIF} 
       add edx, esi;
       jnz @addforyloop4;

       // normalization factor
       lea edx, cLocOne;
       {$IFDEF FPC}vbroadcastsd ymm2, [edx];{$ELSE}db $C4,$E2,$7D,$19,$12;{$ENDIF} 
       {$IFDEF FPC}vsqrtpd ymm1, ymm1;{$ELSE}db $C5,$FD,$51,$C9;{$ENDIF} 
       {$IFDEF FPC}vdivpd ymm0, ymm2, ymm1;{$ELSE}db $C5,$ED,$5E,$C1;{$ENDIF} 

       push eax;

       // normalize
       mov edx, edi;
       @addforyloop4_2:
           {$IFDEF FPC}vmovupd ymm1, [ecx + edx];{$ELSE}db $C5,$FD,$10,$0C,$11;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm1, ymm1, ymm0;{$ELSE}db $C5,$F5,$59,$C8;{$ENDIF} 
           {$IFDEF FPC}vmovupd [eax], ymm1;{$ELSE}db $C5,$FD,$11,$08;{$ENDIF} 

       add eax, adestLineWidth;
       add edx, esi;
       jnz @addforyloop4_2;

       // next columns:
       pop eax;
       add eax, 32;
       add ecx, 32;

   // loop x end
   sub ebx, 4;
   jge @@addforxloop4;

   @@addforxloop4end:

   add ebx, 4;
   jz @@endProc;

   sub ebx, 2;
   jl @@lastcolumn;

   {$IFDEF FPC}vxorpd xmm1, xmm1, xmm1;{$ELSE}db $C5,$F1,$57,$C9;{$ENDIF} 

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov edx, edi;
   @addforyloop2:
       {$IFDEF FPC}vmovupd xmm0, [ecx + edx];{$ELSE}db $C5,$F9,$10,$04,$11;{$ENDIF} 
       {$IFDEF FPC}vmulpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$59,$C0;{$ENDIF} 
       {$IFDEF FPC}vaddpd xmm1, xmm1, xmm0;{$ELSE}db $C5,$F1,$58,$C8;{$ENDIF} 
   add edx, esi;
   jnz @addforyloop2;

   // normalization factor
   lea edx, cLocOne;
   {$IFDEF FPC}vmovddup xmm2, [edx];{$ELSE}db $C5,$FB,$12,$12;{$ENDIF} 
   {$IFDEF FPC}vsqrtpd xmm1, xmm1;{$ELSE}db $C5,$F9,$51,$C9;{$ENDIF} 
   {$IFDEF FPC}vdivpd xmm0, xmm2, xmm1;{$ELSE}db $C5,$E9,$5E,$C1;{$ENDIF} 

   // normalize
   push eax;
   mov edx, edi;
   @addforyloop2_2:
      {$IFDEF FPC}vmovupd xmm1, [ecx + edx];{$ELSE}db $C5,$F9,$10,$0C,$11;{$ENDIF} 
      {$IFDEF FPC}vmulpd xmm1, xmm1, xmm0;{$ELSE}db $C5,$F1,$59,$C8;{$ENDIF} 
      {$IFDEF FPC}vmovupd [eax], xmm1;{$ELSE}db $C5,$F9,$11,$08;{$ENDIF} 
   add eax, adestLineWidth;
   add edx, esi;
   jnz @addforyloop2_2;

   // next columns:
   pop eax;
   add eax, 16;
   add ecx, 16;

   dec ebx;
   jnz @@endProc;

   @@lastcolumn:

   {$IFDEF FPC}vxorpd xmm1, xmm1, xmm1;{$ELSE}db $C5,$F1,$57,$C9;{$ENDIF} 

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov edx, edi;
   @addforyloop_1:
       {$IFDEF FPC}vmovsd xmm0, [ecx + edx];{$ELSE}db $C5,$FB,$10,$04,$11;{$ENDIF} 
       {$IFDEF FPC}vmulsd xmm0, xmm0, xmm0;{$ELSE}db $C5,$FB,$59,$C0;{$ENDIF} 
       {$IFDEF FPC}vaddsd xmm1, xmm1, xmm0;{$ELSE}db $C5,$F3,$58,$C8;{$ENDIF} 
   add edx, esi;
   jnz @addforyloop_1;


   // build result
   lea edx, cLocOne;
   {$IFDEF FPC}vmovsd xmm2, [edx];{$ELSE}db $C5,$FB,$10,$12;{$ENDIF} 
   {$IFDEF FPC}vsqrtsd xmm1, xmm1, xmm1;{$ELSE}db $C5,$F3,$51,$C9;{$ENDIF} 
   {$IFDEF FPC}vdivsd xmm0, xmm2, xmm1;{$ELSE}db $C5,$EB,$5E,$C1;{$ENDIF} 

   // normalize last column
   mov edx, edi;
   @addforyloop2_1:
      {$IFDEF FPC}vmovsd xmm1, [ecx + edx];{$ELSE}db $C5,$FB,$10,$0C,$11;{$ENDIF} 
      {$IFDEF FPC}vmulsd xmm1, xmm1, xmm0;{$ELSE}db $C5,$F3,$59,$C8;{$ENDIF} 
      {$IFDEF FPC}vmovsd [eax], xmm1;{$ELSE}db $C5,$FB,$11,$08;{$ENDIF} 
   add eax, adestLineWidth;
   add edx, esi;
   jnz @addforyloop2_1;

   @@endProc:

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;

{$ENDIF}

end.
