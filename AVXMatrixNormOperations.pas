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

function AVXMatrixElementwiseNorm2Aligned(dest : PDouble; const LineWidth : TASMNativeInt; Width, height : TASMNativeInt) : double;
function AVXMatrixElementwiseNorm2UnAligned(dest : PDouble; const LineWidth : TASMNativeInt; Width, height : TASMNativeInt) : double;


procedure AVXMatrixNormalizeRowAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
procedure AVXMatrixNormalizeRowUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);

procedure AVXMatrixNormalizeColumnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
procedure AVXMatrixNormalizeColumnUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);

{$ENDIF}

implementation

{$IFNDEF x64}

{$WARNINGS OFF}

{$IFDEF FPC} {$ASMMODE intel} {$ENDIF}

function AVXMatrixElementwiseNorm2Aligned(dest : PDouble; const LineWidth : TASMNativeInt; Width, height : TASMNativeInt) : double;
begin
asm
   // prolog - simulate stack
   push ebx;
   push edi;
   push esi;

   mov esi, LineWidth;
   //iters := -width*sizeof(double);
   mov edi, width;
   imul edi, -8;

   // helper registers for the mt1, mt2 and dest pointers
   mov ecx, dest;
   sub ecx, edi;

   {$IFDEF FPC}vxorpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$57,$C0;{$ENDIF} 
   {$IFDEF FPC}vxorpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$57,$C9;{$ENDIF} 

   mov ebx, height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov eax, edi;
       @addforxloop:
           add eax, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [ecx + eax];

           // mul add:
           {$IFDEF FPC}vmovapd ymm3, [ecx + eax - 128];{$ELSE}db $C5,$FD,$28,$5C,$01,$80;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm3, ymm3, ymm3;{$ELSE}db $C5,$E5,$59,$DB;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm1, ymm1, ymm3;{$ELSE}db $C5,$F5,$58,$CB;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm2, [ecx + eax - 96];{$ELSE}db $C5,$FD,$28,$54,$01,$A0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm2, ymm2, ymm2;{$ELSE}db $C5,$ED,$59,$D2;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm1, ymm1, ymm2;{$ELSE}db $C5,$F5,$58,$CA;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm3, [ecx + eax - 64];{$ELSE}db $C5,$FD,$28,$5C,$01,$C0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm3, ymm3, ymm3;{$ELSE}db $C5,$E5,$59,$DB;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm1, ymm1, ymm3;{$ELSE}db $C5,$F5,$58,$CB;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm2, [ecx + eax - 32];{$ELSE}db $C5,$FD,$28,$54,$01,$E0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm2, ymm2, ymm2;{$ELSE}db $C5,$ED,$59,$D2;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm1, ymm1, ymm2;{$ELSE}db $C5,$F5,$58,$CA;{$ENDIF} 
       jmp @addforxloop

       @loopEnd:

       sub eax, 128;

       jz @nextLine;

       @addforxloop2:
           add eax, 16;
           jg @@addforxloop2end;

           {$IFDEF FPC}vmovapd xmm3, [ecx + eax - 16];{$ELSE}db $C5,$F9,$28,$5C,$01,$F0;{$ENDIF} 
           {$IFDEF FPC}vmulpd xmm3, xmm3, xmm3;{$ELSE}db $C5,$E1,$59,$DB;{$ENDIF} 
           {$IFDEF FPC}vaddpd xmm0, xmm0, xmm3;{$ELSE}db $C5,$F9,$58,$C3;{$ENDIF} 
       jmp @addforxloop2;

       @@addforxloop2end:

       sub eax, 16;
       jz @nextLine;

       {$IFDEF FPC}vmovsd xmm3, [ecx + eax];{$ELSE}db $C5,$FB,$10,$1C,$01;{$ENDIF} 
       {$IFDEF FPC}vmulsd xmm3, xmm3, xmm3;{$ELSE}db $C5,$E3,$59,$DB;{$ENDIF} 
       {$IFDEF FPC}vaddsd xmm0, xmm0, xmm3;{$ELSE}db $C5,$FB,$58,$C3;{$ENDIF} 

       @nextLine:

       // next line:
       add ecx, esi;

   // loop y end
   dec ebx;
   jnz @@addforyloop;

   // build result
   {$IFDEF FPC}vextractf128 xmm2, ymm1, 1;{$ELSE}db $C4,$E3,$7D,$19,$CA,$01;{$ENDIF} 
   {$IFDEF FPC}vhaddpd xmm1, xmm1, xmm2;{$ELSE}db $C5,$F1,$7C,$CA;{$ENDIF} 
   {$IFDEF FPC}vaddpd xmm0, xmm0, xmm1;{$ELSE}db $C5,$F9,$58,$C1;{$ENDIF} 
   {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} 

   {$IFDEF FPC}vmovsd Result, xmm0;{$ELSE}db $C5,$FB,$11,$45,$E8;{$ENDIF} 

   // epilog claenup stack
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;
end;

function AVXMatrixElementwiseNorm2UnAligned(dest : PDouble; const LineWidth : TASMNativeInt; Width, height : TASMNativeInt) : double;
begin
asm
   // prolog - simulate stack
   push ebx;
   push edi;
   push esi;

   mov esi, lineWidth;
   //iters := -width*sizeof(double);
   mov edi, width;
   imul edi, -8;

   // helper registers for the mt1, mt2 and dest pointers
   mov ecx, dest;
   sub ecx, edi;

   {$IFDEF FPC}vxorpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$57,$C0;{$ENDIF} 
   {$IFDEF FPC}vxorpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$57,$C9;{$ENDIF} 

   mov ebx, height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov eax, edi;
       @addforxloop:
           add eax, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [ecx + eax];

           // mul add:
           {$IFDEF FPC}vmovupd ymm3, [ecx + eax - 128];{$ELSE}db $C5,$FD,$10,$5C,$01,$80;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm3, ymm3, ymm3;{$ELSE}db $C5,$E5,$59,$DB;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm1, ymm1, ymm3;{$ELSE}db $C5,$F5,$58,$CB;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm2, [ecx + eax - 96];{$ELSE}db $C5,$FD,$10,$54,$01,$A0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm2, ymm2, ymm2;{$ELSE}db $C5,$ED,$59,$D2;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm1, ymm1, ymm2;{$ELSE}db $C5,$F5,$58,$CA;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm3, [ecx + eax - 64];{$ELSE}db $C5,$FD,$10,$5C,$01,$C0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm3, ymm3, ymm3;{$ELSE}db $C5,$E5,$59,$DB;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm1, ymm1, ymm3;{$ELSE}db $C5,$F5,$58,$CB;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm2, [ecx + eax - 32];{$ELSE}db $C5,$FD,$10,$54,$01,$E0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm2, ymm2, ymm2;{$ELSE}db $C5,$ED,$59,$D2;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm1, ymm1, ymm2;{$ELSE}db $C5,$F5,$58,$CA;{$ENDIF} 
       jmp @addforxloop

       @loopEnd:

       sub eax, 128;

       jz @nextLine;

       @addforxloop2:
           add eax, 16;
           jg @@addforxloop2end;

           {$IFDEF FPC}vmovupd xmm3, [ecx + eax - 16];{$ELSE}db $C5,$F9,$10,$5C,$01,$F0;{$ENDIF} 
           {$IFDEF FPC}vmulpd xmm3, xmm3, xmm3;{$ELSE}db $C5,$E1,$59,$DB;{$ENDIF} 
           {$IFDEF FPC}vaddpd xmm0, xmm0, xmm3;{$ELSE}db $C5,$F9,$58,$C3;{$ENDIF} 
       jmp @addforxloop2;

       @@addforxloop2end:

       sub eax, 16;
       jz @nextLine;

       {$IFDEF FPC}vmovsd xmm3, [ecx + eax];{$ELSE}db $C5,$FB,$10,$1C,$01;{$ENDIF} 
       {$IFDEF FPC}vmulsd xmm3, xmm3, xmm3;{$ELSE}db $C5,$E3,$59,$DB;{$ENDIF} 
       {$IFDEF FPC}vaddsd xmm0, xmm0, xmm3;{$ELSE}db $C5,$FB,$58,$C3;{$ENDIF} 

       @nextLine:

       // next line:
       add ecx, esi;

   // loop y end
   dec ebx;
   jnz @@addforyloop;

   // build result
   {$IFDEF FPC}vextractf128 xmm2, ymm1, 1;{$ELSE}db $C4,$E3,$7D,$19,$CA,$01;{$ENDIF} 
   {$IFDEF FPC}vhaddpd xmm1, xmm1, xmm2;{$ELSE}db $C5,$F1,$7C,$CA;{$ENDIF} 
   {$IFDEF FPC}vaddpd xmm0, xmm0, xmm1;{$ELSE}db $C5,$F9,$58,$C1;{$ENDIF} 
   {$IFDEF FPC}vhaddpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$7C,$C0;{$ENDIF} 

   {$IFDEF FPC}vmovsd Result, xmm0;{$ELSE}db $C5,$FB,$11,$45,$E8;{$ENDIF} 

   // epilog claenup stack
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;
end;

procedure AVXMatrixNormalizeRowAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
var tmp : double;
begin
asm
   // prolog - simulate stack
   push ebx;
   push edi;

   //iters := -width*sizeof(double);
   mov edi, width;
   imul edi, -8;

   // helper registers for the mt1, mt2 and dest pointers
   mov ecx, dest;
   mov edx, src;
   sub ecx, edi;
   sub edx, edi;

   mov ebx, height;
   @@addforyloop:
       {$IFDEF FPC}vxorpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$57,$C0;{$ENDIF} 
       {$IFDEF FPC}vxorpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$57,$C9;{$ENDIF} 

       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov eax, edi;
       @addforxloop:
           add eax, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [ecx + eax];

           // mul add:
           {$IFDEF FPC}vmovapd ymm3, [edx + eax - 128];{$ELSE}db $C5,$FD,$28,$5C,$02,$80;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm3, ymm3, ymm3;{$ELSE}db $C5,$E5,$59,$DB;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm1, ymm1, ymm3;{$ELSE}db $C5,$F5,$58,$CB;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm2, [edx + eax - 96];{$ELSE}db $C5,$FD,$28,$54,$02,$A0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm2, ymm2, ymm2;{$ELSE}db $C5,$ED,$59,$D2;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm1, ymm1, ymm2;{$ELSE}db $C5,$F5,$58,$CA;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm3, [edx + eax - 64];{$ELSE}db $C5,$FD,$28,$5C,$02,$C0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm3, ymm3, ymm3;{$ELSE}db $C5,$E5,$59,$DB;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm1, ymm1, ymm3;{$ELSE}db $C5,$F5,$58,$CB;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm2, [edx + eax - 32];{$ELSE}db $C5,$FD,$28,$54,$02,$E0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm2, ymm2, ymm2;{$ELSE}db $C5,$ED,$59,$D2;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm1, ymm1, ymm2;{$ELSE}db $C5,$F5,$58,$CA;{$ENDIF} 
       jmp @addforxloop

       @loopEnd:

       sub eax, 128;

       jz @nextLine;

       @addforxloop2:
           add eax, 16;
           jg @@addforxloop2end;

           {$IFDEF FPC}vmovapd xmm3, [edx + eax - 16];{$ELSE}db $C5,$F9,$28,$5C,$02,$F0;{$ENDIF} 
           {$IFDEF FPC}vmulpd xmm3, xmm3, xmm3;{$ELSE}db $C5,$E1,$59,$DB;{$ENDIF} 
           {$IFDEF FPC}vaddpd xmm0, xmm0, xmm3;{$ELSE}db $C5,$F9,$58,$C3;{$ENDIF} 
       jmp @addforxloop2;

       @@addforxloop2end:

       sub eax, 16;
       jz @nextLine;

       {$IFDEF FPC}vmovsd xmm3, [edx + eax];{$ELSE}db $C5,$FB,$10,$1C,$02;{$ENDIF} 
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
       lea eax, cOne;
       {$IFDEF FPC}vmovsd xmm1, [eax];{$ELSE}db $C5,$FB,$10,$08;{$ENDIF} 
       {$IFDEF FPC}vdivsd xmm1, xmm1, xmm0;{$ELSE}db $C5,$F3,$5E,$C8;{$ENDIF} 

       lea eax, tmp;
       {$IFDEF FPC}vmovsd [eax], xmm1;{$ELSE}db $C5,$FB,$11,$08;{$ENDIF} 
       {$IFDEF FPC}vbroadcastsd ymm2, [eax];   {$ELSE}db $C4,$E2,$7D,$19,$10;{$ENDIF} // need avx2

       //  normalize
       mov eax, edi;
       @addforxloop3:
           add eax, 128;
           jg @loopEnd2;

           // prefetch data...
           // prefetch [edx + eax];
           // prefetchw [ecx + eax];

           // mult:
           {$IFDEF FPC}vmovapd ymm0, [edx + eax - 128];{$ELSE}db $C5,$FD,$28,$44,$02,$80;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm0, ymm0, ymm2;{$ELSE}db $C5,$FD,$59,$C2;{$ENDIF} 
           {$IFDEF FPC}vmovapd [ecx + eax - 128], ymm0;{$ELSE}db $C5,$FD,$29,$44,$01,$80;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm1, [edx + eax - 96];{$ELSE}db $C5,$FD,$28,$4C,$02,$A0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm1, ymm1, ymm2;{$ELSE}db $C5,$F5,$59,$CA;{$ENDIF} 
           {$IFDEF FPC}vmovapd [ecx + eax - 96], ymm1;{$ELSE}db $C5,$FD,$29,$4C,$01,$A0;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm0, [edx + eax - 64];{$ELSE}db $C5,$FD,$28,$44,$02,$C0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm0, ymm0, ymm2;{$ELSE}db $C5,$FD,$59,$C2;{$ENDIF} 
           {$IFDEF FPC}vmovapd [ecx + eax - 64], ymm0;{$ELSE}db $C5,$FD,$29,$44,$01,$C0;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm1, [edx + eax - 32];{$ELSE}db $C5,$FD,$28,$4C,$02,$E0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm1, ymm1, ymm2;{$ELSE}db $C5,$F5,$59,$CA;{$ENDIF} 
           {$IFDEF FPC}vmovapd [ecx + eax - 32], ymm1;{$ELSE}db $C5,$FD,$29,$4C,$01,$E0;{$ENDIF} 

       jmp @addforxloop3

       @loopEnd2:

       sub eax, 128;

       jz @nextLine2;

       @addforxloop4:
           add eax, 16;
           jg @addforxloop4end;

           {$IFDEF FPC}vmovapd xmm0, [edx + eax - 16];{$ELSE}db $C5,$F9,$28,$44,$02,$F0;{$ENDIF} 
           {$IFDEF FPC}vmulpd xmm0, xmm0, xmm2;{$ELSE}db $C5,$F9,$59,$C2;{$ENDIF} 
           {$IFDEF FPC}vmovapd [ecx + eax - 16], xmm0;{$ELSE}db $C5,$F9,$29,$44,$01,$F0;{$ENDIF} 
       jmp @addforxloop4;

       @addforxloop4end:
       sub eax, 16;
       jz @nextLine2;

       {$IFDEF FPC}vmovsd xmm0, [edx + eax];{$ELSE}db $C5,$FB,$10,$04,$02;{$ENDIF} 
       {$IFDEF FPC}vmulsd xmm0, xmm0, xmm2;{$ELSE}db $C5,$FB,$59,$C2;{$ENDIF} 
       {$IFDEF FPC}vmovsd [ecx + eax], xmm0;{$ELSE}db $C5,$FB,$11,$04,$01;{$ENDIF} 

       @nextLine2:

       // next line:
       add ecx, destlineWidth;
       add edx, srcLineWidth;

   // loop y end
   dec ebx;
   jnz @@addforyloop;

   // epilog claenup stack
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop edi;
   pop ebx;
end;
end;

procedure AVXMatrixNormalizeRowUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
var tmp : double;
begin
asm
   // prolog - simulate stack
   push ebx;
   push edi;

   //iters := -width*sizeof(double);
   mov edi, width;
   imul edi, -8;

   // helper registers for the mt1, mt2 and dest pointers
   mov ecx, dest;
   mov edx, src;
   sub ecx, edi;
   sub edx, edi;

   mov ebx, height;
   @@addforyloop:
       {$IFDEF FPC}vxorpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$57,$C0;{$ENDIF} 
       {$IFDEF FPC}vxorpd ymm1, ymm1, ymm1;{$ELSE}db $C5,$F5,$57,$C9;{$ENDIF} 

       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov eax, edi;
       @addforxloop:
           add eax, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [ecx + eax];

           // mul add:
           {$IFDEF FPC}vmovupd ymm3, [edx + eax - 128];{$ELSE}db $C5,$FD,$10,$5C,$02,$80;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm3, ymm3, ymm3;{$ELSE}db $C5,$E5,$59,$DB;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm1, ymm1, ymm3;{$ELSE}db $C5,$F5,$58,$CB;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm2, [edx + eax - 96];{$ELSE}db $C5,$FD,$10,$54,$02,$A0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm2, ymm2, ymm2;{$ELSE}db $C5,$ED,$59,$D2;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm1, ymm1, ymm2;{$ELSE}db $C5,$F5,$58,$CA;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm3, [edx + eax - 64];{$ELSE}db $C5,$FD,$10,$5C,$02,$C0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm3, ymm3, ymm3;{$ELSE}db $C5,$E5,$59,$DB;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm1, ymm1, ymm3;{$ELSE}db $C5,$F5,$58,$CB;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm2, [edx + eax - 32];{$ELSE}db $C5,$FD,$10,$54,$02,$E0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm2, ymm2, ymm2;{$ELSE}db $C5,$ED,$59,$D2;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm1, ymm1, ymm2;{$ELSE}db $C5,$F5,$58,$CA;{$ENDIF} 
       jmp @addforxloop

       @loopEnd:

       sub eax, 128;

       jz @nextLine;

       @addforxloop2:
           add eax, 16;
           jg @@addforxloop2end;

           {$IFDEF FPC}vmovupd xmm3, [edx + eax - 16];{$ELSE}db $C5,$F9,$10,$5C,$02,$F0;{$ENDIF} 
           {$IFDEF FPC}vmulpd xmm3, xmm3, xmm3;{$ELSE}db $C5,$E1,$59,$DB;{$ENDIF} 
           {$IFDEF FPC}vaddpd xmm0, xmm0, xmm3;{$ELSE}db $C5,$F9,$58,$C3;{$ENDIF} 
       jmp @addforxloop2;

       @@addforxloop2end:

       sub eax, 16;
       jz @nextLine;

       {$IFDEF FPC}vmovsd xmm3, [edx + eax];{$ELSE}db $C5,$FB,$10,$1C,$02;{$ENDIF} 
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
       lea eax, cOne;
       {$IFDEF FPC}vmovsd xmm1, [eax];{$ELSE}db $C5,$FB,$10,$08;{$ENDIF} 
       {$IFDEF FPC}vdivsd xmm1, xmm1, xmm0;{$ELSE}db $C5,$F3,$5E,$C8;{$ENDIF} 

       lea eax, tmp;
       {$IFDEF FPC}vmovsd [eax], xmm1;{$ELSE}db $C5,$FB,$11,$08;{$ENDIF} 
       {$IFDEF FPC}vbroadcastsd ymm2, [eax];   {$ELSE}db $C4,$E2,$7D,$19,$10;{$ENDIF} // need avx2

       //  normalize
       mov eax, edi;
       @addforxloop3:
           add eax, 128;
           jg @loopEnd2;

           // prefetch data...
           // prefetch [edx + eax];
           // prefetchw [ecx + eax];

           // mult:
           {$IFDEF FPC}vmovupd ymm0, [edx + eax - 128];{$ELSE}db $C5,$FD,$10,$44,$02,$80;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm0, ymm0, ymm2;{$ELSE}db $C5,$FD,$59,$C2;{$ENDIF} 
           {$IFDEF FPC}vmovupd [ecx + eax - 128], ymm0;{$ELSE}db $C5,$FD,$11,$44,$01,$80;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm1, [edx + eax - 96];{$ELSE}db $C5,$FD,$10,$4C,$02,$A0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm1, ymm1, ymm2;{$ELSE}db $C5,$F5,$59,$CA;{$ENDIF} 
           {$IFDEF FPC}vmovupd [ecx + eax - 96], ymm1;{$ELSE}db $C5,$FD,$11,$4C,$01,$A0;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm0, [edx + eax - 64];{$ELSE}db $C5,$FD,$10,$44,$02,$C0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm0, ymm0, ymm2;{$ELSE}db $C5,$FD,$59,$C2;{$ENDIF} 
           {$IFDEF FPC}vmovupd [ecx + eax - 64], ymm0;{$ELSE}db $C5,$FD,$11,$44,$01,$C0;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm1, [edx + eax - 32];{$ELSE}db $C5,$FD,$10,$4C,$02,$E0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm1, ymm1, ymm2;{$ELSE}db $C5,$F5,$59,$CA;{$ENDIF} 
           {$IFDEF FPC}vmovupd [ecx + eax - 32], ymm1;{$ELSE}db $C5,$FD,$11,$4C,$01,$E0;{$ENDIF} 

       jmp @addforxloop3

       @loopEnd2:

       sub eax, 128;

       jz @nextLine2;

       @addforxloop4:
           add eax, 16;
           jg @addforxloop4end;

           {$IFDEF FPC}vmovupd xmm0, [edx + eax - 16];{$ELSE}db $C5,$F9,$10,$44,$02,$F0;{$ENDIF} 
           {$IFDEF FPC}vmulpd xmm0, xmm0, xmm2;{$ELSE}db $C5,$F9,$59,$C2;{$ENDIF} 
           {$IFDEF FPC}vmovupd [ecx + eax - 16], xmm0;{$ELSE}db $C5,$F9,$11,$44,$01,$F0;{$ENDIF} 
       jmp @addforxloop4;

       @addforxloop4end:
       sub eax, 16;
       jz @nextLine2;

       {$IFDEF FPC}vmovsd xmm0, [edx + eax];{$ELSE}db $C5,$FB,$10,$04,$02;{$ENDIF} 
       {$IFDEF FPC}vmulsd xmm0, xmm0, xmm2;{$ELSE}db $C5,$FB,$59,$C2;{$ENDIF} 
       {$IFDEF FPC}vmovsd [ecx + eax], xmm0;{$ELSE}db $C5,$FB,$11,$04,$01;{$ENDIF} 

       @nextLine2:

       // next line:
       add ecx, destlineWidth;
       add edx, srcLineWidth;

   // loop y end
   dec ebx;
   jnz @@addforyloop;

   // epilog claenup stack
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop edi;
   pop ebx;
end;
end;

procedure AVXMatrixNormalizeColumnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
begin
asm
   // prolog
   push ebx;
   push edi;
   push esi;

   // note: ecx = dest, RDX = destLineWidth, edx = src, R9 = srcLineWidth
   xor edi, edi;
   sub edi, height;
   imul edi, srcLineWidth;

   // helper registers for the mt1 and dest pointers
   mov ecx, dest;
   mov edx, src;
   sub edx, edi;

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
       mov eax, edi;
       @addforyloop4:
           {$IFDEF FPC}vmovapd ymm3, [edx + eax];{$ELSE}db $C5,$FD,$28,$1C,$02;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm3, ymm3, ymm3;{$ELSE}db $C5,$E5,$59,$DB;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm1, ymm1, ymm3;{$ELSE}db $C5,$F5,$58,$CB;{$ENDIF} 
       add eax, esi;
       jnz @addforyloop4;

       // normalization factor
       lea eax, cOne;
       {$IFDEF FPC}vbroadcastsd ymm2, [eax];{$ELSE}db $C4,$E2,$7D,$19,$10;{$ENDIF} 
       {$IFDEF FPC}vsqrtpd ymm1, ymm1;{$ELSE}db $C5,$FD,$51,$C9;{$ENDIF} 
       {$IFDEF FPC}vdivpd ymm0, ymm2, ymm1;{$ELSE}db $C5,$ED,$5E,$C1;{$ENDIF} 

       // normalize
       mov eax, edi;
       @addforyloop4_2:
           {$IFDEF FPC}vmovapd ymm1, [edx + eax];{$ELSE}db $C5,$FD,$28,$0C,$02;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm1, ymm1, ymm0;{$ELSE}db $C5,$F5,$59,$C8;{$ENDIF} 
           {$IFDEF FPC}vmovapd [ecx], ymm1;{$ELSE}db $C5,$FD,$29,$09;{$ENDIF} 

       add ecx, destLineWidth;
       add eax, esi;
       jnz @addforyloop4_2;

       // next columns:
       mov ecx, dest;
       add ecx, 32;
       mov dest, ecx;
       add edx, 32;

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
   mov eax, edi;
   @addforyloop2:
       {$IFDEF FPC}vmovapd xmm0, [edx + eax];{$ELSE}db $C5,$F9,$28,$04,$02;{$ENDIF} 
       {$IFDEF FPC}vmulpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$59,$C0;{$ENDIF} 
       {$IFDEF FPC}vaddpd xmm1, xmm1, xmm0;{$ELSE}db $C5,$F1,$58,$C8;{$ENDIF} 
   add eax, esi;
   jnz @addforyloop2;

   // normalization factor
   lea eax, cOne;
   {$IFDEF FPC}vmovddup xmm2, [eax];{$ELSE}db $C5,$FB,$12,$10;{$ENDIF} 
   {$IFDEF FPC}vsqrtpd xmm1, xmm1;{$ELSE}db $C5,$F9,$51,$C9;{$ENDIF} 
   {$IFDEF FPC}vdivpd xmm0, xmm2, xmm1;{$ELSE}db $C5,$E9,$5E,$C1;{$ENDIF} 

   // normalize
   mov eax, edi;
   @addforyloop2_2:
      {$IFDEF FPC}vmovapd xmm1, [edx + eax];{$ELSE}db $C5,$F9,$28,$0C,$02;{$ENDIF} 
      {$IFDEF FPC}vmulpd xmm1, xmm1, xmm0;{$ELSE}db $C5,$F1,$59,$C8;{$ENDIF} 
      {$IFDEF FPC}vmovapd [ecx], xmm1;{$ELSE}db $C5,$F9,$29,$09;{$ENDIF} 
   add ecx, destLineWidth;
   add eax, esi;
   jnz @addforyloop2_2;

   // next columns:
   mov ecx, dest;
   add ecx, 16;
   mov dest, ecx;
   add edx, 16;

   dec ebx;
   jnz @@endProc;

   @@lastcolumn:

   {$IFDEF FPC}vxorpd xmm1, xmm1, xmm1;{$ELSE}db $C5,$F1,$57,$C9;{$ENDIF} 

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov eax, edi;
   @addforyloop_1:
       {$IFDEF FPC}vmovsd xmm0, [edx + eax];{$ELSE}db $C5,$FB,$10,$04,$02;{$ENDIF} 
       {$IFDEF FPC}vmulsd xmm0, xmm0, xmm0;{$ELSE}db $C5,$FB,$59,$C0;{$ENDIF} 
       {$IFDEF FPC}vaddsd xmm1, xmm1, xmm0;{$ELSE}db $C5,$F3,$58,$C8;{$ENDIF} 
   add eax, esi;
   jnz @addforyloop_1;


   // build result
   lea eax, cOne;
   {$IFDEF FPC}vmovsd xmm2, [eax];{$ELSE}db $C5,$FB,$10,$10;{$ENDIF} 
   {$IFDEF FPC}vsqrtsd xmm1, xmm1, xmm1;{$ELSE}db $C5,$F3,$51,$C9;{$ENDIF} 
   {$IFDEF FPC}vdivsd xmm0, xmm2, xmm1;{$ELSE}db $C5,$EB,$5E,$C1;{$ENDIF} 

   // normalize last column
   mov eax, edi;
   @addforyloop2_1:
      {$IFDEF FPC}vmovsd xmm1, [edx + eax];{$ELSE}db $C5,$FB,$10,$0C,$02;{$ENDIF} 
      {$IFDEF FPC}vmulsd xmm1, xmm1, xmm0;{$ELSE}db $C5,$F3,$59,$C8;{$ENDIF} 
      {$IFDEF FPC}vmovsd [ecx], xmm1;{$ELSE}db $C5,$FB,$11,$09;{$ENDIF} 
   add ecx, destLineWidth;
   add eax, esi;
   jnz @addforyloop2_1;

   @@endProc:

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;
end;


procedure AVXMatrixNormalizeColumnUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
begin
asm
   // prolog
   push ebx;
   push edi;
   push esi;

   // note: ecx = dest, RDX = destLineWidth, edx = src, R9 = srcLineWidth
   xor edi, edi;
   sub edi, height;
   imul edi, srcLineWidth;

   // helper registers for the mt1 and dest pointers
   mov ecx, dest;
   mov edx, src;
   sub edx, edi;

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
       mov eax, edi;
       @addforyloop4:
           {$IFDEF FPC}vmovupd ymm3, [edx + eax];{$ELSE}db $C5,$FD,$10,$1C,$02;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm3, ymm3, ymm3;{$ELSE}db $C5,$E5,$59,$DB;{$ENDIF} 
           {$IFDEF FPC}vaddpd ymm1, ymm1, ymm3;{$ELSE}db $C5,$F5,$58,$CB;{$ENDIF} 
       add eax, esi;
       jnz @addforyloop4;

       // normalization factor
       lea eax, cOne;
       {$IFDEF FPC}vbroadcastsd ymm2, [eax];{$ELSE}db $C4,$E2,$7D,$19,$10;{$ENDIF} 
       {$IFDEF FPC}vsqrtpd ymm1, ymm1;{$ELSE}db $C5,$FD,$51,$C9;{$ENDIF} 
       {$IFDEF FPC}vdivpd ymm0, ymm2, ymm1;{$ELSE}db $C5,$ED,$5E,$C1;{$ENDIF} 

       // normalize
       mov eax, edi;
       @addforyloop4_2:
           {$IFDEF FPC}vmovupd ymm1, [edx + eax];{$ELSE}db $C5,$FD,$10,$0C,$02;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm1, ymm1, ymm0;{$ELSE}db $C5,$F5,$59,$C8;{$ENDIF} 
           {$IFDEF FPC}vmovupd [ecx], ymm1;{$ELSE}db $C5,$FD,$11,$09;{$ENDIF} 

       add ecx, destLineWidth;
       add eax, esi;
       jnz @addforyloop4_2;

       // next columns:
       mov ecx, dest;
       add ecx, 32;
       mov dest, ecx;
       add edx, 32;

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
   mov eax, edi;
   @addforyloop2:
       {$IFDEF FPC}vmovupd xmm0, [edx + eax];{$ELSE}db $C5,$F9,$10,$04,$02;{$ENDIF} 
       {$IFDEF FPC}vmulpd xmm0, xmm0, xmm0;{$ELSE}db $C5,$F9,$59,$C0;{$ENDIF} 
       {$IFDEF FPC}vaddpd xmm1, xmm1, xmm0;{$ELSE}db $C5,$F1,$58,$C8;{$ENDIF} 
   add eax, esi;
   jnz @addforyloop2;

   // normalization factor
   lea eax, [cOne];
   {$IFDEF FPC}vmovddup xmm2, [eax];{$ELSE}db $C5,$FB,$12,$10;{$ENDIF} 
   {$IFDEF FPC}vsqrtpd xmm1, xmm1;{$ELSE}db $C5,$F9,$51,$C9;{$ENDIF} 
   {$IFDEF FPC}vdivpd xmm0, xmm2, xmm1;{$ELSE}db $C5,$E9,$5E,$C1;{$ENDIF} 

   // normalize
   mov eax, edi;
   @addforyloop2_2:
      {$IFDEF FPC}vmovupd xmm1, [edx + eax];{$ELSE}db $C5,$F9,$10,$0C,$02;{$ENDIF} 
      {$IFDEF FPC}vmulpd xmm1, xmm1, xmm0;{$ELSE}db $C5,$F1,$59,$C8;{$ENDIF} 
      {$IFDEF FPC}vmovupd [ecx], xmm1;{$ELSE}db $C5,$F9,$11,$09;{$ENDIF} 
   add ecx, destLineWidth;
   add eax, esi;
   jnz @addforyloop2_2;

   // next columns:
   mov ecx, dest;
   add ecx, 16;
   mov dest, ecx;
   add edx, 16;

   dec ebx;
   jnz @@endProc;

   @@lastcolumn:

   {$IFDEF FPC}vxorpd xmm1, xmm1, xmm1;{$ELSE}db $C5,$F1,$57,$C9;{$ENDIF} 

   // for y := 0 to height - 1;
   // prepare for reverse loop
   mov eax, edi;
   @addforyloop_1:
       {$IFDEF FPC}vmovsd xmm0, [edx + eax];{$ELSE}db $C5,$FB,$10,$04,$02;{$ENDIF} 
       {$IFDEF FPC}vmulsd xmm0, xmm0, xmm0;{$ELSE}db $C5,$FB,$59,$C0;{$ENDIF} 
       {$IFDEF FPC}vaddsd xmm1, xmm1, xmm0;{$ELSE}db $C5,$F3,$58,$C8;{$ENDIF} 
   add eax, esi;
   jnz @addforyloop_1;


   // build result
   lea eax, cOne;
   {$IFDEF FPC}vmovsd xmm2, [eax];{$ELSE}db $C5,$FB,$10,$10;{$ENDIF} 
   {$IFDEF FPC}vsqrtsd xmm1, xmm1, xmm1;{$ELSE}db $C5,$F3,$51,$C9;{$ENDIF} 
   {$IFDEF FPC}vdivsd xmm0, xmm2, xmm1;{$ELSE}db $C5,$EB,$5E,$C1;{$ENDIF} 

   // normalize last column
   mov eax, edi;
   @addforyloop2_1:
      {$IFDEF FPC}vmovsd xmm1, [edx + eax];{$ELSE}db $C5,$FB,$10,$0C,$02;{$ENDIF} 
      {$IFDEF FPC}vmulsd xmm1, xmm1, xmm0;{$ELSE}db $C5,$F3,$59,$C8;{$ENDIF} 
      {$IFDEF FPC}vmovsd [ecx], xmm1;{$ELSE}db $C5,$FB,$11,$09;{$ENDIF} 
   add ecx, destLineWidth;
   add eax, esi;
   jnz @addforyloop2_1;

   @@endProc:

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;
end;

{$ENDIF}

end.
