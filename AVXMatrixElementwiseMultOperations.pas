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

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}
{$IFNDEF x64}

uses MatrixConst;

procedure AVXMatrixElemMultAligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
procedure AVXMatrixElemMultUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);

procedure AVXMatrixElemDivAligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
procedure AVXMatrixElemDivUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);

{$ENDIF}

implementation

{$IFNDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$ENDIF}

procedure AVXMatrixElemMultAligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
begin
asm
   // prolog - simulate stack
   push ebx;
   push edi;
   push esi;

   //iters := -width*sizeof(double);
   mov edi, width;
   imul edi, -8;

   // helper registers for the mt1, mt2 and dest pointers
   mov ecx, dest;
   mov edx, mt1;
   mov esi, mt2;

   sub ecx, edi;
   sub edx, edi;
   sub esi, edi;

   // for y := 0 to height - 1:
   mov ebx, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov eax, edi;
       @addforxloop:
           add eax, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [r8 + rax];
           // prefetch [r9 + rax];

           // mult:
           {$IFDEF FPC}vmovapd ymm0, [edx + eax - 128];{$ELSE}db $C5,$FD,$28,$44,$02,$80;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm0, ymm0, [esi + eax - 128];{$ELSE}db $C5,$FD,$59,$44,$06,$80;{$ENDIF} 

           {$IFDEF FPC}vmovapd [ecx + eax - 128], ymm0;{$ELSE}db $C5,$FD,$29,$44,$01,$80;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm1, [edx + eax - 96];{$ELSE}db $C5,$FD,$28,$4C,$02,$A0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm1, ymm1, [esi + eax - 96];{$ELSE}db $C5,$F5,$59,$4C,$06,$A0;{$ENDIF} 

           {$IFDEF FPC}vmovapd [ecx + eax - 96], ymm1;{$ELSE}db $C5,$FD,$29,$4C,$01,$A0;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm2, [edx + eax - 64];{$ELSE}db $C5,$FD,$28,$54,$02,$C0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm2, ymm2, [esi + eax - 64];{$ELSE}db $C5,$ED,$59,$54,$06,$C0;{$ENDIF} 

           {$IFDEF FPC}vmovapd [ecx + eax - 64], ymm2;{$ELSE}db $C5,$FD,$29,$54,$01,$C0;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm3, [edx + eax - 32];{$ELSE}db $C5,$FD,$28,$5C,$02,$E0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm3, ymm3, [esi + eax - 32];{$ELSE}db $C5,$E5,$59,$5C,$06,$E0;{$ENDIF} 

           {$IFDEF FPC}vmovapd [ecx + eax - 32], ymm3;{$ELSE}db $C5,$FD,$29,$5C,$01,$E0;{$ENDIF} 
       jmp @addforxloop

       @loopEnd:

       sub eax, 128;

       jz @nextLine;

       @addforxloop2:
           add eax, 16;
           jg @@lastElem;

           {$IFDEF FPC}vmovapd xmm0, [edx + eax - 16];{$ELSE}db $C5,$F9,$28,$44,$02,$F0;{$ENDIF} 
           {$IFDEF FPC}vmulpd xmm0, xmm0, [esi + eax - 16];{$ELSE}db $C5,$F9,$59,$44,$06,$F0;{$ENDIF} 

           {$IFDEF FPC}vmovapd [ecx + eax - 16], xmm0;{$ELSE}db $C5,$F9,$29,$44,$01,$F0;{$ENDIF} 
       jmp @addforxloop2;

       @@lastElem:
       sub eax, 16;

       jz @nextLine;

       {$IFDEF FPC}vmovsd xmm0, [edx + eax];{$ELSE}db $C5,$FB,$10,$04,$02;{$ENDIF} 
       {$IFDEF FPC}vmulsd xmm0, xmm0, [esi + eax];{$ELSE}db $C5,$FB,$59,$04,$06;{$ENDIF} 
       {$IFDEF FPC}vmovsd [ecx + eax], xmm0;{$ELSE}db $C5,$FB,$11,$04,$01;{$ENDIF} 

       @nextLine:

       // next line:
       add edx, LineWidth1;
       add esi, LineWidth2;
       add ecx, destLineWidth;

   // loop y end
   dec ebx;
   jnz @@addforyloop;

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;
end;

procedure AVXMatrixElemMultUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
begin
asm
   // prolog - simulate stack
   push ebx;
   push edi;
   push esi;

   //iters := -width*sizeof(double);
   mov edi, width;
   imul edi, -8;

   // helper registers for the mt1, mt2 and dest pointers
   mov ecx, dest;
   mov edx, mt1;
   mov esi, mt2;

   sub ecx, edi;
   sub edx, edi;
   sub esi, edi;

   // for y := 0 to height - 1:
   mov ebx, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov eax, edi;
       @addforxloop:
           add eax, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [edx + eax];
           // prefetch [esi + eax];

           // mult:
           {$IFDEF FPC}vmovupd ymm0, [edx + eax - 128];{$ELSE}db $C5,$FD,$10,$44,$02,$80;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm1, [esi + eax - 128];{$ELSE}db $C5,$FD,$10,$4C,$06,$80;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$59,$C1;{$ENDIF} 

           {$IFDEF FPC}vmovupd [ecx + eax - 128], ymm0;{$ELSE}db $C5,$FD,$11,$44,$01,$80;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm2, [edx + eax - 96];{$ELSE}db $C5,$FD,$10,$54,$02,$A0;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm3, [esi + eax - 96];{$ELSE}db $C5,$FD,$10,$5C,$06,$A0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm2, ymm2, ymm3;{$ELSE}db $C5,$ED,$59,$D3;{$ENDIF} 

           {$IFDEF FPC}vmovupd [ecx + eax - 96], ymm2;{$ELSE}db $C5,$FD,$11,$54,$01,$A0;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm0, [edx + eax - 64];{$ELSE}db $C5,$FD,$10,$44,$02,$C0;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm1, [esi + eax - 64];{$ELSE}db $C5,$FD,$10,$4C,$06,$C0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$59,$C1;{$ENDIF} 

           {$IFDEF FPC}vmovupd [ecx + eax - 64], ymm0;{$ELSE}db $C5,$FD,$11,$44,$01,$C0;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm2, [edx + eax - 32];{$ELSE}db $C5,$FD,$10,$54,$02,$E0;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm3, [esi + eax - 32];{$ELSE}db $C5,$FD,$10,$5C,$06,$E0;{$ENDIF} 
           {$IFDEF FPC}vmulpd ymm2, ymm2, ymm3;{$ELSE}db $C5,$ED,$59,$D3;{$ENDIF} 

           {$IFDEF FPC}vmovupd [ecx + eax - 32], ymm2;{$ELSE}db $C5,$FD,$11,$54,$01,$E0;{$ENDIF} 
       jmp @addforxloop

       @loopEnd:

       sub eax, 128;

       jz @nextLine;

       @addforxloop2:
           add eax, 16;
           jg @@lastElem;

           {$IFDEF FPC}vmovupd xmm0, [edx + eax - 16];{$ELSE}db $C5,$F9,$10,$44,$02,$F0;{$ENDIF} 
           {$IFDEF FPC}vmovupd xmm1, [esi + eax - 16];{$ELSE}db $C5,$F9,$10,$4C,$06,$F0;{$ENDIF} 
           {$IFDEF FPC}vmulpd xmm0, xmm0, xmm1;{$ELSE}db $C5,$F9,$59,$C1;{$ENDIF} 

           {$IFDEF FPC}vmovupd [ecx + eax - 16], xmm0;{$ELSE}db $C5,$F9,$11,$44,$01,$F0;{$ENDIF} 
       jmp @addforxloop2;

       @@lastElem:
       sub eax, 16;

       jz @nextLine;

       {$IFDEF FPC}vmovsd xmm0, [edx + eax];{$ELSE}db $C5,$FB,$10,$04,$02;{$ENDIF} 
       {$IFDEF FPC}vmulsd xmm0, xmm0, [esi + eax];{$ELSE}db $C5,$FB,$59,$04,$06;{$ENDIF} 
       {$IFDEF FPC}vmovsd [ecx + eax], xmm0;{$ELSE}db $C5,$FB,$11,$04,$01;{$ENDIF} 

       @nextLine:

       // next line:
       add edx, LineWidth1;
       add esi, LineWidth2;
       add ecx, destLineWidth;

   // loop y end
   dec ebx;
   jnz @@addforyloop;

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;
end;

// ##############################################################
// #### Elementwise divide of matrix 1 to matrix 2
// ##############################################################

procedure AVXMatrixElemDivAligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
begin
asm
   // prolog - simulate stack
   push ebx;
   push edi;
   push esi;

   //iters := -width*sizeof(double);
   mov edi, width;
   imul edi, -8;

   // helper registers for the mt1, mt2 and dest pointers
   mov ecx, dest;
   mov edx, mt1;
   mov esi, mt2;

   sub edx, edi;
   sub esi, edi;
   sub ecx, edi;

   // for y := 0 to height - 1:
   mov ebx, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov eax, edi;
       @addforxloop:
           add eax, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [edx + eax];
           // prefetch [esi + eax];

           // mult:
           {$IFDEF FPC}vmovapd ymm0, [edx + eax - 128];{$ELSE}db $C5,$FD,$28,$44,$02,$80;{$ENDIF} 
           {$IFDEF FPC}vdivpd ymm0, ymm0, [esi + eax - 128];{$ELSE}db $C5,$FD,$5E,$44,$06,$80;{$ENDIF} 

           {$IFDEF FPC}vmovapd [ecx + eax - 128], ymm0;{$ELSE}db $C5,$FD,$29,$44,$01,$80;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm1, [edx + eax - 96];{$ELSE}db $C5,$FD,$28,$4C,$02,$A0;{$ENDIF} 
           {$IFDEF FPC}vdivpd ymm1, ymm1, [esi + eax - 96];{$ELSE}db $C5,$F5,$5E,$4C,$06,$A0;{$ENDIF} 

           {$IFDEF FPC}vmovapd [ecx + eax - 96], ymm1;{$ELSE}db $C5,$FD,$29,$4C,$01,$A0;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm2, [edx + eax - 64];{$ELSE}db $C5,$FD,$28,$54,$02,$C0;{$ENDIF} 
           {$IFDEF FPC}vdivpd ymm2, ymm2, [esi + eax - 64];{$ELSE}db $C5,$ED,$5E,$54,$06,$C0;{$ENDIF} 

           {$IFDEF FPC}vmovapd [ecx + eax - 64], ymm2;{$ELSE}db $C5,$FD,$29,$54,$01,$C0;{$ENDIF} 

           {$IFDEF FPC}vmovapd ymm3, [edx + eax - 32];{$ELSE}db $C5,$FD,$28,$5C,$02,$E0;{$ENDIF} 
           {$IFDEF FPC}vdivpd ymm3, ymm3, [esi + eax - 32];{$ELSE}db $C5,$E5,$5E,$5C,$06,$E0;{$ENDIF} 

           {$IFDEF FPC}vmovapd [ecx + eax - 32], ymm3;{$ELSE}db $C5,$FD,$29,$5C,$01,$E0;{$ENDIF} 
       jmp @addforxloop

       @loopEnd:

       sub eax, 128;

       jz @nextLine;

       @addforxloop2:
           add eax, 16;
           jg @@lastElem;

           {$IFDEF FPC}vmovapd xmm0, [edx + eax - 16];{$ELSE}db $C5,$F9,$28,$44,$02,$F0;{$ENDIF} 
           {$IFDEF FPC}vdivpd xmm0, xmm0, [esi + eax - 16];{$ELSE}db $C5,$F9,$5E,$44,$06,$F0;{$ENDIF} 

           {$IFDEF FPC}vmovapd [ecx + eax - 16], xmm0;{$ELSE}db $C5,$F9,$29,$44,$01,$F0;{$ENDIF} 
       jmp @addforxloop2;

       @@lastElem:
       sub eax, 16;

       jz @nextLine;

       {$IFDEF FPC}vmovsd xmm0, [edx + eax];{$ELSE}db $C5,$FB,$10,$04,$02;{$ENDIF} 
       {$IFDEF FPC}vdivsd xmm0, xmm0, [esi + eax];{$ELSE}db $C5,$FB,$5E,$04,$06;{$ENDIF} 
       {$IFDEF FPC}vmovsd [ecx + eax], xmm0;{$ELSE}db $C5,$FB,$11,$04,$01;{$ENDIF} 

       @nextLine:

       // next line:
       add edx, LineWidth1;
       add esi, LineWidth2;
       add ecx, destLineWidth;

   // loop y end
   dec ebx;
   jnz @@addforyloop;

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;
end;

procedure AVXMatrixElemDivUnAligned(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
begin
asm
   // prolog - simulate stack
   push ebx;
   push edi;
   push esi;

   //iters := -width*sizeof(double);
   mov edi, width;
   imul edi, -8;

   // helper registers for the mt1, mt2 and dest pointers
   mov ecx, dest;
   mov edx, mt1;
   mov esi, mt2;

   sub edx, edi;
   sub esi, edi;
   sub ecx, edi;

   // for y := 0 to height - 1:
   mov ebx, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov eax, edi;
       @addforxloop:
           add eax, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [edx + eax];
           // prefetch [esi + eax];

           // mult:
           {$IFDEF FPC}vmovupd ymm0, [edx + eax - 128];{$ELSE}db $C5,$FD,$10,$44,$02,$80;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm1, [esi + eax - 128];{$ELSE}db $C5,$FD,$10,$4C,$06,$80;{$ENDIF} 
           {$IFDEF FPC}vdivpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$5E,$C1;{$ENDIF} 

           {$IFDEF FPC}vmovupd [ecx + eax - 128], ymm0;{$ELSE}db $C5,$FD,$11,$44,$01,$80;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm2, [edx + eax - 96];{$ELSE}db $C5,$FD,$10,$54,$02,$A0;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm3, [esi + eax - 96];{$ELSE}db $C5,$FD,$10,$5C,$06,$A0;{$ENDIF} 
           {$IFDEF FPC}vdivpd ymm2, ymm2, ymm3;{$ELSE}db $C5,$ED,$5E,$D3;{$ENDIF} 

           {$IFDEF FPC}vmovupd [ecx + eax - 96], ymm2;{$ELSE}db $C5,$FD,$11,$54,$01,$A0;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm0, [edx + eax - 64];{$ELSE}db $C5,$FD,$10,$44,$02,$C0;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm1, [esi + eax - 64];{$ELSE}db $C5,$FD,$10,$4C,$06,$C0;{$ENDIF} 
           {$IFDEF FPC}vdivpd ymm0, ymm0, ymm1;{$ELSE}db $C5,$FD,$5E,$C1;{$ENDIF} 

           {$IFDEF FPC}vmovupd [ecx + eax - 64], ymm0;{$ELSE}db $C5,$FD,$11,$44,$01,$C0;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm2, [edx + eax - 32];{$ELSE}db $C5,$FD,$10,$54,$02,$E0;{$ENDIF} 
           {$IFDEF FPC}vmovupd ymm3, [esi + eax - 32];{$ELSE}db $C5,$FD,$10,$5C,$06,$E0;{$ENDIF} 
           {$IFDEF FPC}vdivpd ymm2, ymm2, ymm3;{$ELSE}db $C5,$ED,$5E,$D3;{$ENDIF} 

           {$IFDEF FPC}vmovupd [ecx + eax - 32], ymm2;{$ELSE}db $C5,$FD,$11,$54,$01,$E0;{$ENDIF} 
       jmp @addforxloop

       @loopEnd:

       sub eax, 128;

       jz @nextLine;

       @addforxloop2:
           add eax, 16;
           jg @@lastElem;

           {$IFDEF FPC}vmovupd xmm0, [edx + eax - 16];{$ELSE}db $C5,$F9,$10,$44,$02,$F0;{$ENDIF} 
           {$IFDEF FPC}vmovupd xmm1, [esi + eax - 16];{$ELSE}db $C5,$F9,$10,$4C,$06,$F0;{$ENDIF} 
           {$IFDEF FPC}vdivpd xmm0, xmm0, xmm1;{$ELSE}db $C5,$F9,$5E,$C1;{$ENDIF} 

           {$IFDEF FPC}vmovupd [ecx + eax - 16], xmm0;{$ELSE}db $C5,$F9,$11,$44,$01,$F0;{$ENDIF} 
       jmp @addforxloop2;

       @@lastElem:
       sub eax, 16;

       jz @nextLine;

       {$IFDEF FPC}vmovsd xmm0, [edx + eax];{$ELSE}db $C5,$FB,$10,$04,$02;{$ENDIF} 
       {$IFDEF FPC}vdivsd xmm0, xmm0, [esi + eax];{$ELSE}db $C5,$FB,$5E,$04,$06;{$ENDIF} 
       {$IFDEF FPC}vmovsd [ecx + eax], xmm0;{$ELSE}db $C5,$FB,$11,$04,$01;{$ENDIF} 

       @nextLine:

       // next line:
       add edx, LineWidth1;
       add esi, LineWidth2;
       add ecx, destLineWidth;

   // loop y end
   dec ebx;
   jnz @@addforyloop;

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;
end;

{$ENDIF}

end.
