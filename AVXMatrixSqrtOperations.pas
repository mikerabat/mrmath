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


unit AVXMatrixSqrtOperations;

// #####################################################
// #### SQRT opertaion applied to every element in a matrix
// #####################################################

interface

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}
{$IFNDEF x64}

uses MatrixConst;

procedure AVXMatrixSQRTAligned(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt);
procedure AVXMatrixSQRTUnAligned(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt);

{$ENDIF}

implementation

{$IFNDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$ENDIF}

procedure AVXMatrixSQRTAligned(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt);
begin
asm
   push edi;
   push esi;

   //iters := -width*sizeof(double);
   mov edi, width;
   imul edi, -8;

   // helper registers dest pointers
   mov edx, LineWidth;
   mov ecx, dest;
   sub ecx, edi;

   // for y := 0 to height - 1:
   mov esi, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov eax, edi;
       @addforxloop:
           add eax, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetchw [ecx + eax];

           // elementwise sqrt
           {$IFDEF FPC}vsqrtpd ymm0, [ecx + eax - 128];{$ELSE}db $C5,$FD,$51,$44,$01,$80;{$ENDIF} 
           {$IFDEF FPC}vmovapd [ecx + eax - 128], ymm0;{$ELSE}db $C5,$FD,$29,$44,$01,$80;{$ENDIF} 

           {$IFDEF FPC}vsqrtpd ymm1, [ecx + eax - 96];{$ELSE}db $C5,$FD,$51,$4C,$01,$A0;{$ENDIF} 
           {$IFDEF FPC}vmovapd [ecx + eax - 96], ymm1;{$ELSE}db $C5,$FD,$29,$4C,$01,$A0;{$ENDIF} 

           {$IFDEF FPC}vsqrtpd ymm0, [ecx + eax - 64];{$ELSE}db $C5,$FD,$51,$44,$01,$C0;{$ENDIF} 
           {$IFDEF FPC}vmovapd [ecx + eax - 64], ymm0;{$ELSE}db $C5,$FD,$29,$44,$01,$C0;{$ENDIF} 

           {$IFDEF FPC}vsqrtpd ymm1, [ecx + eax - 32];{$ELSE}db $C5,$FD,$51,$4C,$01,$E0;{$ENDIF} 
           {$IFDEF FPC}vmovapd [ecx + eax - 32], ymm1;{$ELSE}db $C5,$FD,$29,$4C,$01,$E0;{$ENDIF} 

       jmp @addforxloop

       @loopEnd:

       sub eax, 128;

       jz @nextLine;

       @addforxloop2:
           add eax, 16;
           jg @addforxloop2end;

           {$IFDEF FPC}vsqrtpd xmm0, [ecx + eax - 16];{$ELSE}db $C5,$F9,$51,$44,$01,$F0;{$ENDIF} 
           {$IFDEF FPC}vmovapd [ecx + eax - 16], xmm0;{$ELSE}db $C5,$F9,$29,$44,$01,$F0;{$ENDIF} 
       jmp @addforxloop2;

       @addforxloop2end:

       sub eax, 16;

       jz @nextLine;

       {$IFDEF FPC}vmovsd xmm0, [ecx + eax];{$ELSE}db $C5,$FB,$10,$04,$01;{$ENDIF} 
       {$IFDEF FPC}vsqrtsd xmm0, xmm0, xmm0;{$ELSE}db $C5,$FB,$51,$C0;{$ENDIF} 
       {$IFDEF FPC}vmovsd [ecx + eax], xmm0;{$ELSE}db $C5,$FB,$11,$04,$01;{$ENDIF} 

       @nextLine:

       // next line:
       add ecx, edx;

   // loop y end
   dec esi;
   jnz @@addforyloop;

   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 
   pop esi;
   pop edi;
end;
end;

procedure AVXMatrixSQRTUnAligned(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt);
begin
asm
   push edi;
   push esi

   //iters := -width*sizeof(double);
   mov edi, width;
   imul edi, -8;

   // helper registers dest pointers
   mov ecx, dest;
   sub ecx, edi;
   mov edx, LineWidth;

   // for y := 0 to height - 1:
   mov esi, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov eax, edi;
       @addforxloop:
           add eax, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetchw [ecx + eax];

           // elementwise sqrt
           {$IFDEF FPC}vmovupd ymm2, [ecx + eax - 128];{$ELSE}db $C5,$FD,$10,$54,$01,$80;{$ENDIF} 
           {$IFDEF FPC}vsqrtpd ymm0, ymm2;{$ELSE}db $C5,$FD,$51,$C2;{$ENDIF} 
           {$IFDEF FPC}vmovupd [ecx + eax - 128], ymm0;{$ELSE}db $C5,$FD,$11,$44,$01,$80;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm3, [ecx + eax - 96];{$ELSE}db $C5,$FD,$10,$5C,$01,$A0;{$ENDIF} 
           {$IFDEF FPC}vsqrtpd ymm1, ymm3;{$ELSE}db $C5,$FD,$51,$CB;{$ENDIF} 
           {$IFDEF FPC}vmovupd [ecx + eax - 96], ymm1;{$ELSE}db $C5,$FD,$11,$4C,$01,$A0;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm2, [ecx + eax - 64];{$ELSE}db $C5,$FD,$10,$54,$01,$C0;{$ENDIF} 
           {$IFDEF FPC}vsqrtpd ymm0, ymm2;{$ELSE}db $C5,$FD,$51,$C2;{$ENDIF} 
           {$IFDEF FPC}vmovupd [ecx + eax - 64], ymm0;{$ELSE}db $C5,$FD,$11,$44,$01,$C0;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm3, [ecx + eax - 32];{$ELSE}db $C5,$FD,$10,$5C,$01,$E0;{$ENDIF} 
           {$IFDEF FPC}vsqrtpd ymm1, ymm3;{$ELSE}db $C5,$FD,$51,$CB;{$ENDIF} 
           {$IFDEF FPC}vmovupd [ecx + eax - 32], ymm1;{$ELSE}db $C5,$FD,$11,$4C,$01,$E0;{$ENDIF} 

       jmp @addforxloop

       @loopEnd:

       sub eax, 128;

       jz @nextLine;

       @addforxloop2:
           add eax, 16;
           jg @addforxloop2end;

           {$IFDEF FPC}vmovupd xmm1, [ecx + eax - 16];{$ELSE}db $C5,$F9,$10,$4C,$01,$F0;{$ENDIF} 
           {$IFDEF FPC}vsqrtpd xmm0, xmm1;{$ELSE}db $C5,$F9,$51,$C1;{$ENDIF} 
           {$IFDEF FPC}vmovupd [ecx + eax - 16], xmm0;{$ELSE}db $C5,$F9,$11,$44,$01,$F0;{$ENDIF} 
       jmp @addforxloop2;

       @addforxloop2end:

       sub eax, 16;

       jz @nextLine;

       {$IFDEF FPC}vmovsd xmm0, [ecx + eax];{$ELSE}db $C5,$FB,$10,$04,$01;{$ENDIF} 
       {$IFDEF FPC}vsqrtsd xmm0, xmm0, xmm0;{$ELSE}db $C5,$FB,$51,$C0;{$ENDIF} 
       {$IFDEF FPC}vmovsd [ecx + eax], xmm0;{$ELSE}db $C5,$FB,$11,$04,$01;{$ENDIF} 

       @nextLine:

       // next line:
       add ecx, edx;

   // loop y end
   dec esi;
   jnz @@addforyloop;

   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 
   pop esi;
   pop edi;
end;
end;

{$ENDIF}

end.
