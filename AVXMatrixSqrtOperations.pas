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

procedure AVXMatrixSQRTAligned(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure AVXMatrixSQRTUnAligned(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

{$ENDIF}

implementation

{$IFNDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$S-} {$ENDIF}

procedure AVXMatrixSQRTAligned(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt);
asm
   push edi;
   push esi;

   //iters := -width*sizeof(double);
   imul ecx, -8;

   // helper registers dest pointers
   sub eax, ecx;

   // for y := 0 to height - 1:
   mov esi, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, ecx;
       @addforxloop:
           add edi, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetchw [eax + edi];

           // elementwise sqrt
           {$IFDEF FPC}vsqrtpd ymm0, [eax + edi - 128];{$ELSE}db $C5,$FD,$51,$44,$38,$80;{$ENDIF} 
           {$IFDEF FPC}vmovapd [eax + edi - 128], ymm0;{$ELSE}db $C5,$FD,$29,$44,$38,$80;{$ENDIF} 

           {$IFDEF FPC}vsqrtpd ymm1, [eax + edi - 96];{$ELSE}db $C5,$FD,$51,$4C,$38,$A0;{$ENDIF} 
           {$IFDEF FPC}vmovapd [eax + edi - 96], ymm1;{$ELSE}db $C5,$FD,$29,$4C,$38,$A0;{$ENDIF} 

           {$IFDEF FPC}vsqrtpd ymm0, [eax + edi - 64];{$ELSE}db $C5,$FD,$51,$44,$38,$C0;{$ENDIF} 
           {$IFDEF FPC}vmovapd [eax + edi - 64], ymm0;{$ELSE}db $C5,$FD,$29,$44,$38,$C0;{$ENDIF} 

           {$IFDEF FPC}vsqrtpd ymm1, [eax + edi - 32];{$ELSE}db $C5,$FD,$51,$4C,$38,$E0;{$ENDIF} 
           {$IFDEF FPC}vmovapd [eax + edi - 32], ymm1;{$ELSE}db $C5,$FD,$29,$4C,$38,$E0;{$ENDIF} 

       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @nextLine;

       @addforxloop2:
           add edi, 16;
           jg @addforxloop2end;

           {$IFDEF FPC}vsqrtpd xmm0, [eax + edi - 16];{$ELSE}db $C5,$F9,$51,$44,$38,$F0;{$ENDIF} 
           {$IFDEF FPC}vmovapd [eax + edi - 16], xmm0;{$ELSE}db $C5,$F9,$29,$44,$38,$F0;{$ENDIF} 
       jmp @addforxloop2;

       @addforxloop2end:

       sub edi, 16;

       jz @nextLine;

       {$IFDEF FPC}vmovsd xmm0, [eax + edi];{$ELSE}db $C5,$FB,$10,$04,$38;{$ENDIF} 
       {$IFDEF FPC}vsqrtsd xmm0, xmm0, xmm0;{$ELSE}db $C5,$FB,$51,$C0;{$ENDIF} 
       {$IFDEF FPC}vmovsd [eax + edi], xmm0;{$ELSE}db $C5,$FB,$11,$04,$38;{$ENDIF} 

       @nextLine:

       // next line:
       add eax, edx;

   // loop y end
   dec esi;
   jnz @@addforyloop;

   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 
   pop esi;
   pop edi;
end;

procedure AVXMatrixSQRTUnAligned(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt);
asm
   push edi;
   push esi

   //iters := -width*sizeof(double);
   imul ecx, -8;

   // helper registers dest pointers
   sub eax, ecx;

   // for y := 0 to height - 1:
   mov esi, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov edi, ecx;
       @addforxloop:
           add edi, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetchw [eax + edi];

           // elementwise sqrt
           {$IFDEF FPC}vmovupd ymm2, [eax + edi - 128];{$ELSE}db $C5,$FD,$10,$54,$38,$80;{$ENDIF} 
           {$IFDEF FPC}vsqrtpd ymm0, ymm2;{$ELSE}db $C5,$FD,$51,$C2;{$ENDIF} 
           {$IFDEF FPC}vmovupd [eax + edi - 128], ymm0;{$ELSE}db $C5,$FD,$11,$44,$38,$80;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm3, [eax + edi - 96];{$ELSE}db $C5,$FD,$10,$5C,$38,$A0;{$ENDIF} 
           {$IFDEF FPC}vsqrtpd ymm1, ymm3;{$ELSE}db $C5,$FD,$51,$CB;{$ENDIF} 
           {$IFDEF FPC}vmovupd [eax + edi - 96], ymm1;{$ELSE}db $C5,$FD,$11,$4C,$38,$A0;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm2, [eax + edi - 64];{$ELSE}db $C5,$FD,$10,$54,$38,$C0;{$ENDIF} 
           {$IFDEF FPC}vsqrtpd ymm0, ymm2;{$ELSE}db $C5,$FD,$51,$C2;{$ENDIF} 
           {$IFDEF FPC}vmovupd [eax + edi - 64], ymm0;{$ELSE}db $C5,$FD,$11,$44,$38,$C0;{$ENDIF} 

           {$IFDEF FPC}vmovupd ymm3, [eax + edi - 32];{$ELSE}db $C5,$FD,$10,$5C,$38,$E0;{$ENDIF} 
           {$IFDEF FPC}vsqrtpd ymm1, ymm3;{$ELSE}db $C5,$FD,$51,$CB;{$ENDIF} 
           {$IFDEF FPC}vmovupd [eax + edi - 32], ymm1;{$ELSE}db $C5,$FD,$11,$4C,$38,$E0;{$ENDIF} 

       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @nextLine;

       @addforxloop2:
           add edi, 16;
           jg @addforxloop2end;

           {$IFDEF FPC}vmovupd xmm1, [eax + edi - 16];{$ELSE}db $C5,$F9,$10,$4C,$38,$F0;{$ENDIF} 
           {$IFDEF FPC}vsqrtpd xmm0, xmm1;{$ELSE}db $C5,$F9,$51,$C1;{$ENDIF} 
           {$IFDEF FPC}vmovupd [eax + edi - 16], xmm0;{$ELSE}db $C5,$F9,$11,$44,$38,$F0;{$ENDIF} 
       jmp @addforxloop2;

       @addforxloop2end:

       sub edi, 16;

       jz @nextLine;

       {$IFDEF FPC}vmovsd xmm0, [eax + edi];{$ELSE}db $C5,$FB,$10,$04,$38;{$ENDIF} 
       {$IFDEF FPC}vsqrtsd xmm0, xmm0, xmm0;{$ELSE}db $C5,$FB,$51,$C0;{$ENDIF} 
       {$IFDEF FPC}vmovsd [eax + edi], xmm0;{$ELSE}db $C5,$FB,$11,$04,$38;{$ENDIF} 

       @nextLine:

       // next line:
       add eax, edx;

   // loop y end
   dec esi;
   jnz @@addforyloop;

   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 
   pop esi;
   pop edi;
end;

{$ENDIF}

end.
