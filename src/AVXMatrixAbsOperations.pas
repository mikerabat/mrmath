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

unit AVXMatrixAbsOperations;

// #####################################################
// #### Abs opertaion applied to every element in a matrix
// #####################################################

interface

{$I 'mrMath_CPU.inc'}

{$IFNDEF x64}

procedure AVXMatrixAbsAligned(Dest : PDouble; const LineWidth, Width, Height : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure AVXMatrixAbsUnAligned(Dest : PDouble; const LineWidth, Width, Height : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

{$ENDIF}

implementation

{$IFNDEF x64}

const cLocSignBits4 : Array[0..3] of int64 = ($7FFFFFFFFFFFFFFF, $7FFFFFFFFFFFFFFF, $7FFFFFFFFFFFFFFF, $7FFFFFFFFFFFFFFF);

procedure AVXMatrixAbsAligned(Dest : PDouble; const LineWidth, Width, Height : NativeInt);
// eax = dest; edx = LineWidth, ecx = Width
asm
   push edi;
   push ebx;

   imul ecx, -8;

   // helper registers for the dest pointer
   sub eax, ecx;

   lea ebx, cLocSignBits4;
   {$IFDEF AVXSUP}vmovupd ymm0, [ebx];                                {$ELSE}db $C5,$FD,$10,$03;{$ENDIF} 

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
           {$IFDEF AVXSUP}vmovapd ymm1, [eax + edi - 128];            {$ELSE}db $C5,$FD,$28,$4C,$38,$80;{$ENDIF} 
           {$IFDEF AVXSUP}vAndpd ymm1, ymm1, ymm0;                    {$ELSE}db $C5,$F5,$54,$C8;{$ENDIF} 
           {$IFDEF AVXSUP}vmovapd [eax + edi - 128], ymm1;            {$ELSE}db $C5,$FD,$29,$4C,$38,$80;{$ENDIF} 

           {$IFDEF AVXSUP}vmovapd ymm2, [eax + edi - 96];             {$ELSE}db $C5,$FD,$28,$54,$38,$A0;{$ENDIF} 
           {$IFDEF AVXSUP}vandpd ymm2, ymm2, ymm0;                    {$ELSE}db $C5,$ED,$54,$D0;{$ENDIF} 
           {$IFDEF AVXSUP}vmovapd [eax + edi - 96], ymm2;             {$ELSE}db $C5,$FD,$29,$54,$38,$A0;{$ENDIF} 

           {$IFDEF AVXSUP}vmovapd ymm3, [eax + edi - 64];             {$ELSE}db $C5,$FD,$28,$5C,$38,$C0;{$ENDIF} 
           {$IFDEF AVXSUP}vandpd ymm3, ymm3, ymm0;                    {$ELSE}db $C5,$E5,$54,$D8;{$ENDIF} 
           {$IFDEF AVXSUP}vmovapd [eax + edi - 64], ymm3;             {$ELSE}db $C5,$FD,$29,$5C,$38,$C0;{$ENDIF} 

           {$IFDEF AVXSUP}vmovapd ymm4, [eax + edi - 32];             {$ELSE}db $C5,$FD,$28,$64,$38,$E0;{$ENDIF} 
           {$IFDEF AVXSUP}vandpd ymm4, ymm4, ymm0;                    {$ELSE}db $C5,$DD,$54,$E0;{$ENDIF} 
           {$IFDEF AVXSUP}vmovapd [eax + edi - 32], ymm4;             {$ELSE}db $C5,$FD,$29,$64,$38,$E0;{$ENDIF} 
       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @nextLine;

       @addforxloop2:
           add edi, 16;
           jg @loopEnd2;

           {$IFDEF AVXSUP}vmovapd xmm1, [eax + edi - 16];             {$ELSE}db $C5,$F9,$28,$4C,$38,$F0;{$ENDIF} 
           {$IFDEF AVXSUP}vandpd xmm1, xmm1, xmm0;                    {$ELSE}db $C5,$F1,$54,$C8;{$ENDIF} 
           {$IFDEF AVXSUP}vmovapd [eax + edi - 16], xmm1;             {$ELSE}db $C5,$F9,$29,$4C,$38,$F0;{$ENDIF} 
       jmp @addforxloop2;

       @loopEnd2:

       sub edi, 16;
       jz @nextLine;

       {$IFDEF AVXSUP}vmovsd xmm1, [eax + edi];                       {$ELSE}db $C5,$FB,$10,$0C,$38;{$ENDIF} 
       {$IFDEF AVXSUP}vandpd xmm1, xmm1, xmm0;                        {$ELSE}db $C5,$F1,$54,$C8;{$ENDIF} 
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

procedure AVXMatrixAbsUnAligned(Dest : PDouble; const LineWidth, Width, Height : NativeInt);
// eax = dest; edx = LineWidth, ecx = Width
asm
   push edi;
   push ebx;

   imul ecx, -8;

   // helper registers for the dest pointer
   sub eax, ecx;

   lea ebx, cLocSignBits4;
   {$IFDEF AVXSUP}vmovupd ymm0, [ebx];                                {$ELSE}db $C5,$FD,$10,$03;{$ENDIF} 

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
           {$IFDEF AVXSUP}vAndpd ymm1, ymm1, ymm0;                    {$ELSE}db $C5,$F5,$54,$C8;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd [eax + edi - 128], ymm1;            {$ELSE}db $C5,$FD,$11,$4C,$38,$80;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd ymm2, [eax + edi - 96];             {$ELSE}db $C5,$FD,$10,$54,$38,$A0;{$ENDIF} 
           {$IFDEF AVXSUP}vandpd ymm2, ymm2, ymm0;                    {$ELSE}db $C5,$ED,$54,$D0;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd [eax + edi - 96], ymm2;             {$ELSE}db $C5,$FD,$11,$54,$38,$A0;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd ymm3, [eax + edi - 64];             {$ELSE}db $C5,$FD,$10,$5C,$38,$C0;{$ENDIF} 
           {$IFDEF AVXSUP}vandpd ymm3, ymm3, ymm0;                    {$ELSE}db $C5,$E5,$54,$D8;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd [eax + edi - 64], ymm3;             {$ELSE}db $C5,$FD,$11,$5C,$38,$C0;{$ENDIF} 

           {$IFDEF AVXSUP}vmovupd ymm4, [eax + edi - 32];             {$ELSE}db $C5,$FD,$10,$64,$38,$E0;{$ENDIF} 
           {$IFDEF AVXSUP}vandpd ymm4, ymm4, ymm0;                    {$ELSE}db $C5,$DD,$54,$E0;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd [eax + edi - 32], ymm4;             {$ELSE}db $C5,$FD,$11,$64,$38,$E0;{$ENDIF} 
       jmp @addforxloop

       @loopEnd:

       sub edi, 128;

       jz @nextLine;

       @addforxloop2:
           add edi, 16;
           jg @loopEnd2;

           {$IFDEF AVXSUP}vmovupd xmm1, [eax + edi - 16];             {$ELSE}db $C5,$F9,$10,$4C,$38,$F0;{$ENDIF} 
           {$IFDEF AVXSUP}vandpd xmm1, xmm1, xmm0;                    {$ELSE}db $C5,$F1,$54,$C8;{$ENDIF} 
           {$IFDEF AVXSUP}vmovupd [eax + edi - 16], xmm1;             {$ELSE}db $C5,$F9,$11,$4C,$38,$F0;{$ENDIF} 
       jmp @addforxloop2;

       @loopEnd2:

       sub edi, 16;
       jz @nextLine;

       {$IFDEF AVXSUP}vmovsd xmm1, [eax + edi];                       {$ELSE}db $C5,$FB,$10,$0C,$38;{$ENDIF} 
       {$IFDEF AVXSUP}vandpd xmm1, xmm1, xmm0;                        {$ELSE}db $C5,$F1,$54,$C8;{$ENDIF} 
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
