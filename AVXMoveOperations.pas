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


unit AVXMoveOperations;

interface

{$I 'mrMath_CPU.inc'}

{$IFNDEF x64}

procedure AVXMatrixCopyAligned(Dest : PDouble; const destLineWidth : NativeInt; src : PDouble; const srcLineWidth : NativeInt; Width, Height : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure AVXMatrixCopyUnAligned(Dest : PDouble; const destLineWidth : NativeInt; src : PDouble; const srcLineWidth : NativeInt; Width, Height : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

procedure AVXRowSwapAligned(A, B : PDouble; width : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure AVXRowSwapUnAligned(A, B : PDouble; width : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

procedure AVXMatrixInitUnAligned( dest : PDouble; const destLineWidth : NativeInt; Width, Height : NativeInt; Value : double); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure AVXMatrixInitAligned( dest : PDouble; const destLineWidth : NativeInt; Width, Height : NativeInt; Value : double); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

procedure AVXInitMemAligned(A : PDouble; NumBytes : NativeInt; Value : double); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

{$ENDIF}

implementation

{$IFNDEF x64}

// eax = dest, edx = destLineWidth, ecx = width
procedure AVXMatrixInitAligned( dest : PDouble; const destLineWidth : NativeInt; Width, Height : NativeInt; Value : double); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
asm
   push ebx;

   lea ebx, Value;
   {$IFDEF FPC}vbroadcastsd ymm1, [ebx];{$ELSE}db $C4,$E2,$7D,$19,$0B;{$ENDIF} 

   //iters := -width*sizeof(double);
   imul ecx, -8;

   // helper registers for the src and dest pointers
   sub eax, ecx;

   // for y := 0 to height - 1:
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov ebx, ecx;
       @addforxloop:
           add ebx, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [ecx + ebx];
           // prefetchw [eax + ebx];

           // move:
           {$IFDEF FPC}vmovapd [eax + ebx - 128], ymm1;{$ELSE}db $C5,$FD,$29,$4C,$18,$80;{$ENDIF} 
           {$IFDEF FPC}vmovapd [eax + ebx - 96], ymm1;{$ELSE}db $C5,$FD,$29,$4C,$18,$A0;{$ENDIF} 
           {$IFDEF FPC}vmovapd [eax + ebx - 64], ymm1;{$ELSE}db $C5,$FD,$29,$4C,$18,$C0;{$ENDIF} 
           {$IFDEF FPC}vmovapd [eax + ebx - 32], ymm1;{$ELSE}db $C5,$FD,$29,$4C,$18,$E0;{$ENDIF} 

       jmp @addforxloop

       @loopEnd:

       sub ebx, 128;

       jz @nextLine;

       @addforxloop2:
          add ebx, 16;
          jg @loopEnd2;

          {$IFDEF FPC}vmovapd [eax + ebx - 16], xmm1;{$ELSE}db $C5,$F9,$29,$4C,$18,$F0;{$ENDIF} 
       jmp @addforxloop2;

       @loopEnd2:
       sub ebx, 16;

       jz @nextLine;

       // last element
       {$IFDEF FPC}vmovsd [eax + ebx], xmm1;{$ELSE}db $C5,$FB,$11,$0C,$18;{$ENDIF} 

       @nextLine:

       // next line:
       add eax, edx;

   // loop y end
   dec height;
   jnz @@addforyloop;

   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   // epilog
   pop ebx;
end;

procedure AVXMatrixInitUnAligned( dest : PDouble; const destLineWidth : NativeInt; Width, Height : NativeInt; Value : double); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
asm
   push ebx;

   lea ebx, Value;
   {$IFDEF FPC}vbroadcastsd ymm1, [ebx];{$ELSE}db $C4,$E2,$7D,$19,$0B;{$ENDIF} 

   //iters := -width*sizeof(double);
   imul ecx, -8;

   // helper registers for the src and dest pointers
   sub eax, ecx;

   // for y := 0 to height - 1:
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov ebx, ecx;
       @addforxloop:
           add ebx, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [ecx + ebx];
           // prefetchw [eax + ebx];

           // move:
           {$IFDEF FPC}vmovupd [eax + ebx - 128], ymm1;{$ELSE}db $C5,$FD,$11,$4C,$18,$80;{$ENDIF} 
           {$IFDEF FPC}vmovupd [eax + ebx - 96], ymm1;{$ELSE}db $C5,$FD,$11,$4C,$18,$A0;{$ENDIF} 
           {$IFDEF FPC}vmovupd [eax + ebx - 64], ymm1;{$ELSE}db $C5,$FD,$11,$4C,$18,$C0;{$ENDIF} 
           {$IFDEF FPC}vmovupd [eax + ebx - 32], ymm1;{$ELSE}db $C5,$FD,$11,$4C,$18,$E0;{$ENDIF} 

       jmp @addforxloop

       @loopEnd:

       sub ebx, 128;

       jz @nextLine;

       @addforxloop2:
          add ebx, 16;
          jg @loopEnd2;

          {$IFDEF FPC}vmovupd [eax + ebx - 16], xmm1;{$ELSE}db $C5,$F9,$11,$4C,$18,$F0;{$ENDIF} 
       jmp @addforxloop2;

       @loopEnd2:
       sub ebx, 16;

       jz @nextLine;

       // last element
       {$IFDEF FPC}vmovsd [eax + ebx], xmm1;{$ELSE}db $C5,$FB,$11,$0C,$18;{$ENDIF} 

       @nextLine:

       // next line:
       add eax, edx;

   // loop y end
   dec height;
   jnz @@addforyloop;

   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   // epilog
   pop ebx;
end;


// rcx = A, rdx = NumBytes;
procedure AVXInitMemAligned(A : PDouble; NumBytes : NativeInt; Value : double); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
asm
   lea ecx, Value;
   {$IFDEF FPC}vbroadcastsd ymm1, [ecx];{$ELSE}db $C4,$E2,$7D,$19,$09;{$ENDIF} 

   imul edx, -1;
   sub eax, edx;

   @@loopUnrolled:
      add edx, 128;
      jg @@loopUnrolledEnd;

      {$IFDEF FPC}vmovapd [eax + edx - 128], ymm1;{$ELSE}db $C5,$FD,$29,$4C,$10,$80;{$ENDIF} 
      {$IFDEF FPC}vmovapd [eax + edx - 96], ymm1;{$ELSE}db $C5,$FD,$29,$4C,$10,$A0;{$ENDIF} 
      {$IFDEF FPC}vmovapd [eax + edx - 64], ymm1;{$ELSE}db $C5,$FD,$29,$4C,$10,$C0;{$ENDIF} 
      {$IFDEF FPC}vmovapd [eax + edx - 32], ymm1;{$ELSE}db $C5,$FD,$29,$4C,$10,$E0;{$ENDIF} 
   jmp @@loopUnrolled;

   @@loopUnrolledEnd:

   sub edx, 128;
   jz @@exitProc;

   @@loop2:
      add edx, 32;
      jg @@loopEnd2;

      {$IFDEF FPC}vmovapd [eax + edx - 32], xmm1;{$ELSE}db $C5,$F9,$29,$4C,$10,$E0;{$ENDIF} 
      {$IFDEF FPC}vmovapd [eax + edx - 16], xmm1;{$ELSE}db $C5,$F9,$29,$4C,$10,$F0;{$ENDIF} 
   jmp @@loop2;

   @@loopEnd2:
   sub edx, 32;
   jz @@exitProc;

   @@loop3:
      {$IFDEF FPC}vmovsd [eax + edx], xmm1;{$ELSE}db $C5,$FB,$11,$0C,$10;{$ENDIF} 
      add edx, 8;
   jnz @@loop3;

   @@exitProc:

   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;

procedure AVXRowSwapAligned(A, B : PDouble; width : NativeInt); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
asm
   // prepare
   imul ecx, -8;

   sub eax, ecx;
   sub edx, ecx;

   @unrolloop:
     add ecx, 128;
     jg @unrolloopend;

     // prefetch data...
     // prefetchw [ecx + r10];
     // prefetchw [edx + r10];

     {$IFDEF FPC}vmovdqa ymm0, [eax + ecx - 128];{$ELSE}db $C5,$FD,$6F,$44,$08,$80;{$ENDIF} 
     {$IFDEF FPC}vmovdqa ymm1, [edx + ecx - 128];{$ELSE}db $C5,$FD,$6F,$4C,$0A,$80;{$ENDIF} 

     {$IFDEF FPC}vmovdqa [eax + ecx - 128], ymm1;{$ELSE}db $C5,$FD,$7F,$4C,$08,$80;{$ENDIF} 
     {$IFDEF FPC}vmovdqa [edx + ecx - 128], ymm0;{$ELSE}db $C5,$FD,$7F,$44,$0A,$80;{$ENDIF} 

     {$IFDEF FPC}vmovdqa ymm2, [eax + ecx - 96];{$ELSE}db $C5,$FD,$6F,$54,$08,$A0;{$ENDIF} 
     {$IFDEF FPC}vmovdqa ymm3, [edx + ecx - 96];{$ELSE}db $C5,$FD,$6F,$5C,$0A,$A0;{$ENDIF} 

     {$IFDEF FPC}vmovdqa [eax + ecx - 96], ymm3;{$ELSE}db $C5,$FD,$7F,$5C,$08,$A0;{$ENDIF} 
     {$IFDEF FPC}vmovdqa [edx + ecx - 96], ymm2;{$ELSE}db $C5,$FD,$7F,$54,$0A,$A0;{$ENDIF} 

     {$IFDEF FPC}vmovdqa ymm0, [eax + ecx - 64];{$ELSE}db $C5,$FD,$6F,$44,$08,$C0;{$ENDIF} 
     {$IFDEF FPC}vmovdqa ymm1, [edx + ecx - 64];{$ELSE}db $C5,$FD,$6F,$4C,$0A,$C0;{$ENDIF} 

     {$IFDEF FPC}vmovdqa [eax + ecx - 64], ymm1;{$ELSE}db $C5,$FD,$7F,$4C,$08,$C0;{$ENDIF} 
     {$IFDEF FPC}vmovdqa [edx + ecx - 64], ymm0;{$ELSE}db $C5,$FD,$7F,$44,$0A,$C0;{$ENDIF} 

     {$IFDEF FPC}vmovdqa ymm2, [eax + ecx - 32];{$ELSE}db $C5,$FD,$6F,$54,$08,$E0;{$ENDIF} 
     {$IFDEF FPC}vmovdqa ymm3, [edx + ecx - 32];{$ELSE}db $C5,$FD,$6F,$5C,$0A,$E0;{$ENDIF} 

     {$IFDEF FPC}vmovdqa [eax + ecx - 32], ymm3;{$ELSE}db $C5,$FD,$7F,$5C,$08,$E0;{$ENDIF} 
     {$IFDEF FPC}vmovdqa [edx + ecx - 32], ymm2;{$ELSE}db $C5,$FD,$7F,$54,$0A,$E0;{$ENDIF} 
   jmp @unrolloop;
   @unrolloopend:

   sub ecx, 128;
   jz @endfunc;

   @loop2:
     add ecx, 16;
     jg @loop2End;

     {$IFDEF FPC}vmovdqa xmm0, [eax + ecx - 16];{$ELSE}db $C5,$F9,$6F,$44,$08,$F0;{$ENDIF} 
     {$IFDEF FPC}vmovdqa xmm1, [edx + ecx - 16];{$ELSE}db $C5,$F9,$6F,$4C,$0A,$F0;{$ENDIF} 

     {$IFDEF FPC}vmovdqa [eax + ecx - 16], xmm1;{$ELSE}db $C5,$F9,$7F,$4C,$08,$F0;{$ENDIF} 
     {$IFDEF FPC}vmovdqa [edx + ecx - 16], xmm0;{$ELSE}db $C5,$F9,$7F,$44,$0A,$F0;{$ENDIF} 
   jmp @loop2;

   @loop2End:
   sub ecx, 16;

   jz @endfunc;

   {$IFDEF FPC}vmovsd xmm0, [eax + ecx];{$ELSE}db $C5,$FB,$10,$04,$08;{$ENDIF} 
   {$IFDEF FPC}vmovsd xmm1, [edx + ecx];{$ELSE}db $C5,$FB,$10,$0C,$0A;{$ENDIF} 

   {$IFDEF FPC}vmovsd [eax + ecx], xmm1;{$ELSE}db $C5,$FB,$11,$0C,$08;{$ENDIF} 
   {$IFDEF FPC}vmovsd [edx + ecx], xmm0;{$ELSE}db $C5,$FB,$11,$04,$0A;{$ENDIF} 

   @endfunc:

   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;

procedure AVXRowSwapUnAligned(A, B : PDouble; width : NativeInt);  {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
asm
   imul ecx, -8;

   sub eax, ecx;
   sub edx, ecx;

   @unrolloop:
     add ecx, 128;
     jg @unrolloopend;

     // prefetch data...
     // prefetchw [ecx + r10];
     // prefetchw [edx + r10];

     {$IFDEF FPC}vmovdqu ymm0, [eax + ecx - 128];{$ELSE}db $C5,$FE,$6F,$44,$08,$80;{$ENDIF} 
     {$IFDEF FPC}vmovdqu ymm1, [edx + ecx - 128];{$ELSE}db $C5,$FE,$6F,$4C,$0A,$80;{$ENDIF} 

     {$IFDEF FPC}vmovdqu [eax + ecx - 128], ymm1;{$ELSE}db $C5,$FE,$7F,$4C,$08,$80;{$ENDIF} 
     {$IFDEF FPC}vmovdqu [edx + ecx - 128], ymm0;{$ELSE}db $C5,$FE,$7F,$44,$0A,$80;{$ENDIF} 

     {$IFDEF FPC}vmovdqu ymm2, [eax + ecx - 96];{$ELSE}db $C5,$FE,$6F,$54,$08,$A0;{$ENDIF} 
     {$IFDEF FPC}vmovdqu ymm3, [edx + ecx - 96];{$ELSE}db $C5,$FE,$6F,$5C,$0A,$A0;{$ENDIF} 

     {$IFDEF FPC}vmovdqu [eax + ecx - 96], ymm3;{$ELSE}db $C5,$FE,$7F,$5C,$08,$A0;{$ENDIF} 
     {$IFDEF FPC}vmovdqu [edx + ecx - 96], ymm2;{$ELSE}db $C5,$FE,$7F,$54,$0A,$A0;{$ENDIF} 

     {$IFDEF FPC}vmovdqu ymm0, [eax + ecx - 64];{$ELSE}db $C5,$FE,$6F,$44,$08,$C0;{$ENDIF} 
     {$IFDEF FPC}vmovdqu ymm1, [edx + ecx - 64];{$ELSE}db $C5,$FE,$6F,$4C,$0A,$C0;{$ENDIF} 

     {$IFDEF FPC}vmovdqu [eax + ecx - 64], ymm1;{$ELSE}db $C5,$FE,$7F,$4C,$08,$C0;{$ENDIF} 
     {$IFDEF FPC}vmovdqu [edx + ecx - 64], ymm0;{$ELSE}db $C5,$FE,$7F,$44,$0A,$C0;{$ENDIF} 

     {$IFDEF FPC}vmovdqu ymm2, [eax + ecx - 32];{$ELSE}db $C5,$FE,$6F,$54,$08,$E0;{$ENDIF} 
     {$IFDEF FPC}vmovdqu ymm3, [edx + ecx - 32];{$ELSE}db $C5,$FE,$6F,$5C,$0A,$E0;{$ENDIF} 

     {$IFDEF FPC}vmovdqu [eax + ecx - 32], ymm3;{$ELSE}db $C5,$FE,$7F,$5C,$08,$E0;{$ENDIF} 
     {$IFDEF FPC}vmovdqu [edx + ecx - 32], ymm2;{$ELSE}db $C5,$FE,$7F,$54,$0A,$E0;{$ENDIF} 
   jmp @unrolloop;
   @unrolloopend:

   sub ecx, 128;
   jz @endfunc;

   @loop2:
     add ecx, 16;
     jg @loop2End;

     {$IFDEF FPC}vmovdqu xmm0, [eax + ecx - 16];{$ELSE}db $C5,$FA,$6F,$44,$08,$F0;{$ENDIF} 
     {$IFDEF FPC}vmovdqu xmm1, [edx + ecx - 16];{$ELSE}db $C5,$FA,$6F,$4C,$0A,$F0;{$ENDIF} 

     {$IFDEF FPC}vmovdqu [eax + ecx - 16], xmm1;{$ELSE}db $C5,$FA,$7F,$4C,$08,$F0;{$ENDIF} 
     {$IFDEF FPC}vmovdqu [edx + ecx - 16], xmm0;{$ELSE}db $C5,$FA,$7F,$44,$0A,$F0;{$ENDIF} 
   jmp @loop2;

   @loop2End:
   sub ecx, 16;

   jz @endfunc;

   {$IFDEF FPC}vmovsd xmm0, [eax + ecx];{$ELSE}db $C5,$FB,$10,$04,$08;{$ENDIF} 
   {$IFDEF FPC}vmovsd xmm1, [edx + ecx];{$ELSE}db $C5,$FB,$10,$0C,$0A;{$ENDIF} 

   {$IFDEF FPC}vmovsd [eax + ecx], xmm1;{$ELSE}db $C5,$FB,$11,$0C,$08;{$ENDIF} 
   {$IFDEF FPC}vmovsd [edx + ecx], xmm0;{$ELSE}db $C5,$FB,$11,$04,$0A;{$ENDIF} 

   @endfunc:

   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;

procedure AVXMatrixCopyAligned(Dest : PDouble; const destLineWidth : NativeInt; src : PDouble; const srcLineWidth : NativeInt; Width, Height : NativeInt);
asm
   push ebx;
   push edi;
   push esi;

   //iters := -width*sizeof(double);
   mov esi, width;
   imul esi, -8;

   // helper registers for the src and dest pointers
   sub ecx, esi;
   sub eax, esi;

   // for y := 0 to height - 1:
   mov edi, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov ebx, esi;
       @addforxloop:
           add ebx, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [ecx + ebx];
           // prefetchw [eax + ebx];

           // move:
           {$IFDEF FPC}vmovdqa ymm0, [ecx + ebx - 128];{$ELSE}db $C5,$FD,$6F,$44,$19,$80;{$ENDIF} 
           {$IFDEF FPC}vmovdqa [eax + ebx - 128], ymm0;{$ELSE}db $C5,$FD,$7F,$44,$18,$80;{$ENDIF} 

           {$IFDEF FPC}vmovdqa ymm1, [ecx + ebx - 96];{$ELSE}db $C5,$FD,$6F,$4C,$19,$A0;{$ENDIF} 
           {$IFDEF FPC}vmovdqa [eax + ebx - 96], ymm1;{$ELSE}db $C5,$FD,$7F,$4C,$18,$A0;{$ENDIF} 

           {$IFDEF FPC}vmovdqa ymm2, [ecx + ebx - 64];{$ELSE}db $C5,$FD,$6F,$54,$19,$C0;{$ENDIF} 
           {$IFDEF FPC}vmovdqa [eax + ebx - 64], ymm2;{$ELSE}db $C5,$FD,$7F,$54,$18,$C0;{$ENDIF} 

           {$IFDEF FPC}vmovdqa ymm3, [ecx + ebx - 32];{$ELSE}db $C5,$FD,$6F,$5C,$19,$E0;{$ENDIF} 
           {$IFDEF FPC}vmovdqa [eax + ebx - 32], ymm3;{$ELSE}db $C5,$FD,$7F,$5C,$18,$E0;{$ENDIF} 

       jmp @addforxloop

       @loopEnd:

       sub ebx, 128;

       jz @nextLine;

       @addforxloop2:
          add ebx, 16;
          jg @loopEnd2;

          {$IFDEF FPC}vmovdqa xmm0, [ecx + ebx - 16];{$ELSE}db $C5,$F9,$6F,$44,$19,$F0;{$ENDIF} 
          {$IFDEF FPC}vmovdqa [eax + ebx - 16], xmm0;{$ELSE}db $C5,$F9,$7F,$44,$18,$F0;{$ENDIF} 
       jmp @addforxloop2;

       @loopEnd2:
       sub ebx, 16;

       jz @nextLine;

       // last element
       {$IFDEF FPC}vmovsd xmm0, [ecx + ebx];{$ELSE}db $C5,$FB,$10,$04,$19;{$ENDIF} 
       {$IFDEF FPC}vmovsd [eax + ebx], xmm0;{$ELSE}db $C5,$FB,$11,$04,$18;{$ENDIF} 

       @nextLine:

       // next line:
       add ecx, srcLineWidth;
       add eax, edx;

   // loop y end
   dec edi;
   jnz @@addforyloop;

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 

   pop esi;
   pop edi;
   pop ebx;
end;

procedure AVXMatrixCopyUnAligned(Dest : PDouble; const destLineWidth : NativeInt; src : PDouble; const srcLineWidth : NativeInt; Width, Height : NativeInt);
asm
   push ebx;
   push edi;
   push esi;

   // note: ecx = dest, edx = destLineWidth, edx = src, R9 = srcLineWidth
   //iters := -width*sizeof(double);
   mov esi, width;
   imul esi, -8;

   // helper registers for the src and dest pointers
   sub ecx, esi;
   sub eax, esi;

   // for y := 0 to height - 1:
   mov edi, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov ebx, esi;
       @addforxloop:
           add ebx, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [ecx + ebx];
           // prefetchw [eax + ebx];

           // move:
           {$IFDEF FPC}vmovdqu ymm0, [ecx + ebx - 128];{$ELSE}db $C5,$FE,$6F,$44,$19,$80;{$ENDIF} 
           {$IFDEF FPC}vmovdqu [eax + ebx - 128], ymm0;{$ELSE}db $C5,$FE,$7F,$44,$18,$80;{$ENDIF} 

           {$IFDEF FPC}vmovdqu ymm1, [ecx + ebx - 96];{$ELSE}db $C5,$FE,$6F,$4C,$19,$A0;{$ENDIF} 
           {$IFDEF FPC}vmovdqu [eax + ebx - 96], ymm1;{$ELSE}db $C5,$FE,$7F,$4C,$18,$A0;{$ENDIF} 

           {$IFDEF FPC}vmovdqu ymm2, [ecx + ebx - 64];{$ELSE}db $C5,$FE,$6F,$54,$19,$C0;{$ENDIF} 
           {$IFDEF FPC}vmovdqu [eax + ebx - 64], ymm2;{$ELSE}db $C5,$FE,$7F,$54,$18,$C0;{$ENDIF} 

           {$IFDEF FPC}vmovdqu ymm3, [ecx + ebx - 32];{$ELSE}db $C5,$FE,$6F,$5C,$19,$E0;{$ENDIF} 
           {$IFDEF FPC}vmovdqu [eax + ebx - 32], ymm3;{$ELSE}db $C5,$FE,$7F,$5C,$18,$E0;{$ENDIF} 

       jmp @addforxloop

       @loopEnd:

       sub ebx, 128;

       jz @nextLine;

       @addforxloop2:
          add ebx, 16;
          jg @loopEnd2;

          {$IFDEF FPC}vmovdqu xmm0, [ecx + ebx - 16];{$ELSE}db $C5,$FA,$6F,$44,$19,$F0;{$ENDIF} 
          {$IFDEF FPC}vmovdqu [eax + ebx - 16], xmm0;{$ELSE}db $C5,$FA,$7F,$44,$18,$F0;{$ENDIF} 
       jmp @addforxloop2;

       @loopEnd2:
       sub ebx, 16;

       jz @nextLine;

       // last element
       {$IFDEF FPC}vmovsd xmm0, [ecx + ebx];{$ELSE}db $C5,$FB,$10,$04,$19;{$ENDIF} 
       {$IFDEF FPC}vmovsd [eax + ebx], xmm0;{$ELSE}db $C5,$FB,$11,$04,$18;{$ENDIF} 

       @nextLine:

       // next line:
       add ecx, srcLineWidth;
       add eax, edx;

   // loop y end
   dec edi;
   jnz @@addforyloop;

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 
   pop esi;
   pop edi;
   pop ebx;
end;

{$ENDIF}

end.
