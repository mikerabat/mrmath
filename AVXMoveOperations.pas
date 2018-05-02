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

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}
{$IFNDEF x64}

uses MatrixConst;

procedure AVXMatrixCopyAligned(Dest : PDouble; const destLineWidth : TASMNativeInt; src : PDouble; const srcLineWidth : TASMNativeInt; Width, Height : TASMNativeInt);
procedure AVXMatrixCopyUnAligned(Dest : PDouble; const destLineWidth : TASMNativeInt; src : PDouble; const srcLineWidth : TASMNativeInt; Width, Height : TASMNativeInt);

procedure AVXRowSwapAligned(A, B : PDouble; width : TASMNativeInt);
procedure AVXRowSwapUnAligned(A, B : PDouble; width : TASMNativeInt);

procedure AVXInitMemAligned(A : PDouble; NumBytes : TASMNativeInt; const Value : double);

{$ENDIF}

implementation

{$IFNDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$S-} {$ENDIF}

// uses non temporal moves so the cache is not poisned
// rcx = A, rdx = NumBytes;
procedure AVXInitMemAligned(A : PDouble; NumBytes : TASMNativeInt; const Value : double);
begin
asm
   lea eax, Value;
   {$IFDEF FPC}vbroadcastsd ymm1, [eax];{$ELSE}db $C4,$E2,$7D,$19,$08;{$ENDIF} 

   mov edx, NumBytes;
   imul edx, -1;
   mov ecx, A;
   sub ecx, edx;

   @@loopUnrolled:
      add edx, 128;
      jg @@loopUnrolledEnd;

      {$IFDEF FPC}vmovntdq [ecx + edx - 128], ymm1;{$ELSE}db $C5,$FD,$E7,$4C,$11,$80;{$ENDIF} 
      {$IFDEF FPC}vmovntdq [ecx + edx - 96], ymm1;{$ELSE}db $C5,$FD,$E7,$4C,$11,$A0;{$ENDIF} 
      {$IFDEF FPC}vmovntdq [ecx + edx - 64], ymm1;{$ELSE}db $C5,$FD,$E7,$4C,$11,$C0;{$ENDIF} 
      {$IFDEF FPC}vmovntdq [ecx + edx - 32], ymm1;{$ELSE}db $C5,$FD,$E7,$4C,$11,$E0;{$ENDIF} 
   jmp @@loopUnrolled;

   @@loopUnrolledEnd:

   sub edx, 128;
   jz @@exitProc;

   @@loop2:
      add edx, 32;
      jg @@loopEnd2;

      {$IFDEF FPC}vmovntdq [ecx + edx - 32], xmm1;{$ELSE}db $C5,$F9,$E7,$4C,$11,$E0;{$ENDIF}
      {$IFDEF FPC}vmovntdq [ecx + edx - 16], xmm1;{$ELSE}db $C5,$F9,$E7,$4C,$11,$F0;{$ENDIF} 
   jmp @@loop2;

   @@loopEnd2:
   sub edx, 32;
   jz @@exitProc;

   @@loop3:
      {$IFDEF FPC}vmovsd [ecx + edx], xmm1;{$ELSE}db $C5,$FB,$11,$0C,$11;{$ENDIF} 
      add edx, 8;
   jnz @@loop3;

   @@exitProc:

   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;
end;

procedure AVXRowSwapAligned(A, B : PDouble; width : TASMNativeInt);
begin
asm
   // prolog
   push ebx;

   // prepare
   mov ecx, A;
   mov edx, B;

   mov ebx, width;
   imul ebx, -8;

   sub ecx, ebx;
   sub edx, ebx;

   @unrolloop:
     add ebx, 128;
     jg @unrolloopend;

     // prefetch data...
     // prefetchw [ecx + r10];
     // prefetchw [edx + r10];

     {$IFDEF FPC}vmovdqa ymm0, [ecx + ebx - 128];{$ELSE}db $C5,$FD,$6F,$44,$19,$80;{$ENDIF} 
     {$IFDEF FPC}vmovdqa ymm1, [edx + ebx - 128];{$ELSE}db $C5,$FD,$6F,$4C,$1A,$80;{$ENDIF} 

     {$IFDEF FPC}vmovdqa [ecx + ebx - 128], ymm1;{$ELSE}db $C5,$FD,$7F,$4C,$19,$80;{$ENDIF} 
     {$IFDEF FPC}vmovdqa [edx + ebx - 128], ymm0;{$ELSE}db $C5,$FD,$7F,$44,$1A,$80;{$ENDIF} 

     {$IFDEF FPC}vmovdqa ymm2, [ecx + ebx - 96];{$ELSE}db $C5,$FD,$6F,$54,$19,$A0;{$ENDIF} 
     {$IFDEF FPC}vmovdqa ymm3, [edx + ebx - 96];{$ELSE}db $C5,$FD,$6F,$5C,$1A,$A0;{$ENDIF} 

     {$IFDEF FPC}vmovdqa [ecx + ebx - 96], ymm3;{$ELSE}db $C5,$FD,$7F,$5C,$19,$A0;{$ENDIF} 
     {$IFDEF FPC}vmovdqa [edx + ebx - 96], ymm2;{$ELSE}db $C5,$FD,$7F,$54,$1A,$A0;{$ENDIF} 

     {$IFDEF FPC}vmovdqa ymm0, [ecx + ebx - 64];{$ELSE}db $C5,$FD,$6F,$44,$19,$C0;{$ENDIF} 
     {$IFDEF FPC}vmovdqa ymm1, [edx + ebx - 64];{$ELSE}db $C5,$FD,$6F,$4C,$1A,$C0;{$ENDIF} 

     {$IFDEF FPC}vmovdqa [ecx + ebx - 64], ymm1;{$ELSE}db $C5,$FD,$7F,$4C,$19,$C0;{$ENDIF} 
     {$IFDEF FPC}vmovdqa [edx + ebx - 64], ymm0;{$ELSE}db $C5,$FD,$7F,$44,$1A,$C0;{$ENDIF} 

     {$IFDEF FPC}vmovdqa ymm2, [ecx + ebx - 32];{$ELSE}db $C5,$FD,$6F,$54,$19,$E0;{$ENDIF} 
     {$IFDEF FPC}vmovdqa ymm3, [edx + ebx - 32];{$ELSE}db $C5,$FD,$6F,$5C,$1A,$E0;{$ENDIF} 

     {$IFDEF FPC}vmovdqa [ecx + ebx - 32], ymm3;{$ELSE}db $C5,$FD,$7F,$5C,$19,$E0;{$ENDIF} 
     {$IFDEF FPC}vmovdqa [edx + ebx - 32], ymm2;{$ELSE}db $C5,$FD,$7F,$54,$1A,$E0;{$ENDIF} 
   jmp @unrolloop;
   @unrolloopend:

   sub ebx, 128;
   jz @endfunc;

   @loop2:
     add ebx, 16;
     jg @loop2End;

     {$IFDEF FPC}vmovdqa xmm0, [ecx + ebx - 16];{$ELSE}db $C5,$F9,$6F,$44,$19,$F0;{$ENDIF} 
     {$IFDEF FPC}vmovdqa xmm1, [edx + ebx - 16];{$ELSE}db $C5,$F9,$6F,$4C,$1A,$F0;{$ENDIF} 

     {$IFDEF FPC}vmovdqa [ecx + ebx - 16], xmm1;{$ELSE}db $C5,$F9,$7F,$4C,$19,$F0;{$ENDIF} 
     {$IFDEF FPC}vmovdqa [edx + ebx - 16], xmm0;{$ELSE}db $C5,$F9,$7F,$44,$1A,$F0;{$ENDIF} 
   jmp @loop2;

   @loop2End:
   sub ebx, 16;

   jz @endfunc;

   {$IFDEF FPC}vmovsd xmm0, [ecx + ebx];{$ELSE}db $C5,$FB,$10,$04,$19;{$ENDIF} 
   {$IFDEF FPC}vmovsd xmm1, [edx + ebx];{$ELSE}db $C5,$FB,$10,$0C,$1A;{$ENDIF} 

   {$IFDEF FPC}vmovsd [ecx + ebx], xmm1;{$ELSE}db $C5,$FB,$11,$0C,$19;{$ENDIF} 
   {$IFDEF FPC}vmovsd [edx + ebx], xmm0;{$ELSE}db $C5,$FB,$11,$04,$1A;{$ENDIF} 

   @endfunc:

   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 
   pop ebx;
end;
end;

procedure AVXRowSwapUnAligned(A, B : PDouble; width : TASMNativeInt);
begin
asm
   // prolog
   push ebx;

   // prepare
   mov ecx, A;
   mov edx, B;

   mov ebx, width;
   imul ebx, -8;

   sub ecx, ebx;
   sub edx, ebx;

   @unrolloop:
     add ebx, 128;
     jg @unrolloopend;

     // prefetch data...
     // prefetchw [ecx + r10];
     // prefetchw [edx + r10];

     {$IFDEF FPC}vmovdqu ymm0, [ecx + ebx - 128];{$ELSE}db $C5,$FE,$6F,$44,$19,$80;{$ENDIF} 
     {$IFDEF FPC}vmovdqu ymm1, [edx + ebx - 128];{$ELSE}db $C5,$FE,$6F,$4C,$1A,$80;{$ENDIF} 

     {$IFDEF FPC}vmovdqu [ecx + ebx - 128], ymm1;{$ELSE}db $C5,$FE,$7F,$4C,$19,$80;{$ENDIF} 
     {$IFDEF FPC}vmovdqu [edx + ebx - 128], ymm0;{$ELSE}db $C5,$FE,$7F,$44,$1A,$80;{$ENDIF} 

     {$IFDEF FPC}vmovdqu ymm2, [ecx + ebx - 96];{$ELSE}db $C5,$FE,$6F,$54,$19,$A0;{$ENDIF} 
     {$IFDEF FPC}vmovdqu ymm3, [edx + ebx - 96];{$ELSE}db $C5,$FE,$6F,$5C,$1A,$A0;{$ENDIF} 

     {$IFDEF FPC}vmovdqu [ecx + ebx - 96], ymm3;{$ELSE}db $C5,$FE,$7F,$5C,$19,$A0;{$ENDIF} 
     {$IFDEF FPC}vmovdqu [edx + ebx - 96], ymm2;{$ELSE}db $C5,$FE,$7F,$54,$1A,$A0;{$ENDIF} 

     {$IFDEF FPC}vmovdqu ymm0, [ecx + ebx - 64];{$ELSE}db $C5,$FE,$6F,$44,$19,$C0;{$ENDIF} 
     {$IFDEF FPC}vmovdqu ymm1, [edx + ebx - 64];{$ELSE}db $C5,$FE,$6F,$4C,$1A,$C0;{$ENDIF} 

     {$IFDEF FPC}vmovdqu [ecx + ebx - 64], ymm1;{$ELSE}db $C5,$FE,$7F,$4C,$19,$C0;{$ENDIF} 
     {$IFDEF FPC}vmovdqu [edx + ebx - 64], ymm0;{$ELSE}db $C5,$FE,$7F,$44,$1A,$C0;{$ENDIF} 

     {$IFDEF FPC}vmovdqu ymm2, [ecx + ebx - 32];{$ELSE}db $C5,$FE,$6F,$54,$19,$E0;{$ENDIF} 
     {$IFDEF FPC}vmovdqu ymm3, [edx + ebx - 32];{$ELSE}db $C5,$FE,$6F,$5C,$1A,$E0;{$ENDIF} 

     {$IFDEF FPC}vmovdqu [ecx + ebx - 32], ymm3;{$ELSE}db $C5,$FE,$7F,$5C,$19,$E0;{$ENDIF} 
     {$IFDEF FPC}vmovdqu [edx + ebx - 32], ymm2;{$ELSE}db $C5,$FE,$7F,$54,$1A,$E0;{$ENDIF} 
   jmp @unrolloop;
   @unrolloopend:

   sub ebx, 128;
   jz @endfunc;

   @loop2:
     add ebx, 16;
     jg @loop2End;

     {$IFDEF FPC}vmovdqu xmm0, [ecx + ebx - 16];{$ELSE}db $C5,$FA,$6F,$44,$19,$F0;{$ENDIF} 
     {$IFDEF FPC}vmovdqu xmm1, [edx + ebx - 16];{$ELSE}db $C5,$FA,$6F,$4C,$1A,$F0;{$ENDIF} 

     {$IFDEF FPC}vmovdqu [ecx + ebx - 16], xmm1;{$ELSE}db $C5,$FA,$7F,$4C,$19,$F0;{$ENDIF} 
     {$IFDEF FPC}vmovdqu [edx + ebx - 16], xmm0;{$ELSE}db $C5,$FA,$7F,$44,$1A,$F0;{$ENDIF} 
   jmp @loop2;

   @loop2End:
   sub ebx, 16;

   jz @endfunc;

   {$IFDEF FPC}vmovsd xmm0, [ecx + ebx];{$ELSE}db $C5,$FB,$10,$04,$19;{$ENDIF} 
   {$IFDEF FPC}vmovsd xmm1, [edx + ebx];{$ELSE}db $C5,$FB,$10,$0C,$1A;{$ENDIF} 

   {$IFDEF FPC}vmovsd [ecx + ebx], xmm1;{$ELSE}db $C5,$FB,$11,$0C,$19;{$ENDIF} 
   {$IFDEF FPC}vmovsd [edx + ebx], xmm0;{$ELSE}db $C5,$FB,$11,$04,$1A;{$ENDIF} 

   @endfunc:

   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 
   pop ebx;
end;
end;

procedure AVXMatrixCopyAligned(Dest : PDouble; const destLineWidth : TASMNativeInt; src : PDouble; const srcLineWidth : TASMNativeInt; Width, Height : TASMNativeInt);
begin
asm
   push ebx;
   push edi;
   push esi;

   //iters := -width*sizeof(double);
   mov esi, width;
   imul esi, -8;

   // helper registers for the src and dest pointers
   mov ebx, src;
   mov ecx, dest;
   sub ebx, esi;
   sub ecx, esi;

   // for y := 0 to height - 1:
   mov edi, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov eax, esi;
       @addforxloop:
           add eax, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [ebx + eax];
           // prefetchw [ecx + eax];

           // move:
           {$IFDEF FPC}vmovdqa ymm0, [ebx + eax - 128];{$ELSE}db $C5,$FD,$6F,$44,$03,$80;{$ENDIF} 
           {$IFDEF FPC}vmovdqa [ecx + eax - 128], ymm0;{$ELSE}db $C5,$FD,$7F,$44,$01,$80;{$ENDIF} 

           {$IFDEF FPC}vmovdqa ymm1, [ebx + eax - 96];{$ELSE}db $C5,$FD,$6F,$4C,$03,$A0;{$ENDIF} 
           {$IFDEF FPC}vmovdqa [ecx + eax - 96], ymm1;{$ELSE}db $C5,$FD,$7F,$4C,$01,$A0;{$ENDIF} 

           {$IFDEF FPC}vmovdqa ymm2, [ebx + eax - 64];{$ELSE}db $C5,$FD,$6F,$54,$03,$C0;{$ENDIF} 
           {$IFDEF FPC}vmovdqa [ecx + eax - 64], ymm2;{$ELSE}db $C5,$FD,$7F,$54,$01,$C0;{$ENDIF} 

           {$IFDEF FPC}vmovdqa ymm3, [ebx + eax - 32];{$ELSE}db $C5,$FD,$6F,$5C,$03,$E0;{$ENDIF} 
           {$IFDEF FPC}vmovdqa [ecx + eax - 32], ymm3;{$ELSE}db $C5,$FD,$7F,$5C,$01,$E0;{$ENDIF} 

       jmp @addforxloop

       @loopEnd:

       sub eax, 128;

       jz @nextLine;

       @addforxloop2:
          add eax, 16;
          jg @loopEnd2;

          {$IFDEF FPC}vmovdqa xmm0, [ebx + eax - 16];{$ELSE}db $C5,$F9,$6F,$44,$03,$F0;{$ENDIF} 
          {$IFDEF FPC}vmovdqa [ecx + eax - 16], xmm0;{$ELSE}db $C5,$F9,$7F,$44,$01,$F0;{$ENDIF} 
       jmp @addforxloop2;

       @loopEnd2:
       sub eax, 16;

       jz @nextLine;

       // last element
       {$IFDEF FPC}vmovsd xmm0, [ebx + eax];{$ELSE}db $C5,$FB,$10,$04,$03;{$ENDIF} 
       {$IFDEF FPC}vmovsd [ecx + eax], xmm0;{$ELSE}db $C5,$FB,$11,$04,$01;{$ENDIF} 

       @nextLine:

       // next line:
       add ebx, srcLineWidth;
       add ecx, destLineWidth;

   // loop y end
   dec edi;
   jnz @@addforyloop;

   // epilog
   {$IFDEF FPC}vzeroupper;{$ELSE}db $C5,$F8,$77;{$ENDIF} 
   pop esi;
   pop edi;
   pop ebx;
end;
end;

procedure AVXMatrixCopyUnAligned(Dest : PDouble; const destLineWidth : TASMNativeInt; src : PDouble; const srcLineWidth : TASMNativeInt; Width, Height : TASMNativeInt);
begin
asm
   push ebx;
   push edi;
   push esi;

   // note: ecx = dest, edx = destLineWidth, ebx = src, R9 = srcLineWidth
   //iters := -width*sizeof(double);
   mov esi, width;
   imul esi, -8;

   // helper registers for the src and dest pointers
   mov ebx, src;
   mov ecx, dest;
   sub ebx, esi;
   sub ecx, esi;

   // for y := 0 to height - 1:
   mov edi, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov eax, esi;
       @addforxloop:
           add eax, 128;
           jg @loopEnd;

           // prefetch data...
           // prefetch [ebx + eax];
           // prefetchw [ecx + eax];

           // move:
           {$IFDEF FPC}vmovdqu ymm0, [ebx + eax - 128];{$ELSE}db $C5,$FE,$6F,$44,$03,$80;{$ENDIF} 
           {$IFDEF FPC}vmovdqu [ecx + eax - 128], ymm0;{$ELSE}db $C5,$FE,$7F,$44,$01,$80;{$ENDIF} 

           {$IFDEF FPC}vmovdqu ymm1, [ebx + eax - 96];{$ELSE}db $C5,$FE,$6F,$4C,$03,$A0;{$ENDIF} 
           {$IFDEF FPC}vmovdqu [ecx + eax - 96], ymm1;{$ELSE}db $C5,$FE,$7F,$4C,$01,$A0;{$ENDIF} 

           {$IFDEF FPC}vmovdqu ymm2, [ebx + eax - 64];{$ELSE}db $C5,$FE,$6F,$54,$03,$C0;{$ENDIF} 
           {$IFDEF FPC}vmovdqu [ecx + eax - 64], ymm2;{$ELSE}db $C5,$FE,$7F,$54,$01,$C0;{$ENDIF} 

           {$IFDEF FPC}vmovdqu ymm3, [ebx + eax - 32];{$ELSE}db $C5,$FE,$6F,$5C,$03,$E0;{$ENDIF} 
           {$IFDEF FPC}vmovdqu [ecx + eax - 32], ymm3;{$ELSE}db $C5,$FE,$7F,$5C,$01,$E0;{$ENDIF} 

       jmp @addforxloop

       @loopEnd:

       sub eax, 128;

       jz @nextLine;

       @addforxloop2:
          add eax, 16;
          jg @loopEnd2;

          {$IFDEF FPC}vmovdqu xmm0, [ebx + eax - 16];{$ELSE}db $C5,$FA,$6F,$44,$03,$F0;{$ENDIF} 
          {$IFDEF FPC}vmovdqu [ecx + eax - 16], xmm0;{$ELSE}db $C5,$FA,$7F,$44,$01,$F0;{$ENDIF} 
       jmp @addforxloop2;

       @loopEnd2:
       sub eax, 16;

       jz @nextLine;

       // last element
       {$IFDEF FPC}vmovsd xmm0, [ebx + eax];{$ELSE}db $C5,$FB,$10,$04,$03;{$ENDIF} 
       {$IFDEF FPC}vmovsd [ecx + eax], xmm0;{$ELSE}db $C5,$FB,$11,$04,$01;{$ENDIF} 

       @nextLine:

       // next line:
       add ebx, srcLineWidth;
       add ecx, destLineWidth;

   // loop y end
   dec edi;
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
