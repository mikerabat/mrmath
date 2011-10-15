// ###################################################################
// #### This file is part of the mathematics library project, and is
// #### offered under the licence agreement described on
// #### http://www.mrsoft.org/
// ####
// #### Copyright:(c) 2011, Michael R. . All rights reserved.
// ####
// #### Unless required by applicable law or agreed to in writing, software
// #### distributed under the License is distributed on an "AS IS" BASIS,
// #### WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// #### See the License for the specific language governing permissions and
// #### limitations under the License.
// ###################################################################


unit ASMMoveOperationsx64;

// #################################################
// #### SSE optimized move oprationes
// #################################################

interface

{$IFDEF CPUX64}

uses ASMConsts;

procedure ASMMatrixCopyAlignedEvenW(Dest : PDouble; const destLineWidth : TASMNativeInt; src : PDouble; const srcLineWidth : TASMNativeInt; Width, Height : TASMNativeInt);
procedure ASMMatrixCopyUnAlignedEvenW(Dest : PDouble; const destLineWidth : TASMNativeInt; src : PDouble; const srcLineWidth : TASMNativeInt; Width, Height : TASMNativeInt);

procedure ASMMatrixCopyAlignedOddW(Dest : PDouble; const destLineWidth : TASMNativeInt; src : PDouble; const srcLineWidth : TASMNativeInt; Width, Height : TASMNativeInt);
procedure ASMMatrixCopyUnAlignedOddW(Dest : PDouble; const destLineWidth : TASMNativeInt; src : PDouble; const srcLineWidth : TASMNativeInt; Width, Height : TASMNativeInt);

{$ENDIF}

implementation

{$IFDEF CPUX64}

procedure ASMMatrixCopyAlignedEvenW(Dest : PDouble; const destLineWidth : TASMNativeInt; src : PDouble; const srcLineWidth : TASMNativeInt; Width, Height : TASMNativeInt);
asm
   // note: RCX = dest, RDX = destLineWidth, R8 = mt1, R9 = mt2
   //iters := -width*sizeof(double);
   mov r10, width;
   shl r10, 3;
   imul r10, -1;

   // helper registers for the mt1, mt2 and dest pointers
   sub src, r10;
   sub dest, r10;

   // for y := 0 to height - 1:
   mov r11, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // prefetch data...
           prefetch [r8 + rax];
           prefetchw [rcx + rax];

           // move:
           movapd xmm0, [r8 + rax - 128];
           movntdq [rcx + rax - 128], xmm0;

           movapd xmm1, [r8 + rax - 112];
           movntdq [rcx + rax - 112], xmm1;

           movapd xmm2, [r8 + rax - 96];
           movntdq [rcx + rax - 96], xmm2;

           movapd xmm3, [r8 + rax - 80];
           movntdq [rcx + rax - 80], xmm3;

           movapd xmm0, [r8 + rax - 64];
           movntdq [rcx + rax - 64], xmm0;

           movapd xmm1, [r8 + rax - 48];
           movntdq [rcx + rax - 48], xmm1;

           movapd xmm2, [r8 + rax - 32];
           movntdq [rcx + rax - 32], xmm2;

           movapd xmm3, [r8 + rax - 16];
           movntdq [rcx + rax - 16], xmm3;
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           movapd xmm0, [r8 + rax];
           movntdq [rcx + rax], xmm0;
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // next line:
       add r8, r9;
       add rcx, rdx;

   // loop y end
   dec r11;
   jnz @@addforyloop;
end;

procedure ASMMatrixCopyUnAlignedEvenW(Dest : PDouble; const destLineWidth : TASMNativeInt; src : PDouble; const srcLineWidth : TASMNativeInt; Width, Height : TASMNativeInt);
asm
   // note: RCX = dest, RDX = destLineWidth, R8 = mt1, R9 = mt2
   //iters := -width*sizeof(double);
   mov r10, width;
   shl r10, 3;
   imul r10, -1;

   // helper registers for the mt1, mt2 and dest pointers
   sub src, r10;
   sub dest, r10;

   // for y := 0 to height - 1:
   mov r11, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // move:
           movupd xmm0, [r8 + rax - 128];
           movupd [rcx + rax - 128], xmm0;

           movupd xmm1, [r8 + rax - 112];
           movupd [rcx + rax - 112], xmm1;

           movupd xmm2, [r8 + rax - 96];
           movupd [rcx + rax - 96], xmm2;

           movupd xmm3, [r8 + rax - 80];
           movupd [rcx + rax - 80], xmm3;

           movupd xmm0, [r8 + rax - 64];
           movupd [rcx + rax - 64], xmm0;

           movupd xmm1, [r8 + rax - 48];
           movupd [rcx + rax - 48], xmm1;

           movupd xmm2, [r8 + rax - 32];
           movupd [rcx + rax - 32], xmm2;

           movupd xmm3, [r8 + rax - 16];
           movupd [rcx + rax - 16], xmm3;
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           movupd xmm0, [r8 + rax];
           movupd [rcx + rax], xmm0;
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // next line:
       add r8, r9;
       add rcx, rdx;

   // loop y end
   dec r11;
   jnz @@addforyloop;
end;

procedure ASMMatrixCopyAlignedOddW(Dest : PDouble; const destLineWidth : TASMNativeInt; src : PDouble; const srcLineWidth : TASMNativeInt; Width, Height : TASMNativeInt);
asm
   // note: RCX = dest, RDX = destLineWidth, R8 = mt1, R9 = mt2
   //iters := -(width - 1)*sizeof(double);
   mov r10, width;
   dec r10;
   shl r10, 3;
   imul r10, -1;

   // helper registers for the mt1, mt2 and dest pointers
   sub src, r10;
   sub dest, r10;

   // for y := 0 to height - 1:
   mov r11, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // prefetch data...
           prefetch [r8 + rax];
           prefetchw [rcx + rax];

           // move:
           movapd xmm0, [r8 + rax - 128];
           movntdq [rcx + rax - 128], xmm0;

           movapd xmm1, [r8 + rax - 112];
           movntdq [rcx + rax - 112], xmm1;

           movapd xmm2, [r8 + rax - 96];
           movntdq [rcx + rax - 96], xmm2;

           movapd xmm3, [r8 + rax - 80];
           movntdq [rcx + rax - 80], xmm3;

           movapd xmm0, [r8 + rax - 64];
           movntdq [rcx + rax - 64], xmm0;

           movapd xmm1, [r8 + rax - 48];
           movntdq [rcx + rax - 48], xmm1;

           movapd xmm2, [r8 + rax - 32];
           movntdq [rcx + rax - 32], xmm2;

           movapd xmm3, [r8 + rax - 16];
           movntdq [rcx + rax - 16], xmm3;
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           movapd xmm0, [r8 + rax];
           movntdq [rcx + rax], xmm0;
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last element
       movlpd xmm0, [r8];
       movlpd [rcx], xmm0;

       // next line:
       add r8, r9;
       add rcx, rdx;

   // loop y end
   dec r11;
   jnz @@addforyloop;
end;

procedure ASMMatrixCopyUnAlignedOddW(Dest : PDouble; const destLineWidth : TASMNativeInt; src : PDouble; const srcLineWidth : TASMNativeInt; Width, Height : TASMNativeInt);
asm
   // note: RCX = dest, RDX = destLineWidth, R8 = mt1, R9 = mt2
   //iters := -(width - 1)*sizeof(double);
   mov r10, width;
   dec r10;
   shl r10, 3;
   imul r10, -1;

   // helper registers for the mt1, mt2 and dest pointers
   sub src, r10;
   sub dest, r10;

   // for y := 0 to height - 1:
   mov r11, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // move:
           movupd xmm0, [r8 + rax - 128];
           movupd [rcx + rax - 128], xmm0;

           movupd xmm1, [r8 + rax - 112];
           movupd [rcx + rax - 112], xmm1;

           movupd xmm2, [r8 + rax - 96];
           movupd [rcx + rax - 96], xmm2;

           movupd xmm3, [r8 + rax - 80];
           movupd [rcx + rax - 80], xmm3;

           movupd xmm0, [r8 + rax - 64];
           movupd [rcx + rax - 64], xmm0;

           movupd xmm1, [r8 + rax - 48];
           movupd [rcx + rax - 48], xmm1;

           movupd xmm2, [r8 + rax - 32];
           movupd [rcx + rax - 32], xmm2;

           movupd xmm3, [r8 + rax - 16];
           movupd [rcx + rax - 16], xmm3;
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           movupd xmm0, [r8 + rax];
           movupd [rcx + rax], xmm0;
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last element
       movlpd xmm0, [r8];
       movlpd [rcx], xmm0;

       // next line:
       add r8, r9;
       add rcx, rdx;

   // loop y end
   dec r11;
   jnz @@addforyloop;
end;

{$ENDIF}

end.
