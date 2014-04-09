unit ASMMatrixAbsOperationsx64;

// #####################################################
// #### SQRT opertaion applied to every element in a matrix
// #####################################################

interface

{$IFDEF CPUX64}

uses ASMConsts;

procedure ASMMatrixAbsAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt);
procedure ASMMatrixAbsUnAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt);

procedure ASMMatrixAbsAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt);
procedure ASMMatrixAbsUnAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt);

{$ENDIF}

implementation

{$IFDEF CPUX64}

const cSignBits : Array[0..1] of int64 = ($7FFFFFFFFFFFFFFF, $7FFFFFFFFFFFFFFF);

procedure ASMMatrixAbsAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt);
asm
	  // note: RCX = dest, RDX = destLineWidth, R8 = width, R9 = height
   //iters := -width*sizeof(double);
   mov r10, width;
   shl r10, 3;
   imul r10, -1;

   // helper registers for the mt1, mt2 and dest pointers
   sub dest, r10;

   movupd xmm0, cSignBits;

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
           //prefetchw [rcx + rax];

           // Abs:
           movapd xmm4, [rcx + rax - 128];
           Andpd xmm4, xmm0;
           movntdq [rcx + rax - 128], xmm4;

           movapd xmm1, [rcx + rax - 112];
           andpd xmm1, xmm0;
           movntdq [rcx + rax - 112], xmm1;

           movapd xmm2, [rcx + rax - 96];
           andpd xmm2, xmm0;
           movntdq [rcx + rax - 96], xmm2;

           movapd xmm3, [rcx + rax - 80];
           andpd xmm3, xmm0;
           movntdq [rcx + rax - 80], xmm3;

           movapd xmm4, [rcx + rax - 64];
           andpd xmm4, xmm0;
           movntdq [rcx + rax - 64], xmm4;

           movapd xmm5, [rcx + rax - 48];
           andpd xmm5, xmm0;
           movntdq [rcx + rax - 48], xmm5;

           movapd xmm6, [rcx + rax - 32];
           andpd xmm6, xmm0;
           movntdq [rcx + rax - 32], xmm6;

           movapd xmm7, [rcx + rax - 16];
           andpd xmm7, xmm0;
           movntdq [rcx + rax - 16], xmm7;
       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           movapd xmm7, [rcx + rax];
           andpd xmm7, xmm0;
           movntdq [rcx + rax], xmm7;
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // next line:
       add rcx, rdx;

   // loop y end
   dec r11;
   jnz @@addforyloop;
end;

procedure ASMMatrixAbsUnAlignedEvenW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt);
asm
	  // note: RCX = dest, RDX = destLineWidth, R8 = width, R9 = height
   //iters := -width*sizeof(double);
   mov r10, width;
   shl r10, 3;
   imul r10, -1;

   // helper registers for the mt1, mt2 and dest pointers
   sub dest, r10;

   movupd xmm0, cSignBits;

   // for y := 0 to height - 1:
   mov r11, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // Abs:
           movupd xmm4, [rcx + rax - 128];
           Andpd xmm4, xmm0;
           movupd [rcx + rax - 128], xmm4;

           movupd xmm1, [rcx + rax - 112];
           andpd xmm1, xmm0;
           movupd [rcx + rax - 112], xmm1;

           movupd xmm2, [rcx + rax - 96];
           andpd xmm2, xmm0;
           movupd [rcx + rax - 96], xmm2;

           movupd xmm3, [rcx + rax - 80];
           andpd xmm3, xmm0;
           movupd [rcx + rax - 80], xmm3;

           movupd xmm4, [rcx + rax - 64];
           andpd xmm4, xmm0;
           movupd [rcx + rax - 64], xmm4;

           movupd xmm5, [rcx + rax - 48];
           andpd xmm5, xmm0;
           movupd [rcx + rax - 48], xmm5;

           movupd xmm6, [rcx + rax - 32];
           andpd xmm6, xmm0;
           movupd [rcx + rax - 32], xmm6;

           movupd xmm7, [rcx + rax - 16];
           andpd xmm7, xmm0;
           movupd [rcx + rax - 16], xmm7;

       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           movupd xmm1, [rcx + rax];
           andpd xmm1, xmm0;
           movupd [rcx + rax], xmm1;
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // next line:
       add rcx, rdx;

   // loop y end
   dec r11;
   jnz @@addforyloop;
end;

procedure ASMMatrixAbsAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt);
asm
	  // note: RCX = dest, RDX = destLineWidth, R8 = width, R9 = height
   //iters := -(width - 1)*sizeof(double);
   mov r10, width;
   dec r10;
   shl r10, 3;
   imul r10, -1;

   // helper registers for the mt1, mt2 and dest pointers
   sub dest, r10;

   movupd xmm0, cSignBits;

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
           //prefetchw [rcx + rax];

           // Abs:
           movapd xmm4, [rcx + rax - 128];
           Andpd xmm4, xmm0;
           movntdq [rcx + rax - 128], xmm4;

           movapd xmm1, [rcx + rax - 112];
           andpd xmm1, xmm0;
           movntdq [rcx + rax - 112], xmm1;

           movapd xmm2, [rcx + rax - 96];
           andpd xmm2, xmm0;
           movntdq [rcx + rax - 96], xmm2;

           movapd xmm3, [rcx + rax - 80];
           andpd xmm3, xmm0;
           movntdq [rcx + rax - 80], xmm3;

           movapd xmm4, [rcx + rax - 64];
           andpd xmm4, xmm0;
           movntdq [rcx + rax - 64], xmm4;

           movapd xmm5, [rcx + rax - 48];
           andpd xmm5, xmm0;
           movntdq [rcx + rax - 48], xmm5;

           movapd xmm6, [rcx + rax - 32];
           andpd xmm6, xmm0;
           movntdq [rcx + rax - 32], xmm6;

           movapd xmm7, [rcx + rax - 16];
           andpd xmm7, xmm0;
           movntdq [rcx + rax - 16], xmm7;

       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           movapd xmm7, [rcx + rax];
           andpd xmm7, xmm0;
           movntdq [rcx + rax], xmm7;
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last element
       movsd xmm1, [rcx + rax];
       andpd xmm1, xmm0;
       movsd [rcx + rax], xmm1;

       // next line:
       add rcx, rdx;

   // loop y end
   dec r11;
   jnz @@addforyloop;
end;

procedure ASMMatrixAbsUnAlignedOddW(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt);
asm
	  // note: RCX = dest, RDX = destLineWidth, R8 = width, R9 = height
   //iters := -width*sizeof(double);
   mov r10, width;
   shl r10, 3;
   imul r10, -1;

   // helper registers for the mt1, mt2 and dest pointers
   sub dest, r10;

   movupd xmm0, cSignBits;

   // for y := 0 to height - 1:
   mov r11, Height;
   @@addforyloop:
       // for x := 0 to w - 1;
       // prepare for reverse loop
       mov rax, r10;
       @addforxloop:
           add rax, 128;
           jg @loopEnd;

           // Abs:
           movupd xmm4, [rcx + rax - 128];
           Andpd xmm4, xmm0;
           movupd [rcx + rax - 128], xmm4;

           movupd xmm1, [rcx + rax - 112];
           andpd xmm1, xmm0;
           movupd [rcx + rax - 112], xmm1;

           movupd xmm2, [rcx + rax - 96];
           andpd xmm2, xmm0;
           movupd [rcx + rax - 96], xmm2;

           movupd xmm3, [rcx + rax - 80];
           andpd xmm3, xmm0;
           movupd [rcx + rax - 80], xmm3;

           movupd xmm4, [rcx + rax - 64];
           andpd xmm4, xmm0;
           movupd [rcx + rax - 64], xmm4;

           movupd xmm5, [rcx + rax - 48];
           andpd xmm5, xmm0;
           movupd [rcx + rax - 48], xmm5;

           movupd xmm6, [rcx + rax - 32];
           andpd xmm6, xmm0;
           movupd [rcx + rax - 32], xmm6;

           movupd xmm7, [rcx + rax - 16];
           andpd xmm7, xmm0;
           movupd [rcx + rax - 16], xmm7;

       jmp @addforxloop

       @loopEnd:

       sub rax, 128;

       jz @nextLine;

       @addforxloop2:
           movupd xmm1, [rcx + rax];
           adnpd xmm1, xmm0;
           movupd [rcx + rax], xmm1;
       add rax, 16;
       jnz @addforxloop2;

       @nextLine:

       // special care of the last element
       movsd xmm1, [rcx + rax];
       andpd xmm1, xmm0;
       movsd [rcx + rax], xmm1;

       // next line:
       add rcx, rdx;

   // loop y end
   dec r11;
   jnz @@addforyloop;
end;

{$ENDIF}

end.
