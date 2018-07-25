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


unit ASMMoveOperations;

// #################################################
// #### SSE optimized move oprationes
// #################################################

interface

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}
{$IFNDEF x64}

uses MatrixConst;

procedure ASMMatrixCopyAlignedEvenW(Dest : PDouble; const destLineWidth : TASMNativeInt; src : PDouble; const srcLineWidth : TASMNativeInt; Width, Height : TASMNativeInt);
procedure ASMMatrixCopyUnAlignedEvenW(Dest : PDouble; const destLineWidth : TASMNativeInt; src : PDouble; const srcLineWidth : TASMNativeInt; Width, Height : TASMNativeInt);

procedure ASMMatrixCopyAlignedOddW(Dest : PDouble; const destLineWidth : TASMNativeInt; src : PDouble; const srcLineWidth : TASMNativeInt; Width, Height : TASMNativeInt);
procedure ASMMatrixCopyUnAlignedOddW(Dest : PDouble; const destLineWidth : TASMNativeInt; src : PDouble; const srcLineWidth : TASMNativeInt; Width, Height : TASMNativeInt);

procedure ASMRowSwapAlignedEvenW(A, B : PDouble; width : TASMNativeInt);
procedure ASMRowSwapUnAlignedEvenW(A, B : PDouble; width : TASMNativeInt);

procedure ASMRowSwapAlignedOddW(A, B : PDouble; width : TASMNativeInt);
procedure ASMRowSwapUnAlignedOddW(A, B : PDouble; width : TASMNativeInt);

// it is assumed that this function has multiple of sizeof(double) as numbytes
procedure ASMInitMemAligned(A : PDouble; NumBytes : TASMNativeInt; const Value : double);

{$ENDIF}

implementation

{$IFNDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$S-} {$ENDIF}

// uses non temporal moves so the cache is not poisned
procedure ASMInitMemAligned(A : PDouble; NumBytes : TASMNativeInt; const Value : double);
begin
     asm
        movddup xmm0, Value;

        mov eax, A;
        mov ecx, NumBytes;
        imul ecx, -1;
        sub eax, ecx;

        @@loopUnrolled:
          add ecx, 128;
          jg @@loopUnrolledEnd;
        
          movntdq [eax + ecx - 128], xmm0;
          movntdq [eax + ecx - 112], xmm0;
          movntdq [eax + ecx - 96], xmm0;
          movntdq [eax + ecx - 80], xmm0;
          movntdq [eax + ecx - 64], xmm0;
          movntdq [eax + ecx - 48], xmm0;
          movntdq [eax + ecx - 32], xmm0;
          movntdq [eax + ecx - 16], xmm0;

        jmp @@loopUnrolled;

        @@loopUnrolledEnd:

        // last few bytes
        sub ecx, 128;
        jz @@exitProc;

        @@loop:
          movsd [eax + ecx], xmm0;
          add ecx, 8;
        jnz @@loop;

        @@exitProc:
     end;
end;

procedure ASMRowSwapAlignedEvenW(A, B : PDouble; width : TASMNativeInt);
var iters : TASMNativeInt;
begin
     iters := -width*sizeof(double);

     asm
        mov eax, A;
        mov ecx, B;

        mov edx, iters;

        sub eax, edx;
        sub ecx, edx;

        @unrolloop:
          add edx, 64;
          jg @unrolloopend;

          // prefetchw [eax + edx];
          // prefetchw [ecx + edx];

          movdqa xmm0, [eax + edx - 64];
          movdqa xmm1, [ecx + edx - 64];

          movdqa [eax + edx - 64], xmm1;
          movdqa [ecx + edx - 64], xmm0;

          movdqa xmm2, [eax + edx - 48];
          movdqa xmm3, [ecx + edx - 48];

          movdqa [eax + edx - 48], xmm3;
          movdqa [ecx + edx - 48], xmm2;

          movdqa xmm4, [eax + edx - 32];
          movdqa xmm5, [ecx + edx - 32];

          movdqa [eax + edx - 32], xmm5;
          movdqa [ecx + edx - 32], xmm4;

          movdqa xmm6, [eax + edx - 16];
          movdqa xmm7, [ecx + edx - 16];

          movdqa [eax + edx - 16], xmm7;
          movdqa [ecx + edx - 16], xmm6;
        jmp @unrolloop;
        @unrolloopend:

        sub edx, 64;
        jz @endfunc;


        @loop:
          movdqa xmm0, [eax + edx];
          movdqa xmm1, [ecx + edx];

          movdqa [eax + edx], xmm1;
          movdqa [ecx + edx], xmm0;

          add edx, 16;
        jnz @loop;

        @endfunc:
     end;
end;

procedure ASMRowSwapUnAlignedEvenW(A, B : PDouble; width : TASMNativeInt);
var iters : TASMNativeInt;
begin
     iters := -width*sizeof(double);

     asm
        mov eax, A;
        mov ecx, B;

        mov edx, iters;

        sub eax, edx;
        sub ecx, edx;

        @unrolloop:
          add edx, 64;
          jg @unrolloopend;

          movdqu xmm0, [eax + edx - 64];
          movdqu xmm1, [ecx + edx - 64];

          movdqu [eax + edx - 64], xmm1;
          movdqu [ecx + edx - 64], xmm0;

          movdqu xmm2, [eax + edx - 48];
          movdqu xmm3, [ecx + edx - 48];

          movdqu [eax + edx - 48], xmm3;
          movdqu [ecx + edx - 48], xmm2;

          movdqu xmm4, [eax + edx - 32];
          movdqu xmm5, [ecx + edx - 32];

          movdqu [eax + edx - 32], xmm5;
          movdqu [ecx + edx - 32], xmm4;

          movdqu xmm6, [eax + edx - 16];
          movdqu xmm7, [ecx + edx - 16];

          movdqu [eax + edx - 16], xmm7;
          movdqu [ecx + edx - 16], xmm6;
        jmp @unrolloop;
        @unrolloopend:

        sub edx, 64;
        jz @endfunc;

        @loop:
          movdqu xmm0, [eax + edx];
          movdqu xmm1, [ecx + edx];

          movdqu [eax + edx], xmm1;
          movdqu [ecx + edx], xmm0;

          add edx, 16;
        jnz @loop;

        @endfunc:
     end;
end;

procedure ASMRowSwapAlignedOddW(A, B : PDouble; width : TASMNativeInt);
var iters : TASMNativeInt;
begin
     iters := -(width - 1)*sizeof(double);

     asm
        mov eax, A;
        mov ecx, B;

        mov edx, iters;
        jz @endfunc;

        sub eax, edx;
        sub ecx, edx;


        @unrolloop:
          add edx, 64;
          jg @unrolloopend;

          // prefetchw [eax + edx];
          // prefetchw [ecx + edx];

          movdqa xmm0, [eax + edx - 64];
          movdqa xmm1, [ecx + edx - 64];

          movdqa [eax + edx - 64], xmm1;
          movdqa [ecx + edx - 64], xmm0;

          movdqa xmm2, [eax + edx - 48];
          movdqa xmm3, [ecx + edx - 48];

          movdqa [eax + edx - 48], xmm3;
          movdqa [ecx + edx - 48], xmm2;

          movdqa xmm4, [eax + edx - 32];
          movdqa xmm5, [ecx + edx - 32];

          movdqa [eax + edx - 32], xmm5;
          movdqa [ecx + edx - 32], xmm4;

          movdqa xmm6, [eax + edx - 16];
          movdqa xmm7, [ecx + edx - 16];

          movdqa [eax + edx - 16], xmm7;
          movdqa [ecx + edx - 16], xmm6;

        jmp @unrolloop;
        @unrolloopend:

        sub edx, 64;
        jz @endfunc;


        @loop:
          movdqa xmm0, [eax + edx];
          movdqa xmm1, [ecx + edx];

          movdqa [eax + edx], xmm1;
          movdqa [ecx + edx], xmm0;

          add edx, 16;
        jnz @loop;

        @endfunc:

        // last swap
        movsd xmm0, [eax];
        movsd xmm1, [ecx];

        movsd [eax], xmm1;
        movsd [ecx], xmm0;
     end;
end;

procedure ASMRowSwapUnAlignedOddW(A, B : PDouble; width : TASMNativeInt);
var iters : TASMNativeInt;
begin
     iters := -(width - 1)*sizeof(double);

     asm
        mov eax, A;
        mov ecx, B;

        mov edx, iters;
        jz @endfunc;

        sub eax, edx;
        sub ecx, edx;

        @unrolloop:
          add edx, 64;
          jg @unrolloopend;

          movdqu xmm0, [eax + edx - 64];
          movdqu xmm1, [ecx + edx - 64];

          movdqu [eax + edx - 64], xmm1;
          movdqu [ecx + edx - 64], xmm0;

          movdqu xmm2, [eax + edx - 48];
          movdqu xmm3, [ecx + edx - 48];

          movdqu [eax + edx - 48], xmm3;
          movdqu [ecx + edx - 48], xmm2;

          movdqu xmm4, [eax + edx - 32];
          movdqu xmm5, [ecx + edx - 32];

          movdqu [eax + edx - 32], xmm5;
          movdqu [ecx + edx - 32], xmm4;

          movdqu xmm6, [eax + edx - 16];
          movdqu xmm7, [ecx + edx - 16];

          movdqu [eax + edx - 16], xmm7;
          movdqu [ecx + edx - 16], xmm6;
        jmp @unrolloop;
        @unrolloopend:

        sub edx, 64;
        jz @endfunc;

        @loop:
          movdqu xmm0, [eax + edx];
          movdqu xmm1, [ecx + edx];

          movdqu [eax + edx], xmm1;
          movdqu [ecx + edx], xmm0;

          add edx, 16;
        jnz @loop;

        @endfunc:

        // last swap
        movsd xmm0, [eax];
        movsd xmm1, [ecx];

        movsd [eax], xmm1;
        movsd [ecx], xmm0;
     end;
end;

procedure ASMMatrixCopyAlignedEvenW(Dest : PDouble; const destLineWidth : TASMNativeInt; src : PDouble; const srcLineWidth : TASMNativeInt; Width, Height : TASMNativeInt);
var iters : TASMNativeInt;
begin
     assert((width > 0) and (height > 0) and (destLineWidth >= width*sizeof(double)) and (srcLineWidth >= width*sizeof(double)), 'Dimension error');

     iters := -width*sizeof(double);

     asm
        push esi;

        // helper registers for the mt1, mt2 and dest pointers
        mov esi, src;
        sub esi, iters;
        mov ecx, dest;
        sub ecx, iters;

        // for y := 0 to height - 1:
        mov edx, Height;
        @@addforyloop:
            // for x := 0 to w - 1;
            // prepare for reverse loop
            mov eax, iters;
            @addforxloop:
                add eax, 128;
                jg @loopEnd;

//                // prefetch [esi + eax];
//                // prefetchw [ecx + eax];

                // move:
                movdqa xmm0, [esi + eax - 128];
                movdqa [ecx + eax - 128], xmm0;

                movdqa xmm1, [esi + eax - 112];
                movdqa [ecx + eax - 112], xmm1;

                movdqa xmm2, [esi + eax - 96];
                movdqa [ecx + eax - 96], xmm2;

                movdqa xmm3, [esi + eax - 80];
                movdqa [ecx + eax - 80], xmm3;

                movdqa xmm4, [esi + eax - 64];
                movdqa [ecx + eax - 64], xmm4;

                movdqa xmm5, [esi + eax - 48];
                movdqa [ecx + eax - 48], xmm5;

                movdqa xmm6, [esi + eax - 32];
                movdqa [ecx + eax - 32], xmm6;

                movdqa xmm7, [esi + eax - 16];
                movdqa [ecx + eax - 16], xmm7;
            jmp @addforxloop

            @loopEnd:

            sub eax, 128;

            jz @nextLine;

            @addforxloop2:
                movdqa xmm0, [esi + eax];
                movdqa [ecx + eax], xmm0;
            add eax, 16;
            jnz @addforxloop2;

            @nextLine:

            // next line:
            add esi, srcLineWidth;
            add ecx, destLineWidth;

        // loop y end
        dec edx;
        jnz @@addforyloop;

        pop esi;
     end;
end;

procedure ASMMatrixCopyUnAlignedEvenW(Dest : PDouble; const destLineWidth : TASMNativeInt; src : PDouble; const srcLineWidth : TASMNativeInt; Width, Height : TASMNativeInt);
var iters : TASMNativeInt;
begin
     assert((width > 0) and (height > 0) and (destLineWidth >= width*sizeof(double)) and (srcLineWidth >= width*sizeof(double)), 'Dimension error');

     iters := -width*sizeof(double);

     asm
        push esi;

        // helper registers for the mt1, mt2 and dest pointers
        mov esi, src;
        sub esi, iters;
        mov ecx, dest;
        sub ecx, iters;

        // for y := 0 to height - 1:
        mov edx, Height;
        @@addforyloop:
            // for x := 0 to w - 1;
            // prepare for reverse loop
            mov eax, iters;
            @addforxloop:
                add eax, 128;
                jg @loopEnd;

                // move:
                movdqu xmm0, [esi + eax - 128];
                movdqu [ecx + eax - 128], xmm0;

                movdqu xmm1, [esi + eax - 112];
                movdqu [ecx + eax - 112], xmm1;

                movdqu xmm2, [esi + eax - 96];
                movdqu [ecx + eax - 96], xmm2;

                movdqu xmm3, [esi + eax - 80];
                movdqu [ecx + eax - 80], xmm3;

                movdqu xmm4, [esi + eax - 64];
                movdqu [ecx + eax - 64], xmm4;

                movdqu xmm5, [esi + eax - 48];
                movdqu [ecx + eax - 48], xmm5;

                movdqu xmm6, [esi + eax - 32];
                movdqu [ecx + eax - 32], xmm6;

                movdqu xmm7, [esi + eax - 16];
                movdqu [ecx + eax - 16], xmm7;
            jmp @addforxloop

            @loopEnd:

            sub eax, 128;

            jz @nextLine;

            @addforxloop2:
                movdqu xmm0, [esi + eax];
                movdqu [ecx + eax], xmm0;
            add eax, 16;
            jnz @addforxloop2;

            @nextLine:

            // next line:
            add esi, srcLineWidth;
            add ecx, destLineWidth;

        // loop y end
        dec edx;
        jnz @@addforyloop;

        pop esi;
     end;
end;

procedure ASMMatrixCopyAlignedOddW(Dest : PDouble; const destLineWidth : TASMNativeInt; src : PDouble; const srcLineWidth : TASMNativeInt; Width, Height : TASMNativeInt);
var iters : TASMNativeInt;
begin
     assert((width > 0) and (height > 0) and (destLineWidth >= width*sizeof(double)) and (srcLineWidth >= width*sizeof(double)), 'Dimension error');

     iters := -(width - 1)*sizeof(double);

     asm
        push esi;

        // helper registers for the mt1, mt2 and dest pointers
        mov esi, src;
        sub esi, iters;
        mov ecx, dest;
        sub ecx, iters;

        // for y := 0 to height - 1:
        mov edx, Height;
        @@addforyloop:
            // for x := 0 to w - 1;
            // prepare for reverse loop
            mov eax, iters;
            @addforxloop:
                add eax, 128;
                jg @loopEnd;

                // prefetch data...
                // prefetch [esi + eax];
                // prefetchw [ecx + eax];

                // addition:
                movdqa xmm0, [esi + eax - 128];
                movdqa [ecx + eax - 128], xmm0;

                movdqa xmm1, [esi + eax - 112];
                movdqa [ecx + eax - 112], xmm1;

                movdqa xmm2, [esi + eax - 96];
                movdqa [ecx + eax - 96], xmm2;

                movdqa xmm3, [esi + eax - 80];
                movdqa [ecx + eax - 80], xmm3;

                movdqa xmm4, [esi + eax - 64];
                movdqa [ecx + eax - 64], xmm4;

                movdqa xmm5, [esi + eax - 48];
                movdqa [ecx + eax - 48], xmm5;

                movdqa xmm6, [esi + eax - 32];
                movdqa [ecx + eax - 32], xmm6;

                movdqa xmm7, [esi + eax - 16];
                movdqa [ecx + eax - 16], xmm7;
            jmp @addforxloop

            @loopEnd:

            sub eax, 128;

            jz @nextLine;

            @addforxloop2:
                movdqa xmm0, [esi + eax];
                movdqa [ecx + eax], xmm0;
            add eax, 16;
            jnz @addforxloop2;

            @nextLine:

            // special care of the last element
            movsd xmm0, [esi];
            movsd [ecx], xmm0;

            // next line:
            add esi, srcLineWidth;
            add ecx, destLineWidth;

        // loop y end
        dec edx;
        jnz @@addforyloop;

        pop esi;
     end;
end;

procedure ASMMatrixCopyUnAlignedOddW(Dest : PDouble; const destLineWidth : TASMNativeInt; src : PDouble; const srcLineWidth : TASMNativeInt; Width, Height : TASMNativeInt);
var iters : TASMNativeInt;
begin
     assert((width > 0) and (height > 0) and (destLineWidth >= width*sizeof(double)) and (srcLineWidth >= width*sizeof(double)), 'Dimension error');

     iters := -(width - 1)*sizeof(double);

     asm
        push esi;

        // helper registers for the mt1, mt2 and dest pointers
        mov esi, src;
        sub esi, iters;
        mov ecx, dest;
        sub ecx, iters;

        // for y := 0 to height - 1:
        mov edx, Height;
        @@addforyloop:
            // for x := 0 to w - 1;
            // prepare for reverse loop
            mov eax, iters;
            @addforxloop:
                add eax, 128;
                jg @loopEnd;

                // addition:
                movdqu xmm0, [esi + eax - 128];
                movdqu [ecx + eax - 128], xmm0;

                movdqu xmm1, [esi + eax - 112];
                movdqu [ecx + eax - 112], xmm1;

                movdqu xmm2, [esi + eax - 96];
                movdqu [ecx + eax - 96], xmm2;

                movdqu xmm3, [esi + eax - 80];
                movdqu [ecx + eax - 80], xmm3;

                movdqu xmm4, [esi + eax - 64];
                movdqu [ecx + eax - 64], xmm4;

                movdqu xmm5, [esi + eax - 48];
                movdqu [ecx + eax - 48], xmm5;

                movdqu xmm6, [esi + eax - 32];
                movdqu [ecx + eax - 32], xmm6;

                movdqu xmm7, [esi + eax - 16];
                movdqu [ecx + eax - 16], xmm7;
            jmp @addforxloop

            @loopEnd:

            sub eax, 128;

            jz @nextLine;

            @addforxloop2:
                movdqu xmm0, [esi + eax];
                movdqu [ecx + eax], xmm0;
            add eax, 16;
            jnz @addforxloop2;

            @nextLine:

            // special care of the last element
            movsd xmm0, [esi];
            movsd [ecx], xmm0;

            // next line:
            add esi, srcLineWidth;
            add ecx, destLineWidth;

        // loop y end
        dec edx;
        jnz @@addforyloop;

        pop esi;
     end;
end;

{$ENDIF}

end.
