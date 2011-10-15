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

{$IFNDEF CPUX64}

procedure ASMMatrixCopyAlignedEvenW(Dest : PDouble; const destLineWidth : integer; src : PDouble; const srcLineWidth : integer; Width, Height : integer);
procedure ASMMatrixCopyUnAlignedEvenW(Dest : PDouble; const destLineWidth : integer; src : PDouble; const srcLineWidth : integer; Width, Height : integer);

procedure ASMMatrixCopyAlignedOddW(Dest : PDouble; const destLineWidth : integer; src : PDouble; const srcLineWidth : integer; Width, Height : integer);
procedure ASMMatrixCopyUnAlignedOddW(Dest : PDouble; const destLineWidth : integer; src : PDouble; const srcLineWidth : integer; Width, Height : integer);

{$ENDIF}

implementation

{$IFNDEF CPUX64}

procedure ASMMatrixCopyAlignedEvenW(Dest : PDouble; const destLineWidth : integer; src : PDouble; const srcLineWidth : integer; Width, Height : integer);
var iters : integer;
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

                // prefetch data...
                prefetch [esi + eax];
                prefetchw [ecx + eax];

                // move:
                movdqa xmm0, [esi + eax - 128];
                movntdq [ecx + eax - 128], xmm0;

                movdqa xmm1, [esi + eax - 112];
                movntdq [ecx + eax - 112], xmm1;

                movdqa xmm2, [esi + eax - 96];
                movntdq [ecx + eax - 96], xmm2;

                movdqa xmm3, [esi + eax - 80];
                movntdq [ecx + eax - 80], xmm3;

                movdqa xmm4, [esi + eax - 64];
                movntdq [ecx + eax - 64], xmm4;

                movdqa xmm5, [esi + eax - 48];
                movntdq [ecx + eax - 48], xmm5;

                movdqa xmm6, [esi + eax - 32];
                movntdq [ecx + eax - 32], xmm6;

                movdqa xmm7, [esi + eax - 16];
                movntdq [ecx + eax - 16], xmm7;
            jmp @addforxloop

            @loopEnd:

            sub eax, 128;

            jz @nextLine;

            @addforxloop2:
                movdqa xmm0, [esi + eax];
                movntdq [ecx + eax], xmm0;
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

procedure ASMMatrixCopyUnAlignedEvenW(Dest : PDouble; const destLineWidth : integer; src : PDouble; const srcLineWidth : integer; Width, Height : integer);
var iters : integer;
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

procedure ASMMatrixCopyAlignedOddW(Dest : PDouble; const destLineWidth : integer; src : PDouble; const srcLineWidth : integer; Width, Height : integer);
var iters : integer;
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
                prefetch [esi + eax];
                prefetchw [ecx + eax];

                // addition:
                movdqa xmm0, [esi + eax - 128];
                movntdq [ecx + eax - 128], xmm0;

                movdqa xmm1, [esi + eax - 112];
                movntdq [ecx + eax - 112], xmm1;

                movdqa xmm2, [esi + eax - 96];
                movntdq [ecx + eax - 96], xmm2;

                movdqa xmm3, [esi + eax - 80];
                movntdq [ecx + eax - 80], xmm3;

                movdqa xmm4, [esi + eax - 64];
                movntdq [ecx + eax - 64], xmm4;

                movdqa xmm5, [esi + eax - 48];
                movntdq [ecx + eax - 48], xmm5;

                movdqa xmm6, [esi + eax - 32];
                movntdq [ecx + eax - 32], xmm6;

                movdqa xmm7, [esi + eax - 16];
                movntdq [ecx + eax - 16], xmm7;
            jmp @addforxloop

            @loopEnd:

            sub eax, 128;

            jz @nextLine;

            @addforxloop2:
                movdqa xmm0, [esi + eax];
                movntdq [ecx + eax], xmm0;
            add eax, 16;
            jnz @addforxloop2;

            @nextLine:

            // special care of the last element
            movlpd xmm0, [esi];
            movlpd [ecx], xmm0;

            // next line:
            add esi, srcLineWidth;
            add ecx, destLineWidth;

        // loop y end
        dec edx;
        jnz @@addforyloop;

        pop esi;
     end;
end;

procedure ASMMatrixCopyUnAlignedOddW(Dest : PDouble; const destLineWidth : integer; src : PDouble; const srcLineWidth : integer; Width, Height : integer);
var iters : integer;
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
            movlpd xmm0, [esi];
            movlpd [ecx], xmm0;

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
