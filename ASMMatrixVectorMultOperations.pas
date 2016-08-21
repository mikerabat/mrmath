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


unit ASMMatrixVectorMultOperations;

// #######################################################
// #### special routines for matrix vector multiplications.
// #######################################################

interface

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}
{$IFNDEF x64}

uses MatrixConst;

// used in the matrx multiplication routine
// note these functions assume to have a vector in row major stored!
procedure ASMMatrixVectorMultAlignedEvenW1(dest : PDouble; const destLineWidth : TaSMNativeInt; mt1, mt2 : PDouble; width1 : TaSMNativeInt; height1 : TaSMNativeInt; height2 : TaSMNativeInt; const LineWidth1 : TaSMNativeInt);
procedure ASMMatrixVectorMultUnAlignedEvenW1(dest : PDouble; const destLineWidth : TaSMNativeInt; mt1, mt2 : PDouble; width1 : TaSMNativeInt; height1 : TaSMNativeInt; height2 : TaSMNativeInt; const LineWidth1 : TaSMNativeInt);

procedure ASMMatrixVectorMultAlignedOddW1(dest : PDouble; const destLineWidth : TaSMNativeInt; mt1, mt2 : PDouble; width1 : TaSMNativeInt; height1 : TaSMNativeInt; height2 : TaSMNativeInt; const LineWidth1 : TaSMNativeInt);
procedure ASMMatrixVectorMultUnAlignedOddW1(dest : PDouble; const destLineWidth : TaSMNativeInt; mt1, mt2 : PDouble; width1 : TaSMNativeInt; height1 : TaSMNativeInt; height2 : TaSMNativeInt; const LineWidth1 : TaSMNativeInt);

// assembler function for the matrix to vector multiplication with additional operation
// LineWidthV is not sizeof(double) --> special care for loading values!
procedure ASMMatrixVectMultAlignedOddW(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);
procedure ASMMatrixVectMultUnalignedOddW(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);
procedure ASMMatrixVectMultAlignedEvenW(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);
procedure ASMMatrixVectMultUnalignedEvenW(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);

procedure ASMMatrixVectMultT(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);

{$ENDIF}

implementation

{$IFNDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$ENDIF}
procedure ASMMatrixVectorMultAlignedOddW1(dest : PDouble; const destLineWidth : TaSMNativeInt; mt1, mt2 : PDouble; width1 : TaSMNativeInt; height1 : TaSMNativeInt; height2 : TaSMNativeInt; const LineWidth1 : TaSMNativeInt);
var iter : TaSMNativeInt;
begin
     assert(((Cardinal(dest) and $0000000F) = 0) and ((Cardinal(mt1) and $0000000F) = 0) and ((Cardinal(mt2) and $0000000F) = 0), 'Error aligned operations cannot be performed');
     assert(((Cardinal(LineWidth1) and $0000000F) = 0), 'Error cannot perform aligned operations');

     assert((width1 and $00000001) = 1, 'Error odd width 1 expected');
     assert(width1 = height2, 'Dimension error');
     assert(height1 > 0, 'Dimension error');

     iter := -(width1 - 1)*sizeof(double);

     asm
        push ebx;
        push edi;

        xorpd xmm7, xmm7;
        mov edi, dest;

        // prepare for "Reverse loop indexing"
        mov ebx, mt2;
        sub ebx, iter;

        mov eax, mt1;
        sub eax, iter;

        // init for y := 0 to height1 - 1:
        mov edx, height1;
        @@foryloop:

            // init values:
            xorpd xmm0, xmm0;  // dest^ := 0;

            mov ecx, iter;
            @@forxloop:
                movapd xmm1, [eax + ecx];
                mulpd xmm1, [ebx + ecx];

                addpd xmm0, xmm1;
            add ecx, 16;
            jnz @@forxloop;

            // special treatment for the last value:
            movlpd xmm1, [eax];
            movlpd xmm2, [ebx];

            mulsd xmm1, xmm2;
            addsd xmm0, xmm1;

            // write back result (final addition and compactation)
            haddpd xmm0, xmm7;
            movlpd [edi], xmm0;

            // next results:
            add edi, destLineWidth;
            add eax, LineWidth1;

        dec edx;
        jnz @@foryloop;

        pop edi;
        pop ebx;
     end;
end;

procedure ASMMatrixVectorMultUnAlignedOddW1(dest : PDouble; const destLineWidth : TaSMNativeInt; mt1, mt2 : PDouble; width1 : TaSMNativeInt; height1 : TaSMNativeInt; height2 : TaSMNativeInt; const LineWidth1 : TaSMNativeInt);
var iter : TaSMNativeInt;
begin
     assert((width1 and $00000001) = 1, 'Error odd width 1 expected');
     assert(width1 = height2, 'Dimension error');
     assert(height1 > 0, 'Dimension error');

     iter := -(width1 - 1)*sizeof(double);

     asm
        push ebx;
        push edi;

        xorpd xmm7, xmm7;
        mov edi, dest;

        // prepare for "Reverse loop indexing"
        mov ebx, mt2;
        sub ebx, iter;

        mov eax, mt1;
        sub eax, iter;
        mov mt1, eax;

        // init for y := 0 to height1 - 1:
        mov edx, height1;

        @@foryloop:

            // init values:
            xorpd xmm0, xmm0;  // dest^ := 0;

            mov ecx, iter;
            @@forxloop:
                movupd xmm1, [eax + ecx];
                movupd xmm2, [ebx + ecx];

                mulpd xmm1, xmm2;
                addpd xmm0, xmm1;
            add ecx, 16;
            jnz @@forxloop;

            // special treatment for the last value:
            movlpd xmm1, [eax];
            movlpd xmm2, [ebx];

            mulsd xmm1, xmm2;
            addsd xmm0, xmm1;

            // write back result (final addition and compactation)
            haddpd xmm0, xmm7;
            movlpd [edi], xmm0;

            // next results:
            add edi, destLineWidth;
            add eax, LineWidth1;

        dec edx;
        jnz @@foryloop;

        pop edi;
        pop ebx;
     end;
end;

procedure ASMMatrixVectorMultAlignedEvenW1(dest : PDouble; const destLineWidth : TaSMNativeInt; mt1, mt2 : PDouble; width1 : TaSMNativeInt; height1 : TaSMNativeInt; height2 : TaSMNativeInt; const LineWidth1 : TaSMNativeInt);
var iter : TaSMNativeInt;
begin
     assert(((Cardinal(dest) and $0000000F) = 0) and ((Cardinal(mt1) and $0000000F) = 0) and ((Cardinal(mt2) and $0000000F) = 0), 'Error aligned operations cannot be performed');
     assert(((Cardinal(LineWidth1) and $0000000F) = 0), 'Error aligned operations cannot be performed');

     assert((width1 and $00000001) = 0, 'Error even width 1 expected');
     assert(width1 = height2, 'Dimension error');
     assert(height1 > 0, 'Dimension error');

     iter := -width1*sizeof(double);

     asm
        push ebx;
        push edi;

        xorpd xmm7, xmm7;
        mov edi, dest;

        // prepare for reverse loop indexing
        mov ebx, mt2;
        sub ebx, iter;

        mov eax, mt1;
        sub eax, iter;

        // init for y := 0 to height1 - 1:
        mov edx, height1;

        @@foryloop:

            // init values:
            xorpd xmm0, xmm0;  // dest^ := 0;

            mov ecx, iter;
            @@forxloop:
                movapd xmm1, [eax + ecx];
                mulpd xmm1, [ebx + ecx];

                addpd xmm0, xmm1;
            add ecx, 16;
            jnz @@forxloop;

            // write back result (final addition and compactation)
            haddpd xmm0, xmm7;
            movlpd [edi], xmm0;

            // next results:
            add edi, destLineWidth;
            add eax, LineWidth1;

        dec edx;
        jnz @@foryloop;

        pop edi;
        pop ebx;
     end;
end;

procedure ASMMatrixVectorMultUnAlignedEvenW1(dest : PDouble; const destLineWidth : TaSMNativeInt; mt1, mt2 : PDouble; width1 : TaSMNativeInt; height1 : TaSMNativeInt; height2 : TaSMNativeInt; const LineWidth1 : TaSMNativeInt);
var iter : TaSMNativeInt;
begin
     assert((width1 and $00000001) = 0, 'Error even width 1 expected');
     assert(width1 = height2, 'Dimension error');
     assert(height1 > 0, 'Dimension error');

     iter := -width1*sizeof(double);

     asm
        push ebx;
        push edi;

        xorpd xmm7, xmm7;
        mov edi, dest;

        // prepare for reverse loop indexing
        mov ebx, mt2;
        sub ebx, iter;

        mov eax, mt1;
        sub eax, iter;

        // init for y := 0 to height1 - 1:
        mov edx, height1;

        @@foryloop:

            // init values:
            xorpd xmm0, xmm0;  // dest^ := 0;

            mov ecx, iter;
            @@forxloop:
                movupd xmm1, [eax + ecx];
                movupd xmm2, [ebx + ecx];

                mulpd xmm1, xmm2;
                addpd xmm0, xmm1;
            add ecx, 16;
            jnz @@forxloop;

            // write back result (final addition and compactation)
            haddpd xmm0, xmm7;
            movlpd [edi], xmm0;

            // next results:
            add edi, destLineWidth;
            add eax, LineWidth1;

        dec edx;
        jnz @@foryloop;

        pop edi;
        pop ebx;
     end;
end;


procedure ASMMatrixVectMultAlignedOddW(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);
var iter : TASMNativeInt;
begin
     assert(((Cardinal(mt1) and $0000000F) = 0), 'Error aligned operations cannot be performed');
     assert(((Cardinal(LineWidthMT) and $0000000F) = 0), 'Error cannot perform aligned operations');
     assert( (width and $01) = 1, 'Odd width expected');
     assert(Width > 1, 'Error at least a vector with two elements expected');

     iter := -(width - 1)*sizeof(double);

     asm
        push eax;
        push edx;
        push ebx;
        push esi;
        push edi;

        xorpd xmm7, xmm7;
        mov edi, dest;

        // for the final multiplication
        movhpd xmm6, beta;
        movlpd xmm6, alpha;

        // prepare for "Reverse loop indexing"

        mov eax, mt1;
        sub eax, iter;

        mov edx, LineWidthV;

        // init for y := 0 to height - 1:
        mov esi, height;
        @@foryloop:

            // init values:
            xorpd xmm0, xmm0;  // dest^ := 0;
            mov ebx, v;        // ebx = first vector element

            mov ecx, iter;

            @@forxloop:
                movlpd xmm1, [ebx]
                movhpd xmm1, [ebx + edx];

                mulpd xmm1, [eax + ecx];
                addpd xmm0, xmm1;
            add ebx, edx;
            add ebx, edx;

            add ecx, 16;
            jnz @@forxloop;

            // special treatment for the last value:
            movlpd xmm1, [eax];
            movlpd xmm2, [ebx];

            mulsd xmm1, xmm2;
            addsd xmm0, xmm1;

            // result building
            // write back result (final addition and compactation)
            haddpd xmm0, xmm7;

            // calculate dest = beta*dest + alpha*xmm0
            movhpd xmm0, [edi];
            mulpd xmm0, xmm6;
            haddpd xmm0, xmm7;

            movlpd [edi], xmm0;

            // next results:
            add edi, destLineWidth;
            add eax, LineWidthMT;

        dec esi;
        jnz @@foryloop;

        // epilog
        pop edi;
        pop esi;
        pop ebx;
        pop edx;
        pop eax;
     end;
end;

procedure ASMMatrixVectMultUnalignedOddW(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);
var iter : TaSMNativeInt;
begin
     assert( (width and $01) = 1, 'Odd width expected');
     assert(Width > 1, 'Error at least a vector with two elements expected');

     iter := -(width - 1)*sizeof(double);

     asm
        push eax;
        push edx;
        push ebx;
        push esi;
        push edi;

        xorpd xmm7, xmm7;
        mov edi, dest;

        // for the final multiplication
        movhpd xmm6, beta;
        movlpd xmm6, alpha;

        // prepare for "Reverse loop indexing"

        mov eax, mt1;
        sub eax, iter;

        mov edx, LineWidthV;

        // init for y := 0 to height - 1:
        mov esi, height;
        @@foryloop:

            // init values:
            xorpd xmm0, xmm0;  // dest^ := 0;
            mov ebx, v;        // ebx = first vector element

            mov ecx, iter;

            @@forxloop:
                movupd xmm1, [eax + ecx];
                movlpd xmm2, [ebx]
                movhpd xmm2, [ebx + edx];

                mulpd xmm1, xmm2;
                addpd xmm0, xmm1;
            add ebx, edx;
            add ebx, edx;

            add ecx, 16;
            jnz @@forxloop;

            // special treatment for the last value:
            movlpd xmm1, [eax];
            movlpd xmm2, [ebx];

            mulsd xmm1, xmm2;
            addsd xmm0, xmm1;

            // result building
            // write back result (final addition and compactation)
            haddpd xmm0, xmm7;

            // calculate dest = beta*dest + alpha*xmm0
            movhpd xmm0, [edi];
            mulpd xmm0, xmm6;
            haddpd xmm0, xmm7;

            movlpd [edi], xmm0;

            // next results:
            add edi, destLineWidth;
            add eax, LineWidthMT;

        dec esi;
        jnz @@foryloop;

        // epilog
        pop edi;
        pop esi;
        pop ebx;
        pop edx;
        pop eax;
     end;
end;

procedure ASMMatrixVectMultUnalignedEvenW(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);
var iter : TaSMNativeInt;
begin
     assert( (width and $01) = 0, 'Even width expected');
     assert(Width > 1, 'Error at least a vector with two elements expected');

     iter := -width*sizeof(double);

     asm
        push eax;
        push edx;
        push ebx;
        push esi;
        push edi;

        xorpd xmm7, xmm7;
        mov edi, dest;

        // for the final multiplication
        movhpd xmm6, beta;
        movlpd xmm6, alpha;

        // prepare for "Reverse loop indexing"

        mov eax, mt1;
        sub eax, iter;

        mov edx, LineWidthV;

        // init for y := 0 to height - 1:
        mov esi, height;
        @@foryloop:

            // init values:
            xorpd xmm0, xmm0;  // res := 0;
            mov ebx, v;        // ebx = first vector element

            mov ecx, iter;

            @@forxloop:
                movupd xmm1, [eax + ecx];
                movlpd xmm2, [ebx]
                movhpd xmm2, [ebx + edx];

                mulpd xmm1, xmm2;
                addpd xmm0, xmm1;
            add ebx, edx;
            add ebx, edx;

            add ecx, 16;
            jnz @@forxloop;

            // result building
            // write back result (final addition and compactation)
            haddpd xmm0, xmm7;

            // calculate dest = beta*dest + alpha*xmm0
            movhpd xmm0, [edi];
            mulpd xmm0, xmm6;
            haddpd xmm0, xmm7;

            movlpd [edi], xmm0;

            // next results:
            add edi, destLineWidth;
            add eax, LineWidthMT;

        dec esi;
        jnz @@foryloop;

        // epilog
        pop edi;
        pop esi;
        pop ebx;
        pop edx;
        pop eax;
     end;
end;

procedure ASMMatrixVectMultAlignedEvenW(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);
var iter : TaSMNativeInt;
begin
     assert( (width and $01) = 0, 'Even width expected');
     assert((Cardinal(mt1) and $0000000F) = 0, 'Error aligned operations cannot be performed');
     assert(((Cardinal(LineWidthMT) and $0000000F) = 0), 'Error cannot perform aligned operations');
     assert(Width > 1, 'Error at least a vector with two elements expected');

     iter := -width*sizeof(double);

     asm
        push eax;
        push edx;
        push ebx;
        push esi;
        push edi;

        xorpd xmm7, xmm7;
        mov edi, dest;

        // for the final multiplication
        movhpd xmm6, beta;
        movlpd xmm6, alpha;

        // prepare for "Reverse loop indexing"

        mov eax, mt1;
        sub eax, iter;

        mov edx, LineWidthV;

        // init for y := 0 to height - 1:
        mov esi, height;
        @@foryloop:

            // init values:
            xorpd xmm0, xmm0;  // dest^ := 0;
            mov ebx, v;        // ebx = first vector element

            mov ecx, iter;

            @@forxloop:
                movlpd xmm1, [ebx]
                movhpd xmm1, [ebx + edx];

                mulpd xmm1, [eax + ecx];
                addpd xmm0, xmm1;
            add ebx, edx;
            add ebx, edx;

            add ecx, 16;
            jnz @@forxloop;

            // result building
            // write back result (final addition and compactation)
            haddpd xmm0, xmm7;

            // calculate dest = beta*dest + alpha*xmm0
            movhpd xmm0, [edi];
            mulpd xmm0, xmm6;
            haddpd xmm0, xmm7;

            movlpd [edi], xmm0;

            // next results:
            add edi, destLineWidth;
            add eax, LineWidthMT;

        dec esi;
        jnz @@foryloop;

        // epilog
        pop edi;
        pop esi;
        pop ebx;
        pop edx;
        pop eax;
     end;
end;


// this function is not that well suited for use of simd instructions...
// so only this version exists
procedure ASMMatrixVectMultT(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);
begin
     asm
        push eax;
        push edx;
        push ebx;
        push esi;
        push edi;

        xorpd xmm7, xmm7;
        mov edi, dest;

        // for the final multiplication
        movhpd xmm6, beta;
        movlpd xmm6, alpha;

        // prepare for loop
        mov esi, LineWidthMT;
        mov edx, LineWidthV;

        // init for x := 0 to width - 1:
        @@forxloop:

            // init values:
            xorpd xmm0, xmm0;  // res := 0;
            mov ebx, v;        // ebx = first vector element
            mov eax, mt1;

            mov ecx, height;
            @@foryloop:
                movlpd xmm1, [eax];
                movlpd xmm2, [ebx];

                mulsd xmm1, xmm2;
                addsd xmm0, xmm1;

                add eax, esi;
                add ebx, edx;

            dec ecx;
            jnz @@foryloop;

            // result building
            // write back result (final addition and compactation)

            // calculate dest = beta*dest + alpha*xmm0
            movhpd xmm0, [edi];

            mulpd xmm0, xmm6;
            haddpd xmm0, xmm7;

            movlpd [edi], xmm0;

            // next results:
            add edi, destLineWidth;
            add mt1, 8;
        dec width;
        jnz @@forxloop;

        // epilog
        pop edi;
        pop esi;
        pop ebx;
        pop edx;
        pop eax;
     end;
end;

{$ENDIF}

end.
