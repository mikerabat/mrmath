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

{$IFNDEF CPUX64}

uses ASMConsts;

procedure ASMMatrixVectorMultAlignedEvenW1(dest : PDouble; const destLineWidth : TaSMNativeInt; mt1, mt2 : PDouble; width1 : TaSMNativeInt; height1 : TaSMNativeInt; height2 : TaSMNativeInt; const LineWidth1 : TaSMNativeInt);
procedure ASMMatrixVectorMultUnAlignedEvenW1(dest : PDouble; const destLineWidth : TaSMNativeInt; mt1, mt2 : PDouble; width1 : TaSMNativeInt; height1 : TaSMNativeInt; height2 : TaSMNativeInt; const LineWidth1 : TaSMNativeInt);

procedure ASMMatrixVectorMultAlignedOddW1(dest : PDouble; const destLineWidth : TaSMNativeInt; mt1, mt2 : PDouble; width1 : TaSMNativeInt; height1 : TaSMNativeInt; height2 : TaSMNativeInt; const LineWidth1 : TaSMNativeInt);
procedure ASMMatrixVectorMultUnAlignedOddW1(dest : PDouble; const destLineWidth : TaSMNativeInt; mt1, mt2 : PDouble; width1 : TaSMNativeInt; height1 : TaSMNativeInt; height2 : TaSMNativeInt; const LineWidth1 : TaSMNativeInt);

{$ENDIF}

implementation

{$IFNDEF CPUX64}

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
{$ENDIF}

end.
