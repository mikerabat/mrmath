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

procedure ASMMatrixVectMultT(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);
procedure ASMMatrixVectMult(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);

// routines with special input layouts: LineWidthV needs to be sizeof(double)
procedure ASMMatrixVectMultEvenAlignedVAligned(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);
procedure ASMMatrixVectMultEvenUnAlignedVAligned(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);
procedure ASMMatrixVectMultOddUnAlignedVAligned(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);

// destlinewidth needs to be sizeof(double)!
// no speed gain agains amsmatrixVectMultT
procedure ASMMatrixVectMultTDestVec(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);

{$ENDIF}

implementation

{$IFNDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$ENDIF}

procedure ASMMatrixVectMult(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);
var iter : TASMNativeInt;
begin
     assert(Width > 0, 'Error at least a vector with one elementexpected');

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
        mov edx, LineWidthV;
        mov esi, LineWidthMT;

        // init for y := 0 to Height div 4 - 1 do
        sub height, 4;
        js @@foryloopend;

        @@foryloop:

            // init values: // unrolled loop - 4 times
            xorpd xmm0, xmm0;  // dest^ := 0;
            xorpd xmm1, xmm1;
            xorpd xmm2, xmm2;
            xorpd xmm3, xmm3; 
            mov ebx, v;        // ebx = first vector element
            mov ecx, iter;

            @@forxloop:
                movsd xmm4, [ebx];

                movsd xmm5, [eax];
                mulsd xmm5, xmm4;
                addsd xmm0, xmm5;

                movsd xmm5, [eax + esi];
                mulsd xmm5, xmm4;
                addsd xmm1, xmm5;

                movsd xmm5, [eax + 2*esi];
                mulsd xmm5, xmm4;
                addsd xmm2, xmm5;

                add eax, esi;
                movsd xmm5, [eax + 2*esi];
                mulsd xmm5, xmm4;
                addsd xmm3, xmm5;
                sub eax, esi;

                add ebx, edx;
                add eax, 8;
            add ecx, 8;
            jnz @@forxloop;

            add eax, iter; // undo increment

            // calculate dest = beta*dest + alpha*xmm0
            // -> 4 times since the loop is unrolled 
            movhpd xmm0, [edi];
            mulpd xmm0, xmm6;
            haddpd xmm0, xmm7;
            movsd [edi], xmm0;
            add edi, destLineWidth;
            add eax, LineWidthMT;

            movhpd xmm1, [edi];
            mulpd xmm1, xmm6;
            haddpd xmm1, xmm7;
            movsd [edi], xmm1;
            add edi, destLineWidth;
            add eax, LineWidthMT;

            movhpd xmm2, [edi];
            mulpd xmm2, xmm6;
            haddpd xmm2, xmm7;
            movsd [edi], xmm2;
            add edi, destLineWidth;
            add eax, LineWidthMT;

            movhpd xmm3, [edi];
            mulpd xmm3, xmm6;
            haddpd xmm3, xmm7;
            movsd [edi], xmm3;
            add edi, destLineWidth;
            add eax, LineWidthMT;

        sub height, 4;
        jns @@foryloop;   // jump not signed...

        // ###########################################
        // #### Remaining rows (max 4):
        @@foryloopend:
        
        add height, 4;
        jz @@endmult;

        sub eax, iter;
        @@foryshortloop:

            // init values: // unrolled loop - 4 times
            xorpd xmm0, xmm0;  // dest^ := 0;
            mov ebx, v;        // ebx = first vector element
            mov ecx, iter;
            
            @@forxshortloop:
                movsd xmm4, [ebx];
                mulsd xmm4, [eax + ecx];
                addsd xmm0, xmm4;

                add ebx, edx;
            add ecx, 8;
            jnz @@forxshortloop;

            // calculate dest = beta*dest + alpha*xmm0
            movhpd xmm0, [edi];
            mulpd xmm0, xmm6;
            haddpd xmm0, xmm7;
            movsd [edi], xmm0;

            // next line
            add edi, destLineWidth;
            add eax, LineWidthMT;

        dec height;
        jnz @@foryshortloop;   

        @@endmult:

        // epilog
        pop edi;
        pop esi;
        pop ebx;
        pop edx;
        pop eax;
     end;
end;

procedure ASMMatrixVectMultEvenAlignedVAligned(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);
var iter : TASMNativeInt;
begin
     assert(Width > 1, 'Error at least a vector with two elements expected');
     assert(LineWidthV = sizeof(double), 'Error Vector not in a row');

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
        mov esi, LineWidthMT;

        // init for y := 0 to Height div 4 - 1 do
        sub height, 4;
        js @@foryloopend;

        @@foryloop:

            // init values: // unrolled loop - 4 times
            xorpd xmm0, xmm0;  // dest^ := 0;
            xorpd xmm1, xmm1;
            xorpd xmm2, xmm2;
            xorpd xmm3, xmm3;
            mov ebx, v;        // ebx = first vector element
            mov ecx, iter;

            @@forxloop:
                movapd xmm4, [ebx];

                movapd xmm5, [eax];
                mulpd xmm5, xmm4;
                addpd xmm0, xmm5;

                movapd xmm5, [eax + esi];
                mulpd xmm5, xmm4;
                addpd xmm1, xmm5;

                movapd xmm5, [eax + 2*esi];
                mulpd xmm5, xmm4;
                addpd xmm2, xmm5;

                add eax, esi;
                movapd xmm5, [eax + 2*esi];
                mulpd xmm5, xmm4;
                addpd xmm3, xmm5;
                sub eax, esi;

                add ebx, 16;
                add eax, 16;
            add ecx, 16;
            jnz @@forxloop;

            add eax, iter; // undo increment

            // calculate dest = beta*dest + alpha*xmm0
            // -> 4 times since the loop is unrolled
            movsd xmm5, [edi];
            haddpd xmm0, xmm5;
            mulpd xmm0, xmm6;
            haddpd xmm0, xmm7;
            movsd [edi], xmm0;
            add edi, destLineWidth;
            add eax, LineWidthMT;

            movsd xmm5, [edi];
            haddpd xmm1, xmm5;
            mulpd xmm1, xmm6;
            haddpd xmm1, xmm7;
            movsd [edi], xmm1;
            add edi, destLineWidth;
            add eax, LineWidthMT;

            movsd xmm5, [edi];
            haddpd xmm2, xmm5;
            mulpd xmm2, xmm6;
            haddpd xmm2, xmm7;
            movsd [edi], xmm2;
            add edi, destLineWidth;
            add eax, LineWidthMT;

            movsd xmm5, [edi];
            haddpd xmm3, xmm5;
            mulpd xmm3, xmm6;
            haddpd xmm3, xmm7;
            movsd [edi], xmm3;
            add edi, destLineWidth;
            add eax, LineWidthMT;

        sub height, 4;
        jns @@foryloop;   // jump not signed...

        // ###########################################
        // #### Remaining rows (max 4):
        @@foryloopend:

        add height, 4;
        jz @@endmult;

        sub eax, iter;
        @@foryshortloop:

            // init values:
            xorpd xmm0, xmm0;  // dest^ := 0;
            mov ebx, v;        // ebx = first vector element
            mov ecx, iter;

            @@forxshortloop:
                movapd xmm4, [ebx];
                movapd xmm5, [eax + ecx];
                mulpd xmm5, xmm4;
                addpd xmm0, xmm5;

                add ebx, 16;
            add ecx, 16;
            jnz @@forxshortloop;

            // calculate dest = beta*dest + alpha*xmm0
            movsd xmm5, [edi];
            haddpd xmm0, xmm5;
            mulpd xmm0, xmm6;
            haddpd xmm0, xmm7;
            movsd [edi], xmm0;

            // next line
            add edi, destLineWidth;
            add eax, LineWidthMT;

        dec height;
        jnz @@foryshortloop;   

        @@endmult:

        // epilog
        pop edi;
        pop esi;
        pop ebx;
        pop edx;
        pop eax;
     end;
end;

procedure ASMMatrixVectMultEvenUnAlignedVAligned(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);
var iter : TASMNativeInt;
begin
     assert(Width > 1, 'Error at least a vector with two elements expected');
     assert(LineWidthV = sizeof(double), 'Error Vector not in a row');

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
        mov esi, LineWidthMT;

        // init for y := 0 to Height div 4 - 1 do
        sub height, 4;
        js @@foryloopend;

        @@foryloop:

            // init values: // unrolled loop - 4 times
            xorpd xmm0, xmm0;  // dest^ := 0;
            xorpd xmm1, xmm1;
            xorpd xmm2, xmm2;
            xorpd xmm3, xmm3; 
            mov ebx, v;        // ebx = first vector element
            mov ecx, iter;

            @@forxloop:
                movupd xmm4, [ebx];

                movupd xmm5, [eax];
                mulpd xmm5, xmm4;
                addpd xmm0, xmm5;

                movupd xmm5, [eax + esi];
                mulpd xmm5, xmm4;
                addpd xmm1, xmm5;

                movupd xmm5, [eax + 2*esi];
                mulpd xmm5, xmm4;
                addpd xmm2, xmm5;

                add eax, esi;
                movupd xmm5, [eax + 2*esi];
                mulpd xmm5, xmm4;
                addpd xmm3, xmm5;
                sub eax, esi;

                add ebx, 16;
                add eax, 16;
            add ecx, 16;
            jnz @@forxloop;

            add eax, iter; // undo increment

            // calculate dest = beta*dest + alpha*xmm0
            // -> 4 times since the loop is unrolled 
            movsd xmm5, [edi];
            haddpd xmm0, xmm5;
            mulpd xmm0, xmm6;
            haddpd xmm0, xmm7;
            movsd [edi], xmm0;
            add edi, destLineWidth;
            add eax, LineWidthMT;

            movsd xmm5, [edi];
            haddpd xmm1, xmm5;
            mulpd xmm1, xmm6;
            haddpd xmm1, xmm7;
            movsd [edi], xmm1;
            add edi, destLineWidth;
            add eax, LineWidthMT;

            movsd xmm5, [edi];
            haddpd xmm2, xmm5;
            mulpd xmm2, xmm6;
            haddpd xmm2, xmm7;
            movsd [edi], xmm2;
            add edi, destLineWidth;
            add eax, LineWidthMT;

            movsd xmm5, [edi];
            haddpd xmm3, xmm5;
            mulpd xmm3, xmm6;
            haddpd xmm3, xmm7;
            movsd [edi], xmm3;
            add edi, destLineWidth;
            add eax, LineWidthMT;

        sub height, 4;
        jns @@foryloop;   // jump not signed...

        // ###########################################
        // #### Remaining rows (max 4):
        @@foryloopend:
        
        add height, 4;
        jz @@endmult;

        sub eax, iter;
        @@foryshortloop:

            // init values: 
            xorpd xmm0, xmm0;  // dest^ := 0;
            mov ebx, v;        // ebx = first vector element
            mov ecx, iter;
            
            @@forxshortloop:
                movupd xmm4, [ebx];
                movupd xmm5, [eax + ecx];
                mulpd xmm5, xmm4;
                addpd xmm0, xmm5;
            
                add ebx, 16;
            add ecx, 16;
            jnz @@forxshortloop;

            
            // calculate dest = beta*dest + alpha*xmm0
            movsd xmm5, [edi];
            haddpd xmm0, xmm5;
            mulpd xmm0, xmm6;
            haddpd xmm0, xmm7;
            movsd [edi], xmm0;

            // next line
            add edi, destLineWidth;
            add eax, LineWidthMT;

        dec height;
        jnz @@foryshortloop;   

        @@endmult:

        // epilog
        pop edi;
        pop esi;
        pop ebx;
        pop edx;
        pop eax;
     end;
end;

procedure ASMMatrixVectMultOddUnAlignedVAligned(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);
var iter : TASMNativeInt;
begin
     assert(Width > 1, 'Error at least a vector with two elements expected');
     assert(LineWidthV = sizeof(double), 'Error Vector not in a row');
     assert(width and $01 = 1, 'Odd width expected');

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
        mov esi, LineWidthMT;

        // init for y := 0 to Height div 4 - 1 do
        sub height, 4;
        js @@foryloopend;

        @@foryloop:

            // init values: // unrolled loop - 4 times
            xorpd xmm0, xmm0;  // dest^ := 0;
            xorpd xmm1, xmm1;
            xorpd xmm2, xmm2;
            xorpd xmm3, xmm3; 
            mov ebx, v;        // ebx = first vector element
            mov ecx, iter;

            @@forxloop:
                movupd xmm4, [ebx];

                movupd xmm5, [eax];
                mulpd xmm5, xmm4;
                addpd xmm0, xmm5;

                movupd xmm5, [eax + esi];
                mulpd xmm5, xmm4;
                addpd xmm1, xmm5;

                movupd xmm5, [eax + 2*esi];
                mulpd xmm5, xmm4;
                addpd xmm2, xmm5;

                add eax, esi;
                movupd xmm5, [eax + 2*esi];
                mulpd xmm5, xmm4;
                addpd xmm3, xmm5;
                sub eax, esi;

                add ebx, 16;
                add eax, 16;
            add ecx, 16;
            jnz @@forxloop;

            // last element handling
            movsd xmm4, [ebx];

            movsd xmm5, [eax];
            mulsd xmm5, xmm4;
            addsd xmm0, xmm5;

            movsd xmm5, [eax + esi];
            mulsd xmm5, xmm4;
            addsd xmm1, xmm5;

            movsd xmm5, [eax + 2*esi];
            mulsd xmm5, xmm4;
            addsd xmm2, xmm5;

            add eax, esi;
            movsd xmm5, [eax + 2*esi];
            mulsd xmm5, xmm4;
            addsd xmm3, xmm5;
            sub eax, esi;


            // ###########################################
            // #### Build result
            add eax, iter; // undo increment

            // calculate dest = beta*dest + alpha*xmm0
            // -> 4 times since the loop is unrolled 
            movsd xmm5, [edi];
            haddpd xmm0, xmm5;
            mulpd xmm0, xmm6;
            haddpd xmm0, xmm7;
            movsd [edi], xmm0;
            add edi, destLineWidth;
            add eax, LineWidthMT;

            movsd xmm5, [edi];
            haddpd xmm1, xmm5;
            mulpd xmm1, xmm6;
            haddpd xmm1, xmm7;
            movsd [edi], xmm1;
            add edi, destLineWidth;
            add eax, LineWidthMT;

            movsd xmm5, [edi];
            haddpd xmm2, xmm5;
            mulpd xmm2, xmm6;
            haddpd xmm2, xmm7;
            movsd [edi], xmm2;
            add edi, destLineWidth;
            add eax, LineWidthMT;

            movsd xmm5, [edi];
            haddpd xmm3, xmm5;
            mulpd xmm3, xmm6;
            haddpd xmm3, xmm7;
            movsd [edi], xmm3;
            add edi, destLineWidth;
            add eax, LineWidthMT;

        sub height, 4;
        jns @@foryloop;   // jump not signed...

        // ###########################################
        // #### Remaining rows (max 4):
        @@foryloopend:

        add height, 4;
        jz @@endmult;

        sub eax, iter;
        @@foryshortloop:

            // init values:
            xorpd xmm0, xmm0;  // dest^ := 0;
            mov ebx, v;        // ebx = first vector element
            mov ecx, iter;

            @@forxshortloop:
                movupd xmm4, [ebx];
                movupd xmm5, [eax + ecx];
                mulpd xmm5, xmm4;
                addpd xmm0, xmm5;

                add ebx, 16;
            add ecx, 16;
            jnz @@forxshortloop;

            // handle last element
            movsd xmm4, [ebx];
            movsd xmm5, [eax];
            mulsd xmm5, xmm4;
            addsd xmm0, xmm5;

            // calculate dest = beta*dest + alpha*xmm0
            movsd xmm5, [edi];
            haddpd xmm0, xmm5;
            mulpd xmm0, xmm6;
            haddpd xmm0, xmm7;
            movsd [edi], xmm0;

            // next line
            add edi, destLineWidth;
            add eax, LineWidthMT;

        dec height;
        jnz @@foryshortloop;

        @@endmult:

        // epilog
        pop edi;
        pop esi;
        pop ebx;
        pop edx;
        pop eax;
     end;
end;

// optimized transposed function
// -> does 16 operations in one loop
// (basically an unrolled simd version)
procedure ASMMatrixVectMultT(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);
var res0, res1, res2, res3,
    res4, res5, res6, res7 : Array[0..1] of  double;
begin
     if (width = 0) or (height = 0) then
        exit;
     asm
        push eax;
        push edx;
        push ebx;
        push esi;
        push edi;

        mov edi, dest;

        // for the final multiplication
        movddup xmm6, alpha;
        movddup xmm7, beta;

        // prepare for loop
        mov esi, LineWidthMT;
        mov edx, LineWidthV;

        // ###################################
        // #### unrolled loop (4 times)

        // init for y := 0 to Height div 4 - 1 do

        sub width, 16;
        js @@forxloopend;

        @@forxloop:

            xorpd xmm0, xmm0;  // two elements
            // clear out res
            movupd res0, xmm0;
            movupd res1, xmm0;
            movupd res2, xmm0;
            movupd res3, xmm0;
            movupd res4, xmm0;
            movupd res5, xmm0;
            movupd res6, xmm0;
            movupd res7, xmm0;

            mov ebx, v;        // ebx = first vector element
            mov eax, mt1;

            mov ecx, height;
            @@foryloop:
                movddup xmm3, [ebx];

                movupd xmm4, [eax];
                movupd xmm0, res0;
                mulpd xmm4, xmm3;
                addpd xmm0, xmm4;
                movupd res0, xmm0;

                movupd xmm4, [eax + 16];
                movupd xmm0, res1;
                mulpd xmm4, xmm3;
                addpd xmm0, xmm4;
                movupd res1, xmm0;

                movupd xmm4, [eax + 32];
                movupd xmm0, res2;
                mulpd xmm4, xmm3;
                addpd xmm0, xmm4;
                movupd res2, xmm0;

                movupd xmm4, [eax + 48];
                movupd xmm0, res3;
                mulpd xmm4, xmm3;
                addpd xmm0, xmm4;
                movupd res3, xmm0;

                movupd xmm4, [eax + 64];
                movupd xmm0, res4;
                mulpd xmm4, xmm3;
                addpd xmm0, xmm4;
                movupd res4, xmm0;

                movupd xmm4, [eax + 80];
                movupd xmm0, res5;
                mulpd xmm4, xmm3;
                addpd xmm0, xmm4;
                movupd res5, xmm0;

                movupd xmm4, [eax + 96];
                movupd xmm0, res6;
                mulpd xmm4, xmm3;
                addpd xmm0, xmm4;
                movupd res6, xmm0;

                movupd xmm4, [eax + 112];
                movupd xmm0, res7;
                mulpd xmm4, xmm3;
                addpd xmm0, xmm4;
                movupd res7, xmm0;

                add eax, esi;
                add ebx, edx;

            dec ecx;
            jnz @@foryloop;

            // build result of 16 values:
            mov ebx, destLineWidth;

            // first two
            movsd xmm3, [edi];
            movsd xmm4, [edi + ebx];
            movlhps xmm3, xmm4;

            movupd xmm0, res0;
            mulpd xmm0, xmm6;   // alpha*res
            mulpd xmm3, xmm7;   // dest*beta

            addpd xmm3, xmm0;
            movhlps xmm4, xmm3;
            movsd [edi], xmm3;
            movsd [edi + ebx], xmm4;
            add edi, ebx;
            add edi, ebx;

            // second two
            movsd xmm3, [edi];
            movsd xmm4, [edi + ebx];
            movlhps xmm3, xmm4;

            movupd xmm0, res1;
            mulpd xmm0, xmm6;   // alpha*res
            mulpd xmm3, xmm7;   // dest*beta

            addpd xmm3, xmm0;
            movhlps xmm4, xmm3;
            movsd [edi], xmm3;
            movsd [edi + ebx], xmm4;

            add edi, ebx;
            add edi, ebx;

            // third two
            movsd xmm3, [edi];
            movsd xmm4, [edi + ebx];
            movlhps xmm3, xmm4;

            movupd xmm0, res2;
            mulpd xmm0, xmm6;   // alpha*res
            mulpd xmm3, xmm7;   // dest*beta

            addpd xmm3, xmm0;
            movhlps xmm4, xmm3;
            movsd [edi], xmm3;
            movsd [edi + ebx], xmm4;

            add edi, ebx;
            add edi, ebx;

            // forth two
            movsd xmm3, [edi];
            movsd xmm4, [edi + ebx];
            movlhps xmm3, xmm4;

            movupd xmm0, res3;
            mulpd xmm0, xmm6;   // alpha*res
            mulpd xmm3, xmm7;   // dest*beta

            addpd xmm3, xmm0;
            movhlps xmm4, xmm3;
            movsd [edi], xmm3;
            movsd [edi + ebx], xmm4;

            add edi, ebx;
            add edi, ebx;

            // fith two
            movsd xmm3, [edi];
            movsd xmm4, [edi + ebx];
            movlhps xmm3, xmm4;

            movupd xmm0, res4;
            mulpd xmm0, xmm6;   // alpha*res
            mulpd xmm3, xmm7;   // dest*beta

            addpd xmm3, xmm0;
            movhlps xmm4, xmm3;
            movsd [edi], xmm3;
            movsd [edi + ebx], xmm4;

            add edi, ebx;
            add edi, ebx;

            // sixth two
            movsd xmm3, [edi];
            movsd xmm4, [edi + ebx];
            movlhps xmm3, xmm4;

            movupd xmm0, res5;
            mulpd xmm0, xmm6;   // alpha*res
            mulpd xmm3, xmm7;   // dest*beta

            addpd xmm3, xmm0;
            movhlps xmm4, xmm3;
            movsd [edi], xmm3;
            movsd [edi + ebx], xmm4;

            add edi, ebx;
            add edi, ebx;

            // seventh two
            movsd xmm3, [edi];
            movsd xmm4, [edi + ebx];
            movlhps xmm3, xmm4;

            movupd xmm0, res6;
            mulpd xmm0, xmm6;   // alpha*res
            mulpd xmm3, xmm7;   // dest*beta

            addpd xmm3, xmm0;
            movhlps xmm4, xmm3;
            movsd [edi], xmm3;
            movsd [edi + ebx], xmm4;

            add edi, ebx;
            add edi, ebx;

            // eighth two
            movsd xmm3, [edi];
            movsd xmm4, [edi + ebx];
            movlhps xmm3, xmm4;

            movupd xmm0, res7;
            mulpd xmm0, xmm6;   // alpha*res
            mulpd xmm3, xmm7;   // dest*beta

            addpd xmm3, xmm0;
            movhlps xmm4, xmm3;
            movsd [edi], xmm3;
            movsd [edi + ebx], xmm4;

            add edi, ebx;
            add edi, ebx;

            // next loop element
            add mt1, 16*8; // inc(mt1, 4)
            sub width, 16;

            jns @@forxloop;

        @@forxloopend:

        // ###########################################
        // elements that not fit into mod 32
        add width, 16;
        jz @@vecaddend;

        @@forxshortloop:
            xorpd xmm0, xmm0;  // first two elements

            mov ebx, v;        // ebx = first vector element
            mov eax, mt1;

            mov ecx, height;
            @@forshortyloop:
                movsd xmm3, [ebx];
                movsd xmm4, [eax];

                mulsd xmm4, xmm3;
                addsd xmm0, xmm4;

                add eax, esi;
                add ebx, edx;

            dec ecx;
            jnz @@forshortyloop;

            mulsd xmm0, xmm6;  // alpha*res

            movsd xmm3, [edi];
            mulsd xmm3, xmm7;  //dest*beta
            addsd xmm0, xmm3;
            movsd [edi], xmm0;

            // next row
            add edi, destLineWidth;
            add mt1, 8;

        dec width;
        jnz @@forxshortloop;

        @@vecaddend:

        // ###########################################
        // #### epilog
        pop edi;
        pop esi;
        pop ebx;
        pop edx;
        pop eax;
     end;
end;

{no speed gain}
procedure ASMMatrixVectMultTDestVec(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);
begin
     assert(DestLineWidth = sizeof(double), 'Error desLineWidth needs to be 8');

     if (width = 0) or (height = 0) then
        exit;
     asm
        push eax;
        push edx;
        push ebx;
        push esi;
        push edi;

        mov edi, dest;

        // for the final multiplication
        movddup xmm6, alpha;
        movddup xmm7, beta;

        // prepare for loop
        mov esi, LineWidthMT;
        mov edx, LineWidthV;

        // ###################################
        // #### unrolled loop (4 times)

        // init for y := 0 to Height div 16 - 1 do

        sub width, 16;
        js @@forxloopend;

        @@forxloop:

            // dest*beta
            movupd xmm0, [edi];
            mulpd xmm0, xmm7;
            movupd [edi], xmm0;
            movupd xmm0, [edi + 16];
            mulpd xmm0, xmm7;
            movupd [edi + 16], xmm0;
            movupd xmm0, [edi + 32];
            mulpd xmm0, xmm7;
            movupd [edi + 32], xmm0;
            movupd xmm0, [edi + 48];
            mulpd xmm0, xmm7;
            movupd [edi + 48], xmm0;
            movupd xmm0, [edi + 64];
            mulpd xmm0, xmm7;
            movupd [edi + 64], xmm0;
            movupd xmm0, [edi + 80];
            mulpd xmm0, xmm7;
            movupd [edi + 80], xmm0;
            movupd xmm0, [edi + 96];
            mulpd xmm0, xmm7;
            movupd [edi + 96], xmm0;
            movupd xmm0, [edi + 112];
            mulpd xmm0, xmm7;
            movupd [edi + 112], xmm0;


            mov ebx, v;        // ebx = first vector element
            mov eax, mt1;

            mov ecx, height;
            @@foryloop:
                movddup xmm3, [ebx];
                mulpd xmm3, xmm6;   // alpha

                movupd xmm4, [eax];
                movupd xmm0, [edi];
                mulpd xmm4, xmm3;
                addpd xmm0, xmm4;
                movupd [edi], xmm0;

                movupd xmm4, [eax + 16];
                movupd xmm0, [edi + 16];
                mulpd xmm4, xmm3;
                addpd xmm0, xmm4;
                movupd [edi + 16], xmm0;

                movupd xmm4, [eax + 32];
                movupd xmm0, [edi + 32];
                mulpd xmm4, xmm3;
                addpd xmm0, xmm4;
                movupd [edi + 32], xmm0;

                movupd xmm4, [eax + 48];
                movupd xmm0, [edi + 48];
                mulpd xmm4, xmm3;
                addpd xmm0, xmm4;
                movupd [edi + 48], xmm0;

                movupd xmm4, [eax + 64];
                movupd xmm0, [edi + 64];
                mulpd xmm4, xmm3;
                addpd xmm0, xmm4;
                movupd [edi + 64], xmm0;

                movupd xmm4, [eax + 80];
                movupd xmm0, [edi + 80];
                mulpd xmm4, xmm3;
                addpd xmm0, xmm4;
                movupd [edi + 80], xmm0;

                movupd xmm4, [eax + 96];
                movupd xmm0, [edi + 96];
                mulpd xmm4, xmm3;
                addpd xmm0, xmm4;
                movupd [edi + 96], xmm0;

                movupd xmm4, [eax + 112];
                movupd xmm0, [edi + 112];
                mulpd xmm4, xmm3;
                addpd xmm0, xmm4;
                movupd [edi + 112], xmm0;

                add eax, esi;
                add ebx, edx;

            dec ecx;
            jnz @@foryloop;

            // next loop element
            add edi, 16*8;
            add mt1, 16*8; // inc(mt1, 16)
            sub width, 16;

            jns @@forxloop;

        @@forxloopend:

        // ###########################################
        // elements that not fit into mod 32
        add width, 16;
        jz @@vecaddend;

        @@forxshortloop:
            xorpd xmm0, xmm0;  // clear out accumulator

            mov ebx, v;        // ebx = first vector element
            mov eax, mt1;

            mov ecx, height;
            @@forshortyloop:
                movsd xmm3, [ebx];
                movsd xmm4, [eax];

                mulsd xmm4, xmm3;
                addsd xmm0, xmm4;

                add eax, esi;
                add ebx, edx;

            dec ecx;
            jnz @@forshortyloop;

            mulsd xmm0, xmm6;  // alpha*res

            movsd xmm3, [edi];
            mulsd xmm3, xmm7;  //dest*beta
            addsd xmm0, xmm3;
            movsd [edi], xmm0;

            // next row
            add edi, destLineWidth;
            add mt1, 8;

        dec width;
        jnz @@forxshortloop;

        @@vecaddend:

        // ###########################################
        // #### epilog
        pop edi;
        pop esi;
        pop ebx;
        pop edx;
        pop eax;
     end;
end;

// simple routine.. not used any more
procedure ASMMatrixVectMultT1(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);
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
                movsd xmm1, [eax];
                movsd xmm2, [ebx];

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
