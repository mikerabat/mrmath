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

unit ASMMatrixCumSumDiffOperations;

interface


{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}
{$IFNDEF x64}

uses MatrixConst;

procedure ASMMatrixCumulativeSumRow(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);

procedure ASMMatrixCumulativeSumColumnEvenWUnaligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
procedure ASMMatrixCumulativeSumColumnOddWUnaligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
procedure ASMMatrixCumulativeSumColumnEvenWAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
procedure ASMMatrixCumulativeSumColumnOddWAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);

procedure ASMMatrixDifferentiateRow(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);

procedure ASMMatrixDifferentiateColumnEvenWUnaligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
procedure ASMMatrixDifferentiateColumnOddWUnaligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
procedure ASMMatrixDifferentiateColumnEvenWAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
procedure ASMMatrixDifferentiateColumnOddWAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);

{$ENDIF}

implementation

{$IFNDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$ENDIF}

// ##################################################
// #### Cumulative sum
// ##################################################

procedure ASMMatrixCumulativeSumRow(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
begin
     asm
        push ebx;
        push esi;

        // if (width <= 0) or (height <= 0) then exit;
        mov eax, width;
        cmp eax, 0;
        jle @@exitproc;
        mov esi, height;
        cmp esi, 0;
        jle @@exitproc;

        // iter := -width*sizeof(Double)
        mov ebx, width;
        imul ebx, -8;

        // prepare counters
        mov edx, dest;
        sub edx, ebx;
        mov ecx, src;
        sub ecx, ebx;

        @@foryloop:
           mov eax, ebx;
           xorpd xmm0, xmm0;

           @@forxloop:
              addsd xmm0, [ecx + eax];
              movsd [edx + eax], xmm0;
           add eax, 8;
           jnz @@forxloop;

           add ecx, srcLineWidth;
           add edx, destLineWidth;
        dec esi;
        jnz @@foryloop;


        @@exitProc:
        pop esi;
        pop ebx;
     end;
end;

procedure ASMMatrixCumulativeSumColumnEvenWUnaligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
begin
     asm
        push ebx;
        push edi;
        push esi;

        // if (width <= 0) or (height <= 0) then exit;
        mov eax, height;
        cmp eax, 0;
        jle @@exitproc;
        mov esi, width;
        cmp esi, 1;
        jle @@exitproc;

        sar esi, 1;  // width div 2
        mov width, esi;

        // prepare counters
        mov edx, dest;
        mov ecx, src;
        mov ebx, srcLineWidth;

        @@forxloop:
           mov eax, height;
           xorpd xmm0, xmm0;
           xor edi, edi;
           xor esi, esi;

           // two values at once
           @@foryloop:
              movupd xmm1, [ecx + edi];
              addpd xmm0, xmm1;
              movupd [edx + esi], xmm0;

              add edi, ebx;
              add esi, destLineWidth;
           dec eax;
           jnz @@foryloop;

           add ecx, 16;
           add edx, 16;
        dec width;
        jnz @@forxloop;

        @@exitProc:
        pop esi;
        pop edi;
        pop ebx;
     end;
end;

procedure ASMMatrixCumulativeSumColumnOddWUnaligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
begin
     asm
        push ebx;
        push edi;
        push esi;

        // if (width <= 0) or (height <= 0) then exit;
        mov eax, height;
        cmp eax, 0;
        jle @@exitproc;
        mov esi, width;
        cmp esi, 0;
        jle @@exitproc;

        sar esi, 1;  // width div 2
        mov width, esi;

        // prepare counters
        mov edx, dest;
        mov ecx, src;
        mov ebx, srcLineWidth;

        mov esi, width;
        test esi, esi;
        jz @@lastColumn;

        @@forxloop:
           mov eax, height;
           xorpd xmm0, xmm0;
           xor edi, edi;
           xor esi, esi;

           // two values at once
           @@foryloop:
              movupd xmm1, [ecx + edi];
              addpd xmm0, xmm1;
              movupd [edx + esi], xmm0;

              add edi, ebx;
              add esi, destLineWidth;
           dec eax;
           jnz @@foryloop;

           add ecx, 16;
           add edx, 16;
        dec width;
        jnz @@forxloop;

        @@lastColumn:

        mov eax, height;
        xorpd xmm0, xmm0;

        // last column
        @@forycolumnloop:
           movsd xmm1, [ecx];
           addsd xmm0, xmm1;
           movsd [edx], xmm0;

           add ecx, ebx;
           add edx, destLineWidth;
        dec eax;
        jnz @@forycolumnloop;

        @@exitProc:

        pop esi;
        pop edi;
        pop ebx;
     end;
end;

procedure ASMMatrixCumulativeSumColumnEvenWAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
begin
     assert(((TASMNativeUInt(dest) and $F) = 0) and ((destLineWidth and $F) = 0) and
            ((TASMNativeUInt(src) and $F) = 0) and ((srcLineWidth and $F) = 0), 'Non aligned operation');
     asm
        push ebx;
        push edi;
        push esi;

        // if (width <= 0) or (height <= 0) then exit;
        mov eax, height;
        cmp eax, 0;
        jle @@exitproc;
        mov esi, width;
        cmp esi, 1;
        jle @@exitproc;

        sar esi, 1;  // width div 2
        mov width, esi;

        // prepare counters
        mov edx, dest;
        mov ecx, src;
        mov ebx, srcLineWidth;

        @@forxloop:
           mov eax, height;
           xorpd xmm0, xmm0;
           xor edi, edi;
           xor esi, esi;

           // two values at once
           @@foryloop:
              movapd xmm1, [ecx + edi];
              addpd xmm0, xmm1;
              movapd [edx + esi], xmm0;

              add edi, ebx;
              add esi, destLineWidth;
           dec eax;
           jnz @@foryloop;

           add ecx, 16;
           add edx, 16;
        dec width;
        jnz @@forxloop;

        @@exitProc:
        pop esi;
        pop edi;
        pop ebx;
     end;
end;


procedure ASMMatrixCumulativeSumColumnOddWAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
begin
     assert(((TASMNativeUInt(dest) and $F) = 0) and ((destLineWidth and $F) = 0) and
            ((TASMNativeUInt(src) and $F) = 0) and ((srcLineWidth and $F) = 0), 'Non aligned operation');
     asm
        push ebx;
        push edi;
        push esi;

        // if (width <= 0) or (height <= 0) then exit;
        mov eax, height;
        cmp eax, 0;
        jle @@exitproc;
        mov esi, width;
        cmp esi, 0;
        jle @@exitproc;

        sar esi, 1;  // width div 2
        mov width, esi;

        // prepare counters
        mov edx, dest;
        mov ecx, src;
        mov ebx, srcLineWidth;

        mov esi, width;
        test esi, esi;
        jz @@lastColumn;

        @@forxloop:
           mov eax, height;
           xorpd xmm0, xmm0;
           xor edi, edi;
           xor esi, esi;

           // two values at once
           @@foryloop:
              movapd xmm1, [ecx + edi];
              addpd xmm0, xmm1;
              movapd [edx + esi], xmm0;

              add edi, ebx;
              add esi, destLineWidth;
           dec eax;
           jnz @@foryloop;

           add ecx, 16;
           add edx, 16;
        dec width;
        jnz @@forxloop;

        @@lastColumn:

        mov eax, height;
        xorpd xmm0, xmm0;

        // last column
        @@forycolumnloop:
           movsd xmm1, [ecx];
           addsd xmm0, xmm1;
           movsd [edx], xmm0;

           add ecx, ebx;
           add edx, destLineWidth;
        dec eax;
        jnz @@forycolumnloop;

        @@exitProc:

        pop esi;
        pop edi;
        pop ebx;
     end;
end;

procedure ASMMatrixDifferentiateRow(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
begin
     asm
        push ebx;
        push esi;

        // if (width <= 1) or (height <= 0) then exit;
        mov eax, width;
        cmp eax, 1;
        jle @@exitproc;
        mov esi, height;
        cmp esi, 0;
        jle @@exitproc;

        // iter := -width*sizeof(Double)
        mov ebx, width;
        imul ebx, -8;

        // prepare counters
        mov edx, dest;
        sub edx, ebx;
        mov ecx, src;
        sub ecx, ebx;
        add ebx, 8;

        @@foryloop:
           mov eax, ebx;
           movsd xmm1, [ecx + eax - 8];

           @@forxloop:
              movsd xmm0, [ecx + eax];
              movsd xmm2, xmm0;
              subsd xmm1, xmm0;
              movsd [edx + eax - 8], xmm1;

              movsd xmm1, xmm2;
           add eax, 8;
           jnz @@forxloop;

           add ecx, srcLineWidth;
           add edx, destLineWidth;
        dec esi;
        jnz @@foryloop;


        @@exitProc:
        pop esi;
        pop ebx;
     end;
end;

procedure ASMMatrixDifferentiateColumnEvenWUnaligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
begin
     asm
        push ebx;
        push edi;
        push esi;

        // if (width <= 0) or (height <= 1) then exit;
        mov eax, height;
        dec eax;
        cmp eax, 0;
        jle @@exitproc;
        mov esi, width;
        cmp esi, 1;
        jle @@exitproc;

        mov height, eax;

        sar esi, 1;  // width div 2
        mov width, esi;

        // prepare counters
        mov edx, dest;
        mov ecx, src;
        mov ebx, srcLineWidth;

        @@forxloop:
           mov eax, height;
           xorpd xmm0, xmm0;
           mov edi, ebx;
           xor esi, esi;

           movupd xmm0, [ecx];

           // two values at once
           @@foryloop:
              movupd xmm1, [ecx + edi];
              movapd xmm2, xmm1;
              subpd xmm1, xmm0;
              movupd [edx + esi], xmm1;

              movapd xmm0, xmm2;
              add edi, ebx;
              add esi, destLineWidth;
           dec eax;
           jnz @@foryloop;

           add ecx, 16;
           add edx, 16;
        dec width;
        jnz @@forxloop;

        @@exitProc:
        pop esi;
        pop edi;
        pop ebx;
     end;
end;


procedure ASMMatrixDifferentiateColumnOddWUnaligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
begin
     asm
        push ebx;
        push edi;
        push esi;

        // if (width <= 0) or (height <= 1) then exit;
        mov eax, height;
        dec eax;
        cmp eax, 0;
        jle @@exitproc;
        mov esi, width;
        cmp esi, 0;
        jle @@exitproc;

        mov height, eax;

        // prepare counters
        mov edx, dest;
        mov ecx, src;
        mov ebx, srcLineWidth;

        sar esi, 1;  // width div 2
        cmp esi, 0;
        je @@lastcolumn;
        mov width, esi;

        @@forxloop:
           mov eax, height;
           xorpd xmm0, xmm0;
           mov edi, ebx;
           xor esi, esi;

           movupd xmm0, [ecx];

           // two values at once
           @@foryloop:
              movupd xmm1, [ecx + edi];
              movapd xmm2, xmm1;
              subpd xmm1, xmm0;
              movupd [edx + esi], xmm1;

              movapd xmm0, xmm2;
              add edi, ebx;
              add esi, destLineWidth;
           dec eax;
           jnz @@foryloop;

           add ecx, 16;
           add edx, 16;
        dec width;
        jnz @@forxloop;

        @@lastcolumn:

        mov eax, height;
        xorpd xmm0, xmm0;
        mov edi, ebx;
        xor esi, esi;

        movsd xmm0, [ecx];

        // two values at once
        @@foryloop1:
           movsd xmm1, [ecx + edi];
           movapd xmm2, xmm1;
           subsd xmm1, xmm0;
           movsd [edx + esi], xmm1;

           movapd xmm0, xmm2;
           add edi, ebx;
           add esi, destLineWidth;
        dec eax;
        jnz @@foryloop1;

        @@exitProc:
        pop esi;
        pop edi;
        pop ebx;
     end;
end;

procedure ASMMatrixDifferentiateColumnEvenWAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
begin
     asm
        push ebx;
        push edi;
        push esi;

        // if (width <= 0) or (height <= 1) then exit;
        mov eax, height;
        dec eax;
        cmp eax, 0;
        jle @@exitproc;
        mov esi, width;
        cmp esi, 1;
        jle @@exitproc;

        mov height, eax;

        sar esi, 1;  // width div 2
        mov width, esi;

        // prepare counters
        mov edx, dest;
        mov ecx, src;
        mov ebx, srcLineWidth;

        @@forxloop:
           mov eax, height;
           xorpd xmm0, xmm0;
           mov edi, ebx;
           xor esi, esi;

           movapd xmm0, [ecx];

           // two values at once
           @@foryloop:
              movapd xmm1, [ecx + edi];
              movapd xmm2, xmm1;
              subpd xmm1, xmm0;
              movapd [edx + esi], xmm1;

              movapd xmm0, xmm2;
              add edi, ebx;
              add esi, destLineWidth;
           dec eax;
           jnz @@foryloop;

           add ecx, 16;
           add edx, 16;
        dec width;
        jnz @@forxloop;

        @@exitProc:
        pop esi;
        pop edi;
        pop ebx;
     end;
end;


procedure ASMMatrixDifferentiateColumnOddWAligned(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
begin
     asm
        push ebx;
        push edi;
        push esi;

        // if (width <= 0) or (height <= 1) then exit;
        mov eax, height;
        dec eax;
        cmp eax, 0;
        jle @@exitproc;
        mov esi, width;
        cmp esi, 0;
        jle @@exitproc;

        mov height, eax;

        // prepare counters
        mov edx, dest;
        mov ecx, src;
        mov ebx, srcLineWidth;

        sar esi, 1;  // width div 2
        cmp esi, 0;
        je @@lastcolumn;
        mov width, esi;

        @@forxloop:
           mov eax, height;
           xorpd xmm0, xmm0;
           mov edi, ebx;
           xor esi, esi;

           movapd xmm0, [ecx];

           // two values at once
           @@foryloop:
              movapd xmm1, [ecx + edi];
              movapd xmm2, xmm1;
              subpd xmm1, xmm0;
              movapd [edx + esi], xmm1;

              movapd xmm0, xmm2;
              add edi, ebx;
              add esi, destLineWidth;
           dec eax;
           jnz @@foryloop;

           add ecx, 16;
           add edx, 16;
        dec width;
        jnz @@forxloop;

        @@lastcolumn:

        mov eax, height;
        xorpd xmm0, xmm0;
        mov edi, ebx;
        xor esi, esi;

        movsd xmm0, [ecx];

        // two values at once
        @@foryloop1:
           movsd xmm1, [ecx + edi];
           movapd xmm2, xmm1;
           subsd xmm1, xmm0;
           movsd [edx + esi], xmm1;

           movapd xmm0, xmm2;
           add edi, ebx;
           add esi, destLineWidth;
        dec eax;
        jnz @@foryloop1;

        @@exitProc:
        pop esi;
        pop edi;
        pop ebx;
     end;
end;

{$ENDIF}

end.
