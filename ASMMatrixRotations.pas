// ###################################################################
// #### This file is part of the mathematics library project, and is
// #### offered under the licence agreement described on
// #### http://www.mrsoft.org/
// ####
// #### Copyright:(c) 2017, Michael R. . All rights reserved.
// ####
// #### Unless required by applicable law or agreed to in writing, software
// #### distributed under the License is distributed on an "AS IS" BASIS,
// #### WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// #### See the License for the specific language governing permissions and
// #### limitations under the License.
// ###################################################################

// Vector/Matrix rotation routines mainly used for the SVD

unit ASMMatrixRotations;

interface

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}
{$IFNDEF x64}


uses MatrixConst;

procedure ASMApplyPlaneRotSeqRVB(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);
procedure ASMApplyPlaneRotSeqRVF(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);
procedure ASMApplyPlaneRotSeqLVF(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);
procedure ASMApplyPlaneRotSeqLVB(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);

procedure ASMMatrixRotate(N : TASMNativeInt; X : PDouble; const LineWidthDX : TASMNativeInt; Y : PDouble; LineWidthDY : TASMNativeInt; const c, s : double);

{$ENDIF}

implementation

{$IFNDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$ENDIF}


procedure ASMApplyPlaneRotSeqLVB(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);
var y2 : TASMNativeInt;
    iter : TASMNativeInt;
begin
     if (height < 2) or (width < 1) then
        exit;
     y2 := height - 1;
     iter := -(width and $FFFFFFFE)*sizeof(double);
     asm
        push eax;
        push ebx;
        push ecx;
        push edx;
        push edi;

        mov eax, c;  // point to y (aka the end)
        mov ebx, s;

        mov edi, y2;
        dec edi;
        shl edi, 3;  // y2*sizeof(double)
        add eax, edi;
        add ebx, edi;


        mov edx, A;     // A[y + 1][x]
        mov edi, LineWidthA;
        imul edi, y2;
        add edx, edi;
        sub edx, iter;

        mov ecx, edx;   // A[y][x]
        sub ecx, LineWidthA;

        movsd xmm7, cOne;
        xorpd xmm6, xmm6;  // haddpd zero extend

        @@foryloop:
           movddup xmm0, [eax];  // c[y]
           movddup xmm1, [ebx];  // s[y]

           // ###########################################
           // #### if (ctemp <> 1) or (stemp <> 0) then
           comisd xmm0, xmm7; // = 1
           jne @@beginXLoop;

           comisd xmm1, xmm6; // = 0
           jne @@beginXLoop;

           jmp @@nextLine; // c=1 and stemp=0 next line -> the statement


           // ###########################################
           // #### for x := 0 to width - 1 do
           @@beginXLoop:

           // init
           mov edi, iter;
           test edi, edi;
           jz @@LastElem;

           @@forxloop:
              //temp := pcAy1^[x];
              //     pcAy1^[x] := cTemp*temp - stemp*pcAy^[x];
              //     pcAy1^[x + 1] := cTemp*temp1 - stemp*pcAy1[x + 1];

              // evaluate 2 values
              movupd xmm2, [ecx + edi];
              movupd xmm3, [edx + edi];

              // temp store...
              movapd xmm4, xmm2
              movapd xmm5, xmm3;

              mulpd xmm3, xmm0; // ctemp*pcay1^[x] and ctemp*a[x+1]
              mulpd xmm2, xmm1;  // stemp*pcAy^[x] and stemp*a[x+1]

              subpd xmm3, xmm2;

              //     pcAy^[x] := stemp*temp + ctemp*pcAy^[x];
              //     pcAy^[x + 1] := stemp*temp1 + ctemp*pcAy^[x + 1]
              mulpd xmm4, xmm0;
              mulpd xmm5, xmm1;

              addpd xmm5, xmm4;

              // write back...
              movupd [ecx + edi], xmm5;
              movupd [edx + edi], xmm3;

              add edi, 16;
           jnz @@forxloop;

           @@LastElem:

           // ###########################################
           // #### Last element handling
           mov edi, width;
           and edi, 1;
           jz @@nextLine;

           // same as above but with single elements
           movsd xmm2, [ecx];
           movsd xmm3, [edx];

           movsd xmm4, xmm2;
           movsd xmm5, xmm3;

           mulsd xmm3, xmm0;
           mulsd xmm2, xmm1;

           subsd xmm3, xmm2;

           mulsd xmm4, xmm0;
           mulsd xmm5, xmm1;

           addsd xmm5, xmm4;

           movsd [ecx], xmm5;
           movsd [edx], xmm3;

           // ###########################################
           // #### next y
           @@nextLine:

           sub ebx, 8;   // sizeof(double)
           sub eax, 8;
           sub ecx, LineWidthA;
           sub edx, LineWidthA;
        dec y2;
        jnz @@foryloop;

        pop edi;
        pop edx;
        pop ecx;
        pop ebx;
        pop eax;
     end;
end;


procedure ASMApplyPlaneRotSeqLVF(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);
var y2 : TASMNativeInt;
    iter : TASMNativeInt;
begin
     if (height < 2) or (width < 1) then
        exit;
     y2 := height - 1;
     iter := -(width and $FFFFFFFE)*sizeof(double);
     asm
        push eax;
        push ebx;
        push ecx;
        push edx;
        push edi;

        mov eax, c;
        mov ebx, s;
        mov ecx, A;     // A[y][x]
        sub ecx, iter;

        mov edx, ecx;   // A[y+1][x]
        add edx, LineWidthA;

        movsd xmm7, cOne;
        xorpd xmm6, xmm6;  // haddpd zero extend

        @@foryloop:
           movddup xmm0, [eax];  // c[y]
           movddup xmm1, [ebx];  // s[y]

           // ###########################################
           // #### if (ctemp <> 1) or (stemp <> 0) then
           comisd xmm0, xmm7; // = 1
           jne @@beginXLoop;

           comisd xmm1, xmm6; // = 0
           jne @@beginXLoop;

           jmp @@nextLine; // c=1 and stemp=0 next line -> the statement


           // ###########################################
           // #### for x := 0 to width - 1 do
           @@beginXLoop:

           // init
           mov edi, iter;
           test edi, edi;
           jz @@LastElem;

           @@forxloop:
              //temp := pcAy1^[x];
              //     pcAy1^[x] := cTemp*temp - stemp*pcAy^[x];
              //     pcAy1^[x + 1] := cTemp*temp1 - stemp*pcAy1[x + 1];

              // evaluate 2 values
              movupd xmm2, [ecx + edi];
              movupd xmm3, [edx + edi];

              // temp store...
              movapd xmm4, xmm2
              movapd xmm5, xmm3;

              mulpd xmm3, xmm0; // ctemp*pcay1^[x] and ctemp*a[x+1]
              mulpd xmm2, xmm1;  // stemp*pcAy^[x] and stemp*a[x+1]

              subpd xmm3, xmm2;

              //     pcAy^[x] := stemp*temp + ctemp*pcAy^[x];
              //     pcAy^[x + 1] := stemp*temp1 + ctemp*pcAy^[x + 1]
              mulpd xmm4, xmm0;
              mulpd xmm5, xmm1;

              addpd xmm5, xmm4;

              // write back...
              movupd [ecx + edi], xmm5;
              movupd [edx + edi], xmm3;

              add edi, 16;
           jnz @@forxloop;

           @@LastElem:

           // ###########################################
           // #### Last element handling
           mov edi, width;
           and edi, 1;
           jz @@nextLine;

           // same as above but with single elements
           movsd xmm2, [ecx];
           movsd xmm3, [edx];

           movsd xmm4, xmm2;
           movsd xmm5, xmm3;

           mulsd xmm3, xmm0;
           mulsd xmm2, xmm1;

           subsd xmm3, xmm2;

           mulsd xmm4, xmm0;
           mulsd xmm5, xmm1;

           addsd xmm5, xmm4;

           movsd [ecx], xmm5;
           movsd [edx], xmm3;

           // ###########################################
           // #### next y
           @@nextLine:

           add ebx, 8;   // sizeof(double)
           add eax, 8;
           add ecx, LineWidthA;
           add edx, LineWidthA;
        dec y2;
        jnz @@foryloop;

        pop edi;
        pop edx;
        pop ecx;
        pop ebx;
        pop eax;
     end;
end;

procedure ASMApplyPlaneRotSeqRVB(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);
var w2 : TASMNativeInt;
    iter : TASMNativeInt;
begin
     if (height <= 0) or (width <= 1) then
        exit;

     w2 := width - 1;
     iter := w2*sizeof(double);

     asm
        push eax;
        push ebx;
        push ecx;
        push edi;

        mov eax, c;
        mov ebx, s;
        mov ecx, A;

        movupd xmm7, cMulM1Bits;
        xorpd xmm6, xmm6;  // haddpd zero extend

        @@foryloop:

           mov edi, iter;
           movhpd xmm2, [ecx + edi];

           // for x := width - 2 downto 0
           @@forxloop:
              movsd xmm4, [eax + edi - 8];  // store c
              movsd xmm3, [ebx + edi - 8];  // store s

              movlpd xmm2, [ecx + edi - 8]; // a[x], a[x+1]

              // handle x, x+1
              // ####################################
              // #### x, x+ 1
              movlhps xmm3, xmm4;
              movlhps xmm4, xmm3;

              xorpd xmm3, xmm7;  // -s, c
              mulpd xmm3, xmm2; // a[x+1)*c[x] - s[x]*a[x]
              haddpd xmm3, xmm6;

              mulpd xmm4, xmm2; // a[x+1]*s[x] + a[x]*c[x]
              haddpd xmm4, xmm6;

              // write back first two values
              movlhps xmm2, xmm4;
              movsd [ecx + edi], xmm3;

           // next one
           sub edi, 8;
           jnz @@forxloop;

           movsd [ecx + edi], xmm4;

           add ecx, LineWidthA;

        dec height;
        jnz @@foryloop;

        // epilog
        pop edi;
        pop ecx;
        pop ebx;
        pop eax;
     end;
end;

procedure ASMApplyPlaneRotSeqRVF(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);
var w2 : TASMNativeInt;
    iter : TASMNativeInt;
begin
     if (height <= 0) or (width <= 1) then
        exit;

     w2 := width - 1;
     iter := -w2*sizeof(double);

     asm
        push eax;
        push ebx;
        push ecx;
        push edi;

        mov eax, c;
        mov ebx, s;
        mov ecx, A;

        sub eax, iter;
        sub ebx, iter;
        sub ecx, iter;

        movupd xmm7, cMulM1Bits;
        xorpd xmm6, xmm6;  // haddpd zero extend

        @@foryloop:

           mov edi, iter;
           movsd xmm2, [ecx + edi];

           @@forxloop:
              movsd xmm4, [eax + edi];  // store c
              movsd xmm3, [ebx + edi];  // store s

              movsd xmm0, [ecx + edi + 8]; // a[x], a[x+1]
              movlhps xmm2, xmm0;

              // handle x, x+1
              // ####################################
              // #### x, x+ 1
              movlhps xmm3, xmm4;
              movlhps xmm4, xmm3;

              xorpd xmm3, xmm7;  // -s, c
              mulpd xmm3, xmm2; // a[x+1)*c[x] - s[x]*a[x]
              haddpd xmm3, xmm6;

              mulpd xmm4, xmm2; // a[x+1]*s[x] + a[x]*c[x]
              haddpd xmm4, xmm6;

              // write back first two values
              movsd xmm2, xmm3;
              movsd [ecx + edi], xmm4;

           // next one
           add edi, 8;
           jnz @@forxloop;

           movsd [ecx + edi], xmm2;

           add ecx, LineWidthA;

        dec height;
        jnz @@foryloop;

        // epilog
        pop edi;
        pop ecx;
        pop ebx;
        pop eax;
     end;
end;

// its assumed that Linewidthdx and linewidthdy = sizeof(double)
procedure ASMMatrixRotateAligned(N : TASMNativeInt; X : PDouble;
  Y : PDouble; const c, s : double);
begin
     asm
        push eax;
        push ebx;
        push ecx;
        push esi;

        movsd xmm2, s;
        mulsd xmm2, cMinusOne;

        movddup xmm0, xmm2;
        movddup xmm1, c;

        movddup xmm2, s;

        mov eax, X;
        mov ebx, Y;

        xor esi, esi;

        mov ecx, n;
        shr ecx, 1;
        test ecx, ecx;
        jz @@exitLoop;

           @@forNloop:
              // do a full load -> intermediate store in xmm5, and xmm6
              movupd xmm5, [eax + esi];      // x, x+1
              movupd xmm6, [ebx + esi];      // y, y+1

              movapd xmm3, xmm5;
              movapd xmm4, xmm6;

              mulpd xmm3, xmm0;  // x, x+1 * -s
              mulpd xmm5, xmm1;  // x, x+1 * c
              mulpd xmm6, xmm2;  // y, y+1 * s
              mulpd xmm4, xmm1;  // y, y+1 * c

              addpd xmm5, xmm6;  // c*x + s*y  , c*(x+1) + s*(y+1)
              addpd xmm3, xmm4;  // -s*x + c*y, -s(x+1) + c*(y+1)

              // write back
              movupd [eax + esi], xmm5;
              movupd [ebx + esi], xmm3;

              add esi, 16;

           dec ecx;
           jnz @@forNloop;

        @@exitLoop:

        // test for an odd N
        mov ecx, N
        and ecx, 1;
        jz @@endProc;

        // handle last element
        movsd xmm5, [eax + esi];
        movsd xmm6, [ebx + esi];

        //dtemp := c*pX^[i] + s*pY^[i];
        //pY^[i] := - s*pX^[i] + c*pY^[i];
        //px^[i] := dtemp;
        movsd xmm3, xmm5;
        movsd xmm4, xmm6;

        mulsd xmm3, xmm0;  // x * -s
        mulsd xmm5, xmm1;  // x * c
        mulsd xmm6, xmm2;  // y * s
        mulsd xmm4, xmm1;  // y * c

        addsd xmm5, xmm6;  // c*x + s*y
        addsd xmm3, xmm4;  // -s*x + c*y

        // write back
        movsd [eax + esi], xmm5;
        movsd [ebx + esi], xmm3;

        @@endProc:

        pop esi;
        pop ecx;
        pop ebx;
        pop eax;
     end;
end;


procedure ASMMatrixRotateUnaligned(N : TASMNativeInt; X : PDouble; const LineWidthDX : TASMNativeInt;
  Y : PDouble; LineWidthDY : TASMNativeInt; const c, s : double);
begin
     asm
        push eax;
        push ebx;
        push ecx;
        push edx;
        push edi;
        push esi;

        movsd xmm2, s;
        mulsd xmm2, cMinusOne;

        movddup xmm0, xmm2;
        movddup xmm1, c;

        movddup xmm2, s;

        mov eax, X;
        mov ebx, Y;

        mov esi, LineWidthDX;
        mov edi, LineWidthDY;

        mov ecx, n;
        shr ecx, 1;
        test ecx, ecx;
        jz @@exitLoop;

           @@forNloop:
              // do a full load -> intermediate store in xmm5, and xmm6
              movsd xmm5, [eax];    // load x, x+1
              movsd xmm3, [eax + esi];
              movsd xmm6, [ebx];    // load y, y+1
              movsd xmm4, [ebx + edi];

              movlhps xmm5, xmm3;
              movlhps xmm6, xmm4;

              movapd xmm3, xmm5;
              movapd xmm4, xmm6;

              mulpd xmm3, xmm0;  // x, x+1 * -s
              mulpd xmm5, xmm1;  // x, x+1 * c
              mulpd xmm6, xmm2;  // y, y+1 * s
              mulpd xmm4, xmm1;  // y, y+1 * c

              addpd xmm5, xmm6;  // c*x + s*y  , c*(x+1) + s*(y+1)
              addpd xmm3, xmm4;  // -s*x + c*y, -s(x+1) + c*(y+1)


              // write back
              movhlps xmm6, xmm5;
              movhlps xmm4, xmm3;

              movsd [eax], xmm5;
              movsd [eax + esi], xmm6;

              movsd [ebx], xmm3;
              movsd [ebx + edi], xmm4;

              add eax, esi;
              add eax, esi;
              add ebx, edi;
              add ebx, edi;

           dec ecx;
           jnz @@forNloop;

        @@exitLoop:

        // test for an odd N
        mov ecx, N
        and ecx, 1;
        jz @@endProc;

        // handle last element
        movsd xmm5, [eax];
        movsd xmm6, [ebx];

        //dtemp := c*pX^[i] + s*pY^[i];
        //pY^[i] := - s*pX^[i] + c*pY^[i];
        //px^[i] := dtemp;
        movsd xmm3, xmm5;
        movsd xmm4, xmm6;

        mulsd xmm3, xmm0;  // x * -s
        mulsd xmm5, xmm1;  // x * c
        mulsd xmm6, xmm2;  // y * s
        mulsd xmm4, xmm1;  // y * c

        addsd xmm5, xmm6;  // c*x + s*y
        addsd xmm3, xmm4;  // -s*x + c*y

        // write back
        movsd [eax], xmm5;
        movsd [ebx], xmm3;

        @@endProc:

        pop esi;
        pop ecx;
        pop ebx;
        pop eax;
     end;
end;

procedure ASMMatrixRotate(N : TASMNativeInt; X : PDouble; const LineWidthDX : TASMNativeInt; Y : PDouble; LineWidthDY : TASMNativeInt; const c, s : double);
begin
     if N <= 0 then
        exit;
        
     if (LineWidthDX = sizeof(double)) and (LineWidthDY = sizeof(double))
     then
         ASMMatrixRotateAligned(N, X, Y, c, s)
     else
         ASMMatrixRotateUnAligned(N, X, LineWidthDX, Y, LineWidthDY, c, s)
end;


{$ENDIF}

end.
