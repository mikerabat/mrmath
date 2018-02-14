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

// Vector/Matrix rotation routines mainly used for the SVD

unit AVXMatrixRotations;

interface

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}
{$IFNDEF x64}


uses MatrixConst;

procedure AVXApplyPlaneRotSeqRVB(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);
procedure AVXApplyPlaneRotSeqRVF(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);
procedure AVXApplyPlaneRotSeqLVF(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);
procedure AVXApplyPlaneRotSeqLVB(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);


procedure AVXMatrixRotate(N : TASMNativeInt; X : PDouble; const LineWidthDX : TASMNativeInt; Y : PDouble; LineWidthDY : TASMNativeInt; const c, s : double);

{$ENDIF}

implementation

{$IFNDEF x64}

{$IFDEF FPC} {$ASMMODE intel} {$ENDIF}

procedure AVXApplyPlaneRotSeqLVB(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);
var y2 : TASMNativeInt;
    iter : TASMNativeInt;
begin
     if (height < 2) or (width < 1) then
        exit;
     y2 := height - 1;
     iter := -(width and $FFFFFFFE)*sizeof(double);
     asm
        push ebx;
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

        lea ecx, cOne;
        vmovsd xmm7, [ecx];

        mov ecx, edx;   // A[y][x]
        sub ecx, LineWidthA;

        vxorpd xmm6, xmm6, xmm6;  // compare reference

        @@foryloop:
           vmovddup xmm0, [eax];  // c[y]
           vmovddup xmm1, [ebx];  // s[y]

           // ###########################################
           // #### if (ctemp <> 1) or (stemp <> 0) then
           vcomisd xmm0, xmm7; // = 1
           jne @@beginXLoop;

           vcomisd xmm1, xmm6; // = 0
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
              vmovupd xmm2, [ecx + edi];
              vmovupd xmm3, [edx + edi];

              // temp store...
              vmovapd xmm4, xmm2
              vmovapd xmm5, xmm3;

              vmulpd xmm3, xmm3, xmm0; // ctemp*pcay1^[x] and ctemp*a[x+1]
              vmulpd xmm2, xmm2, xmm1;  // stemp*pcAy^[x] and stemp*a[x+1]

              vsubpd xmm3, xmm3, xmm2;

              //     pcAy^[x] := stemp*temp + ctemp*pcAy^[x];
              //     pcAy^[x + 1] := stemp*temp1 + ctemp*pcAy^[x + 1]
              vmulpd xmm4, xmm4, xmm0;
              vmulpd xmm5, xmm5, xmm1;

              vaddpd xmm5, xmm5, xmm4;

              // write back...
              vmovupd [ecx + edi], xmm5;
              vmovupd [edx + edi], xmm3;

              add edi, 16;
           jnz @@forxloop;

           @@LastElem:

           // ###########################################
           // #### Last element handling
           mov edi, width;
           and edi, 1;
           jz @@nextLine;

           // same as above but with single elements
           vmovsd xmm2, [ecx];
           vmovsd xmm3, [edx];

           vmovapd xmm4, xmm2;
           vmovapd xmm5, xmm3;

           vmulsd xmm3, xmm3, xmm0;
           vmulsd xmm2, xmm2, xmm1;

           vsubsd xmm3, xmm3, xmm2;

           vmulsd xmm4, xmm4, xmm0;
           vmulsd xmm5, xmm5, xmm1;

           vaddsd xmm5, xmm5, xmm4;

           vmovsd [ecx], xmm5;
           vmovsd [edx], xmm3;

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
        pop ebx;
        vzeroupper;
     end;
end;


procedure AVXApplyPlaneRotSeqLVF(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);
var y2 : TASMNativeInt;
    iter : TASMNativeInt;
begin
     if (height < 2) or (width < 1) then
        exit;
     y2 := height - 1;
     iter := -(width and $FFFFFFFE)*sizeof(double);
     asm
        push ebx;
        push edi;

        mov eax, c;
        mov ebx, s;
        mov ecx, A;     // A[y][x]
        sub ecx, iter;

        lea edx, cOne;
        vmovsd xmm7, [edx];

        mov edx, ecx;   // A[y+1][x]
        add edx, LineWidthA;


        @@foryloop:
           vmovddup xmm0, [eax];  // c[y]
           vmovddup xmm1, [ebx];  // s[y]

           // ###########################################
           // #### if (ctemp <> 1) or (stemp <> 0) then
           vcomisd xmm0, xmm7; // = 1
           jne @@beginXLoop;

           vcomisd xmm1, xmm6; // = 0
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
              vmovupd xmm2, [ecx + edi];
              vmovupd xmm3, [edx + edi];

              vmulpd xmm5, xmm3, xmm0;  // ctemp*pcay1^[x] and ctemp*a[x+1]
              vmulpd xmm4, xmm2, xmm1;  // stemp*pcAy^[x] and stemp*a[x+1]

              //subpd xmm3, xmm2;
              vsubpd xmm5, xmm5, xmm4;

              //     pcAy^[x] := stemp*temp + ctemp*pcAy^[x];
              //     pcAy^[x + 1] := stemp*temp1 + ctemp*pcAy^[x + 1]
              vmulpd xmm2, xmm2, xmm0;
              vmulpd xmm3, xmm3, xmm1;

              //addpd xmm5, xmm4;
              vaddpd xmm3, xmm3, xmm2;

              // write back...
              vmovupd [ecx + edi], xmm3;
              vmovupd [edx + edi], xmm5;

              add edi, 16;
           jnz @@forxloop;

           @@LastElem:

           // ###########################################
           // #### Last element handling
           mov edi, width;
           and edi, 1;
           jz @@nextLine;

           // same as above but with single elements
           vmovsd xmm4, [ecx];
           vmovsd xmm5, [edx];

           vmulsd xmm3, xmm5, xmm0;
           vmulsd xmm2, xmm4, xmm1;

           vsubsd xmm3, xmm3, xmm2;

           vmulsd xmm4, xmm4, xmm0;
           vmulsd xmm5, xmm5, xmm1;

           vaddsd xmm5, xmm5, xmm4;

           vmovsd [ecx], xmm5;
           vmovsd [edx], xmm3;

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
        pop ebx;
     end;
end;

procedure AVXApplyPlaneRotSeqRVB(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);
var w2 : TASMNativeInt;
    iter : TASMNativeInt;
begin
     if (height <= 0) or (width <= 1) then
        exit;

     w2 := width - 1;
     iter := w2*sizeof(double);

     asm
        push ebx;
        push edi;

        mov eax, c;
        mov ebx, s;
        mov ecx, A;

        lea edi, cMulM1Bits;
        vmovupd xmm7, [edi];

        @@foryloop:

           mov edi, iter;
           vmovhpd xmm2, xmm2, [ecx + edi];

           // for x := width - 2 downto 0
           @@forxloop:
              vmovsd xmm4, [eax + edi - 8];  // store c
              vmovsd xmm3, [ebx + edi - 8];  // store s

              vmovlpd xmm2, xmm2, [ecx + edi - 8]; // a[x], a[x+1]

              // handle x, x+1
              // ####################################
              // #### x, x+ 1
              vmovlhps xmm3, xmm3, xmm4;
              vmovlhps xmm4, xmm4, xmm3;

              vxorpd xmm3, xmm3, xmm7;  // -s, c
              vmulpd xmm3, xmm3, xmm2; // a[x+1)*c[x] - s[x]*a[x]
              vhaddpd xmm3, xmm3, xmm3;

              vmulpd xmm4, xmm4, xmm2; // a[x+1]*s[x] + a[x]*c[x]
              vhaddpd xmm4, xmm4, xmm4;

              // write back first two values
              vmovlhps xmm2, xmm2, xmm4;
              vmovsd [ecx + edi], xmm3;

           // next one
           sub edi, 8;
           jnz @@forxloop;

           vmovsd [ecx + edi], xmm4;

           add ecx, LineWidthA;

        dec height;
        jnz @@foryloop;

        // epilog
        vzeroupper;
        pop edi;
        pop ebx;
     end;
end;

procedure AVXApplyPlaneRotSeqRVF(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);
var w2 : TASMNativeInt;
    iter : TASMNativeInt;
begin
     if (height <= 0) or (width <= 1) then
        exit;

     w2 := width - 1;
     iter := -w2*sizeof(double);

     asm
        push ebx;
        push edi;
        push esi;

        mov eax, c;
        mov ebx, s;
        mov ecx, A;

        mov esi, LineWidthA;

        sub eax, iter;
        sub ebx, iter;
        sub ecx, iter;

        lea edi, cMulM1Bits;
        vmovupd xmm7, [edi];

        @@foryloop:

           mov edi, iter;
           movsd xmm2, [ecx + edi];

           @@forxloop:
              vmovsd xmm4, [eax + edi];  // store c
              vmovhpd xmm4, xmm4, [ebx + edi];  // store s

              vshufpd xmm3, xmm4, xmm4, 1;
              vmovhpd xmm2, xmm2, [ecx + edi + 8]; // a[x], a[x+1]

              // handle x, x+1
              // ####################################
              // #### x, x+ 1

              vxorpd xmm3, xmm3, xmm7;  // -s, c
              vmulpd xmm3, xmm3, xmm2; // a[x+1)*c[x] - s[x]*a[x]
              vhaddpd xmm3, xmm3, xmm3;

              vmulpd xmm4, xmm4, xmm2; // a[x+1]*s[x] + a[x]*c[x]
              vhaddpd xmm4, xmm4, xmm4;

              // write back first two values
              vmovsd xmm2, xmm2, xmm3;
              vmovsd [ecx + edi], xmm4;

           // next one
           add edi, 8;
           jnz @@forxloop;

           vmovsd [ecx + edi], xmm2;

           add ecx, esi;

        dec height;
        jnz @@foryloop;

        // epilog
        vzeroupper;
        pop esi;
        pop edi;
        pop ebx;
     end;
end;

// its assumed that Linewidthdx and linewidthdy = sizeof(double)
procedure AVXMatrixRotateAligned(N : TASMNativeInt; X : PDouble;
  Y : PDouble; const c, s : double);
begin
     asm
        push ebx;
        push esi;

        lea ebx, s;
        vmovsd xmm2, [ebx];
        lea eax, cMinusOne;
        vmulsd xmm2, xmm2, [eax];

        vmovddup xmm0, xmm2;
        lea eax, C;
        vmovddup xmm1, [eax];

        vmovddup xmm2, [ebx];

        mov eax, X;
        mov ebx, Y;

        xor esi, esi;

        mov ecx, n;
        sub ecx, 2;
        jl @@exitLoop;

           @@forNloop:
              // do a full load -> intermediate store in xmm5, and xmm6
              vmovupd xmm5, [eax + esi];      // x, x+1
              vmovupd xmm6, [ebx + esi];      // y, y+1

              vmulpd xmm3, xmm5, xmm0;  // x, x+1 * -s
              vmulpd xmm5, xmm5, xmm1;  // x, x+1 * c
              vmulpd xmm4, xmm6, xmm1;  // y, y+1 * c
              vmulpd xmm6, xmm6, xmm2;  // y, y+1 * s

              vaddpd xmm5, xmm5, xmm6;  // c*x + s*y  , c*(x+1) + s*(y+1)
              vaddpd xmm3, xmm3, xmm4;  // -s*x + c*y, -s(x+1) + c*(y+1)

              // write back
              vmovupd [eax + esi], xmm5;
              vmovupd [ebx + esi], xmm3;

              add esi, 16;

           sub ecx, 2;
           jge @@forNloop;

        @@exitLoop:

        // test for an odd N
        add ecx, 2;
        jz @@endProc;

        // handle last element
        vmovsd xmm5, [eax + esi];
        vmovsd xmm6, [ebx + esi];

        //dtemp := c*pX^[i] + s*pY^[i];
        //pY^[i] := - s*pX^[i] + c*pY^[i];
        //px^[i] := dtemp;

        vmulsd xmm3, xmm5, xmm0;  // x * -s
        vmulsd xmm4, xmm6, xmm1;  // y * c
        vmulsd xmm5, xmm5, xmm1;  // x * c
        vmulsd xmm6, xmm6, xmm2;  // y * s

        vaddsd xmm5, xmm5, xmm6;  // c*x + s*y
        vaddsd xmm3, xmm3, xmm4;  // -s*x + c*y

        // write back
        vmovsd [eax + esi], xmm5;
        vmovsd [ebx + esi], xmm3;

        @@endProc:

        vzeroupper;
        pop esi;
        pop ebx;
     end;
end;


procedure AVXMatrixRotateUnaligned(N : TASMNativeInt; X : PDouble; const LineWidthDX : TASMNativeInt;
  Y : PDouble; LineWidthDY : TASMNativeInt; const c, s : double);
begin
     asm
        push ebx;
        push edi;
        push esi;

        lea ebx, S;
        vmovsd xmm2, [ebx];
        lea eax, cMinusOne;
        vmulsd xmm2, xmm2, [eax];

        vmovddup xmm0, xmm2;
        lea eax, C;
        vmovddup xmm1, [eax];

        lea eax, S;
        vmovddup xmm2, [eax];

        mov eax, X;
        mov ebx, Y;

        mov esi, LineWidthDX;
        mov edi, LineWidthDY;

        mov ecx, n;
        sub ecx, 2;
        jl @@exitLoop;

           @@forNloop:
              // do a full load -> intermediate store in xmm5, and xmm6
              vmovlpd xmm5, xmm5, [eax];    // load x, x+1
              vmovhpd xmm5, xmm5, [eax + esi];
              vmovlpd xmm6, xmm6, [ebx];    // load y, y+1
              vmovhpd xmm6, xmm6, [ebx + edi];

              vmulpd xmm3, xmm5, xmm0;  // x, x+1 * -s
              vmulpd xmm5, xmm5, xmm1;  // x, x+1 * c
              vmulpd xmm4, xmm6, xmm1;  // y, y+1 * c
              vmulpd xmm6, xmm6, xmm2;  // y, y+1 * s

              vaddpd xmm5, xmm5, xmm6;  // c*x + s*y  , c*(x+1) + s*(y+1)
              vaddpd xmm3, xmm3, xmm4;  // -s*x + c*y, -s(x+1) + c*(y+1)

              // write back
              vmovlpd [eax], xmm5;
              vmovhpd [eax + esi], xmm5;

              vmovlpd [ebx], xmm3;
              vmovhpd [ebx + edi], xmm3;

              add eax, esi;
              add eax, esi;
              add ebx, edi;
              add ebx, edi;

           sub ecx, 2;
           jge @@forNloop;

        @@exitLoop:

        // test for an odd N
        add ecx, 2;
        jz @@endProc;

        // handle last element
        vmovsd xmm5, [eax];
        vmovsd xmm6, [ebx];

        //dtemp := c*pX^[i] + s*pY^[i];
        //pY^[i] := - s*pX^[i] + c*pY^[i];
        //px^[i] := dtemp;
        vmulsd xmm3, xmm5, xmm0;  // x * -s
        vmulsd xmm5, xmm5, xmm1;  // x * c
        vmulsd xmm4, xmm6, xmm1;  // y * c
        vmulsd xmm6, xmm6, xmm2;  // y * s

        vaddsd xmm5, xmm5, xmm6;  // c*x + s*y
        vaddsd xmm3, xmm3, xmm4;  // -s*x + c*y

        // write back
        vmovsd [eax], xmm5;
        vmovsd [ebx], xmm3;

        @@endProc:
        vzeroupper;
        pop esi;
        pop edi;
        pop ebx;
     end;
end;

procedure AVXMatrixRotate(N : TASMNativeInt; X : PDouble; const LineWidthDX : TASMNativeInt; Y : PDouble; LineWidthDY : TASMNativeInt; const c, s : double);
begin
     if N <= 0 then
        exit;

     if (LineWidthDX = sizeof(double)) and (LineWidthDY = sizeof(double))
     then
         AVXMatrixRotateAligned(N, X, Y, c, s)
     else
         AVXMatrixRotateUnAligned(N, X, LineWidthDX, Y, LineWidthDY, c, s)
end;



{$ENDIF}

end.
