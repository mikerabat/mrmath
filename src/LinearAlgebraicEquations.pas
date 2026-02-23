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


unit LinearAlgebraicEquations;

// ############################################
// #### Functions to solve linear algebraic equations
// ############################################

interface

uses MatrixConst;

// solves the matrix A*X = B where A*x1=b1, A*x2=b2, ... A*xm=bm
// The function stores in A the inverse of A and B stores the result vectors
// A must be a square matrix (width*width) and B must be m*width.
function MatrixGaussJordanInPlace(A : PDouble; const LineWidthA : NativeInt; B : PDouble; const LineWidthB : NativeInt; width : NativeInt;
                                  m : NativeInt; const epsilon : double = 1e-20; progress : TLinEquProgress = nil) : TLinEquResult;

function MatrixGaussJordan(A : PDouble; const LineWidthA : NativeInt; B : PDouble; const LineWidthB : NativeInt;
                           invA : PDouble; const LineWidthInvA : NativeInt; X : PDouble; const LineWidthX : NativeInt;
                           width : NativeInt; m : NativeInt; const epsilon : double = 1e-20; progress : TLinEquProgress = nil) : TLinEquResult;

implementation

uses
  MathUtilFunc;

function MatrixGaussJordan(A : PDouble; const LineWidthA : NativeInt; B : PDouble; const LineWidthB : NativeInt;
                           invA : PDouble; const LineWidthInvA : NativeInt; X : PDouble; const LineWidthX : NativeInt;
                           width : NativeInt; m : NativeInt; const epsilon : double; progress : TLinEquProgress) : TLinEquResult;
var i: NativeInt;
    pInvA : PDouble;
    PX : PDouble;
begin
     Assert(width > 0, 'Dimension Error');
     Assert(lineWidthInvA >= width*sizeof(double), 'Dimension error');
     Assert(lineWidthX >= sizeof(double), 'Dimension error');

     // copy data -> now we can perform an inline gauss elimination procedure
     PInvA := invA;
     PX := X;
     for i := 0 to width - 1 do
     begin
          Move(A^, PInvA^, sizeof(double)*width);
          inc(PByte(PInvA), LineWidthInvA);
          inc(PByte(A), LineWidthA);
          Move(B^, PX^, sizeof(double)*m);
          inc(PByte(B), LineWidthB);
          inc(PByte(PX), LineWidthX);
     end;

     Result := MatrixGaussJordaninPlace(invA, lineWidthInvA, X, LineWidthX, width, m, epsilon, progress);
end;

function MatrixGaussJordanInPlace(A : PDouble; const LineWidthA : NativeInt; B : PDouble; const LineWidthB : NativeInt;
  width : NativeInt; m : NativeInt; const epsilon : double; progress : TLinEquProgress) : TLinEquResult;
var i, icol, irow, j, k, l, ll : NativeInt;
    big, dum, pivinv : double;
    indxc, indxr, ipiv : Array of integer;
    pVal1 : PDouble;
    pVal2 : PDouble;
begin
     assert(LineWidthA >= width*sizeof(double), 'Dimension error');
     assert(LineWidthB >= m*sizeof(double), 'Dimension error');

     Result := leOk;

     SetLength(indxc, width);
     SetLength(indxr, width);
     SetLength(ipiv, width);

     icol := 0;
     irow := 0;

     if Assigned(progress) then
        progress(0);

     for j := 0 to width - 1 do
         ipiv[j] := 0;

     // main loop over the columns to be reduced
     for i := 0 to width - 1 do
     begin
          big := 0;
          for j := 0 to width - 1 do
          begin
               if ipiv[j] <> 1 then
               begin
                    for k := 0 to width - 1 do
                    begin
                         if ipiv[k] = 0 then
                         begin
                              pVal1 := PDouble(NativeUint(A) + NativeUint(j*LineWidthA));
                              inc(pVal1, k);

                              if abs(pVal1^) >= big then
                              begin
                                   big := abs(pVal1^);
                                   irow := j;
                                   icol := k;
                              end;
                         end
                         else if ipiv[k] > 1 then
                         begin
                              Result := leSingular;
                              exit;
                         end;
                    end;
               end;
          end;

          inc(ipiv[icol]);

          // we now have the pivot element, so we interchange rows, if needed, to put the pivot
          // element on the dagonal.

          if irow <> icol then
          begin
               pVal1 := PDouble(NativeUint(A) + NativeUint(irow*LineWidthA));
               pVal2 := PDouble(NativeUint(A) + NativeUint(icol*LineWidthA));
               for l := 0 to width - 1 do
               begin

                    DoubleSwap(pVal1^, pVal2^);
                    inc(pVal1);
                    inc(pVal2);
               end;

               pVal1 := PDouble(NativeUint(B) + NativeUint(irow*LineWidthB));
               pVal2 := PDouble(NativeUint(B) + NativeUint(icol*LineWidthB));
               for l := 0 to m - 1 do
               begin
                    DoubleSwap(pVal1^, pVal2^);
                    inc(pVal1);
                    inc(pVal2);
               end;
          end;

          // we are now ready to divide the pivot row by the pivot element, located in irow and icol
          indxr[i] := irow;
          indxc[i] := icol;

          pVal1 := PDouble(NativeUint(A) + NativeUint(icol*LineWidthA));
          inc(pVal1, icol);

          if abs(pVal1^) < epsilon then
          begin
               Result := leSingular;
               exit;
          end;

          pivinv := 1/pVal1^;

          pVal1^ := 1;
          pVal1 := PDouble(NativeUint(A) + NativeUint(icol*LineWidthA));
          for l := 0 to width - 1 do
          begin
               pVal1^ := pVal1^*pivinv;
               inc(pVal1);
          end;

          pVal1 := PDouble(NativeUint(B) + NativeUint(icol*LineWidthB));
          for l := 0 to m - 1 do
          begin
               pVal1^ := Pivinv*pVal1^;
               inc(pVal1);
          end;

          for ll := 0 to width - 1 do
          begin
               if ll <> icol then
               begin
                    pVal1 := PDouble(NativeUint(A) + NativeUint(ll*LineWidthA));
                    inc(pVal1, icol);
                    dum := pVal1^;
                    pVal1^ := 0;

                    pVal1 := PDouble(NativeUint(A) + NativeUint(ll*LineWidthA));
                    pVal2 := PDouble(NativeUint(A) + NativeUint(icol*LineWidthA));
                    for l := 0 to width - 1 do
                    begin
                         pVal1^ := pVal1^ - pVal2^*dum;
                         inc(pVal1);
                         inc(pVal2);
                    end;

                    pVal1 := PDouble(NativeUint(B) + NativeUint(ll*LineWidthB));
                    pVal2 := PDouble(NativeUint(B) + NativeUint(icol*LineWidthB));
                    for l := 0 to m - 1 do
                    begin
                         pVal1^ := pVal1^ - pVal2^*dum;
                         inc(pVal1);
                         inc(pVal2);
                    end;
               end;
          end;

          if Assigned(progress) then
             progress(100*i div width);
     end;

     for l := width - 1 downto 0 do
     begin
          if indxr[l] <> indxc[l] then
          begin
               for k := 0 to width - 1 do
               begin
                    pVal1 := PDouble(NativeUint(A) + NativeUint(k*LineWidthA));
                    pVal2 := pVal1;
                    inc(pval1, indxr[l]);
                    inc(pval2, indxc[l]);

                    DoubleSwap(pVal1^, pVal2^);
               end;
          end;
     end;

     if Assigned(progress) then
        progress(100);
end;

end.
