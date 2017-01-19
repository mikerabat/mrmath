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

unit MatrixRotations;

interface

uses MatrixConst;

// original dlasr from lapack
procedure ApplyPlaneRotSeqRVB(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);
procedure ApplyPlaneRotSeqRVF(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);
procedure ApplyPlaneRotSeqRTF(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);
procedure ApplyPlaneRotSeqRTB(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);

procedure ApplyPlaneRotSeqLVB(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);
procedure ApplyPlaneRotSeqLVF(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);

procedure MatrixRotate(N : TASMNativeInt; DX : PDouble; const LineWidthDX : TASMNativeInt; DY : PDouble; LineWidthDY : TASMNativeInt; const c, s : double);

procedure GenPlaneRotation(F, G : double; var CS, SN, R : double);


implementation

uses Math;

// generates a plane rotation with real cosine and real sine
// original dlartg in lapack
procedure GenPlaneRotation(F, G : double; var CS, SN, R : double);
var f1, g1, scale : double;
    count : integer;
    i: Integer;
const cSaveMin : double = 6.7180e-138; // sqrt(100*MinDouble)/cDoubleEpsilon
      cSaveMax : double = 1.4885e+137; // 1/cSaveMin;
begin
     if g = 0 then
     begin
          cs := 1;
          sn := 0;
          r := f;
     end
     else if f = 0 then
     begin
          cs := 0;
          sn := 1;
          r := g;
     end
     else
     begin
          f1 := f;
          g1 := g;
          scale := max( abs(f1), abs(g1));
          if scale >= cSaveMax then
          begin
               count := 0;
               repeat
                     inc(count);
                     f1 := f1*cSaveMin;
                     g1 := g1*cSaveMin;

                     scale := max( abs(f1 ), abs( g1 ) );
               until scale >= cSaveMax;

               r := sqrt( sqr(f1) + sqr(g1) );
               cs := f1/r;
               sn := g1/r;

               for i := 0 to count - 1 do
                   r := r*cSaveMax;
          end
          else if scale <= cSaveMin then
          begin
               count := 0;
               repeat
                     inc(count);
                     f1 := f1*cSaveMax;
                     g1 := g1*cSaveMax;

                     scale := max( abs(f1 ), abs( g1 ) );
               until scale <= cSaveMin;

               r := sqrt( sqr(f1) + sqr(g1) );
               cs := f1/r;
               sn := g1/r;
               for i := 0 to count - 1 do
                   r := r*cSaveMin;
          end
          else
          begin
               // normal path
               r := sqrt( sqr(f1) + sqr(g1) );
               cs := f1/r;
               sn := g1/r;
          end;

          if (abs( f ) > abs( g )) and (cs < 0) then
          begin
               cs := -cs;
               sn := -sn;
               r := -r;
          end;
     end;
end;

procedure ApplyPlaneRotSeqLVF(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);
var cTemp, stemp, temp : double;
    x, y : TASMNativeInt;
    pcAy, pcAy1 : PConstDoubleArr;
begin
     for y := 0 to height - 2 do
     begin
          ctemp := c[y];
          stemp := s[y];

          if (ctemp <> 1) or (stemp <> 0) then
          begin
               pcAy := PConstDoubleArr(GenPtr(A, 0, y, LineWidthA));
               pcAy1 := PConstDoubleArr(GenPtr(A, 0, y + 1, LineWidthA));
               for x := 0 to width - 1 do
               begin
                    temp := pcAy1^[x];
                    pcAy1^[x] := cTemp*temp - stemp*pcAy^[x];
                    pcAy^[x] := stemp*temp + ctemp*pcAy^[x];
               end;
          end;
     end;
end;

procedure ApplyPlaneRotSeqLVB(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);
var cTemp, stemp, temp : double;
    x, y : TASMNativeInt;
    pcAy, pcAy1 : PConstDoubleArr;
begin
     for y := height - 2 downto 0 do
     begin
          ctemp := c[y];
          stemp := s[y];

          if (ctemp <> 1) or (stemp <> 0) then
          begin
               pcAy := PConstDoubleArr(GenPtr(A, 0, y, LineWidthA));
               pcAy1 := PConstDoubleArr(GenPtr(A, 0, y + 1, LineWidthA));
               for x := 0 to width - 1 do
               begin
                    temp := pcAy1^[x];
                    pcAy1^[x] := cTemp*temp - stemp*pcAy^[x];
                    pcAy^[x] := stemp*temp + ctemp*pcAy^[x];
               end;
          end;
     end;
end;

{
procedure dlasr_LTF(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);
var cTemp, stemp, temp : double;
    x, y : TASMNativeInt;
    pcAy, pcAy1 : PConstDoubleArr;
begin
     for y := 1 to height - 1 do
     begin
          ctemp := c[y - 1];
          stemp := s[y - 1];

          if (ctemp <> 1) or (stemp <> 0) then
          begin
               pcAy := PConstDoubleArr(GenPtr(A, 0, y, LineWidthA));
               pcAy1 := PConstDoubleArr(GenPtr(A, 0, 0, LineWidthA));
               for x := 0 to width - 1 do
               begin
                    temp := pcAy^[x];
                    pcAy^[x] := cTemp*temp - stemp*pcAy1^[x];
                    pcAy1^[x] := stemp*temp + ctemp*pcAy1^[x];
               end;
          end;
     end;
end;

procedure dlasr_LTB(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);
var cTemp, stemp, temp : double;
    x, y : TASMNativeInt;
    pcAy, pcAy1 : PConstDoubleArr;
begin
     for y := height - 1 downto 1 do
     begin
          ctemp := c[y - 1];
          stemp := s[y - 1];
          if (ctemp <> 1) or (stemp <> 0) then
          begin
               pcAy := PConstDoubleArr(GenPtr(A, 0, y, LineWidthA));
               pcAy1 := PConstDoubleArr(GenPtr(A, 0, 0, LineWidthA));

               for x := 0 to width - 1 do
               begin
                    temp := pcAy^[x];
                    pcAy^[x] := ctemp*temp - stemp*pcAy1^[x];
                    pcAy1^[x] := stemp*temp + ctemp*pcAy1^[x];
               end;
          end;
     end;
end;

procedure dlasr_LBF(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);
var cTemp, stemp, temp : double;
    x, y : TASMNativeInt;
    pcAy, pcAy1 : PConstDoubleArr;
begin
     for y := 0 to height - 2 do
     begin
          ctemp := c[y];
          stemp := s[y];

          if (ctemp <> 1) or (stemp <> 0) then
          begin
               pcAy := PConstDoubleArr(GenPtr(A, 0, y, LineWidthA));
               pcAy1 := PConstDoubleArr(GenPtr(A, 0, height - 1, LineWidthA));
               for x := 0 to width - 1 do
               begin
                    temp := pcAy^[x];
                    pcAy^[x] := stemp*pcAy1^[x] + ctemp*temp;
                    pcAy1^[x] := ctemp*pcAy1^[x] - stemp*temp;
               end;
          end;
     end;
end;

procedure dlasr_LBB(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);
var cTemp, stemp, temp : double;
    x, y : TASMNativeInt;
    pcAy, pcAy1 : PConstDoubleArr;
begin
     for y := height - 2 downto 0 do
     begin
          ctemp := c[y];
          stemp := s[y];

          if (ctemp <> 1) or (stemp <> 0) then
          begin
               pcAy := PConstDoubleArr(GenPtr(A, 0, y, LineWidthA));
               pcAy1 := PConstDoubleArr(GenPtr(A, 0, height - 1, LineWidthA));
               for x := 0 to width - 1 do
               begin
                    temp := pcAy^[x];
                    pcAy^[x] := stemp*pcAy1^[x] + ctemp*temp;
                    pcAy1^[x] := ctemp*pcAy1^[x] - stemp*temp;
               end;
          end;
     end;
end;
}

procedure ApplyPlaneRotSeqRVF(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);
var temp : double;
    x, y : TASMNativeInt;
    pAx : PConstDoubleArr;
begin
     for y := 0 to Height - 1 do
     begin
          pAx := PConstDoubleArr( GenPtr( A, 0, y, LineWidthA) );

          for x := 0 to width - 2 do
          begin
               temp := pAx^[x + 1];
               pAx^[x + 1] := c^[x]*temp - s^[x]*pAx^[x];
               pAx^[x] := s^[x]*temp + c^[x]*pAx^[x];
          end;
     end;
end;

procedure ApplyPlaneRotSeqRVB(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);
var temp : double;
    x, y : TASMNativeInt;
    pAx : PConstDoubleArr;
begin
     for y := 0 to Height - 1 do
     begin
          pAx := PConstDoubleArr( GenPtr( A, 0, y, LineWidthA) );

          for x := width - 2 downto 0 do
          begin
               temp := pAx^[x + 1];
               pAx^[x + 1] := c^[x]*temp - s^[x]*pAx^[x];
               pAx^[x] := s^[x]*temp + c^[x]*pAx^[x];
          end;
     end;
end;

procedure ApplyPlaneRotSeqRTF(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);
var cTemp, stemp, temp : double;
    x, y : TASMNativeInt;
    pAy, pAy1 : PDouble;
begin
     for y := 1 to width - 1 do
     begin
          ctemp := c[y - 1];
          stemp := s[y - 1];

          if (ctemp <> 1) or (stemp <> 0) then
          begin
               pAy := GenPtr(A, y, 0, LineWidthA);
               pAy1 := GenPtr(A, 0, 0, LineWidthA);

               for x := 0 to height - 1 do
               begin
                    temp := pAy^;
                    pAy^ := ctemp*temp - stemp*pAy1^;
                    pAy1^ := stemp*temp + ctemp*pAy1^;

                    inc(PByte(pAy), LineWidthA);
                    inc(PByte(pAy1), LineWidthA);
               end;
          end;
     end;
end;

procedure ApplyPlaneRotSeqRTB(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);
var cTemp, stemp, temp : double;
    x, y : TASMNativeInt;
    pAy, pAy1 : PDouble;
begin
     for y := width - 1 downto 1 do
     begin
          ctemp := c[y - 1];
          stemp := s[y - 1];

          if (ctemp <> 1) or (stemp <> 0) then
          begin
               pAy := GenPtr(A, y, 0, LineWidthA);
               pAy1 := GenPtr(A, 0, 0, LineWidthA);

               for x := 0 to height - 1 do
               begin
                    temp := pAy^;
                    pAy^ := ctemp*temp - stemp*pAy1^;
                    pAy1^ := stemp*temp + ctemp*pAy1^;

                    inc(PByte(pAy), LineWidthA);
                    inc(PByte(pAy1), LineWidthA);
               end;
          end;
     end;
end;

procedure dlasr_RBF(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);
var cTemp, stemp, temp : double;
    x, y : TASMNativeInt;
    pAy, pAy1 : PDouble;
begin
     for y := 0 to width - 2 do
     begin
          ctemp := c[y];
          stemp := s[y];

          if (ctemp <> 1) or (stemp <> 0) then
          begin
               pAy := GenPtr(A, y, 0, LineWidthA);
               pAy1 := GenPtr(A, width - 1, 0, LineWidthA);

               for x := 0 to height - 1 do
               begin
                    temp := pAy^;
                    pAy^ := stemp*pAy1^ + ctemp*temp;
                    pAy1^ := ctemp*pAy1^ - stemp*temp;

                    inc(PByte(pAy), LineWidthA);
                    inc(PByte(pAy1), LineWidthA);
               end;
          end;
     end;
end;

procedure dlasr_RBB(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);
var cTemp, stemp, temp : double;
    x, y : TASMNativeInt;
    pAy, pAy1 : PDouble;
begin
     for y := width - 2 downto 0 do
     begin
          ctemp := c[y];
          stemp := s[y];

          if (ctemp <> 1) or (stemp <> 0) then
          begin
               pAy := GenPtr(A, y, 0, LineWidthA);
               pAy1 := GenPtr(A, width - 1, 0, LineWidthA);

               for x := 0 to height - 1 do
               begin
                    temp := pAy^;
                    pAy^ := stemp*pAy1^ + ctemp*temp;
                    pAy1^ := ctemp*pAy1^ - stemp*temp;

                    inc(PByte(pAy), LineWidthA);
                    inc(PByte(pAy1), LineWidthA);
               end;
          end;
     end;
end;

// apply a plane rotation. orig drot in lapack
procedure MatrixRotate(N : TASMNativeInt; DX : PDouble; const LineWidthDX : TASMNativeInt; DY : PDouble; LineWidthDY : TASMNativeInt; const c, s : double);
var i: Integer;
    pX : PConstDoubleArr;
    pY : PConstDoubleArr;
    dtemp : double;
begin
     if n <= 0 then
        exit;

     // faster code if it's in i row...
     if (LineWidthDX = sizeof(double)) and (LineWidthDY = sizeof(double)) then
     begin
          pX := PConstDoubleArr(DX);
          pY := PConstDoubleArr(DY);
          for i := 0 to n - 1 do
          begin
               dtemp := c*pX[i] + s*pY[i];
               pY[i] := c*pY[i] - s*pX[i];
               px[i] := dtemp;
          end;
     end
     else
     begin
          for i := 0 to n - 1 do
          begin
               dtemp := c*dx^ + s*dy^;
               dy^ := c*dy^ - s*dx^;
               dx^ := dtemp;

               inc(PByte(dx), LineWidthDX);
               inc(PByte(dy), LineWidthDY);
          end;
     end;
end;


end.
