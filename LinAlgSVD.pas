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

// ###################################################################
// #### Implementation of the singular value decomposition
// #### based on numerical recipies and a lapacks dgesvd

unit LinAlgSVD;

interface

uses SysUtils, Types, MatrixConst, OptimizedFuncs, Math, MathUtilFunc, LinearAlgebraicEquations;

// Inplace svd decomposition of a Matrix A
// The output is the computation of A= U*W*V' whereas U is stored in A, and W is a vector 0..Width-1. The matrix V (not V') must be as large as Width*Width!
function MatrixSVDInPlace(A : PDouble; const LineWidthA : TASMNativeInt; width : TASMNativeInt; Height : TASMNativeInt; W : PDouble; const LineWidthW : TASMNativeInt;
                           V : PDouble; const LineWidthV : TASMNativeInt; progress : TLinEquProgress = nil) : TSVDResult;
function MatrixSVD(A : PDouble; const LineWidthA : TASMNativeInt; width : TASMNativeInt; Height : TASMNativeInt;
                   U : PDouble; const LineWidthU : TASMNativeInt; W : PDouble; const LineWidthW : TASMNativeInt;
                   V : PDouble; const LineWidthV : TASMNativeInt; progress : TLinEquProgress = nil) : TSVDResult;

// lapack implementation of the svd
function MatrixSVDInPlace2( A : PDouble; const LineWidthA : TASMNativeInt; width : TASMNativeInt; Height : TASMNativeInt;
                 W : PConstDoubleArr; V : PDouble; const LineWidthV : TASMNativeInt; SVDBlockSize : TASMNativeInt = 32;
                 progress : TLinEquProgress = nil) : TSVDResult;

// threaded version of SVD decomposition
// makes use of threaded versions of matrix multiplication and parallel plane rotations
function ThrMatrixSVDInPlace( A : PDouble; const LineWidthA : TASMNativeInt; width : TASMNativeInt; Height : TASMNativeInt;
                 W : PConstDoubleArr; V : PDouble; const LineWidthV : TASMNativeInt; SVDBlockSize : TASMNativeInt = 32;
                 progress : TLinEquProgress = nil) : TSVDResult;



implementation

uses ASMMatrixOperations, SimpleMatrixOperations, BlockSizeSetup, Classes,
     HouseholderReflectors, MatrixRotations, LinAlgQR,
  ThreadedMatrixOperations, MtxThreadPool;

// ########################################################################
// #### Local definitions
// ########################################################################

type
  TMatrixSingularVecUpd = procedure(u : PDouble; const LineWidthU : TASMNativeInt;
                           vt : PDouble; const LineWidthVT : TASMNativeInt;
                           mm, nru, ncvt : TASMNativeInt;
                           w1, w2,
                           w3, w4 : PConstDoubleArr);
  TMatrixSingularVecRotate = procedure (u : PDouble; const LineWidthU : TASMNativeInt;
                       vt : PDouble; const LineWidthVT : TASMNativeInt;
                       m, nru, ncvt : TASMNativeInt; const cosl, sinl, cosr, sinr : double);

type
  TQRDecompFunc = function (A : PDouble; const LineWidthA : TASMNativeInt; width, height : TASMNativeInt; tau : PDouble; work : PDouble; pnlSize : TASMNativeInt; progress : TLinEquProgress) : TQRResult;
  TQFromQRFunc = procedure (A : PDouble; const LineWidthA : TASMNativeInt; width, height : TASMNativeInt; tau : PDouble; BlockSize : TASMNativeInt; work : PDouble; progress : TLinEquProgress);

// #######################################################
// #### function definitions for the SVD
// (shared with the multithreaded version)
type
  TMtxSVDDecompData = record
    pWorkMem : PByte;
    Progress : TLinEquProgress;
    SVDPnlSize : TASMNativeInt;
    QRPnlSize : TASMNativeInt;
    BlkMltSize : TASMNativeInt;
    SVDMultSize : TASMNativeInt;
    qrDecomp : TQRDecompFunc;
    qFromQRDecomp : TQFromQRFunc;
    LeftQFromQRDecomp : TQFromQRFunc;

    MatrixMultEx : TMatrixBlockedMultfunc;
    MatrixMultT1 : TMatrixBlockedMultfunc;
    MatrixMultT2 : TMatrixBlockedMultfunc;

    MatrixRotateUpdB : TMatrixSingularVecUpd;
    MatrixRotateUpdF : TMatrixSingularVecUpd;
    MatrixRotate : TMatrixSingularVecRotate;
  end;



// ########################################################################
// #### Numerical recipies svd
// ########################################################################

function MatrixSVDInPlace(A : PDouble; const LineWidthA : TASMNativeInt; width : TASMNativeInt; Height : TASMNativeInt; W : PDouble; const LineWidthW : TASMNativeInt;
  V : PDouble; const LineWidthV : TASMNativeInt; progress : TLinEquProgress) : TSVDResult;
const cMaxNumSVDIter = 75;
var flag : boolean;
    i, j, jj, k, l : TASMNativeInt;
    its : TASMNativeInt;
    nm : TASMNativeInt;
    anorm : double;
    c, f, g, h : double;
    s, scale, x, y, z : double;
    rv1 : Array of double;
    pA : PDouble;
    pAi : PDouble;
    pAj : PDouble;
    pV : PDouble;
    pVj : PDouble;
    invScale : double;
    invh : double;
    val : double;
    pWi : PDouble;
begin
     assert(LineWidthA >= width*sizeof(double), 'Dimension error');
     assert(LineWidthV >= width*sizeof(double), 'Dimension error');

     if Assigned(progress) then
        progress(0);

     SetLength(rv1, width);

     g := 0;
     scale := 0;
     anorm := 0;
     l := 1;
     nm := 0;

     Result := srNoConvergence;

     pWi := W;
     // Householder reduction to bidiagonal form
     pAi := A;
     for i := 0 to width - 1 do
     begin
          if Assigned(progress) then
             progress(0 + Round(10*(i/width)));

          l := i + 1;

          rv1[i] := scale*g;
          g := 0;
          s := 0;
          scale := 0;

          if i < height then
          begin
               pA := pAi;
               inc(pA, i);

               for k := i to height - 1 do
               begin
                    scale := scale + abs(pA^);

                    inc(PByte(pA), LineWidthA);
               end;

               if scale <> 0 then
               begin
                    pA := pAi;
                    inc(pA, i);
                    invScale := 1/scale;

                    for k := i to height - 1 do
                    begin
                         pA^ := pA^*invScale;
                         s := s + sqr(pA^);

                         inc(PByte(pA), LineWidthA);
                    end;

                    pA := pAi;
                    inc(pA, i);
                    f := pA^;

                    g := -sign(sqrt(s), f);
                    h := f*g - s;
                    pA^ := f - g;
                    invh := 1/h;

                    for j := l to width - 1 do
                    begin
                         s := 0;
                         pA := pAi;
                         pAj := pA;
                         inc(pA, i);
                         inc(pAj, j);

                         for k := i to height - 1 do
                         begin
                              s := s + pA^*pAj^;
                              inc(PByte(pA), LineWidthA);
                              inc(PByte(pAj), LineWidthA);
                         end;
                         f := s*invh;

                         pA := pAi;
                         pAj := pA;
                         inc(pA, i);
                         inc(pAj, j);
                         for k := i to height - 1 do
                         begin
                              pAj^ := pAj^ + f*pA^;
                              inc(PByte(pAj), LineWidthA);
                              inc(PByte(pA), LineWidthA);
                         end;
                    end;

                    pA := pAi;
                    inc(pA, i);
                    for k := i to height - 1 do
                    begin
                         pA^ := scale*pA^;
                         inc(PByte(pA), LineWidthA);
                    end;
               end; // end if scale
          end; // end if i < height

          pWi^ := scale*g;

          g := 0;
          scale := 0;
          s := 0;

          if (i < width) and (i <> width - 1) then
          begin
               pA := pAi;
               inc(pA, l);

               for k := l to width - 1 do
               begin
                    scale := scale + abs(pA^);

                    inc(pA);
               end;

               if scale <> 0 then
               begin
                    pA := pAi;
                    inc(pA, l);
                    invScale := 1/scale;

                    for k := l to width - 1 do
                    begin
                         pA^ := pA^*invScale;
                         s := s + sqr(pA^);

                         inc(pA);
                    end;

                    pA := pAi;
                    inc(pA, l);
                    f := pA^;

                    g := -sign(sqrt(s), f);
                    h := f*g - s;
                    pA^ := f - g;
                    invh := 1/h;

                    for k := l to width - 1 do
                    begin
                         rv1[k] := pA^*invh;
                         inc(pA);
                    end;

                    for j := l to height - 1 do
                    begin
                         s := 0;
                         pAj := A;
                         inc(pAj, l);
                         pA := pAi;
                         inc(pA, l);
                         inc(PByte(pAj), LineWidthA*j);

                         for k := l to width - 1 do
                         begin
                              s := s + pAj^*pA^;
                              inc(pAj);
                              inc(pA);
                         end;

                         pA := A;
                         inc(PByte(pA), LineWidthA*j);
                         inc(pA, l);
                         for k := l to width - 1 do
                         begin
                              pA^ := pA^ + s*rv1[k];
                              inc(pA);
                         end;
                    end;

                    pA := pAi;
                    inc(pA, l);

                    for k := l to width - 1 do
                    begin
                         pA^ := pA^*scale;
                         inc(pA);
                    end;
               end;
          end;

          anorm := max(anorm, abs(pWi^) + abs(rv1[i]));

          inc(PByte(pWi), LineWidthW);
          inc(PByte(pAi), LineWidthA);
     end;

     // accumulation of right-hand transformations
     pAi := A;
     inc(PByte(pAi), (width - 1)*LineWidthA);
     for i := width - 1 downto 0 do
     begin
          if Assigned(progress) then
             progress(Round(10 + 10*((width - 1 - i)/(width))));

          if i < width - 1 then
          begin
               if g <> 0 then
               begin
                    pV := V;
                    inc(pV, i);
                    inc(PByte(pV), LineWidthV*l);
                    pA := pAi;
                    inc(pA, l);
                    val := pA^;
                    val := 1/(val*g);

                    for j := l to width - 1 do
                    begin
                         pV^ := pA^*val;
                         inc(pA);
                         inc(PByte(pV), LineWidthV);
                    end;

                    for j := l to width - 1 do
                    begin
                         s := 0;

                         pA := pAi;
                         inc(pA, l);
                         pV := V;
                         inc(pV, j);
                         inc(PByte(pV), LineWidthV*l);

                         for k := l to width - 1 do
                         begin
                              s := s + pA^*pV^;
                              inc(pA);
                              inc(PByte(pV), LineWidthV);
                         end;

                         pV := V;
                         inc(pV, i);
                         inc(PByte(pV), LineWidthV*l);
                         pVj := V;
                         inc(pVj, j);
                         inc(PByte(pVj), LineWidthV*l);

                         for k := l to width - 1 do
                         begin
                              pVj^ := pVj^ + s*pV^;
                              inc(PByte(pVj), LineWidthV);
                              inc(PByte(pV), LineWidthV);
                         end;
                    end;
               end;  // end if g <> 0

               pV := V;
               inc(pV, l);
               inc(PByte(pV), LineWidthV*i);
               pVj := V;
               inc(pVj, i);
               inc(PByte(pVj), LineWidthV*l);

               for j := l to width - 1 do
               begin
                    pV^ := 0;
                    pVj^ := 0;
                    inc(pV);
                    inc(PByte(pVj), LineWidthV);
               end;
          end; // end if i < width -1
          pV := V;
          inc(pV, i);
          inc(PByte(pV), LineWidthV*i);

          pV^ := 1;
          g := rv1[i];
          l := i;

          dec(PByte(pAi), LineWidthA);
     end;

     pWi := W;
     inc(PByte(pWi), (min(width, height) - 1)*LineWidthW);
     pAi := A;
     inc(PByte(pAi), (min(width, height) - 1)*LineWidthA);
     // accumulation of left-hand transformations
     for i := min(width, height) - 1 downto 0 do
     begin
          if Assigned(progress) then
             progress(Round(20 + 10*((Min(width, height) - 1 - i)/(Min(width, height)))));

          l := i + 1;
          g := pWi^;

          pA := pAi;
          inc(pA, l);
          for j := l to width - 1 do
          begin
               pA^ := 0;
               inc(pA);
          end;

          if g <> 0 then
          begin
               g := 1/g;

               for j := l to width - 1 do
               begin
                    s := 0;
                    pA := A;
                    inc(pA, i);
                    inc(PByte(pA), l*LineWidthA);
                    pAj := A;
                    inc(pAj, j);
                    inc(PByte(pAj), l*LineWidthA);

                    for k := l to height - 1 do
                    begin
                         s := s + pA^*pAj^;
                         inc(PByte(pA), LineWidthA);
                         inc(PByte(pAj), LineWidthA);
                    end;

                    pA := pAi;
                    inc(pA, i);
                    f := (s/pA^)*g;
                    pAj := pAi;
                    inc(pAj, j);
                    for k := i to height - 1 do
                    begin
                         pAj^ := pAj^ + f*pA^;
                         inc(PByte(pAj), LineWidthA);
                         inc(PByte(pA), LineWidthA);
                    end;
               end;

               pA := pAi;
               inc(pA, i);
               for j := i to height - 1 do
               begin
                    pA^ := pA^*g;
                    inc(PByte(pA), LineWidthA);
               end;
          end
          else
          begin
               pA := pAi;
               inc(pA, i);
               for j := i to height - 1 do
               begin
                    pA^ := 0;
                    inc(PByte(pA), LineWidthA);
               end;
          end;
          pA := pAi;
          inc(pA, i);
          pA^ := pA^ + 1;

          dec(PByte(pWi), LineWidthW);
          dec(PByte(pAi), LineWidthA);
     end;

     // Diagonalization of the bidiagonal form: loop over singular values and over
     // allowed iterations
     for k := width - 1 downto 0 do
     begin
          if Assigned(progress) then
             progress(Round(30 + 70*((Min(width, height) - 1 - k)/(Min(width, height)))));

          for its := 0 to cMaxNumSVDIter - 1 do
          begin
               flag := true;
               // test for splitting
               for l := k downto 0 do
               begin
                    nm := l - 1;
                    if abs(rv1[l]) + anorm = anorm then
                    begin
                         flag := False;
                         break;
                    end;

                    if abs(PDouble(PAnsiChar(W) + nm*LineWidthW)^) + anorm = anorm then
                       break;
               end;

               // Cancellation of rv1[o] if l > 1
               if flag then
               begin
                    c := 0;
                    s := 1;
                    for i := l to k do
                    begin
                         f := s*rv1[i];
                         rv1[i] := c*rv1[i];
                         if abs(f) + anorm <> anorm then    // check if the value is lower than the precission in contrast to anorm
                         begin
                              g := PDouble(PAnsiChar(W) + i*LineWidthW)^;
                              h := pythag(f, g);
                              PDouble(PAnsiChar(W) + i*LineWidthW)^ := h;
                              h := 1/h;
                              c := g*h;
                              s := -f*h;
                              pA := A;
                              inc(pA, nm);
                              pAj := A;
                              inc(pAj, i);

                              for j := 0 to height - 1 do
                              begin
                                   y := pA^;
                                   z := pAj^;
                                   pA^ := y*c + z*s;
                                   pAj^ := z*c - y*s;

                                   inc(PByte(pA), LineWidthA);
                                   inc(PByte(pAj), LineWidthA);
                              end;
                         end;
                    end;
               end;

               z := PDouble(PAnsiChar(W) + k*LineWidthW)^;
               // convergence
               if l = k then
               begin
                    if z < 0 then
                    begin
                         PDouble(PAnsiChar(W) + k*LineWidthW)^ := -z;

                         pV := V;
                         inc(pV, k);

                         for j := 0 to width - 1 do
                         begin
                              pV^ := -pV^;
                              inc(PByte(pV), LineWidthV);
                         end;
                    end;

                    break;
               end;

               if its = cMaxNumSVDIter - 1 then
                  exit;

               x := PDouble(PAnsiChar(W) + l*LineWidthW)^;
               nm := k - 1;
               y := PDouble(PAnsiChar(W) + nm*LineWidthW)^;
               g := rv1[nm];
               h := rv1[k];
               f := ((y - z)*(y + z) + (g - h)*(g + h))/(2*h*y);
               g := pythag(f, 1);
               val := g;
               if f < 0 then
                  val := -val;
               f := ((x - z)*(x + z) + h*((y/(f + val)) - h))/x;
               c := 1;
               s := 1;

               // next QR transformation
               for j := l to nm do
               begin
                    i := j + 1;
                    g := rv1[i];
                    y := PDouble(PAnsiChar(W) + i*LineWidthW)^;
                    h := s*g;
                    g := c*g;
                    z := pythag(f, h);
                    rv1[j] := z;
                    c := f/z;
                    s := h/z;
                    f := x*c + g*s;
                    g := g*c - x*s;
                    h := y*s;
                    y := y*c;

                    pVj := V;
                    inc(pVj, j);
                    pV := V;
                    inc(pV, i);
                    for jj := 0 to width - 1 do
                    begin
                         x := pVj^;
                         z := pV^;
                         pVj^ := x*c + z*s;
                         pV^ := z*c - x*s;

                         inc(PByte(pV), LineWidthV);
                         inc(PByte(pVj), LineWidthV);
                    end;

                    z := pythag(f, h);
                    PDouble(PAnsiChar(W) + j*LineWidthW)^ := z;
                    // rotation can be arbitrary if z = 0
                    if z <> 0 then
                    begin
                         z := 1/z;
                         c := f*z;
                         s := h*z;
                    end;

                    f := c*g + s*y;
                    x := c*y - s*g;

                    pA := A;
                    inc(pA, i);
                    pAj := A;
                    inc(pAj, j);
                    for jj := 0 to height - 1 do
                    begin
                         y := pAj^;
                         z := pA^;
                         pAj^ := y*c + z*s;
                         pA^ := z*c - y*s;
                         inc(PByte(pAj), LineWidthA);
                         inc(PByte(pA), LineWidthA);
                    end;
               end;

               rv1[l] := 0;
               rv1[k] := f;
               PDouble(PAnsiChar(W) + k*LineWidthW)^ := x;
          end;
     end;

     Result := srOk;
end;

function MatrixSVD(A : PDouble; const LineWidthA : TASMNativeInt; width : TASMNativeInt; Height : TASMNativeInt;
                   U : PDouble; const LineWidthU : TASMNativeInt; W : PDouble; const LineWidthW : TASMNativeInt;
                   V : PDouble; const LineWidthV : TASMNativeInt; progress : TLinEquProgress) : TSVDResult;
begin
     assert(LineWidthA >= width*sizeof(double), 'Dimension error');
     assert(LineWidthV >= width*sizeof(double), 'Dimension error');

     MatrixCopy(U, LineWidthU, A, LineWidthA, width, Height);
     Result := MatrixSVDInPlace(U, LineWidthU, width, Height, W, LineWidthW, V, LineWidthV, progress);
end;


// ###########################################
// #### SVD implementation
// ###########################################

// a set of functions to calculate the singular value decomposition

// lapack: dgebd2
// work needs to be at least Max(width, height)
// the other arrays need to be at least min(width, height)
procedure MatrixBidiagonalUnblocked(A : PDouble; const LineWidthA : TASMNativeInt; Width, Height : TASMNativeInt;
                 D, E, TauQ, TauP : PConstDoubleArr; work : PDouble);
var i : TASMNativeInt;
    pAii : PDouble;
    pAmin : PDouble;
    pC : PDouble;
begin
     if (width <= 0) or (height <= 0) then
        exit;

     if height >= width then
     begin
          // Reduce to upper bidiagonal form

          for i := 0 to width - 1 do
          begin
               pAii := GenPtr(A, i, i, LineWidthA);
               // Generate elementary reflector H(i) to annihilate A(i+1:m,i)
               pAmin := GenPtr(A, i, min( i + 1, height - 1), LineWidthA);
               GenElemHousholderRefl(pAmin, LineWidthA, height - i, pAii^, @tauq^[i]);

               d[i] := pAii^;
               pAii^ := 1;

               // Apply H(i) to A(i:m,i+1:n) from the left
               if i < width - 1 then
               begin
                    pC := pAii;
                    inc(pC);
                    ApplyElemHousholderReflLeft( pAii, LineWidthA, pC, LineWidthA, width - i - 1, height - i, @tauq[i], work);
               end;

               pAii^ := d[i];

               if i < width - 1 then
               begin
                    // Generate elementary reflector G(i) to annihilate A(i,i+2:n)
                    pAmin := GenPtr(A, min( i + 2, width - 1), i, LineWidthA);
                    inc(pAii);
                    GenElemHousholderRefl(pAmin, sizeof(double), width - i - 1, pAii^, @taup[i]);
                    e[i]:= pAii^;
                    pAii^ := 1;

                    // Apply G(i) to A(i+1:m,i+1:n) from the right
                    pC := pAii;
                    inc(PByte(pC), LineWidthA);
                    ApplyElemHousholderReflRight(pAii, sizeof(double), pC, LineWidthA,
                                                 width - i - 1, height - i - 1, @taup[i], work);
                    pAii^ := e[i];
               end
               else
                   taup[i] := 0;
          end;
     end
     else
     begin
          // Reduce to lower bidiagonal form
          for i := 0 to height - 1 do
          begin
               pAii := GenPtr(A, i, i, LineWidthA);

               pAmin := GenPtr(A, min( i + 1, width - 1), i, LineWidthA);
               GenElemHousholderRefl(pAmin, sizeof(double), width - i, pAii^, @taup^[i]);

               d[i] := pAii^;
               pAii^ := 1;

               // Apply G(i) to A(i+1:m,i:n) from the right
               if i < height - 1 then
               begin
                    pC := pAii;
                    inc(PByte(pC), LineWidthA);
                    ApplyElemHousholderReflRight(pAii, sizeof(double), pC, LineWidthA, width - i, height - i - 1, @taup[i], work);
               end;

               pAii^ := d[i];

               if i < height - 1 then
               begin
                    inc(PByte(pAii), LineWidthA);

                    // Generate elementary reflector H(i) to annihilate
                    // A(i+2:m,i)
                    pAmin := GenPtr(A, i, min(i + 2, Height - 1), LineWidthA);
                    inc(PByte(pAii), LineWidthA);

                    GenElemHousholderRefl(pAmin, LineWidthA, height - i - 1, pAii^, @tauq[i]);
                    e[i]:= pAii^;
                    pAii^ := 1;

                    // Apply H(i) to A(i+1:m,i+1:n) from the left
                    pC := pAii;
                    inc(pC);

                    inc(pC);
                    ApplyElemHousholderReflLeft(pAii, LineWidthA, pC, LineWidthA, width - i - 1, height - i - 1, @tauq[i], work);
                    pAii^ := e[i];
               end
               else
                   tauq[i] := 0;
          end;
     end;
end;


// reduce parts of A to a bidiagonal form
procedure dlabrd(A : PDouble; const LineWidthA : TASMNativeInt; width, height : TASMNativeInt; nb : TASMNativeInt;
                 D, E, TauQ, TauP : PConstDoubleArr; X : PDouble; const LineWidthX : TASMNativeInt;
                 Y : PDouble; const LineWidthY : TASMNativeInt);
var i : TASMNativeInt;
    pAii00 : PDouble;                    // first index y, second x, first 1 -> +1 height, second 1 +1 width
    pAii01 : PDouble;
    pAii10 : PDouble;
    pAii11 : PDouble;
    pA0i01, pAi010 : PDouble;
    pAi000, pA0i00 : PDouble;

    pYi000, pY0i00 : PDouble;
    pYii10 : PDouble;
    pYi010 : PDouble;
    pXi000, pX0i00 : PDouble;
    pXi010 : PDouble;
    pXii10 : PDouble;
    pAminIM : PDouble;
begin
     if (width <= 0) or (Height <= 0) then
        exit;

     if height >= Width then
     begin
          for i := 0 to nb - 1 do
          begin
               // init variables
               pAii00 := GenPtr(A, i, i, LineWidthA);
               pAii01 := GenPtr(A, i + 1, i, LineWidthA);
               pA0i01 := GenPtr(A, i + 1, 0, LineWidthA);
               pAii11 := GenPtr(A, i + 1, i + 1, LineWidthA);
               pAi010 := GenPtr(A, 0, i + 1, LineWidthA);
               pAi000 := GenPtr(A, 0, i, LineWidthA);
               pA0i00 := GenPtr(A, i, 0, LineWidthA);

               pYi000 := GenPtr(Y, 0, i, LineWidthY);
               pY0i00 := GenPtr(Y, i, 0, LineWidthY);
               pYii10 := GenPtr(Y, i, i + 1, LineWidthY);
               pYi010 := GenPtr(Y, 0, i + 1, LineWidthY);
               pXi000 := GenPtr(X, 0, i, LineWidthX);
               pXi010 := GenPtr(X, 0, i + 1, LineWidthX);
               pXii10 := GenPtr(X, i, i + 1, LineWidthX);
               pX0i00 := GenPtr(X, i, 0, LineWidthX);

               // Update A(i:m,i)
               // CALL dgemv( 'No transpose', m-i+1, i-1, -one, a( i, 1 ),
               //             lda, y( i, 1 ), ldy, one, a( i, i ), 1 )
               MatrixMtxVecMult(pAii00, LineWidthA, pAi000, pYi000, LineWidthA, sizeof(double), i, height - i, -1, 1);
               //  CALL dgemv( 'No transpose', m-i+1, i-1, -one, x( i, 1 ),
               //              ldx, a( 1, i ), 1, one, a( i, i ), 1 )
               MatrixMtxVecMult(pAii00, LineWidthA, pXi000, pA0i00, LineWidthX, LineWidthA, i, height - i, -1, 1);

               // generate reflection Q(i) to annihilate A(i+1:m,i)
               pAminIM := GenPtr(A, i, Min(i + 1, Height - 1), LineWidthA);
               GenElemHousholderRefl(pAminIM, LineWidthA, height - i, pAii00^, @tauQ^[i]);

               d[i] := pAii00^;

               if i < width - 1 then
               begin
                    pAii00^ := 1;

                    // Compute Y(i+1:n, i)

                    // CALL dgemv( 'Transpose', m-i+1, n-i, one, a( i, i+1 ),
                    //             lda, a( i, i ), 1, zero, y( i+1, i ), 1 )
                    MatrixMtxVecMultT(pYii10, LineWidthY, pAii01, pAii00, LineWidthA, LineWidthA, width - i - 1, height - i, 1, 0);
                    // CALL dgemv( 'Transpose', m-i+1, i-1, one, a( i, 1 ), lda,
                    //              a( i, i ), 1, zero, y( 1, i ), 1 )
                    MatrixMtxVecMultT(pY0i00, LineWidthY, pAi000, pAii00, LineWidthA, LineWidthA, i, height - i, 1, 0);
                    // CALL dgemv( 'No transpose', n-i, i-1, -one, y( i+1, 1 ),
                    //             ldy, y( 1, i ), 1, one, y( i+1, i ), 1 )
                    MatrixMtxVecMult(pYii10, LineWidthY, pYi010, pY0i00, LineWidthY, LineWidthY, i, width - i - 1, -1, 1);
                    // CALL dgemv( 'Transpose', m-i+1, i-1, one, x( i, 1 ), ldx,
                    //              a( i, i ), 1, zero, y( 1, i ), 1 )
                    MatrixMtxVecMultT(pY0i00, LineWidthY, pXi000, pAii00, LineWidthX, LineWidthA, i, height - i, 1, 0);
                    // CALL dgemv( 'Transpose', i-1, n-i, -one, a( 1, i+1 ),
                    //              lda, y( 1, i ), 1, one, y( i+1, i ), 1 )
                    MatrixMtxVecMultT(pYii10, LineWidthY, pA0i01, pY0i00, LineWidthA, LineWidthY, width - i - 1, i, -1, 1);

                    // CALL dscal( n-i, tauq( i ), y( i+1, i ), 1 )
                    MatrixScaleAndAdd(pYii10, LineWidthY, 1, Width - i - 1, 0, tauq[i]);

                    // update A(i, i+1:n)

                    // CALL dgemv( 'No transpose', n-i, i, -one, y( i+1, 1 ),
                    //              ldy, a( i, 1 ), lda, one, a( i, i+1 ), lda )
                    MatrixMtxVecMult(pAii01, sizeof(double), pYi010, pAi000, LineWidthY, sizeof(double), i + 1, width - i - 1, -1, 1);
                    // CALL dgemv( 'Transpose', i-1, n-i, -one, a( 1, i+1 ),
                    //             lda, x( i, 1 ), ldx, one, a( i, i+1 ), lda )
                    MatrixMtxVecMultT(pAii01, sizeof(double), pA0i01, pXi000, LineWidthA, sizeof(double), width - i - 1, i, -1, 1);

                    // Generate reflection P(i) to annihilate A(i,i+2:n)
                    pAminIM := GenPtr(A, min(i + 2, width - 1), i, LineWidthA);
                    GenElemHousholderRefl(pAminIM, sizeof(double), width - i - 1, pAii01^, @taup^[i]);

                    e[i] := pAii01^;
                    pAii01^ := 1;

                    // Compute X(i+1:m,i)

                    //CALL dgemv( 'No transpose', m-i, n-i, one, a( i+1, i+1 ),
                    //            lda, a( i, i+1 ), lda, zero, x( i+1, i ), 1 )
                    MatrixMtxVecMult(pXii10, LineWidthX, pAii11, pAii01, LineWidthA, sizeof(double), width - i - 1, height - i - 1, 1, 0);
                    // CALL dgemv( 'Transpose', n-i, i, one, y( i+1, 1 ), ldy,
                    //              a( i, i+1 ), lda, zero, x( 1, i ), 1 )
                    MatrixMtxVecMultT(pX0i00, LineWidthX, pYi010, pAii01, LineWidthY, sizeof(double), i + 1, width - i - 1, 1, 0);
                    // CALL dgemv( 'No transpose', m-i, i, -one, a( i+1, 1 ),
                    //             da, x( 1, i ), 1, one, x( i+1, i ), 1 )
                    MatrixMtxVecMult(pXii10, LineWidthX, pAi010, pX0i00, LineWidthA, LineWidthX, i + 1, height - i - 1, -1, 1);
                    // CALL dgemv( 'No transpose', i-1, n-i, one, a( 1, i+1 ),
                    //             lda, a( i, i+1 ), lda, zero, x( 1, i ), 1 )
                    MatrixMtxVecMult(pX0i00, LineWidthX, pA0i01, pAii01, LineWidthA, sizeof(double), width - i - 1, i, 1, 0);
                    // CALL dgemv( 'No transpose', m-i, i-1, -one, x( i+1, 1 ),
                    //             ldx, x( 1, i ), 1, one, x( i+1, i ), 1 )
                    MatrixMtxVecMult(pXii10, LineWidthX, pXi010, pX0i00, LineWidthX, LineWidthX, i, height - i - 1, -1, 1);
                    // CALL dscal( m-i, taup( i ), x( i+1, i ), 1 )
                    MatrixScaleAndAdd(pXii10, LineWidthX, 1, height - i - 1, 0, taup[i]);
               end;
          end;
     end
     else
     begin
          raise Exception.Create('Not yet implemented');
//          // Reduce to lower bidiagonal form
//          for i := 0 to nb - 1 do
//          begin
//               // init variables
//               pAii00 := GenPtr(A, i, i, LineWidthA);
//               pAii10 := GenPtr(A, i, i + 1, LineWidthA);
//               pA0i01 := GenPtr(A, i + 1, 0, LineWidthA);
//               pAii11 := GenPtr(A, i + 1, i + 1, LineWidthA);
//               pAi010 := GenPtr(A, 0, i + 1, LineWidthA);
//               pAi000 := GenPtr(A, 0, i, LineWidthA);
//               pA0i00 := GenPtr(A, i, 0, LineWidthA);
//
//               pYi000 := GenPtr(Y, 0, i, LineWidthY);
//               pY0i00 := GenPtr(Y, i, 0, LineWidthY);
//               pYii10 := GenPtr(Y, i, i + 1, LineWidthY);
//               pYi010 := GenPtr(Y, 0, i + 1, LineWidthY);
//               pXi000 := GenPtr(X, 0, i, LineWidthX);
//               pXi010 := GenPtr(X, 0, i + 1, LineWidthX);
//               pXii10 := GenPtr(X, i, i + 1, LineWidthX);
//               pX0i00 := GenPtr(X, i, 0, LineWidthX);
//
//               // Update A(i,i:n)
//
//                // CALL dgemv( 'No transpose', n-i+1, i-1, -one, y( i, 1 ),
//                //              ldy, a( i, 1 ), lda, one, a( i, i ), lda )
//               GenericMtxVecMult(pAii00, sizeof(double), pYi000, pAi000, LineWidthY, sizeof(double), i, width - i, -1, 1);
//                // CALL dgemv( 'Transpose', i-1, n-i+1, -one, a( 1, i ), lda,
//                //              x( i, 1 ), ldx, one, a( i, i ), lda )
//               GenericMtxVecMultT(pAii00, sizeof(double), pA0i00, pXi000, LineWidthA, sizeof(double), width - i, i, -1, 1);
//
//               // Generate reflection P(i) to annihilate A(i,i+1:n)
//               pAminIM := GenPtr(A, i, Min(i + 1, Width - 1), LineWidthA);
//               GenElemHousholderRefl(pAminIM, sizeof(double), width - i, pAii00^, @tauP^[i]);
//
//               d^[i] := pAii00^;
//
//               if i < height - 1 then
//               begin
//                    pAii00^ := 1;
//
//                    // Compute X(i+1:m,i)
//
//                    // CALL dgemv( 'No transpose', m-i, n-i+1, one, a( i+1, i ),
//                    //              lda, a( i, i ), lda, zero, x( i+1, i ), 1 )
//                    GenericMtxVecMult(pXii10, LineWidthX, pAii10, pAii00, LineWidthA, sizeof(double), width - i, height - i - 1, 1, 0);
//                    // CALL dgemv( 'Transpose', n-i+1, i-1, one, y( i, 1 ), ldy,
//                    //              a( i, i ), lda, zero, x( 1, i ), 1 )
//                    GenericMtxVecMultT(pX0i00, LineWidthX, pYi000, pAii00, LineWidthY, sizeof(double), i, width - i, 1, 0);
//                    // CALL dgemv( 'No transpose', m-i, i-1, -one, a( i+1, 1 ),
//                    //              lda, x( 1, i ), 1, one, x( i+1, i ), 1 )
//                    GenericMtxVecMult(pXii10, LineWidthX, pAi010, pX0i00, LineWidthA, LineWidthX, i, height - i - 1, -1, 1);
//                    // CALL dgemv( 'No transpose', i-1, n-i+1, one, a( 1, i ),
//                    //             lda, a( i, i ), lda, zero, x( 1, i ), 1 )
//                    GenericMtxVecMult(pX0i00, LineWidthX, pA0i00, pAii00, LineWidthA, sizeof(double), width - i, i, 1, 0);
//                    // CALL dgemv( 'No transpose', m-i, i-1, -one, x( i+1, 1 ),
//                    //             ldx, x( 1, i ), 1, one, x( i+1, i ), 1 )
//                    GenericMtxVecMult(pXii10, LineWidthX, pXi010, pX0i00, LineWidthX, LineWidthX, i, height - i - 1, -1, 1);
//                    // CALL dscal( m-i, taup( i ), x( i+1, i ), 1 )
//                    MatrixScaleAndAdd(pXii10, LineWidthX, 1, height - i - 1, 0, taup^[i]);
//
//                    // Update A(i+1:m,i)
//
//                    // CALL dgemv( 'No transpose', m-i, i-1, -one, a( i+1, 1 ),
//                    //             lda, y( i, 1 ), ldy, one, a( i+1, i ), 1 )
//                    GenericMtxVecMult(pAii10, LineWidthA, pAi010, pYi000, LineWidthA, sizeof(double), i, height - i - 1, -1, 1);
//                    // CALL dgemv( 'No transpose', m-i, i, -one, x( i+1, 1 ),
//                    //             ldx, a( 1, i ), 1, one, a( i+1, i ), 1 )
//                    GenericMtxVecMult(pAii10, LineWidthA, pXi010, pA0i00, LineWidthX, LineWidthA, i + 1, height - i - 1, -1, 1);
//
//                    // Generate reflection Q(i) to annihilate A(i+2:m,i)
//                    pAminIM := GenPtr(A, i, min(i + 2, height - 1), LineWidthA);
//                    GenElemHousholderRefl(pAminIM, LineWidthA, height - i - 1, pAii10^, @tauq^[i]);
//                    e^[i] := pAii10^;
//                    pAii10^ := 1;
//
//                    // Compute Y(i+1:n,i)
//
//                    //  CALL dgemv( 'Transpose', m-i, n-i, one, a( i+1, i+1 ),
//                    //              lda, a( i+1, i ), 1, zero, y( i+1, i ), 1 )
//                    GenericMtxVecMultT(pYii10, LineWidthY, pAii11, pAii10, LineWidthA, LineWidthA, width - i - 1, height - i - 1, 1, 0);
//                    // CALL dgemv( 'Transpose', m-i, i-1, one, a( i+1, 1 ), lda,
//                    //             a( i+1, i ), 1, zero, y( 1, i ), 1 )
//                    GenericMtxVecMultT(pY0i00, LineWidthY, pAi010, pAii10, LineWidthA, LineWidthA, i, height - i - 1, 1, 0);
//                    // CALL dgemv( 'No transpose', n-i, i-1, -one, y( i+1, 1 ),
//                    //             ldy, y( 1, i ), 1, one, y( i+1, i ), 1 )
//                    GenericMtxVecMult(pYii10, LineWidthY, pYi010, pY0i00, LineWidthY, LineWidthY, i, width - i - 1, -1, 1);
//                    // CALL dgemv( 'Transpose', m-i, i, one, x( i+1, 1 ), ldx,
//                    //             a( i+1, i ), 1, zero, y( 1, i ), 1 )
//                    GenericMtxVecMultT(pY0i00, LineWidthY, pXi010, pAii10, LineWidthX, LineWidthA, i + 1, height - i - 1, 1, 0);
//                    // CALL dgemv( 'Transpose', i, n-i, -one, a( 1, i+1 ), lda,
//                    //             y( 1, i ), 1, one, y( i+1, i ), 1 )
//                    GenericMtxVecMultT(pYii10, LineWidthY, pA0i01, pY0i00, LineWidthA, LineWidthY, width - i - 1, i + 1, -1, 1);
//                    // CALL dscal( n-i, tauq( i ), y( i+1, i ), 1 )
//                    MatrixScaleAndAdd(pYii10, LineWidthY, 1, width - i - 1, 0, tauq^[i]);
//               end;
//          end;
     end;
end;


// DORGBR generates one of the real orthogonal matrices Q or P**T
// determined by DGEBRD when reducing a real matrix A to bidiagonal
// form: A = Q * B * P**T.  Q and P**T are defined as products of
// elementary reflectors H(i) or G(i) respectively.




procedure Dorgbr_P(A : PDouble; const LineWidthA : TASMNativeInt; width, height, k : TASMNativeInt; tau : PDouble;
   work : PDouble; const svdData : TMtxSVDDecompData); //; const LineWidthWork : TASMNativeInt);
var pA : PDouble;
    i : Integer;
    j : Integer;
    pAij, PAi1j : PDouble;
begin
     // Shift the vectors which define the elementary reflectors one
     // row downward, and set the first row and column of P**T to
     // those of the unit matrix
     pA := A;
     pA^ := 1;
     inc(PByte(pA), LineWidthA);

     for i := 1 to width - 1 do
     begin
          pA^ := 0;
          inc(PByte(pA), LineWidthA);
     end;

     for j := width - 1 downto 1 do   // rows
     begin
          for i := j to width - 1 do // cols
          begin
               pAij := GenPtr(A, i, j, LineWidthA);
               pAi1j := GenPtr(A, i, j - 1, LineWidthA);

               pAij^ := pAi1j^;
          end;
     end;

     pAij := GenPtr(A, 1, 0, LineWidthA);
     for i := 1 to width - 1 do
     begin
          pAij^ := 0;
          inc(pAij);
     end;


     if width > 1 then
     begin
          pA := GenPtr(A, 1, 1, LineWidthA);

          // form P_Transposed
          svdData.LeftQFromQRDecomp(pA, LineWidthA, width - 1, height - 1, tau, QRBlockSize, work, svdData.Progress);
          //MatrixLeftQFromQRDecomp(pA, LineWidthA, width - 1, height - 1, tau, QRBlockSize, work);
     end;
end;

// DGEBRD reduces a general real M-by-N matrix A to upper or lower
// bidiagonal form B by an orthogonal transformation: Q**T * A * P = B
// M : height
// N : width
// column major
// On entry, the M-by-N general matrix to be reduced.
// On exit,
// if m >= n, the diagonal and the first superdiagonal are
//   overwritten with the upper bidiagonal matrix B; the
//   elements below the diagonal, with the array TAUQ, represent
//   the orthogonal matrix Q as a product of elementary
//   reflectors, and the elements above the first superdiagonal,
//   with the array TAUP, represent the orthogonal matrix P as
//   a product of elementary reflectors;
// if m < n, the diagonal and the first subdiagonal are
//   overwritten with the lower bidiagonal matrix B; the
//   elements below the first subdiagonal, with the array TAUQ,
//   represent the orthogonal matrix Q as a product of
//   elementary reflectors, and the elements above the diagonal,
//   with the array TAUP, represent the orthogonal matrix P as
//   a product of elementary reflectors.
procedure MatrixBidiagonalBlocked(A : PDouble; const LineWidthA : TASMNativeInt; width, height : TASMNativeInt;
                 D, E, TauQ, TauP : PDouble; work : PDouble; const svdData : TMtxSVDDecompData);
var lwkopt : TASMNativeInt;
    minmn : TASMNativeInt;
    ws : TASMNativeInt;
    nx : integer;
    i, j : TASMNativeInt;
    pWork : PDouble;
    ldwrkx, ldwrky : TASMNativeInt;
    pAinb, pAinbnb : PDouble;
    pAii : PDouble;
    pX, pY : PDouble;
    pXnb, pYnb : PDouble;
    pWorkMult : PDouble;
    pD, pE, pTauQ, pTauP : PDouble;
    nb : integer;
begin
     minmn := min(width, height);
     if minmn = 0 then
        exit;

     ldwrkx := height;
     ldwrky := width;

     // determine when to switch from blocked to unblocked code
     // note: we assume that work is large enough to hold (width + height)*nb elements
     nb := svdData.SVDPnlSize;

     nx := nb;
     nx := Min(nx, minmn);

     i := 0;

     px := Work;
     py := Work;
     inc(py, ldwrkx*nb);
     pWorkMult := pY;
     inc(pWorkMult, ldwrky*nb);

     pD := D;
     pE := E;
     pTauQ := tauQ;
     pTauP := tauP;

     while i < minmn - nx do
     begin
          // Reduce rows and columns i:i+nb-1 to bidiagonal form and return
          // the matrices X and Y which are needed to update the unreduced
          // part of the matrix
          pAii := GenPtr(A, i, i, LineWidthA);
          dlabrd(pAii, LineWidthA, width - i, height - i, nb,  PConstDoubleArr(pD), PConstDoubleArr(pE),
                 PConstDoubleArr(ptauQ), PConstDoubleArr(ptauP), px, nb*sizeof(double), py, nb*sizeof(double) );

          // update the trailing submatir A(i +nb:m, i+nb:n) using an update of the form
          // A = A - VY**T - X*U**T
          pAinb := GenPtr(A, i, i + nb, LineWidthA);
          pAinbnb := GenPtr(A, i + nb, i + nb, LineWidthA);

          //  CALL dgemm( 'No transpose', 'Transpose', m-i-nb+1, n-i-nb+1,
          //               nb, -one, a( i+nb, i ), lda,
          //               work( ldwrkx*nb+nb+1 ), ldwrky, one,
          //               a( i+nb, i+nb ), lda )
          pYnb := GenPtr(pY, 0, nb, nb*sizeof(double));

          svdData.MatrixMultT2(pAinbnb, LineWidthA, pAinb, pYnb, nb, height - i - nb, nb, width - i - nb,
                               LineWidthA, nb*sizeof(double), nb, doSub, pWorkMult );
          //MatrixMultT2Ex(pAinbnb, LineWidthA, pAinb, pYnb, nb, height - i - nb, nb, width - i - nb,
//                          LineWidthA, nb*sizeof(double), nb, doSub, pWorkMult );

          // CALL dgemm( 'No transpose', 'No transpose', m-i-nb+1, n-i-nb+1,
          //             nb, -one, work( nb+1 ), ldwrkx, a( i, i+nb ), lda,
          //             one, a( i+nb, i+nb ), lda )
          pXnb := GenPtr(pX, 0, nb, nb*sizeof(double));
          pAinb := GenPtr(A, i + nb, i, LineWidthA);
          //MatrixMultEx(pAinbnb, LineWidthA, pXnb, pAinb, nb, height - i - nb, width - i - nb, nb,
          svdData.MatrixMultEx(pAinbnb, LineWidthA, pXnb, pAinb, nb, height - i - nb, width - i - nb, nb,
                               nb*sizeof(double), LineWidthA, nb, doSub, pWorkMult );

          // Copy diagonal and off-diagonal elements of B back into A
          if height >= width then
          begin
               j := i;
               while j < i + nb do
               begin
                    pAii^ := PConstDoubleArr(d)^[j];
                    inc(pAii);
                    pAii^ := PConstDoubleArr(e)^[j];
                    inc(PByte(pAii), LineWidthA);
                    inc(j);
               end;
          end
          else
          begin
               j := i;
               while j < i + nb do
               begin
                    pAii^ := PConstDoubleArr(d)^[j];
                    inc(PByte(pAii), LineWidthA);
                    pAii^ := PConstDoubleArr(e)^[j];
                    inc(pAii);
                    inc(j);
               end;
          end;

          // counters
          inc(i, nb);
          inc(pD, nb);
          inc(pE, nb);
          inc(pTauQ, nb);
          inc(pTauP, nb);
     end;

     // ###########################################
     // #### Use unblocked code to reduce the remainder of the matrix
     pAii := GenPtr(A, i, i, LineWidthA);
     MatrixBidiagonalUnblocked(pAii, LineWidthA, width - i, height - i, PConstDoubleArr(pD),
            PConstDoubleArr(pE), PConstDoubleArr(pTauQ), PConstDoubleArr(pTauP),
            work);
end;

// computes the singular value decomposition of a 2-by-2 triangular matrix
procedure dlasv2(const F, G, H : double; var ssmin, ssmax, snr, csr, snl, csl : double);
var ft, fa, ht, ha : double;
    pmax : integer;
    swap : boolean;
    temp : double;
    ga, gt : double;
    clt : double;
    crt : double;
    slt : double;
    srt : double;
    gasmal : boolean;
    epsilon : double;
    d : double;
    l : double;
    m, t : double;
    mm, tt : double;
    a, r, s : double;
    tsign : double;
begin
     srt := 0;
     crt := 0;
     clt := 0;
     slt := 0;

     ft := f;
     fa := abs( ft );
     ht := h;
     ha := abs( h );

     // PMAX points to the maximum absolute element of matrix
     // PMAX = 1 if F largest in absolute values
     // PMAX = 2 if G largest in absolute values
     // PMAX = 3 if H largest in absolute values
     pmax := 1;
     swap := ha > fa;

     if swap then
     begin
          pmax := 3;

          temp := ft;
          ft := ht;
          ht := temp;
          temp := fa;
          fa := ha;
          ha := temp;
     end;

     gt := g;
     ga := abs( gt );
     if ga = 0 then
     begin
          // diagonal matrix
          ssmin := ha;
          ssmax := fa;
          clt := 1;
          crt := 1;
          slt := 0;
          srt := 0;
     end
     else
     begin
          gasmal := True;

          if ga > fa then
          begin
               pmax := 2;
               if fa/ga < cDoubleEpsilon then
               begin
                    // Case of very large GA
                    gasmal := False;
                    ssmax := ga;
                    if ha > 1
                    then
                        ssmin := fa/(ga/ha)
                    else
                        ssmin := (fa/ga)*ha;

                    clt := 1;
                    slt := ht/gt;
                    srt := 1;
                    crt := ft/gt;
               end;
          end;

          if gasmal then
          begin
               // normal case
               d := fa - ha;
               if d = fa
               then // copes with infinite F or H
                   l := 1
               else
                   l := d/fa;

               // Note that 0 .le. L .le. 1
               m := gt/ft;

               // Note that abs(M) .le. 1/macheps
               t := 2 - l;

               // Note that T .ge. 1
               mm := sqr(m);
               tt := sqr(t);

               s := sqrt( tt + mm );

               // Note that 1 .le. S .le. 1 + 1/macheps
               if l = 0
               then
                   r := abs( m )
               else
                   r := sqrt(l*l + mm );

               a := 0.5*(s + r);

               ssmin := ha/a;
               ssmax := fa*a;

               if mm = 0 then
               begin
                    // note tha m is ver tiny
                    if l = 0
                    then
                        t := sign(2, ft)*sign(1, gt)
                    else
                        t := gt/sign(d, ft) + m/t;
               end
               else
                   t := (m/(s + t) + m/(r + l) )*(1 + a);

               l := sqrt(sqr(t) + 4);
               crt := 2/l;
               srt := t/l;
               clt := (crt + srt*m)/a;
               slt := (ht / ft)*srt/a;
          end;
     end;

     if swap then
     begin
          csl := srt;
          snl := crt;
          csr := slt;
          snr := clt;
     end
     else
     begin
          csl := clt;
          snl := slt;
          csr := crt;
          snr := srt;
     end;

     // Correct signs of SSMAX and SSMIN
     tsign := 0;
     if pmax = 1 then
        tsign := sign(1, csr)*sign(1, csl)*sign(1, f);
     if pmax = 2 then
        tsign := sign(1, snr)*sign(1, csl)*sign(1, g);
     if pmax = 3 then
        tsign := sign(1, snr)*sign(1, snl)*sign(1, h);

     ssmax := sign(ssmax, tsign);
     ssmin := sign(ssmin, tsign*sign(1, f)*sign(1, h));
end;


// compute singular values of 2 by 2 matrix
procedure dlas2(const F, G, H : double; var ssmin, ssmax : double); //inline;
var fa, ga, ha, fhmn, fhmx : double;
    ass, at, au : double;
    c : double;
begin
     fa := abs(f);
     ga := abs(g);
     ha := abs(h);
     fhmn := min(fa, ha);
     fhmx := max(fa, ha);

     if fhmn = 0 then
     begin
          ssmin := 0;
          if fhmx = 0
          then
              ssmax := ga
          else
              ssmax := max( fhmx, ga )*sqrt( 1 + sqr(min( fhmx, ga ) / max( fhmx, ga ) ) );
     end
     else
     begin
          if ga < fhmx then
          begin
               ass := 1 + fhmn/fhmx;
               at := ( fhmx-fhmn ) / fhmx;
               au := sqr(ga/fhmx);
               c := 2/(sqrt( ass*ass + au) + sqrt( at*at + au ) );
               ssmin := fhmn*c;
               ssmax := fhmx/c;
          end
          else
          begin
               au := fhmx/ga;

               if au = 0 then
               begin
                    // Avoid possible harmful underflow if exponent range
                    // asymmetric (true SSMIN may not underflow even if
                    // AU underflows)
                    ssmin := (fhmn*fhmx)/ga;
                    ssmax := ga;
               end
               else
               begin
                    ass := 1 + fhmn/fhmx;
                    at := (fhmx - fhmn)/fhmx;
                    c := 1/( sqrt(1 + sqr(ass*au) ) + sqrt( 1 + sqr(at*au) ) );
                    ssmin := (fhmn*c)*au;
                    ssmin := ssmin + ssmin;
                    ssmax := ga/(c + c);
               end;
          end;
     end;
end;


procedure DswapCol(A : PDouble; const LineWidthA : TASMNativeInt; i1, i2, height : TASMNativeInt);
var temp : double;
    pAi1, pAi2 : PDouble;
    y: TASMNativeInt;
begin
     pAi1 := A;
     inc(pAi1, i1);
     pAi2 := A;
     inc(pAi2, i2);

     for y := 0 to height - 1 do
     begin
          temp := pAi1^;
          pAi1^ := pAi2^;
          pAi2^ := temp;

          inc(PByte(pAi1), LineWidthA);
          inc(PByte(pAi2), LineWidthA);
     end;
end;

procedure DswapRow(A : PDouble; const LineWidthA : TASMNativeInt; i1, i2, width : TASMNativeInt);
var temp : double;
    pAi1, pAi2 : PConstDoubleArr;
    x : TASMNativeInt;
begin
     pAi1 := PConstDoubleArr(A);
     inc(PByte(pAi1), i1*LineWidthA);
     pAi2 := PConstDoubleArr(A);
     inc(PByte(pAi2), i2*LineWidthA);

     for x := 0 to width - 1 do
     begin
          temp := pAi1^[x];
          pAi1^[x] := pAi2^[x];
          pAi2^[x] := temp;
     end;
end;

procedure MatrixRotateFunc(u : PDouble; const LineWidthU : TASMNativeInt;
                       vt : PDouble; const LineWidthVT : TASMNativeInt;
                       m, nru, ncvt : TASMNativeInt; const cosl, sinl, cosr, sinr : double);
begin
     if ncvt > 0 then
        MatrixRotate(ncvt, GenPtr(vt, 0, m - 1, LineWidthVT), sizeof(double), GenPtr(vt, 0, m, LineWidthVT), sizeof(double), cosr, sinr);
     if NRU > 0 then
        MatrixRotate(nru, GenPtr(u, m - 1, 0, LineWidthU), LineWidthU, GenPtr(u, m, 0, LineWidthU), LineWidthU, cosl, sinl);
     //if ncc > 0 then
//                  drot(ncc, GenPtr(C, 0, m - 1, LineWidthC), sizeof(double), GenPtr(c, 0, m, LineWidthC), sizeof(double), cosl, sinl);

end;

procedure MatrixRotateUpdF(u : PDouble; const LineWidthU : TASMNativeInt;
                          vt : PDouble; const LineWidthVT : TASMNativeInt;
                          mm, nru, ncvt : TASMNativeInt;
                          w1, w2,
                          w3, w4 : PConstDoubleArr);
begin
     if ncvt > 0 then
        ApplyPlaneRotSeqLVF(ncvt, mm, vt, LineWidthVT, w1, w2);
     if nru > 0 then
        ApplyPlaneRotSeqRVF(mm, nru, U, LineWidthU, w3, w4);
//                    if ncc > 0 then
//                       dlasr_LVF(ncc, m - ll + 1, GenPtr(c, 0, ll, LineWidthC), LineWidthC,
//                                 PConstDoubleArr(GenPtr(work, nm12, 0, 0)), PConstDoubleArr(GenPtr(work, nm13, 0, 0)));

end;

procedure MatrixRotateUpdB(u : PDouble; const LineWidthU : TASMNativeInt;
                           vt : PDouble; const LineWidthVT : TASMNativeInt;
                           mm, nru, ncvt : TASMNativeInt;
                           w1, w2,
                           w3, w4 : PConstDoubleArr);
begin
     if ncvt > 0 then
        ApplyPlaneRotSeqLVB(ncvt, mm, vt, LineWidthVT, w1, w2);
     if nru > 0 then
        ApplyPlaneRotSeqRVB(mm, nru, U, LineWidthU, w3, w4);
//                    if ncc > 0 then
//                       dlasr_LVB(ncc, m - ll + 1, GenPtr(c, 0, ll, LineWidthC), LineWidthC,
//                                 PConstDoubleArr(GenPtr(work, 0, 0, 0)), PConstDoubleArr(GenPtr(work, n - 1, 0, 0)));

end;

// upper diagonal
// DBDSQR computes the singular values and, optionally, the right and/or
// left singular vectors from the singular value decomposition (SVD) of
// a real N-by-N (upper or lower) bidiagonal matrix B using the implicit
// zero-shift QR algorithm.
function dbdsqr_upper(D : PConstDoubleArr; E : PConstDoubleArr;
                     VT : PDouble; const LineWidthVT : TASMNativeInt; U : PDouble; const LineWidthU : TASMNativeInt;
                     C : PDouble; const LineWidthC : TASMNativeInt;
                     N, NCVT, NRU, NCC : TASMNativeInt; Work : PDouble;
                     const svdData : TMtxSVDDecompData) : TSVDResult;
const maxitr : integer = 6;
var nm1 : TASMNativeInt;
    nm12 : TASMNativeInt;
    nm13 : TASMNativeInt;
    smax : double;
    tol : Double;
    i : TASMNativeInt;
    thresh : double;
    sminoa : double;
    maxIt : TASMNativeInt;
    oldll : TASMNativeInt;
    iter : TASMNativeInt;
    oldm : TASMNativeInt;
    m : TASMNativeInt;
    mu : double;
    sminl : double;
    lll, ll: Integer;
    smin : double;
    abss, abse : double;
    idir : integer;
    doCont : boolean;
    shift, r : double;
    sll : double;
    f, g, h : double;
    cs, oldcs : double;
    sn, oldsn : double;
    pwork : PConstDoubleArr;
    sigmn, sigmx, sinr, cosr, sinl, cosl : double;
    isub : TASMNativeInt;
    j : TASMNativeInt;
    llFound : boolean;
begin
     Result := srOk;
     pwork := PConstDoubleArr(work);

     tol := 2e-14;//cDoubleEpsilon* max(10, min(100, Power(cDoubleEpsilon, -0.125 ) )) ;

     // Compute approximate maximum, minimum singular values
     smax := 0;
     for i := 0 to n - 1 do
         smax := max(smax, abs(d[i]));
     for i := 0 to n - 2 do
         smax := max(smax, abs(e[i]) );

     // relative accuracy desired
     sminl := 0;
     if tol >= 0 then
     begin
          sminoa := abs(d[0]);

          if sminoa <> 0 then
          begin
               mu := sminoa;
               for i := 1 to n - 1 do
               begin
                    mu := abs( d[i] )*( mu/(mu + abs(e[i - 1])));
                    sminoa := min(sminoa, mu);
                    if sminoa = 0 then
                       break;
               end;
          end;

          sminoa := sminoa/(sqrt(n));
          thresh := max( tol*sminoa, maxitr*n*n*MinDouble);
     end
     else
         thresh := max(abs(tol)*smax, maxitr*n*n*MinDouble);

     idir := 0;
     nm1 := n - 1;
     nm12 := nm1 + nm1;
     nm13 := nm12 + nm1;

     // Prepare for main iteration loop for the singular values
     // MAXIT is the maximum number of passes through the inner
     // loop permitted before nonconvergence signalled.)
     maxIt := maxitr*n*n;
     oldll := -1;
     iter := 0;
     oldm := -1;

     // M points to last element of unconverged part of matrix
     m := n - 1;

     while m > 0 do
     begin
          if iter > maxit then
          begin
               Result := srNoConvergence;
               exit;
          end;

          // Find diagonal block of matrix to work on
          if (tol < 0) and (abs(d[m]) <= thresh) then
             d[m] := 0;

          smax := abs(d[m]);
          smin := smax;

          doCont := False;
          lll := 0;
          ll := -1;
          while lll < m do
          begin
               ll := m - lll - 1;
               abss := abs(d[ll]);
               abse := abs(e[ll]);

               if (tol < 0) and (abss <= thresh) then
                  d[ll] := 0;
               if abse <= thresh then
               begin
                    e[ll] := 0;
                    // matrix splits since e[ll] = 0

                    if ll = m - 1 then
                    begin
                         // Convergence of bottom singular value, return to top of loop
                         dec(m);
                         doCont := True;
                         break;
                    end;

                    break;
               end;

               smin := min(smin, abss);
               smax := max(smax, max(abss, abse));
               inc(lll);
          end;

          if doCont then
             continue;

          if lll = m then
             ll := -1;

          inc(ll);

          // E(LL) through E(M-1) are nonzero, E(LL-1) is zero
          if ll = m - 1 then
          begin
               // 2 by 2 block, handle separately
               dlasv2(d[m - 1], e[m - 1], d[m], sigmn, sigmx, sinr, cosr, sinl, cosl);
               d[m - 1] := sigmx;
               e[m - 1] := 0;
               d[m] := sigmn;

               // Compute singular vectors, if desired
               svdData.MatrixRotate(u, LineWidthU, vt, LineWidthVT, m, nru, ncvt, cosl, sinl, cosr, sinr);

               // todo: this part is not used in our routines -> perhaps add it?
               //if ncc > 0 then
//                  drot(ncc, GenPtr(C, 0, m - 1, LineWidthC), sizeof(double), GenPtr(c, 0, m, LineWidthC), sizeof(double), cosl, sinl);

               dec(m, 2);

               // next m
               continue;
          end;

          // If working on new submatrix, choose shift direction
          // from larger end diagonal element towards smaller)
          if (ll > oldm) or (m < oldll) then
          begin
               if abs( d[ll] ) >= abs( d[m] )
               then
                   // chase bulge from top
                   idir := 1
               else // Chase bulge from bottom (big end) to top (small end)
                   idir := 2;
          end;

          // apply convergence test
          if idir = 1 then
          begin
               // Run convergence test in forward direction
               // First apply standard test to bottom of matrix
               if (abs(e[m - 1]) <= abs(tol)*abs(d[m]) ) or
                  ( (tol < 0) and (abs( e[m - 1]) <= thresh ))
               then
               begin
                    e[m - 1] := 0;
                    continue;
               end;

               // apply convergence criterion forward
               if tol >= 0 then
               begin
                    mu := abs( d[ll] );
                    sminl := mu;

                    doCont := False;
                    for lll := ll to m - 1 do
                    begin
                         if abs( e[lll] ) <= tol*mu then
                         begin
                              e[lll] := 0;
                              doCont := True;
                              break;
                         end;

                         mu := abs( d[lll + 1]) * (mu / (mu + abs( e[lll] )));
                         sminl := min(sminl, mu);
                    end;

                    // check if we need to go all up
                    if doCont then
                       continue;
               end;
          end
          else
          begin
               // Run convergence test in backward direction
               // First apply standard test to top of matrix

               if (abs(e[ll]) < abs(tol)*abs(d[ll]) ) or
                  ( (tol < 0) and (abs(e[ll]) <= thresh) )
               then
               begin
                    e[ll] := 0;
                    // go all up
                    continue;
               end;

               if tol >= 0 then
               begin
                    // If relative accuracy desired,
                    // apply convergence criterion backward
                    mu := abs( d[m] );
                    sminl := mu;

                    doCont := False;
                    for lll := m - 1 downto ll do
                    begin
                         if abs(e[lll]) <= tol*mu then
                         begin
                              e[lll] := 0;
                              doCont := True;
                              break;
                         end;

                         mu := abs(d[lll])*(mu/(mu + abs(e[lll])));
                         sminl := min (sminl, mu);
                    end;

                    // all way up
                    if doCont then
                       continue;
               end;
          end;

          oldll := ll;
          oldm := m;

          // Compute shift.  First, test if shifting would ruin relative
          // accuracy, and if so set the shift to zero.
          if (tol >= 0) and  (n*tol*(sminl/smax) <= max(cDoubleEpsilon, 0.01*tol ) )
          then
              shift := 0
          else
          begin
               // Compute the shift from 2-by-2 block at end of matrix
               if idir = 1 then
               begin
                    sll := abs(d[ll]);
                    dlas2(d[m - 1], e[m - 1], d[m], shift, r);
               end
               else
               begin
                    sll := abs( d[m] );
                    dlas2(d[ll], e[ll], d[ll + 1], shift, r);
               end;

               if sll > 0 then
               begin
                    if sqr(shift/sll) < cDoubleEpsilon then
                       shift := 0;
               end;
          end;

          // increment iteration count
          iter := iter + m - ll;

          // If SHIFT = 0, do simplified QR iteration
          if shift = 0 then
          begin
               if idir = 1 then
               begin
                    // Chase bulge from top to bottom
                    // Save cosines and sines for later singular vector updates
                    cs := 1;
                    oldcs := 1;
                    oldsn := 0;

                    for i := ll to m - 1 do
                    begin
                         GenPlaneRotation( d[i]*cs, e[i], cs, sn, r);
                         if i > ll then
                            e[i - 1] := oldsn*r;
                         GenPlaneRotation(oldcs*r, d[i + 1]*sn, oldcs, oldsn, d[i]);

                         pwork[i - ll] := cs;
                         pwork[i - ll + nm1] := sn;
                         pwork[i - ll + nm12] := oldcs;
                         pwork[i - ll + nm13] := oldsn;
                    end;

                    h := d[m]*cs;
                    d[m] := h*oldcs;
                    e[m - 1] := h*oldsn;

                    // update singular vectors
                    svdData.MatrixRotateUpdF(GenPtr(u, ll, 0, LineWidthU),
                                     LineWidthU, GenPtr(Vt, 0, ll, LineWidthVT), LineWidthVT,
                                     m - ll + 1, nru, ncvt,
                                     PConstDoubleArr(GenPtr(work, 0, 0, 0)),
                                     PConstDoubleArr(GenPtr(work, n - 1, 0, 0)),
                                     PConstDoubleArr(GenPtr(work, nm12, 0, 0)),
                                     PConstDoubleArr(GenPtr(work, nm13, 0, 0)));

                    //if ncvt > 0 then
//                       dlasr_LVF(ncvt, m - ll + 1, GenPtr(VT, 0, ll, LineWidthVT), LineWidthVT,
//                                 PConstDoubleArr(GenPtr(work, 0, 0, 0)), PConstDoubleArr(GenPtr(work, n - 1, 0, 0)));
//                    if nru > 0 then
//                       dlasr_RVF(m - ll + 1, nru, GenPtr(u, ll, 0, LineWidthU), LineWidthU,
//                                 PConstDoubleArr(GenPtr(work, nm12, 0, 0)), PConstDoubleArr(GenPtr(work, nm13, 0, 0)));
//                    if ncc > 0 then
//                       dlasr_LVF(ncc, m - ll + 1, GenPtr(c, 0, ll, LineWidthC), LineWidthC,
//                                 PConstDoubleArr(GenPtr(work, nm12, 0, 0)), PConstDoubleArr(GenPtr(work, nm13, 0, 0)));

                    // test convergence
                    if abs( e[m - 1] ) <= thresh then
                       e[m - 1] := 0;
               end
               else
               begin
                    // Chase bulge from bottom to top
                    // Save cosines and sines for later singular vector updates
                    // Chase bulge from top to bottom
                    // Save cosines and sines for later singular vector updates
                    cs := 1;
                    oldcs := 1;
                    oldsn := 0;

                    for i := m downto ll + 1 do
                    begin
                         GenPlaneRotation( d[i]*cs, e[i - 1], cs, sn, r);
                         if i < m then
                            e[i] := oldsn*r;
                         GenPlaneRotation(oldcs*r, d[i - 1]*sn, oldcs, oldsn, d[i]);

                         pwork[i - ll - 1] := cs;
                         pwork[i - ll - 1 + nm1] := -sn;
                         pwork[i - ll - 1 + nm12] := oldcs;
                         pwork[i - ll - 1 + nm13] := -oldsn;
                    end;

                    h := d[ll]*cs;
                    d[ll] := h*oldcs;
                    e[ll] := h*oldsn;

                    // update singular vectors
                    svdData.MatrixRotateUpdB(GenPtr(u, ll, 0, LineWidthU), LineWidthU,
                                             GenPtr(Vt, 0, ll, LineWidthVT), LineWidthVT,
                                             m - ll + 1, nru, ncvt,
                                             PConstDoubleArr(GenPtr(work, nm12, 0, 0)),
                                             PConstDoubleArr(GenPtr(work, nm13, 0, 0)),
                                             PConstDoubleArr(GenPtr(work, 0, 0, 0)),
                                             PConstDoubleArr(GenPtr(work, n - 1, 0, 0)));

                    //if ncvt > 0 then
//                       dlasr_LVB(ncvt, m - ll + 1, GenPtr(VT, 0, ll, LineWidthVT), LineWidthVT,
//                                 PConstDoubleArr(GenPtr(work, nm12, 0, 0)), PConstDoubleArr(GenPtr(work, nm13, 0, 0)));
//                    if nru > 0 then
//                       dlasr_RVB(m - ll + 1, nru, GenPtr(u, ll, 0, LineWidthU), LineWidthU,
//                                 PConstDoubleArr(GenPtr(work, 0, 0, 0)), PConstDoubleArr(GenPtr(work, n - 1, 0, 0)));
//                    if ncc > 0 then
//                       dlasr_LVB(ncc, m - ll + 1, GenPtr(c, 0, ll, LineWidthC), LineWidthC,
//                                 PConstDoubleArr(GenPtr(work, 0, 0, 0)), PConstDoubleArr(GenPtr(work, n - 1, 0, 0)));

                    // test convergence
                    if abs( e[ll] ) <= thresh then
                       e[ll] := 0;
               end;
          end
          else
          begin
               // use non zero shift
               if idir = 1 then
               begin
                    // Chase bulge from top to bottom
                    // Save cosines and sines for later singular vector updates
                    f := ( abs(D[ll] ) - shift)*
                         ( sign(1, d[ll] ) + shift/d[ll] );
                    g := e[ll];

                    for i := ll to m - 1 do
                    begin
                         GenPlaneRotation( f, g, cosr, sinr, r);
                         if i > ll then
                            e[i - 1] := r;

                         f := cosr*d[i] + sinr*e[i];
                         e[i] := cosr*e[i] - sinr*d[i];
                         g := sinr*d[i + 1];
                         d[i + 1] := cosr*d[i + 1];
                         GenPlaneRotation(f, g, cosl, sinl, r);

                         d[i] := r;
                         f := cosl*e[i] + sinl*d[i + 1];
                         d[i + 1] := cosl*d[i + 1] - sinl*e[i];
                         if i < m - 1 then
                         begin
                              g := sinl*e[i + 1];
                              e[i + 1] := cosl*e[i + 1];
                         end;

                         pwork[i - ll] := cosr;
                         pwork[i - ll + nm1] := sinr;
                         pwork[i - ll + nm12] := cosl;
                         pwork[i - ll + nm13] := sinl;
                    end;

                    e[m - 1] := f;

                    // update singular vectors
                    svdData.MatrixRotateUpdF(GenPtr(u, ll, 0, LineWidthU), LineWidthU,
                                     GenPtr(Vt, 0, ll, LineWidthVT), LineWidthVT,
                                     m - ll + 1, nru, ncvt,
                                     PConstDoubleArr(GenPtr(work, 0, 0, 0)),
                                     PConstDoubleArr(GenPtr(work, n - 1, 0, 0)),
                                     PConstDoubleArr(GenPtr(work, nm12, 0, 0)),
                                     PConstDoubleArr(GenPtr(work, nm13, 0, 0)));

                    //if ncvt > 0 then
//                       dlasr_LVF(ncvt, m - ll + 1, GenPtr(VT, 0, ll, LineWidthVT), LineWidthVT,
//                                 PConstDoubleArr(GenPtr(work, 0, 0, 0)), PConstDoubleArr(GenPtr(work, n - 1, 0, 0)));
//                    if nru > 0 then
//                       dlasr_RVF(m - ll + 1, nru, GenPtr(u, ll, 0, LineWidthU), LineWidthU,
//                                 PConstDoubleArr(GenPtr(work, nm12, 0, 0)), PConstDoubleArr(GenPtr(work, nm13, 0, 0)));
//                    if ncc > 0 then
//                       dlasr_LVF(ncc, m - ll + 1, GenPtr(c, 0, ll, LineWidthC), LineWidthC,
//                                 PConstDoubleArr(GenPtr(work, nm12, 0, 0)), PConstDoubleArr(GenPtr(work, nm13, 0, 0)));

                    // test convergence
                    if abs( e[m - 1] ) <= thresh then
                       e[m - 1] := 0;
               end
               else
               begin
                    // Chase bulge from bottom to top
                    // Save cosines and sines for later singular vector updates
                    f := ( abs( d[m] ) - shift)*(sign(1, d[m]) + shift/d[m]);
                    g := e[m - 1];

                    for i := m downto ll + 1 do
                    begin
                         GenPlaneRotation(f, g, cosr, sinr, r);
                         if i < m then
                            e[i] := r;
                         f := cosr*d[i] + sinr*e[i - 1];
                         e[i - 1] := cosr*e[i - 1] - sinr*d[i];
                         g := sinr*d[i - 1];
                         d[i - 1] := cosr*d[i - 1];
                         GenPlaneRotation(f, g, cosl, sinl, r);
                         d[i] := r;
                         f := cosl*e[i - 1] + sinl*d[i - 1];
                         d[i - 1] := cosl*d[i - 1] - sinl*e[i - 1];
                         if i > ll + 1 then
                         begin
                              g := sinl*e[i - 2];
                              e[i - 2] := cosl*e[i - 2];
                         end;

                         pwork[i - ll - 1] := cosr;
                         pwork[i - ll - 1 + nm1] := -sinr;
                         pwork[i - ll - 1 + nm12] := cosl;
                         pwork[i - ll - 1 + nm13] := -sinl;
                    end;

                    e[ll] := f;

                    // test convergence
                    if abs( e[ll] ) <= thresh then
                       e[ll] := 0;

                    // update singular vectors
                    svdData.MatrixRotateUpdB(GenPtr(u, ll, 0, LineWidthU), LineWidthU,
                                     GenPtr(Vt, 0, ll, LineWidthVT), LineWidthVT,
                                     m - ll + 1, nru, ncvt,
                                     PConstDoubleArr(GenPtr(work, nm12, 0, 0)),
                                     PConstDoubleArr(GenPtr(work, nm13, 0, 0)),
                                     PConstDoubleArr(GenPtr(work, 0, 0, 0)),
                                     PConstDoubleArr(GenPtr(work, n - 1, 0, 0)));

                    // update singular vectors
//                    if ncvt > 0 then
//                       dlasr_LVB(ncvt, m - ll + 1, GenPtr(VT, 0, ll, LineWidthVT), LineWidthVT,
//                                 PConstDoubleArr(GenPtr(work, nm12, 0, 0)), PConstDoubleArr(GenPtr(work, nm13, 0, 0)));
//                    if nru > 0 then
//                       dlasr_RVB(m - ll + 1, nru, GenPtr(u, ll, 0, LineWidthU), LineWidthU,
//                                 PConstDoubleArr(GenPtr(work, 0, 0, 0)), PConstDoubleArr(GenPtr(work, n - 1, 0, 0)));
//                    if ncc > 0 then
//                       dlasr_LVB(ncc, m - ll + 1, GenPtr(c, 0, ll, LineWidthC), LineWidthC,
//                                 PConstDoubleArr(GenPtr(work, 0, 0, 0)), PConstDoubleArr(GenPtr(work, n - 1, 0, 0)));
               end;
          end;
     end;

     // All singular values converged, so make them positive
     for i := 0 to n - 1 do
     begin
          if d[i] < 0 then
          begin
               d[i] := -d[i];

               // change sign of singular vectors, if desired
               if ncvt > 0 then
                  MatrixScaleAndAdd(GenPtr(VT, 0, i, LineWidthVT), LineWidthVT, NCVT, 1, 0, -1);
          end;
     end;

     // sort singular values into decreasing order
     for i := 0 to n - 2 do
     begin
          isub := 0;
          smin := d[0];

          for j := 1 to n - i - 1 do
          begin
               if d[j] <= smin then
               begin
                    isub := j;
                    smin := d[j];
               end;
          end;

          if isub <> n - i - 1 then
          begin
               // swap singular values and vectors
               d[isub] := d[n - i - 1];
               d[n - i - 1] := smin;

               if ncvt > 0 then
                  DswapRow(vt, LineWidthVT, isub, n - i - 1, ncvt);
               if nru > 0 then
                  DswapCol(u, LineWidthU, isub, n - i - 1, nru);
               if ncc > 0 then
                  DswapCol(C, LineWidthC, isub, n - i - 1, ncc);
          end;
     end;
end;

// single threaded work mem need:
function SVDMemSize(const svdData : TMtxSVDDecompData; width, height : TASMNativeInt) : TASMNativeInt;
var w2 : TASMNativeInt;
    minmn : TASMNativeInt;
    mnthr : TASMNativeInt;
begin
     minmn := Min(Width, Height);

     // for factors of 2.. (better aligned memory)
     w2 := width + width and 1;

     mnthr := Trunc( minmn*1.6 );

     Result := 16 + Max(w2*w2*sizeof(double) + w2*sizeof(double) + svdData.SVDMultSize*w2*sizeof(double),
                        sizeof(double)*(w2*w2 + 3*w2)
                        )  +
                     BlockMultMemSize( BlockMatrixCacheSize );

     // no qr decomp -> reduce workspace needed
     if (height >= width) and (Height <= mnthr) then
     begin
          // mem for TauP, TauQ, D, E, Bidiagonlaization multiplication memory
          Result := 16 + sizeof(double)*(  (3 + 5)*w2 + (w2 + height)*SVDBlockSize) + BlockMultMemSize(Max(SVDBlockSize, QRMultBlockSize));
     end;
end;

// The output is the computation of A= U*W*V' whereas U is stored in A, and W is a vector 0..Width-1. The matrix V (not V') must be as large as Width*Width!
// dgesvd from netlib
// note: the routine uses quite a lot of function placeholders within the svdData structure.
// this data structure is also used to perform the multithreaded version of SVD
function InternalMatrixSVD( A : PDouble; const LineWidthA : TASMNativeInt; width : TASMNativeInt;
                            Height : TASMNativeInt;
                            W : PConstDoubleArr; V : PDouble; const LineWidthV : TASMNativeInt;
                            const svdData : TMtxSVDDecompData) : TSVDResult;
var work : PDouble;
    pMem : PByte;
    eps1, smallnum , bignum : double;
    minmn : TASMNativeInt;
    absMax : double;
    doScale : boolean;
    mnthr : TASMNativeInt;
    pWorkITau : PDouble;
    pWorkTauQ, pWorkTaup, pWorkI, pWorkIE : PDouble;
    pWorkU : PDouble;
    pV, pA : PConstDoubleArr;
    pWorkIArr : PConstDoubleArr;
    isScaled : boolean;
    x, y : TASMNativeInt;
    pWorkIR : PDouble;
    LineWidthIR : TASMNativeInt;
    ldwrku, ldwrkr : TASMNativeInt;
    itau : TASMNativeInt;
    chunk : TASMNativeInt;
    w2, h2 : TASMNativeInt;
    memNeed : TASMNativeInt;
    qrMem : TASMNativeInt;
begin
     if width > Height then
        raise Exception.Create('Not implemented');

     // ###########################################
     // #### Machine constants
     minmn := Min(Width, Height);
     eps1 := eps(1);
     if MinDouble <= 1/MaxDouble
     then
         smallnum := 1/MaxDouble * (1 + eps1)
     else
         smallnum := MinDouble;

     smallnum := sqrt(smallnum)/eps1;
     bignum := 1/smallnum;

     // for factors of 2.. (better aligned memory)
     w2 := width + width and 1;
     h2 := height + height and 1;

     mnthr := Trunc( minmn*1.6 );

     // ###########################################
     // #### Allocate workspace for fast algorithm...
     if svdData.pWorkMem = nil then
     begin
          memNeed := 16 + Max(2*w2*w2*sizeof(double) + w2,
                              sizeof(double)*(w2*w2 + 3*w2)
                             )  +
                     BlockMultMemSize( BlockMatrixCacheSize );

          // no qr decomp -> reduce workspace needed
          if (height >= width) and (Height <= mnthr) then
          begin
               // mem for TauP, TauQ, D, E, Bidiagonlaization multiplication memory
               memNeed := 16 + sizeof(double)*(  (3 + 5)*w2 + (w2 + height)*SVDBlockSize) + BlockMultMemSize(Max(SVDBlockSize, QRMultBlockSize));
          end;

          pMem := AllocMem(memNeed);
     end
     else
         pMem := svdData.pWorkMem;

     work := PDouble(pMem);
     if (TASMNativeUInt(work) and $0000000F) <> 0 then
        work := PDouble(TASMNativeUInt(work) + 16 - TASMNativeUInt(work) and $0F);


     // ###########################################
     // #### Scale A if max element outside range [smallnum,bignum]
     absMax := abs(MatrixMax(A, width, height, LineWidthA));

     isScaled := (absMax < smallNum) or (absMax > bigNum);

     if absMax < smallnum
     then
         MatrixScaleAndAdd(A, LineWidthA, Width, Height, 0, smallnum/absMax)
     else if absMax > bigNum
     then
         MatrixScaleAndAdd(A, LineWidthA, Width, Height, 0, bignum/absMax);

     // ###########################################
     // #### crossover
     if Height >= Width then
     begin
          if Height > mnthr then
          begin
               // path3 in dgesvd:
               pWorkIR := work;
               pWorkITau := work;
               inc(pWorkITau, w2*w2);

               // Compute A=Q*R
               if svdData.qrDecomp(A, LineWidthA, width, height, pWorkITau, nil, QRBlockSize, svdData.Progress) <> qrOK then
               begin
                    Result := srNoConvergence;
                    if svdData.pWorkMem = nil then
                       FreeMem(pMem);
                    exit;
               end;

               // copy R to VT - zeroing out below it
               MatrixCopy(V, LineWidthV, A, LineWidthA, width, width);

               for y := 1 to width - 1 do
               begin
                    pV := PConstDoubleArr( GenPtr(V, 0, y, LineWidthV) );
                    for x := 0 to  y - 1 do
                        pV^[x] := 0;
               end;

               // Generate Q in A
               svdData.qFromQRDecomp(A, LineWidthA, width, Height, pWorkITau, QRBlockSize, nil, svdData.Progress);

               // Bidiagonalize R in VT, copying result to WORK(IR)
               pWorkIE := pWorkITau;
               pWorkTauQ := pWorkIE;
               inc(pWorkTauQ, w2);
               pWorkTauP := pWorkTauQ;
               inc(pWorkTauP, w2);
               pWorkI := pWorkTauP;
               inc(pWorkI, w2);

               MatrixBidiagonalBlocked(V, LineWidthV, width, width, PDouble(W), pWorkIE, pWorkTauQ, pWorkTauP, pWorkI, svdData );

               // copy lower triangle to work
               for y := 0 to width - 1 do
               begin
                    pV := PConstDoubleArr( GenPtr(v, 0, y, LineWidthV) );
                    pWorkIArr := PConstDoubleArr( GenPtr(pWorkIR, 0, y, w2*sizeof(double)) );
                    for x := 0 to y do
                        pWorkIArr^[x] := pV^[x];
               end;

               // Generate left vectors bidiagonalizing R in WORK(IR)
               svdData.qFromQRDecomp(pWorkIR, w2*sizeof(double), width, width, pWorkTauq, svdData.QRPnlSize, nil, svdData.Progress);

               // Generate right vectors bidiangoizaing R in VT
               Dorgbr_P(V, LineWidthV, width, width, width, pWorkTaup, nil, svdData);

               pWorkI := pWorkIE;
               inc(pWorkI, width);

               // Perform bidiagonal QR iteration, computing left
               // singular vectors of R in WORK(IR) and computing right
               // singular vectors of R in VT
               Result := dbdsqr_upper(W, PConstDoubleArr(pWorkIE), V, LineWidthV, pWorkIR, w2*sizeof(double), nil, 0, width, width, width, 0, pworki, svdData);

               if Result = srOk then
               begin
                    // Q in A by left singular vectors of R...

                    // pworkU = w2 x svdMultsize
                    pWorkU := pWorkIE;
                    pWorkI := pWorkU;   // w2*w2
                    inc(pWorkI, w2*w2);

                    x := 0;
                    while x < height do
                    begin
                         chunk := min(height - x, svdData.SVDMultSize);

                         svdData.MatrixMultEx(pWorkU, w2*sizeof(double), GenPtr(A, 0, x, LineWidthA), pWorkIR, width, chunk, width, width,
                                      LineWidthA, w2*sizeof(double), BlockMatrixCacheSize, doNone, pWorkI);
                         MatrixCopy(GenPtr(A, 0, x, LineWidthA), LineWidthA, pWorkU, w2*sizeof(double), width, chunk);

                         inc(x, chunk);
                    end;
               end;
          end
          else
          begin
               // m at least N but not much larger
               pWorkIE := work;
               pWorkTauQ := work;
               inc(pWorkTauQ, w2);
               pWorkTaup := pWorkTauQ;
               inc(pWorkTaup, w2);

               pWorkI := pWorkTaup;
               inc(pWorkI, w2);

               // Bidiagonalize A
               MatrixBidiagonalBlocked(A, LineWidthA, width, height, PDouble(W), pWorkIE, pWorkTauQ, pWorkTauP, pWorkI, svdData);

               // right singular vectors on V, copy upper triangle part of A
               for y := 0 to width - 1 do
               begin
                    pV := PConstDoubleArr(GenPtr(V, 0, y, LineWidthV));
                    pA := PConstDoubleArr(GenPtr(A, 0, y, LineWidthA));
                    for x := y to width - 1 do
                        pV^[x] := pA^[x];
               end;

               dorgbr_P(v, LineWidthV, width, width, width, pWorkTaup, nil, svdData);

               // Left singular vectors in A

               svdData.qFromQRDecomp(A, LineWidthA, width, height, pWorkTauq, svdData.QRPnlSize, pWorkI, svdData.Progress);

               pWorkI := pWorkIE;
               inc(pWorkI, w2);
               // bidiagonal QR iteration. if desired, computing
               // left singular vectors in A and computing right singular
               // vectors in VT
               Result := dbdsqr_upper(W, PConstDoubleArr(pWorkIE), V, LineWidthV, A, LineWidthA, nil, 0, width, width, height, 0, pWorkI, svdData)
          end;
     end;

     // ###########################################
     // #### Cleanup
     if svdData.pWorkMem = nil  then
        FreeMem(pMem);
end;

function MatrixSVDInPlace2( A : PDouble; const LineWidthA : TASMNativeInt; width : TASMNativeInt; Height : TASMNativeInt;
                 W : PConstDoubleArr; V : PDouble; const LineWidthV : TASMNativeInt; SVDBlockSize : TASMNativeInt = 32; progress : TLinEquProgress = nil) : TSVDResult;
var svdData : TMtxSVDDecompData;
    minmn : TASMNativeInt;
    aT : TDoubleDynArray;
    pA : PConstDoubleArr;
    x, y : TASMNativeInt;
begin
     if width > Height then
     begin
          // make use of A_T = (U * W * V_T)_T = (V * W_T * U_T)
          MatrixTranspose(V, LineWidthV, A, LineWidthA, width, height);
          MatrixScaleAndAdd(A, LineWidthA, width, height, 0, 0);  // clear out A

          Result := MatrixSVDInPlace2(V, LineWidthV, height, width, W, A, LineWidthA, SVDBlockSize, progress);
          if Result = srOk then
          begin
               aT := MatrixTranspose(A, LineWidthA, height, height);
               // copy back
               MatrixCopy(A, LineWidthA, @aT[0], height*sizeof(double), height, height);

               // set the values that are not needed any more on A to zero
               for y := 0 to height - 1 do
               begin
                    pA := PConstDoubleArr( GenPtr(A, 0, y, LineWidthA));
                    for x := height to Width - 1 do
                        pA^[x] := 0;
               end;
          end;
          exit;
     end;
     // ###########################################
     // #### Machine constants
     minmn := Min(Width, Height);

     // ###################################################
     // ##### Apply svd
     svdData.Progress := progress;
     svdData.SVDPnlSize := SVDBlockSize;
     svdData.SVDMultSize := minmn;
     svdData.QRPnlSize := QRBlockSize;
     svdData.qrDecomp := MatrixQRDecompInPlace2;
     svdData.qFromQRDecomp := MatrixQFromQRDecomp;
     svdData.LeftQFromQRDecomp := MatrixLeftQFromQRDecomp;
     svdData.BlkMltSize := BlockMatrixCacheSize;
     svdData.MatrixMultEx := MatrixMultEx;
     svdData.MatrixMultT1 := MatrixMultT1Ex;
     svdData.MatrixMultT2 := MatrixMultT2Ex;

     svdData.MatrixRotateUpdB := MatrixRotateUpdB;
     svdData.MatrixRotateUpdF := MatrixRotateUpdF;
     svdData.MatrixRotate := MatrixRotateFunc;

     // ###########################################
     // #### Allocate workspace for fast algorithm...
     svdData.pWorkMem := AllocMem( SVDMemSize(svdData, width, height) );

     Result := InternalMatrixSVD(A, LineWidthA, width, height, W, V, LineWidthV, svdData);

     FreeMem(svdData.pWorkMem);
end;

// ###################################################
// ##### SVD - including the use of threaded
// ##### QR decomposition, threaded Matrix Multiplication
// ##### and parallel matrix rotation
// ###################################################

function SVDMemSizeThr(const svdData : TMtxSVDDecompData; width, height : TASMNativeInt) : TASMNativeInt;
var w2 : TASMNativeInt;
    minmn : TASMNativeInt;
    mnthr : TASMNativeInt;
begin
     minmn := Min(Width, Height);

     // for factors of 2.. (better aligned memory)
     w2 := width + width and 1;

     mnthr := Trunc( minmn*1.6 );

     Result := 16 + Max(w2*w2*sizeof(double) + w2*sizeof(double) + svdData.SVDMultSize*w2*sizeof(double),
                        sizeof(double)*(w2*w2 + 3*w2)
                        )  +
                    numCPUCores*BlockMultMemSize( BlockMatrixCacheSize );

     // no qr decomp -> reduce workspace needed
     if (height >= width) and (Height <= mnthr) then
     begin
          // mem for TauP, TauQ, D, E, Bidiagonlaization multiplication memory
          Result := 16 + sizeof(double)*(  (3 + 5)*w2 + (w2 + height)*SVDBlockSize) + numCPUCores*BlockMultMemSize(Max(SVDBlockSize, QRMultBlockSize));
     end;
end;

// for matrix rotation and rotation update
type
  TMatrixRotateObj = class(TObject)
    u : PDouble;
    LineWidthU : TASMNativeInt;
    vt : PDouble;
    LineWidthVT : TASMNativeInt;
    mm, nru, ncvt : TASMNativeInt;
    w1, w2,
    w3, w4 : PConstDoubleArr;

    cosl, sinl,
    cosr, sinr : double;

    constructor CreateRot(aU : PDouble; const aLineWidthU : TASMNativeInt;
                       aVt : PDouble; const aLineWidthVT : TASMNativeInt;
                       am, anru, ancvt : TASMNativeInt; const acosl, asinl, acosr, asinr : double);

    constructor Create(aU : PDouble; const aLineWidthU : TASMNativeInt;
                              aVt : PDouble; const aLineWidthVT : TASMNativeInt;
                              aNM, anru, ancvt : TASMNativeInt;
                              aw1, aw2,
                              aw3, aw4 : PConstDoubleArr); overload;
  end;

constructor TMatrixRotateObj.CreateRot(aU : PDouble; const aLineWidthU : TASMNativeInt;
                       aVt : PDouble; const aLineWidthVT : TASMNativeInt;
                       am, anru, ancvt : TASMNativeInt; const acosl, asinl, acosr, asinr : double);
begin
     u := aU;
     LineWidthU := aLineWidthU;
     vt := aVt;
     LineWidthVT := aLineWidthVT;
     mm := aM;
     nru := anru;
     ncvt := ancvt;

     cosl := acosl;
     cosr := acosr;
     sinl := asinl;
     sinr := asinr;
end;

constructor TMatrixRotateObj.Create(aU : PDouble; const aLineWidthU : TASMNativeInt;
                                    aVt : PDouble; const aLineWidthVT : TASMNativeInt;
                                    aNM, anru, ancvt : TASMNativeInt;
                                    aw1, aw2,
                                    aw3, aw4 : PConstDoubleArr);
begin
     u := aU;
     LineWidthU := aLineWidthU;
     vt := aVt;
     LineWidthVT := aLineWidthVT;
     mm := aNM;
     nru := anru;
     ncvt := ancvt;
     w1 := aw1;
     w2 := aW2;
     w3 := aw3;
     w4 := aW4;
end;

function ThrApplyPlaneRotSeqLVB(obj : TObject) : integer;
begin
     if TMatrixRotateObj(obj).ncvt > 0 then
        ApplyPlaneRotSeqLVB(TMatrixRotateObj(obj).ncvt, TMatrixRotateObj(obj).mm,
                  TMatrixRotateObj(obj).vt, TMatrixRotateObj(obj).LineWidthVT,
                  TMatrixRotateObj(obj).w1, TMatrixRotateObj(obj).w2);

     Result := 0;
end;

function ThrApplyPlaneRotSeqRVB(obj : TObject) : integer;
begin
     if TMatrixRotateObj(obj).nru > 0 then
        ApplyPlaneRotSeqRVB(TMatrixRotateObj(obj).mm, TMatrixRotateObj(obj).nru, TMatrixRotateObj(obj).U,
                  TMatrixRotateObj(obj).LineWidthU,
                  TMatrixRotateObj(obj).w3, TMatrixRotateObj(obj).w4);

     Result := 0;
end;

function ThrApplyPlaneRotSeqLVF(obj : TObject) : integer;
begin
     if TMatrixRotateObj(obj).ncvt > 0 then
        ApplyPlaneRotSeqLVF(TMatrixRotateObj(obj).ncvt, TMatrixRotateObj(obj).mm,
                  TMatrixRotateObj(obj).vt, TMatrixRotateObj(obj).LineWidthVT,
                  TMatrixRotateObj(obj).w1, TMatrixRotateObj(obj).w2);

     Result := 0;
end;

function ThrApplyPlaneRotSeqRVF(obj : TObject) : integer;
begin
     if TMatrixRotateObj(obj).nru > 0 then
        ApplyPlaneRotSeqRVF(TMatrixRotateObj(obj).mm, TMatrixRotateObj(obj).nru,
                  TMatrixRotateObj(obj).U, TMatrixRotateObj(obj).LineWidthU,
                  TMatrixRotateObj(obj).w3, TMatrixRotateObj(obj).w4);

     Result := 0;
end;

function ThrRot_VT(obj : TObject) : integer;
begin
     if TMatrixRotateObj(obj).ncvt > 0 then
        MatrixRotate(TMatrixRotateObj(obj).ncvt, GenPtr(TMatrixRotateObj(obj).vt, 0, TMatrixRotateObj(obj).mm - 1, TMatrixRotateObj(obj).LineWidthVT), sizeof(double),
             GenPtr(TMatrixRotateObj(obj).vt, 0, TMatrixRotateObj(obj).mm, TMatrixRotateObj(obj).LineWidthVT),
             sizeof(double), TMatrixRotateObj(obj).cosr, TMatrixRotateObj(obj).sinr);

     Result := 0;
end;

procedure ThrMatrixRotate(u : PDouble; const LineWidthU : TASMNativeInt;
                       vt : PDouble; const LineWidthVT : TASMNativeInt;
                       m, nru, ncvt : TASMNativeInt; const cosl, sinl, cosr, sinr : double);
var obj : TMatrixRotateObj;
    calls : IMtxAsyncCallGroup;
begin
     obj := TMatrixRotateObj.CreateRot(U, LineWidthU, Vt, LineWidthVT,
                                   m, nru, ncvt, cosl, sinl, cosr, sinr);

     calls := MtxInitTaskGroup;
     calls.AddTask(ThrRot_VT, obj);
     if NRU > 0 then
        MatrixRotate(nru, GenPtr(u, m - 1, 0, LineWidthU), LineWidthU, GenPtr(u, m, 0, LineWidthU), LineWidthU, cosl, sinl);
     calls.SyncAll;
end;


procedure ThrMatrixRotateUpdB(aU : PDouble; const aLineWidthU : TASMNativeInt;
                              aVt : PDouble; const aLineWidthVT : TASMNativeInt;
                              aNM, anru, ancvt : TASMNativeInt;
                              aw1, aw2,
                              aw3, aw4 : PConstDoubleArr);
var obj : TMatrixRotateObj;
    calls : IMtxAsyncCallGroup;
begin
     obj := TMatrixRotateObj.Create(aU, aLineWidthU, aVt, aLineWidthVT,
                                   aNM, anru, ancvt, aw1, aw2, aw3, aw4);

     calls := MtxInitTaskGroup;
     calls.AddTask(ThrApplyPlaneRotSeqRVB, obj);
     ThrApplyPlaneRotSeqLVB(obj);
     calls.SyncAll;
end;

procedure ThrMatrixRotateUpdF(aU : PDouble; const aLineWidthU : TASMNativeInt;
                              aVt : PDouble; const aLineWidthVT : TASMNativeInt;
                              aNM, anru, ancvt : TASMNativeInt;
                              aw1, aw2,
                              aw3, aw4 : PConstDoubleArr);
var obj : TMatrixRotateObj;
    calls : IMtxAsyncCallGroup;
begin
     obj := TMatrixRotateObj.Create(aU, aLineWidthU, aVt, aLineWidthVT,
                                   aNM, anru, ancvt, aw1, aw2, aw3, aw4);

     calls := MtxInitTaskGroup;
     calls.AddTask(ThrApplyPlaneRotSeqLVF, obj);
     ThrApplyPlaneRotSeqRVF(obj);
     calls.SyncAll;
end;


function ThrMatrixSVDInPlace( A : PDouble; const LineWidthA : TASMNativeInt; width : TASMNativeInt; Height : TASMNativeInt;
                 W : PConstDoubleArr; V : PDouble; const LineWidthV : TASMNativeInt; SVDBlockSize : TASMNativeInt = 32;
                 progress : TLinEquProgress = nil) : TSVDResult;
var svdData : TMtxSVDDecompData;
    minmn : TASMNativeInt;
    aT : TDoubleDynArray;
    pa : PConstDoubleArr;
    x, y : TASMNativeInt;
begin
     if width > Height then
     begin
          // make use of A_T = (U * W * V_T)_T = (V * W_T * U_T)
          MatrixTranspose(V, LineWidthV, A, LineWidthA, width, height);
          MatrixScaleAndAdd(A, LineWidthA, width, height, 0, 0);  // clear out A

          Result := ThrMatrixSVDInPlace(V, LineWidthV, height, width, W, A, LineWidthA, SVDBlockSize, progress);
          if Result = srOk then
          begin
               aT := MatrixTranspose(A, LineWidthA, height, height);
               // copy back
               MatrixCopy(A, LineWidthA, @aT[0], height*sizeof(double), height, height);

               // set the values that are not needed any more on A to zero
               for y := 0 to height - 1 do
               begin
                    pA := PConstDoubleArr( GenPtr(A, 0, y, LineWidthA));
                    for x := height to Width - 1 do
                        pA^[x] := 0;
               end;
          end;
          exit;
     end;


     // ###########################################
     // #### Machine constants
     minmn := Min(Width, Height);

     // ###################################################
     // ##### Apply svd
     svdData.Progress := progress;
     svdData.SVDPnlSize := SVDBlockSize;
     svdData.SVDMultSize := minmn;
     svdData.QRPnlSize := QRBlockSize;
     svdData.qrDecomp := {$IFDEF FPC}@{$ENDIF}ThrMatrixQRDecomp;
     svdData.qFromQRDecomp := {$IFDEF FPC}@{$ENDIF}ThrMatrixQFromQRDecomp;
     svdData.LeftQFromQRDecomp := {$IFDEF FPC}@{$ENDIF}ThrMatrixLeftQFromQRDecomp;
     svdData.BlkMltSize := BlockMatrixCacheSize;
     svdData.MatrixMultEx := {$IFDEF FPC}@{$ENDIF}ThrMatrixMultEx;
     svdData.MatrixMultT1 := {$IFDEF FPC}@{$ENDIF}ThrMatrixMultT1Ex;
     svdDAta.MatrixMultT2 := {$IFDEF FPC}@{$ENDIF}ThrMatrixMultT2Ex;


     svdData.MatrixRotateUpdB := {$IFDEF FPC}@{$ENDIF}ThrMatrixRotateUpdB;
     svdData.MatrixRotateUpdF := {$IFDEF FPC}@{$ENDIF}ThrMatrixRotateUpdF;
     svdData.MatrixRotate := {$IFDEF FPC}@{$ENDIF}ThrMatrixRotate;

     // ###########################################
     // #### Allocate workspace for fast algorithm...
     svdData.pWorkMem := AllocMem( SVDMemSizeThr(svdData, width, height) );

     Result := InternalMatrixSVD(A, LineWidthA, width, height, W, V, LineWidthV, svdData);

     FreeMem(svdData.pWorkMem);
end;


end.
