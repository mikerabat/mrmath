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

unit TestLineEquations;

interface

uses
  {$IFDEF FPC} testregistry {$ELSE} TestFramework {$ENDIF} ,
  Classes, SysUtils, Types, OptimizedFuncs, BaseMatrixTestCase,
  MatrixConst;

type
 TTestLinearEquations = class(TBaseMatrixTestCase)
 protected
   procedure SetUp; override;
 published
  procedure TestMatrixRot;
  procedure TestApplyPlaneRot;
  procedure TestDeterminant1;
  procedure TestDeterminant2;
  procedure TestGaussJordan1;
  procedure TestLUDecomp1;
  procedure TestLUBackSubst1;
  procedure TestLUBackSubst2;
  procedure TestBigLUDecomp;
  procedure TestInvert1;
  procedure TestInvert2;
  procedure TestInvert3;

  procedure TestCholesky;
  procedure TestCholesky2;
  procedure TestCholesky3;
  procedure TestQRDecomp;
  procedure TestQRDecomp2;
  procedure TestQRDecomp3;
  procedure TestFullQRDecomp;
  procedure TestLargeFullQRDecomp;
  procedure TestAsymQRDecomp1;
  procedure TestAsymQRDecomp2;

  procedure TestSVD1;
  procedure TestSVD11;
  procedure TestSVD12;
  procedure TestSVD2;
  procedure TestSVD3;
  procedure TestSVD4;
  procedure TestSVD5;
  procedure TestBigSVD;
  procedure TestBigSVDHeightGEWidth;
  procedure TestBigSVDWidthGEHeight;

  procedure TestPseudoInversion;
  procedure TestPseudoInversion2;

  procedure TestLeasSquaresQR;
  procedure TestMatrixSolve;
 end;

type
  TAVXTestLinearEquations = class(TTestLinearEquations)
  protected
    procedure SetUp; override;
  end;

type

  { TFMATestLinearEquations }

  TFMATestLinearEquations = class(TTestLinearEquations)
  protected
    procedure SetUp; override;
  end;


implementation

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}

uses LinearAlgebraicEquations, MtxThreadPool, math, MtxTimer, 
     BlockSizeSetup, LinAlgSVD, LinAlgQR, LinAlgCholesky, LinAlgLU,
     MatrixRotations, CPUFeatures;

{ TFMATestLinearEquations }

procedure TFMATestLinearEquations.SetUp;
begin
     InitMathFunctions( itFMA, False );
end;

{ TAVXTestLinearEquations }

procedure TAVXTestLinearEquations.Setup;
begin
     InitMathFunctions( itAVX, False );
end;

{ TestTDoubleMatrix }

procedure TTestLinearEquations.TestBigLUDecomp;
const cBlkWidth = 1024;
      cBlkSize = cBlkWidth*cBlkWidth;

var x, y : TDoubleDynArray;
    i: Integer;
    idx : Array[0..cBlkWidth-1] of integer;
    start, stop : Int64;
    index : integer;
begin
     InitMtxThreadPool;

     SetLength(x, cBlkSize);
     SetLength(y, cBlkSize);

     RandSeed := 15;
     for i := 0 to cBlkSize - 1 do
         x[i] := Random - 0.5;

     x[0] := 0.4;
     x[1] := 0.7;
     x[2] := -0.2;
     x[3] := 0.8;
     x[4] := 0.1;
     x[5] := -0.1;
     x[6] := -0.7;
     x[7] := 0.3;
     x[8] := -0.2;
     x[9] := 0.25;
     x[10] := 0.15;
     x[11] := -0.11;
     x[12] := -0.79;
     x[13] := 0.34;
     x[14] := -0.25;
     x[15] := 0.44;

     index := 0;
    // WriteMatlabData('D:\x.txt', x, cBlkWidth);
     Move(x[0], y[0], sizeof(double)*length(x));

     start := MtxGetTime;
     MatrixLUDecompInPlace(@x[0], cBlkWidth*sizeof(double), cBlkWidth, @idx[0], nil);
     stop := MtxGetTime;
     Status(Format('Blocked LU decomp: %.2fms', [(stop - start)/mtxFreq*1000]));

    // WriteMatlabData('D:\xlu.txt', x, cBlkWidth);

     start := MtxGetTime;
     ThrMatrixLUDecomp(@y[0], cBlkWidth*sizeof(double), cBlkWidth, @idx[0], nil);
     stop := MtxGetTime;
     Status(Format('Threaded LU decomp: %.2fms', [(stop - start)/mtxfreq*1000]));

     FinalizeMtxThreadPool;

     Check(CheckMtxIdx(x, y, index), Format('error LU decomposition. Error at x[%d] = %.5f, y[%d] = %.5f', [index, x[index], index, y[index]]));
end;

procedure TTestLinearEquations.TestBigSVD;
const cBlkWidth = 247;
      cBlkSize = cBlkWidth*cBlkWidth;

var A, A2, W, W2, V, V2 : TDoubleDynArray;
    i: Integer;
    start1, stop1 : Int64;
    start2, stop2 : Int64;
    j : Integer;
    A3 : TDoubleDynArray;
function SVDMult(A, W, V : TDoubleDynArray) : boolean;
var i, j : integer;
    x : TDoubleDynArray;
begin
     //A*W store in A
     for i := 0 to cBlkWidth - 1 do
         for j := 0 to cBlkWidth - 1 do
             A[i*cBlkWidth + j] := A[i*cBlkWidth + j]*W[j];

     // final mutl: A*W*V'
     v2 := MatrixTranspose(@V[0], cBlkWidth*sizeof(double), cBlkWidth, cBlkWidth);
     x := MatrixMult(@A[0], @v[0], cBlkWidth, cBlkWidth, cBlkWidth, cBlkWidth, cBlkWidth*sizeof(double), cBlkWidth*sizeof(double));

     Result := CheckMtx(X, A3);
end;
begin
     InitMathFunctions(itSSE, False);
     SetLength(A, cBlkSize);
     SetLength(V, cBlkSize);
     SetLength(W, cBlkWidth);
     SetLength(V2, cBlkSize);
     SetLength(W2, cBlkWidth);

     RandSeed := 15;
     for i := 0 to cBlkWidth - 1 do
     begin
          for j := 0 to cBlkWidth - 1 do
              A[i*cBlkWidth + j] := Random - 0.5;
     end;
     
     A2 := Copy(A, 0, Length(A));
     A3 := Copy(A, 0, Length(A));

//     WriteMatlabData('D:\svdinp.txt', A3, cBlkWidth);
     
     QRBlockSize := cBlkWidth;
     SVDBlockSize := cBlkWidth;

     start1 := MtxGetTime;
     MatrixSVDInPlace2(@A2[0], cBlkWidth*sizeof(double), cBlkWidth, cBlkWidth, PConstDoublearr(@W2[0]), @V2[0], cBlkWidth*sizeof(double), SVDBlockSize, nil);
     stop1 := MtxGetTime;

     SVDBlockSize := 24;
     QRBlockSize := 32;

     start2 := MtxGetTime;
     MatrixSVDInPlace2(@A[0], cBlkWidth*sizeof(double), cBlkWidth, cBlkWidth, PConstDoublearr(@W[0]), @V[0], cBlkWidth*sizeof(double), SVDBlockSize, nil);
     stop2 := MtxGetTime;

     Status(Format('BigSVD took: %.3fms, %.3fms', [(stop1 - start1)/mtxfreq*1000, (stop2 - start2)/mtxfreq*1000]));
     
//     WriteMatlabData('D:\U1.txt', A, cBlkWidth);
//     WriteMatlabData('D:\W1.txt', W, 1);
//     WriteMatlabData('D:\V1.txt', V, cBlkWidth);
//
//     WriteMatlabData('D:\U2.txt', A2, cBlkWidth);
//     WriteMatlabData('D:\W2.txt', W2, 1);
//     WriteMatlabData('D:\V2.txt', V2, cBlkWidth);

     // check the back multiplied matrices
     Check( CheckMtx(W2, W, -1, -1, 1e-6), 'Blocked SVD version failed in W');
     Check( SVDMult(A2, W2, V2), 'Error non blocked SVD');
     Check( SVDMult(A, W, V), 'Error blocked SVD');
end;

procedure TTestLinearEquations.TestBigSVDHeightGEWidth;
const cBlkWidths : Array[0..7] of integer = (1, 9, 16, 24, 43, 189, 256, 500);//, 1024);
      cBlkHeights : Array[0..7] of integer = (1, 13, 26, 123, 331, 223, 256, 543);//, 2048);

var A, A2, A3, W, W2, W3, V, V2, V3 : TDoubleDynArray;
    ARef : TDoubleDynArray;
    i: Integer;
    blkWidth : integer;
    blkHeight : integer;
    blkSize : integer;
    start1, stop1 : Int64;
    start2, stop2 : Int64;
    start3, stop3 : Int64;
    j : Integer;
    iter : integer;
    origInstrSet : TCPUInstrType;
function SVDMult(A, W, V : TDoubleDynArray) : boolean;
var i, j : integer;
    x : TDoubleDynArray;
begin
     //A*W store in A
     for i := 0 to BlkHeight - 1 do
         for j := 0 to BlkWidth - 1 do
             A[i*BlkWidth + j] := A[i*BlkWidth + j]*W[j];

     // final mutl: A*W*V'
     x := MatrixMult(@A[0], @v[0], BlkWidth, BlkHeight, BlkWidth, BlkWidth, BlkWidth*sizeof(double), BlkWidth*sizeof(double));

     Result := CheckMtx(X, ARef);
end;
begin
     origInstrSet := GetCurCPUInstrType;
     InitMtxThreadPool;
     try
        for iter := 0 to 2*Length(cBlkWidths) - 1 do
        begin
             if iter mod 2 = 0
             then
                 InitMathFunctions(itFPU, False)
             else
                 InitMathFunctions(origInstrSet, False);

             blkWidth := cBlkWidths[iter div 2];
             blkHeight := cBlkHeights[iter div 2];
             blkSize := blkWidth*blkHeight;

             SetLength(A, BlkSize);
             SetLength(V, BlkWidth*BlkWidth);
             SetLength(W, BlkWidth);
             SetLength(V2, BlkWidth*BlkWidth);
             SetLength(W2, BlkWidth);
             SetLength(V3, BlkWidth*BlkWidth);
             SetLength(W3, BlkWidth);

             RandSeed := 15;
             for i := 0 to BlkHeight - 1 do
             begin
                  for j := 0 to BlkWidth - 1 do
                      A[i*BlkWidth + j] := Random - 0.5;
             end;

             A2 := Copy(A, 0, Length(A));
             A3 := Copy(A, 0, Length(A));
             ARef := Copy(A, 0, Length(A));

             //WriteMatlabData('D:\svdinp.txt', A3, BlkWidth);

             QRBlockSize := BlkWidth;
             SVDBlockSize := BlkWidth;

             start1 := MtxGetTime;
             MatrixSVDInPlace2(@A2[0], BlkWidth*sizeof(double), BlkWidth, BlkHeight, PConstDoublearr(@W2[0]), @V2[0], BlkWidth*sizeof(double), BlkWidth, nil);
             stop1 := MtxGetTime;

             SVDBlockSize := 24;
             QRBlockSize := 32;

             start2 := MtxGetTime;
             MatrixSVDInPlace2(@A[0], BlkWidth*sizeof(double), BlkWidth, BlkHeight, PConstDoublearr(@W[0]), @V[0], BlkWidth*sizeof(double), SVDBlockSize, nil);
             stop2 := MtxGetTime;

             start3 := MtxGetTime;
             ThrMatrixSVDInPlace(@A3[0], BlkWidth*sizeof(double), BlkWidth, BlkHeight, PConstDoublearr(@W3[0]), @V3[0], BlkWidth*sizeof(double), SVDBlockSize, nil);
             stop3 := MtxGetTime;

             Status(Format('BigSVD took (%d, %d): %.3fms, %.3fms, %.3fms', [blkWidth, blkHeight, (stop1 - start1)/mtxfreq*1000, (stop2 - start2)/mtxfreq*1000, (stop3 - start3)/mtxfreq*1000]));

            // WriteMatlabData('D:\U2.txt', A, BlkWidth);
   //          WriteMatlabData('D:\W2.txt', W, 1);
   //          WriteMatlabData('D:\V2.txt', V, BlkWidth);
   //
   //          WriteMatlabData('D:\U1.txt', A2, BlkWidth);
   //          WriteMatlabData('D:\W1.txt', W2, 1);
   //          WriteMatlabData('D:\V1.txt', V2, BlkWidth);

             // check the back multiplied matrices
             Check( CheckMtx(W2, W, -1, -1, 1e-6), 'Iter: ' + intToStr(iter) + ' - Blocked SVD version failed in W');
             Check( SVDMult(A2, W2, V2), 'Iter: ' + intToStr(iter) + ' - Error non blocked SVD');
             Check( SVDMult(A, W, V), 'Iter: ' + intToStr(iter) + ' - Error blocked SVD');
             Check( SVDMult(A3, W3, V3), 'Iter: ' + intToStr(iter) + ' - Error threaded SVD');

             // note: the matrices a and v may have different signs (the final multiplication works though) -> compare the absolute values only
             MatrixAbs(@A2[0], BlkWidth*sizeof(double), BlkWidth, BlkHeight);
             MatrixAbs(@A[0], BlkWidth*sizeof(double), BlkWidth, BlkHeight);
             MatrixAbs(@V2[0], BlkWidth*sizeof(double), BlkWidth, BlkWidth);
             MatrixAbs(@V[0], BlkWidth*sizeof(double), BlkWidth, BlkWidth);
             Check( CheckMtx(A2, A, -1, -1, 1e-6), 'Iter: ' + intToStr(iter) + ' - Blocked SVD version failed in A');
             Check( CheckMtx(V2, V, -1, -1, 1e-6), 'Iter: ' + intToStr(iter) + ' - Blocked SVD version failed in V');
        end;
     finally
            FinalizeMtxThreadPool;
     end;
end;

procedure TTestLinearEquations.TestBigSVDWidthGEHeight;
const cBlkHeights : Array[0..7] of integer = (1, 9, 16, 24, 43, 189, 256, 500); //, 1024);
      cBlkWidths : Array[0..7] of integer = (1, 13, 26, 123, 331, 223, 256, 543); //, 2048);

var A, A2, A3, W, W2, W3, V, V2, V3 : TDoubleDynArray;
    ARef : TDoubleDynArray;
    i: Integer;
    blkWidth : integer;
    blkHeight : integer;
    blkSize : integer;
    start1, stop1 : Int64;
    start2, stop2 : Int64;
    start3, stop3 : Int64;
    j : Integer;
    iter : integer;
    origInstrSet : TCPUInstrType;
    s1, s2 :int64;
function SVDMult(A, W, V : TDoubleDynArray) : boolean;
var i, j : integer;
    x : TDoubleDynArray;
    minMN : integer;
begin
     minMN := Min(blkWidth, blkHeight);
     //A*W store in A
     for i := 0 to minMN - 1 do
         for j := 0 to minMN - 1 do
             A[i*blkWidth + j] := A[i*blkWidth + j]*W[j];

     // final mutl: A*W*V'
     x := MatrixMult(@A[0], @v[0], blkHeight, blkHeight, blkWidth, blkHeight, blkWidth*sizeof(double), blkWidth*sizeof(double));

     Result := CheckMtx(X, ARef);

     if not result then
     begin
          WriteMatlabData('D:\aref.txt', aref, blkWidth);
          WriteMatlabData('D:\xmul.txt', x, blkWidth);
          WriteMatlabData('D:\a.txt', a, blkheight);
          WriteMatlabData('D:\v.txt', v, blkWidth);
     end;
end;
begin
     s1 := timeInSync;
     origInstrSet := GetCurCPUInstrType;
     InitMtxThreadPool;

     try
        for iter := 0 to 2*Length(cBlkWidths) - 1 do
        begin
             if iter mod 2 = 0
             then
                 InitMathFunctions(itFPU, False)
             else
                 InitMathFunctions(origInstrSet, False);

             blkWidth := cBlkWidths[iter div 2];
             blkHeight := cBlkHeights[iter div 2];
             blkSize := blkWidth*blkHeight;

             SetLength(A, BlkSize);
             SetLength(V, BlkSize);
             SetLength(W, blkHeight);
             SetLength(V2, BlkSize);
             SetLength(W2, blkHeight);
             SetLength(V3, BlkSize);
             SetLength(W3, blkHeight);

             RandSeed := 15;
             for i := 0 to BlkHeight - 1 do
             begin
                  for j := 0 to BlkWidth - 1 do
                      A[i*BlkWidth + j] := Random - 0.5;
             end;

             A2 := Copy(A, 0, Length(A));
             A3 := Copy(A, 0, Length(A));
             ARef := Copy(A, 0, Length(A));

             //WriteMatlabData('D:\svdinp.txt', A3, BlkWidth);

             QRBlockSize := BlkWidth;
             SVDBlockSize := BlkWidth;

             start1 := MtxGetTime;
             MatrixSVDInPlace2(@A2[0], BlkWidth*sizeof(double), BlkWidth, BlkHeight, PConstDoublearr(@W2[0]), @V2[0], blkWidth*sizeof(double), BlkWidth, nil);
             stop1 := MtxGetTime;

             SVDBlockSize := 24;
             QRBlockSize := 32;

             start2 := MtxGetTime;
             MatrixSVDInPlace2(@A[0], BlkWidth*sizeof(double), BlkWidth, BlkHeight, PConstDoublearr(@W[0]), @V[0], blkWidth*sizeof(double), SVDBlockSize, nil);
             stop2 := MtxGetTime;

             start3 := MtxGetTime;
             ThrMatrixSVDInPlace(@A3[0], BlkWidth*sizeof(double), BlkWidth, BlkHeight, PConstDoublearr(@W3[0]), @V3[0], blkWidth*sizeof(double), SVDBlockSize, nil);
             stop3 := MtxGetTime;


             Status(Format('BigSVD took (%d, %d): %.3fms, %.3fms, %.3fms', [blkWidth, blkHeight, (stop1 - start1)/mtxfreq*1000, (stop2 - start2)/mtxfreq*1000, (stop3 - start3)/mtxfreq*1000]));

             //WriteMatlabData('D:\U2.txt', A, BlkWidth);
   //          WriteMatlabData('D:\W2.txt', W, 1);
   //          WriteMatlabData('D:\V2.txt', V, BlkWidth);
   //
   //          WriteMatlabData('D:\U1.txt', A2, BlkWidth);
   //          WriteMatlabData('D:\W1.txt', W2, 1);
   //          WriteMatlabData('D:\V1.txt', V2, BlkWidth);

             // check the back multiplied matrices
             Check( CheckMtx(W2, W, -1, -1, 1e-6), 'Iter: ' + intToStr(iter) + ' - Blocked SVD version failed in W');
             Check( SVDMult(A2, W2, V2), 'Iter: ' + intToStr(iter) + ' - Error non blocked SVD');
             Check( SVDMult(A, W, V), 'Iter: ' + intToStr(iter) + ' - Error blocked SVD');
             Check( SVDMult(A3, W3, V3), 'Iter: ' + intToStr(iter) + ' - Error threaded SVD');

             // note: the matrices a and v may have different signs (the final multiplication works though) -> compare the absolute values only
             MatrixAbs(@A2[0], BlkWidth*sizeof(double), BlkWidth, BlkHeight);
             MatrixAbs(@A[0], BlkWidth*sizeof(double), BlkWidth, BlkHeight);
             MatrixAbs(@V2[0], BlkWidth*sizeof(double), BlkWidth, BlkHeight);
             MatrixAbs(@V[0], BlkWidth*sizeof(double), BlkWidth, BlkHeight);
             Check( CheckMtx(A2, A, -1, -1, 1e-6), 'Iter: ' + intToStr(iter) + ' - Blocked SVD version failed in A');
             Check( CheckMtx(V2, V, -1, -1, 1e-6), 'Iter: ' + intToStr(iter) + ' - Blocked SVD version failed in V');
        end;
     finally
            FinalizeMtxThreadPool;
     end;

     s2 := timeInSync;


     Status(Format('CreateEvent code took %.3fms', [(s2 - s1)/mtxfreq*1000]));
end;


procedure TTestLinearEquations.TestCholesky;
const A : Array[0..8] of double = (1, 2, 2, -1, 4, -2, 2, 2, -1);
      B : Array[0..2] of double = (2, -1, -1);
      cResX : Array[0..2] of double = (0.338888889, 0.00555555556, -0.2666666667);

var AL : TDoubleDynArray;
    X : TDoubleDynArray;
    P : Array[0..8] of double;
    res : TCholeskyResult;
    ResX : Array[0..2] of double;
begin
     AL := MatrixTranspose(@A[0], 3*sizeof(double), 3, 3);
     X := MatrixMult(@A[0], @AL[0], 3, 3, 3, 3, 3*sizeof(double), 3*sizeof(double));

     FillChar(P[0], sizeof(P), 0);
     res := MatrixCholeskyInPlace(@X[0], 3*sizeof(double), 3, @P[0], 4*sizeof(double));
     Check(res = crOk, WriteMtx(X, 3));

     Move(A, X[0], sizeof(A));
     FillChar(P[0], sizeof(P), 0);
     res := MatrixCholeskyInPlace(@X[0], 3*sizeof(double), 3, @P[0], 4*sizeof(double));

     Check(res = crNoPositiveDefinite, WriteMtx(X, 3));

     // check solver
     X := MatrixMult(@A[0], @AL[0], 3, 3, 3, 3, 3*sizeof(double), 3*sizeof(double));
     MatrixCholeskyInPlace(@X[0], 3*sizeof(double), 3, @P[0], 4*sizeof(double));
     MatrixCholeskySolveLinEq(@X[0], 3*sizeof(double), 3, @P[0], 4*sizeof(double), @B[0], sizeof(double), @ResX[0], sizeof(double));

     Check(CheckMtx(ResX, cResX), 'Cholesky error');
end;

procedure TTestLinearEquations.TestCholesky2;
const cA : Array[0..8] of double = (1, 2, 2, -1, 4, -2, 2, 2, -1);
      cA2 : Array[0..15] of double = (1, 2, 2, 0.5,
                                   -1, 4, -2, -0.1,
                                   2, 2, -1, 1.2,
                                   3, 2, -1, -1.1);
var A : Array[0..8] of double;
    AL, X, X1 : TDoubleDynArray;
    res : TCholeskyResult;
begin
     AL := MatrixTranspose(@cA[0], 3*sizeof(double), 3, 3);
     X := MatrixMult(@cA[0], @AL[0], 3, 3, 3, 3, 3*sizeof(double), 3*sizeof(double));
     X1 := copy(X, 0, Length(x));

     res := MatrixCholeskyInPlace4(@X1[0], 3*sizeof(double), 3, 2);

     Check(res = crOk, 'Cholesky failed');
     
     Status(WriteMtx(x1, 3));
     
     res := MatrixCholeskyInPlace2(@X[0], 3*sizeof(double), 3);

     Status(WriteMtx(X, 3));
     Check(res = crOk, WriteMtx(x, 3));


     AL := MatrixTranspose(@cA2[0], 4*sizeof(double), 4, 4);
     X := MatrixMult(@cA2[0], @AL[0], 4, 4, 4, 4, 4*sizeof(double), 4*sizeof(double));

     res := MatrixCholeskyInPlace2(@X[0], 4*sizeof(double), 4);

     Status(WriteMtx(X, 4));
     Check(res = crOk, WriteMtx(x, 4));

     AL := MatrixTranspose(@cA2[0], 4*sizeof(double), 4, 4);
     X := MatrixMult(@cA2[0], @AL[0], 4, 4, 4, 4, 4*sizeof(double), 4*sizeof(double));

     res := MatrixCholeskyInPlace4(@X[0], 4*sizeof(double), 4, 2);

     Status(WriteMtx(X, 4));
     Check(res = crOk, WriteMtx(x, 4));
     
     Move(cA, A[0], sizeof(cA));
     res := MatrixCholeskyInPlace2(@A[0], 3*sizeof(double), 3, nil);
     Check(res = crNoPositiveDefinite, WriteMtx(A, 3));
end;

procedure TTestLinearEquations.TestCholesky3;
const cLargeCholSize = 2048;
var x, y : TDoubleDynArray;
    p1, p2 : PDouble;
    a, a1, a2, a3 : TDoubleDynArray;
    res : TCholeskyResult;
    start, stop : Int64;
begin
     InitMtxThreadPool;

     FillMatrix(cLargeCholSize*cLargeCholSize, x, y, p1, p2);
     FreeMem(p1);
     FreeMem(p2);

     y := MatrixTranspose(@x[0], cLargeCholSize*sizeof(double), cLargeCholSize, cLargeCholSize);

     a := MatrixMult(x, y, cLargeCholSize, cLargeCholSize, cLargeCholSize, cLargeCholSize);

//     WriteMatlabData('D:\cholIn.txt', a, cLargeCholSize);
     a1 := Copy(a, 0, Length(a));
     a2 := Copy(a, 0, Length(a));
     a3 := Copy(a, 0, Length(a));

     start := MtxGetTime;
     res := MatrixCholeskyInPlace2(@a[0], cLargeCholSize*sizeof(double), cLargeCholSize);
     stop := MtxGetTime;
     Status(Format('Cholesky large %d: %.2fms', [cLargeCholSize, (stop - start)/mtxfreq*1000]));

     Check(res = crOk, 'Error large cholesky failed');

     start := MtxGetTime;
     res := MatrixCholeskyInPlace3(@a1[0], cLargeCholSize*sizeof(double), cLargeCholSize);
     stop := MtxGetTime;                            
     Status(Format('Cholesky large recursive %d: %.2fms', [cLargeCholSize, (stop - start)/mtxfreq*1000]));

     Check(res = crOk, 'Error large cholesky failed');

     start := MtxGetTime;
     res := MatrixCholeskyInPlace4(@a2[0], cLargeCholSize*sizeof(double), cLargeCholSize, 24);
     stop := MtxGetTime;                            
     Status(Format('Cholesky large panel: %.2fms', [(stop - start)/mtxfreq*1000]));

     Check(res = crOk, 'Error large cholesky failed');

     start := MtxGetTime;
     res := ThrMatrixCholeskyDecomp(@a3[0], cLargeCholSize*sizeof(double), cLargeCholSize, 24, nil, nil);
     stop := MtxGetTime;                            
     Status(Format('Cholesky threaded: %.2fms', [(stop - start)/mtxfreq*1000]));

     Check(res = crOk, 'Error in threaded cholesky decomp');
     
     Check(CheckMtx(a, a2), 'Panel implementation of Cholesky decomposition failed');
     Check(CheckMtx(a, a1), 'Recursive implementation of Cholesky decomposition failed');
     Check(CheckMtx(a, a3), 'Threaded implementation of Cholesky decomposition failed');
     
//     WriteMatlabData('D:\cholOut.txt', a, cLargeCholSize);
//     WriteMatlabData('D:\cholOut1.txt', a1, cLargeCholSize);
//     WriteMatlabData('D:\cholOut2.txt', a2, cLargeCholSize);

     FinalizeMtxThreadPool;
end;

procedure TTestLinearEquations.TestDeterminant1;
const A : Array[0..3] of double = (1, 2, 2, -1);
var det : double;
begin
     det := MatrixDeterminant(@A[0], 2*sizeof(double), 2);

     Check(det = -5, 'Error determinant differs: ' + Format('%.3f', [det]));
end;

procedure TTestLinearEquations.TestDeterminant2;
const cBlkWidth = 256;
      cBlkSize = cBlkWidth*cBlkWidth;
var x : TDoubleDynArray;
    i: Integer;
    start, stop : Int64;
    det, detThr : double;
begin
     InitMtxThreadPool;
     // big inversion
     SetLength(x, cBlkSize);
     RandSeed := 15;
     for i := 0 to cBlkSize - 1 do
         x[i] := Random/3.8 - 0.5/3.8;

     //WriteMatlabData('d:\x1.txt', x, cBlkWidth);

     start := MtxGetTime;
     det := MatrixDeterminant(@x[0], cBlkWidth*sizeof(double), cBlkWidth);
     Check(det <> 0, 'Singular matrix');
     stop := MtxGetTime;
     Status(Format('Big determinant: %.2fms with value %.5f', [(stop - start)/mtxfreq*1000, det]));

     start := MtxGetTime;
     detThr := ThrMatrixDeterminant(@x[0], cBlkWidth*sizeof(double), cBlkWidth);
     Check(detThr <> 0, 'Singular matrix');
     stop := MtxGetTime;

     Status(Format('Big threaded determinant: %.2fms with value %.5f', [(stop - start)/mtxfreq*1000, detThr]));

     FinalizeMtxThreadPool;

     Check(SameValue(det, detThr), 'Error Determinants differ too much');
end;

procedure TTestLinearEquations.TestGaussJordan1;
const A : Array[0..3] of double = (1, 2, 2, -1);
      B : Array[0..1] of double = (1, 1);
      D : Array[0..3] of double = (1, 2, 2, 4);
var invA : Array[0..3] of double;
    solvedX : Array[0..1] of double;
    res : TLinEquResult;
begin
     Move(A, invA, sizeof(A));
     Move(B, solvedX, sizeof(B));

     res := MatrixGaussJordanInPlace(@invA[0], 2*sizeof(double), @solvedX[0], sizeof(double), 2, 1);

     Check(res = leOk, 'Singularity detected');

     Move(D, invA, sizeof(A));
     Move(B, solvedX, sizeof(B));
     res := MatrixGaussJordanInPlace(@invA[0], 2*sizeof(double), @solvedX[0], sizeof(double), 2, 1);

     Check(res = leSingular, 'Singularity missed');     
end;

procedure TTestLinearEquations.TestInvert1;
const A : Array[0..3] of double = (1, 2, 2, -1);
      AInv : Array[0..3] of double = (0.2, 0.4, 0.4, -0.2);
var Inv : Array[0..3] of double;
    res : TLinEquResult;
begin
     Move(A, Inv, sizeof(A));

     res := MatrixInverseInPlace(@Inv[0], 2*sizeof(double), 2);
     Check(res = leok, 'Singularity detected!');
     CheckEqualsMem(@AInv[0], @inv[0], sizeof(AInv), 'Error inverting Mtx: ' + WriteMtx(inv, 2));
end;

procedure TTestLinearEquations.TestInvert2;
const A : Array[0..8] of double = (1, 2, 3, 2, 4, 6, 3, 3, -1);
var inv : Array[0..8] of double;
begin
     move(A, inv, sizeof(a));

     Check(MatrixInverseInPlace(@inv[0], 3*sizeof(double), 3) = leSingular, 'Matrix inversion seems not to be singular');
end;

procedure TTestLinearEquations.TestInvert3;
const cBlkWidth = 512;
      cBlkSize = cBlkWidth*cBlkWidth;
var x, y : TDoubleDynArray;
    i: Integer;
    start, stop : Int64;
begin
     InitMtxThreadPool;
     // big inversion
     SetLength(x, cBlkSize);
     RandSeed := 15;
     for i := 0 to cBlkSize - 1 do
         x[i] := Random - 0.5;

     //WriteMatlabData('D:\x1.txt', x, cBlkWidth);

     y := Copy(x, 0, Length(x));

     start := MtxGetTime;
     Check(MatrixInverseInPlace(@x[0], cBlkWidth*sizeof(double), cBlkWidth) <> leSingular, 'Matrix inversion seems not to be singular');
     stop := MtxGetTime;
     Status(Format('Big Inversion: %.2fms', [(stop - start)/mtxfreq*1000]));

     start := MtxGetTime;
     Check(ThrMatrixInverse(@y[0], cBlkWidth*sizeof(double), cBlkWidth) <> leSingular, 'Matrix inversion seems not to be singular');
     stop := MtxGetTime;
     Status(Format('Big Inversion Threaded: %.2fms', [(stop - start)/mtxfreq*1000]));

     //WriteMatlabData('D:\invx.txt', x, cBlkWidth);
     FinalizeMtxThreadPool;
     check(checkMtx(x, y), 'Error inversion of the threaded version is not the same as the single thread one');
end;

procedure TTestLinearEquations.TestLUBackSubst1;
const A : Array[0..3] of double = (1, 2, 2, -1);
      C : Array[0..3] of double = (2, -1, 0.5, 2.5);
      D : Array[0..1] of double = (0, 1);
var LUDecomp : Array[0..3] of double;
    res : TLinEquResult;
    idx : Array[0..1] of integer;
    B : Array[0..1] of double;
begin
     Move(A, LUDecomp, sizeof(A));

     idx[0] := 0;
     idx[1] := 0;
     res := MatrixLUDecompInPlace(@LUDecomp[0], 2*sizeof(double), 2, @idx[0]);

     Check(res = leOk, 'Singularity detected!');
     CheckEqualsMem(@C[0], @LUDecomp[0], sizeof(C), 'LU decomp failed with: ' + WriteMtx(LUDecomp, 2));

     // back substitute
     B[0] := 2;
     B[1] := -1;
     MatrixLUBackSubst(@LUDecomp[0], 2*sizeof(double), 2, @idx[0], @B[0], sizeof(double));

     CheckEqualsMem(@D[0], @B[0], sizeof(D), 'Error solving Mtx: ' + WriteMtx(B, 1));
end;

procedure TTestLinearEquations.TestLUBackSubst2;
const A : Array[0..5] of double = (1, 2, 0, 2, -1, 0);
      C : Array[0..5] of double = (2, -1, 0, 0.5, 2.5, 0);
      D : Array[0..3] of double = (0, 0, 1, 0);
var LUDecomp : Array[0..5] of double;
    res : TLinEquResult;
    idx : Array[0..1] of integer;
    B : Array[0..3] of double;
begin
     Move(A, LUDecomp, sizeof(A));

     idx[0] := 0;
     idx[1] := 0;
     res := MatrixLUDecompInPlace(@LUDecomp[0], 3*sizeof(double), 2, @idx[0]);

     Check(res = leOk, 'Singularity detected!');
     CheckEqualsMem(@C[0], @LUDecomp[0], sizeof(C), 'LU decomp failed with: ' + WriteMtx(LUDecomp, 2));

     // back substitute
     B[0] := 2;
     B[1] := 0;
     B[2] := -1;
     B[3] := 0;
     MatrixLUBackSubst(@LUDecomp[0], 3*sizeof(double), 2, @idx[0], @B[0], 2*sizeof(double));

     CheckEqualsMem(@D[0], @B[0], sizeof(D), 'Error solving Mtx: ' + WriteMtx(B, 1));
end;

procedure TTestLinearEquations.TestLUDecomp1;
const A : Array[0..3] of double = (1, 2, 2, -1);
      B : Array[0..3] of double = (2, -1, 0.5, 2.5);
var LUDecomp : Array[0..3] of double;
    res : TLinEquResult;
    idx : Array[0..1] of integer;
    C : TDoubleDynArray;
begin
     Move(A, LUDecomp, sizeof(A));

     res := MatrixLUDecompInPlace(@LUDecomp[0], 2*sizeof(double), 2, @idx[0]);

     Check(res = leOk, 'Singularity detected!');
     CheckEqualsMem(@b[0], @LUDecomp[0], sizeof(B), 'LU decomp failed with: ' + WriteMtx(LUDecomp, 2));

     SetLength(C, 4);
     res := MatrixLUDecomp(@A[0], 2*sizeof(double), @C[0], 2*sizeof(double), 2);
     Check(res = leOk, 'Singularity detected!');
     CheckEqualsMem(@B[0], @C[0], sizeof(B), 'LU decomp failed with: ' + WriteMtx(LUDecomp, 2));
end;

procedure TTestLinearEquations.TestMatrixSolve;
const cBlkWidth = 48;
      cBlkSize = cBlkWidth*cBlkWidth;
var a, x1, x2, b : TDoubleDynArray;
    i : integer;
    start, stop : int64;
    index : integer;
begin
     InitMtxThreadPool;

     SetLength(a, cBlkSize);
     SetLength(b, 3*cBlkWidth);
     SetLength(x1, 3*cBlkWidth);
     SetLength(x2, 3*cBlkWidth);

     RandSeed := 15;
     for i := 0 to cBlkSize - 1 do
         a[i] := Random - 0.5;

     for i := 0 to 3*cBlkWidth - 1 do
         b[i] := Random - 0.5;


     start := MtxGetTime;
     MatrixLinEQSolve(@a[0], cBlkWidth*sizeof(double), cBlkWidth, @b[0], 3*sizeof(double), @x1[0], 3*sizeof(double), 3, 0);
     stop := MtxGetTime;
     Status(Format('Blocked LU decomp: %.2fms', [(stop - start)/mtxfreq*1000]));

     start := MtxGetTime;
     ThrMatrixLinEQSolve(@a[0], cBlkWidth*sizeof(double), cBlkWidth, @b[0], 3*sizeof(double), @x2[0], 3*sizeof(double), 3);
     stop := MtxGetTime;
     Status(Format('Threaded LU decomp: %.2fms', [(stop - start)/mtxfreq*1000]));

     FinalizeMtxThreadPool;

     index := 0;
     Check(CheckMtxIdx(x1, x2, index), Format('error Lin equation solve. Error at x[%d] = %.5f, y[%d] = %.5f', [index, x1[index], index, x2[index]]));
end;
 
procedure TTestLinearEquations.TestPseudoInversion;
const A : Array[0..9] of double = (1, 1, 2, -2, 3, -1, -1, 2, -2, 3);
      Res : Array[0..9] of double = (0.2000, 0.0606, 0.2606, 0.0545, 0.0242, 0.2000, -0.0606, 0.1394, 0.1455, 0.1758);
      Res1 : Array[0..9] of double = (0.2500, -0.2500, 0.2500, -0.2500, 0.0588, 0.0588, -0.0588, -0.0588, 0.0882, 0.0882);
var dest : Array[0..9] of double;
    dest2 : Array[0..9] of double;
    Ain : Array[0..9] of double;
begin
     Move(A, Ain, sizeof(A));

     Check(MatrixPseudoinverse(@dest[0], 5*sizeof(double), @Ain[0], 2*sizeof(Double),  2, 5) = srOk, 'Error Pseudo Inversion: ' + WriteMtx(A, 2));
     Check(CheckMtx(Dest, Res), 'Error Pseudo Inversion calculation: ' + WriteMtx(dest, 5));

     Move(A, Ain, sizeof(A));
     Check(MatrixPseudoinverse2(@dest2[0], 5*sizeof(double), @Ain[0], 2*sizeof(Double),  2, 5) = srOk, 'Error Pseudo Inversion: ' + WriteMtx(A, 2));
     Check(CheckMtx(Dest2, res), 'Error second pseudo inversion: ' + WriteMtx(dest2, 5) );
     
     Move(A, Ain, sizeof(A));
     Check(MatrixPseudoinverse(@dest[0], 2*sizeof(double), @Ain[0], 5*sizeof(double), 5, 2) = srOk, 'Error Pseudo Inversion: ' + WriteMtx(A, 2));
     Check(CheckMtx(Dest, Res1), 'Error Pseudo Inversion calculation: ' + WriteMtx(dest, 5));

     Move(A, Ain, sizeof(A));
     Check(MatrixPseudoinverse2(@dest[0], 2*sizeof(double), @Ain[0], 5*sizeof(double), 5, 2) = srOk, 'Error Pseudo Inversion: ' + WriteMtx(A, 2));
     Check(CheckMtx(Dest, Res1), 'Error Pseudo Inversion calculation: ' + WriteMtx(dest, 5));
end;

procedure TTestLinearEquations.TestPseudoInversion2;
var A : TDoubleDynArray;
    B : TDoubleDynArray;
    C : TDoubleDynArray;
    i : Integer;
begin
     SetLength(A, 25*5);
     for i := 0 to Length(A) - 1 do
     begin
          A[i] := i + 1;
     end;

     SetLength(B, 25*5);
     Check(MatrixPseudoinverse(@B[0], 25*sizeof(double), @A[0], 5*sizeof(double), 5, 25) = srOk, 'Error Pseudoinversion failed');

     SetLength(A, 25*5);
     for i := 0 to Length(A) - 1 do
     begin
          A[i] := i + 1;
     end;

     // check if A*pinv(A)*A = A
     C := MatrixMult(A, B, 5, 25, 25, 5);
     C := MatrixMult(C, A, 25, 25, 5, 25);

     Check(CheckMtx(C, A), 'Error Pseudoinversion Test A*pinv(A)*A = A failed');

     // ###########################################
     // #### Second function
     SetLength(A, 25*5);
     for i := 0 to Length(A) - 1 do
     begin
          A[i] := i + 1;
     end;

     B := nil;
     SetLength(B, 25*5);
     Check(MatrixPseudoinverse2(@B[0], 25*sizeof(double), @A[0], 5*sizeof(double), 5, 25) = srOk, 'Error Pseudoinversion failed');

     SetLength(A, 25*5);
     for i := 0 to Length(A) - 1 do
     begin
          A[i] := i + 1;
     end;

     // check if A*pinv(A)*A = A
     C := MatrixMult(A, B, 5, 25, 25, 5);
     C := MatrixMult(C, A, 25, 25, 5, 25);

     Check(CheckMtx(C, A), 'Error Pseudoinversion Test A*pinv(A)*A = A failed');
end;

procedure TTestLinearEquations.TestQRDecomp;
const A : Array[0..8] of double = (1, 2, 2, -1, 4, -2, 2, 2, -1);
      BIn : Array[0..2] of double = (1, -1, 2);
      Bexp : Array[0..2] of double = (1, 0, 0);
var dest : Array[0..8] of double;
    D : Array[0..2] of double;
    C : Array[0..2] of double;
    B : Array[0..2] of double;
begin
     Move(Bin, B, sizeof(Bin));
     MatrixQRDecomp(@dest[0], 3*sizeof(double), @A[0], 3*sizeof(double), 3, @C[0], sizeof(double), @D[0], sizeof(double));
     MatrixQRSolveLinEq(@dest[0], 3*sizeof(double), 3, @C[0], sizeof(double), @D[0], sizeof(double), @B[0], sizeof(double));

     Check(CheckMtx(Bexp, B), 'Error QR decomposistion: ' + WriteMtx(B, 3));
end;

procedure TTestLinearEquations.TestQRDecomp2;
const A : Array[0..15] of double = (1, 2, 2, 0.5,
                                   -1, 4, -2, -0.1,
                                   2, 2, -1, 1.2,
                                   3, 2, -1, -1.1);
var dest : Array[0..15] of double;
    dest2 : Array[0..15] of double;
    tau : Array[0..3] of double;
    work : Array[0..3] of double;
    i, j : integer;
begin
     Move(A, dest, sizeof(A));
     MatrixQRDecompInPlace2(@dest[0], 4*sizeof(double), 4, 4, @tau[0], nil, 4);

     Move(A, dest2, sizeof(A));
     MatrixQRDecompInPlace(@dest2[0], 4*sizeof(double), 4, @tau[0], sizeof(double), @work[0], sizeof(double));

     // overwrite diagonal elements of dest2 with D
     for i := 0 to 3 do
         dest2[i + 4*i] := work[i];


     // compare only matrix R - the two algorithms differ on Q
     for i := 0 to 3 do
     begin
          for j := i to 3 do
              Check(SameValue(dest[i*4 + j], dest2[i*4 + j], 1e-8));
     end;
end;


procedure TTestLinearEquations.TestQRDecomp3;
var a : TDoubleDynArray;
    tau : TDoubleDynArray;
const cSize = 6;
      cA : Array[0..cSize*cSize - 1] of double = (1, 2, 2, 0.5, -1, -2,
                                   -1, 4, -2, -0.1, 0.1, -1,
                                   2, 2, -1, 1.2, 1, 1,
                                   3, 2, -1, -1.1, -1.1, -2,
                                   1.5, 3, -1, -1, -2, 1,
                                   1, 1, 2, 2, -1, -1);
      // economy size qr decomp output from matlab
      cC : Array[0..cSize*cSize-1] of double = (-4.272002, -3.160111, 0.117041, -0.046816, 1.498127, 1.053370,
                                                -0.189681, -5.292797, 1.630544, 0.065739, 0.768167, 0.882564,
                                                 0.379363, 0.004132, 3.511072, 1.708392, -1.004785, -1.584230,
                                                 0.569044, -0.091157, 0.454708, 2.232685, 1.628624, 0.984368,
                                                 0.284522, 0.149133, 0.200997, 0.301584, 1.312831, -1.029085,
                                                 0.189681, 0.002066, -0.316509, -0.502364, 0.442936, -2.360871);
begin
     SetLength(tau, cSize*cSize);

     SetLength(a, cSize*cSize);
     Move(ca, a[0], sizeof(ca));

     MatrixQRDecompInPlace2(@a[0], cSize*sizeof(double), cSize, cSize, @tau[0], nil, 4);

     Check(CheckMtx(cC, a), 'QR Decomposition 2 failed' );
end;

procedure TTestLinearEquations.TestLargeFullQRDecomp;
var a : TDoubleDynArray;
    tau : TDoubleDynArray;
    b, c : TDoubleDynArray;
    q1, q2 : TDoubleDynArray;
    counter: Integer;
    start, stop : Int64;
const cSize = 1258;
begin
     MtxThreadPool.InitMtxThreadPool;
     RandSeed := 243;
     SetLength(a, cSize*cSize);

     for counter := 0 to Length(a) - 1 do
         a[counter] := random - 0.5;
     b := Copy(a, 0, Length(a));
     c := Copy(a, 0, Length(a));

     SetLength(tau, cSize);

     start := MtxGetTime;
     MatrixQRDecompInPlace2(@a[0], cSize*sizeof(double), cSize, cSize, @tau[0], nil, 24);
     stop := MtxGetTime;
     Status(Format('Blocked QR decomp: %.2fms', [(stop - start)/mtxfreq*1000]));

     start := MtxGetTime;
     ThrMatrixQRDecomp(@c[0], cSize*sizeof(double), cSize, cSize, @tau[0], nil, 24);
     stop := MtxGetTime;
     Status(Format('Threaded QR decomp: %.2fms', [(stop - start)/mtxfreq*1000]));

     // compare against the unblocked version
     start := MtxGetTime;
     MatrixQRDecompInPlace2(@b[0], cSize*sizeof(double), cSize, cSize, @tau[0], nil, cSize);
     stop := MtxGetTime;
     Status(Format('QR decomp: %.2fms', [(stop - start)/mtxfreq*1000]));

     Check(CheckMtx(a, b), 'Blocked QR failed miserably');
     Check(CheckMtx(a, c), 'Threaded QR failed miserably');

     // now check the creation of the full Q an R matrices:
     q1 := Copy(a, 0, Length(a));
     q2 := Copy(a, 0, Length(a));

     start := MtxGetTime;
     MatrixQFromQRDecomp(@q1[0], cSize*SizeOf(double), cSize, cSize, @tau[0], 24, nil);
     stop := MtxGetTime;

     Status(Format('Q from ecosize QR decomp: %.2fms', [(stop - start)/mtxfreq*1000]));

     start := MtxGetTime;
     ThrMatrixQFromQRDecomp(@q2[0], cSize*SizeOf(double), cSize, cSize, @tau[0], 24, nil);
     stop := MtxGetTime;

     Status(Format('Threaded Q from ecosize QR decomp: %.2fms', [(stop - start)/mtxfreq*1000]));
     Check(CheckMtx(q1, q2), 'Threaded Q from ecsosize QR failed');
     
     MtxThreadPool.FinalizeMtxThreadPool;
end;

procedure TTestLinearEquations.TestLeasSquaresQR;
const cA : Array[0..5] of double = (3, -6, 4, -8, 0, 1);
      cY : Array[0..2] of double = (-1, 7, 2);
      cX : Array[0..1] of double = (5, 2);
var A : Array[0..5] of double;
    y : Array[0..2] of double;
    x : Array[0..1] of double;
begin
     Move(cA, A, sizeof(A));
     Move(cY, y, sizeof(y));

     FillChar(x, sizeof(x), 0);

     Check( MatrixQRSolve(@x[0], sizeof(double), @A[0], 2*sizeof(double), @y[0], sizeof(double), 2, 3) = qrOK, 'Error could not solve system');

     Status(WriteMtx(x, 2));     

     Check( CheckMtx(x, cX), 'QR Least Squares failed');
end;

procedure TTestLinearEquations.TestFullQRDecomp;
var q : TDoubleDynArray;
    qBlk : TDoubleDynArray;
    R : TDoubleDynArray;
    tau : TDoubleDynArray;
    x, y : integer;
    dest : TDoubleDynArray;
const cSize = 6;
      cA : Array[0..cSize*cSize - 1] of double = (1, 2, 2, 0.5, -1, -2,
                                   -1, 4, -2, -0.1, 0.1, -1,
                                   2, 2, -1, 1.2, 1, 1,
                                   3, 2, -1, -1.1, -1.1, -2,
                                   1.5, 3, -1, -1, -2, 1,
                                   1, 1, 2, 2, -1, -1);
begin
     SetLength(tau, cSize*cSize);

     SetLength(q, cSize*cSize);
     Move(ca, q[0], sizeof(ca));

     MatrixQRDecompInPlace2(@q[0], cSize*sizeof(double), cSize, cSize, @tau[0], nil, 4);
     R := Copy(q, 0, length(q));
     qBlk := Copy(q, 0, length(q));
     // zero out the parts occupied by Q
     for y := 1 to cSize - 1 do
     begin
          for x := 0 to y - 1 do
              R[x + y*cSize] := 0;
     end;
     MatrixQFromQRDecomp(@q[0], cSize*sizeof(double), cSize, cSize, @tau[0], cSize, nil);

     // blocked version
     MatrixQFromQRDecomp(@qBlk[0], cSize*sizeof(double), cSize, cSize, @tau[0], 2, nil );

     // simple test if Q*R matches cA
     dest := MatrixMult(Q, r, cSize, cSize, cSize, cSize);

     Check(CheckMtx(dest, ca), 'Error Q*R does not match A');

     dest := MatrixMult(QBlk, r, cSize, CSize, CSize, CSize);

     Check(CheckMtx(dest, ca), 'Error blocked Q*R does not match A');
end;

procedure TTestLinearEquations.TestAsymQRDecomp1;
const cBlkWidth = 12;
      cBlkHeight = 7;
      cBlkSize = cBlkWidth*cBlkHeight;
var x, y : integer;
    i: Integer;
    A, q, R, qBlk : TDoubleDynArray;
    tau, dest : TDoubleDynArray;
begin
     SetLength(q, cBlkWidth*cBlkHeight);
     RandSeed := 15;
     for i := 0 to cBlkSize - 1 do
         q[i] := Random - 0.5;

     A := Copy(q, 0, Length(q));
     SetLength(tau, cBlkWidth);

     MatrixQRDecompInPlace2(@q[0], cBlkWidth*sizeof(double), cBlkWidth, cBlkHeight, @tau[0]);

     R := Copy(q, 0, length(q));
     qBlk := Copy(q, 0, length(q));
     // zero out the parts occupied by Q
     for y := 1 to cBlkHeight - 1 do
     begin
          for x := 0 to y - 1 do
              R[x + y*cBlkWidth] := 0;
     end;
     MatrixQFromQRDecomp(@q[0], cBlkWidth*sizeof(double), cBlkWidth, cBlkHeight, @tau[0], cBlkWidth, nil);

     // blocked version
     MatrixQFromQRDecomp(@qBlk[0], cBlkWidth*sizeof(double), cBlkWidth, cBlkHeight, @tau[0], 4, nil);

     Check(CheckMtx(q, qBlk, cBlkWidth), 'Error: blocked Q version differs from unblocked');

     // simple test if Q*R matches A
     dest := MatrixMult(@qBlk[0], @r[0], cBlkHeight, cBlkHeight, cBlkWidth, cBlkHeight, cBlkWidth*sizeof(double), cBlkWidth*sizeof(double));
     Check(CheckMtx(dest, a), 'Error Q*R does not match A');
end;

procedure TTestLinearEquations.TestAsymQRDecomp2;
const cBlkWidth = 7;
      cBlkHeight = 12;
      cBlkSize = cBlkWidth*cBlkHeight;
var x, y : integer;
    i: Integer;
    A, q, R, qBlk : TDoubleDynArray;
    tau, dest : TDoubleDynArray;
begin
     SetLength(q, cBlkWidth*cBlkHeight);
     RandSeed := 15;
     for i := 0 to cBlkSize - 1 do
         q[i] := Random - 0.5;

     A := Copy(q, 0, Length(q));
     SetLength(tau, cBlkWidth);

     MatrixQRDecompInPlace2(@q[0], cBlkWidth*sizeof(double), cBlkWidth, cBlkHeight, @tau[0]);

     R := Copy(q, 0, length(q));
     qBlk := Copy(q, 0, length(q));
     // zero out the parts occupied by Q
     for y := 1 to cBlkHeight - 1 do
     begin
          for x := 0 to Min(cBlkWidth, y) - 1 do
              R[x + y*cBlkWidth] := 0;
     end;

     MatrixQFromQRDecomp(@q[0], cBlkWidth*sizeof(double), cBlkWidth, cBlkHeight, @tau[0], cBlkWidth, nil);

     // blocked version
     MatrixQFromQRDecomp(@qBlk[0], cBlkWidth*sizeof(double), cBlkWidth, cBlkHeight, @tau[0], 4, nil);

     Check(CheckMtx(q, qBlk, cBlkWidth), 'Error: blocked Q version differs from unblocked');

     // simple test if Q*R matches A
     dest := MatrixMult(@qBlk[0], @r[0], cBlkWidth, cBlkHeight, cBlkWidth, cBlkWidth, cBlkWidth*sizeof(double), cBlkWidth*sizeof(double));
     Check(CheckMtx(dest, a), 'Error Q*R does not match A');
end;

procedure TTestLinearEquations.TestSVD1;
const A : Array[0..3] of double = (1, 2, 2, -1);

      // results from matlab
      cV : Array[0..3] of double = (-1, 0, 0, -1);
      cS : Array[0..3] of double = (-0.4472, -0.8944, -0.8944, 0.4472);
      cW : Array[0..1] of double = (2.2361, 2.2361);

var res, res1 : TSVDResult;
    S, S2 : Array[0..3] of double;
    W : Array[0..3] of double;
    w2 : Array[0..1] of double;
    V, V2 :  Array[0..3] of double;
    V1 : TDoubleDynArray;
    X : TDoubleDynArray;
    Dest : Array[0..3] of double;
begin
     FillChar(w[0], sizeof(w), 0);
     FillChar(V[0], sizeof(V), 0);
     Move(A, S, sizeof(A));

     FillChar(w2[0], sizeof(w2), 0);
     FillChar(V2[0], sizeof(V2), 0);
     Move(A, S2, sizeof(A));

     res := MatrixSVDInPlace(@S[0], 2*sizeof(double), 2, 2, @W[0], 3*sizeof(double), @V[0], 2*sizeof(double));
     
     // S*W*V should be the start matrix
     V1 := MatrixTranspose(@v[0], 2*sizeof(double), 2, 2);
     X := MatrixMult(@W[0], @V1[0], 2, 2, 2, 2, 2*sizeof(double), 2*sizeof(double));
     MatrixMult(@dest[0], 2*sizeof(double), @S[0], @X[0], 2, 2, 2, 2, 2*sizeof(double), 2*sizeof(double));

     Check(res = srOk, 'S = ' + WriteMtx(S, 2) + ', W = ' + WriteMtx(W, 2) + ', V = ' + WriteMtx(V, 2));
     Check(CheckMtx(A, Dest), 'Result Multiplication: ' + WriteMtx(Dest, 2));

     // check results from matlab implementation
     res1 := MatrixSVDInPlace2(@S2[0], 2*sizeof(double), 2, 2, PConstDoubleArr(@W2[0]), @V2[0], 2*sizeof(double));
     Check(res1 = srOk, 'S = ' + WriteMtx(S2, 2) + ', W = ' + WriteMtx(W2, 2) + ', V = ' + WriteMtx(V2, 2));

     Check(CheckMtx(s2, cS), 'DGSVD failed');
     Check(CheckMtx(w2, cW), 'DGSVD failed');
     Check(CheckMtx(V2, cV), 'DGSVD failed');
end;

procedure TTestLinearEquations.TestSVD11;
const A : Array[0..8] of double = (1, 2, 3, 4, 5, 6, 7, 8, 9);
var res : TSVDResult;
    S, s2 : Array[0..8] of double;
    W, w2 : Array[0..2] of double;
    V, v2 : Array[0..8] of double;
begin
     FillChar(w[0], sizeof(w), 0);
     FillChar(V[0], sizeof(V), 0);
     Move(A, S, sizeof(S));

     FillChar(w2[0], sizeof(w), 0);
     FillChar(V2[0], sizeof(V), 0);
     Move(A, S2, sizeof(S));

     MatrixSVDInPlace(@S2[0], 3*sizeof(double), 3, 3, @W2[0], sizeof(double), @V2[0], 3*sizeof(double));
     Status( WriteMtx(S2, 3, 5));
     status( WriteMtx(W2, 1, 5));
     Status( WriteMtx(V2, 3, 5));

     res := MatrixSVDInPlace2(@s[0], 3*sizeof(double), 3, 3, @W[0], @V[0], 3*sizeof(double));

     Check(res = srOk, 'MatrixSVDInPlace2 failed');
     Status( WriteMtx(S, 3, 5));
     status( WriteMtx(W, 1, 5));
     Status( WriteMtx(V, 3, 5));

     FillChar(w[0], sizeof(w), 0);
     FillChar(V[0], sizeof(V), 0);
     Move(A, S, sizeof(S));

     FillChar(w2[0], sizeof(w), 0);
     FillChar(V2[0], sizeof(V), 0);
     Move(A, S2, sizeof(S));

     MatrixSVDInPlace(@S2[0], 3*sizeof(double), 3, 2, @W2[0], sizeof(double), @V2[0], 3*sizeof(double));
     Status( WriteMtx(S2, 3, 5));
     status( WriteMtx(W2, 1, 5));
     Status( WriteMtx(V2, 3, 5));

     res := MatrixSVDInPlace2(@s[0], 3*sizeof(double), 3, 2, @W[0], @V[0], 3*sizeof(double));

     Status( WriteMtx(S, 3, 5));
     status( WriteMtx(W, 1, 5));
     Status( WriteMtx(V, 3, 5));

     Check(res = srOk, 'MatrixSVDInPlace2 failed');
end;

procedure TTestLinearEquations.TestSVD12;
const A : Array[0..15] of double = (0.1, 0.3, -1, -2, 
                                    2, 2.1, -0.2, 1.4,
                                    -1, 1.2, 3, 3,
                                    -1.2, -0.1, -0.3, -0.4);

// from matlab
const cW : Array[0..3] of double = (5.0897, 3.1466, 1.1928, 0.6494);
      cS : Array[0..15] of double = (-0.3932, 0.1212, 0.7740, -0.4813,
                                     0.2934, 0.9052, 0.1677, 0.2578,
                                     0.8668, -0.2818, 0.3464, -0.2221,
                                     -0.0897, -0.2942, 0.5029, 0.8078);
      cV : Array[0..15] of double = (-0.0416,    0.7810,   -0.4503,   -0.4309,
                                      0.3040,    0.5175,    0.7962,    0.0766,
                                      0.5819,   -0.3367,    0.0676,   -0.7372,
                                      0.7532,    0.0944,   -0.3985,    0.5148);
var res : TSVDResult;
    S, s2 : Array[0..15] of double;
    W, w2 : Array[0..3] of double;
    V, v2 : Array[0..15] of double;
    vT : Array[0..15] of double;
begin
     FillChar(w[0], sizeof(w), 0);
     FillChar(V[0], sizeof(V), 0);
     Move(A, S, sizeof(S));

     FillChar(w2[0], sizeof(w), 0);
     FillChar(V2[0], sizeof(V), 0);
     Move(A, S2, sizeof(S));
     
     MatrixSVDInPlace(@S2[0], 4*sizeof(double), 4, 4, @W2[0], sizeof(double), @V2[0], 4*sizeof(double));
     Status( WriteMtx(S2, 4, 5));
     status( WriteMtx(W2, 1, 5));
     Status( WriteMtx(V2, 4, 5));

     res := MatrixSVDInPlace2(@s[0], 4*sizeof(double), 4, 4, @W[0], @vT[0], 4*sizeof(double));
     MatrixTranspose(@V[0], 4*sizeof(double), @vt[0], 4*sizeof(double), 4, 4);
     
     Check(res = srOk, 'dgesvd failed');
     
     Status( WriteMtx(S, 4, 5));
     status( WriteMtx(W, 1, 5));
     Status( WriteMtx(V, 4, 5));

     Check(CheckMtx(s, cS), 'DGSVD failed');
     Check(CheckMtx(w, cW), 'DGSVD failed');
     Check(CheckMtx(V, cV), 'DGSVD failed');
end;

procedure TTestLinearEquations.TestSVD2;
const A : Array[0..7] of double = (1, 2, 3, 4, 5, 6, 7, 8);
var res : TSVDResult;
    S : Array[0..7] of double;
    W : Array[0..3] of double;
    V : Array[0..3] of double;
    X : TDoubleDynArray;
    V1 : TDoubleDynArray;
    Dest : Array[0..7] of double;
begin
     FillChar(w[0], sizeof(w), 0);
     FillChar(V[0], sizeof(V), 0);

     res := MatrixSVD(@A[0], 2*sizeof(Double), 2, 4, @S[0], 2*sizeof(double), @W[0], 3*sizeof(double), @V[0], 2*sizeof(double));

     // S*W*V should be the start matrix
     V1 := MatrixTranspose(@v[0], 2*sizeof(double), 2, 2);
     X := MatrixMult(@W[0], @V1[0], 2, 2, 2, 2, 2*sizeof(double), 2*sizeof(double));
     MatrixMult(@dest[0], 2*sizeof(double), @S[0], @X[0], 2, 4, 2, 2, 2*sizeof(double), 2*sizeof(double));

     Check(res = srOk, 'S = ' + WriteMtx(S, 2) + ', W = ' + WriteMtx(W, 2) + ', V = ' + WriteMtx(V, 2));
     Check(CheckMtx(A, Dest), 'Result Multiplication: ' + WriteMtx(Dest, 2));

     Move(A, S, sizeof(A));
     FillChar(w[0], sizeof(w), 0);
     FillChar(V[0], sizeof(V), 0);
end;

procedure TTestLinearEquations.TestSVD3;
const A : Array[0..19] of double =
  ( 2.5000e+001,  2.5000e+001,  2.5000e+001,  2.5300e+001,  2.5200e+001,  2.5200e+001,  2.4800e+001,  2.4700e+001,  2.5100e+001,  2.4700e+001,
    -2.5000e+001,  -2.5000e+001,  -2.5000e+001,  -2.5300e+001,  -2.5200e+001,  -2.5200e+001,  -2.4800e+001,  -2.4700e+001,  -2.5100e+001,  -2.4700e+001);
var res : TSVDResult;
    S, S2 : Array[0..19] of double;
    W, W2 : Array[0..3] of double;
    V, V2 : Array[0..3] of double;
    X : TDoubleDynArray;
    V1 : TDoubleDynArray;
    Dest : Array[0..19] of double;
begin
     // test accuracy of linear dependent matrices:
     FillChar(w[0], sizeof(w), 0);
     FillChar(V[0], sizeof(V), 0);

     res := MatrixSVD(@A[0], 2*sizeof(Double), 2, 10, @S[0], 2*sizeof(double), @W[0], 3*sizeof(double), @V[0], 2*sizeof(double));

     // S*W*V should be the start matrix
     V1 := MatrixTranspose(@v[0], 2*sizeof(double), 2, 2);
     X := MatrixMult(@W[0], @V1[0], 2, 2, 2, 2, 2*sizeof(double), 2*sizeof(double));
     MatrixMult(@dest[0], 2*sizeof(double), @S[0], @X[0], 2, 10, 2, 2, 2*sizeof(double), 2*sizeof(double));

     Check(res = srOk, 'S = ' + WriteMtx(S, 2) + ', W = ' + WriteMtx(W, 2) + ', V = ' + WriteMtx(V, 2));
     Check(CheckMtx(A, Dest), 'Result Multiplication: ' + WriteMtx(Dest, 2));

     FillChar(w2, sizeof(w2), 0);
     FillChar(V2, sizeof(v2), 0);
     Move(A, S2, sizeof(s2));
     res := MatrixSVDInPlace2(@s2[0], 2*sizeof(Double), 2, 10, @W2[0], @V2[0], 2*sizeof(double));

     Check(res = srOk, 'S = ' + WriteMtx(S2, 2) + ', W = ' + WriteMtx(W2, 1) + ', V = ' + WriteMtx(V2, 2));

     Status( WriteMtx(S2, 2, 5));
     status( WriteMtx(W2, 1, 5));
     Status( WriteMtx(V2, 2, 5));
end;

procedure TTestLinearEquations.TestSVD4;
const cA : Array[0..11] of double = ( 1, 2, 3, 4,
                                      5, 4, 3, 1,
                                      1, 2, 1, 2 );
      cW : Array[0..2] of double = ( 8.7782, 3.6354, 0.8524 );
      cU : Array[0..8] of double = ( -0.5404, 0.7475, -0.3862,
                                     -0.7714, -0.6235, -0.1274,
                                     -0.3360, 0.2291, 0.9136 );
      cV : Array[0..11] of double = (-0.5392,   -0.5512,   -0.4866,   -0.4107,
                                     -0.5889,   -0.1488,    0.1653,    0.7770,
                                     -0.1284,    0.6397,   -0.7357,    0.1817  );
var a : Array[0..11] of double;
    w : Array[0..2] of double;
    v : Array[0..11] of double;
    u : Array[0..8] of double;
begin
     Move(cA, a, SizeOf(cA));
     FillChar(w, sizeof(w), 0);
     FillChar(v, sizeof(V), 0);

     check( srOk = MatrixSVDInPlace2(@a[0], 4*sizeof(double), 4, 3, @w[0], @V[0], 4*sizeof(double) ), 'SVD failed');

     Status(WriteMtx(a, 4));
     Status(WriteMtx(w, 1));
     Status(WriteMtx(v, 3));

     MatrixCopy(@u[0], 3*sizeof(double), @a[0], 4*sizeof(double), 3, 3);

     Check(CheckMtx(u, cU, -1, -1, 1e-2), 'SVD U failed');
     Check(CheckMtx(w, cW, -1, -1, 1e-2), 'SVD W failed');
     Check(CheckMtx(v, cV, -1, -1, 1e-2), 'SVD W failed');


     MatrixTranspose(@a[0], 3*sizeof(double), @cA[0], 4*sizeof(double), 4, 3);
     FillChar(w, sizeof(w), 0);
     FillChar(v, sizeof(V), 0);

     check( srOk = MatrixSVDInPlace2(@a[0], 3*sizeof(double), 3, 4, @w[0], @V[0], 3*sizeof(double) ), 'SVD failed');

     Status(WriteMtx(a, 3));
     Status(WriteMtx(w, 1));
     Status(WriteMtx(v, 3));
end;

procedure TTestLinearEquations.TestSVD5;
const cA : Array[0..14*7-1] of double =
   (-23.841054899,-23.000184846,37.364632705,7.272944470,-72.860984584,8.392959617,-13.517837487,
    60.833639470,3.528232561,65.804237499,-25.197981829,183.333439172,26.149141747,-13.517837487,
    21.579330488,67.296743757,42.066119531,-48.863406745,-1.629477818,-4.422232876,-13.517837487,
    38.620422590,39.258801436,-4.913997784,14.672415460,47.614035833,26.340871666,-13.517837487,
    -13.690499909,-15.391362672,-35.390273798,16.967858448,-3.694489408,37.019954750,-13.517837487,
    -4.034295958,-27.493006881,-60.436555743,28.847186847,-0.394691221,39.366084701,-13.517837487,
    1.976754100,45.834624533,66.282178166,-34.404550498,52.139325490,11.110598762,-13.517837487,
    15.114082290,1.834218170,-79.628378305,-3.587105772,10.593308957,27.817851407,-13.517837487,
    12.508676902,21.787220575,-27.722453549,5.434413595,1.490079217,27.019600049,-13.517837487,
    -35.301395001,-30.306190220,-51.518289041,-9.061938108,-15.487013198,25.557031986,-13.517837487,
    19.726155128,-46.220738905,-57.209275825,-40.642947792,10.707942501,-42.920308087,-13.517837487,
    -7.391353654,-66.613291328,47.413312602,56.674039448,-123.457046957,-15.540073582,-13.517837487,
    -63.357914386,20.124520361,1.274499920,-2.497354473,17.414166679,-124.031886610,-13.517837487,
    -22.742547160,9.360413460,56.614243622,34.386426948,-105.768594664,-41.859593530,175.731887330);

const cWidth = 7;
      cHeight = 14;
var a, a1 : Array[0..14*7-1] of double;
    w, w1 : Array[0..6] of double;
    v, v1 : Array[0..7*7-1] of double;
    res : TSVDResult;
function SVDMult(var A, W, V : Array of double; doTranspose : boolean) : boolean;
var i, j : integer;
    x : TDoubleDynArray;
    v2 : TDoubleDynArray;
begin
     //A*W store in A
     for i := 0 to cHeight - 1 do
         for j := 0 to cWidth - 1 do
             A[i*cWidth + j] := A[i*cWidth + j]*W[j];

     // final mutl: A*W*V'
     if doTranspose then
     begin
          v2 := MatrixTranspose(@V[0], cWidth*sizeof(double), cWidth, cWidth);
          x := MatrixMult(@A[0], @v2[0], cWidth, cHeight, cWidth, cWidth, cWidth*sizeof(double), cWidth*sizeof(double));
     end
     else
         x := MatrixMult(@A[0], @v[0], cWidth, cHeight, cWidth, cWidth, cWidth*sizeof(double), cWidth*sizeof(double));

     Result := CheckMtx(X, cA);
end;

begin
     Move(cA, a, sizeof(a));
     FillChar(w, sizeof(w), 0);
     FillChar(v, sizeof(v), 0);

     res := MatrixSVDInPlace2(@a[0], 7*sizeof(double), 7, 14, @w[0], @v[0], 7*sizeof(double));

     Check( res = srOk, 'Error in SVD 7x14');

     Status(WriteMtx(a, 7));
     Status(WriteMtx(v, 7));
     Status(WriteMtx(w, 1));

     Check(SVDMult(a, w, v, False), 'error in svd1');


     Move(cA, a1, sizeof(a1));
     FillChar(w1, sizeof(w1), 0);
     FillChar(v1, sizeof(v1), 0);

     res := MatrixSVDInPlace(@a1[0], 7*sizeof(double), 7, 14, @w1[0], sizeof(double), @v1[0], 7*sizeof(double));
     Check( res = srOk, 'Error in SVD 7x14');

     Check(SVDMult(a1, w1, v1, True), 'error in svd2');

     Status(WriteMtx(a1, 7));
     Status(WriteMtx(v1, 7));
     Status(WriteMtx(w1, 1));



end;

procedure TTestLinearEquations.SetUp;
begin
     InitMathFunctions( itSSE, False );
end;

procedure TTestLinearEquations.TestMatrixRot;
const cArrSize = 3877;
var x, y, x1, y1 : Array[0..4] of double;
    cnt : integer;
    lX, lY, lX1, lY1 : TDoubleDynArray;
    start1, stop1 : Int64;
    start2, stop2 : int64;
begin
     for cnt:= 0 to High(x) do
     begin
          x[cnt] := cnt + 1;
          y[cnt] := cnt - 1;
     end;

     Move(x, x1, sizeof(x));
     Move(y, y1, sizeof(y));

     GenericMatrixRotate( Length(x), @x[0], sizeof(Double), @y[0], sizeof(double), 1.2, 0.9);
     MatrixRotate( Length(x), @x1[0], sizeof(double), @y1[0], sizeof(double), 1.2, 0.9);

     Check( CheckMtx( x, x1 ), 'error asm rotation');
     Check( CheckMtx( y, y1 ), 'error asm rotation');

     GenericMatrixRotate( Length(x) - 1, @x[0], sizeof(Double), @y[0], sizeof(double), 1.2, 0.9);
     MatrixRotate( Length(x)- 1, @x1[0], sizeof(double), @y1[0], sizeof(double), 1.2, 0.9);

     Check( CheckMtx( x, x1 ), 'error asm rotation');
     Check( CheckMtx( y, y1 ), 'error asm rotation');


     SetLength(lX, cArrSize);
     SetLength(lY, cArrSize);

     for cnt := 0 to Length(lx) - 1 do
     begin
          lx[cnt] := cnt + 1;
          ly[cnt] := cnt - 1;
     end;

     lX1 := Copy(lx, 0, cArrSize);
     lY1 := Copy(ly, 0, cArrSize);

     start1 := MtxGetTime;
     GenericMatrixRotate( Length(lx), @lx[0], sizeof(Double), @ly[0], sizeof(double), 1.2, 0.9);
     stop1 := MtxGetTime;

     start2 := MtxGetTime;
     MatrixRotate( Length(lx1), @lx1[0], sizeof(double), @ly1[0], sizeof(double), 1.2, 0.9);
     stop2 := MtxGetTime;

     Status(Format('Matrix Rot took: %.3fms, %.3fms', [ (stop1 - start1)/mtxfreq*1000, (stop2 - start2)/mtxfreq*1000]));

     Check( CheckMtx( lx, lx1 ), 'error asm rotation');
     Check( CheckMtx( ly, ly1 ), 'error asm rotation');

     for cnt := 0 to Length(lx) - 1 do
     begin
          lx[cnt] := cnt + 1;
          ly[cnt] := cnt - 1;
     end;

     lX1 := Copy(lx, 0, cArrSize);
     lY1 := Copy(ly, 0, cArrSize);

     start1 := MtxGetTime;
     GenericMatrixRotate( Length(lx) div 2, @lx[0], 2*sizeof(Double), @ly[0], 2*sizeof(double), 1.2, 0.9);
     stop1 := MtxGetTime;

     start2 := MtxGetTime;
     MatrixRotate( Length(lx1) div 2, @lx1[0], 2*sizeof(double), @ly1[0], 2*sizeof(double), 1.2, 0.9);
     stop2 := MtxGetTime;

     Status(Format('Matrix Rot took: %.3fms, %.3fms', [ (stop1 - start1)/mtxfreq*1000, (stop2 - start2)/mtxfreq*1000]));

     Check( CheckMtx( lx, lx1 ), 'error asm rotation 2');
     Check( CheckMtx( ly, ly1 ), 'error asm rotation 2');
end;

procedure TTestLinearEquations.TestApplyPlaneRot;
var x, x1 : Array[0..14] of double;
    s, c : Array[0..4] of double;

    lx, lx1, ls, lc : TDoubleDynArray;

    start1, stop1 : Int64;
    start2, stop2 : int64;

const cVecSize = 2049;
      cHeight = 24;
      cSize = cVecSize*cHeight;
procedure InitArrs(var x, x1, s, c : Array of double);
var cnt : integer;
begin
     for cnt := 0 to High(x) do
     begin
          x[cnt] := cnt - 1;
          x1[cnt] := x[cnt];
     end;
     for cnt := 0 to high(c) do
     begin
          c[cnt] := cnt - 1;
          s[cnt] := cnt + 1;
     end;

     c[0] := 1;
     s[0] := 0;
end;
procedure InitArrs2(var x, x1, s, c : Array of double);
var cnt : integer;
begin
     for cnt := 0 to High(x) do
     begin
          x[cnt] := random;
          x1[cnt] := x[cnt];
     end;
     for cnt := 0 to high(c) do
     begin
          c[cnt] := -random;
          s[cnt] := random;
     end;
end;

begin
     SetLength(lx, cSize);
     SetLength(lx1, cSize);
     SetLength(ls, cVecSize);
     SetLength(lc, cVecSize);

     // #####################################################
     // #### RVB
     InitArrs(x, x1, s, c);
     GenericApplyPlaneRotSeqRVB(5, 3, @x[0], 5*sizeof(double), @c[0], @s[0]);
     ApplyPlaneRotSeqRVB(5, 3, @x1[0], 5*sizeof(double), @c[0], @s[0]);

     Check( CheckMtx(x, x1), 'Error ASM Plane rotate RVB odd width');

     InitArrs(x, x1, s, c);
     GenericApplyPlaneRotSeqRVB(4, 3, @x[0], 5*sizeof(double), @c[0], @s[0]);
     ApplyPlaneRotSeqRVB(4, 3, @x1[0], 5*sizeof(double), @c[0], @s[0]);

     Check( CheckMtx(x, x1), 'Error ASM Plane rotate RVB even width');

     RandSeed := 15;
     InitArrs2(lx, lx1, ls, lc);

     start1 := MtxGetTime;
     GenericApplyPlaneRotSeqRVB(cVecSize, cHeight, @lx[0], cVecSize*sizeof(double), @lc[0], @ls[0]);
     stop1 := MtxGetTime;

     start2 := MtxGetTime;
     ApplyPlaneRotSeqRVB(cVecSize, cHeight, @lx1[0], cVecSize*sizeof(double), @lc[0], @ls[0]);
     stop2 := MtxGetTime;

     Status(Format('Matrix seq plane rot RVB took: %.3fms, %.3fms', [ (stop1 - start1)/mtxfreq*1000, (stop2 - start2)/mtxfreq*1000]));

     Check( CheckMtx(lx, lx1), 'Error ASM Plane rotate RVB odd width');

     InitArrs2(lx, lx1, ls, lc);

     GenericApplyPlaneRotSeqRVB(cVecSize - 1, cHeight, @lx[0], cVecSize*sizeof(double), @lc[0], @ls[0]);
     ApplyPlaneRotSeqRVB(cVecSize - 1, cHeight, @lx1[0], cVecSize*sizeof(double), @lc[0], @ls[0]);

     Check( CheckMtx(lx, lx1), 'Error ASM Plane rotate RVB odd width');

     // ########################################################
     // ### RVF
     InitArrs(x, x1, s, c);
     GenericApplyPlaneRotSeqRVF(5, 3, @x[0], 5*sizeof(double), @c[0], @s[0]);
     ApplyPlaneRotSeqRVF(5, 3, @x1[0], 5*sizeof(double), @c[0], @s[0]);

     Check( CheckMtx(x, x1), 'Error ASM Plane rotate RVF odd width');

     InitArrs(x, x1, s, c);
     GenericApplyPlaneRotSeqRVF(4, 3, @x[0], 5*sizeof(double), @c[0], @s[0]);
     ApplyPlaneRotSeqRVF(4, 3, @x1[0], 5*sizeof(double), @c[0], @s[0]);

     Check( CheckMtx(x, x1), 'Error ASM Plane rotate RVF even width');

     RandSeed := 15;
     InitArrs2(lx, lx1, ls, lc);

     start1 := MtxGetTime;
     GenericApplyPlaneRotSeqRVF(cVecSize, cHeight, @lx[0], cVecSize*sizeof(double), @lc[0], @ls[0]);
     stop1 := MtxGetTime;

     start2 := MtxGetTime;
     ApplyPlaneRotSeqRVF(cVecSize, cHeight, @lx1[0], cVecSize*sizeof(double), @lc[0], @ls[0]);
     stop2 := MtxGetTime;

     Status(Format('Matrix seq plane rot RVF took: %.3fms, %.3fms', [ (stop1 - start1)/mtxfreq*1000, (stop2 - start2)/mtxfreq*1000]));

     Check( CheckMtx(lx, lx1), 'Error ASM Plane rotate RVF odd width');

     InitArrs2(lx, lx1, ls, lc);

     GenericApplyPlaneRotSeqRVF(cVecSize - 1, cHeight, @lx[0], cVecSize*sizeof(double), @lc[0], @ls[0]);
     ApplyPlaneRotSeqRVF(cVecSize - 1, cHeight, @lx1[0], cVecSize*sizeof(double), @lc[0], @ls[0]);

     Check( CheckMtx(lx, lx1), 'Error ASM Plane rotate RVF odd width');

     // ########################################################
     // ### LVF
     InitArrs(x, x1, s, c);
     GenericApplyPlaneRotSeqLVF(5, 3, @x[0], 5*sizeof(double), @c[0], @s[0]);
     ApplyPlaneRotSeqLVF(5, 3, @x1[0], 5*sizeof(double), @c[0], @s[0]);

     Check( CheckMtx(x, x1), 'Error ASM Plane rotate LVF odd width');

     InitArrs(x, x1, s, c);
     GenericApplyPlaneRotSeqLVF(4, 3, @x[0], 5*sizeof(double), @c[0], @s[0]);
     ApplyPlaneRotSeqLVF(4, 3, @x1[0], 5*sizeof(double), @c[0], @s[0]);

     Check( CheckMtx(x, x1), 'Error ASM Plane rotate LVF even width');

     RandSeed := 15;
     InitArrs2(lx, lx1, ls, lc);

     start1 := MtxGetTime;
     GenericApplyPlaneRotSeqLVF(cVecSize, cHeight, @lx[0], cVecSize*sizeof(double), @lc[0], @ls[0]);
     stop1 := MtxGetTime;

     start2 := MtxGetTime;
     ApplyPlaneRotSeqLVF(cVecSize, cHeight, @lx1[0], cVecSize*sizeof(double), @lc[0], @ls[0]);
     stop2 := MtxGetTime;

     Status(Format('Matrix seq plane rot LVF took: %.3fms, %.3fms', [ (stop1 - start1)/mtxfreq*1000, (stop2 - start2)/mtxfreq*1000]));

     Check( CheckMtx(lx, lx1), 'Error ASM Plane rotate LVF odd width');

     InitArrs2(lx, lx1, ls, lc);

     GenericApplyPlaneRotSeqLVF(cVecSize - 1, cHeight, @lx[0], cVecSize*sizeof(double), @lc[0], @ls[0]);
     ApplyPlaneRotSeqLVF(cVecSize - 1, cHeight, @lx1[0], cVecSize*sizeof(double), @lc[0], @ls[0]);

     Check( CheckMtx(lx, lx1), 'Error ASM Plane rotate LVF odd width');

     // ########################################################
     // ### LVB
     InitArrs(x, x1, s, c);
     GenericApplyPlaneRotSeqLVB(5, 3, @x[0], 5*sizeof(double), @c[0], @s[0]);
     ApplyPlaneRotSeqLVB(5, 3, @x1[0], 5*sizeof(double), @c[0], @s[0]);

     Check( CheckMtx(x, x1), 'Error ASM Plane rotate LVB odd width');

     InitArrs(x, x1, s, c);
     GenericApplyPlaneRotSeqLVB(4, 3, @x[0], 5*sizeof(double), @c[0], @s[0]);
     ApplyPlaneRotSeqLVB(4, 3, @x1[0], 5*sizeof(double), @c[0], @s[0]);

     Check( CheckMtx(x, x1), 'Error ASM Plane rotate LVB even width');

     RandSeed := 15;
     InitArrs2(lx, lx1, ls, lc);

     start1 := MtxGetTime;
     GenericApplyPlaneRotSeqLVB(cVecSize, cHeight, @lx[0], cVecSize*sizeof(double), @lc[0], @ls[0]);
     stop1 := MtxGetTime;

     start2 := MtxGetTime;
     ApplyPlaneRotSeqLVB(cVecSize, cHeight, @lx1[0], cVecSize*sizeof(double), @lc[0], @ls[0]);
     stop2 := MtxGetTime;

     Status(Format('Matrix seq plane rot LVB took: %.3fms, %.3fms', [ (stop1 - start1)/mtxfreq*1000, (stop2 - start2)/mtxfreq*1000]));

     Check( CheckMtx(lx, lx1), 'Error ASM Plane rotate LVB odd width');

     InitArrs2(lx, lx1, ls, lc);

     GenericApplyPlaneRotSeqLVB(cVecSize - 1, cHeight, @lx[0], cVecSize*sizeof(double), @lc[0], @ls[0]);
     ApplyPlaneRotSeqLVB(cVecSize - 1, cHeight, @lx1[0], cVecSize*sizeof(double), @lc[0], @ls[0]);

     Check( CheckMtx(lx, lx1), 'Error ASM Plane rotate LVB odd width');
end;

initialization
  RegisterTest(TTestLinearEquations{$IFNDEF FPC}.Suite{$ENDIF});
  if IsAVXPresent then
     RegisterTest(TAVXTestLinearEquations{$IFNDEF FPC}.Suite{$ENDIF});
  if IsFMAPresent then
     RegisterTest(TFMATestLinearEquations{$IFNDEF FPC}.Suite{$ENDIF});

end.
