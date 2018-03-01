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

unit TestFMA;

interface

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}

uses {$IFDEF FPC} testregistry {$ELSE} TestFramework {$ENDIF}
     , Classes, SysUtils, Types, SimpleMatrixOperations, Matrix,
     BaseMatrixTestCase;

type
  { TestFMAMatrixOperations }
  TestFMAMatrixOperations = class(TBaseMatrixTestCase)
  published
    procedure TestFMAMultMod16;
    procedure TestFMAVecMult;
    procedure TestBigMtxVecMult;
    procedure TestFMAMultTransposed;
    procedure TestFMAMult;
    procedure TestFMADiagMtxMult;
    procedure TestFMABigMult;
  end;

implementation

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}

uses {$IFDEF x64}
     AVXMatrixMultTransposedOperationsx64,
     AVXMatrixVectorMultOperationsx64,
     FMAMatrixMultTransposedOperationsx64, FMAMatrixVectorMultOperationsx64,
     FMAMatrixMultOperationsx64, FMAMatrixOperations,
     {$ELSE}
     AVXMatrixMultTransposedOperations, AVXMatrixVectorMultOperations,
     FMAMatrixMultTransposedOperations, FMAMatrixVectorMultOperations,
     FMAMatrixMultOperations, FMAMatrixOperations,

     {$ENDIF}
     MatrixConst, MathUtilFunc, MtxTimer, OptimizedFuncs,
     CPUFeatures;

{ TestFMAMatrixOperations }

procedure TestFMAMatrixOperations.TestFMAMultTransposed;
var p1, p2, p3, p4 : PDouble;
    pMem1, pMem2, pMem3, pMem4 : PByte;
    LineWidth1, LineWidth2, LineWidth3, LineWidth4 : TASMNativeInt;
    start1, stop1, start2, stop2 : int64;
    outIdx : Integer;
    mtxWidth : integer;
    mtxSize : integer;
    counter : integer;
const cMtxWidths : Array[0..9] of integer = (43, 2, 3, 4, 5, 8, 16, 256, 342, 400);
begin
     // unaligned test
     for counter := 0 to Length(cMtxWidths) - 1 do
     begin
          mtxWidth := cMtxWidths[counter];
          mtxSize := mtxWidth*mtxWidth;

          FillAlignedMtx( mtxSize, p1, pMem1);
          FillAlignedMtx( mtxSize, p2, pMem2);
          FillAlignedMtx( mtxSize, p3, pMem3);
          FillAlignedMtx( mtxSize, p4, pMem4);

          start1 := MtxGetTime;
          GenericMtxMultTransp(p3, mtxWidth*sizeof(double), p1, p2, mtxWidth, mtxWidth, mtxWidth, mtxWidth, mtxWidth*sizeof(double), mtxWidth*sizeof(double));
          stop1 := MtxGetTime;

          start2 := MtxGetTime;
          FMAMatrixMultUnAlignedTransposed(p4, mtxWidth*sizeof(double), p1, p2, mtxWidth, mtxWidth, mtxWidth, mtxWidth, mtxWidth*sizeof(double), mtxWidth*sizeof(double));
          stop2 := MtxGetTime;

          Check(CheckMtxIdx(p3, p4, MtxSize, outidx, 1e-6), 'AVX Transposed Error Matrix mult failed');

          Status(Format('mtxWidth %d: %.2f, %.2f', [mtxWidth, (stop1 - start1)/mtxFreq*1000, (stop2 - start2)/mtxFreq*1000]));

          FreeMem(pMem1);
          FreeMem(pMem2);
          FreeMem(pMem3);
          FreeMem(pMem4);
     end;

     // aligned test
     for counter := 0 to Length(cMtxWidths) - 1 do
     begin
          mtxWidth := cMtxWidths[counter];

          FillAlignedMtx( mtxWidth, mtxWidth, p1, pMem1, LineWidth1);
          FillAlignedMtx( mtxWidth, mtxWidth, p2, pMem2, LineWidth2);
          FillAlignedMtx( mtxWidth, mtxWidth, p3, pMem3, LineWidth3);
          FillAlignedMtx( mtxWidth, mtxWidth, p4, pMem4, LineWidth4);

          start1 := MtxGetTime;
          GenericMtxMultTransp(p3, LineWidth3, p1, p2, mtxWidth, mtxWidth, mtxWidth, mtxWidth, LineWidth1, LineWidth2);
          stop1 := MtxGetTime;

          start2 := MtxGetTime;
          FMAMatrixMultAlignedTransposed(p4, LineWidth4, p1, p2, mtxWidth, mtxWidth, mtxWidth, mtxWidth, LineWidth1, LineWidth2);
          stop2 := MtxGetTime;

          Check(CheckMtxIdx(p3, p4, LineWidth3, LineWidth4, mtxWidth, mtxWidth, outidx, 1e-6), 'AVX Transposed Error Matrix mult failed');

          Status(Format('mtxWidth %d: %.2f, %.2f', [mtxWidth, (stop1 - start1)/mtxFreq*1000, (stop2 - start2)/mtxFreq*1000]));

          FreeMem(pMem1);
          FreeMem(pMem2);
          FreeMem(pMem3);
          FreeMem(pMem4);
     end;
end;


procedure TestFMAMatrixOperations.TestFMAVecMult;
const cMtx : Array[0..8] of double = (1, 2, 3, 4, 5, 6, 7, 8, 9);
      cV : Array[0..2] of double = (1, 2, 3);
var dest1, dest : Array[0..2] of double;
    d1, d2 : TDoubleDynArray;
    x, y : TDoubleDynArray;
    i, j : integer;
    px, pv : PDouble;
    pd1, pd2 : PDouble;
    pm1, pm2, pm3, pm4 : PByte;
    lineWidthX : TASMNativeInt;
    idx : integer;
begin
     dest[0] := 1;
     dest[1] := 1;
     dest[2] := 1;
     Move(dest, dest1, sizeof(dest));

     GenericMtxVecMultT(@dest1[0], sizeof(double), @cMtx[0], @cV[0], 3*sizeof(double), sizeof(double), 3, 3, 0.1, -1);
     FMAMatrixVectMultT(@dest[0], sizeof(double), @cMtx[0], @cV[0], 3*sizeof(double), sizeof(double), 3, 3, 0.1, -1);

     Check(CheckMtx(dest1, dest), 'Error matrix vector multiplication failed');

     Move(dest, dest1, sizeof(dest));
     GenericMtxVecMultT(@dest1[0], sizeof(double), @cMtx[0], @cV[0], 3*sizeof(double), sizeof(double), 3, 3, 0.1, -1);
     FMAMatrixVecMultTDestVec(@dest[0], sizeof(double), @cMtx[0], @cV[0], 3*sizeof(double), sizeof(double), 3, 3, 0.1, -1);

     Check(CheckMtx(dest1, dest), 'Error matrix vector multiplication failed');

     Move(dest, dest1, sizeof(dest));
     GenericMtxVecMult(@dest1[0], sizeof(double), @cMtx[0], @cV[0], 3*sizeof(double), sizeof(double), 3, 3, 0.1, -1);
     FMAMatrixVectMultUnAlignedVAligned(@dest[0], sizeof(double), @cMtx[0], @cV[0], 3*sizeof(double), sizeof(double), 3, 3, 0.1, -1);

     Check(CheckMtx(dest1, dest), 'Error matrix vector multiplication failed');

     // larger mult to chec the inner loops
     SetLength(x, 9* 9);
     SetLength(y, 9*9);
     SetLength(d1, 9*9);
     SetLength(d2, 9*9);

     for i := 0 to 8 do
     begin
          for j := 0 to 8 do
          begin
               x[i*9 + j] := i*j;
               y[i*9 + j] := i*j;
               d1[i*9 + j] := 1;
               d2[i*9 + j] := 1;
          end;
     end;

     GenericMtxVecMultT(@d1[0], sizeof(double), @x[0], @y[0], 9*sizeof(double), 9*sizeof(double), 9, 9, 0.1, -1);
     FMAMatrixVectMultT(@d2[0], sizeof(double), @x[0], @y[0], 9*sizeof(double), 9*sizeof(double), 9, 9, 0.1, -1);

     Check(CheckMtx(d1, d2), 'Error matrix vector multiplication failed');

     GenericMtxVecMultT(@d1[0], sizeof(double), @x[0], @y[0], 9*sizeof(double), 9*sizeof(double), 9, 9, 0.1, -1);
     FMAMatrixVecMultTDestVec(@d2[0], sizeof(double), @x[0], @y[0], 9*sizeof(double), 9*sizeof(double), 9, 9, 0.1, -1);

     Check(CheckMtx(d1, d2), 'Error matrix vector multiplication (dest=vector) failed');

     Move(dest, dest1, sizeof(dest));
     GenericMtxVecMult(@d1[0], sizeof(double), @x[0], @y[0], 3*sizeof(double), sizeof(double), 3, 3, 0.1, -1);
     FMAMatrixVectMultUnAlignedVAligned(@d2[0], sizeof(double), @x[0], @y[0], 3*sizeof(double), sizeof(double), 3, 3, 0.1, -1);

     Check(CheckMtx(d1, d2), 'Error matrix vector multiplication (dest=vector) failed');

     for i := 1 to 18 do
     begin
          for j := 1 to 18 do
          begin
               FillAlignedMtx(j, i, px, pm1, lineWidthX);
               FillAlignedMtx(j, pv, pm2);
               AllocAlignedMtx(i, pd1, pm3);
               AllocAlignedMtx(i, pd2, pm4);

               GenericMtxVecMult(pd1, sizeof(double), px, pv, lineWidthX, sizeof(double), j, i, 2, -1);
               FMAMatrixVectMultUnAlignedVAligned(pd2, sizeof(double), px, pv, lineWidthX, sizeof(double), j, i, 2, -1);

               Check(CheckMtxIdx(pd1, pd2, i, idx), Format('Error mtx vec mult u aligned V on %d x %d @ %d', [j, i, idx]));

               GenericMtxVecMult(pd1, sizeof(double), px, pv, lineWidthX, sizeof(double), j, i, 3, -0.1);
               FMAMatrixVectMultAlignedVAligned(pd2, sizeof(double), px, pv, lineWidthX, sizeof(double), j, i, 3, -0.1);

               Check(CheckMtxIdx(pd1, pd2, i, idx), Format('Error mtx vec mult aligned V on %d x %d @ %d', [j, i, idx]));

               FreeMem(pm1);
               FreeMem(pm2);
               FreeMem(pm3);
               FreeMem(pm4);
          end;
     end;
end;

procedure TestFMAMatrixOperations.TestBigMtxVecMult;
const cMtxSize = 1350;
var x, y : TDoubleDynArray;
    xa, ya : PDouble;
    dest1 : TDoubleDynArray;
    dest2,
    dest3 : TDoubleDynArray;
    startTime1, endTime1 : Int64;
    startTime2, endTime2 : int64;
    startTime3, endTime3 : int64;
begin
     randomize;
     FillMatrix(cMtxSize*cMtxSize, x, y, xa, ya);
     dest1 := Copy(x, 0, Length(x));
     dest2 := Copy(x, 0, Length(x));
     dest3 := Copy(x, 0, Length(x));

     startTime1 := MtxGetTime;
     GenericMtxVecMultT(@dest1[0], cMtxSize*sizeof(double), @x[0], @y[0], cMtxSize*sizeof(double), cMtxSize*sizeof(double), cMtxSize - 1, cMtxSize - 1, 2, -0.2);
     endTime1 := MtxGetTime;

     startTime2 := MtxGetTime;
     AVXMatrixVectMultT(@dest2[0], cMtxSize*sizeof(double), @x[0], @y[0], cMtxSize*sizeof(double), cMtxSize*sizeof(double), cMtxSize - 1, cMtxSize - 1, 2, -0.2);
     endTime2 := MtxGetTime;

     startTime3 := MtxGetTime;
     FMAMatrixVectMultT(@dest3[0], cMtxSize*sizeof(double), @x[0], @y[0], cMtxSize*sizeof(double), cMtxSize*sizeof(double), cMtxSize - 1, cMtxSize - 1, 2, -0.2);
     endTime3 := MtxGetTime;


     Status(Format('%.2f,  %.2f,   %.2f', [(endTime1 - startTime1)/mtxFreq*1000,  (endTime2 - startTime2)/mtxFreq*1000, (endTime3 - startTime3)/mtxFreq*1000]));
     Check(CheckMtx(dest1, dest3), 'Error transposed AVX vector multiplication failed');

     Check(CheckMtx(dest1, dest2), 'Error transposed vector multiplication failed');

     dest3 := Copy(dest2, 0, Length(dest2));

     startTime1 := MtxGetTime;
     GenericMtxVecMultT(@dest1[0], sizeof(double), @x[0], @y[0], cMtxSize*sizeof(double), cMtxSize*sizeof(double), cMtxSize - 1, cMtxSize - 1, 2, -0.2);
     endTime1 := MtxGetTime;

     startTime2 := MtxGetTime;
     AVXMatrixVecMultTDestVec(@dest2[0], sizeof(double), @x[0], @y[0], cMtxSize*sizeof(double), cMtxSize*sizeof(double), cMtxSize - 1, cMtxSize - 1, 2, -0.2);
     endTime2 := MtxGetTime;

     startTime3 := MtxGetTime;
     FMAMatrixVecMultTDestVec(@dest3[0], sizeof(double), @x[0], @y[0], cMtxSize*sizeof(double), cMtxSize*sizeof(double), cMtxSize - 1, cMtxSize - 1, 2, -0.2);
     endTime3 := MtxGetTime;


     Status(Format('%.2f,  %.2f,   %.2f', [(endTime1 - startTime1)/mtxFreq*1000,  (endTime2 - startTime2)/mtxFreq*1000, (endTime3 - startTime3)/mtxFreq*1000]));
     Check(CheckMtx(dest1, dest3), 'Error transposed AVX vector multiplication (destination = vector) failed');

     Check(CheckMtx(dest1, dest2), 'Error transposed vector multiplication (destination = vector) failed');

     dest3 := Copy(dest2, 0, Length(dest2));

     startTime1 := MtxGetTime;
     GenericMtxVecMultT(@dest1[0], sizeof(double), @x[0], @y[0], cMtxSize*sizeof(double), cMtxSize*sizeof(double), cMtxSize - 1, cMtxSize - 1, 2, -0.2);
     endTime1 := MtxGetTime;

     startTime2 := MtxGetTime;
     AVXMatrixVectMultT(@dest2[0], sizeof(double), @x[0], @y[0], cMtxSize*sizeof(double), cMtxSize*sizeof(double), cMtxSize - 1, cMtxSize - 1, 2, -0.2);
     endTime2 := MtxGetTime;

     startTime3 := MtxGetTime;
     FMAMatrixVectMultT(@dest3[0], sizeof(double), @x[0], @y[0], cMtxSize*sizeof(double), cMtxSize*sizeof(double), cMtxSize - 1, cMtxSize - 1, 2, -0.2);
     endTime3:= MtxGetTime;

     Status(Format('%.2f,  %.2f,  %.2f', [(endTime1 - startTime1)/mtxFreq*1000,  (endTime2 - startTime2)/mtxFreq*1000, (endTime3 - startTime3)/mtxFreq*1000]));

     Check(CheckMtx(dest1, dest2), 'Error transposed vector multiplication failed');
     Check(CheckMtx(dest1, dest3), 'Error transposed vector multiplication failed');

     startTime1 := MtxGetTime;
     GenericMtxVecMult(@dest1[0], sizeof(double), @x[0], @y[0], cMtxSize*sizeof(double), sizeof(double), cMtxSize, cMtxSize, 2, -0.2);
     endTime1 := MtxGetTime;

     startTime2 := MtxGetTime;
     AVXMatrixVectMultUnAlignedVAligned(@dest2[0], sizeof(double), @x[0], @y[0], cMtxSize*sizeof(double), sizeof(double), cMtxSize, cMtxSize, 2, -0.2);
     endTime2 := MtxGetTime;

     startTime3 := MtxGetTime;
     FMAMatrixVectMultUnAlignedVAligned(@dest3[0], sizeof(double), @x[0], @y[0], cMtxSize*sizeof(double), sizeof(double), cMtxSize, cMtxSize, 2, -0.2);
     endTime3:= MtxGetTime;

     Status(Format('%.2f,  %.2f,  %.2f', [(endTime1 - startTime1)/mtxFreq*1000,  (endTime2 - startTime2)/mtxFreq*1000, (endTime3 - startTime3)/mtxFreq*1000]));

     Check(CheckMtx(dest1, dest2), 'Error v aligned vector multiplication failed');
     Check(CheckMtx(dest1, dest3), 'Error v aligned multiplication failed');

     FreeMem(xa);
     FreeMem(ya);
end;


procedure TestFMAMatrixOperations.TestFMAMultMod16;
var p1, p2, p3, p4, p5 : PDouble;
    pMem1, pMem2, pMem3, pMem4, pMem5 : PByte;
    start1, stop1, start2, stop2 : int64;
    start3, stop3 : int64;
    outIdx : Integer;
    pp3, pp4 : PDouble;
    i : integer;
    res : boolean;
const cMtxWidth = 256;
      cMtxSize = cMtxWidth*cMtxWidth;
begin
     FillAlignedMtx( cMtxSize, p1, pMem1);
     FillAlignedMtx( cMtxSize, p2, pMem2);
     FillAlignedMtx( cMtxSize, p3, pMem3);
     FillAlignedMtx( cMtxSize, p4, pMem4);
     FillAlignedMtx( cMtxSize, p5, pMem5);

     pp3 := p3;
     pp4 := p4;
     for i := 0 to cMtxSize - 1 do
     begin
          pp3^ := i;
          pp4^ := i;
          inc(pp3);
          inc(pp4);
     end;

     GenericMtxMultTransp(p3, cMtxWidth*sizeof(double), p1, p2, 16, 16, 16, 16, 16*sizeof(double), cMtxWidth*sizeof(double));
     FMAMatrixMultAlignedEvenW1EvenH2TransposedMod16(p4, cMtxWidth*sizeof(double), p1, p2, 16, 16, 16, 16, 16*sizeof(double), cMtxWidth*sizeof(double));

     res := CheckMtxIdx(p3, p4, cMtxSize, outidx, 1e-6);
     Check(res, '16x16 Error Matrix mult failed @' + IntToStr(outidx));

     pp3 := p3;
     pp4 := p4;
     for i := 0 to cMtxSize - 1 do
     begin
          pp3^ := i;
          pp4^ := i;
          inc(pp3);
          inc(pp4);
     end;

     GenericMtxMultTransp(p3, cMtxWidth*sizeof(double), p1, p2, 32, 32, 32, 11, 32*sizeof(double), cMtxWidth*sizeof(double));
     FMAMatrixMultAlignedEvenW1OddH2TransposedMod16(p4, cMtxWidth*sizeof(double), p1, p2, 32, 32, 32, 11, 32*sizeof(double), cMtxWidth*sizeof(double));

     res := CheckMtxIdx(p3, p4, cMtxSize, outidx, 1e-6);
     Check(res, '32x11 Error Matrix mult failed @' + IntToStr(outidx));


     start1 := MtxGetTime;
     GenericMtxMultTransp(p3, cMtxWidth*sizeof(double), p1, p2, cMtxWidth, cMtxWidth, cMtxWidth, cMtxWidth, cMtxWidth*sizeof(double), cMtxWidth*sizeof(double));
     stop1 := MtxGetTime;

     start2 := MtxGetTime;
     AVXMatrixMultAlignedEvenW1EvenH2TransposedMod16(p4, cMtxWidth*sizeof(double), p1, p2, cMtxWidth, cMtxWidth, cMtxWidth, cMtxWidth, cMtxWidth*sizeof(double), cMtxWidth*sizeof(double));
     stop2 := MtxGetTime;

     start3 := MtxGetTime;
     FMAMatrixMultAlignedEvenW1EvenH2TransposedMod16(p5, cMtxWidth*sizeof(double), p1, p2, cMtxWidth, cMtxWidth, cMtxWidth, cMtxWidth, cMtxWidth*sizeof(double), cMtxWidth*sizeof(double));
     stop3 := MtxGetTime;

     Check(CheckMtxIdx(p3, p4, cMtxSize, outidx, 1e-6), 'Error Matrix mult failed');
     Check(CheckMtxIdx(p3, p5, cMtxSize, outidx, 1e-6), 'Error Matrix AVX mult failed');

     Status(Format('%.2f, %.2f, %.2f', [(stop1 - start1)/mtxFreq*1000, (stop2 - start2)/mtxFreq*1000, (stop3 - start3)/mtxFreq*1000]));

     FreeMem(pMem1);
     FreeMem(pMem2);
     FreeMem(pMem3);
     FreeMem(pMem4);
     FreeMem(pMem5);
end;

procedure TestFMAMatrixOperations.TestFMAMult;
const mt1 : Array[0..15] of double = (0, 1, 2, 0, 3, 4, 5, 0, 6, 7, 8, 0, 0, 0, 0, 0);
      mt2 : Array[0..15] of double = (-1, 0, 1, 0, 2, 3, 4, 0, 5, 6, 7, 0, 0, 0, 0, 0);
      mt3 : Array[0..15] of double = (12, 15, 18, 0, 30, 42, 54, 0, 48, 69, 90, 0, 0, 0, 0, 0);
      mt4 : Array[0..15] of double = (0, 1, 2, 4, 3, 4, 5, 5, 6, 7, 8, 6, 7, 8, 9, 10);
      mt5 : Array[0..15] of double = (-1, 0, 1, 1, 2, 3, 4, 2, 5, 6, 7, 3, 4, 5, 6, 7);
      mt6 : Array[0..15] of double = (28, 35, 42, 36, 50, 67, 84, 61, 72, 99, 126, 86, 94, 128, 162, 120);

var dest : Array[0..15] of double;
    d, d1, x, y : TDoubleDynArray;

    px, py : PDouble;
begin
     FillChar(dest, sizeof(dest), 0);
     FMAMatrixMultUnaligned(@dest[0], 4*sizeof(double), @mt1[0], @mt2[0], 3, 3, 3, 3, 4*sizeof(double), 4*sizeof(double));

     Check(CheckMtx(dest, mt3, -1, -1, 1e-10), 'Mult Matrix error');

     FillChar(dest, sizeof(dest), 0);
     FMAMatrixMultUnaligned(@dest[0], 4*sizeof(double), @mt4[0], @mt5[0], 4, 4, 4, 4, 4*sizeof(double), 4*sizeof(double));

     Check(CheckMtx(dest, mt6, -1, -1, 1e-10), 'Mult Matrix error');

     FillMatrix(43*43, x, y, px, py);
     SetLength( d, 43*43 );
     SetLength(d1, 43*43 );

     FreeMem(px);
     FreeMem(py);

     GenericMtxMult( @d1[0], 43*sizeof(double), @x[0], @y[0], 43, 43, 43, 43, 43*sizeof(double), 43*sizeof(double));
     FMAMatrixMult( @d[0], 43*sizeof(double), @x[0], @y[0], 43, 43, 43, 43, 43*sizeof(double), 43*sizeof(double), nil);

     Check(CheckMtx(d1, d, -1, -1, 1e-10), 'Mtx mult failed');
end;

procedure TestFMAMatrixOperations.TestFMABigMult;
var x, y, dest1, dest2 : TDoubleDynArray;
    xa, ya, dest2a, dest3a : PDouble;
    za : PDouble;
    idx : integer;
    startTime1: Int64;
    endTime1: Int64;
    startTime2: Int64;
    endTime2: Int64;
    startTime3: Int64;
    endTime3: Int64;
    startTime4: Int64;
    endTime4: Int64;
    res : boolean;
    blk : PDouble;
    pm1, pm2 : PByte;
const cMtxWidth = 1472;
      cMtxHeight = 512;
      cMtxSize = cMtxWidth*cMtxHeight;
      cMtxDestSize = cMtxWidth*cMtxWidth;
      cMtxLinewidth = cMtxWidth*sizeof(double);
      cMtxLineWidth2 = cMtxHeight*sizeof(double);
begin
     randomize;
     AllocAlignedMtx(cMtxSize, xa, pm1);
     AllocAlignedMtx(cMtxSize, ya, pm2);
     SetLength(x, cMtxSize);
     SetLength(y, cMtxSize);

     SetLength(dest1, cMtxWidth*cMtxWidth);
     SetLength(dest2, cMtxWidth*cMtxWidth);

     Move( xa^, x[0], Length(x)*sizeof(double));
     Move( ya^, y[0], Length(y)*sizeof(double));

     blk := AllocMem((2*cMtxDestSize + cMtxSize + 32)*sizeof(double) + 32);
     dest2a := blk;
     inc(Pbyte(dest2a), 32 - TASMNativeUint(blk) and $1F);
     dest3a := dest2a;
     inc(dest3a, cMtxDestSize);
     inc(PByte(dest3a), 32 - TASMNativeUint(dest3a) and $1F);
     za := dest3a;
     inc(za, cMtxDestSize);
     inc(PByte(za), 32 - TASMNativeUint(dest3a) and $1F);

     startTime1 := MtxGetTime;
     GenericMtxMult(@dest1[0], cMtxLineWidth, @x[0], @y[0], cMtxWidth, cMtxheight, cMtxHeight, cMtxWidth, cMtxLinewidth, cMtxLinewidth2);
     endTime1 := MtxGetTime;

     startTime2 := MtxGetTime;
     FMAMatrixMultUnAligned(@dest2[0], cMtxLineWidth, @x[0], @y[0], cMtxWidth, cMtxheight, cMtxHeight, cMtxWidth, cMtxLinewidth, cMtxLinewidth2);
     endTime2 := MtxGetTime;

     startTime3 := MtxGetTime;
     FMAMatrixMultAligned(dest2a, cMtxLineWidth, xa, ya, cMtxWidth, cMtxheight, cMtxHeight, cMtxWidth, cMtxLinewidth, cMtxLinewidth2);
     endTime3 := MtxGetTime;

     startTime4 := MtxGetTime;
     GenericMtxTranspose(za, cMtxLineWidth, ya, cMtxLineWidth2, cMtxHeight, cMtxWidth);
     FMAMatrixMultTransposed(dest3a, cMtxLineWidth, xa, za, cMtxWidth, cMtxheight, cMtxWidth, cMtxheight, cMtxLinewidth, cMtxLinewidth);
     endTime4 := MtxGetTime;

     Status(Format('%.2f, %.2f, %.2f, %.2f', [(endTime1 - startTime1)/mtxFreq*1000, (endTime2 - startTime2)/mtxFreq*1000,
                 (endTime3 - startTime3)/mtxFreq*1000, (endTime4 - startTime4)/mtxFreq*1000]));

     res := CheckMtxIdx(dest1, dest2, idx);
     if not res then
        Status(IntToStr(idx));
     check(res);

     Move(dest2a^, dest2[0], length(dest2)*sizeof(double));
     res := CheckMtxIdx(dest1, dest2, idx);
     if not res then
        Status(IntToStr(idx));
     check(res);

     Move(dest3a^, dest2[0], length(dest2)*sizeof(double));
     res := CheckMtxIdx(dest1, dest2, idx);
     if not res then
        Status(IntToStr(idx));
     check(res);

     FreeMem(blk);

     FreeMem(pm1);
     FreeMem(pm2);
end;

procedure TestFMAMatrixOperations.TestFMADiagMtxMult;
var a1, a : Array[0..143] of double;
    b : Array[0..143] of double;
    c, c1 : Array[0..143] of double;
    w, h : Integer;
  procedure Init;
  var counter : integer;
  begin
       for counter := 0 to Length(a) - 1 do
       begin
            a1[counter] := counter + 1;
            a[counter] := a1[counter];
            b[counter] := counter*2 - 1;
            c[counter] := 0;
            c1[counter] := 0;
       end;
  end;
begin
     for w := 3 to 12 do
     begin
          for h := w to 12 do
          begin
               Init;
               GenericMtxMultTria2T1StoreT1(@a[0], h*sizeof(double), @b[0], h*sizeof(double), w, h, w, w);
               FMAMtxMultTria2T1StoreT1(@a1[0], h*sizeof(double), @b[0], h*sizeof(double), w, h, w, w);
               Check(CheckMtx(a, a1), 'FMAMtxMultTria2T1StoreT1');
          end;
     end;

     for w := 3 to 12 do
     begin
          for h := w to 12 do
          begin
               Init;
               GenericMtxMultTria2TUpperUnit(@c[0], h*sizeof(double), @a[0], h*sizeof(double), @b[0], h*sizeof(double), w, h, w, w);
               FMAMtxMultTria2TUpperUnit(@c1[0], h*sizeof(double), @a1[0], h*sizeof(double), @b[0], h*sizeof(double), w, h, w, w);
               Check(CheckMtx(c, c1), 'FMAMtxMultTria2TUpperUnit');
          end;
     end;
end;


initialization
  if IsFMAPresent then
     RegisterTest(TestFMAMatrixOperations{$IFNDEF FPC}.Suite{$ENDIF});

end.

