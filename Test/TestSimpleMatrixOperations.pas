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

unit TestSimpleMatrixOperations;
{

  Delphi DUnit Testfall
  ----------------------
  Diese Unit enthält ein Codegerüst einer Testfallklasse, das vom Testfall-Experten
  erzeugt wurde. Ändern Sie den erzeugten Code, damit die Methoden aus der 
  getesteten Unit korrekt eingerichtet und aufgerufen werden.

}

interface

uses
  TestFramework, Classes, SysUtils, Types, SimpleMatrixOperations, Matrix, BaseMatrixTestCase;

type
  // testmethoden für die matrix funktionen
  TestMatrixOperations = class(TBaseMatrixTestCase)
  published
   procedure TestAdd;
   procedure TestSub;
   procedure TestMult;
   procedure TestTranspose;
   procedure TestCopy;
   procedure TestNormalize;
  end;

  TASMMatrixOperations = class(TBaseMatrixTestCase)
  private
    fPerfFreq : Int64;
  public
    procedure SetUp; override;
    procedure TearDown; override;

    procedure FillMatrix(mtxSize : integer; var x, y : TDoubleDynArray; var p1, p2 : PDouble);
  published
    procedure TestMatrixASMCopy;
    procedure TestASMAdd;
    procedure TestBigASMAdd;
    procedure TestMultASMEvenW1EvenW2;
    procedure TestMultASMEvenW1OddW2;
    procedure TestMultASMOddW1EvenW2;
    procedure TestMultASMOddW1OddW2;
    procedure TestMultASMEvenW1EvenH2Transposed;
    procedure TestMultASMEvenW1OddH2Transposed;
    procedure TestMultASMOddW1EvenH2Transposed;
    procedure TestMultASMOddW1OddH2Transposed;
    procedure TestMultASMMatrixVectorEvenW1;
    procedure TestMtxMeanRow;
    procedure TestMtxMeanColumn;
    procedure TestMtxSumColumn;
    procedure TestMtxSumRow;
    procedure TestMinMaxASM;
    procedure TestMatrixBigASMMinMax;
    procedure TestMultASM;
    procedure TestMultTransposed;
    procedure TestBigASMMult;
    procedure TestBigTiledMult;
    procedure TestBigTiledMult2;
    procedure TestTransposedASMEvenWEvenH;
    procedure TestTransposedASMEvenWOddH;
    procedure TestTransposeASSMOddWOddH;
    procedure TestBigTransposedASM;
    procedure TestElemWiseNorm2;
    procedure TestMtxNormalizeRow;
    procedure TestMtxNormalizeColumn;
    procedure TestMtxBigNormalizeRow;
    procedure TestThreadMatrixMult;
    procedure TestThreadMatrixAddSub;
    procedure TestThreadMatrixAddAndScale;
    procedure TestStrassenMult;
  end;

  TASMatrixBlockSizeSetup = class(TBaseMatrixTestCase)
  published
    procedure TestSetupTransposedOptSize;
    procedure TestSetupBestBlockSize;
    procedure TestSetupBestBlockVectorSize;
    procedure TestSetupBlock;
    procedure TestSetupBlockedVectorMatrixMultSize;

  end;

implementation

uses Windows, Dialogs, ASMMatrixOperations, ThreadedMatrixOperations, MtxThreadPool,
     BlockSizeSetup, math,
     {$IFDEF CPUX64}
     ASMMatrixMultOperationsx64, ASMMatrixVectorMultOperationsx64, ASMMatrixMultTransposedOperationsx64,
     ASMMatrixTransposeOperationsx64, ASMMatrixNormOperationsx64,
     ASMMatrixMeanOperationsx64, ASMMatrixSumOperationsx64,
     {$ELSE}
     ASMMatrixMultOperations, ASMMatrixVectorMultOperations, ASMMatrixMultTransposedOperations,
     ASMMatrixTransposeOperations, ASMMatrixNormOperations,
     ASMMatrixMeanOperations, ASMMatrixSumOperations,
     {$ENDIF}
     MatrixConst, ASMConsts;

procedure TestMatrixOperations.TestAdd;
const mt1 : Array[0..5] of double = (0, 1, 2, 3, 4, 5);
      mt2 : Array[0..5] of double = (1, 1, 1, 1, 1, 1);
      mt3 : Array[0..5] of double = (1, 2, 3, 4, 5, 6);
      mt4 : Array[0..3] of double = (1, 2, 4, 5);
var res : TDoubleDynArray;
begin
     res := GenericMtxAdd(@mt1, @mt2, 2, 3, sizeof(double)*2, sizeof(double)*2);

     CheckEqualsMem(@mt3, @res[0], sizeof(mt3), 'Error Matrix addition: ' + #13#10 + WriteMtxDyn(res, 2));

     res := GenericMtxAdd(@mt1, @mt2, 2, 2, sizeof(double)*3, sizeof(double)*3);
     CheckEqualsMem(@mt4, @res[0], sizeof(mt4), 'Error Matrix addition: ' + #13#10 + WriteMtxDyn(res, 2));
end;


procedure TASMMatrixOperations.FillMatrix(mtxSize: integer; var x,
  y: TDoubleDynArray; var p1, p2: PDouble);
var px : PDouble;
    py : PDouble;
    idx : integer;
begin
     SetLength(x, mtxSize);
     SetLength(y, mtxSize);

     p1 := GetMemory(mtxSize*sizeof(double));
     p2 := GetMemory(mtxSize*sizeof(double));

     // fill randomly:
     px := @x[0];
     py := @y[0];
     for Idx := 0 to mtxSize - 1 do
     begin
          px^ := random;
          py^ := random;
          inc(px);
          inc(py);
     end;

     px := p1;
     py := p2;
     for Idx := 0 to mtxSize - 1 do
     begin
          px^ := x[idx];
          py^ := y[idx];
          inc(px);
          inc(py);
     end;
end;

procedure TASMMatrixOperations.SetUp;
begin
     QueryPerformanceFrequency(fPerfFreq);
     InitMtxThreadPool;
end;

procedure TASMMatrixOperations.TearDown;
begin
     FinalizeMtxThreadPool;
end;

procedure TASMMatrixOperations.TestASMAdd;
const mt1 : Array[0..15] of double = (0, 1, 2, 0, 3, 4, 5, 0, 6, 7, 8, 0, 0, 0, 0, 0);
      mt2 : Array[0..15] of double = (-1, 0, 1, 0, 2, 3, 4, 0, 5, 6, 7, 0, 0, 0, 0, 0);
      mt3 : Array[0..15] of double = (-1, 1, 3, 0, 5, 7, 9, 0, 11, 13, 15, 0, 0, 0, 0, 0);
      mt4 : Array[0..15] of double = (0, 1, 2, 4, 3, 4, 5, 5, 6, 7, 8, 6, 7, 8, 9, 10);
      mt5 : Array[0..15] of double = (-1, 0, 1, 1, 2, 3, 4, 2, 5, 6, 7, 3, 4, 5, 6, 7);
      mt6 : Array[0..15] of double = (-1, 1, 3, 5, 5, 7, 9, 7, 11, 13, 15, 9, 11, 13, 15, 17);

var dest : Array[0..15] of double;
begin
     FillChar(dest, sizeof(dest), 0);
     ASMMatrixAdd(@dest[0], 4*sizeof(double), @mt4[0], @mt5[0], 4, 4, 4*sizeof(double), 4*sizeof(double));

     Check(CheckMtx(dest, mt6, -1, -1, 1e-10), 'Add Matrix error');

     FillChar(dest, sizeof(dest), 0);
     ASMMatrixAdd(@dest[0], 4*sizeof(double), @mt1[0], @mt2[0], 3, 3, 4*sizeof(double), 4*sizeof(double));

     Check(CheckMtx(dest, mt3, -1, -1, 1e-10), 'Add Matrix error');
end;

procedure TASMMatrixOperations.TestBigASMAdd;
var x, y, dest1, dest2 : Array of double;
    xa, ya, dest2a : PDouble;
    px, py : PDouble;
    idx : integer;
    startTime1: Int64;
    endTime1: Int64;
    startTime2: Int64;
    endTime2: Int64;
    startTime3: Int64;
    endTime3: Int64;
    res : boolean;
const cMtxWidth = 20000;
      cMtxHeight = 500;
      cMtxSize = (cMtxWidth + 2)*cMtxHeight;
      cMtxLinewidth = (cMtxWidth + 2)*sizeof(double);
begin
     randomize;
     SetLength(x, cMtxSize);
     SetLength(y, cMtxSize);
     SetLength(dest1, cMtxSize);
     SetLength(dest2, cMtxSize);
     xa := AllocMem(cMTxSize*sizeof(double));
     ya := AllocMem(cMTxSize*sizeof(double));
     dest2a := AllocMem(cMtxSize*sizeof(double));

     // fill randomly:
     px := @x[0];
     py := @y[0];
     for Idx := 0 to cMtxSize - 1 do
     begin
          px^ := random;
          py^ := random;
          inc(px);
          inc(py);
     end;

     Move(x[0], xa^, sizeof(double)*cMtxSize);
     Move(y[0], ya^, sizeof(double)*cMtxSize);

     QueryPerformanceCounter(startTime1);
     GenericMtxAdd(@dest1[0], cMtxLineWidth, @x[0], @y[0], cMtxWidth, cMtxheight, cMtxLinewidth, cMtxLinewidth);
     QueryPerformanceCounter(endTime1);

     QueryPerformanceCounter(startTime2);
     ASMMatrixAdd(@dest2[0], cMtxLineWidth, @x[0], @y[0], cMtxWidth, cMtxheight, cMtxLinewidth, cMtxLinewidth);
     QueryPerformanceCounter(endTime2);

     QueryPerformanceCounter(startTime3);
     ASMMatrixAdd(dest2a, cMtxLineWidth, xa, ya, cMtxWidth, cMtxheight, cMtxLinewidth, cMtxLinewidth);
     QueryPerformanceCounter(endTime3);

     Status(Format('%.2f, %.2f, %.2f', [(endTime1 - startTime1)/fPerfFreq*1000, (endTime2 - startTime2)/fPerfFreq*1000,
                 (endTime3 - startTime3)/fPerfFreq*1000]));

     res := CheckMtxIdx(dest1, dest2, idx, cMtxWidth, cMtxHeight);
     if not res then
        Status(IntToStr(idx));
     check(res);

     Move(dest2a^, dest2[0], length(dest2)*sizeof(double));
     res := CheckMtxIdx(dest1, dest2, idx, cMtxWidth, cMtxHeight);
     if not res then
        Status(IntToStr(idx));
     check(res);

     FreeMem(xa);
     FreeMem(ya);
     FreeMem(dest2a);
end;

procedure TASMMatrixOperations.TestBigASMMult;
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
const cMtxWidth = 2000;
      cMtxHeight = 500;
      cMtxSize = cMtxWidth*cMtxHeight;
      cMtxDestSize = cMtxWidth*cMtxWidth;
      cMtxLinewidth = cMtxWidth*sizeof(double);
      cMtxLineWidth2 = cMtxHeight*sizeof(double);
begin
     randomize;
     FillMatrix(cMtxSize, x, y, xa, ya);
     SetLength(dest1, cMtxWidth*cMtxWidth);
     SetLength(dest2, cMtxWidth*cMtxWidth);

     blk := AllocMem((2*cMtxDestSize + cMtxSize + 16)*sizeof(double) + 16);
     dest2a := blk;
     inc(Pbyte(dest2a), 16 - TASMNativeInt(blk) and $F);
     dest3a := dest2a;
     inc(dest3a, cMtxDestSize + cMtxDestSize mod 2);
     za := dest3a;
     inc(za, cMtxDestSize + cMtxDestSize mod 2);

     QueryPerformanceCounter(startTime1);
     GenericMtxMult(@dest1[0], cMtxLineWidth, @x[0], @y[0], cMtxWidth, cMtxheight, cMtxHeight, cMtxWidth, cMtxLinewidth, cMtxLinewidth2);
     QueryPerformanceCounter(endTime1);

     QueryPerformanceCounter(startTime2);
     ASMMatrixMult(@dest2[0], cMtxLineWidth, @x[0], @y[0], cMtxWidth, cMtxheight, cMtxHeight, cMtxWidth, cMtxLinewidth, cMtxLinewidth2);
     QueryPerformanceCounter(endTime2);

     QueryPerformanceCounter(startTime3);
     ASMMatrixMult(dest2a, cMtxLineWidth, xa, ya, cMtxWidth, cMtxheight, cMtxHeight, cMtxWidth, cMtxLinewidth, cMtxLinewidth2);
     QueryPerformanceCounter(endTime3);

     QueryPerformanceCounter(startTime4);
     GenericMtxTranspose(za, cMtxLineWidth, ya, cMtxLineWidth2, cMtxHeight, cMtxWidth);
     ASMMatrixMultAlignedEvenW1EvenH2Transposed(dest3a, cMtxLineWidth, xa, za, cMtxWidth, cMtxheight, cMtxWidth, cMtxheight, cMtxLinewidth, cMtxLinewidth);
     QueryPerformanceCounter(endTime4);

     Status(Format('%.2f, %.2f, %.2f, %.2f', [(endTime1 - startTime1)/fPerfFreq*1000, (endTime2 - startTime2)/fPerfFreq*1000,
                 (endTime3 - startTime3)/fPerfFreq*1000, (endTime4 - startTime4)/fPerfFreq*1000]));

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
end;

procedure TASMMatrixOperations.TestBigTiledMult;
var x, y, dest1, dest2 : TDoubleDynArray;
    xa, ya, dest2a, dest3a : PDouble;
    idx : integer;
    startTime1: Int64;
    endTime1: Int64;
    startTime2: Int64;
    endTime2: Int64;
    startTime4: Int64;
    endTime4: Int64;
    vecStartTime1, vecEndTime1 : int64;
    vecStartTime2, vecEndTime2 : int64;
    vecStartTime3, vecEndTime3 : int64;
    res : boolean;
    startTime3, endTime3 : int64;
    startTime5, endTime5 : int64;
const cMtxWidth = 5*cCacheMtxSize;
      cMtxHeight = cCacheMtxSize*4;
      cMtxSize = cMtxWidth*cMtxHeight;
      cMtxLinewidth = cMtxWidth*8;
      cMtxLineWidth2 = cMtxHeight*8;
begin
     SetThreadAffinityMask(GetCurrentThread, 1);
     randomize;
     FillMatrix(cMtxSize, x, y, xa, ya);

     SetLength(dest1, cMtxHeight*cMtxHeight);
     SetLength(dest2, cMtxHeight*cMtxHeight);
     dest2a := AllocMem(cMtxHeight*cMtxHeight*sizeof(double));
     dest3a := AllocMem(cMtxHeight*cMtxHeight*sizeof(double));

     QueryPerformanceCounter(startTime5);
     BlockedMatrixMultiplication(@dest2[0], cMtxLineWidth2, @x[0], @y[0], cMtxWidth, cMtxheight, cMtxHeight, cMtxWidth, cMtxLinewidth, cMtxLinewidth2, 352);
     QueryPerformanceCounter(endTime5);

     QueryPerformanceCounter(startTime1);
     GenericMtxMult(@dest1[0], cMtxLineWidth2, @x[0], @y[0], cMtxWidth, cMtxheight, cMtxHeight, cMtxWidth, cMtxLinewidth, cMtxLinewidth2);
     QueryPerformanceCounter(endTime1);

     QueryPerformanceCounter(startTime2);
     GenericBlockedMatrixMultiplication(@dest2[0], cMtxLineWidth2, @x[0], @y[0], cMtxWidth, cMtxheight, cMtxHeight, cMtxWidth, cMtxLinewidth, cMtxLinewidth2, 352);
     QueryPerformanceCounter(endTime2);

     res := CheckMtxIdx(dest1, dest2, idx);
     if not res then
        Status(IntToStr(idx));
     check(res);

     res := CheckMtxIdx(dest1, dest2, idx);
     if not res then
        Status(IntToStr(idx));
     check(res);

     QueryPerformanceCounter(startTime4);
     BlockedMatrixMultiplication(dest3a, cMtxLineWidth2, xa, ya, cMtxWidth, cMtxheight, cMtxHeight, cMtxWidth, cMtxLinewidth, cMtxLinewidth2, 352);
     QueryPerformanceCounter(endTime4);

     QueryPerformanceCounter(startTime3);
     BlockedMatrixMultiplicationDirect(dest2a, cMtxLineWidth2, xa, ya, cMtxWidth, cMtxheight, cMtxHeight, cMtxWidth, cMtxLinewidth, cMtxLinewidth2, 352);
     QueryPerformanceCounter(endTime3);


     Status(Format('%.2f, %.2f, %.2f, %.2f, %.2f', [(endTime1 - startTime1)/fPerfFreq*1000, (endTime2 - startTime2)/fPerfFreq*1000,
                (endTime3 - startTime3)/fPerfFreq*1000, (endTime4 - startTime4)/fPerfFreq*1000,
                (endTime5 - startTime5)/fPerfFreq*1000]));

     exit;
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

     FreeMem(dest3a);

     // matrix vector operations:
     QueryPerformanceCounter(vecstartTime1);
     GenericMtxMult(@dest1[0], sizeof(double), @x[0], @y[0], cMtxWidth, cMtxheight, 1, cMtxWidth, cMtxLinewidth, sizeof(double));
     QueryPerformanceCounter(vecendTime1);

     QueryPerformanceCounter(vecstartTime2);
     BlockedMatrixVectorMultiplication(@dest2[0], sizeof(double), @x[0], @y[0], cMtxWidth, cMtxheight, cMtxWidth, cMtxLinewidth);
     QueryPerformanceCounter(vecendTime2);

     QueryPerformanceCounter(startTime2);
     ASMMatrixMult(@dest2[0], sizeof(double), @x[0], @y[0], cMtxWidth, cMtxheight, 1, cMtxWidth, cMtxLinewidth, sizeof(double));
     QueryPerformanceCounter(endTime2);

     QueryPerformanceCounter(startTime3);
     ASMMatrixMult(dest2a, sizeof(double), xa, ya, cMtxWidth, cMtxheight, 1, cMtxWidth, cMtxLinewidth, sizeof(double));
     QueryPerformanceCounter(endtime3);

     QueryPerformanceCounter(vecstartTime3);
     BlockedMatrixVectorMultiplication(dest2a, sizeof(double), xa, ya, cMtxWidth, cMtxheight, cMtxWidth, cMtxLinewidth);
     QueryPerformanceCounter(vecendTime3);

     Status(Format('%.2f, %.2f, %.2f, %.2f, %.2f',
         [(vecendTime1 - vecstartTime1)/fPerfFreq*1000, (vecendTime2 - vecstartTime2)/fPerfFreq*1000,
          (vecendTime3 - vecstartTime3)/fPerfFreq*1000, (endTime2 - startTime2)/fPerfFreq*1000,
          (endTime3 - startTime3)/fPerfFreq*1000]));

     res := CheckMtxIdx(dest1, dest2, idx, 1, cMtxHeight);
     if not res then
        Status(IntToStr(idx));
     check(res);

     Move(dest2a^, dest2[0], length(dest2)*sizeof(double));
     res := CheckMtxIdx(dest1, dest2, idx, 1, cMtxHeight);
     if not res then
        Status(IntToStr(idx));
     check(res);

     FreeMem(xa);
     FreeMem(ya);
     FreeMem(dest2a);
end;

procedure TASMMatrixOperations.TestBigTiledMult2;
var x, y, dest1, dest2 : TDoubleDynArray;
    xa, ya, dest2a, dest3a : PDouble;
    idx : integer;
    startTime1: Int64;
    endTime1: Int64;
    startTime2: Int64;
    endTime2: Int64;
    startTime4, endTime4 : Int64;
    vecStartTime1, vecEndTime1 : int64;
    vecStartTime2, vecEndTime2 : int64;
    vecStartTime3, vecEndTime3 : int64;
    res : boolean;
    startTime3, endTime3 : int64;
const cMtxWidth = 3*(cCacheMtxSize - 1);
      cMtxHeight = 3*(cCacheMtxSize - 2);
      //cMtxWidth = 3453;
      //cMtxHeight = 2451;
      cMtxSize = cMtxWidth*cMtxHeight;
      cMtxLinewidth = cMtxWidth*8;
      cMtxLineWidth2 = cMtxHeight*8;
begin
     randseed := 421;
     FillMatrix(cMtxSize, x, y, xa, ya);

     SetLength(dest1, cMtxHeight*cMtxHeight);
     SetLength(dest2, cMtxHeight*cMtxHeight);
     dest2a := AllocMem(cMtxHeight*cMtxHeight*sizeof(double));
     dest3a := AllocMem(cMtxHeight*cMtxHeight*sizeof(double));

     QueryPerformanceCounter(startTime1);
     GenericMtxMult(@dest1[0], cMtxLineWidth2, @x[0], @y[0], cMtxWidth, cMtxheight, cMtxHeight, cMtxWidth, cMtxLinewidth, cMtxLinewidth2);
     QueryPerformanceCounter(endTime1);

     QueryPerformanceCounter(startTime2);
     BlockedMatrixMultiplicationDirect(@dest2[0], cMtxLineWidth2, @x[0], @y[0], cMtxWidth, cMtxheight, cMtxHeight, cMtxWidth, cMtxLinewidth, cMtxLinewidth2);
     QueryPerformanceCounter(endTime2);

     res := CheckMtxIdx(dest1, dest2, idx);
     if not res then
        Status(IntToStr(idx));
     check(res);

     QueryPerformanceCounter(startTime3);
     BlockedMatrixMultiplicationDirect(dest2a, cMtxLineWidth2, xa, ya, cMtxWidth, cMtxheight, cMtxHeight, cMtxWidth, cMtxLinewidth, cMtxLinewidth2, 256);
     QueryPerformanceCounter(endTime3);

     QueryPerformanceCounter(startTime4);
     BlockedMatrixMultiplication(dest3a, cMtxLineWidth2, xa, ya, cMtxWidth, cMtxheight, cMtxHeight, cMtxWidth, cMtxLinewidth, cMtxLinewidth2, 256);
     QueryPerformanceCounter(endTime4);

     Status(Format('%.2f, %.2f, %.2f, %.2f', [(endTime1 - startTime1)/fPerfFreq*1000,
       (endTime2 - startTime2)/fPerfFreq*1000, (endTime3 - startTime3)/fPerfFreq*1000, (endTime4 - startTime4)/fPerfFreq*1000]));

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

     FreeMem(dest3a);

     // matrix vector operations:
     QueryPerformanceCounter(vecstartTime1);
     GenericMtxMult(@dest1[0], sizeof(double), @x[0], @y[0], cMtxWidth, cMtxheight, 1, cMtxWidth, cMtxLinewidth, sizeof(double));
     QueryPerformanceCounter(vecendTime1);

     QueryPerformanceCounter(vecstartTime2);
     BlockedMatrixVectorMultiplication(@dest2[0], sizeof(double), @x[0], @y[0], cMtxWidth, cMtxheight, cMtxWidth, cMtxLinewidth);
     QueryPerformanceCounter(vecendTime2);

     QueryPerformanceCounter(startTime2);
     ASMMatrixMult(@dest2[0], sizeof(double), @x[0], @y[0], cMtxWidth, cMtxheight, 1, cMtxWidth, cMtxLinewidth, sizeof(double));
     QueryPerformanceCounter(endTime2);

     QueryPerformanceCounter(startTime3);
     ASMMatrixMult(dest2a, sizeof(double), xa, ya, cMtxWidth, cMtxheight, 1, cMtxWidth, cMtxLinewidth, sizeof(double));
     QueryPerformanceCounter(endtime3);

     QueryPerformanceCounter(vecstartTime3);
     BlockedMatrixVectorMultiplication(dest2a, sizeof(double), xa, ya, cMtxWidth, cMtxheight, cMtxWidth, cMtxLinewidth);
     QueryPerformanceCounter(vecendTime3);

     Status(Format('%.2f, %.2f, %.2f, %.2f, %.2f',
         [(vecendTime1 - vecstartTime1)/fPerfFreq*1000, (vecendTime2 - vecstartTime2)/fPerfFreq*1000,
          (vecendTime3 - vecstartTime3)/fPerfFreq*1000, (endTime2 - startTime2)/fPerfFreq*1000,
          (endTime3 - startTime3)/fPerfFreq*1000]));

     res := CheckMtxIdx(dest1, dest2, idx, 1, cMtxHeight);
     if not res then
        Status(IntToStr(idx));
     check(res);

     Move(dest2a^, dest2[0], length(dest2)*sizeof(double));
     res := CheckMtxIdx(dest1, dest2, idx, 1, cMtxHeight);
     if not res then
        Status(IntToStr(idx));
     check(res);

     FreeMem(xa);
     FreeMem(ya);
     FreeMem(dest2a);
end;

procedure TASMMatrixOperations.TestBigTransposedASM;
var x, dest1, dest2 : Array of double;
    xa, dest1a, dest2a : PDouble;
    px : PDouble;
    idx : integer;
    startTime1: Int64;
    endTime1: Int64;
    startTime2: Int64;
    endTime2: Int64;
    res : boolean;
const
      cMtxWidth = 2000;
      cMtxHeight = 500;
      cMtxSize = cMtxWidth*cMtxHeight;
      cMtxLinewidth = cMtxWidth*sizeof(double);
      cMtxLineWidth2 = cMtxHeight*sizeof(double);
begin
     randomize;
     SetLength(x, cMtxSize);
     SetLength(dest1, cMtxWidth*cMtxWidth);
     SetLength(dest2, cMtxWidth*cMtxWidth);
     GetMem(xa, cMTxSize*sizeof(double));

     GetMem(dest1a, cMtxWidth*cMtxWidth*sizeof(double));
     GetMem(dest2a, cMtxWidth*cMtxWidth*sizeof(double));

     ZeroMemory(dest1a, cMtxWidth*cMtxWidth*sizeof(double));
     ZeroMemory(dest2a, cMtxWidth*cMtxWidth*sizeof(double));

     // fill randomly:
     px := @x[0];
     for Idx := 0 to cMtxSize - 1 do
     begin
          px^ := random;
          inc(px);
     end;

     px := xa;
     for Idx := 0 to cMtxSize - 1 do
     begin
          px^ := x[idx];
          inc(px);
     end;

     QueryPerformanceCounter(startTime1);
     GenericMtxTranspose(@dest1[0], cMtxLineWidth2, @x[0], cMtxLinewidth, cMtxWidth, cMtxheight);
     QueryPerformanceCounter(endTime1);

     QueryPerformanceCounter(startTime2);
     ASMMatrixTransposeAlignedEvenWEvenH(dest1a, cMtxLineWidth2, xa, cMtxLinewidth, cMtxWidth, cMtxheight);
     QueryPerformanceCounter(endTime2);

     Status(Format('%.2f, %.2f', [(endTime1 - startTime1)/fPerfFreq*1000, (endTime2 - startTime2)/fPerfFreq*1000]));

     Move(dest1a^, dest2[0], length(dest2)*sizeof(double));
     res := CheckMtxIdx(dest1, dest2, idx);
     if not res then
        Status(IntToStr(idx));
     check(res);

     FreeMem(dest1a);
     FreeMem(dest2a);
     FreeMem(xa);
end;

procedure TASMMatrixOperations.TestElemWiseNorm2;
var x, y : TDoubleDynArray;
    xa, ya : PDouble;
    startTime1: Int64;
    endTime1: Int64;
    startTime2: Int64;
    endTime2: Int64;
    startTime3: Int64;
    endTime3: Int64;
    norm1, norm2, norm3 : double;
const cMtxWidth = 5010;
      cMtxHeight = 4983;
      cMtxSize = (cMtxWidth)*cMtxHeight;
      cMtxLinewidth = (cMtxWidth)*sizeof(double);
begin
     randomize;
     FillMatrix(cMtxSize, x, y, xa, ya);

     QueryPerformanceCounter(startTime1);
     norm1 := GenericMtxElementwiseNorm2(@x[0], cMtxLineWidth, cMtxWidth, cMtxheight);
     QueryPerformanceCounter(endTime1);

     QueryPerformanceCounter(startTime2);
     norm2 := ASMMatrixElementwiseNorm2(@x[0], cMtxLineWidth, cMtxWidth, cMtxheight);
     QueryPerformanceCounter(endTime2);

     QueryPerformanceCounter(startTime3);
     norm3 := ASMMatrixElementwiseNorm2(xa, cMtxLineWidth, cMtxWidth, cMtxheight);
     QueryPerformanceCounter(endTime3);

     Status(Format('%.2f, %.2f, %.2f', [(endTime1 - startTime1)/fPerfFreq*1000, (endTime2 - startTime2)/fPerfFreq*1000,
                                             (endTime3 - startTime3)/fPerfFreq*1000]));

     check(CompareValue(norm1, norm2, 1e-5) = 0, 'Norm with even width failed');
     check(CompareValue(norm1, norm3, 1e-5) = 0, 'Norm with even width (aligned) failed');

     FreeMem(xa);
     FreeMem(ya);

     FillMatrix((cmtxWidth + 1)*cMtxHeight, x, y, xa, ya);

     QueryPerformanceCounter(startTime1);
     norm1 := GenericMtxElementwiseNorm2(@x[0], cMtxLineWidth + sizeof(double), cMtxWidth + 1, cMtxheight);
     QueryPerformanceCounter(endTime1);

     QueryPerformanceCounter(startTime2);
     norm2 := ASMMatrixElementwiseNorm2(@x[0], cMtxLineWidth  + sizeof(double), cMtxWidth + 1, cMtxheight);
     QueryPerformanceCounter(endTime2);

     Status(Format('%.2f, %.2f', [(endTime1 - startTime1)/fPerfFreq*1000, (endTime2 - startTime2)/fPerfFreq*1000]));

     check(CompareValue(norm1, norm2, 1e-5) = 0, 'Norm with odd width failed');

     FreeMem(xa);
     FreeMem(ya);
end;

procedure TestMatrixOperations.TestCopy;
const mt1 : Array[0..5] of double = (0, 1, 2, 3, 4, 5);
var res : TDoubleDynArray;
begin
     FailsOnMemoryLeak := True;
     res := GenericMtxCopy(mt1, 3, 2);

     CheckEqualsMem(@mt1, @res[0], sizeof(mt1), 'Error matrix copy: ' + #13#10 + WriteMtxDyn(res, 3));
end;

procedure TestMatrixOperations.TestMult;
const mt1 : Array[0..5] of double = (0, 1, 2, 3, 4, 5);
      mt2 : Array[0..5] of double = (1, 2, 3, 4, 5, 6);
      mt3 : Array[0..3] of double = (13, 16, 40, 52);
      mt4 : Array[0..3] of double = (5, 6, 26, 33);
var res : TDoubleDynArray;
    matrix2 : PDouble;
begin
     res := GenericMtxMult(@mt1, @mt2, 3, 2, 2, 3, sizeof(double)*3, sizeof(double)*2);

     CheckEqualsMem(@mt3, @res[0], sizeof(mt3), 'Error Matrix multiplication: ' + #13#10 + WriteMtxDyn(res, 2));

     matrix2 := @mt2;
     inc(matrix2);

     res := GenericMtxMult(@mt1, matrix2, 2, 2, 2, 2, sizeof(double)*3, sizeof(double)*3);
     CheckEqualsMem(@mt4, @res[0], sizeof(mt4), 'Error Matrix multiplication: ' + #13#10 + WriteMtxDyn(res, 2));
end;

procedure TestMatrixOperations.TestNormalize;
const mt1 : Array[0..5] of double = (1, 2, 3, 4, 5, 6);
var res : TDoubleDynArray;
    x, y : integer;
    val : double;
begin
     res := GenericMtxNormalize(mt1, 3, 2, True);

     // check if the norm of the resulting lines is 1
     for y := 0 to 1 do
     begin
          val := 0;

          for x := 0 to 2 do
              val := val + sqr(res[3*y + x]);

          Check(Abs(val - 1) < 1e-6, 'Error norming not successfull');
     end;

     res := GenericMtxNormalize(mt1, 3, 2, False);
     // check if the norm of the resulting lines is 1
     for x := 0 to 2 do
     begin
          val := 0;

          for y := 0 to 1 do
              val := val + sqr(res[3*y + x]);

          Check(Abs(val - 1) < 1e-6, 'Error norming not successfull');
     end;
end;

procedure TestMatrixOperations.TestSub;
const mt1 : Array[0..5] of double = (0, 1, 2, 3, 4, 5);
      mt2 : Array[0..5] of double = (1, 1, 1, 1, 1, 1);
      mt3 : Array[0..5] of double = (-1, 0, 1, 2, 3, 4);
      mt4 : Array[0..3] of double = (-1, 0, 2, 3);
var res : TDoubleDynArray;
begin
     res := GenericMtxSub(@mt1, @mt2, 2, 3, sizeof(double)*2, sizeof(double)*2);

     CheckEqualsMem(@mt3, @res[0], sizeof(mt3), 'Error Matrix substraction: ' + #13#10 + WriteMtxDyn(res, 2));

     res := GenericMtxSub(@mt1, @mt2, 2, 2, sizeof(double)*3, sizeof(double)*3);
     CheckEqualsMem(@mt4, @res[0], sizeof(mt4), 'Error Matrix substraction: ' + #13#10 + WriteMtxDyn(res, 2));
end;

procedure TASMMatrixOperations.TestThreadMatrixAddAndScale;
var x, y, dest1 : TDoubleDynArray;
    xa, ya : PDouble;
    idx : integer;
    startTime1: Int64;
    endTime1: Int64;
    startTime2: Int64;
    endTime2: Int64;
    res : boolean;
const cMtxWidth = 5010;
      cMtxHeight = 4983;
      cMtxSize = (cMtxWidth)*cMtxHeight;
      cMtxLinewidth = (cMtxWidth)*sizeof(double);
begin
     randomize;
     FillMatrix(cMtxSize, x, y, xa, ya);
     dest1 := Copy(x, 0, Length(x));

     QueryPerformanceCounter(startTime1);
     GenericMtxAddAndScale(@dest1[0], cMtxLineWidth, cMtxWidth, cMtxheight, 5, 0.3);
     QueryPerformanceCounter(endTime1);

     QueryPerformanceCounter(startTime2);
     ThrMatrixAddAndScale(@x[0], cMtxLineWidth, cMtxWidth, cMtxheight, 5, 0.3);
     QueryPerformanceCounter(endTime2);

     Status(Format('%.2f, %.2f', [(endTime1 - startTime1)/fPerfFreq*1000, (endTime2 - startTime2)/fPerfFreq*1000]));

     res := CheckMtxIdx(x, dest1, idx, cMtxWidth, cMtxHeight);
     if not res then
        Status(IntToStr(idx));
     check(res);

     FreeMem(xa);
     FreeMem(ya);
end;

procedure TASMMatrixOperations.TestThreadMatrixAddSub;
var x, y, dest1, dest2 : TDoubleDynArray;
    xa, ya, dest2a : PDouble;
    idx : integer;
    startTime1: Int64;
    endTime1: Int64;
    startTime2: Int64;
    endTime2: Int64;
    startTime3: Int64;
    endTime3: Int64;
    res : boolean;
const cMtxWidth = 5010;
      cMtxHeight = 4983;
      cMtxSize = (cMtxWidth)*cMtxHeight;
      cMtxLinewidth = (cMtxWidth)*sizeof(double);
begin
     randomize;
     FillMatrix(cMtxSize, x, y, xa, ya);
     SetLength(dest1, cMtxSize);
     SetLength(dest2, cMtxSize);
     dest2a := AllocMem(cMtxSize*sizeof(double));

     QueryPerformanceCounter(startTime1);
     ASMMatrixAdd(@dest1[0], cMtxLineWidth, @x[0], @y[0], cMtxWidth, cMtxheight, cMtxLinewidth, cMtxLinewidth);
     QueryPerformanceCounter(endTime1);

     QueryPerformanceCounter(startTime2);
     ThrMatrixAdd(@dest2[0], cMtxLineWidth, @x[0], @y[0], cMtxWidth, cMtxheight, cMtxLinewidth, cMtxLinewidth);
     QueryPerformanceCounter(endTime2);

     QueryPerformanceCounter(startTime3);
     ThrMatrixAdd(dest2a, cMtxLineWidth, xa, ya, cMtxWidth, cMtxheight, cMtxLinewidth, cMtxLinewidth);
     QueryPerformanceCounter(endTime3);

     Status(Format('%.2f, %.2f, %.2f', [(endTime1 - startTime1)/fPerfFreq*1000, (endTime2 - startTime2)/fPerfFreq*1000,
                 (endTime3 - startTime3)/fPerfFreq*1000]));

     res := CheckMtxIdx(dest1, dest2, idx, cMtxWidth, cMtxHeight);
     if not res then
        Status(IntToStr(idx));
     check(res);

     Move(dest2a^, dest2[0], length(dest2)*sizeof(double));
     res := CheckMtxIdx(dest1, dest2, idx, cMtxWidth, cMtxHeight);
     if not res then
        Status(IntToStr(idx));
     check(res);

     FreeMem(xa);
     FreeMem(ya);
     FreeMem(dest2a);
end;

procedure TASMMatrixOperations.TestThreadMatrixMult;
var x, y, dest1, dest2 : TDoubleDynArray;
    xa, ya, dest1a, dest2a : PDouble;
    idx : integer;
    startTime1: Int64;
    endTime1: Int64;
    startTime2: Int64;
    endTime2: Int64;
    res : boolean;
    startTime3, endTime3 : int64;
    vecStartTime1, vecEndTime1 : int64;
    vecStartTime2, vecEndTime2 : int64;
    vecStartTime3, vecEndTime3 : int64;
const //cMtxWidth = 10*cCacheMtxSize;
      //cMtxHeight = cCacheMtxSize*10;
      cMtxWidth = 3543;
      cMtxHeight = 2560;
      cMtxSize = cMtxWidth*cMtxHeight;
      cMtxLinewidth = cMtxWidth*8;
      cMtxLineWidth2 = cMtxHeight*8;
begin
     randomize;
     FillMatrix(cMtxSize, x, y, xa, ya);
     SetLength(dest1, cMtxHeight*cMtxHeight);
     SetLength(dest2, cMtxHeight*cMtxHeight);
     dest2a := AllocMem(cMtxHeight*cMtxHeight*sizeof(double));
     dest1a := AllocMem(cMtxHeight*cMtxHeight*sizeof(double));

     QueryPerformanceCounter(startTime1);
     BlockedMatrixMultiplication(@dest1[0], cMtxLineWidth2, @x[0], @y[0], cMtxWidth, cMtxheight, cMtxHeight, cMtxWidth, cMtxLinewidth, cMtxLinewidth2);
     QueryPerformanceCounter(endTime1);

     QueryPerformanceCounter(startTime2);
     ThrMatrixMult(dest1a, cMtxLineWidth2, xa, ya, cMtxWidth, cMtxheight, cMtxHeight, cMtxWidth, cMtxLinewidth, cMtxLinewidth2);
     QueryPerformanceCounter(endTime2);

     QueryPerformanceCounter(startTime3);
     ThrMatrixMultDirect(dest2a, cMtxLineWidth2, xa, ya, cMtxWidth, cMtxheight, cMtxHeight, cMtxWidth, cMtxLinewidth, cMtxLinewidth2);
     QueryPerformanceCounter(endTime3);

     Status(Format('%.2f, %.2f, %.2f', [(endTime1 - startTime1)/fPerfFreq*1000, (endTime2 - startTime2)/fPerfFreq*1000, (endTime3 - startTime3)/fPerfFreq*1000]));

     Move(dest1a^, dest2[0], length(dest2)*sizeof(double));
     res := CheckMtxIdx(dest1, dest2, idx);
     if not res then
        Status(IntToStr(idx));
     check(res);

     Move(dest2a^, dest2[0], length(dest2)*sizeof(double));
     res := CheckMtxIdx(dest1, dest2, idx);
     if not res then
        Status(IntToStr(idx));
     check(res);

     FreeMem(xa);
     FreeMem(ya);

     FillMatrix(1024*1024*20, x, y, xa, ya);

     // matrix vector operations:
     QueryPerformanceCounter(vecstartTime3);
     ASMMatrixMult(@dest1[0], sizeof(double), @x[0], @y[0], 1024*1024, 20, 1, 1024*1024, 1024*1024*sizeof(double), sizeof(double));
     QueryPerformanceCounter(vecendTime3);
     
     QueryPerformanceCounter(vecstartTime1);
     ThrMatrixVecMult(dest2a, sizeof(double), xa, ya, 1024*1024, 20, 1024*1024, 1024*1024*sizeof(double));
     QueryPerformanceCounter(vecendTime1);

     QueryPerformanceCounter(vecstartTime2);
     ThrMatrixVecMult(@dest2[0], sizeof(double), @x[0], @y[0], 1024*1024, 20, 1024*1024, 1024*1024*sizeof(double));
     QueryPerformanceCounter(vecendTime2);

     Status(Format('%.2f, %.2f, %.2f',
         [(vecendTime1 - vecstartTime1)/fPerfFreq*1000, (vecendTime2 - vecstartTime2)/fPerfFreq*1000,
          (vecendTime3 - vecstartTime3)/fPerfFreq*1000]));

     res := CheckMtxIdx(dest1, dest2, idx, 1, cMtxHeight);
     if not res then
        Status(IntToStr(idx));
     check(res);

     Move(dest2a^, dest2[0], length(dest2)*sizeof(double));
     res := CheckMtxIdx(dest1, dest2, idx, 1, cMtxHeight);
     if not res then
        Status(IntToStr(idx));
     check(res);

     FreeMem(xa);
     FreeMem(ya);
     FreeMem(dest2a);
     FreeMem(dest1a);
end;

procedure TASMMatrixOperations.TestTransposeASSMOddWOddH;
const mt1 : Array[0..8] of double = (28, 35, 42, 50, 67, 84, 72, 99, 126);
      mt2 : Array[0..8] of double = (28, 50, 72, 35, 67, 99, 42, 84, 126);
var dest : Array[0..8] of double;
    desta, mta : PDouble;
    blk : PDouble;
begin
     blk := AllocMem((16 + 12)*sizeof(double) + 16);
     desta := blk;
     inc(PByte(desta), 16 - TASMNativeInt(blk) and $F);
     mta := desta;
     inc(mta, 16);

     GenericMtxCopy(mta, 4*sizeof(double), @mt1[0], 3*sizeof(double), 3, 3); 
     ASMMatrixTransposeAlignedOddWOddH(desta, 4*sizeof(double), mta, 4*sizeof(double), 3, 3);

     GenericMtxCopy(@dest[0], 3*sizeof(double), desta, 4*sizeof(double), 3, 3);
     Check(CheckMtx(mt2, dest), 'Error alligned transpose');

     ASMMatrixTransposeUnAlignedOddWOddH(@dest[0], 3*sizeof(double), @mt1[0], 3*sizeof(double), 3, 3);
     Check(CheckMtx(mt2, dest), 'Error unaligned transpose');

     FreeMem(blk);
end;

procedure TASMMatrixOperations.TestTransposedASMEvenWEvenH;
const mt1 : Array[0..15] of double = (0, 1, 2, 4, 3, 4, 5, 5, 6, 7, 8, 6, 7, 8, 9, 10);
      mt2 : Array[0..15] of double = (-1, 0, 1, 1, 2, 3, 4, 2, 5, 6, 7, 3, 4, 5, 6, 7);
      mt3 : Array[0..15] of double = (0, 3, 6, 7, 1, 4, 7, 8, 2, 5, 8, 9, 4, 5, 6, 10);
var m1, m2, dest : PDouble;
    d1 : Array[0..15] of double;
begin
     m1 := AllocMem(16*sizeof(double));
     m2 := AllocMem(16*sizeof(double));
     dest := AllocMem(16*sizeof(double));

     Move(mt1, m1^, sizeof(mt1));
     Move(mt2, m2^, sizeof(mt2));

     GenericMtxTranspose(dest, 4*sizeof(double), m1, 4*sizeof(double), 4, 4);
     move(dest^,d1, sizeof(d1));
     ASMMatrixTransposeAlignedEvenWEvenH(dest, 4*sizeof(double), m1, 4*sizeof(double), 4, 4);
     move(dest^,d1, sizeof(d1));
     Check(CheckMtx(mt3, d1));

     ASMMatrixTransposeUnAlignedEvenWEvenH(dest, 4*sizeof(double), m1, 4*sizeof(double), 4, 4);
     move(dest^,d1, sizeof(d1));
     Check(CheckMtx(mt3, d1));


     FreeMem(dest);
     FreeMem(m2);
     FreeMem(m1);
end;


procedure TASMMatrixOperations.TestTransposedASMEvenWOddH;
const mt1 : Array[0..5] of double = (0, 1, 2, 3, 4, 5);
      mt2 : Array[0..5] of double = (0, 2, 4, 1, 3, 5);
var dest : Array[0..5] of double;
begin
     ASMMatrixTransposeUnAlignedEvenWOddH(@dest[0], 3*Sizeof(double), @mt1[0], 2*sizeof(double), 2, 3);
     CheckEqualsMem(@mt2, @dest[0], sizeof(mt2), 'Error matrix transpose: ');
end;

procedure TestMatrixOperations.TestTranspose;
const mt1 : Array[0..5] of double = (0, 1, 2, 3, 4, 5);
      mt2 : Array[0..5] of double = (0, 2, 4, 1, 3, 5);
var res : TDoubleDynArray;
begin
     res := GenericMtxTranspose(@mt1, 2*sizeof(double), 2, 3);

     CheckEqualsMem(@mt2, @res[0], sizeof(mt2), 'Error matrix transpose: ' + #10#13 + WriteMtxdyn(res, 3));

     res := GenericMtxTranspose(@mt1, sizeof(double), 1, 6);
     CheckEqualsMem(@mt1, @res[0], sizeof(mt1), 'Error matrix transpose: ' + #13#10 + WriteMTxDyn(res, 6));
end;

procedure TASMMatrixOperations.TestMatrixASMCopy;
var x, dest1, dest2 : Array of double;
    xa, dest1a, dest2a : PDouble;
    px : PDouble;
    idx : integer;
    startTime1: Int64;
    endTime1: Int64;
    startTime2: Int64;
    endTime2: Int64;
    startTime3: Int64;
    endTime3: Int64;
    res : boolean;
const cMtxWidth = 2000;
      cMtxHeight = 500;
      cMtxSize = cMtxWidth*cMtxHeight;
      cMtxLinewidth = cMtxWidth*sizeof(double);
begin
     SetThreadAffinityMask(GetCurrentThread, 1);
     randomize;
     SetLength(x, cMtxSize);
     SetLength(dest1, cMtxSize);
     GetMem(xa, cMTxSize*sizeof(double));

     GetMem(dest1a, cMtxSize*sizeof(double));
     GetMem(dest2a, cMtxSize*sizeof(double));

     // fill randomly:
     px := @x[0];
     for Idx := 0 to cMtxSize - 1 do
     begin
          px^ := random;
          inc(px);
     end;

     px := xa;
     for Idx := 0 to cMtxSize - 1 do
     begin
          px^ := x[idx];
          inc(px);
     end;

     // clear most of the cache!
     SetLength(dest2, cMtxSize);

     QueryPerformanceCounter(startTime1);
     GenericMtxCopy(dest1a, cMtxLineWidth, xa, cMtxLineWidth, cMtxWidth, cMtxheight);
     QueryPerformanceCounter(endTime1);

     QueryPerformanceCounter(startTime2);
     ASMMatrixCopy(dest2a, cMtxLineWidth, xa, cMtxLinewidth, cMtxWidth, cMtxheight);
     QueryPerformanceCounter(endTime2);

     Move(dest1a^, dest1[0], length(dest2)*sizeof(double));
     Move(dest2a^, dest2[0], length(dest2)*sizeof(double));
     res := CheckMtxIdx(dest1, dest2, idx);
     if not res then
        Status(IntToStr(idx));
     check(res);

     QueryPerformanceCounter(startTime3);
     ASMMatrixCopy(@dest2[0], cMtxLineWidth, @x[0], cMtxLinewidth, cMtxWidth, cMtxheight);
     QueryPerformanceCounter(endTime3);

     Status(Format('%.2f, %.2f, %.2f', [(endTime1 - startTime1)/fPerfFreq*1000, (endTime2 - startTime2)/fPerfFreq*1000,
                        (endTime3 - startTime3)/fPerfFreq*1000]));

     Move(dest1a^, dest1[0], length(dest2)*sizeof(double));
     Move(dest2a^, dest2[0], length(dest2)*sizeof(double));
     res := CheckMtxIdx(dest1, dest2, idx);
     if not res then
        Status(IntToStr(idx));
     check(res);

     FreeMem(dest1a);
     FreeMem(dest2a);
     FreeMem(xa);
end;

procedure TASMMatrixOperations.TestMatrixBigASMMinMax;
var x : Array of double;
    xa : PDouble;
    px : PDouble;
    idx : integer;
    startTime1: Int64;
    endTime1: Int64;
    startTime2: Int64;
    endTime2: Int64;
    startTime3: Int64;
    endTime3: Int64;
    dest1, dest2, dest3, dest4 : double;
const cMtxWidth = 2000;
      cMtxHeight = 500;
      cMtxSize = cMtxWidth*cMtxHeight;
      cMtxLinewidth = cMtxWidth*sizeof(double);
begin
     SetThreadAffinityMask(GetCurrentThread, 1);
     randomize;
     SetLength(x, cMtxSize);
     GetMem(xa, cMtxSize*sizeof(double));

     // fill randomly:
     px := @x[0];
     for Idx := 0 to cMtxSize - 1 do
     begin
          px^ := random;
          inc(px);
     end;

     px := xa;
     for Idx := 0 to cMtxSize - 1 do
     begin
          px^ := x[idx];
          inc(px);
     end;

     QueryPerformanceCounter(startTime1);
     dest1 := GenericMtxMax(@x[0], cMtxWidth, cMtxheight, cMtxLineWidth);
     dest2 := GenericMtxMin(@x[0], cMtxWidth, cMtxheight, cMtxLineWidth);
     QueryPerformanceCounter(endTime1);

     QueryPerformanceCounter(startTime2);
     dest3 := ASMMatrixMax(@x[0], cMtxWidth, cMtxheight, cMtxLineWidth);
     dest4 := ASMMatrixMin(@x[0], cMtxWidth, cMtxheight, cMtxLineWidth);
     QueryPerformanceCounter(endTime2);

     check(dest1 = dest3, 'Max error');
     check(dest2 = dest4, 'Min error');

     QueryPerformanceCounter(startTime3);
     dest3 := ASMMatrixMax(xa, cMtxWidth, cMtxheight, cMtxLineWidth);
     dest4 := ASMMatrixMin(xa, cMtxWidth, cMtxheight, cMtxLineWidth);
     QueryPerformanceCounter(endTime3);

     check(dest1 = dest3, 'Max error');
     check(dest2 = dest4, 'Min error');

     Status(Format('%.2f, %.2f, %.2f', [(endTime1 - startTime1)/fPerfFreq*1000, (endTime2 - startTime2)/fPerfFreq*1000,
                        (endTime3 - startTime3)/fPerfFreq*1000]));

     freeMem(xa);
end;


procedure TASMMatrixOperations.TestMinMaxASM;
const mt1 : Array[0..15] of double = (0, 1, 2, -2, 3, 4, 5, 0, 6, 7, -1, 0, 0, 0, 0, 0);
      mt2 : Array[0..15] of double = (-1, 0, 1, 0, 2, 3, 4, 0, 5, 6, 7, 0, 0, 0, 0, 0);
      mt3 : Array[0..15] of double = (12, 15, 18, 0, 30, 42, 54, 0, 48, 69, 90, 0, 0, 0, 0, 0);
      mt4 : Array[0..15] of double = (0, 1, 2, 4, 3, 4, 5, 5, 6, 7, 8, 6, 7, 8, 9, 10);
      mt5 : Array[0..15] of double = (-1, 0, 1, 1, 2, 3, 4, 2, 5, 6, 7, 3, 4, 5, 6, 7);
      mt6 : Array[0..15] of double = (28, 35, 42, 36, 50, 67, 84, 61, 72, 99, 126, 86, 94, 128, 162, 120);

var minmax : double;
begin
     minmax := ASMMatrixOperations.ASMMatrixMin(@mt1[0], 3, 3, 4*sizeof(double));
     Check(minmax = -1, 'Min error');
     minmax := ASMMatrixOperations.ASMMatrixMax(@mt1[0], 3, 3, 4*sizeof(double));
     Check(minmax = 7, 'Max error');

     minmax := ASMMatrixOperations.ASMMatrixMin(@mt5[0], 4, 4, 4*sizeof(double));
     Check(minmax = -1, 'Min error');
     minmax := ASMMatrixOperations.ASMMatrixMax(@mt6[0], 4, 4, 4*sizeof(double));
     Check(minmax = 162, 'Max error');
end;

procedure TASMMatrixOperations.TestMtxBigNormalizeRow;
var x : Array of double;
    xa : PDouble;
    px : PDouble;
    idx : integer;
    startTime1: Int64;
    endTime1: Int64;
    startTime2: Int64;
    endTime2: Int64;
    startTime3: Int64;
    endTime3: Int64;
    dest1, dest2 : Array of double;
    dest1a : PDouble;
const cMtxWidth = 2000;
      cMtxHeight = 500;
      cMtxSize = cMtxWidth*cMtxHeight;
      cMtxLinewidth = cMtxWidth*sizeof(double);
begin
     SetThreadAffinityMask(GetCurrentThread, 1);
     randomize;
     SetLength(x, cMtxSize);
     GetMem(xa, cMtxSize*sizeof(double));
     GetMem(dest1a, cMtxSize*sizeof(double));
     SetLength(dest1, cMtxSize);
     SetLength(dest2, cMtxSize);
     
     // fill randomly:
     px := @x[0];
     for Idx := 0 to cMtxSize - 1 do
     begin
          px^ := random;
          inc(px);
     end;

     px := xa;
     for Idx := 0 to cMtxSize - 1 do
     begin
          px^ := x[idx];
          inc(px);
     end;

     QueryPerformanceCounter(startTime1);
     GenericMtxNormalize(dest1, x, cMtxWidth, cMtxheight, True);
     QueryPerformanceCounter(endTime1);

     QueryPerformanceCounter(startTime2);
     ASMMatrixNormalize(@dest2[0], cMtxLinewidth, @x[0], cMtxLinewidth, cMtxWidth, cMtxheight, True);
     QueryPerformanceCounter(endTime2);

     QueryPerformanceCounter(startTime3);
     ASMMatrixNormalize(dest1a, cMtxLinewidth, xa, cMtxLinewidth, cMtxWidth, cMtxheight, True);
     QueryPerformanceCounter(endTime3);

     Status(Format('%.2f, %.2f, %.2f', [(endTime1 - startTime1)/fPerfFreq*1000, (endTime2 - startTime2)/fPerfFreq*1000,
                        (endTime3 - startTime3)/fPerfFreq*1000]));

     Check(checkMtx(dest1, dest2), 'Error row wise Matrix normalize');
     Move(dest1a^, dest2[0], cMtxSize*sizeof(double));
     Check(checkMtx(dest1, dest2), 'Error row wise aligned Matrix normalize');

     QueryPerformanceCounter(startTime1);
     GenericMtxNormalize(dest1, x, cMtxWidth, cMtxheight, False);
     QueryPerformanceCounter(endTime1);

     QueryPerformanceCounter(startTime2);
     ASMMatrixNormalize(@dest2[0], cMtxLinewidth, @x[0], cMtxLinewidth, cMtxWidth, cMtxheight, False);
     QueryPerformanceCounter(endTime2);

     QueryPerformanceCounter(startTime3);
     ASMMatrixNormalize(dest1a, cMtxLinewidth, xa, cMtxLinewidth, cMtxWidth, cMtxheight, False);
     QueryPerformanceCounter(endTime3);


     Status(Format('%.2f, %.2f, %.2f', [(endTime1 - startTime1)/fPerfFreq*1000, (endTime2 - startTime2)/fPerfFreq*1000,
                        (endTime3 - startTime3)/fPerfFreq*1000]));

     Check(checkMtx(dest1, dest2), 'Error row wise Matrix normalize');
     Move(dest1a^, dest2[0], cMtxSize*sizeof(double));
     Check(checkMtx(dest1, dest2), 'Error row wise aligned Matrix normalize');

     freemem(dest1a);
     freeMem(xa);
end;

procedure TASMMatrixOperations.TestMtxNormalizeColumn;
const mt1 : Array[0..15] of double = (28, 35, 42, 36, 50, 67, 84, 61, 72, 99, 126, 86, 94, 128, 162, 120);
var dest1, dest2 : Array[0..15] of double;
    m, dest : PDouble;
    blk : PDouble;
begin
     blk := AllocMem(32*sizeof(double) + 16);
     m := blk;
     inc(PByte(m), 16 - TASMNativeInt(blk) and $F);
     dest := m;
     inc(dest, 16);
     Move(mt1, m^, 16*sizeof(double));

     GenericMtxNormalize(@dest1[0], 4*sizeof(double), @mt1[0], 4*sizeof(double), 4, 4, False);
     ASMMatrixNormalizeColumnUnAlignedEvenW(@dest2[0], 4*sizeof(double), @mt1[0], 4*sizeof(double), 4, 4);
     ASMMatrixNormalizeColumnAlignedEvenW(dest, 4*sizeof(double), m, 4*sizeof(double), 4, 4);

     Check(checkMtx(dest1, dest2), 'Error column wise Matrix normalize');
     Move(dest^, dest2, sizeof(dest2));
     Check(checkMtx(dest1, dest2), 'Error column wise aligned Matrix normalize');

     GenericMtxNormalize(@dest1[0], 4*sizeof(double), @mt1[0], 4*sizeof(double), 3, 4, False);
     ASMMatrixNormalizeColumnUnAlignedOddW(@dest2[0], 4*sizeof(double), @mt1[0], 4*sizeof(double), 3, 4);
     ASMMatrixNormalizeColumnUnAlignedOddW(dest, 4*sizeof(double), m, 4*sizeof(double), 3, 4);

     Check(checkMtx(dest1, dest2), 'Error row wise Matrix normalize');
     Move(dest^, dest2, sizeof(dest2));
     Check(checkMtx(dest1, dest2), 'Error row wise aligned Matrix normalize');

     FreeMem(blk);
end;

procedure TASMMatrixOperations.TestMtxNormalizeRow;
const mt1 : Array[0..15] of double = (28, 35, 42, 36, 50, 67, 84, 61, 72, 99, 126, 86, 94, 128, 162, 120);
var dest1, dest2 : Array[0..15] of double;
    m, dest : PDouble;
    blk : PDouble;
begin
     blk := AllocMem((3*16)*sizeof(double) + 16);
     m := blk;
     inc(PByte(m), 16 - TASMNativeInt(blk) and $F);
     dest := m;
     inc(dest, 16);
     Move(mt1, m^, 16*sizeof(double));

     GenericMtxNormalize(@dest1[0], 4*sizeof(double), @mt1[0], 4*sizeof(double), 4, 4, True);
     ASMMatrixNormalizeRowUnAlignedEvenW(@dest2[0], 4*sizeof(double), @mt1[0], 4*sizeof(double), 4, 4);
     ASMMatrixNormalizeRowAlignedEvenW(dest, 4*sizeof(double), m, 4*sizeof(double), 4, 4);

     Check(checkMtx(dest1, dest2), 'Error row wise Matrix normalize');
     Move(dest^, dest2, sizeof(dest2));
     Check(checkMtx(dest1, dest2), 'Error row wise aligned Matrix normalize');

     GenericMtxNormalize(@dest1[0], 4*sizeof(double), @mt1[0], 4*sizeof(double), 3, 4, True);
     ASMMatrixNormalizeRowUnAlignedOddW(@dest2[0], 4*sizeof(double), @mt1[0], 4*sizeof(double), 3, 4);
     ASMMatrixNormalizeRowUnAlignedOddW(dest, 4*sizeof(double), m, 4*sizeof(double), 3, 4);

     Check(checkMtx(dest1, dest2), 'Error row wise Matrix normalize');
     Move(dest^, dest2, sizeof(dest2));
     Check(checkMtx(dest1, dest2), 'Error row wise aligned Matrix normalize');

     FreeMem(blk);
end;

procedure TASMMatrixOperations.TestMtxMeanRow;
const mt1 : Array[0..15] of double = (28, 35, 42, 36, 50, 67, 84, 61, 72, 99, 126, 86, 94, 128, 162, 120);
var dest1, dest2 : Array[0..3] of double;
    m, pM, dest, pDest : PDouble;
    destGen, DestDyn : TDoubleDynArray;
    i : Integer;
    blk : PDouble;

    startTime1, endTime1 : Int64;
    startTime2, endTime2 : Int64;
    startTime3, endTime3 : Int64;
const cMtxWidth = 2000;
      cMtxHeight = 500;
      cMtxSize = cMtxWidth*cMtxHeight;
      cMtxLinewidth = cMtxWidth*sizeof(double);

begin
     blk := AllocMem((16 + 8)*sizeof(double) + 16);
     m := blk;
     inc(PByte(m), 16 - TASMNativeInt(blk) and $F);
     dest := m;
     inc(dest, 16);

     Move(mt1, m^, 16*sizeof(double));

     GenericMtxMean(@dest1[0], sizeof(double), @mt1[0], 4*sizeof(double), 4, 4, True);
     ASMMatrixMeanRowUnAlignedEvenW(@dest2[0], sizeof(double), @mt1[0], 4*sizeof(double), 4, 4);
     ASMMatrixMeanRowAlignedEvenW(dest, 2*sizeof(double), m, 4*sizeof(double), 4, 4);

     Check(checkMtx(dest1, dest2), 'Error row wise Matrix normalize');
     pDest := dest;
     for i := 0 to High(dest1) - 1 do
     begin
          dest2[i] := pDest^;
          inc(pDest, 2);
     end;
     Check(checkMtx(dest1, dest2), 'Error row wise aligned Matrix normalize');

     GenericMtxMean(@dest1[0], sizeof(double), @mt1[0], 4*sizeof(double), 3, 4, True);
     ASMMatrixMeanRowUnAlignedOddW(@dest2[0], sizeof(double), @mt1[0], 4*sizeof(double), 3, 4);
     ASMMatrixMeanRowAlignedOddW(dest, 2*sizeof(double), m, 4*sizeof(double), 3, 4);

     Check(checkMtx(dest1, dest2), 'Error row wise Matrix normalize');
     pDest := dest;
     for i := 0 to High(dest1) - 1 do
     begin
          dest2[i] := pDest^;
          inc(pDest, 2);
     end;
     Check(checkMtx(dest1, dest2), 'Error row wise aligned Matrix normalize');

     FreeMem(blk);

     // big row test
     blk := AllocMem((cMtxSize + 16 + cMtxHeight*2)*sizeof(double) + 16);
     m := blk;
     inc(PByte(m), 16 - TASMNativeInt(blk) and $F);
     dest := m;
     inc(dest, cMtxSize + cMtxSize mod 2);

     SetLength(DestDyn, cMtxHeight);
     SetLength(DestGen, cMtxHeight);

     pM := m;
     for i := 0 to cMtxSize - 1 do
     begin
     	    pM^ := i;
          inc(pM);
     end;

					QueryPerformanceCounter(startTime1);
     GenericMtxMean(@destGen[0], sizeof(double), m, cMtxLineWidth, cMtxWidth, cMtxheight, True);
     QueryPerformanceCounter(endTime1);

     QueryPerformanceCounter(startTime2);
     ASMMatrixMeanRowUnAlignedEvenW(dest, 2*sizeof(double), m, cMtxLineWidth, cMtxWidth, cMtxheight);
     QueryPerformanceCounter(endTime2);

     pDest := dest;
     for i := 0 to cMtxHeight - 1 do
     begin
          destDyn[i] := pdest^;
          inc(pdest, 2);
     end;

     Check(CheckMtx(destGen, DestDyn), 'Error Unaligned Mean row operation');

     QueryPerformanceCounter(startTime3);
     ASMMatrixMeanRowAlignedEvenW(dest, 2*sizeof(double), m, cMtxLineWidth, cMtxWidth, cMtxheight);
     QueryPerformanceCounter(endTime3);

     pDest := dest;
     for i := 0 to cMtxHeight - 1 do
     begin
          destDyn[i] := pdest^;
          inc(pdest, 2);
     end;

     Check(CheckMtx(destGen, DestDyn), 'Error aligned Mean row operation');

     Status(Format('%.2f, %.2f, %.2f', [(endTime1 - startTime1)/fPerfFreq*1000, (endTime2 - startTime2)/fPerfFreq*1000,
                        (endTime3 - startTime3)/fPerfFreq*1000]));

     FreeMem(blk);
end;

procedure TASMMatrixOperations.TestMtxMeanColumn;
const mt1 : Array[0..15] of double = (28, 35, 42, 36, 50, 67, 84, 61, 72, 99, 126, 86, 94, 128, 162, 120);
var dest1, dest2 : Array[0..3] of double;
    m, pM, dest, pDest : PDouble;
    destGen, DestDyn : TDoubleDynArray;
    i : Integer;
    blk : PDouble;

    startTime1, endTime1 : Int64;
    startTime2, endTime2 : Int64;
    startTime3, endTime3 : Int64;
const cMtxWidth = 500;
      cMtxHeight = 2000;
      cMtxSize = cMtxWidth*cMtxHeight;
      cMtxLinewidth = cMtxWidth*sizeof(double);

begin
     blk := AllocMem((16 + 8)*sizeof(double) + 16);
     m := blk;
     inc(PByte(m), 16 - TASMNativeInt(blk) and $F);
     dest := m;
     inc(dest, 16);

     Move(mt1, m^, 16*sizeof(double));

     GenericMtxMean(@dest1[0], sizeof(double), @mt1[0], 4*sizeof(double), 4, 4, False);
     ASMMatrixMeanColumnUnAlignedEvenW(@dest2[0], 4*sizeof(double), @mt1[0], 4*sizeof(double), 4, 4);
     ASMMatrixMeanColumnAlignedEvenW(dest, 4*sizeof(double), m, 4*sizeof(double), 4, 4);

     Check(checkMtx(dest1, dest2), 'Error row wise Matrix normalize');
     for i := 0 to High(dest1) - 1 do
     begin
          dest2[i] := PConstDoubleArr(dest)^[i];
     end;
     Check(checkMtx(dest1, dest2), 'Error row wise aligned Matrix normalize');

     GenericMtxMean(@dest1[0], sizeof(double), @mt1[0], 4*sizeof(double), 3, 4, False);
     ASMMatrixMeanColumnUnAlignedOddW(@dest2[0], 4*sizeof(double), @mt1[0], 4*sizeof(double), 3, 4);
     ASMMatrixMeanColumnAlignedOddW(dest, 4*sizeof(double), m, 4*sizeof(double), 3, 4);

     Check(checkMtx(dest1, dest2), 'Error row wise Matrix normalize');
     for i := 0 to High(dest1) - 1 do
     begin
          dest2[i] := PConstDoubleArr(dest)^[i];
     end;
     Check(checkMtx(dest1, dest2), 'Error row wise aligned Matrix normalize');

     FreeMem(blk);

     // big row test
     blk := AllocMem((cMtxSize + 16)*sizeof(double) + 16);
     m := blk;
     inc(PByte(m), 16 - TASMNativeInt(blk) and $F);
     dest := m;
     inc(dest, cMtxSize + cMtxSize mod 2);

     SetLength(DestDyn, cMtxWidth);
     SetLength(DestGen, cMtxWidth);

     pM := m;
     for i := 0 to cMtxSize - 1 do
     begin
     	    pM^ := i;
          inc(pM);
     end;

					QueryPerformanceCounter(startTime1);
     GenericMtxMean(@destGen[0], cMtxWidth*sizeof(double), m, cMtxLineWidth, cMtxWidth, cMtxheight, False);
     QueryPerformanceCounter(endTime1);

     QueryPerformanceCounter(startTime2);
     ASMMatrixMeanColumnUnAlignedEvenW(dest, cMtxWidth*sizeof(double), m, cMtxLineWidth, cMtxWidth, cMtxheight);
     QueryPerformanceCounter(endTime2);

     pDest := dest;
     for i := 0 to cMtxWidth - 1 do
     begin
          destDyn[i] := pdest^;
          inc(pdest);
     end;

     Check(CheckMtx(destGen, DestDyn), 'Error Unaligned sum operation');

     QueryPerformanceCounter(startTime3);
     ASMMatrixMeanColumnAlignedEvenW(dest, cMtxWidth*sizeof(double), m, cMtxLineWidth, cMtxWidth, cMtxheight);
     QueryPerformanceCounter(endTime3);

     pDest := dest;
     for i := 0 to cMtxWidth - 1 do
     begin
          destDyn[i] := pdest^;
          inc(pdest);
     end;

     Check(CheckMtx(destGen, DestDyn), 'Error Aligned sum operation');

     Status(Format('%.2f, %.2f, %.2f', [(endTime1 - startTime1)/fPerfFreq*1000, (endTime2 - startTime2)/fPerfFreq*1000,
                        (endTime3 - startTime3)/fPerfFreq*1000]));

     FreeMem(blk);
end;

procedure TASMMatrixOperations.TestMtxSumRow;
const mt1 : Array[0..15] of double = (28, 35, 42, 36, 50, 67, 84, 61, 72, 99, 126, 86, 94, 128, 162, 120);
var dest1, dest2 : Array[0..3] of double;
    m, pM, dest, pDest : PDouble;
    destGen, DestDyn : TDoubleDynArray;
    i : Integer;
    blk : PDouble;

    startTime1, endTime1 : Int64;
    startTime2, endTime2 : Int64;
    startTime3, endTime3 : Int64;
const cMtxWidth = 2000;
      cMtxHeight = 500;
      cMtxSize = cMtxWidth*cMtxHeight;
      cMtxLinewidth = cMtxWidth*sizeof(double);
begin
     SetThreadAffinityMask(GetCurrentThread, 1);

     blk := AllocMem((16 + 8)*sizeof(double) + 16);
     m := blk;
     inc(PByte(m), 16 - TASMNativeInt(blk) and $F);
     dest := m;
     inc(dest, 16);

     Move(mt1, m^, 16*sizeof(double));

     GenericMtxSum(@dest1[0], sizeof(double), @mt1[0], 4*sizeof(double), 4, 4, True);
     ASMMatrixSumRowUnAlignedEvenW(@dest2[0], sizeof(double), @mt1[0], 4*sizeof(double), 4, 4);
     ASMMatrixSumRowAlignedEvenW(dest, 2*sizeof(double), m, 4*sizeof(double), 4, 4);

     Check(checkMtx(dest1, dest2), 'Error row wise Matrix normalize');
     pDest := dest;
     for i := 0 to High(dest1) - 1 do
     begin
          dest2[i] := pDest^;
          inc(pDest, 2);
     end;
     Check(checkMtx(dest1, dest2), 'Error row wise aligned Matrix normalize');

     GenericMtxSum(@dest1[0], sizeof(double), @mt1[0], 4*sizeof(double), 3, 4, True);
     ASMMatrixSumRowUnAlignedOddW(@dest2[0], sizeof(double), @mt1[0], 4*sizeof(double), 3, 4);
     ASMMatrixSumRowAlignedOddW(dest, 2*sizeof(double), m, 4*sizeof(double), 3, 4);

     Check(checkMtx(dest1, dest2), 'Error row wise Matrix normalize');
     for i := 0 to High(dest1) - 1 do
     begin
          dest2[i] := PConstDoubleArr(dest)^[2*i];
     end;
     Check(checkMtx(dest1, dest2), 'Error row wise aligned Matrix normalize');

     FreeMem(blk);

     // big row test
     blk := AllocMem((cMtxSize + 16)*sizeof(double) + 16);
     m := blk;
     inc(PByte(m), 16 - TASMNativeInt(blk) and $F);
     dest := m;
     inc(dest, cMtxSize + cMtxSize mod 2);

     SetLength(DestDyn, cMtxHeight);
     SetLength(DestGen, cMtxHeight);

     pM := m;
     for i := 0 to cMtxSize - 1 do
     begin
     	    pM^ := i;
          inc(pM);
     end;

					QueryPerformanceCounter(startTime1);
     GenericMtxSum(@destGen[0], sizeof(double), m, cMtxLineWidth, cMtxWidth, cMtxheight, True);
     QueryPerformanceCounter(endTime1);

     QueryPerformanceCounter(startTime2);
     ASMMatrixSumRowUnAlignedEvenW(dest, 2*sizeof(double), m, cMtxLineWidth, cMtxWidth, cMtxheight);
     QueryPerformanceCounter(endTime2);

     pDest := dest;
     for i := 0 to cMtxHeight - 1 do
     begin
          destDyn[i] := pdest^;
          inc(pdest, 2);
     end;

     Check(CheckMtx(destGen, DestDyn), 'Error Unaligned sum operation');

     QueryPerformanceCounter(startTime3);
     ASMMatrixSumRowAlignedEvenW(dest, 2*sizeof(double), m, cMtxLineWidth, cMtxWidth, cMtxheight);
     QueryPerformanceCounter(endTime3);

     pDest := dest;
     for i := 0 to cMtxHeight - 1 do
     begin
          destDyn[i] := pdest^;
          inc(pdest, 2);
     end;

     Check(CheckMtx(destGen, DestDyn), 'Error Aligned sum operation');

     Status(Format('%.2f, %.2f, %.2f', [(endTime1 - startTime1)/fPerfFreq*1000, (endTime2 - startTime2)/fPerfFreq*1000,
                        (endTime3 - startTime3)/fPerfFreq*1000]));

     FreeMem(blk);
end;

procedure TASMMatrixOperations.TestMtxSumColumn;
const mt1 : Array[0..15] of double = (28, 35, 42, 36, 50, 67, 84, 61, 72, 99, 126, 86, 94, 128, 162, 120);
var dest1, dest2 : Array[0..3] of double;
    m, pM, dest, pDest : PDouble;
    destGen, DestDyn : TDoubleDynArray;
    i : Integer;
    blk : PDouble;

    startTime1, endTime1 : Int64;
    startTime2, endTime2 : Int64;
    startTime3, endTime3 : Int64;
const cMtxWidth = 500;
      cMtxHeight = 2000;
      cMtxSize = cMtxWidth*cMtxHeight;
      cMtxLinewidth = cMtxWidth*sizeof(double);

begin
     blk := AllocMem((16 + 8)*sizeof(double) + 16);
     m := blk;
     inc(PByte(m), 16 - TASMNativeInt(blk) and $F);
     dest := m;
     inc(dest, 16);

     Move(mt1, m^, 16*sizeof(double));

     GenericMtxSum(@dest1[0], sizeof(double), @mt1[0], 4*sizeof(double), 4, 4, False);
     ASMMatrixSumColumnUnAlignedEvenW(@dest2[0], 4*sizeof(double), @mt1[0], 4*sizeof(double), 4, 4);
     ASMMatrixSumColumnAlignedEvenW(dest, 4*sizeof(double), m, 4*sizeof(double), 4, 4);

     Check(checkMtx(dest1, dest2), 'Error column wise Matrix sum');
     Move(dest^, dest2, sizeof(dest2));
     Check(checkMtx(dest1, dest2), 'Error column wise aligned Matrix sum');

     GenericMtxSum(@dest1[0], sizeof(double), @mt1[0], 4*sizeof(double), 3, 4, False);
     ASMMatrixSumColumnUnAlignedOddW(@dest2[0], 4*sizeof(double), @mt1[0], 4*sizeof(double), 3, 4);
     ASMMatrixSumColumnAlignedOddW(dest, 4*sizeof(double), m, 4*sizeof(double), 3, 4);

     Check(checkMtx(dest1, dest2), 'Error column wise Matrix sum');
     Move(dest^, dest2, sizeof(dest2));
     Check(checkMtx(dest1, dest2), 'Error column wise aligned Matrix sum');

     FreeMem(blk);

     // big row test
     blk := AllocMem((cMtxSize + 16)*sizeof(double) + 16);
     m := blk;
     inc(PByte(m), 16 - TASMNativeInt(blk) and $F);
     dest := m;
     inc(dest, cMtxSize + cMtxSize mod 2);

     SetLength(DestDyn, cMtxWidth);
     SetLength(DestGen, cMtxWidth);

     pM := m;
     for i := 0 to cMtxSize - 1 do
     begin
     	    pM^ := i;
          inc(pM);
     end;

					QueryPerformanceCounter(startTime1);
     GenericMtxSum(@destGen[0], cMtxWidth*sizeof(double), m, cMtxLineWidth, cMtxWidth, cMtxheight, False);
     QueryPerformanceCounter(endTime1);

     QueryPerformanceCounter(startTime2);
     ASMMatrixSumColumnUnAlignedEvenW(dest, cMtxWidth*sizeof(double), m, cMtxLineWidth, cMtxWidth, cMtxheight);
     QueryPerformanceCounter(endTime2);

     pDest := dest;
     for i := 0 to cMtxWidth - 1 do
     begin
          destDyn[i] := pdest^;
          inc(pdest);
     end;

     Check(CheckMtx(destGen, DestDyn), 'Error Unaligned sum operation');

     QueryPerformanceCounter(startTime3);
     ASMMatrixSumColumnAlignedEvenW(dest, cMtxWidth*sizeof(double), m, cMtxLineWidth, cMtxWidth, cMtxheight);
     QueryPerformanceCounter(endTime3);

     pDest := dest;
     for i := 0 to cMtxWidth - 1 do
     begin
          destDyn[i] := pdest^;
          inc(pdest);
     end;

     Check(CheckMtx(destGen, DestDyn), 'Error Aligned sum operation');

     Status(Format('%.2f, %.2f, %.2f', [(endTime1 - startTime1)/fPerfFreq*1000, (endTime2 - startTime2)/fPerfFreq*1000,
                        (endTime3 - startTime3)/fPerfFreq*1000]));

     FreeMem(blk);
end;

procedure TASMMatrixOperations.TestMultASM;
const mt1 : Array[0..15] of double = (0, 1, 2, 0, 3, 4, 5, 0, 6, 7, 8, 0, 0, 0, 0, 0);
      mt2 : Array[0..15] of double = (-1, 0, 1, 0, 2, 3, 4, 0, 5, 6, 7, 0, 0, 0, 0, 0);
      mt3 : Array[0..15] of double = (12, 15, 18, 0, 30, 42, 54, 0, 48, 69, 90, 0, 0, 0, 0, 0);
      mt4 : Array[0..15] of double = (0, 1, 2, 4, 3, 4, 5, 5, 6, 7, 8, 6, 7, 8, 9, 10);
      mt5 : Array[0..15] of double = (-1, 0, 1, 1, 2, 3, 4, 2, 5, 6, 7, 3, 4, 5, 6, 7);
      mt6 : Array[0..15] of double = (28, 35, 42, 36, 50, 67, 84, 61, 72, 99, 126, 86, 94, 128, 162, 120);

var dest : Array[0..15] of double;
begin
     FillChar(dest, sizeof(dest), 0);
     ASMMatrixOperations.ASMMatrixMult(@dest[0], 4*sizeof(double), @mt1[0], @mt2[0], 3, 3, 3, 3, 4*sizeof(double), 4*sizeof(double));

     Check(CheckMtx(dest, mt3, -1, -1, 1e-10), 'Mult Matrix error');

     FillChar(dest, sizeof(dest), 0);
     ASMMatrixOperations.ASMMatrixMult(@dest[0], 4*sizeof(double), @mt4[0], @mt5[0], 4, 4, 4, 4, 4*sizeof(double), 4*sizeof(double));

     Check(CheckMtx(dest, mt6, -1, -1, 1e-10), 'Mult Matrix error');
end;

procedure TASMMatrixOperations.TestMultASMEvenW1EvenW2;
const mt1 : Array[0..15] of double = (0, 1, 2, 4, 3, 4, 5, 5, 6, 7, 8, 6, 7, 8, 9, 10);
      mt2 : Array[0..15] of double = (-1, 0, 1, 1, 2, 3, 4, 2, 5, 6, 7, 3, 4, 5, 6, 7);
      mt3 : Array[0..15] of double = (28, 35, 42, 36, 50, 67, 84, 61, 72, 99, 126, 86, 94, 128, 162, 120);
var m1, m2, dest : PDouble;
    d1 : Array[0..15] of double;
    blk : PByte;
begin
     blk := AllocMem(3*16*sizeof(double) + 16);
     m1 := PDouble(Cardinal(blk) + 16 - Cardinal(blk) mod $10);
     m2 := m1;
     inc(m2, 16);
     dest := m2;
     inc(dest, 16);

     Move(mt1, m1^, sizeof(mt1));
     Move(mt2, m2^, sizeof(mt2));

     ASMMatrixMultAlignedEvenW1EvenW2(dest, 4*sizeof(double), m1, m2, 4, 4, 4, 4, 4*sizeof(double), 4*sizeof(double));
     move(dest^,d1, sizeof(d1));

     Check(CheckMtx(mt3, d1));

     FreeMem(blk);
end;

procedure TASMMatrixOperations.TestMultASMEvenW1EvenH2Transposed;
const mt1 : Array[0..15] of double = (0, 1, 2, 4, 3, 4, 5, 5, 6, 7, 8, 6, 7, 8, 9, 10);
      mt2 : Array[0..15] of double = (-1, 0, 1, 1, 2, 3, 4, 2, 5, 6, 7, 3, 4, 5, 6, 7);
      mt3 : Array[0..15] of double = (6, 19, 32, 45, 7, 48, 89, 97, 8, 77, 146, 149, 12, 94, 176, 192);
var m1, m2, dest : PDouble;
    d1 : Array[0..15] of double;
    blk : PByte;
begin
     blk := AllocMem((16 + 16 + 16)*sizeof(double) + 16);
     m1 := PDouble(blk);

     inc(PByte(m1), 16 - TASMNativeInt(blk) and $F);
     m2 := m1;
     inc(m2, 16);
     dest := m2;
     inc(dest, 16);

     Move(mt1, m1^, sizeof(mt1));
     Move(mt2, m2^, sizeof(mt2));

     ASMMatrixMultAlignedEvenW1EvenH2Transposed(dest, 4*sizeof(double), m1, m2, 4, 4, 4, 4, 4*sizeof(double), 4*sizeof(double));
     move(dest^,d1, sizeof(d1));

     Check(CheckMtx(mt3, d1));
     FillChar(dest^, 16*sizeof(double), 0);

     ASMMatrixMultUnAlignedEvenW1EvenH2Transposed(dest, 4*sizeof(double), m1, m2, 4, 4, 4, 4, 4*sizeof(double), 4*sizeof(double));
     move(dest^,d1, sizeof(d1));

     Check(CheckMtx(mt3, d1));

     FreeMem(blk);
end;

procedure TASMMatrixOperations.TestMultASMEvenW1OddH2Transposed;
const mt1 : Array[0..15] of double = (0, 1, 2, 4, 3, 4, 5, 5, 6, 7, 8, 6, 7, 8, 9, 10);
      mt2 : Array[0..15] of double = (-1, 0, 1, 1, 2, 3, 4, 2, 5, 6, 7, 3, 4, 5, 6, 7);
      mt3 : Array[0..15] of double = (6, 19, 32, 0, 7, 48, 89, 0, 8, 77, 146, 0, 12, 94, 176, 0);
var m1, m2, dest : PDouble;
    d1 : Array[0..15] of double;
    blk : PByte;
begin
     blk := AllocMem((16 + 16 + 16)*sizeof(double) + 16);
     m1 := PDouble(blk);

     inc(PByte(m1), 16 - TASMNativeInt(blk) and $F);
     m2 := m1;
     inc(m2, 16);
     dest := m2;
     inc(dest, 16);

     Move(mt1, m1^, sizeof(mt1));
     Move(mt2, m2^, sizeof(mt2));

     ASMMatrixMultAlignedEvenW1OddH2Transposed(dest, 4*sizeof(double), m1, m2, 4, 4, 4, 3, 4*sizeof(double), 4*sizeof(double));
     move(dest^,d1, sizeof(d1));

     Check(CheckMtx(mt3, d1));
     FillChar(dest^, 16*sizeof(double), 0);

     ASMMatrixMultUnAlignedEvenW1OddH2Transposed(dest, 4*sizeof(double), m1, m2, 4, 4, 4, 3, 4*sizeof(double), 4*sizeof(double));
     move(dest^,d1, sizeof(d1));

     Check(CheckMtx(mt3, d1));

     FreeMem(blk);
end;

procedure TASMMatrixOperations.TestMultASMEvenW1OddW2;
const mt1 : Array[0..15] of double = (0, 1, 2, 4, 3, 4, 5, 5, 6, 7, 8, 6, 7, 8, 9, 10);
      mt2 : Array[0..15] of double = (-1, 0, 1, 1, 2, 3, 4, 2, 5, 6, 7, 3, 4, 5, 6, 7);
      mt3 : Array[0..8] of double = (28, 35, 42, 50, 67, 84, 72, 99, 126);
      mt4 : Array[0..11] of double = (28, 35, 42, 0, 50, 67, 84, 0, 72, 99, 126, 0);
var dest : Array[0..11] of double;
    dest2 : PDouble;
    m1, m2 : PDouble;
    memBlk : PDouble;
begin
     FillChar(dest, sizeof(dest), 0);
     ASMMatrixMultUnAlignedEvenW1OddW2(@dest[0], 4*sizeof(double), @mt1[0], @mt2[0], 4, 3, 3, 4, 4*sizeof(double), 4*sizeof(double));

     Check(CheckMtx(dest, mt4, 3, 3, 1e-10), 'Mult Matrix error');

     memBlk := AllocMem((16 + 16 + 16)*sizeof(double) + 16);
     dest2 := memBlk;
     if Cardinal(dest2) and $F <> 0 then       // note it's 8 byte aligned!
     	  inc(dest2);

     m1 := dest2;
     inc(m1, 16);
     m2 := m1;
     inc(m2, 16);
     Move(mt1, m1^, sizeof(mt1));
     Move(mt2, m2^, sizeof(mt2));

     ASMMatrixMultAlignedEvenW1OddW2(dest2, 4*sizeof(double), m1, m2, 4, 3, 3, 4, 4*sizeof(double), 4*sizeof(double));

     Move(dest2^, dest[0], sizeof(dest)); 
     Check(CheckMtx(dest, mt4, 3, 3, 1e-10), 'Mult Matrix error');
     FreeMem(memBlk);
end;

procedure TASMMatrixOperations.TestMultASMMatrixVectorEvenW1;
const mt1 : Array[0..15] of double = (0, 1, 2, 4, 3, 4, 5, 5, 6, 7, 8, 6, 7, 8, 9, 10);
      mt2 : Array[0..3] of double = (1, 2, 3, 4);
      mt3 : Array[0..3] of double = (24, 46, 68, 90);
var dest : Array[0..3] of double;
    dest2 : PDouble;
    m1, m2 : PDouble;
    blk : PByte;
begin
     FillChar(dest, sizeof(dest), 0);
     ASMMatrixVectorMultUnAlignedEvenW1(@dest[0], sizeof(double), @mt1[0], @mt2[0], 4, 4, 4, 4*sizeof(double));

     Check(CheckMtx(dest, mt3, -1, -1, 1e-10), 'Mult Matrix error');

     blk := AllocMem((16 + 16 + 16)*sizeof(double) + 16);
     m1 := PDouble(blk);

     inc(PByte(m1), 16 - TASMNativeInt(blk) and $F);
     m2 := m1;
     inc(m2, 16);
     dest2 := m2;
     inc(dest2, 16);

     Move(mt1, m1^, sizeof(mt1));
     Move(mt2, m2^, sizeof(mt2));

     ASMMatrixVectorMultAlignedEvenW1(dest2, sizeof(double), m1, m2, 4, 4, 4, 4*sizeof(double));

     Move(dest2^, dest[0], sizeof(dest));
     Check(CheckMtx(dest, mt3, -1, -1, 1e-10), 'Mult Matrix error');
     FreeMem(blk);
end;

procedure TASMMatrixOperations.TestMultASMOddW1EvenH2Transposed;
const mt1 : Array[0..15] of double = (0, 1, 2, 4, 3, 4, 5, 5, 6, 7, 8, 6, 7, 8, 9, 10);
      mt2 : Array[0..15] of double = (-1, 0, 1, 1, 2, 3, 4, 2, 5, 6, 7, 3, 4, 5, 6, 7);
      mt3 : Array[0..15] of double = (2, 11, 20, 17, 2, 38, 74, 62, 2, 65, 128, 107, 2, 74, 146, 122);

var dest : Array[0..15] of double;
    dest2 : PDouble;
    m1, m2 : PDouble;
    blk : PByte;
begin
     FillChar(dest, sizeof(dest), 0);
     ASMMatrixMultUnAlignedOddW1EvenH2Transposed(@dest[0], 4*sizeof(double), @mt1[0], @mt2[0], 3, 4, 3, 4, 4*sizeof(double), 4*sizeof(double));

     Check(CheckMtx(dest, mt3, -1, -1, 1e-10), 'Mult Matrix error');

     blk := AllocMem((16 + 16 + 16)*sizeof(double) + 16);
     m1 := PDouble(blk);

     inc(PByte(m1), 16 - TASMNativeInt(blk) and $F);
     m2 := m1;
     inc(m2, 16);
     dest2 := m2;
     inc(dest2, 16);

     Move(mt1, m1^, sizeof(mt1));
     Move(mt2, m2^, sizeof(mt2));

     FillChar(dest, sizeof(dest), 0);

     ASMMatrixMultAlignedOddW1EvenH2Transposed(dest2, 4*sizeof(double), m1, m2, 3, 4, 3, 4, 4*sizeof(double), 4*sizeof(double));

     Move(dest2^, dest[0], sizeof(dest));
     Check(CheckMtx(dest, mt3, -1, -1, 1e-10), 'Mult Matrix error');
     FreeMem(blk);
end;

procedure TASMMatrixOperations.TestMultASMOddW1EvenW2;
const mt1 : Array[0..15] of double = (0, 1, 2, 4, 3, 4, 5, 5, 6, 7, 8, 6, 7, 8, 9, 10);
      mt2 : Array[0..15] of double = (-1, 0, 1, 1, 2, 3, 4, 2, 5, 6, 7, 3, 4, 5, 6, 7);
      mt3 : Array[0..11] of double = (12, 15, 18, 8, 30, 42, 54, 26, 48, 69, 90, 44);

var dest : Array[0..11] of double;
    dest2 : PDouble;
    m1, m2 : PDouble;
    memBlk : PDouble;
begin
     FillChar(dest, sizeof(dest), 0);
     ASMMatrixMultUnAlignedOddW1EvenW2(@dest[0], 4*sizeof(double), @mt1[0], @mt2[0], 3, 3, 4, 3, 4*sizeof(double), 4*sizeof(double));

     Check(CheckMtx(dest, mt3, -1, -1, 1e-10), 'Mult Matrix error');

     memBlk := AllocMem((16 + 16 + 16)*sizeof(double) + 16);
     dest2 := memBlk;
     if Cardinal(dest2) and $F <> 0 then       // note it's 8 byte aligned!
     	  inc(dest2);

     m1 := dest2;
     inc(m1, 16);
     m2 := m1;
     inc(m2, 16);

     Move(mt1, m1^, sizeof(mt1));
     Move(mt2, m2^, sizeof(mt2));

     FillChar(dest, sizeof(dest), 0);

     ASMMatrixMultAlignedOddW1EvenW2(dest2, 4*sizeof(double), m1, m2, 3, 3, 4, 3, 4*sizeof(double), 4*sizeof(double));

     Move(dest2^, dest[0], sizeof(dest));
     Check(CheckMtx(dest, mt3, -1, -1, 1e-10), 'Mult Matrix error');
     FreeMem(memBlk);
end;

procedure TASMMatrixOperations.TestMultASMOddW1OddH2Transposed;
const mt1 : Array[0..15] of double = (0, 1, 2, 4, 3, 4, 5, 5, 6, 7, 8, 6, 7, 8, 9, 10);
      mt2 : Array[0..15] of double = (-1, 0, 1, 1, 2, 3, 4, 2, 5, 6, 7, 3, 4, 5, 6, 7);
      mt3 : Array[0..15] of double = (2, 11, 20, 0, 2, 38, 74, 0, 2, 65, 128, 0, 0, 0, 0, 0);
var m1, m2, dest : PDouble;
    d1 : Array[0..15] of double;
    blk : PByte;
begin
     blk := AllocMem((16 + 16 + 16)*sizeof(double) + 16);
     m1 := PDouble(blk);

     inc(PByte(m1), 16 - TASMNativeInt(blk) and $F);
     m2 := m1;
     inc(m2, 16);
     dest := m2;
     inc(dest, 16);

     Move(mt1, m1^, sizeof(mt1));
     Move(mt2, m2^, sizeof(mt2));

     ASMMatrixMultAlignedOddW1OddH2Transposed(dest, 4*sizeof(double), m1, m2, 3, 3, 3, 3, 4*sizeof(double), 4*sizeof(double));
     move(dest^,d1, sizeof(d1));

     Check(CheckMtx(mt3, d1));
     FillChar(dest^, 16*sizeof(double), 0);

     ASMMatrixMultUnAlignedOddW1OddH2Transposed(dest, 4*sizeof(double), m1, m2, 3, 3, 3, 3, 4*sizeof(double), 4*sizeof(double));
     move(dest^,d1, sizeof(d1));

     Check(CheckMtx(mt3, d1));

     FreeMem(blk);
end;

procedure TASMMatrixOperations.TestMultASMOddW1OddW2;
const mt1 : Array[0..15] of double = (0, 1, 2, 4, 3, 4, 5, 5, 6, 7, 8, 6, 7, 8, 9, 10);
      mt2 : Array[0..15] of double = (-1, 0, 1, 1, 2, 3, 4, 2, 5, 6, 7, 3, 4, 5, 6, 7);
      mt3 : Array[0..8] of double = (12, 15, 18, 30, 42, 54, 48, 69, 90);

var dest : Array[0..8] of double;
    dest2 : PDouble;
    m1, m2 : PDouble;
    y: Integer;
    x: Integer;
    pDest : PDouble;
    blk : PByte;
begin
     FillChar(dest, sizeof(dest), 0);
     ASMMatrixMultUnAlignedOddW1OddW2(@dest[0], 3*sizeof(double), @mt1[0], @mt2[0], 3, 3, 3, 3, 4*sizeof(double), 4*sizeof(double));

     Check(CheckMtx(slice(dest, 9), mt3, -1, -1, 1e-10), 'Mult Matrix error');

     blk := AllocMem((16 + 16 + 16)*sizeof(double) + 16);
     dest2 := PDouble(blk);
     inc(PByte(dest2), 16 - TASMNativeInt(blk) and $F);
     m1 := dest2;
     inc(m1, 16);
     m2 := m1;
     inc(m2, 16);
     Move(mt1, m1^, sizeof(mt1));
     Move(mt2, m2^, sizeof(mt2));
     FillChar(dest, sizeof(dest), 0);

     ASMMatrixMultAlignedOddW1OddW2(dest2, 4*sizeof(double), m1, m2, 3, 3, 3, 3, 4*sizeof(double), 4*sizeof(double));

     pDest := dest2;
     for y := 0 to 2 do
     begin
          for x := 0 to 2 do
          begin
               dest[y*3 + x] := pDest^;
               inc(pDest);
          end;

          inc(pDest);
     end;

     Check(CheckMtx(dest, mt3, -1, -1, 1e-10), 'Mult Matrix error');
     FreeMem(blk);
end;

procedure TASMMatrixOperations.TestMultTransposed;
var a : TDoubleDynArray;
    b : TDoubleDynArray;
    dest : TDoubleDynArray;
    dt : TDoubleDynArray;
    dest2 : TDoubleDynArray;
    i, w, h : Integer;
    x : Integer;
begin
     for i := 0 to 20 - 1 do
     begin
          w := Random(100) + 2;
          h := Random(100) + 2;
          SetLength(a, w*h);
          SetLength(b, w*h);
          SetLength(dest, w*h);

          for x := 0 to Length(a) - 1 do
          begin
               a[x] := random;
               b[x] := random;
          end;

          dest := GenericMtxMult(a, b, w, h, h, w);

          SetLength(dest2, Length(dest));
          dt := GenericMtxTranspose(b, h, w);
          if (w mod 2) = 0 then
          begin
               if (h mod 2) = 0
               then
                   ASMMatrixMultUnAlignedEvenW1EvenH2Transposed(@dest2[0], h*sizeof(Double), @a[0], @dt[0], w, h, w, h, w*sizeof(double), w*sizeof(double))
               else
                   ASMMatrixMultUnAlignedEvenW1OddH2Transposed(@dest2[0], h*sizeof(Double), @a[0], @dt[0], w, h, w, h, w*sizeof(double), w*sizeof(double));
          end
          else
          begin
               if (h mod 2) = 0
               then
                   ASMMatrixMultUnAlignedOddW1EvenH2Transposed(@dest2[0], h*sizeof(Double), @a[0], @dt[0], w, h, w, h, w*sizeof(double), w*sizeof(double))
               else
                   ASMMatrixMultUnAlignedOddW1OddH2Transposed(@dest2[0], h*sizeof(Double), @a[0], @dt[0], w, h, w, h, w*sizeof(double), w*sizeof(double));
          end;

          Check(CheckMtx(dest, dest2));

          dest := nil;
          dest2 := nil;
     end;

end;

procedure TASMMatrixOperations.TestStrassenMult;
var x, y : TDoubleDynArray;
    dest1, dest2 : TDoubleDynArray;
    i : Integer;
    startTime1, endTime1 : Int64;
    startTime2, endTime2 : Int64;
    startTime3, endTime3 : Int64;
const cStrassenWidth = 1024;
      cStrassenHeight = 1024;
      cStrassenSize = cStrassenWidth*cStrassenHeight;
begin
     //FillMatrix(cStrassenSize*cStrassenSize, x, y, p1, p2);
     SetLength(x, cStrassenSize);
     SetLength(y, cStrassenSize);
     for i := 0 to cStrassenSize - 1 do
     begin
          x[i] := i;
          y[i] := 1+i;
     end;

     SetLength(dest1, cStrassenHeight*cStrassenHeight);
     SetLength(dest2, cStrassenHeight*cStrassenHeight);

     TryClearCache;
     QueryPerformanceCounter(startTime1);
     BlockedMatrixMultiplication(@dest1[0], cStrassenHeight*sizeof(double), @x[0], @y[0], cStrassenWidth, cStrassenHeight, cStrassenHeight, cStrassenWidth, cStrassenWidth*sizeof(double), cStrassenHeight*sizeof(double), 256);
     QueryPerformanceCounter(endTime1);

     TryClearCache;
     QueryPerformanceCounter(startTime2);
     GenericStrassenMatrixMultiplication(@dest2[0], cStrassenHeight*sizeof(double), @x[0], @y[0], cStrassenWidth, cStrassenHeight, cStrassenHeight, cStrassenWidth, cStrassenWidth*sizeof(double), cStrassenHeight*sizeof(double));
     QueryPerformanceCounter(endTime2);

     Check(CheckMtx(dest1, dest2), 'Error strassen matrix mult failed');

     TryClearCache;
     QueryPerformanceCounter(startTime3);
     ASMStrassenMatrixMultiplication(@dest2[0], cStrassenHeight*sizeof(double), @x[0], @y[0], cStrassenWidth, cStrassenHeight, cStrassenHeight, cStrassenWidth, cStrassenWidth*sizeof(double), cStrassenHeight*sizeof(double));
     QueryPerformanceCounter(endTime3);

     Check(CheckMtx(dest1, dest2), 'Error strassen matrix mult failed');

     Status(Format('%.2f, %.2f, %.2f', [(endTime1 - startTime1)/fPerfFreq*1000, (endTime2 - startTime2)/fPerfFreq*1000, (endTime3 - startTime3)/fPerfFreq*1000]));
end;

{ TASMatrixBlockSizeSetup }

procedure TASMatrixBlockSizeSetup.TestSetupBestBlockSize;
begin
     SetupOptBlockMatrixSize;

     Status('BlockMatrixCacheSize: ' + IntToStr(BlockMatrixCacheSize));
end;

procedure TASMatrixBlockSizeSetup.TestSetupBestBlockVectorSize;
begin
     SetupOptBlockMatrixVectorSize;

     Status('BlockMatrixVectorCachSize: ' + intToStr(BlockMatrixVectorCacheSize));
end;

procedure TASMatrixBlockSizeSetup.TestSetupBlock;
begin
     SetupBlockedMatrixMultSize;

     Status(intToStr(BlockedMatrixMultSize));
end;

procedure TASMatrixBlockSizeSetup.TestSetupBlockedVectorMatrixMultSize;
begin
     SetupBlockedVectorMatrixMultSize;

     Status('blockedVectorMatrixmultSize: ' + IntToStr(BlockedVectorMatrixMultSize));
end;

procedure TASMatrixBlockSizeSetup.TestSetupTransposedOptSize;
begin
     SetupOptTransposeMultMatrixSize;

     Status('TransposeMatrixMultSize: ' + IntToStr(TransposeMatrixMultSize));
end;

initialization
  // Alle Testfälle beim Test-Runner registrieren
  RegisterTest(TestMatrixOperations.Suite);
  RegisterTest(TASMMatrixOperations.Suite);
  RegisterTest(TASMatrixBlockSizeSetup.Suite);
end.

