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

interface

uses {$IFDEF FPC} testregistry {$ELSE} TestFramework {$ENDIF}
     , Classes, SysUtils, Types, SimpleMatrixOperations, Matrix, BaseMatrixTestCase;

type
  TestMatrixOperations = class(TBaseMatrixTestCase)
  published
   procedure TestAdd;
   procedure TestSub;
   procedure TestMult;
   procedure TestTranspose;
   procedure TestCopy;
   procedure TestRowExchange;
   procedure TestNormalize;
   procedure TestApplyfunc;
   procedure TestAbs;
   procedure TestMedian;
  end;

  TASMMatrixOperations = class(TBaseMatrixTestCase)
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestMatrixASMCopy;
    procedure TestASMAdd;
    procedure TestBigASMAdd;
    procedure TestAddAndScale;
    procedure TestMultASMEvenW1EvenW2;
    procedure TestMultASMEvenW1OddW2;
    procedure TestMultASMOddW1EvenW2;
    procedure TestMultASMOddW1OddW2;
    procedure TestMultASMEvenW1EvenH2Transposed;
    procedure TestMultASMEvenW1OddH2Transposed;
    procedure TestMultASMOddW1EvenH2Transposed;
    procedure TestMultASMOddW1OddH2Transposed;
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
    procedure TestTiledMultT1;
    procedure TestTiledMultT2;
    procedure TestTransposedASMEvenWEvenH;
    procedure TestTransposedASMEvenWOddH;
    procedure TestTransposeASSMOddWOddH;
    procedure TestBigTransposedASM;
    procedure TestElementWiseMult;
    procedure TestElementWiseDiv;
    procedure TestElemWiseNorm2;
    procedure TestMtxNormalizeRow;
    procedure TestMtxNormalizeColumn;
    procedure TestMtxBigNormalizeRow;
    procedure TestThreadMatrixMult;
    procedure TestThreadMatrixAddSub;
    procedure TestThreadMatrixAddAndScale;
    procedure TestMatrixMultTria2T1;
    procedure TestThreadedMatrixMultT1;
    procedure TestThreadedMatrixMultT2;
    procedure TestStrassenMult;
    procedure TestAbs;
    procedure TestVarianceRow;
    procedure TestVarianceCol;
    procedure TestMtxVecMult;
    procedure TestBigMtxVecMult;
  end;

  TASMatrixBlockSizeSetup = class(TBaseMatrixTestCase)
  published
    procedure TestSetupBestBlockSize;
    procedure TestSetupBlock;
  end;

implementation

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}

uses ASMMatrixOperations, ThreadedMatrixOperations, MtxThreadPool, mtxTimer,
     BlockSizeSetup, math,
     {$IFDEF x64}
     ASMMatrixMultOperationsx64, ASMMatrixVectorMultOperationsx64, ASMMatrixMultTransposedOperationsx64,
     ASMMatrixTransposeOperationsx64, ASMMatrixNormOperationsx64,
     ASMMatrixMeanOperationsx64, ASMMatrixSumOperationsx64,
     {$ELSE}
     ASMMatrixMultOperations, ASMMatrixVectorMultOperations, ASMMatrixMultTransposedOperations,
     ASMMatrixTransposeOperations, ASMMatrixNormOperations,
     ASMMatrixMeanOperations, ASMMatrixSumOperations,
     {$ENDIF}
     MatrixConst, MathUtilFunc;

procedure TestMatrixOperations.TestAbs;
const mt1 : Array[0..5] of double = (-1, 2, 2, -2, 3, -3);
      mtdest : Array[0..5] of double = (1, 2, 2, 2, 3, 3);
var mtx : TDoubleDynArray;
begin
     SetLength(mtx, Length(mt1));
     Move(mt1[0], mtx[0], sizeof(mt1));

     GenericMtxAbs(@mtx[0], 3*sizeof(double), 3, 2);
     CheckEqualsMem(@mtx[0], @mtdest[0], sizeof(mtdest), 'Error Matrix abs: ' + #13#10 + WriteMtxDyn(mtx, 3));

     Move(mt1[0], mtx[0], sizeof(mt1));
     GenericMtxAbs(@mtx[0], 2*sizeof(double), 2, 3);
     CheckEqualsMem(@mtx[0], @mtdest[0], sizeof(mtdest), 'Error Matrix abs: ' + #13#10 + WriteMtxDyn(mtx, 3));
end;

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

procedure TASMMatrixOperations.SetUp;
begin
     MtxThreadPool.InitMtxThreadPool;
end;

procedure TASMMatrixOperations.TearDown;
begin
     FinalizeMtxThreadPool;
end;

procedure TASMMatrixOperations.TestAbs;
const mt1 : Array[0..5] of double = (-1, 2, 2, -2, 3, -3);
      mtdest : Array[0..5] of double = (1, 2, 2, 2, 3, 3);
      sign : Array[0..1] of double = (-1, 1);
var mt2 : TDoubleDynArray;
    mtx : TDoubleDynArray;
    cnt: Integer;
    mta1 : PConstDoubleArr;
begin
     SetLength(mtx, Length(mt1));
     Move(mt1[0], mtx[0], sizeof(mt1));
     ASMMatrixAbs(@mtx[0], 3*sizeof(double), 3, 2);
     CheckEqualsMem(@mtx[0], @mtdest[0], sizeof(mtdest), 'Error Matrix abs: ' + #13#10 + WriteMtxDyn(mtx, 3));

     Move(mt1[0], mtx[0], sizeof(mt1));

     SetLength(mt2, 1022);
     for cnt := 0 to Length(mt2) - 1 do
         mt2[cnt] := Random(10)*sign[random(2)];

     ASMMatrixAbs(@mt2[0], (length(mt2) div 14)*sizeof(double), (length(mt2) div 14), 14);
     for cnt := 0 to Length(mt2) - 1 do
         Check(mt2[cnt] >= 0, 'Error matrix abs - negative value found');

     SetLength(mt2, 1022);
     for cnt := 0 to Length(mt2) - 1 do
     begin
          mt2[cnt] := Random(10)*sign[random(2)];
          if ((cnt + 1) mod 73) = 0 then
             mt2[cnt] := -1000;
     end;

     ASMMatrixAbs(@mt2[0], (length(mt2) div 14)*sizeof(double), (length(mt2) div 14) - 1, 14);
     for cnt := 0 to Length(mt2) - 1 do
         Check((mt2[cnt] = -1000) or (mt2[cnt] >= 0), 'Error matrix abs - negative value found'); 

     // aligned checks
     mta1 := GetMemory(1024*sizeof(double));
     for cnt := 0 to 1024 - 1 do
         mta1^[cnt] := Random(10)*sign[random(2)];
     
     ASMMatrixAbs(PDouble(mta1), 128*sizeof(double), 128, 8);
     for cnt := 0 to Length(mt2) - 1 do
         Check(mta1^[cnt] >= 0, 'Error matrix abs - negative value found'); 

     for cnt := 0 to 1024 - 1 do
     begin
          mta1^[cnt] := Random(10)*sign[random(2)];
          if ((cnt + 1) mod 128) = 0 then
             mta1^[cnt] := 0;
     end;
     
     ASMMatrixAbs(PDouble(mta1), 128*sizeof(double), 127, 8);
     for cnt := 0 to Length(mt2) - 1 do
         Check(mta1^[cnt] >= 0, 'Error matrix abs - negative value found'); 
     
     FreeMem(mta1);
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

procedure TASMMatrixOperations.TestAddAndScale;
const cBlkWidth = 24;
      cBlkHeight = 3;
var cMtx : Array[0..cBlkWidth*cBlkHeight - 1] of double;
    i : integer;
    dest : Array[0..cBlkWidth*cBlkHeight - 1] of double;
    dest1 : Array[0..cBlkWidth*cBlkHeight - 1] of double;
    aDest : PDouble;
begin
     for i := 0 to Length(cMtx) - 1 do
         cMtx[i] := i;

     Move(cMtx, dest, sizeof(cMtx));
     GenericMtxScaleAndAdd(@dest[0], cBlkWidth*sizeof(double), cBlkWidth, cBlkHeight, 1, 2);

     // check result
     for i := 0 to Length(cMtx) - 1 do
         check( cMtx[i]*2 + 1 = dest[i], 'Error in scale and add');

     Move(cMtx, dest, sizeof(cMtx));
     GenericMtxAddAndScale(@dest[0], cBlkWidth*sizeof(double), cBlkWidth, cBlkHeight, 1, 2);
     // check result
     for i := 0 to Length(cMtx) - 1 do
         check( (cMtx[i] + 1)*2 = dest[i], 'Error in add and scale');

     Move(cMtx, dest, sizeof(cMtx));
     ASMMatrixScaleAndAdd(@dest[0], cBlkWidth*sizeof(double), cBlkWidth, cBlkHeight, 1, 2);
     // check result
     for i := 0 to Length(cMtx) - 1 do
         check( cMtx[i]*2 + 1 = dest[i], 'Error in scale and add');

     Move(cMtx, dest, sizeof(cMtx));
     ASMMatrixAddAndScale(@dest[0], cBlkWidth*sizeof(double), cBlkWidth, cBlkHeight, 1, 2);
     // check result
     for i := 0 to Length(cMtx) - 1 do
         check( (cMtx[i] + 1)*2 = dest[i], 'Error in scale and add');

     Move(cMtx, dest, sizeof(cMtx));
     ASMMatrixScaleAndAdd(@dest[0], cBlkWidth*sizeof(double), cBlkWidth - 1, cBlkHeight, 1, 2);
     // check result
     for i := 0 to Length(cMtx) - 1 do
         if (i + 1) mod cBlkWidth = 0
         then
             check( cmtx[i] = dest[i], 'error in scale and add')
         else
             check( cMtx[i]*2 + 1 = dest[i], 'Error in scale and add');

     Move(cMtx, dest, sizeof(cMtx));
     ASMMatrixAddAndScale(@dest[0], cBlkWidth*sizeof(double), cBlkWidth - 1, cBlkHeight, 1, 2);
     // check result
     for i := 0 to Length(cMtx) - 1 do
         if (i + 1) mod cBlkWidth = 0
         then
             check( cMtx[i] = dest[i], 'error in scale and add')
         else
             check( (cMtx[i] + 1)*2 = dest[i], 'Error in scale and add');

     Move(cMtx, dest, sizeof(cMtx));
     ASMMatrixScaleAndAdd(@dest[0], cBlkWidth*sizeof(double), 1, cBlkHeight, 1, 2);
     // check result
     for i := 0 to Length(cMtx) - 1 do
         if i mod cBlkWidth = 0
         then
             check( cMtx[i]*2 + 1 = dest[i], 'Error in scale and add')
         else
             check( cmtx[i] = dest[i], 'error in scale and add');

     Move(cMtx, dest, sizeof(cMtx));
     ASMMatrixAddAndScale(@dest[0], cBlkWidth*sizeof(double), 1, cBlkHeight, 1, 2);
     // check result
     for i := 0 to Length(cMtx) - 1 do
         if (i) mod cBlkWidth = 0
         then
             check( (cMtx[i] + 1)*2 = dest[i], 'Error in scale and add')
         else
             check( cMtx[i] = dest[i], 'error in scale and add');


     // ###########################################
     // #### Scale and add with one width or height element
     aDest := GetMemory(cBlkWidth*(cBlkheight + 1)*sizeof(double));
     Move(cMtx, dest, sizeof(cMtx));
     Move(cMtx, dest1, sizeof(cMtx));
     Move(cMtx, aDest^, sizeof(cMtx));

     GenericMtxScaleAndAdd(@dest[0], sizeof(double), 1, cBlkWidth - 1, -0.1, 2);
     ASMMatrixScaleAndAdd(@dest1[0], sizeof(double), 1, cBlkWidth - 1, -0.1, 2);
     ASMMatrixScaleAndAdd(aDest, sizeof(double), 1, cBlkWidth - 1, -0.1, 2);

     Check(CheckMtx(dest, dest1), 'Error scale and add width=1');
     Move(aDest^, dest1, sizeof(dest1));
     Check(CheckMtx(dest, dest1), 'Error scale and add aligned width=1');
     

     Move(cMtx, dest, sizeof(cMtx));
     Move(cMtx, dest1, sizeof(cMtx));
     Move(cMtx, aDest^, sizeof(cMtx));

     GenericMtxAddAndScale(@dest[0], sizeof(double), 1, cBlkWidth, -0.1, 2);
     ASMMatrixAddAndScale(@dest1[0], sizeof(double), 1, cBlkWidth, -0.1, 2);
     ASMMatrixAddAndScale(aDest, sizeof(double), 1, cBlkWidth, -0.1, 2);

     Check(CheckMtx(dest, dest1), 'Error add and scale width=1');
     Move(aDest^, dest1, sizeof(dest1));
     Check(CheckMtx(dest, dest1), 'Error add and scale aligned width=1');


     Move(cMtx, dest, sizeof(cMtx));
     Move(cMtx, dest1, sizeof(cMtx));
     Move(cMtx, aDest^, sizeof(cMtx));

     GenericMtxScaleAndAdd(@dest[0], cBlkWidth*sizeof(double), cBlkWidth, 1, -0.1, 2);
     ASMMatrixScaleAndAdd(@dest1[0], cBlkWidth*sizeof(double), cBlkWidth, 1,  -0.1, 2);
     ASMMatrixScaleAndAdd(aDest, cBlkWidth*sizeof(double), cBlkWidth, 1,  -0.1, 2);
     
     Check(CheckMtx(dest, dest1), 'Error scale and add height=1');
     Move(aDest^, dest1, sizeof(dest1));
     Check(CheckMtx(dest, dest1), 'Error scale and add height=1');

     Move(cMtx, dest, sizeof(cMtx));
     Move(cMtx, dest1, sizeof(cMtx));
     Move(cMtx, aDest^, sizeof(cMtx));

     GenericMtxAddAndScale(@dest[0], cBlkWidth*sizeof(double), cBlkWidth, 1, -0.1, 2);
     ASMMatrixAddAndScale(@dest1[0], cBlkWidth*sizeof(double), cBlkWidth, 1, -0.1, 2);
     ASMMatrixAddAndScale(aDest, cBlkWidth*sizeof(double), cBlkWidth, 1, -0.1, 2);
     
     Check(CheckMtx(dest, dest1), 'Error scale and add height=1');
     Move(aDest^, dest1, sizeof(dest1));
     Check(CheckMtx(dest, dest1), 'Error scale and add height=1');

     FreeMem(aDest);
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

     startTime1 := MtxGetTime;
     GenericMtxAdd(@dest1[0], cMtxLineWidth, @x[0], @y[0], cMtxWidth, cMtxheight, cMtxLinewidth, cMtxLinewidth);
     endTime1 := MtxGetTime;

     startTime2 := MtxGetTime;
     ASMMatrixAdd(@dest2[0], cMtxLineWidth, @x[0], @y[0], cMtxWidth, cMtxheight, cMtxLinewidth, cMtxLinewidth);
     endTime2 := MtxGetTime;

     startTime3 := MtxGetTime;
     ASMMatrixAdd(dest2a, cMtxLineWidth, xa, ya, cMtxWidth, cMtxheight, cMtxLinewidth, cMtxLinewidth);
     endTime3 := MtxGetTime;

     Status(Format('%.2f, %.2f, %.2f', [(endTime1 - startTime1)/mtxFreq*1000, (endTime2 - startTime2)/mtxFreq*1000,
                 (endTime3 - startTime3)/mtxFreq*1000]));

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
const cMtxWidth = 1500;
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

     startTime1 := MtxGetTime;
     GenericMtxMult(@dest1[0], cMtxLineWidth, @x[0], @y[0], cMtxWidth, cMtxheight, cMtxHeight, cMtxWidth, cMtxLinewidth, cMtxLinewidth2);
     endTime1 := MtxGetTime;

     startTime2 := MtxGetTime;
     ASMMatrixMult(@dest2[0], cMtxLineWidth, @x[0], @y[0], cMtxWidth, cMtxheight, cMtxHeight, cMtxWidth, cMtxLinewidth, cMtxLinewidth2);
     endTime2 := MtxGetTime;

     startTime3 := MtxGetTime;
     ASMMatrixMult(dest2a, cMtxLineWidth, xa, ya, cMtxWidth, cMtxheight, cMtxHeight, cMtxWidth, cMtxLinewidth, cMtxLinewidth2);
     endTime3 := MtxGetTime;

     startTime4 := MtxGetTime;
     GenericMtxTranspose(za, cMtxLineWidth, ya, cMtxLineWidth2, cMtxHeight, cMtxWidth);
     ASMMatrixMultAlignedEvenW1EvenH2Transposed(dest3a, cMtxLineWidth, xa, za, cMtxWidth, cMtxheight, cMtxWidth, cMtxheight, cMtxLinewidth, cMtxLinewidth);
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

     FreeMem(xa);
     FreeMem(ya);
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
const cMtxWidth = 3*cCacheMtxSize;
      cMtxHeight = cCacheMtxSize*2;
      cMtxSize = cMtxWidth*cMtxHeight;
      cMtxLinewidth = cMtxWidth*sizeof(double);
      cMtxLineWidth2 = cMtxHeight*sizeof(double);
begin
     //SetThreadAffinityMask(GetCurrentThread, 1);
     randomize;
     FillMatrix(cMtxSize, x, y, xa, ya);

     SetLength(dest1, cMtxHeight*cMtxHeight);
     SetLength(dest2, cMtxHeight*cMtxHeight);
     dest2a := AllocMem(cMtxHeight*cMtxHeight*sizeof(double));
     dest3a := AllocMem(cMtxHeight*cMtxHeight*sizeof(double));

     startTime5 := MtxGetTime;
     BlockedMatrixMultiplication(@dest2[0], cMtxLineWidth2, @x[0], @y[0], cMtxWidth, cMtxheight, cMtxHeight, cMtxWidth, cMtxLinewidth, cMtxLinewidth2, 352);
     endTime5 := MtxGetTime;

     startTime1 := MtxGetTime;
     GenericMtxMult(@dest1[0], cMtxLineWidth2, @x[0], @y[0], cMtxWidth, cMtxheight, cMtxHeight, cMtxWidth, cMtxLinewidth, cMtxLinewidth2);
     endTime1 := MtxGetTime;

     startTime2 := MtxGetTime;
     GenericBlockedMatrixMultiplication(@dest2[0], cMtxLineWidth2, @x[0], @y[0], cMtxWidth, cMtxheight, cMtxHeight, cMtxWidth, cMtxLinewidth, cMtxLinewidth2, 352);
     endTime2 := MtxGetTime;

     res := CheckMtxIdx(dest1, dest2, idx);
     if not res then
        Status(IntToStr(idx));
     check(res);

     res := CheckMtxIdx(dest1, dest2, idx);
     if not res then
        Status(IntToStr(idx));
     check(res);

     startTime4 := MtxGetTime;
     BlockedMatrixMultiplication(dest3a, cMtxLineWidth2, xa, ya, cMtxWidth, cMtxheight, cMtxHeight, cMtxWidth, cMtxLinewidth, cMtxLinewidth2, 352);
     endTime4 := MtxGetTime;

     startTime3 := MtxGetTime;
     BlockedMatrixMultiplicationDirect(dest2a, cMtxLineWidth2, xa, ya, cMtxWidth, cMtxheight, cMtxHeight, cMtxWidth, cMtxLinewidth, cMtxLinewidth2, 352);
     endTime3 := MtxGetTime;


     Status(Format('%.2f, %.2f, %.2f, %.2f, %.2f', [(endTime1 - startTime1)/mtxFreq*1000, (endTime2 - startTime2)/mtxFreq*1000,
                (endTime3 - startTime3)/mtxFreq*1000, (endTime4 - startTime4)/mtxFreq*1000,
                (endTime5 - startTime5)/mtxFreq*1000]));

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
     vecstartTime1 := MtxGetTime;
     GenericMtxMult(@dest1[0], sizeof(double), @x[0], @y[0], cMtxWidth, cMtxheight, 1, cMtxWidth, cMtxLinewidth, sizeof(double));
     vecendTime1 := MtxGetTime;

     vecstartTime2 := MtxGetTime;
     BlockedMatrixVectorMultiplication(@dest2[0], sizeof(double), @x[0], @y[0], cMtxWidth, cMtxheight, cMtxWidth, cMtxLinewidth);
     vecendTime2 := MtxGetTime;

     startTime2 := MtxGetTime;
     ASMMatrixMult(@dest2[0], sizeof(double), @x[0], @y[0], cMtxWidth, cMtxheight, 1, cMtxWidth, cMtxLinewidth, sizeof(double));
     endTime2 := MtxGetTime;

     startTime3 := MtxGetTime;
     ASMMatrixMult(dest2a, sizeof(double), xa, ya, cMtxWidth, cMtxheight, 1, cMtxWidth, cMtxLinewidth, sizeof(double));
     endTime3 := MtxGetTime;

     vecstartTime3 := MtxGetTime;
     BlockedMatrixVectorMultiplication(dest2a, sizeof(double), xa, ya, cMtxWidth, cMtxheight, cMtxWidth, cMtxLinewidth);
     vecendTime3 := MtxGetTime;

     Status(Format('%.2f, %.2f, %.2f, %.2f, %.2f',
         [(vecendTime1 - vecstartTime1)/mtxFreq*1000, (vecendTime2 - vecstartTime2)/mtxFreq*1000,
          (vecendTime3 - vecstartTime3)/mtxFreq*1000, (endTime2 - startTime2)/mtxFreq*1000,
          (endTime3 - startTime3)/mtxFreq*1000]));

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

     startTime1 := MtxGetTime;
     GenericMtxMult(@dest1[0], cMtxLineWidth2, @x[0], @y[0], cMtxWidth, cMtxheight, cMtxHeight, cMtxWidth, cMtxLinewidth, cMtxLinewidth2);
     endTime1 := MtxGetTime;

     startTime2 := MtxGetTime;
     BlockedMatrixMultiplicationDirect(@dest2[0], cMtxLineWidth2, @x[0], @y[0], cMtxWidth, cMtxheight, cMtxHeight, cMtxWidth, cMtxLinewidth, cMtxLinewidth2);
     endTime2 := MtxGetTime;

     res := CheckMtxIdx(dest1, dest2, idx);
     if not res then
        Status(IntToStr(idx));
     check(res);

     startTime3 := MtxGetTime;
     BlockedMatrixMultiplicationDirect(dest2a, cMtxLineWidth2, xa, ya, cMtxWidth, cMtxheight, cMtxHeight, cMtxWidth, cMtxLinewidth, cMtxLinewidth2, 256);
     endTime3 := MtxGetTime;

     startTime4 := MtxGetTime; 
     BlockedMatrixMultiplication(dest3a, cMtxLineWidth2, xa, ya, cMtxWidth, cMtxheight, cMtxHeight, cMtxWidth, cMtxLinewidth, cMtxLinewidth2, 256);
     endTime4 := MtxGetTime;

     Status(Format('%.2f, %.2f, %.2f, %.2f', [(endTime1 - startTime1)/mtxFreq*1000,
       (endTime2 - startTime2)/mtxFreq*1000, (endTime3 - startTime3)/mtxFreq*1000, (endTime4 - startTime4)/mtxFreq*1000]));

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
     vecstartTime1 := MtxGetTime;
     GenericMtxMult(@dest1[0], sizeof(double), @x[0], @y[0], cMtxWidth, cMtxheight, 1, cMtxWidth, cMtxLinewidth, sizeof(double));
     vecendTime1 := MtxGetTime;

     vecstartTime2 := MtxGetTime;
     BlockedMatrixVectorMultiplication(@dest2[0], sizeof(double), @x[0], @y[0], cMtxWidth, cMtxheight, cMtxWidth, cMtxLinewidth);
     vecendTime2 := MtxGetTime;

     startTime2 := MtxGetTime;
     ASMMatrixMult(@dest2[0], sizeof(double), @x[0], @y[0], cMtxWidth, cMtxheight, 1, cMtxWidth, cMtxLinewidth, sizeof(double));
     endTime2 := MtxGetTime;

     startTime3 := MtxGetTime;
     ASMMatrixMult(dest2a, sizeof(double), xa, ya, cMtxWidth, cMtxheight, 1, cMtxWidth, cMtxLinewidth, sizeof(double));
     endTime3 := MtxGetTime;

     vecstartTime3 := MtxGetTime;;
     BlockedMatrixVectorMultiplication(dest2a, sizeof(double), xa, ya, cMtxWidth, cMtxheight, cMtxWidth, cMtxLinewidth);
     vecendTime3 := MtxGetTime;

     Status(Format('%.2f, %.2f, %.2f, %.2f, %.2f',
         [(vecendTime1 - vecstartTime1)/mtxFreq*1000, (vecendTime2 - vecstartTime2)/mtxFreq*1000,
          (vecendTime3 - vecstartTime3)/mtxFreq*1000, (endTime2 - startTime2)/mtxFreq*1000,
          (endTime3 - startTime3)/mtxFreq*1000]));

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

procedure TASMMatrixOperations.TestTiledMultT1;
var x, y, dest1, dest2 : TDoubleDynArray;
    idx : integer;
    startTime1: Int64;
    endTime1: Int64;
    startTime2: Int64;
    endTime2: Int64;
    res : boolean;
const cMtxWidth = 8;
      cMtxHeight = 8;
      cMtxSize = cMtxWidth*cMtxHeight;
      cMtxLinewidth = cMtxWidth*8;
      cMtxLineWidth2 = cMtxWidth*8;
begin
     //SetThreadAffinityMask(GetCurrentThread, 1);
     randomize;
     SetLength(x, cMtxSize);
     SetLength(y, cMtxSize);

     for idx := 0 to Length(x) - 1 do
     begin
          x[idx] := idx;
          y[idx] := idx - 1;
     end;

     SetLength(dest1, cMtxWidth*cMtxWidth);
     SetLength(dest2, cMtxWidth*cMtxWidth);

     startTime2 := MtxGetTime;
     GenericTranspMtxMult(@dest1[0], cMtxLineWidth2, @x[0], @y[0], cMtxWidth, cMtxheight, cMtxWidth, cMtxHeight, cMtxLinewidth, cMtxLinewidth2);
     endTime2 := MtxGetTime;

     startTime1 := MtxGetTime;
     BlockedMatrixMultiplicationT1(@dest2[0], cMtxLineWidth2, @x[0], @y[0], cMtxWidth, cMtxheight, cMtxWidth, cMtxHeight, cMtxLinewidth, cMtxLinewidth2, 4, doNone, nil);
     endTime1 := MtxGetTime;

     res := CheckMtxIdx(dest1, dest2, idx);
     if not res then
        Status(IntToStr(idx));

     Status(Format('%.2f, %.2f', [(endTime1 - startTime1)/mtxFreq*1000, (endTime2 - startTime2)/mtxFreq*1000]));
end;

procedure TASMMatrixOperations.TestTiledMultT2;
var x, y, dest1, dest2 : TDoubleDynArray;
    idx : integer;
    startTime1: Int64;
    endTime1: Int64;
    startTime2: Int64;
    endTime2: Int64;
    res : boolean;
const cMtxWidth = 7;
      cMtxHeight = 8;
      cMtxSize = cMtxWidth*cMtxHeight;
      cMtxLinewidth = cMtxWidth*8;
      cMtxLineWidth2 = cMtxWidth*8;
begin
     //SetThreadAffinityMask(GetCurrentThread, 1);
     randomize;

     SetLength(x, cMtxSize);
     SetLength(y, cMtxSize);
     for idx := 0 to Length(x) - 1 do
     begin
          x[idx] := idx;
          y[idx] := idx - 1;
     end;

     SetLength(dest1, cMtxHeight*cMtxHeight);
     SetLength(dest2, cMtxHeight*cMtxHeight);

     startTime2 := MtxGetTime;
     GenericMtxMultTransp(@dest1[0], cMtxHeight*sizeof(double), @x[0], @y[0], cMtxWidth, cMtxheight, cMtxWidth, cMtxHeight, cMtxLinewidth, cMtxLinewidth2);
     endTime2 := MtxGetTime;

     startTime1 := MtxGetTime;
     BlockedMatrixMultiplicationT2(@dest2[0], cMtxHeight*sizeof(double), @x[0], @y[0], cMtxWidth, cMtxheight, cMtxWidth, cMtxHeight, cMtxLinewidth, cMtxLinewidth2, 4, doNone, nil);
     endTime1 := MtxGetTime;

     res := CheckMtxIdx(dest1, dest2, idx);
     if not res then
        Status(IntToStr(idx));

     Status(Format('%.2f, %.2f', [(endTime1 - startTime1)/mtxFreq*1000, (endTime2 - startTime2)/mtxFreq*1000]));
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

     FillChar(dest1a^, cMtxWidth*cMtxWidth*sizeof(double), 0);
     FillChar(dest2a^, cMtxWidth*cMtxWidth*sizeof(double), 0);

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

     startTime1 := MtxGetTime;
     GenericMtxTranspose(@dest1[0], cMtxLineWidth2, @x[0], cMtxLinewidth, cMtxWidth, cMtxheight);
     endTime1 := MtxGetTime;

     startTime2 := MtxGetTime;
     ASMMatrixTransposeAlignedEvenWEvenH(dest1a, cMtxLineWidth2, xa, cMtxLinewidth, cMtxWidth, cMtxheight);
     endTime2 := MtxGetTime;

     Status(Format('%.2f, %.2f', [(endTime1 - startTime1)/mtxFreq*1000, (endTime2 - startTime2)/mtxFreq*1000]));

     Move(dest1a^, dest2[0], length(dest2)*sizeof(double));
     res := CheckMtxIdx(dest1, dest2, idx);
     if not res then
        Status(IntToStr(idx));
     check(res);

     FreeMem(dest1a);
     FreeMem(dest2a);
     FreeMem(xa);
end;

procedure TASMMatrixOperations.TestElementWiseDiv;
var mtx : Array[0..122, 0..123] of double;
    x: Integer;
    y: Integer;
begin
     // basically the same testcase as the elementwise multiplication
     mtx[0, 0] := -2;
     GenericMtxElemDiv(PDouble(@mtx[0, 0]), sizeof(double), PDouble(@mtx[0, 0]), PDouble(@mtx[0, 0]), 1, 1, sizeof(double), sizeof(double));
     Check(SameValue(mtx[0, 0], 1), 'Error 1x1 elementwise mult failed');

     mtx[0, 0] := -2;
     GenericMtxElemDiv(PDouble(@mtx[0, 0]), 2*sizeof(double), PDouble(@mtx[0, 0]), PDouble(@mtx[0, 0]), 1, 1, 2*sizeof(double), 2*sizeof(double));
     Check(SameValue(mtx[0, 0], 1), 'Error 1x1 elementwise mult failed');

     mtx[0, 0] := -2;
     ASMMatrixElemDiv(PDouble(@mtx[0, 0]), sizeof(double), PDouble(@mtx[0, 0]), PDouble(@mtx[0, 0]), 1, 1, sizeof(double), sizeof(double));
     Check(SameValue(mtx[0, 0], 1), 'Error 1x1 elementwise mult failed');

     mtx[0, 0] := -2;
     ASMMatrixElemDiv(PDouble(@mtx[0, 0]), 2*sizeof(double), PDouble(@mtx[0, 0]), PDouble(@mtx[0, 0]), 1, 1, 2*sizeof(double), 2*sizeof(double));
     Check(SameValue(mtx[0, 0], 1), 'Error 1x1 elementwise mult failed');

     mtx[0, 0] := -2;
     mtx[0, 1] := -2;
     mtx[0, 2] := -2;
     mtx[0, 3] := -2;
     GenericMtxElemDiv(PDouble(@mtx[0, 0]), 2*sizeof(double), PDouble(@mtx[0, 0]), PDouble(@mtx[0, 0]), 2, 2, 2*sizeof(double), 2*sizeof(double));
     Check(SameValue(mtx[0, 0], 1) and SameValue(mtx[0, 1], 1), 'Error 2x2 elementwise mult even LineSize failed');
     Check(SameValue(mtx[0, 2], 1) and SameValue(mtx[0, 3], 1), 'Error 2x2 elementwise mult even LineSize failed');

     mtx[0, 0] := -2;
     mtx[0, 1] := -2;
     mtx[0, 3] := -2;
     mtx[0, 4] := -2;
     GenericMtxElemDiv(PDouble(@mtx[0, 0]), 3*sizeof(double), PDouble(@mtx[0, 0]), PDouble(@mtx[0, 0]), 2, 2, 3*sizeof(double), 3*sizeof(double));
     Check(SameValue(mtx[0, 0], 1) and SameValue(mtx[0, 1], 1), 'Error 2x2 elementwise mult odd LineSize failed');
     Check(SameValue(mtx[0, 3], 1) and SameValue(mtx[0, 4], 1), 'Error 2x2 elementwise mult odd LineSize failed');

     mtx[0, 0] := -2;
     mtx[0, 1] := -2;
     mtx[0, 2] := -2;
     mtx[0, 3] := -2;
     ASMMatrixElemDiv(PDouble(@mtx[0, 0]), 2*sizeof(double), PDouble(@mtx[0, 0]), PDouble(@mtx[0, 0]), 2, 2, 2*sizeof(double), 2*sizeof(double));
     Check(SameValue(mtx[0, 0], 1) and SameValue(mtx[0, 1], 1), 'Error 2x2 elementwise asm mult even LineSize failed');
     Check(SameValue(mtx[0, 2], 1) and SameValue(mtx[0, 3], 1), 'Error 2x2 elementwise asm mult even LineSize failed');


     mtx[0, 0] := -2;
     mtx[0, 1] := -2;
     mtx[0, 3] := -2;
     mtx[0, 4] := -2;
     ASMMatrixElemDiv(PDouble(@mtx[0, 0]), 3*sizeof(double), PDouble(@mtx[0, 0]), PDouble(@mtx[0, 0]), 2, 2, 3*sizeof(double), 3*sizeof(double));
     Check(SameValue(mtx[0, 0], 1) and SameValue(mtx[0, 1], 1), 'Error 2x2 elementwise mult failed');
     Check(SameValue(mtx[0, 3], 1) and SameValue(mtx[0, 4], 1), 'Error 2x2 elementwise mult failed');


     mtx[0, 0] := -2;
     mtx[0, 1] := -2;
     mtx[0, 2] := -2;
     mtx[0, 3] := -2;
     mtx[0, 4] := -2;
     mtx[0, 5] := -2;
     GenericMtxElemDiv(PDouble(@mtx[0, 0]), 3*sizeof(double), PDouble(@mtx[0, 0]), PDouble(@mtx[0, 0]), 3, 2, 3*sizeof(double), 3*sizeof(double));
     Check(SameValue(mtx[0, 0], 1) and SameValue(mtx[0, 1], 1), 'Error 3x2 elementwise mult failed');
     Check(SameValue(mtx[0, 2], 1) and SameValue(mtx[0, 3], 1), 'Error 3x2 elementwise mult failed');
     Check(SameValue(mtx[0, 1], 1) and SameValue(mtx[0, 5], 1), 'Error 3x2 elementwise mult failed');

     mtx[0, 0] := -2;
     mtx[0, 1] := -2;
     mtx[0, 2] := -2;
     mtx[0, 3] := -2;
     mtx[0, 4] := -2;
     mtx[0, 5] := -2;
     GenericMtxElemDiv(PDouble(@mtx[0, 0]), 2*sizeof(double), PDouble(@mtx[0, 0]), PDouble(@mtx[0, 0]), 2, 3, 2*sizeof(double), 2*sizeof(double));
     Check(SameValue(mtx[0, 0], 1) and SameValue(mtx[0, 1], 1), 'Error 2x3 elementwise mult failed');
     Check(SameValue(mtx[0, 2], 1) and SameValue(mtx[0, 3], 1), 'Error 2x3 elementwise mult failed');
     Check(SameValue(mtx[0, 1], 1) and SameValue(mtx[0, 5], 1), 'Error 2x3 elementwise mult failed');

     mtx[0, 0] := -2;
     mtx[0, 1] := -2;
     mtx[0, 2] := -2;
     mtx[0, 3] := -2;
     mtx[0, 4] := -2;
     mtx[0, 5] := -2;
     ASMMatrixElemDiv(PDouble(@mtx[0, 0]), 3*sizeof(double), PDouble(@mtx[0, 0]), PDouble(@mtx[0, 0]), 3, 2, 3*sizeof(double), 3*sizeof(double));
     Check(SameValue(mtx[0, 0], 1) and SameValue(mtx[0, 1], 1), 'Error 3x2 elementwise asm mult failed');
     Check(SameValue(mtx[0, 2], 1) and SameValue(mtx[0, 3], 1), 'Error 3x2 elementwise asm mult failed');
     Check(SameValue(mtx[0, 1], 1) and SameValue(mtx[0, 5], 1), 'Error 3x2 elementwise asm mult failed');

     mtx[0, 0] := -2;
     mtx[0, 1] := -2;
     mtx[0, 2] := -2;
     mtx[0, 3] := -2;
     mtx[0, 4] := -2;
     mtx[0, 5] := -2;
     ASMMatrixElemDiv(PDouble(@mtx[0, 0]), 2*sizeof(double), PDouble(@mtx[0, 0]), PDouble(@mtx[0, 0]), 2, 3, 2*sizeof(double), 2*sizeof(double));
     Check(SameValue(mtx[0, 0], 1) and SameValue(mtx[0, 1], 1), 'Error 2x3 elementwise asm mult failed');
     Check(SameValue(mtx[0, 2], 1) and SameValue(mtx[0, 3], 1), 'Error 2x3 elementwise asm mult failed');
     Check(SameValue(mtx[0, 1], 1) and SameValue(mtx[0, 5], 1), 'Error 2x3 elementwise asm mult failed');


     for x := 0 to 122 do
         for y := 0 to 123 do
             mtx[x, y] := -2;

     GenericMtxElemDiv(PDouble(@mtx[0, 0]), 123*sizeof(double), PDouble(@mtx[0, 0]), PDouble(@mtx[0, 0]), 123, 124, 123*sizeof(double), 123*sizeof(double));

     for x := 0 to 122 do
         for y := 0 to 123 do
             Check(SameValue(mtx[x, y], 1), 'Error big elementwise mult failed @' + IntToStr(x) + ',' + IntToStr(y));

     for x := 0 to 122 do
         for y := 0 to 123 do
             mtx[x, y] := -2;

     ASMMatrixElemDiv(PDouble(@mtx[0, 0]), 123*sizeof(double), PDouble(@mtx[0, 0]), PDouble(@mtx[0, 0]), 123, 124, 123*sizeof(double), 123*sizeof(double));

     for x := 0 to 122 do
         for y := 0 to 123 do
             Check(SameValue(mtx[x, y], 1), 'Error big elementwise asm mult failed @' + IntToStr(x) + ',' + IntToStr(y));
end;

procedure TASMMatrixOperations.TestElementWiseMult;
var mtx : Array[0..122, 0..123] of double;
    x: Integer;
    y: Integer;
begin
     mtx[0, 0] := -2;
     GenericMtxElemMult(PDouble(@mtx[0, 0]), sizeof(double), PDouble(@mtx[0, 0]), PDouble(@mtx[0, 0]), 1, 1, sizeof(double), sizeof(double));
     Check(SameValue(mtx[0, 0], 4), 'Error 1x1 elementwise mult failed');

     mtx[0, 0] := -2;
     GenericMtxElemMult(PDouble(@mtx[0, 0]), 2*sizeof(double), PDouble(@mtx[0, 0]), PDouble(@mtx[0, 0]), 1, 1, 2*sizeof(double), 2*sizeof(double));
     Check(SameValue(mtx[0, 0], 4), 'Error 1x1 elementwise mult failed');

     mtx[0, 0] := -2;
     ASMMatrixElemMult(PDouble(@mtx[0, 0]), sizeof(double), PDouble(@mtx[0, 0]), PDouble(@mtx[0, 0]), 1, 1, sizeof(double), sizeof(double));
     Check(SameValue(mtx[0, 0], 4), 'Error 1x1 elementwise mult failed');

     mtx[0, 0] := -2;
     ASMMatrixElemMult(PDouble(@mtx[0, 0]), 2*sizeof(double), PDouble(@mtx[0, 0]), PDouble(@mtx[0, 0]), 1, 1, 2*sizeof(double), 2*sizeof(double));
     Check(SameValue(mtx[0, 0], 4), 'Error 1x1 elementwise mult failed');

     mtx[0, 0] := -2;
     mtx[0, 1] := -2;
     mtx[0, 2] := -2;
     mtx[0, 3] := -2;
     GenericMtxElemMult(PDouble(@mtx[0, 0]), 2*sizeof(double), PDouble(@mtx[0, 0]), PDouble(@mtx[0, 0]), 2, 2, 2*sizeof(double), 2*sizeof(double));
     Check(SameValue(mtx[0, 0], 4) and SameValue(mtx[0, 1], 4), 'Error 2x2 elementwise mult even LineSize failed');
     Check(SameValue(mtx[0, 2], 4) and SameValue(mtx[0, 3], 4), 'Error 2x2 elementwise mult even LineSize failed');

     mtx[0, 0] := -2;
     mtx[0, 1] := -2;
     mtx[0, 3] := -2;
     mtx[0, 4] := -2;
     GenericMtxElemMult(PDouble(@mtx[0, 0]), 3*sizeof(double), PDouble(@mtx[0, 0]), PDouble(@mtx[0, 0]), 2, 2, 3*sizeof(double), 3*sizeof(double));
     Check(SameValue(mtx[0, 0], 4) and SameValue(mtx[0, 1], 4), 'Error 2x2 elementwise mult odd LineSize failed');
     Check(SameValue(mtx[0, 3], 4) and SameValue(mtx[0, 4], 4), 'Error 2x2 elementwise mult odd LineSize failed');

     mtx[0, 0] := -2;
     mtx[0, 1] := -2;
     mtx[0, 2] := -2;
     mtx[0, 3] := -2;
     ASMMatrixElemMult(PDouble(@mtx[0, 0]), 2*sizeof(double), PDouble(@mtx[0, 0]), PDouble(@mtx[0, 0]), 2, 2, 2*sizeof(double), 2*sizeof(double));
     Check(SameValue(mtx[0, 0], 4) and SameValue(mtx[0, 1], 4), 'Error 2x2 elementwise asm mult even LineSize failed');
     Check(SameValue(mtx[0, 2], 4) and SameValue(mtx[0, 3], 4), 'Error 2x2 elementwise asm mult even LineSize failed');


     mtx[0, 0] := -2;
     mtx[0, 1] := -2;
     mtx[0, 3] := -2;
     mtx[0, 4] := -2;
     ASMMatrixElemMult(PDouble(@mtx[0, 0]), 3*sizeof(double), PDouble(@mtx[0, 0]), PDouble(@mtx[0, 0]), 2, 2, 3*sizeof(double), 3*sizeof(double));
     Check(SameValue(mtx[0, 0], 4) and SameValue(mtx[0, 1], 4), 'Error 2x2 elementwise mult failed');
     Check(SameValue(mtx[0, 3], 4) and SameValue(mtx[0, 4], 4), 'Error 2x2 elementwise mult failed');


     mtx[0, 0] := -2;
     mtx[0, 1] := -2;
     mtx[0, 2] := -2;
     mtx[0, 3] := -2;
     mtx[0, 4] := -2;
     mtx[0, 5] := -2;
     GenericMtxElemMult(PDouble(@mtx[0, 0]), 3*sizeof(double), PDouble(@mtx[0, 0]), PDouble(@mtx[0, 0]), 3, 2, 3*sizeof(double), 3*sizeof(double));
     Check(SameValue(mtx[0, 0], 4) and SameValue(mtx[0, 1], 4), 'Error 3x2 elementwise mult failed');
     Check(SameValue(mtx[0, 2], 4) and SameValue(mtx[0, 3], 4), 'Error 3x2 elementwise mult failed');
     Check(SameValue(mtx[0, 4], 4) and SameValue(mtx[0, 5], 4), 'Error 3x2 elementwise mult failed');

     mtx[0, 0] := -2;
     mtx[0, 1] := -2;
     mtx[0, 2] := -2;
     mtx[0, 3] := -2;
     mtx[0, 4] := -2;
     mtx[0, 5] := -2;
     GenericMtxElemMult(PDouble(@mtx[0, 0]), 2*sizeof(double), PDouble(@mtx[0, 0]), PDouble(@mtx[0, 0]), 2, 3, 2*sizeof(double), 2*sizeof(double));
     Check(SameValue(mtx[0, 0], 4) and SameValue(mtx[0, 1], 4), 'Error 2x3 elementwise mult failed');
     Check(SameValue(mtx[0, 2], 4) and SameValue(mtx[0, 3], 4), 'Error 2x3 elementwise mult failed');
     Check(SameValue(mtx[0, 4], 4) and SameValue(mtx[0, 5], 4), 'Error 2x3 elementwise mult failed');

     mtx[0, 0] := -2;
     mtx[0, 1] := -2;
     mtx[0, 2] := -2;
     mtx[0, 3] := -2;
     mtx[0, 4] := -2;
     mtx[0, 5] := -2;
     ASMMatrixElemMult(PDouble(@mtx[0, 0]), 3*sizeof(double), PDouble(@mtx[0, 0]), PDouble(@mtx[0, 0]), 3, 2, 3*sizeof(double), 3*sizeof(double));
     Check(SameValue(mtx[0, 0], 4) and SameValue(mtx[0, 1], 4), 'Error 3x2 elementwise asm mult failed');
     Check(SameValue(mtx[0, 2], 4) and SameValue(mtx[0, 3], 4), 'Error 3x2 elementwise asm mult failed');
     Check(SameValue(mtx[0, 4], 4) and SameValue(mtx[0, 5], 4), 'Error 3x2 elementwise asm mult failed');

     mtx[0, 0] := -2;
     mtx[0, 1] := -2;
     mtx[0, 2] := -2;
     mtx[0, 3] := -2;
     mtx[0, 4] := -2;
     mtx[0, 5] := -2;
     ASMMatrixElemMult(PDouble(@mtx[0, 0]), 2*sizeof(double), PDouble(@mtx[0, 0]), PDouble(@mtx[0, 0]), 2, 3, 2*sizeof(double), 2*sizeof(double));
     Check(SameValue(mtx[0, 0], 4) and SameValue(mtx[0, 1], 4), 'Error 2x3 elementwise asm mult failed');
     Check(SameValue(mtx[0, 2], 4) and SameValue(mtx[0, 3], 4), 'Error 2x3 elementwise asm mult failed');
     Check(SameValue(mtx[0, 4], 4) and SameValue(mtx[0, 5], 4), 'Error 2x3 elementwise asm mult failed');


     for x := 0 to 122 do
         for y := 0 to 123 do
             mtx[x, y] := -2;

     GenericMtxElemMult(PDouble(@mtx[0, 0]), 123*sizeof(double), PDouble(@mtx[0, 0]), PDouble(@mtx[0, 0]), 123, 124, 123*sizeof(double), 123*sizeof(double));

     for x := 0 to 122 do
         for y := 0 to 123 do
             Check(SameValue(mtx[x, y], 4), 'Error big elementwise mult failed @' + IntToStr(x) + ',' + IntToStr(y));

     for x := 0 to 122 do
         for y := 0 to 123 do
             mtx[x, y] := -2;

     ASMMatrixElemMult(PDouble(@mtx[0, 0]), 123*sizeof(double), PDouble(@mtx[0, 0]), PDouble(@mtx[0, 0]), 123, 124, 123*sizeof(double), 123*sizeof(double));

     for x := 0 to 122 do
         for y := 0 to 123 do
             Check(SameValue(mtx[x, y], 4), 'Error big elementwise asm mult failed @' + IntToStr(x) + ',' + IntToStr(y));
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
    m1 : Array[0..1] of double;
const cMtxWidth = 5010;
      cMtxHeight = 4983;
      cMtxSize = (cMtxWidth)*cMtxHeight;
      cMtxLinewidth = (cMtxWidth)*sizeof(double);
begin
     m1[0] := -2;
     m1[1] := -2;

     norm1 := GenericMtxElementwiseNorm2(PDouble(@m1[0]), sizeof(double), 1, 1);
     assert(SameValue(norm1, 2), 'Error elementwise norm failed for w=h=1');

     norm1 := ASMMatrixElementwiseNorm2(PDouble(@m1[0]), sizeof(double), 1, 1);
     assert(SameValue(norm1, 2), 'Error elementwise norm failed for w=h=1');

     norm1 := GenericMtxElementwiseNorm2(PDouble(@m1[0]), 2*sizeof(double), 2, 1);
     assert(SameValue(norm1, sqrt(8)), 'Error elementwise norm failed for w=2, h=1');

     norm1 := GenericMtxElementwiseNorm2(PDouble(@m1[0]), sizeof(double), 1, 2);
     assert(SameValue(norm1, sqrt(8)), 'Error elementwise norm failed for w=2, h=1');

     norm1 := ASMMatrixElementwiseNorm2(PDouble(@m1[0]), 2*sizeof(double), 2, 1);
     assert(SameValue(norm1, sqrt(8)), 'Error elementwise norm failed for w=2, h=1');

     norm1 := ASMMatrixElementwiseNorm2(PDouble(@m1[0]), sizeof(double), 1, 2);
     assert(SameValue(norm1, sqrt(8)), 'Error elementwise norm failed for w=2, h=1');

     randomize;
     FillMatrix(cMtxSize, x, y, xa, ya);

     startTime1 := MtxGetTime;
     norm1 := GenericMtxElementwiseNorm2(@x[0], cMtxLineWidth, cMtxWidth, cMtxheight);
     endTime1 := MtxGetTime;

     startTime2 := MtxGetTime;
     norm2 := ASMMatrixElementwiseNorm2(@x[0], cMtxLineWidth, cMtxWidth, cMtxheight);
     endTime2 := MtxGetTime;

     startTime3 := MtxGetTime;
     norm3 := ASMMatrixElementwiseNorm2(xa, cMtxLineWidth, cMtxWidth, cMtxheight);
     endTime3 := MtxGetTime;

     Status(Format('%.2f, %.2f, %.2f', [(endTime1 - startTime1)/mtxFreq*1000, (endTime2 - startTime2)/mtxFreq*1000,
                                             (endTime3 - startTime3)/mtxFreq*1000]));

     check(CompareValue(norm1, norm2, 1e-5) = 0, 'Norm with even width failed');
     check(CompareValue(norm1, norm3, 1e-5) = 0, 'Norm with even width (aligned) failed');

     FreeMem(xa);
     FreeMem(ya);

     FillMatrix((cmtxWidth + 1)*cMtxHeight, x, y, xa, ya);

     startTime1 := MtxGetTime;
     norm1 := GenericMtxElementwiseNorm2(@x[0], cMtxLineWidth + sizeof(double), cMtxWidth + 1, cMtxheight);
     endTime1 := MtxGetTime;

     startTime2 := MtxGetTime;
     norm2 := ASMMatrixElementwiseNorm2(@x[0], cMtxLineWidth  + sizeof(double), cMtxWidth + 1, cMtxheight);
     endTime2 := MtxGetTime;

     Status(Format('%.2f, %.2f', [(endTime1 - startTime1)/mtxFreq*1000, (endTime2 - startTime2)/mtxFreq*1000]));

     check(CompareValue(norm1, norm2, 1e-5) = 0, 'Norm with odd width failed');

     FreeMem(xa);
     FreeMem(ya);
end;


procedure sinxDivX(var x : double);
begin
     if SameValue(x, 0, 1e-7)
     then 
         x := 1
     else
         x := sin(x)/x;
end;

procedure TestMatrixOperations.TestApplyfunc;
var m1 : IMatrix;
    m2 : IMatrix;
    i : integer;
begin
     m1 := TDoubleMatrix.Create(100, 1); 
     for i := 0 to 99 do
         m1[i, 0] := -pi + pi/100;
     m2 := m1.ElementwiseFunc({$IFDEF FPC}@{$ENDIF}sinxDivX);
end;

procedure TestMatrixOperations.TestCopy;
const mt1 : Array[0..5] of double = (0, 1, 2, 3, 4, 5);
var res : TDoubleDynArray;
begin
     {$IFNDEF FPC}
     FailsOnMemoryLeak := True;
     {$ENDIF}
     res := GenericMtxCopy(mt1, 3, 2);

     CheckEqualsMem(@mt1, @res[0], sizeof(mt1), 'Error matrix copy: ' + #13#10 + WriteMtxDyn(res, 3));
end;

procedure TestMatrixOperations.TestMedian;
const mt1 : Array[0..5] of double = (0, 1, 2, 3, 4, 5);
var hlp : TDoubleDynArray;
begin
     SetLength(hlp, Length(mt1));
     GenericMtxMedian(@hlp[0], sizeof(double), @mt1[0], Length(mt1)*sizeof(double), length(mt1), 1, true);

     CheckEquals(2.5, hlp[0], 'Error median is not 2.5');
     GenericMtxMedian(@hlp[0], sizeof(double), @mt1[0], Length(mt1)*sizeof(double), length(mt1) - 1, 1, true);
     CheckEquals(2, hlp[0], 'Error median is not 2');

     GenericMtxMedian(@hlp[0], sizeof(double), @mt1[0], sizeof(double), 1, length(mt1), True);
     Check(CheckMtx(mt1, hlp), 'Median should be the input');
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

procedure TestMatrixOperations.TestRowExchange;
const mt1 : Array[0..7] of double = (0, 1, 2, 3, 4, 5, 6, 7);
      mt2 : Array[0..7] of double = (4, 5, 6, 7, 0, 1, 2, 3);
var dest : Array[0..7] of double;
    data, data1 : Pdouble;
    mem : PDouble;
begin
     move(mt1, dest, sizeof(dest));

     // test non aligned data
     ASMRowSwap(@dest[0], @dest[4], 3);
     CheckMtx(dest, mt2, 3, 2);

     move(mt1, dest, sizeof(dest));
     ASMRowSwap(@dest[0], @dest[4], 4);
     CheckMtx(dest, mt2, 4, 2);

     // test aligned data
     mem := GetMemory(sizeof(mt1) + 32);
     data := PDouble(NativeUInt(mem) + 16 - NativeUInt(mem) and $0F);
     data1 := data;
     inc(data1, 4);

     move(mt1, data^, sizeof(mt1));
     ASMRowSwap(data, data1, 3);
     move(data^, dest, sizeof(dest));
     CheckMtx(dest, mt2, 3, 2);

     move(mt1, data^, sizeof(mt1));
     ASMRowSwap(data, data1, 4);
     move(data^, dest, sizeof(dest));
     CheckMtx(dest, mt2, 4, 2);

     FreeMem(mem);
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

procedure TASMMatrixOperations.TestThreadedMatrixMultT1;
const cMtxWidth = 15;
      cMtxWidth2 = 8;
var a : Array[0..cMtxWidth*cMtxWidth - 1] of double;
    b : Array[0..cMtxWidth*cMtxWidth2 - 1] of double;
    counter: Integer;
    c1, c2, c3 : Array[0..cMtxWidth*cMtxWidth2-1] of double;
begin
     for counter := 0 to High(a) do
         a[counter] := counter;
     for counter := 0 to High(b) do
         b[counter] := counter;

     GenericTranspMtxMult(@c3[0], cMtxWidth2*sizeof(double), @a[0], @b[0], cMtxWidth, cMtxWidth, cMtxWidth2, cMtxWidth, cMtxWidth*sizeof(double), cMtxWidth2*sizeof(double));
     GenericBlockedMatrixMultiplicationT1(@c1[0], cMtxWidth2*sizeof(double), @a[0], @b[0], cMtxWidth, cMtxWidth, cMtxWidth2, cMtxWidth, cMtxWidth*sizeof(double), cMtxWidth2*sizeof(double), 4, doNone, nil);
     ThrMatrixMultT1Ex(@c2[0], cMtxWidth2*sizeof(double), @a[0], @b[0], cMtxWidth, cMtxWidth, cMtxWidth2, cMtxWidth, cMtxWidth*sizeof(double), cMtxWidth2*sizeof(double), 4, doNone, nil);

     Check(CheckMtx(c3, c1), 'GenericBlockedMatrixMultiplicationT1 failed');
     Check(CheckMtx(c3, c2), 'ThrMatrixMultT1Ex failed');
end;

procedure TASMMatrixOperations.TestThreadedMatrixMultT2;
const cMtxWidth = 24;
      cMtxWidth2 = 617;
var a : Array[0..cMtxWidth*cMtxWidth - 1] of double;
    b : Array[0..cMtxWidth*cMtxWidth2 - 1] of double;
    counter: Integer;
    c1, c2, c3 : Array[0..cMtxWidth*cMtxWidth2-1] of double;
begin
     for counter := 0 to High(a) do
         a[counter] := counter;
     for counter := 0 to High(b) do
         b[counter] := counter;

     GenericMtxMultTransp(@c3[0], cMtxWidth2*sizeof(double), @a[0], @b[0], cMtxWidth, cMtxWidth, cMtxWidth, cMtxWidth2, cMtxWidth*sizeof(double), cMtxWidth*sizeof(double));
     GenericBlockedMatrixMultiplicationT2(@c1[0], cMtxWidth2*sizeof(double), @a[0], @b[0], cMtxWidth, cMtxWidth, cMtxWidth, cMtxWidth2, cMtxWidth*sizeof(double), cMtxWidth*sizeof(double), 4, doNone, nil);
     ThrMatrixMultT2Ex(@c2[0], cMtxWidth2*sizeof(double), @a[0], @b[0], cMtxWidth, cMtxWidth, cMtxWidth, cMtxWidth2, cMtxWidth*sizeof(double), cMtxWidth*sizeof(double), 4, doNone, nil);

     Check(CheckMtx(c3, c1), 'GenericBlockedMatrixMultiplicationT1 failed');
     Check(CheckMtx(c3, c2), 'ThrMatrixMultT1Ex failed');
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

     startTime1 := MtxGetTime;
     GenericMtxAddAndScale(@dest1[0], cMtxLineWidth, cMtxWidth, cMtxheight, 5, 0.3);
     endTime1 := MtxGetTime;

     startTime2 := MtxGetTime;
     ThrMatrixAddAndScale(@x[0], cMtxLineWidth, cMtxWidth, cMtxheight, 5, 0.3);
     endTime2 := MtxGetTime;

     Status(Format('%.2f, %.2f', [(endTime1 - startTime1)/mtxFreq*1000, (endTime2 - startTime2)/mtxFreq*1000]));

     res := CheckMtxIdx(x, dest1, idx, cMtxWidth, cMtxHeight);
     if not res then
        Status(IntToStr(idx));
     check(res);

     startTime1 := MtxGetTime;
     GenericMtxScaleAndAdd(@dest1[0], cMtxLineWidth, cMtxWidth, cMtxheight, 5, 0.3);
     endTime1 := MtxGetTime;

     startTime2 := MtxGetTime;
     ThrMatrixScaleAndAdd(@x[0], cMtxLineWidth, cMtxWidth, cMtxheight, 5, 0.3);
     endTime2 := MtxGetTime;

     Status(Format('%.2f, %.2f', [(endTime1 - startTime1)/mtxFreq*1000, (endTime2 - startTime2)/mtxFreq*1000]));

     FreeMem(xa);
     FreeMem(ya);

     res := CheckMtxIdx(x, dest1, idx, cMtxWidth, cMtxHeight);
     if not res then
        Status(IntToStr(idx));
     check(res);
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
const cMtxWidth = 1000;
      cMtxHeight = 2047;
      cMtxSize = (cMtxWidth)*cMtxHeight;
      cMtxLinewidth = (cMtxWidth)*sizeof(double);
begin
     randomize;
     FillMatrix(cMtxSize, x, y, xa, ya);
     SetLength(dest1, cMtxSize);
     SetLength(dest2, cMtxSize);
     dest2a := AllocMem(cMtxSize*sizeof(double));

     startTime1 := MtxGetTime;
     ASMMatrixAdd(@dest1[0], cMtxLineWidth, @x[0], @y[0], cMtxWidth, cMtxheight, cMtxLinewidth, cMtxLinewidth);
     endTime1 := MtxGetTime;

     startTime2 := MtxGetTime;
     ThrMatrixAdd(@dest2[0], cMtxLineWidth, @x[0], @y[0], cMtxWidth, cMtxheight, cMtxLinewidth, cMtxLinewidth);
     endTime2 := MtxGetTime;

     startTime3 := MtxGetTime;
     ThrMatrixAdd(dest2a, cMtxLineWidth, xa, ya, cMtxWidth, cMtxheight, cMtxLinewidth, cMtxLinewidth);
     endTime3 := MtxGetTime;

     Status(Format('%.2f, %.2f, %.2f', [(endTime1 - startTime1)/mtxFreq*1000, (endTime2 - startTime2)/mtxFreq*1000,
                 (endTime3 - startTime3)/mtxFreq*1000]));

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
      cMtxWidth = 2543;
      cMtxHeight = 1560;
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

     startTime1 := MtxGetTime;
     BlockedMatrixMultiplication(@dest1[0], cMtxLineWidth2, @x[0], @y[0], cMtxWidth, cMtxheight, cMtxHeight, cMtxWidth, cMtxLinewidth, cMtxLinewidth2);
     endTime1 := MtxGetTime;

     startTime2 := MtxGetTime;
     ThrMatrixMult(dest1a, cMtxLineWidth2, xa, ya, cMtxWidth, cMtxheight, cMtxHeight, cMtxWidth, cMtxLinewidth, cMtxLinewidth2);
     endTime2 := MtxGetTime;

     startTime3 := MtxGetTime;
     ThrMatrixMultDirect(dest2a, cMtxLineWidth2, xa, ya, cMtxWidth, cMtxheight, cMtxHeight, cMtxWidth, cMtxLinewidth, cMtxLinewidth2);
     endTime3 := MtxGetTime;

     Status(Format('%.2f, %.2f, %.2f', [(endTime1 - startTime1)/mtxFreq*1000, (endTime2 - startTime2)/mtxFreq*1000, (endTime3 - startTime3)/mtxFreq*1000]));

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
     vecstartTime3 := MtxGetTime;
     ASMMtxVecMult(@dest1[0], sizeof(double), @x[0], @y[0], 1024*128*sizeof(double), sizeof(double), 1024*128, 8*20, 1, 0);
     vecendTime3 := MtxGetTime;

     vecstartTime1 := MtxGetTime;
     ThrMatrixVecMult(dest2a, sizeof(double), xa, ya, 1024*128, 8*20, 128*1024*sizeof(double), sizeof(double), 1, 0);
     vecendTime1 := MtxGetTime;

     vecstartTime2 := MtxGetTime;
     ThrMatrixVecMult(@dest2[0], sizeof(double), @x[0], @y[0], 1024*128, 8*20, 128*1024*sizeof(double), sizeof(double), 1, 0);
     vecendTime2 := MtxGetTime;

     Status(Format('%.2f, %.2f, %.2f',
         [(vecendTime1 - vecstartTime1)/mtxFreq*1000, (vecendTime2 - vecstartTime2)/mtxFreq*1000,
          (vecendTime3 - vecstartTime3)/mtxFreq*1000]));

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
    mem : PDouble;
begin
     mem := AllocMem(3*16*sizeof(double) + 16);
     m1 := PDouble(TASMNativeUInt(mem) + 16 - TASMNativeUInt(mem) and $0F);
     m2 := m1;
     inc(m2, 16);
     dest := m2;
     inc(dest, 16);

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

     FreeMem(mem);
end;


procedure TASMMatrixOperations.TestTransposedASMEvenWOddH;
const mt1 : Array[0..5] of double = (0, 1, 2, 3, 4, 5);
      mt2 : Array[0..5] of double = (0, 2, 4, 1, 3, 5);
var dest : Array[0..5] of double;
begin
     ASMMatrixTransposeUnAlignedEvenWOddH(@dest[0], 3*Sizeof(double), @mt1[0], 2*sizeof(double), 2, 3);
     CheckEqualsMem(@mt2, @dest[0], sizeof(mt2), 'Error matrix transpose: ');
end;

procedure TASMMatrixOperations.TestVarianceCol;
const cMT1Len : integer = 4;
      cMt1height : integer = 3;
      mt1 : Array[0..11] of double = (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11);
      cBigMtxSize = 522;
var ptr : Pointer;
    pMem : PDouble;
    pDest, pDest2 : PConstDoubleArr;
    mt1T : Array[0..11] of double;
    mt1Dyn : Array of double;
    x, y : TDoubleDynArray;
    p1, p2 : PDouble;
begin
     ASMMatrixTranspose(@mt1T[0], 3*sizeof(double), @mt1[0], cMT1Len*sizeof(double), 4, 3);

     SetLength(mt1Dyn, Length(mt1));
     Move(mt1T[0], mt1Dyn[0], sizeof(mt1));
     ptr := GetMemory(sizeof(mt1) + 32 + 8*sizeof(double));
     pMem := PDouble( NativeUInt(ptr) + 16 - NativeUInt(ptr) and $0F );

     pDest := PConstDoubleArr(pMem);
     inc(PDouble(pDest), Length(mt1));

     GenericMtxVar(PDouble(pDest), 4*sizeof(double), @mt1[0], cMT1Len*sizeof(double), cMT1Len, cMt1height, False, True);
     Check( SameValue( Variance(Copy(mt1Dyn, 0, cMt1height)), pDest^[0], 1e-6), 'Error calculating variance.');
     Check( SameValue( Variance(Copy(mt1Dyn, cMt1height, cMt1height)), pDest^[1], 1e-6), 'Error calculating variance.');
     Check( SameValue( Variance(Copy(mt1Dyn, 2*cMt1height, cMt1height)), pDest^[2], 1e-6), 'Error calculating variance.');
     Check( SameValue( Variance(Copy(mt1Dyn, 3*cMt1height, cMt1height)), pDest^[3], 1e-6), 'Error calculating variance.');
     
     FillChar(pDest^, 4*sizeof(double), 0);
     Move(mt1, pMem^, sizeof(mt1));
     ASMMatrixVarColumnAlignedEvenW(PDouble(pDest), 4*sizeof(double), pMem, cMT1Len*sizeof(double), cMT1Len, cMt1height, True);
     Check( SameValue( Variance(Copy(mt1Dyn, 0, cMt1height)), pDest^[0], 1e-6), 'Error calculating variance.');
     Check( SameValue( Variance(Copy(mt1Dyn, cMt1height, cMt1height)), pDest^[1], 1e-6), 'Error calculating variance.');
     Check( SameValue( Variance(Copy(mt1Dyn, 2*cMt1height, cMt1height)), pDest^[2], 1e-6), 'Error calculating variance.');
     Check( SameValue( Variance(Copy(mt1Dyn, 3*cMt1height, cMt1height)), pDest^[3], 1e-6), 'Error calculating variance.');
     
     FillChar(pDest^, 4*sizeof(double), 0);
     ASMMatrixVarColumnUnAlignedEvenW(PDouble(pDest), 4*sizeof(double), @mt1[0], cMT1Len*sizeof(double), cMT1Len, cMt1height, True); 
     Check( SameValue( Variance(Copy(mt1Dyn, 0, cMt1height)), pDest^[0], 1e-6), 'Error calculating variance.');
     Check( SameValue( Variance(Copy(mt1Dyn, cMt1height, cMt1height)), pDest^[1], 1e-6), 'Error calculating variance.');
     Check( SameValue( Variance(Copy(mt1Dyn, 2*cMt1height, cMt1height)), pDest^[2], 1e-6), 'Error calculating variance.');
     Check( SameValue( Variance(Copy(mt1Dyn, 3*cMt1height, cMt1height)), pDest^[3], 1e-6), 'Error calculating variance.');

     FillChar(pDest^, 4*sizeof(double), 0);
     Move(mt1, pMem^, sizeof(mt1));
     ASMMatrixVarColumnAlignedOddW(PDouble(pDest), 4*sizeof(double), pMem, cMT1Len*sizeof(double), cMT1Len - 1, cMt1height, True);
     Check( SameValue( Variance(Copy(mt1Dyn, 0, cMt1height)), pDest^[0], 1e-6), 'Error calculating variance.');
     Check( SameValue( Variance(Copy(mt1Dyn, cMt1height, cMt1height)), pDest^[1], 1e-6), 'Error calculating variance.');
     Check( SameValue( Variance(Copy(mt1Dyn, 2*cMt1height, cMt1height)), pDest^[2], 1e-6), 'Error calculating variance.');
     
     FillChar(pDest^, 4*sizeof(double), 0);
     ASMMatrixVarColumnUnAlignedOddW(PDouble(pDest), 4*sizeof(double), @mt1[0], cMT1Len*sizeof(double), cMT1Len - 1, 3, True); 
     Check( SameValue( Variance(Copy(mt1Dyn, 0, cMt1height)), pDest^[0], 1e-6), 'Error calculating variance.');
     Check( SameValue( Variance(Copy(mt1Dyn, cMt1height, cMt1height)), pDest^[1], 1e-6), 'Error calculating variance.');
     Check( SameValue( Variance(Copy(mt1Dyn, 2*cMt1height, cMt1height)), pDest^[2], 1e-6), 'Error calculating variance.');
     
     FreeMem(ptr);

     // ###########################################
     // #### Big matrix analysis
     ptr := GetMemory(32 + 2*2*cBigMtxSize*sizeof(double));
     pDest := PConstDoubleArr( NativeUInt(ptr) + 16 - NativeUInt(ptr) and $0F );
     pDest2 := PConstDoubleArr( NativeUInt(pDest) + 2*cBigMtxSize*sizeof(double));

     FillChar(pDest^, 2*cBigMtxSize*sizeof(double), 0);
     FillChar(pDest2^, 2*cBigMtxSize*sizeof(double), 0);
     FillMatrix(cBigMtxSize*cBigMtxSize, x, y, p1, p2);

     GenericMtxVar(PDouble(pDest2), cBigMtxSize*sizeof(double), @x[0], cBigMtxSize*sizeof(double), cBigMtxSize, cBigMtxSize, False, True);
     ASMMatrixVar(PDouble(pDest), cBigMtxSize*sizeof(double), @x[0], cBigMtxSize*sizeof(double), cBigMtxSize, cBigMtxSize, False, True);
     Check(CheckMtx(Slice(pDest2^, cBigMtxSize), Slice(pDest^, cBigMtxSize) ), 'Variance big matrix failed');

     FillChar(pDest^, 2*cBigMtxSize*sizeof(double), 0);
     ASMMatrixVar(PDouble(pDest), cBigMtxSize*sizeof(double), p1, cBigMtxSize*sizeof(double), cBigMtxSize, cBigMtxSize, False, True);
     Check(CheckMtx(Slice(pDest2^, cBigMtxSize), Slice(pDest^, cBigMtxSize) ), 'Variance big matrix failed');
     
     FillChar(pDest^, 2*cBigMtxSize*sizeof(double), 0);
     FillChar(pDest2^, 2*cBigMtxSize*sizeof(double), 0);
     GenericMtxVar(PDouble(pDest2), cBigMtxSize*sizeof(double), @x[0], cBigMtxSize*sizeof(double), cBigMtxSize - 1, cBigMtxSize, False, True);
     ASMMatrixVar(PDouble(pDest), cBigMtxSize*sizeof(double), @x[0], cBigMtxSize*sizeof(double), cBigMtxSize - 1, cBigMtxSize, False, True);
     Check(CheckMtx(Slice(pDest2^, cBigMtxSize - 1), Slice(pDest^, cBigMtxSize - 1) ), 'Variance big matrix failed');

     FillChar(pDest^, 2*cBigMtxSize*sizeof(double), 0);
     ASMMatrixVar(PDouble(pDest), cBigMtxSize*sizeof(double), p1, cBigMtxSize*sizeof(double), cBigMtxSize - 1, cBigMtxSize, False, True);
     Check(CheckMtx(Slice(pDest2^, cBigMtxSize - 1), Slice(pDest^, cBigMtxSize - 1) ), 'Variance big matrix failed');
     
     FreeMem(p1);
     FreeMem(p2);
     
     FreeMem(ptr);
end;

procedure TASMMatrixOperations.TestVarianceRow;
const cMT1Len : integer = 6;
      mt1 : Array[0..11] of double = (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11);
      cBigMtxSize = 522;
var ptr : Pointer;
    pMem : PDouble;
    pDest, pDest2 : PConstDoubleArr;
    mt1Dyn : Array of double;
    x, y : TDoubleDynArray;
    p1, p2 : PDouble;
begin
     SetLength(mt1Dyn, Length(mt1));
     Move(mt1[0], mt1Dyn[0], sizeof(mt1));
     ptr := GetMemory(sizeof(mt1) + 32 + 8*sizeof(double));
     pMem := PDouble( NativeUInt(ptr) + 16 - NativeUInt(ptr) and $0F );

     pDest := PConstDoubleArr(pMem);
     inc(PDouble(pDest), Length(mt1));

     GenericMtxVar(PDouble(pDest), sizeof(double), @mt1[0], cMT1Len*sizeof(double), cMT1Len, 2, True, True);
     Check( SameValue( Variance(Copy(mt1Dyn, 0, cMT1Len)), pDest^[0], 1e-6), 'Error calculating variance.');
     Check( SameValue( Variance(Copy(mt1Dyn, cMT1Len, cMT1Len)), pDest^[1], 1e-6), 'Error calculating variance.');
     
     FillChar(pDest^, 4*sizeof(double), 0);
     Move(mt1, pMem^, sizeof(mt1));
     ASMMatrixVarRowAlignedEvenW(PDouble(pDest), 2*sizeof(double), pMem, cMT1Len*sizeof(double), cMT1Len, 2, True);
     Check( SameValue( Variance(Copy(mt1Dyn, 0, cMT1Len)), pDest^[0], 1e-6), 'Error calculating variance.');
     Check( SameValue( Variance(Copy(mt1Dyn, cMT1Len, cMT1Len)), pDest^[2], 1e-6), 'Error calculating variance.');
     
     FillChar(pDest^, 4*sizeof(double), 0);
     ASMMatrixVarRowUnAlignedEvenW(PDouble(pDest), sizeof(double), @mt1[0], cMT1Len*sizeof(double), cMT1Len, 2, True); 
     Check( SameValue( Variance(Copy(mt1Dyn, 0, cMT1Len)), pDest^[0], 1e-6), 'Error calculating variance.');
     Check( SameValue( Variance(Copy(mt1Dyn, cMT1Len, cMT1Len)), pDest^[1], 1e-6), 'Error calculating variance.');

     FillChar(pDest^, 4*sizeof(double), 0);
     Move(mt1, pMem^, sizeof(mt1));
     ASMMatrixVarRowAlignedOddW(PDouble(pDest), 2*sizeof(double), pMem, cMT1Len*sizeof(double), cMT1Len - 1, 2, True);
     Check( SameValue( Variance(Copy(mt1Dyn, 0, cMT1Len - 1)), pDest^[0], 1e-6), 'Error calculating variance.');
     Check( SameValue( Variance(Copy(mt1Dyn, cMT1Len, cMT1Len - 1)), pDest^[2], 1e-6), 'Error calculating variance.');
     
     FillChar(pDest^, 4*sizeof(double), 0);
     ASMMatrixVarRowUnAlignedOddW(PDouble(pDest), sizeof(double), @mt1[0], cMT1Len*sizeof(double), cMT1Len - 1, 2, True); 
     Check( SameValue( Variance(Copy(mt1Dyn, 0, cMT1Len - 1)), pDest^[0], 1e-6), 'Error calculating variance.');
     Check( SameValue( Variance(Copy(mt1Dyn, cMT1Len, cMT1Len - 1)), pDest^[1], 1e-6), 'Error calculating variance.');
     
     FreeMem(ptr);

     // ###########################################
     // #### Big matrix analysis
     ptr := GetMemory(32 + 2*2*cBigMtxSize*sizeof(double));
     pDest := PConstDoubleArr( NativeUInt(ptr) + 16 - NativeUInt(ptr) and $0F );
     pDest2 := PConstDoubleArr( NativeUInt(pDest) + 2*cBigMtxSize*sizeof(double));

     FillChar(pDest^, 2*cBigMtxSize*sizeof(double), 0);
     FillChar(pDest2^, 2*cBigMtxSize*sizeof(double), 0);
     FillMatrix(cBigMtxSize*cBigMtxSize, x, y, p1, p2);

     GenericMtxVar(PDouble(pDest2), 2*sizeof(double), @x[0], cBigMtxSize*sizeof(double), cBigMtxSize, cBigMtxSize, True, True);
     ASMMatrixVar(PDouble(pDest), 2*sizeof(double), @x[0], cBigMtxSize*sizeof(double), cBigMtxSize, cBigMtxSize, True, True);
     Check(CheckMtx(Slice(pDest2^, 2*cBigMtxSize), Slice(pDest^, 2*cBigMtxSize) ), 'Variance big matrix failed');

     FillChar(pDest^, 2*cBigMtxSize*sizeof(double), 0);
     ASMMatrixVar(PDouble(pDest), 2*sizeof(double), p1, cBigMtxSize*sizeof(double), cBigMtxSize, cBigMtxSize, True, True);
     Check(CheckMtx(Slice(pDest2^, 2*cBigMtxSize), Slice(pDest^, 2*cBigMtxSize) ), 'Variance big matrix failed');
     
     FillChar(pDest^, 2*cBigMtxSize*sizeof(double), 0);
     FillChar(pDest2^, 2*cBigMtxSize*sizeof(double), 0);
     GenericMtxVar(PDouble(pDest2), 2*sizeof(double), @x[0], cBigMtxSize*sizeof(double), cBigMtxSize - 1, cBigMtxSize, True, True);
     ASMMatrixVar(PDouble(pDest), 2*sizeof(double), @x[0], cBigMtxSize*sizeof(double), cBigMtxSize - 1, cBigMtxSize, True, True);
     Check(CheckMtx(Slice(pDest2^, 2*cBigMtxSize), Slice(pDest^, 2*cBigMtxSize) ), 'Variance big matrix failed');

     FillChar(pDest^, 2*cBigMtxSize*sizeof(double), 0);
     ASMMatrixVar(PDouble(pDest), 2*sizeof(double), p1, cBigMtxSize*sizeof(double), cBigMtxSize - 1, cBigMtxSize, True, True);
     Check(CheckMtx(Slice(pDest2^, 2*cBigMtxSize), Slice(pDest^, 2*cBigMtxSize) ), 'Variance big matrix failed');
     
     FreeMem(p1);
     FreeMem(p2);
     
     FreeMem(ptr);
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
     //SetThreadAffinityMask(GetCurrentThread, 1);
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

     startTime1 := MtxGetTime;
     GenericMtxCopy(dest1a, cMtxLineWidth, xa, cMtxLineWidth, cMtxWidth, cMtxheight);
     endTime1 := MtxGetTime;

     startTime2 := MtxGetTime;
     ASMMatrixCopy(dest2a, cMtxLineWidth, xa, cMtxLinewidth, cMtxWidth, cMtxheight);
     endTime2 := MtxGetTime;

     Move(dest1a^, dest1[0], length(dest2)*sizeof(double));
     Move(dest2a^, dest2[0], length(dest2)*sizeof(double));
     res := CheckMtxIdx(dest1, dest2, idx);
     if not res then
        Status(IntToStr(idx));
     check(res);

     startTime3 := MtxGetTime;
     ASMMatrixCopy(@dest2[0], cMtxLineWidth, @x[0], cMtxLinewidth, cMtxWidth, cMtxheight);
     endTime3 := MtxGetTime;

     Status(Format('%.2f, %.2f, %.2f', [(endTime1 - startTime1)/mtxFreq*1000, (endTime2 - startTime2)/mtxFreq*1000,
                        (endTime3 - startTime3)/mtxFreq*1000]));

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
     //SetThreadAffinityMask(GetCurrentThread, 1);
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

     startTime1 := MtxGetTime;
     dest1 := GenericMtxMax(@x[0], cMtxWidth, cMtxheight, cMtxLineWidth);
     dest2 := GenericMtxMin(@x[0], cMtxWidth, cMtxheight, cMtxLineWidth);
     endTime1 := MtxGetTime;

     startTime2 := MtxGetTime;
     dest3 := ASMMatrixMax(@x[0], cMtxWidth, cMtxheight, cMtxLineWidth);
     dest4 := ASMMatrixMin(@x[0], cMtxWidth, cMtxheight, cMtxLineWidth);
     endTime2 := MtxGetTime;

     check(dest1 = dest3, 'Max error');
     check(dest2 = dest4, 'Min error');

     startTime3 := MtxGetTime;
     dest3 := ASMMatrixMax(xa, cMtxWidth, cMtxheight, cMtxLineWidth);
     dest4 := ASMMatrixMin(xa, cMtxWidth, cMtxheight, cMtxLineWidth);
     endTime3 := MtxGetTime;

     check(dest1 = dest3, 'Max error');
     check(dest2 = dest4, 'Min error');

     Status(Format('%.2f, %.2f, %.2f', [(endTime1 - startTime1)/mtxFreq*1000, (endTime2 - startTime2)/mtxFreq*1000,
                        (endTime3 - startTime3)/mtxFreq*1000]));

     freeMem(xa);
end;


procedure TASMMatrixOperations.TestMatrixMultTria2T1;
const mt1 : Array[0..15] of double = (-4, 1, 2, -2, 3, 4, 5, 2, 6, 7, -1, 2, -3, 1, -1, -1);
var res1 : Array[0..15] of double;
    res2 : Array[0..15] of double;
begin
     // test matrix multiplication of mt1'*mt1 where the second mt1 is handled as
     // lower triangular matrix with ones in the diagonaly
     // test is always against the reference implementation of the generic version ;)
     GenericMtxMultTria2T1Lower(@res1[0], 4*sizeof(double), @mt1[0], 4*sizeof(double), @mt1[0], 4*sizeof(double), 4, 4, 4, 4);
     GenericMtxMultTria2T1_2(@res2[0], 4*sizeof(double), @mt1[0], 4*sizeof(double), @mt1[0], 4*sizeof(double), 4, 4, 4, 4);

     Check(CheckMtx(res2, res1), 'Error generic MultTria2T1 failed 1');
     ASMMtxMultTria2T1(@res2[0], 4*sizeof(double), @mt1[0], 4*sizeof(double), @mt1[0], 4*sizeof(double), 4, 4, 4, 4);
     Check(CheckMtx(res2, res1), 'Error asm MultTria2T1 failed 1');

     GenericMtxMultTria2T1Lower(@res1[0], 4*sizeof(double), @mt1[0], 4*sizeof(double), @mt1[0], 4*sizeof(double), 3, 3, 3, 3);
     GenericMtxMultTria2T1_2(@res2[0], 4*sizeof(double), @mt1[0], 4*sizeof(double), @mt1[0], 4*sizeof(double), 3, 3, 3, 3);

     Check(CheckMtx(res2, res1, 3, 3), 'Error generic MultTria2T1 failed 2');
     ASMMtxMultTria2T1(@res2[0], 4*sizeof(double), @mt1[0], 4*sizeof(double), @mt1[0], 4*sizeof(double), 3, 3, 3, 3);
     Check(CheckMtx(res2, res1, 3, 3), 'Error asm MultTria2T1 failed 2');
end;

procedure TASMMatrixOperations.TestMinMaxASM;
const mt1 : Array[0..15] of double = (0, 1, 2, -2, 3, 4, 5, 0, 6, 7, -1, 0, 0, 0, 0, 0);
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
     //SetThreadAffinityMask(GetCurrentThread, 1);
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

     startTime1 := MtxGetTime;
     GenericMtxNormalize(dest1, x, cMtxWidth, cMtxheight, True);
     endTime1 := MtxGetTime;

     startTime2 := MtxGetTime;
     ASMMatrixNormalize(@dest2[0], cMtxLinewidth, @x[0], cMtxLinewidth, cMtxWidth, cMtxheight, True);
     endTime2 := MtxGetTime;

     startTime3 := MtxGetTime;
     ASMMatrixNormalize(dest1a, cMtxLinewidth, xa, cMtxLinewidth, cMtxWidth, cMtxheight, True);
     endTime3 := MtxGetTime;

     Status(Format('%.2f, %.2f, %.2f', [(endTime1 - startTime1)/mtxFreq*1000, (endTime2 - startTime2)/mtxFreq*1000,
                        (endTime3 - startTime3)/mtxFreq*1000]));

     Check(checkMtx(dest1, dest2), 'Error row wise Matrix normalize');
     Move(dest1a^, dest2[0], cMtxSize*sizeof(double));
     Check(checkMtx(dest1, dest2), 'Error row wise aligned Matrix normalize');

     startTime1 := MtxGetTime;
     GenericMtxNormalize(dest1, x, cMtxWidth, cMtxheight, False);
     endTime1 := MtxGetTime;

     startTime2 := MtxGetTime;
     ASMMatrixNormalize(@dest2[0], cMtxLinewidth, @x[0], cMtxLinewidth, cMtxWidth, cMtxheight, False);
     endTime2 := MtxGetTime;

     startTime3 := MtxGetTime;
     ASMMatrixNormalize(dest1a, cMtxLinewidth, xa, cMtxLinewidth, cMtxWidth, cMtxheight, False);
     endTime3 := MtxGetTime;


     Status(Format('%.2f, %.2f, %.2f', [(endTime1 - startTime1)/mtxFreq*1000, (endTime2 - startTime2)/mtxFreq*1000,
                        (endTime3 - startTime3)/mtxFreq*1000]));

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

					startTime1 := MtxGetTime;
     GenericMtxMean(@destGen[0], sizeof(double), m, cMtxLineWidth, cMtxWidth, cMtxheight, True);
     endTime1 := MtxGetTime;

     startTime2 := MtxGetTime;
     ASMMatrixMeanRowUnAlignedEvenW(dest, 2*sizeof(double), m, cMtxLineWidth, cMtxWidth, cMtxheight);
     endTime2 := MtxGetTime;

     pDest := dest;
     for i := 0 to cMtxHeight - 1 do
     begin
          destDyn[i] := pdest^;
          inc(pdest, 2);
     end;

     Check(CheckMtx(destGen, DestDyn), 'Error Unaligned Mean row operation');

     startTime3 := MtxGetTime;
     ASMMatrixMeanRowAlignedEvenW(dest, 2*sizeof(double), m, cMtxLineWidth, cMtxWidth, cMtxheight);
     endTime3 := MtxGetTime;

     pDest := dest;
     for i := 0 to cMtxHeight - 1 do
     begin
          destDyn[i] := pdest^;
          inc(pdest, 2);
     end;

     Check(CheckMtx(destGen, DestDyn), 'Error aligned Mean row operation');

     Status(Format('%.2f, %.2f, %.2f', [(endTime1 - startTime1)/mtxFreq*1000, (endTime2 - startTime2)/mtxFreq*1000,
                        (endTime3 - startTime3)/mtxFreq*1000]));

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
     blk := AllocMem((cMtxSize*2)*sizeof(double) + 48);
     m := blk;
     inc(PByte(m), 16 - TASMNativeInt(m) and $F);
     dest := m;
     inc(dest, cMtxSize + cMtxSize mod 2);

     SetLength(DestDyn, cMtxSize);
     SetLength(DestGen, cMtxSize);

     pM := m;
     for i := 0 to cMtxSize - 1 do
     begin
     	    pM^ := i;
          inc(pM);
     end;

					startTime1 := MtxGetTime;
     GenericMtxMean(@destGen[0], cMtxWidth*sizeof(double), m, cMtxLineWidth, cMtxWidth, cMtxheight, False);
     endTime1 := MtxGetTime;

     startTime2 := MtxGetTime;
     ASMMatrixMeanColumnUnAlignedEvenW(dest, cMtxWidth*sizeof(double), m, cMtxLineWidth, cMtxWidth, cMtxheight);
     endTime2 := MtxGetTime;

     pDest := dest;
     for i := 0 to cMtxSize - 1 do
     begin
          destDyn[i] := pdest^;
          inc(pdest);
     end;

     Check(CheckMtx(destGen, DestDyn), 'Error Unaligned sum operation');

     startTime3 := MtxGetTime;
     ASMMatrixMeanColumnAlignedEvenW(dest, cMtxWidth*sizeof(double), m, cMtxLineWidth, cMtxWidth, cMtxheight);
     endTime3 := MtxGetTime;

     pDest := dest;
     for i := 0 to cMtxSize - 1 do
     begin
          destDyn[i] := pdest^;
          inc(pdest);
     end;

     Check(CheckMtx(destGen, DestDyn), 'Error Aligned sum operation');

     Status(Format('%.2f, %.2f, %.2f', [(endTime1 - startTime1)/mtxFreq*1000, (endTime2 - startTime2)/mtxFreq*1000,
                        (endTime3 - startTime3)/mtxFreq*1000]));

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
     //SetThreadAffinityMask(GetCurrentThread, 1);

     blk := AllocMem((16 + 8)*sizeof(double) + 16);
     m := blk;
     inc(PByte(m), 16 - TASMNativeInt(blk) and $F);
     dest := m;
     inc(dest, 16);

     Move(mt1, m^, 16*sizeof(double));

     GenericMtxSum(@dest1[0], sizeof(double), @mt1[0], 4*sizeof(double), 4, 4, True);
     ASMMatrixSumRowUnAlignedEvenW(@dest2[0], sizeof(double), @mt1[0], 4*sizeof(double), 4, 4);
     ASMMatrixSumRowAlignedEvenW(dest, 2*sizeof(double), m, 4*sizeof(double), 4, 4);

     Check(checkMtx(dest1, dest2), 'Error row wise Matrix sum');
     pDest := dest;
     for i := 0 to High(dest1) - 1 do
     begin
          dest2[i] := pDest^;
          inc(pDest, 2);
     end;
     Check(checkMtx(dest1, dest2), 'Error row wise aligned Matrix sum');

     GenericMtxSum(@dest1[0], sizeof(double), @mt1[0], 4*sizeof(double), 3, 4, True);
     ASMMatrixSumRowUnAlignedOddW(@dest2[0], sizeof(double), @mt1[0], 4*sizeof(double), 3, 4);
     ASMMatrixSumRowAlignedOddW(dest, 2*sizeof(double), m, 4*sizeof(double), 3, 4);

     Check(checkMtx(dest1, dest2), 'Error row wise Matrix sum');
     for i := 0 to High(dest1) - 1 do
     begin
          dest2[i] := PConstDoubleArr(dest)^[2*i];
     end;
     Check(checkMtx(dest1, dest2), 'Error row wise aligned Matrix sum');

     FreeMem(blk);

     // big row test
     blk := AllocMem((cMtxSize + 2*cMtxHeight)*sizeof(double) + 48);
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

					startTime1 := MtxGetTime;
     GenericMtxSum(@destGen[0], sizeof(double), m, cMtxLineWidth, cMtxWidth, cMtxheight, True);
     endTime1 := MtxGetTime;

     startTime2 := MtxGetTime;
     ASMMatrixSumRowUnAlignedEvenW(dest, 2*sizeof(double), m, cMtxLineWidth, cMtxWidth, cMtxheight);
     endTime2 := MtxGetTime;

     pDest := dest;
     for i := 0 to cMtxHeight - 1 do
     begin
          destDyn[i] := pdest^;
          inc(pdest, 2);
     end;

     Check(CheckMtx(destGen, DestDyn), 'Error Unaligned sum operation');

     startTime3 := MtxGetTime;
     ASMMatrixSumRowAlignedEvenW(dest, 2*sizeof(double), m, cMtxLineWidth, cMtxWidth, cMtxheight);
     endTime3 := MtxGetTime;

     pDest := dest;
     for i := 0 to cMtxHeight - 1 do
     begin
          destDyn[i] := pdest^;
          inc(pdest, 2);
     end;

     Check(CheckMtx(destGen, DestDyn), 'Error Aligned sum operation');

     Status(Format('%.2f, %.2f, %.2f', [(endTime1 - startTime1)/mtxFreq*1000, (endTime2 - startTime2)/mtxFreq*1000,
                        (endTime3 - startTime3)/mtxFreq*1000]));

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
     blk := AllocMem((cMtxSize+ cMtxHeight*2)*sizeof(double) + 48);
     m := blk;
     inc(PByte(m), 16 - TASMNativeInt(blk) and $F);
     dest := m;
     inc(dest, cMtxSize + cMtxSize mod 2);

     SetLength(DestDyn, cMtxSize);
     SetLength(DestGen, cMtxSize);

     pM := m;
     for i := 0 to cMtxSize - 1 do
     begin
     	    pM^ := i;
          inc(pM);
     end;

					startTime1 := MtxGetTime;
     GenericMtxSum(@destGen[0], cMtxWidth*sizeof(double), m, cMtxLineWidth, cMtxWidth, cMtxheight, False);
     endTime1 := MtxGetTime;

     startTime2 := MtxGetTime;
     ASMMatrixSumColumnUnAlignedEvenW(dest, cMtxWidth*sizeof(double), m, cMtxLineWidth, cMtxWidth, cMtxheight);
     endTime2 := MtxGetTime;

     pDest := dest;
     for i := 0 to cMtxWidth - 1 do
     begin
          destDyn[i] := pdest^;
          inc(pdest);
     end;

     Check(CheckMtx(destGen, DestDyn), 'Error Unaligned sum operation');

     startTime3 := MtxGetTime;
     ASMMatrixSumColumnAlignedEvenW(dest, cMtxWidth*sizeof(double), m, cMtxLineWidth, cMtxWidth, cMtxheight);
     endTime3 := MtxGetTime;

     pDest := dest;
     for i := 0 to cMtxWidth - 1 do
     begin
          destDyn[i] := pdest^;
          inc(pdest);
     end;

     Check(CheckMtx(destGen, DestDyn), 'Error Aligned sum operation');

     Status(Format('%.2f, %.2f, %.2f', [(endTime1 - startTime1)/mtxFreq*1000, (endTime2 - startTime2)/mtxFreq*1000,
                        (endTime3 - startTime3)/mtxFreq*1000]));

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
          x[i] := random;
          y[i] := random;
     end;

     SetLength(dest1, cStrassenHeight*cStrassenHeight);
     SetLength(dest2, cStrassenHeight*cStrassenHeight);

     TryClearCache;
     startTime1 := MtxGetTime;
     BlockedMatrixMultiplication(@dest1[0], cStrassenHeight*sizeof(double), @x[0], @y[0], cStrassenWidth, cStrassenHeight, cStrassenHeight, cStrassenWidth, cStrassenWidth*sizeof(double), cStrassenHeight*sizeof(double), 256);
     endTime1 := MtxGetTime;

     TryClearCache;
     startTime2 := MtxGetTime;
     GenericStrassenMatrixMultiplication(@dest2[0], cStrassenHeight*sizeof(double), @x[0], @y[0], cStrassenWidth, cStrassenHeight, cStrassenHeight, cStrassenWidth, cStrassenWidth*sizeof(double), cStrassenHeight*sizeof(double));
     endTime2 := MtxGetTime;

     Check(CheckMtx(dest1, dest2), 'Error strassen matrix mult failed');

     TryClearCache;
     startTime3 := MtxGetTime;
     ASMStrassenMatrixMultiplication(@dest2[0], cStrassenHeight*sizeof(double), @x[0], @y[0], cStrassenWidth, cStrassenHeight, cStrassenHeight, cStrassenWidth, cStrassenWidth*sizeof(double), cStrassenHeight*sizeof(double));
     endTime3 := MtxGetTime;

     Check(CheckMtx(dest1, dest2), 'Error strassen matrix mult failed');

     Status(Format('%.2f, %.2f, %.2f', [(endTime1 - startTime1)/mtxFreq*1000, (endTime2 - startTime2)/mtxFreq*1000, (endTime3 - startTime3)/mtxFreq*1000]));
end;

{ TASMatrixBlockSizeSetup }

procedure TASMatrixBlockSizeSetup.TestSetupBestBlockSize;
begin
     SetupOptBlockMatrixSize;

     Status('BlockMatrixCacheSize: ' + IntToStr(BlockMatrixCacheSize));
end;

procedure TASMatrixBlockSizeSetup.TestSetupBlock;
begin
     SetupBlockedMatrixMultSize;

     Status(intToStr(BlockedMatrixMultSize));
end;

procedure TASMMatrixOperations.TestMtxVecMult;
const cMtx : Array[0..8] of double = (1, 2, 3, 4, 5, 6, 7, 8, 9);
      cV : Array[0..2] of double = (1, 2, 3);
var dest1, dest2, dest : Array[0..2] of double;
begin
     dest[0] := 1;
     dest[1] := 1;
     dest[2] := 1;
     Move(dest, dest1, sizeof(dest));
     Move(dest, dest2, sizeof(dest));

     GenericMtxVecMult(@dest1[0], sizeof(double), @cMtx[0], @cV[0], 3*sizeof(double), sizeof(double), 3, 3, 0.1, -1);
     ASMMatrixVectMult(@dest[0], sizeof(double), @cMtx[0], @cV[0], 3*sizeof(double), sizeof(double), 3, 3, 0.1, -1);
     ASMMatrixVectMultOddUnAlignedVAligned(@dest2[0], sizeof(double), @cMtx[0], @cV[0], 3*sizeof(double), sizeof(double), 3, 3, 0.1, -1);
     Check(CheckMtx(dest1, dest), 'Error matrix vector multiplication failed');
     Check(CheckMtx(dest2, dest), 'Error matrix vector multiplication failed');

     dest[0] := 1;
     dest[1] := 1;
     dest[2] := 1;
     Move(dest, dest1, sizeof(dest));
     Move(dest, dest2, sizeof(dest));

     GenericMtxVecMult(@dest1[0], sizeof(double), @cMtx[0], @cV[0], 3*sizeof(double), sizeof(double), 2, 2, 0.1, -1);
     ASMMatrixVectMult(@dest[0], sizeof(double), @cMtx[0], @cV[0], 3*sizeof(double), sizeof(double), 2, 2, 0.1, -1);
     ASMMatrixVectMultEvenUnAlignedVAligned(@dest2[0], sizeof(double), @cMtx[0], @cV[0], 3*sizeof(double), sizeof(double), 2, 2, 0.1, -1);
     Check(CheckMtx(dest1, dest), 'Error matrix vector multiplication failed');
     Check(CheckMtx(dest2, dest), 'Error matrix vector multiplication failed');

     dest[0] := 1;
     dest[1] := 1;
     dest[2] := 1;
     Move(dest, dest1, sizeof(dest));

     GenericMtxVecMultT(@dest1[0], sizeof(double), @cMtx[0], @cV[0], 3*sizeof(double), sizeof(double), 3, 3, 0.1, -1);
     ASMMatrixVectMultT(@dest[0], sizeof(double), @cMtx[0], @cV[0], 3*sizeof(double), sizeof(double), 3, 3, 0.1, -1);

     Check(CheckMtx(dest1, dest), 'Error matrix vector multiplication failed');
end;

procedure TASMMatrixOperations.TestBigMtxVecMult;
const cMtxSize = 1350;
var x, y : TDoubleDynArray;
    xa, ya : PDouble;
    dest1 : TDoubleDynArray;
    dest2,
    dest3, dest4 : TDoubleDynArray;
    row : TDoubleDynArray;
    rowa : PConstDoubleArr;
    startTime1, endTime1 : Int64;
    startTime2, endTime2 : int64;
    startTime3, endTime3 : int64;
    startTime4, endTime4 : int64;
    i: Integer;
begin
     randomize;
     FillMatrix(cMtxSize*cMtxSize, x, y, xa, ya);
     dest1 := Copy(x, 0, Length(x));
     dest2 := Copy(x, 0, Length(x));
     dest3 := Copy(x, 0, Length(x));
     dest4 := Copy(x, 0, Length(x));

     GetMem(rowa, sizeof(double)*cMtxSize);
     SetLength(row, cMtxSize);
     for i := 0 to cMtxSize - 1 do
     begin
          row[i] := y[i*cMtxSize];
          rowa^[i] := row[i]; 
     end;
         
     startTime1 := MtxGetTime;
     GenericMtxVecMult(@dest1[0], cMtxSize*sizeof(double), @x[0], @y[0], cMtxSize*sizeof(double), cMtxSize*sizeof(double), cMtxSize, cMtxSize, 2, -0.2);
     endTime1 := MtxGetTime;

     startTime2 := MtxGetTime;
     ASMMatrixVectMult(@dest2[0], cMtxSize*sizeof(double), @x[0], @y[0], cMtxSize*sizeof(double), cMtxSize*sizeof(double), cMtxSize, cMtxSize, 2, -0.2);
     endTime2 := MtxGetTime;

     startTime3 := MtxGetTime;
     ASMMatrixVectMultEvenAlignedVAligned(@dest3[0], cMtxSize*sizeof(double), xa, PDouble(rowa), cMtxSize*sizeof(double), sizeof(double), cMtxSize, cMtxSize, 2, -0.2);
     endTime3 := MtxGetTime;

     startTime4 := MtxGetTime;
     ASMMatrixVectMultEvenUnAlignedVAligned(@dest4[0], cMtxSize*sizeof(double), @x[0], @row[0], cMtxSize*sizeof(double), sizeof(double), cMtxSize, cMtxSize, 2, -0.2);
     endTime4 := MtxGetTime;
     
     Check(CheckMtx(dest1, dest2), 'Error Matrix vector multiplication failed');
     Check(CheckMtx(dest1, dest3), 'Error Matrix vector multiplication failed');
     Check(CheckMtx(dest1, dest4), 'Error Matrix vector multiplication failed');

     Status(Format('%.2f, %.2f, %.2f, %.2f', [(endTime1 - startTime1)/mtxFreq*1000, (endTime2 - startTime2)/mtxFreq*1000, (endTime3 - startTime3)/mtxFreq*1000, (endTime4 - startTime4)/mtxFreq*1000]));

     FreeMem(rowa);
     
     startTime1 := MtxGetTime;
     GenericMtxVecMult(@dest1[0], cMtxSize*sizeof(double), @x[0], @y[0], cMtxSize*sizeof(double), cMtxSize*sizeof(double), cMtxSize, cMtxSize, 2, -0.2);
     endTime1 := MtxGetTime;

     startTime2 := MtxGetTime;
     ASMMatrixVectMult(@dest2[0], cMtxSize*sizeof(double), xa, ya, cMtxSize*sizeof(double), cMtxSize*sizeof(double), cMtxSize, cMtxSize, 2, -0.2);
     endTime2 := MtxGetTime;
     
     Status(Format('%.2f, %.2f', [(endTime1 - startTime1)/mtxFreq*1000, (endTime2 - startTime2)/mtxFreq*1000]));
     Check(CheckMtx(dest1, dest2), 'Error Matrix vector multiplication failed');

     // odd functions

     dest1 := Copy(x, 0, Length(x));
     dest2 := Copy(x, 0, Length(x));
     dest3 := Copy(x, 0, Length(x));

     startTime1 := MtxGetTime;
     GenericMtxVecMult(@dest1[0], cMtxSize*sizeof(double), @x[0], @y[0], cMtxSize*sizeof(double), cMtxSize*sizeof(double), cMtxSize - 1, cMtxSize - 1, 2, -0.2);
     endTime1 := MtxGetTime;

     startTime2 := MtxGetTime;
     ASMMatrixVectMult(@dest2[0], cMtxSize*sizeof(double), @x[0], @y[0], cMtxSize*sizeof(double), cMtxSize*sizeof(double), cMtxSize - 1, cMtxSize - 1, 2, -0.2);
     endTime2 := MtxGetTime;

     startTime3 := MtxGetTime;
     ASMMatrixVectMultOddUnAlignedVAligned(@dest3[0], cMtxSize*sizeof(double), @x[0], @row[0], cMtxSize*sizeof(double), sizeof(double), cMtxSize - 1, cMtxSize - 1, 2, -0.2);
     endTime3 := MtxGetTime;
     
     Status(Format('%.2f, %.2f, %.2f', [(endTime1 - startTime1)/mtxFreq*1000, (endTime2 - startTime2)/mtxFreq*1000, (endTime3 - startTime3)/mtxFreq*1000]));

     Check(CheckMtx(dest1, dest2), 'Error odd len matrix vector multiplication failed');
     Check(CheckMtx(dest1, dest3), 'Error odd len matrix vector multiplication failed');

     startTime1 := MtxGetTime;
     GenericMtxVecMult(@dest1[0], cMtxSize*sizeof(double), @x[0], @y[0], cMtxSize*sizeof(double), cMtxSize*sizeof(double), cMtxSize - 1, cMtxSize - 1, 2, -0.2);
     endTime1 := MtxGetTime;

     startTime2 := MtxGetTime;
     ASMMatrixVectMult(@dest2[0], cMtxSize*sizeof(double), xa, ya, cMtxSize*sizeof(double), cMtxSize*sizeof(double), cMtxSize - 1, cMtxSize - 1, 2, -0.2);
     endTime2 := MtxGetTime;
     
     Status(Format('%.2f,  %.2f', [(endTime1 - startTime1)/mtxFreq*1000,  (endTime2 - startTime2)/mtxFreq*1000]));
     Check(CheckMtx(dest1, dest2), 'Error odd len matrix vector multiplication failed');

     startTime1 := MtxGetTime;
     GenericMtxVecMultT(@dest1[0], cMtxSize*sizeof(double), @x[0], @y[0], cMtxSize*sizeof(double), cMtxSize*sizeof(double), cMtxSize - 1, cMtxSize - 1, 2, -0.2);
     endTime1 := MtxGetTime;

     startTime2 := MtxGetTime;
     ASMMatrixVectMultT(@dest2[0], cMtxSize*sizeof(double), @x[0], @y[0], cMtxSize*sizeof(double), cMtxSize*sizeof(double), cMtxSize - 1, cMtxSize - 1, 2, -0.2);
     endTime2 := MtxGetTime;

     Status(Format('%.2f,  %.2f', [(endTime1 - startTime1)/mtxFreq*1000,  (endTime2 - startTime2)/mtxFreq*1000]));
     Check(CheckMtx(dest1, dest2), 'Error transposed vector multiplication failed');

     dest3 := Copy(dest2, 0, Length(dest2));

     startTime1 := MtxGetTime;
     GenericMtxVecMultT(@dest1[0], sizeof(double), @x[0], @y[0], cMtxSize*sizeof(double), cMtxSize*sizeof(double), cMtxSize - 1, cMtxSize - 1, 2, -0.2);
     endTime1 := MtxGetTime;

     startTime2 := MtxGetTime;
     ASMMatrixVectMultT(@dest2[0], sizeof(double), @x[0], @y[0], cMtxSize*sizeof(double), cMtxSize*sizeof(double), cMtxSize - 1, cMtxSize - 1, 2, -0.2);
     endTime2 := MtxGetTime;

     startTime3 := MtxGetTime;
     ASMMatrixVectMultTDestVec(@dest3[0], sizeof(double), @x[0], @y[0], cMtxSize*sizeof(double), cMtxSize*sizeof(double), cMtxSize - 1, cMtxSize - 1, 2, -0.2);
     endTime3:= MtxGetTime;

     Status(Format('%.2f,  %.2f,    %.2f', [(endTime1 - startTime1)/mtxFreq*1000,  (endTime2 - startTime2)/mtxFreq*1000, (endTime3 - startTime3)/mtxFreq*1000]));
     Status(Format('%.2f,  %.2f,    %.2f', [(endTime1 - startTime1)/mtxFreq*1000,  (endTime2 - startTime2)/mtxFreq*1000, (endTime3 - startTime3)/mtxFreq*1000]));

     Check(CheckMtx(dest1, dest2), 'Error transposed vector multiplication failed');
     //Check(CheckMtx(dest1, dest3), 'Error transposed vector multiplication failed');

     FreeMem(xa);
     FreeMem(ya);
end;

initialization
  RegisterTest(TestMatrixOperations{$IFNDEF FPC}.Suite{$ENDIF});
  RegisterTest(TASMMatrixOperations{$IFNDEF FPC}.Suite{$ENDIF});
  RegisterTest(TASMatrixBlockSizeSetup{$IFNDEF FPC}.Suite{$ENDIF});
end.

