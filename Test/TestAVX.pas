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

unit TestAVX;

interface

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}

{$IFDEF MACOS}
  {$DEFINE FMX}
{$ENDIF}

uses {$IFDEF FPC} testregistry {$ELSE} {$IFDEF FMX}DUnitX.TestFramework {$ELSE}TestFramework {$ENDIF} {$ENDIF}
     , Classes, SysUtils, Types, SimpleMatrixOperations, Matrix,
     ASMMatrixOperations, BaseMatrixTestCase;

type
  { TestAVXMatrixOperations }
  {$IFDEF FMX} [TestFixture] {$ENDIF}
  TestAVXMatrixOperations = class(TBaseMatrixTestCase)
  published
    procedure TestIsAvail;
    procedure TestAVXMove;
    procedure TestAVXInit;
    procedure TestAVXRowExchange;
    procedure TestAVXAdd;
    procedure TestAVXSub;
    procedure TestAVXSubT;
    procedure TestAVXSubVec;
    procedure TestAVXAddVec;
    procedure TestAVXAbs;
    procedure TestAddAndScale;
    procedure TestAVXMinMax;
    procedure TestAVXMean;
    procedure TestAVXVar;
    procedure TestAVXMeanVar;
    procedure TestElementwiseNorm2;
    procedure TestNormalize;
    procedure TestSQRT;
    procedure TestAVXSum;
    procedure TestAVXCumulativeSumColumn;
    procedure TestAVXDifferentiate;
    procedure TestTranspose;
    procedure TestAVXElemMult;
    procedure TestAVXElementWiseDiv;
    procedure TestAVXVecMult;
    procedure TestBigMtxVecMult;
    procedure TestAVXMultMod16;
    procedure TestAVXMultTransposed;
    procedure TestAVXMult;
    procedure TestAVXBigMult;
    procedure TestAVXDiagMtxMult;
    procedure TestBlkThreadedMatrixMultT2;
    procedure TestBlkThreadedMatrixMult;

    procedure TestAVXBigSVDHeightGEWidth;
    procedure TestConvolveBig;
  end;

implementation

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}

uses {$IFDEF x64}
     ASMMatrixMultTransposedOperationsx64, ASMMatrixVectorMultOperationsx64,
     AVXMatrixMultTransposedOperationsx64,
     AVXMatrixOperations, AVXMatrixMultOperationsx64, AVXMatrixVectorMultOperationsx64,
     AVXMatrixAddSubOperationsx64, AVXMatrixCumSumDiffOperationsx64,
     {$ELSE}
     ASMMatrixMultTransposedOperations, ASMMatrixVectorMultOperations,
     AVXMatrixMultTransposedOperations, AVXMatrixOperations,
     AVXMatrixMultOperations, AVXMatrixVectorMultOperations,
     AVXMatrixAddSubOperations, AVXMatrixCumSumDiffOperations,
     {$ENDIF}
     MatrixConst, MathUtilFunc, MtxTimer, MatrixASMStubSwitch, MtxThreadPool, BlockSizeSetup,
     LinAlgSVD, Math, CPUFeatures, BlockedMult, ThreadedMatrixOperations;

{ TestAVXMatrixOperations }

procedure TestAVXMatrixOperations.TestAVXMove;
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
    mem1, mem2, memxa : PByte;
const cMtxWidth = 2000;
      cMtxHeight = 500;
      cMtxSize = cMtxWidth*cMtxHeight;
      cMtxLinewidth = cMtxWidth*sizeof(double);
begin
     randomize;
     SetLength(x, cMtxSize);
     SetLength(dest1, cMtxSize);
     AllocAlignedMtx( cMtxSize, xa, memxa );

     AllocAlignedMtx( cMtxSize, dest1a, mem1 );
     AllocAlignedMtx( cMtxSize + cMtxWidth, dest2a, mem2 );

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
     AVXMatrixCopy(dest2a, cMtxLineWidth, xa, cMtxLinewidth, cMtxWidth, cMtxheight);
     endTime2 := MtxGetTime;

     Move(dest1a^, dest1[0], length(dest2)*sizeof(double));
     Move(dest2a^, dest2[0], length(dest2)*sizeof(double));
     res := CheckMtxIdx(dest1, dest2, idx);
     if not res then
        Status(IntToStr(idx));
     check(res);

     inc(dest2a);
     startTime3 := MtxGetTime;
     AVXMatrixCopy(dest2a, cMtxLineWidth, @x[0], cMtxLinewidth, cMtxWidth, cMtxheight);
     endTime3 := MtxGetTime;

     Status(Format('%.2f, %.2f, %.2f', [(endTime1 - startTime1)/mtxFreq*1000, (endTime2 - startTime2)/mtxFreq*1000,
                        (endTime3 - startTime3)/mtxFreq*1000]));

     Move(dest1a^, dest1[0], length(dest2)*sizeof(double));
     Move(dest2a^, dest2[0], length(dest2)*sizeof(double));
     res := CheckMtxIdx(dest1, dest2, idx);
     if not res then
        Status(IntToStr(idx));
     check(res);

     FreeMem(mem1);
     FreeMem(mem2);
     FreeMem(memxa);
end;

procedure TestAVXMatrixOperations.TestAVXInit;
  var x : Array of double;
      xa : PDouble;
      mem : PDouble;
      startTime1: Int64;
      endTime1: Int64;
      startTime2: Int64;
      endTime2: Int64;
      startTime3: Int64;
      endTime3: Int64;
      mtx : Array[0..15] of double;
  const cMtxWidth = 2000;
        cMtxHeight = 500;
        cMtxSize = cMtxWidth*cMtxHeight;
        cMtxLinewidth = cMtxWidth*sizeof(double);
  begin
       FillChar(mtx, sizeof(mtx), 0);
       AVXMatrixInit( @mtx[0], 4*sizeof(double), 1, 1, 2);
       CheckMtxVal(@mtx[0], 4*sizeof(double), 1, 1, 2);

       AVXMatrixInit( @mtx[0], 4*sizeof(double), 2, 1, 1.2);
       CheckMtxVal(@mtx[0], 4*sizeof(double), 2, 1, 1.2);

       AVXMatrixInit( @mtx[0], 4*sizeof(double), 2, 2, 1.4);
       CheckMtxVal(@mtx[0], 4*sizeof(double), 2, 2, 1.4);

       AVXMatrixInit( @mtx[0], 4*sizeof(double), 3, 3, 1.5);
       CheckMtxVal(@mtx[0], 4*sizeof(double), 3, 3, 1.5);

       AVXMatrixInit( @mtx[0], 4*sizeof(double), 4, 3, 2.5);
       CheckMtxVal(@mtx[0], 4*sizeof(double), 4, 3, 2.5);

       SetLength(x, cMtxSize);
       GetMem(mem, (cMTXSize + 4)*sizeof(double));

       xa := AlignPtr32(mem);

       startTime1 := MtxGetTime;
       GenericMtxInit( @x[0], cMtxWidth*sizeof(double), cMtxWidth, cMtxHeight, 2.1 );
       endTime1 := MtxGetTime;

       Check(CheckMtxVal( x, 2.1), 'Failed to init');

       startTime2 := MtxGetTime;
       AVXMatrixInit( @x[0], cMtxWidth*sizeof(double), cMtxWidth, cMtxHeight, 2.2 );
       endTime2 := MtxGetTime;

       Check(CheckMtxVal( x, 2.2), 'Failed to init');


       startTime3 := MtxGetTime;
       AVXMatrixInit( xa, cMtxWidth*sizeof(double), cMtxWidth, cMtxHeight, 2.4 );
       endTime3 := MtxGetTime;

       Check(CheckMtxVal( xa, cmtxwidth*sizeof(double), cMtxWidth, cMtxHeight, 2.4), 'Failed aligned init');
       Status(Format('%.2f, %.2f, %.2f', [(endTime1 - startTime1)/mtxFreq*1000, (endTime2 - startTime2)/mtxFreq*1000,
                          (endTime3 - startTime3)/mtxFreq*1000]));

       FreeMem(mem);

       SetLength(x, cMtxSize + cMtxHeight);
       GetMem(mem, (cMTxSize + 2*cMtxHeight + 4)*sizeof(double));
       xa := AlignPtr32(mem);

       startTime1 := MtxGetTime;
       GenericMtxInit( @x[0], (cMtxWidth + 1)*sizeof(double), cMtxWidth + 1, cMtxHeight, 2.1 );
       endTime1 := MtxGetTime;

       Check(CheckMtxVal( x, 2.1), 'Failed to init');

       startTime2 := MtxGetTime;
       AVXMatrixInit( @x[0], (cMtxWidth + 1)*sizeof(double), cMtxWidth + 1, cMtxHeight, 2.2 );
       endTime2 := MtxGetTime;

       Check(CheckMtxVal( x, 2.2), 'Failed to init');


       startTime3 := MtxGetTime;
       AVXMatrixInit( xa, (cMtxWidth + 2)*sizeof(double), cMtxWidth + 1, cMtxHeight, 2.4 );
       endTime3 := MtxGetTime;

       Check(CheckMtxVal( xa, (cmtxwidth + 2)*sizeof(double), cMtxWidth + 1, cMtxHeight, 2.4), 'Failed aligned init');
       Status(Format('%.2f, %.2f, %.2f', [(endTime1 - startTime1)/mtxFreq*1000, (endTime2 - startTime2)/mtxFreq*1000,
                          (endTime3 - startTime3)/mtxFreq*1000]));

       FreeMem(mem);
end;


procedure TestAVXMatrixOperations.TestAVXRowExchange;
const mt1 : Array[0..7] of double = (0, 1, 2, 3, 4, 5, 6, 7);
      mt2 : Array[0..7] of double = (4, 5, 6, 7, 0, 1, 2, 3);

      cBigMtxSize = 4096;
var dest : Array[0..7] of double;
    data, data1, data2 : Pdouble;
    mem : PByte;
    mem1 : PByte;
    outIdx : Integer;
    start1, stop1, start2, stop2 : Int64;
begin
     move(mt1, dest, sizeof(dest));

     // test non aligned data
     AVXRowSwap(@dest[0], @dest[4], 3);
     CheckMtx(dest, mt2, 3, 2);

     move(mt1, dest, sizeof(dest));
     AVXRowSwap(@dest[0], @dest[4], 4);
     CheckMtx(dest, mt2, 4, 2);

     // test aligned data
     mem := GetMemory(sizeof(mt1) + 64);
     data := AlignPtr32(mem);
     data1 := data;
     inc(data1, 4);

     move(mt1, data^, sizeof(mt1));
     AVXRowSwap(data, data1, 3);
     move(data^, dest, sizeof(dest));
     CheckMtx(dest, mt2, 3, 2);

     move(mt1, data^, sizeof(mt1));
     AVXRowSwap(data, data1, 4);
     move(data^, dest, sizeof(dest));
     CheckMtx(dest, mt2, 4, 2);

     FreeMem(mem);

     // test unaligned data
     mem := GetMemory(sizeof(mt1) + 96);
     data := PDouble(NativeUInt(mem) + 8);
     data1 := data;
     inc(data1, 4);

     move(mt1, data^, sizeof(mt1));
     AVXRowSwap(data, data1, 3);
     move(data^, dest, sizeof(dest));
     CheckMtx(dest, mt2, 3, 2);

     move(mt1, data^, sizeof(mt1));
     AVXRowSwap(data, data1, 4);
     move(data^, dest, sizeof(dest));
     CheckMtx(dest, mt2, 4, 2);

     FreeMem(mem);

     // long rows
     FillAlignedMtx( 350, data, mem);
     FillAlignedMtx( 350, data1, mem1);

     Move( data^, data1^, 350*sizeof(double));

     data2 := data;
     inc(data2, 350 div 2);
     GenericRowSwap( data, data2, 350 div 2);

     data2 := data1;
     inc(data2, 350 div 2);
     AVXRowSwap(data1, data2, 350 div 2);

     Check(CheckMtxIdx(data, data1, 350, outidx, 1e-6), 'AVX Error Matrix row swap failed');

     FreeMem(mem);
     FreeMem(mem1);

     // long rows
     FillAlignedMtx( cBigMtxSize, data, mem);
     FillAlignedMtx( cBigMtxSize, data1, mem1);

     Move( data^, data1^, cBigMtxSize*sizeof(double));

     data2 := data;
     inc(data2, cBigMtxSize div 2);
     start1 := MtxGetTime;
     GenericRowSwap( data, data2, cBigMtxSize div 2);
     stop1 := MtxGetTime;

     data2 := data1;
     inc(data2, cBigMtxSize div 2);
     start2 := MtxGetTime;
     AVXRowSwap(data1, data2, cBigMtxSize div 2);
     stop2 := MtxGetTime;

     Check(CheckMtxIdx(data, data1, cBigMtxSize, outidx, 1e-6), 'AVX Error Matrix row swap failed');

     Status(Format('%.2f, %.2f', [(stop1 - start1)/mtxFreq*1000, (stop2 - start2)/mtxFreq*1000]));

     FreeMem(mem);
     FreeMem(mem1);
end;

procedure TestAVXMatrixOperations.TestAVXAbs;
const mt1 : Array[0..5] of double = (-1, 2, 2, -2, 3, -3);
      mtdest : Array[0..5] of double = (1, 2, 2, 2, 3, 3);
      sign : Array[0..1] of double = (-1, 1);
var mt2 : TDoubleDynArray;
    mtx : TDoubleDynArray;
    cnt: Integer;
    mta1 : PConstDoubleArr;
    mem : PByte;
begin
     SetLength(mtx, Length(mt1));
     Move(mt1[0], mtx[0], sizeof(mt1));
     AVXMatrixAbs(@mtx[0], 3*sizeof(double), 3, 2);
     Status(WriteMtxDyn(mtx, 3));
     CheckEqualsMem(@mtx[0], @mtdest[0], Length(mtx)*sizeof(double), 'Error Matrix abs: ' + #13#10 + WriteMtxDyn(mtx, 3));

     Move(mt1[0], mtx[0], sizeof(mt1));
     AVXMatrixAbs(@mtx[0], 6*sizeof(double), 6, 1);
     CheckEqualsMem(@mtx[0], @mtdest[0], sizeof(mtdest), 'Error Matrix abs: ' + #13#10 + WriteMtxDyn(mtx, 3));

     Move(mt1[0], mtx[0], sizeof(mt1));
     AVXMatrixAbs(@mtx[0], sizeof(double), 1, 6);
     CheckEqualsMem(@mtx[0], @mtdest[0], sizeof(mtdest), 'Error Matrix abs: ' + #13#10 + WriteMtxDyn(mtx, 3));

     Move(mt1[0], mtx[0], sizeof(mt1));

     SetLength(mt2, 1022);
     for cnt := 0 to Length(mt2) - 1 do
         mt2[cnt] := Random(10)*sign[random(2)];

     AVXMatrixAbs(@mt2[0], (length(mt2) div 14)*sizeof(double), (length(mt2) div 14), 14);
     for cnt := 0 to Length(mt2) - 1 do
         Check(mt2[cnt] >= 0, 'Error matrix abs - negative value found:' + IntToStr(cnt));

     SetLength(mt2, 1022);
     for cnt := 0 to Length(mt2) - 1 do
     begin
          mt2[cnt] := Random(10)*sign[random(2)];
          if ((cnt + 1) mod 73) = 0 then
             mt2[cnt] := -1000;
     end;

     AVXMatrixAbs(@mt2[0], (length(mt2) div 14)*sizeof(double), (length(mt2) div 14) - 1, 14);
     for cnt := 0 to Length(mt2) - 1 do
         Check((mt2[cnt] = -1000) or (mt2[cnt] >= 0), 'Error matrix abs - negative value found');

     // aligned checks
     AllocAlignedMtx(1024, PDouble(mta1), mem);
     for cnt := 0 to 1024 - 1 do
         mta1^[cnt] := Random(10)*sign[random(2)];

     AVXMatrixAbs(PDouble(mta1), 128*sizeof(double), 128, 8);
     for cnt := 0 to Length(mt2) - 1 do
         Check(mta1^[cnt] >= 0, 'Error matrix abs - negative value found');

     for cnt := 0 to 1024 - 1 do
     begin
          mta1^[cnt] := Random(10)*sign[random(2)];
          if ((cnt + 1) mod 128) = 0 then
             mta1^[cnt] := 0;
     end;

     AVXMatrixAbs(PDouble(mta1), 128*sizeof(double), 127, 8);
     for cnt := 0 to Length(mt2) - 1 do
         Check(mta1^[cnt] >= 0, 'Error matrix abs - negative value found');

     FreeMem(mem);
end;

procedure TestAVXMatrixOperations.TestAVXMinMax;
const cMtxWidth : Array[0..11] of integer = (1, 3, 7, 8, 9, 10, 23, 43, 128, 1024, 2048, 2573);
      cMtxHeight : Array[0..11] of integer = (1, 3, 7, 8, 9, 10, 23, 43, 128, 1024, 2048, 2573);
var pX : PDouble;
    pm1 : PByte;
    LineWidthX : TASMNativeInt;
    startTime1, endTime1 : Int64;
    startTime2, endTime2 : int64;
    startTime3, endTime3 : int64;
    startTime4, endTime4 : int64;
    i, j : Integer;
    res1, res2 : double;
    mtxWidth, mtxHeight : integer;
begin
     for i := 0 to Length(cMtxHeight) - 1 do
     begin
          for j := 0 to Length(cMtxWidth) -1  do
          begin
               mtxWidth := cMtxWidth[j];
               mtxHeight := cMtxHeight[i];
               FillAlignedMtx( mtxWidth, mtxHeight, pX, pm1, LineWidthX);

               startTime1 := MtxGetTime;
               res1 := GenericMtxMax(px, mtxWidth, mtxHeight, LineWidthX );
               endTime1 := MtxGetTime;

               startTime2 := MtxGetTime;
               res2 := AVXMatrixMax(px, mtxWidth, mtxHeight, LineWidthX);
               endTime2 := MtxGetTime;

               Check(res1 = res2, Format('AVX Matrix max failed on %d, %d', [mtxWidth, mtxHeight]));

               startTime3 := MtxGetTime;
               res1 := GenericMtxMin(px, mtxWidth, mtxHeight, LineWidthX);
               endTime3 := MtxGetTime;

               startTime4 := MtxGetTime;
               res2 := AVXMatrixMin(px, mtxWidth, mtxHeight, LineWidthX);
               endTime4 := MtxGetTime;

               Check(res1 = res2, Format('AVX Matrix min failed on %d, %d', [mtxWidth, mtxHeight]));

               Status( Format('Big Min/Max: %d, %d took %.3fms, %.3fms, %.3fms, %.3fms', [mtxWidth, mtxHeight,
                  (endTime1 - startTime1)/mtxFreq*1000, (endTime2 - startTime2)/mtxFreq*1000,
                  (endTime3 - startTime3)/mtxFreq*1000, (endTime4 - startTime4)/mtxFreq*1000]));

               FreeMem(pm1);
          end;
     end;
end;

procedure TestAVXMatrixOperations.TestAVXMean;
const cMtxWidth : Array[0..11] of integer = (1, 3, 7, 8, 9, 10, 23, 43, 128, 1024, 2048, 2573);
      cMtxHeight : Array[0..11] of integer = (1, 3, 7, 8, 9, 10, 23, 43, 128, 1024, 2048, 2573);
var pX : PDouble;
    pY : PDouble;
    pY1 : PDouble;
    pm1, pm2, pm3 : PByte;
    LineWidthX, LineWidthY, LineWidthY2 : TASMNativeInt;
    startTime1, endTime1 : Int64;
    startTime2, endTime2 : int64;
    startTime3, endTime3 : int64;
    startTime4, endTime4 : int64;
    i, j, idx : Integer;
    mtxWidth, mtxHeight : integer;
    doAligned : integer;
begin
     for doAligned := 0 to 1 do
     begin
          for i := 0 to Length(cMtxHeight) - 1 do
          begin
               for j := 0 to Length(cMtxWidth) -1  do
               begin
                    mtxWidth := cMtxWidth[j];
                    mtxHeight := cMtxHeight[i];
                    if doAligned = 0
                    then
                        FillUnAlignedMtx( mtxWidth, mtxHeight, pX, pm1, LineWidthX)
                    else
                        FillAlignedMtx( mtxWidth, mtxHeight, pX, pm1, LineWidthX);

                    AllocAlignedMtx(mtxWidth, 1, pY, pm2, LineWidthY);
                    AllocAlignedMtx(mtxWidth, 1, pY1, pm3, LineWidthY2);

                    startTime1 := MtxGetTime;
                    GenericMtxMean(py, LineWidthY, px, LineWidthX, mtxWidth, mtxHeight, False );
                    endTime1 := MtxGetTime;

                    startTime2 := MtxGetTime;
                    AVXMatrixMean(py1, LineWidthY2, px, LineWidthX, mtxWidth, mtxHeight, False );
                    endTime2 := MtxGetTime;

                    Check(CheckMtxIdx(pY, pY1, LineWidthY, LineWidthY2, mtxWidth, 1, idx), Format('AVX Matrix mean column failed on %d, %d @ %d', [mtxWidth, mtxHeight, idx]));

                    FreeMem(pm3);
                    FreeMem(pm2);

                    AllocAlignedMtx(1, mtxHeight, pY, pm2, LineWidthY);
                    AllocAlignedMtx(1, mtxHeight, pY1, pm3, LineWidthY2);

                    startTime3 := MtxGetTime;
                    GenericMtxMean(py, LineWidthY, px, LineWidthX, mtxWidth, mtxHeight, True );
                    endTime3 := MtxGetTime;

                    startTime4 := MtxGetTime;
                    AVXMatrixMean(py1, LineWidthY2, px, LineWidthX, mtxWidth, mtxHeight, True );
                    endTime4 := MtxGetTime;

                    Check(CheckMtxIdx(pY, pY1, LineWidthY, LineWidthY2, 1, mtxHeight, idx), Format('AVX Matrix mean column failed on %d, %d @ %d', [mtxWidth, mtxHeight, idx]));

                    Status( Format('Big Mean: %d, %d took %.3fms, %.3fms, %.3fms, %.3fms', [mtxWidth, mtxHeight,
                            (endTime1 - startTime1)/mtxFreq*1000, (endTime2 - startTime2)/mtxFreq*1000,
                            (endTime3 - startTime3)/mtxFreq*1000, (endTime4 - startTime4)/mtxFreq*1000]));

                    FreeMem(pm1);
                    FreeMem(pm2);
                    FreeMem(pm3);
               end;
          end;
     end;
end;


procedure TestAVXMatrixOperations.TestAVXVar;
const cMtxWidth : Array[0..11] of integer = (1, 3, 7, 8, 9, 10, 23, 43, 128, 1024, 2048, 2573);
      cMtxHeight : Array[0..11] of integer = (1, 3, 7, 8, 9, 10, 23, 43, 128, 1024, 2048, 2573);
var pX : PDouble;
    pY : PDouble;
    pY1 : PDouble;
    pm1, pm2, pm3 : PByte;
    LineWidthX, LineWidthY, LineWidthY2 : TASMNativeInt;
    startTime1, endTime1 : Int64;
    startTime2, endTime2 : int64;
    startTime3, endTime3 : int64;
    startTime4, endTime4 : int64;
    i, j, idx : Integer;
    mtxWidth, mtxHeight : integer;
    doAligned : integer;
    s : string;
begin
     for doAligned := 0 to 1 do
     begin
          if doAligned = 0
          then
              s := 'Unaligned'
          else
              s := 'Aligned';

          for i := 0 to Length(cMtxHeight) - 1 do
          begin
               for j := 0 to Length(cMtxWidth) -1  do
               begin
                    mtxWidth := cMtxWidth[j];
                    mtxHeight := cMtxHeight[i];
                    if doAligned = 0
                    then
                        FillUnAlignedMtx( mtxWidth, mtxHeight, pX, pm1, LineWidthX)
                    else
                        FillAlignedMtx( mtxWidth, mtxHeight, pX, pm1, LineWidthX);
                    AllocAlignedMtx(mtxWidth, 1, pY, pm2, LineWidthY);
                    AllocAlignedMtx(mtxWidth, 1, pY1, pm3, LineWidthY2);

                    startTime1 := MtxGetTime;
                    GenericMtxVar(py, LineWidthY, px, LineWidthX, mtxWidth, mtxHeight, False, False );
                    endTime1 := MtxGetTime;

                    startTime2 := MtxGetTime;
                    AVXMatrixVar(py1, LineWidthY2, px, LineWidthX, mtxWidth, mtxHeight, False, False );
                    endTime2 := MtxGetTime;

                    Check(CheckMtxIdx(pY, pY1, LineWidthY, LineWidthY2, mtxWidth, 1, idx), Format('AVX Matrix var column failed on %d, %d @ %d', [mtxWidth, mtxHeight, idx]));

                    FreeMem(pm3);
                    FreeMem(pm2);

                    AllocAlignedMtx(1, mtxHeight, pY, pm2, LineWidthY);
                    AllocAlignedMtx(1, mtxHeight, pY1, pm3, LineWidthY2);

                    startTime3 := MtxGetTime;
                    GenericMtxVar(py, LineWidthY, px, LineWidthX, mtxWidth, mtxHeight, True, False );
                    endTime3 := MtxGetTime;

                    startTime4 := MtxGetTime;
                    AVXMatrixVar(py1, LineWidthY2, px, LineWidthX, mtxWidth, mtxHeight, True, False );
                    endTime4 := MtxGetTime;

                    Check(CheckMtxIdx(pY, pY1, LineWidthY, LineWidthY2, 1, mtxHeight, idx), Format('AVX Matrix var row failed on %d, %d @ %d', [mtxWidth, mtxHeight, idx]));

                    Status( Format('%s Big Var: %d, %d took %.3fms, %.3fms, %.3fms, %.3fms', [s, mtxWidth, mtxHeight,
                            (endTime1 - startTime1)/mtxFreq*1000, (endTime2 - startTime2)/mtxFreq*1000,
                            (endTime3 - startTime3)/mtxFreq*1000, (endTime4 - startTime4)/mtxFreq*1000]));

                    FreeMem(pm1);
                    FreeMem(pm2);
                    FreeMem(pm3);
               end;
          end;
     end;
end;


procedure TestAVXMatrixOperations.TestAVXMeanVar;
const cMtxWidth : Array[0..11] of integer = (1, 3, 7, 8, 9, 10, 23, 43, 128, 1024, 2048, 2573);
      cMtxHeight : Array[0..11] of integer = (1, 3, 7, 8, 9, 10, 23, 43, 128, 1024, 2048, 2573);
var pX : PDouble;
    pY : PDouble;
    pY1 : PDouble;
    pm1, pm2, pm3 : PByte;
    LineWidthX, LineWidthY, LineWidthY2 : TASMNativeInt;
    startTime1, endTime1 : Int64;
    startTime2, endTime2 : int64;
    startTime3, endTime3 : int64;
    startTime4, endTime4 : int64;
    i, j, idx : Integer;
    mtxWidth, mtxHeight : integer;
    doAligned : integer;
begin
     for doAligned := 0 to 1 do
     begin
          for i := 0 to Length(cMtxHeight) - 1 do
          begin
               for j := 0 to Length(cMtxWidth) -1  do
               begin
                    mtxWidth := cMtxWidth[j];
                    mtxHeight := cMtxHeight[i];
                    if doAligned = 0
                    then
                        FillUnAlignedMtx( mtxWidth, mtxHeight, pX, pm1, LineWidthX)
                    else
                        FillAlignedMtx( mtxWidth, mtxHeight, pX, pm1, LineWidthX);
                    AllocAlignedMtx(mtxWidth, 2, pY, pm2, LineWidthY);
                    AllocAlignedMtx(mtxWidth, 2, pY1, pm3, LineWidthY2);

                    startTime1 := MtxGetTime;
                    GenericMtxMeanVar(py, LineWidthY, px, LineWidthX, mtxWidth, mtxHeight, False, False );
                    endTime1 := MtxGetTime;

                    startTime2 := MtxGetTime;
                    AVXMatrixMeanVar(py1, LineWidthY2, px, LineWidthX, mtxWidth, mtxHeight, False, False );
                    endTime2 := MtxGetTime;

                    Check(CheckMtxIdx(pY, pY1, LineWidthY, LineWidthY2, mtxWidth, 1, idx), Format('AVX Matrix var/mean column failed on %d, %d @ %d', [mtxWidth, mtxHeight, idx]));

                    FreeMem(pm3);
                    FreeMem(pm2);

                    AllocAlignedMtx(2, mtxHeight, pY, pm2, LineWidthY);
                    AllocAlignedMtx(2, mtxHeight, pY1, pm3, LineWidthY2);

                    startTime3 := MtxGetTime;
                    GenericMtxMeanVar(py, LineWidthY, px, LineWidthX, mtxWidth, mtxHeight, True, False );
                    endTime3 := MtxGetTime;

                    startTime4 := MtxGetTime;
                    AVXMatrixMeanVar(py1, LineWidthY2, px, LineWidthX, mtxWidth, mtxHeight, True, False );
                    endTime4 := MtxGetTime;

                    Check(CheckMtxIdx(pY, pY1, LineWidthY, LineWidthY2, 1, mtxHeight, idx), Format('AVX Matrix var/mean row failed on %d, %d @ %d', [mtxWidth, mtxHeight, idx]));

                    Status( Format('Big MeanVar: %d, %d took %.3fms, %.3fms, %.3fms, %.3fms', [mtxWidth, mtxHeight,
                            (endTime1 - startTime1)/mtxFreq*1000, (endTime2 - startTime2)/mtxFreq*1000,
                            (endTime3 - startTime3)/mtxFreq*1000, (endTime4 - startTime4)/mtxFreq*1000]));

                    FreeMem(pm1);
                    FreeMem(pm2);
                    FreeMem(pm3);
               end;
          end;
     end;
end;

procedure TestAVXMatrixOperations.TestElementwiseNorm2;
const cMtxWidth : Array[0..11] of integer = (1, 3, 7, 8, 9, 10, 23, 43, 128, 1024, 2048, 2573);
      cMtxHeight : Array[0..11] of integer = (1, 3, 7, 8, 9, 10, 23, 43, 128, 1024, 2048, 2573);
var pX : PDouble;
    pm1 : PByte;
    LineWidthX : TASMNativeInt;
    startTime1, endTime1 : Int64;
    startTime2, endTime2 : int64;
    i, j : Integer;
    mtxWidth, mtxHeight : integer;
    res1, res2 : double;
    doAligned : integer;
    s : string;
begin
     for doAligned := 0 to 1 do
     begin
          if doAligned = 0
          then
              s := 'Unaligned'
          else
              s := 'Aligned';
          for i := 0 to Length(cMtxHeight) - 1 do
          begin
               for j := 0 to Length(cMtxWidth) -1  do
               begin
                    mtxWidth := cMtxWidth[j];
                    mtxHeight := cMtxHeight[i];
                    if doAligned = 0
                    then
                        FillUnAlignedMtx( mtxWidth, mtxHeight, pX, pm1, LineWidthX)
                    else
                        FillAlignedMtx( mtxWidth, mtxHeight, pX, pm1, LineWidthX);

                    startTime1 := MtxGetTime;
                    res1 := GenericMtxElementwiseNorm2(px, LineWidthX, mtxWidth, mtxHeight, False );
                    endTime1 := MtxGetTime;

                    startTime2 := MtxGetTime;
                    res2 := AVXMatrixElementwiseNorm2(px, LineWidthX, mtxWidth, mtxHeight, False );
                    endTime2 := MtxGetTime;

                    Check(SameValue(res1, res2, 1e-6), 'Elementwise norm failed');

                    Status( Format('%s Elementwise Norm: %d, %d took %.3fms, %.3fms', [s, mtxWidth, mtxHeight,
                            (endTime1 - startTime1)/mtxFreq*1000, (endTime2 - startTime2)/mtxFreq*1000]));

                    FreeMem(pm1);
               end;
          end;
     end;
end;

procedure TestAVXMatrixOperations.TestIsAvail;
begin
     Check(IsAVXPresent, 'No AVX present - Test failed');
end;

procedure TestAVXMatrixOperations.TestNormalize;
const cMtxWidth : Array[0..11] of integer = (1, 3, 7, 8, 9, 10, 23, 43, 128, 1024, 2048, 2573);
      cMtxHeight : Array[0..11] of integer = (1, 3, 7, 8, 9, 10, 23, 43, 128, 1024, 2048, 2573);
var pX : PDouble;
    pY : PDouble;
    pY1 : PDouble;
    pm1, pm2, pm3 : PByte;
    LineWidthX, LineWidthY, LineWidthY2 : TASMNativeInt;
    startTime1, endTime1 : Int64;
    startTime2, endTime2 : int64;
    startTime3, endTime3 : int64;
    startTime4, endTime4 : int64;
    i, j, idx : Integer;
    mtxWidth, mtxHeight : integer;
    doAligned : integer;
    s : string;
begin
     s := 'Unaligned';
     for doAligned := 0 to 1 do
     begin
          for i := 0 to Length(cMtxHeight) - 1 do
          begin
               for j := 0 to Length(cMtxWidth) -1  do
               begin
                    mtxWidth := cMtxWidth[j];
                    mtxHeight := cMtxHeight[i];
                    if doAligned = 0
                    then
                        FillUnAlignedMtx( mtxWidth, mtxHeight, pX, pm1, LineWidthX)
                    else
                        FillAlignedMtx( mtxWidth, mtxHeight, pX, pm1, LineWidthX);

                    AllocAlignedMtx(mtxWidth, mtxHeight, pY, pm2, LineWidthY);
                    AllocAlignedMtx(mtxWidth, mtxHeight, pY1, pm3, LineWidthY2);

                    startTime1 := MtxGetTime;
                    GenericMtxNormalize(py, LineWidthY, px, LineWidthX, mtxWidth, mtxHeight, True );
                    endTime1 := MtxGetTime;

                    startTime2 := MtxGetTime;
                    AVXMatrixNormalize(py1, LineWidthY2, px, LineWidthX, mtxWidth, mtxHeight, True );
                    endTime2 := MtxGetTime;

                    Check(CheckMtxIdx(pY, pY1, LineWidthY, LineWidthY2, mtxWidth, mtxHeight, idx), Format('AVX Matrix normalize row failed on %d, %d @ %d', [mtxWidth, mtxHeight, idx]));

                    FreeMem(pm3);
                    FreeMem(pm2);

                    AllocAlignedMtx(mtxWidth, mtxHeight, pY, pm2, LineWidthY);
                    AllocAlignedMtx(mtxWidth, mtxHeight, pY1, pm3, LineWidthY2);

                    startTime3 := MtxGetTime;
                    GenericMtxNormalize(py, LineWidthY, px, LineWidthX, mtxWidth, mtxHeight, False );
                    endTime3 := MtxGetTime;

                    startTime4 := MtxGetTime;
                    AVXMatrixNormalize(py1, LineWidthY2, px, LineWidthX, mtxWidth, mtxHeight, False );
                    endTime4 := MtxGetTime;

                    Check(CheckMtxIdx(pY, pY1, LineWidthY, LineWidthY2, mtxWidth, mtxHeight, idx), Format('AVX Matrix normalize column failed on %d, %d @ %d', [mtxWidth, mtxHeight, idx]));

                    FreeMem(pm1);
                    FreeMem(pm2);
                    FreeMem(pm3);

                    Status( Format('%s Big Normalize: %d, %d took %.3fms, %.3fms, %.3fms, %.3fms', [s, mtxWidth, mtxHeight,
                            (endTime1 - startTime1)/mtxFreq*1000, (endTime2 - startTime2)/mtxFreq*1000,
                            (endTime3 - startTime3)/mtxFreq*1000, (endTime4 - startTime4)/mtxFreq*1000]));
               end;
          end;

          s := 'Aligned';
     end;
end;

procedure TestAVXMatrixOperations.TestSQRT;
const cMtxWidth : Array[0..11] of integer = (1, 3, 7, 8, 9, 10, 23, 43, 128, 1024, 2048, 2573);
      cMtxHeight : Array[0..11] of integer = (1, 3, 7, 8, 9, 10, 23, 43, 128, 1024, 2048, 2573);
var pX : PDouble;
    pY : PDouble;
    pm1, pm2 : PByte;
    LineWidthX, LineWidthY : TASMNativeInt;
    startTime1, endTime1 : Int64;
    startTime2, endTime2 : int64;
    i, j, idx : Integer;
    mtxWidth, mtxHeight : integer;
    doAligned : integer;
    s : string;
begin
     s := 'Unaligned';
     for doAligned := 0 to 1 do
     begin
          for i := 0 to Length(cMtxHeight) - 1 do
          begin
               for j := 0 to Length(cMtxWidth) -1  do
               begin
                    mtxWidth := cMtxWidth[j];
                    mtxHeight := cMtxHeight[i];
                    if doAligned = 0 then
                    begin
                         FillUnAlignedMtx( mtxWidth, mtxHeight, pX, pm1, LineWidthX);
                         FillUnAlignedMtx( mtxWidth, mtxHeight, pY, pm2, LineWidthY);
                    end
                    else
                    begin
                        FillAlignedMtx( mtxWidth, mtxHeight, pX, pm1, LineWidthX);
                        FillAlignedMtx( mtxWidth, mtxHeight, pY, pm2, LineWidthY);
                    end;
                    MatrixCopy( pY, LineWidthY, px, LineWidthX, mtxWidth, mtxHeight);

                    startTime1 := MtxGetTime;
                    GenericMtxSqrt(px, LineWidthX, mtxWidth, mtxHeight );
                    endTime1 := MtxGetTime;

                    startTime2 := MtxGetTime;
                    AVXMatrixSQRT(py, LineWidthY, mtxWidth, mtxHeight );
                    endTime2 := MtxGetTime;

                    Check(CheckMtxIdx(pX, pY, LineWidthY, LineWidthY, mtxWidth, mtxHeight, idx), Format('AVX Matrix sqrt failed on %d, %d @ %d', [mtxWidth, mtxHeight, idx]));

                    FreeMem(pm1);
                    FreeMem(pm2);

                    Status( Format('%s Big sqrt: %d, %d took %.3fms, %.3fms', [s, mtxWidth, mtxHeight,
                            (endTime1 - startTime1)/mtxFreq*1000, (endTime2 - startTime2)/mtxFreq*1000]));
               end;
          end;

          s := 'Aligned';
     end;
end;

procedure TestAVXMatrixOperations.TestAVXSum;
const cMtxWidth : Array[0..11] of integer = (1, 3, 7, 8, 9, 10, 23, 43, 128, 1024, 2048, 2573);
      cMtxHeight : Array[0..11] of integer = (1, 3, 7, 8, 9, 10, 23, 43, 128, 1024, 2048, 2573);
var pX : PDouble;
    pY : PDouble;
    pY1 : PDouble;
    pm1, pm2, pm3 : PByte;
    LineWidthX, LineWidthY, LineWidthY2 : TASMNativeInt;
    startTime1, endTime1 : Int64;
    startTime2, endTime2 : int64;
    startTime3, endTime3 : int64;
    startTime4, endTime4 : int64;
    i, j, idx : Integer;
    mtxWidth, mtxHeight : integer;
    doAligned : integer;
    s : string;
begin
     s := 'Unaligned';
     for doAligned := 0 to 1 do
     begin
          for i := 0 to Length(cMtxHeight) - 1 do
          begin
               for j := 0 to Length(cMtxWidth) -1  do
               begin
                    mtxWidth := cMtxWidth[j];
                    mtxHeight := cMtxHeight[i];
                    if doAligned = 0
                    then
                        FillUnAlignedMtx( mtxWidth, mtxHeight, pX, pm1, LineWidthX)
                    else
                        FillAlignedMtx( mtxWidth, mtxHeight, pX, pm1, LineWidthX);

                    AllocAlignedMtx(mtxWidth, 1, pY, pm2, LineWidthY);
                    AllocAlignedMtx(mtxWidth, 1, pY1, pm3, LineWidthY2);

                    startTime1 := MtxGetTime;
                    GenericMtxSum(py, LineWidthY, px, LineWidthX, mtxWidth, mtxHeight, False );
                    endTime1 := MtxGetTime;

                    startTime2 := MtxGetTime;
                    AVXMatrixSum(py1, LineWidthY2, px, LineWidthX, mtxWidth, mtxHeight, False );
                    endTime2 := MtxGetTime;

                    Check(CheckMtxIdx(pY, pY1, LineWidthY, LineWidthY2, mtxWidth, 1, idx), Format('AVX Matrix sum column failed on %d, %d @ %d', [mtxWidth, mtxHeight, idx]));

                    FreeMem(pm3);
                    FreeMem(pm2);

                    AllocAlignedMtx(1, mtxHeight, pY, pm2, LineWidthY);
                    AllocAlignedMtx(1, mtxHeight, pY1, pm3, LineWidthY2);

                    startTime3 := MtxGetTime;
                    GenericMtxSum(py, LineWidthY, px, LineWidthX, mtxWidth, mtxHeight, True );
                    endTime3 := MtxGetTime;

                    startTime4 := MtxGetTime;
                    AVXMatrixSum(py1, LineWidthY2, px, LineWidthX, mtxWidth, mtxHeight, True );
                    endTime4 := MtxGetTime;

                    Check(CheckMtxIdx(pY, pY1, LineWidthY, LineWidthY2, 1, mtxHeight, idx), Format('AVX Matrix sum row failed on %d, %d @ %d', [mtxWidth, mtxHeight, idx]));

                    Status( Format('%s Big sum: %d, %d took %.3fms, %.3fms, %.3fms, %.3fms', [s, mtxWidth, mtxHeight,
                            (endTime1 - startTime1)/mtxFreq*1000, (endTime2 - startTime2)/mtxFreq*1000,
                            (endTime3 - startTime3)/mtxFreq*1000, (endTime4 - startTime4)/mtxFreq*1000]));

                    FreeMem(pm1);
                    FreeMem(pm2);
                    FreeMem(pm3);
               end;
          end;

          s := 'Aligned';
     end;
end;

procedure TestAVXMatrixOperations.TestAVXAdd;
const mt1 : Array[0..15] of double = (0, 1, 2, 0, 3, 4, 5, 0, 6, 7, 8, 0, 0, 0, 0, 0);
      mt2 : Array[0..15] of double = (-1, 0, 1, 0, 2, 3, 4, 0, 5, 6, 7, 0, 0, 0, 0, 0);
      mt3 : Array[0..15] of double = (-1, 1, 3, 0, 5, 7, 9, 0, 11, 13, 15, 0, 0, 0, 0, 0);
      mt4 : Array[0..15] of double = (0, 1, 2, 4, 3, 4, 5, 5, 6, 7, 8, 6, 7, 8, 9, 10);
      mt5 : Array[0..15] of double = (-1, 0, 1, 1, 2, 3, 4, 2, 5, 6, 7, 3, 4, 5, 6, 7);
      mt6 : Array[0..15] of double = (-1, 1, 3, 5, 5, 7, 9, 7, 11, 13, 15, 9, 11, 13, 15, 17);

      cMtxWidth : Array[0..11] of integer = (1, 3, 7, 8, 9, 10, 23, 43, 128, 1024, 2048, 2573);
      cMtxHeight : Array[0..11] of integer = (1, 3, 7, 8, 9, 10, 23, 43, 128, 1024, 2048, 2573);
var dest : Array[0..15] of double;
    pX, pY, pDest, pDest2, pDest3 : PDouble;
    pm1, pm2, pm3, pm4, pm5 : PByte;
    LineWidthX, LineWidthY, destLineWidth, destLn2, destLn3 : TASMNativeInt;
    startTime1, endTime1 : Int64;
    startTime2, endTime2 : int64;
    startTime3, endTime3 : int64;
    startTime4, endTime4 : int64;
    i, j, idx : Integer;
    mtxWidth, mtxHeight : integer;
begin
     FillChar(dest, sizeof(dest), 0);
     AVXMatrixAdd(@dest[0], 4*sizeof(double), @mt4[0], @mt5[0], 4, 4, 4*sizeof(double), 4*sizeof(double));

     Check(CheckMtx(dest, mt6, -1, -1, 1e-10), 'Add Matrix error');

     FillChar(dest, sizeof(dest), 0);
     AVXMatrixAdd(@dest[0], 4*sizeof(double), @mt1[0], @mt2[0], 3, 3, 4*sizeof(double), 4*sizeof(double));

     Check(CheckMtx(dest, mt3, -1, -1, 1e-10), 'Add Matrix error');

     for i := 0 to Length(cMtxHeight) - 1 do
     begin
          for j := 0 to Length(cMtxWidth) -1  do
          begin
               mtxWidth := cMtxWidth[j];
               mtxHeight := cMtxHeight[i];
               FillAlignedMtx( mtxWidth, mtxHeight, pX, pm1, LineWidthX);
               FillAlignedMtx( mtxWidth, mtxHeight, pY, pm2, LineWidthY);
               AllocAlignedMtx(mtxWidth, mtxHeight, pDest, pM3, destLineWidth);
               AllocAlignedMtx(mtxWidth, mtxHeight, pDest2, pM4, destLn2);
               AllocAlignedMtx(mtxWidth, mtxHeight, pDest3, pM5, destLn3);

               startTime1 := MtxGetTime;
               GenericMtxAdd(pDest, destLineWidth, px, py, mtxWidth, mtxHeight, LineWidthX, LineWidthY);
               endTime1 := MtxGetTime;

               startTime2 := MtxGetTime;
               ASMMatrixAdd(pDest2, destLn2, px, py, mtxWidth, mtxHeight, LineWidthX, LineWidthY);
               endTime2 := MtxGetTime;

               startTime3 := MtxGetTime;
               AVXMatrixAddAligned(pDest3, destLn3, px, py, mtxWidth, mtxHeight, LineWidthX, LineWidthY);
               endTime3 := MtxGetTime;

               Check(CheckMtxIdx(pDest, pDest2, destLineWidth, destLn2, mtxWidth, mtxHeight, idx), 'ASM Add failed');
               Check(CheckMtxIdx(pDest, pDest3, destLineWidth, destLn3, mtxWidth, mtxHeight, idx), 'AVX Add failed');

               startTime4 := MtxGetTime;
               AVXMatrixAddUnAligned(pDest3, destLn3, px, py, mtxWidth, mtxHeight, LineWidthX, LineWidthY);
               endTime4 := MtxGetTime;

               Check(CheckMtxIdx(pDest, pDest3, destLineWidth, destLn3, mtxWidth, mtxHeight, idx), 'AVX Add failed');
               Status( Format('Big Add: %d, %d took %.3fms, %.3fms, %.3fms, %.3fms', [mtxWidth, mtxHeight,
                  (endTime1 - startTime1)/mtxFreq*1000, (endTime2 - startTime2)/mtxFreq*1000,
                  (endTime3 - startTime3)/mtxFreq*1000, (endTime4 - startTime4)/mtxFreq*1000]));


               FreeMem(pm1);
               FreeMem(pm2);
               FreeMem(pm3);
               FreeMem(pm4);
               FreeMem(pm5);
          end;
     end;
end;

procedure TestAVXMatrixOperations.TestAVXSub;
const cMtxWidth : Array[0..11] of integer = (1, 3, 7, 8, 9, 10, 23, 43, 128, 1024, 2048, 2573);
      cMtxHeight : Array[0..11] of integer = (1, 3, 7, 8, 9, 10, 23, 43, 128, 1024, 2048, 2573);
var pX, pY, pDest, pDest2, pDest3 : PDouble;
    pm1, pm2, pm3, pm4, pm5 : PByte;
    LineWidthX, LineWidthY, destLineWidth, destLn2, destLn3 : TASMNativeInt;
    startTime1, endTime1 : Int64;
    startTime2, endTime2 : int64;
    startTime3, endTime3 : int64;
    startTime4, endTime4 : int64;
    i, j, idx : Integer;
    mtxWidth, mtxHeight : integer;
begin
     for i := 0 to Length(cMtxHeight) - 1 do
     begin
         for j := 0 to Length(cMtxWidth) -1  do
         begin
              mtxWidth := cMtxWidth[j];
              mtxHeight := cMtxHeight[i];
              FillAlignedMtx( mtxWidth, mtxHeight, pX, pm1, LineWidthX);
              FillAlignedMtx( mtxWidth, mtxHeight, pY, pm2, LineWidthY);
              AllocAlignedMtx(mtxWidth, mtxHeight, pDest, pM3, destLineWidth);
              AllocAlignedMtx(mtxWidth, mtxHeight, pDest2, pM4, destLn2);
              AllocAlignedMtx(mtxWidth, mtxHeight, pDest3, pM5, destLn3);

              startTime1 := MtxGetTime;
              GenericMtxSub(pDest, destLineWidth, px, py, mtxWidth, mtxHeight, LineWidthX, LineWidthY);
              endTime1 := MtxGetTime;

              startTime2 := MtxGetTime;
              ASMMatrixSub(pDest2, destLn2, px, py, mtxWidth, mtxHeight, LineWidthX, LineWidthY);
              endTime2 := MtxGetTime;

              startTime3 := MtxGetTime;
              AVXMatrixSubAligned(pDest3, destLn3, px, py, mtxWidth, mtxHeight, LineWidthX, LineWidthY);
              endTime3 := MtxGetTime;

              Check(CheckMtxIdx(pDest, pDest2, destLineWidth, destLn2, mtxWidth, mtxHeight, idx), 'ASM Sub failed');
              Check(CheckMtxIdx(pDest, pDest3, destLineWidth, destLn3, mtxWidth, mtxHeight, idx), 'AVX Sub failed');

              startTime4 := MtxGetTime;
              AVXMatrixSubUnAligned(pDest3, destLn3, px, py, mtxWidth, mtxHeight, LineWidthX, LineWidthY);
              endTime4 := MtxGetTime;

              Check(CheckMtxIdx(pDest, pDest3, destLineWidth, destLn3, mtxWidth, mtxHeight, idx), 'AVX Add failed');
              Status( Format('Big Sub: %d, %d took %.3fms, %.3fms, %.3fms, %.3fms', [mtxWidth, mtxHeight,
                 (endTime1 - startTime1)/mtxFreq*1000, (endTime2 - startTime2)/mtxFreq*1000,
                 (endTime3 - startTime3)/mtxFreq*1000, (endTime4 - startTime4)/mtxFreq*1000]));

              FreeMem(pm1);
              FreeMem(pm2);
              FreeMem(pm3);
              FreeMem(pm4);
              FreeMem(pm5);
         end;
     end;
end;

procedure TestAVXMatrixOperations.TestAVXSubVec;
const cMtxWidth : Array[0..11] of integer = (1, 3, 7, 8, 9, 10, 23, 43, 128, 1024, 2048, 2573);
var pX, pY, pDest, pDest2, pDest3 : PDouble;
    pm1, pm2, pm3, pm4, pm5 : PByte;
    LineWidthX, LineWidthY, destLineWidth, destLn2, destLn3 : TASMNativeInt;
    startTime1, endTime1 : Int64;
    startTime2, endTime2 : int64;
    startTime3, endTime3 : int64;
    j, idx : Integer;
    mtxWidth, mtxHeight : integer;
begin
     for j := 0 to Length(cMtxWidth) -1  do
     begin
          mtxWidth := cMtxWidth[j];
          mtxHeight := cMtxWidth[j];
          FillAlignedMtx( mtxWidth, mtxHeight, pX, pm1, LineWidthX);
          FillAlignedMtx( mtxWidth, mtxHeight, pY, pm2, LineWidthY);
          AllocAlignedMtx(mtxWidth, mtxHeight, pDest, pM3, destLineWidth);
          AllocAlignedMtx(mtxWidth, mtxHeight, pDest2, pM4, destLn2);
          AllocAlignedMtx(mtxWidth, mtxHeight, pDest3, pM5, destLn3);

          GenericMtxCopy(pDest, destLinewidth, px, LineWidthX, mtxWidth, mtxHeight);
          startTime1 := MtxGetTime;
          GenericSubVec(pDest, destLineWidth, py, sizeof(double), mtxWidth, mtxHeight, True);
          endTime1 := MtxGetTime;

          GenericMtxCopy(pDest2, destLn2, px, LineWidthX, mtxWidth, mtxHeight);
          startTime2 := MtxGetTime;
          ASMMatrixSubVec(pDest2, destLn2, py, sizeof(double), mtxWidth, mtxHeight, True);
          endTime2 := MtxGetTime;

          GenericMtxCopy(pDest3, destLn3, px, LineWidthX, mtxWidth, mtxHeight);
          startTime3 := MtxGetTime;
          AVXMatrixSubVec(pDest3, destLn3, py, sizeof(double), mtxWidth, mtxHeight, True);
          endTime3 := MtxGetTime;

          Check(CheckMtxIdx(pDest, pDest2, destLineWidth, destLn2, mtxWidth, mtxHeight, idx), 'ASM Sub vec failed');
          Check(CheckMtxIdx(pDest, pDest3, destLineWidth, destLn3, mtxWidth, mtxHeight, idx), 'AVX Sub vec failed');

          Status( Format('SubVec Vec: %d, %d took %.3fms, %.3fms, %.3fms', [mtxWidth, mtxHeight,
             (endTime1 - startTime1)/mtxFreq*1000, (endTime2 - startTime2)/mtxFreq*1000,
             (endTime3 - startTime3)/mtxFreq*1000]));

          GenericMtxCopy(pDest, destLinewidth, px, LineWidthX, mtxWidth, mtxHeight);
          startTime1 := MtxGetTime;
          GenericSubVec(pDest, destLineWidth, py, LineWidthY, mtxWidth, mtxHeight, True);
          endTime1 := MtxGetTime;

          GenericMtxCopy(pDest2, destLn2, px, LineWidthX, mtxWidth, mtxHeight);
          startTime2 := MtxGetTime;
          ASMMatrixSubVec(pDest2, destLn2, py, LineWidthY, mtxWidth, mtxHeight, True);
          endTime2 := MtxGetTime;

          GenericMtxCopy(pDest3, destLn3, px, LineWidthX, mtxWidth, mtxHeight);
          startTime3 := MtxGetTime;
          AVXMatrixSubVec(pDest3, destLn3, py, LineWidthY, mtxWidth, mtxHeight, True);
          endTime3 := MtxGetTime;

          Check(CheckMtxIdx(pDest, pDest2, destLineWidth, destLn2, mtxWidth, mtxHeight, idx), 'ASM Sub failed');
          Check(CheckMtxIdx(pDest, pDest3, destLineWidth, destLn3, mtxWidth, mtxHeight, idx), 'AVX Sub failed');

          Status( Format('SubVec Vec row: %d, %d took %.3fms, %.3fms, %.3fms', [mtxWidth, mtxHeight,
             (endTime1 - startTime1)/mtxFreq*1000, (endTime2 - startTime2)/mtxFreq*1000,
             (endTime3 - startTime3)/mtxFreq*1000]));

          GenericMtxCopy(pDest, destLinewidth, px, LineWidthX, mtxWidth, mtxHeight);
          startTime1 := MtxGetTime;
          GenericSubVec(pDest, destLineWidth, py, sizeof(double), mtxWidth, mtxHeight, False);
          endTime1 := MtxGetTime;

          GenericMtxCopy(pDest2, destLn2, px, LineWidthX, mtxWidth, mtxHeight);
          startTime2 := MtxGetTime;
          ASMMatrixSubVec(pDest2, destLn2, py, sizeof(double), mtxWidth, mtxHeight, False);
          endTime2 := MtxGetTime;

          GenericMtxCopy(pDest3, destLn3, px, LineWidthX, mtxWidth, mtxHeight);
          startTime3 := MtxGetTime;
          AVXMatrixSubVec(pDest3, destLn3, py, sizeof(double), mtxWidth, mtxHeight, False);
          endTime3 := MtxGetTime;

          Check(CheckMtxIdx(pDest, pDest2, destLineWidth, destLn2, mtxWidth, mtxHeight, idx), 'ASM Sub failed');
          Check(CheckMtxIdx(pDest, pDest3, destLineWidth, destLn3, mtxWidth, mtxHeight, idx), 'AVX Sub failed');

          Status( Format('SubVec Vec col: %d, %d took %.3fms, %.3fms, %.3fms', [mtxWidth, mtxHeight,
             (endTime1 - startTime1)/mtxFreq*1000, (endTime2 - startTime2)/mtxFreq*1000,
             (endTime3 - startTime3)/mtxFreq*1000]));

          FreeMem(pm1);
          FreeMem(pm2);
          FreeMem(pm3);
          FreeMem(pm4);
          FreeMem(pm5);
     end;
end;

procedure TestAVXMatrixOperations.TestAVXAddVec;
const cMtxWidth : Array[0..11] of integer = (1, 3, 7, 8, 9, 10, 23, 43, 128, 1024, 2048, 2573);
var pX, pY, pDest, pDest2, pDest3 : PDouble;
    pm1, pm2, pm3, pm4, pm5 : PByte;
    LineWidthX, LineWidthY, destLineWidth, destLn2, destLn3 : TASMNativeInt;
    startTime1, endTime1 : Int64;
    startTime2, endTime2 : int64;
    startTime3, endTime3 : int64;
    j, idx : Integer;
    mtxWidth, mtxHeight : integer;
begin
     for j := 0 to Length(cMtxWidth) -1  do
     begin
          mtxWidth := cMtxWidth[j];
          mtxHeight := cMtxWidth[j];
          FillAlignedMtx( mtxWidth, mtxHeight, pX, pm1, LineWidthX);
          FillAlignedMtx( mtxWidth, mtxHeight, pY, pm2, LineWidthY);
          AllocAlignedMtx(mtxWidth, mtxHeight, pDest, pM3, destLineWidth);
          AllocAlignedMtx(mtxWidth, mtxHeight, pDest2, pM4, destLn2);
          AllocAlignedMtx(mtxWidth, mtxHeight, pDest3, pM5, destLn3);

          GenericMtxCopy(pDest, destLinewidth, px, LineWidthX, mtxWidth, mtxHeight);
          startTime1 := MtxGetTime;
          GenericAddVec(pDest, destLineWidth, py, sizeof(double), mtxWidth, mtxHeight, True);
          endTime1 := MtxGetTime;

          GenericMtxCopy(pDest2, destLn2, px, LineWidthX, mtxWidth, mtxHeight);
          startTime2 := MtxGetTime;
          ASMMatrixAddVec(pDest2, destLn2, py, sizeof(double), mtxWidth, mtxHeight, True);
          endTime2 := MtxGetTime;

          GenericMtxCopy(pDest3, destLn3, px, LineWidthX, mtxWidth, mtxHeight);
          startTime3 := MtxGetTime;
          AVXMatrixAddVec(pDest3, destLn3, py, sizeof(double), mtxWidth, mtxHeight, True);
          endTime3 := MtxGetTime;

          Check(CheckMtxIdx(pDest, pDest2, destLineWidth, destLn2, mtxWidth, mtxHeight, idx), 'ASM Sub vec failed');
          Check(CheckMtxIdx(pDest, pDest3, destLineWidth, destLn3, mtxWidth, mtxHeight, idx), 'AVX Sub vec failed');

          Status( Format('AddVec Vec: %d, %d took %.3fms, %.3fms, %.3fms', [mtxWidth, mtxHeight,
             (endTime1 - startTime1)/mtxFreq*1000, (endTime2 - startTime2)/mtxFreq*1000,
             (endTime3 - startTime3)/mtxFreq*1000]));

          GenericMtxCopy(pDest, destLinewidth, px, LineWidthX, mtxWidth, mtxHeight);
          startTime1 := MtxGetTime;
          GenericAddVec(pDest, destLineWidth, py, LineWidthY, mtxWidth, mtxHeight, True);
          endTime1 := MtxGetTime;

          GenericMtxCopy(pDest2, destLn2, px, LineWidthX, mtxWidth, mtxHeight);
          startTime2 := MtxGetTime;
          ASMMatrixAddVec(pDest2, destLn2, py, LineWidthY, mtxWidth, mtxHeight, True);
          endTime2 := MtxGetTime;

          GenericMtxCopy(pDest3, destLn3, px, LineWidthX, mtxWidth, mtxHeight);
          startTime3 := MtxGetTime;
          AVXMatrixAddVec(pDest3, destLn3, py, LineWidthY, mtxWidth, mtxHeight, True);
          endTime3 := MtxGetTime;

          Check(CheckMtxIdx(pDest, pDest2, destLineWidth, destLn2, mtxWidth, mtxHeight, idx), 'ASM Sub failed');
          Check(CheckMtxIdx(pDest, pDest3, destLineWidth, destLn3, mtxWidth, mtxHeight, idx), 'AVX Sub failed');

          Status( Format('AddVec Vec col: %d, %d took %.3fms, %.3fms, %.3fms', [mtxWidth, mtxHeight,
             (endTime1 - startTime1)/mtxFreq*1000, (endTime2 - startTime2)/mtxFreq*1000,
             (endTime3 - startTime3)/mtxFreq*1000]));

          GenericMtxCopy(pDest, destLinewidth, px, LineWidthX, mtxWidth, mtxHeight);
          startTime1 := MtxGetTime;
          GenericAddVec(pDest, destLineWidth, py, sizeof(double), mtxWidth, mtxHeight, False);
          endTime1 := MtxGetTime;

          GenericMtxCopy(pDest2, destLn2, px, LineWidthX, mtxWidth, mtxHeight);
          startTime2 := MtxGetTime;
          ASMMatrixAddVec(pDest2, destLn2, py, sizeof(double), mtxWidth, mtxHeight, False);
          endTime2 := MtxGetTime;

          GenericMtxCopy(pDest3, destLn3, px, LineWidthX, mtxWidth, mtxHeight);
          startTime3 := MtxGetTime;
          AVXMatrixAddVec(pDest3, destLn3, py, sizeof(double), mtxWidth, mtxHeight, False);
          endTime3 := MtxGetTime;

          Check(CheckMtxIdx(pDest, pDest2, destLineWidth, destLn2, mtxWidth, mtxHeight, idx), 'ASM Sub failed');
          Check(CheckMtxIdx(pDest, pDest3, destLineWidth, destLn3, mtxWidth, mtxHeight, idx), 'AVX Sub failed');

          Status( Format('AddVec Vec col: %d, %d took %.3fms, %.3fms, %.3fms', [mtxWidth, mtxHeight,
             (endTime1 - startTime1)/mtxFreq*1000, (endTime2 - startTime2)/mtxFreq*1000,
             (endTime3 - startTime3)/mtxFreq*1000]));

          FreeMem(pm1);
          FreeMem(pm2);
          FreeMem(pm3);
          FreeMem(pm4);
          FreeMem(pm5);
     end;
end;

procedure TestAVXMatrixOperations.TestAVXSubT;
const mt1 : Array[0..5] of double = (0, 1, 2, 3, 4, 5);
      mt2 : Array[0..5] of double = (1, 2, -1, 3, -12, -1);
var res, res1 : TDoubleDynArray;
begin
     SetLength(res, Length(mt1));
     Move(mt1, res[0], sizeof(mt1));
     res1 := copy(res, 0, Length(res));

     GenericMatrixSubT(@res[0], 3*sizeof(double), @mt2, 2*sizeof(double), 3, 2);
     AVXMatrixSubT(@res1[0], 3*sizeof(double), @mt2, 2*sizeof(double), 3, 2);
     Check(CheckMtx(res, res1), 'Error ASM subT');
end;

procedure TestAVXMatrixOperations.TestAVXMultMod16;
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
     AVXMatrixMultAlignedEvenW1EvenH2TransposedMod16(p4, cMtxWidth*sizeof(double), p1, p2, 16, 16, 16, 16, 16*sizeof(double), cMtxWidth*sizeof(double));

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
     AVXMatrixMultAlignedEvenW1OddH2TransposedMod16(p4, cMtxWidth*sizeof(double), p1, p2, 32, 32, 32, 11, 32*sizeof(double), cMtxWidth*sizeof(double));

     res := CheckMtxIdx(p3, p4, cMtxSize, outidx, 1e-6);
     Check(res, '32x11 Error Matrix mult failed @' + IntToStr(outidx));

     start1 := MtxGetTime;
     GenericMtxMultTransp(p3, cMtxWidth*sizeof(double), p1, p2, cMtxWidth, cMtxWidth, cMtxWidth, cMtxWidth, cMtxWidth*sizeof(double), cMtxWidth*sizeof(double));
     stop1 := MtxGetTime;

     start2 := MtxGetTime;
     ASMMatrixMultAlignedEvenW1EvenH2TransposedMod16(p4, cMtxWidth*sizeof(double), p1, p2, cMtxWidth, cMtxWidth, cMtxWidth, cMtxWidth, cMtxWidth*sizeof(double), cMtxWidth*sizeof(double));
     stop2 := MtxGetTime;

     start3 := MtxGetTime;
     AVXMatrixMultAlignedEvenW1EvenH2TransposedMod16(p5, cMtxWidth*sizeof(double), p1, p2, cMtxWidth, cMtxWidth, cMtxWidth, cMtxWidth, cMtxWidth*sizeof(double), cMtxWidth*sizeof(double));
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

procedure TestAVXMatrixOperations.TestAVXMultTransposed;
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
          AVXMatrixMultUnAlignedTransposed(p4, mtxWidth*sizeof(double), p1, p2, mtxWidth, mtxWidth, mtxWidth, mtxWidth, mtxWidth*sizeof(double), mtxWidth*sizeof(double));
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
          AVXMatrixMultAlignedTransposed(p4, LineWidth4, p1, p2, mtxWidth, mtxWidth, mtxWidth, mtxWidth, LineWidth1, LineWidth2);
          stop2 := MtxGetTime;

          Check(CheckMtxIdx(p3, p4, LineWidth3, LineWidth4, mtxWidth, mtxWidth, outidx, 1e-6), 'AVX Transposed Error Matrix mult failed');

          Status(Format('mtxWidth %d: %.2f, %.2f', [mtxWidth, (stop1 - start1)/mtxFreq*1000, (stop2 - start2)/mtxFreq*1000]));

          FreeMem(pMem1);
          FreeMem(pMem2);
          FreeMem(pMem3);
          FreeMem(pMem4);
     end;
end;


procedure TestAVXMatrixOperations.TestBlkThreadedMatrixMultT2;
const cMtxWidth : Array[0..5] of integer = (12, 128, 513, 777, 2043, 2048);
      cMtxHeight : Array[0..5] of integer = (13, 128, 343, 768, 2192, 2048);
      //cMtxWidth : Array[0..0] of integer = (2043);
      //cMtxHeight : Array[0..0] of integer = (2192);
var aMt1, aMt2 : PDouble;
    aMem1, aMem2 : PByte;
    aDest1, aDest2 : PDouble;
    aMem3, aMem4 : PByte;
    aMt1LineWidth, aMt2LineWidth : TASMNativeInt;
    aDestLineWidth1, aDestLineWidth2 : TASMNativeInt;
    start1, start2 : Int64;
    end1, end2 : int64;
    idx : integer;
    i : Integer;
    isSame : boolean;
    errOccured : boolean;
begin
     InitMathFunctions(itAVX, False);
     MtxThreadPool.InitMtxThreadPool;

     errOccured := False;
     for i := 0 to Length(cMtxWidth) - 1 do
     begin
          FillAlignedMtx( cMtxWidth[i], cMtxHeight[i], aMt1, aMem1, aMt1LineWidth);
          FillAlignedMtx( cMtxWidth[i], cMtxHeight[i], aMt2, aMem2, aMt2LineWidth);

          AllocAlignedMtx( cMtxHeight[i], cMtxHeight[i], aDest1, amem3, aDestLineWidth1);
          AllocAlignedMtx( cMtxHeight[i], cMtxHeight[i], aDest2, amem4, aDestLineWidth2);

          start1 := MtxGetTime;
          ThrBlockMatrixMultiplicationT2(aDest1, aDestLineWidth1, aMt1, aMt2, cMtxWidth[i], cMtxHeight[i], cMtxWidth[i], cMtxHeight[i],
                                         aMt1LineWidth, aMt2LineWidth, BlockMatrixCacheSize, doNone, nil);
          end1 := MtxGetTime;

          start2 := MtxGetTime;
          ThrMatrixMultT2Ex(aDest2, aDestLineWidth2, aMt1, aMt2, cMtxWidth[i], cMtxHeight[i], cMtxWidth[i], cMtxHeight[i],
                            aMt1LineWidth, aMt2LineWidth, BlockMatrixCacheSize, doNone, nil);
          end2 := MtxGetTime;

          Status( Format('%d x %d Mult took: %.3fms, %.3fms', [ cMtxWidth[i], cMtxHeight[i], (end1 - start1)*1000/mtxFreq, (end2 - start2)*1000/mtxFreq] ) );

          isSame := CheckMtxIdx(aDest1, aDest2, aDestLineWidth1, aDestLineWidth2, cMtxHeight[i], cMtxHeight[i], idx);
          errOccured := errOccured or (not isSame);
          Status( 'Same: ' +  BoolToStr( isSame ));
          if not isSame then
             Status( 'Problem at: ' + IntToStr(idx));

          FreeMem(aMem1);
          FreeMem(aMem2);
          FreeMem(aMem3);
          FreeMem(aMem4);
     end;
     MtxThreadPool.FinalizeMtxThreadPool;

     Check(not errOccured, 'Problem with threaded matrix multiplication');
end;

procedure TestAVXMatrixOperations.TestBlkThreadedMatrixMult;
const cMtxWidth : Array[0..5] of integer = (12, 128, 513, 777, 2043, 2048);
      cMtxHeight : Array[0..5] of integer = (13, 128, 343, 768, 2192, 2048);
      //cMtxWidth : Array[0..0] of integer = (513);
      //cMtxHeight : Array[0..0] of integer = (343);
var aMt1, aMt2 : PDouble;
    aMem1, aMem2 : PByte;
    aDest1, aDest2 : PDouble;
    aMem3, aMem4 : PByte;
    aMt1LineWidth, aMt2LineWidth : TASMNativeInt;
    aDestLineWidth1, aDestLineWidth2 : TASMNativeInt;
    start1, start2 : Int64;
    end1, end2 : int64;
    idx : integer;
    i : Integer;
    isSame : boolean;
    errOccured : boolean;
begin
     InitMathFunctions(itAVX, False);
     MtxThreadPool.InitMtxThreadPool;

     errOccured := False;
     for i := 0 to Length(cMtxWidth) - 1 do
     begin
          FillAlignedMtx( cMtxWidth[i], cMtxHeight[i], aMt1, aMem1, aMt1LineWidth);
          FillAlignedMtx( cMtxHeight[i], cMtxWidth[i], aMt2, aMem2, aMt2LineWidth);

          AllocAlignedMtx( cMtxHeight[i], cMtxHeight[i], aDest1, amem3, aDestLineWidth1);
          AllocAlignedMtx( cMtxHeight[i], cMtxHeight[i], aDest2, amem4, aDestLineWidth2);

          start1 := MtxGetTime;
          ThrBlockMatrixMultiplication(aDest1, aDestLineWidth1, aMt1, aMt2, cMtxWidth[i], cMtxHeight[i], cMtxHeight[i], cMtxWidth[i],
                                         aMt1LineWidth, aMt2LineWidth, BlockMatrixCacheSize, doNone, nil);
          end1 := MtxGetTime;

          start2 := MtxGetTime;
          ThrMatrixMultEx(aDest2, aDestLineWidth2, aMt1, aMt2, cMtxWidth[i], cMtxHeight[i], cMtxHeight[i], cMtxWidth[i],
                            aMt1LineWidth, aMt2LineWidth, BlockMatrixCacheSize, doNone, nil);
          end2 := MtxGetTime;

          Status( Format('%d x %d Mult took: %.3fms, %.3fms', [ cMtxWidth[i], cMtxHeight[i], (end1 - start1)*1000/mtxFreq, (end2 - start2)*1000/mtxFreq] ) );

          isSame := CheckMtxIdx(aDest1, aDest2, aDestLineWidth1, aDestLineWidth2, cMtxHeight[i], cMtxHeight[i], idx);
          errOccured := errOccured or (not isSame);
          Status( 'Same: ' +  BoolToStr( isSame ));
          if not isSame then
             Status( 'Problem at: ' + IntToStr(idx));

          FreeMem(aMem1);
          FreeMem(aMem2);
          FreeMem(aMem3);
          FreeMem(aMem4);
     end;

     MtxThreadPool.FinalizeMtxThreadPool;

     Check(not errOccured, 'Problem with threaded matrix multiplication');
end;


procedure TestAVXMatrixOperations.TestAVXMult;
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
     AVXMatrixMultUnaligned(@dest[0], 4*sizeof(double), @mt1[0], @mt2[0], 3, 3, 3, 3, 4*sizeof(double), 4*sizeof(double));

     Check(CheckMtx(dest, mt3, -1, -1, 1e-10), 'Mult Matrix error');

     FillChar(dest, sizeof(dest), 0);
     AVXMatrixMultUnaligned(@dest[0], 4*sizeof(double), @mt4[0], @mt5[0], 4, 4, 4, 4, 4*sizeof(double), 4*sizeof(double));

     Check(CheckMtx(dest, mt6, -1, -1, 1e-10), 'Mult Matrix error');

     FillMatrix(43*43, x, y, px, py);
     SetLength( d, 43*43 );
     SetLength(d1, 43*43 );

     FreeMem(px);
     FreeMem(py);

     GenericMtxMult( @d1[0], 43*sizeof(double), @x[0], @y[0], 43, 43, 43, 43, 43*sizeof(double), 43*sizeof(double));
     AVXMatrixMult( @d[0], 43*sizeof(double), @x[0], @y[0], 43, 43, 43, 43, 43*sizeof(double), 43*sizeof(double), nil);

     Check(CheckMtx(d1, d, -1, -1, 1e-10), 'Mtx mult failed');
end;

procedure TestAVXMatrixOperations.TestAVXBigMult;
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
     AVXMatrixMultUnAligned(@dest2[0], cMtxLineWidth, @x[0], @y[0], cMtxWidth, cMtxheight, cMtxHeight, cMtxWidth, cMtxLinewidth, cMtxLinewidth2);
     endTime2 := MtxGetTime;

     startTime3 := MtxGetTime;
     AVXMatrixMultAligned(dest2a, cMtxLineWidth, xa, ya, cMtxWidth, cMtxheight, cMtxHeight, cMtxWidth, cMtxLinewidth, cMtxLinewidth2);
     endTime3 := MtxGetTime;

     startTime4 := MtxGetTime;
     GenericMtxTranspose(za, cMtxLineWidth, ya, cMtxLineWidth2, cMtxHeight, cMtxWidth);
     AVXMatrixMultTransposed(dest3a, cMtxLineWidth, xa, za, cMtxWidth, cMtxheight, cMtxWidth, cMtxheight, cMtxLinewidth, cMtxLinewidth);
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

procedure TestAVXMatrixOperations.TestAVXDiagMtxMult;
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
               AVXMtxMultTria2T1StoreT1(@a1[0], h*sizeof(double), @b[0], h*sizeof(double), w, h, w, w);
               Check(CheckMtx(a, a1), 'AVXMtxMultTria2T1StoreT1');
          end;
     end;


     for w := 3 to 12 do
     begin
          for h := w to 12 do
          begin
               Init;
               GenericMtxMultTria2Store1(@a[0], h*sizeof(double), @b[0], h*sizeof(double), w, h, w, w);
               AVXMtxMultTria2Store1(@a1[0], h*sizeof(double), @b[0], h*sizeof(double), w, h, w, w);
               Check(CheckMtx(a, a1), 'AVXMtxMultTria2T1StoreT1');
          end;
     end;

     for w := 3 to 12 do
     begin
          for h := w to 12 do
          begin
               Init;
               GenericMtxMultTria2TUpperUnit(@c[0], h*sizeof(double), @a[0], h*sizeof(double), @b[0], h*sizeof(double), w, h, w, w);
               AVXMtxMultTria2TUpperUnit(@c1[0], h*sizeof(double), @a1[0], h*sizeof(double), @b[0], h*sizeof(double), w, h, w, w);
               Check(CheckMtx(c, c1), 'AVXMtxMultTria2TUpperUnit');
          end;
     end;

     for w := 3 to 12 do
     begin
          for h := w to 12 do
          begin
               Init;
               GenericMtxMultTria2Store1Unit(@a[0], h*sizeof(double), @b[0], h*sizeof(double), w, h, w, w);
               AVXMtxMultTria2Store1Unit(@a1[0], h*sizeof(double), @b[0], h*sizeof(double), w, h, w, w );

               Check(CheckMtx(a, a1), 'AVXMtxMultTria2Store1Unit');
          end;
     end;
end;

procedure TestAVXMatrixOperations.TestAVXElementWiseDiv;
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
     AVXMatrixElemDiv(PDouble(@mtx[0, 0]), sizeof(double), PDouble(@mtx[0, 0]), PDouble(@mtx[0, 0]), 1, 1, sizeof(double), sizeof(double));
     Check(SameValue(mtx[0, 0], 1), 'Error 1x1 elementwise mult failed');

     mtx[0, 0] := -2;
     AVXMatrixElemDiv(PDouble(@mtx[0, 0]), 2*sizeof(double), PDouble(@mtx[0, 0]), PDouble(@mtx[0, 0]), 1, 1, 2*sizeof(double), 2*sizeof(double));
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
     AVXMatrixElemDiv(PDouble(@mtx[0, 0]), 2*sizeof(double), PDouble(@mtx[0, 0]), PDouble(@mtx[0, 0]), 2, 2, 2*sizeof(double), 2*sizeof(double));
     Check(SameValue(mtx[0, 0], 1) and SameValue(mtx[0, 1], 1), 'Error 2x2 elementwise asm mult even LineSize failed');
     Check(SameValue(mtx[0, 2], 1) and SameValue(mtx[0, 3], 1), 'Error 2x2 elementwise asm mult even LineSize failed');


     mtx[0, 0] := -2;
     mtx[0, 1] := -2;
     mtx[0, 3] := -2;
     mtx[0, 4] := -2;
     AVXMatrixElemDiv(PDouble(@mtx[0, 0]), 3*sizeof(double), PDouble(@mtx[0, 0]), PDouble(@mtx[0, 0]), 2, 2, 3*sizeof(double), 3*sizeof(double));
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
     AVXMatrixElemDiv(PDouble(@mtx[0, 0]), 3*sizeof(double), PDouble(@mtx[0, 0]), PDouble(@mtx[0, 0]), 3, 2, 3*sizeof(double), 3*sizeof(double));
     Check(SameValue(mtx[0, 0], 1) and SameValue(mtx[0, 1], 1), 'Error 3x2 elementwise asm mult failed');
     Check(SameValue(mtx[0, 2], 1) and SameValue(mtx[0, 3], 1), 'Error 3x2 elementwise asm mult failed');
     Check(SameValue(mtx[0, 1], 1) and SameValue(mtx[0, 5], 1), 'Error 3x2 elementwise asm mult failed');

     mtx[0, 0] := -2;
     mtx[0, 1] := -2;
     mtx[0, 2] := -2;
     mtx[0, 3] := -2;
     mtx[0, 4] := -2;
     mtx[0, 5] := -2;
     AVXMatrixElemDiv(PDouble(@mtx[0, 0]), 2*sizeof(double), PDouble(@mtx[0, 0]), PDouble(@mtx[0, 0]), 2, 3, 2*sizeof(double), 2*sizeof(double));
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

     AVXMatrixElemDiv(PDouble(@mtx[0, 0]), 123*sizeof(double), PDouble(@mtx[0, 0]), PDouble(@mtx[0, 0]), 123, 124, 123*sizeof(double), 123*sizeof(double));

     for x := 0 to 122 do
         for y := 0 to 123 do
             Check(SameValue(mtx[x, y], 1), 'Error big elementwise asm mult failed @' + IntToStr(x) + ',' + IntToStr(y));
end;

procedure TestAVXMatrixOperations.TestAVXElemMult;
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
     AVXMatrixElemMult(PDouble(@mtx[0, 0]), sizeof(double), PDouble(@mtx[0, 0]), PDouble(@mtx[0, 0]), 1, 1, sizeof(double), sizeof(double));
     Check(SameValue(mtx[0, 0], 4), 'Error 1x1 elementwise mult failed');

     mtx[0, 0] := -2;
     AVXMatrixElemMult(PDouble(@mtx[0, 0]), 2*sizeof(double), PDouble(@mtx[0, 0]), PDouble(@mtx[0, 0]), 1, 1, 2*sizeof(double), 2*sizeof(double));
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
     AVXMatrixElemMult(PDouble(@mtx[0, 0]), 2*sizeof(double), PDouble(@mtx[0, 0]), PDouble(@mtx[0, 0]), 2, 2, 2*sizeof(double), 2*sizeof(double));
     Check(SameValue(mtx[0, 0], 4) and SameValue(mtx[0, 1], 4), 'Error 2x2 elementwise asm mult even LineSize failed');
     Check(SameValue(mtx[0, 2], 4) and SameValue(mtx[0, 3], 4), 'Error 2x2 elementwise asm mult even LineSize failed');


     mtx[0, 0] := -2;
     mtx[0, 1] := -2;
     mtx[0, 3] := -2;
     mtx[0, 4] := -2;
     AVXMatrixElemMult(PDouble(@mtx[0, 0]), 3*sizeof(double), PDouble(@mtx[0, 0]), PDouble(@mtx[0, 0]), 2, 2, 3*sizeof(double), 3*sizeof(double));
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
     AVXMatrixElemMult(PDouble(@mtx[0, 0]), 3*sizeof(double), PDouble(@mtx[0, 0]), PDouble(@mtx[0, 0]), 3, 2, 3*sizeof(double), 3*sizeof(double));
     Check(SameValue(mtx[0, 0], 4) and SameValue(mtx[0, 1], 4), 'Error 3x2 elementwise asm mult failed');
     Check(SameValue(mtx[0, 2], 4) and SameValue(mtx[0, 3], 4), 'Error 3x2 elementwise asm mult failed');
     Check(SameValue(mtx[0, 4], 4) and SameValue(mtx[0, 5], 4), 'Error 3x2 elementwise asm mult failed');

     mtx[0, 0] := -2;
     mtx[0, 1] := -2;
     mtx[0, 2] := -2;
     mtx[0, 3] := -2;
     mtx[0, 4] := -2;
     mtx[0, 5] := -2;
     AVXMatrixElemMult(PDouble(@mtx[0, 0]), 2*sizeof(double), PDouble(@mtx[0, 0]), PDouble(@mtx[0, 0]), 2, 3, 2*sizeof(double), 2*sizeof(double));
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

     AVXMatrixElemMult(PDouble(@mtx[0, 0]), 123*sizeof(double), PDouble(@mtx[0, 0]), PDouble(@mtx[0, 0]), 123, 124, 123*sizeof(double), 123*sizeof(double));

     for x := 0 to 122 do
         for y := 0 to 123 do
             Check(SameValue(mtx[x, y], 4), 'Error big elementwise asm mult failed @' + IntToStr(x) + ',' + IntToStr(y));
end;


procedure TestAVXMatrixOperations.TestAVXVecMult;
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
     AVXMatrixVectMultT(@dest[0], sizeof(double), @cMtx[0], @cV[0], 3*sizeof(double), sizeof(double), 3, 3, 0.1, -1);

     Check(CheckMtx(dest1, dest), 'Error matrix vector multiplication failed');

     Move(dest, dest1, sizeof(dest));
     GenericMtxVecMultT(@dest1[0], sizeof(double), @cMtx[0], @cV[0], 3*sizeof(double), sizeof(double), 3, 3, 0.1, -1);
     AVXMatrixVecMultTDestVec(@dest[0], sizeof(double), @cMtx[0], @cV[0], 3*sizeof(double), sizeof(double), 3, 3, 0.1, -1);

     Check(CheckMtx(dest1, dest), 'Error matrix vector multiplication failed');

     Move(dest, dest1, sizeof(dest));
     GenericMtxVecMult(@dest1[0], sizeof(double), @cMtx[0], @cV[0], 3*sizeof(double), sizeof(double), 3, 3, 0.1, -1);
     AVXMatrixVectMult(@dest[0], sizeof(double), @cMtx[0], @cV[0], 3*sizeof(double), sizeof(double), 3, 3, 0.1, -1);

     Check(CheckMtx(dest1, dest), 'Error matrix vector multiplication failed');

     Move(dest, dest1, sizeof(dest));
     GenericMtxVecMult(@dest1[0], sizeof(double), @cMtx[0], @cV[0], 3*sizeof(double), sizeof(double), 3, 3, 0.1, -1);
     AVXMatrixVectMultUnAlignedVAligned(@dest[0], sizeof(double), @cMtx[0], @cV[0], 3*sizeof(double), sizeof(double), 3, 3, 0.1, -1);

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
     AVXMatrixVectMultT(@d2[0], sizeof(double), @x[0], @y[0], 9*sizeof(double), 9*sizeof(double), 9, 9, 0.1, -1);

     Check(CheckMtx(d1, d2), 'Error matrix vector multiplication failed');

     GenericMtxVecMultT(@d1[0], sizeof(double), @x[0], @y[0], 9*sizeof(double), 9*sizeof(double), 9, 9, 0.1, -1);
     AVXMatrixVecMultTDestVec(@d2[0], sizeof(double), @x[0], @y[0], 9*sizeof(double), 9*sizeof(double), 9, 9, 0.1, -1);

     Check(CheckMtx(d1, d2), 'Error matrix vector multiplication (dest=vector) failed');

     GenericMtxVecMult(@d1[0], sizeof(double), @x[0], @y[0], 9*sizeof(double), 9*sizeof(double), 9, 9, 0.1, -1);
     AVXMatrixVectMult(@d2[0], sizeof(double), @x[0], @y[0], 9*sizeof(double), 9*sizeof(double), 9, 9, 0.1, -1);

     Check(CheckMtx(d1, d2), 'Error matrix vector multiplication (dest=vector) failed');

     Move(dest, dest1, sizeof(dest));
     GenericMtxVecMult(@d1[0], sizeof(double), @x[0], @y[0], 3*sizeof(double), sizeof(double), 3, 3, 0.1, -1);
     AVXMatrixVectMultUnAlignedVAligned(@d2[0], sizeof(double), @x[0], @y[0], 3*sizeof(double), sizeof(double), 3, 3, 0.1, -1);

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
               AVXMatrixVectMultUnAlignedVAligned(pd2, sizeof(double), px, pv, lineWidthX, sizeof(double), j, i, 2, -1);

               Check(CheckMtxIdx(pd1, pd2, i, idx), Format('Error mtx vec mult u aligned V on %d x %d @ %d', [j, i, idx]));

               GenericMtxVecMult(pd1, sizeof(double), px, pv, lineWidthX, sizeof(double), j, i, 3, -0.1);
               AVXMatrixVectMultAlignedVAligned(pd2, sizeof(double), px, pv, lineWidthX, sizeof(double), j, i, 3, -0.1);

               Check(CheckMtxIdx(pd1, pd2, i, idx), Format('Error mtx vec mult aligned V on %d x %d @ %d', [j, i, idx]));

               FreeMem(pm1);
               FreeMem(pm2);
               FreeMem(pm3);
               FreeMem(pm4);
          end;
     end;

end;

procedure TestAVXMatrixOperations.TestBigMtxVecMult;
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
     ASMMatrixVectMultT(@dest2[0], cMtxSize*sizeof(double), @x[0], @y[0], cMtxSize*sizeof(double), cMtxSize*sizeof(double), cMtxSize - 1, cMtxSize - 1, 2, -0.2);
     endTime2 := MtxGetTime;

     startTime3 := MtxGetTime;
     AVXMatrixVectMultT(@dest3[0], cMtxSize*sizeof(double), @x[0], @y[0], cMtxSize*sizeof(double), cMtxSize*sizeof(double), cMtxSize - 1, cMtxSize - 1, 2, -0.2);
     endTime3 := MtxGetTime;


     Status(Format('%.2f,  %.2f,   %.2f', [(endTime1 - startTime1)/mtxFreq*1000,  (endTime2 - startTime2)/mtxFreq*1000, (endTime3 - startTime3)/mtxFreq*1000]));
     Check(CheckMtx(dest1, dest3), 'Error transposed AVX vector multiplication failed');

     Check(CheckMtx(dest1, dest2), 'Error transposed vector multiplication failed');

     dest3 := Copy(dest2, 0, Length(dest2));

     startTime1 := MtxGetTime;
     GenericMtxVecMultT(@dest1[0], sizeof(double), @x[0], @y[0], cMtxSize*sizeof(double), cMtxSize*sizeof(double), cMtxSize - 1, cMtxSize - 1, 2, -0.2);
     endTime1 := MtxGetTime;

     startTime2 := MtxGetTime;
     ASMMatrixVectMultTDestVec(@dest2[0], sizeof(double), @x[0], @y[0], cMtxSize*sizeof(double), cMtxSize*sizeof(double), cMtxSize - 1, cMtxSize - 1, 2, -0.2);
     endTime2 := MtxGetTime;

     startTime3 := MtxGetTime;
     AVXMatrixVecMultTDestVec(@dest3[0], sizeof(double), @x[0], @y[0], cMtxSize*sizeof(double), cMtxSize*sizeof(double), cMtxSize - 1, cMtxSize - 1, 2, -0.2);
     endTime3 := MtxGetTime;


     Status(Format('%.2f,  %.2f,   %.2f', [(endTime1 - startTime1)/mtxFreq*1000,  (endTime2 - startTime2)/mtxFreq*1000, (endTime3 - startTime3)/mtxFreq*1000]));
     Check(CheckMtx(dest1, dest3), 'Error transposed AVX vector multiplication (destination = vector) failed');

     Check(CheckMtx(dest1, dest2), 'Error transposed vector multiplication (destination = vector) failed');

     dest3 := Copy(dest2, 0, Length(dest2));

     startTime1 := MtxGetTime;
     GenericMtxVecMultT(@dest1[0], sizeof(double), @x[0], @y[0], cMtxSize*sizeof(double), cMtxSize*sizeof(double), cMtxSize - 1, cMtxSize - 1, 2, -0.2);
     endTime1 := MtxGetTime;

     startTime2 := MtxGetTime;
     ASMMatrixVectMultT(@dest2[0], sizeof(double), @x[0], @y[0], cMtxSize*sizeof(double), cMtxSize*sizeof(double), cMtxSize - 1, cMtxSize - 1, 2, -0.2);
     endTime2 := MtxGetTime;

     startTime3 := MtxGetTime;
     AVXMatrixVectMultT(@dest3[0], sizeof(double), @x[0], @y[0], cMtxSize*sizeof(double), cMtxSize*sizeof(double), cMtxSize - 1, cMtxSize - 1, 2, -0.2);
     endTime3:= MtxGetTime;

     Status(Format('%.2f,  %.2f,  %.2f', [(endTime1 - startTime1)/mtxFreq*1000,  (endTime2 - startTime2)/mtxFreq*1000, (endTime3 - startTime3)/mtxFreq*1000]));

     Check(CheckMtx(dest1, dest2), 'Error transposed vector multiplication failed');
     Check(CheckMtx(dest1, dest3), 'Error transposed vector multiplication failed');


     startTime1 := MtxGetTime;
     GenericMtxVecMult(@dest1[0], sizeof(double), @x[0], @y[0], cMtxSize*sizeof(double), cMtxSize*sizeof(double), cMtxSize - 1, cMtxSize - 1, 2, -0.2);
     endTime1 := MtxGetTime;

     startTime2 := MtxGetTime;
     ASMMatrixVectMult(@dest2[0], sizeof(double), @x[0], @y[0], cMtxSize*sizeof(double), cMtxSize*sizeof(double), cMtxSize - 1, cMtxSize - 1, 2, -0.2);
     endTime2 := MtxGetTime;

     startTime3 := MtxGetTime;
     AVXMatrixVectMult(@dest3[0], sizeof(double), @x[0], @y[0], cMtxSize*sizeof(double), cMtxSize*sizeof(double), cMtxSize - 1, cMtxSize - 1, 2, -0.2);
     endTime3:= MtxGetTime;

     Status(Format('%.2f,  %.2f,  %.2f', [(endTime1 - startTime1)/mtxFreq*1000,  (endTime2 - startTime2)/mtxFreq*1000, (endTime3 - startTime3)/mtxFreq*1000]));

     Check(CheckMtx(dest1, dest2), 'Error transposed vector multiplication failed');
     Check(CheckMtx(dest1, dest3), 'Error transposed vector multiplication failed');

     startTime1 := MtxGetTime;
     GenericMtxVecMult(@dest1[0], sizeof(double), @x[0], @y[0], cMtxSize*sizeof(double), sizeof(double), cMtxSize, cMtxSize, 2, -0.2);
     endTime1 := MtxGetTime;

     startTime2 := MtxGetTime;
     ASMMatrixVectMultEvenUnAlignedVAligned(@dest2[0], sizeof(double), @x[0], @y[0], cMtxSize*sizeof(double), sizeof(double), cMtxSize, cMtxSize, 2, -0.2);
     endTime2 := MtxGetTime;

     startTime3 := MtxGetTime;
     AVXMatrixVectMultUnAlignedVAligned(@dest3[0], sizeof(double), @x[0], @y[0], cMtxSize*sizeof(double), sizeof(double), cMtxSize, cMtxSize, 2, -0.2);
     endTime3:= MtxGetTime;

     Status(Format('%.2f,  %.2f,  %.2f', [(endTime1 - startTime1)/mtxFreq*1000,  (endTime2 - startTime2)/mtxFreq*1000, (endTime3 - startTime3)/mtxFreq*1000]));

     Check(CheckMtx(dest1, dest2), 'Error v aligned vector multiplication failed');
     Check(CheckMtx(dest1, dest3), 'Error v aligned multiplication failed');

     FreeMem(xa);
     FreeMem(ya);
end;

procedure TestAVXMatrixOperations.TestAVXBigSVDHeightGEWidth;
const cBlkWidths : Array[0..7] of integer = (43, 9, 16, 24, 43, 189, 256, 500);//, 1024);
      cBlkHeights : Array[0..7] of integer = (331, 13, 26, 123, 331, 223, 256, 543);//, 2048);

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
     InitMtxThreadPool;

     for iter := 0 to 2*Length(cBlkWidths) - 1 do
     begin
          if iter mod 2 = 0
          then
              InitMathFunctions(itFPU, False)
          else
              InitMathFunctions(itAVX, False);

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
          MatrixSVDInPlace2(@A2[0], BlkWidth*sizeof(double), BlkWidth, BlkHeight, PConstDoublearr(@W2[0]), @V2[0], BlkWidth*sizeof(double), BlkWidth);
          stop1 := MtxGetTime;

          SVDBlockSize := 24;
          QRBlockSize := 32;

          start2 := MtxGetTime;
          MatrixSVDInPlace2(@A[0], BlkWidth*sizeof(double), BlkWidth, BlkHeight, PConstDoublearr(@W[0]), @V[0], BlkWidth*sizeof(double), SVDBlockSize);
          stop2 := MtxGetTime;

          start3 := MtxGetTime;
          ThrMatrixSVDInPlace(@A3[0], BlkWidth*sizeof(double), BlkWidth, BlkHeight, PConstDoublearr(@W3[0]), @V3[0], BlkWidth*sizeof(double), SVDBlockSize);
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

     FinalizeMtxThreadPool;
end;

procedure TestAVXMatrixOperations.TestConvolveBig;
const cBSize : Array[0..6] of integer = (1, 2, 7, 19, 24, 39, 48);
      cASize : Array[0..5] of integer = (48, 53, 179, 2430, 5133, 8192);
var i: Integer;
    j: Integer;
    s1, e1, s2, e2 : Int64;
    freq : Int64;
    pA, pB : PDouble;
    Dest, DestSSE : TDoubleDynArray;
    pMem1, pMem2 : PByte;
    LineWidthA, LineWidthB : TASMNativeInt;
    idx : integer;
begin
     freq := mtxFreq;
     for i := 0 to Length(cBSize) - 1 do
     begin
          FillAlignedMtx(cBSize[i], 1, pB, pMem1, LineWidthB);

          for j := 0 to Length(cASize) - 1 do
          begin
               SetLength(Dest, cASize[j]);
               SetLength(DestSSE, cASize[j]);
               FillAlignedMtx(cASize[j], 1, pA, pMem2, LineWidthA);

               InitMathFunctions(itFPU, False);
               s1 := MtxGetTime;
               MatrixConvolve(@dest[0], cASize[j]*sizeof(double), pA, pB, LineWidthA, cASize[j], 1, cBSize[i]);
               e1 := MtxGetTime;

               InitMathFunctions(itAVX, False);
               s2 := MtxGetTime;
               MatrixConvolve(@destSSE[0], cASize[j]*sizeof(double), pA, pB, LineWidthA, cASize[j], 1, cBSize[i]);
               e2 := MtxGetTime;

               Status( Format('Conv (%d x %d) took: %.3f ms, %.3fms', [cBSize[i], cASize[j], (e1 - s1)/freq*1000, (e2 - s2)/freq*1000] ));

               if not CheckMtxIdx(dest, DestSSE, idx) then
                  check(false, 'error in asm convolution @' + IntToStr(idx));

               FreeMem(pMem2);
          end;

          FreeMem(pMem1);
     end;
end;


procedure TestAVXMatrixOperations.TestAddAndScale;
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
     AVXMatrixScaleAndAdd(@dest[0], cBlkWidth*sizeof(double), cBlkWidth, cBlkHeight, 1, 2);
     // check result
     for i := 0 to Length(cMtx) - 1 do
         check( cMtx[i]*2 + 1 = dest[i], 'Error in scale and add');

     Move(cMtx, dest, sizeof(cMtx));
     AVXMatrixAddAndScale(@dest[0], cBlkWidth*sizeof(double), cBlkWidth, cBlkHeight, 1, 2);
     // check result
     for i := 0 to Length(cMtx) - 1 do
         check( (cMtx[i] + 1)*2 = dest[i], 'Error in scale and add');

     Move(cMtx, dest, sizeof(cMtx));
     AVXMatrixScaleAndAdd(@dest[0], cBlkWidth*sizeof(double), cBlkWidth - 1, cBlkHeight, 1, 2);
     // check result
     for i := 0 to Length(cMtx) - 1 do
         if (i + 1) mod cBlkWidth = 0
         then
             check( cmtx[i] = dest[i], 'error in scale and add')
         else
             check( cMtx[i]*2 + 1 = dest[i], 'Error in scale and add');

     Move(cMtx, dest, sizeof(cMtx));
     AVXMatrixAddAndScale(@dest[0], cBlkWidth*sizeof(double), cBlkWidth - 1, cBlkHeight, 1, 2);
     // check result
     for i := 0 to Length(cMtx) - 1 do
         if (i + 1) mod cBlkWidth = 0
         then
             check( cMtx[i] = dest[i], 'error in scale and add')
         else
             check( (cMtx[i] + 1)*2 = dest[i], 'Error in scale and add');

     Move(cMtx, dest, sizeof(cMtx));
     AVXMatrixScaleAndAdd(@dest[0], cBlkWidth*sizeof(double), 1, cBlkHeight, 1, 2);
     // check result
     for i := 0 to Length(cMtx) - 1 do
         if i mod cBlkWidth = 0
         then
             check( cMtx[i]*2 + 1 = dest[i], 'Error in scale and add')
         else
             check( cmtx[i] = dest[i], 'error in scale and add');

     Move(cMtx, dest, sizeof(cMtx));
     AVXMatrixAddAndScale(@dest[0], cBlkWidth*sizeof(double), 1, cBlkHeight, 1, 2);
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
     AVXMatrixScaleAndAdd(@dest1[0], sizeof(double), 1, cBlkWidth - 1, -0.1, 2);
     AVXMatrixScaleAndAdd(aDest, sizeof(double), 1, cBlkWidth - 1, -0.1, 2);

     Check(CheckMtx(dest, dest1), 'Error scale and add width=1');
     Move(aDest^, dest1, sizeof(dest1));
     Check(CheckMtx(dest, dest1), 'Error scale and add aligned width=1');


     Move(cMtx, dest, sizeof(cMtx));
     Move(cMtx, dest1, sizeof(cMtx));
     Move(cMtx, aDest^, sizeof(cMtx));

     GenericMtxAddAndScale(@dest[0], sizeof(double), 1, cBlkWidth, -0.1, 2);
     AVXMatrixAddAndScale(@dest1[0], sizeof(double), 1, cBlkWidth, -0.1, 2);
     AVXMatrixAddAndScale(aDest, sizeof(double), 1, cBlkWidth, -0.1, 2);

     Check(CheckMtx(dest, dest1), 'Error add and scale width=1');
     Move(aDest^, dest1, sizeof(dest1));
     Check(CheckMtx(dest, dest1), 'Error add and scale aligned width=1');


     Move(cMtx, dest, sizeof(cMtx));
     Move(cMtx, dest1, sizeof(cMtx));
     Move(cMtx, aDest^, sizeof(cMtx));

     GenericMtxScaleAndAdd(@dest[0], cBlkWidth*sizeof(double), cBlkWidth, 1, -0.1, 2);
     AVXMatrixScaleAndAdd(@dest1[0], cBlkWidth*sizeof(double), cBlkWidth, 1,  -0.1, 2);
     AVXMatrixScaleAndAdd(aDest, cBlkWidth*sizeof(double), cBlkWidth, 1,  -0.1, 2);

     Check(CheckMtx(dest, dest1), 'Error scale and add height=1');
     Move(aDest^, dest1, sizeof(dest1));
     Check(CheckMtx(dest, dest1), 'Error scale and add height=1');

     Move(cMtx, dest, sizeof(cMtx));
     Move(cMtx, dest1, sizeof(cMtx));
     Move(cMtx, aDest^, sizeof(cMtx));

     GenericMtxAddAndScale(@dest[0], cBlkWidth*sizeof(double), cBlkWidth, 1, -0.1, 2);
     AVXMatrixAddAndScale(@dest1[0], cBlkWidth*sizeof(double), cBlkWidth, 1, -0.1, 2);
     AVXMatrixAddAndScale(aDest, cBlkWidth*sizeof(double), cBlkWidth, 1, -0.1, 2);

     Check(CheckMtx(dest, dest1), 'Error scale and add height=1');
     Move(aDest^, dest1, sizeof(dest1));
     Check(CheckMtx(dest, dest1), 'Error scale and add height=1');

     FreeMem(aDest);
end;

procedure TestAVXMatrixOperations.TestAVXCumulativeSumColumn;
const cMtxWidth : Array[0..10] of integer = (1, 3, 7, 8, 9, 10, 23, 128, 1024, 2048, 2573);
      cMtxHeight : Array[0..10] of integer = (1, 3, 7, 8, 9, 10, 23, 128, 1024, 2048, 2573);
var pX, pDest, pDest2 : PDouble;
    pm1, pm2, pm3 : PByte;
    LineWidthX, destLineWidth, destLn2 : TASMNativeInt;
    startTime1, endTime1 : Int64;
    startTime2, endTime2 : int64;
    startTime3, endTime3 : int64;
    i, j, idx : Integer;
    mtxWidth, mtxHeight : integer;
begin
     for i := 0 to Length(cMtxHeight) - 1 do
     begin
          for j := 0 to Length(cMtxWidth) -1  do
          begin
               mtxWidth := cMtxWidth[j];
               mtxHeight := cMtxHeight[i];
               FillAlignedMtx( mtxWidth, mtxHeight, pX, pm1, LineWidthX);
               AllocAlignedMtx(mtxWidth, mtxHeight, pDest, pM2, destLineWidth);
               AllocAlignedMtx(mtxWidth, mtxHeight, pDest2, pM3, destLn2);

               startTime1 := MtxGetTime;
               GenericMtxCumulativeSum(pDest, destLineWidth, px, LineWidthX, mtxWidth, mtxHeight, False);
               endTime1 := MtxGetTime;

               startTime2 := MtxGetTime;
               AVXMatrixCumulativeSumColumnUnaligned(pDest2, destLn2, px, LineWidthX, mtxWidth, mtxHeight);
               endTime2 := MtxGetTime;

               Check(CheckMtxIdx(pDest, pDest2, destLineWidth, destLn2, mtxWidth, mtxHeight, idx), 'ASM Add failed');

               startTime3 := MtxGetTime;
               AVXMatrixCumulativeSumColumnAligned(pDest2, destLn2, px, LineWidthX, mtxWidth, mtxHeight);
               endTime3 := MtxGetTime;

               Check(CheckMtxIdx(pDest, pDest2, destLineWidth, destLn2, mtxWidth, mtxHeight, idx), 'ASM Add failed');

               Status( Format('Cum Sum col: %d, %d took %.3fms, %.3fms, %.3fms', [mtxWidth, mtxHeight,
                  (endTime1 - startTime1)/mtxFreq*1000, (endTime2 - startTime2)/mtxFreq*1000,
                  (endTime3 - startTime3)/mtxFreq*1000]));

               startTime1 := MtxGetTime;
               GenericMtxCumulativeSum(pDest, destLineWidth, px, LineWidthX, mtxWidth, mtxHeight, True);
               endTime1 := MtxGetTime;

               startTime2 := MtxGetTime;
               AVXMatrixCumulativeSumRow(pDest2, destLn2, px, LineWidthX, mtxWidth, mtxHeight);
               endTime2 := MtxGetTime;

               Check(CheckMtxIdx(pDest, pDest2, destLineWidth, destLn2, mtxWidth, mtxHeight, idx), 'ASM Add failed');

               Status( Format('Cum Sum row: %d, %d took %.3fms, %.3fms', [mtxWidth, mtxHeight,
                  (endTime1 - startTime1)/mtxFreq*1000, (endTime2 - startTime2)/mtxFreq*1000]));

               FreeMem(pm1);
               FreeMem(pm2);
               FreeMem(pm3);
          end;
     end;
end;

procedure TestAVXMatrixOperations.TestAVXDifferentiate;
const cMtxWidth : Array[0..10] of integer = (2, 3, 7, 8, 9, 10, 23, 128, 1024, 2048, 2573);
      cMtxHeight : Array[0..10] of integer = (2, 3, 7, 8, 9, 10, 23, 128, 1024, 2048, 2573);
var pX, pDest, pDest2 : PDouble;
    pm1, pm2, pm3 : PByte;
    LineWidthX, destLineWidth, destLn2 : TASMNativeInt;
    startTime1, endTime1 : Int64;
    startTime2, endTime2 : int64;
    startTime3, endTime3 : int64;
    i, j, idx : Integer;
    mtxWidth, mtxHeight : integer;
begin
     for i := 0 to Length(cMtxHeight) - 1 do
     begin
          for j := 0 to Length(cMtxWidth) -1  do
          begin
               mtxWidth := cMtxWidth[j];
               mtxHeight := cMtxHeight[i];
               FillAlignedMtx( mtxWidth, mtxHeight, pX, pm1, LineWidthX);
               AllocAlignedMtx(mtxWidth, mtxHeight, pDest, pM2, destLineWidth);
               AllocAlignedMtx(mtxWidth, mtxHeight, pDest2, pM3, destLn2);

               startTime1 := MtxGetTime;
               GenericMtxDifferentiate(pDest, destLineWidth, px, LineWidthX, mtxWidth, mtxHeight, False);
               endTime1 := MtxGetTime;

               startTime2 := MtxGetTime;
               AVXMatrixDifferentiateColumnUnaligned(pDest2, destLn2, px, LineWidthX, mtxWidth, mtxHeight);
               endTime2 := MtxGetTime;

               Check(CheckMtxIdx(pDest, pDest2, destLineWidth, destLn2, mtxWidth, mtxHeight, idx), 'ASM diff failed');

               startTime3 := MtxGetTime;
               AVXMatrixDifferentiateColumnAligned(pDest2, destLn2, px, LineWidthX, mtxWidth, mtxHeight);
               endTime3 := MtxGetTime;

               Check(CheckMtxIdx(pDest, pDest2, destLineWidth, destLn2, mtxWidth, mtxHeight, idx), 'ASM diff failed');

               Status( Format('Diff col: %d, %d took %.3fms, %.3fms, %.3fms', [mtxWidth, mtxHeight,
                  (endTime1 - startTime1)/mtxFreq*1000, (endTime2 - startTime2)/mtxFreq*1000,
                  (endTime3 - startTime3)/mtxFreq*1000]));

               startTime1 := MtxGetTime;
               GenericMtxDifferentiate(pDest, destLineWidth, px, LineWidthX, mtxWidth, mtxHeight, True);
               endTime1 := MtxGetTime;

               startTime2 := MtxGetTime;
               AVXMatrixDifferentiateRow(pDest2, destLn2, px, LineWidthX, mtxWidth, mtxHeight);
               endTime2 := MtxGetTime;

               Check(CheckMtxIdx(pDest, pDest2, destLineWidth, destLn2, mtxWidth, mtxHeight, idx), 'ASM diff row failed');

               Status( Format('Diff row: %d, %d took %.3fms, %.3fms', [mtxWidth, mtxHeight,
                  (endTime1 - startTime1)/mtxFreq*1000, (endTime2 - startTime2)/mtxFreq*1000]));

               FreeMem(pm1);
               FreeMem(pm2);
               FreeMem(pm3);
          end;
     end;
end;

procedure TestAVXMatrixOperations.TestTranspose;
const cA : Array[0..15] of double = (00, 01, 02, 03, 10, 11, 12, 13, 20, 21, 22, 23, 30, 31, 32, 33);
      cMtxWidths : Array[0..7] of integer = (16, 16, 128, 753, 345, 560, 1078, 2048);
      cMtxHeights : Array[0..7] of integer = (8, 9, 65, 764, 345, 561, 890, 2048);
var cgb, cb : Array[0..15] of double;
    a, b : Array[0..19] of double;
    pa, pB : PDouble;
    i, j : integer;
    dl1, dl2, dl3 : TASMNativeint;
    dest1, dest2, dest3 : PDouble;
    pDest1, pDest2, pDest3 : PByte;
    mt : PDouble;
    pMt : PByte;
    mtLn : TASMNativeInt;
    idx : integer;
    start1, stop1, start2, stop2, start3, stop3 : int64;
begin
     Status( WriteMtx( cA, 4) );

     pa := PDouble( TASMNativeUInt(@a[0]) + 32 - TASMNativeUInt(@a[0]) and $1F);
     pb := PDouble( TASMNativeUInt(@b[0]) + 32 - TASMNativeUInt(@b[0]) and $1F);

     Move(cA, cgb, sizeof(ca));
     Move(cA, cb, sizeof(ca));

     Move(cA, pA^, sizeof(ca));

     GenericMtxTranspose(@cgb[0], 4*sizeof(double), @ca[0], 4*sizeof(double), 2, 4);

     Status('');
     Status( WriteMtx( cgB, 4) );

     AVXMatrixTranspose(@cb[0], 4*sizeof(double), @ca[0], 4*sizeof(double), 2, 4);

     Status('');
     Status( WriteMtx( cB, 4) );

     Check( CheckMtx(cgb, cb), 'error in AVX Tranpsosition');

     for i := 1 to 4 do
     begin
          for j := 1 to 4 do
          begin
               Move(ca, cb, sizeof(cb));
               Move(ca, cgb, sizeof(ca));

               GenericMtxTranspose(@cgb[0], 4*sizeof(double), @ca[0], 4*sizeof(double), i, j);
               AVXMatrixTranspose(@cb[0], 4*sizeof(double), @ca[0], 4*sizeof(double), i, j);

               Check( CheckMtx(cgb, cb), Format('error in AVX Tranpsosition: %d, %d', [i, j]));

               Move(ca, cb, sizeof(cb));
               Move(ca, cgb, sizeof(ca));

               GenericMtxTranspose(@cgb[0], 4*sizeof(double), @ca[0], 4*sizeof(double), j, i);
               AVXMatrixTranspose(@cb[0], 4*sizeof(double), @ca[0], 4*sizeof(double), j, i);
               Check( CheckMtx(cgb, cb), Format('error in AVX Tranpsosition: %d, %d', [j, i]));

               Move(ca, pB^, sizeof(cb));
               Move(ca, cgb, sizeof(ca));

               GenericMtxTranspose(@cgb[0], 4*sizeof(double), @ca[0], 4*sizeof(double), i, j);
               AVXMatrixTranspose(pB, 4*sizeof(double), pA, 4*sizeof(double), i, j);

               Check( CheckMtxIdx(@cgb[0], pb, 4*sizeof(double), 4*sizeof(double), 4, 4, idx), Format('error in AVX Tranpsosition: %d, %d', [i, j]));

               Move(ca, pB^, sizeof(cb));
               Move(ca, cgb, sizeof(ca));

               GenericMtxTranspose(@cgb[0], 4*sizeof(double), @ca[0], 4*sizeof(double), j, i);
               AVXMatrixTranspose(pB, 4*sizeof(double), pA, 4*sizeof(double), j, i);
               Check( CheckMtxIdx(@cgb[0], pb, 4*sizeof(double), 4*sizeof(double), 4, 4, idx), Format('error in AVX Tranpsosition: %d, %d', [j, i]));
          end;
     end;

     // #################################################
     // #### Big transpose test
     for i := 0 to Length(cMtxWidths) - 1 do
     begin
          for j := 0 to Length(cMtxHeights) - 1 do
          begin
               FillAlignedMtx(cMtxWidths[i], cMtxHeights[j], mt, pMt, mtLn);
               AllocAlignedMtx(cMtxHeights[j], cMtxWidths[i], dest1, pDest1, dl1);
               AllocAlignedMtx(cMtxHeights[j], cMtxWidths[i], dest2, pDest2, dl2);
               AllocAlignedMtx(cMtxHeights[j], cMtxWidths[i], dest3, pDest3, dl3);

               start1 := MtxGetTime;
               GenericMtxTranspose(dest1, dl1, mt, mtLn, cMtxWidths[i], cMtxHeights[j]);
               stop1 := MtxGetTime;

               start2 := MtxGetTime;
               AVXMatrixTranspose(dest2, dl2, mt, mtLn, cMtxWidths[i], cMtxHeights[j]);
               stop2 := MtxGetTime;

               start3 := MtxGetTime;
               ASMMatrixTranspose(dest3, dl3, mt, mtLn, cMtxWidths[i], cMtxHeights[j]);
               stop3 := MtxGetTime;

               Check(CheckMtxIdx( dest1, dest2, dl1, dl2, cMtxHeights[j], cMtxWidths[i], idx ), Format('Big avx transpose failed: %d, %d @ %d', [cMtxHeights[j], cMtxWidths[i], idx]));
               Check(CheckMtxIdx( dest1, dest3, dl1, dl3, cMtxHeights[j], cMtxWidths[i], idx ), Format('Big sse transpose failed: %d, %d @ %d', [cMtxHeights[j], cMtxWidths[i], idx]));

               Status( Format('Transpose %d, %d took %.3fms, %.3fms, %.3fms', [cMtxWidths[i], cMtxHeights[j], (stop1 - start1)/mtxFreq*1000, (stop3 - start3)/mtxFreq*1000, (stop2 - start2)/mtxFreq*1000]));

               FreeMem(pMt);
               FreeMem(pDest2);
               FreeMem(pDest1);
               FreeMem(pDest3);
          end;
     end;
end;


initialization
{$IFNDEF FMX}
  if IsAVXPresent then
     RegisterTest(TestAVXMatrixOperations{$IFNDEF FPC}.Suite{$ENDIF});
{$ENDIF}

end.

