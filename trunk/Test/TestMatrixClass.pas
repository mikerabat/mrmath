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

unit TestMatrixClass;

interface

uses
  TestFramework, Classes, SysUtils, Types, SimpleMatrixOperations, Matrix, BaseMatrixTestCase,
  ThreadedMatrix;

type
  TestTDoubleMatrixPersistence = class(TTestCase)
  published
    procedure TestWrite;
    procedure TestRead;
  end;

type
  TestTDoubleMatrix = class(TBaseMatrixTestCase)
  private
   fRefMatrix2 : TDoubleMatrix;
   fRefMatrix1 : TDoubleMatrix;

   procedure ElemWiseSqr(var elem : double);
  public
   procedure SetUp; override;
   procedure TearDown; override;
  published
   procedure TestConstructors;
   procedure TestPersistence;
   procedure TestAdd;
   procedure TestSub;
   procedure TestMult;
   procedure TestMult2;
   procedure TestMult3;
   procedure TestTranspose;
   procedure TestCovariance;
   procedure TestApplyFunc;
  end;

type
  TestIMatrix = class(TBaseMatrixTestCase)
  private
    fRefMatrix2 : IMatrix;
    fRefMatrix1 : IMatrix;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
   procedure TestAssign;
   procedure TestAdd;
   procedure TestSub;
   procedure TestMult;
   procedure TestMult2;
   procedure TestTranspose;
   procedure TestCovariance;
   procedure TestElementWiseMult;
  end;

type
  TestTThreadedMatrix = class(TBaseMatrixTestCase)
  private
    fRefMatrix2 : TDoubleMatrix;
    fRefMatrix1 : TDoubleMatrix;

    procedure ElemWiseSqr(var elem : double);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestThreadMult;
    procedure TestApplyFunc;
    procedure TestInvert;
    procedure TestDeterminant;
    procedure TestMatrixSolve;
  end;

implementation

uses BaseMathPersistence, binaryReaderWriter,
     math, MatrixConst, mtxTimer;

{ TestTDoubleMatrix }

procedure TestTDoubleMatrix.ElemWiseSqr(var elem: double);
begin
     elem := sqr(elem);
end;

procedure TestTDoubleMatrix.SetUp;
begin
     fRefMatrix1 := ReadObjFromFile('matrixData1.txt') as TDoubleMatrix;
     fRefMatrix2 := ReadObjFromFile('matrixData2.txt') as TDoubleMatrix;
end;

procedure TestTDoubleMatrix.TearDown;
begin
     FreeAndNil(fRefMatrix1);
     FreeAndNil(fRefMatrix2);
end;

procedure TestTDoubleMatrix.TestAdd;
var mtx : TDoubleMatrix;
    dx : Array[0..49] of double;
    i : Integer;
begin
     // create reference
     for i := 0 to High(dx) do
         dx[i] := 3 + i*2;

     mtx := fRefMatrix1.add(fRefMatrix2);
     try
        Check(CheckMtx(mtx.SubMatrix, dx), 'Add1 was wrong ' + WriteMtxDyn(mtx.SubMatrix, mtx.Width));
     finally
            mtx.Free;
     end;

     for i := 0 to High(dx) do
         dx[i] := 1;
     mtx := TDoubleMatrix.Create;
     try
        mtx.Assign(fRefMatrix1);
        mtx.ScaleInPlace(-1);
        mtx.AddInplace(fRefMatrix2);
        Check(CheckMtx(mtx.SubMatrix, dx), 'Add2 was wrong ' + WriteMtxDyn(mtx.SubMatrix, mtx.width));
     finally
            mtx.Free;
     end;
end;

procedure ElemWiseSqrt(var value : double);
begin
     value := sqrt(value);
end;

procedure TestTDoubleMatrix.TestApplyFunc;
var mtx : TDoubleMatrix;
begin
     mtx := TDoubleMatrix.Create;
     try
        mtx.Assign(fRefMatrix1);
        mtx.ElementwiseFuncInPlace(ElemWiseSqrt);
        mtx.ElementwiseFuncInPlace(ElemWiseSqr);

        Check(CheckMtx(mtx.SubMatrix, fRefMatrix1.SubMatrix));
     finally
            mtx.Free;
     end;
end;

procedure TestTDoubleMatrix.TestConstructors;
var mtx : IMatrix;
    data : PDouble;
    i : integer;
    dynData : TDoubleDynArray;
begin
     mtx := TDoubleMatrix.Create;
     assert(mtx.width = 1, 'Error');

     mtx := TDoubleMatrix.Create(3, 3, 3);

     assert(mtx.width = 3, 'Error');
     assert(mtx[2, 2] = 3, 'Error');

     data := GetMemory(4*3*sizeof(double));  // create a 16 byte row wise aligned matrix
     FillChar(data^, 0, 4*3*sizeof(double)); // fill with zeros
     mtx := TDoubleMatrix.Create(data, 4*sizeof(double), 3, 3);

     mtx[0, 0] := 1;
     assert(data^ = 1, 'Error');
     assert(mtx.width = 3, 'Error');

     SetLength(dynData, 9); //3x3 matrix
     for i := 0 to Length(dynData) - 1 do
         dynData[i] := i;

     mtx := TDoubleMatrix.Create(dynData, 3, 3);

     assert(mtx.width = 3, 'Error');
     assert(mtx[2, 2] = 8, 'Error');
end;

procedure TestTDoubleMatrix.TestCovariance;
var meanMtx : TDoubleMatrix;
    cov : TDoubleMatrix;
    i : integer;
    c1 : TDoubleMatrix;
    data : Array[0..99] of double;
begin
     meanMtx := fRefMatrix1.Mean(True);

     cov := TDoubleMatrix.Create;
     cov.Assign(fRefMatrix1);

     for i := 0 to cov.Width - 1 do
     begin
          cov.SetSubMatrix(i, 0, 1, cov.Height);
          cov.SubInPlace(meanMtx);
     end;
     cov.UseFullMatrix;

     c1 := TDoubleMatrix.Create;
     c1.Assign(cov);

     c1.TransposeInPlace;
     cov.MultInPlace(c1);
     cov.ScaleInPlace(1/c1.height);

     // resulting covariance matrix is a one matrix!
     for i := 0 to High(data) do
         data[i] := 2;

     Check(CheckMtx(data, cov.SubMatrix), 'Error wrong covariance');

     c1.Free;
     cov.Free;
     meanMtx.Free;
end;

procedure TestTDoubleMatrix.TestMult;
var mtx : TDoubleMatrix;
    mtx1 : TDoubleMatrix;
begin
     mtx1 := fRefMatrix2.Transpose;
     mtx := fRefMatrix1.Mult(mtx1);
     try
        Check((mtx.width = mtx.height) and (mtx.width = 10), 'Mult Matrix dimension error');
     finally
            mtx.Free;
            mtx1.Free;
     end;
end;

procedure TestTDoubleMatrix.TestMult2;
     // test to show if the bug reported from sutetemp is fixed:
var a, b, c: IMatrix;
    aa, bb, cc : TDoubleDynArray;
    counter: Integer;
begin
     // test different odd and even values
     a := TDoubleMatrix.Create(610, 62, 1);
     aa := a.SubMatrix;
     b := TDoubleMatrix.Create(1, 610, 2);
     bb := b.SubMatrix;
     c := a.Mult(b);

     Check((c.Width = 1) and (c.Height = 62), 'Error matrix dimension wrong');
     cc := c.SubMatrix;

     for counter := 0 to Length(cc) - 1 do
         Check(SameValue(cc[counter], 1220, 1e-12), 'Error multiplying line matrices');

     a := TDoubleMatrix.Create(611, 62, 1);
     aa := a.SubMatrix;
     b := TDoubleMatrix.Create(1, 611, 2);
     bb := b.SubMatrix;
     c := a.Mult(b);

     Check((c.Width = 1) and (c.Height = 62), 'Error matrix dimension wrong');
     cc := c.SubMatrix;

     for counter := 0 to Length(cc) - 1 do
         Check(SameValue(cc[counter], 1222, 1e-12), 'Error multiplying line matrices');

     a := TDoubleMatrix.Create(611, 63, 1);
     aa := a.SubMatrix;
     b := TDoubleMatrix.Create(1, 611, 2);
     bb := b.SubMatrix;
     c := a.Mult(b);

     Check((c.Width = 1) and (c.Height = 63), 'Error matrix dimension wrong');
     cc := c.SubMatrix;

     for counter := 0 to Length(cc) - 1 do
         Check(SameValue(cc[counter], 1222, 1e-12), 'Error multiplying line matrices');
end;

procedure TestTDoubleMatrix.TestMult3;
var r,x,y: TDoubleMatrix;
begin
     // test case from G. Kaiser which led to a memory corruption problem
     x := TDoubleMatrix.Create(401,  60, 1.0);
     y := TDoubleMatrix.Create(  1, 401, 2.0);

     r := x.Mult(y);

     r.free;
     x.Free;
     y.Free;
end;

procedure TestTDoubleMatrix.TestPersistence;
var dx, dy : Array[0..49] of double;
    i : Integer;
    mx, my : TDoubleMatrix;
begin
     for i := 0 to High(dx) do
     begin
          dx[i] := i + 1;
          dy[i] := i + 2;
     end;

     mx := TDoubleMatrix.Create;
     with TBinaryReaderWriter.Create do
     try
        mx.Assign(dx, 5, 10);
        SaveToFile(mx, 'matrixData1.txt');
        mx.Assign(dy, 5, 10);
        SaveToFile(mx, 'matrixData2.txt');
     finally
            Free;
     end;

     mx.Assign(dx, 5, 10);
     my := ReadObjFromFile('matrixData1.txt') as TDoubleMatrix;
     check(Assigned(my), 'error could not read data');
     check(CheckMtx(mx.SubMatrix, my.SubMatrix), 'Error read unsuccessfull');
     my.Free;

     mx.Assign(dy, 5, 10);
     my := ReadObjFromFile('matrixData2.txt') as TDoubleMatrix;
     check(Assigned(my), 'error could not read data');
     check(CheckMtx(mx.SubMatrix, my.SubMatrix), 'Error read unsuccessfull');


     mx.Free;
     my.Free;
end;

procedure TestTDoubleMatrix.TestSub;
var mtx : TDoubleMatrix;
    dx : Array[0..49] of double;
    i : Integer;
begin
     // create reference
     for i := 0 to High(dx) do
         dx[i] := 1;

     mtx := fRefMatrix2.sub(fRefMatrix1);
     try
        Check(CheckMtx(mtx.SubMatrix, dx), 'sub1 was wrong: ' + WriteMtxDyn(mtx.SubMatrix, mtx.width));
     finally
            mtx.Free;
     end;

     mtx := TDoubleMatrix.Create;
     try
        mtx.Assign(fRefMatrix2);
        mtx.SubInplace(fRefMatrix1);
        Check(CheckMtx(mtx.SubMatrix, dx), 'sub2 was wrong: ' + WriteMtxDyn(mtx.SubMatrix, mtx.width));
     finally
            mtx.Free;
     end;
end;

procedure TestTDoubleMatrix.TestTranspose;
var mtx : TDoubleMatrix;
begin
     mtx := TDoubleMatrix.Create;
     try
        mtx.Assign(fRefMatrix1);
        mtx.TransposeInPlace;

        Check((mtx.Width = 10) and (mtx.Height = 5), 'Dimension error');
        mtx.Free;

        mtx := fRefMatrix1.Transpose;

        Check((mtx.Width = 10) and (mtx.Height = 5), 'Dimension error');
     finally
            mtx.Free;
     end;
end;

{ TestTThreadedMatrix }

procedure TestTThreadedMatrix.ElemWiseSqr(var elem: double);
begin
     elem := SQR(elem);
end;

procedure TestTThreadedMatrix.SetUp;
const cMtxWidth = 3453;
      cMtxHeight = 2450;
var x, y : integer;
begin
     TThreadedMatrix.InitThreadPool;

     fRefMatrix1 := TDoubleMatrix.Create(cMtxWidth, cMtxHeight);
     fRefMatrix2 := TDoubleMatrix.Create(cMtxHeight, cMtxWidth);

     // fill in random values:
     for y := 0 to fRefMatrix1.Height - 1 do
         for x := 0 to fRefMatrix1.Width - 1 do
             fRefmatrix1[x, y] := Random;

     for y := 0 to fRefMatrix2.Height - 1 do
         for x := 0 to fRefMatrix2.Width - 1 do
             fRefmatrix2[x, y] := Random;
end;

procedure TestTThreadedMatrix.TearDown;
begin
     fRefMatrix1.Free;
     fRefMatrix2.Free;
     TThreadedMatrix.FinalizeThreadPool;
end;

procedure TestTThreadedMatrix.TestApplyFunc;
var dest1, dest2 : IMatrix;
    m1 : IMatrix;
    startTime1, endTime1 : Int64;
    startTime2, endTime2 : Int64;
begin
     startTime1 := MtxGetTime;
     dest1 := fRefMatrix1.ElementwiseFunc(ElemWiseSqr);
     endTime1 := MtxGetTime;

     m1 := TThreadedMatrix.Create;
     m1.Assign(fRefMatrix1);
     startTime2 := MtxGetTime;
     dest2 := m1.ElementwiseFunc(ElemWiseSqr);
     endTime2 := MtxGetTime;

     status(Format('%.2f, %.2f', [(endTime1 - startTime1)/mtxFreq*1000, (endTime2 - startTime2)/mtxFreq*1000]));

     Check(CheckMtx(dest1.SubMatrix, dest2.SubMatrix));
end;

procedure TestTThreadedMatrix.TestDeterminant;
const cBlkWidth = 1024;
      cBlkSize = cBlkWidth*cBlkWidth;
var x : TDoubleDynArray;
    i: Integer;
    start, stop : Int64;
    det, detThr : double;
    m1, m2 : IMatrix;
begin
     // big inversion
     SetLength(x, cBlkSize);
     RandSeed := 15;
     for i := 0 to cBlkSize - 1 do
         x[i] := Random/3.8 - 0.5/3.8;

     m1 := TDoubleMatrix.Create(x, cBlkWidth, cBlkWidth);
     m2 := TThreadedMatrix.Create(x, cBlkWidth, cBlkWidth);

     start := MtxGetTime;
     det := m1.Determinant;
     Check(det <> 0, 'Singular matrix');
     stop := MtxGetTime;
     Status(Format('Big determinant: %.2fms with value %.5f', [(stop - start)/mtxfreq*1000, det]));

     start := MtxGetTime;
     detThr := m2.Determinant;
     Check(detThr <> 0, 'Singular matrix');
     stop := MtxGetTime;

     Status(Format('Big threaded determinant: %.2fms with value %.5f', [(stop - start)/mtxfreq*1000, detThr]));

     Check(SameValue(det, detThr), 'Error Determinatnts differ too much');
end;


procedure TestTThreadedMatrix.TestInvert;
const cBlkWidth = 1024;
      cBlkSize = cBlkWidth*cBlkWidth;
var x : TDoubleDynArray;
    i: Integer;
    start, stop : Int64;
    m1, m2 : IMatrix;
begin
     // big inversion
     SetLength(x, cBlkSize);
     RandSeed := 15;
     for i := 0 to cBlkSize - 1 do
         x[i] := Random/3.8 - 0.5/3.8;

     m1 := TDoubleMatrix.Create(x, cBlkWidth, cBlkWidth);
     m2 := TThreadedMatrix.Create(x, cBlkWidth, cBlkWidth);

     start := MtxGetTime;
     check(m1.InvertInplace = leOk, 'Error inverting single threaded version');
     stop := MtxGetTime;
     Status(Format('Big single inversion: %.2fms', [(stop - start)/mtxfreq*1000]));

     start := MtxGetTime;
     check(m2.InvertInplace = leOk, 'Error inverting multi threaded version');
     stop := MtxGetTime;

     Status(Format('Big threaded inversion: %.2fms', [(stop - start)/mtxfreq*1000]));

     Check(CheckMtx(m1.SubMatrix, m2.SubMatrix), 'Error threaded inverion differs from original one');
end;

procedure TestTThreadedMatrix.TestMatrixSolve;
const cBlkWidth = 512;
      cBlkSize = cBlkWidth*cBlkWidth;
var a, x1, x2, b : TDoubleDynArray;
    i : integer;
    start, stop : int64;
    index : integer;
    m1, m2 : IMatrix;
    mb : IMatrix;
begin
     SetLength(a, cBlkSize);
     SetLength(b, 3*cBlkWidth);
     SetLength(x1, 3*cBlkWidth);
     SetLength(x2, 3*cBlkWidth);

     RandSeed := 15;
     for i := 0 to cBlkSize - 1 do
         a[i] := Random - 0.5;

     for i := 0 to 3*cBlkWidth - 1 do
         b[i] := Random - 0.5;

     mb := TDoubleMatrix.Create(b, 3, cBlkWidth);
     m1 := TDoubleMatrix.Create(a, cBlkWidth, cBlkWidth);
     m2 := TThreadedMatrix.Create(a, cBlkWidth, cBlkWidth);

     start := MtxGetTime;
     m1.SolveLinEQInPlace(mb);
     stop := MtxGetTime;
     Status(Format('Blocked LU decomp: %.2fms', [(stop - start)/mtxfreq*1000]));

     start := MtxGetTime;
     m2.SolveLinEQInPlace(mb);
     stop := MtxGetTime;
     Status(Format('Threaded LU decomp: %.2fms', [(stop - start)/mtxfreq*1000]));

     index := 0;
     Check(CheckMtxIdx(m1.SubMatrix, m2.SubMatrix, index), Format('error Lin equation solve. Error at x[%d] = %.5f, y[%d] = %.5f', [index, x1[index], index, x2[index]]));
end;

procedure TestTThreadedMatrix.TestThreadMult;
var dest1, dest2 : IMatrix;
    m1 : IMatrix;
    startTime1, endTime1 : Int64;
    startTime2, endTime2 : Int64;
begin
     startTime1 := MtxGetTime;
     dest1 := fRefMatrix1.Mult(fRefMatrix2);
     endTime1 := MtxGetTime;

     m1 := TThreadedMatrix.Create;
     m1.Assign(fRefmatrix1);
     startTime2 := MtxGetTime;
     dest2 := m1.Mult(fRefMatrix2);
     endTime2 := MtxGetTime;

     status(Format('%.2f, %.2f', [(endTime1 - startTime1)/mtxFreq*1000, (endTime2 - startTime2)/mtxFreq*1000]));

     Check(CheckMtx(dest1.SubMatrix, dest2.SubMatrix));
end;

{ TestIMatrix }

procedure TestIMatrix.SetUp;
begin
     fRefMatrix1 := ReadObjFromFile('matrixData1.txt') as IMatrix;
     fRefMatrix2 := ReadObjFromFile('matrixData2.txt') as IMatrix;
end;

procedure TestIMatrix.TearDown;
begin
     fRefMatrix2 := nil;
     fRefMatrix1 := Nil;
end;

procedure TestIMatrix.TestAdd;
var mtx : IMatrix;
    dx : Array[0..49] of double;
    i : Integer;
begin
     // create reference
     for i := 0 to High(dx) do
         dx[i] := 3 + i*2;

     mtx := fRefMatrix1.add(fRefMatrix2);
     Check(CheckMtx(mtx.SubMatrix, dx), 'Add1 was wrong ' + WriteMtxDyn(mtx.SubMatrix, mtx.Width));
     mtx := nil;

     for i := 0 to High(dx) do
         dx[i] := 1;
     mtx := TDoubleMatrix.Create;
     mtx.Assign(fRefMatrix1);
     mtx.ScaleInPlace(-1);
     mtx.AddInplace(fRefMatrix2);
     Check(CheckMtx(mtx.SubMatrix, dx), 'Add2 was wrong ' + WriteMtxDyn(mtx.SubMatrix, mtx.width));
end;

procedure TestIMatrix.TestAssign;
const mtx : Array[0..5] of double = (1, 2, 3, 4, 5, 6);
      mty : Array[0..3] of double = (0, -1, -2, -3);
      mtDest : Array[0..5] of double = (1, 0, -1, 4, -2, -3);
var mt1 : IMatrix;
    mt2 : IMatrix;
begin
     mt1 := TDoubleMatrix.Create;
     // creates a 3x2 matrix and assigns the above values
     mt1.Assign(mtx, 3, 2);


     // assign only a submatrix
     mt2 := TDoubleMatrix.Create;
     mt2.Assign(mty, 2, 2);

     // overwrite the elements of mt1
     mt1.AssignSubMatrix(mt2, 1, 0);

     // mt1 now should look like: (1, 0, -2,
     //                            4, -2, -3);
     Check(CheckMtx(mt1.SubMatrix, mtDest), 'Error assign didn''t work');
end;

procedure TestIMatrix.TestCovariance;
var meanMtx : IMatrix;
    cov : IMatrix;
    i : integer;
    c1 : IMatrix;
    data : Array[0..99] of double;
begin
     meanMtx := fRefMatrix1.Mean(True);

     cov := TDoubleMatrix.Create;
     cov.Assign(fRefMatrix1);

     for i := 0 to cov.Width - 1 do
     begin
          cov.SetSubMatrix(i, 0, 1, cov.Height);
          cov.SubInPlace(meanMtx);
     end;
     cov.UseFullMatrix;

     c1 := TDoubleMatrix.Create;
     c1.Assign(cov);

     c1.TransposeInPlace;
     cov.MultInPlace(c1);
     cov.ScaleInPlace(1/c1.height);

     // resulting covariance matrix is a one matrix!
     for i := 0 to High(data) do
         data[i] := 2;

     Check(CheckMtx(data, cov.SubMatrix), 'Error wrong covariance');
end;


procedure TestIMatrix.TestElementWiseMult;
var mtx : IMatrix;
    x, y : integer;
begin
     mtx := TDoubleMatrix.Create(1, 1, -2);
     mtx.ElementWiseMultInPlace(mtx);

     Check(SameValue(mtx[0, 0], 4), 'Error 1x1 elementwise mult failed');

     mtx := TDoubleMatrix.Create(1, 2, -2);
     mtx.ElementWiseMultInPlace(mtx);
     Check(SameValue(mtx[0, 0], 4) and SameValue(mtx[0, 1], 4), 'Error 1x2 elementwise mult failed');

     mtx := TDoubleMatrix.Create(2, 2, -2);
     mtx.ElementWiseMultInPlace(mtx);
     Check(SameValue(mtx[0, 0], 4) and SameValue(mtx[0, 1], 4), 'Error 2x2 elementwise mult failed');
     Check(SameValue(mtx[1, 0], 4) and SameValue(mtx[1, 1], 4), 'Error 2x2 elementwise mult failed');

     mtx := TDoubleMatrix.Create(2, 3, -2);
     mtx.ElementWiseMultInPlace(mtx);
     Check(SameValue(mtx[0, 0], 4) and SameValue(mtx[0, 1], 4) and SameValue(mtx[0, 2], 4), 'Error 2x3 elementwise mult failed');
     Check(SameValue(mtx[1, 0], 4) and SameValue(mtx[1, 1], 4) and SameValue(mtx[1, 2], 4), 'Error 2x3 elementwise mult failed');

     mtx := TDoubleMatrix.Create(3, 2, -2);
     mtx.ElementWiseMultInPlace(mtx);
     Check(SameValue(mtx[0, 0], 4) and SameValue(mtx[0, 1], 4), 'Error 3x2 elementwise mult failed');
     Check(SameValue(mtx[1, 0], 4) and SameValue(mtx[1, 1], 4), 'Error 3x2 elementwise mult failed');
     Check(SameValue(mtx[2, 0], 4) and SameValue(mtx[2, 1], 4), 'Error 3x2 elementwise mult failed');


     mtx := TDoubleMatrix.Create(123, 124, -2);
     mtx.ElementWiseMultInPlace(mtx);
     for x := 0 to mtx.Width - 1 do
         for y := 0 to mtx.Height - 1 do
             Check(SameValue(mtx[x, y], 4), 'Error big elementwise mult failed @' + IntToStr(x) + ',' + IntToStr(y));
end;


procedure TestIMatrix.TestMult;
var mtx : IMatrix;
    mtx1 : IMatrix;
begin
     mtx1 := fRefMatrix2.Transpose;
     mtx := fRefMatrix1.Mult(mtx1);
     Check((mtx.width = mtx.height) and (mtx.width = 10), 'Mult Matrix dimension error');
end;

procedure TestIMatrix.TestMult2;
const x : Array[0..8] of double = (1, 2, 3, 4, 5, 6, 7, 8, 9);
    y : Array[0..8] of double = (0, 1, 2, 3, 4, 5, 6, 7, 8);
    res : Array[0..8] of double = (24, 30, 36, 51, 66, 81, 78, 102, 126);
var m1, m2 : IMatrix;
    m3 : IMatrix;
begin
     m1 := TDoubleMatrix.Create;
     m1.Assign(x, 3, 3);
     m2 := TDoubleMatrix.Create;
     m2.Assign(y, 3, 3);

     m3 := m1.Mult(m2);

     Check(CheckMtx(res, m3.SubMatrix, 3, 3), 'Error multiplication failed');
end;

procedure TestIMatrix.TestSub;
var mtx : IMatrix;
    dx : Array[0..49] of double;
    i : Integer;
begin
     // create reference
     for i := 0 to High(dx) do
         dx[i] := 1;

     mtx := fRefMatrix2.sub(fRefMatrix1);
     Check(CheckMtx(mtx.SubMatrix, dx), 'sub1 was wrong: ' + WriteMtxDyn(mtx.SubMatrix, mtx.width));

     mtx := TDoubleMatrix.Create;
     mtx.Assign(fRefMatrix2);
     mtx.SubInplace(fRefMatrix1);
     Check(CheckMtx(mtx.SubMatrix, dx), 'sub2 was wrong: ' + WriteMtxDyn(mtx.SubMatrix, mtx.width));
end;


procedure TestIMatrix.TestTranspose;
var mtx : IMatrix;
begin
     mtx := TDoubleMatrix.Create;
     mtx.Assign(fRefMatrix1);
     mtx.TransposeInPlace;

     Check((mtx.Width = 10) and (mtx.Height = 5), 'Dimension error');

     mtx := fRefMatrix1.Transpose;
     Check((mtx.Width = 10) and (mtx.Height = 5), 'Dimension error');
end;


{ TestTDoubleMatrixPersistence }

procedure TestTDoubleMatrixPersistence.TestRead;
var mtx : TDoubleMatrix;
begin
     mtx := ReadObjFromFile('matrixData1.dat') as  TDoubleMatrix;
     Check((mtx.Width = 5) and (mtx.Height = 10), 'Error matrix dimension are not correct');
     mtx.Free;
     mtx := ReadObjFromFile('matrixData2.dat') as  TDoubleMatrix;
     Check((mtx.Width = 5) and (mtx.Height = 10), 'Error matrix dimension are not correct');
     mtx.Free;
end;

procedure TestTDoubleMatrixPersistence.TestWrite;
var mtx : TDoubleMatrix;
    dx : TDoubleDynArray;
    i : Integer;
begin
     SetLength(dx, 5*10);

     for i := 0 to Length(dx) - 1 do
         dx[i] := 2 + i;

     mtx := TDoubleMatrix.Create(dx, 5, 10);
     try
        TBinaryReaderWriter.StaticSaveToFile(mtx, 'matrixData1.dat');
     finally
            mtx.Free;
     end;

     for i := 0 to Length(dx) - 1 do
         dx[i] := 1 + i;

     mtx := TDoubleMatrix.Create(dx, 5, 10);
     try
        TBinaryReaderWriter.StaticSaveToFile(mtx, 'matrixData2.dat');
     finally
            mtx.Free;
     end;
end;

initialization
  // Alle Testfälle beim Test-Runner registrieren
  RegisterTest(TestTDoubleMatrix.Suite);
  RegisterTest(TestTDoubleMatrixPersistence.Suite);
  RegisterTest(TestIMatrix.Suite);
  RegisterTest(TestTThreadedMatrix.Suite);

end.
