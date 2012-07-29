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
   procedure TestAdd;
   procedure TestSub;
   procedure TestMult;
   procedure TestMult2;
   procedure TestTranspose;
   procedure TestCovariance;
  end;

type
  TestTThreadedMatrix = class(TBaseMatrixTestCase)
  private
    fRefMatrix2 : TThreadedMatrix;
    fRefMatrix1 : TThreadedMatrix;
    fPerfFreq : Int64;

    procedure ElemWiseSqr(var elem : double);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestThreadMult;
    procedure TestApplyFunc;
  end;

implementation

uses Windows, Dialogs, BaseMathPersistence, binaryReaderWriter;//, OldMatrix, MatrixEncoder;

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

     QueryPerformanceFrequency(fPerfFreq);

     fRefMatrix1 := TThreadedMatrix.Create(cMtxWidth, cMtxHeight);
     fRefMatrix2 := TThreadedMatrix.Create(cMtxHeight, cMtxWidth);

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
var dest1, dest2 : TDoubleMatrix;
    startTime1, endTime1 : Int64;
    startTime2, endTime2 : Int64;
begin
     QueryPerformanceCounter(startTime1);
     dest1 := fRefMatrix1.ElementwiseFunc(ElemWiseSqr);
     QueryPerformanceCounter(endTime1);

     QueryPerformanceCounter(startTime2);
     dest2 := TDoubleMatrix(fRefMatrix1).ElementwiseFunc(ElemWiseSqr);
     QueryPerformanceCounter(endTime2);

     Status(Format('%.2f, %.2f', [(endTime1 - startTime1)/fPerfFreq*1000, (endTime2 - startTime2)/fPerfFreq*1000]));

     Check(CheckMtx(dest1.SubMatrix, dest2.SubMatrix));

     dest1.Free;
     dest2.Free;
end;

procedure TestTThreadedMatrix.TestThreadMult;
var dest1, dest2 : TDoubleMatrix;
    startTime1, endTime1 : Int64;
    startTime2, endTime2 : Int64;
begin
     QueryPerformanceCounter(startTime1);
     dest1 := fRefMatrix1.Mult(fRefMatrix2);
     QueryPerformanceCounter(endTime1);

     QueryPerformanceCounter(startTime2);
     dest2 := TDoubleMatrix(fRefMatrix1).Mult(fRefMatrix2);
     QueryPerformanceCounter(endTime2);

     Status(Format('%.2f, %.2f', [(endTime1 - startTime1)/fPerfFreq*1000, (endTime2 - startTime2)/fPerfFreq*1000]));

     Check(CheckMtx(dest1.SubMatrix, dest2.SubMatrix));

     dest1.Free;
     dest2.Free;
end;

{ TestIMatrix }

procedure TestIMatrix.SetUp;
begin
     fRefMatrix1 := ReadObjFromFile('matrixData1.txt') as IMatrix;
     fRefMatrix2 := ReadObjFromFile('matrixData2.txt') as IMatrix;
end;

procedure TestIMatrix.TearDown;
begin

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
  RegisterTest(TestTDoubleMatrix.Suite);
  RegisterTest(TestTThreadedMatrix.Suite);
  RegisterTest(TestIMatrix.Suite);
  RegisterTest(TestTDoubleMatrixPersistence.Suite);

end.
