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

{$IFDEF MACOS}
   {$DEFINE FMX}
{$ENDIF}

uses
  {$IFDEF FPC} testregistry {$ELSE} {$IFDEF FMX}DUnitX.TestFramework {$ELSE}TestFramework {$ENDIF} {$ENDIF} ,
  Classes, SysUtils, Types, Matrix, BaseMatrixTestCase,
  ThreadedMatrix;

type
  {$IFDEF FMX} [TestFixture] {$ENDIF}
  TestTDoubleMatrixPersistence = class(TBaseMatrixTestCase)
  published
    procedure TestWrite;
    procedure TestRead;

    procedure TestJSONWriteRead;
    procedure TestJSONRead;
  end;

type
  {$IFDEF FMX} [TestFixture] {$ENDIF}
  TestTDoubleMatrix = class(TBaseMatrixTestCase)
  private
   fRefMatrix2 : TDoubleMatrix;
   fRefMatrix1 : TDoubleMatrix;

   procedure OnProgress(Progress : Integer);
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
   procedure TestMultT1;
   procedure TestMultT2;
   procedure TestSVD;
   procedure TestTranspose;
   procedure TestCovariance;
   procedure TestApplyFunc;
   procedure TestSumInPlace;
   procedure TestQR;
   procedure TestCholesky;
   procedure TestSymEig;
   procedure TestEig;
   procedure TestNormalize;
   procedure TestRandomInit;
   procedure TestSort;
   procedure TestAppend;
   procedure TestSelect;
   procedure TestDiag;
  end;

type
  {$IFDEF FMX} [TestFixture] {$ENDIF}
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
   procedure TestElementWiseDiv;
   procedure TestPinv;
   procedure TesMedian;
  end;

type
  {$IFDEF FMX} [TestFixture] {$ENDIF}
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
    procedure TestMedian;
    procedure TestSort;
    procedure TestCholesky;
    procedure TestSVD;
    procedure TestSymEig;
    procedure TestLeastSquares;
  end;

implementation

uses BaseMathPersistence, binaryReaderWriter,
     math, MatrixConst, mtxTimer,
     {$IFNDEF FMX}
     Dialogs,
     {$ENDIF}
     MathUtilFunc, RandomEng,
     JSONReaderWriter, MatrixASMStubSwitch, PCA;

{ TestTDoubleMatrix }

procedure TestTDoubleMatrix.ElemWiseSqr(var elem: double);
begin
     elem := sqr(elem);
end;

procedure TestTDoubleMatrix.SetUp;
begin
     fRefMatrix1 := ReadObjFromFile(BaseDataPath + 'MatrixData1.txt') as TDoubleMatrix;
     fRefMatrix2 := ReadObjFromFile(BaseDataPath + 'MatrixData2.txt') as TDoubleMatrix;
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
     {$IFDEF FMX} Setup; {$ENDIF};
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

     {$IFDEF FMX} TearDown; {$ENDIF};
end;

procedure ElemWiseSqrt(var value : double);
begin
     value := sqrt(value);
end;

procedure TestTDoubleMatrix.TestApplyFunc;
var mtx : TDoubleMatrix;
begin
     {$IFDEF FMX} Setup; {$ENDIF};

     mtx := TDoubleMatrix.Create;
     try
        mtx.Assign(fRefMatrix1);
        mtx.ElementwiseFuncInPlace({$IFDEF FPC}@{$ENDIF}ElemWiseSqrt);
        mtx.ElementwiseFuncInPlace({$IFDEF FPC}@{$ENDIF}ElemWiseSqr);

        Check(CheckMtx(mtx.SubMatrix, fRefMatrix1.SubMatrix));
     finally
            mtx.Free;
     end;

     {$IFDEF FMX} TearDown; {$ENDIF};
end;

procedure TestTDoubleMatrix.TestCholesky;
var a, b, c : TDoubleMatrix;
    res : TCholeskyResult;
begin
     a := TDoubleMatrix.CreateRand(123, 123);
     b := a.Transpose;
     a.MultInPlace(b);
     b.Free;

     res := a.Cholesky(b);

     check(res = crOk, 'Error cholesky decomposition failed');

     c := b.Transpose;
     b.MultInPlace(c);
     c.Free;

     check( CheckMtx(a.SubMatrix, b.SubMatrix), 'Cholesky error L*LT does not result in the original matrix');

     a.Free;
     b.Free;
end;

procedure TestTDoubleMatrix.TestConstructors;
var mtx : IMatrix;
    data : PDouble;
    i : integer;
    dynData : TDoubleDynArray;
begin
     mtx := TDoubleMatrix.Create;
     Check(mtx.width = 1, 'Error');

     mtx := TDoubleMatrix.Create(3, 3, 3);

     Check(mtx.width = 3, 'Error');
     Check(mtx[2, 2] = 3, 'Error');

     data := GetMemory(4*3*sizeof(double));  // create a 16 byte row wise aligned matrix
     FillChar(data^, 0, 4*3*sizeof(double)); // fill with zeros
     mtx := TDoubleMatrix.Create(data, 4*sizeof(double), 3, 3);

     mtx[0, 0] := 1;
     Check(data^ = 1, 'Error');
     Check(mtx.width = 3, 'Error');

     SetLength(dynData, 9); //3x3 matrix
     for i := 0 to Length(dynData) - 1 do
         dynData[i] := i;

     mtx := TDoubleMatrix.CreateDyn(dynData, 3, 3);

     Check(mtx.width = 3, 'Error');
     Check(mtx[2, 2] = 8, 'Error');
end;

procedure TestTDoubleMatrix.TestCovariance;
var meanMtx : TDoubleMatrix;
    cov : TDoubleMatrix;
    i : integer;
    c1 : TDoubleMatrix;
    data : Array[0..99] of double;
begin
     {$IFDEF FMX} Setup; {$ENDIF};

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

     {$IFDEF FMX} TearDown; {$ENDIF};
end;


procedure TestTDoubleMatrix.TestDiag;
var mtx : TDoubleMatrix;
    mtx1 : TDoubleMatrix;
    mtx2 : TDoubleMatrix;
begin
     mtx := TDoubleMatrix.CreateRand( 3, 5 );
     mtx1 := mtx.Diag(True);

     check( (mtx1.Width = 3) and (mtx1.Height = 3), 'Bad matrix size');
     mtx1.Free;

     mtx1 := mtx.Diag(False);
     check( (mtx1.Width = 1) and (mtx1.Height = 3), 'Bad matrix size');
     
     mtx1.Free;

     mtx1 := mtx.Diag(-1);
     check( (mtx1.Width = 1) and (mtx1.Height = 3), 'Bad matrix size');
     mtx2 := mtx1.Diag(-1);
     Check( (mtx2.Width = 3) and (mtx2.Height = 4), 'Bad matrix size');
     mtx2.Free;
     mtx1.Free;


     mtx1 := mtx.Diag(-2);
     check( (mtx1.Width = 1) and (mtx1.Height = 3), 'Bad matrix size');
     mtx2 := mtx1.Diag(-2);
     Check( (mtx2.Width = 3) and (mtx2.Height = 5), 'Bad matrix size');
     mtx2.Free;
     mtx1.Free;

     mtx1 := mtx.Diag(-3);
     check( (mtx1.Width = 1) and (mtx1.Height = 2), 'Bad matrix size');
     mtx2 := mtx1.Diag(-3);
     Check( (mtx2.Width = 2) and (mtx2.Height = 5), 'Bad matrix size');
     mtx2.Free;
     mtx1.Free;

     mtx1 := mtx.Diag(-4);
     check( (mtx1.Width = 1) and (mtx1.Height = 1), 'Bad matrix size');
     mtx2 := mtx1.Diag(-4);
     Check( (mtx2.Width = 1) and (mtx2.Height = 5), 'Bad matrix size');
     mtx2.Free;
     mtx1.Free;

     mtx.TransposeInPlace;
     mtx1 := mtx.Diag(1);
     check( (mtx1.Width = 1) and (mtx1.Height = 3), 'Bad matrix size');
     mtx2 := mtx1.Diag(1);
     Check( (mtx2.Width = 4) and (mtx2.Height = 3), 'Bad matrix size');
     mtx2.Free;
     mtx1.Free;

     mtx1 := mtx.Diag(2);
     check( (mtx1.Width = 1) and (mtx1.Height = 3), 'Bad matrix size');
     mtx2 := mtx1.Diag(2);
     Check( (mtx2.Width = 5) and (mtx2.Height = 3), 'Bad matrix size');
     mtx2.Free;
     mtx1.Free;

     mtx1 := mtx.Diag(3);
     check( (mtx1.Width = 1) and (mtx1.Height = 2), 'Bad matrix size');
     mtx2 := mtx1.Diag(3);
     Check( (mtx2.Width = 5) and (mtx2.Height = 2), 'Bad matrix size');
     mtx2.Free;
     mtx1.Free;

     mtx1 := mtx.Diag(4);
     check( (mtx1.Width = 1) and (mtx1.Height = 1), 'Bad matrix size');
     mtx2 := mtx1.Diag(4);
     Check( (mtx2.Width = 5) and (mtx2.Height = 1), 'Bad matrix size');
     mtx2.Free;
     mtx1.Free;
     
     mtx.Free;
end;

procedure TestTDoubleMatrix.TestEig;
var mtx : TDoubleMatrix;
    eigvals : TDoubleMatrix;
    eigVec : TDoubleMatrix;
    leftSide : TDoubleMatrix;
    rightSide : TDoubleMatrix;
    counter: Integer;
const cEigs : Array[0..7] of double = ( 2.6701862, 0, 0.126526, 0, 0.311542, 0, -0.873255, 0 );  // from matlab
      cMtx : Array[0..15] of double = ( 1.0000,    0.5000,    0.3333,    0.2500,
                                        2.0000,    0.1100,    0.2500,    1.1100,
                                        0.3333,    0.6667,    1.0000,    0.7500,
                                        2.0000,    0.9222,    0.2500,    0.1250 );
begin
     mtx := TDoubleMatrix.Create;
     try
        mtx.Assign(cMtx, 4, 4);
        
        Check( mtx.Eig(eigvals, EigVec) = qlOk, 'Error no convergence');

        Status(WriteMtx(mtx.SubMatrix, mtx.Width, 6));
        
        Status(WriteMtx(eigVals.SubMatrix, eigVals.width, 6));
        Status(WriteMtx(eigVec.SubMatrix, eigVec.width, 6));

        Check( CheckMtx(cEigs, eigVals.SubMatrix), 'Eigenvalue calculation failed');

        // check if it serves A*x = eigval * x
        leftSide := mtx.Mult(eigVec);
        for counter := 0 to 3 do
        begin
             eigVec.SetSubMatrix(counter, 0, 1, eigVec.Height);
             leftSide.SetSubMatrix(counter, 0, 1, leftSide.Height);
             rightSide := eigVec.Scale(eigVals[0, counter]); // only real part!

             Check(CheckMtx(leftSide.SubMatrix, rightSide.SubMatrix), 'Eigenvector failed');

             rightside.Free;
        end;

        leftSide.Free;
        
        eigvals.Free;
        eigVec.Free;

        Check( mtx.Eig(eigVals) = qlOk, 'Error no convergence');
        Status(WriteMtx(eigVals.SubMatrix, eigVals.Width));

        Check( CheckMtx(cEigs, eigVals.SubMatrix), 'Eigenvalue calculation failed');
        eigVals.Free;
     finally
            mtx.Free;
     end;
end;

procedure TestTDoubleMatrix.TestMult;
var mtx : TDoubleMatrix;
    mtx1 : TDoubleMatrix;
begin
     {$IFDEF FMX} Setup; {$ENDIF};
     mtx1 := fRefMatrix2.Transpose;
     mtx := fRefMatrix1.Mult(mtx1);
     try
        Check((mtx.width = mtx.height) and (mtx.width = 10), 'Mult Matrix dimension error');
     finally
            mtx.Free;
            mtx1.Free;
     end;

     {$IFDEF FMX} TearDown; {$ENDIF};
end;

procedure TestTDoubleMatrix.TestMult2;
     // test to show if the bug reported from sutetemp is fixed:
var a, b, c: IMatrix;
    cc : TDoubleDynArray;
    counter: Integer;
begin
     // test different odd and even values
     a := TDoubleMatrix.Create(610, 62, 1);
     b := TDoubleMatrix.Create(1, 610, 2);
     c := a.Mult(b);

     Check((c.Width = 1) and (c.Height = 62), 'Error matrix dimension wrong');
     cc := c.SubMatrix;

     for counter := 0 to Length(cc) - 1 do
         Check(SameValue(cc[counter], 1220, 1e-12), 'Error multiplying line matrices');

     a := TDoubleMatrix.Create(611, 62, 1);
     b := TDoubleMatrix.Create(1, 611, 2);
     c := a.Mult(b);

     Check((c.Width = 1) and (c.Height = 62), 'Error matrix dimension wrong');
     cc := c.SubMatrix;

     for counter := 0 to Length(cc) - 1 do
         Check(SameValue(cc[counter], 1222, 1e-12), 'Error multiplying line matrices');

     a := TDoubleMatrix.Create(611, 63, 1);
     b := TDoubleMatrix.Create(1, 611, 2);
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

procedure TestTDoubleMatrix.TestMultT1;
var mtx : TDoubleMatrix;
    mtx1 : TDoubleMatrix;
begin
     {$IFDEF FMX} Setup; {$ENDIF};

     mtx1 := TDoubleMatrix.Create;
     mtx1.Assign(fRefMatrix1);
     mtx := fRefMatrix1.MultT1(mtx1);
     try
        Check((mtx.width = mtx.height) and (mtx.width = 5), 'Mult Matrix dimension error');
     finally
            mtx.Free;
            mtx1.Free;
     end;

     {$IFDEF FMX} TearDown; {$ENDIF};
end;

procedure TestTDoubleMatrix.TestMultT2;
var mtx : TDoubleMatrix;
    mtx1 : TDoubleMatrix;
begin
     {$IFDEF FMX} Setup; {$ENDIF};
     mtx1 := TDoubleMatrix.Create;
     mtx1.Assign(fRefMatrix1);
     mtx := fRefMatrix1.MultT2(mtx1);
     try
        Check((mtx.width = mtx.height) and (mtx.width = 10), 'Mult Matrix dimension error');
     finally
            mtx.Free;
            mtx1.Free;
     end;
     {$IFDEF FMX} TearDown; {$ENDIF};
end;


procedure TestTDoubleMatrix.TestNormalize;
const cA : Array[0..15] of double = (2, 1, 4, 2, 
                                     4, 5, 3, 2, 
                                     32, 8, 16, 8,
                                     5, 3, 1, 7 );
      cNorm1 : Array[0..15] of double = (2/5, 1/5, 4/5, 2/5, 
                                         0.5443, 0.6804, 0.4082, 0.2722, 
                                         0.8528, 0.2132, 0.4264, 0.2132,
                                         0.5455, 0.3273, 0.1091, 0.7638 );
var mtx : TDoubleMatrix;
begin
     mtx := TDoubleMatrix.Create;
     try
        mtx.Assign(cA, 4, 4);
        mtx.Normalize(True);

        Check(CheckMtx( mtx.SubMatrix, cNorm1), 'Normalized failed');

        mtx.Assign(cA, 4, 4);
        mtx.TransposeInPlace;
        mtx.Normalize(False);
        mtx.TransposeInPlace;

        Check(CheckMtx(mtx.SubMatrix, cNorm1), 'Normalized failed');
     finally
            mtx.Free;
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
        SaveToFile(mx, 'm1.txt');
        mx.Assign(dy, 5, 10);
        SaveToFile(mx, 'm2.txt');
     finally
            Free;
     end;

     mx.Assign(dx, 5, 10);
     my := ReadObjFromFile('m1.txt') as TDoubleMatrix;
     check(Assigned(my), 'error could not read data');
     check(CheckMtx(mx.SubMatrix, my.SubMatrix), 'Error read unsuccessfull');
     my.Free;

     mx.Assign(dy, 5, 10);
     my := ReadObjFromFile('m2.txt') as TDoubleMatrix;
     check(Assigned(my), 'error could not read data');
     check(CheckMtx(mx.SubMatrix, my.SubMatrix), 'Error read unsuccessfull');


     mx.Free;
     my.Free;
end;

procedure TestTDoubleMatrix.TestQR;
var mtx, dest, Q, R : TDoubleMatrix;
    x, y : integer;
const cBlkWidth = 12;
      cBlkHeight = 7;
begin
     mtx := TDoubleMatrix.Create(cBlkWidth, cBlkHeight);
     RandSeed := 15;
     for y := 0 to cBlkHeight - 1 do
         for x := 0 to cBlkWidth - 1 do
             mtx[x, y] := Random - 0.5;

     check( mtx.QRFull(Q, R) = qrOK, 'Error QR decomposition failed');

     // simple test: Q*R = mtx
     dest := Q.Mult(R);

     check(CheckMtx(dest.SubMatrix, mtx.SubMatrix), 'Error matrix class QR decomposition failed');

     q.Free;
     R.Free;
     dest.Free;
     
     // 2nd asymetric case:
     mtx.TransposeInPlace;
     check( mtx.QRFull(Q, R) = qrOK, 'Error QR decomposition failed');

     // simple test: Q*R = mtx
     dest := Q.Mult(R);

     check(CheckMtx(dest.SubMatrix, mtx.SubMatrix), 'Error matrix class QR decomposition failed');
     mtx.Free;
     q.Free;
     R.Free;
     dest.Free;
end;

procedure TestTDoubleMatrix.TestRandomInit;
var mtx1, mtx2 : TDoubleMatrix;
begin
     RandSeed := 301;
     mtx1 := TDoubleMatrix.CreateRand(100, 100);
     mtx2 := TDoubleMatrix.CreateRand(100, 100, raSystem, 301);
     try
        {$IFNDEF FPC}
        // FPC note: the default random function is different than the delphi one -> so a check
        // here is meaningless but we have at least a functional test
        Check(CheckMtx(mtx1.SubMatrix, mtx2.SubMatrix, -1, -1, 0.0000001), 'Random init failed');
        {$ENDIF}
     finally
            mtx1.Free;
            mtx2.Free;
     end;
end;

procedure TestTDoubleMatrix.TestSelect;
var mtx, mtx1 : TDoubleMatrix;
    x, y : integer;
    rows, cols : TIntegerDynArray;
begin
     mtx := TDoubleMatrix.CreateRand(20, 20);

     // invert the columns
     mtx1 := mtx.SubColMtx(19, 0);

     for x := 0 to mtx.Width - 1 do
     begin
          for y := 0 to mtx.Height - 1 do
              Check( mtx[19 - x, y] = mtx1[x, y], 'Bad selection');
     end;

     mtx1.Free;
     
     // invert the rows
     mtx1 := mtx.SubRowMtx(19, 0);

     for x := 0 to mtx.Width - 1 do
     begin
          for y := 0 to mtx.Height - 1 do
              Check( mtx[x, 19 - y] = mtx1[x, y], 'Bad selection');
     end;
     mtx1.Free;
     
     // invert the rows
     mtx1 := mtx.SubRowMtx(19, 0);

     for x := 0 to mtx.Width - 1 do
     begin
          for y := 0 to mtx.Height - 1 do
              Check( mtx[x, 19 - y] = mtx1[x, y], 'Bad selection');
     end;
     mtx1.Free;
     
     // invert both
     mtx1 := mtx.SubMtx(19, 0, 19, 0);
     for x := 0 to mtx.Width - 1 do
     begin
          for y := 0 to mtx.Height - 1 do
              Check( mtx[19 - x, 19 - y] = mtx1[x, y], 'Bad selection');
     end;
     mtx1.Free;

     // sub selection
     rows := Arr([0, 2, 4, 5, 6, 7, 14, 19]);
     cols :=Arr([0, 1, 3, 4, 5, 6, 7, 12, 18]);
     
     mtx1 := mtx.SubMtx(cols, rows);

     for x := 0 to Length(cols) - 1 do
     begin
          for y := 0 to Length(rows) - 1 do
              Check( mtx1[x, y] = mtx[ cols[x], rows[y] ], 'Bad selection');
     end;

     mtx1.Free;
     mtx.Free;
end;

procedure TestTDoubleMatrix.TestSort;
var mtx, mtx1 : TDoubleMatrix;
    x, y : integer;
begin
     mtx := TDoubleMatrix.CreateRand(20, 20);
     mtx1 := mtx.Sort(True);

     for y := 0 to mtx1.Height - 1 do
     begin
          for x := 1 to mtx1.Width - 1 do
              Check( mtx1[x - 1, y] <= mtx1[x, y], 'Error wrong sort order');
     end;

     mtx.SortInPlace(False);

     for x := 0 to mtx.Width - 1 do
     begin
          for y := 1 to mtx.Height - 1 do
              Check( mtx[x, y - 1] <= mtx[x, y], 'Error wrong sort order');
     end;

     mtx1.Free;
     mtx.Free;
end;

procedure TestTDoubleMatrix.TestSub;
var mtx : TDoubleMatrix;
    dx : Array[0..49] of double;
    i : Integer;
begin
     {$IFDEF FMX} Setup; {$ENDIF};
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
     {$IFDEF FMX} TearDown; {$ENDIF};
end;

procedure TestTDoubleMatrix.TestSumInPlace;
var a : TDoubleMatrix;
    i : Integer;
begin
     a := TDoubleMatrix.Create(4, 1);
     for i := 0 to 3 do
         a[i,0] := i + 1;
     a.SumInPlace(False);

     Check((a.Width = 4) and (a.Height = 1), 'Sum in place wrong size');
     Check((a[0, 0] = 1) and (a[1, 0] = 2) and (a[2, 0] = 3) and (a[3, 0] = 4), 'Wrong sum');
     a.Free;

     a := TDoubleMatrix.Create(4, 1);
     for i := 0 to 3 do
         a[i,0] := i + 1;
     a.SumInPlace(True);

     Check((a.Width = 1) and (a.Height = 1), 'Sum in place wrong size');
     Check(a[0, 0] = 10, 'Wrong Sum');
     a.Free;
end;

procedure TestTDoubleMatrix.TestSVD;
const cA : Array[0..11] of double = (1, 2, 3, 4, 5, 4, 3, 1, 1, 2, 1, 2);
var a : TDoubleMatrix;
    u, w, v : TDoubleMatrix;
begin
     a := TDoubleMatrix.Create(4, 3);
     a.Assign(cA, 4, 3);

     a.LinEQProgress := {$IFDEF FPC}@{$ENDIF}OnProgress;
     a.SVD(u, v, w, False);

     Check((u.Width = 3) and (u.Height = 3), 'Error U has the wrong dimension');
     Check((w.Width = 3) and (w.Height = 3), 'Error W has the wrong dimension');
     Check((v.Width = 4) and (v.Height = 3), 'Error V has the wrong dimension');

     status(WriteMtx(u.SubMatrix, u.Width));
     status(WriteMtx(w.SubMatrix, w.Width));
     status(WriteMtx(v.SubMatrix, v.Width));

     // check if U*W*V' = A
     w.MultInPlace(v);
     u.MultInPlace(w);

     Check( CheckMtx(a.SubMatrix, u.SubMatrix), 'Error U*W*V'' is not A');

     a.Free;
     U.Free;
     V.Free;
     w.Free;
end;

procedure TestTDoubleMatrix.TestSymEig;
var mtx : TDoubleMatrix;
    eigvals : TDoubleMatrix;
const cEigs : Array[0..3] of double = ( 0.207753, 0.407837, 0.848242, 2.536168 );
// testcase from a. mauri
begin
     mtx := TDoubleMatrix.Create(4, 4);
     try
        mtx.SetRow(0, [1.0000,    0.5000,    0.3333,    0.2500]);
        mtx.SetRow(1, [0.5000,    1.0000,    0.6667,    0.5000]);
        mtx.SetRow(2, [0.3333,    0.6667,    1.0000,    0.7500]);
        mtx.SetRow(3, [0.2500,    0.5000,    0.7500,    1.0000]);

        mtx.SymEig(eigvals);

        Check(CheckMtx(eigvals.SubMatrix, cEigs), 'Error calculating eigenvalues');
        eigvals.Free;
     finally
            mtx.Free;
     end;
end;

procedure TestTDoubleMatrix.TestTranspose;
var mtx : TDoubleMatrix;
begin
     {$IFDEF FMX} Setup; {$ENDIF};
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
     {$IFDEF FMX} TearDown; {$ENDIF};
end;

procedure TestTDoubleMatrix.OnProgress(Progress: Integer);
begin
     Status(IntToStr(Progress));
end;

procedure TestTDoubleMatrix.TestAppend;
var x : TDoubleMatrix;
    i: Integer;
    j: Integer;
begin
     x := TDoubleMatrix.CreateRand(4, 4);
     x.Append(x.Clone as IMatrix, False);

     for i := 0 to x.Width - 1 do
         for j := 0 to x.Height div 2 - 1 do
             Check( x[i, j] = x[i, j + 4], 'Appending rows failed' );



     x.Free;

     x := TDoubleMatrix.CreateRand(4, 4);
     x.Append(x.Clone as IMatrix, True);

     for i := 0 to x.Width div 2 - 1 do
         for j := 0 to x.Height - 1 do
             Check( x[i, j] = x[i + 4, j], 'Appending columns failed');

     x.Free;
end;

{ TestTThreadedMatrix }

procedure TestTThreadedMatrix.ElemWiseSqr(var elem: double);
begin
     elem := SQR(elem);
end;

procedure TestTThreadedMatrix.SetUp;
const cMtxWidth = 1024;
      cMtxHeight = 1024;
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
     {$IFDEF FMX} Setup; {$ENDIF};
     startTime1 := MtxGetTime;
     dest1 := fRefMatrix1.ElementwiseFunc({$IFDEF FPC}@{$ENDIF}ElemWiseSqr);
     endTime1 := MtxGetTime;

     m1 := TThreadedMatrix.Create;
     m1.Assign(fRefMatrix1);
     startTime2 := MtxGetTime;
     dest2 := m1.ElementwiseFunc({$IFDEF FPC}@{$ENDIF}ElemWiseSqr);
     endTime2 := MtxGetTime;

     status(Format('%.2f, %.2f', [(endTime1 - startTime1)/mtxFreq*1000, (endTime2 - startTime2)/mtxFreq*1000]));

     Check(CheckMtx(dest1.SubMatrix, dest2.SubMatrix));
     {$IFDEF FMX} TearDown; {$ENDIF};
end;

procedure TestTThreadedMatrix.TestCholesky;
var a, b, c : IMatrix;
    res : TCholeskyResult;
    startTime, endTime : Int64;
begin
     {$IFDEF FMX} Setup; {$ENDIF};
     a := TThreadedMatrix.CreateRand(2047, 2047);
     b := a.Transpose;
     a.MultInPlace(b);

     startTime := MtxGetTime;
     res := a.Cholesky(b);
     endTime := MtxGetTime;

     status(Format('Cholesky decomp of 2047x2047 matrix: %.2f', [(endTime - startTime)/mtxFreq*1000]));

     check(res = crOk, 'Error cholesky decomposition failed');

     c := b.Transpose;
     b.MultInPlace(c);

     check( CheckMtx(a.SubMatrix, b.SubMatrix), 'Cholesky error L*LT does not result in the original matrix');
     {$IFDEF FMX} TearDown; {$ENDIF};
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
     {$IFDEF FMX} Setup; {$ENDIF};
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

     // seems that the threaded version has a different error propagation than the single
     // threaded version so the difference is about 1e-10 (on that big matrix)
     Check(SameValue(det, detThr, Abs(det/1e10)), 'Error Determinatnts differ too much');
     {$IFDEF FMX} TearDown; {$ENDIF};
end;


procedure TestTThreadedMatrix.TestInvert;
const cBlkWidth = 1024;
      cBlkSize = cBlkWidth*cBlkWidth;
var x : TDoubleDynArray;
    i: Integer;
    start, stop : Int64;
    m1, m2 : IMatrix;
begin
     {$IFDEF FMX} Setup; {$ENDIF};
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
     {$IFDEF FMX} TearDown; {$ENDIF};
end;

procedure TestTThreadedMatrix.TestLeastSquares;
const cBlkWidth = 1024;
      cBlkSize = cBlkWidth*cBlkWidth;
var x : TDoubleDynArray;
    i: Integer;
    start, stop : Int64;
    m1, m2 : IMatrix;
    y : IMatrix;
    x1, x2 : IMatrix;
begin
     {$IFDEF FMX} Setup; {$ENDIF};
     // big inversion
     SetLength(x, cBlkSize);
     RandSeed := 15;
     for i := 0 to cBlkSize - 1 do
         x[i] := Random/3.8 - 0.5/3.8;

     y := TDoubleMatrix.CreateRand(1, cBlkWidth);

     m1 := TDoubleMatrix.Create(x, cBlkWidth, cBlkWidth);
     m2 := TThreadedMatrix.Create(x, cBlkWidth, cBlkWidth);

     start := MtxGetTime;
     check(m1.SolveLeastSquaresInPlace(x1, y) = qrOK, 'Error big least squares');
     stop := MtxGetTime;
     Status(Format('Big single least squares: %.2fms', [(stop - start)/mtxfreq*1000]));

     start := MtxGetTime;
     check(m2.SolveLeastSquaresInPlace(x2, y) = qrOk, 'Error inverting multi threaded version');
     stop := MtxGetTime;

     Status(Format('Big threaded least sqares: %.2fms', [(stop - start)/mtxfreq*1000]));

     Check(CheckMtx(m1.SubMatrix, m2.SubMatrix), 'Error threaded inverion differs from original one');
     Check(CheckMtx(x1.SubMatrix, x2.SubMatrix), 'Error least squares result is different');
     {$IFDEF FMX} TearDown; {$ENDIF};
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
     {$IFDEF FMX} Setup; {$ENDIF};
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
     {$IFDEF FMX} TearDown; {$ENDIF};
end;

procedure TestTThreadedMatrix.TestMedian;
const cBlkWidth = 2048;
      cBlkSize = cBlkWidth*cBlkWidth;
var a : TDoubleDynArray;
    i : integer;
    start, stop : int64;
    m1, m2 : IMatrix;
begin
     {$IFDEF FMX} Setup; {$ENDIF};
     SetLength(a, cBlkSize);

     RandSeed := 15;
     for i := 0 to cBlkSize - 1 do
         a[i] := Random - 0.5;

     m1 := TDoubleMatrix.Create(a, cBlkWidth, cBlkWidth);
     start := MtxGetTime;
     m1.MedianInPlace(True);
     stop := MtxGetTime;
     Status(Format('Single Thr Median: %.2fms', [(stop - start)/mtxfreq*1000]));

     m2 := TThreadedMatrix.Create(a, cBlkWidth, cBlkWidth);
     start := MtxGetTime;
     m2.MedianInPlace(True);
     stop := MtxGetTime;
     Status(Format('Multi Thr Median: %.2fms', [(stop - start)/mtxfreq*1000]));

     Check(CheckMtx(m1.SubMatrix, m2.SubMatrix), 'Error calculating threaded median');
     {$IFDEF FMX} TearDown; {$ENDIF};
end;

procedure TestTThreadedMatrix.TestSort;
const cBlkWidth = 2048;
      cBlkSize = cBlkWidth*cBlkWidth;
var a : TDoubleDynArray;
    i : integer;
    start, stop : int64;
    m1, m2 : IMatrix;
begin
     {$IFDEF FMX} Setup; {$ENDIF};
     SetLength(a, cBlkSize);

     RandSeed := 15;
     for i := 0 to cBlkSize - 1 do
         a[i] := Random - 0.5;

     m1 := TDoubleMatrix.Create(a, cBlkWidth, cBlkWidth);
     start := MtxGetTime;
     m1.SortInPlace(True);
     stop := MtxGetTime;
     Status(Format('Single Thr Sort: %.2fms', [(stop - start)/mtxfreq*1000]));

     m2 := TThreadedMatrix.Create(a, cBlkWidth, cBlkWidth);
     start := MtxGetTime;
     m2.SortInPlace(True);
     stop := MtxGetTime;
     Status(Format('Multi Thr Sort: %.2fms', [(stop - start)/mtxfreq*1000]));

     Check(CheckMtx(m1.SubMatrix, m2.SubMatrix), 'Error calculating threaded sorted matrix');

     m1 := TDoubleMatrix.Create(a, cBlkWidth, cBlkWidth);
     start := MtxGetTime;
     m1.SortInPlace(False);
     stop := MtxGetTime;
     Status(Format('Single Thr Sort: %.2fms', [(stop - start)/mtxfreq*1000]));

     m2 := TThreadedMatrix.Create(a, cBlkWidth, cBlkWidth);
     start := MtxGetTime;
     m2.SortInPlace(False);
     stop := MtxGetTime;
     Status(Format('Multi Thr Sort: %.2fms', [(stop - start)/mtxfreq*1000]));

     Check(CheckMtx(m1.SubMatrix, m2.SubMatrix), 'Error calculating threaded sorted matrix');
     {$IFDEF FMX} TearDown; {$ENDIF};
end;


procedure TestTThreadedMatrix.TestSVD;
const cBlkWidth = 512;
      cBlkSize = cBlkWidth*cBlkWidth;
var a : TDoubleDynArray;
    i : integer;
    start, stop : int64;
    m1, m2 : IMatrix;

    u1, v1, w1 : IMatrix;
    u2, v2, w2 : IMatrix;
begin
     {$IFDEF FMX} Setup; {$ENDIF};
     SetLength(a, cBlkSize);

     RandSeed := 15;
     for i := 0 to cBlkSize - 1 do
         a[i] := Random - 0.5;

     m1 := TDoubleMatrix.Create(a, cBlkWidth, cBlkWidth);
     m2 := TThreadedMatrix.Create(a, cBlkWidth, cBlkWidth);

     start := MtxGetTime;
     m1.SVD(u1, v1, w1, False);
     stop := MtxGetTime;
     Status(Format('Blocked SVD decomp: %.2fms', [(stop - start)/mtxfreq*1000]));

     start := MtxGetTime;
     m2.SVD(u2, v2, w2, False);
     stop := MtxGetTime;
     Status(Format('Threaded SVD decomp: %.2fms', [(stop - start)/mtxfreq*1000]));

     w1.MultInPlace(v1);
     u1.MultInPlace(w1);

     w2.MultInPlace(v2);
     u2.MultInPlace(w2);
     
     Check( CheckMtx( u1.SubMatrix, a ), 'Error non Threaded SVD');
     Check( CheckMtx( u2.SubMatrix, a ), 'Error threade SVD');
     {$IFDEF FMX} TearDown; {$ENDIF};
end;

procedure TestTThreadedMatrix.TestSymEig;
const cBlkWidth = 512;
      cBlkSize = cBlkWidth*cBlkWidth;
var a : TDoubleDynArray;
    i : integer;
    start, stop : int64;
    m1, m2 : IMatrix;

    e1, v1 : IMatrix;
    e2, v2 : IMatrix;
begin
     {$IFDEF FMX} Setup; {$ENDIF};
     SetLength(a, cBlkSize);

     RandSeed := 15;
     for i := 0 to cBlkSize - 1 do
         a[i] := Random - 0.5;

     m1 := TDoubleMatrix.Create(a, cBlkWidth, cBlkWidth);
     m2 := TThreadedMatrix.Create(a, cBlkWidth, cBlkWidth);

     start := MtxGetTime;
     m1.SymEig(e1, v1);
     stop := MtxGetTime;
     Status(Format('Symmetric eigenvalue: %.2fms', [(stop - start)/mtxfreq*1000]));

     start := MtxGetTime;
     m2.SymEig(e2, v2);
     stop := MtxGetTime;
     Status(Format('Threaded symmetric eigenvalue: %.2fms', [(stop - start)/mtxfreq*1000]));

     Check( CheckMtx( e1.SubMatrix, e2.SubMatrix ), 'Error in symmetric eigenvalue problem');
     Check( CheckMtx( v1.SubMatrix, v2.SubMatrix ), 'Error in symmetric eigenvector problem');
     {$IFDEF FMX} TearDown; {$ENDIF};
end;



procedure TestTThreadedMatrix.TestThreadMult;
var dest1, dest2 : IMatrix;
    m1 : IMatrix;
    startTime1, endTime1 : Int64;
    startTime2, endTime2 : Int64;
begin
     {$IFDEF FMX} Setup; {$ENDIF};
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
     {$IFDEF FMX} TearDown; {$ENDIF};
end;

{ TestIMatrix }

procedure TestIMatrix.SetUp;
begin
     fRefMatrix1 := ReadObjFromFile(BaseDataPath + 'MatrixData1.txt') as IMatrix;
     fRefMatrix2 := ReadObjFromFile(BaseDataPath + 'MatrixData2.txt') as IMatrix;
end;

procedure TestIMatrix.TearDown;
begin
     fRefMatrix2 := nil;
     fRefMatrix1 := Nil;
end;

procedure TestIMatrix.TesMedian;
var mtx1 : IMatrix;
    mtx2 : IMatrix;
begin
     {$IFDEF FMX} Setup; {$ENDIF};
     mtx1 := fRefMatrix1.Median(True);

     mtx2 := fRefmatrix1.Clone;
     mtx2.TransposeInPlace;
     mtx2.MedianInPlace(False);

     Check( CheckMtx(mtx1.SubMatrix, mtx2.SubMatrix), 'Median failed');
     {$IFDEF FMX}  TearDown; {$ENDIF};
end;

procedure TestIMatrix.TestAdd;
var mtx : IMatrix;
    dx : Array[0..49] of double;
    i : Integer;
begin
     {$IFDEF FMX} Setup; {$ENDIF};
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
     {$IFDEF FMX} TearDown; {$ENDIF};
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
     {$IFDEF FMX} Setup; {$ENDIF};
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
     {$IFDEF FMX} TearDown; {$ENDIF};
end;


procedure TestIMatrix.TestElementWiseDiv;
var mtx : IMatrix;
    x, y : integer;
begin
     mtx := TDoubleMatrix.Create(1, 1, -2);
     mtx.ElementWiseDivInPlace(mtx);

     Check(SameValue(mtx[0, 0], 1), 'Error 1x1 elementwise Div failed');

     mtx := TDoubleMatrix.Create(1, 2, -2);
     mtx.ElementWiseDivInPlace(mtx);
     Check(SameValue(mtx[0, 0], 1) and SameValue(mtx[0, 1], 1), 'Error 1x2 elementwise Div failed');

     mtx := TDoubleMatrix.Create(2, 2, -2);
     mtx.ElementWiseDivInPlace(mtx);
     Check(SameValue(mtx[0, 0], 1) and SameValue(mtx[0, 1], 1), 'Error 2x2 elementwise Div failed');
     Check(SameValue(mtx[1, 0], 1) and SameValue(mtx[1, 1], 1), 'Error 2x2 elementwise Div failed');

     mtx := TDoubleMatrix.Create(2, 3, -2);
     mtx.ElementWiseDivInPlace(mtx);
     Check(SameValue(mtx[0, 0], 1) and SameValue(mtx[0, 1], 1) and SameValue(mtx[0, 2], 1), 'Error 2x3 elementwise Div failed');
     Check(SameValue(mtx[1, 0], 1) and SameValue(mtx[1, 1], 1) and SameValue(mtx[1, 2], 1), 'Error 2x3 elementwise Div failed');

     mtx := TDoubleMatrix.Create(3, 2, -2);
     mtx.ElementWiseDivInPlace(mtx);
     Check(SameValue(mtx[0, 0], 1) and SameValue(mtx[0, 1], 1), 'Error 3x2 elementwise Div failed');
     Check(SameValue(mtx[1, 0], 1) and SameValue(mtx[1, 1], 1), 'Error 3x2 elementwise Div failed');
     Check(SameValue(mtx[2, 0], 1) and SameValue(mtx[2, 1], 1), 'Error 3x2 elementwise Div failed');


     mtx := TDoubleMatrix.Create(123, 124, -2);
     mtx.ElementWiseDivInPlace(mtx);
     for x := 0 to mtx.Width - 1 do
         for y := 0 to mtx.Height - 1 do
             Check(SameValue(mtx[x, y], 1), 'Error big elementwise mult failed @' + IntToStr(x) + ',' + IntToStr(y));
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
     {$IFDEF FMX} Setup; {$ENDIF};
     mtx1 := fRefMatrix2.Transpose;
     mtx := fRefMatrix1.Mult(mtx1);
     Check((mtx.width = mtx.height) and (mtx.width = 10), 'Mult Matrix dimension error');
     {$IFDEF FMX} TearDown; {$ENDIF};
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

procedure TestIMatrix.TestPinv;
var mtx, res : IMatrix;
begin
     mtx := ReadObjFromFile( BaseDataPath + 'pinvSpecial.json' ) as IMatrix;

     InitMathFunctions(itFPU, False);
     Check( mtx.PseudoInversion(res) = srOk, 'Pseudoinversion failed');
     Check( SameValue( res[0,0], 0.069, 1e-3));

     InitMathFunctions(itSSE, False);
     Check( mtx.PseudoInversion(res) = srOk, 'Pseudoinversion failed');
     Check( SameValue( res[0,0], 0.069, 1e-3));
     InitMathFunctions(itAVX, False);
     Check( mtx.PseudoInversion(res) = srOk, 'Pseudoinversion failed');
     Check( SameValue( res[0,0], 0.069, 1e-3));
     InitMathFunctions(itFMA, False);
     Check( mtx.PseudoInversion(res) = srOk, 'Pseudoinversion failed');
     Check( SameValue( res[0,0], 0.069, 1e-3));
end;

procedure TestIMatrix.TestSub;
var mtx : IMatrix;
    dx : Array[0..49] of double;
    i : Integer;
begin
     {$IFDEF FMX} Setup; {$ENDIF};
     // create reference
     for i := 0 to High(dx) do
         dx[i] := 1;

     mtx := fRefMatrix2.sub(fRefMatrix1);
     Check(CheckMtx(mtx.SubMatrix, dx), 'sub1 was wrong: ' + WriteMtxDyn(mtx.SubMatrix, mtx.width));

     mtx := TDoubleMatrix.Create;
     mtx.Assign(fRefMatrix2);
     mtx.SubInplace(fRefMatrix1);
     Check(CheckMtx(mtx.SubMatrix, dx), 'sub2 was wrong: ' + WriteMtxDyn(mtx.SubMatrix, mtx.width));
     {$IFDEF FMX} TearDown; {$ENDIF};
end;


procedure TestIMatrix.TestTranspose;
var mtx : IMatrix;
begin
     {$IFDEF FMX} Setup; {$ENDIF};
     mtx := TDoubleMatrix.Create;
     mtx.Assign(fRefMatrix1);
     mtx.TransposeInPlace;

     Check((mtx.Width = 10) and (mtx.Height = 5), 'Dimension error');

     mtx := fRefMatrix1.Transpose;
     Check((mtx.Width = 10) and (mtx.Height = 5), 'Dimension error');
     {$IFDEF FMX} TearDown; {$ENDIF};
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

procedure TestTDoubleMatrixPersistence.TestJSONWriteRead;
var mtx : TDoubleMatrix;
    dx : TDoubleDynArray;
    i : Integer;
begin
     SetLength(dx, 5*10);

     for i := 0 to Length(dx) - 1 do
         dx[i] := 2 + i;

     mtx := TDoubleMatrix.Create(dx, 5, 10);
     try
        TJsonReaderWriter.StaticSaveToFile(mtx, 'matrixData1.json');
     finally
            mtx.Free;
     end;

     for i := 0 to Length(dx) - 1 do
         dx[i] := 1 + i;

     mtx := TDoubleMatrix.Create(dx, 5, 10);
     try
        TJsonReaderWriter.StaticSaveToFile(mtx, 'matrixData2.json');
     finally
            mtx.Free;
     end;

     mtx := ReadObjFromFile('matrixData2.json') as  TDoubleMatrix;

     Check((mtx.Width = 5) and (mtx.Height = 10), 'JSON Reading failed');
     for i := 0 to mtx.Width*mtx.Height - 1 do
         Check( dx[i] = mtx.Vec[i], 'JSON Reading failed');
     mtx.Free;
end;


procedure TestTDoubleMatrixPersistence.TestJSONRead;
// test to check the correct reading of numeric values
const sJSNMtx : utf8String =
                '{"Lib":"mrMath","obj":"MTX","width_i":5,"height_i":2,"data":' + #13#10 +
                '[0.34,1e-5,-12.44e+3 , 4, -9.1e-1,' + #13#10 +
                '1, 2, 3, 44, 55 ]' + #13#10 +
                '}';
const jsnData : Array[0..9] of double = (0.34, 1e-5, -12.44e+3 , 4, -9.1e-1, 1, 2, 3, 44, 55);
var stream : TMemoryStream;
    obj : TDoubleMatrix;
begin
     stream := TMemoryStream.Create;
     try
        stream.Write(sJSNMtx[1], Length(sJSNMtx));
        stream.Position := 0;

        obj := ReadObjFromStream(stream) as TDoubleMatrix;
     finally
            stream.Free;
     end;

     Check(Assigned(obj), 'Error in json reader');
     Check(CheckMtx(obj.SubMatrix, jsnData, -1, -1, 1e-8), 'Error reading numerics from JSON string');
     obj.Free;
end;

initialization
{$IFNDEF FMX}
  RegisterTest(TestTDoubleMatrix{$IFNDEF FPC}.Suite{$ENDIF});
  RegisterTest(TestTDoubleMatrixPersistence{$IFNDEF FPC}.Suite{$ENDIF});
  RegisterTest(TestIMatrix{$IFNDEF FPC}.Suite{$ENDIF});
  RegisterTest(TestTThreadedMatrix{$IFNDEF FPC}.Suite{$ENDIF});
{$ENDIF}

end.
