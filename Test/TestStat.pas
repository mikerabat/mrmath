// ###################################################################
// #### This file is part of the mathematics library project, and is
// #### offered under the licence agreement described on
// #### http://www.mrsoft.org/
// ####
// #### Copyright:(c) 2018, Michael R. . All rights reserved.
// ####
// #### Unless required by applicable law or agreed to in writing, software
// #### distributed under the License is distributed on an "AS IS" BASIS,
// #### WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// #### See the License for the specific language governing permissions and
// #### limitations under the License.
// ###################################################################

unit TestStat;

interface

// ###########################################
// #### Simple Expectation maximization tests
// ###########################################

{$IFDEF MACOS}
  {$DEFINE FMX}
{$ENDIF}

uses {$IFDEF FPC} testregistry, {$ELSE} {$IFDEF FMX}DUnitX.TestFramework {$ELSE}TestFramework {$ENDIF}, {$ENDIF}
     BaseMatrixTestCase, Classes, SysUtils, matrix;

type
  // testmethoden für die matrix funktionen
  {$IFDEF FMX} [TestFixture] {$ENDIF}
  TTestStatFunc = class(TBaseMatrixTestCase)
  published
    procedure TestStudentT1;
    procedure TestStudentT2;
    procedure TestStudentT3;
    procedure TestFTest;
    procedure TestChiSquare1;
    procedure TestKomogorov1;
    procedure TestKolmogorov2;
  end;

implementation

uses RandomEng, ThreadedMatrix, MatrixASMStubSwitch, Statistics, MatrixConst, Types,
     Math;

{ TestEM }

{ TTestStatFunc }

procedure TTestStatFunc.TestStudentT1;
var rnd : TRandomGenerator;
    dat1 : Array[0..5] of TDoubleDynArray;
    y : Integer;
    x: Integer;
    t, prob : double;
const cMean : Array[0..5] of double = (0.5, -0.1, 2, 0.4, 0.5, -0.101);
      cNum : Array[0..5] of integer = (13000, 14400, 5000, 1000, 8000, 10000);
begin
     InitMathFunctions(itFPU, False);
     // test with same variance -> test just checks if the routine does not crash
     rnd := TRandomGenerator.Create(raMersenneTwister);
     rnd.Init( 455 );

     try
        for y := 0 to High(dat1) do
        begin
             SetLength( dat1[y], cNum[y] );
             for x := 0 to Length(dat1[y]) - 1 do
                 dat1[y][x] := sqr(0.7)*rnd.RandGauss + cMean[y];
        end;
     finally
            rnd.Free;
     end;

     for y := 0 to High(dat1) - 1 do
     begin
          for x := y + 1 to High(dat1) do
          begin
               StudentTTest(@dat1[y][0], cNum[y], @dat1[x][0], cNum[x], t, prob);

               Status( Format('Student T %.2f, %d, %.2f, %d, %.3f, %.3f', [cMean[y], cNum[y], cMean[x], cNum[x], t, prob] ) );

               if abs(cMean[y] - cMean[x]) < 0.02  then
               begin
                    StudentTTest(@dat1[y][0], cNum[y], @dat1[x][0], cNum[x], t, prob);
                    Check( prob > 0.1, 'Student t Probability failed');
               end;
          end;
     end;
end;

procedure TTestStatFunc.TestStudentT2;
const cVar : Array[0..5] of double = (0.7, 1, 0.2, 1.1, 0.7, 0.22);
      cMean : Array[0..5] of double = (0.5, -0.1, 2, 0.4, 0.5, -0.101);

      cNum : Array[0..5] of integer = (13000, 14400, 5000, 1000, 8000, 10000);

var rnd : TRandomGenerator;
    dat1 : Array[0..5] of TDoubleDynArray;
    y : Integer;
    x: Integer;
    t, prob : double;
begin
     InitMathFunctions(itFPU, False);
     // test with same variance -> test just checks if the routine does not crash
     rnd := TRandomGenerator.Create(raMersenneTwister);
     rnd.Init( 455 );

     try
        for y := 0 to High(dat1) do
        begin
             SetLength( dat1[y], cNum[y] );
             for x := 0 to Length(dat1[y]) - 1 do
                 dat1[y][x] := sqr(cVar[y])*rnd.RandGauss + cMean[y];
        end;
     finally
            rnd.Free;
     end;

     for y := 0 to High(dat1) - 1 do
     begin
          for x := y + 1 to High(dat1) do
          begin
               // student test with different variances
               StudentTUTest(@dat1[y][0], cNum[y], @dat1[x][0], cNum[x], t, prob);

               Status( Format('Student TU %.2f, %d, %.2f, %d, %.3f, %.3f', [cMean[y], cNum[y], cMean[x], cNum[x], t, prob] ) );

               if abs(cVar[y] - cVar[x]) < 0.02  then
               begin
                    StudentTUTest(@dat1[y][0], cNum[y], @dat1[x][0], cNum[x], t, prob);
                    Check( prob > 0.1, 'Student t Probability failed');
               end;
          end;
     end;
end;

procedure TTestStatFunc.TestStudentT3;
var rnd : TRandomGenerator;
    dat1 : Array[0..5] of TDoubleDynArray;
    y : Integer;
    x: Integer;
    t, prob : double;
const cVar : Array[0..5] of double = (0.7, 1, 0.2, 1.1, 0.7, 0.22);
      cMean : Array[0..5] of double = (0.5, -0.1, 2, 0.4, 0.5, -0.101);
      cNum : integer = 1000;
begin
     InitMathFunctions(itFPU, False);
     // test with same variance -> test just checks if the routine does not crash
     rnd := TRandomGenerator.Create(raMersenneTwister);
     rnd.Init( 455 );

     try
        for y := 0 to High(dat1) do
        begin
             SetLength( dat1[y], cNum );
             for x := 0 to Length(dat1[y]) - 1 do
                 dat1[y][x] := sqr(cVar[y])*rnd.RandGauss + cMean[y];
        end;
     finally
            rnd.Free;
     end;

     for y := 0 to High(dat1) - 1 do
     begin
          for x := y + 1 to High(dat1) do
          begin
               // student test with different variances
               StudentTPTest(@dat1[y][0], @dat1[x][0], cNum, t, prob);

               Status( Format('Student TU %.2f, %.2f, %d, %.3f, %.3f', [cMean[y], cMean[x], cNum, t, prob] ) );
          end;
     end;
end;

procedure TTestStatFunc.TestFTest;
var rnd : TRandomGenerator;
    dat1 : Array[0..5] of TDoubleDynArray;
    y : Integer;
    x: Integer;
    f, prob : double;
const cVar : Array[0..5] of double = (0.7, 1, 0.2, 1.1, 0.7, 0.22);
      cMean : Array[0..5] of double = (0.5, -0.1, 2, 0.4, 0.5, -0.101);
      cNum : Array[0..5] of integer = (900, 1000, 1200, 888, 1500, 999);
begin
     InitMathFunctions(itFPU, False);
     // test with same variance -> test just checks if the routine does not crash
     rnd := TRandomGenerator.Create(raMersenneTwister);
     rnd.Init( 455 );

     try
        for y := 0 to High(dat1) do
        begin
             SetLength( dat1[y], cNum[y] );
             for x := 0 to Length(dat1[y]) - 1 do
                 dat1[y][x] := sqr(cVar[y])*rnd.RandGauss + cMean[y];
        end;
     finally
            rnd.Free;
     end;

     for y := 0 to High(dat1) - 1 do
     begin
          for x := y + 1 to High(dat1) do
          begin
               // student test with different variances
               FTest(@dat1[y][0], cNum[y], @dat1[x][0], cNum[x], f, prob);

               Status( Format('FTest %.2f, %d, %.2f, %d, %.3f, %.3f', [cMean[y], cNum[y], cMean[x], cNum[y], f, prob] ) );
          end;
     end;
end;

procedure TTestStatFunc.TestChiSquare1;
var rnd : TRandomGenerator;
    ref : TDoubleDynArray;
    dat1 : TDoubleDynArray;
    dat2 : TDoubleDynArray;
    x : integer;
    df, chsq, prob : double;
    hist1, hist2, refHist : TDoubleDynArray;
    minVal, maxVal : double;
begin
     rnd := TRandomGenerator.Create(raMersenneTwister);
     try
        rnd.Init(973);

        // gauss distribution
        SetLength( ref, 1200 );
        for x := 0 to Length(ref) - 1 do
            ref[x] := sqr(0.8)*rnd.RandGauss + 2.4;
        SetLength( dat1, 1200 );
        for x := 0 to Length(dat1) - 1 do
            dat1[x] := sqr(0.8)*rnd.RandGauss + 2.4;
        // unit distribution
        SetLength( dat2, 900 );
        for x := 0 to Length(dat2) - 1 do
            dat2[x] := 2.4 + rnd.Random;

        minVal := MatrixMin(@ref[0], Length(ref), 1, Length(ref)*sizeof(double));
        maxVal := MatrixMax(@ref[0], Length(ref), 1, Length(ref)*sizeof(double));

        minVal := Min(minVal, MatrixMin(@dat1[0], Length(dat1), 1, Length(dat1)*sizeof(double)));
        maxVal := Max(maxVal, MatrixMax(@dat1[0], Length(dat1), 1, Length(dat1)*sizeof(double)));

        minVal := Min(minVal, MatrixMin(@dat2[0], Length(dat2), 1, Length(dat2)*sizeof(double)));
        maxVal := Max(maxVal, MatrixMax(@dat2[0], Length(dat2), 1, Length(dat2)*sizeof(double)));

        SetLength(hist1, 50);
        SetLength(hist2, 50);
        SetLength(refHist, 50);
        Hist(@ref[0], Length(ref), @refHist[0], Length(refHist), minVal, maxVal);
        Hist(@dat1[0], Length(dat1), @hist1[0], Length(hist1), minVal, maxVal);
        Hist(@dat2[0], Length(dat2), @hist2[0], Length(hist2), minVal, maxVal);

        for x := 0 to Length(refHist) - 1 do
            refHist[x] := max(0.001, refHist[x]);

        ChiSquareOne(@hist1[0], @refHist[0], Length(hist1), 1, df, chsq, prob);

        Status(Format('ChiSquare on similar distributions: %3.f, %.3f, %.3f', [df, chsq, prob]));

        ChiSquareOne(@hist2[0], @refHist[0], Length(hist1), 1, df, chsq, prob);
        Status(Format('ChiSquare on different distributions: %3.f, %.3f, %.3f', [df, chsq, prob]));
     finally
            rnd.Free;
     end;
end;

function UniformPDF(x : double) : double;
begin
     Result := x;  // just a linear relationship between 0 and 1
end;

procedure TTestStatFunc.TestKomogorov1;
var rnd : TRandomGenerator;
    data : TDoubleDynArray;
    i : Integer;
    d, prob : double;
const cPVal = 0.85746;      // from octave... maybe off by a bit
begin
     rnd := TRandomGenerator.Create(raMersenneTwister);
     try
        rnd.Init(763);

        // uniformly distributed -> linear increasing when sorted
        SetLength(data, 10000);
        for i := 0 to Length(data) - 1 do
            data[i] := rnd.Random; // i/1000 + 0.00001*(rnd.Random - 0.5);
     finally
            rnd.Free;
     end;

     //WriteMatlabData('D:\kolm.txt', data, Length(data));

     kolmogorovSmirnovOne(@data[0], Length(data), UniformPDF, d, prob);
     Status(Format('Kolmogoriv result on uniform distributions: %.4f   ,    %.3f', [d, prob]));
     Check(SameValue( prob, cPVal, 1e-3 ), 'Error uniform distribution p value wrong' );
end;

procedure TTestStatFunc.TestKolmogorov2;
var rnd : TRandomGenerator;
    data : TDoubleDynArray;
    data2 : TDoubleDynArray;
    i : Integer;
    d, prob : double;
const cRef : double = 0.0087727;
      cP : double = 0.813357;          //from matlab
begin
     rnd := TRandomGenerator.Create(raMersenneTwister);
     try
        rnd.Init(763);

        // create 2 near linear (uniform distribution) pdfs
        SetLength(data, 10000);
        for i := 0 to Length(data) - 1 do
            data[i] := rnd.Random; // i/1000 + 0.00001*(rnd.Random - 0.5);
        SetLength(data2, 11000);
        for i := 0 to Length(data2) - 1 do
            data2[i] := rnd.Random;
     finally
            rnd.Free;
     end;

     kolmogorovSmirnovTwo(@data[0], Length(data), @data2[0], Length(data2), d, prob);
     Status(Format('Kolmogoriv result on uniform distributions: %.4f   ,    %.3f', [d, prob]));

     check( SameValue(d, cRef, 1e-3), 'Distance wrong');
     check( SameValue(prob, cP, 1e-3), 'P value wrong');
end;


initialization
{$IFNDEF FMX}
  RegisterTest(TTestStatFunc{$IFNDEF FPC}.Suite{$ENDIF});
{$ENDIF}

end.

