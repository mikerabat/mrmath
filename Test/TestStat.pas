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
  end;

implementation

uses RandomEng, ThreadedMatrix, MatrixASMStubSwitch, Statistics, MatrixConst, Types;

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

initialization
{$IFNDEF FMX}
  RegisterTest(TTestStatFunc{$IFNDEF FPC}.Suite{$ENDIF});
{$ENDIF}

end.

