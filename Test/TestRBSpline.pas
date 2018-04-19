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

unit TestRBSpline;

interface 

// ###########################################
// #### Simple random generator tests (only functional tests no randomness tests!)
// ###########################################

uses {$IFDEF FPC} testregistry, {$ELSE} TestFramework, {$ENDIF}
     BaseMatrixTestCase, Classes, SysUtils, matrix, RandomEng;

type
  // testmethoden für die matrix funktionen
  TTestRBSpline = class(TBaseMatrixTestCase)
  private
    fRnd : TRandomGenerator;
    fNoiseAmpl : double;
    
    procedure GenSplineInput(var x, y, breaks : IMatrix; noiseAmpl : double; cl : TDoubleMatrixClass);

    procedure SinFunc(var value : double);
  published
    procedure SingleThreadSpline;
    procedure RobustSplineTest;
    procedure MultithreadSpline;
  end;

implementation

uses RBSplines, ThreadedMatrix, OptimizedFuncs;

{ TTestEM }

procedure TTestRBSpline.SinFunc(var value: double);
begin
     value := Sin(Value) + sin(2*value) + fNoiseAmpl*fRnd.RandGauss;
end;

procedure TTestRBSpline.GenSplineInput(var x, y, breaks: IMatrix; noiseAmpl : double; cl : TDoubleMatrixClass);
begin
     fNoiseAmpl := noiseAmpl;
     fRnd := TRandomGenerator.Create(raMersenneTwister);
     try
        x := cl.CreateRand(1, 200);
        x.ScaleInPlace(2*pi);
        x.SortInPlace(False);

        y := x.ElementwiseFunc( {$IFDEF FPC}@{$ENDIF}sinFunc );
     finally
            fRnd.Free;
     end;

     breaks := cl.CreateLinSpace(10, 0, 2*pi);
end;

procedure TTestRBSpline.MultithreadSpline;
var x, y, breaks : IMatrix;
    rb : TRobustBSpline;
    xx, yy : IMatrix;
    counter: Integer;
begin
     TThreadedMatrix.InitThreadPool;
     try
        TRobustBSpline.DefMatrixClass := TThreadedMatrix;

        GenSplineInput(x, y, breaks, 0.2, TThreadedMatrix);
     
       // WriteMatlabData('D:\x.txt', x.SubMatrix, x.Width);
       // WriteMatlabData('D:\y.txt', y.SubMatrix, y.Width);
     
        rb := TRobustBSpline.Create;
        try
           rb.InitSpline(x, y, breaks);

           xx := TDoubleMatrix.CreateLinSpace(400, 0, 2*pi);
           yy := rb.EvalSpline(xx);

       //    WriteMatlabData('D:\xx.txt', xx.SubMatrix, xx.Width);
       //    WriteMatlabData('D:\yy.txt', yy.SubMatrix, yy.Width);

           for counter := 10 to yy.VecLen - 1 do
               Check( Abs( yy.Vec[counter] - ( sin(xx.Vec[counter]) + sin(2*xx.Vec[counter]) )) < 0.25, 'Spline evaluation failed @' + IntToStr(counter) );
        finally
               rb.Free;
        end;
     finally
            TRobustBSpline.DefMatrixClass := TDoubleMatrix;;
            TThreadedMatrix.FinalizeThreadPool;
     end;
end;

procedure TTestRBSpline.RobustSplineTest;
var x, y, breaks : IMatrix;
    rb : TRobustBSpline;
    xx, yy : IMatrix;
    yyR : IMatrix;
    counter: Integer;
begin
     GenSplineInput(x, y, breaks, 0.05, TDoubleMatrix);
     
     rb := TRobustBSpline.Create( 4, 0.75 );
     try
        rb.InitSpline(x, y, breaks);

        xx := TDoubleMatrix.CreateLinSpace(400, 0, 2*pi);
        yy := rb.EvalSpline(xx);

        counter := 5;
        while counter < y.VecLen do
        begin  
             // create artificial outliers
             y.Vec[counter] := -1;
             inc(counter, 6);
        end;

        rb.InitSpline(x, y, breaks);

        yyR := rb.EvalSpline(xx);
        
        for counter := 0 to yy.VecLen - 1 do
            Check( Abs( yy.Vec[counter] - ( sin(xx.Vec[counter]) + sin(2*xx.Vec[counter]) )) < 0.1, 'Spline evaluation failed @' + IntToStr(counter) );

        for counter := 0 to yyR.VecLen - 1 do
            Check( Abs( yyR.Vec[counter] - ( sin(xx.Vec[counter]) + sin(2*xx.Vec[counter]) )) < 0.1, 'Spline evaluation failed @' + IntToStr(counter) );
     finally
            rb.Free;
     end; 
end;

procedure TTestRBSpline.SingleThreadSpline;
var x, y, breaks : IMatrix;
    rb : TRobustBSpline;
    xx, yy : IMatrix;
    counter: Integer;
begin
     TRobustBSpline.DefMatrixClass := TDoubleMatrix;

     GenSplineInput(x, y, breaks, 0.2, TDoubleMatrix);
     
     //WriteMatlabData('D:\x.txt', x.SubMatrix, x.Width);
     //WriteMatlabData('D:\y.txt', y.SubMatrix, y.Width);
     
     rb := TRobustBSpline.Create;
     try
        rb.InitSpline(x, y, breaks);

        xx := TDoubleMatrix.CreateLinSpace(400, 0, 2*pi);
        yy := rb.EvalSpline(xx);

     //   WriteMatlabData('D:\xx.txt', xx.SubMatrix, xx.Width);
     //   WriteMatlabData('D:\yy.txt', yy.SubMatrix, yy.Width);
        
        for counter := 0 to yy.VecLen - 1 do
            Check( Abs( yy.Vec[counter] - ( sin(xx.Vec[counter]) + sin(2*xx.Vec[counter]) )) < 0.25, 'Spline evaluation failed @' + IntToStr(counter) );
     finally
            rb.Free;
     end;
end;

initialization
  RegisterTest(TTestRBSpline{$IFNDEF FPC}.Suite{$ENDIF});

end.
