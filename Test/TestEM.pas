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

unit TestEM;

interface

// ###########################################
// #### Simple Expectation maximization tests
// ###########################################

uses {$IFDEF FPC} testregistry, {$ELSE} TestFramework, {$ENDIF}
     BaseMatrixTestCase, Classes, SysUtils, Types, matrix;

type
  // testmethoden für die matrix funktionen
  TTestEM = class(TBaseMatrixTestCase)
  private
    function PrepareEM( cl : TDoubleMatrixClass ) : IMatrix;
  published
    procedure SingleThreadEM;
    procedure MultithreadEM;
  end;

implementation

uses EM, RandomEng, ThreadedMatrix, OptimizedFuncs;

{ TestEM }

procedure TTestEM.SingleThreadEM;
var x : IMatrix;
    W, M : IMatrix;
    V : IMatrixDynArr;
    counter: integer;
begin
     x := PrepareEM( TDoubleMatrix );

     with TExpectationMax.Create do
     try
        MatrixClass := TDoubleMatrix;

        Check( Estimate(x, 2, W, M, V) = True, 'Estimate failed');

        Status('W:');
        Status( WriteMtx(W.GetObjRef));
        Status('');

        Status('M:');
        Status( WriteMtx(M.GetObjRef));
        Status('');
        
        for counter := 0 to Length(V) - 1 do
        begin
             Status('v:' + IntToStr(counter + 1));
             Status( WriteMtx(V[counter].GetObjRef));
             Status('');
        end;
     finally
            Free;
     end;
end;


procedure TTestEM.MultithreadEM;
var x : IMatrix;
    W, M : IMatrix;
    V : IMatrixDynArr;
    counter: integer;
begin
     InitMathFunctions(itFPU, false);
     TThreadedMatrix.InitThreadPool;
     try
        x := PrepareEM( TThreadedMatrix );

        with TExpectationMax.Create do
        try
           MatrixClass := TThreadedMatrix;

           Check( Estimate(x, 2, W, M, V) = True, 'Estimate failed');

           Status('W:');
           Status( WriteMtx(W.GetObjRef));
           Status('');

           Status('M:');
           Status( WriteMtx(M.GetObjRef));
           Status('');
        
           for counter := 0 to Length(V) - 1 do
           begin
                Status('v:' + IntToStr(counter + 1));
                Status( WriteMtx(V[counter].GetObjRef));
                Status('');
           end;
        finally
               Free;
        end;
     finally
            TThreadedMatrix.FinalizeThreadPool;
     end;
end;


function TTestEM.PrepareEM( cl : TDoubleMatrixClass ): IMatrix;
var i: Integer;
    rnd : TRandomGenerator;
begin
     rnd := TRandomGenerator.Create(raSystem);
     rnd.Init(1023);
     Result := cl.Create(2, 100);

     // first class:
     for i := 0 to 50 - 1 do
     begin
          Result[0, i] := 0.5 + rnd.RandGauss*2;
          Result[1, i] := 0.1 + rnd.RandGauss*1.2;
     end;

     for i := 50 to 100 - 1 do
     begin
          Result[0, i] := 3.2 + rnd.RandGauss*0.2;
          Result[1, i] := 2.5 + rnd.RandGauss*1.2;
     end;

     rnd.Free;
end;

initialization
  RegisterTest(TTestEM{$IFNDEF FPC}.Suite{$ENDIF});

end.
