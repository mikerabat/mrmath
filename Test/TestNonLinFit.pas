unit TestNonLinFit;

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

interface

uses {$IFDEF FPC} testregistry {$ELSE} TestFramework {$ENDIF} ,
     Classes, SysUtils, Matrix, BaseMatrixTestCase;

type
  TestNonLinFitOptimization = class(TBaseMatrixTestCase)
  private
    procedure OnAtanIterate(Sender : TObject; a, x, y : IMatrix);
  published
    procedure TestArctanOpt;
  end;

implementation

uses NonLinearFit, OptimizedFuncs;

{ TestTDoubleMatrix }

procedure TestNonLinFitOptimization.OnAtanIterate(Sender: TObject; a, x, y: IMatrix);
var i: Integer;
begin
     for i := 0 to x.Height - 1 do
         y[0, i] := a[0, 0]*ArcTan(a[0, 1]*x[0, i] + a[0, 2]) + a[0, 3];
end;

procedure TestNonLinFitOptimization.TestArctanOpt;
var x, y, a0, a1, a : IMatrix;
    i : integer;
begin
     x := TDoubleMatrix.Create(1, 100);
     for i := 0 to x.height - 1 do
         x[0, i] := -10 + 20*(i/x.height);
     a0 := TDoubleMatrix.Create(1, 4);
     y := TDoubleMatrix.Create(1, 100);
     a0[0, 0] := 5;
     a0[0, 1] := 1.4;
     a0[0, 2] := 0.3;
     a0[0, 3] := 2.2;
     OnAtanIterate(self, a0, x, y);

     a1 := TDoubleMatrix.Create(1, 4);
     a1[0, 0] := 2;
     a1[0, 1] := 0.5;
     a1[0, 2] := 0.7;
     a1[0, 3] := 1.9;

     with TNonLinFitOptimizer.Create do
     try
        OnIterateObj := {$IFDEF FPC}@{$ENDIF}OnAtanIterate;
        a := Optimize(x, y, a1);
     finally
            Free;
     end;

     Check(CheckMtx(a0.SubMatrix, a.SubMatrix, -1, -1, 0.02), 'Differences too high');
end;

initialization
  RegisterTest(TestNonLinFitOptimization{$IFNDEF FPC}.Suite{$ENDIF});

end.
