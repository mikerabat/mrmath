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
    procedure TestPolynomFit;
    procedure TestEvalPolynom;
    procedure TestLinRegress;
  end;

implementation

uses Types, NonLinearFit, OptimizedFuncs;

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

procedure TestNonLinFitOptimization.TestEvalPolynom;
const cNumPts : integer = 20;
var x, y : TDoubleDynArray;
    idx : integer;
    a : IMatrix;
    val : double;
    x1 : IMatrix;
    y1 : IMatrix;
begin
     SetLength(x, cNumPts);
     SetLength(y, cNumPts);
     // create polynome 4 order from [-4, 4]

     for idx := 0 to cNumPts - 1 do
     begin
          val := -4 + 8/cNumPts*idx;;
          x[idx] := val;
          y[idx] := 1 + 0.2*val - 0.8*sqr(val) + 0.25*sqr(val)*val + 0.1*sqr(val)*sqr(val);
     end;

     x1 := TDoubleMatrix.Create( x, Length(x), 1 );

     with TNonLinFitOptimizer.Create do
     try
        a := PolynomFit(x, y, 4);
        y1 := EvalPoly(x1, a);
     finally
            Free;
     end;

     for idx := 0 to Length(y) - 1 do
         Check( abs(y1.Vec[idx] - y[idx]) < 1e-6, 'Error polynom eval failed');
end;

procedure TestNonLinFitOptimization.TestLinRegress;
var x, y : IMatrix;
    counter: Integer;
    coef1, coef2 : IMatrix;
begin
     y := TDoubleMatrix.CreateRand(1, 20);
     x := TDoubleMatrix.CreateLinSpace(20, -5, 5);

     y.ScaleInPlace(0.1);
     for counter := 0 to y.Height - 1 do
         y.Vec[counter] := y.Vec[counter] + x.Vec[counter]*5 - 3;

     with TNonLinFitOptimizer.Create do
     try
        coef1 := PolynomFit(x, y, 1);
        coef2 := LineFit(x, y);
     finally
            Free;
     end;

     Check( (coef1.Width = coef2.Width) and (coef2.Height = coef1.Height), 'Error wrong dimensions');
     Check( CheckMtx( coef1.SubMatrix, coef2.SubMatrix), 'Error in linefit');
end;

procedure TestNonLinFitOptimization.TestPolynomFit;
const cNumPts : integer = 20;
var x, y : TDoubleDynArray;
    idx : integer;
    a : IMatrix;
    val : double;
begin
     SetLength(x, cNumPts);
     SetLength(y, cNumPts);
     // create polynome 4 order from [-4, 4]

     for idx := 0 to cNumPts - 1 do
     begin
          val := -4 + 8/cNumPts*idx;;
          x[idx] := val;
          y[idx] := 1 + 0.2*val - 0.8*sqr(val) + 0.25*sqr(val)*val + 0.1*sqr(val)*sqr(val);
     end;

     with TNonLinFitOptimizer.Create do
     try
        a := PolynomFit(x, y, 4);
     finally
            Free;
     end;

     Status(WriteMtx(a.SubMatrix, a.Width));
     Check(CheckMtx(a.SubMatrix, [0.1, 0.25, -0.8, 0.2, 1]), 'Error polynom fit failed');
end;

initialization
  RegisterTest(TestNonLinFitOptimization{$IFNDEF FPC}.Suite{$ENDIF});

end.
