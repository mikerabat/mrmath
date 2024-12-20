// ###################################################################
// #### This file is part of the mathematics library project, and is
// #### offered under the licence agreement described on
// #### http://www.mrsoft.org/
// ####
// #### Copyright:(c) 2019, Michael R. . All rights reserved.
// ####
// #### Unless required by applicable law or agreed to in writing, software
// #### distributed under the License is distributed on an "AS IS" BASIS,
// #### WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// #### See the License for the specific language governing permissions and
// #### limitations under the License.
// ###################################################################

unit TestTSNE;

interface

{$IFDEF MACOS}
   {$DEFINE FMX}
{$ENDIF}

uses {$IFDEF FPC} testregistry {$ELSE} {$IFDEF FMX}DUnitX.TestFramework {$ELSE}TestFramework {$ENDIF} {$ENDIF},
     Classes, SysUtils, BaseMatrixTestCase, Matrix;

type
{$IFDEF FMX} [TestFixture] {$ENDIF}
 TTestTSNE = class(TBaseImgTestCase)
 published
   procedure TestGauss;
   procedure TestGaussBarnesHut;
 end;


implementation

uses tSNE, RandomEng, MatrixASMStubSwitch;
{ TTestTSNE }

procedure TTestTSNE.TestGauss;
var x : IMatrix;
    i, j : Integer;
    rnd : TRandomGenerator;
    xmap : IMatrix;
begin
     // just pure pascal code
     InitMathFunctions(itFPU, false);
     rnd := TRandomGenerator.Create( raMersenneTwister );
     rnd.Init( 573 ); // ensure always the same start

     x := TDoubleMatrix.Create( 2, 30 );

     for i := 0 to x.Height div 2 - 1 do
     begin
          for j := 0 to x.Width - 1 do
              x[j, i] := 0.5 + 0.2*rnd.RandGauss;
     end;

     for i := x.Height div 2 to x.Height - 1 do
     begin
          for j := 0 to x.Width - 1 do
              x[j, i] := -0.5 + 0.2*rnd.RandGauss;
     end;

     rnd.Free;

     //MatrixToTxtFile('D:\tsne.txt', x.GetObjRef);
     //WriteBinary('D:\tsne_bin.dat', x);
     xmap := TtSNE.SymTSNE(X.GetObjRef, 2, 2, 6, 0);

     check((xmap.Width = 2) and (xmap.Height = x.Height), 'Dimension error in tsne');
     //MatrixToTxtFile('D:\tsne_xmap.txt', xmap.GetObjRef);
end;

procedure TTestTSNE.TestGaussBarnesHut;
var x : IMatrix;
    i, j : Integer;
    rnd : TRandomGenerator;
    xmap : IMatrix;
begin
     // just pure pascal code
     InitMathFunctions(itFPU, false);
     rnd := TRandomGenerator.Create( raMersenneTwister );
     rnd.Init( 573 ); // ensure always the same start

     x := TDoubleMatrix.Create( 2, 30 );

     for i := 0 to x.Height div 2 - 1 do
     begin
          for j := 0 to x.Width - 1 do
              x[j, i] := 0.5 + 0.2*rnd.RandGauss;
     end;

     for i := x.Height div 2 to x.Height - 1 do
     begin
          for j := 0 to x.Width - 1 do
              x[j, i] := -0.5 + 0.2*rnd.RandGauss;
     end;

     rnd.Free;

     //MatrixToTxtFile('D:\tsne.txt', x.GetObjRef);
     //WriteBinary('D:\tsne_bin.dat', x);
     xmap := TtSNE.SymTSNE(X.GetObjRef, 2, 2, 6, 0.5);

     check((xmap.Width = 2) and (xmap.Height = x.Height), 'Dimension error in tsne');
     //MatrixToTxtFile('D:\tsne_xmap.txt', xmap.GetObjRef);
end;


initialization
{$IFNDEF FMX}
  RegisterTest(TTestTSNE{$IFNDEF FPC}.Suite{$ENDIF});
{$ENDIF}


end.
