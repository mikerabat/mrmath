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
   procedure TestASM;
 end;


implementation

uses tSNE, RandomEng, MatrixASMStubSwitch;
{ TTestTSNE }

procedure TTestTSNE.TestASM;
var x : IMatrix;
    i, j : Integer;
    rnd : TRandomGenerator;
    xmap, xmap2 : IMatrix;
    ndim : integer;
begin
     rnd := TRandomGenerator.Create( raMersenneTwister );
     rnd.Init( 573 ); // ensure always the same start

     try
        for ndim := 2 to 5 do
        begin
             // just pure pascal code
             x := TDoubleMatrix.Create( nDim, 30 );

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

             InitMathFunctions(itFPU, false);
             xmap := TtSNE.SymTSNE(X.GetObjRef, ndim, ndim, 5, 0.3, 1000, dfOrig, 221, raSystem);

             InitMathFunctions(itSSE, false);
             xmap2 := TtSNE.SymTSNE(X.GetObjRef, ndim, ndim, 5, 0.3, 1000, dfOrig, 221, raSystem);


             // only for 2 dim the accuracy is good enough that we get the same results regardless the instruction set used
             if ndim = 2 then
                Check( CheckMtx( xmap.SubMatrix, xmap2.SubMatrix ), 'ASM Version for TSNE differs');
        end;

     finally
            rnd.Free;
     end;
end;


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
