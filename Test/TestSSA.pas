// ###################################################################
// #### This file is part of the mathematics library project, and is
// #### offered under the licence agreement described on
// #### http://www.mrsoft.org/
// ####
// #### Copyright:(c) 2020, Michael R. . All rights reserved.
// ####
// #### Unless required by applicable law or agreed to in writing, software
// #### distributed under the License is distributed on an "AS IS" BASIS,
// #### WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// #### See the License for the specific language governing permissions and
// #### limitations under the License.
// ###################################################################

unit TestSSA;

interface

{$IFDEF MACOS}
   {$DEFINE FMX}
{$ENDIF}

uses {$IFDEF FPC} testregistry {$ELSE} {$IFDEF FMX}DUnitX.TestFramework {$ELSE}TestFramework {$ENDIF} {$ENDIF} ,
     Classes, SysUtils, BaseMatrixTestCase;

type
  {$IFDEF FMX} [TestFixture] {$ENDIF}
  TTestSSA = class(TBaseImgTestCase)

  published
    procedure Test1;
    procedure TestOVerlap;
  end;

implementation

uses Matrix, ssa, RandomEng, Types, MatrixASMStubSwitch, Windows, Dialogs;

{ TTestSSA }

procedure TTestSSA.Test1;
var x, y : IMatrix;
    i : integer;
    rnd : TRandomGenerator;
const T = 22;
      N = 100;
      M = 30;
begin
     rnd := TRandomGenerator.Create(raMersenneTwister);
     rnd.Init(721);

     // ###########################################
     // #### Create a test sine signal
     x := TDoubleMatrix.Create( N, 1 );

     for i := 0 to x.VecLen - 1 do
         x.Vec[i] := sin(2*pi*i/T) + 0.3*rnd.RandGauss;

     rnd.Free;

     //MatrixToTxtFile('D:\x.txt', x.GetObjRef);

     // ###########################################
     // #### Just execute the ssa
     with TSingularSpectrumAnalysis.Create do
     try
        CalcSSA(x, M);

        y := Reconstruct(2);
     finally
            Free;
     end;

     //MatrixToTxtFile('D:\y.txt', y.GetObjRef);
end;

procedure TTestSSA.TestOVerlap;
var x : IMatrix;
    res : IMatrix;
begin
     x := MatrixFromTxtFile(BaseDataPath + 'ssaTest.txt');
     res := TSingularSpectrumAnalysis.CalcSSAOverlap(x, 24, 60, 2, 14, nil);
     //MatrixToTxtFile('D:\SSAexample\delphiOut.txt', res.GetObjRef, 4);
end;

initialization
{$IFNDEF FMX}
  RegisterTest(TTestSSA{$IFNDEF FPC}.Suite{$ENDIF});
{$ENDIF}


end.
