unit TestICA;

// ###################################################################
// #### This file is part of the mathematics library project, and is
// #### offered under the licence agreement described on
// #### http://www.mrsoft.org/
// ####
// #### Copyright:(c) 2015, Michael R. . All rights reserved.
// ####
// #### Unless required by applicable law or agreed to in writing, software
// #### distributed under the License is distributed on an "AS IS" BASIS,
// #### WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// #### See the License for the specific language governing permissions and
// #### limitations under the License.
// ###################################################################

interface

{$IFDEF MACOS}
  {$DEFINE FMX}
{$ENDIF}

uses {$IFDEF FPC} testregistry {$ELSE} {$IFDEF FMX}DUnitX.TestFramework {$ELSE}TestFramework {$ENDIF} {$ENDIF},
     Classes, SysUtils, Types, Matrix, BaseMatrixTestCase;

type
  {$IFDEF FMX} [TestFixture] {$ENDIF}
  TTestICA = class(TBaseImgTestCase)
  published
    procedure TestSimpleICA;
    procedure TestICAImages;
    procedure TestNew;
  end;

implementation

uses ICA;

{ TestNonLinFitOptimization }

procedure TTestICA.TestICAImages;
var examples : TDoubleMatrix;
    ica : TMatrixICA;
    w, h : integer;
    counter: integer;
    projMtx : IMatrix;
    reconMtx : IMatrix;
    props : TICAProps;
begin
     Examples := LoadImages(w, h);

     ica := TMatrixICA.Create;
     try
        props := TMatrixICA.DefProps;
        props.NumIC := 10;
        ica.ICA(examples, props);

        for counter := 0 to Examples.Width - 1 do
        begin
             Examples.SetSubMatrix(counter, 0, 1, Examples.Height);
             projMtx := ica.ProjectToFeatureSpace(Examples);
             reconMtx := ica.Reconstruct(projMtx.GetObjRef);

             ImageFromMatrix(reconMtx.GetObjRef, w, h, Format('%s%sica_%d.bmp', [ExtractFilePath(ParamStr(0)), PathDelim, counter]));
        end;
     finally
            ica.Free;
     end;

     Examples.Free;
end;

procedure TTestICA.TestNew;
var data : TDoubleDynArray;
    counter : integer;
    examples : TDoubleMatrix;
    ica : TMatrixICA;
    props : TICAProps;
begin
     Check( true, 'nix');
     exit;

     setLength(data, 1000);
     for counter := 0 to length ( data ) div 2 -1 do
     begin
          data [ counter ] := random/10 + sin ( 2*pi/30*counter ) - sin ( 2*pi/37*counter );
          data [ counter + length ( data ) div 2] := random/10 + 0.9 * sin ( 2*pi/30*counter ) + 1.2 * sin ( 2*pi/37*counter );
     end;
     examples := TDoubleMatrix.Create ( data, 500, 2 );

     MatrixToTxtFile( 'D:\ica.txt', examples );

     ICA := TMatrixICA.Create;

     examples.TransposeInPlace;

     props := TMatrixICA.DefProps;
     props.nonLin := iePow3;

     props.myy := 0.1; // Form1.SpinBox2.Value / 1000;
     props.NumIter := 500;
     props.NumIC := 2;
     props.stabilization := True;
     ICA.ICA ( Examples, props );

     MatrixToTxtFile('D:\ica_w.txt', ICA.W.GetObjRef);
     ICA.Free;
end;

procedure TTestICA.TestSimpleICA;
var data : TDoubleDynArray;
    counter: Integer;
    lastX : integer;
    props : TICAProps;
    examples : TDoubleMatrix;
begin
     SetLength(data, 500);
     lastX := 0;
     for counter := 0 to length(data) - 1 do
     begin
          data[counter] := sin(counter*2*pi/200) + lastX/21 - 0.5;
          
          lastX := lastX + 1;
          if lastX = 21 then
             lastX := 0;
             
     end;

     examples := TDoubleMatrix.Create(data, 100, 5);
     with TMatrixICA.Create do
     try
        examples.TransposeInPlace;
        
        props := TMatrixICA.DefProps;
        props.nonLin := ieSkew;
        props.myy := 0.1;
        props.NumIter := 500;
        props.NumIC := 2;
        props.stabilization := False;

        ICA(Examples, props);
     finally
            Free;
     end;
     examples.Free;
end;

initialization
{$IFNDEF FMX}
  RegisterTest(TTestICA{$IFNDEF FPC}.Suite{$ENDIF});
{$ENDIF};

end.
