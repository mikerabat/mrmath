// ###################################################################
// #### This file is part of the mathematics library project, and is
// #### offered under the licence agreement described on
// #### http://www.mrsoft.org/
// ####
// #### Copyright:(c) 2014, Michael R. . All rights reserved.
// ####
// #### Unless required by applicable law or agreed to in writing, software
// #### distributed under the License is distributed on an "AS IS" BASIS,
// #### WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// #### See the License for the specific language governing permissions and
// #### limitations under the License.
// ###################################################################

unit TestCCA;

interface

uses {$IFDEF FPC} testregistry {$ELSE} TestFramework {$ENDIF} ,
     Classes, SysUtils, BaseMatrixTestCase;

type
 TTestCCA = class(TBaseImgTestCase)
 published
   procedure TestCCAImages;
 end;

implementation

uses CCA, Math, Matrix;

{ TTestCCA }

procedure TTestCCA.TestCCAImages;
var x, y : TDoubleMatrix;
    w, h, i : integer;
begin
     // load images
     x := LoadImages(w, h, 'CCATest', '*.bmp');

     // training orientations
     y := TDoubleMatrix.Create(360 div 12, 2);
     for i := 0 to y.Width - 1 do
     begin
          y[i, 0] := sin(i*12*pi/180);
          y[i, 1] := cos(i*12*pi/180);
     end;

     with TMatrixCCA.Create(X, Y) do
     try
        Check(SameValue(R[0, 0], 0.9999, 0.0001), 'Error computation of R failed');
        Check(SameValue(R[0, 1], 0.9999, 0.0001), 'Error computation of R failed');
        
        Check((WyT.Height = WyT.Width) and (WyT.Width = 2), 'Dimension error');
     finally
            Free;
     end;

     x.Free;
     y.Free;
end;

initialization
  RegisterTest(TTestCCA{$IFNDEF FPC}.Suite{$ENDIF});

end.
