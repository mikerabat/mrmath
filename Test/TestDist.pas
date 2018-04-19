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

unit TestDist;

interface

uses
  {$IFDEF FPC} testregistry {$ELSE} TestFramework {$ENDIF} ,
  Classes, SysUtils, Types, Matrix, BaseMatrixTestCase;

type
  TestDistance = class(TBaseMatrixTestCase)
  published
    procedure TestEuclidDist;
    procedure TestMahalonobis;
    procedure TestAbsDist;
    procedure TestPersistence;
  end;

implementation

uses BinaryReaderWriter, BaseMathPersistence, Dist;

{ TestDistance }

procedure TestDistance.TestMahalonobis;
const cRes : Array[0..3] of double = (0.6288, 19.3520, 21.1384, 0.9404);
var x : IMatrix;
    y : IMatrix;
    d : IMatrix;
begin
     x := ReadObjFromFile('mahalonobis.dat') as TDoubleMatrix;
     y := TDoubleMatrix.Create( [1, 1, 1, -1, -1, 1, -1, -1], 2, 4);

     d := TDistance.Mahalonobis(x, y);

     Status( WriteMtx(d.GetObjRef) );

     Check( CheckMtx(d.SubMatrix, cRes), 'Mahalonibis distance calculation failed');
end;

procedure TestDistance.TestEuclidDist;
const cRes : Array[0..3] of double = (1.6170, 1.9334, 2.1094, 2.4258);
var x : IMatrix;
    y : IMatrix;
    d : IMatrix;
begin
     x := ReadObjFromFile('mahalonobis.dat') as TDoubleMatrix;
     y := TDoubleMatrix.Create( [1, 1, 1, -1, -1, 1, -1, -1], 2, 4);

     d := TDistance.Euclid(x, y);

     Status( WriteMtx(d.GetObjRef) );

     Check( CheckMtx(d.SubMatrix, cRes), 'Mahalonibis distance calculation failed');
end;

procedure TestDistance.TestAbsDist;
var x : IMatrix;
    y : IMatrix;
    d : IMatrix;
const cRes : Array[0..3] of double = (1.8054, 1.9670, 2.0330, 2.1946);
begin
     x := ReadObjFromFile('mahalonobis.dat') as TDoubleMatrix;
     x.Resize(x.Width, x.Height + 1);

     // add outlier ;)
     x.SetRow(x.Height - 1, [ 1000, 1000 ]);
     //WriteMatlabData('D:\xx.txt', x.SubMatrix, x.Width);
     y := TDoubleMatrix.Create( [1, 1, 1, -1, -1, 1, -1, -1], 2, 4);

     d := TDistance.L1(x, y);

     Status( WriteMtx(d.GetObjRef) );

     Check( CheckMtx(d.SubMatrix, cRes, -1, -1, 0.0001), 'L1 distance calculation failed');
end;


procedure TestDistance.TestPersistence;
var x : IMatrix;
    y : IMatrix;
    d : IMatrix;
    dist : TDistance;
const cRes : Array[0..3] of double = (0.6288, 19.3520, 21.1384, 0.9404);
begin
     x := ReadObjFromFile('mahalonobis.dat') as TDoubleMatrix;
     y := TDoubleMatrix.Create( [1, 1, 1, -1, -1, 1, -1, -1], 2, 4);

     dist := TDistance.Create;
     dist.InitMahal(x, False);

     dist.SaveToFile('mahalObj.dat', TBinaryReaderWriter);
     dist.Free;

     dist := ReadObjFromFile('mahalObj.dat') as TDistance;

     d := dist.MahalDist(y);
     dist.Free;
     Status( WriteMtx(d.GetObjRef) );

     Check( CheckMtx(d.SubMatrix, cRes), 'Mahalonibis distance calculation failed');
end;


initialization
  RegisterTest(TestDistance{$IFNDEF FPC}.Suite{$ENDIF});

end.
