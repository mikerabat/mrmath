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

{$IFDEF MACOS}
  {$DEFINE FMX}
{$ENDIF}

uses
  {$IFDEF FPC} testregistry {$ELSE} {$IFDEF FMX}DUnitX.TestFramework {$ELSE}TestFramework {$ENDIF} {$ENDIF} ,
  Classes, SysUtils, Matrix, BaseMatrixTestCase;

type
  {$IFDEF FMX} [TestFixture] {$ENDIF}
  TestDistance = class(TBaseMatrixTestCase)
  published
    procedure TestEuclidDist;
    procedure TestMahalonobis;
    procedure TestAbsDist;
    procedure TestAbsDistRegularized;
    procedure TestPersistence;
    procedure TestPairwiseAbsDist;
    procedure TestPairwiseEuclidDist;
    procedure TestPairwiseMahalDist;
    procedure TestPairwiseNormEuclidDist;
  end;

implementation

uses BinaryReaderWriter, BaseMathPersistence, Dist, MatrixASMStubSwitch, 
  RandomEng;

{ TestDistance }

procedure TestDistance.TestMahalonobis;
const cRes : Array[0..3] of double = (0.6288, 19.3520, 21.1384, 0.9404);
var x : IMatrix;
    y : IMatrix;
    d : IMatrix;
begin
     x := ReadObjFromFile(BaseDataPath + 'mahalonobis.dat') as TDoubleMatrix;
     y := TDoubleMatrix.Create( [1, 1, 1, -1, -1, 1, -1, -1], 2, 4);

     d := TDistance.Mahalonobis(x, y);

     Status( WriteMtx(d.GetObjRef) );

     Check( CheckMtx(d.SubMatrix, cRes), 'Mahalonibis distance calculation failed');
end;

procedure TestDistance.TestAbsDistRegularized;
var x : IMatrix;
    y : IMatrix;
    d : IMatrix;
const cRes : Array[0..3] of double = (1.8054, 1.9670, 2.0330, 2.1946);
begin
     x := ReadObjFromFile(BaseDataPath + 'mahalonobis.dat') as TDoubleMatrix;
     x.Resize(x.Width, x.Height + 1);

     // add outlier ;)
     x.SetRow(x.Height - 1, [ 1000, 1000 ]);
     //WriteMatlabData('D:\xx.txt', x.SubMatrix, x.Width);
     y := TDoubleMatrix.Create( [1, 1, 1, -1, -1, 1, -1, -1], 2, 4);

     with TDistance.Create do
     try
        InitL1DistReg(X, 10, 200, 1e-6);

        status('Iterations needed: ' + intTostr(NumIter));

        d := L1Dist(y);
     finally
            Free;
     end;
     Status( WriteMtx(d.GetObjRef) );

     Check( CheckMtx(d.SubMatrix, cRes, -1, -1, 0.0001), 'L1 distance calculation failed');
end;

procedure TestDistance.TestEuclidDist;
const cRes : Array[0..3] of double = (1.6170, 1.9334, 2.1094, 2.4258);
var x : IMatrix;
    y : IMatrix;
    d : IMatrix;
begin
     x := ReadObjFromFile(BaseDataPath + 'mahalonobis.dat') as TDoubleMatrix;
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
     x := ReadObjFromFile(BaseDataPath + 'mahalonobis.dat') as TDoubleMatrix;
     x.Resize(x.Width, x.Height + 1);

     // add outlier ;)
     x.SetRow(x.Height - 1, [ 1000, 1000 ]);
     //WriteMatlabData('D:\xx.txt', x.SubMatrix, x.Width);
     y := TDoubleMatrix.Create( [1, 1, 1, -1, -1, 1, -1, -1], 2, 4);

     with TDistance.Create do
     try
        InitL1Dist(X, 200, 1e-6, 1e-5);
        status('Iterations needed: ' + intTostr(NumIter));
        d := L1Dist(y);
     finally
            Free;
     end;
     
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
     x := ReadObjFromFile(BaseDataPath + 'mahalonobis.dat') as TDoubleMatrix;
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


procedure TestDistance.TestPairwiseAbsDist;
// vlaues from matlab...
const cX : Array[0..14] of double = (0.5411, 0.8535, -0.4099, 0.9758, 1.3845,
                                    -1.5409, -1.8530, -0.7113, -0.1569, -0.0627,
                                    -0.2031, -0.2073, 0.0614, 0.2778, 0.4489);
      cPDist : Array[0..2] of double = (7.6700, 3.9101, 4.7025);     // cityblock distance...
var x, y : IMatrix;
begin
     x := TDoubleMatrix.Create( cX, 5, 3 );
     y := TDistance.AbsPairDist(x);

     Check( CheckMtx( cPDist, y.SubMatrix, y.Width, 1, 0.001), 'Eucledian pairwise distance failed');
end;

procedure TestDistance.TestPairwiseEuclidDist;
// vlaues from matlab...
const cX : Array[0..14] of double = (0.5411, 0.8535, -0.4099, 0.9758, 1.3845,
                                    -1.5409, -1.8530, -0.7113, -0.1569, -0.0627,
                                    -0.2031, -0.2073, 0.0614, 0.2778, 0.4489);
      cPDist : Array[0..2] of double = (3.8895, 1.8067, 2.3549);
var x, y : IMatrix;
begin
     x := TDoubleMatrix.Create( cX, 5, 3 );
     y := TDistance.EuclidPairDist(x);

     Check( CheckMtx( cPDist, y.SubMatrix, y.Width, 1, 0.001), 'Eucledian pairwise distance failed');
end;

procedure TestDistance.TestPairwiseMahalDist;
// from octave
const  cPDist : Array[0..44] of double = ( 2.7436, 3.7554, 3.2853, 2.3360, 3.6429, 2.0738, 2.3336, 2.3229, 2.6618, 3.5586,
                                          3.4539, 2.6930, 3.6153, 3.6528, 3.7330, 2.4864, 3.9840, 2.7690, 2.3572, 3.6705,
                                          3.3065, 3.4232, 3.4424, 3.2389, 3.7489, 3.8072, 2.7157, 3.0969, 2.0962, 3.5379,
                                          3.3456, 2.8439, 2.9750, 3.2213, 2.6367, 2.4069, 3.9193, 3.3947, 3.9555, 2.5595,
                                          2.3724, 2.4783, 3.6275, 4.0201, 2.6137);
var x, y : IMatrix;
begin
     x := TDoubleMatrix.CreateRand( 5, 10, raSystem, 43);

//     WriteMatlabData('D:\x_maha.txt', x.SubMatrix, x.Width);
     y := TDistance.MahalonobisPairDist(x);

     Check( CheckMtx( cPDist, y.SubMatrix, y.Width, 1, 0.001), 'Eucledian pairwise distance failed');
end;

procedure TestDistance.TestPairwiseNormEuclidDist;
// from octave
const  cPDist : Array[0..44] of double = (  2.5626, 3.7866, 2.8605, 2.8462, 4.1380, 1.9565, 1.6692, 2.2396, 2.3255, 3.8777,
                                            3.0951, 3.2839, 3.4275, 3.0746, 3.1613, 2.3335, 3.8203, 3.6109, 1.9695, 4.0405,
                                            3.2924, 3.0629, 4.1865, 2.7015, 4.1583, 4.0214, 2.3064, 2.8586, 1.5328, 3.1800,
                                            3.9555, 3.0677, 2.5228, 4.1071, 2.4299, 2.8095, 3.8315, 3.9260, 4.6289, 1.8101,
                                            2.2895, 2.4598, 3.0661, 2.6190, 3.1421);
var x, y : IMatrix;
begin
     x := TDoubleMatrix.CreateRand( 5, 10, raSystem, 43);

     y := TDistance.NormEuclidPairDist(x);

     Check( CheckMtx( cPDist, y.SubMatrix, y.Width, 1, 0.001), 'Eucledian pairwise distance failed');
end;

initialization
{$IFNDEF FMX}
  RegisterTest(TestDistance{$IFNDEF FPC}.Suite{$ENDIF});
{$ENDIF}

end.
