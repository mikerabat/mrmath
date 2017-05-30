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

unit TestCorr;

interface

uses
  {$IFDEF FPC} testregistry {$ELSE} TestFramework {$ENDIF} ,
  Classes, SysUtils, Types, Matrix, BaseMatrixTestCase;

type
  TestCorrelation = class(TBaseMatrixTestCase)
  published
    procedure TestCorrelation;
    procedure TestCovariance;
    procedure TestDynamicTimeWarp;
    procedure TestDTWChirp;
    procedure TestFastDTW1;
    procedure TestFastDTW2;
    procedure TestFastDTWChirp;
  end;

implementation

uses Corr, Math, BaseMathPersistence;

{ TestCorrelation }

procedure TestCorrelation.TestCorrelation;
const cw1 : Array[0..4] of double = (1, 2, 3, 4, 5);
      cwref : Array[0..3] of Array[0..4] of double = ( 
                            (2, 1, 2, 1, 2), (-1, -2, -3, -4, -5),
                            (2, 4, 6, 8, 10),
                            (1, 2, 3, 3, 3) );
      cCoreCoeffs : Array[0..3] of double = (0, -1, 1, 0.8839);
var cor : TCorrelation;
    w1 : IMatrix;
    w2 : IMatrix;
    counter : integer;
    cr : double;
begin
     w1 := TDoubleMatrix.Create(cW1, Length(cW1), 1);
     
     cor := TCorrelation.Create;
     try
        for counter := 0 to High(cWref) do
        begin
             w2 := TDoubleMatrix.Create(cwref[counter], Length(cw1), 1);

             cr := cor.Correlate(w1, w2);

             Status(Format('Correlation %d: %.5f', [counter + 1, cr]));   

             Check( SameValue( cCoreCoeffs[counter], cr, 1e-4), 'Correlation failed');
        end;
     finally
            cor.Free;
     end;
end;

procedure TestCorrelation.TestCovariance;
const cA : Array[0..8] of double = (-1, 1, 2, 
                                    -2, 3, 1,
                                     4, 0, 3 );
      // expected result                               
      cCov : Array[0..8] of double = ( 10.3333, -4.1667, 3,
                                       -4.1667, 2.3333, -1.5,
                                        3, -1.5, 1 );
      cCov1 : Array[0..3] of double = (10.3333, 3, 
                                       3, 1);
var A : IMatrix;
    covA : IMatrix;
    covA1 : IMatrix;
    a1, a2 : IMatrix;
begin
     A := TDoubleMatrix.Create( cA, 3, 3);
     with TCorrelation.Create do
     try
        covA := Covariance(A);

        a1 := TDoubleMatrix.Create(1, A.Height);
        a2 := TDoubleMatrix.Create(1, A.Height);

        a1.SetColumn(0, A, 0);
        a2.Setcolumn(0, A, 2);

        covA1 := Covariance(a1, a2);
     finally
            Free;
     end;

     Check(CheckMtx(cCov, covA.SubMatrix, 3, 3, 1e-3), 'Error in covariance calculation');
     Check(CheckMtx(cCov1, covA1.SubMatrix, 2, 2, 1e-3), 'Error in vector covariance calculation');
end;

procedure TestCorrelation.TestDTWChirp;
var x : TDoubleDynArray;
    y : TDoubleDynArray;
    counter : integer;
    dtw : TDynamicTimeWarp;
    xv, yV : IMatrix;
    dist : double;
begin
     SetLength(x, 1000);
     SetLength(y, 400);
     for counter := 0 to Length(x) - 1 do
         x[counter] := cos(2*pi*sqr( (3*(counter + 1)/1000) ));
     for counter := 0 to Length(y) - 1 do
         y[counter] := cos(2*pi*9*(counter + 1)/400);


     xv := TDoubleMatrix.Create(x, Length(x), 1);
     yv := TDoubleMatrix.Create(y, Length(y), 1);

     dtw := TDynamicTimeWarp.Create;
     try
        dtw.DTW(xv, yv, dist);

        //WriteMatlabData('D:\chirp1.txt', dtw.W1.SubMatrix, res.Width);
        //WriteMatlabData('D:\chirp2.txt', dtw.W2.SubMatrix, dtw.W2.Width);

        Status(Format('Distance: %.4f', [dist]));

        Check( SameValue(dist, 0.7869, 1e-3), 'Distance calculation wrong in Chirp test');
     finally
            dtw.Free;
     end;
end;

procedure TestCorrelation.TestDynamicTimeWarp;
const cDWTCorr = 0.9879;
      cCorr = 0.9501;
      cInvCorr = -0.9501;      // referenced with matlab script
      cInvDWTCorr = 0.6899;
      
var master, tpl : IMatrix;
    dtw : TDynamicTimeWarp;
    dist : double;
    cr : double;
    crBase : double;
begin
     // ecg templates
     tpl := ReadObjFromFile('dyntimeWarp1.bin') as TDoubleMatrix;
     master := ReadObjFromFile('dyntimeWarp2.bin') as TDoubleMatrix;
     
     dtw := TDynamicTimeWarp.Create;
     dtw.DTW(tpl, master, dist);

     Check(dist > 0, 'error distance <= 0');

     // check correlation
     cr := dtw.DTWCorr(tpl, master);
     crBase := dtw.Correlate(tpl, master);
     Status( Format('Plain: %.5f, DWTCorr: %.5f', [crBase, cr] ));

     Check( SameValue(crBase, cCorr, 1e-4), 'Error correlation failed');
     Check( SameValue(cr, cDWTCorr, 1e-4), 'Error DTW correlation failed');
     
     master.ScaleInPlace(-1);
     cr := dtw.DTWCorr(tpl, master);

     //WriteMatlabData('D:\dtwm1.txt', dtw.w1.SubMatrix, dtw.w1.width);
     //WriteMatlabData('D:\dtwm2.txt', dtw.w2.SubMatrix, dtw.w2.width);
     //WriteMatlabData('D:\dist.txt', dtw.fAccDist.SubMatrix, dtw.fAccDist.Width);
     //WriteMatlabData('D:\d.txt', dtw.fd.SubMatrix, dtw.fd.Width);
     
     crBase := dtw.Correlate(tpl, master);
     Status( Format('Scale -1 - Plain: %.5f, DWTCorr: %.5f', [crBase, cr] ));
     
     Check( SameValue(crBase, cInvCorr, 1e-4), 'Inverted scale correlation failed');
     Check( SameValue(cr, cInvDWTCorr, 1e-3), 'Inverted scale DTW correlation failed');
     
     dtw.Free;
end;

procedure TestCorrelation.TestFastDTW1;
const cX : Array[0..4] of double = (1, 2, 3, 4, 5);
      cY : Array[0..2] of double = (2, 3, 4);

      cWarpedX : Array[0..4] of double = (1, 2, 3, 4, 5);
      cWarpedY : Array[0..4] of double = (2, 2, 3, 4, 4);
var x, y : IMatrix;
    dtw : TDynamicTimeWarp;
    dist : double;
    warp : IMatrix;
begin
     x := TDoubleMatrix.Create(cX, Length(cX), 1);
     y := TDoubleMatrix.Create(cY, Length(cY), 1);

     dtw := TDynamicTimeWarp.Create(dtwAbsolute);

     warp := dtw.FastDTW(x, y, dist, 1);

     Status( WriteMtx( warp.SubMatrix, warp.Width ) );
     Status( WriteMtx( dtw.W2.SubMatrix, warp.Width ) );
     
     // check result
     Check( dist = 2, 'Distance measure wrong');
     Check( CheckMtx( warp.SubMatrix, cWarpedX ), 'Error X Vector warping is wrong');
     Check( CheckMtx( dtw.W2.SubMatrix, cWarpedY ), 'Error Y Vector warping is wrong');

     dtw.Free;
end;

procedure TestCorrelation.TestFastDTW2;
const cDWTCorr = 0.9879;
      cCorr = 0.9501;
      cInvCorr = -0.9501;      // referenced with matlab script
      cInvDWTCorr = 0.6382;

var master, tpl : IMatrix;
    dtw : TDynamicTimeWarp;
    dist : double;
    cr : double;
    crBase : double;
begin
     // ecg templates
     tpl := ReadObjFromFile('dyntimeWarp1.bin') as TDoubleMatrix;
     master := ReadObjFromFile('dyntimeWarp2.bin') as TDoubleMatrix;
     
     dtw := TDynamicTimeWarp.Create;
     dtw.DTW(tpl, master, dist);

     //WriteMatlabData('D:\dynWarp1.txt', tpl.SubMatrix, tpl.Width);
     //WriteMatlabData('D:\dynWarp2.txt', master.SubMatrix, master.Width);
     

     //WriteMatlabData('D:\dtw1.txt', dtw.w1.SubMatrix, dtw.w1.width);
     //WriteMatlabData('D:\dtw2.txt', dtw.w2.SubMatrix, dtw.w2.width);
     Check(dist > 0, 'error distance <= 0');

     // check correlation
     cr := dtw.DTWCorr(tpl, master);
     crBase := dtw.Correlate(tpl, master);
     Status( Format('Plain: %.5f, DWTCorr: %.5f', [crBase, cr] ));

     Check( SameValue(crBase, cCorr, 1e-4), 'Error correlation failed');
     Check( SameValue(cr, cDWTCorr, 1e-4), 'Error DTW correlation failed');

     dtw.Free;
     dtw := TDynamicTimeWarp.Create(dtwAbsolute);
     dtw.FastDTW(tpl, master, dist, 2);
     cr := dtw.FastDTWCorr(tpl, master, 2);
     
     Status( Format('Dist: %.4f', [dist]) );
     Status( Format('Plain: %.5f, FastDWTCorr: %.5f', [crBase, cr] ));
     
     master.ScaleInPlace(-1);

     crBase := dtw.Correlate(tpl, master);
     cr := dtw.FastDTWCorr(tpl, master, 8);
     Status( Format('Scale -1 - Plain: %.5f, FastDWTCorr: %.5f', [crBase, cr] ));

     Check( SameValue(crBase, cInvCorr, 1e-4), 'Inverted scale correlation failed');
     Check( SameValue(cr, cInvDWTCorr, 1e-3), 'Inverted scale DTW correlation failed');
     
     dtw.Free;
end;

procedure TestCorrelation.TestFastDTWChirp;
var x : TDoubleDynArray;
    y : TDoubleDynArray;
    counter : integer;
    dtw : TDynamicTimeWarp;
    xv, yV : IMatrix;
    dist : double;
begin
     SetLength(x, 1000);
     SetLength(y, 400);
     for counter := 0 to Length(x) - 1 do
         x[counter] := cos(2*pi*sqr( (3*(counter + 1)/1000) ));
     for counter := 0 to Length(y) - 1 do
         y[counter] := cos(2*pi*9*(counter + 1)/400);


     xv := TDoubleMatrix.Create(x, Length(x), 1);
     yv := TDoubleMatrix.Create(y, Length(y), 1);

     dtw := TDynamicTimeWarp.Create(dtwAbsolute);
     try
        dtw.FastDTW(xv, yv, dist, 3);   // minimum radius for this setup to achieve perfect match

        //WriteMatlabData('D:\chirp1.txt', dtw.w1..SubMatrix, res.Width);
        //WriteMatlabData('D:\chirp2.txt', dtw.W2.SubMatrix, dtw.W2.Width);

        Status(Format('Distance: %.4f', [dist]));

        Status('Max Win len: ' + IntToStr(dtw.MaxWinLen));
        Status('Max Path len: ' + IntToStr(dtw.MaxPathLen));

        // valuse from python script:
        Check( SameValue(dist, 21.58345, 1e-3), 'Distance calculation wrong in Chirp test');
     finally
            dtw.Free;
     end;
end;

initialization
  RegisterTest(TestCorrelation{$IFNDEF FPC}.Suite{$ENDIF});

end.
