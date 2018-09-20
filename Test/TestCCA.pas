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

{$IFDEF MACOS}
  {$DEFINE FMX}
{$ENDIF}

uses {$IFDEF FPC} testregistry {$ELSE} {$IFDEF FMX}DUnitX.TestFramework {$ELSE}TestFramework {$ENDIF} {$ENDIF} ,
     Classes, SysUtils, BaseMatrixTestCase;

type
  {$IFDEF FMX} [TestFixture] {$ENDIF}
  TTestCCA = class(TBaseImgTestCase)
  published
    procedure TestCCAImages;
    procedure TestCCAThreadedImg;
    procedure TestCCAPersistence;
  end;

type
  {$IFDEF FMX} [TestFixture] {$ENDIF}
  TTestPLS = class(TBaseImgTestCase)
  published
    procedure TestPLS;
    procedure TestPLSPersistence;
    procedure TestPLSImages;
  end;
 
implementation

uses CCA, Math, Matrix, PLS, BinaryReaderWriter, BaseMathPersistence, 
     JSONReaderWriter, ThreadedMatrix, MtxTimer, MatrixASMStubSwitch;

{ TTestCCA }

procedure TTestCCA.TestCCAImages;
var x, y : TDoubleMatrix;
    w, h, i : integer;
    yp : IMatrix;
    start, stop : int64;
begin
     InitMathFunctions(itFMA, False);
     // load images
     x := LoadImages(w, h, 'CCATest', '*.bmp');

     // training orientations
     y := TDoubleMatrix.Create(360 div 12, 2);
     for i := 0 to y.Width - 1 do
     begin
          y[i, 0] := sin(i*12*pi/180);
          y[i, 1] := cos(i*12*pi/180);
     end;

     with TMatrixCCA.Create do
     try
        start := MtxGetTime;
        CCA(X, Y);
        stop := MtxGetTime;

        Status(Format('CCA images took: %.3fms', [(stop - start)/mtxfreq*1000]));


        Check(SameValue(R[0, 0], 0.9999, 0.0001), 'Error computation of R failed');
        Check(SameValue(R[0, 1], 0.9999, 0.0001), 'Error computation of R failed');

        Check((WyT.Height = WyT.Width) and (WyT.Width = 2), 'Dimension error');

        // ###########################################
        // #### Test projection
        for i := 0 to x.Width - 1 do
        begin
             X.SetSubMatrix(i, 0, 1, X.Height);
             Y.SetSubMatrix(i, 0, 1, Y.Height);
             yp := Project(X);

             Check( CheckMtx( yp.SubMatrix, y.SubMatrix, -1, -1, 1e-1 ), 'Projections not accurate');
        end;
     finally
            Free;
     end;

     x.Free;
     y.Free;
end;

procedure TTestCCA.TestCCAPersistence;
var x, y : TDoubleMatrix;
    w, h, i : integer;
    ccaObj : TMatrixCCA;
    ccaObjRestore : TMatrixCCA;
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

     ccaObj := TMatrixCCA.Create;
     ccaObj.CCA(X, Y);
     TBinaryReaderWriter.StaticSaveToFile(ccaObj, 'cca.txt');

     ccaObjRestore := ReadObjFromFile( 'cca.txt' ) as TMatrixCCA;   

     Check(CheckMtx(ccaObj.WxT.SubMatrix, ccaObjRestore.WxT.SubMatrix), 'Error Wxt not the same' );
     Check(CheckMtx(ccaObj.WyT.SubMatrix, ccaObjRestore.WyT.SubMatrix), 'Error WyT not the same' );
     Check(CheckMtx(ccaObj.R.SubMatrix, ccaObjRestore.R.SubMatrix), 'Error R not the same' );
     
     ccaObj.Free;
     ccaObjRestore.Free;

     x.Free;
     y.Free;
end;

procedure TTestCCA.TestCCAThreadedImg;
var x, y : TDoubleMatrix;
    w, h, i : integer;
    yp : IMatrix;
    start, stop : int64;
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

     TThreadedMatrix.InitThreadPool;

     with TMatrixCCA.Create do
     try
        MatrixClass := TThreadedMatrix;
        start := MtxGetTime;
        CCA(X, Y);
        stop := MtxGetTime;

        Status(Format('CCA images took: %.3fms', [(stop - start)/mtxfreq*1000]));

        Check(SameValue(R[0, 0], 0.9999, 0.0001), 'Error computation of R failed');
        Check(SameValue(R[0, 1], 0.9999, 0.0001), 'Error computation of R failed');

        Check((WyT.Height = WyT.Width) and (WyT.Width = 2), 'Dimension error');

        // ###########################################
        // #### Test projection
        for i := 0 to x.Width - 1 do
        begin
             X.SetSubMatrix(i, 0, 1, X.Height);
             Y.SetSubMatrix(i, 0, 1, Y.Height);
             yp := Project(X);

             Check( CheckMtx( yp.SubMatrix, y.SubMatrix, -1, -1, 1e-1 ), 'Projections not accurate');
        end;
     finally
            Free;
     end;

     TThreadedMatrix.FinalizeThreadPool;

     x.Free;
     y.Free;
end;


{ TTestPLS }

procedure TTestPLS.TestPLS;
const cY : Array[0..3] of double = ( -15.0000, -5.0000, 5.0000, 15.0000);
      cX : Array[0..4*222-1] of double = (
   0.8787,3.2496,3.0585,-10.7504,-29.2169,-29.1465,-21.4041,-12.5048,-7.8549,-8.5615,-8.1106,-2.0006,-4.0198,-13.8779,-14.7728,
   -10.8595,-0.1782,4.6368,5.2710,5.2442,6.9688,13.2951,19.3717,19.8033,4.9173,-18.9493,-49.6349,-75.6756,-87.7544,
   -94.5343,-96.9106,-107.1272,-135.9822,-175.9138,-235.5288,-326.1335,-456.5801,-629.6938,-800.7404,-908.0436,
   -857.3184,-584.5541,-181.7411,189.5611,450.0352,640.4836,790.6850,843.5772,786.1822,681.3392,591.5018,529.9864,
   474.4394,439.7239,453.9683,505.0474,547.3439,566.8213,600.9205,674.5146,772.3243,846.4764,848.8356,762.1599,609.2647,
   435.8732,265.2402,102.3216,-35.7184,-133.6834,-180.0625,-195.1123,-205.8255,-223.2516,-233.7910,-237.8822,-245.2935,
   -240.1840,-222.2031,-194.3909,-164.4054,-144.4468,-133.0202,-130.3580,-128.2767,-117.1218,-97.2474,-81.3467,-72.7890,
   -77.4906,-86.9513,-95.2935,-107.3362,-112.7749,-113.9682,-108.8541,-99.3622,-94.4568,-87.0341,-86.8198,-95.3636,-95.4382,
   -84.5084,-74.8527,-66.0999,-50.5201,-24.2246,-1.4901,-3.6480,-33.0064,-107.6067,-209.3599,-277.0438,-272.1199,-185.7853,
   -53.2894,73.9573,214.3514,369.9168,491.1675,559.1162,570.1847,528.5639,465.1099,389.1138,302.9754,229.7033,181.8250,164.8422,
   171.6235,201.0493,256.2711,344.8557,439.7639,503.9915,538.7841,548.1347,546.9287,540.4176,500.1709,423.1423,325.1462,237.7017,
   171.7453,113.4270,66.7054,36.7037,27.4670,34.3410,54.2854,87.5197,124.9425,153.2496,164.4020,157.7667,123.1675,60.8936,
   -8.3057,-65.6770,-99.4677,-105.6030,-97.5657,-74.1875,-27.8309,16.8906,40.6262,49.9535,44.4983,25.7726,4.1203,-30.4798,
   -61.2708,-72.2582,-77.3594,-74.6212,-59.9122,-51.5944,-49.0636,-40.2718,-36.6413,-36.8798,-38.8512,-44.0847,-43.8071,
   -35.8824,-36.3427,-55.7999,-87.2991,-121.0695,-139.5056,-156.7344,-185.5248,-234.6119,-325.8358,-455.7299,-608.1464,
   -757.1013,-864.3546,-865.2365,-742.6177,-574.4669,-452.7248,-443.0067,-559.4837,-755.5072,-983.8073,-1182.2513,-1284.0321,
   -1245.3322,-1086.8037,-848.4127,-542.9790,-195.6335,126.5154,366.1910,536.1990,677.5923,826.0349,996.4588,1122.4427,1060.4720,
   963.1873,
   
   -68.8111,-95.2598,-136.5010,-139.4904,-118.1711,-114.1363,-116.1451,-115.6803,-106.5731,-102.1182,-112.1749,-120.7536,-104.7324,
   -90.8807,-104.8653,-119.0120,-132.2664,-117.4018,-103.3556,-86.8404,-69.6253,-75.3685,-80.4986,-82.2515,-85.0762,-77.2852,
   -50.1560,-32.6169,-44.6747,-77.2981,-98.8557,-94.8424,-68.5023,-34.2672,5.5711,58.3419,114.9800,189.3044,264.1804,298.2635,
   273.9273,181.2731,60.3840,-27.8241,-70.6049,-115.8366,-183.5180,-229.5146,-251.3522,-260.4001,-222.7653,-142.6130,-60.1220,
   -43.4792,-135.0488,-293.0828,-441.7418,-536.4031,-616.3984,-730.7317,-832.2957,-854.0675,-762.7397,-584.4415,-391.1624,
   -247.5095,-185.9844,-166.7019,-154.4651,-117.9578,-39.2140,97.3979,278.3758,457.3596,564.0578,559.2481,525.0840,495.6115,
   445.8930,374.4790,294.5371,238.4723,209.0813,205.0454,192.2415,146.9746,89.2714,24.6281,-18.9407,-31.1741,-29.6929,-28.4274,
   -30.6838,-32.0543,-30.8420,-29.8003,-3.7626,20.4163,10.8354,10.3815,14.8507,16.0293,5.9832,1.1497,10.1202,-4.9361,
   -32.5651,-32.3473,0.2762,52.1568,105.1860,120.9689,73.7776,-42.8866,-201.8525,-379.3947,-508.4689,-575.9547,-604.2378,-578.6055,
   -512.6034,-418.3359,-322.6002,-297.6612,-312.8218,-274.0256,-144.3038,47.6706,226.9480,352.8377,452.4384,555.2455,645.9749,
   728.3785,761.5247,719.7726,637.0410,519.5839,387.9188,272.4452,163.1336,66.8136,9.8965,-7.5900,14.8565,52.1356,83.4012,
   128.9682,150.4433,128.1206,82.2245,45.4932,33.2677,26.5902,10.3698,-0.8406,-6.5314,-40.1413,-68.2044,-71.9542,-57.2922,
   -12.3223,29.7552,48.2713,53.3867,65.6197,69.7627,56.3768,48.6749,47.1180,42.6074,32.2504,15.9155,5.5343,-7.9158,-30.5200,
   -40.0715,-62.5966,-83.0788,-82.3695,-91.4318,-109.9896,-130.5595,-153.0756,-163.0781,-168.7385,-174.5854,-173.3801,-169.6163,
   -189.5375,-194.8641,-207.3087,-234.5750,-244.4529,-218.3444,-156.9884,-102.7745,-90.9758,-135.6876,-223.6209,-297.3233,
   -329.0682,-313.1938,-226.1729,-123.1862,2.2032,116.1567,183.8431,210.9431,197.2490,163.6650,125.4120,66.8472,12.0909,
   -19.4434,-83.2514,-190.9793,-357.4994,-598.6878,-820.6059,-854.0553,-805.8702,
   
   -80.0359,-89.3837,-85.9890,-75.9002,-79.2120,-82.9922,-82.3898,-83.1816,-79.7638,-66.7645,-54.8675,-36.2786,-21.2453,-1.0646,
   19.5081,25.4774,15.2082,-6.2775,-18.8449,-19.7197,-33.3743,-43.6345,-45.6701,-43.5652,-28.4438,-19.8785,-9.5327,0.5386,
   14.1089,35.7220,42.7606,39.0073,47.4204,62.1848,92.1774,125.4808,153.8128,191.8074,236.3231,285.2552,289.2309,217.3097,
   85.1989,-58.6479,-175.5737,-279.7854,-376.2182,-452.7976,-489.9230,-492.2585,-506.4885,-558.7371,-633.1976,-674.6478,-646.0411,
   -545.7589,-387.1331,-219.4009,-67.8078,44.5016,82.4120,60.4319,19.4469,2.3051,37.0828,126.7750,285.6756,485.9616,
   652.9945,738.9815,728.8684,627.7107,484.0141,342.1716,206.2739,114.7185,64.7401,33.2404,17.8694,13.9953,15.8235,14.0070,
   17.7247,9.8851,3.8474,-3.2029,-11.5840,-5.2597,0.9089,0.0204,-7.7106,-19.6568,-32.2511,-33.5916,-25.7580,-15.2489,-20.4679,
   -21.9219,1.5065,20.3914,49.4015,76.7420,90.4036,98.3417,94.5082,114.2090,142.4590,144.1745,148.8771,162.3369,200.8264,264.8508,
   319.3319,360.1660,363.5179,342.4589,310.6564,213.1569,22.1099,-216.9656,-439.1527,-600.0198,-656.7102,-578.1008,-413.7802,
   -240.5099,-114.6409,-48.5340,-8.3121,9.0483,12.9966,26.6991,62.3098,110.6923,174.8781,244.1571,298.8245,327.6252,335.6291,
   346.8990,387.6427,431.6154,428.0720,393.1218,335.6678,265.4084,206.6715,136.5450,80.4775,63.3895,51.4162,19.1615,-23.1373,
   -37.1727,-25.6516,-6.4433,22.9828,69.9607,118.8573,125.8244,88.9392,31.5115,-15.4216,-59.9089,-104.6157,-155.2470,-202.6028,
   -207.5306,-190.2216,-159.3407,-102.5754,-40.9451,1.7633,26.4263,37.3169,34.8807,29.4629,30.8961,27.2384,11.6916,-3.4443,
   -3.4309,9.3612,23.5135,19.6271,13.2900,21.4751,28.4199,38.4491,46.7538,39.6474,31.6033,34.0045,62.5454,97.3036,123.2206,
   157.1227,190.0196,180.0597,124.4566,48.7305,-11.1764,-21.6682,-9.6117,57.6453,166.0499,275.0275,368.8103,394.0565,349.9598,
   257.5330,100.1185,-110.0510,-348.9264,-588.3458,-777.6583,-917.9323,-1022.6646,-1080.3536,-1074.0282,-934.4701,-816.4687,
   
   148.3371,181.8128,220.7018,224.6121,227.6072,227.5047,218.6069,211.6938,194.3162,179.0661,174.1341,158.8475,128.9668,106.7297,
   100.3456,105.9897,117.5349,117.7845,116.5161,101.5136,94.2614,104.9756,106.8285,106.3805,110.5995,117.5809,110.8841,
   106.3161,116.9511,136.0440,154.7503,163.3567,157.9706,148.6168,139.4247,144.2294,189.3476,249.9443,299.6453,324.6852,
   292.3885,186.6321,37.1583,-101.6988,-201.9203,-245.0767,-231.7219,-161.1724,-45.0140,71.4037,138.8535,169.8918,220.4704,
   279.6487,326.6089,333.0137,280.9896,188.7944,83.6157,12.1088,-21.7628,-52.5688,-105.7447,-179.5057,-253.4115,-313.1862,
   -363.9433,-421.7790,-464.1659,-487.4199,-507.6381,-530.0347,-556.1781,-575.1359,-536.1413,-438.0048,-346.3089,-288.3788,
   -241.6690,-195.5611,-145.8320,-107.6027,-94.5571,-86.3151,-66.5197,-27.0796,20.0748,63.0202,91.4655,109.4289,125.9196,143.7759,
   168.6640,177.0608,171.5426,152.4704,121.8860,95.3943,73.2615,56.9058,30.9605,3.5230,-10.5676,-22.7276,-38.3614,-59.2796,
   -86.1850,-110.5710,-144.8041,-180.3476,-196.6335,-174.6645,-114.7504,-46.7853,24.4003,90.9893,122.4589,148.5065,213.7181,
   302.7901,391.3373,448.2312,451.7180,409.6279,335.9695,210.0528,29.8421,-179.6739,-381.4970,-532.7434,-666.1001,-839.5853,
   -1054.6623,-1277.2184,-1439.5660,-1503.9216,-1482.0480,-1396.0129,-1264.7565,-1117.6997,-973.4372,-821.7787,-677.2101,
   -558.1755,-464.5876,-385.8832,-327.9689,-292.2428,-266.1732,-247.3027,-222.3285,-189.9839,-165.1174,-154.4206,-141.1871,
   -117.4323,-77.0524,-22.6377,16.8106,45.3859,72.5166,77.2439,59.6746,41.0855,34.8516,50.6921,82.7126,107.9909,113.9046,
   109.2224,90.4907,69.0212,56.1679,45.3298,45.2336,54.3637,62.1215,80.0438,95.8031,105.6157,131.5527,152.4965,166.4464,
   175.1141,180.5838,192.5555,210.3516,232.1135,252.5541,281.9070,310.9813,362.4536,436.2901,508.0081,578.1797,641.5916,704.6096,
   765.9468,819.7763,841.5386,823.9275,792.9407,776.6824,796.0872,819.1655,813.7665,791.0180,730.4032,640.5689,540.8099,
   427.1898,318.8875,238.4325,209.7273,243.5491,325.0387,429.8994,552.2612,683.7181,772.3629,728.8856,659.2806
   );

var lpls : TMatrixPLS;
    x, y : IMatrix;
    FNComp : integer;

begin
     fnComp := 3;
     x := TDoubleMatrix.create;
     x.assign(cX, 222, 4);
     y := TdoubleMatrix.Create;
     y.Assign(cY, 1, 4);

     LPLS := TMatrixPLS.Create;

     LPLS.Plsr(X.GetObjRef,Y.GetObjRef, Y.Height, FNComp);
 
     LPLS.Free
end;

procedure TTestPLS.TestPLSImages;
var x, y : TDoubleMatrix;
    w, h, i : integer;
    yp : IMatrix;
begin
     // load images
     x := LoadImages(w, h, 'CCATest', '*.bmp');
     x.TransposeInPlace;
     
     // training orientations
     y := TDoubleMatrix.Create(1, 360 div 12);
     for i := 0 to y.Height - 1 do
         y.Vec[i] := sin(i*12*pi/180);

     with TMatrixPLS.Create do
     try
        Plsr(X, Y, 1, 20);
        
        // ###########################################
        // #### Test projection
        for i := 0 to x.height - 1 do
        begin
             X.SetSubMatrix(0, i, x.Width, 1);
             Y.SetSubMatrix(0, i, y.Width, 1);
             yp := Project(X);

             Check( CheckMtx( yp.SubMatrix, y.SubMatrix, -1, -1, 1e-1 ), 'Projections not accurate');
        end;
     finally
            Free;
     end;

     x.Free;
     y.Free;
end;

procedure TTestPLS.TestPLSPersistence;
var x, y : TDoubleMatrix;
    w, h, i : integer;
    plsObj : TMatrixPLS;
    plsObjRest : TMatrixPLS;
begin
     // load images
     x := LoadImages(w, h, 'CCATest', '*.bmp');
     x.TransposeInPlace;
     
     // training orientations
     y := TDoubleMatrix.Create(1, 360 div 12);
     for i := 0 to y.Height - 1 do
         y.Vec[i] := sin(i*12*pi/180);

     plsObj := TMatrixPLS.Create;
     plsObj.Plsr(X, Y, 1, 20);

     TJsonReaderWriter.StaticSaveToFile(plsObj, 'PLSObj.json');

     plsObjRest := ReadObjFromFile( 'PLSObj.json' ) as TMatrixPLS;

     Check(CheckMtx( plsObj.SSQDif.SubMatrix, plsObjRest.SSQDif.SubMatrix ), 'SSQDif not equal');
     Check(CheckMtx( plsObj.W.SubMatrix, plsObjRest.W.SubMatrix ), 'W not equal');
     Check(CheckMtx( plsObj.Beta.SubMatrix, plsObjRest.Beta.SubMatrix ), 'Beta not equal');
     Check(CheckMtx( plsObj.YRes.SubMatrix, plsObjRest.YRes.SubMatrix ), 'YRes not equal');
     Check(CheckMtx( plsObj.Theta.SubMatrix, plsObjRest.Theta.SubMatrix ), 'Theta not equal');
     
     plsObj.Free;
     plsObjRest.Free;

     x.Free;
     y.Free;
end;

initialization
{$IFNDEF FMX}
  RegisterTest(TTestCCA{$IFNDEF FPC}.Suite{$ENDIF});
  RegisterTest(TTestPLS{$IFNDEF FPC}.Suite{$ENDIF});
{$ENDIF}

end.
