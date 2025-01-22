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

unit ufrmSplineTest;

interface

uses Windows, SysUtils, Classes, Graphics, Forms,
     StdCtrls, Matrix, RandomEng, Controls, ExtCtrls, ComCtrls;

type
  TfrmSplineTest = class(TForm)
    pnlSettings: TPanel;
    radSplineOrderTest: TRadioButton;
    radBaseTest: TRadioButton;
    radPeriodicBoundTest: TRadioButton;
    radRobustTest: TRadioButton;
    radSplineIntegralTest: TRadioButton;
    pcSettings: TPageControl;
    tbBaseTest: TTabSheet;
    pbSpline: TPaintBox;
    Label1: TLabel;
    Label2: TLabel;
    tbSplineOrders: TTabSheet;
    Label3: TLabel;
    Label4: TLabel;
    tbPeriodicBoundary: TTabSheet;
    Label5: TLabel;
    tbRobustTest: TTabSheet;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    tbSplineInt: TTabSheet;
    lblLegend: TLabel;
    ed1NumBreaks: TEdit;
    ud1NumBreaks: TUpDown;
    ed1Noise: TEdit;
    ud1Noise: TUpDown;
    ed2Noise: TEdit;
    ud2Noise: TUpDown;
    ed2NumBreaks: TEdit;
    ud2Breaks: TUpDown;
    ed3Noise: TEdit;
    ud3Noise: TUpDown;
    Edit1: TEdit;
    ud4Noise: TUpDown;
    ed4NumBreaks: TEdit;
    ud4NumBreaks: TUpDown;
    ed4Beta: TEdit;
    ud4Beta: TUpDown;
    procedure FormCreate(Sender: TObject);
    procedure pbSplinePaint(Sender: TObject);
    procedure spe1NoiseChange(Sender: TObject);
    procedure radBaseTestClick(Sender: TObject);
    procedure spe2NoiseChange(Sender: TObject);
    procedure spe3NoiseChange(Sender: TObject);
    procedure spe4BetaChange(Sender: TObject);
    procedure ed1NumBreaksExit(Sender: TObject);
    procedure ud1NumBreaksClick(Sender: TObject; Button: TUDBtnType);
  private
    { Private-Deklarationen }
    fx, fy : IMatrix;
    fSplineX, fSplineY : IMatrix;
    fSplineOrdersY : Array of IMatrix;
    fBreaks : IMatrix;

    fNoiseLevel : double;
    fRnd : TRandomGenerator;
    procedure mcos(var value : double);
    procedure SinFunc(var value : double);
    procedure sinSimpleFunc(var value : double);
    procedure SinCosFunc(var value: double);

    procedure InitBaseTest;
    procedure InitOrdersTest;
    procedure InitPeriodicTest;
    procedure InitRobustTest;
    procedure InitSplineIntegralTest;
  public
    { Public-Deklarationen }
  end;

var
  frmSplineTest: TfrmSplineTest;

implementation

uses Math, RBSplines;

{$R *.dfm}

{ TfrmSplineTest }

procedure TfrmSplineTest.InitBaseTest;
var spline : TRobustBSpline;
begin
     fSplineOrdersY := nil;
     fNoiseLevel := ud1Noise.Position/100;
     fx := TDoubleMatrix.CreateRand(1, 250);
     fx.ScaleInPlace(2*pi);
     fx.SortInPlace(False);
     fy := fx.ElementwiseFunc( sinFunc );

     fSplineX := TDoubleMatrix.CreateLinSpace(400, 0, 2*pi);
     fBreaks := TDoubleMatrix.CreateLinSpace(Max(5, Min(35, ud1NumBreaks.Position)), 0, 2*pi);

     spline := TRobustBSpline.Create;
     try
        spline.InitSpline(fx, fy, fbreaks);
        fSplineY := spline.EvalSpline(fSplineX);
     finally
            spline.Free;
     end;

     lblLegend.Caption := 'The base function used is y = sin(x) + sin(2*x)' + #13#10 +
                          'with random noise added.';
     pbSpline.Invalidate;
end;

procedure TfrmSplineTest.InitOrdersTest;
var spline : TRobustBSpline;
    orders : integer;
begin
     fSplineY := nil;
     SetLength(fSplineOrdersY, 5);

     fNoiseLevel := ud2Noise.Position/100;
     fx := TDoubleMatrix.CreateRand(1, 250);
     fx.ScaleInPlace(2*pi);
     fx.SortInPlace(False);
     fy := fx.ElementwiseFunc( sinFunc );

     fSplineX := TDoubleMatrix.CreateLinSpace(400, 0, 2*pi);
     fBreaks := TDoubleMatrix.CreateLinSpace(ud2Breaks.Position, 0, 2*pi);

     for orders := 1 to 5 do
     begin
          spline := TRobustBSpline.Create( orders );
          try
             spline.InitSpline(fx, fy, fbreaks);
             fSplineOrdersY[orders - 1] := spline.EvalSpline(fSplineX);
          finally
                 spline.Free;
          end;
     end;

     lblLegend.Caption := 'The example shows different spline orders applied to the' + #13#10 +
                          'input function y = sin(x) + sin(2*x). Colors:' + #13#10 +
                          'Order 1: Maroon, Order 2: Olive' + #13#10 +
                          'Order 3: Green, Order 4: Blue,' + #13#10 +
                          'Order 5: dkGray';

     pbSpline.Invalidate;
end;

procedure TfrmSplineTest.InitPeriodicTest;
var spline : TRobustBSpline;
    orders : integer;
begin
     fSplineY := nil;
     SetLength(fSplineOrdersY, 2);

     fNoiseLevel := ud3Noise.Position/100;
     fx := TDoubleMatrix.CreateRand(1, 200);
     fx.ScaleInPlace(2*pi);
     fx.SortInPlace(False);
     fy := fx.ElementwiseFunc( SinCosFunc );

     fSplineX := TDoubleMatrix.CreateLinSpace(400, 0, 2*pi);
     fBreaks := TDoubleMatrix.CreateLinSpace(8, 0, 2*pi);

     for orders := 0 to 1 do
     begin
          spline := TRobustBSpline.Create( 4, 0, orders = 1 );
          try
             spline.InitSpline(fx, fy, fbreaks);
             fSplineOrdersY[orders] := spline.EvalSpline(fSplineX);
          finally
                 spline.Free;
          end;
     end;

     lblLegend.Caption := 'Periodic extension test for the function' + #13#10 +
                          'y = sin(x) + cos(2*x)';

     pbSpline.Invalidate;
end;

procedure TfrmSplineTest.InitRobustTest;
var spline : TRobustBSpline;
    cnt: Integer;
begin
     fSplineOrdersY := nil;
     fNoiseLevel := ud4Noise.Position/100;
     fx := TDoubleMatrix.CreateRand(1, 250);
     fx.ScaleInPlace(2*pi);
     fx.SortInPlace(False);
     fy := fx.ElementwiseFunc( sinFunc );

     // add outlier that destroy the least square fit
     for cnt := 0 to fx.VecLen div 7 - 1 do
         fy.Vec[ cnt*7 ] := -3;

     fSplineX := TDoubleMatrix.CreateLinSpace(400, 0, 2*pi);
     fBreaks := TDoubleMatrix.CreateLinSpace(Max(5, Min(35, ud4NumBreaks.Position)), 0, 2*pi);

     spline := TRobustBSpline.Create( 4, ud4Beta.Position/100, False);
     try
        spline.InitSpline(fx, fy, fbreaks);
        fSplineY := spline.EvalSpline(fSplineX);
     finally
            spline.Free;
     end;

     lblLegend.Caption := 'Robust spline fitting test. Note the extrem outliers' + #13#10 +
                          'would disturb the least squares fit. Good values' + #13#10 +
                          'for beta are between 0.1 and 0.75. Higher values' + #13#10 +
                          'could result in instabilities,' + #13#10 +
                          'lower to more influence of the outlying input values';

     pbSpline.Invalidate;
end;


procedure TfrmSplineTest.InitSplineIntegralTest;
var spline : TRobustBSpline;
begin
     fSplineY := nil;
     SetLength(fSplineOrdersY, 3);
     fx := TDoubleMatrix.CreateLinSpace(12, -pi, pi);
     fy := fx.ElementwiseFunc( sinSimpleFunc );

     fSplineX := TDoubleMatrix.CreateLinSpace(400, -pi, pi);
     fx.SetSubMatrix(0, 1, 1, fx.Height - 2);
     fBreaks := fx.Clone;
     fx.UseFullMatrix;

     spline := TRobustBSpline.Create( 4, 0, False);
     try
        spline.InitSpline(fx, fy, fbreaks);

        fSplineOrdersY[0] := spline.IntSpline( pi/2, fSplineX );
        fSplineOrdersY[1] := fSplineX.ElementwiseFunc(mcos);
        fSplineOrdersY[2] := spline.EvalSpline(fSplineX);
     finally
            spline.Free;
     end;

     lblLegend.Caption := 'Spline integral example with arbitrary integral offset c=pi/2';


     pbSpline.Invalidate;
end;

procedure TfrmSplineTest.SinCosFunc(var value: double);
begin
     value := sin(value) + cos(2*value) + fRnd.Random*fNoiseLevel;
end;

procedure TfrmSplineTest.SinFunc(var value: double);
begin
     value := sin(value) + sin(2*value) + fRnd.Random*fNoiseLevel;
end;

procedure TfrmSplineTest.sinSimpleFunc(var value: double);
begin
     value := sin(value);
end;

procedure TfrmSplineTest.mcos(var value: double);
begin
     value := -cos(value);
end;

procedure TfrmSplineTest.ed1NumBreaksExit(Sender: TObject);
var value : integer;
    upDown : TUpDown;
function GetAssociatedCtrl : TUpDown;
var cnt : integer;
begin
     Result := nil;
     for cnt := 0 to ControlCount - 1 do
     begin
          if (Controls[cnt] is TUpDown) and ( (Controls[cnt] as TUpDown).Associate = Sender) then
          begin
               Result := Controls[cnt] as TUpDown;
               break;
          end;
     end;
end; 
begin
     upDown := GetAssociatedCtrl;

     // update values within constraints
     if TryStrToInt( (Sender as TEdit).Text, value) then
     begin
          if Assigned(upDown) then
          begin
               value := Max( upDown.Max, Min(upDown.Min, value));
               (Sender as TEdit).Text := IntToStr(value);
               upDown.Position := value;
          end;
     end
     else if Assigned(upDown) 
     then
         (Sender as TEdit).Text := IntToStr( upDown.Position );

     // update view
     radBaseTestClick(nil);
end;

procedure TfrmSplineTest.ud1NumBreaksClick(Sender: TObject; Button: TUDBtnType);
begin
     radBaseTestClick(nil);
end;


procedure TfrmSplineTest.FormCreate(Sender: TObject);
begin
     fRnd := TRandomGenerator.Create(raMersenneTwister);
     fRnd.Init;

     InitBaseTest;
end;

procedure TfrmSplineTest.pbSplinePaint(Sender: TObject);
const cYColors : Array[0..4] of TColor = ( clMaroon, clOlive, clGreen, clBlue, clDkGray );
var minX, maxX, minY, maxY : double;
    xFact, yFact : double;
    diff : double;
    cnt: Integer;
    x, y : integer;
    idx : integer;
    pts : Array of TPoint;
    orders: Integer;
begin
     if not Assigned(fx) then
        exit;

     // ##################################################
     // #### prepare factors
     minX := fx.Vec[0];
     maxX := fX.Vec[fx.Height - 1];
     diff := maxX - minX;
     minX := minX - 0.1*diff;
     maxX := maxX + 0.1*diff;

     minY := fy.Min;
     maxY := fy.Max;

     diff := maxY - minY;
     minY := minY - 0.1*diff;
     maxY := maxY + 0.1*diff;

     xFact := pbSpline.Width/(maxX - minX);
     yFact := pbSpline.Height/(maxY - minY);


     // #################################################
     // ##### paint input
     pbSpline.Canvas.Brush.Color := clBlack;
     pbSpline.Canvas.Pen.Color := clBlack;

     for cnt := 0 to fX.Height - 1 do
     begin
          x := Round( (fX.Vec[cnt] - minX)*xFact);
          y := pbSpline.Height - Round( (fY.Vec[cnt] - minY)*yFact);

          pbSpline.Canvas.Ellipse( x - 2, y - 2, x + 2, y + 2);
     end;

     // #################################################
     // #### Paint output
     pts := nil;
     if Assigned(fSplineY) then
     begin
          SetLength(pts, fSplineX.Height);

          for cnt := 0 to Length(pts) - 1 do
          begin
               pts[cnt].X := Round( (fSplineX.Vec[cnt] - minX)*xFact);
               pts[cnt].Y := pbSpline.Height - Round( (fSplineY.Vec[cnt] - minY)*yFact);
          end;

          pbSpline.Canvas.Pen.Color := clBlue;
          pbSpline.Canvas.Polyline(pts);
     end;

     for orders := 0 to Length(fSplineOrdersY) - 1 do
     begin
          SetLength(pts, fSplineX.Height);

          for cnt := 0 to Length(pts) - 1 do
          begin
               pts[cnt].X := Round( (fSplineX.Vec[cnt] - minX)*xFact);
               pts[cnt].Y := pbSpline.Height - Round( (fSplineOrdersY[orders].Vec[cnt] - minY)*yFact);
          end;

          pbSpline.Canvas.Pen.Color := cYColors[orders];
          pbSpline.Canvas.Polyline(pts);
     end;

     if Assigned(fBreaks) and Assigned(fSplineY) then
     begin
          pbSpline.Canvas.Brush.Color := clRed;

          idx := 1;

          // since the breaks do not contain exact y coordinates we use the splines y coordinate
          for cnt := 0 to fBreaks.VecLen - 1 do
          begin
               pts[cnt].X := Round( (fBreaks.Vec[cnt] - minX)*xFact);

               while (idx < fSplineX.VecLen) and (fSplineX.Vec[idx] < fBreaks.Vec[cnt]) do
                     inc(idx);

               if (idx < fSplineX.Height) then
               begin
                    pts[cnt].Y := pbSpline.Height - Round( ( fSplineY.Vec[idx] - minY)*yFact );

                    with pts[cnt] do
                         pbSpline.Canvas.FillRect( Rect( X - 3, y - 3, x + 3, y + 3));
               end;
          end;
     end;
end;

procedure TfrmSplineTest.spe1NoiseChange(Sender: TObject);
begin
     InitBaseTest;
end;

procedure TfrmSplineTest.radBaseTestClick(Sender: TObject);
begin
     if Sender = nil then
     begin
          if radBaseTest.Checked then
             Sender := radBaseTest;
          if radSplineOrderTest.Checked then
             Sender := radSplineOrderTest;
          if radPeriodicBoundTest.Checked then
             Sender := radPeriodicBoundTest;
          if radRobustTest.Checked then
             Sender := radRobustTest;  
          if radSplineIntegralTest.Checked then
             Sender := radSplineIntegralTest;
     end;

     if Sender = radBaseTest then
     begin
          pcSettings.ActivePage := tbBaseTest;
          InitBaseTest;
     end
     else if Sender = radSplineOrderTest then
     begin
          pcSettings.ActivePage := tbSplineOrders;
          InitOrdersTest;
     end
     else if Sender = radPeriodicBoundTest then
     begin
          pcSettings.ActivePage := tbPeriodicBoundary;
          InitPeriodicTest;
     end
     else if Sender = radRobustTest then
     begin
          pcSettings.ActivePage := tbRobustTest;
          InitRobustTest;
     end
     else if Sender = radSplineIntegralTest then
     begin
          pcSettings.ActivePage := tbSplineInt;
          InitSplineIntegralTest;
     end;
end;

procedure TfrmSplineTest.spe2NoiseChange(Sender: TObject);
begin
     InitOrdersTest;
end;

procedure TfrmSplineTest.spe3NoiseChange(Sender: TObject);
begin
     InitPeriodicTest;
end;

procedure TfrmSplineTest.spe4BetaChange(Sender: TObject);
begin
     InitRobustTest;
end;

end.
