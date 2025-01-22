unit ufrmRegression;

// #################################################
// #### Example that shows how to calculate
// #### the regression parameter for
// #### 1.) linear regression via pseudoinversion
// #### 2.) polynomial regression using the fitting class
// #### 3.) optimization via levenberg marquard

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Matrix, ExtCtrls, NonLinearFit, StdCtrls, ComCtrls, RandomEng;

type
  TDataSetType = (dsLinear, dsQudratic, dsCubic, dsAtan);
type
  TfrmRegression = class(TForm)
    pnlOptions: TPanel;
    pbdata: TPaintBox;
    rdLinearPinv: TRadioButton;
    rdQuadraticRegr: TRadioButton;
    rdCubicRegr: TRadioButton;
    rdArctanOpt: TRadioButton;
    tbNoise: TTrackBar;
    lblCapNoise: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure rdLinearPinvClick(Sender: TObject);
    procedure tbNoiseChange(Sender: TObject);
    procedure pbdataPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { Private-Deklarationen }
    fXMin, fXMax : double;
    fYMin, fYMax : double;

    fDataX : IMatrix;
    fDataY : IMatrix;

    fRegressParams : IMatrix;

    fRegressX : IMatrix;
    fRegressY : IMatrix;

    fDataSetTyp : TDataSetType;

    frnd : TRandomGenerator;

    function virtToCoordX( x : double ) : integer;
    function virtToCoordY( Y : double ) : integer;
    procedure CreateDataSet(typ : TDataSetType);
    procedure atanFunc(var xToY: double);
    procedure cubicFunc(var xToY: double);
    procedure LinFunc(var xToY: double);
    procedure quadFunc(var xToY: double);
    function AddNoise: double;

    procedure AtanOpt(Sender : TObject; a, x : IMatrix; y : IMatrix);

    procedure Regression;

    procedure EvalParams;
  public
    { Public-Deklarationen }
  end;

var
  frmRegression: TfrmRegression;

implementation

{$R *.dfm}

procedure TfrmRegression.FormCreate(Sender: TObject);
begin
     // just some "constant" inits
     fXMin := -3;
     fXMax := 3;

     fYmin := -5;
     fYMax := 5;

     frnd := TRandomGenerator.Create( raMersenneTwister );
     frnd.Init(0);
end;

function TfrmRegression.AddNoise : double;
var sign : double;
begin
     // note: the random Gauss function returns only the positive part of the
     // gaussian curve so we randomly select as sign value.
     if frnd.Random < 0.5
     then
         sign := 1
     else
         sign := -1;
     Result := sign*frnd.RandGauss*tbNoise.Position/tbNoise.Max;
end;

procedure TfrmRegression.LinFunc(var xToY : double);
begin
     // linear function y = k*x + d, d=1, k=0.3
     xToY := 1 + 0.3*xToY;

     // add gaussian noise
     xToY := xToY + AddNoise;
end;

procedure TfrmRegression.quadFunc(var xToY : double);
begin
     // quadratic function y = x^2 + b*x + c
     xToY := sqr(xToY) - 0.5*xToY - 2;
     // add gaussian noise
     xToY := xToY + AddNoise;
end;

procedure TfrmRegression.cubicFunc(var xToY : double);
begin
     // cubic function y = x^3 + a*x^2 + b*x + c
     xToY := 0.8*xToY*sqr(xToY) - 0.5*sqr(xToY) - 2*xToY - 1;
     // add gaussian noise
     xToY := xToY + AddNoise;
end;

procedure TfrmRegression.atanFunc(var xToY : double);
begin
     xToY := 2*ArcTan(xToY - 0.2);
     // add gaussian noise
     xToY := xToY + AddNoise;
end;


procedure TfrmRegression.CreateDataSet(typ: TDataSetType);
begin
     fDataSetTyp := typ;

     // create a random - unit distributed - dataset over the x axis
     // from fxmin to fxmax

     // random from 0 to 1
     fDataX := TDoubleMatrix.CreateRand(1, 100, raMersenneTwister, 0);
     // scale X so we are between fxmin, fxmax
     fDataX.ScaleAndAddInPlace(fXMin, fXMax - fXMin);


     // calculate y values
     fDataY := nil;
     case typ of
       dsLinear: fDataY := fDataX.ElementwiseFunc(LinFunc);
       dsQudratic: fDataY := fDataX.ElementwiseFunc(quadFunc);
       dsCubic: fDataY := fDataX.ElementwiseFunc(cubicFunc);
       dsAtan: fDataY := fDataX.ElementwiseFunc(atanFunc);
     end;

     // evaluate params
     Regression;

     pbdata.Invalidate;
end;

procedure TfrmRegression.rdLinearPinvClick(Sender: TObject);
begin
     if rdLinearPinv.Checked
     then
         CreateDataSet(dsLinear)
     else if rdQuadraticRegr.Checked
     then
         CreateDataSet(dsQudratic)
     else if rdCubicRegr.Checked
     then
         CreateDataSet(dsCubic)
     else
         CreateDataSet(dsAtan);
end;

procedure TfrmRegression.tbNoiseChange(Sender: TObject);
begin
     rdLinearPinvClick(nil);
end;

procedure TfrmRegression.pbdataPaint(Sender: TObject);
var x, y : integer;
    counter : integer;
begin
     // coordinate system
     pbdata.Canvas.Pen.Color := clBlack;

     x := virtToCoordX(fXMin + 0.05*(fxmax - fxmin));
     y := virtToCoordY(0);

     pbdata.Canvas.MoveTo(x, y);
     x := virtToCoordX(fXMax - 0.05*(fxmax - fxmin));
     pbData.Canvas.LineTo(x, y);

     x := virtToCoordX(0);
     y := virtToCoordY( fYMin + 0.05*(fymax - fymin));

     pbdata.Canvas.MoveTo(x, y);
     y := virtToCoordY( fYMax - 0.05*(fymax - fymin));
     pbData.Canvas.LineTo(x, y);

     // paint base data
     if Assigned(fDataX) then
     begin
          pbdata.Canvas.Pen.Color := clBlue;

          for counter := 0 to fDataX.Height - 1 do
          begin
               x := virtToCoordX(fDataX.Vec[counter]);
               y := virtToCoordY(fDataY.Vec[counter]);

               pbdata.Canvas.Ellipse(x - 2, y - 2, x + 2, y + 2);
          end;
     end;

     // paint regression curve
     if Assigned(fRegressX) then
     begin
          pbdata.Canvas.Pen.Color := clRed;

          for counter := 0 to fRegressX.Width - 1 do
          begin
               x := virtToCoordX(fRegressX.Vec[counter]);
               y := virtToCoordY(fRegressY.Vec[counter]);

               if counter = 0
               then
                   pbdata.Canvas.MoveTo(x, y)
               else
                   pbdata.Canvas.LineTo(x, y);
          end;
     end;
end;

function TfrmRegression.virtToCoordX(x: double): integer;
begin
     Result := Round((x - fxMin)/(fxMax - fxMin)*pbdata.Width);
end;

function TfrmRegression.virtToCoordY(Y: double): integer;
begin
     Result := pbdata.Height - Round( (y - fyMin)/(fYMax - fYMin)*pbdata.Height);
end;

procedure TfrmRegression.Regression;
var fit : TNonLinFitOptimizer;
    a0 : IMAtrix;
begin
     a0 := TDoubleMatrix.Create(1, 4, 0);

     fit := TNonLinFitOptimizer.Create;
     try
        fit.OnIterateObj := AtanOpt;

        case fDataSetTyp of
          dsLinear: fRegressParams := fit.PolynomFit(fDataX, fDataY, 1);
          dsQudratic: fRegressParams := fit.PolynomFit(fDataX, fDataY, 2);
          dsCubic: fRegressParams := fit.PolynomFit(fDataX, fDataY, 3);
          dsAtan: begin
                       a0.Vec[0] := 0.8;
                       a0.Vec[1] := 0.5;
                       a0.Vec[2] := -0.3;
                       a0.Vec[3] := 0;
                       fit.MaxIter := 1000;
                       fRegressParams := fit.Optimize( fDataX, fDataY, a0);
                  end;
        end;
     finally
            fit.Free;
     end;

     EvalParams;
end;

procedure TfrmRegression.AtanOpt(Sender: TObject; a, x, y: IMatrix);
var i: Integer;
begin
     for i := 0 to x.Height - 1 do
         y.Vec[i] := a.Vec[0]*ArcTan(a.Vec[1]*x.Vec[i] + a.Vec[2]) + a.Vec[3];
end;

procedure TfrmRegression.EvalParams;
var numElem : Integer;
    aVec : IMatrix;
    aX : IMatrix;
    counter : Integer;
begin
     numElem := pbdata.Width div 2;
     fRegressX := TDoubleMatrix.CreateLinSpace(numElem, fXMin, fXMax);

     if fDataSetTyp = dsAtan then
     begin
          fRegressY := TDoubleMatrix.Create(1, numElem);
          atanOpt(nil, fRegressParams, fRegressX, fRegressY);
          fRegressX.TransposeInPlace;
     end
     else
     begin
          fRegressX.TransposeInPlace;
          if fRegressParams.Width < fRegressParams.Height then
             fRegressParams.TransposeInPlace;

          aX := TDoubleMatrix.Create( numElem, fRegressParams.Width);

          avec := TDoubleMatrix.Create(numElem, 1, 1);
          for counter := ax.Height - 1 downto 0 do
          begin
               ax.SetRow(counter, avec);
               avec.ElementWiseMultInPlace(fRegressX);
          end;

          // calculate polynom
          fRegressY := fRegressParams.Mult(ax);
     end;

     pbdata.Invalidate;
end;

procedure TfrmRegression.FormShow(Sender: TObject);
begin
     rdLinearPinvClick(nil);
end;

procedure TfrmRegression.FormResize(Sender: TObject);
begin
     EvalParams;
end;

end.
