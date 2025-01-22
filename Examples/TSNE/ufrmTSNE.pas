unit ufrmTSNE;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Matrix, ExtCtrls, ComCtrls, StdCtrls;

const WM_Update = WM_APP + 123;

type
  TfrmTSNE = class(TForm)
    pnlOptions: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    btnCalc: TButton;
    btnCreateDataSet: TButton;
    tbPerplexity: TTrackBar;
    tbIterations: TTrackBar;
    pbScatter: TPaintBox;
    pbProgress: TProgressBar;
    lblProgress: TLabel;
    lblPerplexity: TLabel;
    lblIterations: TLabel;
    radGauss: TRadioButton;
    radCircle: TRadioButton;
    Label3: TLabel;
    edTheta: TEdit;
    radSimple: TRadioButton;
    procedure btnCreateDataSetClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure pbScatterPaint(Sender: TObject);
    procedure btnCalcClick(Sender: TObject);
    procedure chkMultithreadedClick(Sender: TObject);
    procedure tbPerplexityChange(Sender: TObject);
    procedure tbIterationsChange(Sender: TObject);
  private
    fMtxCls : TDoubleMatrixClass;
    fx : IMatrix;
    fydata : IMatrix;
    fpts : Array of TPoint;
    fMaxIter : integer;
    fStart, fEnd, fFreq : Int64;

    fCurCost : double;
    fCurIter : Integer;

    procedure WMUpdate(var msg : TMessage); message WM_Update;

    procedure FillPts;
    procedure SetupThreaded;
    procedure OnTSNEProgress(Sender : TObject; iter : integer; cost : double; yMap : IMatrix; var doCancel : boolean);
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  frmTSNE: TfrmTSNE;

implementation

uses RandomEng, PCA, ThreadedMatrix, tSNE, MatrixASMStubSwitch, Types, Math;

{$R *.dfm}

type
  TTSNECalcThread = class(TThread)
  private
    fIter : integer;
    fPerpl : integer;
    fProgress : TTSNEProgress;
    fX : IMatrix;
    fTheta : double;
    fY : IMatrix;
  protected
    procedure Execute; override;
  public
    property Y : IMatrix read fY;

    constructor Create( perpl : integer; iter : integer; X : IMatrix; theta : double; progress : TTSNEProgress );
  end;


procedure TfrmTSNE.btnCreateDataSetClick(Sender: TObject);
procedure CreateGaussSet;
var i, j : Integer;
    rnd : TRandomGenerator;
begin
     rnd := TRandomGenerator.Create( raMersenneTwister );
     rnd.Init( 101 );

     fx := fMtxCls.Create( 2, 100 );

     for i := 0 to fx.Height div 2 - 1 do
     begin
          for j := 0 to fx.Width - 1 do
              fx[j, i] := 0.5 + 0.2*rnd.RandGauss;
     end;

     for i := fx.Height div 2 to fx.Height - 1 do
     begin
          for j := 0 to fx.Width - 1 do
              fx[j, i] := -0.5 + 0.2*rnd.RandGauss;
     end;

     rnd.Free;
end;
procedure CreateCircleSet;
var i : Integer;
    radius : double;
    alpha : double;
    rnd : TRandomGenerator;
begin
     rnd := TRandomGenerator.Create( raMersenneTwister );
     rnd.Init( 0 );

     fx := fMtxCls.Create( 2, 100 );

     for i := 0 to fx.Height div 2 - 1 do
     begin
          alpha := 2*pi*rnd.Random;
          radius := 2 + 0.2*rnd.Random;


          fx[0, i] := radius*sin( alpha );
          fx[1, i] := radius*cos( alpha );
     end;

     for i := fx.Height div 2 to fx.Height - 1 do
     begin
          alpha := 2*pi*rnd.Random;
          radius := 0.2 + 0.2*rnd.Random;

          fx[0, i] := radius*sin( alpha );
          fx[1, i] := radius*cos( alpha );
     end;

     rnd.Free;
end;

procedure CreateSimpleSet;
var i : Integer;
begin
     fx := fMtxCls.Create( 2, 50 );

     for i := 0 to fx.Height div 2 - 1 do
     begin
          fx[0, i] := 0.1;
          fx[1, i] := 0.1 + 0.001*i;
     end;

     for i := fx.Height div 2 to fx.Height - 1 do
     begin
          fx[0, i] := 0.8;
          fx[1, i] := 0.1 + 0.001*i;
     end;
end;

begin
     if radGauss.Checked
     then
         CreateGaussSet
     else if radSimple.Checked
     then
         CreateSimpleSet
     else
         CreateCircleSet;

     fydata := fX;
     FillPts;
     pbScatter.Invalidate;
end;

procedure TfrmTSNE.FillPts;
var minXVal, maxXVal : double;
    minYVal, maxYVal : double;
    y : Integer;
begin
     fydata.UseFullMatrix;
     fydata.SetSubMatrix(0, 0, 1, fydata.Height);
     minXVal := fydata.Min - 0.1;
     maxXval := fydata.Max + 0.1;

     fydata.SetSubMatrix(1, 0, 1, fydata.Height);
     minYVal := fydata.Min - 0.1;
     maxYval := fydata.Max + 0.1;

     fydata.UseFullMatrix;

     SetLength( fpts, fydata.Height );

     for y := 0 to fydata.Height - 1 do
     begin
          fpts[y].X := Round( (fydata[0, y] - minXVal)*pbScatter.Width/(maxXval - minXVal));
          fpts[y].Y := Round( pbScatter.Height - (fydata[1, y] - minYVal)*pbScatter.Height/(maxYVal - minYVal));
     end;
end;

procedure TfrmTSNE.SetupThreaded;
begin
     // seems not to make a difference
     if False // chkMultithreaded.Checked
     then
         fMtxCls := TThreadedMatrix
     else
         fMtxCls := TDoubleMatrix;

     TMatrixPCA.DefMatrixClass := fMtxCls;
     TtSNE.DefMatrixClass := fMtxCls;
     TtSNE.Threaded := True;
end;

procedure TfrmTSNE.FormCreate(Sender: TObject);
begin
     InitMathFunctions(itFMA, False);
     TThreadedMatrix.InitThreadPool;
     SetupThreaded;

     // setup some labels
     tbIterationsChange(nil);
     tbPerplexityChange(nil);
end;

procedure TfrmTSNE.FormDestroy(Sender: TObject);
begin
     TThreadedMatrix.FinalizeThreadPool;
end;

procedure TfrmTSNE.pbScatterPaint(Sender: TObject);
var i: Integer;
begin
     if Length(fpts) = 0 then
        exit;

     pbScatter.Canvas.Pen.Color := clDkGray;
     pbScatter.Canvas.Brush.Color := clRed;
     for i := 0 to Length(fPts) - 1 do
     begin
          if i = Length(fPts) div 2 then
             pbScatter.Canvas.Brush.Color := clBlue;

          pbScatter.Canvas.Ellipse( fPts[i].X - 2, fPts[i].Y - 2, fPts[i].X + 2, fPts[i].Y + 2);
     end;
end;

{ TTSNECalcThread }

constructor TTSNECalcThread.Create(perpl, iter: integer; X : IMatrix; theta : double;
  progress: TTSNEProgress);
begin
     fPerpl := perpl;
     fIter := iter;
     fProgress := progress;
     FreeOnTerminate := True;
     fX := X.Clone;
     fTheta := theta;
     fY := nil;

     inherited Create(False);
end;

procedure TTSNECalcThread.Execute;
var tsne : TtSNE;
    doCancel : boolean;
begin
     tsne := TtSNE.Create(2, fPerpl, fIter);
     try
        tsne.OnProgress := fProgress;
        //if ii mod 2 = 0
//        then
//            InitMathFunctions(itFPU, False)
//        else
//            InitMathFunctions(itsse, False);
//
//        inc(ii);
        fY := tsne.SymTSNE(fx.GetObjRef, fTheta, 2);

        // final progress of 100%
        fProgress(Self, fIter, tsne.Cost, fY, doCancel);
     finally
            tsne.Free;
     end;
end;

procedure TfrmTSNE.WMUpdate(var msg: TMessage);
begin
     QueryPerformanceCounter(fEnd);
     lblProgress.Caption := Format('iter: %d, cost: %.4f ' + #13#10 + 'elapsed: %.3fms', [fCuriter, fCurcost, (fEnd - fStart)*1000/fFreq]);
     FillPts;
     pbScatter.Repaint;

     if msg.WParam <> 0 then
        btnCalc.Enabled := True;
end;

procedure TfrmTSNE.OnTSNEProgress(Sender: TObject; iter: integer; cost: double;
  yMap: IMatrix; var doCancel : boolean);
begin
     fydata := yMap.Clone;
     fCurCost := cost;
     fCurIter := iter;

     PostMessage( self.Handle, WM_Update, integer(doCancel or (iter = fMaxIter)), 0);
end;

procedure TfrmTSNE.btnCalcClick(Sender: TObject);
var theta : double;
    fmt : TFormatSettings;
begin
     theta := 0;
     GetLocaleFormatSettings(0, fmt);
     fmt.DecimalSeparator := '.';

     if not TryStrToFloat( edTheta.Text, theta, fmt ) then
     begin
          MessageDlg('Theta is not a floating point value.', mtError, [mbOk], -1);
          exit;
     end;
     if (theta < 0) or (theta > 1) then
     begin
          MessageDlg('Theta must have a value from 0 to 1', mtError, [mbOk], -1);
          exit;
     end;

     QueryPerformanceFrequency(fFreq);
     btnCalc.Enabled := False;
     if not Assigned(fx) then
        btnCreateDataSet.Click;

     fMaxIter := tbIterations.Position;
     TTSNECalcThread.Create( tbPerplexity.Position, fMaxIter, fx, theta, OnTSNEProgress );
     QueryPerformanceCounter(fStart);
end;

procedure TfrmTSNE.chkMultithreadedClick(Sender: TObject);
begin
     SetupThreaded;
end;

procedure TfrmTSNE.tbPerplexityChange(Sender: TObject);
begin
     lblPerplexity.Caption := IntToStr(tbPerplexity.Position);
end;

procedure TfrmTSNE.tbIterationsChange(Sender: TObject);
begin
     lblIterations.Caption := IntToStr(tbIterations.Position);
end;

end.
