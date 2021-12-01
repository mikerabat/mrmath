unit mrMatrixPlot;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, Types;

type
  TYAxisDrawProc = reference to procedure( x, y : integer; s : string );
  TfrmMtxPlot = class(TForm)
    pbPlot: TPaintBox;
    procedure pbPlotPaint(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure pbPlotDblClick(Sender: TObject);
  private
    { Private-Deklarationen }
    fYData : Array of TDoubleDynArray;
    fXData : TDoubleDynArray;

    fBmp : TBitmap;

    fMinX, fMinY,
    fMaxX, fMaxY : single;
    fDataMinX, fDataMaxX : single;
    fDataMinY, fDataMaxY : single;
    fScaleX, fScaleY : single;
    fOX, fOY : integer;

    fMarginX, fMarginY : integer;

    fUserDefMinX, fUserDefMaxX : single;
    fUserDefMinY, fUserDefMaxY : single;

    function V2PX( X : single ) : integer; inline;
    function V2PY( Y : single ) : integer; inline;

    procedure SetupXMargins;
    procedure SetupParams;
    procedure DrawYAxis(func : TYAxisDrawProc);
    procedure DrawXAxis;
    procedure DrawGraph;
    procedure SetupPlot;
  public
    { Public-Deklarationen }
    procedure InitVec( vec : TDoubleDynArray );
    procedure InitX( x : TDoubleDynArray );
    procedure AddPlot( y : TDoubleDynarray );

  end;

var
  frmMtxPlot: TfrmMtxPlot;

implementation

uses MatrixASMStubSwitch, Math, MatrixConst, MathUtilFunc, mrMatrixPlotOptions;

{$R *.dfm}

{ TfrmMtxPlot }

procedure TfrmMtxPlot.DrawXAxis;
var ox : integer;
    gridTxtW : integer;
    numTicks : integer;
    xStart : double;
    axisPow : double;
    xInc : double;
    tx : TSize;
    aLabel : string;
begin
     // ###########################################
     // #### Draw Frame
     fBmp.Canvas.Pen.Color := clBlack;
     fBmp.Canvas.Pen.Width := 1;
     fBmp.Canvas.Brush.Style := bsClear;
     fBmp.Canvas.Rectangle( fMarginX, fMarginY, fBmp.Width - fMarginX, fBmp.Height - fMarginY);

     // ###########################################
     // #### X Axis
     // start from the middle, find a nice value
     fBmp.Canvas.Font.Name := 'Arial';
     fBmp.Canvas.Font.Size := 8;
     gridTxtW := fBmp.Canvas.TextWidth('000000');

     numTicks := Max(1, fBmp.Width div gridTxtW);

     // start at a "nice" number and evaluate a nice increment (1, 2, 5)
     axisPow := power(10, floor( log10( eps( Abs(fMaxX - fMinX) ) + Abs( fMaxX- fMinX ) ) ) );

     xInc := Abs(fMaxX - fMinX)/numTicks;
     xInc := xInc/axisPow;                // bring it into a range of 0 to 1

     // now adjust to 1 to 10
     while xInc < 1 do
     begin
          xInc := xInc*10;
          axisPow := axisPow/10;
     end;

     // adjust to the number of ticks -> a maximum of 10

     // now round to 1, 2, 5
     if xInc <= 2
     then
         xInc := 2
     else if xInc <= 5
     then
         xInc := 5
     else
         xInc := 10;

     xInc := xInc*axisPow;

     // start at 0 or the next power of ten
     if fMinX < 0
     then
         xStart := -xInc
     else
         xStart := xInc;

     while xStart > fMinX do
           xStart := xStart - xInc;

     ox := V2PX(xStart);
     while ox < fBmp.Width - gridTxtW do
     begin
          if ox > gridTxtW then
          begin
               // check if full integer
               if (Frac( xStart ) = 0) and (abs(xStart) < 100000)
               then
                   aLabel := IntToStr(Round(xStart))
               else
                   aLabel := Format('%.2g', [xStart]);

               tx := fBmp.Canvas.TextExtent(aLabel);

               fBmp.Canvas.MoveTo(ox, fBmp.Height - fMarginY);
               fBmp.Canvas.LineTo(ox, fBmp.Height - fMarginY + 2);

               fBmp.Canvas.TextOut(ox - tx.cx div 2, fBmp.Height - fMarginY div 2 - tx.cy div 2, aLabel);
          end;

          xStart := xStart + xInc;
          ox := V2PX(xStart);
     end;
end;

procedure TfrmMtxPlot.AddPlot(y: TDoubleDynarray);
begin
     SetLength(fYData, Length(fYData) + 1);
     fYData[Length(fYData) - 1] := y;
end;

procedure TfrmMtxPlot.DrawGraph;
var pts : Array of TPoint;
    i : Integer;
    numpts : integer;
    newPt : boolean;
    pltCnt : integer;
const cPltCol : Array[0..8] of TColor = (
                  clRed, clBlue, clGreen, clPurple, clDkGray, clOlive, clNavy, clTeal, clAqua
                 );
begin
     for pltCnt := 0 to Length(fYData) - 1 do
     begin
          SetLength(pts, Length(fYData[pltCnt]));

          if Length(fXData) > 0 then
          begin
               for i := 0 to Length(fYData[pltCnt]) - 1 do
               begin
                    pts[i].X := V2PX(fXData[i]);
                    pts[i].Y := V2PY(fYData[pltCnt][i]);
               end;
          end
          else
          begin
               for i := 0 to Length(fYData[pltCnt]) - 1 do
               begin
                    pts[i].X := V2PX(i);
                    pts[i].Y := V2PY(fYData[pltCnt][i]);
               end;
          end;

          // ###########################################
          // #### Compress in case there is more than one point per pixel
          numPts := 0;
          newPt := True;
          for i := 1 to Length(pts) - 1 do
          begin
               if pts[i - 1].X = pts[i].X then
               begin
                    if newPt then
                       inc(numpts);
                    pts[numpts - 1].Y := Max( pts[i].Y, pts[numpts - 1].Y);
                    pts[numpts].Y := Min( pts[i].Y, pts[numpts].Y);
                    pts[numpts].X := pts[numpts - 1].X;
               end
               else
               begin
                    if not newPt then
                       inc(numpts);

                    newPt := True;
                    pts[numpts].X := pts[i].X;
                    pts[numpts].Y := pts[i].Y;

                    inc(numpts);
               end;
          end;
          SetLength(pts, numpts);

          // ###########################################
          // #### Paint the points
          fBmp.Canvas.Pen.Color := cPltCol[ pltCnt mod Length(cPltCol) ];
          fBmp.Canvas.Pen.Width := 1;

          fBmp.Canvas.Polyline( pts );
     end;
end;

procedure TfrmMtxPlot.DrawYAxis(func : TYAxisDrawProc);
var oy : integer;
    gridTxtH : integer;
    numTicks : integer;
    yStart : double;
    axisPow : double;
    yInc : double;
    aLabel : string;
begin
     // ###########################################
     // #### y Axis
     gridTxtH := 4*fBmp.Canvas.TextHeight('A');
     numTicks := Max(1, fBmp.Height div gridTxtH);

     // start at a "nice" number

     // get power base 10 of the displayed value
     // start at a "nice" number and evaluate a nice increment (1, 2, 5)

     // get power base 10 of the displayed value
     // start at a "nice" number and evaluate a nice increment (1, 2, 5)
     axisPow := power(10, floor( log10( eps( Abs(fMaxY - fMinY) ) + Abs( fMaxY - fMinY ) ) ) );

     yInc := Abs(fMaxY - fMinY)/numTicks;
     yInc := yInc/axisPow;                // bring it into a range of 0 to 1

     // now adjust to 1 to 10
     while yInc < 1 do
     begin
          yInc := yInc*10;
          axisPow := axisPow/10;
     end;

     // adjust to the number of ticks -> a maximum of 10

     // now round to 1, 2, 5
     if yInc <= 2
     then
         yInc := 2
     else if yInc <= 5
     then
         yInc := 5
     else
         yInc := 10;

     yInc := yInc*axisPow;

     // start at 0 or the next power of ten
     if fMinY < 0
     then
         yStart := -yInc
     else
         yStart := yInc;

     while yStart > fMinY do
           yStart := yStart - yInc;

     oy := V2PY(yStart);
     while oy > fMarginY do
     begin
          if oy < fBmp.Height - fMarginY then
          begin
               // check if full integer
               if (Frac( yStart ) = 0) and (abs(yStart) < 100000)
               then
                   aLabel := IntToStr(Round(yStart))
               else
                   aLabel := Format('%.2g', [ round(yStart*100*axisPow)/(100*axisPow)]);

               func(2, oy, aLabel);
          end;

          yStart := yStart + yInc;
          oy := V2PY(yStart);
     end;

end;

procedure TfrmMtxPlot.FormCreate(Sender: TObject);
begin
     fMarginX := 20;
     fMarginY := 20;
     fUserDefMinX := 0;
     fUserDefMaxX := 0;
     fUserDefMinY := 0;
     fUserDefMaxY := 0;
end;

procedure TfrmMtxPlot.FormDestroy(Sender: TObject);
begin
     fBmp.Free;
end;

procedure TfrmMtxPlot.InitVec(vec: TDoubleDynArray);
begin
     SetLength(fYData, 1);
     fYData[0] := vec;
     fXData := nil;
     FreeAndNil(fBmp);

     pbPlot.Invalidate;
end;

procedure TfrmMtxPlot.InitX(x: TDoubleDynArray);
begin
     fYData := nil;
     fXData := x;
     FreeAndNil(fBmp);

     pbPlot.Invalidate;
end;


procedure TfrmMtxPlot.pbPlotDblClick(Sender: TObject);
var fmt : TFormatSettings;
begin
     // ###########################################
     // #### Double clik shows the options to UserDefly set the axis options
     with TfrmMtxPlotOptions.Create(nil) do
     try
        fmt := GetLocalFMTSet;
        fmt.DecimalSeparator := '.';

        edMinXAxis.Text := Format( '%.3e', [fMinX], fmt );
        edMaxXAxis.Text := Format( '%.3e', [fMaxX], fmt );
        edMinYAxis.Text := Format( '%.3e', [fMinY], fmt );
        edMaxYAxis.Text := Format( '%.3e', [fMaxY], fmt );

        lblCurXAxis.Caption := Format( '%.3e - %.3e', [fMinX, fMaxX], fmt );
        lblCurYAxis.Caption := Format( '%.3e - %.3e', [fMinY, fMaxY], fmt );
        lblDataX.Caption := Format( '%.3e - %.3e', [fDataMinX, fDataMaxX], fmt );
        lblDataY.Caption := Format( '%.3e - %.3e', [fDataMinY, fDataMaxY], fmt );

        if ShowModal = mrOk then
        begin
             try
                if not TryStrToFloat( edMinXAxis.Text, fUserDefMinX, fmt ) then
                   raise Exception.Create('Error - the minimum X value has a wrong format.');
                if not TryStrToFloat( edMaxXAxis.Text, fUserDefMaxX, fmt ) then
                   raise Exception.Create('Error - the maximum X value has a wrong format.');
                if not TryStrToFloat( edMinYAxis.Text, fUserDefMinY, fmt ) then
                   raise Exception.Create('Error - the minimum Y value has a wrong format.');
                if not TryStrToFloat( edMaxYAxis.Text, fUserDefMaxY, fmt ) then
                   raise Exception.Create('Error - the maximum Y value has a wrong format.');

                FreeAndNil(fBmp);
                pbPlot.Invalidate;
             except
                   on E : Exception do
                   begin
                        fUserDefMinX := fMinX;
                        fUserDefMaxX := fMaxX;
                        fUserDefMinY := fMinY;
                        fUserDefMaxY := fMaxY;

                        MessageDlg(E.Message, mtError, [mbOk], -1);
                   end;
             end;
        end;
     finally
            Free;
     end;
end;

procedure TfrmMtxPlot.pbPlotPaint(Sender: TObject);
begin
     if not Assigned(fBmp) or (fBmp.Width <> pbPlot.Width) or (fBmp.Height <> pbPlot.Height) then
        SetupPlot;

     pbPlot.Canvas.Draw(0, 0, fBmp);
end;

procedure TfrmMtxPlot.SetupXMargins;
var maxW : integer;
begin
     // standard text props
     fBmp.Canvas.Font.Name := 'Arial';
     fBmp.Canvas.Font.Size := 8;

     // basically estimate the y axis width according
     maxW := 0;
     DrawYAxis(procedure (x, y : integer; aLabel : string)
               var tx : TSize;
               begin
                    tx := fBmp.Canvas.TextExtent(aLabel);
                    maxW := max( maxW, tx.cx);
               end
               );

     fMarginX := Max( 20, 10 + maxW);
end;

procedure TfrmMtxPlot.SetupParams;
var cnt: Integer;
    yMin, yMax : double;
begin
     if (fUserDefMinX - fUserDefMaxX = 0) or (fUserDefMaxY - fUserDefMinY = 0) then
     begin
          if Length(fXData) = 0 then
          begin
               fMinX := 0;
               fMaxX := Length(fYData);
               if fMaxX > 0 then
                  fMaxX := Length(fYData[0]);
          end
          else
          begin
               fMinX := MatrixMin( @fXData[0], Length(fXData), 1, Length(fXData)*sizeof(double));
               fMaxX := MatrixMax( @fXData[0], Length(fXData), 1, Length(fXData)*sizeof(double))
          end;

          fDataMinX := fMinX;
          fDataMaxX := fMaxX;

          if SameValue( fMinX, fMaxX, eps(1)) then
          begin
               fMinX := -1;
               fMaxX := 1;
          end;

          if Length(fYData) > 0 then
          begin
               fMaxY := -MaxDouble;
               fMinY := MaxDouble;
               for cnt := 0 to Length(fYData) - 1 do
               begin
                    if Length(fYData[cnt]) = 0 then
                       continue;

                    yMin := MatrixMin( @fYData[cnt][0], Length(fYData[cnt]), 1, Length(fYData[cnt])*sizeof(double));
                    yMax := MatrixMax( @fYData[cnt][0], Length(fYData[cnt]), 1, Length(fYData[cnt])*sizeof(double));

                    fMaxY := max( ymax, fMaxY );
                    fMinY := min( ymin, fMinY );
               end;

               fDataMinY := fMinY;
               fDataMaxY := fMaxY;

               fMaxY := fMaxY + 0.1*(fMaxY - fMinY);
               fMinY := fMinY - 0.1*(fMaxY - fMinY);

               if SameValue( fMinY, fMaxY, eps(1)) then
               begin
                    fMinY := -1;
                    fMaxY := 1;
               end;
          end;
     end
     else
     begin
          // ###########################################
          // #### override with user defined values
          fMinX := fUserDefMinX;
          fMaxX := fUserDefMaxX;
          fMinY := fUserDefMinY;
          fMaxY := fUserDefMaxY;
     end;

     fScaleY := (fBmp.Height - 2*fMarginY)/(fMaxY - fMinY);
     fOY := Round(fBmp.Height - fMarginY);

     SetupXMargins;
     fOX := fMarginX;
     fScaleX := (fBmp.Width - 2*fMarginX)/(fMaxX - fMinX);
end;

procedure TfrmMtxPlot.SetupPlot;
begin
     // ###########################################
     // #### Prepare bitmap
     if not Assigned(fBmp) then
     begin
          fBmp := TBitmap.Create;
          fBmp.PixelFormat := pf24bit;
     end;
     if (fBmp.Width <> pbPlot.Width) or (fBmp.Height <> pbPlot.Height) then
        fBmp.SetSize( Max(1, pbPlot.Width), Max(1, pbPlot.Height) );

     fBmp.Canvas.Brush.Color := clWhite;
     fBmp.Canvas.Brush.Style := bsSolid;
     fBmp.Canvas.FillRect(Rect(0, 0, fBmp.Width, fBmp.Height));

     // ###########################################
     // #### Prepare the graph bitmap
     if (Length(fYData) > 0) and (pbPlot.Width > 5) and (pbPlot.Height > 0) then
     begin
          SetupParams;
          DrawXAxis;
          DrawYAxis(procedure (x, y : integer; aLabel : string)
                    var tx : TSize;
                    begin
                         tx := fBmp.Canvas.TextExtent(aLabel);
                         fBmp.Canvas.MoveTo(fMarginX, y);
                         fBmp.Canvas.LineTo(fMarginX - 2, y);

                         fBmp.Canvas.TextOut(2, y - tx.cy div 2, aLabel);
                    end);

          DrawGraph;
     end;
end;

function TfrmMtxPlot.V2PX(X: single): integer;
begin
     Result := Round((x - fMinX)*fScaleX) + fOX;
end;

function TfrmMtxPlot.V2PY(Y: single): integer;
begin
     Result := Round((fMinY - Y)*fScaleY) + fOY;
end;

end.
