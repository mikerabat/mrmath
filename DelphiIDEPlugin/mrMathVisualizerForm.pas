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

unit mrMathVisualizerForm;

interface

uses Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
     Dialogs, BasicExternalViewerVisualizer, ExtCtrls, StdCtrls, Matrix, Grids,
     ToolsAPI, Buttons, mrMatrixSettings;

type
  TmrMathMtxViewerFrame = class(TBasicVisualizerViewerFrame)
    pnlProperties: TPanel;
    lblCapWidth: TLabel;
    lblCapHeight: TLabel;
    lblWidth: TLabel;
    lblHeight: TLabel;
    lblCapSubWidth: TLabel;
    lblSubHeight: TLabel;
    lblSubWidth: TLabel;
    lblCapSubHeight: TLabel;
    grdData: TDrawGrid;
    lblCapLineWidth: TLabel;
    lblLineWidth: TLabel;
    chkOnlySubMatrix: TCheckBox;
    btnConfigure: TSpeedButton;
    procedure grdDataDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect;
      State: TGridDrawState);
    procedure chkOnlySubMatrixClick(Sender: TObject);
    procedure grdDataDblClick(Sender: TObject);
    procedure grdDataSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure btnConfigureClick(Sender: TObject);
  private
    type
      TMtxProps = record
        OX : integer;
        OY : integer;
        Width : integer;
        Height : integer;
        SubWidth : integer;
        SubHeight : Integer;
      end;
  private
    fMtx : IMatrix;
    fprops : TMtxProps;
    fmtSet : TFormatSettings;
    fName : string;

    fSettings : TfrmMtxSettings;

    // settings
    fBaseMaxNumCols : Integer;
    fBaseMaxNumRows : integer;
    fMaxNumCols : integer;
    fMaxNumRows : integer;
    fTextGap : integer;
    fShowIndexOffset : boolean;
    fPrec : integer;
    fR1, fR2 : double;
    fAlwaysShowCmplMtx : boolean;
    fMtxRmtAddr : LongWord;
    fMtxRmtLineWidth : integer;

    procedure OnSettingsChanged(Sender : TObject);

    function FormatValue(const Value : double) : string;

    procedure WriteRemoteValue(x, y : integer; value : double);
    procedure ShowData;
    procedure SetupGrid;
    procedure SetupcolWidths;
    procedure ReadSettings;
  protected
    procedure EvaluateExpression( Context: TVisualizerDebugContext; const Expression, TypeName, EvalResult: string); override;
  public
    procedure MarkUnavailable(Reason: TOTAVisualizerUnavailableReason); override;
    procedure AfterConstruction; override;
  end;

procedure Register;

implementation

{$R *.dfm}

uses
  TypInfo, Types, Math;

resourcestring
  sMtxVisualizerName = 'Matrix Visualizer for Delphi';
  sMtxVisualizerDescription = 'Displays the content of the actual matrix object';
  sMtxMenuText = 'Show Matrix';
  sMtxFormCaption = 'IMatrix Visualizer for %s';
  sMtxCantDisplay = 'Can''t access ';

type
  TmrMathMatrixDebuggerVisualizer = class(TBasicDebuggerVisualizer)
  protected
    function CreateForm( const Expression : String ) : TInterfacedObject; override;

  public
    function GetMenuText() : string;  override;
    function GetVisualizerName() : string;  override;
    function GetVisualizerDescription() : string;  override;

  public
    constructor Create();

  end;

var
  SLBlockVisualizer: IOTADebuggerVisualizer;

constructor TmrMathMatrixDebuggerVisualizer.Create();
begin
     inherited;
     
     AddType( 'TDoubleMatrix' );
     AddType( 'IMatrix' );
     AddType( 'TThreadedMatrix' );
end;

function TmrMathMatrixDebuggerVisualizer.CreateForm( const Expression : String ) : TInterfacedObject;
begin
     Result := TBasicDebuggerVisualizerForm.Create( Expression, TmrMathMtxViewerFrame, 'mrMathMatrixDebugVisualizer', sMtxFormCaption );
end;

function TmrMathMatrixDebuggerVisualizer.GetVisualizerDescription() : string;
begin
     Result := sMtxVisualizerDescription;
end;

function TmrMathMatrixDebuggerVisualizer.GetMenuText() : string;
begin
     Result := sMtxMenuText;
end;

function TmrMathMatrixDebuggerVisualizer.GetVisualizerName() : string;
begin
     Result := sMtxVisualizerName;
end;

procedure TmrMathMtxViewerFrame.EvaluateExpression( Context: TVisualizerDebugContext; const Expression, TypeName, EvalResult: string );
var data : PDouble;
begin
     fName := Expression;

     fMtxRmtAddr := 0;
     fMtxRmtLineWidth := 0;
     fMtx := nil;
     FillChar(fprops, sizeof(fprops), 0);

     try
        fprops.Width := EvaluateIntegerValue( Context, Expression + '.GetObjRef.fWidth');
        fprops.Height := EvaluateIntegerValue( Context, Expression + '.GetObjRef.fHeight');

        fprops.SubWidth := EvaluateIntegerValue( Context, Expression + '.GetObjRef.fSubWidth');
        fprops.SubHeight := EvaluateIntegerValue( Context, Expression + '.GetObjRef.fSubHeight');

        fprops.OX := EvaluateIntegerValue( Context, Expression + '.GetObjRef.fOffsetX');
        fprops.OY := EvaluateIntegerValue( Context, Expression + '.GetObjRef.fOffsetY');

        if (fprops.Width > 0) and (fprops.Height > 0) then
        begin
             fMtxRmtAddr := EvaluateLongWordValue( Context, expression + '.GetObjRef.fData' );
             fMtxRmtLineWidth := EvaluateLongWordValue( Context, expression + '.GetObjRef.fLineWidth' );

             data := GetMemory( fprops.Height*fMtxRmtLineWidth );

             try
                if Assigned(data) then
                begin
                     Context.CurProcess.ReadProcessMemory( fMtxRmtAddr, fprops.Height*fMtxRmtLineWidth, data^);

                     fMtx := TDoubleMatrix.Create( data, fMtxRmtLineWidth, fprops.Width, fprops.Height );
                     fMtx.SetSubMatrix(fprops.OX, fprops.Oy, fprops.SubWidth, fprops.SubHeight);
                     fMtx.GetObjRef.Name := Expression;
                     data := nil;
                end;
             finally
                    if Assigned(data) then
                       FreeMem(data);
             end;
       end;

       ShowData;
       grdData.Invalidate;
     except
           on E : Exception do
           begin
                MessageLabel.Caption := 'Error evaluating object: ' + E.Message;
           end;
     end;
end;

function TmrMathMtxViewerFrame.FormatValue(const Value: double): string;
begin
     if Value = 0 then
        exit('0');

     // formatting the value (todo: maybe a bit crude what I'm doing here...)
     if (Abs(Value) > fR1) or (Abs(Value) < fR2)
     then
         Result := Format('%.*e', [fPrec, Value], fmtSet)
     else
     begin
          Result := Format('%.*f', [fPrec, Value], fmtSet);

          // beautify the result in case it is very close to decimal
          if (RoundTo(Frac(Value), -fPrec) = 0) and (RoundTo(Frac(Value), Min(15, -2*fPrec)) = 0) then
             Result := Format('%.0f', [Value], fmtSet);
     end;
end;

procedure Register;
begin
     SLBlockVisualizer := TmrMathMatrixDebuggerVisualizer.Create();

     (BorlandIDEServices as IOTADebuggerServices).RegisterDebugVisualizer(SLBlockVisualizer);
end;

procedure RemoveVisualizer();
var DebuggerServices: IOTADebuggerServices;
begin
     if( Supports( BorlandIDEServices, IOTADebuggerServices, DebuggerServices ) ) then
     begin
          DebuggerServices.UnregisterDebugVisualizer(SLBlockVisualizer);
          SLBlockVisualizer := nil;
     end;
end;

procedure TmrMathMtxViewerFrame.ShowData;
begin
     if fAlwaysShowCmplMtx then
     begin
          fMaxNumCols := Max(fBaseMaxNumCols, fProps.Width);
          fMaxNumRows := Max(fBaseMaxNumRows, fProps.Height);
     end;

     pnlProperties.Visible := Assigned(fMtx);
     MessageLabel.Visible := not Assigned(fMtx);
     grdData.Visible := Assigned(fMtx);

     if Assigned(fMtx) then
     begin
          fMtx.UseFullMatrix;

          lblHeight.Caption := intToStr(fprops.Height);
          lblWidth.Caption := intToStr(fprops.Width);
          lblSubHeight.Caption := intToStr(fprops.SubHeight);
          lblSubWidth.Caption := intToStr(fprops.SubWidth);
          lblLineWidth.Caption := intToStr(fMtx.LineWidth);

          if chkOnlySubMatrix.Checked then
             fMtx.SetSubMatrix(fProps.Ox, fProps.Oy, fProps.SubWidth, fProps.SubHeight);

          SetupGrid;
     end
     else
         MessageLabel.Caption := sMtxCantDisplay + fName;
end;

procedure TmrMathMtxViewerFrame.grdDataDblClick(Sender: TObject);
var pt : TPoint;
    aCol, aRow : integer;
begin
     pt := Mouse.CursorPos;
     pt := grdData.ScreenToClient(pt);

     grdData.MouseToCell(pt.X, pt.Y, aCol, aRow);

     if (aCol = grdData.ColCount - 1) and (aCol = fMaxNumCols + 1) then
     begin
          inc(fMaxNumCols, fBaseMaxNumCols);

          // if that happens the second time then show all (possible ones)
          if fMaxNumCols = 3*fBaseMaxNumCols then
             fMaxNumCols := Min(65536, fprops.Width);

          SetupGrid;
     end;

     if (aRow = grdData.RowCount - 1) and (aRow = fMaxNumRows + 1) then
     begin
          inc(fMaxNumRows, fBaseMaxNumRows);

          if fMaxNumRows = 3*fBaseMaxNumRows then
             fMaxNumRows := Min(65536, fProps.Height);

          SetupGrid;
     end;
end;

procedure TmrMathMtxViewerFrame.grdDataDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
var s : string;
    val : double;
    indexOffsetX : integer;
    indexOffsetY : Integer;
begin
     if not assigned(fMtx) then
        exit;

     indexOffsetX := 0;
     indexOffsetY := 0;
     if fShowIndexOffset and not chkOnlySubMatrix.Checked and
       ( (fProps.Width <> fProps.SubWidth) or (fProps.SubHeight <> fProps.Height) ) then
     begin
          indexOffsetx := fProps.Ox;
          indexOffsetY := fProps.Oy;
     end;

     // draw fixed ones
     if (aCol = 0) or (aRow = 0) then
     begin
          if aRow = 0
          then
              s := IntToStr( aCol - indexOffsetX - 1)
          else
              s := IntToStr( aRow - indexOffsetY - 1);

          if (aCol = 0) and (aRow = 0) then
             s := '';

          grdData.Canvas.Brush.Color := RGB( 202, 236, 255);
          grdData.Canvas.Brush.Style := bsSolid;
          grdData.Canvas.FillRect( Rect );

          grdData.Canvas.TextOut( (Rect.Left + Rect.Right ) div 2 - grdData.Canvas.TextWidth(s) div 2,
                                  (Rect.Top + Rect.Bottom) div 2 - grdData.Canvas.TextHeight(s) div 2,
                                  s);
          exit;
     end;

     if (aCol > fMaxNumCols) or (aRow > fMaxNumRows) then
     begin
          s := '...';

          grdData.Canvas.Brush.Color := clLtGray;
          grdData.Canvas.Brush.Style := bsSolid;
          grdData.Canvas.FillRect( Rect );

          grdData.Canvas.TextOut( (Rect.Left + Rect.Right ) div 2 - grdData.Canvas.TextWidth(s) div 2,
                                  (Rect.Top + Rect.Bottom) div 2 - grdData.Canvas.TextHeight(s) div 2,
                                  s);
          exit;
     end;

     // draw data
     if (aCol <= fMtx.Width) and (aRow <= fMtx.Height) then
     begin
          dec(aCol);
          dec(aRow);

          val := fMtx[aCol, aRow];

          grdData.Canvas.Brush.Style := bsSolid;
          grdData.Canvas.FillRect( Rect );

          s := FormatValue(val);

          if ((aCol < fprops.Ox) or (aCol >= fProps.Ox + fprops.SubWidth) or
             (aRow < fProps.Oy) or (aRow >= fProps.Oy + fprops.SubHeight))
             and not chkOnlySubMatrix.Checked
          then
              grdData.Canvas.Brush.Color := $DFDFDF
          else
              grdData.Canvas.Brush.Color := clWhite;
          grdData.Canvas.Brush.Style := bsSolid;
          grdData.Canvas.FillRect( Rect );

          grdData.Canvas.TextRect( Rect, Rect.Left + fTextGap,
                                  (Rect.Top + Rect.Bottom) div 2 - grdData.Canvas.TextHeight(s) div 2,
                                  s);
     end;
end;

procedure TmrMathMtxViewerFrame.MarkUnavailable(
  Reason: TOTAVisualizerUnavailableReason);
begin
     inherited;

     grdData.Visible := FAvailableState = asAvailable;
     pnlProperties.Visible := FAvailableState = asAvailable;
end;

procedure TmrMathMtxViewerFrame.OnSettingsChanged(Sender: TObject);
begin
     ReadSettings;

     SetupGrid;
end;

procedure TmrMathMtxViewerFrame.AfterConstruction;
begin
     fSettings := TfrmMtxSettings.Create(self);
     fSettings.OnChanged := OnSettingsChanged;
     
     ReadSettings;

     inherited;
end;

procedure TmrMathMtxViewerFrame.ReadSettings;
begin
     GetLocaleFormatSettings(0, fmtSet);
     fmtSet.DecimalSeparator := '.';
     fTextGap := 15;

     fMaxNumCols := fSettings.MaxNumCols;
     fMaxNumRows := fSettings.MaxNumRows;
     fPrec := fSettings.Prec;
     fShowIndexOffset := fSettings.ShowIndexOffset;
     fAlwaysShowCmplMtx := fSettings.AlwaysShowCmplMtx;
     chkOnlySubMatrix.Checked := fSettings.OnlySubMatrix;
     
     fBaseMaxNumCols := fMaxNumCols;
     fBaseMaxNumRows := fMaxNumRows;

     fR1 := Power(10, fPrec);
     fR2 := Power(10, -fPrec);
end;

procedure TmrMathMtxViewerFrame.SetupcolWidths;
var s : string;
    colCnt : integer;
    cnt : integer;
    actWidth : integer;
begin
     if not Assigned(fMtx) then
        exit;

     grdData.ColWidths[0] := 2*fTextGap + grdData.Canvas.TextWidth(IntToStr(fMaxNumRows));

     // get the maximum width for each column
     for colCnt := 0 to grdData.ColCount - 2 do
     begin
          actWidth := grdData.DefaultColWidth;
          for cnt := 0 to grdData.RowCount - 2 do
          begin
               s := FormatValue(fMtx[colCnt, cnt]);

               actWidth := Max(actWidth, 2*fTextGap + grdData.Canvas.TextWidth(s));
          end;

          grdData.ColWidths[colCnt + 1] := actWidth;
     end;
end;

procedure TmrMathMtxViewerFrame.btnConfigureClick(Sender: TObject);
var pt : TPoint;
begin
     pt.X := btnConfigure.Left;
     pt.Y := btnConfigure.Top;
     pt := btnConfigure.Parent.ClientToScreen(pt);

     fSettings.Left := pt.X - fSettings.Width;
     fSettings.Top := pt.Y - fSettings.Height;

     fSettings.Show;
end;

procedure TmrMathMtxViewerFrame.chkOnlySubMatrixClick(Sender: TObject);
begin
     if not assigned(fMtx) then
        exit;

     if chkOnlySubMatrix.Checked
     then
         fMtx.SetSubMatrix(fProps.Ox, fProps.Oy, fProps.SubWidth, fProps.SubHeight)
     else
         fMtx.UseFullMatrix;

     SetupGrid;
end;

procedure TmrMathMtxViewerFrame.SetupGrid;
begin
     // setup grid
     grdData.ColCount := Min( fMtx.Width, fMaxNumCols + 1) + 1;
     grdData.RowCount := Min( fMtx.Height, fMaxNumRows + 1) + 1;

     SetupColWidths;
end;

procedure TmrMathMtxViewerFrame.grdDataSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
var dVal : double;
begin
     if not Assigned(fMtx) then
        exit;

     // try to update the value
     if TryStrToFloat( Value, dVal, fmtSet ) then
     begin
          // update the matrix
          dec(ACol);
          dec(ARow);

          if chkOnlySubMatrix.Checked then
          begin
               dec(aCol, fprops.OX);
               dec(aRow, fProps.OY);
          end;

          if (aCol >= 0) and (aCol < fprops.SubWidth) and
             (aRow >= 0) and (aRow < fProps.SubHeight) then
          begin
               WriteRemoteValue( aCol + fProps.OX, aRow + fProps.OY, dVal);

               fMtx[aCol, aRow] := dVal;
          end;
     end;

     grdData.Invalidate;
end;

procedure TmrMathMtxViewerFrame.WriteRemoteValue(x, y: integer; value: double);
var Context: TVisualizerDebugContext;
begin
     if fMtxRmtAddr = 0 then
        exit;

     if Supports(BorlandIDEServices, IOTADebuggerServices, Context.DebugSvcs ) then
        Context.CurProcess := Context.DebugSvcs.CurrentProcess;

     if Context.CurProcess = nil then
        Exit;

     Context.CurThread := Context.CurProcess.CurrentThread;
     if Context.CurThread = nil then
        Exit;

     Context.CurProcess.WriteProcessMemory( fMtxRmtAddr + Cardinal(x*sizeof(double)) + Cardinal(y*fMtxRmtLineWidth),
                                            sizeof(double), value);
end;

initialization

finalization
  RemoveVisualizer();

end.
