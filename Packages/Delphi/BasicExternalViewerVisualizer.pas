{*> Ver: V1.1 *********      History      ***************************\

V1.0  09/13/2009  Released
V1.1  09/19/2009  Small improvements

Legal issues: Copyright (C) 2009 by Boian Mitov
              mitov@mitov.com
              www.mitov.com

              This software is provided 'as-is', without any express or
              implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.
\***************************************************************************}

// ###################################################################
// #### This file is part of the mathematics library project, and is
// #### offered under the licence agreement described on
// #### http://www.mrsoft.org/
// ####
// #### Copyright:(c) 2017, Michael R. . All rights reserved.
// ####
// #### Unless required by applicable law or agreed to in writing, software
// #### distributed under the License is distributed on an "AS IS" BASIS,
// #### WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// #### See the License for the specific language governing permissions and
// #### limitations under the License.
// ###################################################################

// removed unused and uncommented code

unit BasicExternalViewerVisualizer;

interface

uses
  Windows, ActnList, ImgList, Menus, Classes, Graphics, Controls, Forms,
  IniFiles, ToolsAPI, DesignIntf, ExtCtrls, StdCtrls, ComCtrls;

type
  TAvailableState = (asAvailable, asProcRunning, asOutOfScope);

type
  TVisualizerDebugContext = record
    DebugSvcs: IOTADebuggerServices;
    CurProcess: IOTAProcess;
    CurThread: IOTAThread;
  end;

  TBasicVisualizerViewerFrame = class(TFrame, IOTADebuggerVisualizerExternalViewerUpdater, IOTAThreadNotifier)
    MessageLabel: TLabel;
  protected
    FOwningForm: TCustomForm;
    FAvailableState: TAvailableState;
    FClosedProc: TOTAVisualizerClosedProcedure;
    FNotifierIndex: Integer;

    FEvaluteCompleted: Boolean;
    FDeferredEvaluteResult: string;
    FDeferredEvaluteAddress: LongWord;
    FDeferredEvaluteError: Boolean;

    FModifyCompleted: Boolean;
    FDeferredModifyResult: string;
    FDeferredModifyError: Boolean;

  protected
    procedure SetParent(AParent: TWinControl); override;

  protected
    function EvaluateStringValue(Context: TVisualizerDebugContext; Expression: string) : String;
    function EvaluateIntegerValue(Context: TVisualizerDebugContext; Expression: string) : Integer;
    function EvaluateLongWordValue(Context: TVisualizerDebugContext; Expression: string) : LongWord;
    function ModifyLastStringValue( Context: TVisualizerDebugContext; Value : String ) : Boolean;
    function ModifyLastLongWordValue( Context: TVisualizerDebugContext; Value : LongWord ) : Boolean;

    procedure EvaluateComplete(const ExprStr, ResultStr: string; CanModify: Boolean;
      ResultAddress, ResultSize: LongWord; ReturnCode: Integer);

    function AllocateRemoteMem(Context: TVisualizerDebugContext; ASize: Cardinal) : LongWord;
    procedure FreeRemoteMem(Context: TVisualizerDebugContext; ARemoteMem: LongWord);

  protected
    procedure SetForm(AForm: TCustomForm);
    procedure PerformEvaluateExpression(const Expression, TypeName, EvalResult: string); virtual;
    procedure EvaluateExpression( Context: TVisualizerDebugContext; const Expression, TypeName, EvalResult: string); virtual; abstract;

  public
    procedure CloseVisualizer(); virtual;
    procedure MarkUnavailable(Reason: TOTAVisualizerUnavailableReason); virtual;
    procedure RefreshVisualizer(const Expression, TypeName, EvalResult: string); virtual;
    procedure SetClosedCallback(ClosedProc: TOTAVisualizerClosedProcedure);

    procedure ThreadNotify(Reason: TOTANotifyReason); virtual;
    procedure EvaluteComplete(const ExprStr, ResultStr: string; CanModify: Boolean; ResultAddress, ResultSize: LongWord; ReturnCode: Integer); virtual;
    procedure ModifyComplete(const ExprStr, ResultStr: string; ReturnCode: Integer); virtual;
    procedure AfterSave(); virtual;
    procedure BeforeSave(); virtual;
    procedure Destroyed(); virtual;
    procedure Modified(); virtual;

  end;

type
  TBasicDebuggerVisualizer = class(TInterfacedObject, IOTADebuggerVisualizer,
    IOTADebuggerVisualizerExternalViewer)
  protected
    FSupportedTypes: array of String;

  protected
    procedure AddType(AValue: String);

  protected
    function CreateForm(const Expression: String): TInterfacedObject; virtual;
      abstract;

  public
    function GetVisualizerName(): string; virtual; abstract;
    function GetVisualizerDescription(): string; virtual; abstract;
    function GetMenuText(): string; virtual; abstract;

  public
    function GetSupportedTypeCount(): Integer;
    procedure GetSupportedType(Index: Integer; var TypeName: string;
      var AllDescendants: Boolean);
    function GetVisualizerIdentifier(): string;
    function Show(const Expression, TypeName, EvalResult: string;
      Suggestedleft, SuggestedTop: Integer):
      IOTADebuggerVisualizerExternalViewerUpdater;

  end;

type
  IVisualizerFrameFormHelper = interface
    ['{C2019B70-A5DE-43DC-83D9-EAC62C3BEF6C}']
    function GetForm: TCustomForm;
    function GetFrame: TCustomFrame;
    procedure SetForm(Form: TCustomForm);
    procedure SetFrame(Form: TCustomFrame);
  end;

type
  TBasicDebuggerVisualizerForm = class(TInterfacedObject, INTACustomDockableForm,
    IVisualizerFrameFormHelper)
  protected
    FExpression: String;
    FFrameClass: TCustomFrameClass;
    FCaptionFormatString: String;
    FIdentifier: String;
    FViewerForm: TCustomForm;
    FViewerFrame: TBasicVisualizerViewerFrame;

  public
    constructor Create(const AExpression: string;
      AFrameClass: TCustomFrameClass; AIdentifier: String;
      ACaptionFormatString: String);

  public
    // INTACustomDockableForm
    function GetCaption(): String;
    function GetFrameClass(): TCustomFrameClass;
    procedure FrameCreated(AFrame: TCustomFrame);
    function GetIdentifier(): string;
    function GetMenuActionList(): TCustomActionList;
    function GetMenuImageList(): TCustomImageList;
    procedure CustomizePopupMenu(PopupMenu: TPopupMenu);
    function GetToolbarActionList: TCustomActionList;
    function GetToolbarImageList: TCustomImageList;
    procedure CustomizeToolBar(ToolBar: TToolBar);
    procedure LoadWindowState(Desktop: TCustomIniFile; const Section: string);
    procedure SaveWindowState(Desktop: TCustomIniFile; const Section: string;
      IsProject: Boolean);
    function GetEditState(): TEditState;
    function EditAction(Action: TEditAction): Boolean;
  public
    // IFrameFormHelper
    function GetForm(): TCustomForm;
    function GetFrame(): TCustomFrame;
    procedure SetForm(Form: TCustomForm);
    procedure SetFrame(Frame: TCustomFrame);

  end;

implementation

{$R *.dfm}

uses
  SysUtils;

resourcestring
  sBasicProcessNotAccessible = 'process not accessible';
  sBasicOutOfScope = 'out of scope';

function TBasicVisualizerViewerFrame.EvaluateStringValue(Context: TVisualizerDebugContext;
  Expression: string): String;
var
  EvalRes: TOTAEvaluateResult;
  ResultStr: array [0 .. 4095] of Char;
  CanModify: Boolean;
  ResultAddr, ResultSize, ResultVal: LongWord;

begin
  EvalRes := Context.CurThread.Evaluate( Expression, @ResultStr, Length(ResultStr), CanModify, eseAll, '', ResultAddr, ResultSize, ResultVal, '', 0);

  case EvalRes of
    erOK:
      Result := ResultStr;

    erDeferred:
      begin
      FEvaluteCompleted := False;
      FDeferredEvaluteResult := '';
      FDeferredEvaluteError := False;
      FNotifierIndex := Context.CurThread.AddNotifier(Self);
      while not FEvaluteCompleted do
        Context.DebugSvcs.ProcessDebugEvents();

      Context.CurThread.RemoveNotifier(FNotifierIndex);
      FNotifierIndex := -1;
      if not FDeferredEvaluteError then
        begin
        if FDeferredEvaluteResult <> '' then
          Result := FDeferredEvaluteResult

        else
          Result := '';

        end;
      end;

    erBusy:
      begin
      Context.DebugSvcs.ProcessDebugEvents();
      Result := EvaluateStringValue(Context, Expression);
      end;

  end;
end;

function SLStrToIntDef(AValue: String; ADefaultValue: Integer): Integer;
begin
  Result := ADefaultValue;
  if (Length(AValue) < 1) then
    Exit;

  if (AValue[1] = ':') then
    AValue[1] := '$';

  Result := StrToIntDef(AValue, ADefaultValue);
end;

procedure TBasicVisualizerViewerFrame.EvaluateComplete(const ExprStr,
  ResultStr: string; CanModify: Boolean; ResultAddress, ResultSize: LongWord;
  ReturnCode: Integer);
begin
     FEvaluteCompleted := True;
     FDeferredEvaluteResult := ResultStr;
     FDeferredEvaluteAddress := ResultAddress;
     FDeferredEvaluteError := ReturnCode <> 0;
end;

function TBasicVisualizerViewerFrame.EvaluateIntegerValue
  (Context: TVisualizerDebugContext; Expression: string): Integer;
begin
  Result := SLStrToIntDef(EvaluateStringValue(Context, Expression), -1);
end;

function TBasicVisualizerViewerFrame.EvaluateLongWordValue
  (Context: TVisualizerDebugContext; Expression: string): LongWord;
begin
  Result := LongWord(SLStrToIntDef(EvaluateStringValue(Context, Expression), 0));
end;

function TBasicVisualizerViewerFrame.ModifyLastStringValue( Context: TVisualizerDebugContext; Value : String ) : Boolean;
var
  EvalRes: TOTAEvaluateResult;
  ResultStr: array [0 .. 4095] of Char;
//  ResultAddr, ResultSize: LongWord;
  ResultVal: Integer;

begin
  EvalRes := Context.CurThread.Modify( Value, @ResultStr, Length(ResultStr), ResultVal );

  Result := False;
  case EvalRes of
    erOK:
      Result := True;

    erDeferred:
      begin
      FModifyCompleted := False;
      FDeferredModifyResult := '';
      FDeferredModifyError := False;
      FNotifierIndex := Context.CurThread.AddNotifier(Self);
      while not FModifyCompleted do
        Context.DebugSvcs.ProcessDebugEvents();

      Context.CurThread.RemoveNotifier(FNotifierIndex);
      FNotifierIndex := -1;
      if not FDeferredModifyError then
        begin
        Result := True;
{
        if FDeferredModifyResult <> '' then
          Result := FDeferredModifyResult

        else
          Result := '';
}
        end;
      end;

    erBusy:
      begin
      Context.DebugSvcs.ProcessDebugEvents();
      Result := ModifyLastStringValue( Context, Value );
      end;

  end;
end;

function TBasicVisualizerViewerFrame.ModifyLastLongWordValue( Context: TVisualizerDebugContext; Value : LongWord ) : Boolean;
begin
  Result := ModifyLastStringValue( Context, IntToStr( Value ));
end;

function TBasicVisualizerViewerFrame.AllocateRemoteMem(Context: TVisualizerDebugContext;
  ASize: Cardinal): LongWord;
var
  I: Integer;

begin
  for I := 0 to 100 do
  begin
    Result := EvaluateLongWordValue(Context, 'GlobalAlloc( GMEM_FIXED, ' + IntToStr(ASize) + ')');
    if (Result <> 0) then
      Break;

    Context.DebugSvcs.ProcessDebugEvents();
  end;

end;

procedure TBasicVisualizerViewerFrame.FreeRemoteMem(Context: TVisualizerDebugContext;
  ARemoteMem: LongWord);
begin
  EvaluateLongWordValue(Context, 'GlobalFree(' + IntToStr(ARemoteMem) + ')');
end;

procedure TBasicVisualizerViewerFrame.CloseVisualizer();
begin
  if FOwningForm <> nil then
    FOwningForm.Close;

end;

procedure TBasicVisualizerViewerFrame.MarkUnavailable(Reason: TOTAVisualizerUnavailableReason );
begin
  MessageLabel.Visible := True;
  if Reason = ovurProcessRunning then
    begin
    MessageLabel.Caption := sBasicProcessNotAccessible;
    FAvailableState := asProcRunning
    end

  else if Reason = ovurOutOfScope then
    begin
    MessageLabel.Caption := sBasicOutOfScope;
    FAvailableState := asOutOfScope;
    end;

end;

procedure TBasicVisualizerViewerFrame.Modified;
begin

end;

procedure TBasicVisualizerViewerFrame.RefreshVisualizer(const Expression, TypeName,
  EvalResult: string);
begin
  FAvailableState := asAvailable;
  MessageLabel.Visible := False;
  PerformEvaluateExpression(Expression, TypeName, EvalResult);
end;

procedure TBasicVisualizerViewerFrame.SetClosedCallback(
  ClosedProc: TOTAVisualizerClosedProcedure);
begin
  FClosedProc := ClosedProc;
end;

procedure TBasicVisualizerViewerFrame.PerformEvaluateExpression(const Expression, TypeName, EvalResult: string);
var
  Context: TVisualizerDebugContext;

begin
  FAvailableState := asAvailable;

  if Supports(BorlandIDEServices, IOTADebuggerServices, Context.DebugSvcs ) then
    Context.CurProcess := Context.DebugSvcs.CurrentProcess;

  if( Context.CurProcess = nil ) then
    Exit;

  Context.CurThread := Context.CurProcess.CurrentThread;
  if( Context.CurThread = nil ) then
    Exit;

  EvaluateExpression( Context, Expression, TypeName, EvalResult );
end;

procedure TBasicVisualizerViewerFrame.SetForm(AForm: TCustomForm);
begin
  FOwningForm := AForm;
end;

procedure TBasicVisualizerViewerFrame.SetParent(AParent: TWinControl);
begin
  if AParent = nil then
    begin
    if Assigned(FClosedProc) then
      FClosedProc;

    end;

  inherited;
end;

procedure TBasicVisualizerViewerFrame.ThreadNotify(Reason: TOTANotifyReason);
begin

end;

procedure TBasicVisualizerViewerFrame.AfterSave;
begin

end;

procedure TBasicVisualizerViewerFrame.BeforeSave;
begin

end;

procedure TBasicVisualizerViewerFrame.Destroyed;
begin

end;

//type
//  PBitmap = ^TBitmap;

(*
procedure TBasicVisualizerViewerFrame.Evaluate(Expression: string);
const
  BitCounts: array [pf1Bit..pf32Bit] of Byte = (1,4,8,16,16,24,32);

var
  CurProcess: IOTAProcess;
  CurThread: IOTAThread;
  ResultStr: array[0..4095] of Char;
  EvalRes: TOTAEvaluateResult;
  DebugSvcs: IOTADebuggerServices;
  CanModify: Boolean;
  ResultAddr, ResultSize, ResultVal: LongWord;
  ARemoteHandle : LongWord;
  ARemoteMem1    : LongWord;
  ARemoteMem2    : LongWord;
  BI: TBitmapInfoHeader;
  AScanSize : Integer;
  I : Integer;
  APixelFormat: TPixelFormat;
  ASize       : TSize;
//  DS            : TDIBSection;

  function EvaluateStringValue( Expression: string): String;
  begin
    EvalRes := CurThread.Evaluate( Expression, @ResultStr, Length(ResultStr),
          CanModify, eseAll, '', ResultAddr, ResultSize, ResultVal, '', 0);

    case EvalRes of
      erOK: Result := '';
      erDeferred:
        begin
        FCompleted := False;
        FDeferredResult := '';
        FDeferredError := False;
        FNotifierIndex := CurThread.AddNotifier(Self);
        while not FCompleted do
          DebugSvcs.ProcessDebugEvents;

        CurThread.RemoveNotifier(FNotifierIndex);
        FNotifierIndex := -1;
        if not FDeferredError then
          begin
          if FDeferredResult <> '' then
            Result := FDeferredResult

          else
            Result := ResultStr;

          end;
        end;

      erBusy:
        begin
        DebugSvcs.ProcessDebugEvents;
        Result := EvaluateStringValue(Expression);
        end;

    end;
  end;

  function EvaluateIntegerValue( Expression: string): Integer;
  begin
    Result := StrToIntDef( EvaluateStringValue( Expression ), -1 );
  end;

  function EvaluateLongWordValue( Expression: string): LongWord;
  begin
    Result := StrToIntDef( EvaluateStringValue( Expression ), 0 );
  end;

begin
  if Supports(BorlandIDEServices, IOTADebuggerServices, DebugSvcs) then
    CurProcess := DebugSvcs.CurrentProcess;

  if CurProcess <> nil then
    begin
    CurThread := CurProcess.CurrentThread;
    if CurThread <> nil then
      begin
      ASize.cx := EvaluateIntegerValue( Expression + '.Width' );
      ASize.cy := EvaluateIntegerValue( Expression + '.Height' );
      SizeLabel.Caption := IntToStr( ASize.cx ) + 'x' + IntToStr( ASize.cy );
      FormatLabel.Caption := EvaluateStringValue( Expression + '.PixelFormat' );

      APixelFormat := TPixelFormat( EnumIndexFromIdent( TypeInfo( TPixelFormat ), FormatLabel.Caption ));
      if( ( APixelFormat = pfDevice ) or ( APixelFormat = pfCustom )) then
        begin
        PreviewImage.Visible := False;
        MessageLabel.Visible := True;
        MessageLabel.Font.Size := 20;
        MessageLabel.Caption := sBitmapCantDisplay;
        Exit;
        end;

      PreviewImage.AutoSize := True;
      PreviewImage.Picture.Bitmap.Width := ASize.cx;
      PreviewImage.Picture.Bitmap.Height := ASize.cy;

      if( APixelFormat = pf15bit ) then
        APixelFormat := pf16bit;

      PreviewImage.Picture.Bitmap.PixelFormat := APixelFormat;

      PreviewImage.Picture.Bitmap.AlphaFormat := TAlphaFormat( EnumIndexFromIdent( TypeInfo( TPixelFormat ), EvaluateStringValue( Expression + '.AlphaFormat' )));

      ARemoteHandle := 0;
      while( ARemoteHandle = 0 ) do
        begin
        ARemoteHandle := EvaluateLongWordValue( 'CreateCompatibleDC( 0 )' );
        DebugSvcs.ProcessDebugEvents();
        end;

      if( ARemoteHandle <> 0 ) then
        begin
//          ARemoteMem := EvaluateLongWordValue( 'GetMemory(' + IntToStr( Image1.Picture.Bitmap.Width * Image1.Picture.Bitmap.Height ) + ')' );
        BI.biSize := SizeOf(BI);
        BI.biWidth := PreviewImage.Picture.Bitmap.Width;
        BI.biHeight := PreviewImage.Picture.Bitmap.Height;
        BI.biBitCount := BitCounts[ PreviewImage.Picture.Bitmap.PixelFormat ];

        BI.biClrUsed := 0;
        BI.biPlanes := 1;
        AScanSize := BytesPerScanLine(BI.biWidth, BI.biBitCount, 32 );
        BI.biSizeImage := AScanSize * Abs(BI.biHeight);


        BI.biClrImportant := 0;

        for I := 0 to 100 do
          begin
          ARemoteMem1 := EvaluateLongWordValue( 'GlobalAlloc( GMEM_FIXED, ' + IntToStr( SizeOf( TBitmapInfoHeader )) + ')' );
          if( ARemoteMem1 <> 0 ) then
            Break;

          DebugSvcs.ProcessDebugEvents();
          end;

        if( ARemoteMem1 <> 0 ) then
          begin
          for I := 0 to 100 do
            begin
            ARemoteMem2 := EvaluateLongWordValue( 'GlobalAlloc( GMEM_FIXED, ' + IntToStr( AScanSize * PreviewImage.Picture.Bitmap.Height ) + ')' );
            if( ARemoteMem2 <> 0 ) then
              Break;

            DebugSvcs.ProcessDebugEvents;
            end;

          if( ARemoteMem2 <> 0 ) then
            begin
            CurProcess.WriteProcessMemory( ARemoteMem1, SizeOf( BI ), BI );

            EvaluateLongWordValue( 'GetDIBits(' + IntToStr( ARemoteHandle ) + ', ' + Expression + '.Handle, 0, ' + IntToStr( PreviewImage.Picture.Bitmap.Height ) + ', Pointer(' + IntToStr( ARemoteMem2 ) + ' ), PBitmapInfo( ' + IntToStr( ARemoteMem1 ) + ' )^, DIB_RGB_COLORS )' );
            CurProcess.ReadProcessMemory( ARemoteMem2, AScanSize * PreviewImage.Picture.Bitmap.Height, PreviewImage.Picture.Bitmap.ScanLine[ PreviewImage.Picture.Bitmap.Height - 1 ]^ );
            EvaluateLongWordValue( 'GlobalFree(' + IntToStr( ARemoteMem2 ) + ')' );
            end;

          EvaluateLongWordValue( 'GlobalFree(' + IntToStr( ARemoteMem1 ) + ')' );
          end;

        EvaluateLongWordValue( 'DeleteDC(' + IntToStr( ARemoteHandle ) + ')' );
        end;

{
      if( EvaluateStringValue( Expression + '.HandleType' ) = 'bmDDB' ) then
      else
        begin
        SourceAdress := EvaluateLongWordValue( Expression + '.ScanLine[ 0 ]' );
        case( Image1.Picture.Bitmap.PixelFormat ) of
          pf1bit :
            CurProcess.ReadProcessMemory( SourceAdress, Image1.Picture.Bitmap.Height * Image1.Picture.Bitmap.Width div 8, Image1.Picture.Bitmap.ScanLine[ 0 ]^ );

          end;
        end;
}

      end;
    end;

end;
*)

procedure TBasicVisualizerViewerFrame.EvaluteComplete
  (const ExprStr, ResultStr: string; CanModify: Boolean;
  ResultAddress, ResultSize: LongWord; ReturnCode: Integer);
begin
  FEvaluteCompleted := True;
  FDeferredEvaluteResult := ResultStr;
  FDeferredEvaluteAddress := ResultAddress;
  FDeferredEvaluteError := ReturnCode <> 0;
end;

procedure TBasicVisualizerViewerFrame.ModifyComplete(const ExprStr, ResultStr: string; ReturnCode: Integer);
begin
  FModifyCompleted := True;
  FDeferredModifyResult := ResultStr;
  FDeferredModifyError := ReturnCode <> 0;
end;

procedure TBasicDebuggerVisualizer.AddType(AValue: String);
begin
  SetLength(FSupportedTypes, Length(FSupportedTypes) + 1);
  FSupportedTypes[Length(FSupportedTypes) - 1] := AValue;
end;

procedure TBasicDebuggerVisualizer.GetSupportedType(Index: Integer;
  var TypeName: string; var AllDescendants: Boolean);
begin
  TypeName := FSupportedTypes[Index];
end;

function TBasicDebuggerVisualizer.GetSupportedTypeCount(): Integer;
begin
  Result := 2;
end;

function TBasicDebuggerVisualizer.GetVisualizerIdentifier: string;
begin
  Result := ClassName;
end;

function TBasicDebuggerVisualizer.Show(const Expression, TypeName, EvalResult: string; SuggestedLeft, SuggestedTop: Integer): IOTADebuggerVisualizerExternalViewerUpdater;
var
  AForm: TCustomForm;
  AFrame: TBasicVisualizerViewerFrame;
  VisDockForm: INTACustomDockableForm;

begin
  VisDockForm := CreateForm(Expression) as INTACustomDockableForm;
  AForm := (BorlandIDEServices as INTAServices).CreateDockableForm(VisDockForm);
  AForm.Left := Suggestedleft;
  AForm.Top := SuggestedTop; (VisDockForm as IVisualizerFrameFormHelper).SetForm(AForm);
  AFrame := (VisDockForm as IVisualizerFrameFormHelper).GetFrame() as TBasicVisualizerViewerFrame;
  AFrame.PerformEvaluateExpression(Expression, TypeName, EvalResult);
  Result := AFrame as IOTADebuggerVisualizerExternalViewerUpdater;
end;

constructor TBasicDebuggerVisualizerForm.Create(const AExpression: string;
  AFrameClass: TCustomFrameClass; AIdentifier: String;
  ACaptionFormatString: String);
begin
  inherited Create;

  FExpression := AExpression;
  FFrameClass := AFrameClass;
  FIdentifier := AIdentifier;
  FCaptionFormatString := ACaptionFormatString;
end;

function TBasicDebuggerVisualizerForm.GetCaption(): string;
begin
  Result := Format(FCaptionFormatString, [FExpression]);
end;

function TBasicDebuggerVisualizerForm.GetFrameClass(): TCustomFrameClass;
begin
  Result := FFrameClass;
end;

procedure TBasicDebuggerVisualizerForm.FrameCreated(AFrame: TCustomFrame);
begin
  FViewerFrame := TBasicVisualizerViewerFrame(AFrame);
end;

function TBasicDebuggerVisualizerForm.GetIdentifier(): string;
begin
  Result := FIdentifier;
end;

function TBasicDebuggerVisualizerForm.GetMenuActionList: TCustomActionList;
begin
  Result := nil;
end;

function TBasicDebuggerVisualizerForm.GetMenuImageList: TCustomImageList;
begin
  Result := nil;
end;

procedure TBasicDebuggerVisualizerForm.CustomizePopupMenu(PopupMenu: TPopupMenu);
begin
  // no toolbar
end;

procedure TBasicDebuggerVisualizerForm.CustomizeToolBar(ToolBar: TToolBar);
begin
  // no toolbar
end;

function TBasicDebuggerVisualizerForm.EditAction(Action: TEditAction): Boolean;
begin
  Result := False;
end;

function TBasicDebuggerVisualizerForm.GetToolbarActionList: TCustomActionList;
begin
  Result := nil;
end;

function TBasicDebuggerVisualizerForm.GetToolbarImageList: TCustomImageList;
begin
  Result := nil;
end;

procedure TBasicDebuggerVisualizerForm.LoadWindowState(Desktop: TCustomIniFile;
  const Section: string);
begin
  // no desktop saving
end;

procedure TBasicDebuggerVisualizerForm.SaveWindowState(Desktop: TCustomIniFile;
  const Section: string; IsProject: Boolean);
begin
  // no desktop saving
end;

function TBasicDebuggerVisualizerForm.GetEditState(): TEditState;
begin
  Result := [];
end;

function TBasicDebuggerVisualizerForm.GetForm(): TCustomForm;
begin
  Result := FViewerForm;
end;

function TBasicDebuggerVisualizerForm.GetFrame(): TCustomFrame;
begin
  Result := FViewerFrame;
end;

procedure TBasicDebuggerVisualizerForm.SetForm(Form: TCustomForm);
begin
  FViewerForm := Form;
  if (Assigned(FViewerFrame)) then
    FViewerFrame.SetForm(FViewerForm);

end;

procedure TBasicDebuggerVisualizerForm.SetFrame(Frame: TCustomFrame);
begin
  FViewerFrame := TBasicVisualizerViewerFrame(Frame);
end;

end.
