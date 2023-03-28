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

{$IF CompilerVersion < 23}
type
  TOTAAddress = LongWord;
{$IFEND}

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
    function EvaluateInt64Value(Context: TVisualizerDebugContext; Expression: string) : Int64;
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
  ResultAddr : TOTAAddress;
  ResultSize, ResultVal: LongWord;

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

function SLStrToIntDef(AValue: String; ADefaultValue: integer): Integer;
begin
  Result := ADefaultValue;
  if (Length(AValue) < 1) then
    Exit;

  if (AValue[1] = ':') then
    AValue[1] := '$';

  Result := StrToIntDef(AValue, ADefaultValue);
end;

function SLStrToInt64Def(AValue: String; ADefaultValue: Int64): int64;
begin
    Result := ADefaultValue;
    if (Length(AValue) < 1) then
       Exit;

    if (AValue[1] = ':') then
      AValue[1] := '$';

    Result := StrToInt64Def(AValue, ADefaultValue);
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

function TBasicVisualizerViewerFrame.EvaluateInt64Value(
  Context: TVisualizerDebugContext; Expression: string): Int64;
begin
     Result := SLStrToInt64Def(EvaluateStringValue(Context, Expression), -1);
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
