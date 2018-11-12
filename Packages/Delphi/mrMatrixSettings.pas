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

unit mrMatrixSettings;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfrmMtxSettings = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    chkAlwaysShowCmplMtx: TCheckBox;
    chkShowIndexOffset: TCheckBox;
    chkOnlySubMatrix: TCheckBox;
    edDefMaxRows: TEdit;
    edDefMaxColumns: TEdit;
    edPrecision: TEdit;
    procedure FormDeactivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure edDefMaxRowsExit(Sender: TObject);
    procedure edDefMaxColumnsExit(Sender: TObject);
    procedure edPrecisionExit(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    fOnChanged: TNotifyEvent;

    fMaxNumCols : integer;
    fMaxNumRows : integer; 
    fPrec : integer; 
    fShowIndexOffset : boolean;
    fAlwaysShowCmplMtx : boolean;

    fOnlySubMatrix : boolean;
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
    property MaxNumCols : integer read fMaxNumCols;
    property MaxNumRows : integer read fMaxNumRows; 
    property Prec : integer read fPrec; 
    property ShowIndexOffset : boolean read fShowIndexOffset;
    property AlwaysShowCmplMtx : boolean read fAlwaysShowCmplMtx;
    property OnlySubMatrix : boolean read fOnlySubMatrix;

    property OnChanged : TNotifyEvent read fOnChanged write fOnChanged;
  end;

implementation

uses Registry;

{$R *.dfm}

procedure TfrmMtxSettings.edDefMaxColumnsExit(Sender: TObject);
var value : integer;
begin
     if not TryStrToInt(edDefMaxColumns.Text, value) or (value < 1) or (value > 65536) then
        edDefMaxColumns.Text := IntToStr(fMaxNumCols);
end;

procedure TfrmMtxSettings.edDefMaxRowsExit(Sender: TObject);
var value : integer;
begin
     if not TryStrToInt(edDefMaxRows.Text, value) or (value < 1) or (value > 65536) then
        edDefMaxRows.Text := IntToStr(fMaxNumRows)
end;

procedure TfrmMtxSettings.edPrecisionExit(Sender: TObject);
var value : integer;
begin
     if not TryStrToInt(edPrecision.Text, value) or (value < 0) or (value > 15) then
        edPrecision.Text := IntToStr(fPrec);
end;

procedure TfrmMtxSettings.FormClose(Sender: TObject; var Action: TCloseAction);
var reg : TRegIniFile;
    aMaxNumCols : integer;
    aMaxNumRows : integer; 
    aPrec : integer; 
    aShowIndexOffset : boolean;
    aAlwaysShowCmplMtx : boolean;
    aOnlySubMatrix : boolean;
begin
     if not TryStrToInt(edDefMaxColumns.Text, aMaxNumCols) then
        aMaxNumCols := fMaxNumCols;
     if not TryStrToInt(edDefMaxRows.Text, aMaxNumRows) then
        aMaxNumRows := fMaxNumRows;
     if not TryStrToInt(edPrecision.Text, aPrec) then
        aPrec := fPrec;

     aShowIndexOffset := chkShowIndexOffset.Checked;
     aAlwaysShowCmplMtx := chkAlwaysShowCmplMtx.Checked;
     aOnlySubMatrix := chkOnlySubMatrix.Checked;

     if (aShowIndexOffset <> fShowIndexOffset) or (aAlwaysShowCmplMtx <> fAlwaysShowCmplMtx) or 
        (aOnlySubMatrix <> fOnlySubMatrix) or (aMaxNumCols <> fMaxNumCols) or
        (aPrec <> fPrec) or (aMaxNumRows <> fMaxNumRows)
     then
     begin
          fMaxNumCols := aMaxNumCols;
          fMaxNumRows := aMaxNumRows;
          fPrec := aPrec;
          fShowIndexOffset := aShowIndexOffset;
          fAlwaysShowCmplMtx := aAlwaysShowCmplMtx;
          fOnlySubMatrix := aOnlySubMatrix;
     
          if Assigned(fOnChanged) then
             fOnChanged(self);
     
          // store settings
          reg := TRegIniFile.Create;
          try
             reg.RootKey := HKEY_CURRENT_USER;
             if not reg.OpenKey('Software\mrSoft', True) then
                exit;

             reg.WriteInteger('matrixIDEPlugin', 'MaxWidth', fMaxNumCols );
             reg.WriteInteger('matrixIDEPlugin', 'MaxHeight', fMaxNumRows );
             reg.WriteInteger('matrixIDEPlugin', 'NumDecimals', fPrec);
             reg.WriteBool('matrixIDEPlugin', 'ShowIndexOffset', fShowIndexOffset);
             reg.WriteBool('matrixIDEPlugin', 'ShowCompleteMtx', fAlwaysShowCmplMtx);

             reg.ReadBool('matrixIDEPlugin', 'OnlySubMatrix', fOnlySubMatrix);
          finally
                 reg.Free;
          end;
     end;
end;

procedure TfrmMtxSettings.FormDeactivate(Sender: TObject);
begin
     Close;
end;

procedure TfrmMtxSettings.FormPaint(Sender: TObject);
begin
     // just paint a nice rectangle
     Canvas.Brush.Style := bsClear;
     Canvas.Pen.Width := 1;
     Canvas.Pen.Color := clBlack;
     Canvas.Rectangle(0, 0, Width - 1, height - 1);
end;

procedure TfrmMtxSettings.FormCreate(Sender: TObject);
var reg : TRegIniFile;
begin
     // load settings
     reg := TRegIniFile.Create;
     try
        reg.RootKey := HKEY_CURRENT_USER;
        reg.OpenKeyReadOnly('Software\mrSoft');

        fMaxNumCols := reg.ReadInteger('matrixIDEPlugin', 'MaxWidth', 1000 );
        fMaxNumRows := reg.ReadInteger('matrixIDEPlugin', 'MaxHeight', 1000 );
        fPrec := reg.ReadInteger('matrixIDEPlugin', 'NumDecimals', 3);
        fShowIndexOffset := reg.ReadBool('matrixIDEPlugin', 'ShowIndexOffset', True);
        fAlwaysShowCmplMtx := reg.ReadBool('matrixIDEPlugin', 'ShowCompleteMtx', False);

        fOnlySubMatrix := reg.ReadBool('matrixIDEPlugin', 'OnlySubMatrix', False);
     finally
            reg.Free;
     end;

     chkAlwaysShowCmplMtx.Checked := fAlwaysShowCmplMtx;
     chkShowIndexOffset.Checked := fShowIndexOffset;
     chkOnlySubMatrix.Checked := fOnlySubMatrix;
end;

end.
