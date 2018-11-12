object frmMtxSettings: TfrmMtxSettings
  Left = 0
  Top = 0
  BorderStyle = bsNone
  Caption = 'frmMtxSettings'
  ClientHeight = 152
  ClientWidth = 200
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDeactivate = FormDeactivate
  OnPaint = FormPaint
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 81
    Height = 13
    Caption = 'Def. shown rows'
  end
  object Label2: TLabel
    Left = 8
    Top = 32
    Width = 96
    Height = 13
    Caption = 'Def. shown columns'
  end
  object Label3: TLabel
    Left = 8
    Top = 56
    Width = 42
    Height = 13
    Caption = 'Precision'
  end
  object chkAlwaysShowCmplMtx: TCheckBox
    Left = 8
    Top = 80
    Width = 161
    Height = 17
    Caption = 'Always show complete matrix'
    TabOrder = 0
  end
  object chkShowIndexOffset: TCheckBox
    Left = 8
    Top = 103
    Width = 161
    Height = 17
    Caption = 'Show index offset'
    TabOrder = 1
  end
  object chkOnlySubMatrix: TCheckBox
    Left = 8
    Top = 126
    Width = 177
    Height = 17
    Caption = 'Default show only submatrix'
    TabOrder = 2
  end
  object edDefMaxRows: TEdit
    Left = 131
    Top = 5
    Width = 54
    Height = 21
    TabOrder = 3
    Text = '1000'
    OnExit = edDefMaxRowsExit
  end
  object edDefMaxColumns: TEdit
    Left = 131
    Top = 29
    Width = 54
    Height = 21
    TabOrder = 4
    Text = '1000'
    OnExit = edDefMaxColumnsExit
  end
  object edPrecision: TEdit
    Left = 131
    Top = 53
    Width = 54
    Height = 21
    TabOrder = 5
    Text = '3'
    OnExit = edPrecisionExit
  end
end
