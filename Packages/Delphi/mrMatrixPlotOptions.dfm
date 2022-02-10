object frmMtxPlotOptions: TfrmMtxPlotOptions
  Left = 0
  Top = 0
  Caption = 'Plot options'
  ClientHeight = 294
  ClientWidth = 353
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 16
  object lblCapCurXAxis: TLabel
    Left = 21
    Top = 32
    Width = 77
    Height = 16
    Caption = 'Axis X Values'
  end
  object lblCapCurYAxis: TLabel
    Left = 22
    Top = 78
    Width = 76
    Height = 16
    Caption = 'Axis Y Values'
  end
  object lblCurXAxis: TLabel
    Left = 117
    Top = 32
    Width = 220
    Height = 16
    AutoSize = False
    Caption = '---'
  end
  object lblCurYAxis: TLabel
    Left = 117
    Top = 78
    Width = 227
    Height = 16
    AutoSize = False
    Caption = '---'
  end
  object lblCapXAxis: TLabel
    Left = 82
    Top = 104
    Width = 190
    Height = 19
    Caption = 'New X Axis Dimensions'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblCapNewYAxis: TLabel
    Left = 82
    Top = 176
    Width = 190
    Height = 19
    Caption = 'New Y Axis Dimensions'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label1: TLabel
    Left = 175
    Top = 139
    Width = 5
    Height = 16
    Caption = '-'
  end
  object Label2: TLabel
    Left = 175
    Top = 211
    Width = 5
    Height = 16
    Caption = '-'
  end
  object lblCapDataXVals: TLabel
    Left = 22
    Top = 13
    Width = 80
    Height = 16
    Caption = 'Data X Values'
  end
  object lblDataX: TLabel
    Left = 118
    Top = 13
    Width = 220
    Height = 16
    AutoSize = False
    Caption = '---'
  end
  object lblCapDataYVals: TLabel
    Left = 21
    Top = 59
    Width = 79
    Height = 16
    Caption = 'Data Y Values'
  end
  object lblDataY: TLabel
    Left = 116
    Top = 59
    Width = 227
    Height = 16
    AutoSize = False
    Caption = '---'
  end
  object edMinXAxis: TEdit
    Left = 21
    Top = 136
    Width = 121
    Height = 24
    TabOrder = 0
    Text = '-1'
  end
  object edMaxXAxis: TEdit
    Left = 216
    Top = 136
    Width = 121
    Height = 24
    TabOrder = 1
    Text = '1'
  end
  object edMinYAxis: TEdit
    Left = 21
    Top = 208
    Width = 121
    Height = 24
    TabOrder = 2
    Text = '-1'
  end
  object edMaxYAxis: TEdit
    Left = 216
    Top = 208
    Width = 121
    Height = 24
    TabOrder = 3
    Text = '1'
  end
  object btnOk: TButton
    Left = 22
    Top = 261
    Width = 75
    Height = 25
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 4
  end
  object btnCancel: TButton
    Left = 262
    Top = 261
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
end
