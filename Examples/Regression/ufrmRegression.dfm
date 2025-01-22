object frmRegression: TfrmRegression
  Left = 0
  Top = 0
  Caption = 'Regression Test'
  ClientHeight = 443
  ClientWidth = 777
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pbdata: TPaintBox
    Left = 185
    Top = 0
    Width = 592
    Height = 443
    Align = alClient
    OnPaint = pbdataPaint
    ExplicitLeft = 344
    ExplicitTop = 192
    ExplicitWidth = 105
    ExplicitHeight = 105
  end
  object pnlOptions: TPanel
    Left = 0
    Top = 0
    Width = 185
    Height = 443
    Align = alLeft
    TabOrder = 0
    object lblCapNoise: TLabel
      Left = 16
      Top = 164
      Width = 26
      Height = 13
      Caption = 'Noise'
    end
    object rdLinearPinv: TRadioButton
      Left = 16
      Top = 28
      Width = 150
      Height = 18
      Caption = 'Linear Regression'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = rdLinearPinvClick
    end
    object rdQuadraticRegr: TRadioButton
      Left = 16
      Top = 51
      Width = 150
      Height = 17
      Caption = 'Quadratic Regression'
      TabOrder = 1
      OnClick = rdLinearPinvClick
    end
    object rdCubicRegr: TRadioButton
      Left = 16
      Top = 74
      Width = 150
      Height = 17
      Caption = 'Cubic Regression'
      TabOrder = 2
      OnClick = rdLinearPinvClick
    end
    object rdArctanOpt: TRadioButton
      Left = 16
      Top = 97
      Width = 150
      Height = 17
      Caption = 'ArcTan Optimization'
      TabOrder = 3
      OnClick = rdLinearPinvClick
    end
    object tbNoise: TTrackBar
      Left = 16
      Top = 188
      Width = 137
      Height = 45
      Max = 100
      TabOrder = 4
      OnChange = tbNoiseChange
    end
  end
end
