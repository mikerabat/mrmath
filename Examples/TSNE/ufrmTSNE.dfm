object frmTSNE: TfrmTSNE
  Left = 0
  Top = 0
  Caption = 'TSNE Example'
  ClientHeight = 377
  ClientWidth = 527
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 16
  object pbScatter: TPaintBox
    Left = 209
    Top = 0
    Width = 318
    Height = 377
    Align = alClient
    OnPaint = pbScatterPaint
  end
  object pnlOptions: TPanel
    Left = 0
    Top = 0
    Width = 209
    Height = 377
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 16
      Width = 55
      Height = 16
      Caption = 'Perplexity'
    end
    object Label2: TLabel
      Left = 9
      Top = 84
      Width = 54
      Height = 16
      Caption = 'Iterations'
    end
    object lblProgress: TLabel
      Left = 9
      Top = 173
      Width = 15
      Height = 16
      Caption = '---'
    end
    object lblPerplexity: TLabel
      Left = 9
      Top = 42
      Width = 15
      Height = 16
      Caption = '---'
    end
    object lblIterations: TLabel
      Left = 9
      Top = 114
      Width = 15
      Height = 16
      Caption = '---'
    end
    object Label3: TLabel
      Left = 110
      Top = 346
      Width = 33
      Height = 16
      Hint = 'Values from 0 to 1. 0 means an exact calculation of tSNE'
      Caption = 'Theta'
    end
    object btnCalc: TButton
      Left = 8
      Top = 343
      Width = 75
      Height = 25
      Caption = 'Calc'
      TabOrder = 0
      OnClick = btnCalcClick
    end
    object btnCreateDataSet: TButton
      Left = 8
      Top = 312
      Width = 105
      Height = 25
      Caption = 'Create Dataset'
      TabOrder = 1
      OnClick = btnCreateDataSetClick
    end
    object tbPerplexity: TTrackBar
      Left = 48
      Top = 40
      Width = 129
      Height = 33
      Max = 50
      Min = 5
      PageSize = 1
      Position = 10
      TabOrder = 2
      TickStyle = tsNone
      OnChange = tbPerplexityChange
    end
    object tbIterations: TTrackBar
      Left = 48
      Top = 112
      Width = 129
      Height = 33
      Max = 6000
      Min = 5
      PageSize = 100
      Position = 2000
      TabOrder = 3
      TickStyle = tsNone
      OnChange = tbIterationsChange
    end
    object pbProgress: TProgressBar
      Left = 9
      Top = 208
      Width = 194
      Height = 17
      TabOrder = 4
    end
    object radGauss: TRadioButton
      Left = 9
      Top = 240
      Width = 136
      Height = 17
      Caption = 'Gauss distribution'
      Checked = True
      TabOrder = 5
      TabStop = True
    end
    object radCircle: TRadioButton
      Left = 9
      Top = 263
      Width = 113
      Height = 17
      Caption = 'Circles'
      TabOrder = 6
    end
    object edTheta: TEdit
      Left = 154
      Top = 343
      Width = 39
      Height = 24
      TabOrder = 7
      Text = '0.5'
    end
    object radSimple: TRadioButton
      Left = 9
      Top = 286
      Width = 113
      Height = 17
      Caption = 'Simple'
      TabOrder = 8
    end
  end
end
