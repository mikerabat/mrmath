object frmEMTest: TfrmEMTest
  Left = 0
  Top = 0
  Caption = '2D Expectation Maximization Test Application'
  ClientHeight = 479
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object pbEM: TPaintBox
    Left = 185
    Top = 0
    Width = 439
    Height = 479
    Align = alClient
    OnPaint = pbEMPaint
  end
  object pnlSettings: TPanel
    Left = 0
    Top = 0
    Width = 185
    Height = 479
    Align = alLeft
    TabOrder = 0
    object Label4: TLabel
      Left = 9
      Top = 237
      Width = 102
      Height = 13
      Caption = 'Number of iterations:'
    end
    object lblIter: TLabel
      Left = 117
      Top = 237
      Width = 6
      Height = 13
      Caption = '0'
    end
    object grpCenters: TGroupBox
      Left = 0
      Top = 25
      Width = 169
      Height = 198
      Caption = 'Centers'
      TabOrder = 0
      object lblCapVarC1: TLabel
        Left = 13
        Top = 81
        Width = 32
        Height = 13
        Caption = 'Var C1'
      end
      object Label1: TLabel
        Left = 13
        Top = 109
        Width = 32
        Height = 13
        Caption = 'Var C1'
      end
      object Label2: TLabel
        Left = 13
        Top = 135
        Width = 32
        Height = 13
        Caption = 'Var C1'
      end
      object Label3: TLabel
        Left = 13
        Top = 161
        Width = 32
        Height = 13
        Caption = 'Var C1'
      end
      object rad2C: TRadioButton
        Left = 18
        Top = 22
        Width = 113
        Height = 17
        Caption = '2 Centers'
        Checked = True
        TabOrder = 0
        TabStop = True
        OnClick = rad2CClick
      end
      object rad3C: TRadioButton
        Left = 18
        Top = 38
        Width = 113
        Height = 17
        Caption = '3 Centers'
        TabOrder = 1
        OnClick = rad2CClick
      end
      object rad4C: TRadioButton
        Left = 18
        Top = 54
        Width = 113
        Height = 17
        Caption = '4 Centers'
        TabOrder = 2
        OnClick = rad2CClick
      end
      object scrVarC1: TScrollBar
        Left = 59
        Top = 80
        Width = 97
        Height = 17
        PageSize = 0
        Position = 50
        TabOrder = 3
        OnChange = scrVarC1Change
      end
      object scrVarC2: TScrollBar
        Left = 59
        Top = 107
        Width = 97
        Height = 17
        PageSize = 0
        Position = 50
        TabOrder = 4
        OnChange = scrVarC1Change
      end
      object scrVarC3: TScrollBar
        Left = 59
        Top = 133
        Width = 97
        Height = 17
        PageSize = 0
        Position = 50
        TabOrder = 5
        OnChange = scrVarC1Change
      end
      object scrVarC4: TScrollBar
        Left = 59
        Top = 159
        Width = 97
        Height = 17
        PageSize = 0
        Position = 50
        TabOrder = 6
        OnChange = scrVarC1Change
      end
    end
    object Button1: TButton
      Left = 13
      Top = 272
      Width = 75
      Height = 25
      Caption = 'Animate'
      TabOrder = 1
      OnClick = Button1Click
    end
  end
  object timAnimate: TTimer
    Enabled = False
    Interval = 750
    OnTimer = timAnimateTimer
    Left = 16
    Top = 320
  end
end
