object frmSplineTest: TfrmSplineTest
  Left = 0
  Top = 0
  Caption = 'Robust Spline Test'
  ClientHeight = 441
  ClientWidth = 802
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    802
    441)
  PixelsPerInch = 96
  TextHeight = 13
  object pbSpline: TPaintBox
    Left = 217
    Top = 0
    Width = 585
    Height = 441
    Align = alClient
    OnPaint = pbSplinePaint
  end
  object lblLegend: TLabel
    Left = 512
    Top = 24
    Width = 282
    Height = 113
    Anchors = [akTop, akRight]
    AutoSize = False
    Caption = 'lblLegend'
  end
  object pnlSettings: TPanel
    Left = 0
    Top = 0
    Width = 217
    Height = 441
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    object radSplineOrderTest: TRadioButton
      Left = 32
      Top = 63
      Width = 113
      Height = 17
      Caption = 'Spline Orders'
      TabOrder = 0
      OnClick = radBaseTestClick
    end
    object radBaseTest: TRadioButton
      Left = 32
      Top = 40
      Width = 113
      Height = 17
      Caption = 'Base Test'
      Checked = True
      TabOrder = 1
      TabStop = True
      OnClick = radBaseTestClick
    end
    object radPeriodicBoundTest: TRadioButton
      Left = 32
      Top = 86
      Width = 161
      Height = 17
      Caption = 'Periodic boundary conditions'
      TabOrder = 2
      OnClick = radBaseTestClick
    end
    object radRobustTest: TRadioButton
      Left = 32
      Top = 109
      Width = 161
      Height = 17
      Caption = 'Robust fitting'
      TabOrder = 3
      OnClick = radBaseTestClick
    end
    object radSplineIntegralTest: TRadioButton
      Left = 32
      Top = 132
      Width = 161
      Height = 17
      Caption = 'Spline Integral'
      TabOrder = 4
      OnClick = radBaseTestClick
    end
    object pcSettings: TPageControl
      Left = 0
      Top = 168
      Width = 217
      Height = 273
      ActivePage = tbBaseTest
      Align = alBottom
      TabOrder = 5
      object tbBaseTest: TTabSheet
        Caption = 'Base Test'
        object Label1: TLabel
          Left = 16
          Top = 22
          Width = 85
          Height = 13
          Caption = 'Number of breaks'
        end
        object Label2: TLabel
          Left = 16
          Top = 62
          Width = 54
          Height = 13
          Caption = 'Noise Level'
        end
        object ed1NumBreaks: TEdit
          Left = 116
          Top = 19
          Width = 57
          Height = 21
          TabOrder = 0
          Text = '10'
          OnExit = ed1NumBreaksExit
        end
        object ud1NumBreaks: TUpDown
          Left = 173
          Top = 19
          Width = 16
          Height = 21
          Associate = ed1NumBreaks
          Min = 5
          Max = 35
          Position = 10
          TabOrder = 1
          OnClick = ud1NumBreaksClick
        end
        object ed1Noise: TEdit
          Left = 116
          Top = 59
          Width = 57
          Height = 21
          TabOrder = 2
          Text = '20'
          OnExit = ed1NumBreaksExit
        end
        object ud1Noise: TUpDown
          Left = 173
          Top = 59
          Width = 16
          Height = 21
          Associate = ed1Noise
          Position = 20
          TabOrder = 3
          OnClick = ud1NumBreaksClick
        end
      end
      object tbSplineOrders: TTabSheet
        Caption = 'Spline Orders'
        ImageIndex = 1
        object Label3: TLabel
          Left = 16
          Top = 31
          Width = 54
          Height = 13
          Caption = 'Noise Level'
        end
        object Label4: TLabel
          Left = 16
          Top = 69
          Width = 85
          Height = 13
          Caption = 'Number of breaks'
        end
        object ed2Noise: TEdit
          Left = 116
          Top = 28
          Width = 57
          Height = 21
          TabOrder = 0
          Text = '20'
          OnExit = ed1NumBreaksExit
        end
        object ud2Noise: TUpDown
          Left = 173
          Top = 28
          Width = 16
          Height = 21
          Associate = ed2Noise
          Position = 20
          TabOrder = 1
          OnClick = ud1NumBreaksClick
        end
        object ed2NumBreaks: TEdit
          Left = 116
          Top = 68
          Width = 57
          Height = 21
          TabOrder = 2
          Text = '8'
          OnExit = ed1NumBreaksExit
        end
        object ud2Breaks: TUpDown
          Left = 173
          Top = 68
          Width = 16
          Height = 21
          Associate = ed2NumBreaks
          Min = 5
          Max = 10
          Position = 8
          TabOrder = 3
          OnClick = ud1NumBreaksClick
        end
      end
      object tbPeriodicBoundary: TTabSheet
        Caption = 'Periodic boundary'
        ImageIndex = 2
        object Label5: TLabel
          Left = 16
          Top = 31
          Width = 54
          Height = 13
          Caption = 'Noise Level'
        end
        object ed3Noise: TEdit
          Left = 116
          Top = 28
          Width = 57
          Height = 21
          TabOrder = 0
          Text = '20'
          OnExit = ed1NumBreaksExit
        end
        object ud3Noise: TUpDown
          Left = 173
          Top = 28
          Width = 16
          Height = 21
          Associate = ed3Noise
          Position = 20
          TabOrder = 1
          OnClick = ud1NumBreaksClick
        end
      end
      object tbRobustTest: TTabSheet
        Caption = 'Robust Test'
        ImageIndex = 3
        object Label6: TLabel
          Left = 16
          Top = 31
          Width = 54
          Height = 13
          Caption = 'Noise Level'
        end
        object Label7: TLabel
          Left = 16
          Top = 69
          Width = 85
          Height = 13
          Caption = 'Number of breaks'
        end
        object Label8: TLabel
          Left = 16
          Top = 109
          Width = 74
          Height = 13
          Caption = 'Robust Fit Beta'
        end
        object Label9: TLabel
          Left = 162
          Top = 109
          Width = 25
          Height = 13
          Caption = '/ 100'
        end
        object Edit1: TEdit
          Left = 116
          Top = 28
          Width = 57
          Height = 21
          TabOrder = 0
          Text = '5'
          OnExit = ed1NumBreaksExit
        end
        object ud4Noise: TUpDown
          Left = 173
          Top = 28
          Width = 16
          Height = 21
          Associate = Edit1
          Max = 20
          Position = 5
          TabOrder = 1
          OnClick = ud1NumBreaksClick
        end
        object ed4NumBreaks: TEdit
          Left = 116
          Top = 68
          Width = 57
          Height = 21
          TabOrder = 2
          Text = '10'
          OnExit = ed1NumBreaksExit
        end
        object ud4NumBreaks: TUpDown
          Left = 173
          Top = 68
          Width = 16
          Height = 21
          Associate = ed4NumBreaks
          Min = 5
          Max = 15
          Position = 10
          TabOrder = 3
          OnClick = ud1NumBreaksClick
        end
        object ed4Beta: TEdit
          Left = 116
          Top = 106
          Width = 25
          Height = 21
          TabOrder = 4
          Text = '75'
          OnExit = ed1NumBreaksExit
        end
        object ud4Beta: TUpDown
          Left = 141
          Top = 106
          Width = 16
          Height = 21
          Associate = ed4Beta
          Max = 95
          Position = 75
          TabOrder = 5
          OnClick = ud1NumBreaksClick
        end
      end
      object tbSplineInt: TTabSheet
        Caption = 'Spline Integral'
        ImageIndex = 4
      end
    end
  end
end
