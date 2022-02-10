object frmMtxPlot: TfrmMtxPlot
  Left = 0
  Top = 0
  Caption = 'Plot'
  ClientHeight = 336
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pbPlot: TPaintBox
    Left = 0
    Top = 0
    Width = 635
    Height = 336
    Align = alClient
    OnDblClick = pbPlotDblClick
    OnPaint = pbPlotPaint
  end
end
