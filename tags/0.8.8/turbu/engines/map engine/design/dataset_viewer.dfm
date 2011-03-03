object frmDatasetViewer: TfrmDatasetViewer
  Left = 0
  Top = 0
  Caption = 'frmDatasetViewer'
  ClientHeight = 483
  ClientWidth = 633
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 120
  TextHeight = 16
  object DBGrid1: TDBGrid
    Left = 0
    Top = 0
    Width = 633
    Height = 145
    Align = alTop
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -13
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object DBGrid2: TDBGrid
    Left = 0
    Top = 315
    Width = 633
    Height = 168
    Align = alBottom
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -13
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object DBNavigator1: TDBNavigator
    Left = 208
    Top = 144
    Width = 240
    Height = 18
    TabOrder = 2
  end
  object DBMemo1: TDBMemo
    Left = 0
    Top = 176
    Width = 633
    Height = 133
    ScrollBars = ssVertical
    TabOrder = 3
  end
end
