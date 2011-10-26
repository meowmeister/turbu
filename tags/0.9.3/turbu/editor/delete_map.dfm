object dlgDeleteMap: TdlgDeleteMap
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Delete Map?'
  ClientHeight = 261
  ClientWidth = 295
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 120
  TextHeight = 16
  object lblDeleteMap: TLabel
    Left = 26
    Top = 15
    Width = 90
    Height = 16
    Caption = 'Delete map and'
  end
  object RadioGroup1: TRadioGroup
    Left = 26
    Top = 48
    Width = 247
    Height = 105
    ItemIndex = 1
    Items.Strings = (
      'delete all child maps'
      'move child maps to this map'#39's level'
      'move child maps to top level')
    TabOrder = 0
  end
  object Button1: TButton
    Left = 58
    Top = 208
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object Button2: TButton
    Left = 163
    Top = 208
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
