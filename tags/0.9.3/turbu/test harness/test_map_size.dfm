object frmTestMapSize: TfrmTestMapSize
  Left = 0
  Top = 0
  Caption = 'New map size'
  ClientHeight = 181
  ClientWidth = 284
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 120
  TextHeight = 16
  object Label1: TLabel
    Left = 16
    Top = 24
    Width = 13
    Height = 16
    Caption = 'X:'
  end
  object Label2: TLabel
    Left = 96
    Top = 24
    Width = 12
    Height = 16
    Caption = 'Y:'
  end
  object Label3: TLabel
    Left = 208
    Top = 24
    Width = 36
    Height = 16
    Caption = 'Mode:'
  end
  object spnX: TSpinEdit
    Left = 8
    Top = 56
    Width = 73
    Height = 26
    MaxValue = 300
    MinValue = 1
    TabOrder = 0
    Value = 1
  end
  object spnY: TSpinEdit
    Left = 96
    Top = 56
    Width = 73
    Height = 26
    MaxValue = 300
    MinValue = 1
    TabOrder = 1
    Value = 1
  end
  object spnMode: TSpinEdit
    Left = 208
    Top = 56
    Width = 73
    Height = 26
    MaxValue = 9
    MinValue = 1
    TabOrder = 2
    Value = 1
  end
  object btnOK: TButton
    Left = 94
    Top = 112
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
end
