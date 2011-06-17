inherited frmEBEditTranslucency: TfrmEBEditTranslucency
  Caption = 'Change Party Translucency'
  ClientHeight = 94
  ExplicitWidth = 355
  ExplicitHeight = 129
  PixelsPerInch = 120
  TextHeight = 16
  inherited Panel1: TPanel
    Height = 41
  end
  inherited btnOK: TButton
    Top = 51
  end
  inherited btnCancel: TButton
    Top = 51
  end
  inherited btnHelp: TButton
    Top = 51
  end
  object StaticText1: TStaticText
    Left = 8
    Top = 11
    Width = 151
    Height = 20
    Caption = 'Set party translucency to:'
    TabOrder = 4
  end
  object spnTranslucency: TJvSpinEdit
    Left = 192
    Top = 8
    Width = 149
    Height = 24
    ButtonKind = bkClassic
    MaxValue = 100.000000000000000000
    Value = 100.000000000000000000
    TabOrder = 5
  end
end
