inherited frmEBDeleteObject: TfrmEBDeleteObject
  Caption = 'Delete Map Object'
  ClientHeight = 136
  ExplicitWidth = 355
  ExplicitHeight = 171
  PixelsPerInch = 120
  TextHeight = 16
  inherited Panel1: TPanel
    Height = 83
    Padding.Left = 10
    Padding.Top = 5
    Padding.Right = 10
    Padding.Bottom = 5
    object radDuration: TRadioGroup
      Left = 10
      Top = 5
      Width = 329
      Height = 73
      Align = alClient
      Caption = 'Duration'
      ItemIndex = 0
      Items.Strings = (
        'Until map is reloaded'
        'Permanant')
      TabOrder = 0
      ExplicitLeft = 80
      ExplicitTop = 24
      ExplicitWidth = 185
      ExplicitHeight = 105
    end
  end
  inherited btnOK: TButton
    Top = 93
  end
  inherited btnCancel: TButton
    Top = 93
  end
  inherited btnHelp: TButton
    Top = 93
  end
end
