inherited frmEbSetSwitch: TfrmEbSetSwitch
  Caption = 'Set Switch'
  ClientHeight = 204
  ExplicitWidth = 355
  ExplicitHeight = 239
  PixelsPerInch = 120
  TextHeight = 16
  inherited Panel1: TPanel
    Height = 145
    ExplicitHeight = 145
    object GroupBox1: TGroupBox
      Left = 4
      Top = 4
      Width = 341
      Height = 86
      Caption = 'Which switch'
      TabOrder = 0
      object radSwitch: TRadioButton
        Left = 11
        Top = 28
        Width = 70
        Height = 17
        Caption = 'Switch:'
        TabOrder = 0
        OnClick = RadioButtonClick
      end
      object radInt: TRadioButton
        Left = 11
        Top = 61
        Width = 133
        Height = 17
        Caption = 'Variable reference:'
        TabOrder = 1
      end
      object selGlobalSwitch: TSwitchSelector
        Left = 147
        Top = 24
        Width = 188
        Height = 24
      end
      object selGlobalint: TIntSelector
        Left = 147
        Top = 57
        Width = 188
        Height = 24
      end
    end
    object grpSetTo: TRadioGroup
      Left = 4
      Top = 88
      Width = 335
      Height = 49
      Caption = 'Set to'
      Columns = 3
      Items.Strings = (
        'OFF'
        'ON'
        'FLIP')
      TabOrder = 1
    end
  end
  inherited btnOK: TButton
    Top = 161
  end
  inherited btnCancel: TButton
    Top = 161
  end
  inherited btnHelp: TButton
    Top = 161
  end
end
