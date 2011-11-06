inherited frmEBEditShakeScreen: TfrmEBEditShakeScreen
  Caption = 'Shake Screen'
  ClientWidth = 380
  PixelsPerInch = 120
  TextHeight = 16
  inherited Panel1: TPanel
    Width = 380
    object radShakeMode: TRadioGroup
      Left = 10
      Top = 152
      Width = 365
      Height = 48
      Align = alBottom
      Caption = 'Shake Mode'
      Columns = 3
      ItemIndex = 0
      Items.Strings = (
        'Timed'
        'Timed (Wait)'
        'Continually')
      TabOrder = 0
      OnClick = RadioButtonClick
    end
    object GroupBox1: TGroupBox
      Left = 10
      Top = 10
      Width = 175
      Height = 79
      Caption = 'Shake Strength'
      TabOrder = 1
      object sldStrength: TJvxSlider
        Left = 3
        Top = 20
        Width = 169
        Height = 40
        Increment = 1
        MinValue = 1
        MaxValue = 9
        TabOrder = 0
        Value = 5
      end
      object StaticText1: TStaticText
        Left = 3
        Top = 56
        Width = 36
        Height = 20
        Caption = 'Weak'
        TabOrder = 1
      end
      object StaticText2: TStaticText
        Left = 63
        Top = 56
        Width = 49
        Height = 20
        Caption = 'Medium'
        TabOrder = 2
      end
      object StaticText3: TStaticText
        Left = 130
        Top = 56
        Width = 42
        Height = 20
        Caption = 'Strong'
        TabOrder = 3
      end
    end
    object GroupBox2: TGroupBox
      Left = 191
      Top = 10
      Width = 181
      Height = 76
      Caption = 'Shake Speed'
      TabOrder = 2
      object sldSpeed: TJvxSlider
        Left = 3
        Top = 20
        Width = 169
        Height = 40
        Increment = 1
        MinValue = 1
        MaxValue = 9
        TabOrder = 0
        Value = 5
      end
      object StaticText4: TStaticText
        Left = 2
        Top = 56
        Width = 32
        Height = 20
        Caption = 'Slow'
        TabOrder = 1
      end
      object StaticText5: TStaticText
        Left = 62
        Top = 56
        Width = 49
        Height = 20
        Caption = 'Medium'
        TabOrder = 2
      end
      object StaticText6: TStaticText
        Left = 143
        Top = 56
        Width = 28
        Height = 20
        Caption = 'Fast'
        TabOrder = 3
      end
    end
    object grpShakeDuration: TGroupBox
      Left = 10
      Top = 95
      Width = 362
      Height = 51
      Caption = 'Shake Duration'
      TabOrder = 3
      object spnLength: TJvSpinEdit
        Left = 6
        Top = 18
        Width = 67
        Height = 24
        ButtonKind = bkClassic
        MaxValue = 1000.000000000000000000
        TabOrder = 0
      end
      object StaticText7: TStaticText
        Left = 88
        Top = 20
        Width = 109
        Height = 20
        Caption = 'tenths of a second'
        TabOrder = 1
      end
    end
  end
  inherited btnOK: TButton
    Left = 97
  end
  inherited btnCancel: TButton
    Left = 191
  end
  inherited btnHelp: TButton
    Left = 284
  end
end
