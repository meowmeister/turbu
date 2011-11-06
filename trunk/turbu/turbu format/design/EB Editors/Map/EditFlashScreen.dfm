inherited frmEBEditFlashScreen: TfrmEBEditFlashScreen
  Caption = 'Flash Screen'
  ClientHeight = 369
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 16
  inherited Panel1: TPanel
    Height = 316
    object radFlashMode: TRadioGroup
      Left = 10
      Top = 263
      Width = 334
      Height = 48
      Align = alBottom
      Caption = 'Flash Mode'
      Columns = 3
      ItemIndex = 0
      Items.Strings = (
        'Once'
        'Once (Wait)'
        'Continually')
      TabOrder = 0
    end
    object GroupBox1: TGroupBox
      Left = 10
      Top = 10
      Width = 334
      Height = 183
      Align = alTop
      Caption = 'Color'
      TabOrder = 1
      object sldRed: TJvxSlider
        Left = 64
        Top = 20
        Width = 197
        Height = 40
        Increment = 1
        MaxValue = 31
        TabOrder = 0
        Value = 31
        OnChange = sldRedChange
      end
      object spnRed: TJvSpinEdit
        Left = 267
        Top = 26
        Width = 67
        Height = 24
        ButtonKind = bkClassic
        MaxValue = 200.000000000000000000
        Value = 31.000000000000000000
        TabOrder = 1
        OnChange = spnRedChange
      end
      object StaticText1: TStaticText
        Left = 10
        Top = 28
        Width = 31
        Height = 20
        Caption = 'Red:'
        TabOrder = 2
      end
      object StaticText2: TStaticText
        Left = 10
        Top = 68
        Width = 43
        Height = 20
        Caption = 'Green:'
        TabOrder = 3
      end
      object sldGreen: TJvxSlider
        Left = 64
        Top = 60
        Width = 197
        Height = 40
        Increment = 1
        MaxValue = 31
        TabOrder = 4
        Value = 31
        OnChange = sldGreenChange
      end
      object spnGreen: TJvSpinEdit
        Left = 267
        Top = 66
        Width = 67
        Height = 24
        ButtonKind = bkClassic
        MaxValue = 200.000000000000000000
        Value = 31.000000000000000000
        TabOrder = 5
        OnChange = spnGreenChange
      end
      object StaticText3: TStaticText
        Left = 10
        Top = 108
        Width = 33
        Height = 20
        Caption = 'Blue:'
        TabOrder = 6
      end
      object sldBlue: TJvxSlider
        Left = 64
        Top = 96
        Width = 197
        Height = 40
        Increment = 1
        MaxValue = 31
        TabOrder = 7
        Value = 31
        OnChange = sldBlueChange
      end
      object spnBlue: TJvSpinEdit
        Left = 267
        Top = 106
        Width = 67
        Height = 24
        ButtonKind = bkClassic
        MaxValue = 200.000000000000000000
        Value = 31.000000000000000000
        TabOrder = 8
        OnChange = spnBlueChange
      end
      object StaticText4: TStaticText
        Left = 10
        Top = 148
        Width = 58
        Height = 20
        Caption = 'Strength:'
        TabOrder = 9
      end
      object sldAlpha: TJvxSlider
        Left = 64
        Top = 136
        Width = 197
        Height = 40
        Increment = 1
        MaxValue = 31
        TabOrder = 10
        Value = 31
        OnChange = sldAlphaChange
      end
      object spnAlpha: TJvSpinEdit
        Left = 267
        Top = 146
        Width = 67
        Height = 24
        ButtonKind = bkClassic
        MaxValue = 200.000000000000000000
        Value = 31.000000000000000000
        TabOrder = 11
        OnChange = spnAlphaChange
      end
    end
    object GroupBox2: TGroupBox
      Left = 10
      Top = 193
      Width = 79
      Height = 64
      Caption = 'Preview'
      TabOrder = 2
      object pnlPreview: TPanel
        Left = 2
        Top = 18
        Width = 75
        Height = 44
        Align = alClient
        BevelOuter = bvLowered
        ParentBackground = False
        TabOrder = 0
      end
    end
    object GroupBox3: TGroupBox
      Left = 95
      Top = 193
      Width = 246
      Height = 64
      Caption = 'Fade Duration'
      TabOrder = 3
      object spnLength: TJvSpinEdit
        Left = 14
        Top = 25
        Width = 67
        Height = 24
        ButtonKind = bkClassic
        MaxValue = 1000.000000000000000000
        TabOrder = 0
      end
      object StaticText6: TStaticText
        Left = 96
        Top = 27
        Width = 109
        Height = 20
        Caption = 'tenths of a second'
        TabOrder = 1
      end
    end
  end
  inherited btnOK: TButton
    Top = 326
  end
  inherited btnCancel: TButton
    Top = 326
  end
  inherited btnHelp: TButton
    Top = 326
  end
end
