inherited frmEBEditTintScreen: TfrmEBEditTintScreen
  Caption = 'Tint Screen'
  ClientHeight = 368
  ClientWidth = 461
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 16
  inherited Panel1: TPanel
    Width = 461
    Height = 315
    object GroupBox1: TGroupBox
      Left = 10
      Top = 10
      Width = 446
      Height = 231
      Align = alTop
      Caption = 'Color'
      TabOrder = 0
      object sldRed: TJvxSlider
        Left = 96
        Top = 20
        Width = 274
        Height = 40
        MaxValue = 200
        TabOrder = 0
        Value = 100
        OnChange = sldRedChange
      end
      object spnRed: TJvSpinEdit
        Left = 376
        Top = 26
        Width = 67
        Height = 24
        ButtonKind = bkClassic
        MaxValue = 200.000000000000000000
        Value = 100.000000000000000000
        TabOrder = 1
        OnChange = spnRedChange
      end
      object sldSat: TJvxSlider
        Left = 96
        Top = 156
        Width = 274
        Height = 40
        MaxValue = 200
        TabOrder = 2
        Value = 100
        OnChange = sldSatChange
      end
      object spnSat: TJvSpinEdit
        Left = 376
        Top = 162
        Width = 67
        Height = 24
        ButtonKind = bkClassic
        MaxValue = 200.000000000000000000
        Value = 100.000000000000000000
        TabOrder = 3
        OnChange = spnSatChange
      end
      object spnBlue: TJvSpinEdit
        Left = 376
        Top = 116
        Width = 67
        Height = 24
        ButtonKind = bkClassic
        MaxValue = 200.000000000000000000
        Value = 100.000000000000000000
        TabOrder = 4
        OnChange = spnBlueChange
      end
      object sldBlue: TJvxSlider
        Left = 96
        Top = 110
        Width = 274
        Height = 40
        MaxValue = 200
        TabOrder = 5
        Value = 100
        OnChange = sldBlueChange
      end
      object sldGreen: TJvxSlider
        Left = 96
        Top = 64
        Width = 274
        Height = 40
        MaxValue = 200
        TabOrder = 6
        Value = 100
        OnChange = sldGreenChange
      end
      object spnGreen: TJvSpinEdit
        Left = 376
        Top = 70
        Width = 67
        Height = 24
        ButtonKind = bkClassic
        MaxValue = 200.000000000000000000
        Value = 100.000000000000000000
        TabOrder = 7
        OnChange = spnGreenChange
      end
      object imgReference: TSdlFrame
        Left = 96
        Top = 196
        Width = 347
        Height = 26
        Framerate = 0
        Active = False
        LogicalWidth = 347
        LogicalHeight = 26
        OnAvailable = imgReferenceAvailable
      end
    end
    object StaticText1: TStaticText
      Left = 24
      Top = 36
      Width = 31
      Height = 20
      Caption = 'Red:'
      TabOrder = 1
    end
    object StaticText2: TStaticText
      Left = 24
      Top = 82
      Width = 43
      Height = 20
      Caption = 'Green:'
      TabOrder = 2
    end
    object StaticText3: TStaticText
      Left = 24
      Top = 128
      Width = 33
      Height = 20
      Caption = 'Blue:'
      TabOrder = 3
    end
    object StaticText4: TStaticText
      Left = 24
      Top = 174
      Width = 68
      Height = 20
      Caption = 'Saturation:'
      TabOrder = 4
    end
    object StaticText5: TStaticText
      Left = 24
      Top = 208
      Width = 67
      Height = 20
      Caption = 'Reference:'
      TabOrder = 5
    end
    object GroupBox2: TGroupBox
      Left = 10
      Top = 247
      Width = 215
      Height = 58
      Caption = 'Transition Time'
      TabOrder = 6
      object spnLength: TJvSpinEdit
        Left = 14
        Top = 20
        Width = 67
        Height = 24
        ButtonKind = bkClassic
        MaxValue = 1000.000000000000000000
        TabOrder = 0
      end
      object StaticText6: TStaticText
        Left = 96
        Top = 22
        Width = 109
        Height = 20
        Caption = 'tenths of a second'
        TabOrder = 1
      end
    end
    object GroupBox3: TGroupBox
      Left = 240
      Top = 247
      Width = 213
      Height = 58
      Caption = 'Wait'
      TabOrder = 7
      object chkWait: TCheckBox
        Left = 16
        Top = 24
        Width = 137
        Height = 17
        Caption = 'Wait until finished'
        TabOrder = 0
      end
    end
  end
  inherited btnOK: TButton
    Left = 178
    Top = 325
  end
  inherited btnCancel: TButton
    Left = 272
    Top = 325
  end
  inherited btnHelp: TButton
    Left = 365
    Top = 325
  end
end
