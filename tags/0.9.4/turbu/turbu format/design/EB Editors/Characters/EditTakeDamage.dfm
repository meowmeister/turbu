inherited frmEBEditTakeDamage: TfrmEBEditTakeDamage
  Caption = 'Take Damage'
  ClientHeight = 371
  PixelsPerInch = 120
  TextHeight = 16
  inherited Panel1: TPanel
    Height = 318
    object grpCharacter: TGroupBox
      Left = 8
      Top = 9
      Width = 333
      Height = 128
      Caption = 'Character'
      TabOrder = 0
      object cboHeroID: TIDLookupCombo
        Left = 104
        Top = 52
        Width = 215
        Height = 24
        KeyField = 'id'
        ListField = 'name'
        TabOrder = 0
      end
      object radSpecificHero: TRadioButton
        Left = 7
        Top = 55
        Width = 72
        Height = 17
        Caption = 'Specific:'
        TabOrder = 1
        OnClick = RadioButtonClick
      end
      object radHeroPtr: TRadioButton
        Left = 7
        Top = 90
        Width = 91
        Height = 17
        Caption = 'ID Value of:'
        TabOrder = 2
        OnClick = RadioButtonClick
      end
      object selHeroID: TIntSelector
        Left = 104
        Top = 86
        Width = 215
        Height = 24
      end
      object radAllParty: TRadioButton
        Left = 7
        Top = 24
        Width = 128
        Height = 17
        Caption = 'All Party Members'
        Checked = True
        TabOrder = 4
        TabStop = True
        OnClick = RadioButtonClick
      end
    end
    object GroupBox1: TGroupBox
      Left = 8
      Top = 143
      Width = 333
      Height = 58
      Caption = 'Attack'
      TabOrder = 1
      object spnPower: TJvSpinEdit
        Left = 72
        Top = 24
        Width = 74
        Height = 24
        ButtonKind = bkStandard
        MaxValue = 999.000000000000000000
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        TabOrder = 0
      end
      object StaticText1: TStaticText
        Left = 7
        Top = 27
        Width = 45
        Height = 20
        Caption = 'Power:'
        TabOrder = 1
      end
      object StaticText2: TStaticText
        Left = 164
        Top = 27
        Width = 82
        Height = 20
        Caption = 'Randomness:'
        TabOrder = 2
      end
      object spnRandomness: TJvSpinEdit
        Left = 245
        Top = 24
        Width = 74
        Height = 24
        ButtonKind = bkStandard
        MaxValue = 10.000000000000000000
        TabOrder = 3
      end
    end
    object GroupBox2: TGroupBox
      Left = 8
      Top = 211
      Width = 333
      Height = 58
      Caption = 'Defense'
      TabOrder = 2
      object spnDef: TJvSpinEdit
        Left = 72
        Top = 24
        Width = 74
        Height = 24
        ButtonKind = bkStandard
        MaxValue = 100.000000000000000000
        Value = 100.000000000000000000
        TabOrder = 0
      end
      object txtPhysDefense: TStaticText
        Left = 7
        Top = 27
        Width = 54
        Height = 20
        Caption = 'Physical:'
        TabOrder = 1
      end
      object txtMagDefense: TStaticText
        Left = 164
        Top = 27
        Width = 52
        Height = 20
        Caption = 'Magical:'
        TabOrder = 2
      end
      object spnMDef: TJvSpinEdit
        Left = 245
        Top = 24
        Width = 74
        Height = 24
        ButtonKind = bkStandard
        MaxValue = 100.000000000000000000
        TabOrder = 3
      end
    end
    object chkAssign: TCheckBox
      Left = 15
      Top = 279
      Width = 154
      Height = 17
      Caption = 'Store total damage to:'
      TabOrder = 3
      OnClick = RadioButtonClick
    end
    object selAssign: TIntSelector
      Left = 172
      Top = 275
      Width = 155
      Height = 24
    end
  end
  inherited btnOK: TButton
    Top = 328
  end
  inherited btnCancel: TButton
    Top = 328
  end
  inherited btnHelp: TButton
    Top = 328
  end
  object srcHeroes: TDataSource
    DataSet = dmDatabase.heroes
    Left = 16
    Top = 328
  end
end
