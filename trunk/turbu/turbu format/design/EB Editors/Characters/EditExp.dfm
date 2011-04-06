inherited frmEBEditExp: TfrmEBEditExp
  Caption = 'Change Experience'
  ClientHeight = 388
  ExplicitHeight = 423
  PixelsPerInch = 120
  TextHeight = 16
  inherited Panel1: TPanel
    Height = 335
    ExplicitHeight = 335
    object GroupBox2: TGroupBox
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
        ListSource = srcHeroes
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
      object selItemID: TIntSelector
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
      end
    end
    object grpOperation: TRadioGroup
      Left = 8
      Top = 142
      Width = 333
      Height = 49
      Caption = 'Operation'
      Columns = 2
      ItemIndex = 0
      Items.Strings = (
        'Add EXP'
        'Remove EXP')
      TabOrder = 1
      OnClick = RadioButtonClick
    end
    object grpItemCount: TGroupBox
      Left = 8
      Top = 231
      Width = 333
      Height = 94
      Anchors = [akLeft, akBottom]
      Caption = 'Amount'
      TabOrder = 2
      object radExactAmount: TRadioButton
        Left = 7
        Top = 27
        Width = 58
        Height = 17
        Caption = 'Exact:'
        Checked = True
        TabOrder = 0
        TabStop = True
        OnClick = RadioButtonClick
      end
      object radPointer: TRadioButton
        Left = 7
        Top = 62
        Width = 75
        Height = 17
        Caption = 'Value of:'
        TabOrder = 1
        OnClick = RadioButtonClick
      end
      object spnExactValue: TJvSpinEdit
        Left = 104
        Top = 28
        Width = 161
        Height = 24
        ButtonKind = bkClassic
        MaxValue = 9999999.000000000000000000
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        TabOrder = 2
      end
      object selValue: TIntSelector
        Left = 104
        Top = 58
        Width = 215
        Height = 24
      end
    end
  end
  inherited btnOK: TButton
    Top = 345
    ExplicitTop = 345
  end
  inherited btnCancel: TButton
    Top = 345
    ExplicitTop = 345
  end
  inherited btnHelp: TButton
    Top = 345
    ExplicitTop = 345
  end
  object chkLevelMessage: TCheckBox
    Left = 15
    Top = 200
    Width = 312
    Height = 17
    Caption = 'Show message on level-up'
    TabOrder = 4
  end
  object srcHeroes: TDataSource
    DataSet = dmDatabase.heroes
    Left = 16
    Top = 344
  end
end
