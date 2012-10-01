inherited frmEBEditParty: TfrmEBEditParty
  Caption = 'Change Party'
  ClientHeight = 215
  ExplicitWidth = 320
  ExplicitHeight = 250
  PixelsPerInch = 120
  TextHeight = 16
  inherited Panel1: TPanel
    Height = 162
    ExplicitHeight = 162
    object GroupBox2: TGroupBox
      Left = 8
      Top = 62
      Width = 333
      Height = 88
      Caption = 'Hero'
      TabOrder = 0
      object cboHeroID: TIDLookupCombo
        Left = 104
        Top = 24
        Width = 215
        Height = 24
        KeyField = 'id'
        ListField = 'name'
        ListSource = srcHeroes
        TabOrder = 0
      end
      object radSpecificItem: TRadioButton
        Left = 7
        Top = 27
        Width = 72
        Height = 17
        Caption = 'Specific:'
        Checked = True
        TabOrder = 1
        TabStop = True
        OnDblClick = RadioButtonClick
      end
      object radItemPtr: TRadioButton
        Left = 7
        Top = 62
        Width = 91
        Height = 17
        Caption = 'ID Value of:'
        TabOrder = 2
        OnDblClick = RadioButtonClick
      end
      object selItemID: TIntSelector
        Left = 104
        Top = 58
        Width = 215
        Height = 24
      end
    end
    object grpOperation: TRadioGroup
      Left = 8
      Top = 8
      Width = 333
      Height = 49
      Caption = 'Operation'
      Columns = 2
      ItemIndex = 0
      Items.Strings = (
        'Add'
        'Remove')
      TabOrder = 1
    end
  end
  inherited btnOK: TButton
    Top = 172
    ExplicitTop = 172
  end
  inherited btnCancel: TButton
    Top = 172
    ExplicitTop = 172
  end
  inherited btnHelp: TButton
    Top = 172
    ExplicitTop = 172
  end
  object srcHeroes: TDataSource
    DataSet = dmDatabase.heroes
    Left = 24
    Top = 160
  end
end
