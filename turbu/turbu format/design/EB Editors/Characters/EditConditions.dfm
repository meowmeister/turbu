inherited frmEBEditConditions: TfrmEBEditConditions
  Caption = 'Change Condition'
  ClientHeight = 295
  PixelsPerInch = 120
  TextHeight = 16
  inherited Panel1: TPanel
    Height = 242
    object grpOperation: TRadioGroup
      Left = 8
      Top = 142
      Width = 333
      Height = 49
      Caption = 'Operation'
      Columns = 2
      ItemIndex = 0
      Items.Strings = (
        'Add Condition'
        'Remove Condition')
      TabOrder = 1
      OnClick = RadioButtonClick
    end
    object StaticText1: TStaticText
      Left = 15
      Top = 200
      Width = 62
      Height = 20
      Caption = 'Condition:'
      TabOrder = 2
    end
    object cboCondition: TIDLookupCombo
      Left = 112
      Top = 197
      Width = 215
      Height = 24
      KeyField = 'id'
      ListField = 'name'
      ListSource = srcConditions
      TabOrder = 3
    end
  end
  inherited btnOK: TButton
    Top = 252
  end
  inherited btnCancel: TButton
    Top = 252
  end
  inherited btnHelp: TButton
    Top = 252
  end
  object srcConditions: TDataSource
    DataSet = dmDatabase.conditions
    Left = 8
    Top = 256
  end
end
