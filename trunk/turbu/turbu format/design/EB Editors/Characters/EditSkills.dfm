inherited frmEBEditSkills: TfrmEBEditSkills
  Caption = 'frmEBEditSkills'
  ClientHeight = 344
  PixelsPerInch = 120
  TextHeight = 16
  inherited Panel1: TPanel
    Height = 291
    object grpOperation: TRadioGroup
      Left = 8
      Top = 142
      Width = 333
      Height = 51
      Caption = 'Operation'
      Columns = 2
      ItemIndex = 0
      Items.Strings = (
        'Add Skill'
        'Remove Skill')
      TabOrder = 1
    end
    object GroupBox2: TGroupBox
      Left = 8
      Top = 197
      Width = 333
      Height = 88
      Caption = 'Skill'
      TabOrder = 2
      object cboSkillID: TIDLookupCombo
        Left = 104
        Top = 24
        Width = 215
        Height = 24
        KeyField = 'id'
        ListField = 'name'
        ListSource = srcSkills
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
        OnClick = RadioButtonClick
      end
      object radItemPtr: TRadioButton
        Left = 7
        Top = 62
        Width = 91
        Height = 17
        Caption = 'ID Value of:'
        TabOrder = 2
        OnClick = RadioButtonClick
      end
      object selSkillID: TIntSelector
        Left = 104
        Top = 58
        Width = 215
        Height = 24
      end
    end
  end
  inherited btnOK: TButton
    Top = 301
  end
  inherited btnCancel: TButton
    Top = 301
  end
  inherited btnHelp: TButton
    Top = 301
  end
  object srcSkills: TDataSource
    DataSet = dmDatabase.skills
    Left = 16
    Top = 312
  end
end
