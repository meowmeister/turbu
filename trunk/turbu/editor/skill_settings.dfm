object frmSkillLearning: TfrmSkillLearning
  Left = 0
  Top = 0
  Caption = 'Skill Settings'
  ClientHeight = 275
  ClientWidth = 314
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlSillData: TPanel
    Left = 8
    Top = 8
    Width = 298
    Height = 209
    TabOrder = 0
    object grpData1: TGroupBox
      Left = 8
      Top = 8
      Width = 81
      Height = 44
      Caption = 'Item1'
      TabOrder = 0
      object spnData1: TJvDBSpinEdit
        Left = 8
        Top = 14
        Width = 60
        Height = 21
        ButtonKind = bkClassic
        TabOrder = 0
        DataField = 'nums[1]'
        DataSource = dsForeign
      end
    end
    object grpData2: TGroupBox
      Left = 8
      Top = 58
      Width = 81
      Height = 44
      Caption = 'Item2'
      TabOrder = 1
      object spnData2: TJvDBSpinEdit
        Left = 8
        Top = 14
        Width = 60
        Height = 21
        ButtonKind = bkClassic
        TabOrder = 0
        DataField = 'nums[2]'
        DataSource = dsForeign
      end
    end
    object grpAlgorithm: TGroupBox
      Left = 112
      Top = 8
      Width = 177
      Height = 84
      Caption = 'Algorithm'
      TabOrder = 4
      object btnNewAlgorithm: TButton
        Left = 92
        Top = 48
        Width = 75
        Height = 25
        Caption = '&Add New...'
        TabOrder = 1
        OnClick = btnNewAlgorithmClick
      end
      object btnEdit: TButton
        Left = 18
        Top = 48
        Width = 60
        Height = 25
        Caption = '&Edit...'
        TabOrder = 2
        OnClick = btnEditClick
      end
      object cbxAlgorithm: TDBLookupComboBox
        Left = 8
        Top = 14
        Width = 161
        Height = 21
        DataField = 'algName'
        DataSource = dsForeign
        TabOrder = 0
      end
    end
    object grpSkill: TGroupBox
      Left = 112
      Top = 98
      Width = 177
      Height = 44
      Caption = 'Skill'
      TabOrder = 5
      object cbxSkill: TDBLookupComboBox
        Left = 8
        Top = 14
        Width = 161
        Height = 21
        DataField = 'name'
        DataSource = dsForeign
        TabOrder = 0
      end
    end
    object grpData3: TGroupBox
      Left = 8
      Top = 108
      Width = 81
      Height = 44
      Caption = 'Item3'
      TabOrder = 2
      object spnData3: TJvDBSpinEdit
        Left = 8
        Top = 14
        Width = 60
        Height = 21
        ButtonKind = bkClassic
        TabOrder = 0
        DataField = 'nums[3]'
        DataSource = dsForeign
      end
    end
    object grpData4: TGroupBox
      Left = 8
      Top = 158
      Width = 81
      Height = 44
      Caption = 'Item4'
      TabOrder = 3
      object spnData4: TJvDBSpinEdit
        Left = 8
        Top = 14
        Width = 60
        Height = 21
        ButtonKind = bkClassic
        TabOrder = 0
        DataField = 'nums[4]'
        DataSource = dsForeign
      end
    end
    object grpNull: TGroupBox
      Left = 113
      Top = 148
      Width = 176
      Height = 54
      TabOrder = 6
    end
  end
  object btnOK: TButton
    Left = 40
    Top = 240
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 192
    Top = 240
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 2
    OnClick = btnCancelClick
  end
  object dsForeign: TDataSource
    DataSet = dmDatabase.charClasses_skillset
    Left = 248
    Top = 168
  end
end
