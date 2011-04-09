inherited frmEBEditHeroName: TfrmEBEditHeroName
  Caption = 'Change Hero Name'
  ClientHeight = 141
  ClientWidth = 286
  PixelsPerInch = 120
  TextHeight = 16
  inherited Panel1: TPanel
    Width = 286
    Height = 88
    object cboHero: TIDLookupCombo
      Left = 97
      Top = 16
      Width = 181
      Height = 24
      KeyField = 'id'
      ListField = 'name'
      ListSource = srcHero
      TabOrder = 0
    end
    object StaticText1: TStaticText
      Left = 16
      Top = 20
      Width = 36
      Height = 20
      Caption = 'Hero:'
      TabOrder = 1
    end
    object lblNewName: TStaticText
      Left = 16
      Top = 57
      Width = 71
      Height = 20
      Caption = 'New Name:'
      TabOrder = 2
    end
    object txtName: TEdit
      Left = 97
      Top = 53
      Width = 181
      Height = 24
      TabOrder = 3
    end
  end
  inherited btnOK: TButton
    Left = 3
    Top = 98
  end
  inherited btnCancel: TButton
    Left = 97
    Top = 98
  end
  inherited btnHelp: TButton
    Left = 190
    Top = 98
  end
  object srcHero: TDataSource
    DataSet = dmDatabase.heroes
    Left = 16
    Top = 168
  end
end
