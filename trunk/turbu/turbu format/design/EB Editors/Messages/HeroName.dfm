inherited frmInputHeroName: TfrmInputHeroName
  Caption = 'Enter Hero Name'
  ClientHeight = 197
  ClientWidth = 291
  ExplicitWidth = 297
  ExplicitHeight = 232
  PixelsPerInch = 120
  TextHeight = 16
  inherited Panel1: TPanel
    Width = 291
    Height = 144
    object GroupBox1: TGroupBox
      Left = 8
      Top = 8
      Width = 275
      Height = 65
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Hero'
      TabOrder = 0
      ExplicitWidth = 278
      object cboHeroID: TIDLookupCombo
        Left = 9
        Top = 25
        Width = 261
        Height = 24
        KeyField = 'id'
        ListField = 'name'
        ListSource = srcHeroes
        TabOrder = 0
      end
    end
    object GroupBox2: TGroupBox
      Left = 8
      Top = 79
      Width = 275
      Height = 56
      Anchors = [akLeft, akTop, akRight, akBottom]
      Caption = 'Options'
      TabOrder = 1
      ExplicitHeight = 78
      object chkShowName: TCheckBox
        Left = 9
        Top = 25
        Width = 261
        Height = 17
        Caption = 'Show hero name'
        TabOrder = 0
      end
    end
  end
  inherited btnOK: TButton
    Left = 8
    Top = 154
  end
  inherited btnCancel: TButton
    Left = 102
    Top = 154
  end
  inherited btnHelp: TButton
    Left = 195
    Top = 154
  end
  object srcHeroes: TDataSource
    DataSet = dmDatabase.heroes
    Left = 16
    Top = 344
  end
end
