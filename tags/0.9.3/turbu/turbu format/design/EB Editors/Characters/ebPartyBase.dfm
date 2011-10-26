inherited frmEBPartyBase: TfrmEBPartyBase
  Caption = 'frmEBPartyBase'
  ExplicitWidth = 355
  ExplicitHeight = 293
  PixelsPerInch = 120
  TextHeight = 16
  inherited Panel1: TPanel
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
  end
  object srcHeroes: TDataSource
    DataSet = dmDatabase.heroes
    Left = 16
    Top = 344
  end
end
