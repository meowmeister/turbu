inherited frmEBEditPortrait: TfrmEBEditPortrait
  Caption = 'Change Hero Portrait'
  ClientHeight = 199
  ClientWidth = 288
  PixelsPerInch = 120
  TextHeight = 16
  inherited Panel1: TPanel
    Width = 288
    Height = 146
    object cboHero: TIDLookupCombo
      Left = 8
      Top = 31
      Width = 145
      Height = 24
      KeyField = 'id'
      ListField = 'name'
      ListSource = srcHeroes
      TabOrder = 0
      OnClick = cboHeroClick
    end
    object StaticText1: TStaticText
      Left = 8
      Top = 9
      Width = 36
      Height = 20
      Caption = 'Hero:'
      TabOrder = 1
    end
    object imgPortrait: TSdlFrame
      Left = 176
      Top = 31
      Width = 96
      Height = 96
      Framerate = 4
      Active = False
      LogicalWidth = 96
      LogicalHeight = 96
      OnAvailable = imgPortraitAvailable
    end
    object StaticText2: TStaticText
      Left = 176
      Top = 9
      Width = 51
      Height = 20
      Caption = 'Portrait:'
      TabOrder = 3
    end
  end
  inherited btnOK: TButton
    Left = 5
    Top = 156
  end
  inherited btnCancel: TButton
    Left = 99
    Top = 156
  end
  inherited btnHelp: TButton
    Left = 192
    Top = 156
  end
  object btnSetPortrait: TButton
    Left = 72
    Top = 80
    Width = 81
    Height = 25
    Caption = '&Set Portrait'
    TabOrder = 4
    OnClick = btnSetPortraitClick
  end
  object srcHeroes: TDataSource
    DataSet = dmDatabase.heroes
    Left = 16
    Top = 400
  end
end
