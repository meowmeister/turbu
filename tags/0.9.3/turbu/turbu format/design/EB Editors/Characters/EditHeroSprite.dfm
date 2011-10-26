inherited frmEBEditHeroSprite: TfrmEBEditHeroSprite
  Caption = 'Change Hero Sprite'
  ClientHeight = 298
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 16
  inherited Panel1: TPanel
    Height = 245
    object lstFilenames: TListBox
      Left = 164
      Top = 9
      Width = 177
      Height = 228
      IntegralHeight = True
      TabOrder = 0
    end
    object txtName: TStaticText
      Left = 8
      Top = 9
      Width = 36
      Height = 20
      Caption = 'Hero:'
      TabOrder = 1
    end
    object cboHero: TIDLookupCombo
      Left = 8
      Top = 31
      Width = 145
      Height = 24
      KeyField = 'id'
      ListField = 'name'
      ListSource = srcHeroes
      TabOrder = 2
      OnClick = cboHeroClick
    end
    object StaticText2: TStaticText
      Left = 8
      Top = 73
      Width = 43
      Height = 20
      Caption = 'Sprite:'
      TabOrder = 3
    end
    object imgMapSprite: TSdlFrame
      Left = 8
      Top = 95
      Width = 96
      Height = 96
      Framerate = 4
      Active = False
      LogicalWidth = 96
      LogicalHeight = 96
      OnTimer = imgMapSpriteTimer
      OnAvailable = imgMapSpriteAvailable
    end
    object chkTranslucent: TCheckBox
      Left = 8
      Top = 220
      Width = 97
      Height = 17
      Caption = 'Translucent'
      TabOrder = 5
      OnClick = chkTranslucentClick
    end
  end
  inherited btnOK: TButton
    Top = 255
  end
  inherited btnCancel: TButton
    Top = 255
  end
  inherited btnHelp: TButton
    Top = 255
  end
  object srcHeroes: TDataSource
    DataSet = dmDatabase.heroes
    Left = 16
    Top = 400
  end
end
