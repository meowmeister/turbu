object frmTurbuMain: TfrmTurbuMain
  Left = 183
  Top = 38
  Caption = 'TURBU - The Ultimate Rpg BUilder'
  ClientHeight = 640
  ClientWidth = 750
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = mnuMain
  OldCreateOrder = False
  Position = poDesigned
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object imgLogo: TSdlFrame
    Left = 0
    Top = 0
    Width = 750
    Height = 640
    Framerate = 0
    Active = False
    OnAvailable = imgLogoAvailable
  end
  object mnuMain: TMainMenu
    Left = 336
    object mnuFile: TMenuItem
      Caption = '&File'
      object mnuNew: TMenuItem
        Caption = '&New Project...'
        Enabled = False
        ShortCut = 16462
      end
      object mnuOpen: TMenuItem
        Caption = '&Open Project...'
        ShortCut = 16463
        OnClick = mnuOpenClick
      end
      object mnuImport: TMenuItem
        Caption = '&Import Project'
        object mnu2K: TMenuItem
          Caption = 'RPG Maker &2000/2003...'
          OnClick = mnu2KClick
        end
      end
      object mnuSep1: TMenuItem
        Caption = '-'
      end
      object mnuExit: TMenuItem
        Caption = 'E&xit'
        OnClick = mnuExitClick
      end
    end
    object mnuEdit1: TMenuItem
      Caption = '&Edit'
      object mnuSkillEdit: TMenuItem
        Caption = '&Skills'
        Enabled = False
        OnClick = mnuSkillEditClick
      end
      object mnuDatabase: TMenuItem
        Caption = '&Database'
        Enabled = False
        OnClick = mnuDatabaseClick
      end
    end
  end
  object dlgOpen: TOpenDialog
    Left = 184
    Top = 96
  end
  object pluginManager: TJvPluginManager
    Extension = 'TEP'
    PluginKind = plgPackage
    Left = 264
    Top = 48
  end
end
