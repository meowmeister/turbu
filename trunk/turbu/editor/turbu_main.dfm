object frmTurbuMain: TfrmTurbuMain
  Left = 183
  Top = 38
  Caption = 'TURBU - The Ultimate Rpg BUilder'
  ClientHeight = 640
  ClientWidth = 925
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = mnuMain
  OldCreateOrder = False
  Position = poDesigned
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object sbxMain: TScrollBox
    Left = 213
    Top = 0
    Width = 712
    Height = 640
    HorzScrollBar.Tracking = True
    VertScrollBar.ButtonSize = 15
    VertScrollBar.Margin = 10
    VertScrollBar.Range = 100
    VertScrollBar.Size = 15
    VertScrollBar.Style = ssHotTrack
    VertScrollBar.ThumbSize = 10
    VertScrollBar.Tracking = True
    Align = alClient
    AutoScroll = False
    Color = clBlack
    ParentColor = False
    TabOrder = 0
    object imgLogo: TSdlFrame
      Left = 0
      Top = 0
      Width = 691
      Height = 619
      Framerate = 0
      Active = False
      OnAvailable = imgLogoAvailable
      Align = alClient
    end
    object pnlHorizScroll: TPanel
      Left = 0
      Top = 619
      Width = 708
      Height = 17
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      object sbHoriz: TScrollBar
        Left = 0
        Top = 0
        Width = 691
        Height = 17
        Align = alClient
        PageSize = 100
        TabOrder = 0
        OnScroll = OnScrollMap
      end
      object pnlCorner: TPanel
        Left = 691
        Top = 0
        Width = 17
        Height = 17
        Align = alRight
        Caption = 'pnlCorner'
        TabOrder = 1
      end
    end
    object pnlVertScroll: TPanel
      Left = 691
      Top = 0
      Width = 17
      Height = 619
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 2
      object sbVert: TScrollBar
        Left = 0
        Top = 0
        Width = 17
        Height = 619
        Align = alClient
        Kind = sbVertical
        PageSize = 100
        TabOrder = 0
        OnScroll = OnScrollMap
      end
    end
  end
  object pnlSidebar: TPanel
    Left = 0
    Top = 0
    Width = 213
    Height = 640
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
    object splSidebar: TSplitter
      Left = 0
      Top = 384
      Width = 213
      Height = 3
      Cursor = crVSplit
      Align = alTop
      OnMoved = splSidebarMoved
      ExplicitLeft = 1
      ExplicitTop = 385
      ExplicitWidth = 194
    end
    object sbxPallette: TScrollBox
      Left = 0
      Top = 0
      Width = 213
      Height = 384
      HorzScrollBar.Visible = False
      VertScrollBar.ButtonSize = 15
      VertScrollBar.Margin = 15
      VertScrollBar.Range = 100
      VertScrollBar.Size = 10
      VertScrollBar.ThumbSize = 10
      VertScrollBar.Tracking = True
      Align = alTop
      AutoScroll = False
      Color = clGreen
      ParentColor = False
      TabOrder = 0
      object imgPalette: TSdlFrame
        Left = 0
        Top = 0
        Width = 192
        Height = 380
        Framerate = 0
        Active = False
        Align = alClient
      end
      object sbPalette: TScrollBar
        Left = 192
        Top = 0
        Width = 17
        Height = 380
        Align = alRight
        Kind = sbVertical
        PageSize = 100
        TabOrder = 1
        OnScroll = sbPaletteScroll
      end
    end
    object trvMapTree: TTreeView
      Left = 0
      Top = 387
      Width = 213
      Height = 253
      Align = alClient
      Indent = 19
      TabOrder = 1
    end
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
    Filter = 'TURBU Projects (project.tdb)|project.tdb'
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
