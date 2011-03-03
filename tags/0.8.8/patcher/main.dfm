object frmPatcher: TfrmPatcher
  Left = 0
  Top = 0
  Caption = 'TURBU Patcher'
  ClientHeight = 339
  ClientWidth = 754
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btnLoadFile: TButton
    Left = 120
    Top = 296
    Width = 75
    Height = 25
    Action = actLoadFile
    TabOrder = 0
  end
  object txtFile: TMemo
    Left = 8
    Top = 0
    Width = 353
    Height = 273
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
  end
  object txtPatch: TMemo
    Left = 392
    Top = 0
    Width = 353
    Height = 273
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 2
    WordWrap = False
  end
  object btnLoadPatch: TButton
    Left = 536
    Top = 296
    Width = 75
    Height = 25
    Action = actLoadPatch
    TabOrder = 3
  end
  object btnRun: TButton
    Left = 328
    Top = 296
    Width = 75
    Height = 25
    Caption = '&Run Patch'
    Enabled = False
    TabOrder = 4
    OnClick = btnRunClick
  end
  object dlgLoadFile: TOpenTextFileDialog
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 360
    Top = 48
  end
  object dlgLoadPatch: TOpenTextFileDialog
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 360
    Top = 104
  end
  object MainMenu1: TMainMenu
    Left = 360
    Top = 160
    object File1: TMenuItem
      Caption = '&File'
      object LoadFile1: TMenuItem
        Action = actLoadFile
      end
      object LoadPatch1: TMenuItem
        Action = actLoadPatch
      end
      object SaveFile1: TMenuItem
        Caption = '&Save File'
      end
      object SaveFileAs1: TMenuItem
        Caption = 'Save File &As...'
      end
    end
    object Help1: TMenuItem
      Caption = '&Help'
      object AboutTURBUPatcher1: TMenuItem
        Caption = '&About TURBU Patcher...'
        OnClick = AboutTURBUPatcher1Click
      end
    end
  end
  object ActionList1: TActionList
    Left = 360
    Top = 216
    object actLoadFile: TAction
      Caption = 'Load &File'
      OnExecute = btnLoadFileClick
    end
    object actLoadPatch: TAction
      Caption = 'Load &Patch'
      OnExecute = btnLoadPatchClick
    end
  end
end
