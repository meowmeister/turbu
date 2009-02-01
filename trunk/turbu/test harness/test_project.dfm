object frmTestProjLocation: TfrmTestProjLocation
  Left = 0
  Top = 0
  Caption = 'Testing Project Location'
  ClientHeight = 206
  ClientWidth = 339
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lblLocation: TLabel
    Left = 45
    Top = 27
    Width = 117
    Height = 13
    Caption = 'Original project location:'
  end
  object lblConvertLocation: TLabel
    Left = 30
    Top = 59
    Width = 132
    Height = 13
    Caption = 'Converted project location:'
  end
  object dirProjectLocation: TJvDirectoryEdit
    Left = 168
    Top = 24
    Width = 163
    Height = 21
    DialogKind = dkWin32
    DirectInput = False
    TabOrder = 0
    OnChange = dirProjectLocationChange
  end
  object dirOutput: TJvDirectoryEdit
    Left = 168
    Top = 51
    Width = 163
    Height = 21
    DialogKind = dkWin32
    DirectInput = False
    ParentShowHint = False
    ReadOnly = True
    ShowHint = True
    ShowButton = False
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 256
    Top = 165
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object btnOK: TButton
    Left = 152
    Top = 165
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    Enabled = False
    ModalResult = 1
    TabOrder = 3
    OnClick = btnOKClick
  end
end
