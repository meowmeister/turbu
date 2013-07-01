object frmTestProjLocation: TfrmTestProjLocation
  Left = 0
  Top = 0
  Caption = 'Testing Project Location'
  ClientHeight = 269
  ClientWidth = 443
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 17
  object lblLocation: TLabel
    Left = 59
    Top = 35
    Width = 148
    Height = 17
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Original project location:'
  end
  object lblConvertLocation: TLabel
    Left = 39
    Top = 77
    Width = 169
    Height = 17
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Converted project location:'
  end
  object dirProjectLocation: TJvDirectoryEdit
    Left = 220
    Top = 31
    Width = 213
    Height = 25
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    DialogKind = dkWin32
    DirectInput = False
    ButtonWidth = 27
    TabOrder = 0
    OnChange = dirProjectLocationChange
  end
  object dirOutput: TJvDirectoryEdit
    Left = 220
    Top = 67
    Width = 213
    Height = 25
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    DialogKind = dkWin32
    DirectInput = False
    ButtonWidth = 27
    ParentShowHint = False
    ReadOnly = True
    ShowHint = True
    ShowButton = False
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 335
    Top = 216
    Width = 98
    Height = 32
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object btnOK: TButton
    Left = 199
    Top = 216
    Width = 98
    Height = 32
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = '&OK'
    Default = True
    Enabled = False
    ModalResult = 1
    TabOrder = 3
    OnClick = btnOKClick
  end
end
