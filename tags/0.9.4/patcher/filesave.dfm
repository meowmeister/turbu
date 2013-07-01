object dlgSave: TdlgSave
  Left = 245
  Top = 108
  BorderStyle = bsDialog
  Caption = 'Patching Complete'
  ClientHeight = 111
  ClientWidth = 270
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poOwnerFormCenter
  DesignSize = (
    270
    111)
  PixelsPerInch = 96
  TextHeight = 13
  object btnSave: TButton
    Left = 8
    Top = 78
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Save'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object btnCancel: TButton
    Left = 190
    Top = 78
    Width = 72
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object Bevel1: TGroupBox
    Left = 8
    Top = 8
    Width = 254
    Height = 58
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 2
    object Label1: TLabel
      Left = 12
      Top = 3
      Width = 226
      Height = 16
      Alignment = taCenter
      Caption = 'Would you like to save the patched file?'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      WordWrap = True
    end
  end
  object btnSaveAs: TButton
    Left = 96
    Top = 78
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Save &As...'
    TabOrder = 3
    OnClick = btnSaveAsClick
  end
  object dlgSaveAs: TSaveTextFileDialog
    Left = 112
    Top = 40
  end
end
