object frmImageSelector: TfrmImageSelector
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Image Selector'
  ClientHeight = 302
  ClientWidth = 539
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    539
    302)
  PixelsPerInch = 120
  TextHeight = 16
  object btnClose: TButton
    Left = 443
    Top = 259
    Width = 88
    Height = 35
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = '&Close'
    ModalResult = 2
    TabOrder = 0
  end
  object btnSelect: TButton
    Left = 336
    Top = 259
    Width = 88
    Height = 35
    Anchors = [akRight, akBottom]
    Caption = '&Select'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  inline frameImageSelector: TframeImageSelector
    Left = 0
    Top = 0
    Width = 539
    Height = 240
    TabOrder = 2
    ExplicitWidth = 539
    inherited imgSelection: TImage
      Width = 314
    end
    inherited lstFilename: TListBox
      Left = 337
    end
  end
end
