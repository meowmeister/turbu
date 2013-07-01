object frmEbEditBase: TfrmEbEditBase
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  ClientHeight = 258
  ClientWidth = 349
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  DesignSize = (
    349
    258)
  PixelsPerInch = 120
  TextHeight = 16
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 349
    Height = 205
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    Padding.Left = 10
    Padding.Top = 10
    Padding.Right = 5
    Padding.Bottom = 5
    TabOrder = 0
  end
  object btnOK: TButton
    Left = 66
    Top = 215
    Width = 88
    Height = 35
    Anchors = [akRight, akBottom]
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 160
    Top = 215
    Width = 88
    Height = 35
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object btnHelp: TButton
    Left = 253
    Top = 215
    Width = 88
    Height = 35
    Anchors = [akRight, akBottom]
    Caption = '&Help'
    Enabled = False
    TabOrder = 3
  end
end
