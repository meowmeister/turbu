object frmSpriteSelector: TfrmSpriteSelector
  Left = 0
  Top = 0
  Caption = 'Select Sprite'
  ClientHeight = 441
  ClientWidth = 475
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 16
  object Panel1: TPanel
    Left = 8
    Top = 8
    Width = 460
    Height = 367
    TabOrder = 0
    object lstFilenames: TListBox
      Left = 8
      Top = 12
      Width = 177
      Height = 340
      IntegralHeight = True
      TabOrder = 0
      OnClick = lstFilenamesClick
    end
  end
  object btnOK: TButton
    Left = 263
    Top = 395
    Width = 89
    Height = 33
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 359
    Top = 395
    Width = 89
    Height = 33
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object imgSelector: TSdlFrame
    Left = 201
    Top = 20
    Width = 256
    Height = 341
    Framerate = 0
    Active = False
    LogicalWidth = 256
    LogicalHeight = 341
    OnAvailable = imgSelectorAvailable
    OnMouseDown = imgSelectorMouseDown
    OnMouseMove = imgSelectorMouseMove
  end
end
