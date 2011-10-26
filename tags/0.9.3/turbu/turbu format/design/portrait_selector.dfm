inherited frmPortraitSelector: TfrmPortraitSelector
  Caption = 'Portrait Selector'
  ClientHeight = 445
  ClientWidth = 427
  ExplicitWidth = 433
  ExplicitHeight = 480
  PixelsPerInch = 120
  TextHeight = 16
  inherited Panel1: TPanel
    Width = 414
    ExplicitWidth = 414
    inherited lstFilenames: TListBox
      Width = 169
      ExplicitWidth = 169
    end
    object ScrollBar1: TScrollBar
      Left = 386
      Top = 12
      Width = 21
      Height = 336
      Kind = sbVertical
      PageSize = 0
      TabOrder = 1
      Visible = False
    end
  end
  inherited btnOK: TButton
    Left = 231
    ExplicitLeft = 231
  end
  inherited btnCancel: TButton
    Left = 327
    ExplicitLeft = 327
  end
  inherited imgSelector: TSdlFrame
    Width = 192
    Height = 336
    LogicalWidth = 192
    LogicalHeight = 336
    ExplicitWidth = 192
    ExplicitHeight = 336
  end
end
