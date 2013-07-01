inherited frmMessageEdit: TfrmMessageEdit
  Caption = 'Message'
  ClientHeight = 180
  ClientWidth = 410
  PixelsPerInch = 120
  TextHeight = 16
  inherited Panel1: TPanel
    Width = 410
    Height = 127
    object Memo1: TMemo
      Left = 10
      Top = 10
      Width = 395
      Height = 112
      Align = alClient
      TabOrder = 0
    end
  end
  inherited btnOK: TButton
    Left = 127
    Top = 137
  end
  inherited btnCancel: TButton
    Left = 221
    Top = 137
  end
  inherited btnHelp: TButton
    Left = 314
    Top = 137
  end
end
