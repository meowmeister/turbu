inherited frmEBSelector: TfrmEBSelector
  Caption = 'New Event Command'
  ClientHeight = 455
  ClientWidth = 290
  OnCreate = FormCreate
  ExplicitWidth = 296
  ExplicitHeight = 490
  PixelsPerInch = 120
  TextHeight = 16
  inherited Panel1: TPanel
    Width = 290
    Height = 402
    ExplicitWidth = 290
    ExplicitHeight = 402
    object trvList: TTreeView
      Left = 0
      Top = 0
      Width = 290
      Height = 402
      Align = alClient
      Indent = 19
      ReadOnly = True
      RightClickSelect = True
      RowSelect = True
      TabOrder = 0
      OnClick = trvListClick
    end
  end
  inherited btnOK: TButton
    Left = 7
    Top = 412
    ExplicitLeft = 7
    ExplicitTop = 412
  end
  inherited btnCancel: TButton
    Left = 101
    Top = 412
    ExplicitLeft = 101
    ExplicitTop = 412
  end
  inherited btnHelp: TButton
    Left = 194
    Top = 412
    ExplicitLeft = 194
    ExplicitTop = 412
  end
end
