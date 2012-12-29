inherited frmConditionEdit: TfrmConditionEdit
  Caption = 'Condition Editor'
  ClientHeight = 277
  PixelsPerInch = 120
  TextHeight = 16
  inherited Panel1: TPanel
    Height = 224
    inline frEditCondition: TfrEditCondition
      Left = 10
      Top = 10
      Width = 334
      Height = 209
      Align = alClient
      TabOrder = 0
      inherited trvCondition: TEBTreeView
        Top = 23
        Width = 334
        Height = 186
      end
      inherited StaticText1: TStaticText
        Left = 6
        Top = -4
      end
    end
  end
  inherited btnOK: TButton
    Top = 234
  end
  inherited btnCancel: TButton
    Top = 234
  end
  inherited btnHelp: TButton
    Top = 234
  end
end
