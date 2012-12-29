inherited frmEBEditIf: TfrmEBEditIf
  Caption = 'If/Then'
  ClientHeight = 391
  ClientWidth = 443
  PixelsPerInch = 120
  TextHeight = 16
  inherited Panel1: TPanel
    Width = 443
    Height = 338
    object Panel2: TPanel
      Left = 10
      Top = 10
      Width = 428
      Height = 284
      Align = alTop
      Anchors = [akLeft, akTop, akRight, akBottom]
      TabOrder = 0
      inline frEditCondition: TfrEditCondition
        Left = 1
        Top = 1
        Width = 426
        Height = 282
        Align = alClient
        TabOrder = 0
        inherited trvCondition: TEBTreeView
          Top = 30
          Width = 426
          Height = 252
        end
        inherited StaticText1: TStaticText
          Top = 4
        end
      end
    end
    object chkElseBlock: TCheckBox
      Left = 24
      Top = 307
      Width = 105
      Height = 17
      Anchors = [akLeft, akBottom]
      Caption = 'Add Else Block'
      TabOrder = 1
    end
  end
  inherited btnOK: TButton
    Left = 160
    Top = 348
  end
  inherited btnCancel: TButton
    Left = 254
    Top = 348
  end
  inherited btnHelp: TButton
    Left = 347
    Top = 348
  end
end
