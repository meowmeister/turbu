inherited frmSkinSelector: TfrmSkinSelector
  Caption = 'Change System Skin'
  ClientHeight = 209
  ClientWidth = 385
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 16
  inherited Panel1: TPanel
    Width = 385
    Height = 156
    Padding.Left = 0
    Padding.Top = 0
    Padding.Right = 0
    Padding.Bottom = 0
    inline frameImageSelector: TframeImageSelector
      Left = 0
      Top = 0
      Width = 385
      Height = 80
      Align = alClient
      TabOrder = 0
      inherited imgSelection: TImage
        Width = 160
        Height = 80
      end
      inherited lstFilename: TListBox
        Left = 183
        Height = 80
      end
    end
    object Panel2: TPanel
      Left = 0
      Top = 80
      Width = 385
      Height = 76
      Align = alBottom
      TabOrder = 1
      object radStyle: TRadioGroup
        Left = 8
        Top = 12
        Width = 176
        Height = 55
        Caption = 'Style'
        Columns = 2
        ItemIndex = 0
        Items.Strings = (
          'Stretched'
          'Tiled')
        TabOrder = 0
      end
      object radFont: TRadioGroup
        Left = 201
        Top = 12
        Width = 176
        Height = 55
        Caption = 'Font'
        Columns = 2
        ItemIndex = 0
        Items.Strings = (
          'Font 1'
          'Font 2')
        TabOrder = 1
      end
    end
  end
  inherited btnOK: TButton
    Left = 102
    Top = 166
  end
  inherited btnCancel: TButton
    Left = 196
    Top = 166
  end
  inherited btnHelp: TButton
    Left = 289
    Top = 166
  end
end
