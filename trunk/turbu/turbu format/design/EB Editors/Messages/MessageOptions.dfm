inherited frmMessageOptions: TfrmMessageOptions
  Caption = 'Message Options'
  ClientWidth = 292
  ExplicitWidth = 298
  PixelsPerInch = 120
  TextHeight = 16
  inherited Panel1: TPanel
    Width = 292
    ExplicitWidth = 292
    object GroupBox3: TGroupBox
      Left = 9
      Top = 119
      Width = 275
      Height = 74
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Window Options'
      TabOrder = 0
      object chkNoHide: TCheckBox
        Left = 10
        Top = 24
        Width = 247
        Height = 17
        Caption = 'Prevent &window from hiding party'
        TabOrder = 0
      end
      object chkBlock: TCheckBox
        Left = 10
        Top = 50
        Width = 247
        Height = 17
        Caption = 'Pause other &events until finished'
        TabOrder = 1
      end
    end
    object radVisibility: TRadioGroup
      Left = 9
      Top = 8
      Width = 136
      Height = 105
      Caption = 'Visibility'
      ItemIndex = 0
      Items.Strings = (
        'O&paque'
        '&Invisible'
        'T&ranslucent')
      TabOrder = 1
    end
  end
  inherited btnOK: TButton
    Left = 9
    ExplicitLeft = 9
  end
  inherited btnCancel: TButton
    Left = 103
    ExplicitLeft = 103
  end
  inherited btnHelp: TButton
    Left = 196
    ExplicitLeft = 196
  end
  object radPosition: TRadioGroup
    Left = 151
    Top = 8
    Width = 133
    Height = 105
    Caption = 'Position'
    ItemIndex = 0
    Items.Strings = (
      '&Top'
      '&Middle'
      '&Bottom')
    TabOrder = 4
  end
end
