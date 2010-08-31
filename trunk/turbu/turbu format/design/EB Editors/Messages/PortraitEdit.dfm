inherited frmSelectPortrait: TfrmSelectPortrait
  Caption = 'Select Portrait'
  ClientHeight = 183
  ClientWidth = 314
  ExplicitHeight = 218
  PixelsPerInch = 120
  TextHeight = 16
  inherited Panel1: TPanel
    Width = 314
    Height = 130
    ExplicitWidth = 314
    ExplicitHeight = 130
    object GroupBox1: TGroupBox
      Left = 8
      Top = 0
      Width = 193
      Height = 129
      Caption = 'Portrait'
      TabOrder = 0
      object imgPortrait: TSdlFrame
        Left = 10
        Top = 19
        Width = 96
        Height = 96
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Framerate = 0
        Active = False
        LogicalWidth = 96
        LogicalHeight = 96
      end
      object btnSet: TButton
        Left = 113
        Top = 43
        Width = 75
        Height = 33
        Caption = '&Set'
        TabOrder = 1
        OnClick = btnSetClick
      end
      object btnClear: TButton
        Left = 113
        Top = 82
        Width = 75
        Height = 33
        Caption = '&Clear'
        TabOrder = 2
        OnClick = btnClearClick
      end
    end
    object radPosition: TRadioGroup
      Left = 207
      Top = 0
      Width = 99
      Height = 76
      Caption = 'Position'
      ItemIndex = 0
      Items.Strings = (
        '&Left'
        '&Right')
      TabOrder = 1
    end
    object GroupBox2: TGroupBox
      Left = 207
      Top = 76
      Width = 99
      Height = 53
      Caption = 'Flip'
      TabOrder = 2
      object chkFlipped: TCheckBox
        Left = 8
        Top = 22
        Width = 85
        Height = 17
        Caption = '&Flip Image'
        TabOrder = 0
        OnClick = chkFlippedClick
      end
    end
  end
  inherited btnOK: TButton
    Left = 31
    Top = 140
    ExplicitLeft = 31
    ExplicitTop = 140
  end
  inherited btnCancel: TButton
    Left = 125
    Top = 140
    ExplicitLeft = 125
    ExplicitTop = 140
  end
  inherited btnHelp: TButton
    Left = 218
    Top = 140
    ExplicitLeft = 218
    ExplicitTop = 140
  end
end
