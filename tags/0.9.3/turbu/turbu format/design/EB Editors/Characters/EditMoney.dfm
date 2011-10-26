inherited frmEBEditMoney: TfrmEBEditMoney
  Caption = 'Change Money'
  ClientHeight = 215
  ClientWidth = 288
  ExplicitWidth = 294
  ExplicitHeight = 250
  PixelsPerInch = 120
  TextHeight = 16
  inherited Panel1: TPanel
    Width = 288
    Height = 162
    object GroupBox1: TGroupBox
      Left = 8
      Top = 61
      Width = 272
      Height = 94
      Caption = 'Amount'
      TabOrder = 0
      object radExactAmount: TRadioButton
        Left = 7
        Top = 27
        Width = 58
        Height = 17
        Caption = 'Exact:'
        Checked = True
        TabOrder = 0
        TabStop = True
        OnClick = RadioButtonClick
      end
      object radPointer: TRadioButton
        Left = 7
        Top = 62
        Width = 75
        Height = 17
        Caption = 'Value of:'
        TabOrder = 1
        OnClick = RadioButtonClick
      end
      object spnExactValue: TJvSpinEdit
        Left = 91
        Top = 23
        Width = 174
        Height = 24
        ButtonKind = bkClassic
        MaxValue = 999999999.000000000000000000
        TabOrder = 2
      end
      object selValue: TIntSelector
        Left = 91
        Top = 58
        Width = 174
        Height = 24
      end
    end
  end
  inherited btnOK: TButton
    Left = 5
    Top = 172
  end
  inherited btnCancel: TButton
    Left = 99
    Top = 172
  end
  inherited btnHelp: TButton
    Left = 192
    Top = 172
  end
  object grpOperation: TRadioGroup
    Left = 8
    Top = 8
    Width = 272
    Height = 49
    Caption = 'Operation'
    Columns = 3
    ItemIndex = 0
    Items.Strings = (
      'Add'
      'Subtract'
      'Set To')
    TabOrder = 4
  end
end
