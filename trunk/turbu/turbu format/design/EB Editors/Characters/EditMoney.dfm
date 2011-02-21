inherited frmEBEditMoney: TfrmEBEditMoney
  Caption = 'Change Money'
  ClientHeight = 218
  ClientWidth = 288
  OnShow = FormShow
  ExplicitWidth = 294
  ExplicitHeight = 253
  PixelsPerInch = 120
  TextHeight = 16
  inherited Panel1: TPanel
    Width = 288
    Height = 165
    object GroupBox1: TGroupBox
      Left = 8
      Top = 72
      Width = 272
      Height = 105
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
        OnClick = OnRadClick
      end
      object radPointer: TRadioButton
        Left = 7
        Top = 61
        Width = 75
        Height = 17
        Caption = 'Value of:'
        TabOrder = 1
        OnClick = OnRadClick
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
        Top = 57
        Width = 174
        Height = 24
      end
    end
  end
  inherited btnOK: TButton
    Left = 5
    Top = 175
  end
  inherited btnCancel: TButton
    Left = 99
    Top = 175
  end
  inherited btnHelp: TButton
    Left = 192
    Top = 175
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
