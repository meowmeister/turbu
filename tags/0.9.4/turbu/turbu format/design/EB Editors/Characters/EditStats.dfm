inherited frmEBEditStats: TfrmEBEditStats
  Caption = 'Change Stats'
  ClientHeight = 352
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 16
  inherited Panel1: TPanel
    Height = 299
    object grpOperation: TRadioGroup
      Left = 8
      Top = 142
      Width = 177
      Height = 51
      Caption = 'Operation'
      Columns = 2
      ItemIndex = 0
      Items.Strings = (
        'Increase'
        'Decrease')
      TabOrder = 1
    end
    object GroupBox1: TGroupBox
      Left = 191
      Top = 142
      Width = 150
      Height = 51
      Caption = 'Stat'
      TabOrder = 2
      object cboStat: TComboBox
        Left = 13
        Top = 19
        Width = 123
        Height = 24
        Style = csDropDownList
        TabOrder = 0
      end
    end
    object GroupBox2: TGroupBox
      Left = 8
      Top = 196
      Width = 333
      Height = 94
      Caption = 'Amount'
      TabOrder = 3
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
    Top = 309
  end
  inherited btnCancel: TButton
    Top = 309
  end
  inherited btnHelp: TButton
    Top = 309
  end
end
