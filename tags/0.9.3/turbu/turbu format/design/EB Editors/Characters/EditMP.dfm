inherited frmEBEditMP: TfrmEBEditMP
  Caption = 'Change MP'
  ClientHeight = 342
  ExplicitWidth = 320
  ExplicitHeight = 240
  PixelsPerInch = 120
  TextHeight = 16
  inherited Panel1: TPanel
    Height = 289
    ExplicitHeight = 289
    object grpOperation: TRadioGroup
      Left = 8
      Top = 142
      Width = 333
      Height = 49
      Caption = 'Operation'
      Columns = 2
      ItemIndex = 0
      Items.Strings = (
        'Increase MP'
        'Decrease MP')
      TabOrder = 1
      OnClick = RadioButtonClick
    end
    object grpItemCount: TGroupBox
      Left = 8
      Top = 195
      Width = 333
      Height = 94
      Anchors = [akLeft, akBottom]
      Caption = 'Amount'
      TabOrder = 2
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
        Left = 104
        Top = 28
        Width = 161
        Height = 24
        ButtonKind = bkClassic
        MaxValue = 9999999.000000000000000000
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        TabOrder = 2
      end
      object selValue: TIntSelector
        Left = 104
        Top = 58
        Width = 215
        Height = 24
      end
    end
  end
  inherited btnOK: TButton
    Top = 299
    ExplicitTop = 299
  end
  inherited btnCancel: TButton
    Top = 299
    ExplicitTop = 299
  end
  inherited btnHelp: TButton
    Top = 299
    ExplicitTop = 299
  end
end
