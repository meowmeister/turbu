inherited frmInputNumber: TfrmInputNumber
  Caption = 'Input Number'
  ClientHeight = 117
  PixelsPerInch = 120
  TextHeight = 16
  inherited Panel1: TPanel
    Height = 64
    object GroupBox2: TGroupBox
      Left = 96
      Top = 8
      Width = 245
      Height = 47
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Store to'
      TabOrder = 0
      object selInteger: TIntSelector
        Left = 10
        Top = 17
        Width = 223
        Height = 24
      end
    end
  end
  inherited btnOK: TButton
    Top = 74
  end
  inherited btnCancel: TButton
    Top = 74
  end
  inherited btnHelp: TButton
    Top = 74
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 81
    Height = 47
    Caption = '# of digits'
    TabOrder = 4
    object spnDigits: TJvSpinEdit
      Left = 10
      Top = 17
      Width = 55
      Height = 24
      ButtonKind = bkClassic
      MaxValue = 9.000000000000000000
      MinValue = 1.000000000000000000
      Value = 1.000000000000000000
      TabOrder = 0
    end
  end
end
