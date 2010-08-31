inherited frmEBSetInteger: TfrmEBSetInteger
  Caption = 'Set Integer'
  ClientHeight = 434
  ClientWidth = 501
  OnCreate = FormCreate
  ExplicitWidth = 507
  ExplicitHeight = 469
  PixelsPerInch = 120
  TextHeight = 16
  inherited Panel1: TPanel
    Width = 501
    Height = 385
    ExplicitWidth = 501
    ExplicitHeight = 385
    object GroupBox1: TGroupBox
      Left = 4
      Top = 1
      Width = 339
      Height = 86
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Which variable'
      TabOrder = 0
      object radSwitch: TRadioButton
        Left = 11
        Top = 28
        Width = 70
        Height = 17
        Caption = 'Variable'
        TabOrder = 0
      end
      object radInt: TRadioButton
        Left = 202
        Top = 28
        Width = 129
        Height = 17
        Caption = 'Variable Reference'
        TabOrder = 1
      end
      object IntSelector1: TIntSelector
        Left = 11
        Top = 54
        Width = 320
        Height = 24
      end
    end
    object grpOperand: TGroupBox
      Left = 4
      Top = 185
      Width = 489
      Height = 198
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Operand'
      TabOrder = 1
      object radNumber: TRadioButton
        Left = 11
        Top = 27
        Width = 71
        Height = 22
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Number'
        TabOrder = 0
      end
      object radValue: TRadioButton
        Tag = 1
        Left = 11
        Top = 61
        Width = 121
        Height = 23
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Value of Variable'
        TabOrder = 1
      end
      object radReference: TRadioButton
        Tag = 2
        Left = 11
        Top = 96
        Width = 184
        Height = 23
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Value of Variable Reference'
        TabOrder = 2
      end
      object radRandom: TRadioButton
        Tag = 3
        Left = 11
        Top = 131
        Width = 110
        Height = 22
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Function Result'
        TabOrder = 3
      end
      object radItem: TRadioButton
        Tag = 4
        Left = 11
        Top = 164
        Width = 110
        Height = 24
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Object Property'
        TabOrder = 4
      end
      object spnNumber: TJvSpinEdit
        Left = 227
        Top = 24
        Width = 104
        Height = 24
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        ButtonKind = bkStandard
        MaxValue = 9999999.000000000000000000
        MinValue = -9999999.000000000000000000
        TabOrder = 5
      end
      object selValue: TIntSelector
        Left = 225
        Top = 59
        Width = 253
        Height = 24
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
      end
      object selIndex: TIntSelector
        Left = 227
        Top = 93
        Width = 251
        Height = 24
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
      end
      object cbxItem: TComboBox
        Left = 129
        Top = 163
        Width = 182
        Height = 24
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Style = csDropDownList
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 8
      end
      object cbxItemAction: TComboBox
        Left = 331
        Top = 163
        Width = 147
        Height = 24
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Style = csDropDownList
        DoubleBuffered = False
        ParentDoubleBuffered = False
        TabOrder = 9
        Items.Strings = (
          '# Held'
          '# Equipped')
      end
      object cbxFunctions: TComboBox
        Left = 129
        Top = 132
        Width = 182
        Height = 24
        Style = csDropDownList
        TabOrder = 10
      end
    end
    object GroupBox2: TGroupBox
      Left = 351
      Top = 10
      Width = 142
      Height = 169
      TabOrder = 2
    end
  end
  inherited btnOK: TButton
    Left = 218
    Top = 391
    ExplicitLeft = 218
    ExplicitTop = 391
  end
  inherited btnCancel: TButton
    Left = 312
    Top = 391
    ExplicitLeft = 312
    ExplicitTop = 391
  end
  inherited btnHelp: TButton
    Left = 405
    Top = 391
    ExplicitLeft = 405
    ExplicitTop = 391
  end
  object grpOperation: TRadioGroup
    Left = 4
    Top = 90
    Width = 339
    Height = 89
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Operation'
    Columns = 3
    Items.Strings = (
      'Set Equal'
      'Add'
      'Subtract'
      'Multiply'
      'Divide'
      'Modulus')
    TabOrder = 4
  end
end
