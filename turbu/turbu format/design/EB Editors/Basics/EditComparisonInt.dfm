inherited frmEBEditComparisonInt: TfrmEBEditComparisonInt
  Caption = 'Comparison (Integers)'
  ClientHeight = 394
  ClientWidth = 612
  OnDestroy = FormDestroy
  PixelsPerInch = 120
  TextHeight = 16
  inherited Panel1: TPanel
    Width = 612
    Height = 341
    object Panel2: TPanel
      Left = 10
      Top = 10
      Width = 250
      Height = 326
      Align = alLeft
      TabOrder = 0
      object radLVariable: TRadioButton
        Left = 16
        Top = 16
        Width = 113
        Height = 17
        Caption = 'Variable'
        Checked = True
        TabOrder = 0
        TabStop = True
      end
      object radLProperty: TRadioButton
        Left = 16
        Top = 213
        Width = 113
        Height = 17
        Caption = 'Property'
        TabOrder = 1
      end
      object cboLProp: TComboBox
        Left = 16
        Top = 248
        Width = 185
        Height = 24
        Style = csDropDownList
        TabOrder = 2
      end
      object radLFunction: TRadioButton
        Left = 16
        Top = 112
        Width = 113
        Height = 17
        Caption = 'Function Result'
        TabOrder = 3
      end
      object txtLFunc: TEdit
        Left = 16
        Top = 149
        Width = 225
        Height = 24
        ReadOnly = True
        TabOrder = 4
      end
      object btnLFunc: TButton
        Left = 201
        Top = 154
        Width = 33
        Height = 18
        Caption = '...'
        TabOrder = 5
        OnClick = btnLFuncClick
      end
    end
    object Panel3: TPanel
      Left = 357
      Top = 10
      Width = 250
      Height = 326
      Align = alRight
      TabOrder = 1
      object radRValue: TRadioButton
        Left = 19
        Top = 16
        Width = 113
        Height = 17
        Caption = 'Value'
        Checked = True
        TabOrder = 0
        TabStop = True
      end
      object radRVariable: TRadioButton
        Left = 19
        Top = 112
        Width = 113
        Height = 17
        Caption = 'Variable'
        TabOrder = 1
      end
      object radRProperty: TRadioButton
        Left = 19
        Top = 213
        Width = 113
        Height = 17
        Caption = 'Property'
        TabOrder = 2
      end
      object cboRProp: TComboBox
        Left = 19
        Top = 248
        Width = 185
        Height = 24
        Style = csDropDownList
        TabOrder = 3
      end
    end
    object grpOperation: TRadioGroup
      Left = 260
      Top = 10
      Width = 97
      Height = 326
      Align = alClient
      Caption = 'Operation'
      ItemIndex = 0
      Items.Strings = (
        '='
        '>='
        '<='
        '>'
        '<'
        '<>')
      TabOrder = 2
    end
    object spnRValue: TJvSpinEdit
      Left = 376
      Top = 66
      Width = 121
      Height = 24
      ButtonKind = bkClassic
      TabOrder = 3
    end
  end
  inherited btnOK: TButton
    Left = 329
    Top = 351
  end
  inherited btnCancel: TButton
    Left = 423
    Top = 351
  end
  inherited btnHelp: TButton
    Left = 516
    Top = 351
  end
end
