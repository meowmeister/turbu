inherited frmEBEditComparisonBool: TfrmEBEditComparisonBool
  Caption = 'Comparison (Boolean)'
  ClientHeight = 394
  ClientWidth = 348
  OnDestroy = FormDestroy
  PixelsPerInch = 120
  TextHeight = 16
  inherited Panel1: TPanel
    Width = 348
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
      object cboLVar: TComboBox
        Left = 16
        Top = 56
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
    object grpOperation: TRadioGroup
      Left = 260
      Top = 10
      Width = 83
      Height = 326
      Align = alClient
      Caption = 'Value'
      ItemIndex = 0
      Items.Strings = (
        'False'
        'True')
      TabOrder = 1
    end
  end
  inherited btnOK: TButton
    Left = 65
    Top = 351
  end
  inherited btnCancel: TButton
    Left = 159
    Top = 351
  end
  inherited btnHelp: TButton
    Left = 252
    Top = 351
  end
end
