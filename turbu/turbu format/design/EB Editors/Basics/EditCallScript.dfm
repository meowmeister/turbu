inherited frmEBEditCall: TfrmEBEditCall
  Caption = 'Call Script'
  ClientHeight = 269
  PixelsPerInch = 120
  TextHeight = 16
  inherited Panel1: TPanel
    Height = 216
    object grpMain: TGroupBox
      Left = 10
      Top = 10
      Width = 334
      Height = 201
      Align = alClient
      Caption = 'Script To Call'
      TabOrder = 0
      object Label1: TLabel
        Left = 119
        Top = 106
        Width = 33
        Height = 16
        Caption = 'Page:'
      end
      object Label2: TLabel
        Left = 119
        Top = 171
        Width = 33
        Height = 16
        Caption = 'Page:'
      end
      object radGlobalScript: TRadioButton
        Left = 13
        Top = 29
        Width = 100
        Height = 17
        Caption = 'Global Script:'
        Checked = True
        TabOrder = 0
        TabStop = True
        OnClick = RadioButtonClick
      end
      object radMapObject: TRadioButton
        Left = 13
        Top = 75
        Width = 92
        Height = 17
        Caption = 'Map Object:'
        TabOrder = 1
        OnClick = RadioButtonClick
      end
      object cbxMapObject: TComboBox
        Left = 119
        Top = 71
        Width = 212
        Height = 24
        Style = csDropDownList
        TabOrder = 2
        OnClick = cbxMapObjectClick
      end
      object spnMapPage: TJvSpinEdit
        Left = 177
        Top = 103
        Width = 154
        Height = 24
        ButtonKind = bkStandard
        MaxValue = 1.000000000000000000
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        TabOrder = 3
      end
      object radMapObjectPtr: TRadioButton
        Left = 13
        Top = 141
        Width = 158
        Height = 17
        Caption = 'Map Object by variable:'
        TabOrder = 4
        OnClick = RadioButtonClick
      end
      object selObjPtr: TIntSelector
        Left = 177
        Top = 137
        Width = 154
        Height = 24
      end
      object selPage: TIntSelector
        Left = 177
        Top = 168
        Width = 154
        Height = 24
      end
      object cbxGlobalName: TIDLookupCombo
        Left = 119
        Top = 25
        Width = 212
        Height = 24
        KeyField = 'Id'
        ListField = 'Name'
        ListSource = srcGlobals
        TabOrder = 7
      end
    end
  end
  inherited btnOK: TButton
    Top = 226
  end
  inherited btnCancel: TButton
    Top = 226
  end
  inherited btnHelp: TButton
    Top = 226
  end
  object srcGlobals: TDataSource
    DataSet = dmDatabase.GlobalScripts
    Left = 16
    Top = 224
  end
end
