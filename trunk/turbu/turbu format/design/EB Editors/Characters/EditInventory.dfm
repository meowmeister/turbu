inherited frmEBEditInventory: TfrmEBEditInventory
  Caption = 'Change Inventory'
  ClientHeight = 319
  ExplicitWidth = 355
  ExplicitHeight = 354
  PixelsPerInch = 120
  TextHeight = 16
  inherited Panel1: TPanel
    Height = 266
    ExplicitHeight = 266
    object grpOperation: TRadioGroup
      Left = 8
      Top = 8
      Width = 333
      Height = 49
      Caption = 'Operation'
      Columns = 3
      ItemIndex = 0
      Items.Strings = (
        'Add'
        'Remove'
        'Remove All')
      TabOrder = 0
      OnClick = RadioButtonClick
    end
    object grpItemCount: TGroupBox
      Left = 8
      Top = 162
      Width = 333
      Height = 94
      Anchors = [akLeft, akBottom]
      Caption = 'Amount'
      TabOrder = 1
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
        MaxValue = 99.000000000000000000
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
    Top = 276
    ExplicitTop = 276
  end
  inherited btnCancel: TButton
    Top = 276
    ExplicitTop = 276
  end
  inherited btnHelp: TButton
    Top = 276
    ExplicitTop = 276
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 68
    Width = 333
    Height = 88
    Caption = 'Item'
    TabOrder = 4
    object cboItemID: TIDLookupCombo
      Left = 104
      Top = 24
      Width = 215
      Height = 24
      KeyField = 'id'
      ListField = 'name'
      ListSource = srcItems
      TabOrder = 0
    end
    object radSpecificItem: TRadioButton
      Left = 7
      Top = 27
      Width = 72
      Height = 17
      Caption = 'Specific:'
      Checked = True
      TabOrder = 1
      TabStop = True
      OnClick = RadioButtonClick
    end
    object radItemPtr: TRadioButton
      Left = 7
      Top = 62
      Width = 91
      Height = 17
      Caption = 'ID Value of:'
      TabOrder = 2
      OnClick = RadioButtonClick
    end
    object selItemID: TIntSelector
      Left = 104
      Top = 58
      Width = 215
      Height = 24
    end
  end
  object srcItems: TDataSource
    DataSet = dmDatabase.items
    Left = 16
    Top = 272
  end
end
