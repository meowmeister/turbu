inherited frmEBEditEquipment: TfrmEBEditEquipment
  Caption = 'Change Equipment'
  ClientHeight = 459
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 16
  inherited Panel1: TPanel
    Height = 406
    object grpOperation: TRadioGroup
      Left = 8
      Top = 142
      Width = 333
      Height = 51
      Caption = 'Operation'
      Columns = 2
      ItemIndex = 0
      Items.Strings = (
        'Equip'
        'Unequip')
      TabOrder = 1
      OnClick = RadioButtonClick
    end
    object grpItem: TGroupBox
      Left = 8
      Top = 198
      Width = 333
      Height = 88
      Caption = 'Item'
      TabOrder = 2
      object cboItemID: TIDLookupCombo
        Left = 104
        Top = 24
        Width = 215
        Height = 24
        KeyField = 'id'
        ListField = 'name'
        ListSource = srcEquipment
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
    object radSlot: TRadioGroup
      Left = 8
      Top = 292
      Width = 333
      Height = 105
      Caption = 'Slot'
      Columns = 3
      ItemIndex = 5
      Items.Strings = (
        'Weapon'
        'Shield'
        'Armor'
        'Helmet'
        'Relic'
        'All')
      TabOrder = 3
    end
  end
  inherited btnOK: TButton
    Top = 416
  end
  inherited btnCancel: TButton
    Top = 416
  end
  inherited btnHelp: TButton
    Top = 416
  end
  object srcEquipment: TDataSource
    DataSet = items_equipment
    Left = 16
    Top = 400
  end
  object items_equipment: TClientDataSet
    Active = True
    Aggregates = <>
    Filter = 'itemType in (1, 2)'
    Filtered = True
    FieldDefs = <
      item
        Name = 'id'
        DataType = ftInteger
      end
      item
        Name = 'name'
        DataType = ftWideString
        Size = 32
      end
      item
        Name = 'itemType'
        DataType = ftInteger
      end>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 88
    Top = 360
    Data = {
      4F0000009619E0BD0100000018000000030000000000030000004F0002696404
      00010000000000046E616D6501004A0000000100055749445448020002004000
      086974656D5479706504000100000000000000}
    object items_equipmentid: TIntegerField
      FieldName = 'id'
    end
    object items_equipmentname: TWideStringField
      FieldName = 'name'
      Size = 32
    end
    object items_equipmentitemType: TIntegerField
      FieldName = 'itemType'
    end
  end
end
