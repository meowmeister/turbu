inherited frmEBEditShop: TfrmEBEditShop
  Caption = 'Open Shop'
  ClientHeight = 539
  ClientWidth = 493
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 16
  inherited Panel1: TPanel
    Width = 493
    Height = 486
    object GroupBox1: TGroupBox
      Left = 8
      Top = 135
      Width = 477
      Height = 250
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Shop Contents'
      TabOrder = 0
      DesignSize = (
        477
        250)
      object lstShopContents: TListView
        Left = 10
        Top = 25
        Width = 207
        Height = 208
        Anchors = [akLeft, akTop, akBottom]
        Columns = <
          item
            Caption = 'Name'
            Width = 132
          end
          item
            Caption = 'Cost'
          end>
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
        OnDblClick = btnRemoveClick
      end
      object btnAdd: TButton
        Left = 228
        Top = 83
        Width = 50
        Height = 25
        Caption = '&<<'
        TabOrder = 1
        OnClick = btnAddClick
      end
      object btnRemove: TButton
        Left = 228
        Top = 139
        Width = 50
        Height = 25
        Caption = '&>>'
        TabOrder = 2
        OnClick = btnRemoveClick
      end
      object lstItems: TRpgListGrid
        Left = 287
        Top = 25
        Width = 180
        Height = 208
        Anchors = [akLeft, akTop, akBottom]
        DataSource = srcItems
        TabOrder = 3
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -13
        TitleFont.Name = 'Tahoma'
        TitleFont.Style = []
        OnDblClick = btnAddClick
        Columns = <
          item
            Expanded = False
            FieldName = 'name'
            Title.Caption = 'Name'
            Width = 100
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'cost'
            Title.Caption = 'Cost'
            Width = 55
            Visible = True
          end>
      end
    end
    object GroupBox2: TGroupBox
      Left = 8
      Top = 391
      Width = 477
      Height = 66
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Shop Messages'
      TabOrder = 1
      object chkElseBlock: TCheckBox
        Left = 10
        Top = 32
        Width = 457
        Height = 17
        Caption = 'Create blocks for "transaction" and "no transaction"'
        TabOrder = 0
      end
    end
  end
  inherited btnOK: TButton
    Left = 210
    Top = 496
  end
  inherited btnCancel: TButton
    Left = 304
    Top = 496
  end
  inherited btnHelp: TButton
    Left = 397
    Top = 496
  end
  object radTransactions: TRadioGroup
    Left = 8
    Top = 8
    Width = 477
    Height = 49
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Transactions'
    Columns = 3
    ItemIndex = 0
    Items.Strings = (
      'Buy/Sell'
      'Buy'
      'Sell')
    TabOrder = 4
  end
  object grpMessage: TGroupBox
    Left = 8
    Top = 63
    Width = 477
    Height = 66
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Shop Messages'
    TabOrder = 5
    DesignSize = (
      477
      66)
    object cboStyles: TComboBox
      Left = 10
      Top = 26
      Width = 457
      Height = 24
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
  end
  object shopVocab: TClientDataSet
    Aggregates = <>
    FieldDefs = <>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    OnCalcFields = shopVocabCalcFields
    OnFilterRecord = shopVocabFilterRecord
    Left = 8
    Top = 492
    object shopVocabKey: TWideStringField
      FieldName = 'Key'
      Required = True
      Size = 32
    end
    object shopVocabVal: TWideStringField
      FieldName = 'Val'
      Required = True
      Size = 255
    end
    object shopVocabid: TIntegerField
      FieldKind = fkCalculated
      FieldName = 'id'
      Calculated = True
    end
  end
  object srcItems: TDataSource
    DataSet = dmDatabaseAux.itemNames
    Left = 88
    Top = 488
  end
end
