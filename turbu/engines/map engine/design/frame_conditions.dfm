object frameConditions: TframeConditions
  Left = 0
  Top = 0
  Width = 409
  Height = 401
  TabOrder = 0
  object gbxConditions: TGroupBox
    Left = 0
    Top = 0
    Width = 408
    Height = 397
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Conditions'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    object lblSwitch1On: TLabel
      Left = 345
      Top = 21
      Width = 31
      Height = 18
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = '- ON'
    end
    object lblSwitch2On: TLabel
      Left = 345
      Top = 57
      Width = 31
      Height = 18
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = '- ON'
    end
    object lblItemHeld: TLabel
      Left = 345
      Top = 231
      Width = 28
      Height = 18
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Held'
    end
    object lblHeroNeeded: TLabel
      Left = 343
      Top = 266
      Width = 53
      Height = 18
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'In Party'
    end
    object lblMinutes1: TLabel
      Left = 256
      Top = 301
      Width = 23
      Height = 18
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'min'
    end
    object lblSeconds1: TLabel
      Left = 376
      Top = 301
      Width = 22
      Height = 18
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'sec'
    end
    object lblMinutes2: TLabel
      Left = 256
      Top = 336
      Width = 23
      Height = 18
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'min'
      Transparent = False
    end
    object lblSeconds2: TLabel
      Left = 374
      Top = 336
      Width = 22
      Height = 18
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'sec'
    end
    object Label1: TLabel
      Left = 35
      Top = 367
      Width = 40
      Height = 18
      Caption = 'Script:'
    end
    object txtVar1: TDBEdit
      Left = 111
      Top = 89
      Width = 226
      Height = 26
      DataField = 'Variable1Name'
      DataSource = srcConditions
      TabOrder = 26
    end
    object txtSwitch1: TDBEdit
      Left = 112
      Top = 24
      Width = 226
      Height = 26
      DataField = 'Switch1Name'
      DataSource = srcConditions
      TabOrder = 22
    end
    object chkSwitch1: TDBCheckBox
      Left = 14
      Top = 20
      Width = 71
      Height = 22
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Switch'
      DataField = 'bSwitch1'
      DataSource = srcConditions
      TabOrder = 0
      ValueChecked = 'True'
      ValueUnchecked = 'False'
      OnClick = chkSwitch1Click
    end
    object chkSwitch2: TDBCheckBox
      Left = 14
      Top = 56
      Width = 71
      Height = 22
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Switch'
      DataField = 'bSwitch2'
      DataSource = srcConditions
      TabOrder = 1
      ValueChecked = 'True'
      ValueUnchecked = 'False'
      OnClick = chkSwitch2Click
    end
    object chkVar1: TDBCheckBox
      Left = 14
      Top = 91
      Width = 87
      Height = 22
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Variable'
      DataField = 'bVar1'
      DataSource = srcConditions
      TabOrder = 2
      ValueChecked = 'True'
      ValueUnchecked = 'False'
      OnClick = chkVar1Click
    end
    object spnVar1: TJvDBSpinEdit
      Left = 109
      Top = 124
      Width = 112
      Height = 26
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      ButtonKind = bkStandard
      Enabled = False
      TabOrder = 3
      DataField = 'VarValue1'
      DataSource = srcConditions
    end
    object cboVarOp1: TDBIndexComboBox
      Left = 344
      Top = 89
      Width = 54
      Height = 26
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      DataField = 'Var1Op'
      DataSource = srcConditions
      Items.Strings = (
        '>='
        '>'
        '='
        '<'
        '<='
        '<>')
      TabOrder = 4
    end
    object chkItem: TDBCheckBox
      Left = 14
      Top = 230
      Width = 71
      Height = 22
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Item'
      DataField = 'bItem'
      DataSource = srcConditions
      TabOrder = 5
      ValueChecked = 'True'
      ValueUnchecked = 'False'
      OnClick = chkItemClick
    end
    object chkHero: TDBCheckBox
      Left = 14
      Top = 265
      Width = 71
      Height = 22
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Hero'
      DataField = 'bHero'
      DataSource = srcConditions
      TabOrder = 6
      ValueChecked = 'True'
      ValueUnchecked = 'False'
      OnClick = chkHeroClick
    end
    object chkVar2: TDBCheckBox
      Left = 14
      Top = 159
      Width = 87
      Height = 22
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Variable'
      DataField = 'bVar2'
      DataSource = srcConditions
      TabOrder = 7
      ValueChecked = 'True'
      ValueUnchecked = 'False'
      OnClick = chkVar2Click
    end
    object spnVar2: TJvDBSpinEdit
      Left = 109
      Top = 190
      Width = 112
      Height = 26
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      ButtonKind = bkStandard
      Enabled = False
      TabOrder = 8
      DataField = 'VarValue2'
      DataSource = srcConditions
    end
    object chkTimer1: TDBCheckBox
      Left = 14
      Top = 300
      Width = 71
      Height = 22
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Timer'
      DataField = 'bTimer1'
      DataSource = srcConditions
      TabOrder = 9
      ValueChecked = 'True'
      ValueUnchecked = 'False'
      OnClick = chkTimer1Click
    end
    object spnSeconds1: TJvDBSpinEdit
      Left = 294
      Top = 298
      Width = 74
      Height = 26
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      ButtonKind = bkStandard
      MaxValue = 59.000000000000000000
      Enabled = False
      TabOrder = 10
      DataField = 'Clock1Secs'
      DataSource = srcConditions
    end
    object spnMinutes1: TJvDBSpinEdit
      Left = 172
      Top = 298
      Width = 74
      Height = 26
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      ButtonKind = bkStandard
      MaxValue = 119.000000000000000000
      Enabled = False
      TabOrder = 11
      DataField = 'Clock1Mins'
      DataSource = srcConditions
    end
    object chkTimer2: TDBCheckBox
      Left = 14
      Top = 335
      Width = 71
      Height = 22
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Timer'
      DataField = 'bTimer2'
      DataSource = srcConditions
      TabOrder = 12
      ValueChecked = 'True'
      ValueUnchecked = 'False'
      OnClick = chkTimer2Click
    end
    object spnMinutes2: TJvDBSpinEdit
      Left = 172
      Top = 333
      Width = 74
      Height = 26
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      ButtonKind = bkStandard
      MaxValue = 119.000000000000000000
      Enabled = False
      TabOrder = 13
      DataField = 'Clock2Mins'
      DataSource = srcConditions
    end
    object spnSeconds2: TJvDBSpinEdit
      Left = 294
      Top = 333
      Width = 74
      Height = 26
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      ButtonKind = bkStandard
      MaxValue = 59.000000000000000000
      Enabled = False
      TabOrder = 14
      DataField = 'Clock2Secs'
      DataSource = srcConditions
    end
    object cboVarOp2: TDBIndexComboBox
      Left = 344
      Top = 157
      Width = 54
      Height = 26
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      DataField = 'Var2Op'
      DataSource = srcConditions
      Items.Strings = (
        '>='
        '>'
        '='
        '<'
        '<='
        '<>')
      TabOrder = 15
    end
    object cboItem: TDBLookupComboBox
      Left = 109
      Top = 228
      Width = 226
      Height = 26
      DataField = 'itemName'
      DataSource = srcConditions
      TabOrder = 16
    end
    object cboHero: TDBLookupComboBox
      Left = 109
      Top = 263
      Width = 226
      Height = 26
      DataField = 'HeroName'
      DataSource = srcConditions
      TabOrder = 17
    end
    object cboTimerOp1: TDBIndexComboBox
      Left = 109
      Top = 298
      Width = 54
      Height = 26
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      DataField = 'Clock1Op'
      DataSource = srcConditions
      Items.Strings = (
        '>='
        '>'
        '='
        '<'
        '<='
        '<>')
      TabOrder = 18
    end
    object cboTimerOp2: TDBIndexComboBox
      Left = 109
      Top = 333
      Width = 54
      Height = 26
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      DataField = 'Clock2Op'
      DataSource = srcConditions
      Items.Strings = (
        '>='
        '>'
        '='
        '<'
        '<='
        '<>')
      TabOrder = 19
    end
    object DBLookupComboBox5: TDBLookupComboBox
      Left = 109
      Top = 365
      Width = 226
      Height = 26
      DataField = 'ScriptName'
      DataSource = srcConditions
      TabOrder = 20
    end
    object btnSwitch1: TButton
      Left = 312
      Top = 25
      Width = 22
      Height = 22
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = '...'
      Enabled = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 21
      OnClick = btnSwitch1Click
    end
    object txtSwitch2: TDBEdit
      Left = 112
      Top = 54
      Width = 226
      Height = 26
      DataField = 'Switch2Name'
      DataSource = srcConditions
      TabOrder = 24
    end
    object btnSwitch2: TButton
      Left = 312
      Top = 55
      Width = 22
      Height = 22
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = '...'
      Enabled = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 23
      OnClick = btnSwitch2Click
    end
    object btnVar1: TButton
      Left = 311
      Top = 90
      Width = 22
      Height = 22
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = '...'
      Enabled = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 25
      OnClick = btnVar1Click
    end
  end
  object txtVar2: TDBEdit
    Left = 111
    Top = 159
    Width = 226
    Height = 24
    DataField = 'Variable2Name'
    DataSource = srcConditions
    TabOrder = 1
  end
  object btnVar2: TButton
    Left = 311
    Top = 160
    Width = 22
    Height = 22
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = '...'
    Enabled = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
    OnClick = btnVar2Click
  end
  object dsConditions: TClientDataSet
    Active = True
    Aggregates = <>
    FieldDefs = <
      item
        Name = 'Master'
        DataType = ftInteger
      end
      item
        Name = 'switch1'
        DataType = ftInteger
      end
      item
        Name = 'switch2'
        DataType = ftInteger
      end
      item
        Name = 'variable1'
        DataType = ftInteger
      end
      item
        Name = 'variable2'
        DataType = ftInteger
      end
      item
        Name = 'Var1Op'
        DataType = ftByte
      end
      item
        Name = 'Var2Op'
        DataType = ftByte
      end
      item
        Name = 'VarValue1'
        DataType = ftInteger
      end
      item
        Name = 'VarValue2'
        DataType = ftInteger
      end
      item
        Name = 'Item'
        DataType = ftInteger
      end
      item
        Name = 'Hero'
        DataType = ftInteger
      end
      item
        Name = 'Clock1Mins'
        DataType = ftInteger
      end
      item
        Name = 'Clock1Secs'
        DataType = ftInteger
      end
      item
        Name = 'Clock1Op'
        DataType = ftByte
      end
      item
        Name = 'Clock2Mins'
        DataType = ftInteger
      end
      item
        Name = 'Clock2Secs'
        DataType = ftInteger
      end
      item
        Name = 'Clock2Op'
        DataType = ftByte
      end
      item
        Name = 'Script'
        DataType = ftWideString
        Size = 255
      end
      item
        Name = 'bSwitch1'
        DataType = ftBoolean
      end
      item
        Name = 'bSwitch2'
        DataType = ftBoolean
      end
      item
        Name = 'bVar1'
        DataType = ftBoolean
      end
      item
        Name = 'bVar2'
        DataType = ftBoolean
      end
      item
        Name = 'bItem'
        DataType = ftBoolean
      end
      item
        Name = 'bHero'
        DataType = ftBoolean
      end
      item
        Name = 'bTimer1'
        DataType = ftBoolean
      end
      item
        Name = 'bTimer2'
        DataType = ftBoolean
      end>
    IndexDefs = <>
    IndexFieldNames = 'Master'
    Params = <>
    StoreDefs = True
    Left = 24
    Top = 184
    Data = {
      CC0100009619E0BD01000000180000001A000000000003000000CC01064D6173
      7465720400010000000000077377697463683104000100000000000773776974
      6368320400010000000000097661726961626C65310400010000000000097661
      726961626C6532040001000000000006566172314F7001000200000000000656
      6172324F7001000200000000000956617256616C756531040001000000000009
      56617256616C7565320400010000000000044974656D04000100000000000448
      65726F04000100000000000A436C6F636B314D696E7304000100000000000A43
      6C6F636B3153656373040001000000000008436C6F636B314F70010002000000
      00000A436C6F636B324D696E7304000100000000000A436C6F636B3253656373
      040001000000000008436C6F636B324F70010002000000000006536372697074
      02004A000000010005574944544802000200FE01086253776974636831020003
      0000000000086253776974636832020003000000000005625661723102000300
      00000000056256617232020003000000000005624974656D0200030000000000
      05624865726F0200030000000000076254696D65723102000300000000000762
      54696D65723202000300000000000000}
    object dsConditionsMaster: TIntegerField
      FieldName = 'Master'
    end
    object dsConditionsswitch1: TIntegerField
      FieldName = 'switch1'
    end
    object dsConditionsswitch2: TIntegerField
      FieldName = 'switch2'
    end
    object dsConditionsVariable1: TIntegerField
      FieldName = 'variable1'
    end
    object dsConditionsvariable2: TIntegerField
      FieldName = 'variable2'
    end
    object dsConditionsVar1Op: TByteField
      FieldName = 'Var1Op'
    end
    object dsConditionsVar2Op: TByteField
      FieldName = 'Var2Op'
    end
    object dsConditionsVarValue1: TIntegerField
      FieldName = 'VarValue1'
    end
    object dsConditionsVarValue2: TIntegerField
      FieldName = 'VarValue2'
    end
    object dsConditionsItem: TIntegerField
      FieldName = 'Item'
    end
    object dsConditionsHero: TIntegerField
      FieldName = 'Hero'
    end
    object dsConditionsClock1Mins: TIntegerField
      FieldName = 'Clock1Mins'
    end
    object dsConditionsClock1Secs: TIntegerField
      FieldName = 'Clock1Secs'
    end
    object dsConditionsClock1Op: TByteField
      FieldName = 'Clock1Op'
    end
    object dsConditionsClock2Mins: TIntegerField
      FieldName = 'Clock2Mins'
    end
    object dsConditionsClock2Secs: TIntegerField
      FieldName = 'Clock2Secs'
    end
    object dsConditionsClock2Op: TByteField
      FieldName = 'Clock2Op'
    end
    object dsConditionsScript: TWideStringField
      FieldName = 'Script'
      Size = 255
    end
    object dsConditionsbSwitch1: TBooleanField
      FieldName = 'bSwitch1'
    end
    object dsConditionsbSwitch2: TBooleanField
      FieldName = 'bSwitch2'
    end
    object dsConditionsbVar1: TBooleanField
      FieldName = 'bVar1'
    end
    object dsConditionsbVar2: TBooleanField
      FieldName = 'bVar2'
    end
    object dsConditionsbItem: TBooleanField
      FieldName = 'bItem'
    end
    object dsConditionsbHero: TBooleanField
      FieldName = 'bHero'
    end
    object dsConditionsbTimer1: TBooleanField
      FieldName = 'bTimer1'
    end
    object dsConditionsbTimer2: TBooleanField
      FieldName = 'bTimer2'
    end
    object dsConditionsSwitch1Name: TWideStringField
      DisplayWidth = 32
      FieldKind = fkLookup
      FieldName = 'Switch1Name'
      LookupDataSet = dmDatabase.Switches
      LookupKeyFields = 'id'
      LookupResultField = 'DisplayName'
      KeyFields = 'switch1'
      Size = 255
      Lookup = True
    end
    object dsConditionsSwitch2Name: TWideStringField
      DisplayWidth = 32
      FieldKind = fkLookup
      FieldName = 'Switch2Name'
      LookupDataSet = dmDatabase.Switches
      LookupKeyFields = 'id'
      LookupResultField = 'DisplayName'
      KeyFields = 'switch2'
      Size = 255
      Lookup = True
    end
    object dsConditionsVariable1Name: TWideStringField
      DisplayWidth = 32
      FieldKind = fkLookup
      FieldName = 'Variable1Name'
      LookupDataSet = dmDatabase.Variables
      LookupKeyFields = 'id'
      LookupResultField = 'DisplayName'
      KeyFields = 'Variable1'
      Size = 255
      Lookup = True
    end
    object dsConditionsVariable2Name: TWideStringField
      DisplayWidth = 32
      FieldKind = fkLookup
      FieldName = 'Variable2Name'
      LookupDataSet = dmDatabase.Variables
      LookupKeyFields = 'id'
      LookupResultField = 'DisplayName'
      KeyFields = 'Variable2'
      Size = 255
      Lookup = True
    end
    object dsConditionsitemName: TWideStringField
      FieldKind = fkLookup
      FieldName = 'itemName'
      LookupDataSet = dmDatabase.items
      LookupKeyFields = 'id'
      LookupResultField = 'name'
      KeyFields = 'Item'
      Size = 255
      Lookup = True
    end
    object dsConditionsHeroName: TWideStringField
      FieldKind = fkLookup
      FieldName = 'HeroName'
      LookupDataSet = dmDatabase.heroes
      LookupKeyFields = 'id'
      LookupResultField = 'name'
      KeyFields = 'hero'
      Size = 32
      Lookup = True
    end
    object dsConditionsScriptName: TWideStringField
      FieldKind = fkLookup
      FieldName = 'ScriptName'
      LookupDataSet = dmDatabase.scriptRange
      LookupKeyFields = 'name'
      LookupResultField = 'name'
      KeyFields = 'Script'
      Size = 255
      Lookup = True
    end
  end
  object srcConditions: TDataSource
    DataSet = dsConditions
    Left = 36
    Top = 114
  end
end
