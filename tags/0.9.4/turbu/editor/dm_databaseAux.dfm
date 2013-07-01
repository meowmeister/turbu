object dmDatabaseAux: TdmDatabaseAux
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 461
  Width = 465
  object shields: TClientDataSet
    Active = True
    Aggregates = <>
    Filter = 'slot = 2'
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
      end
      item
        Name = 'usableByHero'
        DataType = ftBlob
      end
      item
        Name = 'usableByClass'
        DataType = ftBlob
      end
      item
        Name = 'slot'
        DataType = ftInteger
      end>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 16
    Top = 64
    Data = {
      B10000009619E0BD010000001800000006000000000003000000B10002696404
      00010000000000046E616D6501004A0000000100055749445448020002004000
      086974656D5479706504000100000000000C757361626C6542794865726F0400
      4B0000000100075355425459504502004900070042696E617279000D75736162
      6C654279436C61737304004B0000000100075355425459504502004900070042
      696E6172790004736C6F7404000100000000000000}
    object shieldsid: TIntegerField
      FieldName = 'id'
    end
    object shieldsname: TWideStringField
      FieldName = 'name'
      Size = 32
    end
    object shieldsitemType: TIntegerField
      FieldName = 'itemType'
    end
    object shieldsusableByHero: TBlobField
      FieldName = 'usableByHero'
    end
    object shieldsusableByClass: TBlobField
      FieldName = 'usableByClass'
    end
    object shieldsslot: TIntegerField
      FieldName = 'slot'
    end
  end
  object armors: TClientDataSet
    Active = True
    Aggregates = <>
    Filter = 'slot = 3'
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
      end
      item
        Name = 'usableByHero'
        DataType = ftBlob
      end
      item
        Name = 'usableByClass'
        DataType = ftBlob
      end
      item
        Name = 'slot'
        DataType = ftInteger
      end>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 16
    Top = 168
    Data = {
      B10000009619E0BD010000001800000006000000000003000000B10002696404
      00010000000000046E616D6501004A0000000100055749445448020002004000
      086974656D5479706504000100000000000C757361626C6542794865726F0400
      4B0000000100075355425459504502004900070042696E617279000D75736162
      6C654279436C61737304004B0000000100075355425459504502004900070042
      696E6172790004736C6F7404000100000000000000}
    object armorsid: TIntegerField
      FieldName = 'id'
    end
    object armorsname: TWideStringField
      FieldName = 'name'
      Size = 32
    end
    object armorsitemType: TIntegerField
      FieldName = 'itemType'
    end
    object armorsusableByHero: TBlobField
      FieldName = 'usableByHero'
    end
    object armorsusableByClass: TBlobField
      FieldName = 'usableByClass'
    end
    object armorsslot: TIntegerField
      FieldName = 'slot'
    end
  end
  object helmets: TClientDataSet
    Active = True
    Aggregates = <>
    Filter = 'slot = 4'
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
      end
      item
        Name = 'usableByHero'
        DataType = ftBlob
      end
      item
        Name = 'usableByClass'
        DataType = ftBlob
      end
      item
        Name = 'slot'
        DataType = ftInteger
      end>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 16
    Top = 224
    Data = {
      B10000009619E0BD010000001800000006000000000003000000B10002696404
      00010000000000046E616D6501004A0000000100055749445448020002004000
      086974656D5479706504000100000000000C757361626C6542794865726F0400
      4B0000000100075355425459504502004900070042696E617279000D75736162
      6C654279436C61737304004B0000000100075355425459504502004900070042
      696E6172790004736C6F7404000100000000000000}
    object helmetsid: TIntegerField
      FieldName = 'id'
    end
    object helmetsname: TWideStringField
      FieldName = 'name'
      Size = 32
    end
    object helmetsitemType: TIntegerField
      FieldName = 'itemType'
    end
    object helmetsusableByHero: TBlobField
      FieldName = 'usableByHero'
    end
    object helmetsusableByClass: TBlobField
      FieldName = 'usableByClass'
    end
    object helmetsslot: TIntegerField
      FieldName = 'slot'
    end
  end
  object accessories: TClientDataSet
    Active = True
    Aggregates = <>
    Filter = 'slot = 5'
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
      end
      item
        Name = 'usableByHero'
        DataType = ftBlob
      end
      item
        Name = 'usableByClass'
        DataType = ftBlob
      end
      item
        Name = 'slot'
        DataType = ftInteger
      end>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 16
    Top = 280
    Data = {
      B10000009619E0BD010000001800000006000000000003000000B10002696404
      00010000000000046E616D6501004A0000000100055749445448020002004000
      086974656D5479706504000100000000000C757361626C6542794865726F0400
      4B0000000100075355425459504502004900070042696E617279000D75736162
      6C654279436C61737304004B0000000100075355425459504502004900070042
      696E6172790004736C6F7404000100000000000000}
    object accessoriesid: TIntegerField
      FieldName = 'id'
    end
    object accessoriesname: TWideStringField
      FieldName = 'name'
      Size = 32
    end
    object accessoriesitemType: TIntegerField
      FieldName = 'itemType'
    end
    object accessoriesusableByHero: TBlobField
      FieldName = 'usableByHero'
    end
    object accessoriesusableByClass: TBlobField
      FieldName = 'usableByClass'
    end
    object accessoriesslot: TIntegerField
      FieldName = 'slot'
    end
  end
  object weapons: TClientDataSet
    Active = True
    Aggregates = <>
    Filter = 'itemType = 1'
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
      end
      item
        Name = 'usableByHero'
        DataType = ftBlob
      end
      item
        Name = 'usableByClass'
        DataType = ftBlob
      end
      item
        Name = 'twoHanded'
        DataType = ftBoolean
      end>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 16
    Top = 8
    Data = {
      B60000009619E0BD010000001800000006000000000003000000B60002696404
      00010000000000046E616D6501004A0000000100055749445448020002004000
      086974656D5479706504000100000000000C757361626C6542794865726F0400
      4B0000000100075355425459504502004900070042696E617279000D75736162
      6C654279436C61737304004B0000000100075355425459504502004900070042
      696E617279000974776F48616E64656402000300000000000000}
    object IntegerField4: TIntegerField
      FieldName = 'id'
    end
    object StringField1: TWideStringField
      FieldName = 'name'
      Size = 32
    end
    object weaponsIntegerField: TIntegerField
      FieldName = 'itemType'
    end
    object BytesField1: TBlobField
      FieldName = 'usableByHero'
    end
    object BytesField2: TBlobField
      FieldName = 'usableByClass'
    end
    object weaponsTwoHanded: TBooleanField
      FieldName = 'twoHanded'
    end
  end
  object offhands: TClientDataSet
    Active = True
    Aggregates = <>
    Filter = '(slot = 2) or (itemType = 1)'
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
      end
      item
        Name = 'usableByHero'
        DataType = ftBlob
      end
      item
        Name = 'usableByClass'
        DataType = ftBlob
      end
      item
        Name = 'twoHanded'
        DataType = ftBoolean
      end
      item
        Name = 'slot'
        DataType = ftInteger
      end>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 16
    Top = 120
    Data = {
      C30000009619E0BD010000001800000007000000000003000000C30002696404
      00010000000000046E616D6501004A0000000100055749445448020002004000
      086974656D5479706504000100000000000C757361626C6542794865726F0400
      4B0000000100075355425459504502004900070042696E617279000D75736162
      6C654279436C61737304004B0000000100075355425459504502004900070042
      696E617279000974776F48616E646564020003000000000004736C6F74040001
      00000000000000}
    object IntegerField52: TIntegerField
      FieldName = 'id'
    end
    object StringField5: TWideStringField
      FieldName = 'name'
      Size = 32
    end
    object offhandsitemType: TIntegerField
      FieldName = 'itemType'
    end
    object BytesField5: TBlobField
      FieldName = 'usableByHero'
    end
    object BytesField6: TBlobField
      FieldName = 'usableByClass'
    end
    object BooleanField22: TBooleanField
      FieldName = 'twoHanded'
    end
    object offhandsIntegerField: TIntegerField
      FieldName = 'slot'
    end
  end
  object itemNames: TSimpleDataSet
    Aggregates = <>
    Connection = dmDatabase.Connection
    DataSet.CommandText = 
      'select id, name, cost, itemType, usableByHero, usableByClass, tw' +
      'oHanded, slot from items'
    DataSet.MaxBlobSize = -1
    DataSet.Params = <>
    IndexFieldNames = 'id'
    Params = <>
    AfterOpen = itemNamesAfterOpen
    Left = 16
    Top = 336
    object IntegerField57: TIntegerField
      FieldName = 'id'
    end
    object WideStringField20: TWideStringField
      FieldName = 'name'
      Size = 32
    end
    object itemNamesitemType: TIntegerField
      FieldName = 'itemType'
      Required = True
    end
    object IntegerField31: TIntegerField
      FieldName = 'cost'
      Required = True
    end
    object itemNamesusableByHero: TBlobField
      FieldName = 'usableByHero'
    end
    object itemNamesusableByClass: TBlobField
      FieldName = 'usableByClass'
    end
    object itemNamestwoHanded: TBooleanField
      FieldName = 'twoHanded'
    end
    object itemNamesslot: TIntegerField
      FieldName = 'slot'
    end
  end
  object animNames: TSimpleDataSet
    Aggregates = <>
    Connection = dmDatabase.Connection
    DataSet.CommandText = 'select id, name from animations'
    DataSet.MaxBlobSize = -1
    DataSet.Params = <>
    IndexFieldNames = 'id'
    Params = <>
    Left = 267
    Top = 62
    object animNamesid: TIntegerField
      FieldName = 'id'
    end
    object animNamesname: TWideStringField
      FieldName = 'name'
      Size = 32
    end
  end
  object srcAnimName: TDataSource
    DataSet = animNames
    Left = 339
    Top = 78
  end
  object srcWeapons: TDataSource
    DataSet = weapons
    Left = 91
    Top = 22
  end
  object srcShields: TDataSource
    DataSet = shields
    Left = 91
    Top = 78
  end
  object srcOffhands: TDataSource
    DataSet = offhands
    Left = 91
    Top = 134
  end
  object srcArmors: TDataSource
    DataSet = armors
    Left = 83
    Top = 190
  end
  object srcHelmets: TDataSource
    DataSet = helmets
    Left = 83
    Top = 246
  end
  object srcAccessories: TDataSource
    DataSet = accessories
    Left = 83
    Top = 302
  end
  object condNames: TSimpleDataSet
    Aggregates = <>
    Connection = dmDatabase.Connection
    DataSet.CommandText = 'select name, id from conditions'
    DataSet.MaxBlobSize = -1
    DataSet.Params = <>
    FieldDefs = <>
    IndexDefs = <>
    IndexFieldNames = 'id'
    Params = <>
    StoreDefs = True
    Left = 264
    Top = 8
    object condNamesId: TIntegerField
      FieldName = 'id'
      Required = True
    end
    object condNamesName: TWideStringField
      FieldName = 'name'
      Required = True
      Size = 32
    end
  end
  object srcCondNames: TDataSource
    DataSet = condNames
    Left = 339
    Top = 30
  end
  object attribNames: TSimpleDataSet
    Aggregates = <>
    Connection = dmDatabase.Connection
    DataSet.CommandText = 'select name, id from attributes'
    DataSet.MaxBlobSize = -1
    DataSet.Params = <>
    IndexFieldNames = 'id'
    Params = <>
    Left = 264
    Top = 120
    object attribNamesid: TIntegerField
      FieldName = 'id'
      Required = True
    end
    object attribNamesname: TWideStringField
      FieldName = 'name'
      Required = True
      Size = 32
    end
  end
  object srcAttribNames: TDataSource
    DataSet = attribNames
    Left = 339
    Top = 138
  end
  object charClasses_Resists: TClientDataSet
    Aggregates = <>
    Filtered = True
    FieldDefs = <
      item
        Name = 'master'
        Attributes = [faRequired]
        DataType = ftInteger
      end
      item
        Name = 'x'
        Attributes = [faRequired]
        DataType = ftInteger
      end
      item
        Name = 'y'
        Attributes = [faRequired]
        DataType = ftInteger
      end>
    IndexDefs = <
      item
        Name = 'DEFAULT_ORDER'
      end
      item
        Name = 'CHANGEINDEX'
      end>
    IndexFieldNames = 'master'
    MasterFields = 'id'
    MasterSource = dmDatabase.dsCharClasses
    ObjectView = False
    PacketRecords = 0
    Params = <>
    StoreDefs = True
    Left = 256
    Top = 224
    object charClasses_Resistsmaster: TIntegerField
      FieldName = 'master'
      Required = True
    end
    object charClasses_Resistsx: TIntegerField
      FieldName = 'x'
      Required = True
    end
    object charClasses_Resistsy: TIntegerField
      FieldName = 'y'
      Required = True
    end
    object charClasses_Resistsname: TWideStringField
      FieldKind = fkLookup
      FieldName = 'name'
      LookupDataSet = attribNames
      LookupKeyFields = 'id'
      LookupResultField = 'name'
      KeyFields = 'x'
      Size = 32
      Lookup = True
    end
  end
  object charClasses_Conditions: TClientDataSet
    Aggregates = <>
    Filtered = True
    FieldDefs = <
      item
        Name = 'master'
        Attributes = [faRequired]
        DataType = ftInteger
      end
      item
        Name = 'x'
        Attributes = [faRequired]
        DataType = ftInteger
      end
      item
        Name = 'y'
        Attributes = [faRequired]
        DataType = ftInteger
      end>
    IndexDefs = <
      item
        Name = 'DEFAULT_ORDER'
      end
      item
        Name = 'CHANGEINDEX'
      end>
    IndexFieldNames = 'master'
    MasterFields = 'id'
    MasterSource = dmDatabase.dsCharClasses
    ObjectView = False
    PacketRecords = 0
    Params = <>
    StoreDefs = True
    Left = 256
    Top = 280
    object IntegerField1: TIntegerField
      FieldName = 'master'
      Required = True
    end
    object IntegerField2: TIntegerField
      FieldName = 'x'
      Required = True
    end
    object IntegerField3: TIntegerField
      FieldName = 'y'
      Required = True
    end
    object charClasses_Conditionsname: TWideStringField
      FieldKind = fkLookup
      FieldName = 'name'
      LookupDataSet = condNames
      LookupKeyFields = 'id'
      LookupResultField = 'name'
      KeyFields = 'x'
      Size = 32
      Lookup = True
    end
  end
  object MPartyNames: TSimpleDataSet
    Aggregates = <>
    Connection = dmDatabase.Connection
    DataSet.CommandText = 'select name, id from mparties'
    DataSet.MaxBlobSize = -1
    DataSet.Params = <>
    IndexFieldNames = 'id'
    Params = <>
    Left = 264
    Top = 176
    object IntegerField5: TIntegerField
      FieldName = 'id'
      Required = True
    end
    object WideStringField1: TWideStringField
      FieldName = 'name'
      Required = True
      Size = 32
    end
  end
  object srcMPartyNames: TDataSource
    DataSet = MPartyNames
    Left = 339
    Top = 194
  end
  object terrainNames: TSimpleDataSet
    Aggregates = <>
    Connection = dmDatabase.Connection
    DataSet.CommandText = 'select name, id from terrain'
    DataSet.MaxBlobSize = -1
    DataSet.Params = <>
    FieldDefs = <>
    IndexDefs = <>
    IndexFieldNames = 'id'
    Params = <>
    StoreDefs = True
    Left = 256
    Top = 336
    object IntegerField6: TIntegerField
      FieldName = 'id'
      Required = True
    end
    object WideStringField2: TWideStringField
      FieldName = 'name'
      Required = True
      Size = 32
    end
  end
  object srcTerrainNames: TDataSource
    DataSet = terrainNames
    Left = 339
    Top = 322
  end
  object allVocab: TSimpleDataSet
    Aggregates = <>
    Connection = dmDatabase.Connection
    DataSet.CommandText = 'select key, val from allVocab'
    DataSet.MaxBlobSize = -1
    DataSet.Params = <>
    FieldDefs = <>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 256
    Top = 396
    object allVocabKey: TWideStringField
      FieldName = 'Key'
      Required = True
      Size = 32
    end
    object allVocabVal: TWideStringField
      FieldName = 'Val'
      Required = True
      Size = 255
    end
  end
end
