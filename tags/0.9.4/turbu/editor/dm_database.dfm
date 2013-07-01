object dmDatabase: TdmDatabase
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 574
  Width = 892
  object charClasses: TSimpleDataSet
    Aggregates = <>
    Connection = Connection
    DataSet.CommandText = 'charClasses'
    DataSet.CommandType = ctTable
    DataSet.MaxBlobSize = -1
    DataSet.Params = <>
    DataSet.SortFieldNames = 'id'
    FieldDefs = <>
    IndexDefs = <
      item
        Name = 'DEFAULT_ORDER'
      end
      item
        Name = 'CHANGEINDEX'
      end
      item
        Name = 'charClassesIndexID'
        Fields = 'ID'
        Options = [ixUnique]
      end>
    IndexName = 'charClassesIndexID'
    Params = <>
    StoreDefs = True
    Left = 8
    Top = 8
    object charClassesid: TIntegerField
      FieldName = 'id'
      Required = True
    end
    object charClassesname: TWideStringField
      FieldName = 'name'
      Required = True
    end
    object charClassesmapSprite: TWideStringField
      DisplayWidth = 20
      FieldName = 'mapSprite'
      Required = True
      Size = 255
    end
    object charClassesActionMatrix: TIntegerField
      FieldName = 'actionMatrix'
      Required = True
    end
    object charClassesbattleSprite: TIntegerField
      FieldName = 'battleSprite'
      Required = True
    end
    object charClassesBattleMatrix: TIntegerField
      FieldName = 'battleMatrix'
      Required = True
    end
    object charClassesPortrait: TWideStringField
      DisplayWidth = 20
      FieldName = 'portrait'
      Required = True
      Size = 255
    end
    object charClassesPortraitIndex: TIntegerField
      FieldName = 'portraitIndex'
      Required = True
    end
    object charClassescommand1: TIntegerField
      FieldName = 'command_1'
      Required = True
    end
    object charClassescommand2: TIntegerField
      FieldName = 'command_2'
      Required = True
    end
    object charClassescommand3: TIntegerField
      FieldName = 'command_3'
      Required = True
    end
    object charClassescommand4: TIntegerField
      FieldName = 'command_4'
      Required = True
    end
    object charClassescommand5: TIntegerField
      FieldName = 'command_5'
      Required = True
    end
    object charClassescommand6: TIntegerField
      FieldName = 'command_6'
      Required = True
    end
    object charClassescommand7: TIntegerField
      FieldName = 'command_7'
      Required = True
    end
    object charClassescommands: TByteField
      FieldName = 'commands'
      Required = True
    end
    object charClassesstatblock1: TIntegerField
      FieldName = 'statblock_1'
      Required = True
    end
    object charClassesstatblock2: TIntegerField
      FieldName = 'statblock_2'
      Required = True
    end
    object charClassesstatblock3: TIntegerField
      FieldName = 'statblock_3'
      Required = True
    end
    object charClassesstatblock4: TIntegerField
      FieldName = 'statblock_4'
      Required = True
    end
    object charClassesstatblock5: TIntegerField
      FieldName = 'statblock_5'
      Required = True
    end
    object charClassesstatblock6: TIntegerField
      FieldName = 'statblock_6'
      Required = True
    end
    object charClassesexpFunc: TWideStringField
      FieldName = 'expFunc'
      Required = True
      Size = 32
    end
    object charClassesexpVars1: TIntegerField
      FieldName = 'expVars_1'
      Required = True
    end
    object charClassesexpVars2: TIntegerField
      FieldName = 'expVars_2'
      Required = True
    end
    object charClassesexpVars3: TIntegerField
      FieldName = 'expVars_3'
      Required = True
    end
    object charClassesexpVars0: TIntegerField
      FieldName = 'expVars_4'
      Required = True
    end
    object charClassesdualWield: TIntegerField
      FieldName = 'dualWield'
      Required = True
    end
    object charClassesstaticEq: TBooleanField
      FieldName = 'staticEq'
      Required = True
    end
    object charClassesstrongDef: TBooleanField
      FieldName = 'strongDef'
      Required = True
    end
    object charClassesunarmedAnim: TIntegerField
      FieldName = 'unarmedAnim'
      Required = True
    end
    object charClassesequip1: TIntegerField
      FieldName = 'equip_1'
      Required = True
    end
    object charClassesequip2: TIntegerField
      FieldName = 'equip_2'
      Required = True
    end
    object charClassesequip3: TIntegerField
      FieldName = 'equip_3'
      Required = True
    end
    object charClassesequip4: TIntegerField
      FieldName = 'equip_4'
      Required = True
    end
    object charClassesequip5: TIntegerField
      FieldName = 'equip_5'
      Required = True
    end
    object charClassesTranslucent: TBooleanField
      FieldName = 'translucent'
      Required = True
    end
    object charClassesGuest: TBooleanField
      FieldName = 'guest'
    end
    object charClassesBattlePos_X: TIntegerField
      FieldName = 'battlePos_X'
    end
    object charClassesbattlePos_Y: TIntegerField
      FieldName = 'battlePos_Y'
    end
  end
  object charClasses_skillset: TSimpleDataSet
    Aggregates = <>
    Connection = Connection
    DataSet.CommandText = 'charClasses_skillset'
    DataSet.CommandType = ctTable
    DataSet.MaxBlobSize = -1
    DataSet.Params = <>
    Filtered = True
    FieldDefs = <>
    IndexDefs = <
      item
        Name = 'DEFAULT_ORDER'
      end>
    IndexFieldNames = 'master'
    MasterFields = 'id'
    MasterSource = dsCharClasses
    PacketRecords = 0
    Params = <>
    StoreDefs = True
    OnCalcFields = charClasses_skillsetCalcFields
    Left = 8
    Top = 56
    object charClasses_skillsetmaster: TIntegerField
      FieldName = 'master'
      Required = True
    end
    object charClasses_skillsetid: TWideStringField
      FieldKind = fkCalculated
      FieldName = 'id'
      Size = 10
      Calculated = True
    end
    object charClasses_skillsetname: TWideStringField
      FieldKind = fkCalculated
      FieldName = 'name'
      Size = 32
      Calculated = True
    end
    object charClasses_skillsetstyle: TIntegerField
      FieldName = 'style'
      Required = True
    end
    object charClasses_skillsetnums1: TIntegerField
      FieldName = 'nums_1'
      Required = True
    end
    object charClasses_skillsetnums2: TIntegerField
      FieldName = 'nums_2'
      Required = True
    end
    object charClasses_skillsetnums3: TIntegerField
      FieldName = 'nums_3'
      Required = True
    end
    object charClasses_skillsetnums4: TIntegerField
      FieldName = 'nums_4'
      Required = True
    end
    object charClasses_skillsetmethodName: TWideStringField
      FieldName = 'method_methodName'
      Size = 32
    end
    object charClasses_skillsetmethod_DesignName: TWideStringField
      FieldName = 'method_DesignName'
      Size = 32
    end
    object charClasses_skillsetmethod_Signature: TWideStringField
      FieldName = 'method_Signature'
      Size = 255
    end
    object charClasses_skillsetmethod_strings_1: TWideStringField
      FieldName = 'method_strings_1'
      Size = 32
    end
    object charClasses_skillsetmethod_strings_2: TWideStringField
      FieldName = 'method_strings_2'
      Size = 32
    end
    object charClasses_skillsetmethod_strings_3: TWideStringField
      FieldName = 'method_strings_3'
      Size = 32
    end
    object charClasses_skillsetmethod_strings_4: TWideStringField
      FieldName = 'method_strings_4'
      Size = 32
    end
    object charClasses_skillsetskill: TIntegerField
      FieldName = 'skill'
      Required = True
    end
  end
  object charClasses_Resists: TSimpleDataSet
    Aggregates = <>
    Connection = Connection
    DataSet.CommandText = 'charClasses_Resists'
    DataSet.CommandType = ctTable
    DataSet.MaxBlobSize = -1
    DataSet.Params = <>
    Filtered = True
    FieldDefs = <>
    IndexDefs = <
      item
        Name = 'DEFAULT_ORDER'
      end
      item
        Name = 'CHANGEINDEX'
      end>
    IndexFieldNames = 'master'
    MasterFields = 'id'
    MasterSource = dsCharClasses
    PacketRecords = 0
    Params = <>
    StoreDefs = True
    Left = 8
    Top = 104
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
  end
  object charClasses_Conditions: TSimpleDataSet
    Aggregates = <>
    Connection = Connection
    DataSet.CommandText = 'charClasses_Conditions'
    DataSet.CommandType = ctTable
    DataSet.MaxBlobSize = -1
    DataSet.Params = <>
    Filtered = True
    FieldDefs = <>
    IndexDefs = <
      item
        Name = 'DEFAULT_ORDER'
      end
      item
        Name = 'CHANGEINDEX'
      end>
    IndexFieldNames = 'master'
    MasterFields = 'id'
    MasterSource = dsCharClasses
    PacketRecords = 0
    Params = <>
    StoreDefs = True
    Left = 8
    Top = 160
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
  end
  object animations: TSimpleDataSet
    Aggregates = <>
    Connection = Connection
    DataSet.CommandText = 'animations'
    DataSet.CommandType = ctTable
    DataSet.MaxBlobSize = -1
    DataSet.Params = <>
    FieldDefs = <>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 384
    Top = 16
    object animationsid: TIntegerField
      FieldName = 'id'
      Required = True
    end
    object animationsname: TWideStringField
      FieldName = 'name'
      Required = True
      Size = 32
    end
    object animationshitsAll: TBooleanField
      FieldName = 'hitsAll'
      Required = True
    end
    object animationsyTarget: TIntegerField
      FieldName = 'yTarget'
      Required = True
    end
    object animationsfilename: TWideStringField
      FieldName = 'filename'
      Required = True
      Size = 255
    end
  end
  object items: TSimpleDataSet
    Aggregates = <>
    Connection = Connection
    DataSet.CommandText = 'items'
    DataSet.CommandType = ctTable
    DataSet.MaxBlobSize = -1
    DataSet.Params = <>
    FieldDefs = <>
    IndexDefs = <
      item
        Name = 'itemsIndex1'
        Fields = 'id'
      end>
    IndexFieldNames = 'id'
    Params = <>
    StoreDefs = True
    AfterOpen = itemsAfterOpen
    Left = 184
    Top = 8
    object IntegerField30: TIntegerField
      FieldName = 'id'
      Required = True
    end
    object StringField3: TWideStringField
      FieldName = 'name'
      Required = True
      Size = 32
    end
    object itemsitemType: TIntegerField
      FieldName = 'itemType'
      Required = True
    end
    object StringField4: TWideStringField
      FieldName = 'description'
      Required = True
      Size = 50
    end
    object IntegerField31: TIntegerField
      FieldName = 'cost'
      Required = True
    end
    object itemstag1: TIntegerField
      FieldName = 'tag_1'
      Required = True
    end
    object itemstag2: TIntegerField
      FieldName = 'tag_2'
      Required = True
    end
    object itemstag3: TIntegerField
      FieldName = 'tag_3'
      Required = True
    end
    object itemstag4: TIntegerField
      FieldName = 'tag_4'
      Required = True
    end
    object IntegerField36: TIntegerField
      FieldName = 'usesLeft'
    end
    object IntegerField37: TIntegerField
      FieldName = 'usableWhere'
    end
    object BytesField3: TBlobField
      FieldName = 'usableByHero'
    end
    object BytesField4: TBlobField
      FieldName = 'usableByClass'
    end
    object BooleanField10: TBooleanField
      FieldName = 'evasion'
    end
    object IntegerField44: TIntegerField
      FieldName = 'toHit'
    end
    object IntegerField45: TIntegerField
      FieldName = 'critChance'
    end
    object IntegerField46: TIntegerField
      FieldName = 'critPrevent'
    end
    object IntegerField47: TIntegerField
      FieldName = 'preemptive'
    end
    object IntegerField48: TIntegerField
      FieldName = 'mpReduction'
    end
    object BooleanField11: TBooleanField
      FieldName = 'noTerrainDamage'
    end
    object itemsUsable: TBooleanField
      FieldName = 'usable'
    end
    object itemsConditions: TBlobField
      FieldName = 'conditions'
    end
    object BooleanField13: TBooleanField
      FieldName = 'cursed'
    end
    object BooleanField14: TBooleanField
      FieldName = 'twoHanded'
    end
    object BooleanField15: TBooleanField
      FieldName = 'attackTwice'
    end
    object BooleanField16: TBooleanField
      FieldName = 'areaHit'
    end
    object IntegerField49: TIntegerField
      FieldName = 'battleAnim'
    end
    object IntegerField50: TIntegerField
      FieldName = 'mpCost'
    end
    object IntegerField51: TIntegerField
      FieldName = 'conditionChance'
    end
    object itemsIntegerField: TIntegerField
      FieldName = 'slot'
    end
    object itemsareaMedicine: TBooleanField
      FieldName = 'areaMedicine'
    end
    object itemshpPercent: TIntegerField
      FieldName = 'hpPercent'
    end
    object itemsmpPercent: TIntegerField
      FieldName = 'mpPercent'
    end
    object itemsdeadHeroesOnly: TBooleanField
      FieldName = 'deadHeroesOnly'
    end
    object itemsskill: TIntegerField
      FieldName = 'skill'
    end
    object itemscustomSkillMessage: TBooleanField
      FieldName = 'customSkillMessage'
    end
    object itemswhich: TIntegerField
      FieldName = 'which'
    end
    object itemsmagnitude: TIntegerField
      FieldName = 'magnitude'
    end
    object itemsStyle: TIntegerField
      FieldName = 'style'
    end
    object itemsOperation: TIntegerField
      FieldName = 'operation'
    end
    object itemsevent: TWideStringField
      FieldName = 'event'
      Size = 32
    end
    object itemsinvokeSkill: TBooleanField
      FieldName = 'invokeSkill'
    end
    object itemsInflictReversed: TBooleanField
      FieldName = 'inflictReversed'
    end
    object itemsscript: TWideMemoField
      FieldName = 'script'
      BlobType = ftWideMemo
    end
    object itemsstat1: TIntegerField
      FieldName = 'stat_1'
    end
    object itemsstat2: TIntegerField
      FieldName = 'stat_2'
    end
    object itemsstat3: TIntegerField
      FieldName = 'stat_3'
    end
    object itemsstat4: TIntegerField
      FieldName = 'stat_4'
    end
    object itemsstat5: TIntegerField
      FieldName = 'stat_5'
    end
    object itemsstat6: TIntegerField
      FieldName = 'stat_6'
    end
  end
  object items_armor: TClientDataSet
    Active = True
    Aggregates = <>
    Filter = 'itemType = 2'
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
        Name = 'description'
        DataType = ftWideString
        Size = 50
      end
      item
        Name = 'cost'
        DataType = ftInteger
      end
      item
        Name = 'tag_1'
        DataType = ftInteger
      end
      item
        Name = 'tag_2'
        DataType = ftInteger
      end
      item
        Name = 'tag_3'
        DataType = ftInteger
      end
      item
        Name = 'tag_4'
        DataType = ftInteger
      end
      item
        Name = 'usesLeft'
        DataType = ftInteger
      end
      item
        Name = 'usableWhere'
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
        Name = 'stat_1'
        DataType = ftInteger
      end
      item
        Name = 'stat_2'
        DataType = ftInteger
      end
      item
        Name = 'stat_3'
        DataType = ftInteger
      end
      item
        Name = 'stat_4'
        DataType = ftInteger
      end
      item
        Name = 'stat_5'
        DataType = ftInteger
      end
      item
        Name = 'stat_6'
        DataType = ftInteger
      end
      item
        Name = 'evasion'
        DataType = ftBoolean
      end
      item
        Name = 'toHit'
        DataType = ftInteger
      end
      item
        Name = 'critChance'
        DataType = ftInteger
      end
      item
        Name = 'critPrevent'
        DataType = ftInteger
      end
      item
        Name = 'preemptive'
        DataType = ftInteger
      end
      item
        Name = 'mpReduction'
        DataType = ftInteger
      end
      item
        Name = 'noTerrainDamage'
        DataType = ftBoolean
      end
      item
        Name = 'usable'
        DataType = ftBoolean
      end
      item
        Name = 'cursed'
        DataType = ftBoolean
      end
      item
        Name = 'invokeSkill'
        DataType = ftBoolean
      end
      item
        Name = 'slot'
        DataType = ftInteger
      end>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    AfterOpen = restoreClone
    Left = 184
    Top = 168
    Data = {
      4B0200009619E0BD01000000180000001E0000000000030000004B0202696404
      00010000000000046E616D6501004A0000000100055749445448020002004000
      086974656D5479706504000100000000000B6465736372697074696F6E01004A
      000000010005574944544802000200640004636F737404000100000000000574
      61675F310400010000000000057461675F320400010000000000057461675F33
      0400010000000000057461675F34040001000000000008757365734C65667404
      000100000000000B757361626C65576865726504000100000000000C75736162
      6C6542794865726F04004B000000010007535542545950450200490007004269
      6E617279000D757361626C654279436C61737304004B00000001000753554254
      59504502004900070042696E6172790006737461745F31040001000000000006
      737461745F32040001000000000006737461745F330400010000000000067374
      61745F34040001000000000006737461745F3504000100000000000673746174
      5F3604000100000000000765766173696F6E020003000000000005746F486974
      04000100000000000A637269744368616E636504000100000000000B63726974
      50726576656E7404000100000000000A707265656D7074697665040001000000
      00000B6D70526564756374696F6E04000100000000000F6E6F5465727261696E
      44616D616765020003000000000006757361626C650200030000000000066375
      7273656402000300000000000B696E766F6B65536B696C6C0200030000000000
      04736C6F7404000100000000000000}
    object items_armorid: TIntegerField
      FieldName = 'id'
    end
    object items_armorname: TWideStringField
      FieldName = 'name'
      Size = 32
    end
    object items_armoritemType: TIntegerField
      FieldName = 'itemType'
    end
    object items_armordesc: TWideStringField
      FieldName = 'description'
      Size = 50
    end
    object items_armorcost: TIntegerField
      FieldName = 'cost'
    end
    object items_armortag1: TIntegerField
      FieldName = 'tag_1'
    end
    object items_armortag2: TIntegerField
      FieldName = 'tag_2'
    end
    object items_armortag3: TIntegerField
      FieldName = 'tag_3'
    end
    object items_armortag4: TIntegerField
      FieldName = 'tag_4'
    end
    object items_armorusesLeft: TIntegerField
      FieldName = 'usesLeft'
    end
    object items_armorusableWhere: TIntegerField
      FieldName = 'usableWhere'
    end
    object items_armorusableByHero: TBlobField
      FieldName = 'usableByHero'
    end
    object items_armorusableByClass: TBlobField
      FieldName = 'usableByClass'
    end
    object items_armorstat1: TIntegerField
      FieldName = 'stat_1'
    end
    object items_armorstat2: TIntegerField
      FieldName = 'stat_2'
    end
    object items_armorstat3: TIntegerField
      FieldName = 'stat_3'
    end
    object items_armorstat4: TIntegerField
      FieldName = 'stat_4'
    end
    object items_armorstat5: TIntegerField
      FieldName = 'stat_5'
    end
    object items_armorstat6: TIntegerField
      FieldName = 'stat_6'
    end
    object items_armorevasion: TBooleanField
      FieldName = 'evasion'
    end
    object items_armortoHit: TIntegerField
      FieldName = 'toHit'
    end
    object items_armorcritChance: TIntegerField
      FieldName = 'critChance'
    end
    object items_armorcritPrevent: TIntegerField
      FieldName = 'critPrevent'
    end
    object items_armorpreemptive: TIntegerField
      FieldName = 'preemptive'
    end
    object items_armormpReduction: TIntegerField
      FieldName = 'mpReduction'
    end
    object items_armornoTerrainDamage: TBooleanField
      FieldName = 'noTerrainDamage'
    end
    object items_armorusable: TBooleanField
      FieldName = 'usable'
    end
    object items_armorcursed: TBooleanField
      FieldName = 'cursed'
    end
    object items_armorinvokeSkill: TBooleanField
      FieldName = 'invokeSkill'
    end
    object items_armorslot: TIntegerField
      FieldName = 'slot'
    end
  end
  object items_attributes: TSimpleDataSet
    Aggregates = <>
    Connection = Connection
    DataSet.CommandText = 'items_attributes'
    DataSet.CommandType = ctTable
    DataSet.MaxBlobSize = -1
    DataSet.Params = <>
    Filtered = True
    FieldDefs = <>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 368
    Top = 496
    object IntegerField13: TIntegerField
      FieldName = 'master'
      Required = True
    end
    object IntegerField14: TIntegerField
      FieldName = 'x'
      Required = True
    end
    object IntegerField15: TIntegerField
      FieldName = 'y'
      Required = True
    end
  end
  object items_weapon: TClientDataSet
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
        Name = 'description'
        DataType = ftWideString
        Size = 50
      end
      item
        Name = 'cost'
        DataType = ftInteger
      end
      item
        Name = 'tag_1'
        DataType = ftInteger
      end
      item
        Name = 'tag_2'
        DataType = ftInteger
      end
      item
        Name = 'tag_3'
        DataType = ftInteger
      end
      item
        Name = 'tag_4'
        DataType = ftInteger
      end
      item
        Name = 'usesLeft'
        DataType = ftInteger
      end
      item
        Name = 'usableWhere'
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
        Name = 'stat_1'
        DataType = ftInteger
      end
      item
        Name = 'stat_2'
        DataType = ftInteger
      end
      item
        Name = 'stat_3'
        DataType = ftInteger
      end
      item
        Name = 'stat_4'
        DataType = ftInteger
      end
      item
        Name = 'stat_5'
        DataType = ftInteger
      end
      item
        Name = 'stat_6'
        DataType = ftInteger
      end
      item
        Name = 'evasion'
        DataType = ftBoolean
      end
      item
        Name = 'toHit'
        DataType = ftInteger
      end
      item
        Name = 'critChance'
        DataType = ftInteger
      end
      item
        Name = 'critPrevent'
        DataType = ftInteger
      end
      item
        Name = 'preemptive'
        DataType = ftInteger
      end
      item
        Name = 'mpReduction'
        DataType = ftInteger
      end
      item
        Name = 'noTerrainDamage'
        DataType = ftBoolean
      end
      item
        Name = 'usable'
        DataType = ftBoolean
      end
      item
        Name = 'cursed'
        DataType = ftBoolean
      end
      item
        Name = 'twoHanded'
        DataType = ftBoolean
      end
      item
        Name = 'attackTwice'
        DataType = ftBoolean
      end
      item
        Name = 'areaHit'
        DataType = ftBoolean
      end
      item
        Name = 'battleAnim'
        DataType = ftInteger
      end
      item
        Name = 'mpCost'
        DataType = ftInteger
      end
      item
        Name = 'invokeSkill'
        DataType = ftBoolean
      end
      item
        Name = 'conditionChance'
        DataType = ftInteger
      end>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    AfterOpen = restoreClone
    Left = 184
    Top = 112
    Data = {
      AE0200009619E0BD010000001800000023000000000003000000AE0202696404
      00010000000000046E616D6501004A0000000100055749445448020002004000
      086974656D5479706504000100000000000B6465736372697074696F6E01004A
      000000010005574944544802000200640004636F737404000100000000000574
      61675F310400010000000000057461675F320400010000000000057461675F33
      0400010000000000057461675F34040001000000000008757365734C65667404
      000100000000000B757361626C65576865726504000100000000000C75736162
      6C6542794865726F04004B000000010007535542545950450200490007004269
      6E617279000D757361626C654279436C61737304004B00000001000753554254
      59504502004900070042696E6172790006737461745F31040001000000000006
      737461745F32040001000000000006737461745F330400010000000000067374
      61745F34040001000000000006737461745F3504000100000000000673746174
      5F3604000100000000000765766173696F6E020003000000000005746F486974
      04000100000000000A637269744368616E636504000100000000000B63726974
      50726576656E7404000100000000000A707265656D7074697665040001000000
      00000B6D70526564756374696F6E04000100000000000F6E6F5465727261696E
      44616D616765020003000000000006757361626C650200030000000000066375
      7273656402000300000000000974776F48616E64656402000300000000000B61
      747461636B547769636502000300000000000761726561486974020003000000
      00000A626174746C65416E696D0400010000000000066D70436F737404000100
      000000000B696E766F6B65536B696C6C02000300000000000F636F6E64697469
      6F6E4368616E636504000100000000000000}
    object items_weaponid: TIntegerField
      FieldName = 'id'
    end
    object items_weaponname: TWideStringField
      FieldName = 'name'
      Size = 32
    end
    object items_weaponIntegerField: TIntegerField
      FieldName = 'itemType'
    end
    object items_weapondesc: TWideStringField
      FieldName = 'description'
      Size = 50
    end
    object items_weaponcost: TIntegerField
      FieldName = 'cost'
    end
    object items_weapontag1: TIntegerField
      FieldName = 'tag_1'
    end
    object items_weapontag2: TIntegerField
      FieldName = 'tag_2'
    end
    object items_weapontag3: TIntegerField
      FieldName = 'tag_3'
    end
    object items_weapontag4: TIntegerField
      FieldName = 'tag_4'
    end
    object items_weaponusesLeft: TIntegerField
      FieldName = 'usesLeft'
    end
    object items_weaponusableWhere: TIntegerField
      FieldName = 'usableWhere'
    end
    object items_weaponusableByChar: TBlobField
      FieldName = 'usableByHero'
    end
    object items_weaponusableByClass: TBlobField
      FieldName = 'usableByClass'
    end
    object items_weaponstat1: TIntegerField
      FieldName = 'stat_1'
    end
    object items_weaponstat2: TIntegerField
      FieldName = 'stat_2'
    end
    object items_weaponstat3: TIntegerField
      FieldName = 'stat_3'
    end
    object items_weaponstat4: TIntegerField
      FieldName = 'stat_4'
    end
    object items_weaponstat5: TIntegerField
      FieldName = 'stat_5'
    end
    object items_weaponstat6: TIntegerField
      FieldName = 'stat_6'
    end
    object items_weaponevasion: TBooleanField
      FieldName = 'evasion'
    end
    object items_weapontoHit: TIntegerField
      FieldName = 'toHit'
    end
    object items_weaponcritChance: TIntegerField
      FieldName = 'critChance'
    end
    object items_weaponcritPrevent: TIntegerField
      FieldName = 'critPrevent'
    end
    object items_weaponpreemptive: TIntegerField
      FieldName = 'preemptive'
    end
    object items_weaponmpReduction: TIntegerField
      FieldName = 'mpReduction'
    end
    object items_weaponnoTerrainDamage: TBooleanField
      FieldName = 'noTerrainDamage'
    end
    object items_weaponusable: TBooleanField
      FieldName = 'usable'
    end
    object items_weaponcursed: TBooleanField
      FieldName = 'cursed'
    end
    object items_weapontwoHanded: TBooleanField
      FieldName = 'twoHanded'
    end
    object items_weaponattackTwice: TBooleanField
      FieldName = 'attackTwice'
    end
    object items_weaponareaHit: TBooleanField
      FieldName = 'areaHit'
    end
    object items_weaponbattleAnim: TIntegerField
      FieldName = 'battleAnim'
    end
    object items_weaponmpCost: TIntegerField
      FieldName = 'mpCost'
    end
    object items_weaponinvokeSkill: TBooleanField
      FieldName = 'invokeSkill'
    end
    object items_weaponconditionChance: TIntegerField
      FieldName = 'conditionChance'
    end
  end
  object items_junk: TClientDataSet
    Active = True
    Aggregates = <>
    Filter = 'itemType = 0'
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
        Name = 'description'
        DataType = ftWideString
        Size = 50
      end
      item
        Name = 'cost'
        DataType = ftInteger
      end
      item
        Name = 'tag_1'
        DataType = ftInteger
      end
      item
        Name = 'tag_2'
        DataType = ftInteger
      end
      item
        Name = 'tag_3'
        DataType = ftInteger
      end
      item
        Name = 'tag_4'
        DataType = ftInteger
      end
      item
        Name = 'itemType'
        DataType = ftInteger
      end>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    AfterOpen = restoreClone
    Left = 184
    Top = 60
    Data = {
      B40000009619E0BD010000001800000009000000000003000000B40002696404
      00010000000000046E616D6501004A0000000100055749445448020002004000
      0B6465736372697074696F6E01004A0000000100055749445448020002006400
      04636F73740400010000000000057461675F310400010000000000057461675F
      320400010000000000057461675F330400010000000000057461675F34040001
      0000000000086974656D5479706504000100000000000000}
    object items_junkid: TIntegerField
      FieldName = 'id'
    end
    object items_junkname: TWideStringField
      FieldName = 'name'
      Size = 32
    end
    object items_junkdesc: TWideStringField
      FieldName = 'description'
      Size = 50
    end
    object items_junkcost: TIntegerField
      FieldName = 'cost'
    end
    object items_junktag1: TIntegerField
      FieldName = 'tag_1'
    end
    object items_junktag2: TIntegerField
      FieldName = 'tag_2'
    end
    object items_junktag3: TIntegerField
      FieldName = 'tag_3'
    end
    object items_junktag4: TIntegerField
      FieldName = 'tag_4'
    end
    object items_junkIntegerField: TIntegerField
      FieldName = 'itemType'
    end
  end
  object commands: TSimpleDataSet
    Aggregates = <>
    Connection = Connection
    DataSet.CommandText = 'commands'
    DataSet.CommandType = ctTable
    DataSet.MaxBlobSize = -1
    DataSet.Params = <>
    FieldDefs = <>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 72
    Top = 504
    object commandsid: TIntegerField
      FieldName = 'id'
      Required = True
    end
    object commandsname: TWideStringField
      FieldName = 'name'
      Required = True
      Size = 32
    end
    object commandsstyle: TIntegerField
      FieldName = 'style'
      Required = True
    end
    object commandsvalue: TIntegerField
      FieldName = 'val'
      Required = True
    end
  end
  object skills: TSimpleDataSet
    Aggregates = <>
    Connection = Connection
    DataSet.CommandText = 'skills'
    DataSet.CommandType = ctTable
    DataSet.MaxBlobSize = -1
    DataSet.Params = <>
    FieldDefs = <>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 256
    Top = 8
    object skillsid: TIntegerField
      FieldName = 'id'
      Required = True
    end
    object skillsname: TWideStringField
      FieldName = 'name'
      Required = True
      Size = 32
    end
    object skillscost: TIntegerField
      FieldName = 'cost'
      Required = True
    end
    object skillscostPercent: TBooleanField
      FieldName = 'costPercent'
      Required = True
    end
    object skillsdesc: TWideStringField
      FieldName = 'description'
      Required = True
      Size = 50
    end
    object skillsskillMessagesuseString: TWideStringField
      FieldName = 'useString'
      Required = True
      Size = 50
    end
    object skillsskillMessagesuseString2: TWideStringField
      FieldName = 'useString2'
      Required = True
      Size = 50
    end
    object skillsskillMessagesfailureMessage: TIntegerField
      FieldName = 'failureMessage'
      Required = True
    end
    object skillsusableWhere: TIntegerField
      FieldName = 'usableWhere'
      Required = True
    end
    object skillstag1: TIntegerField
      FieldName = 'tag_1'
      Required = True
    end
    object skillstag2: TIntegerField
      FieldName = 'tag_2'
      Required = True
    end
    object skillstag3: TIntegerField
      FieldName = 'tag_3'
      Required = True
    end
    object skillstag4: TIntegerField
      FieldName = 'tag_4'
      Required = True
    end
    object skillsrange: TByteField
      FieldName = 'range'
    end
    object skillsoffensive: TBooleanField
      FieldName = 'offensive'
    end
    object skillsanim: TWordField
      FieldName = 'anim'
    end
    object skillsSkillPower1: TIntegerField
      FieldName = 'SkillPower_1'
    end
    object skillsSkillPower2: TIntegerField
      FieldName = 'SkillPower_2'
    end
    object skillsSkillPower3: TIntegerField
      FieldName = 'SkillPower_3'
    end
    object skillsSkillPower4: TIntegerField
      FieldName = 'SkillPower_4'
    end
    object skillssuccessRate: TIntegerField
      FieldName = 'successRate'
    end
    object skillsstat1: TIntegerField
      FieldName = 'stat_1'
    end
    object skillsstat2: TIntegerField
      FieldName = 'stat_2'
    end
    object skillsstat3: TIntegerField
      FieldName = 'stat_3'
    end
    object skillsstat4: TIntegerField
      FieldName = 'stat_4'
    end
    object skillsstat5: TIntegerField
      FieldName = 'stat_5'
    end
    object skillsstat6: TIntegerField
      FieldName = 'stat_6'
    end
    object skillscondition: TBlobField
      FieldName = 'condition'
    end
    object skillsvampire: TBooleanField
      FieldName = 'vampire'
    end
    object skillsphased: TBooleanField
      FieldName = 'phased'
    end
    object skillsresistMod: TBooleanField
      FieldName = 'resistMod'
    end
    object skillstarget: TByteField
      FieldName = 'target'
    end
    object skillswhich: TWordField
      FieldName = 'which'
    end
    object skillsmagnitude: TSmallintField
      FieldName = 'magnitude'
    end
    object skillsstyle: TByteField
      FieldName = 'style'
    end
    object skillsoperation: TByteField
      FieldName = 'operation'
    end
    object skills_sfxid: TIntegerField
      FieldName = 'sfx_id'
    end
    object skills_sfxname: TWideStringField
      FieldName = 'sfx_name'
      Size = 255
    end
    object skills_sfxFadeIn: TIntegerField
      FieldName = 'sfx_fadeIn'
    end
    object skills_sfxTempo: TIntegerField
      FieldName = 'sfx_tempo'
    end
    object skills_sfxVolume: TIntegerField
      FieldName = 'sfx_volume'
    end
    object skills_sfxBalance: TIntegerField
      FieldName = 'sfx_Balance'
    end
    object skillsInflictReversed: TBooleanField
      FieldName = 'inflictReversed'
    end
    object skillsDisplaySprite: TIntegerField
      FieldName = 'displaySprite'
    end
  end
  object dsCharClasses: TDataSource
    DataSet = charClasses
    Left = 824
    Top = 24
  end
  object attributes: TSimpleDataSet
    Aggregates = <>
    Connection = Connection
    DataSet.CommandText = 'attributes'
    DataSet.CommandType = ctTable
    DataSet.MaxBlobSize = -1
    DataSet.Params = <>
    Params = <>
    Left = 456
    Top = 8
    object attributesid: TIntegerField
      FieldName = 'id'
      Required = True
    end
    object attributesname: TWideStringField
      FieldName = 'name'
      Required = True
      Size = 32
    end
    object attributesrequiredForSkills: TBooleanField
      FieldName = 'requiredForSkills'
      Required = True
    end
    object attributesstandard_1: TIntegerField
      FieldName = 'standard_1'
      Required = True
    end
    object attributesstandard_2: TIntegerField
      FieldName = 'standard_2'
      Required = True
    end
    object attributesstandard_3: TIntegerField
      FieldName = 'standard_3'
      Required = True
    end
    object attributesstandard_4: TIntegerField
      FieldName = 'standard_4'
      Required = True
    end
    object attributesstandard_5: TIntegerField
      FieldName = 'standard_5'
      Required = True
    end
  end
  object conditions: TSimpleDataSet
    Aggregates = <>
    Connection = Connection
    DataSet.CommandText = 'conditions'
    DataSet.CommandType = ctTable
    DataSet.MaxBlobSize = -1
    DataSet.Params = <>
    FieldDefs = <>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 456
    Top = 64
    object conditionsId: TIntegerField
      FieldName = 'id'
      Required = True
    end
    object conditionsName: TWideStringField
      FieldName = 'name'
      Required = True
      Size = 32
    end
    object conditionsOutOfBattle: TBooleanField
      FieldName = 'outOfBattle'
      Required = True
    end
    object conditionsColor: TIntegerField
      FieldName = 'color'
      Required = True
    end
    object conditionsPriority: TIntegerField
      FieldName = 'priority'
      Required = True
    end
    object conditionsAttackLimit: TByteField
      FieldName = 'attackLimit'
      Required = True
    end
    object conditionsHealTurns: TIntegerField
      FieldName = 'healTurns'
      Required = True
    end
    object conditionsHealPercent: TIntegerField
      FieldName = 'healPercent'
      Required = True
    end
    object conditionsHealShock: TIntegerField
      FieldName = 'healShock'
      Required = True
    end
    object conditionsAttackStat: TBooleanField
      FieldName = 'attackStat'
      Required = True
    end
    object conditionsDefenseStat: TBooleanField
      FieldName = 'defenseStat'
      Required = True
    end
    object conditionsMindStat: TBooleanField
      FieldName = 'mindStat'
      Required = True
    end
    object conditionsSpeedStat: TBooleanField
      FieldName = 'speedStat'
      Required = True
    end
    object conditionsToHitChange: TIntegerField
      FieldName = 'toHitChange'
      Required = True
    end
    object conditionsPhysBlock: TBooleanField
      FieldName = 'physBlock'
      Required = True
    end
    object conditionsPhysCutoff: TIntegerField
      FieldName = 'physCutoff'
      Required = True
    end
    object conditionsMagBlock: TBooleanField
      FieldName = 'magBlock'
      Required = True
    end
    object conditionsMagCutoff: TIntegerField
      FieldName = 'magCutoff'
      Required = True
    end
    object conditionsUsesConditionMessages: TBooleanField
      FieldName = 'usesConditionMessages'
      Required = True
    end
    object conditionsconditionMessages_1: TWideStringField
      FieldName = 'conditionMessages_1'
      Size = 50
    end
    object conditionsconditionMessages_2: TWideStringField
      FieldName = 'conditionMessages_2'
      Size = 50
    end
    object conditionsconditionMessages_3: TWideStringField
      FieldName = 'conditionMessages_3'
      Size = 50
    end
    object conditionsconditionMessages_4: TWideStringField
      FieldName = 'conditionMessages_4'
      Size = 50
    end
    object conditionsConditionMessages_5: TWideStringField
      FieldName = 'conditionMessages_5'
      Size = 50
    end
    object conditionsHpTurnPercent: TIntegerField
      FieldName = 'hpTurnPercent'
      Required = True
    end
    object conditionsHpTurnFixed: TIntegerField
      FieldName = 'hpTurnFixed'
      Required = True
    end
    object conditionsHpStepCount: TIntegerField
      FieldName = 'hpStepCount'
      Required = True
    end
    object conditionsHpStepQuantity: TIntegerField
      FieldName = 'hpStepQuantity'
      Required = True
    end
    object conditionsMpTurnPercent: TIntegerField
      FieldName = 'mpTurnPercent'
      Required = True
    end
    object conditionsMpTurnFixed: TIntegerField
      FieldName = 'mpTurnFixed'
      Required = True
    end
    object conditionsMpStepCount: TIntegerField
      FieldName = 'mpStepCount'
      Required = True
    end
    object conditionsMpStepQuantity: TIntegerField
      FieldName = 'mpStepQuantity'
      Required = True
    end
    object conditionsStatEffect: TByteField
      FieldName = 'statEffect'
      Required = True
    end
    object conditionsEvade: TBooleanField
      FieldName = 'evade'
      Required = True
    end
    object conditionsReflect: TBooleanField
      FieldName = 'reflect'
      Required = True
    end
    object conditionsEqLock: TBooleanField
      FieldName = 'eqLock'
      Required = True
    end
    object conditionsStatusAnimation: TIntegerField
      FieldName = 'statusAnimation'
      Required = True
    end
    object conditionsHpDot: TByteField
      FieldName = 'hpDot'
      Required = True
    end
    object conditionsMpDot: TByteField
      FieldName = 'mpDot'
      Required = True
    end
    object conditionsstandard_1: TIntegerField
      FieldName = 'standard_1'
      Required = True
    end
    object conditionsstandard_2: TIntegerField
      FieldName = 'standard_2'
      Required = True
    end
    object conditionsstandard_3: TIntegerField
      FieldName = 'standard_3'
      Required = True
    end
    object conditionsstandard_4: TIntegerField
      FieldName = 'standard_4'
      Required = True
    end
    object conditionsstandard_5: TIntegerField
      FieldName = 'standard_5'
      Required = True
    end
    object conditionstag_1: TIntegerField
      FieldName = 'tag_1'
      Required = True
    end
    object conditionstag_2: TIntegerField
      FieldName = 'tag_2'
      Required = True
    end
    object conditionstag_3: TIntegerField
      FieldName = 'tag_3'
      Required = True
    end
    object conditionstag_4: TIntegerField
      FieldName = 'tag_4'
      Required = True
    end
  end
  object scriptRange: TClientDataSet
    Active = True
    Aggregates = <>
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
        Name = 'designName'
        DataType = ftWideString
        Size = 255
      end
      item
        Name = 'start'
        DataType = ftInteger
      end
      item
        Name = 'end'
        DataType = ftInteger
      end
      item
        Name = 'unit'
        DataType = ftWideString
        Size = 32
      end
      item
        Name = 'Signature'
        DataType = ftByte
      end>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 752
    Top = 8
    Data = {
      A20000009619E0BD010000001800000007000000000003000000A20002696404
      00010000000000046E616D6501004A0000000100055749445448020002004000
      0A64657369676E4E616D6502004A000000010005574944544802000200FE0105
      7374617274040001000000000003656E64040001000000000004756E69740100
      4A0000000100055749445448020002004000095369676E617475726501000200
      000000000000}
    object scriptRangeId: TIntegerField
      FieldName = 'id'
    end
    object StringField2: TWideStringField
      FieldName = 'name'
      LookupDataSet = attributes
      LookupKeyFields = 'id'
      LookupResultField = 'name'
      KeyFields = 'x'
      Size = 32
    end
    object scriptRangedesignName: TWideStringField
      FieldName = 'designName'
      Size = 255
    end
    object IntegerField6: TIntegerField
      FieldName = 'start'
    end
    object IntegerField7: TIntegerField
      FieldName = 'end'
    end
    object scriptRangeUnit: TWideStringField
      FieldName = 'unit'
      Size = 32
    end
    object scriptRangeSignature: TByteField
      FieldName = 'Signature'
    end
  end
  object skills_attributes: TSimpleDataSet
    Aggregates = <>
    Connection = Connection
    DataSet.CommandText = 'skills_attributes'
    DataSet.CommandType = ctTable
    DataSet.MaxBlobSize = -1
    DataSet.Params = <>
    Filtered = True
    FieldDefs = <>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 368
    Top = 436
    object IntegerField10: TIntegerField
      FieldName = 'master'
      Required = True
    end
    object IntegerField12: TIntegerField
      FieldName = 'x'
      Required = True
    end
    object IntegerField16: TIntegerField
      FieldName = 'y'
      Required = True
    end
  end
  object animations_timingSec: TSimpleDataSet
    Aggregates = <>
    Connection = Connection
    DataSet.CommandText = 'animations_timingSec'
    DataSet.CommandType = ctTable
    DataSet.MaxBlobSize = -1
    DataSet.Params = <>
    Filtered = True
    FieldDefs = <>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 384
    Top = 80
    object animations_timingSecIntegerField: TIntegerField
      FieldName = 'master'
      Required = True
    end
    object animations_timingSecid: TIntegerField
      FieldName = 'id'
      Required = True
    end
    object animations_timingSecname: TWideStringField
      FieldName = 'name'
      Required = True
      Size = 32
    end
    object animations_timingSecframe: TWordField
      FieldName = 'frame'
      Required = True
    end
    object animations_timingSecsoundid: TIntegerField
      FieldName = 'sound_id'
      Required = True
    end
    object animations_timingSecsoundname: TWideStringField
      FieldName = 'sound_name'
      Required = True
      Size = 32
    end
    object animations_timingSecsoundfadeIn: TIntegerField
      FieldName = 'sound_fadeIn'
      Required = True
    end
    object animations_timingSecsoundtempo: TIntegerField
      FieldName = 'sound_tempo'
      Required = True
    end
    object animations_timingSecsoundvolume: TIntegerField
      FieldName = 'sound_volume'
      Required = True
    end
    object animations_timingSecsoundBalance: TIntegerField
      FieldName = 'sound_Balance'
      Required = True
    end
    object animations_timingSecFlashWhere: TByteField
      FieldName = 'flashWhere'
      Required = True
    end
    object animations_timingSecColor: TIntegerField
      FieldName = 'color'
      Required = True
    end
    object animations_timingSecShakeWhere: TByteField
      FieldName = 'shakeWhere'
    end
  end
  object animations_frameSec: TSimpleDataSet
    Aggregates = <>
    Connection = Connection
    DataSet.CommandText = 'animations_frameSec'
    DataSet.CommandType = ctTable
    DataSet.MaxBlobSize = -1
    DataSet.Params = <>
    Filtered = True
    FieldDefs = <>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 384
    Top = 136
    object IntegerField17: TIntegerField
      FieldName = 'master'
      Required = True
    end
    object IntegerField18: TIntegerField
      FieldName = 'id'
      Required = True
    end
    object StringField9: TWideStringField
      FieldName = 'name'
      Required = True
      Size = 32
    end
    object animations_frameSecframe: TWordField
      FieldName = 'frame'
      Required = True
    end
    object animations_frameSecpositionx: TIntegerField
      FieldName = 'position_x'
      Required = True
    end
    object animations_frameSecpositiony: TIntegerField
      FieldName = 'position_y'
      Required = True
    end
    object animations_frameSeczoomx: TIntegerField
      FieldName = 'zoom_x'
      Required = True
    end
    object animations_frameSeczoomy: TIntegerField
      FieldName = 'zoom_y'
      Required = True
    end
    object animations_frameSeccolor: TIntegerField
      FieldName = 'color'
      Required = True
    end
    object animations_frameSecsaturation: TByteField
      FieldName = 'saturation'
      Required = True
    end
    object animations_frameSecImageIndex: TIntegerField
      FieldName = 'imageIndex'
    end
  end
  object tilesets: TSimpleDataSet
    Aggregates = <>
    Connection = Connection
    DataSet.CommandText = 'tilesets'
    DataSet.CommandType = ctTable
    DataSet.MaxBlobSize = -1
    DataSet.Params = <>
    FieldDefs = <>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 520
    Top = 16
    object IntegerField19: TIntegerField
      FieldName = 'id'
      Required = True
    end
    object StringField10: TWideStringField
      FieldName = 'name'
      Required = True
      Size = 32
    end
    object tilesetsHiSpeed: TBooleanField
      FieldName = 'HiSpeed'
      Required = True
    end
  end
  object Switches: TSimpleDataSet
    Aggregates = <>
    Connection = Connection
    DataSet.CommandText = 'switches'
    DataSet.CommandType = ctTable
    DataSet.MaxBlobSize = -1
    DataSet.Params = <>
    FieldDefs = <>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    OnCalcFields = SwitchesVarsCalcFields
    Left = 752
    Top = 120
    object IntegerField20: TIntegerField
      FieldName = 'id'
      Required = True
    end
    object StringField11: TWideStringField
      FieldName = 'name'
      Required = True
      Size = 32
    end
    object SwitchesDisplayName: TWideStringField
      FieldKind = fkCalculated
      FieldName = 'DisplayName'
      Size = 255
      Calculated = True
    end
  end
  object Variables: TSimpleDataSet
    Aggregates = <>
    Connection = Connection
    DataSet.CommandText = 'variables'
    DataSet.CommandType = ctTable
    DataSet.MaxBlobSize = -1
    DataSet.Params = <>
    FieldDefs = <>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    OnCalcFields = SwitchesVarsCalcFields
    Left = 752
    Top = 184
    object IntegerField21: TIntegerField
      FieldName = 'id'
      Required = True
    end
    object StringField12: TWideStringField
      FieldName = 'name'
      Required = True
      Size = 32
    end
    object VariablesDisplayName: TWideStringField
      FieldKind = fkCalculated
      FieldName = 'DisplayName'
      Size = 255
      Calculated = True
    end
  end
  object heroes: TSimpleDataSet
    Aggregates = <>
    Connection = Connection
    DataSet.CommandText = 'heroes'
    DataSet.CommandType = ctTable
    DataSet.MaxBlobSize = -1
    DataSet.Params = <>
    FieldDefs = <>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 103
    Top = 30
    object heroesid: TIntegerField
      FieldName = 'id'
      Required = True
    end
    object heroesname: TWideStringField
      FieldName = 'name'
      Required = True
    end
    object heroesmapSprite: TWideStringField
      DisplayWidth = 20
      FieldName = 'mapSprite'
      Required = True
      Size = 255
    end
    object heroesActionMatrix: TIntegerField
      FieldName = 'actionMatrix'
      Required = True
    end
    object heroesbattleSprite: TIntegerField
      FieldName = 'battleSprite'
      Required = True
    end
    object heroesBattleMatrix: TIntegerField
      FieldName = 'battleMatrix'
      Required = True
    end
    object heroesPortrait: TWideStringField
      DisplayWidth = 20
      FieldName = 'portrait'
      Required = True
      Size = 255
    end
    object heroesPortraitIndex: TIntegerField
      FieldName = 'portraitIndex'
      Required = True
    end
    object heroescommand1: TIntegerField
      FieldName = 'command_1'
      Required = True
    end
    object heroescommand2: TIntegerField
      FieldName = 'command_2'
      Required = True
    end
    object heroescommand3: TIntegerField
      FieldName = 'command_3'
      Required = True
    end
    object heroescommand4: TIntegerField
      FieldName = 'command_4'
      Required = True
    end
    object heroescommand5: TIntegerField
      FieldName = 'command_5'
      Required = True
    end
    object heroescommand6: TIntegerField
      FieldName = 'command_6'
      Required = True
    end
    object heroescommand7: TIntegerField
      FieldName = 'command_7'
      Required = True
    end
    object heroescommands: TByteField
      FieldName = 'commands'
      Required = True
    end
    object heroesstatblock1: TIntegerField
      FieldName = 'statblock_1'
      Required = True
    end
    object heroesstatblock2: TIntegerField
      FieldName = 'statblock_2'
      Required = True
    end
    object heroesstatblock3: TIntegerField
      FieldName = 'statblock_3'
      Required = True
    end
    object heroesstatblock4: TIntegerField
      FieldName = 'statblock_4'
      Required = True
    end
    object heroesstatblock5: TIntegerField
      FieldName = 'statblock_5'
      Required = True
    end
    object heroesstatblock6: TIntegerField
      FieldName = 'statblock_6'
      Required = True
    end
    object heroesexpFunc: TWideStringField
      FieldName = 'expFunc'
      Required = True
      Size = 32
    end
    object heroesexpVars1: TIntegerField
      FieldName = 'expVars_1'
      Required = True
    end
    object heroesexpVars2: TIntegerField
      FieldName = 'expVars_2'
      Required = True
    end
    object heroesexpVars3: TIntegerField
      FieldName = 'expVars_3'
      Required = True
    end
    object heroesexpVars0: TIntegerField
      FieldName = 'expVars_4'
      Required = True
    end
    object heroesdualWield: TIntegerField
      FieldName = 'dualWield'
      Required = True
    end
    object heroesstaticEq: TBooleanField
      FieldName = 'staticEq'
      Required = True
    end
    object heroesstrongDef: TBooleanField
      FieldName = 'strongDef'
      Required = True
    end
    object heroesunarmedAnim: TIntegerField
      FieldName = 'unarmedAnim'
      Required = True
    end
    object heroesequip1: TIntegerField
      FieldName = 'equip_1'
      Required = True
    end
    object heroesequip2: TIntegerField
      FieldName = 'equip_2'
      Required = True
    end
    object heroesequip3: TIntegerField
      FieldName = 'equip_3'
      Required = True
    end
    object heroesequip4: TIntegerField
      FieldName = 'equip_4'
      Required = True
    end
    object heroesequip5: TIntegerField
      FieldName = 'equip_5'
      Required = True
    end
    object heroesTitle: TWideStringField
      FieldName = 'Title'
      Required = True
      Size = 32
    end
    object heroesClass: TIntegerField
      FieldName = 'Class'
      Required = True
    end
    object heroesMinLevel: TWordField
      FieldName = 'MinLevel'
      Required = True
    end
    object heroesMaxLevel: TWordField
      FieldName = 'MaxLevel'
      Required = True
    end
    object heroesGuest: TBooleanField
      FieldName = 'Guest'
      Required = True
    end
    object heroesbattlePos_X: TIntegerField
      FieldName = 'battlePos_X'
    end
    object heroesbattlePos_Y: TIntegerField
      FieldName = 'battlePos_Y'
    end
    object heroesportraitShiftFColorSet1: TFloatField
      FieldName = 'portraitShift_FColorSet_1'
      Required = True
    end
    object heroesportraitShiftFColorSet2: TFloatField
      FieldName = 'portraitShift_FColorSet_2'
      Required = True
    end
    object heroesportraitShiftFColorSet3: TFloatField
      FieldName = 'portraitShift_FColorSet_3'
      Required = True
    end
    object heroesportraitShiftFColorSet4: TFloatField
      FieldName = 'portraitShift_FColorSet_4'
      Required = True
    end
    object heroesportraitShiftFHue: TShortintField
      FieldName = 'portraitshift_FHue'
      Required = True
    end
    object heroesspriteShiftFColorSet1: TFloatField
      FieldName = 'spriteShift_FColorSet_1'
      Required = True
    end
    object heroesspriteShiftFColorSet2: TFloatField
      FieldName = 'spriteShift_FColorSet_2'
      Required = True
    end
    object heroesspriteShiftFColorSet3: TFloatField
      FieldName = 'spriteShift_FColorSet_3'
      Required = True
    end
    object heroesspriteShiftFColorSet4: TFloatField
      FieldName = 'spriteShift_FColorSet_4'
      Required = True
    end
    object heroesspriteShiftFHue: TShortintField
      FieldName = 'spriteShift_FHue'
      Required = True
    end
    object heroesBattleSpriteShiftFColorSet1: TFloatField
      FieldName = 'BattleSpriteShift_FColorSet_1'
      Required = True
    end
    object heroesBattleSpriteShiftFColorSet2: TFloatField
      FieldName = 'BattleSpriteShift_FColorSet_2'
      Required = True
    end
    object heroesBattleSpriteShiftFColorSet3: TFloatField
      FieldName = 'BattleSpriteShift_FColorSet_3'
      Required = True
    end
    object heroesBattleSpriteShiftFColorSet4: TFloatField
      FieldName = 'BattleSpriteShift_FColorSet_4'
      Required = True
    end
    object heroesBattleSpriteShiftFHue: TShortintField
      FieldName = 'BattleSpriteShift_FHue'
      Required = True
    end
    object heroescanCrit: TBooleanField
      FieldName = 'canCrit'
      Required = True
    end
    object heroescritRate: TIntegerField
      FieldName = 'critRate'
      Required = True
    end
    object heroesTranslucent: TBooleanField
      FieldName = 'translucent'
      Required = True
    end
  end
  object heroes_Conditions: TSimpleDataSet
    Aggregates = <>
    Connection = Connection
    DataSet.CommandText = 'heroes_Conditions'
    DataSet.CommandType = ctTable
    DataSet.MaxBlobSize = -1
    DataSet.Params = <>
    Filtered = True
    FieldDefs = <>
    IndexDefs = <
      item
        Name = 'DEFAULT_ORDER'
      end
      item
        Name = 'CHANGEINDEX'
      end>
    IndexFieldNames = 'master'
    MasterFields = 'id'
    PacketRecords = 0
    Params = <>
    StoreDefs = True
    Left = 103
    Top = 182
    object IntegerField22: TIntegerField
      FieldName = 'master'
      Required = True
    end
    object IntegerField23: TIntegerField
      FieldName = 'x'
      Required = True
    end
    object IntegerField24: TIntegerField
      FieldName = 'y'
      Required = True
    end
  end
  object heroes_Resists: TSimpleDataSet
    Aggregates = <>
    Connection = Connection
    DataSet.CommandText = 'heroes_Resists'
    DataSet.CommandType = ctTable
    DataSet.MaxBlobSize = -1
    DataSet.Params = <>
    Filtered = True
    FieldDefs = <>
    IndexDefs = <
      item
        Name = 'DEFAULT_ORDER'
      end
      item
        Name = 'CHANGEINDEX'
      end>
    IndexFieldNames = 'master'
    MasterFields = 'id'
    PacketRecords = 0
    Params = <>
    StoreDefs = True
    Left = 103
    Top = 126
    object heroes_Resistsmaster: TIntegerField
      FieldName = 'master'
      Required = True
    end
    object heroes_Resistsx: TIntegerField
      FieldName = 'x'
      Required = True
    end
    object heroes_Resistsy: TIntegerField
      FieldName = 'y'
      Required = True
    end
  end
  object heroes_skillset: TSimpleDataSet
    Aggregates = <>
    Connection = Connection
    DataSet.CommandText = 'heroes_skillset'
    DataSet.CommandType = ctTable
    DataSet.MaxBlobSize = -1
    DataSet.Params = <>
    Filtered = True
    FieldDefs = <>
    IndexDefs = <
      item
        Name = 'DEFAULT_ORDER'
      end>
    IndexFieldNames = 'master'
    MasterFields = 'id'
    PacketRecords = 0
    Params = <>
    StoreDefs = True
    Left = 103
    Top = 78
    object heroes_skillsetmaster: TIntegerField
      FieldName = 'master'
      Required = True
    end
    object heroes_skillsetid: TWideStringField
      FieldKind = fkCalculated
      FieldName = 'id'
      Size = 10
      Calculated = True
    end
    object heroes_skillsetname: TWideStringField
      FieldKind = fkCalculated
      FieldName = 'name'
      Size = 32
      Calculated = True
    end
    object heroes_skillsetstyle: TIntegerField
      FieldName = 'style'
      Required = True
    end
    object heroes_skillsetnums1: TIntegerField
      FieldName = 'nums_1'
      Required = True
    end
    object heroes_skillsetnums2: TIntegerField
      FieldName = 'nums_2'
      Required = True
    end
    object heroes_skillsetnums3: TIntegerField
      FieldName = 'nums_3'
      Required = True
    end
    object heroes_skillsetnums4: TIntegerField
      FieldName = 'nums_4'
      Required = True
    end
    object heroes_skillsetskill: TIntegerField
      FieldName = 'skill'
      Required = True
    end
  end
  object Floats: TSimpleDataSet
    Aggregates = <>
    Connection = Connection
    DataSet.CommandText = 'floats'
    DataSet.CommandType = ctTable
    DataSet.MaxBlobSize = -1
    DataSet.Params = <>
    FieldDefs = <>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    OnCalcFields = SwitchesVarsCalcFields
    Left = 752
    Top = 232
    object IntegerField5: TIntegerField
      FieldName = 'id'
      Required = True
    end
    object WideStringField1: TWideStringField
      FieldName = 'name'
      Required = True
      Size = 32
    end
    object WideStringField2: TWideStringField
      FieldKind = fkCalculated
      FieldName = 'DisplayName'
      Size = 255
      Calculated = True
    end
  end
  object Strings: TSimpleDataSet
    Aggregates = <>
    Connection = Connection
    DataSet.CommandText = 'strings'
    DataSet.CommandType = ctTable
    DataSet.MaxBlobSize = -1
    DataSet.Params = <>
    FieldDefs = <>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    OnCalcFields = SwitchesVarsCalcFields
    Left = 752
    Top = 288
    object IntegerField8: TIntegerField
      FieldName = 'id'
      Required = True
    end
    object WideStringField3: TWideStringField
      FieldName = 'name'
      Required = True
      Size = 32
    end
    object WideStringField4: TWideStringField
      FieldKind = fkCalculated
      FieldName = 'DisplayName'
      Size = 255
      Calculated = True
    end
  end
  object GlobalScripts: TSimpleDataSet
    Aggregates = <>
    Connection = Connection
    DataSet.CommandText = 'GlobalScripts'
    DataSet.CommandType = ctTable
    DataSet.MaxBlobSize = -1
    DataSet.Params = <>
    FieldDefs = <>
    IndexDefs = <>
    IndexFieldNames = 'Id'
    Params = <>
    StoreDefs = True
    Left = 748
    Top = 496
    object GlobalScriptsId: TIntegerField
      FieldName = 'Id'
      Required = True
    end
    object GlobalScriptsName: TWideStringField
      DisplayWidth = 32
      FieldName = 'Name'
      Required = True
      Size = 255
    end
    object GlobalScriptsStartConditon: TIntegerField
      FieldName = 'StartCondition'
      Required = True
    end
    object GlobalScriptsHasSwitch: TBooleanField
      FieldName = 'HasSwitch'
      Required = True
    end
    object GlobalScriptsSwitch: TIntegerField
      FieldName = 'Switch'
      Required = True
    end
  end
  object Vocab: TSimpleDataSet
    Aggregates = <>
    Connection = Connection
    DataSet.CommandText = 'vocab'
    DataSet.CommandType = ctTable
    DataSet.MaxBlobSize = -1
    DataSet.Params = <>
    FieldDefs = <>
    IndexDefs = <
      item
        Name = 'DEFAULT_ORDER'
      end
      item
        Name = 'CHANGEINDEX'
      end>
    IndexFieldNames = 'Key'
    Params = <>
    StoreDefs = True
    Left = 592
    Top = 384
    object VocabKey: TWideStringField
      FieldName = 'Key'
      Required = True
      Size = 32
    end
    object VocabValue: TWideStringField
      FieldName = 'Val'
      Required = True
      Size = 255
    end
  end
  object CustomVocab: TSimpleDataSet
    Aggregates = <>
    Connection = Connection
    DataSet.CommandText = 'CustomVocab'
    DataSet.CommandType = ctTable
    DataSet.MaxBlobSize = -1
    DataSet.Params = <>
    FieldDefs = <>
    IndexDefs = <
      item
        Name = 'DEFAULT_ORDER'
      end
      item
        Name = 'CHANGEINDEX'
      end>
    IndexFieldNames = 'Key'
    Params = <>
    StoreDefs = True
    Left = 592
    Top = 440
    object WideStringField5: TWideStringField
      FieldName = 'Key'
      Required = True
      Size = 32
    end
    object WideStringField6: TWideStringField
      FieldName = 'Val'
      Required = True
      Size = 255
    end
  end
  object items_script: TClientDataSet
    Active = True
    Aggregates = <>
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
        Name = 'description'
        DataType = ftWideString
        Size = 50
      end
      item
        Name = 'cost'
        DataType = ftInteger
      end
      item
        Name = 'tag_1'
        DataType = ftInteger
      end
      item
        Name = 'tag_2'
        DataType = ftInteger
      end
      item
        Name = 'tag_3'
        DataType = ftInteger
      end
      item
        Name = 'tag_4'
        DataType = ftInteger
      end
      item
        Name = 'usesLeft'
        DataType = ftInteger
      end
      item
        Name = 'usableWhere'
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
        Name = 'event'
        DataType = ftWideString
        Size = 32
      end
      item
        Name = 'script'
        DataType = ftWideMemo
      end
      item
        Name = 'stat_1'
        DataType = ftInteger
      end
      item
        Name = 'stat_2'
        DataType = ftInteger
      end
      item
        Name = 'stat_3'
        DataType = ftInteger
      end
      item
        Name = 'stat_4'
        DataType = ftInteger
      end
      item
        Name = 'stat_5'
        DataType = ftInteger
      end
      item
        Name = 'stat_6'
        DataType = ftInteger
      end>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    AfterOpen = restoreClone
    Left = 184
    Top = 496
    Data = {
      B70100009619E0BD010000001800000014000000000003000000B70102696404
      00010000000000046E616D6501004A0000000100055749445448020002004000
      0B6465736372697074696F6E01004A0000000100055749445448020002006400
      04636F73740400010000000000057461675F310400010000000000057461675F
      320400010000000000057461675F330400010000000000057461675F34040001
      000000000008757365734C65667404000100000000000B757361626C65576865
      726504000100000000000C757361626C6542794865726F04004B000000010007
      5355425459504502004900070042696E617279000D757361626C654279436C61
      737304004B0000000100075355425459504502004900070042696E6172790005
      6576656E7401004A000000010005574944544802000200400006736372697074
      04004B0000000100075355425459504502004900090057696465546578740006
      737461745F31040001000000000006737461745F320400010000000000067374
      61745F33040001000000000006737461745F3404000100000000000673746174
      5F35040001000000000006737461745F3604000100000000000000}
    object items_scriptid: TIntegerField
      FieldName = 'id'
    end
    object items_scriptname: TWideStringField
      FieldName = 'name'
      Size = 32
    end
    object items_scriptdesc: TWideStringField
      FieldName = 'description'
      Size = 50
    end
    object items_scriptcost: TIntegerField
      FieldName = 'cost'
    end
    object items_scripttag1: TIntegerField
      FieldName = 'tag_1'
    end
    object items_scripttag2: TIntegerField
      FieldName = 'tag_2'
    end
    object items_scripttag3: TIntegerField
      FieldName = 'tag_3'
    end
    object items_scripttag4: TIntegerField
      FieldName = 'tag_4'
    end
    object items_scriptusesLeft: TIntegerField
      FieldName = 'usesLeft'
    end
    object items_scriptusableWhere: TIntegerField
      FieldName = 'usableWhere'
    end
    object items_scriptusableByHero: TBlobField
      FieldName = 'usableByHero'
    end
    object items_scriptusableByClass: TBlobField
      FieldName = 'usableByClass'
    end
    object items_scriptevent: TWideStringField
      FieldName = 'event'
      Size = 32
    end
    object items_scriptscript: TWideMemoField
      FieldName = 'script'
      BlobType = ftWideMemo
    end
    object items_scriptIntegerField: TIntegerField
      FieldName = 'stat_1'
    end
    object items_scriptIntegerField2: TIntegerField
      FieldName = 'stat_2'
    end
    object items_scriptIntegerField3: TIntegerField
      FieldName = 'stat_3'
    end
    object items_scriptIntegerField4: TIntegerField
      FieldName = 'stat_4'
    end
    object items_scriptIntegerField5: TIntegerField
      FieldName = 'stat_5'
    end
    object items_scriptIntegerField6: TIntegerField
      FieldName = 'stat_6'
    end
  end
  object items_medicine: TClientDataSet
    Active = True
    Aggregates = <>
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
        Name = 'description'
        DataType = ftWideString
        Size = 50
      end
      item
        Name = 'cost'
        DataType = ftInteger
      end
      item
        Name = 'tag_1'
        DataType = ftInteger
      end
      item
        Name = 'tag_2'
        DataType = ftInteger
      end
      item
        Name = 'tag_3'
        DataType = ftInteger
      end
      item
        Name = 'tag_4'
        DataType = ftInteger
      end
      item
        Name = 'usesLeft'
        DataType = ftInteger
      end
      item
        Name = 'usableWhere'
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
        Name = 'stat_1'
        DataType = ftInteger
      end
      item
        Name = 'stat_2'
        DataType = ftInteger
      end
      item
        Name = 'stat_3'
        DataType = ftInteger
      end
      item
        Name = 'stat_4'
        DataType = ftInteger
      end
      item
        Name = 'stat_5'
        DataType = ftInteger
      end
      item
        Name = 'stat_6'
        DataType = ftInteger
      end
      item
        Name = 'areaMedicine'
        DataType = ftBoolean
      end
      item
        Name = 'hpPercent'
        DataType = ftInteger
      end
      item
        Name = 'mpPercent'
        DataType = ftInteger
      end
      item
        Name = 'deadHeroesOnly'
        DataType = ftBoolean
      end>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    AfterOpen = restoreClone
    Left = 184
    Top = 224
    Data = {
      C70100009619E0BD010000001800000016000000000003000000C70102696404
      00010000000000046E616D6501004A0000000100055749445448020002004000
      0B6465736372697074696F6E01004A0000000100055749445448020002006400
      04636F73740400010000000000057461675F310400010000000000057461675F
      320400010000000000057461675F330400010000000000057461675F34040001
      000000000008757365734C65667404000100000000000B757361626C65576865
      726504000100000000000C757361626C6542794865726F04004B000000010007
      5355425459504502004900070042696E617279000D757361626C654279436C61
      737304004B0000000100075355425459504502004900070042696E6172790006
      737461745F31040001000000000006737461745F320400010000000000067374
      61745F33040001000000000006737461745F3404000100000000000673746174
      5F35040001000000000006737461745F3604000100000000000C617265614D65
      646963696E65020003000000000009687050657263656E740400010000000000
      096D7050657263656E7404000100000000000E646561644865726F65734F6E6C
      7902000300000000000000}
    object items_medicineid: TIntegerField
      FieldName = 'id'
    end
    object items_medicinename: TWideStringField
      FieldName = 'name'
      Size = 32
    end
    object items_medicinedesc: TWideStringField
      FieldName = 'description'
      Size = 50
    end
    object items_medicinecost: TIntegerField
      FieldName = 'cost'
    end
    object items_medicineIntegerField: TIntegerField
      FieldName = 'tag_1'
    end
    object items_medicineIntegerField2: TIntegerField
      FieldName = 'tag_2'
    end
    object items_medicineIntegerField3: TIntegerField
      FieldName = 'tag_3'
    end
    object items_medicineIntegerField4: TIntegerField
      FieldName = 'tag_4'
    end
    object items_medicineusesLeft: TIntegerField
      FieldName = 'usesLeft'
    end
    object items_medicineusableWhere: TIntegerField
      FieldName = 'usableWhere'
    end
    object items_medicineusableByHero: TBlobField
      FieldName = 'usableByHero'
    end
    object items_medicineusableByClass: TBlobField
      FieldName = 'usableByClass'
    end
    object items_medicinestat1: TIntegerField
      FieldName = 'stat_1'
    end
    object items_medicinestat2: TIntegerField
      FieldName = 'stat_2'
    end
    object items_medicinestat3: TIntegerField
      FieldName = 'stat_3'
    end
    object items_medicinestat4: TIntegerField
      FieldName = 'stat_4'
    end
    object items_medicinestat5: TIntegerField
      FieldName = 'stat_5'
    end
    object items_medicinestat0: TIntegerField
      FieldName = 'stat_6'
    end
    object items_medicineareaMedicine: TBooleanField
      FieldName = 'areaMedicine'
    end
    object items_medicinehpPercent: TIntegerField
      FieldName = 'hpPercent'
    end
    object items_medicinempPercent: TIntegerField
      FieldName = 'mpPercent'
    end
    object items_medicinedeadOnly: TBooleanField
      FieldName = 'deadHeroesOnly'
    end
  end
  object items_book: TClientDataSet
    Active = True
    Aggregates = <>
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
        Name = 'description'
        DataType = ftWideString
        Size = 50
      end
      item
        Name = 'cost'
        DataType = ftInteger
      end
      item
        Name = 'tag_1'
        DataType = ftInteger
      end
      item
        Name = 'tag_2'
        DataType = ftInteger
      end
      item
        Name = 'tag_3'
        DataType = ftInteger
      end
      item
        Name = 'tag_4'
        DataType = ftInteger
      end
      item
        Name = 'usesLeft'
        DataType = ftInteger
      end
      item
        Name = 'usableWhere'
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
        Name = 'stat_1'
        DataType = ftInteger
      end
      item
        Name = 'stat_2'
        DataType = ftInteger
      end
      item
        Name = 'stat_3'
        DataType = ftInteger
      end
      item
        Name = 'stat_4'
        DataType = ftInteger
      end
      item
        Name = 'stat_5'
        DataType = ftInteger
      end
      item
        Name = 'stat_6'
        DataType = ftInteger
      end
      item
        Name = 'skill'
        DataType = ftInteger
      end>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    AfterOpen = restoreClone
    Left = 184
    Top = 280
    Data = {
      850100009619E0BD010000001800000013000000000003000000850102696404
      00010000000000046E616D6501004A0000000100055749445448020002004000
      0B6465736372697074696F6E01004A0000000100055749445448020002006400
      04636F73740400010000000000057461675F310400010000000000057461675F
      320400010000000000057461675F330400010000000000057461675F34040001
      000000000008757365734C65667404000100000000000B757361626C65576865
      726504000100000000000C757361626C6542794865726F04004B000000010007
      5355425459504502004900070042696E617279000D757361626C654279436C61
      737304004B0000000100075355425459504502004900070042696E6172790006
      737461745F31040001000000000006737461745F320400010000000000067374
      61745F33040001000000000006737461745F3404000100000000000673746174
      5F35040001000000000006737461745F36040001000000000005736B696C6C04
      000100000000000000}
    object items_bookid: TIntegerField
      FieldName = 'id'
    end
    object items_bookname: TWideStringField
      FieldName = 'name'
      Size = 32
    end
    object items_bookdesc: TWideStringField
      FieldName = 'description'
      Size = 50
    end
    object items_bookcost: TIntegerField
      FieldName = 'cost'
    end
    object items_bookIntegerField: TIntegerField
      FieldName = 'tag_1'
    end
    object items_bookIntegerField2: TIntegerField
      FieldName = 'tag_2'
    end
    object items_bookIntegerField3: TIntegerField
      FieldName = 'tag_3'
    end
    object items_bookIntegerField4: TIntegerField
      FieldName = 'tag_4'
    end
    object items_bookusesLeft: TIntegerField
      FieldName = 'usesLeft'
    end
    object items_bookusableWhere: TIntegerField
      FieldName = 'usableWhere'
    end
    object items_bookusableByHero: TBlobField
      FieldName = 'usableByHero'
    end
    object items_bookusableByClass: TBlobField
      FieldName = 'usableByClass'
    end
    object items_bookstat1: TIntegerField
      FieldName = 'stat_1'
    end
    object items_bookstat2: TIntegerField
      FieldName = 'stat_2'
    end
    object items_bookstat3: TIntegerField
      FieldName = 'stat_3'
    end
    object items_bookstat4: TIntegerField
      FieldName = 'stat_4'
    end
    object items_bookstat5: TIntegerField
      FieldName = 'stat_5'
    end
    object items_bookstat6: TIntegerField
      FieldName = 'stat_6'
    end
    object cskill: TIntegerField
      FieldName = 'skill'
    end
  end
  object items_skill: TClientDataSet
    Active = True
    Aggregates = <>
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
        Name = 'description'
        DataType = ftWideString
        Size = 50
      end
      item
        Name = 'cost'
        DataType = ftInteger
      end
      item
        Name = 'tag_1'
        DataType = ftInteger
      end
      item
        Name = 'tag_2'
        DataType = ftInteger
      end
      item
        Name = 'tag_3'
        DataType = ftInteger
      end
      item
        Name = 'tag_4'
        DataType = ftInteger
      end
      item
        Name = 'usesLeft'
        DataType = ftInteger
      end
      item
        Name = 'usableWhere'
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
        Name = 'stat_1'
        DataType = ftInteger
      end
      item
        Name = 'stat_2'
        DataType = ftInteger
      end
      item
        Name = 'stat_3'
        DataType = ftInteger
      end
      item
        Name = 'stat_4'
        DataType = ftInteger
      end
      item
        Name = 'stat_5'
        DataType = ftInteger
      end
      item
        Name = 'stat_6'
        DataType = ftInteger
      end
      item
        Name = 'skill'
        DataType = ftInteger
      end
      item
        Name = 'customSkillMessage'
        DataType = ftBoolean
      end>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    AfterOpen = restoreClone
    Left = 184
    Top = 336
    Data = {
      A00100009619E0BD010000001800000014000000000003000000A00102696404
      00010000000000046E616D6501004A0000000100055749445448020002004000
      0B6465736372697074696F6E01004A0000000100055749445448020002006400
      04636F73740400010000000000057461675F310400010000000000057461675F
      320400010000000000057461675F330400010000000000057461675F34040001
      000000000008757365734C65667404000100000000000B757361626C65576865
      726504000100000000000C757361626C6542794865726F04004B000000010007
      5355425459504502004900070042696E617279000D757361626C654279436C61
      737304004B0000000100075355425459504502004900070042696E6172790006
      737461745F31040001000000000006737461745F320400010000000000067374
      61745F33040001000000000006737461745F3404000100000000000673746174
      5F35040001000000000006737461745F36040001000000000005736B696C6C04
      0001000000000012637573746F6D536B696C6C4D657373616765020003000000
      00000000}
    object items_skillid: TIntegerField
      FieldName = 'id'
    end
    object items_skillname: TWideStringField
      FieldName = 'name'
      Size = 32
    end
    object items_skilldesc: TWideStringField
      FieldName = 'description'
      Size = 50
    end
    object items_skillcost: TIntegerField
      FieldName = 'cost'
    end
    object items_skillIntegerField: TIntegerField
      FieldName = 'tag_1'
    end
    object items_skillIntegerField2: TIntegerField
      FieldName = 'tag_2'
    end
    object items_skillIntegerField3: TIntegerField
      FieldName = 'tag_3'
    end
    object items_skillIntegerField4: TIntegerField
      FieldName = 'tag_4'
    end
    object items_skillusesLeft: TIntegerField
      FieldName = 'usesLeft'
    end
    object items_skillusableWhere: TIntegerField
      FieldName = 'usableWhere'
    end
    object items_skillusableByHero: TBlobField
      FieldName = 'usableByHero'
    end
    object items_skillusableByClass: TBlobField
      FieldName = 'usableByClass'
    end
    object items_skillstat1: TIntegerField
      FieldName = 'stat_1'
    end
    object items_skillstat2: TIntegerField
      FieldName = 'stat_2'
    end
    object items_skillstat3: TIntegerField
      FieldName = 'stat_3'
    end
    object items_skillstat4: TIntegerField
      FieldName = 'stat_4'
    end
    object items_skillstat5: TIntegerField
      FieldName = 'stat_5'
    end
    object items_skillstat6: TIntegerField
      FieldName = 'stat_6'
    end
    object items_skillskill: TIntegerField
      FieldName = 'skill'
    end
    object items_skillcustomSkillMessage: TBooleanField
      FieldName = 'customSkillMessage'
    end
  end
  object items_upgrade: TClientDataSet
    Active = True
    Aggregates = <>
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
        Name = 'description'
        DataType = ftWideString
        Size = 50
      end
      item
        Name = 'cost'
        DataType = ftInteger
      end
      item
        Name = 'tag_1'
        DataType = ftInteger
      end
      item
        Name = 'tag_2'
        DataType = ftInteger
      end
      item
        Name = 'tag_3'
        DataType = ftInteger
      end
      item
        Name = 'tag_4'
        DataType = ftInteger
      end
      item
        Name = 'usesLeft'
        DataType = ftInteger
      end
      item
        Name = 'usableWhere'
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
        Name = 'stat_1'
        DataType = ftInteger
      end
      item
        Name = 'stat_2'
        DataType = ftInteger
      end
      item
        Name = 'stat_3'
        DataType = ftInteger
      end
      item
        Name = 'stat_4'
        DataType = ftInteger
      end
      item
        Name = 'stat_5'
        DataType = ftInteger
      end
      item
        Name = 'stat_6'
        DataType = ftInteger
      end>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    AfterOpen = restoreClone
    Left = 184
    Top = 388
    Data = {
      770100009619E0BD010000001800000012000000000003000000770102696404
      00010000000000046E616D6501004A0000000100055749445448020002004000
      0B6465736372697074696F6E01004A0000000100055749445448020002006400
      04636F73740400010000000000057461675F310400010000000000057461675F
      320400010000000000057461675F330400010000000000057461675F34040001
      000000000008757365734C65667404000100000000000B757361626C65576865
      726504000100000000000C757361626C6542794865726F04004B000000010007
      5355425459504502004900070042696E617279000D757361626C654279436C61
      737304004B0000000100075355425459504502004900070042696E6172790006
      737461745F31040001000000000006737461745F320400010000000000067374
      61745F33040001000000000006737461745F3404000100000000000673746174
      5F35040001000000000006737461745F3604000100000000000000}
    object items_upgradeid: TIntegerField
      FieldName = 'id'
    end
    object items_upgradename: TWideStringField
      FieldName = 'name'
      Size = 32
    end
    object items_upgradedesc: TWideStringField
      FieldName = 'description'
      Size = 50
    end
    object items_upgradecost: TIntegerField
      FieldName = 'cost'
    end
    object items_upgradeIntegerField: TIntegerField
      FieldName = 'tag_1'
    end
    object items_upgradeIntegerField2: TIntegerField
      FieldName = 'tag_2'
    end
    object items_upgradeIntegerField3: TIntegerField
      FieldName = 'tag_3'
    end
    object items_upgradeIntegerField4: TIntegerField
      FieldName = 'tag_4'
    end
    object items_upgradeusesLeft: TIntegerField
      FieldName = 'usesLeft'
    end
    object items_upgradeusableWhere: TIntegerField
      FieldName = 'usableWhere'
    end
    object items_upgradeusableByHero: TBlobField
      FieldName = 'usableByHero'
    end
    object items_upgradeusableByClass: TBlobField
      FieldName = 'usableByClass'
    end
    object items_upgradestat1: TIntegerField
      FieldName = 'stat_1'
    end
    object items_upgradestat2: TIntegerField
      FieldName = 'stat_2'
    end
    object items_upgradestat3: TIntegerField
      FieldName = 'stat_3'
    end
    object items_upgradestat4: TIntegerField
      FieldName = 'stat_4'
    end
    object items_upgradestat5: TIntegerField
      FieldName = 'stat_5'
    end
    object items_upgradestat6: TIntegerField
      FieldName = 'stat_6'
    end
  end
  object items_variable: TClientDataSet
    Active = True
    Aggregates = <>
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
        Name = 'description'
        DataType = ftWideString
        Size = 50
      end
      item
        Name = 'cost'
        DataType = ftInteger
      end
      item
        Name = 'tag_1'
        DataType = ftInteger
      end
      item
        Name = 'tag_2'
        DataType = ftInteger
      end
      item
        Name = 'tag_3'
        DataType = ftInteger
      end
      item
        Name = 'tag_4'
        DataType = ftInteger
      end
      item
        Name = 'usesLeft'
        DataType = ftInteger
      end
      item
        Name = 'usableWhere'
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
        Name = 'which'
        DataType = ftInteger
      end
      item
        Name = 'magnitude'
        DataType = ftInteger
      end
      item
        Name = 'style'
        DataType = ftInteger
      end
      item
        Name = 'operation'
        DataType = ftInteger
      end
      item
        Name = 'stat_1'
        DataType = ftInteger
      end
      item
        Name = 'stat_2'
        DataType = ftInteger
      end
      item
        Name = 'stat_3'
        DataType = ftInteger
      end
      item
        Name = 'stat_4'
        DataType = ftInteger
      end
      item
        Name = 'stat_5'
        DataType = ftInteger
      end
      item
        Name = 'stat_6'
        DataType = ftInteger
      end>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    AfterOpen = restoreClone
    Left = 184
    Top = 440
    Data = {
      B70100009619E0BD010000001800000016000000000003000000B70102696404
      00010000000000046E616D6501004A0000000100055749445448020002004000
      0B6465736372697074696F6E01004A0000000100055749445448020002006400
      04636F73740400010000000000057461675F310400010000000000057461675F
      320400010000000000057461675F330400010000000000057461675F34040001
      000000000008757365734C65667404000100000000000B757361626C65576865
      726504000100000000000C757361626C6542794865726F04004B000000010007
      5355425459504502004900070042696E617279000D757361626C654279436C61
      737304004B0000000100075355425459504502004900070042696E6172790005
      77686963680400010000000000096D61676E6974756465040001000000000005
      7374796C650400010000000000096F7065726174696F6E040001000000000006
      737461745F31040001000000000006737461745F320400010000000000067374
      61745F33040001000000000006737461745F3404000100000000000673746174
      5F35040001000000000006737461745F3604000100000000000000}
    object items_variableid: TIntegerField
      FieldName = 'id'
    end
    object items_variablename: TWideStringField
      FieldName = 'name'
      Size = 32
    end
    object items_variabledesc: TWideStringField
      FieldName = 'description'
      Size = 50
    end
    object items_variablecost: TIntegerField
      FieldName = 'cost'
    end
    object items_variableIntegerField: TIntegerField
      FieldName = 'tag_1'
    end
    object items_variableIntegerField2: TIntegerField
      FieldName = 'tag_2'
    end
    object items_variableIntegerField3: TIntegerField
      FieldName = 'tag_3'
    end
    object items_scripttag0: TIntegerField
      FieldName = 'tag_4'
    end
    object items_variableusesLeft: TIntegerField
      FieldName = 'usesLeft'
    end
    object items_variableusableWhere: TIntegerField
      FieldName = 'usableWhere'
    end
    object items_variableusableByHero: TBlobField
      FieldName = 'usableByHero'
    end
    object items_variableusableByClass: TBlobField
      FieldName = 'usableByClass'
    end
    object items_variablewhich: TIntegerField
      FieldName = 'which'
    end
    object items_variablemagnitude: TIntegerField
      FieldName = 'magnitude'
    end
    object items_variablestyle: TIntegerField
      FieldName = 'style'
    end
    object items_variableoperation: TIntegerField
      FieldName = 'operation'
    end
    object items_variableIntegerField4: TIntegerField
      FieldName = 'stat_1'
    end
    object items_variableIntegerField5: TIntegerField
      FieldName = 'stat_2'
    end
    object items_variableIntegerField6: TIntegerField
      FieldName = 'stat_3'
    end
    object items_variableIntegerField7: TIntegerField
      FieldName = 'stat_4'
    end
    object items_variableIntegerField8: TIntegerField
      FieldName = 'stat_5'
    end
    object items_variableIntegerField9: TIntegerField
      FieldName = 'stat_6'
    end
  end
  object ArbitraryQuery: TSQLQuery
    MaxBlobSize = -1
    Params = <>
    SQLConnection = Connection
    Left = 824
    Top = 136
  end
  object Connection: TSQLConnection
    LoginPrompt = False
    Left = 824
    Top = 80
  end
  object vehicles: TSimpleDataSet
    Aggregates = <>
    Connection = Connection
    DataSet.CommandText = 'VEHICLES'
    DataSet.CommandType = ctTable
    DataSet.MaxBlobSize = -1
    DataSet.Params = <>
    FieldDefs = <>
    IndexDefs = <>
    IndexFieldNames = 'id'
    Params = <>
    StoreDefs = True
    Left = 311
    Top = 22
    object IntegerField9: TIntegerField
      FieldName = 'id'
      Required = True
    end
    object WideStringField7: TWideStringField
      FieldName = 'name'
      Required = True
    end
    object WideStringField8: TWideStringField
      DisplayWidth = 20
      FieldName = 'mapSprite'
      Required = True
      Size = 255
    end
    object BooleanField19: TBooleanField
      FieldName = 'translucent'
      Required = True
    end
    object vehiclesShallowWater: TBooleanField
      FieldName = 'shallowWater'
      Required = True
    end
    object vehiclesdeepWater: TBooleanField
      FieldName = 'deepWater'
      Required = True
    end
    object vehicleslowLand: TBooleanField
      FieldName = 'lowLand'
      Required = True
    end
    object vehiclesmovementStyle: TByteField
      FieldName = 'movementStyle'
      Required = True
    end
    object vehiclesAltitude: TByteField
      FieldName = 'altitude'
      Required = True
    end
    object vehiclesmusic_id: TIntegerField
      FieldName = 'music_id'
      Required = True
    end
    object vehiclesmusic_FadeIn: TIntegerField
      FieldName = 'music_FadeIn'
      Required = True
    end
    object vehiclesmusic_tempo: TIntegerField
      FieldName = 'music_tempo'
      Required = True
    end
    object vehiclesmusic_volume: TIntegerField
      FieldName = 'music_volume'
      Required = True
    end
    object vehiclesmusic_balance: TIntegerField
      FieldName = 'music_balance'
      Required = True
    end
    object vehiclesmusic_name: TWideStringField
      FieldName = 'music_name'
      Required = True
      Size = 255
    end
  end
  object LegacyData: TSimpleDataSet
    Aggregates = <>
    Connection = Connection
    DataSet.CommandText = 'LegacyData'
    DataSet.CommandType = ctTable
    DataSet.MaxBlobSize = -1
    DataSet.Params = <>
    FieldDefs = <>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 824
    Top = 200
    object IntegerField11: TIntegerField
      FieldName = 'id'
      Required = True
    end
    object WideStringField9: TWideStringField
      FieldName = 'name'
      Required = True
    end
    object LegacyDataSection: TIntegerField
      FieldName = 'section'
      Required = True
    end
    object LegacyDataData: TBlobField
      FieldName = 'data'
    end
  end
  object items_AnimData: TSimpleDataSet
    Aggregates = <>
    Connection = Connection
    DataSet.CommandText = 'items_animdata'
    DataSet.CommandType = ctTable
    DataSet.MaxBlobSize = -1
    DataSet.Params = <>
    Filtered = True
    FieldDefs = <>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 368
    Top = 384
    object IntegerField25: TIntegerField
      FieldName = 'master'
      Required = True
    end
    object items_AnimDataid: TIntegerField
      FieldName = 'id'
      Required = True
    end
    object items_AnimDataname: TWideStringField
      FieldName = 'name'
      Required = True
      Size = 32
    end
    object items_AnimDataanimType: TByteField
      FieldName = 'animType'
      Required = True
    end
    object items_AnimDatawhichWeapon: TIntegerField
      FieldName = 'whichWeapon'
      Required = True
    end
    object items_AnimDatamovementMode: TByteField
      FieldName = 'movementMode'
      Required = True
    end
    object items_AnimDataafterimage: TBooleanField
      FieldName = 'afterimage'
      Required = True
    end
    object items_AnimDataattackNum: TIntegerField
      FieldName = 'attackNum'
      Required = True
    end
    object items_AnimDataranged: TBooleanField
      FieldName = 'ranged'
      Required = True
    end
    object items_AnimDatarangedProjectile: TIntegerField
      FieldName = 'rangedProjectile'
      Required = True
    end
    object items_AnimDatarangedSpeed: TIntegerField
      FieldName = 'rangedSpeed'
      Required = True
    end
    object items_AnimDatabattleAnim: TIntegerField
      FieldName = 'battleAnim'
      Required = True
    end
  end
  object tilesets_records: TSimpleDataSet
    Aggregates = <>
    Connection = Connection
    DataSet.CommandText = 'tilesets_records'
    DataSet.CommandType = ctTable
    DataSet.MaxBlobSize = -1
    DataSet.Params = <>
    Filtered = True
    FieldDefs = <>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 520
    Top = 80
    object tilesets_recordsMaster: TIntegerField
      FieldName = 'master'
      Required = True
    end
    object IntegerField26: TIntegerField
      FieldName = 'id'
      Required = True
    end
    object WideStringField10: TWideStringField
      FieldName = 'name'
      Required = True
      Size = 32
    end
    object tilesets_recordslayers: TByteField
      FieldName = 'layers'
      Required = True
    end
    object tilesets_recordsAnimDir: TByteField
      FieldName = 'AnimDir'
      Required = True
    end
    object tilesets_recordsAttributes: TBlobField
      FieldName = 'attributes'
    end
    object tilesets_recordsTerrain: TBlobField
      FieldName = 'terrain'
    end
    object tilesets_recordstilegroup: TWideStringField
      DisplayWidth = 32
      FieldName = 'tilegroup'
      Size = 255
    end
  end
  object tilegroups: TSimpleDataSet
    Aggregates = <>
    Connection = Connection
    DataSet.CommandText = 'tilegroups'
    DataSet.CommandType = ctTable
    DataSet.MaxBlobSize = -1
    DataSet.Params = <>
    FieldDefs = <>
    IndexDefs = <>
    IndexFieldNames = 'name'
    Params = <>
    StoreDefs = True
    Left = 520
    Top = 160
    object tilegroupsid: TIntegerField
      FieldName = 'id'
      Required = True
    end
    object tilegroupsname: TWideStringField
      DisplayWidth = 32
      FieldName = 'name'
      Required = True
      Size = 50
    end
    object tilegroupsLinkedFileName: TWideStringField
      FieldName = 'LinkedFileName'
      Required = True
      Size = 255
    end
    object tilegroupsOcean: TBooleanField
      FieldName = 'Ocean'
      Required = True
    end
    object tilegroupsTileType: TByteField
      FieldName = 'TileType'
      Required = True
    end
    object tilegroupsDimensions_X: TIntegerField
      FieldName = 'Dimensions_X'
      Required = True
    end
    object tilegroupsDimensions_Y: TIntegerField
      FieldName = 'Dimensions_Y'
      Required = True
    end
  end
  object syslayout: TSimpleDataSet
    Aggregates = <>
    Connection = Connection
    DataSet.CommandText = 'syslayout'
    DataSet.CommandType = ctTable
    DataSet.MaxBlobSize = -1
    DataSet.Params = <>
    FieldDefs = <>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 816
    Top = 520
    object syslayoutid: TIntegerField
      FieldName = 'id'
      Required = True
    end
    object syslayoutname: TWideStringField
      FieldName = 'name'
      Required = True
    end
    object syslayoutWidth: TIntegerField
      FieldName = 'Width'
      Required = True
    end
    object syslayoutHeight: TIntegerField
      FieldName = 'Height'
      Required = True
    end
    object syslayoutPWidth: TIntegerField
      FieldName = 'PWidth'
      Required = True
    end
    object syslayoutPHeight: TIntegerField
      FieldName = 'PHeight'
      Required = True
    end
    object syslayoutTitleScreen: TWideStringField
      FieldName = 'TitleScreen'
      Size = 32
    end
    object syslayoutGameOverScreen: TWideStringField
      FieldName = 'GameOverScreen'
      Size = 32
    end
    object syslayoutSysGraphic: TWideStringField
      FieldName = 'SysGraphic'
      Size = 32
    end
    object syslayoutBattleSysGraphic: TWideStringField
      FieldName = 'BattleSysGraphic'
      Size = 32
    end
    object syslayoutEditorBattleBG: TWideStringField
      FieldName = 'EditorBattleBG'
      Size = 32
    end
    object syslayoutWallpaperStretch: TBooleanField
      FieldName = 'WallpaperStretch'
    end
    object syslayoutWhichFont: TByteField
      FieldName = 'WhichFont'
    end
    object syslayoutStartingHeroes: TIntegerField
      FieldName = 'StartingHeroes'
    end
    object syslayoutStartingHero_1: TIntegerField
      FieldName = 'StartingHero_1'
    end
    object syslayoutStartingHero_2: TIntegerField
      FieldName = 'StartingHero_2'
    end
    object syslayoutStartingHero_3: TIntegerField
      FieldName = 'StartingHero_3'
    end
    object syslayoutStartingHero_4: TIntegerField
      FieldName = 'StartingHero_4'
    end
    object syslayoutUsesFrame: TBooleanField
      FieldName = 'UsesFrame'
    end
    object syslayoutframe: TWideStringField
      FieldName = 'frame'
      Size = 32
    end
    object syslayoutreverseGraphics: TBooleanField
      FieldName = 'reverseGraphics'
    end
    object syslayoutTransition_1: TByteField
      FieldName = 'Transition_1'
    end
    object syslayoutTransition_2: TByteField
      FieldName = 'Transition_2'
    end
    object syslayoutTransition_3: TByteField
      FieldName = 'Transition_3'
    end
    object syslayoutTransition_4: TByteField
      FieldName = 'Transition_4'
    end
    object syslayoutTransition_5: TByteField
      FieldName = 'Transition_5'
    end
    object syslayoutTransition_6: TByteField
      FieldName = 'Transition_6'
    end
    object syslayoutCommands_1: TByteField
      FieldName = 'Commands_1'
    end
    object syslayoutCommands_2: TByteField
      FieldName = 'Commands_2'
    end
    object syslayoutCommands_3: TByteField
      FieldName = 'Commands_3'
    end
    object syslayoutCommands_4: TByteField
      FieldName = 'Commands_4'
    end
    object syslayoutCommands_5: TByteField
      FieldName = 'Commands_5'
    end
    object syslayoutCommands_6: TByteField
      FieldName = 'Commands_6'
    end
    object syslayoutCommands_7: TByteField
      FieldName = 'Commands_7'
    end
    object syslayoutCommands_8: TByteField
      FieldName = 'Commands_8'
    end
    object syslayoutTranslucentMessages: TBooleanField
      FieldName = 'translucentMessages'
    end
  end
  object MapTree: TSimpleDataSet
    Aggregates = <>
    Connection = Connection
    DataSet.CommandText = 'MapTree'
    DataSet.CommandType = ctTable
    DataSet.MaxBlobSize = -1
    DataSet.Params = <>
    FieldDefs = <>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 376
    Top = 280
    object MapTreeCurrentMap: TWordField
      FieldName = 'currentMap'
      Required = True
    end
    object MapTreeMapEngines: TWideMemoField
      FieldName = 'mapEngines'
      Required = True
      BlobType = ftWideMemo
    end
  end
  object StartLocs: TSimpleDataSet
    Aggregates = <>
    Connection = Connection
    DataSet.CommandText = 'StartLocs'
    DataSet.CommandType = ctTable
    DataSet.MaxBlobSize = -1
    DataSet.Params = <>
    FieldDefs = <>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 448
    Top = 288
    object StartLocsid: TIntegerField
      FieldName = 'id'
      Required = True
    end
    object StartLocsmap: TIntegerField
      FieldName = 'map'
      ProviderFlags = [pfInUpdate]
      Required = True
    end
    object StartLocsx: TSmallintField
      FieldName = 'x'
      ProviderFlags = [pfInUpdate]
      Required = True
    end
    object StartLocsy: TSmallintField
      FieldName = 'y'
      ProviderFlags = [pfInUpdate]
      Required = True
    end
  end
  object metadata: TSimpleDataSet
    Aggregates = <>
    Connection = Connection
    DataSet.CommandText = 'metadata'
    DataSet.CommandType = ctTable
    DataSet.MaxBlobSize = -1
    DataSet.Params = <>
    FieldDefs = <>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 512
    Top = 272
    object metadataId: TIntegerField
      FieldName = 'id'
      Required = True
    end
    object metadataName: TWideStringField
      FieldName = 'name'
      Required = True
    end
    object metadataParent: TSmallintField
      FieldName = 'parent'
      Required = True
    end
    object metadatascrollPositionx: TIntegerField
      FieldName = 'scrollPosition_x'
      Required = True
    end
    object metadataScrollPositiony: TIntegerField
      FieldName = 'scrollPosition_y'
      Required = True
    end
    object metadataTreeOpen: TBooleanField
      FieldName = 'treeOpen'
      Required = True
    end
    object metadataBgmState: TByteField
      FieldName = 'bgmState'
      Required = True
    end
    object skills_BgmDataid: TIntegerField
      FieldName = 'BgmData_id'
      Required = True
    end
    object skills_BgmDataname: TWideStringField
      FieldName = 'BgmData_name'
      Required = True
      Size = 255
    end
    object skills_BgmDataFadeIn: TIntegerField
      FieldName = 'BgmData_fadeIn'
      Required = True
    end
    object skills_BgmDataTempo: TIntegerField
      FieldName = 'BgmData_tempo'
      Required = True
    end
    object skills_BgmDataVolume: TIntegerField
      FieldName = 'BgmData_volume'
      Required = True
    end
    object skills_BgmDataBalance: TIntegerField
      FieldName = 'BgmData_Balance'
      Required = True
    end
    object metadataBattleBgState: TByteField
      FieldName = 'battleBgState'
      Required = True
    end
    object metadataBattleBgName: TWideStringField
      FieldName = 'battleBgName'
      Required = True
      Size = 255
    end
    object metadataCanPort: TByteField
      FieldName = 'canPort'
      Required = True
    end
    object metadataCanEscape: TByteField
      FieldName = 'canEscape'
      Required = True
    end
    object metadataCanSave: TByteField
      FieldName = 'canSave'
      Required = True
    end
    object metadataInternalFilenameName: TWideStringField
      FieldName = 'internalFilename_Name'
      Required = True
      Size = 255
    end
    object metadataInternalFilenameDuplicates: TIntegerField
      FieldName = 'internalFilename_duplicates'
      Required = True
    end
    object metadataMapEngine: TShortintField
      FieldName = 'mapEngine'
      Required = True
    end
  end
  object dbData: TSimpleDataSet
    Aggregates = <>
    Connection = Connection
    DataSet.CommandText = 'dbData'
    DataSet.CommandType = ctTable
    DataSet.MaxBlobSize = -1
    DataSet.Params = <>
    FieldDefs = <>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 816
    Top = 464
    object IntegerField27: TIntegerField
      FieldName = 'id'
      Required = True
    end
    object WideStringField11: TWideStringField
      FieldName = 'name'
      Required = True
    end
    object dbDatamoveMatrix: TBlobField
      FieldName = 'moveMatrix'
      Required = True
    end
    object dbDataStatset: TBlobField
      FieldName = 'statset'
      Required = True
    end
    object dbDataScriptFormat: TByteField
      FieldName = 'scriptFormat'
      Required = True
    end
    object dbDatascriptFile: TWideStringField
      FieldName = 'scriptFile'
      Required = True
      Size = 255
    end
    object dbDataMapStyles: TBlobField
      FieldName = 'mapStyles'
      Required = True
    end
    object dbDataBattleStyles: TBlobField
      FieldName = 'battleStyles'
      Required = True
    end
  end
  object boot: TSimpleDataSet
    Aggregates = <>
    Connection = Connection
    DataSet.CommandText = 'boot'
    DataSet.CommandType = ctTable
    DataSet.MaxBlobSize = -1
    DataSet.Params = <>
    FieldDefs = <>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 816
    Top = 408
    object IntegerField28: TIntegerField
      FieldName = 'id'
      Required = True
    end
    object EngineName: TWideStringField
      FieldName = 'EngineName'
      Required = True
      Size = 32
    end
    object bootversion: TIntegerField
      FieldName = 'version'
      Required = True
    end
  end
  object metadata_regions: TSimpleDataSet
    Aggregates = <>
    Connection = Connection
    DataSet.CommandText = 'metadata_regions'
    DataSet.CommandType = ctTable
    DataSet.MaxBlobSize = -1
    DataSet.Params = <>
    Filtered = True
    FieldDefs = <>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 512
    Top = 328
    object metadata_regionsMaster: TIntegerField
      FieldName = 'master'
      Required = True
    end
    object IntegerField29: TIntegerField
      FieldName = 'id'
      Required = True
    end
    object WideStringField12: TWideStringField
      FieldName = 'name'
      Required = True
    end
    object metadata_regionsbounds_left: TIntegerField
      FieldName = 'bounds_left'
      Required = True
    end
    object metadata_regionsbounds_right: TIntegerField
      FieldName = 'bounds_right'
      Required = True
    end
    object metadata_regionsbounds_top: TIntegerField
      FieldName = 'bounds_top'
      Required = True
    end
    object metadata_regionsbounds_bottom: TIntegerField
      FieldName = 'bounds_bottom'
      Required = True
    end
    object metadata_regionsencounterScript: TWideStringField
      FieldName = 'encounterScript'
      Required = True
      Size = 255
    end
    object metadata_regionsencounters_1: TIntegerField
      FieldName = 'encounters_1'
      Required = True
    end
    object metadata_regionsencounters_2: TIntegerField
      FieldName = 'encounters_2'
      Required = True
    end
    object metadata_regionsencounters_3: TIntegerField
      FieldName = 'encounters_3'
      Required = True
    end
    object metadata_regionsencounters_4: TIntegerField
      FieldName = 'encounters_4'
      Required = True
    end
    object metadata_regionsbattles: TBlobField
      FieldName = 'battles'
    end
  end
  object monsters: TSimpleDataSet
    Aggregates = <>
    Connection = Connection
    DataSet.CommandText = 'monsters'
    DataSet.CommandType = ctTable
    DataSet.MaxBlobSize = -1
    DataSet.Params = <>
    FieldDefs = <>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 600
    Top = 8
    object IntegerField32: TIntegerField
      FieldName = 'id'
      Required = True
    end
    object WideStringField13: TWideStringField
      FieldName = 'name'
      Required = True
      Size = 32
    end
    object monstersFilename: TWideStringField
      FieldName = 'Filename'
      Required = True
    end
    object monstersTransparent: TBooleanField
      FieldName = 'Transparent'
      Required = True
    end
    object monstersFlying: TBooleanField
      FieldName = 'Flying'
      Required = True
    end
    object monstersColorShift: TIntegerField
      FieldName = 'ColorShift'
      Required = True
    end
    object monstersExp: TIntegerField
      FieldName = 'Exp'
      Required = True
    end
    object monstersMoney: TIntegerField
      FieldName = 'Money'
      Required = True
    end
    object monstersItem: TIntegerField
      FieldName = 'Item'
      Required = True
    end
    object monstersItemChance: TIntegerField
      FieldName = 'ItemChance'
      Required = True
    end
    object monstersCanCrit: TBooleanField
      FieldName = 'CanCrit'
      Required = True
    end
    object monstersCritChance: TIntegerField
      FieldName = 'CritChance'
      Required = True
    end
    object monstersOftenMiss: TBooleanField
      FieldName = 'OftenMiss'
      Required = True
    end
    object monstersstats_1: TIntegerField
      FieldName = 'stats_1'
      Required = True
    end
    object monstersstats_2: TIntegerField
      FieldName = 'stats_2'
      Required = True
    end
    object monstersstats_3: TIntegerField
      FieldName = 'stats_3'
      Required = True
    end
    object monstersstats_4: TIntegerField
      FieldName = 'stats_4'
      Required = True
    end
    object monstersstats_5: TIntegerField
      FieldName = 'stats_5'
      Required = True
    end
    object monstersstats_6: TIntegerField
      FieldName = 'stats_6'
      Required = True
    end
    object monsterstag_1: TIntegerField
      FieldName = 'tag_1'
      Required = True
    end
    object monsterstag_2: TIntegerField
      FieldName = 'tag_2'
      Required = True
    end
    object monsterstag_3: TIntegerField
      FieldName = 'tag_3'
      Required = True
    end
    object monsterstag_4: TIntegerField
      FieldName = 'tag_4'
      Required = True
    end
  end
  object monsters_Conditions: TSimpleDataSet
    Aggregates = <>
    Connection = Connection
    DataSet.CommandText = 'monsters_Conditions'
    DataSet.CommandType = ctTable
    DataSet.MaxBlobSize = -1
    DataSet.Params = <>
    Filtered = True
    FieldDefs = <>
    IndexDefs = <
      item
        Name = 'DEFAULT_ORDER'
      end
      item
        Name = 'CHANGEINDEX'
      end>
    IndexFieldNames = 'master'
    MasterFields = 'id'
    PacketRecords = 0
    Params = <>
    StoreDefs = True
    Left = 599
    Top = 118
    object IntegerField33: TIntegerField
      FieldName = 'master'
      Required = True
    end
    object IntegerField34: TIntegerField
      FieldName = 'x'
      Required = True
    end
    object IntegerField35: TIntegerField
      FieldName = 'y'
      Required = True
    end
  end
  object monsters_Resists: TSimpleDataSet
    Aggregates = <>
    Connection = Connection
    DataSet.CommandText = 'monsters_Resists'
    DataSet.CommandType = ctTable
    DataSet.MaxBlobSize = -1
    DataSet.Params = <>
    Filtered = True
    FieldDefs = <>
    IndexDefs = <
      item
        Name = 'DEFAULT_ORDER'
      end
      item
        Name = 'CHANGEINDEX'
      end>
    IndexFieldNames = 'master'
    MasterFields = 'id'
    PacketRecords = 0
    Params = <>
    StoreDefs = True
    Left = 599
    Top = 62
    object monsters_Resistsmaster: TIntegerField
      FieldName = 'master'
      Required = True
    end
    object monsters_Resistsx: TIntegerField
      FieldName = 'x'
      Required = True
    end
    object monsters_Resistsy: TIntegerField
      FieldName = 'y'
      Required = True
    end
  end
  object mparties: TSimpleDataSet
    Aggregates = <>
    Connection = Connection
    DataSet.CommandText = 'mparties'
    DataSet.CommandType = ctTable
    DataSet.MaxBlobSize = -1
    DataSet.Params = <>
    FieldDefs = <>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 592
    Top = 176
    object IntegerField38: TIntegerField
      FieldName = 'id'
      Required = True
    end
    object WideStringField14: TWideStringField
      FieldName = 'name'
      Required = True
      Size = 32
    end
    object mpartiesAutoAlign: TBooleanField
      FieldName = 'autoAlign'
    end
    object mpartiesHabitats: TBlobField
      FieldName = 'habitats'
    end
    object mpartiesRandom: TBooleanField
      FieldName = 'random'
    end
  end
  object mparties_monsters: TSimpleDataSet
    Aggregates = <>
    Connection = Connection
    DataSet.CommandText = 'mparties_monsters'
    DataSet.CommandType = ctTable
    DataSet.MaxBlobSize = -1
    DataSet.Params = <>
    FieldDefs = <>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 592
    Top = 232
    object mparties_monstersMaster: TIntegerField
      FieldName = 'master'
      Required = True
    end
    object IntegerField39: TIntegerField
      FieldName = 'id'
      Required = True
    end
    object mparties_monstersMonster: TIntegerField
      FieldName = 'monster'
      Required = True
    end
    object mparties_monstersPosition_x: TIntegerField
      FieldName = 'position_x'
      Required = True
    end
    object mparties_monstersPosition_y: TIntegerField
      FieldName = 'position_y'
      Required = True
    end
    object mparties_monstersInvisible: TBooleanField
      FieldName = 'invisible'
      Required = True
    end
  end
  object mparties_events: TSimpleDataSet
    Aggregates = <>
    Connection = Connection
    DataSet.CommandText = 'mparties_events'
    DataSet.CommandType = ctTable
    DataSet.MaxBlobSize = -1
    DataSet.Params = <>
    FieldDefs = <>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 592
    Top = 288
    object IntegerField40: TIntegerField
      FieldName = 'master'
      Required = True
    end
    object IntegerField41: TIntegerField
      FieldName = 'id'
      Required = True
    end
    object mparties_eventsbSwitch1: TBooleanField
      FieldName = 'bSwitch1'
    end
    object mparties_eventsbSwitch2: TBooleanField
      FieldName = 'bSwitch2'
    end
    object mparties_eventsbVar1: TBooleanField
      FieldName = 'bVar1'
    end
    object mparties_eventsbVar2: TBooleanField
      FieldName = 'bVar2'
    end
    object mparties_eventsbItem: TBooleanField
      FieldName = 'bItem'
    end
    object mparties_eventsbHero: TBooleanField
      FieldName = 'bHero'
    end
    object mparties_eventsbTimer1: TBooleanField
      FieldName = 'bTimer1'
    end
    object mparties_eventsbTimer2: TBooleanField
      FieldName = 'bTimer2'
    end
    object mparties_eventsbTurns: TBooleanField
      FieldName = 'bTurns'
    end
    object mparties_eventsbMonsterTime: TBooleanField
      FieldName = 'bMonsterTime'
    end
    object mparties_eventsbHeroTime: TBooleanField
      FieldName = 'bHeroTime'
    end
    object mparties_eventsbExhaustion: TBooleanField
      FieldName = 'bExhaustion'
    end
    object mparties_eventsbMonsterHP: TBooleanField
      FieldName = 'bMonsterHP'
    end
    object mparties_eventsbHeroHP: TBooleanField
      FieldName = 'bHeroHP'
    end
    object mparties_eventsbCommandUsed: TBooleanField
      FieldName = 'bCommandUsed'
    end
    object mparties_eventsClock1Mins: TIntegerField
      FieldName = 'Clock1Mins'
    end
    object mparties_eventsClock1Secs: TIntegerField
      FieldName = 'Clock1Secs'
    end
    object mparties_eventsClock2Mins: TIntegerField
      FieldName = 'Clock2Mins'
    end
    object mparties_eventsClock2Secs: TIntegerField
      FieldName = 'Clock2Secs'
    end
    object mparties_eventsconditions_Switch1: TIntegerField
      FieldName = 'conditions_Switch1'
      Required = True
    end
    object mparties_eventsconditions_Switch2: TIntegerField
      FieldName = 'conditions_Switch2'
      Required = True
    end
    object mparties_eventsconditions_Variable1: TIntegerField
      FieldName = 'conditions_Variable1'
      Required = True
    end
    object mparties_eventsconditions_Variable2: TIntegerField
      FieldName = 'conditions_Variable2'
      Required = True
    end
    object mparties_eventsconditions_Var1Op: TByteField
      FieldName = 'conditions_Var1Op'
      Required = True
    end
    object mparties_eventsconditions_Var2Op: TByteField
      FieldName = 'conditions_Var2Op'
      Required = True
    end
    object mparties_eventsconditions_VarValue1: TIntegerField
      FieldName = 'conditions_VarValue1'
      Required = True
    end
    object mparties_eventsconditions_VarValue2: TIntegerField
      FieldName = 'conditions_VarValue2'
      Required = True
    end
    object mparties_eventsconditions_Item: TIntegerField
      FieldName = 'conditions_Item'
      Required = True
    end
    object mparties_eventsconditions_Hero: TIntegerField
      FieldName = 'conditions_Hero'
      Required = True
    end
    object mparties_eventsconditions_Clock1Op: TByteField
      FieldName = 'conditions_Clock1Op'
      Required = True
    end
    object mparties_eventsconditions_Clock2Op: TByteField
      FieldName = 'conditions_Clock2Op'
      Required = True
    end
    object mparties_eventsconditions_Script: TWideStringField
      FieldName = 'conditions_Script'
      Required = True
    end
    object mparties_eventsconditions_MonsterHP: TIntegerField
      FieldName = 'conditions_MonsterHP'
      Required = True
    end
    object mparties_eventsconditions_TurnsMultiple: TIntegerField
      FieldName = 'conditions_TurnsMultiple'
      Required = True
    end
    object mparties_eventsconditions_MonsterHPMax: TIntegerField
      FieldName = 'conditions_MonsterHPMax'
      Required = True
    end
    object mparties_eventsconditions_HeroCommandWhich: TIntegerField
      FieldName = 'conditions_HeroCommandWhich'
      Required = True
    end
    object mparties_eventsconditions_HeroHP: TIntegerField
      FieldName = 'conditions_HeroHP'
      Required = True
    end
    object mparties_eventsconditions_HeroHPMax: TIntegerField
      FieldName = 'conditions_HeroHPMax'
      Required = True
    end
    object mparties_eventsconditions_ExhaustionMax: TIntegerField
      FieldName = 'conditions_ExhaustionMax'
      Required = True
    end
    object mparties_eventsconditions_MonsterTurnsMultiple: TIntegerField
      FieldName = 'conditions_MonsterTurnsMultiple'
      Required = True
    end
    object mparties_eventsconditions_HeroTurnsMultiple: TIntegerField
      FieldName = 'conditions_HeroTurnsMultiple'
      Required = True
    end
    object mparties_eventsconditions_MonsterHPMin: TIntegerField
      FieldName = 'conditions_MonsterHPMin'
      Required = True
    end
    object mparties_eventsconditions_HeroHPMin: TIntegerField
      FieldName = 'conditions_HeroHPMin'
      Required = True
    end
    object mparties_eventsconditions_ExhaustionMin: TIntegerField
      FieldName = 'conditions_ExhaustionMin'
      Required = True
    end
    object mparties_eventsconditions_TurnsConst: TIntegerField
      FieldName = 'conditions_TurnsConst'
      Required = True
    end
    object mparties_eventsconditions_MonsterTurnsConst: TIntegerField
      FieldName = 'conditions_MonsterTurnsConst'
      Required = True
    end
    object mparties_eventsconditions_HeroTurnsConst: TIntegerField
      FieldName = 'conditions_HeroTurnsConst'
      Required = True
    end
    object mparties_eventsconditions_MonsterTurn: TIntegerField
      FieldName = 'conditions_MonsterTurn'
      Required = True
    end
    object mparties_eventsconditions_HeroCommandWho: TIntegerField
      FieldName = 'conditions_HeroCommandWho'
      Required = True
    end
    object mparties_eventsconditions_HeroTurn: TIntegerField
      FieldName = 'conditions_HeroTurn'
      Required = True
    end
    object mparties_eventsEventText: TWideMemoField
      FieldName = 'eventText'
      Required = True
      BlobType = ftWideMemo
    end
  end
  object battleChars: TSimpleDataSet
    Aggregates = <>
    Connection = Connection
    DataSet.CommandText = 'battleChars'
    DataSet.CommandType = ctTable
    DataSet.MaxBlobSize = -1
    DataSet.Params = <>
    FieldDefs = <>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 680
    Top = 24
    object IntegerField42: TIntegerField
      FieldName = 'id'
      Required = True
    end
    object WideStringField15: TWideStringField
      FieldName = 'name'
      Required = True
      Size = 32
    end
    object battleCharsSpeed: TIntegerField
      FieldName = 'speed'
      Required = True
    end
  end
  object battleChars_Poses: TSimpleDataSet
    Aggregates = <>
    Connection = Connection
    DataSet.CommandText = 'battleChars_Poses'
    DataSet.CommandType = ctTable
    DataSet.MaxBlobSize = -1
    DataSet.Params = <>
    FieldDefs = <>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 680
    Top = 80
    object battleChars_PosesMaster: TIntegerField
      FieldName = 'master'
      Required = True
    end
    object IntegerField43: TIntegerField
      FieldName = 'id'
      Required = True
    end
    object WideStringField16: TWideStringField
      FieldName = 'name'
      Required = True
      Size = 32
    end
    object WideStringField17: TWideStringField
      FieldName = 'Filename'
      Required = True
    end
    object battleChars_Posesframe: TIntegerField
      FieldName = 'frame'
      Required = True
    end
    object battleChars_Posesunk04: TIntegerField
      FieldName = 'unk04'
      Required = True
    end
    object battleChars_Posesunk05: TIntegerField
      FieldName = 'unk05'
      Required = True
    end
  end
  object battleChars_Weapons: TSimpleDataSet
    Aggregates = <>
    Connection = Connection
    DataSet.CommandText = 'battleChars_Weapons'
    DataSet.CommandType = ctTable
    DataSet.MaxBlobSize = -1
    DataSet.Params = <>
    FieldDefs = <>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 680
    Top = 136
    object battleChars_Weaponsmaster: TIntegerField
      FieldName = 'master'
      Required = True
    end
    object IntegerField53: TIntegerField
      FieldName = 'id'
      Required = True
    end
    object WideStringField18: TWideStringField
      Alignment = taRightJustify
      FieldName = 'name'
      Required = True
      Size = 32
    end
    object WideStringField19: TWideStringField
      Alignment = taRightJustify
      FieldName = 'Filename'
      Required = True
    end
    object IntegerField54: TIntegerField
      FieldName = 'frame'
      Required = True
    end
    object IntegerField55: TIntegerField
      FieldName = 'unk04'
      Required = True
    end
    object IntegerField56: TIntegerField
      FieldName = 'unk05'
      Required = True
    end
  end
  object terrain: TSimpleDataSet
    Aggregates = <>
    Connection = Connection
    DataSet.CommandText = 'TERRAIN'
    DataSet.CommandType = ctTable
    DataSet.MaxBlobSize = -1
    DataSet.Params = <>
    FieldDefs = <>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 431
    Top = 198
    object IntegerField4: TIntegerField
      FieldName = 'id'
      Required = True
    end
    object WideStringField20: TWideStringField
      FieldName = 'name'
      Required = True
    end
    object terrainDamage: TIntegerField
      FieldName = 'Damage'
      Required = True
    end
    object terrainEncounterMultiplier: TIntegerField
      FieldName = 'EncounterMultiplier'
      Required = True
    end
    object terrainBattleBg: TWideStringField
      FieldName = 'BattleBg'
      Required = True
    end
    object terrainAirshipLanding: TBooleanField
      FieldName = 'AirshipLanding'
      Required = True
    end
    object terrainFrame: TWideStringField
      FieldName = 'Frame'
      Required = True
    end
    object terrainsoundEffect_id: TIntegerField
      FieldName = 'soundEffect_id'
    end
    object terrainsoundEffect_name: TWideStringField
      FieldName = 'soundEffect_name'
      Size = 255
    end
    object terrainsoundEffect_fadeIn: TIntegerField
      FieldName = 'soundEffect_fadeIn'
    end
    object terrainsoundEffect_tempo: TIntegerField
      FieldName = 'soundEffect_tempo'
    end
    object terrainsoundEffect_volume: TIntegerField
      FieldName = 'soundEffect_volume'
    end
    object terrainsoundEffect_Balance: TIntegerField
      FieldName = 'soundEffect_Balance'
    end
    object terrainConcealment: TByteField
      FieldName = 'Concealment'
      Required = True
    end
    object terrainVehiclePass: TBlobField
      FieldName = 'VehiclePass'
      Required = True
    end
  end
  object SysSound: TSimpleDataSet
    Aggregates = <>
    Connection = Connection
    DataSet.CommandText = 'SysSound'
    DataSet.CommandType = ctTable
    DataSet.MaxBlobSize = -1
    DataSet.Params = <>
    FieldDefs = <>
    IndexDefs = <
      item
        Name = 'SysSoundIndexPK'
        Fields = 'id;ismusic'
        Options = [ixPrimary, ixUnique]
      end>
    IndexName = 'SysSoundIndexPK'
    Params = <>
    StoreDefs = True
    Left = 432
    Top = 368
    object IntegerField52: TIntegerField
      FieldName = 'id'
      Required = True
    end
    object SysSoundisMusic: TBooleanField
      FieldName = 'isMusic'
      Required = True
    end
    object SysSoundFadeIn: TIntegerField
      FieldName = 'FadeIn'
    end
    object SysSoundTempo: TIntegerField
      FieldName = 'tempo'
    end
    object SysSoundVolume: TIntegerField
      FieldName = 'volume'
    end
    object SysSoundBalance: TIntegerField
      FieldName = 'balance'
    end
    object SysSoundName: TWideStringField
      FieldName = 'name'
      Size = 255
    end
  end
  object MapObjects: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 752
    Top = 64
    object MapObjectsid: TIntegerField
      FieldName = 'id'
    end
    object MapObjectsname: TWideStringField
      FieldName = 'name'
      Size = 255
    end
  end
  object script_cache: TSimpleDataSet
    Aggregates = <>
    Connection = Connection
    DataSet.CommandText = 'script_cache'
    DataSet.CommandType = ctTable
    DataSet.MaxBlobSize = -1
    DataSet.Params = <>
    FieldDefs = <>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    OnReconcileError = script_cacheReconcileError
    Left = 752
    Top = 344
    object IntegerField57: TIntegerField
      FieldName = 'id'
      Required = True
    end
    object script_cachescript: TWideMemoField
      FieldName = 'script'
      Required = True
      BlobType = ftWideMemo
    end
  end
end
