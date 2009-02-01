object dmDatabase: TdmDatabase
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 553
  Width = 882
  object charClasses: TClientDataSet
    Aggregates = <>
    FieldDefs = <
      item
        Name = 'id'
        DataType = ftInteger
      end
      item
        Name = 'name'
        DataType = ftString
        Size = 20
      end
      item
        Name = 'modified'
        DataType = ftBoolean
      end
      item
        Name = 'mapSprite'
        DataType = ftInteger
      end
      item
        Name = 'battleSprite'
        DataType = ftInteger
      end
      item
        Name = 'portrait'
        DataType = ftInteger
      end
      item
        Name = 'command'
        ChildDefs = <
          item
            Name = 'command[0]'
            DataType = ftInteger
          end>
        DataType = ftArray
        Size = 7
      end
      item
        Name = 'commandField2'
        DataType = ftInteger
      end
      item
        Name = 'commandField3'
        DataType = ftInteger
      end
      item
        Name = 'commandField4'
        DataType = ftInteger
      end
      item
        Name = 'commandField5'
        DataType = ftInteger
      end
      item
        Name = 'commandField6'
        DataType = ftInteger
      end
      item
        Name = 'commandField7'
        DataType = ftInteger
      end
      item
        Name = 'statblock'
        ChildDefs = <
          item
            Name = 'statblock[0]'
            DataType = ftLargeint
          end>
        DataType = ftArray
        Size = 6
      end
      item
        Name = 'Sp'
        DataType = ftLargeint
      end
      item
        Name = 'Attack'
        DataType = ftLargeint
      end
      item
        Name = 'Defense'
        DataType = ftLargeint
      end
      item
        Name = 'Mind'
        DataType = ftLargeint
      end
      item
        Name = 'Speed'
        DataType = ftLargeint
      end
      item
        Name = 'expFunc'
        DataType = ftString
        Size = 32
      end
      item
        Name = 'expVars'
        ChildDefs = <
          item
            Name = 'expVars[0]'
            DataType = ftInteger
          end>
        DataType = ftArray
        Size = 4
      end
      item
        Name = 'expVarsField2'
        DataType = ftInteger
      end
      item
        Name = 'expVarsField3'
        DataType = ftInteger
      end
      item
        Name = 'expVarsField4'
        DataType = ftInteger
      end
      item
        Name = 'dualWield'
        DataType = ftInteger
      end
      item
        Name = 'staticEq'
        DataType = ftBoolean
      end
      item
        Name = 'strongDef'
        DataType = ftBoolean
      end
      item
        Name = 'unarmedAnim'
        DataType = ftInteger
      end
      item
        Name = 'animName'
        DataType = ftString
        Size = 32
      end
      item
        Name = 'equip'
        ChildDefs = <
          item
            Name = 'equip[0]'
            DataType = ftInteger
          end>
        DataType = ftArray
        Size = 5
      end
      item
        Name = 'equipField2'
        DataType = ftInteger
      end
      item
        Name = 'equipField3'
        DataType = ftInteger
      end
      item
        Name = 'equipField4'
        DataType = ftInteger
      end
      item
        Name = 'equipField5'
        DataType = ftInteger
      end>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 8
    Top = 8
    object charClassesid: TIntegerField
      FieldName = 'id'
    end
    object charClassesname: TStringField
      FieldName = 'name'
    end
    object charClassesmodified: TBooleanField
      FieldName = 'modified'
    end
    object charClassesmapSprite: TIntegerField
      FieldName = 'mapSprite'
    end
    object charClassesbattleSprite: TIntegerField
      FieldName = 'battleSprite'
    end
    object charClassesportrait: TIntegerField
      FieldName = 'portrait'
    end
    object charClassescommand: TArrayField
      FieldName = 'command'
      object charClassescommand0: TIntegerField
        FieldName = 'command[0]'
      end
      object charClassescommand1: TIntegerField
        FieldName = 'command[1]'
      end
      object charClassescommand2: TIntegerField
        FieldName = 'command[2]'
      end
      object charClassescommand3: TIntegerField
        FieldName = 'command[3]'
      end
      object charClassescommand4: TIntegerField
        FieldName = 'command[4]'
      end
      object charClassescommand5: TIntegerField
        FieldName = 'command[5]'
      end
      object charClassescommand6: TIntegerField
        FieldName = 'command[6]'
      end
    end
    object charClassesstatblock: TArrayField
      FieldName = 'statblock'
      object charClassesstatblock0: TLargeintField
        FieldName = 'statblock[0]'
      end
      object charClassesstatblock1: TLargeintField
        FieldName = 'statblock[1]'
      end
      object charClassesstatblock2: TLargeintField
        FieldName = 'statblock[2]'
      end
      object charClassesstatblock3: TLargeintField
        FieldName = 'statblock[3]'
      end
      object charClassesstatblock4: TLargeintField
        FieldName = 'statblock[4]'
      end
      object charClassesstatblock5: TLargeintField
        FieldName = 'statblock[5]'
      end
    end
    object charClassesexpFunc: TStringField
      FieldName = 'expFunc'
      Size = 32
    end
    object charClassesexpVars: TArrayField
      FieldName = 'expVars'
      object charClassesexpVars0: TIntegerField
        FieldName = 'expVars[0]'
      end
      object charClassesexpVars1: TIntegerField
        FieldName = 'expVars[1]'
      end
      object charClassesexpVars2: TIntegerField
        FieldName = 'expVars[2]'
      end
      object charClassesexpVars3: TIntegerField
        FieldName = 'expVars[3]'
      end
    end
    object charClassesdualWield: TIntegerField
      FieldName = 'dualWield'
    end
    object charClassesstaticEq: TBooleanField
      FieldName = 'staticEq'
    end
    object charClassesstrongDef: TBooleanField
      FieldName = 'strongDef'
    end
    object charClassesunarmedAnim: TIntegerField
      FieldName = 'unarmedAnim'
    end
    object charClassesequip: TArrayField
      FieldName = 'equip'
      object charClassesequip0: TIntegerField
        FieldName = 'equip[0]'
      end
      object charClassesequip1: TIntegerField
        FieldName = 'equip[1]'
      end
      object charClassesequip2: TIntegerField
        FieldName = 'equip[2]'
      end
      object charClassesequip3: TIntegerField
        FieldName = 'equip[3]'
      end
      object charClassesequip4: TIntegerField
        FieldName = 'equip[4]'
      end
    end
    object charClassesanimName: TStringField
      FieldKind = fkLookup
      FieldName = 'animName'
      LookupDataSet = animations
      LookupKeyFields = 'id'
      LookupResultField = 'name'
      KeyFields = 'unarmedAnim'
      Size = 32
      Lookup = True
    end
    object charClassesweaponName: TStringField
      FieldKind = fkLookup
      FieldName = 'weaponName'
      LookupDataSet = weapons
      LookupKeyFields = 'id'
      LookupResultField = 'name'
      KeyFields = 'equip[0]'
      Size = 32
      Lookup = True
    end
    object charClassesweapon2Name: TStringField
      FieldKind = fkLookup
      FieldName = 'weapon2Name'
      LookupDataSet = weapons
      LookupKeyFields = 'id'
      LookupResultField = 'name'
      KeyFields = 'equip[1]'
      Size = 32
      Lookup = True
    end
    object charClassesOffhandName: TStringField
      FieldKind = fkLookup
      FieldName = 'offhandName'
      LookupDataSet = offhands
      LookupKeyFields = 'id'
      LookupResultField = 'name'
      KeyFields = 'equip[1]'
      Size = 32
      Lookup = True
    end
    object charClassesshieldName: TStringField
      FieldKind = fkLookup
      FieldName = 'shieldName'
      LookupDataSet = shields
      LookupKeyFields = 'id'
      LookupResultField = 'name'
      KeyFields = 'equip[1]'
      Size = 32
      Lookup = True
    end
    object charClassesArmorName: TStringField
      FieldKind = fkLookup
      FieldName = 'armorName'
      LookupDataSet = armors
      LookupKeyFields = 'id'
      LookupResultField = 'name'
      KeyFields = 'equip[2]'
      Size = 32
      Lookup = True
    end
    object charClassesHelmetName: TStringField
      FieldKind = fkLookup
      FieldName = 'helmetName'
      LookupDataSet = helmets
      LookupKeyFields = 'id'
      LookupResultField = 'name'
      KeyFields = 'equip[3]'
      Size = 32
      Lookup = True
    end
    object charClassesAccessoryName: TStringField
      FieldKind = fkLookup
      FieldName = 'accessoryName'
      LookupDataSet = accessories
      LookupKeyFields = 'id'
      LookupResultField = 'name'
      KeyFields = 'equip[4]'
      Size = 32
      Lookup = True
    end
    object charClassesExpFuncDesignName: TStringField
      FieldKind = fkLookup
      FieldName = 'expFuncDesignName'
      LookupDataSet = expCalcRecords
      LookupKeyFields = 'name'
      LookupResultField = 'designName'
      KeyFields = 'expFunc'
      Size = 50
      Lookup = True
    end
  end
  object charClasses_skillset: TClientDataSet
    Aggregates = <>
    FieldDefs = <
      item
        Name = 'modified'
        DataType = ftBoolean
      end
      item
        Name = 'style'
        DataType = ftInteger
      end
      item
        Name = 'method'
        ChildDefs = <
          item
            Name = 'methodName'
            DataType = ftString
            Size = 32
          end
          item
            Name = 'arrayArgs'
            DataType = ftBoolean
          end
          item
            Name = 'methodStyle'
            DataType = ftInteger
          end
          item
            Name = 'address'
            DataType = ftInteger
          end
          item
            Name = 'displayAddress'
            DataType = ftInteger
          end>
        DataType = ftADT
        Size = 5
      end
      item
        Name = 'skill'
        DataType = ftInteger
      end
      item
        Name = 'master'
        DataType = ftInteger
      end
      item
        Name = 'num'
        ChildDefs = <
          item
            Name = 'num[0]'
            DataType = ftInteger
          end>
        DataType = ftArray
        Size = 4
      end
      item
        Name = 'num[1]'
        DataType = ftInteger
      end
      item
        Name = 'num[2]'
        DataType = ftInteger
      end
      item
        Name = 'num[3]'
        DataType = ftInteger
      end>
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
    object charClasses_skillsetid: TStringField
      FieldKind = fkCalculated
      FieldName = 'id'
      Size = 10
      Calculated = True
    end
    object charClasses_skillsetname: TStringField
      FieldKind = fkLookup
      FieldName = 'name'
      LookupDataSet = skills
      LookupKeyFields = 'id'
      LookupResultField = 'name'
      KeyFields = 'skill'
      Size = 32
      Lookup = True
    end
    object charClasses_skillsetmodified: TBooleanField
      FieldName = 'modified'
    end
    object charClasses_skillsetstyle: TIntegerField
      FieldName = 'style'
    end
    object charClasses_skillsetnum: TArrayField
      FieldName = 'num'
      object charClasses_skillsetnum0: TIntegerField
        FieldName = 'num[0]'
      end
      object charClasses_skillsetnum1: TIntegerField
        FieldName = 'num[1]'
      end
      object charClasses_skillsetnum2: TIntegerField
        FieldName = 'num[2]'
      end
      object charClasses_skillsetnum3: TIntegerField
        FieldName = 'num[3]'
      end
    end
    object charClasses_skillsetmethod: TADTField
      FieldName = 'method'
      object charClasses_skillsetmethodmethod_name: TStringField
        FieldName = 'methodName'
        Size = 32
      end
      object charClasses_skillsetmethodarrayArgs: TBooleanField
        FieldName = 'arrayArgs'
      end
      object charClasses_skillsetmethodmethod_style: TIntegerField
        FieldName = 'methodStyle'
      end
      object charClasses_skillsetmethodmethod_address: TIntegerField
        FieldName = 'address'
      end
      object charClasses_skillsetmethod_displayAddress: TIntegerField
        FieldName = 'displayAddress'
      end
    end
    object charClasses_skillsetskill: TIntegerField
      FieldName = 'skill'
    end
    object charClasses_skillsetmaster: TIntegerField
      FieldName = 'master'
    end
    object charClasses_skillsetAlgName: TStringField
      FieldKind = fkLookup
      FieldName = 'algName'
      LookupDataSet = skillGainRecords
      LookupKeyFields = 'baseMethod'
      LookupResultField = 'designName'
      KeyFields = 'method.address'
      Size = 50
      Lookup = True
    end
  end
  object charClasses_Resist: TClientDataSet
    Aggregates = <>
    FieldDefs = <
      item
        Name = 'master'
        DataType = ftInteger
      end
      item
        Name = 'x'
        DataType = ftInteger
      end
      item
        Name = 'y'
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
    MasterSource = dsCharClasses
    PacketRecords = 0
    Params = <>
    StoreDefs = True
    Left = 8
    Top = 104
    object charClasses_Resistmaster: TIntegerField
      FieldName = 'master'
    end
    object charClasses_Resistx: TIntegerField
      FieldName = 'x'
    end
    object charClasses_Resisty: TIntegerField
      FieldName = 'y'
    end
    object charClasses_Resistname: TStringField
      FieldKind = fkLookup
      FieldName = 'name'
      LookupDataSet = attributes
      LookupKeyFields = 'id'
      LookupResultField = 'name'
      KeyFields = 'x'
      Size = 32
      Lookup = True
    end
  end
  object charClasses_Condition: TClientDataSet
    Active = True
    Aggregates = <>
    FieldDefs = <
      item
        Name = 'master'
        DataType = ftInteger
      end
      item
        Name = 'x'
        DataType = ftInteger
      end
      item
        Name = 'y'
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
    MasterSource = dsCharClasses
    PacketRecords = 0
    Params = <>
    StoreDefs = True
    Left = 8
    Top = 160
    Data = {
      530000009619E0BD0100000018000000030000000000030000005300066D6173
      7465720400010000000000017804000100000000000179040001000000000001
      000D44454641554C545F4F524445520200820000000000}
    object IntegerField1: TIntegerField
      FieldName = 'master'
    end
    object IntegerField2: TIntegerField
      FieldName = 'x'
    end
    object IntegerField3: TIntegerField
      FieldName = 'y'
    end
    object charClasses_Conditionname: TStringField
      FieldKind = fkLookup
      FieldName = 'name'
      LookupDataSet = conditions
      LookupKeyFields = 'id'
      LookupResultField = 'name'
      KeyFields = 'x'
      Size = 32
      Lookup = True
    end
  end
  object animations: TClientDataSet
    Active = True
    Aggregates = <>
    FieldDefs = <
      item
        Name = 'id'
        DataType = ftInteger
      end
      item
        Name = 'name'
        DataType = ftString
        Size = 32
      end
      item
        Name = 'modified'
        DataType = ftBoolean
      end
      item
        Name = 'hitsAll'
        DataType = ftBoolean
      end
      item
        Name = 'yTarget'
        DataType = ftInteger
      end>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 384
    Top = 8
    Data = {
      6F0000009619E0BD0100000018000000050000000000030000006F0002696404
      00010000000000046E616D650100490000000100055749445448020002002000
      086D6F64696669656402000300000000000768697473416C6C02000300000000
      00077954617267657404000100000000000000}
    object animationsid: TIntegerField
      FieldName = 'id'
    end
    object animationsname: TStringField
      FieldName = 'name'
      Size = 32
    end
    object animationsmodified: TBooleanField
      FieldName = 'modified'
    end
    object animationshitsAll: TBooleanField
      FieldName = 'hitsAll'
    end
    object animationsyTarget: TIntegerField
      FieldName = 'yTarget'
    end
  end
  object items_script: TClientDataSet
    Active = True
    Aggregates = <>
    FieldDefs = <
      item
        Name = 'id'
        DataType = ftInteger
      end
      item
        Name = 'name'
        DataType = ftString
        Size = 32
      end
      item
        Name = 'modified'
        DataType = ftBoolean
      end
      item
        Name = 'desc'
        DataType = ftString
        Size = 50
      end
      item
        Name = 'cost'
        DataType = ftInteger
      end
      item
        Name = 'tag'
        ChildDefs = <
          item
            Name = 'tag[0]'
            DataType = ftInteger
          end>
        DataType = ftArray
        Size = 4
      end
      item
        Name = 'tag[1]'
        DataType = ftInteger
      end
      item
        Name = 'tag[2]'
        DataType = ftInteger
      end
      item
        Name = 'tag[3]'
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
        DataType = ftBytes
        Size = 32
      end
      item
        Name = 'usableByClass'
        DataType = ftBytes
        Size = 32
      end
      item
        Name = 'stat'
        ChildDefs = <
          item
            Name = 'stat[0]'
            DataType = ftInteger
          end>
        DataType = ftArray
        Size = 6
      end
      item
        Name = 'stat[1]'
        DataType = ftInteger
      end
      item
        Name = 'stat[2]'
        DataType = ftInteger
      end
      item
        Name = 'stat[3]'
        DataType = ftInteger
      end
      item
        Name = 'stat[4]'
        DataType = ftInteger
      end
      item
        Name = 'stat[5]'
        DataType = ftInteger
      end
      item
        Name = 'event'
        DataType = ftString
        Size = 32
      end
      item
        Name = 'script'
        DataType = ftMemo
      end>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    AfterOpen = restoreClone
    Left = 184
    Top = 496
    Data = {
      B60100009619E0BD020000001800000017000000000003000000B60102696404
      00010000000000046E616D650100490000000100055749445448020002002000
      086D6F6469666965640200030000000000046465736301004900000001000557
      4944544802000200320004636F737404000100000000000374616704000D0300
      000000067461675B305D0400010000000000067461675B315D04000100000000
      00067461675B325D0400010000000000067461675B335D040001000000000008
      757365734C65667404000100000000000B757361626C65576865726504000100
      000000000C757361626C6542794865726F20000B00000000000D757361626C65
      4279436C61737320000B0000000000047374617406000D030000000007737461
      745B305D040001000000000007737461745B315D040001000000000007737461
      745B325D040001000000000007737461745B335D040001000000000007737461
      745B345D040001000000000007737461745B355D040001000000000005657665
      6E7401004900000001000557494454480200020020000673637269707404004B
      0000000100075355425459504502004900050054657874000000}
    object items_scriptid: TIntegerField
      FieldName = 'id'
    end
    object items_scriptname: TStringField
      FieldName = 'name'
      Size = 32
    end
    object items_scriptmodified: TBooleanField
      FieldName = 'modified'
    end
    object items_scriptdesc: TStringField
      FieldName = 'desc'
      Size = 50
    end
    object items_scriptcost: TIntegerField
      FieldName = 'cost'
    end
    object items_scripttag: TArrayField
      FieldName = 'tag'
      object items_scripttag0: TIntegerField
        FieldName = 'tag[0]'
      end
      object items_scripttag1: TIntegerField
        FieldName = 'tag[1]'
      end
      object items_scripttag2: TIntegerField
        FieldName = 'tag[2]'
      end
      object items_scripttag3: TIntegerField
        FieldName = 'tag[3]'
      end
    end
    object items_scriptusesLeft: TIntegerField
      FieldName = 'usesLeft'
    end
    object items_scriptusableWhere: TIntegerField
      FieldName = 'usableWhere'
    end
    object items_scriptusableByHero: TBytesField
      FieldName = 'usableByHero'
      Size = 32
    end
    object items_scriptusableByClass: TBytesField
      FieldName = 'usableByClass'
      Size = 32
    end
    object items_scriptstat: TArrayField
      FieldName = 'stat'
      object items_scriptstat0: TIntegerField
        FieldName = 'stat[0]'
      end
      object items_scriptstat1: TIntegerField
        FieldName = 'stat[1]'
      end
      object items_scriptstat2: TIntegerField
        FieldName = 'stat[2]'
      end
      object items_scriptstat3: TIntegerField
        FieldName = 'stat[3]'
      end
      object items_scriptstat4: TIntegerField
        FieldName = 'stat[4]'
      end
      object items_scriptstat5: TIntegerField
        FieldName = 'stat[5]'
      end
    end
    object items_scriptevent: TStringField
      FieldName = 'event'
      Size = 32
    end
    object items_scriptscript: TMemoField
      FieldName = 'script'
      BlobType = ftMemo
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
        DataType = ftString
        Size = 32
      end
      item
        Name = 'modified'
        DataType = ftBoolean
      end
      item
        Name = 'itemType'
        DataType = ftInteger
      end
      item
        Name = 'desc'
        DataType = ftString
        Size = 50
      end
      item
        Name = 'cost'
        DataType = ftInteger
      end
      item
        Name = 'tag'
        ChildDefs = <
          item
            Name = 'tag[0]'
            DataType = ftInteger
          end>
        DataType = ftArray
        Size = 10
      end
      item
        Name = 'tag[1]'
        DataType = ftInteger
      end
      item
        Name = 'tag[2]'
        DataType = ftInteger
      end
      item
        Name = 'tag[3]'
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
        DataType = ftBytes
        Size = 32
      end
      item
        Name = 'usableByClass'
        DataType = ftBytes
        Size = 32
      end
      item
        Name = 'stat'
        ChildDefs = <
          item
            Name = 'stat[0]'
            DataType = ftInteger
          end>
        DataType = ftArray
        Size = 10
      end
      item
        Name = 'stat[1]'
        DataType = ftInteger
      end
      item
        Name = 'stat[2]'
        DataType = ftInteger
      end
      item
        Name = 'stat[3]'
        DataType = ftInteger
      end
      item
        Name = 'stat[4]'
        DataType = ftInteger
      end
      item
        Name = 'stat[5]'
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
      3A0200009619E0BD0200000018000000200000000000030000003A0202696404
      00010000000000046E616D650100490000000100055749445448020002002000
      086D6F6469666965640200030000000000086974656D54797065040001000000
      00000464657363010049000000010005574944544802000200320004636F7374
      0400010000000000037461670A000D0300000000067461675B305D0400010000
      000000067461675B315D0400010000000000067461675B325D04000100000000
      00067461675B335D040001000000000008757365734C65667404000100000000
      000B757361626C65576865726504000100000000000C757361626C6542794865
      726F20000B00000000000D757361626C654279436C61737320000B0000000000
      04737461740A000D030000000007737461745B305D0400010000000000077374
      61745B315D040001000000000007737461745B325D0400010000000000077374
      61745B335D040001000000000007737461745B345D0400010000000000077374
      61745B355D04000100000000000765766173696F6E020003000000000005746F
      48697404000100000000000A637269744368616E636504000100000000000B63
      72697450726576656E7404000100000000000A707265656D7074697665040001
      00000000000B6D70526564756374696F6E04000100000000000F6E6F54657272
      61696E44616D616765020003000000000006757361626C650200030000000000
      06637572736564020003000000000004736C6F7404000100000000000000}
    object items_armorid: TIntegerField
      FieldName = 'id'
    end
    object items_armorname: TStringField
      FieldName = 'name'
      Size = 32
    end
    object items_armormodified: TBooleanField
      FieldName = 'modified'
    end
    object items_armoritemType: TIntegerField
      FieldName = 'itemType'
    end
    object items_armordesc: TStringField
      FieldName = 'desc'
      Size = 50
    end
    object items_armorcost: TIntegerField
      FieldName = 'cost'
    end
    object items_armortag: TArrayField
      FieldName = 'tag'
      object items_armortag0: TIntegerField
        FieldName = 'tag[0]'
      end
      object items_armortag1: TIntegerField
        FieldName = 'tag[1]'
      end
      object items_armortag2: TIntegerField
        FieldName = 'tag[2]'
      end
      object items_armortag3: TIntegerField
        FieldName = 'tag[3]'
      end
    end
    object items_armorusesLeft: TIntegerField
      FieldName = 'usesLeft'
    end
    object items_armorusableWhere: TIntegerField
      FieldName = 'usableWhere'
    end
    object items_armorusableByHero: TBytesField
      FieldName = 'usableByHero'
      Size = 32
    end
    object items_armorusableByClass: TBytesField
      FieldName = 'usableByClass'
      Size = 32
    end
    object items_armorstat: TArrayField
      FieldName = 'stat'
      object items_armorstat0: TIntegerField
        FieldName = 'stat[0]'
      end
      object items_armorstat1: TIntegerField
        FieldName = 'stat[1]'
      end
      object items_armorstat2: TIntegerField
        FieldName = 'stat[2]'
      end
      object items_armorstat3: TIntegerField
        FieldName = 'stat[3]'
      end
      object items_armorstat4: TIntegerField
        FieldName = 'stat[4]'
      end
      object items_armorstat5: TIntegerField
        FieldName = 'stat[5]'
      end
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
    object items_armorslot: TIntegerField
      FieldName = 'slot'
    end
  end
  object equipment_conditions: TClientDataSet
    Active = True
    Aggregates = <>
    FieldDefs = <
      item
        Name = 'master'
        DataType = ftInteger
      end
      item
        Name = 'id'
        DataType = ftInteger
      end
      item
        Name = 'value'
        DataType = ftBoolean
      end>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 288
    Top = 480
    Data = {
      420000009619E0BD0100000018000000030000000000030000004200066D6173
      746572040001000000000002696404000100000000000576616C756502000300
      000000000000}
    object IntegerField10: TIntegerField
      FieldName = 'master'
    end
    object equipment_conditionsid: TIntegerField
      FieldName = 'id'
    end
    object equipment_conditionsvalue: TBooleanField
      FieldName = 'value'
    end
  end
  object equipment_attributes: TClientDataSet
    Active = True
    Aggregates = <>
    FieldDefs = <
      item
        Name = 'master'
        DataType = ftInteger
      end
      item
        Name = 'x'
        DataType = ftInteger
      end
      item
        Name = 'y'
        DataType = ftInteger
      end>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 368
    Top = 496
    Data = {
      3D0000009619E0BD0100000018000000030000000000030000003D00066D6173
      7465720400010000000000017804000100000000000179040001000000000000
      00}
    object IntegerField13: TIntegerField
      FieldName = 'master'
    end
    object IntegerField14: TIntegerField
      FieldName = 'x'
    end
    object IntegerField15: TIntegerField
      FieldName = 'y'
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
        DataType = ftString
        Size = 32
      end
      item
        Name = 'modified'
        DataType = ftBoolean
      end
      item
        Name = 'itemType'
        DataType = ftInteger
      end
      item
        Name = 'desc'
        DataType = ftString
        Size = 50
      end
      item
        Name = 'cost'
        DataType = ftInteger
      end
      item
        Name = 'tag'
        ChildDefs = <
          item
            Name = 'tag[0]'
            DataType = ftInteger
          end>
        DataType = ftArray
        Size = 10
      end
      item
        Name = 'tag[1]'
        DataType = ftInteger
      end
      item
        Name = 'tag[2]'
        DataType = ftInteger
      end
      item
        Name = 'tag[3]'
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
        DataType = ftBytes
        Size = 32
      end
      item
        Name = 'usableByClass'
        DataType = ftBytes
        Size = 32
      end
      item
        Name = 'stat'
        ChildDefs = <
          item
            Name = 'stat[0]'
            DataType = ftInteger
          end>
        DataType = ftArray
        Size = 10
      end
      item
        Name = 'stat[1]'
        DataType = ftInteger
      end
      item
        Name = 'stat[2]'
        DataType = ftInteger
      end
      item
        Name = 'stat[3]'
        DataType = ftInteger
      end
      item
        Name = 'stat[4]'
        DataType = ftInteger
      end
      item
        Name = 'stat[5]'
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
      9D0200009619E0BD0200000018000000250000000000030000009D0202696404
      00010000000000046E616D650100490000000100055749445448020002002000
      086D6F6469666965640200030000000000086974656D54797065040001000000
      00000464657363010049000000010005574944544802000200320004636F7374
      0400010000000000037461670A000D0300000000067461675B305D0400010000
      000000067461675B315D0400010000000000067461675B325D04000100000000
      00067461675B335D040001000000000008757365734C65667404000100000000
      000B757361626C65576865726504000100000000000C757361626C6542794865
      726F20000B00000000000D757361626C654279436C61737320000B0000000000
      04737461740A000D030000000007737461745B305D0400010000000000077374
      61745B315D040001000000000007737461745B325D0400010000000000077374
      61745B335D040001000000000007737461745B345D0400010000000000077374
      61745B355D04000100000000000765766173696F6E020003000000000005746F
      48697404000100000000000A637269744368616E636504000100000000000B63
      72697450726576656E7404000100000000000A707265656D7074697665040001
      00000000000B6D70526564756374696F6E04000100000000000F6E6F54657272
      61696E44616D616765020003000000000006757361626C650200030000000000
      0663757273656402000300000000000974776F48616E64656402000300000000
      000B61747461636B547769636502000300000000000761726561486974020003
      00000000000A626174746C65416E696D0400010000000000066D70436F737404
      000100000000000F636F6E646974696F6E4368616E6365040001000000000000
      00}
    object items_weaponid: TIntegerField
      FieldName = 'id'
    end
    object items_weaponname: TStringField
      FieldName = 'name'
      Size = 32
    end
    object items_weaponmodified: TBooleanField
      FieldName = 'modified'
    end
    object items_weaponIntegerField: TIntegerField
      FieldName = 'itemType'
    end
    object items_weapondesc: TStringField
      FieldName = 'desc'
      Size = 50
    end
    object items_weaponcost: TIntegerField
      FieldName = 'cost'
    end
    object items_weapontag: TArrayField
      FieldName = 'tag'
      object items_weapontag0: TIntegerField
        FieldName = 'tag[0]'
      end
      object items_weapontag1: TIntegerField
        FieldName = 'tag[1]'
      end
      object items_weapontag2: TIntegerField
        FieldName = 'tag[2]'
      end
      object items_weapontag3: TIntegerField
        FieldName = 'tag[3]'
      end
    end
    object items_weaponusesLeft: TIntegerField
      FieldName = 'usesLeft'
    end
    object items_weaponusableWhere: TIntegerField
      FieldName = 'usableWhere'
    end
    object items_weaponusableByChar: TBytesField
      FieldName = 'usableByHero'
      Size = 32
    end
    object items_weaponusableByClass: TBytesField
      FieldName = 'usableByClass'
      Size = 32
    end
    object items_weaponstat: TArrayField
      FieldName = 'stat'
      object items_weaponstat0: TIntegerField
        FieldName = 'stat[0]'
      end
      object items_weaponstat1: TIntegerField
        FieldName = 'stat[1]'
      end
      object items_weaponstat2: TIntegerField
        FieldName = 'stat[2]'
      end
      object items_weaponstat3: TIntegerField
        FieldName = 'stat[3]'
      end
      object items_weaponstat4: TIntegerField
        FieldName = 'stat[4]'
      end
      object items_weaponstat5: TIntegerField
        FieldName = 'stat[5]'
      end
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
    object items_weaponconditionChance: TIntegerField
      FieldName = 'conditionChance'
    end
  end
  object items_medicine: TClientDataSet
    Active = True
    Aggregates = <>
    FieldDefs = <
      item
        Name = 'id'
        DataType = ftInteger
      end
      item
        Name = 'name'
        DataType = ftString
        Size = 32
      end
      item
        Name = 'modified'
        DataType = ftBoolean
      end
      item
        Name = 'desc'
        DataType = ftString
        Size = 50
      end
      item
        Name = 'cost'
        DataType = ftInteger
      end
      item
        Name = 'tag'
        ChildDefs = <
          item
            Name = 'tag[0]'
            DataType = ftInteger
          end>
        DataType = ftArray
        Size = 4
      end
      item
        Name = 'tag[1]'
        DataType = ftInteger
      end
      item
        Name = 'tag[2]'
        DataType = ftInteger
      end
      item
        Name = 'tag[3]'
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
        Name = 'stat'
        ChildDefs = <
          item
            Name = 'stat[0]'
            DataType = ftInteger
          end>
        DataType = ftArray
        Size = 6
      end
      item
        Name = 'stat[1]'
        DataType = ftInteger
      end
      item
        Name = 'stat[2]'
        DataType = ftInteger
      end
      item
        Name = 'stat[3]'
        DataType = ftInteger
      end
      item
        Name = 'stat[4]'
        DataType = ftInteger
      end
      item
        Name = 'stat[5]'
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
        Name = 'deadOnly'
        DataType = ftBoolean
      end
      item
        Name = 'skill'
        DataType = ftInteger
      end
      item
        Name = 'usableByClass'
        DataType = ftBytes
        Size = 32
      end
      item
        Name = 'usableByHero'
        DataType = ftBytes
        Size = 32
      end>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    AfterOpen = restoreClone
    Left = 184
    Top = 224
    Data = {
      D20100009619E0BD02000000180000001A000000000003000000D20102696404
      00010000000000046E616D650100490000000100055749445448020002002000
      086D6F6469666965640200030000000000046465736301004900000001000557
      4944544802000200320004636F737404000100000000000374616704000D0300
      000000067461675B305D0400010000000000067461675B315D04000100000000
      00067461675B325D0400010000000000067461675B335D040001000000000008
      757365734C65667404000100000000000B757361626C65576865726504000100
      00000000047374617406000D030000000007737461745B305D04000100000000
      0007737461745B315D040001000000000007737461745B325D04000100000000
      0007737461745B335D040001000000000007737461745B345D04000100000000
      0007737461745B355D04000100000000000C617265614D65646963696E650200
      03000000000009687050657263656E740400010000000000096D705065726365
      6E74040001000000000008646561644F6E6C79020003000000000005736B696C
      6C04000100000000000D757361626C654279436C61737320000B00000000000C
      757361626C6542794865726F20000B00000000000000}
    object items_medicineid: TIntegerField
      FieldName = 'id'
    end
    object items_medicinename: TStringField
      FieldName = 'name'
      Size = 32
    end
    object items_medicinemodified: TBooleanField
      FieldName = 'modified'
    end
    object items_medicinedesc: TStringField
      FieldName = 'desc'
      Size = 50
    end
    object items_medicinecost: TIntegerField
      FieldName = 'cost'
    end
    object items_medicinetag: TArrayField
      FieldName = 'tag'
      object items_medicinetag0: TIntegerField
        FieldName = 'tag[0]'
      end
      object items_medicinetag1: TIntegerField
        FieldName = 'tag[1]'
      end
      object items_medicinetag2: TIntegerField
        FieldName = 'tag[2]'
      end
      object items_medicinetag3: TIntegerField
        FieldName = 'tag[3]'
      end
    end
    object items_medicineusesLeft: TIntegerField
      FieldName = 'usesLeft'
    end
    object items_medicineusableWhere: TIntegerField
      FieldName = 'usableWhere'
    end
    object items_medicineusableByHero: TBytesField
      FieldName = 'usableByHero'
      Size = 32
    end
    object items_medicineusableByClass: TBytesField
      FieldName = 'usableByClass'
      Size = 32
    end
    object items_medicinestat: TArrayField
      FieldName = 'stat'
      object items_medicinestat0: TIntegerField
        FieldName = 'stat[0]'
      end
      object items_medicinestat1: TIntegerField
        FieldName = 'stat[1]'
      end
      object items_medicinestat2: TIntegerField
        FieldName = 'stat[2]'
      end
      object items_medicinestat3: TIntegerField
        FieldName = 'stat[3]'
      end
      object items_medicinestat4: TIntegerField
        FieldName = 'stat[4]'
      end
      object items_medicinestat5: TIntegerField
        FieldName = 'stat[5]'
      end
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
      FieldName = 'deadOnly'
    end
  end
  object items_book: TClientDataSet
    Active = True
    Aggregates = <>
    FieldDefs = <
      item
        Name = 'id'
        DataType = ftInteger
      end
      item
        Name = 'name'
        DataType = ftString
        Size = 32
      end
      item
        Name = 'modified'
        DataType = ftBoolean
      end
      item
        Name = 'desc'
        DataType = ftString
        Size = 50
      end
      item
        Name = 'cost'
        DataType = ftInteger
      end
      item
        Name = 'tag'
        ChildDefs = <
          item
            Name = 'tag[0]'
            DataType = ftInteger
          end>
        DataType = ftArray
        Size = 4
      end
      item
        Name = 'tag[1]'
        DataType = ftInteger
      end
      item
        Name = 'tag[2]'
        DataType = ftInteger
      end
      item
        Name = 'tag[3]'
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
        Name = 'stat'
        ChildDefs = <
          item
            Name = 'stat[0]'
            DataType = ftInteger
          end>
        DataType = ftArray
        Size = 6
      end
      item
        Name = 'stat[1]'
        DataType = ftInteger
      end
      item
        Name = 'stat[2]'
        DataType = ftInteger
      end
      item
        Name = 'stat[3]'
        DataType = ftInteger
      end
      item
        Name = 'stat[4]'
        DataType = ftInteger
      end
      item
        Name = 'stat[5]'
        DataType = ftInteger
      end
      item
        Name = 'skill'
        DataType = ftInteger
      end
      item
        Name = 'usableByClass'
        DataType = ftBytes
        Size = 32
      end
      item
        Name = 'usableByHero'
        DataType = ftBytes
        Size = 32
      end>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    AfterOpen = restoreClone
    Left = 184
    Top = 280
    Data = {
      880100009619E0BD020000001800000016000000000003000000880102696404
      00010000000000046E616D650100490000000100055749445448020002002000
      086D6F6469666965640200030000000000046465736301004900000001000557
      4944544802000200320004636F737404000100000000000374616704000D0300
      000000067461675B305D0400010000000000067461675B315D04000100000000
      00067461675B325D0400010000000000067461675B335D040001000000000008
      757365734C65667404000100000000000B757361626C65576865726504000100
      00000000047374617406000D030000000007737461745B305D04000100000000
      0007737461745B315D040001000000000007737461745B325D04000100000000
      0007737461745B335D040001000000000007737461745B345D04000100000000
      0007737461745B355D040001000000000005736B696C6C04000100000000000D
      757361626C654279436C61737320000B00000000000C757361626C6542794865
      726F20000B00000000000000}
    object items_bookid: TIntegerField
      FieldName = 'id'
    end
    object items_bookname: TStringField
      FieldName = 'name'
      Size = 32
    end
    object items_bookmodified: TBooleanField
      FieldName = 'modified'
    end
    object items_bookdesc: TStringField
      FieldName = 'desc'
      Size = 50
    end
    object items_bookcost: TIntegerField
      FieldName = 'cost'
    end
    object items_booktag: TArrayField
      FieldName = 'tag'
      object items_booktag0: TIntegerField
        FieldName = 'tag[0]'
      end
      object items_booktag1: TIntegerField
        FieldName = 'tag[1]'
      end
      object items_booktag2: TIntegerField
        FieldName = 'tag[2]'
      end
      object items_booktag3: TIntegerField
        FieldName = 'tag[3]'
      end
    end
    object items_bookusesLeft: TIntegerField
      FieldName = 'usesLeft'
    end
    object items_bookusableWhere: TIntegerField
      FieldName = 'usableWhere'
    end
    object items_bookusableByHero: TBytesField
      FieldName = 'usableByHero'
      Size = 32
    end
    object items_bookusableByClass: TBytesField
      FieldName = 'usableByClass'
      Size = 32
    end
    object items_bookstat: TArrayField
      FieldName = 'stat'
      object items_bookstat0: TIntegerField
        FieldName = 'stat[0]'
      end
      object items_bookstat1: TIntegerField
        FieldName = 'stat[1]'
      end
      object items_bookstat2: TIntegerField
        FieldName = 'stat[2]'
      end
      object items_bookstat3: TIntegerField
        FieldName = 'stat[3]'
      end
      object items_bookstat4: TIntegerField
        FieldName = 'stat[4]'
      end
      object items_bookstat5: TIntegerField
        FieldName = 'stat[5]'
      end
    end
    object items_bookskill: TIntegerField
      FieldName = 'skill'
    end
  end
  object items_skill: TClientDataSet
    Active = True
    Aggregates = <>
    FieldDefs = <
      item
        Name = 'id'
        DataType = ftInteger
      end
      item
        Name = 'name'
        DataType = ftString
        Size = 32
      end
      item
        Name = 'modified'
        DataType = ftBoolean
      end
      item
        Name = 'desc'
        DataType = ftString
        Size = 50
      end
      item
        Name = 'cost'
        DataType = ftInteger
      end
      item
        Name = 'tag'
        ChildDefs = <
          item
            Name = 'tag[0]'
            DataType = ftInteger
          end>
        DataType = ftArray
        Size = 4
      end
      item
        Name = 'tag[1]'
        DataType = ftInteger
      end
      item
        Name = 'tag[2]'
        DataType = ftInteger
      end
      item
        Name = 'tag[3]'
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
        Name = 'stat'
        ChildDefs = <
          item
            Name = 'stat[0]'
            DataType = ftInteger
          end>
        DataType = ftArray
        Size = 6
      end
      item
        Name = 'stat[1]'
        DataType = ftInteger
      end
      item
        Name = 'stat[2]'
        DataType = ftInteger
      end
      item
        Name = 'stat[3]'
        DataType = ftInteger
      end
      item
        Name = 'stat[4]'
        DataType = ftInteger
      end
      item
        Name = 'stat[5]'
        DataType = ftInteger
      end
      item
        Name = 'skill'
        DataType = ftInteger
      end
      item
        Name = 'customSkillMessage'
        DataType = ftBoolean
      end
      item
        Name = 'usableByHero'
        DataType = ftBytes
        Size = 32
      end
      item
        Name = 'usableByClass'
        DataType = ftBytes
        Size = 32
      end>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    AfterOpen = restoreClone
    Left = 184
    Top = 336
    Data = {
      A30100009619E0BD020000001800000017000000000003000000A30102696404
      00010000000000046E616D650100490000000100055749445448020002002000
      086D6F6469666965640200030000000000046465736301004900000001000557
      4944544802000200320004636F737404000100000000000374616704000D0300
      000000067461675B305D0400010000000000067461675B315D04000100000000
      00067461675B325D0400010000000000067461675B335D040001000000000008
      757365734C65667404000100000000000B757361626C65576865726504000100
      00000000047374617406000D030000000007737461745B305D04000100000000
      0007737461745B315D040001000000000007737461745B325D04000100000000
      0007737461745B335D040001000000000007737461745B345D04000100000000
      0007737461745B355D040001000000000005736B696C6C040001000000000012
      637573746F6D536B696C6C4D65737361676502000300000000000C757361626C
      6542794865726F20000B00000000000D757361626C654279436C61737320000B
      00000000000000}
    object items_skillid: TIntegerField
      FieldName = 'id'
    end
    object items_skillname: TStringField
      FieldName = 'name'
      Size = 32
    end
    object items_skillmodified: TBooleanField
      FieldName = 'modified'
    end
    object items_skilldesc: TStringField
      FieldName = 'desc'
      Size = 50
    end
    object items_skillcost: TIntegerField
      FieldName = 'cost'
    end
    object items_skilltag: TArrayField
      FieldName = 'tag'
      object items_skilltag0: TIntegerField
        FieldName = 'tag[0]'
      end
      object items_skilltag1: TIntegerField
        FieldName = 'tag[1]'
      end
      object items_skilltag2: TIntegerField
        FieldName = 'tag[2]'
      end
      object items_skilltag3: TIntegerField
        FieldName = 'tag[3]'
      end
    end
    object items_skillusesLeft: TIntegerField
      FieldName = 'usesLeft'
    end
    object items_skillusableWhere: TIntegerField
      FieldName = 'usableWhere'
    end
    object items_skillusableByHero: TBytesField
      FieldName = 'usableByHero'
      Size = 32
    end
    object items_skillusableByClass: TBytesField
      FieldName = 'usableByClass'
      Size = 32
    end
    object items_skillstat: TArrayField
      FieldName = 'stat'
      object items_skillstat0: TIntegerField
        FieldName = 'stat[0]'
      end
      object items_skillstat1: TIntegerField
        FieldName = 'stat[1]'
      end
      object items_skillstat2: TIntegerField
        FieldName = 'stat[2]'
      end
      object items_skillstat3: TIntegerField
        FieldName = 'stat[3]'
      end
      object items_skillstat4: TIntegerField
        FieldName = 'stat[4]'
      end
      object items_skillstat5: TIntegerField
        FieldName = 'stat[5]'
      end
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
    FieldDefs = <
      item
        Name = 'id'
        DataType = ftInteger
      end
      item
        Name = 'name'
        DataType = ftString
        Size = 32
      end
      item
        Name = 'modified'
        DataType = ftBoolean
      end
      item
        Name = 'desc'
        DataType = ftString
        Size = 50
      end
      item
        Name = 'cost'
        DataType = ftInteger
      end
      item
        Name = 'tag'
        ChildDefs = <
          item
            Name = 'tag[0]'
            DataType = ftInteger
          end>
        DataType = ftArray
        Size = 4
      end
      item
        Name = 'tag[1]'
        DataType = ftInteger
      end
      item
        Name = 'tag[2]'
        DataType = ftInteger
      end
      item
        Name = 'tag[3]'
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
        Name = 'stat'
        ChildDefs = <
          item
            Name = 'stat[0]'
            DataType = ftInteger
          end>
        DataType = ftArray
        Size = 6
      end
      item
        Name = 'stat[1]'
        DataType = ftInteger
      end
      item
        Name = 'stat[2]'
        DataType = ftInteger
      end
      item
        Name = 'stat[3]'
        DataType = ftInteger
      end
      item
        Name = 'stat[4]'
        DataType = ftInteger
      end
      item
        Name = 'stat[5]'
        DataType = ftInteger
      end
      item
        Name = 'usableByClass'
        DataType = ftBytes
        Size = 32
      end
      item
        Name = 'usableByHero'
        DataType = ftBytes
        Size = 32
      end>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    AfterOpen = restoreClone
    Left = 184
    Top = 388
    Data = {
      7A0100009619E0BD0200000018000000150000000000030000007A0102696404
      00010000000000046E616D650100490000000100055749445448020002002000
      086D6F6469666965640200030000000000046465736301004900000001000557
      4944544802000200320004636F737404000100000000000374616704000D0300
      000000067461675B305D0400010000000000067461675B315D04000100000000
      00067461675B325D0400010000000000067461675B335D040001000000000008
      757365734C65667404000100000000000B757361626C65576865726504000100
      00000000047374617406000D030000000007737461745B305D04000100000000
      0007737461745B315D040001000000000007737461745B325D04000100000000
      0007737461745B335D040001000000000007737461745B345D04000100000000
      0007737461745B355D04000100000000000D757361626C654279436C61737320
      000B00000000000C757361626C6542794865726F20000B00000000000000}
    object items_upgradeid: TIntegerField
      FieldName = 'id'
    end
    object items_upgradename: TStringField
      FieldName = 'name'
      Size = 32
    end
    object items_upgrademodified: TBooleanField
      FieldName = 'modified'
    end
    object items_upgradedesc: TStringField
      FieldName = 'desc'
      Size = 50
    end
    object items_upgradecost: TIntegerField
      FieldName = 'cost'
    end
    object items_upgradetag: TArrayField
      FieldName = 'tag'
      object items_upgradetag0: TIntegerField
        FieldName = 'tag[0]'
      end
      object items_upgradetag1: TIntegerField
        FieldName = 'tag[1]'
      end
      object items_upgradetag2: TIntegerField
        FieldName = 'tag[2]'
      end
      object items_upgradetag3: TIntegerField
        FieldName = 'tag[3]'
      end
    end
    object items_upgradeusesLeft: TIntegerField
      FieldName = 'usesLeft'
    end
    object items_upgradeusableWhere: TIntegerField
      FieldName = 'usableWhere'
    end
    object items_upgradeusableByHero: TBytesField
      FieldName = 'usableByHero'
      Size = 32
    end
    object items_upgradeusableByClass: TBytesField
      FieldName = 'usableByClass'
      Size = 32
    end
    object items_upgradestat: TArrayField
      FieldName = 'stat'
      object items_upgradestat0: TIntegerField
        FieldName = 'stat[0]'
      end
      object items_upgradestat1: TIntegerField
        FieldName = 'stat[1]'
      end
      object items_upgradestat2: TIntegerField
        FieldName = 'stat[2]'
      end
      object items_upgradestat3: TIntegerField
        FieldName = 'stat[3]'
      end
      object items_upgradestat4: TIntegerField
        FieldName = 'stat[4]'
      end
      object items_upgradestat5: TIntegerField
        FieldName = 'stat[5]'
      end
    end
  end
  object items_variable: TClientDataSet
    Active = True
    Aggregates = <>
    FieldDefs = <
      item
        Name = 'id'
        DataType = ftInteger
      end
      item
        Name = 'name'
        DataType = ftString
        Size = 32
      end
      item
        Name = 'modified'
        DataType = ftBoolean
      end
      item
        Name = 'desc'
        DataType = ftString
        Size = 50
      end
      item
        Name = 'cost'
        DataType = ftInteger
      end
      item
        Name = 'tag'
        ChildDefs = <
          item
            Name = 'tag[0]'
            DataType = ftInteger
          end>
        DataType = ftArray
        Size = 4
      end
      item
        Name = 'tag[1]'
        DataType = ftInteger
      end
      item
        Name = 'tag[2]'
        DataType = ftInteger
      end
      item
        Name = 'tag[3]'
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
        Name = 'stat'
        ChildDefs = <
          item
            Name = 'stat[0]'
            DataType = ftInteger
          end>
        DataType = ftArray
        Size = 6
      end
      item
        Name = 'stat[1]'
        DataType = ftInteger
      end
      item
        Name = 'stat[2]'
        DataType = ftInteger
      end
      item
        Name = 'stat[3]'
        DataType = ftInteger
      end
      item
        Name = 'stat[4]'
        DataType = ftInteger
      end
      item
        Name = 'stat[5]'
        DataType = ftInteger
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
        Name = 'stype'
        DataType = ftInteger
      end
      item
        Name = 'operation'
        DataType = ftInteger
      end
      item
        Name = 'usableByClass'
        DataType = ftBytes
        Size = 32
      end
      item
        Name = 'usableByHero'
        DataType = ftBytes
        Size = 32
      end>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    AfterOpen = restoreClone
    Left = 184
    Top = 440
    Data = {
      BA0100009619E0BD020000001800000019000000000003000000BA0102696404
      00010000000000046E616D650100490000000100055749445448020002002000
      086D6F6469666965640200030000000000046465736301004900000001000557
      4944544802000200320004636F737404000100000000000374616704000D0300
      000000067461675B305D0400010000000000067461675B315D04000100000000
      00067461675B325D0400010000000000067461675B335D040001000000000008
      757365734C65667404000100000000000B757361626C65576865726504000100
      00000000047374617406000D030000000007737461745B305D04000100000000
      0007737461745B315D040001000000000007737461745B325D04000100000000
      0007737461745B335D040001000000000007737461745B345D04000100000000
      0007737461745B355D0400010000000000057768696368040001000000000009
      6D61676E69747564650400010000000000057374797065040001000000000009
      6F7065726174696F6E04000100000000000D757361626C654279436C61737320
      000B00000000000C757361626C6542794865726F20000B00000000000000}
    object items_variableid: TIntegerField
      FieldName = 'id'
    end
    object items_variablename: TStringField
      FieldName = 'name'
      Size = 32
    end
    object items_variablemodified: TBooleanField
      FieldName = 'modified'
    end
    object items_variabledesc: TStringField
      FieldName = 'desc'
      Size = 50
    end
    object items_variablecost: TIntegerField
      FieldName = 'cost'
    end
    object items_variabletag: TArrayField
      FieldName = 'tag'
      object items_variabletag0: TIntegerField
        FieldName = 'tag[0]'
      end
      object items_variabletag1: TIntegerField
        FieldName = 'tag[1]'
      end
      object items_variabletag2: TIntegerField
        FieldName = 'tag[2]'
      end
      object items_variabletag3: TIntegerField
        FieldName = 'tag[3]'
      end
    end
    object items_variableusesLeft: TIntegerField
      FieldName = 'usesLeft'
    end
    object items_variableusableWhere: TIntegerField
      FieldName = 'usableWhere'
    end
    object items_variableusableByHero: TBytesField
      FieldName = 'usableByHero'
      Size = 32
    end
    object items_variableusableByClass: TBytesField
      FieldName = 'usableByClass'
      Size = 32
    end
    object items_variablestat: TArrayField
      FieldName = 'stat'
      object items_variablestat0: TIntegerField
        FieldName = 'stat[0]'
      end
      object items_variablestat1: TIntegerField
        FieldName = 'stat[1]'
      end
      object items_variablestat2: TIntegerField
        FieldName = 'stat[2]'
      end
      object items_variablestat3: TIntegerField
        FieldName = 'stat[3]'
      end
      object items_variablestat4: TIntegerField
        FieldName = 'stat[4]'
      end
      object items_variablestat5: TIntegerField
        FieldName = 'stat[5]'
      end
    end
    object items_variablewhich: TIntegerField
      FieldName = 'which'
    end
    object items_variablemagnitude: TIntegerField
      FieldName = 'magnitude'
    end
    object items_variablestype: TIntegerField
      FieldName = 'stype'
    end
    object items_variableoperation: TIntegerField
      FieldName = 'operation'
    end
  end
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
        DataType = ftString
        Size = 32
      end
      item
        Name = 'modified'
        DataType = ftBoolean
      end
      item
        Name = 'itemType'
        DataType = ftInteger
      end
      item
        Name = 'desc'
        DataType = ftString
        Size = 50
      end
      item
        Name = 'cost'
        DataType = ftInteger
      end
      item
        Name = 'tag'
        ChildDefs = <
          item
            Name = 'tag[0]'
            DataType = ftInteger
          end>
        DataType = ftArray
        Size = 10
      end
      item
        Name = 'tag[1]'
        DataType = ftInteger
      end
      item
        Name = 'tag[2]'
        DataType = ftInteger
      end
      item
        Name = 'tag[3]'
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
        DataType = ftBytes
        Size = 32
      end
      item
        Name = 'usableByClass'
        DataType = ftBytes
        Size = 32
      end
      item
        Name = 'stat'
        ChildDefs = <
          item
            Name = 'stat[0]'
            DataType = ftInteger
          end>
        DataType = ftArray
        Size = 10
      end
      item
        Name = 'stat[1]'
        DataType = ftInteger
      end
      item
        Name = 'stat[2]'
        DataType = ftInteger
      end
      item
        Name = 'stat[3]'
        DataType = ftInteger
      end
      item
        Name = 'stat[4]'
        DataType = ftInteger
      end
      item
        Name = 'stat[5]'
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
        Name = 'slot'
        DataType = ftInteger
      end>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    AfterOpen = restoreClone
    OnFilterRecord = classFilter
    Left = 8
    Top = 280
    Data = {
      3A0200009619E0BD0200000018000000200000000000030000003A0202696404
      00010000000000046E616D650100490000000100055749445448020002002000
      086D6F6469666965640200030000000000086974656D54797065040001000000
      00000464657363010049000000010005574944544802000200320004636F7374
      0400010000000000037461670A000D0300000000067461675B305D0400010000
      000000067461675B315D0400010000000000067461675B325D04000100000000
      00067461675B335D040001000000000008757365734C65667404000100000000
      000B757361626C65576865726504000100000000000C757361626C6542794865
      726F20000B00000000000D757361626C654279436C61737320000B0000000000
      04737461740A000D030000000007737461745B305D0400010000000000077374
      61745B315D040001000000000007737461745B325D0400010000000000077374
      61745B335D040001000000000007737461745B345D0400010000000000077374
      61745B355D04000100000000000765766173696F6E020003000000000005746F
      48697404000100000000000A637269744368616E636504000100000000000B63
      72697450726576656E7404000100000000000A707265656D7074697665040001
      00000000000B6D70526564756374696F6E04000100000000000F6E6F54657272
      61696E44616D616765020003000000000006757361626C650200030000000000
      06637572736564020003000000000004736C6F7404000100000000000000}
    object shieldsid: TIntegerField
      FieldName = 'id'
    end
    object shieldsname: TStringField
      FieldName = 'name'
      Size = 32
    end
    object shieldsmodified: TBooleanField
      FieldName = 'modified'
    end
    object shieldsitemType: TIntegerField
      FieldName = 'itemType'
    end
    object shieldsusableByHero: TBytesField
      FieldName = 'usableByHero'
      Size = 32
    end
    object shieldsusableByClass: TBytesField
      FieldName = 'usableByClass'
      Size = 32
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
        DataType = ftString
        Size = 32
      end
      item
        Name = 'modified'
        DataType = ftBoolean
      end
      item
        Name = 'itemType'
        DataType = ftInteger
      end
      item
        Name = 'desc'
        DataType = ftString
        Size = 50
      end
      item
        Name = 'cost'
        DataType = ftInteger
      end
      item
        Name = 'tag'
        ChildDefs = <
          item
            Name = 'tag[0]'
            DataType = ftInteger
          end>
        DataType = ftArray
        Size = 10
      end
      item
        Name = 'tag[1]'
        DataType = ftInteger
      end
      item
        Name = 'tag[2]'
        DataType = ftInteger
      end
      item
        Name = 'tag[3]'
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
        DataType = ftBytes
        Size = 32
      end
      item
        Name = 'usableByClass'
        DataType = ftBytes
        Size = 32
      end
      item
        Name = 'stat'
        ChildDefs = <
          item
            Name = 'stat[0]'
            DataType = ftInteger
          end>
        DataType = ftArray
        Size = 10
      end
      item
        Name = 'stat[1]'
        DataType = ftInteger
      end
      item
        Name = 'stat[2]'
        DataType = ftInteger
      end
      item
        Name = 'stat[3]'
        DataType = ftInteger
      end
      item
        Name = 'stat[4]'
        DataType = ftInteger
      end
      item
        Name = 'stat[5]'
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
        Name = 'slot'
        DataType = ftInteger
      end>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    AfterOpen = restoreClone
    OnFilterRecord = classFilter
    Left = 8
    Top = 384
    Data = {
      3A0200009619E0BD0200000018000000200000000000030000003A0202696404
      00010000000000046E616D650100490000000100055749445448020002002000
      086D6F6469666965640200030000000000086974656D54797065040001000000
      00000464657363010049000000010005574944544802000200320004636F7374
      0400010000000000037461670A000D0300000000067461675B305D0400010000
      000000067461675B315D0400010000000000067461675B325D04000100000000
      00067461675B335D040001000000000008757365734C65667404000100000000
      000B757361626C65576865726504000100000000000C757361626C6542794865
      726F20000B00000000000D757361626C654279436C61737320000B0000000000
      04737461740A000D030000000007737461745B305D0400010000000000077374
      61745B315D040001000000000007737461745B325D0400010000000000077374
      61745B335D040001000000000007737461745B345D0400010000000000077374
      61745B355D04000100000000000765766173696F6E020003000000000005746F
      48697404000100000000000A637269744368616E636504000100000000000B63
      72697450726576656E7404000100000000000A707265656D7074697665040001
      00000000000B6D70526564756374696F6E04000100000000000F6E6F54657272
      61696E44616D616765020003000000000006757361626C650200030000000000
      06637572736564020003000000000004736C6F7404000100000000000000}
    object armorsid: TIntegerField
      FieldName = 'id'
    end
    object armorsname: TStringField
      FieldName = 'name'
      Size = 32
    end
    object armorsmodified: TBooleanField
      FieldName = 'modified'
    end
    object armorsitemType: TIntegerField
      FieldName = 'itemType'
    end
    object armorsusableByHero: TBytesField
      FieldName = 'usableByHero'
      Size = 32
    end
    object armorsusableByClass: TBytesField
      FieldName = 'usableByClass'
      Size = 32
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
        DataType = ftString
        Size = 32
      end
      item
        Name = 'modified'
        DataType = ftBoolean
      end
      item
        Name = 'itemType'
        DataType = ftInteger
      end
      item
        Name = 'desc'
        DataType = ftString
        Size = 50
      end
      item
        Name = 'cost'
        DataType = ftInteger
      end
      item
        Name = 'tag'
        ChildDefs = <
          item
            Name = 'tag[0]'
            DataType = ftInteger
          end>
        DataType = ftArray
        Size = 10
      end
      item
        Name = 'tag[1]'
        DataType = ftInteger
      end
      item
        Name = 'tag[2]'
        DataType = ftInteger
      end
      item
        Name = 'tag[3]'
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
        DataType = ftBytes
        Size = 32
      end
      item
        Name = 'usableByClass'
        DataType = ftBytes
        Size = 32
      end
      item
        Name = 'stat'
        ChildDefs = <
          item
            Name = 'stat[0]'
            DataType = ftInteger
          end>
        DataType = ftArray
        Size = 10
      end
      item
        Name = 'stat[1]'
        DataType = ftInteger
      end
      item
        Name = 'stat[2]'
        DataType = ftInteger
      end
      item
        Name = 'stat[3]'
        DataType = ftInteger
      end
      item
        Name = 'stat[4]'
        DataType = ftInteger
      end
      item
        Name = 'stat[5]'
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
        Name = 'slot'
        DataType = ftInteger
      end>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    AfterOpen = restoreClone
    OnFilterRecord = classFilter
    Left = 8
    Top = 440
    Data = {
      3A0200009619E0BD0200000018000000200000000000030000003A0202696404
      00010000000000046E616D650100490000000100055749445448020002002000
      086D6F6469666965640200030000000000086974656D54797065040001000000
      00000464657363010049000000010005574944544802000200320004636F7374
      0400010000000000037461670A000D0300000000067461675B305D0400010000
      000000067461675B315D0400010000000000067461675B325D04000100000000
      00067461675B335D040001000000000008757365734C65667404000100000000
      000B757361626C65576865726504000100000000000C757361626C6542794865
      726F20000B00000000000D757361626C654279436C61737320000B0000000000
      04737461740A000D030000000007737461745B305D0400010000000000077374
      61745B315D040001000000000007737461745B325D0400010000000000077374
      61745B335D040001000000000007737461745B345D0400010000000000077374
      61745B355D04000100000000000765766173696F6E020003000000000005746F
      48697404000100000000000A637269744368616E636504000100000000000B63
      72697450726576656E7404000100000000000A707265656D7074697665040001
      00000000000B6D70526564756374696F6E04000100000000000F6E6F54657272
      61696E44616D616765020003000000000006757361626C650200030000000000
      06637572736564020003000000000004736C6F7404000100000000000000}
    object helmetsid: TIntegerField
      FieldName = 'id'
    end
    object helmetsname: TStringField
      FieldName = 'name'
      Size = 32
    end
    object helmetsmodified: TBooleanField
      FieldName = 'modified'
    end
    object helmetsitemType: TIntegerField
      FieldName = 'itemType'
    end
    object helmetsusableByHero: TBytesField
      FieldName = 'usableByHero'
      Size = 32
    end
    object helmetsusableByClass: TBytesField
      FieldName = 'usableByClass'
      Size = 32
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
        DataType = ftString
        Size = 32
      end
      item
        Name = 'modified'
        DataType = ftBoolean
      end
      item
        Name = 'itemType'
        DataType = ftInteger
      end
      item
        Name = 'desc'
        DataType = ftString
        Size = 50
      end
      item
        Name = 'cost'
        DataType = ftInteger
      end
      item
        Name = 'tag'
        ChildDefs = <
          item
            Name = 'tag[0]'
            DataType = ftInteger
          end>
        DataType = ftArray
        Size = 10
      end
      item
        Name = 'tag[1]'
        DataType = ftInteger
      end
      item
        Name = 'tag[2]'
        DataType = ftInteger
      end
      item
        Name = 'tag[3]'
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
        DataType = ftBytes
        Size = 32
      end
      item
        Name = 'usableByClass'
        DataType = ftBytes
        Size = 32
      end
      item
        Name = 'stat'
        ChildDefs = <
          item
            Name = 'stat[0]'
            DataType = ftInteger
          end>
        DataType = ftArray
        Size = 10
      end
      item
        Name = 'stat[1]'
        DataType = ftInteger
      end
      item
        Name = 'stat[2]'
        DataType = ftInteger
      end
      item
        Name = 'stat[3]'
        DataType = ftInteger
      end
      item
        Name = 'stat[4]'
        DataType = ftInteger
      end
      item
        Name = 'stat[5]'
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
        Name = 'slot'
        DataType = ftInteger
      end>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    AfterOpen = restoreClone
    OnFilterRecord = classFilter
    Left = 8
    Top = 496
    Data = {
      3A0200009619E0BD0200000018000000200000000000030000003A0202696404
      00010000000000046E616D650100490000000100055749445448020002002000
      086D6F6469666965640200030000000000086974656D54797065040001000000
      00000464657363010049000000010005574944544802000200320004636F7374
      0400010000000000037461670A000D0300000000067461675B305D0400010000
      000000067461675B315D0400010000000000067461675B325D04000100000000
      00067461675B335D040001000000000008757365734C65667404000100000000
      000B757361626C65576865726504000100000000000C757361626C6542794865
      726F20000B00000000000D757361626C654279436C61737320000B0000000000
      04737461740A000D030000000007737461745B305D0400010000000000077374
      61745B315D040001000000000007737461745B325D0400010000000000077374
      61745B335D040001000000000007737461745B345D0400010000000000077374
      61745B355D04000100000000000765766173696F6E020003000000000005746F
      48697404000100000000000A637269744368616E636504000100000000000B63
      72697450726576656E7404000100000000000A707265656D7074697665040001
      00000000000B6D70526564756374696F6E04000100000000000F6E6F54657272
      61696E44616D616765020003000000000006757361626C650200030000000000
      06637572736564020003000000000004736C6F7404000100000000000000}
    object accessoriesid: TIntegerField
      FieldName = 'id'
    end
    object accessoriesname: TStringField
      FieldName = 'name'
      Size = 32
    end
    object accessoriesmodified: TBooleanField
      FieldName = 'modified'
    end
    object accessoriesitemType: TIntegerField
      FieldName = 'itemType'
    end
    object accessoriesusableByHero: TBytesField
      FieldName = 'usableByHero'
      Size = 32
    end
    object accessoriesusableByClass: TBytesField
      FieldName = 'usableByClass'
      Size = 32
    end
    object accessoriesslot: TIntegerField
      FieldName = 'slot'
    end
  end
  object items_junk: TClientDataSet
    Aggregates = <>
    Filter = 'itemType = 0'
    FieldDefs = <>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    AfterOpen = restoreClone
    Left = 184
    Top = 60
    object items_junkid: TIntegerField
      FieldName = 'id'
    end
    object items_junkname: TStringField
      FieldName = 'name'
      Size = 32
    end
    object items_junkmodified: TBooleanField
      FieldName = 'modified'
    end
    object items_junkdesc: TStringField
      FieldName = 'desc'
      Size = 50
    end
    object items_junkcost: TIntegerField
      FieldName = 'cost'
    end
    object items_junktag: TArrayField
      FieldName = 'tag'
      object items_junktag0: TIntegerField
        FieldName = 'tag[0]'
      end
      object items_junktag1: TIntegerField
        FieldName = 'tag[1]'
      end
      object items_junktag2: TIntegerField
        FieldName = 'tag[2]'
      end
      object items_junktag3: TIntegerField
        FieldName = 'tag[3]'
      end
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
        DataType = ftString
        Size = 32
      end
      item
        Name = 'modified'
        DataType = ftBoolean
      end
      item
        Name = 'desc'
        DataType = ftString
        Size = 50
      end
      item
        Name = 'cost'
        DataType = ftInteger
      end
      item
        Name = 'tag'
        ChildDefs = <
          item
            Name = 'tag[0]'
            DataType = ftInteger
          end>
        DataType = ftArray
        Size = 10
      end
      item
        Name = 'tag[1]'
        DataType = ftInteger
      end
      item
        Name = 'tag[2]'
        DataType = ftInteger
      end
      item
        Name = 'tag[3]'
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
        DataType = ftBytes
        Size = 32
      end
      item
        Name = 'usableByClass'
        DataType = ftBytes
        Size = 32
      end
      item
        Name = 'stat'
        ChildDefs = <
          item
            Name = 'stat[0]'
            DataType = ftInteger
          end>
        DataType = ftArray
        Size = 10
      end
      item
        Name = 'stat[1]'
        DataType = ftInteger
      end
      item
        Name = 'stat[2]'
        DataType = ftInteger
      end
      item
        Name = 'stat[3]'
        DataType = ftInteger
      end
      item
        Name = 'stat[4]'
        DataType = ftInteger
      end
      item
        Name = 'stat[5]'
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
        Name = 'conditionChance'
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
    OnFilterRecord = classFilter
    Left = 8
    Top = 224
    Data = {
      9D0200009619E0BD0200000018000000250000000000030000009D0202696404
      00010000000000046E616D650100490000000100055749445448020002002000
      086D6F6469666965640200030000000000046465736301004900000001000557
      4944544802000200320004636F73740400010000000000037461670A000D0300
      000000067461675B305D0400010000000000067461675B315D04000100000000
      00067461675B325D0400010000000000067461675B335D040001000000000008
      757365734C65667404000100000000000B757361626C65576865726504000100
      000000000C757361626C6542794865726F20000B00000000000D757361626C65
      4279436C61737320000B000000000004737461740A000D030000000007737461
      745B305D040001000000000007737461745B315D040001000000000007737461
      745B325D040001000000000007737461745B335D040001000000000007737461
      745B345D040001000000000007737461745B355D040001000000000007657661
      73696F6E020003000000000005746F48697404000100000000000A6372697443
      68616E636504000100000000000B6372697450726576656E7404000100000000
      000A707265656D707469766504000100000000000B6D70526564756374696F6E
      04000100000000000F6E6F5465727261696E44616D6167650200030000000000
      06757361626C6502000300000000000663757273656402000300000000000974
      776F48616E64656402000300000000000B61747461636B547769636502000300
      00000000076172656148697402000300000000000A626174746C65416E696D04
      00010000000000066D70436F737404000100000000000F636F6E646974696F6E
      4368616E63650400010000000000086974656D54797065040001000000000000
      00}
    object IntegerField4: TIntegerField
      FieldName = 'id'
    end
    object StringField1: TStringField
      FieldName = 'name'
      Size = 32
    end
    object BooleanField1: TBooleanField
      FieldName = 'modified'
    end
    object weaponsIntegerField: TIntegerField
      FieldName = 'itemType'
    end
    object BytesField1: TBytesField
      FieldName = 'usableByHero'
      Size = 32
    end
    object BytesField2: TBytesField
      FieldName = 'usableByClass'
      Size = 32
    end
    object BooleanField6: TBooleanField
      FieldName = 'twoHanded'
    end
  end
  object commands: TClientDataSet
    Active = True
    Aggregates = <>
    FieldDefs = <
      item
        Name = 'id'
        DataType = ftInteger
      end
      item
        Name = 'name'
        DataType = ftString
        Size = 32
      end
      item
        Name = 'modified'
        DataType = ftBoolean
      end
      item
        Name = 'style'
        DataType = ftInteger
      end
      item
        Name = 'value'
        DataType = ftInteger
      end>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 72
    Top = 504
    Data = {
      6B0000009619E0BD0100000018000000050000000000030000006B0002696404
      00010000000000046E616D650100490000000100055749445448020002002000
      086D6F6469666965640200030000000000057374796C65040001000000000005
      76616C756504000100000000000000}
    object commandsid: TIntegerField
      FieldName = 'id'
    end
    object commandsname: TStringField
      FieldName = 'name'
      Size = 32
    end
    object commandsmodified: TBooleanField
      FieldName = 'modified'
    end
    object commandsstyle: TIntegerField
      FieldName = 'style'
    end
    object commandsvalue: TIntegerField
      FieldName = 'value'
    end
  end
  object skills: TClientDataSet
    Active = True
    Aggregates = <>
    FieldDefs = <
      item
        Name = 'id'
        DataType = ftInteger
      end
      item
        Name = 'name'
        DataType = ftString
        Size = 32
      end
      item
        Name = 'modified'
        DataType = ftBoolean
      end
      item
        Name = 'cost'
        DataType = ftInteger
      end
      item
        Name = 'costPercent'
        DataType = ftBoolean
      end
      item
        Name = 'desc'
        DataType = ftString
        Size = 50
      end
      item
        Name = 'messages'
        ChildDefs = <
          item
            Name = 'useString'
            DataType = ftString
            Size = 50
          end
          item
            Name = 'useString2'
            DataType = ftString
            Size = 50
          end
          item
            Name = 'failureMessage'
            DataType = ftInteger
          end>
        DataType = ftADT
        Size = 3
      end
      item
        Name = 'usableWhere'
        DataType = ftInteger
      end
      item
        Name = 'tag'
        ChildDefs = <
          item
            Name = 'tag[0]'
            DataType = ftInteger
          end>
        DataType = ftArray
        Size = 4
      end
      item
        Name = 'tag[1]'
        DataType = ftInteger
      end
      item
        Name = 'tag[2]'
        DataType = ftInteger
      end
      item
        Name = 'tag[3]'
        DataType = ftInteger
      end>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 256
    Top = 8
    Data = {
      4A0100009619E0BD0200000018000000100000000000030000004A0102696404
      00010000000000046E616D650100490000000100055749445448020002002000
      086D6F646966696564020003000000000004636F737404000100000000000B63
      6F737450657263656E7402000300000000000464657363010049000000010005
      5749445448020002003200086D6573736167657303000C010000000009757365
      537472696E6701004900000001000557494454480200020032000A7573655374
      72696E673201004900000001000557494454480200020032000E6661696C7572
      654D65737361676504000100000000000B757361626C65576865726504000100
      000000000374616704000D0300000000067461675B305D040001000000000006
      7461675B315D0400010000000000067461675B325D0400010000000000067461
      675B335D04000100000000000000}
    object skillsid: TIntegerField
      FieldName = 'id'
    end
    object skillsname: TStringField
      FieldName = 'name'
      Size = 32
    end
    object skillsmodified: TBooleanField
      FieldName = 'modified'
    end
    object skillscost: TIntegerField
      FieldName = 'cost'
    end
    object skillscostPercent: TBooleanField
      FieldName = 'costPercent'
    end
    object skillsdesc: TStringField
      FieldName = 'desc'
      Size = 50
    end
    object skillsmessages: TADTField
      FieldName = 'messages'
      object skillsmessagesuseString: TStringField
        FieldName = 'useString'
        Size = 50
      end
      object skillsmessagesuseString2: TStringField
        FieldName = 'useString2'
        Size = 50
      end
      object skillsmessagesfailureMessage: TIntegerField
        FieldName = 'failureMessage'
      end
    end
    object skillsusableWhere: TIntegerField
      FieldName = 'usableWhere'
    end
    object skillstag: TArrayField
      FieldName = 'tag'
      object skillstag0: TIntegerField
        FieldName = 'tag[0]'
      end
      object skillstag1: TIntegerField
        FieldName = 'tag[1]'
      end
      object skillstag2: TIntegerField
        FieldName = 'tag[2]'
      end
      object skillstag3: TIntegerField
        FieldName = 'tag[3]'
      end
    end
  end
  object items: TClientDataSet
    Active = True
    Aggregates = <>
    FieldDefs = <
      item
        Name = 'id'
        DataType = ftInteger
      end
      item
        Name = 'name'
        DataType = ftString
        Size = 32
      end
      item
        Name = 'modified'
        DataType = ftBoolean
      end
      item
        Name = 'itemType'
        DataType = ftInteger
      end
      item
        Name = 'desc'
        DataType = ftString
        Size = 50
      end
      item
        Name = 'cost'
        DataType = ftInteger
      end
      item
        Name = 'tag'
        ChildDefs = <
          item
            Name = 'tag[0]'
            DataType = ftInteger
          end>
        DataType = ftArray
        Size = 10
      end
      item
        Name = 'tag[1]'
        DataType = ftInteger
      end
      item
        Name = 'tag[2]'
        DataType = ftInteger
      end
      item
        Name = 'tag[3]'
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
        DataType = ftBytes
        Size = 32
      end
      item
        Name = 'usableByClass'
        DataType = ftBytes
        Size = 32
      end
      item
        Name = 'stat'
        ChildDefs = <
          item
            Name = 'stat[0]'
            DataType = ftInteger
          end>
        DataType = ftArray
        Size = 10
      end
      item
        Name = 'stat[1]'
        DataType = ftInteger
      end
      item
        Name = 'stat[2]'
        DataType = ftInteger
      end
      item
        Name = 'stat[3]'
        DataType = ftInteger
      end
      item
        Name = 'stat[4]'
        DataType = ftInteger
      end
      item
        Name = 'stat[5]'
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
        Name = 'conditionChance'
        DataType = ftInteger
      end
      item
        Name = 'slot'
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
        Name = 'deadOnly'
        DataType = ftBoolean
      end
      item
        Name = 'skill'
        DataType = ftInteger
      end
      item
        Name = 'customSkillMessage'
        DataType = ftBoolean
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
        Name = 'stype'
        DataType = ftInteger
      end
      item
        Name = 'operation'
        DataType = ftInteger
      end
      item
        Name = 'event'
        DataType = ftString
        Size = 32
      end
      item
        Name = 'script'
        DataType = ftMemo
      end>
    IndexDefs = <
      item
        Name = 'itemsIndex1'
        Fields = 'id'
      end>
    Params = <>
    StoreDefs = True
    Left = 184
    Top = 8
    Data = {
      990300009619E0BD020000001800000032000000000003000000990302696404
      00010000000000046E616D650100490000000100055749445448020002002000
      086D6F6469666965640200030000000000086974656D54797065040001000000
      00000464657363010049000000010005574944544802000200320004636F7374
      0400010000000000037461670A000D0300000000067461675B305D0400010000
      000000067461675B315D0400010000000000067461675B325D04000100000000
      00067461675B335D040001000000000008757365734C65667404000100000000
      000B757361626C65576865726504000100000000000C757361626C6542794865
      726F20000B00000000000D757361626C654279436C61737320000B0000000000
      04737461740A000D030000000007737461745B305D0400010000000000077374
      61745B315D040001000000000007737461745B325D0400010000000000077374
      61745B335D040001000000000007737461745B345D0400010000000000077374
      61745B355D04000100000000000765766173696F6E020003000000000005746F
      48697404000100000000000A637269744368616E636504000100000000000B63
      72697450726576656E7404000100000000000A707265656D7074697665040001
      00000000000B6D70526564756374696F6E04000100000000000F6E6F54657272
      61696E44616D616765020003000000000006757361626C650200030000000000
      0663757273656402000300000000000974776F48616E64656402000300000000
      000B61747461636B547769636502000300000000000761726561486974020003
      00000000000A626174746C65416E696D0400010000000000066D70436F737404
      000100000000000F636F6E646974696F6E4368616E6365040001000000000004
      736C6F7404000100000000000C617265614D65646963696E6502000300000000
      0009687050657263656E740400010000000000096D7050657263656E74040001
      000000000008646561644F6E6C79020003000000000005736B696C6C04000100
      0000000012637573746F6D536B696C6C4D657373616765020003000000000005
      77686963680400010000000000096D61676E6974756465040001000000000005
      73747970650400010000000000096F7065726174696F6E040001000000000005
      6576656E74010049000000010005574944544802000200200006736372697074
      04004B0000000100075355425459504502004900050054657874000000}
    object IntegerField30: TIntegerField
      FieldName = 'id'
    end
    object StringField3: TStringField
      FieldName = 'name'
      Size = 32
    end
    object BooleanField9: TBooleanField
      FieldName = 'modified'
    end
    object itemsitemType: TIntegerField
      FieldName = 'itemType'
    end
    object StringField4: TStringField
      FieldName = 'desc'
      Size = 50
    end
    object IntegerField31: TIntegerField
      FieldName = 'cost'
    end
    object ArrayField3: TArrayField
      FieldName = 'tag'
      object IntegerField32: TIntegerField
        FieldName = 'tag[0]'
      end
      object IntegerField33: TIntegerField
        FieldName = 'tag[1]'
      end
      object IntegerField34: TIntegerField
        FieldName = 'tag[2]'
      end
      object IntegerField35: TIntegerField
        FieldName = 'tag[3]'
      end
    end
    object IntegerField36: TIntegerField
      FieldName = 'usesLeft'
    end
    object IntegerField37: TIntegerField
      FieldName = 'usableWhere'
    end
    object BytesField3: TBytesField
      FieldName = 'usableByHero'
      Size = 32
    end
    object BytesField4: TBytesField
      FieldName = 'usableByClass'
      Size = 32
    end
    object ArrayField4: TArrayField
      FieldName = 'stat'
      object IntegerField38: TIntegerField
        FieldName = 'stat[0]'
      end
      object IntegerField39: TIntegerField
        FieldName = 'stat[1]'
      end
      object IntegerField40: TIntegerField
        FieldName = 'stat[2]'
      end
      object IntegerField41: TIntegerField
        FieldName = 'stat[3]'
      end
      object IntegerField42: TIntegerField
        FieldName = 'stat[4]'
      end
      object IntegerField43: TIntegerField
        FieldName = 'stat[5]'
      end
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
    object BooleanField12: TBooleanField
      FieldName = 'usable'
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
    object itemsdeadOnly: TBooleanField
      FieldName = 'deadOnly'
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
    object itemsstype: TIntegerField
      FieldName = 'stype'
    end
    object itemsoperation: TIntegerField
      FieldName = 'operation'
    end
    object itemsevent: TStringField
      FieldName = 'event'
      Size = 32
    end
    object itemsscript: TMemoField
      FieldName = 'script'
      BlobType = ftMemo
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
        DataType = ftString
        Size = 32
      end
      item
        Name = 'modified'
        DataType = ftBoolean
      end
      item
        Name = 'itemType'
        DataType = ftInteger
      end
      item
        Name = 'desc'
        DataType = ftString
        Size = 50
      end
      item
        Name = 'cost'
        DataType = ftInteger
      end
      item
        Name = 'tag'
        ChildDefs = <
          item
            Name = 'tag[0]'
            DataType = ftInteger
          end>
        DataType = ftArray
        Size = 10
      end
      item
        Name = 'tag[1]'
        DataType = ftInteger
      end
      item
        Name = 'tag[2]'
        DataType = ftInteger
      end
      item
        Name = 'tag[3]'
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
        DataType = ftBytes
        Size = 32
      end
      item
        Name = 'usableByClass'
        DataType = ftBytes
        Size = 32
      end
      item
        Name = 'stat'
        ChildDefs = <
          item
            Name = 'stat[0]'
            DataType = ftInteger
          end>
        DataType = ftArray
        Size = 10
      end
      item
        Name = 'stat[1]'
        DataType = ftInteger
      end
      item
        Name = 'stat[2]'
        DataType = ftInteger
      end
      item
        Name = 'stat[3]'
        DataType = ftInteger
      end
      item
        Name = 'stat[4]'
        DataType = ftInteger
      end
      item
        Name = 'stat[5]'
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
        Name = 'conditionChance'
        DataType = ftInteger
      end
      item
        Name = 'slot'
        DataType = ftInteger
      end>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    AfterOpen = restoreClone
    OnFilterRecord = classFilter
    Left = 8
    Top = 336
    Data = {
      AA0200009619E0BD020000001800000026000000000003000000AA0202696404
      00010000000000046E616D650100490000000100055749445448020002002000
      086D6F6469666965640200030000000000086974656D54797065040001000000
      00000464657363010049000000010005574944544802000200320004636F7374
      0400010000000000037461670A000D0300000000067461675B305D0400010000
      000000067461675B315D0400010000000000067461675B325D04000100000000
      00067461675B335D040001000000000008757365734C65667404000100000000
      000B757361626C65576865726504000100000000000C757361626C6542794865
      726F20000B00000000000D757361626C654279436C61737320000B0000000000
      04737461740A000D030000000007737461745B305D0400010000000000077374
      61745B315D040001000000000007737461745B325D0400010000000000077374
      61745B335D040001000000000007737461745B345D0400010000000000077374
      61745B355D04000100000000000765766173696F6E020003000000000005746F
      48697404000100000000000A637269744368616E636504000100000000000B63
      72697450726576656E7404000100000000000A707265656D7074697665040001
      00000000000B6D70526564756374696F6E04000100000000000F6E6F54657272
      61696E44616D616765020003000000000006757361626C650200030000000000
      0663757273656402000300000000000974776F48616E64656402000300000000
      000B61747461636B547769636502000300000000000761726561486974020003
      00000000000A626174746C65416E696D0400010000000000066D70436F737404
      000100000000000F636F6E646974696F6E4368616E6365040001000000000004
      736C6F7404000100000000000000}
    object IntegerField52: TIntegerField
      FieldName = 'id'
    end
    object StringField5: TStringField
      FieldName = 'name'
      Size = 32
    end
    object BooleanField17: TBooleanField
      FieldName = 'modified'
    end
    object offhandsitemType: TIntegerField
      FieldName = 'itemType'
    end
    object BytesField5: TBytesField
      FieldName = 'usableByHero'
      Size = 32
    end
    object BytesField6: TBytesField
      FieldName = 'usableByClass'
      Size = 32
    end
    object BooleanField22: TBooleanField
      FieldName = 'twoHanded'
    end
    object offhandsIntegerField: TIntegerField
      FieldName = 'slot'
    end
  end
  object dsCharClasses: TDataSource
    DataSet = charClasses
    Left = 824
    Top = 8
  end
  object attributes: TClientDataSet
    Active = True
    Aggregates = <>
    Params = <>
    Left = 456
    Top = 8
    Data = {
      7A0000009619E0BD0100000018000000050000000000030000007A0002696404
      00010000000000046E616D650100490000000100055749445448020002002000
      086D6F6469666965640200030000000000117265717569726564466F72536B69
      6C6C730200030000000000087374616E6461726404000100000000000000}
    object attributesid: TIntegerField
      FieldName = 'id'
    end
    object attributesname: TStringField
      FieldName = 'name'
      Size = 32
    end
    object attributesmodified: TBooleanField
      FieldName = 'modified'
    end
    object attributesrequiredForSkills: TBooleanField
      FieldName = 'requiredForSkills'
    end
    object attributesStandard: TIntegerField
      FieldName = 'standard'
    end
  end
  object conditions: TClientDataSet
    Active = True
    Aggregates = <>
    FieldDefs = <
      item
        Name = 'id'
        DataType = ftInteger
      end
      item
        Name = 'name'
        DataType = ftString
        Size = 32
      end
      item
        Name = 'modified'
        DataType = ftBoolean
      end
      item
        Name = 'outOfBattle'
        DataType = ftBoolean
      end
      item
        Name = 'color'
        DataType = ftWord
      end
      item
        Name = 'priority'
        DataType = ftWord
      end
      item
        Name = 'attackLimit'
        DataType = ftWord
      end
      item
        Name = 'tag'
        ChildDefs = <
          item
            Name = 'tag[0]'
            DataType = ftInteger
          end>
        DataType = ftArray
        Size = 10
      end
      item
        Name = 'tag[1]'
        DataType = ftInteger
      end
      item
        Name = 'tag[2]'
        DataType = ftInteger
      end
      item
        Name = 'tag[3]'
        DataType = ftInteger
      end>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 456
    Top = 64
    Data = {
      DE0000009619E0BD02000000180000000C000000000003000000DE0002696404
      00010000000000046E616D650100490000000100055749445448020002002000
      086D6F64696669656402000300000000000B6F75744F66426174746C65020003
      000000000005636F6C6F720200020000000000087072696F7269747902000200
      000000000B61747461636B4C696D69740200020000000000037461670A000D03
      00000000067461675B305D0400010000000000067461675B315D040001000000
      0000067461675B325D0400010000000000067461675B335D0400010000000000
      0000}
    object conditionsId: TIntegerField
      FieldName = 'id'
    end
    object conditionsName: TStringField
      FieldName = 'name'
      Size = 32
    end
    object conditionsModified: TBooleanField
      FieldName = 'modified'
    end
    object conditionsOutOfBattle: TBooleanField
      FieldName = 'outOfBattle'
    end
    object conditionscolor: TWordField
      FieldName = 'color'
    end
    object conditionspriority: TWordField
      FieldName = 'priority'
    end
    object conditionsattackLimit: TWordField
      FieldName = 'attackLimit'
    end
    object conditionstag: TArrayField
      FieldName = 'tag'
      object conditionstag0: TIntegerField
        FieldName = 'tag[0]'
      end
      object conditionstag1: TIntegerField
        FieldName = 'tag[1]'
      end
      object conditionstag2: TIntegerField
        FieldName = 'tag[2]'
      end
      object conditionstag3: TIntegerField
        FieldName = 'tag[3]'
      end
    end
  end
  object skillGainRecords: TClientDataSet
    Active = True
    Aggregates = <>
    FieldDefs = <
      item
        Name = 'name'
        DataType = ftString
        Size = 32
      end
      item
        Name = 'designName'
        DataType = ftString
        Size = 32
      end
      item
        Name = 'address'
        DataType = ftInteger
      end
      item
        Name = 'baseMethod'
        DataType = ftInteger
      end
      item
        Name = 'style'
        DataType = ftInteger
      end
      item
        Name = 'arrayArgs'
        DataType = ftBoolean
      end
      item
        Name = 'displayName'
        DataType = ftString
        Size = 32
      end
      item
        Name = 'displayMethod'
        DataType = ftInteger
      end>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 752
    Top = 64
    Data = {
      CB0000009619E0BD010000001800000008000000000003000000CB00046E616D
      6501004900000001000557494454480200020020000A64657369676E4E616D65
      0100490000000100055749445448020002002000076164647265737304000100
      000000000A626173654D6574686F640400010000000000057374796C65040001
      00000000000961727261794172677302000300000000000B646973706C61794E
      616D6501004900000001000557494454480200020020000D646973706C61794D
      6574686F6404000100000000000000}
    object skillGainRecordsName: TStringField
      FieldName = 'name'
      Size = 32
    end
    object skillGainRecordsDesignName: TStringField
      FieldName = 'designName'
      Size = 32
    end
    object skillGainRecordsAddress: TIntegerField
      FieldName = 'address'
    end
    object skillGainRecordsBaseMethod: TIntegerField
      FieldName = 'baseMethod'
    end
    object skillGainRecordsStyle: TIntegerField
      FieldName = 'style'
    end
    object skillGainRecordsArrayArgs: TBooleanField
      FieldName = 'arrayArgs'
    end
    object skillGainRecordsDisplayName: TStringField
      FieldName = 'displayName'
      Size = 32
    end
    object skillGainRecordsDisplayMethod: TIntegerField
      FieldName = 'displayMethod'
    end
    object _TScriptRecordStart: TIntegerField
      FieldKind = fkLookup
      FieldName = 'start'
      LookupDataSet = scriptRange
      LookupKeyFields = 'name'
      LookupResultField = 'start'
      KeyFields = 'displayName'
      Lookup = True
    end
    object _TScriptRecordEnd: TIntegerField
      FieldKind = fkLookup
      FieldName = 'end'
      LookupDataSet = scriptRange
      LookupKeyFields = 'name'
      LookupResultField = 'end'
      KeyFields = 'displayName'
      Lookup = True
    end
    object skillGainRecordsunit: TStringField
      FieldKind = fkLookup
      FieldName = 'unit'
      LookupDataSet = scriptRange
      LookupKeyFields = 'name'
      LookupResultField = 'unit'
      KeyFields = 'name'
      Size = 32
      Lookup = True
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
        DataType = ftString
        Size = 32
      end
      item
        Name = 'modified'
        DataType = ftBoolean
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
        DataType = ftString
        Size = 32
      end>
    IndexDefs = <
      item
        Name = 'DEFAULT_ORDER'
      end
      item
        Name = 'CHANGEINDEX'
      end>
    Params = <>
    StoreDefs = True
    Left = 752
    Top = 8
    Data = {
      980000009619E0BD010000001800000006000000000003000000980002696404
      00010000000000046E616D650100490000000100055749445448020002002000
      086D6F6469666965640200030000000000057374617274040001000000000003
      656E64040001000000000004756E697401004900000001000557494454480200
      0200200001000D44454641554C545F4F524445520200820000000000}
    object scriptRangeId: TIntegerField
      FieldName = 'id'
    end
    object StringField2: TStringField
      FieldName = 'name'
      LookupDataSet = attributes
      LookupKeyFields = 'id'
      LookupResultField = 'name'
      KeyFields = 'x'
      Size = 32
    end
    object scriptRangemodified: TBooleanField
      FieldName = 'modified'
    end
    object IntegerField6: TIntegerField
      FieldName = 'start'
    end
    object IntegerField7: TIntegerField
      FieldName = 'end'
    end
    object scriptRangeUnit: TStringField
      FieldName = 'unit'
      Size = 32
    end
  end
  object expCalcRecords: TClientDataSet
    Active = True
    Aggregates = <>
    FieldDefs = <
      item
        Name = 'name'
        DataType = ftString
        Size = 32
      end
      item
        Name = 'designName'
        DataType = ftString
        Size = 32
      end
      item
        Name = 'address'
        DataType = ftInteger
      end
      item
        Name = 'baseMethod'
        DataType = ftInteger
      end>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 752
    Top = 120
    Data = {
      750000009619E0BD0100000018000000040000000000030000007500046E616D
      6501004900000001000557494454480200020020000A64657369676E4E616D65
      0100490000000100055749445448020002002000076164647265737304000100
      000000000A626173654D6574686F6404000100000000000000}
    object expCalcRecordsname: TStringField
      FieldName = 'name'
      Size = 32
    end
    object expCalcRecordsdesignName: TStringField
      FieldName = 'designName'
      Size = 32
    end
    object expCalcRecordsaddress: TIntegerField
      FieldName = 'address'
    end
    object expCalcRecordsBaseMethod: TIntegerField
      FieldName = 'baseMethod'
    end
    object expCalcRecordsStart: TIntegerField
      FieldKind = fkLookup
      FieldName = 'start'
      LookupDataSet = scriptRange
      LookupKeyFields = 'name'
      LookupResultField = 'start'
      KeyFields = 'name'
      Lookup = True
    end
    object expCalcRecordsEnd: TIntegerField
      FieldKind = fkLookup
      FieldName = 'end'
      LookupDataSet = scriptRange
      LookupKeyFields = 'name'
      LookupResultField = 'end'
      KeyFields = 'name'
      Lookup = True
    end
    object expCalcRecordsunit: TStringField
      FieldKind = fkLookup
      FieldName = 'unit'
      LookupDataSet = scriptRange
      LookupKeyFields = 'name'
      LookupResultField = 'unit'
      KeyFields = 'name'
      Size = 32
      Lookup = True
    end
  end
end
