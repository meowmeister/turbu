object dmDatabase: TdmDatabase
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 600
  Width = 883
  object charClasses: TClientDataSet
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
        Size = 20
      end
      item
        Name = 'modified'
        DataType = ftBoolean
      end
      item
        Name = 'mapSprite'
        DataType = ftString
        Size = 255
      end
      item
        Name = 'actionMatrix'
        DataType = ftInteger
      end
      item
        Name = 'battleSprite'
        DataType = ftInteger
      end
      item
        Name = 'battleMatrix'
        DataType = ftInteger
      end
      item
        Name = 'portrait'
        DataType = ftString
        Size = 255
      end
      item
        Name = 'portraitIndex'
        DataType = ftInteger
      end
      item
        Name = 'command[1]'
        DataType = ftInteger
      end
      item
        Name = 'command[2]'
        DataType = ftInteger
      end
      item
        Name = 'command[3]'
        DataType = ftInteger
      end
      item
        Name = 'command[4]'
        DataType = ftInteger
      end
      item
        Name = 'command[5]'
        DataType = ftInteger
      end
      item
        Name = 'command[6]'
        DataType = ftInteger
      end
      item
        Name = 'command[7]'
        DataType = ftInteger
      end
      item
        Name = 'commands'
        DataType = ftByte
      end
      item
        Name = 'statblock[1]'
        DataType = ftInteger
      end
      item
        Name = 'statblock[2]'
        DataType = ftInteger
      end
      item
        Name = 'statblock[3]'
        DataType = ftInteger
      end
      item
        Name = 'statblock[4]'
        DataType = ftInteger
      end
      item
        Name = 'statblock[5]'
        DataType = ftInteger
      end
      item
        Name = 'statblock[6]'
        DataType = ftInteger
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
        Name = 'expVars[1]'
        DataType = ftInteger
      end
      item
        Name = 'expVars[2]'
        DataType = ftInteger
      end
      item
        Name = 'expVars[3]'
        DataType = ftInteger
      end
      item
        Name = 'expVars[4]'
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
        Name = 'equip[1]'
        DataType = ftInteger
      end
      item
        Name = 'equip[2]'
        DataType = ftInteger
      end
      item
        Name = 'equip[3]'
        DataType = ftInteger
      end
      item
        Name = 'equip[4]'
        DataType = ftInteger
      end
      item
        Name = 'equip[5]'
        DataType = ftInteger
      end>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 8
    Top = 8
    Data = {
      3E0300009619E0BD01000000180000002A0000000000030000003E0302696404
      00010000000000046E616D650100490000000100055749445448020002001400
      086D6F6469666965640200030000000000096D61705370726974650200490000
      00010005574944544802000200FF000C616374696F6E4D617472697804000100
      000000000C626174746C6553707269746504000100000000000C626174746C65
      4D6174726978040001000000000008706F727472616974020049000000010005
      574944544802000200FF000D706F727472616974496E64657804000100000000
      000A636F6D6D616E645B315D04000100000000000A636F6D6D616E645B325D04
      000100000000000A636F6D6D616E645B335D04000100000000000A636F6D6D61
      6E645B345D04000100000000000A636F6D6D616E645B355D0400010000000000
      0A636F6D6D616E645B365D04000100000000000A636F6D6D616E645B375D0400
      01000000000008636F6D6D616E647301000200000000000C73746174626C6F63
      6B5B315D04000100000000000C73746174626C6F636B5B325D04000100000000
      000C73746174626C6F636B5B335D04000100000000000C73746174626C6F636B
      5B345D04000100000000000C73746174626C6F636B5B355D0400010000000000
      0C73746174626C6F636B5B365D04000100000000000253700800010000000000
      0641747461636B080001000000000007446566656E7365080001000000000004
      4D696E6408000100000000000553706565640800010000000000076578704675
      6E6301004900000001000557494454480200020020000A657870566172735B31
      5D04000100000000000A657870566172735B325D04000100000000000A657870
      566172735B335D04000100000000000A657870566172735B345D040001000000
      0000096475616C5769656C640400010000000000087374617469634571020003
      0000000000097374726F6E6744656602000300000000000B756E61726D656441
      6E696D04000100000000000865717569705B315D040001000000000008657175
      69705B325D04000100000000000865717569705B335D04000100000000000865
      717569705B345D04000100000000000865717569705B355D0400010000000000
      0000}
    object charClassesid: TIntegerField
      FieldName = 'id'
    end
    object charClassesname: TStringField
      FieldName = 'name'
    end
    object charClassesmodified: TBooleanField
      FieldName = 'modified'
    end
    object charClassesmapSprite: TStringField
      DisplayWidth = 20
      FieldName = 'mapSprite'
      Size = 255
    end
    object charClassesActionMatrix: TIntegerField
      FieldName = 'actionMatrix'
    end
    object charClassesbattleSprite: TIntegerField
      FieldName = 'battleSprite'
    end
    object charClassesBattleMatrix: TIntegerField
      FieldName = 'battleMatrix'
    end
    object charClassesPortrait: TStringField
      DisplayWidth = 20
      FieldName = 'portrait'
      Size = 255
    end
    object charClassesPortraitIndex: TIntegerField
      FieldName = 'portraitIndex'
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
    object charClassescommand7: TIntegerField
      FieldName = 'command[7]'
    end
    object charClassescommands: TByteField
      FieldName = 'commands'
    end
    object charClassesstatblock1: TIntegerField
      FieldName = 'statblock[1]'
    end
    object charClassesstatblock2: TIntegerField
      FieldName = 'statblock[2]'
    end
    object charClassesstatblock3: TIntegerField
      FieldName = 'statblock[3]'
    end
    object charClassesstatblock4: TIntegerField
      FieldName = 'statblock[4]'
    end
    object charClassesstatblock5: TIntegerField
      FieldName = 'statblock[5]'
    end
    object charClassesstatblock6: TIntegerField
      FieldName = 'statblock[6]'
    end
    object charClassesSp: TLargeintField
      FieldName = 'Sp'
    end
    object charClassesAttack: TLargeintField
      FieldName = 'Attack'
    end
    object charClassesDefense: TLargeintField
      FieldName = 'Defense'
    end
    object charClassesMind: TLargeintField
      FieldName = 'Mind'
    end
    object charClassesSpeed: TLargeintField
      FieldName = 'Speed'
    end
    object charClassesexpFunc: TStringField
      FieldName = 'expFunc'
      Size = 32
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
    object charClassesexpVars0: TIntegerField
      FieldName = 'expVars[4]'
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
    object charClassesequip5: TIntegerField
      FieldName = 'equip[5]'
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
      KeyFields = 'equip[1]'
      Size = 32
      Lookup = True
    end
    object charClassesweapon2Name: TStringField
      FieldKind = fkLookup
      FieldName = 'weapon2Name'
      LookupDataSet = weapons
      LookupKeyFields = 'id'
      LookupResultField = 'name'
      KeyFields = 'equip[2]'
      Size = 32
      Lookup = True
    end
    object charClassesOffhandName: TStringField
      FieldKind = fkLookup
      FieldName = 'offhandName'
      LookupDataSet = offhands
      LookupKeyFields = 'id'
      LookupResultField = 'name'
      KeyFields = 'equip[2]'
      Size = 32
      Lookup = True
    end
    object charClassesshieldName: TStringField
      FieldKind = fkLookup
      FieldName = 'shieldName'
      LookupDataSet = shields
      LookupKeyFields = 'id'
      LookupResultField = 'name'
      KeyFields = 'equip[2]'
      Size = 32
      Lookup = True
    end
    object charClassesArmorName: TStringField
      FieldKind = fkLookup
      FieldName = 'armorName'
      LookupDataSet = armors
      LookupKeyFields = 'id'
      LookupResultField = 'name'
      KeyFields = 'equip[3]'
      Size = 32
      Lookup = True
    end
    object charClassesHelmetName: TStringField
      FieldKind = fkLookup
      FieldName = 'helmetName'
      LookupDataSet = helmets
      LookupKeyFields = 'id'
      LookupResultField = 'name'
      KeyFields = 'equip[4]'
      Size = 32
      Lookup = True
    end
    object charClassesAccessoryName: TStringField
      FieldKind = fkLookup
      FieldName = 'accessoryName'
      LookupDataSet = accessories
      LookupKeyFields = 'id'
      LookupResultField = 'name'
      KeyFields = 'equip[5]'
      Size = 32
      Lookup = True
    end
    object charClassesExpFuncDesignName: TStringField
      FieldKind = fkLookup
      FieldName = 'expFuncDesignName'
      LookupDataSet = scriptRange
      LookupKeyFields = 'name'
      LookupResultField = 'designName'
      KeyFields = 'expFunc'
      Size = 50
      Lookup = True
    end
  end
  object charClasses_skillset: TClientDataSet
    Active = True
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
        Name = 'nums[1]'
        DataType = ftInteger
      end
      item
        Name = 'nums[2]'
        DataType = ftInteger
      end
      item
        Name = 'nums[3]'
        DataType = ftInteger
      end
      item
        Name = 'nums[4]'
        DataType = ftInteger
      end
      item
        Name = 'method.methodName'
        DataType = ftString
        Size = 32
      end
      item
        Name = 'method.arrayArgs'
        DataType = ftBoolean
      end
      item
        Name = 'method.methodStyle'
        DataType = ftInteger
      end
      item
        Name = 'method.address'
        DataType = ftInteger
      end
      item
        Name = 'method.displayAddress'
        DataType = ftInteger
      end
      item
        Name = 'master'
        DataType = ftInteger
      end
      item
        Name = 'skill'
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
    Data = {
      3B0100009619E0BD01000000180000000D0000000000030000003B01086D6F64
      69666965640200030000000000057374796C650400010000000000076E756D73
      5B315D0400010000000000076E756D735B325D0400010000000000076E756D73
      5B335D0400010000000000076E756D735B345D0400010000000000116D657468
      6F642E6D6574686F644E616D6501004900000001000557494454480200020020
      00106D6574686F642E6172726179417267730200030000000000126D6574686F
      642E6D6574686F645374796C6504000100000000000E6D6574686F642E616464
      726573730400010000000000156D6574686F642E646973706C61794164647265
      73730400010000000000066D6173746572040001000000000005736B696C6C04
      0001000000000001000D44454641554C545F4F524445520200820000000000}
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
    object charClasses_skillsetnums1: TIntegerField
      FieldName = 'nums[1]'
    end
    object charClasses_skillsetnums2: TIntegerField
      FieldName = 'nums[2]'
    end
    object charClasses_skillsetnums3: TIntegerField
      FieldName = 'nums[3]'
    end
    object charClasses_skillsetnums4: TIntegerField
      FieldName = 'nums[4]'
    end
    object charClasses_skillsetmethodName: TStringField
      FieldName = 'method.methodName'
      Size = 32
    end
    object charClasses_skillsetarrayArgs: TBooleanField
      FieldName = 'method.arrayArgs'
    end
    object charClasses_skillsetmethodStyle: TIntegerField
      FieldName = 'method.methodStyle'
    end
    object charClasses_skillsetaddress: TIntegerField
      FieldName = 'method.address'
    end
    object charClasses_skillsetdisplayAddress: TIntegerField
      FieldName = 'method.displayAddress'
    end
    object charClasses_skillsetmaster: TIntegerField
      FieldName = 'master'
    end
    object charClasses_skillsetskill: TIntegerField
      FieldName = 'skill'
    end
    object charClasses_skillsetAlgName: TStringField
      FieldKind = fkLookup
      FieldName = 'algName'
      LookupDataSet = scriptRange
      LookupKeyFields = 'name'
      LookupResultField = 'designName'
      KeyFields = 'method.methodName'
      Size = 50
      Lookup = True
    end
  end
  object charClasses_Resists: TClientDataSet
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
    Top = 104
    Data = {
      530000009619E0BD0100000018000000030000000000030000005300066D6173
      7465720400010000000000017804000100000000000179040001000000000001
      000D44454641554C545F4F524445520200820000000000}
    object charClasses_Resistsmaster: TIntegerField
      FieldName = 'master'
    end
    object charClasses_Resistsx: TIntegerField
      FieldName = 'x'
    end
    object charClasses_Resistsy: TIntegerField
      FieldName = 'y'
    end
    object charClasses_Resistsname: TStringField
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
  object charClasses_Conditions: TClientDataSet
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
    object charClasses_Conditionsname: TStringField
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
      end
      item
        Name = 'filename'
        DataType = ftString
        Size = 255
      end>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 384
    Top = 16
    Data = {
      8C0000009619E0BD0100000018000000060000000000030000008C0002696404
      00010000000000046E616D650100490000000100055749445448020002002000
      086D6F64696669656402000300000000000768697473416C6C02000300000000
      00077954617267657404000100000000000866696C656E616D65020049000000
      010005574944544802000200FF000000}
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
    object animationsfilename: TStringField
      FieldName = 'filename'
      Size = 255
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
        Name = 'tag[4]'
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
        Name = 'stat[6]'
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
      9D0100009619E0BD0100000018000000150000000000030000009D0102696404
      00010000000000046E616D650100490000000100055749445448020002002000
      086D6F6469666965640200030000000000046465736301004900000001000557
      4944544802000200320004636F73740400010000000000067461675B315D0400
      010000000000067461675B325D0400010000000000067461675B335D04000100
      00000000067461675B345D040001000000000008757365734C65667404000100
      000000000B757361626C65576865726504000100000000000C757361626C6542
      794865726F20000B00000000000D757361626C654279436C61737320000B0000
      00000007737461745B315D040001000000000007737461745B325D0400010000
      00000007737461745B335D040001000000000007737461745B345D0400010000
      00000007737461745B355D040001000000000007737461745B365D0400010000
      000000056576656E740100490000000100055749445448020002002000067363
      7269707404004B00000001000753554254595045020049000500546578740000
      00}
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
    object items_scripttag1: TIntegerField
      FieldName = 'tag[1]'
    end
    object items_scripttag2: TIntegerField
      FieldName = 'tag[2]'
    end
    object items_scripttag3: TIntegerField
      FieldName = 'tag[3]'
    end
    object items_scripttag4: TIntegerField
      FieldName = 'tag[4]'
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
    object items_scriptevent: TStringField
      FieldName = 'event'
      Size = 32
    end
    object items_scriptscript: TMemoField
      FieldName = 'script'
      BlobType = ftMemo
    end
    object items_scriptIntegerField: TIntegerField
      FieldName = 'stat[1]'
    end
    object items_scriptIntegerField2: TIntegerField
      FieldName = 'stat[2]'
    end
    object items_scriptIntegerField3: TIntegerField
      FieldName = 'stat[3]'
    end
    object items_scriptIntegerField4: TIntegerField
      FieldName = 'stat[4]'
    end
    object items_scriptIntegerField5: TIntegerField
      FieldName = 'stat[5]'
    end
    object items_scriptIntegerField6: TIntegerField
      FieldName = 'stat[6]'
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
    object items_armortag1: TIntegerField
      FieldName = 'tag[1]'
    end
    object items_armortag2: TIntegerField
      FieldName = 'tag[2]'
    end
    object items_armortag3: TIntegerField
      FieldName = 'tag[3]'
    end
    object items_armortag4: TIntegerField
      FieldName = 'tag[4]'
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
    object items_armorstat6: TIntegerField
      FieldName = 'stat[6]'
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
  object items_attributes: TClientDataSet
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
    object items_weapontag1: TIntegerField
      FieldName = 'tag[1]'
    end
    object items_weapontag2: TIntegerField
      FieldName = 'tag[2]'
    end
    object items_weapontag3: TIntegerField
      FieldName = 'tag[3]'
    end
    object items_weapontag4: TIntegerField
      FieldName = 'tag[4]'
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
    object items_weaponstat6: TIntegerField
      FieldName = 'stat[6]'
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
        Name = 'tag[4]'
        DataType = ftInteger
      end
      item
        Name = 'usesLeft'
        DataType = ftInteger
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
        Name = 'stat[6]'
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
      end
      item
        Name = 'usableWhere'
        DataType = ftInteger
      end>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    AfterOpen = restoreClone
    Left = 184
    Top = 224
    Data = {
      B90100009619E0BD010000001800000018000000000003000000B90102696404
      00010000000000046E616D650100490000000100055749445448020002002000
      086D6F6469666965640200030000000000046465736301004900000001000557
      4944544802000200320004636F73740400010000000000067461675B315D0400
      010000000000067461675B325D0400010000000000067461675B335D04000100
      00000000067461675B345D040001000000000008757365734C65667404000100
      0000000007737461745B315D040001000000000007737461745B325D04000100
      0000000007737461745B335D040001000000000007737461745B345D04000100
      0000000007737461745B355D040001000000000007737461745B365D04000100
      000000000C617265614D65646963696E65020003000000000009687050657263
      656E740400010000000000096D7050657263656E740400010000000000086465
      61644F6E6C79020003000000000005736B696C6C04000100000000000D757361
      626C654279436C61737320000B00000000000C757361626C6542794865726F20
      000B00000000000B757361626C65576865726504000100000000000000}
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
    object items_medicineIntegerField: TIntegerField
      FieldName = 'tag[1]'
    end
    object items_medicineIntegerField2: TIntegerField
      FieldName = 'tag[2]'
    end
    object items_medicineIntegerField3: TIntegerField
      FieldName = 'tag[3]'
    end
    object items_medicineIntegerField4: TIntegerField
      FieldName = 'tag[4]'
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
    object items_medicinestat0: TIntegerField
      FieldName = 'stat[6]'
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
        Name = 'tag[4]'
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
        Name = 'stat[6]'
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
      6F0100009619E0BD0100000018000000140000000000030000006F0102696404
      00010000000000046E616D650100490000000100055749445448020002002000
      086D6F6469666965640200030000000000046465736301004900000001000557
      4944544802000200320004636F73740400010000000000067461675B315D0400
      010000000000067461675B325D0400010000000000067461675B335D04000100
      00000000067461675B345D040001000000000008757365734C65667404000100
      000000000B757361626C655768657265040001000000000007737461745B315D
      040001000000000007737461745B325D040001000000000007737461745B335D
      040001000000000007737461745B345D040001000000000007737461745B355D
      040001000000000007737461745B365D040001000000000005736B696C6C0400
      0100000000000D757361626C654279436C61737320000B00000000000C757361
      626C6542794865726F20000B00000000000000}
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
    object items_bookIntegerField: TIntegerField
      FieldName = 'tag[1]'
    end
    object items_bookIntegerField2: TIntegerField
      FieldName = 'tag[2]'
    end
    object items_bookIntegerField3: TIntegerField
      FieldName = 'tag[3]'
    end
    object items_bookIntegerField4: TIntegerField
      FieldName = 'tag[4]'
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
    object items_bookstat6: TIntegerField
      FieldName = 'stat[6]'
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
        Name = 'tag[4]'
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
        Name = 'stat[6]'
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
      8A0100009619E0BD0100000018000000150000000000030000008A0102696404
      00010000000000046E616D650100490000000100055749445448020002002000
      086D6F6469666965640200030000000000046465736301004900000001000557
      4944544802000200320004636F73740400010000000000067461675B315D0400
      010000000000067461675B325D0400010000000000067461675B335D04000100
      00000000067461675B345D040001000000000008757365734C65667404000100
      000000000B757361626C655768657265040001000000000007737461745B315D
      040001000000000007737461745B325D040001000000000007737461745B335D
      040001000000000007737461745B345D040001000000000007737461745B355D
      040001000000000007737461745B365D040001000000000005736B696C6C0400
      01000000000012637573746F6D536B696C6C4D65737361676502000300000000
      000C757361626C6542794865726F20000B00000000000D757361626C65427943
      6C61737320000B00000000000000}
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
    object items_skillIntegerField: TIntegerField
      FieldName = 'tag[1]'
    end
    object items_skillIntegerField2: TIntegerField
      FieldName = 'tag[2]'
    end
    object items_skillIntegerField3: TIntegerField
      FieldName = 'tag[3]'
    end
    object items_skillIntegerField4: TIntegerField
      FieldName = 'tag[4]'
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
    object items_skillstat6: TIntegerField
      FieldName = 'stat[6]'
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
        Name = 'tag[4]'
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
        Name = 'stat[6]'
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
      610100009619E0BD010000001800000013000000000003000000610102696404
      00010000000000046E616D650100490000000100055749445448020002002000
      086D6F6469666965640200030000000000046465736301004900000001000557
      4944544802000200320004636F73740400010000000000067461675B315D0400
      010000000000067461675B325D0400010000000000067461675B335D04000100
      00000000067461675B345D040001000000000008757365734C65667404000100
      000000000B757361626C655768657265040001000000000007737461745B315D
      040001000000000007737461745B325D040001000000000007737461745B335D
      040001000000000007737461745B345D040001000000000007737461745B355D
      040001000000000007737461745B365D04000100000000000D757361626C6542
      79436C61737320000B00000000000C757361626C6542794865726F20000B0000
      0000000000}
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
    object items_upgradeIntegerField: TIntegerField
      FieldName = 'tag[1]'
    end
    object items_upgradeIntegerField2: TIntegerField
      FieldName = 'tag[2]'
    end
    object items_upgradeIntegerField3: TIntegerField
      FieldName = 'tag[3]'
    end
    object items_upgradeIntegerField4: TIntegerField
      FieldName = 'tag[4]'
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
    object items_upgradestat6: TIntegerField
      FieldName = 'stat[6]'
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
        Name = 'tag[4]'
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
        Name = 'stat[6]'
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
        Name = 'style'
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
      A10100009619E0BD010000001800000017000000000003000000A10102696404
      00010000000000046E616D650100490000000100055749445448020002002000
      086D6F6469666965640200030000000000046465736301004900000001000557
      4944544802000200320004636F73740400010000000000067461675B315D0400
      010000000000067461675B325D0400010000000000067461675B335D04000100
      00000000067461675B345D040001000000000008757365734C65667404000100
      000000000B757361626C655768657265040001000000000007737461745B315D
      040001000000000007737461745B325D040001000000000007737461745B335D
      040001000000000007737461745B345D040001000000000007737461745B355D
      040001000000000007737461745B365D04000100000000000577686963680400
      010000000000096D61676E69747564650400010000000000057374796C650400
      010000000000096F7065726174696F6E04000100000000000D757361626C6542
      79436C61737320000B00000000000C757361626C6542794865726F20000B0000
      0000000000}
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
    object items_variableIntegerField: TIntegerField
      FieldName = 'tag[1]'
    end
    object items_variableIntegerField2: TIntegerField
      FieldName = 'tag[2]'
    end
    object items_variableIntegerField3: TIntegerField
      FieldName = 'tag[3]'
    end
    object items_scripttag0: TIntegerField
      FieldName = 'tag[4]'
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
      FieldName = 'stat[1]'
    end
    object items_variableIntegerField5: TIntegerField
      FieldName = 'stat[2]'
    end
    object items_variableIntegerField6: TIntegerField
      FieldName = 'stat[3]'
    end
    object items_variableIntegerField7: TIntegerField
      FieldName = 'stat[4]'
    end
    object items_variableIntegerField8: TIntegerField
      FieldName = 'stat[5]'
    end
    object items_variableIntegerField9: TIntegerField
      FieldName = 'stat[6]'
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
    Active = True
    Aggregates = <>
    Filter = 'itemType = 0'
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
      end>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    AfterOpen = restoreClone
    Left = 184
    Top = 60
    Data = {
      BD0000009619E0BD02000000180000000A000000000003000000BD0002696404
      00010000000000046E616D650100490000000100055749445448020002002000
      086D6F6469666965640200030000000000046465736301004900000001000557
      4944544802000200320004636F73740400010000000000037461670A000D0300
      000000067461675B305D0400010000000000067461675B315D04000100000000
      00067461675B325D0400010000000000067461675B335D040001000000000000
      00}
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
    object items_junktag1: TIntegerField
      FieldName = 'tag[1]'
    end
    object items_junktag2: TIntegerField
      FieldName = 'tag[2]'
    end
    object items_junktag3: TIntegerField
      FieldName = 'tag[3]'
    end
    object items_junktag4: TIntegerField
      FieldName = 'tag[4]'
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
        Attributes = [faRequired]
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
      end
      item
        Name = 'usableWhere'
        DataType = ftInteger
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
        Name = 'tag[4]'
        DataType = ftInteger
      end
      item
        Name = 'range'
        DataType = ftByte
      end
      item
        Name = 'offensive'
        DataType = ftBoolean
      end
      item
        Name = 'anim'
        DataType = ftWord
      end
      item
        Name = 'SkillPower[1]'
        DataType = ftInteger
      end
      item
        Name = 'SkillPower[2]'
        DataType = ftInteger
      end
      item
        Name = 'SkillPower[3]'
        DataType = ftInteger
      end
      item
        Name = 'SkillPower[4]'
        DataType = ftInteger
      end
      item
        Name = 'successRate'
        DataType = ftInteger
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
        Name = 'stat[6]'
        DataType = ftInteger
      end
      item
        Name = 'vampire'
        DataType = ftBoolean
      end
      item
        Name = 'phased'
        DataType = ftBoolean
      end
      item
        Name = 'condition'
        DataType = ftBytes
        Size = 32
      end
      item
        Name = 'resistMod'
        DataType = ftBoolean
      end
      item
        Name = 'target'
        DataType = ftByte
      end
      item
        Name = 'which'
        DataType = ftWord
      end
      item
        Name = 'magnitude'
        DataType = ftSmallint
      end
      item
        Name = 'style'
        DataType = ftByte
      end
      item
        Name = 'operation'
        DataType = ftByte
      end
      item
        Name = 'sfx.id'
        DataType = ftInteger
      end
      item
        Name = 'sfx.name'
        DataType = ftString
        Size = 255
      end
      item
        Name = 'sfx.modified'
        DataType = ftBoolean
      end
      item
        Name = 'sfx.fadeIn'
        DataType = ftInteger
      end
      item
        Name = 'sfx.tempo'
        DataType = ftInteger
      end
      item
        Name = 'sfx.volume'
        DataType = ftByte
      end
      item
        Name = 'sfx.leftBalance'
        DataType = ftByte
      end
      item
        Name = 'sfx.rightBalance'
        DataType = ftByte
      end>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 256
    Top = 8
    Data = {
      620300009619E0BD01000000180000002D000000000003000000620302696404
      00010004000000046E616D650100490000000100055749445448020002002000
      086D6F646966696564020003000000000004636F737404000100000000000B63
      6F737450657263656E7402000300000000000464657363010049000000010005
      574944544802000200320009757365537472696E670100490000000100055749
      4454480200020032000A757365537472696E6732010049000000010005574944
      54480200020032000E6661696C7572654D65737361676504000100000000000B
      757361626C6557686572650400010000000000067461675B315D040001000000
      0000067461675B325D0400010000000000067461675B335D0400010000000000
      067461675B345D04000100000000000572616E67650100020000000000096F66
      66656E73697665020003000000000004616E696D02000200000000000D536B69
      6C6C506F7765725B315D04000100000000000D536B696C6C506F7765725B325D
      04000100000000000D536B696C6C506F7765725B335D04000100000000000D53
      6B696C6C506F7765725B345D04000100000000000B7375636365737352617465
      040001000000000007737461745B315D040001000000000007737461745B325D
      040001000000000007737461745B335D040001000000000007737461745B345D
      040001000000000007737461745B355D040001000000000007737461745B365D
      04000100000000000776616D7069726502000300000000000670686173656402
      0003000000000009636F6E646974696F6E20000B000000000009726573697374
      4D6F640200030000000000067461726765740100020000000000057768696368
      0200020000000000096D61676E69747564650200010000000000057374796C65
      0100020000000000096F7065726174696F6E0100020000000000067366782E69
      640400010000000000087366782E6E616D650200490000000100055749445448
      02000200FF000C7366782E6D6F64696669656402000300000000000A7366782E
      66616465496E0400010000000000097366782E74656D706F0400010000000000
      0A7366782E766F6C756D6501000200000000000F7366782E6C65667442616C61
      6E63650100020000000000107366782E726967687442616C616E636501000200
      000000000000}
    object skillsid: TIntegerField
      FieldName = 'id'
      Required = True
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
    object skillsskillMessagesuseString: TStringField
      FieldName = 'useString'
      Size = 50
    end
    object skillsskillMessagesuseString2: TStringField
      FieldName = 'useString2'
      Size = 50
    end
    object skillsskillMessagesfailureMessage: TIntegerField
      FieldName = 'failureMessage'
    end
    object skillsusableWhere: TIntegerField
      FieldName = 'usableWhere'
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
    object skillstag4: TIntegerField
      FieldName = 'tag[4]'
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
      FieldName = 'SkillPower[1]'
    end
    object skillsSkillPower2: TIntegerField
      FieldName = 'SkillPower[2]'
    end
    object skillsSkillPower3: TIntegerField
      FieldName = 'SkillPower[3]'
    end
    object skillsSkillPower4: TIntegerField
      FieldName = 'SkillPower[4]'
    end
    object skillssuccessRate: TIntegerField
      FieldName = 'successRate'
    end
    object skillsstat1: TIntegerField
      FieldName = 'stat[1]'
    end
    object skillsstat2: TIntegerField
      FieldName = 'stat[2]'
    end
    object skillsstat3: TIntegerField
      FieldName = 'stat[3]'
    end
    object skillsstat4: TIntegerField
      FieldName = 'stat[4]'
    end
    object skillsstat5: TIntegerField
      FieldName = 'stat[5]'
    end
    object skillsstat6: TIntegerField
      FieldName = 'stat[6]'
    end
    object skillsvampire: TBooleanField
      FieldName = 'vampire'
    end
    object skillsphased: TBooleanField
      FieldName = 'phased'
    end
    object skillscondition: TBytesField
      FieldName = 'condition'
      Size = 32
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
      FieldName = 'sfx.id'
    end
    object skills_sfxname: TStringField
      FieldName = 'sfx.name'
      Size = 255
    end
    object skills_sfxmodified: TBooleanField
      FieldName = 'sfx.modified'
    end
    object skills_sfxFadeIn: TIntegerField
      FieldName = 'sfx.fadeIn'
    end
    object skills_sfxTempo: TIntegerField
      FieldName = 'sfx.tempo'
    end
    object skills_sfxVolume: TByteField
      FieldName = 'sfx.volume'
    end
    object skills_sfxLeftBalance: TByteField
      FieldName = 'sfx.leftBalance'
    end
    object skills_sfxRightBalance: TByteField
      FieldName = 'sfx.rightBalance'
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
        Name = 'tag[4]'
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
        Name = 'stat[6]'
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
        Name = 'conditions'
        DataType = ftBytes
        Size = 32
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
        Name = 'style'
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
      930300009619E0BD010000001800000031000000000003000000930302696404
      00010000000000046E616D650100490000000100055749445448020002002000
      086D6F6469666965640200030000000000086974656D54797065040001000000
      00000464657363010049000000010005574944544802000200320004636F7374
      0400010000000000067461675B315D0400010000000000067461675B325D0400
      010000000000067461675B335D0400010000000000067461675B345D04000100
      0000000008757365734C65667404000100000000000B757361626C6557686572
      6504000100000000000C757361626C6542794865726F20000B00000000000D75
      7361626C654279436C61737320000B000000000007737461745B315D04000100
      0000000007737461745B325D040001000000000007737461745B335D04000100
      0000000007737461745B345D040001000000000007737461745B355D04000100
      0000000007737461745B365D04000100000000000765766173696F6E02000300
      0000000005746F48697404000100000000000A637269744368616E6365040001
      00000000000B6372697450726576656E7404000100000000000A707265656D70
      7469766504000100000000000B6D70526564756374696F6E0400010000000000
      0F6E6F5465727261696E44616D616765020003000000000006757361626C6502
      000300000000000A636F6E646974696F6E7320000B0000000000066375727365
      6402000300000000000974776F48616E64656402000300000000000B61747461
      636B54776963650200030000000000076172656148697402000300000000000A
      626174746C65416E696D0400010000000000066D70436F737404000100000000
      000F636F6E646974696F6E4368616E6365040001000000000004736C6F740400
      0100000000000C617265614D65646963696E6502000300000000000968705065
      7263656E740400010000000000096D7050657263656E74040001000000000008
      646561644F6E6C79020003000000000005736B696C6C04000100000000001263
      7573746F6D536B696C6C4D657373616765020003000000000005776869636804
      00010000000000096D61676E69747564650400010000000000057374796C6504
      00010000000000096F7065726174696F6E0400010000000000056576656E7401
      004900000001000557494454480200020020000673637269707404004B000000
      0100075355425459504502004900050054657874000000}
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
    object itemstag1: TIntegerField
      FieldName = 'tag[1]'
    end
    object itemstag2: TIntegerField
      FieldName = 'tag[2]'
    end
    object itemstag3: TIntegerField
      FieldName = 'tag[3]'
    end
    object itemstag4: TIntegerField
      FieldName = 'tag[4]'
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
    object itemsConditions: TBytesField
      FieldName = 'conditions'
      Size = 32
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
    object itemsStyle: TIntegerField
      FieldName = 'style'
    end
    object itemsOperation: TIntegerField
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
    object itemsstat1: TIntegerField
      FieldName = 'stat[1]'
    end
    object itemsstat2: TIntegerField
      FieldName = 'stat[2]'
    end
    object itemsstat3: TIntegerField
      FieldName = 'stat[3]'
    end
    object itemsstat4: TIntegerField
      FieldName = 'stat[4]'
    end
    object itemsstat5: TIntegerField
      FieldName = 'stat[5]'
    end
    object itemsstat6: TIntegerField
      FieldName = 'stat[6]'
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
        Name = 'tag[4]'
        DataType = ftInteger
      end>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 456
    Top = 64
    Data = {
      D20000009619E0BD01000000180000000B000000000003000000D20002696404
      00010000000000046E616D650100490000000100055749445448020002002000
      086D6F64696669656402000300000000000B6F75744F66426174746C65020003
      000000000005636F6C6F720200020000000000087072696F7269747902000200
      000000000B61747461636B4C696D69740200020000000000067461675B315D04
      00010000000000067461675B325D0400010000000000067461675B335D040001
      0000000000067461675B345D04000100000000000000}
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
    object conditionstag1: TIntegerField
      FieldName = 'tag[1]'
    end
    object conditionstag2: TIntegerField
      FieldName = 'tag[2]'
    end
    object conditionstag3: TIntegerField
      FieldName = 'tag[3]'
    end
    object conditionstag4: TIntegerField
      FieldName = 'tag[4]'
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
        Name = 'designName'
        DataType = ftString
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
      B70000009619E0BD010000001800000007000000000003000000B70002696404
      00010000000000046E616D650100490000000100055749445448020002002000
      086D6F64696669656402000300000000000A64657369676E4E616D6502004900
      0000010005574944544802000200FF0005737461727404000100000000000365
      6E64040001000000000004756E69740100490000000100055749445448020002
      00200001000D44454641554C545F4F524445520200820000000000}
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
    object scriptRangedesignName: TStringField
      FieldName = 'designName'
      Size = 255
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
  object metadata: TClientDataSet
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
        Size = 20
      end
      item
        Name = 'modified'
        DataType = ftBoolean
      end
      item
        Name = 'parent'
        DataType = ftSmallint
      end
      item
        Name = 'scrollPosition.x'
        DataType = ftInteger
      end
      item
        Name = 'scrollPosition.y'
        DataType = ftInteger
      end
      item
        Name = 'treeOpen'
        DataType = ftBoolean
      end
      item
        Name = 'bgmState'
        DataType = ftByte
      end
      item
        Name = 'BgmData.id'
        DataType = ftInteger
      end
      item
        Name = 'BgmData.name'
        DataType = ftString
        Size = 255
      end
      item
        Name = 'BgmData.modified'
        DataType = ftBoolean
      end
      item
        Name = 'BgmData.fadeIn'
        DataType = ftInteger
      end
      item
        Name = 'BgmData.tempo'
        DataType = ftInteger
      end
      item
        Name = 'BgmData.volume'
        DataType = ftByte
      end
      item
        Name = 'BgmData.leftBalance'
        DataType = ftByte
      end
      item
        Name = 'BgmData.rightBalance'
        DataType = ftByte
      end
      item
        Name = 'battleBgState'
        DataType = ftByte
      end
      item
        Name = 'battleBgName'
        DataType = ftString
        Size = 255
      end
      item
        Name = 'canPort'
        DataType = ftByte
      end
      item
        Name = 'canEscape'
        DataType = ftByte
      end
      item
        Name = 'canSave'
        DataType = ftByte
      end
      item
        Name = 'internalFilename.Name'
        DataType = ftString
        Size = 255
      end
      item
        Name = 'internalFilename.duplicates'
        DataType = ftInteger
      end
      item
        Name = 'mapEngine'
        DataType = ftShortint
      end>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 664
    Top = 8
    Data = {
      450200009619E0BD010000001800000018000000000003000000450202696404
      00010000000000046E616D650100490000000100055749445448020002001400
      086D6F646966696564020003000000000006706172656E740200010000000000
      107363726F6C6C506F736974696F6E2E780400010000000000107363726F6C6C
      506F736974696F6E2E79040001000000000008747265654F70656E0200030000
      0000000862676D537461746501000200000000000A42676D446174612E696404
      000100000000000C42676D446174612E6E616D65020049000000010005574944
      544802000200FF001042676D446174612E6D6F64696669656402000300000000
      000E42676D446174612E66616465496E04000100000000000D42676D44617461
      2E74656D706F04000100000000000E42676D446174612E766F6C756D65010002
      00000000001342676D446174612E6C65667442616C616E636501000200000000
      001442676D446174612E726967687442616C616E636501000200000000000D62
      6174746C654267537461746501000200000000000C626174746C6542674E616D
      65020049000000010005574944544802000200FF000763616E506F7274010002
      00000000000963616E45736361706501000200000000000763616E5361766501
      0002000000000015696E7465726E616C46696C656E616D652E4E616D65020049
      000000010005574944544802000200FF001B696E7465726E616C46696C656E61
      6D652E6475706C6963617465730400010000000000096D6170456E67696E6501
      000100000000000000}
    object metadataId: TIntegerField
      FieldName = 'id'
    end
    object metadataName: TStringField
      FieldName = 'name'
    end
    object metadataModified: TBooleanField
      FieldName = 'modified'
    end
    object metadataParent: TSmallintField
      FieldName = 'parent'
    end
    object metadatascrollPositionx: TIntegerField
      FieldName = 'scrollPosition.x'
    end
    object metadataScrollPositiony: TIntegerField
      FieldName = 'scrollPosition.y'
    end
    object metadataTreeOpen: TBooleanField
      FieldName = 'treeOpen'
    end
    object metadataBgmState: TByteField
      FieldName = 'bgmState'
    end
    object skills_BgmDataid: TIntegerField
      FieldName = 'BgmData.id'
    end
    object skills_BgmDataname: TStringField
      FieldName = 'BgmData.name'
      Size = 255
    end
    object skills_BgmDatamodified: TBooleanField
      FieldName = 'BgmData.modified'
    end
    object skills_BgmDataFadeIn: TIntegerField
      FieldName = 'BgmData.fadeIn'
    end
    object skills_BgmDataTempo: TIntegerField
      FieldName = 'BgmData.tempo'
    end
    object skills_BgmDataVolume: TByteField
      FieldName = 'BgmData.volume'
    end
    object skills_BgmDataLeftBalance: TByteField
      FieldName = 'BgmData.leftBalance'
    end
    object skills_BgmDataRightBalance: TByteField
      FieldName = 'BgmData.rightBalance'
    end
    object metadataBattleBgState: TByteField
      FieldName = 'battleBgState'
    end
    object metadataBattleBgName: TStringField
      FieldName = 'battleBgName'
      Size = 255
    end
    object metadataCanPort: TByteField
      FieldName = 'canPort'
    end
    object metadataCanEscape: TByteField
      FieldName = 'canEscape'
    end
    object metadataCanSave: TByteField
      FieldName = 'canSave'
    end
    object metadataInternalFilenameName: TStringField
      FieldName = 'internalFilename.Name'
      Size = 255
    end
    object metadataInternalFilenameDuplicates: TIntegerField
      FieldName = 'internalFilename.duplicates'
    end
    object metadataMapEngine: TShortintField
      FieldName = 'mapEngine'
    end
  end
  object skills_attributes: TClientDataSet
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
    Top = 436
    Data = {
      3D0000009619E0BD0100000018000000030000000000030000003D00066D6173
      7465720400010000000000017804000100000000000179040001000000000000
      00}
    object IntegerField10: TIntegerField
      FieldName = 'master'
    end
    object IntegerField12: TIntegerField
      FieldName = 'x'
    end
    object IntegerField16: TIntegerField
      FieldName = 'y'
    end
  end
  object animations_timingSec: TClientDataSet
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
        Name = 'name'
        DataType = ftString
        Size = 32
      end
      item
        Name = 'modified'
        DataType = ftBoolean
      end
      item
        Name = 'sound.id'
        DataType = ftInteger
      end
      item
        Name = 'sound.name'
        DataType = ftString
        Size = 32
      end
      item
        Name = 'sound.modified'
        DataType = ftBoolean
      end
      item
        Name = 'sound.fadeIn'
        DataType = ftInteger
      end
      item
        Name = 'sound.tempo'
        DataType = ftInteger
      end
      item
        Name = 'sound.volume'
        DataType = ftByte
      end
      item
        Name = 'sound.leftBalance'
        DataType = ftByte
      end
      item
        Name = 'sound.rightBalance'
        DataType = ftByte
      end
      item
        Name = 'flashWhere'
        DataType = ftByte
      end
      item
        Name = 'color'
        DataType = ftInteger
      end>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 384
    Top = 80
    Data = {
      390100009619E0BD01000000180000000E0000000000030000003901066D6173
      74657204000100000000000269640400010000000000046E616D650100490000
      000100055749445448020002002000086D6F6469666965640200030000000000
      08736F756E642E696404000100000000000A736F756E642E6E616D6501004900
      000001000557494454480200020020000E736F756E642E6D6F64696669656402
      000300000000000C736F756E642E66616465496E04000100000000000B736F75
      6E642E74656D706F04000100000000000C736F756E642E766F6C756D65010002
      000000000011736F756E642E6C65667442616C616E6365010002000000000012
      736F756E642E726967687442616C616E636501000200000000000A666C617368
      5768657265010002000000000005636F6C6F7204000100000000000000}
    object animations_timingSecIntegerField: TIntegerField
      FieldName = 'master'
    end
    object animations_timingSecid: TIntegerField
      FieldName = 'id'
    end
    object animations_timingSecname: TStringField
      FieldName = 'name'
      Size = 32
    end
    object animations_timingSecmodified: TBooleanField
      FieldName = 'modified'
    end
    object animations_timingSecsoundid: TIntegerField
      FieldName = 'sound.id'
    end
    object animations_timingSecsoundname: TStringField
      FieldName = 'sound.name'
      Size = 32
    end
    object animations_timingSecsoundmodified: TBooleanField
      FieldName = 'sound.modified'
    end
    object animations_timingSecsoundfadeIn: TIntegerField
      FieldName = 'sound.fadeIn'
    end
    object animations_timingSecsoundtempo: TIntegerField
      FieldName = 'sound.tempo'
    end
    object animations_timingSecsoundvolume: TByteField
      FieldName = 'sound.volume'
    end
    object animations_timingSecsoundleftBalance: TByteField
      FieldName = 'sound.leftBalance'
    end
    object animations_timingSecsoundrightBalance: TByteField
      FieldName = 'sound.rightBalance'
    end
    object animations_timingSecFlashWhere: TByteField
      FieldName = 'flashWhere'
    end
    object animations_timingSecColor: TIntegerField
      FieldName = 'color'
    end
  end
  object animations_frameSec: TClientDataSet
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
        Name = 'name'
        DataType = ftString
        Size = 32
      end
      item
        Name = 'modified'
        DataType = ftBoolean
      end
      item
        Name = 'sound.id'
        DataType = ftInteger
      end
      item
        Name = 'sound.name'
        DataType = ftString
        Size = 32
      end
      item
        Name = 'sound.modified'
        DataType = ftBoolean
      end
      item
        Name = 'sound.fadeIn'
        DataType = ftInteger
      end
      item
        Name = 'sound.tempo'
        DataType = ftInteger
      end
      item
        Name = 'sound.volume'
        DataType = ftByte
      end
      item
        Name = 'sound.leftBalance'
        DataType = ftByte
      end
      item
        Name = 'sound.rightBalance'
        DataType = ftByte
      end
      item
        Name = 'flashWhere'
        DataType = ftByte
      end
      item
        Name = 'color'
        DataType = ftInteger
      end>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 384
    Top = 136
    Data = {
      390100009619E0BD01000000180000000E0000000000030000003901066D6173
      74657204000100000000000269640400010000000000046E616D650100490000
      000100055749445448020002002000086D6F6469666965640200030000000000
      08736F756E642E696404000100000000000A736F756E642E6E616D6501004900
      000001000557494454480200020020000E736F756E642E6D6F64696669656402
      000300000000000C736F756E642E66616465496E04000100000000000B736F75
      6E642E74656D706F04000100000000000C736F756E642E766F6C756D65010002
      000000000011736F756E642E6C65667442616C616E6365010002000000000012
      736F756E642E726967687442616C616E636501000200000000000A666C617368
      5768657265010002000000000005636F6C6F7204000100000000000000}
    object IntegerField17: TIntegerField
      FieldName = 'master'
    end
    object IntegerField18: TIntegerField
      FieldName = 'id'
    end
    object StringField9: TStringField
      FieldName = 'name'
      Size = 32
    end
    object BooleanField2: TBooleanField
      FieldName = 'modified'
    end
    object animations_frameSecframe: TWordField
      FieldKind = fkCalculated
      FieldName = 'frame'
      Calculated = True
    end
    object animations_frameSecpositionx: TIntegerField
      FieldKind = fkCalculated
      FieldName = 'position.x'
      Calculated = True
    end
    object animations_frameSecpositiony: TIntegerField
      FieldKind = fkCalculated
      FieldName = 'position.y'
      Calculated = True
    end
    object animations_frameSeczoomx: TIntegerField
      FieldKind = fkCalculated
      FieldName = 'zoom.x'
      Calculated = True
    end
    object animations_frameSeczoomy: TIntegerField
      FieldKind = fkCalculated
      FieldName = 'zoom.y'
      Calculated = True
    end
    object animations_frameSeccolor: TIntegerField
      FieldKind = fkCalculated
      FieldName = 'color'
      Calculated = True
    end
    object animations_frameSecsaturation: TByteField
      FieldKind = fkCalculated
      FieldName = 'saturation'
      Calculated = True
    end
  end
  object tilesets: TClientDataSet
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
        Name = 'HiSpeed'
        DataType = ftBoolean
      end>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 520
    Top = 8
    Data = {
      5F0000009619E0BD0100000018000000040000000000030000005F0002696404
      00010000000000046E616D650100490000000100055749445448020002002000
      086D6F6469666965640200030000000000074869537065656402000300000000
      000000}
    object IntegerField19: TIntegerField
      FieldName = 'id'
    end
    object StringField10: TStringField
      FieldName = 'name'
      Size = 32
    end
    object BooleanField3: TBooleanField
      FieldName = 'modified'
    end
    object tilesetsHiSpeed: TBooleanField
      FieldName = 'HiSpeed'
    end
  end
  object scriptRecords: TClientDataSet
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
    BeforeInsert = scriptRecordsBeforeInsert
    Left = 752
    Top = 64
    Data = {
      750000009619E0BD0100000018000000040000000000030000007500046E616D
      6501004900000001000557494454480200020020000A64657369676E4E616D65
      0100490000000100055749445448020002002000076164647265737304000100
      000000000A626173654D6574686F6404000100000000000000}
    object StringField6: TStringField
      FieldName = 'name'
      Size = 32
    end
    object StringField7: TStringField
      FieldName = 'designName'
      Size = 32
    end
    object IntegerField5: TIntegerField
      FieldName = 'address'
    end
    object IntegerField8: TIntegerField
      FieldName = 'baseMethod'
    end
    object IntegerField9: TIntegerField
      FieldKind = fkLookup
      FieldName = 'start'
      LookupDataSet = scriptRange
      LookupKeyFields = 'name'
      LookupResultField = 'start'
      KeyFields = 'name'
      Lookup = True
    end
    object IntegerField11: TIntegerField
      FieldKind = fkLookup
      FieldName = 'end'
      LookupDataSet = scriptRange
      LookupKeyFields = 'name'
      LookupResultField = 'end'
      KeyFields = 'name'
      Lookup = True
    end
    object StringField8: TStringField
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
