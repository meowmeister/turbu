unit dm_database;
{*****************************************************************************
* The contents of this file are used with permission, subject to
* the Mozilla Public License Version 1.1 (the "License"); you may
* not use this file except in compliance with the License. You may
* obtain a copy of the License at
* http://www.mozilla.org/MPL/MPL-1.1.html
*
* Software distributed under the License is distributed on an
* "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
* implied. See the License for the specific language governing
* rights and limitations under the License.
*
*****************************************************************************
*
* This file was created by Mason Wheeler.  He can be reached for support at
* www.turbu-rpg.com.
*****************************************************************************}

interface

uses
   SysUtils, Classes, DBClient, DB, Generics.Collections;

type
  TDatasetList = TList<TClientDataSet>;

   TdmDatabase = class(TDataModule)
      charClasses: TClientDataset;
      charClasses_skillset: TClientDataset;
      charClasses_skillsetmodified: TBooleanField;
      charClasses_skillsetskill: TIntegerField;
      charClasses_skillsetstyle: TIntegerField;
      charClasses_Resist: TClientDataSet;
      charClasses_Resistmaster: TIntegerField;
      charClasses_Resistx: TIntegerField;
      charClasses_Resisty: TIntegerField;
      charClasses_Condition: TClientDataSet;
      IntegerField1: TIntegerField;
      IntegerField2: TIntegerField;
      IntegerField3: TIntegerField;
      animations: TClientDataSet;
      animationsid: TIntegerField;
      animationsmodified: TBooleanField;
      animationshitsAll: TBooleanField;
      animationsyTarget: TIntegerField;
      animationsname: TStringField;
      items_script: TClientDataSet;
      items_armor: TClientDataSet;
      equipment_conditions: TClientDataSet;
      IntegerField10: TIntegerField;
      equipment_attributes: TClientDataSet;
      IntegerField13: TIntegerField;
      IntegerField14: TIntegerField;
      IntegerField15: TIntegerField;
      items_weapon: TClientDataSet;
      items_medicine: TClientDataSet;
      items_book: TClientDataSet;
      items_skill: TClientDataSet;
      items_upgrade: TClientDataSet;
      items_variable: TClientDataSet;
      shields: TClientDataSet;
      armors: TClientDataSet;
      helmets: TClientDataSet;
      accessories: TClientDataSet;
      items_weaponid: TIntegerField;
      items_weaponname: TStringField;
      items_weaponmodified: TBooleanField;
      items_weapondesc: TStringField;
      items_weaponcost: TIntegerField;
      items_weapontag: TArrayField;
      items_weapontag0: TIntegerField;
      items_weapontag1: TIntegerField;
      items_weapontag2: TIntegerField;
      items_weapontag3: TIntegerField;
      items_armorid: TIntegerField;
      items_armorname: TStringField;
      items_armormodified: TBooleanField;
      items_armordesc: TStringField;
      items_armorcost: TIntegerField;
      items_armortag: TArrayField;
      items_armortag0: TIntegerField;
      items_armortag1: TIntegerField;
      items_armortag2: TIntegerField;
      items_armortag3: TIntegerField;
      items_armorusesLeft: TIntegerField;
      items_armorusableWhere: TIntegerField;
      items_armorstat: TArrayField;
      items_armorstat0: TIntegerField;
      items_armorstat1: TIntegerField;
      items_armorstat2: TIntegerField;
      items_armorstat3: TIntegerField;
      items_armorstat4: TIntegerField;
      items_armorstat5: TIntegerField;
      items_armorevasion: TBooleanField;
      items_armortoHit: TIntegerField;
      items_armorcritChance: TIntegerField;
      items_armorcritPrevent: TIntegerField;
      items_armorpreemptive: TIntegerField;
      items_armormpReduction: TIntegerField;
      items_armornoTerrainDamage: TBooleanField;
      items_armorusable: TBooleanField;
      items_armorcursed: TBooleanField;
      items_armorslot: TIntegerField;
      items_weaponusesLeft: TIntegerField;
      items_weaponusableWhere: TIntegerField;
      items_weaponstat: TArrayField;
      items_weaponstat0: TIntegerField;
      items_weaponstat1: TIntegerField;
      items_weaponstat2: TIntegerField;
      items_weaponstat3: TIntegerField;
      items_weaponstat4: TIntegerField;
      items_weaponstat5: TIntegerField;
      items_weaponevasion: TBooleanField;
      items_weapontoHit: TIntegerField;
      items_weaponcritChance: TIntegerField;
      items_weaponcritPrevent: TIntegerField;
      items_weaponpreemptive: TIntegerField;
      items_weaponmpReduction: TIntegerField;
      items_weaponnoTerrainDamage: TBooleanField;
      items_weaponusable: TBooleanField;
      items_weaponcursed: TBooleanField;
      items_weapontwoHanded: TBooleanField;
      items_weaponattackTwice: TBooleanField;
      items_weaponareaHit: TBooleanField;
      items_weaponbattleAnim: TIntegerField;
      items_weaponmpCost: TIntegerField;
      items_weaponconditionChance: TIntegerField;
      items_junk: TClientDataSet;
      equipment_conditionsid: TIntegerField;
      equipment_conditionsvalue: TBooleanField;
      items_junkid: TIntegerField;
      items_junkname: TStringField;
      items_junkmodified: TBooleanField;
      items_junkdesc: TStringField;
      items_junkcost: TIntegerField;
      items_junktag: TArrayField;
      items_junktag0: TIntegerField;
      items_junktag1: TIntegerField;
      items_junktag2: TIntegerField;
      items_junktag3: TIntegerField;
      items_scriptid: TIntegerField;
      items_scriptname: TStringField;
      items_scriptmodified: TBooleanField;
      items_scriptdesc: TStringField;
      items_scriptcost: TIntegerField;
      items_scripttag: TArrayField;
      items_scripttag0: TIntegerField;
      items_scripttag1: TIntegerField;
      items_scripttag2: TIntegerField;
      items_scripttag3: TIntegerField;
      items_medicineid: TIntegerField;
      items_medicinename: TStringField;
      items_medicinemodified: TBooleanField;
      items_medicinedesc: TStringField;
      items_medicinecost: TIntegerField;
      items_medicinetag: TArrayField;
      items_medicinetag0: TIntegerField;
      items_medicinetag1: TIntegerField;
      items_medicinetag2: TIntegerField;
      items_medicinetag3: TIntegerField;
      items_medicineusesLeft: TIntegerField;
      items_medicineusableWhere: TIntegerField;
      items_medicinestat: TArrayField;
      items_medicinestat0: TIntegerField;
      items_medicinestat1: TIntegerField;
      items_medicinestat2: TIntegerField;
      items_medicinestat3: TIntegerField;
      items_medicinestat4: TIntegerField;
      items_medicinestat5: TIntegerField;
      items_medicineareaMedicine: TBooleanField;
      items_medicinehpPercent: TIntegerField;
      items_medicinempPercent: TIntegerField;
      items_medicinedeadOnly: TBooleanField;
      items_scriptusesLeft: TIntegerField;
      items_scriptusableWhere: TIntegerField;
      items_scriptstat: TArrayField;
      items_scriptstat0: TIntegerField;
      items_scriptstat1: TIntegerField;
      items_scriptstat2: TIntegerField;
      items_scriptstat3: TIntegerField;
      items_scriptstat4: TIntegerField;
      items_scriptstat5: TIntegerField;
      items_scriptevent: TStringField;
      items_scriptscript: TMemoField;
      items_bookid: TIntegerField;
      items_bookname: TStringField;
      items_bookmodified: TBooleanField;
      items_bookdesc: TStringField;
      items_bookcost: TIntegerField;
      items_booktag: TArrayField;
      items_booktag0: TIntegerField;
      items_booktag1: TIntegerField;
      items_booktag2: TIntegerField;
      items_booktag3: TIntegerField;
      items_bookusesLeft: TIntegerField;
      items_bookusableWhere: TIntegerField;
      items_bookstat: TArrayField;
      items_bookstat0: TIntegerField;
      items_bookstat1: TIntegerField;
      items_bookstat2: TIntegerField;
      items_bookstat3: TIntegerField;
      items_bookstat4: TIntegerField;
      items_bookstat5: TIntegerField;
      items_bookskill: TIntegerField;
      items_skillid: TIntegerField;
      items_skillname: TStringField;
      items_skillmodified: TBooleanField;
      items_skilldesc: TStringField;
      items_skillcost: TIntegerField;
      items_skilltag: TArrayField;
      items_skilltag0: TIntegerField;
      items_skilltag1: TIntegerField;
      items_skilltag2: TIntegerField;
      items_skilltag3: TIntegerField;
      items_skillusesLeft: TIntegerField;
      items_skillusableWhere: TIntegerField;
      items_skillstat: TArrayField;
      items_skillstat0: TIntegerField;
      items_skillstat1: TIntegerField;
      items_skillstat2: TIntegerField;
      items_skillstat3: TIntegerField;
      items_skillstat4: TIntegerField;
      items_skillstat5: TIntegerField;
      items_skillskill: TIntegerField;
      items_skillcustomSkillMessage: TBooleanField;
      items_upgradeid: TIntegerField;
      items_upgradename: TStringField;
      items_upgrademodified: TBooleanField;
      items_upgradedesc: TStringField;
      items_upgradecost: TIntegerField;
      items_upgradetag: TArrayField;
      items_upgradetag0: TIntegerField;
      items_upgradetag1: TIntegerField;
      items_upgradetag2: TIntegerField;
      items_upgradetag3: TIntegerField;
      items_upgradeusesLeft: TIntegerField;
      items_upgradeusableWhere: TIntegerField;
      items_upgradestat: TArrayField;
      items_upgradestat0: TIntegerField;
      items_upgradestat1: TIntegerField;
      items_upgradestat2: TIntegerField;
      items_upgradestat3: TIntegerField;
      items_upgradestat4: TIntegerField;
      items_upgradestat5: TIntegerField;
      items_variableid: TIntegerField;
      items_variablename: TStringField;
      items_variablemodified: TBooleanField;
      items_variabledesc: TStringField;
      items_variablecost: TIntegerField;
      items_variabletag: TArrayField;
      items_variabletag0: TIntegerField;
      items_variabletag1: TIntegerField;
      items_variabletag2: TIntegerField;
      items_variabletag3: TIntegerField;
      items_variableusesLeft: TIntegerField;
      items_variableusableWhere: TIntegerField;
      items_variablestat: TArrayField;
      items_variablestat0: TIntegerField;
      items_variablestat1: TIntegerField;
      items_variablestat2: TIntegerField;
      items_variablestat3: TIntegerField;
      items_variablestat4: TIntegerField;
      items_variablestat5: TIntegerField;
      items_variablewhich: TIntegerField;
      items_variablemagnitude: TIntegerField;
      items_variablestype: TIntegerField;
      items_variableoperation: TIntegerField;
      shieldsid: TIntegerField;
      shieldsname: TStringField;
      shieldsmodified: TBooleanField;
      armorsid: TIntegerField;
      armorsname: TStringField;
      armorsmodified: TBooleanField;
      shieldsslot: TIntegerField;
      armorsslot: TIntegerField;
      helmetsid: TIntegerField;
      helmetsname: TStringField;
      helmetsmodified: TBooleanField;
      helmetsslot: TIntegerField;
      accessoriesid: TIntegerField;
      accessoriesname: TStringField;
      accessoriesmodified: TBooleanField;
      accessoriesslot: TIntegerField;
      items_weaponusableByChar: TBytesField;
      items_weaponusableByClass: TBytesField;
      items_armorusableByClass: TBytesField;
      items_armorusableByHero: TBytesField;
      items_medicineusableByClass: TBytesField;
      items_medicineusableByHero: TBytesField;
      items_bookusableByClass: TBytesField;
      items_bookusableByHero: TBytesField;
      items_skillusableByHero: TBytesField;
      items_skillusableByClass: TBytesField;
      items_upgradeusableByClass: TBytesField;
      items_upgradeusableByHero: TBytesField;
      items_variableusableByClass: TBytesField;
      items_variableusableByHero: TBytesField;
      items_scriptusableByHero: TBytesField;
      items_scriptusableByClass: TBytesField;
      weapons: TClientDataSet;
      IntegerField4: TIntegerField;
      StringField1: TStringField;
      BooleanField1: TBooleanField;
      BytesField1: TBytesField;
      BytesField2: TBytesField;
      BooleanField6: TBooleanField;
      commands: TClientDataSet;
      commandsid: TIntegerField;
      commandsname: TStringField;
      commandsmodified: TBooleanField;
      commandsstyle: TIntegerField;
      commandsvalue: TIntegerField;
      charClasses_skillsetname: TStringField;
      charClasses_skillsetid: TStringField;
      skills: TClientDataSet;
      skillsid: TIntegerField;
      skillsname: TStringField;
      skillsmodified: TBooleanField;
      skillscost: TIntegerField;
      skillscostPercent: TBooleanField;
      skillsdesc: TStringField;
      skillsmessages: TADTField;
      skillsmessagesuseString: TStringField;
      skillsmessagesuseString2: TStringField;
      skillsmessagesfailureMessage: TIntegerField;
      skillsusableWhere: TIntegerField;
      skillstag: TArrayField;
      skillstag0: TIntegerField;
      skillstag1: TIntegerField;
      skillstag2: TIntegerField;
      skillstag3: TIntegerField;
      items: TClientDataSet;
      IntegerField30: TIntegerField;
      StringField3: TStringField;
      BooleanField9: TBooleanField;
      StringField4: TStringField;
      IntegerField31: TIntegerField;
      ArrayField3: TArrayField;
      IntegerField32: TIntegerField;
      IntegerField33: TIntegerField;
      IntegerField34: TIntegerField;
      IntegerField35: TIntegerField;
      IntegerField36: TIntegerField;
      IntegerField37: TIntegerField;
      BytesField3: TBytesField;
      BytesField4: TBytesField;
      ArrayField4: TArrayField;
      IntegerField38: TIntegerField;
      IntegerField39: TIntegerField;
      IntegerField40: TIntegerField;
      IntegerField41: TIntegerField;
      IntegerField42: TIntegerField;
      IntegerField43: TIntegerField;
      BooleanField10: TBooleanField;
      IntegerField44: TIntegerField;
      IntegerField45: TIntegerField;
      IntegerField46: TIntegerField;
      IntegerField47: TIntegerField;
      IntegerField48: TIntegerField;
      BooleanField11: TBooleanField;
      BooleanField12: TBooleanField;
      BooleanField13: TBooleanField;
      BooleanField14: TBooleanField;
      BooleanField15: TBooleanField;
      BooleanField16: TBooleanField;
      IntegerField49: TIntegerField;
      IntegerField50: TIntegerField;
      IntegerField51: TIntegerField;
      itemsIntegerField: TIntegerField;
      itemsareaMedicine: TBooleanField;
      itemshpPercent: TIntegerField;
      itemsmpPercent: TIntegerField;
      itemsdeadOnly: TBooleanField;
      itemsskill: TIntegerField;
      itemscustomSkillMessage: TBooleanField;
      itemswhich: TIntegerField;
      itemsmagnitude: TIntegerField;
      itemsstype: TIntegerField;
      itemsoperation: TIntegerField;
      itemsevent: TStringField;
      itemsscript: TMemoField;
      itemsitemType: TIntegerField;
      offhands: TClientDataSet;
      IntegerField52: TIntegerField;
      StringField5: TStringField;
      BooleanField17: TBooleanField;
      BytesField5: TBytesField;
      BytesField6: TBytesField;
      BooleanField22: TBooleanField;
      offhandsIntegerField: TIntegerField;
      dsCharClasses: TDataSource;
      attributes: TClientDataSet;
      attributesid: TIntegerField;
      attributesname: TStringField;
      attributesmodified: TBooleanField;
      attributesrequiredForSkills: TBooleanField;
      charClasses_Resistname: TStringField;
      conditions: TClientDataSet;
      conditionsId: TIntegerField;
      conditionsName: TStringField;
      conditionsModified: TBooleanField;
      conditionsOutOfBattle: TBooleanField;
      conditionscolor: TWordField;
      conditionspriority: TWordField;
      conditionsattackLimit: TWordField;
      conditionstag: TArrayField;
      conditionstag0: TIntegerField;
      conditionstag1: TIntegerField;
      conditionstag2: TIntegerField;
      conditionstag3: TIntegerField;
      charClasses_Conditionname: TStringField;
      items_weaponIntegerField: TIntegerField;
      items_armoritemType: TIntegerField;
      weaponsIntegerField: TIntegerField;
      shieldsusableByHero: TBytesField;
      shieldsusableByClass: TBytesField;
      armorsusableByHero: TBytesField;
      armorsusableByClass: TBytesField;
      helmetsusableByHero: TBytesField;
      helmetsusableByClass: TBytesField;
      accessoriesusableByHero: TBytesField;
      accessoriesusableByClass: TBytesField;
      shieldsitemType: TIntegerField;
      offhandsitemType: TIntegerField;
      armorsitemType: TIntegerField;
      helmetsitemType: TIntegerField;
      accessoriesitemType: TIntegerField;
      attributesStandard: TIntegerField;
      skillGainRecords: TClientDataSet;
      skillGainRecordsName: TStringField;
      skillGainRecordsDesignName: TStringField;
      skillGainRecordsAddress: TIntegerField;
      skillGainRecordsStyle: TIntegerField;
      skillGainRecordsArrayArgs: TBooleanField;
      skillGainRecordsDisplayMethod: TIntegerField;
      skillGainRecordsBaseMethod: TIntegerField;
      scriptRange: TClientDataSet;
      scriptRangeId: TIntegerField;
      IntegerField6: TIntegerField;
      IntegerField7: TIntegerField;
      StringField2: TStringField;
      scriptRangemodified: TBooleanField;
      _TScriptRecordStart: TIntegerField;
      _TScriptRecordEnd: TIntegerField;
      skillGainRecordsDisplayName: TStringField;
      scriptRangeUnit: TStringField;
      expCalcRecords: TClientDataSet;
      expCalcRecordsname: TStringField;
      expCalcRecordsdesignName: TStringField;
      expCalcRecordsaddress: TIntegerField;
      expCalcRecordsBaseMethod: TIntegerField;
      expCalcRecordsStart: TIntegerField;
      expCalcRecordsEnd: TIntegerField;
      expCalcRecordsunit: TStringField;
      skillGainRecordsunit: TStringField;
      charClasses_skillsetnum: TArrayField;
      charClasses_skillsetnum0: TIntegerField;
      charClasses_skillsetnum1: TIntegerField;
      charClasses_skillsetnum2: TIntegerField;
      charClasses_skillsetnum3: TIntegerField;
      charClasses_skillsetAlgName: TStringField;
      charClasses_Events: TClientDataSet;
      StringField6: TStringField;
      StringField7: TStringField;
      IntegerField5: TIntegerField;
      IntegerField8: TIntegerField;
      IntegerField9: TIntegerField;
      IntegerField11: TIntegerField;
      StringField8: TStringField;
    charClassesid: TIntegerField;
    charClassesname: TStringField;
    charClassesmodified: TBooleanField;
    charClassesmapSprite: TIntegerField;
    charClassesbattleSprite: TIntegerField;
    charClassesportrait: TIntegerField;
    charClassescommand0: TIntegerField;
    charClassescommand1: TIntegerField;
    charClassescommand2: TIntegerField;
    charClassescommand3: TIntegerField;
    charClassescommand4: TIntegerField;
    charClassescommand5: TIntegerField;
    charClassescommand6: TIntegerField;
    charClassesstatblock: TArrayField;
    charClassesstatblock0: TLargeintField;
    charClassesstatblock1: TLargeintField;
    charClassesstatblock2: TLargeintField;
    charClassesstatblock3: TLargeintField;
    charClassesstatblock4: TLargeintField;
    charClassesstatblock5: TLargeintField;
    charClassesSp: TLargeintField;
    charClassesAttack: TLargeintField;
    charClassesDefense: TLargeintField;
    charClassesMind: TLargeintField;
    charClassesSpeed: TLargeintField;
    charClassesexpFunc: TStringField;
    charClassesexpVars0: TIntegerField;
    charClassesexpVars1: TIntegerField;
    charClassesexpVars2: TIntegerField;
    charClassesexpVars3: TIntegerField;
    charClassesdualWield: TIntegerField;
    charClassesstaticEq: TBooleanField;
    charClassesstrongDef: TBooleanField;
    charClassesunarmedAnim: TIntegerField;
    charClassesanimName: TStringField;
    charClassesequip0: TIntegerField;
    charClassesequip1: TIntegerField;
    charClassesequip2: TIntegerField;
    charClassesequip3: TIntegerField;
    charClassesequip4: TIntegerField;
    charClassesweaponName: TStringField;
    charClassesweapon2Name: TStringField;
    charClassesoffhandName: TStringField;
    charClassesshieldName: TStringField;
    charClassesarmorName: TStringField;
    charClasseshelmetName: TStringField;
    charClassesaccessoryName: TStringField;
    charClassesexpFuncDesignName: TStringField;
    charClasses_skillsetmethodName: TStringField;
    charClasses_skillsetarrayArgs: TBooleanField;
    charClasses_skillsetmethodStyle: TIntegerField;
    charClasses_skillsetaddress: TIntegerField;
    charClasses_skillsetdisplayAddress: TIntegerField;
      procedure DataModuleCreate(Sender: TObject);
      procedure restoreClone(DataSet: TDataSet);
      procedure charClasses_skillsetCalcFields(DataSet: TDataSet);
      procedure DataModuleDestroy(Sender: TObject);
      procedure classFilter(DataSet: TDataSet; var Accept: Boolean);
   private
      { Private declarations }

      FDatasetList: TDatasetList;
      FViewList: TDatasetList;
      function usableByFilter(field: TBytesField; master: TDataset): boolean;
   public
      { Public declarations }
      procedure beginUpload;
      procedure endUpload;

      property datasets: TDatasetList read FDatasetList write FDatasetList;
      property views: TDatasetList read FViewList write FViewList;
   end;

var
   dmDatabase: TdmDatabase;

implementation

uses
   turbu_skills, turbu_defs, turbu_classes;

{$R *.dfm}

procedure TdmDatabase.DataModuleCreate(Sender: TObject);
var
   iterator: TComponent;
   clone: TClientDataset;
begin
   FDatasetList := TDatasetList.Create;
   for iterator in self do
      if iterator is TClientDataset then
         FDatasetList.Add(TClientDataset(iterator));

   FViewList := TDatasetList.Create;
//   FViewList.AddRange([shields, armors, helmets, accessories, weapons, offhands]);
   FViewList.Add(shields);
   FViewList.Add(armors);
   FViewList.Add(helmets);
   FViewList.Add(accessories);
   FViewList.Add(weapons);
   FViewList.Add(offhands);

   shields.CloneCursor(items_armor, false, true);
   armors.CloneCursor(items_armor, false, true);
   helmets.CloneCursor(items_armor, false, true);
   accessories.CloneCursor(items_armor, false, true);
   weapons.CloneCursor(items_weapon, false, true);
   offhands.CloneCursor(items, false, true);
   for clone in FDatasetList do
   begin
      if Pos('items_', clone.Name) = 1 then
         clone.CloneCursor(items, false);
      clone.tag := integer(clone.CloneSource);
   end;
end;

procedure TdmDatabase.DataModuleDestroy(Sender: TObject);
begin
   FDatasetList.Free;
   FViewList.Free;
end;

procedure TdmDatabase.beginUpload;
var
   ds: TClientDataset;
begin
   for ds in datasets do
   begin
      ds.AutoCalcFields := false;
      ds.LogChanges := false;
   end;
end;

procedure TdmDatabase.endUpload;
var
   ds: TClientDataset;
begin
   for ds in dmDatabase.datasets do
   begin
      ds.AutoCalcFields := true;
      ds.LogChanges := true;
   end;
end;

procedure TdmDatabase.charClasses_skillsetCalcFields(DataSet: TDataSet);
var
   func: TSkillGainDisplayFunc;
   result: string;
   args: T4IntArray;
   i: integer;
begin
   if dataset.FieldByName('modified').IsNull then
      Exit;

   func := TSkillGainDisplayFunc(dataset.FieldByName('method.displayAddress').asPSMethod);
   if not assigned(func) then
      result := '?'
   else
   begin
      for I := 0 to 3 do
         args[i + 1] := dataset.FieldByName('num[' + intToStr(i) + ']').AsInteger;
      if dataset.FieldByName('method.arrayArgs').AsBoolean then
         result := TSkillGainDisplayArrayFunc(func)(args)
      else
         result := func(args[1], args[2], args[3], args[4])
      //end if
   end;
   dataset.FieldByName('id').AsString := result;
end;

procedure TdmDatabase.classFilter(DataSet: TDataSet; var Accept: Boolean);
begin
   accept := Self.usableByFilter(DataSet.FieldByName('usableByClass') as TBytesField, charClasses);
end;

procedure TdmDatabase.restoreClone(DataSet: TDataSet);
begin
   if dataset.tag <> 0 then
   begin
      DataSet.AfterOpen := nil;
      (dataSet as TClientDataSet).CloneCursor(TClientDataset(dataset.Tag), false, true);
      DataSet.AfterOpen := self.restoreClone;
   end;
end;

function TdmDatabase.usableByFilter(field: TBytesField; master: TDataset): boolean;
begin
   result := master.FieldByName('id').AsInteger in field.asSet;
end;

end.
