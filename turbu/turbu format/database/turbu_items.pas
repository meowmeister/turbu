unit turbu_items;
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
   classes, DB,
   turbu_constants, turbu_defs, turbu_classes;

type
   TItemType = (it_junk, it_weapon, it_armor, it_medicine, it_upgrade, it_book,
                it_skill, it_variable, it_script);

   TItemTemplate = class abstract(TRpgDatafile)
   private
      FDesc: string;
      FPrice: integer;
      FTag: T4IntArray;
   protected
      class function keyChar: ansiChar; override;
   public
      constructor Load(savefile: TStream); virtual;
      procedure save(savefile: TStream); override;
      procedure upload(db: TDataSet); override;

      property desc: string read FDesc write FDesc;
      property cost: integer read FPrice write FPrice;
      property tag: T4IntArray read FTag write FTag;
   end;

   TJunkTemplate = class(TItemTemplate);

   TUsableItemTemplate = class abstract(TItemTemplate)
   private
      FUses: integer;
      FUsable: TUsableWhere;
      FUsedByHero: TByteSet;
      FUsedByClass: TByteSet;
      FStat: array[1..STAT_COUNT] of integer;

      function getStat(i: byte): integer;
      procedure setStat(i: byte; const Value: integer);
   public
      constructor Load(savefile: TStream); override;
      procedure save(savefile: TStream); override;
      procedure upload(db: TDataSet); override;

      property stat[i: byte]: integer read getStat write setStat;
      property usesLeft: integer read FUses write FUses;
      property usableWhere: TUsableWhere read FUsable write FUsable;
      property usableByHero: TByteSet read FUsedByHero write FUsedByHero;
      property usableByClass: TByteSet read FUsedByClass write FUsedByClass;
   end;

   TEquipmentTemplate = class abstract(TUsableItemTemplate)
   private
      FEvasion: boolean;
      FToHit: integer;
      FCritChance: integer;
      FPreventCrits: integer;
      FPreemptive: integer;
      FMpReduction: integer;
      FNoTerrainDamage: boolean;
      FUsable: boolean;
      FConditions: TByteSet;
      FAttributes: TPointArray;
      FCursed: boolean;
   public
      constructor Load(savefile: TStream); override;
      procedure save(savefile: TStream); override;
      procedure upload(db: TDataSet); override;

      property evasion: boolean read FEvasion write FEvasion;
      property toHit: integer read FToHit write FToHit;
      property critChance: integer read FCritChance write FCritChance;
      property critPrevent: integer read FPreventCrits write FPreventCrits;
      property preemptive: integer read FPreemptive write FPreemptive;
      property mpReduction: integer read FMpReduction write FMpReduction;
      property noTerrainDamage: boolean read FNoTerrainDamage write FNoTerrainDamage;
      property cursed: boolean read FCursed write FCursed;
      property usable: boolean read FUsable write FUsable; //can be used as an item
      property condition: TByteSet read FConditions write FConditions;
      property attribute: TPointArray read FAttributes write FAttributes;
   end;

   TWeaponTemplate = class(TEquipmentTemplate)
   private
      FTwoHanded: boolean;
      FAttackTwice: boolean;
      FAreaHit: boolean;
      FBattleAnim: word;
      FMPCost: word;
      FConditionInflictChance: byte;
   public
      constructor Load(savefile: TStream); override;
      procedure save(savefile: TStream); override;
      procedure upload(db: TDataSet); override;

      property twoHanded: boolean read FTwoHanded write FTwoHanded;
      property attackTwice: boolean read FAttackTwice write FAttackTwice;
      property areaHit: boolean read FAreaHit write FAreaHit;
      property battleAnim: word read FBattleAnim write FBattleAnim;
      property mpCost: word read FMpCost write FMpCost;
      property conditionChance: byte read FConditionInflictChance write FConditionInflictChance;
   end;

   TArmorTemplate = class(TEquipmentTemplate)
   private
      FSlot: byte;
   public
      constructor Load(savefile: TStream); override;
      procedure save(savefile: TStream); override;
      procedure upload(db: TDataSet); override;

      property slot: byte read FSlot write FSlot;
   end;

   TMedicineTemplate = class(TUsableItemTemplate)
   private
      FAreaMedicine: boolean;
      FHPPercent: shortint;
      FMPPercent: shortint;
      FDeadHeroesOnly: boolean;
   public
      constructor Load(savefile: TStream); override;
      procedure save(savefile: TStream); override;

      property areaMedicine: boolean read FAreaMedicine write FAreaMedicine;
      property hpPercent: shortint read FHpPercent write FHPPercent;
      property mpPercent: shortint read FMpPercent write FMPPercent;
      property deadOnly: boolean read FDeadHeroesOnly write FDeadHeroesOnly;
   end;

   TSkillBookTemplate = class(TUsableItemTemplate)
   private
      FSkill: word;
   public
      constructor Load(savefile: TStream); override;
      procedure save(savefile: TStream); override;

      property skill: word read FSkill write FSkill;
   end;

   TSkillItemTemplate = class(TSkillBookTemplate)
   private
      FDisplaySkillMessage: boolean;
   public
      constructor Load(savefile: TStream); override;
      procedure save(savefile: TStream); override;

      property customSkillMessage: boolean read FDisplaySkillMessage write FDisplaySkillMessage;
   end;

   TStatItemTemplate = class(TUsableItemTemplate); //it_upgrade

   TVariableItemTemplate = class(TUsableItemTemplate)
   private
      FWhich: word;
      FMagnitude: smallint;
      FStyle: TVarSets;
      FOperation: TVarOps;
   public
      constructor Load(savefile: TStream); override;
      procedure save(savefile: TStream); override;

      property which: word read FWhich write FWhich;
      property magnitude: smallint read FMagnitude write FMagnitude;
      property style: TVarSets read FStyle write FStyle;
      property operation: TVarOps read FOperation write FOperation;
   end;

   TScriptItemTemplate = class(TUsableItemTemplate)
   private
      FEvent: string;
   public
      constructor Load(savefile: TStream); override;
      procedure save(savefile: TStream); override;

      property event: string read FEvent write FEvent;
   end;

implementation
uses
   types;

{ TItemTemplate }

class function TItemTemplate.keyChar: ansiChar;
begin
   result := 'i';
end;

constructor TItemTemplate.Load(savefile: TStream);
begin
   inherited Load(savefile);
   FDesc := savefile.readString;
   FPrice := savefile.readInt;
   savefile.readBuffer(FTag, sizeof(T4IntArray));
   lassert(savefile.readChar = 'I');
end;

procedure TItemTemplate.save(savefile: TStream);
begin
   inherited save(savefile);
   savefile.writeString(FDesc);
   savefile.writeInt(FPrice);
   savefile.WriteBuffer(FTag, sizeof(T4IntArray));
   savefile.writeChar('I');
end;

procedure TItemTemplate.upload(db: TDataSet);
var
   i: integer;
begin
   inherited upload(db);
   db.FieldByName('desc').asString := FDesc;
   db.FieldByName('cost').AsInteger := FPrice;
   for i := 1 to 4 do
      (db.FieldByName('tag') as TArrayField)[i - 1] := FTag[i];
end;

{ TUsableItemTemplate }

constructor TUsableItemTemplate.Load(savefile: TStream);
var
   dummy: byte;
begin
   inherited Load(savefile);
   FUses := savefile.readInt;
   savefile.readBuffer(FUsable, sizeof(TUsableWhere));
   dummy := savefile.readByte;
   if dummy > 0 then
      savefile.readBuffer(self.FUsedByHero, dummy);
   dummy := savefile.readByte;
   if dummy > 0 then
      savefile.readBuffer(self.FUsedByClass, dummy);
   lassert(savefile.readInt = STAT_COUNT);
   savefile.readBuffer(FStat[1], sizeof(integer) * STAT_COUNT);
   lassert(savefile.readChar = 'U');
end;

procedure TUsableItemTemplate.save(savefile: TStream);
var
   dummy: byte;
begin
   inherited save(savefile);
   savefile.writeInt(FUses);
   savefile.WriteBuffer(FUsable, sizeof(TUsableWhere));
   dummy := getSetLength(FUsedByHero);
   savefile.writeByte(dummy);
   if dummy > 0 then
      savefile.WriteBuffer(self.FUsedByHero, dummy);
   dummy := getSetLength(FUsedByClass);
   savefile.writeByte(dummy);
   if dummy > 0 then
      savefile.WriteBuffer(self.FUsedByClass, dummy);
   savefile.writeInt(STAT_COUNT);
   savefile.WriteBuffer(FStat[1], sizeof(integer) * STAT_COUNT);
   savefile.writeChar('U');
end;

function TUsableItemTemplate.getStat(i: byte): integer;
begin
   assert(i in [1..high(FStat)]);
   result := FStat[i];
end;

procedure TUsableItemTemplate.setStat(i: byte; const Value: integer);
begin
   assert(i in [1..high(FStat)]);
   FStat[i] := value;
end;

procedure TUsableItemTemplate.upload(db: TDataSet);
var
   i: integer;
begin
   inherited upload(db);
   for I := 1 to 6 do
      (db.FieldByName('stat') as TArrayField)[i - 1] := FStat[i];
   db.FieldByName('usesLeft').AsInteger := FUses;
   db.FieldByName('usableWhere').AsInteger := Ord(FUsable);
   (db.FieldByName('usableByHero') as TBytesField).asSet := FUsedByHero;
   (db.FieldByName('usableByClass') as TBytesField).asSet := FUsedByClass;
end;

{ TEquipmentTemplate }

constructor TEquipmentTemplate.Load(savefile: TStream);
var
   dummy: byte;
begin
   inherited Load(savefile);
   FEvasion := savefile.readBool;
   FToHit := savefile.readInt;
   FCritChance := savefile.readInt;
   FPreventCrits := savefile.readInt;
   FPreemptive := savefile.readInt;
   FMpReduction := savefile.readInt;
   FNoTerrainDamage := savefile.readBool;
   FUsable := savefile.readBool;
   dummy := savefile.readInt;
   if dummy > 0 then
      savefile.readBuffer(self.FConditions, dummy);
   setLength(FAttributes, savefile.readInt);
   if length(FAttributes) > 0 then
      savefile.readBuffer(self.FAttributes[0], length(FAttributes) * sizeof(TPoint));
   FCursed := savefile.readBool;
   lassert(savefile.readChar = 'E');
end;

procedure TEquipmentTemplate.save(savefile: TStream);
var
   dummy: byte;
begin
   inherited save(savefile);
   savefile.writeBool(FEvasion);
   savefile.writeInt(FToHit);
   savefile.writeInt(FCritChance);
   savefile.writeInt(FPreventCrits);
   savefile.writeInt(FPreemptive);
   savefile.writeInt(FMpReduction);
   savefile.writeBool(FNoTerrainDamage);
   savefile.writeBool(FUsable);
   dummy := getSetLength(FConditions);
   savefile.writeInt(dummy);
   if dummy > 0 then
      savefile.WriteBuffer(self.FConditions, dummy);
   savefile.writeInt(length(FAttributes));
   if length(FAttributes) > 0 then
      savefile.WriteBuffer(self.FAttributes[0], length(FAttributes) * sizeof(TPoint));
   savefile.writeBool(FCursed);
   savefile.writeChar('E');
end;

procedure TEquipmentTemplate.upload(db: TDataSet);
begin
   inherited upload(db);
   db.FieldByName('evasion').AsBoolean := evasion;
   db.FieldByName('toHit').AsInteger := toHit;
   db.FieldByName('critChance').AsInteger := critChance;
   db.FieldByName('critPrevent').AsInteger := critPrevent;
   db.FieldByName('preemptive').AsInteger := preemptive;
   db.FieldByName('mpReduction').AsInteger := mpReduction;
   db.FieldByName('noTerrainDamage').AsBoolean := noTerrainDamage;
   db.FieldByName('usable').AsBoolean := usable;
   db.FieldByName('cursed').AsBoolean := cursed;
end;

{ TWeaponTemplate }

constructor TWeaponTemplate.Load(savefile: TStream);
begin
   inherited Load(savefile);
   FTwoHanded := savefile.readBool;
   FAttackTwice := savefile.readBool;
   FAreaHit := savefile.readBool;
   FBattleAnim := savefile.readWord;
   FMPCost := savefile.readWord;
   FConditionInflictChance := savefile.readByte;
   lassert(savefile.readChar = 'W');
end;

procedure TWeaponTemplate.save(savefile: TStream);
begin
   inherited save(savefile);
   savefile.writeBool(FTwoHanded);
   savefile.writeBool(FAttackTwice);
   savefile.writeBool(FAreaHit);
   savefile.writeWord(FBattleAnim);
   savefile.writeWord(FMPCost);
   savefile.writeByte(FConditionInflictChance);
   savefile.writeChar('W');
end;

procedure TWeaponTemplate.upload(db: TDataSet);
begin
   inherited upload(db);
   db.FieldByName('twoHanded').AsBoolean := twoHanded;
   db.FieldByName('attackTwice').AsBoolean := attackTwice;
   db.FieldByName('areaHit').AsBoolean := areaHit;
   db.FieldByName('battleAnim').AsInteger := battleAnim;
   db.FieldByName('mpCost').AsInteger := mpCost;
   db.FieldByName('conditionChance').AsInteger := conditionChance;
end;

{ TArmorTemplate }

constructor TArmorTemplate.Load(savefile: TStream);
begin
   inherited Load(savefile);
   FSlot := savefile.readByte;
   lassert(savefile.readChar = 'A');
end;

procedure TArmorTemplate.save(savefile: TStream);
begin
   inherited save(savefile);
   savefile.writeByte(FSlot);
   savefile.writeChar('A');
end;

procedure TArmorTemplate.upload(db: TDataSet);
begin
   inherited upload(db);
   db.FieldByName('slot').AsInteger := slot;
end;

{ TMedicineTemplate }

constructor TMedicineTemplate.Load(savefile: TStream);
begin
   inherited load(savefile);
   FAreaMedicine := savefile.readBool;
   FHPPercent := savefile.readWord;
   FMPPercent := savefile.readWord;
   FDeadHeroesOnly := savefile.readBool;
   lassert(savefile.readChar = 'M');
end;

procedure TMedicineTemplate.save(savefile: TStream);
begin
   inherited save(savefile);
   savefile.writeBool(FAreaMedicine);
   savefile.writeWord(FHPPercent);
   savefile.writeWord(FMPPercent);
   savefile.writeBool(FDeadHeroesOnly);
   savefile.writeChar('M');
end;

{ TSkillBookTemplate }

constructor TSkillBookTemplate.Load(savefile: TStream);
begin
   inherited Load(savefile);
   FSkill := savefile.readWord;
   lassert(savefile.readChar = 'B');
end;

procedure TSkillBookTemplate.save(savefile: TStream);
begin
   inherited save(savefile);
   savefile.writeWord(FSkill);
   savefile.writeChar('B');
end;

{ TSkillItemTemplate }

constructor TSkillItemTemplate.Load(savefile: TStream);
begin
   inherited Load(savefile);
   FDisplaySkillMessage := savefile.readBool;
   lassert(savefile.readChar = 'K');
end;

procedure TSkillItemTemplate.save(savefile: TStream);
begin
   inherited save(savefile);
   savefile.writeBool(FDisplaySkillMessage);
   savefile.writeChar('K');
end;

{ TVariableItemTemplate }

constructor TVariableItemTemplate.Load(savefile: TStream);
begin
   inherited Load(savefile);
   FWhich := savefile.readWord;
   FMagnitude := savefile.readWord;
   savefile.readBuffer(FStyle, sizeof(TVarSets));
   savefile.readBuffer(FOperation, sizeof(TVarOps));
   lassert(savefile.readChar = 'V');
end;

procedure TVariableItemTemplate.save(savefile: TStream);
begin
   inherited Save(savefile);
   savefile.writeWord(FWhich);
   savefile.writeWord(FMagnitude);
   savefile.WriteBuffer(FStyle, sizeof(TVarSets));
   savefile.WriteBuffer(FOperation, sizeof(TVarOps));
   savefile.writeChar('V');
end;

{ TScriptItemTemplate }

constructor TScriptItemTemplate.Load(savefile: TStream);
begin
   assert(false, 'Can''t load this!');
end;

procedure TScriptItemTemplate.save(savefile: TStream);
begin
   assert(false, 'Can''t save this!');
end;

end.
