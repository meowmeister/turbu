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
   turbu_constants, turbu_defs, turbu_classes, turbu_operators;

type
   TItemType = (it_junk, it_weapon, it_armor, it_medicine, it_upgrade, it_book,
                it_skill, it_variable, it_script);

   TWeaponAnimType = (wa_weapon, wa_battleAnim);
   TMovementMode = (mm_none, mm_stepForward, mm_jumpTo, mm_walkTo);

   ItemTypeAttribute = class(TCustomAttribute)
   private
      FType: TItemType;
   public
      constructor Create(it: TitemType);
      property itemType: TItemType read FType;
   end;

   TWeaponAnimData = class(TRpgDatafile)
   protected
       FAnimType: TWeaponAnimType;
       FWhichWeapon: integer;
       FMovementMode: TMovementMode;
       FAfterimage: boolean;
       FAttackNum: integer;
       FRanged: boolean;
       FRangedProjectile: integer;
       FRangedSpeed: integer;
       FBattleAnim: integer;
   protected
      class function keyChar: ansiChar; override;
   public
      property animType: TWeaponAnimType read FAnimType;
      property whichWeapon: integer read FWhichWeapon;
      property moveMode: TMovementMode read FMovementMode;
      property afterimage: boolean read FAfterimage;
      property attackNum: integer read FAttackNum;
      property ranged: boolean read FRanged;
      property rangedProjectile: integer read FRangedProjectile;
      property rangedSpeed: integer read FRangedSpeed;
      property battleAnim: integer read FBattleAnim;
   end;

   TAnimDataList = class(TRpgDataList<TWeaponAnimData>);

   TItemTemplate = class abstract(TRpgDatafile)
   private
      FDescription: string;
      FCost: integer;
      FTag: T4IntArray;
      function GetItemType: TItemType;
   protected
      class function keyChar: ansiChar; override;
   public
      constructor Load(savefile: TStream); override;
      procedure save(savefile: TStream); override;

      property itemType: TItemType read GetItemType;
      property desc: string read FDescription write FDescription;
      property cost: integer read FCost write FCost;
      property tag: T4IntArray read FTag write FTag;
   end;

   [ItemType(it_junk)]
   TJunkTemplate = class(TItemTemplate);

   TUsableItemTemplate = class abstract(TItemTemplate)
   private
      FUsesLeft: integer;
      FUsableWhere: TUsableWhere;
      [TUploadByteSet]
      FUsableByHero: TByteSet;
      [TUploadByteSet]
      FUsableByClass: TByteSet;

      function getStat(i: byte): integer;
      procedure setStat(i: byte; const Value: integer);
   protected
      FStat: TStatArray;
   public
      constructor Load(savefile: TStream); override;
      procedure save(savefile: TStream); override;

      property stat[i: byte]: integer read getStat write setStat;
      property usesLeft: integer read FUsesLeft write FUsesLeft;
      property usableWhere: TUsableWhere read FUsableWhere write FUsableWhere;
      property usableByHero: TByteSet read FUsableByHero write FUsableByHero;
      property usableByClass: TByteSet read FUsableByClass write FUsableByClass;
   end;

   TEquipmentTemplate = class abstract(TUsableItemTemplate)
   private
      FEvasion: boolean;
      FToHit: integer;
      FCritChance: integer;
      FCritPrevent: integer;
      FPreemptive: integer;
      FMpReduction: integer;
      FNoTerrainDamage: boolean;
      FUsable: boolean;
      [TUploadByteSet]
      FConditions: TByteSet;
      FSkill: integer;
      FInvokeSkill: boolean;
      FCursed: boolean;
      FInflictReversed: boolean;
      FSlot: TSlot;
   protected
      FAttributes: TPointArray;
   public
      constructor Load(savefile: TStream); override;
      procedure save(savefile: TStream); override;

      property evasion: boolean read FEvasion write FEvasion;
      property toHit: integer read FToHit write FToHit;
      property critChance: integer read FCritChance write FCritChance;
      property critPrevent: integer read FCritPrevent write FCritPrevent;
      property preemptive: integer read FPreemptive write FPreemptive;
      property mpReduction: integer read FMpReduction write FMpReduction;
      property noTerrainDamage: boolean read FNoTerrainDamage write FNoTerrainDamage;
      property cursed: boolean read FCursed write FCursed;
      property usable: boolean read FUsable write FUsable; //can be used as an item
      property condition: TByteSet read FConditions write FConditions;
      property attribute: TPointArray read FAttributes write FAttributes;
      property skill: integer read FSkill write FSkill;
      property InvokeSkill: boolean read FInvokeSkill write FInvokeSkill;
      property inflictReversed: boolean read FInflictReversed write FInflictReversed;
      property slot: TSlot read FSlot write FSlot;
   end;

   [ItemType(it_weapon)]
   TWeaponTemplate = class(TEquipmentTemplate)
   private
      FTwoHanded: boolean;
      FAttackTwice: boolean;
      FAreaHit: boolean;
      FBattleAnim: word;
      FMPCost: word;
      FConditionChance: byte;
      FAnimData: TAnimDataList;
   public
      constructor Create; override;
      constructor Load(savefile: TStream); override;
      procedure save(savefile: TStream); override;
      destructor Destroy; override;

      property twoHanded: boolean read FTwoHanded write FTwoHanded;
      property attackTwice: boolean read FAttackTwice write FAttackTwice;
      property areaHit: boolean read FAreaHit write FAreaHit;
      property battleAnim: word read FBattleAnim write FBattleAnim;
      property mpCost: word read FMpCost write FMpCost;
      property conditionChance: byte read FConditionChance write FConditionChance;
      property animData: TAnimDataList read FAnimData;
   end;

   [ItemType(it_armor)]
   TArmorTemplate = class(TEquipmentTemplate)
   public
      constructor Load(savefile: TStream); override;
      procedure save(savefile: TStream); override;
   end;

   [ItemType(it_medicine)]
   TMedicineTemplate = class(TUsableItemTemplate)
   private
      FAreaMedicine: boolean;
      FHPPercent: shortint;
      FMPPercent: shortint;
      FDeadHeroesOnly: boolean;
      FOutOfBattle: boolean;
      [TUploadByteSet]
      FConditions: TByteSet;
   public
      constructor Load(savefile: TStream); override;
      procedure save(savefile: TStream); override;

      property areaMedicine: boolean read FAreaMedicine write FAreaMedicine;
      property hpPercent: shortint read FHpPercent write FHPPercent;
      property mpPercent: shortint read FMpPercent write FMPPercent;
      property hpHeal: integer read FStat[1] write FStat[1];
      property mpHeal: integer read FStat[2] write FStat[2];
      property deadOnly: boolean read FDeadHeroesOnly write FDeadHeroesOnly;
      property outOfBattle: boolean read FOutOfBattle write FOutOfBattle;
      property condition: TByteSet read FConditions write FConditions;
   end;

   [ItemType(it_book)]
   TSkillBookTemplate =class(TUsableItemTemplate)
   private
      FSkill: word;
   public
      constructor Load(savefile: TStream); override;
      procedure save(savefile: TStream); override;

      property skill: word read FSkill write FSkill;
   end;

   [ItemType(it_skill)]
   TSkillItemTemplate = class(TSkillBookTemplate)
   private
      FCustomSkillMessage: boolean;
   public
      constructor Load(savefile: TStream); override;
      procedure save(savefile: TStream); override;

      property customSkillMessage: boolean read FCustomSkillMessage write FCustomSkillMessage;
   end;

   [ItemType(it_upgrade)]
   TStatItemTemplate = class(TUsableItemTemplate);

   [ItemType(it_variable)]
   TVariableItemTemplate = class(TUsableItemTemplate)
   private
      FWhich: word;
      FMagnitude: smallint;
      FStyle: TVarSets;
      FOperation: TBinaryOp;
   public
      constructor Load(savefile: TStream); override;
      procedure save(savefile: TStream); override;

      property which: word read FWhich write FWhich;
      property magnitude: smallint read FMagnitude write FMagnitude;
      property style: TVarSets read FStyle write FStyle;
      property operation: TBinaryOp read FOperation write FOperation;
   end;

   [ItemType(it_script)]
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
   RTTI, SysUtils,
   rttiHelper,
   types;

{ TItemTemplate }

function TItemTemplate.GetItemType: TItemType;
var
   att: ItemTypeAttribute;
begin
   att := ItemTypeAttribute(TRttiContext.Create.GetType(self.ClassInfo).GetAttribute(ItemTypeAttribute));
   if not assigned(att) then
      raise Exception.CreateFmt('No ItemTypeAttr found on %s.', [self.ClassName]);
   result := att.itemType;
end;

class function TItemTemplate.keyChar: ansiChar;
begin
   result := 'i';
end;

constructor TItemTemplate.Load(savefile: TStream);
begin
   inherited Load(savefile);
   FDescription := savefile.readString;
   FCost := savefile.readInt;
   savefile.readBuffer(FTag, sizeof(T4IntArray));
   lassert(savefile.readChar = 'I');
end;

procedure TItemTemplate.save(savefile: TStream);
begin
   inherited save(savefile);
   savefile.writeString(FDescription);
   savefile.writeInt(FCost);
   savefile.WriteBuffer(FTag, sizeof(T4IntArray));
   savefile.writeChar('I');
end;

{ TUsableItemTemplate }

constructor TUsableItemTemplate.Load(savefile: TStream);
var
   dummy: byte;
begin
   inherited Load(savefile);
   FUsesLeft := savefile.readInt;
   savefile.readBuffer(FUsableWhere, sizeof(TUsableWhere));
   dummy := savefile.readByte;
   if dummy > 0 then
      savefile.readBuffer(self.FUsableByHero, dummy);
   dummy := savefile.readByte;
   if dummy > 0 then
      savefile.readBuffer(self.FUsableByClass, dummy);
   lassert(savefile.readInt = STAT_COUNT);
   savefile.readBuffer(FStat[1], sizeof(integer) * STAT_COUNT);
   lassert(savefile.readChar = 'U');
end;

procedure TUsableItemTemplate.save(savefile: TStream);
var
   dummy: byte;
begin
   inherited save(savefile);
   savefile.writeInt(FUsesLeft);
   savefile.WriteBuffer(FUsableWhere, sizeof(TUsableWhere));
   dummy := getSetLength(FUsableByHero);
   savefile.writeByte(dummy);
   if dummy > 0 then
      savefile.WriteBuffer(self.FUsableByHero, dummy);
   dummy := getSetLength(FUsableByClass);
   savefile.writeByte(dummy);
   if dummy > 0 then
      savefile.WriteBuffer(self.FUsableByClass, dummy);
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

{ TEquipmentTemplate }

constructor TEquipmentTemplate.Load(savefile: TStream);
var
   dummy: byte;
begin
   inherited Load(savefile);
   FEvasion := savefile.readBool;
   FToHit := savefile.readInt;
   FCritChance := savefile.readInt;
   FCritPrevent := savefile.readInt;
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
   savefile.writeInt(FCritPrevent);
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

{ TWeaponTemplate }

constructor TWeaponTemplate.Create;
begin
   inherited;
   FAnimData := TAnimDataList.Create;
end;

destructor TWeaponTemplate.Destroy;
begin
   FAnimData.Free;
   inherited;
end;

constructor TWeaponTemplate.Load(savefile: TStream);
begin
   inherited Load(savefile);
   FTwoHanded := savefile.readBool;
   FAttackTwice := savefile.readBool;
   FAreaHit := savefile.readBool;
   FBattleAnim := savefile.readWord;
   FMPCost := savefile.readWord;
   FConditionChance := savefile.readByte;
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
   savefile.writeByte(FConditionChance);
   savefile.writeChar('W');
end;

{ TArmorTemplate }

constructor TArmorTemplate.Load(savefile: TStream);
begin
   inherited Load(savefile);
   lassert(savefile.readChar = 'A');
end;

procedure TArmorTemplate.save(savefile: TStream);
begin
   inherited save(savefile);
   savefile.writeChar('A');
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
   FCustomSkillMessage := savefile.readBool;
   lassert(savefile.readChar = 'K');
end;

procedure TSkillItemTemplate.save(savefile: TStream);
begin
   inherited save(savefile);
   savefile.writeBool(FCustomSkillMessage);
   savefile.writeChar('K');
end;

{ TVariableItemTemplate }

constructor TVariableItemTemplate.Load(savefile: TStream);
begin
   inherited Load(savefile);
   FWhich := savefile.readWord;
   FMagnitude := savefile.readWord;
   savefile.readBuffer(FStyle, sizeof(TVarSets));
   savefile.readBuffer(FOperation, sizeof(TBinaryOp));
   lassert(savefile.readChar = 'V');
end;

procedure TVariableItemTemplate.save(savefile: TStream);
begin
   inherited Save(savefile);
   savefile.writeWord(FWhich);
   savefile.writeWord(FMagnitude);
   savefile.WriteBuffer(FStyle, sizeof(TVarSets));
   savefile.WriteBuffer(FOperation, sizeof(TBinaryOp));
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

{ TWeaponAnimData }

class function TWeaponAnimData.keyChar: ansiChar;
begin
   result := 'w';
end;

{ ItemTypeAttribute }

constructor ItemTypeAttribute.Create(it: TitemType);
begin
   FType := it;
end;

end.
