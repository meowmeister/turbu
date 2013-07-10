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
unit turbu_heroes;

interface
uses
   types,
   rsImport,
   turbu_classes, turbu_containers, turbu_defs, turbu_mapchars,
   turbu_characters, turbu_map_sprites, turbu_2k_items, turbu_constants,
   dwsJSON;

type
   TStatComponents = (stat_base, stat_bonus, stat_eq_mod);

   TRpgParty = class;

   //stub declaration that will be filled in later.
   TRpgHero = class(TRpgObject)
   private
      FName: string;
      FClass: string;
      FSprite: string;
      FTransparent: boolean;
      FLevel: integer;
      FCritRate: integer;
      FFaceName: string;
      FFaceNum: integer;
      FParty: TRpgParty;

      FDualWield: TWeaponStyle;
      FStaticEq: boolean;
      FComputerControlled: boolean;
      FStrongDefense: boolean;

      FExpTable: TArray<integer>;
      FExpTotal: integer;
      FEquipment: array[1..5] of TRpgItem;
      FStat: array[TStatComponents, 1..4] of integer;
      FConditionModifier: TArray<integer>;
      FCondition: TArray<boolean>;
      FDtypeModifiers: TArray<integer>;
      FHitPoints: integer;
      FManaPoints: integer;
      FMaxHitPoints: integer;
      FMaxManaPoints: integer;
      FHpModifier: integer;
      FMpModifier: integer;
      FSkill: TArray<boolean>;

      FLevelUpdated: boolean;
      function countSkills: integer;
      procedure die;
      procedure gainLevel;
      function getCondition(x: integer): boolean;
      function getEquipment(which: integer): integer;
      function getExpNeeded: integer;
      function getHighCondition: integer;
      function getLevelUpdatedStatus: boolean;
      function getMHp: integer;
      function getMMp: integer;
      function getSkill(id: integer): boolean;
      function getStat(which: integer): integer;
      procedure levelAdjustDown(before: integer);
      procedure levelAdjustUp(before: integer);
      procedure levelStatAdjust;
      procedure loseLevel;
      procedure setCondition(x: integer; const value: boolean);
      procedure setExp(value: integer);
      procedure setHP(value: integer);
      procedure setLevel(const value: integer);
      procedure setMaxHp(const Value: integer);
      procedure setMaxMp(const Value: integer);
      procedure setMP(value: integer);
      procedure setSkill(id: integer; value: boolean);
      procedure setStat(which: integer; value: integer);
      procedure setTransparent(const Value: boolean);
      procedure updateLevel(const gain: boolean);

   protected
      class function templateClass: TDatafileClass; override;
   public
      [NoImport]
      constructor Create(base: TClassTemplate; party: TRpgParty);
      destructor Destroy; override;
      [NoImport]
      procedure Serialize(writer: TdwsJSONWriter);
      [NoImport]
      procedure Deserialize(obj: TdwsJSONObject);

      procedure equip(id: integer);
      procedure equipSlot(id, slot: integer);
      procedure Unequip(slot: TSlot);
      function equipped(id: integer): boolean;
      procedure fullheal;
      function takeDamage(power: integer; defense, mDefense, variance: integer): integer;
      procedure setSprite(filename: string; translucent: boolean);
      procedure setPortrait(filename: string; index: integer);
      function inParty: boolean;
      function potentialStat(item, slot, whichStat: integer): integer;
      procedure ChangeHP(quantity: integer; deathPossible: boolean);
      procedure ChangeMP(quantity: integer);
      procedure ChangeClass(id: integer; retainLevel: boolean; skillChange, statChange: integer; showMessage: boolean);
      procedure AddBattleCommand(which: integer);
      procedure RemoveBattleCommand(which: integer);

      property name: string read FName write FName;
      property sprite: string read FSprite;
      property transparent: boolean read FTransparent write setTransparent;
      property title: string read FClass write FClass;
      property level: integer read FLevel write setLevel;
      property exp: longint read FExpTotal write setExp;
      property hp: integer read FHitPoints write setHP;
      property mp: integer read FManaPoints write setMP;
      property maxHp: integer read getMHp write setMaxHp;
      property maxMp: integer read getMMp write setMaxMp;
      property stat[x: integer]: integer read getStat write setStat;
      property attack: integer index 1 read getStat write setStat;
      property defense: integer index 2 read getStat write setStat;
      property mind: integer index 3 read getStat write setStat;
      property agility: integer index 4 read getStat write setStat;
      property equipment[x: integer]: integer read getEquipment;
      property expNeeded: integer read getExpNeeded;
      property levelUpdated: boolean read getLevelUpdatedStatus;
      property skill[x: integer]: boolean read getSkill write setSkill;
      property skills: integer read countSkills;
      property condition[x: integer]: boolean read getCondition write setCondition;
      property dead: boolean index 1 read getCondition;
      property dualWield: TWeaponStyle read FDualWield;
      property highCondition: integer read getHighCondition;
      property AIControlled: boolean read FComputerControlled;
      property StrongDefense: boolean read FStrongDefense;
   end;

   TRpgParty = class(TRpgCharacter)
   private
      //data
      FCash: integer;
      FParty: array[1..MAXPARTYSIZE] of TRpgHero;
      FInventory: TRpgInventory;
      FSprite: TMapSprite;

      //flags
      FLevelNotify: boolean;
      FDeathPossible: boolean;

      function getMap: integer;
      procedure setY(const Value: integer);
      procedure setX(const Value: integer);
      function getFacing: integer;
      procedure setFacing(const Value: integer);
      function getHero(x: integer): TRpgHero;
      procedure setHero(x: integer; value: TRpgHero);
      function empty: boolean;
      function GetTFacing: TFacing;
      function First: TRpgHero;
   protected
      function getX: integer; override;
      function getY: integer; override;
      function getTranslucency: integer; override;
      procedure setTranslucency(const Value: integer); override;
      procedure doFlash(r, g, b, power: integer; time: integer); override;
      function getBase: TMapSprite; override;
   public
      [NoImport]
      constructor Create;
      destructor Destroy; override;
      [NoImport]
      procedure Serialize(writer: TdwsJSONWriter);
      [NoImport]
      procedure Deserialize(obj: TdwsJSONObject);

      procedure addItem(const id, number: integer);
      procedure removeItem(const id, number: integer);
      procedure addExp(const id: integer; number: integer);
      procedure removeExp(const id: integer; number: integer);
      procedure addLevels(const id: integer; number: integer);
      procedure removeLevels(const id: integer; number: integer);
      [NoImport]
      procedure Pack;
      [NoImport]
      procedure ChangeSprite(name: string; translucent: boolean); override;
      [NoImport]
      procedure SetSprite(value: TMapSprite);
      procedure ResetSprite;

      function takeDamage(power: integer; defense, mDefense, variance: integer): integer;
      function openSlot: integer;
      function size: integer;
      function indexOf(who: TRpgHero): integer;

      property money: integer read FCash write FCash;
      property inventory: TRpgInventory read FInventory write FInventory;
      property hero[x: integer]: TRpgHero read getHero write setHero; default;
      property levelNotify: boolean read FLevelNotify write FLevelNotify;
      property deathPossible: boolean read FDeathPossible write FDeathPossible;
      property facingValue: integer read getFacing write setFacing;
      property facing: TFacing read GetTFacing;
      property xPos: integer read getX write setX;
      property yPos: integer read getY write setY;
      property mapID: integer read getMap;
      [NoImport]
      property Sprite: TMapSprite read FSprite;
   end;

   TPartyEvent = procedure(hero: TRpgHero; party: TRpgParty) of object;
   TSkillBoolFunc = function(Character: TRpgHero; Level, unused2, unused3, unused4: integer): boolean of object;
   TSkillNumFunc = function(character: TRpgHero; int1, int2, int3, int4: integer): integer of object;
   TSkillDualNumFunc = function(character: TRpgHero; int1, int2, int3, int4: integer): TPoint of object;

const
   CTN_DEAD = 1;

implementation
uses
   Math, SysUtils,
   ArchiveUtils, turbu_database, dm_database, commons, turbu_items,
   turbu_2k_environment, turbu_2k_sprite_engine, turbu_script_engine,
   turbu_skills, turbu_pathing;

const
   WEAPON_SLOT = 1;
   SHIELD_SLOT = 2;
   ARMOR_SLOT = 3;
   HELMET_SLOT = 4;
   RELIC_SLOT = 5;

   STAT_HP = 1;
   STAT_MP = 2;
   STAT_STR = 3;
   STAT_DEF = 4;
   STAT_MIND = 5;
   STAT_AGI = 6;

{ TRpgHero }

constructor TRpgHero.Create(base: TClassTemplate; party: TRpgParty);
var
   I: Integer;
{  calc: TExpCalcEvent;}
   template: THeroTemplate absolute base;
   cond: TPoint;
begin
{$MESSAGE WARN 'Commented out code in live unit'}
   inherited Create(base);
   if base = nil then
      Exit;

   FParty := party;
   FName := template.name;
   FClass := dmDatabase.NameLookup('charClasses', template.charClass);
   FSprite := template.MapSprite;
   FTransparent := template.translucent;
   setLength(FExpTable, max(template.maxLevel, 1) + 1);
{   calc := TExpCalcEvent(GScriptEngine.GetExecMethod(template.expFunc));
   if assigned(calc) then
      for I := 2 to 50 do
         FExpTable[i] := calc(i, template.expVars[1], template.expVars[2], template.expVars[3], template.expVars[4]);
}
   if template.canCrit then
      FCritRate := template.critRate
   else FCritRate := 0;
   FFaceName := template.portrait;
   FFaceNum := template.portraitIndex;
   FDualWield := template.dualWield;
   FStaticEq := template.staticEq;
   setLength(FSkill, GDatabase.skill.count + 1);
   FLevel := 1;
   self.levelAdjustUp(0);
   level := template.minLevel;
   FExpTotal := FExpTable[FLevel];
   for I := low(FEquipment) to high(FEquipment) do
      if template.eq[i] <> 0 then
         self.equip(template.eq[i]);
   i := GDatabase.conditions.Count;
   setLength(FConditionModifier, i);
   setLength(FCondition, i);
   for I := 0 to high(template.condition) do
   begin
      cond := template.condition[i];
      FConditionModifier[cond.x] := cond.y;
   end;
   FHitPoints := maxHp;
   FManaPoints := maxMp;
   FComputerControlled := template.guest;
   FStrongDefense := template.strongDef;
end;

class function TRpgHero.templateClass: TDatafileClass;
begin
   result := TClassTemplate;
end;

procedure TRpgHero.Serialize(writer: TdwsJSONWriter);
var
   base: THeroTemplate;
   i: integer;
begin
   base := self.Template as THeroTemplate;
   writer.BeginObject;
      writer.CheckWrite('Name', FName, base.name);
      writer.CheckWrite('Class', FClass, base.clsName);
      writer.CheckWrite('Sprite', FSprite, base.mapSprite);
      writer.CheckWrite('Transparent', FTransparent, base.translucent);
      writer.CheckWrite('Level', FLevel, base.minLevel);
      writer.CheckWrite('FaceName', FFaceName, base.portrait);
      writer.CheckWrite('FaceNum', FFaceNum, base.portraitIndex);
      writer.CheckWrite('ExpTotal', FExpTotal, 0);
      writer.WriteName('Equipment');
      writer.BeginArray;
         for i := 1 to 5 do
            if FEquipment[i] = nil then
               writer.WriteNull
            else writer.WriteInteger(FEquipment[i].id);
      writer.EndArray;
      writer.WriteName('Stat');
      writer.BeginArray;
         for i := 1 to 4 do
            writer.WriteInteger(FStat[stat_bonus][i]);
      writer.EndArray;
      writer.WriteName('Condition');
      writer.BeginArray;
         for i := 1 to High(FCondition) do
            if FCondition[i] then
               writer.WriteInteger(i);
      writer.EndArray;
      writer.CheckWrite('HitPoints', FHitPoints, FMaxHitPoints + FHpModifier);
      writer.CheckWrite('ManaPoints', FManaPoints, FMaxManaPoints + FMpModifier);
      writer.CheckWrite('HpModifier', FHpModifier, 0);
      writer.CheckWrite('MpModifier', FMpModifier, 0);
      writer.WriteArray('Skill', FSkill);
   writer.EndObject;
end;

procedure TRpgHero.Deserialize(obj: TdwsJSONObject);
var
   arr: TdwsJSONArray;
   i: integer;
begin
   obj.CheckRead('Name', FName);
   obj.CheckRead('Class', FClass);
   obj.CheckRead('Sprite', FSprite);
   obj.CheckRead('Transparent', FTransparent);
   obj.CheckRead('Level', FLevel);
   obj.CheckRead('FaceName', FFaceName);
   obj.CheckRead('FaceNum', FFaceNum);
   obj.CheckRead('ExpTotal', FExpTotal);
   obj.CheckRead('HitPoints', FHitPoints);
   obj.CheckRead('ManaPoints', FManaPoints);
   obj.CheckRead('HpModifier', FHpModifier);
   obj.CheckRead('MpModifier', FMpModifier);
   arr := obj.Items['Equipment'] as TdwsJSONArray;
   for i := 1 to 5 do
      if arr.Elements[i - 1].ValueType = jvtNull then
         self.Unequip(TSlot(i - 1))
      else self.equipSlot(arr.Elements[i - 1].AsInteger, i);
   arr.Free;
   arr := obj.Items['Stat'] as TdwsJSONArray;
   for i := 1 to 4 do
      FStat[stat_bonus][i] := arr.Elements[i - 1].AsInteger;
   arr.Free;
   arr := obj.Items['Condition'] as TdwsJSONArray;
   for i := 0 to arr.ElementCount - 1 do
      FCondition[arr.Elements[i].AsInteger] := true;
   arr.Free;
   obj.readArray('Skill', FSkill);
   obj.checkEmpty;
end;

procedure TRpgHero.AddBattleCommand(which: integer);
begin
{$MESSAGE WARN 'Commented out code in live unit'}
   //TODO: Implement this
end;

procedure TRpgHero.RemoveBattleCommand(which: integer);
begin
{$MESSAGE WARN 'Commented out code in live unit'}
   //TODO: Implement this
end;

procedure TRpgHero.ChangeClass(id: integer; retainLevel: boolean; skillChange,
  statChange: integer; showMessage: boolean);
begin
{$MESSAGE WARN 'Commented out code in live unit'}
   //TODO: Implement this
end;

procedure TRpgHero.ChangeHP(quantity: integer; deathPossible: boolean);
begin
   FParty.deathPossible := deathPossible;
   setHP(FHitPoints + quantity);
end;

procedure TRpgHero.ChangeMP(quantity: integer);
begin
   setMP(FManaPoints + quantity);
end;

function TRpgHero.countSkills: integer;
var
  i: Integer;
begin
   result := 0;
   for i := 1 to high(FSkill) do
      if FSkill[i] then
         inc(result);
end;

destructor TRpgHero.Destroy;
var
   i: integer;
   j: integer;
begin
   for i := 1 to 5 do
   begin
      for j := i + 1 to 5 do
         if FEquipment[j] = FEquipment[i] then
            FEquipment[j] := nil;
      FEquipment[i].free;
   end;
   inherited Destroy;
end;

procedure TRpgHero.die;
begin
   FCondition[CTN_DEAD] := true;
   FHitPoints := 0;
end;

procedure TRpgHero.equip(id: integer);
var
   theItem: TRpgItem;
   dummy: TItemType;
   slot: integer;
begin
   if not IsBetween(id, 0, GDatabase.items) then
      Exit;

   theItem := TRpgItem.newItem(id, 1);
   if not ((theItem.template is TEquipmentTemplate) and (Template.id in (TEquipmentTemplate(theItem.template).usableByHero))) then
   begin
      theItem.free;
      Exit;
   end;

   dummy := theItem.template.itemType;
   if (dummy = it_weapon) and (TWeaponTemplate(theItem.template).twoHanded) then
   begin
      unequip(eq_weapon);
      unequip(eq_shield);
      FEquipment[WEAPON_SLOT] := theItem;
      FEquipment[SHIELD_SLOT] := theItem;
   end
   else begin
      if dummy = it_weapon then
         slot := WEAPON_SLOT
      else slot := (theItem.template as TArmorTemplate).slot;
      unequip(TSlot(slot - 1));
      FEquipment[slot] := theItem;
   end;
   FParty.inventory.Remove(id, 1);
{$MESSAGE WARN 'Commented out code in live unit'}
{   inc(FStat[stat_eq_mod, 1], TEquipment(theItem).attack);
   inc(FStat[stat_eq_mod, 2], TEquipment(theItem).defense);
   inc(FStat[stat_eq_mod, 3], TEquipment(theItem).mind);
   inc(FStat[stat_eq_mod, 4], TEquipment(theItem).speed);}
end;

procedure TRpgHero.equipSlot(id, slot: integer);
{var
   theItem: TRpgItem;
   dummy: TItemType;}
begin
{$MESSAGE WARN 'Commented out code in live unit'}
{   theItem := TRpgItem.newItem(id, 1);
   assert(theItem is TEquipment);
   dummy := theItem.template.itemType;
   if not (theItem.template.usableBy[FTemplate.id]) then
      Exit;

   if (dummy = weaponItem) and (theItem.template.twoHanded) then
   begin
      unequip(integer(weaponItem) - 1);
      unequip(integer(shieldItem) - 1);
      FEquipment[integer(weaponItem)] := theItem;
      FEquipment[integer(shieldItem)] := theItem;
   end
   else begin
      unequip(integer(dummy) - 1);
      FEquipment[slot] := theItem;
   end;
   GParty.inventory.Remove(id, 1);
   inc(FStat[stat_eq_mod, 1], TEquipment(theItem).attack);
   inc(FStat[stat_eq_mod, 2], TEquipment(theItem).defense);
   inc(FStat[stat_eq_mod, 3], TEquipment(theItem).mind);
   inc(FStat[stat_eq_mod, 4], TEquipment(theItem).speed);}
end;

procedure TRpgHero.unequip(slot: TSlot);
var
   I: TSlot;
   id: integer;
begin
   if slot = eq_all then
   begin
      for I := eq_weapon to eq_relic do
         unequip(i);
      Exit;
   end;

   id := ord(slot) + 1;
   if FEquipment[id] <> nil then
   begin
      FParty.inventory.AddItem(FEquipment[id]);
{$MESSAGE WARN 'Commented out code in live unit'}
{      dec(FStat[stat_eq_mod, 1], TEquipment(FEquipment[id]).attack);
      dec(FStat[stat_eq_mod, 2], TEquipment(FEquipment[id]).defense);
      dec(FStat[stat_eq_mod, 3], TEquipment(FEquipment[id]).mind);
      dec(FStat[stat_eq_mod, 4], TEquipment(FEquipment[id]).speed);
      if (id in [1, 2]) and (FEquipment[id].template.twoHanded) then
         FEquipment[3 - id] := nil;}
      FEquipment[id] := nil;
   end;
end;

function TRpgHero.equipped(id: integer): boolean;
var
   i: Integer;
begin
   result := false;
   for I := low(FEquipment) to high(FEquipment) do
      if (FEquipment[i] <> nil) and (FEquipment[i].template.id = id) then
         result := true;
end;

procedure TRpgHero.fullheal;
var
  I: Integer;
begin
   FHitPoints := FMaxHitPoints;
   FManaPoints := FMaxManaPoints;
   for I := 1 to high(FCondition) do
      FCondition[i] := false;
end;

function TRpgHero.getCondition(x: integer): boolean;
begin
   if x in [1..high(FCondition)] then
      result := FCondition[x]
   else result := false;
end;

function TRpgHero.getEquipment(which: integer): integer;
begin
   if assigned(FEquipment[which]) then
      result := FEquipment[which].template.id
   else result := 0;
end;

function TRpgHero.getExpNeeded: integer;
begin
   if FLevel = 50 then
      result := -1
   else result := FExpTable[FLevel + 1] - FExpTotal;
end;

function TRpgHero.getHighCondition: integer;
var
   i: integer;
   highPriority: integer;
begin
   highPriority := 0;
   result := 0;
   for I := high(FCondition) downto 1 do
      if (FCondition[i]) and (GDatabase.conditions[i].priority >= highPriority) then
         result := i;
end;

function TRpgHero.getLevelUpdatedStatus: boolean;
begin
   result := FLevelUpdated;
   FLevelUpdated := false;
end;

function TRpgHero.getMHp: integer;
begin
   result := FMaxHitPoints + FHpModifier;
   if result < 1 then
   begin
      dec(FHpModifier, result + 1);
      result := 1;
   end;
end;

function TRpgHero.getMMp: integer;
begin
   result := FMaxManaPoints + FMpModifier;
   if result < 0 then
   begin
      dec(FMpModifier, result);
      result := 0;
   end;
end;

function TRpgHero.getSkill(id: integer): boolean;
begin
   if (id > 0) and (id < high(FSkill)) then
      result := FSkill[id]
   else
      result := FSkill[id];
end;

function TRpgHero.getStat(which: integer): integer;
var i: TStatComponents;
begin
   result := 0;
   for i := low(TStatComponents) to high(TStatComponents) do
      inc(result, FStat[i, which]);
   if result < 0 then
      result := 0;
end;

function TRpgHero.inParty: boolean;
var
  I: Integer;
begin
   result := false;
   for I := 1 to MAXPARTYSIZE do
      if FParty[i] = self then
         result := true;
end;

procedure TRpgHero.setCondition(x: integer; const value: boolean);
begin
   if (x = CTN_DEAD) and (value = true)then
      self.die
   else if x in [1..high(FCondition)] then
      FCondition[x] := value;
end;

procedure TRpgHero.setExp(value: integer);
begin
   FExpTotal := clamp(value, 0, MAXEXP);
   if (FLevel < MAXLEVEL) then
      if (expNeeded <= 0) then
         updateLevel(true)
      else if (expNeeded > FExpTable[FLevel + 1] - FExpTable[FLevel]) then
         updateLevel(false);
end;

procedure TRpgHero.setHP(value: integer);
begin
   if FCondition[CTN_DEAD] then
      Exit;

   FHitPoints := max(value, 0);
   if FHitPoints = 0 then
   begin
      if FParty.deathPossible = true then
         self.die
      else inc(FHitPoints);
   end
   else if FHitPoints > self.maxHp then
      FHitPoints := maxHp;
end;

procedure TRpgHero.setLevel(const value: integer);
var
   increasing: boolean;
   oldlevel: integer;
begin
   if FLevel = value then
      Exit;

   increasing := FLevel < value;
   oldlevel := FLevel;
   FLevel := clamp(value, 0, MAXLEVEL);

   FExpTotal := FExpTable[FLevel];
   if increasing then
      levelAdjustUp(oldlevel)
   else levelAdjustDown(oldlevel);
end;


procedure TRpgHero.levelAdjustDown(before: integer);
var
   i: integer;
   base: THeroTemplate;
   skill: TSkillGainInfo;
begin
   base := template as THeroTemplate;
   for I := 1 to base.skillset.Count - 1 do
   begin
      skill := base.skillset[i];
      if (skill.style = sf_level) and ((skill.num[1] > FLevel) and (skill.num[1] <= before)) then
         FSkill[base.skillset[i].id] := false;
   end;
   levelStatAdjust;
end;

procedure TRpgHero.levelAdjustUp(before: integer);
var
   i: integer;
   base: THeroTemplate;
   skill: TSkillGainInfo;
begin
   base := template as THeroTemplate;
   for I := 1 to base.skillset.Count - 1 do
   begin
      skill := base.skillset[i];
      if (skill.style = sf_level) and ((skill.num[1] <= FLevel) and (skill.num[1] > before)) then
         FSkill[base.skillset[i].id] := false;
   end;
   levelStatAdjust;
end;

procedure TRpgHero.levelStatAdjust;
var
   base: THeroTemplate;
begin
   base := template as THeroTemplate;
   if base.id = 0 then
      Exit;
   FMaxHitPoints := base.statblock[stat_hp].block[FLevel];
   FMaxManaPoints := base.statblock[stat_mp].block[FLevel];
   FStat[stat_base, 1] := base.statblock[stat_str].block[FLevel];
   FStat[stat_base, 2] := base.statblock[stat_def].block[FLevel];
   FStat[stat_base, 3] := base.statblock[stat_mind].block[FLevel];
   FStat[stat_base, 4] := base.statblock[stat_agi].block[FLevel];
end;

procedure TRpgHero.loseLevel;
begin
   dec(FLevel);
   levelAdjustDown(FLevel + 1);
end;

function TRpgHero.potentialStat(item, slot, whichStat: integer): integer;
var
   theItem: TItemTemplate;
begin
   theItem := GDatabase.findItem(item);
   assert((item = 0) or (theItem.itemType in [it_weapon, it_armor]));
   result := self.stat[whichStat];
   if self.FEquipment[slot] <> nil then
      result := result - (FEquipment[slot].template as TUsableItemTemplate).stat[whichStat];
   if assigned(theItem) and (theItem.itemType = it_weapon) and (TWeaponTemplate(theItem).twoHanded) and (FEquipment[3 - slot] <> nil) then
      result := result - TUsableItemTemplate(FEquipment[3 - slot].template).stat[whichStat];
   if item <> 0 then
      result := result + TUsableItemTemplate(GDatabase.findItem(item)).stat[whichStat];
end;

procedure TRpgHero.gainLevel;
begin
   inc(FLevel);
   levelAdjustUp(FLevel - 1);
end;

procedure TRpgHero.setMaxHp(const Value: integer);
var dummy: integer;
begin
   dummy := value - self.maxHp;
   inc(FHpModifier, dummy);
end;

procedure TRpgHero.setMaxMp(const Value: integer);
var dummy: integer;
begin
   dummy := value - self.maxMp;
   inc(FMpModifier, dummy);
end;

procedure TRpgHero.setMP(value: integer);
begin
   if FCondition[CTN_DEAD] then
      Exit;

   FManaPoints := Value;
   if FManaPoints < 0 then
      FManaPoints := 0
   else if FManaPoints > self.maxMp then
      FManaPoints := maxMp;
end;

procedure TRpgHero.setPortrait(filename: string; index: integer);
begin
   if not (index in [1..16]) then
      Exit;
   if not (ArchiveUtils.GraphicExists(filename, 'portrait')) then
      Exit;

   FFaceName := filename;
   FFaceNum := index;
   GSpriteEngine.Images.EnsureImage(filename, 'portrait', PORTRAIT_SIZE);
end;

procedure TRpgHero.setSkill(id: integer; value: boolean);
begin
   if (id > 0) and (id < high(FSkill)) then
      FSkill[id] := value;
end;

procedure TRpgHero.setSprite(filename: string; translucent: boolean);
begin
   if not (ArchiveUtils.GraphicExists(filename, 'mapsprite')) then
      Exit;

   FSprite := filename;
   FTransparent := translucent;
   if FParty[1] = self then
      FParty.ChangeSprite(ChangeFileExt(filename, ''), translucent);
end;

procedure TRpgHero.setStat(which: integer; value: integer);
begin
   inc(FStat[stat_bonus, which], self.stat[which] - value);
end;

procedure TRpgHero.setTransparent(const Value: boolean);
var
   party: TCharSprite;
begin
   FTransparent := Value;
   if FParty[1] = self then
   begin
      party := GSpriteEngine.CurrentParty;
      if value then
         party.translucency := 3
      else party.translucency := 0;
      party.update(FSprite, value);
   end;
end;

function TRpgHero.takeDamage(power: integer; defense, mDefense, variance: integer): integer;
begin
   FParty.deathPossible := true;
   hp := hp - power;
   result := power;
end;

procedure TRpgHero.updateLevel(const gain: boolean);
begin
   FLevelUpdated := true;
   case gain of
      true:
      begin
         assert(FExpTotal >= FExpTable[FLevel + 1]);
         repeat
            gainLevel;
         until (FExpTotal < FExpTable[FLevel + 1]);
      end;
      false:
      begin
         assert(FExpTotal <= FExpTable[FLevel]);
         repeat
            loseLevel;
         until FExpTotal >= FExpTable[FLevel];
      end;
   end;
end;

{ TRpgParty }

procedure TRpgParty.addExp(const id: integer; number: integer);
var
  I: Integer;
  hero: TRpgHero;
begin
   if id = -1 then
   begin
      for I := 1 to MAXPARTYSIZE do
         if self[i] <> GEnvironment.Heroes[0] then
         begin
            hero := GEnvironment.Heroes[self[i].template.id];
            hero.exp := hero.exp + number;
         end;
   end else
      self[id].exp := self[id].exp + number;
end;

procedure TRpgParty.addItem(const id, number: integer);
begin
   FInventory.Add(id, number);
end;

procedure TRpgParty.addLevels(const id: integer; number: integer);
var
   I: Integer;
   hero: TRpgHero;
begin
   if id = -1 then
   begin
      for I := 1 to MAXPARTYSIZE do
         if self[i] <> GEnvironment.Heroes[0] then
         begin
            hero := GEnvironment.Heroes[self[i].template.id];
            hero.level := hero.level + number;
         end;
   end else
      self[id].level := self[id].level + number;
end;

procedure TRpgParty.ChangeSprite(name: string; translucent: boolean);
begin
   if assigned(FSprite) then
      FSprite.update(name, translucent);
end;

constructor TRpgParty.Create;
begin
   FInventory := TRpgInventory.Create();
   FLevelNotify := true;
end;

destructor TRpgParty.Destroy;
begin
   FInventory.free;
   inherited Destroy;
end;

function TRpgParty.getBase: TMapSprite;
begin
   result := FSprite;
end;

function TRpgParty.getFacing: integer;
begin
   result := 0;
   case FSprite.facing of
      facing_up: result := 8;
      facing_right: result := 6;
      facing_down: result := 2;
      facing_left: result := 4;
   end;
end;

function TRpgParty.getHero(x: integer): TRpgHero;
begin
   if (x = 0) or (x > MAXPARTYSIZE) or (FParty[x] = nil) then
      result := GEnvironment.Heroes[0]
   else result := FParty[x];
end;

function TRpgParty.getMap: integer;
begin
   result := GSpriteEngine.mapID;
end;

function TRpgParty.GetTFacing: TFacing;
begin
   result := FSprite.facing;
end;

function TRpgParty.getTranslucency: integer;
begin
   if empty then
      result := 0
   else result := inherited getTranslucency;
end;

function TRpgParty.getX: integer;
begin
   if assigned(FSprite) then
      result := FSprite.location.x
   else result := 0;
end;

function TRpgParty.getY: integer;
begin
   if assigned(FSprite) then
      result := FSprite.location.y
   else result := 0;
end;

function TRpgParty.indexOf(who: TRpgHero): integer;
var i: integer;
begin
   result := -1;
   for i := 1 to MAXPARTYSIZE do
      if self[i] = who then
         result := i;
end;

function TRpgParty.empty: boolean;
var i: integer;
begin
   result := true;
   for i := 1 to MAXPARTYSIZE do
      if self[i] <> GEnvironment.Heroes[0] then
         result := false;
end;

function TRpgParty.First: TRpgHero;
var
   i: integer;
begin
   for i := 1 to MAXPARTYSIZE do
      if (self[i] <> GEnvironment.Heroes[0]) then
         exit(self[i]);
   result := self[1];
end;

procedure TRpgParty.doFlash(r, g, b, power: integer; time: integer);
begin
   if assigned(GSpriteEngine.CurrentParty) then
      GSpriteEngine.CurrentParty.flash(r, g, b, power, time);
end;

function TRpgParty.size: integer;
var i: integer;
begin
   result := 0;
   for i := 1 to MAXPARTYSIZE do
      if (self[i] <> GEnvironment.Heroes[0]) then
         inc(result);
end;

function TRpgParty.openSlot: integer;
var i: integer;
begin
   i := 1;
   while (self[i] <> GEnvironment.Heroes[0]) and (i <= MAXPARTYSIZE) do
      inc(i);
   if i > MAXPARTYSIZE then
      result := 0
   else result := i;
end;

procedure TRpgParty.removeExp(const id: integer; number: integer);
var
   I: Integer;
   hero: TRpgHero;
begin
   if id = -1 then
   begin
      for I := 1 to MAXPARTYSIZE do
         if self[i] <> GEnvironment.Heroes[0] then
         begin
            hero := GEnvironment.Heroes[self[i].template.id];
            hero.exp := hero.exp - number;
         end;
   end else
      self[id].exp := self[id].exp - number;
end;

procedure TRpgParty.removeItem(const id, number: integer);
begin
   FInventory.Remove(id, number);
end;

procedure TRpgParty.removeLevels(const id: integer; number: integer);
var
   I: Integer;
   hero: TRpgHero;
begin
   if id = -1 then
   begin
      for I := 1 to MAXPARTYSIZE do
         if self[i] <> GEnvironment.Heroes[0] then
         begin
            hero := GEnvironment.Heroes[self[i].template.id];
            hero.level := hero.level - number;
         end;
   end else
      self[id].level := self[id].level - number;
end;

procedure TRpgParty.ResetSprite;
var
   h1: TRpgHero;
begin
   h1 := self.First;
   commons.runThreadsafe(
      procedure begin self.ChangeSprite(h1.sprite, h1.transparent) end, true);
end;

procedure TRpgParty.Serialize(writer: TdwsJSONWriter);
var
   i: integer;
begin
   writer.BeginObject;
      writer.WriteName('Heroes');
      writer.BeginArray;
         for i := 1 to MAXPARTYSIZE do
            if assigned(FParty[i]) then
               writer.WriteInteger(FParty[i].Template.id)
            else writer.WriteNull;
      writer.EndArray;
      writer.CheckWrite('Cash', FCash, 0);
      writer.WriteName('Inventory');
      FInventory.Serialize(writer);
      writer.WriteName('X');
      writer.WriteInteger(getX);
      writer.WriteName('Y');
      writer.WriteInteger(getY);
      writer.WriteName('Facing');
      writer.WriteInteger(getFacing);
      if assigned(FSprite.moveOrder) then
      begin
         writer.WriteName('Path');
         FSprite.moveOrder.serialize(writer);
      end;
      writer.CheckWrite('MoveFreq', FSprite.moveFreq, 1);
      writer.CheckWrite('MoveRate', FSprite.moveRate, 1);
   writer.EndObject;
end;

procedure TRpgParty.Deserialize(obj: TdwsJSONObject);
var
   i: integer;
   value: TdwsJSONValue;
begin
   value := obj.Items['Heroes'];
   for i := 1 to MAXPARTYSIZE do
      if value.Elements[i - 1].IsNull then
         self.setHero(i, nil)
      else self.setHero(i, GEnvironment.Heroes[value.Elements[i - 1].AsInteger]);
   value.Free;
   obj.CheckRead('Cash', FCash);
   value := obj.Items['Inventory'];
   FInventory.Deserialize(value as TdwsJSONArray);
   value.Free;
   value := obj.Items['X'];
   SetX(value.AsInteger);
   value.Free;
   value := obj.Items['Y'];
   SetY(value.AsInteger);
   value.Free;
   value := obj.Items['Facing'];
   setFacing(value.AsInteger);
   value.Free;
   value := obj.Items['Path'];
   if assigned(value) then
   begin
      FSprite.moveOrder := TPath.Deserialize(value as TdwsJSONObject);
      value.Free;
   end;
   value := obj.Items['MoveFreq'];
   if assigned(value) then
   begin
      FSprite.moveFreq := value.AsInteger;
      value.Free;
   end;
   value := obj.Items['MoveRate'];
   if assigned(value) then
   begin
      FSprite.moveRate := value.AsInteger;
      value.Free;
   end;
   obj.CheckEmpty;
end;

procedure TRpgParty.setFacing(const Value: integer);
begin
   case value of
      8: FSprite.facing := facing_up;
      6: FSprite.facing := facing_right;
      4: FSprite.facing := facing_left;
      2: FSprite.facing := facing_down;
   end;
end;

procedure TRpgParty.setHero(x: integer; value: TRpgHero);
begin
   if (x = 0) or (x > MAXPARTYSIZE) then
      Exit;

   FParty[x] := value;
   if assigned(value) then
      FParty[x].FParty := self;
   ResetSprite;
end;

procedure TRpgParty.SetSprite(value: TMapSprite);
begin
   FSprite := value;
end;

procedure TRpgParty.setTranslucency(const Value: integer);
begin
   if not empty then
      inherited setTranslucency(value);
end;

procedure TRpgParty.setX(const Value: integer);
var place: TPoint;
begin
   if assigned(FSprite) then
   begin
      place := FSprite.location;
      FSprite.location := point(value, place.Y);
   end;
end;

procedure TRpgParty.setY(const Value: integer);
var place: TPoint;
begin
   if assigned(FSprite) then
   begin
      place := FSprite.location;
      FSprite.location := point(place.x, value);
   end;
end;

procedure TRpgParty.Pack;
var
   i, j: integer;
begin
   for I := 1 to MAXPARTYSIZE - 1 do
      for j := i to MAXPARTYSIZE - 1 do
      begin
         if FParty[j] = nil then
         begin
            FParty[j] := FParty[j + 1];
            FParty[j + 1] := nil;
         end;
      end;
end;

function TRpgParty.takeDamage(power: integer; defense, mDefense,
  variance: integer): integer;
var i: integer;
begin
   result := 0;
   for i := 1 to MAXPARTYSIZE do
      if self[i] <> GEnvironment.Heroes[0] then
         inc(result, FParty[i].takeDamage(power, defense, mdefense, variance));
end;

end.
