unit script_backend;
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

{$I ..\..\..\..\unfinished.inc}

interface
uses sysUtils,
     {addition_sprite, charset_data, item_data,} turbu_characters, rpg_list,
     {LMT,} turbu_database;

type
   TStatComponents = (stat_base, stat_bonus, stat_eq_mod);

   TRpgHero = class(TObject)
   private
      FName: string;
      FClass: string;
      FSprite: string;
      FSpriteIndex: byte;
      FTransparent: boolean;
      FLevel: byte;
      FCritRate: byte;
      FFaceName: string;
      FFaceNum: byte;
      {}
      FDualWield: boolean;
      FStaticEq: boolean;
      FComputerControlled: boolean;
      FStrongDefense: boolean;
      {}
      FExpTable: array[1..50] of integer;
      FExpTotal: integer;
      FEquipment: array[1..5] of TRpgItem;
      FStat: array[TStatComponents, 1..4] of smallint;
      FConditionModifier: array of byte;
      FCondition: array of boolean;
      FDtypeModifiers: array of byte;
      FHitPoints: integer;
      FManaPoints: integer;
      FMaxHitPoints: integer;
      FMaxManaPoints: integer;
      FHpModifier: integer;
      FMpModifier: integer;
      FSkill: array of boolean;

      FTemplate: THeroTemplate;
      FLevelUpdated: boolean;

      function getMHp: integer;
      function getMMp: integer;
      function getHighCondition: byte;
      function getCondition(x: integer): boolean;
      function getStat(which: integer): smallint;
      function getEquipment(which: byte): word;
      function getExpNeeded: integer;
      function getLevelUpdatedStatus: boolean;
      function getSkill(id: word): boolean;
      function countSkills: word;

      procedure setTransparent(const Value: boolean);
      procedure setCondition(x: integer; const Value: boolean);
      procedure setExp(value: integer);
      procedure setStat(which: integer; value: smallint);
      procedure setLevel(const value: byte);
      procedure updateLevel(const gain: boolean);
      procedure setSkill(id: word; value: boolean);
      procedure setHP(value: integer);
      procedure setMP(value: integer);
      procedure setMaxHp(const Value: integer);
      procedure setMaxMp(const Value: integer);
      procedure gainLevel;
      procedure loseLevel;
      procedure levelAdjustUp(before: byte);
      procedure levelAdjustDown(before: byte);
      procedure levelStatAdjust;
      procedure die;
   public
      constructor create(const base: THeroTemplate);
      destructor Destroy; override;
      procedure equip(id: word); overload;
      procedure equip(id, slot: word); overload;
      procedure unequip(id: byte);
      function equipped(id: smallint): boolean;
      procedure fullheal;
      function takeDamage(power: word; defense, mDefense, variance: byte): word;
      procedure setSprite(filename: string; index: byte; translucent: boolean);
      procedure setPortrait(filename: string; index: byte);
      function inParty: boolean;
      function potentialStat(item, slot, whichStat: word): word;

      property template: THeroTemplate read FTemplate;
      property name: string read FName write FName;
      property sprite: string read FSprite;
      property spriteIndex: byte read FSpriteIndex;
      property transparent: boolean read FTransparent write setTransparent;
      property charClass: string read FClass write FClass;
      property level: byte read FLevel write setLevel;
      property exp: longint read FExpTotal write setExp;
      property hp: integer read FHitPoints write setHP;
      property mp: integer read FManaPoints write setMP;
      property maxHp: integer read getMHp write setMaxHp;
      property maxMp: integer read getMMp write setMaxMp;
      property stat[x: integer]: smallint read getStat write setStat;
      property attack: smallint index 1 read getStat write setStat;
      property defense: smallint index 2 read getStat write setStat;
      property mind: smallint index 3 read getStat write setStat;
      property agility: smallint index 4 read getStat write setStat;
      property equipment[x: byte]: word read getEquipment;
      property expNeeded: integer read getExpNeeded;
      property levelUpdated: boolean read getLevelUpdatedStatus;
      property skill[x: word]: boolean read getSkill write setSkill;
      property skills: word read countSkills;
      property condition[x: integer]: boolean read getCondition write setCondition;
      property dead: boolean index 1 read getCondition;
      property dualWield: boolean read FDualWield;
      property highCondition: byte read getHighCondition;
   end;

const
   MAXEXP = 1000000;
   MAXLEVEL = 50;
   CTN_DEAD = 1;

implementation
uses
   windows,
   {chipset_graphics, charset_graphics, script_engine, locate_files,
   item_code,} script_interface, turbu_items, commons; //turbu libs

{ TRpgHero }

function TRpgHero.countSkills: word;
var
  i: Integer;
begin
   result := 0;
   for i := 1 to high(FSkill) do
      if FSkill[i] then
         inc(result);
      //end if
   //end for
end;

constructor TRpgHero.create(const base: THeroTemplate);
var
  I: Integer;
  dummy: byte;
begin
   FTemplate := base;
   if base = nil then
      Exit;

   FName := base.name;
{   FClass := base.charClass;
   FSprite := base.sprite;
   FSpriteIndex := base.spriteIndex;
   FTransparent := base.transparent;
   FExpTable[1] := 0;
   for I := 2 to 50 do
      FExpTable[i] := hero_data.calcExp(i, base.expStandard, base.expAddition, base.expCorrection, 0);
   if base.canCrit then
      FCritRate := base.critRate
   else FCritRate := 0;
   FFaceName := base.portrait;
   FFaceNum := base.portraitIndex;
   FDualWield := base.dualWield;
   FStaticEq := base.staticEq;
   setLength(FSkill, GDatabase.skills + 1); //wrong identifier; need skill sec max from DB
   for I := 1 to base.skills do
      if base.skill[i].level <= FLevel then
         FSkill[base.skill[i].id] := true;
      //end if
   //end for
   FLevel := 1;
   self.levelAdjustUp(0);
   level := base.startLevel;
   FExpTotal := FExpTable[FLevel];
   for I := low(FEquipment) to high(FEquipment) do
      if base.initialEq[i] <> 0 then
         self.equip(base.initialEq[i]);
   //end for
   i := base.conditionModifiers;
   setLength(FConditionModifier, i);
   setLength(FCondition, i);
   for I := 0 to high(FCondition) do
   begin
      dummy := base.conditionModifier[i];
      FConditionModifier[i] := GDatabase.condition[i].chance[dummy]; //fix this
      FCondition[i] := false;
   end;}
   FHitPoints := maxHp;
   FManaPoints := maxMp;
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
         //end if
      FEquipment[i].free;
   end;
   inherited;
end;

procedure TRpgHero.die;
begin
   FCondition[CTN_DEAD] := true;
   FHitPoints := 0;
end;

procedure TRpgHero.equip(id: word);
var
   theItem: TRpgItem;
   dummy: TItemType;
begin
   if between(id, 0, GDatabase.items) <> id then
      Exit;

   theItem := TRpgItem.newItem(id, 1);
{   if not ((theItem is TEquipmentTemplate) and (TEquipmentTemplate(theItem.template).usableBy[FTemplate.id])) then
   begin
      theItem.free;
      Exit;
   end;

   dummy := theItem.template.itemType;
   if (dummy = weaponItem) and (theItem.template.twoHanded) then
   begin
      unequip(byte(weaponItem) - 1);
      unequip(byte(shieldItem) - 1);
      FEquipment[byte(weaponItem)] := theItem;
      FEquipment[byte(shieldItem)] := theItem;
   end
   else begin
      unequip(byte(dummy) - 1);
      FEquipment[byte(dummy)] := theItem;
   end;
   GParty.inventory.Remove(id, 1);
   inc(FStat[stat_eq_mod, 1], TEquipment(theItem).attack);
   inc(FStat[stat_eq_mod, 2], TEquipment(theItem).defense);
   inc(FStat[stat_eq_mod, 3], TEquipment(theItem).mind);
   inc(FStat[stat_eq_mod, 4], TEquipment(theItem).speed);}
end;

procedure TRpgHero.equip(id, slot: word);
var
   theItem: TRpgItem;
   dummy: TItemType;
begin
   theItem := TRpgItem.newItem(id, 1);
{   assert(theItem is TEquipment);
   dummy := theItem.template.itemType;
   if not (theItem.template.usableBy[FTemplate.id]) then
      Exit;

   if (dummy = weaponItem) and (theItem.template.twoHanded) then
   begin
      unequip(byte(weaponItem) - 1);
      unequip(byte(shieldItem) - 1);
      FEquipment[byte(weaponItem)] := theItem;
      FEquipment[byte(shieldItem)] := theItem;
   end
   else begin
      unequip(byte(dummy) - 1);
      FEquipment[slot] := theItem;
   end;
   GParty.inventory.Remove(id, 1);
   inc(FStat[stat_eq_mod, 1], TEquipment(theItem).attack);
   inc(FStat[stat_eq_mod, 2], TEquipment(theItem).defense);
   inc(FStat[stat_eq_mod, 3], TEquipment(theItem).mind);
   inc(FStat[stat_eq_mod, 4], TEquipment(theItem).speed);}
end;

procedure TRpgHero.unequip(id: byte);
var
   I: Integer;
begin
   if id = 5 then
   begin
      for I := 0 to 4 do
         unequip(i);
      //end for
   //end if
      Exit;
   end;

   inc(id);
   if FEquipment[id] <> nil then
   begin
{      GParty.inventory.Add(FEquipment[id]);
      dec(FStat[stat_eq_mod, 1], TEquipment(FEquipment[id]).attack);
      dec(FStat[stat_eq_mod, 2], TEquipment(FEquipment[id]).defense);
      dec(FStat[stat_eq_mod, 3], TEquipment(FEquipment[id]).mind);
      dec(FStat[stat_eq_mod, 4], TEquipment(FEquipment[id]).speed);
      if (id in [1, 2]) and (FEquipment[id].template.twoHanded) then
         FEquipment[3 - id] := nil;
      FEquipment[id] := nil;}
   end;
end;

function TRpgHero.equipped(id: smallint): boolean;
var
   i: Integer;
begin
   result := false;
   for I := low(FEquipment) to high(FEquipment) do
      if (FEquipment[i] <> nil) and (FEquipment[i].template.id = id) then
         result := true;
      //end if
   //end for
end;

procedure TRpgHero.fullheal;
var
  I: Integer;
begin
   FHitPoints := FMaxHitPoints;
   FManaPoints := FMaxManaPoints;
   for I := 1 to high(FCondition) do
      FCondition[i] := false;
   //end for
end;

function TRpgHero.getCondition(x: integer): boolean;
begin
   if x in [1..high(FCondition)] then
      result := FCondition[x]
   else result := false;
end;

function TRpgHero.getEquipment(which: byte): word;
begin
   if assigned(FEquipment[which]) then   
      result := FEquipment[which].template.id
   else result := 0;
end;

function TRpgHero.getExpNeeded: integer;
begin
   if FLevel = 50 then
      result := -1
   else
      result := FExpTable[FLevel + 1] - FExpTotal;
   //end if
end;

function TRpgHero.getHighCondition: byte;
var
   i: word;
   highPriority: byte;
begin
   highPriority := 0;
   result := 0;
   for I := high(FCondition) downto 1 do
      if (FCondition[i]) and (GDatabase.conditions[i].priority >= highPriority) then
         result := i;
      //end if
   //end for
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

function TRpgHero.getSkill(id: word): boolean;
begin
   if (id > 0) and (id < high(FSkill)) then
      result := FSkill[id]
   else
      result := FSkill[id];
end;

function TRpgHero.getStat(which: integer): smallint;
var i: TStatComponents;
begin
   result := 0;
   for i := low(TStatComponents) to high(TStatComponents) do
      inc(result, FStat[i, which]);
   if result < 0 then
      result := 0;
   //end if
end;

function TRpgHero.inParty: boolean;
var
  I: Integer;
begin
   result := false;
   for I := 1 to MAXPARTYSIZE do
{      if GParty[i] = self then
         result := true;
      //end if
   //end for} ;
end;

procedure TRpgHero.setCondition(x: integer; const value: boolean);
begin
   if (x = CTN_DEAD) and (value = true)then
      self.die
   else if x in [1..high(FCondition)] then
      FCondition[x] := value;
   //end if
end;

procedure TRpgHero.setExp(value: integer);
begin
   FExpTotal := value;
   if FExpTotal < 0 then
      FExpTotal := 0
   else if FExpTotal > MAXEXP then
      FExpTotal := MAXEXP;
   if (FLevel < MAXLEVEL) then
      if (expNeeded <= 0) then
         updateLevel(true)
      else if (expNeeded > FExpTable[FLevel + 1] - FExpTable[FLevel]) then
         updateLevel(false);
      //end if
   //end if
end;

procedure TRpgHero.setHP(value: integer);
begin
   if FCondition[CTN_DEAD] then
      Exit;

   FHitPoints := value;
   if FHitPoints < 0 then
      FHitPoints := 0;
   if FHitPoints = 0 then
   begin
{      if GParty.deathPossible = true then
         self.die
      else
         inc(FHitPoints);
      //end if}
   end else if FHitPoints > self.maxHp then
      FHitPoints := maxHp;
   //end if
end;

procedure TRpgHero.setLevel(const value: byte);
var
   dummy: boolean;
   oldlevel: byte;
begin
   if FLevel = value then
      Exit;

   dummy := FLevel < value;
   oldlevel := FLevel;
   FLevel := value;
   if FLevel = 0 then
      inc(FLevel)
   else if FLevel > MAXLEVEL then
      FLevel := MAXLEVEL;
   //end if

   FExpTotal := FExpTable[FLevel];
   if dummy then
      levelAdjustUp(oldlevel)
   else
      levelAdjustDown(oldlevel);
   //end if
end;


procedure TRpgHero.levelAdjustDown(before: byte);
var i: word;
begin
{   for I := 1 to FTemplate.skills do
      if (FTemplate.skill[i].level > FLevel) and (FTemplate.skill[i].level <= before) then
         FSkill[FTemplate.skill[i].id] := false;
      //end if
   //end for
   levelStatAdjust;}
end;

procedure TRpgHero.levelAdjustUp(before: byte);
var i: word;
begin
{   for I := 1 to FTemplate.skills do
      if (FTemplate.skill[i].level <= FLevel) and (FTemplate.skill[i].level > before) then
         FSkill[FTemplate.skill[i].id] := true;
      //end if
   //end for
   levelStatAdjust;}
end;

procedure TRpgHero.levelStatAdjust;
begin
{   FMaxHitPoints := FTemplate.statCurve[stat_hp, FLevel];
   FMaxManaPoints := FTemplate.statCurve[stat_mp, FLevel];
   FStat[stat_base, 1] := FTemplate.statCurve[stat_str, FLevel];
   FStat[stat_base, 2] := FTemplate.statCurve[stat_def, FLevel];
   FStat[stat_base, 3] := FTemplate.statCurve[stat_mind, FLevel];
   FStat[stat_base, 4] := FTemplate.statCurve[stat_agi, FLevel];}
end;

procedure TRpgHero.loseLevel;
begin
   dec(FLevel);
   levelAdjustDown(FLevel + 1);
end;

function TRpgHero.potentialStat(item, slot, whichStat: word): word;
{var
   theItem: TItem;}
begin
{   theItem := GDatabase.item[item];
   assert((item = 0) or (theItem.itemType in [weaponItem, shieldItem, armorItem, helmetItem, accessoryItem]));
   result := self.stat[whichStat];
   if self.FEquipment[slot] <> nil then
      result := result - FEquipment[slot].template.stat[whichStat];
   if (theItem <> nil) and (theItem.twoHanded) and (FEquipment[3 - slot] <> nil) then
      result := result - FEquipment[3 - slot].template.stat[whichStat];
   if item <> 0 then
      result := result + GDatabase.item[item].stat[whichStat];}
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
   //end if
end;

procedure TRpgHero.setPortrait(filename: string; index: byte);
var dummy: string;
begin
   if not (index in [1..16]) then
      Exit;
   dummy := filename;
{   findGraphic(dummy, 'faceset');
   if dummy = '' then
      Exit;

   FFaceName := filename;
   FFaceNum := index;
   GGameEngine.loadPortrait(filename);}
end;

procedure TRpgHero.setSkill(id: word; value: boolean);
begin
   if (id > 0) and (id < high(FSkill)) then
      FSkill[id] := value;
   //end if
end;

procedure TRpgHero.setSprite(filename: string; index: byte; translucent: boolean);
var
   dummy: string;
begin
   if not (index in [1..8]) then
      Exit;
   dummy := filename;
{   findGraphic(dummy, 'charset');
   if dummy = '' then
      Exit;

   FSprite := filename;
   FSpriteIndex := index;
   FTransparent := translucent;
   if GParty[1] = self then
   with GGameEngine.character[0] as TCharSprite do
      update(sprite, spriteIndex, translucent);}
end;

procedure TRpgHero.setStat(which: integer; value: smallint);
var
   dummy: integer;
begin
   dummy := self.stat[which] - value;
   inc(FStat[stat_bonus, which], dummy);
end;

procedure TRpgHero.setTransparent(const Value: boolean);
begin
   FTransparent := Value;
{   if GParty[1] = self then
   with GGameEngine.character[0] as TCharSprite do
      update(sprite, spriteIndex, translucency >= 3);}
end;

function TRpgHero.takeDamage(power: word; defense, mDefense, variance: byte): word;
begin
{   GParty.deathPossible := true;
   hp := hp - power;
   result := power;}
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

end.
