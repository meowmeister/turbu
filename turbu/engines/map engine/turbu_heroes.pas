unit turbu_heroes;

interface
uses
   types,
   turbu_classes, turbu_containers, turbu_defs, turbu_mapchars,
   turbu_characters, turbu_map_sprites;

const
   MAXPARTYSIZE = 4;
   MAXGOLD = 999999;

type
   TStatComponents = (stat_base, stat_bonus, stat_eq_mod);

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

      FDualWield: TWeaponStyle;
      FStaticEq: boolean;
      FComputerControlled: boolean;
      FStrongDefense: boolean;

      FExpTable: array[1..50] of integer;
      FExpTotal: integer;
//      FEquipment: array[1..5] of TRpgItem;
      FStat: array[TStatComponents, 1..4] of smallint;
      FConditionModifier: array of integer;
      FCondition: array of boolean;
      FDtypeModifiers: array of integer;
      FHitPoints: integer;
      FManaPoints: integer;
      FMaxHitPoints: integer;
      FMaxManaPoints: integer;
      FHpModifier: integer;
      FMpModifier: integer;
      FSkill: array of boolean;

      FLevelUpdated: boolean;
      function countSkills: word;
      procedure die;
      procedure gainLevel;
      function getCondition(x: integer): boolean;
      function getEquipment(which: byte): word;
      function getExpNeeded: integer;
      function getHighCondition: byte;
      function getLevelUpdatedStatus: boolean;
      function getMHp: integer;
      function getMMp: integer;
      function getSkill(id: word): boolean;
      function getStat(which: integer): smallint;
      procedure levelAdjustDown(before: byte);
      procedure levelAdjustUp(before: byte);
      procedure levelStatAdjust;
      procedure loseLevel;
      procedure setCondition(x: integer; const value: boolean);
      procedure setExp(value: integer);
      procedure setHP(value: integer);
      procedure setLevel(const value: integer);
      procedure setMaxHp(const Value: integer);
      procedure setMaxMp(const Value: integer);
      procedure setMP(value: integer);
      procedure setSkill(id: word; value: boolean);
      procedure setStat(which: integer; value: smallint);
      procedure setTransparent(const Value: boolean);
      procedure updateLevel(const gain: boolean);

   protected
      class function templateClass: TDatafileClass; override;
   public
      constructor Create(base: TClassTemplate);
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

      property name: string read FName write FName;
      property sprite: string read FSprite;
      property transparent: boolean read FTransparent write setTransparent;
      property charClass: string read FClass write FClass;
      property level: integer read FLevel write setLevel;
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
      property dualWield: TWeaponStyle read FDualWield;
      property highCondition: byte read getHighCondition;
   end;

   TRpgParty = class(TRpgCharacter)
   private
      //data
      FCash: integer;
      FParty: array[1..MAXPARTYSIZE] of TRpgHero;
//      FInventory: TRpgInventory;

      //flags
      FLevelNotify: boolean;
      FDeathPossible: boolean;

      function getMap: word;
      procedure setY(const Value: word);
      procedure setX(const Value: word);
      function getFacing: byte;
      procedure setFacing(const Value: byte);
      function getHero(x: byte): TRpgHero;
      procedure setHero(x: byte; value: TRpgHero);
      function empty: boolean;
   protected
      function getX: word; override;
      function getY: word; override;
      function getTranslucency: byte; override;
      procedure setTranslucency(const Value: byte); override;
      procedure doFlash(r, g, b, power: byte; time: cardinal); override;
      function getBase: TMapSprite; override;
   public
      constructor Create;
      destructor Destroy; override;
      procedure addItem(const id, number: word);
      procedure removeItem(const id, number: word);
      procedure addExp(const id: smallint; number: integer);
      procedure removeExp(const id: smallint; number: integer);
      procedure addLevels(const id: smallint; number: integer);
      procedure removeLevels(const id: smallint; number: integer);
      procedure sort;
      procedure ChangeSprite(name: string; index: integer; oldSprite: TMapSprite); override;

      function takeDamage(power: word; defense, mDefense, variance: byte): word;
      function openSlot: byte;
      function size: byte;
      function indexOf(who: TRpgHero): integer;

      property money: integer read FCash write FCash;
//      property inventory: TRpgInventory read FInventory write FInventory;
      property hero[x: byte]: TRpgHero read getHero write setHero; default;
      property levelNotify: boolean read FLevelNotify write FLevelNotify;
      property deathPossible: boolean read FDeathPossible write FDeathPossible;
      property facing: byte read getFacing write setFacing;
      property x: word read getX write setX;
      property y: word read getY write setY;
      property map: word read getMap;
   end;

   TPartyEvent = procedure(hero: TRpgHero; party: TRpgParty) of object;
   TSkillBoolFunc = function(Character: TRpgHero; Level, unused2, unused3, unused4: integer): boolean of object;
   TSkillNumFunc = function(character: TRpgHero; int1, int2, int3, int4: integer): integer of object;
   TSkillDualNumFunc = function(character: TRpgHero; int1, int2, int3, int4: integer): TPoint of object;

const
   CTN_DEAD = 1;

implementation
uses
   turbu_database, commons,
   upsRuntime;

{ TRpgHero }

constructor TRpgHero.Create(base: TClassTemplate);
var
  I: Integer;
//  dummy: byte;
  calc: TExpCalcEvent;
  template: THeroTemplate absolute base;
begin
   inherited Create(base);
   if base = nil then
      Exit;

   FName := template.name;
   FClass := GDatabase.charClass[template.charClass].clsName;
   FSprite := template.MapSprite;
   FTransparent := template.translucent;
   FExpTable[1] := 0;

   calc := TExpCalcEvent(GScriptEngine.GetExecMethod(template.expFunc));
   if assigned(calc) then
      for I := 2 to 50 do
         FExpTable[i] := calc(i, template.expVars[1], template.expVars[2], template.expVars[3], template.expVars[4]);
   if template.canCrit then
      FCritRate := template.critRate
   else FCritRate := 0;
   FFaceName := template.portrait;
   FFaceNum := template.portraitIndex;
   FDualWield := template.dualWield;
   FStaticEq := template.staticEq;
   setLength(FSkill, GDatabase.skill.count + 1);
{   for I := 1 to template.skillset.count do
      if template.skillset[i].le <= FLevel then
         FSkill[template.skill[i].id] := true;
      //end if
   //end for
   FLevel := 1;
   self.levelAdjustUp(0);
   level := template.startLevel;
   FExpTotal := FExpTable[FLevel];
   for I := low(FEquipment) to high(FEquipment) do
      if template.initialEq[i] <> 0 then
         self.equip(template.initialEq[i]);
   //end for
   i := template.conditionModifiers;
   setLength(FConditionModifier, i);
   setLength(FCondition, i);
   for I := 0 to high(FCondition) do
   begin
      dummy := template.conditionModifier[i];
      FConditionModifier[i] := GDatabase.condition[i].chance[dummy]; //fix this
      FCondition[i] := false;
   end;
   FHitPoints := maxHp;
   FManaPoints := maxMp;}
end;

class function TRpgHero.templateClass: TDatafileClass;
begin
   result := TClassTemplate;
end;

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

destructor TRpgHero.Destroy;
{var
   i: integer;
   j: integer;}
begin
{   for i := 1 to 5 do
   begin
      for j := i + 1 to 5 do
         if FEquipment[j] = FEquipment[i] then
            FEquipment[j] := nil;
         //end if
      FEquipment[i].free;
   end; }
   inherited Destroy;
end;

procedure TRpgHero.die;
begin
   FCondition[CTN_DEAD] := true;
   FHitPoints := 0;
end;

procedure TRpgHero.equip(id: word);
{var
   theItem: TRpgItem;
   dummy: TItemType; }
begin
   if not IsBetween(id, 0, GDatabase.items) then
      Exit;

//   theItem := TRpgItem.newItem(id, 1);
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
{var
   theItem: TRpgItem;
   dummy: TItemType; }
begin
//   theItem := TRpgItem.newItem(id, 1);
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
      Exit;
   end;

{   inc(id);
   if FEquipment[id] <> nil then
   begin
      GParty.inventory.Add(FEquipment[id]);
      dec(FStat[stat_eq_mod, 1], TEquipment(FEquipment[id]).attack);
      dec(FStat[stat_eq_mod, 2], TEquipment(FEquipment[id]).defense);
      dec(FStat[stat_eq_mod, 3], TEquipment(FEquipment[id]).mind);
      dec(FStat[stat_eq_mod, 4], TEquipment(FEquipment[id]).speed);
      if (id in [1, 2]) and (FEquipment[id].template.twoHanded) then
         FEquipment[3 - id] := nil;
      FEquipment[id] := nil;
   end;}
end;

function TRpgHero.equipped(id: smallint): boolean;
{var
   i: Integer;}
begin
   result := false;
{   for I := low(FEquipment) to high(FEquipment) do
      if (FEquipment[i] <> nil) and (FEquipment[i].template.id = id) then
         result := true;
}
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
{   if assigned(FEquipment[which]) then
      result := FEquipment[which].template.id
   else result := 0; }
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
//var i: TStatComponents;
begin
   result := 0;
{   for i := low(TStatComponents) to high(TStatComponents) do
      inc(result, FStat[i, which]); }
   if result < 0 then
      result := 0;
   //end if
end;

function TRpgHero.inParty: boolean;
{var
  I: Integer;}
begin
   result := false;
{   for I := 1 to MAXPARTYSIZE do
      if GParty[i] = self then
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
 {  if FExpTotal < 0 then
      FExpTotal := 0
   else if FExpTotal > MAXEXP then
      FExpTotal := MAXEXP;
   if (FLevel < MAXLEVEL) then
      if (expNeeded <= 0) then
         updateLevel(true)
      else if (expNeeded > FExpTable[FLevel + 1] - FExpTable[FLevel]) then
         updateLevel(false);
      //end if
   //end if }
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

procedure TRpgHero.setLevel(const value: integer);
var
   dummy: boolean;
   oldlevel: byte;
begin
   if FLevel = value then
      Exit;

   dummy := FLevel < value;
   oldlevel := FLevel;
   FLevel := value;
{   if FLevel = 0 then
      inc(FLevel)
   else if FLevel > MAXLEVEL then
      FLevel := MAXLEVEL;
   //end if }

   FExpTotal := FExpTable[FLevel];
   if dummy then
      levelAdjustUp(oldlevel)
   else
      levelAdjustDown(oldlevel);
   //end if
end;


procedure TRpgHero.levelAdjustDown(before: byte);
//var i: word;
begin
{   for I := 1 to Template.skills do
      if (Template.skill[i].level > FLevel) and (Template.skill[i].level <= before) then
         FSkill[Template.skill[i].id] := false;
      //end if
   //end for
   levelStatAdjust;}
end;

procedure TRpgHero.levelAdjustUp(before: byte);
//var i: word;
begin
{   for I := 1 to Template.skills do
      if (Template.skill[i].level <= FLevel) and (Template.skill[i].level > before) then
         FSkill[Template.skill[i].id] := true;
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
begin
   inc(FStat[stat_bonus, which], self.stat[which] - value);
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

{ TRpgParty }

procedure TRpgParty.addExp(const id: smallint; number: integer);
var
  I: Integer;
//  dummy: TRpgHero;
begin
   if id = -1 then
   begin
      for I := 1 to MAXPARTYSIZE do
{         if self[i] <> GCurrentEngine.hero[0] then
         begin
            dummy := GCurrentEngine.hero[self[i].template.id];
            dummy.exp := dummy.exp + number;
         end;}
      //end for
   //end if
   end else
      self[id].exp := self[id].exp + number;
   //end if
end;

procedure TRpgParty.addItem(const id, number: word);
begin
//   FInventory.Add(id, number);
end;

procedure TRpgParty.addLevels(const id: smallint; number: integer);
var
   I: Integer;
//   dummy: TRpgHero;
begin
   if id = -1 then
   begin
      for I := 1 to MAXPARTYSIZE do
{         if self[i] <> GCurrentEngine.hero[0] then
         begin
            dummy := GCurrentEngine.hero[self[i].template.id];
            dummy.level := dummy.level + number;
         end;
      //end for};
   //end if
   end else
      self[id].level := self[id].level + number;
   //end if
end;

procedure TRpgParty.ChangeSprite(name: string; index: integer; oldSprite: TMapSprite);
begin
{   if assigned(GGameEngine.character[0]) then
   with GGameEngine.character[0] do
      update(filename, index, translucency >= 3);
   //end WITH}
end;

constructor TRpgParty.Create;
begin
//   FInventory := TRpgInventory.Create(database);
   FLevelNotify := true;
end;

destructor TRpgParty.Destroy;
begin
//   FInventory.free;
   inherited Destroy;
end;

function TRpgParty.getBase: TMapSprite;
begin
//   result := GGameEngine.currentParty;
end;

function TRpgParty.getFacing: byte;
begin
   result := 0;
{   case GGameEngine.currentParty.facing of
      facing_up: result := 8;
      facing_right: result := 6;
      facing_down: result := 2;
      facing_left: result := 4;
   end;}
end;

function TRpgParty.getHero(x: byte): TRpgHero;
begin
   if (x = 0) or (x > MAXPARTYSIZE) or (FParty[x] = nil) then
      result := nil //GCurrentEngine.hero[0]
   else
      result := FParty[x];
end;

function TRpgParty.getMap: word;
begin
//   result := GGameEngine.currentMap.mapID;
end;

function TRpgParty.getTranslucency: byte;
begin
   if empty then
      result := 0
   else result := inherited getTranslucency;
end;

function TRpgParty.getX: word;
begin
{   if assigned(GGameEngine.currentParty) then
      result := GGameEngine.currentParty.location.x
   else result := 0;}
end;

function TRpgParty.getY: word;
begin
{   if assigned(GGameEngine.currentParty) then
      result := GGameEngine.currentParty.location.y
   else result := 0;}
end;

function TRpgParty.indexOf(who: TRpgHero): integer;
var i: byte;
begin
   result := -1;
   for i := 1 to MAXPARTYSIZE do
      if self[i] = who then
         result := i;
      //end if
   //end for
end;

function TRpgParty.empty: boolean;
var i: byte;
begin
   result := true;
   for i := 1 to MAXPARTYSIZE do
{      if self[i] <> GCurrentEngine.hero[0] then
         result := false;
      //end if};
   //end for
end;

procedure TRpgParty.doFlash(r, g, b, power: byte; time: cardinal);
begin
{   if assigned(GGameEngine.character[0]) then
      GGameEngine.character[0].flash(r, g, b, power, time);}
end;

function TRpgParty.size: byte;
var i: byte;
begin
   result := 0;
   for i := 1 to MAXPARTYSIZE do
{      if (self[i] <> GCurrentEngine.hero[0]) then
         inc(result)};
      //end if
   //end for
end;

function TRpgParty.openSlot: byte;
var i: byte;
begin
   i := 1;
{   while (self[i] <> GCurrentEngine.hero[0]) and (i <= MAXPARTYSIZE) do
      inc(i);}
   if i > MAXPARTYSIZE then
      result := 0
   else result := i;
end;

procedure TRpgParty.removeExp(const id: smallint; number: integer);
var
   I: Integer;
//   dummy: TRpgHero;
begin
   if id = -1 then
   begin
      for I := 1 to MAXPARTYSIZE do
{         if self[i] <> GCurrentEngine.hero[0] then
         begin
            dummy := GCurrentEngine.hero[self[i].template.id];
            dummy.exp := dummy.exp - number;
         end;};
      //end for
   end else
      self[id].exp := self[id].exp - number;
   //end if
end;

procedure TRpgParty.removeItem(const id, number: word);
begin
//   FInventory.Remove(id, number);
end;

procedure TRpgParty.removeLevels(const id: smallint; number: integer);
var
   I: Integer;
//   dummy: TRpgHero;
begin
   if id = -1 then
   begin
      for I := 1 to MAXPARTYSIZE do
{         if self[i] <> GCurrentEngine.hero[0] then
         begin
            dummy := GCurrentEngine.hero[self[i].template.id];
            dummy.level := dummy.level - number;
         end;};
      //end for
   //end if
   end else
      self[id].level := self[id].level - number;
   //end if
end;

procedure TRpgParty.setFacing(const Value: byte);
var dummy: TMapSprite;
begin
//   dummy := GGameEngine.currentParty;
   case value of
      8: dummy.facing := facing_up;
      6: dummy.facing := facing_right;
      4: dummy.facing := facing_left;
      2: dummy.facing := facing_down;
   end;
end;

procedure TRpgParty.setHero(x: byte; value: TRpgHero);
begin
   if (x = 0) or (x > MAXPARTYSIZE) then
      Exit
   else FParty[x] := value;
end;

procedure TRpgParty.setTranslucency(const Value: byte);
begin
   if not empty then
      inherited setTranslucency(value);
end;

procedure TRpgParty.setX(const Value: word);
//var place: TPoint;
begin
{   place := GGameEngine.currentParty.location;
   GGameEngine.currentParty.location := point(value, place.Y);}
end;

procedure TRpgParty.setY(const Value: word);
//var place: TPoint;
begin
{   place := GGameEngine.currentParty.location;
   GGameEngine.currentParty.location := point(place.x, value);}
end;

procedure TRpgParty.sort;
var
   i, j: byte;
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
   //end if
end;

function TRpgParty.takeDamage(power: word; defense, mDefense,
  variance: byte): word;
var i: byte;
begin
   result := 0;
   for i := 1 to MAXPARTYSIZE do
{      if self[i] <> GCurrentEngine.hero[0] then
         result := FParty[i].takeDamage(power, defense, mdefense, variance);
      //end if};
   //end for
end;

end.
