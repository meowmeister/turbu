unit rm2_turbu_characters;
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
   LDB, hero_data, turbu_characters, turbu_defs, conversion_table;

type
   T2k2StatBlock = class helper for TStatBlock
   public
      constructor convert(base: TRm2CharClass; block: byte; statSet: TStatSet); overload;
      constructor convert(base: THeroRecord; block: byte; statSet: TStatSet); overload;
   end;

   T2k2BattleCommand = class helper for turbu_characters.TBattleCommand
   public
      constructor convert(base: ldb.TBattleCommand; id: integer);
   end;

   T2kCharClass = class helper for TClassTemplate
   public
      constructor convert(base: TRm2CharClass; statSet: TStatSet); overload;
      constructor convert(base: THeroRecord; baseDB: TLcfDataBase; statSet: TStatSet); overload;
      procedure loadEq(base: THeroRecord);
   end;

   T2k2Hero = class helper for THeroTemplate
   public
      constructor convert(base: THeroRecord; classTable: TConversionTable; baseDB: TLcfDataBase; statSet: TStatSet);
   end;

   function isEmpty(data: THeroRecord): boolean; overload;
   function isEmpty(data: TRm2CharClass): boolean; overload;

implementation
uses
   Classes, Types, SysUtils,
   formats, turbu_database, turbu_constants, turbu_items, turbu_skills,
   rm2_turbu_skills, rm2_turbu_database;

{ T2k2StatBlock }

constructor T2k2StatBlock.Convert(base: TRm2CharClass; block: byte; statSet: TStatSet);
var
   blocksize: byte;
   blockPtr: cardinal;
   localArray: array of word;
   i: integer;
begin
   assert(GProjectFormat = pf_2k3);
   blocksize := 99;
   self.Create(blocksize, statSet);
   setLength(localArray, blocksize);
   blockPtr := cardinal(base.statBlock) + (blocksize * 2 * block);
   move(pointer(blockPtr), localArray[0], blocksize * 2); //2 bytes/element
   for I := 0 to blocksize - 1 do
      self.block[i] := localArray[i];
end;

constructor T2k2StatBlock.Convert(base: THeroRecord; block: byte; statSet: TStatSet);
var
   blocksize: byte;
   blockPtr: cardinal;
   localArray: array of word;
   i: integer;
begin
   case GProjectFormat of
      pf_2k: blocksize := 50;
      pf_2k3: blocksize := 99;
      else
      begin
         blocksize := 0;
         assert(false);
      end;
   end;
   self.Create(blocksize, statSet);
   setLength(localArray, blocksize);
   blockPtr := cardinal(base.statBlock) + (blocksize * 2 * block);
   move(pointer(blockPtr)^, localArray[0], blocksize * 2); //2 bytes/element
   for I := 0 to blocksize - 1 do
      self.block[i] := localArray[i];
end;

{ T2kCharClass }

constructor T2kCharClass.convert(base: TRm2CharClass; statSet: TStatSet);
var
   i: integer;
   resistVal: integer;
   newstat: TStatBlock;
begin
   self.Create;
   self.id := base.id;
   self.clsName := unicodeString(base.name);
   self.battleSprite := base.spriteIndex;
   for i := 1 to COMMAND_COUNT do
   begin
      self.command[i] := base.battleCommand[i] - 1;
      if self.command[i] <> -1 then
         self.commands := self.commands + 1
      else Break;
   end;
   for i := 1 to STAT_COUNT do
   begin
      newstat := TStatBlock.convert(base, i, statSet);
      statSet.add(newstat);
      self.statblock[i] := newstat;
   end;
   self.expFunc := 'calcExp2k';
   self.expVars[1] := base.expStandard;
   self.expVars[2] := base.expAddition;
   self.expVars[3] := base.expCorrection;
   for I := 1 to base.skills do
      self.skillset.add(TSkillGainInfo.convert(base.skill[i]));

   for I := 1 to high(self.resist) do
   begin
      resistVal := -(base.dtypeModifier[i] - 100);
      if resistVal <> GLcfDatabase.attribute[i].rate[3] then
         self.addResist(point(i, resistVal));
   end;

   for I := 1 to high(self.condition) do
   begin
      resistVal := -(base.conditionModifier[i] - 100);
      if resistVal <> 0 then
         self.addCondition(point(i, resistVal));
   end;

   case base.dualWield of
      true: self.dualWield := ws_dual;
      false: self.dualWield := ws_shield;
   end;
   self.staticEq := base.staticEq;
   self.strongDef := base.strongDef;
end;

constructor T2kCharClass.convert(base: THeroRecord; baseDB: TLcfDataBase; statSet: TStatSet);
var
   i: integer;
   resistVal: integer;
   newstat: TStatBlock;
begin
   self.Create;
   self.id := base.id;
   if (base.charClass <> '') and (base.charClass <> 'None') then
      self.clsName := unicodeString(base.charClass)
   else self.clsName := unicodeString(base.name) + ' Class';
   self.mapSprite := format('%s %d', [string(base.sprite), base.spriteIndex]);
   self.translucent := base.transparent;
   self.battleSprite := base.battleSprite;
   if base.portrait <> '' then
   begin
      self.portrait := string(base.portrait);
      self.portraitIndex := base.portraitIndex;
   end
   else self.portraitIndex := -1;
   if (GProjectFormat = pf_2k) or (base.classNum = 0) then
   begin
      for I := 1 to 4 do
         self.command[i] := i;
      for I := 5 to COMMAND_COUNT do
         self.command[i] := -1;
      self.commands := 4;
      if base.hasSkillName then
         self.command[2] := GDatabase.command.IndexOf(string(base.skillName));
   end
   else begin
      for I := 1 to COMMAND_COUNT do
      with baseDB.charClass[base.classNum] do
         self.command[i] := command[i];
   end;
   for i := 1 to STAT_COUNT do
   begin
      newstat := TStatBlock.convert(base, i, statSet);
      statSet.add(newstat);
      self.statblock[i] := newstat;
   end;
   if GProjectFormat = pf_2k then
      self.expFunc := 'calcExp2k'
   else self.expFunc := 'calcExp2k';
   self.expVars[1] := base.expStandard;
   self.expVars[2] := base.expAddition;
   self.expVars[3] := base.expCorrection;
   for I := 1 to base.skills do
      self.skillset.add(TSkillGainInfo.convert(base.skill[i]));

   for I := 1 to GLcfDatabase.attributes do
   begin
      resistVal := base.dtypeModifier[i] + 1;
      resistVal := -(GLcfDatabase.attribute[i].rate[resistVal] - 100);
      if resistVal <> GLcfDatabase.attribute[i].rate[3] then
         self.addResist(point(i, resistVal));
   end;

   for I := 1 to GLcfDatabase.conditions do
   begin
      resistVal := base.conditionModifier[i] + 1;
      resistVal := -(GLcfDatabase.condition[i].chance[resistVal] - 100);
      if resistVal <> 0 then
         self.addCondition(point(i, resistVal));
   end;

   //skip eq and do later after it's loaded
   case base.dualWield of
      true: self.dualWield := ws_dual;
      false: self.dualWield := ws_shield;
   end;
   self.staticEq := base.staticEq;
   self.strongDef := base.strongDef;
   self.unarmedAnim := base.unarmedAnim;
end;

procedure T2kCharClass.loadEq(base: THeroRecord);
var
   i: byte;
begin
   self.eq[1] := base.initialEq[1];
   for I := 2 to TOTAL_SLOTS do
      self.eq[i] := base.initialEq[i];
end;

{ T2k2Hero }

constructor T2k2Hero.convert(base: THeroRecord; classTable: TConversionTable; baseDB: TLcfDataBase; statSet: TStatSet);
begin
   inherited convert(base, baseDB, statSet);
   self.name := unicodeString(base.name);
   self.title := unicodeString(base.charClass);
   self.minLevel := base.startLevel;
   self.maxLevel := base.maxLevel;
   self.guest := base.computerControlled;
   if base.classNum = 0 then
      self.charClass := classTable[base.id];
   self.canCrit := base.canCrit;
   self.critRate := base.critRate;
end;

{ T2k2BattleCommand }

constructor T2k2BattleCommand.convert(base: ldb.TBattleCommand; id: integer);
const
   table: array[0..6] of TCommandStyle = (cs_weapon, cs_skill, cs_skill, cs_defend, cs_item, cs_flee, cs_special);
begin
   self.id := id;
   self.name := unicodeString(base.name);
   self.style := table[ord(base.style)];
   case base.style of
      cs_anyskill: self.value := -1;
//      cs_skillgroup: self.value := table[ord(base.style)];
      else self.value := id;
   end;
end;

{ Classless }

function isEmpty(data: THeroRecord): boolean;
begin
   with data do
   begin
      result := (sprite = '') and (portrait = '') and (filename = '') and (name = '') and ((charClass = '') or (charClass = 'None'));
      result := result and (skills = 0) and (conditionModifiers = 0) and (dtypeModifiers = 0);
   end;
end;

function isEmpty(data: TRm2CharClass): boolean;
begin
   with data do
      result := (name = '') and (spriteIndex = 0) and (skills = 0) and (dtypeModifiers = 0) and (conditionModifiers = 0);
end;

end.
