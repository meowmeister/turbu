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

unit EB_Characters;

interface
uses
   EventBuilder;

type
   [UsesUnit('Characters')]
   TEBCharacterObject = class(TEBObject);

   TEBMoney = class(TEBCharacterObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBInventory = class(TEBCharacterObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBChangeParty = class(TEBCharacterObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBExperience = class(TEBCharacterObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBLevel = class(TEBCharacterObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBStats = class(TEBCharacterObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBSkills = class(TEBCharacterObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBEquipment = class(TEBCharacterObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBChangeHP = class(TEBCharacterObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBChangeMP = class(TEBCharacterObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBChangeStatus = class(TEBCharacterObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBFullHeal = class(TEBCharacterObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBTakeDamage = class(TEBCharacterObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBHeroName = class(TEBCharacterObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBHeroClass = class(TEBCharacterObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBHeroSprite = class(TEBCharacterObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBHeroPortrait = class(TEBCharacterObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBVehicleSprite = class(TEBCharacterObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBTranslucency = class(TEBCharacterObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBClassChange = class(TEBObject)
   private
      function CharClassName(id: integer): string;
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBBattleCommand = class(TEBObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

const
   STATS: array[0..5] of string = ('MaxHp', 'MaxMp', 'Attack', 'Defense', 'Mind', 'Agility');
   SLOTS: array[0..5] of string = ('eq_weapon', 'eq_shield', 'eq_armor', 'eq_helmet',
                    'eq_relic', 'eq_all');

implementation
uses
   SysUtils, Classes,
   EB_RpgScript;

function GetIntScript(decider, value: integer): string;
begin
   if boolean(decider) then
      result := format('Ints[%s]', [TEBObject.IntName(value)])
   else result := intToStr(Value);
end;

{ TEBMoney }

function TEBMoney.GetNodeText: string;
const
   LINE = 'Change Money: %s $%s';
   SIGNS: array[0..2] of string = ('Add', 'Subtract', 'Set to');
begin
   result := format(LINE, [SIGNS[Values[0]], GetIntScript(values[1], values[2])]);
end;

function TEBMoney.GetScriptText: string;
const
   LINE = 'money := money %s %s;';
   LINE_EQ = 'money := %s;';
   SIGNS: array[0..1] of char = ('+', '-');
begin
   if values[0] < 2 then
      result := format(LINE, [SIGNS[Values[0]], GetIntScript(values[1], values[2])])
   else result := format(LINE_EQ, [GetIntScript(values[1], values[2])]);
end;

{ TEBInventory }

function TEBInventory.GetNodeText: string;
const LINE = 'Change Inventory: %s %s of %s';
begin
   if values[0] < 2 then
      result := format(LINE, [ADDREM[Values[0]], GetIntScript(Values[1], Values[2]),
                              ChildNode[0]])
   else result := format(LINE, [ADDREM[1], 'all', ChildNode[0]]);
end;

function TEBInventory.GetScriptText: string;
const LINE = '%sItem(%s, %s);';
begin
   if values[0] < 2 then
      result := format(LINE, [ADDREM[Values[0]], GetIntScript(Values[1], Values[2]),
                              ChildScript[0]])
   else result := format(LINE, [ADDREM[1], '-1', ChildNode[0]]);
end;

{ TEBChangeParty }

function TEBChangeParty.GetNodeText: string;
const LINE = 'Change Party: %s %s';
begin
   result := format(LINE, [ADDREM[values[0]], ChildNode[0]]);
end;

function TEBChangeParty.GetScriptText: string;
var
   param: string;
begin
   param := ChildScript[0];
   if boolean(Values[0]) then
      result := format('heroLeave(%s);', [param])
   else result := format('heroJoin(%s);', [param]);
end;

{ TEBExperience }

function TEBExperience.GetNodeText: string;
const LINE = 'Change Experience: %s, %s %s';
var
   param1, param2: string;
begin
   case Values[0] of
      0: param1 := '[All Members]';
      1: param1 := HeroName(Values[1]);
      2: param1 := format('Ints[%s]', [IntName(Values[1])]);
   end;
   if boolean(Values[2]) then
      param2 := 'Subtract'
   else param2 := 'Add';
   result := format(LINE, [param1, param2, GetIntScript(Values[3], Values[4])]);
end;

function TEBExperience.GetScriptText: string;
const
   ADD_LINE = 'AddExp(%s, %s, %s)';
   REM_LINE = 'RemoveExp(%s, %s)';
var
   param1, param2: string;
begin
   case Values[0] of
      0: param1 := '-1';
      1: param1 := intToStr(Values[1]);
      2: param1 := format('Ints[%d]', [Values[1]]);
   end;
   param2 := GetIntScript(Values[3], Values[4]);
   if boolean(Values[2]) then
      result := format(REM_LINE, [param1, param2])
   else result := format(ADD_LINE, [param1, param2, BOOL_STR[Values[5]]]);
end;

{ TEBLevel }

function TEBLevel.GetNodeText: string;
begin
   result := result + 'Change Level: ';
   case Values[0] of
      0: result := result + '[All Members]';
      1: result := result + HeroName(Values[1]);
      2: result := result + format('Ints[%s]', [IntName(Values[1])]);
   end;
   if boolean(Values[2]) then
      result := result + 'Subtract '
   else result := result + 'Add ';
   result := result + GetIntScript(Values[3], Values[4]);
end;

function TEBLevel.GetScriptText: string;
const
   ADD_LINE = 'AddLevels(%s, %s, %s)';
   REM_LINE = 'RemoveLevels(%s, %s)';
var
   param1, param2: string;
begin
   case Values[0] of
      0: param1 := '-1';
      1: param1 := intToStr(Values[1]);
      2: param1 := format('Ints[%d]', [Values[1]]);
   end;
   param2 := GetIntScript(Values[3], Values[4]);
   if boolean(Values[2]) then
      result := format(REM_LINE, [param1, param2])
   else result := format(ADD_LINE, [param1, param2, BOOL_STR[Values[5]]]);
end;

{ TEBStats }

function TEBStats.GetNodeText: string;
const LINE = 'Change Stat: (%s): %s %s';
begin
   result := format(LINE, [ChildNode[0], ADDREM[Values[0]], ChildNode[1]]);
end;

function TEBStats.GetScriptText: string;
const
   LINE = '%s := %s %s %s;';
   OPS: array[0..1] of char = ('+', '-');
var
   dummy: string;
begin
   dummy := ChildScript[0];
   result := format(LINE, [dummy, dummy, OPS[Values[0]],
                           ChildScript[1]]);
end;

{ TEBSkills }

function TEBSkills.GetNodeText: string;
const
   LINE = 'Change Skill: %s, %s %s';
   ACTION: array[0..1] of string = ('Learn', 'Forget');
begin
   result := format(LINE, [(Components[0] as TEBExpression).GetNodeText,
                           ACTION[values[0]],
                           (Components[1] as TEBExpression).GetNodeText]);
end;

function TEBSkills.GetScriptText: string;
const LINE = '%s.skill[%s] := %s;';
begin
   result := format(LINE, [(Components[0] as TEBExpression).GetScript(0),
                           (Components[1] as TEBExpression).GetScript(0),
                           BOOL_STR[values[0]]]);
end;

{ TEBEquipment }

function TEBEquipment.GetNodeText: string;
const
   LINE = 'Change Equipment: %s, %s %s';
   ACTION: array[0..1] of string = ('Equip', 'Unequip');
begin
   result := format(LINE, [(Components[0] as TEBExpression).GetNodeText,
                           ACTION[values[0]],
                           (Components[1] as TEBExpression).GetNodeText]);
end;

function TEBEquipment.GetScriptText: string;
const
   EQUIP_LINE = '%s.Equip(%s);';
   UNEQUIP_LINE = '%s.Unequip(%s);';
var
   line: string;
begin
   if boolean(Values[0]) then
      line := UNEQUIP_LINE
   else
      line := EQUIP_LINE;
   result := format(line, [(Components[0] as TEBExpression).GetScript(0),
                           (Components[1] as TEBExpression).GetScript(0)]);
end;

{ TEBChangeHP }

function TEBChangeHP.GetNodeText: string;
const
   LINE = 'Change HP: %s, %s %s';
   ACTION: array[0..1] of string = ('Gain', 'Lose');
begin
   result := format(LINE, [(Components[0] as TEBExpression).GetNodeText,
                           ACTION[values[0]],
                           (Components[1] as TEBExpression).GetNodeText]);
   if boolean(Values[1]) then
      result := result + ', Death Possible';
end;

function TEBChangeHP.GetScriptText: string;
const
   LINE = '%s.ChangeHP(%s%s, %s);';
   SIGN: array[0..1] of string = ('', '-');
begin
   result := format(LINE, [(Components[0] as TEBExpression).GetScript(0),
                           SIGN[values[0]],
                           (Components[1] as TEBExpression).GetScript(0),
                           BOOL_STR[values[1]]]);
end;

{ TEBChangeMP }

function TEBChangeMP.GetNodeText: string;
const
   LINE = 'Change MP: %s, %s %s';
   ACTION: array[0..1] of string = ('Gain', 'Lose');
begin
   result := format(LINE, [(Components[0] as TEBExpression).GetNodeText,
                           ACTION[values[0]],
                           (Components[1] as TEBExpression).GetNodeText]);
end;

function TEBChangeMP.GetScriptText: string;
const
   LINE = '%s.ChangeMP(%s%s);';
   SIGN: array[0..1] of string = ('', '-');
begin
   result := format(LINE, [(Components[0] as TEBExpression).GetScript(0),
                           SIGN[values[0]],
                           (Components[1] as TEBExpression).GetScript(0)]);
end;

{ TEBChangeStatus }

function TEBChangeStatus.GetNodeText: string;
const
   LINE = 'Change Status: %s, %s %s';
   ACTION: array[0..1] of string = ('Set', 'Clear');
begin
   result := format(LINE, [(Components[0] as TEBExpression).GetNodeText,
                           ACTION[values[0]],
                           (Components[1] as TEBExpression).GetNodeText]);
end;

function TEBChangeStatus.GetScriptText: string;
const LINE = '%s.condition[%s] := %s;';
begin
   result := format(LINE, [(Components[0] as TEBExpression).GetScript(0),
                           (Components[1] as TEBExpression).GetScript(0),
                           BOOL_STR[values[0]]]);
end;

{ TEBFullHeal }

function TEBFullHeal.GetNodeText: string;
const LINE = 'Full Heal: %s';
begin
   result := format(LINE, [(Components[0] as TEBExpression).GetNodeText]);
end;

function TEBFullHeal.GetScriptText: string;
const LINE = '%s.FullHeal;';
begin
   result := format(LINE, [(Components[0] as TEBExpression).GetScript(0)]);
end;

{ TEBTakeDamage }

function TEBTakeDamage.GetNodeText: string;
const LINE = 'Take Damage: %s, Power: %d, Def: %d, M.Def: %d, Variance: %d';
var
   subject: string;
begin
   case Values[0] of
      0: subject := 'Whole Party';
      1: subject := self.HeroName(Values[1]);
      2: subject := format('Hero[Ints[%s]]', [IntName(Values[1])]);
   end;
   result := format(LINE, [subject, values[2], values[3], values[4], values[5]]);
   if boolean(Values[6]) then
      result := format('%s, Store in Ints[%s]', [result, IntName(Values[7])]);
end;

function TEBTakeDamage.GetScriptText: string;
const LINE = '%s.TakeDamage(%d, %d, %d, %d);';
var
   subject: string;
begin
   case Values[0] of
      0: subject := 'party';
      1: subject := format('hero[%d]', [Values[1]]);
      2: subject := format('hero[Ints[%d]]', [Values[1]]);
   end;
   result := format(LINE, [subject, values[2], values[3], values[4], values[5]]);
   if boolean(Values[6]) then
      result := format('Ints[%d] := %s', [Values[7], result]);
end;

{ TEBHeroName }

function TEBHeroName.GetNodeText: string;
begin
   result := format('Rename Hero %s to %s', [HeroName(Values[0]), Text]);
end;

function TEBHeroName.GetScriptText: string;
begin
   result := format('hero[%d].name := %s;', [Values[0], QuotedStr(Text)]);
end;

{ TEBHeroClass }

function TEBHeroClass.GetNodeText: string;
begin
   result := format('Change Hero %s''s class to %s', [HeroName(Values[0]), Text]);
end;

function TEBHeroClass.GetScriptText: string;
begin
   result := format('hero[%d].CharClass := %s;', [Values[0], QuotedStr(Text)]);
end;

{ TEBHeroSprite }

function TEBHeroSprite.GetNodeText: string;
const LINE = 'Change Hero Sprite: %s, %s';
begin
   result := format(LINE, [HeroName(Values[0]), Text]);
   if boolean(Values[1]) then
      result := result + ' (Transparent)';
end;

function TEBHeroSprite.GetScriptText: string;
const LINE = 'hero[%d].SetSprite(%s, %s);';
begin
   result := format(LINE, [Values[0], QuotedStr(Text), BOOL_STR[Values[1]]]);
end;

{ TEBHeroPortrait }

function TEBHeroPortrait.GetNodeText: string;
const LINE = 'Set Portrait: %s, %s #%d';
begin
   result := format(LINE, [HeroName(Values[0]), Text, Values[1]]);
end;

function TEBHeroPortrait.GetScriptText: string;
const LINE = 'hero[%d].SetPortrait(%s, %d);';
begin
   result := format(LINE, [Values[0], QuotedStr(Text), Values[1]]);
end;

{ TEBVehicleSprite }

function TEBVehicleSprite.GetNodeText: string;
const
   LINE = 'Change Vehicle Sprite: %s, %s';
   VEHICLES: array[0..2] of string = ('Boat', 'Ship', 'Airship');
begin
   result := format(LINE, [VEHICLES[Values[0]], Text]);
end;

function TEBVehicleSprite.GetScriptText: string;
const LINE = 'vehicle[%d].SetSprite(%s);';
begin
   result := format(LINE, [Values[0], QuotedStr(Text)]);
end;

{ TEBTranslucency }

function TEBTranslucency.GetNodeText: string;
begin
   result := 'Set Party Translucency: ' + IntToStr(Values[0]);
end;

function TEBTranslucency.GetScriptText: string;
begin
   result := format('party.translucency := %d;', [Values[0]]);
end;

{ TEBClassChange }

function TEBClassChange.CharClassName(id: integer): string;
begin
   result := GetLookup(id, 'CharClasses');
end;

function TEBClassChange.GetNodeText: string;
begin
   result := format('Change Class: %s, %s', [HeroName(Values[0]), CharClassName(Values[1])]);
end;

function TEBClassChange.GetScriptText: string;
const LINE = 'ChangeClass(%d, %d, %s, %d, %d, %s);';
begin
   result := format(LINE, [Values[0], Values[1], BOOL_STR[Values[2]], Values[3],
                           Values[4], BOOL_STR[Values[5]]]);
end;

{ TEBBattleCommand }

function TEBBattleCommand.GetNodeText: string;
const LINE = 'Change Battle Commands: %s %s %s';
var
   name: string;
begin
   if Values[1] = 0 then
      name := '(All)'
   else name := GetLookup(Values[1], 'commands');
   result := format(LINE, [HeroName(Values[0]), ADDREM[Values[2]], name]);
end;

function TEBBattleCommand.GetScriptText: string;
const
   LINE = 'hero[%d].%sBattleCommand(%d);';
begin
   result := format(LINE, [Values[0], ADDREM[Values[2]], Values[1]]);
end;

initialization
   RegisterClasses([TEBMoney, TEBInventory, TEBChangeParty, TEBExperience,
                    TEBLevel, TEBStats,TEBSkills, TEBEquipment, TEBChangeHP,
                    TEBChangeStatus, TEBFullHeal, TEBTakeDamage, TEBHeroName,
                    TEBHeroClass, TEBHeroSprite, TEBHeroPortrait, TEBClassChange,
                    TEBVehicleSprite, TEBChangeMP, TEBTranslucency,
                    TEBBattleCommand]);
end.
