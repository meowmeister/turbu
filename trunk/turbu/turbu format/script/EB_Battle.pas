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

unit EB_Battle;

interface
uses
   Classes,
   EventBuilder, EB_RpgScript, turbu_battle_engine;

type
   [UsesUnit('Battles')]
   TEBBattleObject = class(TEBObject);

   [UsesUnit('Battles')]
   TEBBattleBase = class(TEBMaybeCase)
   protected
      FResults: TBattleResultSet;
   protected
      procedure SerializeProps(list: TStringList; depth: integer); override;
      procedure AssignProperty(const key, value: string); override;
   public
      property results: TBattleResultSet read FResults write FResults;
   end;

   TEBBattle = class(TEBBattleBase)
   public
      function GetNodeText: string; override;
      function GetScriptBase: string; override;
   end;

   TEBBattleEx = class(TEBBattleBase)
   public
      function GetNodeText: string; override;
      function GetScriptBase: string; override;
   end;

   TEBMonsterHP = class(TEBBattleObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBMonsterMP = class(TEBBattleObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBMonsterStatus = class(TEBBattleObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBShowMonster = class(TEBBattleObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBBattleBG = class(TEBBattleObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBEnableCombo = class(TEBBattleObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBBattleAnimation = class(TEBBattleObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBForceFlee = class(TEBBattleObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBEndBattle = class(TEBBattleObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

implementation
uses
   SysUtils, TypInfo,
   turbu_defs, EB_ObjectHelper, EB_Expressions;

{ TEBMonsterHP }

function TEBMonsterHP.GetNodeText: string;
const
   LINE = 'Change Monster %d''s HP: %s %s';
   SIGNS: array[0..2] of string = ('Add', 'Subtract', 'Set to');
var
   rvalue: string;
begin
   case values[2] of
      0: rvalue := intToStr(Values[3]);
      1: rvalue := format('Ints[%d]', [Values[3]]);
      2: rvalue := format('%d%%', [Values[3]]);
   end;
   result := format(LINE, [Values[0], SIGNS[Values[1]], rvalue]);
end;

function TEBMonsterHP.GetScriptText: string;
const
   LINE = 'monster[%d].SetHP(%s, %s;)';
   SIGN = 'monster[%d].hp %s ';
var
   rvalue: string;
begin
   case values[1] of
      0: rvalue := format(SIGN, [values[0], '+']);
      1: rvalue := format(SIGN, [values[0], '-']);
      2: rvalue := '';
   end;
   case values[2] of
      0: rvalue := rvalue + intToStr(Values[3]);
      1: rvalue := rvalue + format('Ints[%d]', [Values[3]]);
      2: rvalue := rvalue + format('percentage(monster[%d].hp, %d)', [Values[0], Values[3]]);
   end;
   result := format(LINE, [Values[0], rvalue, BOOL_STR[values[4]]]);
end;

{ TEBMonsterMP }

function TEBMonsterMP.GetNodeText: string;
const
   LINE = 'Change Monster %d''s MP: %s %s';
   SIGNS: array[0..2] of string = ('Add', 'Subtract', 'Set to');
var
   rvalue: string;
begin
   case values[2] of
      0: rvalue := intToStr(Values[3]);
      1: rvalue := format('Ints[%d]', [Values[3]]);
   end;
   result := format(LINE, [Values[0], SIGNS[Values[1]], rvalue]);
end;

function TEBMonsterMP.GetScriptText: string;
const
   LINE = 'monster[%d].mp := %s;';
   SIGN = 'monster[%d].mp %s ';
var
   rvalue: string;
begin
   case values[1] of
      0: rvalue := format(SIGN, [values[0], '+']);
      1: rvalue := format(SIGN, [values[0], '-']);
      2: rvalue := '';
   end;
   case values[2] of
      0: rvalue := rvalue + intToStr(Values[3]);
      1: rvalue := rvalue + format('Ints[%d]', [Values[3]]);
   end;
   result := format(LINE, [Values[0], rvalue]);
end;

{ TEBMonsterStatus }

function TEBMonsterStatus.GetNodeText: string;
const
   LINE = 'Change Monster %d''s Status: %s %s';
   SIGNS: array[0..1] of string = ('Set', 'Clear');
begin
   result := format(LINE, [values[0], SIGNS[Values[1]], GetLookup(values[2], 'conditions')]);
end;

function TEBMonsterStatus.GetScriptText: string;
const LINE = 'monster[%d].condition[%d] := %s;';
begin
   result := format(LINE, [Values[0], Values[2], BOOL_STR[values[1]]]);
end;

{ TEBShowMonster }

function TEBShowMonster.GetNodeText: string;
const LINE = 'Show Monster %d';
begin
   result := format(LINE, [Values[0]]);
end;

function TEBShowMonster.GetScriptText: string;
begin
   result := format('Monster[%d].Show;', [Values[0]]);
end;

{ TEBBattleBG }

function TEBBattleBG.GetNodeText: string;
begin
   result := format('Set Battle BG: %s', [self.text]);
end;

function TEBBattleBG.GetScriptText: string;
begin
   result := format('SetBattleBG(%s);', [QuotedStr(self.text)]);
end;

{ TEBEnableCombo }

function TEBEnableCombo.GetNodeText: string;
const LINE = 'Enable Combo: %s, %s, %d times';
begin
   result := format(LINE, [HeroName(values[0]), self.GetLookup(values[1], 'commands'), values[2]]);
end;

function TEBEnableCombo.GetScriptText: string;
begin
   result := format('EnableCombo(hero[%d], %d, %d);', [values[0], values[1], values[2]]);
end;

{ TEBForceFlee }

function TEBForceFlee.GetNodeText: string;
const TARGET: array[0..2] of string = ('Party', 'All Monsters', 'Monster #%d');
begin
   result := format('Force flee: ' + TARGET[Values[0]], [Values[1]]);
end;

function TEBForceFlee.GetScriptText: string;
begin
   case Values[0] of
      0: result := format('PartyFlee(%s);', [BOOL_STR[values[2]]]);
      1: result := format('MonsterPartyFlee(%s);', [BOOL_STR[values[2]]]);
      2: result := format('Monster[%d].Flee(true, %s);', [Values[1], BOOL_STR[values[2]]]);
      else raise ERpgScriptError.CreateFmt('Unknown TEBForceFlee Value #0: %d', [Values[0]]);
   end;
end;

{ TEBEndBattle }

function TEBEndBattle.GetNodeText: string;
begin
   result := 'End Battle';
end;

function TEBEndBattle.GetScriptText: string;
begin
   result := 'EndBattle;';
end;

{ TEBBattleAnimation }

function TEBBattleAnimation.GetNodeText: string;
const LINE = 'Show Animation %s, %s';
var
   target: string;
begin
   if boolean(Values[3]) then
   begin
      if values[0] = -1 then
         target := 'Entire Party'
      else target := HeroName(Values[1]);
   end
   else if values[0] = -1 then
         target := 'All Monsters'
      else target := format('Monster #%d', [Values[1]]);
   result := format(LINE, [GetLookup(values[0], 'animations'), target]);
   if boolean(values[2]) then
      result := result + ' (Wait)';
end;

function TEBBattleAnimation.GetScriptText: string;
const LINE = 'ShowAnimation(%d, %s, %d, %s);';
begin
   result := format(LINE, [values[0], BOOL_STR[values[1]], values[2], BOOL_STR[values[3]]]);
end;

{ TEBBattle }

function TEBBattle.GetNodeText: string;
const
   LINE = 'Enter Battle: %s';
   ADDON = '%s, %s';
var
   first: string;
begin
   if boolean(values[0]) then
      first := format('Ints[%d]', [Values[1]])
   else first := self.GetLookup(values[1], 'mparties');
   result := format(LINE, [first]);
   if self.Text <> '' then
      result := format(ADDON, [result, self.Text]);
   if boolean(self.Values[3]) then
      result := format(ADDON, [result, 'First Strike']);
end;

function TEBBattle.GetScriptBase: string;
const LINE = 'battle(%s, %s, %s, %s)';
var
   first, resultsStr: string;
begin
   if boolean(Values[0]) then
      first := format('ints[%d]', [Values[1]])
   else first := IntToStr(values[1]);
   resultsStr := SetToString(PTypeInfo(TypeInfo(TBattleResultSet)), byte(self.results), true);
   result := format(LINE, [first, QuotedStr(self.Text), resultsStr, BOOL_STR[values[2]]]);
end;

{ TEBBattleEx }

function TEBBattleEx.GetNodeText: string;
const
   LINE = 'Enter Battle: %s, %s';
   ADDON = '%s, %s';
var
   party, formation: string;
begin
   if boolean(values[0]) then
      party := format('Ints[%d]', [Values[1]])
   else party := self.GetLookup(values[1], 'mparties');
   formation := CleanEnum(GetEnumName(TypeInfo(TBattleFormation), values[2]));
   result := format(LINE, [formation, party]);
   if (self.Values[3] = 1) and (self.Text <> '') then
      result := format(ADDON, [result, self.Text])
   else if self.Values[3] = 2 then
      result := format(ADDON, [result, self.GetLookup(Values[4], 'terrain')]);
end;

function TEBBattleEx.GetScriptBase: string;
const LINE = 'battleEx(%s, %s, %s, %s, %d, %d)';
var
   first, formation, resultsStr: string;
begin
   if boolean(Values[0]) then
      first := format('ints[%d]', [Values[1]])
   else first := IntToStr(values[1]);
   resultsStr := SetToString(PTypeInfo(TypeInfo(TBattleResultSet)), byte(self.results), true);
   formation := GetEnumName(TypeInfo(TBattleFormation), values[2]);
   result := format(LINE, [first, QuotedStr(self.Text), formation, resultsStr, values[3], values[4]]);
end;

{ TEBBattleBase }

procedure TEBBattleBase.AssignProperty(const key, value: string);
begin
   if key = 'Results' then
      byte(FResults) := StringToSet(PTypeInfo(TypeInfo(TBattleResultSet)), value)
   else inherited;
end;

procedure TEBBattleBase.SerializeProps(list: TStringList; depth: integer);
begin
   inherited;
   if results <> [] then
      list.Add(IndentString(depth) + 'Results = ' + SetToString(PTypeInfo(TypeInfo(TBattleResultSet)), byte(FResults), true));
end;

initialization
   TEBObject.RegisterClasses([TEBMonsterHP, TEBMonsterMP, TEBMonsterStatus,
                    TEBShowMonster, TEBBattleBG, TEBEnableCombo, TEBForceFlee,
                    TEBEndBattle, TEBBattleAnimation, TEBBattle, TEBBattleEx]);
end.
