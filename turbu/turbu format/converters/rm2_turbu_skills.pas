unit rm2_turbu_skills;
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
   skill_data, hero_data, turbu_skills;

type
   T2k2SkillRecord = class helper for TSkillGainInfo
   public
      constructor convert(base: THeroSkillRecord);
   end;

   T2k2SkillTemplate = class helper for TSkillTemplate
   public
      constructor Convert(base: TSkill);
      class procedure addNewSkill(base: TSkill);
   end;

   T2k2NormalSkillTemplate = class helper for TNormalSkillTemplate
   public
      constructor Convert(base: TSkill);
   end;

   T2k2SpecialSkillTemplate = class helper for TSpecialSkillTemplate
   public
      constructor Convert(base: TSkill);
   end;

   T2k2TeleportSkillTemplate = class helper for TTeleportSkillTemplate
   public
      constructor Convert(base: TSkill);
   end;

   T2k2VariableSkillTemplate = class helper for TVariableSkillTemplate
   public
      constructor Convert(base: TSkill);
   end;

implementation
uses
   classes,
   formats,
   turbu_database, turbu_defs, turbu_sounds, rm2_turbu_database, rm2_turbu_sounds,
   turbu_operators;

{ T2k2SkillTemplate }

{$WARN USE_BEFORE_DEF OFF}
class procedure T2k2SkillTemplate.addNewSkill(base: TSkill);
var
   newSkill: TSkillTemplate;
begin
   case base.skillType of
      sk_normal: newSkill := TNormalSkillTemplate.Convert(base);
      sk_teleport, sk_escape: newSkill := TTeleportSkillTemplate.Convert(base);
      sk_switch: newSkill := TVariableSkillTemplate.Convert(base);
      else newSkill := TNormalSkillTemplate.Convert(base); //RM2K3 "skill group" skills
   end;
   GDatabase.skill.Add(newSkill);
end;
{$WARN USE_BEFORE_DEF ON}

constructor T2k2SkillTemplate.Convert(base: TSkill);
begin
   self.id := base.id;
   self.name := unicodeString(base.name);
   self.desc := unicodeString(base.desc);
   if base.usesPercentCost then
   begin
      self.costAsPercentage := true;
      self.cost := base.percentCost;
   end
   else self.cost := base.cost;
   if GProjectFormat = pf_2k then
   begin
      self.useString := string(base.usage);
      self.useString2 := string(base.usage2);
      self.failureMessage := base.failure;
   end;
   if (GProjectFormat = pf_2k3) and (base.unk32 <> '') then
      GDatabase.AddLegacy('Skill', self.id, 32, base.unk32);
   //fill in usableWhere later
end;

{ T2k2NormalSkillTemplate }

constructor T2k2NormalSkillTemplate.Convert(base: TSkill);
var
   i: integer;
   counter: word;
   percentage, runningtotal: extended;
   conditions: TByteSet;
begin
   inherited Convert(base);
   if base.usableOnField then
      self.usableWhere := us_both
   else self.usableWhere := us_battle;
   case base.range of
      skill_data.sr_self: self.range := sr_self;
      sr_attack1, sr_help1: self.range := sr_target;
      sr_attackAll, sr_helpAll: self.range := sr_area;
   end;
   case base.range of
      sr_attack1, sr_attackAll: self.offensive := true;
      skill_data.sr_self..sr_helpAll: self.offensive := false;
   end;
   self.animation := base.anim;
   self.skillPower[1] := base.strEffect;
   self.skillPower[2] := base.mindEffect;
   self.skillPower[3] := base.variance;
   self.skillPower[4] := base.base;
   self.successRate := base.successRate;
   self.stat[1] := base.hp;
   self.stat[2] := base.mp;
   self.stat[3] := base.attack;
   self.stat[4] := base.defense;
   self.stat[5] := base.mind;
   self.stat[6] := base.speed;
   self.drain := base.drain;
   self.phased := base.phased;

   conditions := [];
   for I := 0 to GLcfDatabase.conditions do
      if base.condition[i] then
         include(conditions, i);
   self.condition := conditions;

   counter := 0;
   runningtotal := 0;
   for I := 1 to GLcfDatabase.attributes do
   begin
      if base.attribute[i] then
         inc(counter);
   end;
   if counter > 0 then
   begin
      percentage := 100 / counter;
      for I := 1 to GLcfDatabase.attributes do
         if base.attribute[i] then
         begin
            runningtotal := runningtotal + counter;
            setLength(FAttributes, length(FAttributes) + 1);
            with FAttributes[high(FAttributes)] do
            begin
               x := i;
               y := trunc(percentage);
               if runningTotal - trunc(runningtotal) >= 0.5 then
                  inc(y);
            end;
         end;
      //end for
   end;
   self.resistMod := base.resistMod;
   self.inflictReversed := base.inflictReversed;
   self.displaySprite := base.displaySprite;
end;

{ T2k2SpecialSkillTemplate }

constructor T2k2SpecialSkillTemplate.Convert(base: TSkill);
begin
   inherited Convert(base);
   assert(base.skillType <> sk_normal);
   self.sfx := TRpgSound.Convert(base.sfx);
end;

{ T2k2TeleportSkillTemplate }

constructor T2k2TeleportSkillTemplate.Convert(base: TSkill);
begin
   inherited Convert(base);
   assert(base.skillType in [sk_teleport, sk_escape]);
   self.teleportTarget := ord(base.skillType);
   case base.skillType of
      sk_teleport: self.usableWhere := us_field;
      sk_escape: self.usableWhere := us_battle;
   end;
end;

{ T2k2VariableSkillTemplate }

constructor T2k2VariableSkillTemplate.Convert(base: TSkill);
begin
   inherited Convert(base);
   assert(base.skillType = sk_switch);
   self.which := base.switch;
   self.style := vs_switch;
   self.operation := bo_add;
   if base.field and base.battle then
      self.usableWhere := us_both
   else if base.field then
      self.usableWhere := us_field
   else if base.battle then
      self.usableWhere := us_battle
   else self.usableWhere := us_none;
end;

{ T2k2SkillRecord }

constructor T2k2SkillRecord.convert(base: THeroSkillRecord);
begin
   inherited Create;
   self.id := -1;
   self.style := sf_bool;
   self.skill := base.id;
   self.num[1] := base.level;
   self.num[2] := 0;
   self.num[3] := 0;
   self.num[4] := 0;
end;

end.
