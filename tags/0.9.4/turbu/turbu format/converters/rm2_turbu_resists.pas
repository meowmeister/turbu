unit rm2_turbu_resists;
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
   LDB, condition_data,
   turbu_resists;

type
   T2k2AttributeTemplate = class helper for TAttributeTemplate
   public
      constructor Convert(base: TAttribute; id: word);
   end;

   T2k2ConditionTemplate = class helper for TConditionTemplate
   public
      constructor Convert(base: TCondition; id: word);
   end;

implementation

{ T2k2AttributeTemplate }

constructor T2k2AttributeTemplate.Convert(base: TAttribute; id: word);
var
   i: integer;
begin
   inherited Create;
   self.id := id;
   self.name := string(base.name);
   self.requiredForSkills := base.weaponRestrict;
   for i := 1 to 5 do
      self.standard[i - 1] := base.rate[i];
end;

{ T2k2ConditionTemplate }

constructor T2k2ConditionTemplate.Convert(base: TCondition; id: word);
var
   i: integer;
   msg: TConditionMessages;
begin
   inherited Create;
   self.id := id;
   self.name := string(base.name);
   for i := 1 to 5 do
      self.standard[i - 1] := base.chance[i];
   self.outOfBattle := base.lastsOutsideBattle;
   self.color := base.color;
   self.priority := base.priority;
   self.attackLimit := turbu_resists.TAttackLimitation(base.limitation);

   self.healTurns := base.healTurns;
   self.healTurnPercent := base.healTurnPercent;
   self.healShockPercent := base.healShockPercent;
   self.attack := base.attack;
   self.defense := base.defense;
   self.mind := base.mind;
   self.speed := base.speed;
   self.toHitChange := base.toHitChange;
   self.physBlock := base.physBlock;
   self.magBlock := base.magBlock;
   self.physCutoff := base.physCutoff;
   self.magCutoff := base.magCutoff;
   for msg := Low(TConditionMessages) to High(TConditionMessages) do
      FConditionMessages[msg] := string(base.conditionMessages[condition_data.TConditionMessages(msg)]);
   self.usesConditionMessages := base.usesConditionMessages;
   self.hpTurnPercent := base.hpTurnPercent;
   self.hpTurnFixed := base.hpTurnFixed;
   self.hpStepCount := base.hpStepCount;
   self.hpStepQuantity := base.hpStepQuantity;
   self.mpTurnPercent := base.mpTurnPercent;
   self.mpTurnFixed := base.mpTurnFixed;
   self.mpStepCount := base.mpStepCount;
   self.mpStepQuantity := base.mpStepQuantity;

   self.statEffect := turbu_resists.TStatEffect(base.statEffect);
   self.evade := base.evade;
   self.reflect := base.reflect;
   self.eqLock := base.eqLock;
   self.animation := base.animation;
   self.hpDot := turbu_resists.TDotEffect(base.hpDot);
   self.mpDot := turbu_resists.TDotEffect(base.mpDot);
end;

end.
