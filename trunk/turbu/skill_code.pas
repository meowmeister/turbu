unit skill_code;
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
   types,
   turbu_skills, rm_sound, script_backend;

type
   TRpgSkill = class(TObject)
   private
      FTemplate: TSkillTemplate;
      function getSound1: TRmSound;
   public
      constructor Create(id: word);
      procedure useHero(caster, target: TRpgHero);
      procedure useArea(caster: TRpgHero);
      function baseDamage(caster: TRpgHero): integer;
      function areaSkill: boolean;
      function usableArea: boolean;
      function usableOn(id: word): boolean;

      property template: TSkillTemplate read FTemplate;
      property firstSound: TRmSound read getSound1;
   end;

   TSkillEngine = class(TObject)
   private
    function getSkill(x: word): TRpgSkill;
   public
      property skill[x: word]: TRpgSkill read getSkill; default;
   end;

implementation

uses
   script_engine, script_interface, {chipset_graphics,} battle_anims,
   turbu_database;

{ TRpgSkill }

function TRpgSkill.areaSkill: boolean;
begin
//   result := FTemplate.range in [sr_attackAll, sr_helpAll];
end;

function TRpgSkill.baseDamage(caster: TRpgHero): integer;
var
   dummy: integer;
begin
{   result := round((caster.attack * FTemplate.strEffect / 20) + (caster.mind * FTemplate.mindEffect / 40)) + FTemplate.base;
   dummy := random(FTemplate.variance * 5 + 1) - (FTemplate.variance * 5);
   result := round(result * (1 + (dummy / 100)));}
end;

constructor TRpgSkill.Create(id: word);
begin
   inherited Create;
//   FTemplate := GDatabase.skill[id];
end;

function TRpgSkill.getSound1: TRmSound;
var
   dummy: TBattleAnim;
   I: Integer;
begin
   result := nil;
//   dummy := GDatabase.anim[template.anim];
   i := 1;
   while (i <= dummy.effects) and ((dummy.effect[i].sound = nil) or (dummy.effect[i].sound.filename = '')) do
      inc(i);
   if i <= dummy.effects then
      result := TRmSound(dummy.effect[i].sound);
   //end if
end;

function TRpgSkill.usableArea: boolean;
var i: word;
begin
   result := false;
   if not areaSkill then
      Exit;

   for I := 1 to MAXPARTYSIZE do
      if GParty[i] <> GScriptEngine.hero[0] then
         result := result or usableOn(GParty[i].template.id);
      //end if
   //end for
end;

function TRpgSkill.usableOn(id: word): boolean;
var dummy: TRpgHero;
begin
   dummy := GScriptEngine.hero[id];
{   if self.FTemplate.skillType = sk_normal then
   begin
      result := self.FTemplate.range in [sr_self, sr_help1, sr_helpAll];
      if self.FTemplate.hp and self.FTemplate.mp then
         result := result and ((dummy.hp < dummy.maxHp) or (dummy.mp < dummy.maxMp))
      else if self.FTemplate.hp then
         result := result and (dummy.hp < dummy.maxHp)
      else if self.FTemplate.mp then
         result := result and (dummy.mp < dummy.maxMp)
      else result := false;
   end
   else result := true;}
end;

procedure TRpgSkill.useArea(caster: TRpgHero);
var
   i: Integer;
begin
   for I := 1 to MAXPARTYSIZE do
      if GParty[i] <> GScriptEngine.hero[0] then
         self.useHero(caster, GParty[i]);
      //end if
   //end for
end;

procedure TRpgSkill.useHero(caster, target: TRpgHero);
begin
{   if self.FTemplate.hp then
   begin
      target.hp := target.hp + baseDamage(caster);
   end
   else if self.FTemplate.mp then
   begin
      target.mp := target.mp + baseDamage(caster);
   end;}
end;

{ TSkillEngine }

function TSkillEngine.getSkill(x: word): TRpgSkill;
begin
   result := TRpgSkill.Create(x);
end;

end.
