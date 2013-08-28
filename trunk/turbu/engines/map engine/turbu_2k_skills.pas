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
unit turbu_2k_skills;

interface

uses
   types,
   turbu_classes, turbu_skills, turbu_sounds, turbu_heroes;

type
   TRpgSkill = class(TRpgObject)
   private
      function getSound1: TRpgSound;
    function GetTemplate: TSkillTemplate;
   public
      constructor Create(id: word);
      procedure useHero(caster, target: TRpgHero);
      procedure useParty(caster: TRpgHero);
      function baseDamage(caster: TRpgHero): integer;
      function areaSkill: boolean;
      function usableParty: boolean;
      function usableOn(id: word): boolean;

      property template: TSkillTemplate read GetTemplate;
      property firstSound: TRpgSound read getSound1;
   end;

implementation

uses
   turbu_constants, turbu_animations, turbu_database,
   turbu_2k_environment;

{ TRpgSkill }

function TRpgSkill.areaSkill: boolean;
begin
   result := template.range = sr_area;
end;

function TRpgSkill.baseDamage(caster: TRpgHero): integer;
var
   deviation: integer;
   template: TNormalSkillTemplate;
begin
   assert(template is TNormalSkillTemplate);
   template := TNormalSkillTemplate(template);
   result := round((caster.attack * template.strEffect / 20) + (caster.mind * template.mindEffect / 40)) + template.base;
   deviation := random(template.variance * 5 + 1) - (template.variance * 5);
   result := round(result * (1 + (deviation / 100)));
end;

constructor TRpgSkill.Create(id: word);
begin
   inherited Create(GDatabase.skill[id]);
end;

function TRpgSkill.getSound1: TRpgSound;
begin
   result := template.firstSound;
end;

function TRpgSkill.GetTemplate: TSkillTemplate;
begin
   result := TSkillTemplate(inherited template);
end;

function TRpgSkill.usableParty: boolean;
var i: word;
begin
   result := false;
   if not areaSkill then
      Exit;

   for I := 1 to MAXPARTYSIZE do
      if GEnvironment.Party[i] <> GEnvironment.Heroes[0] then
         result := result or usableOn(GEnvironment.Party[i].template.id);
end;

function TRpgSkill.usableOn(id: word): boolean;
var
   hero: TRpgHero;
   template: TNormalSkillTemplate;
begin
   hero := GEnvironment.Heroes[id];
   if self.template is TNormalSkillTemplate then
   begin
      template := TNormalSkillTemplate(template);
      result := template.offensive = false;
      if template.hp and template.mp then
         result := result and ((hero.hp < hero.maxHp) or (hero.mp < hero.maxMp))
      else if template.hp then
         result := result and (hero.hp < hero.maxHp)
      else if template.mp then
         result := result and (hero.mp < hero.maxMp)
      else result := false;
   end
   else result := true;
end;

procedure TRpgSkill.useParty(caster: TRpgHero);
var
   i: Integer;
begin
   for I := 1 to MAXPARTYSIZE do
      if GEnvironment.Party[i] <> GEnvironment.heroes[0] then
         self.useHero(caster, GEnvironment.Party[i]);
end;

procedure TRpgSkill.useHero(caster, target: TRpgHero);
var
   template: TNormalSkillTemplate;
begin
   if template is TNormalSkillTemplate then
   begin
      template := TNormalSkillTemplate(template);
      if template.hp then
         target.hp := target.hp + baseDamage(caster);
      if template.mp then
         target.mp := target.mp + baseDamage(caster);
   end;
end;

end.
