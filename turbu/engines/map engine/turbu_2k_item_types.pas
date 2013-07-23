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
unit turbu_2k_item_types;

interface
uses
   turbu_2k_items, turbu_heroes, turbu_skills;

type
   TJunkItem = class(TRpgItem)
   protected
      function getOnField: boolean; override;
   public
      function usableBy(hero: integer): boolean; override;
   end;

   TEquipment = class abstract(TRpgItem)
   private
      function getAgi: smallint;
      function getAtk: smallint;
      function getDef: smallint;
      function getMind: smallint;
   protected
      function getOnField: boolean; override;
   public
      function usableBy(hero: integer): boolean; override;

      property attack: smallint read getAtk;
      property defense: smallint read getDef;
      property speed: smallint read getAgi;
      property mind: smallint read getMind;
   end;

   TAppliedItem = class abstract(TRpgItem)
   protected
      function getOnField: boolean; override;
   public
      function usableBy(hero: integer): boolean; override;
      function usableArea: boolean;
      function areaItem: boolean; virtual;
      procedure use(target: TRpgHero); virtual;
      procedure useArea;
   end;

   TRecoveryItem = class(TAppliedItem)
   private
   public
      function usableBy(hero: integer): boolean; override;
      procedure use(target: TRpgHero); override;
   end;

   TBookItem = class(TAppliedItem)
   private
   public
      function usableBy(hero: integer): boolean; override;
      procedure use(target: TRpgHero); override;
   end;

   TStatItem = class(TAppliedItem)
   private
   public
      function usableBy(hero: integer): boolean; override;
      procedure use(target: TRpgHero); override;
   end;

   TSkillItem = class(TAppliedItem)
   private
      FSkill: TSkillTemplate;
   protected
      function getOnField: boolean; override;
   public
      constructor create(const item, quantity: integer); override;

      function usableBy(hero: integer): boolean; override;
      function areaItem: boolean; override;
      procedure use(target: TRpgHero); override;

      property skill: TSkillTemplate read FSkill;
   end;

   TSwitchItem = class(TRpgItem)
   protected
      function getOnField: boolean; override;
   public
      function usableBy(hero: integer): boolean; override;
      procedure use;
   end;

implementation
uses
   turbu_items, turbu_constants, turbu_defs, turbu_2k_environment, turbu_database,
   turbu_2k_sprite_engine;

{ TEquipment }

function TEquipment.getAgi: smallint;
begin
   result := TEquipmentTemplate(template).stat[6];
end;

function TEquipment.getAtk: smallint;
begin
   result := TEquipmentTemplate(template).stat[3];
end;

function TEquipment.getDef: smallint;
begin
   result := TEquipmentTemplate(template).stat[4];
end;

function TEquipment.getMind: smallint;
begin
   result := TEquipmentTemplate(template).stat[5];
end;

function TEquipment.getOnField: boolean;
begin
   result := false;
end;

function TEquipment.usableBy(hero: integer): boolean;
begin
   result := hero in TUsableItemTemplate(template).usableByHero;
end;

{ TAppliedItem }

function TAppliedItem.areaItem: boolean;
begin
   result := TMedicineTemplate(template).areaMedicine;
end;

function TAppliedItem.getOnField: boolean;
begin
   result := true;
end;

function TAppliedItem.usableArea: boolean;
var
  I: Integer;
begin
   result := false;
   for I := 1 to MAXPARTYSIZE do
      if GEnvironment.Party[i] <> GEnvironment.Heroes[0] then
         result := result or usableBy(GEnvironment.Party[i].template.id);
end;

function TAppliedItem.usableBy(hero: integer): boolean;
begin
   result := hero in TUsableItemTemplate(template).usableByHero;
   case (template as TUsableItemTemplate).usableWhere of
      us_none: result := false;
      us_field: result := result and (GSpriteEngine.state <> gs_battle);
      us_battle: result := result and (GSpriteEngine.state = gs_battle);
      us_both: ;
   end;
end;

procedure TAppliedItem.use(target: TRpgHero);
begin
   if not self.areaItem then
      self.useOnce;
end;

procedure TAppliedItem.useArea;
var
   i: Integer;
begin
   for I := 1 to MAXPARTYSIZE do
      if GEnvironment.Party[i] <> GEnvironment.heroes[0] then
         self.use(GEnvironment.Party[i]);
   self.useOnce;
end;

{ TRecoveryItem }

function TRecoveryItem.usableBy(hero: integer): boolean;
var
   med: TMedicineTemplate;
begin
   result := false;
   if not inherited usableBy(hero) then
      Exit;

   med := self.template as TMedicineTemplate;
   begin
      if (med.hpPercent > 0) or (med.hpHeal > 0) then
         result := result or (GEnvironment.Party[hero].hp < GEnvironment.Party[hero].maxHp);
      if (med.mpPercent > 0) or (med.mpHeal > 0) then
         result := result or (GEnvironment.Party[hero].mp < GEnvironment.Party[hero].maxMp);
   end;
   if GEnvironment.heroes[hero].dead then
      result := result and (CTN_DEAD in med.condition)
   else if med.deadOnly then
      result := false;
end;

procedure TRecoveryItem.use(target: TRpgHero);
var
   med: TMedicineTemplate;
   fraction: single;
   I: Integer;
begin
   med := template as TMedicineTemplate;
   for i := 1 to GDatabase.conditions.Count - 1 do
      if i in med.condition then
         target.condition[i] := false;
   if med.hpPercent <> 0 then
   begin
      fraction := med.hpPercent / 100;
      target.hp := target.hp + trunc(target.maxHp * fraction);
   end;
   if med.mpPercent <> 0 then
   begin
      fraction := med.mpPercent / 100;
      target.mp := target.mp + trunc(target.maxMp * fraction);
   end;
   target.hp := target.hp + med.hpHeal;
   target.mp := target.mp + med.mpHeal;
   inherited use(target);
end;

{ TBookItem }

function TBookItem.usableBy(hero: integer): boolean;
begin
   result := inherited usableBy(hero) and not GEnvironment.heroes[hero].skill[TSkillBookTemplate(template).skill];
end;

procedure TBookItem.use(target: TRpgHero);
begin
   target.skill[TSkillBookTemplate(template).skill] := true;
   inherited;
end;

{ TStatItem }

function TStatItem.usableBy(hero: integer): boolean;
begin
   result := false;
   if not inherited usableBy(hero) then
      Exit;
end;

procedure TStatItem.use(target: TRpgHero);
begin
   target.maxHp := target.maxHp + TUsableItemTemplate(template).stat[1];
   target.maxMp := target.maxMP + TUsableItemTemplate(template).stat[2];
   target.attack := target.attack + TUsableItemTemplate(template).stat[3];
   target.defense := target.defense + TUsableItemTemplate(template).stat[4];
   target.mind := target.mind + TUsableItemTemplate(template).stat[5];
   target.agility := target.agility + TUsableItemTemplate(template).stat[6];
   inherited;
end;

{ TSkillItem }

function TSkillItem.areaItem: boolean;
begin
   result := TNormalSkillTemplate(FSkill).range = sr_area;
end;

constructor TSkillItem.Create(const item, quantity: integer);
begin
   inherited;
   FSkill := GDatabase.skill[TSkillItemTemplate(template).skill];
end;

function TSkillItem.getOnField: boolean;
begin
   result := self.skill.usableWhere = us_field;
end;

function TSkillItem.usableBy(hero: integer): boolean;
begin
{$MESSAGE WARN 'Commented out code in live unit'}
   result := inherited usableBy(hero) {and FSkill.usableOn(hero)};
end;

procedure TSkillItem.use(target: TRpgHero);
begin
{$MESSAGE WARN 'Commented out code in live unit'}
//   FSkill.useHero(target, target);
   inherited;
end;

{ TSwitchItem }

function TSwitchItem.getOnField: boolean;
begin
   result := TUsableItemTemplate(template).usableWhere = us_field;
end;

function TSwitchItem.usableBy(hero: integer): boolean;
begin
   result := false;
end;

procedure TSwitchItem.use;
begin
   GEnvironment.Switch[TVariableItemTemplate(template).which] := true;
   self.useOnce;
end;

{ TJunkItem }

function TJunkItem.getOnField: boolean;
begin
   result := false;
end;

function TJunkItem.usableBy(hero: integer): boolean;
begin
   result := false;
end;

end.
