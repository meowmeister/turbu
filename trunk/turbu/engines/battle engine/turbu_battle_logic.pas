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
unit turbu_battle_logic;

interface
uses
   SyncObjs,
   turbu_heroes,
   sdl_sprite;

type
   TBattleCommand = class
   private
      FCharacter: TRpgBattleCharacter;
      FPriority: integer;
   public
      constructor Create(user: TRpgBattleCharacter);
      procedure Execute(signal: TSimpleEvent; engine: TSpriteEngine); virtual; abstract;
      property user: TRpgBattleCharacter read FCharacter;
      property priority: integer read FPriority;
   end;

   TTargetCommand = class(TBattleCommand)
   private
      FTarget: TRpgBattleCharacter;
   public
      constructor Create(user, target: TRpgBattleCharacter);
   end;

   TAttackCommand = class(TTargetCommand)
   private
      function ToHit: boolean;
   public
      procedure Execute(signal: TSimpleEvent; engine: TSpriteEngine);  override;
   end;

implementation
uses
   Math,
   turbu_defs, turbu_constants, turbu_animations, turbu_items, turbu_database,
   turbu_2k_animations, turbu_2k_map_engine, turbu_2k_monster_party,
   turbu_2k_frames,
   commons,
   rs_maps,
   sg_defs;

type
   TMonsterTarget = class(TInterfacedObject, IAnimTarget)
   private
      FTarget: T2kMonster;
      function position(sign: integer): TSgPoint;
      procedure flash(r, g, b, power: integer; time: integer);
   public
      constructor Create(target: T2kMonster);
   end;

{ TBattleCommand }

constructor TBattleCommand.Create(user: TRpgBattleCharacter);
var
   modifier: integer;
begin
   FCharacter := user;
   modifier := RandomRange(80, 120);
   FPriority := round(user.agility * (modifier / 100));
end;

{ TTargetCommand }

constructor TTargetCommand.Create(user, target: TRpgBattleCharacter);
begin
   inherited Create(user);
   FTarget := target;
end;

{ TAttackCommand }

procedure TAttackCommand.Execute(signal: TSimpleEvent; engine: TSpriteEngine);
var
   weapon, idx, damage: integer;
   weaponTemplate: TWeaponTemplate;
   fullscreen: boolean;
   hero: TRpgHero;
begin
   hero := FCharacter as TRpgHero;
   weapon := hero.equipment[eq_weapon];
   if weapon = 0 then
   begin
      idx := hero.template.unarmedAnim;
      fullscreen := false;
   end
   else begin
      weaponTemplate := GDatabase.findItem(weapon) as TWeaponTemplate;
      idx := weaponTemplate.battleAnim;
      fullscreen := weaponTemplate.areaHit;
   end;
   signal.ResetEvent;
   rs_maps.showBattleAnimT(engine, idx, TMonsterTarget.Create(FTarget as T2kMonster), fullscreen, nil);
   damage := FTarget.takeDamage(FCharacter.attack div 2, 100, 0, 4);
   T2kMonster(FTarget).signal := signal;
end;

function TAttackCommand.ToHit: boolean;
var
   hero: TRpgHero;
   weapon, toHitChance, agiMod, attCondMod, defCondMod: integer;
   weaponTemplate: TWeaponTemplate;
begin
   {$MESSAGE WARN 'Incomplete feature in live code'}
   if {Target can't move due to condition} false then
      exit(true);
   hero := FCharacter as TRpgHero;
   weapon := hero.equipment[eq_weapon];
   if weapon = 0 then
   begin
      toHitChance := 90;
      weaponTemplate := nil;
   end
   else begin
      weaponTemplate := GDatabase.findItem(weapon) as TWeaponTemplate;
      toHitChance := weaponTemplate.toHit;
   end;
   if assigned(weaponTemplate) and weaponTemplate.evasion then
   begin
      agiMod := 0; attCondMod := 0; defCondMod := 0;
   end else begin
      agiMod := round((hero.agility / FTarget.agility) * 50);
      {$MESSAGE WARN 'Incomplete feature in live code'}
      attCondMod := {To-hit modification by attacker's conditions} 0;
      {$MESSAGE WARN 'Incomplete feature in live code'}
      defCondMod := {To-hit modification by defender's conditions or equipment} 0;
   end;
   result := random(100) < toHitChance + agiMod + attCondMod + defCondMod;
end;

{ TMonsterTarget }

constructor TMonsterTarget.Create(target: T2kMonster);
begin
   FTarget := target;
end;

procedure TMonsterTarget.flash(r, g, b, power, time: integer);
begin
   FTarget.Flash(time, TRpgColor.Create(r, g, b, power));
end;

function TMonsterTarget.position(sign: integer): TSgPoint;
begin
   assert((sign >= -1) and (sign <= 1));
   result := FTarget.position + (FTarget.sprite.textureSize / 2);
   inc(result.y, (FTarget.sprite.textureSize.y div 2) * sign);
end;

end.
