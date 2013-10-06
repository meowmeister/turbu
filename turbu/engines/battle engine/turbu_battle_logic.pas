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
   public
      procedure Execute(signal: TSimpleEvent; engine: TSpriteEngine);  override;
   end;

implementation
uses
   Math,
   turbu_defs, turbu_animations, turbu_items, turbu_database,
   turbu_2k_animations, turbu_2k_map_engine, turbu_2k_monster_party,
   turbu_2k_frames,
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
   weapon, idx: integer;
   weaponTemplate: TWeaponTemplate;
   fullscreen: boolean;
begin
   weapon := (FCharacter as TRpgHero).equipment[eq_weapon];
   if weapon = 0 then
   begin
      idx := TRpgHero(FCharacter).template.unarmedAnim;
      fullscreen := false;
   end
   else begin
      weaponTemplate := GDatabase.findItem(weapon) as TWeaponTemplate;
      idx := weaponTemplate.battleAnim;
      fullscreen := weaponTemplate.areaHit
   end;
   signal.ResetEvent;
   rs_maps.showBattleAnimT(engine, idx, TMonsterTarget.Create(FTarget as T2kMonster), fullscreen, signal);
end;

{ TMonsterTarget }

constructor TMonsterTarget.Create(target: T2kMonster);
begin
   FTarget := target;
end;

procedure TMonsterTarget.flash(r, g, b, power, time: integer);
begin
   //
end;

function TMonsterTarget.position(sign: integer): TSgPoint;
begin
   assert((sign >= -1) and (sign <= 1));
   result := FTarget.position + (FTarget.sprite.textureSize / 2);
   inc(result.y, (FTarget.sprite.textureSize.y div 2) * sign);
end;

end.
