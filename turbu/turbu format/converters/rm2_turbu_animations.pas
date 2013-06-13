unit rm2_turbu_animations;
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
   battle_anims, turbu_animations;

type
   T2k2RpgAnim = class helper for TAnimTemplate
   public
      constructor convert(base: TBattleAnim; id: word);
   end;

   T2k2RpgBattleChar = class helper for TBattleCharAnim
   public
      constructor convert(base: TBattleAnim2; id: word);
   end;

implementation
uses
   types,
   commons, turbu_sounds,
   rm2_turbu_sounds;

type
   T2k2AnimCell = class helper for turbu_animations.TAnimCell
   public
      constructor Convert(base: battle_anims.TAnimCell; frame: word; index: integer);
   end;

   T2k2AnimEffects = class helper for turbu_animations.TAnimEffects
   public
      constructor Convert(base: battle_anims.TAnimEffects);
   end;

   T2k2RpgBattleData = class helper for TBattleCharData
   public
      constructor convert(base: TBattle2Data; id: word);
   end;

{ T2k2RpgAnim }

constructor T2k2RpgAnim.convert(base: TBattleAnim; id: word);
var
   i, j: integer;
   idx: integer;
   baseCell: battle_anims.TAnimCell;
begin
   inherited Create;
   self.id := id;
   self.name := string(base.name);
   self.filename := string(base.filename);
   self.hitsAll := base.hitsAll;
   self.yTarget := TAnimYTarget(base.yTarget);

   assert(assigned(self.frame));
   frame.Capacity := (base.frames + 1) * 2;
   idx := 0;
   for I := 1 to base.frames do
      for j := 1 to high(base.frame[i]) do
      begin
         baseCell := base.frame[i][j];
         if not baseCell.IsNew then
            Continue;
         inc(idx);
         self.frame.Add(TAnimCell.Convert(baseCell, i, idx));
      end;

   assert(assigned(self.effect));
   effect.Add(TAnimEffects.Create);
   effect.Capacity := base.effects + 1;
   for i := 1 to base.effects do
      effect.add(TAnimEffects.Convert(base.effect[i]));
end;

{ T2k2AnimCell }

constructor T2k2AnimCell.Convert(base: battle_anims.TAnimCell; frame: word; index: integer);
begin
   FId := index;
   FFrame := frame;
   FPosition := point(base.x, base.y);
   FZoom := point(base.zoom, base.zoom);
   FColor := base.color;
   FSaturation := base.saturation;
   FImageIndex := base.index;
end;

var
   lID: integer;

{ T2k2AnimEffects }

constructor T2k2AnimEffects.Convert(base: battle_anims.TAnimEffects);
begin
   inc(lID);
   self.id := lID;
   self.frame := base.frame;
   if assigned(base.sound) then   
      self.sound := TRpgSound.Convert(base.sound);
   self.flashWhere := TFlashTarget(base.flashWhere);
   self.color := TRpgColor.Create(base.r, base.g, base.b, base.a);
   self.shakeWhere := TFlashTarget(base.shakeWhere);
end;

{ T2k2RpgBattleChar }

constructor T2k2RpgBattleChar.convert(base: TBattleAnim2; id: word);
var
   i: integer;
begin
   Create;
   self.id := id;
   self.name := string(base.name);
   FSpeed := base.speed;
   for i := 1 to high(base.poses) do
      FPoses.Add(TBattleCharData.convert(base.poses[i], i));
   for i := 1 to high(base.weapons) do
      FWeapons.Add(TBattleCharData.convert(base.weapons[i], i));
end;

{ T2k2RpgBattleData }

constructor T2k2RpgBattleData.convert(base: TBattle2Data; id: word);
begin
   Create;
   self.id := id;
   self.name := string(base.filename);
   FFrame := base.frame;
   FUnk04 := base.unk04;
   FUnk05 := base.unk05;
end;

end.
