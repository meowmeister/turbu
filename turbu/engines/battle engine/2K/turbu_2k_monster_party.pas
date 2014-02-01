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
unit turbu_2k_monster_party;

interface
uses
   Generics.Collections, OpenGL, Types, SyncObjs,
   turbu_monsters, turbu_classes, turbu_heroes, turbu_battle_logic,
   timing, commons,
   sg_defs, SDL_ImageManager;

type
   TLifeState = (lsAlive, lsInjured, lsDying, lsDead);

   T2kMonsterParty = class;

   T2kMonster = class(TRpgBattleCharacter)
   private
      FSprite: TSdlImage;
      FVisible: boolean;
      FPosition: TsgPoint;
      FFlashTimer: TRpgTimestamp;
      FFlashLength: integer;
      FFlashColor: TRpgColor;
      FLifeState: TLifeState;
      FInjuryCount: integer;
      FSignal: TSimpleEvent;
      FParty: T2kMonsterParty;
      function getTemplate: TRpgMonster; inline;
      function IsDead: boolean; inline;
      function FlashColor: TGLArrayf4;
      procedure ResetSignal;
      procedure Dying;
      procedure Injured;
      procedure DrawFlash;
      procedure DrawDying;
      procedure DrawInjured;
      procedure DrawSelf(SpriteRect: TRect);
   protected
      class function templateClass: TDatafileClass; override;
      procedure setHP(value: integer); override;
      procedure setMP(value: integer); override;
   public
      constructor Create(template: TRpgMonster; element: TRpgMonsterElement; images: TSdlImages);
      destructor Destroy; override;
      procedure Draw;
      function GetMove: TBattleCommand;
      procedure Flash(time: integer; color: TRpgColor);
      function takeDamage(power: integer; defense, mDefense, variance: integer): integer; override;
      function retarget: TRpgBattleCharacter; override;

      property template: TRpgMonster read getTemplate;
      property sprite: TSdlImage read FSprite;
      property visible: boolean read FVisible write FVisible;
      property dead: boolean read IsDead;
      property position: TsgPoint read FPosition;
      property signal: TSimpleEvent read FSignal write FSignal;
   end;

   T2kMonsterParty = class(TRpgObject)
   private
      FMonsters: TObjectList<T2kMonster>;
      function getTemplate: TRpgMonsterParty;
      function IsDefeated: boolean;
   protected
      class function templateClass: TDatafileClass; override;
   public
      constructor Create(template: TRpgMonsterParty; images: TSdlImages);
      destructor Destroy; override;
      procedure Draw;
      function GetMoves: TArray<TBattleCommand>;

      property defeated: boolean read IsDefeated;
      property template: TRpgMonsterParty read getTemplate;
      property monsters: TObjectList<T2kMonster> read FMonsters;
   end;

implementation
uses
   SysUtils,
   turbu_database, turbu_OpenGL, turbu_defs,
   turbu_2k_environment, turbu_2k_sprite_engine,
   rs_media;

{ T2KMonster }

constructor T2KMonster.Create(template: TRpgMonster; element: TRpgMonsterElement; images: TSdlImages);
var
   i: integer;
begin
   inherited Create(template);
   self.name := template.name;
   FVisible := not element.invisible;
   FHitPoints := template.stat[1];
   FMaxHitPoints := FHitPoints;
   FManaPoints := template.stat[2];
   FMaxManaPoints := FManaPoints;
   FSprite := images.Image[format('MONSTER*%s', [template.filename])];
   FPosition := element.position;
   for i := 1 to 4 do
      FStat[stat_base][i] := template.stat[2 + i];
end;

destructor T2kMonster.Destroy;
begin
   FFlashTimer.Free;
   inherited Destroy;
end;

procedure T2KMonster.DrawSelf(SpriteRect: TRect);
var
   top, left: integer;
begin
   left := trunc(FPosition.X);
   top := trunc(FPosition.Y);

   FSprite.surface.bind;
   glEnable(GL_BLEND);
   glEnable(GL_ALPHA_TEST);
   glBegin(GL_QUADS);
      glTexCoord2i(spriteRect.Left,                    spriteRect.Top);
      glVertex2f(left,                                 top);
      glTexCoord2i(spriteRect.Left,                    spriteRect.Top + spriteRect.Bottom);
      glVertex2f(left,                                 top + FSprite.textureSize.y);
      glTexCoord2i(spriteRect.Left + spriteRect.Right, spriteRect.Top + spriteRect.Bottom);
      glVertex2f(left + FSprite.textureSize.x,                    top + FSprite.textureSize.y);
      glTexCoord2i(spriteRect.Left + spriteRect.Right, spriteRect.Top);
      glVertex2f(left + FSprite.textureSize.x,                    top);
   glEnd;
end;

procedure T2KMonster.DrawFlash;
var
   current, handle: integer;
begin
   glGetIntegerv(GL_CURRENT_PROGRAM, @current);
   glPushAttrib(GL_CURRENT_BIT);
   handle := GSpriteEngine.ShaderEngine.ShaderProgram('default', 'flash');
   GSpriteEngine.ShaderEngine.UseShaderProgram(handle);
   GSpriteEngine.ShaderEngine.SetUniformValue(handle, 'flashColor', self.FlashColor);

   DrawSelf(FSprite.spriteRect[0]);
   glUseProgram(current);
   glPopAttrib;
   if FFlashTimer.timeRemaining = 0 then
   begin
      FreeAndNil(FFlashTimer);
      if FInjuryCount > 0 then
         self.Injured
      else if FHitPoints > 0 then
      begin
         FLifeState := lsAlive;
         ResetSignal;
      end
      else self.Dying;
   end;
end;

procedure T2KMonster.DrawDying;
begin
   FSprite.surface.alpha := round((FFlashTimer.timeRemaining / 1000) * 255);
   FSprite.Draw(self.FPosition, []);
   FSprite.surface.alpha := 255;

   if FFlashTimer.timeRemaining = 0 then
   begin
      FreeAndNil(FFlashTimer);
      FLifeState := lsDead;
      ResetSignal;
   end;
end;

procedure T2KMonster.DrawInjured;
begin
   case FInjuryCount of
      0:
      begin
         FreeAndNil(FFlashTimer);
         if FHitPoints > 0 then
         begin
            FLifeState := lsAlive;
            ResetSignal;
         end
         else self.Dying;
      end;
      1, 3:
         if assigned(FFlashTimer) then
         begin
            if FFlashTimer.timeRemaining = 0 then
               FreeAndNil(FFlashTimer);
         end
         else begin
            dec(FInjuryCount);
            FFlashTimer := TRpgTimestamp.Create(150);
         end;
      2, 4:
         if assigned(FFlashTimer) then
         begin
            if FFlashTimer.timeRemaining > 0 then
               FSprite.Draw(self.FPosition, [])
            else FreeAndNil(FFlashTimer);
         end
         else begin
            dec(FInjuryCount);
            FFlashTimer := TRpgTimestamp.Create(150);
         end;
   end;
end;

procedure T2KMonster.Draw;
begin
   if (not self.visible) then
      Exit;
   case FLifeState of
      lsAlive:
         if assigned(FFlashTimer) then
            DrawFlash
         else FSprite.Draw(self.FPosition, []);
      lsInjured: DrawInjured;
      lsDying: DrawDying;
      lsDead: Exit;
   end;
end;

procedure T2kMonster.Flash(time: integer; color: TRpgColor);
begin
   FreeAndNil(FFlashTimer);
   FFlashColor := color;
   time := time * 100;
   FFlashTimer := TRpgTimestamp.Create(time);
   FFlashLength := time;
end;

function T2kMonster.FlashColor: TGLArrayf4;
begin
   result[0] := (FFlashColor.rgba[1] / 255);
   result[1] := (FFlashColor.rgba[2] / 255);
   result[2] := (FFlashColor.rgba[3] / 255);
   result[3] := (FFlashColor.rgba[4] / 255) * (FFlashTimer.timeRemaining / FFlashLength);
end;

function T2kMonster.GetMove: TBattleCommand;
var
   target: integer;
begin
   target := random(GEnvironment.partySize) + 1;
   result := TMonsterAttackCommand.Create(self, GEnvironment.Party.hero[target]);
end;

function T2KMonster.getTemplate: TRpgMonster;
begin
   Result := inherited template as TRpgMonster;
end;

procedure T2kMonster.Injured;
begin
   FLifeState := lsInjured;
   rs_media.PlaySystemSound(sfxEnemyDamage);
end;

function T2KMonster.IsDead: boolean;
begin
   result := FLifeState = lsDead;
end;

procedure T2kMonster.ResetSignal;
begin
   FSignal.SetEvent;
   FSignal := nil;
end;

function T2kMonster.retarget: TRpgBattleCharacter;
begin
   repeat
      result := FParty.monsters[FParty.monsters.Count]
   until result.hp > 0;
end;

procedure T2kMonster.Dying;
begin
   FLifeState := lsDying;
   FreeAndNil(FFlashTimer);
   FFlashTimer := TRpgTimestamp.Create(1000);
   rs_media.PlaySystemSound(sfxEnemyDies);
end;

procedure T2kMonster.setHP(value: integer);
begin
   FHitPoints := value;
end;

procedure T2kMonster.setMP(value: integer);
begin
   FManaPoints := value;
end;

function T2kMonster.takeDamage(power, defense, mDefense,
  variance: integer): integer;
begin
   result := inherited takeDamage(power, defense, mDefense, variance);
   FInjuryCount := 4;
end;

class function T2KMonster.templateClass: TDatafileClass;
begin
   result := TRpgMonster;
end;

{ T2kMonsterParty }

constructor T2kMonsterParty.Create(template: TRpgMonsterParty; images: TSdlImages);
var
   element: TRpgMonsterElement;
   monster: T2kMonster;
begin
   inherited Create(template);
   FMonsters := TObjectList<T2kMonster>.Create;
   for element in template.monsters do
   begin
      monster := T2kMonster.Create(GDatabase.monsters[element.monster], element, images);
      monster.FParty := self;
      FMonsters.Add(monster);
   end;
end;

destructor T2kMonsterParty.Destroy;
begin
   FMonsters.Free;
   inherited Destroy;
end;

procedure T2kMonsterParty.Draw;
var
   monster: T2kMonster;
begin
   for monster in FMonsters do
      monster.draw;
end;

function T2kMonsterParty.GetMoves: TArray<TBattleCommand>;
var
   i: integer;
begin
   setLength(result, FMonsters.Count);
   for i := 0 to High(result) do
      result[i] := FMonsters[i].GetMove;
end;

function T2kMonsterParty.getTemplate: TRpgMonsterParty;
begin
   result := inherited template as TRpgMonsterParty;
end;

function T2kMonsterParty.IsDefeated: boolean;
var
   monster: T2kMonster;
begin
   for monster in FMonsters do
      if not monster.IsDead then
         exit(false);
   result := true;
end;

class function T2kMonsterParty.templateClass: TDatafileClass;
begin
   result := TRpgMonsterParty;
end;

end.
