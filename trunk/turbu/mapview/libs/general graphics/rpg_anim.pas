unit rpg_anim;
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
   math,
   commons, types, timing, battle_anims, script_interface, addition_sprite,
   {AsphyreDef, AsphyreSprite} turbu_defs, SDL_Sprite;

type
   TAnimSprite = class(TSprite)
   private
      FBase: TBattleAnim;
      FTiles: array[1..10] of TSprite;
      FTimer: TRpgTimestamp;
      FTarget: TRpgCharacter;
      FLastFrame: word;
      FLastEffect: word;
      FFullScreen: boolean;

      procedure move; reintroduce;
      function getTime: cardinal; inline;
   public
      constructor Create(parent: TSpriteEngine; base: TBattleAnim; target: TRpgCharacter; fullscreen: boolean); reintroduce;
      destructor Destroy; override;
      procedure Draw; override;

      property time: cardinal read getTime;
   end;

implementation
uses
   windows, sysUtils,
   chipset_graphics, script_engine, distortions, rs_map,
   SDL,
   {asphyreImages} sdl_imageManager;

{ TAnimSprite }

constructor TAnimSprite.Create(parent: TSpriteEngine; base: TBattleAnim; target: TRpgCharacter; fullscreen: boolean);
begin
   inherited Create(parent);
   FBase := base;
   self.z := 19; //one below TRpgImage's 20
   self.pinned := true;
   FTimer := TRpgTimestamp.Create(FBase.frames * 32);
   FTarget := target;
   FFullScreen := fullscreen;
end;

destructor TAnimSprite.Destroy;
var
   I: Integer;
begin
   for I := 1 to high(FTiles) do
      FTiles[i].free;
   FTimer.free;
   inherited;
end;

procedure TAnimSprite.Draw;
var
   i: byte;
begin
   self.move;
   i := 1;
   while assigned(FTiles[i]) do   
   begin
      FTiles[i].Draw;
      inc(i);
   end;
      
   if FTimer.timeRemaining = 0 then
      self.dead;
   //end if
end;

function TAnimSprite.getTime: cardinal;
begin
   result := FTimer.timeRemaining;
end;

procedure TAnimSprite.move;

   procedure extractColors(const effect: TAnimEffects; out r, g, b, a: byte); inline;
   begin
      r := commons.round(effect.r * MULTIPLIER_31);
      g := commons.round(effect.g * MULTIPLIER_31);
      b := commons.round(effect.b * MULTIPLIER_31);
      a := commons.round(effect.a * MULTIPLIER_31);
   end;

var
   I: Integer;
   frame: word;
   currFrame: TAnimFrame;
   currEffect: TAnimEffects;
   r, g, b, a: byte;
   dummy: shortint;
begin
   //timing/sync issues
   frame := max(FBase.frames - (FTimer.timeRemaining div 32), 1);
   if frame = FLastFrame then
      Exit;
   FLastFrame := frame;
   dummy := 0; //suppress compiler warning

   //clear old tiles
   for I := 1 to high(FTiles) do
      freeAndNil(FTiles[i]);

   //create new tiles
   for I := 1 to high(FBase.frame[frame]) do
   begin
      currFrame := FBase.frame[frame];
      FTiles[i] := TSprite.Create(engine);
//fixme
{      FTiles[i].DrawMode := 1;
      FTiles[i].DrawFx := fxBright;}
      FTiles[i].pinned := true;
      FTiles[i].ImageName := 'Anim ' + FBase.filename;
//fixme
//      FTiles[i].PatternIndex := currFrame[i].index;
      if FFullScreen then
      begin
         FTiles[i].x := currFrame[i].x + engine.Canvas.Width div 2;
         FTiles[i].y := currFrame[i].y + engine.Canvas.Height div 2;
      end else begin
         FTiles[i].x := currFrame[i].x + FTarget.screenXP + 12; //fix the 12 later
         case FBase.yTarget of
            at_top: dummy := -16;
            at_center: dummy := 0;
            at_bottom: dummy := 16;
         end;
         FTiles[i].y := currFrame[i].y + FTarget.screenYP + dummy;
      end;
      FTiles[i].scaleX := currFrame[i].zoom / 100;
      FTiles[i].scaleY := FTiles[i].scaleX;
//fixme
{      FTiles[i].Red := commons.round(currFrame[i].color.rgba[1] * 1.275);
      FTiles[i].Green := commons.round(currFrame[i].color.rgba[2] * 1.275);
      FTiles[i].Blue := commons.round(currFrame[i].color.rgba[3] * 1.275);}
      FTiles[i].Alpha := commons.round(currFrame[i].color.rgba[4] * 2.55);
      //add saturation later
   end;

   if FLastEffect < FBase.effects then
   begin
      currEffect := FBase.effect[FLastEffect + 1];
      if currEffect.frame = frame then
      begin
         if currEffect.sound.filename <> '' then
            GScriptEngine.mediaPlayer.playSfx(currEffect.sound);
         case currEffect.flashWhere of
            fl_none: ;
            fl_target:
            begin
               extractColors(currEffect, r, g, b, a);
               FTarget.flash(r, g, b, a, 200, false);
            end;
            fl_screen:
            begin
               extractColors(currEffect, r, g, b, a);
               rs_map.flashScreen(r, g, b, a, 200, false);
            end;
         end;
         inc(FLastEffect);
      end;
   end;
end;

end.
