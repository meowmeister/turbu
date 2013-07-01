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
unit turbu_2k_animations;

interface
uses
   math, types, SyncObjs,
   commons, timing, turbu_animations, turbu_mapchars,
   dm_shaders,
   turbu_defs, sg_Defs, SDL_Sprite;

type
   TAnimSpriteCell = class(TSprite)
   private
      FSaturation: integer;
      procedure PrepareShader(shaders: TdmShaders);
      procedure DrawSelf(center: TSgFloatPoint; halfWidth, halfHeight: single;
        SpriteRect: TRect);
   protected
      procedure DoDraw; override;
      property Sat: integer read FSaturation write FSaturation;
   end;

   TAnimSprite = class(TParentSprite)
   private
      FBase: TAnimTemplate;
      FTimer: TRpgTimestamp;
      FTarget: TRpgCharacter;
      FLastFrame: integer;
      FLastEffect: integer;
      FFullScreen: boolean;
      FFrameCount: integer;
      FSignal: TSimpleEvent;

      procedure move; reintroduce;
      procedure SetupFrame(currFrame: TAnimCell);
      procedure PlayEffect(frame: word);
   protected
      function InVisibleRect: boolean; override;
   public
      constructor Create(parent: TSpriteEngine; base: TAnimTemplate;
        target: TRpgCharacter; fullscreen: boolean; signal: TSimpleEvent); reintroduce;
      destructor Destroy; override;
      procedure Draw; override;
   end;

implementation
uses
   windows, sysUtils, OpenGL,
   turbu_OpenGL, turbu_2k_sprite_engine, turbu_constants, turbu_containers,
   rs_maps, rs_media,
   SDL, sdl_imageManager;

{ TAnimSprite }

constructor TAnimSprite.Create(parent: TSpriteEngine; base: TAnimTemplate;
  target: TRpgCharacter; fullscreen: boolean; signal: TSimpleEvent);
begin
   inherited Create(parent);
   FBase := base;
   self.z := 19; //one below TRpgImage's 20
   self.pinned := true;
   FFrameCount := FBase.frame.Last.frame;
   FTimer := TRpgTimestamp.Create(0);
   FTarget := target;
   FFullScreen := fullscreen;
   FSignal := signal;
end;

destructor TAnimSprite.Destroy;
begin
   if assigned(FSignal) then
      FSignal.SetEvent;
   FTimer.free;
   inherited;
end;

procedure TAnimSprite.Draw;
begin
   self.move;
   inherited Draw;
   if FLastFrame = FFrameCount then
      self.dead;
end;

function TAnimSprite.InVisibleRect: boolean;
begin
   result := false;
end;

procedure TAnimSprite.SetupFrame(currFrame: TAnimCell);
var
   newSprite: TAnimSpriteCell;
   sign: shortint;
begin
   newSprite := TAnimSpriteCell.Create(self);

   newSprite.pinned := true;
   newSprite.ImageName := 'Anim ' + FBase.filename;

   newSprite.ImageIndex := currFrame.ImageIndex;
   if FFullScreen then
   begin
      newSprite.x := currFrame.position.x + (engine.Canvas.Width div 2);
      newSprite.y := currFrame.position.y + (engine.Canvas.Height div 2);
   end else begin
      newSprite.x := (currFrame.position.x + FTarget.screenXP
        + FTarget.base.tiles[1].width div 2) - (newSprite.Width div 2);
      case FBase.yTarget of
         at_top: sign := -1;
         at_center: sign := 0;
         at_bottom: sign := 1;
         else raise ESpriteError.Create('Bad yTarget value');
      end;
      newSprite.y := (currFrame.position.y + FTarget.screenYP
        + (FTarget.base.tiles[1].height * sign)) - (newSprite.Height div 2);
   end;
   newSprite.Z := 1;
   newSprite.scaleX := currFrame.zoom.x / 100;
   newSprite.scaleY := currFrame.zoom.y / 100;
   newSprite.Red := commons.round(currFrame.color.rgba[1] * 1.275);
   newSprite.Green := commons.round(currFrame.color.rgba[2] * 1.275);
   newSprite.Blue := commons.round(currFrame.color.rgba[3] * 1.275);
   newSprite.Alpha := commons.round(currFrame.color.rgba[4] * 1.275);
   newSprite.Sat := commons.round(currFrame.saturation * 1.275);
end;

procedure extractColors(const effect: TAnimEffects; out r, g, b, a: byte); inline;
begin
   r := commons.round(effect.r * MULTIPLIER_31);
   g := commons.round(effect.g * MULTIPLIER_31);
   b := commons.round(effect.b * MULTIPLIER_31);
   a := commons.round(effect.a * MULTIPLIER_31);
end;

procedure TAnimSprite.PlayEffect(frame: word);
var
   currEffect: TAnimEffects;
   r, g, b, a: byte;
begin
   while (FLastEffect < FBase.effect.Count) and (FBase.effect[FLastEffect].frame = frame) do
   begin
      currEffect := FBase.effect[FLastEffect];
      if currEffect.sound.filename <> '' then
         rs_Media.playSoundData(currEffect.sound);
      case currEffect.flashWhere of
         fl_none: ;
         fl_target:
         begin
            extractColors(currEffect, r, g, b, a);
            FTarget.flash(r, g, b, a, 2, false);
         end;
         fl_screen:
         begin
            extractColors(currEffect, r, g, b, a);
            rs_maps.flashScreen(r, g, b, a, 2, false, false);
         end;
         else assert(false);
      end;
      inc(FLastEffect);
   end;
end;

procedure TAnimSprite.move;
var
   frame: word;
   currFrame: TAnimCell;
   tr: cardinal;
   frames: TRpgObjectList<TAnimCell>;
begin
   //timing/sync issues
   tr := FTimer.timeRemaining;
   if tr > 0 then
      Exit;
   inc(FLastFrame);
   frame := FLastFrame;

   //create new tiles
   ClearSpriteList;
   frames := FBase.frame.where(
      function(input: TAnimCell): boolean begin result := input.frame = frame end);
   try
      for currFrame in frames do
         SetupFrame(currFrame);
   finally
      frames.Free;
   end;

   if FLastEffect < FBase.effect.Count then
      PlayEffect(frame);
   FTimer.Free;
   FTimer := TRpgTimestamp.Create(32);
end;

{ TAnimSpriteCell }

procedure TAnimSpriteCell.PrepareShader(shaders: TdmShaders);
var
   handle: integer;
   gla: TGlArrayF4;
begin
   handle := shaders.ShaderProgram('default', 'tint', 'shift');
   shaders.UseShaderProgram(handle);
   shaders.SetUniformValue(handle, 'hShift', 0);
   shaders.SetUniformValue(handle, 'valMult', 1.0);
   gla[0] := self.Red / 128;
   gla[1] := self.Green / 128;
   gla[2] := self.Blue / 128;
   gla[3] := 1;
   shaders.SetUniformValue(handle, 'rgbValues', gla);
   shaders.SetUniformValue(handle, 'satMult', self.Sat / 128);
   glColor4f(1, 1, 1, self.Alpha / 128);
end;

procedure TAnimSpriteCell.DrawSelf(center: TSgFloatPoint; halfWidth, halfHeight: single; SpriteRect: TRect);
begin
   Self.Image.surface.bind;
   glEnable(GL_BLEND);
   glEnable(GL_ALPHA_TEST);
   glBegin(GL_QUADS);
      glTexCoord2i(spriteRect.Left,                    spriteRect.Top);
      glVertex2f(center.x - halfWidth * Self.ScaleX,   center.y - halfHeight * Self.ScaleY);
      glTexCoord2i(spriteRect.Left,                    spriteRect.Top + spriteRect.Bottom);
      glVertex2f(center.x - halfWidth * Self.ScaleX,   center.y + halfHeight * Self.ScaleY);
      glTexCoord2i(spriteRect.Left + spriteRect.Right, spriteRect.Top + spriteRect.Bottom);
      glVertex2f(center.x + halfWidth * Self.ScaleX,   center.y + halfHeight * Self.ScaleY);
      glTexCoord2i(spriteRect.Left + spriteRect.Right, spriteRect.Top);
      glVertex2f(center.x + halfWidth * Self.ScaleX,   center.y - halfHeight * Self.ScaleY);
   glEnd;
end;

procedure TAnimSpriteCell.DoDraw;
var
   current: integer;
   center: TSgFloatPoint;
   halfWidth, halfHeight: single;
   SpriteRect: TRect;
begin
   glGetIntegerv(GL_CURRENT_PROGRAM, @current);
   glPushAttrib(GL_CURRENT_BIT);
   PrepareShader(GSpriteEngine.ShaderEngine);
   halfWidth := self.Width / 2;
   halfHeight := self.Height / 2;
   center := sgPointF(self.X + halfWidth, self.Y + halfHeight);
   spriteRect := self.Image.spriteRect[self.ImageIndex];
   DrawSelf(center, halfWidth, halfHeight, spriteRect);
   glUseProgram(current);
   glPopAttrib;
end;

end.
