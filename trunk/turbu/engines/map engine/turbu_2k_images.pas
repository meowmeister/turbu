unit turbu_2k_images;

interface
uses
   rsImport,
   commons, sg_defs, sdl_sprite, timing;

type
   TImageEffects = (ie_none, ie_rotate, ie_wave);

   TRpgImageSprite = class(TSprite)
   private
      FRefPoint: TSgFloatPoint;
      FRefTarget: TSgFloatPoint;
      FAlphaTarget: integer;
      FSaturation: integer;
      FTransitionTimer: TRpgTimestamp;
      FColor: TRpgColor;
      FColorTarget: TRpgColor;
      FZoomTarget: single;
      FRotationPower: single;
      FWavePower: single;
      FRotation: single;
      FRotationTarget: single;
      FWaveTarget: single;
      FTag: Integer;
      FCenterX, FCenterY: single;
      FMasked: boolean;

      function getTimer: integer;
      procedure setTimer(const Value: integer);
      function getOpaque: integer;
      procedure setOpaque(const Value: integer);
      procedure setZoom(const value: integer);
      procedure centerOn(x, y: real);
      procedure applyColor;
      procedure update;
      function getZoom: integer;
      procedure DrawQuad;
   protected
      procedure DoDraw; override;
   public
      constructor Create(engine: TSpriteEngine; const name: string; x, y: integer;
        zoom: integer; pinned, masked: boolean); reintroduce;
      destructor Destroy; override;
      procedure applyImageColors(r, g, b, sat: integer);
      procedure applyImageEffect(which: TImageEffects; power: integer);
      procedure Draw; override;
      procedure moveTo(x, y: integer; zoom, opacity: integer);
      procedure Dead; override;

      property zoom: integer read getZoom write setZoom;
      property opacity: integer read getOpaque write setOpaque;
      property timer: integer read getTimer write setTimer;
   end;

   TRpgImage = class(TObject)
   private
      FSprite: TRpgImageSprite;
      function getOpaque: integer;
      function getTimer: integer;
      function getZoom: integer;
      procedure setOpaque(const Value: integer);
      procedure setTimer(const Value: integer);
      procedure setZoom(const Value: integer);
   public
      [NoImport]
      constructor Create(engine: TSpriteEngine; const name: string; x, y: integer;
        zoom: integer; pinned, masked: boolean); reintroduce;
      destructor Destroy; override;
      procedure applyImageColors(r, g, b, sat: integer);
      procedure applyImageEffect(which: TImageEffects; power: integer);
      procedure moveTo(x, y: integer; zoom, opacity, duration: integer);
      procedure Erase;
      procedure Waitfor;

      [NoImport]
      property base: TRpgImageSprite read FSprite;
      property zoom: integer read getZoom write setZoom;
      property opacity: integer read getOpaque write setOpaque;
      property timer: integer read getTimer write setTimer;
    end;

implementation
uses
   sysUtils, Math, OpenGL, Types,
   turbu_2k_environment, turbu_OpenGL, dm_shaders, turbu_2k_sprite_engine,
   SDL_ImageManager;

const
   ROTATE_FACTOR = 30;

{ TRpgImage }

constructor TRpgImageSprite.Create(engine: TSpriteEngine; const name: string;
  x, y: integer; zoom: integer; pinned, masked: boolean);
begin
   inherited Create(engine);
   imageName := name;
   self.pinned := pinned;
   self.scaleX := zoom / 100;
   self.scaleY := self.scaleX;
   FZoomTarget := scaleX;
   FRenderSpecial := true;
   self.Z := 20;
   self.centerOn(x, y);
   FRefTarget := sgPointF(x, y);
   FMasked := masked;
   self.applyImageColors(100, 100, 100, 100);
   self.Alpha := 255;
   FAlphaTarget := 255;
end;

procedure TRpgImageSprite.Dead;
begin
   if self <> GEnvironment.Image[0].FSprite then
      inherited Dead;
end;

destructor TRpgImageSprite.Destroy;
begin
   FTransitionTimer.Free;
   inherited;
end;

procedure TRpgImageSprite.applyColor;
begin
   self.Red := min(commons.round(FColor.rgba[1] * 1.275), 255);
   self.Green := min(commons.round(FColor.rgba[2] * 1.275), 255);
   self.Blue := min(commons.round(FColor.rgba[3] * 1.275), 255);
   self.FSaturation := min(commons.round(FColor.rgba[4] * 1.275), 255);
end;

procedure TRpgImageSprite.applyImageColors(r, g, b, sat: integer);
var
  I: Integer;
begin
   FColorTarget.rgba[1] := min(r, 200);
   FColorTarget.rgba[2] := min(g, 200);
   FColorTarget.rgba[3] := min(b, 200);
   FColorTarget.rgba[4] := min(sat, 200);
   for I := 1 to 4 do
      FColor.rgba[i] := FColorTarget.rgba[i];
   applyColor;
end;

procedure TRpgImageSprite.applyImageEffect(which: TImageEffects; power: integer);
begin
   power := min(power, 10);
   case which of
      ie_none:
      begin
         FRotationPower := 0;
         FRotationTarget := 0;
         FWavePower := 0;
         FWaveTarget := 0;
      end;
      ie_rotate: FRotationTarget := power;
      ie_wave: FWaveTarget := power;
      else assert(false);
   end;
end;

procedure TRpgImageSprite.centerOn(x, y: real);
begin
   FRefPoint := sgPointF(x, y);
   FCenterX := x;
   FCenterY := y;
end;

procedure TRpgImageSprite.DrawQuad;
var
   halfwidth, halfheight: single;
   cx, cy: single;
   drawrect: TRect;
   current: integer;
   shaders: TdmShaders;
begin
   glGetIntegerv(GL_CURRENT_PROGRAM, @current);
   glPushAttrib(GL_CURRENT_BIT);
   shaders := GSpriteEngine.ShaderEngine;
   if FMasked then
      shaders.UseShaderProgram(shaders.ShaderProgram('default', 'defaultF'))
   else shaders.UseShaderProgram(shaders.ShaderProgram('default', 'noAlpha'));
   glBindTexture(GL_TEXTURE_RECTANGLE_ARB, Self.Image.surface.handle);
   glColor4f(1, 1, 1, self.alpha / 255);
   halfWidth := (self.Width / 2);
   halfheight := (self.Height / 2);
   if Pinned then
   begin
      cx := FCenterX - engine.worldX;
      cy := FCenterY - engine.WorldY;
   end
   else begin
      cx := FCenterX;
      cy := FCenterY;
   end;
   drawrect := self.GetDrawRect;
   glBegin(GL_QUADS);
      glTexCoord2i(drawRect.Left,                  drawRect.Top);
      glVertex2f(cx - halfWidth * Self.ScaleX,     cy - halfHeight * Self.ScaleY);
      glTexCoord2i(drawRect.Left,                  drawRect.Top + drawRect.Bottom);
      glVertex2f(cx - halfWidth * Self.ScaleX,     cy + halfHeight * Self.ScaleY);
      glTexCoord2i(drawRect.Left + drawRect.Right, drawRect.Top + drawRect.Bottom);
      glVertex2f(cx + halfWidth * Self.ScaleX,     cy + halfHeight * Self.ScaleY);
      glTexCoord2i(drawRect.Left + drawRect.Right, drawRect.Top);
      glVertex2f(cx + halfWidth * Self.ScaleX,     cy - halfHeight * Self.ScaleY);
   glEnd;
   glPopAttrib;
   shaders.UseShaderProgram(current);
end;

procedure TRpgImageSprite.DoDraw;
begin
   if FWavePower = 0 then
DrawQuad
   else begin
//fixme
//      distortions.drawWave(self, commons.round(FWavePower * 3), 5, FTag, drawFX);
DrawQuad;
      inc(FTag);
      if FTag > 3141590 then
         FTag := 0;
   end;
end;

function TRpgImageSprite.getZoom: integer;
begin
   result := commons.round(scaleX * 100);
end;

procedure TRpgImageSprite.moveTo(x, y: integer; zoom, opacity: integer);
begin
   FRefTarget := sgPointF(x, y);
   self.zoom := zoom;
   self.opacity := 100 - min(opacity, 100);
end;

function TRpgImageSprite.getOpaque: integer;
begin
   result := commons.round(self.Alpha / 2.55);
end;

procedure TRpgImageSprite.setOpaque(const Value: integer);
begin
   FAlphaTarget := commons.round(value * 2.55);
end;

procedure TRpgImageSprite.setZoom(const value: integer);
begin
   FZoomTarget := value / 100;
end;

function TRpgImageSprite.getTimer: integer;
begin
   if assigned(FTransitionTimer) then
      result := FTransitionTimer.timeRemaining
   else result := 0;
end;

procedure TRpgImageSprite.setTimer(const Value: integer);
begin
   if assigned(FTransitionTimer) then
      FTransitionTimer.Free;
   FTransitionTimer := TRpgTimestamp.create(value);
end;

{$Q-}{$R-}
procedure TRpgImageSprite.update;
var
   dummy: single;
   i: integer;
   timeRemaining: integer;
   oldcolor: cardinal;
begin
   if assigned(FTransitionTimer) then
      timeRemaining := FTransitionTimer.timeRemaining
   else timeRemaining := 0;
   if (FRefPoint.x <> FRefTarget.x) or (FRefPoint.y <> FRefTarget.y) then
   begin
      moveTowards(timeRemaining, FRefPoint.x, FRefTarget.x);
      moveTowards(timeRemaining, FRefPoint.y, FRefTarget.y);
      centerOn(FRefPoint.x, FRefPoint.y);
   end;
   oldcolor := FColor.color;
   for I := 1 to 4 do
      if FColor.rgba[i] <> FColorTarget.rgba[i] then
         moveTowards(timeRemaining, FColor.rgba[i], FColorTarget.rgba[i]);
   if FColor.color <> oldcolor then
      self.applyColor;
   if self.scaleX <> FZoomTarget then
   begin
      dummy := self.scaleX;
      moveTowards(timeRemaining, dummy, FZoomTarget);
      self.scaleX := dummy;
      self.scaleY := self.scaleX;
   end;
   if self.alpha <> FAlphaTarget then
   begin
      i := self.alpha;
      moveTowards(timeRemaining, i, FAlphaTarget);
      self.alpha := i;
   end;
   if (FRotation <> FRotationTarget) then
      moveTowards(timeRemaining, FRotation, FRotationTarget);
   if FRotation <> 0 then
      self.angle := self.angle + (FRotation / ROTATE_FACTOR)
   else self.angle := 0;
   if (FWavePower <> FWaveTarget) then //implement later
      moveTowards(timeRemaining, FWavePower, FWaveTarget);
end;
{$Q+}{$R+}

procedure TRpgImageSprite.Draw;
begin
   self.update;
   inherited Draw;
end;

{ TRpgImage }

constructor TRpgImage.Create(engine: TSpriteEngine; const name: string; x, y: integer;
  zoom: integer; pinned, masked: boolean);
begin
   FSprite := TRpgImageSprite.Create(engine, name, x, y, zoom, pinned, masked);
end;

destructor TRpgImage.Destroy;
begin
   GEnvironment.RemoveImage(self);
   FSprite.Free;
   inherited;
end;

procedure TRpgImage.Erase;
begin
   FSprite.Dead;
end;

procedure TRpgImage.applyImageColors(r, g, b, sat: integer);
begin
   FSprite.applyImageColors(r, g, b, sat);
end;

procedure TRpgImage.applyImageEffect(which: TImageEffects; power: integer);
begin
   FSprite.applyImageEffect(which, power);
end;

function TRpgImage.getOpaque: integer;
begin
   result := FSprite.getOpaque;
end;

function TRpgImage.getTimer: integer;
begin
   result := FSprite.getTimer;
end;

function TRpgImage.getZoom: integer;
begin
   result := FSprite.getZoom;
end;

procedure TRpgImage.moveTo(x, y: integer; zoom, opacity, duration: integer);
begin
   FSprite.moveTo(x, y, zoom, opacity);
   FSprite.timer := duration * 100;
end;

procedure TRpgImage.setOpaque(const Value: integer);
begin
   FSprite.setOpaque(value);
end;

procedure TRpgImage.setTimer(const Value: integer);
begin
   FSprite.setTimer(value);
end;

procedure TRpgImage.setZoom(const Value: integer);
begin
   FSprite.setZoom(value);
end;

procedure TRpgImage.Waitfor;
begin
   //TODO: Implement this
end;

end.
