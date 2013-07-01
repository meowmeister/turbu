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
unit turbu_2k_weather;

interface
uses
   turbu_defs,
   SDL_13, SDL_ImageManager, SDL_sprite, sdl_canvas;

type
   TWeatherSprite = class(TParticleSprite)
   private
      FErratic: boolean;
   public
      procedure Move(const MoveCount: Single); override;

      property erratic: boolean read FErratic write FErratic;
   end;

   TWeatherSystem = class(TSpriteEngine)
   private
      FSize: word;
      FType: TWeatherEffects;
      FIntensity: byte;
      FFogSprite: TParentSprite;

      procedure setIntensity(const Value: byte);
      procedure setType(const Value: TWeatherEffects);
      procedure addSprite;
      function CreateFog(r, g, b: byte): PSdlSurface;
      function CreateRain(r, g, b: byte): PSdlSurface;
      function CreateSnow(r, g, b: byte): PSdlSurface;
      procedure LoadFog;
      procedure WrapFog;
      procedure EnsureFogSprite;
   public
      constructor Create(parent: TSpriteEngine; images: TSdlImages; canvas: TSdlCanvas);
      procedure Draw; override;

      property weatherType: TWeatherEffects read FType write setType;
      property intensity: byte read FIntensity write setIntensity;
   end;

implementation
uses
   sysUtils, math,
   sg_defs;

const
   WEATHER_POWER: array[TWeatherEffects] of byte = (0, 20, 45, 0, 0);
   RAINFALLX = -1.6;
   RAINFALLY = 5.1;
   SNOWFALL = 2.3;
   RAIN_DECAYRATE = 0.035;
   SNOW_DECAYRATE = 0.004;
   SANDRAIN_DECAYRATE = 0.06;
   FOGSIZE: TsgPoint = (x: 64; y: 64);
   SPAWN_RATE: array[TWeatherEffects] of byte = (0, 3, 2, 0, 0);

{ TWeatherSystem }

constructor TWeatherSystem.Create(parent: TSpriteEngine; images: TSdlImages; canvas: TSdlCanvas);
var
   i: integer;
begin
   inherited Create(parent, Canvas);
   self.Z := 21; //one higher than TRpgImage's 20
   self.Images := images;
   for i := 1 to 6 do
      TSdlImage.Create(self.Canvas.Renderer, CreateFog(242, 255, 242), 'fog' + intToStr(i), images);
   for i := 1 to 6 do
      TSdlImage.Create(self.Canvas.Renderer, CreateFog(255, 240, 183), 'sand' + intToStr(i), images);
   TSdlImage.Create(self.Canvas.Renderer, CreateRain(255, 255, 255), 'rain', images);
   TSdlImage.Create(self.Canvas.Renderer, CreateRain(180, 170, 92), 'sandrain', images);
   TSdlImage.Create(self.Canvas.Renderer, CreateSnow(255, 255, 255), 'snow', images);
end;

procedure putPixel(surface: PSdlSurface; x, y, pixel: integer);
var
   bpp: integer;
   p: PByte;
begin
   bpp := surface.Format.BytesPerPixel;
   p := PByte(nativeInt(surface.pixels) + (y * surface.Pitch) + (x * bpp));
   case bpp of
      1: p^ := byte(pixel);
      2: PWord(p)^ := word(pixel);
      3: assert(false);
      4: PInteger(p)^ := pixel;
   end;
end;

function TWeatherSystem.CreateFog(r, g, b: byte): PSdlSurface;
var
   x, y, pixel: integer;
begin
   result := TSdlSurface.Create(FOGSIZE.x, FOGSIZE.y, 32);
   try
      if result.MustLock then
         result.LockSurface;
      for y := 0 to 63 do
         for x := 0 to 63 do
         begin
            pixel := SDL_MapRGBA(result.Format, r, g, b, random(9) + 74);
            putPixel(result, x, y, pixel);
         end;
      if result.MustLock then
         result.UnlockSurface;
   except
      result.Free;
      raise;
   end;
end;

function TWeatherSystem.CreateRain(r, g, b: byte): PSdlSurface;
var
   y, pixel: integer;
begin
   result := TSdlSurface.Create(8, 20, 32);
   try
      if result.MustLock then
         result.LockSurface;
      for y := 1 to 18 do
      begin
         pixel := SDL_MapRGBA(result.Format, r, g, b, 128);
         putPixel(result, 7 - (y div 3), y, pixel);
      end;
      if result.MustLock then
         result.UnlockSurface;
   except
      result.Free;
      raise;
   end;
end;

function TWeatherSystem.CreateSnow(r, g, b: byte): PSdlSurface;
var
   pixel: integer;
begin
   result := TSdlSurface.Create(2, 2, 32);
   try
      if result.MustLock then
         result.LockSurface;
      pixel := SDL_MapRGBA(result.Format, r, g, b, 190);
      putPixel(result, 0, 0, pixel);
      putPixel(result, 0, 1, pixel);
      putPixel(result, 1, 0, pixel);
      putPixel(result, 1, 1, pixel);
      if result.MustLock then
         result.UnlockSurface;
   except
      result.Free;
      raise;
   end;
end;

procedure TWeatherSystem.addSprite;
var
   sprite: TWeatherSprite;
begin
   sprite := TWeatherSprite.Create(self);
   sprite.Z := 2;
   sprite.UpdateSpeed := 1;
   sprite.x := random(canvas.Width) + random(60);
   sprite.Y := random(canvas.Height) - 30;
   sprite.pinned := true;
   case FType of
      we_none: assert(false);
      we_rain:
      begin
         sprite.VelocityX := RAINFALLX;
         sprite.VelocityY := RAINFALLY;
         sprite.Decay := RAIN_DECAYRATE;
         sprite.LifeTime := 0.5;
         sprite.ImageName := 'rain'
      end;
      we_snow:
      begin
         sprite.VelocityX := random + random - 1;
         sprite.VelocityY := SNOWFALL;
         sprite.Decay := SNOW_DECAYRATE;
         sprite.ImageName := 'snow';
         sprite.erratic := true;
      end;
      we_sand:
      begin
         sprite.VelocityX := (random() - 0.5) * RAINFALLY * 2;
         sprite.VelocityY := RAINFALLY;
         sprite.Decay := SANDRAIN_DECAYRATE;
         sprite.LifeTime := 0.5;
         sprite.ImageName := 'sandrain';
         sprite.Y := -10;
      end
      else ;
   end;
end;

procedure TWeatherSystem.Draw;
var
   i: integer;
begin
   if (FType = we_none) then
      Exit;

   self.Dead;
   for I := FSpriteList.Count to min(FSize - 1, FSpriteList.Count + (SPAWN_RATE[FType] * FIntensity)) do
      self.addSprite;
   if FType in [we_fog, we_sand] then
      LoadFog;
   for I := 0 to FSpriteList.Count - 1 do
      FSpriteList[i].Move(0);
   inherited Draw;
end;

procedure TWeatherSystem.EnsureFogSprite;
begin
   if FFogSprite = nil then
   begin
      FFogSprite := TParentSprite.Create(self);
      FFogSprite.Z := 1;
   end;
end;

procedure TWeatherSystem.WrapFog;
var
   fog: TSprite;
   cw, ch: integer;
begin
   cw := self.Canvas.Width;
   ch := self.Canvas.Height;
   for fog in FFogSprite.SpriteList do
   begin
      if fog.x + fog.Width <= 0 then
         fog.X := fog.X + cw
      else if fog.X >= cw then
         fog.X := fog.X - cw;
      if fog.y + fog.Height <= 0 then
         fog.y := fog.Y + ch
      else if fog.y >= ch then
         fog.Y := fog.Y - ch;
   end;
end;

procedure TWeatherSystem.LoadFog;
var
   i, fogW, fogH, x, y: integer;
   newFog: TWeatherSprite;
   vx, vy: single;
begin
   EnsureFogSprite;
   fogW := (self.Canvas.Width div FOGSIZE.x);
   fogH := (self.Canvas.Height div FOGSIZE.y);
   if self.Canvas.Width mod FOGSIZE.x <> 0 then
      inc(fogW);
   if self.Canvas.Height mod FOGSIZE.y <> 0 then
      inc(fogH);
   if FFogSprite.Count <> (fogw + 2) * (fogh + 2) * FIntensity then
   begin
      FFogSprite.Clear;
      vx := random + random - 1;
      vy := random + random - 1;
      for i := 1 to FIntensity do
         for y := -1 to fogH do
            for x := -1 to fogW do
            begin
               newFog := TWeatherSprite.Create(FFogSprite);
               newFog.VelocityX := vx;
               newFog.VelocityY := vy;
               newFog.Z := 1;
               newFog.X := x * FOGSIZE.x;
               newFog.Y := y * FOGSIZE.y;
               if FType = we_fog then
                  newFog.ImageName := 'fog' + intToStr(random(6) + 1)
               else newFog.ImageName := 'sand' + intToStr(random(6) + 1)
            end;
   end
   else wrapFog;
end;

procedure TWeatherSystem.setIntensity(const Value: byte);
begin
   FIntensity := Value;
   FSize := FIntensity * WEATHER_POWER[FType];
end;

procedure TWeatherSystem.setType(const Value: TWeatherEffects);
var
   i: integer;
begin
   if Value = FType then
      Exit;

   FType := Value;
   for i := 0 to FSpriteList.Count - 1 do
      TSprite(FSpriteList[i]).dead;
   FSize := FIntensity * WEATHER_POWER[FType];
   if value = we_none then
      self.dead;
   if not (value in [we_fog, we_sand]) then
      FFogSprite := nil;
end;

{ TWeatherSprite }

procedure TWeatherSprite.Move(const MoveCount: Single);
begin
   inherited Move(MoveCount);
   Alpha := max(round(255 * LifeTime), 0);
   if FErratic then
      self.X := self.X + random + random - 1;
   if (x < 0) or (y > Engine.Canvas.Height) then
      self.Dead;
end;

end.
