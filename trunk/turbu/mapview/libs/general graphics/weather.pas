unit weather;
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
   SDL_canvas, SDL_ImageManager, SDL_sprite;
   {AsphyreCanvas, AsphyreImages, AsphyreSprite;}

type
   TWeatherEffects = (we_none, we_rain, we_snow, we_fog, we_sand);

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

      procedure setIntensity(const Value: byte);
      procedure setType(const Value: TWeatherEffects);
      procedure addSprite;
   public
      constructor Create(images: TSdlImages);
      procedure Draw; override;

      property weatherType: TWeatherEffects read FType write setType;
      property intensity: byte read FIntensity write setIntensity;
   end;

implementation
uses
   sysUtils, math, contnrs,
   chipset_graphics,
   SDL,
   AsphyreDef;

const
   WEATHER_POWER: array[TWeatherEffects] of byte = (0, 20, 45, 0, 0);
   RAINFALLX = -1.6;
   RAINFALLY = 5.1;
   SNOWFALL = 2.3;
   RAIN_DECAYRATE = 0.035;
   SNOW_DECAYRATE = 0.004;
   SPAWN_RATE: array[TWeatherEffects] of byte = (0, 3, 2, 0, 0);

{ TWeatherSystem }

procedure TWeatherSystem.addSprite;
var
   dummy: TWeatherSprite;
begin
   dummy := TWeatherSprite.Create(self);
   dummy.Z := 1;
   dummy.UpdateSpeed := 1;
   dummy.x := random(canvas.Width) + random(60);
   dummy.Y := random(canvas.Height) - 30;
   dummy.pinned := true;
   case FType of
      we_none: assert(false);
      we_rain:
      begin
         dummy.VelocityX := RAINFALLX;
         dummy.VelocityY := RAINFALLY;
         dummy.Decay := RAIN_DECAYRATE;
         dummy.LifeTime := 0.5;
         dummy.ImageName := 'rain';
      end;
      we_snow:
      begin
         dummy.VelocityX := random + random - 1;
         dummy.VelocityY := SNOWFALL;
         dummy.Decay := SNOW_DECAYRATE;
         dummy.ImageName := 'snow';
         dummy.erratic := true;
      end;
      else ;
   end;
end;

constructor TWeatherSystem.Create(images: TSdlImages);
begin
   inherited Create(GGameEngine);
   self.Images := images;
//fixme
{   images.AddFromASDb('rain', GDataArchive, aqHigh, alMask);
   images.AddFromASDb('snow', GDataArchive, aqHigh, alNone);}
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
   for I := 0 to FSpriteList.Count - 1 do
      TWeatherSprite(FSpriteList[i]).Move(0);
   inherited Draw;
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
   //end if
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
