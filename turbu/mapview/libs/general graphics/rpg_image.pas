unit rpg_image;
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
   commons, types, timing,
   {AsphyreDef, AsphyreSprite} SG_defs, SDL_sprite;

type
   TImageEffects = (ie_none, ie_rotate, ie_wave);

   TRpgImage = class(TSprite)
   private
{      FRefPoint: TPoint2;
      FRefTarget: TPoint2;}
      FAlphaTarget: byte;
      FSaturation: byte;
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

      function getTimer: cardinal;
      procedure setTimer(const Value: cardinal);
      function getOpaque: word;
      procedure setOpaque(const Value: word);
      procedure setZoom(const value: word);
      procedure centerOn(x, y: real);
      procedure applyColor;
      procedure update;
      function getZoom: word;
   public
      constructor Create(engine: TSpriteEngine; name: string; x, y: integer; zoom: word; pinned: boolean); reintroduce;
      destructor Destroy; override;
      procedure applyImageColors(r, g, b, sat: byte); inline;
      procedure applyImageEffect(which: TImageEffects; power: byte);
      procedure Draw; override;
      procedure moveTo(x, y: integer; zoom, opacity: word);
      procedure Dead; override;

      property zoom: word read getZoom write setZoom;
      property opacity: word read getOpaque write setOpaque;
      property timer: cardinal read getTimer write setTimer;
   end;

const
   ROTATE_FACTOR = 30;

implementation
uses
   sysUtils,
   {chipset_graphics, script_engine, distortions,}
   {asphyreImages} SDL_ImageManager;

type
   PRpgColor = ^TRpgColor;

{ TRpgImage }

procedure TRpgImage.applyColor;
begin
//fixme
{   self.Red := min(commons.round(FColor.rgba[1] * 1.275), 255);
   self.Green := min(commons.round(FColor.rgba[2] * 1.275), 255);
   self.Blue := min(commons.round(FColor.rgba[3] * 1.275), 255);}
   self.FSaturation := min(commons.round(FColor.rgba[4] * 1.275), 255);
end;

procedure TRpgImage.applyImageColors(r, g, b, sat: byte);
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

procedure TRpgImage.applyImageEffect(which: TImageEffects; power: byte);
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

procedure TRpgImage.centerOn(x, y: real);
begin
//   FRefPoint := point2(x, y);
   if not self.pinned then
   begin
      x := x + Engine.WorldX;
      y := y + engine.WorldY;
   end;
//fixme
{   self.CenterX := x;
   self.CenterY := y;
   self.x := centerX;
   self.y := centerY;}
end;

constructor TRpgImage.Create(engine: TSpriteEngine; name: string; x, y: integer; zoom: word; pinned: boolean);
begin
   inherited Create(engine);
   imageName := name;
   self.pinned := pinned;
   self.scaleX := zoom / 100;
   self.scaleY := self.scaleX;
   FZoomTarget := scaleX;
//fixme
{   drawMode := 1;
   DrawFx := fxBright;}
   self.centerOn(x, y);
//   FRefTarget := point2(x, y);
   self.applyImageColors(100, 100, 100, 100);
end;

procedure TRpgImage.Dead;
begin
{   if self <> GImages[0] then
      inherited Dead;}
end;

destructor TRpgImage.Destroy;
var
   I: Integer;
begin
   //remove dangling pointers
{   for I := 0 to high(GImages) do
      if GImages[i] = self then
         GImages[i] := nil;}
   FTransitionTimer.Free;
   inherited;
end;

function TRpgImage.getZoom: word;
begin
   result := commons.round(scaleX * 100);
end;

procedure TRpgImage.moveTo(x, y: integer; zoom, opacity: word);
begin
//   FRefTarget := point2(x, y);
   self.zoom := zoom;
   self.opacity := 100 - min(opacity, 100);
end;

function TRpgImage.getOpaque: word;
begin
   result := commons.round(self.Alpha / 2.55);
end;

procedure TRpgImage.setOpaque(const Value: word);
begin
   FAlphaTarget := commons.round(value * 2.55);
end;

procedure TRpgImage.setZoom(const value: word);
begin
   FZoomTarget := value / 100;
end;

function TRpgImage.getTimer: cardinal;
begin
   if assigned(FTransitionTimer) then
      result := FTransitionTimer.timeRemaining
   else result := 0;
end;

procedure TRpgImage.setTimer(const Value: cardinal);
begin
   if assigned(FTransitionTimer) then
      FTransitionTimer.Free;
   FTransitionTimer := TRpgTimestamp.create(value);
end;

{$Q-}{$R-}
procedure TRpgImage.update;
var
   dummy: single;
   i: byte;
   timeRemaining: cardinal;
   oldcolor: cardinal;
begin
   if assigned(FTransitionTimer) then
      timeRemaining := FTransitionTimer.timeRemaining
   else timeRemaining := 0;
{   if (FRefPoint.x <> FRefTarget.x) or (FRefPoint.y <> FRefTarget.y) then
   begin
      moveTowards(timeRemaining, FRefPoint.x, FRefTarget.x);
      moveTowards(timeRemaining, FRefPoint.y, FRefTarget.y);
      centerOn(FRefPoint.x, FRefPoint.y);
   end;}
   oldcolor := FColor.color;
   for I := 1 to 4 do
      if FColor.rgba[i] <> FColorTarget.rgba[i] then
         moveTowards(timeRemaining, FColor.rgba[i], FColorTarget.rgba[i]);
      //end if
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
   //end if
end;
{$Q+}{$R+}

procedure TRpgImage.Draw;
var
   followX, followY: single;
   theImage: TSdlImage;
begin
   self.update;
   if FWavePower = 0 then
      inherited Draw
   else begin
      if pinned then
      begin
         followX := 0;
         followY := 0;
      end else begin
         followX := FEngine.WorldX;
         followY := FEngine.worldY;
      end;
//fixme
{      if (X > followX + VisibleArea.Left)  and
         (Y > followY + VisibleArea.Top)   and
         (X < followX + VisibleArea.Right) and
         (Y < followY + VisibleArea.Bottom)then}
      begin
         if not Visible then Exit;
         if ImageName = '' then Exit;
         theImage := FEngine.Images.Image[ImageName];
         if theImage = nil then
            Exit;

//fixme
//         distortions.drawWave(self, commons.round(FWavePower * 3), 5, FTag, drawFX);
         inc(FTag);
         if FTag > 3141590 then
            FTag := 0;
      end;
   end;
end;

end.
