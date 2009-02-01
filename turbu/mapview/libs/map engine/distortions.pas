unit distortions;
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
   types,
   dxTextures, rpg_image;

procedure drawWave(image: TDxBaseTexture; source: TRect; amp, period, phase, DrawFx: Integer); overload;
procedure drawWave(image: TRpgImage; amp, period, phase, DrawFx: Integer); overload;

implementation
uses
   commons, chipset_graphics,
   AsphyreDef, {AsphyreImages} SDL_ImageManager;

procedure drawWave(image: TDxBaseTexture; source: TRect; amp, period, phase, DrawFx: Integer);
var
   i, shift: integer;
   width, height: word;
   viewRect: TRect;
begin
   width := image.Size.X;
   height := image.Size.Y;
   for i := 0 to Height - 1 do
   begin
      shift := round(amp * sin((phase + i) / period));
      viewRect := rect(source.Left, i, Width, 1);
//fixme
{      GGameEngine.Canvas.DrawRectStretch(image, source.left + shift, source.top + i, width, 1, viewRect, clWhite4, drawFx);}
   end;
end;

procedure drawWave(image: TRpgImage; amp, period, phase, DrawFx: Integer);
var
   i, shift: integer;
   width, height: word;
   viewRect: TRect;
   base: TSdlImage;
   x, y: single;
begin
   width := image.patternWidth;
   height := image.patternHeight;
   x := image.x - (width / 2);
   y := image.y - (height / 2);
   base := image.engine.Images.Image[image.ImageName];
   for i := 0 to Height - 1 do
   begin
      shift := round(amp * sin((phase + i) / period));
      viewRect := rect(commons.round(x), i, Width, 1);
//fixme
{      GGameEngine.Canvas.DrawRectStretch(base, image.PatternIndex, x + shift, i + y, width, 1.28,
                         viewRect.Left, viewRect.Top, viewRect.Right, viewRect.Bottom,
                         image.mirrorX, image.MirrorY, cRGB4(image.Red, image.Green, image.Blue, image.Alpha), image.DrawFx);}
   end;
end;

end.
