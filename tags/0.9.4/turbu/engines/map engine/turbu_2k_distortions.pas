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
unit turbu_2k_distortions;

interface
uses
   types,
   sdl_canvas, turbu_2k_images;

procedure drawWave(image: TSdlRenderTarget; source: TRect; amp, period, phase: Integer); overload;
procedure drawWave(image: TRpgImage; amp, period, phase, DrawFx: Integer); overload;

implementation
uses
   commons, turbu_2k_sprite_engine,
   SG_Defs, SDL_ImageManager;

procedure drawWave(image: TSdlRenderTarget; source: TRect; amp, period, phase: Integer);
var
   i, shift: integer;
   width, height: word;
   viewRect: TRect;
begin
   width := image.width;
   height := image.height;
   for i := 0 to Height - 1 do
   begin
      shift := round(amp * sin((phase + i) / period));
      viewRect := rect(source.Left, i, Width, 1);
      GSpriteEngine.Canvas.DrawRectTo(image, rect(source.left + shift, source.top + i, width, 1), viewRect);
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
{$MESSAGE WARN 'DrawWave(TRpgImage) is missing support for flips, angles, and tinting...'}
   base := image.base.Image;
   width := base.textureSize.x;
   height := base.textureSize.y;
   x := image.base.x - (width / 2);
   y := image.base.y - (height / 2);

   for i := 0 to Height - 1 do
   begin
      shift := round(amp * sin((phase + i) / period));
      viewRect := rect(commons.round(x), i, Width, 1);
      GSpriteEngine.Canvas.DrawRectTo(base, rect(round(x + shift), round(i + y), width, height),
                         viewRect);
   end;
end;

end.
