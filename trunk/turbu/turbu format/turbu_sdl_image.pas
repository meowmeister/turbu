unit turbu_sdl_image;
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
   SDL_ImageManager,
   SDL_13;

type
   TRpgSdlImage = class(TSdlImage)
   private
      FTexture: TSdlTexture;
   public
      procedure setup(filename, imagename: string; container: TSdlImages; spriteSize: TPoint); override;
      property Texture: TSdlTexture read FTexture write FTexture;
   end;

implementation

{ TRpgSdlImage }
uses
   turbu_constants,
   sdl_canvas;

resourcestring
   BAD_LOAD = 'Unable to load image to video memory!';

procedure TRpgSdlImage.setup(filename, imagename: string; container: TSdlImages; spriteSize: TPoint);
var
   dummy: PSdlSurface;
   colorkey: ^TSDL_Color;
begin
   inherited setup(filename, imagename, container, spriteSize);
   dummy := TSdlSurface.Convert(FSurface, FSurface.format);
   FSurface.Free;
   FSurface := dummy;
   FMustLock := FSurface.MustLock;
   if assigned(FSurface.format.palette) then
   begin
      colorkey := @FSurface.format.palette.colors^[0];
      FSurface.ColorKey := SDL_MapRGB(FSurface.format, colorkey.r, colorkey.g, colorkey.b)
   end;
end;

end.
