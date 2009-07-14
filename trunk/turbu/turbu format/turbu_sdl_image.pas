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
      FOrigSurface: PSdlSurface;
   protected
      procedure processImage(image: PSdlSurface); override;
   public
      destructor Destroy; override;
      property Texture: TSdlTexture read FSurface;
      property surface: PSdlSurface read FOrigSurface;
   end;

implementation

{ TRpgSdlImage }

destructor TRpgSdlImage.Destroy;
begin
   FOrigSurface.Free;
   inherited;
end;

procedure TRpgSdlImage.processImage(image: PSdlSurface);
var
   colorkey: TSDL_Color;
begin
   if assigned(image.format.palette) then
   begin
      colorkey := image.format.palette.colors^[0];
      image.ColorKey := SDL_MapRGB(image.format, colorkey.r, colorkey.g, colorkey.b)
   end;
   FOrigSurface := image;
   FOrigSurface.AcquireReference;
end;

end.
