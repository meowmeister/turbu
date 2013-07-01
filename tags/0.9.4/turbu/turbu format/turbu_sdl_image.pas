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
   SG_defs, SDL_ImageManager, SDL, SDL_13;

type
   TRpgSdlImage = class(TSdlImage)
   private
      FOrigSurface: PSdlSurface;
    FSurface: TSdlTexture;

   protected
      procedure processImage(image: PSdlSurface); override;
   public
      constructor CreateSprite(renderer: TSdlRenderer; surface: PSdlSurface; imagename: string; container: TSdlImages); overload;
      constructor CreateSprite(renderer: TSdlRenderer; rw: PSDL_RWops; extension, imagename: string; container: TSdlImages); overload;
      constructor CreateSprite(renderer: TSdlRenderer; rw: PSDL_RWops; extension, imagename: string; container: TSdlImages; spriteSize: TSgPoint); overload; override;
      destructor Destroy; override;
      property Texture: TSdlTexture read FSurface;
      property surface: PSdlSurface read FOrigSurface;
   end;

implementation
uses
   turbu_tbi_lib;

function GetTBIInfo(surface: PSdlSurface): TTbiInfo;
begin
   TBI_prepare(surface);
   result := TObject(surface.Tag) as TTbiInfo;
   if not assigned(result) then
      raise ETbiError.Create('TBI info not found');
end;

{ TRpgSdlImage }

constructor TRpgSdlImage.CreateSprite(renderer: TSdlRenderer; surface: PSdlSurface; imagename: string;
  container: TSdlImages);
begin
   inherited CreateSprite(renderer, surface, imagename, container, GetTBIInfo(surface).size);
end;

constructor TRpgSdlImage.CreateSprite(renderer: TSdlRenderer; rw: PSDL_RWops; extension, imagename: string; container: TSdlImages);
begin
   inherited CreateSprite(renderer, rw, extension, imagename, container, EMPTY);
   self.TextureSize := GetTBIInfo(surface).size;
end;

constructor TRpgSdlImage.CreateSprite(renderer: TSdlRenderer; rw: PSDL_RWops;
  extension, imagename: string; container: TSdlImages; spriteSize: TSgPoint);
begin
   inherited CreateSprite(renderer, rw, extension, imagename, container, spriteSize);
   if spriteSize = EMPTY then
      self.TextureSize := GetTBIInfo(surface).size;
end;

destructor TRpgSdlImage.Destroy;
begin
   GetTBIInfo(FOrigSurface).Free;
   FOrigSurface.Free;
   inherited;
end;

procedure TRpgSdlImage.processImage(image: PSdlSurface);
begin
   FOrigSurface := image;
   FOrigSurface.AcquireReference;
end;

end.
