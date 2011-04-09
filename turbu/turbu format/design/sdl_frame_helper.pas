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

unit sdl_frame_helper;

interface
uses
   sdl_frame, sg_defs, turbu_tilesets, turbu_sdl_image, turbu_sprites;

type
   TRenameProc = reference to procedure(const name: string);

   TSdlFrameSpriteHelper = class helper for TSdlFrame
   private
      function EnsureImage(const filename: string): TRpgSdlImage;
      procedure DrawSpriteCentered(image: TRpgSdlImage; frame: integer);
   public
      procedure SetSprite(name: string; frame: integer; tileset: TTileset = nil;
        rename: TRenameProc = nil);
      procedure SetPortrait(name: string; frame: integer; flipped: boolean);
      procedure loadMapSprite(name: string; id: integer; frame: byte;
        var data: TSpriteData);
      procedure DemoWalk(name: string; id: integer; const matrix: TMoveMatrix;
        var data: TSpriteData; var position: TSgPoint; var repCounter: integer);
   end;

implementation
uses
   Classes, types, SysUtils,
   commons, turbu_constants, ArchiveInterface, turbu_tbi_lib,
   sdl_13;

const
   FILENAME_STRING = '%s\%s.png';

{ TSdlFrameSpriteHelper }

procedure TSdlFrameSpriteHelper.DemoWalk(name: string; id: integer;
  const matrix: TMoveMatrix; var data: TSpriteData; var position: TSgPoint;
  var repCounter: integer);
const
   REPS = 3;
begin
   self.loadMapSprite(name, id, nextPosition(matrix, position), data);
   if position.y = 0 then
   begin
      repCounter := (repCounter + 1) mod REPS;
      if repCounter = 0 then
         position.x := (position.x + 1) mod length(matrix);
   end;
end;

procedure TSdlFrameSpriteHelper.DrawSpriteCentered(image: TRpgSdlImage; frame: integer);
var
   spriteRect, destRect: TRect;
begin
   spriteRect := image.spriteRect[frame];
   destRect.left := (self.Width div 2) - (spriteRect.right);
   destRect.top := (self.height div 2) - (spriteRect.bottom);
   destRect.BottomRight := TSgPoint(spriteRect.BottomRight) * 2;
   self.fillColor(image.surface.Format.palette.colors[image.surface.ColorKey], 255);
   self.DrawTexture(image.Texture, @spriteRect, @destRect);
   self.Flip;
end;

function TSdlFrameSpriteHelper.EnsureImage(const filename: string): TRpgSdlImage;
var
   stream: TStream;
begin
   if not self.ContainsName(filename) then
   begin
      stream := GArchives[IMAGE_ARCHIVE].getFile(filename);
      try
         result := TRpgSdlImage.CreateSprite(self.renderer, loadFromTBI(stream), filename, self.Images);
         assert(assigned(result.Texture.ptr));
      finally
         stream.Free;
      end;
   end
   else result := self.Images.image[filename] as TRpgSdlImage;
end;

procedure TSdlFrameSpriteHelper.SetPortrait(name: string; frame: integer; flipped: boolean);
var
   spriteRect: TRect;
   image : TRpgSdlImage;
begin
   if name = '' then
   begin
      self.Clear;
      Exit;
   end;

   self.ClearTextures;
   name := format(FILENAME_STRING, ['portrait', name]);
   image := EnsureImage(name);
   spriteRect := image.spriteRect[frame];
   self.fillColor(image.surface.Format.palette.colors[image.surface.ColorKey], 255);
   if flipped then
      self.DrawTextureFlipped(image.Texture, [sdlfHoriz], @spriteRect, nil)
   else self.DrawTexture(image.Texture, @spriteRect, nil);
   self.Flip;
end;

procedure TSdlFrameSpriteHelper.SetSprite(name: string; frame: integer;
  tileset: TTileset; rename: TRenameProc);
var
   image : TRpgSdlImage;
begin
   if name = '' then
   begin
      name := '*' + intToStr(tileset.Records.firstIndexWhere(turbu_tilesets.upperLayerFilter));
      if assigned(rename) then
         rename(name);
   end;
   if name[1] = '*' then
   begin
      name := tileset.Records[strToInt(copy(name, 2, 3))].group.filename;
      name := format(FILENAME_STRING, ['tileset', name]);
   end
   else name := format(FILENAME_STRING, ['mapsprite', name]);

   image := EnsureImage(name);
   DrawSpriteCentered(image, frame);
end;

procedure TSdlFrameSpriteHelper.loadMapSprite(name: string; id: integer; frame: byte; var data: TSpriteData);
var
   filename: string;
   image: TRpgSdlImage;
   fileStream: TStream;
begin
   data.name := name;
   data.moveMatrix := id;
   if name = '' then
   begin
      self.Clear;
      Exit;
   end;

   filename := format('mapsprite\%s.png', [data.name]);
   if self.IndexOfName(filename) = -1 then
   begin
      try
         fileStream := GArchives[IMAGE_ARCHIVE].getFile(filename);
         try
            image := TRpgSdlImage.CreateSprite(self.Renderer, loadFromTBI(fileStream), filename, self.Images, SPRITE_SIZE * SgPoint(1, 2));
         finally
            fileStream.Free;
         end;
      except
         on EArchiveError do
            image := TRpgSdlImage.CreateBlankSprite(self.Renderer, filename, self.Images, SPRITE_SIZE, 12);
      end;
      assert(frame in [0..image.count]);
   end
   else image := self.Images.image[filename] as TRpgSdlImage;

   DrawSpriteCentered(image, frame);
end;

end.
