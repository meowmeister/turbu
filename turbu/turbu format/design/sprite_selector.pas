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

unit sprite_selector;

interface

uses
   StdCtrls, Controls, types, Classes, ExtCtrls,
   sdl_frame, base_selector,
   turbu_tilesets, sdl_frame_helper;

type
   TfrmSpriteSelector = class(TfrmBaseSelector)
      procedure lstFilenamesClick(Sender: TObject); override;
   private
      FTileset: TTileset;
      procedure loadTileGroups;
      procedure loadTileset(grouprec: TTileGroupRecord);
      procedure loadSprite(filename: string);
      class procedure SelectSprite(tileset: TTileset; var current: string; var frame: integer);
   protected
      procedure ShowLoad; override;
   public
      class procedure SelectSpriteInto(sdlFrame: TSdlFrame; var current: string;
        var frame: integer; tileset: TTileset = nil; rename: TRenameProc = nil);
   end;

implementation
uses
   Generics.Collections, SysUtils,
   archiveInterface, turbu_containers,
   sdl_13, sdl_ImageManager, sg_defs;

{$R *.dfm}

{ TfrmSpriteSelector }

procedure TfrmSpriteSelector.ShowLoad;
begin
   if assigned(FTileset) then
      loadTileGroups;
end;

procedure TfrmSpriteSelector.loadSprite(filename: string);
var
   oFilename: string;
   image: TSdlImage;
   logicalSize: TSgPoint;
begin
   oFilename := filename;
   if not imgSelector.ContainsName(oFilename) then
   begin
      filename := format('mapsprite\%s.png', [oFilename]);
      assert(GArchives[IMAGE_ARCHIVE].fileExists(filename));
      AddNewImage(filename, oFilename);
   end;

   image := imgSelector.Images.Image[oFilename];
   logicalSize := image.TextureSize * SgPoint(3, 4);
   finishLoading(image, logicalSize, logicalSize * SELECTOR_SCALE,
      procedure
         var
            i: integer;
            position: TSgPoint;
         begin
            position := ORIGIN;
            imgSelector.FillColor(FBackground, $FF);
            for I := 0 to image.count do
            begin
               image.DrawSpriteTo(rect(position, image.textureSize), i);
               if position.x + (2 * image.textureSize.x) <= imgSelector.LogicalWidth then
                  inc(position.x, image.textureSize.x)
               else begin
                  position.x := 0;
                  inc(position.y, image.textureSize.y);
               end;
            end;
         end);
end;

procedure TfrmSpriteSelector.loadTileGroups;
var
   tilegroups: TRpgObjectList<TTileGroupRecord>;
   groupRec: TTileGroupRecord;
begin
   tilegroups := FTileset.Records.where(turbu_tilesets.upperLayerFilter);
   try
      for grouprec in tilegroups do
         lstFilenames.AddItem('*Tile group ' + intToStr(grouprec.id), grouprec);
   finally
      tilegroups.Free;
   end;
end;

procedure TfrmSpriteSelector.loadTileset(grouprec: TTileGroupRecord);
var
   filename, oFilename: string;
   image: TSdlImage;
   size: TSgPoint;
begin
   oFilename := groupRec.group.filename;
   if not imgSelector.ContainsName(oFilename) then
   begin
      filename := format('tileset\%s.png', [oFilename]);
      assert(GArchives[IMAGE_ARCHIVE].fileExists(filename));
      addNewImage(filename, oFilename);
   end;

   image := imgSelector.images.Image[oFilename];
   size := TSgPoint(image.surface.size) * SELECTOR_SCALE;
   FinishLoading(image, size, size,
      procedure
      begin
         imgSelector.FillColor(FBackground, $FF);
         imgSelector.DrawTexture(oFilename);
      end);
end;

procedure TfrmSpriteSelector.lstFilenamesClick(Sender: TObject);
var
   filename: string;
begin
   filename := lstFilenames.Items[lstFilenames.ItemIndex];
   if filename[1] = '*' then
   begin
      loadTileset(lstFilenames.Items.Objects[lstFilenames.ItemIndex] as TTileGroupRecord);
      FFilename := stringReplace(filename, '*Tile group ', '*', []);
   end
   else begin
      loadSprite(filename);
      FFilename := filename;
   end;
   inherited lstFilenamesClick(sender);
end;

class procedure TfrmSpriteSelector.SelectSprite(tileset: TTileset;
  var current: string; var frame: integer);
begin
   doSelection(current, 'mapsprite', frame, self,
     procedure(form: TfrmBaseSelector)
     begin
       (form as TfrmSpriteSelector).FTileset := tileset;
     end);
end;

class procedure TfrmSpriteSelector.SelectSpriteInto(sdlFrame: TSdlFrame;
  var current: string; var frame: integer; tileset: TTileset; rename: TRenameProc);
begin
   if current = '' then
      sdlFrame.setSprite(current, frame, tileset, rename);
   selectSprite(tileset, current, frame);
   sdlFrame.setSprite(current, frame, tileset, rename);
end;

end.
