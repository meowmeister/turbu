unit rm2_turbu_tilesets;
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
   turbu_tilesets, chipset;

type
   T2k2TileSet = class helper for TTileSet
   private
      procedure convertTileGroups(filename: string; base: TChipSet);
   public
      constructor Convert(base: TChipSet; id: Integer);
   end;

implementation
uses
   classes, SysUtils, Generics.Collections,
   locate_files, archiveInterface, turbu_constants, turbu_database,
   sdl, sdl_13, sdl_image, sdlstreams, sdl_sprite;

{ T2k2TileSet }

constructor T2k2TileSet.Convert(base: TChipSet; id: Integer);
begin
   inherited Create;
   self.id := id;
   self.name := string(base.name);
   self.Records := TTileGroupList.Create;
   self.HiSpeed := base.hispeed;
   //convert images here
   convertTileGroups(string(base.filename), base);
end;

const
   TILESET_MAP: array[0..21] of TSdlRect = (
   (Left: 0; Top: 0; Right: 48; Bottom: 64),
   (Left: 0; Top: 64; Right: 48; Bottom: 64),
   (Left: 48; Top: 0; Right: 48; Bottom: 64),
   (Left: 48; Top: 64; Right: 48; Bottom: 64),

   (Left: 0; Top: 128; Right: 48; Bottom: 64),
   (Left: 0; Top: 192; Right: 48; Bottom: 64),
   (Left: 48; Top: 128; Right: 48; Bottom: 64),
   (Left: 48; Top: 192; Right: 48; Bottom: 64),

   (Left: 96; Top: 0; Right: 48; Bottom: 64),
   (Left: 96; Top: 64; Right: 48; Bottom: 64),
   (Left: 144; Top: 0; Right: 48; Bottom: 64),
   (Left: 144; Top: 64; Right: 48; Bottom: 64),

   (Left: 96; Top: 128; Right: 48; Bottom: 64),
   (Left: 96; Top: 192; Right: 48; Bottom: 64),
   (Left: 144; Top: 128; Right: 48; Bottom: 64),
   (Left: 144; Top: 192; Right: 48; Bottom: 64),

   (Left: 192; Top: 0; Right: 96; Bottom: 128),
   (Left: 192; Top: 128; Right: 96; Bottom: 128),
   (Left: 288; Top: 0; Right: 96; Bottom: 128),
   (Left: 288; Top: 128; Right: 96; Bottom: 128),
   (Left: 384; Top: 0; Right: 96; Bottom: 128),
   (Left: 384; Top: 128; Right: 96; Bottom: 128)
);

   TILESET_NAME: array[0..21] of string = ('water1', 'water3', 'water2', 'anims',
                 'border1', 'border2', 'border3', 'border4', 'border5', 'border6',
                 'border7', 'border8', 'border9', 'border10', 'border11',
                 'border12', 'lowtile1', 'lowtile2', 'lowtile3', 'hitile1',
                 'hitile2', 'hitile3');

   KEYNAME = '%s-%s';

{  TTileType = set of (tsBordered, tsAnimated);
   TTileAttribute = (taUp, taDown, taLeft, taRight, taCeiling, taOverhang, taCountertop);
   TTileAttributes = set of TTileAttribute; }

function convertAttributes(value: integer): TTileAttributes;
const
   passDown = 1;
   passLeft = 2;
   passRight = 4;
   passUp = 8;
   passCeiling = $10;
   passSquare = $20;
   COUNTERTOP = $40;
begin
   result := [];
   if (value and passDown) > 0 then
      include(result, taDown);
   if (value and passLeft) > 0 then
      include(result, taLeft);
   if (value and passRight) > 0 then
      include(result, taRight);
   if (value and passUp) > 0 then
      include(result, taUp);
   if (value and passCeiling) > 0 then
      include(result, taCeiling);
   if (value and passSquare) > 0 then
      include(result, taOverhang);
   if (value and COUNTERTOP) > 0 then
      include(result, taCountertop);
end;

procedure convertGroup(filename: string);
var
   surface: PSdlSurface;
   subsurface: PSdlSurface;
   currentRect: TSdlRect;
   i: integer;
   oFilename, lFilename: string;
   outfile: TStream;
   rw: PSdl_RWops;
   newGroup: TTileGroup;
begin
   oFilename := filename;
   outFile := nil;
   locate_files.findGraphic(filename, 'ChipSet');
   if (filename = '') or (ExtractFileExt(filename) = '.xyx') then
      Exit;
   pointer(surface) := sdl_image.IMG_Load(PAnsiChar(ansiString(filename)));
   assert(assigned(surface));
   assert(surface.Width = 480);
   assert(surface.Height = 256);
   assert(surface.Format.BitsPerPixel = 8);
   for i := low(TILESET_MAP) to high(TILESET_MAP) do
   begin
      currentRect := TILESET_MAP[i];
      subsurface := TSdlSurface.Create(currentRect.Right, currentRect.Bottom, 8);
      try
         subsurface.CopyPaletteFrom(surface);
         SDL_BlitSurface(surface, @currentRect, subSurface, nil);
         outFile := TMemoryStream.Create;
         rw := SDLStreamSetup(outFile);
         IMG_SavePNG_RW(pointer(subSurface), rw);
         SDL_FreeRW(rw);
         outFile.Seek(0, soFromBeginning);
         lFilename := format(KEYNAME, [oFilename, TILESET_NAME[i]]);
         GArchives[turbu_constants.IMAGE_ARCHIVE].writeFile(format('tileset\%s.png', [lFilename]), outfile);
         newGroup := TTileGroup.Create;
         newGroup.name := lFilename;
         newGroup.filename := lFilename;
         if i = 1 then
         begin
            newGroup.linkedFilename := format(KEYNAME, [oFilename, TILESET_NAME[0]]);
            newGroup.ocean := true;
         end
         else if i in [0, 2] then
            newGroup.linkedFilename := format(KEYNAME, [oFilename, TILESET_NAME[1]]);
         case i of
            0..2: newGroup.tileType := [tsBordered, tsAnimated];
            3: newGroup.tileType := [tsAnimated];
            4..15: newGroup.tileType := [tsBordered];
            else newGroup.tileType := [];
         end;
         if not (tsBordered in newGroup.tileType) then
            newGroup.dimensions := point(16, 16)
         else newGroup.dimensions := point(8, 8);
         GDatabase.tileGroup.Add(lFilename, newGroup);
      finally
         freeAndNil(outFile);
         subSurface.Free;
      end;
   end;
end;

const
   DATA_INDEX: array[0..19] of integer = (0, 1, 2, 3, 6, 7, 8, 9, 10, 11, 12,
                                          13, 14, 15, 16, 17, 18, 66, 114, 162);

procedure T2k2TileSet.convertTileGroups(filename: string; base: TChipSet);
var
   newRecord: TTileGroupRecord;
   i, j: integer;
begin
   if not GDatabase.tileGroup.ContainsKey(format(KEYNAME, [filename, TILESET_NAME[0]])) then
      convertGroup(filename);
   for i := low(TILESET_MAP) to high(TILESET_MAP) do
   begin
      newRecord := TTileGroupRecord.Create;
      newRecord.group := GDatabase.tileGroup[format(KEYNAME, [filename, TILESET_NAME[i]])];
      if i <= 16 then
         newRecord.layers := [0]
      else newRecord.layers := [1];
      if base.animation then
         newRecord.animDir := pmPingPong
      else newRecord.animDir := pmForward;
      newRecord.attributes := TAttributeList.Create;
      newRecord.terrain := TList<integer>.Create;
      case i of
         0..18:
         begin
            for j := DATA_INDEX[i] to DATA_INDEX[i + 1] - 1 do
            begin
               newRecord.attributes.Add(convertAttributes(base.blockData[j]));
               newRecord.terrain.Add(base.terrain[j]);
            end;
         end;
         19..21:
         begin
            for j := (i - 19) * 48 to ((i - 18) * 48) - 1 do
            begin
               newRecord.attributes.Add(convertAttributes(base.uBlockData[j]));
               newRecord.terrain.Add(-1);
            end;
         end;
      end;
      self.Records.Add(newRecord);
   end;
end;

end.
