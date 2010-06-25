unit turbu_2k_map_engine_D;
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
   Generics.Collections, types, classes,
   turbu_map_engine, turbu_2k_map_engine, turbu_database_interface,
   turbu_map_interface, turbu_tilesets, turbu_maps, turbu_2k_sprite_engine,
   turbu_map_sprites, mapobject_container,
   sdl_13, SG_defs;

type
   T2kMapEngineD = class(T2kMapEngine, IDesignMapEngine, IBreakable)
   private
      FTilesetListD: TList<TTileGroupPair>;
      FTileSize: TSgPoint;
      FDrawFrom: TSgPoint;
      FPaletteList: TList<integer>;
      FCurrentLayer: shortint;
      FAutosaveMaps: boolean;
      FObjectContainers: TMapObjectContainerList;
      FCursorPosition: TSgPoint;
      FHookedObject: TMapSprite;

      function loadTilesetD(const value: TTileSet): TList<TTileGroupPair>;
      procedure saveMap(value: TRpgMap);
      procedure saveAndClearMapCache;
      procedure DrawGrid;
      procedure DrawCursor;
      procedure PrepareContainers;
      procedure ClearContainers;
      procedure ArrowKey(key: word);
      function NewMapObjectID(engine: T2kSpriteEngine): integer;
      function GetCurrentMapObject: TMapSprite;
      procedure DoDelete;
   private //IBreakable
      procedure BreakSomething;
   private //IDesignMapEngine
      procedure initializeDesigner(window: TSdlWindowId; database: IRpgDatabase);
      function GetTilesetImageSize(const index: byte): TSgPoint;
      function GetTilesetImage(const index: byte): PSdlSurface;
      function DesignLoadMap(map: IMapMetadata): IRpgMap;
      function mapSize: TSgPoint;
      function mapPosition: TSgPoint;
      procedure scrollMap(const newPosition: TSgPoint);
      procedure setPaletteList(value: TList<integer>);
      procedure draw(const position: TSgPoint; new: boolean);
      procedure doneDrawing;
      procedure doubleClick;
      procedure rightClick(const position: TSgPoint);
      procedure SetCurrentLayer(const value: shortint);
      function GetCurrentLayer: shortint;
      function getAutosaveMaps: boolean;
      procedure setAutosaveMaps(const value: boolean);
      procedure saveCurrent;
      procedure saveAll;
      function addNewMap(parentID: integer): IMapMetadata;
      procedure editMapProperties(mapID: integer);
      procedure DeleteMap(mapID: integer; deleteMode: TDeleteMapMode);
      procedure Reset;

      procedure Pause;
      procedure Stop;

      procedure IDesignMapEngine.initialize = initializeDesigner;
      function IDesignMapEngine.loadMap = DesignLoadMap;
   protected
      procedure cleanup; override;
      procedure AfterPaint; override;
   public
      function IsDesign: boolean; override;
      procedure KeyDown(key: word; Shift: TShiftState); override;
   end;

implementation
uses
   sysUtils, commons, math, windows,
   turbu_map_metadata, archiveInterface, turbu_constants, turbu_sdl_image,
   turbu_functional, turbu_plugin_interface, turbu_containers, turbu_map_objects,
   eval, MapObject_Editor,
   sdl, sg_utils, sdlstreams;

const
   MAX_WIDTH: byte = 6; //assumed to be 6, but we can't be sure beforetime

{ T2kMapEngineD }

function T2kMapEngineD.IsDesign: boolean;
begin
   result := true;
end;

procedure T2kMapEngineD.KeyDown(key: word; Shift: TShiftState);
begin
   if FTimer.Enabled then
   begin
      inherited KeyDown(key, shift);
      Exit;
   end;
   if FCurrentLayer >= 0 then
      Exit;
   case key of
      VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT: ArrowKey(key);
      VK_RETURN: DoubleClick;
      VK_BACK, VK_DELETE: DoDelete;
   end;
end;

procedure T2kMapEngineD.ArrowKey(key: word);
var
   position: TSgPoint;
begin
   position := FCursorPosition;
   case key of
      VK_UP: dec(position.y);
      VK_DOWN: inc(position.y);
      VK_LEFT: dec(position.x);
      VK_RIGHT: inc(position.x);
   end;
   position.x := safeMod(position.x, FCurrentMap.MapObj.Size.x);
   position.y := safeMod(position.y, FCurrentMap.MapObj.Size.y);
   draw(position, true);
end;

procedure T2kMapEngineD.initializeDesigner(window: TSdlWindowId; database: IRpgDatabase);
begin
   self.initialize(window, database);
   FObjectContainers.Free;
   FObjectContainers := TMapObjectContainerList.Create;
   //do more
end;

procedure T2kMapEngineD.AfterPaint;
begin
  if (FCurrentLayer < 0) and (not FTimer.Enabled) then
  begin
     DrawGrid;
     DrawCursor;
  end;
end;

procedure T2kMapEngineD.BreakSomething;
begin
   raise Exception.Create('Error Message');
end;

procedure T2kMapEngineD.cleanup;
begin
   //FObjectContainers needs to be cleaned up before the sprite engine is
   //destroyed in the call to inherited
   self.ClearContainers;
   inherited cleanup;
   FreeAndNil(FObjectContainers);
   FreeAndNil(FPaletteList);
   FreeAndNil(FTilesetListD);
end;

procedure T2kMapEngineD.ClearContainers;
begin
   if assigned(FObjectContainers) then
      FObjectContainers.Clear;
end;

procedure T2kMapEngineD.Reset;
begin
   self.cleanup;
   self.FDatabase := nil;
   self.FCanvas := nil;
   self.FCurrentMap := nil;
   self.FWaitingMap := nil;
   self.FImages := nil;
   self.FDefaultBattleEngine := nil;
   self.FInitialized := false;
end;

procedure T2kMapEngineD.rightClick(const position: TSgPoint);
begin
   if not FTimer.Enabled then
      Exit;

   if FCurrentMap.passable(position.x, position.y) then
   begin
{      if currentParty = nil then
         initializeHero
      else currentParty.leaveTile;
      FCurrentMap.currentParty.location := whichtile; }
   end;
end;

function T2kMapEngineD.DesignLoadMap(map: IMapMetadata): IRpgMap;
var
   viewport: TRect;
begin
   if assigned(FCurrentMap) then
   begin
      self.ClearContainers;
      if not FCurrentMap.mapObj.modified then
         freeAndNil(FMaps[FCurrentMap.mapObj.id])
      else if FAutosaveMaps then
      begin
         saveMap(FCurrentMap.mapObj);
         freeAndNil(FMaps[FCurrentMap.mapObj.id])
      end;
   end;

   prepareMap(map);
   viewport := createViewport(FWaitingMap, FScrollPosition);
   if not assigned(FMaps[FWaitingMap.id]) then
   begin
      FTilesetListD.Free;
      FTilesetListD := loadTilesetD(FDatabase.tileset[FWaitingMap.tileset]);
      FMaps[FWaitingMap.id] := T2kSpriteEngine.Create(FWaitingMap, viewport,
                                FCanvas, FDatabase.tileset[FWaitingMap.tileset],
                                FImages);
   end;
   FCurrentLayer := 0;
   if doneLoadingMap then
      result := FCurrentMap.mapObj
   else result := nil;
end;

procedure T2kMapEngineD.DoDelete;
var
   sprite: TMapSprite;
   index: integer;
begin
   sprite := GetCurrentMapObject;
   if not assigned(sprite) then
      Exit;
   index := FObjectContainers.firstIndexWhere(
      function(value: TMapObjectContainer): boolean
      begin
         result := value.base = sprite;
      end);
   assert(index <> -1);
   FObjectContainers.Delete(index);
   FCurrentMap.MapObjects.Remove(sprite);
   FHookedObject := nil;
   FCurrentMap.Dead;
   self.repaint;
end;

procedure T2kMapEngineD.saveAll;
begin
   saveAndClearMapCache;
   saveCurrent;
end;

procedure T2kMapEngineD.saveAndClearMapCache;
var
   i: integer;
   map: T2kSpriteEngine;
begin
   for i := low(FMaps) to high(FMaps) do
   begin
      map := FMaps[i];
      if assigned(map) and (map <> FCurrentMap) then
      begin
         saveMap(map.mapObj);
         FreeAndNil(FMaps[i]);
      end;
   end;
end;

procedure T2kMapEngineD.saveCurrent;
begin
   saveMap(FCurrentMap.mapObj);
end;

procedure T2kMapEngineD.saveMap(value: TRpgMap);
var
   metadata: TMapMetadata;
   saveStream: TMemoryStream;
begin
   if not value.modified then
      Exit;

   saveStream := TMemoryStream.Create;
   try
      value.save(saveStream);
      metadata := FDatabase.mapTree.item[value.id];
      assert(metadata.name = value.Name);
      GArchives[MAP_ARCHIVE].writeFile(metadata.internalFilename.name, saveStream);
      value.modified := false;
   finally
      saveStream.Free;
   end;
end;

procedure T2kMapEngineD.setAutosaveMaps(const value: boolean);
begin
   FAutosaveMaps := value;
   if value then
      self.saveAndClearMapCache;
end;

procedure T2kMapEngineD.SetCurrentLayer(const value: shortint);
var
   enumerator: TTileGroupRecord;
   image: TRpgSdlImage;
   texture: TSdlTexture;
begin
   if (value < 0) and (FCurrentLayer >= 0) then
      PrepareContainers
   else if (value >= 0) and (FCurrentLayer < 0) then
      ClearContainers;
   FCurrentLayer := value;
   for enumerator in FCurrentMap.tileset.Records do
   begin
      image := retrieveImage('tileset', enumerator.group.filename);
      assert(assigned(image));
      texture := image.Texture;
      if (value < 0) or (value in enumerator.layers) then
         texture.alpha := $FF
      else texture.alpha := $A0;
   end;
   self.repaint;
end;

procedure T2kMapEngineD.setPaletteList(value: TList<integer>);
begin
   assert((value[0] = 1) or (value.Count mod value[0] = 1));
   FPaletteList.Free;
   FPaletteList := value;
end;

procedure T2kMapEngineD.Stop;
begin
   Pause;
   Assert(false, 'not implemented yet');
end;

procedure T2kMapEngineD.draw(const position: TSgPoint; new: boolean);
var
   tile: TTileRef;
   drawRect: TRect;
   i, j, counter: integer;
   offset: TSgPoint;
begin
   if FCurrentLayer >= 0 then
   begin
      drawRect.TopLeft := position;
      drawRect.Right := drawRect.Left + FPaletteList[0] - 1;
      drawRect.Bottom := drawRect.Top + ((FPaletteList.Count - 1) div FPaletteList[0]) - 1;

      //calculate where to start drawing
      if new then
      begin
         offset := ORIGIN;
         FDrawFrom := position;
      end
      else
         offset := position - FDrawFrom;
      counter := (offset.y * FPaletteList[0]) + offset.x;
      counter := safeMod(counter, FPaletteList.Count - 1) + 1;

      for j := drawRect.Top to drawRect.Bottom do
         for I := drawRect.Left to drawRect.Right do
         begin
            tile := FCurrentMap.tileset.Tile(FPaletteList[counter], FCurrentLayer);
            FCurrentMap.assignTile(i, j, FCurrentLayer, tile);
            inc(counter);
            if counter = FPaletteList.Count then
               counter := 1;
         end;
      drawRect := sg_utils.expandRect(drawRect, 1);
      for j := drawRect.Top to drawRect.Bottom do
         for I := drawRect.Left to drawRect.Right do
            FCurrentMap.updateBorders(i, j, FCurrentLayer);
      FCurrentMap.Dead;
   end
   else begin
      FCursorPosition := position;
      if not assigned(FHookedObject) then
         FHookedObject := self.GetCurrentMapObject;
   end;
   self.repaint;
end;

procedure T2kMapEngineD.DrawCursor;
var
   drawRect: TRect;
begin
   drawRect.TopLeft := FCursorPosition;
   drawRect.Right := drawRect.Left + 1;
   drawRect.Bottom := drawRect.Top + 1;
   drawRect := TRectToSdlRect(constrictRect(multiplyRect(drawRect, TILE_SIZE.x), 1));
   dec(drawrect.Left, trunc(FCurrentMap.WorldX));
   dec(drawrect.Top, trunc(FCurrentMap.WorldY));
   FCanvas.DrawBox(drawRect, SDL_WHITE);
end;

procedure T2kMapEngineD.DrawGrid;
var
   x, y: integer;
begin
   x := -(trunc(FCurrentMap.WorldX) mod TILE_SIZE.x);
   y := -(trunc(FCurrentMap.WorldY) mod TILE_SIZE.y);
   SDL_SetRenderDrawColor(SDL_BLACK);
   while x < FCanvas.Width do
   begin
      SDL_RenderDrawLine(x, 0, x, FCanvas.Height);
      inc(x, TILE_SIZE.x);
   end;
   while y < FCanvas.Height do
   begin
      SDL_RenderDrawLine(0, y, FCanvas.Width, y);
      inc(y, TILE_SIZE.y);
   end;
end;

function T2kMapEngineD.addNewMap(parentID: integer): IMapMetadata;
var
   meta: TMapMetadata;
   newmap: TRpgMap;
begin
   meta := FDatabase.mapTree.AddNewMetadata(parentID);
   newmap := TRpgMap.Create(meta);
   if eval.TfrmMapProperties.EditMap(meta, newMap) then
   begin
      result := meta;
      saveMap(newMap);
      designLoadMap(result);
   end
   else begin
      result := nil;
      FDatabase.mapTree.Remove(meta);
      newmap.Free;
   end;
end;

procedure T2kMapEngineD.editMapProperties(mapID: integer);
var
   map: T2kSpriteEngine;
   oldsize: TSgPoint;
begin
   map := FMaps[mapID];
   assert(assigned(map));
   oldsize := map.mapObj.size;
   if eval.TfrmMapProperties.EditMap(FDatabase.mapTree[mapID], map.mapObj) then
   begin
      if oldsize <> map.mapObj.size then
         map.RecreateTileMatrix;
      self.repaint;
   end;
end;

procedure T2kMapEngineD.DeleteMap(mapID: integer; deleteMode: TDeleteMapMode);

   procedure InternalDeleteMap(mapID: integer; deleteMode: TDeleteMapMode);
   var
      map, child: TMapMetadata;
      filename: string;
      children: TList<TMapMetadata>;
   begin
      map := FDatabase.mapTree[mapID];
      if not assigned(map) then
         raise ERpgPlugin.Create('Invalid map ID.');
      filename := map.internalFilename.name;
      GArchives[MAP_ARCHIVE].deleteFile(filename);
      mapID := map.ID;
      children := FDatabase.mapTree.ChildrenOf(mapID);
      try
         for child in children do
            case deleteMode of
               dmTree: deleteMap(child.id, dmTree);
               dmSibling: child.parent := map.parent;
               dmTop: child.parent := 0;
               dmNone: assert(false);
            end;

         FDatabase.mapTree.remove(map);
      finally
         children.Free;
      end;
   end;

begin
   internalDeleteMap(mapID, deleteMode);
//   FDatabase.mapTree.
end;

function T2kMapEngineD.getAutosaveMaps: boolean;
begin
   result := FAutosaveMaps;
end;

procedure T2kMapEngineD.doneDrawing;
begin
   FCurrentMap.Dead;
   FCurrentMap.mapObj.modified := true;
   if assigned(FHookedObject) then
   begin
      FObjectContainers.Delete(FObjectContainers.FirstIndexWhere(
         function(arg1: TMapObjectContainer): boolean
         begin
            result := (arg1.base = FHookedObject);
         end));
      FHookedObject.location := FCursorPosition;
      FObjectContainers.Add(TMapObjectContainer.Create(FHookedObject, FCanvas));
      FHookedObject := nil;
      self.repaint;
   end;
end;

procedure T2kMapEngineD.doubleClick;
var
   sprite: TMapSprite;
   obj: TRpgMapObject;
begin
   if FCurrentLayer >= 0 then
      Exit;

   sprite := self.GetCurrentMapObject;
   FHookedObject := nil;
   if assigned(sprite) then
   begin
      TfrmObjectEditor.EditMapObject(sprite.event, FCurrentMap.MapObj, FCurrentMap.tileset.name);
      sprite.updatePage(sprite.event.currentPage);
      self.repaint;
   end
   else begin
      obj := TFrmObjectEditor.NewMapObject(self.NewMapObjectID(FCurrentMap), FCurrentMap.tileset.name);
      if assigned(obj) then
      begin
         sprite := FCurrentMap.AddMapObject(obj);
         FCurrentMap.mapObj.mapObjects.Add(obj);
         sprite.location := FCursorPosition;
         FObjectContainers.Add(TMapObjectContainer.Create(sprite, FCanvas));
         self.repaint;
      end;
   end;
end;

function T2kMapEngineD.GetTilesetImageSize(const index: byte): TSgPoint;
var
   tileCount: integer;
   localCount: integer;
   pair: TTileGroupPair;
   kind: TTileType;
begin

   //first calculate size
   //TODO: we shouldn't have to do this at load time.  It should be calculated
   //already and saved with the tileset
   FTilesize := FTilesetListD.last.Key.group.dimensions;
   MAX_WIDTH := FTilesetListD.Last.Value.Width div FTilesize.x;
   tileCount := 0;
   for pair in FTilesetListD do
   begin
      if not (index in pair.Key.layers) then
         Continue;

      kind := pair.Key.group.tileType;
      if tsBordered in kind then
         localCount := 1
      else
      begin
         localcount := pair.Value.Width div FTilesize.x;
         if kind = [] then
            localcount := (pair.Value.Height div FTilesize.y) * localcount;
      end;
      inc(tilecount, localcount);
   end;

{   tileCount := TFunctional.reduce2<TTileGroupPair, integer>(FTilesetListD,
      function(const input1: TTileGroupPair; input2: integer): integer
      var
         kind: TTileType;
      begin
         if not (index in input1.Key.layers) then
            Exit(input2);

         kind := input1.Key.group.tileType;
         if tsBordered in kind then
            result := 1
         else
         begin
            result := input1.Value.Width div FTilesize.x;
            if kind = [] then
               result := (input1.Value.Height div FTilesize.y) * result;
         end;
         result := result + input2;
      end);}

   result := FTilesize * sgPoint(MAX_WIDTH, Ceil(tileCount / MAX_WIDTH));
end;

function T2kMapEngineD.GetCurrentLayer: shortint;
begin
   result := FCurrentLayer;
end;

function T2kMapEngineD.GetCurrentMapObject: TMapSprite;
begin
   result := FCurrentMap.mapObjects.firstWhere(
   function(obj: TMapSprite): boolean
   begin
      result := obj.location = FCursorPosition;
   end);
end;

function T2kMapEngineD.GetTilesetImage(const index: byte): PSdlSurface;
var
   pair: TTileGroupPair;
   surfaceSize: TSgPoint;
   srcRect, dstRect: TRect;
   kind: TTileType;
begin
   assert(assigned(FTilesetListD));
   surfaceSize := GetTilesetImageSize(index);

   result := TSdlSurface.Create(surfaceSize.x, surfaceSize.y, 8);
   result.Fill(nil, 0);
   result.CopyPaletteFrom(FTilesetListD.Last.Value);
   dstRect := rect(point(0, 0), FTileSize);
   for pair in FTilesetListD do
   begin
      if not (index in pair.Key.layers) then
         Continue;

      kind := pair.key.group.tileType;
      if tsBordered in kind then
         srcRect := rect(point(0,0), FTileSize)
      else if kind = [tsAnimated] then
         srcRect := rect(0, 0, pair.Value.Width, FTileSize.y)
      else srcRect := rect(0, 0, pair.value.width, pair.value.height);
      if srcRect.Right + dstRect.Left > result.Width then
      begin
         dstRect.Left := 0;
         inc(dstRect.Top, FTileSize.y);
      end;
      dstRect.BottomRight := srcRect.BottomRight;
      result.BlitFrom(pair.Value, @srcRect, @dstRect);
      inc(dstRect.Left, srcRect.Right);
      inc(dstRect.Top, srcRect.bottom - FTileSize.y);
   end;
end;

// *grumble* Stupid URWs...
function T2kMapEngineD.loadTilesetD(const value: TTileSet): TList<TTileGroupPair>;
var input: TTileGroupRecord;
newItem: TTileGroupPair;
var
filename: string;
rw: PSDL_RWops;
begin
   result := TList<TTileGroupPair>.Create;
   for input in value.Records do

{   result := value.Records.mapFP<TTileGroupPair>(
      function(const input: TTileGroupRecord): TTileGroupPair
      var
         filename: string;
         rw: PSDL_RWops;}
      begin
         filename := 'tileset\' + input.group.filename + '.png';
         if not FImages.contains(filename) then
         begin
            rw := sdlstreams.SDLStreamSetup(GArchives[IMAGE_ARCHIVE].getFile(filename));
            {result} newItem := TTileGroupPair.Create(input,
              TRpgSdlImage.CreateSprite(rw, '.png', input.group.filename, FImages).surface);
            TStream(rw.unknown).Free;
            SDLStreamCloseRWops(rw);
         end
         else
         begin
            {result} newItem := TTileGroupPair.Create(input,
              (FImages.Image[filename] as TRpgSdlImage).surface);
         end;
{      end);}
         result.Add(newItem);
      end;
end;

function T2kMapEngineD.mapPosition: TSgPoint;
begin
   result := sgPoint(trunc(FCurrentMap.WorldX), trunc(FCurrentMap.WorldY));
end;

function T2kMapEngineD.mapSize: TSgPoint;
begin
   result := TSgPoint(FCurrentMap.mapRect.BottomRight) * TILE_SIZE;
end;

function T2kMapEngineD.NewMapObjectID(engine: T2kSpriteEngine): integer;
begin
   result := engine.mapObjects.reduce<integer>(
      function(const input1: TMapSprite; input2: integer): integer
      begin
         result := max(engine.mapObjects[0].event.id, input2);
      end) + 1;
end;

procedure T2kMapEngineD.Pause;
begin
   FTimer.Enabled := false;
end;

procedure T2kMapEngineD.PrepareContainers;
var
   obj: TMapSprite;
begin
   for obj in FCurrentMap.mapobjects do
      FObjectContainers.Add(TMapObjectContainer.Create(obj, FCanvas));
end;

procedure T2kMapEngineD.scrollMap(const newPosition: TSgPoint);
var
   reducedPosition: TSgPoint;
begin
   reducedPosition := newPosition / TILE_SIZE;
   FCurrentMap.viewport := rect(reducedPosition, FCurrentMap.viewport.BottomRight);
   FCurrentMap.WorldX := newPosition.x;
   FCurrentMap.WorldY := newPosition.y;
   self.repaint;
end;

end.
