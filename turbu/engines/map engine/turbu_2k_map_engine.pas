unit turbu_2k_map_engine;
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
   types, syncObjs, Generics.Collections,
   turbu_map_engine, turbu_versioning,
   turbu_database_interface, turbu_map_interface, turbu_sdl_image,
   turbu_database, turbu_maps, turbu_tilesets, turbu_2k_sprite_engine,
   SG_defs, SDL_ImageManager, sdl_canvas,
   sdl_13;

type
   TTileGroupPair = TPair<TTileGroupRecord, PSdlSurface>;

   T2kMapEngine = class(TMapEngine, IDesignMapEngine)
   private //design section
      FTilesetListD: TList<TTileGroupPair>;
      FTileSize: TSgPoint;
      FScrollPosition: TSgPoint;
      FDrawFrom: TSgPoint;
      FPaletteList: TList<integer>;
      FCurrentLayer: byte;
      FAutosaveMaps: boolean;
      procedure initializeDesigner(window: TSdlWindowId; database: IRpgDatabase);
      function GetTilesetImageSize(const index: byte): TSgPoint;
      function GetTilesetImage(const index: byte): PSdlSurface;
      function DesignLoadMap(var map: IRpgMap): boolean;
      function loadTilesetD(const value: TTileSet): TList<TTileGroupPair>;
      function mapSize: TSgPoint;
      function mapPosition: TSgPoint;
      procedure scrollMap(const newPosition: TSgPoint);
      procedure setPaletteList(value: TList<integer>);
      procedure draw(const position: TSgPoint; new: boolean);
      procedure doneDrawing;
      procedure SetCurrentLayer(const value: byte);
      function getAutosaveMaps: boolean;
      procedure setAutosaveMaps(const value: boolean);
      procedure saveMap(value: TRpgMap);
      procedure saveAndClearMapCache;
      procedure saveCurrent;
      procedure saveAll;

      procedure IDesignMapEngine.initialize = initializeDesigner;
      function IDesignMapEngine.loadMap = DesignLoadMap;
   private
      FDatabase: TRpgDatabase;
      FCanvas: TSdlCanvas;
      FStretchRatio: TSgFloatPoint;
      FCurrentMap: T2kSpriteEngine;
      FWaitingMap: TRpgMap;
      FImages: TSdlImages;
      FMaps: array of T2kSpriteEngine;
      FSignal: TSimpleEvent;

      function retrieveImage(const filename: string): TRpgSdlImage;

      procedure repaint;
      procedure loadTileset(const value: TTileSet);
      function CreateViewport(map: TRpgMap; center: TSgPoint): TRect;
      function doneLoadingMap: boolean;
      procedure prepareMap(map: IRpgMap);
   protected
      procedure cleanup; override;
   public
      constructor Create; override;
      procedure initialize(window: TSdlWindowId; database: IRpgDatabase); override;
      function loadMap(var map: IRpgMap): boolean; override;
   end;

implementation
uses
   sysUtils, classes, math,
   archiveInterface, commons, turbu_plugin_interface, turbu_game_data,
   turbu_constants, turbu_map_metadata, turbu_functional, turbu_classes,
   SDL, sdlstreams, sdl_sprite, sg_utils;

{ Callbacks }

function ALoader(filename, keyname: string): PSDL_RWops;
var
   stream: TStream;
begin
   stream := GArchives[IMAGE_ARCHIVE].getFile(filename);
   result := sdlstreams.SDLStreamSetup(stream);
end;

procedure ACallback(var rw: PSdl_RWops);
begin
   (TObject(rw.unknown) as TStream).Free;
   SDL_FreeRW(rw);
   rw := nil;
end;

{ T2kMapEngine }

const TILE_SIZE = 16;

procedure T2kMapEngine.cleanup;
var i: integer;
begin
   assert(FInitialized);
   FPaletteList.Free;
   FCanvas.Free;
   FImages.Free;
   FSignal.Free;
   for I := 0 to high(FMaps) do
      FMaps[i].Free;
   FTilesetListD.Free;
   inherited;
end;

constructor T2kMapEngine.Create;
begin
  inherited;
  self.data := TMapEngineData.Create('TURBU basic map engine', TVersion.Create(0, 1, 0));
end;

function T2kMapEngine.CreateViewport(map: TRpgMap; center: TSgPoint): TRect;
var
   screensize: TSgPoint;
begin
   screensize := FCanvas.size / TILE_SIZE;
   center := (center / TILE_SIZE) - (screensize / 2);
   if not (wrHorizontal in map.wraparound) then
   begin
      if center.x < 0 then
         center.x := 0
      else if center.x + screensize.x >= map.size.X then
         center.x := pred(map.size.x - (screensize.x div 2));
   end;
   if not (wrVertical in map.wraparound) then
   begin
      if center.Y < 0 then
         center.Y := 0
      else if center.Y + screensize.Y >= map.size.Y then
         center.Y := pred(map.size.Y - (screensize.Y div 2));
   end;
   result.TopLeft := center;
   result.BottomRight := screensize;
end;

procedure T2kMapEngine.initialize(window: TSdlWindowId; database: IRpgDatabase);
var
   layout: TGameLayout;
begin
   if FInitialized then
      Exit;

   inherited initialize(window, database);
   FDatabase := TRpgDatabase(database);
   if not assigned(FDatabase) then
      raise ERpgPlugin.Create('Incompatible project database');
   layout := FDatabase.layout;
   if window = 0 then
   begin
      //In RM2K, project title is stored in the LMT.  I'll need to convert that
      //to grab a project title. This will do for now.
      window :=  SDL_CreateWindow(PAnsiChar(ansiString(format('TURBU engine - %s', [FDatabase.mapTree[0].name]))),
                        SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED,
                        layout.physWidth, layout.physHeight,
                        [sdlwOpenGl, sdlwShown {,sdlwResizable, sdlwInputGrabbed}]);

      //add sdlwResizable if Sam's able to add in a "logical size" concept, or
      //if I end up doing it on this end

      if window = 0 then
         raise ERpgPlugin.CreateFmt('Unable to initialize SDL window: %s%s', [LFCR, SDL_GetError]);
      if SDL_CreateRenderer(window, -1, [sdlrPresentFlip2, sdlrAccelerated]) <> 0 then
         raise ERpgPlugin.CreateFmt('Unable to initialize SDL renderer: %s%s', [LFCR, SDL_GetError]);;
      SDL_RenderPresent;
   end;
   FCanvas := TSdlCanvas.CreateFrom(window);
   FStretchRatio.x := layout.physWidth / layout.width;
   FStretchRatio.y := layout.physHeight / layout.height;
   FImages := TSdlImages.Create();
   FImages.ArchiveCallback := aCallback;
   FImages.ArchiveLoader := aLoader;
   setLength(FMaps, FDatabase.mapTree.lookupCount);
   FSignal := TSimpleEvent.Create;
   FSignal.SetEvent;

   FInitialized := true;
end;

function T2kMapEngine.loadMap(var map: IRpgMap): boolean;
var
   viewport: TRect;
begin
   prepareMap(map);
   viewport := createViewport(FWaitingMap, FScrollPosition);
   if not assigned(FMaps[FWaitingMap.id]) then
   begin
      loadTileset(FDatabase.tileset[FWaitingMap.tileset]);
      FMaps[FWaitingMap.id] := T2kSpriteEngine.Create(FWaitingMap, viewport,
                               FCanvas, FDatabase.tileset[FWaitingMap.tileset],
                               FImages);
   end;
   result := doneLoadingMap;
end;

procedure T2kMapEngine.prepareMap(map: IRpgMap);
begin
   if not (FInitialized) then
      raise ERpgPlugin.Create('Can''t load a map on an uninitialized map engine.');
   FWaitingMap := TRpgMap(map);
   if not assigned(FWaitingMap) then
      raise ERpgPlugin.Create('Incompatible map object');
   FScrollPosition := FDatabase.mapTree.item[FWaitingMap.ID].scrollPosition;
end;

procedure T2kMapEngine.repaint;
begin
   FCanvas.SetRenderer;
   SDL_SetRenderDrawColor(0, 0, 0, 255);
   FCanvas.Clear;
   FCurrentMap.Draw;
   FCanvas.Flip;
end;

function T2kMapEngine.retrieveImage(const filename: string): TRpgSdlImage;
var
   fullName: string;
begin
   fullName := 'tileset\' + filename + '.png';
   if not FImages.contains(filename) then
      result := nil
   else
      result := FImages.Image[filename] as TRpgSdlImage;
end;

function T2kMapEngine.doneLoadingMap: boolean;
begin
   FCurrentMap := FMaps[FWaitingMap.id];
   result := FSignal.WaitFor(INFINITE) = wrSignaled;
   if result then
      self.repaint;
end;

procedure T2kMapEngine.loadTileset(const value: TTileSet);
begin
   value.Records.map(
      procedure(const input: TTileGroupRecord)
      var filename: string;
      begin
         filename := 'tileset\' + input.group.filename + '.png';
         if not FImages.contains(filename) then
            FImages.AddSpriteFromArchive(filename, '', input.group.filename, input.group.dimensions);
      end);
end;

//Design methods
const
   MAX_WIDTH: byte = 6; //assumed to be 6, but we can't be sure beforetime

function T2kMapEngine.GetTilesetImageSize(const index: byte): TSgPoint;
var
   tileCount: integer;
begin
   //first calculate size
   //TODO: we shouldn't have to do this at load time.  It should be calculated
   //already and saved with the tileset
   FTilesize := FTilesetListD.last.Key.group.dimensions;
   MAX_WIDTH := FTilesetListD.Last.Value.Width div FTilesize.x;
   tileCount := TFunctional.reduce2<TTileGroupPair, integer>(FTilesetListD,
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
      end);
   result := FTilesize * sgPoint(MAX_WIDTH, Ceil(tileCount / MAX_WIDTH));
end;

function T2kMapEngine.GetTilesetImage(const index: byte): PSdlSurface;
var
   pair: TTileGroupPair;
   surfaceSize: TSgPoint;
   srcRect, dstRect: TRect;
   kind: TTileType;
begin
   assert(assigned(FTilesetListD));
   surfaceSize := GetTilesetImageSize(index);
   FCurrentLayer := index;

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

function T2kMapEngine.loadTilesetD(const value: TTileSet): TList<TTileGroupPair>;
begin
   result := TFunctional.mapF<TTileGroupRecord, TTileGroupPair>(value.Records,
      function(const input: TTileGroupRecord): TTileGroupPair
      var
         filename: string;
         rw: PSDL_RWops;
      begin
         filename := 'tileset\' + input.group.filename + '.png';
         if not FImages.contains(filename) then
         begin
            rw := sdlstreams.SDLStreamSetup(GArchives[IMAGE_ARCHIVE].getFile(filename));
            result := TTileGroupPair.Create(input,
              TRpgSdlImage.CreateSprite(rw, '.png', input.group.filename, FImages,
              input.group.dimensions).surface);
            TStream(rw.unknown).Free;
            SDLStreamCloseRWops(rw);
         end
         else
         begin
            result := TTileGroupPair.Create(input,
              (FImages.Image[filename] as TRpgSdlImage).surface);
         end;
      end);
end;

function T2kMapEngine.mapPosition: TSgPoint;
begin
   result := sgPoint(trunc(FCurrentMap.WorldX), trunc(FCurrentMap.WorldY));
end;

function T2kMapEngine.mapSize: TSgPoint;
begin
   result := TSgPoint(FCurrentMap.mapRect.BottomRight) * TILE_SIZE;
end;

procedure T2kMapEngine.initializeDesigner(window: TSdlWindowId; database: IRpgDatabase);
begin
   self.initialize(window, database);
   //do more
end;

function T2kMapEngine.DesignLoadMap(var map: IRpgMap): boolean;
var
   viewport: TRect;
   temp: TObject;
begin
   if assigned(FCurrentMap) then
   begin
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
   end
   else begin
      FWaitingMap := FMaps[FWaitingMap.id].mapObj;
      //isn't disposing of interfaced objects without ref-counting fun?
      temp := TObject(map);
      map := FWaitingMap;
      temp.Free;
   end;
   result := doneLoadingMap;
end;

procedure T2kMapEngine.saveAll;
begin
   saveAndClearMapCache;
   saveCurrent;
end;

procedure T2kMapEngine.saveAndClearMapCache;
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

procedure T2kMapEngine.saveCurrent;
begin
   saveMap(FCurrentMap.mapObj);
end;

procedure T2kMapEngine.saveMap(value: TRpgMap);
var
   metadata: TMapMetadata;
   saveStream: TMemoryStream;
begin
   if not value.modified then
      Exit;

   saveStream := TMemoryStream.Create;
   try
      value.save(saveStream);
      metadata := GDatabase.mapTree.item[value.id];
      assert(metadata.name = value.Name);
      GArchives[MAP_ARCHIVE].writeFile(metadata.internalFilename.name, saveStream);
      value.modified := false;
   finally
      saveStream.Free;
   end;
end;

procedure T2kMapEngine.scrollMap(const newPosition: TSgPoint);
var
   reducedPosition: TSgPoint;
begin
   reducedPosition := newPosition / TILE_SIZE;
   FCurrentMap.viewport := rect(reducedPosition, FCurrentMap.viewport.BottomRight);
   FCurrentMap.WorldX := newPosition.x;
   FCurrentMap.WorldY := newPosition.y;
   self.repaint;
end;

procedure T2kMapEngine.setAutosaveMaps(const value: boolean);
begin
   FAutosaveMaps := value;
   if value then
      self.saveAndClearMapCache;
end;

procedure T2kMapEngine.SetCurrentLayer(const value: byte);
var
   enumerator: TTileGroupRecord;
   image: TRpgSdlImage;
   texture: TSdlTexture;
begin
   FCurrentLayer := value;
   for enumerator in FCurrentMap.tileset.Records do
   begin
      image := retrieveImage(enumerator.group.filename);
      if not assigned(image) then
         Continue;
      texture := image.Texture;
      if value in enumerator.layers then
         texture.alpha := $FF
      else texture.alpha := $A0;
   end;
   self.repaint;
end;

procedure T2kMapEngine.setPaletteList(value: TList<integer>);
begin
   assert((value[0] = 1) or (value.Count mod value[0] = 1));
   FPaletteList.Free;
   FPaletteList := value;
end;

procedure T2kMapEngine.draw(const position: TSgPoint; new: boolean);
var
   tile: TTileRef;
   drawRect: TRect;
   i, j, counter: integer;
   offset: TSgPoint;
begin
   drawRect.TopLeft := position;
   drawRect.Right := drawRect.Left + FPaletteList[0] - 1;
   drawRect.Bottom := drawRect.Top + ((FPaletteList.Count - 1) div FPaletteList[0]) - 1;

   //calculate where to start drawing
   if new then
   begin
      offset := sgPoint(0, 0);
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
   self.repaint;
end;

function T2kMapEngine.getAutosaveMaps: boolean;
begin
   result := FAutosaveMaps;
end;

procedure T2kMapEngine.doneDrawing;
begin
   FCurrentMap.Dead;
   FCurrentMap.mapObj.modified := true;
end;

end.
