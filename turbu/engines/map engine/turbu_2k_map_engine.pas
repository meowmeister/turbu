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
   turbu_map_engine, turbu_versioning,
   turbu_database_interface, turbu_map_interface,
   turbu_database, turbu_maps, turbu_tilesets,
   SG_defs,
   sdl_13;

type
   T2kMapEngine = class(TMapEngine)
   private
      FDatabase: TRpgDatabase;
      FWindowHandle: TSdlWindowId;
      FStretchRatio: TSgFloatPoint;
      FCurrentMap: TRpgMap;
      FWaitingMap: TRpgMap;
      procedure loadTileset(const value: TTileSet);
      procedure loadTileGroup(const value: TTileGroupRecord);
   protected
      procedure cleanup; override;
   public
      constructor Create; override;
      procedure initialize(window: TSdlWindowId; database: IRpgDatabase); override;
      function loadMap(map: IRpgMap): boolean; override;
   end;

implementation
uses
   sysUtils, classes,
   archiveInterface, commons, turbu_plugin_interface, turbu_game_data,
   turbu_constants, turbu_map_metadata,
   SDL, SDL_ImageManager, sdlstreams;

{ T2kMapEngine }

procedure T2kMapEngine.cleanup;
begin
  assert(FInitialized);
   //do something eventually
end;

constructor T2kMapEngine.Create;
begin
  inherited;
  self.data := TMapEngineData.Create('TURBU basic map engine', TVersion.Create(0, 1, 0));
end;

procedure T2kMapEngine.initialize(window: TSdlWindowId; database: IRpgDatabase);
var
   db: I2kDatabase;
   layout: TGameLayout;
   mapTree: TMapTree;
   currentMap: TMapMetadata;
begin
   inherited initialize(window, database);
   if not supports(database, I2kDatabase, db) then
      raise ERpgPlugin.Create('Incompatible project database');
   FDatabase := db.dbObject;
   layout := FDatabase.layout;
   if window = 0 then
   begin
      FWindowHandle :=  SDL_CreateWindow(PAnsiChar(ansiString(format('TURBU engine - %s', [FDatabase.attributes[0].name]))),
                        SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED,
                        layout.physWidth, layout.physHeight,
                        [sdlwOpenGl, sdlwShown, {sdlwResizable,} sdlwInputGrabbed]);
      //In RM2K, project title is stored in the LMT.  I'll need to convert that
      //to grab a project title. This will do for now.

      //add sdlwResizable if Sam's able to add in a "logical size" concept, or
      //if I end up doing it on this end

      if FWindowHandle = 0 then
         raise ERpgPlugin.Create('Unable to initialize SDL window: ' + LFCR + string(SDL_GetError));
   end
   else FWindowHandle := window;
   FStretchRatio.x := layout.physWidth / layout.width;
   FStretchRatio.y := layout.physHeight / layout.height;

   FInitialized := true;
end;

function T2kMapEngine.loadMap(map: IRpgMap): boolean;
var
   lMap: I2kMap;
   tileset: TTileSet;
begin
   result := false;
   if not (FInitialized) then
      raise ERpgPlugin.Create('Can''t load a map on an uninitialized map engine.');
   if not supports(map, I2kMap, lMap) then
      raise ERpgPlugin.Create('Incompatible map object');
   FWaitingMap := lMap.mapObject;
   for tileset in FDatabase.tileset do
      if tileset.name = FWaitingMap.tileset then
      begin
         loadTileset(tileset);
         break;
      end;
(*
var
   fileName: string;
   i, j: smallint;
   tileIndex: integer;
   currentChipset: TChipSet;
   bgm: TRmMusic;
begin
try
try
   FImages := TSdlImages.Create;
   currentMap := TMapUnit.Create(theMap, ldbData, mapTree, mapID);
   FCanvas := TSdlCanvas.Create(cmHardware, false, rect(100, 100, 320, 240), 16);
   mapEngine := TGameMap.create(currentMap, ldbData, mapTree, FCanvas, FImages, rtpLocation);
   transitions.init;
   GRenderTargets := TSdlRenderTargets.Create;
//   GRenderTargets.AddRenderTargets(4, device.Width, device.Height, aqHigh, alMask);
   mapEngine.Images := FImages;
   currentMap.eventBlock.compiler := mapEngine.scriptEngine.compiler;
   GGlobalEvents.compiler := mapEngine.scriptEngine.compiler;
   GGlobalEvents.compileAll;
   if ldbData.getChipset(currentMap.terrain).hiSpeed then
      heartbeat := 6
   else heartbeat := 12;
   fileName := ldbData.getChipset(currentMap.terrain).filename;
   if filename <> '' then
      mapEngine.loadChipset(fileName, FImages);
//check for background image
   if (currentmap.usesPano) and (currentMap.panoName <> '') then
   begin
      filename := currentMap.panoName;
      findGraphic(filename, 'panorama');
      if filename = '' then
         raise EParseMessage.create('Panorama graphic file "' + fileName + '" not found!');
      mapEngine.loadBG(filename, currentMap.panoName);
      mapEngine.currentMap.setBG;
   end;
   for i := 0 to currentMap.eventCount - 1 do
   begin
      filename := currentMap.events[i].page[0].filename;
      if (currentMap.events[i].page[0].filename <> '') then
      begin
         findGraphic(filename, 'charset');
         if filename <> '' then
            mapEngine.loadCharset(currentMap.events[i].page[0].filename, filename);
      end;
   end;
   mapEngine.currentMap.placeEvents;
   for i := 1 to ldbData.heroes - 1 do
   begin
      filename := ldbData.hero[i].filename;
      if (ldbData.hero[i].filename <> '') and (FImages.Image[filename + intToStr(0)] = nil) then
      begin
         findGraphic(filename, 'charset');
         if filename <> '' then
            mapEngine.loadCharset(ldbData.hero[i].filename, filename);
      end;
   end;
   if GInitializedHero then
   begin
      if ldbData.SystemData.startingHeroes > 0 then
         mapEngine.character[0] := THeroSprite.create(mapEngine, GScriptEngine.hero[ldbData.SystemData.startingHero[1]], GParty)
      else mapEngine.character[0] := THeroSprite.create(mapEngine, nil, GParty);
      mapEngine.currentParty := mapEngine.character[0] as TCharSprite;
      for i := 1 to ldbData.SystemData.startingHeroes do
         rs_system.heroJoin(ldbData.SystemData.startingHero[i]);
   end;
   bgm := mapTree[mapID].bgmData;
   if bgm.filename <> '' then
      mapEngine.scriptEngine.playMusic(bgm);
   tileIndex := 0; //initial value
   currentChipset := ldbData.getChipset(currentMap.terrain);
   for j := 0 to currentMap.height - 1 do
   begin
      for i := 0 to currentMap.width - 1 do
      begin
         mapEngine[lower, i, j].place(i, j, lower, currentMap.lowChip[tileIndex], currentChipset);
         if currentMap.highChip[tileIndex] <> 10000 then
            TLowerTile(mapEngine[lower, i, j]).placeUpper(i, j, upper, currentMap.highChip[tileIndex], currentChipset);
         inc(tileIndex);
      end;
   end;
   mapEngine.currentMap.scanSquare;
{   timer.OnProcess := mapEngine.process;
   timer.Enabled := true;}
   GInputReader.Resume;
except
   on E: EParseMessage do
   begin
      msgBox(E.message, 'TGameForm.FormShow says:', MB_OK);
      raise EMessageAbort.Create
   end
end; // end of TRY block
except
   on EMessageAbort do
   begin
      ShowMessage('Aborting due to fatal error.');
      application.Terminate
   end
end // end of second TRY block
end;
*)
end;

function ArchiveLoader(filename, keyname: string): PSDL_RWops;
var
   stream: TStream;
begin
   stream := GArchives[IMAGE_ARCHIVE].getFile(filename);
   result := sdlstreams.SDLStreamSetup(stream);
end;

procedure ArchiveCallback(var rw: PSdl_RWops);
begin
   (TObject(rw.unknown) as TStream).Free;
   SDL_FreeRW(rw);
end;

procedure T2kMapEngine.loadTileGroup(const value: TTileGroupRecord);
begin

end;

procedure T2kMapEngine.loadTileset(const value: TTileSet);
var
   enumerator: TTileGroupRecord;
begin
   for enumerator in value.Records do
      loadTileGroup(enumerator);
end;

end.
