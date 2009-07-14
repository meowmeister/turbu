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
   types, syncObjs,
   turbu_map_engine, turbu_versioning,
   turbu_database_interface, turbu_map_interface,
   turbu_database, turbu_maps, turbu_tilesets, turbu_2k_sprite_engine,
   SG_defs, SDL_ImageManager, sdl_canvas,
   sdl_13;

type
   T2kMapEngine = class(TMapEngine)
   private
      FDatabase: TRpgDatabase;
      FCanvas: TSdlCanvas;
      FStretchRatio: TSgFloatPoint;
      FCurrentMap: T2kSpriteEngine;
      FWaitingMap: TRpgMap;
      FImages: TSdlImages;
      FMaps: array of T2kSpriteEngine;
      FSignal: TSimpleEvent;
      procedure loadTileset(const value: TTileSet);
      function CreateViewport(map: TRpgMap; center: TSgPoint): TRect;
   protected
      procedure cleanup; override;
   public
      constructor Create; override;
      procedure initialize(window: TSdlWindowId; database: IRpgDatabase); override;
      function loadMap(map: IRpgMap; startPosition: TSgPoint): boolean; override;
   end;

implementation
uses
   sysUtils, classes,
   archiveInterface, commons, turbu_plugin_interface, turbu_game_data,
   turbu_constants, turbu_map_metadata, turbu_functional,
image_callback_hack,
   SDL, sdlstreams, sdl_sprite;

(*
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
end;
*)

{ T2kMapEngine }

const TILE_SIZE = 16;

procedure T2kMapEngine.cleanup;
var i: integer;
begin
   assert(FInitialized);
   FCanvas.Free;
   FImages.Free;
   FSignal.Free;
   for I := 0 to high(FMaps) do
      FMaps[i].Free;
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
   center := center - (screensize / 2);
   if not (wrHorizontal in map.wraparound) then
   begin
      if center.x < 0 then
         center.x := 0
      else if center.x + screensize.x >= map.size.X then
         center.x := pred(map.size.x - screensize.x);
   end;
   if not (wrVertical in map.wraparound) then
   begin
      if center.Y < 0 then
         center.Y := 0
      else if center.Y + screensize.Y >= map.size.Y then
         center.Y := pred(map.size.Y - screensize.Y);
   end;
   result.TopLeft := center;
   result.BottomRight := screensize;
end;

procedure T2kMapEngine.initialize(window: TSdlWindowId; database: IRpgDatabase);
var
   db: I2kDatabase;
   layout: TGameLayout;
begin
   inherited initialize(window, database);
   if not supports(database, I2kDatabase, db) then
      raise ERpgPlugin.Create('Incompatible project database');
   FDatabase := db.dbObject;
   layout := FDatabase.layout;
   if window = 0 then
   begin
      window :=  SDL_CreateWindow(PAnsiChar(ansiString(format('TURBU engine - %s', [FDatabase.projectname]))),
                        SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED,
                        layout.physWidth, layout.physHeight,
                        [sdlwOpenGl, sdlwShown {,sdlwResizable, sdlwInputGrabbed}]);
      //In RM2K, project title is stored in the LMT.  I'll need to convert that
      //to grab a project title. This will do for now.

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
   image_callback_hack.assignCallbacks(FImages);
   setLength(FMaps, FDatabase.mapTree.Count);
   FSignal := TSimpleEvent.Create;
   FSignal.SetEvent;

   FInitialized := true;
end;

function T2kMapEngine.loadMap(map: IRpgMap; startPosition: TSgPoint): boolean;
var
   lMap: I2kMap;
   viewport: TRect;
begin
   if not (FInitialized) then
      raise ERpgPlugin.Create('Can''t load a map on an uninitialized map engine.');
   if not supports(map, I2kMap, lMap) then
      raise ERpgPlugin.Create('Incompatible map object');
   FWaitingMap := lMap.mapObject;
   viewport := createViewport(FWaitingMap, startPosition);
   if not assigned(FMaps[FWaitingMap.id]) then
   begin
      loadTileset(FDatabase.tileset[FWaitingMap.tileset]);
      FMaps[FWaitingMap.id] := T2kSpriteEngine.Create(FWaitingMap, viewport,
                               FCanvas, FDatabase.tileset[FWaitingMap.tileset],
                               FImages);
   end;
   FCurrentMap := FMaps[FWaitingMap.id];
   result := FSignal.WaitFor(INFINITE) = wrSignaled;
   if result then
   begin
      FSignal.ResetEvent;
      FCurrentMap.Draw;
      FCanvas.Flip;
   end;
end;

procedure T2kMapEngine.loadTileset(const value: TTileSet);
begin
   TFunctional.map<TTileGroupRecord>(value.Records,
      procedure(const input: TTileGroupRecord)
      var filename: string;
      begin
         filename := 'tileset\' + input.group.filename + '.png';
         if not FImages.contains(filename) then
            FImages.AddSpriteFromArchive(filename, '', input.group.filename, input.group.dimensions);
      end);
end;

end.
