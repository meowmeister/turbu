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
   AsphyreTimer,
   SG_defs, SDL_ImageManager, sdl_canvas, sdl_13;

type
   TTileGroupPair = TPair<TTileGroupRecord, PSdlSurface>;

   T2kMapEngine = class(TMapEngine)
   private
      FStretchRatio: TSgFloatPoint;
      FSignal: TSimpleEvent;
   protected
      FDatabase: TRpgDatabase;
      FCanvas: TSdlCanvas;
      FCurrentMap: T2kSpriteEngine;
      FWaitingMap: TRpgMap;
      FImages: TSdlImages;
      FMaps: array of T2kSpriteEngine;
      FScrollPosition: TSgPoint;
      FTimer: TAsphyreTimer;

      function retrieveImage(const folder, filename: string): TRpgSdlImage;

      procedure repaint;
      procedure loadTileset(const value: TTileSet);
      procedure loadSprite(const filename: string);
      function CreateViewport(map: TRpgMap; center: TSgPoint): TRect;
      procedure LoadMapSprites(map: TRpgMap);
      function doneLoadingMap: boolean;
      procedure prepareMap(const data: IMapMetadata);
   private //timing and animation
      FFrame: integer;
      FHeartbeat: integer;
      FLatency: integer;
      procedure OnTimer(Sender: TObject);
   protected
      procedure cleanup; override;
   public
      constructor Create; override;
      destructor Destroy; override;
      procedure initialize(window: TSdlWindowId; database: IRpgDatabase); override;
      function loadMap(map: IMapMetadata): IRpgMap; override;
      procedure Play; override;
   end;

implementation
uses
   sysUtils, classes, math,
   archiveInterface, commons, turbu_plugin_interface, turbu_game_data,
   turbu_constants, turbu_map_metadata, turbu_functional, turbu_classes,
   turbu_map_objects,
   SDL, sdlstreams, sdl_sprite, sg_utils;

{ Callbacks }

function ALoader(filename: string): PSDL_RWops;
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
   FCanvas.Free;
   FImages.Free;
   FSignal.Free;
   for I := 0 to high(FMaps) do
      FMaps[i].Free;
   setLength(FMaps, 0);
   inherited Cleanup;
end;

constructor T2kMapEngine.Create;
begin
   inherited Create;
   self.data := TMapEngineData.Create('TURBU basic map engine', TVersion.Create(0, 1, 0));
   FTimer := TAsphyreTimer.Create;
   FTimer.MaxFPS := 60;
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

destructor T2kMapEngine.Destroy;
begin
   FTimer.Free;
   inherited;
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

      //TODO: add sdlwResizable if Sam's able to add in a "logical size" concept, or
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

function T2kMapEngine.loadMap(map: IMapMetadata): IRpgMap;
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
   if doneLoadingMap then
      result := FCurrentMap.mapObj
   else result := nil;
end;

procedure T2kMapEngine.LoadMapSprites(map: TRpgMap);
var
   mapObj: TRpgMapObject;
   page: TRpgEventPage;
begin
   for mapObj in map.mapObjects do
      for page in mapObj.pages do
         if not page.isTile then
            loadSprite(page.name);
end;

procedure T2kMapEngine.prepareMap(const data: IMapMetadata);
var
   mapStream: TStream;
   map: TMapMetadata;
begin
   FTimer.Enabled := false;
   if not (FInitialized) then
      raise ERpgPlugin.Create('Can''t load a map on an uninitialized map engine.');
   map := TMapMetadata(data);
   if not assigned(map) then
      raise ERpgPlugin.Create('Incompatible metadata object.');

   if not assigned(FMaps[map.id]) then
   begin
      mapStream := GArchives[MAP_ARCHIVE].getFile(map.internalFilename.name);
      try
         FWaitingMap := TRpgMap.Load(mapStream);
      finally
         mapStream.Free;
      end;
   end
   else FWaitingMap := FMaps[map.id].mapObj;
   FScrollPosition := map.scrollPosition;
end;

procedure T2kMapEngine.repaint;
begin
   FCanvas.SetRenderer;
   SDL_SetRenderDrawColor(0, 0, 0, 255);
   FCanvas.Clear;
   FCurrentMap.Draw;
   FCanvas.Flip;
end;

function T2kMapEngine.retrieveImage(const folder, filename: string): TRpgSdlImage;
begin
   if not FImages.contains(filename) then
      FImages.AddFromFile(format('%s\%s.png', [folder,filename]), filename);
   result := FImages.Image[filename] as TRpgSdlImage;
end;

function T2kMapEngine.doneLoadingMap: boolean;
begin
   FCurrentMap := FMaps[FWaitingMap.id];
   LoadMapSprites(FCurrentMap.mapObj);
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
            FImages.AddSpriteFromArchive(filename, input.group.filename, input.group.dimensions);
      end);
end;

procedure T2kMapEngine.loadSprite(const filename: string);
var
   lName: string;
begin
   lname := 'mapsprite\' + filename + '.png';
   if not FImages.contains(filename) then
      FImages.AddSpriteFromArchive(lname, filename, SPRITE_SIZE);
end;

{$R-}
procedure T2kMapEngine.OnTimer(Sender: TObject);
begin
   //TODO: Remove commented-out code blocks when render targets, transitions and script events are implemented
   inc(FFrame);
   FLatency := max(commons.round(FTimer.latency), 1);
{   if assigned(mapEngine.transProc) then
      mapEngine.Draw
   else if mapEngine.blank then
      //do nothing
   else begin
      GRenderTargets.RenderOn(2, standardRender, 0, true);
   end; }
   FCurrentMap.Draw;

//   dummy := device.Render(0, true);
   FTimer.Process;
//   if dummy then
      FCanvas.Flip;
   if FFrame > FHeartbeat then
   begin
      FCurrentMap.advanceFrame;
      FFrame := 0;
   end;
{   GScriptEngine.eventTick;
   mapEngine.scriptEngine.timer.tick;}
end;
{$R+}

procedure T2kMapEngine.Play;
begin
   assert(assigned(FCurrentMap));
   FTimer.Enabled := true;
   FTimer.OnTimer := self.OnTimer;
   FTimer.OnProcess := FCurrentMap.Process;
end;

end.
