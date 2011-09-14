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
   types, syncObjs, Generics.Collections, classes,
   turbu_map_engine, turbu_versioning, turbu_map_sprites, turbu_classes, turbu_heroes,
   turbu_database_interface, turbu_map_interface, turbu_sdl_image, turbu_2k_char_sprites,
   turbu_database, turbu_maps, turbu_tilesets, turbu_2k_sprite_engine, turbu_defs,
   turbu_2k_environment,
   AsphyreTimer, SG_defs, SDL_ImageManager, sdl_canvas, SDL, sdl_13;

type
   TTileGroupPair = TPair<TTileGroupRecord, PSdlSurface>;
   TTransProc = reference to procedure(var location: integer);

   T2kMapEngine = class(TMapEngine)
   private
      FStretchRatio: TSgFloatPoint;
      FSignal: TSimpleEvent;
      FButtonState: set of TButtonCode;
      FGameState: TGameState;
   protected
      FDatabase: TRpgDatabase;
      FCanvas: TSdlCanvas;
      FCurrentMap: T2kSpriteEngine;
      FWaitingMap: TRpgMap;
      FImages: TSdlImages;
      FMaps: array of T2kSpriteEngine;
      FScrollPosition: TSgPoint;
      FTimer: TAsphyreTimer;
      FPartySprite: THeroSprite;
      FParty: IRpgCharacter;
      FGameEnvironment: T2kEnvironment;

      function retrieveImage(const folder, filename: string): TRpgSdlImage;

      procedure repaint;
      procedure loadTileset(const value: TTileSet);
      procedure loadSprite(const filename: string);
      function CreateViewport(map: TRpgMap; center: TSgPoint): TRect;
      procedure LoadMapSprites(map: TRpgMap);
      function doneLoadingMap: boolean;
      procedure prepareMap(const data: IMapMetadata);
      procedure initializeParty;
   private //timing and animation
      FFrame: integer;
      FHeartbeat: integer;
      FEnterLock: boolean;
      FDontLockEnter: boolean;
      FCutscene: integer;
      FTransProc: TTransProc;
      FDatabaseOwner: boolean;
      procedure OnTimer(Sender: TObject);
      procedure OnProcess(Sender: TObject);
      procedure HandleEvent(event: TSdlEvent);
      procedure KeyDown(key: TSdlKeySym);
      procedure KeyUp(key: TSdlKeySym);
      procedure PressButton(button: TButtonCode);
      procedure PartyButton(button: TButtonCode);
   protected
      procedure cleanup; override;
      procedure AfterPaint; virtual;
   public
      constructor Create; override;
      destructor Destroy; override;
      procedure initialize(window: TSdlWindow; database: string); override;
      function loadMap(map: IMapMetadata): IRpgMap; override;
      procedure Play; override;
      function Playing: boolean; override;
      function MapTree: IMapTree; override;
      function database: IRpgDatabase; override;

      property PartySprite: THeroSprite read FPartySprite;
      property State: TGameState read FGameState;
      property TransProc: TTransProc write FTransProc;
   end;

implementation
uses
   sysUtils, math, Forms,
   archiveInterface, commons, turbu_plugin_interface, turbu_game_data,
   turbu_constants, turbu_map_metadata, turbu_functional, dm_database,
   turbu_map_objects, turbu_2k_map_locks, timing,
   sdlstreams, sdl_sprite, sg_utils;

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

procedure T2kMapEngine.AfterPaint;
begin
   //this virtual method intentionally left blank
end;

procedure T2kMapEngine.cleanup;
var i: integer;
begin
   assert(FInitialized);
   FGameEnvironment.Free;
   FParty := nil; //owned by FPartySprite
   FreeAndNil(FPartySprite);
   FreeAndNil(FCanvas);
   FreeAndNil(FImages);
   FreeAndNil(FSignal);
   for I := 0 to high(FMaps) do
      FMaps[i].Free;
   setLength(FMaps, 0);
   if FDatabaseOwner then
   begin
      FDatabase.Free;
      GDatabase := nil;
      FreeAndNil(dmDatabase);
   end;
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

function T2kMapEngine.database: IRpgDatabase;
begin
   result := FDatabase;
end;

destructor T2kMapEngine.Destroy;
begin
   FTimer.Free;
   inherited;
end;

procedure T2kMapEngine.initialize(window: TSdlWindow; database: string);
var
   layout: TGameLayout;
   renderer: TSdlRenderer;
begin
   if FInitialized then
      Exit;

   inherited initialize(window, database);
   if dmDatabase = nil then
   begin
      dmDatabase := TdmDatabase.Create(nil);
      dmDatabase.Connect(database, nil);
      FDatabase := TRpgDatabase.Load(dmDatabase);
      GDatabase := FDatabase;
      FDatabaseOwner := true;
      if not assigned(FDatabase) then
         raise ERpgPlugin.Create('Incompatible project database');
   end
   else FDatabase := GDatabase;

   layout := FDatabase.layout;
   if window.ptr = nil then
   begin
      //In RM2K, project title is stored in the LMT.  I'll need to convert that
      //to grab a project title. This will do for now.
      window :=  SDL_CreateWindow(PAnsiChar(ansiString(format('TURBU engine - %s', [FDatabase.mapTree[0].name]))),
                        SDL_WINDOWPOS_CENTERED_MASK, SDL_WINDOWPOS_CENTERED_MASK,
                        layout.physWidth, layout.physHeight,
                        [sdlwOpenGl, sdlwShown {,sdlwResizable, sdlwInputGrabbed}]);

      if window.ptr = nil then
         raise ERpgPlugin.CreateFmt('Unable to initialize SDL window: %s%s', [LFCR, SDL_GetError]);
      renderer := SDL_CreateRenderer(window, -1, [sdlrAccelerated]);
      if renderer.ptr = nil then
         raise ERpgPlugin.CreateFmt('Unable to initialize SDL renderer: %s%s', [LFCR, SDL_GetError]);
      SDL_RenderPresent(renderer);
   end
   else renderer := SDL_GetRenderer(window);
   FCanvas := TSdlCanvas.CreateFrom(window);
   FStretchRatio.x := layout.physWidth / layout.width;
   FStretchRatio.y := layout.physHeight / layout.height;
   FImages := TSdlImages.Create(renderer);
   FImages.ArchiveCallback := aCallback;
   FImages.ArchiveLoader := aLoader;
   setLength(FMaps, FDatabase.mapTree.lookupCount);
   FSignal := TSimpleEvent.Create;
   FSignal.SetEvent;
   FGameEnvironment := T2kEnvironment.Create(FDatabase);

   FInitialized := true;
end;

procedure T2kMapEngine.initializeParty;
var
   i: integer;
   party: TRpgParty;
begin
   if FParty = nil then
      FParty := TRpgParty.Create;
   party := FParty as TRpgParty;
   for I := 0 to FGameEnvironment.Heroes.Count - 1 do
   begin
      party.hero[1] := FGameEnvironment.heroes[i];
      if FGameEnvironment.heroes[i].sprite <> '' then
         Break;
   end;
   FPartySprite := THeroSprite.create(FCurrentMap, party.hero[1], party);
end;

procedure T2kMapEngine.KeyDown(key: TSdlKeySym);

   procedure AddButton(code: TButtonCode);
   begin
      include(FButtonState, code);
   end;

begin
   case key.scancode of
      SDL_SCANCODE_RIGHT: AddButton(btn_right);
      SDL_SCANCODE_LEFT: AddButton(btn_left);
      SDL_SCANCODE_DOWN: AddButton(btn_down);
      SDL_SCANCODE_UP: AddButton(btn_up);
      SDL_SCANCODE_RETURN, SDL_SCANCODE_KP_ENTER: AddButton(btn_enter);
      SDL_SCANCODE_ESCAPE, SDL_SCANCODE_INSERT: AddButton(btn_cancel);
   end;
end;

procedure T2kMapEngine.KeyUp(key: TSdlKeySym);

   procedure RemoveButton(code: TButtonCode);
   begin
      exclude(FButtonState, code);
   end;

begin
   case key.scancode of
      SDL_SCANCODE_RIGHT: RemoveButton(btn_right);
      SDL_SCANCODE_LEFT: RemoveButton(btn_left);
      SDL_SCANCODE_DOWN: RemoveButton(btn_down);
      SDL_SCANCODE_UP: RemoveButton(btn_up);
      SDL_SCANCODE_RETURN, SDL_SCANCODE_KP_ENTER: RemoveButton(btn_enter);
      SDL_SCANCODE_ESCAPE, SDL_SCANCODE_INSERT: RemoveButton(btn_cancel);
   end;
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
   begin
      if mapObj.id = 0 then
         Continue;
      for page in mapObj.pages do
         if not page.isTile then
            loadSprite(page.name);
   end;
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

   if map.id > high(FMaps) then
      setLength(FMaps, map.id + 1);
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

procedure T2kMapEngine.PartyButton(button: TButtonCode);
begin
   if button in [btn_up, btn_right, btn_down, btn_left] then
      GMoveLock.Enter;
   case button of
      btn_enter:
      begin
         if not FDontLockEnter then
            FEnterLock := true;
         GEventLock.enter;
         try
            FPartySprite.action;
         finally
            GEventLock.leave;
         end;
      end;
      btn_up: FPartySprite.move(facing_up);
      btn_down: FPartySprite.move(facing_down);
      btn_left: FPartySprite.move(facing_left);
      btn_right: FPartySprite.move(facing_right);
   end;
   if button in [btn_up, btn_right, btn_down, btn_left] then
      GMoveLock.Leave;
end;

procedure T2kMapEngine.PressButton(button: TButtonCode);
begin
   if FEnterLock and (button in [btn_enter, btn_cancel]) then
      Exit;
   case State of
      gs_map:
         if FCutscene > 0 then
            Exit
         else if (button = btn_cancel) {and FMenuEnabled} then
         begin
{            if not FDontLockEnter then
               FEnterLock := true;
            frmConsole.newScript := TConsoleEventThread.Create(SCRIPT_HEADER + 'openMenu;' + SCRIPT_FOOTER);
            FScriptEngine.registerConsoleThread(frmConsole.newScript);
            FScriptEngine.mediaPlayer.playSystemSound(sfxAccept); }
         end
         else if assigned(FPartySprite) then
            PartyButton(button);
      gs_message: {FCurrentMBox.button(button)};
      gs_menu: {FSystemMenu.button(button)};
      gs_battle: ;
      gs_fading: ;
      gs_minigame: ;
   end;
end;

procedure T2kMapEngine.repaint;
begin
   FCanvas.SetRenderer;
   SDL_SetRenderDrawColor(FCanvas.renderer, 0, 0, 0, 255);
   FCanvas.Clear;
   FCurrentMap.Draw;
   self.AfterPaint;
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
   GSpriteEngine := FCurrentMap;
   LoadMapSprites(FCurrentMap.mapObj);
   result := FSignal.WaitFor(INFINITE) = wrSignaled;
   if result then
      self.repaint;
end;

procedure T2kMapEngine.HandleEvent(event: TSdlEvent);
begin
   case event.type_ of
      SDL_KEYDOWN: KeyDown(event.key.KeySym);
      SDL_KEYUP: KeyUp(event.key.KeySym)
   end;
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

function T2kMapEngine.MapTree: IMapTree;
begin
   result := FDatabase.mapTree
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
   TRpgTimeStamp.FrameLength := max(commons.round(FTimer.latency), 1);
{   if assigned(FTransProc) then
      FCurrentMap.Draw
   else if FCurrentMap.blank then
      //do nothing
   else begin
      GRenderTargets.RenderOn(2, standardRender, 0, true);
   end; }
   FCanvas.Clear;
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

procedure T2kMapEngine.OnProcess(Sender: TObject);
var
   event: TSdlEvent;
   button: TButtonCode;
begin
   SDL_PumpEvents;
   for event in SDL_GetEvents do
      HandleEvent(event);
   for button in FButtonState do
      pressButton(button);
   FCurrentMap.Process(sender);
end;

procedure T2kMapEngine.Play;
begin
   assert(assigned(FCurrentMap));
   //clear the SDL event queue
   SDL_PumpEvents;
   SDL_FlushEvents(0, SDL_LASTEVENT);
   FTimer.Enabled := true;
   FTimer.OnTimer := self.OnTimer;
   FTimer.OnProcess := self.OnProcess;
end;

function T2kMapEngine.Playing: boolean;
begin
   result := FTimer.Enabled;
end;

end.
