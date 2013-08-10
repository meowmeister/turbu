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
   types, SyncObjs, Generics.Collections, classes, sysUtils, SqlExpr, Windows, Messages,
   turbu_map_engine, turbu_versioning, turbu_map_sprites, turbu_classes, turbu_heroes,
   turbu_database_interface, turbu_map_interface, turbu_sdl_image, turbu_2k_char_sprites,
   turbu_database, turbu_maps, turbu_tilesets, turbu_2k_sprite_engine, turbu_defs,
   turbu_2k_environment, turbu_script_engine, turbu_map_metadata, dm_shaders,
   turbu_2k_image_engine, turbu_2k_weather, turbu_transition_interface,
   timing, AsphyreTimer, SG_defs, SDL_ImageManager, sdl_canvas, SDL, sdl_13;

type
   TTileGroupPair = TPair<TTileGroupRecord, PSdlSurface>;
   TTransProc = reference to procedure(var location: integer);
   TSwitchState = (sw_noSwitch, sw_ready, sw_switching);

   T2kMapEngine = class(TMapEngine)
   private
      FStretchRatio: TSgFloatPoint;
      FSignal: TSimpleEvent;
      FButtonState: TButtonCodes;
      FSwitchState: TSwitchState;
      FTeleportThread: TThread;
      FTitleScreen: TRpgTimestamp;
      FHWND: HWND;
      FTransition: ITransition;
      FTransitionFirstFrameDrawn: boolean;
      FRenderPause: TRpgTimestamp;
      FRenderPauseLock: TCriticalSection;
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
      FObjectManager: TMapObjectManager;
      FShaderEngine: TdmShaders;
      FImageEngine: TImageEngine;
      FWeatherEngine: TWeatherSystem;
      FPlaying: boolean;

      function retrieveImage(const folder, filename: string): TRpgSdlImage;

      procedure repaint;
      procedure loadTileset(const value: TTileSet);
      procedure loadSprite(const filename: string);
      function CreateViewport(map: TRpgMap; center: TSgPoint): TRect;
      procedure CanvasResize (sender: TSdlCanvas);
      procedure LoadMapSprites(map: TRpgMap);
      function doneLoadingMap: boolean;
      procedure prepareMap(const data: IMapMetadata);
      procedure initializeParty;
   private //timing and animation
      FFrame: integer;
      FHeartbeat: integer;
      FEnterLock: boolean;
      FSaveLock: boolean;
      FDontLockEnter: boolean;
      FCutscene: integer;
      procedure OnTimer(Sender: TObject);
      procedure standardRender(Sender: TObject);
      procedure RenderImages(sender: TObject);
      procedure OnProcess(Sender: TObject);
      procedure PressButton(button: TButtonCode);
      procedure PartyButton(button: TButtonCode);
      procedure Validate(query: TSqlQuery);
      procedure PlayMapMusic(metadata: TMapMetadata);
      procedure DrawRenderTarget(target: TSdlRenderTarget; canTint: boolean);
      procedure SetupScriptImports;
      procedure RenderFrame(Sender: TObject);
      procedure SetTransition(const Value: ITransition);
      procedure DrawWeather;
   protected
      FDatabaseOwner: boolean;
      procedure cleanup; override;
      procedure AfterPaint; virtual;
      function GetValidateProc: TProc<TSqlQuery>; virtual;
      procedure Quicksave;
      procedure Quickload;
   public
      constructor Create; override;
      destructor Destroy; override;
      function initialize(window: TSdlWindow; const database: string): TSdlWindow; override;
      procedure loadMap(map: IMapMetadata); overload; override;
      procedure loadMap(id: integer); reintroduce; overload;
      procedure Play; override;
      function Playing: boolean; override;
      function MapTree: IMapTree; override;
      procedure NewGame; override;
      procedure changeMaps(newmap: word; newLocation: TSgPoint);

      //script-accessible functions
      procedure loadRpgImage(filename: string; mask: boolean);
      procedure TitleScreen; virtual;
      procedure EnterCutscene;
      procedure LeaveCutscene;
      function ReadKeyboardState: TButtonCodes;
      function EnsureTileset(id: integer): boolean;
      procedure RenderPause;
      procedure RenderUnpause;

      property PartySprite: THeroSprite read FPartySprite;
      property ImageEngine: TImageEngine read FImageEngine;
      property WeatherEngine: TWeatherSystem read FWeatherEngine;
      property CurrentMap: T2kSpriteEngine read FCurrentMap;
      property Transition: ITransition read FTransition write SetTransition;
   end;

var
   GGameEngine: T2kMapEngine;

implementation
uses
//   FastMM4,
   math, Forms, Dialogs, OpenGL, IOUtils,
   archiveInterface, commons, turbu_plugin_interface, turbu_game_data, turbu_OpenGl,
   turbu_constants, turbu_functional, dm_database, turbu_2k_images, turbu_2k_savegames,
   turbu_map_objects, turbu_2k_map_locks, turbu_2k_frames, turbu_text_utils,
   turbu_2k_transitions_graphics,
   rs_maps, rs_message, rs_characters, rs_media, archiveUtils, project_folder,
   logs, EventBuilder,
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

procedure T2kMapEngine.AfterPaint;
begin
   //this virtual method intentionally left blank
end;

procedure T2kMapEngine.CanvasResize(sender: TSdlCanvas);
var
   i: integer;
begin
   GRenderTargets.Clear;
   for i := 1 to 5 do
      GRenderTargets.Add(TSdlRenderTarget.Create(FCanvas.size));
end;

procedure T2kMapEngine.cleanup;
var i: integer;
begin
   assert(FInitialized);
   FInitialized := false;
   if FDatabaseOwner then
   begin
      FObjectManager.ScriptEngine.KillAll;
      FreeAndNil(GMenuEngine);
   end;
   FreeAndNil(FShaderEngine);
   GEnvironment.Free;   //FreeAndNil sets the reference to nil first, which will
   GEnvironment := nil; //break things that require the global for cleanup
   FreeAndNil(FPartySprite);
   FreeAndNil(FCanvas);
   FreeAndNil(FImages);
   FreeAndNil(FSignal);
   FreeAndNil(FImageEngine);
   FreeAndNil(FWeatherEngine);
   for I := 0 to high(FMaps) do
      FMaps[i].Free;
   setLength(FMaps, 0);
   if FDatabaseOwner then
   begin
      GGameEngine := nil;
      FreeAndNil(FDatabase);
      GDatabase := nil;
      FreeAndNil(dmDatabase);
      FreeAndNil(FObjectManager);
      GMapObjectManager := nil;
      GScriptEngine := nil;
   end;
   inherited Cleanup;
end;

constructor T2kMapEngine.Create;
begin
   inherited Create;
   self.data := TMapEngineData.Create('TURBU basic map engine', TVersion.Create(0, 1, 0));
   FTimer := TAsphyreTimer.Create;
   FTimer.MaxFPS := 60;
   FRenderPauseLock := TCriticalSection.Create;
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
   FTitleScreen.Free;
   FRenderPause.Free;
   FRenderPauseLock.Free;
   inherited;
end;

function T2kMapEngine.initialize(window: TSdlWindow; const database: string): TSdlWindow;
var
   layout: TGameLayout;
   renderer: TSdlRenderer;
   info: sdl_13.TSDL_SysWMinfo;
   trn: TTransitionTypes;
   sfx: TSfxTypes;
   bgm: TBgmTypes;
begin
   if FInitialized then
      Exit(window);
   try
      FInitialized := true;
      inherited initialize(window, database);
      if dmDatabase = nil then
      begin
         FDatabaseOwner := true;
         GGameEngine := self;
         dmDatabase := TdmDatabase.Create(nil);
         dmDatabase.Connect(database, self.GetValidateProc());
         FDatabase := TRpgDatabase.Load(dmDatabase);
         GDatabase := FDatabase;
         if not assigned(FDatabase) then
            raise ERpgPlugin.Create('Incompatible project database');
         FObjectManager := TMapObjectManager.Create;
         GScriptEngine.OnEnterCutscene := self.EnterCutscene;
         GScriptEngine.OnLeaveCutscene := self.LeaveCutscene;
         GScriptEngine.OnRenderUnpause := self.RenderUnpause;
         GEnvironment := T2kEnvironment.Create(FDatabase);
         SetupScriptImports;
         FObjectManager.LoadGlobalScripts(GDatabase.globalEvents);
         FObjectManager.OnUpdate := GEnvironment.UpdateEvents;
      end
      else begin
         FDatabase := GDatabase;
         FObjectManager := GMapObjectManager;
      end;

      layout := FDatabase.layout;
      if window.ptr = nil then
      begin
         window :=  SDL_CreateWindow(PAnsiChar(ansiString(format('TURBU engine - %s', [FDatabase.mapTree[0].name]))),
                           SDL_WINDOWPOS_CENTERED_MASK, SDL_WINDOWPOS_CENTERED_MASK,
                           layout.physWidth, layout.physHeight,
                           [sdlwOpenGl, sdlwShown {,sdlwResizable, sdlwInputGrabbed}]);

         if window.ptr = nil then
            raise ERpgPlugin.CreateFmt('Unable to initialize SDL window: %s%s', [LFCR, SDL_GetError]);
         renderer := SDL_CreateRenderer(window, SDL_RendererIndex('opengl'), [sdlrAccelerated]);
         if renderer.ptr = nil then
            raise ERpgPlugin.CreateFmt('Unable to initialize SDL renderer: %s%s', [LFCR, SDL_GetError]);
         SDL_RenderPresent(renderer);
      end
      else renderer := SDL_GetRenderer(window);
      sdl_13.SDL_VERSION(info.version);
      assert(SDL_GetWindowWMInfo(window, info));
      FHWND := info.window;
      FCanvas := TSdlCanvas.CreateFrom(window);
      FCanvas.OnResize := self.CanvasResize;
      FStretchRatio.x := layout.physWidth / layout.width;
      FStretchRatio.y := layout.physHeight / layout.height;
      SDL_RenderSetLogicalSize(SDL_GetRenderer(window), layout.width, layout.height);
      FImages := TSdlImages.Create(renderer);
      FImages.ArchiveCallback := aCallback;
      FImages.ArchiveLoader := aLoader;
      FImages.SpriteClass := TRpgSdlImage;
      FWeatherEngine := TWeatherSystem.Create(nil, FImages, FCanvas);
      setLength(FMaps, FDatabase.mapTree.lookupCount);
      FSignal := TSimpleEvent.Create;
      FSignal.SetEvent;
      FShaderEngine := TdmShaders.Create(nil);
      if FDatabaseOwner then
      begin
         GFontEngine := TFontEngine.Create(FShaderEngine);
         GFontEngine.Current := TRpgFont.Create('RMG2000_0.fon');
         GMenuEngine := TMenuSpriteEngine.Create(
           TSystemImages.Create(FImages, layout.systemGraphic,
             layout.wallpaperStretch, layout.translucentMessages),
           FCanvas, FImages);
         for trn := Low(TTransitionTypes) to High(TTransitionTypes) do
            rs_maps.setTransition(trn, TTransitions(layout.transition[trn] + 1));
         for sfx := low(TSfxTypes) to high(TSfxTypes) do
            rs_media.SetSystemSoundData(sfx, FDatabase.sfx[sfx]);
         for bgm := low(TBgmTypes) to high(TBgmTypes) do
            rs_media.SetSystemMusicData(bgm, FDatabase.bgm[bgm])
      end;
   except
      cleanup;
      raise;
   end;
   result := window;
end;

procedure T2kMapEngine.initializeParty;
var
   i: integer;
   party: TRpgParty;
begin
   party := GEnvironment.Party;
   for I := 1 to FDatabase.layout.startingHeroes do
      party.hero[i] := GEnvironment.heroes[FDatabase.layout.startingHero[i]];
   FPartySprite := THeroSprite.create(FCurrentMap, party.hero[1], party);
end;

procedure T2kMapEngine.PlayMapMusic(metadata: TMapMetadata);
var
   id: smallint;
begin
   id := metadata.id;
   case metadata.bgmState of
      id_parent:
      begin
         repeat
            id := FDatabase.mapTree[id].parent;
         until (id = 0) or (FDatabase.mapTree[id].bgmState <> id_parent);
         if id = 0 then
            rs_media.stopMusic
         else if FDatabase.mapTree[id].bgmState = id_yes then
            rs_media.playMusicData(FDatabase.mapTree[id].bgmData);
      end;
      id_no: ;
      id_yes: rs_media.playMusicData(FDatabase.mapTree[id].bgmData);
   end;
end;

procedure T2kMapEngine.changeMaps(newmap: word; newLocation: TSgPoint);
var
   hero: TCharSprite;
   oldEngine: T2kSpriteEngine;
   metadata: TMapMetadata;
begin
   assert(newmap <> FCurrentMap.mapID);

   FSwitchState := sw_ready;
   FObjectManager.ScriptEngine.killAll;
   while not FCurrentMap.blank do
      sleep(10);
   FTeleportThread := TThread.CurrentThread;
   try
   runThreadsafe(
      procedure
      begin
         FreeAndNil(FImageEngine);
         oldEngine := FCurrentMap;
         hero := FCurrentMap.CurrentParty;
         if assigned(hero) then
            (hero as THeroSprite).packUp;
         metadata := FDatabase.mapTree[newmap];
         GScriptEngine.TeleportThread := FTeleportThread as TScriptThread;
         self.loadMap(metadata);
         if not GEnvironment.preserveSpriteOnTeleport then
            GEnvironment.Party.ResetSprite;
         FCurrentMap.CurrentParty := hero;
         FCurrentMap.CopyState(oldEngine);
         if assigned(hero) then
         begin
            hero.location := newLocation;
            (hero as THeroSprite).settleDown(FCurrentMap);
         end;
         FCurrentMap.centerOn(newLocation.x, newLocation.y);
      end, true);
   finally
      FTeleportThread := nil;
   end;
   PlayMapMusic(metadata);
   FSwitchState := sw_noSwitch;
   FTimer.Enabled := true;
end;

procedure T2kMapEngine.loadMap(map: IMapMetadata);
var
   viewport: TRect;
begin
   prepareMap(map);
   viewport := createViewport(FWaitingMap, FScrollPosition);
   if assigned(FMaps[FWaitingMap.id]) then
      FMaps[FWaitingMap.id].ReloadMapObjects
   else begin
      ensureTileset(FWaitingMap.tileset);
      FMaps[FWaitingMap.id] := T2kSpriteEngine.Create(FWaitingMap, viewport,
                               FShaderEngine, FCanvas, FDatabase.tileset[FWaitingMap.tileset],
                               FImages);
   end;
   if not doneLoadingMap then
      raise Exception.Create('Error loading map');
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
   GEnvironment.ClearEvents;
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

   case GMenuEngine.State of
      msNone:
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
      msShared, msExclusiveShared:
      begin
         GMenuEngine.button(button);
         FEnterLock := true;
      end;
      msFull:;
   end;
end;

procedure T2kMapEngine.Quicksave;
var
   savefile: string;
begin
   savefile := TPath.Combine(GProjectFolder, 'quicksave.tsg');
   turbu_2k_savegames.SaveTo(savefile, GEnvironment.Party.mapID, false);
end;

procedure T2kMapEngine.Quickload;
var
   savefile: string;
   hero: THeroSprite;
begin
   savefile := TPath.Combine(GProjectFolder, 'quicksave.tsg');
   if not FileExists(savefile) then
      Exit;
   FTimer.Enabled := false;
   GScriptEngine.KillAll;
   GEnvironment.Free;
   FPartySprite.Free;
   GEnvironment := T2kEnvironment.Create(FDatabase);
   GScriptEngine.Reset;
   SetupScriptImports;
   initializeParty;
   hero := GEnvironment.Party.Sprite as THeroSprite;
   hero.packUp;
   FPlaying := false;
   turbu_2k_savegames.Load(savefile);
   hero.settleDown(FCurrentMap);
   FCurrentMap.CurrentParty := hero;
   FreeAndNil(FImageEngine);
   FImageEngine := TImageEngine.Create(GSpriteEngine, FCanvas, FImages);
   GEnvironment.CreateTimers;
   GMapObjectManager.InCutscene := false;
   Play;
end;

function KeyIsPressed(value: integer): boolean;
begin
   result := GetAsyncKeyState(value) <> 0;
end;

function T2kMapEngine.ReadKeyboardState: TButtonCodes;
begin
   result := [];
   if keyIsPressed(VK_LEFT) then
      include(result, btn_left);
   if keyIsPressed(VK_RIGHT) then
      include(result, btn_right);
   if keyIsPressed(VK_UP) then
      include(result, btn_up);
   if keyIsPressed(VK_DOWN) then
      include(result, btn_down);
   if keyIsPressed(VK_RETURN) then
      include(result, btn_enter);
   if keyIsPressed(VK_ESCAPE) then
      include(result, btn_cancel)
   else if KeyIsPressed(VK_INSERT) then
      include(result, btn_cancel);
   if result * [btn_enter, btn_cancel] = [] then
      FEnterLock := false;
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

procedure T2kMapEngine.SetTransition(const Value: ITransition);
begin
   FTransition := Value;
   FTransitionFirstFrameDrawn := false;
   RenderUnpause;
end;

procedure T2kMapEngine.SetupScriptImports;
var
   script: string;
begin
   FObjectManager.ScriptEngine.LoadEnvironment(@turbu_2k_environment.RegisterEnvironment);
   rs_maps.RegisterScriptUnit(FObjectManager.ScriptEngine);
   rs_message.RegisterScriptUnit(FObjectManager.ScriptEngine);
   rs_characters.RegisterScriptUnit(FObjectManager.ScriptEngine);
   script := dmDatabase.ScriptLookup(0);
   if script = BAD_LOOKUP then
      script := GDatabase.scriptBlock.GetScript(0);
   {$IFDEF DEBUG}
   logs.logText(script);
   {$ENDIF}
   FObjectManager.ScriptEngine.LoadLibrary(script);
end;

procedure T2kMapEngine.Validate(query: TSqlQuery);
var
   version: integer;
begin
   query.SQL.Text := 'select ID from DBDATA';
   query.Open;
   if query.RecordCount <> 1 then
      raise EBadDB.CreateFmt('Invalid DBDATA record count: %d. The database may be corrupted', [query.RecordCount]);
   version := query.FieldByName('ID').AsInteger;
   if version < DBVERSION then
   begin
      if version >= MIN_DBVERSION then
         MessageDlg('This project is using an out-of-date database and can''t be'+
            ' loaded with the current map engine.  Please open it in the TURBU editor to upgrade the project.', mtWarning, [mbOK], 0)
      else MessageDlg('This project is using an out-of-date database and can''t be loaded.', mtError, [mbOK], 0);
      Application.Terminate;
      Abort;
   end;
end;

function T2kMapEngine.GetValidateProc: TProc<TSqlQuery>;
begin
   result := self.Validate;
end;

function T2kMapEngine.doneLoadingMap: boolean;
begin
   FCurrentMap := FMaps[FWaitingMap.id];
   FCurrentMap.OnDrawWeather := self.DrawWeather;
   if FImageEngine = nil then
   begin
      FImageEngine := TImageEngine.Create(FCurrentMap, FCanvas, FImages);
      GEnvironment.CreateTimers;
   end;
   GSpriteEngine := FCurrentMap;
   LoadMapSprites(FCurrentMap.mapObj);
   FObjectManager.LoadMap(FWaitingMap, FTeleportThread);
   if FPlaying then
      FObjectManager.Tick;
   FCurrentMap.Dead;
   result := FSignal.WaitFor(INFINITE) = wrSignaled;
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

procedure T2kMapEngine.NewGame;
var
   loc: TLocation;
   metadata: TMapMetadata;
begin
   loc := FDatabase.mapTree.location[-1];
   metadata := FDatabase.mapTree[loc.map];
   self.loadMap(metadata);
   InitializeParty;
   FPartySprite.leaveTile;
   FCurrentMap.CurrentParty := FPartySprite;
   FPartySprite.location := sgPoint(loc.x, loc.y);
   PlayMapMusic(metadata);
   self.Play;
end;

procedure T2kMapEngine.loadSprite(const filename: string);
var
   lName: string;
begin
   lname := 'mapsprite\' + filename + '.png';
   if not FImages.contains(filename) then
      FImages.AddSpriteFromArchive(lname, filename, SPRITE_SIZE);
end;

procedure T2kMapEngine.standardRender(Sender: TObject);
begin
   if FSwitchState = sw_switching then
      Exit;
   case GMenuEngine.State of
      msNone, msShared, msExclusiveShared: FCurrentMap.Draw;
      msFull:;
   end;
   if FSwitchState = sw_ready then
      FSwitchState := sw_switching;
end;

procedure T2kMapEngine.RenderImages(sender: TObject);
begin
   if assigned(FImageEngine) then
      FImageEngine.Draw;
end;

procedure T2kMapEngine.RenderPause;
begin
   FRenderPauseLock.Enter;
   FRenderPause.Free;
   FRenderPause := TRpgTimestamp.Create(50);
   FRenderPauseLock.Leave;
end;

procedure T2kMapEngine.RenderUnpause;
begin
   FRenderPauseLock.Enter;
   FreeAndNil(FRenderPause);
   FRenderPauseLock.Leave;
end;

procedure T2kMapEngine.TitleScreen;
var
   cls: TSdlImageClass;
begin
   cls := FImages.SpriteClass;
   FImages.SpriteClass := TSdlImage;
   try
      FImageEngine.Clear;
      FImages.EnsureImage(format('Special Images\%s.png', [FDatabase.layout.titleScreen]), '*TitleScreen');
      GEnvironment.Image[0] := TRpgImage.Create(FImageEngine, '*TitleScreen', FCanvas.Width div 2, FCanvas.Height div 2, 0, 0, 100, false, false);
      FTitleScreen := TRpgTimestamp.Create(5000);
   finally
      FImages.SpriteClass := cls;
   end;
end;

procedure T2kMapEngine.DrawWeather;
begin
   FWeatherEngine.Draw;
end;

procedure T2kMapEngine.DrawRenderTarget(target: TSdlRenderTarget; canTint: boolean);
var
   current: integer;
   r, g, b, a: byte;
begin
   glCheckError;
   glPushAttrib(GL_ALL_ATTRIB_BITS);
//   glPushAttrib(GL_ENABLE_BIT or GL_COLOR_BUFFER_BIT or GL_TEXTURE_BIT);
   glColor4f(1, 1, 1, 1);
   glGetIntegerv(GL_CURRENT_PROGRAM, @current);
   if not (canTint and FCurrentMap.Fade) then
      FShaderEngine.UseShaderProgram(FShaderEngine.ShaderProgram('default', 'defaultF') {4});
   glEnable(GL_ALPHA_TEST);
   glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
   glEnable(GL_BLEND);
   glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

   target.DrawFull();

   glPopAttrib;
   SDL_GetRenderDrawColor(target.parent.Renderer, r, g, b, a);
   glColor4f(r / 255, g / 255, b / 255, a / 255);
   glUseProgram(current);
end;

function T2kMapEngine.EnsureTileset(id: integer): boolean;
begin
   result := FDatabase.tileset.ContainsKey(id);
   if not result then
   try
      loadTileset(FDatabase.tileset[id]);
      result := true;
   except
   end;
end;

procedure T2kMapEngine.EnterCutscene;
begin
   GMapObjectManager.InCutscene := true;
   Inc(FCutscene);
end;

procedure T2kMapEngine.LeaveCutscene;
begin
   if FCutscene <= 0 then
      raise Exception.Create('Mismatched call to T2kMapEngine.LeaveCutscene');
   dec(FCutscene);
   GMapObjectManager.InCutscene := FCutscene <> 0;
end;

procedure T2kMapEngine.loadMap(id: integer);
begin
   try
      if id <> GEnvironment.Party.mapID then
         loadMap(GDatabase.mapTree[id])
   except
      raise Exception.CreateFmt('Invalid map ID: %d', [id]);
   end;
end;

procedure WriteTimestamp;
var
   hour, min, sec, msec: word;
begin
   decodeTime(sysUtils.GetTime, hour, min, sec, msec);
   commons.OutputFormattedString('Frame timestamp: %d:%d:%d.%d', [hour, min, sec, msec]);
end;

//var newAG: integer;

procedure T2kMapEngine.RenderFrame(Sender: TObject);
begin
   GRenderTargets.RenderOn(RENDERER_MAP, standardRender, 0, true);

   DrawRenderTarget(GRenderTargets[RENDERER_MAP], true);

   RenderImages(self);
   FCurrentMap.DrawFlash;

   if GMenuEngine.State <> msNone then
      GMenuEngine.Draw;
end;

{$R-}
procedure T2kMapEngine.OnTimer(Sender: TObject);
begin
   if assigned(FTitleScreen) and (FTitleScreen.timeRemaining = 0) then
   begin
      FTimer.Enabled := false;
      FreeAndNil(FTitleScreen);
      Application.Terminate;
      Exit;
   end;

{   if newAG > 0 then
   begin
      FastMM4.LogAllocatedBlocksToFile(newAG, newAG);
      FastMM4.PopAllocationGroup;
   end;
   inc(newAG);
   FastMM4.PushAllocationGroup(newAG);
}

   //TODO: Remove commented-out code blocks when transitions are implemented
   inc(FFrame);
   TRpgTimeStamp.NewFrame;
   FRenderPauseLock.Enter;
   try
      if assigned(FRenderPause) and (FRenderPause.timeRemaining = 0) then
         FreeAndNil(FRenderPause);
      if FRenderPause = nil then
      begin
         if assigned(FTransition) then
         begin
            if not FTransitionFirstFrameDrawn then
            begin
               GRenderTargets.RenderOn(RENDERER_ALT, RenderFrame, 0, true);
               FTransitionFirstFrameDrawn := true;
            end;
            if not FTransition.Draw then
            begin
               FTransitionFirstFrameDrawn := false;
               FTransition := nil;
            end;
         end
         else begin
            if FCurrentMap.blank then
               GRenderTargets.RenderOn(RENDERER_MAIN, nil, 0, true) //just clear the target
            else GRenderTargets.RenderOn(RENDERER_MAIN, RenderFrame, 0, true);
            DrawRenderTarget(GRenderTargets[RENDERER_MAIN], false);
         end;

         FCanvas.Flip;
      end;
   finally
      FRenderPauseLock.Leave;
   end;
//   WriteTimestamp;
   if FFrame > FHeartbeat then
   begin
      FCurrentMap.advanceFrame;
      FFrame := 0;
   end;
   FTimer.Process;
end;
{$R+}

procedure T2kMapEngine.OnProcess(Sender: TObject);
var
   button: TButtonCode;
begin
   if assigned(FTitleScreen) or (FSwitchState <> sw_noSwitch) then
      Exit;
   FButtonState := ReadKeyboardState;
   for button in FButtonState do
      pressButton(button);
   if not FSaveLock then
   begin
      if KeyIsPressed(VK_F2) then
      begin
         Quicksave;
         FSaveLock := true;
      end
      else if KeyIsPressed(VK_F6) then
      begin
         Quickload;
         FSaveLock := true;
      end;
   end
   else FSaveLock := KeyIsPressed(VK_F2) or KeyIsPressed(VK_F6);
   GMapObjectManager.Tick;
   FCurrentMap.Process(sender);
end;

procedure T2kMapEngine.Play;
begin
   assert(assigned(FCurrentMap));
   if FPartySprite = nil then
      initializeParty;
   FTimer.OnTimer := self.OnTimer;
   FTimer.OnProcess := self.OnProcess;
   FTimer.Enabled := true;
   FPlaying := true;
end;

function T2kMapEngine.Playing: boolean;
begin
   result := FTimer.Enabled;
end;

procedure T2kMapEngine.loadRpgImage(filename: string; mask: boolean);
const MODES: array [boolean] of TSdlBlendModes = ([], [sdlbBlend]);
var
   oName: string;
   image: TSdlImage;
   cls: TSdlImageClass;
begin
   oName := filename;
   if not archiveUtils.GraphicExists(filename, 'pictures') then
      Abort;

   cls := FImages.SpriteClass;
   FImages.SpriteClass := TSdlImage;
   try
      image := FImages.EnsureImage('pictures/' + filename, oName);
      SDL_SetTextureBlendMode(image.surface, MODES[mask])
   finally
      FImages.SpriteClass := cls;
   end;
end;

end.
