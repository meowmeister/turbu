unit mainform;

interface

uses
   Forms, Classes, Controls, Messages,
   JvPluginManager,
   turbu_plugin_interface, turbu_map_engine, turbu_battle_engine,
   sdl_frame;

type
   TfrmMain = class(TForm)
      imgGame: TSdlFrame;
      procedure FormCreate(Sender: TObject);
      procedure imgGameAvailable(Sender: TObject);
      procedure FormDestroy(Sender: TObject);
   private
      pluginManager: TJvPluginManager;
      FDatabaseName: string;
      FMapEngine: IMapEngine;
      procedure Available(var msg: TMessage); message WM_USER;
      procedure Boot;
      procedure loadEngine(data: TEngineData);
      procedure LoadEngines;
   end;

var
  frmMain: TfrmMain;

implementation
uses
   Windows, SysUtils, IOUtils,
   project_folder, engine_manager, ArchiveInterface, DiscInterface,
   turbu_engines, turbu_constants,
   dm_ProjectBoot,
   sdl_image, sdlstreams,
   SDL, SDL_13;

{$R *.dfm}

procedure TfrmMain.loadEngine(data: TEngineData);
var
   iClass: TRpgPlugBase;
   bEngine: IBattleEngine;
   mEngine: IMapEngine;
begin
   iClass := data.engine.Create;
   case data.style of
      et_map:
      begin
         assert(iclass.GetInterface(IMapEngine, mEngine));
         addEngine(et_map, mEngine.getData, mEngine);
      end;
      et_battle:
      begin
         assert(iclass.GetInterface(IBattleEngine, bEngine));
         addEngine(et_battle, bEngine.getData, bEngine);
      end;
      et_menu: assert(false);
      et_minigame: assert(false);
      else assert(false);
   end;
end;

procedure TfrmMain.LoadEngines();
var
   infile: TStream;
   plugins: TStringList;
   engines: TEngineDataList;
   plugStr: string;
   i, j: integer;
   pluginIntf: ITurbuPlugin;
begin
   pluginManager := dmEngineManager.pluginManager;
   inFile := nil;
   plugins := TStringList.Create;
   try
      inFile := GArchives[BASE_ARCHIVE].getFile('plugins');
      plugins.LoadFromStream(inFile);
      for plugStr in plugins do
      try
         pluginManager.LoadPlugin(plugStr, plgPackage);
      except
         on E: EPackageError do
            raise; //TODO: Add code here to handle package loading errors
      end;
   finally
      plugins.free;
      inFile.free;
   end;
   for I := 0 to pluginManager.PluginCount - 1 do
   begin
      assert(pluginManager.Plugins[i].GetInterface(ITurbuPlugin, pluginIntf));
      engines := pluginIntf.listPlugins;
      for j := 0 to engines.Count - 1 do
         loadEngine(engines[j]);
      engines.free;
   end;
end;

function GetProject: string;
begin
   if ParamCount = 0 then
      result := IncludeTrailingPathDelimiter(TPath.GetDirectoryName(ParamStr(0)))
   else result := IncludeTrailingPathDelimiter(ParamStr(1));
end;

procedure openArchive(const folderName: string; index: integer);
var
   filename: string;
begin
   filename := GProjectFolder + folderName;
   if not DirectoryExists(filename) then
      raise EFileNotFoundException.CreateFmt('Required folder %s does not exist', [folderName]);
   ForceDirectories(filename);
   assert(GArchives.Add(openFolder(filename)) = index);
end;

procedure TfrmMain.Available(var msg: TMessage);
begin
   imgGame.ClearTextures;
   Boot;
   FMapEngine.initialize(imgGame.SdlWindow, FDatabaseName);
   FMapEngine.NewGame;
end;

procedure TfrmMain.Boot;
var
   boot: TdmProjectBoot;
begin
   GProjectFolder := GetProject;
   FDatabaseName := TPath.Combine(GProjectFolder, 'turbu.tdb');
   openArchive(MAP_DB, MAP_ARCHIVE);
   openArchive(IMAGE_DB, IMAGE_ARCHIVE);
   openArchive(SCRIPT_DB, SCRIPT_ARCHIVE);
   openArchive(MUSIC_DB, MUSIC_ARCHIVE);
   openArchive(SFX_DB, SFX_ARCHIVE);
   openArchive(VIDEO_DB, VIDEO_ARCHIVE);

   LoadEngines();

   boot := TdmProjectBoot.Create(nil);
   try
      FMapEngine := boot.Boot(FDatabaseName);
   finally
      boot.Free;
   end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
   dmEngineManager := TdmEngineManager.Create(self);
   SDL_InitSubSystem(SDL_INIT_VIDEO);
   self.pluginManager := dmEngineManager.pluginManager;
   assert(GArchives.Add(openFolder(GProjectFolder + DESIGN_DB)) = BASE_ARCHIVE);
   TThread.NameThreadForDebugging('TURBU main thread');
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
   FMapEngine := nil;
   CleanupEngines;
end;

procedure TfrmMain.imgGameAvailable(Sender: TObject);
var
   surface: PSdlSurface;
   convert1, convert2: PSdlSurface;
   rw: PSDL_RWops;
   stream: TResourceStream;
   index: integer;
begin
   stream := TResourceStream.Create(HInstance, 'LOGO', RT_RCDATA);
   rw := SDLStreamSetup(stream);
   surface := pointer(IMG_LoadPNG_RW(rw));
   assert(assigned(surface));
   convert1 := TSdlSurface.Create(1, 1, 32);
   convert2 := TSdlSurface.Convert(surface, convert1.Format);
   index := imgGame.AddTexture(convert2);
   SDLStreamCloseRWops(rw);
   stream.Free;
   surface.Free;
   convert1.Free;
   imgGame.DrawTexture(index);
   PostMessage(self.Handle, WM_USER, 0, 0);
end;

end.
