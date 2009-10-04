unit turbu_main;
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
   SysUtils, Classes, Controls, Forms, Menus, Graphics, ExtCtrls, StdCtrls,
   design_script_engine, Dialogs, JvComponentBase, JvPluginManager,
   turbu_plugin_interface, turbu_engines, turbu_map_engine, turbu_map_interface,
   sdl_frame, ComCtrls, sdl, sdl_13;

type
   TfrmTurbuMain = class(TForm)
      mnuMain: TMainMenu;
      mnuFile: TMenuItem;
      mnuNew: TMenuItem;
      mnuOpen: TMenuItem;
      mnuImport: TMenuItem;
      mnuSep1: TMenuItem;
      mnuExit: TMenuItem;
      mnu2K: TMenuItem;
      mnuEdit1: TMenuItem;
      mnuSkillEdit: TMenuItem;
      mnuDatabase: TMenuItem;
      dlgOpen: TOpenDialog;
      pluginManager: TJvPluginManager;
      pnlSidebar: TPanel;
      sbxPallette: TScrollBox;
      splSidebar: TSplitter;
      imgPalette: TSdlFrame;
      trvMapTree: TTreeView;
      sbPalette: TScrollBar;
      imgLogo: TSdlFrame;
      pnlHorizScroll: TPanel;
      sbHoriz: TScrollBar;
      pnlCorner: TPanel;
      pnlVertScroll: TPanel;
      sbVert: TScrollBar;
      procedure mnu2KClick(Sender: TObject);
      procedure mnuSkillEditClick(Sender: TObject);
      procedure FormShow(Sender: TObject);
      procedure mnuDatabaseClick(Sender: TObject);
      procedure mnuExitClick(Sender: TObject);
      procedure mnuOpenClick(Sender: TObject);
      procedure FormCreate(Sender: TObject);
      procedure imgLogoAvailable(Sender: TObject);
      procedure FormDestroy(Sender: TObject);
      procedure sbPaletteScroll(Sender: TObject; ScrollCode: TScrollCode;
        var ScrollPos: Integer);
      procedure splSidebarMoved(Sender: TObject);
      procedure OnScrollMap(Sender: TObject; ScrollCode: TScrollCode;
        var ScrollPos: Integer);
   private
      FMapEngine: IDesignMapEngine;
      FCurrentMap: IRpgMap;
      FPaletteTexture: integer;
      procedure loadEngine(data: TEngineData);
      procedure loadProject;
      procedure openProject(location: string);
      procedure closeProject;

      procedure assignPaletteImage(surface: PSdlSurface);
      procedure displayPalette(height: integer);
      procedure resizePalette;

      procedure configureScrollBars(const size, position: TPoint);
   public
      { Public declarations }
   end;

var
  frmTurbuMain: TfrmTurbuMain;

implementation

uses
   types, DBClient, windows, generics.Collections, math,
   commons, rm_converter, skill_settings, turbu_database, archiveInterface,
   turbu_constants, turbu_characters, database, turbu_battle_engine, turbu_classes,
   turbu_versioning, turbu_maps, turbu_tilesets,
   dm_database, discInterface, formats, strtok,
   sdl_image, sdlstreams, sg_defs;

{$R *.dfm}

const
   TILE_SIZE: integer = 16;

procedure TfrmTurbuMain.displayPalette(height: integer);
var
   texture: TSdlTexture;
   displayRect: TRect;
begin
   assert(FPaletteTexture > -1);
   texture := imgPalette.textures[FPaletteTexture];
   height := min(height, texture.size.Y - sbPalette.pageSize);
   displayRect := rect(0, height, imgPalette.width div 2, imgPalette.height div 2);
   imgPalette.drawTexture(texture, @displayRect, nil);
   imgPalette.Flip;
end;

procedure TfrmTurbuMain.resizePalette;
var
   texture: TSdlTexture;
begin
   texture := imgPalette.textures[FPaletteTexture];
   sbPalette.Max := texture.size.y;
   sbPalette.PageSize := imgPalette.height div 2;
   sbPalette.LargeChange := sbPalette.PageSize - TILE_SIZE;
   displayPalette(sbPalette.Position);
end;

procedure TfrmTurbuMain.assignPaletteImage(surface: PSdlSurface);
var
   dummy: TSdlTexture;
begin
   FPaletteTexture := imgPalette.AddTexture(surface, dummy);
   resizePalette;
end;

procedure TfrmTurbuMain.closeProject;
begin
   //check for modifications first
   FreeAndNil(GDatabase);
   GArchives.clearFrom(1);
end;

procedure TfrmTurbuMain.configureScrollBars(const size, position: TPoint);
begin
   sbHoriz.Max := size.x;
   sbHoriz.PageSize := imgLogo.width;
   sbHoriz.LargeChange := sbHoriz.PageSize - TILE_SIZE;
   sbHoriz.Position := position.X;
   sbVert.Max := size.y;
   sbVert.PageSize := imgLogo.height;
   sbVert.LargeChange := sbVert.PageSize - TILE_SIZE;
   sbVert.Position := position.Y;
end;

procedure TfrmTurbuMain.FormCreate(Sender: TObject);
begin
   if getProjectFolder = '' then
      createProjectFolder;
   FPaletteTexture := -1;
end;

procedure TfrmTurbuMain.FormDestroy(Sender: TObject);
begin
   cleanupEngines;
   FCurrentMap := nil;
   FMapEngine := nil;
   GScriptEngine := nil;
end;

procedure TfrmTurbuMain.FormShow(Sender: TObject);
var
   infile: TStream;
   plugins: TStringList;
   engines: TEngineDataList;
   plugStr: string;
   i, j: integer;
   pluginIntf: ITurbuPlugin;
begin
   GProjectFolder := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
   assert(GArchives.Add(openFolder(GProjectFolder + DESIGN_DB)) = BASE_ARCHIVE);
   plugins := TStringList.Create;
   try
      inFile := GArchives[BASE_ARCHIVE].getFile('plugins');
      try
         plugins.LoadFromStream(inFile);
      finally
         inFile.free;
      end;
      for plugStr in plugins do
      begin
         pluginManager.LoadPlugin(plugStr, plgPackage);
      end;
   finally
      plugins.free;
   end;
   for I := 0 to pluginManager.PluginCount - 1 do
   begin
      assert(pluginManager.Plugins[i].GetInterface(ITurbuPlugin, pluginIntf));
      engines := pluginIntf.listPlugins;
      for j := 0 to engines.Count - 1 do
         loadEngine(engines[j]);
      engines.free;
   end;
   focusControl(trvMapTree);
end;

procedure TfrmTurbuMain.imgLogoAvailable(Sender: TObject);
var
   surface: PSdlSurface;
   convert1, convert2: PSdlSurface;
   rw: PSDL_RWops;
   stream: TResourceStream;
   texture: TSdlTexture;
begin
   stream := TResourceStream.Create(HInstance, 'logo', RT_RCDATA);
   rw := SDLStreamSetup(stream);
   surface := pointer(IMG_LoadPNG_RW(rw));
   assert(assigned(surface));
   convert1 := TSdlSurface.Create(1, 1, 32);
   convert2 := TSdlSurface.Convert(surface, convert1.Format);

   SDL_SelectRenderer(imgLogo.SdlWindow);
   texture := tsdlTexture.Create(0, convert2);
   SDLStreamCloseRWops(rw);
   stream.Free;
   surface.Free;
   convert1.Free;

   imgLogo.DrawTexture(texture);
end;

procedure TfrmTurbuMain.loadEngine(data: TEngineData);
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

procedure TfrmTurbuMain.loadProject;
var
   mapStream: TStream;
   tileset: TTileset;
const
   SDL_BLACK: SDL_Color = ();
begin
   if GDatabase = nil then
      GDatabase := TRpgDatabase.Create;
   frmDatabase.init(GDatabase);
   FMapEngine := retrieveEngine(et_map, GDatabase.mapTree[GDatabase.mapTree.CurrentMap].mapEngine,
                 TVersion.Create(0, 0, 0)) as IDesignMapEngine;
   FMapEngine.initialize(imgLogo.sdlWindow, GDatabase);
   mapStream := GArchives[MAP_ARCHIVE].getFile(GDatabase.mapTree[GDatabase.mapTree.CurrentMap].internalFilename.name);
   try
      FCurrentMap := TRpgMap.Load(mapStream);
   finally
      mapStream.Free;
   end;
   FMapEngine.loadMap(FCurrentMap);
   self.configureScrollBars(FMapEngine.mapSize, FMapEngine.mapPosition);

   tileset := GDatabase.tileset[FCurrentMap.tileset];
   //I don't know why this is necessary, but without the next two lines,
   //nothing will ever actually show in imgPalette;
   imgPalette.fillColor(SDL_BLACK, 255);
   imgPalette.Flip;

   assignPaletteImage(FMapEngine.tilesetImage[0]);
end;

procedure TfrmTurbuMain.mnu2KClick(Sender: TObject);
begin
   closeProject;
   if frmRmConverter.loadProject = SUCCESSFUL_IMPORT then
   begin
      frmDatabase.reset;
      self.loadProject;
      mnuDatabase.Enabled := true;
   end;
end;

procedure TfrmTurbuMain.mnuDatabaseClick(Sender: TObject);
var
   dataset: TClientDataSet;
begin
   for dataset in dmDatabase.datasets do
      if dataset.Active = false then
         dataset.CreateDataSet;
   frmDatabase.ShowModal;
end;

procedure TfrmTurbuMain.mnuExitClick(Sender: TObject);
begin
   self.Close;
end;

procedure TfrmTurbuMain.mnuOpenClick(Sender: TObject);
var
   filename: string;
begin
   dlgOpen.InitialDir := getProjectFolder;
   if dlgOpen.Execute then
   begin
      filename := ExcludeTrailingPathDelimiter(ExtractFilePath(dlgOpen.FileName));
      closeProject;
      openProject(filename);
      mnuDatabase.Enabled := true;
   end;
end;

procedure TfrmTurbuMain.mnuSkillEditClick(Sender: TObject);
begin
   frmSkillLearning.Show;
end;

procedure TfrmTurbuMain.OnScrollMap(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: Integer);
begin
   fMapEngine.ScrollMap(sgPoint(sbHoriz.Position, sbVert.Position));
end;

procedure TfrmTurbuMain.openProject(location: string);
var
   filename: string;
   loadStream: TStream;
begin
   filename := location + DIRMARK + PROJECT_DB;
   assert(GArchives.Add(openFolder(filename)) = DATABASE_ARCHIVE);
   filename := location + DIRMARK + MAP_DB;
   assert(GArchives.Add(openFolder(filename)) = MAP_ARCHIVE);
   filename := location + DIRMARK + IMAGE_DB;
   assert(GArchives.Add(openFolder(filename)) = IMAGE_ARCHIVE);
   loadStream := TFileStream.Create(IncludeTrailingPathDelimiter(location) + DBNAME, fmOpenRead);
   frmDatabase.reset;

   try
      try
         GDatabase := TRpgDatabase.Load(loadStream);
      except
         on ERpgLoadError do
         begin
            GDatabase := nil;
            MsgBox('TDB file is using an outdated format and can''t be loaded.', 'Load error');
            Abort;
         end
         else begin
            GDatabase := nil;
            raise;
         end;
      end;
   finally
      loadStream.free;
   end;
   loadProject;
end;

procedure TfrmTurbuMain.sbPaletteScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
   if FPaletteTexture <> -1 then
      displayPalette(ScrollPos);
end;

procedure TfrmTurbuMain.splSidebarMoved(Sender: TObject);
begin
   resizePalette;
end;

initialization
finalization
   frmTurbuMain := nil;
end.