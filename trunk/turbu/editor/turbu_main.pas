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
   SysUtils, Classes, Controls, Forms, Menus, Contnrs, Graphics, ExtCtrls, StdCtrls,
   design_script_engine, Dialogs, JvComponentBase, JvPluginManager,
   turbu_plugin_interface, turbu_engines, turbu_map_engine,
   sdl_frame;

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
      imgLogo: TSdlFrame;
      procedure mnu2KClick(Sender: TObject);
      procedure mnuSkillEditClick(Sender: TObject);
      procedure FormShow(Sender: TObject);
      procedure mnuDatabaseClick(Sender: TObject);
      procedure FormClose(Sender: TObject; var Action: TCloseAction);
      procedure mnuExitClick(Sender: TObject);
      procedure mnuOpenClick(Sender: TObject);
      procedure FormDestroy(Sender: TObject);
      procedure FormCreate(Sender: TObject);
      procedure imgLogoAvailable(Sender: TObject);
   private
      FMapEngine: IMapEngine;
      procedure loadEngine(data: TEngineData);
      procedure loadProject;
      procedure openProject(location: string);
      procedure closeProject;
   public
      { Public declarations }
   end;

var
  frmTurbuMain: TfrmTurbuMain;

implementation

uses
   types, DBClient,
   commons, rm_converter, skill_settings, turbu_database, archiveInterface,
   turbu_constants, turbu_characters, database, turbu_battle_engine, turbu_classes,
   dm_database, discInterface, formats, strtok,
   sdl_13, sdl, sdl_image, sdlstreams;

{$R *.dfm}

resourcestring
   FILTER_TEXT = 'TURBU Projects (project.tdb)|project.tdb';

procedure TfrmTurbuMain.closeProject;
begin
   //check for modifications first
   FreeAndNil(GDatabase);
   GArchives.clearFrom(1);
end;

procedure TfrmTurbuMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   assert(true);
end;

procedure TfrmTurbuMain.FormCreate(Sender: TObject);
begin
   if getProjectFolder = '' then
      createProjectFolder;
end;


procedure TfrmTurbuMain.FormDestroy(Sender: TObject);
begin
   cleanupEngines;
end;

procedure TfrmTurbuMain.FormShow(Sender: TObject);
var
   infile: TStream;
   plugins: TStringList;
   engines: TObjectList;
   plugStr: string;
   i, j: integer;
   pluginIntf: ITurbuPluginInterface;
begin
   GProjectFolder := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
   assert(GArchives.Add(openFolder(GProjectFolder + DESIGN_DB)) = BASE_ARCHIVE);
   dlgOpen.Filter := FILTER_TEXT;
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
      assert(pluginManager.Plugins[i].GetInterface(ITurbuPluginInterface, pluginIntf));
      engines := pluginIntf.listPlugins;
      for j := 0 to engines.Count - 1 do
         loadEngine(TEngineData(engines[j]));
      engines.free;
   end;
end;

procedure TfrmTurbuMain.imgLogoAvailable(Sender: TObject);
var
   surface: PSdlSurface;
   rw: PSDL_RWops;
   stream: TResourceStream;
   texture: TSdlTexture;
begin
   stream := TResourceStream.Create(HInstance, 'logo', RT_RCDATA);
   rw := SDLStreamSetup(stream);
   surface := pointer(IMG_LoadPNG_RW(rw));
   assert(assigned(surface));
   texture := tsdlTexture.Create(0, surface);
   SDLStreamCloseRWops(rw);
   stream.Free;
   surface.Free;

   imgLogo.DrawTexture(texture);
   imgLogo.Flip;
end;

procedure TfrmTurbuMain.loadEngine(data: TEngineData);
var
   iClass: TRpgPlugBase;
   engine: IBattleEngine;
begin
   iClass := data.engine.Create;
   case data.style of
      et_map: assert(false);
      et_battle:
      begin
         assert(iclass.GetInterface(IBattleEngine, engine));
         addEngine(et_battle, engine.getData);
      end;
      et_menu: assert(false);
      et_minigame: assert(false);
      else assert(false);
   end;
end;

procedure TfrmTurbuMain.loadProject;
begin
   if GDatabase = nil then
      GDatabase := TRpgDatabase.Create;
   frmDatabase.init(GDatabase);
   //do more
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
      if not assigned(FMapEngine) then
      begin

      end;
   end;
end;

procedure TfrmTurbuMain.mnuSkillEditClick(Sender: TObject);
begin
   frmSkillLearning.Show;
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
         MsgBox('Project was loaded successfully.', 'Load complete!');
      except
         on ERpgLoadError do
         begin
            GDatabase := nil;
            MsgBox('TDB file is using an outdated format and can''t be loaded.', 'Load error')
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

end.
