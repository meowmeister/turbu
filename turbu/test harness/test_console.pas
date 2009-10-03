unit test_console;
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
  SysUtils, Classes, Controls, Forms, Menus, StdCtrls, DBCtrls, Dialogs;

type
   TfrmTestConsole = class(TForm)
      MainMenu1: TMainMenu;
      Database1: TMenuItem;
      mnuTestDatasets: TMenuItem;
      mnuTestLoading: TMenuItem;
      mnuTestDatabasewindow1: TMenuItem;
      File1: TMenuItem;
      mnuTestConversion1: TMenuItem;
      mnuSetDefaultProject: TMenuItem;
      Graphics1: TMenuItem;
      mnuCreateSdlWindow: TMenuItem;
      mnuTestSDL: TMenuItem;
      mnuTestMapLoading: TMenuItem;
      procedure mnuTestDatasetsClick(Sender: TObject);
      procedure mnuTestLoadingClick(Sender: TObject);
      procedure FormShow(Sender: TObject);
      procedure mnuTestDatabasewindow1Click(Sender: TObject);
      procedure mnuTestConversion1Click(Sender: TObject);
      procedure mnuSetDefaultProjectClick(Sender: TObject);
      procedure FormCreate(Sender: TObject);
      procedure mnuCreateSdlWindowClick(Sender: TObject);
      procedure FormDestroy(Sender: TObject);
      procedure mnuTestSDLClick(Sender: TObject);
      procedure mnuTestMapLoadingClick(Sender: TObject);
   private
      { Private declarations }
      folder, outFolder: string;

   public
      { Public declarations }
   end;

var
  frmTestConsole: TfrmTestConsole;

implementation

uses
   Contnrs, DBClient, Generics.Collections, Registry, windows,
   dm_database, turbu_database, database, test_project,
   archiveInterface, discInterface,
   turbu_constants, conversion_report, conversion_report_form,
   rm2_turbu_converter_thread, design_script_engine,
   commons, fileIO, formats, LDB, LMT, LMU,
   turbu_characters, locate_files,
   rm2_turbu_characters, rm2_turbu_database, turbu_unit_dictionary,
   turbu_engines, turbu_plugin_interface, turbu_battle_engine,
   turbu_2k3_battle_engine, turbu_2k_battle_engine, turbu_sprites,
   turbu_map_engine, turbu_2k_map_engine, turbu_maps,
   turbu_tbi_lib, turbu_sdl_image, turbu_map_interface,
   sdl_canvas, sdl_13,
   strtok;

{$R *.dfm}

var
   freeList: TObjectList;
   lCanvas: TSdlCanvas;

procedure TfrmTestConsole.mnuTestDatabasewindow1Click(Sender: TObject);
begin
   if not dmDatabase.charClasses.Active then
      mnuTestDatasetsClick(sender);
   if not assigned(GDatabase) then
      mnuTestLoadingClick(Sender);
   frmDatabase.init(GDatabase);
   frmDatabase.ShowModal;
end;

procedure TfrmTestConsole.mnuCreateSdlWindowClick(Sender: TObject);
begin
   if not assigned(lCanvas) then
      lCanvas := TSdlCanvas.Create('TURBU testing canvas', rect(400, 400, 320, 240), [{sdlwOpenGl,} sdlwShown]);
   if (sender = mnuCreateSdlWindow) and (assigned(lCanvas)) then
      Application.MessageBox('Test concluded successfully!', 'Finished.')
end;

procedure TfrmTestConsole.mnuSetDefaultProjectClick(Sender: TObject);
begin
   frmTestProjLocation.getLocation(folder);
   mnuTestConversion1.Enabled := folder <> '';
end;

procedure TfrmTestConsole.mnuTestConversion1Click(Sender: TObject);
var
   filename: string;
   conversionReport: IConversionReport;
begin
   try
      filename := IncludeTrailingPathDelimiter(outFolder);
      assert(DirectoryExists(filename));
      GArchives.clearFrom(1);
      filename := IncludeTrailingPathDelimiter(outFolder) + PROJECT_DB;
      assert(GArchives.Add(newFolder(filename)) = DATABASE_ARCHIVE);
      filename := IncludeTrailingPathDelimiter(outFolder) + MAP_DB;
      assert(GArchives.Add(newFolder(filename)) = MAP_ARCHIVE);
      filename := IncludeTrailingPathDelimiter(outFolder) + IMAGE_DB;
      assert(GArchives.Add(newFolder(filename)) = IMAGE_ARCHIVE);

      filename := IncludeTrailingPathDelimiter(folder);

      GCurrentFolder := folder;
      GProjectFolder := outFolder;
      rtpLocation := GetRegistryValue('\Software\ASCII\RPG2000', 'RuntimePackagePath');

      turbu_characters.SetScriptEngine(GDScriptEngine);
      frmConversionReport := TfrmConversionReport.Create(Application);
      ConversionReport := frmConversionReport;
      frmConversionReport.thread := TConverterThread.Create(conversionReport, folder, outFolder, pf_2k);
      case frmConversionReport.ShowModal of
         mrOk:;
         else ;
      end;
   finally

   end;
end;

procedure TfrmTestConsole.mnuTestMapLoadingClick(Sender: TObject);
var
   engine: IDesignMapEngine;
   map: IRpgMap;
   mapStream: TStream;
begin
   if not assigned(GDatabase) then
      mnuTestLoadingClick(Sender);

   engine := T2kMapEngine.Create;
//   freeList.add(engine.data);
   engine.initialize(0, gdatabase);
   mapStream := GArchives[MAP_ARCHIVE].getFile(GDatabase.mapTree[1].internalFilename.name);
   try
      map := TRpgMap.Load(mapStream);
   finally
      mapStream.Free;
   end;
   engine.loadMap(map, point(0,0));

   if sender = mnuTestMapLoading then
      Application.MessageBox('Test concluded successfully!', 'Finished.')
end;

procedure TfrmTestConsole.FormCreate(Sender: TObject);
begin
   if getProjectFolder = '' then
      createProjectFolder;
end;

procedure TfrmTestConsole.FormDestroy(Sender: TObject);
begin
   lCanvas.Free;
end;

procedure TfrmTestConsole.FormShow(Sender: TObject);
type
   TBattleEngineClass = class of TBattleEngine;
   TMapEngineClass = class of TMapEngine;

   {$WARN CONSTRUCTING_ABSTRACT OFF}
   procedure addBattleEngine(aClass: TBattleEngineClass);
   var
      engine: TBattleEngine;
   begin
      engine := aClass.Create;
      addEngine(et_battle, engine.data, engine);
//      freeList.Add(engine.data);
   end;

   procedure AddMapEngine(aClass: TMapEngineClass);
   var
      engine: TMapEngine;
   begin
      engine := aClass.Create;
      addEngine(et_map, engine.data, engine);
//      freeList.Add(engine.data);
   end;
   {$WARN CONSTRUCTING_ABSTRACT ON}

var
   dummy: string;
begin
   JITEnable := 0;
   GProjectFolder := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
   assert(GArchives.Add(openFolder(GProjectFolder + DESIGN_DB)) = BASE_ARCHIVE);
   addBattleEngine(T2kBattleEngine);
   addBattleEngine(T2k3BattleEngine);
   addMapEngine(T2kMapEngine);
   folder := IncludeTrailingPathDelimiter(GetRegistryValue('\Software\TURBU', 'TURBU Test Project'));
   if folder <> '\' then
   begin
      dummy := ExcludeTrailingPathDelimiter(folder);
      dummy := strtok.getLastToken(dummy, PathDelim);
      outFolder := getProjectFolder + dummy;
   end;
   mnuTestConversion1.Enabled := folder <> '\';
end;

procedure TfrmTestConsole.mnuTestDatasetsClick(Sender: TObject);
var
   dataset: TClientDataSet;
begin
   for dataset in dmDatabase.datasets do
   try
      dataset.Close;
      dataset.CreateDataSet;
//      dataset.Open;
   except
      on E: Exception do
         asm int 3 end;
   end;
   if sender = mnuTestDatasets then
      Application.MessageBox('Test concluded successfully!', 'Finished.')
end;

procedure TfrmTestConsole.mnuTestLoadingClick(Sender: TObject);
var
   location: string;
   filename: string;
   loadStream: TStream;
begin
   try
      location := IncludeTrailingPathDelimiter(GetRegistryValue('\Software\TURBU', 'TURBU Test Project Output'));
      GArchives.clearFrom(1);
      filename := location + PROJECT_DB;
      assert(GArchives.Add(openFolder(filename)) = DATABASE_ARCHIVE);
      filename := location + MAP_DB;
      assert(GArchives.Add(openFolder(filename)) = MAP_ARCHIVE);
      filename := location + IMAGE_DB;
      assert(GArchives.Add(openFolder(filename)) = IMAGE_ARCHIVE);
      loadStream := TFileStream.Create(IncludeTrailingPathDelimiter(location) + DBNAME, fmOpenRead);

      turbu_characters.SetScriptEngine(GDScriptEngine);
      try
         freeAndNil(GDatabase);
         try
            GDatabase := TRpgDatabase.Load(loadStream);
         except
            GDatabase := nil;
            raise;
         end;
      finally
         loadStream.free;
      end;
   except
      on E: Exception do
         asm int 3 end;
   end;
   if sender = mnuTestLoading then
      Application.MessageBox('Test concluded successfully!', 'Finished.')
end;

procedure TfrmTestConsole.mnuTestSDLClick(Sender: TObject);
var
   filename: string;
   spriteData: TSpriteData;
   filestream: TStream;
   image: TRpgSdlImage;
begin
   if not assigned(GDatabase) then
      mnuTestLoadingClick(Sender);
   if not assigned(lCanvas) then
      mnuCreateSdlWindowClick(Sender);

   filename := GDatabase.spriteList[GDatabase.charClass[1].portrait];
   spriteData := extractSpriteData(filename);
   filename := spriteData.name + '.tbi';
   fileStream := GArchives[IMAGE_ARCHIVE].getFile(filename);
   try
      image := TRpgSdlImage.CreateSprite(loadFromTBI(fileStream), filename, nil, SPRITE_SIZE);
   finally
      fileStream.Free;
   end;
   lCanvas.Draw(image, point(0, 0));
   SDL_RenderPresent;
   image.Free;
   if sender = mnuTestSdl then
      Application.MessageBox('Test concluded successfully!', 'Finished.')
end;

initialization
   freeList := TObjectList.Create(true);

finalization
   freeList.Free;

end.
