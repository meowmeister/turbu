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
  SysUtils, Classes, Controls, Forms, Menus, StdCtrls, DBCtrls;

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
    procedure mnuTestDatasetsClick(Sender: TObject);
    procedure mnuTestLoadingClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure mnuTestDatabasewindow1Click(Sender: TObject);
    procedure mnuTestConversion1Click(Sender: TObject);
    procedure mnuSetDefaultProjectClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    folder, outFolder: string;

    procedure DoNothing;
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
   turbu_constants,
   commons, fileIO, formats, LDB, LMT, LMU,
   rm2_turbu_characters, rm2_turbu_database, turbu_unit_dictionary,
   turbu_engines, turbu_plugin_interface, turbu_battle_engine,
   turbu_2k3_battle_engine, turbu_2k_battle_engine,
   strtok;

{$R *.dfm}

var
   freeList: TObjectList;

procedure TfrmTestConsole.mnuTestDatabasewindow1Click(Sender: TObject);
begin
   if not dmDatabase.charClasses.Active then
      mnuTestDatasetsClick(sender);
   if not assigned(GDatabase) then
      mnuTestLoadingClick(Sender);
   frmDatabase.init(GDatabase);
   frmDatabase.ShowModal;
end;

procedure TfrmTestConsole.DoNothing;
begin
   assert(true);
end;

procedure TfrmTestConsole.mnuSetDefaultProjectClick(Sender: TObject);
begin
   frmTestProjLocation.getLocation(folder);
   mnuTestConversion1.Enabled := folder <> '';
end;

procedure TfrmTestConsole.mnuTestConversion1Click(Sender: TObject);
var
   database, mapTree, mapUnit: TFileStream;
   outFile: TFileStream;
   savefile: TMemoryStream;
   filename, uFilename: string;
   stream: TStream;
   dic: TUnitDictionary;

   FLdb: TLcfDataBase;
   FLmt: TFullTree;
   FLmu: TMapUnit;
begin
   database := nil;
   mapTree := nil;
   mapUnit := nil;
   FLmt := nil;
   FLdb := nil;
   FLmu := nil;

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

      database := openFile(folder + '\RPG_RT.ldb');
      if getString(database) <> 'LcfDataBase' then
         raise EParseMessage.create('RPG_RT.LDB is corrupt!');
      GProjectFormat := scanRmFormat(database);
      assert(GProjectFormat = pf_2k);
      FLdb := TLcfDataBase.Create(database);
      //end of format and database check

      maptree := openFile(folder + '\RPG_RT.lmt');
      if getString(maptree) <> 'LcfMapTree' then
         raise EParseMessage.create('RPG_RT.LMT is corrupt!');
      FLmt := TFullTree.Create(maptree);
      //done checking the map tree; now let's do some converting

      GDatabase.Free;
      dic := TUnitDictionary.Create([doOwnsValues], 10);
      for filename in GArchives[0].allFiles('scripts\general\') do
      begin
         stream := GArchives[0].getFile(filename);
         uFilename := StringReplace(filename, 'scripts\general\', '', []);
         uFilename := StringReplace(uFilename, '.trs', '', []);
         try
            dic.Add(uFilename, TStringList.Create);
            dic[uFilename].LoadFromStream(stream);
         finally
            stream.Free;
         end;
      end;

      try
         GDatabase := TRpgDatabase.convert(FLdb, dic, self.DoNothing);
      except
         on E: EMissingPlugin do
         begin
            MsgBox(E.Message, 'Missing plugin');
            FreeAndNil(GDatabase);
            Exit;
         end;
      end;
      savefile := TMemoryStream.Create;
      try
         GDatabase.save(savefile);
         filename := IncludeTrailingPathDelimiter(outFolder) + DBNAME;
         if FileExists(filename) then
            deleteFile(PChar(filename));
         outFile := TFileStream.Create(filename, fmCreate);
         try
            outfile.CopyFrom(savefile, 0);
         finally
            outFile.Free;
         end;
      finally
         savefile.free;
      end;
      Application.MessageBox('Test concluded successfully!', 'Finished.')
   finally
      database.free;
      mapTree.free;
      mapUnit.free;
      FLmt.Free;
      FLdb.Free;
      FLmu.Free;
   end;
end;

procedure TfrmTestConsole.FormCreate(Sender: TObject);
begin
   if getProjectFolder = '' then
      createProjectFolder;
end;

procedure TfrmTestConsole.FormShow(Sender: TObject);
type
   TBattleEngineClass = class of TBattleEngine;

   {$WARN CONSTRUCTING_ABSTRACT OFF}
   procedure addBattleEngine(aClass: TBattleEngineClass);
   var
      engine: TBattleEngine;
   begin
      engine := aClass.Create;
      addEngine(et_battle, engine.getData);
      freeList.Add(engine);
      freeList.Add(engine.getData);
   end;
   {$WARN CONSTRUCTING_ABSTRACT ON}

var
   dummy: string;
begin
   GProjectFolder := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
   assert(GArchives.Add(openFolder(GProjectFolder + DESIGN_DB)) = BASE_ARCHIVE);
   addBattleEngine(T2kBattleEngine);
   addBattleEngine(T2k3BattleEngine);
   folder := IncludeTrailingPathDelimiter(GetRegistryValue('\Software\TURBU', 'TURBU Test Project'));
   if folder <> '' then
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

initialization
   freeList := TObjectList.Create(true);

finalization
   freeList.Free;

end.
