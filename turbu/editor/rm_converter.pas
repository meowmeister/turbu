unit rm_converter;
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
   SysUtils, Classes, Controls, Forms, Dialogs, ComCtrls,
   JvSelectDirectory, JvBaseDlg,
   formats, StdCtrls, Mask, JvExMask, JvToolEdit, ExtCtrls,
   LDB, LMT, LMU;

type
   TfrmRmConverter = class(TForm)
      pagOptions: TPageControl;
      tshBasic: TTabSheet;
      tshEvents: TTabSheet;
      btnCancel: TButton;
      btnConvert: TButton;
      dirProjectLocation: TJvDirectoryEdit;
      lblLocation: TLabel;
      dirOutput: TJvDirectoryEdit;
      lblConvertLocation: TLabel;
      tmrValidate: TTimer;
      grpEventFormat: TRadioGroup;
      grpFormat: TRadioGroup;
      tshCull: TTabSheet;
      lstCull: TListView;
      lblCullSelect: TLabel;
      btnCullSelectDB: TButton;
      btnCullClear: TButton;
      btnCullSelectGraphics: TButton;
      prgConversion: TProgressBar;
      lblProgress: TLabel;
      procedure FormShow(Sender: TObject);
      procedure tmrValidateTimer(Sender: TObject);
      procedure btnConvertClick(Sender: TObject);
      procedure FormDestroy(Sender: TObject);
      procedure dirProjectLocationClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
   private
      FFormat: TProjectFormat;
      FLdb: TLcfDataBase;
      FLmt: TFullTree;
      FLmu: TMapUnit;
      function validateDirectories: boolean; inline;
      procedure grabFormat;
      procedure setFormat(const Value: TProjectFormat);
   public
      function loadProject: integer;
      property format: TProjectFormat read FFormat write setFormat;
   end;

const
   SUCCESSFUL_IMPORT = 6100;
var
   frmRmConverter: TfrmRmConverter;

implementation

uses
   windows, inifiles, strUtils, Generics.Collections,
   commons, fileIO,
   turbu_database, turbu_characters, turbu_constants, locate_files,
   rm2_turbu_characters, rm2_turbu_database, discInterface,
   archiveInterface, design_script_engine, turbu_engines, logs,
   turbu_unit_dictionary,
   strtok,
   uPSCompiler,
   sdl;

{$R *.dfm}

resourcestring
   PROJECT_EXISTS = 'The destination folder already exists.  Continuing conversion will overwrite the target folder and delete everything in it. Continue?';

{ TfrmRmConverter }

resourcestring
   UNEX_FORMAT_MESSAGE1 = 'The project in this folder appears to be a RPG Maker';
   RM2K_FORMAT = '2000 ';
   RM2K3_FORMAT = '2003 ';
   UNEX_FORMAT_MESSAGE2 = 'project, where a RPG Maker ';
   UNEX_FORMAT_MESSAGE3 = 'project was expected. Press OK to convert the project as an RPG Maker ';
   UNEX_FORMAT_MESSAGE4 = 'project, or Cancel to change the import options.';

procedure TfrmRmConverter.btnConvertClick(Sender: TObject);
var
   database, mapTree, mapUnit: TFileStream;
   outFile: TFileStream;
   savefile: TMemoryStream;
   stream: TStream;
   errorstring: string;
   goodformat, badformat: string;
   filename, uFilename: string;
   dic: TUnitDictionary;
begin
   database := nil;
   mapTree := nil;
   mapUnit := nil;
   if not validateDirectories then
   begin
      btnConvert.Enabled := false;
      Exit;
   end;
   tmrValidate.Enabled := false;
   try
      filename := IncludeTrailingPathDelimiter(dirOutput.text);
      if DirectoryExists(filename) then
      begin
         if MsgBox(PROJECT_EXISTS, 'Overwrite existing project?', MB_YESNO) = IDNO then
            Exit;
      end else if not CreateDir(filename) then
         RaiseLastOSError;
      GArchives.clearFrom(1);
      filename := IncludeTrailingPathDelimiter(dirOutput.text) + PROJECT_DB;
      assert(GArchives.Add(newFolder(filename)) = DATABASE_ARCHIVE);
      filename := IncludeTrailingPathDelimiter(dirOutput.text) + MAP_DB;
      assert(GArchives.Add(newFolder(filename)) = MAP_ARCHIVE);
      filename := IncludeTrailingPathDelimiter(dirOutput.text) + IMAGE_DB;
      assert(GArchives.Add(newFolder(filename)) = IMAGE_ARCHIVE);

      lblProgress.Visible := true;
      prgConversion.Position := 0;
      prgConversion.Visible := true;
      Application.ProcessMessages;
      filename := IncludeTrailingPathDelimiter(dirOutput.text);

      GCurrentFolder := dirProjectLocation.Text;
      GProjectFolder := dirOutput.Text;
      logs.closeLog;
      if fileExists(logName) then
         deleteFile(PChar(logName));

      database := openFile(dirProjectLocation.Text + '\RPG_RT.ldb');
      if getString(database) <> 'LcfDataBase' then
         raise EParseMessage.create('RPG_RT.LDB is corrupt!');
      GProjectFormat := scanRmFormat(database);
      if GProjectFormat <> FFormat then
      begin
         if GProjectFormat = pf_2k then
         begin
            goodformat := RM2K_FORMAT;
            badformat := RM2K3_FORMAT;
         end else
         begin
            badformat := RM2K_FORMAT;
            goodformat := RM2K3_FORMAT;
         end;
         errorstring := UNEX_FORMAT_MESSAGE1 + goodformat + UNEX_FORMAT_MESSAGE2 + badformat + UNEX_FORMAT_MESSAGE3 + goodformat + UNEX_FORMAT_MESSAGE4;
         case msgBox(errorstring, 'Unexpected format found', MB_OKCANCEL) of
            IDOK: FFormat := GProjectFormat;
            IDCANCEL:
            begin
               tmrValidate.Enabled := true;
               Exit;
            end;
         end;
      end;
      FLdb := TLcfDataBase.Create(database);
      prgConversion.StepIt;
      //end of format and database check

      maptree := openFile(dirProjectLocation.Text + '\RPG_RT.lmt');
      if getString(maptree) <> 'LcfMapTree' then
         raise EParseMessage.create('RPG_RT.LMT is corrupt!');
      FLmt := TFullTree.Create(maptree);
      prgConversion.StepIt;
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
         GDatabase := TRpgDatabase.convert(FLdb, dic, prgConversion.StepIt);
      except
         on E: EMissingPlugin do
         begin
            MsgBox(E.Message, 'Missing plugin');
            GDatabase := nil;
            Exit;
         end;
         else begin
            GDatabase := nil;
            raise;
         end;
      end;
      savefile := TMemoryStream.Create;
      try
         GDatabase.save(savefile);
         prgConversion.StepIt;
         filename := IncludeTrailingPathDelimiter(dirOutput.text) + DBNAME;
         if FileExists(filename) then
            deleteFile(PChar(filename));
         outFile := TFileStream.Create(filename, fmCreate);
         try
            outfile.CopyFrom(savefile, 0);
         finally
            outFile.Free;
         end;
         prgConversion.StepIt;
      finally
         savefile.free;
      end;
      MsgBox('No errors found.', 'Conversion complete!');
      self.ModalResult := SUCCESSFUL_IMPORT;
   finally
      database.free;
      mapTree.free;
      mapUnit.free;
      tmrValidate.Enabled := self.ModalResult = mrNone;
      prgConversion.Visible := false;
      lblProgress.Visible := false;
   end;
end;

procedure TfrmRmConverter.dirProjectLocationClick(Sender: TObject);
begin
   grabFormat;
end;

procedure TfrmRmConverter.FormCreate(Sender: TObject);
begin
   dirOutput.Hint := 'Output folder selection is unavailable.  The project folder''s name and location' + LFCR +
                     'will be generated automatically based on the original project folder''s name.';
end;

procedure TfrmRmConverter.FormDestroy(Sender: TObject);
begin
   FLdb.Free;
   FLmt.Free;
   FLmu.free;
end;

procedure TfrmRmConverter.FormShow(Sender: TObject);
var
   dummy: ansiString;
   stream: TStream;
begin
   assert(fsModal in FFormState);
   freeAndNil(FLdb);
   freeAndNil(FLmt);
   freeAndNil(FLmu);
   self.grabFormat;
   dirProjectLocation.Text := '';
   dirOutput.InitialDir := getPersonalFolder;
   dirOutput.Text := '';

   GDatabase.Free;
   tmrValidate.Enabled := true;
end;

procedure TfrmRmConverter.grabFormat;
begin
   case grpFormat.ItemIndex of
      0: self.format := pf_2k;
      1: self.format := pf_2k3;
   end;
end;

function TfrmRmConverter.loadProject: integer;
begin
   grabFormat;
   result := self.ShowModal;
end;

procedure TfrmRmConverter.setFormat(const Value: TProjectFormat);
var
   ini: TMemIniFile;
   startDir: string;
begin
   if FFormat = value then
      Exit;
      
   FFormat := Value;
   GProjectFormat := value;
   case FFormat of
      pf_2k:
      begin
         ini := TMemIniFile.Create(GetRegistryValue('\Software\ASCII\RPG2000', 'ApplicationPath') + 'RPG2000.ini');
         try
            if ini.FileName = '' then
               startDir := getPersonalFolder
            else startDir := ini.ReadString('RPG2000', 'ProjectBasePath', '');
         finally
            ini.free;
         end;
         rtpLocation := GetRegistryValue('\Software\ASCII\RPG2000', 'RuntimePackagePath')
      end;
      pf_2k3:
      begin
         startDir := GetRegistryValue('\Software\Enterbrain\RPG2003', 'ProjectBasePath');
         if startDir = '' then
            startDir := getPersonalFolder;
         rtpLocation := GetRegistryValue('\Software\Enterbrain\RPG2003', 'RUNTIMEPACKAGEPATH')
      end;
      else assert(false);
   end;
   if dirProjectLocation.text = '' then
      dirProjectLocation.InitialDir := startDir;
end;

procedure TfrmRmConverter.tmrValidateTimer(Sender: TObject);
begin
   grabFormat;
   if (dirProjectLocation.Text = '') then
   begin
      btnConvert.Enabled := false;
      Exit;
   end
   else btnConvert.Enabled := validateDirectories;
end;

function TfrmRmConverter.validateDirectories: boolean;
var
   dummy: string;
begin
   result := false;
   assert(dirProjectLocation.Text <> '');
   if DirectoryExists(dirProjectLocation.Text) then
      result := (FileExists(dirProjectLocation.Text + '\RPG_RT.lmt')) and (FileExists(dirProjectLocation.Text + '\RPG_RT.ldb'));
   dummy := ExcludeTrailingPathDelimiter(dirProjectLocation.Text);
   dummy := strtok.getLastToken(dummy, PathDelim);
   dirOutput.Text := getProjectFolder + dummy;
end;

end.
