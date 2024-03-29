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
   formats, StdCtrls, Mask, JvExMask, JvToolEdit, ExtCtrls;

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
      procedure FormShow(Sender: TObject);
      procedure tmrValidateTimer(Sender: TObject);
      procedure btnConvertClick(Sender: TObject);
      procedure dirProjectLocationClick(Sender: TObject);
      procedure FormCreate(Sender: TObject);
   private
      FFormat: TProjectFormat;
      FDbName: string;
      function validateDirectories: boolean; inline;
      procedure grabFormat;
      procedure setFormat(const Value: TProjectFormat);
   public
      function loadProject: integer;
      property format: TProjectFormat read FFormat write setFormat;
      property dbName: string read FDbName;
   end;

const
   SUCCESSFUL_IMPORT = 6100;
implementation

uses
   windows, inifiles, strUtils, Generics.Collections, IOUtils,
   commons, fileIO, rm2_turbu_converter_thread, project_folder,
   conversion_report_form, conversion_report,
   turbu_database, turbu_characters, turbu_constants, locate_files,
   discInterface,
   archiveInterface, logs,
   sdl;

{$R *.dfm}

resourcestring
   PROJECT_EXISTS = 'The destination folder already exists.  Continuing ' +
    'conversion will overwrite the target folder and delete everything in it. Continue?';

{ TfrmRmConverter }

procedure TfrmRmConverter.btnConvertClick(Sender: TObject);

   procedure openArchive(folderName: string; index: integer);
   var
      filename: string;
   begin
      filename := IncludeTrailingPathDelimiter(dirOutput.text) + folderName;
      assert(GArchives.Add(newFolder(filename)) = index);
   end;

var
   filename: string;
begin
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
      openArchive(MAP_DB, MAP_ARCHIVE);
      openArchive(IMAGE_DB, IMAGE_ARCHIVE);
      openArchive(SCRIPT_DB, SCRIPT_ARCHIVE);
      openArchive(MUSIC_DB, MUSIC_ARCHIVE);
      openArchive(SFX_DB, SFX_ARCHIVE);

      GCurrentFolder := dirProjectLocation.Text;
      GProjectFolder := dirOutput.Text;
      logs.closeLog;

      frmConversionReport := TfrmConversionReport.Create(nil);
      try
         frmConversionReport.thread := TConverterThread.Create(frmConversionReport, dirProjectLocation.Text, dirOutput.Text, FFormat);
         case frmConversionReport.ShowModal of
            mrOk:
            begin
               FDbName := IncludeTrailingPathDelimiter(dirOutput.Directory) + 'turbu.tdb';
               assert(TFile.Exists(FDbName));
               self.ModalResult := SUCCESSFUL_IMPORT;
            end;
            else ;
         end;
      finally
         frmConversionReport.Free;
      end;
   finally
      tmrValidate.Enabled := self.ModalResult = mrNone;
      GArchives.clearFrom(1);
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

procedure TfrmRmConverter.FormShow(Sender: TObject);
begin
   assert(fsModal in FFormState);
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
   dummy := ExtractFileName(ExcludeTrailingPathDelimiter(dirProjectLocation.Text));
   dirOutput.Text := getProjectFolder + dummy;
end;

end.
