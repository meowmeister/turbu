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

unit EditVehicleBGM;

interface

uses
  Forms, StdCtrls, Controls, Classes, ExtCtrls, DB, DBClient, DBCtrls, DBIndexComboBox,
  EventBuilder, EbEdit, button_edit, sound_edit;

type
   [EditorCategory('Settings', 'Change Vehicle BGM')]
   [EditorContext('RM2K')]
   TfrmEditVehicleBGM = class(TfrmEbEditBase)
      StaticText1: TStaticText;
      cboWhich: TComboBox;
      selMusic: TSoundEdit;
      cdsVehicles: TClientDataSet;
      cdsVehiclesid: TIntegerField;
      cdsVehiclesname: TWideStringField;
      cdsVehiclesmusic_id: TIntegerField;
      cdsVehiclesmusic_FadeIn: TIntegerField;
      cdsVehiclesmusic_tempo: TIntegerField;
      cdsVehiclesmusic_volume: TIntegerField;
      cdsVehiclesmusic_balance: TIntegerField;
      cdsVehiclesmusic_name: TWideStringField;
      procedure FormCreate(Sender: TObject);
      procedure cboWhichChange(Sender: TObject);
   protected
      procedure UploadObject(obj: TEbObject); override;
      procedure DownloadObject(obj: TEbObject); override;
      function NewClassType: TEbClass; override;
   end;

implementation
uses
   dm_database, EB_Settings, turbu_sounds, ArchiveInterface;

{$R *.dfm}

procedure TfrmEditVehicleBGM.FormCreate(Sender: TObject);
begin
   //TODO: fix when QC 97406 is fixed
   cdsVehicles.CloneCursor(dmDatabase.vehicles, false);
   cdsVehicles.IndexFieldNames := 'id';

   cdsVehicles.First;
   while not cdsVehicles.Eof do
   begin
      cboWhich.AddItem(cdsVehiclesname.Value, nil);
      cdsVehicles.Next;
   end;
end;

function TfrmEditVehicleBGM.NewClassType: TEbClass;
begin
   result := TEBVehicleBGM;
end;

procedure TfrmEditVehicleBGM.UploadObject(obj: TEbObject);
var
   music: TRpgMusic;
begin
   cboWhich.ItemIndex := obj.Values[0] - 1;
   music := TRpgMusic.Create;
   try
      music.filename := obj.Text;
      music.fadeIn := obj.Values[1];
      music.volume := obj.Values[2];
      music.tempo := obj.Values[3];
      music.balance := obj.Values[4];
      selMusic.setup(GArchives[MUSIC_ARCHIVE].root, true, music);
   except
      music.Free;
      raise;
   end;
end;

procedure TfrmEditVehicleBGM.cboWhichChange(Sender: TObject);
var
   music: TRpgMusic;
begin
   cdsVehicles.Locate('id', cboWhich.ItemIndex + 1, []);
   music := TRpgMusic.Create;
   music.id := cdsVehiclesmusic_id.Value;
   music.name := cdsVehiclesmusic_name.Value;
   music.fadeIn := cdsVehiclesmusic_FadeIn.Value;
   music.tempo := cdsVehiclesmusic_tempo.Value;
   music.balance := cdsVehiclesmusic_balance.Value;
   music.volume := cdsVehiclesmusic_volume.Value;
   selMusic.setup(GArchives[MUSIC_ARCHIVE].root, true, music);
end;

procedure TfrmEditVehicleBGM.DownloadObject(obj: TEbObject);
var
   music: TRpgMusic;
begin
   obj.Clear;
   obj.Values.Add(cboWhich.ItemIndex + 1);
   music := selMusic.sound as TRpgMusic;
   obj.Text := music.filename;
   obj.Values.Add(music.fadeIn);
   obj.Values.Add(music.volume);
   obj.Values.Add(music.tempo);
   obj.Values.Add(music.balance);
end;

initialization
   RegisterEbEditor(TEBVehicleBGM, TfrmEditVehicleBGM);
finalization
   UnregisterEbEditor(TEBVehicleBGM);
end.
