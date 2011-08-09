unit EditVehicleBGM;

interface

uses
  Forms, StdCtrls, Controls, Classes, ExtCtrls, DB, DBClient, DBCtrls, DBIndexComboBox,
  EventBuilder, EbEdit, button_edit, sound_edit;

type
   [EditorCategory('Settings', 'Change Vehicle BGM')]
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
   cdsVehicles.CloneCursor(dmDatabase.vehicles, false, true);
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
      selMusic.setup(GArchives[MUSIC_ARCHIVE], music);
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
   selMusic.setup(GArchives[MUSIC_ARCHIVE], music);
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
