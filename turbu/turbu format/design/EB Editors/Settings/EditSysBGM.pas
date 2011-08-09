unit EditSysBGM;

interface

uses
  Forms, Controls, StdCtrls, Classes, ExtCtrls,
  EventBuilder, EbEdit, button_edit, sound_edit;

type
   [EditorCategory('Settings', 'Change System BGM')]
   TfrmEditSysBGM = class(TfrmEbEditBase)
      StaticText1: TStaticText;
      cboWhich: TComboBox;
      selMusic: TSoundEdit;
      procedure cboWhichChange(Sender: TObject);
   protected
      procedure UploadObject(obj: TEbObject); override;
      procedure DownloadObject(obj: TEbObject); override;
      function NewClassType: TEbClass; override;
   end;

implementation
uses
   Variants,
   ArchiveInterface, EB_Settings, turbu_sounds, dm_database, turbu_database;

{$R *.dfm}

{ TfrmEBEditBase1 }

function TfrmEditSysBGM.NewClassType: TEbClass;
begin
   result := TEBSysBGM;
end;

procedure TfrmEditSysBGM.UploadObject(obj: TEbObject);
var
   music: TRpgMusic;
begin
   cboWhich.ItemIndex := obj.Values[0];
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

procedure TfrmEditSysBGM.cboWhichChange(Sender: TObject);
var
   music: TRpgMusic;
begin
   dmDatabase.SysSound.Locate('id;isMusic', VarArrayOf([cboWhich.ItemIndex, true]), []);
   music := TRpgMusic.Create;
   music.download(GDatabase.serializer, dmDatabase.SysSound);
   selMusic.setup(GArchives[MUSIC_ARCHIVE], music);
end;

procedure TfrmEditSysBGM.DownloadObject(obj: TEbObject);
var
   music: TRpgMusic;
begin
   obj.Clear;
   obj.Values.Add(cboWhich.ItemIndex);
   music := selMusic.sound as TRpgMusic;
   obj.Text := music.filename;
   obj.Values.Add(music.fadeIn);
   obj.Values.Add(music.volume);
   obj.Values.Add(music.tempo);
   obj.Values.Add(music.balance);
end;

initialization
   RegisterEbEditor(TEBSysBGM, TfrmEditSysBGM);
finalization
   UnregisterEbEditor(TEBSysBGM);
end.
