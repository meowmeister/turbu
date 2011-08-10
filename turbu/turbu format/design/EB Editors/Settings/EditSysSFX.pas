unit EditSysSFX;

interface

uses
  Forms, Controls, StdCtrls, Classes, ExtCtrls,
  EventBuilder, EbEdit, button_edit, sound_edit;

type
   [EditorCategory('Settings', 'Change System SFX')]
   TfrmEditSysSFX = class(TfrmEbEditBase)
      StaticText1: TStaticText;
      cboWhich: TComboBox;
      selSFX: TSoundEdit;
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

{ TfrmEditSysSFX }

function TfrmEditSysSFX.NewClassType: TEbClass;
begin
   result := TEBSysSFX;
end;

procedure TfrmEditSysSFX.UploadObject(obj: TEbObject);
var
   sfx: TRpgSound;
begin
   cboWhich.ItemIndex := obj.Values[0];
   sfx := TRpgSound.Create;
   try
      sfx.filename := obj.Text;
      sfx.fadeIn := 0;
      sfx.volume := obj.Values[1];
      sfx.tempo := obj.Values[2];
      sfx.balance := obj.Values[3];
      selSFX.setup(GArchives[SFX_ARCHIVE], sfx);
   except
      sfx.Free;
      raise;
   end;
end;

procedure TfrmEditSysSFX.cboWhichChange(Sender: TObject);
var
   sfx: TRpgSound;
begin
   dmDatabase.SysSound.Locate('id;isMusic', VarArrayOf([cboWhich.ItemIndex, false]), []);
   sfx := TRpgSound.Create;
   sfx.download(GDatabase.serializer, dmDatabase.SysSound);
   selSFX.setup(GArchives[SFX_ARCHIVE], sfx);
end;

procedure TfrmEditSysSFX.DownloadObject(obj: TEbObject);
var
   sfx: TRpgSound;
begin
   obj.Clear;
   obj.Values.Add(cboWhich.ItemIndex);
   sfx := selSFX.sound as TRpgSound;
   obj.Text := sfx.filename;
   obj.Values.Add(sfx.fadeIn);
   obj.Values.Add(sfx.volume);
   obj.Values.Add(sfx.tempo);
   obj.Values.Add(sfx.balance);
end;

initialization
   RegisterEbEditor(TEBSysSFX, TfrmEditSysSFX);
finalization
   UnregisterEbEditor(TEBSysSFX);
end.
