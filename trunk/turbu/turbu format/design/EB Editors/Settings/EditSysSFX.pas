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

unit EditSysSFX;

interface

uses
  Forms, Controls, StdCtrls, Classes, ExtCtrls,
  EventBuilder, EbEdit, button_edit, sound_edit;

type
   [EditorCategory('Settings', 'Change System SFX')]
   [EditorContext('RM2K')]
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
      selSFX.setup(GArchives[SFX_ARCHIVE].root, false, sfx);
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
   selSFX.setup(GArchives[SFX_ARCHIVE].root, false, sfx);
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
