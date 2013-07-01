{ *****************************************************************************
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
  ***************************************************************************** }

unit EditHeroName;

interface

uses
  StdCtrls, DBCtrls, Classes, Controls, ExtCtrls, DB,
  ebEdit, EventBuilder, dm_Database, IDLookupCombo;

type
   [EditorCategory('Characters', 'Change Hero Name')]
   [EditorContext('RM2K')]
   TfrmEBEditHeroName = class(TfrmEbEditBase)
      cboHero: TIDLookupCombo;
      lblNewName: TStaticText;
      srcHero: TDataSource;
      txtName: TEdit;
   protected
      procedure UploadObject(obj: TEbObject); override;
      procedure DownloadObject(obj: TEbObject); override;
      function NewClassType: TEbClass; override;
   end;

   [EditorCategory('Characters', 'Change Hero Title')]
   TFrmEBEditHeroTitle = class(TfrmEBEditHeroName)
      procedure FormShow(Sender: TObject);
   protected
      function NewClassType: TEbClass; override;
   end;

implementation
uses
   EB_Characters;

{$R *.dfm}

{ TfrmEBEditHeroName }

procedure TfrmEBEditHeroName.UploadObject(obj: TEbObject);
begin
   cboHero.id := obj.Values[0];
   txtName.Text := obj.Text;
end;

procedure TfrmEBEditHeroName.DownloadObject(obj: TEbObject);
begin
   obj.Clear;
   obj.Values.Add(cboHero.id);
   obj.Text := txtName.Text;
end;

function TfrmEBEditHeroName.NewClassType: TEbClass;
begin
   result := TEBHeroName;
end;

{ TFrmEBEditHeroTitle }

procedure TFrmEBEditHeroTitle.FormShow(Sender: TObject);
begin
   inherited FormShow(Sender);
   self.Caption := 'Change Hero Title';
   lblNewName.Caption := 'New Title';
end;

function TFrmEBEditHeroTitle.NewClassType: TEbClass;
begin
   result := TEBHeroTitle;
end;

initialization
   RegisterEbEditor(TEBHeroName, TfrmEBEditHeroName);
   RegisterEbEditor(TEBHeroTitle, TfrmEBEditHeroTitle);
finalization
   UnRegisterEbEditor(TEBHeroName);
   UnRegisterEbEditor(TEBHeroTitle);
end.
