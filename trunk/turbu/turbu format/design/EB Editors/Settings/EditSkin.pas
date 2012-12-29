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

unit EditSkin;

interface

uses
   Forms, StdCtrls, ExtCtrls, Classes, Controls,
   EventBuilder, EbEdit, imageSelectorFrame;

type
   [EditorCategory('Settings', 'Change System Skin')]
   [EditorContext('RM2K')]
   TfrmSkinSelector = class(TfrmEbEditBase)
      frameImageSelector: TframeImageSelector;
      Panel2: TPanel;
      radStyle: TRadioGroup;
      radFont: TRadioGroup;
      procedure FormCreate(Sender: TObject);
   protected
      procedure UploadObject(obj: TEbObject); override;
      procedure DownloadObject(obj: TEbObject); override;
      function NewClassType: TEbClass; override;
   end;

implementation
uses
   archiveInterface, EB_Settings;

{$R *.dfm}

procedure TfrmSkinSelector.FormCreate(Sender: TObject);
begin
   frameImageSelector.Setup(GArchives[IMAGE_ARCHIVE], 'System');
end;

function TfrmSkinSelector.NewClassType: TEbClass;
begin
   result := TEBSysSkin;
end;

procedure TfrmSkinSelector.UploadObject(obj: TEbObject);
begin
   frameImageSelector.Selection := obj.Text;
   radStyle.ItemIndex := obj.Values[0];
   radFont.ItemIndex := obj.Values[1];
end;

procedure TfrmSkinSelector.DownloadObject(obj: TEbObject);
begin
   obj.Clear;
   obj.Text := frameImageSelector.Selection;
   obj.Values.Add(radStyle.ItemIndex);
   obj.Values.Add(radFont.ItemIndex);
end;

initialization
   RegisterEbEditor(TEBSysSkin, TfrmSkinSelector);
finalization
   UnregisterEbEditor(TEBSysSkin);
end.
