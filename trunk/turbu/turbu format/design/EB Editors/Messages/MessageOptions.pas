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

unit MessageOptions;

interface

uses
   StdCtrls, Classes, Controls, ExtCtrls,
   EbEdit, EventBuilder;

type
   [EditorCategory('Messages', 'Message Options')]
   TfrmMessageOptions = class(TfrmEbEditBase)
      GroupBox3: TGroupBox;
      radVisibility: TRadioGroup;
      radPosition: TRadioGroup;
      chkNoHide: TCheckBox;
      chkBlock: TCheckBox;
   protected
      procedure UploadObject(obj: TEbObject); override;
      procedure DownloadObject(obj: TEbObject); override;
      function NewClassType: TEbClass; override;
   end;

implementation
uses
   EB_Messages;

{$R *.dfm}

{ TfrmMessageOptions }

procedure TfrmMessageOptions.UploadObject(obj: TEbObject);
begin
   assert(obj is TEBMessageOptions);
   radVisibility.ItemIndex := obj.Values[0];
   radPosition.ItemIndex := obj.Values[1];
   chkNoHide.Checked := boolean(obj.Values[2]);
   chkBlock.Checked := boolean(obj.Values[3]);
end;

procedure TfrmMessageOptions.DownloadObject(obj: TEbObject);
begin
   obj.Values.Clear;
   obj.Values.Add(radVisibility.ItemIndex);
   obj.Values.Add(radPosition.ItemIndex);
   obj.Values.Add(ord(chkNoHide.Checked));
   obj.Values.Add(ord(chkBlock.Checked));
end;

function TfrmMessageOptions.NewClassType: TEbClass;
begin
   result := TEbMessageOptions;
end;

initialization
   RegisterEbEditor(TEbMessageOptions, TfrmMessageOptions);
finalization
   UnRegisterEbEditor(TEbMessageOptions);
end.
