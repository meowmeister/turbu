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

unit EditInput;

interface

uses
  SysUtils, Classes, Controls, Forms, StdCtrls, ExtCtrls, Mask,
  EventBuilder, EbEdit, variable_selector;

type
   [EditorCategory('Basics', 'Direct Input', 3)]
   TfrmInputEdit = class(TfrmEbEditBase)
   protected
      procedure UploadObject(obj: TEbObject); override;
      procedure DownloadObject(obj: TEbObject); override;
      function NewClassType: TEbClass; override;
   end;

var
  frmInputEdit: TfrmInputEdit;

implementation
uses
   EB_System;

{$R *.dfm}

{ TfrmInputEdit }

procedure TfrmInputEdit.DownloadObject(obj: TEbObject);
begin
   obj.Values.Clear;
end;

procedure TfrmInputEdit.UploadObject(obj: TEbObject);
begin
end;

function TfrmInputEdit.NewClassType: TEbClass;
begin
   result := TEBInput;
end;

{initialization
   RegisterEbEditor(TEBInput, TfrmInputEdit);}
end.
