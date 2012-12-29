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

unit EditDeleteObject;

interface

uses
  StdCtrls, Classes, Controls, Forms, ExtCtrls,
  EbEdit, EventBuilder;

type
   [EditorCategory('Basics', 'Delete Map Object')]
   [EditorContext('RM2K')]
   TfrmEBDeleteObject = class(TfrmEbEditBase)
      radDuration: TRadioGroup;
   public
      procedure UploadObject(obj: TEbObject); override;
      procedure DownloadObject(obj: TEbObject); override;
      function NewClassType: TEbClass; override;
   end;

implementation
uses
   EB_System;

{$R *.dfm}

{ TfrmEBDeleteEvent }

procedure TfrmEBDeleteObject.DownloadObject(obj: TEbObject);
begin
   obj.Values.Clear;
   obj.Values.Add(radDuration.ItemIndex);
end;

function TfrmEBDeleteObject.NewClassType: TEbClass;
begin
   result := TEBDeleteObj;
end;

procedure TfrmEBDeleteObject.UploadObject(obj: TEbObject);
begin
   radDuration.ItemIndex := obj.values[0];
end;

initialization
   RegisterEbEditor(TEBDeleteObj, TfrmEBDeleteObject);
finalization
   UnRegisterEbEditor(TEBDeleteObj);
end.
