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

unit EditMemorizeLocation;

interface

uses
   Forms, StdCtrls, ExtCtrls, Classes, Controls,
   EventBuilder, EbEdit, button_edit, variable_selector;

type
   [EditorCategory('Map', 'Memorize Location')]
   TfrmMemorizeLocation = class(TfrmEbEditBase)
      GroupBox1: TGroupBox;
      Label1: TLabel;
      cboMapID: TIntSelector;
      cboXVar: TIntSelector;
      cboYVar: TIntSelector;
   protected
      procedure UploadObject(obj: TEbObject); override;
      procedure DownloadObject(obj: TEbObject); override;
      function NewClassType: TEbClass; override;
   end;

implementation
uses
   EB_Maps;

{$R *.dfm}

{ TfrmMemorizeLocation }

function TfrmMemorizeLocation.NewClassType: TEbClass;
begin
   result := TEBMemorizeLocation;
end;

procedure TfrmMemorizeLocation.DownloadObject(obj: TEbObject);
begin
   obj.Clear;
   obj.Values.Add(cboMapID.ID);
   obj.Values.Add(cboXVar.ID);
   obj.Values.Add(cboYVar.ID);
end;

procedure TfrmMemorizeLocation.UploadObject(obj: TEbObject);
begin
   cboMapID.ID := obj.Values[0];
   cboXVar.ID := obj.Values[1];
   cboYVar.ID := obj.Values[2];
end;

initialization
   RegisterEbEditor(TEBMemorizeLocation, TfrmMemorizeLocation);
finalization
   UnRegisterEbEditor(TEBMemorizeLocation);
end.
