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
   EventBuilder, EbEdit, button_edit, variable_selector, turbu_variable_selector,
   EB_Maps;

type
   TfrmMemorizedLocation = class(TfrmEbEditBase)
      GroupBox1: TGroupBox;
      Label1: TLabel;
      cboMapID: TIntSelector;
      cboXVar: TIntSelector;
      cboYVar: TIntSelector;
   protected
      procedure UploadObject(obj: TEbObject); override;
      procedure DownloadObject(obj: TEbObject); override;
      function ValidateForm: boolean; override;
   end;

   TfrmMemorizedLocation<T: TEBObject> = class(TfrmMemorizedLocation)
   protected
      function NewClassType: TEbClass; override;
   end;

   [EditorCategory('Map', 'Memorize Location')]
   [EditorContext('RM2K')]
   TfrmMemorizeLocation = class(TfrmMemorizedLocation<TEBMemorizeLocation>)
      procedure FormShow(Sender: TObject);
   end;

   [EditorCategory('Map', 'Teleport To Memorized Location')]
   [EditorContext('RM2K')]
   TfrmMemoTeleport = class(TfrmMemorizedLocation<TEBMemoTeleport>)
      procedure FormShow(Sender: TObject);
   end;

implementation

{$R *.dfm}

{ TfrmMemorizedLocation<T> }

function TfrmMemorizedLocation<T>.NewClassType: TEbClass;
begin
   result := T;
end;

procedure TfrmMemorizedLocation.DownloadObject(obj: TEbObject);
begin
   obj.Clear;
   obj.Values.Add(cboMapID.ID);
   obj.Values.Add(cboXVar.ID);
   obj.Values.Add(cboYVar.ID);
end;

procedure TfrmMemorizedLocation.UploadObject(obj: TEbObject);
begin
   cboMapID.ID := obj.Values[0];
   cboXVar.ID := obj.Values[1];
   cboYVar.ID := obj.Values[2];
end;

function TfrmMemorizedLocation.ValidateForm: boolean;
const GLOBALS_ONLY = 'This command requires slots from the global ints array';
begin
   if cboMapID.ID <= 0 then
      ValidateError(cboMapID, GLOBALS_ONLY);
   if cboXVar.ID <= 0 then
      ValidateError(cboXVar, GLOBALS_ONLY);
   if cboYVar.ID <= 0 then
      ValidateError(cboYVar, GLOBALS_ONLY);
   result := true;
end;

{ TfrmMemorizeLocation }

procedure TfrmMemorizeLocation.FormShow(Sender: TObject);
begin
   self.Caption := 'Memorize Location';
end;

{ TfrmMemoTeleport }

procedure TfrmMemoTeleport.FormShow(Sender: TObject);
begin
   self.Caption := 'Teleport To Memorized Location';
end;

initialization
   RegisterEbEditor(TEBMemorizeLocation, TfrmMemorizeLocation);
   RegisterEbEditor(TEBMemoTeleport, TfrmMemoTeleport);
finalization
   UnRegisterEbEditor(TEBMemorizeLocation);
   UnRegisterEbEditor(TEBMemoTeleport);
end.
