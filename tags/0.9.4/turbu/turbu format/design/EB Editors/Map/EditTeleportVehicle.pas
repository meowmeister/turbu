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

unit EditTeleportVehicle;

interface

uses
   Forms, DBCtrls, StdCtrls, Classes, Controls, ExtCtrls, DB,
   EventBuilder, EbEdit, IDLookupCombo, variable_selector, button_edit,
   turbu_variable_selector,
   turbu_map_interface, sg_defs;

type
   [EditorCategory('Map', 'Teleport Vehicle')]
   [EditorContext('RM2K')]
   TfrmTeleportVehicle = class(TfrmEbEditBase)
      cboVehicle: TIDLookupCombo;
      srcVehicles: TDataSource;
      txtPosition: TRpgButtonEdit;
      radPosition: TRadioButton;
      radPtr: TRadioButton;
      grpVariables: TGroupBox;
      Label1: TLabel;
      cboMapID: TIntSelector;
      cboXVar: TIntSelector;
      cboYVar: TIntSelector;
      procedure txtPositionButtonClick(Sender: TObject);
   private
      FCurrentMap: integer;
      FPosition: TsgPoint;
   protected
      procedure UploadObject(obj: TEbObject); override;
      procedure DownloadObject(obj: TEbObject); override;
      function NewClassType: TEbClass; override;
      procedure EnableControlsProperly; override;
   public
      procedure SetupMap(const map: IRpgMap); override;
   end;

implementation
uses
   SysUtils,
   dm_database, turbu_database, EB_Maps, EB_Expressions,
   EditTeleport;

{$R *.dfm}

procedure TfrmTeleportVehicle.SetupMap(const map: IRpgMap);
begin
   FCurrentMap := map.id;
end;

procedure TfrmTeleportVehicle.txtPositionButtonClick(Sender: TObject);
var
   editor: TfrmEBEditTeleport;
   tel: TEBTeleport;
begin
   tel := TEBTeleport.Create(nil);
   editor := TfrmEBEditTeleport.Create(nil);
   try
      tel.Values.AddRange([FCurrentMap, FPosition.x, FPosition.y, 0]);
      if editor.EditExternal(tel) then
      begin
         FCurrentMap := tel.Values[0];
         FPosition := sgPoint(tel.Values[1], tel.Values[2]);
         EnableControlsProperly;
      end;
   finally
      editor.Release;
      tel.Free;
   end;
end;

procedure TfrmTeleportVehicle.EnableControlsProperly;
begin
   EnableControl(txtPosition, radPosition);
   EnableGroupBox(grpVariables, radPtr.Checked);
   if txtPosition.Enabled then
   begin
      txtPosition.Text := format('%.4d: %s, (%.3d, %.3d)',
         [FCurrentMap,
          dmDatabase.metadata.Lookup('id', FCurrentMap, 'name'),
          FPosition.x,
          FPosition.y]);
   end
   else txtPosition.Text := '';
end;

function TfrmTeleportVehicle.NewClassType: TEbClass;
begin
   result := TEBTeleportVehicle;
end;

procedure TfrmTeleportVehicle.DownloadObject(obj: TEbObject);
begin
   obj.Clear;
   obj.Values.Add(cboVehicle.id);
   if radPosition.Checked then
   begin
      obj.Values.Add(0);
      obj.Values.Add(FCurrentMap);
      obj.Values.AddRange([FPosition.x, FPosition.y]);
   end
   else begin
      obj.Values.Add(1);
      obj.Values.Add(cboMapID.ID);
      obj.Values.Add(cboXVar.ID);
      obj.Values.Add(cboYVar.ID);
   end;
end;

procedure TfrmTeleportVehicle.UploadObject(obj: TEbObject);
begin
   cboVehicle.id := obj.Values[0];
   if obj.Values[1] = 0 then
   begin
      radPosition.Checked := true;
      FCurrentMap := obj.Values[2];
      FPosition := sgPoint(obj.Values[3], obj.Values[4]);
   end
   else begin
      radPtr.Checked := true;
      cboMapID.ID := obj.Values[2];
      cboXVar.ID := obj.Values[3];
      cboYVar.ID := obj.Values[4];
   end;
end;

initialization
   RegisterEbEditor(TEBTeleportVehicle, TfrmTeleportVehicle);
finalization
   UnRegisterEbEditor(TEBTeleportVehicle);
end.
