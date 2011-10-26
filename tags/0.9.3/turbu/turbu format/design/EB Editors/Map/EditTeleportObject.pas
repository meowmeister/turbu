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

unit EditTeleportObject;

interface

uses
  Forms, Controls, Classes, Dialogs, StdCtrls, ExtCtrls,
  EventBuilder, EbEdit, variable_selector, button_edit, turbu_map_interface,
  sg_defs;

type
   [EditorCategory('Map', 'Teleport Map Object')]
   TfrmEditTeleportObject = class(TfrmEbEditBase)
      cboObject: TComboBox;
      txtPosition: TRpgButtonEdit;
      grpVariables: TGroupBox;
      cboXVar: TIntSelector;
      cboYVar: TIntSelector;
      radPosition: TRadioButton;
      radPtr: TRadioButton;
      Label2: TLabel;
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
   EB_Maps, EB_Expressions, EditTeleport;

{$R *.dfm}

{ TfrmEditTeleportObject }

procedure TfrmEditTeleportObject.SetupMap(const map: IRpgMap);
var
   list: TStrings;
begin
   if assigned(map) then
   begin
      FCurrentMap := map.id;
      list := map.GetMapObjects;
      try
         list.Delete(0);
         self.cboObject.Items.AddStrings(list);
      finally
         list.Free;
      end;
   end
   else FCurrentMap := -1;
end;

type
   THackTeleport = class(TfrmEBEditTeleport);

procedure TfrmEditTeleportObject.txtPositionButtonClick(Sender: TObject);
var
   editor: TfrmEBEditTeleport;
   tel: TEBTeleport;
begin
   tel := TEBTeleport.Create(nil);
   editor := TfrmEBEditTeleport.Create(nil);
   try
      editor.radFacing.Visible := false;
      editor.trvMapTree.Visible := false;
      tel.Values.AddRange([FCurrentMap, FPosition.x, FPosition.y, 0]);
      THackTeleport(editor).UploadObject(tel);
      if editor.ShowModal = mrOk then
      begin
         THackTeleport(editor).DownloadObject(tel);
         FCurrentMap := tel.Values[0];
         FPosition := sgPoint(tel.Values[1], tel.Values[2]);
         EnableControlsProperly;
      end;
   finally
      editor.Release;
      tel.Free;
   end;
end;

procedure TfrmEditTeleportObject.UploadObject(obj: TEbObject);
begin
   UploadObjectRef(obj.children[0] as TEBObjExpr, cboObject);
   if obj.Values[0] = 0 then
   begin
      radPosition.checked := true;
      FPosition := sgPoint(obj.Values[1], obj.Values[2]);
      txtPosition.Text := format('(%.3d, %.3d)', [FPosition.x, FPosition.y]);
   end
   else begin
      radPtr.Checked := true;
      cboXVar.ID := obj.Values[1];
      cboYVar.ID := obj.Values[2];
   end;
end;

procedure TfrmEditTeleportObject.DownloadObject(obj: TEbObject);
begin
   obj.Clear;
   obj.Add(DownloadObjectRef(cboObject));
   if radPosition.Checked then
      obj.Values.AddRange([0, FPosition.x, FPosition.y])
   else obj.Values.AddRange([1, cboXVar.ID, cboYVar.ID]);
end;

procedure TfrmEditTeleportObject.EnableControlsProperly;
begin
   EnableControl(txtPosition, radPosition);
   EnableGroupBox(grpVariables, radPtr.Checked);
   if txtPosition.Enabled then
      txtPosition.Text := format('(%.3d, %.3d)', [FPosition.x, FPosition.y])
   else txtPosition.Text := '';
end;

function TfrmEditTeleportObject.NewClassType: TEbClass;
begin
   result := TEBTeleportMapObj;
end;

initialization
   RegisterEbEditor(TEBTeleportMapObj, TfrmEditTeleportObject);
finalization
   UnRegisterEbEditor(TEBTeleportMapObj);
end.
