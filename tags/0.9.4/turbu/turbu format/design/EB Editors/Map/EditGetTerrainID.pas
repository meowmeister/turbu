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

unit EditGetTerrainID;

interface

uses
   Forms, StdCtrls, Controls, Classes, ExtCtrls,
   EventBuilder, EbEdit, variable_selector, turbu_variable_selector, button_edit,
   turbu_map_interface, EB_Maps,
   SG_Defs;

type
   [EditorCategory('Map', 'Get Terrain ID')]
   [EditorContext('RM2K')]
   TfrmGetID = class(TfrmEbEditBase)
      GroupBox2: TGroupBox;
      Label2: TLabel;
      txtPosition: TRpgButtonEdit;
      grpVariables: TGroupBox;
      cboXVar: TIntSelector;
      cboYVar: TIntSelector;
      radPosition: TRadioButton;
      radPtr: TRadioButton;
      selLValue: TIntSelector;
      procedure txtPositionButtonClick(Sender: TObject);
   private
      FPosition: TSgPoint;
      FCurrentMap: integer;
   protected
      procedure UploadObject(obj: TEbObject); override;
      procedure DownloadObject(obj: TEbObject); override;
      procedure EnableControlsProperly; override;
   public
      procedure SetupMap(const map: IRpgMap); override;
   end;

   TfrmGetID<T: TEBObject> = class(TfrmGetID)
   protected
      function NewClassType: TEbClass; override;
   end;

   [EditorCategory('Map', 'Get Terrain ID')]
   TfrmGetTerrainID = class(TFrmGetID<TEBTerrainID>);

   [EditorCategory('Map', 'Get Map Object ID')]
   TfrmGetMapObjID = class(TFrmGetID<TEBMapObjID>)
      procedure FormShow(Sender: TObject);
   end;

implementation
uses
   SysUtils,
   dm_database, EditTeleport;

{$R *.dfm}

{ TfrmStoreTerrainID }

procedure TfrmGetID.EnableControlsProperly;
begin
   EnableControl(txtPosition, radPosition);
   EnableGroupBox(grpVariables, radPtr.Checked);
   if txtPosition.Enabled then
      txtPosition.Text := format('(%.3d, %.3d)', [FPosition.x, FPosition.y])
   else txtPosition.Text := '';
end;

procedure TfrmGetID.SetupMap(const map: IRpgMap);
begin
   FCurrentMap := map.id;
end;

procedure TfrmGetID.txtPositionButtonClick(Sender: TObject);
var
   editor: TfrmEBEditTeleport;
   tel: TEBTeleport;
begin
   tel := TEBTeleport.Create(nil);
   editor := TfrmEBEditTeleport.Create(nil);
   try
      tel.Values.AddRange([FCurrentMap, FPosition.x, FPosition.y, 0]);
      if editor.EditExternal(tel, true) then
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

procedure TfrmGetID.UploadObject(obj: TEbObject);
begin
   if obj.Values[0] = 0 then
   begin
      radPosition.Checked := true;
      FPosition := sgPoint(obj.Values[1], obj.Values[2]);
   end
   else begin
      radPtr.Checked := true;
      cboXVar.ID := obj.Values[1];
      cboYVar.ID := obj.Values[2];
   end;
   selLValue.ID := obj.Values[3];
end;

procedure TfrmGetID.DownloadObject(obj: TEbObject);
begin
   obj.Clear;
   if radPosition.Checked then
   begin
      obj.Values.Add(0);
      obj.Values.Add(FPosition.x);
      obj.Values.Add(FPosition.y);
   end
   else begin
      assert(radPtr.Checked);
      obj.Values.Add(1);
      obj.Values.Add(cboXVar.ID);
      obj.Values.Add(cboyVar.ID);
   end;
   obj.Values.Add(selLValue.ID);
end;

{ TfrmGetID<T> }

function TfrmGetID<T>.NewClassType: TEbClass;
begin
   result := T;
end;

{ TfrmGetMapObjID }

procedure TfrmGetMapObjID.FormShow(Sender: TObject);
begin
   self.Caption := 'Get Map Object ID';
end;

initialization
   RegisterEbEditor(TEBTerrainID, TfrmGetTerrainID);
   RegisterEbEditor(TEBMapObjID, TfrmGetMapObjID);
finalization
   UnRegisterEbEditor(TEBTerrainID);
   UnRegisterEbEditor(TEBMapObjID);
end.
