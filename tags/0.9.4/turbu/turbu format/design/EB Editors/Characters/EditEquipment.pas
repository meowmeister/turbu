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

unit EditEquipment;

interface

uses
   DBClient, Classes, Controls, ExtCtrls, DB, StdCtrls, DBCtrls,
   EventBuilder, variable_selector, ebPartyBase, IDLookupCombo, EBEdit,
   turbu_variable_selector, button_edit;

type
   [EditorCategory('Characters', 'Change Equipment')]
   [EditorContext('RM2K')]
   TfrmEBEditEquipment = class(TfrmEBPartyBase)
      grpOperation: TRadioGroup;
      grpItem: TGroupBox;
      cboItemID: TIDLookupCombo;
      radSpecificItem: TRadioButton;
      radItemPtr: TRadioButton;
      selItemID: TIntSelector;
      srcEquipment: TDataSource;
      items_equipment: TClientDataSet;
      items_equipmentid: TIntegerField;
      items_equipmentname: TWideStringField;
      radSlot: TRadioGroup;
      procedure FormCreate(Sender: TObject);
   protected
      procedure UploadObject(obj: TEbObject); override;
      procedure DownloadObject(obj: TEbObject); override;
      function NewClassType: TEbClass; override;
      procedure EnableControlsProperly; override;
   end;

implementation
uses
   turbu_constants, dm_database, EB_Characters, EB_Expressions, EbEditHelper;

{$R *.dfm}

{ TfrmEBEditEquipment }

procedure TfrmEBEditEquipment.FormCreate(Sender: TObject);

   function slotLookup(index: integer): string;
   const SLOTS: array[0..4] of string =
     (V_EQ_WEAPON, V_EQ_SHIELD, V_EQ_ARMOR, V_EQ_HELMET, V_EQ_ACCESSORY);
   begin
      result := dmDatabase.Vocab.Lookup('Key', SLOTS[index], 'Val');
   end;

var
   i: integer;
begin
   dmDatabase.Vocab.Active := true;
   radSlot.Items.Clear;
   for i := 0 to 4 do
      radSlot.Items.Add(slotLookup(i));
   radSlot.Items.Add('All');
end;

procedure TfrmEBEditEquipment.UploadObject(obj: TEbObject);
var
   i: integer;
   expr: TEBExpression;
begin
   inherited UploadObject(Obj);
   grpOperation.ItemIndex := obj.Values[0];
   if grpOperation.ItemIndex = 0 then
      UploadLookupPtrSelection(obj.Children[1] as TEBExpression, radSpecificItem,
                               radItemPtr, cboItemID, selItemID)
   else begin
      expr := obj.Children[1] as TEBEnumValue;
      radSlot.ItemIndex := -1;
      for i := 0 to High(SLOTS) do
         if SLOTS[i] = expr.Text then
         begin
            radSlot.ItemIndex := i;
            break;
         end;
      assert(radSlot.ItemIndex <> -1);
   end;
end;

procedure TfrmEBEditEquipment.DownloadObject(obj: TEbObject);
begin
   inherited DownloadObject(obj);
   obj.Values.Add(grpOperation.ItemIndex);
   if grpOperation.ItemIndex = 0 then
      obj.Add(DownloadLookupPtrSelection(radSpecificItem, radItemPtr, cboItemID, selItemID, 'items'))
   else obj.Add(TEBEnumValue.Create(SLOTS[radSlot.ItemIndex]));
end;

procedure TfrmEBEditEquipment.EnableControlsProperly;
begin
   inherited EnableControlsProperly;
   if grpOperation.ItemIndex = 0 then
   begin
      EnableGroupBox(grpItem, true);
      EnableControl(cboItemID, radSpecificItem);
      EnableControl(selItemID, radItemPtr);
      radSlot.Enabled := false;
   end
   else begin
      EnableGroupBox(grpItem, false);
      radSlot.Enabled := true;
   end;
end;

function TfrmEBEditEquipment.NewClassType: TEbClass;
begin
   result := TEBEquipment;
end;

initialization
   RegisterEbEditor(TEBEquipment, TfrmEBEditEquipment);
finalization
   UnRegisterEbEditor(TEBEquipment);
end.
