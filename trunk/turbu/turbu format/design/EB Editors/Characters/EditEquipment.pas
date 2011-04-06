unit EditEquipment;

interface

uses
  Dialogs, DBClient, Classes, Controls, ExtCtrls, DB, StdCtrls, DBCtrls,
  EventBuilder, variable_selector, ebPartyBase, IDLookupCombo, EBEdit;

type
   [EditorCategory('Characters', 'Change Equipment')]
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
   turbu_constants, dm_database, EB_Characters, EB_Expressions;

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
      UploadLookupPtrSelection(obj.Components[1] as TEBExpression, radSpecificItem,
                               radItemPtr, cboItemID, selItemID)
   else begin
      expr := obj.Components[1] as TEBEnumValue;
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
