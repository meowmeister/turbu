unit EditInventory;

interface

uses
   StdCtrls, Classes, Controls, ExtCtrls, DB, DBCtrls, Mask,
   JvExMask, JvSpin,
   EventBuilder, EbEdit, dm_database, variable_selector, IDLookupCombo,
  button_edit;

type
   [EditorCategory('Characters', 'Change Inventory')]
   TfrmEBEditInventory = class(TfrmEbEditBase)
      grpOperation: TRadioGroup;
      grpItemCount: TGroupBox;
      radExactAmount: TRadioButton;
      radPointer: TRadioButton;
      spnExactValue: TJvSpinEdit;
      selValue: TIntSelector;
      srcItems: TDataSource;
      cboItemID: TIDLookupCombo;
      radSpecificItem: TRadioButton;
      radItemPtr: TRadioButton;
      selItemID: TIntSelector;
   protected
      procedure UploadObject(obj: TEbObject); override;
      procedure DownloadObject(obj: TEbObject); override;
      function NewClassType: TEbClass; override;
      procedure EnableControlsProperly; override;
   end;

implementation
uses
   EB_Characters, EB_Expressions, EbEditHelper;

{$R *.dfm}

{ TfrmEBEditInventory }

procedure TfrmEBEditInventory.EnableControlsProperly;
begin
   EnableControl(cboItemID, radSpecificItem);
   EnableControl(selItemID, radItemPtr);
   EnableControl(spnExactValue, radExactAmount);
   EnableControl(selValue, radPointer);
   EnableGroupBox(grpItemCount, grpOperation.ItemIndex <> 2);
end;

function TfrmEBEditInventory.NewClassType: TEbClass;
begin
   result := TEBInventory;
end;

procedure TfrmEBEditInventory.DownloadObject(obj: TEbObject);
var
   pair: TIntPair;
begin
   obj.Clear;
   obj.Values.Add(grpOperation.ItemIndex);
   if grpOperation.ItemIndex <> 2 then
   begin
      pair := DownloadValuePtrSelection(radExactAmount, radPointer, spnExactValue, selValue);
      obj.Values.AddRange(pair);
   end;
   obj.Add(DownloadLookupPtrSelection(radSpecificItem, radItemPtr, cboItemID, selItemID, 'Items'));
end;

procedure TfrmEBEditInventory.UploadObject(obj: TEbObject);
begin
   grpOperation.ItemIndex := obj.Values[0];
   UploadLookupPtrSelection(obj.Children[0] as TEBExpression, radSpecificItem,
     radItemPtr, cboItemID, selItemID);
   if grpOperation.ItemIndex <> 2 then
      UploadValuePtrSelection(obj.Values[1], obj.Values[2], radExactAmount, radPointer,
        spnExactValue, selValue);
end;

initialization
   RegisterEbEditor(TEBInventory, TfrmEBEditInventory);
finalization
   UnRegisterEbEditor(TEBInventory);
end.
