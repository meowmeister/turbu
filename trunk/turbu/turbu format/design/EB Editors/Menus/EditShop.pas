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

unit EditShop;

interface

uses
   Forms, StdCtrls, ExtCtrls, Classes, Controls, DB, DBClient,
   EventBuilder, EbEdit, Grids, DBGrids, turbu_listGrid, ComCtrls;

type
   [EditorCategory('Menus', 'Open Shop')]
   TfrmEBEditBase1 = class(TfrmEBEditBase)
      radTransactions: TRadioGroup;
      grpMessage: TGroupBox;
      shopVocab: TClientDataSet;
      shopVocabKey: TWideStringField;
      shopVocabid: TIntegerField;
      shopVocabVal: TWideStringField;
      cboStyles: TComboBox;
      GroupBox1: TGroupBox;
      lstShopContents: TListView;
      btnAdd: TButton;
      btnRemove: TButton;
      lstItems: TRpgListGrid;
      srcItems: TDataSource;
      chkElseBlock: TCheckBox;
      procedure FormCreate(Sender: TObject);
      procedure shopVocabFilterRecord(DataSet: TDataSet; var Accept: Boolean);
      procedure shopVocabCalcFields(DataSet: TDataSet);
      procedure btnAddClick(Sender: TObject);
      procedure btnRemoveClick(Sender: TObject);
   protected
      procedure UploadObject(obj: TEbObject); override;
      procedure DownloadObject(obj: TEbObject); override;
      function NewClassType: TEbClass; override;
      function ValidateForm: boolean; override;
   end;

implementation
uses
   SysUtils, StrUtils,
   uDataSetHelper,
   EB_Messages, EB_RpgScript, turbu_defs,
   dm_databaseAux;

{$R *.dfm}

procedure TfrmEBEditBase1.FormCreate(Sender: TObject);
var
   rec: variant;
begin
   dmDatabaseAux.EnsureVocab;
   dmDatabaseAux.EnsureItems;
   shopVocab.CloneCursor(dmDatabaseAux.allVocab, false, true);
   shopVocab.IndexFieldNames := 'Key';
   shopVocab.Filtered := true;
   for rec in shopVocab do
      cboStyles.AddItem(format('%d: %s', [shopVocabid.Value, shopVocabVal.value]), pointer(shopVocabId.Value));
   cboStyles.ItemIndex := 0;
end;

procedure TfrmEBEditBase1.btnAddClick(Sender: TObject);
var
   new: TListItem;
   source: TDataset;
begin
   new := lstShopContents.Items.Add;
   source := srcItems.DataSet;
   new.Caption := source.FieldByName('Name').AsString;
   new.SubItems.Add(source.FieldByName('Cost').AsString);
   new.Data := pointer(source.FieldByName('Id').AsInteger);
end;

procedure TfrmEBEditBase1.btnRemoveClick(Sender: TObject);
begin
   lstShopContents.Selected.Free;
end;

procedure TfrmEBEditBase1.shopVocabCalcFields(DataSet: TDataSet);
var
   key: string;
begin
   key := shopVocabKey.Value;
   shopVocabid.Value := StrToIntDef(Trim(Copy(key, 5, length(key) - 10)), -1);
end;

procedure TfrmEBEditBase1.shopVocabFilterRecord(DataSet: TDataSet; var Accept: Boolean);
var
   key: string;
   subkey: string;
   dummy: integer;
begin
   key := shopVocabKey.Value;
   accept := AnsiStartsText('Shop', key) and AnsiEndsText('-Greet', key);
   if accept then
   begin
      subkey := Trim(Copy(key, 5, length(key) - 10));
      accept := TryStrToInt(subkey, dummy);
   end;
end;

function TfrmEBEditBase1.NewClassType: TEbClass;
begin
   result := TEBShop;
end;

procedure TfrmEBEditBase1.DownloadObject(obj: TEbObject);
var
   item: TListItem;
   shop: TEBShop;
begin
   if obj.ChildCount = 0 then
      (obj as TEBShop).Setup('Transaction');
   obj.Values.Clear;
   obj.Values.Add(radTransactions.ItemIndex);
   obj.Values.Add(integer(cboStyles.Items.Objects[cboStyles.ItemIndex]));
   for item in lstShopContents.Items do
      obj.Values.Add(integer(item.Data));
   shop := obj as TEBShop;
   if shop.elseSet and not chkElseBlock.Checked then
      shop.clearElse
   else if (not shop.elseSet) and chkElseBlock.Checked then
      shop.setElse;
end;

procedure TfrmEBEditBase1.UploadObject(obj: TEbObject);
var
   i: integer;
begin
   radTransactions.ItemIndex := obj.Values[0];
   cboStyles.ItemIndex := 0;
   for i := 0 to cboStyles.Items.Count - 1 do
      if integer(cboStyles.Items.Objects[i]) = obj.Values[2] then
      begin
         cboStyles.ItemIndex := i;
         break;
      end;
   srcItems.DataSet.DisableControls;
   try
      for i := 2 to obj.Values.Count - 1 do
         if srcItems.DataSet.Locate('id', obj.Values[i], []) then
            btnAdd.Click;
      srcItems.DataSet.First;
   finally
      srcItems.DataSet.EnableControls;
   end;
   chkElseBlock.Checked := (obj as TEBShop).ElseSet;
end;

function TfrmEBEditBase1.ValidateForm: boolean;
begin
   if (lstShopContents.Items.Count = 0) and (TShopTypes(radTransactions.ItemIndex) <> st_Buy) then
      ValidateError(lstItems, 'Please select at least one item for the shop to sell.');
   result := true;
end;

initialization
   RegisterEbEditor(TEBShop, TfrmEBEditBase1);
finalization
   UnRegisterEbEditor(TEBShop);
end.
