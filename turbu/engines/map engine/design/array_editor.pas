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

unit array_editor;

interface

uses
  SysUtils, Classes, Controls, Forms, StdCtrls, Mask, DBCtrls, DB, ExtCtrls, DbClient,
  Grids, DBGrids, turbu_listGrid;

type
  TfrmArrayEdit = class(TForm)
    Panel1: TPanel;
    btnOK: TButton;
    btnCancel: TButton;
    btnApply: TButton;
    srcList: TDataSource;
    lstGroups: TListBox;
    btnArraySIze: TButton;
    GroupBox1: TGroupBox;
    lblFieldNumber: TLabel;
    lblArrayType: TLabel;
    DBEdit1: TDBEdit;
    RpgListGrid1: TRpgListGrid;
    btnAddVar: TButton;
    procedure lstGroupsClick(Sender: TObject);
    procedure btnAddVarClick(Sender: TObject);
  private
    { Private declarations }
    FGlobals: TCustomClientDataset;
    FContext: TDataset;
    FVartype: string;
    FMaxLocalID: integer;

    procedure setup(const name, vartype: string; dset, local: TCustomClientDataset; id: integer);
    procedure LocalAfterScroll(DataSet: TDataSet);
    procedure ScanMaxLocalID;
  public
    { Public declarations }
    class procedure Edit(const caption, vartype: string; dset, local: TCustomClientDataset; var id: integer);
  end;

implementation
uses
   Math,
   extensible_cds, EBEdit;

{$R *.dfm}

const SUBLIST_SIZE = 50;

{ TfrmArrayEdit }

procedure TfrmArrayEdit.btnAddVarClick(Sender: TObject);
begin
   FContext.AppendRecord(['New', FVartype, FMaxLocalID + 1]);
   inc(FMaxLocalID);
end;

class procedure TfrmArrayEdit.Edit(const caption, vartype: string; dset, local: TCustomClientDataset; var id: integer);
var
   form: TfrmArrayEdit;
   sp: Integer;
begin
   assert(assigned(local));
   form := TfrmArrayEdit.Create(nil);
   try
      form.setup(caption, vartype, dset, local, id);
      sp := form.FGlobals.SavePoint;
      if form.ShowModal = mrOK then
      begin
         form.FGlobals.ApplyUpdates(0);
         id := form.srcList.DataSet.FieldByName('id').AsInteger;
         if form.srcList.DataSet = form.FContext then
            id := -id;
      end
      else form.FGlobals.SavePoint := sp;
   finally
      form.Free;
   end;
end;

procedure TfrmArrayEdit.LocalAfterScroll(DataSet: TDataSet);
begin
   lblFieldNumber.Caption := format('%.4d:', [FGlobals.FieldByName('id').AsInteger]);
end;

procedure TfrmArrayEdit.lstGroupsClick(Sender: TObject);
const
   FILTER = '(id >= %d) and (id < %d)';
var
   index: nativeInt;
begin
   if lstGroups.ItemIndex >= 0 then
      index := nativeInt(lstGroups.items.Objects[lstGroups.ItemIndex])
   else index := -1;
   if index >= 0 then
   begin
      srcList.DataSet := FGlobals;
      FGlobals.Filter := format(FILTER, [index, index + SUBLIST_SIZE]);
      FGlobals.First;
      btnArraySIze.Visible := true;
      btnAddVar.Visible := false;
   end
   else begin
      srcList.Dataset := FContext;
      btnArraySIze.Visible := false;
      btnAddVar.Visible := true;
   end;
end;

procedure TfrmArrayEdit.ScanMaxLocalID;
begin
   FMaxLocalID := 0;
   FContext.First;
   while not FContext.Eof do
   begin
      FMaxLocalID := max(FMaxLocalID, FContext['id']);
      FContext.Next;
   end;
   FContext.First;
end;

procedure TfrmArrayEdit.setup(const name, vartype: string; dset, local: TCustomClientDataset; id: integer);
const
   NEW_NAME = '[%.4d - %.4d]';
var
   i, maxGroup: integer;
   count: integer;
begin
   Caption := format(Caption, [name]);
   lblArrayType.Caption := name;
   FGlobals := TExtensibleClientDataset.Create(self);
   if assigned(dset) then
      FGlobals.CloneCursor(dset, false, true)
   else FGlobals.CreateDataset;
   FGlobals.AfterScroll := self.LocalAfterScroll;
   FGlobals.Filtered := true;
   srcList.DataSet := FGlobals;
   FContext := local;
   ScanMaxLocalID;
   count := dset.RecordCount - 1;
   maxGroup := count div SUBLIST_SIZE;
   if count mod SUBLIST_SIZE <> 0 then
      inc(maxGroup);
   lstGroups.Items.BeginUpdate;
   try
      //can't use -1; see TListBoxStrings.GetObject
      lstGroups.AddItem('Local', pointer(-2));
      if dset.RecordCount > 0 then
         for i := 0 to maxGroup do
            lstGroups.AddItem(format(NEW_NAME, [i * SUBLIST_SIZE,
                                                min((i * SUBLIST_SIZE) + pred(SUBLIST_SIZE), dset.RecordCount)]),
                              pointer(i * SUBLIST_SIZE));
   finally
      lstGroups.Items.EndUpdate;
   end;
   if id >= 0 then
      lstGroups.ItemIndex := (id div SUBLIST_SIZE) + 1
   else lstGroups.ItemIndex := 0;
   lstGroupsClick(self);
//   FContext.filter := 'type = ' + QuotedStr(vartype);
   FGlobals.Locate('id', id, []);
end;

end.
