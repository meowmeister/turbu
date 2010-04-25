unit array_editor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Mask, DBCtrls, DB, ExtCtrls, DbClient,
  dm_database, Grids, DBGrids, turbu_listGrid;

type
  TfrmArrayEdit = class(TForm)
    Panel1: TPanel;
    btnOK: TButton;
    btnCancel: TButton;
    btnApply: TButton;
    srcList: TDataSource;
    lstGroups: TListBox;
    Button1: TButton;
    GroupBox1: TGroupBox;
    lblFieldNumber: TLabel;
    lblArrayType: TLabel;
    DBEdit1: TDBEdit;
    RpgListGrid1: TRpgListGrid;
    procedure lstGroupsClick(Sender: TObject);
  private
    { Private declarations }
    FLocal: TClientDataset;
    procedure setup(const name: string; dset: TClientDataset; id: integer);
    procedure LocalBeforeOpen(DataSet: TDataSet);
    procedure LocalAfterScroll(DataSet: TDataSet);
  public
    { Public declarations }
    class procedure Edit(const caption: string; dset: TClientDataset; var id: integer);
  end;

implementation
uses
   Math,
   extensible_cds;

{$R *.dfm}

{ TfrmArrayEdit }

class procedure TfrmArrayEdit.Edit(const caption: string; dset: TClientDataset; var id: integer);
var
   form: TfrmArrayEdit;
begin
   form := TfrmArrayEdit.Create(nil);
   try
      form.setup(caption, dset, id);
      if form.ShowModal = mrOK then
      begin
         form.FLocal.MergeChangeLog;
         id := form.FLocal.FieldByName('id').AsInteger;
      end
      else form.FLocal.LogChanges := false;
   finally
      form.Free;
   end;
end;

procedure TfrmArrayEdit.LocalAfterScroll(DataSet: TDataSet);
begin
   lblFieldNumber.Caption := format('%.4d:', [FLocal.FieldByName('id').AsInteger]);
end;

procedure TfrmArrayEdit.LocalBeforeOpen(DataSet: TDataSet);
var
   calc: TWideStringField;
begin
   calc := TWideStringField.Create(nil);
   calc.FieldName := 'DisplayName';
   calc.Size := 255;
   calc.FieldKind := fkCalculated;
   calc.Dataset := DataSet;
end;

procedure TfrmArrayEdit.lstGroupsClick(Sender: TObject);
const
   FILTER = '(id >= %d) and (id < %d)';
var
   index: nativeInt;
begin
   index := nativeInt(lstGroups.items.Objects[lstGroups.ItemIndex]);
   FLocal.Filter := format(FILTER, [index, index + 20]);
   FLocal.First;
end;

procedure TfrmArrayEdit.setup(const name: string; dset: TClientDataset; id: integer);
const
   NEW_NAME = '[%.4d - %.4d]';
var
   i, maxGroup: integer;
   count: integer;
begin
   Caption := format(Caption, [name]);
   lblArrayType.Caption := name;
   FLocal := TExtensibleClientDataset.Create(self);
   FLocal.BeforeOpen := self.LocalBeforeOpen;
   FLocal.CloneCursor(dset, false);
   FLocal.OnCalcFields := dset.OnCalcFields;
   FLocal.AfterScroll := self.LocalAfterScroll;
   FLocal.LogChanges := true;
   FLocal.Filtered := true;
   srcList.DataSet := FLocal;
   count := dset.RecordCount;
   maxGroup := count div 20;
   if count mod 20 <> 0 then
      inc(maxGroup);
   lstGroups.Items.BeginUpdate;
      for i := 0 to maxGroup do
         lstGroups.AddItem(format(NEW_NAME, [i * 20, min((i * 20) + 19, count)]), pointer(i * 20));
   lstGroups.Items.EndUpdate;
   lstGroups.ItemIndex := id div 20;
   FLocal.Locate('id', id, []);
end;

end.
