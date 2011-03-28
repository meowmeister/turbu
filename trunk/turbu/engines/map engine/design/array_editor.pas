unit array_editor;

interface

uses
  SysUtils, Classes, Controls, Forms, StdCtrls, Mask, DBCtrls, DB, ExtCtrls, DbClient,
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
    FLocal: TCustomClientDataset;
    class var FContext: TDataset;

    procedure setup(const name, vartype: string; dset: TCustomClientDataset; id: integer);
    procedure LocalBeforeOpen(DataSet: TDataSet);
    procedure LocalAfterScroll(DataSet: TDataSet);
  public
    { Public declarations }
    class property VariableContext: TDataset read FContext write FContext;
    class procedure Edit(const caption, vartype: string; dset: TCustomClientDataset; var id: integer);
  end;

implementation
uses
   Math,
   extensible_cds, EBEdit;

{$R *.dfm}

const SUBLIST_SIZE = 50;

{ TfrmArrayEdit }

class procedure TfrmArrayEdit.Edit(const caption, vartype: string; dset: TCustomClientDataset; var id: integer);
var
   form: TfrmArrayEdit;
begin
   form := TfrmArrayEdit.Create(nil);
   try
      form.setup(caption, vartype, dset, id);
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
   if index >= 0 then
   begin
      srcList.DataSet := FLocal;
      FLocal.Filter := format(FILTER, [index, index + SUBLIST_SIZE]);
      FLocal.First;
   end
   else srcList.Dataset := FContext;
end;

procedure TfrmArrayEdit.setup(const name, vartype: string; dset: TCustomClientDataset; id: integer);
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
   count := dset.RecordCount - 1;
   maxGroup := count div SUBLIST_SIZE;
   if count mod SUBLIST_SIZE <> 0 then
      inc(maxGroup);
   lstGroups.Items.BeginUpdate;
      //can't use -1; see TListBoxStrings.GetObject
      lstGroups.AddItem('Local', pointer(-2));
      for i := 0 to maxGroup do
         lstGroups.AddItem(format(NEW_NAME, [i * SUBLIST_SIZE,
                                             min((i * SUBLIST_SIZE) + pred(SUBLIST_SIZE), count)]),
                           pointer(i * SUBLIST_SIZE));
   lstGroups.Items.EndUpdate;
   if id >= 0 then
      lstGroups.ItemIndex := (id div SUBLIST_SIZE) + 1
   else lstGroups.ItemIndex := 0;
   lstGroupsClick(self);
//   FContext.filter := 'type = ' + QuotedStr(vartype);
   FLocal.Locate('id', id, []);
end;

end.
