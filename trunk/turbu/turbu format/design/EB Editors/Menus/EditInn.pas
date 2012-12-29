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

unit EditInn;

interface

uses
   Forms, StdCtrls, ExtCtrls, Classes, Controls, DB, DBClient, ComCtrls, Mask,
   EventBuilder, EbEdit,
  JvExMask, JvSpin;

type
   [EditorCategory('Messages', 'Call Inn')]
   [EditorContext('RM2K')]
   TfrmEBEditInn = class(TfrmEbEditBase)
      grpMessage: TGroupBox;
      innVocab: TClientDataSet;
      innVocabKey: TWideStringField;
      innVocabid: TIntegerField;
      innVocabVal: TWideStringField;
      cboStyles: TComboBox;
      chkElseBlock: TCheckBox;
      spnPrice: TJvSpinEdit;
      procedure FormCreate(Sender: TObject);
      procedure innVocabFilterRecord(DataSet: TDataSet; var Accept: Boolean);
      procedure innVocabCalcFields(DataSet: TDataSet);
   protected
      procedure UploadObject(obj: TEbObject); override;
      procedure DownloadObject(obj: TEbObject); override;
      function NewClassType: TEbClass; override;
   end;

implementation
uses
   SysUtils, StrUtils,
   uDataSetHelper,
   EB_Messages, EB_RpgScript, turbu_defs,
   dm_databaseAux;

{$R *.dfm}

procedure TfrmEBEditInn.FormCreate(Sender: TObject);
var
   rec: variant;
begin
   dmDatabaseAux.EnsureVocab;
   innVocab.CloneCursor(dmDatabaseAux.allVocab, false, true);
   innVocab.IndexFieldNames := 'Key';
   innVocab.Filtered := true;
   for rec in innVocab do
      cboStyles.AddItem(format('%d: %s', [innVocabid.Value, innVocabVal.value]), pointer(innVocabid.Value));
   cboStyles.ItemIndex := 0;
end;

procedure TfrmEBEditInn.innVocabCalcFields(DataSet: TDataSet);
var
   key: string;
begin
   key := innVocabKey.Value;
   innVocabid.Value := StrToIntDef(Trim(Copy(key, 4, length(key) - 10)), -1);
end;

procedure TfrmEBEditInn.innVocabFilterRecord(DataSet: TDataSet; var Accept: Boolean);
var
   key: string;
   subkey: string;
   dummy: integer;
begin
   key := innVocabKey.Value;
   accept := AnsiStartsText('Inn', key) and AnsiEndsText('-Greet1', key);
   if accept then
   begin
      subkey := Trim(Copy(key, 4, length(key) - 10));
      accept := TryStrToInt(subkey, dummy);
   end;
end;

function TfrmEBEditInn.NewClassType: TEbClass;
begin
   result := TEBInn;
end;

procedure TfrmEBEditInn.DownloadObject(obj: TEbObject);
var
   inn: TEBInn;
begin
   inn := obj as TEBInn;
   if obj.ChildCount = 0 then
      inn.Setup('Stay');
   obj.Values.Clear;
   obj.Values.Add(integer(cboStyles.Items.Objects[cboStyles.ItemIndex]));
   obj.Values.Add(spnPrice.AsInteger);
   if inn.elseSet and not chkElseBlock.Checked then
      inn.clearElse
   else if (not inn.elseSet) and chkElseBlock.Checked then
      inn.setElse;
   inn.IfBlock := chkElseBlock.Checked;
end;

procedure TfrmEBEditInn.UploadObject(obj: TEbObject);
var
   i: integer;
   inn: TEBInn;
begin
   inn := obj as TEBInn;
   cboStyles.ItemIndex := 0;
   for i := 0 to cboStyles.Items.Count - 1 do
      if integer(cboStyles.Items.Objects[i]) = obj.Values[0] then
      begin
         cboStyles.ItemIndex := i;
         break;
      end;
   spnPrice.AsInteger := obj.Values[1];
   chkElseBlock.Checked := inn.ElseSet;
   assert(inn.ElseSet = inn.IfBlock);
end;

initialization
   RegisterEbEditor(TEBInn, TfrmEBEditInn);
finalization
   UnRegisterEbEditor(TEBInn);
end.
