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

unit EditStats;

interface

uses
   ExtCtrls, DB, StdCtrls, DBCtrls, Classes, Controls, Mask, JvExMask, JvSpin,
   EBEdit, ebPartyBase, variable_selector, EventBuilder, IDLookupCombo,
   turbu_variable_selector, button_edit;

type
   [EditorCategory('Characters', 'Change Stats')]
   [EditorContext('RM2K')]
   TfrmEBEditStats = class(TfrmEBPartyBase)
      grpOperation: TRadioGroup;
      GroupBox1: TGroupBox;
      cboStat: TComboBox;
      radExactAmount: TRadioButton;
      radPointer: TRadioButton;
      spnExactValue: TJvSpinEdit;
      selValue: TIntSelector;
      procedure FormCreate(Sender: TObject);
   protected
      procedure UploadObject(obj: TEbObject); override;
      procedure DownloadObject(obj: TEbObject); override;
      function NewClassType: TEbClass; override;
      procedure EnableControlsProperly; override;
   end;

implementation
uses
   turbu_constants, dm_database, EB_Characters, EB_Expressions, EbEditHelper,
   EB_Expressions_RM;

{$R *.dfm}

procedure TfrmEBEditStats.FormCreate(Sender: TObject);

   function statLookup(index: integer): string;
   const STATS: array[0..5] of string =
     (V_STAT_HP, V_STAT_MP, V_STAT_ATTACK, V_STAT_DEFENSE, V_STAT_MIND, V_STAT_SPEED);
   begin
      result := dmDatabase.Vocab.Lookup('Key', STATS[index], 'Val');
   end;

var
   i: integer;
   stat: string;
begin
   dmDatabase.Vocab.Active := true;
   for i := 0 to 5 do
   begin
      stat := statLookup(i);
      //first 2 stats are HP and MP, which have both maximum and current values
      if i < 2 then
         stat := 'Max ' + stat;
      cboStat.Items.Add(stat);
   end;
end;

function TfrmEBEditStats.NewClassType: TEbClass;
begin
   result := TEBStats;
end;

procedure TfrmEBEditStats.UploadObject(obj: TEbObject);
var
   prop: TEBPropExpr;
   i: integer;
begin
   inherited UploadObject(obj);
   prop := (obj.Children[0] as TEBChainable).chain as TEBPropExpr;
   assert(assigned(prop));
   cboStat.ItemIndex := -1;
   for i := 0 to High(STATS) do
      if STATS[i] = prop.Text then
      begin
         cboStat.ItemIndex := i;
         break;
      end;
   assert(cboStat.ItemIndex <> -1);
   grpOperation.ItemIndex := obj.Values[0];
   UploadValuePtrSelection(obj.Children[1] as TEBExpression, radExactAmount,
     radPointer, spnExactValue, selValue);
end;

procedure TfrmEBEditStats.DownloadObject(obj: TEbObject);
var
   value: TEBExpression;
begin
   inherited DownloadObject(obj);
   (obj.Children[0] as TEBChainable).chain := TEBPropExpr.Create(STATS[cboStat.ItemIndex]);
   obj.Values.Add(grpOperation.ItemIndex);
   if radExactAmount.Checked then
      value := TEBIntegerValue.Create(spnExactValue.AsInteger)
   else value := TEBIntsValue.Create(selValue.ID);
   obj.Add(value);
end;

procedure TfrmEBEditStats.EnableControlsProperly;
begin
   inherited EnableControlsProperly;
   EnableControl(spnExactValue, radExactAmount);
   EnableControl(selValue, radPointer);
end;

initialization
   RegisterEbEditor(TEBStats, TfrmEBEditStats);
finalization
   UnRegisterEbEditor(TEBStats);
end.
