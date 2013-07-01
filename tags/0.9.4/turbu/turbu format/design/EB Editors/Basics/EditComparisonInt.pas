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

unit EditComparisonInt;

interface

uses
   Classes, Controls, Forms, StdCtrls, ExtCtrls,
   EventBuilder, EbEdit, Mask, JvExMask, JvSpin;

type
   [EditorCategory('Basics', 'Comparison (Integers)')]
   [EditorContext('RM2K Cond')]
   TfrmEBEditComparisonInt = class(TfrmEbEditBase, IContextualEditor)
      Panel2: TPanel;
      grpOperation: TRadioGroup;
      radLVariable: TRadioButton;
      radLProperty: TRadioButton;
      cboLProp: TComboBox;
      spnRValue: TJvSpinEdit;
      radRValue: TRadioButton;
      radRVariable: TRadioButton;
      radRProperty: TRadioButton;
      cboRProp: TComboBox;
      radLFunction: TRadioButton;
      txtLFunc: TEdit;
      btnLFunc: TButton;
      procedure btnLFuncClick(Sender: TObject);
      procedure FormDestroy(Sender: TObject);
   private
      FFunctionL: TEBExpression;
      FContext, FSuffix: string;
      procedure SetContext(const context, suffix: string);
   protected
      procedure UploadObject(obj: TEbObject); override;
      procedure DownloadObject(obj: TEbObject); override;
      function NewClassType: TEbClass; override;
   end;

implementation
uses
   SysUtils,
   EB_Expressions, EditConditional;

{$R *.dfm}

{ TfrmEBEditComparison }

procedure TfrmEBEditComparisonInt.UploadObject(obj: TEbObject);
var
   comp: TEBComparison;
   left, right: TEBExpression;
begin
   comp := obj as TEBComparison;
   left := obj.children[0] as TEBExpression;
   right := obj.children[1] as TEBExpression;
   if left is TEBPropExpr then
   begin
      radLProperty.Checked := true;
      cboLProp.ItemIndex := cboLProp.Items.IndexOf(left.Text);
      assert(cboLProp.ItemIndex >= 0);
   end
   else assert(false);

   if right is TEBIntegerValue then
   begin
      radRValue.Checked := true;
      spnRValue.AsInteger := right.Values[0];
   end
   else if right is TEBPropExpr then
   begin
      radRProperty.Checked := true;
      cboRProp.ItemIndex := cboRProp.Items.IndexOf(left.Text);
      assert(cboRProp.ItemIndex >= 0);
   end
   else assert(false);

   grpOperation.ItemIndex := comp.Values[0];
end;

procedure TfrmEBEditComparisonInt.DownloadObject(obj: TEbObject);
var
   comp: TEBComparison;
   left, right: TEBExpression;
begin
   comp := obj as TEBComparison;
   obj.Clear;
   if radLProperty.Checked then
   begin
      assert(cboLProp.ItemIndex >= 0);
      left := TEBPropExpr.Create(cboLProp.Text);
   end
   else assert(false);
   comp.Add(left);

   if radRProperty.Checked then
      right := TEBIntegerValue.Create(spnRValue.AsInteger)
   else if radRVariable.Checked then
   begin
      assert(cboRProp.ItemIndex >= 0);
      right := TEBPropExpr.Create(cboRProp.Text);
   end
   else assert(false);
   comp.Add(right);

   comp.Values.Add(grpOperation.ItemIndex);
end;

procedure TfrmEBEditComparisonInt.FormDestroy(Sender: TObject);
begin
   FFunctionL.Free;
   inherited;
end;

function TfrmEBEditComparisonInt.NewClassType: TEbClass;
begin
   result := TEBComparison;
end;

procedure TfrmEBEditComparisonInt.btnLFuncClick(Sender: TObject);
var
   text: string;
begin
   if assigned(FFunctionL) then
      text := FFunctionL.Serialize
   else text := '';
   if TfrmConditionEdit.Edit(text, FContext, FSuffix + ' Func') then
   begin
      FreeAndNil(FFunctionL);
      if text = '' then
         txtLFunc.Text := ''
      else begin
         FFunctionL := TEBObject.Load(text) as TEBExpression;
         txtLFunc.Text := FFunctionL.GetNodeText;
      end;
   end;
end;

procedure TfrmEBEditComparisonInt.SetContext(const context, suffix: string);
var
   temp, parser: TStringList;
   sl: TStrings;
   i: integer;
begin
   FContext := context;
   FSuffix := suffix + ' Integers';
   parser := TStringList.Create;
   temp := TStringList.Create;
   try
      parser.CommaText := context;
      for i := 0 to parser.Count - 1 do
      begin
         sl := GetEBGlobalProps(parser[i] + FSuffix);
         if assigned(sl) then
            temp.AddStrings(sl);
      end;
      temp.Sort;
      cboLProp.Items.Assign(temp);
      cboRProp.Items.Assign(temp);
   finally
      parser.Free;
      temp.Free;
   end;
end;

initialization
   RegisterEbEditor(TEBComparison, TfrmEBEditComparisonInt);
finalization
   UnRegisterEbEditor(TEBComparison);
end.
