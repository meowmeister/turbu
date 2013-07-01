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

unit EditComparisonBool;

interface

uses
   Forms, StdCtrls, ExtCtrls, Controls, Classes,
   EbEdit, EventBuilder;

type
   [EditorCategory('Basics', 'Comparison (Boolean)')]
   [EditorContext('Universal Cond')]
   TfrmEBEditComparisonBool = class(TfrmEbEditBase, IContextualEditor)
      Panel2: TPanel;
      radLVariable: TRadioButton;
      radLProperty: TRadioButton;
      cboLVar: TComboBox;
      radLFunction: TRadioButton;
      txtLFunc: TEdit;
      btnLFunc: TButton;
      grpOperation: TRadioGroup;
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

{ TfrmEBEditComparisonBool }

procedure TfrmEBEditComparisonBool.UploadObject(obj: TEbObject);
var
   left: TEBExpression;
begin
   left := obj.children[0] as TEBExpression;
   if left is TEBVariableValue then
   begin
      radLVariable.Checked := true;
      cboLVar.ItemIndex := cboLVar.Items.IndexOf(left.Text);
      assert(cboLVar.ItemIndex >= 0);
   end
   else assert(false);

   grpOperation.ItemIndex := obj.Values[0];
end;

procedure TfrmEBEditComparisonBool.btnLFuncClick(Sender: TObject);
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

procedure TfrmEBEditComparisonBool.DownloadObject(obj: TEbObject);
var
   left: TEBExpression;
begin
   obj.Clear;
   if radLVariable.Checked then
   begin
      assert(cboLVar.ItemIndex >= 0);
      left := TEBVariableValue.Create(cboLVar.Text);
   end
   else if radLFunction.Checked then
      left := FFunctionL.Clone as TEBExpression
   else assert(false);
   obj.Add(left);

   obj.Values.Add(grpOperation.ItemIndex);
end;

procedure TfrmEBEditComparisonBool.FormDestroy(Sender: TObject);
begin
   FFunctionL.Free;
   inherited;
end;

function TfrmEBEditComparisonBool.NewClassType: TEbClass;
begin
   result := TEBComparisonBool;
end;

procedure TfrmEBEditComparisonBool.SetContext(const context, suffix: string);
var
   temp, parser: TStringList;
   sl: TStrings;
   i: integer;
begin
   FContext := context;
   FSuffix := suffix + ' Booleans';
   parser := TStringList.Create;
   temp := TStringList.Create;
   try
      parser.CommaText := context;
      for i := 0 to parser.Count - 1 do
      begin
         sl := GetEBGlobals(parser[i] + FSuffix);
         if assigned(sl) then
            temp.AddStrings(sl);
      end;
      temp.Sort;
      cboLVar.Items.Assign(temp);
   finally
      parser.Free;
      temp.Free;
   end;
end;

initialization
   RegisterEbEditor(TEBComparisonBool, TfrmEBEditComparisonBool);
finalization
   UnRegisterEbEditor(TEBComparisonBool);
end.