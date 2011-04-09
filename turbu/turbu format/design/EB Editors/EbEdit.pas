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

unit EbEdit;

interface

uses
   SysUtils, Classes, Controls, Forms, StdCtrls, ExtCtrls, Generics.Collections,
   DB, JVSpin,
   EventBuilder, finalizer_hack, turbu_map_interface, variable_selector,
   IDLookupCombo;

type
   EditorCategoryAttribute = finalizer_hack.EditorCategoryAttribute;
   TfrmEBEditBase = class;
   TEbEditorClass = class of TfrmEBEditBase;

   TEditorDic = class(TDictionary<TEbClass, TEbEditorClass>);
   TIntPair = array[1..2] of integer;

   TfrmEBEditBase = class abstract(TForm)
      Panel1: TPanel;
      btnOK: TButton;
      btnCancel: TButton;
      btnHelp: TButton;
      procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
      procedure RadioButtonClick(Sender: TObject);
      procedure FormShow(Sender: TObject);
   private
      class function GetEditors: TEditorDic; static;
   protected
      function EditObj(obj: TEbObject): boolean; dynamic;
      procedure UploadObject(obj: TEbObject); virtual; abstract;
      procedure DownloadObject(obj: TEbObject); virtual; abstract;
      function NewClassType: TEbClass; virtual; abstract;
      function ValidateForm: boolean; dynamic;
      procedure ValidateError(control: TWinControl; const reason: string);
      function ContextLookup(const name: string): integer;

      class property AllEditors: TEditorDic read GetEditors;
   protected
      procedure UploadValuePtrSelection(v1, v2: integer; r1, r2: TRadioButton;
        valueBox: TJvSpinEdit; ptrBox: TIntSelector); overload;
      procedure UploadValuePtrSelection(expr: TEBExpression; r1, r2: TRadioButton;
        valueBox: TJvSpinEdit; ptrBox: TIntSelector); overload;
      procedure UploadLookupPtrSelection(expr: TEBExpression; r1, r2: TRadioButton;
        valueBox: TIDLookupCombo; ptrBox: TIntSelector);
      function DownloadValuePtrSelection(r1, r2: TRadioButton;
        valueBox: TJvSpinEdit; ptrBox: TIntSelector): TIntPair;
      function DownloadLookupPtrSelection(r1, r2: TRadioButton;
        valueBox: TIDLookupCombo; ptrBox: TIntSelector; const lookupName: string): TEBExpression;
      procedure EnableControl(control: TControl; controller: TRadioButton); overload;
      procedure EnableControl(control: TControl; controller: TCheckBox); overload;
      procedure EnableGroupBox(box: TGroupBox; value: boolean);
      procedure EnableControlsProperly; virtual;
   public
      procedure SetupMap(const map: IRpgMap); dynamic;
      function NewObj: TEbObject; dynamic;
      class function Category: EditorCategoryAttribute;
   end;

   procedure RegisterEbEditor(ebType: TEbClass; editor: TEbEditorClass);
   procedure UnregisterEbEditor(ebType: TEbClass);
   function NewEbObject(ebType: TEbClass; const context: IRpgMap): TEbObject;
   function EditEbObject(obj: TEbObject; const context: IRpgMap): boolean;

implementation
uses
   RTTI,
   rttiHelper, array_editor, EB_Expressions;

{$R *.dfm}

var
   dic: TEditorDic;

procedure RegisterEbEditor(ebType: TEbClass; editor: TEbEditorClass);
begin
   dic.Add(ebType, editor);
end;

procedure UnregisterEbEditor(ebType: TEbClass);
begin
   dic.Remove(ebType);
end;

function NewEbObject(ebType: TEbClass; const context: IRpgMap): TEbObject;
var
   editorCls: TEbEditorClass;
   editor: TfrmEBEditBase;
begin
   if not dic.TryGetValue(ebType, editorCls) then
      exit(nil);

   editor := editorCls.Create(nil);
   try
      editor.SetupMap(context);
      result := editor.NewObj;
   finally
      editor.Release;
   end;
end;

function EditEbObject(obj: TEbObject; const context: IRpgMap): boolean;
var
   editorCls: TEbEditorClass;
   editor: TfrmEBEditBase;
begin
   if not dic.TryGetValue(TEbClass(obj.classType), editorCls) then
      exit(false);

   editor := editorCls.Create(nil);
   try
      editor.SetupMap(context);
      result := editor.EditObj(obj);
   finally
      editor.Release;
   end;
end;

{ TfrmEBEditBase }

class function TfrmEBEditBase.Category: EditorCategoryAttribute;
begin
   result := TRttiContext.Create.GetType(self).GetAttribute(EditorCategoryAttribute) as EditorCategoryAttribute;
end;

function TfrmEBEditBase.ContextLookup(const name: string): integer;
begin
   result := -TfrmArrayEdit.VariableContext.Lookup('name', name, 'id');
end;

function TfrmEBEditBase.EditObj(obj: TEbObject): boolean;
begin
   UploadObject(obj);
   EnableControlsProperly;
   result := ShowModal = mrOK;
   if result then
      DownloadObject(obj);
end;

procedure TfrmEBEditBase.EnableControl(control: TControl; controller: TRadioButton);
begin
   control.Enabled := controller.Checked;
end;

procedure TfrmEBEditBase.EnableControl(control: TControl; controller: TCheckBox);
begin
   control.Enabled := controller.Checked;
end;

procedure TfrmEBEditBase.EnableControlsProperly;
begin
   //This method intentionally left blank
end;

procedure TfrmEBEditBase.EnableGroupBox(box: TGroupBox; value: boolean);
var
   i: integer;
begin
   for i := 0 to box.ControlCount - 1 do
      box.Controls[i].Enabled := value;
end;

procedure TfrmEBEditBase.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
   CanClose := self.ValidateForm;
end;

procedure TfrmEBEditBase.FormShow(Sender: TObject);
begin
   EnableControlsProperly;
end;

class function TfrmEBEditBase.GetEditors: TEditorDic;
begin
   result := dic;
end;

function TfrmEBEditBase.NewObj: TEbObject;
begin
   if self.ShowModal = mrOK then
   begin
      result := NewClassType.Create(nil);
      try
         DownloadObject(result);
      except
         result.free;
         raise;
      end;
   end
   else result := nil;
end;

procedure TfrmEBEditBase.SetupMap(const map: IRpgMap);
begin
  //this method intentionally left blank
end;

procedure TfrmEBEditBase.UploadLookupPtrSelection(expr: TEBExpression;
  r1, r2: TRadioButton; valueBox: TIDLookupCombo; ptrBox: TIntSelector);
begin
   if expr is TEBLookupValue then
   begin
      r1.Checked := true;
      valueBox.ID := expr.Values[0];
   end
   else begin
      assert(expr is TEBIntsValue);
      r2.Checked := true;
      ptrBox.ID := expr.Values[0];
   end;
end;

function TfrmEBEditBase.DownloadLookupPtrSelection(r1, r2: TRadioButton;
  valueBox: TIDLookupCombo; ptrBox: TIntSelector; const lookupName: string): TEBExpression;
begin
   assert(r1.Checked or r2.Checked);
   if r1.Checked then
      result := TEBLookupValue.Create(valueBox.id, lookupName)
   else result := TEBIntsValue.Create(ptrBox.id);
end;

procedure TfrmEBEditBase.UploadValuePtrSelection(v1, v2: integer; r1,
  r2: TRadioButton; valueBox: TJvSpinEdit; ptrBox: TIntSelector);
begin
   if v1 = 0 then
   begin
      r1.Checked := true;
      valueBox.AsInteger := v2;
   end
   else begin
      r2.Checked := true;
      ptrBox.ID := v2;
   end;
end;

procedure TfrmEBEditBase.UploadValuePtrSelection(expr: TEBExpression; r1,
  r2: TRadioButton; valueBox: TJvSpinEdit; ptrBox: TIntSelector);
var
   v1: integer;
begin
   assert(expr.values.count > 0);
   v1 := ord(expr.ClassType = TEBIntsValue);
   UploadValuePtrSelection(v1, expr.Values[0], r1, r2, valueBox, ptrBox);
end;

function TfrmEBEditBase.DownloadValuePtrSelection(r1, r2: TRadioButton;
  valueBox: TJvSpinEdit; ptrBox: TIntSelector): TIntPair;
begin
   assert(r1.Checked or r2.Checked);
   if r1.Checked then
   begin
      result[1] := 0;
      result[2] := valueBox.AsInteger;
   end
   else begin
      result [1] := 1;
      result [2] := ptrBox.ID;
   end;
end;

procedure TfrmEBEditBase.ValidateError(control: TWinControl; const reason: string);
begin
   assert(assigned(control));
   assert(GetParentForm(control) = self);
   assert(reason <> '');
   Application.MessageBox(PChar(reason), PChar(self.Caption));
   FocusControl(control);
   Abort;
end;

function TfrmEBEditBase.ValidateForm: boolean;
begin
   result := true;
end;

procedure TfrmEBEditBase.RadioButtonClick(Sender: TObject);
begin
   EnableControlsProperly;
end;

initialization
   dic := TEditorDic.Create;
finalization
   dic.Free;
end.
