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
   DB, JVSpin, DBClient,
   EventBuilder, finalizer_hack, turbu_map_interface,
   IDLookupCombo, EB_Expressions;

type
   EditorCategoryAttribute = finalizer_hack.EditorCategoryAttribute;
   EditorContextAttribute = finalizer_hack.EditorContextAttribute;
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
      procedure EnableControlsProperly; virtual;
      function ValidateForm: boolean; dynamic;
      procedure ValidateError(control: TWinControl; const reason: string);
      function ContextLookup(const name: string): integer;

      class property AllEditors: TEditorDic read GetEditors;
   protected
      procedure UploadObjectRef(base: TEBObjExpr; box: TComboBox);
      function DownloadObjectRef(box: TComboBox): TEBObjExpr;
      procedure EnableControl(control: TControl; controller: TRadioButton); overload;
      procedure EnableControl(control: TControl; controller: TCheckBox); overload;
      procedure EnableGroupBox(box: TGroupBox; value: boolean);
      procedure PrepareNewObject(obj: TEBObject); dynamic;
   public
      procedure SetupMap(const map: IRpgMap); dynamic;
      procedure SetupEvent(const obj: IRpgMapObject); dynamic;
      function NewObj: TEbObject; dynamic;
      class function Category: EditorCategoryAttribute;
      class function Context: EditorContextAttribute;
   end;

   IContextualEditor = interface
   ['{13A5C344-D6F7-4130-90F2-8490D3F4E0EA}']
      procedure SetContext(const context, suffix: string);
   end;

   IVariableEditor = interface
   ['{A41D2914-5536-4E8A-9134-227E76196C27}']
      procedure SetVariables(locals, globals: TCustomClientDataset);
   end;

   procedure RegisterEbEditor(ebType: TEbClass; editor: TEbEditorClass);
   procedure UnregisterEbEditor(ebType: TEbClass);
   procedure RegisterEBGlobalProp(const category, name: string);
   function GetEBGlobalProps(const category: string): TStringList;
   function EditEbObject(obj: TEbObject; const context: IRpgMap; const mapObj: IRpgMapObject;
      const category, suffix: string; locals, globals: TCustomClientDataset): boolean;

implementation
uses
   RTTI, StrUtils,
   rttiHelper, array_editor;

{$R *.dfm}

var
   dic: TEditorDic;
   properties: TObjectDictionary<string, TStringList>;

procedure RegisterEbEditor(ebType: TEbClass; editor: TEbEditorClass);
begin
   dic.Add(ebType, editor);
end;

procedure UnregisterEbEditor(ebType: TEbClass);
begin
   dic.Remove(ebType);
end;

procedure RegisterEBGlobalProp(const category, name: string);
var
   sl: TStringList;
begin
   if not properties.TryGetValue(category, sl) then
   begin
      sl := TStringList.Create;
      sl.Duplicates := dupError;
      sl.Sorted := true;
      properties.Add(category, sl);
   end;
   sl.Add(name);
end;

function GetEBGlobalProps(const category: string): TStringList;
begin
   properties.TryGetValue(category, result);
end;

function EditEbObject(obj: TEbObject; const context: IRpgMap; const mapObj: IRpgMapObject;
   const category, suffix: string; locals, globals: TCustomClientDataset): boolean;
var
   editorCls: TEbEditorClass;
   editor: TfrmEBEditBase;
   cxe: IContextualEditor;
   vars: IVariableEditor;
begin
   if not dic.TryGetValue(TEbClass(obj.classType), editorCls) then
      exit(false);

   editor := editorCls.Create(nil);
   try
      editor.SetupMap(context);
      editor.SetupEvent(mapObj);
      if supports(editor, IContextualEditor, cxe) then
         cxe.SetContext(category, suffix);
      if supports(editor, IVariableEditor, vars) then
         vars.SetVariables(locals, globals);
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

class function TfrmEBEditBase.Context: EditorContextAttribute;
begin
   result := TRttiContext.Create.GetType(self).GetAttribute(EditorContextAttribute) as EditorContextAttribute;
end;

function TfrmEBEditBase.ContextLookup(const name: string): integer;
begin
//   result := -TfrmArrayEdit.VariableContext.Lookup('name', name, 'id');
result := -1;
{$MESSAGE WARN 'TfrmEBEditBase.ContextLookup is broken, please fix it!'}
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
      PrepareNewObject(result);
      try
         DownloadObject(result);
      except
         result.free;
         raise;
      end;
   end
   else result := nil;
end;

procedure TfrmEBEditBase.PrepareNewObject(obj: TEBObject);
begin
  //this method intentionally left blank
end;

procedure TfrmEBEditBase.SetupEvent(const obj: IRpgMapObject);
begin
  //this method intentionally left blank
end;

procedure TfrmEBEditBase.SetupMap(const map: IRpgMap);
begin
  //this method intentionally left blank
end;

procedure TfrmEBEditBase.UploadObjectRef(base: TEBObjExpr; box: TComboBox);
begin
   if base.Text = 'ThisObject' then
      box.ItemIndex := 0
   else begin
      assert(base.Text = 'MapObject');
      box.ItemIndex := base.Values[0];
   end;
end;

function TfrmEBEditBase.DownloadObjectRef(box: TComboBox): TEBObjExpr;
begin
   if box.ItemIndex = 0 then
      result := TEBObjExpr.Create('ThisObject')
   else result := TEBObjArrayValue.Create('MapObject', box.ItemIndex);
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
   properties := TObjectDictionary<string, TStringList>.Create([doOwnsValues]);
finalization
   dic.Free;
   properties.Free;
end.
