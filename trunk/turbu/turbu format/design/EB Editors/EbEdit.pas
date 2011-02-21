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
   DB,
   EventBuilder, finalizer_hack, turbu_map_interface;

type
   EditorCategoryAttribute = finalizer_hack.EditorCategoryAttribute;
   TfrmEBEditBase = class;
   TEbEditorClass = class of TfrmEBEditBase;

   TEditorDic = class(TDictionary<TEbClass, TEbEditorClass>);

   TfrmEBEditBase = class abstract(TForm)
      Panel1: TPanel;
      btnOK: TButton;
      btnCancel: TButton;
      btnHelp: TButton;
      procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
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
   rttiHelper, array_editor;

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
   result := ShowModal = mrOK;
   if result then
      DownloadObject(obj);
end;

procedure TfrmEBEditBase.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
   CanClose := self.ValidateForm;
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

initialization
   dic := TEditorDic.Create;
finalization
   dic.Free;
end.
