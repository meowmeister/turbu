unit EbEdit;

interface

uses
   SysUtils, Classes, Controls, Forms, StdCtrls, ExtCtrls, Generics.Collections,
   DB,
   EventBuilder, finalizer_hack;

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
      function NewObj: TEbObject; dynamic;
      class function Category: EditorCategoryAttribute;
   end;

   procedure RegisterEbEditor(ebType: TEbClass; editor: TEbEditorClass);
   procedure UnregisterEbEditor(ebType: TEbClass);
   function NewEbObject(ebType: TEbClass): TEbObject;
   function EditEbObject(obj: TEbObject): boolean;

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

function NewEbObject(ebType: TEbClass): TEbObject;
var
   editorCls: TEbEditorClass;
   editor: TfrmEBEditBase;
begin
   if not dic.TryGetValue(ebType, editorCls) then
      exit(nil);

   editor := editorCls.Create(nil);
   try
      result := editor.NewObj;
   finally
      editor.Release;
   end;
end;

function EditEbObject(obj: TEbObject): boolean;
var
   editorCls: TEbEditorClass;
   editor: TfrmEBEditBase;
begin
   if not dic.TryGetValue(TEbClass(obj.classType), editorCls) then
      exit(false);

   editor := editorCls.Create(nil);
   try
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
