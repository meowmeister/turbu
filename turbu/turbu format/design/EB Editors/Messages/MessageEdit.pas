unit MessageEdit;

interface

uses
  SysUtils, Classes, Controls, Forms, StdCtrls, ExtCtrls,
  EbEdit, EventBuilder;

type
   [EditorCategory('Messages', 'Show Message', 0)]
   TfrmMessageEdit = class(TfrmEbEditBase)
      Memo1: TMemo;
   private
      procedure AddExtensions(obj: TEbObject);
   protected
      procedure UploadObject(obj: TEbObject); override;
      procedure DownloadObject(obj: TEbObject); override;
      function NewClassType: TEbClass; override;
   end;

implementation
uses
   Generics.Collections,
   EB_Messages, EB_RpgScript;

{$R *.dfm}

{ TfrmMessageEdit }

procedure TfrmMessageEdit.AddExtensions(obj: TEbObject);
var
   i: integer;
   ext: TEbExtension;
begin
   for I := 1 to Memo1.Lines.Count - 1 do
   begin
      ext := TEbExtension.Create(obj);
      ext.Text := memo1.Lines[i];
   end;
end;

function TfrmMessageEdit.NewClassType: TEbClass;
begin
   result := TEBShowMessage;
end;

procedure TfrmMessageEdit.UploadObject(obj: TEbObject);
var
   sub: TEbObject;
begin
   Memo1.Lines.add(obj.Text);
   for sub in obj do
      Memo1.Lines.Add((sub as TEbExtension).Text);
end;

procedure TfrmMessageEdit.DownloadObject(obj: TEbObject);
begin
   while obj.ComponentCount > 0 do
      obj.RemoveComponent(obj.components[0]);
   if Memo1.lines.count > 0 then
   begin
      obj.Text := Memo1.Lines[0];
      AddExtensions(obj);
   end;
end;

initialization
   RegisterEbEditor(TEbShowMessage, TfrmMessageEdit);

end.