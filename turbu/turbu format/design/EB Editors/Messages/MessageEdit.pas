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

unit MessageEdit;

interface

uses
  SysUtils, Classes, Controls, Forms, StdCtrls, ExtCtrls,
  EbEdit, EventBuilder;

type
   [EditorCategory('Messages', 'Show Message')]
   [EditorContext('RM2K')]
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
   while obj.ChildCount > 0 do
      obj.Children.Extract(obj.children[0]);
   if Memo1.lines.count > 0 then
   begin
      obj.Text := Memo1.Lines[0];
      AddExtensions(obj);
   end;
end;

initialization
   RegisterEbEditor(TEbShowMessage, TfrmMessageEdit);
finalization
   UnRegisterEbEditor(TEbShowMessage);
end.
