unit EditInput;

interface

uses
  SysUtils, Classes, Controls, Forms, StdCtrls, ExtCtrls, Mask,
  EventBuilder, EbEdit, variable_selector;

type
   [EditorCategory('Basics', 'Direct Input', 3)]
   TfrmInputEdit = class(TfrmEbEditBase)
   protected
      procedure UploadObject(obj: TEbObject); override;
      procedure DownloadObject(obj: TEbObject); override;
      function NewClassType: TEbClass; override;
   end;

var
  frmInputEdit: TfrmInputEdit;

implementation
uses
   EB_System;

{$R *.dfm}

{ TfrmInputEdit }

procedure TfrmInputEdit.DownloadObject(obj: TEbObject);
begin
   obj.Values.Clear;
end;

procedure TfrmInputEdit.UploadObject(obj: TEbObject);
begin
end;

function TfrmInputEdit.NewClassType: TEbClass;
begin
   result := TEBInput;
end;

{initialization
   RegisterEbEditor(TEBInput, TfrmInputEdit);}
end.
