unit EditDeleteObject;

interface

uses
  StdCtrls, Classes, Controls, Forms, ExtCtrls,
  EbEdit, EventBuilder;

type
   [EditorCategory('Basics', 'Delete Map Object', 4)]
   TfrmEBDeleteObject = class(TfrmEbEditBase)
      radDuration: TRadioGroup;
   public
      procedure UploadObject(obj: TEbObject); override;
      procedure DownloadObject(obj: TEbObject); override;
      function NewClassType: TEbClass; override;
   end;

implementation
uses
   EB_System;

{$R *.dfm}

{ TfrmEBDeleteEvent }

procedure TfrmEBDeleteObject.DownloadObject(obj: TEbObject);
begin
   obj.Values.Clear;
   obj.Values.Add(radDuration.ItemIndex);
end;

function TfrmEBDeleteObject.NewClassType: TEbClass;
begin
   result := TEBDeleteObj;
end;

procedure TfrmEBDeleteObject.UploadObject(obj: TEbObject);
begin
   radDuration.ItemIndex := obj.values[0];
end;

initialization
   RegisterEbEditor(TEBDeleteObj, TfrmEBDeleteObject);
end.
