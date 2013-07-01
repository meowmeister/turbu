unit EditTranslucency;

interface

uses
  StdCtrls, Mask, JvExMask, JvSpin, Classes, Controls, ExtCtrls,
  EbEdit, EventBuilder;

type
   [EditorCategory('Characters', 'Change Party Translucency')]
   TfrmEBEditTranslucency = class(TfrmEbEditBase)
      StaticText1: TStaticText;
      spnTranslucency: TJvSpinEdit;
   protected
      procedure UploadObject(obj: TEbObject); override;
      procedure DownloadObject(obj: TEbObject); override;
      function NewClassType: TEbClass; override;
   end;

implementation
uses
   EB_Characters;

{$R *.dfm}

{ TfrmEBEditTranslucency }

procedure TfrmEBEditTranslucency.UploadObject(obj: TEbObject);
begin
   spnTranslucency.AsInteger := obj.values[0];
end;

procedure TfrmEBEditTranslucency.DownloadObject(obj: TEbObject);
begin
   obj.Clear;
   obj.values.Add(spnTranslucency.AsInteger);
end;

function TfrmEBEditTranslucency.NewClassType: TEbClass;
begin
   result := TEBTranslucency;
end;

initialization
   RegisterEbEditor(TEBTranslucency, TfrmEBEditTranslucency);
finalization
   UnRegisterEbEditor(TEBTranslucency);
end.
