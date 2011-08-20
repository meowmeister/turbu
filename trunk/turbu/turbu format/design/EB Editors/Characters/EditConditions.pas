unit EditConditions;

interface

uses
   DBClient, Classes, Controls, ExtCtrls, DB, StdCtrls, DBCtrls,
   EventBuilder, variable_selector, ebPartyBase, IDLookupCombo, EBEdit,
   dm_database, button_edit;

type
   TfrmEBEditConditions = class(TfrmEBPartyBase)
      grpOperation: TRadioGroup;
      StaticText1: TStaticText;
      cboCondition: TIDLookupCombo;
      srcConditions: TDataSource;
   protected
      procedure UploadObject(obj: TEbObject); override;
      procedure DownloadObject(obj: TEbObject); override;
      function NewClassType: TEbClass; override;
   end;

implementation
uses
   EB_Characters, EB_Expressions;

{$R *.dfm}

{ TfrmEBEditConditions }

procedure TfrmEBEditConditions.UploadObject(obj: TEbObject);
begin
   inherited UploadObject(obj);
   grpOperation.ItemIndex := obj.Values[0];
   cboCondition.id := (obj.Children[1] as TEBLookupValue).values[0];
end;

procedure TfrmEBEditConditions.DownloadObject(obj: TEbObject);
begin
   inherited DownloadObject(obj);
   obj.Values.Add(grpOperation.ItemIndex);
   obj.Add(TEBLookupValue.Create(cboCondition.id, 'conditions'));
end;

function TfrmEBEditConditions.NewClassType: TEbClass;
begin
   result := TEBChangeStatus;
end;

initialization
   RegisterEbEditor(TEBChangeStatus, TfrmEBEditConditions);
finalization
   UnRegisterEbEditor(TEBChangeStatus);
end.
