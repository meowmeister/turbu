unit EditMP;

interface

uses
  DBClient, Classes, Controls, ExtCtrls, DB, StdCtrls, DBCtrls,  Mask,
  JvExMask, JvSpin,
  EventBuilder, variable_selector, ebPartyBase, IDLookupCombo, EBEdit,
  button_edit;

type
   [EditorCategory('Characters', 'Change MP')]
   TfrmEBEditMP = class(TfrmEBPartyBase)
      grpOperation: TRadioGroup;
      grpItemCount: TGroupBox;
      radExactAmount: TRadioButton;
      radPointer: TRadioButton;
      spnExactValue: TJvSpinEdit;
      selValue: TIntSelector;
   protected
      procedure UploadObject(obj: TEbObject); override;
      procedure DownloadObject(obj: TEbObject); override;
      function NewClassType: TEbClass; override;
      procedure EnableControlsProperly; override;
   end;

implementation
uses
   EB_Characters, EB_Expressions, EbEditHelper, EB_Expressions_RM;

{$R *.dfm}

{ TfrmEBEditMP }

procedure TfrmEBEditMP.UploadObject(obj: TEbObject);
begin
   inherited UploadObject(obj);
   grpOperation.ItemIndex := obj.Values[0];
   UploadValuePtrSelection(obj.Children[1] as TEBExpression, radExactAmount,
                           radPointer, spnExactValue, selValue);
end;

procedure TfrmEBEditMP.DownloadObject(obj: TEbObject);
var
   value: TEBExpression;
begin
   inherited DownloadObject(obj);
   obj.Values.Add(grpOperation.ItemIndex);
   if radExactAmount.Checked then
      value := TEBIntegerValue.Create(spnExactValue.AsInteger)
   else value := TEBIntsValue.Create(selValue.ID);
   obj.Add(value);
end;

procedure TfrmEBEditMP.EnableControlsProperly;
begin
   inherited EnableControlsProperly;
   EnableControl(spnExactValue, radExactAmount);
   EnableControl(selValue, radPointer);
end;

function TfrmEBEditMP.NewClassType: TEbClass;
begin
   result := TEBChangeMP;
end;

initialization
   RegisterEbEditor(TEBChangeMP, TfrmEBEditMP);
finalization
   UnRegisterEbEditor(TEBChangeMP);
end.
