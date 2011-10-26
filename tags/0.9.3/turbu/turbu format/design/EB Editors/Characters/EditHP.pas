unit EditHP;

interface

uses
  DBClient, Classes, Controls, ExtCtrls, DB, StdCtrls, DBCtrls,  Mask,
  JvExMask, JvSpin,
  EventBuilder, variable_selector, ebPartyBase, IDLookupCombo, EBEdit,
  button_edit;

type
   [EditorCategory('Characters', 'Change HP')]
   TfrmEBEditHP = class(TfrmEBPartyBase)
      grpOperation: TRadioGroup;
      grpItemCount: TGroupBox;
      radExactAmount: TRadioButton;
      radPointer: TRadioButton;
      spnExactValue: TJvSpinEdit;
      selValue: TIntSelector;
      chkLevelMessage: TCheckBox;
   protected
      procedure UploadObject(obj: TEbObject); override;
      procedure DownloadObject(obj: TEbObject); override;
      function NewClassType: TEbClass; override;
      procedure EnableControlsProperly; override;
   end;

implementation
uses
   EB_Characters, EB_Expressions;

{$R *.dfm}

{ TfrmEBEditHP }

procedure TfrmEBEditHP.UploadObject(obj: TEbObject);
begin
   inherited UploadObject(obj);
   grpOperation.ItemIndex := obj.Values[0];
   chkLevelMessage.Checked := boolean(obj.Values[1]);
   UploadValuePtrSelection(obj.Children[1] as TEBExpression, radExactAmount,
                           radPointer, spnExactValue, selValue);
end;

procedure TfrmEBEditHP.DownloadObject(obj: TEbObject);
var
   value: TEBExpression;
begin
   inherited DownloadObject(obj);
   obj.Values.Add(grpOperation.ItemIndex);
   obj.Values.Add(ord(chkLevelMessage.Checked));
   if radExactAmount.Checked then
      value := TEBIntegerValue.Create(spnExactValue.AsInteger)
   else value := TEBIntsValue.Create(selValue.ID);
   obj.Add(value);
end;

procedure TfrmEBEditHP.EnableControlsProperly;
begin
   inherited EnableControlsProperly;
   chkLevelMessage.Enabled := grpOperation.ItemIndex = 1;
   EnableControl(spnExactValue, radExactAmount);
   EnableControl(selValue, radPointer);
end;

function TfrmEBEditHP.NewClassType: TEbClass;
begin
   result := TEBChangeHP;
end;

initialization
   RegisterEbEditor(TEBChangeHP, TfrmEBEditHP);
finalization
   UnRegisterEbEditor(TEBChangeHP);
end.
