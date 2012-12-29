{ *****************************************************************************
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
  ***************************************************************************** }

unit EditHP;

interface

uses
   DBClient, Classes, Controls, ExtCtrls, DB, StdCtrls, DBCtrls,  Mask,
   JvExMask, JvSpin,
   EventBuilder, variable_selector, ebPartyBase, IDLookupCombo, EBEdit,
   turbu_variable_selector, button_edit;

type
   [EditorCategory('Characters', 'Change HP')]
   [EditorContext('RM2K')]
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
   EB_Characters, EB_Expressions, EbEditHelper, EB_Expressions_RM;

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
