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

unit EditMoney;

interface

uses
  Forms, StdCtrls, ExtCtrls, Classes, Controls, Mask,
  JvExMask, JvSpin,
  variable_selector,  EventBuilder, EbEdit, button_edit;

type
   [EditorCategory('Characters', 'Change Money')]
   TfrmEBEditMoney = class(TfrmEbEditBase)
      grpOperation: TRadioGroup;
      GroupBox1: TGroupBox;
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
   EB_Characters, EbEditHelper;

{$R *.dfm}

{ TfrmEBEditMoney }

function TfrmEBEditMoney.NewClassType: TEbClass;
begin
   result := TEBMoney;
end;

procedure TfrmEBEditMoney.DownloadObject(obj: TEbObject);
begin
   obj.Values.Clear;
   obj.Values.Add(grpOperation.ItemIndex);
   if radExactAmount.Checked then
   begin
      obj.Values.Add(0);
      obj.Values.Add(spnExactValue.AsInteger);
   end
   else begin
      obj.Values.Add(1);
      obj.Values.Add(selValue.ID);
   end;
end;

procedure TfrmEBEditMoney.EnableControlsProperly;
begin
   spnExactValue.Enabled := radExactAmount.Checked;
   selValue.Enabled := radPointer.Checked;
end;

procedure TfrmEBEditMoney.UploadObject(obj: TEbObject);
begin
   grpOperation.ItemIndex := obj.Values[0];
   UploadValuePtrSelection(obj.Values[1], obj.Values[2], radExactAmount, radPointer,
     spnExactValue, selValue);
end;

initialization
   RegisterEbEditor(TEBMoney, TfrmEBEditMoney);
finalization
   UnRegisterEbEditor(TEBMoney);
end.
