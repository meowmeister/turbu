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
  variable_selector,  EventBuilder, EbEdit;

type
   [EditorCategory('Characters', 'Change Money', 0)]
   TfrmEBEditMoney = class(TfrmEbEditBase)
      grpOperation: TRadioGroup;
      GroupBox1: TGroupBox;
      radExactAmount: TRadioButton;
      radPointer: TRadioButton;
      spnExactValue: TJvSpinEdit;
      selValue: TIntSelector;
      procedure FormShow(Sender: TObject);
      procedure OnRadClick(Sender: TObject);
   private
      procedure EnableControlsProperly;
   protected
      procedure UploadObject(obj: TEbObject); override;
      procedure DownloadObject(obj: TEbObject); override;
      function NewClassType: TEbClass; override;
   end;

implementation
uses
   EB_Characters;

{$R *.dfm}

{ TfrmEBEditMoney }

function TfrmEBEditMoney.NewClassType: TEbClass;
begin
   result := TEBMoney;
end;

procedure TfrmEBEditMoney.OnRadClick(Sender: TObject);
begin
   EnableControlsProperly;
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

procedure TfrmEBEditMoney.FormShow(Sender: TObject);
begin
   EnableControlsProperly;
end;

procedure TfrmEBEditMoney.UploadObject(obj: TEbObject);
begin
   grpOperation.ItemIndex := obj.Values[0];
   if obj.Values[1] = 0 then
   begin
      radExactAmount.Checked := true;
      spnExactValue.AsInteger := obj.Values[2];
   end
   else begin
      radPointer.Checked := true;
      selValue.ID := obj.Values[2];
   end;
   EnableControlsProperly;
end;

initialization
   RegisterEbEditor(TEBMoney, TfrmEBEditMoney);
end.
