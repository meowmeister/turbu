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

unit EditExp;

interface

uses
   StdCtrls, Classes, Controls, ExtCtrls, DB, DBCtrls, Mask,
   JvExMask, JvSpin,
   EventBuilder, EbEdit, dm_database, variable_selector, IDLookupCombo,
   turbu_variable_selector, button_edit;

type
   [EditorCategory('Characters', 'Change Experience')]
   [EditorContext('RM2K')]
   TfrmEBEditExp = class(TfrmEbEditBase)
      cboHeroID: TIDLookupCombo;
      radSpecificHero: TRadioButton;
      radHeroPtr: TRadioButton;
      selItemID: TIntSelector;
      srcHeroes: TDataSource;
      radAllParty: TRadioButton;
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

   [EditorCategory('Characters', 'Change Level')]
   TFrmEBEditLevel = class(TfrmEBEditExp)
      procedure FormShow(Sender: TObject);
   protected
      function NewClassType: TEbClass; override;
   end;

implementation
uses
   EB_Characters, EbEditHelper;

{$R *.dfm}

{ TfrmEBEditExp }

procedure TfrmEBEditExp.EnableControlsProperly;
begin
   EnableControl(cboHeroID, radSpecificHero);
   EnableControl(selItemID, radHeroPtr);
   EnableControl(spnExactValue, radExactAmount);
   EnableControl(selValue, radPointer);
   chkLevelMessage.Enabled := grpOperation.ItemIndex = 0;
end;

function TfrmEBEditExp.NewClassType: TEbClass;
begin
   result := TEBExperience;
end;

procedure TfrmEBEditExp.UploadObject(obj: TEbObject);
begin
   case obj.Values[0] of
      0: radAllParty.Checked := true;
      1:
      begin
         radSpecificHero.Checked := true;
         cboHeroID.id := obj.Values[1];
      end;
      2:
      begin
         radHeroPtr.Checked := true;
         selItemID.ID := obj.Values[1];
      end
      else assert(false);
   end;
   grpOperation.ItemIndex := obj.Values[2];
   UploadValuePtrSelection(obj.Values[3], obj.Values[4], radExactAmount, radPointer, spnExactValue, selValue);
   chkLevelMessage.Checked := boolean(obj.Values[5]);
end;

procedure TfrmEBEditExp.DownloadObject(obj: TEbObject);
var
   pair: TIntPair;
begin
   obj.Clear;
   if radAllParty.Checked then
      obj.Values.AddRange([0, 0])
   else if radSpecificHero.Checked then
   begin
      obj.Values.Add(1);
      obj.values.Add(cboHeroID.id);
   end
   else begin
      obj.Values.Add(2);
      obj.Values.Add(selItemID.ID);
   end;
   obj.Values.Add(grpOperation.ItemIndex);
   pair := DownloadValuePtrSelection(radExactAmount, radPointer, spnExactValue, selValue);
   obj.Values.AddRange(pair);
   obj.Values.Add(ord(chkLevelMessage.Checked));
end;

{ TFrmEBEditLevel }

procedure TFrmEBEditLevel.FormShow(Sender: TObject);
begin
   inherited FormShow(sender);
   self.Caption := 'Change Level';
   spnExactValue.MaxValue := 99;
   grpOperation.Items[0] := 'Increase Level';
   grpOperation.Items[1] := 'Decrease Level';
end;

function TFrmEBEditLevel.NewClassType: TEbClass;
begin
   result := TEBLevel;
end;

initialization
   RegisterEbEditor(TEBExperience, TfrmEBEditExp);
   RegisterEbEditor(TEBLevel, TfrmEBEditLevel);
finalization
   UnRegisterEbEditor(TEBExperience);
   UnRegisterEbEditor(TEBLevel);
end.
