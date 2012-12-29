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

unit EditTakeDamage;

interface

uses
   StdCtrls, DBCtrls, Classes, Controls, ExtCtrls, Mask, JvExMask, JvSpin, DB,
   ebEdit, variable_selector, IDLookupCombo, EventBuilder, dm_database,
   turbu_variable_selector, button_edit;

type
   [EditorCategory('Characters', 'Take Damage')]
   [EditorContext('RM2K')]
   TfrmEBEditTakeDamage = class(TfrmEBEditBase)
      grpCharacter: TGroupBox;
      cboHeroID: TIDLookupCombo;
      radSpecificHero: TRadioButton;
      radHeroPtr: TRadioButton;
      selHeroID: TIntSelector;
      radAllParty: TRadioButton;
      spnPower: TJvSpinEdit;
      spnRandomness: TJvSpinEdit;
      spnDef: TJvSpinEdit;
      txtPhysDefense: TStaticText;
      spnMDef: TJvSpinEdit;
      chkAssign: TCheckBox;
      selAssign: TIntSelector;
      srcHeroes: TDataSource;
   protected
      procedure UploadObject(obj: TEbObject); override;
      procedure DownloadObject(obj: TEbObject); override;
      function NewClassType: TEbClass; override;
      procedure EnableControlsProperly; override;
  end;

implementation
uses
   EB_Characters;

{$R *.dfm}

{ TfrmEBEditTakeDamage }

procedure TfrmEBEditTakeDamage.UploadObject(obj: TEbObject);
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
         selHeroID.ID := obj.Values[1];
      end
      else assert(false);
   end;
   spnPower.AsInteger := obj.Values[2];
   spnDef.AsInteger := obj.Values[3];
   spnMDef.AsInteger := obj.Values[4];
   spnRandomness.AsInteger := obj.Values[5];
   chkAssign.checked := boolean(obj.Values[6]);
   selAssign.ID := obj.Values[7];
end;

procedure TfrmEBEditTakeDamage.DownloadObject(obj: TEbObject);
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
      obj.Values.Add(selHeroID.ID);
   end;
   obj.Values.Add(spnPower.AsInteger);
   obj.Values.Add(spnDef.AsInteger);
   obj.Values.Add(spnMDef.AsInteger);
   obj.Values.Add(spnRandomness.AsInteger);
   obj.Values.Add(ord(chkAssign.checked));
   obj.Values.Add(selAssign.ID);
end;

procedure TfrmEBEditTakeDamage.EnableControlsProperly;
begin
   EnableControl(cboHeroID, radSpecificHero);
   EnableControl(selHeroID, radHeroPtr);
   EnableControl(selAssign, chkAssign);
end;

function TfrmEBEditTakeDamage.NewClassType: TEbClass;
begin
   result := TEBTakeDamage;
end;

initialization
   RegisterEbEditor(TEBTakeDamage, TfrmEBEditTakeDamage);
finalization
   UnRegisterEbEditor(TEBTakeDamage);
end.
