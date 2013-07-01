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

unit EditSkills;

interface

uses
   ExtCtrls, DB, StdCtrls, DBCtrls, Classes, Controls, Mask, JvExMask, JvSpin,
   EBEdit, ebPartyBase, variable_selector, EventBuilder, IDLookupCombo, dm_database,
   turbu_variable_selector, button_edit;

type
   [EditorCategory('Characters', 'Change Skills')]
   [EditorContext('RM2K')]
   TfrmEBEditSkills = class(TfrmEBPartyBase)
      grpOperation: TRadioGroup;
      GroupBox2: TGroupBox;
      cboSkillID: TIDLookupCombo;
      radSpecificItem: TRadioButton;
      radItemPtr: TRadioButton;
      selSkillID: TIntSelector;
      srcSkills: TDataSource;
   protected
      procedure UploadObject(obj: TEbObject); override;
      procedure DownloadObject(obj: TEbObject); override;
      function NewClassType: TEbClass; override;
      procedure EnableControlsProperly; override;
   end;

implementation
uses
   EB_Characters, EB_Expressions, EB_Expressions_RM;

{$R *.dfm}

{ TfrmEBEditSkills }

procedure TfrmEBEditSkills.DownloadObject(obj: TEbObject);
var
   skill: TEBLookupObjExpr;
begin
   inherited DownloadObject(obj);
   obj.Values.Add(grpOperation.ItemIndex);
   if radItemPtr.Checked then
      skill := TEBLookupObjExpr.Create('skill', TEBIntsValue.Create(selSkillID.ID), 'skills')
   else skill := TEBLookupObjExpr.Create('skill', cboSkillID.id, 'skills');
   obj.Add(skill);
end;

procedure TfrmEBEditSkills.UploadObject(obj: TEbObject);
var
   skill: TEBLookupObjExpr;
begin
   inherited UploadObject(Obj);
   grpOperation.ItemIndex := obj.Values[0];
   skill := obj.Children[1] as TEBLookupObjExpr;
   if skill.Values.Count > 0 then
   begin
      radSpecificItem.Checked := true;
      cboSkillID.id := skill.Values[0];
   end
   else begin
      radItemPtr.Checked := true;
      selSkillID.ID := (skill.Children[0] as TEBIntsValue).Values[0];
   end;
end;

procedure TfrmEBEditSkills.EnableControlsProperly;
begin
   inherited EnableControlsProperly;
   EnableControl(cboSkillID, radSpecificItem);
   EnableControl(selSkillID, radItemPtr);
end;

function TfrmEBEditSkills.NewClassType: TEbClass;
begin
   result := TEBSkills;
end;

initialization
   RegisterEbEditor(TEBSkills, TfrmEBEditSkills);
finalization
   UnRegisterEbEditor(TEBSkills);
end.
