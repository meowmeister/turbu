unit EditSkills;

interface

uses
   ExtCtrls, DB, StdCtrls, DBCtrls, Classes, Controls, Mask, JvExMask, JvSpin,
   EBEdit, ebPartyBase, variable_selector, EventBuilder, IDLookupCombo, dm_database;

type
   [EditorCategory('Characters', 'Change Skills')]
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
   EB_Characters, EB_Expressions;

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
   skill := obj.Components[1] as TEBLookupObjExpr;
   if skill.Values.Count > 0 then
   begin
      radSpecificItem.Checked := true;
      cboSkillID.id := skill.Values[0];
   end
   else begin
      radItemPtr.Checked := true;
      selSkillID.ID := (skill.Components[0] as TEBIntsValue).Values[0];
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
