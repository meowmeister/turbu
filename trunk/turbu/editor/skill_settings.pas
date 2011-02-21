unit skill_settings;
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

interface

uses
   SysUtils, Classes, Controls, Forms, StdCtrls, JvSpin, ExtCtrls, Mask,
   JvExMask, DB,
   turbu_skills, dm_database, DBCtrls, JvDBSpinEdit, DBClient;

type
   TfrmSkillLearning = class(TForm)
      pnlSillData: TPanel;
      grpData1: TGroupBox;
      grpData2: TGroupBox;
      grpAlgorithm: TGroupBox;
      grpSkill: TGroupBox;
      grpData3: TGroupBox;
      grpData4: TGroupBox;
      btnNewAlgorithm: TButton;
      btnEdit: TButton;
      btnOK: TButton;
      btnCancel: TButton;
      grpNull: TGroupBox;
      spnData1: TJvDBSpinEdit;
      spnData2: TJvDBSpinEdit;
      spnData3: TJvDBSpinEdit;
      spnData4: TJvDBSpinEdit;
      cbxSkill: TDBLookupComboBox;
      dsForeign: TDataSource;
    cbxAlgorithm: TDBLookupComboBox;
      procedure FormShow(Sender: TObject);
      procedure btnCancelClick(Sender: TObject);
   private
      FFunctionType: TSkillFuncStyle;
      FBookmark: integer;
      { Private declarations }
   public
      { Public declarations }
   end;

implementation

uses
   turbu_defs;

{$R *.dfm}

const
   RETURN_TYPES: array[TSkillFuncStyle] of string =
      ('boolean', 'integer', 'integer', 'TSkillProgress');

procedure TfrmSkillLearning.btnCancelClick(Sender: TObject);
begin
   (dsForeign.DataSet as TClientDataset).SavePoint := FBookmark;
end;

procedure TfrmSkillLearning.FormShow(Sender: TObject);
var
   dummy: integer;
   dset: TDataset;
begin
   dset := dsForeign.DataSet;
   FBookmark := (dset as TClientDataset).SavePoint;
   //ugly hack to make the lookup box display properly
   dset.Edit;
   dummy := dset.FieldByName('method.address').AsInteger;
   dset.FieldByName('method.address').AsInteger := 0;
   dset.FieldByName('method.address').AsInteger := dummy;
   dset.Post;
end;

end.
