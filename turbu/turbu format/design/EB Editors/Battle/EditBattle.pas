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

unit EditBattle;

interface

uses
  Dialogs, StdCtrls, DBCtrls, ExtCtrls, DB, Classes, Controls,
  EbEdit, EventBuilder, variable_selector, IDLookupCombo, pic_edit, button_edit;

type
   [EditorCategory('Battles', 'Enter Battle (Simple)')]
   TfrmBattle = class(TfrmEbEditBase)
      grpMParty: TGroupBox;
      cboMpartyID: TIDLookupCombo;
      radSpecificParty: TRadioButton;
      radMPartyPtr: TRadioButton;
      selMPartyID: TIntSelector;
      Background: TGroupBox;
      radDefault: TRadioButton;
      radFromFile: TRadioButton;
      selFilename: TImageEdit;
      grpOutcomes: TGroupBox;
      chkAllowEscape: TCheckBox;
      chkDefeat: TCheckBox;
      grpFormation: TGroupBox;
      cboConditions: TComboBox;
      procedure FormCreate(Sender: TObject);
   protected
      procedure UploadObject(obj: TEbObject); override;
      procedure DownloadObject(obj: TEbObject); override;
      function NewClassType: TEbClass; override;
      procedure EnableControlsProperly; override;
   end;

implementation
uses
   dm_databaseAux, EB_Battle, ArchiveInterface, turbu_battle_engine;

{$R *.dfm}

{ TfrmEditBattle }

procedure TfrmBattle.FormCreate(Sender: TObject);
begin
   dmDatabaseAux.EnsureMParties;
   selFilename.setup(GArchives[IMAGE_ARCHIVE], 'Battle BG');
end;

function TfrmBattle.NewClassType: TEbClass;
begin
   result := TEBBattle;
end;

procedure TfrmBattle.EnableControlsProperly;
begin
   EnableControl(cboMpartyID, radSpecificParty);
   EnableControl(selMPartyID, radMPartyPtr);
   EnableControl(selFilename, radFromFile);
end;

procedure TfrmBattle.UploadObject(obj: TEbObject);
var
   results: TBattleResultSet;
begin
   UploadLookupPtrSelection(obj.Values[0], obj.Values[1], radSpecificParty, radMPartyPtr,
     cboMpartyID, selMPartyID);
   cboConditions.ItemIndex := obj.Values[2];
   if obj.Text <> '' then
      radDefault.Checked := true
   else begin
      radFromFile.Checked := true;
      selFilename.Text := obj.Text;
   end;
   results := (obj as TEBBattleEx).results;
   chkAllowEscape.Checked := br_escaped in results;
   chkDefeat.Checked := br_defeated in results;
end;

procedure TfrmBattle.DownloadObject(obj: TEbObject);
var
   pair: TIntPair;
   results: TBattleResultSet;
begin
   obj.Clear;
   pair := DownloadLookupPtrSelectionInts(radSpecificParty, radMPartyPtr,
     cboMpartyID, selMPartyID);
   obj.Values.AddRange(pair);
   obj.Values.Add(cboConditions.ItemIndex);
   if radDefault.Checked then
      obj.Values.Add(0)
   else if radFromFile.Checked then
   begin
      obj.Values.Add(1);
      obj.Text := selFilename.Text;
   end
   else assert(false);
   results := [];
   if chkAllowEscape.Checked then
      include(results, br_escaped);
   if chkDefeat.Checked then
      include(results, br_defeated);
   (obj as TEBBattleEx).results := results;
end;

initialization
   RegisterEbEditor(TEBBattle, TfrmBattle);
finalization
   UnRegisterEbEditor(TEBBattle);
end.
