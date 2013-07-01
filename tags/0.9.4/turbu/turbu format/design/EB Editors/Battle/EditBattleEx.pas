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

unit EditBattleEx;

interface

uses
   Dialogs, StdCtrls, DBCtrls, ExtCtrls, DB, Classes, Controls,
   EbEdit, EventBuilder, variable_selector, IDLookupCombo, pic_edit,
   turbu_variable_selector, button_edit;

type
   [EditorCategory('Battles', 'Enter Battle (Extended)')]
   [EditorContext('RM2K')]
   TfrmBattleEx = class(TfrmEbEditBase)
      grpMParty: TGroupBox;
      cboMpartyID: TIDLookupCombo;
      radSpecificParty: TRadioButton;
      radMPartyPtr: TRadioButton;
      selMPartyID: TIntSelector;
      Background: TGroupBox;
      radDefault: TRadioButton;
      radFromFile: TRadioButton;
      radTerrain: TRadioButton;
      cboFormation: TComboBox;
      selFilename: TImageEdit;
      cboTerrain: TIDLookupCombo;
      chkAllowEscape: TCheckBox;
      chkDefeat: TCheckBox;
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
   dm_databaseAux, EB_Battle, ArchiveInterface, turbu_battle_engine, EbEditHelper;

{$R *.dfm}

{ TfrmBattleEx }

procedure TfrmBattleEx.FormCreate(Sender: TObject);
begin
   inherited;
   dmDatabaseAux.EnsureMParties;
   selFilename.setup(GArchives[IMAGE_ARCHIVE], 'Battle BG');
   dmDatabaseAux.EnsureTerrain;
end;

function TfrmBattleEx.NewClassType: TEbClass;
begin
   result := TEBBattleEx;
end;

procedure TfrmBattleEx.EnableControlsProperly;
begin
   EnableControl(cboMpartyID, radSpecificParty);
   EnableControl(selMPartyID, radMPartyPtr);
   EnableControl(selFilename, radFromFile);
   EnableControl(cboFormation, radFromFile);
   EnableControl(cboTerrain, radTerrain);
end;

procedure TfrmBattleEx.UploadObject(obj: TEbObject);
var
   results: TBattleResultSet;
begin
   UploadLookupPtrSelection(obj.Values[0], obj.Values[1], radSpecificParty, radMPartyPtr,
     cboMpartyID, selMPartyID);
   cboConditions.ItemIndex := obj.Values[2];
   case obj.Values[3] of
      0: radDefault.Checked := true;
      1:
      begin
         radFromFile.Checked := true;
         cboFormation.ItemIndex := obj.Values[4];
         selFilename.Text := obj.Text;
      end;
      2:
      begin
         radTerrain.Checked := true;
         cboTerrain.ID:= obj.Values[4];
      end;
      else assert(false);
   end;
   results := (obj as TEBBattleEx).results;
   chkAllowEscape.Checked := br_escaped in results;
   chkDefeat.Checked := br_defeated in results;
end;

procedure TfrmBattleEx.DownloadObject(obj: TEbObject);
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
      obj.Values.Add(cboFormation.ItemIndex);
      obj.Text := selFilename.Text;
   end
   else if radTerrain.Checked then
   begin
      obj.Values.Add(2);
      obj.Values.Add(cboTerrain.ID);
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
   RegisterEbEditor(TEBBattleEx, TfrmBattleEx);
finalization
   UnRegisterEbEditor(TEBBattleEx);
end.