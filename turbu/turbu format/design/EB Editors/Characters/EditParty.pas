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

unit EditParty;

interface

uses
   DB, StdCtrls, DBCtrls, Classes, Controls, ExtCtrls,
   EventBuilder, dm_database, EbEdit, variable_selector, IDLookupCombo,
   turbu_variable_selector, button_edit;

type
   [EditorCategory('Characters', 'Change Party')]
   [EditorContext('RM2K')]
   TfrmEBEditParty = class(TfrmEbEditBase)
      GroupBox2: TGroupBox;
      cboHeroID: TIDLookupCombo;
      radSpecificItem: TRadioButton;
      radItemPtr: TRadioButton;
      selItemID: TIntSelector;
      srcHeroes: TDataSource;
      grpOperation: TRadioGroup;
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

{ TfrmEBEditParty }

procedure TfrmEBEditParty.EnableControlsProperly;
begin
   EnableControl(cboHeroID, radSpecificItem);
   EnableControl(selItemID, radItemPtr);
end;

function TfrmEBEditParty.NewClassType: TEbClass;
begin
   result := TEBChangeParty;
end;

procedure TfrmEBEditParty.DownloadObject(obj: TEbObject);
begin
   obj.Clear;
   obj.Values.Add(grpOperation.ItemIndex);
   obj.Add(DownloadLookupPtrSelection(radSpecificItem, radItemPtr, cboHeroID, selItemID, 'Heroes'));
end;

procedure TfrmEBEditParty.UploadObject(obj: TEbObject);
begin
   grpOperation.ItemIndex := obj.Values[0];
   UploadLookupPtrSelection(obj.Children[0] as TEBExpression, radSpecificItem,
     radItemPtr, cboHeroID, selItemID);
end;

initialization
   RegisterEbEditor(TEBChangeParty, TfrmEBEditParty);
finalization
   UnRegisterEbEditor(TEBChangeParty);
end.
