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

unit EditConditions;

interface

uses
   DBClient, Classes, Controls, ExtCtrls, DB, StdCtrls, DBCtrls,
   EventBuilder, variable_selector, ebPartyBase, IDLookupCombo, EBEdit,
   dm_database, button_edit;

type
   [EditorCategory('Characters', 'Change Condition')]
   [EditorContext('RM2K')]
   TfrmEBEditConditions = class(TfrmEBPartyBase)
      grpOperation: TRadioGroup;
      StaticText1: TStaticText;
      cboCondition: TIDLookupCombo;
      srcConditions: TDataSource;
   protected
      procedure UploadObject(obj: TEbObject); override;
      procedure DownloadObject(obj: TEbObject); override;
      function NewClassType: TEbClass; override;
   end;

implementation
uses
   EB_Characters, EB_Expressions;

{$R *.dfm}

{ TfrmEBEditConditions }

procedure TfrmEBEditConditions.UploadObject(obj: TEbObject);
begin
   inherited UploadObject(obj);
   grpOperation.ItemIndex := obj.Values[0];
   cboCondition.id := (obj.Children[1] as TEBLookupValue).values[0];
end;

procedure TfrmEBEditConditions.DownloadObject(obj: TEbObject);
begin
   inherited DownloadObject(obj);
   obj.Values.Add(grpOperation.ItemIndex);
   obj.Add(TEBLookupValue.Create(cboCondition.id, 'conditions'));
end;

function TfrmEBEditConditions.NewClassType: TEbClass;
begin
   result := TEBChangeStatus;
end;

initialization
   RegisterEbEditor(TEBChangeStatus, TfrmEBEditConditions);
finalization
   UnRegisterEbEditor(TEBChangeStatus);
end.
