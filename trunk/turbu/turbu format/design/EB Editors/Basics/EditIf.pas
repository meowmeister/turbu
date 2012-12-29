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

unit EditIf;

interface

uses
   Forms, StdCtrls, ExtCtrls, Classes, Controls, ComCtrls, DBClient,
   EbEdit, EBListView, EventBuilder, fr_EditCondition, turbu_map_interface;

type
   [EditorCategory('Basics', 'If/Then')]
   [EditorContext('Universal')]
   TfrmEBEditIf = class(TfrmEbEditBase, IContextualEditor, IVariableEditor)
      Panel2: TPanel;
      chkElseBlock: TCheckBox;
      frEditCondition: TfrEditCondition;
   private //IContextualEditor implementation
      procedure SetContext(const context, suffix: string);
   private //IVariableEditor implementation
      procedure SetVariables(locals, globals: TCustomClientDataset);
   protected
      procedure UploadObject(obj: TEbObject); override;
      procedure DownloadObject(obj: TEbObject); override;
      function NewClassType: TEbClass; override;
      procedure PrepareNewObject(obj: TEBObject); override;
   public
      procedure SetupMap(const map: IRpgMap); override;
      procedure SetupEvent(const obj: IRpgMapObject); override;
   end;

implementation
uses
   EB_RpgScript, EB_Expressions;

{$R *.dfm}

{ TfrmEBEditIf }

procedure TfrmEBEditIf.UploadObject(obj: TEbObject);
begin
   frEditCondition.UploadObject(obj.children[0]);
   chkElseBlock.Checked := obj.ChildCount = 3;
end;

procedure TfrmEBEditIf.DownloadObject(obj: TEbObject);
begin
   obj.children.Delete(0);
   obj.children.Insert(0, TEBObject.Load(frEditCondition.DownloadObject));
   if (obj.ChildCount = 3) and not chkElseBlock.Checked then
      obj.children.Extract(obj.children[3])
   else if (obj.ChildCount = 2) and chkElseBlock.Checked then
      TEBCodeBlock.Create(obj);
end;

function TfrmEBEditIf.NewClassType: TEbClass;
begin
   result := TEBIf;
end;

procedure TfrmEBEditIf.PrepareNewObject(obj: TEBObject);
begin
   obj.Add(TEBBooleanValue.Create(true));
   TEBCodeBlock.Create(obj)
end;

procedure TfrmEBEditIf.SetContext(const context, suffix: string);
begin
   (frEditCondition as IContextualEditor).SetContext(context, suffix + ' Cond');
end;

procedure TfrmEBEditIf.SetupEvent(const obj: IRpgMapObject);
begin
   frEditCondition.trvCondition.mapObj := obj;
end;

procedure TfrmEBEditIf.SetupMap(const map: IRpgMap);
begin
   frEditCondition.trvCondition.map := map;
end;

procedure TfrmEBEditIf.SetVariables(locals, globals: TCustomClientDataset);
begin
   frEditCondition.trvCondition.Globals := globals;
   frEditCondition.trvCondition.Context := locals;
end;

initialization
   RegisterEbEditor(TEBIf, TfrmEBEditIf);
finalization
   UnRegisterEbEditor(TEBIf);
end.
