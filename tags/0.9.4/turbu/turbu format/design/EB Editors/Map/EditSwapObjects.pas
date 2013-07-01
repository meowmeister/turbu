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

unit EditSwapObjects;

interface

uses
  Forms, StdCtrls, ExtCtrls, Classes, Controls,
  EventBuilder, EbEdit, turbu_map_interface;

type
   [EditorCategory('Map', 'Swap Map Object')]
   [EditorContext('RM2K')]
   TfrmEditSwapObjects = class(TfrmEbEditBase)
      GroupBox1: TGroupBox;
      cboObject1: TComboBox;
      cboObject2: TComboBox;
   protected
      procedure UploadObject(obj: TEbObject); override;
      procedure DownloadObject(obj: TEbObject); override;
      function NewClassType: TEbClass; override;
   public
      procedure SetupMap(const map: IRpgMap); override;
   end;

implementation
uses
   EB_Maps, EB_Expressions;

{$R *.dfm}

{ TfrmEditSwapObjects }

function TfrmEditSwapObjects.NewClassType: TEbClass;
begin
   result := TEBSwapObjects;
end;

procedure TfrmEditSwapObjects.SetupMap(const map: IRpgMap);
var
   list: TStrings;
begin
   if assigned(map) then
   begin
      list := map.GetMapObjects;
      try
         list.Delete(0);
         cboObject1.Items.AddStrings(list);
         cboObject2.Items.AddStrings(list);
      finally
         list.Free;
      end;
   end;
end;

procedure TfrmEditSwapObjects.DownloadObject(obj: TEbObject);
begin
   obj.Clear;
   obj.Add(DownloadObjectRef(cboObject1));
   obj.Add(DownloadObjectRef(cboObject2));
end;

procedure TfrmEditSwapObjects.UploadObject(obj: TEbObject);
begin
   UploadObjectRef(obj.children[0] as TEBObjExpr, cboObject1);
   UploadObjectRef(obj.children[1] as TEBObjExpr, cboObject2);
end;

initialization
   RegisterEbEditor(TEBSwapObjects, TfrmEditSwapObjects);
finalization
   UnRegisterEbEditor(TEBSwapObjects);
end.
