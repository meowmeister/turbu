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

unit EditCallScript;

interface

uses
   Forms, StdCtrls, Classes, Controls, ExtCtrls, DB, DBCtrls, DBIndexComboBox,
   Mask, DBClient, JvExMask, JvSpin,
   dm_database, EbEdit, EventBuilder, variable_selector, turbu_map_interface;

type
   [EditorCategory('Basics', 'Call Script', 5)]
   TfrmEBEditCall = class(TfrmEbEditBase)
      grpMain: TGroupBox;
      radGlobalScript: TRadioButton;
      srcGlobals: TDataSource;
      radMapObject: TRadioButton;
      cbxMapObject: TComboBox;
      Label1: TLabel;
      spnMapPage: TJvSpinEdit;
      radMapObjectPtr: TRadioButton;
      selObjPtr: TIntSelector;
      selPage: TIntSelector;
      cbxGlobalName: TDBLookupComboBox;
      srcGlobalOutput: TDataSource;
      dsGlobalOutput: TClientDataSet;
      dsGlobalOutputid: TIntegerField;
      procedure RadioButtonClick(Sender: TObject);
      procedure cbxMapObjectClick(Sender: TObject);
   private
      function GetCheckedRadioButton: integer;
      procedure EnableControlsProperly;
   protected
      procedure UploadObject(obj: TEbObject); override;
      procedure DownloadObject(obj: TEbObject); override;
      function NewClassType: TEbClass; override;
   public
      procedure SetupMap(const map: IRpgMap); override;
   end;

implementation
uses
   EB_RpgScript, EB_System, turbu_map_objects;

{$R *.dfm}

{ TfrmEBEditCall }

function TfrmEBEditCall.GetCheckedRadioButton: integer;
begin
   if radGlobalScript.Checked then
      result := 0
   else if radMapObject.Checked then
      result := 1
   else if radMapObjectPtr.Checked then
      result := 2
   else raise ERPGScriptError.Create('No active radio button.');
end;

procedure TfrmEBEditCall.EnableControlsProperly;
var
   i: integer;
begin
   for i := 0 to grpMain.ControlCount -1 do
      if not (grpMain.Controls[i] is TRadioButton) then
         grpMain.Controls[i].Enabled := false;
   case GetCheckedRadioButton of
      0: cbxGlobalName.Enabled := true;
      1:
      begin
         cbxMapObject.Enabled := true;
         spnMapPage.Enabled := true;
      end;
      2:
      begin
         selObjPtr.Enabled := true;
         selPage.Enabled := true;
      end
      else assert(false);
   end;
end;

procedure TfrmEBEditCall.cbxMapObjectClick(Sender: TObject);
var
   obj: TRpgMapObject;
begin
   obj := cbxMapObject.Items.Objects[cbxMapObject.ItemIndex] as TRpgMapObject;
   spnMapPage.MaxValue := obj.pages.Count;
end;

procedure TfrmEBEditCall.DownloadObject(obj: TEbObject);
var
   checked: integer;
begin
   obj.Values.Clear;
   checked := GetCheckedRadioButton;
   obj.Values.Add(checked);
   case checked of
      0:
      begin
         obj.Values.add(dsGlobalOutputid.Value);
         obj.Values.add(0);
      end;
      1:
      begin
         obj.Values.add(cbxMapObject.ItemIndex);
         obj.Values.add(spnMapPage.AsInteger);
      end;
      2:
      begin
         obj.Values.add(selObjPtr.ID);
         obj.Values.add(selPage.ID);
      end
      else assert(false);
   end;
end;

function TfrmEBEditCall.NewClassType: TEbClass;
begin
   result := TEBCallEvent;
end;

procedure TfrmEBEditCall.RadioButtonClick(Sender: TObject);
begin
   EnableControlsProperly;
end;

procedure TfrmEBEditCall.SetupMap(const map: IRpgMap);
var
   list: TStrings;
begin
   dsGlobalOutput.AppendRecord([0]);
   EnableControlsProperly;
   if assigned(map) then
   begin
      list := map.GetMapObjects;
      try
         list.Delete(0);
         self.cbxMapObject.Items.AddStrings(list);
      finally
         list.Free;
      end;
   end
   else begin
      radGlobalScript.Checked := true;
      radMapObject.Enabled := false;
      radMapObjectPtr.Enabled := false;
   end;
end;

procedure TfrmEBEditCall.UploadObject(obj: TEbObject);
begin
   case obj.Values[0] of
      0:
      begin
         radGlobalScript.Checked := true;
         dsGlobalOutput.Edit;
         dsGlobalOutputid.Value := obj.Values[1];
         dsGlobalOutput.Post;
      end;
      1:
      begin
         radMapObject.Checked := true;
         cbxMapObject.ItemIndex := obj.Values[1];
         spnMapPage.AsInteger := obj.Values[2];
      end;
      2:
      begin
         selObjPtr.ID := obj.Values[1];
         selPage.ID := obj.Values[2];
      end
      else assert(false);
   end;
   EnableControlsProperly;
end;

initialization
   RegisterEbEditor(TEBCallEvent, TfrmEBEditCall);
end.
