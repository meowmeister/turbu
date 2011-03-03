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

unit EditSwitch;

interface

uses
   Classes, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
   variable_selector,
   EventBuilder, EbEdit;

type
   [EditorCategory('Basics', 'Set Switch', 0)]
   TfrmEbSetSwitch = class(TfrmEbEditBase)
      GroupBox1: TGroupBox;
      radSwitch: TRadioButton;
      radInt: TRadioButton;
      selGlobalSwitch: TSwitchSelector;
      selGlobalint: TIntSelector;
      grpSetTo: TRadioGroup;
      procedure RadioButtonClick(Sender: TObject);
   protected
      procedure UploadObject(obj: TEbObject); override;
      procedure DownloadObject(obj: TEbObject); override;
      function NewClassType: TEbClass; override;
   end;

implementation
uses
   Messages,
   EB_System, EB_Expressions;

{$R *.dfm}

{ TfrmEbSetSwitch }

function TfrmEbSetSwitch.NewClassType: TEbClass;
begin
   if (selGlobalSwitch.Enabled) and (selGlobalSwitch.ID < 0) then //elaborate later
      result := TEBLocalSwitch
   else result := TEBGlobalSwitch;
end;

procedure TfrmEbSetSwitch.RadioButtonClick(Sender: TObject);
begin
   selGlobalSwitch.Enabled := radSwitch.checked;
   selGlobalint.Enabled := radInt.checked;
end;

procedure TfrmEbSetSwitch.DownloadObject(obj: TEbObject);
begin
   assert(obj is NewClassType);
   obj.Values.Clear;
   obj.Values.Add(grpSetTo.ItemIndex);
   if (obj.ComponentCount > 0) then
      (obj.Components[0] as TEBExpression).Free;
   assert(obj.ComponentCount = 0);
   if NewClassType = TEBLocalSwitch then
      obj.Text := selGlobalSwitch.varname
   else begin
      if selGlobalSwitch.Enabled then
         obj.add(TEBIntegerValue.Create(selGlobalSwitch.ID))
      else if selGlobalInt.ID >= 0 then
         obj.add(TEBIntsValue.Create(selGlobalInt.ID))
      else obj.add(TEBVariableValue.Create(selGlobalInt.varname));
   end;
end;

procedure TfrmEbSetSwitch.UploadObject(obj: TEbObject);
var
   expr: TEBExpression;
begin
   assert(obj is TEBSwitch);
   if obj is TEBGlobalSwitch then
   begin
      expr := obj.components[0] as TEBExpression;
      if expr is TEBIntegerValue then
      begin
         radSwitch.Checked := true;
         selGlobalSwitch.ID := expr.Values[0];
      end
      else begin
         radInt.Checked := true;
         if expr is TEBIntsValue then
            selGlobalint.ID := expr.Values[0]
         else selGlobalint.ID := ContextLookup((expr as TEBVariableValue).GetNodeText);
      end;
   end
   else begin
      radSwitch.Checked := true;
      selGlobalSwitch.ID := ContextLookup(obj.Text);
   end;
   RadioButtonClick(self);
   grpSetTo.ItemIndex := obj.Values[0];
end;

initialization
   RegisterEbEditor(TEBGlobalSwitch, TfrmEbSetSwitch);
   RegisterEbEditor(TEBLocalSwitch, TfrmEbSetSwitch);
end.
