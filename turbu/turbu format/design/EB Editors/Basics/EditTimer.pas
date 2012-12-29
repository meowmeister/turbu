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

unit EditTimer;

interface

uses
   SysUtils, Classes, Controls, Forms, StdCtrls, ExtCtrls, Mask,
   JvExMask, JvSpin,
   EventBuilder, EbEdit, variable_selector, turbu_variable_selector, button_edit;

type
   [EditorCategory('Basics', 'Set Timer')]
   [EditorContext('RM2K')]
   TfrmTimerEdit = class(TfrmEbEditBase)
      radWhichTimer: TRadioGroup;
      radOperation: TRadioGroup;
      grpOptions: TGroupBox;
      chkVisible: TCheckBox;
      chkBattle: TCheckBox;
      grpDuration: TGroupBox;
      lblMinutes: TLabel;
      lblSeconds: TLabel;
      radFixed: TRadioButton;
      spnMinutes: TJvSpinEdit;
      spnSeconds: TJvSpinEdit;
      radVariable: TRadioButton;
      selSeconds: TIntSelector;
      procedure radOperationClick(Sender: TObject);
      procedure radFixedClick(Sender: TObject);
   private
      procedure DisableControls;
      procedure EnableControls(group: TGroupBox; value: boolean);
   protected
      procedure UploadObject(obj: TEbObject); override;
      procedure DownloadObject(obj: TEbObject); override;
      function NewClassType: TEbClass; override;
   end;

var
  frmTimerEdit: TfrmTimerEdit;

implementation
uses
   EB_System;

{$R *.dfm}

{ TfrmTimerEdit }

procedure TfrmTimerEdit.DownloadObject(obj: TEbObject);
begin
   obj.Values.Clear;
   obj.Values.Add(radOperation.ItemIndex);                            //[0]
   obj.Values.Add(ord(radVariable.Checked));                          //[1]
   obj.Values.Add(round((spnMinutes.Value * 60) + spnSeconds.Value)); //[2]
   obj.Values.Add(ord(chkVisible.Checked));                           //[3]
   obj.Values.Add(ord(chkBattle.Checked));                            //[4]
   obj.Values.Add(radWhichTimer.ItemIndex);                           //[5]
end;

procedure TfrmTimerEdit.UploadObject(obj: TEbObject);
begin
   if (obj.values.count > 5) and (obj.values[5] = 1) then
      radWhichTimer.ItemIndex := 1
   else radWhichTimer.ItemIndex := 0;
   radOperation.ItemIndex := obj.Values[0];
   case obj.Values[0] of
      0: if boolean(obj.values[1]) then
            radVariable.Checked := true
         else begin
            radFixed.Checked := true;
            spnMinutes.Value := obj.values[2] div 60;
            spnSeconds.Value := obj.values[2] mod 60;
         end;
      1:
      begin
         chkVisible.Checked := boolean(obj.Values[3]);
         chkBattle.Checked := boolean(obj.Values[4]);
      end;
   end;
   radOperationClick(self);
end;

function TfrmTimerEdit.NewClassType: TEbClass;
begin
   result := TEBTimer;
end;

procedure TfrmTimerEdit.EnableControls(group: TGroupBox; value: boolean);
var
   i: integer;
begin
   for i := 0 to group.ControlCount -1 do
      group.Controls[i].Enabled := value;
end;

procedure TfrmTimerEdit.DisableControls;
begin
   EnableControls(grpOptions, false);
   EnableControls(grpDuration, false);
end;

procedure TfrmTimerEdit.radFixedClick(Sender: TObject);
var
   first: boolean;
begin
   radFixed.Enabled := true;
   radVariable.Enabled := true;
   first := radFixed.Checked;
   spnMinutes.Enabled := first;
   spnSeconds.Enabled := first;
   lblMinutes.Enabled := first;
   lblSeconds.Enabled := first;
   selSeconds.Enabled := not first;
end;

procedure TfrmTimerEdit.radOperationClick(Sender: TObject);
begin
   DisableControls;
   case radOperation.ItemIndex of
      0: EnableControls(grpDuration, true);
      1: radFixedClick(sender);
   end;
end;

initialization
   RegisterEbEditor(TEBTimer, TfrmTimerEdit);
finalization
   UnRegisterEbEditor(TEBTimer);
end.
