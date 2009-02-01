unit frame_params;
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
   Forms, Controls, StdCtrls, Classes,
   turbu_classes, turbu_defs, JvExStdCtrls, JvEdit, JvValidateEdit, Mask,
  JvExMask, JvSpin;

type
   TParamRecord = record
   procedure enable(value: boolean);
   case boolean of
      true: (
         box: TGroupBox;
         name: TJvValidateEdit;
         isVar: TCheckBox;
         typename: TComboBox;
         L1, L2: TLabel);
      false:
         (control: array[1..6] of TControl);
   end;

   TframeParams = class(TFrame)
      lblNumber: TLabel;
      spnCount: TJvSpinEdit;
      GroupBox1: TGroupBox;
      Label1: TLabel;
      Label2: TLabel;
      cbxTypes: TComboBox;
      txtCodeName: TJvValidateEdit;
      chkVar: TCheckBox;
      procedure spnCountChange(Sender: TObject);
      procedure headerChange(Sender: TObject);
   private
      FParamSet: TRpgList<TParamRecord>;
      FStringSet: TStringSetProc;

      procedure setSize(Value: byte);
      function getSize: byte;
      procedure addParam;
      procedure deleteParam;
   public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;

      procedure setup(value: TRpgDecl);
      property size: byte read getSize write setSize;
      property onUpdate: TStringSetProc write FStringSet;
   end;

implementation
uses
   SysUtils, typInfo,
   turbu_constants;

{$R *.dfm}

constructor TframeParams.Create(AOwner: TComponent);
var
   boxRecord: TParamRecord;
begin
   inherited Create(AOwner);
   boxRecord.box := GroupBox1;
   boxRecord.name := txtCodeName;
   boxRecord.isVar := chkVar;
   boxRecord.typename := cbxTypes;
   boxRecord.L1 := Label1;
   boxRecord.L2 := Label2;
   FParamSet := TRpgList<TParamRecord>.Create;
   FParamSet.Add(boxRecord);
end;

destructor TframeParams.Destroy;
begin
   FParamSet.Free;
   inherited Destroy;;
end;

procedure TframeParams.addParam;
var
   last, new: TGroupBox;
   control, newControl: TControl;
   compClass: TControlClass;
   i: integer;
   oldParam, newParam: TParamRecord;
begin
   oldParam := FParamSet.Last;
   last := oldParam.box;
   fillchar(newParam, sizeof(TParamRecord), 0);
   new := TGroupBox.Create(nil);
   new.Parent := self;
   new.BoundsRect := last.BoundsRect;
   new.Top := new.Top + last.Height + 8;
   newParam.box := new;
   for i := 2 to high(oldParam.control) do
   begin
      control := oldParam.control[i];
      compClass := TControlClass(control.ClassType);
      newControl := compClass.Create(new);
      newControl.Parent := new;
      newControl.BoundsRect := control.BoundsRect;
      if newControl is TLabel then
         TLabel(newControl).Caption := TLabel(control).Caption
      else if newControl is TComboBox then
      begin
         TComboBox(newControl).Items := TComboBox(control).Items;
         TComboBox(newControl).ItemIndex := TComboBox(control).ItemIndex;
      end;
      newParam.control[i] := newControl;
   end;
//   newParam.name.DisplayFormat := dfIdentifier;
   newParam.name.DisplayFormat := dfAlphaNumeric;
   newParam.name.OnChange := self.headerChange;
   newParam.isVar.OnClick := self.headerChange;
   newParam.typename.OnChange := self.headerChange;
   FParamSet.Add(newParam);
   newParam.name.Text := 'Param' + intToStr(FParamSet.Count);
end;

procedure TframeParams.deleteParam;
begin
   FParamSet.last.box.Free;
   FParamSet.Remove(FParamSet.last);
end;

function TframeParams.getSize: byte;
begin
   result := FParamSet.Count;
end;

procedure TframeParams.setSize(Value: byte);
var
   i: integer;
   zf: boolean; //zero flag
begin
   zf := value = 0;
   if zf then
      inc(value);
   if value = self.size then
      Exit;
   if value > self.size then
      for I := size + 1 to value do
         self.addParam
   else begin
      for I := self.size downto value + 1 do
         self.deleteParam;
   end;
   if zf then
   begin
      dec(value);
      GroupBox1.Enabled := false;
   end;
   spnCount.Value := value;
   Application.ProcessMessages;
end;

procedure TframeParams.setup(value: TRpgDecl);
var
  I: Integer;
begin
   self.setSize(value.params.count);
   for I := 0 to self.getSize - 1 do
   begin
      FParamSet[i].typename.ItemIndex := value.params[i].typeVar;
      FParamSet[i].isVar.Checked :=  pfVar in value.params[i].flags;
      FParamSet[i].name.Text := value.params[i].name;
   end;
end;

procedure TframeParams.spnCountChange(Sender: TObject);
begin
   self.setSize(trunc(spnCount.Value));
   self.headerChange(sender);
end;

procedure TframeParams.headerChange(Sender: TObject);
var
   result: string;
   i: integer;
   group: boolean;
begin
   if not assigned(FStringSet) then
      Exit;
   group := false;
   result := '(';
   for i := 0 to FParamSet.Count - 1 do
   begin
      if (FParamSet[i].isVar.Checked) and (not group) then
         result := result + 'var ';
      if FParamSet[i].name.Text = '' then
         Exit
      else result := result + FParamSet[i].name.Text;
      group := (i < FParamSet.Count - 1) and (FParamSet[i].isVar.Checked = FParamSet[i + 1].isVar.Checked)
            and (FParamSet[i].typename.Text = FParamSet[i + 1].typename.Text);
      if group then
         result := result + ', '
      else begin
         if FParamSet[i].typename.Text = '' then
            Exit
         else result := result + ': ' + FParamSet[i].typename.Text;
         if (i < FParamSet.Count - 1) then
            result := result + '; ';
      end;
   end;
   result := result + ')';
   FStringSet(result);
end;

{ TParamRecord }

procedure TParamRecord.enable(value: boolean);
var
   i: Integer;
begin
   for i := low(control) to high(control) do
      control[i].Enabled := value;
end;

end.
