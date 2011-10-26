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

unit ChoiceEdit;

interface

uses
  ExtCtrls, StdCtrls, Classes, Controls,
  EbEdit, EventBuilder;

type
   [EditorCategory('Messages', 'Show Choice')]
   TfrmShowChoice = class(TfrmEbEditBase)
      txtChoice1: TEdit;
      txtChoice2: TEdit;
      txtChoice3: TEdit;
      txtChoice4: TEdit;
      grpCancel: TRadioGroup;
      GroupBox1: TGroupBox;
      procedure FormCreate(Sender: TObject);
   private
      FEdits: array[1..4] of TEdit;
      function GetMaxAnswers: integer;
   protected
      procedure UploadObject(obj: TEbObject); override;
      procedure DownloadObject(obj: TEbObject); override;
      function NewClassType: TEbClass; override;
      function ValidateForm: boolean; override;
   end;

implementation
uses
   EB_Messages, EB_RpgScript, EB_Expressions;

{$R *.dfm}

{ TfrmShowChoice }

procedure TfrmShowChoice.FormCreate(Sender: TObject);
begin
   FEdits[1] := txtChoice1;
   FEdits[2] := txtChoice2;
   FEdits[3] := txtChoice3;
   FEdits[4] := txtChoice4;
end;

function TfrmShowChoice.GetMaxAnswers: integer;
var
   i: Integer;
begin
   for i := high(FEdits) downto low(FEdits) do
      if FEdits[i].text <> '' then
         exit(i);
   result := 0;
end;

function TfrmShowChoice.NewClassType: TEbClass;
begin
   result := TEBChoiceMessage;
end;

procedure TfrmShowChoice.UploadObject(obj: TEbObject);
var
   sub: TEbObject;
   counter: integer;
begin
   assert(obj is TEBChoiceMessage);
   counter := 0;
   for sub in obj do
   begin
      if sub is TEbElseBlock then
         Continue;
      if sub is TEBEndCase then
         Break;
      assert(sub is TEbCaseBlock);
      inc(counter);
      FEdits[counter].text := sub.text;
   end;
   for counter := counter + 1 to 4 do
      FEdits[counter].text := '';
   grpCancel.ItemIndex := obj.values[0];
end;

function TfrmShowChoice.ValidateForm: boolean;
var
   max: integer;
begin
   result := true;
   max := GetMaxAnswers;
   if max = 0 then
      ValidateError(txtChoice1, 'Please fill in at least one choice');
   if (grpCancel.ItemIndex > max) and (grpCancel.ItemIndex < grpcancel.Items.count - 1) then
      ValidateError(grpCancel, 'Cancel handler must be a valid choice');
end;

procedure TfrmShowChoice.DownloadObject(obj: TEbObject);
const CANCEL_ELSE = 5;
var
   i: integer;
   elseBlock: TEbElseBlock;
   caseBlock: TEbCaseBlock;
   text: string;
begin
   obj.Values.Clear;
   obj.Values.Add(grpCancel.ItemIndex);
   ElseBlock := (obj as TEbCase).GetElseBlock;
   if assigned(ElseBlock) then
      obj.Children.Extract(ElseBlock);
   for I := 1 to GetMaxAnswers do
   begin
      if i > 1 then
         text := text + '/'
      else text := '';
      text := text + FEdits[i].Text;
      if i <= obj.ChildCount then
         caseBlock := obj.Children[i - 1] as TEbCaseBlock
      else caseBlock := TEbCaseBlock.Create(obj);
      caseBlock.Text := FEdits[i].Text;
   end;
   obj.Text := text;
   for I := GetMaxAnswers + 1 to obj.ChildCount - 1 do
      obj.Children.Extract(obj.Children[i]);
   if grpCancel.ItemIndex = CANCEL_ELSE then
   begin
      if not assigned(ElseBlock) then
         ElseBlock := TEbElseBlock.Create(nil);
      obj.Add(ElseBlock);
   end
   else elseBlock.Free;
end;

initialization
   RegisterEbEditor(TEBChoiceMessage, TfrmShowChoice);
finalization
   UnRegisterEbEditor(TEBChoiceMessage);
end.
