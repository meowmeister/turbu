unit generic_algorithm_editor;
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

{THIS UNIT IS CURRENTLY BROKEN
Dependency on the old GDatabase.algorithms property needs to be replaced by
a unit lookup.}

interface

uses
   types, Forms, Controls, Dialogs, StdCtrls, ExtCtrls, Mask, Classes,
   JvExControls, JvEditorCommon, JvEditor, JvHLEditor,
   commons, turbu_defs, turbu_classes, dm_database, DB;

type
   TNameChangeResult = (nr_new, nr_renamed, nr_cancel);
   TNameChangeSet = set of TNameChangeResult;

   TfrmAlgorithmEditor = class(TForm)
      btnOK: TButton;
      btnCancel: TButton;
      btnUnitView: TButton;
      grpEditor: TGroupBox;
      grpFuncNames: TGroupBox;
      lblDesign: TLabel;
      lblCode: TLabel;
      txtDesignName: TEdit;
      txtCodeName: TMaskEdit;
      txtHeader: TEdit;
      Button1: TButton;
      txtEditor: TJvHLEditor;
      Label1: TLabel;
      dsRanges: TDataSource;

      procedure txtEditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
      procedure FormCreate(Sender: TObject);
      procedure btnOKClick(Sender: TObject);
      procedure Button1Click(Sender: TObject);
   private
      FReturnType: string;
      FScript: TScriptRecord;
      FStyle: TScriptStyle;
      FPadding: TRpgPoint;

      dsFunction: TDataSet;

      function verifyNames: TNameChangeSet;
      procedure setHeader(header: string);
   public
      function funcEdit(style: TScriptStyle; script: TDataSet; returnType: string): boolean;
      property script: TScriptRecord read FScript;
   end;

var
   frmAlgorithmEditor: TfrmAlgorithmEditor;

implementation

uses
   windows, SysUtils, strUtils,
   turbu_database, turbu_characters, design_script_engine, turbu_script_basis,
   function_header,
   uPSRuntime;

{$R *.dfm}

procedure TfrmAlgorithmEditor.setHeader(header: string);
begin
   txtHeader.Text := header;
end;

procedure TfrmAlgorithmEditor.btnOKClick(Sender: TObject);
var
   bounds: TRpgPoint;
   verifyResult: TNameChangeSet;
   newName: string;
   algs: TStringList;
   oldAlgs: string;
   worked: boolean;
begin
   newName := FScript.designName;
   verifyResult := verifyNames;
   if nr_cancel in verifyResult then
      Exit
   else if nr_renamed in verifyResult then
      newName := txtDesignName.Text;
   bounds := GDScriptEngine.bounds[FScript.name];
   inc(bounds.x, FPadding.x);
   dec(bounds.y, FPadding.y);
   algs := GDatabase.units[dsFunction.FieldByName('unit').AsString];
   worked := true;
   oldAlgs := algs.Text;
   algs.Text := StuffString(oldAlgs, bounds.x, bounds.y - bounds.x, txtEditor.Lines.Text);
   if not GDatabase.scriptBuild then
   begin
      worked := false;
      algs.Text := oldAlgs;
   end;

   if worked then
   begin
      if nr_new in verifyResult then
      begin
         case FStyle of
            sc_exp: FScript := TExpCalcRecord.Create(txtCodeName.Text, newName);
            else assert(false);
         end;

      end else begin
         FScript.name := txtCodeName.Text;
         FScript.designName := newName;
      end;
      self.ModalResult := mrOk;
   end;
end;

procedure TfrmAlgorithmEditor.Button1Click(Sender: TObject);
begin
   frmFuncHeader.show;
end;

procedure TfrmAlgorithmEditor.FormCreate(Sender: TObject);
begin
   frmFuncHeader.onUpdate := self.setHeader;
end;

function TfrmAlgorithmEditor.funcEdit(style: TScriptStyle; script: TDataSet; returnType: string): boolean;
var
   dummy: string;
//   i: integer;
begin
   result := false;
   FReturnType := returnType;
   dsFunction := script;
   FScript := TObject(script.FieldByName('address').AsInteger) as TScriptRecord;
   dummy := '';
   if (assigned(FScript)) and (FScript.name <> '') then
   begin
      dummy := GDScriptEngine.func[FScript.name];
      FPadding := point(0, 0);
      if (length(dummy) > 0) then
      begin
         if dummy[1] = #$A then
         begin
            delete(dummy, 1, 1);
            FPadding.X := 1;
         end;
         while charInSet(dummy[length(dummy)], [#$A, #$D]) do
         begin
            delete(dummy, length(dummy), 1);
            inc(FPadding.Y);
         end;
      end;
      frmFuncHeader.setup(GDScriptEngine.decl.decl[FScript.name]);
      txtCodeName.Text := FScript.name;
      txtDesignName.Text := FScript.designName;
   end;
   txtEditor.lines.Text := dummy;
   if self.ShowModal = mrOK then
   begin
      result := not ((script.FieldByName('address').AsInteger = integer(FScript)) and
                     (script.fieldByName('name').AsString = FScript.name) and
                     (script.fieldByName('designName').AsString = FScript.designName));
      FScript.update(dsFunction);
   end
   else
      ; {add more stuff here}
end;

procedure TfrmAlgorithmEditor.txtEditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
   if Key = VK_ESCAPE then
      btnCancel.Click;
end;

function TfrmAlgorithmEditor.verifyNames: TNameChangeSet;
resourcestring
   NAMECHANGED = 'The function''s name has been changed.  Continuing will create a new script function for ';
   BOTHCHANGED = 'The function''s name and design name have been changed.  Continuing will create a new script function.';
   DNAMECHANGED = 'The function''s design name has been changed.  Continuing will rename the function''s design name.';
   CHANGE_HEADER = 'Name has been changed!';
var
   dummy: string;
begin
   result := [];
   if (FScript.name = txtCodeName.Text) and (FScript.designName = txtDesignName.Text) then
      Exit
   else if FScript.name <> txtCodeName.Text then
   begin
      if FScript.designName = txtDesignName.Text then
         dummy := NAMECHANGED + FScript.designName + '.'
      else
         dummy := BOTHCHANGED;

      if MsgBox(dummy, CHANGE_HEADER, MB_OKCANCEL) = IDOK then
         result := [nr_new]
      else result := [nr_cancel];
   end
   else if FScript.designName <> txtDesignName.Text then
   begin
      if MsgBox(DNAMECHANGED, CHANGE_HEADER, MB_OKCANCEL) = IDOK then
         result := [nr_renamed]
      else result := [nr_cancel];
   end;
   if (result = [nr_new]) and (dummy = BOTHCHANGED) then
      result := result + [nr_renamed];
end;

end.
