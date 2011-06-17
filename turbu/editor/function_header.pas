unit function_header;
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
  Forms, Controls, StdCtrls, Mask, Classes,
  frame_params,
  turbu_defs, turbu_classes, JvExStdCtrls, JvEdit, JvValidateEdit;

type
   TfrmFuncHeader = class(TForm)
      lblName: TLabel;
      cbxResult: TComboBox;
      Label1: TLabel;
      cbxProcType: TComboBox;
      grpParams: TGroupBox;
      frameParams: TframeParams;
      txtCodeName: TJvValidateEdit;
      procedure FormShow(Sender: TObject);
      procedure cbxProcTypeChange(Sender: TObject);
   private
      { Private declarations }
      FSetHeader: TStringSetProc;
      procedure updateHeader(value: string);
   public
      { Public declarations }
      procedure setup(value: TRpgDecl);
      property onUpdate: TStringSetProc write FSetHeader;
  end;

implementation
uses
   turbu_vartypes;

{$R *.dfm}

procedure TfrmFuncHeader.cbxProcTypeChange(Sender: TObject);
var
   enable: boolean;
begin
   enable := cbxProcType.ItemIndex = 0;
   lblName.Enabled := enable;
   cbxResult.Enabled := enable;
end;

procedure TfrmFuncHeader.FormShow(Sender: TObject);
begin
   cbxResult.Items := getTypeList;
   frameParams.cbxTypes.Items := cbxResult.Items;
   frameParams.onUpdate := self.updateHeader;
end;

procedure TfrmFuncHeader.setup(value: TRpgDecl);
begin
   txtCodeName.Text := value.name;
   cbxResult.ItemIndex := value.retval;
   frameParams.setup(value);
end;

procedure TfrmFuncHeader.updateHeader(value: string);
var
   result: string;
begin
   case cbxProcType.ItemIndex of
      0: result := 'function ' + value + ': ' + cbxProcType.Text + ';';
      1: result := 'procedure ' + value + ';';
      else assert(false);
   end;

end;

end.
