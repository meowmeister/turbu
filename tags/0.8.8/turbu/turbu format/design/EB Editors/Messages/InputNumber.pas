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

unit InputNumber;

interface

uses
  SysUtils, Classes, Controls, Forms, StdCtrls, ExtCtrls,
  EbEdit, EventBuilder, Mask, JvExMask, JvSpin, variable_selector;

type
   [EditorCategory('Messages', 'Input Number', 4)]
   TfrmInputNumber = class(TfrmEbEditBase)
      GroupBox1: TGroupBox;
      spnDigits: TJvSpinEdit;
      GroupBox2: TGroupBox;
      selInteger: TIntSelector;
   protected
      procedure UploadObject(obj: TEbObject); override;
      procedure DownloadObject(obj: TEbObject); override;
      function NewClassType: TEbClass; override;
   end;

implementation
uses
   EB_Messages;

{$R *.dfm}

{ TfrmInputNumber }

procedure TfrmInputNumber.DownloadObject(obj: TEbObject);
begin
   assert(obj is TEBInputNumber);
   obj.Values.Clear;
   obj.Values.Add(spnDigits.AsInteger);
   obj.Values.Add(selInteger.ID);
end;

function TfrmInputNumber.NewClassType: TEbClass;
begin
   result := TEBInputNumber;
end;

procedure TfrmInputNumber.UploadObject(obj: TEbObject);
begin
   assert(obj is TEBInputNumber);
   spnDigits.AsInteger := obj.Values[0];
   selInteger.ID := obj.Values[1];
end;

initialization
   RegisterEbEditor(TEBInputNumber, TfrmInputNumber);
end.
