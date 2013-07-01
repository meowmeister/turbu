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

unit PortraitEdit;

interface

uses
   StdCtrls, Classes, Controls, ExtCtrls,
   EventBuilder, EbEdit, sdl_frame;

type
   [EditorCategory('Messages', 'Select Portrait')]
   [EditorContext('RM2K')]
   TfrmSelectPortrait = class(TfrmEbEditBase)
      GroupBox1: TGroupBox;
      imgPortrait: TSdlFrame;
      btnSet: TButton;
      btnClear: TButton;
      radPosition: TRadioGroup;
      GroupBox2: TGroupBox;
      chkFlipped: TCheckBox;
      procedure btnClearClick(Sender: TObject);
      procedure btnSetClick(Sender: TObject);
      procedure chkFlippedClick(Sender: TObject);
   private
      FName: string;
      FIndex: integer;
      FFlipped: boolean;
   protected
      procedure UploadObject(obj: TEbObject); override;
      procedure DownloadObject(obj: TEbObject); override;
      function NewClassType: TEbClass; override;
   end;

implementation
uses
   EB_Messages, portrait_selector, sdl_frame_helper;

{$R *.dfm}

{ TfrmEbEditBase1 }

procedure TfrmSelectPortrait.UploadObject(obj: TEbObject);
begin
   assert(obj is TEBPortrait);
   FName := obj.Text;
   if FName <> '' then
   begin
      FIndex := obj.Values[0];
      radPosition.ItemIndex := obj.Values[1];
      FFlipped := boolean(obj.Values[2]);
   end;
   imgPortrait.SetPortrait(FName, FIndex, FFlipped);
end;

procedure TfrmSelectPortrait.btnSetClick(Sender: TObject);
begin
   TfrmPortraitSelector.SelectPortraitInto(imgPortrait, FName, FIndex, FFlipped);
end;

procedure TfrmSelectPortrait.chkFlippedClick(Sender: TObject);
begin
   FFlipped := chkFlipped.Checked;
   imgPortrait.SetPortrait(FName, FIndex, FFlipped);
end;

procedure TfrmSelectPortrait.DownloadObject(obj: TEbObject);
begin
   assert(obj is TEBPortrait);
   obj.Text := FName;
   obj.Values.Clear;
   if FName <> '' then
   begin
      obj.Values.add(FIndex);
      obj.Values.add(radPosition.ItemIndex);
      obj.Values.add(ord(FFlipped));
   end;
end;

function TfrmSelectPortrait.NewClassType: TEbClass;
begin
   result := TEBPortrait;
end;

procedure TfrmSelectPortrait.btnClearClick(Sender: TObject);
begin
   imgPortrait.Clear;
   FName := '';
   FIndex := 0;
end;

initialization
   RegisterEbEditor(TEBPortrait, TfrmSelectPortrait);
finalization
   UnRegisterEbEditor(TEBPortrait);
end.
