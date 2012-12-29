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

unit EditEraseScreen;

interface

uses
   Forms, StdCtrls, Controls, Classes, ExtCtrls,
   EventBuilder, EbEdit, EB_Maps;

type
   TfrmEBEditShowTransition = class(TfrmEbEditBase)
      GroupBox2: TGroupBox;
      cboTransition: TComboBox;
   protected
      procedure UploadObject(obj: TEbObject); override;
      procedure DownloadObject(obj: TEbObject); override;
   end;

   TfrmEBEditShowTransition<T: TEBObject> = class(TfrmEBEditShowTransition)
   protected
      function NewClassType: TEbClass; override;
   end;

   [EditorCategory('Map', 'Erase Screen')]
   [EditorContext('RM2K')]
   TfrmEBEditEraseScreen = class(TfrmEBEditShowTransition<TEBEraseScreen>);

   [EditorCategory('Map', 'Show Screen')]
   [EditorContext('RM2K')]
   TfrmEBEditShowScreen = class(TfrmEBEditShowTransition<TEBShowScreen>)
      procedure FormShow(Sender: TObject);
   end;

implementation

{$R *.dfm}

{ TfrmEBEditShowTransition }

procedure TfrmEBEditShowTransition.DownloadObject(obj: TEbObject);
begin
   obj.Clear;
   obj.Values.Add(cboTransition.ItemIndex);
end;

procedure TfrmEBEditShowTransition.UploadObject(obj: TEbObject);
begin
   cboTransition.ItemIndex := obj.Values[0];
end;

{ TfrmEBEditShowTransition<T> }

function TfrmEBEditShowTransition<T>.NewClassType: TEbClass;
begin
   result := T;
end;

{ TfrmEBEditShowScreen }

procedure TfrmEBEditShowScreen.FormShow(Sender: TObject);
begin
   self.Caption := 'Show Screen';
end;

initialization
   RegisterEbEditor(TEBEraseScreen, TfrmEBEditEraseScreen);
   RegisterEbEditor(TEBShowScreen, TfrmEBEditShowScreen);
finalization
   UnRegisterEbEditor(TEBEraseScreen);
   UnRegisterEbEditor(TEBShowScreen);
end.
