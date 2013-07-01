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

unit pic_edit;

interface
uses
   Classes,
   button_edit {$IFNDEF COMPONENT}, ImageSelector, ArchiveInterface{$ENDIF};

type
   TImageEdit = class(TRpgCustomButtonEdit)
   {$IFNDEF COMPONENT}
   private
      FSelector: TfrmImageSelector;
   protected
      procedure ButtonClick(Sender: TObject); override;
   public
      constructor Create(AOwner: TComponent); override;
      procedure Setup(const archive: IArchive; const path: string; nullable: boolean = false);
   {$ENDIF}
   end;

procedure Register;

implementation
uses
   Controls;

{ TImageEdit }

{$IFNDEF COMPONENT}
procedure TImageEdit.ButtonClick(Sender: TObject);
begin
   self.Text := FSelector.Select(self.Text);
end;

constructor TImageEdit.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);
   FSelector := TfrmImageSelector.Create(self);
end;

procedure TImageEdit.Setup(const archive: IArchive; const path: string;
  nullable: boolean);
begin
   FSelector.Setup(archive, path, nullable);
end;
{$ENDIF}

procedure Register;
begin
   RegisterComponents('TURBU', [TImageEdit]);
end;

end.
