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

unit sound_edit;

interface
uses
   Classes,
   button_edit, MusicSelector;

type
   TSoundEdit = class(TRpgCustomButtonEdit)
   protected
      procedure ButtonClick(Sender: TObject); override;
   public
      constructor Create(AOwner: TComponent); override;
   end;

implementation
uses
   Controls;

{ TSoundEdit }

procedure TSoundEdit.ButtonClick(Sender: TObject);
begin
{   if GetMusicSelector.Choose then
   begin
   end; }
end;

constructor TSoundEdit.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);

end;

end.
