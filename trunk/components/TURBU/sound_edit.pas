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
   button_edit {$IFNDEF COMPONENT}, MusicSelector, turbu_sounds{$ENDIF};

type
   TSoundEdit = class(TRpgCustomButtonEdit)
   private
      {$IFNDEF COMPONENT}
      FSelector: TfrmMusicSelector;
      FSound: TSoundTemplate;
      {$ENDIF}
   protected
      procedure ButtonClick(Sender: TObject); override;
   public
      {$IFNDEF COMPONENT}
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure Setup(const archive: string; isMusic: boolean; sound: TSoundTemplate);
      property sound: TSoundTemplate read FSound;
      {$ENDIF}
   end;

procedure Register;

implementation
uses
   Controls;

procedure Register;
begin
   RegisterComponents('TURBU', [TSoundEdit]);
end;

{ TSoundEdit }

procedure TSoundEdit.ButtonClick(Sender: TObject);
begin
{$IFNDEF COMPONENT}
   if FSelector.Choose then
   begin
      FSound.filename := FSelector.filename;
      FSound.volume := FSelector.sldVolume.Value;
      FSound.fadeIn := FSelector.sldFadeIn.Value * 10;
      FSound.balance := FSelector.sldPanning.Value;
      FSound.tempo := FSelector.sldTempo.Value;
      self.Text := FSound.filename;
   end;
{$ENDIF}
end;

{$IFNDEF COMPONENT}
constructor TSoundEdit.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);
   FSelector := TfrmMusicSelector.Create(self);
end;

destructor TSoundEdit.Destroy;
begin
   FSound.Free;
   inherited;
end;

procedure TSoundEdit.Setup(const archive: string; isMusic: boolean; sound: TSoundTemplate);
begin
   FSound.Free;
   FSound := sound;
   FSelector.Setup(archive, isMusic);
   if assigned(FSound) then
   begin
      FSelector.Filename := sound.filename;
      FSelector.sldVolume.Value := sound.volume;
      FSelector.sldFadeIn.Value := sound.fadeIn div 10;
      FSelector.sldPanning.Value := sound.balance;
      FSelector.sldTempo.Value := sound.tempo;
   end;
   self.Text := sound.filename;
end;
{$ENDIF}

end.
