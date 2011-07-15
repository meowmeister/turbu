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

unit MusicSelector;

interface

uses
   Forms, StdCtrls, Classes, Controls, JvExControls, JvxSlider, ExtCtrls,
   SdlAudioMixer;

type
   TfrmMusicSelector = class(TForm)
      lstFilename: TListBox;
      btnPlay: TButton;
      btnStop: TButton;
      Panel1: TPanel;
      GroupBox1: TGroupBox;
      sldVolume: TJvxSlider;
      sldPanning: TJvxSlider;
      sldTempo: TJvxSlider;
      sldFadeIn: TJvxSlider;
      btnClose: TButton;
      btnSelect: TButton;
      procedure FormCreate(Sender: TObject);
      procedure FormDestroy(Sender: TObject);
      procedure btnPlayClick(Sender: TObject);
      procedure btnStopClick(Sender: TObject);
      procedure sldVolumeChange(Sender: TObject);
      procedure sldPanningChange(Sender: TObject);
      procedure sldFadeInChange(Sender: TObject);
      procedure btnCloseClick(Sender: TObject);
      procedure FormShow(Sender: TObject);
   private
      FCurrentSound: TSDLAudio;
      FFadeInTime: integer;
      FManager: TSDLAudioManager;
   public
//      function Choose: boolean;
   end;

function GetMusicSelector: TfrmMusicSelector;

implementation
uses
   SysUtils,
   ArchiveInterface,
   SDL_mixer;

{$R *.dfm}

var
   LSelector: TfrmMusicSelector;

function GetMusicSelector: TfrmMusicSelector;
begin
   if not assigned(LSelector) then
      LSelector := TfrmMusicSelector.Create(nil);
   result := LSelector;
end;

procedure TfrmMusicSelector.FormCreate(Sender: TObject);
begin
   FManager := TSDLAudioManager.Create;
end;

procedure TfrmMusicSelector.FormDestroy(Sender: TObject);
begin
   FCurrentSound.Free;
   FManager.Free;
end;

procedure TfrmMusicSelector.FormShow(Sender: TObject);
var
   filename: string;
begin
   for filename in GArchives[MUSIC_ARCHIVE].allFiles do
      lstFilename.AddItem(filename, nil);
end;

procedure TfrmMusicSelector.sldFadeInChange(Sender: TObject);
begin
   FFadeInTime := sldFadeIn.Value;
end;

procedure TfrmMusicSelector.sldPanningChange(Sender: TObject);
begin
   if assigned(FCurrentSound) then
      FCurrentSound.Panning := sldPanning.Value;
end;

procedure TfrmMusicSelector.sldVolumeChange(Sender: TObject);
begin
   if assigned(FCurrentSound) then
      (FCurrentSound.Volume := round(sldVolume.Value * 1.28));
end;

procedure TfrmMusicSelector.btnStopClick(Sender: TObject);
begin
   FreeAndNil(FCurrentSound);
end;

procedure TfrmMusicSelector.btnCloseClick(Sender: TObject);
begin
   self.Close;
end;

procedure TfrmMusicSelector.btnPlayClick(Sender: TObject);
var
   filename: string;
   stream: TStream;
begin
   btnStopClick(sender);
   filename := lstFilename.Items[lstFilename.ItemIndex];
   if UpperCase(ExtractFileExt(filename)) = '.MP3' then
   begin
      Application.MessageBox('The current version of TURBU does not support MP3 music', 'MP3s not supported');
      Exit;
   end;

   stream := GArchives[MUSIC_ARCHIVE].getFile(filename);
   try
      FCurrentSound := TSDLMusic.Create(stream);
   finally
//      stream.Free;
   end;
   FCurrentSound.Channel := 0;
   FCurrentSound.FadeIn(FFadeInTime * 10, -1);
   sldVolumeChange(self);
   sldPanningChange(self);
end;

initialization
finalization
   FreeAndNil(LSelector);
end.
