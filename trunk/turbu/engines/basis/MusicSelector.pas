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
   ArchiveInterface, SdlAudioMixer;

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
      FArchive: IArchive;
      FFilename: string;
      procedure BuildList;
      procedure SetFilename(const Value: string);
   public
      procedure Setup(const archive: IArchive);
      function Choose: boolean;
      property Filename: string read FFilename write SetFilename;
   end;

implementation
uses
   SysUtils, Math, StrUtils,
   SDL_mixer;

{$R *.dfm}

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
begin
   BuildList;
   if FFilename = '' then
      FFilename := '(OFF)';
   SetFilename(FFilename);
end;

procedure TfrmMusicSelector.SetFilename(const Value: string);
var
   i: Integer;
   tFilename: string;
begin
   FFilename := Value;
   lstFilename.ItemIndex := max(lstFilename.Items.IndexOf(FFilename), 0);
   if (lstFilename.ItemIndex = 0) and (lstFilename.items[lstFilename.ItemIndex] <> FFilename) then
   begin
      tFilename := FFilename + '.';
      for i := 0 to lstFilename.Items.Count - 1 do
         if AnsiStartsText(tFilename, lstFilename.Items[i]) then
         begin
            lstFilename.ItemIndex := i;
            break;
         end;
   end;
end;

procedure TfrmMusicSelector.Setup(const archive: IArchive);
begin
   FArchive := archive;
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

procedure TfrmMusicSelector.BuildList;
var
   filename: string;
begin
   lstFilename.Clear;
   lstFilename.AddItem('(OFF)', nil);
   for filename in FArchive.allFiles do
      lstFilename.AddItem(filename, nil);
end;

function TfrmMusicSelector.Choose: boolean;
begin
   btnSelect.Visible := true;
   try
      result := self.ShowModal = mrOK;
   finally
      btnSelect.Visible := false;
   end;
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

   stream := FArchive.getFile(filename);
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

end.
