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
   Disharmony;

type
   TfrmMusicSelector = class(TForm)
      lstFilename: TListBox;
      btnPlay: TButton;
      btnStop: TButton;
      Panel1: TPanel;
      grpFadeIn: TGroupBox;
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
      procedure sldTempoChange(Sender: TObject);
   private
      FFadeInTime: integer;
      FManager: IDisharmony;
      FArchive: string;
      FFilename: string;
      FIsMusic: boolean;
      procedure ClearFilenames;
      procedure BuildList;
      procedure SetFilename(const Value: string);
      function GetVolume: integer; inline;
   public
      procedure Setup(const archive: string; isMusic: boolean);
      function Choose: boolean;
      property Filename: string read FFilename write SetFilename;
   end;

implementation
uses
   SysUtils, Math, StrUtils, IOUtils;

{$R *.dfm}

type
   TBoxedString = class
   private
      FValue: string;
   public
      constructor Create(const value: string);
      property value: string read FValue;
   end;


procedure TfrmMusicSelector.FormCreate(Sender: TObject);
begin
   FManager := Disharmony.LoadDisharmony;
end;

procedure TfrmMusicSelector.FormDestroy(Sender: TObject);
begin
   FManager := nil;
   ClearFilenames;
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

procedure TfrmMusicSelector.Setup(const archive: string; isMusic: boolean);
begin
   FArchive := archive;
   FIsMusic := isMusic;
   grpFadeIn.Visible := IsMusic;
end;

procedure TfrmMusicSelector.sldFadeInChange(Sender: TObject);
begin
   FFadeInTime := sldFadeIn.Value;
end;

procedure TfrmMusicSelector.sldPanningChange(Sender: TObject);
begin
   if FIsMusic then
      FManager.SetPanPos(round(sldPanning.Value / 2.55));
end;

procedure TfrmMusicSelector.sldTempoChange(Sender: TObject);
begin
   if FIsMusic then
      FManager.SetMusicSpeed(sldTempo.Value);
end;

function TfrmMusicSelector.GetVolume: integer;
begin
   result := sldVolume.Value;
end;

procedure TfrmMusicSelector.sldVolumeChange(Sender: TObject);
begin
   if FIsMusic then
      FManager.SetMusicVolume(GetVolume);
end;

procedure TfrmMusicSelector.btnStopClick(Sender: TObject);
begin
   if FIsMusic then
      FManager.StopMusic
   else FManager.StopSound;
end;

procedure TfrmMusicSelector.BuildList;
var
   filename: string;
begin
   ClearFilenames;
   lstFilename.AddItem('(OFF)', nil);
   for filename in TDirectory.GetFiles(FArchive, '*.*') do
      lstFilename.AddItem(ExtractFileName(filename), TBoxedString.Create(filename));
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

procedure TfrmMusicSelector.ClearFilenames;
var
   i: integer;
begin
   for i := 0 to lstFilename.Items.Count - 1 do
      lstFilename.Items.Objects[i].Free;
   lstFilename.Clear;
end;

procedure TfrmMusicSelector.btnCloseClick(Sender: TObject);
begin
   self.Close;
end;

procedure TfrmMusicSelector.btnPlayClick(Sender: TObject);
var
   filename: TBoxedString;
begin
   btnStopClick(sender);
   filename := (lstFilename.Items.Objects[lstFilename.ItemIndex] as TBoxedString);
   if assigned(filename) then
   begin
      if FIsMusic then
      begin
         FManager.PlayMusic(filename.value);
         sldPanningChange(self);
         sldVolumeChange(self);
         sldTempoChange(self);
         FManager.FadeInMusic(FFadeInTime * 10);
      end
      else FManager.PlaySoundEx(filename.value, GetVolume, sldTempo.Value, sldPanning.Value);
   end
   else FManager.StopMusic;
end;

{ TBoxedString }

constructor TBoxedString.Create(const value: string);
begin
   FValue := value;
end;

end.
