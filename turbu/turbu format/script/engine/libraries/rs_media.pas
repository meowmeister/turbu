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
unit rs_media;

interface
uses
   turbu_sounds, turbu_defs;

   procedure playSound(name: string; volume, tempo, balance: integer);
   procedure StopMusic;
   procedure playMusic(name: string; time, volume, tempo, balance: integer);
   procedure PlayMusicData(music: TRpgMusic);
   procedure PlaySoundData(sound: TRpgSound);
   procedure PlaySystemSound(sound: TSfxTypes);
   procedure fadeOutMusic(time: integer);
   procedure MemorizeBGM;
   procedure PlayMemorizedBgm;
   procedure SetSystemSound(style: TSfxTypes; filename: string; volume, tempo, balance: integer);
   procedure SetSystemSoundData(style: TSfxTypes; sound: TRpgSound);

implementation
uses
   SysUtils,
   commons, ArchiveInterface, ArchiveUtils,
   Disharmony;

var
   MediaPlayer: IDisharmony;

procedure playSound(name: string; volume, tempo, balance: integer);
begin
   if ArchiveUtils.SoundExists(name) then
   begin
      name := IncludeTrailingPathDelimiter(GArchives[SFX_ARCHIVE].root) + name;
      commons.runThreadsafe(
         procedure begin
            MediaPlayer.PlaySoundEx(name, volume, tempo, balance);
         end, true);
   end;
end;

procedure StopMusic;
begin
   commons.runThreadsafe(procedure begin MediaPlayer.StopMusic end);
end;

var
   LLastMusic: string;
   LLastTime, LLastVolume, LLastTempo, LLastBalance: integer;
   LMemorizedBGM: TRpgMusic;

procedure playMusic(name: string; time, volume, tempo, balance: integer);
begin
   if ArchiveUtils.MusicExists(name) or (Name = '(OFF)') then
      commons.runThreadsafe(
         procedure begin
            mediaPlayer.PlayMusic(IncludeTrailingPathDelimiter(GArchives[MUSIC_ARCHIVE].root) + name);
            MediaPlayer.FadeInMusic(time);
            MediaPlayer.SetMusicVolume(volume);
            MediaPlayer.SetMusicSpeed(tempo);
            MediaPlayer.SetPanPos(balance);
            LLastMusic := name;
            LLastTime := time;
            LLastVolume := volume;
            LLastTempo := tempo;
            LLastBalance := balance;
         end, true);
end;

procedure PlayMusicData(music: TRpgMusic);
begin
   playMusic(music.filename, music.fadeIn, music.volume, music.tempo, music.balance);
end;

procedure MemorizeBGM;
begin
   commons.runThreadsafe(
      procedure begin
         LMemorizedBGM.Free;
         LMemorizedBGM := TRpgMusic.Create;
         LMemorizedBGM.filename := LLastMusic;
         LMemorizedBGM.fadeIn := LLastTime;
         LMemorizedBGM.tempo := LLastTempo;
         LMemorizedBGM.volume := LLastVolume;
         LMemorizedBGM.balance := LLastBalance;
      end, true);
end;

procedure PlayMemorizedBgm;
begin
   if assigned(LMemorizedBGM) then
      PlayMusicData(LMemorizedBGM);
end;

procedure PlaySoundData(sound: TRpgSound);
begin
   playSound(sound.filename, sound.volume, sound.tempo, sound.balance);
end;

var
   LSystemSounds: array[TSfxTypes] of TRpgSound;

procedure SetSystemSound(style: TSfxTypes; filename: string; volume, tempo, balance: integer);
var
   newSound: TRpgSound;
begin
   runThreadsafe(
      procedure begin
         newSound := TRpgSound.Create;
         newSound.filename := filename;
         newSound.volume := volume;
         newSound.tempo := tempo;
         newSound.balance := balance;
         LSystemSounds[style].Free;
         LSystemSounds[style] := newSound;
      end, true);
end;

procedure SetSystemSoundData(style: TSfxTypes; sound: TRpgSound);
begin
   SetSystemSound(style, sound.filename, sound.volume, sound.tempo, sound.balance);
end;

procedure PlaySystemSound(sound: TSfxTypes);
begin
   PlaySoundData(LSystemSounds[sound]);
end;

procedure fadeOutMusic(time: integer);
begin
   commons.runThreadsafe(procedure begin MediaPlayer.FadeOutMusic(time) end);
end;

procedure FreeSoundsAndMusic;
var
   sound: TSoundTemplate;
begin
   LMemorizedBGM.Free;
   for sound in LSystemSounds do
      sound.Free;
end;

initialization
   MediaPlayer := LoadDisharmony;
finalization
   FreeSoundsAndMusic;
end.
