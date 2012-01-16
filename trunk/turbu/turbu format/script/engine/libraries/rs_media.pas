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
   turbu_sounds;

   procedure playSound(name: string; volume, tempo, balance: integer);
   procedure StopMusic;
   procedure playMusic(name: string; time, volume, tempo, balance: integer);
   procedure PlayMusicData(music: TRpgMusic);
   procedure PlaySoundData(sound: TRpgSound);
   procedure fadeOutMusic(time: integer);

implementation
uses
   SysUtils,
   ArchiveInterface, ArchiveUtils,
   Disharmony;

var
   MediaPlayer: IDisharmony;

procedure playSound(name: string; volume, tempo, balance: integer);
begin
   if ArchiveUtils.SoundExists(name) then
      MediaPlayer.PlaySoundEx(IncludeTrailingPathDelimiter(GArchives[SFX_ARCHIVE].root) + name, volume, tempo, balance);
end;

procedure StopMusic;
begin
   MediaPlayer.StopMusic;
end;

procedure playMusic(name: string; time, volume, tempo, balance: integer);
begin
   if ArchiveUtils.MusicExists(name) or (Name = '(OFF)') then
   begin
      mediaPlayer.PlayMusic(IncludeTrailingPathDelimiter(GArchives[MUSIC_ARCHIVE].root) + name);
      MediaPlayer.FadeInMusic(time);
      MediaPlayer.SetMusicVolume(volume);
      MediaPlayer.SetMusicSpeed(tempo);
      MediaPlayer.SetPanPos(balance);
   end;
end;

procedure PlayMusicData(music: TRpgMusic);
begin
   playMusic(music.filename, music.fadeIn, music.volume, music.tempo, music.balance);
end;

procedure PlaySoundData(sound: TRpgSound);
begin
   playSound(sound.filename, sound.volume, sound.tempo, sound.balance);
end;

procedure fadeOutMusic(time: integer);
begin
   MediaPlayer.FadeOutMusic(time);
end;

initialization
   MediaPlayer := LoadDisharmony;
end.
