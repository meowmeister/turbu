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
   turbu_sounds, turbu_defs, dwsJSON;

   procedure playSound(name: string; volume, tempo, balance: integer);
   procedure StopMusic;
   procedure playMusic(name: string; time, volume, tempo, balance: integer);
   procedure PlayMusicData(music: TRpgMusic);
   procedure PlaySoundData(sound: TRpgSound);
   procedure PlaySystemSound(sound: TSfxTypes);
   procedure PlaySystemMusic(music: TBgmTypes; once: boolean = false);
   procedure fadeOutMusic(time: integer);
   procedure fadeInLastMusic(time: integer);
   procedure MemorizeBGM;
   procedure PlayMemorizedBgm;
   procedure SetSystemSound(style: TSfxTypes; filename: string; volume, tempo, balance: integer);
   procedure SetSystemSoundData(style: TSfxTypes; sound: TRpgSound);
   procedure SetSystemMusic(style: TBgmTypes; filename: string; fadeIn, volume, tempo, balance: integer);
   procedure SetSystemMusicData(style: TBgmTypes; music: TRpgMusic);

   procedure SerializeSound(writer: TdwsJSONWriter);
   procedure DeserializeSound(obj: TdwsJSONObject);

implementation
uses
   SysUtils,
   commons, ArchiveInterface, ArchiveUtils, turbu_script_engine, turbu_classes,
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
   LMemorizedBGM, LFadedBGM: TRpgMusic;

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

procedure MemorizeMusic(var music: TRpgMusic);
begin
   music.Free;
   music := TRpgMusic.Create;
   music.filename := LLastMusic;
   music.fadeIn := LLastTime;
   music.tempo := LLastTempo;
   music.volume := LLastVolume;
   music.balance := LLastBalance;
end;

procedure MemorizeBGM;
begin
   commons.runThreadsafe( procedure begin MemorizeMusic(LMemorizedBGM) end, true);
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
   LSystemMusic: array[TBgmTypes] of TRpgMusic;

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

function WaitForMusicPlayed: boolean;
begin
   result := MediaPlayer.GetMusicLooping > 0;
end;

procedure PlaySystemMusic(music: TBgmTypes; once: boolean);
begin
   PlayMusicData(LSystemMusic[music]);
   if once then
      GScriptEngine.SetWaiting(WaitForMusicPlayed);
end;

procedure SetSystemMusic(style: TBgmTypes; filename: string; fadeIn, volume, tempo, balance: integer);
var
   newMusic: TRpgMusic;
begin
   runThreadsafe(
      procedure begin
         newMusic := TRpgMusic.Create;
         newMusic.filename := filename;
         newMusic.volume := volume;
         newMusic.tempo := tempo;
         newMusic.balance := balance;
         newMusic.fadeIn := fadeIn;
         LSystemMusic[style].Free;
         LSystemMusic[style] := newMusic;
      end, true);
end;

procedure SetSystemMusicData(style: TBgmTypes; music: TRpgMusic);
begin
   SetSystemMusic(style, music.filename, music.fadeIn, music.volume, music.tempo, music.balance);
end;

procedure fadeOutMusic(time: integer);
begin
   commons.runThreadsafe(
      procedure begin
         MediaPlayer.FadeOutMusic(time);
         MemorizeMusic(LFadedBGM);
      end);
end;

procedure fadeInLastMusic(time: integer);
begin
   if assigned(LFadedBGM) then
   begin
      LFadedBGM.fadeIn := time;
      PlayMusicData(LFadedBGM);
   end;
end;

procedure SerializeSystemSound(writer: TdwsJSONWriter);
var
   currentMusic: TRpgMusic;
   currentSound: TRpgSound;
begin
   writer.WriteName('SystemSounds');
   writer.BeginArray;
      for currentSound in LSystemSounds do
         currentSound.Serialize(writer);
   writer.EndArray;
   writer.WriteName('SystemMusic');
   writer.BeginArray;
      for currentMusic in LSystemMusic do
         currentMusic.Serialize(writer);
   writer.EndArray;
end;

procedure SerializeSound(writer: TdwsJSONWriter);
var
   current: TRpgMusic;
begin
   current := nil;
   writer.BeginObject;
      writer.WriteName('CurrentBGM');
      try
         MemorizeMusic(current);
         current.Serialize(writer);
      finally
         current.Free;
      end;
      if assigned(LFadedBGM) then
      begin
         writer.WriteName('FadedBGM');
         LFadedBGM.serialize(writer);
      end;
      if assigned(LMemorizedBGM) then
      begin
         writer.WriteName('MemorizedBGM');
         LMemorizedBGM.serialize(writer);
      end;
      SerializeSystemSound(writer);
   writer.EndObject;
end;

procedure DeserializeSystemSound(obj: TdwsJSONObject);
var
   item: TdwsJSONValue;
   arr: TdwsJSONArray;
   sfx: TSfxTypes;
   bgm: TBgmTypes;
begin
   arr := obj.Items['SystemSounds'] as TdwsJSONArray;
   for sfx := Low(TSfxTypes) to High(TSfxTypes) do
   begin
      LSystemSounds[sfx].Free;
      LSystemSounds[sfx] := TRpgSound.Create;
      LSystemSounds[sfx].deserialize(arr.Elements[ord(sfx)] as TdwsJSONObject);
   end;
   arr.Free;

   arr := obj.Items['SystemMusic'] as TdwsJSONArray;
   for bgm := Low(TBgmTypes) to High(TBgmTypes) do
   begin
      LSystemMusic[bgm].Free;
      LSystemMusic[bgm] := TRpgMusic.Create;
      LSystemMusic[bgm].deserialize(arr.Elements[ord(bgm)] as TdwsJSONObject);
   end;
   arr.Free;
end;

procedure DeserializeSound(obj: TdwsJSONObject);
var
   item: TdwsJSONValue;
   current: TRpgMusic;
begin
   item := obj.Items['FadedBGM'];
   if assigned(item) then
   begin
      LFadedBGM.Free;
      LFadedBGM := TRpgMusic.Create;
      LFadedBGM.deserialize(item as TdwsJSONObject);
      item.Free;
   end;
   item := obj.Items['MemorizedBGM'];
   if assigned(item) then
   begin
      LMemorizedBGM.Free;
      LMemorizedBGM := TRpgMusic.Create;
      LMemorizedBGM.deserialize(item as TdwsJSONObject);
      item.Free;
   end;
   DeserializeSystemSound(obj);
   item := obj.Items['CurrentBGM'];
   assert(assigned(item));
   current := TRpgMusic.Create;
   try
      current.deserialize(item as TdwsJSONObject);
      item.Free;
      PlayMusicData(current);
   finally
      current.Free;
   end;
   obj.CheckEmpty;
end;

procedure FreeSoundsAndMusic;
var
   sound: TSoundTemplate;
begin
   LMemorizedBGM.Free;
   LFadedBGM.Free;
   for sound in LSystemSounds do
      sound.Free;
   for sound in LSystemMusic do
      sound.Free;
end;

initialization
   MediaPlayer := LoadDisharmony;
finalization
   FreeSoundsAndMusic;
end.
