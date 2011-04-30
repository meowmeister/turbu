unit script_interface;
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

interface
uses
   types, contnrs, //windows libs
   {turbu_database,} rpg_list, charset_data, turbu_mapchars, turbu_heroes,
   events, rm_sound, turbu_map_sprites, {rpg_image,} commons, turbu_defs, //turbu libs
   turbu_map_objects,
   SDL_sprite, SG_Defs, sdl_mixer, sdlaudiomixer; //SDL libs

{$I ..\..\..\..\unfinished.inc}

type
   TRpgMediaPlayer = class(TObject)
   private
      FBgm: string;
      FSavedBgm: TSDLMusic;
      FBgmLooped: boolean;
      FMediaPlayer: TSDLAudioManager;
      FCurrentTrack: TSDLMusic;
      FFadeTime: word;
      FLoadName: string;
      FSyncWait: boolean;
      FPlayOnce: boolean;
      FSystemSfx: array[TSfxTypes] of TSDLAudio;
      FSystemBgmRecord: array[TBgmTypes] of TRmMusic;
      FSystemBgmTrack: array[TBgmTypes] of TSdlMusic;

      function getBgmPosition: integer;
      procedure syncPlayMusic;
      procedure syncLoadFile;
   public
      constructor create;
      destructor Destroy; override;
      procedure playMusic(musicFile: TRmMusic);
      procedure loadSystemSound(which: TSfxTypes; soundFile: TRmSound);
      procedure playSfx(soundFile: TRmSound);
      procedure playAndFreeSfx(soundFile: TRmSound);
      procedure setSystemSound(which: TSfxTypes; filename: string);
      procedure playSystemSound(which: turbu_defs.TSfxTypes);
      procedure playSystemMusic(which: TBgmTypes);
      procedure loadSystemMusic(which: TBgmTypes; soundFile: TRmMusic);
      procedure loop;
      procedure restart;
      procedure stopMusic;
      procedure fadeOut(time: integer);
      procedure fadeIn(time: integer);
      procedure memorizeBgm;
      procedure playMemorizedBgm;

      property position: integer read getBgmPosition;
      property looped: boolean read FBgmLooped;
      property player: TSdlAudioManager read FMediaPlayer;
      property currentTrack: TSDLMusic read FCurrentTrack write FCurrentTrack;
      property syncWait: boolean read FSyncWait;
   end;

   TRpgGlobalEvent = class(TObject)
   private
      FID: word;
      FEvent: TEvent;
   public
      constructor create(base: TEvent; id: word);
      procedure update;

      property event: TEvent read FEvent;
   end;

   TRpgTimer = class(TObject)
   private
      FTimerSprite: TSprite;
      FSecs: word;
      FTimeRemaining: integer;
      FActivated: boolean;
      FVisible: boolean;
      FInBattle: boolean;

      procedure setVisible(value: boolean);
   public
      constructor create(sprite: TSprite);

      procedure start; overload;
      procedure start(const visible, inBattle: boolean); overload;
      procedure pause;
      procedure reset;
      procedure tick;

      property time: integer read FTimeRemaining write FTimeRemaining;
      property visible: boolean read FVisible write setVisible;
      property inBattle: boolean read FInBattle write FInBattle;
      property active: boolean read FActivated;
   end;

   TScriptInterface = class(TObject)
   private
      FEngine: TObject;

{      function getImage(x: word): TRpgImage;
      procedure setImage(x: word; const Value: TRpgImage);}
      procedure enableMenu(const Value: boolean);
      function isMenuEnabled: boolean;
      function getVehicle(x: TVehicleSet): TRpgVehicle;
      procedure notifyOnLevelGain(value: boolean);
      procedure canDieOnHpChange(value: boolean);
      procedure setVariable(index: word; value: integer);
      function getVariable(index: word): integer;
      procedure setSwitch(index: word; value: boolean);
      function getSwitch(index: word): boolean;
      function getHero(index: word): TRpgHero;
      procedure setTimer(value: TRpgTimer);
      procedure setTimer2(value: TRpgTimer);
      function getCash: integer;
      procedure setCash(value: integer);
      function getTimer: TRpgTimer;
      function getTimer2: TRpgTimer;
//      function getPlayer: TRpgMediaPlayer;
      function getEvent(index: word): TRpgEvent;
      function getParty: TRpgParty;
      function getPartySize: byte;
      function getSaveCount: word;
      function battleCount: word;
      function battleVictories: word;
      function battleLosses: word;
      function battleFlees: word;
   public
      constructor create(engine: TObject);

      property variable[x: word]: integer read getVariable write setVariable;
      property switch[x: word]: boolean read getSwitch write setSwitch;
      property hero[x: word]: TRpgHero read getHero;
      property event[x: word]: TRpgEvent read getEvent;
      property vehicle[x: TVehicleSet]: TRpgVehicle read getVehicle;
      property party: TRpgParty read getParty;
      property money: integer read getCash write setCash;
      property partySize: byte read getPartySize;
      property saveCount: word read getSaveCount;
      property battles: word read battleCount;
      property victories: word read battleVictories;
      property losses: word read battleLosses;
      property flees: word read battleFlees;
      property timer: TRpgTimer read getTimer write setTimer;
      property timer2: TRpgTimer read getTimer2 write setTimer2;
//      property BGM: TRpgMediaPlayer read getPlayer;
      property levelGainNotify: boolean write notifyOnLevelGain;
      property deathPossible: boolean write canDieOnHpChange;
      property menuEnabled: boolean read isMenuEnabled write enableMenu;
//      property image[x: word]: TRpgImage read getImage write setImage;
   end;

implementation

uses sysUtils, classes, math, //windows
     {script_engine, chipset_graphics, charset_graphics, tiles, frames,}
     locate_files, {LMU,} //turbu
     SDL;

{ TScriptInterface }

function TScriptInterface.battleCount: word;
begin
   result := 0;
end;

function TScriptInterface.battleFlees: word;
begin
   result := 0;
end;

function TScriptInterface.battleLosses: word;
begin
   result := 0;
end;

function TScriptInterface.battleVictories: word;
begin
   result := 0;
end;

procedure TScriptInterface.canDieOnHpChange(value: boolean);
begin
//   GParty.deathPossible := value;
end;

constructor TScriptInterface.create(engine: TObject);
begin
{   if not (engine is TScriptEngine) then
      raise EFatalError.create('Bad engine used to initialize script interface!');
   FEngine := engine;}
end;

procedure TScriptInterface.enableMenu(const Value: boolean);
begin
//   GGameEngine.menuEnabled := value;
end;

function TScriptInterface.getCash: integer;
begin
//   result := GParty.money;
end;

function TScriptInterface.getEvent(index: word): TRpgEvent;
var
   i: integer;
begin
{   result := GRpgEvents[0];
   for i := 1 to high(GRpgEvents) do
      if GRpgEvents[i].id = index then
      begin
         result := GRpgEvents[i];
         Exit;
      end;
   //end FOR}
end;

function TScriptInterface.getHero(index: word): TRpgHero;
begin
{   if (index > GCurrentEngine.heroes) or (index < 1) then
      result := GCurrentEngine.hero[0]
   else result := GCurrentEngine.hero[index];}
end;

{function TScriptInterface.getImage(x: word): TRpgImage;
begin
   if not x in [1..high(GImages)] then
      result := GImages[0]
   else result := GImages[x];
end;}

function TScriptInterface.getParty: TRpgParty;
begin
//   result := GParty;
end;

function TScriptInterface.getPartySize: byte;
var
  I: Integer;
begin
{   result := 0;
   for I := 1 to MAXPARTYSIZE do
      if GParty[i] <> GCurrentEngine.hero[0] then
         inc(result);
      //end if
   //end FOR}
end;

(*
function TScriptInterface.getPlayer: TRpgMediaPlayer;
begin
//   result := GCurrentEngine.mediaPlayer;
end;
*)

function TScriptInterface.getSaveCount: word;
begin
   result := 0;
end;

function TScriptInterface.getSwitch(index: word): boolean;
begin
{   if (index > high(GSwitches)) then
      index := 0;
   result := GSwitches[index];}
end;

function TScriptInterface.getTimer: TRpgTimer;
begin
//   result := GCurrentEngine.timer;
end;

function TScriptInterface.getTimer2: TRpgTimer;
begin
//   result := GCurrentEngine.timer2;
end;

function TScriptInterface.getVariable(index: word): integer;
begin
{   if (index > high(GVariables)) then
      index := 0;
   result := GVariables[index];}
end;

function TScriptInterface.getVehicle(x: TVehicleSet): TRpgVehicle;
begin
//   result := GVehicles[ord(x)];
end;

function TScriptInterface.isMenuEnabled: boolean;
begin
//   result := GGameEngine.menuEnabled;
end;

procedure TScriptInterface.notifyOnLevelGain(value: boolean);
begin
//   GParty.levelNotify := value;
end;

procedure TScriptInterface.setCash(value: integer);
begin
//   GParty.money := lesserOf(greaterOf(value, 0), MAXGOLD);
end;

{procedure TScriptInterface.setImage(x: word; const Value: TRpgImage);
begin
   if (value = nil) or (not x in [0..high(GImages)]) then
      Exit;

   if (GImages[x] <> nil) then
      GImages[x].Dead;
   GImages[x] := Value;
   GImages[x].z := 20 + x;
end;}

procedure TScriptInterface.setSwitch(index: word; value: boolean);
begin
{   if index > high(GSwitches) then
      index := 0;
   GSwitches[index] := value;}
end;

procedure TScriptInterface.setTimer(value: TRpgTimer);
begin
//   GCurrentEngine.timer := value;
end;

procedure TScriptInterface.setTimer2(value: TRpgTimer);
begin
//   GCurrentEngine.timer2 := value;
end;

procedure TScriptInterface.setVariable(index: word; value: integer);
begin
{   if index > high(GVariables) then
      index := 0;
   GVariables[index] := value;}
end;

{ TRpgTimer }

constructor TRpgTimer.create(sprite: TSprite);
var dummy, msec: word;
begin
//   assert(sprite is TSystemTimer);
   FTimerSprite := sprite;
   decodeTime(sysUtils.GetTime, dummy, dummy, FSecs, msec);
   if MSec >= 500 then
      inc(FSecs);
   //end if
end;

procedure TRpgTimer.pause;
begin
   FActivated := false;
end;

procedure TRpgTimer.reset;
begin
   FActivated := false;
   FVisible := false;
   FTimeRemaining := 0;
end;

procedure TRpgTimer.setVisible(value: boolean);
begin
   if value = FVisible then
      Exit;

   FVisible := value;
   FTimerSprite.visible := value;
end;

procedure TRpgTimer.start(const visible, inBattle: boolean);
begin
   self.visible := visible;
   FInBattle := inBattle;
   FActivated := true;
end;

procedure TRpgTimer.start;
begin
   FActivated := true;
end;

procedure TRpgTimer.tick;
var dummy, secs: word;
begin
   if not FActivated then
      Exit;

   decodeTime(sysUtils.GetTime, dummy, dummy, secs, dummy);
   if (secs = 0) and (FSecs = 59) then
      secs := 60;
   if (secs - FSecs > 0) and (FTimeRemaining > 0) then
   begin
      dec(FTimeRemaining);
      FSecs := secs mod 60;
   end;
end;

{ TRpgMediaPlayer }

constructor TRpgMediaPlayer.create;
begin
   FBgm := '';
   FMediaPlayer := TSDLAudioManager.Create;
   FCurrentTrack := nil;
end;

destructor TRpgMediaPlayer.Destroy;
begin
   FMediaPlayer.free;
   inherited;
end;

procedure musicLoopedHook;
begin
//   GCurrentEngine.mediaPlayer.loop;
end;

procedure TRpgMediaPlayer.fadeOut(time: integer);
begin
   Mix_HookMusicFinished(nil);
//   Mix_FadeOutMusic(time);
end;

procedure TRpgMediaPlayer.fadeIn(time: integer);
begin
{   FCurrentTrack.FadeIn(time);
   FCurrentTrack.OnMusicFinished := musicLoopedHook;
   FBgmLooped := false; }
end;

function TRpgMediaPlayer.getBgmPosition: integer;
begin
   result := 0;
end;

procedure TRpgMediaPlayer.loadSystemMusic(which: TBgmTypes;
  soundFile: TRmMusic);
var dummy: TSDLMusic;
begin
   if soundFile.filename = '' then
      Exit;

   dummy := TSDLMusic.Create(soundFile.filename);
   FMediaPlayer.MusicManager.Add(dummy);
   FSystemBgmRecord[which].Free;
   FSystemBgmRecord[which] := soundFile;
end;

procedure TRpgMediaPlayer.loadSystemSound(which: TSfxTypes;
  soundFile: TRmSound);
var dummy: TSDLSoundEffect;
begin
   if (soundFile = nil) or (soundFile.filename = '') then
      Exit;

   dummy := TSDLSoundEffect.Create(soundFile.filename);
   dummy.SetPanning(soundFile.left);
   FMediaPlayer.SoundEffectManager.Add(dummy, soundFile.filename);
   FSystemSfx[which].Free;
   FSystemSfx[which] := dummy;
end;

procedure TRpgMediaPlayer.loop;
begin
   FCurrentTrack.Play;
   FBgmLooped := true;
end;

procedure TRpgMediaPlayer.memorizeBgm;
begin
   FSavedBgm := FCurrentTrack;
end;

procedure TRpgMediaPlayer.playAndFreeSfx(soundFile: TRmSound);
begin
   playSfx(soundFile);
   soundFile.Free;
end;

procedure TRpgMediaPlayer.playMemorizedBgm;
begin
   if assigned(FSavedBgm) then
   begin
      self.stopMusic;
      FCurrentTrack := FSavedBgm;
//      SDL_Delay(100);
      FCurrentTrack.Play;
   end;
end;

procedure TRpgMediaPlayer.playMusic(musicFile: TRmMusic);
var
   dummy: smallint;
begin
   FBgmLooped := false;
   if FCurrentTrack <> nil then
      FCurrentTrack.Stop;
   dummy := FMediaPlayer.MusicManager.IndexOf(musicFile.filename);
   if dummy = -1 then
   begin
      FLoadname := musicFile.filename;
      FSyncWait := true;
{      if GCurrentThread <> nil then
         GCurrentThread.syncRun(self.syncLoadFile)
      else} self.syncLoadFile;
      while FSyncWait do
         sdl_delay(10);
   end
   else FCurrentTrack := FMediaPlayer.MusicManager[dummy];
   FCurrentTrack.OnMusicFinished := musicLoopedHook;
   FFadeTime := musicFile.fadeIn;
   FPlayOnce := false;
{   if GCurrentThread <> nil then
      GCurrentThread.syncRun(self.syncPlayMusic)
   else} self.syncPlayMusic;
end;

procedure TRpgMediaPlayer.playSfx(soundFile: TRmSound);
var
   dummy: TSDLSoundEffect;
   index: integer;
begin
   if (soundFile = nil) or (soundFile.filename = '') then
      Exit;

   index := FMediaPlayer.SoundEffectManager.IndexOf(soundFile.filename);
   if index = -1 then
   begin
      dummy := TSDLSoundEffect.Create(soundFile.filename);
      dummy.SetPanning(soundFile.left);
      dummy.Volume := soundFile.volume;
      index := FMediaPlayer.SoundEffectManager.Add(dummy, soundFile.filename);
   end;
   FMediaPlayer.SoundEffectManager[index].Play;
end;

procedure TRpgMediaPlayer.playSystemMusic(which: TBgmTypes);
begin
   FCurrentTrack.OnMusicFinished := nil;
   if FSystemBgmRecord[which] <> nil then
   begin
//      Mix_VolumeMusic(128);
      if FSystemBgmTrack[which] = nil then
         FSystemBgmTrack[which] := TSDLMusic.Create(FSystemBgmRecord[which].filename);
      FSystemBgmTrack[which].play(1);
      case which of
         bgm_Inn: ;
         else ;
      end;
   end;
end;

procedure TRpgMediaPlayer.playSystemSound(which: turbu_defs.TSfxTypes);
begin
   if (FSystemSfx[which] <> nil) then
      FSystemSfx[which].Play;
   //end if
end;

procedure TRpgMediaPlayer.restart;
begin
   FCurrentTrack.Play;
   FBgmLooped := false;
end;

procedure TRpgMediaPlayer.setSystemSound(which: TSfxTypes; filename: string);
var
   newsound: TSDLSoundEffect;
   dummy: TRmSound;
begin
   if (filename = '') or (filename = '''OFF''') then
   begin
      FSystemSfx[which].Free;
      Exit;
   end;

   dummy := TRmSound.Create(filename);
   newsound := TSDLSoundEffect.Create(dummy.filename);
   newsound.SetPanning(dummy.left);
   FMediaPlayer.SoundEffectManager.Add(newsound);
   FSystemSfx[which].Free;
   FSystemSfx[which] := newsound;
   dummy.free;
end;

procedure TRpgMediaPlayer.stopMusic;
begin
   if assigned(FCurrentTrack) and (FCurrentTrack.IsPlaying) then
      FCurrentTrack.Stop;
end;

procedure TRpgMediaPlayer.syncLoadFile;
begin
   FCurrentTrack := TSDLMusic.Create(FLoadName);
   FMediaPlayer.MusicManager.Add(FCurrentTrack);
   FSyncWait := false;
end;

procedure TRpgMediaPlayer.syncPlayMusic;
begin
   if FFadeTime > 0 then
      FCurrentTrack.FadeIn(FFadeTime)
   else FCurrentTrack.play;
   FSyncWait := false;
end;

{ TRpgGlobalEvent }

constructor TRpgGlobalEvent.create(base: TEvent; id: word);
begin
   inherited Create;
   FID := id;
   FEvent := base;
end;

procedure TRpgGlobalEvent.update;
var
   page: TEventPage;
begin
   assert(assigned(FEvent));
   FEvent.locked := false;
   page := FEvent.lastCurrentPage;
   assert(assigned(page));
{   if (page.hasScript) and (page.startCondition in [parallel, automatic]) and (not FEvent.playing) and page.valid then
      GCurrentEngine.executeEvent(FEvent, nil);}
end;

end.
