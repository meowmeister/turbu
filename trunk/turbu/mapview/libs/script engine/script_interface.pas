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
   turbu_database, hero_data, item_data, rpg_list, charset_data, script_backend,
   events, rm_sound, turbu_map_sprites, {rpg_image,} commons, turbu_defs, //turbu libs
   turbu_map_objects,
   SDL_sprite, SG_Defs, sdl_mixer, sdlaudiomixer; //SDL libs

{$I ..\..\..\..\unfinished.inc}
const
   MAXGOLD = 999999;
   MAXPARTYSIZE = 4;

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

   TRpgCharacter = class(TInterfacedObject, IRpgCharacter)
   private
      function getScreenX: integer; inline;
      function getScreenY: integer; inline;
      function getScreenXP: integer; inline;
      function getScreenYP: integer; inline;
   protected
      procedure doFlash(r, g, b, power: byte; time: cardinal); virtual; abstract;
      function getX: word; virtual; abstract;
      function getY: word; virtual; abstract;
      function getBase: TMapSprite; virtual; abstract;
      function getTranslucency: byte; virtual;
      procedure setTranslucency(const Value: byte); virtual;
   public
      procedure flash(r, g, b, power: byte; time: cardinal; wait: boolean);
      procedure move(frequency: byte; skip: boolean; route: word);
      procedure ChangeSprite(name: string; index: integer; oldSprite: TMapSprite); virtual; abstract;

      property screenX: integer read getScreenX;
      property screenY: integer read getScreenY;
      property screenXP: integer read getScreenXP;
      property screenYP: integer read getScreenYP;
      property base: TMapSprite read getBase;
      property translucency: byte read getTranslucency write setTranslucency;
   end;

   TRpgEvent = class(TRpgCharacter)
   private
      FID: word;
      FTile: TSprite;
      FBase: TMapSprite;
      FIsChar: boolean;
      FEvent: TRpgMapObject;

      function getLocation: TSgPoint;
      procedure setLocation(const Value: TSgPoint);
      function getMap: word;
      function getFacing: byte;
   protected
      function getX: word; override;
      function getY: word; override;
      function getBase: TMapSprite; override;
      procedure doFlash(r, g, b, power: byte; time: cardinal); override;
   public
      constructor create(base: TMapSprite);
      destructor Destroy; override;
      destructor delete;
      procedure update;
      procedure ChangeSprite(name: string; index: integer; oldSprite: TMapSprite); override;
      procedure switchType;

      property map: word read getMap;
      property x: word read getX;
      property y: word read getY;
      property facing: byte read getFacing;
      property id: word read FID;
      property base: TMapSprite read FBase;
      property location : TSgPoint read getLocation write setLocation;
   end;

   TRpgParty = class(TRpgCharacter)
   private
      //data
      FCash: integer;
      FParty: array[1..MAXPARTYSIZE] of TRpgHero;
      FInventory: TRpgInventory;

      //flags
      FLevelNotify: boolean;
      FDeathPossible: boolean;

      function getMap: word;
      procedure setY(const Value: word);
      procedure setX(const Value: word);
      function getFacing: byte;
      procedure setFacing(const Value: byte);
      function getHero(x: byte): TRpgHero;
      procedure setHero(x: byte; value: TRpgHero);
      function empty: boolean;
   protected
      function getX: word; override;
      function getY: word; override;
      function getTranslucency: byte; override;
      procedure setTranslucency(const Value: byte); override;
      procedure doFlash(r, g, b, power: byte; time: cardinal); override;
      function getBase: TMapSprite; override;
   public
      constructor Create(database: TRpgDatabase);
      destructor Destroy; override;
      procedure addItem(const id, number: word);
      procedure removeItem(const id, number: word);
      procedure addExp(const id: smallint; number: integer);
      procedure removeExp(const id: smallint; number: integer);
      procedure addLevels(const id: smallint; number: integer);
      procedure removeLevels(const id: smallint; number: integer);
      procedure sort;
      procedure ChangeSprite(name: string; index: integer; oldSprite: TMapSprite); override;

      function takeDamage(power: word; defense, mDefense, variance: byte): word;
      function openSlot: byte;
      function size: byte;
      function indexOf(who: TRpgHero): integer;

      property money: integer read FCash write FCash;
      property inventory: TRpgInventory read FInventory write FInventory;
      property hero[x: byte]: TRpgHero read getHero write setHero; default;
      property levelNotify: boolean read FLevelNotify write FLevelNotify;
      property deathPossible: boolean read FDeathPossible write FDeathPossible;
      property facing: byte read getFacing write setFacing;
      property x: word read getX write setX;
      property y: word read getY write setY;
      property map: word read getMap;
   end;

   TRpgVehicle = class(TRpgCharacter)
   private
      FSprite: string;
      FSpriteIndex: byte;
      FMap: smallint;
      FX: word;
      FY: word;
      FGameSprite: TMapSprite;
      FVehicleType: TVehicleSet;
      FCarrying: TRpgParty;

      function getLocation: TSgPoint;
      procedure setLocation(const Value: TSgPoint);
      procedure setX(const Value: word);
      procedure setY(const Value: word);
      procedure setMap(const Value: smallint);
      function getFacing: byte;
      procedure setFacing(const Value: byte);
   protected
      function getX: word; override;
      function getY: word; override;
      function getBase: TMapSprite; override;
      procedure doFlash(r, g, b, power: byte; time: cardinal); override;
   public
      constructor Create({mapTree: TFullTree;} which: TVehicleSet);
      destructor Destroy; override;
      procedure setSprite(filename: string; index: byte);
      procedure ChangeSprite(name: string; index: integer; oldSprite: TMapSprite); override;
      function inUse: boolean;

      property sprite: string read FSprite;
      property spriteIndex: byte read FSpriteIndex;
      property map: smallint read FMap write setMap;
      property x: word read getX write setX;
      property y: word read getY write setY;
      property location: TSgPoint read getLocation write setLocation;
      property facing: byte read getFacing write setFacing;
      property gamesprite: TMapSprite read FGameSprite write FGameSprite;
      property vehicleType: TVehicleSet read FVehicleType;
      property carrying: TRPgParty read FCarrying write FCarrying;
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
      function getPlayer: TRpgMediaPlayer;
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
      property BGM: TRpgMediaPlayer read getPlayer;
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

function TScriptInterface.getPlayer: TRpgMediaPlayer;
begin
//   result := GCurrentEngine.mediaPlayer;
end;

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

{ TRpgEvent }

constructor TRpgEvent.create(base: TMapSprite);
begin
//   FIsChar := (base is TCharSprite);
   FBase := base;
   FBase.character := self;
   FTile := FBase.baseTile;
   FEvent := FBase.event;
   if FEvent <> nil then
      FID := FBase.event.id;
   //end if
end;

destructor TRpgEvent.delete;
var i: smallint;
begin
{   GGameEngine.currentMap.deleteEvent(FBase);
   FBase.leaveTile;
   FBase.Free;
   i := min(FID, high(GRpgEvents));
   while (i >= 0) and (GRpgEvents[i] <> self) do
      dec(i);
   assert(i <> -1);
   for I := i to high(GRpgEvents) - 1 do
      GRpgEvents[i] := GRpgEvents[i + 1];
   setLength(GRpgEvents, length(GRpgEvents) - 1);}
   inherited Destroy;
end;

destructor TRpgEvent.Destroy;
begin
{   if FBase is TNullEvent then
      FBase.free;}
   inherited;
end;

procedure TRpgEvent.ChangeSprite(name: string; index: integer; oldSprite: TMapSprite);
begin
   base.update(name, index, base.translucency >= 3);
end;

procedure TRpgEvent.doFlash(r, g, b, power: byte; time: cardinal);
begin
   FBase.flash(r, g, b, power, time);
end;

function TRpgEvent.getBase: TMapSprite;
begin
   result := self.FBase;
end;

function TRpgEvent.getFacing: byte;

   function translate(facing: TFacing): byte;
   begin
      case facing of
         facing_up: result := 8;
         facing_right: result := 6;
         facing_down: result := 2;
         facing_left: result := 4;
         else result := 0;
      end;
   end;

begin
   if FIsChar then
{   with FBase as TCharSprite do
      result := translate(facing)}
   else result := 2;
end;

function TRpgEvent.getLocation: TSgPoint;
begin
   result := point(self.x, self.y);
end;

function TRpgEvent.getMap: word;
begin
{   if FBase is TVehicleSprite then
      result := TVehicleSprite(FBase).template.Map
   else
      result := GGameEngine.currentMap.mapID;}
end;

function TRpgEvent.getX: word;
begin
   if FIsChar then
{   with FBase as TCharSprite do
      result := location.X
   else with FTile as TEventTile do
      result := trunc(FTile.X) div TILESIZE;}
end;

function TRpgEvent.getY: word;
begin
   if FIsChar then
{   with FBase as TCharSprite do
      result := location.Y
   else with FTile as TEventTile do
      result := trunc(FTile.Y) div TILESIZE;}
end;

procedure TRpgEvent.setLocation(const Value: TSgPoint);
begin
   FBase.leaveTile;
   FBase.location := value;
end;

{$WARN CONSTRUCTING_ABSTRACT OFF}
procedure TRpgEvent.switchType;
var index: word;
begin
{   index := GGameEngine.currentMap.indexOf(FBase);
   FBase.nuke;
   FBase.Free;
   GGameEngine.currentMap.event[index] := nil;
   case FEvent.isTile of
      false: FBase := TCharSprite.create(FEvent, GCurrentEngine.parent, self);
      true: FBase := TEventSprite.create(FEvent, GCurrentEngine.parent, self);
   end;
   FBase.place;
   GGameEngine.currentMap.swapEvent(FBase, index);}
end;
{$WARN CONSTRUCTING_ABSTRACT ON}

procedure TRpgEvent.update;
var newpage, oldpage: TEventPage;
begin
   if FEvent = nil then
      Exit;
   FEvent.locked := false;
{   oldpage := FEvent.lastCurrentPage;
   newpage := FEvent.newCurrentPage;
   if assigned(newpage) and (newpage.hasScript) and (newpage.startCondition in [parallel, automatic]) and (not FEvent.playing) then
      GCurrentEngine.executeEvent(FEvent, FBase);

   if oldpage = newpage then
      Exit;

   if ((fevent.isTile) and (FBase is TCharSprite)) or
      ((fevent.isTile = false) and (FBase is TEventSprite)) then
      switchType
   else
      FBase.updatePage(fevent.lastCurrentPage);}
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
   Mix_FadeOutMusic(time);
end;

procedure TRpgMediaPlayer.fadeIn(time: integer);
begin
   FCurrentTrack.FadeIn(time);
   FCurrentTrack.OnMusicFinished := musicLoopedHook;
   FBgmLooped := false;
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
      if GCurrentThread <> nil then
         GCurrentThread.syncRun(self.syncLoadFile)
      else self.syncLoadFile;
      while FSyncWait do
         sdl_delay(10);
   end
   else FCurrentTrack := FMediaPlayer.MusicManager[dummy];
   FCurrentTrack.OnMusicFinished := musicLoopedHook;
   FFadeTime := musicFile.fadeIn;
   FPlayOnce := false;
   if GCurrentThread <> nil then
      GCurrentThread.syncRun(self.syncPlayMusic)
   else self.syncPlayMusic;
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
      Mix_VolumeMusic(128);
      if FSystemBgmTrack[which] = nil then
         FSystemBgmTrack[which] := TSDLMusic.Create(FSystemBgmRecord[which].filename);
      FSystemBgmTrack[which].play(1);
      case which of
         bgmInn: ;
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

{ TRpgParty }

procedure TRpgParty.addExp(const id: smallint; number: integer);
var
  I: Integer;
  dummy: TRpgHero;
begin
   if id = -1 then
   begin
      for I := 1 to MAXPARTYSIZE do
{         if self[i] <> GCurrentEngine.hero[0] then
         begin
            dummy := GCurrentEngine.hero[self[i].template.id];
            dummy.exp := dummy.exp + number;
         end;}
      //end for
   //end if
   end else
      self[id].exp := self[id].exp + number;
   //end if
end;

procedure TRpgParty.addItem(const id, number: word);
begin
   FInventory.Add(id, number);
end;

procedure TRpgParty.addLevels(const id: smallint; number: integer);
var
   I: Integer;
   dummy: TRpgHero;
begin
   if id = -1 then
   begin
      for I := 1 to MAXPARTYSIZE do
{         if self[i] <> GCurrentEngine.hero[0] then
         begin
            dummy := GCurrentEngine.hero[self[i].template.id];
            dummy.level := dummy.level + number;
         end;
      //end for};
   //end if
   end else
      self[id].level := self[id].level + number;
   //end if
end;

procedure TRpgParty.ChangeSprite(name: string; index: integer; oldSprite: TMapSprite);
begin
{   if assigned(GGameEngine.character[0]) then
   with GGameEngine.character[0] do
      update(filename, index, translucency >= 3);
   //end WITH}
end;

constructor TRpgParty.Create(database: TRpgDatabase);
begin
//   FInventory := TRpgInventory.Create(database);
   FLevelNotify := true;
end;

destructor TRpgParty.Destroy;
begin
   FInventory.free;
   inherited;
end;

function TRpgParty.getBase: TMapSprite;
begin
//   result := GGameEngine.currentParty;
end;

function TRpgParty.getFacing: byte;
begin
   result := 0;
{   case GGameEngine.currentParty.facing of
      facing_up: result := 8;
      facing_right: result := 6;
      facing_down: result := 2;
      facing_left: result := 4;
   end;}
end;

function TRpgParty.getHero(x: byte): TRpgHero;
begin
{   if (x = 0) or (x > MAXPARTYSIZE) or (FParty[x] = nil) then
      result := GCurrentEngine.hero[0]
   else
      result := FParty[x];
   //end if}
end;

function TRpgParty.getMap: word;
begin
//   result := GGameEngine.currentMap.mapID;
end;

function TRpgParty.getTranslucency: byte;
begin
   if empty then
      result := 0
   else result := inherited getTranslucency;
end;

function TRpgParty.getX: word;
begin
{   if assigned(GGameEngine.currentParty) then
      result := GGameEngine.currentParty.location.x
   else result := 0;}
end;

function TRpgParty.getY: word;
begin
{   if assigned(GGameEngine.currentParty) then
      result := GGameEngine.currentParty.location.y
   else result := 0;}
end;

function TRpgParty.indexOf(who: TRpgHero): integer;
var i: byte;
begin
   result := -1;
   for i := 1 to MAXPARTYSIZE do
      if self[i] = who then
         result := i;
      //end if
   //end for
end;

function TRpgParty.empty: boolean;
var i: byte;
begin
   result := true;
   for i := 1 to MAXPARTYSIZE do
{      if self[i] <> GCurrentEngine.hero[0] then
         result := false;
      //end if};
   //end for
end;

procedure TRpgParty.doFlash(r, g, b, power: byte; time: cardinal);
begin
{   if assigned(GGameEngine.character[0]) then
      GGameEngine.character[0].flash(r, g, b, power, time);}
end;

function TRpgParty.size: byte;
var i: byte;
begin
   result := 0;
   for i := 1 to MAXPARTYSIZE do
{      if (self[i] <> GCurrentEngine.hero[0]) then
         inc(result)};
      //end if
   //end for
end;

function TRpgParty.openSlot: byte;
var i: byte;
begin
   i := 1;
{   while (self[i] <> GCurrentEngine.hero[0]) and (i <= MAXPARTYSIZE) do
      inc(i);}
   if i > MAXPARTYSIZE then
      result := 0
   else result := i;
end;

procedure TRpgParty.removeExp(const id: smallint; number: integer);
var
   I: Integer;
   dummy: TRpgHero;
begin
   if id = -1 then
   begin
      for I := 1 to MAXPARTYSIZE do
{         if self[i] <> GCurrentEngine.hero[0] then
         begin
            dummy := GCurrentEngine.hero[self[i].template.id];
            dummy.exp := dummy.exp - number;
         end;};
      //end for
   end else
      self[id].exp := self[id].exp - number;
   //end if
end;

procedure TRpgParty.removeItem(const id, number: word);
begin
   FInventory.Remove(id, number);
end;

procedure TRpgParty.removeLevels(const id: smallint; number: integer);
var
   I: Integer;
   dummy: TRpgHero;
begin
   if id = -1 then
   begin
      for I := 1 to MAXPARTYSIZE do
{         if self[i] <> GCurrentEngine.hero[0] then
         begin
            dummy := GCurrentEngine.hero[self[i].template.id];
            dummy.level := dummy.level - number;
         end;};
      //end for
   //end if
   end else
      self[id].level := self[id].level - number;
   //end if
end;

procedure TRpgParty.setFacing(const Value: byte);
var dummy: TMapSprite;
begin
//   dummy := GGameEngine.currentParty;
   case value of
      8: dummy.facing := facing_up;
      6: dummy.facing := facing_right;
      4: dummy.facing := facing_left;
      2: dummy.facing := facing_down;
   end;
end;

procedure TRpgParty.setHero(x: byte; value: TRpgHero);
begin
   if (x = 0) or (x > MAXPARTYSIZE) then
      Exit
   else FParty[x] := value;
end;

procedure TRpgParty.setTranslucency(const Value: byte);
begin
   if not empty then
      inherited setTranslucency(value);
end;

procedure TRpgParty.setX(const Value: word);
var place: TPoint;
begin
{   place := GGameEngine.currentParty.location;
   GGameEngine.currentParty.location := point(value, place.Y);}
end;

procedure TRpgParty.setY(const Value: word);
var place: TPoint;
begin
{   place := GGameEngine.currentParty.location;
   GGameEngine.currentParty.location := point(place.x, value);}
end;

procedure TRpgParty.sort;
var
   i, j: byte;
begin
   for I := 1 to MAXPARTYSIZE - 1 do
      for j := i to MAXPARTYSIZE - 1 do
      begin
         if FParty[j] = nil then
         begin
            FParty[j] := FParty[j + 1];
            FParty[j + 1] := nil;
         end;
      end;
   //end if
end;

function TRpgParty.takeDamage(power: word; defense, mDefense,
  variance: byte): word;
var i: byte;
begin
   result := 0;
   for i := 1 to MAXPARTYSIZE do
{      if self[i] <> GCurrentEngine.hero[0] then
         result := FParty[i].takeDamage(power, defense, mdefense, variance);
      //end if};
   //end for
end;

{ TRpgVehicle }

procedure TRpgVehicle.ChangeSprite(name: string; index: integer; oldSprite: TMapSprite);
begin
   Self.setSprite(name, index);
end;

constructor TRpgVehicle.Create({mapTree: TFullTree;} which: TVehicleSet);
{type
   TVhSpriteClass = class of TVehicleSprite;
var
   newSprite: TVhSpriteClass;}
begin
   inherited Create;
   FVehicleType := which;
{   if assigned(mapTree) then
   begin
      FMap := mapTree.vhStartMap[which];
      FX := mapTree.vhStartX[which];
      FY := mapTree.vhStartY[which];
   end;
   setSprite(GDatabase.SystemData.vehicleGraphic[which], GDatabase.SystemData.vehicleIndex[which]);
   case which of
      vh_boat: newSprite := TBoatSprite;
      vh_ship: newSprite := TShipSprite;
      vh_airship: newSprite := TAirshipSprite;
      else begin
         newSprite := nil;
         assert(false, 'Bad vehicle type!');
      end;
   end;
   FGameSprite := newSprite.Create(GCurrentEngine.parent, self, nil);
   if FMap = GGameEngine.currentMap.mapID then
      FGameSprite.location := point(FX, FY)
   else FGameSprite.location := point(0, -1);
   FGameSprite.facing := facing_left;
   (FGameSprite as TVehicleSprite).update(FSprite, FSpriteIndex, false);}
end;

destructor TRpgVehicle.Destroy;
begin
   FGameSprite.Free;
   inherited;
end;

procedure TRpgVehicle.doFlash(r, g, b, power: byte; time: cardinal);
begin
{   if map = GGameEngine.currentMap.mapID then
      self.gamesprite.flash(r, g, b, power, time);
   //end if}
end;

function TRpgVehicle.getBase: TMapSprite;
begin
   result := FGameSprite;
end;

function TRpgVehicle.getFacing: byte;
begin
   result := 0;
   case FGameSprite.facing of
      facing_up: result := 8;
      facing_right: result := 6;
      facing_down: result := 2;
      facing_left: result := 4;
   end;
end;

function TRpgVehicle.getLocation: TSgPoint;
begin
   result := point(self.x, self.y);
end;

function TRpgVehicle.getX: word;
begin
   FX := FGameSprite.location.x;
   result := FX;
end;

function TRpgVehicle.getY: word;
begin
   FY := FGameSprite.location.Y;
   result := FY;
end;

procedure TRpgVehicle.setLocation(const Value: TSgPoint);
begin
   FX := value.x;
   FY := value.Y;
   FGameSprite.location := value;
end;

procedure TRpgVehicle.setX(const Value: word);
begin
   FX := value;
   FGameSprite.location := point(FX, FGameSprite.location.Y);
end;

procedure TRpgVehicle.setY(const Value: word);
begin
   FY := value;
   FGameSprite.location := point(FGameSprite.location.x, FY);
end;

function TRpgVehicle.inUse: boolean;
begin
   result := FCarrying <> nil;
end;

procedure TRpgVehicle.setFacing(const Value: byte);
begin
   case value of
      8: FGameSprite.facing := facing_up;
      6: FGameSprite.facing := facing_right;
      4: FGameSprite.facing := facing_left;
      2: FGameSprite.facing := facing_down;
   end;
end;

procedure TRpgVehicle.setMap(const Value: smallint);
begin
   FMap := Value;
//   FGameSprite.visible := (map = GGameEngine.currentMap.mapID);
end;

procedure TRpgVehicle.setSprite(filename: string; index: byte);
var dummy: string;
begin
   if not (index in [0..7]) then
      Exit;
   dummy := filename;
   findGraphic(dummy, 'charset');
   if dummy = '' then
      Exit;

   FSprite := filename;
   FSpriteIndex := index;
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

{ TRpgCharacter }

procedure TRpgCharacter.flash(r, g, b, power: byte; time: cardinal; wait: boolean);
begin
   doFlash(r, g, b, power, time);
{   if wait then
      TEventThread(GCurrentThread).threadSleep(time, true);}
end;

function TRpgCharacter.getScreenX: integer;
begin
//   result := getX - round(GCurrentEngine.parent.WorldX / TILESIZE);
end;

function TRpgCharacter.getScreenXP: integer;
begin
//   result := self.screenX * TILESIZE;
end;

function TRpgCharacter.getScreenYP: integer;
begin
//   result := self.screenY * TILESIZE;
end;

function TRpgCharacter.getTranslucency: byte;
begin
   result := base.translucency;
end;

procedure TRpgCharacter.move(frequency: byte; skip: boolean; route: word);
begin
{   if route > high(GGameEngine.currentMap.routes) then
      Exit;
   if not assigned(self.base) then
      Exit;

   self.base.moveOrder := GGameEngine.currentMap.routes[route];}
   self.base.canSkip := skip;
   self.base.moveFreq := between(frequency, 1, 8);
end;

procedure TRpgCharacter.setTranslucency(const Value: byte);
begin
   base.translucency := Value;
end;

function TRpgCharacter.getScreenY: integer;
begin
//   result := getY - round(GCurrentEngine.parent.WorldY / TILESIZE);
end;

end.
