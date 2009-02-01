unit script_engine;
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
   classes, syncObjs, types, math, //windows libs
   commons, events, chipset_data, script_interface, script_backend,
   {charset_graphics, frames,} charset_data, addition_sprite, turbu_database,
   rm_sound, skill_code, shop_data, {rpg_image,} timing, //turbu libs
   upsRuntime, uPSCompiler, //PascalScript libs
   {asphyreSprite} sdl_sprite; //asphyre libs

const
   MAXSCRIPTS = 15;
   GLOBAL_EVENT_TIMEOUT = 250;

type
   TStatList = (sMaxHP, sMaxMP, sAttack, sDefense, sMind, sAgility);
   TEventStartType = (es_actionButton, es_bump, es_parallel, es_auto, es_call);
   TBattleResult = (br_victory, br_escaped, br_defeated);
   TSysMenuList = (mnuMain, mnuShop, mnuName);
   TScriptEngine = class;

   TThreadWaitEvent = function: boolean;

   TEventThread = class(TRpgThread)
   private
      scriptExec: TPSExec;
      FParent: TScriptEngine;
      FEvent: TEvent;
      FEventBase: TRpgEvent;
      FWhichExec: byte;
      FStartType: TEventStartType;
      FCallStackEvent: TEventThread;

      procedure SetPointerToData(const VarName: string; Data: Pointer; aType: TIFTypeRec);
      procedure scriptOnLine(Sender: TPSExec);

      procedure textbox(const msg: string);
      procedure addExp(const id: smallint; number: integer);
      procedure removeExp(const id: smallint; number: integer);
      procedure addLevels(const id: smallint; number: byte);
      procedure removeLevels(const id: smallint; number: byte);
      function showChoice(input: string; handler: byte): integer;
      function inputNumberBox(const digits: byte): integer;
      function inn(messageStyle: byte; cost: integer): boolean;
      function buttonStart: boolean;
      procedure setSkin(const name: string);
      procedure sysMenu;
      function shop(style: word; messageStyle: byte; store: word): boolean;
      function inputText(start: string; heroId: word): string;
      function battle(which: word; allow_escape, first_strike: boolean): TBattleResult;
      procedure deleteThisEvent(permanant: boolean);
   protected
      procedure Execute; override;
      procedure setupScript(script: string);
   public
      constructor Create(parent: TScriptEngine; event: TEvent; base: TAdditionSprite; startType: TEventStartType = es_actionButton);
      constructor CreateCall(parent: TScriptEngine; event: TEvent; page: word; base: TAdditionSprite);
      destructor Destroy; override;
      procedure threadSleep; overload; inline;
      procedure threadSleep(time: cardinal; block: boolean = false); overload; inline;
      procedure syncRun(AMethod: TThreadMethod); inline;
   end;

   TScriptEngine = class(TObject)
   private
      FCompiler: TPSPascalCompiler;
      FImporter: TPSRuntimeClassImporter;
      FMessageLock: TCriticalSection;
      FBaseExec: TPSExec;
      FScriptEngine: array[1..MAXSCRIPTS] of TPSExec;
      FEngineInUse: array[1..MAXSCRIPTS] of boolean;
      FEventThread: array of TEventThread;
      FParent: TSpriteEngine;
      FCanSave: boolean;
      FScriptInterface: TScriptInterface;
      FTimer: TRpgTimer;
      FTimer2: TRpgTimer;
      FGlobalEventTimer: TRpgTimestamp;
      FMediaPlayer: TRpgMediaPlayer;
      FHeroPool: array of TRpgHero;
      FCurrentHero: TRpgHero;
      FCurrentShop: word;
      FLastState: TGameState;
      FThreadsFinalized: boolean;
      FIdle: boolean;

      function getHero(id: byte): TRpgHero;
      function heroCount: byte;
      procedure cleanupThreadArray;
      procedure cleanupScriptEngines;
   public
      //class functions
      constructor create(parent: TSpriteEngine);
      destructor Destroy; override;
      procedure registerConsoleThread(thread: TEventThread);
      procedure executeEvent(event: TEvent; base: TAdditionSprite; lockedThread: TEventThread = nil);
      procedure executeEventPage(base: TAdditionsprite; page: word; lockedThread: TEventThread = nil);
      function suspendEvent(event: TEvent): TEventThread;
      procedure notifyFinished(whichThread: TEventThread);
      procedure releaseExec(const which: byte);
      procedure assignCompiler(const which: byte; const address: TPSExec);
      function getFreeScriptExec(var which: byte): TPSExec;
      procedure registerEvent(const input: TAdditionSprite);
      procedure unregisterEvents(exceptFor: TRpgThread);
      procedure registerGlobalEvents;
      procedure playMusic(const filename: TRmMusic);
      procedure eventTick;
      procedure finalizeThreads;
      procedure killAll;
      procedure resume; inline;

      //script functions
      function rpgRandom(one, two: integer): integer;
      function onMap(where: TRpgPoint): boolean; overload;
      function onMap(where: TRpgPoint; map: integer): boolean; overload; inline;
      procedure stopAllEvents;

      //message box
      procedure waitForMboxReturn(sender: TEventThread);

      //menus
      procedure setCurrentShop(const Value: word);
      function getCurrentShop: TStoreInventory;

      //properties
      property compiler: TPSPascalCompiler read FCompiler;
      property exec: TPSExec read FBaseExec;
      property parent: TSpriteEngine read FParent;
      property messageLock: TCriticalSection read FMessageLock write FMessageLock;
      property timer: TRpgTimer read FTimer write FTimer;
      property timer2: TRpgTimer read FTimer2 write FTimer2;
      property mediaPlayer: TRpgMediaPlayer read FMediaPlayer;
      property hero[id: byte]: TRpgHero read getHero;
      property heroes: byte read heroCount;
      property canSave: boolean read FCanSave write FCanSave;
      property currentHero: TRpgHero read FCurrentHero write FCurrentHero;
      property currentShop: TStoreInventory read getCurrentShop;
   end;

{   TNullEvent = class(TEventSprite)
      constructor create(const AParent: TSpriteEngine); reintroduce;
   end;}

var
   GSwitches: array of boolean;
   GVariables: array of integer;
   GRpgEvents: array of TRpgEvent;
   GGlobalEvent: array of TRpgGlobalEvent;
   GVehicles: array of TRpgVehicle;
   GSkills: TSkillEngine;
   GGlobalEvents: TEventBlock;
   GScriptEngine: TScriptEngine;
   GParty: TRpgParty;
   GRsys: TScriptInterface;
//   GImages: array [0..20] of TRpgImage;
   GMoveLock: TCriticalSection;
   GEventLock: TCriticalSection;
   GEventUpdateLock: TCriticalSection;
   GThreadCleanLock: TCriticalSection;
   GCleanupThreads: boolean;

threadvar
   GDelay: TRpgTimestamp;
   GWaiting: TThreadWaitEvent;
   GOnHold: boolean;

resourcestring
   SCRIPT_HEADER = 'uses messages, sysdata, menu, maps;' + LFCR + 'begin' + LFCR +
                   'with rsys do ' + LFCR + 'begin' + LFCR + '   ';
   SCRIPT_FOOTER = LFCR + 'end;' + LFCR + 'end.';

implementation

uses
     sysUtils, windows, //windows
     {LMT, chipset_graphics,} rpg_list, uPSI_script_interface,
     {console, rs_system, rs_message, rs_menu, rs_map, locate_files, strtok,
     png_routines,//turbu}
     sdl,
     uPSUtils;

const
   DELAY_SLICE = 50;

function scriptOnUses(Sender: TPSPascalCompiler; const Name: tbtString): Boolean; forward;

{ TScriptEngine }

procedure TScriptEngine.cleanupScriptEngines;
var
   i: word;
begin
   i := 1;
   while I <= high(FScriptEngine) do
   begin
      if FEngineInUse[i] then
      begin
         repeat
            FScriptEngine[i].Stop;
            sleep(10);
         until not FEngineInUse[i];
      end;
      FScriptEngine[i].Free;
      inc(i);
   end;
end;

procedure TScriptEngine.cleanupThreadArray;
var i, j: integer;
begin
   i := 0;
   while I <= high(FEventThread) do
   begin
      if FEventThread[i] = nil then
         for j := i + 1 to high(FEventThread) do
            FEventThread[j - 1] := FEventThread[j];
         setLength(FEventThread, length(FEventThread) - 1);
      //end if
      inc(i);
   end;
end;

constructor TScriptEngine.create(parent: TSpriteEngine);
var
   I: Integer;
   v: TVehicleSet;
begin
   inherited create;
   GScriptEngine := self;
   FCompiler := TPSPascalCompiler.Create;
   FCompiler.BooleanShortCircuit := true;
   FCompiler.onUses := scriptOnUses;
   FImporter := TPSRuntimeClassImporter.Create;
   RIRegister_script_interface(FImporter);
   FScriptInterface := TScriptInterface.Create(self);
   GRsys := FScriptInterface;
   for I := 1 to high(FScriptEngine) do
      FScriptEngine[i] := nil;
   FMessageLock := TCriticalSection.Create;
   GMoveLock := TCriticalSection.Create;
   GEventLock := TCriticalSection.Create;
   GEventUpdateLock := TCriticalSection.Create;
   GThreadCleanLock := TCriticalSection.Create;
   FParent := parent;
{   if not (FParent is TGameMap) then
      raise EFatalError.create('Incompatible map engine used to initialize script engine!');
   setLength(FEventThread, 0);
   setLength(GSwitches, TGameMap(parent).database.switches.len + 1);
   setLength(GVariables, TGameMap(parent).database.variables.len + 1);
   setLength(GRpgEvents, 1);}
   {$WARN CONSTRUCTING_ABSTRACT OFF}
 //  GRpgEvents[0] := TRpgEvent.create(TNullEvent.create(TGameMap(parent)));
   {$WARN CONSTRUCTING_ABSTRACT ON}
   FMediaPlayer := TRpgMediaPlayer.create;
{   with TGameMap(FParent) do
   begin
      GDatabase := database;
      GGlobalEvents := ldb.globalEventBlock as TEventBlock;
      self.registerGlobalEvents;
      FTimer := TRpgTimer.create(TGameMap(FParent).timer);
      FTimer2 := TRpgTimer.create(TGameMap(FParent).timer2);
      GParty := TRpgParty.Create(database);
      setLength(FHeroPool, database.heroes + 1);
      for I := 0 to database.heroes do
         FHeroPool[i] := TRpgHero.create(database.hero[i]);
      setLength(GVehicles, ord(high(TVehicleSet)) + 2);
      for v := low(TVehicleSet) to high(TVehicleSet) do
         GVehicles[ord(v) + 1] := TRpgVehicle.Create(mapTree, v);
      GVehicles[0] := TRpgVehicle.create(nil, vh_boat);
      for I := 0 to ord(high(TSfxTypes)) do
         FMediaPlayer.loadSystemSound(TSfxTypes(i), database.systemData.sfx[TSfxTypes(i)]);
      for i := 0 to ord(high(TBgmTypes)) do
         FMediaPlayer.loadSystemMusic(TBgmTypes(i), database.SystemData.bgm[TBgmTypes(i)]);
      //end for
   end;
   GSkills := TSkillEngine.Create;
   prepareStore('');
   GImages[0] := TRpgImage.Create(GGameEngine, '', 0, 0, 0, false);}
end;

destructor TScriptEngine.destroy;
var
  I: Integer;
begin
   if not FThreadsFinalized then
      self.finalizeThreads;
   FCompiler.free;
   FMessageLock.Free;
   GMoveLock.Free;
   GEventLock.free;
   GEventUpdateLock.free;
   GThreadCleanLock.Enter;
   try
      cleanupThreadArray;
   finally
      GThreadCleanLock.Leave;
   end;
   cleanupScriptEngines;
   GSkills.free;
   FScriptInterface.Free;
   FImporter.free;
   GParty.free;
   FMediaPlayer.free;
   for I := low(FHeroPool) to high(FHeroPool) do
      FHeroPool[i].free;
   finalize(FHeroPool);
   for i := low(GVehicles) to high(GVehicles) do
      GVehicles[i].free;
   finalize(GVehicles);
   for i := low(GRpgEvents) to high(GRpgEvents) do
      GRpgEvents[i].free;
   finalize(GRpgEvents);
   for i := low(GGlobalEvent) to high(GGlobalEvent) do
      GGlobalEvent[i].free;
   finalize(GGlobalEvent);
{   for i := low(GImages) to high(GImages) do
      GImages[i].free;}
   finalize(GSwitches);
   finalize(GVariables);
   FTimer.Free;
   FGlobalEventTimer.Free;
   GScriptEngine := nil;
   inherited destroy;
end;

procedure TScriptEngine.eventTick;
var
  I: Integer;
  j: TVehicleSet;
begin
   if FIdle then
      Exit;

   GEventUpdateLock.enter;
   try
      for I := 0 to high(GRpgEvents) do
         GRpgEvents[i].update;
      if not assigned(FGlobalEventTimer) then
      begin
         for I := 1 to high(GGlobalEvent) do
            GGlobalEvent[i].update;
         FGlobalEventTimer := TRpgTimestamp.create(GLOBAL_EVENT_TIMEOUT);
      end else if FGlobalEventTimer.timeRemaining = 0 then
         freeAndNil(FGlobalEventTimer);
      for j := low(TVehicleSet) to high(TVehicleSet) do
         GVehicles[ord(j)].gamesprite.event.locked := false;
   finally
      GEventUpdateLock.leave;
   end;

{   if TGameMap(FParent).state <> FLastState then
   begin
      if TGameMap(FParent).state = on_map then
         for I := 1 to GGameEngine.characters do
            GGameEngine.character[i].resume
         else for I := 1 to GGameEngine.characters do
            GGameEngine.character[i].pause;
         //end for
      FLastState := TGameMap(FParent).state;
   end;
   if TGameMap(FParent).state in [on_map, in_message] then
   begin
      GMoveLock.Enter;
      try
         for I := 0 to GGameEngine.characters do
            if assigned(GGameEngine.character[i]) then
               GGameEngine.character[i].moveTick;
            //end if
      finally
          GMoveLock.Leave;
      end;
   end;}
end;

procedure TScriptEngine.executeEvent(event: TEvent; base: TAdditionSprite; lockedThread: TEventThread = nil);
var
   startCondition: TEventStartType;
begin
{   if event.playing or event.locked then
      Exit;

   event.playing := true;
   startCondition := es_actionButton; //suppress a compiler warning
   case event.lastCurrentPage.startCondition of
      by_key: startCondition := es_actionButton;
      by_touch, by_collision: startCondition := es_bump;
      automatic: startCondition := es_auto;
      parallel: startCondition := es_parallel;
   end;
   GThreadCleanLock.enter;
   try
      setLength(FEventThread, length(FEventThread) + 1);
      FEventThread[high(FEventThread)] := TEventThread.Create(self, event, base, startCondition);
      FEventThread[high(FEventThread)].FCallStackEvent := lockedThread;
   finally
      GThreadCleanLock.Leave;
   end;}
end;

procedure TScriptEngine.executeEventPage(base: TAdditionsprite; page: word; lockedThread: TEventThread = nil);
var
   event: TEvent;
begin
   event := base.event;
//   event.playing := true;
   GThreadCleanLock.enter;
   try
      setLength(FEventThread, length(FEventThread) + 1);
      FEventThread[high(FEventThread)] := TEventThread.CreateCall(self, event, page, base);
      FEventThread[high(FEventThread)].FCallStackEvent := lockedThread;
   finally
      GThreadCleanLock.Leave;
   end;
end;

procedure TScriptEngine.finalizeThreads;
var i: integer;
begin
   GCleanupThreads := true;
   for I := 0 to high(FEventThread) do
      if assigned(FEventThread[i]) then
         FEventThread[i].Terminate;
      //end if
   repeat
      sleep(20);
      GThreadCleanLock.Enter;
      try
         cleanupThreadArray;
      finally
         GThreadCleanLock.Leave;
      end;
   until length(FEventThread) = 0;
   finalize(FEventThread);
   FThreadsFinalized := true;
end;

function TScriptEngine.suspendEvent(event: TEvent): TEventThread;
var
  I: Integer;
begin
   result := nil;
   for I := 0 to high(FEventThread) do
      if FEventThread[i].FEvent = event then
      begin
         result := FEventThread[i];
         result.Suspend;
         Exit;
      end;
   //end if
end;

function TScriptEngine.getCurrentShop: TStoreInventory;
begin
//   result := GShops[FCurrentShop];
end;

procedure TScriptEngine.setCurrentShop(const Value: word);
begin
{   if high(GShops) < 0 then
      raise EFatalError.create('No shops available!')
   else if value > high(GShops) then
      FCurrentShop := 0
   else FCurrentShop := value;}
end;

procedure TScriptEngine.stopAllEvents;
var
   i: Integer;
begin
   for I := 0 to high(FEventThread) do
      if assigned(FEventThread[i]) then
         FEventThread[i].Terminate;
      //end if
   //end for
end;

function TScriptEngine.getFreeScriptExec(var which: byte): TPSExec;
var
  I: Integer;
begin
   i := 0;
   repeat
      inc(i);
      if i > high(FScriptEngine) then
         raise EFatalError.create('Too many concurrent scripts!');
      if not FEngineInUse[i] then
      begin
         result := FScriptEngine[i];
         FEngineInUse[i] := true;
         which := i;
         Exit;
      end;
   until false;
end;

function TScriptEngine.getHero(id: byte): TRpgHero;
begin
   if (id = 0) or (id > high(FHeroPool)) then
      result := FHeroPool[0]
   else
      result := FHeroPool[id];
   //end if
end;

function TScriptEngine.heroCount: byte;
begin
   result := high(FHeroPool);
end;

procedure TScriptEngine.killAll;
var
   I: Integer;
begin
   for I := 0 to high(FEventThread) do
      if assigned(FEventThread[i]) then
      begin
         FEventThread[i].Terminate;
         if FEventThread[i].Suspended then
            FEventThread[i].Resume;
      end;
   //end for
   FIdle := true;
end;

procedure TScriptEngine.notifyFinished(whichThread: TEventThread);
var i: byte;
begin
   GThreadCleanLock.Enter;
   try
      i := 0;
      while ((i <= high(FEventThread)) and (FEventThread[i] <> whichThread)) do
         inc(i);
      if i <= high(FEventThread) then
      begin
         FEventThread[i] := nil;
         cleanupThreadArray;
      end;
   finally
      GThreadCleanLock.Leave;
   end;
end;

function TScriptEngine.onMap(where: TRpgPoint; map: integer): boolean;
begin
{   if map = TGameMap(parent).currentMap.mapID then
      result := onMap(where)
   else result := true; //fix this in the future}
end;

function TScriptEngine.onMap(where: TRpgPoint): boolean;
begin
//   result := (between(where.x, 0, TGameMap(parent).width) = where.x) and (between(where.y, 0, TGameMap(parent).height) = where.y);
end;

function TScriptEngine.rpgRandom(one, two: integer): integer;
var dummy: integer;
begin
   dummy := abs(two - one);
   result := system.Random(dummy) + min(one, two);
end;                                        

procedure TScriptEngine.playMusic(const filename: TRmMusic);
begin
   FMediaPlayer.playMusic(filename);
end;

procedure TScriptEngine.registerEvent(const input: TAdditionSprite);
begin
   setLength(GRpgEvents, length(GRpgEvents) + 1);
   GRpgEvents[high(GRpgEvents)] := TRpgEvent.create(input);
end;

procedure TScriptEngine.unregisterEvents(exceptFor: TRpgThread);
var
   I: Integer;
begin
   for I := 1 to high(GRpgEvents) do
   begin
      if assigned(GRpgEvents[i].base) and assigned(GRpgEvents[i].base.event) then
      begin
         if GRpgEvents[i].base.event <> (exceptFor as TEventThread).FEvent then
{         while GRpgEvents[i].base.event.playing do
            sleep(10);}
         freeAndNil(GRpgEvents[i]);
      end;
   end;
   setLength(GRpgEvents, 1);
end;

procedure TScriptEngine.registerGlobalEvents;
var
   i: word;
begin
   setLength(GGlobalEvent, GGlobalEvents.len + 1);
   for I := 1 to high(GGlobalEvent) do
      GGlobalEvent[i] := TRpgGlobalEvent.create(GGlobalEvents[i  - 1], i);
end;

procedure TScriptEngine.registerConsoleThread(thread: TEventThread);
begin
{   assert(thread is TConsoleEventThread);
   GThreadCleanLock.Enter;
   try
      setLength(FEventThread, length(FEventThread) + 1);
      FEventThread[high(FEventThread)] := thread;
   finally
      GThreadCleanLock.Leave;
   end;
   thread.Resume;}
end;

procedure TScriptEngine.releaseExec(const which: byte);
begin
//   assert(FScriptEngine[which].Status in [isLoaded, isNotLoaded]);
   FScriptEngine[which].Clear;
   FEngineInUse[which] := false;
end;

procedure TScriptEngine.resume;
begin
   FIdle := false;
end;

procedure TScriptEngine.assignCompiler(const which: byte; const address: TPSExec);
begin
   assert(FEngineInUse[which]);
   if (FScriptEngine[which] <> address) then
      FScriptEngine[which] := address;
end;

procedure TScriptEngine.waitForMboxReturn(sender: TEventThread);
begin
{   repeat
      sleep(31);
   until sender.Terminated or not (TGameMap(FParent).State = in_message);
   sender.scriptOnLine(sender.scriptExec);}
end;

{ TEventThread }

procedure TEventThread.addExp(const id: smallint; number: integer);
var
  I: Integer;
begin
   if id = -1 then
      GParty.addExp(-1, number)
   else
      FParent.FHeroPool[id].exp := FParent.FHeroPool[id].exp + number;
   if not GParty.levelNotify then
      Exit;
   if not ((id in [0..high(FParent.FHeroPool)]) or (id = -1)) then
      Exit;

   if id = -1 then
   begin
      for I := 1 to high(FParent.FHeroPool) do
      with FParent.FHeroPool[i] do
         if levelUpdated then
            textbox(name + ' advanced to level ' + intToStr(level) + '!');
         //end if
      //end with
      //end for
   end
   else with FParent.FHeroPool[id] do
      if levelUpdated then
         textbox(name + ' advanced to level ' + intToStr(level) + '!');
      //end if
      //end with
   //end if
end;

procedure TEventThread.addLevels(const id: smallint; number: byte);
var
  I: Integer;
begin
   if id = -1 then
      GParty.addLevels(-1, number)
   else
      FParent.FHeroPool[id].level := FParent.FHeroPool[id].level + number;
   if not GParty.levelNotify then
      Exit;
   if not (id in [0..high(FParent.FHeroPool)]) or (id = -1) then
      Exit;

   if id = -1 then
   begin
      for I := 1 to high(FParent.FHeroPool) do
      with FParent.FHeroPool[i] do
         textbox(name + ' advanced to level ' + intToStr(level) + '!');
         //end if
      //end with
      //end for
   end
   else with FParent.FHeroPool[id] do
      textbox(name + ' advanced to level ' + intToStr(level) + '!');
      //end if
      //end with
   //end if
end;

function TEventThread.buttonStart: boolean;
begin
   result := FStartType = es_actionButton;
end;

constructor TEventThread.Create(parent: TScriptEngine; event: TEvent; base: TAdditionSprite; startType: TEventStartType = es_actionButton);
var
   I: Integer;
begin
try
   inherited create(event = nil);
   FEvent := event;
   FStartType := startType;
   freeOnTerminate := true;
   FParent := parent;
   if FEvent <> nil then
      self.setupScript(FEvent.lastCurrentPage.compiledScript);
   i := 0;
   if assigned(base) then
   begin
      while (i < length(GRpgEvents)) and (GRpgEvents[i].base <> base) do
         inc(i);
      if (i < length(GRpgEvents)) and (GRpgEvents[i].base = base) then
         FEventBase := GRpgEvents[i];
      //end if
   end else FEventBase := GRpgEvents[0];
except
   on E: EFatalError do
      self.Terminate;
end;
end;

constructor TEventThread.CreateCall(parent: TScriptEngine; event: TEvent; page: word; base: TAdditionSprite);
var
   i: integer;
begin
try
   inherited create(false);
   assert(assigned(event));
   FEvent := event;
   FStartType := es_call;
   freeOnTerminate := true;
   FParent := parent;
   self.setupScript(FEvent.page[page - 1].compiledScript);
   if assigned(base) then
   begin
      i := 0;
      while (i < length(GRpgEvents)) and (GRpgEvents[i].base <> base) do
         inc(i);
      if (i < length(GRpgEvents)) and (GRpgEvents[i].base = base) then
         FEventBase := GRpgEvents[i];
      //end if
   end else FEventBase := GRpgEvents[0];
except
   on E: EFatalError do
      self.Terminate;
end;
end;

destructor TEventThread.Destroy;
begin
   FParent.releaseExec(FWhichExec);
   FParent.notifyFinished(self);
{   png_routines.bmpCleanup;
   if assigned(FEvent) then
   begin
      FEvent.playing := false;
      FEvent.locked := true;
   end;
   if FStartType = es_auto then
      GGameEngine.cutscene := GGameEngine.cutscene - 1;
   wait(GFrameLength);}
   if assigned(FCallStackEvent) then
   begin
      assert(FCallStackEvent.Suspended);
      FCallStackEvent.Resume;
   end;
   inherited;
end;

procedure TEventThread.deleteThisEvent(permanant: boolean);
begin
   if not (assigned(FEventBase) and assigned(FEventBase.base))then
      Exit;

   GEventUpdateLock.Enter;
   try
      FEventBase.delete;
      self.Terminate;
      if permanant then
         FEvent.deleted := true;
   finally
      GEventUpdateLock.Leave;
   end;
end;

procedure TEventThread.SetPointerToData(const VarName: string; Data: Pointer; aType: TIFTypeRec);
var
   v: PIFVariant;
   t: TPSVariantIFC;
begin
   v := scriptExec.GetVar2(VarName);
   if (Atype = nil) or (v = nil) then raise EParseMessage.Create('Unable to find variable');
   t.Dta := @PPSVariantData(v).Data;
   t.aType := v.FType;
   t.VarParam := false;
   VNSetPointerTo(t, Data, aType);
end;

procedure TEventThread.Execute;
begin
   try
      if terminated then
         Exit;

{      if FStartType = es_auto then
         GGameEngine.cutscene := GGameEngine.cutscene + 1;}
      GDelay := nil;
      GWaiting := nil;
      GCurrentThread := self;
      inherited;
      scriptExec.RunScript;
   finally
{      if FEvent <> nil then
         FEvent.playing := false;
      //end if}
   end;
end;

function TEventThread.inn(messageStyle: byte; cost: integer): boolean;
begin
   FParent.messageLock.Enter;
{   if messageStyle >= INN_STYLES then
      messageStyle := 0;
   result := rs_message.inn(messageStyle + 1, cost, self); //compensation for 0-order array}
   FParent.messageLock.Leave;
end;

function TEventThread.inputNumberBox(const digits: byte): integer;
begin
   FParent.messageLock.Enter;
//   result := rs_message.inputNumber(digits, self);
   FParent.messageLock.Leave;
end;

procedure TEventThread.removeExp(const id: smallint; number: integer);
var
  I: Integer;
begin
   if id = -1 then
      GParty.removeExp(-1, number)
   else
      FParent.FHeroPool[id].exp := FParent.FHeroPool[id].exp - number;
   if not GParty.levelNotify then
      Exit;
   if not (id in [0..high(FParent.FHeroPool)]) or (id = -1) then
      Exit;

   if id = -1 then
   begin
      for I := 1 to high(FParent.FHeroPool) do
      with FParent.FHeroPool[i] do
         if levelUpdated then
            textbox(name + ' was reduced to level ' + intToStr(level) + '!');
         //end if
      //end with
      //end for
   end
   else with FParent.FHeroPool[id] do
      if levelUpdated then
         textbox(name + ' was reduced to level ' + intToStr(level) + '!');
      //end if
      //end with
   //end if
end;

procedure TEventThread.removeLevels(const id: smallint; number: byte);
var
  I: Integer;
begin
   if id = -1 then
      GParty.removeLevels(-1, number)
   else
      FParent.FHeroPool[id].level := FParent.FHeroPool[id].level - number;
   if not GParty.levelNotify then
      Exit;
   if not (id in [0..high(FParent.FHeroPool)]) or (id = -1) then
      Exit;

   if id = -1 then
   begin
      for I := 1 to high(FParent.FHeroPool) do
      with FParent.FHeroPool[i] do
         textbox(name + ' was reduced to level ' + intToStr(level) + '!');
         //end if
      //end with
      //end for
   end
   else with FParent.FHeroPool[id] do
      textbox(name + ' was reduced to level ' + intToStr(level) + '!');
      //end if
      //end with
   //end if
end;

procedure TEventThread.scriptOnLine(Sender: TPSExec);
begin
   if (not self.terminated) and assigned(GWaiting) then
   begin
      repeat
//         sleep(GFrameLength);
      until (GWaiting() = true) or (self.Terminated);
      GWaiting := nil;
   end
   else if assigned(GDelay) then
      if (GDelay.timeRemaining > 0) then
         threadSleep
      else
      begin
         freeAndNil(GDelay);
         if GOnHold then
         begin
{            GGameEngine.cutscene := GGameEngine.cutscene - 1;
            GOnHold := false;                                 }
         end;
      end;
   if Self.Terminated then
   begin
      sender.Pause;
      sender.Stop;
   end;
end;

procedure TEventThread.setSkin(const name: string);
begin
   FParent.FMessageLock.Enter;
//   TGameMap(FParent.parent).setSkin(name);
   FParent.FMessageLock.Leave;
end;

procedure TEventThread.setupScript(script: string);
var output: string;
begin
   scriptExec := FParent.getFreeScriptExec(FWhichExec);
   if scriptExec = nil then
   begin
      scriptExec := TPSExec.Create;
      //If a function doesn't require a SELF pointer to the event thread, which
      //changes with each thread, it only needs to be registered once.
{      scriptExec.RegisterDelphiFunction(@rs_message.messageOptions, 'MESSAGEOPTIONS', cdRegister);
      scriptExec.RegisterDelphiFunction(@rs_message.clearPortrait, 'CLEARPORTRAIT', cdRegister);
      scriptExec.RegisterDelphiFunction(@rs_message.setPortrait, 'SETPORTRAIT', cdRegister);
      scriptExec.RegisterDelphiMethod(FParent, @TScriptEngine.rpgRandom, 'RANDOM', cdRegister);
      scriptExec.RegisterDelphiMethod(FParent, @TScriptEngine.stopAllEvents, 'STOPALLEVENTS', cdRegister);
      scriptExec.RegisterDelphiFunction(@rs_system.heldItems, 'HELDITEMS', cdRegister);
      scriptExec.RegisterDelphiFunction(@rs_system.addItems, 'ADDITEM', cdRegister);
      scriptExec.RegisterDelphiFunction(@rs_system.removeItems, 'REMOVEITEM', cdRegister);
      scriptExec.RegisterDelphiFunction(@rs_system.heroJoin, 'HEROJOIN', cdRegister);
      scriptExec.RegisterDelphiFunction(@rs_system.heroLeave, 'HEROLEAVE', cdRegister);
      scriptExec.RegisterDelphiFunction(@rs_system.setSystemMusic, 'SETSYSTEMMUSIC', cdRegister);
      scriptExec.RegisterDelphiFunction(@rs_system.wait, 'WAIT', cdRegister);
      scriptExec.RegisterDelphiFunction(@rs_system.playMusic, 'PLAYMUSIC', cdRegister);
      scriptExec.RegisterDelphiFunction(@rs_system.fadeOutMusic, 'FADEOUTMUSIC', cdRegister);
      scriptExec.RegisterDelphiFunction(@rs_system.memorizeBgm, 'MEMORIZEBGM', cdRegister);
      scriptExec.RegisterDelphiFunction(@rs_system.playMemorizedBgm, 'PLAYMEMORIZEDBGM', cdRegister);
      scriptExec.RegisterDelphiFunction(@rs_system.playSound, 'PLAYSOUND', cdRegister);
      scriptExec.RegisterDelphiFunction(@rs_system.keyScan, 'KEYSCAN', cdRegister);
      scriptExec.RegisterDelphiFunction(@rs_system.callGlobalEvent, 'CALLGLOBALEVENT', cdRegister);
      scriptExec.RegisterDelphiFunction(@rs_system.callEvent, 'CALLEVENT', cdRegister);
      scriptExec.RegisterDelphiMethod(FParent.FMediaPlayer, @TRpgMediaPlayer.setSystemSound, 'SETSYSTEMSOUND', cdRegister);
      scriptExec.RegisterDelphiFunction(@rs_menu.prepareStore, 'PREPARESTORE', cdRegister);
      scriptExec.RegisterDelphiFunction(@rs_map.teleport, 'TELEPORT', cdRegister);
      scriptExec.RegisterDelphiFunction(@rs_map.teleportVehicle, 'TELEPORTVEHICLE', cdRegister);
      scriptExec.RegisterDelphiFunction(@rs_map.teleportEvent, 'TELEPORTEVENT', cdRegister);
      scriptExec.RegisterDelphiFunction(@rs_map.swapEvents, 'SWAPEVENTS', cdRegister);
      scriptExec.RegisterDelphiFunction(@rs_map.memorizeLocation, 'MEMORIZELOCATION', cdRegister);
      scriptExec.RegisterDelphiFunction(@rs_map.rideVehicle, 'RIDEVEHICLE', cdRegister);
      scriptExec.RegisterDelphiFunction(@rs_map.getTerrainID, 'GETTERRAINID', cdRegister);
      scriptExec.RegisterDelphiFunction(@rs_map.getEventID, 'GETEVENTID', cdRegister);
      scriptExec.RegisterDelphiFunction(@rs_map.setTransition, 'SETTRANSITION', cdRegister);
      scriptExec.RegisterDelphiFunction(@rs_map.eraseScreen, 'ERASESCREEN', cdRegister);
      scriptExec.RegisterDelphiFunction(@rs_map.showScreen, 'SHOWSCREEN', cdRegister);
      scriptExec.RegisterDelphiFunction(@rs_map.setScreenTone, 'SETSCREENTONE', cdRegister);
      scriptExec.RegisterDelphiFunction(@rs_map.flashscreen, 'FLASHSCREEN', cdRegister);
      scriptExec.RegisterDelphiFunction(@rs_map.shakescreen, 'SHAKESCREEN', cdRegister);
      scriptExec.RegisterDelphiFunction(@rs_map.lockscreen, 'LOCKSCREEN', cdRegister);
      scriptExec.RegisterDelphiFunction(@rs_map.unlockscreen, 'UNLOCKSCREEN', cdRegister);
      scriptExec.RegisterDelphiFunction(@rs_map.panScreen, 'PANSCREEN', cdRegister);
      scriptExec.RegisterDelphiFunction(@rs_map.panScreenTo, 'PANSCREENTO', cdRegister);
      scriptExec.RegisterDelphiFunction(@rs_map.returnScreen, 'RETURNSCREEN', cdRegister);
      scriptExec.RegisterDelphiFunction(@rs_map.setWeather, 'SETWEATHER', cdRegister);
      scriptExec.RegisterDelphiFunction(@rs_map.increaseWeather, 'INCREASEWEATHER', cdRegister);
      scriptExec.RegisterDelphiFunction(@rs_map.decreaseWeather, 'DECREASEWEATHER', cdRegister);
      scriptExec.RegisterDelphiFunction(@rs_map.newImage, 'NEWIMAGE', cdRegister);
      scriptExec.RegisterDelphiFunction(@rs_map.setBG, 'SETBGIMAGE', cdRegister);
      scriptExec.RegisterDelphiFunction(@rs_map.showBattleAnim, 'SHOWBATTLEANIM', cdRegister);
      scriptExec.RegisterDelphiFunction(@rs_map.prepareRoute, 'PREPAREROUTE', cdRegister);
      scriptExec.RegisterDelphiFunction(@rs_map.waitUntilMoved, 'WAITUNTILMOVED', cdRegister);
      scriptExec.RegisterDelphiFunction(@rs_map.stopMoveScripts, 'STOPMOVESCRIPTS', cdRegister);}
      RegisterClassLibraryRuntime(scriptExec, FParent.FImporter);
   end;
   scriptExec.RegisterDelphiMethod(self, @TEventThread.textbox, 'SHOWMESSAGE', cdRegister);
   scriptExec.RegisterDelphiMethod(self, @TEventThread.showChoice, 'SHOWCHOICE', cdRegister);
   scriptExec.RegisterDelphiMethod(self, @TEventThread.inputNumberBox, 'INPUTNUMBER', cdRegister);
   scriptExec.RegisterDelphiMethod(self, @TEventThread.addExp, 'ADDEXP', cdRegister);
   scriptExec.RegisterDelphiMethod(self, @TEventThread.removeExp, 'REMOVEEXP', cdRegister);
   scriptExec.RegisterDelphiMethod(self, @TEventThread.addLevels, 'ADDLEVELS', cdRegister);
   scriptExec.RegisterDelphiMethod(self, @TEventThread.removeLevels, 'REMOVELEVELS', cdRegister);
   scriptExec.RegisterDelphiMethod(self, @TEventThread.buttonStart, 'BUTTONSTART', cdRegister);
   scriptExec.RegisterDelphiMethod(self, @TEventThread.battle, 'BATTLE', cdRegister);
   scriptExec.RegisterDelphiMethod(self, @TEventThread.shop, 'SHOP', cdRegister);
   scriptExec.RegisterDelphiMethod(self, @TEventThread.inn, 'INN', cdRegister);
   scriptExec.RegisterDelphiMethod(self, @TEventThread.setSkin, 'SETSKIN', cdRegister);
   scriptExec.RegisterDelphiMethod(self, @TEventThread.sysMenu, 'OPENMENU', cdRegister);
   scriptExec.RegisterDelphiMethod(self, @TEventThread.inputText, 'INPUTTEXT', cdRegister);
   scriptExec.RegisterDelphiMethod(self, @TEventThread.deleteThisEvent, 'DELETECHARACTER', cdRegister);

   if not scriptExec.LoadData(script) then
   begin
      output := TIFErrorToString(scriptExec.ExceptionCode, scriptExec.ExceptionString);
      freeAndNil(scriptExec);
      raise EFatalError.create('Could not load script!');
   end;
   SetPointerToData('switchD', @GSwitches, scriptExec.GetTypeNo(scriptExec.GetType('switchArray')));
   SetPointerToData('varD', @GVariables, scriptExec.GetTypeNo(scriptExec.GetType('varArray')));
   SetPointerToData('rsys', @FParent.FScriptInterface, scriptExec.GetTypeNo(scriptExec.GetType('TScriptInterface')));
   setPointerToData('party', @GParty, scriptExec.GetTypeNo(scriptExec.GetType('TRpgParty')));
   SetPointerToData('thisEvent', @self.FEventBase, scriptExec.GetTypeNo(scriptExec.GetType('TRpgEvent')));
   SetPointerToData('vehicle', @GVehicles, scriptExec.GetTypeNo(scriptExec.GetType('vehicleArray')));
   SetPointerToData('boat', @GVehicles[1], scriptExec.GetTypeNo(scriptExec.GetType('TRpgVehicle')));
   SetPointerToData('ship', @GVehicles[2], scriptExec.GetTypeNo(scriptExec.GetType('TRpgVehicle')));
   SetPointerToData('airship', @GVehicles[3], scriptExec.GetTypeNo(scriptExec.GetType('TRpgVehicle')));
   FParent.assignCompiler(FWhichExec, scriptExec);
//   scriptExec.OnRunLine := scriptOnLine;
end;

procedure TEventThread.syncRun(AMethod: TThreadMethod);
begin
   self.Synchronize(AMethod);
end;

procedure TEventThread.sysMenu;
begin
   FParent.messageLock.Enter;
   try
//      TGameMap(FParent.parent).systemMenu(mnuMain);
   finally
      FParent.messageLock.Leave;
   end;
end;

function TEventThread.shop(style: word; messageStyle: byte; store: word): boolean;
begin
   FParent.messageLock.Enter;
   try
//      result := rs_menu.shop(style, messageStyle, store);
   finally
      FParent.messageLock.Leave;
   end;
end;

function TEventThread.inputText(start: string; heroId: word): string;
begin
   FParent.messageLock.Enter;
   try
{      TGameMap(FParent).menuInt := heroID;
      TGameMap(FParent).menuStr := start;
      TGameMap(FParent).systemMenu(mnuName);
      result := TGameMap(FParent).menuStr;}
   finally
      FParent.MessageLock.Leave;
   end;
end;

function TEventThread.battle(which: word; allow_escape, first_strike: boolean): TBattleResult;
begin
   FParent.messageLock.Enter;
   try
      result := br_victory; //expand on this
   finally
      FParent.messageLock.Leave;
   end;
end;

function TEventThread.showChoice(input: string; handler: byte): integer;
begin
   if handler > 5 then
      handler := 5;
   FParent.messageLock.Enter;
   try
//      result := rs_message.choiceBox(input, handler > 0, self);
      if (result = -1) and (handler <> 5) then
         result := handler;
   finally
      FParent.messageLock.Leave;
   end;
end;

procedure TEventThread.textbox(const msg: string); register;
begin
   FParent.messageLock.Enter;
   try
//      rs_message.showMessage(msg, self);
   finally
      FParent.messageLock.Leave;
   end;
end;

procedure TEventThread.threadSleep;
var
   timeleft: cardinal;
   dummy: cardinal;
begin               
   repeat
      timeleft := GDelay.timeRemaining;
      dummy := min(DELAY_SLICE, timeleft);
      SDL_Delay(dummy);
   until (timeleft = dummy) or (self.Terminated);
   self.scriptOnLine(self.scriptExec);
end;

procedure TEventThread.threadSleep(time: cardinal; block: boolean = false);
begin
   GDelay := TRpgTimestamp.Create(time);
{   if block then
      GGameEngine.cutscene := GGameEngine.cutscene + 1;
   threadSleep;
   if block then
      GGameEngine.cutscene := GGameEngine.cutscene - 1;}
end;

{ TNullEvent }

{constructor TNullEvent.create(const AParent: TSpriteEngine);
begin
   //do nothing
end;}

{ Classless }

function scriptOnUses(Sender: TPSPascalCompiler; const Name: tbtString): Boolean;

   function AddPointerVariable(const VarName, VarType: tbtString): Boolean;
   var
      FVar: TPSVar;
   begin
      FVar := sender.AddUsedVariableN(varname, vartype);
      if fvar = nil then
         result := False
      else begin
         fvar.exportname := fvar.Name;
         fvar.SaveAsPointer := true;
         Result := True;
      end;
   end;

var dummy: TPSType;
begin
   result := true;
   if Name = 'SYSTEM' then begin
      dummy := sender.addtypeS('varArray', 'array of integer');
      dummy.ExportName := true;
      dummy := sender.AddTypeS('switchArray', 'array of boolean');
      dummy.ExportName := true;
      sender.AddTypeS('TStatList', '(sMaxHP, sMaxMP, sAttack, sDefense, sMind, sAgility)');
      sender.AddTypeS('TSlotList', '(weapon, shield, armor, helmet, relic, all)');
      sender.AddTypeS('TBgmTypes', '(bgmTitle, bgmBattle, bgmBossBattle, bgmVictory, bgmInn, bgmBoat, bgmShip, bgmAirship, bgmGameOver)');
      sender.AddTypeS('TSfxTypes', '(sfxCursor, sfxAccept, sfxCancel, sfxBuzzer, sfxBattleStart, sfxEscape, sfxEnemyAttack, sfxEnemyDamage, sfxAllyDamage, sfxEvade, sfxEnemyDies, sfxItemUsed)');
      sender.AddTypeS('TBattleResult', '(br_victory, br_escaped, br_defeated)');
      SIRegister_script_interface(GScriptEngine.FCompiler);
      dummy := sender.AddTypeS('vehicleArray', 'array of TRpgVehicle');
      dummy.ExportName := true;
      dummy.ExportName := true;
      AddPointerVariable('varD', 'varArray');
      AddPointerVariable('switchD', 'switchArray');
      AddPointerVariable('vehicle', 'vehicleArray');
      AddPointerVariable('boat', 'TRpgVehicle');
      AddPointerVariable('ship', 'TRpgVehicle');
      AddPointerVariable('airship', 'TRpgVehicle');
      AddPointerVariable('rsys', 'TScriptInterface');
      AddPointerVariable('party', 'TRpgParty');
      AddPointerVariable('thisEvent', 'TRpgEvent');
      sender.AddDelphiFunction('function random(one, two: integer): integer;');
      sender.AddDelphiFunction('function buttonStart: boolean');
      sender.AddDelphiFunction('function battle(which: word; allow_escape, first_strike: boolean): TBattleResult;');
      sender.AddDelphiFunction('procedure wait(time: cardinal);');
      sender.AddDelphiFunction('procedure stopAllEvents;');
      sender.addDelphiFunction('procedure deleteCharacter(permanant: boolean);');
   end
   else if name = 'MESSAGES' then
   begin
      sender.AddTypeS('TMboxLocation', '(mb_top, mb_middle, mb_bottom)');
      Sender.AddDelphiFunction('procedure showMessage(const msg: string)');
      sender.addDelphiFunction('procedure messageOptions(const transparent: boolean; const position: TMboxLocation; const dontHideHero: boolean; const continueEvents: boolean);');
      Sender.AddDelphiFunction('procedure clearPortrait;');
      Sender.AddDelphiFunction('procedure setPortrait(const filename: string; const index: byte; const rightside, flipped: boolean);');
      Sender.AddDelphiFunction('function showChoice(input: string; handler: byte): integer;');
      sender.AddDelphiFunction('function inputNumber(const digits: byte): integer');
      sender.AddDelphiFunction('function inn(messageStyle: byte; cost: integer): boolean;');
   end
   else if name = 'SYSDATA' then
   begin
      sender.AddConstantN('KS_DOWN', 'word').SetUInt(1);
      sender.AddConstantN('KS_LEFT', 'word').SetUInt(2);
      sender.AddConstantN('KS_RIGHT', 'word').SetUInt(4);
      sender.AddConstantN('KS_UP', 'word').SetUInt(8);
      sender.AddConstantN('KS_DIRS', 'word').SetUInt($F);
      sender.AddConstantN('KS_ACTION', 'word').SetUInt($10);
      sender.AddConstantN('KS_CANCEL', 'word').SetUInt($20);
      sender.AddConstantN('KS_ALL', 'word').SetUInt($FFFF);
      sender.AddDelphiFunction('function heldItems(const index: word; const equipped: boolean): word;');
      sender.AddDelphiFunction('procedure removeItem(const id, number: word);');
      sender.AddDelphiFunction('procedure addItem(const id, number: word);');
      sender.AddDelphiFunction('procedure heroLeave(const id: byte);');
      sender.AddDelphiFunction('procedure heroJoin(const id: byte);');
      sender.AddDelphiFunction('procedure addExp(const id: smallint; number: integer)');
      sender.AddDelphiFunction('procedure removeExp(const id: smallint; number: integer)');
      sender.AddDelphiFunction('procedure addLevels(const id: smallint; number: integer)');
      sender.AddDelphiFunction('procedure removeLevels(const id: smallint; number: integer)');
      sender.AddDelphiFunction('procedure setSystemMusic(const which: TBgmTypes; const newSong: string);');
      sender.AddDelphiFunction('procedure setSystemSound(const which: TSfxTypes; const newSound: string);');
      sender.AddDelphiFunction('procedure setSkin(const name: string);');
      sender.AddDelphiFunction('procedure playMusic(name: string; time, volume, tempo, balance: word);');
      sender.AddDelphiFunction('procedure fadeOutMusic(time: word);');
      sender.AddDelphiFunction('procedure memorizeBgm;');
      Sender.AddDelphiFunction('procedure playMemorizedBgm;');
      sender.AddDelphiFunction('procedure playSound(name: string; volume, tempo, balance: word);');
      sender.addDelphiFunction('function keyScan(mask: word; wait: boolean): byte;');
      sender.AddDelphiFunction('procedure callGlobalEvent(id: word);');
      sender.AddDelphiFunction('procedure callEvent(event, page: word);');
   end
   else if name = 'MENU' then
   begin
      sender.AddDelphiFunction('procedure openMenu;');
      sender.AddDelphiFunction('function prepareStore(merchandise: string): word;');
      sender.AddDelphiFunction('function shop(style: word; messageStyle: byte; store: word): boolean;');
      sender.AddDelphiFunction('function inputText(start: string; heroId: word): string;')
   end
   else if name = 'MAPS' then
   begin
      sender.AddTypeS('TTransitionTypes', '(trnMapEnter, trnMapExit, trnBattleStartErase, trnBattleStartShow, trnBattleEndErase, trnBattleEndShow)');
      sender.AddTypeS('TTransitions', '(trnDefault, trnFade, trnBlocks, trnBlockUp, trnBlockDn, trnBlinds, trnStripeHiLo, trnStripeLR, trnOutIn, trnInOut, trnScrollU, trnScrollD, trnScrollL, trnScrollR, trnDivHiLow, trnDivLR, trnDivQuarters, trnZoom, trnMosaic, trnRipple, trnNone);');
      sender.AddTypeS('TDirections', '(dir_up, dir_right, dir_down, dir_left);');
      sender.AddTypeS('TWeatherEffects', '(we_off, we_rain, we_snow, we_fog, we_sand);');
      sender.addDelphiFunction('procedure teleport(map, x, y: integer);');
      sender.addDelphiFunction('procedure memorizeLocation(var map, x, y: integer);');
      sender.addDelphiFunction('procedure rideVehicle;');
      sender.AddDelphiFunction('procedure teleportVehicle(which: TRpgVehicle; map, x, y: integer);');
      sender.AddDelphiFunction('procedure teleportEvent(which: TRpgEvent; x, y: integer);');
      sender.AddDelphiFunction('procedure swapEvents(first, second: TRPgEvent);');
      sender.AddDelphiFunction('function getTerrainID(x, y: word): word;');
      sender.AddDelphiFunction('function getEventID(x, y: word): word;');
      sender.AddDelphiFunction('procedure setTransition(const which: TTransitionTypes; const newTransition: TTransitions);');
      sender.AddDelphiFunction('procedure eraseScreen(whichTransition: TTransitions);');
      sender.AddDelphiFunction('procedure showScreen(whichTransition: TTransitions);');
      sender.AddDelphiFunction('procedure setScreenTone(r, g, b, sat: byte; duration: cardinal; wait: boolean);');
      sender.AddDelphiFunction('procedure flashScreen(r, g, b, power: byte; duration: cardinal; wait: boolean);');
      sender.AddDelphiFunction('procedure shakeScreen(power, speed: byte; duration: cardinal; wait: boolean);');
      sender.AddDelphiFunction('procedure lockScreen;');
      sender.AddDelphiFunction('procedure unlockScreen;');
      sender.AddDelphiFunction('procedure panScreen(direction: TDirections; distance: word; speed: byte; wait: boolean);');
      sender.AddDelphiFunction('procedure panScreenTo(x, y: word; speed: byte; wait: boolean);');
      sender.AddDelphiFunction('procedure returnScreen(speed: byte; wait: boolean);');
      sender.AddDelphiFunction('procedure setWeather(effect: TWeatherEffects; severity: byte);');
      sender.AddDelphiFunction('procedure increaseWeather;');
      sender.AddDelphiFunction('procedure decreaseWeather;');
      sender.AddDelphiFunction('function newImage(name: string; x, y: integer; zoom, transparency: word; pinned, mask: boolean): TRpgImage;');
      sender.AddDelphiFunction('procedure flashEvent(r, g, b, power: byte; duration: cardinal; wait: boolean);');
      sender.AddDelphiFunction('procedure setBGImage(name: string; scrollX, scrollY: shortint; autoX, autoY: boolean);');
      sender.AddDelphiFunction('procedure showBattleAnim(which: word; target: TRpgCharacter; wait, fullscreen: boolean);');
      sender.AddDelphiFunction('function prepareRoute(route: string; loop: boolean): word;');
      sender.AddDelphiFunction('procedure waitUntilMoved;');
      sender.AddDelphiFunction('procedure stopMoveScripts;');
   end
   else
      Result := False;
   //end if
end;

initialization
begin
   GScriptEngine := nil;
   GThreadCleanLock := nil;
end;

finalization
begin
   if assigned(GScriptEngine) then
      GScriptEngine.finalizeThreads;
end;

end.
