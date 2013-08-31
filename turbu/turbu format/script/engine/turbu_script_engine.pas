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
unit turbu_script_engine;

interface
uses
   SysUtils, Classes, Generics.Collections, RTTI, SyncObjs,
   timing,
   rsCompiler, rsExec, rsDefsBackend,
   turbu_map_interface, turbu_map_objects;

type
   TRegisterEnvironmentProc = procedure(compiler: TrsCompiler; importer: TrsTypeImporter; exec: TrsExec);
   TThreadWaitEvent = reference to function: boolean;
   TCutsceneEvent = procedure of object;
   TCallScriptEvent = reference to function(values: TArray<TValue>): TValue;

   TScriptEngine = class;

   TScriptThread = class(TThread)
   private
      FPages: TStack<TRpgEventPage>;
      FPage: TRpgEventPage;
      FParent: TscriptEngine;
      FOwnedExec: TrsExec;
      FOwnedProgram: TrsProgram;
      FDelay: TRpgTimestamp;
      FWaiting: TThreadWaitEvent;
      FSignal: TSimpleEvent;
      FOnCleanup: TProc;

      procedure scriptOnLine(Sender: TrsVM);
      procedure InternalThreadSleep();
      procedure threadSleep(Sender: TrsVM);

      procedure PushPage(value: TRpgEventPage);
      procedure PopPage;
   protected
      procedure Execute; override;
   public
      constructor Create(page: TRpgEventPage; parent: TScriptEngine);
      destructor Destroy; override;
      function CurrentObject: TRpgMapObject;
      property Terminated;
   end;

   TScriptEngine = class
   private
      FCompiler: TrsCompiler;
      FExec: TrsExec;
      FCurrentProgram: TrsProgram;
      FThreads: TList<TScriptThread>;
      FThreadLock: TCriticalSection;
      FImports: TArray<TPair<string, TrsExecImportProc>>;
      FEnvProc: TRegisterEnvironmentProc;
      FEnterCutscene: TCutsceneEvent;
      FLeaveCutscene: TCutsceneEvent;
      FThreadPool: TQueue<TScriptThread>;
      FTeleportThread: TScriptThread;
      FRenderUnpause: TCutsceneEvent;

      procedure AddScriptThread(thread: TScriptThread);
      procedure ClearScriptThread(thread: TScriptThread);
      procedure CreateExec;
      procedure InternalLoadEnvironment(compiler: TRsCompiler);
      procedure OnRunLine(Sender: TrsVM; const line: TrsDebugLineInfo);
      function GetThread(page: TRpgEventPage): TScriptThread;
      procedure SaveToPool(thread: TScriptThread);
      procedure RegisterImports;
      function OnDivideByZero(Sender: TrsVM; l: integer; var handled: boolean): integer;
   public
      constructor Create;
      destructor Destroy; override;
      procedure LoadScript(const script: string; context: TThread = nil);
      procedure LoadLibrary(const script: string);
      procedure LoadEnvironment(proc: TRegisterEnvironmentProc);
      procedure RegisterUnit(const name: string; const comp: TrsCompilerRegisterProc; const exec: TrsExecImportProc);
      procedure RunScript(const name: string); overload;
      procedure RunScript(const name: string; const args: TArray<TValue>); overload;
      procedure RunObjectScript(obj: TRpgMapObject; page: integer);
      function GetScriptRoutine(const name: string): TCallScriptEvent;
      procedure KillAll(const Cleanup: TProc = nil);
      procedure AbortThread;
      procedure threadSleep(time: integer; block: boolean = false);
      procedure SetWaiting(value: TThreadWaitEvent);
      procedure ThreadWait;
      procedure Reset;

      property OnEnterCutscene: TCutsceneEvent read FEnterCutscene write FEnterCutscene;
      property OnLeaveCutscene: TCutsceneEvent read FLeaveCutscene write FLeaveCutscene;
      property OnRenderUnpause: TCutsceneEvent read FRenderUnpause write FRenderUnpause;
      property TeleportThread: TScriptThread read FTeleportThread write FTeleportThread;
   end;

   TMapObjectManager = class
   private
      FMapObjects: TList<TRpgMapObject>;
      FGlobalScripts: TList<TRpgEventPage>;
      FScriptEngine: TScriptEngine;
      FPlaylist: TList<TRpgEventPage>;
      FOnUpdate: TProc;
      FInCutscene: boolean;
   public
      constructor Create;
      destructor Destroy; override;

      procedure LoadGlobalScripts(list: TMapObjectList);
      procedure LoadMap(const map: IRpgMap; context: TThread = nil);
      procedure Tick;
      procedure RunPageScript(page: TRpgEventPage);

      property ScriptEngine: TScriptEngine read FScriptEngine;
      property OnUpdate: TProc read FOnUpdate write FOnUpdate;
      property InCutscene: boolean read FInCutscene write FInCutscene;
   end;

var
   GScriptEngine: TScriptEngine;
   GMapObjectManager: TMapObjectManager;

implementation
uses
   Math,
   turbu_defs, turbu_battle_engine, {$IFDEF DEBUG}logs,{$ENDIF} delayedAction,
   rs_media,
   rsDefs,
   SDL;

procedure RegisterBattlesC(input: TrsTypeImporter);
begin
   input.ImportType(TypeInfo(TBattleResult));
   input.ImportType(TypeInfo(TBattleResultSet));
   input.ImportType(TypeInfo(TBattleFormation));
   input.ImportFunction('function battle(which: integer; const background: string; results: TBattleResultSet; firstStrike: boolean): TBattleResult');
   input.ImportFunction('function battleEx(which: integer; const background: string; formation: TBattleFormation; results: TBattleResultSet; bgMode, terrain: integer): TBattleResult');
end;

procedure RegisterMediaC(input: TrsTypeImporter);
begin
   input.ImportType(TypeInfo(TSfxTypes));
   input.ImportType(TypeInfo(TBgmTypes));
   input.ImportFunction('procedure playMusic(name: string; fadeIn, volume, tempo, balance: integer);');
   input.ImportFunction('procedure fadeOutMusic(time: integer);');
   input.ImportFunction('procedure memorizeBgm;');
   input.ImportFunction('procedure playMemorizedBgm;');
   input.ImportFunction('procedure playSound(name: string; volume, tempo, balance: integer);');
   input.ImportFunction('procedure playMovie(name: string; posX, posY, width, height: integer);');
   input.ImportFunction('procedure SetSystemSound(style: TSfxTypes; filename: string; volume, tempo, balance: integer);');
   input.ImportFunction('procedure PlaySystemSound(sound: TSfxTypes);');
   input.ImportFunction('procedure SetSystemMusic(style: TBgmTypes; filename: string; fadeIn, volume, tempo, balance: integer);');
end;

function Nullbattle(which: integer; const background: string; results: TBattleResultSet; firstStrike: boolean): TBattleResult;
begin
   result := br_victory
end;

function NullbattleEx(which: integer; const background: string; formation: TBattleFormation; results: TBattleResultSet; bgMode, terrain: integer): TBattleResult;
begin
   result := br_victory
end;

procedure RegisterBattlesE(RegisterFunction: TExecImportCall; RegisterArrayProp: TArrayPropImport);
begin
   RegisterFunction('battle', @NullBattle);
   RegisterFunction('battleEx', @NullBattleEx);
end;

procedure RegisterMediaE(RegisterFunction: TExecImportCall; RegisterArrayProp: TArrayPropImport);
begin
   RegisterFunction('playMusic', @rs_media.playMusic);
   RegisterFunction('fadeOutMusic', @rs_media.FadeOutMusic);
   RegisterFunction('memorizeBgm', @rs_media.MemorizeBGM);
   RegisterFunction('playMemorizedBgm', @rs_media.PlayMemorizedBgm);
   RegisterFunction('playSound', @rs_Media.playSound);
   RegisterFunction('SetSystemSound', @rs_Media.SetSystemSound);
   RegisterFunction('PlaySystemSound', @rs_Media.PlaySystemSound);
   RegisterFunction('SetSystemMusic', @rs_media.SetSystemMusic);
   RegisterFunction('playMovie', @rs_media.PlayMovie);
end;

{ TScriptEngine }

constructor TScriptEngine.Create;
begin
   assert(GScriptEngine = nil);
   FCompiler := TrsCompiler.Create;
   CreateExec;
   GScriptEngine := self;
   FThreads := TList<TScriptThread>.Create;
   FThreadLock := TCriticalSection.Create;
   FThreadPool := TQueue<TScriptThread>.Create;
   RegisterImports;
end;

destructor TScriptEngine.Destroy;
begin
   GScriptEngine := nil;
   FThreads.Free;
   FThreadPool.free;
   FThreadLock.Free;
   FCompiler.Free;
   FCurrentProgram.Free;
   FExec.Free;
   inherited Destroy;
end;

function TScriptEngine.GetThread(page: TRpgEventPage): TScriptThread;
begin
   FThreadLock.Enter;
   try
      if FThreadPool.count > 0 then
      begin
         result := FThreadPool.Dequeue;
         result.FPage := page;
         result.FSignal.SetEvent;
      end
      else begin
         result := TScriptThread.Create(page, self);
         result.FreeOnTerminate := true;
         result.start;
      end;
   finally
      FThreadLock.Leave;
   end;
end;

procedure TScriptEngine.SaveToPool(thread: TScriptThread);
begin
   FThreadLock.Enter;
   try
      FThreadPool.Enqueue(thread);
   finally
      FThreadLock.Leave;
   end;
end;

procedure TScriptEngine.CreateExec;
begin
   FExec := TrsExec.Create;
   FExec.RegisterStandardUnit('battles', RegisterBattlesE);
   FExec.RegisterStandardUnit('media', RegisterMediaE);
   FExec.OnLine := self.OnRunLine;
   FExec.OnDivideByZero := self.OnDivideByZero;
end;

function TScriptEngine.OnDivideByZero(Sender: TrsVM; l: integer;
  var handled: boolean): integer;
begin
   result := l;
   handled := true;
end;

procedure TScriptEngine.OnRunLine(Sender: TrsVM; const line: TrsDebugLineInfo);
var
   st: TScriptThread;
begin
   st := TThread.CurrentThread as TScriptThread;
   st.scriptOnLine(sender);
end;

procedure TScriptEngine.KillAll(const Cleanup: TProc = nil);
var
   curr: TThread;

   procedure WakeAllThreads;
   var
      thread: TScriptThread;
   begin
      FThreadLock.Enter;
      FThreadPool.Clear;
      for thread in FThreads do
         if thread <> curr then
         begin
            thread.Terminate;
            (thread as TScriptThread).FSignal.SetEvent;
         end;
      FThreadLock.Leave;
   end;

var
   done: boolean;
begin
   curr := TThread.CurrentThread;
   if not (curr is TScriptThread) then
      curr := nil;
   if assigned(Cleanup) then
   begin
      assert(assigned(curr));
      if assigned(TScriptThread(curr).FOnCleanup) then
         DelayExec(Cleanup)
      else TScriptThread(curr).FOnCleanup := cleanup;
   end;
   wakeAllThreads;

   repeat
      sleep(10);
      WakeAllThreads;
      FThreadLock.Enter;
      done := ((curr = nil) and (FThreads.Count = 0)) or ((FThreads.Count = 1) and (FThreads[0] = curr));
      FThreadLock.Leave;
      if TThread.CurrentThread.ThreadID = MainThreadID then
         CheckSynchronize();
   until done;
end;

procedure TScriptEngine.AbortThread;
var
   curr: TThread;
begin
   curr := TThread.CurrentThread;
   if curr is TScriptThread then
   begin
      curr.Terminate;
      Abort;
   end;
end;

procedure TScriptEngine.AddScriptThread(thread: TScriptThread);
begin
   FThreadLock.Enter;
   FThreads.Add(thread);
   FThreadLock.Leave;
end;

procedure TScriptEngine.ClearScriptThread(thread: TScriptThread);
begin
   FThreadLock.Enter;
   FThreads.Remove(thread);
   if FTeleportThread = thread then
      FTeleportThread := nil;
   FThreadLock.Leave;
end;

procedure TScriptEngine.InternalLoadEnvironment(compiler: TRsCompiler);
var
   importer: TrsTypeImporter;
begin
   importer := TrsTypeImporter.Create(compiler, compiler.GetUnit('SYSTEM'));
   try
      FEnvProc(compiler, importer, FExec);
   finally
      importer.Free;
   end;
end;

procedure TScriptEngine.LoadEnvironment(proc: TRegisterEnvironmentProc);
begin
   FEnvProc := proc;
   InternalLoadEnvironment(FCompiler);
end;

procedure TScriptEngine.LoadLibrary(const script: string);
begin
   FCompiler.CompileUnit(script);
end;

procedure TScriptEngine.LoadScript(const script: string; context: TThread = nil);
var
   pair: TPair<string, TrsExecImportProc>;
   tempCompiler: TrsCompiler;
begin
   {$IFDEF DEBUG}
   logs.logText(script);
   {$ENDIF}
   if context is TScriptThread then
   begin
      TScriptThread(context).FOwnedExec := FExec;
      TScriptThread(context).FOwnedProgram := FCurrentProgram;
      TScriptThread(context).Priority := tpHighest;
      CreateExec;
      for pair in FImports do
         FExec.RegisterStandardUnit(pair.Key, pair.Value);
      tempCompiler := TrsCompiler.Create;
      try
         InternalLoadEnvironment(tempCompiler);
      finally
         tempCompiler.Free;
      end;
   end
   else FCurrentProgram.Free;
   try
      FCurrentProgram := FCompiler.Compile(script);
   except
      FCurrentProgram := nil;
      raise;
   end;
   FExec.Load(FCurrentProgram);
end;

procedure TScriptEngine.RunScript(const name: string);
begin
   FExec.RunProc(name, []);
end;

procedure TScriptEngine.RunScript(const name: string; const args: TArray<TValue>);
begin
   FExec.RunProc(name, args);
end;

function TScriptEngine.GetScriptRoutine(const name: string): TCallScriptEvent;
begin
   result :=
      function(values: TArray<TValue>): TValue
      begin
         result := FExec.RunProc(name, values);
      end;
end;

procedure TScriptEngine.RunObjectScript(obj: TRpgMapObject; page: integer);
var
   context: TScriptThread;
   lPage: TRpgEventPage;
begin
   context := TThread.CurrentThread as TScriptThread;
   lPage := obj.pages[page - 1];
   assert(assigned(context.FPage));
   context.PushPage(lPage);
   try
      RunScript(lPage.scriptName);
   finally
      context.PopPage;
   end;
end;

procedure TScriptEngine.RegisterImports;
begin
   FCompiler.RegisterStandardUnit('battles', RegisterBattlesC);
   FCompiler.RegisterStandardUnit('media', RegisterMediaC);
end;

procedure TScriptEngine.RegisterUnit(const name: string;
  const comp: TrsCompilerRegisterProc; const exec: TrsExecImportProc);
begin
   FCompiler.RegisterStandardUnit(name, comp);
   FExec.RegisterStandardUnit(name, exec);
   SetLength(FImports, length(FImports) + 1);
   FImports[high(FImports)] := TPair<string, TrsExecImportProc>.Create(name, exec);
end;

procedure TScriptEngine.Reset;
begin
   FCompiler.Free;
   FExec.free;
   rsDefs.ResetTables;
   FImports := nil;

   FCompiler := TrsCompiler.Create;
   CreateExec;
   RegisterImports;
end;

{$O-}
procedure TScriptEngine.SetWaiting(value: TThreadWaitEvent);
var
   st: TScriptThread;
begin
   FRenderUnpause();
   st := TThread.CurrentThread as TScriptThread;
   st.FWaiting := value;
end;

procedure TScriptEngine.threadSleep(time: integer; block: boolean = false);
var
   st: TScriptThread;
begin
   FRenderUnpause();
   st := TThread.CurrentThread as TScriptThread;
   st.FDelay.Free;
   st.FDelay := TRpgTimestamp.Create(time);
   if block then
      FEnterCutscene();
   try
      st.InternalThreadSleep;
   finally
      if block then
         FLeaveCutscene();
   end;
end;

procedure TScriptEngine.ThreadWait;
var
   st: TScriptThread;
begin
   FRenderUnpause();
   st := TThread.CurrentThread as TScriptThread;
   st.scriptOnLine(nil);
end;

{ TMapObjectManager }

constructor TMapObjectManager.Create;
begin
   assert(GMapObjectManager = nil);
   GMapObjectManager := self;
   FMapObjects := TList<TRpgMapObject>.Create;
   FGlobalScripts := TList<TRpgEventPage>.Create;
   FScriptEngine := TScriptEngine.Create;
   FPlaylist := TList<TRpgEventPage>.Create;
end;

destructor TMapObjectManager.Destroy;
begin
   GMapObjectManager := nil;
   FPlaylist.Free;
   FScriptEngine.Free;
   FMapObjects.Free;
   FGlobalScripts.Free;
   inherited Destroy;
end;

procedure TMapObjectManager.LoadGlobalScripts(list: TMapObjectList);
var
   i: integer;
   page: TRpgEventPage;
begin
   assert(FGlobalScripts.Count = 0);
   for i := 0 to list.Count - 1 do
   begin
      page := list[i].pages[0];
      if page.startCondition <> on_call then
         FGlobalScripts.Add(page);
   end;
end;

procedure TMapObjectManager.LoadMap(const map: IRpgMap; context: TThread = nil);
var
   list: TStrings;
   i: integer;
begin
   FMapObjects.Clear;
   list := map.GetMapObjects;
   try
      FMapObjects.capacity := list.Count;
      for i := 0 to list.Count - 1 do
         FMapObjects.Add(list.Objects[i] as TRpgMapObject);
   finally
      list.Free;
   end;
   FScriptEngine.LoadScript(map.GetScript, context);
end;

procedure TMapObjectManager.RunPageScript(page: TRpgEventPage);
var
   thread: TScriptThread;
begin
   if page.parent.playing then
      Exit;
   page.parent.playing := true;
   thread := FScriptEngine.GetThread(page);
   {$IFDEF DEBUG}
   TThread.NameThreadForDebugging(AnsiString(format('TURBU Script thread: %s', [page.ScriptName])), thread.ThreadID);
   {$ENDIF}
end;

procedure TMapObjectManager.Tick;
var
   obj: TRpgMapObject;
   page: TRpgEventPage;
begin
   FPlaylist.Clear;
   for obj in FMapObjects do
   begin
      obj.UpdateCurrentPage;
      if assigned(obj.currentPage) and obj.currentPage.hasScript and (not obj.locked) and (not obj.playing)
         and (obj.currentPage.startCondition in [automatic, parallel]) then
         FPlaylist.Add(obj.currentPage);
   end;
   for page in FGlobalScripts do
   begin
      obj := page.parent;
      if obj.locked or obj.playing then
         Continue;
      if page.valid then
         FPlaylist.Add(page);
   end;
   if assigned(FOnUpdate) then
      FOnUpdate;
   if FScriptEngine.TeleportThread = nil then
   begin
      for page in FPlaylist do
         RunPageScript(page);
   end;
end;

{ TScriptThread }

constructor TScriptThread.Create(page: TRpgEventPage; parent: TScriptEngine);
begin
   inherited Create(true);
   FPage := page;
   FParent := parent;
   parent.AddScriptThread(self);
   FSignal := TSimpleEvent.Create;
end;

function TScriptThread.CurrentObject: TRpgMapObject;
begin
   result := FPage.parent;
end;

destructor TScriptThread.Destroy;
begin
   FParent.ClearScriptThread(self);
   FSignal.Free;
   FDelay.Free;
   FOwnedExec.Free;
   FOwnedProgram.Free;
   FPages.Free;
   if assigned(FOnCleanup) then
      DelayExec(FOnCleanup);
   inherited Destroy;
end;

procedure TScriptThread.Execute;
begin
   while not Terminated do
   begin
      if FPage.startCondition <> parallel then
         GScriptEngine.FEnterCutscene();
      try
         if FPage.scriptName <> '' then
         begin
//            NameThreadForDebugging('TURBU Script Thread');
            FParent.RunScript(FPage.scriptName);
         end;
      finally
         Sleep(TRpgTimestamp.FrameLength);
         FPage.parent.playing := false;
         if FPage.startCondition <> parallel then
            GScriptEngine.FLeaveCutscene();
      end;
      if not Terminated then
      begin
         if assigned(FOwnedExec) then
            Terminate
         else begin
            FSignal.ResetEvent;
            FParent.SaveToPool(self);
            FSignal.WaitFor;
         end;
      end;
   end;
end;

procedure TScriptThread.InternalThreadSleep;
const DELAY_SLICE = 50;
var
   timeleft: cardinal;
   dummy: cardinal;
begin
   repeat
      timeleft := FDelay.timeRemaining;
      dummy := min(DELAY_SLICE, timeleft);
      SDL_Delay(dummy);
   until (timeleft = dummy) or (self.Terminated);
   if Terminated then
      Abort;
end;

procedure TScriptThread.PushPage(value: TRpgEventPage);
begin
   if FPages = nil then
      FPages := TStack<TRpgEventPage>.Create;
   FPages.Push(FPage);
   FPage := value;
end;

procedure TScriptThread.PopPage;
begin
   FPage := FPages.Pop;
end;

procedure TScriptThread.threadSleep(Sender: TrsVM);
begin
   InternalThreadSleep;
   self.scriptOnLine(sender);
end;

procedure TScriptThread.scriptOnLine(Sender: TrsVM);
begin
   if (not self.terminated) and assigned(FWaiting) then
   begin
      repeat
         sleep(TRpgTimestamp.FrameLength);
      until (self.Terminated) or (FWaiting() = true);
      FWaiting := nil;
   end
   else if assigned(FDelay) then
      if (FDelay.timeRemaining > 0) then
         threadSleep(sender)
      else freeAndNil(FDelay);
   if Self.Terminated then
      Abort;
end;

end.
